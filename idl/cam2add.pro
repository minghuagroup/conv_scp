

; change 1

input_file = '/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo_bak.nc'
output_file = '/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo.nc'


; 1 --specify input file or directory, input field names and derived fields--------

fj = file_search(input_file, test_dir=1)

if(fj eq '')then begin  ; it is a file

   files_in     = input_file
   old_sav_path = strstrip(input_file, '/', last=1)
   new_sav_path = old_sav_path ; if the individually written files need to be in another folder
   if(input_file ne output_file)then spawn, 'cp ' + input_file + ' ' + output_file

endif else begin

   old_sav_path = input_file +'/'
   files_in     = file_search(old_sav_path+'/*.nc', count = n_file)
   new_sav_path = old_sav_path ; if the individually written files need to be in another folder

endelse


n_file = n_elements(files_in)

 print,''
 print,'  calculating and saving additional fields from input in ', old_sav_path
 print,''


; change 2

 vars_in = ['SOLIN','FSDS','FSNS', 'FSNSC','FSDSC','FSNTC','FSNT',$
            'FLDS','FLNS','FLNSC','FLNT','FLNTC','FLUT','FLUTC',$
              'LHFLX','SHFLX','QFLX','PRECC','PRECL','T','Q','Z3', 'PHIS', 'QRL','QRS']

 vars_out = ['FLNA','FSNA','FLNAC','FSNAC','SRFRAD','SRFRADC','TOARAD','TOARADC','ATMRAD','ATMRADC', $
    'LWCF','SWCF','LWCFS','SWCFS','LWCFA','SWCFA','TOTCRF','TOTCRFS','TOTCRFA', $
    'PRECT','PME','SRFNET','ATMNET', $
    'H','THETA', 'Z3','QR']
  


 ; input data


; 2 ---- get input data for derived fields and put in structure
; --------------------------------------------------------------
data_in = create_struct('fields',vars_in)

  FOR iv = 0 , n_elements(vars_in)-1 DO BEGIN

    var = vars_in[iv]
    if(n_file eq 1)then begin
       file0 = files_in[0]
    endif else begin
        file0 = old_sav_path + var + '.nc' 
    endelse

    if(not file_exist(file0)) then begin
         print, 'This file does not exist, stop in cam2add.pro ', file0
         stop
    endif

    print, '  get input var ', strtrim(iv,2), '  ',var

     dd_in      = get_fld(file0, var)  
;    ==================================

     dd_in_att  = get_cdf_att_all(file0, var)
     
     data_in = create_struct(data_in, var, dd_in, var+'_att', dd_in_att, var+'_file', file0)
 ENDFOR

; 3 ---- derive new fields
; ----------------------------------------
 FLNA   = get_stru(data_in, 'FLNS') - get_stru(data_in, 'FLNT')
 FSNA   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNS')
 FLNAC   = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNTC')
 FSNAC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FSNSC')
 SRFRAD   = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FLNS')
 SRFRADC   = get_stru(data_in, 'FSNSC') - get_stru(data_in, 'FLNSC')
 TOARAD   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FLNT') 
 TOARADC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FLNTC') 
  
 LWCF = get_stru(data_in, 'FLNTC') - get_stru(data_in, 'FLNT')
 SWCF = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNTC')
 LWCFS = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNS')
 SWCFS = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FSNSC')
 QR = get_stru(data_in, 'QRL') + get_stru(data_in, 'QRS')


 PRECT = get_stru(data_in, 'PRECC') + get_stru(data_in, 'PRECL') 
 PME = PRECT - get_stru(data_in, 'QFLX')/1000.  ; to the same unit as m/s 

 ATMRAD  = TOARAD - SRFRAD 
 ATMRADC = TOARADC - SRFRADC 

 LWCFA = LWCF - LWCFS
 SWCFA = SWCF - SWCFS
   
 LWCFA = LWCF - LWCFS
 SWCFA = SWCF - SWCFS
 TOTCRF  = LWCF + SWCF
 TOTCRFS = LWCFS + SWCFS
 TOTCRFA = LWCFA + SWCFA
    
 SRFNET = SRFRAD - get_stru(data_in, 'LHFLX') - get_stru(data_in, 'SHFLX')  
 ATMNET = TOARAD - SRFNET

 PHIS = get_stru(data_in, 'PHIS') 
 T = get_stru(data_in, 'T')
 Q = get_stru(data_in, 'Q')
 Z3 = get_stru(data_in, 'Z3')

 ;;Z3 = Z3 + PHIS/9.8  ; already added, and it becomes a 2D array!

 H = T + 9.8/1004.* Z3 + 2.5e6/1004.*Q

 file0 = get_stru(data_in, 'T_file')
 lev = get_fld(file0,'lev')

 pp = T*0.0
 for k = 0, n_elements(lev)-1 do begin
   pp[*,*,k,*] = lev[k]
 endfor
 THETA = T*(1000./pp)^0.287

 ; 3.1 ----- put new fields in structure
 ; !!!!!

; change 3 above calculation
; change 4 put in out structure

 data_out =create_struct('fields', vars_out, 'FLNA',FLNA, 'FSNA',FSNA, 'FLNAC',FLNAC,'FSNAC',FSNAC, $
  'SRFRAD',SRFRAD,'SRFRADC',SRFRADC,'TOARAD',TOARAD,'TOARADC',TOARADC,'ATMRAD',ATMRAD,'ATMRADC',ATMRADC, $
   'LWCF',LWCF, 'SWCF',SWCF,'LWCFS',LWCFS,'SWCFS',SWCFS,'LWCFA',LWCFA,'SWCFA',SWCFA,$
   'TOTCRF',TOTCRF,'TOTCRFS',TOTCRFS,'TOTCRFA',TOTCRFA, $
   'PRECT',PRECT,'PME',PME,'SRFNET',SRFNET,'ATMNET',ATMNET, $
   'H',H, 'THETA', THETA, 'Z3',Z3, 'QR',QR)

; 4 ---- write out by using data from structure
; ----------------------------------------
 nv = n_elements(vars_out)-1
 for iv = nv,0,-1 do begin
; ============================================
  var = vars_out[iv]

  dd = get_stru(data_out, var)    ; get data
 ; ===========================
  sz = size(dd)
  
 if(n_file gt 1)then begin 

; 4.1 ---- write out file individually by copying existing file and modifing it
; -----------------------------------------------------------------------------
   file = new_sav_path + var + '.nc'
  if(array_equal (size(dd), size(T)))then begin 
    file_tmp = old_sav_path + 'T.nc'
    var_tmp = 'T
   endif else begin
    file_tmp = old_sav_path + 'TS.nc'
    var_tmp = 'TS'
   endelse

  spawn, 'cp ' + file_tmp + ' ' + file
  
  fid = ncdf_open(file, /write)
  ncdf_control, fid, /REDEF
  varid = NCDF_VARID(fid, var_tmp)
  NCDF_ATTPUT, fid, /Global , 'processed field by' , 'Minghua Zhang'
  
  NCDF_VARRENAME, fid, Varid, var
  ncdf_control, fid, /ENDEF
  ncdf_close,fid
  
  fid = ncdf_open(file, /write)
  varid = NCDF_VARID(fid, var)
  NCDF_VARPUT, fid,varid,dd
  NCDF_ATTDEL, fid, Varid , 'long_name' 
  NCDF_ATTDEL, fid, Varid , 'short_name' 
  NCDF_ATTDEL, fid, Varid , 'units'
  ncdf_close,fid
  print,' saved file ', iv, ' ', file
   print, iv,'  ', var, '  ',min(dd), max(dd), mean(dd)

 endif else begin ; single file

; 4.2 ---- open original file, copy a variable, then replace it
; -----------------------------------------------------------------------------
  file = output_file

  if(array_equal (size(dd), size(T)))then  var_tmp = 'T' else var_tmp = 'TS'

   ncdf_vars,file,varsj, no_print=1

   if(not belongsto(var, varsj))then begin
     cmd = 'ncap2 -O -s '+"'"+var+'=array(1.0f,1.,'+var_tmp+')'+"'"+' '+ file+ ' '+ file
     spawn,cmd

   ;; disallow replacement
   ;; endif

   print,' added new var', iv, '  ',var, ' in ', file
   fid = ncdf_open(file, /write)
   varid = NCDF_VARID(fid, var)
   NCDF_VARPUT, fid,varid,dd
   ncdf_close,fid

   endif

;   print,'file saved in ', file
 endelse  ; fingle file 
  
 endfor  ; iv finished with added fields



;return


end
