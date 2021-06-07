


PRO ADD_FIELD, INPUT_FILE, OUTPUT_FILE = OUTPUT_FILE
; ====================================================

;input_file = '/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo_bak.nc'
;output_file = '/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo.nc'

; 1 --specify input file or directory, input field names and derived fields--------

if(not keyword_set(output_file))then output_file = input_file

fj = file_search(input_file, test_dir=1)
if(fj eq '')then begin  ; it is a file
   files_in     = input_file
   old_sav_path = strstrip(input_file, '/', last=1)
   new_sav_path = strstrip(output_file, '/', last=1)
   ;new_sav_path = old_sav_path ; if the individually written files need to be in another folder
   if(input_file ne output_file)then spawn, 'cp ' + input_file + ' ' + output_file

endif else begin

   old_sav_path = input_file +'/'
   new_sav_path = output_file + '/'
   files_in     = file_search(old_sav_path+'/*.nc', count = n_file)
   ;new_sav_path = old_sav_path ; if the individually written files need to be in another folder
   make_folder, new_sav_path
endelse



n_file = n_elements(files_in)

 print,''
 print,'  calculating and saving additional fields from input in ', old_sav_path
 print,''


; change 1 datatype specification

 restore,'cmip6_macro.sav'
 what = 0
 for i = 0, n_elements(cmip6.mips)-1 do begin
  what = what or (strpos(input_file, '/'+cmip6.mips[i] + '/') ge 0)
 endfor
 if(what)then datatype = 'cmip6'

 if(strpos(input_file, '/ERA5/') ge 0)then datatype = 'era5'
 if(strpos(input_file, '/GCM/' ) ge 0)then datatype = 'cam'

 
; change 2 common fields
 vars_out = ['FLNA','FSNA','FLNAC','FSNAC','SRFRAD','SRFRADC','TOARAD','TOARADC','ATMRAD','ATMRADC', $
             'LWCF','SWCF','LWCFS','SWCFS','LWCFA','SWCFA','TOTCRF','TOTCRFS','TOTCRFA', $
             'PRECT','PME','SRFNET','ATMNET', $
             'H','THETA','TSDIFF']

; change 2.1  customized input fields and special output fields

CASE DataType OF
'cam': begin   
       vars_in = ['SOLIN','FSDS','FSNS', 'FSNSC','FSDSC','FSNTC','FSNT',$
            'FLDS','FLNS','FLNSC','FLNT','FLNTC','FLUT','FLUTC',$
              'LHFLX','SHFLX','QFLX','PRECC','PRECL','T','Q','Z3', 'PHIS', 'QRL','QRS','TS','TREFHT']

       cesm_vars_in = vars_in

       vars_out = [vars_out, 'QR']
       mapping = 'cesm2cesm'
       end
'cmip6': begin
       vars_in =['hfls','hfss','hus','pr','rlds','rldscs','rlus','rlut','rlutcs','rsds','rsdscs','rsdt','rsus',$
                    'rsuscs','rsut','rsutcs','rtmt','ta','zg','ts','tas']

       cesm_vars_in = var_mapping(vars_in,'all2cesm')   ; to use cesm name in the data_instructure
       vars_out = [vars_out, 'FLNS','FLNSC','FSNT','FSNTC','FSNS','FSNSC','QFLX']

       mapping = 'cesm2cmip'
       end
'era5':  begin
          
       ; -----------------------
       ERA2ADD, old_sav_path  ; done!

       RETURN
       ; -----------------------

         end
else:    begin
          print,'add_field data type needs to be defined to add fields, file path needs to be included'
          print, input_file
          print, ' should be in CMIP6 MIPS, ERA5 or GCM folders
          stop
         end
ENDCASE


 ; input data


; 2 ---- get input data for derived fields and put in structure
; --------------------------------------------------------------
data_in = create_struct('fields',vars_in)

  FOR iv = 0 , n_elements(vars_in)-1 DO BEGIN

    var = vars_in[iv]
    cesm_var = cesm_vars_in[iv]

    if(n_file eq 1)then begin
       file0 = files_in[0]
    endif else begin
        file0 = old_sav_path + var + '.nc' 
    endelse

    if(not file_exist(file0)) then begin
         print, 'This file does not exist, stop in add_field.pro ', file0
         stop
    endif

    print, '  get input var ', strtrim(iv,2), '  ',var

     dd_in      = get_fld(file0, var)  
;    ==================================

     dd_in_att  = get_cdf_att_all(file0, var)
     
     data_in = create_struct(data_in, cesm_var, dd_in, cesm_var +'_att', dd_in_att, cesm_var +'_file', file0)
 ENDFOR

; 3 ---- derive new fields
; ----------------------------------------

; change 2.1

CASE DataType of
'cam': begin

 FLNS  = get_stru(data_in, 'FLNS')
 FLNT = get_stru(data_in, 'FLNT')
 FLNSC  = get_stru(data_in, 'FLNSC')
 FLNTC = get_stru(data_in, 'FLNTC')

 FSNS   = get_stru(data_in, 'FSNS')
 FSNT   = get_stru(data_in, 'FSNT')
 FSNSC   = get_stru(data_in, 'FSNSC')
 FSNTC   = get_stru(data_in, 'FSNTC')

 PRECT = get_stru(data_in, 'PRECC') + get_stru(data_in, 'PRECL') 
 PME = PRECT - get_stru(data_in, 'QFLX')/1000.  ; to the same unit as m/s 

 LHFLX   = get_stru(data_in, 'LHFLX')
 PME = PRECT - get_stru(data_in, 'QFLX')/1000.  ; to the same unit as m/s

 QR = get_stru(data_in, 'QRL') + get_stru(data_in, 'QRS')

     end

'cmip6': begin

 FLNS   = get_stru(data_in, 'FLUS') - get_stru(data_in, 'FLDS') 
 FLNT   = get_stru(data_in, 'FLNT') 
 FLNSC  = get_stru(data_in, 'FLUS') - get_stru(data_in, 'FLDSC') 
 FLNTC  = get_stru(data_in, 'FLNTC') 

 FSNS   = get_stru(data_in, 'FSDS') - get_stru(data_in, 'FSUS') 
 FSNT   = get_stru(data_in, 'SOLIN') - get_stru(data_in, 'FSUT') 
 FSNSC   = get_stru(data_in, 'FSDSC') - get_stru(data_in, 'FSUSC') 
 FSNTC   = get_stru(data_in, 'SOLIN') - get_stru(data_in, 'FSUTC') 

 LHFLX   = get_stru(data_in, 'LHFLX')
 PRECT   = get_stru(data_in, 'PRECT')
 QFLX = LHFLX / 2.5e6
 PRECL  = PRECT*0.0
 PRECSL  = PRECT*0.0
 PME = PRECT - QFLX

;; FLNA   = get_stru(data_in, 'FLNS') - get_stru(data_in, 'FLNT')
;; FSNA   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNS')
;; FLNAC   = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNTC')
;; FSNAC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FSNSC')
;; SRFRAD   = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FLNS')
;; SRFRADC   = get_stru(data_in, 'FSNSC') - get_stru(data_in, 'FLNSC')
;; TOARAD   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FLNT') 
;; TOARADC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FLNTC') 
  
;; LWCF = get_stru(data_in, 'FLNTC') - get_stru(data_in, 'FLNT')
;; SWCF = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNTC')
;; LWCFS = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNS')
;; SWCFS = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FSNSC')

         end
else:
endcase

 FLNA  = FLNS  - FLNT
 FLNAC = FLNSC - FLNTC
 FSNA   = FSNT - FSNS
 FSNAC   = FSNTC - FSNSC
 SRFRAD  = FSNS - FLNS
 SRFRADC = FSNSC - FLNSC
 TOARAD  = FSNT - FLNT
 TOARADC = FSNTC - FLNTC
 ATMRAD  = TOARAD - SRFRAD
 ATMRADC = TOARADC - SRFRADC
 LWCF = FLNTC - FLNT
 SWCF = FSNT - FSNTC
 LWCFS = FLNSC - FLNS
 SWCFS = FSNS - FSNSC
 LWCFA = LWCF - LWCFS
 SWCFA = SWCF - SWCFS

;;  FLNAC  = get_stru(data_in, 'FLUS') - get_stru(data_in, 'FLDSC') - get_stru(data_in, 'FLUTC') 
;;  FLNA   = get_stru(data_in, 'FLUS') - get_stru(data_in, 'FLDS') - get_stru(data_in, 'FLUT') 
;;  FLNAC  = get_stru(data_in, 'FLUS') - get_stru(data_in, 'FLDSC') - get_stru(data_in, 'FLUTC') 
;;  FSNA   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNS')
;;  FLNAC   = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNTC')
;;  FSNAC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FSNSC')
;;  SRFRAD   = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FLNS')
;;  SRFRADC   = get_stru(data_in, 'FSNSC') - get_stru(data_in, 'FLNSC')
;;  TOARAD   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FLNT')
;;  TOARADC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FLNTC')

;;  LWCF = get_stru(data_in, 'FLNTC') - get_stru(data_in, 'FLNT')
;;  SWCF = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNTC')
;;  LWCFS = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNS')
;;  SWCFS = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FSNSC')
;;  QR = get_stru(data_in, 'QRL') + get_stru(data_in, 'QRS')

;;  PRECT = get_stru(data_in, 'PRECC') + get_stru(data_in, 'PRECL')

;; ATMRAD  = TOARAD - SRFRAD 
;;  ATMRADC = TOARADC - SRFRADC 

;;  LWCFA = LWCF - LWCFS
;;  SWCFA = SWCF - SWCFS
   
;;  LWCFA = LWCF - LWCFS
;;  SWCFA = SWCF - SWCFS

  TOTCRF  = LWCF + SWCF
  TOTCRFS = LWCFS + SWCFS
  TOTCRFA = LWCFA + SWCFA
    
 TSDIFF= get_stru(data_in, 'TS') - get_stru(data_in, 'TREFHT')  
 SRFNET = SRFRAD - get_stru(data_in, 'LHFLX') - get_stru(data_in, 'SHFLX')  
 ATMNET = TOARAD - SRFNET

 T = get_stru(data_in, 'T')
 Q = get_stru(data_in, 'Q')
 Z3 = get_stru(data_in, 'Z3')

 H = T + 9.8/1004.* Z3 + 2.5e6/1004.*Q

 file0 = get_stru(data_in, 'T_file')
 if(datatype eq 'cam')then $ 
 lev = get_fld(file0,'lev') else $
 lev = get_fld(file0,'plev') 
 if(max(lev) gt 1500.)then lev = lev/100.

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
   'H',H, 'THETA', THETA, 'TSDIFF',TSDIFF)  ;, 'FLDSC',FLDSC)

 case datatype of
 'cam':   begin
               data_out =create_struct(data_out,'QR',QR)
          end
 'cmip6': begin
              data_out = create_struct(data_out, 'FLNS',FLNS, 'FLNSC',FLNSC, 'FSNT',FSNT, 'FSNTC',FSNTC, $
                     'FSNS',FSNS, 'FSNSC',FSNSC, 'QFLX', QFLX)
          end
  else:
  endcase
; 4 ---- write out by using data from structure
; ----------------------------------------
 vars_out = vars_out[reverse(sort(vars_out))]  ; to appear in a-z order in the nc file
 nv = n_elements(vars_out)-1

 if(datatype eq 'cam')then $
 att_del_names = ['long_name', 'short_name','units'] else $
 att_del_names = ['long_name', 'comment','description', 'out_name', 'standard_name', 'title', 'variable_id','id']

 for iv = 0,nv do begin
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
    var_tmp = var_map('T', mapping)
   endif else begin
    var_tmp = var_map('TS', mapping)
   endelse
    file_tmp = old_sav_path + var_tmp +'.nc'

  if(file_exist(file))then spawn, 'rm ' + file
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

  jnames = get_cdf_att_all(file, var)
  jnames = tag_names(jnames)
  for j = 0, n_elements(att_del_names)-1 do begin
     if(belongsto(att_del_names[j], jnames))then $
     NCDF_ATTDEL, fid, Varid ,att_del_names[j]
  endfor
;  NCDF_ATTDEL, fid, Varid , 'long_name' 
;  NCDF_ATTDEL, fid, Varid , 'short_name' 
;  NCDF_ATTDEL, fid, Varid , 'units'
  ncdf_close,fid
  print,' saved file ', iv, ' ', file
   print, iv,'  ', var, '  ',min(dd), max(dd), mean(dd)

 endif else begin ; single file

; 4.2 ---- open original file, copy a variable, then replace it
; -----------------------------------------------------------------------------
  file = output_file

  if(array_equal (size(dd), size(T)))then  var_tmp = var_map('T',mapping) else var_tmp = var_map('TS',mapping)

   ncdf_vars,file,varsj, no_print=1

   if(not belongsto(var, varsj))then begin
     cmd = 'ncap2 -O -s '+"'"+var+'=array(1.0f,1.,'+var_tmp+')'+"'"+' '+ file+ ' '+ file
     spawn,cmd

   ;; disallow replacement
   ;; endif

   print,' added new var', nv-iv, '  ',var, ' in ', file
   fid = ncdf_open(file, /write)
   varid = NCDF_VARID(fid, var)
   NCDF_VARPUT, fid,varid,dd
   ncdf_close,fid

   endif

 endelse  ; fingle file 
  
 endfor  ; iv finished with added fields

print,'file saved in ', output_file
print,'=========================================================================='


;return


end
