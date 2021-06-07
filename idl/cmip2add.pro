

pro cmip2add, old_sav_path, new_sav_path

; if file does not exist, save a dummy dd2m = 1.0e20

 print,''
 print,'  calculating and saving additional 2d fields in vars_add specified in cmip6_vars.pro....' 
 print,''

  large_number = 999990.

; retrive and rewrite saved data as follows
; data from running cmip2sav.pro
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

  file0 = old_sav_path 
  file  = new_sav_path 

  cmip6_vars = cmip6_vars()
  vars_atm = cmip6_vars.vars_atm
  nv = n_elements(vars_atm)
  
  sav_type = 0
  if(strpos(file0,'/sav/') ge 0)then sav_type = 1

  if(sav_type)then begin
    restore, file0 + 'ts.sav'      ; assuming these exist
    ; -----------------------
  endif else begin  ; all defaults are defined for later save purpose
    filein = file0 + 'ts.nc'       ; assuming exists
    ; -----------------------
    dd3m = get_fld(filein, 'ts')
    var3 = 'dummy'
    modelid3 = 'dummy'
    mipid3 ='dummy'
    expid3 = 'dummy'
    dataType3 = 'dummy'
    lat3 = get_fld(filein, 'lat')
    lon3 = get_fld(filein, 'lon')
    lev3 = [1000.]
    time3 = 1.0
    year3 = 1.0
    month3 = 1.0
    day3 =1.0
    nc_files3= 'dummy'
    climo3 =1
    dd3m_att= create_struct('dummy', 'dummy')   
  endelse
  ; ================================================

;  for iv = 0,nv-1 do begin  ; this is to avoid error for restoring non-existent file
;   var3 =  vars_atm[iv] 
;   test = file + var3 + '.sav'
;   if(not file_exist(test)) then begin 
;      print, test,  ' does not exist, saved a single large number into it'
;      dd3m = large_number
;      filesave = file + var3+'.sav'
;        save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
;                   nc_files3, climo3,dd3m_att
;    endif 
;   endfor

 print,'  radiation fields....'

  dd3m = get_dd3m(file0, 'FSDT', sav_type)
  SOLIN = dd3m

;stop
  var3 = 'SOLIN'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

  ;       restore, file0 + cmip6_2cesm_var('FLUT')+'.sav'
  var3 = 'FLUT'
  dd3m = get_dd3m(file0, var3, sav_type)
  FLNT= dd3m
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

  ;       restore, file0 + cmip6_2cesm_var('FLUTC')+'.sav'
  var3 = 'FLUTC'
  dd3m = get_dd3m(file0, var3, sav_type)
  FLNTC= dd3m
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

  var3 = 'FSDT'
;         restore, file0 + cmip6_2cesm_var('FSDT')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m
  var3 = 'FSUT'
;         restore, file0 + cmip6_2cesm_var('FSUT')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att
         dd3m   = dda - ddb 

  FSNT = dd3m
  var3 = 'FSNT'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

  var3 = 'FSDT'
  ;       restore, file0 + cmip6_2cesm_var('FSDT')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m

  var3 = 'FSUTC'
;         restore, file0 + cmip6_2cesm_var('FSUTC')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
         dd3m   = dda - ddb 
  FSNTC = dd3m
  var3 = 'FSNTC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

   var3 = 'FSDS'
;         restore, file0 + cmip6_2cesm_var('FSDS')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m
   var3 = 'FSUS'
;         restore, file0 + cmip6_2cesm_var('FSUS')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
         dd3m   = dda - ddb 
  FSNS = dd3m
  var3 = 'FSNS'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

   var3 = 'FSDSC'
;         restore, file0 + cmip6_2cesm_var('FSDSC')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m
   var3 = 'FSUSC'
;         restore, file0 + cmip6_2cesm_var('FSUSC')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
         dd3m   = dda - ddb 
  FSNSC = dd3m
  var3 = 'FSNSC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

    var3 = 'FLUS'
;         restore, file0 + cmip6_2cesm_var('FLUS')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m
    var3 = 'FLDS'
;         restore, file0 + cmip6_2cesm_var('FLDS')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
         dd3m   = dda - ddb 
  FLNS = dd3m
  var3 = 'FLNS'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

    var3 = 'FLUS'
;         restore, file0 + cmip6_2cesm_var('FLUS')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m
    var3 = 'FLDSC'
;         restore, file0 + cmip6_2cesm_var('FLDSC')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
         dd3m   = dda - ddb 
  FLNSC = dd3m
  var3 = 'FLNSC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

    var3 = 'TGCLDLWP'
;         restore, file0 + cmip6_2cesm_var('TGCLDLWP')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         dda = dd3m
    var3 = 'TGCLDIWP'
;         restore, file0 + cmip6_2cesm_var('TGCLDIWP')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         ddb  = dd3m
         dd3m   = dda + ddb 
  TGCLDCWP = dd3m
  var3 = 'TGCLDCWP'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att


 FLNA   = FLNS - FLNT
 dd3m   = FLNA
  var3 = 'FLNA'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 FSNA   = FSNT - FSNS
 dd3m   = FSNA 
  var3 = 'FSNA'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 FLNAC   = FLNSC - FLNTC
 dd3m   = FLNAC
  var3 = 'FLNAC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 FSNAC   = FSNTC - FSNSC
 dd3m   = FSNAC
  var3 = 'FSNAC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 SRFRAD  = FSNS - FLNS
 dd3m   = SRFRAD
  var3 = 'SRFRAD'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 SRFRADC = FSNSC - FLNSC
 dd3m   = SRFRADC 
  var3 = 'SRFRADC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 TOARAD  = FSNT - FLNT
 dd3m   = TOARAD
  var3 = 'TOARAD'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 TOARADC = FSNTC - FLNTC
 dd3m   = TOARADC
  var3 = 'TOARADC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 ATMRAD  = TOARAD - SRFRAD
 dd3m   = ATMRAD
  var3 = 'ATMRAD'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 ATMRADC = TOARADC - SRFRADC
 dd3m   = ATMRADC
  var3 = 'ATMRADC'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 LWCF = FLNTC - FLNT
 dd3m   = LWCF
  var3 = 'LWCF'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 SWCF = FSNT - FSNTC
 dd3m   = SWCF
  var3 = 'SWCF'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 LWCFS = FLNSC - FLNS
 dd3m   = LWCFS
  var3 = 'LWCFS'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 SWCFS = FSNS - FSNSC
 dd3m   = SWCFS
  var3 = 'SWCFS'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 LWCFA = LWCF - LWCFS
 dd3m   = LWCFA
  var3 = 'LWCFA'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 SWCFA = SWCF - SWCFS
 dd3m   = SWCFA
  var3 = 'SWCFA'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 TOTCRF  = LWCF + SWCF
 dd3m   = TOTCRF
  var3 = 'TOTCRF'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3, expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                   nc_files3, climo3,dd3m_att

 TOTCRFS = LWCFS + SWCFS
 dd3m   = TOTCRFS
  var3 = 'TOTCRFS'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

 TOTCRFA = LWCFA + SWCFA
 dd3m   = TOTCRFA
  var3 = 'TOTCRFA'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
 
 print,'  other fields....'

    var3 = 'LHFLX'
;         restore,file0 +  cmip6_2cesm_var('LHFLX')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
 	 LHFLX = dd3m
    var3 = 'SHFLX'
;         restore,file0 +  cmip6_2cesm_var('SHFLX')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
 	 SHFLX = dd3m
    var3 = 'PRECT'
;         restore,file0 +  cmip6_2cesm_var('PRECT')+'.sav'
         dd3m = get_dd3m(file0, var3, sav_type)
         PRECT = dd3m

 QFLX = LHFLX / 2.5e6
 dd3m   = QFLX
  var3 = 'QFLX'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

 PME = PRECT - QFLX
 dd3m   = PME
  var3 = 'PME'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
  
 SRFNET = SRFRAD - LHFLX - SHFLX
 dd3m   = SRFNET
  var3 = 'SRFNET'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3,nc_files3, climo3,dd3m_att

 ATMNET = TOARAD - SRFNET
 dd3m   = ATMNET 
  var3 = 'ATMNET'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3,nc_files3, climo3,dd3m_att

 PRECL  = PRECT*0.0
 dd3m   = PRECL
  var3 = 'PRECL'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

 PRECSL  = PRECT*0.0
 dd3m   = PRECSL
  var3 = 'PRECSL'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
 

;;  if(file_exist(file + cmip6_2cesm_var('sftlf')+'.sav')) then begin
;;    var3 = 'sftlf'
;         restore, file0 + cmip6_2cesm_var('sftlf')+'.sav' 
;;         dd3m = get_dd3m(file0, var3, sav_type, , lon3 = lon3, lat3=lat3,lev3=lev3)
;;  endif else begin
;;         dd3m = large_number
;;  endelse     
;; LANDFRAC = dd3m
;; dd3m   = LANDFRAC
;;  var3 = 'LANDFRAC'
;;  filesave = file + var3+'.sav'
;;  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
;;                  nc_files3, climo3,dd3m_att

;;  if(file_exist(file + cmip6_2cesm_var('siconc')+'.sav')) then begin
;;    var3 = 'siconc'
;;;         restore, file0 + cmip6_2cesm_var('siconc') +'.sav'
;;         dd3m = get_dd3m(file0, var3, sav_type,lon3 = lon3, lat3=lat3,lev3=lev3)
;;  endif else begin
;;         dd3m = large_number
;;  endelse     
;; ICEFRAC = dd3m
;; dd3m   = ICEFRAC
;;  var3 = 'ICEFRAC'
;;  filesave = file + var3+'.sav'
;;  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
;;                  nc_files3, climo3,dd3m_att

;;  if(file_exist(file + cmip6_2cesm_var('snowc')+'.sav')) then begin
;;    var3 = 'snowc'
;;;         restore, file0 + cmip6_2cesm_var('snowc') +'.sav'
;;         dd3m = get_dd3m(file0, var3, sav_type)
;;  endif else begin
;;         dd3m = large_number
;;  endelse     
;; SNOWHLND= dd3m
;; dd3m   = SNOWHLND
;;  var3 = 'SNOWHLND'
;;  filesave = file + var3+'.sav'
;;  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
;;                  nc_files3, climo3,dd3m_att

;;  if(file_exist(file + cmip6_2cesm_var('tos')+'.sav')) then begin
;;    var3 = 'tos'
;;;         restore, file0 + cmip6_2cesm_var('tos') +'.sav'
;;         dd3m = get_dd3m(file0, var3, sav_type)
;;  endif else begin
;;         dd3m = large_number
;;  endelse     
;; SST= dd3m
;; dd3m   = SST
;;  var3 = 'SST'
;;  filesave = file + var3+'.sav'
;;  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
;;                  nc_files3, climo3,dd3m_att

  dd3m = - 999990.
  if(file_exist(file + cmip6_2cesm_var('orog')+'.*')) then begin
    var3 = 'orog'
;         restore, file0 + cmip6_2cesm_var('orog') +'.sav'
         dd3m = get_dd3m(file0, var3, sav_type, lon3 = lon2, lat3=lat2,lev3=lev2)
    if(array_equal(lon2, lon3) and array_equal(lat2, lat3))then begin
         ORO = dd3m
    endif else begin
         ORO = my_interpol2(dd3m, lon2,lat2,lon3,lat3, tdim=0)
    endelse
         dd3m = ORO

  endif else begin
         dd3m = large_number
  endelse     
  ORO= dd3m
  var3 = 'ORO'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

 PHIS = ORO*9.8
 dd3m   = PHIS
  var3 = 'PHIS'
  filesave = file + var3+'.sav'
  save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

; 3d fields  H and THETA calculated in cmip6_fill_lowest_missing

print,''
print,'  used files in ', old_sav_path, ' to create files in ', new_sav_path
print,''
spawn, 'ls -ltr ' + new_sav_path + '/*', list
print,list

return


end
