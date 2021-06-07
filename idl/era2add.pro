

pro era2add, old_sav_path ;, new_sav_path

;old_sav_path = '../cmip6/ERA5/climo/1979_1988/nc/'
;old_sav_path = '../cmip6/ERA5/climo/1989_1998/nc/'
;old_sav_path = '../cmip6/ERA5/climo/1999_2008/nc/'
;old_sav_path = '../cmip6/ERA5/climo/2009_2018/nc/'

new_sav_path = old_sav_path

 print,''
 print,'  calculating and saving additional era5 fields  '
 print,''

  large_number = 999990.

; retrive and rewrite saved data as follows
; data from running cmip2sav.pro
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

  file0 = old_sav_path 
  file  = new_sav_path 

  cmip6_vars = cmip6_vars()
  era5_vars = era5_vars()
  vars_ac   = strupcase(era5_vars.vars_ac)
  vars_atm = cmip6_vars.vars_atm
  nv = n_elements(vars_atm)

restore, 'scale_era5.sav0'
scale_names = tag_names(scale_era5)

  
 ; input data
 vars_in = ['SOLIN','FSDS','FLDS','FSNS','FLNS','FLNT','FSNT','FLNTC','FSNTC','FSNSC','FLNSC','FSDSC','FLDSC',$
  'LHFLX','SHFLX','QFLX','PRECT','T','Q','Z3','TS','TREFHT']
  
 vars_out = ['FLNA','FSNA','FLNAC','FSNAC','SRFRAD','SRFRADC','TOARAD','TOARADC','ATMRAD','ATMRADC', $
    'LWCF','SWCF','LWCFS','SWCFS','LWCFA','SWCFA','TOTCRF','TOTCRFS','TOTCRFA', $
    'PME','SRFNET','ATMNET', $
    'H','THETA','TSDIFF']
  
 data_in = create_struct('fields',vars_in)
 
 scale = 1.0
 for iv = 0, n_elements(vars_in)-1 do begin
     var   = vars_in[iv]
     var0s  = era5_2cesm_var(var,c=0,m=1)
     var0   = var0s[0]
     fvar0  = var0
     if(belongsto(var0, strupcase(era5_vars.vars_m))) then begin
       fvar0 = 'ML_'+var0 
       if(belongsto(var0, scale_names))then scale = get_stru(scale_era5,var0)
     endif else begin

       for k = 0, n_elements(var0s)-1 do begin
         if(file_exist(old_sav_path + var0s[k] + '.nc')  )then begin
           var0 = var0s[k]
           var1 = strupcase(var0)
           if(belongsto(var1, scale_names))then begin
            scale = get_stru(scale_era5,var1)
           endif
           if(belongsto(var0, vars_ac))then scale = scale/86400.
            goto, jump1
          endif
       endfor 
       jump1:
       fvar0 = var0
     endelse 
     
     file0 = old_sav_path + fvar0 + '.nc'
     if(var0 eq '2T')then begin
;     print,var0
       file0 = old_sav_path + 'VAR_'+fvar0 + '.nc'
       dd_in      = get_fld(file0, 'VAR_2T')  * scale
       dd_in_att  = get_cdf_att_all(file0, 'VAR_2T')
;     print,var0
     endif else begin
       dd_in      = get_fld(file0, var0)  * scale
       dd_in_att  = get_cdf_att_all(file0, var0)
    endelse
     
     data_in = create_struct(data_in, var, dd_in, var+'_att', dd_in_att, var+'_file', file0)
  endfor
  
;stop
 TSDIFF   = get_stru(data_in, 'TS') - get_stru(data_in, 'TREFHT')
 FLNA   = get_stru(data_in, 'FLNS') - get_stru(data_in, 'FLNT')
 FSNA   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNS')
 FLNAC   = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNTC')
 FSNAC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FSNSC')
 SRFRAD   = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FLNS')
 SRFRADC   = get_stru(data_in, 'FSNSC') - get_stru(data_in, 'FLNSC')
 TOARAD   = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FLNT') 
 TOARADC   = get_stru(data_in, 'FSNTC') - get_stru(data_in, 'FLNTC') 
  
 ATMRAD  = TOARAD - SRFRAD 
 ATMRADC = TOARADC - SRFRADC 
 
 LWCF = get_stru(data_in, 'FLNTC') - get_stru(data_in, 'FLNT')
 SWCF = get_stru(data_in, 'FSNT') - get_stru(data_in, 'FSNTC')
 LWCFS = get_stru(data_in, 'FLNSC') - get_stru(data_in, 'FLNS')
 SWCFS = get_stru(data_in, 'FSNS') - get_stru(data_in, 'FSNSC')
 LWCFA = LWCF - LWCFS
 SWCFA = SWCF - SWCFS
   
 LWCFA = LWCF - LWCFS
 SWCFA = SWCF - SWCFS
 TOTCRF  = LWCF + SWCF
 TOTCRFS = LWCFS + SWCFS
 TOTCRFA = LWCFA + SWCFA
 
 PME = get_stru(data_in, 'PRECT') - get_stru(data_in, 'QFLX') 
    
 SRFNET = SRFRAD - get_stru(data_in, 'LHFLX') - get_stru(data_in, 'SHFLX')  
 ATMNET = TOARAD - SRFNET
 

 
 T = get_stru(data_in, 'T')

 Q = get_stru(data_in, 'Q')
 Z3 = get_stru(data_in, 'Z3')
 H = T + 1./1004.* Z3 + 2.5e6/1004.*Q/1000.
 
 file0 = get_stru(data_in, 'T_file')
 lev = get_fld(file0,'level')
 pp = T*0.0
 for k = 0, n_elements(lev)-1 do begin
   pp[*,*,k,*] = lev[k]
 endfor
 THETA = T*(1000./pp)^0.287

 ;stop
; print,lev


 data_out =create_struct('fields', vars_out, 'FLNA',FLNA, 'FSNA',FSNA, 'FLNAC',FLNAC,'FSNAC',FSNAC, $
  'SRFRAD',SRFRAD,'SRFRADC',SRFRADC,'TOARAD',TOARAD,'TOARADC',TOARADC,'ATMRAD',ATMRAD,'ATMRADC',ATMRADC, $
   'LWCF',LWCF, 'SWCF',SWCF,'LWCFS',LWCFS,'SWCFS',SWCFS,'LWCFA',LWCFA,'SWCFA',SWCFA,$
   'TOTCRF',TOTCRF,'TOTCRFS',TOTCRFS,'TOTCRFA',TOTCRFA, $
   'PME',PME,'SRFNET',SRFNET,'ATMNET',ATMNET, $
   'H',H, 'THETA', THETA,'TSDIFF',TSDIFF)

 for iv = 0, n_elements(vars_out)-1 do begin
  var = vars_out[iv]
  file = new_sav_path + var + '.nc'
  ;file =  'jj.nc'
  dd = get_stru(data_out, var)
  sz = size(dd)
  if(sz[0] eq 3)then begin
    file_tmp = old_sav_path + 'VAR_2T.nc'
    var_tmp = 'VAR_2T
  endif else begin
    file_tmp = old_sav_path + 'ML_T.nc'
    var_tmp = 'T
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
  NCDF_ATTDEL, fid, Varid , 'ecmwf_local_table' 
  NCDF_ATTDEL, fid, Varid , 'ecmwf_parameter' 
  NCDF_ATTDEL, fid, Varid , 'minimum_value' 
  NCDF_ATTDEL, fid, Varid , 'maximum_value'
  ncdf_close,fid
  print,' saved file ', iv, ' ', file
   print, iv,'  ', var, '  ',min(dd), max(dd), mean(dd)
 endfor

return


end
