function get_dd2m, file0, var_in,lon3 = lon3, lat3=lat3,lev3=lev3,$
         time3=time3, dd3m_att = dd3m_att, $
         cntl_file=cntl_file, cntl_map = cntl_map

; =======================================================================================================
; - EXAMPLE

; FLNT =  get_fld('../cmip6/CMIP/NCAR_CESM2/amip/nc',var_map('FLNT','cesm2cmip') )
; or
; DFLNT =  get_fld('../cmip6/CMIP/NCAR_CESM2/amip/nc',var_map('FLNT','cesm2cmip'), $  ; get cmip var since FLNT is in cesm
;;        cntl_file = '../cmip6/ERA5/climo/1979_1988/nc',cntl_map = 'cmip2era'))   ;from cmip to get the era field
;
; cntl_map = 'none' means the same between the two runs
; =======================================================================================================
; file0 can be a folder

  var0     = var_in 
  filein   = file0 

  if(strpos(file0,'ERA5') ge 0)then begin  
    vars_ec = era5_vars()
    if(belongsto(var0, vars_ec.vars_number))then var0 = 'VAR_'+var0
  endif

;  print,var0,' ', file0

  filein = get_ncfile(file0, var0)

; The following is to skip reading data if matching file does not exist
  if(keyword_Set(cntl_file)) then begin
    if(not keyword_set(cntl_map))then cntl_map = 'none'
    varj2 = var_map(var0, cntl_map) 
    if(strpos(cntl_file,'ERA5') ge 0)then begin  
        vars_ec = era5_vars()
        if(belongsto(varj2, vars_ec.vars_number))then varj2 = 'VAR_'+varj2
    endif
    fileinj = get_ncfile(cntl_file, varj2)
    match   = file_exist(filein) and file_exist(fileinj)
    if(not match)then begin
      print,' no matching var from ', filein, '  ', fileinj
      return, -999999.
    endif
  endif

;  print,var0,' ---------> ', filein

  ncdf_Vars, filein, vars_tmp, no_print=1

;stop
  dd2m = get_fld(filein, var0)
  dd2m_att = get_cdf_att_all(filein,var0)

  lat3 = get_fld(filein, 'lat')
  lon3 = get_fld(filein, 'lon')
  ncdf_Vars, filein, vars_tmp, no_print =1

  lev3 = [1000.]
  if(belongsto('lev',vars_tmp))then lev3 = get_fld(filein,'lev')     ; cam
  if(belongsto('plev',vars_tmp))then lev3 = get_fld(filein,'plev')   ; cmip
  if(belongsto('level',vars_tmp))then lev3 = get_fld(filein,'level') ; era5
; taking care the NCAR CMIP results
  if(max(lev3) le 0.)then lev3 = abs(lev3)
  if(max(lev3) lt 2.)then lev3 = lev3*1000.
  if(max(lev3) gt 2000)then lev3 = lev3/100.
             
  time3 = [0.0]
  if(belongsto('time',vars_tmp))then begin
      time3 = get_fld(filein,'time')
  endif

; ----------------------------------------
  if(not keyword_Set(cntl_file))then return, dd2m
    
; -------------------------
; Make difference fields
; -------------------------

  var00 = var_map(var0, cntl_map)  ; e.g., cmip to era
;  print,var0, var00, cntl_map

;stop
  
  dd0m = get_dd2m(cntl_file, var00, lon3 = lon0, lat3=lat0,lev3=lev0,$
                 time3=time0, dd3m_att = dd3m_att0, cntl_file = 0)

  no_inter = array_equal(lon3,lon0) and array_equal(lat3, lat0) and array_equal(lev3,lev0)
  if(not no_inter)then begin
     sz = size(dd2m)
     if(n_elements(lev3) eq 1) then begin ; 2D array 
       tdim = sz[0] - 2   
       dd0m = my_interpol2(dd0m, lon0,lat0, lon3, lat3, tdim=tdim)
     endif else begin
       tdim = sz[0] - 3   

        ; check if vertical coordinates are opposite
        zzj = lev0
        if((lev3[1]-lev3[0])*(lev0[1] - lev0[0]) lt 0) then begin
          zzj = reverse(lev0) 
          dd0m = reverse(dd0m,3)
        endif

       print,'making interpolations for ....  ', var00
       dd0m2 = dd0m
  ;stop
       dd0m = my_interpol3d(dd0m, lon0, lat0, zzj, lon3, lat3, lev3, tdim = tdim )
     endelse
  endif

  dd2m = less_100(dd2m, var_map(var0,  'all2cesm'))
  dd0m = less_100(dd0m, var_map(var00, 'all2cesm'))

  dd2m = dd2m - dd0m

  return, dd2m

end

