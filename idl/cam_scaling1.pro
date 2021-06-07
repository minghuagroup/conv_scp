
missing = 1.0e15

input_file = '../data/cam5_data/lens_ANN_climo3.nc'


 ; cammn_scaling

 cmip6_vars = cmip6_Vars()
 cam_vars   = strupcase(cmip6_Vars.vars_cam_all)
 cam_vars   = ncdf_vars2d(input_file )

restore, 'scale_era5.sav0'   ; after running era5_scaling
era_lev_names = tag_names(era5_lev)


; openw, 1, 'get_lev_cam.pro'
 close,1
 openw, 1, 'j.txt'

  printf,1, ''
  printf,1, '---------------------------------------------------------'
  printf, 1, ' function get_lev_cam, dd, var'
  printf,1, '---------------------------------------------------------'
  printf,1, ''

  text = ' cam_lev = create_struct("cam_vars", cam_vars) '
  printf,1, text

 ; loop through all fields to check max and min

 cam_vars = cam_vars(sort(cam_vars))

 for iv = 0, n_elements(cam_vars)-1 do begin
; =========================================

  var0 = cam_vars[iv]
  dd   = get_fld(input_file, var0)
  att   = get_cdf_att(input_file, var0, 'units')

   printf,1, ''

; borrow from era

   levinfo2 = '[0,0,0])   ;'

   var_era = era5_2cesm_var(var0)
   if(belongsto(var_era, era_lev_names))then begin
     levinfo = get_stru(era5_lev,var_era) 
;     if(n_elememts(levinfo) lt 3)then, levinfo = levinfo2
   endif else begin
     lev = get_lev([0.,1], var0, scale)
     levinfo = [min(lev),max(lev),scale]
   endelse
    
   levinfo = strtrim(levinfo,2)
   levinfo2 = '[' + levinfo[0] + ', ' + levinfo[1] + ', ' + levinfo[2] + '])    ;'

   text0 = strtrim(min(dd),2) +', '  + strtrim( max(dd), 2)+',  ' + strtrim( mean(dd),2) 
   text = "       cam_lev = create_struct(cam_lev, '"+ var0 + "', " + levinfo2 + " [ " + text0 +"  ])"
   printf,1,text

   text2 = "       cam_lev = create_struct(cam_lev, '"+ var0 + "_unit',  '" + att + "'  )"
   printf,1,text2

   print,iv, '  ', var0, ' ', min(dd), max(dd), mean(dd)

 endfor

 print, ' ..... sketch file printed in j.txt'

 close,1

end
