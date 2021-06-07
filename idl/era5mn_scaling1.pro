
missing = 1.0e15

 ; era5mn_scaling

 era5_vars = era5_Vars()
 vars_m    = strupcase( era5_Vars.vars_m)
 vars_s    = strupcase(era5_Vars.vars_s)
 vars_v    = strupcase(era5_Vars.vars_v)
 vars_fm   = strupcase(era5_Vars.vars_fm)
 vars_fi   = strupcase(era5_Vars.vars_fi)
 vars_ac   = strupcase(era5_Vars.vars_ac)
 vars_era5_all   = strupcase(era5_Vars.vars_era5_all)
 vars_number   = strupcase(era5_Vars.vars_number)
 VARS_ERA5_STR_ALL = era5_vars.vars_era5_str_all


; openw, 1, 'get_lev_era5.pro'
 openw, 1, 'j.txt'

  printf,1, ''
  printf,1, '---------------------------------------------------------'
  print, 1, 'function get_lev_era5, dd, var'
  printf,1, '---------------------------------------------------------'
  printf,1, ''

  text = 'era5_lev = create_struct("era5_vars", era5_vars) '
  printf,1, text

 ; look through all fields to check max and min
 for iv = 0, n_elements(vars_era5_all)-1 do begin

  var0 = vars_era5_all[iv]
  if(belongsto(var0, vars_number))then var0 = 'VAR_'+var0

  var  = var0
  if(belongsto(var, vars_m))then var = 'ML_'+var0
 
  file = '../cmip6/ERA5/climo/1979_1988/nc/'+var + '.nc'
 if(file_exist(file))then begin
    aa   = get_fld(file, var0)
    att   = get_cdf_att(file, var0, 'units')
    
    dd  = aa
    jj= where(abs(dd) ge missing, cnt)
  
    if(cnt gt 0)then dd[jj] = !Values.F_NAN

    varj = var0
    if(belongsto(var0, vars_ac))then begin
       dd = dd/86400.
       varj = var0+'_AC' 
     endif

   text0 = strtrim(min(dd),2) +', ' +strtrim( max(dd), 2)+',  ' + strtrim( mean(dd),2) 
   text1 =   "'"+era5_vars.vars_era5_str_all[iv] + "', '" + att + "'" 

   
   text = "       era5_lev = create_struct(era5_lev, '"+ var0 + "', [ " + text0 +"  ])"
   text2 = "       era5_lev = create_struct(era5_lev, '"+ var0 + "_str', [ " + text1 +"  ])"
   printf,1, ''
    printf,1,text
    printf,1,text2
    print,iv, '  ', varj, ' ', min(dd), max(dd), mean(dd), '      ', att, '     ',era5_vars.vars_era5_str_all[iv]  ;,'  ', file
  endif

 endfor

 close,1

end
