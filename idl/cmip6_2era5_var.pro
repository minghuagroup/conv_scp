
function cmip6_2era5_var, var, c=c, v=v
; era5 to cmip6, if keyword set, then to get era5 variable name from cmip6

restore, 'era5_vars.sav0' 
; help,vars_era5, vars_cesm
; stop
if(keyword_set(c))then begin
     varj2 = era5_2cesm_var(var,c=0,v=v)
     varj = cmip6_2cesm_var(varj2,c=1)
     
     ;stop
     return, varj
 endif
 
 varj2 = era5_2cesm_var(var, c=1, v=v)
 varj  = cmip6_2cesm_var(varj2, c=0)
;stop
 return, varj 

  
end