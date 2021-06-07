function var_duplicate, var, up=up

cmip6_vars = cmip6_vars()

 vars = cmip6_vars.vars_cmip6

 vars_low   = strarr(200)
 vars_up    = strarr(200)

 k = 0
 for i = 0, n_elements(vars)-1 do begin
   var0 = vars[i]
   var_Cam = cmip6_2cesm_var(var0, c=1)
   if(belongsto(var_Cam, cmip6_vars.vars3d))then begin

;     print,i, ' ',var_cam
     var_Cam = cmip6_2cesm_var(var0, c=1)
     vars_low[k] = var0
     vars_up[k]    = var_Cam
     k = k+1
   endif
 endfor

 vars_low = vars_low[0:k-1]
 vars_up  = vars_up[0:k-1]

 if(not belongsto(var,[vars_low,vars_up]))then return,0

 if(keyword_set(up)) then begin
  if(not belongsto(var,vars_low))then return,1
  return,0
 endif
  if(not belongsto(var,vars_up))then return,1
  return,0
end

