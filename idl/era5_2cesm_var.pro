
function era5_2cesm_var, var0, cesm = cesm, v=v , m=m

;restore, 'era5_vars.sav0' 
;; help,vars_era5, vars_cesm
; m to return duplicates
; v to add 'VAR_

era5_vars = era5_vars()
vars_era5 = era5_vars.vars_era5_all
vars_cesm = era5_vars.vars_era5_cesm

vars0_era5 = strupcase(vars_era5)
var = strupcase(var0)
if(keyword_set(cesm))then begin
    jj = where(vars0_era5 eq strstrip(var,'_'),cnt)
    ;stop
  if(keyword_Set(m))then begin  
    if(cnt gt 0)then return, vars_cesm[jj] else return, var0  ; take the priority list!
  endif else begin
    if(cnt gt 0)then return, vars_cesm[jj[0]] else return, var0  ; take the priority list!
  endelse
 endif
   
jj = where(vars_cesm eq var,cnt)
if(cnt gt 0)then begin
  if(keyword_Set(m))then begin  
       varj = vars_era5[jj] 
  endif else begin
       varj = vars_era5[jj[0]] 
       if(keyword_set(v) and belongsto(varj,['2t','2d','100u','100v','10u','10v','10si']))then varj = 'VAR_'+strupcase(varj)
  endelse
  return,strupcase(varj)
endif else begin
  return, var0  ; take the priority list!
endelse

 end
 
