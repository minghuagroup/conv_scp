
function cmip6_2cesm_var, var, cm2ce = cm2ce 
;=================================================
; dictionary
; cm2ce= 1 frin CMIP to CESM variables

cmip6_vars = cmip6_vars()

;defined dictionary relationships
vars_cmip  = [cmip6_vars.vars_atm,'orog', 'sftlf', 'sicon',  'snc',      'tos']
vars_cesm  = [cmip6_vars.vars_cam,'ORO','LANDFRAC','ICEFRAC','SNOWHLND', 'SST']

;vars_cmip = [vars_cmip, cmip6_vars.vars_ocn, cmip6_vars.vars_lnd, cmip6_vars.vars_lice, cmip6_vars.vars_sice, cmip6_vars.vars_fx]
;vars_cesm = [vars_cesm, cmip6_vars.vars_ocn, cmip6_vars.vars_lnd, cmip6_vars.vars_lice, cmip6_vars.vars_sice, cmip6_vars.vars_fx]

if(keyword_Set(cm2ce))then begin
; from cmip to CESM
  jj = where(vars_cmip eq var, cnt)  ; find the index in cesm vars order  if(cnt eq 0)then begin
  if(cnt le 0)then begin
   ;print,var, '  no variable matches from CESM field to CMIP fields, returns NNN'
   ;return, 'NNN'
   return,var  ; return the original name
  endif
  return, vars_cesm[jj[0]]
endif

; from CESM to CMIP
  jj = where(vars_cesm eq var, cnt)  ; find the index in cesm vars order
  if(cnt eq 0)then begin
   ;print,var, '  no variable matches from CESM to CMIP, returns NNN'
   ;return, 'NNN'
   return,var  ; return the original name
  endif
  return, vars_cmip[jj[0]]

end ;============================
