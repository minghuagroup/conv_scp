
function var_map, var, name, v=v
;=================================================
; dictionary
; cm2ce= 1 frin CMIP to CESM variables

name0 = name

if(n_elements(var) eq 1)then begin
case name0 of
  'era2cesm':  return, era5_2cesm_var(var,c=1,v=v)
  'cesm2era':  return, era5_2cesm_var(var,c=0,v=v)
  'cmip2cesm': return, cmip6_2cesm_var(var,c=1)
  'cesm2cmip': return, cmip6_2cesm_var(var,c=0)
  'cmip2era':  begin
                varj = var_map(var, 'cmip2cesm')
                return, var_map(varj,'cesm2era')
               end
  'era2cmip':  begin
               varj = var_map(var, 'era2cesm')
               return, var_map(varj,'cesm2cmip')
               end
  'all2cesm':  begin
               varj = var
               if(varj eq var)then varj = var_map(var, 'era2cesm') 
               if(varj eq var)then varj = var_map(var, 'cmip2cesm')
               return, varj
               end
  'all2cmip':  begin
               varj = var
               if(varj eq var)then varj = var_map(var, 'era2cmip')
               if(varj eq var)then varj = var_map(var, 'cesm2cmip')
               return, varj
               end
  'all2era':  begin
               varj = var
               if(varj eq var)then varj = var_map(var, 'cmip2era')
               if(varj eq var)then varj = var_map(var, 'cesm2era')
               return, varj
               end

  else:        return, var
endcase 
return,var  


endif

var2   = var
for iv = 0,n_elements(var)-1 do begin
 var2[iv] = var_map(var[iv],name)
endfor
 return,var2
end ;============================
