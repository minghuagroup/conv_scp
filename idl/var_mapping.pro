
function var_mapping, var, name
;=================================================
; dictionary

name0 = name

nv = n_elements(var)
if (nv eq 1)then begin
case name0 of
  'era2cesm':  return, era5_2cesm_var(var,c=1)
  'cmip2cesm': return, cmip6_2cesm_var(var,c=1)
  'cmip2era':  return, cmip6_2era5_var(var,c=1)
  'cesm2era':  return, era5_2cesm_var(var,c=0)
  'cesm2cmip': return, cmip6_2cesm_var(var,c=0)
  'era2cmip':  return, cmip6_2era5_var(var,c=0)
  'all2cesm':  begin
               varj = var
               if(varj eq var)then varj = era5_2cesm_var(var,c=1)
               if(varj eq var)then varj = cmip6_2cesm_var(varj,c=1)
               return, varj
               end
  'all2cmip':  begin
               varj = var
               if(varj eq var)then varj = cmip6_2era5_var(var,c=0)
               if(varj eq var)then varj = cmip6_2cesm_var(varj,c=0)
               return, varj
               end
  'all2era':  begin
               varj = var
               if(varj eq var)then varj = cmip6_2era5_var(var,c=1)
               if(varj eq var)then varj = era5_2cesm_var(varj,c=0)
               return, varj
               end

  else:        return, var
endcase 
return,var  

endif else begin ; nv

 vars2 = var
 for i = 0, nv-1 do begin
   vars2[i] = var_mapping(var[i],name)
 endfor
endelse
 
 return, vars2
   

end ;============================
