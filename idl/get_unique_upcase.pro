

 function get_unique_upcase, varsj, uniq_vars = varsj4

  cmip6_vars = cmip6_vars()
  nv = n_elements(varsj)
  index = indgen(nv)

  kk = 0
  varsj2 = varsj
  ; get upcase
  for i= 0, nv-1 do begin
    ;;if(varsj[i] eq strupcase(varsj[i]))then begin
    if(varsj[i] ne strupcase(varsj[i]) or (not belongsto (varsj[i], cmip6_vars.vars_cmip6)))then begin

      varsj2[kk] = varsj[i]
      kk = kk+1
    endif
  endfor
  varsj2 = varsj2[0:kk-1]
 
  kk = 0
  varsj3 = varsj
  for i= 0, nv-1 do begin
    ;if(varsj[i] eq strupcase(varsj[i]))then begin
    if(varsj[i] ne strupcase(varsj[i]) or (not belongsto (varsj[i], cmip6_vars.vars_cmip6)))then begin
      varsj3[kk] = varsj[i]
      index[kk]  = i
      kk = kk+1
    endif else begin
      if(not belongsto(cmip6_2cesm_var(varsj[i],c=1),varsj2 )) then begin
      varsj3[kk] = varsj[i]
      index[kk]  = i
      kk = kk+1
      endif
    endelse
  endfor

  varsj4 = varsj3[0:kk-1]
  index  = index[0:kk-1] 

  return, index

end
