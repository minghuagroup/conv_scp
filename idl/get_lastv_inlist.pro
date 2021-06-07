function get_lastv_inlist, vars, sep, rev=rev

 ; rev=1, delete the last element

  jvars = vars
  for i = 0, n_elements(vars)-1 do begin
    jvs = reverse(str_sep(vars[i],sep))
    jvars[i] = jvs[0]
  endfor

 if(not keyword_Set(rev))then return, jvars

  for i = 0, n_elements(vars)-1 do begin
    jvs = strpos(vars[i],jvars[i])
    jvars[i] = strmid(vars[i], 0, jvs-1)
  endfor
  return, jvars


end

