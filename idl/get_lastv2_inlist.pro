function get_lastv2_inlist, vars, sep, sep2, sep3 = sep3

  vars2 = get_lastv_inlist(vars,sep)
  jvars = vars
  for i = 0, n_elements(vars2)-1 do begin
    jvs = str_sep(vars2[i],sep2)
    vars2[i] = jvs[0]
  endfor
  
  if(keyword_Set(sep3))then begin
   for i = 0, n_elements(vars2)-1 do begin
    vj = reverse(str_sep(Vars2[i],sep3))
    vars2[i] = vj[0]
   endfor
   endif

  return, vars2

end

