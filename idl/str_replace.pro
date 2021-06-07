function str_replace, str0, in, out

 if(n_elements(str0) eq 1)then begin

 str = str_sep(str0,in)
 if(n_elements(str) eq 1)then return,str0

 str1 = str[0]
 for i = 1, n_elements(str) -1 do begin
  str1 = str1 + out + str[i]
 endfor
 return, str1

endif else begin
  nv = n_elements(str0)
  str4 = str0
  for k = 0, nv-1 do begin
    str4[k] = str_replace(str0[k], in, out)
  endfor
  return, str4
endelse

end
~                                                                                                                             

