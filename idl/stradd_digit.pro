function stradd_digit, stra, str = str


 if(not keyword_set(str))then str = 'X'

 if(n_elements(stra) eq 1 )then begin

  stra2 = stra
  if (valid_num( strmid(stra,0,1))) then stra2 = str+stra
  return, stra2

 endif else begin

   nv = n_elements(stra)
   stre2 = strarr(nv)
   for i = 0, nv-1 do begin
     stre2[i] = stradd_digit(stra[i], str = str)
   endfor

 return, stre2
 endelse



end


