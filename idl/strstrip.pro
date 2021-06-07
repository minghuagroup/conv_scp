function strstrip, stra, str, last = last


 if(not keyword_set(last))then last = 0 

 if(n_elements(stra) eq 1 )then begin

 len = strlen(stra)

 if(not keyword_set(last))then  begin
   j = strpos(stra,str)
   strb = strmid(stra, j+1, len-j)
   return, strb
 endif else begin
   strc = str_reverse(stra)
   strd = strstrip(strc,str)
   stre = str_reverse(strd)
   return, stre
 endelse

 
 endif else begin

   nv = n_elements(stra)
   stre2 = strarr(nv)
   for i = 0, nv-1 do begin
     stre2[i] = strstrip(stra[i], str, last = last  )
   endfor

 return, stre2
 endelse


 
end

 
 
 
   

