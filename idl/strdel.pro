function strdel, stra, str, last = last

; delete one occurance, either front or last

 if(not keyword_set(last))then last = 0 

 if(n_elements(stra) eq 1 )then begin

 len = strlen(stra)

 if(not keyword_set(last))then  begin
   strb = stra
   j = strpos(stra,str)
   len = strlen(str)
   len0 = strlen(stra)
   if(j[0] eq -1)then return, strb
   if(j[0] eq 0)then begin
     strb = strmid(stra, j+len, len0-len)
   endif else begin
     strb = strmid(stra,0,j) + strmid(stra,j+len,len0-len-j)
   endelse
   ;stop
 
   return, strb
 endif else begin
   strc = str_reverse(stra)
   strd = strdel(strc,str)
   stre = str_reverse(strd)
   return, stre
 endelse
 
 endif else begin

   nv = n_elements(stra)
   stre2 = strarr(nv)
   for i = 0, nv-1 do begin
     stre2[i] = strdel(stra[i], str, last = last  )
   endfor

 return, stre2
 endelse


 
end

 
 
 
   

