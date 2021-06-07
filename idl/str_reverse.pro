function str_reverse, stra
 len = strlen(stra)
 strb = ''
 for i = 0,len-1 do begin
  j = len-i-1
  strb = strb + strmid(stra, j,1 )
 endfor
 return, strb
end

 
 
 
   

