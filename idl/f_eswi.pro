 function f_eswi,temp,thresh=thresh

  if(not keyword_set(thresh))then thresh = 20.

   es = f_es(temp)
   
   tice= temp+273.16
   esi = 23.33086-6111.72784/tice + 0.15215*alog(tice)
   esi = exp(esi)

   jj2 = where(temp lt -thresh,cnt2) 
   if(cnt2 gt 0)then es[jj2] = esi[jj2]

   jj=where((temp lt 0.) and (temp ge -thresh),count)

  if(count ge 1) then begin
   alpha = (273.16- tice)/thresh  
   es(jj) = alpha[jj]*esi[jj] + (1.-alpha[jj])*es(jj)
  endif
  return,es
 end


