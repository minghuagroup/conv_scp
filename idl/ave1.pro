
function ave1,d,missing=missing

  dd=d

 if(not keyword_Set(missing))then missing = -999999.
  large_number = missing

   dj = missing

;if(missing lt 0)then jj = where(dd gt missing, cnt) else jj = where(dd lt missing, cnt)
   jj=where(abs(dd) lt abs(large_number) ,cnt)
   if(cnt gt 0)then dj=mean(dd[jj]) 
 return,dj

end

