
function ave10,d,missing=missing

  dd=d

 if(not keyword_Set(missing))then missing = 0
  large_number = missing

   dj = 0

   jj=where(abs(dd) gt abs(large_number) ,cnt)
   if(cnt gt 0)then dj=mean(dd[jj]) 
 return,dj

end

