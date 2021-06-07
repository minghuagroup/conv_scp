 function dlimit, dd0, amin, amax
  dd = dd0
  jj = where(dd gt amax,cnt)
  if(cnt gt 0)then dd[jj] = amax

  jj = where(dd lt amin,cnt)
  if(cnt gt 0)then dd[jj] = amin

  return, dd
 end


