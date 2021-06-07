 function set_bounds, dj2, lev1
  jj=where(dj2 le min(lev1),cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj=where(dj2 ge max(lev1)-1.0e-3,cnt)
  if(cnt gt 0)then dj2[jj] = max(lev1)-1.0e-3
  return, dj2
 end


