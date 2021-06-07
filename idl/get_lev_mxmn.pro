function get_lev_mxmn,aa,nn, missing = missing, scale = scale

  if(not keyword_Set(missing))then missing = 1.0e20
  if(not keyword_Set(scale))then scale = 1

  jj = where(abs(aa) lt abs(missing), cnt)
  if(cnt le 0)then return,[0,1]
  
  mx = max(aa[jj])
  mn = min(aa[jj]) 

  lev = cal_lev([mn,mx],nn)

 return, lev

end
