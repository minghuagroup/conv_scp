function get_lev_diff,aa,nn, missing = missing, contrast = contrast

  if(not keyword_Set(missing))then missing = 1.0e20
  if(not keyword_Set(contrast))then contrast = 1

  jj = where(abs(aa) lt abs(missing), cnt)
  if(cnt le 0)then return,[0,1]
  
  mx = abs(max(aa[jj]))
  mn = abs(min(aa[jj]) )

  v1 = max([mx,mn]) * contrast
  
  lev = cal_lev([-v1,v1],nn)

 return, lev

end
