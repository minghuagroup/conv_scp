function callev,a,n

  dd = [min(a),max(a)]
  if(dd[0] eq dd[1])then dd[1] = dd[0]+1.
  lev = cal_lev(dd,n)
  return,lev

end
