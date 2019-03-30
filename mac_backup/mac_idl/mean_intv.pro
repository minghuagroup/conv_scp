function mean_intv,dd,nt,nintv
; to average based on n intvals

 nt2 = nt/nintv
 dd2 = fltarr(nt2)
 for ii = 0,nt2-1 do begin
  i1 = ii*nintv
  i2 = i1+nintv-1
  dj = dd[i1:i2]
  dd2[ii] = mean(dj)
 endfor
 return,dd2
end 
