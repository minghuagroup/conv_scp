function daydev2d_intv,dd,nx,ny,nt,nintv
; to average based on n intvals

 nt2 = nt/nintv
 dd2 = fltarr(nx,ny,nt2)

 ddd = dd
 
for i=0,nx-1 do begin
for j=0,ny-1 do begin

 for ii = 0,nt2-1 do begin

  i1 = ii*nintv
  i2 = i1+nintv-1
  dj = dd[i,j,i1:i2]
  dd2[i,j,ii] = mean(dj[*])
 endfor

 for ii = 0,nt-1 do begin
  ii2 = ii/nintv
  ddd[i,j,ii] = dd[i,j,ii] = dd[i,j,ii2]
 endfor

endfor
endfor
 return,ddd
end 
