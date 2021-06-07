function trmm_coarse, d, x,y,x2,y2

 nx = n_elements(x2)
 ny = n_elements(y2)
 d2 = fltarr(nx,ny) - 9999.

 dx2 = (x2[1]-x2[0])/2.

 j1 = where(y2 lt -60.) & j1=max(j1)
 j2 = where(y2 gt 60.)  & j2 = min(j2)

 for i=0,nx-1 do begin
  ii = where(abs(x-x2[i]) le dx2,cnt1)
  dj1 = d[ii,*]
 for j=j1,j2 do begin
      dy2 = (y2[j+1]-y2[j-1])/4.
     jj=where(abs(y-y2[j]) le dy2,cnt2) 
      dj = dj1[*,jj]
     kk=where(dj gt -999.,cnt)
     if(cnt gt 0)then d2[i,j] = mean(dj[kk])
  endfor
 endfor

return,d2
end
