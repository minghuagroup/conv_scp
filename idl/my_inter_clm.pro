function my_inter_clm,d,x,y,x2,y2,ZZ=ZZ,dx=dx
 ; regular grid to irregular grid

 nc = n_elements(x2)
 nx = n_elements(x)
 ny = n_elements(y)
 xx= d*0
 yy= d*0
 for i=0,nx-1 do begin
   xx[i,*] = x[i]
   yy[i,*] = y
 endfor
 
 dist0=0.6*(abs(x[1]-x[0])+abs(y[1]-y[0]))
  if(keyword_Set(dx))then dist0 = dx

 dd = fltarr(nc) - 9999.0
 for k=0,nc-1 do begin
  dist = abs(xx-x2[k])+abs(yy-y2[k])

  jj=where(dist le dist0,cnt)
  if(cnt gt 0) then begin
   dj = d[jj]
    jj2=where((dj gt -999.) or (dj le 1.0e10),cnt2)
    if(cnt2 gt 0)then dd[k] = mean(dj)
   endif
  endfor

  jj3 = where(dd le -999.,cnt3)
  if(cnt3 gt 0)then dd[jj3] = ZZ[jj3]

  return,dd
end
  

   

 


