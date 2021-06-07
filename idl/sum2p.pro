
function sum2p,d, p, ps
 ; d and p are 3d arrays, ps is 2d array

  nx  = n_elements(d[*,0,0])
  ny  = n_elements(d[0,*,0])
  nz  = n_elements(d[0,0,*])

  d2 = fltarr(nx,ny)-9999.

  for i=0,nx-1 do begin
  for j=0,ny-1 do begin
    dw = reform(d[i,j,*])
    pw = reform(p[i,j,*])
    pw = abs ((shift(dw,1)-shift(dw,-1))/2.0 )

    dw[0]   = 0.0
    dw[nz-1]=0.0
    jj=where(dw gt -999.,cnt)
    if(cnt gt 0)then d2[i,j] = total(dw[jj]*pw[jj])
 endfor
 endfor

 ;boundary

  dp0 = abs( reform(p[*,*,1]-p[*,*,0]) /2. ) 
  dp2 = abs( reform(p[*,*,nz-1]-p[*,*,nz-2]) /2. )

 kz = nz-1
 if(p[0,0,0] gt p[0,0,nz-1])then kz=0
  dps  = abs( ps-reform(p[*,*,kz]))


  dw1  = reform(d[*,*,0])
    jj=where(dw1 gt -999.,cnt1)
    if(cnt1 gt 0)then d2[jj]  = d2[jj]+abs(dp0[jj])*dw1[jj]

  dw1  = reform(d[*,*,nz-1])
    jj=where(dw1 gt -999.,cnt1)
    if(cnt1 gt 0)then d2[jj]  = d2[jj]+abs(dp2[jj])*dw1[jj]

  dw1  = reform(d[*,*,kz])
    jj=where(dw1 gt -999.,cnt1)
    if(cnt1 gt 0)then d2[jj]  = d2[jj]+abs(dps[jj])*dw1[jj]
    
return,d2
end
    
