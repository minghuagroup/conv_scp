
function sum2cld, cldliq, z2
  nx  = n_elements(cldliq[*,0,0])
  ny  = n_elements(cldliq[0,*,0])
  nz  = n_elements(cldliq[0,0,*])

  d2 = fltarr(nx,ny)

    dz = z2-shift(z2,-1)
    dz[133] = 60.
    dz[134:*] = 0.0

  for i=0,nx-1 do begin
  for j=0,ny-1 do begin
    dw = reform(cldliq[i,j,*])
    dw2 = (dw+shift(dw,-1))/2.0
    dw2[0] = 0.
    d2[i,j] = total(dw2*dz)
 endfor
 endfor
    
return,d2
end
    
