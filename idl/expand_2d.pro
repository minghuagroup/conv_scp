function expand_2d, dd, xdn = xdn, ydn = ydn

 nx = n_elements(dd[*,0])
 ny = n_elements(dd[0,*])

if(not keyword_Set(xdn))then xdn = nx
if(not keyword_Set(ydn))then ydn = ny


 xdn = nx
 ydn = ny

 nx2 = nx + xdn
 ny2 = ny + ydn 

 dd2 = fltarr(nx2, ny2)

 dd2[0:nx-1,0:ny-1]   = dd
 dd2[nx:*,0:ny-1]     = dd[0:xdn-1,*]
 jj = reverse(indgen(ydn))
 dd2[0:nx-1,ny:*]     = dd[*,jj]
 dd2[nx:*,ny:*]       = dd[0:xdn-1,jj]

 dd3 = shift(dd2, [nx/2, 0])
 dd4 = shift(dd3, [0, ny/2])

 return,dd4

end



