
function ave4,d,missing=missing
 nx=n_elements(d[*,0,0,0])
 ny=n_elements(d[0,*,0,0])
 nz=n_elements(d[0,0,*,0])

 dm = fltarr(nx,ny,nz)

 for i=0,nx-1 do begin
   dj = reform(d[i,*,*,*])
   dm[i,*,*] = ave3(dj,missing=missing)
 endfor 
 
return,dm

end

