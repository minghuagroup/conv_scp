
function ave4,d
 nx=n_elements(d[*,0,0,0])
 ny=n_elements(d[0,*,0,0])
 nz=n_elements(d[0,0,*,0])

 dm = fltarr(nx,ny,nz)

 for i=0,nx-1 do begin
 for j=0,ny-1 do begin
 for k=0,nz-1 do begin

  dm[i,j,k] = mean(d[i,j,k,*])
 endfor
 endfor
 endfor

return,dm

end

