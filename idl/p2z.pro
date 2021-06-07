
function p2z,d,z3,z2
;==============
; interolate to Z2 levels
; given d at p3 and z3 levels

 nx  =n_elements(d[*,0,0])
 ny  =n_elements(d[0,*,0])
 nz  = n_elements(z2)

d2 = fltarr(nx,ny,nz)-9999.
for i=0,nx-1 do begin
for j=0,ny-1 do begin
 dw = reform(d[i,j,*])
 zw = reform(z3[i,j,*])
 dd = interpol(dw,zw,z2)
 d2[i,j,*] = dd

 jj=where((z2 gt max(zw)) or (z2 lt min(zw)), cnt)
 if(cnt gt 0)then d2[i,j,jj] = -9999. 
endfor
endfor

 
return,d2

end
