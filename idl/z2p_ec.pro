
function z2p_ec,d,z,lev,ps,zs,p,t,q
;==============

 nx  =n_elements(d[*,0,0])
 ny  =n_elements(d[0,*,0])
 nz  = n_elements(lev)

d2 = fltarr(nx,ny,nz)-9999.
for i=0,nx-1 do begin
for j=0,ny-1 do begin
 t3 = reform(t[i,j,*])
 q3 = reform(q[i,j,*])
 p3 = reform(p[i,j,*])

 z3 = calc_Z3(p3,zs[i,j],PS[i,j],T3,Q3)
 p4 = my_interpol(p3,z3,z)   ;---- to get the p from z

 dw = reform(d[i,j,*])

 dj=my_interpol(dw,p4,lev) 
 
 d2[i,j,*] = dj
endfor
endfor
 
return,d2

end
