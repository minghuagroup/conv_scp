
function z2p,d,z,z3,lev3=lev3,ps=ps
;==============
; interolate to model Z3 levels

 nx  =n_elements(d[*,0,0])
 ny  =n_elements(d[0,*,0])

 nlev  = n_elements(z3[0,0,*])

filein='../obs/CAM/myctl_test.cam.h0.2008-01.nc'
hyam = get_fld(filein,'hyam')
hybm = get_fld(filein,'hybm')
P0 = 1000.
if(not keyword_set(lev3))then lev3 = get_fld(filein,'lev')
if(not keyword_set(ps))then PS = get_fld(filein,'PS')/100.

phis = get_fld(filein,'PHIS')/10.;;

P = fltarr(nx,ny,nlev)
for k=0,nlev-1 do begin
  P[*,*,k] = hyam[k]*p0+hybm[k]*ps[*,*]
endfor

nz  = n_elements(lev3)
d2 = fltarr(nx,ny,nz)-9999.

for i=0,nx-1 do begin
for j=0,ny-1 do begin

 dw = reform(d[i,j,*])
 zw = z

 z3j = [reform(Z3[i,j,*]),phis[i,j]]
 p3j = [reform(p[i,j,*]),ps[i,j]]

 pw=interpol(p3j,z3j,z) 
 
 p2w = reform(z3[i,j,*])
 dd = interpol(dw,pw,lev3)
 d2[i,j,*] = dd
 jj=where(lev3 gt max(pw) or (lev3 lt min(pw)),cnt) 
 if(cnt gt 0)then d2[i,j,jj]= -9999.
endfor
endfor
 
return,d2

end
