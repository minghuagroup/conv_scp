
function m2p,d,p2=p2,ps=ps,pres=pres
;==============
; interolate CAM data to pressure levels, ps same horizontal grids as d

filein='../obs/CAM/myctl_test.cam.h0.2008-01.nc'

 nx  =n_elements(d[*,0,0])
 ny  =n_elements(d[0,*,0])

 lev = get_fld(filein,'lev')
 if(not keyword_set(p2))then p2=lev

 nz=n_elements(p2)
 nlev  =n_elements(lev)

hyam = get_fld(filein,'hyam')
hybm = get_fld(filein,'hybm')
P0 = 1000.
;phis = get_fld(filein,'PHIS')/100.

if(not keyword_set(ps))then PS = get_fld(filein,'PS')/100.

P = fltarr(nx,ny,nlev)
for k=0,nlev-1 do begin
  P[*,*,k] = hyam[k]*p0+hybm[k]*ps[*,*]
endfor
pres = P

d2 = fltarr(nx,ny,nz) - 9999.
for i=0,nx-1 do begin
for j=0,ny-1 do begin
 dw = reform(d[i,j,*])
 pw = reform(p[i,j,*])
 jj2 = where(dw gt -999.,cnt)
 if(cnt gt 0)then begin
 dw = dw[jj2]
 pw = pw[jj2]
 dd = my_interpol(dw,pw,p2)
 d2[i,j,*] = dd
 jj=where(p2 gt ps[i,j] or (p2 lt min(pw)),cnt) 
 if(cnt gt 0)then d2[i,j,jj]= -9999.

 endif

endfor
endfor
 
return,d2


end
