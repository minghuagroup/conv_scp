
function m2p_ec,d,p2=p2,ps=ps,pres=pres
;==============
; interolate ERAIdata to pressure levels, ps same horizontal grids as d

filein= '../obs/ERAI/ei.moda.an.ml.regn128sc.2008010100.nc'
;get sigma coordinate
hyam = get_fld(filein,'lv_HYBL2_a')
hybm = get_fld(filein,'lv_HYBL2_b')
P0 = 1000.

 nx  =n_elements(d[*,0,0])
 ny  =n_elements(d[0,*,0])

 nz=n_elements(p2)  ; must have !!

 d2 = fltarr(nx,ny,nz)


if(not keyword_set(ps))then begin
 PS = get_fld(filein,'LNSP_GDS4_HYBL_S123')
 PS = exp(PS)/100.
endif

if(not keyword_Set(pres))then begin
nlev = n_elements(hyam)
P = fltarr(nx,ny,nlev)
for k=0,nlev-1 do begin
  P[*,*,k] = hyam[k]*p0+hybm[k]*ps[*,*]
endfor
pres = p
endif

p=pres

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

;if(max(d2) gt 320)then stop
endfor
endfor

;stop
 
return,d2

end
