
function m2z,d,z3,z2=z2
;==============
; interolate CAM data to height levels

if(not keyword_set(z2))then p2=lev
 nx  =n_elements(d[*,0,0])
 ny  =n_elements(d[0,*,0])

d2 = d*0 - 9999.

filein = 'data/USGS-gtopo30_1.9x2.5_remap_c050602.nc'
 PHIS  = get_fld(filein,'PHIS')/9.8
 
 if(not keyword_Set(z2))then begin
  z2 = ave3(transpose(z3))
 endif

 nz  = n_elements(z2)
 d2 = fltarr(nx,ny,nz)-9999.

for i=0,nx-1 do begin
for j=0,ny-1 do begin
 dw = reform(d[i,j,*])
 pw = reform(z3[i,j,*])
 dd = interpol(dw,pw,z2)
 d2[i,j,*] = dd
 jj=where(z2 lt phis[i,j] or (z2 gt max(pw)),cnt) 
 if(cnt gt 0)then d2[i,j,jj]= -9999.
endfor
endfor
 
return,d2

;filein = 'data/USGS-gtopo30_1.9x2.5_remap_c050602.nc'
; lon2  = get_fld(filein,'lon')
; lat2  = get_fld(filein,'lat')
; PHIS  = get_fld(filein,'PHIS')
; SGH   = get_fld(filein,'SGH')
; SGH30 = get_fld(filein,'SGH30')
; LANDFRAC = get_fld(filein,'LANDFRAC')
; LANDM_COSLAT= get_fld(filein,'LANDM_COSLAT')


end
