
goto,jump1
; map figure

; Sea-2evel Rise after wslr to do smoothing of dps

land = get_fld('../cmip6/CMIP/NCAR_CESM2/amip/nc/sftlf.nc','sftlf')

 dd = d2 - d0
 ddd = dd

 ddm = ave2(transpose(dd))
 ddm2 = transpose(replicate2(ddm,nx))
 dd = dd - ddm2 ;* 0

 ;sph_smooth(dd,xx,yy,R)
 r0 = 6400. ;
 rs  = [500., 1000., 2000., 3000] ;
 rr = rs/r0

 nx = n_elements(dd[*,0])
 ny = n_elements(dd[0,*])

 yy0 = yy*3.14/180
 xx0 = xx*3.14/180

 xx00 = replicate2(xx0,ny)
 yy00 = transpose( replicate2(yy0,nx))

lat2 = expand_2d(yy00)
lon2 = expand_2d(xx00)

nr   = n_elements(rs)
dps0 =  dd * 0.0 
dpsrr = fltarr(nx,ny,nr)

weight2 = expand_2d(dd)*0.0
dj = dd

for ir = 0, nr-1 do begin
;for ir = 0, 0 do begin
 print,ir
for j2 = 0,ny-1 do begin 
;   print,j2,yy[j2]
   j    = j2 + ny/2
   lat1 = lat2[0,j]

   s1 = sin(lat1)*sin(lat2)
   c1 = cos(lat1)*cos(lat2)
   
   i2 = 0
   i  = i2 + nx/2
   lon1 = lon2[i,j]
   cdist= s1 + c1*cos(lon2 - lon1)
   dist = acos(cdist) 

   weight2 = weight2*0.0
   jj = where(dist le rr[ir],cnt)
   weight2[jj] = 1.0

   weight = compact_2d(weight2)
   tweight = total(weight)

  for i2 = 0,nx-1 do begin 
    dpsrr[i2,j2,ir] = total(dd*weight)/tweight
    weight = shift(weight,-1,0)
  endfor

 endfor
endfor

 ; ==================================

 
jump1:
 
lev1 = cal_lev([-1.,1],10)
lev1 = cal_lev([-5.,5],10)

;lev1 = get_lev_cam(dps0,var,scale,diff=1)

lev2 = lev1*1000

for ir = 0, nr-1 do begin 
 dps0  = reform(dpsrr[*,*,ir])
 dj2 = (dd - dps0)*land/100*10. ; scale purpose only?

;dj2 = dps0*land/100.

 view3d, 'test R=' + strtrim(rs[ir],2), dj2, dd2=dj2, xx,yy,zz, iave,lev1,lev1*10 
 ix = 1
 read,ix
endfor

jk = where(dj2 lt 0)
bias = mean(dj2[jk])

dj2 = (dd - dps0 - bias * 0)*land/100.
;          low-pass filter


stop



;for j = 0, ny2-1 do begin
for j = j1,j1  do begin
 lat1 = yy0[j]
 lat2 = yy0
 sinlat = sin(lat1)*sin(lat2)
 coslat = cos(lat1)*cos(lat2)
; for i = 0, nx-1 do begin
 for i = i1, i1 do begin
   cosd = sinlat + coslat * cos(xx0 - xx0[i])
 ;  d    = acos(cosd)*r0
   jj = where(d le r,cnt)
   if(cnt gt 0)then dj[jj] = 1.0
 endfor
endfor
end
