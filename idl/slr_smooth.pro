

 function slr_smooth, dd, xx = xx, yy=yy, oro = oro, rs=rs, dd2d = dd2d,$
            sdd2d = sdd2d
; ================================================================================
; returns smoothed sdd[nx,ny,nr]
; dd and oro should be in same dimension

; dd2d is after removal of zonal average

 restore,'cam6.sav0'
 if(not keyword_set(xx)) then xx = lon2  ; cam
 if(not keyword_set(yy)) then yy = lat2
 if(not keyword_set(oro)) then oro = phis2/9.8
 if(not keyword_set(rs)) then rs  = [1000., 2000., 3000]

 r0 = 6400. ;
 rr = rs/r0

 nx = n_elements(xx)
 ny = n_elements(yy)

 yy0 = yy*3.14/180
 xx0 = xx*3.14/180

 xx00 = replicate2(xx0,ny)
 yy00 = transpose( replicate2(yy0,nx))

 lat2 = expand_2d(yy00)
 lon2 = expand_2d(xx00)

 nr   = n_elements(rs)

; =====================================

 dd2d = dd ; remove zonal averaging
 ddm  = ave2(transpose(dd))
 ddm2 = transpose(replicate2(ddm,nx))
 dd2d   = dd - ddm2 

; dd3 and dd3d are the original ps data and the deviation data

 sdd  = fltarr(nx,ny,nr)  ; smoothed
 sdd2d = sdd

 weight2 = lon2 * 0.0

; =====================================
 for ir = 0, nr-1 do begin

   print,ir, rs[ir]
  for j2 = 0,ny-1 do begin 

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
     sdd[i2,j2,ir]  = total(reform(dd[*,*])*weight)/tweight
     sdd2d[i2,j2,ir] = total(reform(dd2d[*,*])*weight)/tweight
    weight = shift(weight,-1,0)
   endfor; i2

  endfor ; j2
  endfor ; ir

  return, sdd
  
end
