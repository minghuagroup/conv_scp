
function my_interpol3d, d3,x2,y2,z2,x,y,z,dxh=dhx,dyh=dyh,tdim = tdim,  missing=missing, $
   noextra=noextra, ps=ps, d3s = d3s
;to regular grids

 if(not keyword_set(dxh))then dxh = (x[1]-x[0])/2
 if(not keyword_set(dyh))then dyh = (y[1]-y[0])/2

 if(not keyword_set(missing))then missing=-9990.
 if(not keyword_set(noextra))then noextra = 0

 nx  =n_elements(x)
 ny  =n_elements(y)
 nz  =n_elements(z)

 missing=-9990.

if(not keyword_set(tdim))then begin
;===============================

 np = n_elements(d3[0,0,*])
 dd3 = fltarr(nx,ny,np)+missing

 hh = (array_equal(x2, x) and array_equal(y2, y))
if(not hh)then begin
 for k=0,np-1 do begin
   dj = my_interpol2(reform(d3[*,*,k]),x2,y2,x,y, missing=missing, noextra= noextra)
   dd3[*,*,k] = dj
 endfor
endif else begin
   dd3 = d3
endelse

 z22 = z2
 sz2 = size(z2)
 if(sz2[0] eq 1)then begin
   z22 = replicate3(dd3,z2,2)
 endif

if(not keyword_Set(ps))then begin
 if(z22[0,0,0] gt z22[0,0,2])then ns = 0 else ns = np-1
   dd2s = reform(dd3[*,*,ns])
   ps2j = reform(z22[*,*,ns]) 
endif else begin
   dd2s = d3s
   ps2j = ps
endelse

 dd4 = my_interpol3v(dd3, z2, dd2s, ps2j, z)

 return,dd4

endif else begin  ; with t dimesion
; =================================

 np = n_elements(d3[0,0,*,0])
 nt = n_elements(d3[0,0,0,*])

 dd5 = fltarr(nx,ny,nz,nt)
 for k = 0,nt-1 do begin
  dw = reform(d3[*,*,*,k])
  dw2 =  my_interpol3d(dw,x2,y2,z2,x,y,z,dxh=dhx,dyh=dyh,tdim = 0,  missing=missing)
  dd5[*,*,*,k] = dw2
 endfor
 return, dd5


end

 

end
