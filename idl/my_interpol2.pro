
function my_interpol2, d2,x2,y2,x,y,dxh=dhx,dyh=dyh, tdim = tdim, missing=missing, wrap = wrap,noextra=noextra

 if(not keyword_set(dxh))then dxh = abs(x[1]-x[0])/2.0
 if(not keyword_set(dyh))then dyh = abs(y[1]-y[0])/2.0
 if(not keyword_set(tdim))then tdim=0
 if(not keyword_set(missing))then missing=-9990.
 if(not keyword_set(wrap))then wrap = 1
 if(not keyword_set(noextra))then wrap = 0


 nx2  =n_elements(x2)
 ny2  =n_elements(y2)

 nx  =n_elements(x)
 ny  =n_elements(y)

ix=1

hh = (array_equal(x2, x) and array_equal(y2, y))  ; same resolution
if(hh)then return, d2


if(not tdim)then begin;   regular 2d
;----------------------
;========= if x2[nx,ny] situation, same dimension as d2
 szx = size(x2)
 szd = size(d2)
 if(array_equal(szx[0:2], szd[0:2])) then begin
   d4 = interp2d(d2,x2,y2,x,y,/grid, extrapolate=0)
   return, d4
 endif

 d3 = fltarr(nx,ny2)-9999.
 for j=0,ny2-1 do begin
  dw = reform(d2[*,j])
  dj = my_interpol(dw,x2,x,missing=missing,wrap=wrap,dxh=dxh)

;  read,ix
  d3[*,j] = dj
 endfor

 d4=fltarr(nx,ny)-9999.
 for j=0,nx-1 do begin
  dw = reform(d3[j,*])
  dj = my_interpol(dw,y2,y,missing=missing,dxh=dyh, noextra=1)
  d4[j,*] = dj
 endfor

;stop
 return,d4

endif ; end 2d
;----------------------

nt = n_elements(d2[0,0,*])
d4 = fltarr(nx,ny,nt)-9999.
for k = 0,nt-1 do begin
 dj = reform(d2[*,*,k])
 d4[*,*,k] = my_interpol2(dj,x2,y2,x,y,dxh=dhx,dyh=dyh, missing=missing, wrap=wrap, noextra=noextra)
endfor

return,d4


end

 
 

