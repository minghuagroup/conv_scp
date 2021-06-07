
function my_interpol3, d3,x2,y2,x,y,dxh=dhx,dyh=dyh, tdim = tdim, missing=missing
; horizontal interpolation from x2,y2 to x, y

 if(not keyword_set(dxh))then dxh = (x[1]-x[0])/2
 if(not keyword_set(dyh))then dyh = (y[1]-y[0])/2

 if(not keyword_set(tdim))then tdim=0
 if(not keyword_set(missing))then missing=-9990.


 nx  =n_elements(x)
 ny  =n_elements(y)

 missing=-9990.

if(not tdim)then begin;   regular 3d
 np = n_elements(d3[0,0,*])
 dd3 = fltarr(nx,ny,np)+missing
 for k=0,np-1 do begin
   dj = my_interpol2(reform(d3[*,*,k]),x2,y2,x,y, missing=missing)
   dd3[*,*,k] = dj
 endfor

 return,dd3
endif

np = n_elements(d3[0,0,*,0])
nt = n_elements(d3[0,0,0,*])
dd3 = fltarr(nx,ny,np,nt)-9999.
for k=0,nt-1 do begin
  dj = reform(d3[*,*,*,k])
  dd3[*,*,*,k] = my_interpol3(dj,x2,y2,x,y,dxh=dhx,dyh=dyh,missing=missing)
endfor
return,dd3

end
