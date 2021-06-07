
function my_interpol_isccp, d2,x2,y2,humtab,x,y,dxh=dhx,dyh=dyh
;x2  eqlon, y2=eqlat
; x,y all 
 if(not keyword_set(dxh))then dxh = (x[1]-x[0])/2
 if(not keyword_set(dyh))then dyh = (y[1]-y[0])/2

 nn  =n_elements(x2)
 nx  =n_elements(x)
 ny  =n_elements(y)

 dd = fltarr(nx,ny)-9999.

missing=-9990.
ix=1

 for j=0,ny-1 do begin
  y0 = y[j]
  jj = where(y2 eq y0,cnt)
  dw = humtab ( d2[jj] )
  xw = x2[jj]
  kk= where(dw gt 0,cnt)
   if(cnt gt 0)then begin
      dw2= dw[kk]
      xw2= xw[kk]
     dd[*,j] = interpol(dw2,xw2,x)
    endif
  endfor
 

return,dd
end




 
 

