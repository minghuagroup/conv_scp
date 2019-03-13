
 function interpol2,d2,p1,p2

 nst = n_elements(d2[0,*])
 np = n_elements(p2)
 dd=fltarr(np,nst)

 for i=0,nst-1 do begin
  dj = reform(d2[*,i])
  dd[*,i]  = interpol(dj,p1,p2)
 endfor
 return,dd
 end

