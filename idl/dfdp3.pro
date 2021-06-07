function dfdp3, h, yy
 ;h[nx,ny,nz]
  
  d   = h*0.
  y = yy
  sz  = size(h)
  sz2 = size(y)

    nv = sz[1]  ; lon
   if(sz2[0] eq 3)then begin
    for i = 0,nv-1 do begin
     y  = reform(yy[i,*,*])
     d3 = reform(h[i,*,*])
     d[i,*,*] = dfdp2(d3,y)
    endfor
   endif else begin
    for i = 0,nv-1 do begin
     d3 = reform(h[i,*,*])
     d[i,*,*] = dfdp2(d3,y)
    endfor
   endelse

   return,d
 
end
