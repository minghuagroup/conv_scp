function dfdp, h, yy
 ;h[nx,ny,nz]
  
  d   = h*0.
  y = yy
  sz  = size(h)
  sz2 = size(y)

  dim = sz[0]
 
  case dim of
1: begin

  n = sz[1]
  for i = 1, N-2 do begin
   d[i] = (h[i+1] - h[i-1])/(y[i+1] - y[i-1])
  endfor
   i = 0
   d[i] = (h[i+1] - h[i])/(y[i+1] - y[i])
   i = N-1 
   d[i] = (h[i] - h[i-1])/(y[i] - y[i-1])

   end
2: begin  ; zonally averaged
   nx = sz[1]  ;  is ny
   if(sz2[0] eq 2)then begin
     for i = 0,nx-1 do begin
       d2 = reform(h[i,*])
       y  = reform(y[i,*])
       d[i,*] = dfdp(d2,y)
    endfor
   endif else begin
      for i = 0,nx-1 do begin
        d2 = reform(h[i,*])
        d[i,*] = dfdp(d2,y)
      endfor
   endelse
   end
3: begin
    nv = sz[1]  ; lon
   if(sz2[0] eq 3)then begin
    for i = 0,nv-1 do begin
     y  = reform(yy[i,*,*])
     d3 = reform(h[i,*,*])
     d[i,*,*] = dfdp(d3,y)
    endfor
   endif else begin
    for i = 0,nv-1 do begin
     d3 = reform(h[i,*,*])
     d[i,*,*] = dfdp(d3,y)
    endfor
   endelse
   end

endcase

   return,d
 
end
