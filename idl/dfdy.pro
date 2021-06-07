function dfdy, h, y
 ;h[nx,ny,nz]
  
  d   = h*0.

  sz  = size(h)
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
2: begin 
   nx = sz[1]
    for i = 0,nx-1 do begin
    d2 = reform(h[i,*])
    d[i,*] = dfdy(d2,y)
    endfor
   end
3: begin
    nv = sz[3]
    for i = 0,nv-1 do begin
     d3 = reform(h[*,*,i])
     d[*,*,i] = dfdy(d3,y)
    endfor
   end

endcase

   return,d
 
end
