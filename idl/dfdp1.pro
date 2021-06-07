function dfdp1, h, yy
 ;h[nx,ny,nz]
  
  d   = h*0.
  y = yy
  sz  = size(h)
  sz2 = size(y)

  dim = sz[0]
 
  n = sz[1]
  for i = 1, N-2 do begin
   d[i] = (h[i+1] - h[i-1])/(y[i+1] - y[i-1])
  endfor
   i = 0
   d[i] = (h[i+1] - h[i])/(y[i+1] - y[i])
   i = N-1 
   d[i] = (h[i] - h[i-1])/(y[i] - y[i-1])
 return,d

end
