function dfdp2, h, yy
 ;h[nx,ny,nz]
  
  d   = h*0.
  y = yy
  sz  = size(h)
  sz2 = size(y)

  dim = sz[0]
 
   nx = sz[1]  ;  is ny
   if(sz2[0] eq 2)then begin
     for i = 0,nx-1 do begin
       d2 = reform(h[i,*])
       y  = reform(yy[i,*])
       d[i,*] = dfdp1(d2,y)
    endfor
   endif else begin
      for i = 0,nx-1 do begin
        d2 = reform(h[i,*])
        d[i,*] = dfdp1(d2,y)
      endfor
   endelse

   return,d

   end
