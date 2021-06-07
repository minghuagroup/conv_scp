function dfdx2d,d,x,ind=ind,x2d=x2d
; d[nx,ny] to do differencing with d/dx ; if ind=1 then d/dy

 if(not keyword_set(ind))then ind=0
 n1=n_elements(d[*,0]) 
 n2=n_elements(d[0,*]) 

 dw = d
 dw2 = dw*0

 if(ind)then begin
  for j=0,n1-1 do begin
    dj       = reform(dw[j,*])
    if(keyword_set(x2d))then xx = reform(x[j,*]) else xx=x
    dw2[j,*] = dfdx1d(dj,xx)
  endfor 
 endif else begin
  for j=0,n2-1 do begin
    dj       = reform(dw[*,j])
    if(keyword_set(x2d))then xx = reform(x[*,j]) else xx=x
    dw2[*,j] = dfdx1d(dj,xx)
  endfor 
 endelse

  return,dw2
 end
