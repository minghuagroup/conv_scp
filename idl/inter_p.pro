function inter_p,d,p,lev,ind=ind,x2d=x2d
; d[nx,ny] to do interpolation with d[np,nx ] on p[np,nx] 
; linearly interpolated to lev[np,nx] or lev[np] levels 
; if ind=1 then transpose, if x2d=1 then lev is a 2-D vector as well

 if(not keyword_set(ind))then ind=0
 n1=n_elements(d[*,0]) 
 n2=n_elements(d[0,*]) 

 x=lev
 dw = d
 dw2 = dw*0

 if(ind)then begin
  for j=0,n1-1 do begin
    dj       = reform(dw[j,*])
    pj       = reform(p[j,*])
    if(keyword_set(x2d))then xx = reform(x[j,*]) else xx=x
    dw2[j,*] = interpol(dj,pj,xx)  ;it could be cubic, spline etc
  endfor 
 endif else begin
  for j=0,n2-1 do begin
    dj       = reform(dw[*,j])
    pj       = reform( p[*,j])
    if(keyword_set(x2d))then xx = reform(x[*,j]) else xx=x
    dw2[*,j ] = interpol(dj,pj,xx)
  endfor 
 endelse

  return,dw2
 end
