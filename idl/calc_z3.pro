
 function calc_Z3, p,zs,ps,t,q 
 ; p 
 ; q kg/kg

 zvir = 0.608
 R = 286.
 g = 9.8

 z = p*0
 dz=z

  tv = T*(1+zvir*Q)
  pp = p 

  n=n_elements(p) 
  if(p[0] gt p[n-1])then begin
    pp = reverse(p)
    tv = reverse(tv)
  endif

  k=n-1
  dz[k] = R*Tv[k]/g*alog(ps/pp[k])
  for k=n-2,0,-1 do begin
   dz[k] = 0.5*R*(Tv[k]+Tv[k+1])/g*alog(pp[k+1]/pp[k])
  endfor 
  
  z[n-1] = zs + dz[n-1]
  for k=n-2,0,-1 do begin
   z[k] = z[k+1]+dz[k]
  endfor

  if(p[0] gt p[n-1])then z=reverse(z)

 return,z

end
 
