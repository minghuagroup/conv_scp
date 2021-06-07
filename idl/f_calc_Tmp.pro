
function f_calc_Tmp,T0,p,z=z,ps=ps,z=z0
;===============================
; moist profile and height from p coordinate

p2 = p
if(p[1] gt p[0])then p2=reverse(p)

g =  9.8
Rd = 287.
if(not keyword_Set(PS))then ps = 1000.
if(not keyword_Set(z0))then z0 = 0.

NZ = n_elements(p)

T = p*0
z = p*0

T[0] = T0
Z[0] = Z0

for i=1,nz-1 do begin
 Z[i] = Z[i-1]+Rd/g*T[i-1]*alog(p2[i]/p2[i-1])
 dz = z[i]-z[i-1]
 T[i] = T[i-1]-f_lapserate(T[i-1],p2[i-1])*dz
endfor

if(p[1] gt p[0])then begin
  T=reverse(T)
  Z=reverse(Z)
endif
 
return,T

end

