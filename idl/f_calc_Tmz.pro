
function f_calc_Tmz,T0,z,pp=pp,ps=ps,z=z0
;===============================
; all input from small to big
;moist profile and p from z coordinate

g =  9.8
Rd = 287.
if(not keyword_Set(PS))then ps = 1000.
if(not keyword_Set(PP))then pp=z*0


NZ = n_elements(z)

T = Z*0
pp = Z*0

T[0] = T0
Pp[0] = PS
for i=1,nz-1 do begin
 dz = z[i]-z[i-1]
 T[i] = T[i-1]-f_lapserate(T[i-1],pp[i-1])*dz
 Tm = 0.5*(T[i-1]+T[i])
 Pp[i] = Pp[i-1]*exp(-g*dz/(Rd*Tm) )
endfor

return,T
end
