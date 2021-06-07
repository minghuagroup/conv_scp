
 function calc_p, z, ps, zs, p,t, q ,reverset=reverset,reversez=reversez
 ; p 
 ; q kg/kg  ;t q orderred from top to surface
; calculated the p corresponding to t and q profile at the z height

 z3 = calc_z3(p,zs,ps,t,q)
 p3 = my_interpol(p,z3,z)

 return,p3

end
 
