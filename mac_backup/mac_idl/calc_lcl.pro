function calc_lcl,T,q,rh,z0
 ;standard units
 Tstar = 1./( 1./(T-55.) - alog(rh)/2840. ) +55.
 cpd = 1004.
 cpv = 561.
 Rd = 287.
 g=9.8
 z = cpd/g*(1+q*cpv/cpd)/(1+q)*(T-Tstar) + z0
 return,z
end

