
k=5
T=get_fld(filein2,'T')
Q=get_fld(filein2,'Q')
RH=get_fld(filein2,'RELHUM')
P=get_fld(filein2,'PMID')/100.  

T=reform(T[*,k])
Q=reform(Q[*,k])
P=reform(P[*,k])
RH=reform(RH[*,k])

es = f_eswi(T-273.16)
qs = 0.622*es/(P-es)
RH2 = q/qs*100.

theta = T*(1000./p)^0.286
p2    = indgen(200)*5+50. 
th2   = interpol(theta,p,p2)
T2    = th2*(p2/1000.)^0.286
q2    = interpol(q,p,p2)
e2    = f_eswi(T2-273.16)
qs2   = 0.622*e2/(p2-e2)
R2    = q2/qs2*100.




end
