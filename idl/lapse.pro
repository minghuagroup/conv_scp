
nz= 1001

z  = findgen(nz)*5.0 
T0 = 300.
P0 =1000.
T1 = T0+1
RH0 = 0.85
RH1 = 0.8 ;1.0 ;0.9

Tp  = f_calc_Tmz(T0,z, pp=pp)
dTp = f_calc_Tmz(T1,z,pp=pp2) - Tp

qp  = RH1*f_qs(Tp,pp)  ; approximate

h0  = Cp*T0+RH0*Lv*f_qs(T0,p0)
dh0 = Cp*T1+RH0*Lv*f_qs(T1,p0) - h0


dqp1 = (dh0/dtp - cp)/Lv  ; from DSST=1
dqps  = f_qs(T1,pp)-f_qs(T0,pp)

B0 = dqp1
B1 = -Lv*qp/Rv/Tp^2
B2 = qp*(pp2-pp)/pp/dtp

BB = dqp1 + B1 + B2

plot,dqp1*1000.,z,xrange=[-1.5,1.5]
oplot,B1*1000.,z,color=colors.red
oplot,B2*1000.,z,color=colors.pink
oplot,BB*1000,z,color=colors.blue,thick=2
oplot,BB*0,z

;check
C1 = f_qs(T0,p0)/T0^2
C2 = f_qs(Tp,pp)/Tp^2
C3 = C1-C2


end
