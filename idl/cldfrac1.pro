
;USE ERA data at C3M grids  run era_c3m first, 

alpha=indgen(201)*0.01+0.01

pi =3.1416
pi4 = 4.0*pi*alpha
pi2 = 2.0*pi*alpha


sig = 0.5+sin(pi4)/pi4/2.-(sin(pi2)/pi2)^2

;plot,alpha,sig
;oplot,alpha,0.5*(alpha/0.5)^2.5,color=colors.blue

var='cld'

gifname=0
R0=0.6

CFRAC = RH2-9999.
jj=where(RH2 ge R0)

CFRAC[jj] = ((RH2[jj]-R0) /(1.0-R0))^2 ;^0.5
;CFRAC[jj] = ((RH2[jj]-R0) /(1.0-R0))^0.5
aa = cfrac

CFRAC2 = CFRAC*0-9999.
U0 = 0.4
jj = where ((RH2 gt 0) and (RH2 le 1 ) ,cnt)
if(cnt gt 0)then CFRAC2[jj] = 1.-sqrt( (1-RH2[jj])/(1-u0) )

jj = where((RH2 gt 0) and ( RH2 le U0),cnt1)
if(cnt1 gt 0)then CFRAC2[jj]=0

jj2 = where(RH2 ge 1.,cnt3) 
if(cnt3 gt 0)then CFRAC2[jj2] = 1.

aa = cfrac2

CFRAC3 = CFRAC*0-9999.

cosz = T2*0
for j=0,ny-1 do cosz[*,j,*] = cos(3.1416/180.*y[j])
dxj = 6400*1000.*cosz*(x[1]-x[0])*2*3.1416/180.
dxr0 = 100.*1000   ;!!!!! 100 km =========
dxjn = dxj/dxr0
FL   = (dxjn/0.5)^2
jj=where(dxjn gt 0.5)
FL[jj] = 1.0
FL = sqrt(fl)

FORCE = 0.01 ;!!!!!!!!========= perturbation force
ALPHA = 0.4 ;0.5
NN2J = NN2 
jj=where(nn2 lt -999.,cnt)
if(cnt gt 0)then NN2J[jj] = max(nn2)
nn2j = (nn2 > 1.0e-6) 

LL2  = FORCE/NN2j < 300.  ;LL too big, sig ok, or too big

;====> in cam LL is weighted by 
;weightp = 2*cos( (p2 - 500.)/500.*3.1416/2.0) > 0   
weightp = 1.
LL = LL2*weightp

AAM = ( (1-RH2)/0.8 < 1 ) > 0.01

DRHDZJ = 0.10 *1.0e-3 ;*RH2  ;( -dRH_DZ2 < 10.e-3 ) > 1.e-5
sig1 = (dRHDZJ *LL)*FL*10
SIG = ( sig1 + 0.05) ; add convection to it!! 

; big sig makes more cld
; small sig less cld

lev1 = cal_lev ([0.,1.],20)
levj1 = cal_lev([0.05,0.45],20)
levj2 = cal_lev([0.2,1.0],20)

AAM = ( (1-RH2) < 1 ) > 0.01
;U0  = ( (1-SIG/ALPHA*AAM) > 0.2 ) < 1  ;!!!
U0  = ( (1-SIG/ALPHA) > 0.2 ) < 1  ;!!!

;U0[*,*,*] = 0.4
jj = where ((RH2 gt U0) and (RH2 le 1 ) ,cnt)
if(cnt gt 0)then CFRAC3[jj] = 1.-sqrt( (1-RH2[jj])/(1-U0[jj]) )

jj = where((RH2 gt 0) and ( RH2 le U0),cnt1)
if(cnt1 gt 0)then CFRAC3[jj]=0

jj2 = where(RH2 ge 1.,cnt3) 
if(cnt3 gt 0)then CFRAC3[jj2] = 1.

jj2 = where(RH2 lt 0.,cnt3) 
if(cnt3 gt 0)then CFRAC3[jj2] = -9999.

;aa = cfrac
;aa = cfrac2
aa= ll
aa= sig
;aa= u0
aa = cfrac3


lev2=[1000.,2000]
end
