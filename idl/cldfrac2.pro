
;USE CAM instantaneous fields

filein2 = '../obs/CAM/my20170727_ctl.cam.h1.2017-07-28-00000.nc'

filein2 = '../obs/CAM/myctl_test.cam.h0.2008-07.nc'

xx =  get_fld(filein2,'lon')
yy =  get_fld(filein2,'lat')
zz =  get_fld(filein2,'lev')
nz = 30
yy[0]=-89.9
yy[95] = 89.9
nx = n_elements(xx)
ny = n_elements(yy)

T2 =  get_fld(filein2,'T')
Q2 =  get_fld(filein2,'Q')
CLOUD2 =  get_fld(filein2,'CLOUD')

p2 = T2*0
PS2 =  get_fld(filein2,'PS')/100.
hyam =  get_fld(filein2,'hyam')
hybm =  get_fld(filein2,'hybm')
for k=0,nz-1 do P2[*,*,k,*] = 1000.*hyam[k] + PS2*hybm[k]

it = 0 ; time snapshot
T2 = reform(T2[*,*,*,it])
Q2 = reform(Q2[*,*,*,it])
P2 = reform(P2[*,*,*,it])
CLOUD2 = reform(CLOUD2[*,*,*,it])

ES2 = f_es(T2-273.18)
Qs2 = 0.622*es2/p2
RH2 = Q2/QS2

;stop

theta2 = T2*(1000./P2)^0.286
rho2   = P2/(287.*T2)*100.
dtheta_dp2 = theta2*0

;dtheta/dp
  for k=1,nz-2 do $
   dtheta_dp2[*,*,k] = (theta2[*,*,k+1] - theta2[*,*,k-1])/(p2[*,*,k+1] - p2[*,*,k-1])/100.
  k=0
   dtheta_dp2[*,*,k] = dtheta_dp2[*,*,k+1]
  k=nz-1
   dtheta_dp2[*,*,k] = dtheta_dp2[*,*,k-1] 

;dtheta_dz & N^2
   dtheta_dz2 = -dtheta_dp2*rho2*9.8

; N^2
  nn2        = dtheta_dz2/theta2*9.8

;=====================================
;stop

alpha=indgen(201)*0.01+0.01
pi =3.1416
pi4 = 4.0*pi*alpha
pi2 = 2.0*pi*alpha


sig = 0.5+sin(pi4)/pi4/2.-(sin(pi2)/pi2)^2

;plot,alpha,sig
;oplot,alpha,0.5*(alpha/0.5)^2.5,color=colors.blue

var='cld'

gifname=0
R0=0.9

CFRAC = RH2-9999.
jj=where(RH2 ge R0)
CFRAC[jj] =( ((RH2[jj]-R0) /(1.0-R0))^2 <1 ) ;^0.5
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
for j=0,ny-1 do cosz[*,j,*] = cos(3.1416/180.*yy[j])
dxj = 6400*1000.*cosz*(xx[1]-xx[0])*2*3.1416/180.
dxr0 = 100.*1000   ;!!!!! 100 km =========
dxjn = dxj/dxr0
FL   = (dxjn/0.5)^2
jj=where(dxjn gt 0.5)
FL[jj] = 1.0
FL = sqrt(fl)

; land and ocean
FORCE = 0.01 ;!!!!!!!!========= perturbation force
ALPHA = 0.4 ;0.5
NN2J = NN2 
jj=where(nn2 lt -999.,cnt)
if(cnt gt 0)then NN2J[jj] = max(nn2)
nn2j = (nn2 > 1.0e-6) 

weightp = 2*cos( (p2 - 500.)/500.*3.1416/2.0) > 0                ;!!!! how to justify???
LL2  = FORCE/NN2j   < 200.  ;LL too big, sig ok, or too big
LL = weightp*LL2
;LL = LL2

AAM = ( (1-RH2)/0.8 < 1 ) > 0.01

DRHDZJ = 0.10 *1.0e-3 ;*RH2  ;( -dRH_DZ2 < 10.e-3 ) > 1.e-5
sig1 = (dRHDZJ *LL)*FL*10
SIG = ( sig1 + 0.01) ; add convection to it!! 

; big sig makes more cld
; small sig less cld

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

levj0 = cal_lev ([0.,1.],20)
levj1 = cal_lev([0.0,0.4],20)
levj2 = cal_lev([0.2,1.0],20)


lev2=[1000.,2000]
end
