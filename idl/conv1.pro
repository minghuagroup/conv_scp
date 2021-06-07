iplot=0
file = '../obs/IAP/core365.cam2.h0.1992-12-19-19800.nc'
file = '../obs/IAP/twp365.cam2.h0.2006-01-22-19800.nc'
file = '../obs/IAP/twp371.cam2.h0.2006-01-22-19800.nc'

cp = 1004.63
Rv = 287.4
g = 9.8
Lv = 2.5e6

lev = get_fld(file,'lev')
time = get_fld(file,'time')
t = get_fld(file,'T')
q = get_fld(file,'Q')
mu0 = get_fld(file,'mu0')
eu0 = get_fld(file,'eu0')
du0 = get_fld(file,'du0')
ed0 = get_fld(file,'ed0')
buoy_up = get_fld(file,'buoy_up')
Z3 = get_fld(file,'Z3')
pconvt = get_fld(file,'PCONVT')
pconvb = get_fld(file,'PCONVB')
precc = get_fld(file,'PRECC')
prect = get_fld(file,'PRECT')

nt=n_elements(time)
np=n_elements(lev)

b = buoy_up


pver=34
p = lev
dz1 = 0.15
dz2 = 0.15
wint = 1.0
alpha = 0.1
beta = 0.2
radius = 10.e3
;-------------------------

mu2 = mu0*0
ww2 = mu2
bb2 = mu0
eu2 = mu0
du2 = mu0

jt0 = indgen(nt)*0
jtt = indgen(nt)*0

FOR i1 = 0,nt-1 do begin
;==========================================
;i1 = 55

pt = pconvt[i1]/100.
pb = pconvb[i1]/100.

z  = reform(z3[*,i1])
dz = (shift(z,1)-shift(z,-1))/2.
dz[0]    = dz[1]/2.
dz[pver] = dz[pver-1]/2.
rho  = lev/287./reform(T[*,i1])*100.
qmn = reform(q[*,i1])
tmn = reform(T[*,i1])
b0 = reform(buoy_up[*,i1])*9.8/reform(T[*,i1])

;specified
eps_turb = 0.0002

;initial guess
eps0     = 0.0002        ;- (iter-1)*0.0002

 mu = lev*0.
 w  = mu
 
for iter=1,3 do begin ;================

 print,'pt=',pt
pj = abs(p-pt)
jj = where(pj eq min(pj))
jt = jj[0]
pj = abs(p-pb)
jj = where(pj eq min(pj))
jb = jj[0]
print,jt,jb

if(iter eq 1)then jt0[i1] = jt

zt = z[jt]
zb = z[jb]

;stop
; get eps and del profiles
 get_eps_prof, z,dz,zb,zt,dz1,dz2, jb2, jt2, jd2 ,epsprof,delprof,ratio
 ;----------
;stop
 eps = eps0*epsprof
 del = eps0*delprof
 

; get jt2 and buoyancy
 get_parcel2, tmn[jb], qmn[jb], jb, 5, tmn, qmn, z,p, eps+eps_turb, tp, qvp, qlp,buoy,jt2
 ;----------
;stop
; no positive buoyancy
 if(jt2 lt 0)then goto,next_it  ; no positive buoyancy
 if(jd2 gt jb-2)then goto,next_it  ; no positive buoyancy

; mass flux
 mu[jb]=1.
 for k=jb-1,jd2,-1 do begin
   mu[k] = mu[k+1]*exp(eps[k+1]*dz[k])
 endfor
 for k=jd2,jt,-1 do begin
  mu[k] = mu[k+1]*exp(-del[k+1]*dz[k])
 endfor

; print,'mu',mu

;overcome all negative b below bmax
 b = buoy
 jmaxb = where(b eq max(b))
 jmaxb = jmaxb[0]
 bmax  = max([b[jmaxb],0.]) 
 for k=jb,jmaxb,-1 do b[k]=max([b[k],0.]) ;overcome all negative b

;vertical velocity
 w[jb] = wint
 for k=jb-1, 5, -1 do begin
   c1 = alpha*B[k]
   eps1 = eps[k+1]+eps_turb  ;[k+1]
   wj = exp(-eps1*dz[k])

     if(eps1 le 1.0e-6)then begin
        w2 = wj * w[k+1]*w[k+1] + dz[k]*c1
     endif else begin
        w2 = wj * w[k+1]*w[k+1] + (1.-wj)/eps1*c1
     endelse
; ---------------------------------
;     w3 = w[k+1]
;     w2 =  w[k+1]*w[k+1] +(c1-eps1*w3^3)*dz[k]  
   if(w2 lt 0.)then begin
      jt3 = k
      print,'new jt',jt3

      dz2 = (z[jt3] - z[jmaxb])/(z[jt3]-z[jb])
      dz2 = min([dz2, 0.40])    
      dz2 = max([dz2, 0.10])    

    ;  print,'b:',b
    ;  print,'w:',w
         goto,jump1
    endif
    w[k] = sqrt(w2)
 endfor
 jump1:
;  print,'w',w

;stop
;=================
wmax  = max([w[jmaxb],  0.01])
eps0g = sqrt(bmax*(z[jt]-z[jb]))/Radius/wmax *0.2/dz1*0.3

;stop
print,'i1,iter,eps0g',i1,iter,eps0,bmax,wmax

eps0 = eps0g
jt = max([jt3,5])
pt = p[jt3]

endfor ;--------- iter

next_it:

 mu2[*,i1] = mu
 ww2[*,i1] = w
 bb2[*,i1] = b
 eu2[*,i1] = eps + eps_turb
 du2[*,i1] = del + eps_turb

 jtt[i1]   = jt

;stop
 ix=1
; read,ix
 endfor ; it
;==================;======================
 bb3 = bb2/g*t


end
