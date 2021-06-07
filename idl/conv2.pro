
file = '../obs/IAP/core367.cam2.h0.1992-12-19-19800.nc'
file = '../obs/IAP/twp367.cam2.h0.2006-01-22-19800.nc'

lev = get_fld(file,'lev')
ilev = get_fld(file,'ilev')
time = get_fld(file,'time')
t = get_fld(file,'T')
sz = size(reform(t)) 
np = sz[1] 
nt   = sz[2]
pver = np-1
hyam= get_fld(file,'hyam')
hybm= get_fld(file,'hybm')
hyai= get_fld(file,'hyai')
hybi= get_fld(file,'hybi')
ps= get_fld(file,'PS')
p0 = 100000.; get_fld(file,'P0')
pp = replicate2(lev,nt)*100.
ppi = replicate2(ilev,nt)*100.

for k=0,pver do begin
 pp[k,*] = hyam[k]*p0+hybm[k]*ps[*]
endfor
for k=0,np do begin
 ppi[k,*] = hyai[k]*p0+hybi[k]*ps[*]
endfor

q = get_fld(file,'Q')
mu0 = get_fld(file,'mu0')
eu0 = get_fld(file,'eu0')
;du0 = get_fld(file,'du0')
buoy_up = get_fld(file,'buoy_up')
Z3 = get_fld(file,'Z3')
pconvt = get_fld(file,'PCONVT')
pconvb = get_fld(file,'PCONVB')
precc = get_fld(file,'PRECC')
prect = get_fld(file,'PRECT')
cmfmc = get_fld(file,'CMFMC')
cmfmcdzm= get_fld(file,'CMFMCDZM')
cmfmc2 = cmfmc - cmfmcdzm
relhum= get_fld(file,'RELHUM')


cloud= get_fld(file,'CLOUD')
concld= get_fld(file,'CONCLD')
cldst= get_fld(file,'CLDST')
AST= get_fld(file,'AST')
ALST= get_fld(file,'ALST')
AIST= get_fld(file,'AIST')
U00= get_fld(file,'U00')
DRH00= get_fld(file,'DRH00')
;= get_fld(file,'')


 sh1     =  0.07
 sh2     =  500.0
 dp1     =  0.10
 dp2     =  500.0

  shcu1 = sh1*alog(1.0+sh2*cmfmc2)
  dpcu1 = dp1*alog(1.0+dp2*cmfmcdzm)

  dpcu = dlimit(dpcu1, 0.0, 0.6)
  shcu = dlimit(shcu1, 0.0, 0.3)

 es = f_eswi(t-273.16)
 qs = 0.622*es/pp*100.
 rh = q/qs 


 cld = cloud*0.0
 for i=0,nt-1 do begin
 for k=0,np-1 do begin
  cld[k,i] = cld_f(rh[k,i],u00[k,i],3,umax=1.0)
 endfor
 endfor


stop

b = buoy_up

pver=34
p = lev
dz1 = 0.3
dz2 = 0.15
wint = 1.0
alpha = 0.1
beta = 0.2
;-------------------------
ii = 35
pt = pconvt[i1]/100.
pb = pconvb[i1]/100.
z  = reform(z3[*,ii])
dz = (shift(z,1)-shift(z,-1))/2.
dz[0]    = dz[1]/2.
dz[pver] = dz[pver-1]/2.
rho  = lev/287./reform(T[*,ii])*100.
b = reform(buoy_up[*,ii])*9.8/reform(T[*,ii])

w=lev*0.

eps0 = 0.0002
eps_turb = 0.0001

pj = abs(p-pt)
jj = where(pj eq min(pj))
jt = jj[0]
pj = abs(p-pb)
jj = where(pj eq min(pj))
jb = jj[0]
print,jt,jb

zt = z[jt]
zb = z[jb]



eps = lev*0.
del = lev*0.

zz = (reform(z3[*,ii])-zb)/(zt-zb)

zb2 = zb + dz1*(zt-zb)
zj  = abs(z-zb2)
jj = where(zj eq min(zj))
jb2 = jj[0]
zt2 = zt - dz2*(zt-zb)
zj  = abs(z-zt2)
jj = where(zj eq min(zj))
jt2 = jj[0]
zd2 = zt - 0.5*(zt-zb)
zj  = abs(z-zd2)
jj = where(zj eq min(zj))
jd2 = jj[0]
print,jt2,jb2,jd2

for iter=1,2 do begin ;================
 eps0 = 0.0002 - (iter-1)*0.0002

jj=where((zz ge 0.0) and (zz le dz1))
eps[jj] = (1. - zz[jj]/dz1)

jj=where((zz ge 1.-dz2) and (zz le 1.))
del[jj] = (zz[jj] -(1.-dz2)) /dz2

 teps = total(eps[jb2-1:jb]*dz[jb2-2:jb-1] )
 tdel = total(del[jt+1:jt2+1]*dz[jt:jt2] )

 ratio = teps/tdel

;stop


eps = eps0*eps
del = ratio*eps0*del

mu = lev*0.
mu[jb]=1.
for k=jb-1,jd2,-1 do begin
 mu[k] = mu[k+1]*exp(eps[k+1]*dz[k])
endfor

for k=jd2,jt,-1 do begin
 mu[k] = mu[k+1]*exp(-del[k+1]*dz[k])
endfor

print,'del ratio=',ratio
;print,eps
;print,del
print,mu

for k=jb-1, 2, -1 do begin
 c1 = alpha*B[k]
 wj = exp(-eps[k+1]*dz[k])
 if(eps[k+1] le 1.0e-6)then begin
  w2 = wj * w[k+1]*w[k+1] + dz[k]*c1
 endif else begin
  w2 = wj * w[k+1]*w[k+1] + (1.-wj)/eps[k+1]*c1
 endelse
  
 if(w2 lt 0.)then begin
  jtt = k
  print,'new jt',jtt
  goto,jump1
 endif

  w[k] = sqrt(w2)

endfor
jump1:

if(iter eq 1)then begin
plot,mu,lev,yrange=[1000.,100.]
oplot,mu,lev,psym=2
oplot,w/10.,lev,color=colors.red
oplot,w/10.,lev,color=colors.red,psym=1
w1 = w
mu1 = mu
e1 = eps0

endif else begin

oplot,mu,lev,color=colors.green
oplot,mu,lev,psym=2,color=colors.green
oplot,w/10.,lev,color=colors.blue
oplot,w/10.,lev,color=colors.blue,psym=1

w2 = w
mu2 = mu
e2 = eps0

endelse

endfor ;--------- iter

jj=where((w1 gt 1.0e-2) and (b gt 0.))
eps2 = lev*0.
eps2[jj] = sqrt(b[jj])/w1[jj]

window,/free
plot,eps2,lev,yrange=[1000.,100]
oplot,b,lev,color=colors.red

window,/free
plot,sqrt(b[jj]),lev[jj],yrange=[1000.,100]
oplot,1/w1[jj]/10.,lev[jj],color=colors.red

end
