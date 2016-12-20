
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;load "$SCMDIAG_CODE/scmlib.ncl"
load "$XXUTILS"

begin

model="scm"
;for gcm
;lats = 20
;late = 90
;lons = 0;40
;lone = 143;120 
;;for scm
lats = 0 ;30
late = 0 ;30
lons = 0 ;80
lone = 0 ;80

f = addfile("./cam"+model+".nc", "r")

avo=6.02214e26
boltz=1.38065e-23
rgas=avo*boltz
md=28.966

g = 9.8
rair = rgas/md
p0 = 1.e5
cp = 1.00464e3
cappa = rair/cp
lv = 2.5e6


lat = f->lat
lon = f->lon
lev = f->lev
time = f->time
ntime = dimsizes(time)
dtime = tofloat( (time(1)-time(0))*24*3600. )
print("dtime:"+dtime)

nlev = dimsizes(lev)

times = 1
timee = ntime-1
;timee = 25

u = f->OFFU(times:timee, :, lats:late, lons:lone)
;u = f->U
v = f->OFFV(times:timee, :, lats:late, lons:lone)
;v = f->V

t = f->OFFT(times:timee, :, lats:late, lons:lone)
;t = f->T
q = f->OFFQ(times:timee, :, lats:late, lons:lone)
;q = f->Q
bfls_t = f->BFLST(times:timee, :, lats:late, lons:lone)
bfls_q = f->BFLSQ(times:timee, :, lats:late, lons:lone)

tmp = t
delete(tmp@long_name)
delete(tmp@units)
tmp2d = f->PHIS(times:timee, lats:late, lons:lone)
delete(tmp2d@long_name)
delete(tmp2d@units)

p = f->PMID(times:timee, :, lats:late, lons:lone)

rho = tmp
rho = p/rair/t
rho@long_name = "air density"
rho@units = "kg/m3"


omega = tmp
omega = f->OFFOMEGA(times:timee, :, lats:late, lons:lone)
omega@long_name = "Pressure Vertical Velocity"
omega@units = "Pa/s"


;pi = tmp
;pi = (p/p0)^cappa
;pi@long_name = "exner function"
;pi@units = "1"

;theta = tmp
;theta = t/pi
;theta@long_name = "potential temeperature"
;theta@units = "K"

z = f->Z3(times:timee, :, lats:late, lons:lone)
;he = tmp
;he = g*z+cp*t+lv*q

psrf = f->PSRF(times:timee, lats:late, lons:lone)
dp = f->PDEL(times:timee, :, lats:late, lons:lone)
zsrf = f->ZSRF(times:timee, lats:late, lons:lone)
dz = f->ZDEL(times:timee, :, lats:late, lons:lone)

ht = tmp2d
ht = f->PHIS(times:timee, lats:late, lons:lone)/g
ht@long_name = "surface height"
ht@units = "m"

landf = tmp2d
landf = f->LANDFRAC(times:timee, lats:late, lons:lone)
landf@long_name = "land fraction"
landf@units = "1"

;precc = f->PRECC
precc = f->PRECDP(times:timee, lats:late, lons:lone)
;precc = precc
prect = f->PRECT(times:timee, lats:late, lons:lone)
;prect = prect


stend = f->CONVDPSTEND(times:timee, :, lats:late, lons:lone)
qtend = f->CONVDPQTEND(times:timee, :, lats:late, lons:lone)
stendcond = f->CONVDPCONDSTEND(times:timee, :, lats:late, lons:lone)
qtendcond = f->CONVDPCONDQTEND(times:timee, :, lats:late, lons:lone)
stendtranup = f->CONVDPTRANUPSTEND(times:timee, :, lats:late, lons:lone)
qtendtranup = f->CONVDPTRANUPQTEND(times:timee, :, lats:late, lons:lone)
stendtrandn = f->CONVDPTRANDNSTEND(times:timee, :, lats:late, lons:lone)
qtendtrandn = f->CONVDPTRANDNQTEND(times:timee, :, lats:late, lons:lone)


outfile="input"+model+".nc"
fout = addfile(outfile, "crw")

fout->t = t
fout->q = q

fout->bfls_t = bfls_t
fout->bfls_q = bfls_q

fout->u = u
fout->v = v
fout->omega = omega

fout->z = z
fout->p = p

fout->psrf = psrf
fout->dp = dp
fout->zsrf = zsrf
fout->dz = dz
fout->rho = rho

;fout->he = he
;fout->pi = pi

fout->camstend = stend
fout->camqtend = qtend
fout->camstendcond = stendcond
fout->camqtendcond = qtendcond
fout->camstendtranup = stendtranup
fout->camqtendtranup = qtendtranup
fout->camstendtrandn = stendtrandn
fout->camqtendtrandn = qtendtrandn

fout->ht = ht
fout->landfrac = landf
fout->camprecc = precc
fout->camprect = prect

end

