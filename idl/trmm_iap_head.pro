

print,'run iap0.pro first'
ndays =10 

ij = 1  ;for L30, L35
caseiap = 'zyx1'
print,'zyx??? caseiap # = ?'
cn=''
read,cn
caseiap = 'zyx'+cn
;====================
;caseiap = months[ij]
;icase=icase+months[ij]
;datatype2 = datatype2[ij]
;datatype = datatype[ij]

icase = caseiap
datatype2 = caseiap
datatype = caseiap

gif_folder='gif_trmm_iap_'+caseiap+'/'
fileins[0]='../obs/IAP/'+caseiap+'.cam2.h1.1979-07-27-00000.nc'

;fileins[0]='../obs/IAP/'+filems[ij] ; L30 myctl_20170727.cam.h1.2017-07-27_PRECT.nc'
;fileins[0]='../obs/IAP/'+'zyx0.cam2.h0.1979-08-06-00000.nc'
;fileins[0]='../obs/IAP/'+caseiap+'.cam2.h1.1979-08-06-00000.nc'

iix=0
igif=0
icoarse=0
titlename= icase+'_3Hourly_20170727_0805' ;'CAM_3Hourly_20170727_30'


print, titlename

;lev_prect = cal_lev([0,100],20)   ;mm/day

finday = [strtrim(indgen(5)+27,2),'01','02','03','04','05','06','07']
fin = indgen(9)*3 +3 + 100 & fins=strmid(strtrim(fin,2),1,2)  ; starting from hr 3

;fileins=strarr(10)


;fileins[0]='mytest1_xin.cam.h1.2017-07-27-PRECT.nc'
;gif_folder='gif_trmm_cam_xin1/'
;icase='xin1_'
;fileins[0] = 'my20170727_ctl.cam.h1.2017-07-28-00000.nc'

;fileins='../obs/CAM/'+fileins[0]

;fileins='GCM/'+icase+'_gcmrun.cam.h1.2017-07-27-00000.nc'
;spawn,'mkdir gif_trmm_'+icase
;gif_folder='gif_scp/'
;gif_folder='gif_trmm_'+icase+'/'

;icase = 'SCP40'

x2 = get_fld(fileins[0],'lon')
y2 = get_fld(fileins[0],'lat')
nx2 = n_elements(x2)
ny2 = n_elements(y2)
time = get_fld(fileins[0],'time')
ntime = n_elements(time)
ndays = ntime/8

xx=x2
yy=y2
yy2 = replicate2(yy,nx2) & YY2=transpose(YY2)
pi2=3.1416/180.
cosz2 = cos(yy2*pi2)

kf = 0
filein2 = fileins[0]
;PRECT = get_fld(filein2,'PRECC')+get_fld(filein2,'PRECL')           ;mm/day
PRECT = get_fld(filein2,'PRECT')
help,prect

jj = file_search(gif_folder+'/*')
if(n_elements(jj) le 1)then spawn,'mkdir '+gif_folder

ix=0
;window,/free,xsize=600,ysize=450

pthreshold = 0.2

f0728 = '../obs/IAP/zyx0.cam2.h0.1979-07-28-00000.nc'
f0729 = '../obs/IAP/zyx0.cam2.h0.1979-07-29-00000.nc'

ihour = 6 ;24
ihour = 12
;ihour = 24
;ihour = 36
;ihour = 120
ihour = 96
ihour = 48

file=fileins[0]

print,'ihour=?'
read,ihour
print,'to be followed by running iap22.pro for cldlow or iap22p for prec.'
print,'or iap23.pro with iap30 to plot 3-d fields'
end

