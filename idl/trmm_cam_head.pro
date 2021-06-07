print,'run 00_range'
ndays =10 

ij = 0  ;for L30
icase=icase+months[ij]
datatype2 = datatype2[ij]
datatype = datatype[ij]

iix=0
igif=0
icoarse=0
titlename= icase+'_3Hourly_20170727_0805' ;'CAM_3Hourly_20170727_30'
print, titlename

;lev_prect = cal_lev([0,100],20)   ;mm/day

finday = [strtrim(indgen(5)+27,2),'01','02','03','04','05','06','07']
fin = indgen(9)*3 +3 + 100 & fins=strmid(strtrim(fin,2),1,2)  ; starting from hr 3

fileins=strarr(10)

fileins[0]=filems[ij] ; L30 myctl_20170727.cam.h1.2017-07-27_PRECT.nc'
gif_folder='gif_trmm_iap/'

;fileins[0]='mytest1_xin.cam.h1.2017-07-27-PRECT.nc'
;gif_folder='gif_trmm_cam_xin1/'
;icase='xin1_'
;;fileins[0] = 'my20170727_ctl.cam.h1.2017-07-28-00000.nc'

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

kf = 0
filein2 = fileins[0]
;PRECT = get_fld(filein2,'PRECC')+get_fld(filein2,'PRECL')           ;mm/day
PRECT = get_fld(filein2,'PRECT')

ix=0

pthreshold = 0.2

end

