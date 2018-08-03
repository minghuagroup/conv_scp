var = 'CLOUD'

file0 = 'zmrun.cam.h0.1992-12-19-00000.nc'
file1 = 'zyxzm.cam.h0.1992-12-19-00000.nc'
file2 = 'zyxcam.cam.h0.1992-12-19-00000.nc'
file3 = 'scprun.cam.h0.1992-12-19-00000.nc'
file4 = 'zyxscp.cam.h0.1992-12-19-00000.nc'
file5 = 'zyxtest.cam.h0.1992-12-19-00000.nc'
filecam = 'zyxcam0801.cam.h0.1992-12-19-00000.nc'
filescp = 'zyxscp0802.cam.h0.1992-12-19-00000.nc'
filezyx = 'zyxzyx0802.cam.h0.1992-12-19-00000.nc'
filetest = 'zyxtest.cam.h0.1992-12-19-00000.nc'

filezm = 'ztwpzm0803.cam.h0.2006-01-18-00000.nc'
filecam = 'ztwpcam0803.cam.h0.2006-01-18-00000.nc'
filescp = 'ztwpscp0803.cam.h0.2006-01-18-00000.nc'
filezyx = 'ztwpzyx0803.cam.h0.2006-01-18-00000.nc'



cld0 = get_fld(file0,var)
cld1 = get_fld(file1,var)
cld2 = get_fld(file2,var)
cld3 = get_fld(file3,var)
cld4 = get_fld(file3,var)
i1 = 200
print,'file0 ',file0,  '   for zmrun'
print,'file1 ',file1,  '   for zyxzm with uw'
print,'file2 ',file2,  '   for zyxcam no uw'
print,'file3 ',file3,  '   for scprun'
print,'file4 ',file4,  '   for zyxscp'
print,'file5 ',file5,  '   for zyxtest'
print,'filecam ',filecam,  '   for zyxcam0801'
print,'filescp ',filescp,  '   for zyxscp0802'
print,'filezyx ',filezyx,  '   for zyxzyx0802'
print,'filetest ',filetest,  '   for zyxtest
ncdf_vars,filetest,vars

;print, 'cld0[*,i1] ',reform(cld0[20:24,i1])
;print, 'cld1[*,i1] ',reform(cld1[20:24,i1])
;print, 'cld2[*,i1] ',reform(cld2[20:24,i1])
;print, 'cld3[*,i1] ',reform(cld3[20:24,i1])
;print, 'cld4[*,i1] ',reform(cld4[20:24,i1])

dqdt = 'STENDCONVDPCOND'
dqldt = 'QTENDCONVDPCOND'
dqidt = 'STENDCONVDPEVAP'
dncdt = 'QTENDCONVDPEVAP'
rprd = 'STENDCONVDPTRANUP'
dlf = 'QTENDCONVDPTRANUP'
ntprprd = 'STENDCONVDPTRANDN'
precrate = 'QTENDCONVDPTRANDN'


iav=6
var = 'CLOUD'
print,'set VAR !!!: ',var, ' iav=',iav
print,'use var=vname("precrate") etc to do other var names'
print,'then type --> ncdf_view,var=var,iav=iav,file=file N...'
end


