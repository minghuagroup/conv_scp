;run 00_range

;run 00_range
;get aa,xx,yy,lev1

igif=0
titlename = ''

var='sst'
aa = get_fld(sst_fileold,'SST_cpl')
aa = reform(aa[*,*,9]) 
xx= get_fld(sst_fileold,'lon')
yy= get_fld(sst_fileold,'lat')

lev1 = cal_lev([0,30],20)


var='sst1new'
aa = get_fld(sst_filenew,'SST_cpl')
aa = reform(aa[*,*,9]) 
xx= get_fld(sst_filenew,'lon')
yy= get_fld(sst_filenew,'lat')

cam_fileold= '../obs/FNL/Cheng/cami-mam3_0000-01-01_0.9x1.25_L30_c100618.nc'
cam_h   = '../obs/FNL/Cheng/FC5_test_2011.cam.h0.2011-10-10-00000.nc'
cam_r   = '../obs/FNL/Cheng/20111001_new.cam.r.2012-10-01.nc'
fnl_file   = '../obs/FNL/Cheng/fnl_20111001_00_00.nc'
cam_filenew= '../obs/FNL/Cheng/INIT/cami-mam3_2011-10-01_0.9x1.25_L30_c100618.nc

file=cam_h
var='TS1'
aa = get_fld(file,'TS') -273.16
xx= get_fld(file,'lon')
yy= get_fld(file,'lat')

end

