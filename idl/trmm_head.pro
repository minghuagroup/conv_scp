

iix=0
igif=1

icoarse = 1  ; cam grids

titlename= 'TRMM_3Hourly_20170727_30'
print, titlename

finday = [strtrim(indgen(5)+27,2),'01','02','03','04','05','06','07']
fin = indgen(9)*3 + 100 & fins=strmid(strtrim(fin,2),1,2)

ndays=10  ;!!

fileins=strarr(31,8)  ; day x hr
fileins[0,*]='3B42RT.20170727'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[1,*]='3B42RT.20170728'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[2,*]='3B42RT.20170729'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[3,*]='3B42RT.20170730'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[4,*]='3B42RT.20170731'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[5,*]='3B42RT.20170801'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[6,*]='3B42RT.20170802'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[7,*]='3B42RT.20170803'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[8,*]='3B42RT.20170804'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[9,*]='3B42RT.20170805'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N
fileins[10,*]='3B42RT.20170806'+fins[0:7]+'.7.bin' ; 3hr data in mm/hr -60 to 60N

fileins='../obs/TRMM/'+fileins

x2 = indgen(1440)*0.25 + 0.125
y2 = -indgen(480)*0.25 + 59.875

cam_file = '../obs/CAM/my20170727_ctl.cam.h0.2017-07-28-00000.nc'
xc = get_fld(cam_file,'lon')
yc = get_fld(cam_file,'lat')

ix=0
end
