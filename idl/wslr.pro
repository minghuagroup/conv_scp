; Sea-level Rise

Seasons    = ['DJF','MAM','JJA','SON','ANN']
jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1
foro = '../cmip6/CMIP/NCAR_CESM2/amip/nc/orog.nc'
oro = get_fld(foro,'orog')

var  = 'PS'


f0 = '../cmip6/ERA5/climo/1979_1988/nc'
f2 = '../cmip6/ERA5/climo/2009_2018/nc'
var0 = var_map(var, 'cesm2era')

f0 = '../cmip6/CMIP/NCAR_CESM2/historical/nc'
f2 = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/nc'
var0 = var_map(var, 'cesm2cmip')

ps0 = get_dd2m(f0,var0, lon3=xx,lat3=yy,lev3=zz)
ps2 = get_dd2m(f2,var0, lon3=xx,lat3=yy,lev3=zz)

kk = 4
if(kk eq 4) then jjj = indgen(12) else $
 jjj = reform(jj_seasons[*,kk])

d0 = ave3(ps0[*,*,jjj])/100.
d2 = ave3(ps2[*,*,jjj])/100.

nx = n_elements(d0[*,0])
ny = n_elements(d0[0,*])

level= 1
aa   = d0
diff = 0

aa   = d2 - d0
diff = 1
title = 'DPS'

lev1 = get_lev_cam(aa, var, scale,diff=diff,level=level)
lev2 = 100*lev1
 ;aa   = aa * scale

iave = 3

stop

view3d, title, aa, dd2=aa, xx,yy,zz, iave,lev1,lev2

lev1 = get_lev_cam(oro, 'ORO', scale,diff=0,level=1)
lev2 = 10*lev1
view3d, 'ORO', oro, dd2=oro, xx,yy,zz, iave,lev1,lev2


 yy0 = yy*3.14/180
 xx0 = xx*3.14/180

 xx00 = replicate2(xx0,ny)
 yy00 = transpose( replicate2(yy0,nx))

stop


restore,'era5.sav0'
zs = phisec
lev1 = get_lev_cam(zs, 'ORO', scale,diff=0,level=1)
lev2 = 10*lev1
view3d, var, zs, dd2=zs, xx,yy,zz, iave,lev1,lev2, title = 'ORO'

;stop

end
