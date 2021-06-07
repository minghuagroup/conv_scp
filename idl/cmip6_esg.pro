
file_esm = '/Users/minghuazhang/unix/cmip6/GCM/esm/B20TR/nc/B20TR_C35_y200_ANN_climo.nc'
lsm = get_fld(file_esm,'ORO')
oro = get_fld(file_esm,'PHIS')/9.8

dir  = '../cmip6/ESG/tmp/' 
file = 'ps_Amon_CAS-ESM2-0_ssp585_r1i1p1f1_gn_201501-210012.nc'

fileinfo = cmip6_fileinfo(file)

ps = get_dd2m(dir+file,'ps',lat3=yy,lon3=xx)


dd0 = ave3(ps[*,*,0:119])
dd2 = ave3(ps[*,*,912:1031])

dd = (dd2-dd0)/100.

zz = 0
dj2 = dd
iave=3
lev1 = cal_lev([-5.,5],20)
view3d, 'test', dj2, dd2=dj2, xx,yy,zz, iave,lev1,lev1*10


end

