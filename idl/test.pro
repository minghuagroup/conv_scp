
var = 'Z3'
f2 = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/nc/'
f1 = '../cmip6/CMIP/NCAR_CESM2/historical/nc'
var0 =var_map(var, 'cesm2cmip')
Z2 = get_dd2m(f2,var0, lon3=xx3,lat3=yy3,lev3=zz3)
Z1 = get_dd2m(f1,var0, lon3=xx3,lat3=yy3,lev3=zz3)


plev= 850
jj=where(zz3 eq plev,cnt)
if(cnt eq 0)then stop
 d1 = reform(z1[*,*,jj[0],*])
 d2 = reform(z2[*,*,jj[0],*])

; d1 = ave3(d1[*,*,5:7])
; d2 = ave3(d2[*,*,5:7])

 d1 = ave3(d1[*,*,0:2])
 d2 = ave3(d2[*,*,0:2])


 dd = d2-d1
 dd = d1

 dj2 = dd/9.8/10.
 lev1 = cal_lev([0.,40],20)
 lev1 = cal_lev([0.,20],20)
 lev1 = cal_lev([-10.,10],20)
 lev2 = lev1

 lev1 = cal_lev([85,100],20) ; 300 mb
 lev1 = cal_lev([50,62],20) ;500
 lev1 = cal_lev([12,16],20) ;850
 
 view2d,var,dj2, bb=dj2, xx3,yy3, 3, lev1,lev1 

print,'Normal stop'
stop



f1 = '../cmip6/CFMIP/NCAR_CESM2/aqua-p4K/nc'
f2 = '../cmip6/CFMIP/NCAR_CESM2/aqua-control/nc'

f1 = '../cmip6/CFMIP/NCAR_CESM2/amip-4XCO2/nc'
f2 = '../cmip6/CMIP/NCAR_CESM2/amip/nc'

;var = 'TREFHT'
var = 'TS'

;var0 =var_map(var, 'cesm2cmip')
;lwcf1 = get_dd2m(f1,var0, lon3=xx3,lat3=yy3,lev3=zz3)
;lwcf2 = get_dd2m(f2,var0)
;dcrf  = get_dd2m(f1,var0,cntl_file = f2, cntl_map = 'none' )


;stop


;ncfile_in ='/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo.nc'
;NC3FIG, ncfile_in, vars_only = ['U'], std_levels=[850.,500],setZ = 0
;NC3FIG, ncfile_in, vars_only = ['FLNT','TAUX','LWCF'], setZ = 0

;stop

var = 'T'
dir1 = '../cmip6/CMIP/NCAR_CESM2/amip/nc'
dir2 = '../cmip6/ERA5/climo/1979_1988/nc'

ts1 = get_dd2m(dir1,var_map(var,'cesm2cmip'), lon3=xx3,lat3=yy3,lev3=zz3)
ts2 = get_dd2m(dir2,var_map(var,'cesm2era') , lon3=xx0,lat3=yy0,lev3=zz0)

dts =  get_dd2m(dir1,var_map(var,'cesm2cmip'), cntl_file = dir2, cntl_map= 'cmip2era')

im=0 
aa = reform(ts2[*,*,*,im])
;zz  = zz0

aa = reform(ts1[*,*,*,im])

aa = reform(dts[*,*,*,im])
;aa = reform(ts1[*,*,im])
;aa = reform(ts2[*,*,im])
;aa = reform(dts[*,*,im])

stop

zz = zz3


aa = reform(ts1[*,*,2,im]) 
bb = reform(ts2[*,*,30,im])

aa = aa - bb
aa = reform(dts[*,*,*,im])


lev1 = get_lev_Cam(aa, 'T', diff=1)
lev2 = lev1*2

;plot_4dhtml, aa,aa,xx3,yy3,lev1,lev2
stop

z_range =0 ; [300, 400]
iav = 1


diff=0
im = 0
aa = reform(lwcf1[*,*,im])

iav =3
lev1 = get_lev_cam(aa,var, diff=diff, contrast=1)
lev2 = lev1*2

view3d, var, aa, dd2=aa, xx3,yy3,zz3, iav,lev1,lev2,z_range = z_range, lev_adjust=0,missing=-9999.


stop





set_plot,'Z'
set_plot,'PS'

mydevice=!d.name
if (mydevice eq 'X') then begin
   device,retain=2
endif
           DEVICE, Decomposed=0
           colors = GetColor(/Load, Start=1)

           !P.background = colors.white
           !P.color      = colors.black
;           !P.font=0
           !P.charsize=1.2
           !P.charthick=2

;           HELP, colors, /Structure
;          PLOT, data, COLOR=colors.yellow

Device, Set_Resolution=[500,350];, Z_Buffer=0
   Erase
plot,indgen(10),color=colors.blue
saveimage,'j.jpeg',jpeg=1


set_plot,'X'  


stop

MAP_SET, lat0, lon0, /ORTHOGRAPHIC, /ISOTROPIC, $
   LIMIT=[latmin, lonmin, latmax, lonmax]
result = MAP_IMAGE(image,Startx,Starty, COMPRESS=1, $
   LATMIN=latmin, LONMIN=lonmin, $
   LATMAX=latmax, LONMAX=lonmax)

; Display the warped image on the map at the proper position:
TV, result, Startx, Starty

; Draw gridlines over the map and image:
MAP_GRID, latdel=10, londel=10, /LABEL, /HORIZON

; Draw continent outlines:
MAP_CONTINENTS, /coasts
rturn
end


