igif=0
;run 00_range
;get aa,xx,yy,lev1

 var='TAS'

file='m+tas_Amon_ACCESS1-0_historical_r1i1p1_185001-200512.nc'
xx = get_fld(file,'LON')
yy = get_fld(file,'LAT')
Data = get_fld(file,var)
im = 0
aa = reform(data[*,*,im])-273.16

stop
goto,jump1

 n=1 & m=n & nx=n & ny=n
 openr,2,'1.txt'
 readf,2,n,m,nx,ny
  lat = fltarr(m)
  lon = fltarr(n)

  x = fltarr(nx)
  y = fltarr(ny)
  datm = fltarr(n,m,12)
  dout = fltarr(nx,ny,12)

 readf,2,lat,lon
 readf,2,x,y
 readf,2,format='(8F10.3)',datm
 readf,2,format='(8F10.3)',dout
 close,2

jump1:

 im = 6
 aa = reform(datm[*,*,im])-273.16
 xx = lon
 yy = lat

 lev1 = cal_lev([-10,30],20)

stop
 
 aa = reform(dout[*,*,im])-273.16
 xx = x
 yy = y



end
