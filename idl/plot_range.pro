
igif=0
ixx = 0

 filein = '../diag/cesm20ctl/cesm20ctl_ANN_climo.nc'
 ncdf_vars,filein,vars

 lon = get_fld(filein,'lon')
 lat = get_fld(filein,'lat')
 lev = get_fld(filein,'lev')
 time = get_fld(filein,'time')

 print,'Which variable?'
 var=''
 read,var

 aa = get_fld(filein,var)
 print,var,' ','  max=',max(aa),'  min=',min(aa)
 lev1=[0.,1]
 scale1 = 1.
 print,'lev1 and scale'
 read,lev1,scale

 lev1= cal_lev(lev1,20)
 aa = scale*aa

 ctypes =['1: Zonal','2: Meridional','3: Vertical','4:GPCI','5:Level']
 for i=0,n_elements(ctypes)-1 do print,ctypes[i] 
 print,'which plot type?
;======


 iave=1 
 read,iave

 iave2=iave

case iave of
 1:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Zonal_mean'
    
    xx = lat
    yy = lev
   end 

 2:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Meridional_mean'

    xx = lon
    yy = lev
   end 

 3:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,100]
    ctype = 'Vertical_mean'
    xx = lon
    yy = lat
   end 

 4:begin
    lon_range = 360-[173.,119]
    lat_range = [0.,40]
    z_range   = [1000.,0]
    lat_width=2.  ; for iave=4 only
    lon_width=2.
    ctype = 'GPCI'
    xx = lat
    yy = lev
   end 

 5:begin
   z_range   = [950.,950]
   mb = 1000.
   print,'which level?'
   read,mb
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [mb,mb]
    ctype = strdigit(mb,0)+'mb'
    iave=3   ; vertically level
    xx = lon
    yy = lat
   end 
 else:
 endcase


  xx = lon
  yy = lat
  zz = lev
  lev2 = lev1*1.0e6
  if(igif)then gifname = var

  view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''



end
