
pro myncview,filein=filein,aa=aa,var=var,iave=iave,lev1=lev1,scale=scale,igif=igif,gifname=gifname

if(not keyword_set(filein)) then begin
 print,'input .nc file name?'
 read,filein
endif


if(not keyword_set(var)) then begin
 ncdf_vars,filein,vars
 print,'Which variable?'
 var=''
 read,var
endif

 if(not keyword_set(aa))then $
 aa = get_fld(filein,var)

 aa=reform(aa)

if(not keyword_set(igif))then igif=0
if(not keyword_set(gifname))then gifname = var

if(not keyword_set(lev1)) then begin
    print,var,' ','  min=',min(aa),'  max=',max(aa)
    lev1=[0.,1]
    print,'color range lev1?'
    read,lev1
    lev1= cal_lev(lev1,20)
 endif

if(not keyword_set(scale)) then begin
 print,'scale=?' 
 scale1 = 1.
 read,scale
endif

if(not keyword_set(lev2)) then begin
 lev2=lev1*1.0e6
endif

 aa = scale*aa

 sz = size(aa)

 ctypes =['0: Time series','1: Zonal','2: Meridional','3: Vertical',$
        '4:GPCI','5:Level','6:time_lev_section']

if(not keyword_set(iave)) then begin
 for i=0,n_elements(ctypes)-1 do print,ctypes[i] 
 print,'......'
 print,'which plot type?
 iave=1 
 read,iave
endif
;======

case iave of
 1:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Zonal_mean'
   end 

 2:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Meridional_mean'
   end 

 3:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,100]
    ctype = 'Vertical_mean'
   end 
 4:begin
    lon_range = 360-[173.,119]
    lat_range = [0.,40]
    z_range   = [1000.,0]
    lat_width=2.  ; for iave=4 only
    lon_width=2.
    ctype = 'GPCI'
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
   end 
 6:begin
   ctype=ctypes[iave-1]
   end 
 else: ctype=''
 endcase

 lon =1.0
 lat=1.0
if(not belongsto(iave,[0,6]))then begin
 lon = get_fld(filein,'lon')
 lat = get_fld(filein,'lat')
endif

 lev = get_fld(filein,'lev')
 time = get_fld(filein,'time')

 sz = size(aa)
  xx = lon
  yy = lat
  zz = lev
  tt = time

 window,/free,title=filein+'_'+var+'_'+ctype
;=============
 case sz[0] of ;dimensions of arrays
  1: begin
      plot,tt,aa,xrange=[min(tt),max(tt)],yrange=[min(lev1),max(lev1)],title=var
     end
  2: begin
      bb=aa

   if(iave ne 6)then begin 
     plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
           title=var,xtitle='longitude',ytitle=ytitle
    endif else begin
     plot_4dhtml,aa,bb,tt,zz,lev1,lev2,xrange=[min(tt),max(tt)],$
           yrange=[1000.,100],xtitle='time',tran=1,title=var
    endelse

     end
  3: begin
   view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width

     end

  else:   begin
           print,'array size not in 1-3:'
           help,filein,var,aa
           end
  endcase

      if(igif)then mygif,'gif_3d/tmp_'+gifname+'.gif'
      print,var,' ','  min=',min(aa),'  max=',max(aa)

     print,'use myview3d,filein=filein,aa=aa,var=var,iave=iave,lev1=lev1,$'
     print,'   scale=scale,igif=igif,gifname=gifname'
  return


end
