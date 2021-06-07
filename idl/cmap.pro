
igif=1
 file='../obs/precip.mon.mean.nc'
 dd3 = get_fld(file,'precip')  ;mm/day
 var = 'PRECT'
 xx = get_fld(file,'lon')
 yy = get_fld(file,'lat')
 zz = xx ;temporarily

 lev1 = cal_lev([0.,15],15)
 lev2 = lev1+2000.

;================================
 for iy = 1979,1979 do begin
; for iy = 1979,2016 do begin
  i = iy-1979
;goto,jump_web
 for im = 0,0 do begin
; for im = 0,11 do begin
;================================
  mm = strmid(strtrim(im+101,2),1,2)
  yymm = strtrim(iy,0)+'-'+strtrim(mm,2)
  print,yymm
 
  k = i*12+im
  aa = reform(dd3[*,*,k])
  
  aam = mean_g(aa, yy, -1., amin=amin)

 var2 ='CMAP_'+strtrim(yymm,2)
 ;title2 = ' min:'+strdigit(amin,2)+ ' max:'+ strdigit(max(aa),2) + $
 ;  ' mean='+ strdigit(aam,3)
 gifname=var2
 title2 =''
 lev2 = lev1+2000.

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
if(igif)then mygif,'gifs_cmap/'+gifname
  print,''
  print,var, '  ',ytitle, ' iave=',iave


stop
; wdelete
 endfor ;mm ;====================
;============================

  i1 = i*12
  i2 = i1+11
 aa = ave3(dd3[*,*,i1:i2],three=3,missing=-1)
 var2 ='CMAP_'+strtrim(iy,2)+'_ANN'
 gifname=var2
 lev2 = lev1+2000.

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
if(igif)then mygif,'gifs_cmap/'+gifname


  i1 = i*12 + 5
  i2 = i1+2
 aa = ave3(dd3[*,*,i1:i2],three=3,missing=-1)
 var2 ='CMAP_'+strtrim(iy,2)+'_JJA'
 gifname=var2
 lev2 = lev1+2000.

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
if(igif)then mygif,'gifs_cmap/'+gifname


  i1 = (i*12-1) > 0 
  i2 = i1+2
 aa = ave3(dd3[*,*,i1:i2],three=3,missing=-1)
 var2 ='CMAP_'+strtrim(iy,2)+'_DJF'
 gifname=var2
 lev2 = lev1+2000.

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
if(igif)then mygif,'gifs_cmap/'+gifname


  i1 = i*12+2
  i2 = i1+2
 aa = ave3(dd3[*,*,i1:i2],three=3,missing=-1)
 var2 ='CMAP_'+strtrim(iy,2)+'_MAM'
 gifname=var2
 lev2 = lev1+2000.

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
if(igif)then mygif,'gifs_cmap/'+gifname


  i1 = i*12+8
  i2 = i1+2
 aa = ave3(dd3[*,*,i1:i2],three=3,missing=-1)
 var2 ='CMAP_'+strtrim(iy,2)+'_SON'
 gifname=var2
 lev2 = lev1+2000.

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
if(igif)then mygif,'gifs_cmap/'+gifname

jump_web:

 htmlfile ='html/CMAP/'+strtrim(iy,2)+'.html'
 gifgroups = file_search('~/unix/workgcm/gifs_cmap/*'+strtrim(iy,2)+'*.gif')
 web_view,gifgroups,htmlfile,title='CMAP_'+strtrim(iy,2),ncolumns=2

 endfor ;yy
;============================
end
