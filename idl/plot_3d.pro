print, 'run 00_3d first, assign aa, xx,yy,zz,lev1,lev2'

if(max(aa) eq min(aa))then begin
  print,' Skipped...',var,max(aa)
 stop
endif

lev2 = [9999.,99999.]
lev3 = lev2

gif=0
var2 = ''
;  var2 = caseid+'_'+ctype+'_'+var
;  if(igif)then gifname = var2

window,/free,xsize=600,ysize=460,title=var2
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  ;print,var, '  ',ytitle, ' iave=',iave

lev2=lev3

end

