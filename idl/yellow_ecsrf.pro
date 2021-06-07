
path = '/glade/p/rda/data/ds627.1/ei.moda.an.sfc/'
years = indgen(20)+1986
months = indgen(12)+101

TAS = fltarr(512,256,12)
for im = 0,11 do begin
for iy = 0,nyears-1 do begin
 file = 'ei.moda.an.sfc.regn128sc.'+year[iy]+month[im]+'0100'
 cmd = 'mv '+path+file+' ./j.grb'
 spawn,cmd
 cmd = 'ncl_convert2nc j.grb'
 x   = get_fld('j.nc','g4_lon_1')
 y   = get_fld('j.nc','g4_lat_0')
 dat = get_fld('j.nc','2D_GDS4_SFC_S123') 

 tas[*,*,im] = dat
 print,file

endfor
stop
endfor

end



