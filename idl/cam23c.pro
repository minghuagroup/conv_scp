 print, 'to run iap0 first for instant, here to prepare for aa zz,xx etc first'

print, 'run iap30 iap21 first to determin plot types and HOURLY input file'
 
; print,'filenumber cloud???? ihour?'
; caseid='' 
; read,caseid,ihour
; file='../obs/IAP/cloud'+caseid+'.nc'

 it = ihour/3

vars = ['CLOUD','CLDLIQ','CLDICE']
vars = ['CLOUD']
vars = ['CLDLIQ']
vars = ['CLDLIQ','CLDICE']

 xx = get_fld(file,'lon')
 yy = get_fld(file,'lat')
 zz = get_fld(file,'lev')
 
nv = n_elements(vars)
for iv=0,nv-1 do begin
;==========================
 var = vars[iv]

 dd3 = get_fld(file,var)

 get_lev,dd3,var,lev1j,scale

if(var eq 'CLOUD')then begin
  lev1j = cal_lev([0.,100],20)
  scale = 100.
endif
if(belongsto(var,['CLDLIQ','CLDICE']))then begin
  lev1j = cal_lev([0.,100],20)
  scale = 1.e6
endif
 aa  = reform(dd3[*,*,*,it])*scale

 lev2 = [2000.,4000]+lev1j


if(max(aa) eq min(aa))then begin
  print,' Skipped...',var,max(aa)
 stop
endif

lev3 = lev2

  var2 = caseid+'_'+ctype+'_'+var
  if(igif)then gifname = var2

 title2 = ' min:'+strdigit(min(aa),2)+ ' max:'+ strdigit(max(aa),2) + $
   ' scale='+ strdigit(scale,3)

window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  print,var, '  ',ytitle, ' iave=',iave

lev2=lev3

ENDFOR ;iv

end
