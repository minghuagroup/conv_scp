; ncks -v CLDTOT,CLDLOW,CLDHGH,CLDMED,TGCLDLWP,TGCLDIWP,TGCLDCWP,CLOUD,CLDLIQ,CLDICE,TMQ,PRECC,PRECL,PRECCDZM,PRECT,TMQ
;ncks -v CLDTOT,CLDLOW,CLDHGH,CLDMED,TGCLDLWP,TGCLDIWP,TGCLDCWP
;ncks -v PRECC,PRECL,PRECCDZM,PRECT,PREW
;ncks -v CLOUD,CLDLIQ,CLDICE,RELHUM,
;ncks -v CONCLD,LCLOUD,ICECLDF,cufrc_Cu      ; these need to be specially saved


 print, 'to run iap0 first for instant, here to prepare for aa zz,xx etc first'

print, 'run iap30 first to determin plot types for MEAN input file'
 
; print,'filenumber cloud???? (number or n) or set file '
; caseid='' 
; read,caseid
; if(caseid ne 'n')then begin
;   file='../obs/IAP/cloud'+caseid+'.nc'
; endif else begin
;   print,'filename in ../obs/IAP?'
;   read,file
;   file='../obs/IAP/cloud'+file
; endelse

vars = ['CLDLIQ']
vars = ['CLDLIQ','CLDICE']
vars = ['CLDLOW']
vars = ['CLDTOT','CLDLOW','CLDHGH','CLDMED']
vars = ['CLOUD','CLDLIQ','CLDICE','RELHUM']
vars = ['CONCLD','LCLOUD','ICECLDF','cufrc_Cu']
vars = ['TGCLDLWP']
vars = ['CLOUD']
vars = ['TMQ']
varsp = ['PRECC','PRECL','PRECCDZM','PRECT']
vars = ['PRECC','PRECL','PRECCDZM','PRECT','PRECSH','TMQ']
vars = ['TGCLDLWP','TGCLDIWP']
vars = ['PBLH']
vars = ['CLDTOT','CLDLOW','CLDHGH','CLDMED']
vars = ['CONCLD','CLOUD']
vars = ['CLDLIQ','CLDICE','CLOUD']
vars = ['TGCLDLWP','TGCLDIWP']




 xx = get_fld(file,'lon')
 yy = get_fld(file,'lat')
 zz = xx ;temporarily

 
nv = n_elements(vars)
for iv=0,nv-1 do begin
;==========================
 var = vars[iv]

case var of 
'PRECCSH': begin
  dd3 = get_fld(fileins[0],'PRECC') - get_fld(fileins[0],'PRECCDZM')
 get_lev,dd3,'PRECT',lev1j,scale
  end
else: begin
 dd3 = get_fld(file,var)
 get_lev,dd3,var,lev1j,scale
      end
endcase

if(belongsto(var,varsp))then lev1j = cal_lev([0.,18],18)
 
 sz = size(dd3)

 if(sz[0] eq 3)then begin
  zz = get_fld(file,'lev')
 endif


if(var eq 'CLOUD')then begin
  lev1j = cal_lev([0.,100],20)
  scale = 100.
endif
;if(belongsto(var,['CLDLIQ','CLDICE']))then begin
;  lev1j = cal_lev([0.,100],20)
;  scale = 1.e6
;endif

 ;aa  = reform(dd3[*,*,*,it])*scale
 aa  = dd3*scale

 lev1 = lev1j
 lev2 = [2000.,4000]+lev1j


if(max(aa) eq min(aa))then begin
  print,' Skipped...',var,max(aa)
 stop
endif

lev3 = lev2

  gifname=''
  var2 = caseid+'_'+ctype+'_'+var
  if(igif)then gifname = var2

 
 title2=''
 if(strpos(ctype,'Vertical') ne 0)then $
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
