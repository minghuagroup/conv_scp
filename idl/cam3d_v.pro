;!interrogate 3d array level by level
;! run iap0 first

; ncks -v CLDTOT,CLDLOW,CLDHGH,CLDMED,TGCLDLWP,TGCLDIWP,TGCLDCWP,CLOUD,CLDLIQ,CLDICE,TMQ,PRECC,PRECL,PRECCDZM,PRECT,TMQ
;ncks -v CLDTOT,CLDLOW,CLDHGH,CLDMED,TGCLDLWP,TGCLDIWP,TGCLDCWP
;ncks -v PRECC,PRECL,PRECCDZM,PRECT,PREW
;ncks -v CLOUD,CLDLIQ,CLDICE,RELHUM,
;ncks -v CONCLD,LCLOUD,ICECLDF,cufrc_Cu      ; these need to be specially saved


 varsp = ['PRECT']

 print,'file in obs/iap? (cloud0, Q05 etc.)'
 caseid='' 
 ;read,caseid  
 caseid = 'cloud0605' ;------------
 file='../obs/IAP/'+caseid+'.nc'

 print,'variable?'
 ;read,var
 var = 'T'    ;-------------------
 var = 'Q'    ;-------------------
 var = 'PS'    ;-------------------

 xx = get_fld(file,'lon')
 yy = get_fld(file,'lat')


;3d array ;--------------
 zz =  get_fld(file,'lev')
 np = n_elements(zz)

;2d array ;--------------
 zz = xx ;temporarily
 np = 1

for iv=np-1,0,-1 do begin
;==========================


case var of 
'PRECCSH': begin
  dd3 = get_fld(fileins[0],'PRECC') - get_fld(fileins[0],'PRECCDZM')
 get_lev,dd3,'PRECT',lev1j,scale
  end
else: begin

 dd2 = get_fld(file,var)

 if(np eq 1)then begin
  dd3 = dd2 
  print,'var:',var
  clev = ''
 endif else begin
  dd3 = reform(dd2[*,*,iv])
  print,'var:',var,' level :',iv, '  at p=',zz[iv]
  clev = strdigit(iv,0)+'_'+strdigit(zz[iv],0)
 endelse

 get_lev,dd3,var,lev1j,scale

 if(var eq 'Q')then begin
   if(max(dd3) le 2.0e-3)then begin
    scale = scale*10. 
   endif else begin
    if(max(dd3) le 2.0e-4)then begin
     scale = scale*100. 
    endif
   endelse
 endif
      end
endcase

;if(belongsto(var,varsp))then lev1j = cal_lev([0.,18],18)
 
if(var eq 'CLOUD')then begin
  lev1j = cal_lev([0.,100],20)
  scale = 100.
endif
;if(belongsto(var,['CLDLIQ','CLDICE']))then begin
;  lev1j = cal_lev([0.,100],20)
;  scale = 1.e6
;endif

if(var eq 'PS')then lev1j = cal_lev([500.,1100],12)
 aa  = dd3*scale
; =================
 lev1 = lev1j
 lev2 = [20000.,40000]+lev1j

 ctype = '' 
 title2=''
 title2 = ' min:'+strdigit(min(aa),2)+ ' max:'+ strdigit(max(aa),2) + $
   ' scale='+ strdigit(scale,3)


;if(belongsto(var,varsp))then begin
 jj =where(aa eq 0., cnt)
 if(cnt gt 0)then aa[jj] = lev1[0]-1.
;endif


if(max(aa) eq min(aa))then begin
  print,' Skipped...',var,max(aa)
 stop
endif

lev3 = lev2

  gifname=''
  var2 = caseid+'_'+var+'_'+clev
  if(igif)then gifname = var2
 

bb=aa
window,/free,xsize=600,ysize=460,title=var2 

title = title2
  plot_map4,aa,bb,xx,yy,lev1j,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle


;  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
;        ytitle=ytitle,gifname=gifname, title2=title2,$
;        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
;        lon_width=lon_width, lat_width=lat_width
;  print,''
;  print,var, '  ',ytitle, ' iave=',iave

lev2=lev3

;if(igif)then mygif,'gifs_temp/'+gifname+'.gif'
ix = 1
read,ix
print,title2

ENDFOR ;iv

end
