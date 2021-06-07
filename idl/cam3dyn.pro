print, 'plot text file to run iap30 first for files prepare on storm by proc_dyn3.pro in fout2d folder'
print, 'run iap30 first to determin plot types for MEAN input file'

vars2d = ['psa','pt']
vars3d = ['ply','tt','ut','vt','deltac']

vars2db = ['psa''pt']
vars3db = ['P','T','U','V','deltac']

;++++++++++++++++++++
 nstep = 10605

 var   = 'vt'
 var_scale = 'U'

 var   = 'pt'
 var_scale = 'PS'

 var   = 'ply'
 var_scale = 'P'

;++++++++++++++++++++

 caseid=var+'_'+strdigit(nstep,0)

;======================????
 file='../obs/IAP/fout2d/'+caseid+'.sav'

 print,'variable and file'
 print,var, ' read from file ',file
;====================??????

;3d array ;--------------
 restore,'lev.sav'
 zz = lev

 restore,file     ; <- dd3 and xx, tt read in here !!!!
 ;================<<<<<<<<<<
 help,xx,yy,dd3
 print,'min,max: ',min(dd3),max(dd3)
 dd2 = dd3

  
;if(belongsto(var,['CLDLIQ','CLDICE']))then begin
;  lev1j = cal_lev([0.,100],20)
;  scale = 1.e6
;endif

 get_lev,dd3,var_scale,lev1j,scale

 if(var eq 'pt')then begin
    dd3 = dd2*dd2*100000.
    lev1j = cal_lev([550.,1050],20)
  endif
if(var eq 'PS')then lev1j = cal_lev([500.,1100],12)
if(var eq 'T')then begin
  dd3 = dd3 -273.16
 lev1j = cal_lev([-50.,40],20)
endif
 aa  = dd3*scale
; =================
 lev1 = lev1j
 lev2 = [20000.,40000]+lev1j

; ctype = '' 
 title2=''
 title2 = ctype+' min:'+strdigit(min(aa),2)+ ' max:'+ strdigit(max(aa),2) + $
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
  var2 = caseid+'_'+ctype+'_'+var
  if(igif)then gifname = var2

 
; if(strpos(ctype,'Vertical') ne 0)then $
; title2 = ' min:'+strdigit(min(aa),2)+ ' max:'+ strdigit(max(aa),2) + $
;   ' scale='+ strdigit(scale,3)

sz = size(aa)
if(sz[0] eq 3)then begin
window,/free,xsize=600,ysize=460,title=var2 
  view3d,var2,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, title2=title2,$
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
endif else begin

titlew = caseid
window,/free,xsize=600,ysize=400,title=titlew

  plot_map4,aa,bb,xx,yy,lev1j,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title2,xtitle='longitude',ytitle=ytitle
endelse
  print,''
  print,var, '  ',ytitle, ' iave=',iave

lev2=lev3

igif=0
if(igif)then mygif,'gifs_temp/'+gifname+'.gif'



end
