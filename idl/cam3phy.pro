print, 'plot text file to run iap30 first for files prepare on storm by proc_phy3.pro in fout2 folder'
print, 'run iap30 first to determin plot types for MEAN input file'


 nstep = 11486
 var = 'Q'    ;-------------------
 caseid='q_'+strdigit(nstep,0)

 var = 'PS'    ;-------------------
 caseid='PS_'+strdigit(nstep,0)

 var = 'T'    ;-------------------
 caseid='T_'+strdigit(nstep,0)

;======================????
 file='../obs/IAP/fout2/'+caseid+'.sav'

 print,'variable?'
 ;read,var
;====================??????

;3d array ;--------------
 restore,'lev.sav'
 zz = lev

 restore,file
 ;================<<<<<<<<<<
 help,xx,yy,dd3
 dd2 = dd3
 print,'min,max: ',min(dd3),max(dd3)
 
;if(belongsto(var,['CLDLIQ','CLDICE']))then begin
;  lev1j = cal_lev([0.,100],20)
;  scale = 1.e6
;endif



 get_lev,dd3,var,lev1j,scale
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

if(igif)then mygif,'gifs_temp/'+gifname+'.gif'



end
