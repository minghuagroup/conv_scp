
print,' plot field or *cam.r.* tape after getting to cloudXX.nc with ncks extraction'
print,'data are directly assigned here from file specified in iap31.pro'

 dd1 = get_fld(file,'T3')
 var = 'T'    ;-------------------
 dd3 = reform( dd1[*,*,*,1])

 var = 'PS'    ;-------------------
 dd1 = get_fld(file,'PS')
 dd3 = reform( dd1[*,*,1])

 var = 'Q'    ;-------------------
 dd1 = get_fld(file,var)
 dd3 = reform( dd1[*,*,*,1])

 help,dd1
;======================????
; file='../obs/IAP/fout2/'+caseid+'.sav'

 print,'variable', var
 ;read,var
;====================??????
 restore,'lev.sav' ;xx,yy,zz

;3d array ;--------------
; restore,file
; help,xx,yy,dd3
; dd2 = dd3
 print,'min,max: ',min(dd3),max(dd3)
 
;if(belongsto(var,['CLDLIQ','CLDICE']))then begin
;  lev1j = cal_lev([0.,100],20)
;  scale = 1.e6
;endif



 get_lev,dd3,var,lev1j,scale
if(var eq 'PS')then lev1j = cal_lev([500.,1100],12)

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
