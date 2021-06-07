

 pro plot_map_uv,aa,xinp,yinp,levels,title=title,maxv=maxv,minv=minv,line=line,$
    xtitle=xtitle,ytitle=ytitle,tran=tran,fac=fac,$
    nxp=nxp,nxx0=nxx0,avex=avex,$
        xrange=xrange,yrange=yrange,xsize=xsize,ysize=ysize,$
        latdel = latdel,londel=londel,box=box,$
        mapcolor=mapcolor,mapfill=mapfill , $
        contour=contour, vect = vect,uvel=uvel,vvel=vvel,vlen = vlen,vsteps = vsteps,vmissing=vmissing
; contour = 2 no contour

; maxv=1 replace fill black
; line   to get lines
 a=aa
 x=xinp
 y=yinp
 if(not keyword_set(xrange))then xrange=[min(x),max(x)]
 if(not keyword_set(yrange))then yrange=[min(y),max(y)]
 if(not keyword_set(latdel))then lat_del = 30.
 if(not keyword_set(londel))then lon_del = 60.
 if(not keyword_set(box))then box=0
 if(not keyword_set(contour))then contour=1


 ii=where((xinp ge min(xrange)) and (xinp le max(xrange)),cnt1)
 if(min(ii) gt 0)then ii=[min(ii)-1,ii]
 if(max(ii) lt n_elements(xinp) )then ii=[ii,max(ii)+1]

 jj=where((yinp ge min(yrange)) and (yinp le max(yrange)),cnt2)
 if(min(jj) gt 0)then jj=[min(jj)-1,jj]
 if(max(jj) lt n_elements(xinp) )then jj=[jj,max(jj)+1]

 if(max(ii) lt n_elements(xinp) )then ii=[ii,max(ii)+1]
 if(cnt1 eq 0 or cnt2 eq 0)then begin
  print,'xrange and yrange exceed xinp and yinp'
  stop
 endif

 x = x[ii] & y=y[jj]
 a = a[ii,*] & a=a[*,jj]
 if(keyword_Set(vect))then begin
  uu1 = uvel[ii,*] & uu1 = uu1[*,jj]
  vv1 = vvel[ii,*] & vv1 = vv1[*,jj]
 endif

 if(not keyword_set(xtitle))then xtitle='Longitude'
 if(not keyword_set(ytitle))then ytitle='Latitude'
 if(keyword_set(tran))then a=transpose(a)
 if(keyword_set(fac))then a=a*fac
 if(keyword_set(nxp))then begin
   if(keyword_set(nxx0))then begin 
    nxp2=nxx0+nxp-1
    a=a[nxx0:nxp2,*]
    x=x[nxx0:nxp2]
   endif else begin
    a=a[0:nxp-1,*]
    x=x[0:nxp-1]
   endelse
 endif

 if(keyword_set(avex))then begin
  print,'y'
   a=diur_xave2(a,avex)
   x=diur_ave(x,avex)
 endif
 
 ; if set keyword maxv and minv, then making no white and black
 
 nj=n_elements(levels)
 dlev = levels[nj-1]-levels[nj-2]
 
 if(keyword_set(maxv))then begin
    jj=where(a ge max(levels),cnt)
    if(cnt gt 0)then a[jj]=levels[nj-1]-dlev/1.0e6
 endif
 if(keyword_set(minv))then begin
    jj=where(a lt min(levels),cnt)
    if(cnt gt 0)then a[jj]=min(levels)
 endif
 
;-----------------------------
 if(keyword_set(vect))then begin
  if(not keyword_Set(vlen))then vlen =1.0
  if(not keyword_Set(vsteps))then vsteps=1
  if(not keyword_Set(uvel))then begin
      print,'u and v must be specified if vect is set!! '
      stop  
   endif
    xi = indgen(1000)*vsteps 
    ij = where( (xi le n_elements(x) ) and (xi le n_elements(uvel[*,0])) )
     xij = xi[ij]
    xvec = x[xij]  & uj = uu1[xij,*] & vj = vv1[xij,*]
    ij = where( (xi le n_elements(y) ) and (xi le n_elements(uvel[0,*])) )
     yij = xi[ij]
    yvec = y[yij]  & uj = uj[*,yij] & vj = vj[*,yij]
    ; data sampled to uj,vj,xvec,yvec
 endif

 nlv=n_elements(levels)
 c_labels=indgen(nlv)*0
 for i=0,nlv-1,2 do c_labels[i]=1

 restore,'~mzhang/idl/my_rain18.color'
 nm=n_elements(r2)

   r3 = congrid(r2[1:*],nlv-1,/interp,/minus_one) ; nlv intervals
   g3 = congrid(g2[1:*],nlv-1,/interp,/minus_one)
   b3 = congrid(b2[1:*],nlv-1,/interp,/minus_one)

 tvlct,[255,r3,0],[255,g3,0],[255,b3,0]   ; 0 white, nm black


 r4=[255,r3,0]
 g4=[255,g3,0]
 b4=[255,b3,0] 
 save,filename='~mzhang/idl/current.color',r4,g4,b4
 ;!P.background=colors.white
 !P.background=-1

 c_colors = indgen(nlv)+1
    ; 0 white, nm black

; stop
 
 !P.multi=[0,0,2]

; print,!p.multi
;set_plot,'X'

if(keyword_set(xsize))then xsize=xsize else xsize = 640
if(keyword_set(ysize))then ysize=ysize else ysize = 420
window, /free,title=title,xsize=xsize,ysize=ysize

 x0=0.1
 y0=0.2
 x2=0.95
 y2=0.90

 y0=0.23 - 0.04
 y2=0.97 - 0.04

 !p.position=[x0,y0,x2,y2]

; print,!p.position
 px=(!x.window) * (!d.x_vsize)
 py=(!y.window) * (!d.y_vsize)
 px=[x0,x2] * (!d.x_vsize)
 py=[y0,y2] * (!d.y_vsize)


 sx = px(1)-px(0)+1
 sy = py(1)-py(0)+1
 sz = size(a)

;  print,!x.window
;  print,!y.window
;  print,!d.x_vsize
;  print,!d.y_vsize
;  print,px
;  print,py
;  print,sx,sy
 erase


 ntic = 9  ;200 mb  9:100
 yticks=ntic
 ytickv=indgen(ntic)*100-900.
 ytickn=strmid(strtrim(fix(ytickv),2),1,3)

 xrangemap = xrange 
 yrangemap = yrange 
 jj=where(xrange gt 180.,cnt)
 if(cnt eq 2)then xrangemap2 = xrange-360.
 if(cnt eq 1)then xrangemap2 = [-180.,xrange-360.]
 xrangemap2 = [min(xrangemap),max(xrangemap)]
 limit = [yrange[0],xrangemap2[0],yrange[1],xrangemap2[1] ]
 
 latcen = 0
 loncen = 180
 MAP_SET,  latcen,loncen,0,/ISOTROPIC, limit=limit,position=!P.position,$
       title=title


 if(contour ne 2)then begin

 contour,a,x,y,xstyle=1,ystyle=1,levels=levels,c_label=c_labels,$
  xtitle=xtitle,ytitle=ytitle,title=title,c_colors=c_colors,$
  xticklen=0.04,yminor=2,/cell_fill,color=nlv $
 , xrange=xrange,yrange=yrange,/overp,/closed
 
 if(keyword_Set(line))then begin
    c_c=levels*0+nlv
    c_labels = levels*0    ;;; not intended
 contour,a,x,y,levels=levels,c_label=c_labels, c_colors=c_c,/overplot,/Follow
 endif
 endif

; stop

 if(keyword_Set(vect))then begin
   if(not keyword_set(vmissing))then vmissing= 99999.
   velovect,uj,vj,xvec,yvec,length=vlen,/overplot,missing=vmissing
 endif 
;-----------------------------------------------------
;stop

 latlab = yrange[0]
 lonlab = xrange[0]
 digit=0

 mcolor=max(c_colors)  ;
 mfill = 0
 if(keyword_set(mapcolor))then mcolor=0 
 if(keyword_set(mapfill))then mfill=1

  MAP_CONTINENTS ,color=mcolor,fill=mapfill
;stop

 xtickv = (indgen(60)-20)*londel+lonlab  ;map x coordinates
 jj=where(xtickv ge xrangemap2[0] and xtickv le xrangemap2[1],cnt)
 lons     = xtickv[jj]
 lonnames = strdigit(abs(lons),digit)
  jj2=where(lons lt 180.0 and lons gt 0.0,cnt2) 
       if(cnt2 gt 0)then lonnames[jj2] = lonnames[jj2]+'E' 
  jj2=where(lons lt 0.0,cnt2)
       if(cnt2 gt 0)then lonnames[jj2] = lonnames[jj2]+'W' 
  jj2=where(lons gt 180.0 ,cnt2)
    if(cnt2 gt 0)then lonnames[jj2] = strdigit(abs(360-lons[jj2]),digit)+'W'
  jj2=where(lons eq 360 ,cnt2)
    if(cnt2 gt 0)then lonnames[jj2] = strdigit(abs(360-lons[jj2]),digit)
  
 ytickv = indgen(60-20)*latdel+latlab
 jj=where(ytickv ge yrangemap[0] and ytickv le yrangemap[1],cnt)
 ytickv = ytickv[jj]
 ytickn = strtrim(strdigit(abs(ytickv),digit),2)
  jj1=where(ytickv lt 0.0 ,cnt1)
  jj2=where(ytickv gt 0.0,cnt2)
  if(cnt1 gt 0) then ytickn [jj1] = ytickn[jj1]+'S'
  if(cnt2 gt 0) then ytickn [jj2] = ytickn[jj2]+'N'
 lats     = ytickv[jj]
 latnames = ytickn

 ;xyouts,mean(xrange),yrange[0]-latdel/5.,xtitle,align=0.5,charsize=2
 ;xyouts,xrange[0]-londel/5.,mean(yrange),ytitle,align=0.5,orientation=90.,charsize=2
 xyouts,x0-0.06,0.5*(y0+y2),normal=1,ytitle,align=0.5,orien=90.
 xyouts,0.5*(x0+x2),y0-0.06,xtitle,normal=1,align=0.5

  ni = n_elements(lonnames)
  nj = n_elements(latnames)

  MAP_GRID,box=box,$ 
      lons=lons,lonnames=  strarr(ni), $ ;lonnames,$
      lats=lats,latnames=  strarr(nj) ;latnames 

    xj=convert_coord(lons,replicate(lats[0],ni),/data,/to_normal)
    yj=convert_coord(replicate(lons[0],nj),lats,/data,/to_normal)
    for i=0,ni-1 do xyouts,xj[0,i],xj[1,i]-0.04,lonnames[i],align=0.5,/normal
    for i=0,nj-1 do xyouts,yj[0,i]-0.04,yj[1,i]-0.01,latnames[i],align=0.5,/nor

;stop
ix=1
;read,ix
!P.multi=[1,0,2]
 !p.position=[0.1,0.08,0.95,0.11]



 dc = fltarr(nlv,2)
 for i=0,1 do dc(*,i)=levels(*)
 xc = levels
 yc=[0,1]
 nlv2=nlv-1
 
 levels2=levels
 if(nlv gt 28)then begin
    nlv2=nlv/2
    levels2=fltarr(nlv2) 
    for i=0,nlv2-1 do levels2[i]=levels[i*2]
 endif

 ;color bar ending

 dc1 = levels[0]-dlev/5.
 dc2 = max(levels)+dlev/5.

 if(min(a) lt min(levels))then begin
     xc=[dc1,xc]
     dc=[transpose([dc1,dc1]),dc]
 endif       
 if(max(a) gt max(levels))then begin
     xc=[xc,dc2]
     dc=[dc,transpose([dc2,dc2])]
 endif       

if(contour ne 2)then begin

 contour,dc,xc,yc,levels=levels,xticks=nlv2,xstyle=1,yticks=1,yticklen=0,$
       c_colors=c_colors,/fill,ytickn=[' ',' '],ytickv=[0,1],color=nlv ,$
       xtickv = levels2
endif

  !p.multi=[0,1,1]
 !p.position=[0.1,0.1,0.95,0.95]


a=aa
;read,ix2
end

