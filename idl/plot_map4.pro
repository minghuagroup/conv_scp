

 pro plot_map4,aa,bb,xinp,yinp,levels,levels2,title=title,maxv=maxv,minv=minv,line=line,$
    xtitle=xtitle,ytitle=ytitle,tran=tran,fac=fac,xx2=xx2,yy2=yy2,$
    nxp=nxp,nxx0=nxx0,avex=avex,no_contour=no_contour, $
        xrange=xrange,yrange=yrange,xsize=xsize,ysize=ysize,$
        latdel = latdel,londel=londel,box=box,$
        mapcolor=mapcolor,mapfill=mapfill,isetz=isetz, colorbar_nn = ccolorbar_nn, $
        vect = vect,uvel=uvel,vvel=vvel,vlen = vlen,usteps=usteps,vsteps = vsteps,$
        vmissing=vmissing, vcolor=vcolor,sidebar = sidebar, noc_label=noc_label,charsize=charsize

; maxv=1 replace fill black
; line   to get lines
 a=aa
 x=xinp
 y=yinp
 if(not keyword_set(charsize))then charsize=1
 if(not keyword_set(xrange))then xrange=[min(x),max(x)]
 if(not keyword_set(yrange))then yrange=[min(y),max(y)]
 if(not keyword_set(latdel))then latdel = 30.
 if(not keyword_set(londel))then londel = 60.
 if(not keyword_set(box))then box=0
 if(not keyword_set(isetz))then isetz=0
 if(not keyword_set(colorbar_nn)) then colorbar_nn = 10
 if(not keyword_set(vect)) then begin
    uverl = 0
    vvel = 0
    vsteps = 0
    vlen = 0
    vmissing = 99999.
  endif



 if(not keyword_Set(xx2))then xx2=x
 if(not keyword_Set(yy2))then yy2=y


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
 
; Vectors
;-----------------------------
 if(keyword_set(vect))then begin
  if(not keyword_Set(vlen))then vlen =1.0
  if(not keyword_Set(vsteps))then vsteps=1
  if(not keyword_Set(usteps))then usteps=1

   uj   = array_spaced2(uvel, usteps, vsteps)
   vj   = array_spaced2(vvel, usteps, vsteps)
   xvec = array_spaced(xinp,usteps)
   yvec = array_spaced(yinp,vsteps)

 endif
; -----------------------------


 nlv=n_elements(levels)
 c_labels=indgen(nlv)*0
 for i=0,nlv-1,2 do c_labels[i]=1

f1color = '/Users/minghuazhang/unix/idl/my_rain18.color'
f2color = '~/idl/my_rain18.color'

if(file_exist(f1color))then restore, f1color else restore, f2color
;restore,'~/idl/my_rain18.color'
;stop
 nm=n_elements(r2)

   r3 = congrid(r2[1:*],nlv-1,/interp,/minus_one) ; nlv intervals
   g3 = congrid(g2[1:*],nlv-1,/interp,/minus_one)
   b3 = congrid(b2[1:*],nlv-1,/interp,/minus_one)

 tvlct,[255,r3,0],[255,g3,0],[255,b3,0]   ; 0 white, nm black


 r4=[255,r3,0]
 g4=[255,g3,0]
 b4=[255,b3,0] 
 save,filename='~/idl/current.color',r4,g4,b4
 ;!P.background=colors.white
 !P.background=-1

 c_colors = indgen(nlv)+1
    ; 0 white, nm black

; stop
if(keyword_set(sidebar))then begin
 !P.multi=[0,2,0]
endif else begin
 !P.multi=[0,0,2]
endelse 

if(keyword_set(xsize))then xsize=xsize else xsize = 640
if(keyword_set(ysize))then ysize=ysize else ysize = 420

;if(keyword_set(xsize))then xsize=xsize else xsize = 480
;if(keyword_set(ysize))then ysize=ysize else ysize = 360

;if(keyword_set(isetz))then begin
;; erase
; setz,xsize=xsize,ysize=ysize
;endif else begin
;window, /free,title=title,xsize=xsize,ysize=ysize
;endelse

sidebar_dx = 0.0
if(keyword_Set(sidebar))then sidebar_dx = 0.03

 x0=0.1 - sidebar_dx
 y0=0.2 
 x2=0.95 - 1.5*sidebar_dx
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
 
 colors=getcolor(/load)
 latcen = 0
 loncen = 180
 MAP_SET,  latcen,loncen,0,/ISOTROPIC, limit=limit,position=!P.position,$
       title=title,color=colors.black 

 if (keyword_Set(vect) and (vect gt 1))then begin
    cell_fill = 0 
    c_thick = 2
 endif else begin
    cell_fill = 1
    c_thick = 1
 endelse

 contour,a,x,y,xstyle=1,ystyle=1,levels=levels,c_label=c_labels,$
  xtitle='',ytitle=ytitle,title=title,c_colors=c_colors, c_thick=c_thick,$
  xticklen=0.04,yminor=2,cell_fill = cell_fill ,color=colors.black $ ;nlv $
 , xrange=xrange,yrange=yrange,/overp,/closed, charsize=charsize ;, $
 ;   xTICKFONT_NAME = 'times', TICKFONT_SIZE=8,TICKFONT_STYLE=1
 ;, xrange=xrange,yrange=yrange,/overp,/closed,font_name = 'times',font_size=8,font_type='bold'
; =========================
 
 if(keyword_Set(line))then begin
    c_c=levels*0+nlv
    ;;c_labels = levels*0    ;;; not intended
 contour,a,x,y,levels=levels,c_label=c_labels, c_colors=c_c,/overplot,/Follow,charsize=charsize
 end

 if ((not keyword_Set(vect)) or (vect eq 1))then begin

 if(not keyword_Set(no_contour))then begin

    c_c=levels2*0+nlv
    jj=where(levels2 lt 0,cnt)
    c_lines = levels2*0
;    if(cnt gt 0)then c_lines[jj]=2
    c_labels = levels2*0+1    ;;; not intended
; contour,bb,xx2,yy2,levels=levels2,c_label=c_labels, c_colors=c_c,/overplot ,$
;    c_lines=c_lines,/Follow

 nj = 1
 if(keyword_set(noc_label))then nj = 0
 contour,bb,xx2,yy2,levels=levels2,c_label=c_labels*nj, c_colors=c_c,/overplot ,$
    c_lines=c_lines,/Follow, c_thick=2, color=colors.black


;stop
 endif
 endif

 latlab = yrange[0]
 lonlab = xrange[0]
 digit=0

 mcolor=max(c_colors)  ;
 mfill = 0
 if(keyword_set(mapcolor))then mcolor=0 
 if(keyword_set(mapfill))then mfill=1

;= Vector ===================
 if(not keyword_set(vcolor))then vcolor = colors.black
 if(keyword_Set(vect))then begin
   if(not keyword_set(vmissing))then vmissing= 99990.
   velovect,uj,vj,xvec,yvec,length=vlen,/overplot,missing=vmissing, /dots, color=colors.black
 endif

  ;MAP_CONTINENTS ,color=mcolor,fill=mapfill
  MAP_CONTINENTS ,color=mcolor,fill=mapfill ;,USA=1
;stop

;;goto,jump11

EW = 1
if(EW)then begin
 xtickv = indgen(12)*30
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

endif else begin

 nticx = (xrange[1]-xrange[0])/londel+1
 xtickv = indgen(nticx)*londel+lonlab  ;map x coordinates
 xtickn=strtrim(fix(xtickv),2)
 lonnames = xtickn
 lons=xtickv

 nticy = (yrange[1]-yrange[0])/latdel+1
 ytickv = indgen(nticy)*latdel+latlab  ;map x coordinates
 ytickn=strtrim(fix(ytickv),2)
 latnames = ytickn
 lats = ytickv

endelse

; contour,bb,xx2,yy2,levels=levels2,c_orientation=[45,-45,0],/fill,/noerase

;; labels , size, and location of x and y titles
;  ====================================================

 xyouts,x0-0.05,0.5*(y0+y2),normal=1,ytitle,align=0.5,orien=90.,color=colors.black,charsize=0.9*charsize

; xyouts,0.5*(x0+x2),y0,xtitle,normal=1,align=0.5,color=colors.black,charsize=0.9*charsize
 xyouts,0.5*(x0+x2),y0-0.015,xtitle,normal=1,align=0.5,color=colors.black,charsize=0.9*charsize

  ni = n_elements(lonnames)
  nj = n_elements(latnames)

  MAP_GRID,box=box  ,$ 
      lons=lons,lonnames=   lonnames, $
      lats=lats,latnames= latnames 

    xj=convert_coord(lons,replicate(lats[0],ni),/data,/to_normal)
    yj=convert_coord(replicate(lons[0],nj),lats,/data,/to_normal)


;; labels , size, and location of x and y coordinates
;  ====================================================

    for i=0,ni-1 do xyouts,xj[0,i],xj[1,i]-0.04,lonnames[i],align=0.5,/normal,$
 color=colors.black, charsize = 0.8*charsize
    for i=0,nj-1 do xyouts,yj[0,i]-0.03,yj[1,i]-0.01,latnames[i],align=0.5,$
 /nor,color=colors.black,charsize=charsize*0.8


 axis,yaxis = 0,YTICKFORMAT="(A1)",ythick=2,yticks=nj-1,ytickname = strarr(nj),yminor=1,color=colors.black
 axis,yaxis = 1,YTICKFORMAT="(A1)",ythick=2,yticks=nj-1,ytickname = strarr(nj),yminor=1,color=colors.black
 axis,xaxis = 0,XTICKFORMAT="(A1)",xthick=2,xticks=ni,xtickname = strarr(ni+1),xminor=1,color=colors.black
 axis,xaxis = 1,XTICKFORMAT="(A1)",xthick=2,xticks=ni,xtickname = strarr(ni+1),xminor=1,color=colors.black

; oplot,[min(xj),max(xj)],[min(yj),min(yj)],thick = 3

ix=1
;read,ix
; color_bar position
; -------------------

if(not keyword_Set(sidebar))then begin
  !P.multi=[1,0,2]
  ;!p.position=[0.10,0.12,0.95,0.14]
  !p.position=[0.10,0.14,0.95,0.16] 
endif else begin
 sidebar_dx = 0.05
  !P.multi=[1,2,0]
  !p.position=[0.93,y0+0.075,0.945,y2-0.075]
  !p.position=[0.93,y0+0.075,0.945,y2-0.075]
endelse

 dc = fltarr(nlv,2)
 for i=0,1 do dc(*,i)=levels(*)
 xc = levels
 yc=[0,1]
 nlv2=nlv-1
 
; levels2=levels2
; if(nlv gt 28)then begin
;    nlv2=nlv/2
;    levels2=fltarr(nlv2) 
;    for i=0,nlv2-1 do levels2[i]=levels[i*2]
; endif

 ;color bar ending

 dc1 = levels[0]-dlev/5.
 dc2 = max(levels)+dlev/5.

; if(min(a) lt min(levels))then begin
;     xc=[dc1,xc]
;     dc=[transpose([dc1,dc1]),dc]
; endif       
; if(max(a) gt max(levels))then begin
;     xc=[xc,dc2]
;     dc=[dc,transpose([dc2,dc2])]
; endif       

 bartickinterval = (max(xc) - min(xc))/(colorbar_nn*1.0)
; print,'bartickinterval ',bartickinterval 
; bartickinterval = (levels[0] - levels[-1])/(colorbar_nn*1.)
; print,'bartickinterval ',bartickinterval 
;print,colorbar_nn
;print, levels


; print,'levels',levels
; print,'levels2',levels2
; print,'xc',xc
; print,'yc',yc
; print,'dc',size(dc),dc
; levels = levels - xc[0]
; levels2 = levels
; dc = dc - xc[0]
; xc = xc -xc[0]

;goto,jump_junk
 dc2 = dc
 xc2 = xc
 yc2 = yc
 xticks = nlv2
 yticks = 1
 xticklen = 1
 yticklen = 0
 ytickn = [' ',' ']
 ytickv = [0,1]
 xstyle = 1
 xrange = [min(xc),max(xc)]
 xtickinterval = bartickinterval
 xtickn =0
 xtickv = 0
 ytickinterval =0
 ystyle = 0
 yrange = [0,1]

 if(keyword_set(sidebar))then begin
  xc = yc2
  yc = xc2
  dc = transpose(dc2)
  xticklen = 0
  yticklen = 1
 xticks = 1
 yticks = nlv2
 xtickn = [' ',' ']
 ytickv = [0,1]
 ystyle = 4
 yrange = [min(yc),max(yc)]
 ytickinterval = bartickinterval
 ytickn =0
 ytickv = 0
 xtickinterval =0
 xstyle = 0
 xrange = [0,1]
 endif

jump_junk:
if(not keyword_Set(sidebar))then begin
contour,dc,xc,yc,levels=levels,xticks=nlv2,yticks=1,yticklen=0,$
       c_colors=c_colors,/fill,ytickn=[' ',' '],ytickv=[0,1],color=nlv ,$
       xrange = [min(xc),max(xc)],xstyle=1,  xtickinterval = bartickinterval,charsize=charsize*0.9
       ;xtickv = levels2,xrange = [min(xc),max(xc)],xstyle=1

endif else begin

;; dc = transpose(dc)
;; xc = [0,1]
;; yc = levels
 contour,dc,xc,yc,levels=levels,xticks=1,yticks=1,xticklen=0,yticklen=0,$
       c_colors=c_colors,xtickn=[' ',' '], ytickn=[' ',' '],xtickv=[0,1],color=nlv ,$
       ;yrange = [min(yc),max(yc)], ystyle=1, ytickinterval = bartickinterval,$
       yrange = [min(yc),max(yc)], ystyle=1,$ 
       charsize=charsize*0.9, /fill, cell_fill = 1,/follow
 !Y.tickname=''
 axis,yaxis = 0,YTICKFORMAT="(A1)"

; axis,yaxis = 0,YTICKFORMAT="(A1)",color=colors.black
 axis,yaxis = 1, yticks=nlev2,yticklen=yticklen,yrange = [min(yc),max(yc)], ystyle=1, $
      ytickinterval = ytickinterval , charsize=charsize*0.9, color=colors.black
; axis,yaxis = 0,YTICKFORMAT="(A1)",ythick=4,yticks=nj-1,ytickname = strarr(nj),yminor=1,color=colors.black

 endelse

 !p.multi=[0,1,1]
 !p.position=[0.1,0.1,0.95,0.95]

a=aa
;read,ix2
end

