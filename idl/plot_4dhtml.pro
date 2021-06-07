

 pro plot_4dhtml,aa,bb,x,y,levelsco,levelsct,title=title,$
     maxv=maxv,minv=minv,line=line,xtitle=xtitle,ytitle=ytitle,$
     tran=tran,fac=fac,xx2=xx2,yy2=yy2, xlog=xlog,ylog=ylog ,$
        xrange=xrange,yrange=yrange,missing=missing, colorbar_nn = colorbar_nn

 if(not keyword_set(missing))then missing=-999.
 if(not keyword_set(xrange))then xrange=[min(x),max(x)]
 if(not keyword_set(yrange))then yrange=[min(y),max(y)]
 if(not keyword_set(xlog))then xlog=0
 if(not keyword_set(ylog))then ylog=0
 if(not keyword_Set(colorbar_nn))then colorbar_nn = 15; number of color bar ticks


; maxv=1 replace fill black
; line   to get lines
 a=aa
 b=bb

levels=levelsco
levels2=levelsct

 ; if set keyword maxv and minv, then making no white and black
 
 nj=n_elements(levels)
 dlev = levels[nj-1]-levels[nj-2]
 
 if(not keyword_set(xtitle))then xtitle='Calday'
 if(not keyword_set(ytitle))then ytitle='Pressure (mb)'
 if(keyword_set(tran))then begin
    a=transpose(a)
    b=transpose(b)
  endif
 if(keyword_set(fac))then a=a*fac
 if(not keyword_Set(xx2))then xx2=x
 if(not keyword_Set(yy2))then yy2=y



 if(keyword_set(maxv))then begin
    jj=where(a ge max(levels),cnt)
    if(cnt gt 0)then a[jj]=levels[nj-1]-dlev/1.0e6
 endif
 if(keyword_set(minv))then begin
    jj=where(a lt min(levels),cnt)
    if(cnt gt 0)then a[jj]=min(levels)
 endif
 

 nlv=n_elements(levels)
 c_labels=indgen(nlv)*0
 for i=0,nlv-1,2 do c_labels[i]=1

 restore,'/Users/minghuazhang/unix/idl/my_rain18.color'
 nm=n_elements(r2)

   r3 = congrid(r2[1:*],nlv-1,/interp,/minus_one) ; nlv intervals
   g3 = congrid(g2[1:*],nlv-1,/interp,/minus_one)
   b3 = congrid(b2[1:*],nlv-1,/interp,/minus_one)

 tvlct,[255,r3,0],[255,g3,0],[255,b3,0]   ; 0 white, nm black
 tvlct,255,255,255,254

 !P.BACKGROUND=254

r4=[255,r3,0]
 g4=[255,g3,0]
 b4=[255,b3,0] 
; restore,'/Users/minghuazhang/unix/idl/my_rain18.color'
 save,filename='/Users/minghuazhang/unix/idl/current.color',r4,g4,b4

 c_colors = indgen(nlv)+1
    ; 0 white, nm black
 
 !P.multi=[0,0,2]

; print,!p.multi
;set_plot,'X'
;window, xsize=640,ysize=540

; window,xsize=640,ysize=640

 x0=0.1
 y0=0.2
 x2=0.95
 y2=0.90
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


 if(not keyword_Set(ytitle))then begin

 ytickv=indgen(9)*100-900.
 ytickn=strmid(strtrim(fix(ytickv),2),1,3)

;=============
 !P.BACKGROUND = [255,255,255]

 contour,a,x,y,xstyle=1,ystyle=1,levels=levels,c_label=c_labels,$
  xtitle=xtitle,ytitle=ytitle,title=title,c_colors=c_colors,yrange=yrange,xrange=xrange,$
  yticks=8,ytickv=ytickv,ytickname=ytickn,xticklen=0.04,yminor=2,/fill,$
    color=nlv,background_color=[255,255,255],xlog=xlog,ylog=ylog


 endif else begin

; c_colors=c_colors[0:1]

 contour,a,x,y,xstyle=1,ystyle=1,levels=levels,c_label=c_labels,$
  xtitle=xtitle,ytitle=ytitle,title=title,c_colors=c_colors,xlog=xlog,ylog=ylog,$
  xticklen=0.04,yminor=2,/fill,color=nlv,xrange=xrange,yrange=yrange

 endelse

; if(keyword_Set(line))then begin
    c_c=levels2*0+nlv
    jj=where(levels2 lt 0,cnt)
    c_lines = levels2*0
    if(cnt gt 0)then c_lines[jj]=2
    c_labels = levels2*0+1    ;;; not intended
;stop
 contour,b,xx2,yy2,levels=levels2,c_label=c_labels, c_colors=c_c,/overplot,$
    c_lines=c_lines,/Follow,xrange=xrange,yrange=yrange,min_value=missing
 ;contour,b,xx2,yy2,levels=levels2, c_colors=c_c,/overplot,$
 ;   /Follow,xrange=xrange,yrange=yrange,min_value=missing



ix=1
;read,ix
!P.multi=[1,0,2]
 !p.position=[0.1,0.08,0.95,0.11]



 dc = fltarr(nlv,2)
 for i=0,1 do dc(*,i)=levels(*)
 xc = levels
 yc=[0,1]
 nlv2=nlv-1
 
; levels2=levels
; if(nlv gt 28)then begin
;    nlv2=nlv/2
;    levels2=fltarr(nlv2) 
;    for i=0,nlv2-1 do levels2[i]=levels[i*2]
;  levels2 = array_spaced(levels,2)
; endif

 ;color bar ending

 dc1 = levels[0]-dlev/5.
 dc2 = max(levels)+dlev/5.

 bartickinterval = ((max(xc) - min(xc))/colorbar_nn*1.0)


; if(min(a) lt min(levels))then begin
;     xc=[dc1,xc]
;     dc=[transpose([dc1,dc1]),dc]
; endif       
; if(max(a) gt max(levels))then begin
;     xc=[xc,dc2]
;     dc=[dc,transpose([dc2,dc2])]
; endif       
 contour,dc,xc,yc,levels=levels,xticks=nlv2,xstyle=1,yticks=1,yticklen=0,$
       c_colors=c_colors,/fill,ytickn=[' ',' '],ytickv=[0,1],color=nlv ,$
       ;xtickv = levels2,xrange=[min(xc),max(xc)]
       xrange=[min(xc),max(xc)], xtickinterval = bartickinterval

;stop

;  !p.multi=[0,1,1]
 !p.position=[0.1,0.1,0.95,0.95]

a=aa
;read,ix2
; stop
end

