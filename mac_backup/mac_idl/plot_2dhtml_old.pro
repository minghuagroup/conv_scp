
 pro plot_2dhtml,a,x,y,levels,title=title,maxv=maxv,minv=minv


 nlv=n_elements(levels)
 c_labels=indgen(nlv)*0
 for i=0,nlv-1,2 do c_labels[i]=1

 
 c_colors=indgen(nlv)*0+200
 c_colors[nlv/2:*]=20

 !P.multi=[0,0,2]

 print,!p.multi
;set_plot,'X'
window, xsize=640,ysize=540

; window,xsize=640,ysize=640

 x0=0.1
 y0=0.2
 x2=0.95
 y2=0.90
 !p.position=[x0,y0,x2,y2]

 print,!p.position
 px=(!x.window) * (!d.x_vsize)
 py=(!y.window) * (!d.y_vsize)
 px=[x0,x2] * (!d.x_vsize)
 py=[y0,y2] * (!d.y_vsize)


 sx = px(1)-px(0)+1
 sy = py(1)-py(0)+1
 sz = size(a)

  print,!x.window
  print,!y.window
  print,!d.x_vsize
  print,!d.y_vsize
  print,px
  print,py
  print,sx,sy
 erase

 b=a

 if(not keyword_set(maxv))then maxv=max(a)
 if(not keyword_Set(minv))then minv=min(a)

 jj1=where(a gt maxv,count1)
 jj2=where(a lt minv,count2)
 if(count1 gt 0)then b(jj1)=maxv
 if(count2 gt 0)then b(jj2)=minv

 b2=[[b],[b*0+minv]]
 ny=n_elements(b2[0,*])
 b2[*,ny-1]=maxv

 b3=bytscl(b2)

 b3=b3[*,0:(ny/2-1)]


 ;!p.background=100
 ;print,!p.background

 dj2 = poly_2d(b3,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy ],[0,0]],0,sx, sy)
 tv,dj2,px(0),py(0)

 ix=0
 ;read,ix
 ytickv=indgen(9)*100-900.
 ytickn=strmid(strtrim(fix(ytickv),2),1,3)

;=============
; loadct,13,ncolors=n_elements(levels)
; c_colors=indgen(n_elements(levels))+1
;=============


; contour,a,x,y,/noerase,xstyle=1,ystyle=1,levels=levels,c_labels=c_labels,$
;  xtitle='Calendar Day',ytitle='Pressure (mb)',title=title,c_colors=c_colors,$
;  yticks=8,ytickv=ytickv,ytickname=ytickn,xticklen=0.04,yminor=2
; xyouts,0.03,0.45,'Pressure (mb)',color=255,/normal,orientation=90

 contour,a,x,y,xstyle=1,ystyle=1,levels=levels,c_labels=c_labels,$
  xtitle='Calendar Day',ytitle='Pressure (mb)',title=title,c_colors=c_colors,$
  yticks=8,ytickv=ytickv,ytickname=ytickn,xticklen=0.04,yminor=2
; xyouts,0.03,0.45,'Pressure (mb)',color=255,/normal,orientation=90

ix=1
;read,ix
!P.multi=[1,0,2]
 !p.position=[0.1,0.08,0.95,0.11]


 ny=n_elements(dj2(*,0))
 max2=max(dj2)
 min2=min(dj2)

 dyy=(max2-min2)/float(ny-1)
 cj1=indgen(ny)*dyy+min2
 cj2=[[cj1],[cj1],[cj1],[cj1],[cj1],[cj1]]
 cbyt=byte(cj2)



 tv,cbyt,px(0),60


 ;use max(b) and min(b) to lable
 jj=where(levels lt max(b) and levels gt min(b),count)
 levels2=levels
 if(count gt 0)then begin
  levels2=levels[jj]
 endif
 nl2=n_elements(levels2)

 nint=1
 if(max(abs(b)) le 0.01 or max(abs(b)) ge 100.)then nint=2
 for i=0,nl2-1,nint do begin
  xxp = px(0)+ny*(levels2[i]-min(b))/(max(b)-min(b))
  xyouts,xxp,57,/device,'|',color=255
 junka= strtrim(levels2[i],2)
 jc=junka
  for k=strlen(junka)-1,0,-1 do begin

    if(strmid(junka,k,1) ne '0' and strpos(junka,'.') gt 0)then begin
     jc=strmid(junka,0,k+1)
     goto,jump2
    endif
  endfor
  jump2:

  xyouts,xxp,46,/device, jc,align=0.5,color=255
 endfor

 ;read,ix

;  colorbar,cj2,!p.position,levels,unit='',format='(f5.1)'

  !p.multi=[0,1,1]
 !p.position=[0.1,0.1,0.95,0.95]


;read,ix2
; stop
end

