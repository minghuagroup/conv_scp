
 pro plot_2dhtml,a,x,y,levels,title=title,maxv=maxv,minv=minv

 if(not keyword_Set(maxv))then maxv = max(a)
 if(not keyword_Set(minv))then minv = min(a)
 if(not keyword_Set(title))then title='No Title'

 maxv=min([maxv,max(a)])
 minv=max([minv,min(a)])

 if(levels[0] gt minv)then begin
   j=min(where(levels ge minv))
   levels=levels[j:*]
 endif

 if(levels[n_elements(levels)-1] lt maxv)then begin
   j=max(where(levels lt maxv))
   levels=levels[0:j]
 endif


 nlv=n_elements(levels)
 c_labels=indgen(nlv)*0
 for i=0,nlv-1,2 do c_labels[i]=1

 loadct,13,ncolors=nlv
 
 c_colors=indgen(nlv)+1

 !P.multi=[0,0,2]

; print,!p.multi
;set_plot,'X'
window, xsize=640,ysize=540

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

 b=a

 if(not keyword_set(maxv))then maxv=max(a)
 if(not keyword_Set(minv))then minv=min(a)

 jj1=where(a gt maxv,count1)
 jj2=where(a lt minv,count2)
 if(count1 gt 0)then b(jj1)=maxv
 if(count2 gt 0)then b(jj2)=minv

 ytickv=indgen(9)*100-900.
 ytickn=strmid(strtrim(fix(ytickv),2),1,3)

;=============

 contour,a,x,y,xstyle=1,ystyle=1,levels=levels,c_labels=c_labels,$
  xtitle='Calendar Day',ytitle='Pressure (mb)',title=title,c_colors=c_colors,$
  yticks=8,ytickv=ytickv,ytickname=ytickn,xticklen=0.04,yminor=2,/fill

ix=1
;read,ix
!P.multi=[1,0,2]
 !p.position=[0.1,0.08,0.95,0.11]


 ny=n_elements(b(*,0))

 dc = fltarr(nlv,2)
 for i=0,1 do dc(*,i)=levels(*)
 xc = levels
 yc=[0,1]
 nlv2=nlv-1
 if(nlv gt 20)then nlv2=nlv/2

 contour,dc,xc,yc,levels=levels,xticks=nlv2,xstyle=1,yticks=1,yticklen=0,$
       c_colors=c_colors,/fill,ytickn=[' ',' '],ytickv=[0,1] 


  !p.multi=[0,1,1]
 !p.position=[0.1,0.1,0.95,0.95]


;read,ix2
; stop
end

