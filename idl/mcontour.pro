pro mcontour,f,over=over,color=color,title=title,nlines=nlines,x=x,y=y
;================


m=n_elements(f[*,0])
n=n_elements(f[0,*])

if(not keyword_Set(nlines))then nlines=5
if(not keyword_Set(x))then x=indgen(m)
if(not keyword_Set(y))then y=indgen(n)

b=f[1:m-2,1:n-2]
kk=indgen(nlines)+1
dx = (max(b)-min(b))/(nlines+2)

cx = min(b)+indgen(nlines)*dx+dx
c_labels=indgen(nlines)*0+1

kk1=kk*0


if(not keyword_set(color))then color=7
if(not keyword_set(color))then title=''

jj=where(cx lt 0.0,count)
if(count gt 0)then kk1[jj]=1

if(not keyword_set(over))then begin

contour,f,x,y,levels=cx,c_labels=c_labels,title=title,xrange=[min(x),max(x)],xstyle=1,yrange=[min(y),max(y)],ystyle=1,color=color,c_linestyle=kk1

endif else begin
contour,f,x,y,levels=cx,c_labels=c_labels, color=color,c_linestyle=kk1,/overplot
endelse
end