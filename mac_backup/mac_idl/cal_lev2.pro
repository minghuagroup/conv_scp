function cal_lev2,lw,n
 lev = fltarr(n)
 jj=where(lw gt -900.)
 lw2=lw[jj]
 m1=min(lw2)*1.0
 m2=max(lw2)*1.0
 int=(m2-m1)/n

 lev=[indgen(n)*int+m1,m2]

; print,m1,m2,int,n
; print,lev
 return,lev
 end


