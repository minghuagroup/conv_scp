pro colorskew,comment

; Color table: 0=black, 1=dk blue 2=med blue 3=cyan 4=green 5=yellow
; 6=orange 7=red 8=grey 9=white
; Don't know why all this works, but it is necessary...if you want to write
; gifs.


 common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

   r=bytscl([0,  0,  0,  0,  0,255,255,255,127,255])
   g=bytscl([0,  0,119,255,255,255,119,  0,127,255])
   b=bytscl([0,255,255,255,  0,  0,  0,  0,127,255])
   r=bytscl([255,  0,  0,  0,000,255,255,255,127,0])
   g=bytscl([255,255,119,  0,175,255,119,  0,127,0])
   b=bytscl([255,255,255,200,  0,  0,  0,  0,127,0])
   !p.color=9
   !p.background=0
   r_orig=r
   g_orig=g
   b_orig=b
   tvlct,r_orig,g_orig,b_orig
   r_curr= r_orig
   g_curr= g_orig
   b_curr= b_orig


;   Generic version, just plots the skew-t base, no data

;!p.title=comment
!x.margin=[2,11]
ptop=100.0
pbot=1050.0
tleft=254.15
tright=305.15

a=fltarr(100)
b=fltarr(100)
temp=fltarr(100)
pres=fltarr(100)
lab=strarr(30)
tmoist=fltarr(15)
pmoist=1050.0

R=287.0
K=-33.0
cp=1005.7
kappa=R/cp
bbot=-R*alog(pbot)
btop=-R*alog(ptop)
aleft=tleft+(K*alog(pbot))
aright=tright+(K*alog(pbot))
theta=tright+90.0
p0=1000.0

;print,aleft,aright,btop,bbot

; Draw isotherms.

tstart=tleft-80.0
temp(0)=tstart
a(0)=temp(0)+K*alog(pbot)
b(0)=bbot 
const=a(0)-(-K/R)*b(0)
for ii=0,29 do lab(ii)=' '
for ii=1,99 do begin
  b(ii)=b(ii-1)+1.0
  a(ii)=(-K/R)*b(ii) + const
endfor

case 1 of
(tstart-273.15 mod 5 eq 0) : $
plot,a,b,/xstyle,/ystyle,xrange=[aleft,aright],yrange=[bbot,btop], $
     xminor=0,yminor=0,xticklen=0.0001,yticklen=0.0001,xtickname=lab, $
     ytickname=lab,thick=1.0,title=comment,charsize=1.3
(tstart-273.15 mod 5 ne 0): $
plot,a,b,/xstyle,/ystyle,xrange=[aleft,aright],yrange=[bbot,btop], $
     xminor=0,yminor=0,xticklen=0.0001,yticklen=0.0001,xtickname=lab, $
     ytickname=lab,thick=0.1,title=comment,charsize=1.3,charthick=2.0
endcase

bline=-R*alog(825.0)
bline2=-R*alog(175.0)
for kk=0,129 do begin
 tstart=tstart+1.0
 temp(0)=tstart
 a(0)=temp(0)+K*alog(pbot)
 b(0)=bbot
 const=a(0)-(-K/R)*b(0)
 for ii=1,99 do begin
  b(ii)=b(ii-1)+7.0
  a(ii)=(-K/R)*b(ii) + const
 endfor
 tmp=tstart-273.15
case 1 of
(tmp mod 10 eq 0) : begin
 oplot,a,b,thick=1.0,color=7
   stmp=string(fix(tmp))
   stmp=strtrim(stmp,2)
   stmp=strmid(stmp,0,3)
   aa=tstart+K*alog(825.0)
   aa2=tstart+K*alog(175.0)
   if aa gt aleft and aa lt aright then $
   xyouts,align=0.5,aa,bline,stmp,charsize=1.2,charthick=1.5,color=7
   if aa2 gt aleft and aa2 lt aright then $
   xyouts,align=0.5,aa2,bline2,stmp,charsize=1.2,charthick=1.5,color=7
  end
(tmp mod 10 ne 0):  
;oplot,a,b,thick=0
endcase
endfor

; Draw pressure lines 

pline=[950,900,850,800,750,700,650,600,550,500,450,400,350,300,250,200, $
   150,100]

  bline=-R*alog(1000)
  plots,aleft,bline
  plots,aright,bline,/continue
  xyouts,aleft+1.0,bline,'1000',charsize=1.3 

for ii=0,17 do begin
  bline=-R*alog(pline(ii))
  spline=string(pline(ii))
  spline=strtrim(spline,1)
  spline=strmid(spline,0,3)
  plots,aleft,bline
  plots,aright,bline,/continue
  xyouts,aleft+1.0,bline, spline,charsize=1.3
endfor

; Draw dry adiabats

for kk=1,60 do begin
  theta=theta-10.0
  for ii=0,99 do pres(ii)=1050.0 - ii*10.0
  for ii=0,99 do begin
    temp(ii)=theta*((pres(ii)/p0)^kappa)
    a(ii)=temp(ii)+(K*alog(pres(ii)))
    b(ii)=-R*alog(pres(ii))
  endfor
 oplot,a,b,color=9
endfor

; Draw moist adiabats

; First specify initial temperature and pressure

tmoist=[301.15,299.15,297.15,295.15,293.15,291.15,289.15,287.15,285.15 , $
   281.15,277.15,273.15,269.15,265.15,261.15]
pmoist=1000.0
L=2501000.0

for kk=0,14 do begin
  pcheck=100.0
  mm=0

; Calculate theta-e

  theta=tmoist(kk)*(p0/pmoist)^kappa
  tt=tmoist(kk)-273.15
  es=6.112*exp((17.67*tt)/(tt+243.5))
  ws=0.622*(es/(pmoist-es))
  thetae=theta*exp((2675.0*ws)/tmoist(kk))
  temp(0)=tmoist(kk)
  pres(0)=pmoist
  a(0)=temp(0)+(K*alog(pres(0)))
  b(0)=-R*alog(pres(0))

  for jj=1,99 do begin
    pcheck=100.0
    temp(jj)=temp(jj-1)-1.5
    ptemp=pres(jj-1)
    while pcheck gt 0.5 or pcheck lt -0.5 do begin
      tt=temp(jj)-273.15
      es=6.112*exp((17.67*tt)/(tt+243.5))
      ws=0.622*(es/(ptemp-es))
      theta=thetae/exp((2675.0*ws)/temp(jj))
      pres(jj)=((temp(jj)/theta)^(1/kappa))*p0
      pcheck=pres(jj)-ptemp
      ptemp=pres(jj)
    endwhile
     a(jj)=temp(jj)+(K*alog(pres(jj)))
     b(jj)=-R*alog(pres(jj))

    if pres(jj) lt 200.0 then begin
     mm=jj
     a(jj)=temp(jj)+(K*alog(pres(jj)))
     b(jj)=-R*alog(pres(jj))
     jj=99
    endif

  endfor
; print,a,b
; mm=99
  oplot,a(0:mm),b(0:mm),linestyle=3,color=3,thick=1.0
  tmc=tmoist(kk)-273.15
  stmc=strtrim(string(fix(tmc)),2)
  if a(mm) gt aleft and a(mm) lt aright then $
    xyouts,a(mm),b(mm),stmc,color=3
 
endfor

; Draw constant mixing ratio lines

mixrat=[.009,.010,.012,.014,.016,.018,.020,.024,.008,.007,.006,.005,.004, $
     .003,.0025,.002,.0015,.001]
tmrat=[285.95,287.45,290.35,292.65,294.75,296.65,298.35,301.35,284.15,282.1, $
      280.0,277.25,274.15,270.50,268.15,265.15,261.50,256.15]
for ii=0,17 do begin
  temp(0)=tmrat(ii)
  tt=temp(0)-273.15
    es=6.112*exp((17.67*tt)/(tt+243.5))
    pres(0)=(.622*es)/mixrat(ii) +es
  a(0)=temp(0)+(K*alog(pres(0)))
  b(0)=-R*alog(pres(0))
  for jj=1,99 do begin
    temp(jj)=tmrat(ii)-jj*0.5
    tt=temp(jj)-273.15
    es=6.112*exp((17.67*tt)/(tt+243.5))
    pres(jj)=(0.622*es)/mixrat(ii) +es
    a(jj)=temp(jj)+(K*alog(pres(jj)))
    b(jj)=-R*alog(pres(jj))
    if pres(jj) le 500. then begin
      ixx=jj
      jj=99
    endif
  endfor
  oplot,a(0:ixx),b(0:ixx),linestyle=2,color=4
  case 1 of
   (mixrat(ii)*1000. ge 3.): begin
       smixrat=strtrim(string(fix(mixrat(ii)*1000.)),2)
    end
   (mixrat(ii)*1000. lt 3.): begin
       smixrat=strtrim(string(mixrat(ii)*1000.),2)
       smixrat=strmid(smixrat,0,3)
    end
  endcase

  if a(0) gt aleft and a(0) lt aright then $
  xyouts,a(0),b(0),align=0.5,smixrat,color=4,charsize=1.2
endfor

end
