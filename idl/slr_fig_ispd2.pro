
Dir0 = '../W/sav/'
Dir0 = '../cmip6/SLR/sav0/'
years  = 1950 + indgen(66) ; 1950-2015
years  = strtrim(years,2)
nt = n_elements(years)
nt2= n_elements(years)*12

z0 = 3000.
goto,jump_WSH
goto,jump_SH
goto,jump_NH
goto,jump_Tibet2
;goto,jump_Tibet
;goto,jump_WNH


years0 = years
months = strmid(strtrim(101+indgen(12),2),1,2)

hh = indgen(12)*500. +500
hh = [-500.,1., 50., hh]
hh = shift(hh,-2)

nh = n_elements(hh)

nx = 360
ny = 180
nn = 4
nx2= nx*nn
ny2= ny*nn
xx = (indgen(nx2)+0.5)/nn
yy = (indgen(ny2)+0.5)/nn
xx2 = replicate2(xx,ny2)
yy2 = replicate2(yy,nx2) & yy2 = transpose(yy2)
yy2 = yy2 - 90.

d3 = fltarr(nx2,ny2, nt2)
z3 = d3
c3 = d3

d1 = fltarr(nx2,ny2,12)
z1 = d1
c1 = d1

d2 = d1
z2 = d2
c2 = d2

; 1 stitch up
goto, jump_stitch_up

;=============================================
;for iy =0,nt-1 do begin
for iy =0,nt-1 do begin
 print,years[iy]
 filesav = dir0 + 'ispd_'+years0[iy]+'.sav'
 restore, filesav ;,years,d1,z1,c1  ; [720,360,12]
 if(iy eq 0)then begin
    d3 = d1 
    z3 = z1 
    c3 = c1 
 endif else begin
    d3 = [[[d3]],[[d1]]]  
    z3 = [[[z3]],[[z1]]]  
    c3 = [[[c3]],[[c1]]]  
 endelse
endfor

jump1:

; form stations
z4 = ave30(z3,missing=0.) 
jjj = where(z4 gt 2000, cnt) & print,cnt/12./nt
jjj = long(jjj)

ns = n_elements(jjj)
ps = fltarr(ns,nt2)
zs = ps
lat = ps
lon = ps

 for iy = nt2-1,0,-1 do begin
  iy0 = iy/12
  dj = reform(d3[*,*,iy])
  ps[*,iy] = dj[jjj]

  dj = reform(z3[*,*,iy])
  zs[*,iy] = dj[jjj]

  dj = xx2[jjj]
  dj = dj-180.
  jk = where(dj lt 0,cnt)
  if(cnt gt 0)then dj[jk] = 360.+dj[jk]
  lon[*,iy] = dj

  lat[*,iy] = yy2[jjj]
  jj = where(zs[*,iy] ge 2000.,cnt) & print,years[iy0],cnt

 endfor

 filesav = dir0 + '../sav0/ispd_stations.sav'
 save, file=filesav , ps,zs, lat, lon, nt,nt2
 print,'file saved ...', filesav

stop
jump_stitch_up:

jump2:

filesav = dir0 + '../sav0/ispd_stations.sav'
restore,filesav

; limit the stations with data to 20 yrs and z0

 ns = n_elements(ps[*,0])
 ps2 = ps
 zs2 = zs
 lon2 = lon
 lat2 = lat
 is2 = 0
 for is = 0,ns-1 do begin
  dj = reform(zs[is,*])  ; all year of a fixed station
  jj = where(dj ge z0, cnt)
  if(cnt gt 20*12)then begin  ;  for stations with more than 20 yrs and above 3000m
   print,is, cnt
    ps2[is2,*] = ps[is,*]
    zs2[is2,*] = zs[is,*]
    lon2[is2,*] = lon[is,*]
    lat2[is2,*] = lat[is,*]
    is2 = is2+1
  endif
 endfor
 ns = is2
 ps2 = ps2[0:ns-1,*]
 zs2 = zs2[0:ns-1,*]
 lon2 = lon2[0:ns-1,*]
 lat2 = lat2[0:ns-1,*]

 filesav = dir0 + '../sav0/ispd_stations2.sav'
 save, file=filesav , ps2,zs2, lat2, lon2
 print,'file saved ...', filesav

stop
jump_tibet:
jump3:
 filesav = dir0 + '../sav0/ispd_stations2.sav'
 restore, filesav ;, ps2,zs2, lat2, lon2

; plot and inspect

 win
 ic = 0
 is = 0
 pj  = ps2
 nt  = n_elements(ps2[0,*])
 ns  = n_elements(ps2[*,0])
 xj = indgen(nt)/12.0

 dd = ps*0.0
 dd_lat = ps*0.0
 dd_lon = ps*0.0
 dd_zs = ps*0.0

 win

 is2=0
 for is = 0, ns-1 do begin
  dj = reform(ps2[is,*]) 
  latj = reform(lat2[is,*]) 
  lonj = reform(lon2[is,*]) 
  zsj = reform(zs2[is,*]) 
  dj2 = dj*0.0

;; print,'is=',is,max(dj),max(zsj)
;  dj = quality_control(dj, std_limit=300.,range2=300.)

  jj = where(dj le 800. and dj gt 400, cnt)
  if(cnt gt 0)then begin
   if(abs(latj[is,0]-30) le 10. and (abs(lonj[is,0]-90) le 10.) and  (max(zs[is,*]) ge 3000.))then begin
    jj2 = where(dj gt 100, cnt)


    dd[is2,*] = dj
    dd_lat[is2,*] = latj
    dd_lon[is2,*] = lonj
    dd_zs[is2,*]  = zsj

     print,is,min(dj2[jj2]),max(dj2[jj2]),min(latj[jj2]),min(lonj[jj2])
     if(ic eq 0)then begin
      plot, xj, dj2,min_val = 100.,title=strdigit(is,0),ynozero=1, yrange=[600,700],$
        xrange=[30.,50.],ystyle=1
      ic=ic+20
     endif else begin
      if(ic ge 80)then ic = ic-60
      oplot,xj,dj2 ,min_val  = 100,color=colors.(ic)
      ic=ic+4
     endelse
     is2 = is2+1
    endif
  endif
 endfor
 dd      = dd[0:is2-1,*]
 dd_lat  = dd_lat[0:is2-1,*]
 dd_lon  = dd_lon[0:is2-1,*]
 dd_zs   = dd_zs[0:is2-1,*]
 save,file=dir0+'ispd_tibet.sav0', dd, dd_lat,dd_lon, dd_zs

 for is = 0, is2-1 do begin
  win
  plot, xj,dd_zs[is,*], min_val=100., ynozero=1,psym=2,xrange=[30.,50]
  win
  plot, xj,dd[is,*], min_val=100., ynozero=1,xrange=[30.,50]
  print,is, max(dd_zs[is,*])
  read,ix
  wwdelete
 endfor
 
stop
jump_tibet2:

restore, dir0+'ispd_tibet.sav0'

is2 = n_elements(dd_zs[*,0])
jj3 = where(xj le 50 and xj ge 30)
win
xj3 = xj[jj3]
 ic2 = 0
 is = 0

  ddtxt = xj3 + 1950.
  dd_c  = ' Year' 
; -----------------

for is=0, is2-1 do begin
 yy3 = xj3*0.0
 dd3 = reform(dd[is,jj3])
 dd3_zs = reform(dd_zs[is,jj3]) 

 dd3 = quality_control(dd3, nstd=2)

 if(max(dd3_zs) gt 4000.)then begin
 jj = where(dd3 gt 100. , cnt)
 if(cnt gt n_elements(xj3)*3/4.)then begin
 z3 = dd3_zs[jj]
 p3 = dd3[jj]
 result = regress(z3,p3,yfit=yfit, FTEST=FTEST, CHISQ=CHISQ, SIGMA=SIGMA)
 if(sigma[0] le 0.003 and ftest gt 500.)then begin
 print,SIGMA[0],FTEST,CHISQ
; win
; plot,z3,p3,ynoz=1,psym=2

 yy3[jj] = p3 - yfit + ave10(p3)
 
; win
 yy3 = quality_control(yy3, nstd=2)
 if(ic2 eq 0)then begin
   plot, xj3,yy3,min_val=100., ynoz=1,yrange=[610.,680]
 endif else begin
   oplot,xj3, yy3,min_val=100.,color=colors.(ic2*5+20)
 endelse
  jlon = dd_lon[is,0]
  if(jlon gt 180)then jlon = jlon - 360. 

  print,is,dd_lat[is,0],jlon,ave10(dd3_zs)
  print,'=============================================='

  ddtxt = [ddtxt,yy]

  yloc   = strdigit(abs( dd_lat[is,0]),1 )
  if(dd_lat[is,0] gt 0)then yloc = yloc+'N' else yloc = yloc+'S'
  xloc = strdigit(abs(jlon),1)
  if(jlon gt 0)then xloc = xloc+'E' else xloc = xloc+'W'

  dd_c  = [dd_c, yloc+','+xloc]
; -------------------------------

  yy = de_seasonal(yy3)

  jj = where(yy gt 100)
  p3 = yy[jj]
  x3 = xj3[jj]
  result = regress(x3,p3,sigma=sigma, double=1, const=const, yfit=yfit)
  print,'result==> ',result[0], sigma[0]



; read,ix
 ic2 = ic2+1
; wwdelete

 endif
 endif
 endif
endfor

outf = dir0 + 'st_tibet.txt'
mac1,outf,dd_c,ddtxt
; -----------------

saveimage,'../cmip6/SLR/figs/ispd_tibet.jpeg' 

stop

jump4:

jump_NH:
; for NH and SH 50 degrees poleward, take all stations > 2000 m

 dd = ps*0.0
 dd_lat = ps*0.0
 dd_lon = ps*0.0
 dd_zs = ps*0.0

 ns = n_elements(zs[*,0])
 is1 = 0
 is2 = 0
 xj = indgen(nt2)/12.

 win
 for is = 0, ns-1 do begin  
  dj = reform(ps[is,*])
;  dj = quality_control(dj, std_limit=300.,range2=300.)
  jj  = where(dj gt 100, cnt)
  ;;if(cnt ge 20*12)then begin
  if(cnt ge 15*12)then begin
  if(lat[is,0] gt 50.)then begin
    print,is,lon[is,0],lat[is,0],max(zs[is,*]),max(dj),min(dj)

    dd[is2,*] = dj
    dd_lat[is2,*] = lat[is,*]
    dd_lon[is2,*] = lon[is,*]
    dd_zs[is2,*]  = zs[is,*]

    if(is2 eq 0)then begin
        plot,xj,dj,min_val = 200,yrange=[500.,800],xrange=[20.,66],max_val = 900.,title='NH'
    endif else begin
        oplot, xj, dj, color=colors.(is2*4+10), min_val=100,max_val = 900.
    endelse
    is2 = is2 + 1
  ;  read,ix
  endif
  endif
 endfor

 dd      = dd[0:is2-1,*]
 dd_lat  = dd_lat[0:is2-1,*]
 dd_lon  = dd_lon[0:is2-1,*]
 dd_zs   = dd_zs[0:is2-1,*]
 save,file=dir0+'ispd_nh.sav0', dd, dd_lat,dd_lon, dd_zs

jj3 = where(xj le 55 and xj ge 40) ; based on visual
xj3 = xj[jj3]

  ddtxt = xj3 + 1950.
  dd_c  = ' Year'
; -----------------

for is = 2, is2-1 do begin
 yy3 = xj3*0.0
 dd3 = reform(dd[is,jj3])
 dd3_zs = reform(dd_zs[is,jj3]) 

 jj = where(dd3 gt 100,cnt)
 print,'is=',is,max(dd3),max(dd3_zs),cnt
 if(cnt gt n_elements(dd2)*2./4)then begin

 p3 = dd3[jj]
 z3 = dd3_zs[jj]
 result = regress(z3, p3, yfit=yfit, sigma=sigma, ftest=ftest, chisq = chisq)
; result = my_regress(dd3_zs, dd3, yfit = yfit, sigma=sigma, ftest=ftest, chisq = chisq, $
;      ave = ave, yy= yy )

 p33 = p3 - yfit + mean(p3)
 yy  = dd3*0 
 yy[jj] = p33
 ;win
 ;plot,z3,p3,ynoz=1,psym=2
 yy = quality_control(yy, nstd=2)

 if(max(yy) le 0.)then goto,jump_next_greenland_endif
 win
; if(ic2 eq 0)then begin
;   plot, xj3,yy3,min_val=100., ynoz=1
   plot, xj3,yy,min_val=100., ynoz=1
; endif else begin
;   oplot,xj3, yy3,min_val=100.,color=colors.(ic2*5+20)
; endelse

  jlon = dd_lon[is,0]
  if(jlon gt 180)then jlon = jlon - 360.
  print,is,dd_lat[is,0],jlon,ave10(dd3_zs)
  print,'=============================================='

 ddtxt = [ddtxt,yy]

  yloc   = strdigit(abs( dd_lat[is,0]),1 )
  if(dd_lat[is,0] gt 0)then yloc = yloc+'N' else yloc = yloc+'S'
  xloc = strdigit(abs(jlon),1)
  if(jlon gt 0)then xloc = xloc+'E' else xloc = xloc+'W'

  dd_c  = [dd_c, yloc+','+xloc]
; -------------------------------


  yy = de_seasonal(yy)

  jj = where(yy gt 100)
  p3 = yy[jj]
  x3 = xj3[jj]
  result = regress(x3,p3,sigma=sigma, double=1, const=const, yfit=yfit)
  print,'result==> ',result[0], sigma[0]

 ;read,ix
 ic2 = ic2+1
; wwdelete

 jump_next_greenland_endif:
 endif
 endfor

outf = dir0 + 'st_Greenland.txt'
mac1,outf,dd_c,ddtxt
; -----------------


saveimage,'../cmip6/SLR/figs/ispd_NH.jpeg' 

 save,file=dir0+'ispd_nh.sav0', dd, dd_lat,dd_lon, dd_zs
;======================================================= 
stop

jump_SH:

 dd = ps*0.0
 dd_lat = ps*0.0
 dd_lon = ps*0.0
 dd_zs = ps*0.0

 win
 is2 = 0
 for is = 0, ns-1 do begin  
  dj = reform(ps[is,*])
;  dj = quality_control(dj, std_limit=300.,range2_limit=300)
  jj  = where(dj gt 100, cnt)
  if(cnt ge 20*12)then begin
  if(lat[is,0] lt -50.)then begin

    dd[is2,*] = dj
    dd_lat[is2,*] = lat[is,*]
    dd_lon[is2,*] = lon[is,*]
    dd_zs[is2,*]  = zs[is,*]

    print,is,lon[is,0],lat[is,0],max(zs[is,*]),max(dj),min(dj)
    
    if(is2 eq 0)then begin
        plot,xj,dj,min_val = 200,yrange=[500.,800],xrange=[35.,55],max_val = 900.,title='SH'
    endif else begin
        oplot, xj, dj, color=colors.(is2*4+10), min_val=100,max_val = 900.
    endelse
    is2 = is2 + 1
  ;  read,ix
  endif
  endif
 endfor

 dd      = dd[0:is2-1,*]
 dd_lat  = dd_lat[0:is2-1,*]
 dd_lon  = dd_lon[0:is2-1,*]
 dd_zs   = dd_zs[0:is2-1,*]
 save,file=dir0+'ispd_sh.sav0', dd, dd_lat,dd_lon, dd_zs

jj3 = where(xj le 57 and xj ge 35) ; based on visual
;jj3 = where(xj le 69 and xj ge 0) ; based on visual
xj3 = xj[jj3]

  ddtxt = xj3 + 1950.
  dd_c  = ' Year'
; -----------------
for is = 1, is2-1 do begin
 yy3 = xj3*0.0
 dd3 = reform(dd[is,jj3])
 dd3_zs = reform(dd_zs[is,jj3]) 

 jj = where(dd3 gt 100,cnt)
 if(cnt gt n_elements(dd2)*3./4)then begin

 result = my_regress(dd3_zs, dd3, yfit = yfit, sigma=sigma, ftest=ftest, chisq = chisq, $
      ave = ave, yy= yy )

 yy=dd3
 win
 plot,z3,p3,ynoz=1,psym=2
 ;yy = quality_control(yy, nstd=2)
 win
; if(ic2 eq 0)then begin
;   plot, xj3,yy3,min_val=100., ynoz=1
   plot, xj3,yy,min_val=100., ynoz=1,xstyle=1
; endif else begin
;   oplot,xj3, yy3,min_val=100.,color=colors.(ic2*5+20)
; endelse
  jlon = dd_lon[is,0]

 ddtxt = [ddtxt,yy]

  if(jlon gt 180)then jlon = jlon - 360.
  print,is,dd_lat[is,0],jlon,ave10(dd3_zs)
  print,'=============================================='

  yloc   = strdigit(abs( dd_lat[is,0]),1 )
  if(dd_lat[is,0] gt 0)then yloc = yloc+'N' else yloc = yloc+'S'
  xloc = strdigit(abs(jlon),1)
  if(jlon gt 0)then xloc = xloc+'E' else xloc = xloc+'W'

  dd_c  = [dd_c, yloc+','+xloc]
; -------------------------------
  yy = de_seasonal(yy)

  jj = where(yy gt 100)
  p3 = yy[jj]
  x3 = xj3[jj]
  result = regress(x3,p3,sigma=sigma, double=1, const=const, yfit=yfit)
  print,'result==> ',result[0], sigma[0]

 read,ix
 ic2 = ic2+1
;stop
; wwdelete

 endif
endfor

outf = dir0 + 'st_Antarctic.txt'
mac1,outf,dd_c,ddtxt
; -----------------


saveimage,'../cmip6/SLR/figs/ispd_SH.jpeg' 
;======================================================= 

stop
jump_WNH:
 win
 is2 = 0
 dd = ps*0.0
 dd_lat = ps*0.0
 dd_lon = ps*0.0
 dd_zs = ps*0.0

 win
 for is = 0, ns-1 do begin  
  dj = reform(ps[is,*])
  dj = quality_control(dj, std_limit=300.,range2_limit=300)
  jj  = where(dj gt 100, cnt)
  if(cnt ge 20*12 and max(zs[is,*]) ge 3000.)then begin
  if(abs(lat[is,0]-25) le 30. and lon[is,0] gt 180)then begin
    dd[is2,*] = dj
    dd_lat[is2,*] = lat[is,*]
    dd_lon[is2,*] = lon[is,*]
    dd_zs[is2,*]  = zs[is,*]

    print,is,lon[is,0],lat[is,0],max(zs[is,*]),max(dj),min(dj)
    if(is2 eq 0)then begin
        plot,xj,dj,min_val = 200,yrange=[710.,800],xrange=[20.,66],max_val = 900.,title='WNH'
    endif else begin
        ic2 = is2*4 + 10
        if(ic2 ge 80)then ic2 = ic2/80 + 8
        oplot, xj, dj, color=colors.(ic2), min_val=100,max_val = 900.
    endelse
    is2 = is2 + 1
  ;  read,ix
  endif
  endif
 endfor
 dd      = dd[0:is2-1,*]
 dd_lat  = dd_lat[0:is2-1,*]
 dd_lon  = dd_lon[0:is2-1,*]
 dd_zs   = dd_zs[0:is2-1,*]
 save,file=dir0+'ispd_wnh.sav0', dd, dd_lat,dd_lon, dd_zs
saveimage,'../cmip6/SLR/figs/ispd_WNH0.jpeg' 
 
jj3 = where(xj le 58 and xj ge 35) ; based on visual
xj3 = xj[jj3]

for is = 1, is2-1 do begin
 yy3 = xj3*0.0
 dd3 = reform(dd[is,jj3])
 dd3_zs = reform(dd_zs[is,jj3])

 jj = where(dd3 gt 100,cnt)
 if(cnt gt n_elements(dd3)*3./4)then begin

 result = my_regress(dd3_zs, dd3, yfit = yfit, sigma=sigma, ftest=ftest, chisq = chisq, $
      ave = ave, yy= yy )

 yy = quality_control(yy, nstd=2)
 jj = where(yy gt 100,cnt)

 if(cnt gt n_elements(yy)*3./4 and (max(yy) le 800.) )then begin
 print,'is', is, cnt
 win
 plot,xj3,dd3_zs,ynoz=1,min_va=100.
 win
 plot,z3,p3,ynoz=1,psym=2
 win
; if(ic2 eq 0)then begin
;   plot, xj3,yy3,min_val=100., ynoz=1
   plot, xj3,yy,min_val=100., ynoz=1,xstyle=1
; endif else begin
;   oplot,xj3, yy3,min_val=100.,color=colors.(ic2*5+20)
; endelse
 read,ix
 ic2 = ic2+1
 wwdelete

 endif
 endif
endfor

stop
 
saveimage,'../cmip6/SLR/figs/ispd_WNH.jpeg' 
stop
jump_WSH:
 win

 ic2 = 0
 is2  = 0
 dd = ps*0.0
 dd_lat = ps*0.0
 dd_lon = ps*0.0
 dd_zs = ps*0.0

 ;win
 for is = 0, ns-1 do begin  
  dj = reform(ps[is,*])
  dj = quality_control(dj, std_limit=300.,range2_limit=300)
  jj  = where(dj gt 100, cnt)
  if(cnt ge 20*12 and max(zs[is,*]) ge 3000)then begin
  if(abs(lat[is,0]+25) le 30. and lon[is,0] gt 180)then begin
    dd[is2,*] = dj
    dd_lat[is2,*] = lat[is,*]
    dd_lon[is2,*] = lon[is,*]
    dd_zs[is2,*]  = zs[is,*]

    print,is,lon[is,0],lat[is,0],max(zs[is,*]),max(dj),min(dj)
    if(is2 eq 0)then begin
        plot,xj,dj,min_val = 200,yrange=[600.,800],xrange=[20.,66],max_val = 900.,title='WSH'
    endif else begin
        ic2 = is2*4 + 8 
        if(ic2 ge 80)then ic2 = ic2/80 + 8
        oplot, xj, dj, color=colors.(ic2), min_val=100,max_val = 900.
    endelse
    is2 = is2 + 1
  ;  read,ix
  endif
  endif
 endfor
 dd      = dd[0:is2-1,*]
 dd_lat  = dd_lat[0:is2-1,*]
 dd_lon  = dd_lon[0:is2-1,*]
 dd_zs   = dd_zs[0:is2-1,*]
 save,file=dir0+'ispd_wsh.sav0', dd, dd_lat,dd_lon, dd_zs

win
;--------------------------------------------------
;jj3 = where(xj le 58 and xj ge 45) ; based on visual
jj3 = where(xj le 58 and xj ge 35) ; based on visual
;jj3 = where(xj le 69 and xj ge 0) ; based on visual
xj3 = xj[jj3]
ic2 = 0

  ddtxt = xj3 + 1950.
  dd_c  = ' Year'
; -----------------

for is = 0, is2-1 do begin

 if(is eq 4) then goto,next_sta_wsh
 print,'is=',is
 yy3 = xj3*0.0
 dd3 = reform(dd[is,jj3])
 dd3_zs = reform(dd_zs[is,jj3])

 jj = where(dd3 gt 100,cnt)
 if(cnt gt n_elements(dd2)*3./4)then begin

 result = my_regress(dd3_zs, dd3, yfit = yfit, sigma=sigma, ftest=ftest, chisq = chisq, $
      ave = ave, yy= yy )

;; yy=dd3

 print,'significance sigma ', sigma[0],ftest,chisq
; win
; plot,z3,p3,ynoz=1,psym=2
; win
 yy = quality_control(yy, nstd= 1)
 jj = where(yy gt 100,cnt)
 if(cnt gt n_elements(yy)*3./4 and (max(yy) le 700.) and (is ne 3) )then begin ; is=3 zs change > 800m
; yy = yy-mean(yy[jj])+650.
  if(ic2 eq 0)then begin
    plot, xj3,yy,min_val=100., ynoz=1,xstyle=1 ,yrange=[630.,690]
  endif else begin
    oplot,xj3, yy,min_val=100.,color=colors.(ic2*30+10)
    ;plot,xj3, yy,min_val=100.,color=colors.(ic2*4+10), ynoz=1
  endelse
  ic2 = ic2+1
  jlon = dd_lon[is,0]
  if(jlon gt 180)then jlon = jlon - 360. 
;print,'xxx',max(yy)

  print,is,dd_lat[is,0],jlon,ave10(dd3_zs)
  print,'=============================================='

  yloc   = strdigit(abs( dd_lat[is,0]),1 )
  if(dd_lat[is,0] gt 0)then yloc = yloc+'N' else yloc = yloc+'S'
  xloc = strdigit(abs(jlon),1)
  if(jlon gt 0)then xloc = xloc+'E' else xloc = xloc+'W'

  ddtxt = [ddtxt,yy]

  dd_c  = [dd_c, yloc+','+xloc]
; -------------------------------
  yy = de_seasonal(yy)
  jj = where(yy gt 100)
  p3 = yy[jj]
  x3 = xj3[jj]
  result = regress(x3,p3,sigma=sigma, double=1, const=const, yfit=yfit) 
  print,'result==> ',result[0], sigma[0] 
;    win
;    plot,dd3_zs[jj],p3,min_val=100., ynoz=1,psym=2
 read,ix
;stop
; wwdelete

 endif
 endif
next_sta_wsh:
endfor

outf = dir0 + 'st_Andes.txt'
mac1,outf,dd_c,ddtxt
; -----------------
 
;stop
 
saveimage,'../cmip6/SLR/figs/ispd_WSH.jpeg' 

end
