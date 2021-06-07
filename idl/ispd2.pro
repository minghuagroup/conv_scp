z0 = 3500.
goto,jump4


Dir0 = '../W/sav/'
Dir0 = '../cmip6/SLR/sav0/'
years  = 1950 + indgen(66) ; 1950-2015
years  = strtrim(years,2)
nt = n_elements(years)
nt2= n_elements(years)*12


years0 = years
months = strmid(strtrim(101+indgen(12),2),1,2)

hh = indgen(12)*500. 
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

 for is = 0, ns-1 do begin
  dj = reform(ps2[is,*]) 
  latj = reform(lat2[is,*]) 
  lonj = reform(lon2[is,*]) 
  dj2 = dj*0.0

  jj = where(dj le 800. and dj gt 400, cnt)
  if(cnt gt 0)then begin
   std = stddev(dj[jj])
   dev = dj - mean(dj[jj])
   range = max(dj[jj]) - min(dj[jj])
   jj2 = where(abs(dev) le 1.*std, cnt2)  ; 2 std 
   if(cnt2 gt 0 and (std le 20) and (range le 100))then begin
     dj2[jj2] = dj[jj2]
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

     xr = findgen(20)+ 30
     yr = xr
     nxr = n_elements(xr)
     indx = indgen(nxr)*12

     yseason = fltarr(12)  
     for i = 0, 11 do begin
        ii = i + indx
        yseason[i] = ave10(dj2[ii+30*12])
     endfor  
        ymean = ave10(yseason)
        yyseason = replicate2(yseason,nt)      
        yyseason = reform(yyseason,nt*12,1)
        dj3 = dj2 -yyseason + ymean 
       
       jj=where(dj2 lt 100,cnt)
       if(cnt gt 0)then dj3[jj] = 0.0
 
;      oplot,xj,dj3,min_val=300, color=colors.(ic),thick=2 

      for i = 0, nxr-1 do begin
       indx = indgen(12) + i*12
       yr[i] = ave10(dj3[indx+30*12])
      endfor
 
      jj = where(dj2 gt 300,cnt)
;      oplot,xr,yr,min_val=300, color=colors.(ic),thick=2 

     jj = where(yr gt 100.,cnt)
     if(cnt gt 1)then begin
     xr= xr[jj]
     yr= yr[jj]
     result = regress(xr,yr,yfit=yfit)
;     oplot,xr,yfit,color=colors.(ic),thick=2
     print,'result=',result
     endif
;     read,ix
    endif
  endif
 endfor

saveimage,'../cmip6/SLR/figs/ispd3500.jpeg' 

stop
jump4:

; for NH and SH 50 degrees poleward, take all stations > 2000 m

 ns = n_elements(zs[*,0])
 is1 = 0
 is2 = 0
 xj = indgen(nt2)/12.

 win
 for is = 0, ns-1 do begin  
  dj = reform(ps[is,*])
  jj  = where(dj gt 100, cnt)
  if(cnt ge 10*12)then begin
  if(lat[is,0] gt 50.)then begin
    print,is,lon[is,0],lat[is,0],max(zs[is,*]),max(dj),min(dj)
    if(is1 eq 0)then begin
        plot,xj,dj,min_val = 200,yrange=[500.,800],xrange=[20.,66],max_val = 900.,title='NH'
    endif else begin
        oplot, xj, dj, color=colors.(is1*4+10), min_val=100,max_val = 900.
    endelse
    is1 = is1 + 1
  ;  read,ix
  endif
  endif
 endfor

saveimage,'../cmip6/SLR/figs/ispd_NH.jpeg' 

 win
 for is = 0, ns-1 do begin  
  dj = reform(ps[is,*])
  jj  = where(dj gt 100, cnt)
  if(cnt ge 10*12)then begin
  if(lat[is,0] lt -50.)then begin
    print,is,lon[is,0],lat[is,0],max(zs[is,*]),max(dj),min(dj)
    if(is2 eq 0)then begin
        plot,xj,dj,min_val = 200,yrange=[500.,800],xrange=[20.,66],max_val = 900.,title='SH'
    endif else begin
        oplot, xj, dj, color=colors.(is2*4+10), min_val=100,max_val = 900.
    endelse
    is2 = is2 + 1
  ;  read,ix
  endif
  endif
 endfor
 
saveimage,'../cmip6/SLR/figs/ispd_SH.jpeg' 



end
