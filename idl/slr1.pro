
 goto,jump_tmp
;goto,jump_plots

dir0 = '../cmip6/SLR/sav0/'

tera20   = indgen(111)
tncep20  = indgen(116)
tera5    = indgen(69) + 50
thist  = indgen(115)
tssp   = indgen(86)+15

; 1. era20 time series d11[111, 12], d22[111,12], hh bin, 1900-2010
 restore,dir0+'era20c2.sav' ;xx,yy,d11,d22,hh, oro, lsm
 d11_era20 = d11
 d22_era20 = d22


; 2 NCEP-CIRES-DOE 20th 1900-2015 d11[116,12]
 restore, dir0 + 'ncep20c2.sav' ;, xx,yy,d11,d22,hh,lsm,oro
 d11_ncep20 = d11
 d22_ncep20 = d22

; 3. ERA5 1950-1978, 1979-2018 ; d11[69,12]
 restore,dir0 + 'era5c2.sav'   ; xx,yy,d11,d22,hh,lsm,oro
 d11_era5 = d11
 d22_era5 = d22

; 4. cmip6_historical and ssp585, ensemble and individual models
;              PS_ENS          FLOAT     = Array[288, 192, 115] ;1900-2014
;              PS_BIN          FLOAT     = Array[12, 115]
;              PS_BIN2         FLOAT     = Array[12, 115], equivalent to d22
;              MPS_BIN         FLOAT     = Array[12, 115, 26] ; 26 each model 
;              MPS_BIN2        FLOAT     = Array[12, 115, 26]
; ssp585, 22 models and 86 yrs from 2015 to 2100 

  expid = 'historical'
  filesav = dir0+'ps_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models
 
 nmodels = n_elements(mps_bin[0,0,*]) 
 nt      = n_elements(mps_bin[0,*,0]) 
 nh      = n_elements(mps_bin[*,0,0]) 

 ps_bin  = fltarr(nh, nt)
 ps_bin2 = fltarr(nh, nt)
for ih = 0, nh-1 do begin
 km = 0
 for im = 0, nmodels-1 do begin
  dj = reform(mps_bin[ih,*,im])
  dj2 = reform(mps_bin2[ih,*,im])
  if(min(dj) gt 100)then begin
    ps_bin[ih,*] = ps_bin[ih,*] + dj
    ps_bin2[ih,*] = ps_bin2[ih,*] + dj2
    km = km+1
  endif
; print, ih,min(dj),max(dj),km
 endfor ; im
 if(km gt 0)then begin
 ps_bin[ih,*] = ps_bin[ih,*]/km
 ps_bin2[ih,*] = ps_bin2[ih,*]/km
 endif
endfor ; ih

 d11_cmip = transpose(ps_bin)
 d22_cmip = transpose(ps_bin2)

 d11_models = transpose(mps_bin,[1,0,2])
 d22_models = transpose(mps_bin2,[1,0,2])
 ps_hist    = ps_ens

  expid = 'ssp585'
  filesav = dir0+'ps_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models

 nmodels = n_elements(mps_bin[0,0,*])
 nt      = n_elements(mps_bin[0,*,0])
 nh      = n_elements(mps_bin[*,0,0])

 ps_bin  = fltarr(nh, nt)
 ps_bin2 = fltarr(nh, nt)
for ih = 0, nh-1 do begin
 km = fltarr(nt)
 for im = 0, nmodels-1 do begin
  dj = reform(mps_bin[ih,*,im])
  dj2 = reform(mps_bin2[ih,*,im])
  jj = where(dj gt 100.,cnt)
  if(cnt gt 0)then begin
    ps_bin[ih,jj] = ps_bin[ih,jj] + dj[jj]
    ps_bin2[ih,jj] = ps_bin2[ih,jj] + dj2[jj]
    km[jj]  = km[jj] +1
  endif
 endfor ; im
 kk = where(km gt 0, cnt)
 if(cnt gt 0)then begin
 ps_bin[ih,kk] = ps_bin[ih,kk]/km[kk]
 ps_bin2[ih,kk] = ps_bin2[ih,kk]/km[kk]
 endif
endfor ; ih

 d11_cmip_ssp = transpose(ps_bin)
 d22_cmip_ssp = transpose(ps_bin2)
 d11_models_ssp = transpose(mps_bin,[1,0,2])
 d22_models_ssp = transpose(mps_bin2,[1,0,2])
 ps_ssp    = ps_ens

;5.era5 3 periods
;             DD3             FLOAT     = Array[1440, 721, 3], three periods
 filesav = dir0 + 'era5_decadal.sav' ;,dd3, ies, ibs
 restore,filesav
 ; difference between two periods
 dp1 = dd3[*,*,1] - dd3[*,*,0]
 dp2 = dd3[*,*,2] - dd3[*,*,1]
 dp1 = reform(dp1)
 dp2 = reform(dp2)
;  
; 6. era5 3 periods, smoothed based on three radius, original dd3 and zonal deviation dd3d
;         DD3             FLOAT     = Array[1440, 721, 3]
;         DD3D            FLOAT     = Array[1440, 721, 3]
;         SDD3            FLOAT     = Array[1440, 721, 3]
;         SDD3D           FLOAT     = Array[1440, 721, 3]

 rs = [1000,2000,3000]
 ir=1
 filesav = dir0 +'era5_decadal_smoothed_R'+strdigit(rs[ir],0)+'.sav'
 restore,  filesav ;, dd3,dd3d,sdd3,sdd3d
 ; difference between two periods of zonal deviations
 ddp1 = dd3d[*,*,1] - dd3d[*,*,0]
 ddp2 = dd3d[*,*,2] - dd3d[*,*,1]
 ddp1 = reform(ddp1)
 ddp2 = reform(ddp2)

 sdp1 = sdd3[*,*,1] - sdd3[*,*,0]
 sdp2 = sdd3[*,*,2] - sdd3[*,*,1]
 sdp1 = reform(sdp1)
 sdp2 = reform(sdp2)

 sddp1 = sdd3d[*,*,1] - sdd3d[*,*,0]
 sddp2 = sdd3d[*,*,2] - sdd3d[*,*,1]
 sddp1 = reform(sddp1)
 sddp2 = reform(sddp2)

; different periods; filtered ps difference 
 fdp1 = dp1 - sdp1  
 fdp2 = dp2 - sdp2
; different periods; filtered ps difference of zonal deviations
 fddp1 = ddp1 - sddp1  
 fddp2 = ddp2 - sddp2

; to look, period 2009-2018 minus 1979-1988
; dp2, ddp2, fdp2, fddp2 ; original ps difference, deviation difference, filted original, filterd devia
; dp1 is for 1979-1988 minus 1950-1959

; model dp[x,y] to be processed by ps_hist[*,*,115] and ps_ssp[*,*,86], including smoothing

 ; to recover coordinates
 restore,dir0 + 'era20c2.sav' ; restore,'../W/sav0/era20c2.sav' 

; 7 ispd
; 8. tas
jump_plots:

; fig1
win
ih = 8
nh = n_elements(hh)
tj = tncep20
dj = d22_ncep20[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
plot,tj,dj, title = 'PS black-NCEP, red-ERA20C, blue-ERA5, green-CMIP_ENS'

tj = tera20
dj = d22_era20[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
oplot,tj,dj,color=colors.red

tj = tera5
dj = d22_era5[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[0:19])
oplot,tj,dj,color=colors.blue

tj = thist
dj = d22_cmip[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
oplot,tj,dj,color=colors.green

saveimage,dir0+'../figs/fig1.jpeg'

win
ih = 8
; fig 2 model ensemble
tj = thist
dj = d22_cmip[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
plot,tj,dj,thick=2, title = 'Ensemble Models ps'
djens = dj*0
dtens = tj

km = 0
for im = 0, n_elements(d22_models[0,ih,*])-1 do begin
 tj = thist
 dj = d22_models[*,ih,im]
 print,im, min(dj),max(dj)
  
 jj = where(dj eq 0, cnt)
 if(cnt eq 0)then begin
  dj = dj - mean(dj[50:69])
  oplot,tj,dj,color=colors.red
  km = km+1
  djens = djens + dj
 endif
endfor
djens = djens/km
oplot,dtens,djens,thick=2, color=colors.blue
saveimage,dir0+'../figs/fig2.jpeg'

win
ih = 8
; fig 2b model ensemble future
tj = tssp
dj = d22_cmip_ssp[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[0:19])
plot,tj,dj,thick=2, title = 'Ensemble Models Future ps',xrange=[15., 98],xstyle=1
djens = dj*0
dtens = tj

km = 0
for im = 0, n_elements(d22_models_ssp[0,ih,*])-1 do begin
 tj = tssp
 dj = d22_models_ssp[*,ih,im]
 print,im, min(dj),max(dj)
  
 jj = where(dj eq 0, cnt)
 ;if(cnt eq 0)then begin
  dj = dj - mean(dj[1:20])
  oplot,tj,dj,color=colors.red
  km = km+1
;  djens = djens + dj
; endif
endfor
djens = djens/km
;oplot,dtens,djens,thick=2, color=colors.blue
saveimage,dir0+'../figs/fig2b.jpeg'



win
;fig3
; as a function of height d11, model
tj = thist
for ih = 0, nh-1,2 do begin
 dj = reform(d11_cmip[*,ih])
 dj = dj - mean(dj[50:69])
 if(ih eq 0)then $
   plot,tj,dj,yrange=[-2.,2],title='CMIP6 Historical ENS as a function of height' else $
   oplot,tj,dj
   if(ih eq 4) then oplot,tj,dj,color=colors.red
   if(ih eq 6) then oplot,tj,dj,color=colors.blue
   if(ih eq 8) then oplot,tj,dj,color=colors.green
;   if(ih eq 10) then oplot,tj,dj,color=colors.cyan
 endfor 
saveimage,dir0+'../figs/fig3.jpeg'

win
;fig3b
; as a function of height d11, model future
tj = tssp
for ih = 0, nh-1,2 do begin
 dj = reform(d11_cmip_ssp[*,ih])
 dj = dj - mean(dj[1:20])
 if(ih eq 0)then $
   plot,tj,dj,title='CMIP6 Future ENS as a function of height', $
         xrange = [15.,98],xstyle=1, yrange=[-1,7] else $
   oplot,tj,dj
   if(ih eq 4) then oplot,tj,dj,color=colors.red
   if(ih eq 6) then oplot,tj,dj,color=colors.blue
   if(ih eq 8) then oplot,tj,dj,color=colors.green
   if(ih eq 10) then oplot,tj,dj,color=colors.cyan
 endfor 
saveimage,dir0+'../figs/fig3b.jpeg'

stop
win
;fig4
; as a function of height d11, era5
tj = tera5
for ih = 0, nh-1,2 do begin
 dj = reform(d11_era5[*,ih])
 dj = dj - mean(dj[0:19])
 if(ih eq 0)then $
   plot,tj,dj,yrange=[-2.,2],title='ERA5 as a function of height' else $
   oplot,tj,dj
   if(ih eq 4) then oplot,tj,dj,color=colors.red
   if(ih eq 6) then oplot,tj,dj,color=colors.blue
   if(ih eq 8) then oplot,tj,dj,color=colors.green
;   if(ih eq 10) then oplot,tj,dj,color=colors.cyan
 endfor 
saveimage,dir0+'../figs/fig4.jpeg'


stop
jump2:
restore,'../W/sav0/era5c2.sav' ;xx,yy,d11,d22,hh, oro, lsm
dj2 = dp2
dj2 = ddp2  ; zonal deviation
dj2 = fddp2
dj2 = fdp2  ; filtered

dj2 = dj2*lsm
zz = 0
iave=3
lev1 = cal_lev([-5.,5],20)
view3d, 'test', dj2, dd2=dj2, xx,yy,zz, iave,lev1,lev1*10



end
