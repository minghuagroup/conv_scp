cc = ['black','red','blue','green','pink','cyan','brown','gold','tomato','blueviolet']
cc = [cc,cc,cc,cc]

dir0 = '../cmip6/SLR/sav0/'
zz = 0
iave=3

 goto,jump_plots
; goto,jump_oro
; goto,jump_map_historical
; goto,jump_fig3c
;  goto,jump_fig3_scatter
;  goto,jump_fig2_ens
; goto,jump_fig3a
;  goto,jump_tmp2
; goto,jump_statistical
; goto, jump_map_percentage
;  goto,jump_map_ssp
; goto,jump_map_era
; goto,jump_map


tera20   = indgen(111)
tncep20  = indgen(116)
tera5    = indgen(69) + 50
thist  = indgen(115)
tssp   = indgen(86)+15

; 1. era20 time series d11[111, 15], d22[111,15], hh bin, 1900-2010
 restore,dir0+'era20c2.sav' ;xx,yy,d11,d22,hh, oro, lsm
 d11_era20 = d11
 d22_era20 = d22
 t11_era20 = t11
 t22_era20 = t22


; 2 NCEP-CIRES-DOE 20th 1900-2015 d11[116,15]
 restore, dir0 + 'ncep20c2.sav' ;, xx,yy,d11,d22,hh,lsm,oro
 d11_ncep20 = d11
 d22_ncep20 = d22
 t11_ncep20 = t11
 t22_ncep20 = t22

; 3. ERA5 1950-1978, 1979-2018 ; d11[69,15]
 restore,dir0 + 'era5c2.sav'   ; xx,yy,d11,d22,hh,lsm,oro
 d11_era5 = d11
 d22_era5 = d22
 t11_era5 = t11
 t22_era5 = t22

; 4. cmip6_historical and ssp585, ensemble and individual models
;              PS_ENS          FLOAT     = Array[288, 192, 115] ;1900-2014
;              PS_BIN          FLOAT     = Array[15, 115]
;              PS_BIN2         FLOAT     = Array[15, 115], equivalent to d22
;              MPS_BIN         FLOAT     = Array[15, 115, 26] ; 26 each model 
;              MPS_BIN2        FLOAT     = Array[15, 115, 26]
; ssp585, 22 models and 86 yrs from 2015 to 2100 
; results for some models are zero because of resolution

  expid = 'historical'

 vars = ['ps','tas']
FOR IV = 0, n_elements(Vars)-1 DO BEGIN
  var = vars[iv]
  filesav = dir0+ var + '_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models, models2
   
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


 if(var eq 'ps')then begin
   d11_cmip = transpose(ps_bin)
   d22_cmip = transpose(ps_bin2)
   d11_models = transpose(mps_bin,[1,0,2])
   d22_models = transpose(mps_bin2,[1,0,2])
   ps_hist    = ps_ens
 models_cmip= models
 models2_cmip= models2
 endif else begin
   t11_cmip = transpose(ps_bin)
   t22_cmip = transpose(ps_bin2)
   t11_models = transpose(mps_bin,[1,0,2])
   t22_models = transpose(mps_bin2,[1,0,2])
   tas_hist    = ps_ens
 models_cmip_tas= models
 models2_cmip_tas= models2
 endelse

ENDFOR ; iv

  expid = 'ssp585'
 vars = ['ps','tas']
FOR IV = 0, n_elements(Vars)-1 DO BEGIN
  var = vars[iv]
  filesav = dir0+ var + '_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models, models2

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

 if(var eq 'ps')then begin
   d11_cmip_ssp = transpose(ps_bin)
   d22_cmip_ssp = transpose(ps_bin2)
   d11_models_ssp = transpose(mps_bin,[1,0,2])
   d22_models_ssp = transpose(mps_bin2,[1,0,2])
   ps_ssp    = ps_ens
 models_ssp= models
 models2_ssp= models2
 endif else begin
   t11_cmip_ssp = transpose(ps_bin)
   t22_cmip_ssp = transpose(ps_bin2)
   t11_models_ssp = transpose(mps_bin,[1,0,2])
   t22_models_ssp = transpose(mps_bin2,[1,0,2])
   tas_ssp    = ps_ens
 models_ssp_tas= models
 models2_ssp_tas= models2
 endelse

ENDFOR ; iv

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

; 7 ispd in slr_fig_ispd2.pro
; 8. tas t11 etc
stop
; ========================================================================
jump_plots:

ddtxt = fltarr(5,119)
dd_c  = ' Year NCEP20C ERA20C ERA5 CMIP6'

; fig1
win
ih = 8  ;8
nh = n_elements(hh)
tj = tncep20
dj = d22_ncep20[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
plot,tj,dj, title = 'PS black-NCEP, red-ERA20C, blue-ERA5, green-CMIP_ENS'

ddtxt[0,*] = indgen(119) + 1900.
ddtxt[1,0:115] = dj*100.


tj = tera20
dj = d22_era20[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
oplot,tj,dj,color=colors.red

ddtxt[2,0:110] = dj*100.


tj = tera5
dj = d22_era5[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[0:19])
oplot,tj,dj,color=colors.blue

ddtxt[3,50:118] = dj*100

tj = thist
dj = d22_cmip[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
oplot,tj,dj,color=colors.green


saveimage,dir0+'../figs/fig1.jpeg'

win



ih = 9
; fig 2 model ensemble
tj = thist
dj = d22_cmip[*,ih]
print,min(dj),max(dj)
dj = dj - mean(dj[50:69])
plot,tj,dj,thick=2, title = 'Ensemble Models ps'
djens = dj*0
dtens = tj

dd_c2 = [' Year']
ddtxt2= indgen(115)+1900.
;-------------
km = 0
for im = 0, n_elements(d22_models[0,ih,*])-1 do begin
 tj = thist
 dj = d22_models[*,ih,im]
 print,im, min(dj),max(dj)
  
 jj = where(dj eq 0, cnt)
 if(cnt eq 0)then begin
  dj = dj - mean(dj[50:69])
  oplot,tj,dj,color=colors.red
  dd_c2  = dd_c2 + ' ' + models_cmip[im]
  ddtxt2 = [ddtxt2,dj*100]
  km = km+1
  djens = djens + dj
 endif
endfor
djens = djens/km
oplot,dtens,djens,thick=2, color=colors.blue
saveimage,dir0+'../figs/fig2.jpeg'

ddtxt[4,0:114] = djens

outf = dir0 + '1a_ncep.txt'
ddtxt = transpose(ddtxt)
mac1,outf,dd_c,ddtxt

dd_c2  = dd_c2 + ' ' + 'CMIP6_ENS'
ddtxt2= [ddtxt2,djens]
outf = dir0 + '1c_ncep.txt'
mac1,outf,dd_c2,ddtxt2

;================================
stop

jump_fig2_ens:
jump_fig3_scatter:

ih = 9 
win
xj = t11_models_ssp[*,ih]    
yj = d11_models_ssp[*,ih] 
plot,xj[1:82],yj[1:82],psym=1,ynoz=1,xrange=[265.,300], title='ssp T versus Ps at '+strdigit(hh[ih],0) 

ddtxt = [xj[1:82], yj[1:82]]

xj = t22_models_ssp[*,13]
oplot,xj[1:82],yj[1:82],psym=1,color=colors.red

ddtxt = [xj[1:82],ddtxt]

saveimage,dir0+'../figs/fig2_Ts_Ps_ssp_'+strdigit(hh[ih],0)+'.jpeg' 

dd_c = ' T_Global T_Local Ps '
outf = dir0 + '3a_ssp.txt'
mac1,outf,dd_c,ddtxt

win
xj = t11_cmip[*,ih]    
yj = d11_cmip[*,ih] 
plot,xj[1:112],yj[1:112],psym=1,ynoz=1,xrange=[265.,300], title='cmip: T versus Ps at '+strdigit(hh[ih],0) 

ddtxt = [xj[1:112], yj[1:112]]

xj = t22_cmip[*,13]
oplot,xj[1:112],yj[1:112],psym=1,color=colors.red
ddtxt = [xj[1:112],ddtxt]

saveimage,dir0+'../figs/fig2_Ts_Ps_cmip_'+strdigit(hh[ih],0)+'.jpeg' 

dd_c = ' T_Global T_Local Ps '
outf = dir0 + '3a_cmip.txt'
mac1,outf,dd_c,ddtxt

win
xj = t11_era5[*,ih]    
yj = d11_era5[*,ih] 
plot,xj,yj,psym=1,ynoz=1,xrange=[265.,300], title='era5: T versus Ps at '+strdigit(hh[ih],0) 
ddtxt = [xj, yj]

xj = t22_era5[*,13]
oplot,xj,yj,psym=1,color=colors.red
ddtxt = [xj,ddtxt]

saveimage,dir0+'../figs/fig2_Ts_Ps_era5_'+strdigit(hh[ih],0)+'.jpeg' 

dd_c = ' T_Global T_Local Ps '
outf = dir0 + '3a_era5.txt'
mac1,outf,dd_c,ddtxt

win
xj = t11_era20[*,ih]    
yj = d11_era20[*,ih] 
plot,xj,yj,psym=1,ynoz=1,xrange=[265.,300], title='era20: T versus Ps at '+strdigit(hh[ih],0) 
ddtxt = [xj, yj]

xj = t22_era20[*,13]
oplot,xj,yj,psym=1,color=colors.red
ddtxt = [xj,ddtxt]

saveimage,dir0+'../figs/fig2_Ts_Ps_era20_'+strdigit(hh[ih],0)+'.jpeg' 

dd_c = ' T_Global T_Local Ps '
outf = dir0 + '3a_era20.txt'
mac1,outf,dd_c,ddtxt


win
xj = t11_ncep20[*,ih]    
yj = d11_ncep20[*,ih] 
plot,xj,yj,psym=1,ynoz=1,xrange=[265.,300], title='ncep20: T versus Ps at '+strdigit(hh[ih],0) 
ddtxt = [xj,yj]

xj = t22_ncep20[*,13]
oplot,xj,yj,psym=1,color=colors.red
ddtxt = [xj,ddtxt]

saveimage,dir0+'../figs/fig2_Ts_Ps_ncep20_'+strdigit(hh[ih],0)+'.jpeg' 

dd_c = ' T_Global T_Local Ps '
outf = dir0 + '3a_ncep20c.txt'
mac1,outf,dd_c,ddtxt

stop

;jump_tmp2:

win; time series at different height
ic = 0
for ih=10,6,-2 do begin
xj = indgen(69)+50
yj = d11_era5[*,ih]
 if(ic eq 0)then begin
plot,xj,yj-mean(yj),ynoz=1, title='era5: Ps at 3 h levels '
  endif else begin
oplot,xj,yj-mean(yj),color=get_stru(colors,cc[ic])
  endelse
  ic = ic+1
endfor
saveimage,dir0+'../figs/fig2_Ps_3h_era5.jpeg'

stop
jump_tmp2:
jump_fig3c:

win; ssp time series at different height
ic = 0

slope = fltarr(nh+1)
slope_sigma = slope


dd_c = strtrim(indgen(13)*500,2)+'-'+strtrim((indgen(13)+1)*500,2)

dd_c = [' Year', dd_c, 'Sea_Level', 'Low_Land', 'Surface']

for ih=0,14 do begin
xj = 2016 + indgen(82)
yj = d11_cmip_ssp[1:82,ih]
 if(ic eq 0)then begin
plot,xj,yj-mean(yj[0:9]),ynoz=1, title='ssp: Ps at hh levels ',yrange=[-1,8],$
     xrange=[2015,2100],xstyle=1

  ddtxt = yj - mean(yj[0:9])
  endif 

  oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic])
  ddtxt = [ddtxt, yj-mean(yj[0:9])]

  if(ih eq 10 or (ih eq 13))then oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic]),thick=2
  xj2 = indgen(100)+2000.
  if(ih eq 11)then oplot,xj2,xj2*0, color=colors.black
  ic = ic+1

  result = regress(xj,yj,sigma=sigma)
  slope[ih] = result[0]
  slope_sigma[ih]= sigma[0]
endfor
  yj = d22_cmip_ssp[1:82,13]
  oplot,xj,yj-mean(yj[0:9]),color=colors.red,thick=2
  ddtxt = [ddtxt, yj-mean(yj[0:9])]

  result = regress(xj,yj,sigma=sigma)
  ih = 13
  slope[nh] = result[0]
  slope_sigma[ih]= sigma[0]

outf = dir0 + '3c_ps.txt'
ddtxt= ddtxt*100
ddtxt[0:81] = xj
mac1,outf,dd_c,ddtxt

saveimage,dir0+'../figs/fig2_Ps_hh_ssp.jpeg'

win; ssp tas time series at different height
ic = 0
for ih=0,14 do begin
xj = 2016 + indgen(82)
yj = t11_cmip_ssp[1:82,ih]
 if(ic eq 0)then begin
plot,xj,yj-mean(yj[0:9]),ynoz=1, title='ssp: tas at hh levels ',yrange=[-1,8],$
     xrange=[2015,2100],xstyle=1
  ddtxt = yj-mean(yj[0:9])
  endif 
  oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic])
  ddtxt = [ddtxt, yj-mean(yj[0:9])]
  if(ih eq 10 or (ih eq 13))then oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic]),thick=2
  xj2 = indgen(100)+2000.
  if(ih eq 11)then oplot,xj2,xj2*0, color=colors.black
  ic = ic+1
endfor
  yj = t22_cmip_ssp[1:82,13]
  oplot,xj,yj-mean(yj[0:9]),color=colors.red,thick=2
  ddtxt = [ddtxt, yj-mean(yj[0:9])]

outf = dir0 + '3d_tas.txt'
ddtxt[0:81] = xj
mac1,outf,dd_c,ddtxt

saveimage,dir0+'../figs/fig2_tas_hh_ssp.jpeg'
stop

;jump_tmp2:
win; ssp time series at different height for an individual model
;                                         ======================

model = 'CESM2'
jj = where(models_ssp eq model) & jjm = jj[0]

ic = 0
for ih=0,nh-1 do begin
xj = 2016 + indgen(82)
yj = d11_models_ssp[1:82,ih,jjm]
 if(ic eq 0)then begin
plot,xj,yj-mean(yj[0:9]),ynoz=1, title=model+' ssp: Ps at hh levels ',yrange=[-1,8],$
     xrange=[2015,2100],xstyle=1
  endif 
  oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic])
  if(ih eq 10 or (ih eq 13))then oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic]),thick=2
  xj2 = indgen(100)+2000.
  if(ih eq 11)then oplot,xj2,xj2*0, color=colors.black
  ic = ic+1
endfor
  yj = d22_models_ssp[1:82,13,jjm]
  oplot,xj,yj-mean(yj[0:9]),color=colors.red,thick=2

saveimage,dir0+'../figs/fig2_Ps_hh_'+model+'_ssp.jpeg'

win; ssp tas time series at different height
ic = 0
for ih=0,nh-1 do begin
xj = 2016 + indgen(83)
yj = t11_models_ssp[1:82,ih,jjm]
 if(ic eq 0)then begin
plot,xj,yj-mean(yj[0:9]),ynoz=1, title=model+ ' ssp: tas at hh levels ',yrange=[-1,8],$
     xrange=[2015,2100],xstyle=1
  endif 
  oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic])
  if(ih eq 10 or (ih eq 13))then oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic]),thick=2
  xj2 = indgen(100)+2000.
  if(ih eq 11)then oplot,xj2,xj2*0, color=colors.black
  ic = ic+1
endfor
  yj = t22_models_ssp[1:82,13,jjm]
  oplot,xj,yj-mean(yj[0:9]),color=colors.red,thick=2

saveimage,dir0+'../figs/fig2_tas_hh_'+model+'_ssp.jpeg'

stop


win; historical time series at different height
ic = 0
for ih=0,nh-1 do begin
xj = 1900 + indgen(115)
yj = d11_cmip[0:113,ih]
 if(ic eq 0)then begin
plot,xj,yj-mean(yj[0:9]),ynoz=1, title='historical: Ps at hh levels ',yrange=[-1,2],$
     xrange=[1900.,2014],xstyle=1
  endif 
  oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic])
  if(ih eq 10 or (ih eq 13))then oplot,xj,yj-mean(yj[0:9]),color=get_stru(colors,cc[ic]),thick=2
  xj2 = indgen(200)+1900.
  if(ih eq 11)then oplot,xj2,xj2*0, color=colors.black
  ic = ic+1
endfor
  yj = d22_cmip[0:113,13]
  oplot,xj,yj-mean(yj[0:9]),color=colors.red,thick=2

saveimage,dir0+'../figs/fig2_Ps_hh_historical.jpeg'



stop


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

jump_map_historical:
; historical ensemble model map figues

 restore,'cam6.sav0' ;, hyam2,hybm2,hyai2,hybi2,lev2, ilev2, lon2,lat2, phis2, lsm2

  oro = phis2/9.8
  dj2 = oro
  xx = lon2
  yy = lat2
  iave=3
  lev1 = cal_lev([0,5000],25)
  lev2 = [-100.,-10]*1.0e10
  view2d, '', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1, charsize = 1.1,sidebar=0
stop
 expid = 'historical'
  filesav = dir0+'ps_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models
  
  oro = phis2/9.8
;  era: 2009_2018 - 1979_1988
;  ens: 2005_2014 - 1975_1984
  y0 = 1900 
  y1 = 1975 - y0
  y2 = 2005 - y0
  dps = ave3(ps_ens[*,*,y2:y2+9]) - ave3(ps_ens[*,*,y1:y1+9])
  xx = lon2
  yy = lat2
  dj2 = dps  ;* lsm2
  lev1 = cal_lev([-1.,1.],20) 
  ;;lev1 = cal_lev([-2.,2.],20) 
  lev2 = [0, 1.0e30]
  ;view2d, Expid + ' DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
  view2d, Expid + ' DPS (mb) ', dj2, bb=dj2, xx,yy,iave,lev1,lev2,max_limit=1, min_limit=1,$
     no_contour=1
  saveimage,dir0+'../figs/'+expid+'_dps_unfilted_nolsm.jpeg'

stop
;  sm_dps = slr_smooth(dps, dd2d=dd2d, sdd2d=sdd2d)

  dj2 = dps - reform(sm_dps[*,*,1]) ; 2000 km filter
  dj2 = dj2 * lsm2

  dj2 = set_bounds(dj2, lev1)
 
  jj = where(lsm2 eq 0, cnt)
  dj2[jj] = -9999.
  lev2 = [1.0e20, 1.0e30]
  view2d, Expid + ' F DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
  saveimage,dir0+'../figs/'+expid+'_dps_filted.jpeg'

  dj2 = dd2d - reform(sdd2d[*,*,1]) ; 2000 km zonal deviations
  dj2 = dj2 * lsm2
  jj=where(dj2 le min(lev1),cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj=where(dj2 ge max(lev1)-1.0e-3,cnt)
  if(cnt gt 0)then dj2[jj] = max(lev1)-1.0e-3
; ensemble model map figues

 restore,'cam6.sav0' ;, hyam2,hybm2,hyai2,hybi2,lev2, ilev2, lon2,lat2, phis2, lsm2

 expid = 'historical'
  filesav = dir0+'ps_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models

  oro = phis2/9.8
;  era: 2009_2018 - 1979_1988
;  ens: 2005_2014 - 1975_1984
  y0 = 1900 
  y1 = 1975 - y0
  y2 = 2005 - y0
  dps = ave3(ps_ens[*,*,y2:y2+9]) - ave3(ps_ens[*,*,y1:y1+9])
  xx = lon2
  yy = lat2
  dj2 = dps ;* lsm2
  lev1 = cal_lev([-1.,1.],20) 
  lev2 = [1.0e20, 1.0e30]
  view2d, Expid + ' DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1,$
       no_contour=1
  saveimage,dir0+'../figs/'+expid+'_dps_unfilted.jpeg'

stop
;  sm_dps = slr_smooth(dps, dd2d=dd2d, sdd2d=sdd2d)

  dj2 = dps - reform(sm_dps[*,*,1]) ; 2000 km filter
  dj2 = dj2 * lsm2
  jj=where(dj2 le min(lev1),cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj=where(dj2 ge max(lev1)-1.0e-3,cnt)
  if(cnt gt 0)then dj2[jj] = max(lev1)-1.0e-3
  jj = where(lsm2 eq 0, cnt)
  dj2[jj] = -9999.
  lev2 = [1.0e20, 1.0e30]
  view2d, Expid + ' F DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
  saveimage,dir0+'../figs/'+expid+'_dps_filted.jpeg'

  dj2 = dd2d - reform(sdd2d[*,*,1]) ; 2000 km zonal deviations
  dj2 = dj2 * lsm2
  jj=where(dj2 le min(lev1),cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj=where(dj2 ge max(lev1)-1.0e-3,cnt)
  if(cnt gt 0)then dj2[jj] = max(lev1)-1.0e-3
  jj = where(lsm2 eq 0, cnt)
  dj2[jj] = -9999.
  lev2 = [1.0e20, 1.0e30]
  view2d, Expid + ' FD DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
  saveimage,dir0+'../figs/'+expid+'_dps_filted_d.jpeg'

stop
jump_map_ssp:
; ssp ensemble model map figues

 restore,'cam6.sav0' ;, hyam2,hybm2,hyai2,hybi2,lev2, ilev2, lon2,lat2, phis2, lsm2

 expid = 'historical'
  filesav = dir0+'ps_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models
  hps_ens = ps_ens

 expid = 'ssp585'
  filesav = dir0+'ps_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models

  oro = phis2/9.8
;  era: 2009_2018 - 1979_1988
;  ens: 2190-2199 - 2015_2024 
  y0 = 1900 
  y1 = 1975 - y0
  y2 = 2005 - y0
  dps = ave3(ps_ens[*,*,73:82]) - ave3(ps_ens[*,*,1:10])
  xx = lon2
  yy = lat2
  dj2 = dps ; * lsm2
  lev1 = cal_lev([-5.,5.],20) 
  lev2 = [0*1.0e20, 1.0e30]
  view2d, Expid + ' DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1,$
       no_contour=1
;  saveimage,dir0+'../figs/'+expid+'_dps_unfilted.jpeg'
  saveimage,dir0+'../figs/'+expid+'_dps_unfilted_nolsm.jpeg'

stop
  sm_dps = slr_smooth(dps, dd2d=dd2d, sdd2d=sdd2d)

  dj2 = dps - reform(sm_dps[*,*,1]) ; 2000 km filter
  dj2 = dj2 * lsm2
  jj=where(dj2 le min(lev1),cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj=where(dj2 ge max(lev1)-1.0e-3,cnt)
  if(cnt gt 0)then dj2[jj] = max(lev1)-1.0e-3
  jj = where(lsm2 eq 0, cnt)
  dj2[jj] = -9999.
  lev2 = [1.0e20, 1.0e30]
  view2d, Expid + ' F DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
  saveimage,dir0+'../figs/'+expid+'_dps_filted.jpeg'

  dj2 = dd2d - reform(sdd2d[*,*,1]) ; 2000 km zonal deviations
  dj2 = dj2 * lsm2
  jj=where(dj2 le min(lev1),cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj=where(dj2 ge max(lev1)-1.0e-3,cnt)
  if(cnt gt 0)then dj2[jj] = max(lev1)-1.0e-3
  jj = where(lsm2 eq 0, cnt)
  dj2[jj] = -9999.
  lev2 = [1.0e20, 1.0e30]
  view2d, Expid + ' FD DPS (mb) ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
  saveimage,dir0+'../figs/'+expid+'_dps_filted_d.jpeg'

jump_map_percentage:
;------------------
  dps = ave3(ps_ens[*,*,73:82]) - ave3(ps_ens[*,*,1:10])
  dj2 = dps/ave3(ps_ens[*,*,1:10]) * 1000.
  lev1 = cal_lev([-5., 5.],40)
  view2d, Expid + ' 1/1000 percent ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1,$
    no_contour=1
  saveimage,dir0+'../figs/'+expid+'_dps_percentage05.jpeg'

  jj = where(lsm2 eq 0, cnt)
  dj2[jj] = -9999.
  view2d, Expid + ' 1/1000 percent ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1,$
    no_contour=1
  saveimage,dir0+'../figs/'+expid+'_dps_percentage05_masked.jpeg'
stop
  lev1 = cal_lev([-10., 10.],40)
  view2d, Expid + ' 1/1000 percent ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1,$
    no_contour=1
  saveimage,dir0+'../figs/'+expid+'_dps_percentage10.jpeg'

stop
jump2:
; ERA maps
jump_map_era:

 restore,dir0 + 'era5c2.sav'   ; xx,yy,d11,d22,hh,lsm,oro

 rs = [1000,2000,3000]
 ir=1
 filesav = dir0 +'era5_decadal_smoothed_R'+strdigit(rs[ir],0)+'.sav'
 restore,  filesav ;, dd3,dd3d,sdd3,sdd3d

 filesav = dir0 + 'era5_decadal.sav' ;,dd3, ies, ibs
 restore,filesav
 
 ddps  = dd3  - sdd3
 ddpsd = dd3d - sdd3d

 dj2 = ddps[*,*,2]  - ddps[*,*,1]
 dj3 = ddpsd[*,*,2] - ddpsd[*,*,1]
 dj2 = reform(dj2)/100.
 dj3 = reform(dj3)/100.

 dj2 = dj2 ;*lsm
 ;lev1 = cal_lev([-1.5,1.5],20)
 lev1 = cal_lev([-1.,1.],20)
 lev2 = [0, 1.0e30]
;; view2d, 'ERA dps ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1, No_contour=1
;; saveimage,dir0+'../figs/dps_era5_nolsm.jpeg'

 ;dj2 = dj3 *lsm
 dj2 = dj2 *lsm
 lev1 = cal_lev([-1.,1.],20)
 lev2 = [-10000.,-3000]
  jj = where(dj2 le min(lev1), cnt)
  if(cnt gt 0)then dj2[jj] = min(lev1)
  jj = where(lsm eq 0, cnt)
  dj2[jj] = -9999.
 view2d, 'ERA dps ', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=0,no_contour=1
; saveimage,dir0+'../figs/dpsd_era5_nolsm.jpeg'
 saveimage,dir0+'../figs/dps_era5.jpeg'


stop

jump_oro:

 restore,dir0 + 'era5c2.sav'   ; xx,yy,d11,d22,hh,lsm,oro
dj2 = oro*lsm 
lev1 = cal_lev([0.,5000],25)
lev2 = [-10000.,-3000]*1.0e5
jj = where(dj2 le 0, cnt)
dj2[jj] = - 9999.
jj = where(dj2 gt 5000.)
dj2[jj] = 5000.

 isetZ = 0
 filesav = dir0+'../figs/oro.ps'
 view2d, '', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=0,min_limit=0, setZ=2, $
    ps_file=filesav ,sidebar=0


; view2d, 'Orography (m)', dj2, bb=dj2, xx,yy, iave,lev1,lev2,max_limit=1,min_limit=1
;if(isetZ ne 2)then saveimage,dir0+'../figs/oro2.jpeg'

stop

jump_map:

dj2 = oro*lsm 
lev1 = cal_lev([0.,5000],22)
lev2 = [-10000.,-3000]
jj = where(dj2 le 0, cnt)
dj2[jj] = -1.0
jj = where(dj2 gt 5000.)
dj2[jj] = 5000.

lev1 = cal_lev([-1.,1],20)
view2d, 'test', dj2, bb=dj2, xx,yy, iave,lev1,lev1*10


jump_statistical:

; cmip5

PS = 1
setZ = 0
if(PS)then setZ = 2

expids = ['historical','ssp585','era5']
for iexp = 0,2 do begin
; ==========================
  expid = expids[iexp]

 if(expid ne 'era5')then begin
  restore,'cam6.sav0'
  xx = lon2
  yy = lat2
  lsm = lsm2
  var = 'ps'
  filesav = dir0+ var + '_ens_'+expid+'.sav0'
  restore,filesav ;, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models

 if(iexp eq 0)then begin
  y0 = 1900
  y1 = 1975 - y0
  y2 = 2005 - y0
  lev1 = cal_lev([-1.,1.],20)
 endif else begin
  y0 = 2015 
  y1 = 2016 - y0 ; 1:82 instead of 0:86
  y2 = 2088 - y0 ; 73:82  2088-2097
 lev1 = cal_lev([-5.,5.],20)
 lev1 = cal_lev([-4.,4.],20)
 endelse



  dps = ave3(ps_ens[*,*,y2:y2+9]) - ave3(ps_ens[*,*,y1:y1+9])

;;  sm_dps = slr_smooth(dps, dd2d=dd2d, sdd2d=sdd2d, rs=[2000.])


  d0 = reform(ps_ens[*,*,y1:y1+9]) 
  d2 = reform(ps_ens[*,*,y2:y2+9])
  std0 = stddev(d0,dim=3)
  std2 = stddev(d2,dim=3)
;;  dps = dps - sm_dps ; !!
  
endif else begin ; era5

 restore,dir0 + 'era5c2.sav'   ; xx,yy,d11,d22,hh,lsm,oro
 restore,dir0 + 'era5_decadal.sav'; dd3, std3,ies, ibs
 ir=1
 filesav = dir0 +'era5_decadal_smoothed_R'+strdigit(rs[ir],0)+'.sav'
 restore,  filesav ;, dd3,dd3d,sdd3,sdd3d

 lev1 = cal_lev([-1.,1.],20)
 dd3 = dd3/100.
 sdd3 = sdd3/100.
 std3 = std3/100
 d0 = reform(dd3[*,*,1])
 d2 = reform(dd3[*,*,2])
 sd0 = reform(sdd3[*,*,1])
 sd2 = reform(sdd3[*,*,2])

 std0 = reform(std3[*,*,1])
 std2 = reform(std3[*,*,2])
 dps = d2 - d0
 sdps = sd2 - sd0

;; dps = dps - sdps  ; !!! filtered
endelse
nx = n_elements(xx)
ny = n_elements(yy)

  sp = sqrt((std0*std0+std2*std2)/2.0)
  t = (d2-d0)/sp/sqrt(2.0/10.)
  
  tsig1 = t*0
  tsig2 = t*0
  jj = where(t ge 1.372) ;1.812 10 degrees of freedom, 90% 1-sided versus 95%
  tsig1[jj] = 1.0
  
  jj = where(t ge 1.476) ;90%
  jj = where(t ge 1.372) ;90%
; half degrees of freedom
  if(expid eq 'ssp')then jj = where(t ge 2.015) ; 95%
  if(expid eq 'historical')then jj = where(t ge 1.476) ; 95%
  if(expid eq 'era5')then jj = where(t ge 1.0) ; 95%

  tsig2[jj] = 1.0         ; auto

  ;jj = where(t ge 2.015) ;95%
  ;tsig2[jj] = 2.0         ; auto

  jjl = where(lsm eq 0)

  lev2 = [0.1,0.5,1.1]
  lev2 = [0.1]

 if(expid eq 'era5')then begin
  dj2 = tsig2 * lsm
  dps = dps*lsm
  dj1 = set_bounds(dps, lev1)
  dj1[jjl] = -9999.
 endif else begin
  dj2 = tsig2
  dj1 = set_bounds(dps, lev1)
 endelse

  jjl2 = where(lsm le 0.1 or (dj1 lt max(lev1)/3.))
  tsig2[jjl2] = -9999. 
  dj2 = tsig2
  
  filefig = dir0 + '../figs/test.ps'

  if(PS)then $
  filefig = dir0 + '../figs/fig2b_statistics_'+expid+'.ps' else $
  filefig = dir0 + '../figs/fig2b_statistics_'+expid+'.jpeg'

  title= '' ;'dps '+expid
  view2d, title, dj1*100, bb=dj2,xx,yy, iave,lev1*100,lev2,$
      setZ = 2, PS_file = filefig,sidebar= 1, noc_label=1 

  if(SetZ ne 2)then saveimage,filefig

  ;deviation after smoothing

;  dj1 = dps - sdps
;  zdps = ave2(dps)
;  for j = 0, ny-1 do dj1[*,j] = dj1[*,j] - zdps[j] 
;  dj1 = set_bounds(dj1,lev1)
;  jj = where(lsm eq 0)
;  dj1[jj] = -9999.
;  view2d, 'dps_deviation '+expid, dj1, bb=dj2,xx,yy, iave,lev1,[0.1,0.5,0.9,1.2] ;,max_lim=1,min_lim=1
;  saveimage,dir0 + '../figs/fig2b_statistics_dps_deviation'+expid+'.jpeg'
   
set_plot,'X'
endfor ; expids


stop


end
