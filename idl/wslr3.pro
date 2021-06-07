
goto,jump1
; height binned figure and relation with local vs global temperature

; Sea-2evel Rise after wslr to do smoothing of dps

 land = get_fld('../cmip6/CMIP/NCAR_CESM2/amip/nc/sftlf.nc','sftlf')
 
 map  = 'cesm2cmip'
 cntl = 'cmip2cmip' 
 if(strpos(f0,'ERA5') ge 0)then begin
   map = 'cesm2era'
   cntl_map = 'era2era'
 endif
 
 varj = var_map('PS',map)
 ps2 = get_dd2m(f2,varj) & ps2 = ave3(ps2)*0.01
 ps0 = get_dd2m(f0,varj) & ps0 = ave3(ps0)*0.01

 varj = var_map('TS',map)
; dts = get_dd2m(f2,varj,cntl_file=f0,cntl_map = cntl_map)
  ts2 = get_dd2m(f2,varj) & ts2 = ave3(ts2)
  ts0 = get_dd2m(f0,varj) & ts0 = ave3(ts0)

 varj = var_map('T',map)
; dt = get_dd2m(f2,varj,cntl_file=f0,cntl_map = cntl_map, lev3 = zz)
  ta2 = get_dd2m(f2,varj) & ta2 = ave4(ta2)
  ta0 = get_dd2m(f0,varj) & ta0 = ave4(ta0)

 varj = var_map('Z3',map)
; dt = get_dd2m(f2,varj,cntl_file=f0,cntl_map = cntl_map, lev3 = zz)
  Z32 = get_dd2m(f2,varj,lev3=zz) & z32 = ave4(z32)
  Z30 = get_dd2m(f0,varj) & z30 = ave4(z30)

  
 dd = (ps2 - ps0)
 dd_ts = (ts2 - ts0) 

 ddm = ave2(transpose(dd))
 ddm2 = transpose(replicate2(ddm,nx))

 dd = dd - ddm2 ;* 0

 oro_land = oro*land/100.

jump1:

 hh = indgen(22)*250.
 hh = indgen(12)*500.
 nh = n_elements(hh)

 d_psh   = fltarr(nh)
 d_tsh   = fltarr(nh)
 areah = fltarr(nh)
 psh0  = d_psh
 psh2  = d_psh
 tsh0  = d_psh
 tsh2  = d_psh
 zh    = d_psh
 tah0  = d_psh
 tah2  = d_psh
 z3h0  = d_psh
 z3h2  = d_psh

 dpcos = ta0 * 0
 for k = 1, n_elements(zz)-2 do begin
  dpcos[*,*,k] = 0.5*abs(zz[k+1] - zz[k-1]) * cos(yy00)
  jj = where(ps0 lt zz[k], cnt)
  if(cnt gt 0)then dpcos[jj,k] = 0.0
 endfor
;
; ; 1000 mb
 k = 0  ;n_elements(zz)-1
 dp_tmp = ps0 - zz[k]
 jj    = where(dp_tmp lt 0., cnt)
 if(cnt gt 0)then dp_tmp[jj] = 0.0
 dpcos[*,*,k] = dp_tmp * cos(yy00)
; 

 ddpcos = ta0 * 0
 for k = 0, n_elements(zz)-1 do begin
  ddpcos[*,*,k] = cos(yy00)
  jj = where((ps0 lt zz[k]) or (ps2 lt zz[k]), cnt)
  if(cnt gt 0)then ddpcos[jj,k] = 0.0
 endfor

 ttah0 = tah0
 ttah2 = tah2

   dt_tmp = ta2 - ta0
 for ih = 0, nh-2 do begin
   kk   = where( (oro ge hh[ih]) and (oro lt hh[ih+1])) 
   area    = cos(yy00[kk])
   tarea = total(area)
   d_psh[ih] = total(dd[kk] * area)/tarea
   d_tsh[ih] = total(dd_ts[kk] * area)/tarea
   areah[ih] = tarea
   tsh0[ih]  = total(ts0[kk] * area)/tarea
   tsh2[ih]  = total(ts2[kk] * area)/tarea
   psh0[ih]  = total(ps0[kk] * area)/tarea
   psh2[ih]  = total(ps2[kk] * area)/tarea
   zh[ih]  = total(oro[kk] * area)/tarea


   jj0 = where((z30 le zh[ih]*9.8) and (abs(z30) le 60000.) and (abs(ta0) le 500.) $
          and (abs(dt_tmp) le 100.) )  

   ;weight  = ddpcos[jj0]
   weight  = dpcos[jj0]

   tweight = total(weight)
   tah0[ih]  = total(ta0[jj0]*weight)/tweight
   tah2[ih]  = total(ta2[jj0]*weight)/tweight

   ttah0[ih]  = total(dt_tmp[jj0]/(ta0[jj0]+ta2[jj0])^2 * weight) /tweight
   ;print,ttah0[ih],max(dt_tmp[jj0]),max(jj0),max(weight), tweight, max(ta2[jj0])
 endfor


 hh      = hh[0:nh-2]
 d_psh   = d_psh[0:nh-2]
 d_tsh   = d_tsh[0:nh-2]
 areah   = areah[0:nh-2]

 psh0   = psh0[0:nh-2]
 psh2   = psh2[0:nh-2]
 tsh0   = tsh0[0:nh-2]
 tsh2   = tsh2[0:nh-2]
 tah0   = tah0[0:nh-2]
 tah2   = tah2[0:nh-2]
 zh   = zh[0:nh-2]
 ttah0   = ttah0[0:nh-2]*hh

 d_ps2 = psh2 - psh0
 d_ts2 = tsh2 - tsh0

 ytj = (tsh2 - tsh0)/(tsh2 + tsh0)^2 
 yp = d_psh/(psh2+psh0)*2.
 yt = 9.8/287.*ytj*4. * hh
win
plot,yt,yp,psym=2,color=colors.red 
 ytj = (tah2-tah0)/(tah2 + tah0)^2 * hh
 ytj = ttah0
 yt = 9.8/287.*ytj*4. 
 oplot,yt,yp,psym=2

win
lev1 = cal_lev([-1.,1],10)
dj2 = (ps2 - ps0)/(ps2 + ps0)*2. * land
dj2 = (ps2 - ps0)
view3d, 'precentage dps ', dj2, dd2=dj2, xx,yy,zz, iave,lev1,lev1*10

lev2 = lev1*1000



 
end
