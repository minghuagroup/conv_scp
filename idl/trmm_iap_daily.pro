
print,'run 00_range and trmm_cam_head.pro '
;read,ix

kf=0

lev_prect = cal_lev([0,20],20)

FOR  iday = 0,ndays-1 DO BEGIN
;=======================================================

;titlename= icase+'CAM_3h_daily_201707_'+finday[iday]+'_'
;if(iday gt 4)then titlename= icase+'CAM_3h_daily_201708_'+finday[iday]+'_'
titlename= icase+'_3h_daily_201707_'+finday[iday]+'_'

if(iday gt 4)then titlename= icase+'_3h_daily_201708_'+finday[iday]+'_'
print,filein2,'  ',titlename

dday = fltarr(nx2,ny2)

FOR  ih = 0,7 DO BEGIN  ; ----------- do different hours
;=======================================================
kf = kf+1
km = kf
 precip = reform(prect[*,*,km])*1000.*86400
 VARS=[ 'PRECT']

FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2

  case VARS[iv]of 
   'PRECT':     begin & aa2 = precip &  levc=lev_prect & end          ;??

   'CLDHGH':    begin & aa2 = CLDHGH   &  levc=lev_cld		 & end
   'CLDLOW':    begin & aa2 = CLDLOW   &  levc=lev_cld		& end
   'CLDMED':    begin & aa2 = CLDMED   &  levc=lev_cld		 & end
   'CLDTOT':    begin & aa2 = reform(CLDTOT[*,*,nf*6])   &  levc=lev_cld   & end
   'FLNSC':     begin & aa2 = reform(FLNSC[*,*,nf*6])    &  levc=lev_flnsc & end
   'FLNT':      begin & aa2 = reform(OLR[*,*,ntime+nf*6])     &  levc=lev_flnt & end
   'FLNTC':     begin & aa2 = reform(FLNTC[*,*,nf*6])    &  levc=lev_flnt	& end
   'FSDS':      begin & aa2 = reform(FSDS[*,*,nf*6])     &  levc=lev_fsds	& end
   'FSDSC':     begin & aa2 = reform(FSDSC[*,*,nf*6])    &  levc=lev_fsds	& end
   'FSNS':      begin & aa2 = reform(FSNS[*,*,nf*6])     &  levc=lev_fsds & end
   'FSNSC':     begin & aa2 = reform(FSNSC[*,*,nf*6])    &  levc=lev_fsds & end
   'FSNT':      begin & aa2 = reform(FSNT[*,*,nf*6])     &  levc=lev_fsnt & end
   'FSNTC':     begin & aa2 = reform(FSNTC[*,*,nf*6])    &  levc=lev_fsnt & end
   'LHFLX':     begin & aa2 = LHFLX    &  levc=lev_lhflx & end
   'LWCF':      begin & aa2 = reform(LWCF[*,*,nf*6])     &  levc=lev_lwcf & end
   'PBLH':      begin & aa2 = PBLH     &  levc=lev_pblh & end
   'PRECC':     begin & aa2 = PRECC*86400*1000    &  levc=lev_precc & end
   'PRECDP':    begin & aa2 = PRECDP*86400*1000   &  levc=lev_precc & end
   'PRECL':     begin & aa2 = PRECL *86400*1000   &  levc=lev_precl & end
   'PRECSH':    begin & aa2 = PRECSH *86400*1000  &  levc=lev_precc & end
   'PRECSC':    begin & aa2 = PRECSC *86400*1000  &  levc=lev_precc & end
   'PRECSL':    begin & aa2 = PRECSL *86400*1000  &  levc=lev_precl & end
   'PSL':       begin & aa2 = PSL/100.      &  levc=lev_ps & end
   'SHFLX':     begin & aa2 = SHFLX    &  levc=lev_shflx & end
   'SWCF':      begin & aa2 = reform(SWCF[*,*,nf*6])     &  levc=lev_swcf & end
   'TAUX':      begin & aa2 = TAUX     &  levc=lev_taux & end
   'TAUY':      begin & aa2 = TAUY     &  levc=lev_taux & end
   'TGCLDCWP':  begin & aa2 = TGCLDCWP*100 &  levc=lev_tgcldlwp & end
   'TGCLDIWP':  begin & aa2 = TGCLDIWP*100 &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 = TGCLDLWP*100 &  levc=lev_tgcldlwp & end
   'TMQ':       begin & aa2 = TMQ      &  levc=lev_tmq & end
   'TS':        begin & aa2 = TS-273.10       &  levc=lev_ts & end
   'U10':       begin & aa2 = U10      &  levc=lev_u10 & end
   'WGUSTD':    begin & aa2 = WGUSTD   &  levc=lev_wgustd & end

   else: print,'Variable not found!!'
  endcase
  
  var  = VARS[iv] 
  lev1 = levc
  ;lev2 = (levc-levc[0])*3+levc[0]  ;[1000.,2000]
  lev2 = [1000.,2000]


  aa = aa2
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )

;;if(icoarse)then begin
;;   aa = trmm_coarse(aa2,xx,yy,xc,yc)                 ;========!!!
;;   xx = xc
;;   yy = yc
;;endif   

 dday = dday + aa
ENDFOR ;iv
ENDFOR ;ih
;======================================

aa = DDAY/8.
bb = aa

 ny=n_elements(yy)  ; do weighted average
 aaw=aa*0
 bbw=aaw
 jj=where(aa ge 0. and aa lt 10000.)         ;!!
 aaw[jj] = aa[jj]
 bbw[jj] = 1.
 pi2=3.1416/180.
 for j=0,ny-1 do begin
  cosz = cos(yy[j]*pi2)
  aaw[*,j] = cosz*aaw[*,j]
  bbw[*,j] = cosz*bbw[*,j]
 endfor

 value = ' ('+strtrim(min(aa[jj]),2)+', '+strtrim(max(aa[jj]),2)+', '$
    +strtrim(mean(aaw)/mean(bbw),2)+')'

 gifname= titlename+var
 title= gifname+value

 yy2 = replicate2(yy,nx2) & YY2=transpose(YY2)
 jj=where((yy2 lt -60.) or (yy2 gt 60)) & aa[jj] = -9999.   ;MASK


 jj=where(aa le pthreshold,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa 
 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=lon_range,$
      yrange=lat_range,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

;gif_folder = 'gif_trmm_cam/'
 print,gif_folder,gifname

   if(igif)then begin
       mygif,gif_folder+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif
;read,ix

ENDFOR ;iday
;====================

end
