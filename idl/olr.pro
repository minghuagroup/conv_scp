

print,' run 00_range.pro first ok ?? 1 for yes'
read,ix

;NF =0  ;!!!

filein2='../obs/olr.mon.mean.nc'

titlename= 'OLR'+months[nf]+'_'

print,filein2,'  ',titlename

 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')

 OLR= get_fld(filein2,'olr')

 time = get_fld(filein2,'time') ; 1979-1 to 2013-12 for 408 months
  ntime = 348  ; jan 2008


 VARS=[ 'FLNT']

FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2

  case VARS[iv]of 

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
   'PRECT':     begin & aa2 = PRECT *86400*1000   &  levc=lev_prect & end
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
  lev2 = (levc-levc[0])*3+levc[0]  ;[1000.,2000]

  aa = aa2
  bb = aa
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )

 ny=n_elements(yy)  ; do weighted average
 aaw=aa*0
 bbw=aaw
 jj=where(aa gt -999.)
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

 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=lon_range,$
      yrange=lat_range,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

 print,gifname
   if(igif)then begin
       mygif,'gif_2d/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif

ENDFOR ;iv
;====================

end
