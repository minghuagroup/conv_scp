

print,' run 00_range.pro first ok ?? 1 for yes'
read,ix

;;NF =0  ;!!!

fileera1='../obs/ERAI/'+[ $
   'ei.moda.an.sfc.regn128sc.2008010100.nc',$
   'ei.moda.an.sfc.regn128sc.2008070100.nc']

fileera2='../obs/ERAI/'+[ $
   'ei.mdfa.fc12hr.sfc.regn128sc.2008010100.nc',$
   'ei.mdfa.fc12hr.sfc.regn128sc.2008010100.nc']

fileeras1 = ['ei.moda.an.ml.regn128sc.2008010100.nc',$
	'ei.moda.an.ml.regn128sc.2008070100.nc']



filein1= fileera1[nf]
filein2= fileera2[nf]
filein3 = '../obs/ERAI/'+fileeras1[nf]  ; to get pressure for vertical integration

titlename= 'ERA2d_'+months[nf]+'_'

print,filein2,titlename

 x2  =get_fld(filein2,'g4_lon_1')
 y2  =get_fld(filein2,'g4_lat_0')

;ALBEDO  = get_fld(filein1,'AL_GDS4_SFC_S123')
CLDHGH  = get_fld(filein1,'HCC_GDS4_SFC_S123')
CLDLOW  = get_fld(filein1,'LCC_GDS4_SFC_S123')
CLDMED  = get_fld(filein1,'MCC_GDS4_SFC_S123') 
;CLDTOT  = get_fld(filein2,'CLDTOT') 
FLDS    = -get_fld(filein2,'STRD_GDS4_SFC_120')/86400
FLNS    = -get_fld(filein2,'STR_GDS4_SFC_120')/86400
FLNSC   = -get_fld(filein2,'STRC_GDS4_SFC_120') /86400
FLNT    = -get_fld(filein2,'TTR_GDS4_SFC_120') /86400.
FLNTC   = -get_fld(filein2,'TTRC_GDS4_SFC_120')/86400.
FSDS    = get_fld(filein2,'SSRD_GDS4_SFC_120')/ 86400.
FSNS    = get_fld(filein2,'SSR_GDS4_SFC_120') /86400.
FSNSC   = get_fld(filein2,'SSRC_GDS4_SFC_120')/86400.
FSNT    = get_fld(filein2,'TSR_GDS4_SFC_120') /86400.
FSNTC   = get_fld(filein2,'TSRC_GDS4_SFC_120')/86400. 
LANDFRAC= get_fld(filein1,'LSM_GDS4_SFC_S123') 
LHFLX   = -get_fld(filein2,'SLHF_GDS4_SFC_120')/86400 
LWCF    = FLNTC - FLNT
;OCNFRAC = get_fld(filein2,'OCNFRAC') 
;PBLH    = get_fld(filein2,'PBLH') 
PHIS    = get_fld(filein1,'Z_GDS4_SFC_S123') 
PRECC   = get_fld(filein2,'CP_GDS4_SFC_120') 
;PRECDP  = get_fld(filein2,'PRECDP') 
PRECL   = get_fld(filein2,'LSP_GDS4_SFC_120') 
PRECLF  = get_fld(filein2,'LSPF_GDS4_SFC_120') 
PRECSC  = get_fld(filein2,'CSF_GDS4_SFC_120')
PRECSL  = get_fld(filein2,'LSF_GDS4_SFC_120') 
PRECT   = get_fld(filein2,'TP_GDS4_SFC_120') 
PS      = get_fld(filein1,'SP_GDS4_SFC_S123') 
PSL     = get_fld(filein1,'MSL_GDS4_SFC_S123') 
QFLX    = -get_fld(filein2,'E_GDS4_SFC_120')/86400 
;QREFHT  = get_fld(filein2,'QREFHT') 
ROUGH   = get_fld(filein1,'SR_GDS4_SFC_S123')
RUNOFF  = get_fld(filein2,'RO_GDS4_SFC_120')
SHFLX   = -get_fld(filein2,'SSHF_GDS4_SFC_120')/86400 
;SNOWHICE= get_fld(filein2,'SNOWHICE') 
SNOWHLND= get_fld(filein1,'SD_GDS4_SFC_S123')
SOLIN   = get_fld(filein2,'TISR_GDS4_SFC_120')/86400 
SST     = get_fld(filein1,'SSTK_GDS4_SFC_S123')
SWCF    = -FSNTC + FSNT
TAUX    = get_fld(filein2,'EWSS_GDS4_SFC_120') /86400
TAUY    = get_fld(filein2,'NSSS_GDS4_SFC_120') /86400
TGCLDCWP= get_fld(filein1,'TCW_GDS4_SFC_S123') 
;TGCLDIWP= get_fld(filein2,'TGCLDIWP') 
;TGCLDLWP= get_fld(filein2,'TGCLDLWP') 
;TMP2D   = get_fld(filein2,'TMP2D') 
TMQ     = get_fld(filein1,'TCWV_GDS4_SFC_S123') 
;TREFHT  = get_fld(filein2,'TREFHT') 
TS      = get_fld(filein1,'2T_GDS4_SFC_S123') 
;TSMN    = get_fld(filein2,'TSMN')
;TSMX    = get_fld(filein2,'TSMX') 
U10U    = get_fld(filein1,'10U_GDS4_SFC_S123')
U10V    = get_fld(filein1,'10V_GDS4_SFC_S123')
U10     = get_fld(filein1,'10SI_GDS4_SFC_S123')
;WGUSTD  = get_fld(filein2,'WGUSTD')


 ; vertical integration
 Z2 = indgen(41)*25.
; Define pressure levels !!!

hyam = get_fld(filein3,'lv_HYBL2_a')
hybm = get_fld(filein3,'lv_HYBL2_b')
P0 = 1000.
PS2 = get_fld(filein3,'LNSP_GDS4_HYBL_S123')
PS2 = exp(PS2)/100.
nlev = n_elements(hyam)
nx = n_elements(ps2[*,0])
ny = n_elements(ps2[0,*])
nz = n_elements(hyam)
P = fltarr(nx,ny,nz)
for k=0,nz-1 do begin
  P[*,*,k] = hyam[k]*p0+hybm[k]*ps[*,*]
endfor

 CLDLIQ2 = get_fld(filein3,'CLWC_GDS4_HYBL_S123')
 CLDICE2 = get_fld(filein3,'CIWC_GDS4_HYBL_S123')

 TGCLDLWP = sum2p(cldliq2,p,ps2)
 TGCLDIWP = sum2p(cldice2,p,ps2)
 

; save,filena=filesave,$
;AEROD_v, ANRAIN, ANSNOW, AQRAIN, AQSNOW, AREI, AREL, AWNC, AWNI, CCN3, CDNUMC, $
;CLDHGH, CLDICE, CLDLIQ, CLDLOW, CLDMED, CLDTOT, CLOUD, DCQ, DTCOND, DTV, EMISCLD, FICE, FLDS,$
;FLNS, FLNSC, FLNT, FLNTC, FLUT, FLUTC, FREQI, FREQL, FREQR, FREQS, FSDS, FSDSC, FSNS, FSNSC, $
;FSNT, FSNTC, FSNTOA, FSNTOAC, FSUTOA, ICEFRAC, ICIMR, ICWMR, IWC, LANDFRAC, LHFLX, LWCF, $
;NUMICE, NUMLIQ, OCNFRAC, OMEGA, PBLH, PHIS, PMID, PRECC, PRECDP, PRECL, PRECSC, $
;PRECSH, PRECSL, PRECT, PS, PSL, Q, QFLX, QREFHT, QRL, QRS, RELHUM, SHFLX, SNOWHICE, SNOWHLND, $
;SOLIN, SWCF, T, TAUX, TAUY, TGCLDCWP, TGCLDIWP, TGCLDLWP, TMP2D, TMQ, TREFHT, TS, TSMN,$
;TSMX, U, U10, V, VD01, WGUSTD, WSUB, Z3, X2,Y2,Z2'


 VARS=['CLDHGH',  'CLDLOW', 'CLDMED', 'FLNSC', 'FLNT', 'FLNTC',$
       'FSDS',  'FSNS', 'FSNSC', 'FSNT', 'FSNTC','LHFLX', 'LWCF', $
       'PRECC', 'PRECL', 'PRECSC', $
       'PRECSL', 'PRECT', 'PSL', 'SHFLX',$ 
        'SWCF', 'TAUX', 'TAUY', 'TGCLDCWP', 'TGCLDIWP', 'TGCLDLWP', $
       'TMQ', 'TS', 'U10']


FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2

  case VARS[iv]of 

   'CLDHGH':    begin & aa2 = CLDHGH   &  levc=lev_cld		 & end
   'CLDLOW':    begin & aa2 = CLDLOW   &  levc=lev_cld		& end
   'CLDMED':    begin & aa2 = CLDMED   &  levc=lev_cld		 & end
   'CLDTOT':    begin & aa2 = CLDTOT   &  levc=lev_cld   & end
   'FLNSC':     begin & aa2 = FLNSC    &  levc=lev_flnsc & end
   'FLNT':      begin & aa2 = FLNT     &  levc=lev_flnt & end
   'FLNTC':     begin & aa2 = FLNTC    &  levc=lev_flnt	& end
   'FSDS':      begin & aa2 = FSDS     &  levc=lev_fsds	& end
   'FSDSC':     begin & aa2 = FSDSC    &  levc=lev_fsds	& end
   'FSNS':      begin & aa2 = FSNS     &  levc=lev_fsds & end
   'FSNSC':     begin & aa2 = FSNSC    &  levc=lev_fsds & end
   'FSNT':      begin & aa2 = FSNT     &  levc=lev_fsnt & end
   'FSNTC':     begin & aa2 = FSNTC    &  levc=lev_fsnt & end
   'LHFLX':     begin & aa2 = LHFLX    &  levc=lev_lhflx & end
   'LWCF':      begin & aa2 = LWCF     &  levc=lev_lwcf & end
   'PBLH':      begin & aa2 = PBLH     &  levc=lev_pblh & end
   'PRECC':     begin & aa2 = PRECC*1000    &  levc=lev_precc & end
   'PRECDP':    begin & aa2 = PRECDP*1000   &  levc=lev_precc & end
   'PRECL':     begin & aa2 = PRECL *1000   &  levc=lev_precl & end
   'PRECSH':    begin & aa2 = PRECSH *1000  &  levc=lev_precc & end
   'PRECSC':    begin & aa2 = PRECSC *1000  &  levc=lev_precc & end
   'PRECSL':    begin & aa2 = PRECSL *1000  &  levc=lev_precl & end
   'PRECT':     begin & aa2 = PRECT *1000   &  levc=lev_prect & end
   'PSL':       begin & aa2 = PSL/100.      &  levc=lev_ps & end
   'SHFLX':     begin & aa2 = SHFLX    &  levc=lev_shflx & end
   'SWCF':      begin & aa2 = SWCF     &  levc=lev_swcf & end
   'TAUX':      begin & aa2 = TAUX     &  levc=lev_taux & end
   'TAUY':      begin & aa2 = TAUY     &  levc=lev_taux & end
   'TGCLDCWP':  begin & aa2 = TGCLDCWP &  levc=lev_tgcldlwp & end
   'TGCLDIWP':  begin & aa2 = TGCLDIWP &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 = TGCLDLWP &  levc=lev_tgcldlwp & end
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
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gif_2d/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif

ENDFOR ;iv
;====================

end
