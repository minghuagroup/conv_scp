

print,' run 00_range.pro first ok ?? 1 for yes'
read,ix

;NF =0  ;!!!

;filems='../obs/'+['test1.cam.h0.2008-01.nc','test1.cam.h0.2008-07.nc']
;filein2= filems[nf]
titlename= 1 ;datatype2

filein2 = '../data/lens_ANN_climo.nc'

print,filein2,titlename
print,'do plots?'
read,ix

 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')

AEROD_v = get_fld(filein2,'AEROD_v')
CLDHGH  = get_fld(filein2,'CLDHGH')
CLDLOW  = get_fld(filein2,'CLDLOW')
CLDMED  = get_fld(filein2,'CLDMED') 
CLDTOT  = get_fld(filein2,'CLDTOT') 
FLDS    = get_fld(filein2,'FLDS')
FLNS    = get_fld(filein2,'FLNS')
FLNSC   = get_fld(filein2,'FLNSC') 
FLNT    = get_fld(filein2,'FLNT') 
FLNTC   = get_fld(filein2,'FLNTC') 
FLUT    = get_fld(filein2,'FLUT') 
FLUTC   = get_fld(filein2,'FLUTC') 
FSDS    = get_fld(filein2,'FSDS') 
FSDSC   = get_fld(filein2,'FSDSC') 
FSNS    = get_fld(filein2,'FSNS') 
FSNSC   = get_fld(filein2,'FSNSC')
FSNT    = get_fld(filein2,'FSNT') 
FSNTC   = get_fld(filein2,'FSNTC') 
FSNTOA  = get_fld(filein2,'FSNTOA') 
FSNTOAC = get_fld(filein2,'FSNTOAC') 
FSUTOA  = get_fld(filein2,'FSUTOA') 
ICEFRAC = get_fld(filein2,'ICEFRAC') 
LANDFRAC= get_fld(filein2,'LANDFRAC') 
LHFLX   = get_fld(filein2,'LHFLX') 
LWCF    = get_fld(filein2,'LWCF')
OCNFRAC = get_fld(filein2,'OCNFRAC') 
PBLH    = get_fld(filein2,'PBLH') 
PHIS    = get_fld(filein2,'PHIS') 
PRECC   = get_fld(filein2,'PRECC') 
PRECDP  = get_fld(filein2,'PRECDP') 
PRECL   = get_fld(filein2,'PRECL') 
PRECSC  = get_fld(filein2,'PRECSC')
PRECSH  = get_fld(filein2,'PRECSH') 
PRECSL  = get_fld(filein2,'PRECSL') 
PRECT   = get_fld(filein2,'PRECT') 
PS      = get_fld(filein2,'PS') 
PSL     = get_fld(filein2,'PSL') 
QFLX    = get_fld(filein2,'QFLX') 
QREFHT  = get_fld(filein2,'QREFHT') 
SHFLX   = get_fld(filein2,'SHFLX') 
SNOWHICE= get_fld(filein2,'SNOWHICE') 
SNOWHLND= get_fld(filein2,'SNOWHLND')
SOLIN   = get_fld(filein2,'SOLIN') 
SWCF    = get_fld(filein2,'SWCF') 
TAUX    = get_fld(filein2,'TAUX') 
TAUY    = get_fld(filein2,'TAUY') 
TGCLDCWP= get_fld(filein2,'TGCLDCWP') 
TGCLDIWP= get_fld(filein2,'TGCLDIWP') 
TGCLDLWP= get_fld(filein2,'TGCLDLWP') 
TMP2D   = get_fld(filein2,'TMP2D') 
TMQ     = get_fld(filein2,'TMQ') 
TREFHT  = get_fld(filein2,'TREFHT') 
TS      = get_fld(filein2,'TS') 
TSMN    = get_fld(filein2,'TSMN')
TSMX    = get_fld(filein2,'TSMX') 
U10     = get_fld(filein2,'U10')
WGUSTD  = get_fld(filein2,'WGUSTD')


IF(idata eq 15)then begin  ;case 20170728
;;ACTNI  = get_fld(filein2,'ACTNI')
;;ACTNL  = get_fld(filein2,'ACTNL')
;;ACTREL  = get_fld(filein2,'ACTREL')
;;ACTREI  = get_fld(filein2,'ACTREI')
PCLDTOP  = get_fld(filein2,'PCLDTOP')  ;;shuld be saved!
PCLDBOT  = get_fld(filein2,'PCLDBOT')  ;;shuld be saved!
CLDTOP  = get_fld(filein2,'CLDTOP')
CLDBOT  = get_fld(filein2,'CLDBOT')
;;FCTL  = get_fld(filein2,'FCTL')
;;FCTI  = get_fld(filein2,'FCTI')
;;3d LIQCLDF  = get_fld(filein2,'LIQCLDF')
;;3d  ICECLDF  = get_fld(filein2,'ICECLDF')

UW_pblh = get_fld(filein2,'UW_pblh')
cbmf_Cu = get_fld(filein2,'cbmf_Cu')
rcwp_Cu = get_fld(filein2,'rcwp_Cu')
riwp_Cu = get_fld(filein2,'riwp_Cu')
rlwp_Cu = get_fld(filein2,'rlwp_Cu')
tkeavg_Cu = get_fld(filein2,'tkeavg_Cu')
;3d ufrc_Cu = get_fld(filein2,'ufrc_Cu')
; 3d AST
endif


; save,filena=filesave,$
;AEROD_v, ANRAIN, ANSNOW, AQRAIN, AQSNOW, AREI, AREL, AWNC, AWNI, CCN3, CDNUMC, $
;CLDHGH, CLDICE, CLDLIQ, CLDLOW, CLDMED, CLDTOT, CLOUD, DCQ, DTCOND, DTV, EMISCLD, FICE, FLDS,$
;FLNS, FLNSC, FLNT, FLNTC, FLUT, FLUTC, FREQI, FREQL, FREQR, FREQS, FSDS, FSDSC, FSNS, FSNSC, $
;FSNT, FSNTC, FSNTOA, FSNTOAC, FSUTOA, ICEFRAC, ICIMR, ICWMR, IWC, LANDFRAC, LHFLX, LWCF, $
;NUMICE, NUMLIQ, OCNFRAC, OMEGA, PBLH, PHIS, PMID, PRECC, PRECDP, PRECL, PRECSC, $
;PRECSH, PRECSL, PRECT, PS, PSL, Q, QFLX, QREFHT, QRL, QRS, RELHUM, SHFLX, SNOWHICE, SNOWHLND, $
;SOLIN, SWCF, T, TAUX, TAUY, TGCLDCWP, TGCLDIWP, TGCLDLWP, TMP2D, TMQ, TREFHT, TS, TSMN,$
;TSMX, U, U10, V, VD01, WGUSTD, WSUB, Z3, X2,Y2,Z2'

 VARS2=['ACTNI','ACTNL','ACTREL','ACTREI','FCTI','FCTL'] ; not output!
 VARS2 = ['PCLDTOP', 'PCLDBOT','CLDTOP', 'CLDBOT',$
           'UW_pblh','rcwp_Cu','riwp_Cu','rlwp_Cu','tkeavg_Cu']


 VARS=['CLDHGH',  'CLDLOW', 'CLDMED', 'CLDTOT', 'FLNSC', 'FLNT', 'FLNTC',$
       'FSDS', 'FSDSC', 'FSNS', 'FSNSC', 'FSNT', 'FSNTC','LHFLX', 'LWCF', $
       'PBLH', 'PRECC', 'PRECDP', 'PRECL', 'PRECSC', $
       'PRECSH', 'PRECSL', 'PRECT', 'PSL', 'SHFLX',$ 
        'SWCF', 'TAUX', 'TAUY', 'TGCLDCWP', 'TGCLDIWP', 'TGCLDLWP', $
       'TMQ', 'TS', 'U10','WGUSTD']

 VARS=[VARS,VARS2]

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
   'PRECC':     begin & aa2 = PRECC*86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.  &  levc=lev_precc & end
   'PRECDP':    begin & aa2 = PRECDP*86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.   &  levc=lev_precc & end
   'PRECL':     begin & aa2 = PRECL *86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.   &  levc=lev_precl & end
   'PRECSH':    begin & aa2 = PRECSH *86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.  &  levc=lev_precc & end
   'PRECSC':    begin & aa2 = PRECSC *86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.  &  levc=lev_precc & end
   'PRECSL':    begin & aa2 = PRECSL *86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.  &  levc=lev_precl & end
   'PRECT':     begin & aa2 = PRECT *86400*1000 & jj=where(PRECC eq 0) & aa2[jj] = -1.   &  levc=lev_prect & end
   'PSL':       begin & aa2 = PSL/100.      &  levc=lev_ps & end
   'SHFLX':     begin & aa2 = SHFLX    &  levc=lev_shflx & end
   'SWCF':      begin & aa2 = SWCF     &  levc=lev_swcf & end
   'TAUX':      begin & aa2 = TAUX     &  levc=lev_taux & end
   'TAUY':      begin & aa2 = TAUY     &  levc=lev_taux & end
   'TGCLDCWP':  begin & aa2 = TGCLDCWP*100 &  levc=lev_tgcldlwp & end
   'TGCLDIWP':  begin & aa2 = TGCLDIWP*100 &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 = TGCLDLWP*100 &  levc=lev_tgcldlwp & end
   'TMQ':       begin & aa2 = TMQ      &  levc=lev_tmq & end
   'TS':        begin & aa2 = TS-273.10       &  levc=lev_ts & end
   'U10':       begin & aa2 = U10      &  levc=lev_u10 & end
   'WGUSTD':    begin & aa2 = WGUSTD   &  levc=lev_wgustd & end

   'PCLDTOP':     begin & aa2 = 1000.-PCLDTOP/100.  &  levc=lev_pres  & end
   'PCLDBOT':     begin & aa2 = 1000.-PCLDBOT/100.  &  levc=lev_pres  & end
   'CLDTOP':      begin & aa2 = 30-CLDTOP &  levc=lev_k  & end
   'CLDBOT':      begin & aa2 = 30-CLDBOT &  levc=lev_k  & end
   'LIQCLDF':     begin & aa2 = LIQCLDF  &  levc=lev_cld  & end
   'ICECLDF':     begin & aa2 = ICECLDF  &  levc=lev_cld  & end
   'UW_pblh':     begin & aa2 = UW_pblh  &  levc=lev_pblh  & end
   'rcwp_Cu':     begin & aa2 = rcwp_Cu*100  &  levc=lev_tgcldlwp  & end
   'rlwp_Cu':     begin & aa2 = rlwp_Cu*100   &  levc=lev_tgcldlwp  & end
   'riwp_Cu':     begin & aa2 = riwp_Cu*100   &  levc=lev_tgcldlwp  & end
   'tkeavg_Cu':   begin & aa2 = tkeavg_Cu   &  levc=lev_tke_cu  & end
   'ufrc_Cu':     begin & aa2 = ufrc_Cu  &  levc=lev_cld_cu  & end
;;   '':     begin & aa2 =   &  levc=lev_  & end

   else: print,'Variable not found!!'
  endcase
  
  var  = VARS[iv] 
  lev1 = levc
;  lev2 = (levc-levc[0])*3+levc[0]  
  lev2 = [2000.,4000]

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
