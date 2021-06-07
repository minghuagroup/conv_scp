
;CAM physics fields
; need to readin the variables and plot specification
; input filein2, datatype,igif
;------------------------

print,filein2
print,' run 00_range first ok ?? 1 for yes'
;;read,ix

print,' process saving data for .sav file?  1 for yes'
read,ix
if(not ix)then goto,jump1  ; read data from saved file======

 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 z2lev  =get_fld(filein2,'lev')
 PS2  =get_fld(filein2,'PS')/100.
 Z2 = z2lev
; Define pressure levels !!!

;3D
T	= get_fld2p(filein2,p2=z2,ps=ps2,'T')
Q	= get_fld2p(filein2,p2=z2,ps=ps2,'Q')
OMEGA	= get_fld2p(filein2,p2=z2,ps=ps2,'OMEGA')
U	= get_fld2p(filein2,p2=z2,ps=ps2,'U')
V	= get_fld2p(filein2,p2=z2,ps=ps2,'V')
CLOUD   = get_fld2p(filein2,p2=z2,ps=ps2,'CLOUD')
CLDLIQ  = get_fld2p(filein2,p2=z2,ps=ps2,'CLDLIQ')
CLDICE  = get_fld2p(filein2,p2=z2,ps=ps2,'CLDICE')
RH      = get_fld2p(filein2,p2=z2,ps=ps2,'RELHUM')


ANRAIN  = get_fld2p(filein2,p2=z2,ps=ps2,'ANRAIN') 
ANSNOW  = get_fld2p(filein2,p2=z2,ps=ps2,'ANSNOW') 
AQRAIN  = get_fld2p(filein2,p2=z2,ps=ps2,'AQRAIN') 
AQSNOW  = get_fld2p(filein2,p2=z2,ps=ps2,'AQSNOW') 
AREI    = get_fld2p(filein2,p2=z2,ps=ps2,'AREI') 
AREL    = get_fld2p(filein2,p2=z2,ps=ps2,'AREL') 
AWNC    = get_fld2p(filein2,p2=z2,ps=ps2,'AWNC') 
AWNI    = get_fld2p(filein2,p2=z2,ps=ps2,'AWNI') 
CCN3    = get_fld2p(filein2,p2=z2,ps=ps2,'CCN3') 
CDNUMC  = get_fld2p(filein2,p2=z2,ps=ps2,'CDNUMC')
FREQI   = get_fld2p(filein2,p2=z2,ps=ps2,'FREQI') 
FREQL   = get_fld2p(filein2,p2=z2,ps=ps2,'FREQL') 
FREQR   = get_fld2p(filein2,p2=z2,ps=ps2,'FREQR') 
FREQS   = get_fld2p(filein2,p2=z2,ps=ps2,'FREQS') 
DCQ     = get_fld2p(filein2,p2=z2,ps=ps2,'DCQ') 
DTCOND  = get_fld2p(filein2,p2=z2,ps=ps2,'DTCOND') 
DTV     = get_fld2p(filein2,p2=z2,ps=ps2,'DTV') 
EMISCLD = get_fld2p(filein2,p2=z2,ps=ps2,'EMISCLD') 
FICE    = get_fld2p(filein2,p2=z2,ps=ps2,'FICE') 
ICIMR   = get_fld2p(filein2,p2=z2,ps=ps2,'ICIMR') 
ICWMR   = get_fld2p(filein2,p2=z2,ps=ps2,'ICWMR') 
IWC     = get_fld2p(filein2,p2=z2,ps=ps2,'IWC') 
NUMICE  = get_fld2p(filein2,p2=z2,ps=ps2,'NUMICE') 
NUMLIQ  = get_fld2p(filein2,p2=z2,ps=ps2,'NUMLIQ') 
Z3      = get_fld2p(filein2,p2=z2,ps=ps2,'Z3')
 hyam= get_fld(filein2,'hyam')
 hybm=get_fld(filein2,'hybm')
 PMID = Z3*0
 for k=0,n_elements(hyam)-1 do PMID[*,*,k]=hyam[k]*100000.+hybm[k]*get_fld(filein2,'PS')
;;PMID    = get_fld2p(filein2,p2=z2,ps=ps2,'PMID') 
QRL     = get_fld2p(filein2,p2=z2,ps=ps2,'QRL') 
QRS     = get_fld2p(filein2,p2=z2,ps=ps2,'QRS') 
RELHUM  = get_fld2p(filein2,p2=z2,ps=ps2,'RELHUM') 
VD01    = get_fld2p(filein2,p2=z2,ps=ps2,'VD01')
WSUB    = get_fld2p(filein2,p2=z2,ps=ps2,'WSUB') 

;2D
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


 save,filena=filesave,$
AEROD_v, ANRAIN, ANSNOW, AQRAIN, AQSNOW, AREI, AREL, AWNC, AWNI, CCN3, CDNUMC, $
CLDHGH, CLDICE, CLDLIQ, CLDLOW, CLDMED, CLDTOT, CLOUD, DCQ, DTCOND, DTV, EMISCLD, FICE, FLDS,$
FLNS, FLNSC, FLNT, FLNTC, FLUT, FLUTC, FREQI, FREQL, FREQR, FREQS, FSDS, FSDSC, FSNS, FSNSC, $
FSNT, FSNTC, FSNTOA, FSNTOAC, FSUTOA, ICEFRAC, ICIMR, ICWMR, IWC, LANDFRAC, LHFLX, LWCF, $
NUMICE, NUMLIQ, OCNFRAC, OMEGA, PBLH, PHIS, PMID, PRECC, PRECDP, PRECL, PRECSC, $
PRECSH, PRECSL, PRECT, PS, PSL, Q, QFLX, QREFHT, QRL, QRS, RELHUM, SHFLX, SNOWHICE, SNOWHLND, $
SOLIN, SWCF, T, TAUX, TAUY, TGCLDCWP, TGCLDIWP, TGCLDLWP, TMP2D, TMQ, TREFHT, TS, TSMN,$
TSMX, U, U10, V, VD01, WGUSTD, WSUB, Z3, X2,Y2,Z2


 print,'saved file ',filesave
 
; stop

jump1:
 restore,filesave
 print,'data resfored from ',filesave
;===========================

 VARS = ['ANRAIN ANSNOW AQRAIN AQSNOW AREI AREL AWNC AWNI CCN3 CDNUMC ICIMR ICWMR IWC NUMICE NUMLIQ QRL QRS  DCQ DTCOND DTV EMISCLD FICE WSUB']
 VARS = ['ICIMR', 'ICWMR', 'IWC', 'NUMICE', 'NUMLIQ', 'DCQ', 'DTCOND', 'DTV', 'VD01','WSUB']

FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2
 zz = Z2

 levc = cal_lev([min(aa2),max(aa2)],20)

  case VARS[iv]of 
   'ICIMR':  begin & aa2 = ICIMR*1.0e5 &  levc=lev_Q		 & end
   'ICWMR':  begin & aa2 = ICWMR*1.0e5 &  levc=lev_Q 		& end
   'IWC'  :  begin & aa2 = IWC*1.0e5   &  levc=lev_Q		 & end
   'NUMICE': begin & aa2 = NUMICE/1000         &  levc=lev_numi 		& end
   'NUMLIQ': begin & aa2 = NUMLIQ/1000          &  levc=lev_numw  	& end
   'DCQ'   : begin & aa2 = DCQ*1000*86400.    &  levc=lev_DQ 	& end
   'DTCOND': begin & aa2 = DTCOND*86400.      &  levc=lev_DT	& end
   'DTV':    begin & aa2 = DTV*86400          &  levc=lev_DT	& end
   'VD01':   begin & aa2 = VD01*1000*86400    &  levc=lev_DQ	& end
   'WSUB':   begin & aa2 = WSUB              &  levc=lev_WSUB	& end
   else: print,'Variable not found!!'
  endcase
  
  var  = VARS[iv] 
  lev1 = levc
  lev2 = [1000.,2000]

  aa = aa2

 if(max(aa) ne min(aa)) then begin

  gifname = 0
  var = datatype+var
  if(igif)then gifname = var

  view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  print,var, '  ',ytitle, ' iave=',iave

  ix=1
  if(iix)then  read,ix

 endif

ENDFOR ;iv
;====================



end
