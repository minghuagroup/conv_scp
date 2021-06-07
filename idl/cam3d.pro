
; need to readin the variables and plot specification
; input filein2, datatype,igif
;------------------------

;filein2 = '../obs/INIT/inputdata/my_init2.cam.h0.2017-07-28-00000.nc'
;filein2 = '../obs/INIT/inputdata/my_init2.cam.h0.2017-07-29-00000.nc'

print,filein2
print,' run 00_range first ok ?? 1 for yes '

;read,ix

print,'this time read in cam data ?? '
read,ix
if(not ix)then goto,jump1  ; read data from saved file======

 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 z2lev  =get_fld(filein2,'lev')
 PS2  =get_fld(filein2,'PS')/100.

 CLOUD2  = get_fld(filein2,'CLOUD')
 CLDLIQ2 = get_fld(filein2,'CLDLIQ')*1.0e5
 CLDICE2 = get_fld(filein2,'CLDICE')*1.0e5

 Z2 = z2lev
; Define pressure levels !!!
 
 T2       = get_fld(filein2,'T')
  ES2 = f_eswi(T2-273.16) 
  p2=1
  T2 = m2p(T2, p2=z2, PS=PS2,Pres=P2)
 
 Q2       = get_fld(filein2, 'Q')*1000. ;g/kg
  QS2  = 0.622*ES2/P2 *1000.  
  RH2  = Q2/QS2
  Q2 = m2p(Q2, p2=z2, PS=PS2)
  RH2 = m2p(RH2, p2=z2, PS=PS2)

 RELHUM2       = get_fld(filein2, 'RELHUM')
  RELHUM2 = m2p(RELHUM2, p2=z2, PS=PS2)

 W2       = get_fld(filein2, 'OMEGA')*36    ;mb/hr
  W2 = m2p(W2, p2=z2, PS=PS2)


 U2        = get_fld(filein2,'U') 
  U2 = m2p(U2, p2=z2, PS=PS2)

 V2        = get_fld(filein2,'V') 
  V2 = m2p(V2, p2=z2, PS=PS2)

 CLOUD2  = get_fld(filein2,'CLOUD')
  CLOUD2 = m2p(CLOUD2, p2=z2, PS=PS2)

 CLDLIQ2 = get_fld(filein2,'CLDLIQ')*1.0e5
  CLDLIQ2 = m2p(CLDLIQ2, p2=z2, PS=PS2)

 CLDICE2 = get_fld(filein2,'CLDICE')*1.0e5
  CLDICE2 = m2p(CLDICE2, p2=z2, PS=PS2)

 PHI2 = get_fld(filein2,'PHIS')

 save,filena=filesave,T2,Q2,W2,RH2,U2,V2,$
         CLOUD2,CLDLIQ2,CLDICE2,PS2,PHI2,X2,Y2,Z2,RELHUM2
 print,'saved file ',filesave
 
 stop

jump1:
 restore,filesave
 print,'data resfored from ',filesave
;===========================

 VARS = ['T','Q','W','RH','U','V','cloud','cldliq','cldice','cldw'] ;10

FOR iv=0,n_elements(vars)-1 DO BEGIN
;====================

 xx = x2
 yy = y2
 zz = Z2
  case strupcase(vars[iv]) of 
   'T':         begin & aa2 = T2  &      levc=lev_T               & end
   'Q':         begin & aa2 = Q2  &      levc=lev_Q               & end
   'U':         begin & aa2 = U2 &       levc=lev_U               & end
   'V':         begin & aa2 = V2 &       levc=lev_V               & end
   'W':         begin & aa2 = W2 &       levc=lev_W               & end
   'RH':        begin & aa2 = RH2 &      levc=lev_RH              & end
   'CLOUD':     begin & aa2 = CLOUD2 &   levc=lev_CLD             & end
   'CLDLIQ':    begin & aa2 = CLDLIQ2 &  levc=lev_CLDLIQ          & end
   'CLDICE':    begin & aa2 = CLDICE2 &  levc=lev_CLDLIQ          & end
   'CLDW':      begin & aa2 = CLDLIQ2+CLDICE2 &  levc=lev_CLDLIQ & END
   'ICIMR':  begin & aa2 = ICIMR2*1.0e5 &  levc=lev_Q             & end
   'ICWMR':  begin & aa2 = ICWMR2*1.0e5 &  levc=lev_Q            & end
   'IWC'  :  begin & aa2 = IWC2*1.0e5   &  levc=lev_Q             & end
   'NUMICE': begin & aa2 = NUMICE2/1000 &  levc=lev_numi  & end
   'NUMLIQ': begin & aa2 = NUMLIQ2/1000 &  levc=lev_numw        & end
   else:
  endcase
  
  var  = VARS[iv] 
  lev1 = levc
  lev2 = [1000.,2000]

  aa = aa2
if(max(aa) eq min(aa))then begin
  print,' Skipped...',var,max(aa)
  goto,jump_loop1
endif

  gifname = 0
  var = datatype+var
  if(igif)then gifname = var

  view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  print,var, '  ',ytitle, ' iave=',iave

  ix=0
  if(iix)then  read,ix

jump_loop1:

ENDFOR ;iv
;====================



end
