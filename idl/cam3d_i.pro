
;IAP initial field fields
; need to readin the variables and plot specification

 datatype = 'IAP_i_20170727_'+ctypes[iave2-1]+'_'

; input filein2, datatype,igif
;------------------------


print,filein2
print,' run 00_range first ok ?? 1 for yes'
read,ix

 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 z2lev  =get_fld(filein2,'lev')
 PS2  =get_fld(filein2,'PS')/100.
 Z2 = z2lev
; Define pressure levels !!!

;3D
T2       = get_fld(filein2,'T')
  ES2 = f_eswi(T2-273.16)
  p2=1
  T = m2p(T2, p2=z2, PS=PS2,Pres=P2)

 Q2       = get_fld(filein2, 'Q')*1000. ;g/kg
  QS2  = 0.622*ES2/P2 *1000.
  RH2  = Q2/QS2
  Q = m2p(Q2, p2=z2, PS=PS2)
  RH = m2p(RH2, p2=z2, PS=PS2)

;T	= get_fld2p(filein2,p2=z2,ps=ps2,'T')
;Q	= get_fld2p(filein2,p2=z2,ps=ps2,'Q')
;OMEGA	= get_fld2p(filein2,p2=z2,ps=ps2,'OMEGA')

if(strmid(datatype,0,3) eq 'IAP')then begin
U	= get_fld2p(filein2,p2=z2,ps=ps2,'U')
V	= get_fld2p(filein2,p2=z2,ps=ps2,'V')
endif

if(strmid(datatype,0,3) eq 'CAM')then begin
; U stagged grid
;--------------
UW	= get_fld(filein2,'US')  
U = T2*0
U[*,1:*,*] = UW
U[*,0,  *] = U[*,1,  *]
U = m2p(U, p2=z2, PS=PS2)
V	= get_fld2p(filein2,p2=z2,ps=ps2,'VS')
endif



;CLOUD   = get_fld2p(filein2,p2=z2,ps=ps2,'CLOUD')
;CLDLIQ  = get_fld2p(filein2,p2=z2,ps=ps2,'CLDLIQ')
;CLDICE  = get_fld2p(filein2,p2=z2,ps=ps2,'CLDICE')

;NUMICE  = get_fld2p(filein2,p2=z2,ps=ps2,'NUMICE')
;NUMLIQ  = get_fld2p(filein2,p2=z2,ps=ps2,'NUMLIQ')

;ICEFRAC = get_fld(filein2,'ICEFRAC') 
;SNOWHICE= get_fld(filein2,'SNOWHICE') 
TS      = get_fld(filein2,'TS1') 


 VARS = ['T','Q','RH','U','V']  ;,'NUMICE', 'NUMLIQ']
 VARS2 = ['TS']

FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2
 zz = Z2


  case VARS[iv]of 
   'T':  	begin & aa2 = T  &  	levc=lev_T		 & end
   'Q':  	begin & aa2 = Q  &  	levc=lev_Q		 & end
   'U':  	begin & aa2 = U &  	levc=lev_U		 & end
   'V':  	begin & aa2 = V &  	levc=lev_V		 & end
   'RH':  	begin & aa2 = RH &  	levc=lev_RH		 & end
   'CLOUD':  	begin & aa2 = CLOUD &  	levc=lev_CLD		 & end
   'CLDLIQ':  	begin & aa2 = CLDLIQ &  levc=lev_CLDLIQ		 & end
   'CLDICE':  	begin & aa2 = CLDICE &  levc=lev_CLDLIQ		 & end
   'CLDW':  	begin & aa2 = CLDW &  	levc=lev_CLDLIQ		 & end
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

print,'do 2-d plots?'
if(iix) then read,ix

FOR iv=0,n_elements(VARS2)-1 DO BEGIN
;====================
 xx = x2
 yy = y2

  case VARS2[iv]of

   'CLDHGH':    begin & aa2 = CLDHGH   &  levc=lev_cld           & end
   'CLDLOW':    begin & aa2 = CLDLOW   &  levc=lev_cld          & end
   'CLDMED':    begin & aa2 = CLDMED   &  levc=lev_cld           & end
   'CLDTOT':    begin & aa2 = CLDTOT   &  levc=lev_cld   & end
   'FLNSC':     begin & aa2 = FLNSC    &  levc=lev_flnsc & end
   'FLNT':      begin & aa2 = FLNT     &  levc=lev_flnt & end
   'FLNTC':     begin & aa2 = FLNTC    &  levc=lev_flnt & end
   'FSDS':      begin & aa2 = FSDS     &  levc=lev_fsds & end
   'FSDSC':     begin & aa2 = FSDSC    &  levc=lev_fsds & end
   'FSNS':      begin & aa2 = FSNS     &  levc=lev_fsds & end
   'FSNSC':     begin & aa2 = FSNSC    &  levc=lev_fsds & end
   'FSNT':      begin & aa2 = FSNT     &  levc=lev_fsnt & end
   'FSNTC':     begin & aa2 = FSNTC    &  levc=lev_fsnt & end
   'LHFLX':     begin & aa2 = LHFLX    &  levc=lev_lhflx & end
   'LWCF':      begin & aa2 = LWCF     &  levc=lev_lwcf & end
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

   else: print,'Variable not found!!'
  endcase

  var  = VARS2[iv]
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

 gifname= datatype+var
 title= gifname+value

 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gif_3d/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif

ENDFOR ;iv2
;====================







end
