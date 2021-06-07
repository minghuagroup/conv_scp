;igif=0
;iix = 1
;ix=0
;icoarse=1

;goto,jump1
; filein = '../obs/FNL/'+'fnl_20170727_00_00.nc'
 print,''
 print,filein2 
 ncdf_vars,filein,vars
 x2=get_fld(filein2,'longitude')
 y2=get_fld(filein2,'latitude')

 plev  =indgen(17)*50+100  ; 100 to 900 mb
 plev = [1.,2.,3.,5.,7.,10.,20.,30.,50.,70.,plev, 925.,950.,975, 1000.]
 z2 = plev
 nx = n_elements(x2)
 ny = n_elements(y2)
 nz = n_elements(plev)

 U2   = get_fld_fnl3d(filein2,'UGRD',plev)
 V2   = get_fld_fnl3d(filein2,'VGRD',plev)
 HGT2 = get_fld_fnl3d(filein2,'HGT',plev)
 T2   = get_fld_fnl3d(filein2,'TMP',plev)
 RH2  = get_fld_fnl3d(filein2,'RH',plev)

   plevw = plev[10:*]
 W2   = get_fld_fnl3d(filein2,'VVEL',plevw)
 CLDW2= get_fld_fnl3d(filein2,'CLWMR',plevw)

   plevo3 = plev[0:16]
 O3MR2= get_fld_fnl3d(filein2,'O3MR',plevo3)

 QS = f_eswi(T-273.16) & for k=0,nz-1 do QS[*,*,k]= 0.622*QS[*,*,k]/Plev[k]
 Q2  = RH2 * QS /100.

VARS3 = ['T','Q','RH','U','V','W','CLDW'] 

FOR iv=0,n_elements(VARS3)-1 DO BEGIN
;====================
 xx = x2  & xx[0]=x2[0]+0.01 & xx[nx-1]=x2[nx-1]-0.01 
 yy = y2  & yy[0]=y2[0]+0.01 & yy[ny-1]=y2[ny-1]-0.01 
 zz = Z2
  case vars3[iv] of
   'T':         begin & aa2 = T2  &      levc=lev_T               & end
   'Q':         begin & aa2 = Q2*1000  &      levc=lev_Q               & end
   'U':         begin & aa2 = U2 &       levc=lev_U               & end
   'V':         begin & aa2 = V2 &       levc=lev_V               & end
   'W':         begin & aa2 = W2*36. &       levc=lev_W       & zz=plevw        & end
   'RH':        begin & aa2 = RH2/100 &      levc=lev_RH              & end
   'CLOUD':     begin & aa2 = CLOUD2 &   levc=lev_CLD             & end
   'CLDLIQ':    begin & aa2 = CLDLIQ2 &  levc=lev_CLDLIQ          & end
   'CLDICE':    begin & aa2 = CLDICE2 &  levc=lev_CLDLIQ          & end
   'CLDW':      begin & aa2 = CLDW2*1.0e5       &  levc=lev_CLDLIQ & & zz=plevw   & END
;   'CLDW':      begin & aa2 = CLDLID2+CLDICE2 &  levc=lev_CLDLIQ & END
   'ICIMR':  begin & aa2 = ICIMR2*1.0e5 &  levc=lev_Q             & end
   'ICWMR':  begin & aa2 = ICWMR2*1.0e5 &  levc=lev_Q            & end
   'IWC'  :  begin & aa2 = IWC2*1.0e5   &  levc=lev_Q             & end
   'NUMICE': begin & aa2 = NUMICE2/1000 &  levc=lev_numi  & end
   'NUMLIQ': begin & aa2 = NUMLIQ2/1000 &  levc=lev_numw        & end
   else:
  endcase
  var  = VARS3[iv]
  cscale=1
  lev1 = levc * cscale 
  if(levc[0] gt 0)then lev1= (levc-levc[0])*cscale + levc[0]

  lev2 = [1000.,2000]

  aa = aa2

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

ENDFOR ;iv
;====================



end

