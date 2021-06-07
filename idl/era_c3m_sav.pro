; to save data

pro era_c3m_sav, month, fileout = fileout
; end the end:
;  save,file=fileout,T2,Q2,P2,RH2,nn2,qs2,dnn_dz2,vor2,avor2,pv2,div2,duv_dz2,drh_dz2,d2rh_dz2,$
;    w2,cld2,cliq2,cice2,cloud4,cldliq4,cldice4,nx2,ny2,nz2,xx2,yy2,zz2,xx,yy,zz,xxx2,lev1,lev2

if(not keyword_set(fileout))then fileout = 'erac3m_'+strtrim(month,2)+'.sav'
;if(not keyword_set(cldfunc))then cldfunc = 2

;goto, jump_cld
;goto, jump_c3m

; need to readin the variables and plot specification
; input filein2, datatype,igif
;------------------------

nf=month/7   ; 0, 1 for jan and july

fileeras = '../obs/ERAI/'+['ei.moda.an.ml.regn128sc.2008010100.nc',$
             'ei.moda.an.ml.regn128sc.2008070100.nc']
fileerasuv = '../obs/ERAI/'+ ['ei.moda.an.ml.regn128uv.2008010100.nc',$
             'ei.moda.an.ml.regn128uv.2008070100.nc']

fileerasfc='../obs/ERAI/'+[ $
   'ei.moda.an.sfc.regn128sc.2008010100.nc',$
   'ei.moda.an.sfc.regn128sc.2008070100.nc']

filecs    = '../obs/'+['c3m200801.nc','c3m200807.nc']
filecs    = '../obs/C3M/'+['01.nc','07.nc']

filein2 = fileeras[nf]
filein2uv = fileerasuv[nf]
filein2sfc = fileerasfc[nf]
fileincld = filecs[nf]

;ERA
 ncdf_vars,filein2,vars2
 x2  =  get_fld(filein2,'g4_lon_1')
 y2  =  get_fld(filein2,'g4_lat_0')
 hyam = get_fld(filein2,'lv_HYBL2_a')
 hybm = get_fld(filein2,'lv_HYBL2_b')
 P0 = 1000.
 nx2=n_elements(x2)
 ny2=n_elements(y2)
 nz2 = n_elements(hyam)

;c3m
 x  =get_fld(fileincld,'lon')
 y  =get_fld(fileincld,'lat')
 z  =get_fld(fileincld,'lev')
 nx=n_elements(x)
 ny=n_elements(y)
 nz=n_elements(z)

;========== Define pressure levels !!!
 nz3 = 41
 PP = indgen(nz3)*25+25
 XX = indgen(90)*5.+ 2.5
 YY = indgen(45)*5.+ 2.5

;ERA
 PS2 =      get_fld(filein2,'LNSP_GDS4_HYBL_S123')
 PS2 =       exp(PS2)/100.
 TS2 =      get_fld(filein2sfc,'2T_GDS4_SFC_S123')
 U10U2    = get_fld(filein2sfc,'10U_GDS4_SFC_S123')
 U10V2    = get_fld(filein2sfc,'10V_GDS4_SFC_S123')
 SST2     =  get_fld(filein2sfc,'SSTK_GDS4_SFC_S123')
 LANDFRAC2= get_fld(filein2sfc,'LSM_GDS4_SFC_S123')
; LHFLX2   =-get_fld(filein2sfcfct,'SLHF_GDS4_SFC_120')/86400
; SHFLX2   =-get_fld(filein2sfcfct,'SSHF_GDS4_SFC_120')/86400
 TDEW2    = get_fld(filein2sfc,'2D_GDS4_SFC_S123')


; ERA horizontal interpolation
;---------------
 PS2   = my_interpol2(PS2,X2,Y2,X,Y)
 TS2   = my_interpol2(TS2,X2,Y2,X,Y)
 TDEW2 = my_interpol2(TDEW2,X2,Y2,X,Y)
 U10U2 = my_interpol2(U10U2,X2,Y2,X,Y)
 U10V2 = my_interpol2(U10V2,X2,Y2,X,Y)

 SST2      = my_interpol2(SST2 ,X2,Y2,X,Y)
 LANDFRAC2 = my_interpol2(LANDFRAC2,X2,Y2,X,Y)
; LHFLX2    = my_interpol2(LHFLX2,X2,Y2,X,Y)
; SHFLX     = my_interpol2(SHFLX2,X2,Y2,X,Y)

T3 = fltarr(nx,ny,nz2)
Q3=T3 
VOR3 = T3
DIV3 = T3
U3   = T3
V3   = T3
P3 = T3
W3 = T3
CLD3 = T3
CICE3 = T3
CLIQ3 = T3

for k=0,nz2-1 do begin
  P3[*,*,k] = hyam[k]*p0+hybm[k]*ps2[*,*]
endfor

 PHIS2    = get_fld(filein2, 'Z_GDS4_HYBL_S123')/9.8
 PHIS2     = my_interpol2(PHIS2,X2,Y2,X,Y)

 T2       = get_fld(filein2,'T_GDS4_HYBL_S123')
 Q2       = get_fld(filein2, 'Q_GDS4_HYBL_S123') ;*1000. ;g/kg
 vor2     = get_fld(filein2, 'VO_GDS4_HYBL_S123') 
 div2     = get_fld(filein2, 'D_GDS4_HYBL_S123') 
 U2       = get_fld(filein2uv,'U_GDS4_HYBL_S123')
 V2       = get_fld(filein2uv,'V_GDS4_HYBL_S123')
 W2       = get_fld(filein2, 'W_GDS4_HYBL_S123')*36    ;mb/hr
 CLD2  = get_fld(filein2,'CC_GDS4_HYBL_S123')
 CLIQ2 = get_fld(filein2,'CLWC_GDS4_HYBL_S123')*1.0e5
 CICE2 = get_fld(filein2,'CIWC_GDS4_HYBL_S123')*1.0e5


 for k=0,nz2-1 do begin
  tj   = my_interpol2(reform(T2[*,*,k]), X2,Y2,X,Y) 
  qj   = my_interpol2(reform(Q2[*,*,k]), X2,Y2,X,Y) 
  vorj = my_interpol2(reform(vor2[*,*,k]), X2,Y2,X,Y) 
  divj = my_interpol2(reform(div2[*,*,k]), X2,Y2,X,Y) 
  uj   = my_interpol2(reform(u2[*,*,k]), X2,Y2,X,Y) 
  vj   = my_interpol2(reform(v2[*,*,k]), X2,Y2,X,Y) 
  wj   = my_interpol2(reform(w2[*,*,k]), X2,Y2,X,Y) 
  cldj   = my_interpol2(reform(cld2[*,*,k]), X2,Y2,X,Y) 
  cicej   = my_interpol2(reform(cice2[*,*,k]), X2,Y2,X,Y) 
  cliqj   = my_interpol2(reform(cliq2[*,*,k]), X2,Y2,X,Y) 

  T3[*,*,k] = tj
  Q3[*,*,k] = qj
  U3[*,*,k] = uj
  V3[*,*,k] = vj
  vor3[*,*,k] = vorj
  div3[*,*,k] = divj
  w3[*,*,k] = wj
  cld3[*,*,k] = cldj
  cliq3[*,*,k] = cicej
  cice3[*,*,k] = cliqj

 endfor ;k

; P3 already from PS2

; calculate other physical quantities

;RH
  ES3 = f_eswi(t3-273.16) 
  QS3  = 0.622*ES3/P3 ;*1000.  
  RH3  = Q3/QS3

;theta, rho
  theta3  = T3*(1000./p3)^0.286 
  theta3s = TS2*(1000./ps2)^0.286 
  rho3   = P3/(287.*T3)*100.
  dtheta_dp3 = theta3*0

;dtheta/dp
  for k=1,nz2-2 do $
   dtheta_dp3[*,*,k] = (theta3[*,*,k+1] - theta3[*,*,k-1])/(p3[*,*,k+1] - p3[*,*,k-1])/100.
  k=0
   dtheta_dp3[*,*,k] = dtheta_dp3[*,*,k+1]    
  k=nz2-1
   dtheta_dp3[*,*,k] = (theta3s - theta3[*,*,k-1])/(ps2[*,*] - p3[*,*,k-1])/100.

;dtheta_dz & N^2 
   dtheta_dz3 = -dtheta_dp3*rho3*9.8

; N^2
  nn3        = dtheta_dz3/theta3*9.8

; dN^2/dz
  dnn_dp3 = T3*0
  for k=1,nz2-2 do $
   dnn_dp3[*,*,k] = (nn3[*,*,k+1] - nn3[*,*,k-1])/(p3[*,*,k+1] - p3[*,*,k-1])/100.
  k=0
   dnn_dp3[*,*,k] = dnn_dp3[*,*,k+1]    
  k=nz2-1
   dnn_dp3[*,*,k] = dnn_dp3[*,*,k-1]    

   dnn_dz3 = -dnn_dp3*rho3*9.8

; absolute vorticity
  var='AVor'
  avor3 = vor3 *0
  for j=0,ny-1 do avor3[*,j,*] = vor3[*,j,*] + 4*3.1416/86400.*sin(3.1416/180*y[j])

;PV
 PV3 = -avor3*dtheta_dp3 *9.8

; shear squired
 duv_dp3 = T3*0
 for k=1,nz2-2 do $
   duv_dp3[*,*,k] = ( (U3[*,*,k+1] - U3[*,*,k-1])/(p3[*,*,k+1] - p3[*,*,k-1])/100.)^2 $
      + ( (V3[*,*,k+1] - V3[*,*,k-1])/(p3[*,*,k+1] - p3[*,*,k-1])/100.)^2
  k=0
   duv_dp3[*,*,k] = duv_dp3[*,*,k+1]    
  k=nz2-1
   duv_dp3[*,*,k] =  ( (u10u2 - u3[*,*,k-1])/(ps2[*,*] - p3[*,*,k-1])/100.)^2 $
                + ( (u10v2 - v3[*,*,k-1])/(ps2[*,*] - p3[*,*,k-1])/100.)^2

  duv_dz3 = duv_dp3*(rho3*9.8)^2

;Richardson number
 RI3 = T3*0-9999.
  RI3 = NN3/(duv_dz3+1.e-10)

;RH vertical gradient
 dRH_dp3 = T3*0
 ess2 = f_eswi(TS2-273.16)
 dss2 = f_eswi(TDEW2-273.16)
 RHS2 = dss2/ess2 
 for k=1,nz2-2 do $
   drh_dp3[*,*,k] =  (RH3[*,*,k+1] - RH3[*,*,k-1])/(p3[*,*,k+1] - p3[*,*,k-1])/100.
  k=0
   dRH_dp3[*,*,k] = dRH_dp3[*,*,k+1]    
  k=nz2-1
   dRH_dp3[*,*,k] =   (RHS2 - RH3[*,*,k-1])/(ps2[*,*] - p3[*,*,k-1])/100.
  
  dRH_dz3 = - dRH_dp3*rho3*9.8

; d2RH/dz^2
 d2RH_dp3 = T3*0
 for k=1,nz2-2 do $
   d2RH_dp3[*,*,k] =  (dRH_dz3[*,*,k+1] - dRH_dz3[*,*,k-1])/(p3[*,*,k+1] - p3[*,*,k-1])/100.
  k=0
   d2RH_dp3[*,*,k] = d2RH_dp3[*,*,k+1]
  k=nz2-1
   d2RH_dp3[*,*,k] =  d2RH_dp3[*,*,k-1]
  
  d2RH_dz3 = - dRH_dp3*rho3*9.8

;========== Define pressure levels !!!
; nz3 = 41
; PP = indgen(nz3)*25+25

;ERA vertical interpolation
  T2       = m2p_ec(T3,      p2=pp, Pres=P3, ps=ps2)
  Q2       = m2p_ec(Q3,      p2=pp, Pres=P3, ps=ps2)
  P2       = m2p_ec(P3,      p2=pp, Pres=P3, ps=ps2)
  RH2      = m2p_ec(RH3,     p2=pp, pres=P3, ps=ps2)
  nn2      = m2p_ec(nn3,     p2=pp, pres=P3, ps=ps2)
  QS2      = m2p_ec(QS3,     p2=pp, pres=P3, ps=ps2)
  dnn_dz2  = m2p_ec(dnn_dz3,p2=pp, pres=P3, ps=ps2)
  vor2     = m2p_ec(vor3,    p2=pp, pres=P3, ps=ps2)
  avor2    = m2p_ec(avor3,   p2=pp, pres=P3, ps=ps2)
  PV2      = m2p_ec(PV3,     p2=pp, pres=P3, ps=ps2)
  div2     = m2p_ec(div3,    p2=pp, pres=P3, ps=ps2)
  duv_dz2  = m2p_ec(duv_dz3, p2=pp, pres=P3, ps=ps2)
  RI2      = m2p_ec(RI3,     p2=pp, pres=P3, ps=ps2)
  dRH_dz2  = m2p_ec(dRH_dz3, p2=pp, pres=P3, ps=ps2)
  d2RH_dz2 = m2p_ec(d2RH_dz3,p2=pp, pres=P3, ps=ps2)
  W2 = m2p_ec(W3, p2=pp, PS=PS2)
  CLD2 = m2p_ec(CLD3, p2=pp, PS=PS2)
  CLIQ2 = m2p_ec(CLIQ3, p2=pp, PS=PS2)
  CICE2 = m2p_ec(CICE3, p2=pp, PS=PS2)

; ===== next get c3m cloud

jump_c3m:

 ncdf_vars,fileincld,vars
 CLOUD2  = get_fld(fileincld,'CLOUD')
  jjc = where(CLOUD2 le -999.,cntc)
 CLOUD2 = CLOUD2/100.
 CLDLIQ2 = get_fld(fileincld,'CLDLIQ')*1.0e5
 CLDICE2 = get_fld(fileincld,'CLDICE')*1.0e5

 cloud4 = fltarr(nx,ny,nz3)
 cldliq4 = fltarr(nx,ny,nz3)
 cldice4 = fltarr(nx,ny,nz3)

;C3M vertical interpolation

 for i=0,nx-1 do begin
 for j=0,ny-1 do begin
  t0 = reform(t3[i,j,*])
  q0 = reform(q3[i,j,*])
  p0 = reform(p3[i,j,*])
  ps0 = ps2[i,j]
  zs0  = phis2[i,j]

  p_c3m   = calc_p(z, ps0, zs0,p0, t0,q0)  ;!! z to p

  cldj = reform(cloud2[i,j,*])
  cldliqj = reform(cldliq2[i,j,*])
  cldicej = reform(cldice2[i,j,*])

  cloud4[i,j,*]  = my_interpol(cldj,   p_c3m,pp)
  cldliq4[i,j,*] = my_interpol(cldliqj,p_c3m,pp)
  cldice4[i,j,*] = my_interpol(cldicej,p_c3m,pp)
 endfor
 endfor
;stop

 nx2 = nx
 ny2 = ny
 nz2 = nz3
 xx2 = x
 yy2 = y
 zz2 = pp*1.0
 xx = xx2
 yy = yy2
 zz = zz2

 lev1 = cal_lev([0.,100],20)
 lev2 = cal_lev([0.,100],20)*1000 + 9999.

 xxx2 = indgen(2)*1.0
 save,file=fileout,T2,Q2,P2,RH2,nn2,qs2,dnn_dz2,vor2,avor2,pv2,div2,duv_dz2,drh_dz2,d2rh_dz2,$
   w2,cld2,cliq2,cice2,cloud4,cldliq4,cldice4,nx2,ny2,nz2,xx2,yy2,zz2,xx,yy,zz,xxx2,lev1,lev2


 help,T2,Q2,P2,RH2,nn2,qs2,dnn_dz2,vor2,avor2,pv2,div2,duv_dz2,drh_dz2,d2rh_dz2,$
   w2,cld2,cliq2,cice2,cloud4,cldliq4,cldice4,nx2,ny2,nz2,xx2,yy2,zz2,xx,yy,zz,xxx2

 print,'file is saved in ',fileout
 return

end

