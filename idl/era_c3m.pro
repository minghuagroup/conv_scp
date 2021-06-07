
;goto, jump_cld
;goto, jump_c3m

; need to readin the variables and plot specification
; input filein2, datatype,igif
;------------------------

nf=1
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


 for k=0,nz2-1 do begin
  tj   = my_interpol2(reform(T2[*,*,k]), X2,Y2,X,Y) 
  qj   = my_interpol2(reform(Q2[*,*,k]), X2,Y2,X,Y) 
  vorj = my_interpol2(reform(vor2[*,*,k]), X2,Y2,X,Y) 
  divj = my_interpol2(reform(div2[*,*,k]), X2,Y2,X,Y) 
  uj   = my_interpol2(reform(u2[*,*,k]), X2,Y2,X,Y) 
  vj   = my_interpol2(reform(v2[*,*,k]), X2,Y2,X,Y) 

  T3[*,*,k] = tj
  Q3[*,*,k] = qj
  U3[*,*,k] = uj
  V3[*,*,k] = vj
  vor3[*,*,k] = vorj
  div3[*,*,k] = divj
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

;!!   W2       = get_fld(filein2, 'W_GDS4_HYBL_S123')*36    ;mb/hr
;!!  W2 = m2p_ec(W2, p2=pp, PS=PS2)

;!! CLOUD2  = get_fld(filein2,'CC_GDS4_HYBL_S123')
;!!  CLOUD2 = m2p_ec(CLOUD2, p2=pp, PS=PS2)

;!! CLDLIQ2 = get_fld(filein2,'CLWC_GDS4_HYBL_S123')*1.0e5
;!!  CLDLIQ2 = m2p_ec(CLDLIQ2, p2=pp, PS=PS2)

;!! CLDICE2 = get_fld(filein2,'CIWC_GDS4_HYBL_S123')*1.0e5
;!!  CLDICE2 = m2p_ec(CLDICE2, p2=pp, PS=PS2)

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

jump_cld:
;=======================
cloud4x = ave3(cloud4,three=1)
 
nu0 = 20
u0range = cal_lev([0.1,0.99],20)
cldx = fltarr(ny,nz3,nu0)
u00x_5 = fltarr(ny,nz3)          ; 2d cloud
cldx_5 = fltarr(ny,nz3)          ; 2d cloud
jkm    = intarr(ny,nz3)

for j=0,ny-1 do begin
print,'j=',j
for k=0,nz3-1 do begin

  for m = 0,nu0-1 do begin
        u0j  = u0range[m]
        cldj = fltarr(nx)
       for i = 0,nx -1 do begin
          rhj  = rh2[i,j,k]
          ;cldj[i] = cld_f1(rhj, u0j) 
          cldj[i] = cld_f2(rhj, u0j) 
       endfor
       cldx[j,k,m] = mean(cldj)
  endfor

  cldg = reform(cldx[j,k,*])
  delt = abs( cldg - cloud4x[j,k])
  jj = where(delt eq min(delt))
  jj = jj[0]

  u00x_5[j,k] = u0range[jj]
  jkm[j,k]  = jj
  cldx_5[j,k] = cldg[jj]  ; calculated zonal average  

endfor ;k
endfor ;j 

;for plot_3d
aa = fltarr(2,ny,nz3)
xx  = fltarr(2)
for i=0,1 do aa[i,*,*] = cldx_5[*,*]*100.
for i=0,1 do aa[i,*,*] = u00x_5[*,*]*100.


jump_cldx_5:
;================================

help,rh2,cloud4
 
; theta = T2*(1000./P2)^0.286
; z2_e  = 
; dtheta_dz2 = 
; NN2 = sqrt=g/theta*dtheta/dz
 

;for plot_3d
;xx = x 144
yy = y
zz = pp*1.0
aa = cloud4*100.
lev1 = cal_lev([0.,100],20)
 stop


  if(igif)then gifname = var

  view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  print,var, '  ',ytitle, ' iave=',iave

  ix=0
  if(iix)then  read,ix

;====================



end
