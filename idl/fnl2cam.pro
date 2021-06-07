
cam_fileold = '../obs/INIT/myctl_test.cam.i.2008-07-01-00000.nc'
cam_filenew = '../obs/INIT/myinput/my_cam_init_temp_20170727_00_00.nc'
fnl_file    = '../obs/FNL/fnl_20170727_00_00.nc'

date=20170727
idiag = 1

SPAWN, 'cp '+ cam_fileold+' '+cam_filenew

filein = cam_filenew
filein2 = fnl_file

 print,'to rewrite ',filein
 print,'use data from ',filein

;=== get CAM info as target

 ncdf_vars,filein,vars
 lon  = get_fld(filein,'lon')
 lat  = get_fld(filein,'lat')
 slon = get_fld(filein,'slon')
 slat = get_fld(filein,'slat')
 hyam = get_fld(filein,'hyam')
 hybm = get_fld(filein,'hybm')
 lev  = get_fld(filein,'lev')
 ilev = get_fld(filein,'ilev')
 P0 = 1000.
 nlon = n_elements(lon)
 nlat= n_elements(lat)
 nslon = n_elements(slon)
 nslat= n_elements(slat)
 nlev = n_elements(lev)
 nilev = n_elements(ilev)
 PHIS    = get_fld_fnl2d('../obs/CAM/myctl_test.cam.h0.2008-07.nc','PHIS')

;=== get FNL data

 ncdf_vars,filein2,vars2
 x2=get_fld(filein2,'longitude')
 y2=get_fld(filein2,'latitude')
 plev  =indgen(17)*50+100  ; 100 to 900 mb
 plev = [1.,2.,3.,5.,7.,10.,20.,30.,50.,70.,plev, 925.,950.,975, 1000.]
 nx2 = n_elements(x2)
 ny2 = n_elements(y2)
 np2 = n_elements(plev)

 U2   = get_fld_fnl3d(filein2,'UGRD',plev)
 V2   = get_fld_fnl3d(filein2,'VGRD',plev)
 HGT2 = get_fld_fnl3d(filein2,'HGT',plev)
 T2   = get_fld_fnl3d(filein2,'TMP',plev)
 RH2  = get_fld_fnl3d(filein2,'RH',plev)

  plevw = plev[10:*]
 W2   = get_fld_fnl3d(filein2,'VVEL',plevw)
 CLDW2 = T2*0
 QS  = get_fld_fnl3d(filein2,'CLWMR',plevw)
 CLDW2[*,*,10:*] = QS

  plevo3 = plev[0:16]
 O3MR2= get_fld_fnl3d(filein2,'O3MR',plevo3)

 QS=T2*0
 QS = f_es(T2-273.16) & for k=0,np2-1 do QS[*,*,k]= 0.622*QS[*,*,k]/Plev[k]
 Q2  = RH2 * QS /100.

; get FNL 2D data

ICEFRAC2 = get_fld_fnl2d(filein2,'ICEC_surface')
LANDFRAC2= get_fld_fnl2d(filein2,'LAND_surface')
PHIS2    = get_fld_fnl2d(filein2,'HGT_surface')
PS2      = get_fld_fnl2d(filein2,'PRES_surface')
PSL2     = get_fld_fnl2d(filein2,'PRMSL_meansealevel')
SNOWD2   = get_fld_fnl2d(filein2,'SNOD_surface')
SNOWW2   = get_fld_fnl2d(filein2,'WEASD_surface')
SOILW1_2   = get_fld_fnl2d(filein2,'SOILW_0M0D1mbelowground')
SOILW2_2   = get_fld_fnl2d(filein2,'SOILW_0D1M0D4mbelowground')
SOILW3_2   = get_fld_fnl2d(filein2,'SOILW_1M2mbelowground')
SOILW4_2   = get_fld_fnl2d(filein2,'SOILW_0D4M1mbelowground')
TS_2      = get_fld_fnl2d(filein2,'TMP_2maboveground')
TS1_2      = get_fld_fnl2d(filein2,'TSOIL_0M0D1mbelowground')
TS2_2      = get_fld_fnl2d(filein2,'TSOIL_0D1M0D4mbelowground')
TS3_2      = get_fld_fnl2d(filein2,'TSOIL_1M2mbelowground')
TS4_2      = get_fld_fnl2d(filein2,'TSOIL_0D4M1mbelowground')

T80_2      = get_fld_fnl2d(filein2,'TMP_80maboveground')
T995_2      = get_fld_fnl2d(filein2,'TMP_0D995sigmalevel')

P80_2      = get_fld_fnl2d(filein2,'PRES_80maboveground')
P995_2      = 0.955* PS2 ;get_fld_fnl2d(filein2,'PRES_0D995sigmalevel')


RH995_2      = get_fld_fnl2d(filein2,'RH_0D995sigmalevel')  ;RH
Q80_2      = get_fld_fnl2d(filein2,'SPFH_80maboveground')

Q995_2 = 0.622*f_es(T995_2-273.16)/P995_2 *RH995_2 
RH80_2   = Q80_2/ ( 0.622*f_es(T80_2-273.16)/P80_2 ) ;%
jj1= where(RH80_2 gt 100.,cnt1)
jj2= where(RH80_2 le 10.0,  cnt2)
if(cnt1 gt 0)then RH80_2[jj1] = 100.
if(cnt2 gt 0)then RH80_2[jj2] = 10.0


U80_2      = get_fld_fnl2d(filein2,'UGRD_80maboveground')
U995_2      = get_fld_fnl2d(filein2,'UGRD_0D995sigmalevel')

V80_2      = get_fld_fnl2d(filein2,'VGRD_80maboveground')
V995_2      = get_fld_fnl2d(filein2,'VGRD_0D995sigmalevel')


;== inteoplolation of FNL(sub_2) PHIS to CAM:  sub_1 for camH fsnlV  or no sub for CAM
TS     = my_interpol2(TS_2,  x2,y2,lon,lat)
;stop
TS1   = my_interpol2(TS1_2,  x2,y2,lon,lat)
TS2   = my_interpol2(TS2_2,  x2,y2,lon,lat)
TS3   = my_interpol2(TS3_2,  x2,y2,lon,lat)
TS4   = my_interpol2(TS4_2,  x2,y2,lon,lat)
ICEFRAC   = my_interpol2(ICEFRAC2,  x2,y2,lon,lat)
SNOWD     = my_interpol2(SNOWD2,  x2,y2,lon,lat)

;  to augment the near-surface layer

T80   = my_interpol2(T80_2,x2,y2,lon,lat)
Q80   = my_interpol2(Q80_2,x2,y2,lon,lat)
RH80   = my_interpol2(RH80_2,x2,y2,lon,lat)
P80   = my_interpol2(P80_2,x2,y2,lon,lat)
U80   = my_interpol2(U80_2,x2,y2,lon,lat)
V80   = my_interpol2(V80_2,x2,y2,slon,lat)
PU80  = my_interpol2(P80_2,x2,y2,lon,slat)
PV80   = my_interpol2(P80_2,x2,y2,slon,lat)
US80   = my_interpol2(U80_2,x2,y2,lon,slat)
VS80   = my_interpol2(V80_2,x2,y2,slon,lat)

U995   = my_interpol2(U995_2,x2,y2,lon,lat)
V995   = my_interpol2(V995_2,x2,y2,lon,lat)
T995   = my_interpol2(T995_2,x2,y2,lon,lat)
Q995   = my_interpol2(Q995_2,x2,y2,lon,lat)/1000.
RH995   = my_interpol2(RH995_2,x2,y2,lon,lat)
P995   = my_interpol2(P995_2,x2,y2,lon,lat)
US995   = my_interpol2(U995_2,x2,y2,lon,slat)
VS995   = my_interpol2(V995_2,x2,y2,slon,lat)
PU995  = my_interpol2(P995_2,x2,y2,lon,slat)
PV995  = my_interpol2(P995_2,x2,y2,slon,lat)

; get PS with hydrostatic adj
PHIS1 = my_interpol2(PHIS2,x2,y2,lon,lat)  ; to distinuish from CAM PHIS
PS   = my_interpol2(PS2,  x2,y2,lon,lat)
dPHIS = PHIS - PHIS1*9.8
jj = where(dPHIS gt 1.0,cnt)
if(cnt gt 0)then PS[jj] = PS[jj] - (abs(dPHIS) /(287.3*T80) )
jj = where(dPHIS lt -1.,cnt)
if(cnt gt 0)then PS[jj] = PS[jj] + (abs(dPHIS) /(287.3*T80) )

PSU = my_interpol2(PS,lon,lat,lon,slat)
PSV = my_interpol2(PS,lon,lat,slon,lat)

; get P
P = fltarr(nlon,nlat,nlev)
PU = fltarr(nlon,nslat,nlev)
PV = fltarr(nslon,nlat,nlev)
P0 = 100000.
for kk=0,nlev-1 do begin
 P[*,*,kk]  = hyam[kk]*p0+hybm[kk]*ps[*,*]
 PU[*,*,kk] = hyam[kk]*p0+hybm[kk]*psu[*,*]
 PV[*,*,kk] = hyam[kk]*p0+hybm[kk]*psv[*,*]
endfor


;declare intermediate fields  ; U1, V1 on T grids for diagnostics only

; horizontally CAM, vertically FNL
U1 = fltarr(nlon,nlat,np2)
V1 = U1 & CLDICE1=U1 &  CLDLIQ=U1 &  NUMICE=U1 &  NUMLIQ=U1 & Q1=U1 & T1=U1
CLDW1 = U1 & RH1=U1
US1 = fltarr(nlon,nslat,np2)
VS1 = fltarr(nslon,nlat,np2)

; horizontal interpolation of intermediate fields
for kk=0,np2-1 do begin 
U1[*,*,kk]    = my_interpol2(reform(U2[*,*,kk]),x2,y2,lon,lat)
V1[*,*,kk]    = my_interpol2(reform(V2[*,*,kk]),x2,y2,lon,lat)
T1[*,*,kk]    = my_interpol2(reform(T2[*,*,kk]),x2,y2,lon,lat)
Q1[*,*,kk]    = my_interpol2(reform(Q2[*,*,kk]),x2,y2,lon,lat)
RH1[*,*,kk]    = my_interpol2(reform(RH2[*,*,kk]),x2,y2,lon,lat)
US1[*,*,kk]   = my_interpol2(reform(U2[*,*,kk]),x2,y2,lon,slat)
VS1[*,*,kk]   = my_interpol2(reform(V2[*,*,kk]),x2,y2,slon,lat)
CLDW1[*,*,kk] = my_interpol2(reform(CLDW2[*,*,kk]),x2,y2,slon,lat)
endfor

jj=where(RH1 le 10.,cnt)
if(cnt gt 0)then RH1[jj] = 10.0

;declare final cam fields
U = fltarr(nlon,nlat,nlev)
V = U & CLDICE =U &  CLDLIQ=U &  NUMICE=U &  NUMLIQ=U & Q=U & T=U & RH=U
CLDW = U*0
US = fltarr(nlon,nslat,nlev)
VS = fltarr(nslon,nlat,nlev)

plev= plev*100.

; vertical interpolation
for i=0,nlon-1 do begin
for j=0,nlat-1 do begin
 pp = reform(p[i,j,*])
  pw = [plev,              p995[i,j],  p80[i,j] ]
  dw = [reform(U1[i,j,*]), u995[i,j], u80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 U[i,j,*] = my_interpol(dw,pw,pp,dxh=2500.,noextra=1)  ;Pa 25 mb

  pw = [plev,              p995[i,j],  p80[i,j] ]
  dw = [reform(V1[i,j,*]), v995[i,j], v80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 V[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)

  pw = [plev,              p995[i,j],  p80[i,j] ]
  dw = [reform(T1[i,j,*]), T995[i,j], T80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 T[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)

  pw = [plev,              p995[i,j],  p80[i,j] ]
  dw = [reform(Q1[i,j,*]), Q995[i,j], Q80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 Q[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)

  pw = [plev,              p995[i,j],  p80[i,j] ]
  dw = [reform(RH1[i,j,*]), RH995[i,j], RH80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 RH[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)

  pw = plev
  dw = reform(CLDW1[i,j,*])
 CLDW[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)

  pw = [plev,              pv995[i,j],  pv80[i,j] ]
  dw = [reform(VS1[i,j,*]), vS995[i,j], vs80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 VS[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)
endfor ;j lat

for j=0,nslat-1 do begin
  pw = [plev,              pu995[i,j], pu80[i,j] ]
  dw = [reform(US1[i,j,*]), us995[i,j], us80[i,j] ]
  kk= sort(pw)
  pw = pw[kk]
  dw = dw[kk]
 US[i,j,*] = my_interpol(dw,pw,pp,dx=2500.,noextra=1)
endfor

endfor ;------ i, lon

; ready, U,V,T,Q,CLDW,US,VS
; ready, PS, TS, TS1,TS2,TS3,TS4

QS = 0.622*f_es(T-273.16)/P*100.
Q = RH*QS/100.
km = where(lev le 130.,cnt)
kmx = max(km)
if(cnt gt 0)then for k=0,kmx-1 do Q[*,*,k] = Q[*,*,kmx]
jj=where(Q lt 1.0e-6,cnt1) & if(cnt1 gt 0)then Q[jj] = 1.0e-6

FICE = T*0
jj=where(T gt -5+273.16,cnt1) & if(cnt1 gt 0)then FICE[jj] = 0.0
jj=where(T lt -20+273.16,cnt1) & if(cnt1 gt 0)then FICE[jj] = 1.0
jj=where((T gt -20+273.16) and (T le -20+273.16),cnt2)  
 if(cnt2 gt 0)then FICE[jj] = (T[jj]+5.-273.16)/15.

CLDLIQ = CLDW*(1-FICE)
CLDICE = CLDW*FICE


NUMLIQ = CLDLIQ*0 + 150.0e6   ; /cm-3
NUMICE = CLDLIQ*0 + 1.5e6     ; /cm-3

ICEFRAC = ICEFRAC
SICTHK  = ICEFRAC*0 +0.01  ;10 cm
SNOWHICE = SNOWD
TSICE    = TS             ;

; ready CLDLIQ CLDICE NUMLIQ NUMICE ICEFRAC SICTHK SNOWHICE TSICE


if(idiag)then begin

print,'phis1',min(phis1),max(phis1)
print,'ps2',min(ps2),max(ps2)
print,'tsf',min(ts_2),max(ts_2)
print,'ts1f',min(ts1_2),max(ts1_2)
print,'ts2f',min(ts2_2),max(ts2_2)
print,'ts3f',min(ts3_2),max(ts3_2)
print,'ts4f',min(ts4_2),max(ts4_2)
print,'t2',min(T2),max(T2)
print,'t1',min(T1),max(T1)
print,'t',min(T),max(T)
print,'Q',min(Q2),max(Q2)
print,'RH',min(RH2),max(RH2)
print,'U',min(U2),max(U2)
print,'V',min(V2),max(V2)


print,'phis',min(phis),max(phis)
print,'ps',min(ps),max(ps)
print,'ts',min(ts),max(ts)
print,'ts1',min(ts1),max(ts1)
print,'ts2',min(ts2),max(ts2)
print,'ts3',min(ts3),max(ts3)
print,'ts4',min(ts4),max(ts4)
print,'Q',min(Q),max(Q)
print,'RH',min(RH),max(RH)
print,'US',min(US),max(US)
print,'VS',min(VS),max(VS)


print,'t,q,rh,80',min(t80),max(t80),min(q80),max(q80),min(rh80),max(rh80)
print,'t,q,rh,995',min(t995),max(t995),min(q995),max(q995),min(rh995),max(rh995)

print,'t80_2',min(t80_2),max(t80_2)
print,'t995_2',min(t995_2),max(t995_2),min(q995_2),max(q995_2)

endif ;idiag

jj=where(TS1 lt 0., cnt) & if(cnt gt 0)then TS1[jj] =  TS[jj]
jj=where(TS2 lt 0., cnt) & if(cnt gt 0)then TS2[jj] =  TS[jj]
jj=where(TS3 lt 0., cnt) & if(cnt gt 0)then TS3[jj] =  TS[jj]
jj=where(TS4 lt 0., cnt) & if(cnt gt 0)then TS4[jj] =  TS[jj]
jj=where(Q lt 1.0e-6,cnt) & if(cnt gt 0)then Q[jj] = 1.0e-6
jj=where(CLDLIQ lt 1.0e-6,cnt) & if(cnt gt 0)then CLDLIQ[jj] =  0.
jj=where(CLDICE lt 1.0e-6,cnt) & if(cnt gt 0)then CLDICE[jj] =  0.
jj=where(SNOWHICE lt 1.0e-6,cnt) & if(cnt gt 0)then SNOWHICE[jj] = 0


print,'minimum values of CLDICE CLDLIQ ICEFRAC NUMICE NUMLIQ PS Q SICTHK RH'
print,min(CLDICE),min(CLDLIQ),min(ICEFRAC)
print,min(NUMICE), min(NUMLIQ), min(PS)
print,min(Q), min(SICTHK)
print, 'SNOWHICE T TS1 TS2 TS3 TS4 TSICE US VS'
print,min(SNOWHICE),min(T), min(TS1), min(TS2)
print,min(TS3), min(TS4), min(TSICE )
print,min(US), min(VS),min(RH)

print,'maximum values of CLDICE CLDLIQ ICEFRAC NUMICE NUMLIQ PS Q SICTHK '
print,max(CLDICE),max(CLDLIQ),max(ICEFRAC)
print,max(NUMICE), max(NUMLIQ), max(PS)
print,max(Q), max(SICTHK)
print, 'SNOWHICE T TS1 TS2 TS3 TS4 TSICE US VS,RH'
print,max(SNOWHICE),max(T), max(TS1), max(TS2)
print,max(TS3), max(TS4), max(TSICE )
print,max(US), max(VS), MAX(RH)


;=================================
fileID=ncdf_open(cam_filenew,/write)
NCDF_CONTROL, fileid, /FILL
;-------
VarID = NCDF_VARID(fileID, 'CLDICE')
;;;cdf_varput,fileID, varID,CLDICE
;-------
VarID = NCDF_VARID(fileID, 'CLDLIQ')
;;;cdf_varput,fileID, varID,CLDLIQ
;-------
VarID = NCDF_VARID(fileID, 'ICEFRAC')
;;;cdf_varput,fileID, varID,ICEFRAC
;-------
VarID = NCDF_VARID(fileID, 'NUMICE')
ncdf_varput,fileID, varID,NUMICE
;;;;-------
VarID = NCDF_VARID(fileID, 'NUMLIQ')
;;;ncdf_varput,fileID, varID,NUMLIQ
;-------
VarID = NCDF_VARID(fileID, 'PS')
ncdf_varput,fileID, varID,PS
;-------
VarID = NCDF_VARID(fileID, 'Q')
ncdf_varput,fileID, varID,Q
;-------
VarID = NCDF_VARID(fileID, 'SICTHK')
ncdf_varput,fileID, varID,SICTHK
;-------
VarID = NCDF_VARID(fileID, 'SNOWHICE')
ncdf_varput,fileID, varID,SNOWHICE
;-------
VarID = NCDF_VARID(fileID, 'T')
ncdf_varput,fileID, varID,T
;-------
VarID = NCDF_VARID(fileID, 'TS1')
ncdf_varput,fileID, varID,TS1
;-------
VarID = NCDF_VARID(fileID, 'TS2')
ncdf_varput,fileID, varID,TS2
;-------
VarID = NCDF_VARID(fileID, 'TS3')
ncdf_varput,fileID, varID,TS3
;-------
VarID = NCDF_VARID(fileID, 'TS4')
ncdf_varput,fileID, varID,TS4
;-------
VarID = NCDF_VARID(fileID, 'TSICE')
ncdf_varput,fileID, varID,TSICE
;-------
VarID = NCDF_VARID(fileID, 'US')
ncdf_varput,fileID, varID,US
;-------
VarID = NCDF_VARID(fileID, 'VS')
ncdf_varput,fileID, varID,VS
;-------
VarID = NCDF_VARID(fileID, 'date')
ncdf_varput,fileID, varID,date
;-------
VarID = NCDF_VARID(fileID, 'nbdate')
ncdf_varput,fileID, varID,date
;-------
VarID = NCDF_VARID(fileID, 'nsbase')
ncdf_varput,fileID, varID,0
;-------
VarID = NCDF_VARID(fileID, 'ndbase')
ncdf_varput,fileID, varID, 0.0
;-------
VarID = NCDF_VARID(fileID, 'time')
ncdf_varput,fileID, varID, 0
;-------
VarID = NCDF_VARID(fileID, 'ndcur')
ncdf_varput,fileID, varID, 0
;-------
VarID = NCDF_VARID(fileID, 'nsteph')
ncdf_varput,fileID, varID, 0
;-------

ncdf_close,fileID

print,'saved file:' ,cam_filenew

end

