
cam_fileold= '../obs/FNL/Cheng/cami-mam3_0000-01-01_0.9x1.25_L30_c100618.nc'
cam_h   = '../obs/FNL/Cheng/FC5_test_2011.cam.h0.2011-10-10-00000.nc'
cam_r   = '../obs/FNL/Cheng/20111001_new.cam.r.2012-10-01.nc'

date=20111001
fnl_file   = '../obs/FNL/Cheng/fnl_20111001_00_00.nc'
cam_filenew= '../obs/FNL/Cheng/INIT/cami-mam3_2011-10-01_0.9x1.25_L30_c100618.nc

date=20111005
fnl_file   = '../obs/FNL/Cheng/fnl_20111005_00_00.nc'
cam_filenew= '../obs/FNL/Cheng/INIT/cami-mam3_2011-10-05_0.9x1.25_L30_c100618.nc

idiag = 1

SPAWN, 'cp '+ cam_fileold+' '+cam_filenew

filein = cam_filenew
filein2 = fnl_file
fileinr = cam_r
fileinh = cam_h

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
 hyai = get_fld(filein,'hyai')
 hybi = get_fld(filein,'hybi')
 lev  = get_fld(filein,'lev')
 ilev = get_fld(filein,'ilev')
 P0 = 1000.
 nlon = n_elements(lon)
 nlat= n_elements(lat)
 nslon = n_elements(slon)
 nslat= n_elements(slat)
 nlev = n_elements(lev)
 nilev = n_elements(ilev)
 PHIS    = get_fld_fnl2d(fileinh,'PHIS')

;=== get FNL data

 ncdf_vars,filein2,vars2
 ;x2=get_fld(filein2,'longitude')
 ;y2=get_fld(filein2,'latitude')
 x2=get_fld(filein2,'lon_0')
 y2=get_fld(filein2,'lat_0')

 plev  =get_fld(filein2,'lv_ISBL0') 
 plev7  =get_fld(filein2,'lv_ISBL7') 
 plev6  =get_fld(filein2,'lv_ISBL6') 
 plev4  =get_fld(filein2,'lv_ISBL4') 
 siglev1 = get_fld(filein2,'lv_SIGL5_l1')
 siglev0 = get_fld(filein2,'lv_SIGL5_l0')
 alt1 = get_fld(filein2,'lv_AMSL1')

 nx2 = n_elements(x2)
 ny2 = n_elements(y2)
 np2 = n_elements(plev)

 U2   = get_fld(filein2,'UGRD_P0_L100_GLL0')
 V2   = get_fld(filein2,'VGRD_P0_L100_GLL0')
 HGT2 = get_fld(filein2,'HGT_P0_L100_GLL0')
 T2   = get_fld(filein2,'TMP_P0_L100_GLL0')
 RHJ  = get_fld(filein2,'RH_P0_L100_GLL0')  ; 21 levels
 RH2  = T2*0
 RH2[*,*,5:np2-1] = RHJ
 for k=0,4 do RH2[*,*,k] = RHJ[*,*,5]
; O32   = get_fld(filein2,'O3MR_P0_L100_GLL0')
 
 QS=T2*0
 ES = f_es(T2-273.16) 
 for k=0,np2-1 do QS[*,*,k]= 0.622*ES[*,*,k]/(Plev[k]/100.)
 Q2  = RH2 * QS /100.
 for k=21,np2-1 do Q2[*,*,k] = Q2[*,*,20]

; get FNL 2D data

ICEFRAC2 = get_fld_fnl2d(filein2,'ICEC_P0_L1_GLL0')
LANDFRAC2= get_fld_fnl2d(filein2,'LAND_P0_L1_GLL0')
PHIS2    = get_fld_fnl2d(filein2,'HGT_P0_L1_GLL0') *9.8 ;!!!
PS2      = get_fld_fnl2d(filein2,'PRES_P0_L1_GLL0')
PSL2     = get_fld_fnl2d(filein2,'PRES_P0_L101_GLL0')
;SNOWD2   = get_fld_fnl2d(filein2,'SNOD_surface')
SNOWW2   = get_fld_fnl2d(filein2,'WEASD_P0_L1_GLL0')
SOILS    = get_fld(filein2,'SOILW_P0_2L106_GLL0')
SOILW1_2   = reform(SOILS[*,*,0])
SOILW2_2   = reform(SOILS[*,*,1])
SOILW3_2   = reform(SOILS[*,*,2])
SOILW4_2   = reform(SOILS[*,*,3])

TG_2      = get_fld_fnl2d(filein2,'TMP_P0_L1_GLL0')
TS_2m     = get_fld_fnl2d(filein2,'TMP_P0_L103_GLL0')
RHS_2m     = get_fld_fnl2d(filein2,'RH_P0_L103_GLL0')
TSS       = get_fld_fnl2d(filein2,'TMP_P0_2L106_GLL0')

for kk=0,3 do begin
  dj = reform(TSS[*,*,kk])
  jj=where (dj le 100.,cnt)
  if(cnt gt 0)then begin
    dj[jj] = Tg_2[jj]
  endif
  TSS[*,*,kk] = dj
endfor

TS1_2     = reform(TSS[*,*,0])
TS2_2     = reform(TSS[*,*,1])
TS3_2     = reform(TSS[*,*,2])
TS4_2     = reform(TSS[*,*,3])

; 'UGRD_P0_L103_GLL0' ; 10 meter wind

;T80_2      = get_fld_fnl2d(filein2,'TMP_80maboveground')
T995_2      = get_fld_fnl2d(filein2,'TMP_P0_L104_GLL0')

;Q80_2      = get_fld_fnl2d(filein2,'SPFH_80maboveground')
RH995_2      = get_fld_fnl2d(filein2,'RH_P0_L104_GLL0')

;U80_2      = get_fld_fnl2d(filein2,'UGRD_80maboveground')
U995_2      = get_fld_fnl2d(filein2,'UGRD_P0_L104_GLL0')

;V80_2      = get_fld_fnl2d(filein2,'VGRD_80maboveground')
V995_2      = get_fld_fnl2d(filein2,'VGRD_P0_L104_GLL0')

;P80_2      = get_fld_fnl2d(filein2,'PRES_80maboveground')
P995_2      = 0.955* PS2 ;get_fld_fnl2d(filein2,'PRES_0D995sigmalevel')

Q995_2 = 0.622*f_es(T995_2-273.16)/P995_2 *RH995_2 

;== inteoplolation of FNL(sub_2) PHIS to CAM:  sub_1 for camH fsnlV  or no sub for CAM
TG     = my_interpol2(Tg_2,  x2,y2,lon,lat)
TS     = my_interpol2(TS_2m,  x2,y2,lon,lat)
;stop
TS1   = my_interpol2(TS1_2,  x2,y2,lon,lat)
TS2   = my_interpol2(TS2_2,  x2,y2,lon,lat)
TS3   = my_interpol2(TS3_2,  x2,y2,lon,lat)
TS4   = my_interpol2(TS4_2,  x2,y2,lon,lat)
ICEFRAC   = my_interpol2(ICEFRAC2,  x2,y2,lon,lat)
;SNOWD     = my_interpol2(SNOWD2,  x2,y2,lon,lat)

;  to augment the near-surface layer

;T80   = my_interpol2(T80_2,x2,y2,lon,lat)
;Q80   = my_interpol2(Q80_2,x2,y2,lon,lat)
;P80   = my_interpol2(P80_2,x2,y2,lon,lat)
;U80   = my_interpol2(U80_2,x2,y2,lon,lat)
;V80   = my_interpol2(V80_2,x2,y2,slon,lat)
;PU80  = my_interpol2(P80_2,x2,y2,lon,slat)
;PV80   = my_interpol2(P80_2,x2,y2,slon,lat)
;US80   = my_interpol2(U80_2,x2,y2,lon,slat)
;VS80   = my_interpol2(V80_2,x2,y2,slon,lat)

U995   = my_interpol2(U995_2,x2,y2,lon,lat)
V995   = my_interpol2(V995_2,x2,y2,lon,lat)
T995   = my_interpol2(T995_2,x2,y2,lon,lat)
Q995   = my_interpol2(Q995_2,x2,y2,lon,lat) ;/1000.
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
if(cnt gt 0)then PS[jj] = PS[jj] - (abs(dPHIS) /(287.3*T995))
jj = where(dPHIS lt -1.,cnt)
if(cnt gt 0)then PS[jj] = PS[jj] + (abs(dPHIS) /(287.3*T995) )

PSU = my_interpol2(PS,lon,lat,lon,slat)
PSV = my_interpol2(PS,lon,lat,slon,lat)

; get P
P = fltarr(nlon,nlat,nlev)
PI = fltarr(nlon,nlat,nlev+1)
PU = fltarr(nlon,nslat,nlev)
PV = fltarr(nslon,nlat,nlev)
P0 = 100000.

PS1 = get_fld(filein,'PS')

for kk=0,nlev-1 do begin
 P[*,*,kk]  = hyam[kk]*p0+hybm[kk]*ps[*,*]
 PI[*,*,kk]  = hyai[kk]*p0+hybi[kk]*ps[*,*]
 PU[*,*,kk] = hyam[kk]*p0+hybm[kk]*psu[*,*]
 PV[*,*,kk] = hyam[kk]*p0+hybm[kk]*psv[*,*]
endfor
 kk = nlev
 PI[*,*,kk]  = hyai[kk]*p0+hybi[kk]*ps[*,*]

 DPI = P*0
 for kk=0,nlev-1 do DPI[*,*,kk] = PI[*,*,kk+1] - PI[*,*,kk] 
 DELP = DPI


;declare intermediate fields  ; U1, V1 on T grids for diagnostics only

; horizontally CAM, vertically FNL
U1 = fltarr(nlon,nlat,np2)
V1 = U1 & CLDICE1=U1 &  CLDLIQ=U1 &  NUMICE=U1 &  NUMLIQ=U1 & Q1=U1 & T1=U1
CLDW1 = U1
US1 = fltarr(nlon,nslat,np2)
VS1 = fltarr(nslon,nlat,np2)
RH1 = U1

; horizontal interpolation of intermediate fields
for kk=0,np2-1 do begin 
U1[*,*,kk]    = my_interpol2(reform(U2[*,*,kk]),x2,y2,lon,lat)
V1[*,*,kk]    = my_interpol2(reform(V2[*,*,kk]),x2,y2,lon,lat)
T1[*,*,kk]    = my_interpol2(reform(T2[*,*,kk]),x2,y2,lon,lat)
Q1[*,*,kk]    = my_interpol2(reform(Q2[*,*,kk]),x2,y2,lon,lat)
RH1[*,*,kk]    = my_interpol2(reform(RH2[*,*,kk]),x2,y2,lon,lat)
US1[*,*,kk]   = my_interpol2(reform(U2[*,*,kk]),x2,y2,lon,slat)
VS1[*,*,kk]   = my_interpol2(reform(V2[*,*,kk]),x2,y2,slon,lat)
;CLDW1[*,*,kk] = my_interpol2(reform(CLDW2[*,*,kk]),x2,y2,slon,lat)
endfor

;declare final cam fields
U = fltarr(nlon,nlat,nlev)
V = U & CLDICE =U &  CLDLIQ=U &  NUMICE=U &  NUMLIQ=U & Q=U & T=U & RH=U
CLDW = U*0
US = fltarr(nlon,nslat,nlev)
VS = fltarr(nslon,nlat,nlev)

;plev= plev*100.

; vertical interpolation
for i=0,nlon-1 do begin
for j=0,nlat-1 do begin
 pp = reform(p[i,j,*])
  pw = [plev,              p995[i,j]]
  dw = [reform(U1[i,j,*]), u995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 U[i,j,*] = my_interpol(dw,pw,pp,dxh=3500.,noextra=1)  ;Pa 25 mb

  pw = [plev,              p995[i,j] ]
  dw = [reform(V1[i,j,*]), v995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 V[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)

  pw = [plev,              p995[i,j]]
  dw = [reform(T1[i,j,*]), T995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 T[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)

  pw = [plev,              p995[i,j]]
  dw = [reform(Q1[i,j,*]), Q995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 Q[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)

  pw = [plev,              p995[i,j]]
  dw = [reform(RH1[i,j,*]), RH995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 RH[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)

;  pw = plev
;  dw = reform(CLDW1[i,j,*])
; CLDW[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)

  pw = [plev,              pv995[i,j]]
  dw = [reform(VS1[i,j,*]), vS995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 VS[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)
endfor ;j lat

for j=0,nslat-1 do begin
  pw = [plev,              pu995[i,j]]
  dw = [reform(US1[i,j,*]), us995[i,j]]
  kk= sort(pw) 
  pw = pw[kk]
  dw = dw[kk]
 US[i,j,*] = my_interpol(dw,pw,pp,dx=3500.,noextra=1)
endfor

endfor ;------ i, lon

;stop

; ready, U,V,T,Q,CLDW,US,VS
; ready, PS, TS, TS1,TS2,TS3,TS4


QS = 0.622*f_es(T-273.16)/P*100.

Q  = RH*QS/100.
for k=0,8 do begin
 Q[*,*,k] = Q[*,*,9]
endfor

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
;SICTHK  = ICEFRAC*0 +0.01  ;10 cm
;SNOWHICE = SNOWD
TSICE    = TS             ;
TBOT    = T995            ;

; ready CLDLIQ CLDICE NUMLIQ NUMICE ICEFRAC SICTHK SNOWHICE TSICE


if(idiag)then begin

print,'phis1',min(phis1),max(phis1)
print,'ps2',min(ps2),max(ps2)
print,'tsf',min(ts_2m),max(ts_2m)
print,'ts1f',min(ts1_2),max(ts1_2)
print,'ts2f',min(ts2_2),max(ts2_2)
print,'ts3f',min(ts3_2),max(ts3_2)
print,'ts4f',min(ts4_2),max(ts4_2)
print,'t2',min(T2),max(T2)
print,'t1',min(T1),max(T1)
print,'t',min(T2),max(T2)
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
print,'t',min(T),max(T)
print,'Q',min(Q),max(Q)
print,'RH',min(RH),max(RH)
print,'US',min(US),max(US)
print,'VS',min(VS),max(VS)

;print,'t80',min(t80),max(t80)
print,'t995',min(t995),max(t995)

;print,'t80_2',min(t80_2),max(t80_2)
print,'t995_2',min(t995_2),max(t995_2)

endif ;idiag

jj=where(TS1 lt 0., cnt) & if(cnt gt 0)then TS1[jj] =  TS[jj]
jj=where(TS2 lt 0., cnt) & if(cnt gt 0)then TS2[jj] =  TS[jj]
jj=where(TS3 lt 0., cnt) & if(cnt gt 0)then TS3[jj] =  TS[jj]
jj=where(TS4 lt 0., cnt) & if(cnt gt 0)then TS4[jj] =  TS[jj]
jj=where(Q lt 1.0e-6,cnt) & if(cnt gt 0)then Q[jj] = 1.0e-6
jj=where(CLDLIQ lt 1.0e-6,cnt) & if(cnt gt 0)then CLDLIQ[jj] =  0.
jj=where(CLDICE lt 1.0e-6,cnt) & if(cnt gt 0)then CLDICE[jj] =  0.
SNOWHICE = CLDLIQ*0
SICTHK   = CLDLIQ*0  

jj=where(SNOWHICE lt 1.0e-6,cnt) & if(cnt gt 0)then SNOWHICE[jj] = 0


print,'minimum values of CLDICE CLDLIQ ICEFRAC NUMICE NUMLIQ PS Q SICTHK RH'
print,min(CLDICE),min(CLDLIQ),min(ICEFRAC)
print,min(NUMICE), min(NUMLIQ), min(PS)
print,min(Q), min(SICTHK)
print, 'SNOWHICE T TS1 TS2 TS3 TS4 TSICE US VS'
print,min(SNOWHICE),min(T), min(TS1), min(TS2)
print,min(TS3), min(TS4), min(TSICE )
print,min(US), min(VS),min(RH),min(Q)
print,min(US), min(VS),min(RH2),min(Q2)

print,'maximum values of CLDICE CLDLIQ ICEFRAC NUMICE NUMLIQ PS Q SICTHK '
print,max(CLDICE),max(CLDLIQ),max(ICEFRAC)
print,max(NUMICE), max(NUMLIQ), max(PS)
print,max(Q), max(SICTHK)
print, 'SNOWHICE T TS1 TS2 TS3 TS4 TSICE US VS,RH'
print,max(SNOWHICE),max(T), max(TS1), max(TS2)
print,max(TS3), max(TS4), max(TSICE )
print,max(US), max(VS), MAX(RH),max(Q)
print,max(US), max(VS), MAX(RH2),max(Q2)


;=================================
fileID=ncdf_open(cam_filenew,/write)
NCDF_CONTROL, fileid, /FILL
;-------
;=====================================
VarID = NCDF_VARID(fileID, 'PS')
ncdf_varput,fileID, varID,PS
;-------
VarID = NCDF_VARID(fileID, 'Q')
ncdf_varput,fileID, varID,Q
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
VarID = NCDF_VARID(fileID, 'TBOT')
ncdf_varput,fileID, varID,TBOT
;-------
VarID = NCDF_VARID(fileID, 'TSICE')
ncdf_varput,fileID, varID,TSICE
;-------
VarID = NCDF_VARID(fileID, 'TSICERAD')
ncdf_varput,fileID, varID,TSICE
;-------
VarID = NCDF_VARID(fileID, 'TSOCN')
ncdf_varput,fileID, varID,TG
;-------
VarID = NCDF_VARID(fileID, 'US')
ncdf_varput,fileID, varID,US
;-------
VarID = NCDF_VARID(fileID, 'VS')
ncdf_varput,fileID, varID,VS
;-------
VarID = NCDF_VARID(fileID, 'DELP')
ncdf_varput,fileID, varID,DELP
;-------
; ----------------- unnecessary if history tape is at the same time
VARI = ['CLDLIQ','CLDICE','NUMLIQ','NUMICE']
for iv=0,n_elements(VARI)-1 do begin
 var = vari[iv]
 DD  = get_fld(fileinr,VAR)
 VarID = NCDF_VARID(fileID, VAR)
 ncdf_varput,fileID, varID, DD
endfor


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
;-------
;VarID = NCDF_VARID(fileID, 'CLDICE')
;;;cdf_varput,fileID, varID,CLDICE
;-------
;VarID = NCDF_VARID(fileID, 'CLDLIQ')
;;;cdf_varput,fileID, varID,CLDLIQ
;-------
;VarID = NCDF_VARID(fileID, 'ICEFRAC')
;;;cdf_varput,fileID, varID,ICEFRAC
;-------
;VarID = NCDF_VARID(fileID, 'NUMICE')
;;;ncdf_varput,fileID, varID,NUMICE
;;;;-------
;VarID = NCDF_VARID(fileID, 'NUMLIQ')
;;;ncdf_varput,fileID, varID,NUMLIQ
;-------
;VarID = NCDF_VARID(fileID, 'SICTHK')
;ncdf_varput,fileID, varID,SICTHK
;-------
;VarID = NCDF_VARID(fileID, 'SNOWHICE')
;ncdf_varput,fileID, varID,SNOWHICE

ncdf_close,fileID

print,'saved file:' ,cam_filenew

end

