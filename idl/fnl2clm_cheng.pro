
clm_fileold= '../obs/FNL/Cheng/clmi.BCN.2000-01-01_0.9x1.25_gx1v6_simyr2000_c100303.nc'
clm_r   = '../obs/FNL/Cheng/20111001_new.clm2.r.2012-10-01.nc'

date=20111001
fnl_file   = '../obs/FNL/Cheng/fnl_20111001_00_00.nc'
clm_filenew= '../obs/FNL/Cheng/INIT/clmi.BCN.2000-01-01_0.9x1.25_gx1v6_simyr2011-10-01b.nc'

date=20111005
fnl_file   = '../obs/FNL/Cheng/fnl_20111005_00_00.nc'
clm_filenew= '../obs/FNL/Cheng/INIT/clmi.BCN.2000-01-01_0.9x1.25_gx1v6_simyr2011-10-05b.nc'


idiag = 1

SPAWN, 'cp '+ clm_fileold+' '+clm_filenew

filein = clm_filenew
filein2 = fnl_file
fileinr = clm_r

;;goto,jump1

 print,'to rewrite ',filein
 print,'use data from ',filein

;=== get FNL data
 lon = get_fld(filein2,'lon_0')
 lat = get_fld(filein2,'lat_0')

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

TS_2 = TS_2m

TS1_2     = reform(TSS[*,*,0])
TS2_2     = reform(TSS[*,*,1])
TS3_2     = reform(TSS[*,*,2])
TS4_2     = reform(TSS[*,*,3])
T995_2      = get_fld_fnl2d(filein2,'TMP_P0_L104_GLL0')


;=== get clm info as target

 ncdf_vars,filein,vars

 grid1d_lon = get_fld(filein,'grid1d_lon')
 grid1d_lat = get_fld(filein,'grid1d_lat')
 land1d_lon = get_fld(filein,'land1d_lon')
 land1d_lat = get_fld(filein,'land1d_lat')
 cols1d_lon = get_fld(filein,'cols1d_lon')
 cols1d_lat = get_fld(filein,'cols1d_lat')
 pfts1d_lon = get_fld(filein,'pfts1d_lon')
 pfts1d_lat = get_fld(filein,'pfts1d_lat')


; supplemental
  TS_GRND= my_inter_clm(TS_2,lon,lat,cols1d_lon,cols1d_lat,dx=1.)

 T_GRND = get_fld(filein,'T_GRND')
  dj= my_inter_clm(TS1_2,lon,lat,cols1d_lon,cols1d_lat,ZZ= TS_GRND)
  T_GRND = dj
 T_REF2M = get_fld(filein,'T_REF2M')
   T_REF2M = my_inter_clm(TS_2,lon,lat,pfts1d_lon, pfts1d_lat)
 T_REF2M_MIN = T_REF2M
 T_REF2M_MAX = T_REF2M
 T_REF2M_MIN_INST = T_REF2M
 T_REF2M_MAX_INST = T_REF2M

 T_GRND_U = T_GRND
 T_REF2M_U = T_REF2M
 T_REF2M_MIN_U = T_REF2M_MIN
 T_REF2M_MAX_U = T_REF2M_MAX
 T_REF2M_MIN_INST_U = T_REF2M_MIN_INST
 T_REF2M_MAX_INST_U = T_REF2M_MAX_INST

 T_GRND_R = T_GRND
 T_REF2M_R = T_REF2M
 T_REF2M_MIN_R = T_REF2M_MIN
 T_REF2M_MAX_R = T_REF2M_MAX
 T_REF2M_MIN_INST_R = T_REF2M_MIN_INST
 T_REF2M_MAX_INST_R = T_REF2M_MAX_INST

 T_SOISNO = get_fld(filein,'T_SOISNO')
 dj = T_GRND 
 jj = where(dj gt 273.16,cnt)
 if(cnt gt 0)then dj[jj] = 273.16
;  dj1= my_inter_clm(TS1_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
;  dj2= my_inter_clm(TS2_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
;  dj3= my_inter_clm(TS3_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
;  dj4= my_inter_clm(TS4_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
 nj = n_elements(T_SOISNO[*,0])
 for k=0,nj-1 do  T_SOISNO[k,*] = dj


 T_LAKE = get_fld(filein,'T_LAKE')
 for k=0,n_elements(T_LAKE[*,0])-1 do begin
  T_LAKE[k,*] = T_SOISNO[0,*]
 endfor

 T_VEG = T_REF2M
 T_VEG24_VALUE = T_REF2M
 T_VEG240_VALUE = T_REF2M

; ready to write 
; DZSNO,H2OSNO,H2OSOI_LIQ,T_GRND,$
; T_REF2M_MIN,T_REF2M_MAX,T_REF2M_MIN_INST,T_REF2M_MAX_INST 
; T_GRND_U, T_REF2M_MIN_U, ...
; T_GRND_R, T_REF2M_MIN_R,...
; T_SOISNO, T_LAKE,T_VEG,T_VEG24_VALUE,T_VEG240_VALUE


print,'minimum values of T_GRND'
print,min(T_GRND)
print, 'minimum values of T_SOISNO T_REF2M'
print,min(T_SOISNO),min(T_REF2M)

print,'maximum values of T_GRND'
print,max(T_GRND)
print, 'maximum values of T_SOISNO T_REF2M'
print,max(T_SOISNO),max(T_REF2M)

print,'write initial clm file?', clm_filenew
read,ix

jump1:

;=================================
fileID=ncdf_open(clm_filenew,/write)
NCDF_CONTROL, fileid, /FILL
;-------
VarID = NCDF_VARID(fileID, 'T_GRND')
ncdf_varput,fileID, varID,T_GRND
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MIN')
ncdf_varput,fileID, varID,T_REF2M_MIN      ;!!T REF
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MAX')
ncdf_varput,fileID, varID,T_REF2M_MAX
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MIN_INST')
ncdf_varput,fileID, varID,T_REF2M_MIN_INST
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MAX_INST')
ncdf_varput,fileID, varID,T_REF2M_MAX_INST
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M')      ;!!T
ncdf_varput,fileID, varID,T_REF2M

;-------
VarID = NCDF_VARID(fileID, 'TREFAV_R_VALUE')  ;!! new
ncdf_varput,fileID, varID,T_REF2M
;-------
;-------
VarID = NCDF_VARID(fileID, 'T_SOISNO')
ncdf_varput,fileID, varID,T_SOISNO         ;!!TSOIL
;-------
VarID = NCDF_VARID(fileID, 'T_LAKE')
ncdf_varput,fileID, varID,T_LAKE
;-------
VarID = NCDF_VARID(fileID, 'T_VEG')
ncdf_varput,fileID, varID,T_VEG
;-------

;==============================
;
;VARI = SNOWDP,WA,WT,ZWT,frac_sno,DZSNO,ZSNO,ZISNO,H2OSNO,H2OSOI_LIQ,H2OSOI_ICE
;xx VARI = ['SNOWDP','WA','WT','ZWT','frac_sno','DZSNO',$
;xx         'ZSNO','ZISNO','H2OSNO','H2OSOI_LIQ','H2OSOI_ICE' ] ;, $
;;        'T_GRND_U','T_REF2M_MIN_U','T_REF2M_MAX_U','T_REF2M_MIN_INST_U',]
for iv=0,n_elements(VARI)-1 do begin
 var = vari[iv]
 DD  = get_fld(fileinr,VAR)
 VarID = NCDF_VARID(fileID, VAR)
;; ncdf_varput,fileID, varID, DD
endfor
;==============================
;-------

VarID = NCDF_VARID(fileID, 'timemgr_rst_start_ymd')
ncdf_varput,fileID, varID, date
;-------
VarID = NCDF_VARID(fileID, 'timemgr_rst_start_tod')
ncdf_varput,fileID, varID,0
;-------
VarID = NCDF_VARID(fileID, 'timemgr_rst_ref_ymd')
ncdf_varput,fileID, varID, date
;-------
VarID = NCDF_VARID(fileID, 'timemgr_rst_ref_tod')
ncdf_varput,fileID, varID,0
;-------
VarID = NCDF_VARID(fileID, 'timemgr_rst_curr_ymd')
ncdf_varput,fileID, varID,date
;-------
VarID = NCDF_VARID(fileID, 'timemgr_rst_curr_tod')
ncdf_varput,fileID, varID, 0
;-------
VarID = NCDF_VARID(fileID, 'mcdate')
ncdf_varput,fileID, varID,date
;-------
VarID = NCDF_VARID(fileID, 'mcsec')
ncdf_varput,fileID, varID,0
;-------
ncdf_close,fileID

print,'saved file:' ,clm_filenew

;on initial data file

;; timemgr_rst_start_ymd
;; timemgr_rst_start_tod
;; timemgr_rst_ref_ymd
;; timemgr_rst_ref_tod
;; timemgr_rst_curr_ymd
;; timemgr_rst_curr_tod
;; mcdate
;; mcsec


;; grid1d_lon(gridcell)
;; grid1d_lat(gridcell)
;; grid1d_ixy(gridcell)
;; grid1d_jxy(gridcell)

;; land1d_lon(landunit)
;; land1d_lat(landunit)
;; land1d_ixy(landunit)
;; land1d_jxy(landunit)
;; land1d_gi(landunit)
;; land1d_wtxy(landunit)
;; land1d_ityplun(landunit)

;; cols1d_lon(column)
;; cols1d_lat(column)
;; cols1d_ixy(column)
;; cols1d_jxy(column)
;; cols1d_gi(column)
;; cols1d_wtxy(column)
;; cols1d_wtlnd(column)
;; cols1d_ityplun(column)

;; SNLSNO(column)
;; SNOWDP(column)
;; WA(column) 
;; WT(column)
;; frac_sno(column)
;; DZSNO(column, levsno) 
;; H2OSNO(column)
;; H2OSOI_LIQ(column, levtot)
;; T_GRND(column)
;; T_REF2M_MIN(pft)
;; T_REF2M_MAX(pft)
;; T_REF2M_MIN_INST(pft)
;; T_REF2M_MAX_INST(pft)
;; T_REF2M_U(pft)
;; T_GRND_U(column)
;; T_REF2M_MIN_U(pft)
;; T_REF2M_MAX_U(pft)
;; T_REF2M_MIN_INST_U(pft)
;; T_REF2M_MAX_INST_U(pft)
;;  T_REF2M_R(pft)
;; T_GRND_R(column)
;; T_REF2M_MIN_R(pft)
;; T_REF2M_MAX_R(pft)
;; T_REF2M_MIN_INST_R(pft)
;; T_REF2M_MAX_INST_R(pft)
;; T_SOISNO(column, levtot)
;; T_LAKE(column, levlak) 
;; T_VEG(pft) 
;; T_REF2M(pft)
;; TREFAV_R_VALUE(pft) 
;; T_VEG24_VALUE(pft)
;; T_VEG240_VALUE(pft)

print, ''
print, 'verify against original data:'
H2OSNO = get_fld(filein,'H2OSNO')
H2OSOI_LIQ= get_fld(filein,'H2OSOI_LIQ')
T_GRND= get_fld(filein,'T_GRND')
T_SOISNO= get_fld(filein,'T_SOISNO')
T_REF2M= get_fld(filein,'T_REF2M')

print,'minimum values of H2OSNO,H2OSOI_LIQ,T_GRND'
print,min(H2OSNO),min(H2OSOI_LIQ),min(T_GRND)
print, 'minimum values of T_SOISNO T_REF2M'
print,min(T_SOISNO),min(T_REF2M)

print,'maximum values of H2OSNO,H2OSOI_LIQ,T_GRND'
print,max(H2OSNO),max(H2OSOI_LIQ),max(T_GRND)
print, 'maximum values of T_SOISNO T_REF2M'
print,max(T_SOISNO),max(T_REF2M)


end

