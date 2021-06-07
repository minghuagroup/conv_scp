
;clm_fileold = '../obs/INIT/clmi.BCN.2000-01-01_1.9x2.5_gx1v6_simyr2000_c100309.nc'
clm_fileold = '../obs/INIT/myctl_test.clm2.r.2008-07-01-00000.nc'
clm_filenew = '../obs/INIT/myinput/my_clm_init_temp_20170727_00_00.nc'

;clm_fileold = '../temp/test2i.clm2.r.2017-07-28-00000.nc'
;clm_filenew = '../temp/my_clm_init_temp_20170727_00_00.nc'
fnl_file    = '../obs/FNL/fnl_20170727_00_00.nc'

date=20170727
;date=20170101
;idiag = 1

SPAWN, 'cp '+ clm_fileold+' '+clm_filenew

filein = clm_filenew
filein2 = fnl_file

;;goto,jump1

 print,'to rewrite ',filein
 print,'use data from ',filein

;=== get FNL data
 lon = get_fld(filein2,'longitude')
 lat = get_fld(filein2,'latitude')

; plev  =indgen(17)*50+100  ; 100 to 900 mb
; plev = [1.,2.,3.,5.,7.,10.,20.,30.,50.,70.,plev, 925.,950.,975, 1000.]

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


; DZSNO = get_fld(filein,'DZSNO')
;  dj= my_inter_clm(SNOWD2,lon,lat,cols1d_lon,cols1d_lat,ZZ=DZSNO[0,*])
;  DZSNO[0,*] = dj
  
 H2OSNO = get_fld(filein,'H2OSNO')
  H2OSNO= my_inter_clm(SNOWD2,lon,lat,cols1d_lon,cols1d_lat,ZZ=H2OSNO)

 frac_sno = get_fld(filein,'frac_sno')
  jj = where(H2OSNO gt 0.0 and (H2OSNO le 1.0e10),cnt)
  if(cnt gt 0)then frac_sno[jj] = 1.0

 H2OSOI_LIQ= get_fld(filein,'H2OSOI_LIQ')
  dj1= my_inter_clm(SOILW1_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=H2OSOI_LIQ[0,*])
  dj2= my_inter_clm(SOILW2_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=H2OSOI_LIQ[1,*])
  dj3= my_inter_clm(SOILW3_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=H2OSOI_LIQ[2,*])
  dj4= my_inter_clm(SOILW4_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=H2OSOI_LIQ[3,*])
  H2OSOI_LIQ[0,*] = dj1
  H2OSOI_LIQ[1,*] = dj2
  H2OSOI_LIQ[2,*] = dj3
  H2OSOI_LIQ[3,*] = dj4

;;  WT = ave2(H2OSOI_LIQ)*4

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
  dj1= my_inter_clm(TS1_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
  dj2= my_inter_clm(TS2_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
  dj3= my_inter_clm(TS3_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
  dj4= my_inter_clm(TS4_2,lon,lat,cols1d_lon,cols1d_lat,ZZ=TS_GRND)
  T_SOISNO[0,*] = dj1
  T_SOISNO[1,*] = dj2
  T_SOISNO[2,*] = dj3
  T_SOISNO[3,*] = dj4


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

; ready CLDLIQ CLDICE NUMLIQ NUMICE ICEFRAC SICTHK SNOWHICE TSICE

print,'minimum values of H2OSNO,H2OSOI_LIQ,T_GRND'
print,min(H2OSNO),min(H2OSOI_LIQ),min(T_GRND)
print, 'minimum values of T_SOISNO T_REF2M'
print,min(T_SOISNO),min(T_REF2M)

print,'maximum values of H2OSNO,H2OSOI_LIQ,T_GRND'
print,max(H2OSNO),max(H2OSOI_LIQ),max(T_GRND)
print, 'maximum values of T_SOISNO T_REF2M'
print,max(T_SOISNO),max(T_REF2M)

print,'write initial clm file?', clm_filenew
read,ix

jump1:

;=================================
fileID=ncdf_open(clm_filenew,/write)
NCDF_CONTROL, fileid, /FILL
;-------
VarID = NCDF_VARID(fileID, 'DZSNO')
;ncdf_varput,fileID, varID,DZSNO
;-------
VarID = NCDF_VARID(fileID, 'frac_sno')
;ncdf_varput,fileID, varID,frac_sno        ;!! SNOWF this one needs to be commented out!
;-------
VarID = NCDF_VARID(fileID, 'H2OSNO')
;ncdf_varput,fileID, varID,H2OSNO          ;!! SNOWW
;-------
VarID = NCDF_VARID(fileID, 'H2OSOI_LIQ')
ncdf_varput,fileID, varID,H2OSOI_LIQ      ;!! SOILLIQ
;-------
;;VarID = NCDF_VARID(fileID, 'WT')
;;ncdf_varput,fileID, varID,WT
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
VarID = NCDF_VARID(fileID, 'T_GRND_U')
;;ncdf_varput,fileID, varID, T_GRND_U
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_U')
;;ncdf_varput,fileID, varID,T_REF2M_U
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MIN_U')
;;ncdf_varput,fileID, varID,T_REF2M_MIN_U
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MAX_U')
;;ncdf_varput,fileID, varID,T_REF2M_MAX_U
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MIN_INST_U')
;;ncdf_varput,fileID, varID,T_REF2M_MIN_INST_U
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MAX_INST_U')
;;ncdf_varput,fileID, varID,T_REF2M_MAX_INST_U
;-------
VarID = NCDF_VARID(fileID, 'T_GRND_R')
;;ncdf_varput,fileID, varID,T_GRND_R
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_R')
;;ncdf_varput,fileID, varID,T_REF2M_MIN_R
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MAX_R')
;;ncdf_varput,fileID, varID,T_REF2M_MAX_R
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MIN_INST_R')
;;ncdf_varput,fileID, varID,T_REF2M_MIN_INST_R
;-------
VarID = NCDF_VARID(fileID, 'T_REF2M_MAX_INST_R')
;;ncdf_varput,fileID, varID,T_REF2M_MAX_INST_R
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
VarID = NCDF_VARID(fileID, 'T_VEG24_VALUE')
ncdf_varput,fileID, varID,T_VEG24_VALUE
;-------
VarID = NCDF_VARID(fileID, 'T_VEG240_VALUE')
ncdf_varput,fileID, varID,T_VEG240_VALUE
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

