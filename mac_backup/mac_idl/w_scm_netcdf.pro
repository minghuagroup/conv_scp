
; created in ever/
; opposite is mread,fileout,vars,data3
; gp03 for data3 and data4 struc
; run w_scm_struc0.pro first
; use structure to prepare the scm output
; data3,2d arrays in (np,nt) format
; all units are SI
;vars3=['fileout','lev;,'tsec','lat','lon','phis','bdate','lhflx','shflx','ps',$
;    'ptend','tsair','tg','t','q','u','v','omega','div','divt','vertdivt',$
;    'divt3d','divq','vertdivq','divq3d']
; possibly: solin,u_srf,v_srf,rh_srf,o3mmr
; in form of: data3.div[np,nt]


pro w_scm_netcdf, data3,zenith=zenith

fileout = data3.fileout

 vars_all = strlowcase(tag_names(data3))

fileID=ncdf_create(fileout,/clobber)
if (fileID lt 0) then stop

; define the dimensions 
;======================
;
; Synopsis : dimID = ncdf_dimdef(fileID, dimName, dimSize)
np = n_elements(data3.lev)
nt = n_elements(data3.tsec)

dimID_lat = ncdf_dimdef(fileID,'lat',1) 
dimID_lon = ncdf_dimdef(fileID,'lon',1) 
dimID_lev = ncdf_dimdef(fileID,'lev',np) 
dimID_tsec = ncdf_dimdef(fileID,'time',nt)      ;/UNLIMITED)

;global attributes, if any
; ========================= -------------------------
   ncdf_attput,fileID,'Title',/GLOBAL, 'IOP Analysis'
   ncdf_attput,fileID,'time_step_length',/GLOBAL,' '
; define the variables
; =====================
;
; Synopsis:  varID = ncdf_vardef(fileID,varname[,dims,/type])
; type could be char, long, short, float, double, etc.

; define scalar variables
; =====================------------------------

varID_bdate = ncdf_vardef(fileID,'bdate',/long)
   ncdf_attput,fileID,varID_bdate,'long_name','baseline date'
varID_phis = ncdf_vardef(fileID,'phis')
   ncdf_attput,fileID,varID_phis,'long_name','Surface geopotential'
   ncdf_attput,fileID,varID_phis,'units','m2/s2'


; define 1D arrays
; ==================---------------------------------------------
;dims = dimID_time
;   ncdf_attput,fileID,varID_,'long_name', ' '
;   ncdf_attput,fileID,varID_,'units',''
;--------------------------------------------
dims =  dimID_lon
varID_lon = ncdf_vardef(fileID,'lon',dims,/float)  ; type float is default
   ncdf_attput,fileID,varID_lon,'long_name','longitude '
   ncdf_attput,fileID,varID_lon,'units','degrees E'

dims =  dimID_lat
varID_lat = ncdf_vardef(fileID,'lat',dims,/float)
   ncdf_attput,fileID,varID_lat,'long_name','latitude'
   ncdf_attput,fileID,varID_lat,'units','degrees N'


dims =  dimID_lev
varID_lev = ncdf_vardef(fileID,'lev',dims,/float)
   ncdf_attput,fileID,varID_lev,'long_name', ' pressure levels'
   ncdf_attput,fileID,varID_lev,'units','Pa'

dims = dimID_tsec
varID_tsec = ncdf_vardef(fileID,'tsec',dims,/long)
   ncdf_attput,fileID,varID_tsec,'long_name', 'time '
   ncdf_attput,fileID,varID_tsec,'units','seconds'

dims = [dimID_lon,dimId_lat,dimID_tsec]
varID_lh = ncdf_vardef(fileID,'lhflx',dims,/float)
   ncdf_attput,fileID,varID_lh,'long_name', 'Surface latent heat flux'
   ncdf_attput,fileID,varID_lh,'units','W/m2'

varID_sh = ncdf_vardef(fileID,'shflx',dims,/float)
   ncdf_attput,fileID,varID_sh,'long_name', 'Surface sensible heat flux'
   ncdf_attput,fileID,varID_sh,'units','W/m2'

varID_ps = ncdf_vardef(fileID,'Ps',dims,/float)
   ncdf_attput,fileID,varID_ps,'long_name', 'Surface pressure'
   ncdf_attput,fileID,varID_ps,'units','Pa'

varID_Ptend = ncdf_vardef(fileID,'Ptend',dims,/float)
   ncdf_attput,fileID,varID_Ptend,'long_name', 'Surface pressure tendency'
   ncdf_attput,fileID,varID_Ptend,'units','Pa/s'

varID_Tsair = ncdf_vardef(fileID,'Tsair',dims,/float)
   ncdf_attput,fileID,varID_Tsair,'long_name', 'Surface air temperature'
   ncdf_attput,fileID,varID_Tsair,'units','K'

varID_tg = ncdf_vardef(fileID,'Tg',dims,/float)
   ncdf_attput,fileID,varID_tg,'long_name', 'Surface temperature'
   ncdf_attput,fileID,varID_tg,'units','K'

var='solin'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_solin = ncdf_vardef(fileID,'solin',dims,/float)
   ncdf_attput,fileID,varID_solin,'long_name', 'TOA Insolation'
   ncdf_attput,fileID,varID_solin,'units','W/m2'
endif

if(keyword_set(zenith))then begin ;------
var='zenith'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_zenith= ncdf_vardef(fileID,'zenith',dims,/float)
   ncdf_attput,fileID,varID_zenith,'long_name', 'Solar zenith angle'
   ncdf_attput,fileID,varID_zenith,'units','Degrees'
endif

var='dayfrac'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_dayfrac = ncdf_vardef(fileID,'dayfrac',dims,/float)
   ncdf_attput,fileID,varID_dayfrac,'long_name', 'Fraction of daytime'
   ncdf_attput,fileID,varID_dayfrac,'units','Fraction'
endif

var='delta'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_delta = ncdf_vardef(fileID,'delta',dims,/float)
   ncdf_attput,fileID,varID_delta,'long_name', 'Solar declination angle'
   ncdf_attput,fileID,varID_delta,'units','Degrees'
endif

var='ecc'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_ecc = ncdf_vardef(fileID,'ecc',dims,/float)
   ncdf_attput,fileID,varID_ecc,'long_name', 'Eccentricity'
   ncdf_attput,fileID,varID_ecc,'units','dimensionless'
endif

endif  ;zenith

var='u_srf'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_u_srf = ncdf_vardef(fileID,'u_srf',dims,/float)
   ncdf_attput,fileID,varID_u_srf,'long_name', 'u-component wind at surface'
   ncdf_attput,fileID,varID_u_srf,'units','W/m2'
endif

var='v_srf'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_v_srf = ncdf_vardef(fileID,'v_srf',dims,/float)
   ncdf_attput,fileID,varID_v_srf,'long_name', 'v-component wind at surface'
   ncdf_attput,fileID,varID_v_srf,'units','m/s'
endif

var='rh_srf'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_rh_srf = ncdf_vardef(fileID,'rh_srf',dims,/float)
   ncdf_attput,fileID,varID_rh_srf,'long_name', 'Surface air relative humidity' 
   ncdf_attput,fileID,varID_rh_srf,'units',' '
endif

var='malb'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_malb = ncdf_vardef(fileID,'srf_alb',dims,/float)
   ncdf_attput,fileID,varID_malb,'long_name', 'Surface albedo'
   ncdf_attput,fileID,varID_malb,'units',' '
endif

; define multi dimensional arrays
;===========================---------------------------------

dims2 = [dimID_lon,dimID_lat,dimID_lev,dimID_tsec]

varID_T = ncdf_vardef(fileID,'T',dims2,/float)
   ncdf_attput,fileID,varID_T,'long_name', 'Temperature'
   ncdf_attput,fileID,varID_T,'units', 'K'

varID_q = ncdf_vardef(fileID,'q',dims2,/float)
   ncdf_attput,fileID,varID_q,'long_name', 'Water vapor mixing ratio'
   ncdf_attput,fileID,varID_q,'units', 'kg/kg'

varID_u = ncdf_vardef(fileID,'u',dims2,/float)
   ncdf_attput,fileID,varID_u,'long_name', 'u wind'
   ncdf_attput,fileID,varID_u,'units', 'm/s'


varID_v = ncdf_vardef(fileID,'v',dims2,/float)
   ncdf_attput,fileID,varID_v,'long_name', 'v wind'
   ncdf_attput,fileID,varID_v,'units', 'm/s'

varID_omega = ncdf_vardef(fileID,'omega',dims2,/float)
   ncdf_attput,fileID,varID_omega,'long_name', 'Vertical pressure velocity'
   ncdf_attput,fileID,varID_omega,'units', 'Pa/s'

varID_divT = ncdf_vardef(fileID,'divT',dims2,/float)
   ncdf_attput,fileID,varID_divT,'long_name', 'Horizontal large scale temp. forcing'
   ncdf_attput,fileID,varID_divT,'units', 'K/s'

varID_divq = ncdf_vardef(fileID,'divq',dims2,/float)
   ncdf_attput,fileID,varID_divq,'long_name', 'Horizontal large scale water vapor forcing'
   ncdf_attput,fileID,varID_divq,'units', 'kg/kg/s'

varID_vertdivT = ncdf_vardef(fileID,'vertdivT',dims2,/float)
   ncdf_attput,fileID,varID_vertdivT,'long_name', 'Vertcal large scale temp. forcing'
   ncdf_attput,fileID,varID_vertdivT,'units', 'K/s'

varID_vertdivq = ncdf_vardef(fileID,'vertdivq',dims2,/float)
   ncdf_attput,fileID,varID_vertdivq,'long_name', 'Vertical large scale water vapor forcing'
   ncdf_attput,fileID,varID_vertdivq,'units', 'kg/kg/s'

varID_divT3d = ncdf_vardef(fileID,'divT3d',dims2,/float)
   ncdf_attput,fileID,varID_divT3d,'long_name', '3d large scale temp. forcing'
   ncdf_attput,fileID,varID_divT3d,'unTts', 'K/s'

varID_divq3d = ncdf_vardef(fileID,'divq3d',dims2,/float)
   ncdf_attput,fileID,varID_divq3d,'long_name', '3d large scale water vapor forcing'
   ncdf_attput,fileID,varID_divq3d,'units', 'kg/kg/s'

varID_div = ncdf_vardef(fileID,'div',dims2,/float)
   ncdf_attput,fileID,varID_div,'long_name', 'Large scale horizontal divergence'
   ncdf_attput,fileID,varID_div,'units', '1/s'

var='o3mmr'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
varID_o3mmr = ncdf_vardef(fileID,'o3mmr',dims2,/float)
   ncdf_attput,fileID,varID_o3mmr,'long_name', 'O3 mixing ratio'
   ncdf_attput,fileID,varID_o3mmr,'units', 'kg/kg'
endif


jump2:


; quit the define mode and enter data mode,this is necessary
;===========================================================

ncdf_control,fileID, /ENDEF    

; put data to the file
; ====================

 lon   = data3.lon
 lat   = data3.lat
 phis  = data3.phis
 bdate = data3.bdate
; ------------------------------
; put to the file
; ---------------

  ncdf_varput,fileID, varID_bdate,bdate
  ncdf_varput,fileID, varID_phis,phis
  ncdf_varput,fileID, varID_lat,lat
  ncdf_varput,fileID, varID_lon,lon

  ncdf_varput,fileID, varID_lev,data3.lev

  ncdf_varput,fileID, varID_tsec,data3.tsec

  dj = reform(data3.lhflx,1,1,nt)
  ncdf_varput,fileID, varID_lh, dj 

  dj = reform(data3.shflx,1,1,nt)
  ncdf_varput,fileID, varID_sh,dj

  dj = reform(data3.ps,1,1,nt)
  ncdf_varput,fileID, varID_ps,dj

  dj = reform(data3.ptend,1,1,nt)
  ncdf_varput,fileID, varID_Ptend,dj

  dj = reform(data3.tsair,1,1,nt)
  ncdf_varput,fileID, varID_Tsair,dj

  dj = reform(data3.tg,1,1,nt)
  ncdf_varput,fileID, varID_tg,dj

var='solin'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.solin,1,1,nt)
 ncdf_varput,fileID, varID_solin,dj
endif

if(keyword_set(zenith))then begin ;-------
var='zenith'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.zenith,1,1,nt)
 ncdf_varput,fileID, varID_zenith,dj
endif

var='dayfrac'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.dayfrac,1,1,nt)
 ncdf_varput,fileID, varID_dayfrac,dj
endif

var='delta'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.delta,1,1,nt)
 ncdf_varput,fileID, varID_delta,dj
endif

var='ecc'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.ecc,1,1,nt)
 ncdf_varput,fileID, varID_ecc,dj
endif

endif ; zenith ---

var='u_srf'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.u_srf,1,1,nt)
 ncdf_varput,fileID, varID_u_srf,dj
endif

var='v_srf'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.v_srf,1,1,nt)
 ncdf_varput,fileID, varID_v_srf,dj
endif

var='rh_srf'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.rh_srf,1,1,nt)
 ncdf_varput,fileID, varID_rh_srf,dj
endif

var='malb'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
 dj = reform(data3.malb,1,1,nt)
 ncdf_varput,fileID, varID_malb,dj
endif

; 2d put ===================================

  dj = reform(data3.T,1,1,np,nt)
  ncdf_varput,fileID, varID_T,dj

  dj = reform(data3.q,1,1,np,nt)
  ncdf_varput,fileID, varID_q,dj

  dj = reform(data3.u,1,1,np,nt)
  ncdf_varput,fileID, varID_u,dj

  dj = reform(data3.v,1,1,np,nt)
  ncdf_varput,fileID, varID_v,dj

  dj = reform(data3.omega,1,1,np,nt)
  ncdf_varput,fileID, varID_omega,dj

  dj = reform(data3.div,1,1,np,nt)
  ncdf_varput,fileID, varID_div,dj

  dj = reform(data3.divt,1,1,np,nt)
  ncdf_varput,fileID, varID_divT,dj

  dj = reform(data3.vertdivT,1,1,np,nt)
  ncdf_varput,fileID, varID_vertdivT,dj

  dj = reform(data3.divT3d,1,1,np,nt)
  ncdf_varput,fileID, varID_divT3d,dj

  dj = reform(data3.divq,1,1,np,nt)
  ncdf_varput,fileID, varID_divq,dj

  dj = reform(data3.vertdivq,1,1,np,nt)
  ncdf_varput,fileID, varID_vertdivq,dj

  dj = reform(data3.divq3d,1,1,np,nt)
  ncdf_varput,fileID, varID_divq3d,dj

var='o3mmr'
jj=where(vars_all eq var)
if(jj[0] ge 0)then begin
  dj = reform(data3.o3mmr,1,1,np,nt)
  ncdf_varput,fileID, varID_o3mmr,dj
endif

jumpend:
ncdf_close,fileID 

print,'fileout saved as..',fileout
return

end
