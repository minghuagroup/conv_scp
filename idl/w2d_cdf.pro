
pro w2d_cdf,data

;data = {fileout:'mytest2.nc', time:time3, lon:lon, lat:lat, bdate:19790100, PRECT:dd3}

fileout = data.fileout

fileID=ncdf_create(fileout,/clobber)

; define the dimensions 
;======================
;
time = data.time
lon  = data.lon
lat = data.lat
nt   = n_elements(time)
nlon = n_elements(lon)
nlat = n_elements(lat)
bdate = data.bdate

dimID_lat = ncdf_dimdef(fileID,'lat',nlat) 
dimID_lon = ncdf_dimdef(fileID,'lon',nlon) 
dimID_tsec = ncdf_dimdef(fileID,'time',nt)      ;/UNLIMITED)

;global attributes, if any
; ========================= -------------------------
   ncdf_attput,fileID,'Title',/GLOBAL, fileout
; define the variables
; =====================
;
; Synopsis:  varID = ncdf_vardef(fileID,varname[,dims,/type])
; type could be char, long, short, float, double, etc.

; define scalar variables
; =====================------------------------

varID_bdate = ncdf_vardef(fileID,'bdate',/long)
   ncdf_attput,fileID,varID_bdate,'long_name','baseline date'

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


;ldims =  dimID_lev
;lvarID_lev = ncdf_vardef(fileID,'lev',dims,/float)
;l   ncdf_attput,fileID,varID_lev,'long_name', ' pressure levels'
;l   ncdf_attput,fileID,varID_lev,'units','Pa'

dims = dimID_tsec
varID_tsec = ncdf_vardef(fileID,'tsec',dims,/long)
   ncdf_attput,fileID,varID_tsec,'long_name', 'time '
   ncdf_attput,fileID,varID_tsec,'units','days'

;dims = [dimID_lon,dimId_lat,dimID_tsec]
;varID_lh = ncdf_vardef(fileID,'lhflx',dims,/float)
;   ncdf_attput,fileID,varID_lh,'long_name', 'Surface latent heat flux'
;   ncdf_attput,fileID,varID_lh,'units','W/m2'

; define multi dimensional arrays
;===========================---------------------------------

;dims2 = [dimID_lon,dimID_lat,dimID_lev,dimID_tsec]
dims2 = [dimID_lon,dimID_lat,dimID_tsec]

vars_all = strlowcase(tag_names(data))

nvars = n_elements(vars_all)

varID_T = INTARR(nvars)

for iv = 0,nvars-1 do begin
 dj = data.(iv)
 sz = size(dj)
 if(sz[0] eq 3)then begin  ; 

  var = strupcase(vars_all[iv])
  varID_T[iv] = ncdf_vardef(fileID, var,dims2,/float)
   ncdf_attput,fileID,varID_T[iv],'long_name', vars_all[iv]
   ncdf_attput,fileID,varID_T[iv],'units', 'units'
 endif
endfor

; quit the define mode and enter data mode,this is necessary
;===========================================================

ncdf_control,fileID, /ENDEF    

; put data to the file
; ====================

; ------------------------------
; put to the file
; ---------------

  ncdf_varput,fileID, varID_bdate,bdate
  ncdf_varput,fileID, varID_lat,lat
  ncdf_varput,fileID, varID_lon,lon

  ;ncdf_varput,fileID, varID_lev,data3.lev

  ncdf_varput,fileID, varID_tsec,time

  ;dj = reform(data3.lhflx,1,1,nt)
  ;ncdf_varput,fileID, varID_lh, dj 

; 2d put ===================================

for iv = 0,nvars-1 do begin
 dj = data.(iv)
 sz = size(dj)
 if(sz[0] eq 3)then begin  ; 
  ncdf_varput,fileID, varID_T[iv],dj
 endif
endfor
ncdf_close,fileID 

print,'fileout saved as ',fileout
return

end
