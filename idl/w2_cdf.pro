;pro w2_cdf,file_cdf,data

file_cdf = 'mytest'

lon=get_fld(file,'lon')
lat=get_fld(file,'lat')
prect=get_fld(file,'PRECT')
dd = prect
ntime = 720
nlon = 256
nlat = 128

fid=ncdf_create(file_cdf +'.nc',/CLOBBER)

d=indgen(3,/LONG)
;d[0]=ncdf_dimdef(fid,'time', /unlimited)
d[0]=ncdf_dimdef(fid,'time',ntime );, /unlimited)
d[1]=ncdf_dimdef(fid,'lon',nlon)
d[2]=ncdf_dimdef(fid,'lat',nlat)

var_id=ncdf_vardef(fid,'Longitude',d[1],/float)
var_id=ncdf_vardef(fid,'Latitude' ,d[2],/float)

var_id=ncdf_vardef(fid,'time' ,d[0],/float)

var_id=ncdf_vardef(fid,var,[d[2],d[1],d[0]] ,/float)
ncdf_control,fid,/ENDEF

ncdf_varput,fid,'Longitude', lon
ncdf_varput,fid,'Latitude',  lat
ncdf_varput,fid, var,  dd
ncdf_close,fid
file2out = file_cdf+'.nc'
print,' > Writing complete... Closing...',file_cdf+'.nc'

;return
end

