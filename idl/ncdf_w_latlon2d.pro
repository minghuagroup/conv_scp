
pro ncdf_w_latlon2d, data3
;-------------------------
;for aa(nx,ny,nm)

fileout = data3.fileout

fileID=ncdf_create(fileout,/clobber)
if (fileID lt 0) then stop
 ncdf_attput,fileID,'Title',/GLOBAL, '+ data.attribute 

; define the dimensions 
 lon   = data3.lon
 lat   = data3.lat
 time  = data3.time
   nx = n_elements(lon)
   ny = n_elements(lat)
   nt = n_elements(time)

 dimID_lat = ncdf_dimdef(fileID,'lat',ny) 
 dimID_lon = ncdf_dimdef(fileID,'lon',nx) 
 dimID_time = ncdf_dimdef(fileID,'time',nt)      ;/UNLIMITED)

 dims2 = [dimID_lon,dimID_lat,dimID_time]

 varID_var = ncdf_vardef(fileID,'data',dims2,/float)   
 ncdf_attput,fileID,varID_var,'long_name', var
 ncdf_attput,fileID,varID_var,'units', units

 ncdf_control,fileID, /ENDEF    
; ------------------------------
; put to the file
; ---------------

  ncdf_varput,fileID, varID_time,time
  ncdf_varput,fileID, varID_lat,lat
  ncdf_varput,fileID, varID_lon,lon

  ncdf_varput,fileID, varID_var, data3.var

  ncdf_close,fileID 

 print,'fileout saved as..',fileout
 
 return
end
