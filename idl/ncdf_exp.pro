
;pro ncdf_replace, file_old, file_new, var, var, aa
; to replace the var field  in file_old and make file_new with new data aa

fileold = '../obs/C3M/01.nc'
filenew = '../obs/C3M/1a.nc'

 SPAWN, 'cp '+ fileold+' '+filenew  

 print,'to rewrite ',fileold
 print,'into       ',filenew

 fileID=ncdf_open(filenew,/write)
 NCDF_CONTROL, fileid, /REDEF

 lat0 = get_fld(fileold,'lat')
 lon0 = get_fld(fileold,'lon')
 nx = 256
 ny = 128
 dimID_lat = ncdf_dimid(fileID,'lat')
 dimID_lon = ncdf_dimid(fileID,'lon')

; dims = dimID_lon
; varID_lon = ncdf_vardef(fileID,'lon',dims,/float)
 
; dims = dimID_lat
; varID_lat = ncdf_vardef(fileID,'lat',dims,/float)
 
 dims2 = [dimID_lon,dimID_lat]
 varID_dd = ncdf_vardef(fileID, 'CC', dims2,/float)

 varID_cldlow = ncdf_varid(fileID, 'CLDLOW')

 result = NCDF_ATTCOPY(fileID, varID_cldlow, 'CLDLOW', fileID, varID_dd)
 NCDF_CONTROL, fileid,  /ENDEF 

; ncdf_varput,fileID, varID_lat, indgen(128)*1.0
; ncdf_varput,fileID, varID_lon, indgen(256)*1.0 

 ncdf_varput,fileID, varID_dd, fltarr(nx,ny)

 ncdf_close,fileID

 end



