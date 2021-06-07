
pro ncdf_replace, file_old, file_new, var, var, aa
; to replace the var field  in file_old and make file_new with new data aa

 SPAWN, 'cp '+ fileold+' '+filenew  

 print,'to rewrite ',file_old
 print,'into       ',file_new

 fileID=ncdf_open(filenew,/write)
 NCDF_CONTROL, fileid, /FILL

 VarID = NCDF_VARID(fileID, var)
 ncdf_varput,fileID, varID, aa

 ncdf_close,fileID

 end



