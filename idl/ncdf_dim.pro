; 2008-8-14 added dimension info 

function ncdf_dim,filename,var, dims = dims, dnames = dnames
;==============================

  if(not file_Exist(filename))then begin
  print,'Stop: '+filename+' not exists.'
  endif

  cdfid=ncdf_open(filename)

  varid = ncdf_varid(cdfid, var)
  varInfo = NCDF_VarInq(cdfid, varID)    
  dimIDs = varInfo.dim
  nDims = N_Elements(dimIDs)    
  dims = IntArr(nDims)
  dnames = StrArr(nDims)
  FOR j=0,nDims-1 DO BEGIN      
     NCDF_DimInq, cdfid, dimIDs[j], dname, dsize
     dims[j] = dsize    
     dnames[j] = dname
  ENDFOR 

  ncdf_close,cdfid

  return, varinfo.ndims
end
