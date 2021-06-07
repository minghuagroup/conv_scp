;to list the field names of the netcdf file
; 2008-8-14 added dimension info 

pro  ncdf_vars,filename,vars,dims=dims, dsize = dsize, no_print = no_print

  filename = strcompress(filename)
  temp=findfile(filename,count=foundfile)
  if(foundfile eq 0) then begin
  print,'Stop: '+filename+' not exists.'
  stop
  endif
  cdfid=ncdf_open(filename)

  cdfinfo=ncdf_inquire(cdfid)

  nvars=cdfinfo.nvars
  vars=strarr(nvars)

 if(not keyword_set(dims))then begin

  for i=0, nvars-1 do begin
     varid = i
     varinfo = ncdf_varinq(cdfid,varid)
     vars(i) = varinfo.name
;  print,vars[i],varinfo.longname
  endfor

  if(not keyword_set(no_print))then print,vars

 endif else begin

  for i=0, nvars-1 do begin
     varid = i
     varinfo = ncdf_varinq(cdfid,varid)
     vars(i) = varinfo.name
     dsize = intarr(varinfo.ndims)
     dname = strarr(varinfo.ndims)
      for k=0,varinfo.ndims-1 do begin
        ncdf_diminq,cdfid,varinfo.dim[k],dname0,dsize0
        dsize[k]=dsize0
        dname[k]=dname0
      endfor 
      ;print,'--------------'
      ;print,vars[i]+': '
      ;print,'      ',strtrim(dsize,2)
      ;print,'      ', strtrim(dname,2)
  endfor
 endelse

  ncdf_close,cdfid

  return
  end


