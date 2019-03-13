function get_slice,filen,vname,it,dsize=dsize,dname=dname

 Inid=ncdf_open(filen,/nowrite) ;  open for read
 finq = NCDF_INQUIRE( Inid )

 in_varid=ncdf_varid(Inid,vname)

 if(in_varid lt 0)then begin
   print,'name does not exist for : ',vname
   ncdf_vars,filen,vars
   ncdf_close,Inid
   stop
 endif

 var_def = NCDF_VARINQ( Inid, in_varid )

 ndims = var_def.ndims

 if(ndims eq 1)then begin
    ncdf_varget,Inid,in_varid,vdata
    ncdf_close,Inid
    return,vdata
  endif

 stride = replicate(1,ndims)
 offset = [replicate(0,ndims-1),it]
 count  = intarr(ndims)

 dsize = intarr(ndims)
 dname = strarr(ndims)

 for k =0,ndims-1 do begin
    ncdf_diminq,Inid,var_def.dim[k],dname0,dsize0
    dsize[k] = dsize0
    dname[k] = dname0
 endfor

 if(it gt dsize[ndims-1])then begin
    print,'The slide index is too large:',it
    print,'The dimension is:',dsize
    return,dsize
 endif
 
 count = dsize & count[ndims-1] = 1

 ncdf_varget,Inid,in_varid,vdata,count=count,offset=offset,stride=stride

 ncdf_close,Inid

 return,vdata

end


