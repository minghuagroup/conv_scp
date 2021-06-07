function get_fld,filen,var,j1=j1,j2=j2,double=double, offset=offset, count = count,$
     stride=stride,prefix = prefix

 if(not keyword_set(prefix))then prefix=0
 if(not keyword_set(double))then double=0

  if(keyword_set(offset))then offset0 = offset
  if(keyword_set(count))then count0 = count
  if(keyword_set(stride))then stride0 = stride 

  ncdf_mread,filen,var,data,offset=offset, count = count, stride=stride, $
      prefix=prefix ;,double=double
 
   sz=size(data.(1))
   
   d = -999999.0
   if(sz[0] gt 0)then begin
    d=reform(data.(1))
    if(keyword_set(j1))then d=d[j1:j2]
   endif
   d=reform(d)

  if(keyword_set(offset))then offset = offset0
  if(keyword_set(count))then count = count0
  if(keyword_set(stride))then stride = stride0 

    return,d
end


