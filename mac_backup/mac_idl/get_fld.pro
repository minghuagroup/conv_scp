function get_fld,filen,var,j1=j1,j2=j2,double=double

 if(not keyword_set(double))then double=0

  ncdf_mread,filen,var,data,double=double
 
   sz=size(data.(1))
   
   if(sz[0] gt 0)then begin
    d=reform(data.(1))
    if(keyword_set(j1))then d=d[j1:j2]
   endif
   d=reform(d)
    return,d
end


