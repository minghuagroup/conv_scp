 function get_fld_fnl2d,filein,var
;combine different fields together

    dd = get_fld(filein,var)
 jj=where(dd ge 10.e15,cnt)
 if(cnt gt 0)then dd[jj] = -9999.

 return,dd
 end


