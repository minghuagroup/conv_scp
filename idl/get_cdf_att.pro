
function get_cdf_att,file, var, att 

 fid     = ncdf_open(file)
 vid     = ncdf_varid(fid,var)
 varinfo = ncdf_varinq(fid,vid)
 natts   = varinfo.natts

 for i   = 0,natts-1 do begin
   result = ncdf_attname(fid,vid,i)
   if(strupcase(att) eq strupcase(strtrim(result,2)))then begin
    ncdf_attget,fid,vid,result,value
    value = string(value)
    return,value
   endif
 endfor
    return,''

end
  
 
 

