
function get_cdf_att_all,file, var

 fid     = ncdf_open(file)
 vid     = ncdf_varid(fid,var)
 varinfo = ncdf_varinq(fid,vid)
 natts   = varinfo.natts

 data = create_Struct('var',var,'atts','')
 atts = ''
 for i   = 0,natts-1 do begin
   result = ncdf_attname(fid,vid,i)
   ncdf_attget,fid,vid,result,value
   value = string(value)
   
   atts = atts+':'+result

   data = create_Struct(data,result,value)
   data.atts = atts
 endfor

  return,data

end
  
 
 

