function get_fldr,filen,var,j1,j2

 ncdf_mread,filen,var,data
   
   d=reform(data.(1))
   d=d[j1:j2]
   return,d
end


