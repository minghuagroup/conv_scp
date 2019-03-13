function get_fld,filen,var
 ncdf_mread,filen,var,data
 return,reform(data.(1))
end


