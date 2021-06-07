function get_fldsh,filen,var,i1,i2

 tim=get_fld(filen,'time')
 n=n_elements(tim)

 ncdf_mread,filen,var,data
 dat = reform(data.(1))

 if(n lt n_elements(dat))then dat=dat[*,i1:i2] else dat=dat[i1:i2]

 return, float(dat)
end


