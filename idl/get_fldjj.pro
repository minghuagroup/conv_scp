function get_fldjj,filen,var,jj

 tim=get_fld(filen,'time')
 n=n_elements(tim)

 ncdf_mread,filen,var,data
 dat = reform(data.(1))

 if(n lt n_elements(dat))then dat=dat[*,jj] else dat=dat[jj]

 return, float(dat)
end


