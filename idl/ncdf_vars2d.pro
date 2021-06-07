; 2008-8-14 added dimension info 

;function ncdf_vars2d,filename,var, dim_n = dim_n, dim_eq = dim_eq

function ncdf_vars2d,filename, dim_n = dim_n, dim_eq = dim_eq , all_ncfiless = all_ncfiles
;================================================================
; select variables with dimension eq or larger than dim_n


  if(not file_Exist(filename))then begin
  print,'Stop: '+filename+' not exists.'
  endif

  all_ncfiles = filename
  if(file_search(filename, /test_dir)) then begin
   ; it is a folder
     ffiles = file_search(filename+'/*.nc')
     varsj = get_lastv2_inlist(ffiles,'/','.')
     all_ncfiles = ffiles
     return, varsj
  endif
 
  if(not keyword_set(dim_n))then dim_n = 2
   
  ncdf_vars, filename, jvars, no_print = 1

if(keyword_set(dim_eq)) then begin
    
  jvarsj = jvars
  kk = 0
  for iv = 0, n_elements(jvars)-1 do begin
    sz0 = ncdf_dim(filename, jvars[iv], dims = dims)
    if(sz0 eq dim_n)then begin
      jvarsj[kk] = jvars[iv]
      kk = kk+1
    endif
   endfor
    jvars = jvarsj[0:kk-1]

  return, jvars
  
 endif 
 
 jvarsj = jvars
 kk = 0
 excludes = ['time_bnds', 'time_written']
 for iv = 0, n_elements(jvars)-1 do begin
   sz0 = ncdf_dim(filename, jvars[iv], dims = dims)
   
   ;if(sz0 ge dim_n)then begin
   if(sz0 ge dim_n and (not belongsto(jvars[iv], excludes)))then begin
     jvarsj[kk] = jvars[iv]
     kk = kk+1
   endif
 endfor
 jvars = jvarsj[0:kk-1]



 return, jvars
  
end
