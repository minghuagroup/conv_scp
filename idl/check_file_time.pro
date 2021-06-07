function check_file_time, nc_file

  ; to check whether it is a single time file or 12 moth file
  ; nc_file can be a file or a folder

  if(strpos(nc_file, '.nc') gt 0) then begin
    ncdf_vars, nc_file, vars, no_print =1
    if(blt('time',vars))then time = get_fld(nc_file, 'time') else time = 0
    ntime = n_elements(time) 
    if(ntime eq 1)then return,0 else return,ntime

  endif

  files = file_search(nc_file+'/*.nc')
  return, check_file_time(files[0])

 end
    


