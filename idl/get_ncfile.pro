function get_ncfile, file0, var, all_ncfiles = all_ncfiles

   if(strpos(file0,'.nc') gt 0)then return, file0

   filein = ''
   if(file_search(file0, /test_dir)) then begin
     var1 = strupcase(var)
     ffiles = file_search(file0+'/*.nc')
     fvars = get_lastv2_inlist(ffiles,'/','.nc')
     fvars = strdel(fvars, 'ML_')
     jj = where(strupcase(fvars) eq var1, cnt)
     if(cnt gt 0)then filein = ffiles[jj[0]]
    endif   

    if(strpos(filein, '.nc') lt 0)then begin   
      print,' ======= file does not exist, returned XXXX.nc'
      return, filein
    endif

    all_ncfiles = ffiles

    return,filein
 end


