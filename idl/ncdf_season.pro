pro ncdf_season, ncfile , remove_months = remove_months

    climo   = get_lastv2_inlist(ncfile,'/','.')
    dir_nc = strstrip(ncfile,'/', last=1) + '/' + climo 
    make_folder, dir_nc
    dir_nc = dir_nc+'/'
 
    f_months = strarr(12)
   
    for k = 0, 11 do begin
     km = strmid(strtrim(101+k,2), 1,2)
     f_months[k] = dir_nc + climo+'_'+strtrim(km,2) + '.nc'
     cmd = 'ncks -O -d time,'+strtrim(k,2)+' ' + ncfile +' '+ f_months[k]
     print,cmd
     spawn, cmd
     print, 'created ', f_months[k]
    endfor

    print,' Do seasonal averaging ....'

    cmd = 'ncra -O '+dir_nc+climo+'_01.nc '+ dir_nc+climo+'_02.nc '+dir_nc +climo+'_12.nc '+dir_nc+climo+'_DJF.nc'
     print,cmd
    spawn,cmd
    cmd = 'ncra -O '+dir_nc+climo+'_03.nc '+ dir_nc+climo+'_04.nc '+dir_nc +climo+'_05.nc '+dir_nc+climo+'_MAM.nc'
     print,cmd
    spawn,cmd
    cmd = 'ncra -O '+dir_nc+climo+'_06.nc '+ dir_nc+climo+'_07.nc '+dir_nc +climo+'_08.nc '+dir_nc+climo+'_JJA.nc'
     print,cmd
    spawn,cmd
    cmd = 'ncra -O '+dir_nc+climo+'_09.nc '+ dir_nc+climo+'_10.nc '+dir_nc +climo+'_11.nc '+dir_nc+climo+'_SON.nc'
     print,cmd
    spawn,cmd
    cmd = 'ncra -O '+dir_nc+climo+'_DJF.nc '+ dir_nc+climo+'_MAM.nc '+dir_nc +climo+'_JJA.nc '+ $
                  dir_nc +climo+'_SON.nc '+dir_nc+climo+'_ANN.nc'
     print,cmd
    spawn,cmd

    print, ' last file created:  ', dir_nc+climo+'_ANN.nc'

    if(keyword_set(remove_months)) then begin
       for k = 0, 11 do begin
       spawn, 'rm '+ f_months[k] 
       print, ' removed ', f_months[k]
       endfor
    endif
return

end

