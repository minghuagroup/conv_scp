
dir0 = '/Users/minghuazhang/unix/cmip6/GCM/esm/'

expids  = ['B20TR','SSP585']
Seasons    = ['DJF','MAM','JJA','SON','ANN'] 

for iexp = 1, n_elements(expids)-1 do begin
  dir_nc = dir0 + expids[iexp] + '/nc/'
  for is = 3, 4 do begin
   file = file_search(dir_nc + '*'+seasons[is]+'*.nc')
   file = file[0] 
   add_field, file
   ; ================
  endfor
endfor

end

