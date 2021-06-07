pro make_folder, path0

; like path = /glade/u/home/mzhang/work/C/CMIP/NCAR_CESM2/amip/sav

path = path0
nlen = strlen(path)
if(strmid(path,nlen-1,1) eq '/') then path = strmid(path,0,nlen-1) 
 

 paths = str_sep(path,'/')

 if(file_exist(path)) then return

 nlevels = n_elements(paths) 
 paths2 = paths

 for i = 1, nlevels-1 do begin
   paths2[i] = paths2[i-1]+'/'+paths[i]
 endfor

 j=nlevels
 for i= nlevels-1,1,-1 do begin
  if(not file_exist(paths2[i])) then begin
    j = i
  endif 
 endfor
    
 for i = j, nlevels-1 do begin
  print,' created level = ', i,  '  folder: ',paths2[i]
   spawn, 'mkdir '+paths2[i]    
 endfor

return
  
end

