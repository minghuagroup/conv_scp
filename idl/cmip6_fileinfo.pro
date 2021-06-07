
function cmip6_fileinfo, file
;============================
 nf = n_elements(file)

 if(nf eq 1)then begin
  f = get_lastv_inlist(file,'/')
  finfo    = str_sep(f,'_')
  var      = finfo[0]
  Datatype = finfo[1] 
  modelID  = finfo[2] 
  Expid    = finfo[3] 
  dates    = strstrip(finfo[6],'.',l=1) 
  yrrange  = str_sep(dates,'-')
 
  fs = create_struct('file',file, 'f',f, 'var',var, 'datatype',datatype, 'modelid', $
       modelid,'expid',expid, 'yrrange', yrrange)

  return, fs
 endif

 files     = strarr(nf)
 fs        = strarr(nf)
 vars      = files
 DataTypes = vars
 ModelIDs  = vars
 EXPIDs    = vars
 yrranges  = strarr(2, nf)
   
 for i = 0, nf-1 do begin
   ff          = cmip6_fileinfo(file[i])
   files[i]    = ff.file  
   fs[i]       = ff.f  
   vars[i]     = ff.var  
   datatypes[i]= ff.datatype  
   modelids[i] = ff.modelid  
   expids[i]   = ff.expid  
   yrranges[*,i] = ff.yrrange  
 endfor
 ffs = create_struct('file',files, 'f',fs, 'var',vars, 'datatype',datatypes, 'modelid', $
       modelids, 'expid',expids, 'yrrange', yrranges)
 return, ffs
end

