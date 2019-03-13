; To read multiple netCDF file into a structure

pro ncdf_mread,filename,varnames,data,reform=reform,double=double

if(not keyword_set(double))then double=0

nvars=n_elements(varnames)
if(nvars eq 0) then begin
  print, "varnames not specified"
  print, "Usage: ncdf_mread,filename,varnames,data"
  stop
endif
temp=findfile(filename,count=foundfile)
if(foundfile eq 0) then begin
  print,'Stop: '+filename+' not exists.'
  stop
endif

;concatenate the variable names

field=varnames(0)
for i=1,nvars-1 do field=field+":"+varnames(i)

;initialize the structure

data = create_struct('fields',field)

Inid=ncdf_open(filename,/nowrite) ;  open for read

if(Inid eq -1) then begin
   print, 'Cant open file  ',filename
   stop
endif

;the structure can also be referred using the index
;start reading the data
for loop = 0,nvars-1 do begin
;   vname = varnames(loop)
   in_varid=ncdf_varid(Inid,varnames(loop))
   if(in_varid eq -1)then begin
   print, "Can't find Variable ",varnames(loop), ' in inputfile ',filename
   ncdf_close,Inid
   stop
   endif else begin
   ncdf_varget,Inid,in_varid,vfield,double=double
     if(keyword_set(reform))then if ( (size(vfield))[0] ne 0)then $
        vfield = reform(vfield)
   data = create_struct(data,varnames(loop),vfield)

   endelse

endfor

ncdf_close,Inid

return
end
