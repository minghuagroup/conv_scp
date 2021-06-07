
function get_myfld,filename,vname

Inid=ncdf_open(filename,/nowrite) ;  open for read

if(Inid eq -1) then begin
   print, 'Cant open file  ',filename
   stop
endif

   in_varid=ncdf_varid(Inid,vname)
   if(in_varid eq -1)then begin
   print, "Can't find Variable ",vname, ' in inputfile ',filename
   ncdf_close,Inid
   stop
   endif else begin
   ncdf_varget,Inid,in_varid,vfield
   endelse

  d=reform(vfield)

ncdf_close,Inid

return,d
end
