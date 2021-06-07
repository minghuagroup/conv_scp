
; To read multiple netCDF file into a structure

pro ncdf_mread,filename,varnames,data,reform=reform,double=double, offset=offset, count = count, stride=stride,prefix = prefix

  filename = strcompress(filename)
if(not keyword_set(prefix))then prefix=''
if(not keyword_set(double))then double=0

    count2 = 0
    offset2 =0
    stride2 =0
    yoffset = 0
    ycount = 0
    ystride = 0
  if(keyword_set(offset))then begin & offset2 = offset & yoffset =1 & end
  if(keyword_set(count))then  begin & count2  = count &  ycount = 1 & end
  if(keyword_set(stride))then begin & stride2 = stride & ystride = 1 & end

nvars=n_elements(varnames)
if(nvars eq 0) then begin
  print, "varnames not specified"
  print, "Usage: ncdf_mread,filename,varnames,data"
  stop
endif
;temp=findfile(filename,count=foundfile)
;temp=file_search(filename,count=foundfile)
foundfile = file_exist(filename)
if(foundfile eq 0) then begin
  print,'Stop: '+filename+' not exists.'
  stop
endif

dims = 0
if(nvars eq 1)then begin
 a = ncdf_dim(filename,varnames,dims = dims)
 nc = n_elements(dims)
endif

 if(nc ge 2)then begin
; ----------
 if(keyword_set(offset))then begin
    offset0 = dims*0
    offset0[nc-1] = offset
    offset = offset0
 endif else begin
    offset = dims*0
 endelse

 if(keyword_set(count)) then begin
    count0 = dims
    count0[0:nc-2] = dims[0:nc-2]
    count0[nc-1] = count   ; only need to specify the last number!!
    count = count0
 endif

 if(keyword_set(stride)) then begin
    stride0 = dims*0 + 1
    stride0[nc-1] = stride   ; only need to specify the last number!!
    stride = stride0
 endif
; ----------
endif

;
;stop
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

; help,varnames, nvars, offset2,offset, count
;stop

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
   ;ncdf_varget,Inid,in_varid,vfield,double=double
   ;;ncdf_varget,Inid,in_varid,vfield ;,double=double

 ; only 2d or above variable and a single variable
   if((nc ge 2) and (nvars eq 1)) then ncdf_varget,Inid,in_varid,vfield, offset = offset, count=count, stride=stride else $
        ncdf_varget,Inid,in_varid,vfield

     if(keyword_set(reform))then if ( (size(vfield))[0] ne 0)then $
        vfield = reform(vfield)
   data = create_struct(data,prefix+varnames(loop),vfield)
   endelse

endfor

ncdf_close,Inid

  if(keyword_set(yoffset))then offset = offset2 else offset = 0
  if(keyword_set(ycount))then count = count2    else count = 0
  if(keyword_set(ystride))then stride = stride2 else stride = 0


return
end

