;create a netcdf file, if exists, override it
;after read_output

; assume np, strhead1, dd1(32,nt),dd2(19,np,nt)

pro write_cdf,filename,title,ytitle,xtitle,p2,t2,y_unit,x_unit, $
              nvar0,nvar1,nvar2,                  $
              strhead0,strhead1,strhead2,         $
              units0,  units1, units2,            $
              dd0,    dd1,     dd2,               $
              notes=notes

; scalar, 1 d, 2D, and coordinates

;strhead1
;strhead2
;nvar1
;nvar2
;dd1(nvar1,nt1)
;dd2(nvar2,np2,nt2)
;p2(np2),t2(nt2),yy2,mo2,dy2,hh2,mm2
;nt1=nt2


if(not keyword_Set(notes))then notes=''

 vars0 = strhead0

 nt2= n_elements(t2)
 np2= n_elements(p2)

  nt =nt2
  np=np2
  p = p2
  time=t2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


fileID=ncdf_create(filename,/clobber)
NCDF_CONTROL, fileid, /FILL

if (fileID lt 0) then stop

; define the dimensions 
;======================
;
; Synopsis : dimID = ncdf_dimdef(fileID, dimName, dimSize)

dimID_lev = ncdf_dimdef(fileID,'np',np) 
dimID_time = ncdf_dimdef(fileID,'nt',nt) ;,/UNLIMITED)

;global attributes, if any
; ========================= -------------------------
   ncdf_attput,fileID,'Title',/GLOBAL,title
   ncdf_attput,fileID,'Notes:',/Global,notes
; define the variables
; =====================
;
; Synopsis:  varID = ncdf_vardef(fileID,varname[,dims,/type])
; type could be char, long, short, float, double, etc.

dims =  dimID_lev
varID_p = ncdf_vardef(fileID,ytitle,dims,/float)
   ncdf_attput,fileID,varID_p,'long_name', ytitle
   ncdf_attput,fileID,varID_p,'units',y_unit

dims = dimID_time
varID_time = ncdf_vardef(fileID,xtitle,dims,/double)
   ncdf_attput,fileID,varID_time,'long_name', 'time '
   ncdf_attput,fileID,varID_time,'units',x_unit
; define scalar variables
; =====================------------------------

; define scalars
 if(nvar0 gt 0)then varID_0 = lonarr(nvar0)
 for ii=0,nvar0-1 do begin
   varID_0[ii] = ncdf_vardef(fileID,strhead0[ii])
   ncdf_attput,fileID,varID_0[ii],'long_name',vars0[ii]
   ncdf_attput,fileID,varID_0[ii],'units',units0[ii]
 endfor

; define 1D arrays
; ==================---------------------------------------------
 
if(nvar1 gt 0)then varID_single = lonarr(nvar1)
 dims = dimID_time
 for ii=0,nvar1-1 do begin
 varID_single[ii] = ncdf_vardef(fileID,strhead1[ii],dims,/float)
 ncdf_attput,fileID,varID_single[ii],'long_name', strhead1[ii]
 ncdf_attput,fileID,varID_single[ii],'units', units1[ii]
 endfor

; define multi dimensional arrays
;===========================---------------------------------

if(nvar2 gt 0)then varID_layer = lonarr(nvar2)
dims2 = [dimID_lev,dimID_time]
for ii=0,nvar2-1 do begin
 varID_layer[ii] = ncdf_vardef(fileID,strhead2[ii],dims2,/float)
 ncdf_attput,fileID,varID_layer[ii],'long_name', strhead2[ii]
 ncdf_attput,fileID,varID_layer[ii],'units', units2[ii]
endfor

; quit the define mode and enter data mode,this is necessary
;===========================================================

ncdf_control,fileID, /ENDEF    

; put data to the file
; ====================

 if(nvar0 gt 0)then begin
  for ii=0,nvar0-1 do begin
   ncdf_varput,fileID, varID_0[ii],dd0[ii]
  endfor
 endif

  ncdf_varput,fileID, varID_p,p

  ncdf_varput,fileID, varID_time,time

; single fields
;===============
if(nvar1 gt 0)then begin
 dj  = fltarr(nt)
 for ii=0,nvar1-1 do begin
  dj[*]=reform(dd1[ii,*])
  ncdf_varput,fileID, varID_single[ii],dj
 endfor
endif

; layered fields
;================
if(nvar2 gt 0)then begin
 dj=fltarr(np,nt)
 for ii=0,nvar2-1 do begin
  dj[*,*]=reform(dd2[ii,*,*])
  ncdf_varput,fileID, varID_layer[ii],dj
 endfor
endif

ncdf_close,fileID 
print,'saved file:' ,filename
return
end
