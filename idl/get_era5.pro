
;-------------------------------------------
  pro get_era5,var,yyyymmdd,aa,aam,file=file, ml = ml, fc= fc, hr24=hr24, mn = mn
;-------------------------------------------
; var is original var name in era5
; input: var, yyyymmddhh, ml for multi-level fields
; output, aa, aam(daily mean), file, hr24 hourly output, mn=1 every hour in the month for single fied
;-------------------------------------------

yyyymmddhh = yyyymmdd   ; to save hourly data

restore,'era5.sav0' 
;        lon,lat,levels,varsm,varss,varsv,varsf,varsi,dir,dirm,dirs,dirv,dirf,diri

if(strlen(yyyymmdd) ne 8)then begin
  print,'input format of date yyyymmdd is not right, stopped in get_era4'
  stop
endif



h24 =1  ; save hourly data ! set
if(not keyword_set(ml))then ml = 0 
if(not keyword_set(fc))then fc = 0
if(not keyword_set(hr24))then hr24 = 0 
if(not keyword_set(mn))then mn = 0 
if(ml and mn)then begin
  print,'hourly multi-layer fields for the entire month cannot be read, change ml and mn'
  stop
endif


 if(var eq 'PRECT')then begin
    get_era5,'PRECC',yyyymmddhh,aa1,aam2,file=file, ml = ml, hr24=hr24, mn = mn, fc=1
    get_era5,'PRECL',yyyymmddhh,aa2,aam2,file=file, ml = ml, hr24=hr24, mn = mn, fc=1
    aa = aa1 + aa2
    aam = aam1 + aam2
    return
  endif

  var0 = var
  dir1 = yyyymmddhh
  fj = file_search(dir1, count=cnt)
  if(cnt eq 0)then spawn,'mkdir '+dir1

  ;output file to check whether alreay saved in era5sav, should be the same
  if(ml)then begin
      f1  = dir1+'/'+yyyymmddhh+'ML_'+var0+'.sav' ;, aa,aam
  endif else begin
      f1  = dir1+'/'+yyyymmddhh+'_'+var0+'.sav' ;, aa,aam   ;from dump
  endelse
  if(fc)then begin ; forecast file
      f1  = dir1+'/'+yyyymmddhh+'FC_'+var0+'.sav' ;, aa,aam ; single level
  endif

  ;if(hr24)then begin
  ;    f1 = f1+'_hr24'
  ;endif
  if(mn)then begin
      f1 = f1+'_mn'
  endif

file2 = file_search(f1,count=cnt)
if(cnt gt 0)then begin
  restore,f1  ; aa,aam    !  This is to save time reading original data
  return
endif
; =========== need to read below 

month = strmid(yyyymmddhh,0,6) ;'201707/'
day   = strmid(yyyymmddhh,0,8) ;'20170727/'

var1 = var
if(strmid(var,0,4) eq 'VAR_')then var1 = strmid(var,4,strlen(var)-4)
var1 = strlowcase(var1) 

files = file_search(dir+dirs+month+'/*_'+var1+'.regn320*.nc', count=nfs)
filev = file_search(dir+dirv+month+'/*_'+var1+'.regn320*.nc', count=nfv)
filei = file_search(dir+diri+'/*/*_'+var1+'.regn320*.nc', count=nfi)
filef = file_search(dir+dirf+month+'/*_'+var1+'.regn320*.nc', count=nff)
fileac = file_search(dir+dirac+month+'/*_'+var1+'.regn320*.nc', count=nfac)

;================================================================
; multilevel fields
if(ml ne 0)then begin
filem = file_search(dir+dirm+month+'/*_'+var1+'.regn320*'+day+'*.nc', count=nfm)
 if(nfm eq 0)then begin
  print,var,' file not found! in get_era5'
  fj = dir+dirm+month+'/*_'+var1+'.regn320*'+day+'*.nc'
  print,fj
  stop 
 endif

 file = filem[0]

 dd = get_fld(file,var)
 hh = fix(strmid(yyyymmddhh,8,2))
 aa  = reform(dd[*,*,*,hh])

 aam = ave4(dd)

 if(hr24)then aa = dd   ;<----------- always

 print,file
 help,var
 help,aa,aam
; save,filename = f1, aa,aam
 return
endif


;================================================================
; single-level fields

;stop
if(nfs+nfv+nfi+nff+nfac eq 0)then begin
  print,var,' file not found! in get_era5 2'
  stop
  return
endif


 if(nfs eq 1)then file = files[0]
 if(nfv eq 1)then file = filev[0]
 if(nfi eq 1)then file = filei[0]
 if(nff eq 1)then file = filef[0]
 if(nfac eq 1)then file = fileac[0]
;stop

 ; single layer fcst fields ==================================================
 if(nff ge 1)then begin
   get_era5_fcst,var,yyyymmddhh,aa,aam,file=file, accu=accu, hr24=hr24, mn=mn
    print,' after call get_era5_fcst in get_era5 file'
    help,var
    help,aa,aam

;    save,filename = f1, aa,aam
    return
 endif

;stop
 ; single layer fcst fields ==================================================
 if(nfac ge 1)then begin
   get_era5_fcst,var,yyyymmddhh,aa,aam,file=file, accu=accu, hr24=hr24, mn=mn
    print,' after call get_era5_fcst in get_era5 file'
    help,var
    help,aa,aam

;    save,filename = f1, aa,aam
    return
 endif

;stop

 dd = get_fld(file,var)  ; <--------------------------

 sz = size(dd)

 if(sz[0] eq 3)then begin
  utc = get_fld(file,'utc_date')
  utcs = strtrim(utc,2)                ;take the specific hour
  jj =where(utcs eq yyyymmddhh)
  j1 = jj[0]

  utcs2 = strmid(utcs,0,8)             ;take the day
  jj =where(utcs2 eq day)
  
  aa = reform(dd[*,*,j1])
  if(hr24)then aa = dd[*,*,jj]
  if(mn)  then aa = dd

  aam = ave3(dd[*,*,jj],three=3)

 endif else begin  ;static fields
  aa  = dd
  aam = dd
 endelse

  print,file
  help,var
  help,dd
  help,aa,aam
; save,filename = f1, aa,aam
  return

end
