
;-------------------------------------------
  pro get_era5mn,var,yyyy,aa,aam,file=file, ml = ml, m12 = m12, fc=fc
;-------------------------------------------
; input: var, yyyymm, ml for multi-level fields
; output, aa, aam(annual,mean), file, mn=1 every month 
;-------------------------------------------

yyyymm = yyyy  ; all 12 months are saved

if(strlen(yyyy) ne 4)then begin
  print,'input format of date yyyy is not right, stopped in get_era4mn'
  stop
endif

restore,'era5mn.sav0' 
;        lon,lat,levels,varsm,varss,varsv,varsf,varsi,dir,dirm,dirs,dirv,dirf,diri

if(not keyword_set(ml))then ml = 0 
if(not keyword_set(m12))then m12 = 0 
if(not keyword_set(fc))then fc = 0 

 m = 12  ; save all 12 months always 12 month

 if(var eq 'PRECT')then begin
     get_era5mn,'PRECC',yyyymm,aa1,aam1,file=file,m12=m12 
     get_era5mn,'PRECL',yyyymm,aa2,aam2,file=file,m12=m12 

    aa = aa1 + aa2
    aam = aam1 + aam2

    return
  endif


  dir1 = yyyymm

  fj = file_search(dir1, count=cnt)
  if(cnt eq 0)then spawn,'mkdir '+dir1

  var0 = var
  if(ml)then begin
      f1  = dir1+'/'+yyyymm+'ML_'+var0+'.sav' ;, aa,aam
  endif else begin
      f1  = dir1+'/'+yyyymm+'_'+var0+'.sav' ;, aa,aam   ;from dump
  endelse

  if(fc)then begin ; forecast file , only single level field available
      f1  = dir1+'/'+yyyymm+'FC_'+var0+'.sav'
  endif

  if(m12)then begin
      f1 = f1+'_m12'
  endif



  file2 = file_search(f1,count=cnt)
  if(cnt gt 0)then begin
    restore,f1  ; aa,aam
    return
  endif
; =========== need to read below

year = strmid(yyyymm,0,4) ;'2017/'
month   = strmid(yyyymm,0,6) ;'201707/'

var1 = var
if(strmid(var,0,4) eq 'VAR_')then var1 = strmid(var,4,strlen(var)-4)
var1 = strlowcase(var1) 

files = file_search(dir+dirs+year+'/*_'+var1+'.regn320*.nc', count=nfs)
filev = file_search(dir+dirv+year+'/*_'+var1+'.regn320*.nc', count=nfv)
filei = file_search(dir+diri+'/*/*_'+var1+'.regn320*.nc', count=nfi)

; multilevel fields

if(ml ne 0)then begin
filem = file_search(dir+dirm+year+'/*_'+var1+'.regn320*'+'*.nc', count=nfm)
 if(nfm eq 0)then begin
  print,var,' file not found! in get_era5mn'
  stop
  return
 endif

 file = filem[0]

 dd = get_fld(file,var)
 ;aa  = reform(dd[*,*,*,hh])
 aa  = dd
 aam = ave4(dd)  ;averaged to ave4 for 3d fld

 ;if(m12)then aa = dd

 print,file
 help,var
 help,aa,aam
; save,filename = f1, aa,aam   

 return
endif


; single-level fields

if(nfs+nfv+nfi eq 0)then begin
  print,var,' file not found! in get_era5mn 2'
  stop
  return
endif


 if(nfs eq 1)then file = files[0]
 if(nfv eq 1)then file = filev[0]
 if(nfi eq 1)then file = filei[0]

 dd = get_fld(file,var)

 sz = size(dd)

 if(sz[0] eq 3)then begin
 ; hh = strdigit(strmid(yyyymm,4,2),0) -1
 ; aa  = reform(dd[*,*,hh])
 
  aa = dd
  aam = ave3(dd[*,*,*],three=3)  ; averaged to 2d field
  ;if(m12)then aa = dd            ; <-------------

;stop
 endif else begin  ;static fields
  aa  = dd
  aam = dd
 endelse

  print,file
  help,var
  help,dd
  help,aa,aam
;  save,filename = f1, aa,aam   

  return

end
