
function get_fldera5fcmn,yyyymm, var,myvars = myvars

restore,'~/mymods_hist/proc/era5.sav0' 
restore,'~/mymods_hist/proc/iap.sav0' 

;   lon,lat,levels,xx,yy,zz,varsm,varss,varsv,varsf,varsi,dir,dirm,dirs,dirv,dirf,diri
;   2 lon,lat,levels,xx,yy,zz,varsm,varss,varsv,varsf,varsi,dir,dirm,dirs,dirv,dirf,diri

yyyy = strmid(yyyymm,0,4)
mm   = strmid(yyyymm,4,2)

dirin = yyyy
dirout = yyyy+'a'
if(not file_exists(dirout))then spawn,'mkdir '+dirout 
if(not file_exists(dirin ))then spawn,'mkdir '+dirin

if(not keyword_set(myvars))then vars = [var] else vars = myvars

FOR IV = 0,n_elements(vars)-1 DO BEGIN

var = vars[iv]
var0 = era5_vars(var)

;print,' var ,var0: ---------- ', var, ' ',var0

  fja = dirout+'/'+yyyymm+'FC_'+var0+'.sav'
  fjsa = file_search(fja,count=cnt)
  if(cnt gt 0)then begin
   restore,fjsa
   goto,jump_saved
  endif

  fj = dirin+'/'+yyyymm+'FC_'+var0+'.sav'
  fjs = file_search(fj,count=cnt)

 if(cnt gt 0)then begin
  restore,fj  ; aa,aam
   aa   = ave3(aa)
   aa2  = my_interpol2(aa,lon,lat,lon2,lat2)
   aam2 = my_interpol2(aam,lon,lat,lon2,lat2)
   save,filename =  fja,aa2,aam2
   print,'saved : ',fja
   goto,jump_saved
 endif

;===================== get monthly for the entire year from daily data

if(var eq 'PRECT')then begin
  precl = get_fldera5fcmn(yyyymm, 'PRECL')
  precc = get_fldera5fcmn(yyyymm, 'PRECC')
  aa2 = precl+precc
  aam2 = aa2
  save,filename =  fja,aa2,aam2
  print,'saved : ',fja
  goto,jump_saved
endif

  date2 = yyyymm+'01'
  get_era5,var0,date2,aa,aam,file=file, ml = 0, fc= 1, hr24=0, mn = 1  ; <================
  
;  ddd=get_fldera5(date2,var, daily=1, fc=1, native=0)
;  dd2 = replicate2(ddd,31)*0.0

;  for k = 0,30 do begin
;    date3 = caldate2(date2,k)
;    mmj = strmid(date3,4,2)
;    if(mmj eq mm)then begin
;    ddd=get_fldera5(date3,var, daily=1, fc=1, native=0)
;    dd2[*,*,k] = ddd
;    endif else goto,out3
;  endfor ;day
;  out3:
;  dd2 = dd2[*,*,0:k-1]
;  aa2  = ave3(dd2)
;  aam2 = aa2

;;  endif
; 

  aa  = ave3(aa)
  save,filename = fj, aa,aam
  print,' saved:',fj

   aa2  = my_interpol2(aa,lon,lat,lon2,lat2)
   aam2 = my_interpol2(aam,lon,lat,lon2,lat2)

   save,filename =  fja,aa2,aam2
   print,'saved : ',fja

jump_saved:
ENDFOR

   return,aa2

end
