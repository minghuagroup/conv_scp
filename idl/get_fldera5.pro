
;=====================================

function get_fldera5,yyyymmddhh,var, ml=ml, fc=fc,ps2=ps2, daily=daily, hr24 = hr24, mn = mn, $
    native=native,var0already = var0already

;=====================================

; var is newly named

;   ml: for multi-level fields
;   ps2: surface pressure corrdinate if from external such as cam 
;   daily: output daily field instead of instantaneoous
;   hr24: output 24 hrs 
;   mn: out 744 hrs 
;   daily hr24 and mn should only be 1 
;   when ml=1 only hr24 or daily can be selected
;   native=1  is to read origional ERA data without interpolation 

; used:

; restore,f2 from folder yyyymmddhha(era4sav) ; era4_iap3d
; restore,f1 from folder yyyymmddhh (era4res) ; interpol2, interpol3; save yyyymmddhha field
; get_era5.pro to save yyyymmddhh field
; my_interpol2.pro
; my_interpol3.pro
; my_interpol3v.pro
;=====================================

  yyyymmdd = strmid(yyyymmddhh,0,8)
  hh = fix(strmid(yyyymmddhh,8,2)) ; only get one time data, either hh or daily (daily overides)

  
; ==============================
; ==============================
  ; check ori and interpolated file in two data folders, if exist, do nothing, if not, read and save 

; ==============================
  era5sav,yyyymmdd, myvars = var   ;  <----------------------- 
; ==============================

  ; this can save more than one files if var names are same in different files

  ; data file will always be available after this
; ==============================

  dir1 = yyyymmdd
  dir2 = dir1+'a'
  fj = file_search(dir1, count=cnt)
  if(cnt eq 0)then spawn,'mkdir '+dir1
  fj = file_search(dir2, count=cnt)
  if(cnt eq 0)then spawn,'mkdir '+dir2


  restore,'~/mymods_hist/proc/era5.sav0'
  restore,'~/mymods_hist/proc/iap.sav0'
  
  if(not keyword_set(ml))then ml=0
  if(not keyword_set(native))then era=0 else era=1
  if(not keyword_set(fc))then fc =0
  if(not keyword_set(daily))then daily=0
  if(not keyword_set(hr24))then hr24 =0
  if(not keyword_set(mn))then mn =0
  
  if(daily)then hr24=0  ; daily overides

; special case

; done in get_obs
;  if(var eq 'PRECT')then begin
;    print,'PRECT does not exist in ERA5, call PRECL and PRECC separately'
;    stop
;  endif

  if(var eq 'PRECT')then begin  ; call self here
    dd1 = get_fldera5(yyyymmddhh,'PRECL', ml=0, fc=1,ps2=ps2, daily=daily, hr24 = hr24, $
                     mn = mn ,native=native,var0already = var0already)
    dd2 = get_fldera5(yyyymmddhh,'PRECC', ml=0, fc=1,ps2=ps2, daily=daily, hr24 = hr24, $
                     mn = mn,native=native,var0already = var0already)
    dd  = (dd1+dd2)
    return,dd
   endif

 ; define file names to restore, f1 for orgional data, f2 for interpolated data
;=====================================

  var0 = era5_Vars(var, rev=1, ml=ml)  ; from new naming to original naming
  if(keyword_set(var0already))then var0 =var

  if(ml)then begin
      f1  = dir1+'/'+yyyymmdd+'ML_'+var0+'.sav' ;, aa,aam
      f2  = dir2+'/'+yyyymmdd+'ML_'+var0+'.sav' ;, aa2,aam2
      f3  = dir2+'/'+yyyymmdd+'ML3_'+var0+'.sav' ;, aa2,aam2
  endif else begin
      f1  = dir1+'/'+yyyymmdd+'_'+var0+'.sav' ;, aa,aam   ;from dump
      f2  = dir2+'/'+yyyymmdd+'_'+var0+'.sav' ;, aa2,aam2 ;from horizontally interpolated
      f3  = dir2+'/'+yyyymmdd+'3_'+var0+'.sav' ;, aa2,aam2
  endelse
 
 
 if(not belongsto(var0, varss)) then begin
  if(fc)then begin
      f1  = dir1+'/'+yyyymmdd+'FC_'+var0+'.sav' ;, aa,aam
      f2  = dir2+'/'+yyyymmdd+'FC_'+var0+'.sav' ;, aa2,aam2
  endif
 endif

 if(belongsto(var,['PRECC','PRECL']))then begin
      f1  = dir1+'/'+yyyymmdd+'FC_'+var0+'.sav' ;, aa,aam
      f2  = dir2+'/'+yyyymmdd+'FC_'+var0+'.sav' ;, aa2,aam2
  endif
 if(belongsto(var0,[varsac]))then begin
      f1  = dir1+'/'+yyyymmdd+'FC_'+var0+'.sav' ;, aa,aam
      f2  = dir2+'/'+yyyymmdd+'FC_'+var0+'.sav' ;, aa2,aam2
  endif

  ;if(hr24)then begin
  ;    f1 = f1+'_hr24'
  ;    f2 = f2+'_hr24'
  ;endif    

  if(mn)then begin
      f1 = f1+'_mn'
      f2 = f2+'_mn'
  endif    

i=0
if(i)then begin
  file2 = file_search(f2,count=cnt)  ;search for horizontally regirdded file
  if(cnt eq 0)then begin  
     print,'data file error, does not exist in get_fldera5 after callling era5save 1'
     print,'Check whether flags ML and FC are set correctly in call'
     print,f2
     stop
  endif

  file1 = file_search(f1,count=cnt)  ;search for horizontally regirdded file
  if(cnt eq 0 and era)then begin  
     print,'data file error, does not exist in get_fldera5 after callling era5save 2'
     print,'Check whether flags ML and FC are set correctly in call'
     print,f1
     stop
  endif
endif

;=========================================================
;   print,''
;   print,f2, ' for ', var, ' exists, restore from it'  ;-----------------------

   if(era eq 0)then begin
    if(ml and file_exists(f3))then restore, f3 else restore, f2
     ;restore,f2  ;, aa2, aam2 <--------
   endif

   if(era eq 1)then begin
      restore,f1                ;, aa , aam <--------
      aa2 = aa
      aam2 = aam
   endif 

    sz = size(aam2)
    if(sz[0] eq 3 and ml eq 0)then begin
      print,'the restore field aa2 dimension and ML flag are not compatible in get_fldera5'
      print,'f2=',f2
      stop
    endif 

  ;-------------------------
  if(ml eq 0)then begin 
    dd = reform(aa2[*,*,hh])
  endif else begin
    dd = reform(aa2[*,*,*,hh])         ; a time snatshot
  endelse

;stop
  if(hr24) then dd = aa2  ;  return 24 hr
  if(daily)then dd = aam2 ;  overrides hr24

  if(era eq 1 or ml eq 0)then return,dd            ;<------ do nothing when original data  is requested
; -------------------------------------
; return horizontally-interpolated single-level data or return original era data


;=================================================================
  ;-- BELOW VERTICAL INTRPOLATION NEEDED

  ; multi-level field          
  ;-------------------------
if(not file_exists(f3))then begin

  ; vertical interpolation, if hr24=0, just a snapshot, other wise 24 hrs

  np = n_elements(levels)
  pp = levels

  nx2 = n_elements(lon2)  
  ny2 = n_elements(lat2)
  np2 = n_elements(levels2)
  pp2 = levels2  ; model levels


  var1 = 'PS'
  aa2  = get_fldera5(yyyymmddhh,var1,ml=0, fc=0,daily=daily, hr24=hr24, mn=mn, native=native) 
        ; called self for snapshot

  ps2j = aa2/100.
  if(keyword_Set(ps2))then ps2j = ps2/100.   ; destination ps file 

 if(hr24 eq 0)then begin  ; single snapshot
   pp3 = fltarr(nx2,ny2,np2)
   pp2j= fltarr(nx2,ny2,np)
   for kk = 0,np2-1 do pp3[*,*,kk] = hyam[kk]*p0/100.+hybm[kk]*ps2j[*,*]  ;in mb
   for k  = 0,np-1  do pp2j[*,*,k] = levels[k]
 endif else begin         ; 24 hrs
   pp3 = fltarr(nx2,ny2,np2,24)
   pp2j= fltarr(nx2,ny2,np,24)
   for kk = 0,np2-1 do pp3[*,*,kk,*] = hyam[kk]*p0/100.+hybm[kk]*ps2j[*,*,*]  ;in mb
   for k  = 0,np-1  do pp2j[*,*,k,*] = levels[k]
 endelse

  dd3 = pp3*0.0
; --------------------------------------------
 case var of
  'T': begin
        var1 = 'TS'
       end
  'Q': begin
        var1 = 'TD'
       end
  'U': begin
        var1 = 'U10'
       end
  'V': begin
        var1 = 'V10'
       end
 else: begin
        var1='XX'
       end
 endcase

; --------------------------------------------
 case var1 of
 'XX': begin
        if(hr24 eq 0)then dd2s = reform(dd[*,*,np-1]) ;lowest level as srf level
        if(hr24 ne 0)then dd2s = reform(dd[*,*,np-1,*]) ;lowest level as srf level, 24 hrs
       end
 'TD': begin
       aa2 = get_fldera5(yyyymmddhh,var1,ml=0, fc=0,daily=daily, hr24=hr24, mn=mn, native=native)
       ES = f_eswi(aa2-273.16) 
       dd2s = 0.622*es/ps2j
       end
 else: dd2s  = get_fldera5(yyyymmddhh,var1,ml=0, fc=0,daily=daily, hr24=hr24, mn=mn, native=native)
 endcase
; --------------------------------------------

; vertical interpolation

 dd2 = my_interpol3v(dd, pp2j, dd2s, ps2j, pp3, tdim = hr24) 

 aa2 = dd2
 aam2 = ave4(aa2)
 save,file=f3,aa2,aam2
 print,'saved file ',f3
 help,aa2,aam2
 endif else begin
  dd2 = aa2
 endelse


;stop

 return,dd2         ; <---------- return multi-level field

end
