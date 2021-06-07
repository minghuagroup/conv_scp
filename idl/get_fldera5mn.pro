
;=====================================

function get_fldera5mn,yyyymm,var, ml=ml, ps2=ps2, ann=ann, m12=m12,fc=fc,era=era,native=native,$
     var0already = var0already

;=====================================
;
;   var is newly named as in cam
;
;   ml: for multi-level fields
;   ps2: surface pressure corrdinate if from external such as cam 
;   ann: output annual mean field instead of monthly
;   m12: output 12 month
;   when ml=1 only m12=0 can be selected
 

; used:

; not yet restore,f2 from folder yyyymma(era4sav) ; era4_iap3d
; not yet restore,f1 from folder yyyymm (era4res) ; interpol2, interpol3; save yyyymma field
; get_era5mn.pro to save yyyymm field
; my_interpol2.pro
; my_interpol3.pro
; my_interpol3v.pro
;=====================================
  

  if(strlen(yyyymm) ne 6)then begin
    print,'input date format is not correct in get_fldera5mn'
    print,'yyyymm=',yyyymm
    stop
   endif
;=====================================
; monthly forecast fields
;=====================================
  if(belongsto(var,['PRECL','PRECC','PRECT']))then begin ;!!!!!!!!!!!!
    dd = get_fldera5fcmn(yyyymm,var)
    return,dd
  endif
   
;=====================================
;=====================================

  yyyy = strmid(yyyymm,0,4)
  mm   = strmid(yyyymm,4,2)

; ==============================
; ==============================
  ; check ori and interpolated file in two data folders, if exist, do nothing, if not, read and save 

  era5savmn,yyyy, myvars = var   ;  <----------------------- 

  ; this can save more than one files if var names are same in different files

  ; data file will always be available after this
; ==============================
; ==============================

  dir1 = yyyy
  dir2 = dir1+'a'

  fj = file_search(dir1, count=cnt)
  if(cnt eq 0)then spawn,'mkdir '+dir1
  fj = file_search(dir2, count=cnt)
  if(cnt eq 0)then spawn,'mkdir '+dir2

  restore,'~/mymods_hist/proc/era5.sav0'
  restore,'~/mymods_hist/proc/iap.sav0'
  
  if(not keyword_set(ml))then ml=0
  if(not keyword_set(fc))then fc=0
  if(not keyword_set(native))then native=0
  era=native
  if(not keyword_set(ann))then ann=0
  if(not keyword_set(m12))then m12 =0

  if(ann)then m12 =0 ;overides

;special case
  if(var eq 'PRECT')then begin
     print,'PRECT does not exist in ERA5, call PRECL and PRECC separately'
     stop
   endif
;    dd1 = get_fldera5mn(yyyymm,'PRECL', ml=0, ps2=ps2,ann=ann, m12=m12, era=era,fc=fc) 
;    dd2 = get_fldera5mn(yyyymm,'PRECC', ml=0, ps2=ps2, ann=ann, m12=m12, era=era,fc=fc)
;    dd  = dd1+dd2
;    return,dd
;  endif

; define file names to restore, f1 for orgional data, f2 for interpolated data
;=====================================

  var0 = era5_Vars(var, rev=1, ml=ml)
  ;var0 = era5_Vars(var, rev=0, ml=ml)
  if(keyword_set(var0already))then var0 =var


  if(ml)then begin
      f1  = dir1+'/'+yyyy+'ML_'+var0+'.sav' ;, aa,aam
      f2  = dir2+'/'+yyyy+'ML_'+var0+'.sav' ;, aa2,aam2
  endif else begin
      f1  = dir1+'/'+yyyy+'_'+var0+'.sav' ;, aa,aam   ;from dump
      f2  = dir2+'/'+yyyy+'_'+var0+'.sav' ;, aa2,aam2 ;from horizontally interpolated
  endelse

 if(not belongsto(var0, varss)) then begin
  if(fc)then begin
      f1  = dir1+'/'+yyyy+'FC_'+var0+'.sav' ;, aa,aam
      f2  = dir2+'/'+yyyy+'FC_'+var0+'.sav' ;, aa2,aam2
  endif
 endif

 ; if(m12)then begin
 ;     f1 = f1+'_m12'
 ;     f2 = f2+'_m12'
 ; endif    

  file2 = file_search(f2,count=cnt)  ;search for horizontally regirdded file
  if(cnt eq 0)then begin
     print,'data file error, does not exist in get_fldera5mn after callling era5savemn 1'
     print,'Check whether flags ML and FC are set correctly in call'
     print,f2
     stop
  endif

  file1 = file_search(f1,count=cnt)  ;search for horizontally regirdded file
  if(cnt eq 0 and era)then begin
     print,'data file error, does not exist in get_fldera5mn after callling era5savemn 2'
     print,'Check whether flags ML and FC are set correctly in call'
     print,f2
     stop
  endif

   if(era eq 0)then restore,f2  ;, aa2, aam2 <--------
   if(era eq 1)then begin
      restore,f1                ;, aa , aam <--------
      aa2 = aa
      aam2 = aam
   endif

    sz = size(aam2)
    if(sz[0] eq 3 and ml eq 0)then begin
      print,'the restore field aa2 dimension and ML flag are not compatible in get_fldera5mn'
      print,'f2=',f2
      stop
    endif

 hh = fix(strmid(yyyymm,4,2))
 ;-------------------------
  if(ml eq 0)then begin            ; a single months or annual average
     dd = reform(aa2[*,*,hh])
   endif else begin
    dd = reform(aa2[*,*,*,hh])         ; a time snatshot
  endelse

  if(m12)then dd = aa2  ; 
  if(ann)then dd = aam2 ;  overrides m12 
 

  if(era eq 1 or ml eq 0)then return,dd            ;<------ do nothing when original data  is requested
; -------------------------------------
; return horizontally-interpolated single-level data or return original era data


;=================================================================
  ;-- BELOW VERTICAL INTRPOLATION NEEDED

  ; multi-level field          
  ;-------------------------

  ; vertical interpolation, if m12=0, just a snapshot, other wise 12 months


  np = n_elements(levels)
  pp = levels

  nx2 = n_elements(lon2)
  ny2 = n_elements(lat2)
  np2 = n_elements(levels2)
  pp2 = levels2

  var1 = 'PS'
  aa2j  = get_fldera5mn(yyyymm,var1,ml=0,ann=ann, m12=m12, era=era)

  ps2j = aa2j/100.
  if(keyword_Set(ps2))then ps2j = ps2/100.

 if(m12 eq 0)then begin ; single month
   pp3 = fltarr(nx2,ny2,np2)
   pp2j= fltarr(nx2,ny2,np)
   for kk = 0,np2-1 do pp3[*,*,kk] = hyam[kk]*p0/100.+hybm[kk]*ps2j[*,*]  ;in mb
   for k  = 0,np-1  do pp2j[*,*,k] = levels[k]
 endif else begin         ; 24 hrs
   pp3 = fltarr(nx2,ny2,np2,12)
   pp2j= fltarr(nx2,ny2,np,12)
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
        if(m12 eq 0)then dd2s = reform(dd[*,*,np-1]) ;lowest level as srf level
        if(m12 ne 0)then dd2s = reform(dd[*,*,np-1,*]) ;lowest level as srf level, 24 hrs
       end
 'TD': begin
       aa2  = get_fldera5mn(yyyymm,var1,ml=0,ann=ann, m12=m12, era=era)
       ES = f_eswi(aa2-273.16) 
       dd2s = 0.622*es/ps2j
       end
 else: begin
       aa2  = get_fldera5mn(yyyymm,var1,ml=0,ann=ann, m12=m12, era=era)
       dd2s = aa2
       end
 endcase
; --------------------------------------------

 dd = my_interpol3v(dd, pp2j, dd2s, ps2j, pp3, tdim=m12) 

return,dd

end
