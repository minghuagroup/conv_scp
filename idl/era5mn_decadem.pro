
dirworkE = '/glade/scratch/mzhang/E/'

ndecade = 4
nyear   = 10
years   = indgen(40)+1979
yyyys   = strtrim(years,2)  ; 1995-2014
yyyys4  = reform(yyyys,nyear, ndecade)

decades = strarr(ndecade)
for i   = 0, ndecade-1 do decades[i] = yyyys4[0,i] + '_' + yyyys4[nyear-1, i]
decades_folder = dirworkE + decades  
make_folder, decades_folder

restore, 'era5mn.sav0'  ; has lon and lat info

savenc = 1
regrid = 1         ; only horizontally
;regrid = 0         ; only horizontally
prefix0 = '' ;'H_'

if(regrid) then begin  ; get lon2 and lat2
  grids = 'cam6'
  case grids of 
  'cmip6': restore, 'cmip6.sav0' 
  'cam6' : restore, 'cam6.sav0'
  'esm2' : restore, 'esm2.sav0'
   else  : begin print,' No grids of lon2, lat2 set ' & stop & end
  endcase
  nsmoothx = (n_elements(lon)/n_elements(lon2) /2 )*2 +1
  nsmoothy = (n_elements(lat)/n_elements(lat2) /2 )*2 +1

  missing = 1.0e20

endif 


;goto,jump1

FOR itype = 0,0 do begin  ; multi-level versus single level
;==============&&&&&&&&&&&&&=======================================
 if(itype eq 0) then begin
  ml = 0
  prefix = ''
  varsj = [varss, varsv, varsfi, varsm, varsac]   ; DEFINE WHAT FIELDS
 endif else begin
  ml = 1
  prefix = 'ML_'
  varsj = varsm                                   ; DEFINE WHAT FIELDS
 endelse

 nv    = n_elements(varsj)
 for iv = 0,nv-1 do begin                         ; LOOP Variables
 ;; for iv = 4,nv-1 do begin                         ; LOOP Variables
 ;=&&&&&&&&&&&&&======================
  var0 = varsj[iv]
;  cam_var = ??
;  cmip_var = cmip6_2cesm_var(cam_var)

 ;; print,iv, var0
;  if(var0 eq 'SP')then stop
  cam_varj = era5_2cesm_var(var0, c=1, m=1)
  cam_varj = cam_varj[0] 
;; if(cam_varj ne 'PS')then goto, jump_next_var

 ; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&7

 for idecade = 0, ndecade-1 do begin

; if(var0 ne 'CI')then goto, jump_next_var0
 if(not belongsto(var0, strupcase(['vige', 'vithee','vithen','vitoee','vitoen', 'cbh','cin','ci']))) $
    then goto, jump_next_var0
 ;=======================
   print, ' iv ', iv, '  ', var0, '  ', era5mn_vars(var0) , ' decade ', idecade, '  ', decades[idecade] 

  kk = 0
 for iy = 0,nyear-1 do begin
 ;=======================
    print, 'year ', iy, '  ', yyyys4[iy, idecade]
     era5savmn, yyyys4[iy, idecade], myvars = varsj[iv], no_int=1, do_save = 0, ml=ml, aa=aa, file=file

   if(n_elements(aa) gt 10) then begin
     if(iy eq 0)then taa = aa else taa = taa + aa   ; SUM UP the DATA
      kk = kk+1
   endif
 endfor ; iy
 ;=======================

  if(kk gt 0)then begin
  taa = taa/kk
  aa   = taa
  jj = where(abs(aa) gt 1.0e18, cnt)
  if(cnt gt 0)then aa[jj] = !Values.F_NaN         ; set missing value consistently
 
  if(regrid)then begin
     if(itype eq 0) then begin
       taa = smooth(aa, [nsmoothx,    1, 1], edge_wrap=1 , missing = missing)  
       aa  = smooth(taa,[1, nsmoothy, 1],    edge_truncate=1, missing = missing)  
       aa2  = my_interpol2(aa,lon,lat,lon2,lat2,tdim=1, missing=missing)
     endif else begin
       taa = smooth(aa, [nsmoothx,    1, 1, 1], edge_wrap=1 , missing = missing)  
       aa  = smooth(taa,[1, nsmoothy, 1, 1],    edge_truncate=1 , missing = missing)  
       aa2  = my_interpol3(aa,lon,lat,lon2,lat2, tdim=1)
     endelse
     aa = aa2
  endif

 fileout0 = decades_folder[idecade] + '/' +  prefix0 + prefix+var0 

 if(savenc)then begin
  fileout = fileout0+'.nc'
  save_nc, file, fileout,var0, aa, global_att = decades[idecade],$
            regrid=regrid, lon2 = lon2, lat2 = lat2, s_lon = 'longitude', s_lat = 'latitude'
 endif else begin
  fileout = fileout0+'.sav'
  save,file = fileout,aa,file   
 endelse
  print, '==========================================================='
  print,'  ', iv, idecade, ' ', var0, ' ', cam_varj, ' ', min(aa), max(aa), mean(aa)
  print,' file saved: ', fileout
  help,aa 
  print, '==========================================================='
 
 endif ;kk > 0
;stop
jump_next_var0:

endfor ; idecades
;====================

;stop
jump_next_var:
endfor ; iv

endfor ; itype

end
