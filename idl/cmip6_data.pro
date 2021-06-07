; =====================================================================================
;  Purpose: to read and save customized data into the folder by variable name such as ts.sav
; =====================================================================================
;
; afterwards check the folder like /glade/u/home/mzhang/work/C/NCAR_CESM2/CMIP/amip_climo/ for .sav data files

;goto, jump1

restore,'cmip6_macro.sav'

; =====================================================================================
; =====================================================================================

; ====================
do_single_atm_variable = 0
do_only_atm_variable   = 1  ; set to 1 if not final sweep
my_variable            = 'CLDICE'
; ====================

;my_model = 'NOAA-GFDL/GFDL-CM4'
my_model = 'NCAR/CESM2'
my_k     = get_cmip6_model_number(my_model)
;-------
;my_mip = 'CFMIP'
;my_mip = 'GeoMIP'
my_mip = 'ScenarioMIP'
my_mip = 'CMIP'
my_m     = get_cmip6_mip_number(my_mip)

;my_Exp = 'ssp126'
my_Exp = 'historical'
my_Exp = 'amip'
my_e     = get_cmip6_exp_number(my_Exp, my_mip)
;-------

; =====================================================================================

  ; GRAND LOOP

;;for imodel = 0, n_elements(cmip6.models)-1 do begin
 for imodel = my_k,my_k do begin
; ^^^^^ ----------------------------------
   Model = cmip6.models[imodel]
   Model0 = Model
   print, imodel, '   ', model

  if(strpos(model, 'CESM') ge 0)then begin
    dir_ROOT = '/glade/collections/cdg/data/CMIP6/'
  endif else begin
    dir_ROOT = '/glade/collections/cmip/CMIP6/'
  endelse

;;for imip   = 0, n_elements(cmip6.mips) -1 do begin 
;for imip   = 3,3 do begin  ;  3 CFMIP
;for imip   = 4,4 do begin  ;   4 CMIP
;for imip   = 7,7 do begin  ;   7 GeoMIP
;for imip   = 16,16 do begin  ; 16 ScenaroMIP, 
for imip   = my_m, my_m do begin  ; 16 ScenaroMIP, 

; ^^^^^ ----------------------------------
   MIPid = cmip6.mips[imip]
   print, imip, '  ',mipid

   jj    = where(tag_names(cmip6) eq strupcase(MIPid), cnt)
   if(cnt eq 0)then goto, jump_next_mip  ; Expids not yet defined in cmip6_macro.pro 
   k = jj[0]
   exps = cmip6.(k)

;for iExpid = 0, n_elements(exps)-1 do begin
for iExpid = my_e, my_e do begin  ;amip
; ^^^^^ ----------------------------------

   Expid = cmip6.(k)[iExpid]   ; CFMIP - > a4SST etc.

   spawn, ' ls -d ' + dir_ROOT + MIPID +'/'+ MODEL +'/' + ExpID +'/*/Amon/ts/*/*/*', listing
    ; some models may not have data in these folders, need visiual inspect and change either the model version or folder

   temp = str_sep(listing[0],'/')
   n = n_elements(temp)
   if(n le 2)then goto, jump_next_expid
   Model_folder = '/'+temp[n-3] + '/' + temp[n-2]+'/'+temp[n-1]

;   goto, jump_next_mip
  ;   
  ; ===========================================

 cmip6_vars = cmip6_vars()

case Model0 of     ;!!!! Model name changes!
;--------------
'NCAR': begin
;       Model_Folder    = '/gn/latest'   ; for NCAR/CESM specifically
        end
'NOAA-GFDL': begin
       end
 else: begin
     end
endcase

case MIPid of
;--------------
 'CMIP':begin
;   if(Expid ne 'amip')then goto, jump_next_expid
        end
 'CFMIP': begin
;         if(not belongsto(Expid,['amip-m4K','amip-p4K','amip-4xCO2','amip-a4SST-4xCO2'])) $
;             then goto,jump_next_expid
        end
 'ScenarioMIP': begin
        end
   endcase
  
   print,'imodel, imip, iexp ', imodel,' ',  imip,' ', iexpid,' ', model, ' ', mipid, ' ',expid

;  stop
yearrange = 0 ;[1995,2014] , default set in get_fld_cmip6.pro

; =====================================================================================
; =====================================================================================

; prepare output file as .sav for individual variables

 scratch = '/glade/work/mzhang/C/'
 dir_model = scratch + model.replace('/','_')
 dir_mip   = dir_model + '/' + mipid
 dir_exp   = dir_mip   + '/' + expid+'_climo'
 file_cdf  = dir_exp 
 spawn, 'mkdir '+ dir_model
 spawn, 'mkdir '+ dir_mip
 spawn, 'mkdir '+ dir_exp
;stop

time  = indgen(12)+1.0

; ===========================================
; 1. read_in atm data
; ===========================================
dataType  = 'Amon'      ; in cmip6.dataType , later below are Omon etc.
vars   = cmip6_vars.vars_atm
nv         = n_elements(vars)
; -- preparation of coordinates
dd2       = get_fld_cmip6('ts', dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)
;                   -----------------
file2 = files[0]
lat   = get_fld(file2,'lat')
lon   = get_fld(file2,'lon')
plev  = [1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10, 5, 1]*100.0


iv1 = 0
iv2 = nv-1
if(do_single_atm_variable) then begin
  jj = where(vars eq cmip6_2cesm_var(my_variable),cnt)
    if(cnt eq 0)then begin
       print,' The specified single variable does not exist , stopped ', my_variable, cmip6_2cesm_var(my_variable)
       stop
    endif
  iv1 = jj[0]
  iv2 = iv1
endif

; =========================  
 for iv=iv1,iv2 do begin
; =========================  
 var = vars[iv]
 cam_var = cmip6_2cesm_var(var,c=1)

;if( not belongsto(cam_var,['CLDLIQ','CLDICE','CLOUD']) )then goto, next_var
if( belongsto(cam_var,['CLDLIQ','CLDICE','CLOUD']) )then goto, next_var
;!!!!!!! temporary statement
 print,iv,'  ', var
 dd2 = get_fld_cmip6(var, dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)
 ;--------------------------------------- get a section of the data specified by yearrange
  if(files[0] eq '')then goto,next_var

   mean_anom,dd2,12,dd2m,dd2a,dd2p         ; climo
   ;===============================

;  DECIDE WHETHER TO GET CLIMATOLOGICAL OR ORIGINAL DATA
 ;;  ; special cases for data
 ;     cam vertical levels different
   help,dd2,dd2m

;  if(Model eq 'NCAR/CESM2' and (belongsto(cam_var,['CLDLIQ','CLDICE','CLOUD']) )) then begin
  if(Model eq 'NCAR/CESM2' and (belongsto(cam_var,['CLOUD']) )) then begin
    lat = get_fld(files[0],'lat')
    lon = get_fld(files[0],'lon')
    time = get_fld(files[0],'time')
    lev = get_fld(files[0],'lev')
    lev = abs(lev)
    if(max(lev) lt 10.)then lev = lev*1.e5    ; special for the ncar runs
    if(max(lev) lt 1.e4)then lev = lev*100.

    print,'Special interpolation',min(lev),max(lev)

;; dd2j = dd2
;; zz3 = lev
;; yy3 = lat
;; dd2mj1 = dd2m
    if(lev[0] lt lev[1])then begin
      lev = reverse(lev)
      dd2m = reverse(dd2m,3)
    endif
    dd3m = interpol_p(dd2m,lev,plev)
;; dd2mj2 = dd2m

    jj = where(dd3m lt 0, cnt)
    if(cnt gt 0) then dd3m[jj] = 0.0

    if(cam_var eq 'CLOUD')then begin
      jj = where(dd3m gt 100., cnt)
      if(cnt gt 0) then dd3m[jj] = 100.0
     endif
;stop
    dd2m = dd3m
  endif

 save,file = file_cdf+'/'+var+'.sav',var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
 ; IN THE FUTURE, SHOULD CHOOSE DIFFERENT VARIABLE NAMES TO AVOID OVERWRING OTHERS WHEN RESTORED
 next_var:
endfor
; =========================  

if(do_single_atm_variable + do_only_atm_variable gt 0) then goto, jump_next_expid
 
; ===========================================
; 2 read_in ocn data
; ===========================================
dataType  = 'Omon'      ; in cmip6  dataType
vars   = cmip6_vars.vars_ocn   ; ocean variables
nv         = n_elements(vars)
for iv=0,nv-1 do begin
 var = vars[iv]
 print,iv,'   ',var,' ====> ',var

 dd2 = get_fld_cmip6(var, dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)

 if(files[0] ne '')then begin
   mean_anom,dd2,12,dd2m,dd2a,dd2p         ; climo
   help,dd2,dd2m
   save,file = file_cdf+'/'+var+'.sav',var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
 endif
endfor

; ===========================================
; read_in lnd data
; ===========================================
dataType  = 'Lmon'      ; in cmip6 dataType
vars   = cmip6_vars.vars_lnd   ; lnd variables
nv         = n_elements(vars)
for iv=0,nv-1 do begin
 var = vars[iv]
 print,iv,'   ',var,' ====> ',var

 dd2 = get_fld_cmip6(var, dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)

 if(files[0] ne '')then begin
   mean_anom,dd2,12,dd2m,dd2a,dd2p         ; climo
   help,dd2,dd2m
   save,file = file_cdf+'/'+var+'.sav',var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
 endif
endfor

; ===========================================
; read_in lice data
; ===========================================
dataType  = 'LImon'      ; in cmip6 dataType
vars   = cmip6_vars.vars_lice   ; lice variables
nv         = n_elements(vars)
for iv=0,nv-1 do begin
 var = vars[iv]
 print,iv,'   ',var,' ====> ',var

 dd2 = get_fld_cmip6(var, dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)

 if(files[0] ne '')then begin
   mean_anom,dd2,12,dd2m,dd2a,dd2p         ; climo
   help,dd2,dd2m
   save,file = file_cdf+'/'+var+'.sav',var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
 endif
endfor

; ===========================================
; read_in sice data
; ===========================================
dataType  = 'SImon'      ; in cmip6 dataType
vars   = cmip6_vars.vars_sice   ; sice variables
nv         = n_elements(vars)
for iv=0,nv-1 do begin
 var = vars[iv]
 print,iv,'   ',var,' ====> ',var

 dd2 = get_fld_cmip6(var, dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)

 if(files[0] ne '')then begin
   mean_anom,dd2,12,dd2m,dd2a,dd2p         ; climo
   help,dd2,dd2m
   save,file = file_cdf+'/'+var+'.sav',var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
 endif
endfor
;jump1:

; ===========================================
; read_in fx data
; ===========================================
dataType  = 'fx'      ; in cmip6 dataType
vars   = cmip6_vars.vars_fx   ; fixed variables
nv         = n_elements(vars)
for iv=0,nv-1 do begin
 var = vars[iv]
 print,iv,'   ',var,' ====> ',var

 dd2 = get_fld_cmip6(var, dir_ROOT, MIPid, Model, Expid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files)

 if(files[0] ne '')then begin
   mean_anom,dd2,12,dd2m,dd2a,dd2p         ; climo
   help,dd2,dd2m
   save,file = file_cdf+'/'+var+'.sav',var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
 endif
endfor


 print,'=========> Finished ', Model, '  ', MIPid, '  ', Expid, ' in ', file_cdf+'/'+var+'.sav'
; =====================================================================================
; =====================================================================================
; stop 
;===================
jump_next_expid:
  ;print,'  end ', mipid, ' ', expid
 endfor ; expids
jump_next_mip:
 endfor ; mips
  ;print,  'end2 ', expid, ' ', mipid, ' ', model
jump_next_model:
 endfor ; models

end
