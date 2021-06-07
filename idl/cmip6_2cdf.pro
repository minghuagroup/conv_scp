
restore,'cmip6_macro.sav'
; =====================================================================================
; =====================================================================================

cdf_tmp_file = '/glade/u/home/mzhang/obs/FNL/Cheng/NCAR_CESM2_CMIP_amip_tmp.nc'

; ====================
my_model = 'NOAA-GFDL/GFDL-CM4'
my_model = 'NCAR/CESM2'
my_k     = get_cmip6_model_number(my_model)

;-------
;my_mip = 'CFMIP'
;my_mip = 'GeoMIP'
;my_mip = 'ScenarioMIP'
my_mip = 'CMIP'
my_m     = get_cmip6_mip_number(my_mip)

;my_Exp = 'ssp585'
my_Exp = 'historical'
my_Exp = 'amip'
my_e     = get_cmip6_exp_number(my_Exp, my_mip)

; ====================

  ; GRAND LOOP

;;for imodel = 0, n_elements(cmip6.models)-1 do begin
for imodel = my_k,my_k do begin
   Model = cmip6.models[imodel]
   print, imodel, '   ', model

;;for imip   = 0, n_elements(cmip6.mips) -1 do begin 
for imip   = my_m, my_m do begin  ; 16 ScenaroMIP, 
; ^^^^^^----------------------------------
   MIPid = cmip6.mips[imip]

;for iExpid = 0, n_elements(exps)-1 do begin
for iExpid = my_e, my_e do begin
; ^^^^^^--------------------------------
   Expid = cmip6.(k)[iExpid]   ; CFMIP - > a4SST etc.



   cmip6_vars = cmip6_vars()
  ;   
   print,'imodel, imip, iexp ', imodel,' ',  imip,' ', iexpid,' ', model, ' ', mipid, ' ',expid

; =====================================================================================

; prepare output file
 scratch = '/glade/work/mzhang/C/'
 dir_model = scratch + model.replace('/','_')
 dir_mip   = dir_model + '/' + mipid
 dir_exp   = dir_mip   + '/' + expid+'_climo'
 file_cdf  = dir_exp 
 file_save_path = file_cdf

cdf_tmp_file = '~/mzhangs/tmp/'+model.replace('/','_')+'_'+mipid+'_'+Expid+'_tmp.nc'
; this is obtained after run cmip6_cdf_template.pro, which uses coordinates from ts.sav or specified
; cdf_tmp_file = '/glade/u/home/mzhang/obs/FNL/Cheng/NCAR_CESM2_CMIP_amip_tmp.nc'

; saved file:
; save,file = file_cdf+'/'+cam_var+'.sav',var,,dd2m,model,mipid,expid,lat,lon,plev,yearrange,files
;---------------------------------------------------------------------------------------------------------

restore,file_cdf+'/ts.sav'
cmip6_vars = cmip6_vars()
cesm_vars  = cmip6_vars.vars_cesm_all
cmip_vars  = cmip6_vars.vars_cmip6
vars_add   = cmip6_vars.vars_add
vars3d     = cmip6_vars.vars3d
nv         = n_elements(cesm_vars)
nv_cmip    = n_elements(cmip_vars)

; 1 ===== fill missing data and prepare the additional cam fileds


  cmip6_fill_lowest, file_save_path
; ^^^^^^^^^^ ===============
; use path file_path to locate .sav files and update 3d fields by filling and reversing, get H, theta

 ;----------------------------
jumpzz:

  cmip6_add_vars, file_save_path
; ^^^^^^^^^^ ===============
;!!!
; use path file_path to calculated vars_cam_add fields
 ;----------------------------

; all variables in cesm2_vars are saved in individual files using cmip name or cam_add name, 
; non-exist fields assigned a single value dd2m = 1.0e20 


; 2 ====== cdf file preparation
Title = 'Monthly climatology for CMIP6 '+ MIPid + ' '+  Model + ' ' + Expid + $
           ' from year '+strdigit(yearrange[0],0) +' to '+strdigit(yearrange[1],0)


dir_climo = dir_model+'/cdfs/'+mipid+'_'+expid
if(not file_exist(dir_climo))then spawn, 'mkdir '+ dir_climo

;;cdf_tmp_file = '~/mzhangs/tmp/'+model.replace('/','_')+'_'+mipid+'_'+Expid+'_tmp.nc'
                ; this is obtained by running cmip6_cdf_template.pro

nmonth = 12  ; this could be days depending saved data from cmip6_data.pro
file_out12 = strarr(nmonth)
file_out5 =  strarr(5)
for im = 0, nmonth-1 do begin
 im2 = im+101
 im2 = strtrim(im2,2)
 im2 = '_'+strmid(im2,1,2)
 file_out12[im] = dir_climo+'/'+mipid+'_'+expid+im2+'_climo.nc'
endfor
seasons = '_'+['DJF','MAM','JJA','SON','ANN']
for is = 0,4 do begin
 file_out5[is] =  dir_climo+'/'+mipid+'_'+expid+seasons[is]+'_climo.nc'
endfor

; 3 ====== restore data of variables from .sav for put into monthly cdf files

; Need to loop in month, or the program kills idl

FOR IM = 0, NMONTH-1 DO BEGIN
; ================================

  yrsj = strtrim(yearrange[0],2)+'-'+ strtrim(yearrange[1],2)
  Title = model.replace('/','_')+ '_' + MIPID + '_' + Strupcase(EXPID) +' ('+yrsj+')'

  print,' for month to prepare: ', im, ' ', file_out12[im]

  cmd = 'cp '+ cdf_tmp_file + ' ' + file_out12[im]

;;    spawn,cmd
    ;^^^^^^^^^------------
ENDFOR
  
FOR IM = 0, NMONTH-1 DO BEGIN
; ================================
  fileID = ncdf_open(file_out12[im],/write)
  NCDF_CONTROL,fileID , /REDEF
 ;-----------------------------------
  NCDF_ATTPUT, fileID, /GLOBAL, "title", 'CMIP6 DECK Experimenet (Monthly Climatology)'
  NCDF_ATTPUT, fileID, /GLOBAL,"case", Title
  NCDF_ATTPUT, fileID, /GLOBAL,"Month", strmid(strtrim(101+im,2),1,2)

  ncdf_close,fileID
ENDFOR
; ================================

FOR IM = 0, NMONTH-1 DO BEGIN
; ================================
  fileID = ncdf_open(file_out12[im],/write)
  NCDF_CONTROL,fileID  ;,/FILL

 for iv = 0, nv-1 do begin
; ================================
 var1 = cesm_vars[iv]


 if(belongsto(var1, cmip6_vars.vars_delete))then goto,jump_next_put_var ; duplicates
 VarID = NCDF_VARID(fileID,var1)
 if(VarID lt 0)then begin
   print,' ****  variable not on template cdf file ',var1 , ' ',var0
   goto, jump_next_put_var ; variable not on tmp_history file, need to run cdf_data_template 
                                           ; likely due to change in vars_add in cmip6_Vars.pro
 endif

 if(belongsto(var1, cmip6_vars.vars3d))then begin ; 3D fields just processed above into CESM variables, so use the CESM var files
   var0 = var1
 endif else begin                                 ; original cmip names
   var0     = cmip6_2cesm_var(var1)
 endelse

 filesaved = file_cdf+'/'+var0+'.sav'
 if(file_exist(filesaved)) then begin ; .sav file exists

   restore,filesaved                           ; obtained data
; ====================
   sz = size(dd2m)

   if(sz[0] eq 0)then begin
    if(im eq 0) then print,' ***** variable does not exist in original .sav file, get dummy values only .. ',iv,' ', var,iv,' ', var0
    goto, jump_next_put_var
   endif

   if(belongsto(var1, ['CLOUD','CLDTOT','CLDLOW','CLDMED','CLDHGH']))then dd2m = dd2m/100.  ; unit
   if(strmid(var1,0,3) eq 'PREC')then    dd2m = dd2m/1000.  ; unit
;   ========== customization
  
   case sz[0] of
  2: begin 
     dd = dd2m    ; static field
     ncdf_varput,fileID, varID, dd
     end

  3: begin        ; 2d time dependent fields

      dd = reform(dd2m[*,*,im]) 
       
           if(var1 eq 'PS') then begin
                   ncdf_varput,fileID, varID, dd*0.0 + 100000. ; 1000 mb replaced by the following field
                   VarID = NCDF_VARID(fileID,'PSS')  ; to keep PS as 100000. ito use AMWG package on CMIP pressure levels
           endif

      ncdf_varput,fileID, varID, dd
     end 

   4: begin 
       dd = reform(dd2m[*,*,*,im]) 
       ncdf_varput,fileID, varID, dd   ; need to reverse coordinates in p
;       if(var1 eq 'CLDLIQ')then begin
;         print,max(dd)
;       ncdf_varget,fileID, varID, ddj   ; need to reverse coordinates in p
;         print,max(ddj)   ; different!!
;       stop
;       endif
     end
   else: 
   endcase  ; dimension

   if(im eq 0)then print,iv, '  ',sz[0],' ',var1, ' ',min(dd), ' ', max(dd),' ',' --  to be put in cdf file from --', iv, ' ',var1, ' ', var0

 endif ; for variable file exists
 ddj = dd
jump_next_put_var:
endfor ; for the cesm variable
; ================================

   ncdf_close,fileID
;stop
ENDFOR ;im
; ================================


; 4 average files into seasonal mean
jump_ncra:
print,'  averaging monthly files to seasonal means ..... '
 cmd1 = 'ncra -O '+file_out12[0]+' '+ file_out12[1]+' '+ file_out12[11]+ ' ' +  file_out5[0]
 spawn,cmd1

 cmd2 = 'ncra -O '+file_out12[2]+' '+ file_out12[3]+' '+ file_out12[4]+ ' ' +  file_out5[1]
 spawn,cmd2

 cmd3 = 'ncra -O '+file_out12[5]+' '+ file_out12[6]+' '+ file_out12[7]+ ' ' +  file_out5[2]
 spawn,cmd3

 cmd4 = 'ncra -O '+file_out12[8]+' '+ file_out12[9]+' '+ file_out12[10]+ ' ' +  file_out5[3]
 spawn,cmd4

 cmd5 = 'ncra -O '+file_out5[0]+' '+ file_out5[1]+' '+ file_out5[2]+ ' ' +  file_out5[3] +' '+file_out5[4]
 spawn,cmd5

 print,'  last cdf data saved in ', file_out5[4]
;stop


 print,'=========> Finished ', Model, '  ', MIPid, '  ', Expid
;stop 
; =====================================================================================
; =====================================================================================

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
