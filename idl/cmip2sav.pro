; =====================================================================================
;  Purpose: to read and save customized data into the folder by variable name such as ts.sav
; =====================================================================================
;
; afterwards check the folder like /glade/u/home/mzhang/work/C/NCAR_CESM2/CMIP/amip_climo/ for .sav data files

;goto, jump1

restore,'cmip6_macro.sav'

; =====================================================================================
; =====================================================================================

climo = 1  ; climatologically averged

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

my_Exp = 'amip'
my_Exp = 'ssp585'
my_Exp = 'historical'
my_e     = get_cmip6_exp_number(my_Exp, my_mip)
;-------

; =====================================================================================

  ; GRAND LOOP

;;for imodel = 0, n_elements(cmip6.models)-1 do begin
 for imodel = my_k,my_k do begin
; ^^^^^ ----------------------------------
   ModelID = cmip6.models[imodel]

;;for imip   = 0, n_elements(cmip6.mips) -1 do begin 
for imip   = my_m, my_m do begin  ; 16 ScenaroMIP, 

; ^^^^^ ----------------------------------
   MIPid = cmip6.mips[imip]

;for iExpid = 0, n_elements(exps)-1 do begin
for iExpid = my_e, my_e do begin  ;amip
; ^^^^^ ----------------------------------
  ExpIDs = get_stru(cmip6, mipid)
  ExpID = ExpIDs[iExpid] 


  ; source nc files
  if(strpos(modelID, 'CESM') ge 0)then begin
    dir_ROOT = '/glade/collections/cdg/data/CMIP6/'
  endif else begin
    dir_ROOT = '/glade/collections/cmip/CMIP6/'
  endelse

  dir_exp = dir_ROOT + MIPID +'/'+ ModelID +'/' + ExpID
 ; ---------

 ; save folder, local
  
  dir_Local = '/glade/work/mzhang/C/'+ MIPID + '/'+ str_replace(ModelID,'/','_') +'/' + ExpID +'/sav'
  make_folder, dir_Local
; -------------

  yearrange = get_cmip_years(ModelID, MipID, ExpID)

;  1 ------ specify the datatype, source file folder and sav folder

  DataType = 'Amon'  ; this can be from cmip6.Datatype_stru.Monthly
  DataTypes = ['LImon', 'Lmon', 'Omon', 'SImon', 'fx', 'Amon']

FOR iDataType = 0, n_elements(DataTypes)-1 DO BEGIN

  DataType = DataTypes[iDataType]
  print, ' *********** FROM THIS DATA TYPE: ', DataType

 ; ---------

  spawn, 'ls -d ' + dir_exp + '/*', runids
  runid = runids[0]
  dir_run = runid +'/'+DataType 

  spawn, 'ls -d ' + dir_run + '/*', vars
  vars = get_lastv_inlist(vars,'/')

  spawn, 'ls -d ' + dir_run +'/'+vars[0] + '/*', gn
  gn = get_lastv_inlist(gn,'/')

  spawn, 'ls -d ' + dir_run + '/'+vars[0] + '/' + gn[0] +'/*', latest
  latest = get_lastv_inlist(latest,'/')
  if(ModelID eq 'NCAR/CESM2')then latest[0] = 'latest'

  model_folder = gn[0] +'/'+ latest[0] 


  ; ===========================================

;  2 ------ loop through individual variables in the dir_run folder, but needs to be in the specified vars_cmip6 list
;           so that not too many fields are extracted.

 c_vars     = cmip6_vars()
 vars_cmip6 = c_vars.vars_cmip6 

 for iv = 0,n_elements(vars)-1 do begin   ; these are variables in the dir_run folder
 ; ====================================

 ; 2.1 --- specify variable jvar

  jvar = vars[iv]
  if(not belongsto(jvar,vars_cmip6)) then goto, jump_next_var

 ; 2.2 --- the specific folder ifor the variable to get source data, the save file for the variable

  data_folder = dir_run   + '/' + jvar +'/' + model_folder +'/'
  filesav     = dir_Local + '/' + jvar + '.sav'	
  print,'nc data_folder = ', data_folder
  ; like /glade/collections/cdg/data/CMIP6/CMIP/NCAR/CESM2/amip/r10i1p1f1/Amon/zg/gn/latest/
  ; ===========================================

  nc_files = file_Search(data_folder+'*.nc')
  ; ----------------------------------------

  kk = 0
 ; 2.3 --- loop through files for the variable

  ; ============== loop throuigh files
  for k = 0,n_elements(nc_files)-1 do begin

   file2 = nc_files[k]
   print, file2
   ncdf_vars,file2,vars2

 ; 2.4 --- set default values, since some variables may not exist 

 ; default fields
 time = [0] 
 lat  = [0] 
 lon = [0] 
 lev = [0] 
 times = 0
 year = 0
 months= 0
 days = 0
 if(belongsto('time',vars2))then time = get_fld(file2,'time') 
 if(belongsto('lev',vars2))then  lev  = get_fld(file2,'lev')
 if(belongsto('plev',vars2))then lev  = get_fld(file2,'plev')
 if(belongsto('lat',vars2))then  lat  = get_fld(file2,'lat')
 if(belongsto('lon',vars2))then  lon  = get_fld(file2,'lon')

 ; 2.3 --- get the field, if independent of time, finish to the end for sav

 dd     = get_fld(file2,jvar)
 dd_att = get_cdf_att_all(file2,jvar)
  
 if(not belongsto('time',vars2)) then begin ; static field
  dd3m = dd
  goto, jump_finished_dd3m 
 endif


 ; 2.4 --- get the time, convert to days since 1,1,0000, here the time unit is assumed to be day

 time = get_fld(file2,'time')
 yymmdd0 = get_cdf_time_since(file2, base0_day = base0_day)
 time = time + base0_day

 ; 2.5 check dates of the file and find the jj data within specified range for the mipid and expid

 year = fix(time/365.+0.99)
 dayofyear  = time - year*365.
 CALDAT, JULDAY(1, dayofyear, year), month, day

 if(max(year) lt yearrange[0]) then goto, jump_next_file
 if(min(year) gt yearrange[1]) then goto, jump_finished_file
 
 jj = where((year ge yearrange[0]) and (year le yearrange[1]),cnt)

 sz = size(dd)
 nx = sz[1]
 ny = sz[2]
 np = sz[3]

 ; 2.6 stitch data from multiple files

 if(kk eq 0)then begin
  times  = time[jj]
  years  = year[jj]
  months = month[jj]
  days   = day[jj]
  case sz[0] of
   3: dd2 = dd[*,*,jj]
   4: dd2 = dd[*,*,*,jj]
   else: dd2 = dd
  endcase
  kk=1
 endif else begin
  times  = [times, time[jj] ]
  years  = [years,year[jj] ]
  months = [months,month[jj]]
  days   = [days  ,day[jj]  ]
  case sz[0] of
   3: dd2 = [[[dd2]], [[dd[*,*,jj]]] ]
   4: begin
       sz = size(dd2)
       ntw = sz[4] + n_elements(jj)
       ddd = fltarr(nx,ny,np,ntw)
       for k = 0,np-1 do begin
         dw2 = reform(dd2[*,*,k,*])
         dww = reform(dd[*,*,k,jj])
         dd2w = [ [ [dw2]], [[dww]] ]
         ddd[*,*,k,*] = dd2w
       endfor
       dd2 = ddd
       end
  else: dd2 = [0]
  endcase  ; size of array
 endelse   ; first file versus second file etc.

; endif ; cnt file relevant
 jump_next_file:
 endfor  ; k file
 jump_finished_file:

if(kk eq 0) then begin
  print,' --- No files found in the specified time range ',yearrange
  stop 
endif

 ; 2.7 prepared stitched data for post processing

  dd3m = dd2
  sz = size(dd3m)

 ; postprocessing
   case 1 of
   climo:  begin

      case sz[0] of 

      3: begin
         dd4m = fltarr(nx,ny,12)
         for im = 0, 11 do begin
            jjm = where(months eq im)
             dd4m[*,*,im] =  ave3(dd3m[*,*,jjm])
         endfor
         end
      4: begin
         dd4m = fltarr(nx,ny,np,12)
         for im = 0, 11 do begin
            jjm = where(months eq im)
             dd4m[*,*,*,im] =  ave4(dd3m[*,*,*,jjm])
         endfor
         end
      else: dd4m = dd3m
      endcase  
      dd3m = dd4m

        months = indgen(12)+1
        times   = months
        years   = months*0 + yearrange[0]
        days    = months*0 + 1
         
        ;===============================
           end

   else: begin
         end
   endcase

jump_finished_dd3m:

 ; by now, data have been stiched in dd2 file

 ; 2.7 prepared data to save

   var3 = jvar
   lat3 = lat
   lon3 = lon
   lev3 = lev
   time3 = times
   year3 = years
   month3 = months
   day3   = days

   yearrange3 = yearrange
   modelid3 = modelid
   mipid3 = mipid
   expid3 = expid
   dataType3 = DataType

   var3 = jvar
   nc_files3 = nc_files
   climo3 = climo

   dd3m_att = dd_att
   
   save,file=filesav,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m ,dd3m_att 
;  help, var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m, dd3m_att
    
 print,''
   print,'filesav = ', filesav
 print,''
;stop

 jump_next_var:
 endfor

 ; 2.8 FINISHED SAVING DATA, use cmip6_verify to check

 print,''
 print,'=========> Finished ', ModelID, '  ', MIPid, '  ', Expid, ' ', DataType
 print,''
; =====================================================================================
; =====================================================================================
; stop 
;===================
ENDFOR ; DataType, Amon, Omon etc

jump_next_expid:
  ;print,'  end ', mipid, ' ', expid
 endfor ; expids
jump_next_mip:
 endfor ; mips
  ;print,  'end2 ', expid, ' ', mipid, ' ', model
jump_next_model:
 endfor ; models

end
