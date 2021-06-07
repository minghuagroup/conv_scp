;  Purpose: to process cam tseries data
; =====================================================================================
; adapted from cmip62nc
;
; =======================
climo   = 1  ; climatologically averged
savenc  = 1   ; save .sav file or nc file
do_select_variables  = 1
my_variable         = ['ts','clt']

var_stream = 'CLDHGH CLDICE CLDLIQ CLDLOW CLDMED CLDTOT CLOUD DCQ DTCOND DTV FLDS FLNS FLNSC FLNT FLNTC FLUT FLUTC ' $
         +'FSDS FSDSC FSNS FSNSC FSNT FSNTC ICEFRAC ICIMR ICLDIWP ICLDTWP ICWMR IWC LANDFRAC LHFLX LWCF OCNFRAC ' $
         +'OMEGA OMEGAT PBLH PHIS PRECC PRECL PRECSC PRECSL PS PSL Q QFLX QRL QRS RELHUM SHFLX SNOWHICE SNOWHLND ' $
         +'SOLIN SRFRAD SST SWCF T TAUX TAUY TGCLDIWP TGCLDLWP TMQ TOT_CLD_VISTAU TREFHT TREFHTMN TREFHTMX TROP_P ' $
         +'TROP_T TS TSMN TSMX U U10 UBOT UQ UU V VBOT VD01 VQ VT VU VV WGUSTD WSPDSRFMX WSUB Z3'

My_variable = str_sep(Strcompress(var_Stream),' ')
; =======================

for imodel = 0, 0  do begin
; ^^^^^ ----------------------------------
   ModelID = 'GCM'
for imip   = 0,0 do begin

; ^^^^^ ----------------------------------
   MIPid = 'cam5'

for iExpid = 0,0 do begin
; ^^^^^ ----------------------------------
  ; source nc files
  dir_ROOT  = '/glade/collections/cdg/data/cesmLE/CESM-CAM5-BGC-LE/atm/proc/tseries/monthly/'
  dir_exp   = dir_ROOT 

  Exptypes  = ['historical', 'ssp585']   ; name holder
  ExpNames  = ['B20TR','BRCP85']
  dir_Local = '/glade/scratch/mzhang/C/GCM/cam5/'

  for iExp = 1, 1 do begin

   expid = exptypes[iexp]
   iexpid = expnames[iexp]
   ;yearrange = get_cmip_years('NCAR_CESM2', 'CMIP', ExpID) ; to mimic


   if(iexp eq 1) then yearrange = [2061,2080.] else yearrange = [1986,2005]

  print, ' iExpID and ExpID:  ', iExpID, '  ', ExpID 
  print, ' YearRange:         ', YearRange
  print, '-------------------------------------------------------------'

  spawn, 'ls -d ' + dir_exp + '* ', vars 
  vars = get_lastv_inlist(vars,'/')

 jvars = vars
 k = 0
 if(do_select_variables)then begin
   for iv = 0, n_elements(vars)-1 do begin 
     if(belongsto(vars[iv], my_variable))then begin
        jvars[k] = vars[iv]
        k = k + 1
     endif
    endfor
   vars = jvars[0:k-1]
  endif
 
 make_folder,dir_Local + expNames[iexp]+'/nc/'

 for iv = 71,n_elements(vars)-1 do begin   ; these are variables in the dir_run folder
 ; ====================================

  jvar = vars[iv]
  jvar0 = jvar

 ; 2.2 --- the specific folder ifor the variable to get source data, the save file for the variable

  data_folder = dir_exp   + '/' + jvar +'/' 

  filesav     = dir_Local + expNames[iexp]+'/nc/' + jvar + '.nc' 
  ; ----------------------------------------

  cmd = 'ls ' + data_folder + '*' + expNames[iexp] + '*.f09_g16.001.*.nc'  ; one ensember member

  spawn, cmd, nc_files   ; to get the nc files

  if(belongsto(jvar,['SST']))then $
    nc_files = '/glade/u/home/mzhang/C/GCM/cam/lens/monthly/SST/f.e11.FAMIPC5CN.f09_f09.historical.toga.ens01.cam.h0.SST.188001-200512.nc'
  ; ====================
  if(n_elements(nc_files) eq 1)then begin
  if(nc_files eq '')then begin
     print, 'no member files found in ', data_folder , '  ', jvar, ' ', cmd
     print, '&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
     goto, jump_next_Var
  endif 
  endif
  kk = 0
 ; 2.3 --- loop through files for the variable

 time = [0] 
 lat  = [0] 
 lon = [0] 
 lev = [0] 
 times = 0
 years = 0
 months= 0
 days = 0
 dd_att = create_struct('dummy','dummy')
  ; ============== loop throuigh files
;stop
  for k_file = 0,n_elements(nc_files)-1 do begin

   file2 = nc_files[k_file]
   print, file2
   ncdf_vars,file2,vars2, no_print =1
   ;if(not belongsto(jvar, vars2))then goto, jump_next_file

 ; 2.4 --- set default values, since some variables may not exist 

 ; default fields

 if(belongsto('time',vars2))then time = get_fld(file2,'time') 
 if(belongsto('lev',vars2))then  lev  = get_fld(file2,'lev')
 if(belongsto('plev',vars2))then lev  = get_fld(file2,'plev')
 if(belongsto('lat',vars2))then  lat  = get_fld(file2,'lat')
 if(belongsto('lon',vars2))then  lon  = get_fld(file2,'lon')
 ;static field

 ;if(jvar eq 'UBOT')then  jvar0 = 'U'

 if(not belongsto('time',vars2)) then begin ; static field

  dd = get_fld(file2,jvar0)
  dd3m = dd
  goto, jump_finished_dd3m 
 endif
 tmp = ncdf_dim(file2,jvar0)  
 if(tmp le 1) then goto, jump_next_var  ; exclude 1D array like CO2
 
 ; 2.3 --- get the time, convert to days since 1,1,0000, here the time unit is assumed to be day

 time = get_fld(file2,'time')


 yymmdd0 = get_cdf_time_since(file2, base0_day = base0_day)
 time = time + base0_day

 ; 2.4 check dates of the file and find the jj data within specified range for the mipid and expid

 year = fix(time/365.+0.99)
 dayofyear  = time - year*365.
 CALDAT, JULDAY(1, dayofyear, year), month, day

 if(max(year) lt yearrange[0]) then goto, jump_next_file
 if(min(year) gt yearrange[1]) then goto, jump_finished_file
 

 ; 2.5 --- get the field, if independent of time, finish to the end for sav

;help,jj0
;print,jj[0],cnt

 jj = where((year ge yearrange[0]) and (year le yearrange[1]),cnt)
 if(cnt lt 0)then goto, jump_next_file

 dd_att = get_cdf_att_all(file2,jvar0)
 aj = ncdf_dim(file2,jvar0,dims=dims)
 szz =[n_elements(dims),dims]

 
 if((min(szz) le 0) or (max(szz) gt 9999))then goto, jump_next_var  ; cell based 

 ;;dd     = get_fld(file2,jvar)
 ;;jj2 = jj


 dd     = get_fld(file2,jvar0, offset = jj[0], count=n_elements(jj))   ; cnt changes!!!
 jj2 = indgen(n_elements(jj))
 ; ------ the above two lines should be together  !!!

 nx = szz[1]
 ny = szz[2]
 np = szz[3]

 ; 2.6 stitch data from multiple files

 dd = reform(dd)
 szz = size(dd)
 if(kk eq 0)then begin
  times  = time[jj]
  years  = year[jj]
  months = month[jj]
  days   = day[jj]

; help,jj,jj2
  case szz[0] of
   3: begin
      szdd = size(dd)
      if(szdd[0] eq 2)then dd = reform(dd,szdd[1],szdd[2],1)
      dd2 = dd[*,*,jj2]
      szj = size(dd2)
      if(szj[0] eq 2)then dd2 = reform(dd2,szj[1],szj[2],1)
      end
   4: begin 
      szdd = size(dd)
      if(szdd[0] eq 3)then dd = reform(dd,szdd[1],szdd[2],szdd[3],1)
      dd2 = dd[*,*,*,jj2]
      szj = size(dd2)
      if(szj[0] eq 3)then dd2 = reform(dd2,szj[1],szj[2],szj[3],1)
      end
   else: dd2 = dd
  endcase

  kk=1

 endif else begin
  times  = [times, time[jj] ]
  years  = [years,year[jj] ]
  months = [months,month[jj]]
  days   = [days  ,day[jj]  ]
  case szz[0] of
   3: begin
      szdd = size(dd)
      if(szdd[0] eq 2)then dd = reform(dd,szdd[1],szdd[2],1)
      dd2 = [[[dd2]], [[dd[*,*,jj2]]] ]
      end 
   4: begin
      szdd = size(dd)
      if(szdd[0] eq 3)then dd = reform(dd,szdd[1],szdd[2],szdd[3],1)
       sz = size(dd2)
       ntw = sz[4] + n_elements(jj)
       ddd = fltarr(nx,ny,np,ntw)
       for k = 0,np-1 do begin
         dw2 = reform(dd2[*,*,k,*])
         dww = reform(dd[*,*,k,jj2])
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
 endfor  ; k_file file
 jump_finished_file:

if(kk eq 0) then begin
  print,' --- No files found in the specified time range ',yearrange
  goto, jump_next_var
  stop 
endif

 ; 2.7 prepared stitched data for post processing

  dd3m = dd2
  sz = size(dd3m)

  print,'     jvar =       ',iv, ' ',jvar
  help,  nc_files, dd3m

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

   var3 = jvar
   nc_files3 = nc_files
   climo3 = climo

   dd3m_att = dd_att


if(savenc) then begin
; ===========================
  cmd = 'cp ' + file2 +' ' + filesav
  spawn, cmd 
  cmd = 'ncks -O -d time,0,0 '+filesav + ' ' + filesav
  spawn, cmd

  syearrange = strtrim(yearrange[0],2)+'-'+strtrim(yearrange[1],2)
  casename = MIPID + '-' + MODELID + '-' + iEXPID + '-' +  syearrange
  case_desc = [MIPID, MODELID, iEXPID, syearrange]
 
  fid = ncdf_open(filesav,   /write)

  NCDF_CONTROL,fid , /REDEF
 ;-----------------------------------

;;  dim_case = ncdf_dimdef(fid,'case_specification', n_elements(case_Desc))
;;  case_id  = ncdf_vardef(fid,'case_specification',     dim_case, /string)

;  if(jvar eq 'UBOT')then begin
;   varid = NCDF_VARID(fid, 'U')
;   NCDF_VARRENAME, fid, varid, 'UBOT'
;  endif

  NCDF_ATTPUT, fid, /GLOBAL,"Author", "Monthly climatology prepared by Minghua Zhang"
  NCDF_ATTPUT, fid, /GLOBAL,"Years", strtrim(yearrange[0],2)+'-'+strtrim(yearrange[1],2)
  NCDF_ATTPUT, fid, /GLOBAL,"Casename", casename

  t_id = NCDF_VARID(fid,  'time')
  ncdf_attput,fid,  t_id,  'new_unit', 'month'
;;  case_id = NCDF_VARID(fid,  'case_specfication')

  NCDF_CONTROL, fid,  /ENDEF

;;;;  ncdf_varput,fid,  case_id,  case_desc

  t_id = NCDF_VARID(fid,  'time')
  if(t_id ge 0)then ncdf_varput,fid,  t_id,  time3

;  t_id = NCDF_VARID(fid,  'time_bnds')
;  ncdf_varput,fid,  t_id,  [[time3],[time3]]

  VarID = NCDF_VARID(fid,  jvar)
  ncdf_varput,fid, varID, dd3m

  ncdf_close,fid

;  dim_time = ncdf_dimdef(fid,'time', /unlimited)

endif else begin
; ===========================
   
   save,file=filesav,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m ,dd3m_att , yearrange3
;  help, var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m, dd3m_att, yearrange3
endelse
; ===========================
    
 print,''
   print,'filesav = ', filesav
 print,''
;stop

 jump_next_var:
 endfor

 print,''
 print,'=========> Finished ', ModelID, '  ', MIPid, '  ', iExpid
 print,''

;stop
jump_next_datatype:
ENDFOR ; iexp

jump_next_expid:
  ;print,'  end ', mipid, ' ', expid
 endfor ; expids
jump_next_mip:
 endfor ; mips
  ;print,  'end2 ', expid, ' ', mipid, ' ', model
jump_next_model:
 endfor ; models

end
