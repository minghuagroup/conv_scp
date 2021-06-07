
; =====================================================================================
;  PRO NC2NC, dir_sav, dir_nc,    ; FROM individual nc files to a single file  dir_sav is the input directory, run from cmip2nc.pro
; =====================================================================================
; =====================================================================================
; 
; purpose from one data sav folder to write netcdf file
;
restore, 'cmip6_macro.sav'

MODELID ='NOAA-GFDL_GFDL-CM4'
MODELID ='NCAR_CESM2' 

; models can be looped here too

MIPID   = 'CFMIP'
EXPS1  = str_sep(strcompress('amip-4xCO2 amip-a4SST-4xCO2 amip-future4K amip-lwoff amip-m4K amip-p4K amip-p4K-lwoff amip-piForcing'),' ')
EXPS2 = str_sep(strcompress('aqua-4xCO2 aqua-control aqua-control-lwoff aqua-p4K aqua-p4K-lwoff'),' ')
EXPs = [EXPS1, EXPS2]

MIPID = 'CMIP'
EXPS = ['historical', 'piControl', 'amip','abrupt-4xCO2' ] 

MIPID = 'ScenarioMIP'
EXPs = ['ssp585']

;^^^^ 1  ^^^^^^^^^^^^^^^^^^^^^^^^^
; =================================================================================

; ======== BIG LOOP
FOR IEXP = 0, N_ELEMENTS(EXPS)-1 DO BEGIN

 EXPID = EXPS[IEXP]

expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID
dir_nc   = expdir + '/'
ncfile   = dir_nc + 'climo.nc'

dir_sav    = expdir+'/nc/'  ;  default is nc
print,file_exist(dir_Sav)
if(not file_exist(dir_Sav))then $
  dir_sav    = expdir+'/sav/'  ; already prepared

dir_sav2   = expdir+'/sav2/'  ; already or to be prepared  below or by mycmip6_add.pro
dirs_sav   = [dir_sav, dir_sav2]


sav2_exist = 0
;; &&&&&&&&&&&&
;;sav2_exist = file_exist(dir_sav2+'/H.sav')        ; commented out this if need reinteroplation
;=================== put added data here
if((strpos(modelid, 'CESM2') ge 0) and (not sav2_exist))then begin
 make_folder,dir_sav2 
 cmip2add,  dir_sav, dir_sav2        ; make sure necessary sav/*.sav files exist, cmip6 variables
 cmip2fill, dir_sav, dir_sav2        ; make sure H.sav is deleted when re-inteopolate is needed sav/*.sav files exist
endif
;=================== put added data here

print, ' from dir_sav = ', dirs_sav, ' to dir_nc = ', dir_nc

;goto, jump_tmp
; -----------------------------------------------------------------
; 1 basic coordinates of NCDF file
; -----------------------------------------------------------------

 sav_type = 0
 if(strpos(strstrip(dirs_sav[0], '/', last=1), '/sav') gt 0) then sav_type = 1 

 varjj = 'ta'
 if(not file_exist(dirs_sav[0]+varjj+'.*' ))then varjj = 'ua'
 dd3m = get_dd3m(dir_sav,varjj, sav_type, lon3=xx0, lat3 = yy0, lev3 = zz0, time3 =time0 )
 if(n_elements(dd3m) le 1)then dd3m = get_dd3m(dir_sav,'ts', sav_type, lon3=xx0, lat3 = yy0, lev3 = zz0, time3 =time0 )

;;if(sav_type) then begin
;;  file_tmp = dir_sav+'ta.sav'
;;  if(not file_exist(file_tmp)) then file_tmp = dir_sav+'ts.sav'
;;  restore, tile_tmp
;; xx0 = lon3
;; yy0 = lat3
;; zz0 = lev3
;; time0 = time3 
;;endif else begin
  
;;  file_tmp = dir_sav+'ta.nc'
;;  filein = file_tmp
;;  if(file_exist(file_tmp)) then begin
;;   ncdf_Vars, filein, vars_tmp, no_print=1
;;     if(belongsto('lev',vars_tmp))then begin
;;        lev3 = get_fld(filein,'lev')
;;     endif else begin
;;        lev3 = get_fld(filein,'plev')
;;     endelse
;;    zz0 = lev3
;;  endif else begin
;;    file_tmp = dir_sav+'ts.nc'  ; or another 2d file
;;    zz0 = [1000.]
;;  endelse  
;; xx0 = get_fld(file_tmp,'lon')
;; yy0 = get_fld(file_tmp,'lat')
;; time0 = get_fld(file_tmp,'time')
;;endelse

; data from running cmip2sav.pro  
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

 nx = n_elements(xx0)
 ny = n_elements(yy0)
 nz = n_elements(zz0)
 nt = n_elements(time0)
 size0 = size(dd3m)
 if(max(zz0) gt 2000.)then zz0 = zz0/100.
 if(max(zz0) lt 20.)then zz0 = zz0*1000
; 
fid = ncdf_create(ncfile,   /clobber)

  NCDF_ATTPUT, fid, /GLOBAL, "title", 'CMIP6 Climatology'
  NCDF_ATTPUT, fid, /GLOBAL,"case", 'AMIP'
  NCDF_ATTPUT, fid, /GLOBAL,"logname", 'Minghua Zhang'
  NCDF_ATTPUT, fid, /GLOBAL,"Note", 'test'

dim_time = ncdf_dimdef(fid,'time', /unlimited)
dim_x    = ncdf_dimdef(fid,'lon',nx)
dim_y    = ncdf_dimdef(fid,'lat',ny)
dim_z    = ncdf_dimdef(fid,'lev',nz)


x_id    = ncdf_vardef(fid,'lon',dim_x,/float)
y_id    = ncdf_vardef(fid,'lat', dim_y,/float)
z_id    = ncdf_vardef(fid,'lev', dim_z,/float)
t_id    = ncdf_vardef(fid,'time',     dim_time,/float)

NCDF_CONTROL, fid,  /ENDEF

 ncdf_varput,fid,  x_id,  xx0
 ncdf_varput,fid,  y_id,  yy0
 ncdf_varput,fid,  z_id,  zz0
 ncdf_varput,fid,  t_id,  time0

; -----------------------------------------------------------------
; 2 loop through .sav folder and variables for write into  NCDF file
; -----------------------------------------------------------------

 isav2 = 1
 if(strpos(modelid,'GFDL') ge 0)then isav2=0

 for isav = 0,1 do begin 
; ============

 ; second folder with sav2 so sav_type  = 1
 sav_type = 0
 if(strpos(strstrip(dirs_sav[isav], '/', last=1), '/sav') gt 0) then sav_type = 1 

  if(sav_type)then     jfiles = file_search(dirs_sav[isav]+'*.sav') else $
   jfiles = file_search(dirs_sav[isav]+'*.nc')
  
 jvars  = get_lastv2_inlist(jfiles, '/', '.', sep3 = '_')

 for iv = 0, n_elements(jfiles)-1 do begin
; for iv = 42, 50 do begin
; ===========

  input_fname = jfiles[iv]
  var0        = jvars[iv]
  if(strpos(var0,'co2mass') ge 0)then goto,jump_next_var
  var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose

dd3m = get_dd3m(dirs_sav[isav],var0, sav_type, lon3=xx3, lat3 = yy3, lev3 = zz3, time3 =time3 , dd3m_att = dd3m_att)

;;IF(sav_type)THEN BEGIN
;;  restore, input_fname
;;  xx3 = lon3
;;  yy3 = lat3
;;  zz3 = lev3
;;  sz = size(dd3m)
;;;   ====================
;;; ---------------------------------
;;; data from running cmip2sav.pro  
;;; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;;;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att
;;ENDIF ELSE BEGIN
;; 
;;  dd3m   = get_fld(input_fname,var0)
;;  sz = size(dd3m)
;;  if(sz[0] le 1)then goto, jump_next_var
;;  
;;
;;  dd_att = get_cdf_att_all(input_fname,var0)
;;  dd3m_att = create_struct('atts', dd_att)
;;
;;  xx3 = get_fld(input_fname,'lon')
;;  yy3 = get_fld(input_fname,'lat')
;;  if(sz[0] eq 4)then begin
;;   filein = input_fname
;;      ncdf_Vars, filein, vars_tmp, no_print =1
;;     if(belongsto('lev',vars_tmp))then begin
;;        lev3 = get_fld(filein,'lev')
;;      endif else begin
;;        lev3 = get_fld(filein,'plev')
;;      endelse
;;    zz3 = lev3
;;   endif else begin 
;;    zz3=[1000.0]
;;   endelse
; =========================================
;;ENDELSE

 sz = size(dd3m)
 if(sz[0] le 1)then goto, jump_next_var

  print, isav, ' ', iv,' ', ' var0 ',var0, '  var_cam ',  var_cam, ' ', min(dd3m),'  ', max(dd3m),'  ', get_lastv_inlist(input_fname,'/')

 zz3 = abs(zz3)
 if(max(zz3) gt 2000.)then zz3 = zz3/100.
 if(max(zz3) lt 20.)then zz3 = zz3*1000


 if(not array_equal(xx3,xx0))then help,xx0, yy0, xx3,yy3
 if(sz[0] eq 4 and not array_equal(zz3,zz0)) then help, zz0, zz3

hh = (array_equal(xx0, xx3) and array_equal(yy0, yy3))

NCDF_CONTROL, fid,  /REDEF
; =======================

;sz[0] = 1000

; -----------------------------------------------------------------
; 2.1  define var_id, using .sav variables  and interpolate to common grids
; -----------------------------------------------------------------
  case sz[0] of
 2: begin 
      var_id=ncdf_vardef(fid, var0 , [dim_x, dim_y], /float)

      if(not hh)then begin
        dd3m = my_interpol2(dd3m, xx3,yy3,xx0,yy0, tdim=0, noextra =  1)    ; modify the my_interpolX programs
      endif
    end
 3: begin 
      var_id=ncdf_vardef(fid, var0 , [dim_x, dim_y, dim_time], /float)
      if(not hh)then begin
        dd3m = my_interpol2(dd3m, xx3,yy3,xx0,yy0, tdim=1, noextra =  1)    ; modify the my_interpolX programs
      endif
    end
 4: begin 
      var_id=ncdf_vardef(fid, var0 , [dim_x, dim_y, dim_z, dim_time], /float)

      if((not hh) or (not array_equal(zz0, zz3)))then begin

        if((modelid eq 'NCAR_CESM2') and (EXPID eq 'historical'))then begin 
           dd3m = reverse(dd3m, 3)          ; to correct a mistake in the history tape
         endif

        dd3m = my_interpol3d(dd3m, xx3,yy3,zz3,xx0,yy0,zz0, tdim=1, noextra =  1)    ; modify the my_interpolX programs
;print,reform(dd3m[100,100,*,0]) 
;stop
      endif
    end
 else:
 endcase

; -----------------------------------------------------------------
; 2.2  write attributes if appropriate
; -----------------------------------------------------------------
;; if(isav eq 0)then begin  ; only the .sav folder has attributes, the .sav2 folder is derived fields
;;    atts  = dd3m_att.atts
;;    names = str_sep(atts.names,':')
;;    natt  = n_elements(names)

;;    fillValue = get_stru(dd3m_att,'_FillValue')
;;    NCDF_ATTPUT, fid, var_id, '_FillValue', Float(fillValue), /FLOAT
;;    for i = 2, natt-1 do begin
;;      value = get_stru(dd3m_att,names[i])
;;      NCDF_ATTPUT, fid, var_id, names[i], value
;;    endfor
;; endif
 
NCDF_CONTROL, fid,  /ENDEF
; =======================

; -----------------------------------------------------------------
; 2.3 put data into NCDF file 
; -----------------------------------------------------------------
    ncdf_varput,fid, var_id, dd3m
    

 jump_next_var:
 endfor; iv
endfor ; isav

; -----------------------------------------------------------------
; 2.3 close  NCDF file 
; -----------------------------------------------------------------
    ncdf_close, fid
    print,'saved ', ncfile


jump_tmp:
; -----------------------------------------------------------------
; 3 write monthly and seasonal files
; -----------------------------------------------------------------

 ncdf_season, ncfile, remove=1

ENDFOR ; IEXP
print,' '
print,'   Program Finished Normally!'
print,' '
end
