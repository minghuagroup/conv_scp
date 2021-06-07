
; =====================================================================================
;  PRO SAV2NC, dir_sav, dir_nc, 
; =====================================================================================
; =====================================================================================
; 
; purpose from one data sav folder to write netcdf file
;

separate_files = 1
sav2_exist     = 1
write_seasonal = 0

MODELID ='NCAR_CESM2'


MIPID = 'CMIP'
EXPID = 'amip'
EXPID = 'historical'
EXPID = 'piControl'
EXPID = 'abrupt-4xCO2'


; models can be looped here too

restore, 'cmip6_macro.sav'

MIPID = 'CFMIP'
EXPS = [CMIP6.CFMIP_STRU.AMIP, CMIP6.CFMIP_STRU.AQUA]


MIPID = 'ScenarioMIP'
EXPID = 'ssp585'
exps = [EXPID]
;; ======== BIG LOOP
FOR IEXP = 0, N_ELEMENTS(EXPS)-1 DO BEGIN

 EXPID = EXPS[IEXP]

expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID
dir_nc   = expdir + '/'
;ncfile   = dir_nc + 'climo.nc'
if(separate_files) then make_folder, dir_nc + '/nc'

dir_sav    = expdir+'/sav/'  ; already prepared
dir_sav2   = expdir+'/sav2/'  ; already or to be prepared  below or by mycmip6_add.pro
dirs_sav   = [dir_sav, dir_sav2]

;sav2_exist = file_exist(dir_sav2+'/H.sav') 
if(not sav2_exist)then begin
 make_folder,dir_sav2 
 cmip2add,  dir_sav, dir_sav2 
 cmip2fill, dir_sav, dir_sav2
endif
;=================== put added data here

print, ' from dir_sav = ', dirs_sav, ' to dir_nc = ', dir_nc

; -----------------------------------------------------------------
; 2 loop through .sav folder and variables for write into  NCDF file
; -----------------------------------------------------------------

 for isav = 1,1 do begin 
; ============
    jfiles = file_search(dirs_sav[isav]+'*.sav')
    jvars  = get_lastv2_inlist(jfiles, '/', '.', sep3 = '_')

 for iv = 0, n_elements(jfiles)-1 do begin

;goto, jump_tmp
; -----------------------------------------------------------------
; 1 basic coordinates of NCDF file
; -----------------------------------------------------------------

  input_fname = jfiles[iv]
  var0        = jvars[iv]
  var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose

;;  if(not belongsto(var0, ['cl','clw','cli']))then goto, jump_next_var
  if(not belongsto(var0, ['CLOUD','CLDICE','CLDLIQ']))then goto, jump_next_var
  

 if(separate_files or ((isav+iv) eq 0)) then begin

if(separate_files and belongsto(var0,['cli','clw','cl'])) then restore, dir_sav+'cl.sav' else $
 restore, dir_sav+'ta.sav'

; data from running cmip2sav.pro  
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

 xx0 = lon3
 yy0 = lat3
 zz0 = lev3
 time0 = time3 
 nx = n_elements(xx0)
 ny = n_elements(yy0)
 nz = n_elements(zz0)
 nt = n_elements(time0)
 size0 = size(dd3m)
 if(max(lev3) gt 2000.)then zz0 = lev3/100.
 if(max(lev3) lt 20.)then zz0 = lev3*1000
; 
 endif ; separate or first
 if(separate_files)then begin
    ncfile = dir_nc + '/nc/'+jvars[iv]+'.nc'
    if(belongsto(var0, ['T','Q','U','V','RELHUM','OMEGA', 'Z3']))then goto, jump_next_var
 endif

 if((strpos(ncfile, 'CFMIP') ge 0) and $
     belongsto(var0, ['ORO','LANDFRAC','PHIS','SST','SNOWHLND','ICEFRAC','LANDFRAC']))then goto, jump_next_var

restore, input_fname
sz = size(dd3m)


 fid = ncdf_create(ncfile,   /clobber)

  NCDF_ATTPUT, fid, /GLOBAL, "title", 'CMIP6 Climatology'
  NCDF_ATTPUT, fid, /GLOBAL,"case", 'AMIP'
  NCDF_ATTPUT, fid, /GLOBAL,"logname", 'Minghua Zhang'
  NCDF_ATTPUT, fid, /GLOBAL,"Note", 'test'

dim_time = ncdf_dimdef(fid,'time', /unlimited)
dim_x    = ncdf_dimdef(fid,'lon',nx)
dim_y    = ncdf_dimdef(fid,'lat',ny)
if((not separate_files) or (sz[0] eq 4))then $
dim_z    = ncdf_dimdef(fid,'lev',nz)

x_id    = ncdf_vardef(fid,'lon',dim_x,/float)
y_id    = ncdf_vardef(fid,'lat', dim_y,/float)
if((not separate_files) or (sz[0] eq 4))then $
z_id    = ncdf_vardef(fid,'lev', dim_z,/float)
t_id    = ncdf_vardef(fid,'time',     dim_time,/float)

NCDF_CONTROL, fid,  /ENDEF

 ncdf_varput,fid,  x_id,  xx0
 ncdf_varput,fid,  y_id,  yy0
if((not separate_files) or (sz[0] eq 4))then $
 ncdf_varput,fid,  z_id,  zz0
 ncdf_varput,fid,  t_id,  time0

; ---------------------------------
; data from running cmip2sav.pro  
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

  print, isav, ' ', iv,' ', ' var0 ',var0, '  var_cam ',  var_cam, ' ', [min(dd3m),max(dd3m)],' ', input_fname

 sz = size(dd3m)
 if(sz[0] le 1)then goto, jump_next_var

 xx3 = lon3
 yy3 = lat3
 zz3 = lev3

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

    if(not separate_files)then begin  ; separate files to keep the original data
      if((not hh) or (not array_equal(zz0, zz3)))then begin

        if((modelid eq 'NCAR_CESM2') and (EXPID eq 'historical'))then begin 
           dd3m = reverse(dd3m, 3)          ; to correct a mistake in the history tape
         endif
        dd3m = my_interpol3d(dd3m, xx3,yy3,zz3,xx0,yy0,zz0, tdim=1, noextra =  1)    ; modify the my_interpolX programs
     endif ;
;print,reform(dd3m[100,100,*,0]) 
;stop
      endif
    end
 else:
 endcase

; -----------------------------------------------------------------
; 2.2  write attributes if appropriate
; -----------------------------------------------------------------
 if(isav eq 0)then begin  ; only the .sav folder has attributes, the .sav2 folder is derived fields
    atts  = dd3m_att.atts
    names = str_sep(atts,':')
    natt  = n_elements(names)

    fillValue = get_stru(dd3m_att,'_FillValue')
    NCDF_ATTPUT, fid, var_id, '_FillValue', Float(fillValue), /FLOAT
    for i = 2, natt-1 do begin
      value = get_stru(dd3m_att,names[i])
      NCDF_ATTPUT, fid, var_id, names[i], value
    endfor
 endif
 
NCDF_CONTROL, fid,  /ENDEF
; =======================

; -----------------------------------------------------------------
; 2.3 put data into NCDF file 
; -----------------------------------------------------------------
    ncdf_varput,fid, var_id, dd3m
    
    ncdf_close, fid
    if(separate_files) then print,'saved ', ncfile

 jump_next_var:
 endfor; iv
endfor ; isav
    if(not separate_files) then print,'saved ', ncfile

; -----------------------------------------------------------------
; 2.3 close  NCDF file 
; -----------------------------------------------------------------


jump_tmp:
; -----------------------------------------------------------------
; 3 write monthly and seasonal files
; -----------------------------------------------------------------

 if(write_seasonal)then  ncdf_season, ncfile, remove=1

ENDFOR ; IEXP

end
