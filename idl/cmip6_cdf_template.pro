goto,jumpxx
;goto, jump_coordinates_update

; afterwards check ~/mzhangs/tmp/NCAR_CESM2_CMIP_amip_tmp.nc and copy it to /glade/u/home/mzhang/obs/FNL/Cheng/ for use
; by cmip6_cdf_data.pro
; manually copy it to 
;              /glade/u/home/mzhang/obs/FNL/Cheng/NCAR_CESM2_CMIP_amip_tmp.nc for use by cmip6_cdf_data.pro

; modify cam_fileold
; set new dimension size
; delete old fields
; add new fields
; get the new resolution from model, mip, expid, or can be specified

cam_fileold = '/glade/u/home/mzhang/obs/FNL/Cheng/FC5_test_2011.cam.h0.2011-10-10-00000.nc'


 ncdf_vars,cam_fileold, old_vars

 cmip6_vars = cmip6_vars()
 new_vars   = cmip6_vars.vars_cesm_all
 
; below info are used for extraction 
 lon  = get_fld(cam_fileold,'lon')
 lat  = get_fld(cam_fileold,'lat')
 slon = get_fld(cam_fileold,'slon')
 slat = get_fld(cam_fileold,'slat')
 lev = get_fld(cam_fileold,'lev')
 ilev = get_fld(cam_fileold,'ilev')
; time = get_fld(cam_fileold,'time')

 nlon = n_elements(lon)
 nlat= n_elements(lat)
 nslon = n_elements(slon)
 nslat= n_elements(slat)
 nlev = n_elements(lev)
 nilev = n_elements(ilev)
 nt    = n_elements(nt)

;these are the new coordinates dimensions  <===============================
 nlon2 = nlon ; or any other values
 nlat2 = nlat ; or any other values
 nslon2 = nlon2 
 nslat2 = nlat2 -1
 nlev2  = 19  ; cmip6 levels
 nilev2 = nlev2+1

 ;place holder dimensions
 lon2 = lon[0:nlon2-1]
 lat2 = lat[0:nlat2-1]
 slon2 = slon[0:nslon2-1]
 slat2 = slat[0:nslat2-1]
 lev2  = lev[0:nlev2-1]
 ilev2 = ilev[0:nilev2-1]

 lon2_range = [min(lon2)-0.1, max(lon2)+0.1]  
 lat2_range = [min(lat2)-0.1, max(lat2)+0.1]  
 slon2_range = [min(slon2)-0.1, max(slon2)+0.1]  
 slat2_range = [min(slat2)-0.1, max(slat2)+0.1]  
 lev2_range = [min(lev2)-0.1, max(lev2)+0.1]  
 ilev2_range = [min(ilev2)-0.1, max(ilev2)+0.1]  

 slab_lon = '-d lon,'+strdigit(lon2_range[0],1)+','+strdigit(lon2_range[1],1)
 slab_lat = '-d lat,'+strdigit(lat2_range[0],1)+','+strdigit(lat2_range[1],1)
 slab_slon = '-d slon,'+strdigit(slon2_range[0],1)+','+strdigit(slon2_range[1],1)
 slab_slat = '-d slat,'+strdigit(slat2_range[0],1)+','+strdigit(slat2_range[1],1)
 slab_lev = '-d lev,'+strdigit(lev2_range[0],1)+','+strdigit(lev2_range[1],1)
 slab_ilev = '-d ilev,'+strdigit(ilev2_range[0],1)+','+strdigit(ilev2_range[1],1)

 slab_str = slab_lon +' '+slab_lat +' '+slab_slon + ' ' +slab_slat + ' ' + slab_lev + ' ' + slab_ilev
 print,slab_str 

; 1  cp and extract a slab dimension from old file
; =========================================
dir = '/glade/scratch/mzhang/'
cmd = 'cp '+cam_fileold+ ' /glade/scratch/mzhang/in.nc'
spawn, cmd

out1 = '/glade/scratch/mzhang/out1.nc'
cmd = 'ncks -O '+ slab_str +' ' +'/glade/scratch/mzhang/in.nc '+out1

spawn,cmd

jj = where(old_vars eq 'Z500') ; visual print to find thelast one
nv_end = jj[0] 

; 2 delete fields that are in hist_vars but not in vars_in
; =========================================================
k=0
for iv = 0, nv_end do begin
  old_var = old_vars[iv]
  if(old_var eq 'P0') then goto,jump_next_delete
  if(not belongsto(old_var,new_vars))then begin
    dd = get_fld(out1,old_var)
    sz = size(dd)
    if(sz[0] ge 2)then begin
       if(k eq 0)then slab = old_var else slab = slab+ ','+old_var
       k = k+1
       print, '   deleted old_var', iv, ' ', old_var
     endif
  endif
jump_next_delete:
endfor

; 2.1 additional fields to delete after Z500
; =========================================================
deletes = ['bc_','dst', 'ncl', 'num', 'pom', 'so4', 'sol', 'soa']

for iv = nv_end,n_elements(old_vars)-1 do begin
  old_var = old_vars[iv]
  if(belongsto(strmid(old_var,0,3), deletes))then begin
     slab = slab+ ','+old_var
       print, '   deleted old_var', iv, ' ', old_var
  endif
endfor
cmd = 'ncks -O -x -v ' +slab+' '+ out1 +' '+ out1
; ==================================================
print,cmd
spawn,cmd

; 3. add new fields from cmips
; =========================================================
for iv = n_elements(new_vars)-1,0,-1 do begin  ; order reversed in cdf
  var = new_vars[iv]
  if(not belongsto(var, old_vars))then begin  ; new fields to cam standard history tape
    if(belongsto(var, cmip6_vars.vars_delete))then goto,jump_next_add
    if(belongsto(var, cmip6_vars.vars3d))then begin
       varr = 'T'     ; use T as the modelled field
    endif else begin
       varr = 'TS'    ; use TS as the modelled field
    endelse
    cmd = 'ncap2 -O -s '+"'"+var+'=array(1.0f,1.,'+varr+')'+"'"+' '+ out1 + ' '+ out1
    spawn,cmd
    print,' added new var', iv, '  ',var
   endif
jump_next_add:
endfor
    cmd = 'ncap2 -O -s '+"'"+'PSS'+'=array(1.0f,1.,TS)'+"'"+' '+ out1 + ' '+ out1   ;PS0 added to replace PS for diagnostic package
    spawn,cmd
    print,' added new var', iv, '  PSS '



restore,'cmip6_macro.sav'

; 4. update cdf file, attributes etc., these can be modified later
; =====================================================================================
; =====================================================================================

my_model = 'NOAA-GFDL/GFDL-CM4'
my_model = 'NCAR/CESM2'
my_k     = get_cmip6_model_number(my_model)

;-------
my_mip = 'CMIP'
;my_mip = 'CFMIP'
;my_mip = 'GeoMIP'
;my_mip = 'ScenarioMIP'
my_m     = get_cmip6_mip_number(my_mip)
;-------

;my_Exp = 'ssp126'
my_Exp = 'historical'
my_Exp = 'amip'
my_e     = get_cmip6_exp_number(my_Exp, my_mip)
;-------

; ====================

  ; GRAND LOOP

;;for imodel = 0, n_elements(cmip6.models)-1 do begin
for imodel = my_k,my_k do begin
; ^^^^^^----------------------------------
   Model = cmip6.models[imodel]
   Model0 = Model
   print, imodel, '   ', model

;for imip   = 0, n_elements(cmip6.mips) -1 do begin 
for imip   = my_m,my_m do begin  ; 16 ScenaroMIP, 
; ^^^^^^----------------------------------

   MIPid = cmip6.mips[imip]

   jj    = where(tag_names(cmip6) eq strupcase(MIPid), cnt)
   if(cnt eq 0)then goto, jump_next_mip  ; Expids not yet defined in cmip6_macro.pro 
   k = jj[0]
   exps = cmip6.(k)

;;for iExpid = 0, n_elements(exps)-1 do begin
for iExpid = my_e, my_e  do begin
; ^^^^^^--------------------------------
   Expid = cmip6.(k)[iExpid]   ; CFMIP - > a4SST etc.

   print,'imodel, imip, iexp ', imodel,' ',  imip,' ', iexpid,' ', model, ' ', mipid, ' ',expid

if(not belongsto(MIPid,['CMIP','CFMIP','ScenarioMIP'])) then goto,jump_next_mip

   case MIPid of
 'CMIP':begin
        end
 'CFMIP': begin
         if(not belongsto(Expid,['amip-m4K','amip-p4K','amip-4xCO2','amip-a4SST-4xCO2'])) $
             then goto,jump_next_expid
        end
 'ScenarioMIP': begin
        end
   endcase

; prepare output file
 scratch = '/glade/work/mzhang/C/'
 dir_model = scratch + model.replace('/','_')
 dir_mip   = dir_model + '/' + mipid
 dir_exp   = dir_mip   + '/' + expid+'_climo'
 file_cdf  = dir_exp 

  restore,file_cdf+'/ts.sav'
  ; get the dimensions of lon lat,lev, plev 

; These are the values of the new dimensions
;  =======================================
  lon2 = lon
  lat2 = lat
  lev2 = reverse(plev/100.)   ; reversed order
  slon2 = lon2 + (lon2[1] - lon[0])*0.5       ;
  slon2 = lat2[1:*] - (lat2[1] - lat[0])*0.5
  ilev2 = [lev2 + 1, lev2[n_elements(lev2)-1] -1 ]

   p0 = 1000.0
   hybm2 = lev2/p0
   hyam2 = lev2*0.0
   hybi2 = ilev2/p0
   hyai2 = hybi2*0.0

stop
;  =======================================
jumpxx:

temp_out1 = '/glade/scratch/mzhang/out1.nc'


fid = ncdf_open(temp_out1,/write)
NCDF_CONTROL, fid,  /REDEF

  NCDF_ATTPUT, fid, /GLOBAL, "title", 'CMIP6'
  NCDF_ATTPUT, fid, /GLOBAL,"case", 'AMIP'
  NCDF_ATTPUT, fid, /GLOBAL,"logname", 'Minghua Zhang'
  NCDF_ATTPUT, fid, /GLOBAL,"Note", 'PSS is added to replace PS so that PS can be set to 100000. for the diagnostic package to work for CMIP6 pressure levels'

  NCDF_ATTDEL, fid, /GLOBAL,"source"
  NCDF_ATTDEL, fid, /GLOBAL,"initial_file"
  NCDF_ATTDEL, fid, /GLOBAL,"Conventions"
  NCDF_ATTDEL, fid,/GLOBAL, "revision_Id"
  NCDF_ATTDEL, fid, /GLOBAL,"topography_file"
  NCDF_ATTDEL, fid, /GLOBAL,"history"
  NCDF_ATTDEL, fid, /GLOBAL,"host"
  NCDF_ATTDEL, fid, /GLOBAL,"Version"
  NCDF_ATTDEL, fid, /GLOBAL,"NCO"

NCDF_CONTROL, fid,  /ENDEF  ;!!!!

VarID = NCDF_VARID(fid, 'lon')
    ncdf_varput,fid, varID, lon2
VarID = NCDF_VARID(fid, 'lat')
    ncdf_varput,fid, varID, lat2 
VarID = NCDF_VARID(fid, 'slon')
    ncdf_varput,fid, varID, slon2
VarID = NCDF_VARID(fid, 'slat')
    ncdf_varput,fid, varID, slat2
VarID = NCDF_VARID(fid, 'lev')
    ncdf_varput,fid, varID, lev2
VarID = NCDF_VARID(fid, 'ilev')
    ncdf_varput,fid, varID, ilev2
VarID = NCDF_VARID(fid, 'hyam')
    ncdf_varput,fid, varID, hyam2
VarID = NCDF_VARID(fid, 'hybm')
    ncdf_varput,fid, varID, hybm2
VarID = NCDF_VARID(fid, 'hyai')
    ncdf_varput,fid, varID, hyai2
VarID = NCDF_VARID(fid, 'hybi')
    ncdf_varput,fid, varID, hybi2

 ncdf_close,fid
;stop

; assign values of zero
ncdf_vars,temp_out1,new_vars
FileID = ncdf_open(temp_out1,/write)
NCDF_CONTROL,fileID , /FILL
 for iv = 0, n_elements(new_vars)-1 do begin
   var1 = new_Vars[iv]
   VarID = NCDF_VARID(fileID,var1) 
   ncdf_varget,fileID, varID, dd
   sz = size(dd)
   if(sz[0] ge 2)then dd = dd*0.0   ; assign zero values
   ncdf_varput,fileID, varID, dd
endfor
   var1 = 'PS'
   VarID = NCDF_VARID(fileID,'PS')
   ncdf_varget,fileID, varID, dd

   ncdf_varput,fileID, varID, dd*0.0 + 1.0e5  ; 1000 mb

   VarID = NCDF_VARID(fileID,'PSS')
   ncdf_varput,fileID, varID, dd*0.0
 ncdf_close,FileID

print,'out1',out1
jump_coordinates_update:
 
 tmp_file = '~/mzhangs/tmp/'+model.replace('/','_')+'_'+mipid+'_'+Expid+'_tmp.nc'
 cmd = 'cp '+temp_out1 + ' ' + tmp_file
 spawn,cmd

 

 print,'ncdf_template file saved in ', tmp_file

stop

jump_next_expid:
  ;print,'  end ', mipid, ' ', expid
 endfor ; expids
jump_next_mip:
 endfor ; mips
  ;print,  'end2 ', expid, ' ', mipid, ' ', model
jump_next_model:
 endfor ; models

end

