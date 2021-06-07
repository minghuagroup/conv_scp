; One file or folder
; NC3FIGS for 12 month files 
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&7

 PRO NC2FIG, ncfile_in, dir_jpeg = dir_jpeg, icolor=icolor, color_dir_jpeg = dir_jpeg_color_in, $
            SS = SS, setZ = setZ, map = map, adjust_lev = adjust_lev, diff = diff,      $
            make_basic_plots = make_basic_plots,  make_3dlevel_plots = make_3dlevel_plots,  $
            make_tau_plots   = make_tau_plots,    make_uv_plots      = make_uv_plots, $
            std_levels = std_levels, cntl_file = cntl_file, cntl_map = cntl_map, $
            vars_only = vars_only, $
            expdir = expdir

; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&7

; Example:
;
; ncfile_in ='/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo.nc'
; NC3FIG, ncfile_in, vars_only = ['FLNT','TAUX','LWCF'], setZ = 0
; 
; THIS PROGRAM WORKS FOR A SINGLE FILE OR A SINGLE FOLDER, BUT ONE SLICE of time
; vars_only, cam names
; map is to translate TAU back to the file type
; cntl_map is to translate test type to cntl type:  cntl_map  = 'cmip2era', cesm2era for difference with era

; ===================================================================================
; set parameters below
; ===================================================================================

;; setZ               = 0  ; run in background batch mode
;; make_basic_plots   = 1
;; make_3dlevel_plots = 1
;; make_tau_plots     = 1
;; make_uv_plots      = 1
;; make_2d_plots      = 0

;; std_levels  = [500.]
;; std_levels  = [1000., 850., 500.,200]
;^^^^ 2 ^^^^^^^^^^^^^^^^^^^^^^^^^

;; icolor          = 1
;; SS             = 'DJF'
;; dir_jpeg_color  = '../cmip6/CMIP/NCAR_CESM2/amip/'         +SS + '/'   ; this is the input folder
;; dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/'+SS + '/'   ; this is the input folder


if(not keyword_Set(expdir))then begin
 if(file_search(ncfile_in,/test_dir))then expdir=ncfile_in else $
 expdir    = strstrip(ncfile_in,'/',last=1) ; to put in jpeg folder underneath
endif

if(not keyword_Set(dir_jpeg)) then begin
  expid     = get_lastv2_inlist(expdir,'/','.')
  dir_jpeg  = expdir+'/jpeg_'+expid + '/'
end

if(not keyword_Set(icolor))then begin
 icolor=0
 level = 1
endif else begin
  if(not keyword_Set(dir_jpeg_color_in ))then begin
      dir_jpeg_color_in = '../cmip6/GCM/cam5/BRCP85/'
  endif
 level = 0
endelse
if(diff)then level = 0

if(not keyword_Set(SS)) then SS  = 'DJF'
if(not keyword_Set(setZ))then setZ = 0
if(not keyword_Set(map)) then map  = 'cesm2cesm'
if(not keyword_Set(adjust_lev))then adjust_lev = 0
if(not keyword_Set(diff))      then diff       = 0
if(not keyword_Set(make_basic_plots))  then make_basic_plots   = 0
if(not keyword_Set(make_3dlevel_plots))then make_3dlevel_plots = 0
if(not keyword_Set(make_tau_plots))    then make_tau_plots     = 0
if(not keyword_Set(make_uv_plots))then make_uv_plots = 0
if(not keyword_Set( std_levels))       then  std_levels = [1000., 850., 500.,200]
if(not keyword_Set(cntl_file)) then cntl_file = 0
if(not keyword_Set(cntl_map))  then cntl_map  = 'cesm2cesm'
;if(not keyword_Set(nseasons))then begin nseasons = indgen(5)
;if(not keyword_Set(


type = strstrip(map,'2')

if(type eq 'era')then begin
 restore, 'scale_era.sav0'  ; era_lev structure
 scale_names = tag_names(scale_era)
endif else begin
 restore, 'scale_cam.sav0'  ; cam_lev structure
 scale_names = tag_names(scale_cam)
endelse
; ===================================================================================
scale = 1.0
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&77

    ; icolor = 0                      ; these two lines must be set together
    ; dir_jpeg_color  = dir_jpeg      ;  internally generated for DJF

;^^^^ 4 ^^^^^^^^^^^^^^^^^^^^^^^^^

; ===================================================================================

 make_folder, dir_jpeg

print, ' from  ncfile_in     = ', ncfile_in
print, ' to    dir_jpeg   = ', dir_jpeg

;---------------------------------------------------------------------------------
; 2. get coordinates, interpolated already by sav2nc.pro
;---------------------------------------------------------------------------------

 jvars = ncdf_vars2d(ncfile_in, all_ncfiles = ncfiles)
; ================================================== 
 nv = n_elements(jvars)
 if(n_elements(ncfiles) eq 1)then ncfiles = replicate(ncfiles,nv)

 jj      = sort(jvars)
 jvars   = jvars[jj]
 ncfiles = ncfiles[jj]

; 	THE ABOVE IS THE HEAD

;---------------------------------------------------------------------------------
; 3.1 basic plots, loop through all variables to make map and cross section plots
;---------------------------------------------------------------------------------


 FOR iv = 0, n_elements(jvars)-1 DO BEGIN 
; ====================================
 
 ncfile = ncfiles[iv]

 var0        = jvars[iv]
 var_cam     = var_map(var0, 'all2cesm')   ; for unit and title purpose
 vart        = var_cam + ' (' + var0 + ')'    

  
  sskip = strmid(var0,0,3)

  skip = 0
  skip = blt(sskip, ['SFD','SFH','SFN','SFS', 'SO2','GS_','AQ_','ODV_','AOD','BUR','ODV','AQS','RAM','XPH'])
  skip = skip or (strupcase(var0) ne var0)

;  print,'skip1 ',iv, ' ', skip, ' ',var0
  if(skip)then goto,jump_next_var


 if(keyword_Set(vars_only))then begin
    if(not belongsto(var_cam, vars_only)) then goto, jump_next_var
 endif 

 print,iv, var0

 dd_factor = 1.0
 if(belongsto(var0, scale_names))then dd_Factor = get_stru(scale_cam,var0)

 dd = get_dd2m(ncfile, var0, lon3 = xx2, lat3 = yy2, lev3 = zz2, cntl_file=cntl_file, cntl_map = cntl_map) 
 dd = dd * dd_factor

 doilev = 0
 doilev = blt(strmid(var0,0,3),['KVH','KVM','QTF','SLF','TKE','UFL','VFL'])

 if(blt(strmid(var0,0,5),['CMFMC']))then doilev = 1


 if(doilev)then zz2 = get_fld(ncfile, 'ilev')

 skip = (n_elements(dd) le n_elements(xx2)) or (max(dd) eq min(dd))

 if(skip)then goto, jump_next_var

 sz = size(dd)

 print,'  ', iv, ' ',var0, ' ', var_cam,min(dd),max(dd)

 dd = less_100(dd, var_cam)
 dd = reform(dd)

 uu_exist = 0
 vv_exist = 0
 Z3_exist = 0

if(make_uv_plots)then begin
   if(var_cam eq 'U')then begin uu00 = dd & uu_exist = 1 & endif
   if(var_cam eq 'V')then begin vv00 = dd & vv_exist = 1 & endif
   if(var_cam eq 'Z3')then begin Z300 = dd & Z3_exist = 1 & endif
endif

 taux_exist = 0
 tauy_exist = 0
 psl_exist = 0
if(make_tau_plots)then begin
   if(var_cam eq 'TAUX')then begin taux00 = dd & taux_exist = 1 & endif
   if(var_cam eq 'TAUY')then begin tauy00 = dd & tauy_exist = 1 & endif
   if(var_cam eq 'PSL')then begin psl00 = dd & psl_exist = 1 & endif
endif


;====================================================

if(make_basic_plots)then begin
   
 iave = 3
 if(sz[0] eq 3)then iave =1 

 file_jpeg  = dir_jpeg +  var0 + '.jpeg'
 file_jpeg_color_out = dir_jpeg + var0 + '.color'
 ;file_jpeg_color_in  = dir_jpeg_color + var0 + '.color'
 if(icolor eq 0)then begin
    file_jpeg_color_in  = expdir +SS +'/' + var0 + '.color'
 endif else begin
    file_jpeg_color_in  = dir_jpeg_color_in + '/' + SS +'/' + var0 + '.color'
 endelse


 get2d, vart, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
       ,title= title, xtitle=xtitle, ytitle=ytitle ,xrange=xrange, yrange=yrange

 if(icolor and file_exist(file_jpeg_color_in)) then begin
    restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev_cam(aa, var0, scale, diff=diff, level=level) 
    lev2 = [1.0e10,1.0e20]
 endelse
 if(min(lev1) eq max(lev1)) then goto, jump_next_var

 view2d,vart,aa, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange $
           , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=0, setZ=setZ

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale 

 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif


endif  ;
; === end of regular plots for each var =========

;stop
;jump_tmp:
 
jump_3dlevel_plots:

;---------------------------------------------------------------------------------
; 3.2 loop through all 3d variables to make pressure level map figures 
;---------------------------------------------------------------------------------

if((sz[0] eq 3) and make_3dlevel_plots) then begin

    iave = 3

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

    varj =  var_cam+strdigit(std_levels[i],0)

    z_Range = [std_levels[i], std_levels[i]]

    print, iv, ' ', varj, z_range

 file_jpeg  = dir_jpeg + varj + '.jpeg'
 file_jpeg_color_out = dir_jpeg + varj + '.color'
 if(icolor eq 0)then begin
    file_jpeg_color_in  = expdir +SS +'/' + varj + '.color'
 endif else begin
    file_jpeg_color_in  = dir_jpeg_color_in + '/' + SS +'/'  + varj + '.color'
 endelse


 get2d, varj, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
      ,title= title, xtitle=xtitle, ytitle=ytitle ,$
      xrange=xrange, yrange=yrange,z_range=z_range  ; z_range is input, yrange is output!

 if(icolor and file_exist(file_jpeg_color_in)) then begin
    restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev_cam(aa, var0, scale, diff=diff, level=level)
    lev2 = [1.0e10,1.0e20]
 endelse
 if(min(lev1) eq max(lev1)) then goto, jump_next_3dvar
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6,1.0e7]

 view2d,vart,aa, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange $
          , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale
 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif

endfor ; levels

jump_next_3dvar:
endif ; 3d levle

;---------------------------------------------------------------------------------
; 3.3 make plots with tauu and tauv
;---------------------------------------------------------------------------------

 if((var_cam eq 'TAUY') and make_tau_plots)then begin

  iave = 3

 if(not taux_exist)then begin
  var0j = var_map('TAUX', map) 
  taux = get_dd2m(ncfile, var0j,  cntl_file=cntl_file, cntl_map = cntl_map)
  taux_exist = 1
 endif

 ;;if(not tauy_exist)then begin
 ;; var0j = var_map('TAUY', map)
 ;; tauy = get_dd2m(ncfile, var0j , cntl_file=cntl_file, cntl_map = cntl_map)
 ;;  tauy_exist = 1
 ;;endif

  tauy  = dd

 if(not psl_exist)then begin
  var0j = var_map('PSL', map)
  psl = get_dd2m(ncfile, var0j, cntl_file=cntl_file, cntl_map = cntl_map)*0.01
  taux_psl = 1
 endif

  varj =  'PSLUV'
  vart = varj

 file_jpeg  = dir_jpeg + varj + '.jpeg'
 file_jpeg_color_out = dir_jpeg + varj + '.color'
 if(icolor eq 0)then begin
    file_jpeg_color_in  = expdir +SS +'/' + varj + '.color'
 endif else begin
    file_jpeg_color_in  = dir_jpeg_color_in + '/' + SS +'/' + varj + '.color'
 endelse


 get2d, vart, psl, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
       ,title= title, xtitle=xtitle, ytitle=ytitle ,xrange=xrange, yrange=yrange
 get2d, vart, taux, dd2=tauy, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor and file_exist(file_jpeg_color_in)) then begin
    restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev_cam(aa, 'PSL', scale, diff=diff, level = level)
    lev2 = [1.0e10,1.0e20]
    scale = scale
    usteps = 10
    vsteps = 10
    vlen   = 5
    scale1 = scale
    scale2 = scale
 endelse

 if(min(lev1) eq max(lev1)) then goto, jump_uv_plots
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6,1.0e7]

 view2d,varj,bb, bb= bb,xx,yy,iave,lev1,lev1,xrange=xrange, yrange=yrange $
            , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ,$
             vect = 2, uvel = uvel, vvel = vvel, vlen=vlen, usteps=usteps,vsteps=vsteps

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale1,scale2,usteps,vsteps,vlen

    if(not setZ) then begin
     read,ix
     wwdelete
     endif 

endif ; tau plots

jump_uv_plots:

;---------------------------------------------------------------------------------
; 3.4 make special plots with u and v
;---------------------------------------------------------------------------------

  iave = 3
if(make_uv_plots and (var_cam eq 'Z3'))then begin

  if(not uu_exist)then begin
   var0j = var_map('U', map) 
   uu = get_dd2m(ncfile, var0j, cntl_file=cntl_file, cntl_map = cntl_map)
   uu_exist = 1
  endif

  if(not vv_exist)then begin
   var0j = var_map('V', map) 
   vv = get_dd2m(ncfile, var0j, cntl_file=cntl_file, cntl_map = cntl_map)
   vv_exist = 1
  endif

  ;if(not Z3_exist)then begin
  ; var0j = var_map('Z3', map) 
  ; z3 = get_dd2m(ncfile, var0j, cntl_file=cntl_file, cntl_map = cntl_map)
  ; z3_exist = 1
  ;endif
  Z3 = dd

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

  varj =  'ZUV'+strdigit(std_levels[i],0)
  vart = varj

  varjz =  'Z3'+strdigit(std_levels[i],0)

  z_Range = [std_levels[i], std_levels[i]]

    print, i, ' ', varj, z_range

 file_jpeg  = dir_jpeg + varj + '.jpeg'
 file_jpeg_color_out = dir_jpeg + varj + '.color'
 ;file_jpeg_color_in = dir_jpeg_color +SS + '_' + varj + '.color'
 if(icolor eq 0)then begin
    file_jpeg_color_in  = expdir +SS +'/'  + varj + '.color'
 endif else begin
    file_jpeg_color_in  = dir_jpeg_color_in + '/' + SS +'/' + varj + '.color'
 endelse


 jj = where(z3 le -900., cnt)
 if(cnt gt 0)then z3[jj] = -1.0e20

 get2d, vart, z3, dd2=z3, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
      ,title= title, xtitle=xtitle, ytitle=ytitle ,$
      xrange=xrange, yrange=yrange, z_Range = z_range
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor and file_exist(file_jpeg_color_in)) then begin
    restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev_cam(aa, var0, scale, diff=diff, level=level)
    lev2 = [1.0e10,1.0e20]
    scale1 = scale
    lev2 = get_lev_cam( bb, 'Z3', scale,  nn=60, level=level, diff=diff)
    scale2 = scale
    usteps =10 
    vsteps =10 
    vlen   = 3
 endelse

 if(min(lev1) eq max(lev1)) then goto, jump_next_var
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6,1.0e7]

 view2d,varj,bb, bb= bb,xx,yy,iave,lev2,lev2,xrange=xrange, yrange=yrange $
             , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ,$
             vect = 2, uvel = uvel, vvel = vvel, vlen=vlen, usteps=usteps,vsteps=vsteps

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale1,scale2,usteps,vsteps,vlen

    if(not setZ) then begin
     read,ix
     wwdelete
     endif 
 
endfor     ; number of levels

endif ; uv plots

 ;---------------------------------------

jump_next_var:

ENDFOR  ; var 

set_plot, 'X'

print, 'jpeg color reference folder is: ', dir_jpeg_color_in

print,' '
print,'   NC3FIG.PRO Program Finished Normally!'
print,'   For The Case Of ', ncfile_in
print,'   -------------------------------------------------------------------------------'
print,' '

return

END

