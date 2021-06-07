
; THIS PROGRAM WORKS FOR A SINGLE FILE OR A SINGLE FOLDER, BUT ONE SLICE of time
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&7

ncfile = '/Users/minghuazhang/unix/cmip6/GCM/cam5/LENS/lens_ANN_climo.nc'
diff       = 0  ; not a difference plot
adjust_lev = 1  ; single-level contour level based on value
map        = 'cesm2cesm'  ; to assist with taux etc, cesm2era, cesm2cmip

; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&7

expdir    = strstrip(ncfile,'/',last=1) ; to put in jpeg folder underneath
expid     = get_lastv2_inlist(expdir,'/','.')
dir_jpeg  = expdir+'/jpeg_'+expid + '/'

; ===================================================================================
; set parameters below
; ===================================================================================

setZ               = 0  ; run in background batch mode
make_basic_plots   = 1
make_3dlevel_plots = 1
make_tau_plots     = 1
make_uv_plots      = 1
make_2d_plots      = 0

std_levels  = [500.]
std_levels  = [1000., 850., 500.,200]
;^^^^ 2 ^^^^^^^^^^^^^^^^^^^^^^^^^

icolor          = 1
SSC             = 'DJF'
dir_jpeg_color  = '../cmip6/CMIP/NCAR_CESM2/amip/'         +SSC + '/'   ; this is the input folder
dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/'+SSC + '/'   ; this is the input folder

restore, 'scale_cam.sav0'  ; cam_lev structure
scale_names = tag_names(scale_cam)
; ===================================================================================
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&77

    ; icolor = 0                      ; these two lines must be set together
    ; dir_jpeg_color  = dir_jpeg      ;  internally generated for DJF

    icolor = 0
;^^^^ 4 ^^^^^^^^^^^^^^^^^^^^^^^^^

; ===================================================================================

 make_folder, dir_jpeg

print, ' from  ncfile     = ', ncfile
print, ' to    dir_jpeg   = ', dir_jpeg

;---------------------------------------------------------------------------------
; 2. get coordinates, interpolated already by sav2nc.pro
;---------------------------------------------------------------------------------

 jvars = ncdf_vars2d(ncfile, all_ncfiles = ncfiles)
; ================================================== 
 nv = n_elements(jvars)
 if(n_elements(ncfiles) eq 1)then ncfiles = replicate(ncfiles,nv)

; 	THE ABOVE IS THE HEAD

;---------------------------------------------------------------------------------
; 3.1 basic plots, loop through all variables to make map and cross section plots
;---------------------------------------------------------------------------------

 FOR iv = 20, n_elements(jvars)-1 DO BEGIN 
; ====================================
 
 ncfile = ncfiles[iv]

 var0        = jvars[iv]
 var_cam     = var_map(var0, 'all2cesm')   ; for unit and title purpose
 vart        = var_cam + ' (' + var0 + ')'    

 dd_factor = 1.0
 if(belongsto(var0, scale_names))then dd_Factor = get_stru(scale_cam,var0)

 dd = get_dd2m(ncfile, var0, lon3 = xx2, lat3 = yy2, lev3 = zz2) 
 dd = dd * dd_factor

 skip = (n_elements(dd) le n_elements(xx2)) or (max(dd) eq min(dd))

 if(skip)then goto, jump_next_var

 sz = size(dd)

 print,'  ', iv, ' ',var0, ' ', var_cam,min(dd),max(dd)

 dd = less_100(dd, var_cam)
 dd = reform(dd)
;====================================================

if(make_basic_plots)then begin
   
 iave = 3
 if(sz[0] eq 3)then iave =1 

 file_jpeg  = dir_jpeg +  var0 + '.jpeg'
 file_jpeg_color_out = dir_jpeg + var0 + '.color'
 file_jpeg_color_in  = dir_jpeg_color +SSC + '_' + var0 + '.color'

 get2d, vart, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
       ,title= title, xtitle=xtitle, ytitle=ytitle ,xrange=xrange, yrange=yrange

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  
 endif else begin
    lev1 = get_lev_cam(aa, var0, scale, diff=diff) ;, level=adjust_lev)
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
 file_jpeg_color_in  = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg + varj + '.color'

 get2d, varj, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
      ,title= title, xtitle=xtitle, ytitle=ytitle ,$
      xrange=xrange, yrange=yrange,z_range=z_range  ; z_range is input, yrange is output!

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev_cam(aa, var0, scale, diff=diff, level=1)
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

 if((var_cam eq 'TAUX') and make_tau_plots)then begin

  iave = 3

  var0j = var_map('TAUX', map) 
  uu = get_dd2m(ncfile, var0j)

  var0j = var_map('TAUY', map) 
  vv = get_dd2m(ncfile, var0j)

  var0j = var_map('PSL', map) 
  psl = get_dd2m(ncfile, var0j)*0.01

  varj =  'PSLUV'
  vart = varj

 file_jpeg  = dir_jpeg + varj + '.jpeg'
 file_jpeg_color_in = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg + varj + '.color'

 get2d, vart, psl, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
       ,title= title, xtitle=xtitle, ytitle=ytitle ,xrange=xrange, yrange=yrange
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  ; lev1, lev2, 
 endif else begin
    lev1 = get_lev_cam(aa, 'PSL', scale, diff=diff, level = adjust_lev)
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

if(make_uv_plots and (var_cam eq 'U'))then begin

  iave = 3
  var0j = var_map('U', map) 
  uu = get_dd2m(ncfile, var0j)
  var0j = var_map('V', map) 
  vv = get_dd2m(ncfile, var0j)
  var0j = var_map('Z3', map) 
  z3 = get_dd2m(ncfile, var0j)

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

  varj =  'ZUV'+strdigit(std_levels[i],0)
  vart = varj

  varjz =  'Z3'+strdigit(std_levels[i],0)

  z_Range = [std_levels[i], std_levels[i]]

    print, i, ' ', varj, z_range

 file_jpeg  = dir_jpeg + varj + '.jpeg'
 file_jpeg_color_in = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg + varj + '.color'

 jj = where(z3 le -900., cnt)
 if(cnt gt 0)then z3[jj] = -1.0e20

 get2d, vart, z3, dd2=z3, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy $
      ,title= title, xtitle=xtitle, ytitle=ytitle ,$
      xrange=xrange, yrange=yrange, z_Range = z_range
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  ; lev1, lev2, 
 endif else begin
    lev1 = get_lev_cam(aa, var0, scale, diff=diff, level=1)
    lev2 = [1.0e10,1.0e20]
    scale1 = scale
    lev2 = get_lev_cam( bb, 'Z3', scale,  nn=60, level=1, diff=diff)
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

print, 'jpeg color reference folder is: ', dir_jpeg_color

print,' '
print,'   Program Finished Normally!'
print,' '
END

