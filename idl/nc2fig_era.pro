
; Plot a folder
;  12 months of data in nc folder
;^^^^ 4 places to change to run the diagnostoc , this is for one simulation, nc2gifs.pro is for two different cases  ^^^^^^^^^^^^^^^^^^^^^^^^^

; dir_sav   = '/Users/minghuazhang/unix/cmip6/CMIP/NCAR_CESM2/amip/sav/'  ; input
; dir_jpeg  = '~/unix/cmip6/test/jpeg/'  ; output

ModelID = 'NOAA-GFDL_GFDL-CM4'
MIPID = 'CMIP'
EXPS = ['historical' , 'piControl', 'amip','abrupt-4xCO2' ]

;^^^^ 1  ^^^^^^^^^^^^^^^^^^^^^^^^^

MIPID = 'ERA5'
MODELID = 'climo'
EXPS = ['1999_2008']
EXPS = ['1979_1988','2009_2018']

SSC             = 'DJF'   ; to specify the reference color variable
icolor          = 1
dir_jpeg_color  = '../cmip6/CMIP/NCAR_CESM2/amip/'         +SSC + '/'   ; this is the input folder
dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/'+SSC + '/'   ; this is the input folder
dir_jpeg_color  = '../cmip6/ERA5/climo/1979_1988/DJF/'


FOR IEXP = 0, N_ELEMENTS(EXPS)-1 DO BEGIN
   
  EXPID = EXPS[IEXP]

;---------------------------------------------------------------------------------
; 1. specify source folders, jpeg out folders, and write/read jpeg color inf folders, seasons, icolor, setZ
;---------------------------------------------------------------------------------

Seasons    = ['DJF','MAM','JJA','SON','ANN']
jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1
cmip6_vars = cmip6_vars()
era5_vars  = era5_vars()
mapping    = 'era2cesm'
restore, 'scale_era5.sav0'
scale_names = tag_names(scale_era5) 

; =======================================

; type of preparations

; ===================================================================================
expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID + '/'  ; where seasonal jpeg files go
dir_nc   = expdir + 'nc/'                                 ; nc source file or files

; ===================================================================================
; set parameters below
; ===================================================================================

setZ               = 1  ; run in background batch mode
make_basic_plots   = 1
make_3dlevel_plots = 1
make_uv_plots      = 1
make_2d_plots      = 0

std_levels  = [500.]
std_levels  = [1000., 850., 500.,200]
;^^^^ 2 ^^^^^^^^^^^^^^^^^^^^^^^^^

; ===================================================================================
FOR ISEASON = 0,4 DO BEGIN
;^^^^ 3 ^^^^^^^^^^^^^^^^^^^^^^^^^
; ================================
 SS = seasons[iseason]
 season = SS
 
 dir_jpeg  = expdir + season + '/'
 ; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&77
 if(SS eq SSC)then begin
   if(iEXP eq 0)then begin
     icolor = 0
     dir_jpeg_color  = dir_jpeg      ;  internally generated for DJF
     ;icolor = 1
   endif else begin 
     icolor = 1
   endelse
 endif else begin
   icolor = 1
 endelse
 ;^^^^ 4 ^^^^^^^^^^^^^^^^^^^^^^^^^
 
 make_folder, dir_jpeg
 

 print, ' from  dir_nc     = ', dir_nc 
 print, ' to    dir_jpeg   = ', dir_jpeg


 
files_in = file_search(dir_nc+'/*.nc', count = n_file)
dd_factor = 1.0
if(n_file eq 1)then begin
  ;----------------------
  files_tmp = replicate(files_in, n_file)
  ncdf_vars2d, files_in, jvars, no_print =1 ; only variabiles with dimension >= 2
  longnames = jvars
endif else begin
    ;----------------------
  files_tmp = files_in
  jvars = get_lastv2_inlist(files_tmp,'/','.')
  jvars = strdel(jvars,'ML_')  ; delete ERA multi-level indicator when files were saved
  longnames = jvars

; to get longnames matched with jvars
  if(strpos(files_in[0],'ERA5'))then begin
     jnames = strupcase(era5_vars.vars_era5_all)
     jlongnames = era5_vars.vars_era5_str_all
     vars_tmpac = strupcase(era5_vars.vars_ac)
     vars_tmpv = strupcase(era5_vars.vars_v)

     nv = n_elements(jvars)
     for i = 0, n_elements(jvars)-1 do begin
       var_cam = var_mapping(jvars[i],mapping)
       jj = where(strupcase(jvars[i]) eq jnames,cnt) 
       if(cnt eq 0)then jlongnames[i] ='' else $
         longnames[i] = jlongnames[jj[0]] 
      endfor
   endif  ; ERA5
endelse   ; single file or multi file
;----------------------

;; =====================================================================
FOR iv = 0, n_file - 1 do begin
 ; =====================================================================

 ncfile = files_tmp(iv)
 var0        = jvars[iv] 
 var_cam     = var_mapping(var0, mapping)   ; for unit and title purpose
 vart        = var_cam + ' (' + var0 + ')'

  if(var0 ne 'TSDIFF')then goto, jump_next_2dvar

  dd_factor = 1.0
  if(belongsto(jvars[iv], vars_tmpac)) then dd_factor = 1./86400. ; accumulation
  if(belongsto(jvars[iv], scale_names)) then dd_factor = dd_factor * get_stru(scale_era5,jvars[iv])

  print, iv, ' ', var0, ' ', var_cam, ' ', longnames[iv]
  
 savtype = 0
 dd = get_dd3m(ncfile, var0, savtype,lon3 = xx2, lat3=yy2, lev3 = zz2, level = 'level',mapping = mapping) * dd_factor
 
  print,var0, dd_factor, min(dd),max(dd)
 dd = less_100(dd, var_cam)
                                                                                 ; ========================
 ;===============================================================
 sz = size(dd) 
; 
 if(iseason eq 4)then jjj = indgen(12) else $
 jjj = reform(jj_seasons[*,iseason])
 case sz[0] of
   3: dd = ave3(dd[*,*,jjj], missing = 1.0e12)
   4: dd = ave4(dd[*,*,*,jjj], missing = 1.0e12) 
   else:
 endcase
 sz = size(dd) 
;----------------------------------

;;---------------------------------------------------------------------------------
;; 3.1 basic plots, loop through all variables to make map and cross section plots
;;---------------------------------------------------------------------------------
; 
 if(make_basic_plots)then begin

file_jpeg  = dir_jpeg +SS + '_' + var0 + '.jpeg'
file_jpeg_color_out = dir_jpeg +SS + '_' + var0 + '.color'
file_jpeg_color_in  = dir_jpeg_color +SSC + '_' + var0 + '.color'
   
 iave = 3
 if(sz[0] eq 3)then iave =1 

  get2d, vart, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, xtitle=xtitle, ytitle=ytitle $
            ,xrange=xrange, yrange=yrange

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  
 endif else begin
    lev1 = get_lev_era5(aa, var0, scale)
    lev2 = [1.0e10,1.0e20] 
 endelse

 if(min(lev1) eq max(lev1))then goto, jump_3dlevel_plots 
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e16, 1.0e20]
 
 title = longnames[iv] +' '+ title
 
 view2d,vart,aa, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange , title=title $
          ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=0, setZ=setZ

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale 

 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif
 
endif ; make basic plots
JUMP_3DLEVEL_PLOTS:


;---------------------------------------------------------------------------------
; 3.2 loop through all 3d variables to make pressure level map figures 
;---------------------------------------------------------------------------------
if(make_3dlevel_plots) then begin

;jump_tmp:

 if(sz[0] ne 3)then goto, jump_next_3dvar
 
 print, ' pressure level: ', iv, ' ',var0, ' ', var_cam
 
 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

    iave = 3
    varj =  var_cam+strdigit(std_levels[i],0)

    z_Range = [std_levels[i], std_levels[i]]

    print, iv, ' ', varj, z_range

 file_jpeg  = dir_jpeg +SS + '_' + varj + '.jpeg'
 file_jpeg_color_in  = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg +SS + '_' + varj + '.color'

 get2d, varj, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, xtitle=xtitle, ytitle=ytitle ,$
      xrange=xrange, yrange=yrange,z_range=z_range  ; z_range is input, yrange is output!

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev_era5(aa, var0, scale, level=1)
    lev2 = [1.0e10,1.0e20] 
 endelse

 if(min(lev1) eq max(lev1)) then goto, jump_next_3dvar
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6,1.0e7]

 title = longnames[iv] + ' '+title
 view2d,vart,aa, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange , title=title $
           ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale
 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif

endfor ; levels

jump_next_3dvar: 


;---------------------------------------------------------------------------------
; 3.3 make plots with tauu and tauv
;---------------------------------------------------------------------------------

if(var_cam eq 'TAUX')then begin
  iave = 3
  var0u = var_mapping('TAUX', 'cesm2era')
  var0v = var_mapping('TAUY', 'cesm2era')
  var0ps = var_mapping('PSL', 'cesm2era')

  if(n_file eq 1)then begin
    uu = get_dd3m(ncfile, var0u, savtype) 
    vv = get_dd3m(ncfile, var0v, savtype) 
    psl = get_dd3m(ncfile, var0ps, savtype)
 endif else begin
    var_tmp = var_mapping('TAUX', 'cesm2era')
    jj = where(var_tmp eq jvars)
    uu = get_dd3m(files_tmp[jj[0]], var_tmp , savtype)

    var_tmp = var_mapping('TAUY', 'cesm2era')
    jj = where(var_tmp eq jvars)
    vv = get_dd3m(files_tmp[jj[0]], var_tmp , savtype)

    var_tmp = var_mapping('PSL', 'cesm2era')
    jj = where(var_tmp eq jvars)
    psl = get_dd3m(files_tmp[jj[0]], var_tmp , savtype) * get_stru(scale_era5, var_tmp)
    var_psl = var_tmp

 if(max(uu) gt 500.)then uu= uu/86400.    ; instantaneous vs accumulated
 if(max(vv) gt 500.)then vv= vv/86400.

  endelse
  psl = psl 
  psl = limit(psl, 1.0e6)
  uu = limit(uu, 1.0e4)
  vv = limit(vv, 1.0e4)
 
  varj =  'PSLUV'
  vart = varj

 file_jpeg  = dir_jpeg +SS + '_' + varj + '.jpeg'
 file_jpeg_color_in = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg +SS + '_' + varj + '.color'

 get2d, vart, psl, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title $
               , xtitle=xtitle, ytitle=ytitle ,xrange=xrange, yrange=yrange
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  ; lev1, lev2, 
 endif else begin
    lev1 = get_lev_era5(bb,var_psl , scale)
    lev2 = [1.0e10,1.0e20] 
    usteps = 10
    vsteps = 10
    vlen   =  5
    scale1 = scale
    scale2 = scale
 endelse 

 if(min(lev1) eq max(lev1)) then goto, jump_uv_plots
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6,1.0e7]

 view2d,varj,bb, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange , title=title $
             ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ,$
             vect = 2, uvel = uvel, vvel = vvel, vlen=vlen, usteps=usteps,vsteps=vsteps

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale1,scale2,usteps,vsteps,vlen

    if(not setZ) then begin
     read,ix
     wwdelete
     endif 

ENDIF ; taux, tauy
jump_uv_plots:

if((not make_uv_plots))then goto, jump_2d_plots

;---------------------------------------------------------------------------------
; 3.4 make special plots with u and v
;---------------------------------------------------------------------------------
IF(var_cam eq 'U')then begin
  iave = 3
 
  if(n_file eq 1)then begin 
    uu = get_dd3m(ncfile, var_mapping('V', 'cesm2era') , savtype, level='level')
    vv = get_dd3m(ncfile, var_mapping('V', 'cesm2era') , savtype, level='level')
    tt = get_dd3m(ncfile, var_mapping('T', 'cesm2era') , savtype, level='level')
    z3 = get_dd3m(ncfile, var_mapping('Z3','cesm2era'), savtype, level='level')
  endif else begin
    var_tmp = var_mapping('U', 'cesm2era')
    jj = where(var_tmp eq jvars)
    uu = get_dd3m(files_tmp[jj[0]], var_tmp , savtype)
    
    var_tmp = var_mapping('V', 'cesm2era')
    jj = where(var_tmp eq jvars)
    vv = get_dd3m(files_tmp[jj[0]], var_tmp , savtype)
    
    var_tmp = var_mapping('T', 'cesm2era')
    jj = where(var_tmp eq jvars)
    tt = get_dd3m(files_tmp[jj[0]], var_tmp , savtype)
    
    var_tmp = var_mapping('Z3', 'cesm2era')
    jj = where(var_tmp eq jvars)
    z3 = get_dd3m(files_tmp[jj[0]], var_tmp , savtype) 
  endelse

  tt = limit(tt, 1.0e4)
  z3 = limit(z3, 1.0e6)
  uu = limit(uu, 1.0e4)
  vv = limit(vv, 1.0e4)

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

  varj =  'ZUV'+strdigit(std_levels[i],0)
  vart = varj

  varjz =  'Z3'+strdigit(std_levels[i],0)
  var_Z3 = var_mapping('Z3', 'cesm2era')

  z_Range = [std_levels[i], std_levels[i]]

    print, i, ' ', varj, z_range

 file_jpeg  = dir_jpeg +SS + '_' + varj + '.jpeg'
 file_jpeg_color_in = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg +SS + '_' + varj + '.color'

 jj = where(z3 le -900., cnt)
 if(cnt gt 0)then z3[jj] = -1.0e20

 get2d, vart, tt, dd2=z3, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, $
      xtitle=xtitle, ytitle=ytitle ,$
      xrange=xrange, yrange=yrange, z_Range = z_range
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

  
 bb = bb*0.01   ; for Z3   !!!!

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  ; lev1, lev2, 
 endif else begin
    lev1 = get_lev_era5(bb,var_z3, scale, level=1) 
    scale1 = 0.01
    scale2 = 0.01
    usteps =10 
    vsteps =10 
    vlen   = 3
 endelse

 if(min(lev1) eq max(lev1)) then goto, jump_2d_plots
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6,1.0e7]


 view2d,varj,bb, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange , title=title $
             ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ,$
             vect = 2, uvel = uvel, vvel = vvel, vlen=vlen, usteps=usteps,vsteps=vsteps

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale1,scale2,usteps,vsteps,vlen

    if(not setZ) then begin
     read,ix
     wwdelete
     endif 
 
endfor     ; number of levels

ENDIF ; uv plots

endif ; make_3dlevel_plots
 ;---------------------------------------

 jump_2d_plots:
;---------------------------------------------------------------------------------
; 3.5 make 2D plots
;---------------------------------------------------------------------------------

;
; FOR iv = 0, -n_elements(jvars)-1 DO BEGIN 
;; ====================================
;
; var0        = jvars[iv]
; var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose
; vart        = var_cam + ' (' + var0 + ')'    
;
; print, '2d  ', iv, ' ',var0, ' ', var_cam
;;;if(var_duplicate(var0)) then goto, jump_next_2dvar  ; only do the filled cases
;
; if(ncdf_dim(ncfile, var0) le 1)then goto, jump_next_2dvar
;; dd = get_fld(ncfile, var0)
;
; sz = size(dd)
;
; case sz[0] of
;
; 3: begin 
;    dd   = ave4(dd[*,*,jjm])
;    ; do 2D line plots
;    end
; 4: begin
;    dd   = ave4(dd[*,*,*,jjm])
;    ; do level line plots 
;    end
; else:
; endcase

jump_next_2dvar:



ENDFOR  ; var, and files_in


ENDFOR ; iseason

jump_end:

set_plot, 'X'

print, 'jpeg color reference folder is: ', dir_jpeg_color

ENDFOR ; IEXP
print,' '
print,'   Program Finished Normally!'
print,' '
END

