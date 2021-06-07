; WORKS for Single Files!

; IDL may complain for too many openned files, just restart the season loop

;^^^^ 4 places to change to run the diagnostoc , this is for  diff figures,  nc2gif.pro is for a single simulation


; =====================================================================================
;;  PRO nc2fig_diff, dir_sav, dir_jpeg, setZ = setZ, month = month, season = season  , $
;;    plot_levels = plot_levels, std_levels=std_levels,  SSC = SSC, icolor=icolor, $
;;   plot_1d =plot_1d
; =====================================================================================
; =====================================================================================
; 
; purpose from one data sav folder to plot to the jpeg foler, inteval scales based on CAM mapping in cmip6_2cesm_Var.pro
; the restored data format can be customized
; SSC is for the color file, icolor to create color structure, SSC may reuse part
;
; it will create folders for month and seasons under dir_jpeg and create jpeg files
; input:

;;EXPIDS  = 'amip-4xCO2 amip-a4SST-4xCO2 amip-future4K amip-lwoff amip-m4K amip-p4K amip-p4K-lwoff amip-piForcing '
;;EXPIDS2 = 'aqua-4xCO2 aqua-control aqua-control-lwoff aqua-p4K aqua-p4K-lwoff'


; dir_sav   = '/Users/minghuazhang/unix/cmip6/CMIP/NCAR_CESM2/amip/sav/'  ; input
; dir_jpeg  = '~/unix/cmip6/test/jpeg/'  ; output

 
ModelID = 'NOAA-GFDL_GFDL-CM4'
MODELID ='NCAR_CESM2'

MIPID   = 'CFMIP'
EXPS1  = str_sep(strcompress('amip-4xCO2 amip-a4SST-4xCO2 amip-future4K amip-lwoff amip-m4K amip-p4K amip-p4K-lwoff amip-piForcing'),' ')
EXPS2 = str_sep(strcompress('aqua-4xCO2 aqua-control aqua-control-lwoff aqua-p4K aqua-p4K-lwoff'),' ')
EXPs = [EXPS1, EXPS2]

MIPID = 'ScenarioMIP'
EXPs = ['ssp585']

MIPID = 'CMIP'
EXPS = ['historical' , 'piControl', 'amip','abrupt-4xCO2' ]

; ========================================================================================================
TEST_MODELIDS = replicate('NCAR_CESM2',10)
CNTL_MODELIDS = replicate('NCAR_CESM2',10)

TEST_MIPIDS = ['ScenarioMIP', 'CMIP'        ,'CMIP'          ,'CMIP'       ]      
CNTL_MIPIDS = ['CMIP'       , 'CMIP'        ,'CMIP'          ,'CMIP'       ]     

TEST_EXPIDS = ['ssp585'     , 'abrupt-4xCO2','amip'          ,'historical' ]   
CNTL_EXPIDS = ['historical' , 'piControl'   ,'historical'    ,'piControl'  ]

TEST_MIPIDS = ['CFMIP'      ,'CFMIP'         ,'CMIP'         ,'CFMIP'         ,'CFMIP'        ]       
CNTL_MIPIDS = ['CMIP'       ,'CMIP'          ,'CFMIP'        ,'CFMIP'         ,'CFMIP'        ]

TEST_EXPIDS = ['amip-p4K' ,'amip-4xCO2'     ,'amip'          ,'aqua-4xCO2'    ,'aqua-p4K'     ]
CNTL_EXPIDS = ['amip'       ,'amip'         ,'amip-m4K'      ,'aqua-control'  ,'aqua-control' ]

; ========================================================================================================

;^^^^ 1  ^^^^^^^^^^^^^^^^^^^^^^^^^


FOR IEXP = 0, N_ELEMENTS(TEST_EXPIDS)-1 DO BEGIN
 
  TEST_MODELID = TEST_MODELIDS[IEXP]
  TEST_MIPID   = TEST_MIPIDS[IEXP]
  TEST_EXPID   = TEST_EXPIDS[IEXP]
  
  CNTL_MODELID = CNTL_MODELIDS[IEXP]
  CNTL_MIPID   = CNTL_MIPIDS[IEXP]
  CNTL_EXPID   = CNTL_EXPIDS[IEXP]

;^^^^ 1  ^^^^^^^^^^^^^^^^^^^^^^^^^


casename = TEST_EXPID+'-'+CNTL_EXPID
;casename = strupcase(casename)

; models can be looped here too

;---------------------------------------------------------------------------------
; 1. specify source folders, jpeg out folders, and write/read jpeg color inf folders, seasons, icolor, setZ
;---------------------------------------------------------------------------------

Seasons    = ['DJF','MAM','JJA','SON','ANN']
jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1
cmip6_vars = cmip6_vars()

; ===================================================================================

expdir      = '../cmip6/'+TEST_MIPID+'/'+TEST_MODELID+'/'+casename  ; this is the place where jpeg files will be written

;below are only to specify the ncd file
;------------------
TEST_expdir   = '../cmip6/'+TEST_MIPID+'/'+TEST_MODELID+'/'+TEST_EXPID
CNTL_expdir   = '../cmip6/'+CNTL_MIPID+'/'+CNTL_MODELID+'/'+CNTL_EXPID
TEST_dir_nc   = TEST_expdir + '/climo/'                               ; this is the ncd file folder
CNTL_dir_nc   = CNTL_expdir + '/climo/'

; ===================================================================================
; set parameters below
; ===================================================================================
setZ               = 1  ; run in background batch mode
make_basic_plots   = 1
make_3dlevel_plots = 0
make_uv_plots      = 0
make_2d_plots      = 0
std_levels  = [500.]
std_levels  = [1000., 850., 500.,200]
;^^^^ 2 ^^^^^^^^^^^^^^^^^^^^^^^^^

 SSC             = 'DJF'
 icolor          = 1
 dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585-historical/DJF/'
 dir_jpeg_color  = '../cmip6/CFMIP/NCAR_CESM2/amip-p4K-amip/DJF/'
;^^^^ 3 ^^^^^^^^^^^^^^^^^^^^^^^^^

; ===================================================================================
; ===================================================================================
FOR ISEASON = 0,4 DO BEGIN
; ================================
 SS = seasons[iseason]
 season = SS
 test_ncfile = test_dir_nc+'climo_'+SS+'.nc'
 cntl_ncfile = cntl_dir_nc+'climo_'+SS+'.nc'

 dir_jpeg  = expdir + '/'+season+'/'

 ; ^^^^^^^^^^^^^^^^^^^^^^^^^
   if(SS eq SSC)then begin
     if(iexp eq 0)then begin
       ;; &&& the following line works together with icolor=0 
       dir_jpeg_color  = expdir + '/'+ SSC +'/'   ; input folder, does not work for icolor=0 
       icolor = 0
     endif else begin
       icolor = 1
     endelse
   endif else begin
       icolor = 1
   endelse
;^^^^ 4 ^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

;goto,jump_tmp
; &&&&&& 

 make_folder, dir_jpeg

 print, ' from test_dir_nc = ', test_dir_nc,  ' ---> ', test_ncfile  
 print, '      cntl_dir_nc = ', CNTL_dir_nc,  ' ---> ', CNTL_ncfile  
 print, ' to   dir_jpeg   = ', dir_jpeg

;---------------------------------------------------------------------------------
; 2. get coordinates, interpolated already by sav2nc.pro
;---------------------------------------------------------------------------------

 ncfile  = test_ncfile
 ncfile0 = cntl_ncfile

 doit = file_Exist(ncfile) and file_exist(ncfile0)
 if(not doit)then begin
  print,'ncdf files do not exist'
  stop
 endif


 ncdf_vars,ncfile , jvars
 ncdf_vars,ncfile0, jvars0
 
 xx2 = get_fld(ncfile, 'lon')
 yy2 = get_fld(ncfile, 'lat')
 zz2 = get_fld(ncfile, 'lev')

 ; test resolution
 
 xxc = get_fld(ncfile0, 'lon')
 yyc = get_fld(ncfile0, 'lat')
 zzc = get_fld(ncfile0, 'lev')

 same = (array_equal(xx2,xxc) and array_equal(yy2,yyc) and array_equal(zz2,zzc))
 if(not same)then begin
  print,' The dimensions of the data arrays in the two ncdf files are different!'
  help,xx2,yy2,zz2
  help,xxc,yyc,zzc
  print,'prepare the data first!'
  stop
 endif

; 	THE ABOVE IS THE HEAD

 if(not make_basic_plots)then goto, jump_3dlevel_plots

;---------------------------------------------------------------------------------
; 3.1 loop through all variables to make map and cross section plots
;---------------------------------------------------------------------------------

 FOR iv = 0, n_elements(jvars)-1 DO BEGIN 
; FOR iv = 47,48 DO BEGIN 
; ====================================

 var0        = jvars[iv]
 var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose
 vart        = var_cam + ' (' + var0 + ')'    

;; if(not belongsto(var_Cam,  ['QREFHT','PRECT','PS','PSL']))then goto, jump_next_var
;; if(not belongsto(var_Cam,  ['TAUY','sbl']))then goto, jump_next_var
;; if(not belongsto(var_Cam,  ['SST']))then goto, jump_next_var
;; if(not belongsto(var_Cam,  ['CLDLIQ']))then goto, jump_next_var
; &&&&&&&&&&&&&&&&&&&&&

 print,'basic plots: ', iv, ' ',var0, ' ', var_cam

 if(ncdf_dim(ncfile, var0) le 1) then goto, jump_next_var
;;if(strpos(modelid, 'CESM2') ge 0)then $
 ;;if(var_duplicate(var0))         then goto, jump_next_var  ; only do the filled cases
if(not belongsto (var0, jvars0))then goto, jump_next_var  ; not in the cntl file

 ;-------- SPECIAL DATA TREATMENT, cld positive, precipitation positive etc

 dd  = get_fld(ncfile, var0)
 dd0 = get_fld(ncfile0, var0)

 dd  = less_100(dd,  cmip6_2cesm_var(var0,c=1))
 dd0 = less_100(dd0, cmip6_2cesm_var(var0,c=1))

 dd  = dd - dd0

 sz = size(dd)

;stop

;----------------------------------
   
 iave = 3
 if(sz[0] eq 3)then iave =1 

 file_jpeg  = dir_jpeg +SS + '_' + var0 + '.jpeg'
 file_jpeg_color_out = dir_jpeg +SS + '_' + var0 + '.color'
 file_jpeg_color_in  = dir_jpeg_color +SSC + '_' + var0 + '.color'

 
 get2d, vart, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, xtitle=xtitle, ytitle=ytitle ,xrange=xrange, yrange=yrange

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  
 endif else begin
    lev1 = get_lev(name='cmip6', get_diff=1,  aa , var_cam, scale, adjust=1, cnn_nn = lev2_nn, slev2=lev2, nn2 =0, missing = 0 ) 
 endelse
 if(min(lev1) eq max(lev1)) then goto, jump_next_var
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6, 1.0e7]

 aa = aa*scale
 bb = bb*scale

;print,lev1

 view2d,vart,aa, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=0, setZ=setZ

;print,lev1

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale 
 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif

jump_next_var:

ENDFOR ; iv
; === end of regular plots for each var =========

;stop
;jump_tmp:
 
jump_3dlevel_plots:

if(not make_3dlevel_plots) then goto, jump_uv_plots

;---------------------------------------------------------------------------------
; 3.2 loop through all 3d variables to make pressure level map figures 
;---------------------------------------------------------------------------------

;jump_tmp:

FOR iv = 0, n_elements(jvars)-1 DO BEGIN 
; ====================================
; FOR iv = 47, 48 DO BEGIN 

 var0        = jvars[iv]
 var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose
 vart        = var_cam + ' (' + var0 + ')'    
 ;;if(var_duplicate(var0)) then goto, jump_next_3dvar  ; only do the filled cases

 dsize = ncdf_dim ( ncfile, var0)

 if(dsize ne 4)then goto, jump_next_3dvar

 dd = get_fld(ncfile, var0)
 dd0 = get_fld(ncfile0, var0)

 dd  = less_100(dd,  cmip6_2cesm_var(var0,c=1))
 dd0 = less_100(dd0, cmip6_2cesm_var(var0,c=1))
 
 dd = dd - dd0

 print, ' pressure level: ', iv, ' ',var0, ' ', var_cam

;jump_tmp:

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

    iave = 3
    varj =  var_cam+strdigit(std_levels[i],0)

    z_Range = [std_levels[i], std_levels[i]]

    print, iv, ' ', varj, z_range

 file_jpeg  = dir_jpeg +SS + '_' + varj + '.jpeg'
 file_jpeg_color_in  = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg +SS + '_' + varj + '.color'

 get2d, varj, dd, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, xtitle=xtitle, ytitle=ytitle , $
    xrange=xrange, yrange=yrange, z_range=z_range  ; z_range is input, yrange is output!

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in
 endif else begin
    lev1 = get_lev(name='cmip6', get_diff=1,  aa , var_cam, scale, adjust=1, cnn_nn = lev2_nn, slev2=lev2, nn2 =0)
 endelse
 if(min(lev1) eq max(lev1)) then goto, jump_next_3dvar
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6, 1.0e7]

 aa = aa*scale
 bb = bb*scale

 view2d,vart,aa, bb= bb,xx,yy,iave,lev1,lev2,xrange=xrange, yrange=yrange , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ

 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale
 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif

endfor ; levels

jump_next_3dvar:
ENDFOR  ; iv variables

jump_tmp:
jump_plot_uv:

;---------------------------------------------------------------------------------
; 3.3 make plots with tauu and tauv
;---------------------------------------------------------------------------------

  iave = 3
  var0 = cmip6_2cesm_var('TAUX')

 if(not belongsto (var0, jvars))then goto, jump_uv_plots

  uu = get_fld(ncfile, var0) - get_fld(ncfile0, var0)

  var0 = cmip6_2cesm_var('TAUY') 
  vv = get_fld(ncfile, var0) - get_fld(ncfile0, var0)

  var0 = cmip6_2cesm_var('PSL') 
  psl = get_fld(ncfile, var0)  - get_fld(ncfile0, var0)

  varj =  'PSLUV'
  vart = varj

 file_jpeg  = dir_jpeg +SS + '_' + varj + '.jpeg'
 file_jpeg_color_in = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg +SS + '_' + varj + '.color'

 get2d, vart, psl, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, xtitle=xtitle, ytitle=ytitle , $
   xrange=xrange, yrange=yrange
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor)then begin
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  ; lev1, lev2,
 endif else begin
    lev1 = get_lev(name='cmip6', get_diff=1,  aa, 'PSL', scale, adjust=1, nn=30)
    lev2 = lev1
    scale = scale
    usteps = 10
    vsteps = 10
    vlen   =20
    scale1 = scale
    scale2 = scale
 endelse


 if(min(lev1) eq max(lev1)) then goto, jump_next_var
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6, 1.0e7]

 aa = aa*scale1
 bb = bb*scale2

 view2d,varj,bb, bb= bb,xx,yy,iave,lev2,lev2,xrange=xrange, yrange=yrange , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ,$
             vect = 2, uvel = uvel, vvel = vvel, vlen=vlen, usteps=usteps,vsteps=vsteps



 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale1,scale2,usteps,vsteps,vlen

    if(not setZ) then begin
     read,ix
     wwdelete
     endif

jump_next_uvpsl:


jump_uv_plots:

if((not make_uv_plots))then goto, jump_2d_plots

;stop

;---------------------------------------------------------------------------------
; 3.4 make special plots with u and v
;---------------------------------------------------------------------------------

 if(not make_basic_plots)then goto, JUMP_2D_plots

  iave = 3
  uu = get_fld(ncfile, 'U')
  vv = get_fld(ncfile, 'V')
  tt = get_fld(ncfile, 'T')
  z3 = get_fld(ncfile, 'Z3')

  uu0 = get_fld(ncfile0, 'U')
  vv0 = get_fld(ncfile0, 'V')
  tt0 = get_fld(ncfile0, 'T')
  z30 = get_fld(ncfile0, 'Z3')

  uu = uu - uu0
  vv = vv - vv0
  tt = tt - tt0
  z3 = z3 - z30

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

  varj =  'ZUV'+strdigit(std_levels[i],0)
  vart = varj
  varjz =  'Z3'+strdigit(std_levels[i],0)
  z_Range = [std_levels[i], std_levels[i]]

    print, i, ' ', varj, z_range

 file_jpeg  = dir_jpeg +SS + '_' + varj + '.jpeg'
 file_jpeg_color_in = dir_jpeg_color +SSC + '_' + varj + '.color'
 file_jpeg_color_out = dir_jpeg +SS + '_' + varj + '.color'

 jj = where(z3 le -900., cnt)
 if(cnt gt 0)then z3[jj] = -1.0e20

 get2d, vart, tt, dd2=z3, xx2, yy2, zz2, iave, aa=aa,bb=bb,xx=xx,yy=yy,title= title, xtitle=xtitle, ytitle=ytitle ,$
        xrange=xrange, yrange=yrange,z_range=z_range  ; z_range is input, yrange is output!
 get2d, vart, uu, dd2=vv, xx2, yy2, zz2, iave, aa=uvel,bb=vvel,xx=xx,yy=yy 

 if(icolor)then begin
    lev_ajust = 0
    if(file_exist(file_jpeg_color_in)) then restore, file_jpeg_color_in  ; lev1, lev2, 
 endif else begin
    lev_adjust = 1
    lev1 = get_lev(name='cmip6', get_diff=1,  bb, 'Z3', scale, adjust=1, nn=60) 
    scale1 = scale
    lev2 = get_lev(name='cmip6', get_diff=1,  bb, 'Z3', scale, adjust=1, cnn_nn = 1, missing = 0, nn=60)
    scale2 = scale
    usteps =10 
    vsteps =10 
    vlen   = 3
 endelse
 lev1 = lev2
 scale1 = scale2


 if(min(lev1) eq max(lev1)) then goto, jump_next_uvlev
 if(min(lev2) eq max(lev2)) then lev2 = [1.0e6, 1.0e7]

 aa = aa*scale1
 bb = bb*scale2

 view2d,varj,bb, bb= bb,xx,yy,iave,lev2,lev2,xrange=xrange, yrange=yrange , title=title ,xtitle=xtitle, ytitle=ytitle, colorbar_nn=10, setZ=setZ,$
             vect = 2, uvel = uvel, vvel = vvel, vlen=vlen, usteps=usteps,vsteps=vsteps



 saveimage, file_jpeg, jpeg = 1, quality=100
 if(not icolor) then save,      file=file_jpeg_color_out, lev1, lev2, scale1,scale2,usteps,vsteps,vlen

    if(not setZ) then begin
     read,ix
     wwdelete
     endif 

jump_next_uvlev:

endfor     ; number of levels

 ;---------------------------------------

 jump_2d_plots:
;---------------------------------------------------------------------------------
; 3.4 make 2D plots
;---------------------------------------------------------------------------------


 FOR iv = 0, -n_elements(jvars)-1 DO BEGIN 
; ====================================

 var0        = jvars[iv]
 var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose
 vart        = var_cam + ' (' + var0 + ')'    

 print, '2d  ', iv, ' ',var0, ' ', var_cam
;;if(var_duplicate(var0)) then goto, jump_next_2dvar  ; only do the filled cases

 if(ncdf_dim(ncfile, var0) le 1)then goto, jump_next_2dvar

; dd = get_fld(ncfile, var0)
; dd0 = get_fld(ncfile0, var0)
; dd = dd - dd0
 sz = size(dd)

 case sz[0] of

 3: begin 
    dd   = ave4(dd[*,*,jjm])
    ; do 2D line plots
    end
 4: begin
    dd   = ave4(dd[*,*,*,jjm])
    ; do level line plots 
    end
 else:
 endcase

jump_next_2dvar:



ENDFOR  ; var 2d


ENDFOR ; iseason

set_plot, 'X'

print, 'jpeg color reference folder is: ', dir_jpeg_color

ENDFOR ; IEXP
print,' '
print,'   Program Finished Normally!'
print,' '

END

