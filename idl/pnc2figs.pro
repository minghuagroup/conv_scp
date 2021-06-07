
; process 4 seasonal and 1 annual file


ModelID = 'NOAA-GFDL_GFDL-CM4'
MODELID ='NCAR_CESM2'

;^^^^ 1  ^^^^^^^^^^^^^^^^^^^^^^^^^
MIPID = 'CMIP'
EXPS = ['historical' , 'piControl', 'amip','abrupt-4xCO2' ]


MIPID   = 'CFMIP'
EXPS1  = str_sep(strcompress('amip-4xCO2 amip-a4SST-4xCO2 amip-future4K amip-lwoff amip-m4K amip-p4K amip-p4K-lwoff amip-piForcing'),' ')
EXPS2 = str_sep(strcompress('aqua-4xCO2 aqua-control aqua-control-lwoff aqua-p4K aqua-p4K-lwoff'),' ')
EXPs = [EXPS1, EXPS2]
 
MIPID = 'ScenarioMIP'
EXPs = ['ssp585']

MIPID = 'GCM'
MODELID = 'cam5'
EXPs = ['BRCP85', 'B20TR']

MIPID = 'GCM'
MODELID = 'esm'
EXPs = ['SSP585', 'B20TR']
Seasons    = ['DJF','MAM','JJA','SON','ANN']

; choice of plots
; ================================================================
 SSC  = 'DJF'
 icolor    = 1
 dir_jpeg_color_in  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585-historical/'
 dir_jpeg_color_in  = '../cmip6/CFMIP/NCAR_CESM2/amip-p4K-amip/'
 dir_jpeg_color_in  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/'
 dir_jpeg_color_in  = '../cmip6/GCM/cam5/BRCP85/'
 dir_jpeg_color_in  = '../cmip6/GCM/esm/SSP585/'
 SSC = 'DJF'
 setZ               = 1  ; run in background batch mode
 make_basic_plots   = 1
 make_3dlevel_plots = 1
 make_tau_plots     = 1
 make_uv_plots      = 1
 make_2d_plots      = 1
 std_levels  = [500.]
 std_levels  = [1000., 850., 500.,200]
 map         = 'cmip2cesm'
 cntl_map    = 'cesm2cesm'
 diff        = 0
 vars_only   = 0  ;['Z3'] ;,'TAUY']
; vars_only   = 'TSDIFF'
 nseasons    = 0  ;[0, 2]
 do_add_field = 0
; ================================================================

;;setZ = 0

FOR IEXP = 1, N_ELEMENTS(EXPS)-1 DO BEGIN
; ================================================================
 if(iexp eq 0)then icolor=0 else icolor =1
FOR Is = 0, N_ELEMENTS(Seasons)-1 DO BEGIN

  EXPID = EXPS[IEXP]
  SS    = Seasons[is]


 expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID
 
 dir_nc   = expdir + '/nc/' ;or file

 file = file_search(dir_nc + '*'+seasons[is]+'*.nc')
 file = file[0]

 ;if(do_add_field or (not file_exist(dir_nc + '/H.nc')) )then $
 ;add_field, dir_nc

 ;goto, jump_next_exp

 ncfile_in = file
 dir_jpeg   = expdir + '/' + SS + '/'

     NC2FIG, ncfile_in, dir_jpeg = dir_jpeg, icolor=icolor, color_dir_jpeg = dir_jpeg_color_in, $
            SS = SS, setZ = setZ, map = map, adjust_lev = adjust_lev, diff = diff,      $
            make_basic_plots = make_basic_plots,  make_3dlevel_plots = make_3dlevel_plots,  $
            make_tau_plots   = make_tau_plots,    make_uv_plots      = make_uv_plots, $
            std_levels = std_levels, cntl_file = 0, cntl_map = 0, $
            vars_only = vars_only , expdir = expdir


jump_next_exp:
ENDFOR ; Season
ENDFOR ; EXP

print,' '
print,'   Program Finished Normally!'
print,' '
END

