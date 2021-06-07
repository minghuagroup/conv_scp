

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


CNTL_MIPIDS = 'GCM'
CNTL_MODELIDS = 'cam5'
CNTL_EXPIDS = ['B20TR']

TEST_MIPIDS = 'GCM'
TEST_MODELIDS = 'cam5'
TEST_EXPIDS = ['BRCP85']


CNTL_MIPIDs = 'GCM'
CNTL_MODELIDs = 'esm'
CNTL_EXPIDs = ['B20TR']

TEST_MIPIDs = 'GCM'
TEST_MODELIDs = 'esm'
TEST_EXPIDs = ['SSP585']

Seasons    = ['DJF','MAM','JJA','SON','ANN']

; ========================================================================================================

; set parameters below
; ===================================================================================
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
 diff        = 1
 vars_only   = 0 ;['TS','TAUY','Z3'] ; or 0 for all
 ;;vars_only   = 'TSDIFF'
 nseasons    = [0] ;, 2]
 SSC             = 'DJF'
 icolor          = 1
 dir_jpeg_color_in  = '../cmip6/CFMIP/NCAR_CESM2/amip-p4K-amip/'
 dir_jpeg_color_in  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/'
 dir_jpeg_color_in  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585-historical/'
 dir_jpeg_color_in  = '../cmip6/GCM/esm/SSP585-B20TR/DJF/'


FOR IEXP = 0, N_ELEMENTS(TEST_EXPIDS)-1 DO BEGIN
; ==============================================

 icolor=0
FOR Is = 3, N_ELEMENTS(seasons)-1 DO BEGIN
 ;if(is eq 0)then icolor = 0 else icolor=1

 SS = seasons[is]
 
  TEST_MODELID = TEST_MODELIDS[IEXP]
  TEST_MIPID   = TEST_MIPIDS[IEXP]
  TEST_EXPID   = TEST_EXPIDS[IEXP]
  
  CNTL_MODELID = CNTL_MODELIDS[IEXP]
  CNTL_MIPID   = CNTL_MIPIDS[IEXP]
  CNTL_EXPID   = CNTL_EXPIDS[IEXP]

  

   casename = TEST_EXPID+'-'+CNTL_EXPID
   expdir      = '../cmip6/'+TEST_MIPID+'/'+TEST_MODELID+'/'+casename +'/' 
                 ; this is the place where jpeg files will be written

   TEST_expdir   = '../cmip6/'+TEST_MIPID+'/'+TEST_MODELID+'/'+TEST_EXPID 
   CNTL_expdir   = '../cmip6/'+CNTL_MIPID+'/'+CNTL_MODELID+'/'+CNTL_EXPID 


   test_file  = file_Search(TEST_ExpDIR + '/nc/*'+   seasons[is]+'*.nc')
   cntl_file  = file_search(CNTL_ExpDIR + '/nc/*'+   seasons[is]+'*.nc') 
   test_file = test_file[0]
   cntl_file = cntl_file[0]

   dir_jpeg   = expdir + '/'+SS+'/'


      NC2FIG, test_file, dir_jpeg = dir_jpeg, icolor=icolor, color_dir_jpeg = dir_jpeg_color_in, $
            SS = SS, setZ = setZ, map = map, adjust_lev = adjust_lev, diff = diff,      $
            make_basic_plots = make_basic_plots,  make_3dlevel_plots = make_3dlevel_plots,  $
            make_tau_plots   = make_tau_plots,    make_uv_plots      = make_uv_plots, $
            std_levels = std_levels, cntl_file = cntl_file, cntl_map = cntl_map, $
            vars_only = vars_only, expdir = expdir


; ===================================================================================

ENDFOR ; IS
ENDFOR ; IEXP
; ===================================================================================
print,' '
print,'   Program Finished Normally!'
print,' '

END

