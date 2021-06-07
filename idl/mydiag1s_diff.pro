
; after run nc2fig.pro ; note color folder eg. dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/ANN/' and icolor


FOUR_SEASONS = 1   ;default DJF, JJA and ANN
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Seasons = ['DJF','MAM','JJA','SON','ANN']

;=========================================================
;=========================================================
; Modify below
;=========================================================
testMIP = 'ScenarioMIP' ;'CFMIP'  ;'CMIP';        'ScenarioMIP' ;'CMIP'
testID  = 'ssp585' ;aqua-p4K' ;amip-p4K'    ;historical';  'ssp585'      ;'amip'

cntlMIP = 'CMIP' ;CMIP'
cntlID  = 'historical' ;aqua-control'   ;piControl';   'historical'


; ========================================================================================================

CNTL_MODELIDS = replicate('NCAR_CESM2',10)
order     = 'yes' ;0, 1, era5

TEST_MIPIDS = ['ScenarioMIP', 'CMIP'        ,'CMIP'          ,'CMIP'       ]
CNTL_MIPIDS = ['CMIP'       , 'CMIP'        ,'CMIP'          ,'CMIP'       ]

TEST_EXPIDS = ['ssp585'     , 'abrupt-4xCO2','amip'          ,'historical' ]
CNTL_EXPIDS = ['historical' , 'piControl'   ,'historical'    ,'piControl'  ]
TEST_MODELIDS = replicate('NCAR_CESM2',10)

TEST_MIPIDS = ['CFMIP'      ,'CFMIP'         ,'CMIP'         ,'CFMIP'         ,'CFMIP'        ]
CNTL_MIPIDS = ['CMIP'       ,'CMIP'          ,'CFMIP'        ,'CFMIP'         ,'CFMIP'        ]

TEST_EXPIDS = ['amip-4xCO2' ,'amip-p4K'     ,'amip'          ,'aqua-4xCO2'    ,'aqua-p4K'     ]
CNTL_EXPIDS = ['amip'       ,'amip'         ,'amip-m4K'      ,'aqua-control'  ,'aqua-control' ]


TEST_MIPIDS = 'GCM'
TEST_MODELIDS = 'cam5'
TEST_EXPIDS = ['BRCP85']

CNTL_MIPIDS = 'GCM'
CNTL_MODELIDS = 'cam5'
CNTL_EXPIDS = ['B20TR']


TEST_MIPIDS = ['ERA5']
CNTL_MIPIDS = ['ERA5']
order       = 'ERA5' 

TEST_MODELIDS = replicate('climo',10)
CNTL_MODELIDS = replicate('climo',10)

TEST_EXPIDS = ['2009_2018']
CNTL_EXPIDS = ['1979_1988']

CNTL_MIPIDS = 'GCM'
CNTL_MODELIDS = 'esm'
CNTL_EXPIDS = ['B20TR']

TEST_MIPIDS = 'GCM'
TEST_MODELIDS = 'esm'
TEST_EXPIDS = ['SSP585']

; ========================================================================================================

FOR IEXP = 0, N_ELEMENTS(TEST_EXPIDS)-1 DO BEGIN

  TEST_MODELID = TEST_MODELIDS[IEXP]
  TEST_MIPID   = TEST_MIPIDS[IEXP]
  TEST_EXPID   = TEST_EXPIDS[IEXP]

  CNTL_MODELID = CNTL_MODELIDS[IEXP]
  CNTL_MIPID   = CNTL_MIPIDS[IEXP]
  CNTL_EXPID   = CNTL_EXPIDS[IEXP]


;=========================================================

 
ModelID =  TEST_MODELID
 
EXPID   = TEST_EXPID +'-' + CNTL_EXPID

casename        =  EXPID

testdir = '../cmip6/'+TEST_MIPID + '/'+MODELID+'/'+TEST_EXPID
cntldir = '../cmip6/'+CNTL_MIPID + '/'+MODELID+'/'+CNTL_EXPID
expdir  = '../cmip6/'+TEST_MIPID + '/'+MODELID+'/'+EXPID

if(strpos(modelid, 'CESM2') ge 0) then begin
  html_file0 = '../cmip6/html/' + EXPID 
endif else begin
  html_file0 =  '../cmip6/html/'+ get_lastv_inlist(modelid,'_') + '_'+ EXPID
endelse  
if(strpos(testdir, 'ERA5') ge 0) then $
  html_file0 =  '../cmip6/html/ERA5_'+ get_lastv_inlist(modelid,'_') + '_'+ EXPID

 testdir = '/Users/minghuazhang/unix/cmip6/'+TEST_MIPID + '/'+MODELID+'/'+TEST_EXPID
 cntldir = '/Users/minghuazhang/unix/cmip6/'+CNTL_MIPID + '/'+MODELID+'/'+CNTL_EXPID
 expdir  = '/Users/minghuazhang/unix/cmip6/'+TEST_MIPID + '/'+MODELID+'/'+EXPID

dir_jpeg_test    = testdir + '/' 
dir_jpeg_cntl    = cntldir + '/' 
dir_jpeg         = expdir  + '/' 

nfolders  = 3


jpeg_folders = strarr(3,5)
columnIDs    = strarr(3)

expID0   = str_replace(ExpID,'-','__')

Column_IDs = MIPID+'_'+[TEST_EXPID, CNTL_EXPID, expID0]

;=====================
FOR IS = 0, 4 DO BEGIN
 jpeg_folders[0,IS] = dir_jpeg_test + seasons[is]
 jpeg_folders[1,IS] = dir_jpeg_cntl + seasons[is]
 jpeg_folders[2,IS] = dir_jpeg      + seasons[is]

 dir_jpegs  = reform(jpeg_folders[*,IS]) + '/'
 html_file  = html_file0 +'_'+seasons[is]+'.html'

 title        =  casename+'_'+seasons[is]

;stop

 GIF2WEBS, DIR_JPEGS, Column_IDs,HTML_FILE, ORDER = ORDER, Title = title, nfolders = nfolders, $
          remove_duplicates = 0
ENDFOR ; IS
 
ENDFOR ; IEXP

print,' '
print,'   Program Finished Normally!'
print,' '

end
