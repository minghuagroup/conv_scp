; after run nc2sav, just to view a folder on web

MODELID ='NCAR_CESM2'

MIPID = 'CMIP'
EXPID = 'amip'


EXPID ='ssp585-historical'
SS    = 'DJF'

MIPID = 'CFMIP'
EXPID = 'amip-p4K

MIPID = 'ScenarioMIP'
EXPID = 'ssp585'

MIPID = 'ERA5'
MODELID = 'climo'
EXPID = '1979_1988'
;e.g.
; dir_sav   = '/Users/minghuazhang/unix/cmip6/CMIP/NCAR_CESM2/amip/ANN/'

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^

expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID

; make_folder,dir_sav

dir_jpeg   = expdir+'/'+ SS +'/'    ; 
;dir_jpeg   = expdir+'/'+ SS+'_jj'    ; 

dir_jpeg = '../cmip6/ERA5/climo/1979_1988/DJF/'
dir_jpeg = '../ERA5/climo/1979_1988/DJF/'
dir_jpeg = '/Users/minghuazhang/unix/cmip6/ERA5/climo/1979_1988/DJF/'
dir_jpeg = '/Users/minghuazhang/unix/cmip6/ScenarioMIP/NCAR_CESM2/ssp585/DJF/'
dir_jpeg = '../cmip6//GCM/cam5/LENS/jpeg_LENS/'


html_file       = '../cmip6/html/'+ EXPID+ '_' + SS +'.html'
html_file       = '../cmip6/html/'+ EXPID+ '_' + SS +'_JJ.html'
html_file       = '../cmip6/html/test_' + SS +'.html'
html_file       = '../cmip6/html/cam5_LENS_ANN.html'
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^

title = ModelID + '_'+ EXPID
title = 'CAM5 LENS ANN'

if(not file_exist(dir_jpeg+'/*.jpeg')) then begin
  print,' ==================================================
  print,' jpeg files in dir_jpeg  does not exist', dir_jpeg+'/'
  print,' ==================================================
  stop
endif

gif2web, dir_jpeg, html_file, ncolumn = 3, title = title, nfolder = 6, all = 1, remove=0
print,' html_file is :  ', html_file

Seasons = ['DJF','MAM','JJA','SON','ANN']
; ====================================

 
end
