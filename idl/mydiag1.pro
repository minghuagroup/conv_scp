
; after run nc2fig.pro ; note color folder eg. dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/ANN/' and icolor


FOUR_SEASONS = 2   ;default DJF, JJA and ANN
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Seasons = ['DJF','MAM','JJA','SON','ANN']

MODELID ='NCAR_CESM2'

MIPID = 'CMIP'
EXPID = 'amip'

MIPID = 'ScenarioMIP'
EXPID = 'ssp585'

; models can be looped here too

casename        =  ModelID + '_'+MIPID + '_' + EXPID

expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID


html_file = '../cmip6/html/' + MIPID + '_' + EXPID + '.html' ; displays 4 seasons and annual mean

dir_jpeg    = expdir+ '/' 
jpeg_folders = dir_jpeg + seasons

dir_jpegs  = jpeg_folders
Column_IDs   = seasons
if(not four_seasons)then begin
  jj = [0,2,4]
  dir_jpegs = dir_jpegs[jj]
  Column_IDs   = seasons[jj]
endif

nfolders  = 3
order     = 1
title        =  casename


GIF2WEBS, DIR_JPEGS, Column_IDs,HTML_FILE, ORDER = ORDER, Title = title, nfolders = nfolders, $
          remove_duplicates = 1

 
end
