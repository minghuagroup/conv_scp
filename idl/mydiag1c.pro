
; after run nc2fig.pro ; note color folder eg. dir_jpeg_color  = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585/ANN/' and icolor

; One season, different cases

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Seasons = ['DJF','MAM','JJA','SON','ANN']

MODELID ='NCAR_CESM2'

MIPID = 'CMIP'
EXPID = 'amip'

MIPID = 'ScenarioMIP'
EXPID = 'ssp585'

SS = Seasons[0]

casename = 'SSP585__AMIP'
html_file = '../cmip6/html/' + casename +'_'+ SS + '.html' ; displays 4 seasons and annual mean

dir1   = '../cmip6/CMIP/NCAR_CESM2/amip'
dir2   = '../cmip6/ScenarioMIP/NCAR_CESM2/ssp585'
dir3   = '../cmip6/ScenarioMIP/NCAR_CESM2/' + casename  

jpeg_folders = [dir1,dir2,dir3] + '/'+SS



Column_IDs   = get_lastv_inlist(strstrip(jpeg_folders,'/', last=1), '/')

nfolders  = 3
order     = 1
title        =  str_replace(casename,'__', ' - ') + ' (' + SS + ')'

;stop

dir_jpegs = jpeg_folders


GIF2WEBS, DIR_JPEGS, Column_IDs,HTML_FILE, ORDER = ORDER, Title = title, nfolders = nfolders, $
          remove_duplicates = 0

 
end
