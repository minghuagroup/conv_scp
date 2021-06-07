MODELID ='NCAR_CESM2'

setZ = 1
do_plot_ctl  = 0   ; if already plotted, then no need to replot
do_plot_test = 0
do_plot_diff = 1

MIPIDs = ['CMIP',  'ScenarioMIP'] 
EXPIDs = ['amip',  'ssp585']

DIFF_ID = EXPIDs[1] + '-' + EXPIDs[0]

html_file0 = '../cmip6/html/'+DIFF_ID  ; add seasons

DIR_SAVs = strarr(2)
DIR_SAVs2 = strarr(2)
DIR_Exps = strarr(2)

Seasons = ['DJF','MAM','JJA','SON','ANN']

jpeg_folders = strarr(5,3) ; 5 seasons and 3 columns
Column_IDs   = strarr(5,3) ;
for i = 0,4 do begin
 Column_IDs[i,0] = EXPIDs[0] + '_'+Seasons[i]
 Column_IDs[i,1] = EXPIDs[1] + '_'+Seasons[i]
 Column_IDs[i,2] = DIFF_ID  + '_'+Seasons[i]
endfor
;=========================================
FOR k = 0,4 DO BEGIN ; seasons
;=========================================

 season = Seasons[k]    

;------------------------------------------
 FOR ID = 0, N_ELEMENTS(EXPIDS) -1 DO BEGIN
;------------------------------------------

  MIPID = MIPIDs[ID]
  EXPID = EXPIDs[ID]

  dir_exp = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID

  dir_sav    = dir_exp+'/sav/'  ; already prepared
  dir_sav2    = dir_exp+'/sav2/'  ; already prepared
   ; check if there are added data folder ; figures added to the same folder
    filesj = file_search(dir_sav2+'*sav')   ;
    sav2_exist = (n_elements(filesj) gt 0)

  dir_jpeg   = dir_exp  + '/'  ;;+'/jpeg/'

  make_folder,dir_jpeg

  dir_jpeg0 = dir_jpeg + season+'/'


 if(do_plot_ctl and (ID eq 0))  then begin 
   SAV2GIF, dir_sav, dir_jpeg0, setZ = setZ, season=season ;month = month
   if(sav2_exist)then $
     SAV2GIF, dir_sav2, dir_jpeg0, setZ = setZ, season=season ;month = month
 endif


 if(do_plot_test and (ID eq 1))  then begin
    SAV2GIF, dir_sav, dir_jpeg0, setZ = setZ, season=season ;month = month
   if(sav2_exist)then $
     SAV2GIF, dir_sav2, dir_jpeg0, setZ = setZ, season=season ;month = month
 endif


 ;^^^^^^^^^^^^^^^^^^^^^^^^^^


 dir_savs[ID] = dir_sav
 dir_savs2[ID] = dir_sav2
 dir_exps[ID] = dir_exp

 jpeg_folders[k, ID] = dir_jpeg0

 ENDFOR ; EXIDs
;-----------------------------


; make the difference plots

 dir_jpeg_diff = '../cmip6/'+MIPID+'/'+MODELID+'/'+ DIFF_ID + '/' + season +'/'

 make_folder, dir_jpeg_diff

 if(do_plot_diff) then  begin
   ;SAV2GIF_DIFF, dir_savs, dir_jpeg_diff, setZ = setZ, season = season
 ;^^^^^^^^^^^^^^^^^^^^^^^^^^
   if(sav2_exist)then begin
    SAV2GIF_DIFF, dir_savs2, dir_jpeg_diff, setZ = setZ, season = season
   endif
 endif  
 

 jpeg_folders[k, 2] = dir_jpeg_diff
 
 jjr = [1,0,2]

 title    = 0
 nfolders = 4

 html_file  = html_file0 + '_' + seasons[k] + '.html'
 DIR_JPEGS  = reform(jpeg_folders[k,jjr])
 Column_ID2 = reform(Column_IDs[k,jjr])
 for j = 0, 2  do begin
  Column_ID2[j] = str_replace(Column_ID2[j], '-', '__')
 endfor

 GIF2WEBS, DIR_JPEGS, Column_ID2,HTML_FILE, ORDER = 1, Title = title, nfolders = nfolders, $
           remove_duplicates = 0 


ENDFOR ; seasons k
;=============================


end
