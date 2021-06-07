
; This is to display multiple colums to compare

MODELID ='NCAR_CESM2'
MIPID   = 'ScenarioMIP'
EXPID   = 'ssp585'

CTL_MODELID ='NCAR_CESM2'
CTL_MIPID   = 'CMIP'
CTL_EXPID   = 'amip'

ID1 =     MIPID + '_' + EXPID                   ; modelID is neglected here
ID2 = CTL_MIPID + '_' +  CTL_EXPID

ID1 =      EXPID                   ; modelID and MIPID are neglected here
ID2 =  CTL_EXPID

Title = ID1 + '-' + ID2

expdir     = '/Users/minghuazhang/unix/cmip6/'+     MIPID + '/' +   ModelID + '/' +    EXPID
CTL_expdir = '/Users/minghuazhang/unix/cmip6/'+ CTL_MIPID + '/' + CTL_ModelID+ '/' + CTL_EXPID

dir_jpeg1 = expdir+'/jpeg/'
dir_jpeg2 = CTL_expdir+'/jpeg/'
dir_jpegs = [dir_jpeg1, dir_jpeg2]
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


dir_html ='../cmip6/html/'+MIPID+ '/'
make_folder,dir_html
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Seasons = ['DJF','MAM','JJA','SON','ANN']

FOR ISEASON = 0,0 DO BEGIN
;==============================

htmlfile = dir_html+Title+'_'+seasons[iseason]+'.html' ; could be a test file

varstr = create_struct('ncolumn', 2,  'names', [ID1, ID2])
;                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dir_jpegs2 = dir_jpegs + Seasons[iseason]+'/'

for i = 0, n_elements(dir_jpegs2) -1 do begin
  cnames = varstr.names[i]
  files = file_search(dir_jpegs2[i]+'/*.jpeg')
  varstr = create_struct(varstr, cnames, files)
 endfor


;=========================== do variable match

column_groups = get_match_list(varstr)

test_files = reform(column_groups[*,0])
varsj      = get_lastv2_inlist( test_files, '/', '.', sep3 = '_')

; get the corresponding CESM variable for sort and title
vars_cesm = varsj
for i = 0,n_elements(varsj)-1 do begin
  vars_cesm[i] = cmip6_2cesm_var(varsj[i], c=1)
endfor

; sort based on cesm variables
jj = sort(vars_cesm)
varsj = varsj[jj]
vars_cesm = vars_cesm[jj]

column_groups2 = column_groups
for j=0,varstr.ncolumn-1 do begin
  cj = reform(column_groups2[*,j])
  column_groups[*,j] = cj[jj]
endfor 

jpeggroups = reform(transpose(column_groups),n_elements(varsj) * varstr.ncolumn)

; ===== 

 ;dir='../'
 ;jpeggroups = dir+jpeggroups
 web_view,jpeggroups,htmlfile,title=title,ncolumns=varstr.ncolumn, nfolders = 4
 print,'webfile:',htmlfile

ENDFOR ; season

end
