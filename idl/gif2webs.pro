; =====================================================================================

PRO GIF2WEBS, DIR_JPEGS, IDS, HTML_FILE,  ORDER = ORDER, Title = title, nfolders = nfolders, $
     ref_column = ref_column, remove_duplicates = remove_duplicates
; gif2web

; =====================================================================================
; =====================================================================================
; IDS are the names of the columns
; from jpeg folders to the web, the order of listed variables based on input order
; number of column based on number of folders in DIRS_JPEG

; nfolders control how many layers of folders for the jpeg file to be shown in the html file
;
; input:

 IDS = str_replace(IDs,'-','_')

 ncolumn = n_elements(IDs)
 nc      = ncolumn

 if(not keyword_set(ref_column))then ref_column = 0
 if(not keyword_set(nfolders)) then  nfolders = 4  ; level of folders to display in web title
 if(not keyword_set(title))then title = 0

 varstr = create_struct('ncolumn', ncolumn,  'names', IDS)
;                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

for i = 0, nc -1 do begin
  cnames = varstr.names[i]
  files = file_search(dir_jpegs[i]+'/*.jpeg')
  varstr = create_struct(varstr, cnames, files)   ;eg. amip_DJF : all file names
endfor


column_groups = get_match_list(varstr)  ; returns array of matched variables

;stop
test_files = reform(column_groups[*,ref_column])
varsj      = get_lastv2_inlist( test_files, '/', '.', sep3 = '_')

; remove duplicated fig files created from sav (low case) and sav2 (capital case)  folders

if(keyword_set(remove_Duplicates))then begin
  varsj3 = varsj
  column_groups3 =  column_groups
  k = 0
  for i = 0, n_elements(Varsj)-1 do begin

   if(not var_duplicate(varsj[i], up=1)) then begin
     varsj3[k] = varsj[i]
     column_groups3[k,*] =  column_groups[i,*]
     k = k+1
   endif
  endfor
   varsj=varsj3[0:k-1] 
  column_groups =  column_groups3[0:k-1,*]
endif 

; get the corresponding CESM variable for sort and title
vars_cesm = varsj
for i = 0,n_elements(varsj)-1 do begin
  vars_cesm[i] = cmip6_2cesm_var(varsj[i], c=1)
endfor

if(not keyword_set(order))then order =0
if(order eq 'ERA5')then begin
   cmip6_vars = cmip6_vars()
   vars_cesm_all = cmip6_vars.vars_cesm_all

   vars_cesm = varsj
     for i = 0,n_elements(varsj)-1 do begin
       vars_cesm[i] = era5_2cesm_var(varsj[i], c=1)
       if(not belongsto(vars_cesm[i], vars_cesm_all))then vars_cesm[i] = strlowcase(vars_cesm[i] )
      endfor
endif

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


 web_view,jpeggroups,html_file,title=title,ncolumns=varstr.ncolumn, nfolders = nfolders
 print,'webfile:',html_file

return

end
