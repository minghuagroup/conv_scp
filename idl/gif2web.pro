; =====================================================================================

PRO GIF2WEB, DIR_JPEG, HTML_FILE, NCOLUMN = NCOLUMN, ORDER = ORDER, Title = title, $
     nfolders = nfolders, remove_duplicates = remove_duplicates, all=all
; gif2web

; =====================================================================================
; =====================================================================================
; from jpeg folder to the web, the order of listed variables based on input order

; nfolders control how many layers of folders for the jpeg file to be shown in the html file
;
; input:

;dir_jpeg   = '~/unix/cmip6/test/jpeg/ANN/' 
;;dir_jpeg   = '../test/jpeg/ANN/' 
;html_file  = '../cmip6/html/test.html'

;cmip_vars = cmip6_vars()
;cesm_vars = cmip_vars.vars_cesm_all


 jpeggroups  = file_search(dir_jpeg+'/*.jpeg')
 jpeggroups0 = jpeggroups

;===========================
 varsj = get_lastv2_inlist(jpeggroups,'/', '.', sep3 = '_')

if(keyword_set(all))then begin
;-----------------
  jj = sort(varsj)
  jpeggroups = jpeggroups0[jj]
endif else begin
;-----------------

; remove duplicated fig files created from sav (low case) and sav2 (capital case)  folders
if(keyword_set(remove_Duplicates))then begin
  jj = get_unique_upcase(varsj, uniq_Vars = unique_Vars)
  varsj =  varsj[jj]
endif

 if(keyword_set(order))then begin

  varsk = varsj
  for iv       = 0,n_elements(varsj)-1 do begin
     varsk[iv] = cmip6_2cesm_var(varsj[iv], c=1)
  endfor
  jj = sort(varsk)
  jpeggroups = jpeggroups0[jj]
 endif 

endelse
;-----------------

 if(not keyword_set(ncolumn))then ncolumn = 0
 if(not keyword_set(title))then title = 0
 if(not keyword_set(nfolders))then nfolders = 0

;stop

 web_view,jpeggroups,html_file,title=title,ncolumns = ncolumn, nfolders = nfolders
 print,'webfile:',html_file
;stop
;stop

return

end
