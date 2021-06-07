
function get_match_list, varstr
;=============================
; return a flilename array of matched variables  column_names[nv, ncolumn] file names
;
; varstr created as follows
; varstr = create_struct('ncolumn', 2,  'names', names) ; names are list of filenames
; for i= 0, n_elements(names)-1 do begin
; varstr = create_struct(varstr, name[i], filelists[*,i])
; endfor


 ncolumn = varstr.ncolumn
 names   = varstr.names  ; full filename

; do matching files and order files
jpeggroups = get_stru(varstr, names[0])
jpeggroups2 = jpeggroups

varsj      =  get_lastv2_inlist(jpeggroups2, '/', '.', sep3 = '_')
varsj2      =  varsj

; only include the subgroup of common variables across the columnsm
kk = 0
for i = 0, n_elements(varsj)-1 do begin
 var = varsj[i]
 nn = 1
 for j = 1,varstr.ncolumn-1 do begin
   listname = get_stru(varstr, names[j])
   listvars = get_lastv2_inlist(listname, '/', '.',sep3 = '_')
   if(not belongsto(var, listvars) ) then nn = 0
 endfor ; columns
 if(nn) then begin
  jpeggroups[kk] = jpeggroups2[i]
  varsj2[kk]     = varsj[i]
  kk = kk+1
 endif
endfor
jpeggroups = jpeggroups[0:kk-1]
varsj2     = varsj2[0:kk-1]


column_groups = strarr(kk , varstr.ncolumn)
column_groups[*,0 ] = jpeggroups

; assign the file numbers
for i = 0, n_elements(varsj2)-1 do begin
 var = varsj2[i]
 for j = 0,varstr.ncolumn-1 do begin
    listnames = get_stru(varstr, names[j])
    listvars  = get_lastv2_inlist(listnames, '/', '.', sep3 = '_')
    jj = where(var eq listvars) & jj = jj[0]
    column_groups[i,j] = listnames[jj]
 endfor
endfor ; columns


return, column_groups

end
