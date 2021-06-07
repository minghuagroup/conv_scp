;goto,jump1
;==================
caseid = '647b5'
;==================

var    = 'PRECT'
;==================

files = file_search(caseid+'.cam2.h0.*.nc')
help,files
 
k = strpos(files[0],'h0.')+3
yrs = strdigit (strmid(files,k,4) ,0)
months = strdigit (strmid(files,k+5,2),0 )

;jj1 = where((yrs ge 1980 or months ge 12) and (yrs le 1984),cntf)
;jj1 = where((yrs ge 1980 or months ge 12) and (yrs le 1984),cntf)
jj1 = where((yrs ge 1980 or months ge 12) ,cntf)
jj1 = where((yrs ge 1979 or months ge 12) ,cntf)
;==================================================================
files = files[jj1]
yrs = yrs[jj1]
months = months[jj1]
nf = n_elements(files)
time3 = indgen(nf)*1.0

yb = strmid(files[0],k,4)
ye = strmid(files[nf-1],k,4)

mb = strmid(files[0],k+5,2)
me = strmid(files[nf-1],k+5,2)

fileout=yb+'-'+mb  ;+'.'+yb+'-'+me ;just the begining yr
print,'dates:',fileout

help,files

for ii = 0,nf-1 do begin ;discard the last file since it maybe incomplete

 file = files[ii]
 dd1   = get_fld(file,var)

 if(ii eq 0)then begin
  nx = n_elements(dd1[*,0,0])
  ny = n_elements(dd1[0,*,0])
  lon = get_fld(file,'lon')
  lat = get_fld(file,'lat')
  dd3 = fltarr(nx,ny,nf)
 endif

 dd3[*,*,ii] = dd1

 print,ii,' ',file

endfor  ; -------- files done

nt3 = nf

print,min(time3),max(time3)
print,min(dd3),max(dd3)
help,nt3,var,dd3,time3

 dd33 = dd3

jump1:
file2 = caseid+'_monthly_'+var+'_'+fileout



data = {fileout:file2+'.nc', time:time3, lon:lon, lat:lat, bdate:19790100, PRECT:dd3}
help,data,/struc
w2d_cdf,data

end


 



