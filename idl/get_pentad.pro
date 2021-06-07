;goto,jump1
;==================
caseid = '647b5'
;==================

var    = 'PRECT'

;==================
nave = 24  
ftype = 'daily'
annual_average = 0

nave = 24*5  
ftype = 'pentad'
annual_average = 0; 1

;==================

nyrs = 10 ; place holder
;==========

fileout=var

files = file_search(caseid+'.cam2.h1.*.nc')
help,files
 
k = strpos(files[0],'h1.')+3
yrs = strdigit (strmid(files,k,4) ,0)
months = strdigit (strmid(files,k+5,2),0 )
days   = strdigit (strmid(files,k+8,2),0 )

;jj1 = where((yrs ge 1980 or months ge 12) and (yrs le 1984),cntf)
;jj1 = where((yrs ge 1980 or months ge 12) and (yrs le 1984),cntf)
jj1 = where((yrs ge 1980 or months ge 12) ,cntf)
jj1 = where((yrs ge 1979 or months ge 12) ,cntf)
;==================================================================

;stop
files = files[jj1]
help,files

nf = n_elements(files)

dd3   = fltarr(10*365) ;10 yrs
time3 = dd3

i1 = 0
for ii = 0,nf-2 do begin ;discard the last file since it maybe incomplete

;for ii = 0,13 do begin
 file = files[ii]
 time1 = get_fld(file,'time')
 dd1   = get_fld(file,var)
 nt1 = n_elements(dd1[0,0,*])

 if(i1 eq 0)then begin
  nx = n_elements(dd1[*,0,0])
  ny = n_elements(dd1[0,*,0])
  dd3 = fltarr(nx,ny,nyrs*365)
  time3 = fltarr(nyrs*365)
  lon = get_fld(file,'lon')
  lat = get_fld(file,'lat')
 endif

 print,ii,nt1,' ',file

 dd2   = mean2d_intv(dd1,  nx,ny,nt1, nave)
 time2 = mean_intv(time1,nt1,nave) 

 nt2 = n_elements(time2)

 i2 = i1+nt2-1
 dd3[*,*,i1:i2] = dd2   ;to use the current chunk to fill array dd3 and time3
 time3[i1:i2] = time2
 i1 = i2+1
endfor  ; -------- files done

nt3 = i2+1
dd3 = dd3[*,*,0:i2]
time3 = time3[0:i2]

;save,filename=file2+'.sav', dd3,time3,nt3
;print,'saved nt3,dd3,time3,var in ',file2
print,min(time3),max(time3)
print,min(dd3),max(dd3)
help,nt3,var,dd3,time3

 dd33 = dd3

jump1:
file2 = caseid+'_'+ftype+'_'+fileout

if(annual_average)then begin
 nave2 = nave/24
 dd3 = ann_ave(dd33,time3,nave=nave2)
 time3=indgen(fix(365/nave2))*nave2*1.0
 file2 = file2+'_ann'  
print,min(dd3),max(dd3)
print,min(time3),max(time3)
help,nt3,var,dd3,time3
endif

data = {fileout:file2+'.nc', time:time3, lon:lon, lat:lat, bdate:19790100, PRECT:dd3}
help,data,/struc
w2d_cdf,data

end


 



