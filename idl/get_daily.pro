caseid = '647d4'
var    = 'PRECT'
hfre   = 1
nyrs   = 10
fileout=var

files = file_search(caseid+'.cam2.h1.*.nc')
help,files
nf = n_elements(files)

dd3   = fltarr(10*365) ;10 yrs
time3 = dd3

i1 = 0
for ii = 0,nf-2 do begin ;discard the last file since it maybe incomplete
;for ii = 0,2 do begin
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

 dd2   = mean2d_intv(dd1,  nx,ny,nt1, 24/hfre)
 time2 = mean_intv(time1,nt1, 24/hfre)

 nt2 = n_elements(time2)

 i2 = i1+nt2-1
 dd3[*,*,i1:i2] = dd2
 time3[i1:i2] = time2
 i1 = i2+1
endfor

nt3 = i2+1
dd3 = dd3[*,*,0:i2]
time3 = time3[0:i2]

file2 = caseid+'_'+fileout
save,filename=file2+'.sav', dd3,time3,nt3
print,'saved nt3,dd3,time3,var in ',file2
help,nt3,var,dd3,time3

data = {fileout:file2+'.nc', time:time3, lon:lon, lat:lat, bdate:19790100, PRECT:dd3}
help,data,/struc
w2d_cdf,data

end


 



