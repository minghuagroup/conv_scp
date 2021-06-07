caseid = '647d4'
var    = 'PRECT'
hfre   = 1
nyrs   = 10
fileout=var+'_diurnal'

ndays = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

files = file_search(caseid+'.cam2.h1.*.nc')
help,files
nf = n_elements(files)

  nx = 256
  ny = 128
  yr0 = 1979
  yr2 = 1982

  djf = fltarr(nx,ny,24,nf)
  jja = fltarr(nx,ny,24,nf)


i1 = 0
;=======================
for ii = 0,nf-2 do begin ;discard the last file since it maybe incomplete yr limit can be added here
;for ii = 0,7 do begin  ;======== files
 file = files[ii]
 time1 = get_fld(file,'time')
 dd1   = get_fld(file,var)
 nt1 = n_elements(dd1[0,0,*])

 print,ii,'  ',file,min(dd1),max(dd1)

 for i=0,nx-1 do begin
 for j=0,ny-1 do begin
   dj = dd1[i,j,*]

   days = (time1 mod 365)  ; days from 0
   hours = fix(days*24 mod 24)

   jj1 = where((days lt 59. or days ge 334. ) ,cnt1)  ; yr can be added here
   jj7 = where(days ge 151. and days le 242.,cnt7)    ; same
     
   for it = 0,23 do begin
   if(cnt1 gt 0)then begin
     jjk = indgen(cnt1/24)*24 + it
     djf[i,j,it,ii] = mean(dj[jjk])  ; averaged to hrly
    endif
    if(cnt7 gt 0)then begin
     jjk = indgen(cnt7/24)*24 + it
     jja[i,j,it,ii] = mean(dj[jjk])  ; averaged to hrly
    endif
   endfor ;it 
 endfor
 endfor

endfor ;file

dd_jja = ave4(jja)
dd_djf = ave4(djf)
time = indgen(24)+0.5

lon = get_fld(file,'lon')
lat = get_fld(file,'lat')

file2 = caseid+'_'+fileout
save,filename=file2+'.sav', dd_jja,dd_djf,time,lon,lat,var
print,'saved dd,time,var,lon,lat ',file2
help,var,dd_jja,dd_djf

data = {fileout:file2+'.nc', time:time, lon:lon, lat:lat, bdate:19790100, PRECT_1:dd_djf, PRECT_7:dd_jja}
help,data,/struc
w2d_cdf,data

end


 



