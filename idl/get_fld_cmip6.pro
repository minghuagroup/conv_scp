; =============================================
function  get_fld_cmip6, var, dir_ROOT,MIPid, Model, Expid, Runid=Runid, dataType, Model_folder,$
         yearrange = yearrange,years=years,months=months,days=days,files = files
         ;yearrange,years=years,months=months,days=days,files = files
; =============================================

;; Change datatype to determine data frequency, 

;if(strpos(model, 'CESM') ge 0)then begin
;  dir_ROOT = '/glade/collections/cdg/data/CMIP6/'
;endif else begin
;  dir_ROOT = '/glade/collections/cmip/CMIP6/'
;endelse

dir_cesm2 =  dir_ROOT + MIPid + '/' + Model+ '/' + Expid + '/'
spawn, 'ls '+ dir_cesm2,listing
if(not keyword_set(Runid))then Runid  = listing[0] ; the first experiment

dir_cesm2 =  dir_cesm2 + Runid + '/' + dataType +'/'+var + Model_Folder + '/'
   ;  /glade/collections/cdg/data/CMIP6/'
   ;   'CMIP/  NCAR/CESM2/ amip /   r10i1p1f1/  Amon/     ts/   gn/v20200226/;
   ;   MIPid    Model     Expid      Runid    dataType
   ;   ts_Amon_CESM2_amip_r10i1p1f1_gn_200001-201412.nc   var    Model_folder
; example:
; Model  = 'NCAR/CESM2'

; MIPid  = 'CMIP'
; Expid  = 'amip'
; Expid  = 'piControl'
; Runid     = 'r1i1p1f1'
; dataType  = 'Amon/'
; Model_Folder    = '/gn/latest'


if(not keyword_set(yearrange))    then begin 
   yearrange = [80, 99] 
  ;if(strpos(expid,'CO2') ge 0 )     then yearrange = [80, 99] 
  ;if(strpos(expid,'abrupt') ge 0)   then yearrange = [80, 99] 
  if(strpos(expid,'amip') ge 0 or (strpos(expid,'historical') ge 0) )     then yearrange = [1995,2014]
  if(strpos(expid,'piControl') ge 0)     then yearrange = [281,300]
  if(strpos(expid,'ssp') ge 0)     then yearrange = [2080,2099]

  if(strpos(model, 'GFDL') ge 0 and (strpos(expid,'historical') ge 0) ) then yearrange = [1995,2014] - 1850
endif

files     = findfile(dir_cesm2+'*.nc')
;           --------------------------
nf = n_elements(files)

if(files[0] eq '')then begin
  print,' ******> No file is found:',dir_cesm2+'*.nc'
  dd = -999.0 
  return,dd
endif else begin
;  print, ' Number of files ',nf
endelse

kk = 0
for k = 0,nf-1 do begin
 
 file2     =   files[k]

 print, file2
 ncdf_vars,file2,vars2
 if(not belongsto('time',vars2)) then begin ; static field
  dd2    = get_fld(file2,var)
  return,dd2
 endif   

 time   = get_fld(file2,'time')
 year = fix(time/365.+0.99)
 dayofyear  = time - year*365.
 CALDAT, JULDAY(1, dayofyear, year), month, day

 jj = where((year ge min(yearrange)) and (year le max(yearrange)),cnt)

 if(cnt gt 0)then begin
  dd    = get_fld(file2,var)
 ;---------------------------
  sz    = size(dd)
  if(sz[0] eq 1)then return, dd  ; time independent fields (lat lon, lev etc.) 
  if(sz[0] eq 2)then return, dd  ; time independent fields (oro)

 if(kk eq 0)then begin
  times  = time[jj]
  years  = year[jj]
  months = month[jj]
  days   = day[jj]
  case sz[0] of
   3: dd2 = dd[*,*,jj]
   4: dd2 = dd[*,*,*,jj]
  endcase
  kk=1
 endif else begin
  times  = [times, time[jj] ]
  years  = [years,year[jj] ]
  months = [months,month[jj]]
  days   = [days  ,day[jj]  ]
  case sz[0] of
   3: dd2 = [[[dd2]], [[dd[*,*,jj]]] ]
   4: begin
       nx = n_elements(dd2[*,0,0,0])
       ny = n_elements(dd2[0,*,0,0])
       np = n_elements(dd2[0,0,*,0])
       ntw = n_elements(dd2[0,0,0,*]) + n_elements(jj)
       ddd = fltarr(nx,ny,np,ntw)
       for k = 0,np-1 do begin
         dw2 = reform(dd2[*,*,k,*])
         dww = reform(dd[*,*,k,jj])
         dd2w = [ [ [dw2]], [[dww]] ]
         ddd[*,*,k,*] = dd2w
       endfor 
       dd2 = ddd
       end
  endcase
 endelse

 endif ; cnt file

; help,dd2,dd
endfor  ; k file

if(kk eq 0) then begin
  print,' --- No files found in the specified time range ',yearrange
  print,' --- returned -999.0'
endif
return,dd2

end
;======================
