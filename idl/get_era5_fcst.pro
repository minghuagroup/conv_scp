
;-------------------------------------------
  pro get_era5_fcst,var,yyyymmddhh,aa,aam,file=file, accu=accu, hr24=hr24, mn=mn
;-------------------------------------------
; input: var, yyyymmddhh, see notes of get_era5, single level fields only
; output, aa, aam(daily mean), file
;-------------------------------------------

restore,'era5.sav0' 
;        lon,lat,levels,varsm,varss,varsv,varsf,varsi,dir,dirm,dirs,dirv,dirf,diri

if(keyword_set(accu))  then dirsf = 'e5.oper.fc.sfc.accumu/'
if(not keyword_set(hr24))  then hr24=0
if(not keyword_set(mn))    then mn  =0

var1 = var
if(strmid(var,0,4) eq 'VAR_')then var1 = strmid(var,4,strlen(var)-4)
var1 = strlowcase(var1) 

yy = strmid(yyyymmddhh,0,4) 
mm = strmid(yyyymmddhh,4,2) 
dd = strmid(yyyymmddhh,6,2) 
hh = strmid(yyyymmddhh,8,2) 

yyv = strdigit(yy,0)+0
mmv = strdigit(mm,0)+0
ddv = strdigit(dd,0)+0
hhv = strdigit(hh,0)+0

month = strmid(yyyymmddhh,0,6) ;'201707/'
day   = strmid(yyyymmddhh,0,8) ;'20170727/'

filef = file_search(dir+dirf+month+'/*_'+var1+'.*'+month+'*_*.nc', count=nff)
fileac = file_search(dir+dirac+month+'/*_'+var1+'.*'+month+'*_*.nc', count=nfac)
if(nfac gt 0)then filef=fileac

if(ddv le 16)then yyyymmddhh2 = caldate2(yyyymmddhh,-20.)
if(ddv gt 16)then yyyymmddhh2 = caldate2(yyyymmddhh,-40.) ; previous month

month = strmid(yyyymmddhh2,0,6) ;'201707/'
filef2 = file_search(dir+dirf+month+'/*_'+var1+'.*'+month+'16*_*.nc', count=nff)
fileac2 = file_search(dir+dirac+month+'/*_'+var1+'.*'+month+'16*_*.nc', count=nff)
if(nfac gt 0)then filef2=fileac2

filef2 = filef2[0]
if(not belongsto(filef2,filef))then filef=[filef2,filef]
nff = n_elements(filef)

month = strmid(yyyymmddhh,0,6) ;'201707/'
day   = strmid(yyyymmddhh,0,8) ;'20170707/'
print,var, ' ', month

ncdf_Vars,filef[0],varsj
jj = where(varsj eq var,cnt)
if(cnt eq 0)then begin
 print,'get_era5_fcst var not found in file ',var, ' ',file
 stop
endif

 for kk = 0,nff-1 do begin
   file = filef[kk]
 print,kk,'  ',file

    utc = get_fld(file,'utc_date')+6

    nd   = n_elements(utc)
    h12  = indgen(12)
    utc2 = lonarr(nd*12)             ; stitch together 12 hrs

      for id = 0,nd-1 do begin
        for ih = 0,11 do begin
           k = id*12 + ih
           utc2[k] = utc[id] + ih  
        endfor
       endfor
      utcs = strtrim(utc2,2)       
      utcs = caldate2(utcs,0)       ;convert to date format

      dd2 = get_fld(file,var) ; dd2[1280, 640, 19, 30] 
       
      sz = size(dd2) 
; stitch together to get a daily field

      dd1 = reform(dd2[*,*,6:17,*],sz[1],sz[2],12*sz[4])  ;6:17 hour 6 to 18 forecast

  if(kk eq 0)then begin
     utcsj = utcs
     ddj   = dd1 
  endif else begin
     utcsj = [utcsj,utcs]
     ddj   = [[[ddj]],[[dd1]] ]
  endelse
endfor ;kk

   utcs = utcsj
   dd1 = ddj

   jj = where(strmid(utcs,0,8) eq day,cnt2)
   if(cnt2 gt 0)then aam = ave3(dd1[*,*,jj],three=3)

   jj = where(utcs eq yyyymmddhh,cnt1)
   if(hr24)then jj = where(strmid(utcs,0,8) eq day) 
   if(mn) then  jj = where(strmid(utcs,0,6) eq month) 
   aa = reform(dd1[*,*,jj])

 file=filef
 help,var
 help,aa,aam


 return

end
