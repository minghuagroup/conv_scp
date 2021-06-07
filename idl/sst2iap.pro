
;sst_fileold = '../obs/SST/sst_HadOIBl_bc_1.9x2.5_clim_c061031.nc'
;sst_fileold = sst_filenew

;ICE INPUT
sst_fileold0 = '../obs/SST/HadOIB1_2009_IAP.nc' ;'my_sst_clim_20170727_IAP.nc'
sst_fileold = '../obs/SST/HadOIB1_1850_01+2009_12.nc' ;'HadOIB1_2009_IAP.nc' ;'my_sst_clim_20170727_IAP.nc'
sst_inputfile    = '../obs/SST/avhrr-only-v2.20170726_preliminary.nc'
sst_filenew = '../obs/SST/my_sst_clim_20170727_IAP.nc' 

;sst_fileold = '../obs/SST/HadOIB1_1850+2009.nc' ;'HadOIB1_2009_IAP.nc' ;'my_sst_clim_20170727_IAP.nc'
;sst_fileold = '../obs/SST/sst_HadOIBl_bc_128x256_1850_2009_c111225_IAP.nc'
;mn = 7
;sst_inputfile    = '../obs/SST/amsr-avhrr-v2.20080115.nc'
;mn = 1
;sst_inputfile    = '../obs/SST/amsr-avhrr-v2.20080715.nc'
;mn = 7
;sst_filenew = '../obs/SST/my_sst_clim_2008_01_07.nc' 

date=20170727


SPAWN, 'cp '+ sst_fileold+' '+sst_filenew

filein = sst_filenew
filein2 = sst_inputfile

 print,'to rewrite ',filein
 print,'use data from ',filein2

;=== get CAM info as target

 ncdf_vars,filein,vars
 lon  = get_fld(filein,'lon')
 lat  = get_fld(filein,'lat')
 SST_cpl  = get_fld(filein,'SST_cpl')   ;oC
 SST_cpl_prediddle  = get_fld(filein,'SST_cpl_prediddle')

;ICE 12 months here
 ice_cov0  = get_fld(sst_fileold0,'ice_cov')
 ice_cov_cpl_prediddle0  = get_fld(sst_fileold0,'ice_cov_prediddle')
;take July here
 ice_cov7 = reform(ice_cov0[*,*,6])
 ice_cov_cpl_prediddle7 = reform(ice_cov_cpl_prediddle0[*,*,6])


;=== get sst input data

 ncdf_vars,filein2,vars2
 x2=get_fld(filein2,'lon')
 y2=get_fld(filein2,'lat')
 nx2 = n_elements(x2)
 ny2 = n_elements(y2)

 sst2   = get_fld(filein2,'sst')/100.  ; scaling factor
 sst1   = my_interpol2(sst2,  x2,y2,lon,lat)

 SST = SST_cpl
 ice_cov = sst*0.

; mn=mn-1
;  jj = where(sst1 le -5.,cnt)           ;!!!
;  if(cnt gt 0)then begin
;    SST_m = reform(SST[*,*,mn])
;    SST1[jj] = SST_m[jj]
;  endif

;!!!!!!!!! neigbouring month
; nm2 = mn 
; if(nm2 lt 0)then nm2 = 12 
; SST[*,*,mn] = SST1
; SST[*,*,nm2-1] = SST1
; SST[*,*,nm2+1] = SST1

m = n_elements(sst[0,0,*])
for nm=0,m-1 do begin
 sst[*,*,nm] = sst1  ; every month the same
 ice_cov[*,*,nm] = ice_cov7
endfor
print,'steps=',m
                                   ;!!!!!!!!!!!!!!!!!!!!
  
print,'sst_cpl',min(sst_cpl),max(sst_cpl)
print,'sst_cpl__prediddle',min(sst_cpl_prediddle),max(sst_cpl_prediddle)
print,'sst2',min(sst2),max(sst2)
print,'sst1',min(sst1),max(sst1)
print,'sst',min(sst),max(sst)

;jj=where(TS2 lt 0., cnt) & if(cnt gt 0)then TS2[jj] =  TS[jj]
print,'input file is: ',sst_inputfile
print,'old file is  : ',sst_fileold
print,'save file ?  ?', sst_filenew
read,ix
if(ix ne 1)then stop

;=================================
fileID=ncdf_open(sst_filenew,/write)
NCDF_CONTROL, fileid, /FILL
;-------
VarID = NCDF_VARID(fileID, 'SST_cpl')
ncdf_varput,fileID, varID,SST
;-------
VarID = NCDF_VARID(fileID, 'SST_cpl_prediddle')
ncdf_varput,fileID, varID,SST
;-------
VarID = NCDF_VARID(fileID, 'ice_cov')
ncdf_varput,fileID, varID,ice_cov
;-------
VarID = NCDF_VARID(fileID, 'ice_cov_prediddle')
ncdf_varput,fileID, varID,ice_cov
;-------
VarID = NCDF_VARID(fileID, 'date')
dates = get_fld(sst_fileold,'date')
dates = dates - 20090000 + 19790000   ;!!!!!!!!
;;ncdf_varput,fileID, varID,dates
;-------
VarID = NCDF_VARID(fileID, 'datesec')
datesec = get_fld(sst_fileold,'datesec')
datesec = datesec*0
;;ncdf_varput,fileID, varID,datesec
;-------
VarID = NCDF_VARID(fileID, 'time')
time = get_fld(sst_fileold,'time')
t0   = time[0]
time = time - t0 +15.5
;;ncdf_varput,fileID, varID,time
;-------


ncdf_close,fileID

print,'saved file:' ,sst_filenew

end

