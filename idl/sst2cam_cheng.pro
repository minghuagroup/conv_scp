
sst_fileold = '../obs/FNL/Cheng/sst_HadOIBl_bc_1x1_clim_c101029.nc'


sst_inputfile    = '../obs/FNL/Cheng/tmidaily20112012.nc'

date=20111001
iday = 31*5 + 4*30  ;Oct 1
sst_filenew = '../obs/FNL/Cheng/INIT/sst_2011_10_01.nc'

date=20111005
iday = 31*5 + 4*30 +4 ;Oct 1
sst_filenew = '../obs/FNL/Cheng/INIT/sst_2011_10_05.nc'
;==================


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

 ice_cov  = get_fld(filein,'ice_cov')
 SST_cpl_prediddle  = get_fld(filein,'ice_cov_prediddle')

;=== get sst input data

 ncdf_vars,filein2,vars2
 x2=get_fld(filein2,'lon')
 y2=get_fld(filein2,'lat')
 nx2 = n_elements(x2)
 ny2 = n_elements(y2)

 sst2   = get_fld(filein2,'var11') - 273.16   ;!!!
 sst2   =reform(sst2[*,*,iday])

 sst1   = my_interpol2(sst2,  x2,y2,lon,lat)

SST = SST_cpl 
for im=0,11 do SST[*,*,im] = sst1

print,'sst1',min(sst1),max(sst1)
print,'sst2',min(sst2),max(sst2)


;=================================
fileID=ncdf_open(sst_filenew,/write)
NCDF_CONTROL, fileid, /FILL
;-------
VarID = NCDF_VARID(fileID, 'SST_cpl')
ncdf_varput,fileID, varID,SST
;-------
;VarID = NCDF_VARID(fileID, 'SST_cpl_prediddle')
;ncdf_varput,fileID, varID,SST
;-------

ncdf_close,fileID

print,'saved file:' ,sst_filenew

end

