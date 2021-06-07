;---------------------------------------------------------------
; gpcp2netcdf.pro
;
; Convert yearly files of GPCP SG monthly precip and error data (at
; 2.5x2.5 deg) to CF-compliant NetCDF at 1x1 deg (using bilinear
; interpolation).
;
; This IDL source file contains the read_gpcp procedure, which reads
; all the data fields in a GPCP binary data file.  
; Recall that the data files always have 12 months of data space, 
; even if you are accessing the current year and some months have not
; yet occurred.  
;
; The read_gpcp procedure swaps the bytes of the data arrays if needed.
;
; Instructions:
;
; * Type "idl" at the UNIX command prompt to start IDL.
; * Type ".run gpcp2netcdf.pro" to compile the procedures.
; * Type "gpcp2netcdf, FILE, DATA, HEADER" where 
;   "year_start" = first year to be converted
;   "year_end"   = last year to be converted
;   "basedir"    = user-specified name of the directory containing 
;                  subdirectories for the input files and output.
;                  It must be contained in double quotes (" ") if it
;                  contains characters that are meaningful to the IDL 
;                  command-line interpreter, such as "."
;
; Change log:
; R. Stockli/MeteoSwiss   07/25/2011 
; G.J. Huffman/SSAI       07/25/2012 Upgrade documentation for V2.2
;                                    but not tested
;---------------------------------------------------------------


PRO read_gpcp, file, data, header
;-----------------------------------------
;	The main procedure; create the data structure, read both 
;	header and data, and swap bytes if needed.
;-----------------------------------------
  
  num_lon		= 144
  num_lat		= 72
  num_mon		= 12
  
  struc = { header:	bytarr(num_lon*4), $
            data:		fltarr(num_lon,num_lat,num_mon) $
          }
  
  openr,lun, file, /get_lun
  readu,lun, struc
  free_lun,lun
  
; ----- Determine byte order of system and of file (keyed to "machine=
;	Silicon Graphics").
;
  arch            = strlowcase( !version.arch )
  
  if (arch eq 'x86') OR (arch eq 'alpha') OR (arch eq 'i386') OR (arch eq 'x86_64') $
  then system_byte_order       = 'little_endian' $
  else system_byte_order       = 'big_endian'
  
  if strpos( string(struc.header), 'Silicon') ne -1 $
  then file_byte_order		= 'big_endian' $
  else file_byte_order		= 'little_endian'
;
; ----- If necessary, swap the bytes of the float variables.
;
  if system_byte_order ne file_byte_order then begin
     print, 'byte_swap_V2.2: warning: swapping bytes...'
     struc.data  		= swap_endian( struc.data )
  endif

  data = struc.data
  header = str_sep( strtrim(string(struc.header),2), ' ' )

END

PRO gpcp2netcdf, year_start, year_end, basedir

  ;; converts GPCP Satellite-Gauge Monthly dataset into Monthly 1x1
  ;; degree NetCDF files.

;x  year_start = 1979
;x  year_end = 2009

  nmonth = 12

;x  basedir = '/Users/stockli/GPCP/'
;x  basedir = '/project/msclim/stockli/GPCP/'

  psgdir = basedir+'psg/'
  esgdir = basedir+'esg/'
  outdir = basedir+'netcdf/'

  title='GPCP Version 2.2 Combined Data Sets'
  version='2.2' 
  creation_date='20120701'
  variable='precip' 
  technique='satellite/gauge' 
  units='mm/day'
  missing_value=-99999.
  contact='Mr. D. Smith, NCDC, Rm. 120, 151 Patton Ave, Ashville, NC  28801-5001 USA'
  telephone='828-271-4053' 
  facsimile='828-271-4328' 
  internet='dsmith@ncdc.noaa.gov'

  varnames = ['Precip','Error']
  varlongnames = ['Precipitation Estimate from the GPCP satellite/gauge combined data set', $
                  'Precipitation Error Estimate from the GPCP satellite/gauge combined data set']
  varunits = ['mm/day','mm/day']
  nvar = n_elements(varnames)
  
  NAN = 9.96921E+36             ; default NetCDF floating point fill value

  ;; input grid
  inx = 144
  iny = 72
  idx = 360.d0 / double(inx)
  idy = 180.d0 / double(iny)
  ilon = dindgen(inx) * idx - 180.d0 + 0.5d0 * idx
  ilat = dindgen(iny) * idy - 90.d0 + 0.5d0 * idy

  ;; output grid
  onx = 360
  ony = 180
  odx = 360.d0 / double(onx)
  ody = 180.d0 / double(ony)
  olon = dindgen(onx) * odx - 180.d0 + 0.5d0 * odx
  olat = dindgen(ony) * ody - 90.d0 + 0.5d0 * ody

  ;; create output grid bilinear interpolation indices on input grid
  ;; coordinates
  xval = ((olon - min(ilon)) / (max(ilon) - min(ilon)) * double(inx-1)) # replicate(1.d0,ony)
  yval = replicate(1.d0,onx) # ((olat - min(ilat)) / (max(ilat) - min(ilat)) * double(iny-1))

  FOR year=year_start,year_end DO BEGIN

     syear = string(year,format='(I4.4)')

     ;; define file names
     psgfile = psgdir + 'gpcp_v' + version + '_psg.' + syear
     esgfile = esgdir + 'gpcp_v' + version + '_esg.' + syear

     ;; read data
     read_gpcp, psgfile, psgdata, psgheader

     ;; read errors
     read_gpcp, esgfile, esgdata, esgheader

     ;; shift data and errors:
     ;; original data is from 0E - 360E and goes from 90N - 90S
     ;; output grid should go from -180E - 180E and from 90S to 90N
     psgdata = reverse(shift(psgdata,inx/2,0,0),2)
     esgdata = reverse(shift(esgdata,inx/2,0,0),2)

     ;; add NAN for missing values
     badidx = where(psgdata EQ missing_value,badcount)
     IF badcount GT 0 THEN psgdata[badidx] = !values.f_nan
     badidx = where(esgdata EQ missing_value,badcount)
     IF badcount GT 0 THEN esgdata[badidx] = !values.f_nan

     ;; write yearly NetCDF file regridded monthly data fields
     outfile = 'gpcp_v2.2.'+syear+'.nc'
     ncid = NCDF_CREATE(outdir+outfile,/CLOBBER)
     londim = NCDF_DIMDEF(ncid, 'lon',onx)
     latdim = NCDF_DIMDEF(ncid, 'lat',ony)
     timedim = NCDF_DIMDEF(ncid, 'time',12)

     lonid = NCDF_VARDEF(ncid, 'lon', [londim], /DOUBLE)
     NCDF_ATTPUT,ncid,lonid,'long_name','Longitude'
     NCDF_ATTPUT,ncid,lonid,'units','degrees_east'
     
     latid = NCDF_VARDEF(ncid, 'lat', [latdim], /DOUBLE)
     NCDF_ATTPUT,ncid,latid,'long_name','Latitude'
     NCDF_ATTPUT,ncid,latid,'units','degrees_north'
     
     timeid = NCDF_VARDEF(ncid, 'time', [timedim], /DOUBLE)
     NCDF_ATTPUT,ncid,timeid,'long_name','time (utc)'
     NCDF_ATTPUT,ncid,timeid,'units','months since '+syear+'-1-1 0:0:0'

     ncvarid = lonarr(nvar)
     FOR v = 0,nvar-1 DO BEGIN
        ncvarid[v] = NCDF_VARDEF(ncid,varnames[v], [londim,latdim,timedim], /FLOAT)

        ;; Attributes for data variables
        NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',NAN
        NCDF_ATTPUT, ncid, ncvarid[v], 'axis','YX'                              
        NCDF_ATTPUT, ncid, ncvarid[v], 'long_name',varlongnames[v]
        NCDF_ATTPUT, ncid, ncvarid[v], 'units', varunits[v]
        NCDF_ATTPUT, ncid, ncvarid[v], 'version', version
        NCDF_ATTPUT, ncid, ncvarid[v], 'prod_date', creation_date
     ENDFOR

     ;; create global attributes
     NCDF_ATTPUT, ncid, /GLOBAL,'title',title
     NCDF_ATTPUT, ncid, /GLOBAL,'technique',technique
     NCDF_ATTPUT, ncid, /GLOBAL,'contact',contact
     NCDF_ATTPUT, ncid, /GLOBAL,'telephone',telephone
     NCDF_ATTPUT, ncid, /GLOBAL,'facsimile',facsimile
     NCDF_ATTPUT, ncid, /GLOBAL,'internet',internet
     NCDF_ATTPUT, ncid, /GLOBAL,'post-processing','Reto Stockli (MeteoSwiss): transformed into a 1x1 global grid by use of bilinear interpolation'

     ;; Put file in data mode.          
     NCDF_CONTROL, ncid, /ENDEF 
     
     ;; write longitude, latitude
     NCDF_VARPUT, ncid, lonid, olon
     NCDF_VARPUT, ncid, latid, olat

     FOR month = 1,nmonth DO BEGIN
     
        ;; write time
        NCDF_VARPUT,ncid,timeid,double(month-1),offset=[month-1],count=[1]
 
        ;; regrid data to output grid
        temp = psgdata[*,*,month-1]
        outdata = bilinear([temp[0,*],temp,temp[inx-1,*]],xval+1.d0,(yval>0.0)<double(iny-1),missing=!values.f_nan)

        badidx = where(finite(outdata,/NAN),badcount)
        IF badcount GT 0 THEN outdata[badidx] = NAN
                      
        NCDF_VARPUT, ncid, ncvarid[0],outdata,offset=[0,0,month-1],count=[onx,ony,1]

        ;; regrid error to output grid
        temp = esgdata[*,*,month-1]
        outdata = bilinear([temp[0,*],temp,temp[inx-1,*]],xval+1.d0,(yval>0.0)<double(iny-1),missing=!values.f_nan)

        badidx = where(finite(outdata,/NAN),badcount)
        IF badcount GT 0 THEN outdata[badidx] = NAN
                      
        NCDF_VARPUT, ncid, ncvarid[1],outdata,offset=[0,0,month-1],count=[onx,ony,1]

     ENDFOR

     ;; close NetCDF file
     NCDF_CLOSE, ncid 

  ENDFOR

  stop
  
END

;-----------------------------------------
;	Help.
;-----------------------------------------
; * Type "gpcp2netcdf, FILE, DATA, HEADER" where 
;   "year_start" = first year to be converted
;   "year_end"   = last year to be converted
;   "basedir"    = user-specified name of the directory containing 
;                  subdirectories for the input files and output.
;                  It must be contained in double quotes (" ") if it
;                  contains characters that are meaningful to the IDL 
;                  command-line interpreter, such as ".", and 

	print, ' '
	print, 'gpcp2netcdf, year_start, year_end, basedir'
	print, '"year_start" = first year to be converted'
	print, '"year_end"   = last year to be converted'
	print, '"basedir"    = user-specified name of the directory containing'
	print, '               subdirectories for the input files and output.'
	print, '               It must be contained in double quotes (" ") if it'
	print, '               contains characters that are meaningful to the IDL'
	print, '               command-line interpreter, such as "."'
	print, ' '

	end
