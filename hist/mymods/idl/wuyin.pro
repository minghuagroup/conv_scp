;
; reset data value in netcdf file

  file='fc35_609.cam2.r.1979-06-09-00000.nc'
;  ncdf_mread,file,['fincl','nhfil','nfils','nhtfrq','nflds'],dh
  ncdf_mread,file,['nhtfrq'],dh
  f_id=ncdf_open(file,/WRITE)
;  fincl_id=ncdf_varid(f_id,'fincl')
;  nfils_id=ncdf_varid(f_id,'nfils')
;  nflds_id=ncdf_varid(f_id,'nflds')
;  fincl=dh.fincl
;  nfils=dh.nfils
;  nflds=dh.nflds

  nhtfrq_id=ncdf_varid(f_id,'nhtfrq')
  nhtfrq=dh.nhtfrq

;===== set
  nhtfrq[0] = 0 ;original value 0   ; 8 dimension

  ncdf_varput,f_id, nhtfrq_id, nhtfrq


  ncdf_close,f_id
  stop

; h3 contains 8 fields, reset with the 9th field which is empty
; if adding a field, need to convert the string to byte using byte(fieldname)


;  for i=0,7 do fincl[*,i,3]=fincl[*,8,3]

;  nflds[3]=0

; set this value to the corresponding mfilt value. This may not be needed

;  nfils[3]=240

; it appears fincl not even needs to be reset
; ncdf_varput,f_id,fincl_id,fincl
; only resetting nflds nd nfils

  ncdf_varput,f_id,nflds_id,nflds
  ncdf_varput,f_id,nfils_id,nfils
  ncdf_close,f_id
  end
