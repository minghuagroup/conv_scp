pro hdf_read,filen,var,        data,geolocation,time
;             IN   IN(1 var)  OUT ---
 
 sdsfileid = hdf_sd_start(filen,/read)
 
 
 sds_id = hdf_sd_select(sdsfileid,hdf_sd_nametoindex(sdsfileid, 'geolocation'))

 hdf_sd_getdata, sds_id, geolocation

 sds_id = hdf_sd_select(sdsfileid,hdf_sd_nametoindex(sdsfileid, var))
 hdf_sd_getdata, sds_id, data

 hdf_sd_end, sdsfileid
; -------------------------------

  file_handle = hdf_open(filen,/read)
  vdata_ID = hdf_vd_find(file_handle,'scan_time')
vdata_H = hdf_vd_attach(file_handle,vdata_ID)
    hdf_vd_get,vdata_H,name=name,fields=raw_field
    fields = str_sep(raw_field,',')
    nscan = hdf_vd_read(vdata_h,time)
    hdf_vd_detach, Vdata_H
    hdf_close,file_handle  

end



