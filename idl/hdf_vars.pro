
pro hdf_vars,filen,names,vdata_name
;            IN    OUT    OUT
 
 sdsfileid = hdf_sd_start(filen,/read)
 
 hdf_sd_fileinfo,sdsfileid,numsds,ngatt


    names = strarr(numsds)
    ndims = lonarr(numsds)
    dtype = strarr(numsds)
    unit=   dtype

; Print out a table of the name, number of dimensions and type of each SDS
; in file    
    for i = 0, numsds - 1 do begin
        sds_id = hdf_sd_select(sdsfileid, i)
        hdf_sd_getinfo, sds_id, name = na, ndim = nd,type= typ
        names( i ) = na 
        ndims( i ) = nd
        dtype(i) = typ  
    endfor
    F1='(" ",A,I4)'
    print,'List of SDS names'
    print,' # of SDSs = ',numsds,FORMAT=F1
    print,''
    if numsds gt 0 then begin
        print,"     Label       Dims   Type  unit"
        print,"---------------- ---- --------"
        for i=0,numsds-1 do begin
            print,names(i),ndims(i),dtype(i),unit(i), $
                 FORMAT='(A14,"   ",I4," ",2A8," ")'
        endfor
        print,"---------------- ---- --------"
    endif

hdf_sd_end, sdsfileid

;================================
  file_handle = hdf_open(filen,/read)

    vdata_ID = hdf_vd_getid(  file_handle, -1 )
    is_NOT_fakeDim = 1
    num_vdata = 0


    while (vdata_ID ne -1) and (is_NOT_fakeDim) do begin
        vdata_H = hdf_vd_attach(file_handle,vdata_ID)
        hdf_vd_get, vdata_H, name=name,size= size, nfields = nfields

        hdf_vd_detach, vdata_H

               is_NOT_fakeDim = strpos(name,'fakeDim') eq -1

; Build up the list of Vdata names,sizes and number of fields 
        if (num_vdata eq 0) then begin
            Vdata_name = name 
            Vdata_size = size
            Vdata_nfields = nfields
            num_vdata = 1
        endif else if is_NOT_fakeDim then begin
            Vdata_name = [Vdata_name,name]
            Vdata_size = [Vdata_size,size]
            Vdata_nfields = [Vdata_nfields,nfields]
            num_vdata = num_vdata + 1
        endif

       vdata_ID = hdf_vd_getid( file_handle, vdata_ID )

    endwhile 


print,'ok'

;Print out the list of names   
    print,''   
    print, 'List of Vdata names    Size (bytes)   Num. Fields'
    print, '-------------------------------------------------'
    for i = 0,num_vdata-1  do begin
        print, Vdata_name(i),Vdata_size(i),Vdata_nfields(i),$
               format='(A18,I10,I14)'
    endfor
    print, '-------------------------------------------------'

    hdf_close,file_handle   
end


