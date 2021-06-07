function get_dd3m, file0, var, sav_type , $
      mapping = mapping,lon3 = lon3, lat3=lat3,lev3=lev3,time3=time3, dd3m_att = dd3m_att,$
      level = level

 if(not keyword_Set(mapping))then mapping = 0
 if(not keyword_Set(level))then level = 'lev'
        
        if(strpos(file0,'sav2') ge 0)then var0 = var else $
           if(not keyword_set(mapping))then mapping = 'cesm2cmip'
           var0 = cmip6_2cesm_var(var) ; cmip var

        if(strpos(file0,'ERA5') ge 0)then var0 = var 
         
        
           ffile    = file0 + var0
;print,sav_type
;print,ffile
         if(sav_type) then begin
            filein = ffile + '.sav'


;print,filein
;;stop
            if(not file_exist(filein)) then return, -999999.0
            restore, filein

         endif else begin
            if(strpos(file0,'.nc') gt 0)then filein = file0 else $
            filein = ffile + '.nc'
            if(not file_exist(filein)) then  begin
               print,' File not found =========== in get_dd3m.pro =============> ', filein
               return, -999999.0
            endif

            dd3m = get_fld(filein, var0)
            dd3m_att = get_cdf_att_all(filein,var0)

            lat3 = get_fld(filein, 'lat')
            lon3 = get_fld(filein, 'lon')
            ncdf_Vars, filein, vars_tmp, no_print =1

            lev3 = [1000.]
            if(belongsto(level,vars_tmp))then begin
                lev3 = get_fld(filein,level)

             endif else begin
                 if(belongsto('plev',vars_tmp))then lev3 = get_fld(filein,'plev')
              endelse
            ; taking care the NCAR CMIP results
              if(max(lev3) le 0.)then lev3 = abs(lev3)
              if(max(lev3) lt 2.)then lev3 = lev3*1000.
              if(max(lev3) gt 2000)then lev3 = lev3/100.

             
            time3 = [0.0]
            if(belongsto('time',vars_tmp))then begin
                time3 = get_fld(filein,'time')
            endif
         endelse


         return, dd3m

end

