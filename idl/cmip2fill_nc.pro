
!!! unfinished!
; ======================================
 pro cmip2fill_nc,  old_sav_path,  new_sav_path
;========================================
; use .nc files in folder to fill 3d fields
; filling data below surface pressure and save data for the following fields

   vars3 = ['T','Q','U','V', 'RELHUM']      ; use surface

   vars30 = ['CLOUD','OMEGA','CLDLIQ','CLDICE'] ;set to zero

   vars = [vars3, vars30]

   large_number = 999998. ;1.0e12

  file     = old_sav_path + '/'
  file_new = new_sav_path + '/'

  print,''
  print,'filling 3d missing data by using surface fields ....'
  print,''

  
   var = 'T' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   T = get_fld (ff_in, var)
   T_att = get_cdf_att_all(ff_in,var)

   var = 'Q' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   Q = get_fld (ff_in, var)
   Q_att = get_cdf_att_all(ff_in,var)

   var = 'U' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   U = get_fld (ff_in, var)
   U_att = get_cdf_att_all(ff_in,var)

   var = 'V' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   V = get_fld (ff_in, var)
   V_att = get_cdf_att_all(ff_in,var)

   var = 'Z3' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   Z3 = get_fld (ff_in, var)
   Z3_att = get_cdf_att_all(ff_in,var)

   var = 'OMEGA' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   OMEGA = get_fld (ff_in, var)
   Omega_att = get_cdf_att_all(ff_in,var)

   lon = get_fld(ff_in, 'lon') 
   lat = get_fld(ff_in, 'lat') 
   lev = get_fld(ff_in, 'lev') 

;;     restore, file + cmip6_2cesm_var('T')+'.nc'
;;   T = dd3m
;;     restore, file + cmip6_2cesm_var('Q')+'.nc'
;;   Q = dd3m
;;     restore, file + cmip6_2cesm_var('U')+'.nc'
;;   U = dd3m
;;     restore, file + cmip6_2cesm_var('V')+'.nc'
;;   V = dd3m
;;     restore, file + cmip6_2cesm_var('Z3')+'.nc'
;;   Z3 = dd3m
;;     restore, file + cmip6_2cesm_var('OMEGA')+'.nc'
;;   OMEGA = dd3m

;;  lev = lev3
   
    fid_relhum= 0 
    RELHUM = T*0.0
    fid = file + cmip6_2cesm_var('RELHUM')+'.nc'
    if(file_exist(fid)) then begin  
      fid_relhum = 1 

        var        = 'RELHUM' 
        RELHUM     = get_fld (fid, var)
        RELHUM_att = get_cdf_att_all(fid,var)

     ;; restore, file + cmip6_2cesm_var('RELHUM')+'.nc'
     ;; RELHUM  = dd3m
     
     jj = where(RELHUM le 1.0e-6,cnt)
     if(cnt gt 0)then RELHUM[jj] = 1.0e-6
     endif

   var        = 'TS' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   TS     = get_fld (ff_in, var)
   TS_att = get_cdf_att_all(ff_in,var)

   var        = 'RELHUMS' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   RELHUMS     = get_fld (ff_in, var)
   RELHUMS_att = get_cdf_att_all(ff_in,var)

   var        = 'QREFHT' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   QREFHT     = get_fld (ff_in, var)
   QREFHT_att = get_cdf_att_all(ff_in,var)

   var        = 'ORO' & ff_in = file + cmip6_2cesm_var(var)+'.nc'
   ORO     = get_fld (ff_in, var)
   ORO_att = get_cdf_att_all(ff_in,var)


;;     restore, file + cmip6_2cesm_var('TS')+'.nc'
;;   TS = dd3m
;;     restore, file + cmip6_2cesm_var('QREFHT')+'.nc'
;;   QREFHT = dd3m
;;;     restore, file + cmip6_2cesm_var('U10')+'.nc'
;;;   U10 = dd3m
;;;     restore, file + cmip6_2cesm_var('V10')+'.nc'
;;;   V10 = dd3m

;;     restore, file + cmip6_2cesm_var('RELHUMS')+'.nc'
;;   RELHUMS  = dd3m

;;     restore, file + cmip6_2cesm_var('ORO')+'.nc'
;;   ORO = dd3m
;;     restore, file + cmip6_2cesm_var('PS')+'.nc'
;;   PS  = dd3m

   nx = n_elements(T[*,0,0,0])
   ny = n_elements(T[0,*,0,0])
   np = n_elements(T[0,0,*,0])
   nt = n_elements(T[0,0,0,*])
  
  for k = 0,np-1 do begin
        dw = reform(T[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  TS[jj]
           T[*,*,k,*] = dw
         endif

        dw = reform(Q[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  QREFHT[jj]
           Q[*,*,k,*] = dw
         endif

        dw = reform(U[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  dw[jj]*0.0 ;U10[jj]
           U[*,*,k,*] = dw
         endif

        dw = reform(V[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  dw[jj]*0.0 ;V10[jj]
           V[*,*,k,*] = dw
         endif

        dw = reform(OMEGA[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  dw[jj]*0.0
           OMEGA[*,*,k,*] = dw
         endif

        dw = reform(Z3[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  ORO[jj]
           Z3[*,*,k,*] = dw
         endif
  endfor ;k

  if(fid_relhum)then begin
  for k = 0,np-1 do begin
        dw = reform(RELHUM[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  RELHUMS[jj]
           RELHUM[*,*,k,*] = dw
         endif
  endfor ;k
  endif


   H = T + 9.8/1004.*Z3 + 2.5e6/1004.*Q
   PP = T * 0

   rp = lev
   for k = 0, np-1 do begin
     pp[*,*,k,*] = rp[k]
   endfor
   THETA = T*(100000./pp)^0.287

;   nv = n_elements(vars)-1
;   for iv = 0 ,nv do begin
;     new_file = file + cmip6_2cesm_var(vars[iv])+'.nc'
;     old_file = file + cmip6_2cesm_var(vars[iv])+'_ori.nc'
;     if(file_exist(new_file))then $
;     spawn, 'cp '+ new_file +' ' + old_file
;    endfor

; created new file 3d var names
    
; to recover the lat3, lon3 info

    restore, file + cmip6_2cesm_var('T')+'.nc'
    dd3m = T
    var3  = 'T'
    filesave = file_new + var3+'.nc'
    save_nc, file, T, T_att
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
;  stop
    dd3m = H
    var3  = 'H'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = THETA
    var3  = 'THETA'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

   
    dd3m = Q
    var3  = 'Q'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = U
    var3  = 'U'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = V
    var3  = 'V'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = OMEGA
    var3  = 'OMEGA'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    restore, file + cmip6_2cesm_var('T')+'.nc'
    dd3m = RELHUM
    var3  = 'RELHUM'
    filesave = file_new + var3+'.nc'
     if(fid_relhum)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    restore, file + cmip6_2cesm_var('T')+'.nc'
    dd3m = Z3
    var3  = 'Z3'
    filesave = file_new + var3+'.nc'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   

; cld fields from CESM2 has its own vertical resolutions, so put at the end to use its own coordinates lev
;=======================================================

    fid_cldliq = 0 
    CLDLIQ = T*0.0
    fid = file + cmip6_2cesm_var('CLDLIQ')+'.nc'
    if(file_exist(fid)) then begin  
      fid_cldliq = 1 

        var        = 'CLDLIQ'
        CLDLIQ     = get_fld (fid, var)
        CLDLIQ_att = get_cdf_att_all(fid,var)

       ;; restore, file + cmip6_2cesm_var('CLDLIQ')+'.nc'
       ;; CLDLIQ = dd3m
     endif
 
    fid_cldice = 0 
    CLDICE = T*0.0
    fid = file + cmip6_2cesm_var('CLDICE')+'.nc'
    if(file_exist(fid)) then begin  
      fid_cldice = 1 

        var        = 'CLDICE'
        CLDICE     = get_fld (fid, var)
        CLDICE_att = get_cdf_att_all(fid,var)

     ;; restore, file + cmip6_2cesm_var('CLDICE')+'.nc'
      ;; CLDICE  = dd3m
     endif

    fid_cloud = 0 
    CLOUD= T*0.0
    fid = file + cmip6_2cesm_var('CLOUD')+'.nc'
    if(file_exist(fid)) then begin  
      fid_cloud = 1 

        var        = 'CLOUD'
        CLOUD     = get_fld (fid, var)
        CLOUD_att = get_cdf_att_all(fid,var)

     ;; restore, file + cmip6_2cesm_var('CLOUD')+'.nc'
     ;;  CLOUD  = dd3m
     endif

   nx = n_elements(CLOUD[*,0,0,0])
   ny = n_elements(CLOUD[0,*,0,0])
   np = n_elements(CLOUD[0,0,*,0])
   nt = n_elements(CLOUD[0,0,0,*])

  if(fid_cloud)then begin
  for k = 0,np-1 do begin
        dw = reform(CLOUD[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  dw[jj]*0.0
           CLOUD[*,*,k,*] = dw
         endif
  endfor ;k
  endif

  if(fid_cldliq)then begin
  for k = 0,np-1 do begin
        dw = reform(CLDLIQ[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  dw[jj]*0.0
           CLDLIQ[*,*,k,*] = dw
         endif
  endfor ;k
  endif

  if(fid_cldice)then begin
  for k = 0,np-1 do begin
        dw = reform(CLDICE[*,*,k,*])
        jj = where(dw gt large_number or (dw lt -large_number), cnt)
         if(cnt gt 0)then begin
           dw[jj] =  dw[jj]*0.0
           CLDICE[*,*,k,*] = dw
         endif

   endfor
   endif

    dd3m = CLOUD
    var3  = 'CLOUD'
    filesave = file_new + var3+'.nc'
     if(fid_cloud)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = CLDLIQ
    var3  = 'CLDLIQ'
    filesave = file_new + var3+'.nc'
     if(fid_cldliq)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = CLDICE
    var3  = 'CLDICE'
    filesave = file_new + var3+'.nc'
     if(fid_cldice)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
;stop

print,''
print,'  used files in ', old_sav_path, ' to create files in ', new_sav_path
print,''
spawn, 'ls -ltr ' + new_sav_path + '/*', list
print,list

return
end
   
