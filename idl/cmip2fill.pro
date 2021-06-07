; ======================================
 pro cmip2fill,  old_sav_path,  new_sav_path
;========================================
; use .sav files in folder to fill 3d fields
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

file0 = old_sav_path

 sav_type = 0
  if(strpos(file0,'/sav/') ge 0)then sav_type = 1

  if(sav_type)then begin
    restore, file0 + 'ta.sav'      ; assuming these exist
    ; -----------------------
  endif else begin  ; all defaults are defined for later save purpose
    filein = file0 + 'ta.nc'       ; assuming exists
    ; -----------------------
    dd3m = get_fld(filein, 'ta')
    var3 = 'dummy'
    modelid3 = 'dummy'
    mipid3 ='dummy'
    expid3 = 'dummy'
    dataType3 = 'dummy'
    lat3 = get_fld(filein, 'lat')
    lon3 = get_fld(filein, 'lon')
    ncdf_Vars, filein, vars_tmp, no_print=1
     if(belongsto('lev',vars_tmp))then begin
        lev3 = get_fld(filein,'lev')
      endif else begin
        lev3 = get_fld(filein,'plev')
      endelse
    time3 = 1.0
    year3 = 1.0
    month3 = 1.0
    day3 =1.0
    nc_files3= 'dummy'
    climo3 =1
    dd3m_att= create_struct('dummy', 'dummy')
  endelse
  ; ================================================

;     restore, file + cmip6_2cesm_var('T')+'.sav'
   var3 = 'T'
   dd3m = get_dd3m(file0, var3, sav_type)
   T = dd3m

;     restore, file + cmip6_2cesm_var('Q')+'.sav'
   var3 = 'Q'
   dd3m = get_dd3m(file0, var3, sav_type)
   Q = dd3m

;     restore, file + cmip6_2cesm_var('U')+'.sav'
   var3 = 'U'
   dd3m = get_dd3m(file0, var3, sav_type)
   U = dd3m

;     restore, file + cmip6_2cesm_var('V')+'.sav'
   var3 = 'V'
   dd3m = get_dd3m(file0, var3, sav_type)
   V = dd3m

;     restore, file + cmip6_2cesm_var('Z3')+'.sav'
   var3 = 'Z3'
   dd3m = get_dd3m(file0, var3, sav_type)
   Z3 = dd3m

;     restore, file + cmip6_2cesm_var('OMEGA')+'.sav'
   var3 = 'OMEGA'
   dd3m = get_dd3m(file0, var3, sav_type)
   OMEGA = dd3m

  lev = lev3
   
    fid_relhum= 0 
    RELHUM = T*0.0
    fid = file + cmip6_2cesm_var('RELHUM')+'.*'
    if(file_exist(fid)) then begin  
      fid_relhum = 1 
;     restore, file + cmip6_2cesm_var('RELHUM')+'.sav'
   var3 = 'RELHUM'
   dd3m = get_dd3m(file0, var3, sav_type)
     RELHUM  = dd3m
     jj = where(RELHUM le 1.0e-6,cnt)
     if(cnt gt 0)then RELHUM[jj] = 1.0e-6
     endif

;     restore, file + cmip6_2cesm_var('TS')+'.sav'
   var3 = 'TS'
   dd3m = get_dd3m(file0, var3, sav_type)
   TS = dd3m
;     restore, file + cmip6_2cesm_var('QREFHT')+'.sav'
   var3 = 'QREFHT'
   dd3m = get_dd3m(file0, var3, sav_type)
   QREFHT = dd3m
;;     restore, file + cmip6_2cesm_var('U10')+'.sav'
;;   U10 = dd3m
;;     restore, file + cmip6_2cesm_var('V10')+'.sav'
;;   V10 = dd3m

;     restore, file + cmip6_2cesm_var('RELHUMS')+'.sav'
   var3 = 'RELHUMS'
   dd3m = get_dd3m(file0, var3, sav_type, lon3=lon3, lat3 = lat3)
   RELHUMS  = dd3m

;     restore, file + cmip6_2cesm_var('ORO')+'.sav'
 fid = file + cmip6_2cesm_var('ORO')+'.*'
 if(file_exist(fid)) then begin  
   var3 = 'ORO'
   dd3m = get_dd3m(file0, var3, sav_type , lon3 = lon2, lat3=lat2,lev3=lev2)
   ORO = my_interpol2(dd3m, lon2,lat2,lon3,lat3, tdim=0)
 endif else begin
   ORO = TS*0.0 - 999999.
 endelse


;     restore, file + cmip6_2cesm_var('PS')+'.sav'
   var3 = 'PS'
   dd3m = get_dd3m(file0, var3, sav_type)
   PS  = dd3m

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
;     new_file = file + cmip6_2cesm_var(vars[iv])+'.sav'
;     old_file = file + cmip6_2cesm_var(vars[iv])+'_ori.sav'
;     if(file_exist(new_file))then $
;     spawn, 'cp '+ new_file +' ' + old_file
;    endfor

; created new file 3d var names
    
; to recover the lat3, lon3 info

;    restore, file + cmip6_2cesm_var('T')+'.sav'
    dd3m = T
    var3  = 'T'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
;  stop
    dd3m = H
    var3  = 'H'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = THETA
    var3  = 'THETA'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att

   
    dd3m = Q
    var3  = 'Q'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = U
    var3  = 'U'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = V
    var3  = 'V'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = OMEGA
    var3  = 'OMEGA'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
;    restore, file + cmip6_2cesm_var('T')+'.sav'
    dd3m = RELHUM
    var3  = 'RELHUM'
    filesave = file_new + var3+'.sav'
     if(fid_relhum)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
;    restore, file + cmip6_2cesm_var('T')+'.sav'
    dd3m = Z3
    var3  = 'Z3'
    filesave = file_new + var3+'.sav'
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   

; cld fields from CESM2 has its own vertical resolutions, so put at the end to use its own coordinates lev
;=======================================================

    fid_cldliq = 0 
    CLDLIQ = T*0.0
    fid = file + cmip6_2cesm_var('CLDLIQ')+'.*'
    if(file_exist(fid)) then begin  
      fid_cldliq = 1 
;       restore, file + cmip6_2cesm_var('CLDLIQ')+'.sav'
   var3 = 'CLDLIQ'
   dd3m = get_dd3m(file0, var3, sav_type, lon3=lon3,lat3=lat3,lev3=lev3,dd3m_att = dd3m_att)
       CLDLIQ = dd3m
     endif
 
    fid_cldice = 0 
    CLDICE = T*0.0
    fid = file + cmip6_2cesm_var('CLDICE')+'.*'
    if(file_exist(fid)) then begin  
      fid_cldice = 1 
;     restore, file + cmip6_2cesm_var('CLDICE')+'.sav'
   var3 = 'CLDICE'
   dd3m = get_dd3m(file0, var3, sav_type)
      CLDICE  = dd3m
     endif

    fid_cloud = 0 
    CLOUD= T*0.0
    fid = file + cmip6_2cesm_var('CLOUD')+'.*'
    if(file_exist(fid)) then begin  
      fid_cloud = 1 
;     restore, file + cmip6_2cesm_var('CLOUD')+'.sav'
   var3 = 'CLOUD'
   dd3m = get_dd3m(file0, var3, sav_type, lon3 = lon3, lat3=lat3,lev3=lev3, dd3m_att = dd3m_att)
      CLOUD  = dd3m

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
    filesave = file_new + var3+'.sav'
     if(fid_cloud)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = CLDLIQ
    var3  = 'CLDLIQ'
    filesave = file_new + var3+'.sav'
     if(fid_cldliq)then $
    save,file = filesave,var3,dd3m,modelid3,mipid3,expid3,dataType3,lat3,lon3,lev3,time3,year3,month3,day3, $
                  nc_files3, climo3,dd3m_att
   
    dd3m = CLDICE
    var3  = 'CLDICE'
    filesave = file_new + var3+'.sav'
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
   
