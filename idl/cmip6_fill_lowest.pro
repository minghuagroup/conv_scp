; ======================================
 pro cmip6_fill_lowest,filepath
;========================================
; use .sav files in folder to fill 3d fields
; filling data below surface pressure and save data for the following fields

   vars3 = ['T','Q','U','V', 'RELHUM']      ; use surface

   vars30 = ['CLOUD','OMEGA','CLDLIQ','CLDICE'] ;set to zero

   vars = [vars3, vars30]

   large_number = 999998. ;1.0e12

   ;varss = ['TS','QREFHT','U10','V10','RELHUMS']
   
  print,''
  print,'filling 3d missing data by using surface fields ....'
  print,''

  file = filepath + '/'
  
     restore, file + cmip6_2cesm_var('T')+'.sav'
   T = dd2m
     restore, file + cmip6_2cesm_var('Q')+'.sav'
   Q = dd2m
     restore, file + cmip6_2cesm_var('U')+'.sav'
   U = dd2m
     restore, file + cmip6_2cesm_var('V')+'.sav'
   V = dd2m
     restore, file + cmip6_2cesm_var('Z3')+'.sav'
   Z3 = dd2m
     restore, file + cmip6_2cesm_var('OMEGA')+'.sav'
   OMEGA = dd2m
   
    fid_cldliq = 0 
    CLDLIQ = T*0.0
    fid = file + cmip6_2cesm_var('CLDLIQ')+'.sav'
    if(file_exist(fid)) then begin  
      fid_cldliq = 1 
       restore, file + cmip6_2cesm_var('CLDLIQ')+'.sav'
       CLDLIQ = dd2m
     endif
 
    fid_cldice = 0 
    CLDICE = T*0.0
    fid = file + cmip6_2cesm_var('CLDICE')+'.sav'
    if(file_exist(fid)) then begin  
      fid_cldice = 1 
     restore, file + cmip6_2cesm_var('CLDICE')+'.sav'
      CLDICE  = dd2m
     endif

    fid_cloud = 0 
    CLOUD= T*0.0
    fid = file + cmip6_2cesm_var('CLOUD')+'.sav'
    if(file_exist(fid)) then begin  
      fid_cloud = 1 
     restore, file + cmip6_2cesm_var('CLOUD')+'.sav'
      CLOUD  = dd2m
     endif

    fid_relhum= 0 
    RELHUM = T*0.0
    fid = file + cmip6_2cesm_var('CLDLIQ')+'.sav'
    if(file_exist(fid)) then begin  
      fid_relhum = 1 
     restore, file + cmip6_2cesm_var('RELHUM')+'.sav'
     RELHUM  = dd2m
     jj = where(RELHUM le 1.0e-6,cnt)
     if(cnt gt 0)then RELHUM[jj] = 1.0e-6
     endif

     restore, file + cmip6_2cesm_var('TS')+'.sav'
   TS = dd2m
     restore, file + cmip6_2cesm_var('QREFHT')+'.sav'
   QREFHT = dd2m
;     restore, file + cmip6_2cesm_var('U10')+'.sav'
;   U10 = dd2m
;     restore, file + cmip6_2cesm_var('V10')+'.sav'
;   V10 = dd2m

     restore, file + cmip6_2cesm_var('RELHUMS')+'.sav'
   RELHUMS  = dd2m

     restore, file + cmip6_2cesm_var('ORO')+'.sav'
   ORO = dd2m
     restore, file + cmip6_2cesm_var('PS')+'.sav'
   PS  = dd2m

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

   T = reverse(T,3)
   Q = reverse(Q,3)
   U = reverse(U,3)
   V = reverse(V,3)
   OMEGA = reverse(OMEGA,3)
   RELHUM = reverse(RELHUM,3)
   CLDLIQ = reverse(CLDLIQ,3)
   CLDICE = reverse(CLDICE,3)
   CLOUD = reverse(CLOUD,3)
   Z3     = reverse(Z3 ,3)
   H = T + 9.8/1004.*Z3 + 2.5e6/1004.*Q
   PP = T * 0

   rp = reverse(plev)
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

; created new file 3di var names
    
    dd2m = T
    var  = 'T'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
;  stop
   
    dd2m = Q
    var  = 'Q'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = U
    var  = 'U'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = V
    var  = 'V'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = OMEGA
    var  = 'OMEGA'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = RELHUM
    var  = 'RELHUM'
    new_file = file + var+'.sav'
     if(fid_relhum)then $
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = CLOUD
    var  = 'CLOUD'
    new_file = file + var+'.sav'
     if(fid_cloud)then $
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = CLDLIQ
    var  = 'CLDLIQ'
    new_file = file + var+'.sav'
     if(fid_cldliq)then $
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = CLDICE
    var  = 'CLDICE'
    new_file = file + var+'.sav'
     if(fid_cldice)then $
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = Z3
    var  = 'Z3'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = H
    var  = 'H'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files
   
    dd2m = THETA
    var  = 'THETA'
    new_file = file + var+'.sav'
    save,file = new_file,var,dd2m,model,mipid,expid,dataType,lat,lon,plev,yearrange,files

return
end
   
