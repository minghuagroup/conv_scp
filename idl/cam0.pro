; do 3d plots specifications
; Need to specify: nf, iave,idata


NF=2  ;!!!!!!!!!!!! 0 !!jan july or 0728 0729 

igif= 0
iix = 0
;=====
months=['Jan','July']

; idata=1 ;C3M      ;!!!!!!! c3m.pro
; idata=2 ;ERA     ;        era3d.pro
; idata=3 ; CAM_3D ;       cam3d.pro
; idata= 4 ;CAM_P        ;        cam3d_p.pro physics 
; idate =42 ;CAM_2D
; idata= 5  ;CAM_i        ;        cam3d_i.pro initial condition 
; idata= 6 ;IAP_i         ;        iap3d_i.pro initial condition 
; idata=  7 ;FNL         ;        fnl.pro  
; idata= 13  ;CASE        ;cam3d.pro  for CAM case study of 20170727
; idata = 14 ;CASE;      ;cam3d_P for case study of 20170727
; idata = 15 ;CASE;      ;cam2d for case study of 20170727
; idata= 23  ;CASE hourly     ;*cam3d_hourly.pro  for CAM case study of 20170727
; idata = 24 ;CASE; hourly      ;*cam3d_hourly_P for case study of 20170727
idata = 25 ;CASE; hourly     ;cam2d_hourly for case study of 20170727
idata = 26 ; case scp40 cam2d_hourly for case study of 20170727

;L30
 idata=  53  ;CASE hourly     ;iap3d_hourly.pro  for CAM case study of 20170727
 idata = 54 ;CASE; hourly    ;iap3d_hourly_P for case study of 20170727
 idata = 55 ;CASE; hourly    ;iap2d_hourly for case study of 20170727

; 2D fields readin independently in program
;  cam2d  c3m2d era2d obs2d can be run independently without setting idata file in 00_range
;============
; idata=2 ;ERA     ;        era3d.pro

 iave=1 ;zonal average ;!!!!!!!!!!!
; iave=2 ;meridional average
; iave=3 ;vertical average
; iave=4 ;GPCI
; iave=5 ;950mb
; iave=6 ;300mb
; iave=7 ;700mb

 ctypes =['Zonal','Meridional','Vertical','GPCI','950mb','300mb','700mb']

;============================================
idata = 1
iave =  3

iave2=iave

case iave of
 1:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Zonal_mean'
   end 

 2:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Meridional_mean'
   end 

 3:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,100]
    ctype = 'Vertical_mean'
   end 

 4:begin
    lon_range = 360-[173.,119]
    lat_range = [0.,40]
    z_range   = [1000.,0]
    lat_width=2.  ; for iave=4 only
    lon_width=2.
    ctype = 'GPCI'
   end 

 5:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [950.,950]
    ctype = '950mb'
    iave=3   ; vertically level
   end 

 6:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [300.,300]
    ctype = '300mb'
    iave=3   ; vertically level
   end 

 7:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [700.,700]
    ctype = '700mb'
    iave=3   ; vertically level
   end 


 else:
 endcase

;==================



 case idata of
 1: begin
   datatype2 = 'C3M' 
	;C3M
	filecs=['c3m200801.nc','c3m200807.nc']
	filems=['myctl_test.cam.h0.2008-01.nc','myctl_test.cam.h0.2008-07.nc']
   filein2  = '../obs/'+filecs ;[nf]

   iz = 1  ; in height coordinate 120 m 137lev
   iz = 0  ; in pressure coordinate 101 lev

   if(iz)then ytitle='Altitude (km)'

   end 

 2: begin
   datatype2 = 'ERAI' 
	;ERAI ============
	fileeras1 = ['ei.moda.an.ml.regn128sc.2008010100.nc',$
             'ei.moda.an.ml.regn128sc.2008070100.nc']
	fileeras2 = ['ei.moda.an.ml.regn128uv.2008010100.nc',$
             'ei.moda.an.ml.regn128uv.2008070100.nc']
	fileeras_sav=['data/era_01.sav','data/era_07.sav']

	fileeras1 = 'ERAI/'+fileeras1
	fileeras2 = 'ERAI/'+fileeras2
   filein2  = '../obs/'+fileeras1[nf]
   filein2b  = '../obs/'+fileeras2[nf]
   filesave  = fileeras_sav[nf]
   end 

 3: begin
   datatype2 = 'CAM_3D' 
	;Model======  all data in ../obs
	filems=['myctl_test.cam.h0.2008-01.nc', $
                'myctl_test.cam.h0.2008-07.nc']
	filecam_sav=['data/cam2008_01.sav','data/cam2008_07.sav']
   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 


 4: begin
   datatype2 = 'Model_P' 
	filems=['myctl_test.cam.h0.2008-01.nc', $
                'myctl_test.cam.h0.2008-07.nc']
	filecamp_sav=['data/cam2008_01_P.sav','data/cam2008_07_P.sav']
   filesave  = filecamp_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 

 42: begin
   datatype2 = 'CAM_2D_' 
	filems=['myctl_test.cam.h0.2008-01.nc', $
                'myctl_test.cam.h0.2008-07.nc']
	filecam_sav=['data/cam2008_01.sav','data/cam2008_07.sav']
   filesave  = filecamp_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 

 5: begin
   datatype2 = 'CAM_i' 
	filems=[ 'CAM5_fnl_20170727_00_00_L30.f19.nc', $
                'CAM5_fnl_20110520_00_00_L30.f19.nc']
   filein2  = '../obs/INIT/'+filems[nf]
   end 

 6: begin
   datatype2 = 'IAP_i' 
	filems=['IAP5_fnl_128x256_20170727_00_00_L30.nc',$
                'IAP5_fnl_128x256_20110520_00_00_L30.nc']
   filein2  = '../obs/CAM/'+filems[nf]
   end 

 7: begin
   datatype2 = 'FNL3d' 
	filems=['fnl_20170727_00_00.nc' $
              ,'fnl_20170727_00_00.nc']
   filein2  = '../obs/FNL/'+filems[nf]
   end 


 13: begin
   datatype2 = 'CAM_2017_JUL'
    months=['0728','0729']
        filems=['my20170727_ctl.cam.h0.2017-07-28-00000.nc',$
                'my20170727_ctl.cam.h0.2017-07-29-00000.nc']
	filecam_sav=['data/cam_0728.sav','data/cam_0729.sav']
   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 


 14: begin
   datatype2 = 'CAM_P_2017_JUL'
    months=['0728','0729']
        filems=['my20170727_ctl.cam.h0.2017-07-28-00000.nc',$
                'my20170727_ctl.cam.h0.2017-07-29-00000.nc']
	filecam_sav=['data/cam_p_0728.sav','data/cam_p_0729.sav']

   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 

 15: begin
   datatype2 = 'CAM_2D_2017_JUL'
    months=['0728','0729']
        filems=['my20170727_ctl.cam.h0.2017-07-28-00000.nc',$
                'my20170727_ctl.cam.h0.2017-07-29-00000.nc']
	filecam_sav=['data/cam_p_0728.sav','data/cam_p_0729.sav']

   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
    ctype=''
   end 

 23: begin
   datatype2 = 'CAM_hourly_2017_JUL'
    months=['0728','0729']
        filems=['my20170727_ctl.cam.h1.2017-07-28-00000.nc',$
                'my20170727_ctl.cam.h1.2017-07-29-00000.nc']
	filecam_sav=['data/cam_hourly_0728.sav','data/cam_hourly_0729.sav']
   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 


 24: begin
   datatype2 = 'CAM_hourly_P_2017_JUL'
    months=['0728','0729']
        filems=['my20170727_ctl.cam.h1.2017-07-28-00000.nc',$
                'my20170727_ctl.cam.h1.2017-07-29-00000.nc']
	filecam_sav=['data/cam_hourly_p_0728.sav','data/cam_hourly_p_0729.sav']

   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
   end 
 25: begin
   datatype2 = 'CAM_hourly_2D_2017_JUL'
    months=['0728','0729']
        filems=['my20170727_ctl.cam.h1.2017-07-28-00000.nc',$
                'my20170727_ctl.cam.h1.2017-07-29-00000.nc']
	filecam_sav=['data/cam_hourly_p_0728.sav','data/cam_hourly_p_0729.sav']
   filesave  = filecam_sav[nf]
   filein2  = '../obs/CAM/'+filems[nf]
    ctype=''
   end 
 26: begin

   icase = 'scp20' ;ZM
   icase = 'scp40' ; scp original github with fixed basemassflx 08-06-01
   icase = 'scp41' ; scp original github with closure github
   icase = 'scp42' ; scp original github with closure github wint changed to more shallower

   datatype2 = icase+'_hourly_2D_2017_JUL'
    months=['0727','0728']
        filems=icase+'_'+['gcmrun.cam.h1.2017-07-27-00000.nc', $
                'gcmrun.cam.h1.2017-07-27-00000.nc'] 
	filecam_sav='data/'+icase+'_hourly_p_'+['0727.sav','0727.sav']
   filesave  = filecam_sav[nf]
   filein2  = 'GCM/'+filems[nf]
    ctype=''
   end 


 53: begin ;iap3d
   icase ='IAP'
   datatype2 = icase+'_mean_2D_2017_JUL'
    months=['L30','L35']
        filems=['fc5f.cam2.h0.1979-08-06-00000.nc', $
                'fc35f.cam2.h0.1979-08-06-00000.nc']
	filecam_sav=['data/iap_10day_L30.sav','data/iap_10day_L35.sav']
   filesave  = filecam_sav
   filein2  = '../obs/IAP/'+filems
   ctype=''
   end 

 55: begin
   icase ='IAP'
   months=['L30','L35']
   
   datatype2 = icase+'_hourly_2D_2017_JUL'
        filems=['fc5f.cam2.h1.1979-07-27-00000.nc', $
                'fc35f.cam2.h1.1979-07-27-00000.nc']
	filecam_sav=['data/iap_hourly_0727_L30.sav','data/iap_hourly_0727_L35.sav']

   filesave  = filecam_sav
   filein2  = '../obs/IAP/'+filems
   ctype=''
   end 

 else:
 endcase

 fileins = filems
; print,filein2
 

 lev_ACTREL     = cal_lev([0.,20],20)
 lev_ACTREI     = cal_lev([0.,160],20)
 lev_T     = cal_lev([200.,300],20)
 lev_Q     = cal_lev([0.,20],20)
 lev_U     = cal_lev([-30.,30],20)
 lev_V     = cal_lev([-20.,20],20)/2.
 lev_RH    = cal_lev([0.,100],20)/100.
 lev_W     = cal_lev([-20.,20],20)/5.
 lev_DQ   = cal_lev([-10.,10],20)/2   ;g/kg/day
 lev_DT   = cal_lev([-5.,5],20)   ;K/day
 lev_cldliq = cal_lev([0,3],20)
 lev_NUMW   = cal_lev([1.,1000],20)*5   ;/g
 lev_NUMi   = cal_lev([1.,100],20)   ;/g
 lev_WSUB   = cal_lev([-1,1],20)   ;m/s

 lev_cape= cal_lev([0.,2000],20)
 lev_cin = cal_lev([0.,1000],20)
 lev_cld   = cal_lev([0.,1],20)
 lev_crf   = cal_lev([-400,200],20)
 lev_flnsc = cal_lev([0,200 ],20)
 lev_flnt = cal_lev([0,300 ],20)
 lev_fsds  = cal_lev([50,450 ],20)
 lev_fsnt  = cal_lev([0,500 ],20)
 lev_lhflx = cal_lev([0,300 ],20)
 lev_lwcf  = cal_lev([0.,100],20)
 lev_pblh  = cal_lev([100,1500],20)
; lev_prect = cal_lev([0.,15],20)
 lev_prect = cal_lev([0.,30],20)  ;tempo

 lev_precc = cal_lev([0.,15],20)
 lev_precl = cal_lev([0.,15],20)
 lev_precsl= cal_lev([0.,15],20)
 lev_ps    = cal_lev([970.,1050.],20)
 lev_qrefht= cal_lev([0.,20.],20)
 lev_shflx = cal_lev([0,120 ],20)
 lev_swcf  = cal_lev([-300.,0],20)
 lev_taux  = cal_lev([-0.3,0.3 ],20)
 lev_tgcldlwp = cal_lev([0,30 ],20)  
 lev_tgcldlwp = cal_lev([0,50 ],20)   ;!! temp
 lev_tmq   = cal_lev([0.,60],20)
 lev_ts    = cal_lev([0,30],20)
 lev_U10 = cal_lev([0,15 ],20)
 lev_V10 = cal_lev([-15,15 ],20)
 lev_vrate = cal_lev([0.,100],20)  ;1/1000 FNL
 lev_WGUSTD = cal_lev([0,1],20)    ;1/10 FNL

 lev_pres = cal_lev([100,1000],18)
 lev_k = cal_lev([0,30],20)
 lev_tke_cu= cal_lev([0.,2],20)
 lev_cld_cu= cal_lev([0,0.2],20)

; lev_= cal_lev([],20)

 xrange=lon_range
 yrange=lat_range

month = months
;==============
 datatype2=datatype2+'_'+month+'_'
 datatype=datatype2+ctype+'_'  ; source, month, cross sec type
 print,'datatype:',datatype
;==============
ix=0
end
