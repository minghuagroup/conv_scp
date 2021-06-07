
 ;run iweb to put on web

; strid = datatype2+'_'+month
; gifgroup = file_search('gif_3d/'+strid+'*.gif')

 ;customerized !!
 strid = 'obs_jan'
 gifgroup1 = file_search('gif_3d/ERA*Jan*.gif')
 gifgroup2 = file_search('gif_3d/C3M*Jan*.gif')
 gifgroups = [gifgroup2,gifgroup1]

;========================== ERA
; strid = 'JAN_ERA'
; gifgroups = file_search('gif_3d/ERA_Jan*.gif')

; strid = 'JUL_ERA'
; gifgroups = file_search('gif_3d/ERA_Jul*.gif')
;----------
; strid = 'JAN_ERA2d'
; gifgroups = file_search('gif_2d/ERA2d_Jan*.gif')

; strid = 'JUL_ERA2d'
; gifgroups = file_search('gif_2d/ERA2d_Jul*.gif')

;========================== FNL 
; strid = 'JAN_FNL3d_20080101'
; gifgroups = file_search('gif_3d/FNL3d_2008*Jan*.gif')

; strid = 'JUL_FNL3d_20080701'
; gifgroups = file_search('gif_3d/FNL3d_2008*Jul*.gif')
;----------
; strid = 'JAN_FNL2d_20080101'
; gifgroups = file_search('gif_2d/FNL2d_2008*Jan*.gif')

; strid = 'JAN_FNL2d_20080701'
; gifgroups = file_search('gif_2d/FNL2d_2008*Jul*.gif')


;========================== C3M
;------
; strid = 'JAN_C3M'
; gifgroups = file_search('gif_3d/C3M_Jan*.gif')

; strid = 'JUL_C3M'
; gifgroups = file_search('gif_3d/C3M_Jul*.gif')
;------
; strid = 'JAN_C3M2d'
; gifgroups = file_search('gif_2d/C3M2d_Jan*.gif')

; strid = 'JUL_C3M2d'
; gifgroups = file_search('gif_2d/C3M2d_Jul*.gif')

;========================== CERES 
; strid = 'JAN_CERES'
; gifgroups = file_search('gif_2d/CERES*Jan*.gif')

; strid = 'JUL_CERES'
; gifgroups = file_search('gif_2d/CERES*Jul*.gif')

;========================== OBS  CMAP GPCP TRMM OLR 
 strid ='JAN_OBS'
; gifgroups = file_search('gif_2d/CMAP*Jan**.gif')
; gifgroups = [gifgroups,file_search('gif_2d/gpcp*Jan**.gif')]
; gifgroups = [gifgroups,file_search('gif_2d/TRMM_Jan**.gif')]
; gifgroups = [gifgroups,file_search('gif_2d/OLR*Jan*.gif')]

; strid ='JUL_OBS'
; gifgroups = file_search('gif_2d/CMAP*Jul**.gif')
; gifgroups = [gifgroups,file_search('gif_2d/gpcp*Jul**.gif')]
; gifgroups = [gifgroups,file_search('gif_2d/TRMM_Jul**.gif')]
; gifgroups = [gifgroups,file_search('gif_2d/OLR*Jul*.gif')]

;========================== CAM
 strid = 'JAN_CAM_3D'
 gifgroups = file_search('gif_3d/CAM_3D_Jan*.gif')

; strid = 'JUL_CAM'
; gifgroups = file_search('gif_3d/Model_Jul*.gif')

;-------
; strid = 'JAN_CAM_P'
; gifgroups = file_search('gif_3d/Model_P*Jan*.gif')

; strid = 'JUL_CAM_P'
; gifgroups = file_search('gif_3d/Model_P*Jul*.gif')

;-------
; strid = 'JAN_CAM2d'
; gifgroups = file_search('gif_2d/cam2d*Jan*.gif')

; strid = 'JUL_CAM2d'
; gifgroups = file_search('gif_2d/cam2d*Jul*.gif')

;-------
;========================== FNL TRMM CAM_i daily
; strid = 'JUL_FNL3d_20170727'
; gifgroups = file_search('gif_3d/FNL3d_Jul*.gif')

; strid = 'JUL_FNL2d_20170727'
; gifgroups = file_search('gif_2d/FNL2d_Jul*.gif')

;========

; strid = 'TRMM_20170727'
; gifgroups = file_search('gif_2d/TRMM*20170727*.gif')

; strid = 'CAM_i_20170727'
; gifgroups = file_search('gif_3d/CAM_i_20170727*.gif')

; strid = 'myCAM_i_20170727'
; gifgroups = file_search('gif_3d/myCAM_i_20170727*.gif')

; strid = 'IAP_i_20170727'
; gifgroups = file_search('gif_3d/IAP_i_20170727*.gif')

;===CASE 20170727 ===========================================================

 strid = 'CAM_20170728_CTL'
 gifgroups = file_search('gif_3d/CAM_2017_JUL_0728*')

; strid = 'CAM_P_20170728_CTL'
; gifgroups = file_search('gif_3d/CAM_P_2017_JUL_0728*')

; strid = 'CAM_2D_20170728_CTL'
; gifgroups = file_search('gif_2d/CAM_2D_2017_JUL_0728*')

; strid = 'CAM_2D_20170729_CTL'
; gifgroups = file_search('gif_3d/CAM_2D_2017_JUL_0729*')

;===========================
; strid = 'CAM_HOURLY_2D_20170728_CTL'
; gifgroups = file_search('gif_2d/CAM_hourly_2D_2017_JUL_0728*')

;===========================
 strid = 'TRMM_3HOURLY_20170728-30'
 gifgroups = file_search('gif_TRMM/TRMM_3h_*.gif')

; strid = 'TRMM2d_3HOURLY_20170728-30'
; gifgroups = file_search('gif_TRMM_coarse/TRMM_3h_*.gif')

 strid = 'TRMM_CAM2d_3HOURLY_20170728-30'
 gifgroups = file_search('gif_TRMM_CAM/CAM_3h_*.gif')

 strid = 'TRMM_CAM2dxin1_3HOURLY_20170728-30'
 gifgroups = file_search('gif_TRMM_CAM_xin1/xin1_CAM_3h_*.gif')
;===========================

; SCP tests
 strid = icase+'_20170728'
  f3=icase+'_3h_20170727_03_PRECT.gif'  ; to view like trmm starting from 00
  f0=icase+'_3h_20170727_00_PRECT.gif'
  str = 'cp '+ 'gif_trmm_'+icase+'/'+f3+' gif_trmm_'+icase+'/'+f0
  spawn,str
 gifgroups = file_search('gif_trmm_'+icase+'/'+icase+'*')


;=======================
 nlev=caseiap
 strid = 'IAP'+nlev+'_3HOURLY_20170728-30'

 file1 = gif_folder+'/'+caseiap+'_3h_20170727_03_PRECT.gif ' 
 file2 = gif_folder+'/'+caseiap+'_3h_20170727_00_PRECT.gif ' 
 spawn,'cp '+ file1+file2

 ;gifgroups = file_search('gif_trmm_iap30/IAPL30_3h*.gif')

 ;gifgroups = file_search('gif_trmm_iap'+nlev+'/IAP'+nlev+'_3h*.gif')
 ;gifgroups = file_search('gif_trmm_iap_'+nlev+'/'+nlev+'_3h*.gif')
 gifgroups = file_search(gif_folder+'/'+nlev+'_3h*.gif')

 htmlfile ='html/'+strid+'.html'

 dir='../'
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile

end
