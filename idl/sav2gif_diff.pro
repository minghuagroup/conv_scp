

; =====================================================================================
  PRO SAV2GIF_DIFF, dir_savs, dir_jpeg, setZ = setZ, month = month, season = season  
; =====================================================================================
; =====================================================================================
; 
; purpose from two data sav folders dir_savs to plot to the jpeg foler, 
; dir_sav[0] is the CTL folder, dir_sav[2] is the test folder data files 
; inteval scales based on CAM mapping in cmip6_2cesm_Var.pro
; the restored data format can be customized
;
; output folder will be appended by the season  dir_jpeg/ANN/ /DJF/ ...
;
; it will create folders for month and seasons under dir_jpeg and create jpeg files
; input like:
; dir_sav[0]   = '/Users/minghuazhang/unix/cmip6/CMIP/NCAR_CESM2/amip/sav/'  ; input CTL
; dir_sav[1]   = '/Users/minghuazhang/unix/cmip6/ScenarioMIP/NCAR_CESM2/ssp585/sav/'  ; input
; dir_jpeg  = '~/unix/cmip6/test/jpeg/'  ; output

; im   = 6
; setZ = 1  ; run in background batch mode

; type of preparations

nSeasons    = ['DJF','MAM','JJA','SON','ANN']
jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1

if(not keyword_set(season))then season = 'ANN'

jjm = indgen(12) ; annual average is assumed as default
if(keyword_set(month))then begin
  im = month
  SS = strmid(strtrim(im+101),1,2) 
endif else begin
  jj = where(season eq nseasons,cnt)
  if(cnt eq 0)then begin
     print,'the specified season is not correct'
     stop
  endif
  jj=jj[0]
  SS = 'ANN'
  if(season ne 'ANN')then begin
    jjm = reform( jj_seasons[*,jj[0]])
    SS  = season
  endif
endelse 

 make_folder, dir_jpeg

 print, ' test case dir_savs[1] = ', dir_savs[1]
 print, ' CTL  case dir_savs[0] = ', dir_savs[0]
 print, ' Output    dir_jpeg    = ', dir_jpeg

 sav0_files = file_search(dir_savs[0]+'*.sav')
 vars_sav0  = get_lastv2_inlist(sav0_files, '/', '.', sep3 = '_')

 sav1_files = file_search(dir_savs[1]+'*.sav')
 vars_sav1  = get_lastv2_inlist(sav1_files, '/', '.', sep3 = '_')

 for iv = 14, n_elements(sav1_files)-1 do begin
; ====================================
  varj       = vars_sav1[iv]
  input_fname = sav1_files[iv]

  restore, input_fname


; data from running cmip2sav.pro  
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

  dd3m_b = less_100(dd3m, cmip6_2cesm_var(varj,c=1))
 
 ;-----------------
; find the matching sav0_files
  jj=where(varj eq vars_sav0, cnt)
  if(cnt eq 0)then goto, jump_next_var  ; no matching file found

  j1 = jj[0]
  restore,sav0_files[j1]
  dd3m_a = less_100(dd3m, cmip6_2cesm_var(varj,c=1))


  dd3m = dd3m_b - dd3m_a

 sz = size(dd3m)
 if(sz[0] lt 2)then goto,jump_next_var

 xx2 = lon3
 yy2 = lat3
 zz2 = lev3
 zz2 = abs(zz2)
 if(max(zz2) gt 2000.)then zz2 = zz2/100.
 if(max(zz2) lt 20.)then zz2 = zz2*1000


 szx = size(xx2)
 szy = size(yy2)
 if(szx[0] gt 1 or (szy[0] gt 1))then goto, jump_next_var ; do not do the irregular grids

;============ THE PART BELOW DEPENDS ON THE FORMAT OF THE SAVED DATA, CAN BE ADAPTED TO READ NETCDF FILES

IF(KEYWORD_SET(MONTH))THEN BEGIN


 case sz[0] of
 3: dd  = ave3(dd3m[*,*,im])
 4: dd  = ave4(dd3m[*,*,*,im])
 2: dd  = dd3m
 else: goto, jump_next_var
 endcase

ENDIF ELSE BEGIN; SEASONAL


 case sz[0] of
 3: dd  = ave3(dd3m[*,*,jjm])
 4: dd  = ave4(dd3m[*,*,*,jjm])
 2: dd  = dd3m
 else: goto, jump_next_var
 endcase

 ENDELSE
;=======================================

 iave = 3
 if(sz[0] eq 4)then iave =1 
 
 var_cam = cmip6_2cesm_var(varj, c=1)   ; for unit and title purpose
 vart    = varj
 if(not keyword_set(cam_var))then vart = var_cam + ' (' + varj + ')'    

;stop
 levstr = get_levstr( dd, var_cam, adjust=0) ;nn=20,or -20 etc
 lev1   = levstr.dlev
 scale  = levstr.scale
 lev2  = lev1*4 

 jj = where(lev2 ne 0.0, cnt)       ; to exclude the zero line to reduce clutter!!
 if(cnt gt 0) then jj = lev2[jj]  
 dd = dd  * scale

 if(min(dd) eq max(dd)) then goto, jump_next_var


 view3d,vart,dd,xx2,yy2,zz2,iave,lev1,lev2, setZ= setZ
 ix = 1

 file_jpeg_out  = dir_jpeg +SS + '_' + varj + '.jpeg'  ; ;like /ANN/ANN_ts.jpeg'
 saveimage, file_jpeg_out, jpeg = 1, quality=100
 print, iv, ' ', varj, ' ', vart, ' ', file_jpeg_out
 set_plot,'X'

 if(not setZ) then begin
  read,ix
  wwdelete
 endif

jump_next_var:

;stop
 
endfor ; iv
return

end

