

; =====================================================================================
  PRO SAV2GIF, dir_sav, dir_jpeg, setZ = setZ, month = month, season = season  
; =====================================================================================
; =====================================================================================

; purpose from one data sav folder to plot to the jpeg foler, inteval scales based on CAM mapping in cmip6_2cesm_Var.pro
 ; input:

; dir_sav   = '/Users/minghuazhang/unix/cmip6/CMIP/NCAR_CESM2/amip/sav/'  ; input
; dir_jpeg  = '~/unix/cmip6/test/jpeg/'  ; output

; im   = 6
; setZ = 1  ; run in background batch mode


; type of preparations

nSeasons    = ['DJF','MAM','JJA','SON','ANN']
jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1


if(keyword_set(month))then begin
  im = month
endif else if (keyword_set(season)) then begin
  jj = where(season eq nseasons,cnt)
  if(cnt eq 0)then begin
     print,'the specified season is not correct'
     stop
  endfor
  jj=jj[0]
  jjm = reform( jj_seasons[*,jj[0]])
else begin
  jjm = indgen(12) ; annual average is assumed as default
endelse 


 make_folder, dir_jpeg

 print, ' from dir_sav = ', dir_sav, ' to dir_jpeg = ', dir_jpeg

 jfiles = file_search(dir_sav+'*.sav')
 jvars  = get_lastv2_inlist(jfiles, '/', '.', sep3 = '_')
 
 for iv = 0, n_elements(jfiles)-1 do begin
; for iv = 31, n_elements(jfiles)-1 do begin
; ====================================
  fname = jfiles[iv]
  var0  = jvars[iv]
  file_jpeg = dir_jpeg + var0 

  restore, fname
; data from running cmip2sav.pro  
; save,file=fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att

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

 case sz[0] of
 3: dd  = ave3(dd3m[*,*,im])
 4: dd  = ave4(dd3m[*,*,*,im])
 2: dd  = dd3m
 else: goto, jump_next_var
 endcase

 iave = 3
 if(sz[0] eq 4)then iave =1 
 
 var_cam = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose
 vart    = var0
 if(not keyword_set(cam_var))then vart = var_cam + ' (' + var0 + ')'    

 levstr = get_levstr( dd, var_cam) ;nn=20,or -20 etc
 lev1   = levstr.lev
 scale  = levstr.scale
 lev2  = lev1*4
 dd = dd  * scale

 if(min(dd) eq max(dd)) then goto, jump_next_var


 view3d,vart,dd,xx2,yy2,zz2,iave,lev1,lev2, setZ= setZ
 ix = 1
 saveimage, file_jpeg+'.jpeg', jpeg = 1, quality=100
 set_plot,'X'

 if(not setZ) then begin
  read,ix
  wwdelete
 endif

jump_next_var:

;stop
 
endfor ; iv

end

