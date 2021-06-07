
; =====================================================================================
  PRO SAV2GIF, dir_sav, dir_jpeg, setZ = setZ, month = month, season = season  , $
    plot_levels = plot_levels, std_levels=std_levels,  SSC = SSC, icolor=icolor, $
   plot_1d =plot_1d
; =====================================================================================
; =====================================================================================
; 
; purpose from one data sav folder to plot to the jpeg foler, inteval scales based on CAM mapping in cmip6_2cesm_Var.pro
; the restored data format can be customized
; SSC is for the color file, icolor to create color structure, SSC may reuse part
;
; it will create folders for month and seasons under dir_jpeg and create jpeg files
; input:

; dir_sav   = '/Users/minghuazhang/unix/cmip6/CMIP/NCAR_CESM2/amip/sav/'  ; input
; dir_jpeg  = '~/unix/cmip6/test/jpeg/'  ; output

; im   = 6
; setZ = 1  ; run in background batch mode

; type of preparations

cmip6_vars = cmip6_vars()

if(not keyword_set(icolor))then icolor = 0 

if(not keyword_set(SSC)) then begin
 view3d_colors = create_struct('dir_jpeg', dir_jpeg )
endif else begin
 restore,SSC ; to get view3d_colors
 SSC0  = strmid(get_lastv_inlist(SSC,'/'),0,3)
 print, ' Color structure file', SSC
endelse


;stop

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

 print, ' from dir_sav = ', dir_sav, ' to dir_jpeg = ', dir_jpeg

 jfiles = file_search(dir_sav+'*.sav')
 jvars  = get_lastv2_inlist(jfiles, '/', '.', sep3 = '_')
 
 for iv = 0, n_elements(jfiles)-1 do begin
; for iv = 25, n_elements(jfiles)-1 do begin
; ====================================

  input_fname = jfiles[iv]
  var0        = jvars[iv]
  var_cam     = cmip6_2cesm_var(var0, c=1)   ; for unit and title purpose

; if(var0 ne 'ta')then goto, jump_next_var


; special data plots
IF(not KEYWORD_SET(MONTH))THEN BEGIN

 case var0 of
 'ta': begin         ; to save multi level fields
; ===========
   jj = where(jvars eq 'ua') & ufile = jfiles[jj[0] ]
   jj = where(jvars eq 'va') & vfile = jfiles[jj[0] ]
   jj = where(jvars eq 'zg') & zfile = jfiles[jj[0] ]
   jj = where(jvars eq 'psl') & psfile = jfiles[jj[0] ]

  restore, psfile
    view3dstr = get_view3dstr( [0,1], 'psl', lev_adjust=1) 
    dd3m = dd3m * view3dstr.scale1
    ps3m = dd3m

  restore, ufile
    view3dstr = get_view3dstr( [0,1], 'ua', lev_adjust=1) 
    dd3m =  dd3m * view3dstr.scale1
    uu3m = dd3m

  restore, vfile
    view3dstr = get_view3dstr( [0,1], 'va', lev_adjust=1) 
    dd3m = dd3m * view3dstr.scale1
    vv3m = dd3m

  restore, zfile
    view3dstr = get_view3dstr( [0,1], 'zg', lev_adjust=1) 
    dd3m = dd3m * view3dstr.scale1
    zz3m = dd3m
         end

; 'psl': begin 
;   if(jj[0] eq -1)then goto, jump_next_var
;; these files do not exist use 1000 mb
;   jj = where(jvars eq 'us') & ufile = jfiles[jj[0] ]
;   jj = where(jvars eq 'vs') & vfile = jfiles[jj[0] ]
;
;   restore, ufile
;    uu3m = dd3m
;  restore, vfile
;    vv3m = dd3m
;
;!!!! interpolate to non-uniform grids
;  lons = reform(lon3[*,100])
;  lats = reform(lat3[100,*])
;stop
;         end
 else:
 endcase
;stop
ENDIF

  restore, input_fname
; ---------------------------------
; data from running cmip2sav.pro  
; save,file= input_fname,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
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

 case var0 of   ; to plot multi level fields under 'ta'
'ta': begin
   uu  = ave4(uu3m[*,*,*,jjm])
   vv  = ave4(vv3m[*,*,*,jjm])
   zz  = ave4(zz3m[*,*,*,jjm])
   psl = ave4(ps3m[*,*,jjm])

      end
;'psl': begin
;   uu  = ave3(uu3m[*,*,jjm])
;   vv  = ave3(vv3m[*,*,jjm])
;      end
 else:
 endcase
 ;-------- SPECIAL DATA TREATMENT, cld positive, precipitation positive etc

 dd = less_100(dd, cmip6_2cesm_var(var0,c=1))

;----------------------------------
   
 iave = 3
 if(sz[0] eq 4)then iave =1 
 
 vart    = var0

 if(not keyword_set(cam_var))then vart = var_cam + ' (' + var0 + ')'    


 sj = SSC0 + '_'+strupcase(var0)
 if(icolor and getin_tag(sj, view3d_colors,r=1))then view3dstr = get_stru(view3d_colors, sj ) else $
    view3dstr = get_view3dstr( dd, var_cam, lev_adjust=1) 

 lev1   = view3dstr.lev1
 scale  = view3dstr.scale1
 lev2  = lev1 ;*4
 dd = dd  * scale

 if(min(dd) eq max(dd)) then goto, jump_next_var

 view3d,vart,dd,dd2 = dd, xx2,yy2,zz2,iave,lev1,lev2, setZ= setZ, view3dstr=view3dstr

 file_jpeg_out  = dir_jpeg +SS + '_' + var0 + '.jpeg'
 saveimage, file_jpeg_out, jpeg = 1, quality=100
 sj = SS+'_'+var0
 if(not icolor and (getin_tag(sj, view3dstr) )) then $
      view3d_colors = create_struct(view3d_colors, sj, view3dstr) 
 print, iv, ' ', var0, ' ', vart, ' ', file_jpeg_out

 ix = 1
 if(not setZ) then begin
       read,ix
       wwdelete
 endif

 sz2 = size(dd)

; =======================================================
; below are to plot level maps for 3-d fields
; ===== plot at pressure levels for 3d fields ==============

IF(KEYWORD_SET(MONTH))THEN GOTO, JUMP_NEXT_VAR

if((not keyword_set(plot_levels)) or (sz[0] ne 4))then goto, jump_next_var

;goto,jump_tmp


judge_dolev1 = (belongsto(var_cam, cmip6_vars.vars3d))
judge_dolev2 = (var0 ne var_cam) or (not belongsto(var0, jvars))   ; take the lowcase var or added 3D
judge_do     = judge_dolev1 and judge_dolev2

if(not judge_do)then goto, jump_next_var

 if(not keyword_set(std_levels))then $
;       std_levels = [300.]
       std_levels = [1000., 850., 500.,200]

 for i=0,n_elements(std_levels)-1 do begin
 ;---------------------------------------

    iave = 3
    varj =  var_cam+strdigit(std_levels[i],0)

    z_Range = [std_levels[i], std_levels[i]]

    print, iv, ' ', varj, z_range

   sj = SSC0 + '_'+strupcase(varj)
   if(icolor and getin_tag(sj,view3d_colors, r=1))then view3dstr = get_stru(view3d_colors, sj ) else $
    view3dstr = get_view3dstr( dd, var_cam, lev_adjust=1) 

    view3d,varj,dd,xx2,yy2,zz2,iave,lev1,lev2, setZ= setZ, z_range=z_range, view3dstr = view3dstr

    file_jpeg_out  = dir_jpeg +SS + '_' + varj + '.jpeg'
    saveimage, file_jpeg_out, jpeg = 1, quality=100
    sj = SS+'_'+varj
   if(not icolor and (getin_tag(sj, view3dstr) )) then $
      view3d_colors = create_struct(view3d_colors, sj, view3dstr) 
    print, iv, ' ', varj, file_jpeg_out
    if(not setZ) then begin
     read,ix
     wwdelete
    endif


 ; added plots 
 if(var0 eq 'ta')then begin  ;   vector plot

 varj =  'UVTZ'+strdigit(std_levels[i],0)
   sj = SSC0 + '_'+strupcase(varj)
   if(icolor and getin_tag(sj,view3d_colors,r=1))then view3dstr = get_stru(view3d_colors, sj ) else $
   view3dstr = get_view3dstr( dd, var0, dd2 = dd, var2 = var0, lev_adjust=1) 
   lev1   = view3dstr.lev1
   lev2   = view3dstr.lev2

   view3d,varj,dd, dd2 = dd, vect = 1, uvel = uu, vvel = vv,xx2,yy2,zz2,iave,lev1,lev2, setZ= setZ, $
          view3dstr = view3dstr
     
    print, iv, ' ', varj, z_range
    file_jpeg_out  = dir_jpeg +SS + '_' + varj +'.jpeg'
    saveimage, file_jpeg_out, jpeg = 1, quality=100
    sj = SS+'_'+varj
   if(not icolor and (getin_tag(sj, view3dstr) )) then $
      view3d_colors = create_struct(view3d_colors, sj, view3dstr) 
    print, iv, ' ', varj, file_jpeg_out
    if(not setZ) then begin
     read,ix
     wwdelete
     endif 
 endif  ; vector plots

endfor     ; number of levels
 ;---------------------------------------

; =======================================================

jump_tmp:

 set_plot,'X'

 if(not setZ) then begin
  read,ix
  wwdelete
 endif

jump_next_var:

;stop
 
endfor ; iv

if(not icolor)then begin
 file_view3d_color = dir_jpeg + SS+'_view3d_colors.sav'
   sj = 'file_view3d_color'
   if( getin_tag(sj,view3d_colors )) then $
      view3d_colors = create_struct(view3d_colors, sj, file_view3d_color) 
 save,file = file_view3d_color, view3d_colors
  print, 'view3d_colors structure saved in file_view3d_color =', file_view3d_color
endif
return

end

