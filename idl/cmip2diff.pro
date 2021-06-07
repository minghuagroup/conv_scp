
;do individual months to test


MIPID = 'CMIP'
MODELID ='NCAR_CESM2'
EXPID = 'amip'
expdir = '../cmip6/'+MIPID+'/'+MODELID+'/'+EXPID
dir_sav = expdir+'/sav/'
dir_jpeg = expdir+'/jpeg/'
make_folder,dir_sav
make_folder,dir_jpeg


  spawn, 'ls ' + dir_sav+'*', sav_files
  vars_sav  = get_lastv_inlist(sav_files,'/')
;====================================

varsj = cmip6_vars()
vars_cmip6 = varsj.vars_cmip6

im= 1

Seasons = ['DJF','MAM','JJA','SON','ANN']

jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1

FOR ISEASON = 0,4 DO BEGIN
; ====================================
  if iseason eq 4 then jjs=indgen(12) else jjs = reform(jj_seasons[*,iseason])
  dir_jpeg = expdir + '/jpeg/'+Seasons[iseason]+'/'
  make_folder, dir_jpeg    

; ====================================
for iv = 0, n_elements(sav_files)-1 do begin
;for iv = 37, 37 do begin
; ====================================
 var_sav = str_sep(vars_sav[iv],'.')
 var_sav = var_sav[0]
 var_cam = cmip6_2cesm_var(var_sav,c=1)

 file_jpeg = dir_jpeg + seasons[iseason]+'_'+var_sav

; if(not belongsto(var_cam, varsj.vars3d))then goto, jump_next_var

  restore,sav_files[iv]
; save,file=filesav,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att
 
 sz = size(dd3m)
 if(sz[0] lt 2)then goto,jump_next_var

 xx2 = lon3
 yy2 = lat3
 zz2 = lev3
 zz2 = abs(zz2)
 if(max(zz2) gt 2000.)then zz2 = zz2/100.
 if(max(zz2) lt 20.)then zz2 = zz2*1000

 

 case sz[0] of
 3: dd  = ave3(dd3m[*,*,jjs]) 
 4: dd  = ave4(dd3m[*,*,*,jjs]) 
 else: dd = dd3m
 endcase

 levstr = get_levstr( dd, var_cam) ;nn=20,or -20 etc 
 lev1   = levstr.lev
 scale  = levstr.scale
 lev2  = lev1*4


  dd = dd  * scale

  var = var_cam +  ' ('+ var_sav + ')'
 print, iv, ' ', var, ' ', min(dd), max(dd)

 sz = size(dd)
if(sz[0] ge 2 and (min(dd) lt max(dd)) )then begin

 if(sz[0] eq 2)then iave =3 else iave=1
 view3d,var,dd,xx2,yy2,zz2,iave,lev1,lev2
 ix = 1
 saveimage, file_jpeg+'.jpeg', jpeg = 1, quality=100
 ;read,ix
 wdelete
 endif

jump_next_var:

endfor

ENDFOR ; seasons

 
end
