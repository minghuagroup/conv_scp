
;take difference between two exps and make diff plots

MODELID ='NCAR_CESM2'
MIPID   = 'ScenarioMIP'
EXPID   = 'ssp585'

CTL_MODELID ='NCAR_CESM2'
CTL_MIPID   = 'CMIP'
CTL_EXPID   = 'amip'

ID1 =     MIPID + '_' + EXPID                   ; modelID is neglected here
ID2 = CTL_MIPID + '_' +  CTL_EXPID

ID1 =      EXPID                                ; modelID and MIPID are neglected here
ID2 =  CTL_EXPID

Title = ID1 + '-' + ID2

expdir     = '/Users/minghuazhang/unix/cmip6/'+     MIPID + '/' +   ModelID + '/' +    EXPID
CTL_expdir = '/Users/minghuazhang/unix/cmip6/'+ CTL_MIPID + '/' + CTL_ModelID+ '/' + CTL_EXPID

dir_sav1 = expdir+'/sav/'
dir_sav2 = CTL_expdir+'/sav/'
dir_savs = [dir_sav1, dir_sav2]

dir_jpeg  = '/Users/minghuazhang/unix/cmip6/'+     MIPID + '/' +   ModelID + '/' + Title+'/jpeg/'
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
make_folder, dir_jpeg


  spawn, 'ls ' + dir_sav1+'*', sav1_files
  vars_sav1  = get_lastv2_inlist(sav1_files,'/', '.')
;====================================

  spawn, 'ls ' + dir_sav2+'*', sav2_files
  vars_sav2  = get_lastv2_inlist(sav2_files,'/', '.')
;====================================

varsj = cmip6_vars()
vars_cmip6 = varsj.vars_cmip6


Seasons = ['DJF','MAM','JJA','SON','ANN']

jj_seasons = [[1,2,12],[3,4,5],[6,7,8],[9,10,11]] -1

setZ = 0

FOR ISEASON = 0,0 DO BEGIN
; ====================================
  if iseason eq 4 then jjs=indgen(12) else jjs = reform(jj_seasons[*,iseason])
  dir_jpegd = dir_jpeg + '/'+Seasons[iseason]+'/'
  make_folder, dir_jpegd    

; ====================================
for iv = 0, n_elements(sav1_files)-1 do begin
;for iv = 58, n_elements(sav1_files)-1 do begin
; ====================================
 var_sav = vars_sav1[iv]
 var_cam = cmip6_2cesm_var(var_sav,c=1)

 file_jpeg = dir_jpegd + seasons[iseason]+'_'+var_sav

; if(not belongsto(var_cam, varsj.vars3d))then goto, jump_next_var

  restore,sav1_files[iv]
; save,file=filesav,var3, lon3,lat3,lev3,time3,year3,month3,day3, $
;           modelid3, mipid3, expid3, datatype3,nc_files3, climo3,dd3m,dd3m_att
   dd3m_a = dd3m

; find the matching sav2_files
  jj=where(var_sav eq vars_sav2, cnt)
  if(cnt eq 0)then goto, jump_next_var  ; no matching file found

  j1 = jj[0]
  restore,sav2_files[j1]
  dd3m_b = dd3m

  dd3m = dd3m_a - dd3m_b
 
  sz = size(dd3m)
 if(sz[0] lt 2)then goto,jump_next_var
 if(max(dd3m) eq min(dd3m))then goto,jump_next_var


 xx2 = lon3
 yy2 = lat3
 zz2 = lev3
 zz2 = abs(zz2)
 if(max(zz2) gt 2000.)then zz2 = zz2/100.
 if(max(zz2) lt 20.)then zz2 = zz2*1000

 szx = size(xx2)
 szy = size(yy2)
 if(szx[0] gt 1 or (szy[0] gt 1))then goto, jump_next_var
 

 case sz[0] of
 3: dd  = ave3(dd3m[*,*,jjs]) 
 4: dd  = ave4(dd3m[*,*,*,jjs]) 
 else: dd = dd3m
 endcase

 levstr = get_levstr( dd, var_cam) ;nn=20,or -20 etc 
 lev1   = levstr.dlev ;lev
 scale  = levstr.scale
 lev2  = lev1*4


  dd = dd  * scale

  var = var_cam +  ' ('+ var_sav + ')'
 print, iv, ' ', var, ' ', min(dd), max(dd)

 sz = size(dd)
if(sz[0] ge 2 and (min(dd) lt max(dd)) )then begin

 if(sz[0] eq 2)then iave =3 else iave=1
 view3d,var,dd,xx2,yy2,zz2,iave,lev1,lev2,  setZ =setZ 
 ix = 1
 ;saveimage, file_jpeg+'.jpeg', jpeg = 1, quality=100 
 saveimage, 'j1.jpeg', jpeg = 1, quality=100 
 set_plot,'X'
 print,'saved!'
 if(not set_Z) then read,ix
 wwdelete
 endif
stop
jump_next_var:

endfor

ENDFOR ; seasons


 
end
