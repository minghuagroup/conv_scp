
;=====================================

function get_fldvisst,yyyymmddhh,var, daily=daily,hr24=hr24, scale1,$
    nnearest=nnearest,hours=hours, native=native, nostop = nostop,$
    restoreonly=restoreonly,f1sav=filesav,f2sav=filesav2,ff=f1

;=====================================

; var is newly named
; hours returns the hours of the day when data are available

;   daily: output daily averaged field
;   output scale of var

; restore,f2 from folder yyyymmddhha(era4sav) ; era4_iap3d
; restore,f1 from folder yyyymmddhh (era4res) ; interpol2, interpol3; save yyyymmddhha field
; get_era5.pro to save yyyymmddhh field
; my_interpol2.pro
; my_interpol3.pro
; my_interpol3v.pro
;=====================================
 restore,'visst.sav0'
 restore,'iap.sav0'
 nx2 = n_elements(lon2)
 ny2 = n_elements(lat2)

  if(not keyword_set(daily))then daily=0
  if(not keyword_set(native))then visst=0 else visst=1
  if(not keyword_set(hr24))then hr24=0
  if(not keyword_set(scale1))then scale1=1.0
  if(not keyword_set(nostop))then nostop=0  ; nostop do not terminate

 datadir0  = 'visst0'  ; .nc downloaded data
 datadir  = 'visst'    ; .sav data in native resolution
 datadir2 = 'vissta'   ; .sav data in interoplated resultion
 if(not file_exists(datadir0))then spawn,'mkdir '+datadir0
 if(not file_exists(datadir))then  spawn,'mkdir '+datadir
 if(not file_exists(datadir2))then spawn,'mkdir '+datadir2

  yyyymmdd = strmid(yyyymmddhh,0,8)
  yyyy = strmid(yyyymmddhh,0,4)
  mm   = strmid(yyyymmddhh,4,2)
  dd   = strmid(yyyymmddhh,6,2)
  hh   = strmid(yyyymmddhh,8,2)
  hh0  = fix(strmid(yyyymmddhh,8,2)) ; only get one time data, either hh or daily (daily overides)

 datadir0= datadir0+'/'+yyyy+mm
 datadir = datadir +'/'+yyyy+mm
 datadir2= datadir2+'/'+yyyy+mm
 if(not file_exists(datadir0))then spawn,'mkdir '+datadir0
 if(not file_exists(datadir))then  spawn,'mkdir '+datadir
 if(not file_exists(datadir2))then spawn,'mkdir '+datadir2
 datadir0= datadir0+'/'
 datadir = datadir +'/'
 datadir2= datadir2+'/'

  ndays = julday(fix(mm),fix(dd),fix(yyyy),fix(hh),0,0)
  ndays = ndays- julday(1,1,fix(yyyy),fix(hh),0,0) +1
  sndays= strmid(strtrim(ndays+1000,2),1,3) 

  var0 = visst_vars(var,scale1=scale1)  ; to visst filename

  webdir0 ='https://satcorps.larc.nasa.gov/prod/Global-GEO/visst-grid-netcdf/'
  webdir1 = webdir0 + yyyy+'/'+mm+'/'+dd+'/'

  fileseg1 = 'prod.Global-GEO.visst-grid-netcdf.'

  f1day = fileseg1 + yyyymmdd + '.GEO-MRGD.*.GRID.NC' ; for download one day 

 f1 = fileseg1 + yyyymmdd + '.GEO-MRGD.*'+hh+'00.GRID.NC'
 f11 = datadir0 + f1


 filesav  = datadir  +yyyymmdd+'_'+var0+'.sav'    ; to sav/res original resolution
 filesav2 = datadir2 + yyyymmdd+'_'+var0+'.sav'   ; to save/res interopated data

 if(keyword_Set(restoreonly))then begin
  if(not file_exists(filesav2))then begin
     restoreonly = 2
     return,2
  endif else begin
     goto,jump1
  endelse
 endif

;1 ==============================================================
;  check post-processed var filesav data ======================
;stop

 if(visst) then begin  ; get native data
  if(file_exists(filesav)) then begin
     restore,filesav   ; aa obtained, filesaved
     goto, jump1
  endif 
 endif 

 if(not visst) then begin  ; get native data
  if(file_exists(filesav2)) then begin   ; filesav2 and filesav are saved together
     restore,filesav2 ; get aa2 reassign to aa
     aa = aa2
     goto, jump1
  endif 
 endif

;-------- daily data -----------------------
; filesav2 and filesav are saved together

;1.1 ==============================================================
;  check whether raw data exist, if not download all hourly data in the day
;  and prepare them for save

  files = file_search(datadir0 + f1day, count=cntf)

 if(cntf eq 0)then begin                          ; *.nc data does not exist
;1.1.1 download data ==========================================
   for i=0,23 do begin
    sii = strmid(strtrim(i+100,2),1,2)
    f1j = fileseg1 + yyyymmdd + '.GEO-MRGD.'+yyyy+sndays+'.'+sii+'00.GRID.NC' 
    cmd = 'wget '+ webdir1+  f1j + ' | ' +'mv '+f1j+' '+datadir0+'.'
;stop
    print,i
    spawn, cmd   ; <---------- download data 1 file at a time
   endfor

   files = file_search(datadir0 + f1day, count=cntf)

;1.1.2 check whether data are downloaded and check # of hrs =======================
   if(cntf eq 0)then begin
      print,'No file exists on web for: ', cmd 
      print,'stopped in get_fldvisst 1'
      if(nostop)then return, i else stop
    endif

    if(cntf ne 24)then begin
      print,'******** This folder does not have 24 hourly files *****'
      print,'******** Verify this at website: ',webdir1
      ;print,'If the number is incorrect, remove visst/. and re-reun program'
      for i=0,cntf-1 do print,i,': ',files[i]
      endif
  endif ;finished nc data 

;1.1.3 read and save the variable for the whole day, save aa
  ; create aa dimension
  dd = get_fld(files[0], var0)
  aa = replicate2(dd, cntf)

  sz = size(dd)
; ----------read all data in the data and store in aa in native grids
  for i=0, cntf-1 do begin                
    f12 = files[i]
    if(sz[0] eq 2)then aa[*,*,i] = get_fld(f12,var0) 
    if(sz[0] eq 3)then aa[*,*,*,i] = get_fld(f12,var0) 
  endfor

  filesaved = files

  save,filename = filesav,aa,filesaved ; <-----------------
  help,aa,filesaved
  print,'saved file:',filesav 

;1.1.4 interpolate saved data  and save aa2 =======================

jump2:
  restore,filesav
  cntf = n_elements(filesaved)
  sz = size(aa)

;---------------------
  if(cntf eq 1)then begin ; only one file is available, array dimension different
    if(sz[0] eq 2)then begin  ;aa(nx,ny)
      bb = my_interpol2(aa,visstlon,visstlat,lon2,lat2,missing=-9900.)
    endif else begin   ; aa(ntype,nx,ny)
      bb = fltarr(sz[1],nx2,ny2)
      for k = 0,sz[1]-1 do begin
       aaj = reform(aa[k,*,*],sz[2],sz[3])
       bb[k,*,*] = my_interpol2(reform(aaj),visstlon,visstlat,$
                   lon2,lat2,tdim=1, missing= - 9900.)
      endfor
    endelse 

    goto,jump_cntf
  endif
;---------------------

  if(sz[0] eq 3)then begin  ;aa(nx,ny,cntf)
   bb = my_interpol2(aa,visstlon,visstlat,lon2,lat2,tdim=1, missing=-9900.)
  endif else begin          ;aa(ntypes,nx,ny,cntf)
   bb = fltarr(sz[1],nx2,ny2,cntf)
   for k = 0,sz[1]-1 do begin
     aaj = reform(aa[k,*,*,*],sz[2],sz[3],sz[4])
     bb[k,*,*,*] = my_interpol2(reform(aaj),visstlon,visstlat,$
                   lon2,lat2,tdim=1, missing= - 9900.)
   endfor
  endelse 

jump_cntf:

   aa2 = bb
  save,filename = filesav2,aa2,filesaved ; <-----------------
  help,aa2
  print,'saved file:',filesav2

;;=================================--------------

jump1: ;<==============================================
 if(visst)then begin
   restore,filesav    ;< --------------  !!!!!!!!!!!!!!!
   print,'aa restored from ',filesav
 endif
 if(not visst)then begin
    restore,filesav2  ;< --------------  !!!!!!!!!!!!!!!!1
    print,'aa2 restored and assigned to aa from ',filesav2 
    aa = aa2
 endif


 ;aa and filesaved restored

;2 ===============================================================

;2.1 check what hours data exisit ================================

  files = filesaved  ; original houly file list
  cntf  = n_elements(files)
; ==============
  sz = size(aa)
; ==============
   if(cntf ne 24)then begin
    print,'******** My warning: ***********************************'
    print,'******** This folder does not have 24 hourly files *****'
    print,'******** Verify this at website for the: ',cntf, ' files at ',webdir1
;;    for i=0,cntf-1 do print,i,'  ', files[i]
    ;print,'If the number is incorrect, remove visst/. and re-reun program'
   endif

  j1 = strpos(files[0],'00.GRID.NC')-2
  hhs = strmid(files,j1,2)   
  dhh = abs(fix(hhs)-hh0)
  jj2 = where(dhh eq min(dhh))
  j1  = jj2[0]
  hours = fix(hhs)    ;<-------- to recored what hours are returned

;2.2 if for a particular hr, get the nearest hour ========================

if(daily+hr24 eq 0) then begin ; for a particular hr
  jj1 = strpos(files,'.'+hh+'00.GRID.NC')   ; check this hour
  jj  = where(jj1 gt 0,cnt)
   if(cnt eq 0 and (not keyword_Set(nnearest)))then begin
   print,'these files exisit:'
   for i=0,cntf-1 do print,i,' ',files[i]
   print,'requested file is for hour ',hh 
   print,'This hourly file does not exisit, if need to check the nearest hour,'$
         +'set flag nearest=1'
   print,'stop in get_fldvisst 3'
   if(nostop)then return, i else stop
   endif  
endif   

;3 ===============================================================
; get data in native or interpolated grids using aa 

   if(hr24)  then return, aa
   
 if(cntf eq 0)then begin
     return,aa     ; only 1 array is available
 endif

   if(sz[0] eq 3)then begin     ; d(nlon,nlat,nhour)
     if(daily eq 1)then dd = ave3(aa,missing=-9899.) 
     if(daily eq 0)then dd = reform(aa[*,*,j1])
     return,dd
   endif

   if(sz[0] eq 4)then begin
                                ; d(ntypes,nlon,nlat,nhour)
     if(daily eq 1)then dd = ave4(aa,missing=-9899.) 
     if(daily eq 0)then dd = reform(aa[*,*,*,j1])
     return,dd
   endif

end
