; docformat = 'rst'
;+
;
; This program outputs an interactive GUI with droplists to quickly
;  view all available variables in a WRF output file.
;
; :Private:
;
; :Categories:
;   WRF, GUI, Nair Programs
;
; :Examples:
;   .compile wrf_quicklook
;   wrf_quicklook, '/rstor/freitagb/WRF-3.7.1/WRFV3/run_maracaibo/maracaibo/g4_20141008/'
;
; NOTE: The trailing slash is required at the end of the directory name
;
; :Uses:
;   None.
;
; :Author:
;   Brian M. Freitag, University of Alabama in Huntsville, freitagb@uah.edu
;   
; :History:
;   Modification History::
;     First written: Jan 8, 2016
;     
;-
;

;output the vapor pressure in the plot window
PRO plot_model

compile_opt idl2

;call to the info structure
COMMON plot_data,  pvar, units, desc, xlon, xlat
COMMON user_data, uservar, userdom, userdate, userpress, derived, new

erase, color=cgcolor('white')

if userdom EQ 'd01' then begin
lon_del = 2.5 & lat_del = 2.5 & lbl = 2
cgrd1 = 21 & cgrd2 = 21
endif

if userdom EQ 'd02' then begin
lon_del = 0.75 & lat_del = 0.75 & lbl = 4
cgrd1 = 21 & cgrd2 = 21
endif

if userdom EQ 'd03' then begin
lon_del = 0.25 & lat_del = 0.25 & lbl = 4
cgrd1 = 20 & cgrd2 = 20
endif

if userdom EQ 'd04' then begin
lon_del = 0.125 & lat_del = 0.125 & lbl = 2
cgrd1 = 14 & cgrd2 = 14
endif


if uservar EQ 'T' then pvar += 300
c = mean(pvar)
s = stddev(pvar)

max_pvar = c + 1.75*s
if min(pvar) EQ 0 then min_pvar = min(pvar) else $
   min_pvar = c - 1.75*s

if uservar EQ 'REFL' then max_pvar = 60.
if uservar EQ 'RAINRATE' then max_pvar = 50.
if uservar EQ 'ACCRAIN' then max_pvar = max(pvar)-(max(pvar)*.2)
if uservar EQ 'CLWP' then begin
max_pvar = 500. & min_pvar = 0.
endif

diff = max_pvar - min_pvar
lab_constant = diff/5.
lev_constant = diff/255.

map_set, /LAMBERT,mean(xlat),mean(xlon), $
 LIMIT=[min(xlat),min(xlon),max(xlat),max(xlon)], $
 CHARSIZE=2,/hires, /NOERASE,/NOBORDER, /isotropic, $
 YMARGIN=[5,5],XMARGIN=[5,5],COLOR=cgcolor('white')

cgloadct,34,RGB_TABLE=256, NCOLORS=256,/silent
if min_pvar EQ 0 then tvlct, 255,255,255,0
mclrs = indgen(256) & mlvls = (findgen(256)*lev_constant)+ min_pvar
lab = string((indgen(6)*lab_constant+ min_pvar),form='(F10.2)')
mlvls[0] = -1D+36 & mlvls[255] = 1D+36

cgcontour, pvar, xlon, xlat,/cell_fill, levels=mlvls,$
c_colors=mclrs,/overplot

cgcolorbar,palette=rgb,Divisions=5, BOTTOM=1, CHARSIZE=1.15,$
POSITION= [0.05,0.2,0.075,0.8], TICKNAMES = lab

if uservar EQ 'WIND10M' OR uservar EQ 'WIND' then begin
 ;read in U10 and V10 data
 pfile = file_search(!indir + '/wrfout_' + userdom + '_' + $
                     userdate)
 pid = NCDF_OPEN(pfile)
     varid = NCDF_VARID(pid, 'U10')
     NCDF_VARGET, pid, varid, u10
     varid = NCDF_VARID(pid, 'V10')
     NCDF_VARGET, pid, varid, v10
     varid = NCDF_VARID(pid, 'XLONG')
     NCDF_VARGET, pid, varid, veclon
     varid = NCDF_VARID(pid, 'XLAT')
     NCDF_VARGET, pid, varid, veclat
 NCDF_CLOSE, pid
 lon = reform(veclon[*,0], n_elements(xlon[*,0])) 
 lat = reform(veclat[0,*],n_elements(xlat[0,*]))
 vecu10 = congrid(u10,cgrd1,cgrd2) & vecv10 = congrid(v10,cgrd1,cgrd2) 
 vec10mwind = congrid(pvar,cgrd1,cgrd2)
 veclon = congrid(lon,cgrd1) & veclat = congrid(lat,cgrd2)
 velovect, vecu10/vec10mwind, vecv10/vec10mwind, veclon, veclat,$
          color=cgcolor('black'),length=0.5, thick=2.5, /OVERPLOT
endif

map_continents, /USA, /countries, /coasts, /hires, $
                COLOR=cgcolor('black'),thick=3, /NOERASE

map_grid, LABEL=lbl, LONDEL=lon_del, LATDEL=lon_del, /BOX_AXES, $
          COLOR = cgcolor('charcoal'), CHARSIZE=1.5

cgtext, 0.5, 0.95, 'WRF-Simulated ' + desc + ' on ' + userdate + $
                   ' for ' + userdom, color=cgcolor('black'),/normal,$
                   charsize=1.75, align=0.5

cgtext, 0.5, 0.01, units, color=cgcolor('black'),$
        charsize=1.5, charthick=2, align=0.5, /normal

END

;get the wrf model data from the user selections
PRO get_wrf

compile_opt idl2

COMMON plot_data, pvar, units, desc, xlon, xlat
COMMON user_data, uservar, userdom, userdate, userpress, derived,new
COMMON user_select, datelist, domlist, vars2d,vars3d, derlist, presslist

pfile = file_search(!indir + '/wrfout_' + userdom + '_' + $
                    userdate)
 print, 'opening file for ' + userdate
 pid = NCDF_OPEN(pfile)
 varid = NCDF_VARID(pid, 'XLONG')
 NCDF_VARGET,pid,varid,xlon
 varid = NCDF_VARID(pid, 'XLAT')
 NCDF_VARGET,pid,varid,xlat

IF NOT derived then begin
   varid = NCDF_VARID(pid, uservar)
   NCDF_VARGET,pid,varid, pvar ;plotting variable
   NCDF_ATTGET, pid,varid,'units', units
   NCDF_ATTGET, pid,varid, 'description', desc
   units = string(units)
   desc = string(desc)
   ;get the dimensions of the user variable
   dvar = size(pvar,/DIM)

   ;get the lat/lon points for the u-grid
   print, dvar[0],dvar[1]
   if dvar[0] GT dvar[1] and dvar[0]-dvar[1] EQ 1 or $
      dvar[0]-dvar[1] EQ 51 then begin
     tmp = pvar
     pvar = fltarr(dvar[1],dvar[1],dvar[2])
     for x=0,dvar[1]-1 do $
     pvar[x,*,*] = (tmp[x,*,*]+tmp[x+1,*,*])/2.
   endif
   ;get the lat/lon points for the v-grid
   if dvar[0] LT dvar[1] and dvar[1]-dvar[0] EQ 1 or $
      dvar[1]-dvar[0] EQ 51 then begin
     tmp = pvar
     pvar = fltarr(dvar[0],dvar[0],dvar[2])
     for y=0,dvar[0]-1 do $
     pvar[*,y,*] = (tmp[*,y,*]+tmp[*,y+1,*])/2.
   endif

   if n_elements(dvar) EQ 3 then begin
   dvar = size(pvar,/dim)
   ;varid = NCDF_VARID(pid, 'PH')
   ;NCDF_VARGET,pid,varid,ph
   ;varid = NCDF_VARID(pid, 'PHB')
   ;NCDF_VARGET,pid,varid, phb
   varid = NCDF_VARID(pid, 'P')
   NCDF_VARGET,pid,varid, ppress
   varid = NCDF_VARID(pid, 'PB')
   NCDF_VARGET,pid,varid, bpress
   ;get the model heights at each vertical level
   ;hgt = get_height(ph, phb)
   ;for n =0, dvar[2]-1 do height = $
   ;  (hgt[*,*,n] + hgt[*,*,n+1])/2.
   ;get the model pressure
   mpress = get_press(ppress,bpress)/100.
   tmp = pvar
   pvar = fltarr(dvar[0],dvar[1])
   for lo = 0, dvar[0]-1 do begin
     for la = 0, dvar[0]-1 do begin
     
       diff = abs(mpress[lo,la,*] - userpress)
       ind = sort(diff)
       pvar[lo,la] = get_ref_value(mpress[lo,la,*],tmp[lo,la,*],$
                                   ind[0], userpress)
     endfor
   endfor
   endif
 ENDIF

 IF derived then begin
  if uservar EQ 'WIND10M' then begin
     varid = NCDF_VARID(pid, 'U10')
     NCDF_VARGET, pid, varid, u10
     varid = NCDF_VARID(pid, 'V10')
     NCDF_VARGET, pid, varid, v10
     
     ;compute the bulk wind
     pvar = bulk_wind(u10,v10)
     units = 'm s-1'
     desc = '10m Wind'
  endif

  if uservar EQ 'REFL' then begin
     varid = ncdf_varid(pid,'T')
     NCDF_VARGET,pid,varid, pottemp ;potential temperature perturbation
     varid = ncdf_varid(pid,'P')
     NCDF_VARGET,pid,varid, press_pert ;perturbation pressure
     varid = ncdf_varid(pid,'PB')
     NCDF_VARGET,pid,varid, press_base ; base pressure
     varid = ncdf_varid(pid,'PH')
     NCDF_VARGET,pid,varid, geo_pert ;perturbation geopotential
     varid = ncdf_varid(pid,'PHB')
     NCDF_VARGET,pid,varid, geo_base ; base geopotential
     varid = ncdf_varid(pid,'QRAIN')
     NCDF_VARGET,pid,varid, qrain ;rain mixing ratio
     varid = ncdf_varid(pid,'QGRAUP')
     NCDF_VARGET,pid,varid, qgraup ;graupel mixing ratio
     varid = ncdf_varid(pid, 'QSNOW')
     NCDF_VARGET,pid,varid, qsnow ;snow mixing ratio
     varid = ncdf_varid(pid, 'QVAPOR')
     NCDF_VARGET,pid,varid, qvapor ;water vapor mixing ratio

     ;compute the temperature and density to be inputted into
     ; reflectivity function
     press = get_press(press_pert, press_base)
     temp = get_temp(press, pottemp)
     rho = get_rho(press, temp, qvapor)
     height = get_height(geo_pert, geo_base)
     pvar = get_refl(xlon,xlat,qrain,qgraup,qsnow,rho,temp,height)
     units = 'dBZ'
     desc = 'Simulated Reflectivity'
  endif

  if uservar EQ 'PWAT' then begin
     varid = ncdf_varid(pid,'T')
     NCDF_VARGET,pid,varid, pottemp ;potential temperature perturbation
     varid = ncdf_varid(pid,'P')
     NCDF_VARGET,pid,varid, press_pert ;perturbation pressure
     varid = ncdf_varid(pid,'PB')
     NCDF_VARGET,pid,varid, press_base ; base pressure
     varid = ncdf_varid(pid,'PH')
     NCDF_VARGET,pid,varid, geo_pert ;perturbation geopotential
     varid = ncdf_varid(pid,'PHB')
     NCDF_VARGET,pid,varid, geo_base ; base geopotential
     varid = ncdf_varid(pid, 'QVAPOR')
     NCDF_VARGET,pid,varid, qvapor ;water vapor mixing ratio

     ;compute the density to be inputted into the precipitable water
     ; function
     height = get_height(geo_pert,geo_base)
     press  = get_press(press_pert, press_base)
     temp = get_temp(press, pottemp)
     rho = get_rho(press, temp, qvapor)
     pvar = get_pwat(qvapor,rho, height)
     units = 'in'
     desc = 'Precipitable Water'
  endif

  if uservar EQ 'CLWP' then begin
     varid = ncdf_varid(pid,'T')
     NCDF_VARGET,pid,varid, pottemp ;potential temperature perturbation
     varid = ncdf_varid(pid,'P')
     NCDF_VARGET,pid,varid, press_pert ;perturbation pressure
     varid = ncdf_varid(pid,'PB')
     NCDF_VARGET,pid,varid, press_base ; base pressure
     varid = ncdf_varid(pid,'PH')
     NCDF_VARGET,pid,varid, geo_pert ;perturbation geopotential
     varid = ncdf_varid(pid,'PHB')
     NCDF_VARGET,pid,varid, geo_base ; base geopotential
     varid = ncdf_varid(pid, 'QVAPOR')
     NCDF_VARGET,pid,varid, qvapor ;water vapor mixing ratio
     varid = ncdf_varid(pid, 'QCLOUD')
     NCDF_VARGET,pid,varid, qcloud ; cloud water mixing ratio
     varid = ncdf_varid(pid, 'QRAIN')
     NCDF_VARGET,pid,varid, qrain ; rain water mixing ratio
     varid = ncdf_varid(pid, 'QICE')
     NCDF_VARGET, pid, varid, qice ;ice water mixing ratio
     varid = ncdf_varid(pid, 'QSNOW')
     NCDF_VARGET, pid, varid, qsnow ;snow mixing ratio

     ;compute the cloud LWP 
     press = get_press(press_pert, press_base)
     height = get_height(geo_pert, geo_base)
     temp = get_temp(press, pottemp)
     rho = get_rho(press, temp, qvapor)
     pvar = get_lwp(temp,height,rho,qvapor,qcloud,qrain,qice,qsnow)
     units = 'g m!e-2!n'
     desc = 'Liquid Water Path'
  endif

  if uservar EQ 'RAINRATE' OR uservar EQ 'ACCRAIN' then begin
     varid = ncdf_varid(pid,'RAINSH')
     NCDF_VARGET,pid,varid, rainsh ;potential temperature perturbation
     varid = ncdf_varid(pid,'RAINNC')
     NCDF_VARGET,pid,varid, rainnc ;perturbation pressure
     varid = ncdf_varid(pid,'RAINC')
     NCDF_VARGET,pid,varid, rainc
     acc_rain = rainsh + rainnc + rainc
     print, max(acc_rain)
     ind = where(datelist EQ userdate)
     tempdate = datelist[ind-1]
     print, 'the temp date is: ' + tempdate, 'the user date is: ' + userdate
     oldfile = file_search(!indir + '/wrfout_' + userdom + '_' + $
                    tempdate) 
     oid = NCDF_OPEN(oldfile)
     varid = ncdf_varid(oid,'RAINSH')
     NCDF_VARGET,oid,varid, rainsh ;potential temperature perturbation
     varid = ncdf_varid(oid,'RAINNC')
     NCDF_VARGET,oid,varid, rainnc ;perturbation pressure
     varid = ncdf_varid(oid,'RAINC')
     NCDF_VARGET,oid,varid, rainc
     old_rain = rainsh + rainnc + rainc
     NCDF_CLOSE, oid
     print, max(old_rain)
     ;subtract the accumulated rain in the previous file and multiply by 4
     ; to get RR in terms of mm per hour
     if uservar EQ 'RAINRATE' then begin
     pvar = (acc_rain - old_rain)*4.
     units = 'mm hr!e-1!n'
     desc = 'Rain Rate'
     end
     
     if uservar EQ 'ACCRAIN' then begin
     pvar = acc_rain
     units = 'mm'
     desc = 'Accumulated Precipitation'
     end
  endif
  
  if uservar EQ '2M-DEW' then begin
     varid = ncdf_varid(pid,'T2')
     NCDF_VARGET,pid,varid, t2 ;2-meter temperature
     varid = ncdf_varid(pid,'PSFC')
     NCDF_VARGET,pid,varid, psfc ;surface pressure
     varid = ncdf_varid(pid,'QVAPOR')
     NCDF_VARGET,pid,varid, qvapor
     
     ;get the 2-meter dewpoint temperature in Kelvin
     pvar = get_td(t2,qvapor, psfc)
     units = 'K'
     desc = '2-meter Dewpoint'
  endif

  if uservar EQ 'MSLP' then begin
    varid = ncdf_varid(pid,'T')
    NCDF_VARGET,pid,varid,pottemp ;perturbation potential temperature
    varid = ncdf_varid(pid, 'PH')
    NCDF_VARGET,pid,varid,geo_pert ;perturbation geopotential
    varid = ncdf_varid(pid, 'PHB')
    NCDF_VARGET,pid,varid,geo_base ;base geopotential
    varid = ncdf_varid(pid, 'P') 
    NCDF_VARGET,pid,varid,press_pert ;perturbation pressure
    varid = ncdf_varid(pid, 'PB')
    NCDF_VARGET,pid,varid,press_base ;base pressure
    varid = ncdf_varid(pid, 'QVAPOR')
    NCDF_VARGET,pid,varid, qvapor ;vapor pressure
    varid = ncdf_varid(pid, 'HGT')
    NCDF_VARGET,pid,varid, hgt ;topographic height
    ;get the mean sea level pressure
    press = get_press(press_pert, press_base)
    height = get_height(geo_pert, geo_base)
    temp = get_temp(press, pottemp)
    pvar = get_mslp(height,press,temp,qvapor)
    units = 'hPa'
    desc = 'Mean Sea-Level Pressure'

  endif

  if uservar EQ 'HEIGHT' then begin
    varid = ncdf_varid(pid, 'PH')
    NCDF_VARGET,pid,varid,geo_pert ;perturbation geopotential
    varid = ncdf_varid(pid, 'PHB')
    NCDF_VARGET,pid,varid,geo_base ;base geopotential
    varid = NCDF_VARID(pid, 'P')
    NCDF_VARGET,pid,varid, ppress
    varid = NCDF_VARID(pid, 'PB')
    NCDF_VARGET,pid,varid, bpress
    
   ;get the model heights at each vertical level
    mpress = get_press(ppress,bpress)/100.
    dvar = size(geo_pert, /dimension)
    hgt = []
    for i = 0, dvar[2]-2 do begin
        hgt = [[[hgt]],[[(get_height(geo_pert[*,*,i],geo_base[*,*,i]) + $
              get_height(geo_pert[*,*,i+1],geo_base[*,*,i]))/2.]]]
    endfor
    tmp = hgt
    pvar = fltarr(dvar[0],dvar[1])
    for lo = 0, dvar[0]-1 do begin
      for la = 0, dvar[0]-1 do begin
        diff = abs(mpress[lo,la,*] - userpress)
        ind = sort(diff)
        pvar[lo,la] = get_ref_value(mpress[lo,la,*],tmp[lo,la,*],$
                                   ind[0], userpress)/10.
      endfor
    endfor
   units = 'dm'
   desc = 'Geopotential Height'
  endif
 
  if uservar EQ '1000-500THICK' then begin
    varid = ncdf_varid(pid, 'PH')
    NCDF_VARGET,pid,varid,geo_pert ;perturbation geopotential
    varid = ncdf_varid(pid, 'PHB')
    NCDF_VARGET,pid,varid,geo_base ;base geopotential
    varid = NCDF_VARID(pid, 'P')
    NCDF_VARGET,pid,varid, ppress
    varid = NCDF_VARID(pid, 'PB')
    NCDF_VARGET,pid,varid, bpress

   ;get the model heights at each vertical level
    mpress = get_press(ppress,bpress)/100.
    dvar = size(geo_pert, /dimension)
    hgt = []
    for i = 0, dvar[2]-2 do begin
        hgt = [[[hgt]],[[(get_height(geo_pert[*,*,i],geo_base[*,*,i]) + $
              get_height(geo_pert[*,*,i+1],geo_base[*,*,i]))/2.]]]
    endfor
    tmp = hgt
    h1000 = fltarr(dvar[0],dvar[1])
    h500 = fltarr(dvar[0],dvar[1])
    for lo = 0, dvar[0]-1 do begin
      for la = 0, dvar[0]-1 do begin
        diff = abs(mpress[lo,la,*] - 1000.) & ind1 = sort(diff)
        diff = abs(mpress[lo,la,*] - 500.)  & ind5 = sort(diff)
        h1000[lo,la] = get_ref_value(mpress[lo,la,*],tmp[lo,la,*],$
                                   ind1[0], 1000.)/10.
        h500[lo,la] = get_ref_value(mpress[lo,la,*],tmp[lo,la,*],$
                                   ind5[0],500.)/10.
      endfor
    endfor

   ;calculate the 1000-500 mb thickness
   pvar = h500-h1000  
   units = 'dm'
   desc = '1000-500 mb Thickness'
  endif

;end the derived variable section the code
 ENDIF

;close the netcdf
NCDF_CLOSE, pid

END

;create an event handler to obtain the user selected values
pro wrf_quicklook_event, event

compile_opt idl2

;common block for default values
COMMON user_select, datelist, domlist, vars2d,vars3d, derlist, presslist
COMMON user_data, uservar, userdom, userdate, userpress, derived, new
COMMON plot_data, pvar, units, desc, xlon, xlat

;call the info structure to use within the case statement
widget_control, event.top, get_uvalue=info
widget_control, event.id, get_uvalue=uval

; Run a series of commands based on the value of uval to obtain the events
; for each droplist.
CASE uval OF
 
 'TIME' : BEGIN
          userdate = $
	  widget_info(info.time,/combobox_gettext)
          ;get the model data
          get_wrf
          new = 0
          END

 'DOMAIN': BEGIN
           userdom = $
	   widget_info(info.domain, /combobox_gettext)
           ;get the model data
           get_wrf
           END
 
 '2DVAR': BEGIN
	  uservar = $
          widget_info(info.var2d, /combobox_gettext)
          ;get the model data for the 2d variables
          derived = 0
          get_wrf
          END 

 '3DVAR': BEGIN
	  uservar = $
          widget_info(info.var3d,/combobox_gettext)
          ;get the model data for the 3d variables
          derived = 0
          get_wrf
          END

 'DERIVED': BEGIN
	    uservar = $
            widget_info(info.dervar, /combobox_gettext)
            ;get the model data needed for the derived variables
            derived = 1
            get_wrf
            END

 'PRESSURE': BEGIN
	     userpress = $
             widget_info(info.plevel, /combobox_gettext)
             ;get the model data for the new pressure level
             get_wrf
             END
    
 'PLOT': BEGIN
         if max(size(pvar,/DIM)) EQ 0 then begin
         print, 'PLEASE SELECT A VARIABLE TO BE PLOTTED'
         break
         endif else plot_model
         END

 'ADVANCE':BEGIN
         if new EQ 0 then begin
         ind = where(datelist EQ userdate)
         new = ind + 1
         endif else $
         new += 1
         if new GT n_elements(datelist) then begin
         print, 'You are already on the final history file!'
         break
         endif else userdate = datelist[new]
         get_wrf
         plot_model
           END

 'BACK':BEGIN
         if new EQ 0 then begin
         ind = where(datelist EQ userdate)
         new = ind - 1
         endif else $ 
         new -=1
         if new LT 0 then begin
         print, 'You already at the earliest history file!'
         break
         endif else userdate = datelist[new]
         get_wrf
         plot_model
         END

ENDCASE

END

;write the main program for the widget (GUI)
PRO wrf_quicklook, indir

compile_opt idl2

;error catcher
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /cancel
  HELP, /last_message, output = errMsg
  For i=0, N_ELEMENTS(errMsg) -1 DO print, errMsg[i]
ENDIF

;save the indir as a system variable
DEFSYSV, '!indir', indir

;create the common block for the default variables
COMMON user_select, datelist, domlist, vars2d, vars3d, derlist, presslist
COMMON user_data, uservar, userdom, userdate, userpress, derived,new

;get the wrf output files
file = file_search(indir + '/wrfout_d02*')

;set up the droplist for the date, 2d and 3d variables
datelist = []
vars2d = []
vars3d = []
;open the netcdf files for each day
  for k = 0,n_elements(file)-1 do begin
  id = ncdf_open(file[k])
  fname = strsplit(file[k],/EXTRACT, '/')
  fname =  fname[n_elements(fname)-1]
  ;obtain the list of dates for the dropdown list in the widget
  datelist = [datelist,strmid(fname, 11,19)]
  ncdf_inq = ncdf_inquire(id)
  ;obtain the list of variables for the dropdown list in the widget
  varlist = STRARR(ncdf_inq.nvars)
  for l=0, ncdf_inq.nvars-1 DO varlist[l] = (NCDF_VARINQ(id,l)).name
  if k EQ 0 then begin
  ;separate the model variables into 2d and 3d variables
  for v = 0, n_elements(varlist)-1 do begin
  varid = ncdf_varid(id,varlist[v])
  NCDF_VARGET, id,varid, var
  dims = size(var, /dim)
  if n_elements(dims) EQ 2 then $
     vars2d = [vars2d, varlist[v]]
  if n_elements(dims) EQ 3 then $
     vars3d = [vars3d, varlist[v]]
  endfor
  endif
  ncdf_close, id
  endfor
spawn, 'ls ' + indir + 'wrfout*' + datelist[0], doms

domlist = []

;develop the list of domains for the widget
for k=0, n_elements(doms)-1 do begin
  temp = strsplit(doms[k], /EXTRACT, '/')
  temp = temp[n_elements(temp)-1]
  domlist = [domlist, strmid(temp,7,3)]
endfor

;develop the list for model derived variables
derlist = ['REFL','WIND10M','CLWP','PWAT','RAINRATE', 'ACCRAIN',$
           'MUCAPE', 'LI', '2M-DEW', 'THETA-E', '1000-500THICK',$
           'MSLP', 'HEIGHT']
presslist = [1000.,950.,900.,850.,800.,750.,700.,600.,500.,400.,300.,200.,100.]
spresslist = string(presslist)
;develop the widget framework
base = widget_base(row=2, title = 'WRF Output',$
                   tlb_frame_attr=1)

;set up the title for the plot window
base1 = widget_base(base, row=1,$
                    tlb_frame_attr=1)

;set up the droplists and plot button
base2 = widget_base(base, column=9)
droplist1 = widget_combobox(base2, value=datelist, uvalue= 'TIME')
droplist2 = widget_combobox(base2, value=domlist, uvalue = 'DOMAIN')
droplist3 = widget_combobox(base2, value=vars2d,$
                            uvalue = '2DVAR')
droplist4 = widget_combobox(base2, value=vars3d,$
                            uvalue = '3DVAR')
droplist5 = widget_combobox(base2, value=derlist, $
                            uvalue = 'DERIVED')
droplist6 = widget_combobox(base2, value=spresslist, $
                            uvalue = 'PRESSURE')
plot_button = widget_button(base2, uvalue='PLOT', value='PLOT')
back_button = widget_button(base2, uvalue='BACK', value='<<')
adv_button = widget_button(base2, uvalue='ADVANCE',value='>>')
;set up the blank plot window
draw1 = widget_draw(base1, xsize = 1000, ysize=900)

;open the widget on the plot window
widget_control, base, /realize

info = {time:droplist1, domain:droplist2, var2d:droplist3,$
        var3d:droplist4, dervar:droplist5, plevel:droplist6,$
        pbutton:plot_button,abutton:adv_button,bbutton:back_button}
widget_control, base, set_uvalue=info
widget_control, draw1, get_value=winid1
wset, winid1
device, decomposed=0
tvlct,[0,0,255],[0,255,255], [0,255,255]


;set up the default variables
info.time = 0 & userdate = datelist[0]
info.domain = 0 & userdom = domlist[0]
info.var2d = 0 & uservar = vars2d[0]
info.var3d = 0 & new = 0
info.dervar = 0 & derived = 0
info.plevel = 0  & userpress = presslist[0]
XMANAGER, 'wrf_quicklook', base, /NO_BLOCK

end 
