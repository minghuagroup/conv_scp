;igif=0
;iix = 1
;ix=0
;icoarse=1

;goto,jump1
; filein = '../obs/FNL/'+'fnl_20170727_00_00.nc'

 titlename='FNL2d_'+month+'_'
; titlename='FNL2d_2008'+

 print,''
 print,filein2 
 ncdf_vars,filein2,vars
 x=get_fld(filein2,'longitude')
 y=get_fld(filein2,'latitude')

 plev  =indgen(17)*50+100  ; 100 to 900 mb
 plev = [1.,2.,3.,5.,7.,10.,20.,30.,50.,70.,plev, 925.,950.,975, 1000.]

 nx = n_elements(x)
 ny = n_elements(y)
 nz = n_elements(plev)


;AEROD_v = get_fld_fnl2d(filein2,'AEROD_v')
CAPE    = get_fld_fnl2d(filein2,'CAPE_surface')
CIN     = get_fld_fnl2d(filein2,'CIN_surface')

;CLDHGH  = get_fld_fnl2d(filein2,'CLDHGH')
;CLDLOW  = get_fld_fnl2d(filein2,'CLDLOW')
;CLDMED  = get_fld_fnl2d(filein2,'CLDMED')
;CLDTOT  = get_fld_fnl2d(filein2,'CLDTOT')
;FLDS    = get_fld_fnl2d(filein2,'FLDS')
;FLNS    = get_fld_fnl2d(filein2,'FLNS')
;FLNSC   = get_fld_fnl2d(filein2,'FLNSC')
;FLNT    = get_fld_fnl2d(filein2,'FLNT')
;FLNTC   = get_fld_fnl2d(filein2,'FLNTC')
;FLUT    = get_fld_fnl2d(filein2,'FLUT')
;FLUTC   = get_fld_fnl2d(filein2,'FLUTC')
;FSDS    = get_fld_fnl2d(filein2,'FSDS')
;FSDSC   = get_fld_fnl2d(filein2,'FSDSC')
;FSNS    = get_fld_fnl2d(filein2,'FSNS')
;FSNSC   = get_fld_fnl2d(filein2,'FSNSC')
;FSNT    = get_fld_fnl2d(filein2,'FSNT')
;FSNTC   = get_fld_fnl2d(filein2,'FSNTC')
;FSNTOA  = get_fld_fnl2d(filein2,'FSNTOA')
;FSNTOAC = get_fld_fnl2d(filein2,'FSNTOAC')
;FSUTOA  = get_fld_fnl2d(filein2,'FSUTOA')
ICEFRAC = get_fld_fnl2d(filein2,'ICEC_surface')
LANDFRAC= get_fld_fnl2d(filein2,'LAND_surface')
;LHFLX   = get_fld_fnl2d(filein2,'LHFLX')
;LWCF    = get_fld_fnl2d(filein2,'LWCF')
;OCNFRAC = get_fld_fnl2d(filein2,'OCNFRAC')
;PBLH    = get_fld_fnl2d(filein2,'PBLH')
PHIS    = get_fld_fnl2d(filein2,'HGT_surface')
;PRECC   = get_fld_fnl2d(filein2,'PRECC')
;PRECDP  = get_fld_fnl2d(filein2,'PRECDP')
;PRECL   = get_fld_fnl2d(filein2,'PRECL')
;PRECSC  = get_fld_fnl2d(filein2,'PRECSC')
;PRECSH  = get_fld_fnl2d(filein2,'PRECSH')
;PRECSL  = get_fld_fnl2d(filein2,'PRECSL')
;PRECT   = get_fld_fnl2d(filein2,'PRECT')
PS      = get_fld_fnl2d(filein2,'PRES_surface')
PSL     = get_fld_fnl2d(filein2,'PRMSL_meansealevel')
;QFLX    = get_fld_fnl2d(filein2,'QFLX')

RH2M   = get_fld_fnl2d(filein2,'RH_2maboveground')
Q2M    = get_fld_fnl2d(filein2,'SPFH_2maboveground')

;QREFHT  = get_fld_fnl2d(filein2,'QREFHT')
;SHFLX   = get_fld_fnl2d(filein2,'SHFLX')
SNOWD   = get_fld_fnl2d(filein2,'SNOD_surface')
SNOWW   = get_fld_fnl2d(filein2,'WEASD_surface')
;SNOWHICE= get_fld_fnl2d(filein2,'SNOWHICE')
;SNOWHLND= get_fld_fnl2d(filein2,'SNOWHLND')
;SOLIN   = get_fld_fnl2d(filein2,'SOLIN')

SOILW1   = get_fld_fnl2d(filein2,'SOILW_0M0D1mbelowground')
SOILW2   = get_fld_fnl2d(filein2,'SOILW_0D1M0D4mbelowground')
SOILW3   = get_fld_fnl2d(filein2,'SOILW_1M2mbelowground')
SOILW4   = get_fld_fnl2d(filein2,'SOILW_0D4M1mbelowground')

;SWCF    = get_fld_fnl2d(filein2,'SWCF')
;TAUX    = get_fld_fnl2d(filein2,'TAUX')
;TAUY    = get_fld_fnl2d(filein2,'TAUY')
TGCLDCWP= get_fld_fnl2d(filein2,'CWAT_entireatmosphere_consideredasasinglelayer_')
;TGCLDIWP= get_fld_fnl2d(filein2,'TGCLDIWP')
;TGCLDLWP= get_fld_fnl2d(filein2,'TGCLDLWP')
;TMP2D   = get_fld_fnl2d(filein2,'TMP2D')
TMQ = get_fld_fnl2d(filein2,'PWAT_entireatmosphere_consideredasasinglelayer_')
;TREFHT  = get_fld_fnl2d(filein2,'TREFHT')
TOZNE   = get_fld_fnl2d(filein2,'TOZNE_entireatmosphere_consideredasasinglelayer_')
TS      = get_fld_fnl2d(filein2,'TMP_2maboveground')
TS1      = get_fld_fnl2d(filein2,'TSOIL_0M0D1mbelowground')
TS2      = get_fld_fnl2d(filein2,'TSOIL_0D1M0D4mbelowground')
TS3      = get_fld_fnl2d(filein2,'TSOIL_1M2mbelowground')
TS4      = get_fld_fnl2d(filein2,'TSOIL_0D4M1mbelowground')
;TSMN    = get_fld_fnl2d(filein2,'TSMN')
;TSMX    = get_fld_fnl2d(filein2,'TSMX')
U10U     = get_fld_fnl2d(filein2,'UGRD_planetaryboundarylayer')
U10V     = get_fld_fnl2d(filein2,'VGRD_planetaryboundarylayer')
U10     = get_fld_fnl2d(filein2,'UGRD_10maboveground')
V10     = get_fld_fnl2d(filein2,'VGRD_10maboveground')
VRATE    = get_fld_fnl2d(filein2,'VRATE_planetaryboundarylayer')
WGUSTD  = get_fld_fnl2d(filein2,'GUST_surface')

;VARS3 = ['T','Q','RH','U','V','W','CLDW'] 

VARS2 = ['CAPE','CIN','PSL','PS','RH2M','TS','TS1','TGCLDCWP','TMQ',$
       'U10','V10','US10','VRATE','WGUSTD']


FOR iv=0,n_elements(VARS2)-1 DO BEGIN
;====================
 xx = x2  & xx[0]=x2[0]+0.01 & xx[nx-1]=x2[nx-1]-0.01
 yy = y2  & yy[0]=y2[0]+0.01 & yy[ny-1]=y2[ny-1]-0.01

  case VARS2 [iv] of

   'CAPE':      begin & aa2 = CAPE     &  levc=lev_cape           & end
   'CIN':       begin & aa2 = -CIN      &  levc=lev_cin           & end
   'CLDHGH':    begin & aa2 = CLDHGH   &  levc=lev_cld           & end
   'CLDLOW':    begin & aa2 = CLDLOW   &  levc=lev_cld          & end
   'CLDMED':    begin & aa2 = CLDMED   &  levc=lev_cld           & end
   'CLDTOT':    begin & aa2 = CLDTOT   &  levc=lev_cld   & end
   'FLNSC':     begin & aa2 = FLNSC    &  levc=lev_flnsc & end
   'FLNT':      begin & aa2 = FLNT     &  levc=lev_flnt & end
   'FLNTC':     begin & aa2 = FLNTC    &  levc=lev_flnt & end
   'FSDS':      begin & aa2 = FSDS     &  levc=lev_fsds & end
   'FSDSC':     begin & aa2 = FSDSC    &  levc=lev_fsds & end
   'FSNS':      begin & aa2 = FSNS     &  levc=lev_fsds & end
   'FSNSC':     begin & aa2 = FSNSC    &  levc=lev_fsds & end
   'FSNT':      begin & aa2 = FSNT     &  levc=lev_fsnt & end
   'FSNTC':     begin & aa2 = FSNTC    &  levc=lev_fsnt & end
   'LHFLX':     begin & aa2 = LHFLX    &  levc=lev_lhflx & end
   'LWCF':      begin & aa2 = LWCF     &  levc=lev_lwcf & end
   'PBLH':      begin & aa2 = PBLH     &  levc=lev_pblh & end
   'PRECC':     begin & aa2 = PRECC*86400*1000    &  levc=lev_precc & end
   'PRECDP':    begin & aa2 = PRECDP*86400*1000   &  levc=lev_precc & end
   'PRECL':     begin & aa2 = PRECL *86400*1000   &  levc=lev_precl & end
   'PRECSH':    begin & aa2 = PRECSH *86400*1000  &  levc=lev_precc & end
   'PRECSC':    begin & aa2 = PRECSC *86400*1000  &  levc=lev_precc & end
   'PRECSL':    begin & aa2 = PRECSL *86400*1000  &  levc=lev_precl & end
   'PRECT':     begin & aa2 = PRECT *86400*1000   &  levc=lev_prect & end
   'PS':       begin & aa2 = PS/100.      &  levc=lev_ps & end
   'PSL':       begin & aa2 = PSL/100.      &  levc=lev_ps & end
   'RH2M':       begin & aa2 = RH2M/100.      &  levc=lev_rh & end
   'SHFLX':     begin & aa2 = SHFLX    &  levc=lev_shflx & end
   'SWCF':      begin & aa2 = SWCF     &  levc=lev_swcf & end
   'TAUX':      begin & aa2 = TAUX     &  levc=lev_taux & end
   'TAUY':      begin & aa2 = TAUY     &  levc=lev_taux & end
   'TGCLDCWP':  begin & aa2 = TGCLDCWP*100 &  levc=lev_tgcldlwp & end
   'TGCLDIWP':  begin & aa2 = TGCLDIWP*100 &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 = TGCLDLWP*100 &  levc=lev_tgcldlwp & end
   'TMQ':       begin & aa2 = TMQ      &  levc=lev_tmq & end
   'TS':        begin & aa2 = TS-273.10       &  levc=lev_ts & end
   'TS1':        begin & aa2 = TS1-273.10       &  levc=lev_ts & end
   'U10':       begin & aa2 = U10      &  levc=lev_v10 & end
   'V10':       begin & aa2 = V10      &  levc=lev_v10 & end
   'US10':       begin & aa2 = SQRT(U10^2+V10^2) &  levc=lev_u10 & end
   'VRATE':       begin & aa2 = VRATE/1000      &  levc=lev_vrate & end
   'WGUSTD':    begin & aa2 = WGUSTD/10   &  levc=lev_wgustd & end

   else: print,'Variable not found!!'
  endcase

  var  = VARS2[iv]
  lev1 = levc
  lev2 = [1000.,2000]
;  lev2 = (levc-levc[0])*3+levc[0]  ;[1000.,2000]

  aa = aa2
  bb = aa
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )

 ny=n_elements(yy)  ; do weighted average
 aaw=aa*0
 bbw=aaw
 jj=where(aa gt -999.)
 aaw[jj] = aa[jj]
 bbw[jj] = 1.
 pi2=3.1416/180.
 for j=0,ny-1 do begin
  cosz = cos(yy[j]*pi2)
  aaw[*,j] = cosz*aaw[*,j]
  bbw[*,j] = cosz*bbw[*,j]
 endfor

 value = ' ('+strtrim(min(aa[jj]),2)+', '+strtrim(max(aa[jj]),2)+', '$
    +strtrim(mean(aaw)/mean(bbw),2)+')'

 gifname= titlename+var
 title= gifname+value

 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gif_2d/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif

ENDFOR ;iv 2d
;====================


end

