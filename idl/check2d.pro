
igif=0

;specify file and var

dir2 = '../obs/CAM/'
filein2= 'myctl_test.cam.h0.2008-01.nc'
filein2= 'myctl_test.cam.h0.2008-07.nc'
filein2= 'my20170727_ctl.cam.h0.2017-07-28-00000.nc'
;filein2= 'my20170727_ctl.cam.h0.2017-07-29-00000.nc'

;dir2 = '../obs/SST/'
;filein2 = 'my_sst_clim_20170727.nc'
;filein2 = 'my_sst_clim_2008_01_07.nc'
;filein2 = 'sst_HadOIBl_bc_1.9x2.5_clim_c061031.nc'

dir2 = '../temp/'

var = 'SST_cpl'
var = 'SST'
var = 'TS'
var = 'AREL'
;var = 'TGCLDCWP'
;;var = 'PRECT'
titlename= filein2+' '
 gifname= filein2+'_'+var
;
filein2 = dir2 + filein2
print,'plotting '+var
print,'from file: ',filein2

 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')


aa2  = get_fld(filein2,var)
aa2  = reform(aa2[*,*,25])

;====================
  case VAR of 
   'AREL':      begin & aa2 = aa2 &  levc=lev_actrel		 & end
   'AREI':      begin & aa2 = aa2 &  levc=lev_actrei		 & end
   'CLDHGH':    begin & aa2 = aa2 &  levc=lev_cld		 & end
   'CLDLOW':    begin & aa2 = aa2 &  levc=lev_cld		& end
   'CLDMED':    begin & aa2 = aa2 &  levc=lev_cld		 & end
   'CLDTOT':    begin & aa2 = aa2 &  levc=lev_cld   & end
   'FLNSC':     begin & aa2 = aa2 &  levc=lev_flnsc & end
   'FLNT':      begin & aa2 = aa2 &  levc=lev_flnt & end
   'FLNTC':     begin & aa2 = aa2 &  levc=lev_flnt	& end
   'FSDS':      begin & aa2 = aa2 &  levc=lev_fsds	& end
   'FSDSC':     begin & aa2 = aa2 &  levc=lev_fsds	& end
   'FSNS':      begin & aa2 = aa2 &  levc=lev_fsds & end
   'FSNSC':     begin & aa2 = aa2 &  levc=lev_fsds & end
   'FSNT':      begin & aa2 = aa2 &  levc=lev_fsnt & end
   'FSNTC':     begin & aa2 = aa2 &  levc=lev_fsnt & end
   'LHFLX':     begin & aa2 = aa2 &  levc=lev_lhflx & end
   'LWCF':      begin & aa2 = aa2 &  levc=lev_lwcf & end
   'PBLH':      begin & aa2 = aa2 &  levc=lev_pblh & end
   'PRECC':     begin & aa2 = aa2 *86400*1000    &  levc=lev_precc & end
   'PRECDP':    begin & aa2 = aa2 *86400*1000   &  levc=lev_precc & end
   'PRECL':     begin & aa2 = aa2 *86400*1000   &  levc=lev_precl & end
   'PRECSH':    begin & aa2 = aa2 *86400*1000  &  levc=lev_precc & end
   'PRECSC':    begin & aa2 = aa2 *86400*1000  &  levc=lev_precc & end
   'PRECSL':    begin & aa2 = aa2 *86400*1000  &  levc=lev_precl & end
   'PRECT':     begin & aa2 = aa2 *86400*1000   &  levc=lev_prect & end
   'PSL':       begin & aa2 = aa2 /100.      &  levc=lev_ps & end
   'SHFLX':     begin & aa2 = aa2 &  levc=lev_shflx & end
   'SWCF':      begin & aa2 = aa2 &  levc=lev_swcf & end
   'SST':       begin & aa2 = aa2-273.16 &  levc=lev_TS		 & end
   'TAUX':      begin & aa2 = aa2 &  levc=lev_taux & end
   'TAUY':      begin & aa2 = aa2 &  levc=lev_taux & end
   'TGCLDCWP':  begin & aa2 = aa2 *100 &  levc=lev_tgcldlwp & end
   'TGCLDIWP':  begin & aa2 = aa2 *100 &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 = aa2 *100 &  levc=lev_tgcldlwp & end
   'TMQ':       begin & aa2 = aa2 &  levc=lev_tmq & end
   'TG':        begin & aa2 = aa2 -273.10       &  levc=lev_ts & end
   'TS':        begin & aa2 = aa2 -273.10       &  levc=lev_ts & end
   'TSA':        begin & aa2 = aa2 -273.10       &  levc=lev_ts & end
   'U10':       begin & aa2 = aa2 &  levc=lev_u10 & end
   'WGUSTD':    begin & aa2 = aa2 &  levc=lev_wgustd & end

   'PCLDTOP':     begin & aa2 = 1000.-aa2 /100.  &  levc=lev_pres  & end
   'PCLDBOT':     begin & aa2 = 1000.-aa2 /100.  &  levc=lev_pres  & end
   'CLDTOP':      begin & aa2 = 30-aa2 &  levc=lev_k  & end
   'CLDBOT':      begin & aa2 = 30-aa2 &  levc=lev_k  & end
   'LIQCLDF':     begin & aa2 = aa2 &  levc=lev_cld  & end
   'ICECLDF':     begin & aa2 = aa2 &  levc=lev_cld  & end
   'UW_pblh':     begin & aa2 = aa2 &  levc=lev_pblh  & end
   'rcwp_Cu':     begin & aa2 = aa2 *100  &  levc=lev_tgcldlwp  & end
   'rlwp_Cu':     begin & aa2 = aa2 *100   &  levc=lev_tgcldlwp  & end
   'riwp_Cu':     begin & aa2 = aa2 *100   &  levc=lev_tgcldlwp  & end
   'tkeavg_Cu':   begin & aa2 = aa2 &  levc=lev_tke_cu  & end
   'ufrc_Cu':     begin & aa2 = aa2 &  levc=lev_cld_cu  & end
;;   '':     begin & aa2 =   &  =lev_  & end

   else: print,'Variable not found!!'
  endcase
  
  lev1 = levc
;  lev2 = (levc-levc[0])*3+levc[0]  
  lev2 = [2000.,4000]

 xx = x2
 yy = y2

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

 title= gifname+value

 window,/free

 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gif_2d/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif

;====================

end
