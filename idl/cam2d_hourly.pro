

print,' run 00_range.pro first ok ?? 1 for yes'

;NF =0  ;!!!

;filems='../obs/'+['test1.cam.h0.2008-01.nc','test1.cam.h0.2008-07.nc']

FOR ij = 0,nf-1 do begin ;L30, L35 loop
;================================
filein2= filems[ij]

titlename= datatype2

ncdf_vars,filein2,vars2

print,filein2
print,titlename

var=''
print, 'Which variable to plot?'
read,var
 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 nx2 = n_elements(x2)
 ny2 = n_elements(y2)

 CASE VAR of
 ;'PRECT': data = get_fld(filein2,'PRECC')+get_fld(filein2,'PRECL')
 'ALBEDO': begin
    solin = get_fld(filein2,'SOLIN')
    fsnt = get_fld(filein2,'FSNT')
    jj=where(SOLIN gt 1.0e-3)
    data=solin*0
    data[jj] = (solin[jj]-fsnt[jj])/solin[jj]
    end
 'TGCLDCWP': data = get_fld(filein2,'TGCLDIWP')+get_fld(filein2,'TGCLDLWP')
     
 else: data = get_fld(filein2,var)
 endcase

  print,'in file:', filein2
; FOR II = 22,23,2 DO BEGIN
 FOR II = 0,,2 DO BEGIN
;====================================
  aa2 = reform(data[*,*,II])
  print,'var  :  ',var
  print,'month:  ',II+1

  goto,jump_plot

AEROD_v = get_fld(filein2,'AEROD_v')
CLDHGH  = get_fld(filein2,'CLDHGH')
CLDLOW  = get_fld(filein2,'CLDLOW')
CLDMED  = get_fld(filein2,'CLDMED') 
CLDTOT  = get_fld(filein2,'CLDTOT') 
FLDS    = get_fld(filein2,'FLDS')
FLNS    = get_fld(filein2,'FLNS')
FLNSC   = get_fld(filein2,'FLNSC') 
FLNT    = get_fld(filein2,'FLNT') 
FLNTC   = get_fld(filein2,'FLNTC') 
FLUT    = get_fld(filein2,'FLUT') 
FLUTC   = get_fld(filein2,'FLUTC') 
FSDS    = get_fld(filein2,'FSDS') 
FSDSC   = get_fld(filein2,'FSDSC') 
FSNS    = get_fld(filein2,'FSNS') 
FSNSC   = get_fld(filein2,'FSNSC')
FSNT    = get_fld(filein2,'FSNT') 
FSNTC   = get_fld(filein2,'FSNTC') 
FSNTOA  = get_fld(filein2,'FSNTOA') 
FSNTOAC = get_fld(filein2,'FSNTOAC') 
FSUTOA  = get_fld(filein2,'FSUTOA') 
ICEFRAC = get_fld(filein2,'ICEFRAC') 
LANDFRAC= get_fld(filein2,'LANDFRAC') 
LHFLX   = get_fld(filein2,'LHFLX') 
LWCF    = get_fld(filein2,'LWCF')
OCNFRAC = get_fld(filein2,'OCNFRAC') 
PBLH    = get_fld(filein2,'PBLH') 
PHIS    = get_fld(filein2,'PHIS') 
PRECC   = get_fld(filein2,'PRECC') 
PRECDP  = get_fld(filein2,'PRECDP') 
PRECL   = get_fld(filein2,'PRECL') 
PRECSC  = get_fld(filein2,'PRECSC')
PRECSH  = get_fld(filein2,'PRECSH') 
PRECSL  = get_fld(filein2,'PRECSL') 
PRECT   = get_fld(filein2,'PRECT') 
PS      = get_fld(filein2,'PS') 
PSL     = get_fld(filein2,'PSL') 
QFLX    = get_fld(filein2,'QFLX') 
QREFHT  = get_fld(filein2,'QREFHT') 
SHFLX   = get_fld(filein2,'SHFLX') 
SNOWHICE= get_fld(filein2,'SNOWHICE') 
SNOWHLND= get_fld(filein2,'SNOWHLND')
SOLIN   = get_fld(filein2,'SOLIN') 
SWCF    = get_fld(filein2,'SWCF') 
TAUX    = get_fld(filein2,'TAUX') 
TAUY    = get_fld(filein2,'TAUY') 
TGCLDCWP= get_fld(filein2,'TGCLDCWP') 
TGCLDIWP= get_fld(filein2,'TGCLDIWP') 
TGCLDLWP= get_fld(filein2,'TGCLDLWP') 
TMP2D   = get_fld(filein2,'TMP2D') 
TMQ     = get_fld(filein2,'TMQ') 
TREFHT  = get_fld(filein2,'TREFHT') 
TS      = get_fld(filein2,'TS') 
TSMN    = get_fld(filein2,'TSMN')
TSMX    = get_fld(filein2,'TSMX') 
U10     = get_fld(filein2,'U10')
WGUSTD  = get_fld(filein2,'WGUSTD')


IF(idata eq 15)then begin  ;case 20170728
;;ACTNI  = get_fld(filein2,'ACTNI')
;;ACTNL  = get_fld(filein2,'ACTNL')
;;ACTREL  = get_fld(filein2,'ACTREL')
;;ACTREI  = get_fld(filein2,'ACTREI')
PCLDTOP  = get_fld(filein2,'PCLDTOP')  ;;shuld be saved!
PCLDBOT  = get_fld(filein2,'PCLDBOT')  ;;shuld be saved!
CLDTOP  = get_fld(filein2,'CLDTOP')
CLDBOT  = get_fld(filein2,'CLDBOT')
;;FCTL  = get_fld(filein2,'FCTL')
;;FCTI  = get_fld(filein2,'FCTI')
;;3d LIQCLDF  = get_fld(filein2,'LIQCLDF')
;;3d  ICECLDF  = get_fld(filein2,'ICECLDF')

UW_pblh = get_fld(filein2,'UW_pblh')
cbmf_Cu = get_fld(filein2,'cbmf_Cu')
rcwp_Cu = get_fld(filein2,'rcwp_Cu')
riwp_Cu = get_fld(filein2,'riwp_Cu')
rlwp_Cu = get_fld(filein2,'rlwp_Cu')
tkeavg_Cu = get_fld(filein2,'tkeavg_Cu')
;3d ufrc_Cu = get_fld(filein2,'ufrc_Cu')
; 3d AST
endif


; save,filena=filesave,$
;AEROD_v, ANRAIN, ANSNOW, AQRAIN, AQSNOW, AREI, AREL, AWNC, AWNI, CCN3, CDNUMC, $
;CLDHGH, CLDICE, CLDLIQ, CLDLOW, CLDMED, CLDTOT, CLOUD, DCQ, DTCOND, DTV, EMISCLD, FICE, FLDS,$
;FLNS, FLNSC, FLNT, FLNTC, FLUT, FLUTC, FREQI, FREQL, FREQR, FREQS, FSDS, FSDSC, FSNS, FSNSC, $
;FSNT, FSNTC, FSNTOA, FSNTOAC, FSUTOA, ICEFRAC, ICIMR, ICWMR, IWC, LANDFRAC, LHFLX, LWCF, $
;NUMICE, NUMLIQ, OCNFRAC, OMEGA, PBLH, PHIS, PMID, PRECC, PRECDP, PRECL, PRECSC, $
;PRECSH, PRECSL, PRECT, PS, PSL, Q, QFLX, QREFHT, QRL, QRS, RELHUM, SHFLX, SNOWHICE, SNOWHLND, $
;SOLIN, SWCF, T, TAUX, TAUY, TGCLDCWP, TGCLDIWP, TGCLDLWP, TMP2D, TMQ, TREFHT, TS, TSMN,$
;TSMX, U, U10, V, VD01, WGUSTD, WSUB, Z3, X2,Y2,Z2'

 VARS2=['ACTNI','ACTNL','ACTREL','ACTREI','FCTI','FCTL'] ; not output!
 VARS2 = ['PCLDTOP', 'PCLDBOT','CLDTOP', 'CLDBOT',$
           'UW_pblh','rcwp_Cu','riwp_Cu','rlwp_Cu','tkeavg_Cu']


 VARS=['CLDHGH',  'CLDLOW', 'CLDMED', 'CLDTOT', 'FLNSC', 'FLNT', 'FLNTC',$
       'FSDS', 'FSDSC', 'FSNS', 'FSNSC', 'FSNT', 'FSNTC','LHFLX', 'LWCF', $
       'PBLH', 'PRECC', 'PRECDP', 'PRECL', 'PRECSC', $
       'PRECSH', 'PRECSL', 'PRECT', 'PSL', 'SHFLX',$ 
        'SWCF', 'TAUX', 'TAUY', 'TGCLDCWP', 'TGCLDIWP', 'TGCLDLWP', $
       'TMQ', 'TS', 'U10','WGUSTD']

 VARS=[VARS,VARS2]

jump_plot:
 VARS =[var]

FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2

  case VARS[iv] of 

   'ALBEDO':    begin & aa2= aa2  &  levc=lev_cld & end
   'ACTREI':    begin & aa2= aa2  &  levc=lev_actrei		 & end
   'ACTREL':    begin & aa2= aa2  &  levc=lev_actrel		 & end
   'CLDHGH':    begin & aa2= aa2 &  levc=lev_cld		 & end
   'CLDLOW':    begin & aa2= aa2 &  levc=lev_cld		& end
   'CLDMED':    begin & aa2= aa2 &  levc=lev_cld		 & end
   'CLDTOT':    begin & aa2= aa2 &  levc=lev_cld   & end
   'FLNSC':     begin & aa2= aa2 &  levc=lev_flnsc & end
   'FLNT':      begin & aa2= aa2 &  levc=lev_flnt & end
   'FLNTC':     begin & aa2= aa2 &  levc=lev_flnt	& end
   'FSDS':      begin & aa2= aa2 &  levc=lev_fsds	& end
   'FSDSC':     begin & aa2= aa2 &  levc=lev_fsds	& end
   'FSNS':      begin & aa2= aa2 &  levc=lev_fsds & end
   'FSNSC':     begin & aa2= aa2 &  levc=lev_fsds & end
   'FSNT':      begin & aa2= aa2 &  levc=lev_fsnt & end
   'FSNTC':     begin & aa2= aa2 &  levc=lev_fsnt & end
   'LHFLX':     begin & aa2= aa2 &  levc=lev_lhflx & end
   'LWCF':      begin & aa2= aa2 &  levc=lev_lwcf & end
   'PBLH':      begin & aa2= aa2 &  levc=lev_pblh & end

   'PRECC':     begin & aa2= aa2 *86400*1000  & jj=where(aa2 le 1.0e-2) & aa2[jj] = -1.  &  levc=lev_precc & end
   'PRECDP':    begin & aa2= aa2 *86400*1000 & jj=where(aa2 le 1.0e-2) & aa2[jj] = -1.  &  levc=lev_precc & end
   'PRECL':     begin & aa2= aa2 *86400*1000 & jj=where(aa2 le 1.0e-2) & aa2[jj] = -1.  &  levc=lev_precl & end
   'PRECSH':    begin & aa2= aa2 *86400*1000 & jj=where(aa2 le 1.0e-2) & aa2[jj] = -1. &  levc=lev_precc & end
   'PRECSC':    begin & aa2= aa2 *86400*1000 & jj=where(aa2 le 1.0e-2) & aa2[jj] = -1. &  levc=lev_precc & end
   'PRECSL':    begin & aa2= aa2 *86400*1000  &  jj=where(aa2 le 1.0e-2)&  aa2[jj] = -1. &  levc=lev_precl & end
   'PRECT':     begin & aa2= aa2 *86400*1000 & jj=where(aa2 le 1.0e-2) & aa2[jj] = -1.  &  levc=lev_prect & end
   'PSL':       begin & aa2= aa2 /100.      &  levc=lev_ps & end
   'SHFLX':     begin & aa2= aa2 &  levc=lev_shflx & end
   'SWCF':      begin & aa2= aa2 &  levc=lev_swcf & end
   'TAUX':      begin & aa2= aa2 &  levc=lev_taux & end
   'TAUY':      begin & aa2= aa2 &  levc=lev_taux & end
   'TGCLDCWP':  begin & aa2= aa2 *100 &  levc=lev_tgcldlwp & end
   'TGCLDIWP':  begin & aa2 =aa2 *100 &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 =aa2 *100 &  levc=lev_tgcldlwp & end
   'TMQ':       begin & aa2 = aa2&  levc=lev_tmq & end
   'TS':        begin & aa2 = aa2-273.10       &  levc=lev_ts & end
   'U10':       begin & aa2 = aa2&  levc=lev_u10 & end
   'WGUSTD':    begin & aa2 = aa2&  levc=lev_wgustd & end

   'PCLDTOP':     begin & aa2 = 1000.-aa2/100.  &  levc=lev_pres  & end
   'PCLDBOT':     begin & aa2 = 1000.-aa2/100.  &  levc=lev_pres  & end
   'CLDTOP':      begin & aa2 = 30-aa2&  levc=lev_k  & end
   'CLDBOT':      begin & aa2 = 30-aa2&  levc=lev_k  & end
   'LIQCLDF':     begin & aa2 = aa2&  levc=lev_cld  & end
   'ICECLDF':     begin & aa2 = aa2&  levc=lev_cld  & end
   'UW_pblh':     begin & aa2 = aa2&  levc=lev_pblh  & end
   'rcwp_Cu':     begin & aa2 = aa2*100  &  levc=lev_tgcldlwp  & end
   'rlwp_Cu':     begin & aa2 = aa2*100   &  levc=lev_tgcldlwp  & end
   'riwp_Cu':     begin & aa2 = aa2*100   &  levc=lev_tgcldlwp  & end
   'tkeavg_Cu':   begin & aa2 = aa2&  levc=lev_tke_cu  & end
   'ufrc_Cu':     begin & aa2 = aa2&  levc=lev_cld_cu  & end
;;   '':     begin & aa2 =   &  levc=lev_  & end

   else: print,'Variable not found!!'
  endcase
  
  var  = VARS[iv] 
  lev1 = levc
;  lev2 = (levc-levc[0])*3+levc[0]  
  lev2 = [2000.,4000]

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

 gifname= titlename+var+'_'+strtrim(II+1,2)
 title= gifname+value

jj=where(abs(aa) le 1.0e-4,cnt)  ;!!!
if(cnt gt 0)then aa[jj] = lev1[0]-1

i60=1
jjlat = where(abs(y) gt 60.)
if(i60) then for i=0,nx-1 do aa[i,jjlat] = lev1[0]-1
   
bb=aa
 if(max(aa) ne min(aa)) then begin
  plot_map40,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
;;  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gif_2d/'+gifname+'.gif'
   endif

  if(iix)then  read,ix

 endif

ENDFOR ;iv
;====================
ENDFOR ;II for month
;====================


end
