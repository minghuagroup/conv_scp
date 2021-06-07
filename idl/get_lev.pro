
;===============================================================
function get_lev,dd, VAR,SCALE, diff = diff, adjust = adjust, cnn_nn = lev2_nn, get_diff = get_diff, $
    lev_max=lev_max, lev_min = lev_min, missing = missing, nn2 = nn2, slev2 = lev2, sdiff2 = diff2, $
    name = name
;===============================================================

; NAME: separate CMIP6 level specifications by calling get_lev_cmip6.pro

; nn2: how many lines,  cnn, how many lines to space, -1 for no line
; lev_min and max set the bound of lev

; output lev, diff= lev2= diff2= 
; input: dd, var, adjust, cnn lev2_nn (spacing), nn2 (number of lines), lev_max,lev_min, bounds of lev in adjust

; input, dd, var, nn ( < 0 number of intervals, based on data values; >0 assigns nn intervals and s
; output  LEVSTR.lev, LEVSTR.dlev (for diff fields) for customized levels for each var, and .levn, .dlevn for specified nn levels

; lev2_nn is the spacing of lines
; nn2 is to force the nu,ner based on lev[0], lev[-1]


ddd = dd

if(keyword_set(name))then begin

  if(name eq 'cmip6')then begin
    LEV = get_lev_cmip6 (dd, VAR,SCALE, diff = diff, adjust = adjust, cnn_nn = lev2_nn, get_diff = get_diff, $
    lev_max=lev_max, lev_min = lev_min, missing = missing, nn2 = nn2, slev2 = lev2, sdiff2 = diff2)
    RETURN, LEV
  endif

endif
 

  

; adjust is to adjust lev and diff based on data values


 if(not keyword_Set(missing))then missing  = 99990. 
 if(not keyword_Set(nn2))then nn = 20                     ; forced lines
 if(not keyword_Set(cnn_nn))then lev2_nn = 1 else lev2_nn = cnn           ; spacing contours or whether to overplot 
 if(not keyword_Set(adjust))then adjust =0 

  diff = [0,0]
  if(max(dd) eq min(dd))then return, [0.,0] 

SCALE=1.
SCALE_PRECIP = 86400.
;SCALE_PRECIP = 1000*86400.  ;if m/s
SCALE_Q      = 1000.
scale_omega = 3600 /100.  ;mb/hr
scale_dqdt=   1.0e3*86400 ; g/kg/day
scale_dtdt =  86400. ; T/day
scale_liq =   1.0e5  ; 10^-2 g/kg
scale_cldnl=  1.0e-6 ; cm-3
scale_cldni=  1.0e-6 ;
scale_tgliq = 1.0e3   ;mm
scale_p =     1.0e-2  ;mb
scale_z = 1.0e-3
scale_phis = 1.0e-4
if(not keyword_set(nn2)) then nn2 = 20


PREC = ['PRECT', 'PRECDP', 'PRECSH', 'PRECC', 'PRECZ', 'PRECCDZM', 'PRECL', 'PRECSL']
PREC = [PREC, 'QFLX','evspsbl']

 CLOUD = ['LCLOUD', 'CLDTOT','CLDMED','CLDLOW','AST','CLDST','LCLOUD','CUFRAC_CU','LIQCLD'] 
 CLOUD = [CLOUD,    'CONCLD', 'CLOUD']

 CLDLIQ = ['CLDLIQ', 'CLDLIQCON', 'CLDLIQDET', 'CLDLIQSTR']
 CLDICE = ['CLDICE', 'CLDICECON', 'CLDICEDET', 'CLDICESTR']


 DTDT = ['DTCOND', 'PTTEND', 'SHDLFT' ,'CMFDT', 'MACPDT','EVAPTZM','EVSNTZM', 'FZSNTZM']
 DTDT = [DTDT, 'HPROGCLD',  'ZMMTT',  'ZMDT', 'DTCORE', 'DPDLFT', 'DTH',  'DTV',  'TTEND', 'TTGWORO',  'TTEND_TOT']
 DTDT = [DTDT, 'QRL','QRS','QRLC','QRSC','DT']

 DQDT = ['CMFLIQ', 'CMFDQ', 'SHDLFLIQ', 'SHDLFICE',  'CMFDQR',   'CME',  'CMELIQ',  'EVAPPREC' ]
 DQDT = [DQDT, 'EVAPQZM', 'EVAPSNOW', 'ZMDICE', 'ZMDLF', 'ZMDLQ', 'TANUMICE',  'TANUMLIQ', 'TAQ',  'TACLDICE']
 DQDT = [DQDT, 'TACLDLIQ', 'PTEQ',  'PTECLDICE', 'PTECLDLIQ',  'PRODPREC', 'QITEN_CU', 'QLTEN_CU', 'QVTEN_CU']
 DQDT = [DQDT, 'QTTEN_CU', 'ZMDQ', 'VD01' , 'DLF_PLEV','DCQ','DQ']

TOTCF = ['TOARAD','TOARADC','TOTCRF','TOTCRFA', 'TOTCRFS', 'ATMNET']
ATMRAD = ['ATMRAD','ATMRADC']
SRFNET = ['SRFNET']
LWCF = ['LWCF','LWCRF']
SWCF = ['SWCF','SWCFA','SWCFS']

std_levels  = [10,20,50,850,(indgen(10)+1)*100]
std_levels  = std_levels[sort(std_levels)]
std_levels  = strtrim(std_levels,2)

CLOUD = [CLOUD, 'CLOUD'+std_levels]
CLDLIQ = [CLDLIQ, 'CLDLIQ'+std_levels]
CLDICE = [CLDICE, 'CLDICE'+std_levels]
U      = ['U'     ,'U'+std_levels]
V      = ['V'     ,'V'+std_levels]
OMEGA = [ 'OMEGA'+std_levels]
RELHUM = ['RELHUM', 'RELHUM'+std_levels]

Q1000 = ['Q'+std_levels[9:*]]
Q100  = ['Q'+std_levels[0:8]]
H100  = ['H'+std_levels]
THETA100  = ['THETA'+std_levels]


;default

;  if(missing lt 0)then jj = where(dd gt missing, cnt) else jj = where(dd lt missing, cnt)
  jj = where(abs(dd) lt abs(missing), cnt) 
       if(cnt gt 0)then begin
         dmin = min(dd[jj])
         dmax = max(dd[jj])
         LEV =  [dmin,dmax, nn2]
         delta = (dmax-dmin)/nn2
         DIFF = [-nn2*delta/2 ,nn2*delta/2 ,nn2]
        endif else begin
         lev = [0,1,1]
         diff = [0,1,1]
        endelse

  CASE 1 OF

; state variables
 belongsto(var, ['T']): BEGIN DIFF = [-10 ,10  ,20  ]  &  LEV =  [180.,310,26] & SCALE=1.0 & END
 belongsto(var, ['TS','TREFHT','TG','TSMN','TSMX']): BEGIN DIFF = [-10 ,10  ,20  ]  &  LEV =  [220.,320,20] & SCALE=1.0 & END
 belongsto(var, ['QREFHT', 'QS']): BEGIN DIFF = [-7 ,7  ,14  ]   & LEV = [0.,24,24]    & SCALE=scale_Q & END
 belongsto(var, ['Q', 'QPLEV',Q1000]): BEGIN DIFF = [-7 ,7  ,14  ]   & LEV = [0.,24,24]    & SCALE=scale_Q & END
 belongsto(var, [Q100]): BEGIN DIFF = [-1 ,1  ,20  ]   & LEV = [0.,3,20]    & SCALE=scale_Q & END
 belongsto(var, [U     ]): BEGIN DIFF = [-15 , 15 , 20 ]  &     LEV =  [-40.,40,20]  & SCALE=1.0 & END
 belongsto(var, [V     ]): BEGIN DIFF = [-5 ,5  ,20  ]  &     LEV =  [-10.,10,20]  & SCALE=1.0 & END
 belongsto(var, [RELHUM, 'RELHUMS']) : BEGIN DIFF = [-20 , 20 ,20  ]  &     LEV =  [0.,100,20]    & SCALE=1  & $
       if (max(dd) gt 10.) then scale =1 & END  ;TEMPO
 belongsto(var, ['OMEGA'     ]): BEGIN DIFF = [-1. , 1 ,20  ]  &     LEV =  [-2.,2,20] & SCALE=scale_omega & END   ;climate
 belongsto(var, [OMEGA     ]): BEGIN DIFF = [-2. , 2 ,20  ]  &     LEV =  [-5.,5,20] & SCALE=scale_omega & END   ;climate
 belongsto(var, ['PS', 'PSL', 'PSLUV','PSS'])    : BEGIN DIFF = [-10. , 10. ,40  ]  &     LEV =   [950.,1050.,40]  & SCALE=1.0e-2 & END
 belongsto(var, ['H','S','THETA','MSE','MSESAT']): BEGIN DIFF = [-20. ,20.  ,20  ]  &     LEV =  [240.,440,40] & SCALE=1.0 & END
 belongsto(var, ['H100','THETA100']): BEGIN DIFF = [-20. ,20.  ,20  ]  &     LEV =  [250.,400,30] & SCALE=1.0 & END

 belongsto(var, ['Z3']): BEGIN DIFF = [-50. ,50  ,20  ]  &     LEV =   [0.,5000,25]    & SCALE=1.e-1& END
 belongsto(var, ['Z31000']): BEGIN DIFF = [-10. ,10  ,40  ]  &     LEV =   [0.,30,30]    & SCALE=1.e-1& END
 belongsto(var, ['Z3850']): BEGIN DIFF = [-20. ,20  ,40  ]  &     LEV =   [110.,170,30]    & SCALE=1.e-1& END
 belongsto(var, ['Z3500']): BEGIN DIFF = [-30. ,30  ,40  ]  &     LEV =   [460.,620,30]    & SCALE=1.e-1& END
 belongsto(var, ['Z3200']): BEGIN DIFF = [-40. ,40  ,40  ]  &     LEV =   [1000.,1300,30]    & SCALE=1.e-1& END

 belongsto(var, ['ORO']): BEGIN DIFF = [-0.2 ,0.2  ,20  ]  &     LEV =   [0.,5.,20]    & SCALE=scale_z & END  ;km
 belongsto(var, ['PHIS']): BEGIN DIFF = [-0.2 ,0.2  ,20  ]  &     LEV =   [0.,5.,20]    & SCALE=scale_phis & END ;km

 belongsto(var, ['SST'   ]): BEGIN  DIFF = [-20 ,20  , 20] &     LEV =   [-4,36,20]     & SCALE=1.0 &  END
 belongsto(var, ['TMQ'   ]): BEGIN  DIFF = [-20 ,20  , 20] &     LEV =   [0.,80,20]     & SCALE=1.0 &  END
;PREC
 belongsto(var, PREC):BEGIN  DIFF = [-4. ,4  ,16  ]  &     LEV =   [0.,15,20]    & SCALE=scale_precip & END

 belongsto(var, ['CAPE'])  : BEGIN DIFF = [ -200, 200. ,20  ]  &     LEV =   [0.,2000,20]  & SCALE=1.0 & END
 belongsto(var, ['CIN' ])  : BEGIN DIFF = [ -100 , 100. ,20  ]  &     LEV =   [0.,1000,20]  & SCALE=1.0 & END
 belongsto(var, ['LHFLX']) : BEGIN DIFF = [-50 , 50 , 20 ]  &     LEV =   [-20,300 ,32]   & SCALE=1.0 & END
 belongsto(var, ['PBLH' ]) : BEGIN DIFF = [-100 , 100. ,20  ]  &     LEV =   [100,1500,20] & SCALE=1.0 & END
 belongsto(var, ['SHFLX']) : BEGIN DIFF = [-20. ,20  ,20  ]  &     LEV =   [-20,200 ,12]   & SCALE=1.0 & END
 belongsto(var, ['PME']) : BEGIN DIFF = [-10 , 10 , 20 ]  &     LEV =   [-100,100 ,20]   & SCALE=scale_precip & END
 belongsto(var, ['TAUX', 'TAUY' ]) : BEGIN DIFF = [-0.1 , 0.1 ,20  ]  &     LEV =   [-0.3,0.3 ,20] & SCALE=1.0 & END
 belongsto(var, ['CI']) : BEGIN DIFF = [-0.5 ,0.5  ,20  ]  &     LEV =   [0,1 ,10]   & SCALE=1.0 & END
 belongsto(var, ['snd']) : BEGIN DIFF = [-0.4 ,0.4  ,20  ]  &     LEV =   [0,1 ,20]   & SCALE=1.0e3 & END
 belongsto(var, ['snm']) : BEGIN DIFF = [-0.05 ,0.05  ,10  ]  &     LEV =   [0,1 ,20]   & SCALE=scale_precip  & END
 belongsto(var, ['sbl']) : BEGIN DIFF = [-0.1 ,0.1  ,20  ]  &     LEV =   [0,0.4 ,20]   & SCALE=scale_precip  & END
 belongsto(var, ['snw']) : BEGIN DIFF = [-0.2 ,0.2  ,20  ]  &     LEV =   [0,2 ,20]   & SCALE=1.0e-3 & END
 belongsto(var, ['mrro'   ]): BEGIN DIFF = [ -20, 20 , 10] &     LEV =   [0.,60,20]  & SCALE=1.0e6& END
 belongsto(var, ['mrros'   ]): BEGIN DIFF = [ -10, 10 , 10] &     LEV =   [0.,10,20]  & SCALE=1.0e6& END
 belongsto(var, ['mrso'  ]): BEGIN DIFF = [ -800, 800 , 20] &     LEV =   [0.,4000,20]  & SCALE=1.0& END
 belongsto(var, ['mrsos'  ]): BEGIN DIFF = [ -100, 100 , 20] &     LEV =   [0.,100,20]  & SCALE=1.0& END

;CLOUD
 belongsto(var, CLOUD):BEGIN 
          DIFF = [-20. , 20 ,20  ]  &     LEV =   [0.,100,20]    & SCALE=100.0 
          if(max(dd) gt 1.9)then SCALE = 1.0 
          END

 belongsto(var, ['TGCLDLWP' ,'TGCLDCWP']) : BEGIN DIFF = [-50. ,50  ,10  ]  &     LEV =   [0,300 ,20]  & SCALE=scale_tgliq & END  
 belongsto(var, ['TGCLDIWP' ]) : BEGIN DIFF = [-50. ,50  ,10  ]  &     LEV =   [0,100 ,20]  & SCALE=scale_tgliq & END  

 belongsto(var, CLDLIQ): BEGIN DIFF = [-2 , -2 ,20  ]  &     LEV =   [0,10,20]      & SCALE=scale_liq & END
 belongsto(var, CLDICE): BEGIN DIFF = [-0.5 ,0.5 ,20  ]  &     LEV =   [0,2,20]      & SCALE=scale_liq & END

;SW
 belongsto(var,['FLUT','FLUTC','FLNT','FLNTC']) : BEGIN DIFF = [-40 ,40  ,20  ]  &     LEV =   [0,360,36]& END  
 belongsto(var,['FLDS','FLDSC','FLDS','FLDSC'])  : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [0,500,25]& END
 belongsto(var,['FLUS','FLUSC'])  : BEGIN DIFF = [-80 ,80  ,20  ]  &     LEV =   [0,600,30]& END
 belongsto(var,['FSDS','FSDSC','SOLIN','FSDT','FSNS','FSNSC','FSNT','FSNTC','FSNTOA','FSNTOAC'] ) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [0,500,25]& END  
 belongsto(var,['FSUS','FSUSC','FSUT','FSUTC','FSUTOA'] ) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [0,200,20]& END  
 belongsto(var,['FLNA','FLNAC','FLNS', 'FLNSC'] ) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [-100,200,20]& END  
 belongsto(var,['FSNA','FSNAC'] ) : BEGIN DIFF = [-10 ,10  ,20  ]  &     LEV =   [0,150,30]& END  

;strpos(var,'FL') eq 0 : BEGIN DIFF = [-30 ,30  ,20  ]  &     LEV =   [-100,500,20]& END  
;strpos(var,'FS') eq 0 : BEGIN DIFF = [-30 ,30  ,20  ]  &     LEV =   [0,500,20] & END 

belongsto(var,LWCF) : BEGIN DIFF = [-10 ,10  ,20  ]  &     LEV =   [-20,120.,28]& END  
belongsto(var,'LWCFA') : BEGIN DIFF = [-10 ,10  ,20  ]  &     LEV =   [-100,100.,20]& END  
belongsto(var,'LWCFS') : BEGIN DIFF = [-20 ,20  ,20  ]  &     LEV =   [0,100.,20]& END  
strpos(var,'LWCRF') ge 0 : BEGIN DIFF = [-30 , 30 ,20  ]  &     LEV =   [0,100,20] & END 

belongsto(var,SWCF) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [-200,0.,20]& END  
belongsto(var,TOTCF) : BEGIN DIFF = [-60 ,60  ,20  ]  &     LEV =   [-200,200.,20]& END  
belongsto(var,SRFNET) : BEGIN DIFF = [-100 ,100  ,20  ]  &     LEV =   [-300,300.,20]& END  
belongsto(var,ATMRAD) : BEGIN DIFF = [-30 ,30  ,20  ]  &     LEV =   [-200,0.,20]& END  
belongsto(var,RESTOM) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [-200,200.,20]& END  
belongsto(var,['SRFRAD']) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [-100,300.,20]& END  
belongsto(var,['SRFRADC']) : BEGIN DIFF = [-50 ,50  ,20  ]  &     LEV =   [-100,300.,20]& END  
belongsto(var,SRFNET) : BEGIN DIFF = [-10 ,10  ,20  ]  &     LEV =   [-300,300.,20]& END  


strpos(var,'RAD') ge 0 : BEGIN DIFF = [-30 , 30 ,20  ]  &     LEV =   [-200,200,20] & END 
strpos(var,'SWCRF') ge 0 : BEGIN DIFF = [30 ,30  ,20  ]  &     LEV =   [-300,0,20] & END 
strpos(var,'SWCF') ge 0 : BEGIN DIFF = [-30 ,30  ,20  ]  &     LEV =   [-300,0,20]& END 
strpos(var,'CRF') ge 0 : BEGIN DIFF = [-30 , 30 ,20  ]  &     LEV =   [-100,100,20] & END 
belongsto(var,['RESTOM']) : BEGIN DIFF = [-20 ,20  ,20  ]  &     LEV =   [-150,150.,20]& END  


 belongsto(var, ['LANDFRAC']): BEGIN DIFF = [-0.10 , 0.1 , 20 ]  &     LEV =   [0.,110,11]     & SCALE=1.0 & END
; dT/dt

  belongsto(var, DTDT) : BEGIN DIFF = [-0.5 ,.5  , 20 ]  &     LEV =   [-10.,10,20] & SCALE=scale_dtdt & END

;dq/dt
 belongsto(var,DQDT)  : BEGIN DIFF = [-0.5 , 0.5 , 20 ]  &     LEV =   [-10.,10,20] & SCALE=scale_dqdt & END

 belongsto(var, ['TDIFF'    ]) : BEGIN DIFF = [-5 ,5  ,20  ]  &     LEV =   [200.,300,20] & SCALE=1.0 & END
 belongsto(var, ['QDIFF'    ]) : BEGIN DIFF = [-5 ,5  ,20  ]  &     LEV =   [0.,20,20]    & SCALE=scale_Q & END

 belongsto(var, ['U10'   ]): BEGIN DIFF = [-5 ,5  , 20] &     LEV =   [0,15 ,20]     & SCALE=1.0 & END
 belongsto(var, ['V10'   ]): BEGIN DIFF = [-5 ,5  , 20] &     LEV =   [-15,15 ,20]   & SCALE=1.0 & END
 belongsto(var, ['WGUSTD']): BEGIN DIFF = [-5 ,5  , 20] &     LEV =   [0,1,20]       & SCALE=1.0 & END    ;1/10 FNL


 belongsto(var, ['CMFMCDZM','CMFMC'  ]) : BEGIN DIFF = [ -5, 5 , 20] &     LEV =   [0.,40,20]  & SCALE=1000. & END

 belongsto(var, ['ZMMD','ZMMU'   ]): BEGIN DIFF = [-5 ,5  , 20] &     LEV =   [0.,40,20]  & SCALE=-1000. & END

 belongsto(var, ['FDR_CU','FER_CU', 'FDR_CU', 'FER_CU',  'UFRC_CU',  'WU_CU'  ]) : BEGIN DIFF = [-2 ,2  , 20] &     LEV =   [0.,10,20]  & SCALE=1. & END

 belongsto(var, ['KVH', 'UW_KVH', 'KVM' , 'UW_KVM']): BEGIN DIFF = [-2 ,2  , 20] &     LEV =  [0.,20,20]  & SCALE=1.0e-3& END 

 belongsto(var, ['ICIMR', 'ICWMR' ,'IWC']): BEGIN DIFF = [-2 ,2  , 20] &     LEV =   [0.,20,20]    & SCALE=scale_liq & END


 belongsto(var, ['UW_LENG'   ]): BEGIN DIFF = [-100 ,100  , 20] &     LEV =   [0.,1000,20]  & SCALE=1. & END

 belongsto(var, ['EMISCLD']): BEGIN DIFF = [-0.2 ,0.2  , 20] & LEV =   [0.,2,20]    & SCALE=10000.0 & END
 belongsto(var, ['FICE', 'FREQI', 'FREQL', 'FREQR','FREQS']): BEGIN DIFF = [-0.1 , 0.1 , 20] &     LEV =   [0.,1,20]    & SCALE=1.0 & END

 belongsto(var, ['PV']): BEGIN DIFF = [-50 , 50 , 20] &     LEV =   [-100.,100,20]    & SCALE=1.0 & END
 belongsto(var, ['AVOR']): BEGIN DIFF = [-5 , 5 , 20] &     LEV =   [-20.,20,20]    & SCALE=1.0e5 & END
 belongsto(var, ['VOR']): BEGIN DIFF = [ -2, 2 , 20] &     LEV =   [-5.,5,20]    & SCALE=1.0e6 & END
 belongsto(var, ['VOR']): BEGIN DIFF = [ -2, 2 , 20] &     LEV =   [-5.,5,20]    & SCALE=1.0e6 & END
 belongsto(var, ['DIV']): BEGIN DIFF = [-2 , 2 , 20] &     LEV =   [-5.,5,20]    & SCALE=1.0e7 & END
 belongsto(var, ['ANRAIN', 'ANSNOW', 'AQRAIN', 'AQSNOW']): BEGIN DIFF = [-2 ,2  , 20] &     LEV =   [0.,10,20]    & SCALE=1.0e5 & END
 belongsto(var, ['AREI']): BEGIN DIFF = [-10 , 10 , 20]    &     LEV =   [0.,200,20]    & SCALE=1.0 & END
 belongsto(var, ['AREL']): BEGIN DIFF = [-2 , 2 , 20] &     LEV =   [0.,20,20]    & SCALE=1.0 & END
 belongsto(var, ['AWNC', 'AWNI']): BEGIN DIFF = [-2 , 2 , 20] &     LEV =   [0.,10,20]    & SCALE=1.0e-6 & END
 belongsto(var, ['UU','VV']): BEGIN DIFF = [-10 , 10 , 20]  &     LEV =   [0.,120,20]    & SCALE=1.0 & END
 belongsto(var, ['VQ','UQ']): BEGIN DIFF = [-10 , 10 , 20]   &     LEV =   [-100,100,20]    & SCALE=1.0e3 & END
 belongsto(var, ['VT','UT']): BEGIN DIFF = [-5 , 5 , 20]   &     LEV =   [-30.,30,20]    & SCALE=1.0/300 & END
 belongsto(var, ['NUMW', 'NUMI',  'NUMICE', 'NUMLIQ' ]) : BEGIN DIFF = [-10 ,10  , 20] &     LEV =   [1.,100,20]   & SCALE=scale_cldni & END   ;/G
 belongsto(var, ['WSUB',  'WSUBI'  ]): BEGIN DIFF = [-0.1 , 0.1 , 20] &     LEV =   [-1,1,20]     & SCALE=1.0 & END   ;M/S
 belongsto(var, ['TKE', 'UW_TKE', 'TKE_CU']): BEGIN DIFF = [-0.1 , 0.1 , 20] &     LEV =   [0.,2,20]      & SCALE=1.0 & END
 belongsto(var, ['RI']): BEGIN DIFF = [-0.2 ,0.2  , 20] &     LEV =   [-4.,36,20]    & SCALE=1.0e-2 & END
 belongsto(var, ['DUDZ']): BEGIN DIFF = [-1 , 1 , 20] &     LEV =   [-10.,10,20]    & SCALE=1.0e3 & END
 belongsto(var, ['DNDZ']): BEGIN DIFF = [-0.5 , 0.5 , 20] &     LEV =   [-5.,5,20]    & SCALE=1.0e7 & END
 belongsto(var, ['ANRAIN','ANSNOW','AQRAIN','AQSNOW']): BEGIN DIFF = [-1 , 1 , 20] &     LEV =   [0.,10,20]    & SCALE=1.0e-5 & END
 belongsto(var, ['AREI','AREL']): BEGIN DIFF = [-10 ,10  , 20] &     LEV =   [0.,200,20]   & SCALE=1.0 & END
 belongsto(var, ['AWNC','AWNC']): BEGIN DIFF = [-10 , 10 , 20] &     LEV =   [0.,100,20]    & SCALE=1.0e-6 & END
 belongsto(var, ['DUV','DVV', 'ZMMTU' , 'ZMMTV'   ]): BEGIN DIFF = [ -1, 1 , 20] &     LEV =   [-10.,10,20]  & SCALE=scale_dtdt & END

;  (not belongsto(var, ['XXXXX'])): begin 
  ELSE: begin 

        end
  ENDCASE


 lev00 = lev
 diff00 = diff 

;;print,var, lev, diff
  levj = lev+0.0
  diffj = diff+0.0
  lev = levj
  diff = diffj


  if(lev[2] gt 40)then lev = levj
  if(diff[2] gt 40)then diff = diffj

  ; lev2_nn
  ; determine how many line intervals and whether to do lines
  ; lev2_nn < 0,  nn=2 every two lines for the contour overplot
  ;  lev2_nn = 2  ; default

lev2_nn = 1

 if((strpos(var, 'PREC') eq 0 ) or (strpos(var, 'CLD') ge 0) or (strpos(var,'CRF') ge 0) $
       or (strpos(var,'sn') eq 0) or (strpos(var, 'sbl') ge 0)    $
       or (strpos(var, 'mr') eq 0) or (strpos(var,'CLOUDXXX') eq 0) or (strpos(var,'CLDLIQ') eq 0) $
       or (strpos(var,'CLDICE') eq 0)  or (strpos(var, 'OMEGA') eq 0) $
       or (strpos(var,'SWCF') ge 0)  or (strpos(var, 'LWCF') ge 0) $
       or (strpos(var,'SWCRF') ge 0)  or (strpos(var, 'LWCRF') ge 0) $
       or (belongsto(var, ['LHFLX','SHFLX', 'LANDFRAC','PME','QFLX']))  $
       or ( belongsto(var, ['sbl','TAUX','TAUY','CI']))  $
         ) $  
        then lev2_nn = -1  ; no lines added



  lev2 = lev
  diff2 = diff
;print,var, lev, diff

; true levels
  lev  = cal_lev(lev[0:1],lev[2]) 
  diff = cal_lev(diff[0:1],diff[2]) 

 lev_min = 0*lev[0]
 lev_max = 0*lev[1]

 if(belongsto(var, ['H','THETA']))then lev_max = 400
 if(belongsto(var, ['H','THETA']))then lev_max_diff = 12

adjust_lev = adjust
if(belongsto(var,['TAUX','TAUY','sbl','SST', 'snw', 'Z31000','Z3850','Z3500', $
           'OMEGA1000','OMEGA850','OMEGA500','OMEGA200']))then adjust_lev=0
if (strpos(var, 'mr') eq 0) then adjust_lev = 0
if (strpos(var, 'OMEGA') eq 0) then adjust_lev = 0

;print,lev
 if(adjust_lev) then begin
   ddj = dd*scale
   lev = get_lev_adjust( ddj, LEV, nn=nn2, lev_max=lev_max, lev_min = lev_min, missing = missing)
 ;===================================
 endif

 adjust_diff = adjust_lev

adjust_diff = 0

 if(adjust_diff) then begin
   ddj = dd*scale

   diff = get_lev_adjust( ddj, diff, nn=nn2, lev_max=lev_max_diff, lev_min = lev_min_diff, missing = missing)
 ;===================================
 endif


lev2 = lev
diff2 = diff

 
 if(lev2_nn eq -1)then begin
    lev2 = [1.e10, 1.e20]
    diff2 = [1.e10, 1.e20]
    endif

 if(lev2_nn gt 0) then begin
   lev2 = array_spaced(lev, lev2_nn)
   diff2 = array_spaced(diff, lev2_nn)
  endif


;print,lev

;==============================

 if(keyword_set(get_diff))then begin

  lev = diff
  lev2 = diff2

  mx = diff00[-1] & mn = diff00[0]
;  mx = diff[-1] & mn = diff[0]
  mm = (abs(mx)+abs(mn) )/2.

;    mm = max([abs(mn),abs(mx)])
  m1 = -mm
  m2 =  mm
;;;; ;  diff = cal_lev([m1,m2],nn2)

  if(belongsto(var,['Q','QS','QREFHT','Q1000','Q850','Q500','Q200', $
               'Z3','Z31000','Z3850','Z3500','Z3200', $
               'FLDS','FLDSC','FLUS','FLUT','FLUTC','ATMNET', $
                'SRFNET','SRFRAD','SRFRADC','TOARAD','TOARADC','TOTCRF', $
                'TOTCRFA','TOTCRFS'])    ) then begin
   endif

 
  if(belongsto(var,[ $
               'T','TS','SST','TREFHT','T1000','T850','T500','T200'$
                ]    )) then begin
    diff = cal_lev([-10, 10.], 40)
  endif

  if(belongsto(var,[ $
               'H','H1000','H850','H500','H200',   $
               'THETA','THETA1000','THETA850','THETA500','THETA200' $
                ])    ) then begin 
    diff = cal_lev([-18, 18.], 36)
   endif

  if(belongsto(var,[ $
               'OMEGA1000','OMEGA850','OMEGA500','OMEGA200' $
                ])    ) then begin
    diff = cal_lev([-2, 2.], 20)
  endif
  if(belongsto(var,[ $
               'Q1000','Q850','Q500','Q200' $
                ])    ) then begin
    diff = cal_lev([-6, 6.], 24)
   endif
  if(belongsto(var,[ $
               'CLDLIQ','CLDLIQ1000','CLDLIQ850','CLDLIQ500','CLDLIQ200' $
                ])    ) then begin
    diff = cal_lev([-2, 2.], 20)
  endif

  if(belongsto(var,[ $
               'CLDICE','CLDICE1000','CLDICE850','CLDICE500','CLDICE200' $
                ])    ) then begin
    diff = cal_lev([-0.5, 0.5], 20)
  endif


  lev = diff
  lev2 =  array_spaced(diff, 2)

 endif
; === get_diff
;stop


 dd = ddd
 
  return, LEV

end
 

