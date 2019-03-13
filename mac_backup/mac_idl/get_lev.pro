
;==========================
Pro get_lev,dd, VAR,LEV,SCALE

SCALE=1.
SCALE_PRECIP = 1000*86400.
SCALE_Q      = 1000.
scale_omega = 3600 /100.  ;mb/hr
scale_dqdt=   1.0e3*86400 ; g/kg/day
scale_dtdt =  86400. ; T/day
scale_liq =   1.0e4  ; 10^-2 g/kg
scale_cldnl=  1.0e-6 ; cm-3
scale_cldni=  1.0e-6 ;
scale_tgliq = 1.0e6   ;mm
scale_p =     1.0e-2  ;mb


  CASE strupcase(VAR) OF

 'D2RHDZ': BEGIN LEV =  CAL_LEV([-60.,60],20)    & SCALE=1.0e3 & END
 'DRHDZ': BEGIN LEV =  CAL_LEV([-50.,50],20)    & SCALE=1.0e3 & END
 'RI': BEGIN LEV =  CAL_LEV([-4.,36],20)    & SCALE=1.0e-2 & END
 'DUDZ': BEGIN LEV =  CAL_LEV([-10.,10],20)    & SCALE=1.0e3 & END
 'PV': BEGIN LEV =  CAL_LEV([-100.,100],20)    & SCALE=1.0 & END
 'AVOR': BEGIN LEV =  CAL_LEV([-20.,20],20)    & SCALE=1.0e5 & END
 'VOR': BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=1.0e6 & END
 'DNDZ': BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=1.0e7 & END
 'NN':   BEGIN LEV =  CAL_LEV([-5.,55],20)    & SCALE=1.0e5 & END
 'ANRAIN': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e-5 & END
 'ANSNOW': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e-5 & END
 'AQRAIN': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e5 & END
 'AQSNOW': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e5 & END
 'AREI': BEGIN LEV =  CAL_LEV([0.,200],20)    & SCALE=1.0 & END
 'AREL': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=1.0 & END
 'AWNC': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=1.0e-6 & END
 'AWNI': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e-6 & END
 'AIST': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'ALST': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'AST': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'CLDSH': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'CLDDP': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'U00': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'DRH00': BEGIN LEV =  CAL_LEV([-50.,50],20)    & SCALE=100.0 & END
 'CLDST': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'LCLOUD': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'CUFRAC_CU': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'CLOUD': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'LIQCLDF': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'CONCLD': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=100.0 & END
 'CCN3': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0 & END
 'DLF'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'DPDLFICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'DPDLFLIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END

 'MACPDQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'MACPDLIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'MACPDICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'CMFICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'CMFLIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'CMFDQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'SHDLFLIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'SHDLFICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'CMFDQR'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'CME'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'CMELIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'EVAPPREC'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'EVAPQZM'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'EVAPSNOW'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'ZMDICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'ZMDLF'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'ZMDLQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END

 'TANUMICE'   : BEGIN LEV =  CAL_LEV([-2.,2],20)  & SCALE= 86400/1.0e6 & END
 'TANUMLIQ'   : BEGIN LEV =  CAL_LEV([-2.,2],20)  & SCALE= 86400/1.0e6 & END

 'TAQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'TACLDICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'TACLDLIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'PTEQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'PTECLDICE'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'PTECLDLIQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'PRODPREC'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'QITEN_CU'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'QLTEN_CU'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'QVTEN_CU'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'QTTEN_CU'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END

 'ZMDQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'VD01'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'DLF_PLEV'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'DTCOND'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'PTTEND'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'SHDLFT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'CMFDT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'MACPDT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'EVAPTZM'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'EVSNTZM'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'FZSNTZM'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'HPROGCLD'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'ZMMTT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'ZMDT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'DTCORE': BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'DPDLFT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'DTH'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'DTV'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'TTEND'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'TTGWORO'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'TTEND_TOT'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'CMFMCDZM'   : BEGIN LEV =  CAL_LEV([0.,40],20)  & SCALE=1000. & END
 'CMFMC'   : BEGIN LEV =  CAL_LEV([0.,40],20)  & SCALE=1000. & END
 'ZMMD'   : BEGIN LEV =  CAL_LEV([0.,40],20)  & SCALE=-1000. & END
 'ZMMU'   : BEGIN LEV =  CAL_LEV([0.,40],20)  & SCALE=1000. & END

 'FDR_CU'   : BEGIN LEV =  CAL_LEV([0.,2],20)  & SCALE=1.0e-4 & END
 'FER_CU'   : BEGIN LEV =  CAL_LEV([0.,2],20)  & SCALE=1.0e-4 & END

 'UFRC_CU'   : BEGIN LEV =  CAL_LEV([0.,20],20)  & SCALE=100. & END
 'WU_CU'   : BEGIN LEV =  CAL_LEV([0.,10],20)  & SCALE=1. & END

 'KVH'   : BEGIN LEV =  CAL_LEV([0.,20],20)  & SCALE=1.0e-3& END
 'UW_KVH'   : BEGIN LEV =  CAL_LEV([0.,20],20)  & SCALE=1.0e-3& END
 'KVM'   : BEGIN LEV =  CAL_LEV([0.,20],20)  & SCALE=1.0e-3& END
 'UW_KVM'   : BEGIN LEV =  CAL_LEV([0.,20],20)  & SCALE=1.0e-3& END
 'DUV'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'DVV'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'ZMMTU'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END
 'ZMMTV'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dtdt & END


 'UW_LENG'   : BEGIN LEV =  CAL_LEV([0.,1000],20)  & SCALE=1. & END

 'EMISCLD': BEGIN LEV =  CAL_LEV([0.,2],20)    & SCALE=10000.0 & END
 'FICE': BEGIN LEV =  CAL_LEV([0.,1],20)    & SCALE=1.0 & END
 'FREQI': BEGIN LEV =  CAL_LEV([0.,1],20)    & SCALE=1.0 & END
 'FREQL': BEGIN LEV =  CAL_LEV([0.,1],20)    & SCALE=1.0 & END
 'FREQR': BEGIN LEV =  CAL_LEV([0.,1],20)    & SCALE=1.0 & END
 'FREQS': BEGIN LEV =  CAL_LEV([0.,1],20)    & SCALE=1.0 & END
 'ICIMR': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_liq & END
 'ICWMR': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_liq & END
 'IWC': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_liq & END
 'MSEUP':           BEGIN LEV =  CAL_LEV([300.,350],20)    & SCALE=1.0e-3 & END
 'MSE':           BEGIN LEV =  CAL_LEV([320.,360],20)    & SCALE=1.0e-3 & END
 'MSESAT':        BEGIN LEV =  CAL_LEV([320.,360],20)    & SCALE=1.0e-3 & END
 'MSESAT_PLEV':   BEGIN LEV =  CAL_LEV([320.,360],20)    & SCALE=1.0e-3 & END
 'MSESAT_UPPLEV': BEGIN LEV =  CAL_LEV([320.,360],20)    & SCALE=1.0e-3 & END
 'MSE_PLEV':      BEGIN LEV =  CAL_LEV([320.,360],20)    & SCALE=1.0e-3 & END
 'PMID': BEGIN LEV =  CAL_LEV([0.,1000],20)    & SCALE=1.0e-2 & END
 'Z3': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=1.0e-3 & END
 'UU': BEGIN LEV =  CAL_LEV([0.,120],20)    & SCALE=1.0 & END
 'VV': BEGIN LEV =  CAL_LEV([0.,120],20)    & SCALE=1.0 & END
 'VQ': BEGIN LEV =  CAL_LEV([-100,100],20)    & SCALE=1.0e3 & END
 'VT': BEGIN LEV =  CAL_LEV([-30.,30],20)    & SCALE=1.0/300 & END
 'UT': BEGIN LEV =  CAL_LEV([-30.,30],20)    & SCALE=1.0/300 & END
; '': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=1.0 & END
 'ACTREL': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=1.0 & END
 'ACTREI': BEGIN LEV =  CAL_LEV([0.,160],20)   & SCALE=1.0 & END
 'TDIFF'     : BEGIN LEV =  CAL_LEV([-5.,5.],20) & SCALE=1.0 & END
 'TDIFFPLEV'     : BEGIN LEV =  CAL_LEV([200.,300],20) & SCALE=1.0 & END
 'T'     : BEGIN LEV =  CAL_LEV([200.,300],20) & SCALE=1.0 & END
 'TPLEV'     : BEGIN LEV =  CAL_LEV([200.,300],20) & SCALE=1.0 & END
 'OFFT'     : BEGIN LEV =  CAL_LEV([200.,300],20) & SCALE=1.0 & END
 'OMEGA'     : BEGIN LEV =  CAL_LEV([-30.,30],20) & SCALE=scale_omega & END
 'OMEGAT'     : BEGIN LEV =  CAL_LEV([200.,300],20) & SCALE=scale_omega/300. & END
 'OFFTPLEV'     : BEGIN LEV =  CAL_LEV([200.,300],20) & SCALE=1.0 & END
 'QDIFF'     : BEGIN LEV =  CAL_LEV([-2.,2],20)    & SCALE=scale_Q & END
 'QDIFFPLEV'     : BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_Q & END
 'OFFQ'     : BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_Q & END
 'OFFQPLEV'     : BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_Q & END
 'Q'     : BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_Q & END
 'QS'     : BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_Q & END
 'QPLEV'     : BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=scale_Q & END
 'U'     : BEGIN LEV =  CAL_LEV([-30.,30],20)  & SCALE=1.0 & END
 'V'     : BEGIN LEV =  CAL_LEV([-20.,20],20)  & SCALE=1.0 & END
 'RH'    : BEGIN LEV =  CAL_LEV([0.,100],20)   & SCALE=1.0 & END
 'W'     : BEGIN LEV =  CAL_LEV([-20.,20],20)  & SCALE=scale_omega & END
 'QTENDCONVDP'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'DCQ'   : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'DQ'    : BEGIN LEV =  CAL_LEV([-10.,10],20)  & SCALE=scale_dqdt & END
 'STENDCONVDPPLEV'    : BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=scale_dtdt & END
 'STENDCONVDP'    : BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=scale_dtdt/1004. & END  ;?
 'QRL'    : BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=scale_dtdt & END
 'QRS'    : BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=scale_dtdt & END
 'DT'    : BEGIN LEV =  CAL_LEV([-5.,5],20)    & SCALE=scale_dtdt & END
 'CLDLIQ': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDICE': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDICECON': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDICEDET': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDICESTR': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDLIQCON': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDLIQDET': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'CLDLIQSTR': BEGIN LEV =  CAL_LEV([0,3],20)      & SCALE=scale_liq & END
 'NUMW'  : BEGIN LEV =  CAL_LEV([1.,1000],20)  & SCALE=scale_cldnl & END
 'NUMI'  : BEGIN LEV =  CAL_LEV([1.,100],20)   & SCALE=scale_cldni & END   ;/G
 'NUMICE'  : BEGIN LEV =  CAL_LEV([1.,100],20)   & SCALE=scale_cldni & END   ;/G
 'NUMLIQ'  : BEGIN LEV =  CAL_LEV([1.,100],20)   & SCALE=scale_cldni & END   ;/G
 'WSUB'  : BEGIN LEV =  CAL_LEV([-1,1],20)     & SCALE=1.0 & END   ;M/S
 'WSUBI'  : BEGIN LEV =  CAL_LEV([-1,1],20)     & SCALE=1.0 & END   ;M/S

 'CAPE'  : BEGIN LEV =  CAL_LEV([0.,2000],20)  & SCALE=1.0 & END
 'CIN'   : BEGIN LEV =  CAL_LEV([0.,1000],20)  & SCALE=1.0 & END
 'CLD'   : BEGIN LEV =  CAL_LEV([0.,1],20)     & SCALE=1.0 & END
 'CRF'   : BEGIN LEV =  CAL_LEV([-400,200],20) & SCALE=1.0 & END
 'FLNSC' : BEGIN LEV =  CAL_LEV([0,200 ],20)   & SCALE=1.0 & END
 'FLNT'  : BEGIN LEV =  CAL_LEV([0,300 ],20)   & SCALE=1.0 & END
 'FSDS'  : BEGIN LEV =  CAL_LEV([50,450 ],20)  & SCALE=1.0 & END
 'FSNT'  : BEGIN LEV =  CAL_LEV([0,500 ],20)   & SCALE=1.0 & END
 'LHFLX' : BEGIN LEV =  CAL_LEV([0,300 ],20)   & SCALE=1.0 & END
 'LWCF'  : BEGIN LEV =  CAL_LEV([0.,100],20)   & SCALE=1.0 & END
 'PBLH'  : BEGIN LEV =  CAL_LEV([100,1500],20) & SCALE=1.0 & END
; 'PRECT' : BEGIN LEV =  CAL_LEV([0.,15],20)   & SCALE=scale_precip & END
 'PRECT' : BEGIN LEV =  CAL_LEV([0.,50],20)    & SCALE=scale_precip & END  ;TEMPO
 'RELHUM' : BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=1 & END  ;TEMPO
 'RHI' : BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=1 & END  ;TEMPO
 'RHCFMIP' : BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=1 & END  ;TEMPO

 'PRECDP': BEGIN LEV = CAL_LEV([0.,50],20)     & SCALE = scale_precip & END
 'PRECSH': BEGIN LEV = CAL_LEV([0.,15],20)     & SCALE = scale_precip & END
 'PRECC': BEGIN LEV =  CAL_LEV([0.,50],20)     & SCALE = scale_precip & END
 'PRECZ' : BEGIN LEV =  CAL_LEV([0.,50],20)    & SCALE=scale_precip & END
 'PRECCDZM' : BEGIN LEV =  CAL_LEV([0.,50],20)    & SCALE=scale_precip & END
 'PRECL' : BEGIN LEV =  CAL_LEV([0.,15],20)    & SCALE=scale_precip & END
 'PRECSL': BEGIN LEV =  CAL_LEV([0.,15],20)    & SCALE=scale_precip & END
 'PS'    : BEGIN LEV =  CAL_LEV([970.,1050.],20)  & SCALE=1.0 & END
 'QREFHT': BEGIN LEV =  CAL_LEV([0.,20.],20)   & SCALE=1.0 & END
 'SHFLX' : BEGIN LEV =  CAL_LEV([0,120 ],20)   & SCALE=1.0 & END
 'SWCF'  : BEGIN LEV =  CAL_LEV([-300.,0],20)  & SCALE=1.0 & END
 'TAUX'  : BEGIN LEV =  CAL_LEV([-0.3,0.3 ],20) & SCALE=1.0 & END
 'TGCLDIWP' : BEGIN LEV =  CAL_LEV([0,300 ],20)  & SCALE=scale_tgliq & END
 'TGCLDLWP' : BEGIN LEV =  CAL_LEV([0,300 ],20)  & SCALE=scale_tgliq & END
 'TGCLDCWP' : BEGIN LEV =  CAL_LEV([0,300 ],20)  & SCALE=scale_tgliq & END   ;!! TEMP
 'TMQ'   : BEGIN LEV =  CAL_LEV([0.,60],20)     & SCALE=1.0 & END
 'TS'    : BEGIN LEV =  CAL_LEV([0,30],20)      & SCALE=1.0 & END
 'U10'   : BEGIN LEV =  CAL_LEV([0,15 ],20)     & SCALE=1.0 & END
 'V10'   : BEGIN LEV =  CAL_LEV([-15,15 ],20)   & SCALE=1.0 & END
 'VRATE' : BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=1.0 & END  ;1/1000 FNL
 'WGUSTD': BEGIN LEV =  CAL_LEV([0,1],20)       & SCALE=1.0 & END    ;1/10 FNL

 'PRES'  : BEGIN LEV =  CAL_LEV([100,1000],18)  & SCALE=scale_p & END
 'K'     : BEGIN LEV =  CAL_LEV([0,30],20)      & SCALE=1.0 & END
 'TKE': BEGIN LEV =  CAL_LEV([0.,2],20)      & SCALE=1.0 & END
 'UW_TKE': BEGIN LEV =  CAL_LEV([0.,2],20)      & SCALE=1.0 & END
 'TKE_CU': BEGIN LEV =  CAL_LEV([0.,2],20)      & SCALE=1.0 & END
 'CLD_CU': BEGIN LEV =  CAL_LEV([0,0.2],20)     & SCALE=1.0 & END

 'EU0': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
 'DU0': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
 'ED0': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
 'DD0': BEGIN LEV =  CAL_LEV([0,20.],20)/10.     & SCALE=100.0 & END

 'EU1': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
 'DU1': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
 'ED1': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
 'DD1': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=100.0 & END
  'BUOY_UP': BEGIN LEV =  CAL_LEV([-10,10.],20)     & SCALE=1.0 & END
  'BUOY_DN': BEGIN LEV =  CAL_LEV([-10,10.],20)     & SCALE=1.0 & END
  'CLDSR': BEGIN LEV =  CAL_LEV([0,10.],20)     & SCALE=1.0 & END

 'ENT_ORG': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.e5 & END
 'ENT_TURB': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.0e5 & END
 'ENT_TOT': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.0e5 & END
 'DET_ORG': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.0e5 & END
 'DET_TURB': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.0e5 & END
 'DET_TOT': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.0e5 & END

 'WTKE': BEGIN LEV =  CAL_LEV([0,10.],20)     & SCALE=10.0 & END
 'EPS0': BEGIN LEV =  CAL_LEV([0,22.],22)     & SCALE=1.0e5 & END
 'MU0': BEGIN LEV =  CAL_LEV([0,40.],20)     & SCALE=10.0 & END
 'MU1': BEGIN LEV =  CAL_LEV([0,40.],20)     & SCALE=10.0 & END
 'MD0': BEGIN LEV =  CAL_LEV([0,40.],20)     & SCALE=-10.0 & END
 'MD1': BEGIN LEV =  CAL_LEV([0,40.],20)     & SCALE=-10.0 & END
 'W_UP': BEGIN LEV =  CAL_LEV([0,20.],20)     & SCALE=1.0 & END

  ELSE: begin 
       jj = where(dd gt -9990.,cnt)
       if(cnt gt 0)then lev = cal_lev([min(dd[jj]),max(dd[jj])],20)     
        end
  ENDCASE

  return

end
 

