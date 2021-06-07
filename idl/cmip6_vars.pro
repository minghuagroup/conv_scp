
function cmip6_vars
;========================== 


;dictionary between cmip atm and cam
vars_atm = 'ci cli clw clt cl co2mass rlds rldscs rlus rlut rlutcs rsds rsdscs rsdt rsus rsuscs rsut rsutcs '+$
            'hfls wap prc prsn pr ps psl hus huss hur hurs rtmt hfss ta tauu tauv '+ $
            'clivi clwvi prw tas ts tasmin tasmax ua sfcWind va zg evspsbl sbl'
vars_atm = str_sep(strcompress(vars_atm),' ')

;------
vars_cam = 'CI CLDICE CLDLIQ CLDTOT CLOUD CO2 FLDS FLDSC FLUS FLNT FLNTC FSDS FSDSC SOLIN FSUS FSUSC FSUT FSUTC '+ $
            'LHFLX OMEGA PRECC PRECSC PRECT PS PSL Q QREFHT RELHUM RELHUMS RESTOM SHFLX T TAUX TAUY '+$
            'TGCLDIWP TGCLDLWP TMQ TREFHT TS TSMN TSMX U U10 V Z3 evspsbl sbl'
vars_cam = str_sep(strcompress(vars_cam),' ')

;------
vars_add = 'ATMNET ATMRAD ATMRADC CLDHGH CLDLOW CLDMED FLNA FLNAC FLNS FLNSC FLNT FLNTC FSNA FSNAC ' + $
           'FSNS FSNSC FSNT FSNTC FSNTOA FSNTOAC FSUTOA H ICEFRAC LANDFRAC LWCF LWCFA LWCFS ORO PHIS ' + $
           'PME PRECL PRECSL QFLX SNOWHLND SOLIN SRFNET SRFRAD SRFRADC SST SWCF SWCFA SWCFS ' + $
           'TGCLDCWP THETA TOARAD TOARADC TOTCRFA TOTCRF TOTCRFS'
vars_add = str_sep(strcompress(vars_add),' ')


;/Amon
jj       =  sort(vars_cam)
vars_cam = vars_cam[jj]
vars_atm = vars_atm[jj]

;/Omon
vars_ocn =  ['tos','sos','zos']

;/Lmon
vars_lnd = str_sep('mrso mrsos mrfsp mrro mrros',' ')

;/LImon
vars_lice =  str_sep('snc snd snw snm',' ')

;/SImon
vars_sice = ['sithick', 'siconc']

;/fx
vars_fx = ['orog', 'sftlf']

vars_delete = ['orog', 'sftlf','tos','siconc','snc']  ; redundant with PHIS and LANDFRAC, SST, 
                                 ;'ICEFRAC','SNOWHLND'

vars_cmip6 = [vars_atm, vars_ocn, vars_lnd, vars_lice, vars_sice, vars_fx]

; --- to translate to CESM fields
; --------------------------------


vars_cam_all   =  [vars_cam,vars_add]
jj             =  sort(vars_cam_all)
vars_cam_all   = vars_cam_all[jj]
vars_cesm_all  = [vars_cam_all, vars_ocn, vars_lnd, vars_lice, vars_sice, vars_fx]

vars3d  = ['CLDLIQ','CLDICE','CLOUD','T','Q','U','V','Z3','RELHUM','H','THETA','OMEGA']

cmip6_vars = create_struct('vars_atm', vars_atm   ,'vars_ocn', vars_ocn,  'vars_lnd', vars_lnd,  $
  'vars_lice', vars_lice,  'vars_sice', vars_sice,  'vars_fx', vars_fx,  'vars_cmip6', vars_cmip6, $
  'vars_cam', vars_cam,  'vars_add', vars_add, 'vars_cam_all', vars_cam_all,  $
  'vars_cesm_all', vars_cesm_all,  'vars3d', vars3d  ,'vars_delete',vars_delete )


 ; find duplicates of low case and up case fields
 vars_low   = strarr(200)
 vars_up    = strarr(200)



return,cmip6_vars

;============================
;vars_atm     = ['cl',     'clw',      'hfls',  'hus',        'pr',    'ps',      'rlus',    'rsdscs', $
;             'rsut',    'sfcWind', 'tasmin',  'ua', 'cli',    'clwvi',    'hfss',  'huss',       'prc', $
;             'psl',     'rlut',    'rsdt',    'rsutcs',  'ta',       'tauu',    'va', $
;              'clivi',  'co2mass',  'hur',   'prsn',  'rlds',    'rlutcs',  'rsus',    'rtmt',   $
;             'tas',      'tauv',    'wap',   'ci', 'clt',    'evspsbl',  'hurs',  'prw',   'rldscs', $
;             'rsds',    'rsuscs',  'sbl',     'tasmax',   'ts',      'zg']

;vars_cam = ['CLOUD','CLDLIQ' ,'LHFLX', 'Q', 'PRECT','PS','FLUS' ,'FSDSC',                 $  ;PRECT total only
;             'FSUT', 'U10', 'TSMN',   'U', 'CLDICE', 'TGCLDLWP','SHFLX', 'QREFHT' ,'PRECC',   $  ; QS
;             'PSL', 'FLUT', 'FSDT' , 'FSUTC', 'T', 'TAUX', 'V',                           $  ; 
;             'TGCLDIWP', 'CO2', 'RELHUM', 'PRECSC', 'FLDS', 'FLUTC', 'FSUS', 'RESTOM',      $ ; CO2 
;             'TREFHT', 'TAUY', 'OMEGA', 'CI', 'CLDTOT', 'SBL','RELHUMS', 'TMQ','FLDSC',$ ; evspsbl set to QFLX
;             'FSDS', 'FSUSC', 'sbl', 'TSMX', 'TS', 'Z3'  ]  ; sbl to ICEFRAC



;vars_add = 'FSNT, FSNTC, FSNS, FSNSC, FLNS, FLNSC, FLNT, FLNTC,SRFRAD, FSNTOA, FSNTOAC, FSUTOA,' + $
;           'SRFRADC, TOARAD, TOARADC,LWCF, SWCF, LWCFS, SWCFS, TOTCRF, TOTCRFS,' + $ 
;           'FSNA, FLNA, FSNAC, FLNAC,,LWCFA,SWCFA, TOTCFA,ATMRAD,ATMRADC,ATMNET,' + $
;           'PME, SRFNET, PRECL, PRECSL, PRECSC, SOLIN, H, THETA,PHIS, LANDFRAC,ORO,QFLX,SST,' + $
;           'CLDHGH, CLDLOW, CLDMED, TGCLDCWP, ICEFRAC,SNOWHLND' 

;vars_add = strtrim(str_sep(vars_add,','),2)

; selected cmip6 fields
end ;============================
