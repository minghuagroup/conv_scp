
;cmip6_macro

; 1 ======= Models

 Models0 = ['AS-RCEC',  'BCC',   'CAS',    'CCCR-IITM',  'CNRM-CERFACS',  'CSIRO-ARCCSS', $
      'EC-Earth-Consortium',  'HAMMOZ-Consortium',  'IPSL',   'MIROC',  'MPI-M',  'NASA-GISS',$
      'NCC',       'NOAA-GFDL',  'SNU',  'UA', 'AWI',      'CAMS',  'CCCma',  'CMCC',       $
      'CSIRO',         'E3SM-Project',  'FIO-QLNM',             'INM',                'KIOST', $
      'MOHC',   'MRI',    'NCAR',       'NIMS-KMA',  'NUIST',      'THU']


 dir = '/glade/collections/cmip/CMIP6/CMIP/'
 spawn, 'ls ' + dir, Models0
 Institutions = Models0

 ; take care of multi models from same institution
 Models = strarr(1000)
  k = 0
 for im = 0, n_elements(models0)-1 do begin ; 
  spawn, 'ls ' + dir+ Models0[im] +'/', listing
  if(strlen(listing[0]) ge 1) then begin
  for i=0,n_elements(listing)-1 do begin
    print,'im,k', im, ' ', k, ' ', models0[im], '/', listing[i]
    Models[k] = Models0[im] + '/' + listing[i]
    k = k + 1
  endfor
  endif
 endfor 
 Models_all = Models[0:k-1]

 ; manually exclude models
 Models_Exclude = 'AWI/AWI-ESM-1-1-LR BCC/BCC-ESM1 CCCma/CanESM5-CanOE CMCC/CMCC-CM2-HR4 CNRM-CERFACS/CNRM-CM6-1-HR '+ $
                  'CNRM-CERFACS/CNRM-ESM2-1 CSIRO/ACCESS-ESM1-5 EC-Earth-Consortium/EC-Earth3-AerChem ' + $
                  'EC-Earth-Consortium/EC-Earth3-CC EC-Earth-Consortium/EC-Earth3-LR EC-Earth-Consortium/EC-Earth3-Veg '+ $
                  'EC-Earth-Consortium/EC-Earth3-Veg-LR INM/INM-CM4-8 MIROC/MIROC-ES2L MOHC/HadGEM3-GC31-LL MOHC/UKESM1-0-LL '+$
                  'MPI-M/MPI-ESM1-2-LR NASA-GISS/GISS-E2-1-G NASA-GISS/GISS-E2-1-G-CC NCAR/CESM2-FV2 NCAR/CESM2-WACCM ' + $
                  'NCAR/CESM2-WACCM-FV2 NCC/NorESM1-F NCC/NorESM2-LM NCC/NorESM2-MM NIMS-KMA/UKESM1-0-LL NOAA-GFDL/GFDL-AM4 '+ $
                  'NOAA-GFDL/GFDL-ESM4' 
 Models_Exclude = str_sep(Strcompress(Models_Exclude),' ')
 k = 0
 for i = 0, n_elements(Models_all)-1 do begin
   if(not belongsto(Models_all[i], Models_Exclude)) then begin
     Models[k] = Models_all[i]
     k = k+1
   endif
 endfor
 Models = Models[0:k-1]

 n1 = n_elements(models_all)
 n2 = n_elements(models_exclude)
 n3 = n_elements(models)
 print, ' # of total models, excluded models and final models in Model array: ',n1,n2,n3 


; 2 ======= MIPs

 MIPS = ['AerChemMIP',  'C4MIP',  'CDRMIP',  'CFMIP',  'CMIP',  'DAMIP', $
          'DCPP',  'GeoMIP',  'GMMIP',  'HighResMIP',  'LS3MIP',  'LUMIP',$
          'OMIP',  'PAMIP',  'PMIP',  'RFMIP',  'ScenarioMIP']

; 3 ======= Datatype (frequency)
 Hourly3 = '3hr  E3hr'
 Hourly6 = '6hrLev  6hrPlev  6hrPlevPt'
 Daily   = 'AERday  CFday  day  Eday  EdayZ  Oday  SIday'
 Monthly = 'AERmon  AERmonZ  Amon  CFmon  Emon  EmonZ  ImonAnt  ImonGre  LImon  Lmon  Omon  SImon'
 Yearly  = 'Eyr  Oyr' 
 FixType = 'Efx  fx  IfxGre  Ofx'

 Hourly3 = str_sep(strcompress(Hourly3) ,' ')
 Hourly6 = str_sep(strcompress(hourly6) ,' ')
 Daily   = str_sep(strcompress(Daily)   ,' ')
 Monthly = str_sep(strcompress(Monthly) ,' ')
 Yearly  = str_sep(strcompress(Yearly)  ,' ')
 FixType = str_sep(strcompress(FixType) ,' ')
 DataType = [Hourly3, Hourly6, Daily, Monthly, Yearly, FixType]

 DataType_Stru = create_struct('Hourly3',Hourly3 ,  'Hourly6', Hourly6,  'Daily ',Daily, 'Monthly',Monthly, 'Yearly',Yearly, $
                'FixType',FixType, 'all',DataType) 

; 4 ======= Specific MIPs
; 4.1 -- CMIP
 CMIP = ['1pctCO2',  'abrupt-4xCO2',  'amip',  'esm-hist',  'esm-piControl',  $
             'historical',  'piControl']

; 4.2 -- ScenarioMIP
 scenarioMIP= ['ssp126',  'ssp245',  'ssp370',  'ssp585']

; 4.3 -- CFMIP
 CFMIP_amip   = 'amip-4xCO2  amip-a4SST-4xCO2  amip-future4K  amip-lwoff  amip-m4K  amip-p4K  amip-p4K-lwoff  amip-piForcing'
 CFMIP_aqua   = 'aqua-4xCO2  aqua-control  aqua-control-lwoff  aqua-p4K  aqua-p4K-lwoff'
 CFMIP_abrupt = 'abrupt-0p5xCO2  abrupt-2xCO2  abrupt-solm4p  abrupt-solp4p'
 CFMIP_a4SST  = 'a4SST  a4SSTice  a4SSTice-4xCO2  amip-a4SST-4xCO2'
 CFMIP_piSST  = 'piSST  piSST-4xCO2  piSST-4xCO2-rad  piSST-pxK'

 CFMIP_amip   = str_sep(strcompress(CFMIP_amip)  ,' ')
 CFMIP_aqua   = str_sep(strcompress(CFMIP_aqua)  ,' ')
 CFMIP_abrupt = str_sep(strcompress(CFMIP_abrupt),' ')
 CFMIP_a4SST  = str_sep(strcompress(CFMIP_a4SST) ,' ')
 CFMIP_piSST  = str_sep(strcompress(CFMIP_piSST) ,' ')
 CFMIP        = [CFMIP_amip, CFMIP_aqua, CFMIP_abrupt, CFMIP_a4SST, CFMIP_piSST] 

 CFMIP_stru = create_struct('amip',CFMIP_amip,   'aqua', CFMIP_aqua,  'abrupt', CFMIP_abrupt, $
         'a4SST',CFMIP_a4SST , 'piSST',CFMIP_piSST , 'all',CFMIP) 

 print, ' '
 print,' Final models to include: '
 for i = 0,n_elements(models) -1 do begin
   print,'  model ', i, '  ', models[i]
 endfor

 print, ' '
 print,' Final MIPs to include: '
 for i = 0,n_elements(MIPs) -1 do begin
   print,'  mip ', i, '  ', MIPS[i]
 endfor

; ERA5
  plev    = [1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10, 5, 1]
  plevec  = '1000/975/950/925/900/875/850/825/800/775/750/700/650/600/550/500/450/400/350/300/250/225/200/175/150/125/100/70/50/30/20/10/7/5/3/2/1'
  plevec  = str_sep(plevec,'/')
  plevec3 = long(plevec)
  jj      = [0,3,6,11,13,15,17,19,20,22,24,26,27,28,29,30,31,33,36]
  cmip_era5_lev = create_struct('era5_lev',plevec, 'cmip_lev',plev, 'era5_2cmip_lev', jj)


 print,' structure cmip6 saved in cmip6_macro.sav'

 cmip6 = create_struct('Models', Models, 'Models0', Models0, 'Models_all', models_all, 'Models_Execlude', Models_Exclude, $
              'MIPS', MIPS,   'CMIP', CMIP,    'CFMIP', CFMIP, 'CFMIP_stru', CFMIP_stru, $
              'ScenarioMIP', ScenarioMIP ,  'DataType', DataType, 'DataType_Stru', DataType_Stru, $
              'cmip_era5_lev',cmip_era5_lev)

 save, file='cmip6_macro.sav',cmip6
 print, 'saved ..'
 print, "'Models', Models, 'Models0', Models0, 'Models_all', models_all, 'Models_Execlude', Models_Exclude," + $
        "       'MIPS', MIPS,   'CMIP', CMIP,    'CFMIP', CFMIP, 'CFMIP_stru', CFMIP_stru," + $
        "       'ScenarioMIP', ScenarioMIP ,  'DataType', DataType, 'DataType_Stru', DataType_Stru," + $
        "       'cmip_era5_lev',cmip_era5_lev"
 

;return,1
; DataType = ['3hr',     '6hrPlev',    'AERday',  'AERmonZ',  'CFday',  'day',   'E3hr',  'EdayZ', $
;             'Emon',   'Eyr',  'IfxGre',   'ImonGre',  'Lmon',  'Ofx',   'Oyr',    'SImon', $
;             '6hrLev',  '6hrPlevPt',  'AERmon',  'Amon',     'CFmon',  'E1hr',  'Eday',  'Efx', $
;             'EmonZ',  'fx',   'ImonAnt',  'LImon',    'Oday',  'Omon',  'SIday']
;
; CFMIP = ['a4SST',           'abrupt-2xCO2',   'amip-a4SST-4xCO2',  'amip-p4K',   'aqua-control',$
;        'piSST', 'a4SSTice',        'abrupt-solm4p',  'amip-future4K',     'amip-p4K-lwoff', $
;         'aqua-control-lwoff',  'piSST-4xCO2', 'a4SSTice-4xCO2',  'abrupt-solp4p', $
;         'amip-lwoff',        'amip-piForcing',  'aqua-p4K',            'piSST-4xCO2-rad', $
;         'abrupt-0p5xCO2',  'amip-4xCO2',     'amip-m4K',          'aqua-4xCO2', $
;         'aqua-p4K-lwoff',      'piSST-pxK']

end
