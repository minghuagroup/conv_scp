

; run cam_scaling2 to generate the template

;cam_vars = cam_vars()

NaN = !Values.F_NaN

lev_us   = [-10., 10., 1]
lev_u    = [-5., 5., 1]
lev_uu    = [-50., 50., 1]
lev_01   = [0., 1., 1]
lev_T    = [180., 310., 1]
lev_TS    = [240., 310., 1]
lev_TSi   = [240, 275, 1]
lev_Q    = [0., 24, 1.0e3]
lev_QS   = [0., 32., 1.0e-3]
lev_cld    = [0, 100, 100]
lev_tau   = [-0.5,0.5,1]
lev_swvl  = [0., 0.5, 1]
lev_00    = [0,0,1]
scale_precip = 86400*1000.
scale_day = 86400.
;

       cam_lev = create_struct('ABSORB', [0.00000, 1.00000, 1.00000])    ; [ 3.32107e-13, 0.000449653,  3.43393e-07  ])
       ;cam_lev = create_struct(cam_lev, 'ABSORB', [0.00000, 1.00000, 1.00000])    ; [ 3.32107e-13, 0.000449653,  3.43393e-07  ])
       cam_lev = create_struct(cam_lev, 'ABSORB_unit',  '/m'  )

       cam_lev = create_struct(cam_lev, 'ANRAIN', [0.00000, 20.0000, 1.0e-3])    ; [ 0.00000, 120742.,  869.113  ])
       cam_lev = create_struct(cam_lev, 'ANRAIN_unit',  'm-3'  )

       cam_lev = create_struct(cam_lev, 'ANSNOW', [0.00000, 20.0000, 1.0e-3])    ; [ 0.00000, 117763.,  1564.88  ])
       cam_lev = create_struct(cam_lev, 'ANSNOW_unit',  'm-3'  )

       cam_lev = create_struct(cam_lev, 'AODABS', [0.00000, 1.00000, 1.00000e1])    ; [ 3.25793e-05, 0.369465,  0.00322179  ])
       cam_lev = create_struct(cam_lev, 'AODABS_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'AODDUST1', [0.00000, 1.00000, 1.00000])    ; [ 2.33678e-05, 0.440260,  0.00476082  ])
       cam_lev = create_struct(cam_lev, 'AODDUST1_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'AODDUST2', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'AODDUST2_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'AODDUST3', [0.00000, 1.00000, 1.00000])    ; [ 4.88943e-06, 1.76860,  0.0113032  ])
       cam_lev = create_struct(cam_lev, 'AODDUST3_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'AODVIS', [0.00000, 1.00000, 1.00000])    ; [ 0.00166152, 2.25046,  0.0803696  ])
       cam_lev = create_struct(cam_lev, 'AODVIS_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'AQRAIN', [0.00000, 5.0000, 1.e5])    ; [ 0.00000, 0.000112676,  1.60086e-06  ])
       cam_lev = create_struct(cam_lev, 'AQRAIN_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'AQSNOW', [0.00000, 5.0000, 1.e5])    ; [ 0.00000, 0.000112066,  3.20195e-06  ])
       cam_lev = create_struct(cam_lev, 'AQSNOW_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'AREI', [0.00000, 50.000, 1.00000])    ; [ 0.00000, 53.6300,  5.31727  ])
       cam_lev = create_struct(cam_lev, 'AREI_unit',  'Micron'  )

       cam_lev = create_struct(cam_lev, 'AREL', [0.00000, 10.0000, 1.00000])    ; [ 0.00000, 8.89293,  0.361023  ])
       cam_lev = create_struct(cam_lev, 'AREL_unit',  'Micron'  )

       cam_lev = create_struct(cam_lev, 'ATMNET', [-200.000, 200.000, 1.00000])    ; [ -122.499, 188.465,  -39.8891  ])
       cam_lev = create_struct(cam_lev, 'ATMNET_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'ATMRAD', [-170.000, -10.0000, 1.00000])    ; [ -150.583, -45.5695,  -105.021  ])
       cam_lev = create_struct(cam_lev, 'ATMRAD_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'ATMRADC', [-170.000, -10.0000, 1.00000])    ; [ -140.136, -47.3560,  -101.630  ])
       cam_lev = create_struct(cam_lev, 'ATMRADC_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'AWNC', [0.00000, 100.0000, 1.00000e-06])    ; [ 0.00000, 1.24722e+08,  2.76179e+06  ])
       cam_lev = create_struct(cam_lev, 'AWNC_unit',  'm-3'  )

       cam_lev = create_struct(cam_lev, 'AWNI', [0.00000, 1.0000, 1.00000e-06])    ; [ 0.00000, 158135.,  3206.31  ])
       cam_lev = create_struct(cam_lev, 'AWNI_unit',  'm-3'  )

       cam_lev = create_struct(cam_lev, 'BURDENBC', [0.00000, 1.00000, 1.00000e3])    ; [ 1.17681e-09, 1.02046e-06,  4.76963e-08  ])
       cam_lev = create_struct(cam_lev, 'BURDENBC_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'BURDENDUST', [0.00000, 1.00000, 1.00000e3])    ; [ 2.86357e-08, 0.00536852,  3.17297e-05  ])
       cam_lev = create_struct(cam_lev, 'BURDENDUST_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'BURDENPOM', [0.00000, 1.00000, 1.00000e3])    ; [ 1.21475e-08, 1.21385e-05,  5.06089e-07  ])
       cam_lev = create_struct(cam_lev, 'BURDENPOM_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'BURDENSEASALT', [0.00000, 1.00000, 1.00000e3])    ; [ 6.88763e-07, 6.46937e-05,  1.66554e-05  ])
       cam_lev = create_struct(cam_lev, 'BURDENSEASALT_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'BURDENSO4', [0.00000, 1.00000, 1.00000e3])    ; [ 2.57676e-07, 8.47886e-06,  8.60007e-07  ])
       cam_lev = create_struct(cam_lev, 'BURDENSO4_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'BURDENSOA', [0.00000, 1.00000, 1.00000e3])    ; [ 1.23024e-07, 1.86742e-05,  1.19581e-06  ])
       cam_lev = create_struct(cam_lev, 'BURDENSOA_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'CCN3', [0.00000, 100.000, 1.00000])    ; [ 0.000122726, 942.708,  12.5643  ])
       cam_lev = create_struct(cam_lev, 'CCN3_unit',  '#/cm3'  )

       cam_lev = create_struct(cam_lev, 'CDNUMC', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.0659502, 8.30196e+10,  8.94396e+09  ])
       cam_lev = create_struct(cam_lev, 'CDNUMC_unit',  '#/m2'  )

       cam_lev = create_struct(cam_lev, 'CLDHGH', [0, 100, 100])    ; [ 0.0623436, 0.825948,  0.382946  ])
       cam_lev = create_struct(cam_lev, 'CLDHGH_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'CLDICE', [0.00000, 2.00000, 100000.])    ; [ 0.00000, 3.29911e-05,  6.47347e-07  ])
       cam_lev = create_struct(cam_lev, 'CLDICE_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'CLDLIQ', [0.00000, 5.00000, 100000.])    ; [ 0.00000, 0.000124504,  2.47388e-06  ])
       cam_lev = create_struct(cam_lev, 'CLDLIQ_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'CLDLOW', [0, 100, 100])    ; [ 0.00000, 0.913364,  0.457244  ])
       cam_lev = create_struct(cam_lev, 'CLDLOW_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'CLDMED', [0, 100, 100])    ; [ 0.000776948, 0.908729,  0.316292  ])
       cam_lev = create_struct(cam_lev, 'CLDMED_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'CLDTOT', [0, 100, 100])    ; [ 0.157858, 0.951204,  0.667920  ])
       cam_lev = create_struct(cam_lev, 'CLDTOT_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'CLOUD', [0.00000, 80.0000, 100.000])    ; [ 0.00000, 0.734070,  0.128288  ])
       cam_lev = create_struct(cam_lev, 'CLOUD_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'CO2', [0.00000, 1.00000, 1.00000])    ; [ 0.000427331, 0.000466802,  0.000436683  ])
       cam_lev = create_struct(cam_lev, 'CO2_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'CO2_LND', [0.00000, 1.00000, 1.00000])    ; [ 0.000421288, 0.000460644,  0.000430316  ])
       cam_lev = create_struct(cam_lev, 'CO2_LND_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'CO2_OCN', [0.00000, 1.00000, 1.00000])    ; [ 0.000436269, 0.000439885,  0.000437946  ])
       cam_lev = create_struct(cam_lev, 'CO2_OCN_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'DCQ', [-10.0000, 10.0000, 8.64000e+07])    ; [ -1.79447e-07, 3.70851e-08,  -2.83808e-09  ])
       cam_lev = create_struct(cam_lev, 'DCQ_unit',  'kg/kg/s'  )

       cam_lev = create_struct(cam_lev, 'DTCOND', [-10.0000, 10.0000, 86400.0])    ; [ -0.000171682, 0.000333832,  3.92738e-06  ])
       cam_lev = create_struct(cam_lev, 'DTCOND_unit',  'K/s'  )

       cam_lev = create_struct(cam_lev, 'DTV', [-10.0000, 10.0000, 86400.0])    ; [ -0.000276199, 0.000760032,  3.26994e-06  ])
       cam_lev = create_struct(cam_lev, 'DTV_unit',  'K/s'  )

       cam_lev = create_struct(cam_lev, 'EXTINCT', [0.00000, 1.00000, 1.00000])    ; [ 6.11099e-12, 0.00261339,  1.05735e-05  ])
       cam_lev = create_struct(cam_lev, 'EXTINCT_unit',  '/m'  )

       cam_lev = create_struct(cam_lev, 'FICE', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.990095,  0.272294  ])
       cam_lev = create_struct(cam_lev, 'FICE_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'FLDS', [0.00000, 500.000, 1.00000])    ; [ 72.1100, 427.723,  293.424  ])
       cam_lev = create_struct(cam_lev, 'FLDS_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FLNA', [-250.000, -50.0000, 1.00000])    ; [ -237.469, -72.0139,  -167.726  ])
       cam_lev = create_struct(cam_lev, 'FLNA_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'FLNAC', [-240.000, -70.0000, 1.00000])    ; [ -225.629, -67.6777,  -159.959  ])
       cam_lev = create_struct(cam_lev, 'FLNAC_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'FLNS', [-100.000, 200.000, 1.00000])    ; [ 17.6546, 139.901,  52.9241  ])
       cam_lev = create_struct(cam_lev, 'FLNS_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FLNSC', [-100.000, 200.000, 1.00000])    ; [ 32.2075, 145.356,  79.3887  ])
       cam_lev = create_struct(cam_lev, 'FLNSC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FLNT', [0.00000, 360.000, 1.00000])    ; [ 121.719, 297.914,  220.654  ])
       cam_lev = create_struct(cam_lev, 'FLNT_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FLNTC', [0.00000, 360.000, 1.00000])    ; [ 121.041, 306.030,  239.348  ])
       cam_lev = create_struct(cam_lev, 'FLNTC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FLUT', [0.00000, 360.000, 1.00000])    ; [ 122.737, 299.321,  221.919  ])
       cam_lev = create_struct(cam_lev, 'FLUT_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FLUTC', [0.00000, 360.000, 1.00000])    ; [ 122.059, 307.437,  240.615  ])
       cam_lev = create_struct(cam_lev, 'FLUTC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FREQI', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.752015,  0.0668739  ])
       cam_lev = create_struct(cam_lev, 'FREQI_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'FREQL', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.824161,  0.0358339  ])
       cam_lev = create_struct(cam_lev, 'FREQL_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'FREQR', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.924055,  0.0570389  ])
       cam_lev = create_struct(cam_lev, 'FREQR_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'FREQS', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.902931,  0.121079  ])
       cam_lev = create_struct(cam_lev, 'FREQS_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'FSDS', [0.00000, 500.000, 1.00000])    ; [ 66.4052, 294.423,  169.671  ])
       cam_lev = create_struct(cam_lev, 'FSDS_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSDSC', [0.00000, 500.000, 1.00000])    ; [ 126.173, 334.388,  219.352  ])
       cam_lev = create_struct(cam_lev, 'FSDSC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSNA', [0.00000, 120.000, 1.00000])    ; [ 20.8293, 156.069,  62.7054  ])
       cam_lev = create_struct(cam_lev, 'FSNA_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'FSNAC', [0.00000, 120.000, 1.00000])    ; [ 20.3163, 157.985,  58.3280  ])
       cam_lev = create_struct(cam_lev, 'FSNAC_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'FSNS', [0.00000, 500.000, 1.00000])    ; [ 18.9599, 265.402,  129.895  ])
       cam_lev = create_struct(cam_lev, 'FSNS_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSNSC', [0.00000, 500.000, 1.00000])    ; [ 22.2530, 292.621,  173.152  ])
       cam_lev = create_struct(cam_lev, 'FSNSC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSNT', [0.00000, 500.000, 1.00000])    ; [ 46.1181, 352.711,  192.600  ])
       cam_lev = create_struct(cam_lev, 'FSNT_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSNTC', [0.00000, 500.000, 1.00000])    ; [ 46.4049, 378.068,  231.483  ])
       cam_lev = create_struct(cam_lev, 'FSNTC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSNTOA', [0.00000, 500.000, 1.00000])    ; [ 48.1633, 356.624,  195.685  ])
       cam_lev = create_struct(cam_lev, 'FSNTOA_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'FSNTOAC', [0.00000, 500.000, 1.00000])    ; [ 48.4471, 382.057,  234.536  ])
       cam_lev = create_struct(cam_lev, 'FSNTOAC_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'H', [260.000, 450.000, 1.00000])    ; [ 0.00239432, 43.3143,  4.55642  ])
       cam_lev = create_struct(cam_lev, 'H_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'ICEFRAC', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.991761,  0.125919  ])
       cam_lev = create_struct(cam_lev, 'ICEFRAC_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'ICIMR', [0.00000, 20.0000, 100000.])    ; [ 0.00000, 0.000116822,  1.76121e-06  ])
       cam_lev = create_struct(cam_lev, 'ICIMR_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'ICLDIWP', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.0908777,  0.000943120  ])
       cam_lev = create_struct(cam_lev, 'ICLDIWP_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'ICLDTWP', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.148610,  0.00447472  ])
       cam_lev = create_struct(cam_lev, 'ICLDTWP_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'ICWMR', [0.00000, 20.0000, 100000.])    ; [ 0.00000, 0.000316916,  1.07002e-05  ])
       cam_lev = create_struct(cam_lev, 'ICWMR_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'IWC', [0.00000, 20.0000, 100000.])    ; [ 0.00000, 3.51049e-05,  7.16080e-07  ])
       cam_lev = create_struct(cam_lev, 'IWC_unit',  'kg/m3'  )

       cam_lev = create_struct(cam_lev, 'LANDFRAC', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 1.00000,  0.343875  ])
       cam_lev = create_struct(cam_lev, 'LANDFRAC_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'LHFLX', [-20.0000, 300.000, 1.00000])    ; [ -2.33806, 274.283,  65.1310  ])
       cam_lev = create_struct(cam_lev, 'LHFLX_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'LWCF', [-10.0000, 100.000, 1.00000])    ; [ -1.81019, 93.2581,  18.6977  ])
       cam_lev = create_struct(cam_lev, 'LWCF_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'LWCFA', [-60.0000, 90.0000, 1.00000])    ; [ -36.8332, 64.3464,  -7.76638  ])
       cam_lev = create_struct(cam_lev, 'LWCFA_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'LWCFS', [-20.0000, 80.0000, 1.00000])    ; [ 2.00039, 71.1914,  26.4637  ])
       cam_lev = create_struct(cam_lev, 'LWCFS_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'NUMICE', [1.00000, 100.000, 1.00000e-06])    ; [ 1.00000e-12, 537687.,  5729.74  ])
       cam_lev = create_struct(cam_lev, 'NUMICE_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'NUMLIQ', [1.00000, 100.000, 1.00000e-06])    ; [ 1.00000e-12, 7.02548e+07,  1.01096e+06  ])
       cam_lev = create_struct(cam_lev, 'NUMLIQ_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'OCNFRAC', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 1.00000,  0.530208  ])
       cam_lev = create_struct(cam_lev, 'OCNFRAC_unit',  'fraction'  )

       cam_lev = create_struct(cam_lev, 'OMEGA', [-2, 2, 36])    ; [ -0.711592, 1.81371,  0.000829065  ])
       cam_lev = create_struct(cam_lev, 'OMEGA_unit',  'Pa/s'  )

       cam_lev = create_struct(cam_lev, 'OMEGAT', [-6.00000, 6.000, 1.00000e-2])    ; [ -199.271, 412.419,  0.137551  ])
       cam_lev = create_struct(cam_lev, 'OMEGAT_unit',  'K Pa/s'  )

       cam_lev = create_struct(cam_lev, 'PBLH', [0.00000, 1500.00, 1.00000])    ; [ 54.4829, 1078.39,  433.425  ])
       cam_lev = create_struct(cam_lev, 'PBLH_unit',  'm'  )

       cam_lev = create_struct(cam_lev, 'PHIS', [-200.00, 5000., 1.00000e-1])    ; [ -263.894, 51773.8,  3745.85  ])
       cam_lev = create_struct(cam_lev, 'PHIS_unit',  'm2/s2'  )

       cam_lev = create_struct(cam_lev, 'PME', [-10.0000, 15.0000, 8.64e7])    ; [ -0.000109607, 8.28593e-07,  -2.60033e-05  ])
       cam_lev = create_struct(cam_lev, 'PME_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'PRECC', [0.00000, 10.0000, 8.64000e+07])    ; [ 0.00000, 1.98042e-07,  1.60491e-08  ])
       cam_lev = create_struct(cam_lev, 'PRECC_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'PRECL', [0.00000, 10.0000, 8.64000e+07])    ; [ 2.17036e-10, 2.29649e-07,  1.22191e-08  ])
       cam_lev = create_struct(cam_lev, 'PRECL_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'PRECSC', [0.00000, 5.00000, 8.64e7])    ; [ 0.00000, 4.91032e-09,  1.20029e-10  ])
       cam_lev = create_struct(cam_lev, 'PRECSC_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'PRECSL', [0.00000, 15.0000, 8.64e7])    ; [ -3.30979e-23, 8.21047e-08,  4.05108e-09  ])
       cam_lev = create_struct(cam_lev, 'PRECSL_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'PRECT', [0.00000, 15.0000, 8.64e7])    ; [ 3.64337e-10, 3.56054e-07,  2.82682e-08  ])
       cam_lev = create_struct(cam_lev, 'PRECT_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'PS', [900.000, 1030.00, 0.0100000])    ; [ 53075.1, 102319.,  96581.5  ])
       cam_lev = create_struct(cam_lev, 'PS_unit',  'Pa'  )

       cam_lev = create_struct(cam_lev, 'PSL', [980.000, 1030.00, 0.0100000])    ; [ 97905.4, 102404.,  100912.  ])
       cam_lev = create_struct(cam_lev, 'PSL_unit',  'Pa'  )

       cam_lev = create_struct(cam_lev, 'Q', [0.00000, 24.0000, 1000.00])    ; [ 9.61559e-07, 0.0173950,  0.00182990  ])
       cam_lev = create_struct(cam_lev, 'Q_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'QFLX', [-1.00000, 10.0000, -8.64000e+04])    ; [ -8.24882e-07, 0.000109670,  2.60316e-05  ])
       cam_lev = create_struct(cam_lev, 'QFLX_unit',  'kg/m2/s'  )

       cam_lev = create_struct(cam_lev, 'QR', [-2.0000, 2.0000, 86400.0])    ; [ -0.000126349, 2.28970e-05,  -1.64886e-05  ])
       cam_lev = create_struct(cam_lev, 'QR_unit',  'K/s'  )

       cam_lev = create_struct(cam_lev, 'QRL', [-4.0000, 4.0000, 86400.0])    ; [ -0.000126349, 2.28970e-05,  -1.64886e-05  ])
       cam_lev = create_struct(cam_lev, 'QRL_unit',  'K/s'  )

       cam_lev = create_struct(cam_lev, 'QRS', [-4.0000, 4.0000, 86400.0])    ; [ 0.00000, 5.93063e-05,  3.43804e-06  ])
       cam_lev = create_struct(cam_lev, 'QRS_unit',  'K/s'  )

       cam_lev = create_struct(cam_lev, 'RELHUM', [0.00000, 100.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'RELHUM_unit',  'percent'  )

       cam_lev = create_struct(cam_lev, 'SFCO2', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SFCO2_unit',  'kg/m2/s'  )

       cam_lev = create_struct(cam_lev, 'SFCO2_LND', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SFCO2_LND_unit',  'kg/m2/s'  )

       cam_lev = create_struct(cam_lev, 'SFCO2_OCN', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SFCO2_OCN_unit',  'kg/m2/s'  )

       cam_lev = create_struct(cam_lev, 'SHFLX', [-20.0000, 200.000, -1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SHFLX_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'SNOWHICE', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SNOWHICE_unit',  'm'  )

       cam_lev = create_struct(cam_lev, 'SNOWHLND', [0.00000, 1.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SNOWHLND_unit',  'm'  )

       cam_lev = create_struct(cam_lev, 'SOLIN', [0.00000, 500.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SOLIN_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'SRFNET', [-400.000, 400.000, 1.00000])    ; [ -186.897, 152.815,  11.8385  ])
       cam_lev = create_struct(cam_lev, 'SRFNET_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'SRFRAD', [0, 500, 1])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SRFRAD_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'SRFRADC', [-100.000, 300.000, 1.00000])    ; [ -49.8539, 229.588,  93.7659  ])
       cam_lev = create_struct(cam_lev, 'SRFRADC_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'SWCF', [-200.000, 30.0000, 0.100000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'SWCF_unit',  'W/m2'  )

       cam_lev = create_struct(cam_lev, 'SWCFA', [-10.0000, 20.0000, 1.00000])    ; [ -3.38833, 23.7084,  4.37606  ])
       cam_lev = create_struct(cam_lev, 'SWCFA_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'SWCFS', [-200.000, 30.0000, 1.00000])    ; [ -187.297, -0.469570,  -43.2592  ])
       cam_lev = create_struct(cam_lev, 'SWCFS_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'T', [180.000, 310.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'T_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TAUX', [-0.500000, 0.500000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TAUX_unit',  'N/m2'  )

       cam_lev = create_struct(cam_lev, 'TAUY', [-0.500000, 0.500000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TAUY_unit',  'N/m2'  )

       cam_lev = create_struct(cam_lev, 'TGCLDCWP', [0.00000, 200.000, 1000.00])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TGCLDCWP_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'TGCLDIWP', [0.00000, 100.000, 1000.00])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TGCLDIWP_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'TGCLDLWP', [0.00000, 200.000, 1000.00])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TGCLDLWP_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'THETA', [260.000, 450.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'THETA_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'TMCO2', [0.00000, 5.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TMCO2_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'TMCO2_LND', [0.00000, 5.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TMCO2_LND_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'TMCO2_OCN', [0.00000, 5.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TMCO2_OCN_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'TMQ', [0.00000, 50.0000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TMQ_unit',  'kg/m2'  )

       cam_lev = create_struct(cam_lev, 'TOARAD', [-200.000, 200.000, 1.00000])    ; [ -128.866, 84.3881,  -28.0507  ])
       cam_lev = create_struct(cam_lev, 'TOARAD_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'TOARADC', [-200.000, 200.000, 1.00000])    ; [ -130.995, 107.017,  -7.86500  ])
       cam_lev = create_struct(cam_lev, 'TOARADC_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'TOTCRF', [-180.000, 40.0000, 1.00000])    ; [ -111.575, 13.4320,  -20.1857  ])
       cam_lev = create_struct(cam_lev, 'TOTCRF_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'TOTCRFA', [-60.0000, 60.0000, 1.00000])    ; [ -30.6094, 69.4421,  -3.39025  ])
       cam_lev = create_struct(cam_lev, 'TOTCRFA_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'TOTCRFS', [-100.000, 60.0000, 1.00000])    ; [ -128.537, 37.0709,  -16.7953  ])
       cam_lev = create_struct(cam_lev, 'TOTCRFS_unit',  ''  )

       cam_lev = create_struct(cam_lev, 'TOT_CLD_VISTAU', [0.00000, 10.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TOT_CLD_VISTAU_unit',  '1'  )

       cam_lev = create_struct(cam_lev, 'TREFHT', [220.000, 320.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TREFHT_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TREFHTMN', lev_ts)    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TREFHTMN_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TREFHTMX', lev_ts)    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TREFHTMX_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TROP_P', [60.00000, 250.00000, 1.00000e-2])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TROP_P_unit',  'Pa'  )

       cam_lev = create_struct(cam_lev, 'TROP_T', lev_tsi)    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TROP_T_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TS', [240.000, 310.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TS_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TSMN', [220.000, 320.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TSMN_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'TSMX', [220.000, 320.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'TSMX_unit',  'K'  )

       cam_lev = create_struct(cam_lev, 'U', [-50.0000, 50.0000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'U_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'U10', [0.0000, 15.0000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'U10_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'UQ', [-100.000, 100.000, 1000.00])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'UQ_unit',  'm/skg/kg'  )

       cam_lev = create_struct(cam_lev, 'UU', [0.00000, 1000.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'UU_unit',  'm2/s2'  )

       cam_lev = create_struct(cam_lev, 'V', [-5.00000, 5.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'V_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'VD01', [-10.0000, 2.0000, 8.64000e+07])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'VD01_unit',  'kg/kg/s'  )

       cam_lev = create_struct(cam_lev, 'VQ', [-100.000, 20.000, 1000.00])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'VQ_unit',  'm/skg/kg'  )

       cam_lev = create_struct(cam_lev, 'VT', [-1000.0000, 1000.0000, 1.0])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'VT_unit',  'K m/s'  )

       cam_lev = create_struct(cam_lev, 'VU', [100.00000, 100.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'VU_unit',  'm2/s2'  )

       cam_lev = create_struct(cam_lev, 'VV', [0.00000, 100.000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'VV_unit',  'm2/s2'  )

       cam_lev = create_struct(cam_lev, 'WGUSTD', [0.00000, 5.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'WGUSTD_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'WSPDSRFMX', [0.00000, 20.00000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'WSPDSRFMX_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'WSUB', [0, 0.50000, 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'WSUB_unit',  'm/s'  )

       cam_lev = create_struct(cam_lev, 'Z3', [-2000.00, 500000., 1.00000])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'Z3_unit',  'm'  )

       cam_lev = create_struct(cam_lev, 'bc_a1_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'bc_a1_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'dst_a1_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'dst_a1_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'dst_a3_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'dst_a3_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'pom_a1_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'pom_a1_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'so4_a1_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'so4_a1_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'so4_a2_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'so4_a2_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'so4_a3_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'so4_a3_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'soa_a1_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'soa_a1_SRF_unit',  'kg/kg'  )

       cam_lev = create_struct(cam_lev, 'soa_a2_SRF', [0.00000, 1.00000, 1.00000e-6])    ; [ 0.00000, 0.00000,  0.00000  ])
       cam_lev = create_struct(cam_lev, 'soa_a2_SRF_unit',  'kg/kg'  )


    vars_all = tag_names(cam_lev)
    vars_all = strdel(vars_all,'_UNIT')
    vars_all = vars_all[uniq(vars_all)]

    cam_lev = create_struct("cam_vars", vars_all, cam_lev) 

   scale_cam = create_struct('vars',vars_all)
   for iv = 0, n_elements(vars_all)-1 do begin
     var0 = vars_all[iv]
     dlev = get_stru(cam_lev,var0) 
     scale_cam = create_struct(scale_cam,var0, dlev[2])
   endfor

   save,file = 'scale_cam.sav0', scale_cam, cam_lev
  
end
