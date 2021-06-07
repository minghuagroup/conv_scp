

if(not keyword_Set(missing))then missing  = 1.0e12
if(not keyword_Set(nn))then nn = 20                     ; forced lines

era5_vars = era5_vars()


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


era5_lev = create_struct("era5_vars", era5_vars) 
       era5_lev = create_struct(era5_lev, 'VAR_100U', lev_us)
       era5_lev = create_struct(era5_lev, 'VAR_100U_str', [ '100m_u-component_of_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VAR_100V', lev_us)
       era5_lev = create_struct(era5_lev, 'VAR_100V_str', [ '100m_v-component_of_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VAR_10U',  lev_us)
       era5_lev = create_struct(era5_lev, 'VAR_10U_str', [ '10m_u-component_of_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VAR_10V', lev_us)
       era5_lev = create_struct(era5_lev, 'VAR_10V_str', [ '10m_v-component_of_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VAR_2D', lev_TS)
       era5_lev = create_struct(era5_lev, 'VAR_2D_str', [ '2m_dewpoint_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'VAR_2T', lev_TS)
       era5_lev = create_struct(era5_lev, 'VAR_2T_str', [ '2m_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'ALNID',   lev_01)
       era5_lev = create_struct(era5_lev, 'ALNID_str', [ 'near_ir_albedo_for_diffuse_radiation', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'ALNIP', lev_01)
       era5_lev = create_struct(era5_lev, 'ALNIP_str', [ 'near_ir_albedo_for_direct_radiation', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'ALUVD', lev_01)
       era5_lev = create_struct(era5_lev, 'ALUVD_str', [ 'uv_visible_albedo_for_diffuse_radiation', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'ALUVP', lev_01)
       era5_lev = create_struct(era5_lev, 'ALUVP_str', [ 'uv_visible_albedo_for_direct_radiation', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'ANOR', [ -2,2,1])
       era5_lev = create_struct(era5_lev, 'ANOR_str', [ 'angle_of_sub_gridscale_orography', 'radians'  ])

       era5_lev = create_struct(era5_lev, 'ASN', lev_01)
       era5_lev = create_struct(era5_lev, 'ASN_str', [ 'snow_albedo', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'BLD', [ 0, 10,  1.e-0  ])
       era5_lev = create_struct(era5_lev, 'BLD_str', [ 'boundary_layer_dissipation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'BLH', [ 0, 1500., 1  ])
       era5_lev = create_struct(era5_lev, 'BLH_str', [ 'boundary_layer_height', 'm'  ])

       era5_lev = create_struct(era5_lev, 'CAPE', [ 0, 1500,  1  ])
       era5_lev = create_struct(era5_lev, 'CAPE_str', [ 'convective_available_potential_energy', 'J kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'CBH', [ 0., 3000.,1])
       era5_lev = create_struct(era5_lev, 'CBH_str', [ 'cloud_base_height', 'm'  ])
;??
       era5_lev = create_struct(era5_lev, 'CC', [0., 80, 100])
       era5_lev = create_struct(era5_lev, 'CC_str', [ 'fraction_of_cloud_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'CDIR', [ 0, 400,  1])
       era5_lev = create_struct(era5_lev, 'CDIR_str', [ 'clear_sky_direct_solar_radiation_at_surface', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'CHNK', [ 0, 2,  100  ])
       era5_lev = create_struct(era5_lev, 'CHNK_str', [ 'charnock', '-'  ])

       era5_lev = create_struct(era5_lev, 'CI',  lev_01)
       era5_lev = create_struct(era5_lev, 'CI_str', [ 'sea-ice_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'CIN', [0., 1000., 1])
       era5_lev = create_struct(era5_lev, 'CIN_str', [ 'convective_inhibition', 'J kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'CIWC', [0 , 2,  1.0e5  ])
       era5_lev = create_struct(era5_lev, 'CIWC_str', [ 'specific_cloud_ice_water_content', 'kg kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'CL', lev_01)
       era5_lev = create_struct(era5_lev, 'CL_str', [ 'lake_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'CLWC', [ 0., 5., 1.0e5])
       era5_lev = create_struct(era5_lev, 'CLWC_str', [ 'specific_cloud_liquid_water_content', 'kg kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'CP', [0., 10., scale_precip]  ) 
       era5_lev = create_struct(era5_lev, 'CP_str', [ 'convective_precipitation', 'm'  ])

       era5_lev = create_struct(era5_lev, 'CRR', [ 0., 10., scale_day])
       era5_lev = create_struct(era5_lev, 'CRR_str', [ 'convective_rain_rate', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'CRWC', [0, 1., 1.0e5] )
       era5_lev = create_struct(era5_lev, 'CRWC_str', [ 'specific_rain_water_content', 'kg kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'CSF', [ 0., 5., scale_precip]) 
       era5_lev = create_struct(era5_lev, 'CSF_str', [ 'convective_snowfall', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'CSFR', [ 0, 3, scale_day])
       era5_lev = create_struct(era5_lev, 'CSFR_str', [ 'convective_snowfall_rate_water_equivalent', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'CSWC', [0., 5, 1.0e5]) 
       era5_lev = create_struct(era5_lev, 'CSWC_str', [ 'specific_snow_water_content', 'kg kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'CVH', [0., 1000., 1000])  ; mm 
       era5_lev = create_struct(era5_lev, 'CVH_str', [ 'high_vegetation_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'CVL', [ 0., 1000., 1000])
       era5_lev = create_struct(era5_lev, 'CVL_str', [ 'low_vegetation_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'D', [ -10.,10.,1.e5])
       era5_lev = create_struct(era5_lev, 'D_str', [ 'divergence', 's**-1'  ])

       era5_lev = create_struct(era5_lev, 'DCTB', [ 0., 1000,1])
       era5_lev = create_struct(era5_lev, 'DCTB_str', [ 'duct_base_height', 'm'  ])

       era5_lev = create_struct(era5_lev, 'DL', [0., 7000.,1])
       era5_lev = create_struct(era5_lev, 'DL_str', [ 'lake_depth', 'm'  ])

       era5_lev = create_struct(era5_lev, 'DNDZA', [ -1., 1., 1]) 
       era5_lev = create_struct(era5_lev, 'DNDZA_str', [ 'mean_vertical_gradient_of_refractivity_inside_trapping_layer', 'm**-1'  ])

       era5_lev = create_struct(era5_lev, 'DNDZN', [ -1, 1, 1])
       era5_lev = create_struct(era5_lev, 'DNDZN_str', [ 'minimum_vertical_gradient_of_refractivity_inside_trapping_layer', 'm**-1'  ])

       era5_lev = create_struct(era5_lev, 'E', [ -1., 10., -scale_precip] )  ;-1.61414e-07, 9.06868e-09,  -2.41725e-08  ])
       era5_lev = create_struct(era5_lev, 'E_str', [ 'evaporation', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'ES', [-0.1, 1., -scale_precip] ) ; -9.07577e-09, 1.37894e-09,  -1.12031e-10  ])
       era5_lev = create_struct(era5_lev, 'ES_str', [ 'snow_evaporation', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'EWSS', lev_tau) ; -1.10729, 1.79919,  0.0149826  ])
       era5_lev = create_struct(era5_lev, 'EWSS_str', [ 'eastward_turbulent_surface_stress', 'N m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'FAL', lev_01) ;[ 0.0587649, 0.850136,  0.277135  ])
       era5_lev = create_struct(era5_lev, 'FAL_str', [ 'forecast_albedo', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'FDIR', [0., 300.,1 ]) ; -1.92872e-06, 376.963,  102.892  ])
       era5_lev = create_struct(era5_lev, 'FDIR_str', [ 'total_sky_direct_solar_radiation_at_surface', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'FLSR', [-11., 1, 1] )  ; -11.3592, 0.684979,  -8.62637  ])
       era5_lev = create_struct(era5_lev, 'FLSR_str', [ 'forecast_logarithm_of_surface_roughness_for_heat', '-'  ])

       era5_lev = create_struct(era5_lev, 'FSR', [0.,4.,1] ) ;2.95404e-05, 1.98430,  0.141969  ])
       era5_lev = create_struct(era5_lev, 'FSR_str', [ 'forecast_surface_roughness', 'm'  ])

       era5_lev = create_struct(era5_lev, 'GWD', [0., 10,1 ]) ; -0.000103841, 26.4061,  0.116286  ])
       era5_lev = create_struct(era5_lev, 'GWD_str', [ 'gravity_wave_dissipation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'HCC', lev_cld )  ; 3.19513e-08, 0.947425,  0.324219  ])
       era5_lev = create_struct(era5_lev, 'HCC_str', [ 'high_cloud_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'I10FG', [0., 15, 1]) ; 2.09862, 21.8984,  8.93436  ])
       era5_lev = create_struct(era5_lev, 'I10FG_str', [ 'instantaneous_10m_wind_gust', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'IE', [0., 10., -scale_day] ) ; -0.000155375, 9.11955e-06,  -2.37307e-05  ])
       era5_lev = create_struct(era5_lev, 'IE_str', [ 'instantaneous_moisture_flux', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'IEWS', lev_tau) ;-1.05754, 1.79925,  0.0145028  ])
       era5_lev = create_struct(era5_lev, 'IEWS_str', [ 'instantaneous_eastward_turbulent_surface_stress', 'N m**-2'  ])

       era5_lev = create_struct(era5_lev, 'ILSPF', lev_01 ) ;[ -1.62183e-08, 0.991728,  0.379924  ])
       era5_lev = create_struct(era5_lev, 'ILSPF_str', [ 'instantaneous_large_scale_surface_precipitation_fraction', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'INSS', lev_tau) ;-1.24428, 1.52983,  0.00405459  ])
       era5_lev = create_struct(era5_lev, 'INSS_str', [ 'instantaneous_northward_turbulent_surface_stress', 'N m**-2'  ])

       era5_lev = create_struct(era5_lev, 'ISHF', [-20., 200, -1]) ; -216.187, 71.6869,  -9.28836  ])
       era5_lev = create_struct(era5_lev, 'ISHF_str', [ 'instantaneous_surface_sensible_heat_flux', 'W m**-2'  ])

       era5_lev = create_struct(era5_lev, 'ISOR', lev_01) ; [ -5.38717e-08, 0.824782,  0.160931  ])
       era5_lev = create_struct(era5_lev, 'ISOR_str', [ 'anisotropy_of_sub_gridscale_orography', '-'  ])

       era5_lev = create_struct(era5_lev, 'ISTL1', lev_Tsi ) ;[ 242.944, 273.160,  269.936  ])
       era5_lev = create_struct(era5_lev, 'ISTL1_str', [ 'ice_temperature_layer_1', 'K'  ])

       era5_lev = create_struct(era5_lev, 'ISTL2', lev_Tsi) ;[ 245.330, 273.158,  270.060  ])
       era5_lev = create_struct(era5_lev, 'ISTL2_str', [ 'ice_temperature_layer_2', 'K'  ])

       era5_lev = create_struct(era5_lev, 'ISTL3', lev_Tsi) ;[ 253.227, 272.659,  270.474  ])
       era5_lev = create_struct(era5_lev, 'ISTL3_str', [ 'ice_temperature_layer_3', 'K'  ])

       era5_lev = create_struct(era5_lev, 'ISTL4', lev_Tsi) ;[ 263.294, 272.000,  271.003  ])
       era5_lev = create_struct(era5_lev, 'ISTL4_str', [ 'ice_temperature_layer_4', 'K'  ])

       era5_lev = create_struct(era5_lev, 'LBLT', lev_Ts ) ;[ 273.160, 306.598,  285.140  ])
       era5_lev = create_struct(era5_lev, 'LBLT_str', [ 'lake_bottom_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'LCC', lev_cld) ;[ -1.84733e-08, 0.985117,  0.467329  ])
       era5_lev = create_struct(era5_lev, 'LCC_str', [ 'low_cloud_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'LGWS', lev_tau) ; -1.33641, 1.81757,  0.00103500  ])
       era5_lev = create_struct(era5_lev, 'LGWS_str', [ 'eastward_gravity_wave_surface_stress', 'N m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'LICD', [0., 3,1]) ; -1.45261e-07, 3.00000,  0.409023  ])
       era5_lev = create_struct(era5_lev, 'LICD_str', [ 'lake_ice_depth', 'm'  ])

       era5_lev = create_struct(era5_lev, 'LICT', lev_tsi) ;[ 205.444, 273.160,  265.576  ])
       era5_lev = create_struct(era5_lev, 'LICT_str', [ 'lake_ice_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'LMLD', [0., 50, 1]) ; -2.20432e-06, 50.0000,  35.2504  ])
       era5_lev = create_struct(era5_lev, 'LMLD_str', [ 'lake_mix_layer_depth', 'm'  ])

       era5_lev = create_struct(era5_lev, 'LMLT', lev_Ts) ;[ 273.150, 306.598,  285.089  ])
       era5_lev = create_struct(era5_lev, 'LMLT_str', [ 'lake_mix_layer_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'LSF', [0., 5, scale_precip]) ; -4.53166e-15, 1.48500e-07,  3.67953e-09  ])
       era5_lev = create_struct(era5_lev, 'LSF_str', [ 'large_scale_snowfall', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'LSHF', [0.5, 1.,1] ) ; 0.500000, 0.799984,  0.658316  ])
       era5_lev = create_struct(era5_lev, 'LSHF_str', [ 'lake_shape_factor', 'dimensionless'  ])

       era5_lev = create_struct(era5_lev, 'LSM', lev_01) ;[ -7.49892e-08, 1.00000,  0.336448  ])
       era5_lev = create_struct(era5_lev, 'LSM_str', [ 'land_sea_mask', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'LSP', [0., 10., scale_precip]) ; -2.98939e-15, 5.02303e-07,  1.36180e-08  ])
       era5_lev = create_struct(era5_lev, 'LSP_str', [ 'large_scale_precipitation', 'm'  ])

       era5_lev = create_struct(era5_lev, 'LSPF', lev_01 ) ;[ -1.50097e-08, 0.985084,  0.379838  ])
       era5_lev = create_struct(era5_lev, 'LSPF_str', [ 'large_scale_precipitation_fraction', 's'  ])

       era5_lev = create_struct(era5_lev, 'LSRR', [ 0., 10, scale_day]) ;-1.24445e-11, 0.000493951,  9.97462e-06  ])
       era5_lev = create_struct(era5_lev, 'LSRR_str', [ 'large_scale_rain_rate', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'LSSFR', [0., 5, scale_day]) ; -6.22753e-12, 0.000146628,  3.67971e-06  ])
       era5_lev = create_struct(era5_lev, 'LSSFR_str', [ 'large_scale_snowfall_rate_water_equivalent', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'LTLT', lev_ts) ;[ 273.160, 306.598,  285.076  ])
       era5_lev = create_struct(era5_lev, 'LTLT_str', [ 'lake_total_layer_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'MCC', lev_cld) ;[ 2.70043e-05, 0.718046,  0.298475  ])
       era5_lev = create_struct(era5_lev, 'MCC_str', [ 'medium_cloud_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'MGWS', lev_tau) ;-1.51837, 1.67412,  0.00165607  ])
       era5_lev = create_struct(era5_lev, 'MGWS_str', [ 'northward_gravity_wave_surface_stress', 'N m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'MSL', [980, 1030.,0.01]) ; 97312.2, 103705.,  100961.  ])
       era5_lev = create_struct(era5_lev, 'MSL_str', [ 'mean_sea_level_pressure', 'Pa'  ])

       era5_lev = create_struct(era5_lev, 'NSSS', lev_tau) ;[ -1.27825, 1.54068,  0.00409788  ])
       era5_lev = create_struct(era5_lev, 'NSSS_str', [ 'northward_turbulent_surface_stress', 'N m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'O3', [0., 16., 1.0e6]) ; 1.57502e-08, 1.68008e-05,  2.18622e-06  ])
       era5_lev = create_struct(era5_lev, 'O3_str', [ 'ozone_mass_mixing_ratio', 'kg kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'PEV', [-1., 5, -scale_precip]) ;[ -1.42991e-07, 6.28045e-09,  -6.81265e-09  ])
       era5_lev = create_struct(era5_lev, 'PEV_str', [ 'potential_evaporation', 'm'  ])

       era5_lev = create_struct(era5_lev, 'PV', [-3., 3., 1.e5]) ; -0.0147285, 0.0127170,  -6.99101e-06  ])
       era5_lev = create_struct(era5_lev, 'PV_str', [ 'potential_vorticity', 'K m**2 kg**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'Q', lev_Q) ;[ 1.07237e-06, 0.0228532,  0.00175161  ])
       era5_lev = create_struct(era5_lev, 'Q_str', [ 'specific_humidity', 'kg kg**-1'  ])

       era5_lev = create_struct(era5_lev, 'R', [0., 100.,1]) ; 3.19473e-05, 108.113,  41.2435  ])
       era5_lev = create_struct(era5_lev, 'R_str', [ 'relative_humidity', '%'  ])

       era5_lev = create_struct(era5_lev, 'RO', [0.,1, scale_precip]) ;  -1.71207e-14, 4.33309e-07,  2.32008e-09  ])
       era5_lev = create_struct(era5_lev, 'RO_str', [ 'runoff', 'm'  ])

       era5_lev = create_struct(era5_lev, 'RSN', [100., 300,1.]) ; 100.000, 398.538,  129.676  ])
       era5_lev = create_struct(era5_lev, 'RSN_str', [ 'snow_density', 'kg m**-3'  ])

       era5_lev = create_struct(era5_lev, 'SD', [0., 100, 1000]) ; -9.81518e-07, 10.0000,  1.17294  ])
       era5_lev = create_struct(era5_lev, 'SD_str', [ 'snow_depth', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'SDFOR', [0., 600., 1]) ; -1.99080e-05, 555.136,  14.0733  ])
       era5_lev = create_struct(era5_lev, 'SDFOR_str', [ 'standard_deviation_of_filtered_subgrid_orography', 'm'  ])

       era5_lev = create_struct(era5_lev, 'SDOR', [0., 600,1]) ; -2.37671e-05, 671.150,  20.3905  ])
       era5_lev = create_struct(era5_lev, 'SDOR_str', [ 'standard_deviation_of_orography', '-'  ])

       era5_lev = create_struct(era5_lev, 'SF', [0., 5, scale_precip]) ; -4.32445e-15, 1.60708e-07,  4.25890e-09  ])
       era5_lev = create_struct(era5_lev, 'SF_str', [ 'snowfall', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'SKT', lev_Ts) ;[ 205.561, 314.696,  278.432  ])
       era5_lev = create_struct(era5_lev, 'SKT_str', [ 'skin_temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'SLHF', [-20., 220, -1]) ; -403.666, 22.6779,  -60.5549  ])
       era5_lev = create_struct(era5_lev, 'SLHF_str', [ 'surface_latent_heat_flux', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SLOR', [0., 0.12, 1]) ;[ 9.99936e-05, 0.116923,  0.00345456  ])
       era5_lev = create_struct(era5_lev, 'SLOR_str', [ 'slope_of_sub_gridscale_orography', '-'  ])

       era5_lev = create_struct(era5_lev, 'SLT', [0., 7, 1]) ; -2.38419e-07, 7.00000,  0.670377  ])
       era5_lev = create_struct(era5_lev, 'SLT_str', [ 'soil_type', 'index'  ])

       era5_lev = create_struct(era5_lev, 'SMLT', [0., 1.5, scale_precip]) ; -5.78954e-15, 1.59240e-07,  7.51837e-10  ])
       era5_lev = create_struct(era5_lev, 'SMLT_str', [ 'snowmelt', 'm of water equvalent'  ])

       era5_lev = create_struct(era5_lev, 'SP', [900., 1030, 0.01]) ; 50936.0, 102731.,  96607.4  ])
       era5_lev = create_struct(era5_lev, 'SP_str', [ 'surface_pressure', 'Pa'  ])

       era5_lev = create_struct(era5_lev, 'SRC', [0., 0.7, 1000.]) ; mm -3.22863e-11, 0.000707968,  3.14427e-05  ])
       era5_lev = create_struct(era5_lev, 'SRC_str', [ 'skin_reservoir_content', 'm of water equivalent'  ])

       era5_lev = create_struct(era5_lev, 'SRO', [0., 3., scale_precip]) ; -7.59093e-15, 3.28837e-07,  7.92023e-10  ])
       era5_lev = create_struct(era5_lev, 'SRO_str', [ 'surface_runoff', 'm'  ])

       era5_lev = create_struct(era5_lev, 'SSHF', [-20., 220, -1] ) ; -221.993, 71.6357,  -11.3219  ])
       era5_lev = create_struct(era5_lev, 'SSHF_str', [ 'surface_sensible_heat_flux', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SSR', [0., 350.,1]) ; -6.75502e-06, 352.754,  128.719  ])
       era5_lev = create_struct(era5_lev, 'SSR_str', [ 'surface_net_solar_radiation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SSRC', [0., 350., 1]) ; -9.66272e-06, 370.278,  169.995  ])
       era5_lev = create_struct(era5_lev, 'SSRC_str', [ 'surface_net_solar_radiation_clear_sky', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SSRD', [0., 400,1]) ; -1.78520e-08, 460.982,  163.879  ])
       era5_lev = create_struct(era5_lev, 'SSRD_str', [ 'surface_solar_radiation_downwards', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SSRDC', [0., 400., 1]);  -3.08472e-08, 472.020,  212.674  ])
       era5_lev = create_struct(era5_lev, 'SSRDC_str', [ 'surface_solar_radiation_downward_clear_sky', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SSRO', [ 0., 2, scale_precip]) ;-6.27542e-15, 2.37539e-07,  1.52802e-09  ])
       era5_lev = create_struct(era5_lev, 'SSRO_str', [ 'sub_surface_runoff', 'm'  ])

       era5_lev = create_struct(era5_lev, 'STL1', lev_Ts) ;[ 210.919, 314.787,  279.123  ])
       era5_lev = create_struct(era5_lev, 'STL1_str', [ 'soil_temperature_level_1', 'K'  ])

       era5_lev = create_struct(era5_lev, 'STL2', lev_Ts) ;[ 211.089, 313.096,  279.227  ])
       era5_lev = create_struct(era5_lev, 'STL2_str', [ 'soil_temperature_level_2', 'K'  ])

       era5_lev = create_struct(era5_lev, 'STL3', lev_Ts) ;[ 211.644, 311.807,  279.599  ])
       era5_lev = create_struct(era5_lev, 'STL3_str', [ 'soil_temperature_level_3', 'K'  ])

       era5_lev = create_struct(era5_lev, 'STL4', lev_Ts) ;[ 212.219, 311.609,  279.079  ])
       era5_lev = create_struct(era5_lev, 'STL4_str', [ 'soil_temperature_level_4', 'K'  ])

       era5_lev = create_struct(era5_lev, 'STR', [-20., 150, -1] ) ; -187.747, 13.3110,  -53.1484  ])
       era5_lev = create_struct(era5_lev, 'STR_str', [ 'surface_net_thermal_radiation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'STRC', [-20., 150, -1]) ; -195.259, -18.4429,  -81.5799  ])
       era5_lev = create_struct(era5_lev, 'STRC_str', [ 'surface_net_thermal_radiation_clear_sky', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'STRD', [70, 470,1]) ; 71.5206, 458.263,  299.249  ])
       era5_lev = create_struct(era5_lev, 'STRD_str', [ 'surface_thermal_radiation_downwards', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'STRDC', [ 70., 470.,1]) ;58.8363, 454.369,  270.421  ])
       era5_lev = create_struct(era5_lev, 'STRDC_str', [ 'surface_thermal_radiation_downward_clear_sky', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'SWVL1', lev_swvl) ; -3.04796e-06, 0.720757,  0.0853331  ])
       era5_lev = create_struct(era5_lev, 'SWVL1_str', [ 'volumetric_soil_water_layer_1', 'm**3 m**-3'  ])

       era5_lev = create_struct(era5_lev, 'SWVL2', lev_swvl) ;[ -2.35597e-06, 0.731798,  0.0868543  ])
       era5_lev = create_struct(era5_lev, 'SWVL2_str', [ 'volumetric_soil_water_layer_2', 'm**3 m**-3'  ])

       era5_lev = create_struct(era5_lev, 'SWVL3', lev_swvl) ;[ -1.12438e-05, 0.734315,  0.0846852  ])
       era5_lev = create_struct(era5_lev, 'SWVL3_str', [ 'volumetric_soil_water_layer_3', 'm**3 m**-3'  ])

       era5_lev = create_struct(era5_lev, 'SWVL4', lev_swvl) ;[ -4.00111e-08, 0.741979,  0.0844144  ])
       era5_lev = create_struct(era5_lev, 'SWVL4_str', [ 'volumetric_soil_water_layer_4', 'm**3 m**-3'  ])

       era5_lev = create_struct(era5_lev, 'T', lev_T) ;[ 182.326, 315.190,  245.629  ])
       era5_lev = create_struct(era5_lev, 'T_str', [ 'temperature', 'K'  ])

       era5_lev = create_struct(era5_lev, 'TCC', lev_cld) ;[ 0.00299907, 0.994317,  0.676323  ])
       era5_lev = create_struct(era5_lev, 'TCC_str', [ 'total_cloud_cover', '(0-1)'  ])

       era5_lev = create_struct(era5_lev, 'TCIW', [0., 100., 1000]) ; -6.00730e-13, 0.165214,  0.0221393  ])
       era5_lev = create_struct(era5_lev, 'TCIW_str', [ 'total_column_cloud_ice_water', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCLW', [0., 100., 1000]) ; -7.15753e-09, 0.508306,  0.0474581  ])
       era5_lev = create_struct(era5_lev, 'TCLW_str', [ 'total_column_cloud_liquid_water', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCO3', [0., 10., 1000]) ; 0.00418874, 0.0102261,  0.00642851  ])
       era5_lev = create_struct(era5_lev, 'TCO3_str', [ 'total_column_ozone', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCRW', [0., 100., 1000]) ; -5.89255e-09, 0.421033,  0.00542906  ])
       era5_lev = create_struct(era5_lev, 'TCRW_str', [ 'total_column_rain_water', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCSLW', [0., 100., 1000]);  -1.75812e-09, 0.172717,  0.0208662  ])
       era5_lev = create_struct(era5_lev, 'TCSLW_str', [ 'total_column_supercooled_liquid_water', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCSW', [0., 200., 1000]) ; -1.29204e-09, 1.08898,  0.0374203  ])
       era5_lev = create_struct(era5_lev, 'TCSW_str', [ 'total_column_snow_water', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCW', [0., 70, 1]) ; 0.136460, 68.4107,  18.3025  ])
       era5_lev = create_struct(era5_lev, 'TCW_str', [ 'total_column_water', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TCWV', [0., 70.,1]) ; 0.133771, 67.9120,  18.1900  ])
       era5_lev = create_struct(era5_lev, 'TCWV_str', [ 'total_column_water_vapour', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'TISR', [0., 550,1]) ; -7.70004e-08, 549.173,  297.785  ])
       era5_lev = create_struct(era5_lev, 'TISR_str', [ 'toa_incident_solar_radiation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'TP', [0., 12.,scale_precip]);  -1.14696e-15, 6.43350e-07,  2.74133e-08  ])
       era5_lev = create_struct(era5_lev, 'TP_str', [ 'total_precipitation', 'm'  ])

       era5_lev = create_struct(era5_lev, 'TPLB', [0., 1200., 1]) ; -1.00001, 1242.00,  111.271  ])
       era5_lev = create_struct(era5_lev, 'TPLB_str', [ 'trapping_layer_base_height', 'm'  ])

       era5_lev = create_struct(era5_lev, 'TPLT', [0., 1200, 1]) ; -1.00002, 1392.77,  130.459  ])
       era5_lev = create_struct(era5_lev, 'TPLT_str', [ 'trapping_layer_top_height', 'm'  ])

       era5_lev = create_struct(era5_lev, 'TSN', lev_Ts) ;[ 205.984, 306.606,  275.349  ])
       era5_lev = create_struct(era5_lev, 'TSN_str', [ 'temperature_of_snow_layer', 'K'  ])

       era5_lev = create_struct(era5_lev, 'TSR', [0., 420., 1]) ; -5.13336e-08, 447.555,  197.000  ])
       era5_lev = create_struct(era5_lev, 'TSR_str', [ 'top_net_solar_radiation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'TSRC', [0., 480., 1]) ; -3.85638e-06, 464.916,  235.796  ])
       era5_lev = create_struct(era5_lev, 'TSRC_str', [ 'top_net_solar_radiation_clear_sky', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'TTR', [100., 320, -1]) ; -344.643, -107.398,  -225.977  ])
       era5_lev = create_struct(era5_lev, 'TTR_str', [ 'top_net_thermal_radiation', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'TTRC', [100, 320., -1]) ;, -104.965,  -245.071  ])
       era5_lev = create_struct(era5_lev, 'TTRC_str', [ 'top_net_thermal_radiation_clear_sky', 'W m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'TVH', [0., 20,1]) ; -9.53674e-07, 19.0000,  1.81845  ])
       era5_lev = create_struct(era5_lev, 'TVH_str', [ 'type_of_high_vegetation', 'index'  ])

       era5_lev = create_struct(era5_lev, 'TVL', [0., 20, 1]) ; -9.68575e-07, 17.0000,  1.40215  ])
       era5_lev = create_struct(era5_lev, 'TVL_str', [ 'type_of_low_vegetation', 'index'  ])

       era5_lev = create_struct(era5_lev, 'U', lev_uu) ; -66.7152, 121.431,  6.21210  ])
       era5_lev = create_struct(era5_lev, 'U_str', [ 'u_component_of_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'U10N',[-10.,10.,1]) ; [ -12.6987, 10.9413,  -0.0639038  ])
       era5_lev = create_struct(era5_lev, 'U10N_str', [ '10m_u-component_of_neutral_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'UVB', [0., 50, 1]) ; -4.00234e-09, 52.2063,  19.4474  ])
       era5_lev = create_struct(era5_lev, 'UVB_str', [ 'downward_uv_radiation_at_the_surface', 'w m**-2 s'  ])

       era5_lev = create_struct(era5_lev, 'V', [-5.,5,1] ) ;[ -39.3547, 38.7322,  0.0308609  ])
       era5_lev = create_struct(era5_lev, 'V_str', [ 'v_component_of_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'V10N', [-10.,10.,1]) ;[ -10.1226, 15.3034,  0.173605  ])
       era5_lev = create_struct(era5_lev, 'V10N_str', [ '10m_v-component_of_neutral_wind', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIEC', [ -4000., 4000.,1]) ;-4301.75, 3474.77,  -1.18916  ])
       era5_lev = create_struct(era5_lev, 'VIEC_str', [ 'vertical_integral_of_energy_conversion', 'W m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIGD', [ -4000., 4000., 1]) ;-3336.49, 4317.91,  -1.45807  ])
       era5_lev = create_struct(era5_lev, 'VIGD_str', [ 'vertical_integral_of_divergence_of_geopotential_flux', 'W m**-2'  ])

;       era5_lev = create_struct(era5_lev, 'VIGE', lev_01) ;[-10.,25, 1.0e-9   ]) ;-1.33116e+10  3.11167e+10  6.22845e+09 
;       era5_lev = create_struct(era5_lev, 'VIGE_str', [ 'vertical_integral_of_eastward_geopotential_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIGN', [-10., 10., 1.0e-9]) ; -9.92874e+09, 7.76670e+09,  -3.78285e+07  ])
       era5_lev = create_struct(era5_lev, 'VIGN_str', [ 'vertical_integral_of_northward_geopotential_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIIWD', [-1., 1., 1.0e5]) ; -1.64640e-05, 1.03025e-05,  -3.13287e-08  ])
       era5_lev = create_struct(era5_lev, 'VIIWD_str', [ 'vertical_integral_of_divergence_of_cloud_frozen_water_flux', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIIWE', [-1.5,1.5,1]) ; -1.06221, 1.88846,  0.186032  ])
       era5_lev = create_struct(era5_lev, 'VIIWE_str', [ 'vertical_integral_of_eastward_cloud_frozen_water_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIIWN', [ -1.5,1.5,1]) ;-1.11989, 1.53357,  -0.0164832  ])
       era5_lev = create_struct(era5_lev, 'VIIWN_str', [ 'vertical_integral_of_northward_cloud_frozen_water_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIKE', [0., 600., 1.0e-4]) ; 169934., 1.07776e+07,  1.61646e+06  ])
       era5_lev = create_struct(era5_lev, 'VIKE_str', [ 'vertical_integral_of_kinetic_energy', 'J m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIKED', [-100., 100., 1]) ; -279.425, 219.284,  -0.103907  ])
       era5_lev = create_struct(era5_lev, 'VIKED_str', [ 'vertical_integral_of_divergence_of_kinetic_energy_flux', 'W m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIKEE', [ -300., 300., 1.0e-6]) ;-3.23770e+07, 7.09124e+08,  4.15543e+07  ])
       era5_lev = create_struct(era5_lev, 'VIKEE_str', [ 'vertical_integral_of_eastward_kinetic_energy_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIKEN', [ -70., 70., 1.0e-6]) ;-5.97113e+07, 9.66151e+07,  -88970.4  ])
       era5_lev = create_struct(era5_lev, 'VIKEN_str', [ 'vertical_integral_of_northward_kinetic_energy_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VILWD', [ -2., 2., 1.0e6]) ;-3.31159e-05, 2.88742e-05,  -2.07966e-08  ])
       era5_lev = create_struct(era5_lev, 'VILWD_str', [ 'vertical_integral_of_divergence_of_cloud_liquid_water_flux', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VILWE', [ -3, 3, 1]) ;-2.29355, 4.58888,  0.189921  ])
       era5_lev = create_struct(era5_lev, 'VILWE_str', [ 'vertical_integral_of_eastward_cloud_liquid_water_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VILWN', [ -3,3,1]) ;-2.06009, 3.13350,  -0.0106804  ])
       era5_lev = create_struct(era5_lev, 'VILWN_str', [ 'vertical_integral_of_northward_cloud_liquid_water_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIMA', [5.,12,1.0e-3]) ; 5194.02, 10475.6,  9850.20  ])
       era5_lev = create_struct(era5_lev, 'VIMA_str', [ 'vertical_integral_of_mass_of_atmosphere', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIMAD', [-15, 15., 1.0e3]) ; -0.0135343, 0.0178951,  -2.77875e-06  ])
       era5_lev = create_struct(era5_lev, 'VIMAD_str', [ 'vertical_integral_of_divergence_of_mass_flux', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIMAE', [-100., 100., 1.0e-4]) ; -96229.1, 366164.,  66036.1  ])
       era5_lev = create_struct(era5_lev, 'VIMAE_str', [ 'vertical_integral_of_eastward_mass_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIMAN', [-100., 100., 1.0e-3]) ; -104010., 90226.6,  -9.33865  ])
       era5_lev = create_struct(era5_lev, 'VIMAN_str', [ 'vertical_integral_of_northward_mass_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIMAT', [-20., 20, 1.0e6]) ; -0.000100500, 8.49876e-05,  -1.10710e-07  ])
       era5_lev = create_struct(era5_lev, 'VIMAT_str', [ 'vertical_integral_of_mass_tendency', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIMD', [-100., 100., 1.0e6]) ; -0.000665154, 0.000116930,  -2.89950e-06  ])
       era5_lev = create_struct(era5_lev, 'VIMD_str', [ 'vertically_integrated_moisture_divergence', 'kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIOZD', [ -2.,2, 1.0e9]) ;-5.07461e-09, 7.55915e-09,  -1.07995e-10  ])
       era5_lev = create_struct(era5_lev, 'VIOZD_str', [ 'vertical_integral_of_divergence_of_ozone_flux', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIOZE', [ -0.4, 0.4,1]) ;-0.142945, 0.457793,  0.0360483  ])
       era5_lev = create_struct(era5_lev, 'VIOZE_str', [ 'vertical_integral_of_eastward_ozone_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIOZN', [ -0.2,0.2,1]) ;-0.178827, 0.161077,  -2.56982e-05  ])
       era5_lev = create_struct(era5_lev, 'VIOZN_str', [ 'vertical_integral_of_northward_ozone_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIPIE', [1.5, 3.0, 1.e-9]) ; 1.44106e+09, 2.71706e+09,  2.48453e+09  ])
       era5_lev = create_struct(era5_lev, 'VIPIE_str', [ 'vertical_integral_of_potential_and_internal_energy', 'J m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIPILE', [1.5,3.0,1.0e-9]) ; 1.44141e+09, 2.86284e+09,  2.53006e+09  ])
       era5_lev = create_struct(era5_lev, 'VIPILE_str', [ 'vertical_integral_of_potential_internal_and_latent_energy', 'J m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VIT', [1.2, 2.8, 1.0e-6]) ; 1.17584e+06, 2.70294e+06,  2.43992e+06  ])
       era5_lev = create_struct(era5_lev, 'VIT_str', [ 'vertical_integral_of_temperature', 'K kg m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VITHE', [1.2, 3.0, 1.e-9]) ; 1.18151e+09, 2.72044e+09,  2.45567e+09  ])
       era5_lev = create_struct(era5_lev, 'VITHE_str', [ 'vertical_integral_of_thermal_energy', 'J m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VITHED', [-1,1,1.0e-3]) ; -3633.11, 6753.64,  -19.7976  ])
       era5_lev = create_struct(era5_lev, 'VITHED_str', [ 'vertical_integral_of_divergence_of_thermal_energy_flux', 'W m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VITHEE', [-20.,80,1.0e-9])  ; -2.41184e+10  8.83902e+10  1.54451e+10
       era5_lev = create_struct(era5_lev, 'VITHEE_str', [ 'vertical_integral_of_eastward_heat_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VITHEN', [-20.,20,1.e-9]) ;-2.40788e+10  2.17789e+10  1.74904e+06
       era5_lev = create_struct(era5_lev, 'VITHEN_str', [ 'vertical_integral_of_northward_heat_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VITOE', [1.5,3.0,1.0e-9]) ; 1.44190e+09, 2.86357e+09,  2.53168e+09  ])
       era5_lev = create_struct(era5_lev, 'VITOE_str', [ 'vertical_integral_of_total_energy', 'J m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VITOED', [-0.5,0.5,1.e-3]) ; -3491.90, 5441.73,  -27.0124  ])
       era5_lev = create_struct(era5_lev, 'VITOED_str', [ 'vertical_integral_of_divergence_of_total_energy_flux', 'W m**-2'  ])

       era5_lev = create_struct(era5_lev, 'VITOEE', [-30.,150, 1.0e-9]) ;-3.54894e+10  1.20539e+11  2.17484e+10 
       era5_lev = create_struct(era5_lev, 'VITOEE_str', [ 'vertical_integral_of_eastward_total_energy_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VITOEN', [-30.,30, 1.0e-9]) ; -3.39405e+10  2.88500e+10 -6.09869e+06
       era5_lev = create_struct(era5_lev, 'VITOEN_str', [ 'vertical_integral_of_northward_total_energy_flux', 'W m**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIWVD', [ -0.1,0.1,1.e3]) ;-0.000589213, 0.000168549,  -2.26152e-06  ])
       era5_lev = create_struct(era5_lev, 'VIWVD_str', [ 'vertical_integral_of_divergence_of_moisture_flux', 'kg m**-2 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIWVE', [-0.4,0.4,1.0e-3]) ; -581.488, 591.817,  13.3291  ])
       era5_lev = create_struct(era5_lev, 'VIWVE_str', [ 'vertical_integral_of_eastward_water_vapour_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VIWVN', [ -0.2,0.2,1.0e-3]) ;-295.058, 381.238,  0.912056  ])
       era5_lev = create_struct(era5_lev, 'VIWVN_str', [ 'vertical_integral_of_northward_water_vapour_flux', 'kg m**-1 s**-1'  ])

       era5_lev = create_struct(era5_lev, 'VO', [ -10., 10, 1.0e5]) ;0.000328945, 0.000349132,  -3.82312e-07  ])
       era5_lev = create_struct(era5_lev, 'VO_str', [ 'vorticity', 's**-1'  ])

       era5_lev = create_struct(era5_lev, 'W', [ -2, 2, 36]); -1.01263, 2.14538,  0.00424482  ])
       era5_lev = create_struct(era5_lev, 'W_str', [ 'vertical_velocity', 'Pa s**-1'  ])

       era5_lev = create_struct(era5_lev, 'ZS', [ -200., 5200.,1]) ;2049.18, 494884.,  124668.  ])
       era5_lev = create_struct(era5_lev, 'ZS_str', [ 'geopotential', 'm**2 s**-2'  ])

       era5_lev = create_struct(era5_lev, 'Z', [ -2000, 500000, 1.]) ;2049.18, 494884.,  124668.  ])
       era5_lev = create_struct(era5_lev, 'Z_str', [ 'orography', 'm**2 s**-2'  ])

       era5_lev = create_struct(era5_lev, 'ZUST', [ 0., 0.7, 1]) ;0.0185246, 0.709606,  0.259037  ])
       era5_lev = create_struct(era5_lev, 'ZUST_str', [ 'friction_velocity', 'm s**-1'  ])

       era5_lev = create_struct(era5_lev, 'FLNA', [-250.,-50,1 ]) 
       era5_lev = create_struct(era5_lev, 'FLNA_str', 'FLNA ')

       era5_lev = create_struct(era5_lev, 'FSNA', [0., 120., 1]) 
       era5_lev = create_struct(era5_lev, 'FSNA_str', 'FSNA')

       era5_lev = create_struct(era5_lev, 'FLNAC', [-240., -70.,1]) 
       era5_lev = create_struct(era5_lev, 'FLNAC_str', 'FLNAC')

       era5_lev = create_struct(era5_lev, 'FSNAC', [0., 120, 1]) 
       era5_lev = create_struct(era5_lev, 'FSNAC_str', 'FSNAC')

       era5_lev = create_struct(era5_lev, 'SRFRAD', [-100, 300, 1]) 
       era5_lev = create_struct(era5_lev, 'SRFRAD_str', 'SRFRAD')

       era5_lev = create_struct(era5_lev, 'SRFRADC', [-100., 300,1]) 
       era5_lev = create_struct(era5_lev, 'SRFRADC_str', 'SRFRADC')

       era5_lev = create_struct(era5_lev, 'TOARAD', [-200., 200, 1]) 
       era5_lev = create_struct(era5_lev, 'TOARAD_str', 'TOARAD')

       era5_lev = create_struct(era5_lev, 'TOARADC', [-200., 200,1]) 
       era5_lev = create_struct(era5_lev, 'TOARADC_str', 'TOARADC')

       era5_lev = create_struct(era5_lev, 'ATMRAD', [-170., -10, 1]) 
       era5_lev = create_struct(era5_lev, 'ATMRAD_str', 'ATMRAD')

       era5_lev = create_struct(era5_lev, 'ATMRADC', [-170.,-10,1.]) 
       era5_lev = create_struct(era5_lev, 'ATMRADC_str', 'ATMRADC')

       era5_lev = create_struct(era5_lev, 'LWCF', [-10., 100, 1]) 
       era5_lev = create_struct(era5_lev, 'LWCF_str', 'LWCF')

       era5_lev = create_struct(era5_lev, 'SWCF', [-200.,30, .1]) 
       era5_lev = create_struct(era5_lev, 'SWCF_str', 'SWCF')

       era5_lev = create_struct(era5_lev, 'LWCFS', [-20.,80,1]) 
       era5_lev = create_struct(era5_lev, 'LWCFS_str', 'LWCFS')

       era5_lev = create_struct(era5_lev, 'SWCFS', [-200., 30,1]) 
       era5_lev = create_struct(era5_lev, 'SWCFS_str', 'SWCFS')

       era5_lev = create_struct(era5_lev, 'LWCFA', [-60.,90, 1]) 
       era5_lev = create_struct(era5_lev, 'LWCFA_str', 'LWCFA')

       era5_lev = create_struct(era5_lev, 'SWCFA', [-10.,20,1]) 
       era5_lev = create_struct(era5_lev, 'SWCFA_str', 'SWCFA')

       era5_lev = create_struct(era5_lev, 'TOTCRF', [-180.,40,1]) 
       era5_lev = create_struct(era5_lev, 'TOTCRF_str', 'TOTCRF')

       era5_lev = create_struct(era5_lev, 'TOTCRFS', [-100.,60,1]) 
       era5_lev = create_struct(era5_lev, 'TOTCRFS_str', 'TOTCRFS')

       era5_lev = create_struct(era5_lev, 'TOTCRFA', [-60.,60,1.]) 
       era5_lev = create_struct(era5_lev, 'TOTCRFA_str', 'TOTCRFA')

       era5_lev = create_struct(era5_lev, 'PME', [-10., 15.,1.]) 
       era5_lev = create_struct(era5_lev, 'PME_str', 'PME')

       era5_lev = create_struct(era5_lev, 'SRFNET', [-400.,400.,1]) 
       era5_lev = create_struct(era5_lev, 'SRFNET_str', 'SRFNET')

       era5_lev = create_struct(era5_lev, 'ATMNET', [-200.,200,1.]) 
       era5_lev = create_struct(era5_lev, 'ATMNET_str', 'ATMNET')

       era5_lev = create_struct(era5_lev, 'H', [260.,450,1.]) 
       era5_lev = create_struct(era5_lev, 'H_str', 'H')

       era5_lev = create_struct(era5_lev, 'THETA', [260.,450.,1.]) 
       era5_lev = create_struct(era5_lev, 'THETA_str', 'THETA')


;
   vars_all = strupcase(era5_vars.vars_era5_all)
   vars_all =  vars_all(uniq(vars_all))
   scale_era5 = create_struct('vars',vars_all)

   names = tag_names(era5_lev)

   for iv = 0, n_elements(vars_all)-1 do begin
    var0 = vars_all[iv]
    if(belongsto(var0, era5_vars.vars_number))then var0 = 'VAR_'+var0
    if(belongsto(var0,names))then begin
     dlev = get_stru(era5_lev,var0) 
     scale_era5 = create_struct(scale_era5,var0, dlev[2])
    endif else begin
     scale_era5 = create_struct(scale_era5,var0, 1.0)
    endelse
   ; print,iv, var0
   endfor

   save,file = 'scale_era5.sav0', scale_era5, era5_lev
  
end
