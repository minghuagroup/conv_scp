
      module mo_sim_dat

      private
      public :: set_sim_dat

      contains

      subroutine set_sim_dat

      use chem_mods,   only : clscnt, cls_rxt_cnt, clsmap, permute, adv_mass, fix_mass, crb_mass
      use chem_mods,   only : diag_map
      use chem_mods,   only : phtcnt, rxt_tag_cnt, rxt_tag_lst, rxt_tag_map
      use chem_mods,   only : pht_alias_lst, pht_alias_mult
      use chem_mods,   only : extfrc_lst, inv_lst, slvd_lst
      use abortutils,  only : endrun
      use mo_tracname, only : solsym
      use chem_mods,   only : frc_from_dataset
      use shr_kind_mod,only : r8 => shr_kind_r8
      use cam_logfile, only : iulog

      implicit none

!--------------------------------------------------------------
!      ... local variables
!--------------------------------------------------------------
      integer :: ios

      clscnt(:) = (/      5,     0,     0,    49,     0 /)

      cls_rxt_cnt(:,1) = (/      2,    11,     0,     5 /)
      cls_rxt_cnt(:,4) = (/      4,    33,    62,    49 /)

      solsym(: 54) = (/ 'O3              ','O               ','O1D             ','N2O             ','N               ', &
                        'NO              ','NO2             ','NO3             ','HNO3            ','HO2NO2          ', &
                        'N2O5            ','CH4             ','CH3O2           ','CH3OOH          ','CH3OH           ', &
                        'CH2O            ','CO              ','H               ','OH              ','HO2             ', &
                        'H2O2            ','CL              ','CL2             ','CLO             ','OCLO            ', &
                        'CL2O2           ','HCL             ','HOCL            ','CLONO2          ','ISOP            ', &
                        'CFC11           ','CFC12           ','CO2             ','SO2             ','DMS             ', &
                        'H2SO4           ','SOAG            ','SOA             ','so4_a1          ','pom_a1          ', &
                        'soa_a1          ','bc_a1           ','dst_a1          ','ncl_a1          ','num_a1          ', &
                        'so4_a2          ','soa_a2          ','ncl_a2          ','num_a2          ','dst_a3          ', &
                        'ncl_a3          ','so4_a3          ','num_a3          ','H2O             ' /)

      adv_mass(: 54) = (/    47.998200_r8,    15.999400_r8,    15.999400_r8,    44.012880_r8,    14.006740_r8, &
                             30.006140_r8,    46.005540_r8,    62.004940_r8,    63.012340_r8,    79.011740_r8, &
                            108.010480_r8,    16.040600_r8,    47.032000_r8,    48.039400_r8,    32.040000_r8, &
                             30.025200_r8,    28.010400_r8,     1.007400_r8,    17.006800_r8,    33.006200_r8, &
                             34.013600_r8,    35.452700_r8,    70.905400_r8,    51.452100_r8,    67.451500_r8, &
                            102.904200_r8,    36.460100_r8,    52.459500_r8,    97.457640_r8,    68.114200_r8, &
                            137.367503_r8,   120.913206_r8,    44.009800_r8,    64.064800_r8,    62.132400_r8, &
                             98.078400_r8,    12.011000_r8,   144.132000_r8,   115.107340_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,   135.064039_r8,    58.442468_r8,     1.007400_r8, &
                            115.107340_r8,    12.011000_r8,    58.442468_r8,     1.007400_r8,   135.064039_r8, &
                             58.442468_r8,   115.107340_r8,     1.007400_r8,    18.014200_r8 /)

      crb_mass(: 54) = (/     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,    60.055000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,     0.000000_r8,    24.022000_r8, &
                              0.000000_r8,    12.011000_r8,   144.132000_r8,     0.000000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8 /)

      fix_mass(:  3) = (/ 0.00000000_r8, 28.0134800_r8, 31.9988000_r8 /)

      clsmap(:  5,1) = (/   12,   4,  31,  32,  33 /)
      clsmap(: 49,4) = (/    1,   2,   3,  17,   5,   6,   7,  19,   8,   9, &
                            10,  11,  13,  14,  16,  18,  20,  21,  54,  22, &
                            23,  24,  25,  26,  27,  28,  29,  30,  15,  34, &
                            35,  38,  36,  37,  39,  40,  41,  42,  43,  44, &
                            45,  46,  47,  48,  49,  50,  51,  52,  53 /)

      permute(: 49,4) = (/   42,  45,  47,  35,   1,  44,  46,  48,  49,   2, &
                             33,  27,  38,  34,  40,  39,  43,  32,  36,  37, &
                              3,  41,   4,  24,   5,  26,  30,  31,  28,  25, &
                             29,   6,   7,   8,   9,  10,  11,  12,  13,  14, &
                             15,  16,  17,  18,  19,  20,  21,  22,  23 /)

      diag_map(: 49) = (/    1,   2,   3,   4,   5,   6,   7,   8,   9,  10, &
                            11,  12,  13,  14,  15,  16,  17,  18,  19,  20, &
                            21,  22,  23,  24,  27,  29,  33,  36,  42,  46, &
                            51,  58,  63,  69,  75,  79,  83,  91, 100, 110, &
                           124, 139, 159, 173, 189, 205, 220, 246, 262 /)

      extfrc_lst(: 10) = (/ 'NO              ','NO2             ','CO              ','SO2             ','so4_a1          ', &
                            'so4_a2          ','pom_a1          ','bc_a1           ','num_a1          ','num_a2          ' /)

      frc_from_dataset(: 10) = (/ .true., .true., .true., .true., .true., &
                                  .true., .true., .true., .true., .true. /)

      inv_lst(:  3) = (/ 'M               ', 'N2              ', 'O2              ' /)

      if( allocated( rxt_tag_lst ) ) then
         deallocate( rxt_tag_lst )
      end if
      allocate( rxt_tag_lst(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_lst; error = ',ios
         call endrun
      end if
      if( allocated( rxt_tag_map ) ) then
         deallocate( rxt_tag_map )
      end if
      allocate( rxt_tag_map(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_map; error = ',ios
         call endrun
      end if
      rxt_tag_lst(:rxt_tag_cnt) = (/ 'jo2_b           ', 'jo3_a           ', 'jo3_b           ', 'jn2o            ', &
                                     'jno2            ', 'jno3_a          ', 'jno3_b          ', 'jho2no2_a       ', &
                                     'jho2no2_b       ', 'jch3ooh         ', 'jch2o_a         ', 'jch2o_b         ', &
                                     'jh2o2           ', 'jhocl           ', 'jclono2_a       ', 'jclono2_b       ', &
                                     'jcfcl3          ', 'jcf2cl2         ', 'usr_O_O2        ', 'O_O3            ', &
                                     'usr_O_O         ', 'O1D_N2          ', 'O1D_O2b         ', 'ox_l1           ', &
                                     'O1D_N2Oa        ', 'O1D_N2Ob        ', 'O1D_O3          ', 'O1D_CFC11       ', &
                                     'O1D_CFC12       ', 'O1D_CH4a        ', 'O1D_CH4b        ', 'O1D_CH4c        ', &
                                     'H_O2            ', 'H_O3            ', 'H_HO2a          ', 'H_HO2b          ', &
                                     'H_HO2c          ', 'OH_O            ', 'ox_l2           ', 'OH_HO2          ', &
                                     'OH_OH           ', 'OH_OH_M         ', 'OH_H2O2         ', 'HO2_O           ', &
                                     'ox_l3           ', 'usr_HO2_HO2     ', 'H2O2_O          ', 'NO_O_M          ', &
                                     'ox_p1           ', 'NO_O3           ', 'NO2_O           ', 'NO2_O_M         ', &
                                     'NO2_O3          ', 'tag_NO2_NO3     ', 'usr_N2O5_M      ', 'tag_NO2_OH      ', &
                                     'NO3_NO          ', 'NO3_O           ', 'NO3_OH          ', 'NO3_HO2         ', &
                                     'tag_NO2_HO2     ', 'HO2NO2_OH       ', 'usr_HO2NO2_M    ', 'CL_O3           ', &
                                     'CL_HO2a         ', 'CL_HO2b         ', 'CLO_O           ', 'CLO_OHa         ', &
                                     'CLO_OHb         ', 'CLO_HO2         ', 'CLO_NO          ', 'CLO_NO2_M       ', &
                                     'CLO_CLOa        ', 'CLO_CLOb        ', 'CLO_CLOc        ', 'tag_CLO_CLO_M   ', &
                                     'usr_CL2O2_M     ', 'CH4_OH          ', 'usr_CO_OH_b     ', 'CO_OH_M         ', &
                                     'CH2O_NO3        ', 'CH2O_OH         ', 'CH2O_O          ', 'ox_p2           ', &
                                     'CH3O2_HO2       ', 'CH3O2_CH3O2a    ', 'CH3O2_CH3O2b    ', 'CH3OH_OH        ', &
                                     'CH3OOH_OH       ', 'usr_N2O5_aer    ', 'usr_NO3_aer     ', 'usr_NO2_aer     ', &
                                     'usr_SO2_OH      ', 'DMS_OHb         ', 'usr_DMS_OH      ', 'DMS_NO3         ', &
                                     'usr_HO2_aer     ' /)
      rxt_tag_map(:rxt_tag_cnt) = (/    1,   2,   3,   4,   5,   6,   7,   8,   9,  10, &
                                       11,  12,  13,  14,  15,  16,  17,  18,  19,  20, &
                                       21,  22,  23,  24,  25,  26,  27,  28,  29,  30, &
                                       31,  32,  33,  34,  35,  36,  37,  38,  39,  40, &
                                       41,  42,  43,  44,  45,  46,  47,  48,  49,  50, &
                                       51,  52,  53,  54,  55,  56,  57,  58,  59,  60, &
                                       61,  62,  63,  64,  65,  66,  67,  68,  69,  70, &
                                       71,  72,  73,  74,  75,  76,  77,  78,  79,  80, &
                                       81,  82,  83,  84,  85,  86,  87,  88,  89,  92, &
                                       93,  94,  95,  96,  97,  98,  99 /)
      if( allocated( pht_alias_lst ) ) then
         deallocate( pht_alias_lst )
      end if
      allocate( pht_alias_lst(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_lst; error = ',ios
         call endrun
      end if
      if( allocated( pht_alias_mult ) ) then
         deallocate( pht_alias_mult )
      end if
      allocate( pht_alias_mult(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_mult; error = ',ios
         call endrun
      end if
      pht_alias_lst(:,1) = (/ 'userdefined     ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ' /)
      pht_alias_lst(:,2) = (/ '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ' /)
      pht_alias_mult(:,1) = (/ 1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8 /)
      pht_alias_mult(:,2) = (/ 1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8 /)

      end subroutine set_sim_dat

      end module mo_sim_dat
