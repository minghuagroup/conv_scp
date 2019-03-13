module Var_name_inic
!----------------------------------------------------------------------------------
! Purpose: Set variables and their attributes in atmosphere initial data
! Author : ZhangHe
! Complete: 2007.5.29
! Update: 2011.12.01
!----------------------------------------------------------------------------------
!!   use precision, only: r8
   use Grid
   
   implicit none
!----------------------------------------------------------------------------------
! variables' long_names in atmosphere initial data 
   character (len = *), parameter :: lat_lname  = "latitude"
   character (len = *), parameter :: lon_lname  = "longitude"
   character (len = *), parameter :: lev_lname  = "sigma level at midpoints "
   character (len = *), parameter :: ilev_lname = "sigma level at interfaces "
   character (len = *), parameter :: time_lname = "time"
!
   character (len = *), parameter :: ndb_lname  = "base day"
   character (len = *), parameter :: nsb_lname  = "seconds of base day"
   character (len = *), parameter :: nbd_lname  = "base date (YYYYMMDD)"
   character (len = *), parameter :: nbs_lname  = "seconds of base date"
   character (len = *), parameter :: ndc_lname  = "current day (from base day)"
   character (len = *), parameter :: nsc_lname  = "current seconds of current day"
   character (len = *), parameter :: date_lname = "current date (YYYYMMDD)"
   character (len = *), parameter :: dats_lname = "current seconds of current date"
   character (len = *), parameter :: nst_lname  = "current timestep"
   character (len = *), parameter :: ntrk_lname  = "spectral truncation parameter K"
   character (len = *), parameter :: ntrm_lname  = "spectral truncation parameter M"
   character (len = *), parameter :: ntrn_lname  = "spectral truncation parameter N"
!
!!   character (len = *), parameter :: CLDI_lname = "Grid box averaged ice condensate amount"
!!   character (len = *), parameter :: CLDL_lname = "Grid box averaged liquid condensate amount"
!!   character (len = *), parameter :: CLD_lname  = "Cloud fraction"
   character (len = *), parameter :: CWAT_lname  = "Total Grid box averaged Condensate Amount (liquid + ice)"
!!   character (len = *), parameter :: IFR_lname  = "Fraction of sfc area covered by sea-ice"
!   character (len = *), parameter :: LFR_lname  = "gridbox land fraction"
!   character (len = *), parameter :: LANDM_lname  = "land ocean transition mask: ocean (0), continent (1), transition (0-1)"
   character (len = *), parameter :: P0_lname   = "reference pressure"
!!   character (len = *), parameter :: PBLH_lname = "PBL height"
   character (len = *), parameter :: PHIS_lname = "Surface geopotential"
   character (len = *), parameter :: PS_lname   = "Surface pressure"
   character (len = *), parameter :: Q_lname    = "Specific humidity"
!!   character (len = *), parameter :: QPER_lname = "Perturbation specific humidity (eddies in PBL)"
!   character (len = *), parameter :: SGH_lname  = "Standard deviation of orography"
!!   character (len = *), parameter :: SICT_lname = "Sea ice thickness"
   character (len = *), parameter :: SNOW_lname = "Water equivalent snow depth"
   character (len = *), parameter :: T_lname    = "Temperature"
!!   character (len = *), parameter :: TBOT_lname = "Lowest model level temperature"
!!   character (len = *), parameter :: TPER_lname = "Perturbation temperature (eddies in PBL)"
   character (len = *), parameter :: TS_lname   = "Surface temperature (radiative)"
   character (len = *), parameter :: TS1_lname  = "TS1      subsoil temperature"
   character (len = *), parameter :: TS2_lname  = "TS2      subsoil temperature"
   character (len = *), parameter :: TS3_lname  = "TS3      subsoil temperature"
   character (len = *), parameter :: TS4_lname  = "TS4      subsoil temperature"
   character (len = *), parameter :: TSIC_lname = "Ice temperature"
   character (len = *), parameter :: U_lname    = "Zonal wind"
   character (len = *), parameter :: V_lname    = "Meridional wind"
!
   character (len = *), parameter :: gw_lname   = "gauss weights"
   character (len = *), parameter :: hyai_lname = "hybrid A coefficient at layer interfaces"
   character (len = *), parameter :: hyam_lname = "hybrid A coefficient at layer midpoints"
   character (len = *), parameter :: hybi_lname = "hybrid B coefficient at layer interfaces"
   character (len = *), parameter :: hybm_lname = "hybrid B coefficient at layer midpoints"
!
   character (len = *), parameter :: mdt_lname  = "timestep"
!!
! variables in atmosphere initial data (gaussian grid)

   real*8 :: lat_in(GLAT)
   real*8 :: lon_in(GLON)
   real*8 :: lev_in(GLEV)
   real*8 :: ilev_in(GLVP)
   real*8 :: time_in  
!
   integer  :: ndbase_in
   integer  :: nsbase_in
   integer  :: nbdate_in
   integer  :: nbsec_in
!
   integer  :: ndcur_in  
   integer  :: nscur_in  
   integer  :: date_in  
   integer  :: datesec_in  
   integer  :: nsteph_in  
!
!!   real*8 :: CLDICE_in(GLON, GLAT, GLEV)
!!   real*8 :: CLDLIQ_in(GLON, GLAT, GLEV)
!!   real*8 :: CLOUD_in (GLON, GLAT, GLEV)
   real*8 :: CWAT_in (GLON, GLAT, GLEV)    !zhh
!!   real*8 :: ICEFRAC_in(GLON, GLAT)
!   real*8 :: LANDFRAC_in(GLON, GLAT)
!   real*8 :: LANDM_in(GLON, GLAT)         !zhh
!   real*8 :: LANDM_COSLAT_in(GLON, GLAT)
!!   real*8 :: LCWAT_in(GLON, GLAT, GLEV)
   real*8 :: P0_in
!!   real*8 :: PBLH_in(GLON, GLAT)
   real*8 :: PHIS_in(GLON, GLAT)
   real*8 :: PS_in(GLON, GLAT)
   real*8 :: Q_in(GLON, GLAT, GLEV)
!!   real*8 :: QCWAT_in(GLON, GLAT, GLEV)
!!   real*8 :: QPERT_in(GLON, GLAT)
!   real*8 :: SGH_in(GLON, GLAT)
!!   real*8 :: SICTHK_in(GLON, GLAT)
   real*8 :: SNOWHICE_in(GLON, GLAT)
   real*8 :: T_in(GLON, GLAT, GLEV)
!!   real*8 :: TBOT_in(GLON, GLAT)
!!   real*8 :: TCWAT_in(GLON, GLAT, GLEV)
!!   real*8 :: TPERT_in(GLON, GLAT)
   real*8 :: TS_in(GLON, GLAT)
   real*8 :: TS1_in(GLON, GLAT)
   real*8 :: TS2_in(GLON, GLAT)
   real*8 :: TS3_in(GLON, GLAT)
   real*8 :: TS4_in(GLON, GLAT)
   real*8 :: TSICE_in(GLON, GLAT)
!!   real*8 :: TSICERAD_in(GLON, GLAT)
!!   real*8 :: TSOCN_in(GLON, GLAT)
   real*8 :: U_in(GLON, GLAT, GLEV)
   real*8 :: V_in(GLON, GLAT, GLEV)
!
   real*8 :: gw_in(GLAT)
   real*8 :: hyai_in(GLVP)
   real*8 :: hyam_in(GLEV)
   real*8 :: hybi_in(GLVP)
   real*8 :: hybm_in(GLEV)
!
   integer  :: mdt_in
   character*8 :: date_written_in(NCHR)
   character*8 :: time_written_in(NCHR)
!!
! variables's id
   integer  :: lat_varid
   integer  :: lon_varid
   integer  :: lev_varid
   integer  :: ilev_varid
   integer  :: time_varid
!
   integer  :: ndb_varid
   integer  :: nsb_varid
   integer  :: nbd_varid
   integer  :: nbs_varid
   integer  :: ndc_varid
   integer  :: nsc_varid
   integer  :: date_varid
   integer  :: dats_varid
   integer  :: nst_varid
   integer  :: ntrk_varid
   integer  :: ntrm_varid
   integer  :: ntrn_varid
!
   integer  :: CLDI_varid
   integer  :: CLDL_varid
   integer  :: CLD_varid
   integer  :: IFR_varid
   integer  :: LFR_varid
   integer  :: LDC_varid
   integer  :: LCW_varid
   integer  :: CWAT_varid
   integer  :: P0_varid
   integer  :: PBLH_varid
   integer  :: PHIS_varid
   integer  :: PS_varid
   integer  :: Q_varid
   integer  :: QCW_varid
   integer  :: QPER_varid
   integer  :: SGH_varid
   integer  :: SICT_varid
   integer  :: SNOW_varid
   integer  :: T_varid
   integer  :: TBOT_varid
   integer  :: TCW_varid
   integer  :: TPER_varid
   integer  :: TS_varid
   integer  :: TS1_varid
   integer  :: TS2_varid
   integer  :: TS3_varid
   integer  :: TS4_varid
   integer  :: TSIC_varid
   integer  :: TIRD_varid
   integer  :: TSOC_varid
   integer  :: U_varid
   integer  :: V_varid
!
   integer  :: gw_varid
   integer  :: hyai_varid
   integer  :: hyam_varid
   integer  :: hybi_varid
   integer  :: hybm_varid
!
   integer  :: mdt_varid
   integer  :: dwr_varid
   integer  :: twr_varid

! Attributes (units)
   character (len = *), parameter :: lat_units  = "degrees_north"
   character (len = *), parameter :: lon_units  = "degrees_east"
   character (len = *), parameter :: lev_units  = "level"
   character (len = *), parameter :: ilev_units = "level"
   character (len = *), parameter :: time_units = "days since 0010-09-01 00:00:00"
!
!!   character (len = *), parameter :: CLDI_units = "kg/kg"
!!   character (len = *), parameter :: CLDL_units = "kg/kg"
!!   character (len = *), parameter :: CLD_units  = "fraction"
   character (len = *), parameter :: CWAT_units  = "kg/kg"
!!   character (len = *), parameter :: IFR_units  = "fraction"
!   character (len = *), parameter :: LFR_units  = "fraction"
!   character (len = *), parameter :: LANDM_units  = "none"
   character (len = *), parameter :: P0_units   = "Pa"
!!   character (len = *), parameter :: PBLH_units = "m"
   character (len = *), parameter :: PHIS_units = "m2/s2"
   character (len = *), parameter :: PS_units   = "Pa"
   character (len = *), parameter :: Q_units    = "kg/kg"
!!   character (len = *), parameter :: QPER_units = "kg/kg"
!   character (len = *), parameter :: SGH_units  = "m"
!!   character (len = *), parameter :: SICT_units = "m"
   character (len = *), parameter :: SNOW_units = "m"
   character (len = *), parameter :: T_units    = "K"
!!   character (len = *), parameter :: TBOT_units = "K"
!!   character (len = *), parameter :: TPER_units = "K"
   character (len = *), parameter :: TS_units   = "K"
   character (len = *), parameter :: TS1_units  = "K"
   character (len = *), parameter :: TS2_units  = "K"
   character (len = *), parameter :: TS3_units  = "K"
   character (len = *), parameter :: TS4_units  = "K"
   character (len = *), parameter :: TSIC_units = "K"
   character (len = *), parameter :: U_units    = "m/s"
   character (len = *), parameter :: V_units    = "m/s"
   character (len = *), parameter :: mdt_units  = "s"

! variables to write a new initial data (lat-lon grid)

   real*8 :: lat_out(NLAT)
   real*8 :: lon_out(NLON)
   real*8 :: lev_out(NLEV)
   real*8 :: ilev_out(NLVP)
   real*8 :: time_out  
!
   integer  :: ndbase_out
   integer  :: nsbase_out
   integer  :: nbdate_out
   integer  :: nbsec_out
!
   integer  :: ndcur_out  
   integer  :: nscur_out  
   integer  :: date_out  
   integer  :: datesec_out  
   integer  :: nsteph_out  
!
   integer  :: ntrk_out  
   integer  :: ntrm_out  
   integer  :: ntrn_out  
!
!!   real*8 :: CLDICE_out(NLON, NLAT, NLEV)
!!   real*8 :: CLDLIQ_out(NLON, NLAT, NLEV)
!!   real*8 :: CLOUD_out (NLON, NLAT, NLEV)
   real*8 :: CWAT_out (NLON, NLAT, NLEV)
   real*8 :: CWAT_out_gs (NLON, NLAT, GLEV)!czy
!!   real*8 :: ICEFRAC_out(NLON, NLAT)
!   real*8 :: LANDFRAC_out(NLON, NLAT)
!   real*8 :: LANDM_out(NLON, NLAT)
!   real*8 :: LANDM_COSLAT_out(NLON, NLAT)
!!   real*8 :: LCWAT_out(NLON, NLAT, NLEV)
   real*8 :: P0_out
!!   real*8 :: PBLH_out(NLON, NLAT)
   real*8 :: PHIS_out(NLON, NLAT)
   real*8 :: PS_out(NLON, NLAT)
   real*8 :: Q_out(NLON, NLAT, NLEV)
   real*8 :: Q_out_gs(NLON, NLAT, GLEV)!czy
!!   real*8 :: QCWAT_out(NLON, NLAT, NLEV)
!!   real*8 :: QPERT_out(NLON, NLAT)
!   real*8 :: SGH_out(NLON, NLAT)
!!   real*8 :: SICTHK_out(NLON, NLAT)
   real*8 :: SNOWHICE_out(NLON, NLAT)
   real*8 :: T_out(NLON, NLAT, NLEV)
   real*8 :: T_out_gs(NLON, NLAT, GLEV)!czy
!!   real*8 :: TBOT_out(NLON, NLAT)
!!   real*8 :: TCWAT_out(NLON, NLAT, NLEV)
!!   real*8 :: TPERT_out(NLON, NLAT)
   real*8 :: TS_out(NLON, NLAT)
   real*8 :: TS1_out(NLON, NLAT)
   real*8 :: TS2_out(NLON, NLAT)
   real*8 :: TS3_out(NLON, NLAT)
   real*8 :: TS4_out(NLON, NLAT)
   real*8 :: TSICE_out(NLON, NLAT)
!!   real*8 :: TSICERAD_out(NLON, NLAT)
!!   real*8 :: TSOCN_out(NLON, NLAT)
   real*8 :: U_out(NLON, NLAT, NLEV)
   real*8 :: U_out_gs(NLON, NLAT, GLEV)!czy
   real*8 :: V_out(NLON, NLAT, NLEV)
   real*8 :: V_out_gs(NLON, NLAT, GLEV)!czy
!
   real*8 :: gw_out(NLAT)
   !real*8 :: hyai_out(GLVP)
   !real*8 :: hyam_out(GLEV)
   !real*8 :: hybi_out(GLVP)
   !real*8 :: hybm_out(GLEV)
   !czy20170717
   real*8 :: hyai_out(NLVP)
   real*8 :: hyam_out(NLEV)
   real*8 :: hybi_out(NLVP)
   real*8 :: hybm_out(NLEV)
!
   integer  :: mdt_out
   character*8 :: date_written_out(NCHR)
   character*8 :: time_written_out(NCHR)

end module Var_name_inic
