subroutine read_ini(FILE_NAME)
!----------------------------------------------------------------------------------
! Purpose: read atmosphere initial data (Gaussian grid)
! Author : ZhangHe
! Complete: 2007.6.2
! Update: revise a severe error, TSICERAD ==> TSOCN
!         2010.07.19
!         2011.12.01, for IAP5
!----------------------------------------------------------------------------------
   use netcdf
   use Var_name_inic 
   
   implicit none
!------------------------------------ Arguments ----------------------------------
! This is the name of the data file we will read.
   character(*), intent(in) :: FILE_NAME      ! file name needed to read
   integer :: ncid1

! We are reading 4D data, a 26 x 181 x 360 lvl-lat-lon grid, with 1 timesteps of data.

!----------------------------------------------------------------------------------

! Open the file (read only access). 
  call check( nf90_open(FILE_NAME, nf90_nowrite, ncid1) )

! Get the varids of all the variables.
  call check( nf90_inq_varid(ncid1, 'lat', lat_varid) )
  call check( nf90_inq_varid(ncid1, 'lon', lon_varid) )
  call check( nf90_inq_varid(ncid1, 'lev', lev_varid) )
  call check( nf90_inq_varid(ncid1, 'ilev', ilev_varid) )
  call check( nf90_inq_varid(ncid1, 'time', time_varid) )
!!
  call check( nf90_inq_varid(ncid1, 'ndbase', ndb_varid) )
  call check( nf90_inq_varid(ncid1, 'nsbase', nsb_varid) )
  call check( nf90_inq_varid(ncid1, 'nbdate', nbd_varid) )
  call check( nf90_inq_varid(ncid1, 'nbsec' , nbs_varid) )
  call check( nf90_inq_varid(ncid1, 'ndcur' , ndc_varid) )
  call check( nf90_inq_varid(ncid1, 'nscur' , nsc_varid) )
  call check( nf90_inq_varid(ncid1, 'date',  date_varid) )
  call check( nf90_inq_varid(ncid1, 'datesec', dats_varid) )
  call check( nf90_inq_varid(ncid1, 'nsteph', nst_varid) )
!!
  print *,"**** SUCCESS get id nsteph ***** "
!
!!  call check( nf90_inq_varid(ncid1, 'CLDICE', CLDI_varid) )
!!  call check( nf90_inq_varid(ncid1, 'CLDLIQ', CLDL_varid) )
!!  call check( nf90_inq_varid(ncid1, 'CLOUD', CLD_varid) )
!!  call check( nf90_inq_varid(ncid1, 'ICEFRAC', IFR_varid) )
!!  call check( nf90_inq_varid(ncid1, 'LANDFRAC', LFR_varid) )
!!  call check( nf90_inq_varid(ncid1, 'LANDM_COSLAT', LDC_varid) )
!!  call check( nf90_inq_varid(ncid1, 'LCWAT', LCW_varid) )
!!!  call check( nf90_inq_varid(ncid1, 'CWAT', CWAT_varid) )
  call check( nf90_inq_varid(ncid1, 'P0', P0_varid) )
!!  call check( nf90_inq_varid(ncid1, 'PBLH', PBLH_varid) )
!!  call check( nf90_inq_varid(ncid1, 'PHIS', PHIS_varid) )
  call check( nf90_inq_varid(ncid1, 'PS', PS_varid) )
  call check( nf90_inq_varid(ncid1, 'Q', Q_varid) )
!
  print *,"**** SUCCESS get id Q ***** "
!
!!  call check( nf90_inq_varid(ncid1, 'QCWAT', QCW_varid) )
!!  call check( nf90_inq_varid(ncid1, 'QPERT', QPER_varid) )
!!  call check( nf90_inq_varid(ncid1, 'SGH', SGH_varid) )
!!  call check( nf90_inq_varid(ncid1, 'SICTHK', SICT_varid) )
  call check( nf90_inq_varid(ncid1, 'SNOWHICE', SNOW_varid) )
  call check( nf90_inq_varid(ncid1, 'T', T_varid) )
!!  call check( nf90_inq_varid(ncid1, 'TBOT', TBOT_varid) )
!!  call check( nf90_inq_varid(ncid1, 'TCWAT', TCW_varid) )
!!  call check( nf90_inq_varid(ncid1, 'TPERT', TPER_varid) )
!!  call check( nf90_inq_varid(ncid1, 'TS', TS_varid) )
  call check( nf90_inq_varid(ncid1, 'TS1', TS1_varid) )
  call check( nf90_inq_varid(ncid1, 'TS2', TS2_varid) )
  call check( nf90_inq_varid(ncid1, 'TS3', TS3_varid) )
  call check( nf90_inq_varid(ncid1, 'TS4', TS4_varid) )
  call check( nf90_inq_varid(ncid1, 'TSICE', TSIC_varid) )
!
  print *,"**** SUCCESS get id TSICE ***** "
!
!!  call check( nf90_inq_varid(ncid1, 'TSICERAD', TIRD_varid) )
!!  call check( nf90_inq_varid(ncid1, 'TSOCN', TSOC_varid) )
  call check( nf90_inq_varid(ncid1, 'U', U_varid) )
  call check( nf90_inq_varid(ncid1, 'V', V_varid) )
!
  call check( nf90_inq_varid(ncid1, 'gw', gw_varid) )
  call check( nf90_inq_varid(ncid1, 'hyai', hyai_varid) )
  call check( nf90_inq_varid(ncid1, 'hyam', hyam_varid) )
  call check( nf90_inq_varid(ncid1, 'hybi', hybi_varid) )
  call check( nf90_inq_varid(ncid1, 'hybm', hybm_varid) )
!
!
  print *,"**** SUCCESS get id hybm ***** "
!
  call check( nf90_inq_varid(ncid1, 'mdt', mdt_varid) )
  call check( nf90_inq_varid(ncid1, 'date_written', dwr_varid) )
  call check( nf90_inq_varid(ncid1, 'time_written', twr_varid) )
!
  print *,"*** SUCCESS get variables'id (reading) "
!
! Read all the variable data.
  call check( nf90_get_var(ncid1, lat_varid, lat_in) )
  call check( nf90_get_var(ncid1, lon_varid, lon_in) )
  call check( nf90_get_var(ncid1, lev_varid, lev_in) )
  call check( nf90_get_var(ncid1, ilev_varid, ilev_in) )
  call check( nf90_get_var(ncid1, time_varid, time_in) )
!!
!!  print *,"*** SUCCESS get time_in "
!
  call check( nf90_get_var(ncid1, ndb_varid, ndbase_in) )
  call check( nf90_get_var(ncid1, nsb_varid, nsbase_in) )
  call check( nf90_get_var(ncid1, nbd_varid, nbdate_in) )
  call check( nf90_get_var(ncid1, nbs_varid, nbsec_in) )
  call check( nf90_get_var(ncid1, ndc_varid, ndcur_in) )
  call check( nf90_get_var(ncid1, nsc_varid, nscur_in) )
  call check( nf90_get_var(ncid1, date_varid, date_in) )
  call check( nf90_get_var(ncid1, dats_varid, datesec_in) )
  call check( nf90_get_var(ncid1, nst_varid, nsteph_in) )
!!
  print *,"*** SUCCESS get nsteph_in "
!
!!  call check( nf90_get_var(ncid1, CLDI_varid, CLDICE_in) )
!!  call check( nf90_get_var(ncid1, CLDL_varid, CLDLIQ_in) )
!!  call check( nf90_get_var(ncid1, CLD_varid, CLOUD_in) )
!!  call check( nf90_get_var(ncid1, IFR_varid, ICEFRAC_in) )
!!  call check( nf90_get_var(ncid1, LFR_varid, LANDFRAC_in) )
!!  call check( nf90_get_var(ncid1, LDC_varid, LANDM_COSLAT_in) )
!!  call check( nf90_get_var(ncid1, LCW_varid, LCWAT_in) )
!!  call check( nf90_get_var(ncid1, CWAT_varid, CWAT_in) )
  call check( nf90_get_var(ncid1, P0_varid, P0_in) )
!!  call check( nf90_get_var(ncid1, PBLH_varid, PBLH_in) )
!!  call check( nf90_get_var(ncid1, PHIS_varid, PHIS_in) )
  call check( nf90_get_var(ncid1, PS_varid, PS_in) )
  call check( nf90_get_var(ncid1, Q_varid, Q_in) )
!!  call check( nf90_get_var(ncid1, QCW_varid, QCWAT_in) )
!!  call check( nf90_get_var(ncid1, QPER_varid, QPERT_in) )
!!  call check( nf90_get_var(ncid1, SGH_varid, SGH_in) )
!!  call check( nf90_get_var(ncid1, SICT_varid, SICTHK_in) )

  print *,"*** SUCCESS get Q_in "
!
  call check( nf90_get_var(ncid1, SNOW_varid, SNOWHICE_in) )
  call check( nf90_get_var(ncid1, T_varid, T_in) )
!!  call check( nf90_get_var(ncid1, TBOT_varid, TBOT_in) )
!!  call check( nf90_get_var(ncid1, TCW_varid, TCWAT_in) )
!!  call check( nf90_get_var(ncid1, TPER_varid, TPERT_in) )
!!  call check( nf90_get_var(ncid1, TS_varid, TS_in) )
  call check( nf90_get_var(ncid1, TS1_varid, TS1_in) )
  call check( nf90_get_var(ncid1, TS2_varid, TS2_in) )
  call check( nf90_get_var(ncid1, TS3_varid, TS3_in) )
  call check( nf90_get_var(ncid1, TS4_varid, TS4_in) )
  call check( nf90_get_var(ncid1, TSIC_varid, TSICE_in) )
!!  call check( nf90_get_var(ncid1, TIRD_varid, TSICERAD_in) )
!!  call check( nf90_get_var(ncid1, TSOC_varid, TSOCN_in) )    !!
  call check( nf90_get_var(ncid1, U_varid, U_in) )
  call check( nf90_get_var(ncid1, V_varid, V_in) )
!
  print *,"*** SUCCESS get V_in "
!
  call check( nf90_get_var(ncid1, gw_varid, gw_in) )
  call check( nf90_get_var(ncid1, hyai_varid, hyai_in) )
  call check( nf90_get_var(ncid1, hyam_varid, hyam_in) )
  call check( nf90_get_var(ncid1, hybi_varid, hybi_in) )
  call check( nf90_get_var(ncid1, hybm_varid, hybm_in) )
!
  print *,"*** SUCCESS get hybm_in "
!
  call check( nf90_get_var(ncid1, mdt_varid, mdt_in) )
  call check( nf90_get_var(ncid1, dwr_varid, date_written_in) )
  call check( nf90_get_var(ncid1, twr_varid, time_written_in) )
!
! Close the file. This frees up any internal netCDF resources
! associated with the file.
  call check( nf90_close(ncid1) )

! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS reading initial file ", FILE_NAME, "!"
!----------------------------------------------------------------------------------

end 
