subroutine write_ini(FILE_NAME)
!----------------------------------------------------------------------------------
! Purpose: write atmosphere initial data (lat-lon grid)
! Author : ZhangHe
! Complete: 2007.6.5
! Update: 2011.12.01, ZhangHe
!         2011.12.24, ZhangHe
!----------------------------------------------------------------------------------
   use netcdf
   use Var_name_inic 
   
   implicit none
!------------------------------------ Arguments ----------------------------------
! This is the name of the data file we will write.
   character(*), intent(in) :: FILE_NAME      ! file name needed to read
  integer :: ncid2

! We are writing 4D data, a 26 x 181 x 360 lvl-lat-lon grid, with 1 timesteps of data.
! 
!--------------------------------- Local workspace --------------------------------
  integer :: lon_dimid, lat_dimid, lev_dimid, ilev_dimid, tim_dimid, chr_dimid

! The start and count arrays will tell the netCDF library where to
! write our data.
  integer :: start1d(2), count1d(2)
  integer :: start2d(3), count2d(3)
  integer :: start3d(4), count3d(4)

! for dimension ids
  integer :: dim1ids(1)
  integer :: dim2ids(2)
  integer :: dim3ids(3)
  integer :: dim4ids(4)
  integer :: dimids(NDIMS)

! for global attributes
  character (len = *),parameter :: str = 'CF-1.0'
  character (len = *),parameter :: caseid = 'iap128x256'
  character (len = *),parameter :: ctitle = '...'
  character (len = *),parameter :: logname = 'zhangh'
  character (len = *),parameter :: host = ''
!!  character (len = *),parameter :: history = 'FRI JUN 1 15:31:30 2007: ncks -A -x -v              &
!!&  ndbase,nsbase,nbdate,nbsec,ndcur,nscur,date,datesec,nsteph     & 
!!&  eul128x256_d50_ic.cam2.i.0050-09-01-00000.nc                   &
!!&  cami_0000-09-01_128x256_L26_c040422.nc'
  character (len = *),parameter :: note = 'Processed using /fs/cgd/home0/jmccaa/scripts/process_ic_files.csh'
  character (len = *),parameter :: history = 'DEC 24 2011'

!----------------------------------------------------------------------------------

! Create the file. 
  call check( nf90_create(FILE_NAME, nf90_clobber, ncid2) )

! Define the dimensions. The record dimension is defined to have
! unlimited length - it can grow as needed. In this example it is
! the time dimension.
  call check( nf90_def_dim(ncid2, 'lat', NLAT, lat_dimid) )
  call check( nf90_def_dim(ncid2, 'lon', NLON, lon_dimid) )
  call check( nf90_def_dim(ncid2, 'lev', NLEV, lev_dimid) )
  call check( nf90_def_dim(ncid2, 'ilev', NLVP, ilev_dimid) )
  call check( nf90_def_dim(ncid2, 'chars', NCHR, chr_dimid) )
  call check( nf90_def_dim(ncid2, 'time', NF90_UNLIMITED, tim_dimid) )

!  define all the dimension ids
  dim1ids(1) = tim_dimid

  dim2ids(1) = chr_dimid 
  dim2ids(2) = tim_dimid 

  dim3ids(1) = lon_dimid 
  dim3ids(2) = lat_dimid 
  dim3ids(3) = tim_dimid 

  dim4ids(1) = lon_dimid 
  dim4ids(2) = lat_dimid 
  dim4ids(3) = lev_dimid 
  dim4ids(4) = tim_dimid 
!
! Define the variables' dimension
!
! for coordinate
  call check( nf90_def_var(ncid2, 'lat',  NF90_DOUBLE, lat_dimid, lat_varid) )
  call check( nf90_def_var(ncid2, 'lon',  NF90_DOUBLE, lon_dimid, lon_varid) )
  call check( nf90_def_var(ncid2, 'lev',  NF90_DOUBLE, lev_dimid, lev_varid) )
  call check( nf90_def_var(ncid2, 'ilev', NF90_DOUBLE, ilev_dimid, ilev_varid) )
  call check( nf90_def_var(ncid2, 'time', NF90_DOUBLE, tim_dimid, time_varid) )
! for coefficient related to coordinate
  call check( nf90_def_var(ncid2, 'gw',   NF90_DOUBLE, lat_dimid,  gw_varid) )
  call check( nf90_def_var(ncid2, 'hyai', NF90_DOUBLE, ilev_dimid, hyai_varid) )
  call check( nf90_def_var(ncid2, 'hyam', NF90_DOUBLE, lev_dimid,  hyam_varid) )
  call check( nf90_def_var(ncid2, 'hybi', NF90_DOUBLE, ilev_dimid, hybi_varid) )
  call check( nf90_def_var(ncid2, 'hybm', NF90_DOUBLE, lev_dimid,  hybm_varid) )
! for integer
  call check( nf90_def_var(ncid2, 'ndbase', NF90_INT, varid=ndb_varid) )
  call check( nf90_def_var(ncid2, 'nsbase', NF90_INT, varid=nsb_varid) )
  call check( nf90_def_var(ncid2, 'nbdate', NF90_INT, varid=nbd_varid) )
  call check( nf90_def_var(ncid2, 'nbsec ', NF90_INT, varid=nbs_varid) )
  call check( nf90_def_var(ncid2, 'ndcur',  NF90_INT, dim1ids, ndc_varid) )
  call check( nf90_def_var(ncid2, 'nscur',  NF90_INT, dim1ids, nsc_varid) )
  call check( nf90_def_var(ncid2, 'date' ,  NF90_INT, dim1ids, date_varid) )
  call check( nf90_def_var(ncid2, 'datesec', NF90_INT, dim1ids, dats_varid) )
  call check( nf90_def_var(ncid2, 'nsteph', NF90_INT, dim1ids, nst_varid) )
  call check( nf90_def_var(ncid2, 'mdt',    NF90_INT, varid=mdt_varid) )
  call check( nf90_def_var(ncid2, 'ntrk',    NF90_INT, varid=ntrk_varid) )
  call check( nf90_def_var(ncid2, 'ntrm',    NF90_INT, varid=ntrm_varid) )
  call check( nf90_def_var(ncid2, 'ntrn',    NF90_INT, varid=ntrn_varid) )
! for double real 
!!  call check( nf90_def_var(ncid2, 'CLDICE',  NF90_DOUBLE, dim4ids, CLDI_varid) )
!!  call check( nf90_def_var(ncid2, 'CLDLIQ',  NF90_DOUBLE, dim4ids, CLDL_varid) )
!!  call check( nf90_def_var(ncid2, 'CLOUD',   NF90_DOUBLE, dim4ids, CLD_varid) )
!!  call check( nf90_def_var(ncid2, 'ICEFRAC', NF90_DOUBLE, dim3ids, IFR_varid) )
!!  call check( nf90_def_var(ncid2, 'LANDFRAC', NF90_DOUBLE, dim3ids, LFR_varid) )
!!  call check( nf90_def_var(ncid2, 'LANDM_COSLAT', NF90_DOUBLE, dim3ids, LDC_varid) )
!!  call check( nf90_def_var(ncid2, 'LCWAT',   NF90_DOUBLE, dim4ids, LCW_varid) )
!!!  call check( nf90_def_var(ncid2, 'CWAT',   NF90_DOUBLE, dim4ids, CWAT_varid) )
  call check( nf90_def_var(ncid2, 'P0',      NF90_DOUBLE,     varid=P0_varid) )
!!  call check( nf90_def_var(ncid2, 'PBLH',    NF90_DOUBLE, dim3ids, PBLH_varid) )
!!  call check( nf90_def_var(ncid2, 'PHIS',    NF90_DOUBLE, dim3ids, PHIS_varid) )
  call check( nf90_def_var(ncid2, 'PS',      NF90_DOUBLE, dim3ids, PS_varid) )
  call check( nf90_def_var(ncid2, 'Q',       NF90_DOUBLE, dim4ids, Q_varid) )
!!  call check( nf90_def_var(ncid2, 'QCWAT',   NF90_DOUBLE, dim4ids, QCW_varid) )
!!  call check( nf90_def_var(ncid2, 'QPERT',   NF90_DOUBLE, dim3ids, QPER_varid) )
!!  call check( nf90_def_var(ncid2, 'SGH',     NF90_DOUBLE, dim3ids, SGH_varid) )
!!  call check( nf90_def_var(ncid2, 'SICTHK',  NF90_DOUBLE, dim3ids, SICT_varid) )
  call check( nf90_def_var(ncid2, 'SNOWHICE', NF90_DOUBLE, dim3ids, SNOW_varid) )
  call check( nf90_def_var(ncid2, 'T',       NF90_DOUBLE, dim4ids, T_varid) )
!!  call check( nf90_def_var(ncid2, 'TBOT',    NF90_DOUBLE, dim3ids, TBOT_varid) )
!!  call check( nf90_def_var(ncid2, 'TCWAT',   NF90_DOUBLE, dim4ids, TCW_varid) )
!!  call check( nf90_def_var(ncid2, 'TPERT',   NF90_DOUBLE, dim3ids, TPER_varid) )
!!  call check( nf90_def_var(ncid2, 'TS',      NF90_DOUBLE, dim3ids, TS_varid) )
  call check( nf90_def_var(ncid2, 'TS1',     NF90_DOUBLE, dim3ids, TS1_varid) )
  call check( nf90_def_var(ncid2, 'TS2',     NF90_DOUBLE, dim3ids, TS2_varid) )
  call check( nf90_def_var(ncid2, 'TS3',     NF90_DOUBLE, dim3ids, TS3_varid) )
  call check( nf90_def_var(ncid2, 'TS4',     NF90_DOUBLE, dim3ids, TS4_varid) )
  call check( nf90_def_var(ncid2, 'TSICE',   NF90_DOUBLE, dim3ids, TSIC_varid) )
!!  call check( nf90_def_var(ncid2, 'TSICERAD', NF90_DOUBLE, dim3ids, TIRD_varid) )
!!  call check( nf90_def_var(ncid2, 'TSOCN',   NF90_DOUBLE, dim3ids, TSOC_varid) )
  call check( nf90_def_var(ncid2, 'U',       NF90_DOUBLE, dim4ids, U_varid) )
  call check( nf90_def_var(ncid2, 'V',       NF90_DOUBLE, dim4ids, V_varid) )
! for character
  call check( nf90_def_var(ncid2, 'date_written', NF90_CHAR, dim2ids, dwr_varid) )
  call check( nf90_def_var(ncid2, 'time_written', NF90_CHAR, dim2ids, twr_varid) )
!!
!
  print *,"*** SUCCESS define variables' dimension (writing) "
!------------------------------------------------------------------------------
! Assign long_name attributes to all the variables.
  call check( nf90_put_att(ncid2, lat_varid, "long_name", lat_lname) )
  call check( nf90_put_att(ncid2, lon_varid, "long_name", lon_lname) )
  call check( nf90_put_att(ncid2, lev_varid, "long_name", lev_lname) )
  call check( nf90_put_att(ncid2, ilev_varid, "long_name", ilev_lname) )
  call check( nf90_put_att(ncid2, time_varid, "long_name", time_lname) )
!
  call check( nf90_put_att(ncid2, ndb_varid, "long_name", ndb_lname) )
  call check( nf90_put_att(ncid2, nsb_varid, "long_name", nsb_lname) )
  call check( nf90_put_att(ncid2, nbd_varid, "long_name", nbd_lname) )
  call check( nf90_put_att(ncid2, nbs_varid, "long_name", nbs_lname) )
  call check( nf90_put_att(ncid2, ndc_varid, "long_name", ndc_lname) )
  call check( nf90_put_att(ncid2, nsc_varid, "long_name", nsc_lname) )
  call check( nf90_put_att(ncid2, date_varid, "long_name", date_lname) )
  call check( nf90_put_att(ncid2, dats_varid, "long_name", dats_lname) )
  call check( nf90_put_att(ncid2, nst_varid, "long_name", nst_lname) )
  call check( nf90_put_att(ncid2, ntrk_varid, "long_name", ntrk_lname) )
  call check( nf90_put_att(ncid2, ntrm_varid, "long_name", ntrm_lname) )
  call check( nf90_put_att(ncid2, ntrn_varid, "long_name", ntrn_lname) )
!
!!  call check( nf90_put_att(ncid2, CLDI_varid, "long_name", CLDI_lname) )
!!  call check( nf90_put_att(ncid2, CLDL_varid, "long_name", CLDL_lname) )
!!  call check( nf90_put_att(ncid2, CLD_varid,  "long_name", CLD_lname) )
!!  call check( nf90_put_att(ncid2, IFR_varid,  "long_name", IFR_lname) )
!!  call check( nf90_put_att(ncid2, LFR_varid,  "long_name", LFR_lname) )
!!!  call check( nf90_put_att(ncid2, CWAT_varid,  "long_name", CWAT_lname) )
  call check( nf90_put_att(ncid2, P0_varid,   "long_name", P0_lname) )
!!  call check( nf90_put_att(ncid2, PBLH_varid, "long_name", PBLH_lname) )
!!  call check( nf90_put_att(ncid2, PHIS_varid, "long_name", PHIS_lname) )
  call check( nf90_put_att(ncid2, PS_varid,   "long_name", PS_lname) )
  call check( nf90_put_att(ncid2, Q_varid,    "long_name", Q_lname) )
!!  call check( nf90_put_att(ncid2, QPER_varid, "long_name", QPER_lname) )
!!  call check( nf90_put_att(ncid2, SGH_varid,  "long_name", SGH_lname) )
!!  call check( nf90_put_att(ncid2, SICT_varid, "long_name", SICT_lname) )
  call check( nf90_put_att(ncid2, SNOW_varid, "long_name", SNOW_lname) )
  call check( nf90_put_att(ncid2, T_varid,    "long_name", T_lname) )
!!  call check( nf90_put_att(ncid2, TBOT_varid, "long_name", TBOT_lname) )
!!  call check( nf90_put_att(ncid2, TPER_varid, "long_name", TPER_lname) )
!!  call check( nf90_put_att(ncid2, TS_varid,   "long_name", TS_lname) )
  call check( nf90_put_att(ncid2, TS1_varid,  "long_name", TS1_lname) )
  call check( nf90_put_att(ncid2, TS2_varid,  "long_name", TS2_lname) )
  call check( nf90_put_att(ncid2, TS3_varid,  "long_name", TS3_lname) )
  call check( nf90_put_att(ncid2, TS4_varid,  "long_name", TS4_lname) )
  call check( nf90_put_att(ncid2, TSIC_varid, "long_name", TSIC_lname) )
  call check( nf90_put_att(ncid2, U_varid,    "long_name", U_lname) )
  call check( nf90_put_att(ncid2, V_varid,    "long_name", V_lname) )
!
  call check( nf90_put_att(ncid2, gw_varid,    "long_name", gw_lname) )
  call check( nf90_put_att(ncid2, hyai_varid,  "long_name", hyai_lname) )
  call check( nf90_put_att(ncid2, hyam_varid,  "long_name", hyam_lname) )
  call check( nf90_put_att(ncid2, hybi_varid,  "long_name", hybi_lname) )
  call check( nf90_put_att(ncid2, hybm_varid,  "long_name", hybm_lname) )
!
  call check( nf90_put_att(ncid2, mdt_varid,  "long_name", mdt_lname) )
!
! Assign units & other attributes to all the variables.
  call check( nf90_put_att(ncid2, lat_varid, "units", lat_units) )
  call check( nf90_put_att(ncid2, lon_varid, "units", lon_units) )
!
  call check( nf90_put_att(ncid2, lev_varid, "units", lev_units) )
  call check( nf90_put_att(ncid2, lev_varid, "positive", "down") )
  call check( nf90_put_att(ncid2, lev_varid, "standard_name", "atmosphere_sigma_pressure_coordinate") )
!
  call check( nf90_put_att(ncid2, ilev_varid, "units", ilev_units) )
  call check( nf90_put_att(ncid2, ilev_varid, "positive", "down") )
  call check( nf90_put_att(ncid2, ilev_varid, "standard_name", "atmosphere_sigma_pressure_coordinate") )
!
  call check( nf90_put_att(ncid2, time_varid, "units", time_units) )
  call check( nf90_put_att(ncid2, time_varid, "calendar", "noleap") )
!!
!!  call check( nf90_put_att(ncid2, CLDI_varid, "units", CLDI_units) )
!!  call check( nf90_put_att(ncid2, CLDL_varid, "units", CLDL_units) )
!!  call check( nf90_put_att(ncid2, CLD_varid,  "units", CLD_units) )
!!  call check( nf90_put_att(ncid2, IFR_varid,  "units", IFR_units) )
!!  call check( nf90_put_att(ncid2, LFR_varid,  "units", LFR_units) )
!!!  call check( nf90_put_att(ncid2, CWAT_varid,  "units", CWAT_units) )
  call check( nf90_put_att(ncid2, P0_varid,   "units", P0_units) )
!!  call check( nf90_put_att(ncid2, PBLH_varid, "units", PBLH_units) )
!!  call check( nf90_put_att(ncid2, PHIS_varid, "units", PHIS_units) )
  call check( nf90_put_att(ncid2, PS_varid,   "units", PS_units) )
  call check( nf90_put_att(ncid2, Q_varid,    "units", Q_units) )
!!  call check( nf90_put_att(ncid2, QPER_varid, "units", QPER_units) )
!!  call check( nf90_put_att(ncid2, SGH_varid,  "units", SGH_units) )
!!  call check( nf90_put_att(ncid2, SICT_varid, "units", SICT_units) )
  call check( nf90_put_att(ncid2, SNOW_varid, "units", SNOW_units) )
  call check( nf90_put_att(ncid2, T_varid,    "units", T_units) )
!!  call check( nf90_put_att(ncid2, TBOT_varid, "units", TBOT_units) )
!!  call check( nf90_put_att(ncid2, TPER_varid, "units", TPER_units) )
!!  call check( nf90_put_att(ncid2, TS_varid,   "units", TS_units) )
  call check( nf90_put_att(ncid2, TS1_varid,  "units", TS1_units) )
  call check( nf90_put_att(ncid2, TS2_varid,  "units", TS2_units) )
  call check( nf90_put_att(ncid2, TS3_varid,  "units", TS3_units) )
  call check( nf90_put_att(ncid2, TS4_varid,  "units", TS4_units) )
  call check( nf90_put_att(ncid2, TSIC_varid, "units", TSIC_units) )
  call check( nf90_put_att(ncid2, U_varid,    "units", U_units) )
  call check( nf90_put_att(ncid2, V_varid,    "units", V_units) )
  call check( nf90_put_att(ncid2, mdt_varid,  "units", mdt_units) )
!
! Global header information 
!
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'Conventions', str) )
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'source', 'IAP AGCM') )
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'case', caseid) )
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'title',ctitle) )
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'logname',logname) )
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'host', host) )
  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'history', history) )
!!  call check( nf90_put_att(ncid2, NF90_GLOBAL, 'Note', note) )

!
  print *,"*** SUCCESS put attributes (writing) "
!
! End define mode.
  call check( nf90_enddef(ncid2) )
!------------------------------------------------------------------------------

! define start & count
  start1d = (/1, 1/)
  count1d = (/NCHR, 1/)

  start2d = (/1, 1, 1/)
  count2d = (/NLON, NLAT, 1/)

  start3d = (/1, 1, 1, 1/)
  count3d = (/NLON, NLAT, NLEV, 1/)

! Write the coordinate variable data. This will put the latitudes
! and longitudes of our data grid into the netCDF file.
  call check( nf90_put_var(ncid2, lat_varid, lat_out) )
  call check( nf90_put_var(ncid2, lon_varid, lon_out) )
  call check( nf90_put_var(ncid2, lev_varid, lev_out) )
  call check( nf90_put_var(ncid2, ilev_varid, ilev_out) )
  call check( nf90_put_var(ncid2, time_varid, time_out) )
! for scale var
  call check( nf90_put_var(ncid2, ndb_varid, ndbase_out) )
  call check( nf90_put_var(ncid2, nsb_varid, nsbase_out) )
  call check( nf90_put_var(ncid2, nbd_varid, nbdate_out) )
  call check( nf90_put_var(ncid2, nbs_varid, nbsec_out) )
  call check( nf90_put_var(ncid2, P0_varid,  P0_out) )
! for 1D var (time)
  call check( nf90_put_var(ncid2, ndc_varid, ndcur_out) )
  call check( nf90_put_var(ncid2, nsc_varid, nscur_out) )
  call check( nf90_put_var(ncid2, date_varid, date_out) )
  call check( nf90_put_var(ncid2, dats_varid, datesec_out) )
  call check( nf90_put_var(ncid2, nst_varid, nsteph_out) )
  call check( nf90_put_var(ncid2, mdt_varid, mdt_out) )
  call check( nf90_put_var(ncid2, ntrk_varid, ntrk_out) )
  call check( nf90_put_var(ncid2, ntrm_varid, ntrm_out) )
  call check( nf90_put_var(ncid2, ntrn_varid, ntrn_out) )
! for 2D var (NLON, NLAT)
!!  call check( nf90_put_var(ncid2, IFR_varid,  ICEFRAC_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, LFR_varid,  LANDFRAC_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, LDC_varid,  LANDM_COSLAT_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, PBLH_varid, PBLH_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, PHIS_varid, PHIS_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, PS_varid,   PS_out,   start2d, count2d) )
!!  call check( nf90_put_var(ncid2, QPER_varid, QPERT_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, SGH_varid,  SGH_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, SICT_varid, SICTHK_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, SNOW_varid, SNOWHICE_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, TBOT_varid, TBOT_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, TPER_varid, TPERT_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, TS_varid,   TS_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, TS1_varid,  TS1_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, TS2_varid,  TS2_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, TS3_varid,  TS3_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, TS4_varid,  TS4_out, start2d, count2d) )
  call check( nf90_put_var(ncid2, TSIC_varid, TSICE_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, TIRD_varid, TSICERAD_out, start2d, count2d) )
!!  call check( nf90_put_var(ncid2, TSOC_varid, TSOCN_out, start2d, count2d) )
! for 3D var (NLON, NLEV, NLAT)
!!  call check( nf90_put_var(ncid2, CLDI_varid, CLDICE_out, start3d, count3d) )
!!  call check( nf90_put_var(ncid2, CLDL_varid, CLDLIQ_out, start3d, count3d) )
!!  call check( nf90_put_var(ncid2, CLD_varid,  CLOUD_out,  start3d, count3d) )
!!  call check( nf90_put_var(ncid2, LCW_varid, LCWAT_out, start3d, count3d) )
!!!  call check( nf90_put_var(ncid2, CWAT_varid, CWAT_out, start3d, count3d) )
  call check( nf90_put_var(ncid2, Q_varid, Q_out, start3d, count3d) )
!!  call check( nf90_put_var(ncid2, QCW_varid, QCWAT_out, start3d, count3d) )
  call check( nf90_put_var(ncid2, T_varid, T_out, start3d, count3d) )
!!  call check( nf90_put_var(ncid2, TCW_varid, TCWAT_out, start3d, count3d) )
  call check( nf90_put_var(ncid2, U_varid, U_out, start3d, count3d) )
  call check( nf90_put_var(ncid2, V_varid, V_out, start3d, count3d) )
! for 1D var (NLAT, or NLEV, or NLVP)
  call check( nf90_put_var(ncid2, gw_varid, gw_out) )
  call check( nf90_put_var(ncid2, hyai_varid, hyai_out) )
  call check( nf90_put_var(ncid2, hyam_varid, hyam_out) )
  call check( nf90_put_var(ncid2, hybi_varid, hybi_out) )
  call check( nf90_put_var(ncid2, hybm_varid, hybm_out) )
! for 1D var (NCHR)
  call check( nf90_put_var(ncid2, dwr_varid, date_written_out, start1d, count1d) )
  call check( nf90_put_var(ncid2, twr_varid, time_written_out, start1d, count1d) )
!         
! Close the file. This frees up any internal netCDF resources
! associated with the file.
  call check( nf90_close(ncid2) )

! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS writing initial file ", FILE_NAME, "!"
!----------------------------------------------------------------------------------

end 
