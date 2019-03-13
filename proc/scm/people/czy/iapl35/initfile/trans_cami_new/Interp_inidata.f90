program Interp_inidata
!----------------------------------------------------------------------------------
! Purpose: Interpolation all the atmosphere initial datas from Gaussian grid 
!          to lat-lon grid
! Author : ZhangHe
! Complete: 2007.6.5
! Update: 2011.12.24
!----------------------------------------------------------------------------------
   use netcdf
   use Grid
   use Var_name_inic
   use prognostic, only: SIG, SIGL
   
   implicit none
!--------------------------------- Local workspace --------------------------------
   real*8 :: ZERO, PI, wt, DLAT
   real*8 :: wlat(NLAT)
   integer  :: I, J, K
!
   character(50) :: path1      !  
   character(50) :: fname1      !  
   character(100) :: FILE_NAME1      ! file name to read 
   character(100) :: FILE_NAME2      ! file name to write 
!---------------------------------------------------------------------------
   integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real

   !integer, parameter :: nlev = 51
   !integer, parameter :: nilev = nlev + 1
   real(r8), dimension(1:NLEV)  :: lev_tmp, hyam_tmp, hybm_tmp, sigm_tmp 
   real(r8), dimension(1:NLVP)  :: ilev_tmp, hyai_tmp, hybi_tmp, sigi_tmp 
!---------------------------------------------------------------------------
   ZERO = 0.0D0
!
   path1 = './'
   fname1 = 'cami_0000-01-01_64x128_L30_c090102.nc'
   FILE_NAME1 = trim(path1)//fname1

!   FILE_NAME2 = 'IAPi_0000-01-01_128x256_L30_c120110.nc'
   FILE_NAME2 = 'IAPi_0000-01-01_128x256_L35_c180626.nc'

!  read atmosphere initial data (Gaussian grid)
   call read_ini(FILE_NAME1)
!---------------------------------------------------------------------------
!!   print*, 'before change direction:' 
!!   print*, 'V_in(1,26,2)=', V_in(1,26,2), '     V_in(1,26,127)=', V_in(1,26,127) 
!   
   print*, 'before Inp_gs_ll'
! ===================================== zhh ======================================
   write(*,*) 'U_in(102,81,1) =', U_in(102,81,1), ' V_in(102,81,1) =', V_in(102,81,1)
   write(*,*) 'U_in(102,81,26) =', U_in(102,81,26), ' V_in(102,81,26) =', V_in(102,81,26)
   write(*,*) 'T_in(2,88,1) =', T_in(2,88,1), ' T_in(2,88,26) =', T_in(2,88,26)
   write(*,*) 'PS_in(7,2)     =', PS_in(7,2)
   write(*,*) 'PHIS_in(209,80) =', PHIS_in(209,80), 'PHIS_in(210,80) =', PHIS_in(210,80)
! ================================= 2007.11.4 =====================================

!-----------------------------------------------------------------
! Interpolation variables from Gaussian grid to lat-lon grid
   call Inp_gs_ll
   print*, '**** SUCCESS horizontal interpolate'
! ===================================== zhh ======================================
   write(*,*) 'U_out(102,81,1) =', U_out(102,81,1), ' V_out(102,81,1) =', V_out(102,81,1)
   write(*,*) 'U_out(102,81,26) =', U_out(102,81,26), ' V_out(102,81,26) =', V_out(102,81,26)
   write(*,*) 'T_out(2,88,1) =', T_out(2,88,1), ' T_out(2,88,26) =', T_out(2,88,26)
   write(*,*) 'PS_out(7,2)     =', PS_out(7,2)
   write(*,*) 'PHIS_out(209,80) =', PHIS_out(209,80), 'PHIS_out(210,80) =', PHIS_out(210,80)
! ================================= 2007.11.4 =====================================
! ===================================== czy ======================================
   write(*,*) 'U_out_gs(102,81,1) =', U_out_gs(102,81,1), ' V_out(102,81,1) =', V_out_gs(102,81,1)
   write(*,*) 'U_out_gs(102,81,26) =', U_out_gs(102,81,26), ' V_out(102,81,26) =', V_out_gs(102,81,26)
   write(*,*) 'T_out_gs(2,88,1) =', T_out_gs(2,88,1), ' T_out(2,88,26) =', T_out_gs(2,88,26)
   write(*,*) 'PS_out(7,2)     =', PS_out(7,2)
   write(*,*) 'PHIS_out(209,80) =', PHIS_out(209,80), 'PHIS_out(210,80) =', PHIS_out(210,80)
! ================================= 2017.7.14 =====================================
!-----------------------------------------------------------------
   call read_IAPL35coordinate(lev_tmp, ilev_tmp, hyam_tmp, hyai_tmp&
           , hybm_tmp, hybi_tmp, sigm_tmp, sigi_tmp )
   !stop
   do k = 1, NLEV
      !lev_out(k) = lev_in(k)
      lev_out(k) = lev_tmp(k)!czy
   end do
   do k = 1, NLVP
      !ilev_out(k) = ilev_in(k)
      ilev_out(k) = ilev_tmp(k)!czy
   end do
!-----------------------------------------------------------------
! Interpolation variables from sigma-p coordinate to sigma coordinate
   call Inp_sp_sig
   print*, '**** SUCCESS vertical interpolate'
!-----------------------------------------------------------------
!!   print*, 'after Inp_gs_ll'
!!   print*, 'T_in (1,26,126)=', T_in(1,26,126)
!!   print*, 'LCWAT_in(1,19,1)=', LCWAT_in(1,19,1)
!!   print*, 'LCWAT_out(1,19,1)=', LCWAT_out(1,19,1)
! ===================================== zhh ======================================
   write(*,*) 'U_out(102,81,1) =', U_out(102,81,1), ' V_out(102,81,1) =', V_out(102,81,1)
   write(*,*) 'U_out(102,81,2) =', U_out(102,81,2), ' V_out(102,81,2) =', V_out(102,81,2)
   write(*,*) 'U_out(102,81,3) =', U_out(102,81,3), ' V_out(102,81,3) =', V_out(102,81,3)
   write(*,*) 'U_out(102,81,4) =', U_out(102,81,4), ' V_out(102,81,4) =', V_out(102,81,4)
   write(*,*) 'U_out(102,81,5) =', U_out(102,81,5), ' V_out(102,81,5) =', V_out(102,81,5)
   write(*,*) 'U_out(102,81,26) =', U_out(102,81,26), ' V_out(102,81,26) =', V_out(102,81,26)
   write(*,*) 'T_out(2,88,1) =', T_out(2,88,1), ' T_out(2,88,26) =', T_out(2,88,26)
   write(*,*) 'PS_out(7,2)     =', PS_out(7,2)
   write(*,*) 'PHIS_out(209,80) =', PHIS_out(209,80), 'PHIS_out(210,80) =', PHIS_out(210,80)
! ================================= 2007.11.4 =====================================
! ================================================================================
! czy
   write(*,*) 'Q_out(102,81,1) =', Q_out(102,81,1), ' Q_out(2,88,1) =', Q_out(2,88,1)
   write(*,*) 'Q_out(102,81,2) =', Q_out(102,81,2), ' Q_out(2,88,2) =', Q_out(2,88,2)
   write(*,*) 'Q_out(102,81,3) =', Q_out(102,81,3), ' Q_out(2,88,3) =', Q_out(2,88,3)
   write(*,*) 'Q_out(102,81,4) =', Q_out(102,81,4), ' Q_out(2,88,4) =', Q_out(2,88,4)
   write(*,*) 'Q_out(102,81,5) =', Q_out(102,81,5), ' Q_out(2,88,5) =', Q_out(2,88,5)
   write(*,*) 'Q_out(102,81,26) =', Q_out(102,81,26), ' Q_out(2,88,26) =', Q_out(2,88,26)
! ================================================================================
!    
!
! do some variables that need not to interpolate
!
! for coordinate
   do j = 1, NLAT
      lat_out(j) = -90.0 + DBLE(j-1) * 180.0 / dble(NLAT-1)  !zhh 2008.3.10
   end do
   do i = 1, NLON
      lon_out(i) = 0.0 + DBLE(i-1) * 360.0 / dble(NLON)  !zhh 2010.07.19
   end do
!====================== zhh 08.03.10 ==========================
   print*, 'lat_out(27)=', lat_out(27)
   print*, 'lat_out(128)=', lat_out(128)
   print*, 'lon_out(1)=', lon_out(1)
   print*, 'lon_out(256)=', lon_out(256)
!====================== zhh 08.03.10 ==========================
!
   PI = asin(dble(1))*2.0
   DLAT = PI / dble(NLAT-1)
   do j = 2, NLAT-1
      wlat(j) = PI / dble(NLAT-1) * sin( dble(j-1)*PI/dble(NLAT-1) )
   end do
   wlat(1)  = 0.25 * sin( 0.5*DLAT ) * DLAT
   wlat(NLAT) = 0.25 * sin( 0.5*DLAT ) * DLAT
!
   wt = 0.0
   do j = 1, NLAT
      wt = wt + wlat(j)
   end do
   print*, 'wt =', wt
!
   DO j = 1, NLAT
      wlat(j) = wlat(j) * 2.0 / wt 
      gw_out(j) = wlat(j)  
   END DO
!
   do k = 1, NLEV
      !lev_out(k) = lev_in(k)
!      lev_out(k) = lev_tmp(k)!czy
   end do
   do k = 1, NLVP
      !ilev_out(k) = ilev_in(k)
!      ilev_out(k) = ilev_tmp(k)!czy
   end do
!
   do k = 1, NLVP
      hyai_out(k) = ZERO
   end do
   do k = 1, NLEV
      hyam_out(k) = ZERO
   end do
   do k = 1, NLVP
      hybi_out(k) = SIG(k)
   end do
   do k = 1, NLEV
      hybm_out(k) = SIGL(k)
   end do
!
! for variables related to time
   time_out   = time_in
   ndbase_out = ndbase_in
   nsbase_out = nsbase_in
   nbdate_out = nbdate_in
   nbsec_out  = nbsec_in
   ndcur_out  = ndcur_in
   date_out   = date_in
   datesec_out = datesec_in
   nsteph_out = nsteph_in
!
   ntrk_out = 85
   ntrm_out = 85
   ntrn_out = 85
!
! for others
   P0_out  = P0_in
   mdt_out = mdt_in
   do i = 1, NCHR
      date_written_out(i) = date_written_in(i)
      time_written_out(i) = time_written_in(i)
   end do
! ================================= zhh ==================================
   do K = 1, NLEV
      do I = 1, NLON
         do J = 1, NLAT  
!!            if (CLDICE_out(i,k,j) < 0) CLDICE_out(i,k,j) = ZERO
!!            if (CLDLIQ_out(i,k,j) < 0) CLDLIQ_out(i,k,j) = ZERO
!!            if (CLOUD_out(i,k,j) < 0)  CLOUD_out(i,k,j)  = ZERO
!!            if (LCWAT_out(i,k,j) < 0)  LCWAT_out(i,k,j)  = ZERO
            if (CWAT_out(i,j,k) < 0)  CWAT_out(i,j,k)  = ZERO
            if (Q_out(i,j,k) < 0)      Q_out(i,j,k)      = ZERO
!!            if (QCWAT_out(i,k,j) < 0)  QCWAT_out(i,k,j)  = ZERO
         end do      
      end do      
   end do      
   do J = 1, NLAT 
      do I = 1, NLON 
!!         if (ICEFRAC_in(i,j) < 0)  ICEFRAC_out(i,j)  = ZERO
!!         if (LANDFRAC_in(i,j) < 0) LANDFRAC_out(i,j) = ZERO
!!         if (QPERT_in(i,j) < 0)    QPERT_out(i,j)    = ZERO
!!         if (SICTHK_in(i,j) < 0)   SICTHK_out(i,j)   = ZERO
         if (SNOWHICE_in(i,j) < 0) SNOWHICE_out(i,j) = ZERO
      end do      
   end do      
! ============================= 2007.11.16 ===============================
!-----------------------------------------------------------------
! write atmosphere initial data (lat-lon grid)
   call write_ini(FILE_NAME2)
!-----------------------------------------------------------------
! check
!!   print*, 'T_out(1,26,2)=', T_out(1,26,2)
!!   print*, 'PS_in (22,26)=', PS_in(22,26)

end program Interp_inidata
