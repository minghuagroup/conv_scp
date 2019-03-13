subroutine Inp_gs_ll
!----------------------------------------------------------------------------------
! Purpose: Interpolation variables from Gaussian grid to lat-lon grid
! Author : ZhangHe
! Complete: 2007.6.5
! Update: ZhangHe, 2007.6.30, change the identifier of variables ICEFRAC & LANDFRAC 
!                             from 0(scalar) to 2(flag)
!         ZhangHe, 2011.12.24
!----------------------------------------------------------------------------------
!!   use precision, only: r8 
   use Grid, only: GLAT, GLON, NLAT, NLON, NLEV
   use Var_name_inic
   
   implicit none
!--------------------------------- Local workspace --------------------------------
!
!!   real*8 :: CLDICE2(NLON, NLAT)
!!   real*8 :: CLDLIQ2(NLON, NLAT)
!!   real*8 :: CLOUD2 (NLON, NLAT)
!!   real*8 :: LCWAT2 (NLON, NLAT)
   real*8 :: CWAT2 (NLON, NLAT)
   real*8 :: Q2     (NLON, NLAT)
!!   real*8 :: QCWAT2 (NLON, NLAT) 
   real*8 :: T2     (NLON, NLAT)
!!   real*8 :: TCWAT2 (NLON, NLAT)
   real*8 :: U2     (NLON, NLAT)
   real*8 :: V2     (NLON, NLAT)
!
   integer  :: i, j, k, sthi, stho 
!----------------------------------------------------------------------------------
   sthi = 0
   stho = 0
! ===================================== zhh ======================================
   write(*,*) 'At sub. Inp_gs_ll: before calling gs_ll'
   write(*,*) 'U_in(102,81,1) =', U_in(102,81,1), ' V_in(102,81,1) =', V_in(102,81,1)
   write(*,*) 'U_in(102,81,26) =', U_in(102,81,26), ' V_in(102,81,26) =', V_in(102,81,26)
   write(*,*) 'T_in(2,88,1) =', T_in(2,88,1), ' T_in(2,88,26) =', T_in(2,88,26)
! ================================= 2007.11.4 =====================================
!
! Interpolate variables from Gaussian grid to lat-lon grid
!
! for 2D var
!!   call gs_ll(0, sthi, stho, PHIS_in, PHIS_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, PS_in, PS_out, GLON, GLAT, NLON, NLAT)
!   call gs_ll(0, sthi, stho, SGH_in, SGH_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, SNOWHICE_in, SNOWHICE_out, GLON, GLAT, NLON, NLAT)
!!   call gs_ll(0, sthi, stho, TS_in, TS_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, TS1_in, TS1_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, TS2_in, TS2_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, TS3_in, TS3_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, TS4_in, TS4_out, GLON, GLAT, NLON, NLAT)
   call gs_ll(0, sthi, stho, TSICE_in, TSICE_out, GLON, GLAT, NLON, NLAT)
!
! ===================================== zhh ======================================
   write(*,*) 'At sub. Inp_gs_ll: after calling gs_ll'
   write(*,*) 'PS_out(7,2) =', PS_out(7,2)
! ================================= 2007.11.4 =====================================
!
!  for 3D var
   !do k = 1, NLEV
   do k = 1, GLEV!czy
!!      call gs_ll(0, sthi, stho, CWAT_in(1,1,k), CWAT2, GLON, GLAT, NLON, NLAT)
      call gs_ll(0, sthi, stho, Q_in(1,1,k), Q2, GLON, GLAT, NLON, NLAT)
      call gs_ll(0, sthi, stho, T_in(1,1,k), T2, GLON, GLAT, NLON, NLAT)
      call gs_ll(0, sthi, stho, U_in(1,1,k), U2, GLON, GLAT, NLON, NLAT)
      call gs_ll(0, sthi, stho, V_in(1,1,k), V2, GLON, GLAT, NLON, NLAT)
!
! from var3 ==> var_out
      do j = 1, NLAT
         do i = 1, NLON
!!            CLDICE_out(i,j,k) = CLDICE2(i,j)
!!            CLDLIQ_out(i,j,k) = CLDLIQ2(i,j)
!!            CLOUD_out(i,j,k)  = CLOUD2(i,j)
!!            LCWAT_out(i,j,k)  = LCWAT2(i,j)
!!!            CWAT_out(i,j,k)  = CWAT2(i,j)
           ! Q_out(i,j,k)      = Q2(i,j)
            Q_out_gs(i,j,k)      = Q2(i,j)!czy
!!            QCWAT_out(i,j,k)  = QCWAT2(i,j)
           ! T_out(i,j,k)      = T2(i,j)
            T_out_gs(i,j,k)      = T2(i,j)!czy
!!            TCWAT_out(i,j,k)  = TCWAT2(i,j)
            !U_out(i,j,k)      = U2(i,j)
            U_out_gs(i,j,k)      = U2(i,j)!czy
            !V_out(i,j,k)      = V2(i,j)
            V_out_gs(i,j,k)      = V2(i,j)!czy
         end do
      end do		    
   end do
!
!  set U & V to zero at north and south pole
   !do k = 1, NLEV
   do k = 1, GLEV
      do i = 1, NLON
         !U_out(i,1,k)    = 0.0
         !V_out(i,1,k)    = 0.0
         !U_out(i,NLAT,k) = 0.0
         !V_out(i,NLAT,k) = 0.0
         U_out_gs(i,1,k)    = 0.0!czy
         V_out_gs(i,1,k)    = 0.0!czy
         U_out_gs(i,NLAT,k) = 0.0!czy
         V_out_gs(i,NLAT,k) = 0.0!czy
      end do
   end do

end subroutine 
