subroutine Inp_sp_sig
!----------------------------------------------------------------------------------
! Purpose: Interpolation variables from sigma-p coordinate to sigma coordinate
! Author : ZhangHe
! Complete: 2007.9.22
! Update: 2007.10.10, ZhangHe 
!         2011.12.01, ZhangHe
!         2011.12.24, ZhangHe
!----------------------------------------------------------------------------------
!   use precision
   use Grid
   use Var_name_inic, only: CWAT_out, T_out, Q_out, U_out, V_out
   use Var_name_inic, only: CWAT_out_gs, T_out_gs, Q_out_gs, U_out_gs, V_out_gs!czy
   use prognostic,  only: SIGC, SIGLC, SIG, SIGL

   implicit none

!----------------------------------Local workspace----------------------------------------------
!!   real*8 :: CLDICE_tmp(NLON,NLAT,NLEV)
!!   real*8 :: CLDLIQ_tmp(NLON,NLAT,NLEV)
!!   real*8 :: CLOUD_tmp(NLON,NLAT,NLEV)
!!   real*8 :: TCWAT_tmp(NLON,NLAT,NLEV)
   !czy real*8 :: CWAT_tmp(NLON,NLAT,NLEV)
   !czy real*8 :: T_tmp(NLON,NLAT,NLEV)

   !czy real*8 :: Q_tmp(NLON,NLAT,NLEV)
   !czy real*8 :: U_tmp(NLON,NLAT,NLEV)
   !czy real*8 :: V_tmp(NLON,NLAT,NLEV)
!!   real*8 :: LCWAT_tmp(NLON,NLAT,NLEV)
!!   real*8 :: QCWAT_tmp(NLON,NLAT,NLEV)
!   real(r8) :: FIN(NX,NY,NL), FOT(IM,NY,NL)
   integer  :: I, J, K, ITYPE, KOT
!------------------------------------------------------------------------------------------------
   KOT = NLEV
!
   do j = 1, NLAT
      do k = 1, NLEV     
         do i = 1, NLON 
!!            CLDICE_tmp(i,j,k) = CLDICE_out(i,j,k)
!!            CLDLIQ_tmp(i,j,k) = CLDLIQ_out(i,j,k)
!!            CLOUD_tmp(i,j,k) = CLOUD_out(i,j,k)
!!            TCWAT_tmp(i,j,k) = TCWAT_out(i,j,k)
!!            LCWAT_tmp(i,j,k) = LCWAT_out(i,j,k)
!!            QCWAT_tmp(i,j,k) = QCWAT_out(i,j,k)
!!!            CWAT_tmp(i,j,k) = CWAT_out(i,j,k)
!
!czy            T_tmp(i,j,k) = T_out(i,j,k)
!czy            Q_tmp(i,j,k) = Q_out(i,j,k)
!czy            U_tmp(i,j,k) = U_out(i,j,k)
!czy            V_tmp(i,j,k) = V_out(i,j,k)
         end do
      end do
   end do
!  
!------------------------------------------------------------------------------------
   call calsig
!------------------------------------------------------------------------------------
!
!!   print*, 'T_tmp(138,82,1)=', T_tmp(138,82,1), ' T_tmp(138,82,26)=', T_tmp(138,82,26)
!!   print*, 'SIGLC(138,82,1)=', SIGLC(138,82,1), ' SIGC(138,82,26)=', SIGC(138,82,26)
!!   print*, 'SIGLC(138,82,27)=', SIGLC(138,82,27), ' SIGC(138,82,1)=', SIGC(138,82,1)
!!   print*, 'SIG(27)=', SIG(27), ' SIGL(1)=', SIGL(1)
!
! [1] ****************************  interpolation T  ********************************
      ITYPE = 2                  ! T LINEAR IN sigma
 !czy     CALL INTP3D( ITYPE,T_tmp,SIGLC,T_out,SIGL,KOT,01,NLAT )    
      CALL INTP3D( ITYPE,T_out_gs,SIGLC,T_out,SIGL,KOT,01,NLAT )    
!------------------------------------------------------------------------------------
! [2] ****************************  interpolation CLDICE  ********************************
!!      ITYPE = 1                  
!!      CALL INTP3D( ITYPE,CLDICE_tmp,SIGLC,CLDICE_out,SIGL,KOT,01,NLAT )    
!------------------------------------------------------------------------------------
! [3] ****************************  interpolation CLDLIQ  ********************************
!!      ITYPE = 1                  
!!      CALL INTP3D( ITYPE,CLDLIQ_tmp,SIGLC,CLDLIQ_out,SIGL,KOT,01,NLAT )    
!------------------------------------------------------------------------------------
! [4] ****************************  interpolation CLOUD  ********************************
!!      ITYPE = 2                  
!!      CALL INTP3D( ITYPE,CLOUD_tmp,SIGLC,CLOUD_out,SIGL,KOT,01,NLAT )    
!------------------------------------------------------------------------------------
! [5] ****************************  interpolation TCWAT  ********************************
      ITYPE = 2                  
!!!      CALL INTP3D( ITYPE,CWAT_tmp,SIGLC,CWAT_out,SIGL,KOT,01,NLAT )    
!------------------------------------------------------------------------------------
! [6] ****************************  interpolation U  ********************************
!czy      ITYPE = 1                  
      ITYPE = 2                  
!czy      CALL INTP3D( ITYPE,U_tmp,SIGC,U_out,SIGL,KOT,JB,JE )  
      CALL INTP3D( ITYPE,U_out_gs,SIGC,U_out,SIGL,KOT,JB,JE )  
!------------------------------------------------------------------------------- 
! [7] ****************************  interpolation V  ********************************
!czy      ITYPE = 1                  
      ITYPE =  2                 
!czy      CALL INTP3D( ITYPE,V_tmp,SIGC,V_out,SIGL,KOT,JB,JE )  
      CALL INTP3D( ITYPE,V_out_gs,SIGC,V_out,SIGL,KOT,JB,JE )  
!------------------------------------------------------------------------------- 
! [8] ****************************  interpolation Q  ********************************
!czy      ITYPE = 1                  
      ITYPE = 2                  
!czy      CALL INTP3D( ITYPE,Q_tmp,SIGC,Q_out,SIGL,KOT,01,NLAT )  
      CALL INTP3D( ITYPE,Q_out_gs,SIGC,Q_out,SIGL,KOT,01,NLAT )  
!------------------------------------------------------------------------------- 
! [9] ****************************  interpolation LCWAT  ********************************
!!      ITYPE = 1                  
!!      CALL INTP3D( ITYPE,LCWAT_tmp,SIGC,LCWAT_out,SIGL,KOT,01,NLAT )  
!------------------------------------------------------------------------------- 
! [10] ***************************  interpolation QCWAT  ********************************
!!      ITYPE = 1                  
!!      CALL INTP3D( ITYPE,QCWAT_tmp,SIGC,QCWAT_out,SIGL,KOT,01,NLAT )  
!------------------------------------------------------------------------------- 
   print*, 'At the end of Inp_sp_sig'
   print*, 'CWAT_out(137,8,1)=', CWAT_out(137,8,1)
!!   print*, 'T_out(137,82,3)=', T_out(137,82,3), ' T_out(137,82,4)=', T_out(137,82,4)
!!   print*, 'U_out(137,82,2)=', U_out(137,82,2), ' U_out(137,2,2)=', U_out(137,2,2)
!!   print*, 'V_out(137,82,12)=', V_out(137,82,12), ' V_out(137,2,12)=', V_out(137,2,12)
    
end subroutine Inp_sp_sig  
