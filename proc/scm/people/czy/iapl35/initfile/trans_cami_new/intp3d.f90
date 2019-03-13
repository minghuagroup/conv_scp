SUBROUTINE INTP3D(ITYPE,FIN,PIN,FOT,POT,KOT,JB0,JE0)
!------------------------------------------------------------------------------------------------
! Purpose: Vertical interpolation of 3D variables from sigma coordinate to p coordinate
! Original version : INTP3D.f (IAP 9L)
! Reconstructed   : ZhangHe
! Completed : 2005.9.7
! Update : 2007.10.7, ZhangHe, from (NX,NY,NL) to (NLON,NLAT,NLEV)
!          2012.01.10, ZhangHe, FOT(NLON,KOT,NLAT) ==> FOT(NLON,NLAT,KOT)
!------------------------------------------------------------------------------------------------
   use Grid 

   implicit none
!------------------------------------Arguments--------------------------------------------------
!!   real(r8), intent(in)  :: FIN(NX,NY,NL)    ! input variable needed to interpolate
!!   real(r8), intent(in)  :: PIN(NX,NY,NZ)    !
!czy   real*8, intent(in)   :: FIN(NLON,NLAT,NLEV)    ! input variable needed to interpolate
!czy   real*8, intent(in)   :: PIN(NLON,NLAT,NLVP)    !
   real*8, intent(in)   :: FIN(NLON,NLAT,GLEV)    ! input variable needed to interpolate
   real*8, intent(in)   :: PIN(NLON,NLAT,GLVP)    !
   real*8, intent(in)   :: POT(*)           ! vertical layers need to interpolate to          
   integer,  intent(in) :: KOT              ! vertical layers of output variable FOT
   integer,  intent(in) :: JB0              ! start index of latitude
   integer,  intent(in) :: JE0              ! end   index of latitude
   integer,  intent(in) :: ITYPE            ! elective index
!!   real*8, intent(out)  :: FOT(IM,NY,KOT)   ! output variable been interpolated to p level
   real*8, intent(out)  :: FOT(NLON,NLAT,KOT)   ! output variable been interpolated to p level
!----------------------------------Local workspace----------------------------------------------
   integer, parameter :: KOO = NLVP
!czy   real*8 :: FINT(NLEV)
   real*8 :: FINT(GLEV)
!czy   real*8 :: PINT(NLVP)
   real*8 :: PINT(GLVP)
   real*8 :: FOO(KOO)
   integer :: I, J, K 
!------------------------------------------------------------------------------------------------

   GOTO(10, 20), ITYPE
!  ITYPE = 1 : Vertical interpolation weighted by pressure to conserve mass vertical integration 
!  ITYPE = 2 : Linear vertical interpolation by pressure
!    
10 CONTINUE
   DO I = 1, NLON
!!      IEX  = I + EX   ! EX = 3, see module Dyn_grid
      DO J = JB0, JE0
!czy         DO K = 1, NLEV                                     
         DO K = 1, GLEV                                     
            FINT(K)    = FIN(I,J,K)
         END DO
!czy         DO K = 1, NLVP
         DO K = 1, GLVP
            PINT(K)    = PIN(I,J,K)
         END DO
!------------------------ interpolate FINT to FOO -----------------------------------------
!czy         CALL INTPAV( FINT,PINT,NLEV,FOO,POT,KOT )
         CALL INTPAV( FINT,PINT,GLEV,FOO,POT,KOT )
!------------------------------------------------------------------------------------------
         DO K = 1 ,KOT
            FOT(I,J,K) = FOO(K)
         END DO
      END DO
   END DO
   RETURN
!
!   ITYPE = 2 
20 CONTINUE
   DO I = 1, NLON
!!      IEX  = I + EX
      DO J = JB0, JE0
 !czy        DO K = 1, NLEV                                     
         DO K = 1, GLEV                                     
            FINT(K)    = FIN(I,J,K)
         END DO
!czy         DO K = 1, NLVP
         DO K = 1, GLVP
            PINT(K)    = PIN(I,J,K)
         END DO
!------------------------ interpolate FINT to FOO -----------------------------------------
!czy         CALL INTLAY( FINT,PINT,NLEV,FOO,POT,KOT )
         CALL INTLAY( FINT,PINT,GLEV,FOO,POT,KOT )
!------------------------------------------------------------------------------------------
         DO K = 1 ,KOT
            FOT(I,J,K) = FOO(K)
         END DO
      END DO
   END DO
   RETURN
END 

