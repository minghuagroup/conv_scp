module prognostic
!------------------------------------------------------------------------------------------------
! Purpose:    Set meteorological variables  
! Author :    ZhangHe
! Completed : 2007.10.8
!------------------------------------------------------------------------------------------------
   use Grid

   implicit none
   save
   public

   real*8 :: PMTOP                  ! pressure at the top level of the model (unit: hPa)
   real*8 :: SIG(NLVP)              ! sigma value on model layer
   real*8 :: SIGL(NLEV)             ! sigma value on interface layer
   !real*8 :: SIGC(NLON,NLAT,NLVP)   ! sigma value on model layer
   !real*8 :: SIGLC(NLON,NLAT,NLVP)  ! sigma value on interface layer
   !real*8 :: PINC(NLON,NLAT,NLVP)   ! pressure at interface sigma layer
   !real*8 :: PLYC(NLON,NLAT,NLVP)   ! pressure at model sigma layer
   !czy20170717
   real*8 :: SIGC(NLON,NLAT,GLVP)   ! sigma value on model layer
   real*8 :: SIGLC(NLON,NLAT,GLVP)  ! sigma value on interface layer
   real*8 :: PINC(NLON,NLAT,GLVP)   ! pressure at interface sigma layer
   real*8 :: PLYC(NLON,NLAT,GLVP)   ! pressure at model sigma layer
 
 end module
