subroutine gs_ll(MP, sthi, stho, WKIN, WKOUT, nx_in, ny_in, nx_out, ny_out)
!----------------------------------------------------------------------------------
! Purpose: Driver for interpolation variables from Gaussian grid to lat-lon grid
! Author : ZhangHe
! Complete: 2007.6.5
! Update: 2011.1.27, Zhang He, add nx_in, ny_in, nx_out, ny_out
!         2011.12.01, Zhang He, real*4 WKOUT --> real*8 WKOUT
!----------------------------------------------------------------------------------
   
   implicit none
!------------------------------------Arguments---------------------------------------------------
   integer , intent(in)  :: MP   ! field parameter identifier 
!                                  0:SCALAR; 1:VECTOR;  2:FLAG;  3:BUDGET
   integer , intent(in)  :: sthi   ! input fields run direction: 0: northward; 1: southward 
   integer , intent(in)  :: stho   ! output fields run direction: 0: northward; 1: southward 
   integer , intent(in)  :: nx_in    ! number of longitudes - Gaussian grid  
   integer , intent(in)  :: ny_in    ! number of latitudes - Gaussian grid  
   integer , intent(in)  :: nx_out   ! number of longitudes - lat-lon grid  
   integer , intent(in)  :: ny_out   ! number of latitudes - lat-lon grid  
!   real*4, intent(in)  :: WK2D(GLATON)       ! field to interpolate
   real*8, intent(inout)  :: WKIN(nx_in,ny_in)      ! input field to interpolate
   real*8, intent(out) :: WKOUT(nx_out,ny_out)   ! output interpolated field
!--------------------------------- Local workspace --------------------------------
   integer :: MB, NNX, NNY, IXN, JXN, IMO, JMO, IXO, JXO 
   real*4 :: CIO, CJO, DIO, DJO
   real*4 :: WK2D(nx_in*ny_in)
   real*4 :: GRDO(nx_out*ny_out)
   real*4 :: tmp_in(ny_in), tmp_out(ny_out)
   real*4 :: GGLAT(ny_in/2)
   logical :: BMAPI(nx_in*ny_in), BMAPO(nx_in*ny_in)
   real*4 :: PK(ny_in/2),PKM1(ny_in/2)
   real*4 :: SJN(ny_in/2),WJN(ny_in/2),CJN1(ny_in/2)
   real*4 :: AJN(ny_in+1),AJO(ny_out+1),XIO(nx_out+1),XJO(ny_out+1)
   real*4 :: WIO1(nx_out),WIO2(nx_out)
   integer :: JPO(ny_out)
   real*4 :: FPX(nx_out),FP0X(nx_out),WPX(nx_out),WTX(nx_out)
   integer  :: i, j, js, ij
!!
!!   real*4 :: GRDO(NLON*NLAT), GRDO2(NLON,NLAT) 
!!   EQUIVALENCE (GRDO(1),GRDO2(1,1))
!----------------------------------------------------------------------------------
   MB = 0           ! 0:no bitmap
   GGLAT(1) = 0.0   ! the Gaussian latitudes are calculated 
!
   NNX = nx_in       ! input longitude dimension
   NNY = ny_in       ! input latitude  dimension (even)
!
   IXN = 1          ! number to skip between input longitudes
   JXN = nx_in       ! number to skip between input latitudes
!
   IMO = nx_out       ! output longitude dimension
   JMO = ny_out       ! output latitude  dimension 
!
   IXO = 1          ! number to skip between output longitudes
   JXO = nx_out       ! number to skip between output latitudes
!
   CIO = 0.0        ! start longitude in degrees east
   CJO = 90.0       ! start latitude  in degrees north
!
   DIO = 360.0 / real(nx_out)     ! longitude increment in degrees east
   DJO = -180.0 / real(ny_out-1)  ! latitude  increment in degrees north
!
! change the run direction of the field from northward to southward if neccessary
   if (sthi == 0) then
      do i = 1, nx_in
         do j = 1, ny_in
            js = ny_in + 1 - j
            tmp_in(js) = WKIN(i,j)
         end do
!
         do j = 1, ny_in
            WKIN(i,j) = tmp_in(j)
         end do
      end do
   end if
!
! copy data from WKIN to WK2D
   do j = 1, ny_in
      do i = 1, nx_in
         ij = (j-1)*nx_in + i
         WK2D(ij) = WKIN(i,j)
      end do
   end do
!
! ===========================================================================
   CALL GG2LL(MP,MB,NNX,IXN,NNY,JXN,GGLAT,BMAPI,WK2D(1),       &
              IMO,IXO,JMO,JXO,CIO,CJO,DIO,DJO,BMAPO,GRDO,      &
              SJN,WJN,CJN1,AJN,AJO,XIO,XJO,WIO1,WIO2,JPO,      &
              FPX,WPX,WTX,FP0X,PK,PKM1)
! ===========================================================================
!
! copy data from GRDO to WKOUT
   do j = 1, ny_out
      do i = 1, nx_out
         ij = (j-1)*nx_out + i
         WKOUT(i,j) = GRDO(ij)
      end do
   end do
!
! change the run direction of the field from southward to northward if neccessary
   if (stho == 0) then
      do i = 1, nx_out
         do j = 1, ny_out
            js = ny_out + 1 - j
            tmp_out(js) = WKOUT(i,j)
         end do
!
         do j = 1, ny_out
            WKOUT(i,j) = tmp_out(j)
         end do
      end do
   end if

end subroutine 