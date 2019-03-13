module mzfunctions_mod
!------------------------------------------------------------------------------------------------
! Purpose:    Calculate ramp functions
! Author :    Minghua Zhang
! Completed : 2018-08-15
! Update    
!------------------------------------------------------------------------------------------------

   use shr_kind_mod, only: r8 => shr_kind_r8
   use units,           only: getunit, freeunit
   use time_manager,    only: get_nstep
   

   implicit none
   save
   public

CONTAINS
!================================================================================================

  subroutine mzfunc1(nx,x,y1, xmin, xmax,x0,xscale, iflag)

! =====================================================
!  x0 is the infection point
!  xscale is the contraction xscale in x
!  returned value from -1. to 1.
! -------------------------------------
! input
  integer :: nx
  integer :: iflag  ! -1 to return (-1. 1.), else return (0.0, 1.)
  real(8) :: x(nx)
  real(8) :: xmin, xmax, x0, xscale

! output
  real(8) :: y1(nx)

! local
  real(8) :: x1,x1min, x1max
  integer :: i

   if(xmin .eq. xmax) then 
    write(*,*)'xmin cannot be the same as xmax, stop in zmh_function'
    stop
   end if

   x1min = (xmin - x0)*xscale
   x1max = (xmax - x0)*xscale

   do i=1,nx
    x1     = (x(i)    - x0)*xscale 
    y1(i)  = (atan( -x1) - atan(-x1min) )/(atan(-x1max) - atan(-x1min)) 

    if(iflag .eq.  -1)then
       y1(i)  = 2.0*y1(i) - 1. 
    end if

   end do

  return

 end subroutine

!================================================================================================

  subroutine fout2d(d2,nnx,nny,nx0,ny0,var)

! work with IDL program iap_proc1.pro to view fields

  real(8)      :: d2(nnx,nny)
  integer      :: nnx,nny,nx0,ny0
  integer      :: nstep
  character(*) :: var

  character(len=160) :: filename
  character(len=4) :: strt,strx,stry
  logical      :: file_exists
  integer      :: unitn,J,K

  nstep = get_nstep()

  write(strt,'(I4.4)')nstep
  write(strx,'(I4.4)')nx0
  write(stry,'(I4.4)')ny0

  filename = 'fout/'//var//'_'//strt//'_'//strx//'_'//stry//'.txt'
  INQUIRE(FILE=trim(filename), EXIST=file_exists)

  if(file_exists)then 
      return   !!
      filename = trim(filename)//'1'
  endif

  write(*,*)filename
  unitn = getunit()
  open( unitn, file=trim(filename),status='unknown')
   write(unitn,'(8I8)') nstep,nnx,nny,nx0,ny0
   do J = 1, nny
     write(unitn,'(1000(5E15.7/))') d2(1:nnx,j)
   enddo
  close(unitn)
  call freeunit(unitn)
               

  return

 end subroutine


!================================================================================================

  subroutine fout3d(d3,nnx,nnz,nny,nx0,nz0,ny0,var)

! work with IDL program iap_proc1.pro to view fields

  real(8)      :: d3(nnx,nnz,nny)
  integer      :: nnx,nnz,nny,nx0,nz0,ny0
  integer      :: nstep
  character(*) :: var

  character(len=160) :: filename
  character(len=4) :: strt,strx,stry,strz
  logical      :: file_exists
  integer      :: unitn,J,K

  nstep = get_nstep()

  write(strt,'(I4.4)')nstep
  write(strx,'(I4.4)')nx0
  write(strz,'(I4.4)')nz0
  write(stry,'(I4.4)')ny0

  filename = 'fout/'//var//'_'//strt//'_'//strx//'_'//strz//'_'//stry//'.txt'
  INQUIRE(FILE=trim(filename), EXIST=file_exists)

  if(file_exists)then 
      return !!
      filename = trim(filename)//'1'
  endif

  write(*,*)filename
  unitn = getunit()
  open( unitn, file=trim(filename),status='unknown')
   write(unitn,'(8I8)') nstep,nnx,nnz,nny,nx0,nz0,ny0
   do J = 1, nny
    do k = 1, nnz
     write(unitn,'(1000(5E15.7/))') d3(1:nnx,k,j)
    enddo
   enddo
  close(unitn)
  call freeunit(unitn)


  return

 end subroutine


end module
