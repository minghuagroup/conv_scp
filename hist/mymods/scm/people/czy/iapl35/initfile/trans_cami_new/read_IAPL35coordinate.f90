subroutine read_IAPL35coordinate(lev_tmp, ilev_tmp, hyam_tmp, hyai_tmp&
                , hybm_tmp, hybi_tmp, sigm_tmp, sigi_tmp &
                ,p0_tmp, ptop_tmp)
! Purpose:
! Methord:
! Author:

use netcdf
implicit none
!--------------------------------------------------------------------------------
integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real

integer, parameter :: nlev = 35
integer, parameter :: nilev = nlev + 1
!--------------------------------------------------------------------------------

real(r8),dimension(nlev), intent(out) :: lev_tmp, hyam_tmp, hybm_tmp, sigm_tmp 
real(r8),dimension(nilev),intent(out) :: ilev_tmp, hyai_tmp, hybi_tmp, sigi_tmp 
real(r8),optional, intent(out)        :: p0_tmp, ptop_tmp

real(r8),dimension(nlev)  :: lev, hyam, hybm, sigm 
real(r8),dimension(nilev) :: ilev, hyai, hybi, sigi 
real(r8)                  :: p0, ptop

integer :: levDimID, ilevDimID
integer :: levID, hyamID, hybmID, sigmID
integer :: ilevID, hyaiID, hybiID, sigiID
integer :: p0ID, ptopID
!--------------------------------------------------------------------------------

integer :: argc
integer :: ncid
character(len=128)      :: filename
!--------------------------------------------------------------------------------
integer :: k
!--------------------------------------------------------------------------------
! get the file name
argc = iargc()
if (argc == 1) then
        call getarg(1,filename)
else
        filename = "IAPL35coordinate.nc"
endif
write(*,1000) filename
1000 format(' ','The input file name is: ', A)

! open file and check errors
call check(nf90_open(path = filename, mode = nf90_nowrite, ncid = ncid))

call check(nf90_inq_dimid(ncid = ncid, name = "lev", dimid = levDimID))
call check(nf90_inq_dimid(ncid = ncid, name = "ilev", dimid = ilevDimID))

call check(nf90_inq_varid(ncid = ncid, name = "lev", varid=levID))
call check(nf90_inq_varid(ncid = ncid, name = "ilev", varid=ilevID))
call check(nf90_inq_varid(ncid = ncid, name = "hyam", varid=hyamID))
call check(nf90_inq_varid(ncid = ncid, name = "hybm", varid=hybmID))
call check(nf90_inq_varid(ncid = ncid, name = "hyai", varid=hyaiID))
call check(nf90_inq_varid(ncid = ncid, name = "hybi", varid=hybiID))
call check(nf90_inq_varid(ncid = ncid, name = "sigm", varid=sigmID))
call check(nf90_inq_varid(ncid = ncid, name = "sigi", varid=sigiID))
call check(nf90_inq_varid(ncid = ncid, name = "P0", varid=p0ID))
call check(nf90_inq_varid(ncid = ncid, name = "Ptop", varid=ptopID))

call check(nf90_get_var(ncid, levID, lev, start = (/ 1 /) ))
call check(nf90_get_var(ncid, ilevID, ilev, start = (/ 1 /) ))
call check(nf90_get_var(ncid, hyamID, hyam, start = (/ 1 /) ))
call check(nf90_get_var(ncid, hybmID, hybm, start = (/ 1 /) ))
call check(nf90_get_var(ncid, hyaiID, hyai, start = (/ 1 /) ))
call check(nf90_get_var(ncid, hybiID, hybi, start = (/ 1 /) ))
call check(nf90_get_var(ncid, sigmID, sigm, start = (/ 1 /) ))
call check(nf90_get_var(ncid, sigiID, sigi, start = (/ 1 /) ))
call check(nf90_get_var(ncid, P0ID, p0))
call check(nf90_get_var(ncid, PtopID, ptop))
call check(nf90_close(ncid))

write(*,'(2X,A)')"  k      ilev(k)      hyai(k)      hybi(k)     sigi(k)       lev(k)      hyam(k)      hybm(k)      sigm(k)"
write(*,'(2X,A)')"===== ============ ============ ============ ============ ============ ============ ============ ============"
do k = 1, nlev
        write(*,100)k, ilev(k), hyai(k), hybi(k), sigi(k), lev(k), hyam(k), hybm(k), sigm(k)
enddo
100 format(1X,I4,8(1X,F12.7))
k = nilev
write(*,100)k, ilev(k), hyai(k), hybi(k), sigi(k)

!--------------------------------------------------------------------------------
lev_tmp   = lev
ilev_tmp  = ilev
hyai_tmp  = hyai
hyam_tmp  = hyam
hybi_tmp  = hybi
hybm_tmp  = hybm
sigm_tmp  = sigm 
sigi_tmp  = sigi
if(present(p0_tmp))p0_tmp = p0
if(present(ptop_tmp))ptop_tmp = ptop
!--------------------------------------------------------------------------------

contains
subroutine check(status)
!----------------------------------------------------------------------------------
! Purpose: check th errors about netcdf
! Author : ZhangHe
! Complete: 2007.5.27
!----------------------------------------------------------------------------------
  use netcdf
  implicit none

!------------------------------------Arguments--------------------------------------------------
  integer, intent ( in) :: status
!----------------------------------------------------------------------------------

  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
end subroutine check
end subroutine read_IAPL35coordinate
