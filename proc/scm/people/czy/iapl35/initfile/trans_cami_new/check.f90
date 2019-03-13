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
