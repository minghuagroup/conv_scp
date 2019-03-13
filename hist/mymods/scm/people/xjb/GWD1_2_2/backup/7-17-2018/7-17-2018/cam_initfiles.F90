module cam_initfiles
!----------------------------------------------------------------------- 
! 
! Open, close, and provide access to the initial conditions and topography files.
! 
!-----------------------------------------------------------------------

use pio,          only: file_desc_t

implicit none
private
save

! Public methods

public :: &
   cam_initfiles_open,   &! open initial and topo files
   initial_file_get_id,  &! returns filehandle for initial file
   topo_file_get_id,     &! returns filehandle for topo file
   cam_initfiles_close ,   & ! close initial and topo files
   topoGWD_file_get_id
!==========Jinbo Xie===================

	type(file_desc_t), pointer :: fh_ini, fh_topo
!============
! Jinbo Xie0
!============
type(file_desc_t), pointer :: fh_topoGWD
!============
! Jinbo Xie0
!============

!======================================================================= 
contains
!======================================================================= 

function initial_file_get_id()
  type(file_desc_t), pointer :: initial_file_get_id
  initial_file_get_id => fh_ini
end function initial_file_get_id

function topo_file_get_id()
  type(file_desc_t), pointer :: topo_file_get_id
  topo_file_get_id => fh_topo
end function topo_file_get_id

!====================================
!       Jinbo Xie
function topoGWD_file_get_id()
  type(file_desc_t), pointer :: topoGWD_file_get_id
  topoGWD_file_get_id => fh_topoGWD
end function topoGWD_file_get_id
!====================================

!======================================================================= 

subroutine cam_initfiles_open()

   ! Open the initial conditions and topography files.

   use filenames,        only: ncdata, bnd_topo , bnd_topo2 !Jinbo Xie
   use ioFileMod,        only: getfil

   use cam_pio_utils,    only: cam_pio_openfile
   use pio,              only: pio_nowrite

   use readinitial,      only: read_initial

   character(len=256) :: ncdata_loc     ! filepath of initial file on local disk
   character(len=256) :: bnd_topo_loc   ! filepath of topo file on local disk

!===============Jinbo Xie================
character(len=256) :: bnd_topo_loc2   ! filepath of topo file on local disk
!========================================
   !----------------------------------------------------------------------- 
   
   ! Open initial, topography, and landfrac datasets
   call getfil (ncdata, ncdata_loc)

   allocate(fh_ini)
   call cam_pio_openfile(fh_ini, ncdata_loc, PIO_NOWRITE, .TRUE.)
   ! Backward compatibility: look for topography data on initial file if topo file name not provided.
   if (trim(bnd_topo) /= 'bnd_topo' .and. len_trim(bnd_topo) > 0) then
      allocate(fh_topo)
      call getfil(bnd_topo, bnd_topo_loc)
write(*,*), "Jinbo Xie1", bnd_topo,"Jinbo Xie2", bnd_topo_loc
      call cam_pio_openfile(fh_topo, bnd_topo_loc, PIO_NOWRITE)
!=================
! Jinbo Xie1
!=================
        !read in 11 topo data from my processed dataa
                allocate(fh_topoGWD)
		call getfil(bnd_topo2, bnd_topo_loc2)
write(*,*), "Jinbo Xie3", bnd_topo2,"Jinbo Xie4", bnd_topo_loc2
                call cam_pio_openfile(fh_topoGWD, bnd_topo_loc2, PIO_NOWRITE)
		!call cam_pio_openfile(fh_topoGWD,"/public/home/xiejinbo/Analysis/OD/GW_proc/data/0.9x1.25degGWD.nc", PIO_NOWRITE)
                !call cam_pio_openfile(fh_topoGWD,"/public/home/xiejinbo/Analysis/OD/GW_proc/data/test/8min_topo_my.nc",PIO_NOWRITE)

!=================
! Jinbo Xie1
!=================
   else
      fh_topo => fh_ini
   end if

   ! Check for consistent settings on initial dataset -- this is dycore
   ! dependent -- should move to dycore interface
   call read_initial (fh_ini)

end subroutine cam_initfiles_open

!======================================================================= 

subroutine cam_initfiles_close()

  use pio,          only: pio_closefile

  if(associated(fh_ini)) then
     if(.not. associated(fh_ini, target=fh_topo)) then
        call pio_closefile(fh_topo)
        deallocate(fh_topo)
     end if
     
     call pio_closefile(fh_ini)
     deallocate(fh_ini)
     nullify(fh_ini)
     nullify(fh_topo)
  end if
end subroutine cam_initfiles_close

!======================================================================= 

end module cam_initfiles
