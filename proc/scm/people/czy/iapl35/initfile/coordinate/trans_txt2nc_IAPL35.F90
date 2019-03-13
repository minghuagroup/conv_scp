program trans_txt2nc
! From trans_txt2nc_IAPL91.F90
!--------------------------------------------------------------------------------
! Purpose: 
!        Transform the files 'IAP_L35_Pint.txt' & 'IAP_L35_Pmid.txt' to netcdf
!        file 'IAPL35coordinate.nc', which includes the following variables:
!        lev            hybrid level at midpoints (1000*(A+B))
!        ilev           hybrid level at interfaces (1000*(A+B))
!        hyai(ilev)     hybrid A coefficient at layer interfaces
!        hybi(ilev)     hybrid B coefficient at layer interfaces
!        hyam(lev)      hybrid A coefficient at layer midpoints
!        hybm(lev)      hybrid B coefficient at layer midpoints
!        sigi(ilev)     sigma coordinate at interfaces (pint-ptop)/(pint(ilev)-ptop)
!        pint(ilev)     reference interface pressure used to define sigma coordinate
!        sigm(lev)      sigma coordinate at midpoints (pmid-ptop)/(pmid(lev)-ptop)
!        pmid(lev)      reference midpoint pressure used to define sigma coordinate
!        P0             reference pressure used to calculate P=A*P0+B*PS
!        Ptop           model top pressure used to define sigma coordinate
!--------------------------------------------------------------------------------
! Methord:
!        Use pint & pmid to calculate other vars:
!        Def: P0 = 1000, Ptop = 0, nilev = 36, nlev = 35
!        sigi(1:nilev) = (pint(1:nilev)-ptop)/(pint(nilev)-ptop)
!        sigm(1:nlev)  = (pmid(1:nlev)-ptop)/(pint(nilev)-ptop)
!        hyai = 0, hyam = 0
!        hybi = sigi, hybm = sigm
!        ilev = 1000*(hyai + hybi)
!        lev  = 1000*(hyam + hybm)
!--------------------------------------------------------------------------------
! Build:
!       ifort -I./ -I//public/software/mathlib/netcdf/4.3.2/intel/include
!       -L/public/software/mathlib/netcdf/4.3.2/intel/lib -lnetcdff -o
!       trans_txt2nc trans_txt2nc.F90
! Run:  ./trans_txt2nc
!--------------------------------------------------------------------------------
! Note: PS is specified as the same as P0, ie 1000mb.
!--------------------------------------------------------------------------------

! Author:
!        IAP_L35_Pint.txt & IAP_L35_Pmid.txt, Zhang MingHua, ZhangHe, 20180131;
!        trans_txt2nc.F90, Chai ZhaoYang, 20180207;
!--------------------------------------------------------------------------------

use netcdf
implicit none
!--------------------------------------------------------------------------------
integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real

integer, parameter :: nlev = 35
integer, parameter :: nilev = nlev + 1

real(r8),parameter :: FillValue = real(1.0E36,r8)! Don't use FillValue=1.0E36

real(r8),parameter :: Ptop = 0.0_r8
real(r8),parameter :: P0 = 1000.0_r8            ! reference pressure (mb)
!real(r8)        :: PS                           ! surface pressure
real(r8), dimension(nilev)      :: Pint         ! interface pressure
real(r8), dimension(nlev)       :: Pmid         ! middle pressure
real(r8), dimension(nilev)      :: ilev         ! interface pressure layers
real(r8), dimension(nlev)       :: lev          ! middle pressure layers
real(r8), dimension(nlev)       :: PdelInt      ! delta P between two interface pressure
real(r8), dimension(nlev-1)       :: PdelMid      ! delta P between two interface pressure
real(r8), dimension(nilev)      :: SigmaInt     !
real(r8), dimension(nlev)       :: SigmaMid
real(r8), dimension(nlev)       :: SigmaDelInt
real(r8), dimension(nlev-1)       :: SigmaDelMid

real(r8), dimension(nilev)      :: hyai
real(r8), dimension(nilev)      :: hybi
real(r8), dimension(nlev)       :: hyam
real(r8), dimension(nlev)       :: hybm
!--------------------------------------------------------------------------------

character(len=20)       :: filename1, filename2
integer                 :: status       ! I/O status
integer                 :: file_unit
integer                 :: line
integer                 :: argc
character(len=132)      :: tmp_string


integer :: i, j, k
real(r8) :: tmp, tmp1, tmp2
!--------------------------------------------------------------------------------
! for write to nc

character(len=20)       :: filename
integer ::  ncid, lev_dimid, ilev_dimid
integer :: lev_varid, ilev_varid, hyai_varid, hybi_varid, hyam_varid,&
        hybm_varid, sigi_varid, sigm_varid, pint_varid, pmid_varid,&
       p0_varid, ptop_varid 

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!initialization
file_unit = 3
Pint = FillValue
Pmid = FillValue
ilev = FillValue
lev  = FillValue
PdelInt = FillValue
PdelMid = FillValue
SigmaInt = FillValue
SigmaMid = FillValue
SigmaDelInt = FillValue
SigmaDelMid = FillValue

hyai = FillValue
hybi = FillValue
hyam = FillValue
hybm = FillValue
!--------------------------------------------------------------------------------

! get the file name
argc = iargc()
!if (argc == 2) then
if (argc == 1) then
        call getarg(1,filename1)
!        call getarg(2,filename2)
else
        filename1 = "IAP_L35_Pint.txt"
!        filename2 = "IAP_L91_Pmid.txt"
endif
write(*,1000) filename1
!write(*,1000) filename2
1000 format(' ','The input file name is: ', A)

!--------------------------------------------------------------------------------
! open file and check errors
open(unit=file_unit, file=filename1, status='old', action='read', iostat=status)
openif: if (status == 0) then
        ! open successfully, get variables
        read(file_unit,100,iostat = status)
        100 format(A)
        do line = 2, 2+nilev-1
        !        read(file_unit, 990, iostat = status) tmp_string
        !        990 format (A)
        !        if (status /= 0) exit
        !        write(*,*)tmp_string
                read(file_unit, 990, iostat = status) i, tmp
                990 format(1X, I4, 3X, F12.7)
                if (status /= 0) exit
                k = nilev - i + 1
                Pint(k) = tmp
        !        write(*,990)k, tmp

        enddo
        readif: if (status > 0) then
                write(*,*)"error occurred!"
        elseif (status == -1) then
                write(*,*)"End of file reached!"
        elseif (status == -2) then
                write(*,*)"End of record reached!"
        else
                write(*,*)"End of loop reached!"
        endif readif
else openif
        write(*,1040) status
        1040 format(' ','Error opening file: iostat = ', I6)
endif openif
!close file
close (unit = file_unit)
write(*,1050) filename1
1050 format(' ','Finished reading: ', A)
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
! open file and check errors
#if 0
open(unit=file_unit, file=filename2, status='old', action='read', iostat=status)
openif2: if (status == 0) then
        ! open successfully, get variables
        read(file_unit,'(A)',iostat = status)
        do line = 2, 2+nlev-1
        !        read(file_unit, 990, iostat = status) tmp_string
        !        990 format (A)
        !        if (status /= 0) exit
        !        write(*,*)tmp_string
                read(file_unit, '(1X, I4, 2(3X, F12.7))', iostat = status) i, tmp1, tmp2
                if (status /= 0) exit
                k = nlev - i + 1
                Pmid(k) = tmp1
                PdelInt(k) = tmp2
        !        write(*,'(1X, I4, 2(3X, F12.7))')k, tmp1, tmp2

        enddo
        readif2: if (status > 0) then
                write(*,*)"error occurred!"
        elseif (status == -1) then
                write(*,*)"End of file reached!"
        elseif (status == -2) then
                write(*,*)"End of record reached!"
        else
                write(*,*)"End of loop reached!"
        endif readif2
else openif2
        write(*,1041) status
        1041 format(' ','Error opening file: iostat = ', I6)
endif openif2
!close file
close (unit = file_unit)
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!change Pmid & PdelInt!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! czy 20180325 because of the low accuracy of the FILE IAP91_Pmid.txt From M.H. Zhang.
do k = 1,nlev
        Pmid(k) = 0.5_r8*(Pint(k+1)+Pint(k))
        PdelInt(k) = Pint(k+1)-Pint(k)
enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(*,1050) filename2
!1050 format(' ','Finished reading: ', A)
!==========================================================================================
SigmaInt(1:nilev) = (Pint(1:nilev) - Ptop)/(Pint(nilev) - Ptop)
SigmaMid(1:nlev)  = (Pmid(1:nlev)  - ptop)/(Pint(nilev)  - Ptop)
hyai(1:nilev) = 0.0_r8
hybi(1:nilev)  = SigmaInt(1:nilev)
do k = 1, nlev
        SigmaDelInt(k)  = SigmaInt(k+1) - SigmaInt(k)
        PdelInt(k)      = Pint(k+1)  - Pint(k)
enddo
hyam(1:nlev) = 0.0_r8
hybm(1:nlev) = SigmaMid(1:nlev)
do k = 1, nlev-1
        SigmaDelMid(k)  = SigmaMid(k+1) - SigmaMid(k)
        PdelMid(k)      = Pmid(k+1)  - Pmid(k)
enddo
write(*,*)"  k      Pint(k)    SigmaInt(k)    Pmid(k)   SigmaMid(k)   PdelInt(k)"
write(*,*)"===== ============ ============ ============ ============ ============"
do k = 1, nlev
        write(*,300)k,Pint(k),SigmaInt(k),Pmid(k),SigmaMid(k),PdelInt(k)
enddo
write(*,300)nilev,Pint(nilev),SigmaInt(nilev)
write(*,*)"===== ============ ============ ============ ============ ============"
write(*,*)"  k      Pint(k)    SigmaInt(k)    Pmid(k)   SigmaMid(k)   PdelInt(k)"
write(*,*)"======================================================================"
write(*,*)"  k    PdelMid(k) SigmaDelMid(k)"
write(*,*)"===== ============ ============"
do k = 1, nlev-1
        write(*,400)k,PdelMid(k), SigmaDelMid(k)
enddo
300 format(1X, I4, 5(1X,F12.7))
400 format(1X, I4, 2(1X,F12.7))

#ifndef printonly
!================================================================================
! prepare vars:
! pint, pmid
! sigi(1:nilev) = SigmaInt(1:nilev)
! sigm(1:nlev) = SigmaMid(1:nlev)
ilev(1:nilev) = 1000.0_r8 * SigmaInt(1:nilev)
lev(1:nlev)   = 1000.0_r8 * SigmaMid(1:nlev)
!================================================================================
! write ilev(ilev), hyai(ilev), hybi(ilev), sigi(ilev), pint(ilev)
!       lev(lev), hyam(lev), hybm(lev), sigm(lev), pmid(lev)
!================================================================================
filename = 'IAPL35coordinate.nc' !output ncfile
call check( nf90_create(filename, nf90_clobber, ncid) )

call check( nf90_def_dim(ncid, 'lev', nlev, lev_dimid) )
call check( nf90_def_dim(ncid, 'ilev', nilev, ilev_dimid) )
!--------------------------------------------------------------------------------
! var: lev
call check( nf90_def_var(ncid, 'lev',  NF90_DOUBLE, lev_dimid, lev_varid) )
call check( nf90_put_att(ncid, lev_varid, "long_name", "hybrid level at midpoints (1000*(A+B))") )
call check( nf90_put_att(ncid, lev_varid, "units", "level") )
call check( nf90_put_att(ncid, lev_varid, "positive", "down") )
call check( nf90_put_att(ncid, lev_varid, "_FillValue", FillValue) )
call check( nf90_put_att(ncid, lev_varid, "standard_name","atmosphere_hybrid_sigma_pressure_coordinate") )
call check( nf90_put_att(ncid, lev_varid, "formula_terms", "a: hyam b: hybm p0: P0 ps: PS") )
! var: ilev
call check( nf90_def_var(ncid, 'ilev',  NF90_DOUBLE, ilev_dimid, ilev_varid) )
call check( nf90_put_att(ncid, ilev_varid, "long_name", "hybrid level at interfaces (1000*(A+B))") )
call check( nf90_put_att(ncid, ilev_varid, "units", "level") )
call check( nf90_put_att(ncid, ilev_varid, "positive", "down") )
call check( nf90_put_att(ncid, ilev_varid, "_FillValue", FillValue) )
call check( nf90_put_att(ncid, ilev_varid, "standard_name","atmosphere_hybrid_sigma_pressure_coordinate") )
call check( nf90_put_att(ncid, ilev_varid, "formula_terms", "a: hyai b: hybi p0: P0 ps: PS") )
! var: hyai, hybi
call check( nf90_def_var(ncid, 'hyai',  NF90_DOUBLE, ilev_dimid, hyai_varid) )
call check( nf90_put_att(ncid, hyai_varid, "long_name", "hybrid A coefficient at layer interfaces") )
call check( nf90_put_att(ncid, hyai_varid, "units", "1") )
call check( nf90_put_att(ncid, hyai_varid, "_FillValue", FillValue) )

call check( nf90_def_var(ncid, 'hybi',  NF90_DOUBLE, ilev_dimid, hybi_varid) )
call check( nf90_put_att(ncid, hybi_varid, "long_name", "hybrid B coefficient at layer interfaces") )
call check( nf90_put_att(ncid, hybi_varid, "units", "1") )
call check( nf90_put_att(ncid, hybi_varid, "_FillValue", FillValue) )
! var: hyam, hybm
call check( nf90_def_var(ncid, 'hyam',  NF90_DOUBLE, lev_dimid, hyam_varid) )
call check( nf90_put_att(ncid, hyam_varid, "long_name", "hybrid A coefficient at layer midpoints") )
call check( nf90_put_att(ncid, hyam_varid, "units", "1") )
call check( nf90_put_att(ncid, hyam_varid, "_FillValue", FillValue) )

call check( nf90_def_var(ncid, 'hybm',  NF90_DOUBLE, lev_dimid, hybm_varid) )
call check( nf90_put_att(ncid, hybm_varid, "long_name", "hybrid B coefficient at layer midpoints") )
call check( nf90_put_att(ncid, hybm_varid, "units", "1") )
call check( nf90_put_att(ncid, hybm_varid, "_FillValue", FillValue) )
! var: sigi, pint
call check( nf90_def_var(ncid, 'sigi',  NF90_DOUBLE, ilev_dimid, sigi_varid) )
call check( nf90_put_att(ncid, sigi_varid, "long_name", "sigma coordinate at interfaces (pint-ptop)/(pint(ilev)-ptop)") )
call check( nf90_put_att(ncid, sigi_varid, "units", "1") )
call check( nf90_put_att(ncid, sigi_varid, "_FillValue", FillValue) )

call check( nf90_def_var(ncid, 'pint',  NF90_DOUBLE, ilev_dimid, pint_varid) )
call check( nf90_put_att(ncid, pint_varid, "long_name", "reference interface pressure used to define sigma coordinate") )
call check( nf90_put_att(ncid, pint_varid, "units", "hPa") )
call check( nf90_put_att(ncid, pint_varid, "_FillValue", FillValue) )
! var: sigm, pmid
call check( nf90_def_var(ncid, 'sigm',  NF90_DOUBLE, lev_dimid, sigm_varid) )
call check( nf90_put_att(ncid, sigm_varid, "long_name", "sigma coordinate at midpoints (pmid-ptop)/(pmid(lev)-ptop)") )
call check( nf90_put_att(ncid, sigm_varid, "units", "1") )
call check( nf90_put_att(ncid, sigm_varid, "_FillValue", FillValue) )

call check( nf90_def_var(ncid, 'pmid',  NF90_DOUBLE, lev_dimid, pmid_varid) )
call check( nf90_put_att(ncid, pmid_varid, "long_name", "reference midpoint pressure used to define sigma coordinate") )
call check( nf90_put_att(ncid, pmid_varid, "units", "hPa") )
call check( nf90_put_att(ncid, pmid_varid, "_FillValue", FillValue) )
! var: P0, Ptop
call check( nf90_def_var(ncid, 'P0',  NF90_DOUBLE, p0_varid) )
call check( nf90_put_att(ncid, p0_varid, "long_name", "reference pressure used to calculate P=A*P0+B*PS") )
call check( nf90_put_att(ncid, p0_varid, "units", "hPa") )
call check( nf90_put_att(ncid, p0_varid, "_FillValue", FillValue) )

call check( nf90_def_var(ncid, 'Ptop',  NF90_DOUBLE, ptop_varid) )
call check( nf90_put_att(ncid, ptop_varid, "long_name", "model top pressure used to define sigma coordinate") )
call check( nf90_put_att(ncid, ptop_varid, "units", "hPa") )
call check( nf90_put_att(ncid, ptop_varid, "_FillValue", FillValue) )

call check( nf90_enddef(ncid) )
!----------------------------------------------------------------------------------

call check( nf90_put_var(ncid, ilev_varid, ilev) )
call check( nf90_put_var(ncid, hyai_varid, hyai) )
call check( nf90_put_var(ncid, hybi_varid, hybi) )
call check( nf90_put_var(ncid, lev_varid, lev) )
call check( nf90_put_var(ncid, hyam_varid, hyam) )
call check( nf90_put_var(ncid, hybm_varid, hybm) )
call check( nf90_put_var(ncid, sigi_varid, SigmaInt) )
call check( nf90_put_var(ncid, sigm_varid, SigmaMid) )
call check( nf90_put_var(ncid, pint_varid, Pint) )
call check( nf90_put_var(ncid, pmid_varid, Pmid) )
call check( nf90_put_var(ncid, p0_varid, P0) )
call check( nf90_put_var(ncid, ptop_varid, Ptop) )

call check( nf90_close(ncid) )
#endif

end program trans_txt2nc

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
