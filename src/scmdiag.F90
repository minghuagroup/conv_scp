
module scmdiag

!---------------------------------------------------------------------------------
! Purpose:
! Simple CESM diag, netcdf outputer
!
! Contributor: Xin Xie
!---------------------------------------------------------------------------------

use netcdf

implicit none
private
save

public :: subcol_netcdf_init
public :: subcol_netcdf_setdim
public :: subcol_netcdf_addfld
public :: subcol_netcdf_putfld
public :: subcol_netcdf_putclm
public :: subcol_netcdf_nextstep
public :: subcol_netcdf_end

integer, parameter :: nvarmax = 200
integer, parameter :: r8 = selected_real_kind(12)

integer :: nsubcol, nlev, nlevp

!private data for subcol_netcdf
type cdf_diag

    integer :: ncid

    integer :: subcol_dimid, lev_dimid, levp_dimid, rec_dimid

    integer :: lev_varid, subcol_varid

    integer :: nvar
    integer       :: varidlist(nvarmax)
    character(50) :: varnamelist(nvarmax)
    character(50) :: vartypelist(nvarmax)
    integer :: varstartlist(nvarmax)
    integer :: curtimestep

end type cdf_diag

logical :: netcdf_enddef = .false.
type (cdf_diag) output

contains


subroutine subcol_netcdf_setdim( innsubcol, innlev )
    integer, intent(in) :: innsubcol, innlev

    nsubcol = innsubcol
    nlev = innlev
    nlevp = innlev+1
end subroutine subcol_netcdf_setdim


subroutine subcol_netcdf_init( outfile )
    character(*), intent(in) :: outfile

    character(*), parameter :: units = "units"

    character(*), parameter :: subcol_name = "subcol"
    character(*), parameter :: lev_name    = "lev"
    character(*), parameter :: levp_name   = "levp"
    character(*), parameter :: rec_name    = "time"

    call netcdf_check( nf90_create( trim(outfile), nf90_clobber, output%ncid) )

    call netcdf_check( nf90_def_dim(output%ncid, subcol_name, &
        nsubcol, output%subcol_dimid) )
    call netcdf_check( nf90_def_dim(output%ncid, lev_name,    &
        nlev   , output%lev_dimid)    )
    call netcdf_check( nf90_def_dim(output%ncid, levp_name,    &
        nlevp  , output%levp_dimid)    )
    call netcdf_check( nf90_def_dim(output%ncid, rec_name,    &
        NF90_UNLIMITED, output%rec_dimid)    )

    output%nvar = 0
    output%varidlist = 0
    output%varstartlist = 0
    output%curtimestep = 0

    write(*,*) "[subcol_netcdf_init]"

end subroutine subcol_netcdf_init



subroutine subcol_netcdf_addfld(varname, varunit, vartype)
    character(*), intent(in) :: varname
    character(*), intent(in) :: varunit
    character(*), intent(in) :: vartype

    integer :: dimids(3)
    integer :: ndim

    if ( netcdf_enddef .eqv. .true.) then
        write(*,*) "enddef marked, can not add variable."
        return
    end if
    ! write(*,*) "add ", varname

    if (vartype == "slev") then
        ndim = 2
        dimids(1:ndim) = (/ output%subcol_dimid, output%rec_dimid /)
    else if (vartype == "mlev") then
        ndim = 3
        dimids(1:ndim) = (/ output%subcol_dimid, output%lev_dimid, output%rec_dimid /)
    else if (vartype == "mlevp") then
        ndim = 3
        dimids(1:ndim) = (/ output%subcol_dimid, output%levp_dimid, output%rec_dimid /)
    else if (vartype == "stdmlev") then
        ndim = 2
        dimids(1:ndim) = (/ output%lev_dimid, output%rec_dimid /)
    end if

    output%nvar = output%nvar + 1

    output%vartypelist(output%nvar) = vartype

    call netcdf_check( nf90_def_var(output%ncid, trim(varname), NF90_REAL,&
        dimids(1:ndim), output%varidlist(output%nvar)) )

    call netcdf_check( nf90_put_att(output%ncid, &
        output%varidlist(output%nvar), "units", trim(varunit) ) )
    call netcdf_check( nf90_put_att(output%ncid, &
        output%varidlist(output%nvar), "_FillValue", nf90_fill_real ) )


    output%varnamelist(output%nvar) = trim(varname)
end subroutine subcol_netcdf_addfld



!subroutine used to output the field over all subcolumns.
subroutine subcol_netcdf_putfld(varname, input)
    character(*), intent(in) :: varname
    real(r8),     intent(in) :: input(nsubcol,*)
    integer :: i, varind
    integer :: starts(3), counts(3)
    integer :: ndim

!  write(*,*) "put ", varname
    if ( netcdf_enddef .eqv. .false. ) then
        netcdf_enddef = .true.
        call netcdf_check( nf90_enddef(output%ncid) )
    end if

!locate the var position in the list.
    varind = 0
    do i = 1, output%nvar
        if ( output%varnamelist(i) == varname) then
            varind = i
        end if
    end do

    output%varstartlist(varind) = output%varstartlist(varind) + 1

!put the var
    if ( output%vartypelist(varind) == "mlev" ) then
        ndim = 3
!     starts(1:ndim) = (/ 1, 1, output%varstartlist(varind) /)
        starts(1:ndim) = (/ 1, 1, output%curtimestep /)
        counts(1:ndim) = (/ nsubcol, nlev, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(:, 1:nlev), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    else if ( output%vartypelist(varind) == "mlevp" ) then
        ndim = 3
!     starts(1:ndim) = (/ 1, 1, output%varstartlist(varind) /)
        starts(1:ndim) = (/ 1, 1, output%curtimestep /)
        counts(1:ndim) = (/ nsubcol, nlevp, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(:, 1:nlevp), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    else if ( output%vartypelist(varind) == "slev" ) then
        ndim = 2
!     starts(1:ndim) = (/ 1, output%varstartlist(varind) /)
        starts(1:ndim) = (/ 1, output%curtimestep /)
        counts(1:ndim) = (/ nsubcol, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(:, 1), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    else if ( output%vartypelist(varind) == "stdmlev" ) then
        ndim = 2
!     starts(1:ndim) = (/ 1, output%varstartlist(varind) /)
        starts(1:ndim) = (/ 1, output%curtimestep /)
        counts(1:ndim) = (/ nlev, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(:, 1), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    end if

!  write(*,*) starts(1:ndim)
!  write(*,*) counts(1:ndim)
end subroutine subcol_netcdf_putfld



!subroutine used to output the field over all subcolumns.
subroutine subcol_netcdf_putclm(varname, in_nlev, input, isubcol)
    character(*), intent(in) :: varname
    integer ,     intent(in) :: in_nlev
    real(r8),     intent(in) :: input(in_nlev)
    integer ,     intent(in) :: isubcol
    integer :: i, varind
    integer :: starts(3), counts(3)
    integer :: ndim

!  write(*,*) "put ", varname, " isub:", isubcol
    if ( netcdf_enddef .eqv. .false. ) then
        netcdf_enddef = .true.
        call netcdf_check( nf90_enddef(output%ncid) )
    end if

   ! do i = 1, in_nlev
    !    if (isnan(input(i))) then
    !        write(*, *) 'scmdiag:putclm:nan:', varname, isubcol
    !    end if
        !input(i) = min(1e12, max(-1e12, input(i)))
   ! end do

    varind = 0
    do i = 1, output%nvar
        if ( output%varnamelist(i) == varname) then
            varind = i
        end if
    end do
    if ( varind == 0 ) then
        write(*,*) "CANNOT FIND OUTPUT FIELD ", varname
        return
    end if
    if ( output%vartypelist(varind) == "mlev" ) then
        if ( in_nlev .ne. nlev ) then
            write(*,*) "INCONSISTENT VERTICAL DIMENSION:", varname
            write(*,*) in_nlev, "given ", nlev, "needed"
            return
        end if
        ndim = 3
!     starts(1:ndim) = (/ isubcol, 1, output%varstartlist(varind) /)
        starts(1:ndim) = (/ isubcol, 1, output%curtimestep /)
        counts(1:ndim) = (/ 1, nlev, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(1:nlev), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    else if ( output%vartypelist(varind) == "mlevp" ) then
        if ( in_nlev .ne. nlevp ) then
            write(*,*) "INCONSISTENT VERTICAL DIMENSION:", varname
            write(*,*) in_nlev, "given ", nlev, "needed"
            return
        end if
        ndim = 3
!     starts(1:ndim) = (/ isubcol, 1, output%varstartlist(varind) /)
        starts(1:ndim) = (/ isubcol, 1, output%curtimestep /)
        counts(1:ndim) = (/ 1, nlevp, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(1:nlevp), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    else if ( output%vartypelist(varind) == "slev" ) then
        if ( in_nlev .ne. 1 ) then
            write(*,*) "INCONSISTENT VERTICAL DIMENSION:", varname
            write(*,*) in_nlev, "given ", nlev, "needed"
            return
        end if
        ndim = 2
!     starts(1:ndim) = (/ isubcol, output%varstartlist(varind) /)
        starts(1:ndim) = (/ isubcol, output%curtimestep /)
        counts(1:ndim) = (/ 1, 1 /)
        call netcdf_check( nf90_put_var(output%ncid, &
            output%varidlist(varind), input(1:1), &
            start=starts(1:ndim), &
            count=counts(1:ndim)) )
    end if

!  write(*,*) starts(1:ndim)
!  write(*,*) counts(1:ndim)
end subroutine subcol_netcdf_putclm



subroutine subcol_netcdf_nextstep
    output%curtimestep = output%curtimestep + 1
end subroutine subcol_netcdf_nextstep



subroutine subcol_netcdf_end
    write(*,*) "[subcol_netcdf_end]"
    call netcdf_check( nf90_close(output%ncid) )
end subroutine subcol_netcdf_end



subroutine netcdf_check( status )
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then 
        print *, trim(nf90_strerror(status))
        stop "Stopped"
    end if
end subroutine netcdf_check


end module scmdiag
