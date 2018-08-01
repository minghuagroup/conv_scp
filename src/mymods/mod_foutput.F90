module mod_foutput

use spmd_utils,     only: iam

implicit none
save

public foutput, foutput_6var, foutput_1d, foutput_2d


contains
!=============================================================================
subroutine foutput_2d(str, indata, nx, ny)
    use shr_kind_mod,       only: r8 => shr_kind_r8
    use time_manager,       only: get_nstep
implicit none
    character(len=*) :: str
    integer :: nx, ny
    real(r8) :: indata(nx, ny)

    character(len=255) :: tstr, cpustr
    character(len=255) :: fname 
    integer :: nstep, k
    
    nstep = get_nstep()
    write (tstr, '(I5.5)' ) nstep
    write (cpustr, '(I4.4)') iam
    fname = './foutput/foutput_'//trim(cpustr)//'_step_'//trim(tstr)//'.txt'
    open(unit=10, file=trim(fname), position="append")
    write(10, *) str, minval(indata), maxval(indata)
    close(10)
end subroutine foutput_2d
!=============================================================================
subroutine foutput_1d(str, indata, insize)
    use shr_kind_mod,       only: r8 => shr_kind_r8
    use time_manager,       only: get_nstep
implicit none
    character(len=*) :: str
    integer :: insize
    real(r8) :: indata(insize)

    character(len=255) :: tstr, cpustr
    character(len=255) :: fname 
    integer :: nstep, k
    
    nstep = get_nstep()
    write (tstr, '(I5.5)' ) nstep
    write (cpustr, '(I4.4)') iam
    fname = './foutput/foutput_'//trim(cpustr)//'_step_'//trim(tstr)//'.txt'
    open(unit=10, file=trim(fname), position="append")
    write(10, *) str, minval(indata(1:insize)), maxval(indata(1:insize))
    close(10)
end subroutine foutput_1d
!=============================================================================
subroutine foutput(str, state)
    use shr_kind_mod,       only: r8 => shr_kind_r8
    use ppgrid,             only: pver, pcols, pverp
    use time_manager,       only: get_nstep
    use physics_types,      only: physics_state
    use physics_buffer,     only: physics_buffer_desc
implicit none
    character(len=*) :: str
    type(physics_state) :: state
    character(len=255) :: tstr
    character(len=255) :: fname 
    integer :: nstep, k
    
    nstep = get_nstep()
    write (tstr, '(I5.5)' ) nstep
    fname = './testoutput/state_step_'//trim(tstr)//'.txt'
    open(unit=10, file=trim(fname), position="append")
    write(10,*) str
    do k = 1,pver,1
        write(10,"(F10.2, F10.4, F10.7, F10.2, F10.3)") state%pmid(1,k), state%t(1,k), state%q(1,k,1), state%zm(1,k), state%pdel(1,k)
    end do
    close(10)
end subroutine foutput
!=============================================================================
subroutine foutput_6var(str, var1,var2,var3,var4,var5,var6)
    use shr_kind_mod,       only: r8 => shr_kind_r8
    use ppgrid,             only: pver, pcols, pverp
    use time_manager,       only: get_nstep
    use physics_types,      only: physics_state
    use physics_buffer,     only: physics_buffer_desc
implicit none
    character(len=*) :: str
    real(r8) :: var1(pver)
    real(r8) :: var2(pver)
    real(r8) :: var3(pver)
    real(r8) :: var4(pver)
    real(r8) :: var5(pver)
    real(r8) :: var6(pver)
    character(len=255) :: tstr
    character(len=255) :: fname 
    integer :: nstep, k
    
    nstep = get_nstep()
    write (tstr, '(I5.5)' ) nstep
    fname = './testoutput/state_step_'//trim(tstr)//'.txt'
    open(unit=10, file=trim(fname), position="append")
    write(10,*) str
    do k = 1,pver,1
        write(10,"(6F16.6)") var1(k), var2(k), var3(k), var4(k), var5(k), var6(k)
    end do
    close(10)
end subroutine foutput_6var



end module mod_foutput
