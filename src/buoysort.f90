
module buoysort

    implicit none
    private
    save

    public :: cal_buoysort

    integer,parameter :: r8 = selected_real_kind(12)

    real(r8), parameter :: latvap = 2.501e6
    real(r8), parameter :: latice = 3.34e5
    real(r8), parameter :: latall = latvap+latice

    real(r8), parameter :: mwh2o = 34._r8
    real(r8), parameter :: mwdry = 28.966_R8 
    real(r8), parameter :: ep2 = mwh2o/mwdry

    real(r8), parameter :: g = 9.8_r8

    real(r8), parameter :: p00   = 1.e5_r8
    real(r8), parameter :: rair   = 287.042311365
    real(r8), parameter :: cpair  = 1004.64
    real(r8), parameter :: rovcp=rair/cpair

    real(r8), parameter :: epsilo = 0.6219705862
    real(r8), parameter :: tveps  = 1.0/epsilo - 1

    !real(r8) :: cridis, rle, scaleh

    !real(r8) :: p

    !real(r8) :: xc

    !real(r8) :: thlue, qtue
    !real(r8) :: thle, qte
    !real(r8) :: wue


    !rle = 0.1_r8
    !scaleh = 8.e3
    !cridis = rle*scaleh

    !p = 900.e2
    !thle = 300._r8
    !qte = 0.01_r8
    !thlue = 302._r8
    !qtue = 0.05_r8

    !wue = 1._r8

    !call cal_buoysort(cridis, p, thle, qte, thlue, qtue, wue, xc )

contains



subroutine cal_buoysort(cridis, z, p, rho, thle, qte, thlue, qtue, wue, xc, ent_rate, det_rate)


    real(r8), intent(in) :: cridis
    real(r8), intent(in) :: z, p, rho
    real(r8), intent(in) :: thle, qte, thlue, qtue
    real(r8), intent(in) :: wue
    real(r8), intent(out) :: xc
    real(r8), intent(out) :: ent_rate, det_rate
!    real(r8), intent(out) :: fer, fdr
    real(r8) :: fer, fdr

    real(r8), parameter :: rbuoy = 1._r8
    real(r8), parameter :: rkm = 14.0_r8

    real(r8) :: exne

    real(r8) :: tj, thvj, thv0j
    real(r8) :: excessu, excess0

    real(r8) :: qsat_arg, thj, qvj, qlj, qij, qse
    real(r8) :: qs

    real(r8) :: aquad, bquad, cquad
    real(r8) :: xsat, thlxsat, qtxsat
    real(r8) :: thv_x0, thv_x1, x_en, x_cu, thvxsat
    real(r8) :: xs1, xs2

    real(r8) :: ee2,ud2,rei

    integer :: id_check, exit_conden
    logical :: id_exit

    integer :: kk, stat


    exne = (p/p00)**rovcp

    call conden(p,thle,qte,thj,qvj,qlj,qij,qse,id_check)
    thv0j = thj * ( 1._r8 + tveps * qvj - qlj - qij )
    tj   = thj * exne
!thle = thle + (latvap/cpair/exne)*exql + (latall/cpair/exne)*exqi
    qsat_arg = thle*exne
    call cal_qsat(qsat_arg, p, qs)
    excess0 = qte-qs

    !write(*,"(5a20)") "thle", "qte", "qs", "excess0"
    !write(*,"(5f20.10)") thle, qte, qs, excess0


    call conden(p,thlue,qtue,thj,qvj,qlj,qij,qse,id_check)
    thvj = thj * ( 1._r8 + tveps * qvj - qlj - qij )
    tj   = thj * exne
!thlue = thlue + (latvap/cpair/exne)*exql + (latall/cpair/exne)*exqi
    qsat_arg = thlue*exne
    call cal_qsat(qsat_arg, p, qs)
    excessu = qtue-qs

    !write(*,"(5a20)") "thlue", "qtue", "qs", "excessu"
    !write(*,"(5f20.10)") thlue, qtue, qs, excessu


    if ( ( excessu .le. 0._r8 .and. excess0 .le. 0._r8 ) .or. ( excessu .ge. 0._r8 .and. excess0 .ge. 0._r8 ) ) then
        xc = min(1._r8,max(0._r8,1._r8-2._r8*rbuoy*g*cridis/wue**2._r8*(1._r8-thvj/thv0j)))
              ! Below 3 lines are diagnostic output not influencing
              ! numerical calculations.
        aquad = 0._r8
        bquad = 0._r8
        cquad = 0._r8
    else
          ! -------------------------------------------------- !
          ! Case 2 : When either cumulus or env. is saturated. !
          ! -------------------------------------------------- !
        xsat    = excessu / ( excessu - excess0 )
        thlxsat = thlue + xsat * ( thle - thlue )
        qtxsat  = qtue  + xsat * ( qte - qtue )
        call conden(p,thlxsat,qtxsat,thj,qvj,qlj,qij,qse,id_check)
        if( id_check .eq. 1 ) then
            exit_conden = 1._r8
            id_exit = .true.
            write(*,*) 'error'
        end if
        thvxsat = thj * ( 1._r8 + tveps * qvj - qlj - qij )
              ! -------------------------------------------------- !
              ! kk=1 : Cumulus Segment, kk=2 : Environment Segment !
              ! -------------------------------------------------- !
        do kk = 1, 2
!            write(*,*) "kk=", kk
            if( kk .eq. 1 ) then
                thv_x0 = thvj
                thv_x1 = ( 1._r8 - 1._r8/xsat ) * thvj + ( 1._r8/xsat ) * thvxsat
            else
                thv_x1 = thv0j
                thv_x0 = ( xsat / ( xsat - 1._r8 ) ) * thv0j + ( 1._r8/( 1._r8 - xsat ) ) * thvxsat
            endif
            aquad =  wue**2
            bquad =  2._r8*rbuoy*g*cridis*(thv_x1 - thv_x0)/thv0j - 2._r8*wue**2
            cquad =  2._r8*rbuoy*g*cridis*(thv_x0 -  thv0j)/thv0j +       wue**2

            !write(*,*)
            !write(*,"(5a20)") "a", "b", "c", "b2-4ab"
            !write(*,"(5f20.10)") aquad, bquad, cquad, bquad**2-4._r8*aquad*cquad

            if( kk .eq. 1 ) then
                if( ( bquad**2-4._r8*aquad*cquad ) .ge. 0._r8 ) then
                    call roots(aquad,bquad,cquad,xs1,xs2,stat)
                    x_cu = min(1._r8,max(0._r8,min(xsat,min(xs1,xs2))))
!                    write(*,*) "solve x_cu", x_cu, xs1, xs2
                else
                    x_cu = xsat
                endif
            else
                if( ( bquad**2-4._r8*aquad*cquad) .ge. 0._r8 ) then
                    call roots(aquad,bquad,cquad,xs1,xs2,stat)
                    x_en = min(1._r8,max(0._r8,max(xsat,min(xs1,xs2))))
!                    write(*,*) "solve x_en", x_en, xs1, xs2
                else
                    x_en = 1._r8
                endif
            endif

            !write(*,"(5a20)") "xsat", "x_cu", "x_en"
            !write(*,"(5f20.10)") xsat, x_cu, x_en

        enddo

        if( x_cu .eq. xsat ) then
            xc = max(x_cu, x_en)
        else
            xc = x_cu
        endif

    endif

    ee2    = xc**2
    ud2    = 1._r8 - 2._r8*xc + xc**2

    rei = ( 0.5_r8 * rkm / z / g /rho )

!    if( xc .gt. 0.5_r8 ) rei = min(rei,0.9_r8*log(dp0(k)/g/dt/umf(km1) + 1._r8)/dpe/(2._r8*xc-1._r8))
    fer = rei * ee2
    fdr = rei * ud2

    !write(*,"(6a20)") "xc", "ee2", "ud2", "fer", "fdr"
    !write(*,"(6f20.10)") xc, ee2, ud2, fer, fdr
    !write(*,*)

    ent_rate = fer
    det_rate = fdr

end subroutine cal_buoysort

real(r8) function exnf(pressure)
    real(r8), intent(in)              :: pressure
    exnf = (pressure/p00)**rovcp
    return
end function exnf

subroutine roots(a,b,c,r1,r2,status)
  ! --------------------------------------------------------- !
  ! Subroutine to solve the second order polynomial equation. !
  ! I should check this subroutine later.                     !
  ! --------------------------------------------------------- !
    real(r8), intent(in)  :: a
    real(r8), intent(in)  :: b
    real(r8), intent(in)  :: c
    real(r8), intent(out) :: r1
    real(r8), intent(out) :: r2
    integer , intent(out) :: status
    real(r8)              :: q

    status = 0

    if( a .eq. 0._r8 ) then                            ! Form b*x + c = 0
        if( b .eq. 0._r8 ) then                        ! Failure: c = 0
            status = 1
        else                                           ! b*x + c = 0
            r1 = -c/b
        endif
        r2 = r1
    else
        if( b .eq. 0._r8 ) then                        ! Form a*x**2 + c = 0
            if( a*c .gt. 0._r8 ) then                  ! Failure: x**2 = -c/a < 0
                status = 2
            else                                       ! x**2 = -c/a 
                r1 = sqrt(-c/a)
            endif
            r2 = -r1
        else                                            ! Form a*x**2 + b*x + c = 0
            if( (b**2 - 4._r8*a*c) .lt. 0._r8 ) then   ! Failure, no real roots
                status = 3
            else
                q  = -0.5_r8*(b + sign(1.0_r8,b)*sqrt(b**2 - 4._r8*a*c))
                r1 =  q/a
                r2 =  c/q
            endif
        endif
    endif

    return
end subroutine roots

subroutine cal_qsat( t, p, qsat)
    real(r8) :: t, p, qsat
    qsat = epsilo * 611.2*exp(17.67*(t-273.15)/(t-273.15+243.5)) / p
end subroutine cal_qsat

subroutine conden(p,thl,qt,th,qv,ql,qi,rvls,id_check)
  ! --------------------------------------------------------------------- !
  ! Calculate thermodynamic properties from a given set of ( p, thl, qt ) !
  ! --------------------------------------------------------------------- !
    implicit none
    real(r8), intent(in)  :: p
    real(r8), intent(in)  :: thl
    real(r8), intent(in)  :: qt
    real(r8), intent(out) :: th
    real(r8), intent(out) :: qv
    real(r8), intent(out) :: ql
    real(r8), intent(out) :: qi
    real(r8), intent(out) :: rvls
    integer , intent(out) :: id_check
    real(r8)              :: tc,temps,t
    real(r8)              :: leff, nu, qc
    integer               :: iteration
    real(r8)              :: es              ! Saturation vapor pressure
    real(r8)              :: qs              ! Saturation spec. humidity


    tc   = thl*exnf(p)
  ! Modification : In order to be compatible with the dlf treatment in stratiform.F90,
  !                we may use ( 268.15, 238.15 ) with 30K ramping instead of 20 K,
  !                in computing ice fraction below. 
  !                Note that 'cldfrc_fice' uses ( 243.15, 263.15 ) with 20K ramping for stratus.
    nu   = max(min((268._r8 - tc)/20._r8,1.0_r8),0.0_r8)  ! Fraction of ice in the condensate. 
    leff = (1._r8 - nu)*latvap + nu*latall                      ! This is an estimate that hopefully speeds convergence

    ! --------------------------------------------------------------------------- !
    ! Below "temps" and "rvls" are just initial guesses for iteration loop below. !
    ! Note that the output "temps" from the below iteration loop is "temperature" !
    ! NOT "liquid temperature".                                                   !
    ! --------------------------------------------------------------------------- !
    temps  = tc
    call cal_qsat(temps, p, qs)
    rvls   = qs

    if( qs .ge. qt ) then
        id_check = 0
        qv = qt
        qc = 0._r8
        ql = 0._r8
        qi = 0._r8
        th = tc/exnf(p)
    else
        do iteration = 1, 10
            temps  = temps + ( (tc-temps)*cpair/leff + qt - rvls )/( cpair/leff + ep2*leff*rvls/rair/temps/temps )
            call cal_qsat(temps, p, qs)
            rvls   = qs
        end do
        qc = max(qt - qs,0._r8)
        qv = qt - qc
        ql = qc*(1._r8 - nu)
        qi = nu*qc
        th = temps/exnf(p)
        if( abs((temps-(leff/cpair)*qc)-tc) .ge. 1._r8 ) then
            id_check = 1
        else
            id_check = 0
        end if
    end if

    return
end subroutine conden



end module buoysort

