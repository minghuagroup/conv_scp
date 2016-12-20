
module jp_conv
!------------------------------------------------------
! Stochastic Convective Parameterization Scheme
! implemented by Xin Xie
!------------------------------------------------------
! Convention:
! SI unit
! ***************************************************
!                ATMOSPHERE TOP k=1
! ***************************************************
!                         .
!                         .
!                         .
!                         .  CLOUD TOP (NEUTRAL BOUYANCY)[kuptop]
!                         .
!                         .
!                         .
! ********cloud vars are at the interface************       K+1/2 K_cloud=1
!
! **variables : all input vars are in the mid level**       K=1
!                         .
! ********cloud vars are at the interface************       K+1/2 K_cloud=2
!                     ...........
!                     ...........
!                     ...........
!                     ...........
! ********cloud vars are at the interface************       K+1/2 K_cloud=nlev
!
! **variables : all input vars are in the mid level**       K=nlev
!
! ********cloud vars are at the interface************       K+1/2 K_cloud=nlev+1
!                         .
!                         .  CLOUD BASE (POSITIVE BOUYANCY) [kupbase]
!                         .
!                         .
!                         .  LCL: [kuplcl]
!                         .
!                         .
!                         .  LAUNCH LEVEL: [kuplaunch] max MSE
!                         .
! ***************************************************
!                ATMOSPHERE BOTTOM k=nlev
! ***************************************************
!
!------------------------------------------------------

#ifdef SCMDIAG
    use scmdiag, only: subcol_netcdf_nextstep
    use scmdiag, only: subcol_netcdf_putclm, subcol_netcdf_putfld
#endif

    implicit none
    private
    save

    integer,parameter :: r8 = selected_real_kind(12)

    public :: scp_conv_init, scp_conv_tend

    integer :: ncol=0, nlev=0, nlevp=0

!physical parameter
    real(r8), parameter :: gravit = 9.80616    ! gravitational acceleration (m/s**2)
    real(r8), parameter :: pi     = 3.141592653 ! Pi
    real(r8), parameter :: cpair  = 1004.64     ! specific heat of dry air (J/K/kg)
    real(r8), parameter :: cpliq  = 4188.       ! specific heat of fresh h2o (J/K/kg)
    real(r8), parameter :: cpwv   = 1810.       ! specific heat of water vapor (J/K/kg)
    real(r8), parameter :: latvap = 2501000.    ! Latent heat of vaporization (J/kg)
    real(r8), parameter :: epsilo = 0.6219705862 ! ratio of h2o to dry air molecular weights
    real(r8), parameter :: tmelt  = 273.15       ! Freezing point of water (K)
    real(r8), parameter :: rair   = 287.042311365   ! Dry air gas constant     ~ J/K/kg
    real(r8), parameter :: rh2o   = 461.5046398202  ! Water vapor gas constant ~ J/K/kg
    real(r8), parameter :: rhofw  = 1000. ! liquid water density ~ J/K/kg

    real(r8), parameter :: min_q=1.e-12

!parameter for subcloud entrainment
!    real(r8), parameter :: ent_rate_subcld=2.e-4_r8
!    real(r8), parameter :: ent_rate_subcld=0.0001_r8
    real(r8), parameter :: ent_rate_subcld=0._r8
!parameter for bulk TOTAL fractional en/detrainment rate depending on RH
    real(r8), parameter :: fe_up_dp=1.0_r8, fe_up_sh=2.0_r8
    real(r8), parameter :: e_up_dp=1.25e-3_r8, d_up_dp=0.5e-4_r8
!    real(r8), parameter :: e_up_dp=1.75e-3_r8, d_up_dp=0.75e-4_r8 !original
    real(r8), parameter :: e_up_sh=1.75e-3_r8

    integer :: ent_opt ! 0=ec, 1=greg

!parameter for bulk TOTAL fractional en/detrainment rate depending on vertical velocity
!old
!    real(r8), parameter :: greg_a=0.2_r8, greg_ce=0.4_r8
!    real(r8), parameter :: greg_a=0.08_r8, greg_ce=0.15_r8
    real(r8), parameter :: greg_a=0.1_r8, greg_ce=0.2_r8

!parameter for prognostics mass flux calculation
    real(r8), parameter :: pmf_inc =3.e-10_r8, pmf_dec=2.e-10_r8


    real(r8), parameter :: adjdt = 100._r8

    integer, parameter :: kmaxref = 12
    real, dimension(kmaxref), parameter :: refcape = &
!        (/ 0., 4.42,  14.71,  50.10,  88.80, 155.23, 299.37, 482.90, 848.15, 1535.19, 3157.54, 5000. /)
        (/ 0., 4.42,  14.71,  50.10,  88.80, 155.23, 299.37, 682.90, 1248.15, 1935.19, 3557.54, 5200. /)
    real, dimension(kmaxref), parameter :: refz = &
        (/ 0., 0.90,   2.70,   4.50,   6.30,   8.10,   9.90,  11.70,  13.50,   15.30,   17.10, 50. /)

    real(r8), parameter :: evapke = 0.2e-5_r8

!    real(r8), parameter :: w_up_param = 1.5_r8
    real(r8), parameter :: w_up_param = 0.3_r8



!parameter for converting cloud water to rainfall
    real(r8) :: c0

    real(r8) :: zuplaunchtop  ! max cloud parcel launch height [m]
    real(r8) :: zuplaunchlow  ! min cloud parcel launch height [m]

    real(r8) :: capelmt

    real(r8) :: f_dcape
    real(r8) :: f_cape
    real(r8) :: f_w

    contains


subroutine temp( &
!input
        input, &
!output
        output, &
!in/out
        trig)

!------------------------------------------------------
!calculate the cloud levels
!------------------------------------------------------
!input
        real(r8), dimension(ncol, nlev), intent(in) :: input     ! [unit]
!output
        real(r8), dimension(ncol, nlev), intent(out) :: output   ! [unit]
!input/output
        integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local

        integer :: i,j,k

!intialize output
        output = 0._r8

        do i=1, ncol
            if ( trig(i) < 1 ) cycle
            do k=nlev, 1, -1
            end do
        end do

end subroutine temp



subroutine scp_conv_init(innlev)
!------------------------------------------------------
!do some initialization.
!------------------------------------------------------
!input
    integer, intent(in) :: innlev
!local


    nlev  = innlev
    nlevp = innlev+1
#ifdef SCMDIAG
    write(*,*) "[scp_conv_init]"
    write(*,*) "Parameter"
    write(*,"(a20f20.10)") "gravit", gravit
    write(*,"(a20f20.10)") "cpair", cpair
    write(*,"(a20f20.10)") "cpliq", cpliq
    write(*,"(a20f20.10)") "cpwv", cpwv
    write(*,"(a20f20.10)") "latvap", latvap
    write(*,"(a20f20.10)") "epsilo", epsilo
    write(*,"(a20f20.10)") "tmelt", tmelt
    write(*,"(a20f20.10)") "rair", rair
    write(*,"(a20f20.10)") "rh2o", rh2o
    write(*,"(a20f20.10)") "rhofw", rhofw
#endif

!setting parameter
    zuplaunchtop = 2000._r8 ! max cloud parcel launch height [m]
    zuplaunchlow = 100._r8  ! min cloud parcel launch height [m]

end subroutine scp_conv_init



subroutine scp_conv_tend( &
!input
        inncol, &
        in_ent_opt, nplume, dtime, &
        lat, ht, landfrac, lhflx, &
        psrf, p, dp, zsrf, z, dz, &
        t, q, bfls_t, bfls_q, &
        omega, pblh, tpert, &
!in/output
        massflxbase_p, &
!output
        stend, qtend, qliqtend, prec, qliq, rainrate, &
        compstend, compqtend, &
!diagnostics
        dilucape, bfls_dilucape, &
        outtmp2d, outtmp3d, &
        outmb, outmse, outmsesat, outmseup, &
        outstend, outqtend, &
        outcondstend, outcondqtend, &
        outtranupstend, outtranupqtend, &
        outtrandnstend, outtrandnqtend, &
        outevapstend, outevapqtend  )
!------------------------------------------------------
!Calculate convective tendency
!------------------------------------------------------

! Main Interface
    integer , intent(in) :: inncol ! size of column dimension

    integer , intent(in) :: in_ent_opt ! 0=ec, 1=greg
    integer , intent(in) :: nplume !
    real(r8), intent(in) :: dtime  ! [s] time step
    real(r8), dimension(inncol), intent(in) :: lat, ht, landfrac, lhflx
    real(r8), dimension(inncol), intent(in) :: psrf, zsrf
    real(r8), dimension(inncol, nlev), intent(in) :: p, dp, z, dz ! [Pa] ; [m]
    real(r8), dimension(inncol, nlev), intent(in) :: t, q ! [K] ; [kg/kg]
       ! T and Q state after the large-scale forcing is applied, current state
    real(r8), dimension(inncol, nlev), intent(in) :: bfls_t, bfls_q ! [K] ; [kg/kg]
       ! T and Q state before the large-scale forcing is applied
    real(r8), dimension(inncol, nlev), intent(in) :: omega ! [m/s]
    real(r8), dimension(inncol), intent(in) :: pblh ! [m/s]
    real(r8), dimension(inncol), intent(in) :: tpert ! [m/s]
!in/output
    real(r8), dimension(inncol), intent(inout) :: massflxbase_p !output convective precip[m/s]
!output
    real(r8), dimension(inncol), intent(out) :: prec !output convective precip[m/s]
    real(r8), dimension(inncol, nlev), intent(out) :: stend, qtend, qliqtend
    ! [K/s] ; [kg/kg/s] output tendencies calculated by adding (condensate rate+transport)
    real(r8), dimension(inncol, nlev), intent(out) :: qliq, rainrate ! [K/s] ; [kg/kg/s]

    real(r8), dimension(inncol, nlev), intent(out) :: compstend, compqtend
    ! [K/s] ; [kg/kg/s] tendencies but calculated using the compensating way
    ! should be same as stend and qtend

!diagnostic output
    real(r8), dimension(inncol), intent(out) :: outmb, outtmp2d
    real(r8), dimension(inncol, nlev), intent(out) :: outtmp3d
    real(r8), dimension(inncol, nlev), intent(out) :: outmse, outmsesat, outmseup

    real(r8), dimension(inncol, nlev), intent(out) :: outstend, outqtend
    real(r8), dimension(inncol, nlev), intent(out) :: outcondstend, outcondqtend
    real(r8), dimension(inncol, nlev), intent(out) :: outtranupstend, outtranupqtend
    real(r8), dimension(inncol, nlev), intent(out) :: outtrandnstend, outtrandnqtend
    real(r8), dimension(inncol, nlev), intent(out) :: outevapstend, outevapqtend


!local
    integer i, j, k, begk, endk
    real(r8), dimension(inncol, nlev) :: dse !environment [J/kg]
    real(r8), dimension(inncol, nlev) :: mse, msesat ! [J/kg] ; [J/kg]

    real(r8), dimension(inncol, nlev) :: esat, qsat ! [Pa] ; [kg/kg]
    real(r8), dimension(inncol, nlev) :: rh  ! [1] relative humidity
    real(r8), dimension(inncol, nlev) :: rho ! [kg/m3]

    integer, dimension(inncol) :: trigdp ! [1] 1 trigger ; 0 no trigger
    integer, dimension(inncol) :: trigsh ! [1] 1 trigger ; 0 no trigger

    integer, dimension(inncol) :: kuplaunch ! [1] cloud launching level
    integer, dimension(inncol) :: kuplcl    ! [1] cloud base
    integer, dimension(inncol) :: kupbase   ! [1] cloud base
    integer, dimension(inncol) :: kuptopmax ! [1] maximum cloud top, lift without entrainment

    integer, dimension(inncol) :: kuptop ! [1] cloud top where buoyancy is zero
    real(r8),dimension(inncol) :: zuptop ! [m] exact z at kuptop
    integer, dimension(inncol) :: knbtop ! [1] cloud top where overshooting or negative buoyance ends

    real(r8), dimension(inncol, nlev) :: convallmask_up ! [1] mask from launching point to cloud top


!for bulk TOTAL fractional en/detrainment rate depending on vertical velocity
    real(r8), dimension(inncol, nlevp) :: zint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: pint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: tint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: qint ! [1] height at the interface
!    real(r8), dimension(inncol, nlevp) :: esatint, qsatint ! [Pa] ; [kg/kg]
    real(r8), dimension(inncol, nlevp) :: normassflx_up ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlevp) :: normassflx_up_tmp ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlev)  :: ent_rate_bulk_up_mid ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlevp) :: ent_rate_bulk_up ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlevp) :: det_rate_bulk_up ! [1] solved PARCEL fractional entrainment rates

    real(r8), dimension(inncol, nlev) :: ent_fc ! [1] an entrainment rate modifier

    real(r8), dimension(inncol, nlevp) :: mse_up ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: dse_up ! [J/kg]  bulk in-cloud DSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: buoy ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: t_up   ! [K]  bulk in-cloud temperatur given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: tv_up  ! [K]  bulk in-cloud temperatur given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: q_up   ! [kg/kg]  bulk in-cloud sat water vapor given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: qliq_up   ! [kg/kg]  bulk in-cloud liquid water given en/detrainment rate.

    real(r8), dimension(inncol, nlev) :: mse_up_mid ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlev) :: t_up_mid ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlev) :: q_up_mid ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlev) :: buoy_mid ! [m/s-2] bulk in-cloud buoyancy
    real(r8), dimension(inncol, nlev) :: normassflx_up_mid ! [1]  bulk normalized updraft mass flux

    real(r8), dimension(inncol, nlev) :: condrate_up   ! [kg/kg]  condensation rate.
    real(r8), dimension(inncol, nlev) :: rainrate_up   ! [1]  bulk precipitation production

    real(r8), dimension(inncol, nlev) :: normassflx_dn ! [1]  bulk normalized updraft mass flux

!for evaporation
    real(r8), dimension(inncol, nlev) :: evaprate  ! [1]  bulk evaporation production
    real(r8), dimension(inncol) :: evaprateflx     ! [1]  bulk evaporation production
    real(r8), dimension(inncol) :: netprec         ! [1]  bulk evaporation production
    real(r8) :: evaplimit ! [1]  bulk evaporation limit for special case

!for EC en/detrainment rate depending on RH
    real(r8), dimension(inncol, nlev) :: fscale_up ! [1]
!calculated w from Grell's en/detrainment rate scheme
    real(r8), dimension(inncol) :: w_up_init  ! [J/kg]  bulk in-cloud vertical velocity
    real(r8), dimension(inncol, nlev) :: w_up_mid ! [J/kg]  bulk in-cloud vertical velocity
    real(r8), dimension(inncol, nlevp) :: w_up ! [J/kg]  bulk in-cloud vertical velocity


! These varaiables are used to calcualte the closure parameter F
! (cloud work function change after applying tendencies with unit base mass flux)
    integer, dimension(inncol) :: trig_closure
    real(r8),dimension(inncol, nlev) :: t_closure  ! [K] adjusted temperature for closure use
                                                   ! by unit base mass flux
    real(r8),dimension(inncol, nlev) :: q_closure  ! [kg/kg] adjusted moisture for closure use
                                                   ! by unit base mass flux
    real(r8),dimension(inncol, nlev) :: rh_closure   ! [K] adjusted temperature for closure use
    real(r8),dimension(inncol, nlev) :: qsat_closure ! [K] adjusted temperature for closure use
    real(r8),dimension(inncol, nlev) :: mse_closure
    real(r8),dimension(inncol, nlev) :: msesat_closure

    integer, dimension(inncol) :: kuplaunch_closure ! [1] cloud launching level
    integer, dimension(inncol) :: kuplcl_closure    ! [1] cloud base
    integer, dimension(inncol) :: kupbase_closure   ! [1] cloud base
    integer, dimension(inncol) :: kuptop_closure ! [1] cloud top where buoyancy is zero
    real(r8),dimension(inncol) :: zuptop_closure ! [m] exact kuptop
    integer, dimension(inncol) :: kuptopmax_closure ! [1] cloud top where buoyancy is zero

    real(r8),dimension(inncol, nlev) :: mse_up_closure
    real(r8),dimension(inncol, nlev) :: t_up_closure
    real(r8),dimension(inncol, nlev) :: q_up_closure
    real(r8),dimension(inncol, nlev) :: tv_up_closure
    real(r8),dimension(inncol, nlev) :: normassflx_up_closure

    real(r8),dimension(inncol, nlev) :: fscale_up_closure ! [1]
    real(r8),dimension(inncol, nlev) :: ent_rate_bulk_up_closure
    real(r8),dimension(inncol, nlev) :: det_rate_bulk_up_closure

    real(r8),dimension(inncol, nlev) :: tv_closure ! [K]
    real(r8),dimension(inncol, nlev) :: buoy_closure  ! [kg/kg] adjusted buoyancy for closure use
    real(r8),dimension(inncol, nlev) :: w_up_closure ! [J/kg]  bulk in-cloud vertical velocity


! These variables are for closure calculation, OR base mass flux
    real(r8),dimension(inncol, nlev) :: w         ! [m/s] environment vertical velocity
    real(r8),dimension(inncol) :: mconv           ! [1] moisture convergence
    real(r8),dimension(inncol) :: conv            ! [1] wind convergence
    real(r8),dimension(inncol) :: dilucape        ! [1] CAPE cloud work function
    real(r8),dimension(inncol) :: bfls_dilucape   ! [1] CAPE cloud work function before LS forcing
    real(r8),dimension(inncol) :: dilucape_closure

    real(r8),dimension(inncol) :: capeclm
    integer,dimension(inncol)  :: kclm

    real(r8),dimension(inncol) :: massflxbase       ! [1]
    real(r8),dimension(inncol) :: massflxbase_cape  ! [1]
    real(r8),dimension(inncol) :: massflxbase_clm   ! [1]
    real(r8),dimension(inncol) :: massflxbase_clmcape  ! [1]
    real(r8),dimension(inncol) :: massflxbase_w     ! [1]
    real(r8),dimension(inncol) :: massflxbase_mconv ! [1]
    real(r8),dimension(inncol) :: massflxbase_conv  ! [1]
    real(r8),dimension(inncol) :: massflxbase_dcape ! [1]
    real(r8),dimension(inncol) :: capefc
    real(r8),dimension(inncol) :: convfrc_up ! [1] conv fraction, for scale-aware, not used yet
    real(r8),dimension(inncol) :: lat_coef

! These variables are used to prevent negative water vapor
    real(r8), dimension(inncol, nlev) :: qcheck ! [1]  used to check negative q
    real(r8), dimension(inncol) :: qcheckout    ! [1]  used to check negative q
    real(r8) :: qcheckf, minqcheckf             ! [1]

    real(r8), dimension(inncol) :: tmp2d    ! [1]  used to check negative q


!for diag
    real(r8), dimension(inncol, nlev) :: diffdse_up ! [1]  Delta DSE
    real(r8), dimension(inncol, nlev) :: diffq_up   ! [1]  Delta Q
    real(r8), dimension(inncol, nlev) :: condstend  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: condqtend  ! [K/s] Q tendency
    real(r8), dimension(inncol, nlev) :: transtend_up  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: tranqtend_up  ! [K/s] Q tendency
    real(r8), dimension(inncol, nlev) :: evapstend  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: evapqtend  ! [K/s] Q tendency

    real(r8), dimension(inncol, nlev) :: condrate  ! [K/s] Total condensation rate

    real(r8), dimension(inncol, nlev) :: tmp1stend, tmp1qtend
    real(r8), dimension(inncol, nlev) :: tmp2stend, tmp2qtend

!for test
    real(r8), dimension(inncol) :: tmp ! [1] number of convective lev
    real(r8) :: diffz
    logical :: flag

!setting the internal dimension size same as the input
    ncol = inncol

    ent_opt = in_ent_opt

!intialize output
    massflxbase_p = 0._r8
    prec = 0._r8
    stend = 0._r8
    qtend = 0._r8
    qliqtend = 0._r8
    qliq  = 0._r8
    rainrate = 0._r8

    !outmb = 0._r8
    !outtmp2d = 0._r8
    !outtmp3d = 0._r8
    !outmse = 0._r8
    !outmsesat = 0._r8
    !outmseup = 0._r8
    !outstend = 0._r8
    !outqtend = 0._r8
    !outcondstend = 0._r8
    !outcondqtend = 0._r8
    !outtranupstend = 0._r8
    !outtranupqtend = 0._r8
    !outtrandnstend = 0._r8
    !outtrandnqtend = 0._r8
    !outevapstend = 0._r8
    !outevapqtend = 0._r8


!zero local variables
    mse = 0._r8
    dse = 0._r8

    qliq_up = 0._r8
    condrate = 0._r8
    rainrate_up = 0._r8
    condrate_up = 0._r8

    evaprate = 0._r8
    evaprateflx = 0._r8
    netprec = 0._r8

    w = 0._r8
    tv_closure = 0._r8
    q_closure = 0._r8
    buoy_closure = 0._r8

    dilucape = 0._r8
    bfls_dilucape = 0._r8
    dilucape_closure = 0._r8

    diffdse_up = 0._r8
    diffq_up = 0._r8
    condstend = 0._r8
    condqtend = 0._r8
    transtend_up = 0._r8
    tranqtend_up = 0._r8
    evapstend = 0._r8
    evapqtend = 0._r8
    compstend  = 0._r8
    compqtend  = 0._r8

    knbtop = nlev

    massflxbase = 0._r8
    massflxbase_w = 0._r8
    massflxbase_conv  = 0._r8
    massflxbase_mconv = 0._r8
    massflxbase_cape = 0._r8
    massflxbase_clm  = 0._r8
    massflxbase_dcape = 0._r8


!init some local variables
    trigdp = 1
    trigsh = 1
!setting parameter
!   ent_rate_bulk_up_dp = 0.0001916885_r8

!Calculation begins
#ifdef SCMDIAG
    write(*,*) "[scp_conv_tend]"
    call subcol_netcdf_nextstep
#endif


!!------------------------------------------------------
!!------------------------------------------------------
!!------------------------------------------------------
!!STEP ONE: calculate cloud work
!!          in the last step after convection before 
!!------------------------------------------------------
!!------------------------------------------------------
!!------------------------------------------------------

!!------------------------------------------------------
!!Calculate basic properties.
!!DSE:dry static energy, MSE: moist static energy
!!saturated water vapor pressure
!!saturated water vapor mixing ratio
!!saturated water vapor moist static energy
!!------------------------------------------------------
    !dse = cpair*bfls_t+gravit*z
    !mse = cpair*bfls_t+gravit*z+latvap*bfls_q
!!two forms of formula for saturated water vapor pressure
    !esat = 611.2*exp(5417*(1/273.16-1/bfls_t))
!!   esat = 611.2*exp(17.67*(t-273.16)/(t-273.16+243.5))
    !qsat = 0.622*esat/p
    !msesat = cpair*bfls_t+gravit*z+latvap*qsat
    !rh  = bfls_q/qsat
    !rho = p/bfls_t/rair

!!------------------------------------------------------
!!Different methods to calculate entrainment rate
!!------------------------------------------------------
    !fscale_up = 0._r8
    !convallmask_up = 0._r8

    !mse_up = 0._r8
    !t_up = 0._r8
    !tv_up = 0._r8
    !q_up = 0._r8
    !w_up = 0._r8
    !buoy = 0._r8
    !normassflx_up = 0._r8
    !ent_rate_bulk_up = 0._r8
    !det_rate_bulk_up = 0._r8

    !normassflx_dn = 0._r8

    !call cal_launchtolcl( z, p, t, q, mse, landfrac, lhflx, tpert, &
        !kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up, trigdp)

    !do i=1, inncol
        !convallmask_up(i,kuplaunch(i):1:-1) = 1._r8
    !end do

    !ent_fc = 0._r8

    !do i=1, inncol
        !do k = kuplaunch(i), 1, -1
!!        ent_fc(:,k) = 1/sqrt(2*10.**2)*exp()
!!        ent_fc(:,k) = 1.+0.01*z(:,k)**2
!!        ent_fc(i,k) = z(i,k)*0.00000001_r8
        !end do
    !end do

!!------------------------------------------------------
!! *Entrainment rate scheme #1 depending on RH from ECMWF
!! =>ent_rate_bulk_up
!!------------------------------------------------------
    !if( ent_opt == 0 ) then
        !call cal_endet_ec( &
            !qsat, rh, kuplaunch, kuplcl, &
            !ent_rate_bulk_up, det_rate_bulk_up, &
            !trigdp)
    !end if
    !!ent_rate_bulk_up = 0._r8
    !!det_rate_bulk_up = 0._r8

!!------------------------------------------------------
!! Calculate Updraft Cloud Base
!! * Launch level "kuplaunch"
!! betweeen zuplaunchlow<=kuplaunch<=zuplaunchtop with maximum MSE
!! * Cloud Base "kupbase" is where parcel MSE overcome saturated MSE
!! * "kuptopmax" is highest possible cloud top (assume no entrainment)
!!------------------------------------------------------
    !call cal_uplcl( z, p, t, q, &
        !mse, msesat, ent_rate_bulk_subcld_up, &
        !kuplaunch, kuplcl, &
        !kupbase, kuptopmax, mse_up, t_up, q_up, normassflx_up, &
        !trigdp )
    !trigsh = trigdp

!!------------------------------------------------------
!! Given a entrainment rate, Caculate Updraft In-coud MSE
!! and also the actual cloud top (with entrainment)
!!------------------------------------------------------
    !if ( ent_opt == 1 ) then 
        !w_up_init = w_up_param
    !end if

    !call cal_mse_up( &
        !ent_opt, z, p, t, q, qsat, mse, msesat, &
        !kuplaunch, kupbase, &
        !ent_rate_bulk_up, det_rate_bulk_up, w_up_init, &
        !mse_up, t_up, q_up, normassflx_up, buoy, w_up, kuptop, zuptop, &
        !trigdp)


!!get CAPE in the last field
    !call cal_cape( &
        !dz, buoy, kupbase, kuptop, &
        !bfls_dilucape, &
        !trigdp)


    trigdp = 1
    trigsh = 1
!------------------------------------------------------
!------------------------------------------------------
!------------------------------------------------------
!STEP TWO: calculate cape in the current step
!------------------------------------------------------
!------------------------------------------------------
!------------------------------------------------------

!------------------------------------------------------
!Calculate basic properties
!dse:dry static energy, mse: moist static energy
!esat:saturated water vapor pressure
!qsat:saturated water vapor mixing ratio
!msesat:saturated water vapor moist static energy
!------------------------------------------------------
    dse = cpair*t+gravit*z
    mse = cpair*t+gravit*z+latvap*q
!two forms of formula for saturated water vapor pressure
    esat = 611.2*exp(5417*(1/273.16-1/t))
!    esat = 611.2*exp(17.67*(t-273.16)/(t-273.16+243.5))
    qsat = 0.622*esat/p
    msesat = cpair*t+gravit*z+latvap*qsat
    rh  = q/qsat
    rho = p/t/rair
    w = -omega/rho/gravit

!estimate z at the interface from z and dz
    zint(:,nlevp) = ht(:)
    do k=nlev,1,-1
        zint(:,k) = zint(:,k+1)+dz(:,k)
    end do

    pint(:,nlevp) = psrf(:)
    do k=nlev,1,-1
        pint(:,k) = pint(:,k+1)-dp(:,k)
    end do

!estimate t at the interface from z and dz
    tint(:,nlevp) = t(:,nlev)
    tint(:,1) = t(:,1)
    do k=2,nlev
        tint(:,k) = 0.5*( t(:,k)+t(:,k-1) )
    end do

    qint(:,nlevp) = q(:,nlev)
    qint(:,1) = q(:,1)
    do k=2,nlev
        qint(:,k) = 0.5*( q(:,k)+q(:,k-1) )
    end do

    !esatint = 611.2*exp(5417*(1/273.16-1/tint))
    !qsatint = 0.622*esatint/pint

!------------------------------------------------------
!Different methods to calculate entrainment rate
!------------------------------------------------------
    fscale_up = 0._r8
    convallmask_up = 0._r8

    mse_up = 0._r8
    t_up = 0._r8
    tv_up = 0._r8
    q_up = 0._r8
    w_up = 0._r8
    w_up_mid = 0._r8
    buoy = 0._r8
    buoy_mid = 0._r8
    normassflx_up = 0._r8
    ent_rate_bulk_up_mid = 0._r8
    ent_rate_bulk_up = 0._r8
    det_rate_bulk_up = 0._r8
    mse_up_mid = 0._r8
    t_up_mid = 0._r8
    q_up_mid = 0._r8

    normassflx_dn = 0._r8

    call cal_launchtolcl( z, zint, p, t, q, qsat, mse, msesat, landfrac, lhflx, tpert, &
        kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up, trigdp)

    !do i=1, inncol
        !convallmask_up(i,kuplaunch(i):1:-1) = 1._r8
    !end do
!!------------------------------------------------------
!! *METHOD ONE: Constant parcel fractional entrainment rate to reach given levels
!! =>ent_rate_up
!! Caculate Updraft fractional Entrainment Rates
!! *"up_ent_rate" is how much entrainment rates you need
!! to reach differen levels.
!!------------------------------------------------------
    !!call solve_ent_rate_up( &
        !!z, mse, msesat, &
        !!kuplaunch, kupbase, kuptopmax, &
        !!ent_rate_up, &
        !!trigdp)

!!------------------------------------------------------
!! *Entrainment rate scheme #1 depending on RH from ECMWF
!! =>ent_rate_bulk_up
!!------------------------------------------------------

    !if( ent_opt == 0 ) then
        !call cal_endet_ec( &
            !qsat, rh, kuplaunch, kuplcl, &
            !ent_rate_bulk_up, det_rate_bulk_up, &
            !trigdp)
    !end if
    !!ent_rate_bulk_up = 0._r8
    !!det_rate_bulk_up = 0._r8

    !ent_rate_bulk_subcld_up = ent_rate_bulk_up
!!------------------------------------------------------
!! Calculate Updraft Cloud Base First
!! * Launch level "kuplaunch"
!! betweeen zuplaunchlow<=kuplaunch<=zuplaunchtop with maximum MSE
!! * Cloud Base "kupbase" is where parcel MSE overcome saturated MSE
!! * "kuptopmax" is highest possible cloud top (assume no entrainment)
!!------------------------------------------------------
    !call cal_uplcl( z, p, t, q, &
        !mse, msesat, ent_rate_bulk_subcld_up, &
        !kuplaunch, kuplcl, &
        !kupbase, kuptopmax, mse_up, t_up, q_up, normassflx_up, &
        !trigdp )
    !trigsh = trigdp

!!------------------------------------------------------
!! Given a entrainment rate, Caculate Updraft In-coud MSE
!! and also the actual cloud top (with entrainment)
!!------------------------------------------------------
!#ifdef SCMDIAG 
    !if ( ent_opt == 1 ) then 
    !do i = 1, nplume
        !w_up_init = i*0.2
        !call cal_mse_up( &
            !ent_opt, z, p, t, q, qsat, mse, msesat, &
            !kuplaunch, kupbase, &
            !ent_rate_bulk_up, det_rate_bulk_up, w_up_init, &
            !mse_up, t_up, q_up, normassflx_up, buoy, w_up, kuptop, zuptop, &
            !trigdp)
!!        call stdout3d( mse, msesat, mse_up, normassflx_up, buoy, w_up)
        !call subcol_netcdf_putclm( "mse_up_plume", mse_up(1,:), i )
        !if ( (kupbase(1)-1) >= 1 )  mse_up(1, 1:kupbase(1)-1) = 0._r8
    !end do
    !end if
!#endif


    !do j = 1, nplume
        !ent_rate_bulk_up = 0._r8
        !det_rate_bulk_up = 0._r8
        !normassflx_up_tmp = normassflx_up

        !w_up_init = j*0.2
        !call cal_mse_up( &
            !ent_opt, z, zint, dz, p, pint, t, q, qsat, mse, msesat, &
            !kuplaunch, kuplcl, &
            !ent_rate_bulk_up, det_rate_bulk_up, w_up_init, &
            !mse_up, t_up, q_up, normassflx_up_tmp, w_up, w_up_mid, buoy_mid, kuptop, zuptop, &
            !trigdp)

        !mse_up_mid = 0._r8
        !t_up_mid = 0._r8
        !q_up_mid = 0._r8
        !normassflx_up_mid = 0._r8

        !do i=1, inncol
            !do k=nlev, 1, -1
                !if ( (mse_up(i,k)>0._r8) .and. (mse_up(i,k+1)>0._r8) ) then
                    !mse_up_mid(i,k) = 0.5*( mse_up(i,k) + mse_up(i,k+1) )
                    !t_up_mid(i,k) = 0.5*( t_up(i,k) + t_up(i,k+1) )
                    !q_up_mid(i,k) = 0.5*( q_up(i,k) + q_up(i,k+1) )
                    !normassflx_up_mid(i,k) = 0.5*( normassflx_up_tmp(i,k) + normassflx_up_tmp(i,k+1) )
                !end if
            !end do
        !end do

        !call subcol_netcdf_putclm( "ent_rate", ent_rate_bulk_up(1,:), j )
        !call subcol_netcdf_putclm( "w_up", w_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "buoy", buoy_mid(1,:), j )
        !call subcol_netcdf_putclm( "mse_up", mse_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "t_up", t_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "q_up", q_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "normassflx_up", normassflx_up_mid(1,:), j )

    !end do

    w_up_init = w_up_param
    call cal_mse_up( &
        ent_opt, z, zint, dz, p, pint, t, tint, q, qint, qsat, mse, msesat, &
        kuplaunch, kuplcl, &
        ent_rate_bulk_up, det_rate_bulk_up, w_up_init, &
        mse_up, t_up, q_up, normassflx_up, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
        trigdp)
    do i=1, inncol
        do k=nlev, 1, -1
            if ( (mse_up(i,k)>0._r8) .and. (mse_up(i,k+1)>0._r8) ) then
                mse_up_mid(i,k) = 0.5*( mse_up(i,k) + mse_up(i,k+1) )
                t_up_mid(i,k) = 0.5*( t_up(i,k) + t_up(i,k+1) )
                q_up_mid(i,k) = 0.5*( q_up(i,k) + q_up(i,k+1) )
                normassflx_up_mid(i,k) = 0.5*( normassflx_up(i,k) + normassflx_up(i,k+1) )
            end if
        end do
    end do

!!------------------------------------------------------
!!------------------------------------------------------
!!------------------------------------------------------
!!STEP THREE
!!------------------------------------------------------
!!------------------------------------------------------
!!------------------------------------------------------
    !do i=1, inncol
        !convallmask_up(i, kuptop(i)-1:1:-1 ) = 0._r8
    !end do

    !call cal_cape( &
        !dz, buoy, kupbase, kuptop, &
        !dilucape, &
        !trigdp)

    !do i=1, inncol
        !if( dilucape(i) <= 0 ) trigdp(i) = -4
        !if( ent_opt == 0 ) then
!!            c0 = 2.e-3_r8 ! no dependence on cape consumption, old parameter
!!            c0 = 2.e-5_r8 ! no dependence on cape consumption
!!            c0 = 3.e-3_r8 ! dependence on cape consumption single adjustment, old parameter
!!            c0 = 10.e-4_r8 ! dependence on cape consumption single adjustment

!!            c0 = 3.e-4_r8  ! dependence on cape consumption multi adjustment

!!this version works well.
!!old version enscntr, before condensate is fixed.
!!            c0 = 4.e-4_r8  ! dependence on cape consumption multi adjustment
!!
            !c0 = 3.e-4_r8  ! dependence on cape consumption multi adjustment

        !else
!!            c0 = 3.e-4_r8  ! dependence on cape consumption multi adjustment

            !c0 = 4.e-4_r8  ! dependence on cape consumption multi adjustment
        !end if

    !end do

!!------------------------------------------------------
!! Normalized Moisture Calculation
!!------------------------------------------------------
    !call cal_moisture_up( &
        !z, kuplaunch, kuplcl, kupbase, kuptop, &
        !rho, q, qsat, q_up, &
        !ent_rate_bulk_subcld_up, ent_rate_bulk_up, det_rate_bulk_up, &
        !normassflx_up, landfrac, &
        !qliq_up, condrate_up, rainrate_up, &
        !trigdp)

    !dse_up = (cpair*t_up+gravit*z)*convallmask_up

!!------------------------------------------------------
!! Normalized Transport Calculation
!!------------------------------------------------------
    !call cal_tendtransport( &
        !z, kuplaunch, kuplcl, kupbase, kuptop, &
        !rho, dse, q, dse_up, q_up, &
        !normassflx_up,  &
        !transtend_up, tranqtend_up, &
        !trigdp)
    !!condstend = latvap*condrate_up/rho
    !!condqtend = -condrate_up/rho
    !condstend = latvap*condrate_up
    !condqtend = -condrate_up




!!------------------------------------------------------
!!apply the cloud tendencies using unit base mass flux
!!assuming environment change by 1s time
!!and calculate cloud work function again
!!the result is f: cloud work funcion change per unit base mass flux
!!this needs to rerun all the calculations again!!
!!add _closure naming for difference
!!------------------------------------------------------
    !fscale_up = 0._r8
    !convallmask_up = 0._r8

    !trig_closure = 1
    !t_closure = 0._r8
    !q_closure = 0._r8
    !mse_closure = 0._r8
    !tv_closure = 0._r8

    !mse_up_closure = 0._r8
    !t_up_closure = 0._r8
    !tv_up_closure = 0._r8
    !q_up_closure = 0._r8
    !buoy_closure = 0._r8
    !w_up_closure = 0._r8
    !normassflx_up_closure = 0._r8
    !ent_rate_bulk_up_closure = 0._r8
    !det_rate_bulk_up_closure = 0._r8
    !ent_rate_bulk_subcld_up = 0._r8

    !dilucape_closure = 0._r8

    !q_closure = q + adjdt*( condqtend+tranqtend_up )
    !t_closure = t + adjdt*( condstend+transtend_up )/cpair
    !tv_closure = t_closure*( 1+0.61*q_closure )

!!------------------------------------------------------
!! TWO choices
!!------------------------------------------------------

!!--------------------------------------------------------------------------------------------------
!!Choice #1 Use full calculation
!!--------------------------------------------------------------------------------------------------
    !mse_closure = cpair*t_closure+gravit*z+latvap*q_closure
    !qsat_closure = 0.622*611.2*exp(5417*(1/273.16-1/t_closure))/p
    !msesat_closure = cpair*t_closure+gravit*z+latvap*qsat_closure
    !rh_closure  = q_closure/qsat_closure

    !call cal_launchtolcl( z, p, t_closure, q_closure, mse_closure, landfrac, lhflx, tpert, &
        !kuplaunch_closure, kuplcl_closure, mse_up_closure, t_up_closure, q_up_closure, &
        !normassflx_up_closure, trig_closure)

    !do i=1, inncol
        !convallmask_up(i,kuplaunch_closure(i):1:-1) = 1._r8
    !end do

!!------------------------------------------------------
!! *Entrainment rate scheme #1 depending on RH from ECMWF
!! ent_optr= 0
!!------------------------------------------------------
    !if( ent_opt == 0 ) then
        !call cal_endet_ec( &
            !qsat_closure, rh_closure, kuplaunch_closure, kuplcl_closure, &
            !ent_rate_bulk_up_closure, det_rate_bulk_up_closure, &
            !trigdp)
    !end if
    !!ent_rate_bulk_up = 0._r8
    !!det_rate_bulk_up = 0._r8

    !ent_rate_bulk_subcld_up = ent_rate_bulk_up
    !call cal_uplcl( z, p, t_closure, q_closure, &
        !mse_closure, msesat_closure, ent_rate_bulk_subcld_up, &
        !kuplaunch_closure, kuplcl_closure, &
        !kupbase_closure, kuptopmax_closure, &
        !mse_up_closure, t_up_closure, q_up_closure, normassflx_up_closure, &
        !trig_closure )

!!------------------------------------------------------
!! Bulk Cloud Model
!!------------------------------------------------------
    !if ( ent_opt == 1 ) then 
        !w_up_init = w_up_param
    !end if

    !call cal_mse_up( &
        !ent_opt, z, p, t_closure, q_closure, qsat_closure, mse_closure, msesat_closure, &
        !kuplaunch_closure, kupbase_closure, &
        !ent_rate_bulk_up_closure, det_rate_bulk_up_closure, w_up_init, &
        !mse_up_closure, t_up_closure, q_up_closure, normassflx_up_closure, &
        !buoy_closure, w_up_closure, kuptop_closure, zuptop_closure, &
        !trigdp)

    !do i=1, inncol
        !convallmask_up(i, kuptop_closure(i)-1:1:-1 ) = 0._r8
    !end do

    !tv_up_closure = t_up_closure*( 1+0.61*q_up_closure )
    !do i=1, inncol
        !do k=kupbase_closure(i), kuptop_closure(i), -1
            !buoy_closure(i,k) = gravit*(tv_up_closure(i,k)-tv_closure(i,k))/tv_closure(i,k)
        !end do
    !end do
    !call cal_cape( &
        !dz, buoy_closure, kupbase_closure, kuptop_closure, &
        !dilucape_closure, &
        !trigdp)
!!--------------------------------------------------------------------------------------------------
!! End of Choice #1
!!--------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------
!! Choice #2
!! Use simplification, assuming parcel profile not change too much
!! after applying cloud tendencies
!! save a lot of computing, no need to RERUN the calculation
!!--------------------------------------------------------------------------------------------------
    !!tv_up_closure = t_up*( 1+0.61*q_up )
    !!do i=1, inncol
        !!do k=kupbase(i), kuptop(i), -1
            !!buoy_closure(i,k) = gravit*(tv_up_closure(i,k)-tv_closure(i,k))/tv_closure(i,k)
        !!end do
    !!end do
    !!call cal_cape( &
        !!dz, buoy_closure, kupbase, kuptop, &
        !!dilucape_closure, &
        !!trigdp)
!!--------------------------------------------------------------------------------------------------
!! End of Choice #2
!!--------------------------------------------------------------------------------------------------


!!------------------------------------------------------
!! Closure Calculation
!!------------------------------------------------------

    !convfrc_up = 0.02_r8
!!    capelmt = 40._r8
    !capelmt = 20._r8

    !capefc = 0.01_r8
    !capeclm = 0._r8
    !kclm = kmaxref

!!    lat_coef = 0.8+0.2/20/20*abs( lat*lat )
    !lat_coef = 0._r8
    !!do i=1, inncol
        !!if ( abs(lat(i)) <= 30./180*3.14159 ) then
            !!lat_coef(i) = 1._r8
        !!end if
    !!end do
    
!! Closure of CAPE
!! Closure of DCAPE
!!------------------------------------------------------
    !do i=1, inncol
!!            f_dcape = 5.0_r8

!!            f_cape  = 1.0_r8 ! no dependence on cape consumption, old parameter
!!            f_cape  = 3.0_r8 ! no dependence on cape consumption
!!            f_cape = dilucape(i)-dilucape_closure(i) ! dependence on cape consumption
        !f_cape = min(max( (dilucape(i)-dilucape_closure(i))/adjdt,0.1_r8),300._r8)
        !capefc(i) = f_cape

        !do k=2, kmaxref
            !if( refz(k)*1000.>=zuptop(i) ) then
                !kclm(i) = k-1
                !exit
            !end if
        !end do

        !capeclm(i) = refcape(kclm(i))
        !!if ( zuptop(i)>refz(kmaxref)*1000. ) then
            !!capeclm(i) = refz(kmaxref)
        !!else
            !capeclm(i) = refcape(kclm(i))+(zuptop(i)-refz(kclm(i))*1000.)*&
                !(refcape(kclm(i)+1)-refcape(kclm(i)))/(refz(kclm(i)+1)-refz(kclm(i)))/1000.
        !!end if

        !if( ent_opt == 0 ) then
!!                massflxbase_cape  = f_cape*convfrc_up*dilucape/dtime !for ec
!!                massflxbase_cape(i)  = 0.02*dilucape(i)/dtime/f_cape ! for ec single adjustment, old parameter
!!                massflxbase_cape(i)  = 0.01*dilucape(i)/dtime/f_cape ! for ec single adjustment
!!            massflxbase_cape(i)  = 0.08*max( (dilucape(i)-capelmt), 0._r8 )/dtime ! for ec single adjustment

!!this version works well.
!!            massflxbase_cape(i)  = 2*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
!!enscntr old with unfixed condensate
!!            massflxbase_cape(i)  = 0.1*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
!!new tuned
!!curtest2
            !massflxbase_cape(i)  = 0.05*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
!!for adding entrainment.
!!            massflxbase_cape(i)  = 0.02*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
!!curtest4
!!            massflxbase_cape(i)  = 0.07*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment

            !!if ( landfrac(i)>0.9 ) then
                !!massflxbase_cape(i)  = 0.2*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
            !!else
                !!massflxbase_cape(i)  = 0.1*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
            !!end if

!!add a little bit base mass flux for CAPE for test.
!!            massflxbase_cape(i)  = 0.12*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment

!!            massflxbase_cape(i) = 0.1*max( (dilucape(i) - (capelmt+1.e-4*(dilucape(i)**2) ) ), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
!!            massflxbase_cape(i) = 0.1*max( (dilucape(i) - (capelmt+6.e-6*(dilucape(i)**2) ) ), 0._r8 )/dtime/f_cape ! for ec multi level adjustment
!!            massflxbase_cape(i) = 0.1*max( -20+0.0008*(dilucape(i)+100)**2, 0._r8 )/dtime/f_cape ! for ec multi level adjustment

            !massflxbase_dcape(i) = 0.018*max(dilucape(i)-bfls_dilucape(i), 0._r8)/dtime/f_cape
        !else
!!            massflxbase_cape  = f_cape*convfrc_up*dilucape/dtime !for greg
            !massflxbase_cape(i)  = 0.05*max( (dilucape(i)-capelmt), 0._r8 )/dtime/f_cape ! for ec multi level adjustment

            !massflxbase_dcape = f_dcape*convfrc_up*( dilucape-bfls_dilucape )/dtime

        !end if

    !end do

!!    massflxbase_cape = massflxbase_cape*lat_coef

!! Closure of Moisture Convergence
!!------------------------------------------------------
    !mconv = 0._r8
    !do i=1, inncol
        !begk = nlev-1
!!        endk = kupbase(1)
        !endk = 1
        !do k = begk, endk, -1
            !mconv(i) = mconv(i)+ max( omega(i,k)*( q(i,k)-q(i,k+1) )/gravit, 0._r8)
        !end do
        !mconv(i) = mconv(i)/(begk-endk+1)
        !massflxbase_mconv(i) = 4000*mconv(i)
        !massflxbase_mconv(i) = max( 0._r8, massflxbase_mconv(i) )
    !end do

!! Closure of Wind Convergence
!!------------------------------------------------------
    !conv = 0._r8
    !do i=1, inncol
        !do k = nlev, 1, -1
            !if (z(i,k)>pblh(i) ) then
                !conv(i) = w(i,k)
                !exit
            !end if
        !end do
!!        conv(i) = w(i,endk)
!!        massflxbase_conv(i) = max( 0._r8, massflxbase_conv(i) )
        !massflxbase_conv(i) = conv(i)
    !end do

!! Closure of Vertical Velocity
!!------------------------------------------------------
    !f_w = 20._r8
    !do i=1, inncol
        !do k=nlev, kupbase(i), -1
            !massflxbase_w(i) = massflxbase_w(i) + max(0._r8, w(i,k) )
        !end do
        !massflxbase_w(i) = f_w*convfrc_up(i)*massflxbase_w(i)
    !end do

!! Closure of P
!!------------------------------------------------------
!!    do i=1, inncol
        !!if( dilucape(i)>500 ) then
            !!pmassflxbase(i) = massflxbase(i)
        !!else
            !!pmassflxbase(i) = pmassflxbase(i) + dtime*( -pmf_dec*pmassflxbase(i) )
!!!            pmassflxbase(i) = pmassflxbase(i) + dtime*( -pmf_dec*( massflxbase(i)-pmassflxbase(i) ) )
        !!end if
        !!if( pmassflxbase(i)<0 ) then
            !!pmassflxbase(i) = 0._r8
        !!end if
!!        pmassflxbase(i) = pmassflxbase(i) + dtime*( pmf_inc*dilucape(i)-pmf_dec*pmassflxbase(i) )
!!    end do


!!------------------------------------------------------
!! Triggerring Condition
!!------------------------------------------------------
    !call triggercond( z, rh, landfrac, conv, dilucape, bfls_dilucape, &
        !kuplaunch, kupbase, &
        !trigdp)


!!------------------------------------------------------
!! Closure Ensemble Choice
!!------------------------------------------------------
    !do i=1, inncol
        !if ( trigdp(i)<1 ) cycle
!!    massflxbase = 0.5*(massflxbase+massflxbase_w)
!!    massflxbase = 0.5*(massflxbase_dcape+massflxbase)
!!    massflxbase = 0.5*(massflxbase_cape+massflxbase_dcape)
!!    massflxbase = (massflxbase_cape+massflxbase_dcape+massflxbase_w)/3.
!!    massflxbase = (massflxbase_cape+massflxbase_dcape+massflxbase_w+massflxbase_mconv)/4.
!!    massflxbase = massflxbase_cape
!!    massflxbase = (massflxbase_cape+massflxbase_mconv)/2.
!!    massflxbase = massflxbase_mconv
!!    massflxbase = massflxbase_dcape
!!    massflxbase = massflxbase_w
!!        massflxbase(i) = massflxbase_cape(i)
!!        massflxbase(i) = massflxbase_clm(i)
!!        massflxbase(i) = massflxbase_dcape(i)
!!        massflxbase(i) = massflxbase_mconv(i)
!!        massflxbase(i) = massflxbase_w(i)
!!control version
!!enscntr
       !massflxbase(i) = 0.6*massflxbase_cape(i)+0.1*massflxbase_mconv(i)+0.3*massflxbase_w(i)
!!        massflxbase(i) = massflxbase_cape(i)
!!        massflxbase(i) = massflxbase_mconv(i)
!!        massflxbase(i) = massflxbase_w(i)

!!        massflxbase(i) = 0.6*massflxbase_dcape(i)+0.1*massflxbase_mconv(i)+0.3*massflxbase_w(i)

!!        massflxbase(i) = max( max( massflxbase_cape(i),massflxbase_mconv(i) ), massflxbase_w(i) )
!!        massflxbase(i) = 0.5*massflxbase_cape(i)+0.1*massflxbase_mconv(i)+0.4*massflxbase_w(i)
    !end do


!!    massflxbase = massflxbase*lat_coef

!!for amazon
    !!do i=1, inncol
        !!if( landfrac(i)==1. ) then
            !!massflxbase(i) = 3.*massflxbase(i)
        !!end if
    !!end do

!!------------------------------------------------------
!! Calculate Final Output
!!------------------------------------------------------
    !do i=1, inncol
!! Apply Base Mass Flux to Normalized Tendencies
        !condrate_up(i,:) = condrate_up(i,:)*massflxbase(i)
        !rainrate_up(i,:) = rainrate_up(i,:)*massflxbase(i)

        !condstend(i,:) = condstend(i,:)*massflxbase(i)
        !condqtend(i,:) = condqtend(i,:)*massflxbase(i)
        !transtend_up(i,:) = transtend_up(i,:)*massflxbase(i)
        !tranqtend_up(i,:) = tranqtend_up(i,:)*massflxbase(i)

        !if ( trigdp(i)<1 ) cycle

!! Final Tendencies
        !stend(i,:) = condstend(i,:)+transtend_up(i,:)
        !qtend(i,:) = condqtend(i,:)+tranqtend_up(i,:)
        !qliq(i,:)  = qliq_up(i,:)
        !rainrate(i,:) = rainrate_up(i,:)
        !condrate(i,:) = condrate_up(i,:)

!! Accumulate surface precipitation
        !do k=kuplcl(i), kuptop(i), -1
            !prec(i) = prec(i) + rho(i,k)*rainrate(i,k)*dz(i,k)
        !end do

    !end do

!!------------------------------------------------------
!! Liquid Tendency Calculation
!!------------------------------------------------------
    !do i=1, inncol
        !if ( trigdp(i)<1 ) cycle
        !do k=kuplcl(i), kuptop(i), -1
            !qliqtend(i,k) = normassflx_up(i,k)*det_rate_bulk_up(i,k)&
                !*qliq(i,k)/rho(i,k)
        !end do
        !qliqtend(i,:) = qliqtend(i,:)*massflxbase(i)
    !end do
!!    qliqtend = 0._r8

!!------------------------------------------------------
!! Evaporation Calculation
!!------------------------------------------------------
    !do i=1, inncol
        !if ( trigdp(i)<1 ) cycle
!!        do k=kuptop(i), kuplcl(i)
        !do k=kuptop(i), nlev 
            !evaprate(i,k) = evapke*max( 1-rh(i,k), 0._r8 )*sqrt( netprec(i) )
            !evaplimit = max(0._r8, (qsat(i,k)-q(i,k))/dtime )
            !evaplimit = min(evaplimit, netprec(i) / (rho(i,k)*dz(i,k)) )
!!            evaplimit = min(evaplimit, (prec(i) - evaprateflx(i))/(rho(i,k)*dz(i,k))  )
            !evaprate(i,k) = min( evaplimit, evaprate(i,k) )

            !evapstend(i,k) = -evaprate(i,k)*latvap
            !evapqtend(i,k) = evaprate(i,k)

            !netprec(i) = netprec(i) + rho(i,k)*( rainrate(i,k)-evaprate(i,k) )*dz(i,k)

            !if ( netprec(i)<=0. ) then
                !netprec(i) = 0._r8
            !end if

!!            write(*,*) netprec(i), evaplimit, evaprate(i,k), rainrate(i,k)

!!            evaprateflx(i) = evaprateflx(i) + rho(i,k)*evaprate(i,k)*dz(i,k)

        !end do

        !evapstend = 0._r8
        !evapqtend = 0._r8

        !stend(i,:) = stend(i,:)+evapstend(i,:)
        !qtend(i,:) = qtend(i,:)+evapqtend(i,:)

    !end do
    !netprec = netprec/rhofw
!!    prec = netprec

    !prec = prec/rhofw

    !!qliq = 0._r8
    !!rainrate = 0._r8

!!------------------------------------------------------
!! Another Way of Calculating Output Tendencies
!! tend = condensation rate + transport
!! According to Yanai 1972, 
!! the net effect of both condensatin rate and transnport is equal to
!! a gradient term (he called it compensating warming and drying)
!! + detrainment related terms + evaporation terms.
!! Ideally, we should have
!! stend = condstend+transtend = compstend
!! qtend = condqtend+tranqtend = compqtend
!! (outputs are very similar)
!!------------------------------------------------------
    !tmp1stend = 0._r8
    !tmp1qtend = 0._r8
    !tmp2stend = 0._r8
    !tmp2qtend = 0._r8
    !do i=1, inncol
        !if ( trigdp(i)<1 ) cycle
!!        do k=kupbase(i)-1, kuptop(i), -1
!!        do k=kuplcl(i)-1, kuptop(i), -1
        !do k=kuplaunch(i)-1, kuptop(i), -1
            !diffz = z(i,k)-z(i,k+1)
            !compstend(i,k) = &
                !normassflx_up(i,k)*( dse(i,k)-dse(i,k+1) )/diffz/rho(i,k) &
               !+normassflx_up(i,k)*det_rate_bulk_up(i,k)&
                !*( dse_up(i,k)-dse(i,k) )/rho(i,k)
            !compqtend(i,k) = &
                !normassflx_up(i,k)*( q(i,k)-q(i,k+1) )/diffz/rho(i,k) &
               !+normassflx_up(i,k)*det_rate_bulk_up(i,k)&
                !*( q_up(i,k)-q(i,k) )/rho(i,k)

!!for diagnostics
            !tmp1stend(i,k) = &
                !normassflx_up(i,k)*( dse(i,k)-dse(i,k+1) )/diffz/rho(i,k)
            !tmp1qtend(i,k) = &
                !normassflx_up(i,k)*( q(i,k)-q(i,k+1) )/diffz/rho(i,k)
            !tmp2stend(i,k) = &
                !normassflx_up(i,k)*det_rate_bulk_up(i,k)&
                !*( dse_up(i,k)-dse(i,k) )/rho(i,k)
            !tmp2qtend(i,k) = &
                !normassflx_up(i,k)*det_rate_bulk_up(i,k)&
                !*( q_up(i,k)-q(i,k) )/rho(i,k)

        !end do
        !compstend(i,:) = compstend(i,:)*massflxbase(i)
        !compqtend(i,:) = compqtend(i,:)*massflxbase(i)

!!for diagnostics
        !tmp1stend(i,:) = tmp1stend(i,:)*massflxbase(i)
        !tmp1qtend(i,:) = tmp1qtend(i,:)*massflxbase(i)
        !tmp2stend(i,:) = tmp2stend(i,:)*massflxbase(i)
        !tmp2qtend(i,:) = tmp2qtend(i,:)*massflxbase(i)

    !end do

!!------------------------------------------------------
!!make sure no negative q
!!------------------------------------------------------
    !qcheckout = 1._r8
    qcheck  = 0._r8
    !qcheck  = q + dtime*qtend

    !minqcheckf = 1._r8
    !do i=1, inncol
        !if ( trigdp(i)<1 ) cycle

!!whole column adjustment
        !minqcheckf = 1._r8
        !do k=nlev, 1, -1
            !if( (qtend(i,k)<0.) .and. (q(i,k)>0.) ) then
                !qcheckf = (min_q-q(i,k))/dtime/qtend(i,k)
                !if( (qcheckf<minqcheckf) .and. (qcheckf>0._r8) ) then
                    !minqcheckf = qcheckf
                !end if
            !end if
        !end do

        !if( minqcheckf<1._r8 ) then
            !qcheckout(i) = minqcheckf
            !massflxbase(i) = minqcheckf*massflxbase(i)
            !stend(i,:) = minqcheckf*stend(i,:)
            !qtend(i,:) = minqcheckf*qtend(i,:)
            !condstend(i,:) = minqcheckf*condstend(i,:)
            !condqtend(i,:) = minqcheckf*condqtend(i,:)
            !transtend_up(i,:) = minqcheckf*transtend_up(i,:)
            !tranqtend_up(i,:) = minqcheckf*tranqtend_up(i,:)
            !evapstend(i,:) = minqcheckf*evapstend(i,:)
            !evapqtend(i,:) = minqcheckf*evapqtend(i,:)

            !compstend(i,:) = minqcheckf*compstend(i,:)
            !compqtend(i,:) = minqcheckf*compqtend(i,:)

            !tmp1stend(i,:) = minqcheckf*tmp1stend(i,:)
            !tmp1qtend(i,:) = minqcheckf*tmp1qtend(i,:)
            !tmp2stend(i,:) = minqcheckf*tmp2stend(i,:)
            !tmp2qtend(i,:) = minqcheckf*tmp2qtend(i,:)

            !qliq(i,:) = minqcheckf*qliq(i,:)
            !rainrate(i,:) = minqcheckf*rainrate(i,:)
            !prec(i) = minqcheckf*prec(i)
            !netprec(i) = minqcheckf*netprec(i)
        !end if

!!!single level adjustment
        !!minqcheckf = 1._r8
        !!do k=nlev, 1, -1
            !!if( qcheck(i,k)<=min_q ) then
!!!                qtend(i,k) = (min_q-q(i,k))/dtime
                !!qcheckf = (min_q-q(i,k))/dtime/qtend(i,k)
!!!                stend(i,k) = qcheckf*stend(i,k)
!!!                compstend(i,k) = qcheckf*compstend(i,k)
                !!qtend(i,k) = qcheckf*qtend(i,k)
                !!compqtend(i,k) = qcheckf*compqtend(i,k)

                !!!if( (qcheckf<minqcheckf) .and. (qcheckf>0._r8) ) then
                    !!!minqcheckf = qcheckf
!!!                end if
            !!end if
        !!end do

        !qcheckout(i) = minqcheckf

    !end do

!!clean output.
    !do i=1, inncol
        !if ( trigdp(i)<1 ) then
            !mse_up(i,:) = 0._r8
        !end if
    !end do


    outmb = massflxbase

!    outtmp2d = lat_coef
!    outtmp2d = capeclm
!    outtmp2d = dilucape
!    outtmp2d zero = capefc
!    outtmp2d = massflxbase_conv
!    outtmp2d = massflxbase
!    outtmp2d = massflxbase_cape/(massflxbase_cape+massflxbase_mconv+massflxbase_w)
!    outtmp2d = massflxbase_dcape
    outtmp2d = trigdp
!    outtmp2d = landfrac
!    outtmp2d = massflxbase
!    outtmp3d = t

    outmse = mse
    outmsesat= msesat
    outmseup = mse_up

    outstend = stend
    outqtend = qtend
    outcondstend = condstend
    outcondqtend = condqtend
    outtranupstend = transtend_up
    outtranupqtend = tranqtend_up
    outtrandnstend = 0._r8
    outtrandnqtend = 0._r8
    outevapstend = evapstend
    outevapqtend = evapqtend


#ifdef SCMDIAG 
    write(*,"(a20,f20.10)") "dtime:", dtime
    write(*,"(a20,f20.10,a20,f20.10,a20,f20.10)") "lat:", lat, "zsrf:", zsrf, "psrf:", psrf
    write(*,"(a20,i4,a20,i4)") "uplaunch:", kuplaunch, " upbase:  ", kupbase, " uplcl:", kuplcl
    write(*,"(a20,i4)") "uptopmax:", kuptopmax
    write(*,"(a20,i4,a20,i4)") "uptop:", kuptop,    "knbtop:", knbtop
    write(*,"(a20,i4,a20,i4)") "trigdp:", trigdp,    "trigsh:   ", trigsh
    write(*,"(a20,f20.10)") "ht:", ht
    write(*,"(a20,f20.10)") "bflsdilucape:", bfls_dilucape
    write(*,"(a20,f20.10)") "dilucape:", dilucape
    write(*,"(a20,f20.10)") "dilucape_closure:", dilucape_closure
    write(*,"(a20,f20.10)") "capefc:", capefc
    write(*,"(a20,f20.10)") "capeclm:", capeclm
    write(*,"(a20,f20.10)") "mconv:", mconv
    write(*,"(a20,f20.10)") "massflxbase_p:", massflxbase_p
    write(*,"(a20,f20.10)") "massflxbase_cape:", massflxbase_cape
    write(*,"(a20,f20.10)") "massflxbase_dcape:", massflxbase_dcape
    write(*,"(a20,f20.10)") "massflxbase_clm:", massflxbase_clm
    write(*,"(a20,f20.10)") "massflxbase_w:", massflxbase_w
    write(*,"(a20,f20.10)") "massflxbase_mconv:", massflxbase_mconv
    write(*,"(a20,f20.10)") "massflxbase:", massflxbase
    write(*,"(a20,f20.10)") "prec:", prec*3600*24*1000
    write(*,"(a20,f20.10)") "netprec:", netprec*3600*24*1000
    write(*,"(a20,f20.10)") "minqcheckf:", minqcheckf

!netcdf output
    call subcol_netcdf_putclm( "mse", nlev, mse(1,:), 1 )
    call subcol_netcdf_putclm( "msesat", nlev, msesat(1,:), 1 )
    call subcol_netcdf_putclm( "z", nlev, z(1,:), 1 )
    call subcol_netcdf_putclm( "p", nlev, p(1,:), 1 )
    call subcol_netcdf_putclm( "rho", nlev, rho(1,:), 1 )

    call subcol_netcdf_putclm( "ent_rate", nlev, ent_rate_bulk_up(1,:), 1 )

    call subcol_netcdf_putclm( "w_up_mid", nlev, w_up_mid(1,:), 1 )
    call subcol_netcdf_putclm( "buoy_mid", nlev, buoy_mid(1,:), 1 )
    call subcol_netcdf_putclm( "mse_up_mid", nlev, mse_up_mid(1,:), 1 )
    call subcol_netcdf_putclm( "t_up_mid", nlev, t_up_mid(1,:), 1 )
    call subcol_netcdf_putclm( "q_up_mid", nlev, q_up_mid(1,:), 1 )
    call subcol_netcdf_putclm( "normassflx_up_mid", nlev, normassflx_up_mid(1,:), 1 )

    call subcol_netcdf_putclm( "zint", nlevp, zint(1,:), 1 )
    call subcol_netcdf_putclm( "pint", nlevp, pint(1,:), 1 )

    call subcol_netcdf_putclm( "w_up", nlevp, w_up(1,:), 1 )
    call subcol_netcdf_putclm( "buoy", nlevp, buoy(1,:), 1 )
    call subcol_netcdf_putclm( "mse_up", nlevp, mse_up(1,:), 1 )
    call subcol_netcdf_putclm( "t_up", nlevp, t_up_mid(1,:), 1 )
    call subcol_netcdf_putclm( "q_up", nlevp, q_up_mid(1,:), 1 )
    call subcol_netcdf_putclm( "normassflx_up", nlevp, normassflx_up_mid(1,:), 1 )

    !do i=1, inncol
        !if ( trigdp(i)<1 ) cycle
        !do k=kuplaunch(i), kuptop(i), -1
            !diffdse_up(i,k) = normassflx_up(i,k)*( dse_up(i,k)-dse(i,k) )
            !diffq_up(i,k)   = normassflx_up(i,k)*( q_up(i,k)-q(i,k) )
        !end do
    !end do

    !call subcol_netcdf_putclm( "diffdse_up", diffdse_up(1,:), 1 )
    !call subcol_netcdf_putclm( "diffq_up", diffq_up(1,:), 1 )
    !call subcol_netcdf_putclm( "qcheck", qcheck(1,:), 1 )

    call subcol_netcdf_putclm( "t", nlev, t(1,:), 1 )
    call subcol_netcdf_putclm( "q", nlev, q(1,:), 1 )
    call subcol_netcdf_putclm( "qsat", nlev, qsat(1,:), 1 )

    !call subcol_netcdf_putclm( "dse", dse(1,:), 1 )
    !call subcol_netcdf_putclm( "dse_up", dse_up(1,:), 1 )

    !call subcol_netcdf_putclm( "stend", stend(1,:), 1 )
    !call subcol_netcdf_putclm( "qtend", qtend(1,:), 1 )
    !call subcol_netcdf_putclm( "stendcond", condstend(1,:), 1 )
    !call subcol_netcdf_putclm( "qtendcond", condqtend(1,:), 1 )
    !call subcol_netcdf_putclm( "stendtran", transtend_up(1,:), 1 )
    !call subcol_netcdf_putclm( "qtendtran", tranqtend_up(1,:), 1 )
    !call subcol_netcdf_putclm( "compstend", compstend(1,:), 1 )
    !call subcol_netcdf_putclm( "compqtend", compqtend(1,:), 1 )

    !call subcol_netcdf_putclm( "tmp1stend", tmp1stend(1,:), 1 )
    !call subcol_netcdf_putclm( "tmp1qtend", tmp1qtend(1,:), 1 )
    !call subcol_netcdf_putclm( "tmp2stend", tmp2stend(1,:), 1 )
    !call subcol_netcdf_putclm( "tmp2qtend", tmp2qtend(1,:), 1 )

    !call subcol_netcdf_putclm( "evapstend", evapstend(1,:), 1 )
    !call subcol_netcdf_putclm( "evapqtend", evapqtend(1,:), 1 )

    !call subcol_netcdf_putclm( "qliq", qliq(1,:), 1 )
    !call subcol_netcdf_putclm( "rainrate", rainrate(1,:), 1 )
    !call subcol_netcdf_putclm( "condrate", condrate(1,:), 1 )
    !call subcol_netcdf_putclm( "evaprate", evaprate(1,:), 1 )

    !call subcol_netcdf_putclm( "prec", prec(1), 1 )
    !call subcol_netcdf_putclm( "dilucape", dilucape(1), 1 )
    !call subcol_netcdf_putclm( "pmassflxbase", massflxbase_p(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase", massflxbase(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_cape", massflxbase_cape(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_w", massflxbase_w(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_mconv", massflxbase_mconv(1), 1 )
    call subcol_netcdf_putclm( "qcheck", 1, qcheckout(1), 1 )

    tmp2d = trigdp
    call subcol_netcdf_putclm( "trigdp", 1, tmp2d(1), 1 )

    tmp = kupbase-kuptop+1
    call subcol_netcdf_putclm( "nconvlev", 1, tmp(1), 1 )
    tmp = kuplaunch
    call subcol_netcdf_putclm( "kuplaunch", 1, tmp(1), 1 )
    tmp = kupbase
    call subcol_netcdf_putclm( "kupbase", 1, tmp(1), 1 )
    tmp = kuplcl
    call subcol_netcdf_putclm( "kuplcl", 1, tmp(1), 1 )



!   call stdout3d( mse, msesat, t, q, z)
!   call stdout3d( mse, msesat, mse_up, q, qsat)
!   call stdout3d( z, mse, msesat, mse_up, w)

   !call stdout3d( z, ent_rate_bulk_up, &
      !det_rate_bulk_up, q_up, condrate_up)
   !call stdout3d( z, ent_rate_bulk_up, &
      !det_rate_bulk_up, normassflx_up)

!   call stdout3d( z, normassflx_up, q_up, condrate_up)
!   call stdout3d( z, normassflx_up, t_up, dse, dse_up)
!   call stdout3d( z, normassflx_up, t, t_up, tv_closure)

!   call stdout3d( z, w_up, buoy, ent_rate_bulk_up, normassflx_up )
!   call stdout3d( z, dz, buoy)
!   call stdout3d( z, q, q_up, mse_up, condrate_up)
!    call stdout3dmix( z, zint, dp, pint )
!    call stdout3dmix( ent_rate_bulk_up, mse_up, buoy_mid, normassflx_up )
!    call stdout3dmix( w_up_mid, mse_up, msesat, w_up )
    call stdout3dmix( q, qint, t, tint )

!   call stdout3d( z, q_up, normassflx_up, qliq_up, condrate_up )
!   call stdout3d( z, normassflx_up, q, qsat, q_up)
!   call stdout3d( z, normassflx_up, mse, msesat, mse_up)
!   call stdout3d( z, dse, dse_up, normassflx_up, stend )
!   call stdout3d( z, q, q_up, normassflx_up, qtend )
!   call stdout3d( z, normassflx_up, q_up, qtend, qcheck)
!   call stdout3d( z, normassflx_up, q_up, q)
!   call stdout3d( z, normassflx_up, q_up, qsat, condrate_up)
!   call stdout3d( z, condrate_up, rainrate, evaprate, qliqtend )
!   call stdout3d( z, stend, compstend, qtend, compqtend, normassflx_up)

#endif

end subroutine scp_conv_tend



subroutine cal_launchtolcl( &
!input
        z, zint, p, t, q, qsat, mse, msesat, landfrac, lhflx, tpert, &
!output
        kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up,  &
!in/out
        trig)
!------------------------------------------------------
!launch to LCL, no entrainment up, in-cloud properties
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: z   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: zint  ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: p   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: t   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: q   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: qsat! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: mse ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: msesat ! [J/kg]
    real(r8), dimension(ncol), intent(in) :: landfrac ! [J/kg]
    real(r8), dimension(ncol), intent(in) :: lhflx    ! [J/kg]
    real(r8), dimension(ncol), intent(in) :: tpert    ! [J/kg]
!output
    integer, dimension(ncol), intent(out) :: kuplaunch ! [1]
    integer, dimension(ncol), intent(out) :: kuplcl    ! [1]

    real(r8), dimension(ncol, nlevp), intent(out) :: mse_up ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: t_up ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: q_up ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: normassflx_up ! [kg/kg]

!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]

!local
    integer :: i, k, stat
    real(r8) :: msemax, q_up_test
    real(r8) :: diffmse, dt, dq, buoy
    integer, dimension(ncol) :: kuplaunchmin  ! [1]
    integer, dimension(ncol) :: kuplaunchmax  ! [1]
    real(r8) :: perturbt
    real(r8) :: perturbq

!intialize output
    kuplaunch = 1
    kuplcl = 1

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        do k=nlev, 1, -1
            if ( z(i,k) >= zuplaunchlow ) then
                kuplaunchmin(i) = k
                exit
            end if
        end do
        do k=nlev, 1, -1
            if ( z(i,k) >= zuplaunchtop ) then
                kuplaunchmax(i) = k
                exit
            end if
        end do
    end do

!find the maximun MSE level as cloud parcel launching point
    kuplaunch = nlev
    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        msemax = 0._r8
        do k=kuplaunchmin(i), kuplaunchmax(i), -1
            if ( mse(i,k) >= msemax ) then
                msemax = mse(i,k)
                kuplaunch(i) = k
            end if
        end do

        perturbt = 0._r8
        perturbq = 0._r8

!        perturbt = 0._r8
!        perturbq = 0.15*q(i,k)
!        perturbq = 0.0005_r8

!        if ( i == 5 ) then
!            write(*,'(f10.5)') landfrac(i)
!            write(*,'(f10.5)') lhflx(i)
!        end if

        !if( landfrac(i)==1 ) then
!!            perturbt = 100*tpert(i)
            !perturbq = lhflx(i)/20000.
        !end if

        t_up(i,kuplaunch(i) ) = t(i,kuplaunch(i) )+perturbt
        q_up(i,kuplaunch(i) ) = q(i,kuplaunch(i) )+perturbq
        mse_up(i,kuplaunch(i) ) = mse(i,kuplaunch(i) )+perturbt*cpair+perturbq*latvap

        do k=kuplaunch(i)-1, 1, -1

            mse_up(i,k) = mse_up(i,k+1)
            q_up(i,k)   = q_up(i,k+1)

            !diffmse = mse_up(i,k)-msesat(i, k)
            !call cal_buoy( diffmse, t(i,k), q(i,k), qsat(i,k), dt, dq, buoy )
            !t_up(i,k) = t(i,k) + dt
!!            q_up(i,k) = qsat(i,k) + dq
            !q_up_test = qsat(i,k) + dq

            call mse2tsat( mse_up(i,k), z(i,k), p(i,k), t_up(i,k), q_up_test, stat )

            if( q_up(i,k)>q_up_test ) then
                q_up(i,k) = q_up_test
                if ( kuplcl(i) == 1 ) then
                    kuplcl(i) = k
                    exit
                end if
            else
                t_up(i,k) = ( mse_up(i,k)-latvap*q_up(i,k)-gravit*zint(i,k) )/cpair
            end if
        end do

        do k=nlevp, kuplcl(i), -1
            normassflx_up(i,k) = (zint(i,k)/zint(i,kuplcl(i)))**0.5
        end do

    end do

end subroutine cal_launchtolcl



subroutine cal_uplcl( &
!input
        z, p, t, q, &
        mse, msesat, ent_rate_bulk_subcld_up, &
        kuplaunch, kuplcl, &
!output
        kupbase, kuptopmax, mse_up, t_up, q_up, normassflx_up, &
!in/out
        trig)
!------------------------------------------------------
!calculate the cloud levels
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: z   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: p   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: t   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: q   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: mse ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: msesat ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: ent_rate_bulk_subcld_up ! [J/kg]
    integer, dimension(ncol), intent(in) :: kuplaunch ! [1]
    integer, dimension(ncol), intent(in) :: kuplcl ! [1]
!output
    integer, dimension(ncol), intent(out) :: kupbase ! [1]
    integer, dimension(ncol), intent(out) :: kuptopmax ! [1]
    real(r8), dimension(ncol, nlev), intent(out) :: mse_up ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(out) :: t_up ! [K]
    real(r8), dimension(ncol, nlev), intent(out) :: q_up ! [kg/kg]
    real(r8), dimension(ncol, nlev), intent(out) :: normassflx_up ! [kg/kg]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]

!local
    integer :: i, k, stat
    real(r8) :: mselaunch
    real(r8) :: dz, intsummassflux, intsum, inttest, q_up_test, t_up_test

    real(r8) :: ent_rate_subcld_tmp

!intialize output
    kupbase = 1
    kuptopmax = 1

    intsum = 0._r8
    inttest = 0._r8

!find the cloud base by launching cloud parcel until [parcel MSE]>[env MSE*]
    do i=1, ncol
        if ( trig(i) < 1 ) cycle

        intsummassflux = 0._r8

        do k=kuplcl(i)-1, 1, -1
            dz = z(i,k)-z(i,k+1)

            ent_rate_subcld_tmp = ent_rate_subcld
!            ent_rate_subcld_tmp = ent_rate_bulk_subcld_up(i,k)

!higher than launch point calculate mass flux, entrained in-cloud MSE, and entrained in-cloud Q
            intsummassflux = intsummassflux + dz*ent_rate_subcld_tmp

            !normassflx_up(i,k) = min( &
                !normassflx_up(i, k+1 )&
                !+dz*normassflx_up(i,k+1)*ent_rate_subcld_tmp&
                !, 100._r8)
            normassflx_up(i,k) = min( exp( intsummassflux ), 100._r8)

!!flux form
!!must be a backward scheme to match condensation calculation
            !intsum = 1./normassflx_up(i,k)*( normassflx_up(i,k+1)*mse_up(i,k+1)&
                    !+dz*ent_rate_subcld_tmp*normassflx_up(i,k)*mse(i,k) )

!forward
            intsum = mse_up(i,k+1) &
                +dz*ent_rate_subcld_tmp*(mse(i,k+1)-mse_up(i,k+1) )
!!backward
            !intsum = ( mse_up(i,k+1)+dz*ent_rate_up(i,k)*mse(i,k) )&
                !/( 1 + dz*ent_rate_up(i,k) )

            mse_up(i,k) = intsum

!!flux form
!!must be a backward scheme to match condensation calculation
            !intsum = 1./normassflx_up(i,k)*( normassflx_up(i,k+1)*q_up(i,k+1)&
                    !+dz*ent_rate_subcld_tmp*normassflx_up(i,k)*q(i,k) )
!forward
            q_up(i,k) = q_up(i,k+1) &
                +dz*ent_rate_subcld_tmp*(q(i,k+1)-q_up(i,k+1) )

            call mse2tsat( mse_up(i,k), z(i,k), p(i,k), t_up_test, q_up_test, stat )

            !diffmse = mse_up(i,k)-msesat(i, k)
            !call cal_buoy( diffmse, t(i,k), q(i,k), qsat(i,k), dt, dq, buoy(i,k) )
            !t_up(i,k) = t(i,k) + dt
            !q_up(i,k) = qsat(i,k) + dq

            if( q_up(i,k)<q_up_test ) then
                trig(i) = -1
                exit
            else
                q_up(i,k) = q_up_test
                t_up(i,k) = t_up_test
            end if
            
            if( mse_up(i,K)<mse(i,k) ) then
                trig(i) = -1
                exit
            end if

!positive buoyancy, cloud base
            if ( mse_up(i,k) >= msesat(i,k) ) then
                kupbase(i) = k
                exit
            end if

        end do

        if ( kupbase(i) == 1 ) then
!cannot find cloud base, not trigger
            trig(i) = -2
        else
!can find cloud base, try to launch to see how high it can reach(without entrainment)
            do k=kupbase(i), 1, -1
                if ( msesat(i,k) > mse_up(i, kupbase(i)) ) then
                    kuptopmax(i) = k+1
                    exit
                end if
            end do

        end if

    end do

end subroutine cal_uplcl



subroutine triggercond( &
!input
        z, rh, landfrac, conv, dilucape, bfls_dilucape, &
        kuplaunch, kupbase, &
!output
!        output, &
!in/out
        trig)

!------------------------------------------------------
!calculate the cloud levels
!------------------------------------------------------
!input
!        real(r8), dimension(ncol, nlev), intent(in) :: input     ! [unit]
    real(r8), dimension(ncol, nlev), intent(in) :: z      ! [unit]
    real(r8), dimension(ncol, nlev), intent(in) :: rh     ! [unit]
    real(r8), dimension(ncol), intent(in) :: landfrac     ! [unit]
    real(r8), dimension(ncol), intent(in) :: conv         ! [unit]
    real(r8), dimension(ncol), intent(in) :: dilucape       ! [unit]
    real(r8), dimension(ncol), intent(in) :: bfls_dilucape  ! [unit]
    integer , dimension(ncol), intent(in) :: kuplaunch  ! [1]
    integer , dimension(ncol), intent(in) :: kupbase    ! [1]
 !output
!        real(r8), dimension(ncol, nlev), intent(out) :: output   ! [unit]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local

    integer :: i,j,k

!intialize output
!        output = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle

        !if( dilucape(i)<200. ) then
            !trig(i) = -1
            !cycle
        !end if

!main trigger
!        if( landfrac(i) == 0. ) then
            if( (dilucape(i)-bfls_dilucape(i))<25. ) then
                trig(i) = -5
                cycle
            end if
!        end if


        !if( conv(i)<0. ) then
            !trig(i) = -5
            !cycle
        !end if

        !if( landfrac(i)>0. ) then
            !if( (dilucape(i)-bfls_dilucape(i))<0. ) then
                !trig(i) = -1
                !cycle
            !end if
        !else
            !if( (dilucape(i)-bfls_dilucape(i))<10. ) then
                !trig(i) = -1
                !cycle
            !end if
        !end if

        !if( (zupbase(i)-z(i, kuplaunch(i)))>2500. ) then
            !trig(i) = -1
            !cycle
        !end if

        !do k=nlev, 1, -1
        !end do

    end do

end subroutine triggercond



subroutine cal_mse_up( &
!input
        ent_opt, z, zint, dz, p, pint, t, tint, q, qint, qsat, mse, msesat, kuplaunch, kuplcl, &
        ent_rate_up, det_rate_up, w_up_init, &
!output
        mse_up, t_up, q_up, normassflx_up, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
!in/out
        trig)

!input
    integer, intent(in) :: ent_opt
    real(r8), dimension(ncol, nlev),  intent(in) :: z       ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: zint   ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: dz      ! [m]
    real(r8), dimension(ncol, nlev), intent(in)  :: p      ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: pint   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: t      ! [K]
    real(r8), dimension(ncol, nlevp), intent(in) :: tint   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: q      ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: qint   ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: qsat   ! [kg/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: mse    ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: msesat ! [J/kg]
    integer , dimension(ncol), intent(in) :: kuplaunch ! [1]
    integer , dimension(ncol), intent(in) :: kuplcl    ! [1]

    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_up ! [1]
    real(r8), dimension(ncol), intent(inout) :: w_up_init ! [1]
!output
    real(r8), dimension(ncol, nlevp), intent(out) :: w_up ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(out)  :: w_up_mid ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: buoy ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(out)  :: buoy_mid  ! [J/kg]

    integer , dimension(ncol), intent(out) :: kuptop
    real(r8), dimension(ncol), intent(out) :: zuptop  ! [m]
!input/output
    real(r8), dimension(ncol, nlevp), intent(inout) :: t_up  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: q_up  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: mse_up  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: normassflx_up  ! [J/kg]
    integer , dimension(ncol), intent(inout) :: trig     ! [1]
!local
    real(r8), dimension(ncol, nlev)  :: w2_up    ! [1]
    real(r8), dimension(ncol, nlevp) :: tvint    ! [J/kg]

    real(r8) :: dt   ! [m]
    real(r8) :: dq   ! [m]
    real(r8) :: diffmse !
    real(r8) :: intsum, inttest
    real(r8) :: intsummassflux
    real(r8) :: tv, tv_up, msesat_lcl, ent_rate_up_l, ent_rate_up_h
    real(r8) :: esat_tmp, qsat_tmp
    real(r8) :: q_up_test

    integer :: i,j,k
    integer :: kuplcl_tmp
    integer :: stat

!intialize output.
    t_up = 0._r8
    q_up = 0._r8
    mse_up = 0._r8

    w_up = 0._r8
    w_up_mid = 0._r8
    buoy = 0._r8
    buoy_mid = 0._r8
    w2_up = 0._r8
    kuptop = nlev
    zuptop = 0._r8

    tvint = tint*(1+0.61*qint)

    if ( ent_opt == 0 ) then
    else if (ent_opt == 1) then
        ent_rate_up = 0._r8
        det_rate_up = 0._r8
        do i=1, ncol
            if ( trig(i) < 1 ) cycle

            kuplcl_tmp = kuplcl(i)

            w_up(i,kuplcl_tmp)  = w_up_init(i)
            w2_up(i,kuplcl_tmp) = w_up_init(i)**2
            w_up_mid(i,kuplcl_tmp) = w_up_init(i)

            t_up(i,kuplcl_tmp) = tint(i,kuplcl_tmp)
            esat_tmp = 611.2*exp(5417*(1/273.16-1/t_up(i,kuplcl_tmp)))
            qsat_tmp = 0.622*esat_tmp/t_up(i,kuplcl_tmp)
            q_up(i,kuplcl_tmp) = qsat_tmp

            mse_up(i,kuplcl_tmp) = msesat(i,kuplcl_tmp)

            !tv = 0.5*( t(i,kuplcl_tmp)*(1+0.61*q(i,kuplcl_tmp) ) &
                      !+t(i,kuplcl_tmp-1)*(1+0.61*q(i,kuplcl_tmp-1) ) )
            tv = tvint(i,kuplcl_tmp)
            tv_up = t_up(i,kuplcl_tmp )*(1+0.61*q_up(i,kuplcl_tmp ) )
            buoy(i,kuplcl_tmp ) = gravit*(tv_up-tv)/tv
            if ( buoy(i,kuplcl_tmp)<0. ) then
                trig(i) = -11
                cycle
            end if
            ent_rate_up_l = greg_ce*greg_a*buoy(i,kuplcl_tmp)/w2_up(i,kuplcl_tmp )

            do k=kuplcl_tmp-1, 1, -1
!precalculate k+1/2
                w2_up(i,k) = w2_up(i,k+1)+2*dz(i,k)*greg_a*(1-greg_ce)*buoy(i,k+1)
                if ( w2_up(i,k)<=0._r8 ) then
                    trig(i) = -12
                    exit
                end if
                w_up(i,k)  = sqrt( w2_up(i,k) )
                normassflx_up(i,k) = normassflx_up(i,k+1)*exp(ent_rate_up_l*dz(i,k) )
                mse_up(i,k) = (normassflx_up(i,k+1)*mse_up(i,k+1) &
                    +dz(i,k)*ent_rate_up_l*normassflx_up(i,k+1)*mse(i,k) ) &
                    /normassflx_up(i,k)

                call mse2tsat( mse_up(i,k), zint(i,k), pint(i,k), t_up(i,k), q_up_test, stat )
                tv_up = t_up(i,k)*( 1+0.61*q_up_test )
                tv = tint(i,k)*(1+0.61*qint(i,k) )
                buoy(i,k) = gravit*(tv_up-tv)/tv
                if ( buoy(i,k)<=0 ) then
                    ent_rate_up_h = 0._r8
                else
                    ent_rate_up_h = greg_ce*greg_a*buoy(i,k)/w2_up(i,k)
                end if
!calculate mid
                w_up_mid(i,k) = 0.5*( w_up(i,k)+w_up(i,k+1) )
                buoy_mid(i,k) = 0.5*( buoy(i,k)+buoy(i,k+1) )
                ent_rate_up(i,k) = 0.5*( ent_rate_up_l+ent_rate_up_h )
!recalculate k+1/2
                w2_up(i,k) = w2_up(i,k+1)+2*dz(i,k)*greg_a*(1-greg_ce)*buoy_mid(i,k)
                if ( w2_up(i,k)<=0._r8 ) then
                    trig(i) = -13
                    exit
                end if
                w_up(i,k)  = sqrt( w2_up(i,k) )
                normassflx_up(i,k) = normassflx_up(i,k+1)*exp(ent_rate_up(i,k)*dz(i,k) )

                mse_up(i,k) = ( normassflx_up(i,k+1)*mse_up(i,k+1) &
                    +( normassflx_up(i,k)-normassflx_up(i,k+1) )*mse(i,k) ) &
                    /normassflx_up(i,k)
                !mse_up(i,k) = exp(-ent_rate_up(i,k)*dz(i,k))*mse_up(i,k+1) &
                    !+ ( 1 - exp(-ent_rate_up(i,k)*dz(i,k)) )*mse(i,k)

                call mse2tsat( mse_up(i,k), zint(i,k), pint(i,k), t_up(i,k), q_up_test, stat )

                q_up(i,k) = q_up_test

                tv_up = t_up(i,k)*( 1+0.61*q_up_test )
                buoy(i,k) = gravit*(tv_up-tv)/tv

                if ( buoy(i,k)<=0 ) then
                    ent_rate_up_l = 0._r8
                else
                    ent_rate_up_l = ent_rate_up_h
                end if

            end do

            !buoy(i,kupbase(i) ) = 0._r8
            !ent_rate_up(i,kupbase(i)) = greg_ce*&
                !greg_a*buoy(i,kupbase(i))/w2_up(i,kupbase(i))
            !ent_rate_up(i,kupbase(i)-1) = ent_rate_up(i,kupbase(i))

        end do
    end if

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
    end do


    !do i=1, ncol
        !if ( trig(i) < 1 ) cycle

        !intsummassflux = 0._r8
        !do k=kupbase(i)-1, 1, -1
            !dz = z(i,k)-z(i,k+1)
!!!old method
            !!inttest = mse_up(i,k+1)+0.5*dz*&
                !!0.5*( ent_rate_up(i,k)+ent_rate_up(i,k+1) )&
                !!*(mse(i,k+1)-mse_up(i,k+1) )
            !!if( inttest<0.5*(mse(i,k)+mse(i,k+1)) ) then
                !!inttest = mse_up(i,k+1)+dz*&
                    !!0.5*( ent_rate_up(i,k)+ent_rate_up(i,k+1) )&
                    !!*(mse(i,k+1)-mse_up(i,k+1) )
                !!intsum = inttest
            !!else
                !!intsum =  mse_up(i,k+1)+dz*0.5*(ent_rate_up(i,k)+ent_rate_up(i,k+1))&
                    !!*( 0.5*(mse(i,k)+mse(i,k+1)) - inttest )
            !!end if

!!new method
!!integral method
            !if ( ent_opt == 1 ) then
                !ent_rate_up(i,k) = ent_rate_up(i,k+1)
            !end if
            !intsummassflux = intsummassflux + dz*0.5 &
                !*( ent_rate_up(i,k)-det_rate_up(i,k)+&
                   !ent_rate_up(i,k+1)-det_rate_up(i,k+1) )
            !!intsummassflux = intsummassflux + dz &
                !!*( ent_rate_up(i,k+1)-det_rate_up(i,k+1) )
            !normassflx_up(i,k) = normassflx_up(i, kupbase(i) )*exp( intsummassflux )
!!forward method
            !!normassflx_up(i,k) = normassflx_up(i, k+1 )&
                !!+dz*normassflx_up(i,k+1)*( ent_rate_up(i,k+1)-det_rate_up(i,k+1) )

            !!intsum = 1/normassflx_up(i,k)*( normassflx_up(i,k+1)*mse_up(i,k+1) &
                !!+0.5*dz*normassflx_up(i,k)&
                !!*( ent_rate_up(i,k)*( mse(i,k)+mse(i,k+1) ) &
                  !!-det_rate_up(i,k)*( msesat(i,k)+msesat(i,k+1) )  ) )

!!ensemble cloud model in Arakawa and ZM
            !!intsum = mse_up(i,k+1) &
                !!+dz*( ent_rate_up(i,k+1)*mse(i,k+1)   -ent_rate_up(i,k+1)*mse_up(i,k+1) &
                     !!-det_rate_up(i,k+1)*msesat(i,k+1)+det_rate_up(i,k+1)*mse_up(i,k+1) )
            !!intsum = ( mse_up(i,k+1)&
                !!+dz*(ent_rate_up(i,k)*mse(i,k)-det_rate_up(i,k)*msesat(i,k)) )&
                !!/( 1 + dz*(ent_rate_up(i,k)-det_rate_up(i,k)) )

            !!intsum = 1/normassflx_up(i,k)*( normassflx_up(i,k+1)*mse_up(i,k+1) &
                !!+dz*normassflx_up(i,k+1)&
                !!*( ent_rate_up(i,k+1)*mse(i,k+1)-det_rate_up(i,k+1)*msesat(i,k+1) ) )
            !!intsum = 1/normassflx_up(i,k)*( normassflx_up(i,k+1)*mse_up(i,k+1) &
                !!+dz*normassflx_up(i,k)&
                !!*( ent_rate_up(i,k)*mse(i,k)-det_rate_up(i,k)*msesat(i,k) ) )

!!bulk cloud model
!!forward
            !intsum = mse_up(i,k+1) &
                !+dz*ent_rate_up(i,k+1)*(mse(i,k+1)-mse_up(i,k+1) )
!!!backward
            !!intsum = ( mse_up(i,k+1)+dz*ent_rate_up(i,k)*mse(i,k) )&
                !!/( 1 + dz*ent_rate_up(i,k) )
!!!X not working
            !!intsum = 1/normassflx_up(i,k)*( normassflx_up(i,k+1)*mse_up(i,k+1) &
                !!+dz*normassflx_up(i,k+1)&
                !!*( ent_rate_up(i,k+1)*mse(i,k+1)-det_rate_up(i,k+1)*mse_up(i,k+1) ) )

            !mse_up(i,k) = intsum

!!forward
            !intsum = q_up(i,k+1) &
                !+dz*ent_rate_up(i,k+1)*(q(i,k+1)-q_up(i,k+1) )

            !q_up(i,k) = intsum

!!method 1 to get parcel T/Q, using bisection solver
            !call mse2tsat( mse_up(i,k), z(i,k), p(i,k), t_up(i,k), q_up_test, stat )

            !if ( ( mse_up(i,k)<=msesat(i,k) ) &
                !.or. ( mse_up(i,K)<mse(i,k) )  ) then
                !if ( k == (kupbase(i)-1) ) then
                    !trig(i) = -3
                !else
                    !zuptop(i) = z(i,k+1)+ ( mse_up(i,k+1)-msesat(i,k+1) ) &
                        !/( mse_up(i,k+1)-msesat(i,k+1) + msesat(i,k)-mse_up(i,k) ) &
                        !*( z(i,k)-z(i,k+1) )
                    !kuptop(i) = k+1
                !end if
                !exit
            !end if

!!test if too dry
            !if( q_up(i,k)<q_up_test ) then
                !kuptop(i) = k+1
                !exit
            !else
                !q_up(i,k) = q_up_test
            !end if

            !tv = t(i,k)*(1+0.61*q(i,k) )
            !tv_up = t_up(i,k)*(1+0.61*q_up(i,k) )
            !buoy(i,k) = gravit*(tv_up-tv)/tv

!!method 2 to get parcel T/Q
            !!diffmse = mse_up(i,k)-msesat(i, k)
            !!call cal_buoy( diffmse, t(i,k), q(i,k), qsat(i,k), dt, dq, buoy(i,k) )
            !!t_up(i,k) = t(i,k) + dt
            !!q_up(i,k) = qsat(i,k) + dq

            !if ( ent_opt == 1 ) then
                !w2_up(i,k) = w2_up(i,k+1)+2*dz*greg_a*(1-greg_ce)*buoy(i,k)
                !w_up(i,k)  = sqrt( w2_up(i,k) )
                !ent_rate_up(i,k) = greg_ce*greg_a*buoy(i,k)/w2_up(i,k)
!!                ent_rate_up(i,k) = 0._r8
            !end if

            !if( q_up(i,k)<0 ) q_up(i,k) = 0._r8
        !end do

    !end do

end subroutine cal_mse_up



subroutine cal_mse_dn( &
!input
        ent_opt, z, p, t, q, qsat, mse, msesat, kdnlaunch, kdnbase, &
        ent_rate_dn, det_rate_dn, &
!output
        mse_dn, t_dn, q_dn, normassflx_dn, kdntop, zdntop, &
!in/out
        trig)
!input
    integer, intent(in) :: ent_opt
    real(r8), dimension(ncol, nlev), intent(in) :: z      ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: p      ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: t      ! [K]
    real(r8), dimension(ncol, nlev), intent(in) :: q      ! [kg/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: qsat   ! [kg/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: mse    ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: msesat ! [J/kg]
    integer , dimension(ncol), intent(in) :: kdnlaunch  ! [1]
    integer , dimension(ncol), intent(in) :: kdnbase    ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_dn ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_dn ! [1]
!output
    real(r8), dimension(ncol, nlev), intent(inout) :: t_dn  ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(inout) :: q_dn  ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(inout) :: normassflx_dn  ! [J/kg]
    integer , dimension(ncol), intent(out) :: kdntop
    real(r8), dimension(ncol), intent(out) :: zdntop  ! [m]
!input/output
    real(r8), dimension(ncol, nlev), intent(inout) :: mse_dn  ! [J/kg]
    integer , dimension(ncol), intent(inout) :: trig     ! [1]


end subroutine cal_mse_dn


subroutine cal_moisture_up( &
!input
        z, kuplaunch, kuplcl, kupbase, kuptop, &
        rho, q, qsat, q_up, &
        ent_rate_bulk_subcld_up, ent_rate_up, det_rate_up, &
        normassflx_up, landfrac, &
!output
        qliq_up, condrate, rainrate, &
!in/out
        trig)

!------------------------------------------------------
!calculate the cloud levels
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: z ! [m]
    integer , dimension(ncol), intent(in) :: kuplaunch  ! [1]
    integer , dimension(ncol), intent(in) :: kuplcl  ! [1]
    integer , dimension(ncol), intent(in) :: kupbase ! [1]
    integer , dimension(ncol), intent(in) :: kuptop
    real(r8), dimension(ncol), intent(in) :: landfrac ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: rho      ! [K]  environment water vapor
    real(r8), dimension(ncol, nlev), intent(in) :: q        ! [K]  environment water vapor
    real(r8), dimension(ncol, nlev), intent(in) :: qsat     ! [K]  environment water vapor
    real(r8), dimension(ncol, nlev), intent(inout) :: q_up  ! [unit]
    real(r8), dimension(ncol, nlev), intent(in) :: ent_rate_bulk_subcld_up ! [1]
    real(r8), dimension(ncol, nlev), intent(in) :: ent_rate_up ! [1]
    real(r8), dimension(ncol, nlev), intent(in) :: det_rate_up ! [1]
    real(r8), dimension(ncol, nlev), intent(in) :: normassflx_up    ! [K]
!output
    real(r8), dimension(ncol, nlev), intent(out) :: qliq_up   ! [unit]
    real(r8), dimension(ncol, nlev), intent(out) :: condrate  ! [unit]
    real(r8), dimension(ncol, nlev), intent(out) :: rainrate  ! [unit]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local
    real(r8) :: dz
    real(r8) :: intsum
    real(r8) :: inttest

    integer :: i,j,k

!intialize output
    qliq_up = 0._r8
    condrate = 0._r8
    rainrate = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        intsum = 0._r8

        !if ( landfrac(i)==1. ) then
            !c0 = 6.e-4_r8
        !else
            !c0 = 3.e-4_r8
        !end if


!        do k=kupbase(i)-1, kuptop(i), -1
        do k=kuplcl(i), kuptop(i), -1
!        do k=kuplaunch(i)-1, kuptop(i), -1
            dz = z(i,k)-z(i,k+1)
#ifdef SCMDIAG
            !write(*,"(i3,10f20.15)") k, ent_rate_up(i,k)*massflxbase(i)*normassflx_up(i,k)*q(i,k), &
                !-det_rate_up(i,k)*massflxbase(i)*normassflx_up(i,k)*q(i,k), &
                !-massflxbase(i)*( normassflx_up(i,k)*q_up(i,k) &
                !-normassflx_up(i,k+1)*q_up(i,k+1) )/dz, &
                !normassflx_up(i,k)*q_up(i,k) 
#endif
            if( k>=kupbase(i) ) then
                condrate(i,k) = &
                    1/rho(i,k)*( ent_rate_subcld*normassflx_up(i,k)*q(i,k) &
                    -( normassflx_up(i,k)*q_up(i,k) &
                    -normassflx_up(i,k+1)*q_up(i,k+1) )/dz )
                qliq_up(i,k) = &
                     ( normassflx_up(i,k+1)*qliq_up(i,k+1) &
                     + rho(i,k)*dz*condrate(i,k) ) &
                    /( 1 + c0*dz )/( normassflx_up(i,k) )

                !if ( condrate(i,k)<0. ) then
                    !condrate(i,k) = 0._r8
                    !qliq_up(i,k)  = 0._r8
                !end if
            else
                condrate(i,k) = &
                    1/rho(i,k)*( ent_rate_up(i,k)*normassflx_up(i,k)*q(i,k) &
!                    -det_rate_up(i,k)*normassflx_up(i,k)*qsat(i,k) &
                    -det_rate_up(i,k)*normassflx_up(i,k)*q_up(i,k) &
                    -( normassflx_up(i,k)*q_up(i,k) &
                    -normassflx_up(i,k+1)*q_up(i,k+1) )/dz )
                qliq_up(i,k) = &
                     ( normassflx_up(i,k+1)*qliq_up(i,k+1) &
                     + rho(i,k)*dz*condrate(i,k) ) &
                    /( 1 + c0*dz )/( normassflx_up(i,k) )
            end if

            if( condrate(i,k)<0. ) then
!                write(*,*) "negative Q or condensate, entrain too much Q"
                !condrate(i,k) = 0._r8
                !qliq_up(i,k) = 0._r8
                condrate(i,:) = 0._r8
                qliq_up(i,:) = 0._r8
                trig(i) = -6
                exit
            end if

            rainrate(i,k) = 1/rho(i,k)*c0*normassflx_up(i,k)*qliq_up(i,k)

        end do
    end do

end subroutine cal_moisture_up



subroutine solve_ent_rate_up( &
!input
        z, mse, msesat, kuplaunch, kupbase, kuptopmax, &
!output
        ent_rate_up, &
!in/out
        trig)
!------------------------------------------------------
!calculate the cloud levels
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) ::  z      ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: mse     ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: msesat  ! [J/kg]
    integer , dimension(ncol), intent(in) :: kuplaunch  ! [1]
    integer , dimension(ncol), intent(in) :: kupbase    ! [1]
    integer , dimension(ncol), intent(in) :: kuptopmax  ! [1]
!output
    real(r8), dimension(ncol, nlev), intent(out) :: ent_rate_up   ! [1]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]

!local
    real(r8) :: msesat_reach  ! [1]
    real(r8) :: mselaunch     ! [1]
    integer :: i,j,k

    real(r8) :: xa, xb, xc, fa, fb, fc, error
    integer :: n

!intialize output.
    ent_rate_up = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle

        mselaunch = mse( i, kuplaunch(i) )

!Solve the entrainment rates layer by layer
!bisection method
        do k=kupbase(i), kuptopmax(i), -1
            msesat_reach = msesat(i,k)
!solution range 0.-100.
            xa = 0._r8
            xb = 1._r8
            call up_ent_f( z, mse, msesat_reach, &
                kuplaunch(i), kupbase(i), k, &
                xa, fa)
            call up_ent_f( z, mse, msesat_reach, &
                kuplaunch(i), kupbase(i), k, &
                xb, fb)
            if ( fa*fb>0 ) then
                write(*,*) "SCP conv: entrainmetn not solved"
                ent_rate_up(i,k) = 100._r8
                cycle
            end if
            n = 1
            error = abs(xa-xb)
            do while ( (error > 0.000001_r8) .or. (n>100) )
                xc = (xa+xb)/2
                call up_ent_f( z, mse, msesat_reach, &
                    kuplaunch(i), kupbase(i), k, &
                    xc, fc)
                if (fc*fa > 0) then
                    xa = xc
                else
                    xb = xc
                end if
                error = abs(xa-xb)
                n = n + 1
            end do
            ent_rate_up(i,k) = xc
        end do
    end do

end subroutine solve_ent_rate_up

subroutine up_ent_f( &
!input
        z, mse, msesat_reach, &
        klaunch, kbase, kreach, &
        x, &
!output
        f)

!------------------------------------------------------
!calculate the cloud levels
!------------------------------------------------------
!input
    real(r8), dimension(nlev), intent(in) :: z     ! [m]
    real(r8), dimension(nlev), intent(in) :: mse   ! [J/kg]
    real(r8), intent(in) :: msesat_reach  ! [J/kg]

    integer , intent(in) :: klaunch ! [m]
    integer , intent(in) :: kbase  ! [1]
    integer , intent(in) :: kreach   ! [1]

    real(r8), intent(in) :: x   ! [unit]
!output
    real(r8), intent(out) :: f   ! [unit]
!local
    integer :: i,j,k
    real(r8) :: dz    ! [m]
    real(r8) :: zuplaunch ! [m]
    real(r8) :: mselaunch ! [m]

!intialize output.
    f = 0._r8

    zuplaunch = z(klaunch)
    mselaunch  = mse(klaunch)
!Numerical integration in order to solve entrainment rates.
    do k=kbase, kreach, -1
        if ( k == kbase ) then
            dz = z(k)-z( kbase )

            f = f + 0.5*x*(mselaunch-mse(k))*exp( x*( z(k)-z(kreach) ) )*dz
        else
            dz = z(k)-z(k+1)
            f = f + x* 0.5*( (mselaunch-mse(k))*exp( x*( z(k)-z(kreach) ) ) &
                + (mselaunch-mse(k+1))*exp( x*( z(k+1)-z(kreach) ) ) &
                )*dz
        end if

    end do

    f = f - ( mselaunch-msesat_reach )

end subroutine up_ent_f



subroutine cal_endet_ec( &
!input
        qsat, rh, kuplaunch, kuplcl, &
!output
        ent_rate_bulk_up, det_rate_bulk_up, &
!in/out
        trig)
!------------------------------------------------------
!calculate en/detrainment using EC formula
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: qsat  ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: rh    ! [ms-2]
    integer, dimension(ncol), intent(in) :: kuplaunch ! [1]
    integer, dimension(ncol), intent(in) :: kuplcl    ! [1]
!output
    real(r8), dimension(ncol, nlev), intent(out) :: ent_rate_bulk_up  ! [1]
    real(r8), dimension(ncol, nlev), intent(out) :: det_rate_bulk_up  ! [1]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local
    real(r8), dimension(ncol, nlev) :: fscale_up ! [1]
    integer :: i,j,k

!intialize output

    fscale_up = 0._r8
    ent_rate_bulk_up = 0._r8
    det_rate_bulk_up = 0._r8

    !do i=1, ncol
        !if ( trig(i) < 1 ) cycle
        !do k=kupbase(i), kuptop(i), -1
        !end do
    !end do

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        do k = kuplaunch(i), 1, -1
!            fscale_up(i,k) = (qsat(i,k)/qsat(i,kupbase(i)))**3
            fscale_up(i,k) = (qsat(i,k)/qsat(i,kuplcl(i)))**3
        end do
    end do
    ent_rate_bulk_up = e_up_dp*fe_up_dp*(1.3-rh)*fscale_up
    det_rate_bulk_up = d_up_dp*(1.6-rh)
    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        do k = nlev, 1, -1
            if( ent_rate_bulk_up(i,k)<0._r8 ) then
                ent_rate_bulk_up(i,k) = 0._r8
            end if
            if( det_rate_bulk_up(i,k)<0._r8 ) then
                det_rate_bulk_up(i,k) = 0._r8
            end if
        end do
    end do

end subroutine cal_endet_ec 



subroutine cal_cape( &
!input
        dz, buoy, kupbase, kuptop, &
!output
        cape, &
!in/out
        trig)
!------------------------------------------------------
!calculate CAPE given buoyancy
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: dz  ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: buoy  ! [ms-2]
    integer, dimension(ncol), intent(in) :: kupbase ! [1]
    integer, dimension(ncol), intent(in) :: kuptop ! [1]
!output
    real(r8), dimension(ncol), intent(out) :: cape  ! [kgm-2-s]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local
    integer :: i,j,k

!intialize output
    cape = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        do k=kupbase(i), kuptop(i), -1
            cape(i) = cape(i) + dz(i,k)*max(buoy(i,k), 0._r8)
        end do
    end do
end subroutine cal_cape



subroutine cal_tendtransport( &
!input
        z, kuplaunch, kuplcl, kupbase, kuptop, &
        rho, dse, q, dse_up, q_up, &
        normassflx_up, &
!output
        stend, qtend, &
!in/out
        trig)
!------------------------------------------------------
!calculate the feedback tendencies
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: z       ! [m]
    integer, dimension(ncol), intent(in) :: kuplaunch ! [1]
    integer, dimension(ncol), intent(in) :: kuplcl    ! [1]
    integer, dimension(ncol), intent(in) :: kupbase ! [1]
    integer, dimension(ncol), intent(in) :: kuptop ! [1]
    real(r8), dimension(ncol, nlev), intent(in) :: rho      ! [kg/m3]
    real(r8), dimension(ncol, nlev), intent(in) :: dse      ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: q        ! [kg/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: dse_up   ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: q_up  ! [kg/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: normassflx_up  ! [1]
!output
    real(r8), dimension(ncol, nlev), intent(out) :: stend   ! [J/s]
    real(r8), dimension(ncol, nlev), intent(out) :: qtend   ! [kg/kg/s]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local

    integer :: i,j,k
    real(r8) :: dz

!intialize output
    stend = 0._r8
    qtend = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
!        if ( massflxbase(i) <= 0. ) cycle

!lowest level transport
        !do k=kuplaunch(i),nlev
            !dz = z(i, kuplaunch(i) )
            !if( k == kuplaunch(i) ) then
                !stend(i,k) = -( &
                    !normassflx_up(i,k)*( cpair*0.5 )&
                    !)/dz/rho(i,k)
                !qtend(i,k) = -( &
                    !normassflx_up(i,k)*0.5*q(i,k) &
                    !)/dz/rho(i,k)
            !else
                !stend(i,k) = stend(i,k-1)
                !qtend(i,k) = qtend(i,k-1)
            !end if
        !end do

        k = kuplaunch(i)
        dz = z(i,k-1)-z(i,k)
        stend(i,k) = -( &
            normassflx_up(i,k-1)*( dse_up(i,k-1)-dse(i,k-1) )&
            - normassflx_up(i,k)*( dse_up(i,k)-dse(i,k) )&
            )/dz/rho(i,k)
        qtend(i,k) = -( &
            normassflx_up(i,k-1)*( q_up(i,k-1)-q(i,k-1) )&
            - normassflx_up(i,k)*( q_up(i,k)-q(i,k) )&
            )/dz/rho(i,k)

!sub cloud layer transport
!        write(*,*) kuplaunch(i), kuptop(i)
!        do k=kupbase(i)-1, kuptop(i), -1
!        do k=kuplcl(i)-1, kuptop(i), -1
        do k=kuplaunch(i)-1, kuptop(i), -1
            dz = z(i,k)-z(i,k+1)

            stend(i,k) = -( &
                normassflx_up(i,k)*( dse_up(i,k)-dse(i,k) )&
                - normassflx_up(i,k+1)*( dse_up(i,k+1)-dse(i,k+1) )&
                )/dz/rho(i,k)
            qtend(i,k) = -( &
                normassflx_up(i,k)*( q_up(i,k)-q(i,k) )&
                - normassflx_up(i,k+1)*( q_up(i,k+1)-q(i,k+1) )&
                )/dz/rho(i,k)
        end do

    end do

end subroutine cal_tendtransport


subroutine mse2tsat( mse, z, p, t, q, stat)
!------------------------------------------------------
!calculate tempeature and saturated Q given MSE and pressure
!------------------------------------------------------
    real(r8), intent(in) :: mse
    real(r8), intent(in) :: z
    real(r8), intent(in) :: p
    real(r8), intent(out) :: t
    real(r8), intent(out) :: q
    integer, intent(out) :: stat
!local
    real(r8) :: fc, fa, fb, ta, tb, tc, error, torl, tmp
    integer :: n

    stat = 0
    torl = 0.1
    ta = 10.
    tb = 400.
    error = 100.
    tmp = 611*exp( 5417.*(1/273.-1/ta) )
    fa = cpair*ta+gravit*z+latvap*0.622*tmp/p-mse
    tmp = 611*exp( 5417.*(1/273.-1/tb) )
    fb = cpair*tb+gravit*z+latvap*0.622*tmp/p-mse
    
    n = 1
    if( fa*fb>0 ) then
        !if( mse>gravit*z ) then
            !write(*,*) "SCP conv: mse decomposition not solved"
            !write(*,"(a10f25.10)") "mse", mse
            !write(*,"(a10f25.10)") "z", z
            !write(*,"(a10f25.10)") "p", p
            !write(*,"(a10f25.10)") "ta", ta
            !write(*,"(a10f25.10)") "fa", fa
            !write(*,"(a10f25.10)") "tb", tb
            !write(*,"(a10f25.10)") "fb", fb
            !write(*,"(a10f25.10)") "g*z", gravit*z
            !stat = 1
        !end if
        t = 0.
        q = 0.
    else
        do while( (error>torl) .and. (n<100) )
            tc = (ta+tb)/2.
            tmp = 611*exp( 5417.*(1/273.-1/tc) )
            fc = cpair*tc+gravit*z+latvap*0.622*tmp/p-mse
            if (fc*fa>0) then
                ta = tc
            else
                tb = tc
            end if
            error = abs(ta-tb)
!            write(*,*) ta, tb, error
            n = n + 1
        end do
        t = tc
!        write(*,*) t
    end if
    q = 0.622*611/p*exp( 5417.*(1/273.-1/tb) )

end subroutine mse2tsat

subroutine cal_buoy( diffmse, t, q, qsat, dt, dq, buoy )
!------------------------------------------------------
!calculate staturated tempeature given MSE and pressure
!------------------------------------------------------
    real(r8), intent(in) :: diffmse
    real(r8), intent(in) :: t
    real(r8), intent(in) :: q
    real(r8), intent(in) :: qsat
    real(r8), intent(out) :: dt
    real(r8), intent(out) :: dq
    real(r8), intent(out) :: buoy
!local
    real(r8) :: dqsdt, tv, tv_up, q_up

    dt = 0._r8
    dq = 0._r8
    buoy = 0._r8

    dqsdt = (latvap/cpair)* latvap*qsat/rair/(t**2)
    dt = diffmse/(1+dqsdt)/cpair
    dq = dt*dqsdt/latvap*cpair
    tv = t*(1+0.61*q)
    q_up = q+dq
    tv_up = (t+dt)*(1+0.61*q_up)

    buoy = gravit*(tv_up-tv)/tv

end subroutine cal_buoy


subroutine stdout3d( var1, var2, var3, var4, var5, var6)
!------------------------------------------------------
!output to stdout for diagnostics
!------------------------------------------------------
    real(r8), dimension(ncol, nlev), intent(in) :: &
        var1, var2
    real(r8), dimension(ncol, nlev), optional, intent(in) :: &
        var3, var4, var5, var6

    integer i, j, k

    if ( present(var6) ) then
        do i=1, ncol
            do k=1, nlev
                write(*,"(i3,6f20.12)") k, var1(i,k), var2(i,k), var3(i,k), var4(i,k), &
                    var5(i,k), var6(i,k)
            end do
        end do
    else if ( present(var5) ) then
        do i=1, ncol
            do k=1, nlev
                write(*,"(i3,5f20.12)") k, var1(i,k), var2(i,k), var3(i,k), var4(i,k), var5(i,k)
            end do
        end do
    else if ( present(var4) ) then
        do i=1, ncol
            do k=1, nlev
                write(*,"(i3,5f20.12)") k, var1(i,k), var2(i,k), var3(i,k), var4(i,k)
            end do
        end do
    else if ( present(var3) ) then
        do i=1, ncol
            do k=1, nlev
                write(*,"(i3,5f20.12)") k, var1(i,k), var2(i,k), var3(i,k)
            end do
        end do
    end if
end subroutine stdout3d


subroutine stdout3dmix( var1, var2, var3, var4, var5, var6)
!------------------------------------------------------
!output to stdout for diagnostics
!------------------------------------------------------
    real(r8), dimension(ncol, nlev), intent(in) :: var1
    real(r8), dimension(ncol, nlevp), intent(in) :: var2
    real(r8), dimension(ncol, nlevp), optional, intent(in) :: &
        var3, var4, var5, var6

    integer i, j, k

    if ( present(var6) ) then
        do i=1, ncol
            do k=1, nlevp
                write(*,"(i3,6f20.12)") k, var1(i,k), var2(i,k), var3(i,k), var4(i,k), &
                    var5(i,k), var6(i,k)
            end do
        end do
    else if ( present(var4) ) then
        do i=1, ncol
            do k=1, nlev
                write(*,"(a3,i3,2x,20x,f20.12,20x,f20.12)") "cld", k, var2(i,k), var4(i,k)
                write(*,"(5x,i3,f20.12,20x,f20.12)") k, var1(i,k), var3(i,k)
                if (k==nlev) then
                    write(*,"(a3,i3,2x,20x,f20.12,20x,f20.12)") "cld", k+1, var2(i,k+1), var4(i,k+1)
!                    write(*,"(i3,20x,5f20.12)") k+1, var2(i,k+1), var4(i,k+1)
                end if
            end do
        end do
    end if
end subroutine stdout3dmix

end module jp_conv

