    
module conv_jp
!------------------------------------------------------
! Stochastic Convective Parameterization Scheme
! contributors: Xin Xie, Haiyang Yu
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

    public :: conv_jp_init, conv_jp_tend

    integer :: ncol=0, nlev=0, nlevp=0

!physical parameter
    real(r8), parameter :: gravit = 9.80616     ! gravitational acceleration (m/s**2)
    real(r8), parameter :: pi     = 3.141592653 ! Pi
    real(r8), parameter :: cpair  = 1004.64     ! specific heat of dry air (J/K/kg)
    real(r8), parameter :: cpliq  = 4188.       ! specific heat of fresh h2o (J/K/kg)
    real(r8), parameter :: cpwv   = 1810.       ! specific heat of water vapor (J/K/kg)
    real(r8), parameter :: latvap = 2.501e6     ! Latent heat of vaporization (J/kg)
    real(r8), parameter :: latice = 3.34e5
    real(r8), parameter :: epsilo = 0.6219705862 ! ratio of h2o to dry air molecular weights
    real(r8), parameter :: tmelt  = 273.15       ! Freezing point of water (K)
    real(r8), parameter :: rair   = 287.042311365   ! Dry air gas constant     ~ J/K/kg
    real(r8), parameter :: rh2o   = 461.5046398202  ! Water vapor gas constant ~ J/K/kg
    real(r8), parameter :: rhofw  = 1000. ! liquid water density ~ J/K/kg
    real(r8), parameter :: tveps  = 1.0/epsilo - 1


    integer :: ent_opt ! 0=EC, 2=GREG, 3=NSJ

!-------------------------------------
! EC scheme parameter
!-------------------------------------
!parameter for bulk TOTAL fractional en/detrainment rate depending on RH
    real(r8), parameter :: fe_up_dp=1.0_r8, fe_up_sh=2.0_r8
    real(r8), parameter :: e_up_dp=1.25e-3_r8, d_up_dp=0.5e-4_r8
!    real(r8), parameter :: e_up_dp=1.75e-3_r8, d_up_dp=0.75e-4_r8 !original
    real(r8), parameter :: e_up_sh=1.75e-3_r8


!-------------------------------------
! GRE and NSJ parameters
!-------------------------------------
    integer, parameter :: maxiteration = 2
    integer, parameter :: ctopflag = 1  ! 1: B<0; 2: w<0
    integer, parameter :: buoyflag = 1  ! 1: B=Tv'/Tv; 2: B=Tv'/Tv - qliq - qice
    integer, parameter :: mse2tsatflag = 1  ! 1: Taylor; 2: bi-section
    integer, parameter :: mseqiflag = 1  ! 1: use Qi; 0: Qi=0
    integer, parameter :: entratemidflag = 1  ! 1: averaged; 2: recalculated with B and w
    
! updraft lifting formula parameters
    real(r8), parameter :: max_ent_rate = 4.e-3_r8 !paper default 

    !integer,  parameter :: nplume_sh = 0  ! cntr
    !integer,  parameter :: nplume_dp = 15 ! cntr
    integer,  parameter :: nplume_sh = 6
    integer,  parameter :: nplume_dp = 9

    real(r8), parameter :: greg_z0_sh    = 1.e4_r8 !cntr deep

    !real(r8), parameter :: greg_ent_a_sh_beg = 0.3_r8
    !real(r8), parameter :: greg_ent_a_sh_end = 0.3_r8
    !real(r8), parameter :: greg_ce_sh_beg    = 0.8_r8
    !real(r8), parameter :: greg_ce_sh_end    = 0.8_r8
    !real(r8), parameter :: w_up_init_sh_beg = 0.1
    !real(r8), parameter :: w_up_init_sh_end = 1.2

    real(r8), parameter :: greg_ent_a_sh_beg = 0.15_r8
    real(r8), parameter :: greg_ent_a_sh_end = 0.15_r8
    real(r8), parameter :: greg_ce_sh_beg    = 0.8_r8
    real(r8), parameter :: greg_ce_sh_end    = 0.8_r8
    real(r8), parameter :: w_up_init_sh_beg = 0.1
    real(r8), parameter :: w_up_init_sh_end = 1.2


    real(r8), parameter :: greg_z0_dp    = 1.e4_r8 !cntr deep

    real(r8), parameter :: greg_ent_a_dp_beg = 0.15_r8 !cntr
    real(r8), parameter :: greg_ent_a_dp_end = 0.15_r8 !cntr
    real(r8), parameter :: greg_ce_dp_beg    = 0.5_r8  !cntr
    real(r8), parameter :: greg_ce_dp_end    = 0.5_r8  !cntr
    real(r8), parameter :: w_up_init_dp_beg  = 0.2     !cntr
    real(r8), parameter :: w_up_init_dp_end  = 4.      !cntr

    !real(r8), parameter :: greg_ent_a_dp_beg = 0.15_r8
    !real(r8), parameter :: greg_ent_a_dp_end = 0.15_r8
    !real(r8), parameter :: greg_ce_dp_beg    = 0.45_r8
    !real(r8), parameter :: greg_ce_dp_end    = 0.45_r8
    !real(r8), parameter :: w_up_init_dp_beg = 1.
    !real(r8), parameter :: w_up_init_dp_end = 4.

    !real(r8), parameter :: greg_ent_a_dp_beg = 0.3_r8
    !real(r8), parameter :: greg_ent_a_dp_end = 0.15_r8
    !real(r8), parameter :: greg_ce_dp_beg    = 0.8_r8
    !real(r8), parameter :: greg_ce_dp_end    = 0.5_r8
    !real(r8), parameter :: w_up_init_dp_beg = 0.1
    !real(r8), parameter :: w_up_init_dp_end = 6.0

    real(r8) :: greg_ent_a_beg, greg_ent_a_end, greg_ent_a_delta
    real(r8) :: greg_ce_beg, greg_ce_end, greg_ce_delta
    real(r8) :: greg_z0, greg_ent_a, greg_ce

    !real(r8) :: greg_z0    = 1.e4_r8 !cntr deep
    !real(r8) :: greg_ent_a = 0.15_r8 !cntr deep
    !real(r8) :: greg_ce    = 0.5_r8  !cntr deep


    real(r8), parameter :: nsj_ent_a_dp = 0.9_r8     ! cntr deep
    real(r8), parameter :: nsj_coef_dp  = 1.8e-3_r8  ! cntr deep
    real(r8) :: nsj_ent_a = 0.9_r8     ! cntr deep
    real(r8) :: nsj_coef  = 1.8e-3_r8  ! cntr deep

    real(r8) :: w_up_init_beg ! cntr
    real(r8) :: w_up_init_end ! cntr

    integer :: nplume
    integer :: nplume_tot, ind_offset


! rain fraction parameters
    !real(r8), parameter :: rain_z0 = 1500._r8 !paper default
    !real(r8), parameter :: rain_zp = 4000._r8 !paper default
    real(r8), parameter :: rain_z0 = 0._r8     ! cntr deep
    real(r8), parameter :: rain_zp = 1500._r8  ! cntr deep

! cloud ice fraction parameters
    real(r8), parameter :: cloud_t1 = 258.15
    real(r8), parameter :: cloud_t2 = 273.15
    
! downdraft parameters
    real(r8), parameter :: dn_be = 5.e-4_r8
    real(r8), parameter :: dn_ae = 0.3_r8
    real(r8), parameter :: dn_vt = 10._r8
    
! downdraft base massflux ratio to updraft
    real(r8), parameter :: dn_fac = 0.3_r8

! parameter for prognostics mass flux calculation
!    real(r8), parameter :: pmf_alpha = 5.e7_r8, pmf_tau = 1.e3_r8 !paper default
!    real(r8), parameter :: pmf_alpha =2000.e7_r8, pmf_tau=20000.e3_r8
!    real(r8), parameter :: pmf_alpha =1000.e7_r8, pmf_tau=200.e3_r8

    real(r8), parameter :: pmf_alpha_dp = 4000.e7_r8 , pmf_tau_dp = 800.e3_r8 ! cntr deep
    real(r8), parameter :: pmf_alpha_sh = 50000.e7_r8, pmf_tau_sh = 20000.e3_r8
    real(r8) :: pmf_alpha = 4000.e7_r8, pmf_tau = 800.e3_r8 ! cntr deep






    real(r8), parameter :: adjdt = 100._r8

    integer, parameter :: kmaxref = 12
    real, dimension(kmaxref), parameter :: refcape = &
!        (/ 0., 4.42,  14.71,  50.10,  88.80, 155.23, 299.37, 482.90, 848.15, 1535.19, 3157.54, 5000. /)
        (/ 0., 4.42,  14.71,  50.10,  88.80, 155.23, 299.37, 682.90, 1248.15, 1935.19, 3557.54, 5200. /)
    real, dimension(kmaxref), parameter :: refz = &
        (/ 0., 0.90,   2.70,   4.50,   6.30,   8.10,   9.90,  11.70,  13.50,   15.30,   17.10, 50. /)

    real(r8), parameter :: evapke = 0.2e-5_r8





!parameter for converting cloud water to rainfall
    real(r8) :: c0

    real(r8) :: zuplaunchtop  ! max cloud parcel launch height [m]
    real(r8) :: zuplaunchlow  ! min cloud parcel launch height [m]

    real(r8), parameter :: capelmt = 50._r8

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



subroutine conv_jp_init(innlev)
!------------------------------------------------------
!do some initialization.
!------------------------------------------------------
!input
    integer, intent(in) :: innlev
!local


    nlev  = innlev
    nlevp = innlev+1
#ifdef SCMDIAG
    write(*,*) "[conv_jp_init]"
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
    zuplaunchtop = 3000._r8 ! max cloud parcel launch height [m]
    zuplaunchlow = 0._r8  ! min cloud parcel launch height [m]

end subroutine conv_jp_init



subroutine conv_jp_tend( &
!input
        inncol, &
        in_ent_opt, dtime, qmin, &
        lat, landfrac, lhflx, &
        psrf, p, dp, zsrf, z, dz, &
        t, q, bfls_t, bfls_q, &
        omega, pblh, tpert, &
!in/output
        massflxbase_p, &
!output
        jctop, jcbot, &
        stend, qtend, &
        qliqtend,  prec, qliq,  &
        precrate_out, &
!        qliqtend, qicetend, prec, qliq, qice, &
!        rainrate_out, snowrate_out, precrate_out, &
        stendcomp, qtendcomp, &
!diagnostics
        dilucape, bfls_dilucape, &
        outtmp2d, outtmp3d, &
        outmb, outmse, outmsesat, outmseup, &
        outstend, outqtend, &
        outstendcond, outqtendcond, &
        outstendtranup, outqtendtranup, &
        outstendtrandn, outqtendtrandn, &
        outstendevap, outqtendevap  )
!------------------------------------------------------
!Calculate convective tendency
!------------------------------------------------------

! Main Interface
    integer , intent(in) :: inncol ! size of column dimension

    integer , intent(in) :: in_ent_opt ! 0=ec, 1=greg
    real(r8), intent(in) :: dtime  ! [s] time step
    real(r8), intent(in) :: qmin   ! [kg/kg] minimum Q
    real(r8), dimension(inncol), intent(in) :: lat, landfrac, lhflx
    real(r8), dimension(inncol), intent(in) :: psrf, zsrf
    real(r8), dimension(inncol, nlev), intent(in) :: p, dp, z, dz ! [Pa] ; [m]
    real(r8), dimension(inncol, nlev), intent(in) :: t, q ! [K] ; [kg/kg]
       ! T and Q state after the large-scale forcing is applied, current state
    real(r8), dimension(inncol, nlev), intent(in) :: bfls_t, bfls_q ! [K] ; [kg/kg]
       ! T and Q state before the large-scale forcing is applied
    real(r8), dimension(inncol, nlev), intent(in) :: omega ! [m/s]
    real(r8), dimension(inncol), intent(in) :: pblh  ! [m/s]
    real(r8), dimension(inncol), intent(in) :: tpert ! [m/s]
!in/output
    real(r8), dimension(inncol, nlev), intent(inout) :: massflxbase_p !output convective precip[m/s]
!output
    real(r8), dimension(inncol), intent(out) :: jctop
    real(r8), dimension(inncol), intent(out) :: jcbot

    real(r8), dimension(inncol), intent(out) :: prec !output convective precip[m/s]
    real(r8), dimension(inncol, nlev), intent(out) :: stend, qtend, qliqtend
    ! [K/s] ; [kg/kg/s] output tendencies calculated by adding (condensate rate+transport)
    real(r8), dimension(inncol, nlev), intent(out) :: qliq! kg/kg
    real(r8), dimension(inncol, nlev), intent(out) :: precrate_out ! 1/s

!    real(r8), dimension(inncol, nlev), intent(out) :: stend, qtend, qliqtend, qicetend
!    ! [K/s] ; [kg/kg/s] output tendencies calculated by adding (condensate rate+transport)
!    real(r8), dimension(inncol, nlev), intent(out) :: qliq, qice ! kg/kg
!    real(r8), dimension(inncol, nlev), intent(out) :: rainrate_out, snowrate_out, precrate_out ! 1/s

    real(r8), dimension(inncol, nlev), intent(out) :: stendcomp, qtendcomp
    ! [K/s] ; [kg/kg/s] tendencies but calculated using the compensating way
    ! should be same as stend and qtend

!diagnostic output
    real(r8), dimension(inncol), intent(out) :: outmb, outtmp2d
    real(r8), dimension(inncol, nlev), intent(out) :: outtmp3d
    real(r8), dimension(inncol, nlev), intent(out) :: outmse, outmsesat, outmseup

    real(r8), dimension(inncol, nlev), intent(out) :: outstend, outqtend
    real(r8), dimension(inncol, nlev), intent(out) :: outstendcond, outqtendcond
    real(r8), dimension(inncol, nlev), intent(out) :: outstendtranup, outqtendtranup
    real(r8), dimension(inncol, nlev), intent(out) :: outstendtrandn, outqtendtrandn
    real(r8), dimension(inncol, nlev), intent(out) :: outstendevap, outqtendevap


!local
    integer i, j, k, begk, endk, iconv
    real(r8), dimension(inncol, nlev) :: dse !environment [J/kg]
    real(r8), dimension(inncol, nlev) :: mse, msesat ! [J/kg] ; [J/kg]
    real(r8), dimension(inncol, nlev) :: twet !environment [K]
 
    real(r8), dimension(inncol, nlev) :: esat, qsat ! [Pa] ; [kg/kg]
    real(r8), dimension(inncol, nlev) :: rh  ! [1] relative humidity
    real(r8), dimension(inncol, nlev) :: rho ! [kg/m3]

    integer, dimension(inncol) :: trigdp ! [1] 1 trigger ; 0 no trigger
    integer, dimension(inncol) :: trigsh ! [1] 1 trigger ; 0 no trigger

    integer, dimension(inncol) :: kuplaunch ! [1] cloud launching level
    integer, dimension(inncol) :: kuplcl    ! [1] cloud base
    integer, dimension(inncol) :: kupbase   ! [1] cloud base

    integer, dimension(inncol) :: kuptop ! [1] cloud top where buoyancy is zero
    real(r8),dimension(inncol) :: zuptop ! [m] exact z at kuptop


    real(r8), dimension(inncol, nlev)  :: lvmid !
    real(r8), dimension(inncol, nlevp) :: lvint !

!for bulk TOTAL fractional en/detrainment rate depending on vertical velocity
    real(r8), dimension(inncol, nlevp) :: zint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: pint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: tint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: qint ! [1] height at the interface

    real(r8), dimension(inncol, nlevp) :: mseint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: dseint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: esatint, qsatint ! [Pa] ; [kg/kg]
    real(r8), dimension(inncol, nlevp) :: msesatint ! [1] height at the interface

    real(r8), dimension(inncol, nlevp) :: normassflx_up ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlevp) :: normassflx_up_tmp ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlev)  :: ent_rate_bulk_up ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlev)  :: det_rate_bulk_up ! [1] solved PARCEL fractional entrainment rates

    real(r8), dimension(inncol, nlev) :: ent_fc ! [1] an entrainment rate modifier

    real(r8), dimension(inncol, nlevp) :: mse_up ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: dse_up ! [J/kg]  bulk in-cloud DSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: buoy ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: t_up   ! [K]  bulk in-cloud temperatur given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: tv_up  ! [K]  bulk in-cloud temperatur given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: q_up   ! [kg/kg]  bulk in-cloud sat water vapor given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: qliq_up   ! [kg/kg]  bulk in-cloud liquid water given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: qice_up   ! [kg/kg]  bulk in-cloud liquid water given en/detrainment rate.

    real(r8), dimension(inncol, nlev) :: mse_up_mid ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlev) :: t_up_mid ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlev) :: q_up_mid ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlev) :: buoy_mid ! [m/s-2] bulk in-cloud buoyancy
    real(r8), dimension(inncol, nlev) :: normassflx_up_mid ! [1]  bulk normalized updraft mass flux

    real(r8), dimension(inncol, nlev) :: mseqi      ! freezing term in mse equation
    real(r8), dimension(inncol, nlev) :: condrate   ! [m2/kg]  condensation rate.
    real(r8), dimension(inncol, nlev) :: rainrate   ! [1]  bulk precipitation production
    real(r8), dimension(inncol, nlev) :: snowrate   ! [1]  bulk precipitation production
    real(r8), dimension(inncol, nlev) :: precrate   ! [1]  bulk precipitation production

    real(r8), dimension(inncol, nlev)  :: normassflx_dn ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlevp) :: normassflx_dn_tmp ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlev)  :: ent_rate_bulk_dn ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlevp) :: det_rate_bulk_dn ! [1] solved PARCEL fractional entrainment rates

    real(r8), dimension(inncol, nlevp) :: mse_dn ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: t_dn   ! [K]  bulk in-cloud temperatur given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: q_dn   ! [kg/kg]  bulk in-cloud sat water vapor given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: dse_dn ! [J/kg]  bulk in-cloud DSE given en/detrainment rate.

!for evaporation
    real(r8), dimension(inncol, nlev) :: accuprec  ! [1]  bulk precipitation production
    real(r8), dimension(inncol, nlev) :: evaprate  ! [1]  bulk evaporation production

    real(r8), dimension(inncol) :: surfprec      ! [1]  bulk evaporation production
    real(r8), dimension(inncol) :: netprec       ! [1]  bulk evaporation production

!calculated w from Grell's en/detrainment rate scheme
    real(r8), dimension(inncol) :: w_up_init  ! [J/kg]  bulk in-cloud vertical velocity
    real(r8), dimension(inncol, nlev)  :: w_up_mid ! [J/kg]  bulk in-cloud vertical velocity
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

    real(r8),dimension(inncol, nlev) :: mse_up_closure
    real(r8),dimension(inncol, nlev) :: t_up_closure
    real(r8),dimension(inncol, nlev) :: q_up_closure
    real(r8),dimension(inncol, nlev) :: tv_up_closure
    real(r8),dimension(inncol, nlev) :: normassflx_up_closure

    real(r8),dimension(inncol, nlev) :: ent_rate_bulk_up_closure
    real(r8),dimension(inncol, nlev) :: det_rate_bulk_up_closure

    real(r8),dimension(inncol, nlev) :: tv_closure ! [K]
    real(r8),dimension(inncol, nlev) :: buoy_closure  ! [kg/kg] adjusted buoyancy for closure use
    real(r8),dimension(inncol, nlev) :: w_up_closure ! [J/kg]  bulk in-cloud vertical velocity


! These variables are for closure calculation, OR base mass flux
    real(r8),dimension(inncol, nlev) :: w         ! [m/s] environment vertical velocity
    real(r8),dimension(inncol) :: mconv           ! [1] moisture convergence
    real(r8),dimension(inncol) :: conv            ! [1] wind convergence
    real(r8),dimension(inncol, nlev) :: dilucape        ! [1] CAPE cloud work function
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
    real(r8), dimension(inncol, nlevp) :: diffdse_up ! [1]  Delta DSE
    real(r8), dimension(inncol, nlevp) :: diffq_up   ! [1]  Delta Q
    real(r8), dimension(inncol, nlev) :: stendcond  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: qtendcond  ! [K/s] Q tendency
    real(r8), dimension(inncol, nlev) :: stendtran_up  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: qtendtran_up  ! [K/s] Q tendency
    real(r8), dimension(inncol, nlev) :: stendtran_dn  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: qtendtran_dn  ! [K/s] Q tendency
    real(r8), dimension(inncol, nlev) :: qliqtend_det  ! [K/s] liq tendency due to detrainment
    real(r8), dimension(inncol, nlev) :: stendevap  ! [K/s] DSE tendency
    real(r8), dimension(inncol, nlev) :: qtendevap  ! [K/s] Q tendency

    real(r8), dimension(inncol, nlev) :: stendsum
    real(r8), dimension(inncol, nlev) :: qtendsum
    real(r8), dimension(inncol) :: precsum
    real(r8), dimension(inncol) :: massflxbasesum
    real(r8), dimension(inncol, nlevp) :: massflxsum
    real(r8), dimension(inncol, nlevp) :: massflx

    real(r8), dimension(inncol, nlev) :: tmp1stend, tmp1qtend
    real(r8), dimension(inncol, nlev) :: tmp2stend, tmp2qtend

    real(r8), dimension(inncol) :: bg_q
    real(r8), dimension(inncol) :: bg_qtend
    real(r8), dimension(inncol) :: bg_qtendcond
    real(r8), dimension(inncol) :: bg_precrate
    real(r8), dimension(inncol) :: bg_netprecrate
    real(r8), dimension(inncol) :: bg_net
    real(r8), dimension(inncol) :: bg_qtendup
    real(r8), dimension(inncol) :: bg_qtenddn
    real(r8), dimension(inncol) :: bg_qtendevap
    real(r8), dimension(inncol) :: bg_stendcond
    real(r8), dimension(inncol) :: bg_stendup
    real(r8), dimension(inncol) :: bg_stenddn
    real(r8), dimension(inncol) :: bg_stendevap
    real(r8), dimension(inncol) :: bg_qliqtenddet
    real(r8), dimension(inncol) :: bg_qtendsum
    real(r8), dimension(inncol) :: bg_factor

!for test
    real(r8), dimension(inncol) :: tmp ! [1] number of convective lev
    real(r8) :: diffz, dw_up_init
    logical :: flag

!setting the internal dimension size same as the input
    ncol = inncol

    ent_opt = in_ent_opt

!intialize output
!    massflxbase_p = 0._r8
    prec = 0._r8
    stend = 0._r8
    qtend = 0._r8
    qliqtend = 0._r8
    qliq  = 0._r8
    precrate_out = 0._r8

    stendcomp  = 0._r8
    qtendcomp  = 0._r8

    outmb = 0._r8
    outtmp2d = 0._r8
    outtmp3d = 0._r8
    outmse = 0._r8
    outmsesat = 0._r8
    outmseup = 0._r8
    outstend = 0._r8
    outqtend = 0._r8
    outstendcond = 0._r8
    outqtendcond = 0._r8
    outstendtranup = 0._r8
    outqtendtranup = 0._r8
    outstendtrandn = 0._r8
    outqtendtrandn = 0._r8
    outstendevap = 0._r8
    outqtendevap = 0._r8


!zero local variables

    mse = 0._r8
    dse = 0._r8

    qliq_up = 0._r8
    qice_up = 0._r8
    condrate = 0._r8
    rainrate = 0._r8
    snowrate = 0._r8
    precrate = 0._r8
    accuprec = 0._r8
    evaprate = 0._r8

    surfprec = 0._r8

    w = 0._r8
    tv_closure = 0._r8
    q_closure = 0._r8
    buoy_closure = 0._r8

    dilucape = 0._r8
    bfls_dilucape = 0._r8
    dilucape_closure = 0._r8

    stendcond = 0._r8
    qtendcond = 0._r8
    stendtran_up = 0._r8
    qtendtran_up = 0._r8
    stendtran_dn = 0._r8
    qtendtran_dn = 0._r8
    stendevap = 0._r8
    qtendevap = 0._r8
    qliqtend_det = 0._r8

    stendsum = 0._r8
    qtendsum = 0._r8
    precsum = 0._r8
    massflxbasesum = 0._r8
    massflxsum = 0._r8

    massflxbase = 0._r8
    massflxbase_w = 0._r8
    massflxbase_conv  = 0._r8
    massflxbase_mconv = 0._r8
    massflxbase_cape = 0._r8
    massflxbase_clm  = 0._r8
    massflxbase_dcape = 0._r8

!Calculation begins
#ifdef SCMDIAG
    write(*,*) "[conv_jp_tend]"
    call subcol_netcdf_nextstep
#endif


    trigdp = 1
    trigsh = 1

!------------------------------------------------------
!Calculate basic properties
!dse:dry static energy, mse: moist static energy
!esat:saturated water vapor pressure
!qsat:saturated water vapor mixing ratio
!msesat:saturated water vapor moist static energy
!------------------------------------------------------

    do i=1, inncol
        zint(i,nlevp) = max(0., zsrf(i) )
    end do

!estimate z at the interface from z and dz
    do k=nlev,1,-1
        zint(:,k) = zint(:,k+1)+dz(:,k)
    end do

    pint(:,nlevp) = psrf(:)
    do k=nlev,1,-1
        pint(:,k) = pint(:,k+1)-dp(:,k)
    end do




    lvmid = latvap - (cpliq-cpwv) * (t-273.15)
    call cal_qsat2d(t(:,:), p(:,:), qsat(:,:))

    dse = cpair*t + gravit*z
    mse = dse + lvmid*q
    msesat = dse + lvmid*qsat
    rh  = q/qsat
    rho = p/t/rair
    w = -omega/rho/gravit

    twet = (t-273.16)*atan( 0.151977*(rh*100.+8.313659)**0.5 ) + atan(t-273.16+rh*100.) &
        - atan(rh*100.-1.676331) + 0.00391838*((rh*100.)**1.5)*atan(0.023101*rh*100.) - 4.686035 &
        + 273.16

    do i=1, inncol
        do k=1, nlev
            twet(i,k) = min( t(i,k), twet(i,k) )
        end do
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

    lvint = latvap - (cpliq-cpwv) * (tint-273.15)
    call cal_qsat2d(tint, pint, qsatint)

    dseint = cpair*tint + gravit*zint
    mseint = dseint + lvint*qint
    msesatint = dseint + lvint*qsatint


!------------------------------------------------------
!Different methods to calculate entrainment rate
!------------------------------------------------------


    !write(*,*) "before:"
    !write(*,"(a20,50f20.10)") "massflxbase_p:", massflxbase_p(1,:)

    nplume_tot = nplume_sh + nplume_dp

! --- the big loop for dp and sh convection
    do iconv = 1, 2

        trigdp = 1

! do some variable cleaning here
! variables w_up, buoy
        mse_up = 0._r8
        dse_up = 0._r8
        t_up = 0._r8
        q_up = 0._r8
        ent_rate_bulk_up = 0._r8
        det_rate_bulk_up = 0._r8
        normassflx_up = 0._r8

        mse_dn = 0._r8
        dse_dn = 0._r8
        t_dn = 0._r8
        q_dn = 0._r8
        ent_rate_bulk_dn = 0._r8
        det_rate_bulk_dn = 0._r8
        normassflx_dn = 0._r8

        if ( iconv == 1 ) then

            call cal_launchtocldbase( 2, z, zint, p, pint, t, tint, q, qint, qsat, qsatint, &
                mse, mseint, msesat, msesatint, landfrac, lhflx, tpert, &
                kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up, trigdp)

            greg_z0 = greg_z0_sh

            w_up_init_beg = w_up_init_sh_beg
            w_up_init_end = w_up_init_sh_end

            greg_ent_a_beg = greg_ent_a_sh_beg
            greg_ent_a_end = greg_ent_a_sh_end
            greg_ce_beg = greg_ce_sh_beg
            greg_ce_end = greg_ce_sh_end

            pmf_alpha = pmf_alpha_sh
            pmf_tau   = pmf_tau_sh
            nplume = nplume_sh
            ind_offset = 0

            kupbase = kuplaunch

        else if ( iconv == 2 ) then

            call cal_launchtocldbase( 1, z, zint, p, pint, t, tint, q, qint, qsat, qsatint, &
                mse, mseint, msesat, msesatint, landfrac, lhflx, tpert, &
                kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up, trigdp)

            greg_z0 = greg_z0_dp

            w_up_init_beg = w_up_init_dp_beg
            w_up_init_end = w_up_init_dp_end

            greg_ent_a_beg = greg_ent_a_dp_beg
            greg_ent_a_end = greg_ent_a_dp_end
            greg_ce_beg = greg_ce_dp_beg
            greg_ce_end = greg_ce_dp_end

            pmf_alpha = pmf_alpha_dp
            pmf_tau   = pmf_tau_dp
            nplume = nplume_dp
            ind_offset = nplume_sh

            kupbase = kuplcl

!            kupbase = kuplaunch

        end if

        jcbot = kupbase
        jctop = kupbase

        !write(*,*) "iconv:", iconv
        !call stdout3dmix( z, normassflx_up, z, zint )
        !write(*,'(a25,i10,a25,i10)') "kupbase = ", kupbase(1), "kuplaunch = ", kuplaunch(1)

        dse_up = cpair*t_up+gravit*zint

        if (nplume > 1) then
            dw_up_init = (w_up_init_end - w_up_init_beg) / (nplume-1)
            greg_ent_a_delta = ( greg_ent_a_end - greg_ent_a_beg ) / (nplume-1)
            greg_ce_delta    = ( greg_ce_end - greg_ce_beg ) / (nplume-1)
        else
            dw_up_init = 0.0
            greg_ent_a_delta = 0.
            greg_ce_delta = 0.
        end if


        do j = ind_offset+1, ind_offset+nplume

            greg_ent_a = greg_ent_a_beg + (j-ind_offset-1)*greg_ent_a_delta
            greg_ce    = greg_ce_beg + (j-ind_offset-1)*greg_ce_delta

            w_up_init = w_up_init_beg + (j-ind_offset-1) * dw_up_init

#ifdef SCMDIAG 
            write(*,'(a10,i5,a10,f10.5)') "plume:", j, ", w = ",w_up_init
#endif

            !do i=1, inncol
                !if ( trigdp(i)<1 ) cycle
                !mse_up(i, 1:kupbase(i)-1) = mseint(i, 1:kupbase(i)-1)
                !t_up(i, 1:kupbase(i)-1) = tint(i, 1:kupbase(i)-1)
                !q_up(i, 1:kupbase(i)-1) = qint(i, 1:kupbase(i)-1)
            !end do

            normassflx_up_tmp = normassflx_up
            normassflx_dn_tmp = 0._r8

            trigdp = 1

!            write(*,'(a25,i10)') "kupbase ", kupbase(1)
!        call stdout3dmix( z, mseint, z, mse_up )

!updraft properties
            call cal_mse_up( &
                ent_opt, rho, z, zint, dz, p, pint, t, tint, q, qint, qsat, qsatint, &
                mse, mseint, msesat, msesatint, &
                kuplaunch, kupbase, &
                ent_rate_bulk_up, det_rate_bulk_up, w_up_init, &
                mse_up, t_up, q_up, qliq_up, qice_up, mseqi, condrate, rainrate, snowrate, precrate, &
                normassflx_up_tmp, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
                trigdp)

            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                if ( kuptop(i)<jctop(i) ) then
                    jctop(i) = kuptop(i)
                end if
            end do

            dse_up = cpair*t_up+gravit*zint


!downdraft properties
            call cal_mse_dn( &
                ent_opt, kuptop, trigdp, dz, p, rho, t, twet, lvmid, &
                qint, dseint, accuprec, evaprate, &
                dse_dn, q_dn, normassflx_dn_tmp)

            mse_dn = dse_dn + lvint*q_dn

!updraft transport tendency
            call cal_tendtransport( &
                dz, kupbase, kuptop, &
                rho, dseint, qint, dse_up, q_up, &
                normassflx_up_tmp,  &
                stendtran_up, qtendtran_up, &
                trigdp)

!downdraft transport tendency
            call cal_tendtransport( &
                dz, kupbase, kuptop, &
                rho, dseint, qint, dse_dn, q_dn, &
                normassflx_dn_tmp,  &
                stendtran_dn, qtendtran_dn, &
                trigdp)

!liquid detrainment tendency
            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                k = kuptop(i)-1
                qliqtend_det(i,k) = -( &
                    - normassflx_up_tmp(i,k+1)*( qliq_up(i,k+1)+qice_up(i,k+1) )&
                    )/dz(i,k)/rho(i,k)
            end do

!evaporation tendency
            call cal_evap( &
                ent_opt, kuptop, trigdp, dz, p, rho, t, twet, q, &
                precrate, accuprec, surfprec, evaprate )

            stendcond =  latvap*condrate
            qtendcond = -condrate
            stendevap = -latvap*evaprate
            qtendevap =  evaprate

            call cal_cape( &
                dz, buoy_mid, kupbase, kuptop, &
                dilucape(:,j), &
                trigdp)

            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                massflxbase_p(i,j) = min( 0.1, max( 0., &
                    massflxbase_p(i,j) + dtime*( dilucape(i,j)/(2*pmf_alpha) &
!                    massflxbase_p(i,j) + dtime*( max( (dilucape(i,j) - capelmt), 0._r8 )/(2*pmf_alpha) &
                    - massflxbase_p(i,j)/(2*pmf_tau) ) ) )

!            write(*,*) rh(i,nlev)
            !if ( rh(i,nlev)<0.8_r8 ) then
                !trigdp(i) = -20
            !end if

            end do

!            massflxbase = 0.01_r8
            massflxbase(:) = massflxbase_p(:,j)

#ifdef SCMDIAG 
            write(*,'(a25,f10.5,a25,i10)') "massflxbase = ", massflxbase(1), "trigdp = ", trigdp(1)
!            write(*,'(a25,i10,a25,f10.5)') "kuptop = ", kuptop(1), "dilucape=", dilucape(1,j)
!        write(*,'(10f20.10)') dtime, dilucape(1,j), pmf_alpha, dtime*dilucape(1,j)/(2*pmf_alpha)
#endif


            netprec = 0._r8
            do i=1, inncol
                if ( trigdp(i)<1 ) massflxbase(i) = 0._r8
                condrate(i,:) = condrate(i,:) * massflxbase(i)  ! 1/s
                rainrate(i,:) = rainrate(i,:) * massflxbase(i)  ! 1/s
                snowrate(i,:) = snowrate(i,:) * massflxbase(i)  ! 1/s
                precrate(i,:) = precrate(i,:) * massflxbase(i)  ! 1/s
                accuprec(i,:) = accuprec(i,:) * massflxbase(i)  ! kg/m2/s
                evaprate(i,:) = evaprate(i,:) * massflxbase(i)  ! 1/s
                surfprec(i) = surfprec(i) * massflxbase(i) / rhofw  ! m/s

                stendtran_up(i,:) = stendtran_up(i,:)*massflxbase(i)
                qtendtran_up(i,:) = qtendtran_up(i,:)*massflxbase(i)

                stendtran_dn(i,:) = stendtran_dn(i,:) * massflxbase(i) * dn_fac
                qtendtran_dn(i,:) = qtendtran_dn(i,:) * massflxbase(i) * dn_fac

                stendcond(i,:) = stendcond(i,:)*massflxbase(i)
                qtendcond(i,:) = qtendcond(i,:)*massflxbase(i)

                stendevap(i,:) = stendevap(i,:)*massflxbase(i)
                qtendevap(i,:) = qtendevap(i,:)*massflxbase(i)

                qliqtend_det(i,:) = qliqtend_det(i,:)*massflxbase(i)

                massflx(i,:) = normassflx_up_tmp(i,:)*massflxbase(i)

                stend(i,:) = stendcond(i,:) + stendevap(i,:) &
                    + stendtran_up(i,:) + stendtran_dn(i,:)
                qtend(i,:) = qtendcond(i,:) + qtendevap(i,:) &
                    + qtendtran_up(i,:) + qtendtran_dn(i,:)

                do k=1, nlev
                    netprec(i) = netprec(i) - ( qtend(i,k) + qliqtend_det(i,k) )*rho(i,k)*dz(i,k)
                end do
                netprec(i) = netprec(i)/rhofw

            end do


!water budget adjustment
            !bg_q = 0._r8
            !bg_qtend = 0._r8
            !bg_qtendcond = 0._r8
            !bg_precrate= 0._r8
            !bg_netprecrate= 0._r8
            !bg_net = 0._r8
            !bg_qtendup = 0._r8
            !bg_qtenddn = 0._r8
            !bg_qtendevap = 0._r8

            !bg_stendcond = 0._r8
            !bg_stendup = 0._r8
            !bg_stenddn = 0._r8
            !bg_stendevap = 0._r8

            !bg_qliqtenddet = 0._r8
            !bg_factor = 0._r8
            !do i=1, ncol
                !do k=1, nlev

                    !bg_q(i) = bg_q(i) + q(i,k)*rho(i,k)*dz(i,k)

                    !bg_qtend(i) = bg_qtend(i) + qtend(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_qtendcond(i) = bg_qtendcond(i) + qtendcond(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_qtendup(i)   = bg_qtendup(i) + qtendtran_up(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_qtenddn(i)   = bg_qtenddn(i) + qtendtran_dn(i,k)*dtime*rho(i,k)*dz(i,k)

                    !bg_precrate(i) = bg_precrate(i) + precrate(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_netprecrate(i) = bg_netprecrate(i) + ( precrate(i,k)-evaprate(i,k) ) *dtime*rho(i,k)*dz(i,k)

                    !bg_qtendevap(i) = bg_qtendevap(i) + qtendevap(i,k)*dtime*rho(i,k)*dz(i,k)

                    !bg_qliqtenddet(i) = bg_qliqtenddet(i) + qliqtend_det(i,k)*dtime*rho(i,k)*dz(i,k)

                    !bg_net(i) = bg_net(i) - ( qtend(i,k)+qliqtend_det(i,k) )*dtime*rho(i,k)*dz(i,k)

                    !bg_stendcond(i) = bg_stendcond(i) + stendcond(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_stendup(i)   = bg_stendup(i) + stendtran_up(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_stenddn(i)   = bg_stenddn(i) + stendtran_dn(i,k)*dtime*rho(i,k)*dz(i,k)
                    !bg_stendevap(i) = bg_stendevap(i) + stendevap(i,k)*dtime*rho(i,k)*dz(i,k)

                !end do

            !end do
            !write(*,'(a20,f20.10)') 'bg_qtend', bg_qtend(1)
            !write(*,'(a20,f20.10)') 'bg_qtendcond', bg_qtendcond(1)
            !write(*,'(a20,f20.10)') 'bg_precrate', bg_precrate(1)
            !write(*,'(a20,f20.10)') 'bg_netprecrate', bg_netprecrate(1)
            !write(*,'(a20,f20.10)') 'bg_net', bg_net(1)
            !write(*,'(a20,f20.10)') 'bg_qtendup', bg_qtendup(1)
            !write(*,'(a20,f20.10)') 'bg_qtenddn', bg_qtenddn(1)
            !write(*,'(a20,f20.10)') 'bg_qtendevap', bg_qtendevap(1)
            !write(*,'(a20,f20.10)') 'bg_qliqtenddet', bg_qliqtenddet(1)

            !write(*,'(a20,f20.10)') 'bg_stendcond', bg_stendcond(1)
            !write(*,'(a20,f20.10)') 'bg_stendup', bg_stendup(1)
            !write(*,'(a20,f20.10)') 'bg_stenddn', bg_stenddn(1)
            !write(*,'(a20,f20.10)') 'bg_stendevap', bg_stendevap(1)


            do i=1, inncol
                stendsum(i,:) = stendsum(i,:)+stend(i,:)
                qtendsum(i,:) = qtendsum(i,:)+qtend(i,:)

                precsum(i) = precsum(i)+netprec(i)

                massflxbasesum(i) = massflxbasesum(i) + massflxbase(i)
                massflxsum(i,:) = massflxsum(i,:) + massflx(i,:)
            end do


            diffdse_up = 0._r8
            diffq_up = 0._r8
            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                do k=nlevp, kuptop(i), -1
                    diffdse_up(i,k) = normassflx_up_tmp(i,k)*( dse_up(i,k)-dseint(i,k) )
                    diffq_up(i,k)   = normassflx_up_tmp(i,k)*( q_up(i,k)-qint(i,k) )
                end do
            end do


!        write(*,*) j, trigdp(1)

!        write(*,*) 'plume', j, kupbase(1), kuptop(1), trigdp(1)
#ifdef SCMDIAG
!        call stdout3dmix( z, zint, t, t_up )
!        call stdout3dmix( z, zint, t, tint )
!        call stdout3dmix( qliqtend_det, normassflx_up_tmp, qliqtend_det, qice_up )
        !call stdout3dmix( stendcond, zint, stendevap, zint )
!        call stdout3dmix( condrate, zint, evaprate, zint )
!        call stdout3dmix( precrate, zint, evaprate, zint )
!        call stdout3dmix( z, tint, t, t_up )
!        call stdout3dmix( z, msesatint, msesat, mse_up )
!        call stdout3dmix( q, msesatint, t, mse_up )
!        call stdout3dmix( z, dseint, t, dse_up )
!        call stdout3dmix( z, qint, t, q_up )
!        call stdout3dmix( t, zint, twet, t_up )
!        call stdout3dmix( rh, zint, twet, t_up )
!        call stdout3dmix( t, zint, rh, t_up )
!        call stdout3dmix( q, zint, qsat, t_up )

            call subcol_netcdf_putclm( "ent_rate", nlev, ent_rate_bulk_up(1,:), j )

            call subcol_netcdf_putclm( "w_up_mid", nlev, w_up_mid(1,:), j )
            call subcol_netcdf_putclm( "buoy_mid", nlev, buoy_mid(1,:), j )

            call subcol_netcdf_putclm( "w_up_init", 1, w_up_init(1), j )
            call subcol_netcdf_putclm( "w_up", nlevp, w_up(1,:), j )
            call subcol_netcdf_putclm( "buoy", nlevp, buoy(1,:), j )
            call subcol_netcdf_putclm( "mse_up", nlevp, mse_up(1,:), j )
            call subcol_netcdf_putclm( "t_up", nlevp, t_up(1,:), j )
            call subcol_netcdf_putclm( "q_up", nlevp, q_up(1,:), j )
            call subcol_netcdf_putclm( "qliq_up", nlevp, qliq_up(1,:), j )
            call subcol_netcdf_putclm( "qice_up", nlevp, qice_up(1,:), j )
            call subcol_netcdf_putclm( "dse_up", nlevp, dse_up(1,:), j )
            call subcol_netcdf_putclm( "normassflx_up", nlevp, normassflx_up_tmp(1,:), j )


            call subcol_netcdf_putclm( "mse_dn", nlevp, mse_dn(1,:), j )
            call subcol_netcdf_putclm( "normassflx_dn", nlevp, normassflx_dn_tmp(1,:), j )

            call subcol_netcdf_putclm( "dilucape", 1, dilucape(1,j), j )
            call subcol_netcdf_putclm( "mseqi", nlev, mseqi(1,:), j )
            call subcol_netcdf_putclm( "condrate", nlev, condrate(1,:), j )
            call subcol_netcdf_putclm( "rainrate", nlev, rainrate(1,:), j )
            call subcol_netcdf_putclm( "snowrate", nlev, snowrate(1,:), j )
            call subcol_netcdf_putclm( "precrate", nlev, precrate(1,:), j )
            call subcol_netcdf_putclm( "accuprec", nlev, accuprec(1,:), j )
            call subcol_netcdf_putclm( "evaprate", nlev, evaprate(1,:), j )

            call subcol_netcdf_putclm( "stend", nlev, stend(1,:), j )
            call subcol_netcdf_putclm( "qtend", nlev, qtend(1,:), j )
            call subcol_netcdf_putclm( "stendcond", nlev, stendcond(1,:), j )
            call subcol_netcdf_putclm( "qtendcond", nlev, qtendcond(1,:), j )
            call subcol_netcdf_putclm( "stendevap", nlev, stendevap(1,:), j )
            call subcol_netcdf_putclm( "qtendevap", nlev, qtendevap(1,:), j )
            call subcol_netcdf_putclm( "stendtranup", nlev, stendtran_up(1,:), j )
            call subcol_netcdf_putclm( "qtendtranup", nlev, qtendtran_up(1,:), j )
            call subcol_netcdf_putclm( "stendtrandn", nlev, stendtran_dn(1,:), j )
            call subcol_netcdf_putclm( "qtendtrandn", nlev, qtendtran_dn(1,:), j )
            call subcol_netcdf_putclm( "qliqtenddet", nlev, qliqtend_det(1,:), j )

            call subcol_netcdf_putclm( "diffdse_up", nlevp, diffdse_up(1,:), j )
            call subcol_netcdf_putclm( "diffq_up", nlevp, diffq_up(1,:), j )

            call subcol_netcdf_putclm( "massflxbase", 1, massflxbase(1), j )
            call subcol_netcdf_putclm( "massflx", nlevp, massflx(1,:), j )
            call subcol_netcdf_putclm( "prec", 1, surfprec(1), j )

            tmp2d = trigdp
            call subcol_netcdf_putclm( "trigdp", 1, tmp2d(1), j )

#endif
        end do     ! loop of plume

    end do     ! loop of convection(dp/sh)


!for diag only
    mse_up_mid = 0._r8
    t_up_mid = 0._r8
    q_up_mid = 0._r8
    normassflx_up_mid = 0._r8

    do i=1, inncol
        do k=nlev, 1, -1
            if ( (mse_up(i,k)>0._r8) .and. (mse_up(i,k+1)>0._r8) ) then
                mse_up_mid(i,k) = 0.5*( mse_up(i,k) + mse_up(i,k+1) )
                t_up_mid(i,k) = 0.5*( t_up(i,k) + t_up(i,k+1) )
                q_up_mid(i,k) = 0.5*( q_up(i,k) + q_up(i,k+1) )
            end if
            normassflx_up_mid(i,k) = 0.5*( normassflx_up(i,k) + normassflx_up(i,k+1) )
        end do
    end do

        !call subcol_netcdf_putclm( "mse_up_mid", nlev, mse_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "t_up_mid", nlev, t_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "q_up_mid", nlev, q_up_mid(1,:), j )
        !call subcol_netcdf_putclm( "normassflx_up_mid", nlev, normassflx_up_mid(1,:), j )

!use sum as ouput
    prec = precsum
    stend = stendsum
    qtend = qtendsum

    qliqtend = qliqtend_det

#ifdef SCMDIAG
    call subcol_netcdf_putclm( "stendsum", nlev, stendsum(1,:), 1 )
    call subcol_netcdf_putclm( "qtendsum", nlev, qtendsum(1,:), 1 )
    call subcol_netcdf_putclm( "precsum", 1, precsum(1), 1 )
    call subcol_netcdf_putclm( "massflxsum", nlevp, massflxsum(1,:), 1 )
#endif


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
        !qliq_up, condrate, rainrate, &
        !trigdp)

    !dse_up = (cpair*t_up+gravit*z)*convallmask_up

!!------------------------------------------------------
!! Normalized Transport Calculation
!!------------------------------------------------------
    !call cal_tendtransport( &
        !z, kuplaunch, kuplcl, kupbase, kuptop, &
        !rho, dse, q, dse_up, q_up, &
        !normassflx_up,  &
        !stendtran_up, qtendtran_up, &
        !trigdp)
    !!stendcond = latvap*condrate/rho
    !!qtendcond = -condrate/rho
    !stendcond = latvap*condrate
    !qtendcond = -condrate




!!------------------------------------------------------
!!apply the cloud tendencies using unit base mass flux
!!assuming environment change by 1s time
!!and calculate cloud work function again
!!the result is f: cloud work funcion change per unit base mass flux
!!this needs to rerun all the calculations again!!
!!add _closure naming for difference
!!------------------------------------------------------
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

    !q_closure = q + adjdt*( qtendcond+qtendtran_up )
    !t_closure = t + adjdt*( stendcond+stendtran_up )/cpair
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

    !call cal_launchtocldbase( z, p, t_closure, q_closure, mse_closure, landfrac, lhflx, tpert, &
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
        !condrate(i,:) = condrate(i,:)*massflxbase(i)
        !rainrate(i,:) = rainrate(i,:)*massflxbase(i)

        !stendcond(i,:) = stendcond(i,:)*massflxbase(i)
        !qtendcond(i,:) = qtendcond(i,:)*massflxbase(i)
        !stendtran_up(i,:) = stendtran_up(i,:)*massflxbase(i)
        !qtendtran_up(i,:) = qtendtran_up(i,:)*massflxbase(i)

        !if ( trigdp(i)<1 ) cycle

!! Final Tendencies
        !stend(i,:) = stendcond(i,:)+stendtran_up(i,:)
        !qtend(i,:) = qtendcond(i,:)+qtendtran_up(i,:)
        !qliq(i,:)  = qliq_up(i,:)
        !rainrate_out(i,:) = rainrate(i,:)

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
!    qliqtend = 0._r8



!!------------------------------------------------------
!! Another Way of Calculating Output Tendencies
!! tend = condensation rate + transport
!! According to Yanai 1972, 
!! the net effect of both condensatin rate and transnport is equal to
!! a gradient term (he called it compensating warming and drying)
!! + detrainment related terms + evaporation terms.
!! Ideally, we should have
!! stend = stendcond+transtend = stendcomp
!! qtend = qtendcond+tranqtend = qtendcomp
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
            !stendcomp(i,k) = &
                !normassflx_up(i,k)*( dse(i,k)-dse(i,k+1) )/diffz/rho(i,k) &
               !+normassflx_up(i,k)*det_rate_bulk_up(i,k)&
                !*( dse_up(i,k)-dse(i,k) )/rho(i,k)
            !qtendcomp(i,k) = &
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
        !stendcomp(i,:) = stendcomp(i,:)*massflxbase(i)
        !qtendcomp(i,:) = qtendcomp(i,:)*massflxbase(i)

!!for diagnostics
        !tmp1stend(i,:) = tmp1stend(i,:)*massflxbase(i)
        !tmp1qtend(i,:) = tmp1qtend(i,:)*massflxbase(i)
        !tmp2stend(i,:) = tmp2stend(i,:)*massflxbase(i)
        !tmp2qtend(i,:) = tmp2qtend(i,:)*massflxbase(i)

    !end do


!------------------------------------------------------
!make sure no negative q
!------------------------------------------------------

    qcheckout = 1._r8
    minqcheckf = 1._r8

    do i=1, inncol

!whole column adjustment
        minqcheckf = 1._r8
        do k=nlev, 1, -1

            if ( (q(i,k)<=qmin*1.001) .and. (qtend(i,k)<0.) ) then
#ifdef SCMDIAG 
                write(*,*) 'too small Q'
#endif
                minqcheckf = 0._r8
                trigdp(i) = -91
                exit
            end if

            qcheckf = q(i,k)+qtend(i,k)*dtime

            if( qcheckf<qmin ) then
                qcheckf = (qmin*1.001-q(i,k))/dtime/qtend(i,k)
                if( qcheckf<minqcheckf ) then
                    minqcheckf = qcheckf
!                    qcheckf = qtend(i,k)*dtime*minqcheckf
!                    write(*,'(a10,f30.25)') 'qtend ', qcheckf
!                    qcheckf = q(i,k) + qcheckf - qmin
!                    qcheckf = q(i,k) + qtend(i,k)*dtime*minqcheckf - qmin
                    !write(*,'(a10,f30.25)') 'qtend ', qtend(i,k)*minqcheckf*dtime
                    !write(*,*) k, qcheckf
                end if
            end if

        end do

        if( minqcheckf<1._r8 ) then
            massflxbase(i) = minqcheckf*massflxbase(i)

            stendcond(i,:) = minqcheckf*stendcond(i,:)
            qtendcond(i,:) = minqcheckf*qtendcond(i,:)
            stendtran_up(i,:) = minqcheckf*stendtran_up(i,:)
            qtendtran_up(i,:) = minqcheckf*qtendtran_up(i,:)
            stendevap(i,:) = minqcheckf*stendevap(i,:)
            qtendevap(i,:) = minqcheckf*qtendevap(i,:)


            stend(i,:) = minqcheckf*stend(i,:)
            qtend(i,:) = minqcheckf*qtend(i,:)
            prec(i) = minqcheckf*prec(i)
            precrate(i,:) = minqcheckf*precrate(i,:)


            stendcomp(i,:) = minqcheckf*stendcomp(i,:)
            qtendcomp(i,:) = minqcheckf*qtendcomp(i,:)

            tmp1stend(i,:) = minqcheckf*tmp1stend(i,:)
            tmp1qtend(i,:) = minqcheckf*tmp1qtend(i,:)
            tmp2stend(i,:) = minqcheckf*tmp2stend(i,:)
            tmp2qtend(i,:) = minqcheckf*tmp2qtend(i,:)
        end if

!!!single level adjustment
        !!minqcheckf = 1._r8
        !!do k=nlev, 1, -1
            !!if( qcheck(i,k)<=qmin ) then
!!!                qtend(i,k) = (qmin-q(i,k))/dtime
                !!qcheckf = (qmin-q(i,k))/dtime/qtend(i,k)
!!!                stend(i,k) = qcheckf*stend(i,k)
!!!                stendcomp(i,k) = qcheckf*stendcomp(i,k)
                !!qtend(i,k) = qcheckf*qtend(i,k)
                !!qtendcomp(i,k) = qcheckf*qtendcomp(i,k)

                !!!if( (qcheckf<minqcheckf) .and. (qcheckf>0._r8) ) then
                    !!!minqcheckf = qcheckf
!!!                end if
            !!end if
        !!end do

        qcheckout(i) = minqcheckf
    end do


!clean output.
    do i=1, inncol
        if ( trigdp(i)<1 ) then
            mse_up(i,:) = 0._r8
        end if
    end do

    outmb = massflxbasesum

    outtmp2d = qcheckout
!    outtmp2d = bg_qtendsum
!    outtmp2d = lat_coef
!    outtmp2d = capeclm
!    outtmp2d = dilucape
!    outtmp2d zero = capefc
!    outtmp2d = massflxbase_conv
!    outtmp2d = massflxbase
!    outtmp2d = massflxbase_cape/(massflxbase_cape+massflxbase_mconv+massflxbase_w)
!    outtmp2d = massflxbase_dcape
!    outtmp2d = trigdp
!    outtmp2d = landfrac
!    outtmp2d = massflxbase

!    outtmp3d = t
!    outtmp3d = massflxbase_p

    outmse = mse
    outmsesat= msesat
    outmseup = mse_up_mid

    outstend = stend
    outqtend = qtend
    outstendcond = stendcond
    outqtendcond = qtendcond
    outstendtranup = stendtran_up
    outqtendtranup = qtendtran_up
    outstendtrandn = stendtran_dn
    outqtendtrandn = qtendtran_dn
    outstendtrandn = 0._r8
    outqtendtrandn = 0._r8
    outstendevap = stendevap
    outqtendevap = qtendevap



#ifdef SCMDIAG 
    write(*,"(a20,f20.10)") "dtime:", dtime
    write(*,"(a20,f20.10,a20,f20.10,a20,f20.10)") "lat:", lat, "psrf:", psrf
    write(*,"(a20,i4,a20,i4)") "uplaunch:", kuplaunch, " upbase:  ", kupbase, " uplcl:", kuplcl
    write(*,"(a20,i4,a20,i4)") "uptop:", kuptop
    write(*,"(a20,i4,a20,i4)") "trigdp:", trigdp, "trigsh:", trigsh
    write(*,"(a20,f20.10)") "zsrf:",zsrf 
    write(*,"(a20,f20.10)") "bflsdilucape:", bfls_dilucape
    write(*,"(a20,50f20.10)") "dilucape:", dilucape(1,1:nplume_tot)
    write(*,"(a20,f20.10)") "dilucape_closure:", dilucape_closure
    write(*,"(a20,f20.10)") "capefc:", capefc
    write(*,"(a20,f20.10)") "capeclm:", capeclm
    write(*,"(a20,f20.10)") "mconv:", mconv
    write(*,"(a20,50f20.10)") "massflxbase_p:", massflxbase_p(1,1:nplume)
    write(*,"(a20,f20.10)") "massflxbase_cape:", massflxbase_cape
    write(*,"(a20,f20.10)") "massflxbase_dcape:", massflxbase_dcape
    write(*,"(a20,f20.10)") "massflxbase_clm:", massflxbase_clm
    write(*,"(a20,f20.10)") "massflxbase_w:", massflxbase_w
    write(*,"(a20,f20.10)") "massflxbase_mconv:", massflxbase_mconv
    write(*,"(a20,f20.10)") "massflxbase:", massflxbase
    write(*,"(a20,f20.10)") "prec:", prec*3600*24*1000
    write(*,"(a20,f20.10)") "surfprec:", surfprec*3600*24*1000
    write(*,"(a20,f20.10)") "minqcheckf:", minqcheckf


!netcdf output
    call subcol_netcdf_putclm( "mse", nlev, mse(1,:), 1 )
    call subcol_netcdf_putclm( "dse", nlev, dse(1,:), 1 )
    call subcol_netcdf_putclm( "msesat", nlev, msesat(1,:), 1 )
    call subcol_netcdf_putclm( "z", nlev, z(1,:), 1 )
    call subcol_netcdf_putclm( "p", nlev, p(1,:), 1 )
    call subcol_netcdf_putclm( "rho", nlev, rho(1,:), 1 )

    call subcol_netcdf_putclm( "mseint", nlevp, mseint(1,:), 1 )
    call subcol_netcdf_putclm( "msesatint", nlevp, msesatint(1,:), 1 )

    call subcol_netcdf_putclm( "zint", nlevp, zint(1,:), 1 )
    call subcol_netcdf_putclm( "pint", nlevp, pint(1,:), 1 )
    call subcol_netcdf_putclm( "tint", nlevp, tint(1,:), 1 )
    call subcol_netcdf_putclm( "qint", nlevp, qint(1,:), 1 )
    call subcol_netcdf_putclm( "qsatint", nlevp, qsatint(1,:), 1 )

    call subcol_netcdf_putclm( "t", nlev, t(1,:), 1 )
    call subcol_netcdf_putclm( "q", nlev, q(1,:), 1 )
    call subcol_netcdf_putclm( "qsat", nlev, qsat(1,:), 1 )

    !call subcol_netcdf_putclm( "prec", prec(1), 1 )
    !call subcol_netcdf_putclm( "pmassflxbase", massflxbase_p(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_cape", massflxbase_cape(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_w", massflxbase_w(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_mconv", massflxbase_mconv(1), 1 )
    call subcol_netcdf_putclm( "qcheck", 1, qcheckout(1), 1 )

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
      !det_rate_bulk_up, q_up, condrate)
   !call stdout3d( z, ent_rate_bulk_up, &
      !det_rate_bulk_up, normassflx_up)

!   call stdout3d( z, normassflx_up, q_up, condrate)
!   call stdout3d( z, normassflx_up, t_up, dse, dse_up)
!   call stdout3d( z, normassflx_up, t, t_up, tv_closure)

!   call stdout3d( z, w_up, buoy, ent_rate_bulk_up, normassflx_up )
!   call stdout3d( z, dz, buoy)
!   call stdout3d( z, q, q_up, mse_up, condrate)
!    call stdout3dmix( dz, zint, dp, pint )
!    call stdout3dmix( mse, mse_up, buoy_mid, buoy )
!    call stdout3dmix( ent_rate_bulk_up, buoy, dz, normassflx_up )
!    call stdout3dmix( z, zint, buoy_mid, normassflx_up )
!    call stdout3dmix( w_up_mid, mse_up, msesat, w_up )
!    call stdout3dmix( q, qint, t, tint )

!   call stdout3d( z, q_up, normassflx_up, qliq_up, condrate )
!   call stdout3d( z, normassflx_up, q, qsat, q_up)
!   call stdout3d( z, normassflx_up, mse, msesat, mse_up)
!   call stdout3d( z, dse, dse_up, normassflx_up, stend )
!   call stdout3d( z, q, q_up, normassflx_up, qtend )
!   call stdout3d( z, normassflx_up, q_up, qtend, qcheck)
!   call stdout3d( z, normassflx_up, q_up, q)
!   call stdout3d( z, normassflx_up, q_up, qsat, condrate)
!   call stdout3d( z, condrate, rainrate, evaprate, qliqtend )
!   call stdout3d( z, stend, stendcomp, qtend, qtendcomp, normassflx_up)

#endif

end subroutine conv_jp_tend



subroutine cal_launchtocldbase( &
!input
        opt, z, zint, p, pint, t, tint, q, qint, qsat, qsatint, mse, mseint, msesat, msesatint, landfrac, lhflx, tpert, &
!output
        kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up,  &
!in/out
        trig)
!------------------------------------------------------
!launch to LCL, no entrainment up, in-cloud properties
!------------------------------------------------------
!input
    integer, intent(in) :: opt ! 1:LCL  2:launch point
    real(r8), dimension(ncol, nlev),  intent(in) :: z     ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: zint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: p     ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: pint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: t     ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: tint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: q     ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: qint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: qsat  ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: qsatint! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: mse   ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: mseint ! [J/kg]
    real(r8), dimension(ncol, nlev),  intent(in) :: msesat ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: msesatint ! [J/kg]
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
        do k=nlevp, 1, -1
            if ( zint(i,k) >= zuplaunchlow ) then
                kuplaunchmin(i) = k
                exit
            end if
        end do
        do k=nlevp, 1, -1
            if ( zint(i,k) >= zuplaunchtop ) then
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
            if ( mseint(i,k) >= msemax ) then
                msemax = mseint(i,k)
                kuplaunch(i) = k
            end if
        end do

        perturbt = 0._r8
        perturbq = 0._r8

        t_up(i,kuplaunch(i) ) = tint(i,kuplaunch(i) )+perturbt
        q_up(i,kuplaunch(i) ) = qint(i,kuplaunch(i) )+perturbq
        mse_up(i,kuplaunch(i) ) = mseint( i,kuplaunch(i) )+perturbt*cpair+perturbq*latvap

        do k=kuplaunch(i)-1, 1, -1

            mse_up(i,k) = mse_up(i,k+1)
            
            call cal_mse2tsat(mse_up(i,k), tint(i,k), qsatint(i,k), msesatint(i,k), t_up(i,k))
            call cal_qsat(t_up(i,k), pint(i,k), q_up_test)
        
            if( q_up(i,k+1)>q_up_test ) then
                kuplcl(i) = k
                t_up(i,k) = tint(i,k)
                q_up(i,k) = qsatint(i,k)
                mse_up(i,k) = cpair*t_up(i,k) + gravit*zint(i,k) + &
                    (latvap-(cpliq-cpwv)*(t_up(i,k)-273.15))*q_up(i,k)
                exit
            else
                t_up(i,k) = ( mse_up(i,k)-latvap*q_up(i,k)-gravit*zint(i,k)-(cpliq-cpwv)*273.15*q_up(i,k) ) &
                    / (cpair-(cpliq-cpwv)*q_up(i,k))
                q_up(i,k) = q_up(i,k+1)
            end if

        end do

        if ( opt == 1 ) then
            do k=nlevp, kuplcl(i), -1
                normassflx_up(i,k) = (zint(i,k)/zint(i,kuplcl(i)))**0.5
                mse_up(i,k) = mse_up(i, kuplcl(i))
                q_up(i,k) = q_up(i, kuplcl(i))
                t_up(i,k) = ( mse_up(i,k) - gravit*zint(i,k) - (latvap+(cpliq-cpwv)*273.15)*q_up(i,k) )/ &
                    (cpair-(cpliq-cpwv)*q_up(i,k))
            end do
        else
            if ( kuplaunch(i) == nlevp ) then
                kuplaunch(i) = kuplaunch(i)-1
            end if
            mse_up(i, kuplaunch(i) ) = msesatint(i, kuplaunch(i) )
            t_up(i, kuplaunch(i) ) = tint(i, kuplaunch(i) )
            q_up(i, kuplaunch(i) ) = qsatint(i, kuplaunch(i) )
            do k=nlevp, kuplaunch(i), -1
                normassflx_up(i,k) = (zint(i,k)/zint(i,kuplaunch(i)))**0.5
                mse_up(i,k) = mse_up(i, kuplaunch(i))
                q_up(i,k) = q_up(i, kuplaunch(i))
                t_up(i,k) = ( mse_up(i,k) - gravit*zint(i,k) - (latvap+(cpliq-cpwv)*273.15)*q_up(i,k) )/ &
                    (cpair-(cpliq-cpwv)*q_up(i,k))
            end do
        end if
        normassflx_up(i,nlevp) = 0._r8

    end do

end subroutine cal_launchtocldbase


subroutine cal_mse_up( &
!input
        ent_opt, rho, z, zint, dz, p, pint, t, tint, q, qint, qsat, qsatint, &
        mse, mseint, msesat, msesatint, kuplaunch, kupbase, &
        ent_rate_up_mid, det_rate_up, w_up_init, &
!in/output
        mse_up, t_up, q_up, qliq_up, qice_up, mseqi, condrate, rainrate, snowrate, precrate, &
        normassflx_up, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
        trig)

!input
    integer, intent(in) :: ent_opt
    real(r8), dimension(ncol, nlev),  intent(in) :: rho   ! [kg/m3]
    real(r8), dimension(ncol, nlev),  intent(in) :: z     ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: zint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: dz    ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: p     ! [m]
    real(r8), dimension(ncol, nlevp), intent(in) :: pint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: t     ! [K]
    real(r8), dimension(ncol, nlevp), intent(in) :: tint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: q     ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: qint  ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: qsat   ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: qsatint! [kg/kg]
    real(r8), dimension(ncol, nlev),  intent(in) :: mse    ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in):: mseint ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in) :: msesat ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in):: msesatint ! [J/kg]
    integer , dimension(ncol), intent(in) :: kuplaunch ! [1]
    integer , dimension(ncol), intent(in) :: kupbase    ! [1]

    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_up_mid ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_up ! [1]
    real(r8), dimension(ncol), intent(inout) :: w_up_init ! [1]
!output
    real(r8), dimension(ncol, nlevp), intent(out) :: w_up ! [J/kg]
    real(r8), dimension(ncol, nlev),  intent(out) :: w_up_mid ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: buoy ! [J/kg]
    real(r8), dimension(ncol, nlev),  intent(out) :: buoy_mid  ! [J/kg]

    integer , dimension(ncol), intent(out) :: kuptop
    real(r8), dimension(ncol), intent(out) :: zuptop  ! [m]
!input/output
    real(r8), dimension(ncol, nlevp), intent(inout) :: mse_up  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: t_up  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: q_up  ! 
    real(r8), dimension(ncol, nlevp), intent(inout) :: qliq_up  ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: qice_up  ! [kg/kg]
    real(r8), dimension(ncol, nlev ), intent(inout) :: mseqi    ! 
    real(r8), dimension(ncol, nlev ), intent(inout) :: condrate  ! [m2/kg]
    real(r8), dimension(ncol, nlev ), intent(inout) :: rainrate  ! [m2/kg]
    real(r8), dimension(ncol, nlev ), intent(inout) :: snowrate  ! [m2/kg]
    real(r8), dimension(ncol, nlev ), intent(inout) :: precrate  ! [m2/kg]

    real(r8), dimension(ncol, nlevp), intent(inout) :: normassflx_up  ! [J/kg]
    integer , dimension(ncol), intent(inout) :: trig     ! [1]
!local
    real(r8), dimension(ncol, nlevp)  :: ent_rate_up ! [1]
    real(r8) :: tv, tv_up,  w2, qw, Fp, Fi, Ek
    real(r8) :: buoy_l, buoy_h

    integer :: i,j,k, iteration
    integer :: ngbuoy

    !intialize output.
    qliq_up = 0.0
    qice_up = 0.0
    mseqi = 0.0
    condrate = 0.0
    rainrate = 0.0
    snowrate = 0.0
    precrate = 0.0
    
    w_up = 0._r8
    w_up_mid = 0._r8
    buoy = 0._r8
    buoy_mid = 0._r8
    kuptop = nlev
    zuptop = 0._r8
    ngbuoy = 0

    ent_rate_up = 0._r8
    det_rate_up = 0._r8

! ------------------------
! cloud base diagram
! ------------------------  kupbase-1
!
!        ----------         kupbase-1
!
! ------------------------  kupbase (*)
!
!        ----------         kupbase
!
! ------------------------  kupbase+1
!
!            ...
!
! ------------------------  nlev
!
!        ----------         nlev
!
! ------------------------  nlevp

    do i=1, ncol
        if ( trig(i) < 1 ) cycle

        !do k=nlevp, kupbase(i)+1, -1
            !tv = tint(i,k)*(1+tveps*qint(i,k))
            !tv_up = t_up(i,k)*(1 + tveps*q_up(i,k) )
            !buoy(i,k) = gravit*(tv_up-tv)/tv
            !if ( k<=nlev ) then
                !buoy_mid(i,k) = 0.5*( buoy(i,k)+buoy(i,k+1) )
            !end if
        !end do

        k = kupbase(i)

        tv = tint(i,k)*(1+tveps*qint(i,k))
        tv_up = t_up(i,k)*(1 + tveps*q_up(i,k) )
        buoy(i,k) = gravit*(tv_up-tv)/tv 

        w_up(i, kupbase(i) )  = w_up_init(i)

        !if ( buoy(i,k)<0. ) then
            !trig(i) = -10
            !cycle
        !end if

        if ( ent_opt == 2 ) then
            ent_rate_up(i,k) = greg_ce*greg_ent_a*buoy(i,k)/w_up(i,k)/w_up(i,k)
        else if ( ent_opt == 3 ) then
            ent_rate_up(i,k) = nsj_coef/w_up(i,k) 
        else
            ent_rate_up(i,k) = 0
        end if
        ent_rate_up(i,k) = max(0.0, min( max_ent_rate,  ent_rate_up(i,k)))

        !write(*,*) k
        !write(*,"(10f20.10)") tv, tv_up, tint(i,k), qint(i,k), buoy(i,k), tv_up, tv, ent_rate_up(i,k)

#ifdef SCMDIAG 
        !write(*,"(i3,10f15.6)") k, mse_up(i,k), ent_rate_up_l, buoy(i,k), w_up(i,k)
        !write(*,"(a3,10a15)") 'L', 'normassflx_up', 'normassflx_up' &
            !, 'mse_up(L)', 'mse_up(H)', 'msesat', 'ent_rate_up_l', 'dz', 'buoy' &
            !, 'ent_rate_up', 'w_up'
#endif

        do k=kupbase(i)-1, 1, -1

!            write(*,*) k
        
            do iteration = 1, maxiteration, 1
                if (iteration == 1) then
                    w_up_mid(i,k) = w_up(i,k+1)
                    buoy_mid(i,k) = buoy(i,k+1)
                    ent_rate_up_mid(i,k) = ent_rate_up(i,k+1)
                else
                    w_up_mid(i,k) = 0.5 * ( w_up(i,k)+w_up(i,k+1) )
                    buoy_mid(i,k) = 0.5 * ( buoy(i,k)+buoy(i,k+1) )
                    
                    if (entratemidflag == 1) then
                        ent_rate_up_mid(i,k) = 0.5 * ( ent_rate_up(i,k) + ent_rate_up(i,k+1) )
                    end if
                    if (entratemidflag == 2) then
                        if ( ent_opt == 2 ) then
                            ent_rate_up_mid(i,k) = &
                                greg_ce*greg_ent_a*buoy_mid(i,k)/w_up_mid(i,k)/w_up_mid(i,k)
                        else if ( ent_opt == 3 ) then
                            ent_rate_up_mid(i,k) = nsj_coef/w_up_mid(i,k) 
                        else
                            ent_rate_up_mid(i,k) = 0
                        end if
                        ent_rate_up_mid(i,k) = max(0.0, &
                            min( max_ent_rate,  ent_rate_up_mid(i,k)))
                    end if
                end if

                if ( ent_opt == 2 ) then
                    w2 = ( 2*greg_ent_a*(1-greg_ce)*buoy_mid(i,k) + &
                           w_up(i,k+1)*w_up(i,k+1)/dz(i,k) ) / &
                           (1.0/dz(i,k) + 1.0/greg_z0)
                else if ( ent_opt == 3 ) then
                    w2 = w_up(i,k+1)*w_up(i,k+1) + 2*dz(i,k)*( &
                            nsj_ent_a*buoy_mid(i,k)-nsj_coef*w_up(i,k+1) )
                else
                    w2 = w_up(i,k+1)**2
                end if                
                w_up(i,k)  = sqrt(max(w2, 0.0))

                normassflx_up(i,k) = normassflx_up(i,k+1)*exp(ent_rate_up_mid(i,k)*dz(i,k) )
                Ek = ( normassflx_up(i,k) - normassflx_up(i,k+1) ) / dz(i,k)
                
                mse_up(i,k) = ( normassflx_up(i,k+1)*mse_up(i,k+1) &
                                + Ek*mse(i,k)*dz(i,k) + mseqi(i,k)*dz(i,k) ) &
                                /normassflx_up(i,k)

!                write(*,*) iteration, ent_rate_up_mid(i,k), normassflx_up(i,k)
                
            !----method 1: Taylor expanding ------------------------------------------------
                if (mse2tsatflag == 1) then
                    call cal_mse2tsat(mse_up(i,k), tint(i,k), &
                        qsatint(i,k), msesatint(i,k), t_up(i,k) )
                    call cal_qsat(t_up(i,k), pint(i,k), q_up(i,k))
                end if
            !----method 2: bi-section ------------------------------------------------------
                if (mse2tsatflag == 2) then
                    call mse2tsat( mse_up(i,k), zint(i,k), pint(i,k), t_up(i,k), q_up(i,k) )
                end if
            !-------------------------------------------------------------------------------

                condrate(i,k) = 1.0/rho(i,k)*( Ek*q(i,k) &
                    -( normassflx_up(i,k)*q_up(i,k) &
                    -normassflx_up(i,k+1)*q_up(i,k+1) )/dz(i,k) )

                Fp = max(0.0, 1.0 - exp(-(z(i,k) - zint(i,kupbase(i)) - rain_z0)/rain_zp) )
!                fp = 1._r8
                qw = (normassflx_up(i,k+1)*(qliq_up(i,k+1)+qice_up(i,k+1)) + &
                    rho(i,k)*(1.0-Fp)*condrate(i,k)*dz(i,k) )/normassflx_up(i,k)

                Fi = 0.0
                if (cloud_t1 < t_up(i,k) .and. t_up(i,k) < cloud_t2) then
                    Fi = (cloud_t2-t_up(i,k)) / (cloud_t2-cloud_t1)
                else if (t_up(i,k) <= cloud_t1) then
                    Fi = 1.0
                end if

                qliq_up(i,k) = (1.0-Fi) * qw
                qice_up(i,k) = Fi * qw
                
                rainrate(i,k) = (1.0-Fi) * Fp * condrate(i,k)
                snowrate(i,k) = Fi * Fp * condrate(i,k)
                precrate(i,k) = rainrate(i,k) + snowrate(i,k)

                if (iteration < maxiteration) then
                    if (mseqiflag > 0) then
                        mseqi(i,k) = latice * (normassflx_up(i,k)*qice_up(i,k) - &
                            normassflx_up(i,k+1)*qice_up(i,k+1)) / dz(i,k)                
                    else
                        mseqi(i,k) = 0
                    end if
                end if

                tv = tint(i,k)*(1+tveps*qint(i,k))
                tv_up = t_up(i,k)*( 1+tveps*q_up(i,k) )
                
                if (buoyflag == 1) then
                    buoy(i,k) = gravit* ( (tv_up-tv)/tv ) 
                end if
                if (buoyflag == 2) then
                    buoy(i,k) = gravit* ( (tv_up-tv)/tv - qliq_up(i,k) - qice_up(i,k) ) 
                end if

                if ( ent_opt == 2 ) then
                    ent_rate_up(i,k) = greg_ce*greg_ent_a*buoy(i,k)/w_up(i,k)/w_up(i,k)
                else if ( ent_opt == 3 ) then
                    ent_rate_up(i,k) = nsj_coef/w_up(i,k) 
                else
                    ent_rate_up(i,k) = 0.0
                end if
                ent_rate_up(i,k) = max(0.0, min( max_ent_rate,  ent_rate_up(i,k)))                
            end do   ! loop of iteration
            
            if (ctopflag == 1) then 
                if (buoy(i,k) < 0.0) then
                    exit
                end if
            end if
            
            if (ctopflag == 2) then
                if (w_up(i,k) < 0.1) then
                    exit
                end if
            end if

            if ( condrate(i,k)<0 ) then
                condrate(i,k) = 0
                exit
            end if

        end do  ! loop for levels

        if (k>=1) then
            mse_up(i,k) = mseint(i,k)
            t_up(i,k) = tint(i,k)
            q_up(i,k) = qint(i,k)
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8
            w_up(i,k) = 0._r8
            buoy(i,k) = 0._r8
            ent_rate_up(i,k) = 0._r8
            det_rate_up(i,k) = 0._r8
            normassflx_up(i,k) = 0._r8

            condrate(i,k) = 0._r8
            rainrate(i,k) = 0._r8
            snowrate(i,k) = 0._r8
            precrate(i,k) = 0._r8
            mseqi(i,k) = 0._r8
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8

            ent_rate_up(i,kupbase(i) ) = 0._r8

            kuptop(i) = k+1

!not penetrating more than one level
            if ( k == kupbase(i)-1 ) then
                trig(i) = -11
            end if
        else
            k = 1
            mse_up(i,k) = mseint(i,k)
            t_up(i,k) = tint(i,k)
            q_up(i,k) = qint(i,k)
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8
            w_up(i,k) = 0._r8
            buoy(i,k) = 0._r8
            ent_rate_up(i,k) = 0._r8
            det_rate_up(i,k) = 0._r8
            normassflx_up(i,k) = 0._r8

            condrate(i,k) = 0._r8
            rainrate(i,k) = 0._r8
            snowrate(i,k) = 0._r8
            precrate(i,k) = 0._r8
            mseqi(i,k) = 0._r8
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8

            ent_rate_up(i,kupbase(i) ) = 0._r8

            kuptop(i) = 2

        end if

    end do

end subroutine cal_mse_up


subroutine cal_mse_dn( &
!input
        ent_opt, kuptop, trig, dz, p, rho, t, twet, lvmid, &
        qint, dseint, accuprec, evaprate, &
!output
        dse_dn, q_dn, normassflx_dn )

!input
    integer, intent(in) :: ent_opt
    integer , dimension(ncol), intent(in) :: trig     ! [1]
    integer , dimension(ncol), intent(in) :: kuptop    ! [1]
    real(r8), dimension(ncol, nlev),  intent(in) :: dz    ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: p     ! [Pa]
    real(r8), dimension(ncol, nlev),  intent(in) :: rho   ! [kg/m3]
    real(r8), dimension(ncol, nlev),  intent(in) :: t     ! [K]
    real(r8), dimension(ncol, nlev), intent(in)  :: twet  ! [K]
    real(r8), dimension(ncol, nlev), intent(in)  :: lvmid  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: qint  ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: dseint  ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in)  :: accuprec ! [#]
    real(r8), dimension(ncol, nlev), intent(in)  :: evaprate ! [1/m]

!output
    real(r8), dimension(ncol, nlevp), intent(inout) :: dse_dn  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: q_dn  ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: normassflx_dn  ! [kg/m2/s]

!local
    integer :: i,j,k

    dse_dn = 0._r8
    q_dn = 0._r8
    normassflx_dn = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        k = kuptop(i)

        normassflx_dn(i,k) = -1.0_r8
        q_dn(i,k) = qint(i,k)
        dse_dn(i,k) = dseint(i,k)

        do k=kuptop(i), nlev
            normassflx_dn(i,k+1) = normassflx_dn(i,k) + dn_be*rho(i,k) * &
                max(0.0, twet(i,k)-t(i,k)) * accuprec(i,k) * dz(i,k)
            dse_dn(i,k+1) = (normassflx_dn(i,k)*dse_dn(i,k) + &
                lvmid(i,k)*evaprate(i,k)*dz(i,k)) / normassflx_dn(i,k+1)
            q_dn(i,k+1) = (normassflx_dn(i,k)*q_dn(i,k) - &
                evaprate(i,k)*dz(i,k)) / normassflx_dn(i,k+1)
        end do
        
        normassflx_dn(i,nlev+1) = 0._r8

!        do k=kuptop(i), nlev
!            ent_rate_dn(i,k) = -dn_be*rho(i,k)*( twet(i,k)-t(i,k) )*rho(i,k)*rainrate(i,k)
!            qsat_tmp = 0.622*611.2*exp(5417*(1/273.16-1/twet(i,k)))/p(i,k)
!            evap(i,k) = dn_ae*( qsat_tmp-q(i,k) )*rho(i,k)*rainrate(i,k)/dn_vt
!!            write(*,'(10f20.15)') qsat_tmp, q(i,k), rainrate(i,k), evap(i,k)
!        end do

!        do k=kuptop(i)+1, nlevp
!            normassflx_dn(i,k) = normassflx_dn(i,k-1)-ent_rate_dn(i,k-1)*dz(i,k-1)
!            dse_dn(i,k) = ( normassflx_dn(i,k-1)*dse_dn(i,k-1)+latvap*evap(i,k-1)*dz(i,k-1) ) &
!                /normassflx_dn(i,k)
!            t_dn(i,k) = ( dse_dn(i,k)-gravit*zint(i,k) )/cpair
!            qsat_tmp = 0.622*611.2*exp(5417*(1/273.16-1/t_dn(i,k)))/pint(i,k)
!            q_dn(i,k) = ( normassflx_dn(i,k-1)*q_dn(i,k-1)-latvap*evap(i,k-1)*dz(i,k-1) ) &
!                /normassflx_dn(i,k)
!            mse_dn(i,k) = dse_dn(i,k) + latvap*qsat_tmp
!            write(*,'(i3,10f20.10)') k, qsat_tmp, q_dn(i,k)
!        end do
    end do


end subroutine cal_mse_dn



subroutine cal_evap( &
!input
       ent_opt, kuptop, trig, dz, p, rho, t, twet, q, &
       precrate, &
!output
       accuprec, surfprec, evaprate )

!input
    integer, intent(in) :: ent_opt
    integer , dimension(ncol), intent(in) :: kuptop    ! [1]
    integer , dimension(ncol), intent(in) :: trig     ! [1]
    real(r8), dimension(ncol, nlev),  intent(in) :: dz    ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: p     ! [Pa]
    real(r8), dimension(ncol, nlev),  intent(in) :: rho   ! [kg/m3]
    real(r8), dimension(ncol, nlev),  intent(in) :: t     ! [K]
    real(r8), dimension(ncol, nlev),  intent(in) :: twet  ! [K]
    real(r8), dimension(ncol, nlev),  intent(in) :: q     ! [kg/kg]
    real(r8), dimension(ncol, nlev),  intent(in) :: precrate ! [m2/kg]
!output
    real(r8), dimension(ncol, nlev), intent(out) :: accuprec  ! [#]
    real(r8), dimension(ncol, nlev), intent(out) :: evaprate  ! [m2/kg]
    real(r8), dimension(ncol), intent(out) :: surfprec  ! [#]

!local
    real(r8) :: qsat_tmp
    integer :: i,j,k

    evaprate = 0._r8
    accuprec = 0._r8
    surfprec  = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        do k=kuptop(i), nlev
            accuprec(i,k)  = accuprec(i,k-1) + rho(i,k)*precrate(i,k)*dz(i,k)
            call cal_qsat( twet(i,k), p(i,k), qsat_tmp )
            !evaprate(i,k) = dn_ae*max( 0._r8, qsat_tmp-q(i,k) ) * &
                    !accuprec(i,k) / dn_vt / rho(i,k)
            !accuprec(i,k) = max(0._r8, accuprec(i,k) - evaprate(i,k)*rho(i,k)*dz(i,k) )

            evaprate(i,k) = min( dn_ae*max( 0._r8, qsat_tmp-q(i,k) ) * &
                    accuprec(i,k) / dn_vt / rho(i,k), accuprec(i,k)/rho(i,k)/dz(i,k) )
            accuprec(i,k) = accuprec(i,k) - evaprate(i,k)*rho(i,k)*dz(i,k)

        end do
        surfprec(i) = accuprec(i,nlev)
    end do

end subroutine cal_evap



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
        dz, buoy_mid, kupbase, kuptop, &
!output
        cape, &
!in/out
        trig)
!------------------------------------------------------
!calculate CAPE given buoyancy
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: dz  ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: buoy_mid  ! [ms-2]
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
        do k=kupbase(i)-1, kuptop(i), -1
            cape(i) = cape(i) + dz(i,k)*max(buoy_mid(i,k), 0._r8)
        end do
    end do
end subroutine cal_cape



subroutine cal_tendtransport( &
!input
        dz, kupbase, kuptop, &
        rho, dseint, qint, dse_up, q_up, &
        normassflx_up,  &
!output
        stend, qtend, &
!in/out
        trig)
!------------------------------------------------------
!calculate the feedback tendencies
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: dz       ! [m]
    integer, dimension(ncol), intent(in) :: kupbase ! [1]
    integer, dimension(ncol), intent(in) :: kuptop ! [1]
    real(r8), dimension(ncol, nlev), intent(in) :: rho      ! [kg/m3]
    real(r8), dimension(ncol, nlevp), intent(in) :: dseint  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: qint    ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: dse_up  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: q_up  ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: normassflx_up  ! [1]
!output
    real(r8), dimension(ncol, nlev), intent(out) :: stend   ! [J/s]
    real(r8), dimension(ncol, nlev), intent(out) :: qtend   ! [kg/kg/s]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local

    integer :: i,j,k

!intialize output
    stend = 0._r8
    qtend = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle

!sub cloud layer transport
!        do k=kuplaunch(i)-1, kuptop(i), -1
!        do k=kupbase(i)-1, kuptop(i), -1
        do k=nlev, kuptop(i), -1
            stend(i,k) = -( &
                normassflx_up(i,k)*( dse_up(i,k)-dseint(i,k) )&
                - normassflx_up(i,k+1)*( dse_up(i,k+1)-dseint(i,k+1) )&
                )/dz(i,k)/rho(i,k)
            qtend(i,k) = -( &
                normassflx_up(i,k)*( q_up(i,k)-qint(i,k) )&
                - normassflx_up(i,k+1)*( q_up(i,k+1)-qint(i,k+1) )&
                )/dz(i,k)/rho(i,k)
        end do

        k = kuptop(i)-1
        if ( k>=1 ) then
            stend(i,k) = -( &
                - normassflx_up(i,k+1)*( dse_up(i,k+1)-dseint(i,k+1) )&
                )/dz(i,k)/rho(i,k)
            qtend(i,k) = -( &
                - normassflx_up(i,k+1)*( q_up(i,k+1)-qint(i,k+1) )&
                )/dz(i,k)/rho(i,k)
        end if

    end do

end subroutine cal_tendtransport


subroutine mse2tsat( mse, z, p, t, q)
!------------------------------------------------------
!calculate tempeature and saturated Q given MSE and pressure
!------------------------------------------------------
    real(r8), intent(in) :: mse
    real(r8), intent(in) :: z
    real(r8), intent(in) :: p
    real(r8), intent(out) :: t
    real(r8), intent(out) :: q
!local
    real(r8) :: fc, fa, fb, ta, tb, tc, error, torl, tmp
    integer :: n

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

! ==============================================================================
! (T,p) --> qsat
! ==============================================================================
subroutine cal_qsat( t, p, qsat)
    real(r8) :: t, p, qsat
    qsat = epsilo * 611.2*exp(17.67*(t-273.15)/(t-273.15+243.5)) / p
end subroutine cal_qsat

subroutine cal_qsat2d( t, p, qsat)
    real(r8) :: t(:,:), p(:,:), qsat(:,:)
    qsat = epsilo * 611.2*exp(17.67*(t-273.15)/(t-273.15+243.5)) / p
end subroutine cal_qsat2d

! ==============================================================================
! MSEsat --> T (given reference T, q, and mse)
! ==============================================================================
subroutine cal_mse2tsat(mse, Tref, qref, mseref, T)
    real(r8) :: mse, Tref, qref, mseref, T
    real(r8) :: L, gama

    L = latvap - (cpliq-cpwv)*(T-273.15)
    gama = latvap*latvap/cpair/rh2o * qref/Tref/Tref
    T = Tref + (mse-mseref)/(1+gama)/cpair

end subroutine cal_mse2tsat


end module conv_jp

