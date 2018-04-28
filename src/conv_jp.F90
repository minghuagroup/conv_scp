   
module conv_jp
!------------------------------------------------------
! Stochastic Convective Parameterization Scheme
! contributors: Minghua Zhang, Xin Xie, Haiyang Yu
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
    public :: ecp_readnl
    public :: nplume_sh, nplume_dp

    integer :: ncol=0, nlev=0, nlevp=0

    real(r8), parameter :: unset_r8 = huge(1.0_r8)  ! set namelist variables
    integer,  parameter :: unset_int = -1       ! set namelist variables
!--------------------------------------------------------------
! tunable variables from namelist
!--------------------------------------------------------------
    integer :: ecp_nplume_sh = unset_int
    integer :: ecp_nplume_dp = unset_int
    real(r8) :: ecp_turb_enhance = unset_r8
    real(r8) :: ecp_basemass_enhance = unset_r8
    real(r8) :: ecp_org_enhance = unset_r8
    real(r8) :: ecp_org_shape = unset_r8
    real(r8) :: ecp_evap_enhance = unset_r8
    real(r8) :: ecp_evap_shape = unset_r8
    real(r8) :: ecp_trig_eps0 = unset_r8
    real(r8) :: ecp_trig_c2 = unset_r8
    real(r8) :: ecp_fixcldsr = unset_r8
    real(r8) :: ecp_ratio_ent_rad = unset_r8
    real(r8) :: ecp_orgent_a = unset_r8
    real(r8) :: ecp_orgent_beta0 = unset_r8
    real(r8) :: ecp_w_up_init_sh_beg = unset_r8
    real(r8) :: ecp_w_up_init_sh_end = unset_r8
    real(r8) :: ecp_w_up_init_dp_beg = unset_r8
    real(r8) :: ecp_w_up_init_dp_end = unset_r8
    real(r8) :: ecp_rain_z0 = unset_r8
    real(r8) :: ecp_rain_zp = unset_r8
    real(r8) :: ecp_dn_be = unset_r8
    real(r8) :: ecp_dn_ae = unset_r8
    real(r8) :: ecp_dn_vt = unset_r8
    real(r8) :: ecp_dn_frac_sh = unset_r8
    real(r8) :: ecp_dn_frac_dp = unset_r8
    real(r8) :: ecp_pmf_alpha_sh = unset_r8
    real(r8) :: ecp_pmf_tau_sh = unset_r8
    real(r8) :: ecp_pmf_alpha_dp = unset_r8
    real(r8) :: ecp_pmf_tau_dp = unset_r8
    real(r8) :: ecp_capelmt_sh   = unset_r8
    real(r8) :: ecp_capelmt_dp   = unset_r8
    real(r8) :: ecp_tpertglob   = unset_r8
    real(r8) :: ecp_qpertglob   = unset_r8
    integer  :: ecp_meanorsum   = unset_int
    real(r8) :: ecp_facdlf = unset_r8

!--------------------------------------------------------------
! physical parameter
!--------------------------------------------------------------
    real(r8), parameter :: gravit = 9.80616         ! gravitational acceleration (m/s**2)
    real(r8), parameter :: pi     = 3.141592653     ! Pi
    real(r8), parameter :: cpair  = 1004.64         ! specific heat of dry air (J/K/kg)
    real(r8), parameter :: cpliq  = 4188.           ! specific heat of fresh h2o (J/K/kg)
    real(r8), parameter :: cpwv   = 1810.           ! specific heat of water vapor (J/K/kg)
    real(r8), parameter :: latvap = 2.501e6         ! latent heat of vaporization (J/kg)
    real(r8), parameter :: latice = 3.34e5          ! latent heat of freezing (J/kg)
    real(r8), parameter :: epsilo = 0.6219705862    ! ratio of h2o to dry air molecular weights
    real(r8), parameter :: tmelt  = 273.15          ! Freezing point of water (K)
    real(r8), parameter :: rair   = 287.042311365   ! Dry air gas constant (J/K/kg)
    real(r8), parameter :: rh2o   = 461.5046398202  ! Water vapor gas constant (J/K/kg)
    real(r8), parameter :: rhofw  = 1000.           ! liquid water density (kg/m3)
    real(r8), parameter :: tveps  = 1.0/epsilo - 1  ! coefficient of virsual temperature
   
!--------------------------------------------------------------
! parameters of the new scheme: MZhang scheme
!--------------------------------------------------------------
    integer,  parameter :: ischeme = 2          ! 1: CS2010;  2: MZhang Group
    integer,  parameter :: flagbspdf = 2        ! 1: uniform distribution;  2: new pdf
    integer,  parameter :: cldhiteration = 2    ! iteration for cloud height
    integer,  parameter :: flagorgent = 6       ! 1: using beta0 and minMSE as the division between entr and detr
                                                ! 2: new organized entr and detr, and use half of H as division
                                                ! 3,4: only organized entr, no orgnized detr
                                                ! 5: when B<=0, use detr
                                                ! 6: same as 5, but add a profile shape
    integer,  parameter :: flagtotent = 3       ! 1: organized only; 2: turbulence only; 3: sum of org and turb
    integer,  parameter :: flagturbent = 1      ! 1: using low interface only; 
                                                ! 2: use low interface at the first iteration, then averaged
    integer,  parameter :: flagbuoysort = 2     ! 1: old codes from CAM UW
                                                ! 2: new codes 
    real(r8) :: trig_eps0  = unset_r8   ! trigger parameters: w -> R (default: 0.003)
    real(r8) :: trig_c2    = unset_r8   ! trigger parameters: w -> R: 23.5 ~ 240m; default: 117.5 ~ 1km
    real(r8) :: turb_enhance = unset_r8   ! enhance turbulence entrainment and detrainment (1.0)
    real(r8) :: basemass_enhance = unset_r8   ! enhance cloud base mass flux for shallow plumes (>1.0)
    real(r8) :: org_enhance = unset_r8   ! enhance organized entrainment and detrainment (2.0)
    real(r8) :: org_shape = unset_r8     ! shape parameter of organized entrainment and detrainment (2.0)
    real(r8) :: evap_enhance = unset_r8   ! enhance evaporation (5.0)
    real(r8) :: evap_shape = unset_r8     ! shape parameter of evaporation profile (2.0)
    real(r8), parameter :: fixcldrad0 = -1      ! if +: fixed cloud base radius;  if -: diagnostic from winit
    real(r8) :: fixcldsr   = unset_r8   ! default: 1.0; if +: fixed cloud size ratio;  if -: iteration
    real(r8) :: ratio_ent_rad = unset_r8  ! relation between turbulent entr/detr and cloud radius (0.2)
    real(r8) :: orgent_a     = unset_r8  ! organized entrainment parameters: default is 1.23
    real(r8) :: orgent_beta0 = unset_r8   ! organized entrainment parameters: default is 2.0
    real(r8), parameter :: wupmin = 0.01        ! threshold of w for stopping convection
    real(r8), parameter :: wupmax = 100.0       ! upbound of w
!#ifdef SCMDIAG
!    real(r8), parameter :: fixbasemf = 0.01    ! if +: fixed cloud base mass flux; if -: prognostic
!#endif
!#if (! defined SCMDIAG)
    real(r8), parameter :: fixbasemf = -0.01    ! if +: fixed cloud base mass flux; if -: prognostic
!#endif
    
    real(r8), parameter :: zpbltop = -1000.0    ! if +: downdraft mass flux decreases gradually in PBL
                                                ! if -: no decreasing 
    integer :: meanorsum = unset_int            ! 1: mean of plumes
                                                ! 2: sum of plumes
    real(r8) :: facdlf = unset_r8
!--------------------------------------------------------------
! GRE and NSJ parameters
!--------------------------------------------------------------
    integer, parameter :: flagmeaniter = 0      ! 0: no mean;  
                                                ! 1: mean of entr/detr of the last iteration
    integer, parameter :: maxiteration = 2      ! maximum iteration number for mseQi
    integer, parameter :: ctopflag = 2          ! 1: B<0; 2: w<0
    integer, parameter :: buoyflag = 2          ! 1: B=Tv'/Tv; 2: B=Tv'/Tv - qliq - qice
    integer, parameter :: mse2tsatflag = 1      ! 1: Taylor; 2: bi-section
    integer, parameter :: mseqiflag = 1         ! 1: use Qi; 0: Qi=0
    integer, parameter :: entratemidflag = 1    ! 1: averaged; 2: recalculated with B and w
    integer, parameter :: bsflag = 1            ! 0: no buoy sort 1: buoy sort
    integer, parameter :: mflxmeanflag = 1      ! 0: averaged; 1: with log weighted
    integer, parameter :: negcondflag  = 1      ! 0: exit when c<0; 1: recalculate qv,T when c<0

    real(r8), parameter :: max_ent_rate = 4.e-3_r8      ! maximum entrainment rate (1/m)
    real(r8), parameter :: max_det_rate = 4.e-3_r8      ! maximum detrainment rate (1/m)
    real(r8), parameter :: zuplaunchtop = 3000.0      ! default: 3000; max cloud parcel launch height [m]
    real(r8), parameter :: zuplaunchlow = 0.0         ! default: 0; min cloud parcel launch height [m]
    
    integer :: nplume_sh = unset_int               ! shallow plumes
    integer :: nplume_dp = unset_int                ! deep plumes

!--------------------------------------------------------------
! shallow plumes parameters
!--------------------------------------------------------------
    real(r8), parameter :: greg_z0_sh = 1.e4_r8
    real(r8), parameter :: greg_ent_a_sh_beg = 0.15_r8
    real(r8), parameter :: greg_ent_a_sh_end = 0.15_r8
    real(r8), parameter :: greg_ce_sh_beg    = 0.8_r8
    real(r8), parameter :: greg_ce_sh_end    = 0.8_r8
    real(r8) :: w_up_init_sh_beg  = unset_r8 ! 0.1
    real(r8) :: w_up_init_sh_end  = unset_r8 ! 1.2

!--------------------------------------------------------------
! deep plumes parameters
!--------------------------------------------------------------
    real(r8), parameter :: greg_z0_dp = 1.e4_r8 
    real(r8), parameter :: greg_ent_a_dp_beg = 0.15_r8 
    real(r8), parameter :: greg_ent_a_dp_end = 0.15_r8 
    real(r8), parameter :: greg_ce_dp_beg    = 0.5_r8  
    real(r8), parameter :: greg_ce_dp_end    = 0.5_r8  
    real(r8) :: w_up_init_dp_beg  = unset_r8 ! 0.2    
    real(r8) :: w_up_init_dp_end  = unset_r8 ! 3.0

!--------------------------------------------------------------
! parameters for NSJ scheme
!--------------------------------------------------------------
    real(r8), parameter :: nsj_ent_a = 0.9_r8     
    real(r8), parameter :: nsj_coef  = 1.8e-3_r8  

!--------------------------------------------------------------
! rain fraction parameters
!--------------------------------------------------------------
    real(r8) :: rain_z0 = unset_r8      ! default: 0; CS2010: 1500
    real(r8) :: rain_zp = unset_r8   ! default: 1500; CS2010: 4000

!--------------------------------------------------------------
! cloud ice fraction parameters
!--------------------------------------------------------------
    real(r8), parameter :: cloud_t1 = 258.15    ! mixed / ice cloud
    real(r8), parameter :: cloud_t2 = 273.15    ! liq / mixed cloud
    
!--------------------------------------------------------------
! downdraft parameters
!--------------------------------------------------------------
    real(r8) :: dn_be = unset_r8  ! default: 5.0e-4
    real(r8) :: dn_ae = unset_r8  ! default: 0.3
    real(r8) :: dn_vt = unset_r8  ! default: 10
    real(r8) :: dn_frac_sh = unset_r8     ! default: 0.3; ratio of downdraft base massflux to updraft
    real(r8) :: dn_frac_dp = unset_r8     ! default: 0.3; ratio of downdraft base massflux to updraft

!--------------------------------------------------------------
! parameter for prognostics mass flux calculation
!--------------------------------------------------------------
! default setting: 
!    real(r8), parameter :: pmf_alpha_dp = 4000.e7_r8 , pmf_tau_dp = 800.e3_r8 ! cntr deep
!    real(r8), parameter :: pmf_alpha_sh = 50000.e7_r8, pmf_tau_sh = 20000.e3_r8
    real(r8) :: pmf_alpha_dp = unset_r8
    real(r8) :: pmf_tau_dp = unset_r8
    real(r8) :: pmf_alpha_sh = unset_r8
    real(r8) :: pmf_tau_sh = unset_r8

    real(r8) :: pmf_alpha, pmf_tau

!--------------------------------------------------------------
! parameter for diagnostic mass flux calculation
!--------------------------------------------------------------
    real(r8), parameter :: cape_timescale = 10.e7_r8  ! default: not used
    real(r8) :: capelmt_sh = unset_r8  ! default: 80; threshold of CAPE for triggering convection
    real(r8) :: capelmt_dp = unset_r8  ! default: 80; threshold of CAPE for triggering convection

!--------------------------------------------------------------
! parameter for diagnostic mass flux calculation
!--------------------------------------------------------------
    real(r8) :: tpertglob = unset_r8   ! default: 0.0
    real(r8) :: qpertglob = unset_r8   ! default: 0.0

! -------------------------------------------------------------------
! local variables
! -------------------------------------------------------------------
    integer :: ent_opt ! 0=EC, 2=GREG, 3=NSJ
    integer :: nplume
    integer :: nplume_tot, ind_offset
    real(r8) :: w_up_init_beg 
    real(r8) :: w_up_init_end 
    real(r8) :: greg_ent_a_beg, greg_ent_a_end, greg_ent_a_delta
    real(r8) :: greg_ce_beg, greg_ce_end, greg_ce_delta
    real(r8) :: greg_z0, greg_ent_a, greg_ce
    real(r8) :: c0              ! parameter for converting cloud water to rainfall

    real(r8) :: f_dcape
    real(r8) :: f_cape
    real(r8) :: f_w

    contains

! ==============================================================================
! initialize the convection scheme
! ==============================================================================
subroutine ecp_readnl(nlfile)
#ifdef SCMDIAG
    use shr_nl_mod,  only: shr_nl_find_group_name
#endif
    
#if (! defined SCMDIAG)    
    use namelist_utils,  only: find_group_name
    use spmd_utils,      only: masterproc
    use abortutils,      only: endrun
    use units,           only: getunit, freeunit
    use mpishorthand
#endif

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: subname = 'ecp_readnl'

   namelist /ecp_nl/ ecp_nplume_sh, ecp_nplume_dp, ecp_trig_eps0, ecp_trig_c2, &
       ecp_turb_enhance, ecp_basemass_enhance, ecp_fixcldsr, &
       ecp_ratio_ent_rad, ecp_orgent_a, ecp_orgent_beta0, ecp_org_enhance, ecp_org_shape, &
       ecp_w_up_init_sh_beg, ecp_w_up_init_sh_end, ecp_w_up_init_dp_beg, ecp_w_up_init_dp_end, &
       ecp_rain_z0, ecp_rain_zp, ecp_dn_be, ecp_dn_ae, ecp_dn_vt, ecp_dn_frac_sh, ecp_dn_frac_dp, &
       ecp_pmf_alpha_sh, ecp_pmf_tau_sh, ecp_pmf_alpha_dp, ecp_pmf_tau_dp, ecp_capelmt_sh, ecp_capelmt_dp, &
       ecp_tpertglob, ecp_qpertglob, ecp_meanorsum, ecp_facdlf, ecp_evap_enhance, ecp_evap_shape
   !-----------------------------------------------------------------------------

#if (! defined SCMDIAG)    
   if (masterproc) then
       unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'ecp_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, ecp_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
#endif

#ifdef SCMDIAG
      open( 10, file=trim(nlfile), status='old' )
      call shr_nl_find_group_name(10, 'ecp_nl', status=ierr)
      if (ierr == 0) then
         read(10, ecp_nl, iostat=ierr)
         if (ierr /= 0) then
            write(*,*) 'Haiyang: ERROR reading namelist'
         end if
      end if
      close(10)
#endif

    ! set local variables
        nplume_sh = ecp_nplume_sh
        nplume_dp = ecp_nplume_dp
        trig_eps0 = ecp_trig_eps0
        trig_c2   = ecp_trig_c2
        turb_enhance = ecp_turb_enhance
        basemass_enhance = ecp_basemass_enhance
        org_enhance = ecp_org_enhance
        org_shape = ecp_org_shape
        fixcldsr  = ecp_fixcldsr
        ratio_ent_rad = ecp_ratio_ent_rad
        orgent_a      = ecp_orgent_a
        orgent_beta0  = ecp_orgent_beta0
        w_up_init_sh_beg = ecp_w_up_init_sh_beg
        w_up_init_sh_end = ecp_w_up_init_sh_end
        w_up_init_dp_beg = ecp_w_up_init_dp_beg
        w_up_init_dp_end = ecp_w_up_init_dp_end
        rain_z0 = ecp_rain_z0
        rain_zp = ecp_rain_zp
        dn_be   = ecp_dn_be
        dn_ae   = ecp_dn_ae
        dn_vt   = ecp_dn_vt
        dn_frac_sh = ecp_dn_frac_sh
        dn_frac_dp = ecp_dn_frac_dp
        pmf_alpha_sh = ecp_pmf_alpha_sh
        pmf_tau_sh   = ecp_pmf_tau_sh
        pmf_alpha_dp = ecp_pmf_alpha_dp
        pmf_tau_dp   = ecp_pmf_tau_dp
        capelmt_sh = ecp_capelmt_sh
        capelmt_dp = ecp_capelmt_dp
        tpertglob = ecp_tpertglob
        qpertglob = ecp_qpertglob
        meanorsum = ecp_meanorsum 
        facdlf = ecp_facdlf
        evap_enhance = ecp_evap_enhance
        evap_shape = ecp_evap_shape
#if (! defined SCMDIAG)    
    end if
#endif

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast(nplume_sh,  1, mpiint,  0, mpicom)
   call mpibcast(nplume_dp,  1, mpiint,  0, mpicom)
   call mpibcast(meanorsum,  1, mpiint,  0, mpicom)
   call mpibcast(trig_eps0,  1, mpir8,  0, mpicom)
   call mpibcast(trig_c2,    1, mpir8,  0, mpicom)
   call mpibcast(turb_enhance,    1, mpir8,  0, mpicom)
   call mpibcast(basemass_enhance,    1, mpir8,  0, mpicom)
   call mpibcast(org_enhance,    1, mpir8,  0, mpicom)
   call mpibcast(org_shape,    1, mpir8,  0, mpicom)
   call mpibcast(fixcldsr,   1, mpir8,  0, mpicom)
   call mpibcast(ratio_ent_rad,  1, mpir8,  0, mpicom)
   call mpibcast(orgent_a,  1, mpir8,  0, mpicom)
   call mpibcast(orgent_beta0,  1, mpir8,  0, mpicom)
   call mpibcast(w_up_init_sh_beg,  1, mpir8,  0, mpicom)
   call mpibcast(w_up_init_sh_end,  1, mpir8,  0, mpicom)
   call mpibcast(w_up_init_dp_beg,  1, mpir8,  0, mpicom)
   call mpibcast(w_up_init_dp_end,  1, mpir8,  0, mpicom)
   call mpibcast(rain_z0,  1, mpir8,  0, mpicom)
   call mpibcast(rain_zp,  1, mpir8,  0, mpicom)
   call mpibcast(dn_be,  1, mpir8,  0, mpicom)
   call mpibcast(dn_ae,  1, mpir8,  0, mpicom)
   call mpibcast(dn_vt,  1, mpir8,  0, mpicom)
   call mpibcast(dn_frac_sh,  1, mpir8,  0, mpicom)
   call mpibcast(dn_frac_dp,  1, mpir8,  0, mpicom)
   call mpibcast(pmf_alpha_sh,  1, mpir8,  0, mpicom)
   call mpibcast(pmf_tau_sh,  1, mpir8,  0, mpicom)
   call mpibcast(pmf_alpha_dp,  1, mpir8,  0, mpicom)
   call mpibcast(pmf_tau_dp,  1, mpir8,  0, mpicom)
   call mpibcast(capelmt_sh,  1, mpir8,  0, mpicom)
   call mpibcast(capelmt_dp,  1, mpir8,  0, mpicom)
   call mpibcast(tpertglob,  1, mpir8,  0, mpicom)
   call mpibcast(qpertglob,  1, mpir8,  0, mpicom)
   call mpibcast(facdlf,  1, mpir8,  0, mpicom)
   call mpibcast(evap_enhance,  1, mpir8,  0, mpicom)
   call mpibcast(evap_shape,  1, mpir8,  0, mpicom)
#endif

    write(*, *) "nplume_sh: ", nplume_sh
    write(*, *) "nplume_dp: ", nplume_dp
    write(*, *) "meanorsum: ", meanorsum
    write(*, *) "trig_eps0: ", trig_eps0
    write(*, *) "trig_c2: ", trig_c2
    write(*, *) "turb_enhance: ", turb_enhance
    write(*, *) "basemass_enhance: ", basemass_enhance
    write(*, *) "org_enhance: ", org_enhance
    write(*, *) "org_shape: ", org_shape
    write(*, *) "fixcldsr: ", fixcldsr
    write(*, *) "ratio_ent_rad: ", ratio_ent_rad
    write(*, *) "orgent_a: ", orgent_a
    write(*, *) "orgent_beta0: ", orgent_beta0
    write(*, *) "w_up_init_sh_beg: ", w_up_init_sh_beg
    write(*, *) "w_up_init_sh_end: ", w_up_init_sh_end
    write(*, *) "w_up_init_dp_beg: ", w_up_init_dp_beg
    write(*, *) "w_up_init_dp_end: ", w_up_init_dp_end
    write(*, *) "rain_z0: ", rain_z0
    write(*, *) "rain_zp: ", rain_zp
    write(*, *) "dn_be: ", dn_be
    write(*, *) "dn_ae: ", dn_ae 
    write(*, *) "dn_vt: ", dn_vt
    write(*, *) "dn_frac_sh: ", dn_frac_sh
    write(*, *) "dn_frac_dp: ", dn_frac_dp
    write(*, *) "pmf_alpha_sh: ", pmf_alpha_sh
    write(*, *) "pmf_tau_sh: ", pmf_tau_sh
    write(*, *) "pmf_alpha_dp: ", pmf_alpha_dp
    write(*, *) "capelmt_sh: ", capelmt_sh
    write(*, *) "capelmt_dp: ", capelmt_dp
    write(*, *) "tpertglob: ", tpertglob
    write(*, *) "qpertglob: ", tpertglob
    write(*, *) "facdlf: ", facdlf
    write(*, *) "evap_enhance: ", evap_enhance
    write(*, *) "evap_shape: ", evap_shape

end subroutine ecp_readnl

! ==============================================================================
! initialize the convection scheme
! ==============================================================================
subroutine conv_jp_init(innlev)
    
!input
    integer, intent(in) :: innlev

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
    
    call ecp_readnl('atm_in')
    
end subroutine conv_jp_init


! ==============================================================================
! master route of convection scheme
! ==============================================================================
subroutine conv_jp_tend( &
!input
        inncol, &
        in_ent_opt, dtime, qmin, &
        lat, landfrac, lhflx, &
        psrf, p, dp, zsrf, z, dz, &
        t_in, q_in, bfls_t, bfls_q, &
        omega, pblh, tpert, nn_prec, nn_stend, nn_qtend, &
!in/output
        massflxbase_p, &
!output
        jctop, jcbot, &
        stend, qtend, &
        qliqtend,  prec, qliq,  &
        precrate_out, &
!        qliqtend, qicetend, prec, qliq, qice, &
!        rainrate_out, snowrate_out, precrate_out, &
        mcon, &
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
! Haiyang Yu
    use nnparameter, only: cal_weight, nn_flag

! Main Interface
    integer , intent(in) :: inncol ! size of column dimension

    integer , intent(in) :: in_ent_opt ! 0=ec, 1=greg
    real(r8), intent(in) :: dtime  ! [s] time step
    real(r8), intent(in) :: qmin   ! [kg/kg] minimum Q
    real(r8), dimension(inncol), intent(in) :: lat, landfrac, lhflx
    real(r8), dimension(inncol), intent(in) :: psrf, zsrf
    real(r8), dimension(inncol, nlev), intent(in) :: p, dp, z, dz ! [Pa] ; [m]
    real(r8), dimension(inncol, nlev), intent(in) :: t_in, q_in ! [K] ; [kg/kg]
       ! T and Q state after the large-scale forcing is applied, current state
    real(r8), dimension(inncol, nlev), intent(in) :: bfls_t, bfls_q ! [K] ; [kg/kg]
       ! T and Q state before the large-scale forcing is applied
    real(r8), dimension(inncol, nlev), intent(in) :: omega ! [m/s]
    real(r8), dimension(inncol), intent(in) :: pblh  ! [m/s]
    real(r8), dimension(inncol), intent(in) :: tpert  ! [K]
    real(r8), dimension(inncol), intent(in) :: nn_prec  ! [m/s] NNprec
    real(r8), dimension(inncol, nlev), intent(in) :: nn_stend  ! [J/kg/s] NN
    real(r8), dimension(inncol, nlev), intent(in) :: nn_qtend  ! [kg/kg/s] NN
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

    real(r8), dimension(inncol, nlevp), intent(out) :: mcon

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
    real(r8), dimension(inncol, nlev) :: t, q
    real(r8), dimension(inncol, nlev) :: dse !environment [J/kg]
    real(r8), dimension(inncol, nlev) :: mse, msesat ! [J/kg] ; [J/kg]
    real(r8), dimension(inncol, nlev) :: twet !environment wet bulb temperature [K]
    real(r8), dimension(inncol, nlevp) :: twetint !environment wet bulb temperature [K]
 
    real(r8), dimension(inncol, nlev) :: esat, qsat ! [Pa] ; [kg/kg]
    real(r8), dimension(inncol, nlev) :: rh  ! [1] relative humidity
    real(r8), dimension(inncol, nlev) :: rho ! [kg/m3]

    integer, dimension(inncol) :: trigdp ! [1] 1 trigger ; 0 no trigger
    integer, dimension(inncol) :: trigsh ! [1] 1 trigger ; 0 no trigger

    integer, dimension(inncol) :: kuplaunch ! [1] cloud launching level
    integer, dimension(inncol) :: kuplcl    ! [1] LCL
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
    real(r8), dimension(inncol, nlevp) :: rhoint ! [1] density at the interface

    real(r8), dimension(inncol, nlevp) :: mseint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: dseint ! [1] height at the interface
    real(r8), dimension(inncol, nlevp) :: esatint, qsatint ! [Pa] ; [kg/kg]
    real(r8), dimension(inncol, nlevp) :: msesatint ! [1] height at the interface

    real(r8), dimension(inncol, nlevp) :: normassflx_up ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlevp) :: normassflx_up_tmp ! [1]  bulk normalized updraft mass flux
    real(r8), dimension(inncol, nlev)  :: ent_rate_dp_up ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlev)  :: det_rate_dp_up ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlev)  :: ent_rate_sh_up ! [1] solved PARCEL fractional entrainment rates
    real(r8), dimension(inncol, nlev)  :: det_rate_sh_up ! [1] solved PARCEL fractional entrainment rates
    ! Haiyang: diagnostic purpose
    real(r8), dimension(inncol, nlev)  :: ent_org 
    real(r8), dimension(inncol, nlev)  :: det_org 
    real(r8), dimension(inncol, nlev)  :: ent_turb 
    real(r8), dimension(inncol, nlev)  :: det_turb
    real(r8), dimension(inncol, nlev)  :: cldrad
    real(r8), dimension(inncol, nlev)  :: bs_xc

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

    real(r8), dimension(inncol, nlevp) :: mse_dn ! [J/kg]  bulk in-cloud MSE given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: t_dn   ! [K]  bulk in-cloud temperatur given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: q_dn   ! [kg/kg]  bulk in-cloud sat water vapor given en/detrainment rate.
    real(r8), dimension(inncol, nlevp) :: dse_dn ! [J/kg]  bulk in-cloud DSE given en/detrainment rate.
    
    real(r8) :: dn_frac   ! downdraft fraction

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

    real(r8),dimension(inncol, nlev) :: ent_rate_dp_up_closure
    real(r8),dimension(inncol, nlev) :: det_rate_dp_up_closure

    real(r8),dimension(inncol, nlev) :: tv_closure ! [K]
    real(r8),dimension(inncol, nlev) :: buoy_closure  ! [kg/kg] adjusted buoyancy for closure use
    real(r8),dimension(inncol, nlev) :: w_up_closure ! [J/kg]  bulk in-cloud vertical velocity


! These variables are for closure calculation, OR base mass flux
    real(r8),dimension(inncol, nlev) :: w         ! [m/s] environment vertical velocity
    real(r8),dimension(inncol) :: mconv           ! [1] moisture convergence
    real(r8),dimension(inncol) :: conv            ! [1] wind convergence
    real(r8),dimension(inncol, nlev) :: dilucape  ! [1] CAPE cloud work function
    real(r8),dimension(inncol, nlev) :: cwf       ! [1] CAPE cloud work function
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
    real(r8), dimension(inncol, nlev) :: qliqtendsum
    real(r8), dimension(inncol, nlev) :: precratesum
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

! Haiyang Yu: weights of plumes
    real(r8), dimension(inncol) :: totalweight
    real(r8), dimension(inncol, 50) :: weights
    integer, dimension(inncol) :: validplume
    integer :: valid

!for test
    real(r8), dimension(inncol) :: tmp ! [1] number of convective lev
    real(r8) :: diffz, dw_up_init, basemass_scale
    logical :: flag

! weights of plumes
    weights = 0.0
    totalweight = 0.0
    validplume(:) = 0
    valid = 0

!setting the internal dimension size same as the input
    ncol = inncol

    ent_opt = in_ent_opt

!intialize output
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
    cwf = 0._r8
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
    qliqtendsum = 0._r8
    precratesum = 0._r8
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

    t = t_in
    q = q_in

    lvmid = latvap - (cpliq-cpwv) * (t-273.15)
    call cal_qsat2d(t(:,:), p(:,:), qsat(:,:))

    dse = cpair*t + gravit*z
    mse = dse + lvmid*q
    msesat = dse + lvmid*qsat
    rh  = q/qsat
    rho = p/t/rair
    w = -omega/rho/gravit

    call cal_twet2d(t, rh, twet)

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

    rhoint = pint/tint/rair

    lvint = latvap - (cpliq-cpwv) * (tint-273.15)
    call cal_qsat2d(tint, pint, qsatint)

    call cal_twet2d(tint, qint/qsatint, twetint)

    dseint = cpair*tint + gravit*zint
    mseint = dseint + lvint*qint
    msesatint = dseint + lvint*qsatint

!------------------------------------------------------
!Different methods to calculate entrainment rate
!------------------------------------------------------

    nplume_tot = nplume_sh + nplume_dp

! --- the big loop for dp and sh convection
    do iconv = 1, 2

        ! trigdp = 1

        if ( iconv == 1 ) then

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

            dn_frac = dn_frac_sh

        else if ( iconv == 2 ) then

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

            dn_frac = dn_frac_dp
        end if

        if (nplume > 1) then
            dw_up_init = (w_up_init_end - w_up_init_beg) / (nplume-1)
            greg_ent_a_delta = ( greg_ent_a_end - greg_ent_a_beg ) / (nplume-1)
            greg_ce_delta    = ( greg_ce_end - greg_ce_beg ) / (nplume-1)
        else
            dw_up_init = 0.0
            greg_ent_a_delta = 0.
            greg_ce_delta = 0.
        end if

        if (nplume <= 0) then
            cycle
        end if

! do some variable cleaning here
! variables w_up, buoy
        mse_up = 0._r8
        dse_up = 0._r8
        t_up = 0._r8
        q_up = 0._r8
        ent_rate_dp_up = 0._r8
        det_rate_dp_up = 0._r8
        ent_rate_sh_up = 0._r8
        det_rate_sh_up = 0._r8
        normassflx_up = 0._r8
    
        ent_org = 0.0_r8
        det_org = 0.0_r8
        ent_turb = 0.0_r8
        det_turb = 0.0_r8
        cldrad = 0.0_r8

        mse_dn = 0._r8
        dse_dn = 0._r8
        t_dn = 0._r8
        q_dn = 0._r8
        normassflx_dn = 0._r8

        if ( iconv == 1 ) then   ! shallow plumes
            call cal_launchtocldbase( 2, z, zint, p, pint, t, tint, q, qint, qsat, qsatint, &
                mse, mseint, msesat, msesatint, landfrac, lhflx,  &
                kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up, trigdp)
            kupbase = kuplaunch

        else if ( iconv == 2 ) then    ! deep plumes
            call cal_launchtocldbase( 1, z, zint, p, pint, t, tint, q, qint, qsat, qsatint, &
                mse, mseint, msesat, msesatint, landfrac, lhflx,  &
                kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up, trigdp)
            kupbase = kuplcl

        end if

        dse_up = cpair*t_up+gravit*zint

        jcbot = kupbase
        do i = 1, inncol
            if (jcbot(i) > nlev) then
                jcbot(i) = nlev
            end if
        end do

        jctop = kupbase


        do j = ind_offset+1, ind_offset+nplume

            greg_ent_a = greg_ent_a_beg + (j-ind_offset-1)*greg_ent_a_delta
            greg_ce    = greg_ce_beg + (j-ind_offset-1)*greg_ce_delta

            w_up_init = w_up_init_beg + (j-ind_offset-1) * dw_up_init

#ifdef SCMDIAG
            write(*, *) "---------------------------------------------------"
            write(*,'(a10,i5,a10,f10.5)') "plume:", j, ", w = ",w_up_init
#endif

            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                mse_up(i, 1:kupbase(i)-1) = mseint(i, 1:kupbase(i)-1)
                t_up(i, 1:kupbase(i)-1) = tint(i, 1:kupbase(i)-1)
                q_up(i, 1:kupbase(i)-1) = qint(i, 1:kupbase(i)-1)
            end do

            normassflx_up_tmp = normassflx_up
            normassflx_dn_tmp = 0._r8

!updraft properties
            if (ischeme == 2) then
                ! new scheme: MZhang
                call cal_mse_up( &
                    j, rho, rhoint, z, zint, dz, p, pint, t, tint, q, qint, qsat, qsatint, &
                    mse, mseint, msesat, msesatint, &
                    kuplaunch, kupbase, &
                    ent_rate_dp_up, det_rate_dp_up, ent_rate_sh_up, det_rate_sh_up, &
                    ent_org, det_org, ent_turb, det_turb, cldrad, &
                    bs_xc, w_up_init, &
                    mse_up, t_up, q_up, qliq_up, qice_up, mseqi, condrate, rainrate, snowrate, precrate, &
                    normassflx_up_tmp, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
                    trigdp)
            end if
            if (ischeme == 1) then
                ! old scheme: GRE and NSJ
                call cal_mse_up_old( &
                    ent_opt, rho, z, zint, dz, p, pint, t, tint, q, qint, qsat, qsatint, &
                    mse, mseint, msesat, msesatint, &
                    kuplaunch, kupbase, &
                    ent_rate_dp_up, det_rate_dp_up, ent_rate_sh_up, det_rate_sh_up, bs_xc, w_up_init, &
                    mse_up, t_up, q_up, qliq_up, qice_up, mseqi, condrate, rainrate, snowrate, precrate, &
                    normassflx_up_tmp, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
                    trigdp)
             end if

            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                if ( kuptop(i) < jctop(i) ) then
                    jctop(i) = kuptop(i)
                end if
            end do

            dse_up = cpair*t_up+gravit*zint
            stendcond =  latvap*condrate
            qtendcond = -condrate

!evaporation tendency
            call cal_evap( &
                ent_opt, kuptop, trigdp, dz, p, rho, t, twet, q, &
                precrate, accuprec, surfprec, evaprate )

            stendevap = -latvap*evaprate
            qtendevap =  evaprate

!downdraft properties
            call cal_mse_dn( &
                ent_opt, kuptop, trigdp, dz, zint, p, pint, rho, t, twet, twetint, lvmid, &
                qint, dseint, accuprec, evaprate, buoy_mid, dn_frac, &
                dse_dn, q_dn, normassflx_dn_tmp)

            mse_dn = dse_dn + lvint*q_dn

! dilute CAPE
            call cal_cape( &
                dz, buoy_mid, normassflx_up_tmp, kupbase, kuptop, &
                dilucape(:,j), cwf(:,j), &
                trigdp)

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
                qliqtend_det(i,1:nlev) = 0.0
                if ( trigdp(i)<1 ) cycle

                ! New version: detrain occurs through cloud layers
                do k = kuptop(i)-1, kupbase(i)-1, 1
                    qliqtend_det(i,k) = max(0.0, &
                        normassflx_up_tmp(i,k+1) * min(det_rate_dp_up(i,k) + det_rate_sh_up(i,k), max_det_rate) &
                            * (qliq_up(i,k+1) + qice_up(i,k+1)) / rho(i,k) )
                    
                    !write(*,*) "qliqtend_net:", k, qliqtend_det(i,k), normassflx_up_tmp(i,k+1), &
                    !    det_rate_dp_up(i,k), det_rate_sh_up(i,k), &
                    !    qliq_up(i,k+1), qice_up(i,k+1), rho(i,k)
                end do

                ! Old version: detrain occurs only at the cloud top layer
                !k = kuptop(i)-1
                !qliqtend_det(i,k) = max( 0.0, &
                !    normassflx_up_tmp(i,k+1)*( qliq_up(i,k+1)+qice_up(i,k+1) ) &
                !    /dz(i,k)/rho(i,k) )
            end do


            do i=1, inncol
                if ( trigdp(i)<1 ) cycle
                if (iconv == 1) then  ! shallow plumes 
                    if (basemass_enhance > 1.0) then
                        basemass_scale = max(0.0, -(basemass_enhance-1.0)/w_up_init_end * w_up_init(i) + basemass_enhance )
                    else
                        basemass_scale = w_up_init_end/w_up_init(i)
                    end if
                    massflxbase_p(i,j) = min( 0.1, max( 0., &
                        massflxbase_p(i,j) + dtime*( max( (dilucape(i,j) - capelmt_sh), 0._r8 )/(2*pmf_alpha) &
                        * basemass_scale &
                        - massflxbase_p(i,j)/(2*pmf_tau) ) ) )
                else    ! deep plumes
                    massflxbase_p(i,j) = min( 0.1, max( 0., &
                        massflxbase_p(i,j) + dtime*( max( (dilucape(i,j) - capelmt_dp), 0._r8 )/(2*pmf_alpha) &
                        - massflxbase_p(i,j)/(2*pmf_tau) ) ) )
                end if
            end do

!SENS
!                    massflxbase_p(i,j) + dtime*( cwf(i,j)/(2*pmf_alpha) &
!                    massflxbase_p(i,j) + dtime*( dilucape(i,j)/(2*pmf_alpha) &
!                massflxbase_p(i,j) = min( 0.1, max( 0., (dilucape(i,j) - capelmt)/cape_timescale ) )


            if (fixbasemf > 0) then
                massflxbase = fixbasemf
            else
                massflxbase(:) = massflxbase_p(:,j)
            end if
            
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

                stendtran_dn(i,:) = stendtran_dn(i,:) * massflxbase(i)
                qtendtran_dn(i,:) = qtendtran_dn(i,:) * massflxbase(i)

                stendcond(i,:) = stendcond(i,:)*massflxbase(i)
                qtendcond(i,:) = qtendcond(i,:)*massflxbase(i)

                stendevap(i,:) = stendevap(i,:)*massflxbase(i)
                qtendevap(i,:) = qtendevap(i,:)*massflxbase(i)

                qliqtend_det(i,:) = qliqtend_det(i,:)*massflxbase(i)

                massflx(i,:) = normassflx_up_tmp(i,:)*massflxbase(i)

                stend(i,:) = stendcond(i,:) + stendevap(i,:) &
                    + stendtran_up(i,:) + stendtran_dn(i,:) &
                    - lvmid(i,:) * qliqtend_det(i,:)
                
                qtend(i,:) = qtendcond(i,:) + qtendevap(i,:) &
                    + qtendtran_up(i,:) + qtendtran_dn(i,:) &
                    + qliqtend_det(i,:)

     !do k = 1, nlev
     ! write(*, *) "yhy:plume:stend:", j, stendcond(i,:), stendevap(i,:), stendtran_up(i,:), stendtran_dn(i,:)
     !write(*, *) "yhy:plume:qtend:", k, qtendcond(i,k), qtendevap(i,k), qtendtran_up(i,k), qtendtran_dn(i,k), qliqtend_det(i,k)
     !end do

                do k=1, nlev
                    netprec(i) = netprec(i) + max(0.0, - ( qtend(i,k) + qliqtend_det(i,k) )*rho(i,k)*dz(i,k))
                end do
                netprec(i) = netprec(i)/rhofw

                ! yhy:
                !do k=1, nlev
                !    qliq(i,k) = max(1e-13, qliqtend_det(i,k)*dtime)
                !end do
                
                !----------------------------------------------------------------------
                ! Haiyang Yu: calculate the weight for each plume
                    call cal_weight(nlev, p(i,:), dp(i,:), nn_stend(i,:), stend(i,:), nn_qtend(i,:), qtend(i,:), weights(i,j), valid)
                    validplume(i) = validplume(i) + valid
                    totalweight(i) = totalweight(i) + weights(i,j)
#ifdef SCMDIAG
    write(*, *) "weight of plume", j, " = ", weights(i,j)
#endif

                if (nn_flag > 0) then
                    stend(i,:) = stend(i,:) * weights(i,j)
                    qtend(i,:) = qtend(i,:) * weights(i,j)
                    qliqtend_det(i,:) = qliqtend_det(i,:) * weights(i,j)
                    precrate(i,:) = precrate(i,:) * weights(i,j)
                    evaprate(i,:) = evaprate(i,:) * weights(i,j)
                    netprec(i) = netprec(i) * weights(i,j)
                    surfprec(i) = surfprec(i) * weights(i,j)
                    massflxbase(i) = massflxbase(i) * weights(i,j)
                    massflx(i,:) = massflx(i,:) * weights(i,j)
                end if
                !----------------------------------------------------------------------
            end do
            
            do i=1, inncol
                stendsum(i,:) = stendsum(i,:)+stend(i,:)
                qtendsum(i,:) = qtendsum(i,:)+qtend(i,:)
                qliqtendsum(i,:) = qliqtendsum(i,:)+qliqtend_det(i,:)
                precratesum(i,:) = precratesum(i,:)+( precrate(i,:)-evaprate(i,:) )
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

#ifdef SCMDIAG
            call subcol_netcdf_putclm( "ent_rate", nlev, &
                min( max_ent_rate, ent_rate_dp_up(1,:)+ent_rate_sh_up(1,:)), j )
            call subcol_netcdf_putclm( "det_rate", nlev, &
                min( max_det_rate, det_rate_dp_up(1,:)+det_rate_sh_up(1,:)), j )
            call subcol_netcdf_putclm( "ent_rate_dp", nlev, ent_rate_dp_up(1,:), j )
            call subcol_netcdf_putclm( "det_rate_dp", nlev, det_rate_dp_up(1,:), j )
            call subcol_netcdf_putclm( "ent_rate_sh", nlev, ent_rate_sh_up(1,:), j )
            call subcol_netcdf_putclm( "det_rate_sh", nlev, det_rate_sh_up(1,:), j )
            call subcol_netcdf_putclm( "bs_xc", nlev, bs_xc(1,:), j )
            
            call subcol_netcdf_putclm( "radius_up", nlev, cldrad(1,:), j )
            call subcol_netcdf_putclm( "ent_rate_org", nlev, ent_org(1,:), j )
            call subcol_netcdf_putclm( "det_rate_org", nlev, det_org(1,:), j )
            call subcol_netcdf_putclm( "ent_rate_turb", nlev, ent_turb(1,:), j )
            call subcol_netcdf_putclm( "det_rate_turb", nlev, det_turb(1,:), j )

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

    !----------------------------------------------------------------------
    ! Haiyang Yu: normalized with weights
    do i = 1, inncol
        if (nn_flag > 0) then
            if ( validplume(i) > 0 .and. abs(totalweight(i)) > 1e-6) then
                totalweight(i) = 1.0 / totalweight(i)
            else
                totalweight(i) = 0.0
            end if

            stendsum(i,:) = stendsum(i,:) * totalweight(i)
            qtendsum(i,:) = qtendsum(i,:) * totalweight(i)
            qliqtendsum(i,:) = qliqtendsum(i,:) * totalweight(i)
            precratesum(i,:) = precratesum(i,:) * totalweight(i)
            precsum(i) = precsum(i) * totalweight(i)
            surfprec(i) = surfprec(i) * totalweight(i)
            massflxbasesum(i) = massflxbasesum(i) * totalweight(i)
            massflxsum(i,:) = massflxsum(i,:) * totalweight(i)
            
        else
            ! without NN: mean
            if (meanorsum == 1) then
                stendsum(i,:) = stendsum(i,:) / nplume_tot  
                qtendsum(i,:) = qtendsum(i,:) / nplume_tot
                qliqtendsum(i,:) = qliqtendsum(i,:) / nplume_tot
                precratesum(i,:) = precratesum(i,:) / nplume_tot
                precsum(i) = precsum(i) / nplume_tot 
                surfprec(i) = surfprec(i) / nplume_tot 
                massflxbasesum(i) = massflxbasesum(i) / nplume_tot 
                massflxsum(i,:) = massflxsum(i,:) / nplume_tot 
            end if
        
        end if
    end do
    !----------------------------------------------------------------------

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

    !use sum as ouput
    prec = precsum
    stend = stendsum
    qtend = qtendsum
    qliqtend = qliqtendsum
    precrate_out = precratesum
    mcon = massflxsum

#ifdef SCMDIAG
!    call subcol_netcdf_putclm( "stendsum", nlev, stendsum(1,:), 1 )
!    call subcol_netcdf_putclm( "qtendsum", nlev, qtendsum(1,:), 1 )
!    call subcol_netcdf_putclm( "qliqtendsum", nlev, qliqtendsum(1,:), 1 )
!    call subcol_netcdf_putclm( "precratesum", nlev, precratesum(1,:), 1 )
!    call subcol_netcdf_putclm( "precsum", 1, precsum(1), 1 )
!    call subcol_netcdf_putclm( "massflxsum", nlevp, massflxsum(1,:), 1 )
!    call subcol_netcdf_putclm( "massflxbasesum", 1, massflxbasesum(1), 1 )
#endif

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
                write(*,*) 'too small Q: ', p(i,k), q(i,k), qtend(i,k)*dtime
#endif
                ! Haiyang Yu
                qtend(i,k) = 0.0
                stend(i,k) = 0.0
                qliqtend(i,k) = 0.0
                mcon(i,k) = 0.0
                precrate_out(i,k) = 0.0

!                minqcheckf = 0._r8
!                trigdp(i) = -91
!                exit
            end if

            qcheckf = q(i,k)+qtend(i,k)*dtime

            if( qcheckf<qmin ) then
                if (abs(qtend(i,k)) > 1e-15) then
                    qcheckf = (qmin*1.001-q(i,k))/dtime/qtend(i,k)
                else
                    qcheckf = 1.0
                end if
                if( qcheckf<minqcheckf ) then
                    minqcheckf = qcheckf
                end if
            end if

        end do

        if( minqcheckf<1.0_r8 ) then
            massflxbase(i) = minqcheckf*massflxbase(i)

            stendcond(i,:) = minqcheckf*stendcond(i,:)
            qtendcond(i,:) = minqcheckf*qtendcond(i,:)
            stendtran_up(i,:) = minqcheckf*stendtran_up(i,:)
            qtendtran_up(i,:) = minqcheckf*qtendtran_up(i,:)
            stendevap(i,:) = minqcheckf*stendevap(i,:)
            qtendevap(i,:) = minqcheckf*qtendevap(i,:)
            
            qliqtend(i,:) = minqcheckf*qliqtend(i,:)
            mcon(i,:) = minqcheckf*mcon(i,:)

            stend(i,:) = minqcheckf*stend(i,:)
            qtend(i,:) = minqcheckf*qtend(i,:)
            prec(i) = minqcheckf*prec(i)
            precrate_out(i,:) = minqcheckf*precrate_out(i,:)

            stendcomp(i,:) = minqcheckf*stendcomp(i,:)
            qtendcomp(i,:) = minqcheckf*qtendcomp(i,:)

            tmp1stend(i,:) = minqcheckf*tmp1stend(i,:)
            tmp1qtend(i,:) = minqcheckf*tmp1qtend(i,:)
            tmp2stend(i,:) = minqcheckf*tmp2stend(i,:)
            tmp2qtend(i,:) = minqcheckf*tmp2qtend(i,:)
        end if

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
    !write(*,"(a20,f20.10)") "bflsdilucape:", bfls_dilucape
    write(*,"(a20,50f20.10)") "dilucape:", dilucape(1,1:nplume_tot)
    write(*,"(a20,50f20.10)") "cwf:", cwf(1,1:nplume_tot)
    !write(*,"(a20,f20.10)") "dilucape_closure:", dilucape_closure
    !!write(*,"(a20,f20.10)") "capefc:", capefc
    !write(*,"(a20,f20.10)") "capeclm:", capeclm
    !write(*,"(a20,f20.10)") "mconv:", mconv
    write(*,"(a20,50f20.10)") "massflxbase_p:", massflxbase_p(1,1:nplume_tot)
    !write(*,"(a20,f20.10)") "massflxbase_cape:", massflxbase_cape
    !write(*,"(a20,f20.10)") "massflxbase_dcape:", massflxbase_dcape
    !write(*,"(a20,f20.10)") "massflxbase_clm:", massflxbase_clm
    !write(*,"(a20,f20.10)") "massflxbase_w:", massflxbase_w
    !write(*,"(a20,f20.10)") "massflxbase_mconv:", massflxbase_mconv
    !write(*,"(a20,f20.10)") "massflxbase:", massflxbase
    write(*,"(a20,f20.10)") "prec:", prec*3600*24*1000
    write(*,"(a20,f20.10)") "surfprec:", surfprec*3600*24*1000
    write(*,"(a20,f20.10)") "minqcheckf:", minqcheckf

    !netcdf output
    !call subcol_netcdf_putclm( "mse", nlev, mse(1,:), 1 )
    !call subcol_netcdf_putclm( "dse", nlev, dse(1,:), 1 )
    !call subcol_netcdf_putclm( "msesat", nlev, msesat(1,:), 1 )
    !call subcol_netcdf_putclm( "z", nlev, z(1,:), 1 )
    call subcol_netcdf_putclm( "p", nlev, p(1,:), 1 )
    !call subcol_netcdf_putclm( "rho", nlev, rho(1,:), 1 )

    !call subcol_netcdf_putclm( "mseint", nlevp, mseint(1,:), 1 )
    !call subcol_netcdf_putclm( "msesatint", nlevp, msesatint(1,:), 1 )

    !call subcol_netcdf_putclm( "zint", nlevp, zint(1,:), 1 )
    call subcol_netcdf_putclm( "pint", nlevp, pint(1,:), 1 )
    !call subcol_netcdf_putclm( "tint", nlevp, tint(1,:), 1 )
    !call subcol_netcdf_putclm( "qint", nlevp, qint(1,:), 1 )
    !call subcol_netcdf_putclm( "qsatint", nlevp, qsatint(1,:), 1 )

    !call subcol_netcdf_putclm( "t", nlev, t(1,:), 1 )
    !call subcol_netcdf_putclm( "q", nlev, q(1,:), 1 )
    !call subcol_netcdf_putclm( "qsat", nlev, qsat(1,:), 1 )

    !call subcol_netcdf_putclm( "prec", prec(1), 1 )
    !call subcol_netcdf_putclm( "pmassflxbase", massflxbase_p(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_cape", massflxbase_cape(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_w", massflxbase_w(1), 1 )
    !call subcol_netcdf_putclm( "massflxbase_mconv", massflxbase_mconv(1), 1 )
    !call subcol_netcdf_putclm( "qcheck", 1, qcheckout(1), 1 )

    !tmp = kupbase-kuptop+1
    !call subcol_netcdf_putclm( "nconvlev", 1, tmp(1), 1 )
    !tmp = kuplaunch
    !call subcol_netcdf_putclm( "kuplaunch", 1, tmp(1), 1 )
    !tmp = kupbase
    !call subcol_netcdf_putclm( "kupbase", 1, tmp(1), 1 )
    !tmp = kuplcl
    !call subcol_netcdf_putclm( "kuplcl", 1, tmp(1), 1 )

#endif

    !-------------------------------------------------
    ! Haiyang test
     qliqtend = qliqtend * facdlf
    !-------------------------------------------------

end subroutine conv_jp_tend

! ==============================================================================
! calculate the launch processes
! ==============================================================================
subroutine cal_launchtocldbase( &
!input
        opt, z, zint, p, pint, t, tint, q, qint, qsat, qsatint, mse, mseint, msesat, msesatint, landfrac, lhflx,  &
!output
        kuplaunch, kuplcl, mse_up, t_up, q_up, normassflx_up,  &
!in
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
!output
    integer, dimension(ncol), intent(out) :: kuplaunch ! [1]
    integer, dimension(ncol), intent(out) :: kuplcl    ! [1]

    real(r8), dimension(ncol, nlevp), intent(out) :: mse_up ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: t_up ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: q_up ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(out) :: normassflx_up ! [kg/kg]

!input/output
    integer, dimension(ncol), intent(in) :: trig     ! [1]

!local
    integer :: i, k, stat
    real(r8) :: msemax, q_up_test
    real(r8) :: diffmse, dt, dq, buoy
    integer, dimension(ncol) :: kuplaunchmin  ! [1]
    integer, dimension(ncol) :: kuplaunchmax  ! [1]
    integer, dimension(ncol) :: kcbase  ! [1]

    !intialize output
    kuplaunch = 1
    kuplcl = 1
    kcbase = nlevp

    do i=1, ncol
        kuplaunchmin(i) = nlevp
        kuplaunchmax(i) = 1

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

    kuplaunch = nlev
    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        
        !find the maximun MSE level as cloud parcel launching point
        msemax = 0._r8
        do k=kuplaunchmin(i), kuplaunchmax(i), -1
            if ( mseint(i,k) >= msemax ) then
                msemax = mseint(i,k)
                kuplaunch(i) = k
            end if
        end do

        if (qpertglob > 1.0) then
            ! Use qpertglob as the RH of the launching parcel
            if (qpertglob < 100.0) then
                t_up(i,kuplaunch(i) ) = tint(i,kuplaunch(i) ) + tpertglob
                call cal_qsat( t_up(i,kuplaunch(i)), pint(i,kuplaunch(i)), q_up_test)
                q_up(i,kuplaunch(i)) = q_up_test * qpertglob / 100.0
                mse_up(i, kuplaunch(i)) = mseint( i,kuplaunch(i) ) + tpertglob*cpair + &
                    latvap*( q_up(i,kuplaunch(i))-qint(i,kuplaunch(i)) )
            else
                t_up(i,kuplaunch(i)) = tint(i,kuplaunch(i))
                q_up(i,kuplaunch(i)) = qsatint(i,kuplaunch(i))
                mse_up(i,kuplaunch(i)) = msesatint(i,kuplaunch(i))
            end if
        else
            ! Use tpertglob and qpertglob
            t_up(i,kuplaunch(i) ) = tint(i,kuplaunch(i) ) + tpertglob
            q_up(i,kuplaunch(i) ) = qint(i,kuplaunch(i) ) + qpertglob
            mse_up(i,kuplaunch(i) ) = mseint( i,kuplaunch(i) ) + tpertglob*cpair + qpertglob*latvap
        end if
        
        ! cloud base at launch point
        if ( opt == 2 ) then
            kcbase(i) = kuplaunch(i)
            kuplcl(i) = kuplaunch(i)
        end if

        ! cloud base at LCL
        if ( opt == 1 ) then
            do k=kuplaunch(i)-1, 1, -1
                mse_up(i,k) = mse_up(i,k+1)
                call cal_mse2tsat(mse_up(i,k), tint(i,k), qsatint(i,k), msesatint(i,k), t_up(i,k))
                call cal_qsat(t_up(i,k), pint(i,k), q_up_test)
            
                if( q_up(i,k+1) > q_up_test ) then
                    kuplcl(i) = k
                    kcbase(i) = k
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
        end if

        ! cloud properties below cloud base
        normassflx_up(i,kcbase(i)) = 1.0
        do k=nlevp, kcbase(i)+1, -1
            normassflx_up(i,k) = ( max( 0.0, (zint(i,k)-zint(i,nlevp))/(zint(i,kuplcl(i))-zint(i,nlevp)) ) )**0.5
            mse_up(i,k) = mse_up(i, kuplcl(i))
            q_up(i,k) = q_up(i, kuplcl(i))
            t_up(i,k) = ( mse_up(i,k) - gravit*zint(i,k) - (latvap+(cpliq-cpwv)*273.15)*q_up(i,k) )/ &
                (cpair-(cpliq-cpwv)*q_up(i,k))
        end do
    end do  ! loop for icol

end subroutine cal_launchtocldbase


! ==============================================================================
! calculate updraft properties (new scheme of MZhang grouup)
! ==============================================================================
subroutine cal_mse_up( &
!input
        iplume, rho, rhoint, z, zint, dz, p, pint, t, tint, q, qint, qsat, qsatint, &
        mse, mseint, msesat, msesatint, kuplaunch, kupbase, &
!in/output
        ent_rate_dp_up, det_rate_dp_up, ent_rate_sh_up, det_rate_sh_up, &
        ent_org, det_org, ent_turb, det_turb, cldrad, & 
        bs_xc, w_up_init, &
        mse_up, t_up, q_up, qliq_up, qice_up, mseqi, condrate, rainrate, snowrate, precrate, &
        normassflx_up, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
        trig)

     use buoysort, only : cal_buoysort, cal_fracmix, cal_entdet

!input
    integer, intent(in) :: iplume
    real(r8), dimension(ncol, nlev),  intent(in) :: rho   ! [kg/m3]
    real(r8), dimension(ncol, nlevp), intent(in) :: rhoint   ! [kg/m3]
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

    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_dp_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_dp_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_sh_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_sh_up ! [1]
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

    real(r8), dimension(ncol, nlevp), intent(inout) :: normassflx_up  ! [#]
    integer , dimension(ncol), intent(inout) :: trig     ! [1]
    real(r8), dimension(ncol, nlev ), intent(inout)  :: ent_org    ! organized entrainment rate (1/m)
    real(r8), dimension(ncol, nlev ), intent(inout)  :: det_org    ! organized detrainment rate (1/m)
    real(r8), dimension(ncol, nlev ), intent(inout)  :: ent_turb   ! turbulent entrainment rate (1/m)
    real(r8), dimension(ncol, nlev ), intent(inout)  :: det_turb   ! turbulent detrainment rate (1/m)
    real(r8), dimension(ncol, nlev ), intent(inout)  :: cldrad     ! cloud radius [m]
    real(r8), dimension(ncol, nlev ), intent(inout) :: bs_xc
!local
    real(r8), dimension(ncol, nlev )  :: frezrate  ! [m2/kg]
    real(r8), dimension(ncol, nlev )  :: normassflx_up_mid  ! [#]
    real(r8), dimension(ncol, nlev )  :: cldsr      ! cloud size ratio: 2R/H [#]
    real(r8), dimension(ncol)  :: cldh              ! cloud height [m]
    real(r8), dimension(ncol)  :: cldradinit        ! initial cloud radius [m]
   
    real(r8) :: cldh_bak, ent_bak, det_bak
    integer  :: ktop_tmp
    real(r8) :: tmp_t_up, tmp_q_up, tmp_buoy, tmp_zuptop_max

    real(r8) :: xc, tv, tv_up,  w2, qw, Fp, Fi, Ek
    real(r8) :: nom, denom, ent_rate, det_rate, tmp
    real(r8) :: ent1,ent2,det1,det2

    real(r8) :: bs_p0, bs_wue, bs_rle, bs_scaleh, bs_cridis, bs_thetal_e, bs_thetal_up

    integer :: i,j,k, iteration, itercldh, iminmse
    integer :: ngbuoy

    bs_p0 = 1000.e2_r8
    bs_rle = 0.1_r8

    !intialize output.
    qliq_up = 0.0
    qice_up = 0.0
    mseqi = 0.0
    frezrate = 0.0
    condrate = 0.0
    rainrate = 0.0
    snowrate = 0.0
    precrate = 0.0
    
    w_up = 0._r8
    w_up_mid = 0._r8
    buoy = 0._r8
    buoy_mid = 0._r8
    normassflx_up_mid = 0.0_r8
    cldrad = 0.0_r8
    cldradinit = 0.0
    cldsr  = 0.0
    cldh = 0.0_r8

    kuptop = nlev
    zuptop = 0._r8
    ngbuoy = 0

    ent_rate_dp_up = 0._r8
    det_rate_dp_up = 0._r8
    ent_rate_sh_up = 0._r8
    det_rate_sh_up = 0._r8
    ent_org = 0.0
    det_org = 0.0
    ent_turb = 0.0
    det_turb = 0.0

    bs_xc = 0._r8

    itercldh = 0
    iteration = 0

    ent_rate = 0.0
    det_rate = 0.0

    do i=1, ncol
        if ( trig(i) < 1 ) cycle

        ! firstly, get the undiluted cloud top height, and initialize cldh
        do k=kupbase(i)-1, 1, -1
            call cal_mse2tsat( mse_up(i,kupbase(i)), tint(i,k), &
                qsatint(i,k), msesatint(i,k), tmp_t_up )
            call cal_qsat(tmp_t_up, pint(i,k), tmp_q_up)

            tv = tint(i,k)*(1+tveps*qint(i,k))
            tv_up = tmp_t_up*(1+tveps*tmp_q_up )
            tmp_buoy = gravit*(tv_up-tv)/tv 
            if ( tmp_buoy <= 0. ) then
                tmp_zuptop_max = zint(i,k)
                exit
            end if
        end do
        
        ktop_tmp = max(1, k)  
        tmp_zuptop_max = zint(i, ktop_tmp)        
        

        ! initialize cloud base properties
        k = kupbase(i)
        tv = tint(i,k)*(1 + tveps*qint(i,k) )
        tv_up = t_up(i,k)*(1 + tveps*q_up(i,k) )
        buoy(i,k)  = gravit*(tv_up-tv)/tv 
        w_up(i,k) = w_up_init(i)
        normassflx_up(i,k) = 1.0
        if ( fixcldrad0 > 0 ) then
            cldradinit(i) = fixcldrad0
        else
            cldradinit(i) = trig_c2 * (trig_eps0**(-0.4)) * (w_up(i,k)**1.2)
        end if
        iminmse = minloc(mse(i,:), 1)
        if (fixcldsr > 0) then
            cldh(i) = 2 * cldradinit(i) / fixcldsr
        else
            cldh(i) = 0.5 * ( tmp_zuptop_max - zint(i,kupbase(i)) )
        end if
        cldh_bak = cldh(i)


        do itercldh = 1, cldhiteration, 1

            do k=kupbase(i)-1, 1, -1

                do iteration = 1, maxiteration, 1
                    
                    ! w, buoyancy, and mass flux at mid-layer
                    if (iteration == 1) then
                        w_up_mid(i,k) = w_up(i,k+1)
                        buoy_mid(i,k) = buoy(i,k+1)
                        normassflx_up_mid(i,k) = normassflx_up(i,k+1)
                    else
                        w_up_mid(i,k) = 0.5 * ( w_up(i,k)+w_up(i,k+1) )
                        buoy_mid(i,k) = 0.5 * ( buoy(i,k)+buoy(i,k+1) )    
                    end if                        

                    ! cloud radius, cloud size ratio
                    cldrad(i,k) = cldradinit(i)*sqrt(normassflx_up_mid(i,k)* &
                        rhoint(i,kupbase(i))*w_up(i,kupbase(i))/rho(i,k)/max(wupmin,w_up_mid(i,k)) )
                    cldsr(i,k) = 2*cldrad(i,k) / cldh(i)

                    ! organized entrainment/detrainment
                    if (flagorgent == 1) then
                        tmp = orgent_beta0 * cldsr(i,k)/sqrt(1+cldsr(i,k)*cldsr(i,k)) * &
                            sqrt(abs(buoy_mid(i,k))*cldh(i)) / ( 2*cldrad(i,k)*max(wupmin,w_up_mid(i,k)) )
                        !if (iteration > 1 .and. rhoint(i,k)*buoy(i,k) < rhoint(i,k+1)*buoy(i,k+1)) then
                        !if (iteration > 1 .and. buoy(i,k) < buoy(i,k+1)) then
                        !if ( buoy_mid(i,k) < 0 ) then
                        if ( k < iminmse ) then
                            ent_org(i,k) = 0.0
                            det_org(i,k) = tmp
                        else
                            ent_org(i,k) = tmp
                            det_org(i,k) = 0.0                        
                        end if
                    end if
                    if (flagorgent == 5) then
                        tmp = orgent_beta0 * cldsr(i,k)/sqrt(1+cldsr(i,k)*cldsr(i,k)) * &
                            sqrt(abs(buoy_mid(i,k))*cldh(i)) / ( 2*cldrad(i,k)*max(wupmin,w_up_mid(i,k)) )
                        if ( buoy_mid(i,k) <= 0 ) then
                            ent_org(i,k) = 0.0
                            det_org(i,k) = tmp
                        else
                            ent_org(i,k) = tmp
                            det_org(i,k) = 0.0                        
                        end if
                    end if
                    if (flagorgent == 6) then
                        if (abs(buoy_mid(i,k)) > 1e-15) then
                            tmp = orgent_beta0 * cldsr(i,k)/sqrt(1+cldsr(i,k)*cldsr(i,k)) * &
                                sqrt(abs(buoy_mid(i,k))*cldh(i)) / ( 2*cldrad(i,k)*max(wupmin,w_up_mid(i,k)) )
                        else
                            tmp = 0.0
                        end if
                        if ( buoy_mid(i,k) <= 0 ) then
                            ent_org(i,k) = 0.0
                            det_org(i,k) = tmp * org_enhance &
                                * (max(0.0, pint(i,kupbase(i))-p(i,k))/( max(pint(i,kupbase(i))-p(i,ktop_tmp), 0.0) + 10.0))**org_shape
                        else
                            ent_org(i,k) = tmp * org_enhance & 
                                * (max(0.0, p(i,k)-p(i,ktop_tmp))/( max(pint(i,kupbase(i))-p(i,ktop_tmp), 0.0) + 10.0))**org_shape
                            det_org(i,k) = 0.0                        
                        end if
                    end if
                    if (flagorgent == 2) then
                        tmp = 0.8*sqrt( cldsr(i,k)*cldsr(i,k) + cldsr(i,k)*sqrt(cldsr(i,k)*cldsr(i,k)+6) )
                        tmp = tmp*sqrt(abs(cos(3.14159265*(zint(i,k)-zint(i,kupbase(i)))/cldh(i))))
                        tmp = tmp*sqrt(abs(buoy_mid(i,k))*cldh(i)) / ( 2*cldrad(i,k)*max(wupmin,w_up_mid(i,k)) )
                        if ( zint(i,k)-zint(i,kupbase(i)) > cldh(i)*0.5 ) then
                            ent_org(i,k) = 0.0
                            det_org(i,k) = tmp
                        else
                            ent_org(i,k) = tmp
                            det_org(i,k) = 0.0                        
                        end if
                    end if
                    if (flagorgent == 3) then
                        tmp = orgent_beta0 * cldsr(i,k)/sqrt(1+cldsr(i,k)*cldsr(i,k)) * &
                            sqrt(abs(buoy_mid(i,k))*cldh(i)) / ( 2*cldrad(i,k)*max(wupmin,w_up_mid(i,k)) )
                        ent_org(i,k) = tmp 
                        det_org(i,k) = 0.0
                    end if
                    if (flagorgent == 4) then
                        tmp = 0.8*sqrt( cldsr(i,k)*cldsr(i,k) + cldsr(i,k)*sqrt(cldsr(i,k)*cldsr(i,k)+6) )
                        ! tmp = tmp*sqrt(abs(cos(3.14159265*(zint(i,k)-zint(i,kupbase(i)))/cldh(i))))
                        tmp = tmp*sqrt(abs(buoy_mid(i,k))*cldh(i)) / ( 2*cldrad(i,k)*max(wupmin,w_up_mid(i,k)) )
                        ent_org(i,k) = tmp
                        det_org(i,k) = 0.0
                    end if
                    ent_org(i,k) = min(max_ent_rate, max(0.0, ent_org(i,k)))
                    det_org(i,k) = min(max_det_rate, max(0.0, det_org(i,k)))

                    ! treat organized ent/det as deep plumes
                    ent_rate_dp_up(i,k) = ent_org(i,k)
                    det_rate_dp_up(i,k) = det_org(i,k)

                    
                    ! turbulent entrainment/detrainment
                    bs_scaleh = tmp_zuptop_max-zint(i, kupbase(i) ) 
                    bs_cridis = bs_rle*bs_scaleh
                    bs_thetal_e = t(i,k)*( bs_p0/p(i,k) )**(rair/cpair)
                    
                    if (flagbuoysort == 1) then
                        if (iteration == 1 .or. flagturbent == 1) then
                            bs_wue = w_up(i,k+1)
                            bs_thetal_up = ( t_up(i,k+1)-latvap*qliq_up(i,k+1)/cpair- &
                                (latice+latvap)*qice_up(i,k+1)/cpair ) * ( bs_p0/pint(i,k+1) )**(rair/cpair)
                            !bs_thetal_up =  t_up(i,k+1)  * ( bs_p0/pint(i,k+1) )**(rair/cpair)

                            call cal_buoysort(flagbspdf, bs_cridis, z(i,k), p(i,k), rho(i,k), &
                                bs_thetal_e, q(i,k), bs_thetal_up, q_up(i,k+1)+qliq_up(i,k+1)+qice_up(i,k+1), &
                                bs_wue, bs_xc(i,k), ent_turb(i,k), det_turb(i,k) )
                            !call cal_buoysort(flagbspdf, bs_cridis, z(i,k), p(i,k), rho(i,k), &
                            !    bs_thetal_e, q(i,k), bs_thetal_up, q_up(i,k+1), &
                            !    bs_wue, bs_xc(i,k), ent_turb(i,k), det_turb(i,k) )
                        else
                            bs_wue = 0.5*(w_up(i,k+1) + w_up(i,k))
                            bs_thetal_up = 0.5*( ( t_up(i,k+1)-latvap*qliq_up(i,k+1)/cpair- &
                                (latice+latvap)*qice_up(i,k+1)/cpair ) * ( bs_p0/pint(i,k+1) )**(rair/cpair) &
                                + ( t_up(i,k)-latvap*qliq_up(i,k)/cpair- &
                                (latice+latvap)*qice_up(i,k)/cpair ) * ( bs_p0/pint(i,k) )**(rair/cpair) )

                            call cal_buoysort(flagbspdf, bs_cridis, z(i,k), p(i,k), rho(i,k), &
                                bs_thetal_e, q(i,k), bs_thetal_up, 0.5*( q_up(i,k)+qliq_up(i,k)+qice_up(i,k) + &
                                q_up(i,k+1)+qliq_up(i,k+1)+qice_up(i,k+1) ), &
                                bs_wue, bs_xc(i,k), ent_turb(i,k), det_turb(i,k) )
                        end if
                    
                        ! new PDF for xc 
                        if (flagbspdf == 2) then
                            ent_turb(i,k) = ent_turb(i,k) * ratio_ent_rad/cldrad(i,k)
                            det_turb(i,k) = det_turb(i,k) * ratio_ent_rad/cldrad(i,k)
                        end if
                    end if  ! old buoysort code from CAM UW

                    if (flagbuoysort == 2) then  ! new buoysort code
                        call cal_fracmix(t(i,k), q(i,k), t_up(i,k+1), q_up(i,k+1)+qliq_up(i,k+1)+qice_up(i,k+1), &
                            p(i,k), w_up(i,k+1), bs_cridis, bs_xc(i,k))
                        !write(*,*) "fracmix:"
                        !write(*,"(8E10.3)") t(i,k), q(i,k), t_up(i,k+1), q_up(i,k+1)+qliq_up(i,k+1)+qice_up(i,k+1), &
                        !   p(i,k), w_up(i,k+1), bs_cridis, bs_xc(i,k)

                        call cal_entdet(flagbspdf, bs_xc(i,k), ent_turb(i,k), det_turb(i,k))
                        ent_turb(i,k) = ent_turb(i,k) * ratio_ent_rad/cldrad(i,k)
                        det_turb(i,k) = det_turb(i,k) * ratio_ent_rad/cldrad(i,k)
                    end if
                    
                    ! enhanced turbulent ent/det by w
                    if (turb_enhance > 0) then
                        ent_turb(i,k) = ent_turb(i,k) * turb_enhance/max(wupmin,w_up_mid(i,k))
                        det_turb(i,k) = det_turb(i,k) * turb_enhance/max(wupmin,w_up_mid(i,k))
                    end if

                    ent_turb(i,k) = min(max_ent_rate, max(0.0, ent_turb(i,k)))
                    det_turb(i,k) = min(max_det_rate, max(0.0, det_turb(i,k)))
                    
                    ! treat turbulent ent/det as shallow plumes
                    ent_rate_sh_up(i,k) = ent_turb(i,k)
                    det_rate_sh_up(i,k) = det_turb(i,k)

                    ! total entrainment and detrainment
                    if (flagtotent == 1) then  ! deep (org) only
                        ent_rate = max( 0.0, min( max_ent_rate, &
                            ent_rate_dp_up(i,k) ) )
                        det_rate = max( 0.0, min( max_det_rate, &
                            det_rate_dp_up(i,k) ) ) 
                    end if
                    if (flagtotent == 2) then  ! shallow (turb) only
                        ent_rate = max( 0.0, min( max_ent_rate, &
                            ent_rate_sh_up(i,k) ) )
                        det_rate = max( 0.0, min( max_det_rate, &
                            det_rate_sh_up(i,k) ) ) 
                    end if
                    if (flagtotent == 3) then  ! deep (i.e. organized) + shallow (i.e. turbulence)
                        ent_rate = max( 0.0, min( max_ent_rate, &
                            ent_rate_dp_up(i,k) + ent_rate_sh_up(i,k) ) )
                        det_rate = max( 0.0, min( max_det_rate, &
                            det_rate_dp_up(i,k) + det_rate_sh_up(i,k) ) ) 
                    end if

                    ! final output from iteration
                    if (flagmeaniter == 1) then
                        if (iteration == maxiteration-2) then
                            ent1 = ent_rate
                            det1 = det_rate
                        end if
                        if (iteration == maxiteration-1) then
                            ent2 = ent_rate
                            det2 = det_rate
                        end if
                        if (iteration == maxiteration) then
                            ent_rate = (ent1+ent2)/2.0
                            det_rate = (det1+det2)/2.0
                        end if
                    end if
                    
                    ! inner iteration loop 
                    if (iteration > 1) then
                        if (abs(ent_bak-ent_rate)<1e-5 .and. abs(det_bak-det_rate)<1e-5) then
                            ent_rate = ent_bak
                            det_rate = det_bak
                            exit
                        end if
                    end if
                    ent_bak = ent_rate
                    det_bak = det_rate

                    ! get w**2(i,k)
                    w2 = w_up(i,k+1)*w_up(i,k+1) + 2*dz(i,k)*(orgent_a*buoy_mid(i,k) / &
                        (1+cldsr(i,k)*cldsr(i,k)) - ent_rate*w_up_mid(i,k)*w_up_mid(i,k) )
                    w_up(i,k)  = sqrt(max(w2, 1e-15))

                    ! alias of nominater and denominater
                    nom   = 1.0/dz(i,k) - 0.5*ent_rate
                    denom = max(1e-15, 1.0/dz(i,k) + 0.5*ent_rate)


                    ! normalized mass flux
                    normassflx_up(i,k) = normassflx_up(i,k+1) &
                        * exp( (ent_rate-det_rate)*dz(i,k) )
                    if (abs(normassflx_up(i,k)-normassflx_up(i,k+1))<1e-6) then
                        normassflx_up_mid(i,k) = 0.5*(normassflx_up(i,k)+normassflx_up(i,k+1))
                    else
                        normassflx_up_mid(i,k) = (normassflx_up(i,k)-normassflx_up(i,k+1)) / &
                            (log(normassflx_up(i,k))-log(normassflx_up(i,k+1)))
                    end if

                    ! moisture static energy
                    mse_up(i,k) =  1./denom*( &
                          ent_rate * mse(i,k) &
                        + nom * mse_up(i,k+1) &
                        + mseqi(i,k) / max(1e-15, normassflx_up_mid(i,k)) )
                    ! in-cloud temperature and moisture
                    ! ---- method 1: Taylor expanding ----
                    if (mse2tsatflag == 1) then
                        call cal_mse2tsat(mse_up(i,k), tint(i,k), &
                            qsatint(i,k), msesatint(i,k), t_up(i,k) )
                        call cal_qsat(t_up(i,k), pint(i,k), q_up(i,k))
                    end if
                    ! ---- method 2: bi-section ----
                    if (mse2tsatflag == 2) then
                        call mse2tsat( mse_up(i,k), zint(i,k), pint(i,k), t_up(i,k), q_up(i,k) )
                    end if

                    ! condensation
                    condrate(i,k) = normassflx_up_mid(i,k)/rho(i,k) *( &
                          ent_rate * q(i,k)   &
                        + nom   * q_up(i,k+1) &
                        - denom * q_up(i,k)   )

                    ! negative condensation adjustment
                    if (condrate(i,k) < 0) then
                        condrate(i,k) = 0.0
                        q_up(i,k) = 1.0/denom * ( &
                              ent_rate * q(i,k) &
                            + nom * q_up(i,k+1) )
                        t_up(i,k) = ( mse_up(i,k) - gravit*zint(i,k) &
                            - (latvap+(cpliq-cpwv)*273.15)*q_up(i,k) ) &
                            / max(1e-15, cpair-(cpliq-cpwv)*q_up(i,k))
                    end if

                    ! phase conversion rate
                    Fp = max(0.0, 1.0 - exp(-(z(i,k) - zint(i,kupbase(i)) - rain_z0)/rain_zp) )
                    Fi = 0.0
                    if (cloud_t1 < t_up(i,k) .and. t_up(i,k) < cloud_t2) then
                        Fi = (cloud_t2-t_up(i,k)) / (cloud_t2-cloud_t1)
                    else if (t_up(i,k) <= cloud_t1) then
                        Fi = 1.0
                    end if

                    frezrate(i,k) = Fi * condrate(i,k)
                    rainrate(i,k) = (1.0-Fi) * Fp * condrate(i,k)
                    snowrate(i,k) = Fi * Fp * condrate(i,k)
                    precrate(i,k) = rainrate(i,k) + snowrate(i,k)

                    ! in-cloud liquid and ice water
                    qliq_up(i,k) =  1.0/denom*( &
                          nom * qliq_up(i,k+1) &
                        + rho(i,k)*(condrate(i,k)-frezrate(i,k)-rainrate(i,k)) / &
                            max(1e-15, normassflx_up_mid(i,k)) ) 
                    qice_up(i,k) =  1./denom*( &
                          nom * qice_up(i,k+1) &
                        + rho(i,k)*(frezrate(i,k)-snowrate(i,k)) / &
                            max(1e-15, normassflx_up_mid(i,k)) )

                    ! contribution of freezing to moisture static energy
                    if (iteration < maxiteration .and. mseqiflag > 0) then
                        mseqi(i,k) = latice*rho(i,k)*frezrate(i,k)
                    end if

                    ! buoyancy at upper interfacial layer
                    tv = tint(i,k)*(1+tveps*qint(i,k))
                    tv_up = t_up(i,k)*( 1+tveps*q_up(i,k) )
                    
                    if (buoyflag == 1) then
                        buoy(i,k) = gravit* ( (tv_up-tv)/tv ) 
                    end if
                    if (buoyflag == 2) then
                        buoy(i,k) = gravit* ( (tv_up-tv)/tv - qliq_up(i,k) - qice_up(i,k) ) 
                    end if

                end do   ! loop of iteration for Qi
                
                if (ctopflag == 1) then 
                    if (buoy(i,k) < 0.0 .or. w_up(i,k) < wupmin) then
                        exit
                    end if
                end if
                
                if (ctopflag == 2) then
                    if (w_up(i,k) < wupmin) then
                        exit
                    end if
                end if

            end do  ! loop for levels
          
            ktop_tmp = max(1, k)    ! update for calculating the shapes of ent_org and det_org

            if (fixcldsr < 0) then
                if (k == kupbase(i)-1) then
                    cldh(i) = 0.0
                    exit
                else
                    cldh(i) = max( 1.0, min( tmp_zuptop_max, zint(i,k+1) ) - zint(i,kupbase(i)) )
                    
                    if (abs(cldh(i)-cldh_bak) < 1.0) then
                        exit
                    else
                        cldh_bak = cldh(i)
                    end if
                end if
            end if

        end do   ! iteration for cloud height


        if (k>=1) then
            mse_up(i,1:k) = mseint(i,1:k)
            t_up(i,1:k) = tint(i,1:k)
            q_up(i,1:k) = qint(i,1:k)
            qliq_up(i,1:k) = 0._r8
            qice_up(i,1:k) = 0._r8
            w_up(i,1:k) = 0._r8
            buoy(i,1:k) = 0._r8
            det_rate_dp_up(i,1:k) = 0._r8
            det_rate_sh_up(i,1:k) = 0._r8
            det_org(i,1:k) = 0.0
            det_turb(i,1:k) = 0.0
            ent_rate_dp_up(i,1:k) = 0.0
            ent_rate_sh_up(i,1:k) = 0.0
            det_org(i,1:k) = 0.0
            det_turb(i,1:k) = 0.0
            normassflx_up(i,1:k) = 0._r8

            condrate(i,1:k) = 0._r8
            rainrate(i,1:k) = 0._r8
            snowrate(i,1:k) = 0._r8
            precrate(i,1:k) = 0._r8
            mseqi(i,1:k) = 0._r8
            qliq_up(i,1:k) = 0._r8
            qice_up(i,1:k) = 0._r8


            kuptop(i) = k+1
            if ( kuptop(i) /= nlev ) then
                zuptop(i) = zint( i, kuptop(i) )
            end if

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
            det_rate_dp_up(i,k) = 0._r8
            det_rate_sh_up(i,k) = 0._r8
            det_org(i,k) = 0.0
            det_turb(i,k) = 0.0
            ent_rate_dp_up(i,k) = 0.0
            ent_rate_sh_up(i,k) = 0.0
            det_org(i,k) = 0.0
            det_turb(i,k) = 0.0
            normassflx_up(i,k) = 0._r8

            condrate(i,k) = 0._r8
            rainrate(i,k) = 0._r8
            snowrate(i,k) = 0._r8
            precrate(i,k) = 0._r8
            mseqi(i,k) = 0._r8
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8

            kuptop(i) = 2
            if ( kuptop(i) /= nlev ) then
                zuptop(i) = zint( i, kuptop(i) )
            end if

        end if

    end do

end subroutine cal_mse_up

! ==============================================================================
! calculate updraft properties (old scheme from CS2010 for GRE and NSJ)
! ==============================================================================
subroutine cal_mse_up_old( &
!input
        ent_opt, rho, z, zint, dz, p, pint, t, tint, q, qint, qsat, qsatint, &
        mse, mseint, msesat, msesatint, kuplaunch, kupbase, &
!in/output
        ent_rate_dp_up, det_rate_dp_up, ent_rate_sh_up, det_rate_sh_up, bs_xc, w_up_init, &
        mse_up, t_up, q_up, qliq_up, qice_up, mseqi, condrate, rainrate, snowrate, precrate, &
        normassflx_up, w_up, w_up_mid, buoy, buoy_mid, kuptop, zuptop, &
        trig)

     use buoysort, only : cal_buoysort

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

    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_dp_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_dp_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: ent_rate_sh_up ! [1]
    real(r8), dimension(ncol, nlev), intent(inout) :: det_rate_sh_up ! [1]
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
    real(r8), dimension(ncol, nlev )  :: frezrate  ! [m2/kg]

    real(r8), dimension(ncol, nlevp)  :: ent_rate_dp_up_int ! [1]
    real(r8), dimension(ncol, nlevp)  :: ent_rate_sh_up_int ! [1]
    real(r8), dimension(ncol, nlevp)  :: det_rate_sh_up_int ! [1]

    real(r8) :: tmp_t_up, tmp_q_up, tmp_buoy, tmp_zuptop_max

    real(r8) :: tv, tv_up,  w2, qw, Fp, Fi, Ek, tmp
    real(r8) :: nom, denom, ent_rate, det_rate, massflxmid

    real(r8) :: bs_p0, bs_wue, bs_rle, bs_scaleh, bs_cridis, bs_thetalint, bs_thetal_up
    real(r8), dimension(ncol, nlev ), intent(inout) :: bs_xc

    integer :: i,j,k, iteration
    integer :: ngbuoy

    bs_p0 = 1000.e2_r8
    bs_rle = 0.1_r8

    !intialize output.
    qliq_up = 0.0
    qice_up = 0.0
    mseqi = 0.0
    frezrate = 0.0
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

    ent_rate_dp_up_int = 0._r8
    det_rate_dp_up = 0._r8
    ent_rate_sh_up = 0._r8
    det_rate_sh_up = 0._r8
    ent_rate_sh_up_int = 0._r8
    det_rate_sh_up_int = 0._r8
    bs_xc = 0._r8

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

        do k=kupbase(i)-1, 1, -1
            call cal_mse2tsat( mse_up(i,kupbase(i)), tint(i,k), &
                qsatint(i,k), msesatint(i,k), tmp_t_up )
            call cal_qsat(tmp_t_up, pint(i,k), tmp_q_up)

            tv = tint(i,k)*(1+tveps*qint(i,k))
            tv_up = tmp_t_up*(1+tveps*tmp_q_up )
            tmp_buoy = gravit*(tv_up-tv)/tv 
            if ( tmp_buoy <= 0. ) then
                tmp_zuptop_max = zint(i,k)
                exit
            end if
        end do
        if ( k<1 ) tmp_zuptop_max = zint(i, 1)

        k = kupbase(i)

        tv = tint(i,k)*(1 + tveps*qint(i,k) )
        tv_up = t_up(i,k)*(1 + tveps*q_up(i,k) )
        buoy(i,k) = gravit*(tv_up-tv)/tv 

        w_up(i, kupbase(i) )  = w_up_init(i)


        !if ( buoy(i,k)<0. ) then
            !trig(i) = -10
            !cycle
        !end if

        tmp = max(w_up(i,k), wupmin)
        if ( ent_opt == 2 ) then
            ent_rate_dp_up_int(i,k) = greg_ce*greg_ent_a*buoy(i,k)/tmp/tmp
        else if ( ent_opt == 3 ) then
            ent_rate_dp_up_int(i,k) = nsj_coef/tmp
        else
            ent_rate_dp_up_int(i,k) = 0
        end if
        ent_rate_dp_up_int(i,k) = max(0.0, min( max_ent_rate,  ent_rate_dp_up_int(i,k)))

        !write(*,*) k
        !write(*,"(10f20.10)") tv, tv_up, tint(i,k), qint(i,k), buoy(i,k), tv_up, tv, ent_rate_dp_up_int(i,k)

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
                    ent_rate_dp_up(i,k) = ent_rate_dp_up_int(i,k+1)
                    ent_rate_sh_up(i,k) = ent_rate_sh_up_int(i,k+1)
                    det_rate_sh_up(i,k) = det_rate_sh_up_int(i,k+1)
                else
                    w_up_mid(i,k) = 0.5 * ( w_up(i,k)+w_up(i,k+1) )
                    buoy_mid(i,k) = 0.5 * ( buoy(i,k)+buoy(i,k+1) )
                    
                    if (entratemidflag == 1) then
                        ent_rate_dp_up(i,k) = 0.5 * ( ent_rate_dp_up_int(i,k) + ent_rate_dp_up_int(i,k+1) )
                        ent_rate_sh_up(i,k) = 0.5 * ( ent_rate_sh_up_int(i,k) + ent_rate_sh_up_int(i,k+1) )
                        det_rate_sh_up(i,k) = 0.5 * ( det_rate_sh_up_int(i,k) + det_rate_sh_up_int(i,k+1) )
                    end if
                    if (entratemidflag == 2) then
                        tmp = max(wupmin, w_up_mid(i,k))
                        if ( ent_opt == 2 ) then
                            ent_rate_dp_up(i,k) = &
                                greg_ce*greg_ent_a*buoy_mid(i,k)/tmp/tmp
                        else if ( ent_opt == 3 ) then
                            ent_rate_dp_up(i,k) = nsj_coef/tmp
                        else
                            ent_rate_dp_up(i,k) = 0
                        end if
                    end if
                end if
                ent_rate_dp_up(i,k) = max(0.0, &
                    min( max_ent_rate,  ent_rate_dp_up(i,k)))

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

                ent_rate = ent_rate_dp_up(i,k) + ent_rate_sh_up(i,k)
                det_rate = det_rate_dp_up(i,k) + det_rate_sh_up(i,k)

!flux form
                !normassflx_up(i,k) = normassflx_up(i,k+1)*exp(ent_rate_dp_up(i,k)*dz(i,k) )
                !Ek = ( normassflx_up(i,k) - normassflx_up(i,k+1) ) / dz(i,k)
                !mse_up(i,k) = ( normassflx_up(i,k+1)*mse_up(i,k+1) &
                                !+ Ek*mse(i,k)*dz(i,k) + mseqi(i,k)*dz(i,k) ) &
                                !/normassflx_up(i,k)
                !write(*,*) "old"
                !write(*,"(2i2,10f20.10)") k, iteration, ent_rate, normassflx_up(i,k), &
                    !mse_up(i,k), mse(i,k), mse_up(i,k+1)
!scalar form
                normassflx_up(i,k) = normassflx_up(i,k+1) &
                    *exp( (ent_rate-det_rate)*dz(i,k) )
                denom = 1. + 0.5*ent_rate*dz(i,k)
                mse_up(i,k) =  1./denom*( &
                      ent_rate*dz(i,k)*mse(i,k) &
                    + ( 1 - 0.5*ent_rate*dz(i,k) )*mse_up(i,k+1) &
                    + dz(i,k)/( 0.5*( normassflx_up(i,k)+normassflx_up(i,k+1) ) )*mseqi(i,k) )
                !write(*,*) "new"
                !write(*,"(2i2,10f20.10)") k, iteration, ent_rate, normassflx_up(i,k), &
                    !mse_up(i,k), mse(i,k), mse_up(i,k+1)

                
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

                Fp = max(0.0, 1.0 - exp(-(z(i,k) - zint(i,kupbase(i)) - rain_z0)/rain_zp) )
!                fp = 1._r8
                Fi = 0.0
                if (cloud_t1 < t_up(i,k) .and. t_up(i,k) < cloud_t2) then
                    Fi = (cloud_t2-t_up(i,k)) / (cloud_t2-cloud_t1)
                else if (t_up(i,k) <= cloud_t1) then
                    Fi = 1.0
                end if

!flux form
                !condrate(i,k) = 1.0/rho(i,k)*( Ek*q(i,k) &
                    !-( normassflx_up(i,k)*q_up(i,k) &
                    !-normassflx_up(i,k+1)*q_up(i,k+1) )/dz(i,k) )
                !write(*,*) "old cond"
                !write(*,"(2i2,10f20.10)") k, iteration, condrate(i,k)
!scalar form
                condrate(i,k) = 1./dz(i,k)/rho(i,k) &
                    *( 0.5*( normassflx_up(i,k)+normassflx_up(i,k+1) ) ) &
                    *( -denom*q_up(i,k) + ent_rate*dz(i,k)*q(i,k) &
                       +( 1 - 0.5*ent_rate*dz(i,k) )*q_up(i,k+1) )
                !write(*,*) "new cond"
                !write(*,"(2i2,10f20.10)") k, iteration, condrate(i,k)


                frezrate(i,k) = Fi * condrate(i,k)
                rainrate(i,k) = (1.0-Fi) * Fp * condrate(i,k)
                snowrate(i,k) = Fi * Fp * condrate(i,k)
                precrate(i,k) = rainrate(i,k) + snowrate(i,k)


!flux form fi*rate
                !qliq_up(i,k) = (normassflx_up(i,k+1)*( qliq_up(i,k+1) ) + &
                    !rho(i,k)*( condrate(i,k)-frezrate(i,k)-rainrate(i,k) )*dz(i,k) )/normassflx_up(i,k)
                !qice_up(i,k) = (normassflx_up(i,k+1)*( qice_up(i,k+1) ) + &
                    !rho(i,k)*( frezrate(i,k)-snowrate(i,k) )*dz(i,k) )/normassflx_up(i,k)
                !write(*,*) "old liq and ice rate"
                !write(*,"(2i2,10f20.10)") k, iteration, qliq_up(i,k), qice_up(i,k)
!flux form fi*qw
                !qw = (normassflx_up(i,k+1)*( qliq_up(i,k+1)+qice_up(i,k+1) ) + &
                    !rho(i,k)*(1.0-Fp)*condrate(i,k)*dz(i,k) )/normassflx_up(i,k)
                !qliq_up(i,k) = (1.0-Fi) * qw
                !qice_up(i,k) = Fi * qw
                !write(*,*) "old liq and ice qw"
                !write(*,"(2i2,10f20.10)") k, iteration, qliq_up(i,k), qice_up(i,k)
!sclar form fi*rate
                qliq_up(i,k) =  1./denom*( &
                    + ( 1 - 0.5*ent_rate*dz(i,k) )*qliq_up(i,k+1) &
                    + dz(i,k)/( 0.5*( normassflx_up(i,k)+normassflx_up(i,k+1) ) ) &
                      *rho(i,k)*( condrate(i,k)-frezrate(i,k)-rainrate(i,k) ) )
                qice_up(i,k) =  1./denom*( &
                    + ( 1 - 0.5*ent_rate*dz(i,k) )*qice_up(i,k+1) &
                    + dz(i,k)/( 0.5*( normassflx_up(i,k)+normassflx_up(i,k+1) ) ) &
                      *rho(i,k)*( frezrate(i,k)-snowrate(i,k) ) )
                !write(*,*) "new liq and ice rate"
                !write(*,"(2i2,10f20.10)") k, iteration, qliq_up(i,k), qice_up(i,k)
!scalar form fi*qw
                !qw =  1./denom*( &
                    !+ ( 1 - 0.5*ent_rate*dz(i,k) )*( qliq_up(i,k+1)+qice_up(i,k+1)  )&
                    !+ dz(i,k)/( 0.5*( normassflx_up(i,k)+normassflx_up(i,k+1) ) ) &
                      !*rho(i,k)*(1.0-Fp)*condrate(i,k) )
                !qliq_up(i,k) = (1.0-Fi) * qw
                !qice_up(i,k) = Fi * qw
                !write(*,*) "new liq and ice qw"
                !write(*,"(2i2,10f20.10)") k, iteration, qliq_up(i,k), qice_up(i,k)

!MJO
                qliq_up(i,k) = max( 0., qliq_up(i,k) )
                qice_up(i,k) = max( 0., qice_up(i,k) )

                if (iteration < maxiteration) then
                    if (mseqiflag > 0) then
                        mseqi(i,k) = latice*rho(i,k)*frezrate(i,k)
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

                tmp = max(wupmin, w_up(i,k))
                if ( ent_opt == 2 ) then
                    ent_rate_dp_up_int(i,k) = greg_ce*greg_ent_a*buoy(i,k)/tmp/tmp
                else if ( ent_opt == 3 ) then
                    ent_rate_dp_up_int(i,k) = nsj_coef/tmp
                else
                    ent_rate_dp_up_int(i,k) = 0.0
                end if
                ent_rate_dp_up_int(i,k) = max(0.0, min( max_ent_rate,  ent_rate_dp_up_int(i,k)))                


                if ( bsflag == 1 ) then

                    bs_scaleh = tmp_zuptop_max-zint(i, kupbase(i) ) 
                    bs_cridis = bs_rle*bs_scaleh
                    bs_wue = w_up(i,k)
                    bs_thetalint = tint(i,k)*( bs_p0/pint(i,k) )**(rair/cpair)
                    bs_thetal_up = ( t_up(i,k)-latvap*qliq_up(i,k)/cpair-latice*qice_up(i,k)/cpair  ) &
                        *( bs_p0/pint(i,k) )**(rair/cpair)
                    ! Haiyang
                    call cal_buoysort(flagbspdf, bs_cridis, zint(i,k), pint(i,k), rho(i,k), &
                        bs_thetalint, qint(i,k), bs_thetal_up, q_up(i,k)+qliq_up(i,k)+qice_up(i,k), &
                        bs_wue, bs_xc(i,k), ent_rate_sh_up_int(i,k), det_rate_sh_up_int(i,k) )

                end if

            end do   ! loop of iteration

            
            if (ctopflag == 1) then 
                if (buoy(i,k) < 0.0 .or. w_up(i,k)<wupmin) then
                    exit
                end if
            end if
            
            if (ctopflag == 2) then
                if (w_up(i,k) < wupmin) then
                    exit
                end if
            end if

            if (negcondflag == 0) then
                if ( condrate(i,k)<0 ) then
                    condrate(i,k) = 0.
                    frezrate(i,k) = 0.
                    rainrate(i,k) = 0.
                    snowrate(i,k) = 0.
                    precrate(i,k) = 0.
                    qliq_up(i,k) =  0.
                    qice_up(i,k) =  0.
                    exit
                end if
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
            ent_rate_dp_up_int(i,k) = 0._r8
            det_rate_dp_up(i,k) = 0._r8
            normassflx_up(i,k) = 0._r8

            condrate(i,k) = 0._r8
            rainrate(i,k) = 0._r8
            snowrate(i,k) = 0._r8
            precrate(i,k) = 0._r8
            mseqi(i,k) = 0._r8
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8

            ent_rate_dp_up_int(i,kupbase(i) ) = 0._r8

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
            ent_rate_dp_up_int(i,k) = 0._r8
            det_rate_dp_up(i,k) = 0._r8
            normassflx_up(i,k) = 0._r8

            condrate(i,k) = 0._r8
            rainrate(i,k) = 0._r8
            snowrate(i,k) = 0._r8
            precrate(i,k) = 0._r8
            mseqi(i,k) = 0._r8
            qliq_up(i,k) = 0._r8
            qice_up(i,k) = 0._r8

            ent_rate_dp_up_int(i,kupbase(i) ) = 0._r8

            kuptop(i) = 2

        end if

    end do

end subroutine cal_mse_up_old

! ==============================================================================
! calculate downdraft 
! ==============================================================================
subroutine cal_mse_dn( &
!input
        ent_opt, kuptop, trig, dz, zint, p, pint, rho, t, twet, twetint, lvmid, &
        qint, dseint, accuprec, evaprate, buoy_mid, dn_frac, &
!output
        dse_dn, q_dn, normassflx_dn )

!input
    integer, intent(in) :: ent_opt
    integer , dimension(ncol), intent(in) :: trig     ! [1]
    integer , dimension(ncol), intent(in) :: kuptop    ! [1]
    real(r8), dimension(ncol, nlev),  intent(in) :: dz    ! [m]
    real(r8), dimension(ncol, nlevp),  intent(in) :: zint    ! [m]
    real(r8), dimension(ncol, nlev),  intent(in) :: p     ! [Pa]
    real(r8), dimension(ncol, nlevp),  intent(in) :: pint     ! [Pa]
    real(r8), dimension(ncol, nlev),  intent(in) :: rho   ! [kg/m3]
    real(r8), dimension(ncol, nlev),  intent(in) :: t     ! [K]
    real(r8), dimension(ncol, nlev), intent(in)  :: twet  ! [K]
    real(r8), dimension(ncol, nlevp), intent(in)  :: twetint  ! [K]
    real(r8), dimension(ncol, nlev), intent(in)  :: lvmid  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: qint  ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(in) :: dseint  ! [J/kg]
    real(r8), dimension(ncol, nlev), intent(in)  :: accuprec ! [#]
    real(r8), dimension(ncol, nlev), intent(in)  :: evaprate ! [1/m]
    real(r8), dimension(ncol, nlev), intent(in)  :: buoy_mid
    real(r8), intent(in)  :: dn_frac ! [#]

!output
    real(r8), dimension(ncol, nlevp), intent(inout) :: dse_dn  ! [J/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: q_dn  ! [kg/kg]
    real(r8), dimension(ncol, nlevp), intent(inout) :: normassflx_dn  ! [kg/m2/s]

!local
    real(r8) :: fac
    integer  :: i,j,k, kdntop
    real(r8), dimension(ncol, nlevp) :: qswetint, dsewetint

    dsewetint = cpair * twetint + gravit * zint
    call cal_qsat2d(twetint(:,:), pint(:,:), qswetint(:,:))

    dse_dn = dseint
    q_dn = qint
    normassflx_dn = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        ! downdraft starts at the layer lower than the neutral buoyancy layer
        kdntop = nlev
        do k = kuptop(i), nlev
            if (buoy_mid(i,k) > 0) then
                kdntop = k
                exit
            end if
        end do
        kdntop = min(kdntop+1, nlev)
        
        normassflx_dn(i,kdntop) = -1.0_r8 * dn_frac
        q_dn(i,kdntop) = qint(i,kdntop)
        dse_dn(i,kdntop) = dseint(i,kdntop)

        do k=kdntop, nlev
            normassflx_dn(i,k+1) = normassflx_dn(i,k) + dn_be*rho(i,k) * &
                min(0.0, twet(i,k)-t(i,k)) * accuprec(i,k) * dz(i,k)
            if (zpbltop > 0) then
                fac = min(1.0, max(0.0, (zint(i,k+1)-zint(i,nlevp))/zpbltop ))
            else
                fac = 1.0
            end if
            normassflx_dn(i,k+1) = normassflx_dn(i,k+1) * fac
            q_dn(i,k+1) = qswetint(i,k+1)
            dse_dn(i,k+1) = dsewetint(i,k+1)

            !if (abs(normassflx_dn(i,k+1)) < 1.0e-3) then
            !    dse_dn(i,k+1) = dse_dn(i,k)
            !    q_dn(i,k+1) = q_dn(i,k)
            !else
            !    dse_dn(i,k+1) = (normassflx_dn(i,k)*dse_dn(i,k) + &
            !        lvmid(i,k)*evaprate(i,k)*dz(i,k)) / normassflx_dn(i,k+1)
            !    q_dn(i,k+1) = (normassflx_dn(i,k)*q_dn(i,k) - &
            !        evaprate(i,k)*dz(i,k)) / normassflx_dn(i,k+1)
            !end if
        end do
        
        ! normassflx_dn(i,nlev+1) = 0._r8

    end do


end subroutine cal_mse_dn

! ==============================================================================
! calculate evaporation
! ==============================================================================
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
            evaprate(i,k) = min( dn_ae*max( 0._r8, qsat_tmp-q(i,k) ) * &
                    accuprec(i,k) / dn_vt / rho(i,k), accuprec(i,k)/rho(i,k)/dz(i,k) ) &
                    * evap_enhance * ((p(i,k)-p(i,kuptop(i)))/(p(i,nlev)-p(i,kuptop(i))+10.0))**evap_shape
            evaprate(i,k) = min(evaprate(i,k), accuprec(i,k)/rho(i,k)/dz(i,k))

            accuprec(i,k) = max(0.0, accuprec(i,k) - evaprate(i,k)*rho(i,k)*dz(i,k))

        end do
        surfprec(i) = accuprec(i,nlev)
    end do

end subroutine cal_evap


! ==============================================================================
! calculate CAPE
! ==============================================================================
subroutine cal_cape( &
!input
        dz, buoy_mid, normassflx_up, kupbase, kuptop, &
!output
        cape, cwf, &
!in/out
        trig)
!------------------------------------------------------
!calculate CAPE given buoyancy
!------------------------------------------------------
!input
    real(r8), dimension(ncol, nlev), intent(in) :: dz  ! [m]
    real(r8), dimension(ncol, nlev), intent(in) :: buoy_mid  ! [ms-2]
    real(r8), dimension(ncol, nlevp), intent(in) :: normassflx_up  ! [ms-2]
    integer, dimension(ncol), intent(in) :: kupbase ! [1]
    integer, dimension(ncol), intent(in) :: kuptop ! [1]
!output
    real(r8), dimension(ncol), intent(out) :: cape  ! [kgm-2-s]
    real(r8), dimension(ncol), intent(out) :: cwf   ! [kgm-2-s]
!input/output
    integer, dimension(ncol), intent(inout) :: trig     ! [1]
!local
    integer :: i,j,k

!intialize output
    cape = 0._r8
    cwf  = 0._r8

    do i=1, ncol
        if ( trig(i) < 1 ) cycle
        do k=kupbase(i)-1, kuptop(i), -1
            cape(i) = cape(i) + dz(i,k)*max(buoy_mid(i,k), 0._r8)
            cwf(i) = cwf(i) + dz(i,k)*max(buoy_mid(i,k), 0._r8)&
                *0.5*(normassflx_up(i,k)+normassflx_up(i,k+1) )
            if ( isnan(cwf(i)) ) then
                write(*,*) i, normassflx_up(i,:)
            end if
        end do
    end do
end subroutine cal_cape

! ==============================================================================
! calculate the tendency due to transport
! ==============================================================================
subroutine cal_tendtransport( &
!input
        dz, kupbase, kuptop, &
        rho, dseint, qint, dse_up, q_up, &
        normassflx_up,  &
!output
        stend, qtend, &
!in/out
        trig)
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

!cloud layer transport
        do k=kupbase(i)-1, kuptop(i), -1
            stend(i,k) = -( normassflx_up(i,k)*( dse_up(i,k)-dseint(i,k) ) &
                - normassflx_up(i,k+1)*( dse_up(i,k+1)-dseint(i,k+1) ) ) / dz(i,k)/rho(i,k)
            qtend(i,k) = -( normassflx_up(i,k)*( q_up(i,k)-qint(i,k) ) &
                - normassflx_up(i,k+1)*( q_up(i,k+1)-qint(i,k+1) ) ) / dz(i,k)/rho(i,k)
        end do

        ! one layer above plume
        k = max(1, kuptop(i)-1)
        stend(i,k) = -( 0.0 &
            - normassflx_up(i,k+1)*( dse_up(i,k+1)-dseint(i,k+1) ) ) / dz(i,k)/rho(i,k)
        qtend(i,k) = -( 0.0 &
            - normassflx_up(i,k+1)*( q_up(i,k+1)-qint(i,k+1) ) ) / dz(i,k)/rho(i,k)
       
        ! one layer below plume
        k = min(kupbase(i), nlev)
        stend(i,k) = -( normassflx_up(i,k)*( dse_up(i,k)-dseint(i,k) ) &
            - 0.0 ) / dz(i,k)/rho(i,k)
        qtend(i,k) = -( normassflx_up(i,k)*( q_up(i,k)-qint(i,k) ) &
            - 0.0 ) / dz(i,k)/rho(i,k)


    end do

end subroutine cal_tendtransport

! ==============================================================================
! calculate tempeature and saturated Q given MSE and pressure with bi-section
! ==============================================================================
subroutine mse2tsat( mse, z, p, t, q)
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
            n = n + 1
        end do
        t = tc
    end if
    q = 0.622*611/p*exp( 5417.*(1/273.-1/tb) )

end subroutine mse2tsat

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

! ==============================================================================
! T, RH --> wet-bulb temperture
! ==============================================================================
subroutine cal_twet2d(t, rh, twet)
    real(r8) :: t(:,:), rh(:,:), twet(:,:)
    twet = (t-273.16)*atan( 0.151977*(rh*100.+8.313659)**0.5 ) + atan(t-273.16+rh*100.) &
        - atan(rh*100.-1.676331) + 0.00391838*((rh*100.)**1.5)*atan(0.023101*rh*100.) - 4.686035 &
        + 273.16
end subroutine cal_twet2d


end module conv_jp

