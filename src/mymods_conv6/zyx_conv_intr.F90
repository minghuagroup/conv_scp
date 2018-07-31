module zyx_conv_intr

use shr_kind_mod, only: r8=>shr_kind_r8
use ppgrid,       only: pver, pcols, pverp, begchunk, endchunk
use pmgrid,         only: plev,plevp
use spmd_utils,     only: masterproc
use perf_mod

use physconst,    only: cpair, gravit
use phys_control,   only: phys_deepconv_pbl, phys_getopts, cam_physpkg_is, plume_model

use physics_types, only: physics_state, physics_ptend
use physics_types, only: physics_ptend_init, physics_ptend_sum
use physics_types, only: physics_update, physics_ptend_dealloc
use physics_types, only: physics_state_copy, physics_state_dealloc
use time_manager,  only: get_nstep, is_first_step
use error_messages, only: alloc_err


use constituents,  only: pcnst, cnst_get_ind, cnst_is_convtran1, qmin

use cam_history,  only: outfld, addfld, add_default, phys_decomp

use cam_logfile,  only: iulog

!MZ
use zyx_conv,     only: zyx_conv_init, zyx_conv_tend, zyx_conv_evap, convtran, momtran !, zyx_convi
use mod_foutput,     only: foutput_1d, foutput_2d

implicit none
private
save

public :: zyx_conv_intr_init, zyx_conv_intr_register, zyx_conv_intr_tend, zyx_conv_tend_2

  
   real(r8), allocatable, dimension(:,:,:) :: mu  !(pcols,pver,begchunk:endchunk)
   real(r8), allocatable, dimension(:,:,:) :: eu  !(pcols,pver,begchunk:endchunk)
   real(r8), allocatable, dimension(:,:,:) :: du  !(pcols,pver,begchunk:endchunk)
   real(r8), allocatable, dimension(:,:,:) :: md  !(pcols,pver,begchunk:endchunk)
   real(r8), allocatable, dimension(:,:,:) :: ed  !(pcols,pver,begchunk:endchunk)
   real(r8), allocatable, dimension(:,:,:) :: dp  !(pcols,pver,begchunk:endchunk) 
	! wg layer thickness in mbs (between upper/lower interface).
   real(r8), allocatable, dimension(:,:)   :: dsubcld  !(pcols,begchunk:endchunk)
	! wg layer thickness in mbs between lcl and maxi.

   integer, allocatable, dimension(:,:) :: jt   !(pcols,begchunk:endchunk)
        ! wg top  level index of deep cumulus convection.
   integer, allocatable, dimension(:,:) :: maxg !(pcols,begchunk:endchunk)
        ! wg gathered values of maxi.
   integer, allocatable, dimension(:,:) :: ideep !(pcols,begchunk:endchunk)               
	! w holds position of gathered points vs longitude index

   integer, allocatable, dimension(:) :: lengath !(begchunk:endchunk)

   integer ::& ! indices for fields in the physics buffer
      dp_flxprc_idx, &
      dp_flxsnw_idx, &
      dp_cldliq_idx, &
      dp_cldice_idx, &
      prec_dp_idx,   &
      snow_dp_idx

!  indices for fields in the physics buffer
   integer  ::    cld_idx          = 0    
   integer  ::    icwmrdp_idx      = 0     
   integer  ::    rprddp_idx       = 0    
   integer  ::    fracis_idx       = 0   
   integer  ::    nevapr_dpcu_idx  = 0    


!  indices for fields in the physics buffer

integer  ::  pblh_idx        = 0
integer  ::  tpert_idx       = 0

integer  ::  cldtop_idx       = 0
integer  ::  cldbot_idx       = 0

!used since CESM V1.2.2
!integer  :: dp_flxprc_idx = 0
!integer  :: dp_flxsnw_idx = 0
!integer  :: dp_cldliq_idx = 0
!integer  :: dp_cldice_idx = 0
integer  :: massflxbase_p_idx = 0

!xiex
integer :: bfls_t_idx = 0
integer :: bfls_q_idx = 0

!MZ
   character(len=16) :: deep_scheme    ! default set in phys_control.F90, use namelist to change
   !character(len=16) :: plume_model


contains

!------------------------------------------------------
subroutine zyx_conv_intr_register
!------------------------------------------------------
!register for memory and so on
!------------------------------------------------------
    use physics_buffer, only : pbuf_add_field, dtype_r8

!MZ
!MZ  this was originally in the chemistry ...src/chemistry/bulk_aero/aerosol_intr.F90
!      !call pbuf_add('FRACIS' , 'physpkg', 1,pver, pcnst, fracis_idx)
!      call pbuf_add_field('FRACIS','global',dtype_r8,(/pcols,pver,pcnst/),fracis_idx)

!used since CESM V1.2.2
! Flux of precipitation from deep convection (kg/m2/s)
   call pbuf_add_field('DP_FLXPRC','global',dtype_r8,(/pcols,pverp/),dp_flxprc_idx)
! Flux of snow from deep convection (kg/m2/s)
   call pbuf_add_field('DP_FLXSNW','global',dtype_r8,(/pcols,pverp/),dp_flxsnw_idx)
! deep gbm cloud liquid water (kg/kg)
   call pbuf_add_field('DP_CLDLIQ','global',dtype_r8,(/pcols,pver/), dp_cldliq_idx)
! deep gbm cloud liquid water (kg/kg)
   call pbuf_add_field('DP_CLDICE','global',dtype_r8,(/pcols,pver/), dp_cldice_idx)

end subroutine zyx_conv_intr_register



!------------------------------------------------------
subroutine zyx_conv_intr_init(pref_edge)

!MZ<
!----------------------------------------
! Purpose:  declare output fields, initialize variables needed by convection
!----------------------------------------

  use cam_history,    only: outfld, addfld, add_default, phys_decomp
  use ppgrid,         only: pcols, pver
!MZ  use zm_conv,        only: zm_convi
  use pmgrid,         only: plev,plevp
  use spmd_utils,     only: masterproc
  use error_messages, only: alloc_err
  use phys_control,   only: phys_deepconv_pbl, phys_getopts, cam_physpkg_is
  use physics_buffer, only: pbuf_get_index

  implicit none

     real(r8),intent(in) :: pref_edge(plevp)        ! reference pressures at interfaces


  logical :: no_deep_pbl    ! if true, no deep convection in PBL
  integer  limcnv           ! top interface level limit for convection
  integer k, istat
  logical :: history_budget ! output tendencies and state variables for CAM4
                            ! temperature, water vapor, cloud ice and cloud
                            ! liquid budgets.
  integer :: history_budget_histfile_num ! output history file number for budget fields

!
! Allocate space for arrays private to this module
!
     allocate( mu(pcols,pver,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'mu', &
                      pcols*pver*((endchunk-begchunk)+1) )
     allocate( eu(pcols,pver,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'eu', &
                      pcols*pver*((endchunk-begchunk)+1) )
     allocate( du(pcols,pver,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'du', &
                      pcols*pver*((endchunk-begchunk)+1) )
     allocate( md(pcols,pver,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'md', &
                      pcols*pver*((endchunk-begchunk)+1) )
     allocate( ed(pcols,pver,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'ed', &
                      pcols*pver*((endchunk-begchunk)+1) )
     allocate( dp(pcols,pver,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'dp', &
                      pcols*pver*((endchunk-begchunk)+1) )
     allocate( dsubcld(pcols,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'dsubcld', &
                      pcols*((endchunk-begchunk)+1) )
     allocate( jt(pcols,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'jt', &
                      pcols*((endchunk-begchunk)+1) )
     allocate( maxg(pcols,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'maxg', &
                      pcols*((endchunk-begchunk)+1) )
     allocate( ideep(pcols,begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'ideep', &
                      pcols*((endchunk-begchunk)+1) )
     allocate( lengath(begchunk:endchunk), stat=istat )
      call alloc_err( istat, 'zyx_conv_tend', 'lengath', &
                      ((endchunk-begchunk)+1) )

! 
! Register fields with the output buffer
!


    call addfld ('PRECZ   ','m/s     ',1,    'A','total precipitation from ZM convection',        phys_decomp)
    call addfld ('ZMDT    ','K/s     ',pver, 'A','T tendency - Zhang-McFarlane moist convection', phys_decomp)
    call addfld ('ZMDQ    ','kg/kg/s ',pver, 'A','Q tendency - Zhang-McFarlane moist convection', phys_decomp)
    call addfld ('ZMDICE ','kg/kg/s ',pver, 'A','Cloud ice tendency - Zhang-McFarlane convection',phys_decomp)
    call addfld ('ZMDLIQ ','kg/kg/s ',pver, 'A','Cloud liq tendency - Zhang-McFarlane convection',phys_decomp)
    call addfld ('EVAPTZM ','K/s     ',pver, 'A','T tendency - Evaporation/snow prod from Zhang convection',phys_decomp)
    call addfld ('FZSNTZM ','K/s     ',pver, 'A','T tendency - Rain to snow conversion from Zhang convection',phys_decomp)
    call addfld ('EVSNTZM ','K/s     ',pver, 'A','T tendency - Snow to rain prod from Zhang convection',phys_decomp)

    call addfld ('EVAPQZM ','kg/kg/s ',pver, 'A','Q tendency - Evaporation from Zhang-McFarlane moist convection',phys_decomp)
    
    call addfld ('ZMFLXPRC','kg/m2/s ',pverp, 'A','Flux of precipitation from ZM convection'       ,phys_decomp)
    call addfld ('ZMFLXSNW','kg/m2/s ',pverp, 'A','Flux of snow from ZM convection'                ,phys_decomp)
    call addfld ('ZMNTPRPD','kg/kg/s ',pver , 'A','Net precipitation production from ZM convection',phys_decomp)
    call addfld ('ZMNTSNPD','kg/kg/s ',pver , 'A','Net snow production from ZM convection'         ,phys_decomp)
    call addfld ('ZMEIHEAT','W/kg'    ,pver , 'A','Heating by ice and evaporation in ZM convection',phys_decomp)
    
    call addfld ('CMFMCDZM','kg/m2/s ',pverp,'A','Convection mass flux from ZM deep ',phys_decomp)
    call addfld ('PRECCDZM','m/s     ',1,    'A','Convective precipitation rate from ZM deep',phys_decomp)

    call addfld ('PCONVB','Pa'    ,1 , 'A','convection base pressure',phys_decomp)
    call addfld ('PCONVT','Pa'    ,1 , 'A','convection top  pressure',phys_decomp)

    call addfld ('CAPE',   'J/kg',       1, 'A', 'Convectively available potential energy', phys_decomp)
    call addfld ('FREQZM ','fraction  ',1  ,'A', 'Fractional occurance of ZM convection',phys_decomp) 

    call addfld ('ZMMTT ', 'K/s',     pver, 'A', 'T tendency - ZM convective momentum transport',phys_decomp)
    call addfld ('ZMMTU',  'm/s2',    pver, 'A', 'U tendency - ZM convective momentum transport',  phys_decomp)
    call addfld ('ZMMTV',  'm/s2',    pver, 'A', 'V tendency - ZM convective momentum transport',  phys_decomp)

    call addfld ('ZMMU',   'kg/m2/s', pver, 'A', 'ZM convection updraft mass flux',   phys_decomp)
    call addfld ('ZMMD',   'kg/m2/s', pver, 'A', 'ZM convection downdraft mass flux', phys_decomp)

    call addfld ('ZMUPGU', 'm/s2',    pver, 'A', 'zonal force from ZM updraft pressure gradient term',       phys_decomp)
    call addfld ('ZMUPGD', 'm/s2',    pver, 'A', 'zonal force from ZM downdraft pressure gradient term',     phys_decomp)
    call addfld ('ZMVPGU', 'm/s2',    pver, 'A', 'meridional force from ZM updraft pressure gradient term',  phys_decomp)
    call addfld ('ZMVPGD', 'm/s2',    pver, 'A', 'merdional force from ZM downdraft pressure gradient term', phys_decomp)

    call addfld ('ZMICUU', 'm/s',     pver, 'A', 'ZM in-cloud U updrafts',      phys_decomp)
    call addfld ('ZMICUD', 'm/s',     pver, 'A', 'ZM in-cloud U downdrafts',    phys_decomp)
    call addfld ('ZMICVU', 'm/s',     pver, 'A', 'ZM in-cloud V updrafts',      phys_decomp)
    call addfld ('ZMICVD', 'm/s',     pver, 'A', 'ZM in-cloud V downdrafts',    phys_decomp)

    call phys_getopts( history_budget_out = history_budget, &
                       history_budget_histfile_num_out = history_budget_histfile_num)

    if ( history_budget ) then
       call add_default('EVAPTZM  ', history_budget_histfile_num, ' ')
       call add_default('EVAPQZM  ', history_budget_histfile_num, ' ')
       call add_default('ZMDT     ', history_budget_histfile_num, ' ')
       call add_default('ZMDQ     ', history_budget_histfile_num, ' ')
       call add_default('ZMDLIQ   ', history_budget_histfile_num, ' ')
       call add_default('ZMDICE   ', history_budget_histfile_num, ' ')

       if( cam_physpkg_is('cam4') .or. cam_physpkg_is('cam5') ) then
          call add_default('ZMMTT    ', history_budget_histfile_num, ' ')
       end if

    end if
!
    limcnv = 0   ! null value to check against below

!MZMZ 7/16/18
    !if (pref_edge(1) >= 4.e3_r8) then
    if (pref_edge(1) >= 10.0e3_r8) then 
       limcnv = 1
    else
       do k=1,plev
          !if (pref_edge(k) < 4.e3_r8 .and. pref_edge(k+1) >= 4.e3_r8) then
          if (pref_edge(k) < 10.e3_r8 .and. pref_edge(k+1) >= 10.e3_r8) then
             limcnv = k
             exit
          end if
       end do
       if ( limcnv == 0 ) limcnv = plevp
    end if
    
    if (masterproc) then
       !write(iulog,*)'ZM_CONV_INIT: Deep convection will be capped at intfc ',limcnv
       write(iulog,*)'ZYX_CONV_INIT: Deep convection will be capped at intfc ',limcnv, &
           ' which is ',pref_edge(limcnv),' pascals'
    end if

    no_deep_pbl = phys_deepconv_pbl()
!MZ
    !call zyx_convi(limcnv,no_deep_pbl_in = no_deep_pbl)


!------------------------------------------------------
!do some initialization.
!------------------------------------------------------

   cld_idx         = pbuf_get_index('CLD')
   icwmrdp_idx     = pbuf_get_index('ICWMRDP')
   rprddp_idx      = pbuf_get_index('RPRDDP')
   fracis_idx      = pbuf_get_index('FRACIS')
   nevapr_dpcu_idx = pbuf_get_index('NEVAPR_DPCU')
   prec_dp_idx     = pbuf_get_index('PREC_DP')
   snow_dp_idx     = pbuf_get_index('SNOW_DP')

   cldtop_idx = pbuf_get_index('CLDTOP')
   cldbot_idx = pbuf_get_index('CLDBOT')

   pblh_idx   = pbuf_get_index('pblh')
   tpert_idx  = pbuf_get_index('tpert')

   bfls_t_idx = pbuf_get_index('BFLS_T')
   bfls_q_idx = pbuf_get_index('BFLS_Q')

!MZ call zyx_conv_init( pver )
  call zyx_conv_init( pver, limcnv)
  call phys_getopts(deep_scheme_out = deep_scheme)
  !call phys_getopts(plume_model_out = plume_model)

   massflxbase_p_idx = pbuf_get_index('MBCONVDP_P')

end subroutine zyx_conv_intr_init



!------------------------------------------------------
subroutine zyx_conv_intr_tend( &
        ztodt, landfrac, lhflx, state &
       ,ptend_all, pbuf, dlf, mcon, precrate_out)


    use physics_buffer, only : pbuf_get_field, physics_buffer_desc, pbuf_old_tim_idx
        
    use scamMod,       only: single_column, wfld

! Haiyang Yu
    use nnparameter,   only: nnmodel, negqtendadj, profileadj, nn_flag
!------------------------------------------------------
!Calculate convective tendency
!------------------------------------------------------
   real(r8), intent(in) :: ztodt                        ! 2 delta t (model time increment)
   type(physics_state), intent(in )   :: state          ! Physics state variables
   real(r8), intent(in) :: landfrac(pcols)
   real(r8), intent(in) :: lhflx(pcols)
   type(physics_ptend), intent(out)   :: ptend_all      ! individual parameterization tendencies
   type(physics_buffer_desc), pointer :: pbuf(:)
   real(r8), intent(out) :: dlf(pcols,pver)
   real(r8), intent(out) :: mcon(pcols,pverp)
!xiex
   real(r8), intent(out) :: precrate_out(pcols,pver)

!local
   integer  :: ncol, lchnk
   logical  :: lq(pcnst)
   integer  :: itim 
   integer :: nstep

   type(physics_ptend) :: ptend_loc     ! package tendencies
   type(physics_state) :: state_loc

! physics buffer fields
   real(r8), pointer, dimension(:,:) :: cld
   real(r8), pointer, dimension(:,:) :: ql           ! wg grid slice of cloud liquid water.
   real(r8), pointer, dimension(:,:) :: rprd         ! rain production rate
   real(r8), pointer, dimension(:,:,:) :: fracis     ! fraction of transported species that are insoluble
   real(r8), pointer, dimension(:,:) :: evapcdp      ! Evaporation of deep convective precipitation
   real(r8), pointer, dimension(:)   :: prec         ! total precipitation
   real(r8), pointer, dimension(:)   :: snow         ! snow from ZM convection

   real(r8), pointer :: pblh(:)      ! Planetary boundary layer height
   real(r8), pointer :: tpert(:)     ! Thermal temperature excess

   real(r8), pointer, dimension(:) :: jctop
   real(r8), pointer, dimension(:) :: jcbot
   !real(r8), pointer, dimension(:) :: jt     !MZ
   !integer,  dimension(pcols) :: ideep !(pcols,begchunk:endchunk)

!========

   real(r8), pointer, dimension(:,:) :: flxprec      ! Convective-scale flux of precip at interfaces (kg/m2/s)
   real(r8), pointer, dimension(:,:) :: flxsnow      ! Convective-scale flux of snow   at interfaces (kg/m2/s)
   real(r8), pointer, dimension(:,:) :: dp_cldliq
   real(r8), pointer, dimension(:,:) :: dp_cldice

   real(r8),pointer :: bfls_t(:,:)           ! temp state right after conv
   real(r8),pointer :: bfls_q(:,:)           ! state right after conv

   real(r8), pointer, dimension(:,:) :: massflxbase_p

   real(r8) dilucape(pcols,pver) !thickness in [m]
   real(r8) bfls_dilucape(pcols) !thickness in [m]

   real(r8) zsrf(pcols)    ! model lowest interface Z
   real(r8) dz(pcols,pver) ! model delta Z
   real(r8) z(pcols,pver)  ! Z in the level middel
   real(r8) omega(pcols,pver) ! OMEGA

   real(r8) :: stend(pcols,pver)    ! s tend
   real(r8) :: qtend(pcols,pver)    ! q tend

!for diagnostics
   real(r8) :: qliqtend(pcols,pver)     ! liquid water tendency
   real(r8) :: precrate(pcols,pver)     ! liquid water tendency

   real(r8) :: stendcomp(pcols,pver)    ! s tend but calculated from compensation
   real(r8) :: qtendcomp(pcols,pver)    ! q tend but calculated from compensation

   real(r8) :: outmb(pcols)       ! base mass flux total
   real(r8) :: outtmp2d(pcols)    ! temp 2D output
   real(r8) :: outtmp3d(pcols,pver)  ! temp 3D output
   real(r8) :: outmse(pcols,pver)    ! MSE
   real(r8) :: outmsesat(pcols,pver)   ! MSESAT
   real(r8) :: outmseup(pcols,pver)    ! MSEUP

   real(r8) :: outstend(pcols,pver)    ! total DSE tend
   real(r8) :: outqtend(pcols,pver)    ! total Q tend

   real(r8) :: outstendcond(pcols,pver)    ! condensation DSE tend
   real(r8) :: outqtendcond(pcols,pver)    ! condensation Q tend
   real(r8) :: outstendtranup(pcols,pver)  ! up transport DSE tend
   real(r8) :: outqtendtranup(pcols,pver)  ! up transport Q tend
   real(r8) :: outstendtrandn(pcols,pver)  ! down transport DSE tend
   real(r8) :: outqtendtrandn(pcols,pver)  ! down transport Q tend
   real(r8) :: outstendevap(pcols,pver)    ! evaporation DSE tend
   real(r8) :: outqtendevap(pcols,pver)    ! evaporation Q tend

!MZ
   real(r8) :: pflx(pcols,pverp)  ! scattered precip flux at each level
   real(r8) :: zdu(pcols,pver)    ! detraining mass flux
   real(r8) :: rprd2(pcols,pver)     ! rain production rate
   !real(r8) :: mu(pcols,pver)
   real(r8) :: mu_out(pcols,pver)
   !real(r8) :: eu(pcols,pver)
   !real(r8) :: du(pcols,pver)
   !real(r8) :: md(pcols,pver)
   real(r8) :: md_out(pcols,pver)
   !real(r8) :: ed(pcols,pver)
   real(r8) :: cme(pcols,pver)    ! cmf condensation - evaporation
   !real(r8) :: dsubcld(pcols)   
   real(r8) :: tend_s_snwprd  (pcols,pver) ! Heating rate of snow production
   real(r8) :: tend_s_snwevmlt(pcols,pver) ! Heating rate of evap/melting of snow

  real(r8) :: pcont(pcols), pconb(pcols), freqzm(pcols)


   real(r8) :: cape(pcols)   
   !integer maxg(pcols)                      

   real(r8) :: ftem(pcols,pver)
   real(r8) :: fake_dpdry(pcols,pver) ! used in convtran call

    
   ! used in momentum transport calculation
   real(r8) :: winds(pcols, pver, 2)
   real(r8) :: wind_tends(pcols, pver, 2)
   real(r8) :: pguall(pcols, pver, 2)
   real(r8) :: pgdall(pcols, pver, 2)
   real(r8) :: icwu(pcols,pver, 2)
   real(r8) :: icwd(pcols,pver, 2)
   real(r8) :: seten(pcols, pver)
   logical  :: l_windt(2)
   real(r8) :: tfinal1, tfinal2
   real(r8) :: ntprprd(pcols,pver)    ! net precip production in layer
   real(r8) :: ntsnprd(pcols,pver)    ! net snow production in layer
   integer :: ixcldice, ixcldliq      ! constituent indices for cloud liquid and ice water.



    ! Haiyang Yu
    real(r8) :: nn_prec(pcols)
    real(r8) :: nn_stend(pcols, pver)
    real(r8) :: nn_qtend(pcols, pver)


   integer :: i, j, k,ii
   logical :: flag
   real(r8) :: tmp

!!MZMZ
  call phys_getopts(deep_scheme_out = deep_scheme)
  !call phys_getopts(plume_model_out = plume_model)

!!!!!!!!!!!!!!!!!MZ
!    deep_scheme = 'ZYX'
!    plume_model = 'cam'


!   write(*,*)'ptend_loc%psetcols 1'
!   write(*,*)ptend_loc%psetcols 
!   write(*,*)ptend_all%psetcols 
!   write(*,*)'ptend_loc%psetcols 1end'
   !
!!     goto 1002
     
!MZ  !! the following is needed to pass to call tend!  
   lchnk = state%lchnk
   ncol  = state%ncol
   nstep = get_nstep()

!MZ
   ftem = 0._r8
   mu_out(:,:) = 0._r8
   md_out(:,:) = 0._r8
   wind_tends(:ncol,:pver,:) = 0.0_r8

   call physics_state_copy(state,state_loc)             ! copy state to local state_loc.

   lq(:) = .false.
   lq(1) = .true.

   !call physics_ptend_init(ptend_all, state%psetcols, 'convect_deep')
   call physics_ptend_init(ptend_loc, state%psetcols, 'convect_deep', ls=.true., lq=lq)

   itim = pbuf_old_tim_idx()
   call pbuf_get_field(pbuf, cld_idx,         cld,    start=(/1,1,itim/), kount=(/pcols,pver,1/) )
   call pbuf_get_field(pbuf, icwmrdp_idx,     ql )
!MZ

   call pbuf_get_field(pbuf, rprddp_idx,      rprd )
   call pbuf_get_field(pbuf, fracis_idx,      fracis, start=(/1,1,1/),    kount=(/pcols, pver, pcnst/) )
   call pbuf_get_field(pbuf, nevapr_dpcu_idx, evapcdp )
   call pbuf_get_field(pbuf, prec_dp_idx,     prec )
   call pbuf_get_field(pbuf, snow_dp_idx,     snow )

   call pbuf_get_field(pbuf, cldtop_idx, jctop )
   call pbuf_get_field(pbuf, cldbot_idx, jcbot )

!MZ
 if(plume_model == 'scp')then
!   cld = 0._r8
   ql  = 0._r8
   rprd = 0._r8
   fracis = 0._r8
   evapcdp = 0._r8
   prec = 0._r8
   snow = 0._r8
  endif
  
   call pbuf_get_field(pbuf, pblh_idx,  pblh)
   call pbuf_get_field(pbuf, tpert_idx, tpert)

!used since CESM V1.2.2
   call pbuf_get_field(pbuf, dp_flxprc_idx, flxprec    )
   call pbuf_get_field(pbuf, dp_flxsnw_idx, flxsnow    )
   call pbuf_get_field(pbuf, dp_cldliq_idx, dp_cldliq  )
   call pbuf_get_field(pbuf, dp_cldice_idx, dp_cldice  )

!    flxprec(:ncol,:) = 0._r8
!    flxsnow(:ncol,:) = 0._r8
!    dp_cldliq(:ncol,:) = 0._r8
!    dp_cldice(:ncol,:) = 0._r8

   call pbuf_get_field(pbuf, massflxbase_p_idx, massflxbase_p )

!xiex
   call pbuf_get_field(pbuf, bfls_t_idx, bfls_t)
   call pbuf_get_field(pbuf, bfls_q_idx, bfls_q)

   if (single_column) then
       omega(1,:) = wfld
   else
       omega(:ncol,:) = state%omega
   end if

!for layer depth
   do k=1,pver
       dz(:ncol,k) = state%zi(:ncol,k) - state%zi(:ncol,k+1)
       z(:ncol,k)  = state%zm(:ncol,k) + state%phis(:ncol)/gravit
   end do
   zsrf(:ncol) = state%phis(:ncol)/gravit
    
    ! ---------------------------------------------------------------
    ! Haiyang Yu: nnmodel
    do i = 1,ncol
        if ( landfrac(i)<0.5 ) then
            call nnmodel(pver, landfrac(i), state%pmid(i,:), state%u(i,:), state%v(i,:), &
                state%t(i,:), state%q(i,:,1), z(i,:), omega(i,:), state%ps(i), &
                nn_stend(i,:), nn_qtend(i,:), nn_prec(i) )
        end if
    end do
    ! ---------------------------------------------------------------


   call t_startf('zyx_conv_tend')


i=1
if(i<0)then
    k = 25
              !write(*,*) 'XXXXXX state%lchnk pcols,pver,ncol,nlev ', state%lchnk,pcols,pver,ncol
              !write(*,*) 'state%lon(i)*180._r8/pi'
              !write(*,"(5F15.2)") state%lon(:)*180._r8/3.1416_r8
              !write(*,*) 'state%lat(i)*180._r8/pi'
              !write(*,"(5F15.2)") state%lat(:)*180._r8/3.1416_r8
              !write(*,*) 'state%q(:,k,1) -----------'
              !write(*, "(5E15.7)") state%q(:,k,1)
              !write(*,*) 'state%q(:,k,2) -----------'
              !write(*, "(5E15.7)") state%q(:,k,2)
              !write(*,*) 'mcon -----------'
              !write(*, "(5E15.7)") mcon(:,k)
              !write(*,*) 'rprd -----------'
              !write(*, "(5E15.7)") rprd(:,k)

    !if(masterproc)then
              write(*,"(A50/,A30/,4I10/,2(A5/,3(5F15.2/)),4(A10/,3(5E15.7/)) )") &
              'XXXX before calling zyx_conv ',   &
              'state%lchnk, pcols,pver,ncol',state%lchnk, pcols,pver,ncol, &
              'lon',state%lon(1:15)*180._r8/3.1416_r8,&
              'lat',state%lat(1:15)*180._r8/3.1416_r8, &
              'q(1)',state%q(1:15,k,1),        &
              'q(2)',state%q(1:15,k,2),        &
              'mcon',mcon(1:15,k),             &
              'rprd',rprd(1:15,k)
    !  endif     
endif



!write(*,*)
!if (masterproc) then
!          write(iulog,*)'ZYX_CONV_INTR: starting call zyx_conv_tend'
!end if

   !MZ call zyx_conv_tend( &
   call zyx_conv_tend(lchnk, &
!input
       ncol, &
       2, ztodt, qmin(1), &
       state%ulat(:ncol), landfrac(:ncol), lhflx(:ncol), &
       state%ps(:ncol), state%pmid(:ncol,:), state%pdel(:ncol,:), &
       zsrf(:ncol), z(:ncol,:), dz(:ncol,:), &
       state%t(:ncol,:), state%q(:ncol,:,1), &
       bfls_t(:ncol,:), bfls_q(:ncol,:), &
       omega(:ncol,:), pblh(:ncol), tpert(:ncol), &
       nn_prec(:ncol), nn_stend(:ncol,:), nn_qtend(:ncol,:),  &
!in/output
       massflxbase_p(:ncol,:), &
!output
       jctop(:ncol), jcbot(:ncol), &
       stend(:ncol,:), qtend(:ncol,:), &
       qliqtend(:ncol,:), &
       prec(:ncol), ql(:ncol,:), precrate(:ncol,:), &
       mcon(:ncol,:), &
       stendcomp(:ncol,:), qtendcomp(:ncol,:), &
!!       ptend_loc%s(:ncol,:),ptend_loc%q(:ncol,:,1), &
       dilucape(:ncol,:), bfls_dilucape(:ncol), &
!diagnostics
       outtmp2d(:ncol), outtmp3d(:ncol,:), &
       outmb(:ncol), outmse(:ncol,:), outmsesat(:ncol,:), outmseup(:ncol,:), &
       outstend(:ncol,:), outqtend(:ncol,:), &
       outstendcond(:ncol,:), outqtendcond(:ncol,:), &
       outstendtranup(:ncol,:), outqtendtranup(:ncol,:), &
       outstendtrandn(:ncol,:), outqtendtrandn(:ncol,:), &
       outstendevap(:ncol,:), outqtendevap(:ncol,:)  , &
!MZ added CAM output
         state%phis(:ncol), cme(:ncol,:), cape(:ncol), ideep(:ncol,lchnk),&
         dlf(:ncol,:)     ,pflx(:ncol,:)    ,zdu(:ncol,:)     ,rprd(:ncol,:)    , &
         mu(:ncol,:,lchnk),md(:ncol,:,lchnk),du(:ncol,:,lchnk),eu(:ncol,:,lchnk),ed(:ncol,:,lchnk) , &
         dsubcld(:ncol,lchnk),  jt(:ncol,lchnk),maxg(:ncol,lchnk),&
         lengath(lchnk))

!!     write(*,*)'lengath(lchnk),lengath,lchnk,begchunk,endchunk'
!!     write(*,*)lengath(lchnk),lengath,lchnk,begchunk,endchunk

! yhy test:
    call foutput_1d('zyx intro: zyx_conv_tend: after: ulat: ', state%ulat(:ncol), ncol)
    call foutput_1d('zyx intro: zyx_conv_tend: after: landfrac: ', landfrac(:ncol), ncol)
    call foutput_1d('zyx intro: zyx_conv_tend: after: lhflx: ', lhflx(:ncol), ncol)
    call foutput_1d('zyx intro: zyx_conv_tend: after: ps: ', state%ps(:ncol), ncol)
    call foutput_2d('zyx intro: zyx_conv_tend: after: t: ', state%t(:ncol,:), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: q1: ', state%q(:ncol,:,1), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: q2: ', state%q(:ncol,:,2), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: q3: ', state%q(:ncol,:,3), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: q4: ', state%q(:ncol,:,4), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: q5: ', state%q(:ncol,:,5), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: omega: ', omega(:ncol,:), ncol, pver)
    call foutput_1d('zyx intro: zyx_conv_tend: after: pblh: ', pblh(:ncol), ncol)
    call foutput_1d('zyx intro: zyx_conv_tend: after: massflxbase_p: ', massflxbase_p(:ncol, 1), ncol)
    call foutput_2d('zyx intro: zyx_conv_tend: after: stend: ', stend(:ncol,:), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: qtend: ', qtend(:ncol,:), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: qliqtend: ', qliqtend(:ncol,:), ncol, pver)
    call foutput_1d('zyx intro: zyx_conv_tend: after: prec: ', prec(:ncol), ncol)
    call foutput_2d('zyx intro: zyx_conv_tend: after: mcon: ', mcon(:ncol, :), ncol, pverp)
    call foutput_2d('zyx intro: zyx_conv_tend: after: dilucape: ', dilucape(:ncol, :), ncol, pver)
    
    call foutput_1d('zyx intro: zyx_conv_tend: after: phis: ', state%phis(:ncol), ncol)
    call foutput_2d('zyx intro: zyx_conv_tend: after: cme: ',  cme(:ncol, :), ncol, pver)
    call foutput_1d('zyx intro: zyx_conv_tend: after: cape: ', cape(:ncol), ncol)
    call foutput_2d('zyx intro: zyx_conv_tend: after: dlf: ',  dlf(:ncol, :), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: pflx: ', pflx(:ncol, :), ncol, pverp)
    call foutput_2d('zyx intro: zyx_conv_tend: after: zdu: ',  zdu(:ncol, :), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: rprd: ', rprd(:ncol, :), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: mu: ',   mu(:ncol, :, lchnk), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: md: ',   md(:ncol, :, lchnk), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: du: ',   du(:ncol, :, lchnk), ncol, pver)
    call foutput_2d('zyx intro: zyx_conv_tend: after: eu: ',   eu(:ncol, :, lchnk), ncol, pver)
    call foutput_1d('zyx intro: zyx_conv_tend: after: dsubcld: ',   dsubcld(:ncol, lchnk), ncol)



!MZ !!
     do i = 1,lengath(lchnk)
       dp(i,:,lchnk) = 0.01_r8*state%pdel(ideep(i,lchnk),:) !! in mb
     end do

    ! ---------------------------------------------------------------
    ! Haiyang Yu: nn_prec adjustment
    if (nn_flag > 0) then
        do i = 1,ncol
            !write(*, *) "yhy:before profile adjustment"
            !do k = 1, pver
            !    write(*, "(5E10.2)") stend(i,k), qtend(i,k), precrate(i,k), qliqtend(i,k), mcon(i,k)
            !end do
            !write(*, *) "yhy:prec=", prec(i)
            !write(*, *) "yhy:jctop,jcbot=", jctop(i), jcbot(i)
            
            if ( landfrac(i)<0.5 ) then
                !write(*,*) 'yhy:nn:',state%lat(i)*180/3.14159, state%lon(i)*180/3.14159, prec(i)*86400*1000.0, nn_prec(i)*86400*1000.0
                call profileadj(pver, nn_prec(i), prec(i), stend(i,:), qtend(i,:), precrate(i,:), qliqtend(i,:), mcon(i,:) )
                call negqtendadj(pver, state%q(i,:,1), qtend(i,:), stend(i,:), &
                    precrate(i,:), qliqtend(i,:), mcon(i,:), ztodt, qmin(1)*1.001)
            end if
            
            !!write(*, *) "yhy:after profile adjustment"
            !do k = 1, pver
            !    write(*, "(5E10.2)") stend(i,k), qtend(i,k), precrate(i,k), qliqtend(i,k), mcon(i,k)
            !end do
            !write(*, *) "yhy:prec=", prec(i)
            !write(*, *) "yhy:jctop,jcbot=", jctop(i), jcbot(i)
    !            write(*, *) "yhy:after adj:",nn_prec(i), nn_stend(i,:), nn_qtend(i,:), &
    !                ", ecp:", prec(i), stend(i,:), qtend(i,:), &
    !                ", precrate qliq mcon:", precrate(i,:), qliqtend(i,:), mcon(i,:)
        end do
    end if
    ! ---------------------------------------------------------------

!MZ
   stendcomp = stend
   qtendcomp = qtend
   ptend_loc%s(:ncol,:)   = stend(:ncol,:)
   ptend_loc%q(:ncol,:,1) = qtend(:ncol,:)

i=1
if(i<0)then
    k = 25
              !write(*,*) 'state%lchnk pcols,pver,ncol,nlev ', state%lchnk,pcols,pver,ncol
            !do i=0,pcols 
             !if(abs( state%lat(i)*180._r8/3.1416_r8) < 3.0_r8)then
              !write(*,*) 'state%lon(i)*180._r8/pi'
              !write(*,"(5F15.2)") state%lon(:)*180._r8/3.1416_r8
              !write(*,*) 'state%lat(i)*180._r8/pi'
              !write(*,"(5F15.2)") state%lat(:)*180._r8/3.1416_r8
              !write(*,*) 'state%q(:,k,1) -----------'
              !write(*, "(5E15.7)") state%q(:,k,1)
              !write(*,*) 'state%q(:,k,2) -----------'
              !write(*, "(5E15.7)") state%q(:,k,2)
              !write(*,*) 'ptend%q(:,k,1) -----------'
              !write(*, "(5E15.7)") ptend_loc%q(:,k,1)
              !write(*,*) 'ptend%q(:,k,2) -----------'
              !write(*, "(5E15.7)") ptend_loc%q(:,k,2)
              !write(*,*) 'mcon -----------'
              !write(*, "(5E15.7)") mcon(:,k)
              !write(*,*) 'rprd -----------'
              !write(*, "(5E15.7)") rprd(:,k)

    !if(masterproc)then
              write(*,"(A50/,A30/,4I10/,2(A5/,3(5F15.2/)),6(A10/,3(5E15.7/)) )") &
              'YYYY After calling zyx_conv ',   &
              'state%lchnk, pcols,pver,ncol',state%lchnk, pcols,pver,ncol, &
              'lon',state%lon(1:15)*180._r8/3.1416_r8,&
              'lat',state%lat(1:15)*180._r8/3.1416_r8, &
              'q(1)',state%q(1:15,k,1),        &
              'q(2)',state%q(1:15,k,2),        &
              'mcon',mcon(1:15,k),             &
              'rprd',rprd(1:15,k), &
              'ptendq(1)',ptend_loc%q(1:15,k,1),        &
              'ptendq(2)',ptend_loc%q(1:15,k,2)

     ! endif     
endif

!   call outfld('TMP2D', outtmp2d, pcols, state%lchnk )
!   call outfld('TMP3D', outtmp3d, pcols, state%lchnk )

   call outfld('MBCONVDP_P', massflxbase_p, pcols, state%lchnk )
   call outfld('MBCONVDP', outmb, pcols, state%lchnk )

   call outfld('NNSTEND', nn_stend, pcols, lchnk)
   call outfld('NNQTEND', nn_qtend, pcols, lchnk)
   call outfld('NNPREC', nn_prec, pcols, lchnk)


   call outfld('MSE', outmse, pcols, state%lchnk )
   call outfld('MSESAT', outmsesat, pcols, state%lchnk )
   call outfld('MSEUP', outmseup, pcols, state%lchnk )
   call outfld('CONVZ', z, pcols, state%lchnk )

   call outfld('STENDCONVDP', stend, pcols, lchnk)
   call outfld('QTENDCONVDP', qtend, pcols, lchnk)

!   call outfld('STENDCONVDPCOND', outstendcond, pcols, lchnk)
!   call outfld('QTENDCONVDPCOND', outqtendcond, pcols, lchnk)
!   call outfld('STENDCONVDPTRANUP', outstendtranup, pcols, lchnk)
!   call outfld('QTENDCONVDPTRANUP', outqtendtranup, pcols, lchnk)
!   call outfld('STENDCONVDPTRANDN', outstendtrandn, pcols, lchnk)
!   call outfld('QTENDCONVDPTRANDN', outqtendtrandn, pcols, lchnk)
!   call outfld('STENDCONVDPEVAP', outstendevap, pcols, lchnk)
!   call outfld('QTENDCONVDPEVAP', outqtendevap, pcols, lchnk)

!   call outfld('STENDCONVDPCOMP', stendcomp, pcols, lchnk)
!   call outfld('QTENDCONVDPCOMP', qtendcomp, pcols, lchnk)

   call outfld('DILUCAPE', dilucape, pcols, lchnk)            ! RBN - CAPE output
!   call outfld('BFLSDILUCAPE', bfls_dilucape, pcols, lchnk)   ! RBN - CAPE output

   do i = 1, ncol
       do k = 1, pver
           if ( isnan( ptend_loc%s(i,k) ) .or. &
                isnan( ptend_loc%q(i,k,1) ) ) then
               write(iulog,*) 'problem in zyx_conv', i, k
               write(iulog,*) 'ptend_loc%s(i,k)'
               write(iulog,'(5f15.10)') ptend_loc%s(i,:)
               write(iulog,*) 'ptend_loc%q(i,k,ixcldice)'
               write(iulog,'(5f15.10)') ptend_loc%q(i,:,1)
               write(iulog,*) 'stend(i,k)'
               write(iulog,'(5f15.10)') stend(i,:)
               write(iulog,*) 'qtend(i,k)'
               write(iulog,'(5f15.10)') qtend(i,:)

               exit
           end if
       end do
   end do
   ! Haiyang

   dlf(:ncol,1:pver)  = qliqtend(:ncol,1:pver)
   rprd(:ncol,1:pver) = precrate(:ncol,1:pver)
   precrate_out = precrate
!xiex
!!   precrate_out = rprd

if(i<0)then
   write(*,*)'-- after call zym_conv_tend to prepare for dlf in SCP'
   write(*,*)'dlf',dlf
   write(*,*)'qliqtend',qliqtend
   write(*,*)'precrate',precrate
   write(*,*)'prec',prec
   write(*,*)'ptend_loc%s(:ncol,:)',ptend_loc%s
   write(*,*)'ptend_loc%q(:ncol,:,1)',ptend_loc%q
   write(*,*)'stend(:ncol,:)',stend
   write(*,*)'qtend(:ncol,:)',qtend
   write(*,*)'stendcomp(:ncol,:)',stendcomp
   write(*,*)'qtendcomp(:ncol,:)',qtendcomp
   write(*,*)'-------------'
endif

   do i = 1, ncol
       do k = 1, pver
           tmp = state_loc%q(i,k,1) + qtend(i,k)*ztodt
           if ( tmp<qmin(1) ) then
               write(iulog,'(a10,f30.25)') 'qmin', qmin(1)
               write(iulog,'(a10,f30.25)') 'tmp', tmp
               write(iulog,'(a10,f30.25)') 'dtime', ztodt
               write(iulog,'(a10,f30.25)') 'minqcheckf', outtmp2d(i)
               write(iulog,*) tmp<qmin(1), tmp-qmin(1)
               write(iulog,*) 'problem in neg Q', i, k
               write(iulog,*) 'base', jcbot(i), 'top', jctop(i)
               do j = 1, pver
                   write(iulog,'(i3,f10.5,3f30.25,l3)') j, &
                       state_loc%pmid(i,j)/100., state_loc%q(i,j,1), ptend_loc%q(i,j,1)*ztodt, &
                       state_loc%q(i,j,1)>qmin(1)
                   write(iulog,'(i3,f10.5,3f30.25,l3)') j, state_loc%pmid(i,j)/100., &
                       state_loc%q(i,j,1), qtend(i,j)*ztodt, outqtend(i,j)*ztodt, &
                       state_loc%q(i,j,1) > qmin(1)
               end do
               exit
           end if
       end do
   end do


    !yhy
    !write(*, *) "yhy:after phy update"
    !do i = 1, ncol
    !    do k = 1, pver
    !        write(*,"(E10.4)") state_loc%q(i,k,1)
    !    end do
    !end do

   !call outfld('TMP3D', outmu, pcols, lchnk)        ! RBN - CAPE output
   !call outfld('CONVDPSU', outsu, pcols, lchnk)        ! RBN - CAPE output
   !call outfld('CONVDPQU', outqu, pcols, lchnk)        ! RBN - CAPE output


   !!call outfld('STENDCONVDP', ptend_loc%s, pcols, lchnk)        ! RBN - CAPE output
   !!call outfld('QTENDCONVDP', ptend_loc%q(:,:,1), pcols, lchnk)        ! RBN - CAPE output

   !call outfld('STENDCONVDPCOND', outcondtends, pcols, lchnk)        ! RBN - CAPE output
   !call outfld('QTENDCONVDPCOND', outcondtendq, pcols, lchnk)        ! RBN - CAPE output
   !!call outfld('STENDCONVDPCOND', outscondtend, pcols, lchnk)        ! RBN - CAPE output
   !!call outfld('QTENDCONVDPCOND', outqcondtend, pcols, lchnk)        ! RBN - CAPE output
   !!call outfld('STENDCONVDPTRANUP', outtranuptends, pcols, lchnk)      ! RBN - CAPE output
   !!call outfld('QTENDCONVDPTRANUP', outtranuptendq, pcols, lchnk)      ! RBN - CAPE output
   !!call outfld('STENDCONVDPTRANDN', outtrandntends, pcols, lchnk)      ! RBN - CAPE output
   !!call outfld('QTENDCONVDPTRANDN', outtrandntendq, pcols, lchnk)      ! RBN - CAPE output
!xiex#

   call outfld('CAPE', cape, pcols, lchnk)        ! RBN - CAPE output
!
! Output fractional occurance of ZM convection
!
   freqzm(:) = 0._r8
   do i = 1,lengath(lchnk)
      freqzm(ideep(i,lchnk)) = 1.0_r8
   end do
   call outfld('FREQZM  ',freqzm          ,pcols   ,lchnk   )
!
! Convert mass flux from reported mb/s to kg/m^2/s
!
!!   write(*,*)'my sum1.2.1'
     mcon(:ncol,:pver) = mcon(:ncol,:pver) * 100._r8/gravit

   ! Store upward and downward mass fluxes in un-gathered arrays
   ! + convert from mb/s to kg/m^2/s
   do i=1,lengath(lchnk) 
      do k=1,pver
         ii = ideep(i,lchnk)
         mu_out(ii,k) = mu(i,k,lchnk) * 100._r8/gravit
         md_out(ii,k) = md(i,k,lchnk) * 100._r8/gravit
      end do
   end do

   call outfld('ZMMU', mu_out(1,1), pcols, lchnk)
   call outfld('ZMMD', md_out(1,1), pcols, lchnk)

   ftem(:ncol,:pver) = ptend_loc%s(:ncol,:pver)/cpair
   call outfld('ZMDT    ',ftem           ,pcols   ,lchnk   )
   call outfld('ZMDQ    ',ptend_loc%q(1,1,1) ,pcols   ,lchnk   )

!write(*,*)
!if (masterproc) then
!          write(iulog,*)'ZYX_CONV_INTR: finished call zyx_conv_tend'
!end if


   
   call t_stopf ('zyx_conv_tend')

   !!goto 1002

!!   write(*,*)'my sum1.2.2'
   !do i = 1,pcols
   !do i = 1,nco
!MZ   
   pcont(:ncol) = state%ps(:ncol)
   pconb(:ncol) = state%ps(:ncol)
   do i = 1,lengath(lchnk)
       if (maxg(i,lchnk).gt.jt(i,lchnk)) then
          pcont(ideep(i,lchnk)) = state%pmid(ideep(i,lchnk),jt(i,lchnk))  ! gathered array (or jctop ungathered)
          pconb(ideep(i,lchnk)) = state%pmid(ideep(i,lchnk),maxg(i,lchnk))! gathered array
       endif
       !     write(iulog,*) ' pcont, pconb ', pcont(i), pconb(i), cnt(i), cnb(i)
    end do
    call outfld('PCONVT  ',pcont          ,pcols   ,lchnk   )
    call outfld('PCONVB  ',pconb          ,pcols   ,lchnk   )

!MZ
  call physics_ptend_init(ptend_all, state%psetcols, 'convect_deep')

  call physics_ptend_sum(ptend_loc, ptend_all, ncol)

  call physics_update(state_loc, ptend_loc, ztodt)


!MZ ------------ mimic conv_intr_jp in SCP  
!!   if(plume_model == 'cam')then
   if(plume_model == 'scp')then
     goto 1002
     !================================
   endif 
!MZ---------------------


  ! initialize ptend for next process
  lq(:) = .FALSE.
  lq(1) = .TRUE.
  call physics_ptend_init(ptend_loc, state_loc%psetcols, 'zyx_conv_evap', ls=.true., lq=lq)

   call t_startf ('zyx_conv_evap')
!
! Determine the phase of the precipitation produced and add latent heat of fusion
! Evaporate some of the precip directly into the environment (Sundqvist)
! Allow this to use the updated state_loc and the fresh ptend_loc type
! heating and specific humidity tendencies produced
!

!MZ
    call pbuf_get_field(pbuf, dp_flxprc_idx, flxprec    )
    call pbuf_get_field(pbuf, dp_flxsnw_idx, flxsnow    )
    call pbuf_get_field(pbuf, dp_cldliq_idx, dp_cldliq  )
    call pbuf_get_field(pbuf, dp_cldice_idx, dp_cldice  )
    dp_cldliq(:ncol,:) = 0._r8
    dp_cldice(:ncol,:) = 0._r8

  !  write(*,*) 'flxprec 1'
  !flxprec(:,:) = pflx(:,:)
  !flxsnow(:,:) = 0._r8
  !tend_s_snwprd(:,:) =0._r8
  !tend_s_snwevmlt(:,:) =0._r8
  ! write(*,*)'my sum2'



if(i<0)then    
    write(*,*) '......... before zyx_conv_evap'
write(*,*)'state_loc%ncol,state_loc%lchnk',state_loc%ncol,state_loc%lchnk,pver,pverp
write(*,*)'state_loc%t',state_loc%t
write(*,*)'state_loc%pmid',state_loc%pmid
write(*,*)'state_loc%pdel',state_loc%pdel
write(*,*)'state_loc%q(:pcols,:pver,1)',state_loc%q(:pcols,:pver,1)
write(*,*)'rprd',rprd
write(*,*)'cld',cld
write(*,*)'ztodt',ztodt
write(*,*)'prec',prec
write(*,*)'snow',snow
write(*,*)'ntprprd',ntprprd
write(*,*)'ntsnprd',ntsnprd
write(*,*)'flxprec',flxprec
write(*,*)'flxsnow',flxsnow
write(*,*)'ptend_loc%s',ptend_loc%s
write(*,*)'tend_s_snwprd',tend_s_snwprd
write(*,*)'tend_s_snwevmlt',tend_s_snwevmlt
write(*,*)'ptend_loc%q(:pcols,:pver,1)',ptend_loc%q(:pcols,:pver,1)
endif
!MZ 
    !call zyx_conv_evap(state_loc%ncol,state_loc%lchnk,&
         !state_loc%t,state_loc%pmid,state_loc%pdel,state_loc%q(:pcols,:pver,1), &
         !ptend_loc%s, tend_s_snwprd, tend_s_snwevmlt, ptend_loc%q(:pcols,:pver,1), &
         !rprd, cld, ztodt, &
         !prec, snow, ntprprd, ntsnprd , flxprec, flxsnow)

    call zyx_conv_evap(state_loc%lchnk,state_loc%ncol, pver,pverp,&
         state_loc%t(:ncol,:),state_loc%pmid(:ncol,:),state_loc%pdel(:ncol,:),state_loc%q(:ncol,:pver,1), &
         ptend_loc%s(:ncol,:), tend_s_snwprd(:ncol,:), tend_s_snwevmlt(:ncol,:), ptend_loc%q(:ncol,:pver,1), &
         rprd(:ncol,:), cld(:ncol,:), ztodt, &
         prec(:ncol), snow(:ncol), ntprprd(:ncol,:), ntsnprd(:ncol,:) , flxprec(:ncol,:), flxsnow(:ncol,:))

    evapcdp(:ncol,:pver) = ptend_loc%q(:ncol,:pver,1)
if(i<0)then
   write(*,*)'my sum3'
    write(*,*) '......... after zyx_conv_evap'
write(*,*)'evapcdp',evapcdp
write(*,*)'prec',prec
write(*,*)'snow',snow
write(*,*)'ntprprd',ntprprd
write(*,*)'ntsnprd',ntsnprd
write(*,*)'flxprec',flxprec
write(*,*)'flxsnow',flxsnow
write(*,*)'ptend_loc%s',ptend_loc%s
write(*,*)'tend_s_snwprd',tend_s_snwprd
write(*,*)'tend_s_snwevmlt',tend_s_snwevmlt
write(*,*)'ptend_loc%q(:pcols,:pver,1)',ptend_loc%q(:pcols,:pver,1)
endif
!MZ 
!
! Write out variables from zyx_conv_evap
!
   ftem(:ncol,:pver) = ptend_loc%s(:ncol,:pver)/cpair
   call outfld('EVAPTZM ',ftem           ,pcols   ,lchnk   )
   ftem(:ncol,:pver) = tend_s_snwprd  (:ncol,:pver)/cpair
   call outfld('FZSNTZM ',ftem           ,pcols   ,lchnk   )
   ftem(:ncol,:pver) = tend_s_snwevmlt(:ncol,:pver)/cpair
   call outfld('EVSNTZM ',ftem           ,pcols   ,lchnk   )
   call outfld('EVAPQZM ',ptend_loc%q(1,1,1) ,pcols   ,lchnk   )
   call outfld('ZMFLXPRC', flxprec, pcols, lchnk)
   call outfld('ZMFLXSNW', flxsnow, pcols, lchnk)
   call outfld('ZMNTPRPD', ntprprd, pcols, lchnk)
   call outfld('ZMNTSNPD', ntsnprd, pcols, lchnk)
   call outfld('ZMEIHEAT', ptend_loc%s, pcols, lchnk)
   call outfld('CMFMCDZM   ',mcon ,  pcols   ,lchnk   )
   call outfld('PRECCDZM   ',prec,  pcols   ,lchnk   )


   call t_stopf ('zyx_conv_evap')

   call outfld('PRECZ   ', prec   , pcols, lchnk)

  ! add tendency from this process to tend from other processes here
  call physics_ptend_sum(ptend_loc,ptend_all, ncol)

  ! update physics state type state_loc with ptend_loc 
  call physics_update(state_loc, ptend_loc, ztodt)


  ! Momentum Transport (non-cam3 physics)


  if ( .not. cam_physpkg_is('cam3')) then

     call physics_ptend_init(ptend_loc, state_loc%psetcols, 'momtran', ls=.true., lu=.true., lv=.true.)

     winds(:ncol,:pver,1) = state_loc%u(:ncol,:pver)
     winds(:ncol,:pver,2) = state_loc%v(:ncol,:pver)

if(i<0)then
write(*,*)'....before momtran'
write(*,*)'winds',winds
write(*,*)'dp',dp
write(*,*)'dsubcld',dsubcld
   write(*,*)'my sum4'
endif   
     l_windt(1) = .true.
     l_windt(2) = .true.

     call t_startf ('momtran')
!MZ
     !call momtran (lchnk, ncol,                                        &
                   !l_windt,winds, 2,  mu(1,1,lchnk), md(1,1,lchnk),   &
                   !du(1,1,lchnk), eu(1,1,lchnk), ed(1,1,lchnk), dp(1,1,lchnk), dsubcld(1,lchnk),  &
                   !jt(1,lchnk),maxg(1,lchnk), ideep(1,lchnk), 1, lengath(lchnk),  &
                   !nstep,  wind_tends, pguall, pgdall, icwu, icwd, ztodt, seten )  
     call momtran (lchnk, ncol,  pver, pverp,                                         &
                   l_windt,winds(:ncol,:,:), 2,  mu(:ncol,:,lchnk), md(:ncol,:,lchnk),   &
                   du(:ncol,:,lchnk), eu(:ncol,:,lchnk), ed(:ncol,:,lchnk), &
                   dp(:ncol,:,lchnk), dsubcld(:ncol,lchnk),  &
                   jt(:ncol,lchnk),maxg(:ncol,lchnk), ideep(:ncol,lchnk), 1, lengath(lchnk),  &
                   nstep,  wind_tends(:ncol,:,:), pguall(:ncol,:,:), pgdall(:ncol,:,:), &
                   icwu(:ncol,:,:), icwd(:ncol,:,:), ztodt, seten(:ncol,:) )  
     call t_stopf ('momtran')

if(i<0)then
write(*,*)'----- after momtran'
write(*,*)'wind_tends',wind_tends
write(*,*)'dp',dp
write(*,*)'winds',winds
write(*,*)'mu',mu
write(*,*)'pguall',pguall
write(*,*)'pgdall',pgdall
write(*,*)'seten',seten
   write(*,*)'my sum5'
endif


!xiex
     !ptend_loc%u(:ncol,:pver) = 0._r8
     !ptend_loc%v(:ncol,:pver) = 0._r8
     !ptend_loc%s(:ncol,:pver) = 0._r8

     ptend_loc%u(:ncol,:pver) = wind_tends(:ncol,:pver,1)
     ptend_loc%v(:ncol,:pver) = wind_tends(:ncol,:pver,2)
     ptend_loc%s(:ncol,:pver) = seten(:ncol,:pver)  

!!   goto 1002

     call physics_ptend_sum(ptend_loc,ptend_all, ncol)


     ! update physics state type state_loc with ptend_loc 
     call physics_update(state_loc, ptend_loc, ztodt)

!!   goto 1002

     ftem(:ncol,:pver) = seten(:ncol,:pver)/cpair
     call outfld('ZMMTT', ftem             , pcols, lchnk)
     call outfld('ZMMTU', wind_tends(1,1,1), pcols, lchnk)
     call outfld('ZMMTV', wind_tends(1,1,2), pcols, lchnk)
   
     ! Output apparent force from  pressure gradient
     call outfld('ZMUPGU', pguall(1,1,1), pcols, lchnk)
     call outfld('ZMUPGD', pgdall(1,1,1), pcols, lchnk)
     call outfld('ZMVPGU', pguall(1,1,2), pcols, lchnk)
     call outfld('ZMVPGD', pgdall(1,1,2), pcols, lchnk)

     ! Output in-cloud winds
     call outfld('ZMICUU', icwu(1,1,1), pcols, lchnk)
     call outfld('ZMICUD', icwd(1,1,1), pcols, lchnk)
     call outfld('ZMICVU', icwu(1,1,2), pcols, lchnk)
     call outfld('ZMICVD', icwd(1,1,2), pcols, lchnk)

   end if

   !!goto 1002

   ! Transport cloud water and ice only
   call cnst_get_ind('CLDLIQ', ixcldliq)
   call cnst_get_ind('CLDICE', ixcldice)

   lq(:)  = .FALSE.
   lq(2:) = cnst_is_convtran1(2:)
   call physics_ptend_init(ptend_loc, state_loc%psetcols, 'convtran1', lq=lq)


   ! dpdry is not used in this call to convtran since the cloud liquid and ice mixing
   ! ratios are moist
   fake_dpdry(:,:) = 0._r8

   call t_startf ('convtran1')

!MZ
if(i<0)then
write(*,*)'.........before convtran'
write(*,*)'lchnk, ncol, pver, pverp',lchnk, ncol, pver, pverp
write(*,*)'state_loc%q',state_loc%q
write(*,*)'mu',mu
write(*,*)'md',md
write(*,*)'du',du
write(*,*)'eu',eu
write(*,*)'ed',ed
write(*,*)'dp',dp
write(*,*)'dsubcld',dsubcld
write(*,*)'jt',jt
write(*,*)'maxg',maxg
write(*,*)'ideep',ideep
write(*,*)'lengath',lengath
write(*,*)'fracis',fracis
write(*,*)'ptend_loc%q',ptend_loc%q
write(*,*)'fake_dpdry',fake_dpdry
endif

!MZ
   !call convtran (lchnk,                                        &
                  !ptend_loc%lq,state_loc%q, pcnst,  mu(:,:,lchnk), md(:,:,lchnk),   &
                  !du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk),  &
                  !jt(:,lchnk),maxg(:,lchnk), ideep(:,lchnk), 1, lengath(lchnk),  &
                  !nstep,   fracis,  ptend_loc%q, fake_dpdry)
   !call convtran (lchnk, ncol, pver, pverp,                                        &
   !               ptend_loc%lq,state_loc%q, pcnst,  mu(:ncol,:,lchnk), md(:ncol,:,lchnk),   &
   !               du(:ncol,:,lchnk), eu(:ncol,:,lchnk), ed(:ncol,:,lchnk), dp(:ncol,:,lchnk), &
   !               dsubcld(:ncol,lchnk),  &
   !               jt(:ncol,lchnk),maxg(:ncol,lchnk), ideep(:ncol,lchnk), 1, lengath(lchnk),  &
   !               nstep,   fracis(:ncol,:,:),  ptend_loc%q(:ncol,:,:), fake_dpdry(:ncol,:) )
! yhy test:
    call convtran (lchnk, ncol, pver, pverp,                                        &
                  ptend_loc%lq(1:3),state_loc%q(:ncol,:,1:3), 3,  mu(:ncol,:,lchnk), md(:ncol,:,lchnk),   &
                  du(:ncol,:,lchnk), eu(:ncol,:,lchnk), ed(:ncol,:,lchnk), dp(:ncol,:,lchnk), &
                  dsubcld(:ncol,lchnk),  &
                  jt(:ncol,lchnk),maxg(:ncol,lchnk), ideep(:ncol,lchnk), 1, lengath(lchnk),  &
                  nstep,   fracis(:ncol,:,1:3),  ptend_loc%q(:ncol,:,1:3), fake_dpdry(:ncol,:) )
   call t_stopf ('convtran1')
if(i<0)then
write(*,*)'---zyx after convtran'
write(*,*)'ptend_loc%q',ptend_loc%q
endif

   call outfld('ZMDICE ',ptend_loc%q(1,1,ixcldice) ,pcols   ,lchnk   )
   call outfld('ZMDLIQ ',ptend_loc%q(1,1,ixcldliq) ,pcols   ,lchnk   )

   ! add tendency from this process to tend from other processes here
   call physics_ptend_sum(ptend_loc,ptend_all, ncol)

1002 continue

   call physics_state_dealloc(state_loc)
   call physics_ptend_dealloc(ptend_loc)

!write(*,*) 'in zyx_conv'

end subroutine zyx_conv_intr_tend
!=========================================================================================


subroutine zyx_conv_tend_2( state,  ptend,  ztodt, pbuf)

   use physics_types, only: physics_state, physics_ptend, physics_ptend_init
   use time_manager,  only: get_nstep
   use physics_buffer, only: pbuf_get_index, pbuf_get_field, physics_buffer_desc
   use constituents,  only: pcnst, cnst_get_ind, cnst_is_convtran1
   use error_messages, only: alloc_err	
 
! Arguments
   type(physics_state), intent(in )   :: state          ! Physics state variables
   type(physics_ptend), intent(out)   :: ptend          ! indivdual parameterization tendencies
   
   type(physics_buffer_desc), pointer :: pbuf(:)

   real(r8), intent(in) :: ztodt                          ! 2 delta t (model time increment)

! Local variables
   integer :: i, lchnk, istat
   integer :: nstep
   integer :: ncol
   real(r8), dimension(pcols,pver) :: dpdry

! physics buffer fields 
   integer itim, ifld
   real(r8), pointer, dimension(:,:,:) :: fracis  ! fraction of transported species that are insoluble
   logical   :: lq(pcnst)

!
! Initialize
!
  lq(:) = .FALSE.
  lq(:) = .not. cnst_is_convtran1(:)
  call physics_ptend_init(ptend, state%psetcols, 'convtran2', lq=lq )

!
! Associate pointers with physics buffer fields
!
   ifld = pbuf_get_index('FRACIS')
   call pbuf_get_field(pbuf, fracis_idx, fracis, start=(/1,1,1/), kount=(/pcols, pver, pcnst/) )

!
! Transport all constituents except cloud water and ice
!

  lchnk = state%lchnk
  ncol  = state%ncol
  nstep = get_nstep()

   if (any(ptend%lq(:))) then
      ! initialize dpdry for call to convtran
      ! it is used for tracers of dry mixing ratio type
      dpdry = 0._r8
!      do i = 1,lengath(lchnk)
         !dpdry(i,:) = state%pdeldry(ideep(i,lchnk),:)/100._r8
      do i = 1,pcols
         dpdry(i,:) = state%pdeldry(i,:)/100._r8
      end do

!!      write(*,*) 'in zyx_conv_tend_2 1'
      call t_startf ('convtran2')


if(i<0)then
write(*,*)lchnk, ncol, pver, pverp
write(*,*)'in tend_2 ptend%lq',ptend%lq,nstep,lengath(lchnk), ncol
!write(*,*)'pcnst,  mu(:,:,lchnk), md(:,:,lchnk)',pcnst,  mu(:,:,lchnk), md(:,:,lchnk)
!write(*,*)'du(:,:,lchnk)',du(:,:,lchnk)
!write(*,*)'eu(:,:,lchnk)',eu(:,:,lchnk)
!write(*,*)'ed(:,:,lchnk),',ed(:,:,lchnk)
!write(*,*)'dp(:,:,lchnk)',dp(:,:,lchnk)
!write(*,*)'dsubcld(:,lchnk)',dsubcld(:,lchnk)
write(*,*)'jt(:,lchnk)',jt(:,lchnk)
write(*,*)'maxg(:,lchnk)',maxg(:,lchnk)
!write(*,*)'fracis',fracis
!write(*,*)'ptend%q',ptend%q
!write(*,*)'dpdry',dpdry
endif

!MZ
      !call convtran (lchnk, &
                     !ptend%lq,state%q, pcnst,  mu(:,:,lchnk), md(:,:,lchnk),   &
                     !du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk),  &
                     !jt(:,lchnk),maxg(:,lchnk),ideep(:,lchnk), 1, lengath(lchnk),  &
                     !nstep,   fracis,  ptend%q, dpdry)
      !call convtran (lchnk, pcols, pver, pverp,                                        &
      !               ptend%lq,state%q(:ncol,:,:), pcnst,  mu(:ncol,:,lchnk), md(:ncol,:,lchnk),   &
      !               du(:ncol,:,lchnk), eu(:ncol,:,lchnk), ed(:ncol,:,lchnk), &
      !               dp(:ncol,:,lchnk), dsubcld(:,lchnk),  &
      !               jt(:ncol,lchnk),maxg(:ncol,lchnk),ideep(:ncol,lchnk), 1, lengath(lchnk),  &
      !               nstep,   fracis(:ncol,:,:),  ptend%q(:ncol,:,:), dpdry(:ncol,:))
      
! yhy test:
      call convtran (lchnk, pcols, pver, pverp,                                        &
                     ptend%lq(1:3), state%q(:ncol,:,1:3), 3,  mu(:ncol,:,lchnk), md(:ncol,:,lchnk),   &
                     du(:ncol,:,lchnk), eu(:ncol,:,lchnk), ed(:ncol,:,lchnk), &
                     dp(:ncol,:,lchnk), dsubcld(:,lchnk),  &
                     jt(:ncol,lchnk), maxg(:ncol,lchnk), ideep(:ncol,lchnk), 1, lengath(lchnk),  &
                     nstep, fracis(:ncol,:,1:3),  ptend%q(:ncol,:,1:3), dpdry(:ncol,:) )
    
        call t_stopf ('convtran2')
  end if

  call foutput_2d('zyx intro: zyx_conv_tend_2: after: q1: ', state%q(:ncol,:,1), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: q2: ', state%q(:ncol,:,2), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: q3: ', state%q(:ncol,:,3), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: q4: ', state%q(:ncol,:,4), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: q5: ', state%q(:ncol,:,5), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: mu: ', mu(:ncol,:,lchnk), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: md: ', md(:ncol,:,lchnk), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: du: ', du(:ncol,:,lchnk), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: eu: ', eu(:ncol,:,lchnk), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: ed: ', ed(:ncol,:,lchnk), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: dp: ', dp(:ncol,:,lchnk), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: fracis1: ', fracis(:ncol,:,1), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: fracis2: ', fracis(:ncol,:,2), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: fracis3: ', fracis(:ncol,:,3), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: fracis4: ', fracis(:ncol,:,4), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: fracis5: ', fracis(:ncol,:,5), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: qtend1: ', ptend%q(:ncol,:,1), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: qtend2: ', ptend%q(:ncol,:,2), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: qtend3: ', ptend%q(:ncol,:,3), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: qtend4: ', ptend%q(:ncol,:,4), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: qtend5: ', ptend%q(:ncol,:,5), ncol, pver)
  call foutput_2d('zyx intro: zyx_conv_tend_2: after: dpdry: ', dpdry(:ncol,:), ncol, pver)

!!      write(*,*) 'in zyx_conv_tend_2 2'

end subroutine zyx_conv_tend_2


end module zyx_conv_intr

