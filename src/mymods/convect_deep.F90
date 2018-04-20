
module convect_deep
!---------------------------------------------------------------------------------
! Purpose:
!
! CAM interface to several deep convection interfaces. Currently includes:
!    Zhang-McFarlane (default)
!    Kerry Emanuel 
!
!
! Author: D.B. Coleman, Sep 2004
!
!---------------------------------------------------------------------------------
   use shr_kind_mod, only: r8=>shr_kind_r8
   use ppgrid,       only: pver, pcols, pverp, begchunk, endchunk
   use cam_logfile,  only: iulog

   implicit none

   save
   private                         ! Make default type private to the module

! Public methods

   public ::&
      convect_deep_register,           &! register fields in physics buffer
      convect_deep_init,               &! initialize donner_deep module
      convect_deep_tend,               &! return tendencies
      convect_deep_tend_2,             &! return tendencies
      deep_scheme_does_scav_trans             ! = .t. if scheme does scavenging and conv. transport
   
! Private module data
   character(len=16) :: deep_scheme    ! default set in phys_control.F90, use namelist to change
! Physics buffer indices 
   integer     ::  icwmrdp_idx      = 0 
   integer     ::  rprddp_idx       = 0 
   integer     ::  nevapr_dpcu_idx  = 0 
   integer     ::  cldtop_idx       = 0 
   integer     ::  cldbot_idx       = 0 
   integer     ::  cld_idx          = 0 
   integer     ::  fracis_idx       = 0 

   integer     ::  pblh_idx        = 0 
   integer     ::  tpert_idx       = 0 
   integer     ::  prec_dp_idx     = 0
   integer     ::  snow_dp_idx     = 0

   integer     ::  zmdt_idx        = 0
!xiex
   integer     ::  bfls_t_idx   = 0
   integer     ::  bfls_q_idx   = 0
   integer     ::  massflxbase_p_idx = 0


!=========================================================================================
  contains 

!=========================================================================================
function deep_scheme_does_scav_trans()
!
! Function called by tphysbc to determine if it needs to do scavenging and convective transport
! or if those have been done by the deep convection scheme. Each scheme could have its own
! identical query function for a less-knowledgable interface but for now, we know that KE 
! does scavenging & transport, and ZM doesn't
!

  logical deep_scheme_does_scav_trans

  deep_scheme_does_scav_trans = .false.

  if ( deep_scheme .eq. 'KE' ) deep_scheme_does_scav_trans = .true.

  return

end function deep_scheme_does_scav_trans

!=========================================================================================
subroutine convect_deep_register

!----------------------------------------
! Purpose: register fields with the physics buffer
!----------------------------------------

  
  use physics_buffer, only : pbuf_add_field, dtype_r8
  use zm_conv_intr, only: zm_conv_register
  use phys_control, only: phys_getopts
!xiex
  use conv_intr_jp, only: conv_intr_jp_register

  implicit none

  integer idx

  ! get deep_scheme setting from phys_control
  call phys_getopts(deep_scheme_out = deep_scheme)

  select case ( deep_scheme )
  case('ZM') !    Zhang-McFarlane (default)
     call zm_conv_register
  case('SCP') !   SCP
     call conv_intr_jp_register
  end select

  call pbuf_add_field('ICWMRDP',    'physpkg',dtype_r8,(/pcols,pver/),icwmrdp_idx)
  call pbuf_add_field('RPRDDP',     'physpkg',dtype_r8,(/pcols,pver/),rprddp_idx)
  call pbuf_add_field('NEVAPR_DPCU','physpkg',dtype_r8,(/pcols,pver/),nevapr_dpcu_idx)
  call pbuf_add_field('PREC_DP',    'physpkg',dtype_r8,(/pcols/),     prec_dp_idx)
  call pbuf_add_field('SNOW_DP',   'physpkg',dtype_r8,(/pcols/),      snow_dp_idx)
!xiex
  call pbuf_add_field('MBCONVDP_P', 'physpkg', dtype_r8, (/pcols,pver/), massflxbase_p_idx)
!xiex over

end subroutine convect_deep_register

!=========================================================================================



subroutine convect_deep_init(pref_edge)

!----------------------------------------
! Purpose:  declare output fields, initialize variables needed by convection
!----------------------------------------

  use pmgrid,        only: plevp
  use spmd_utils,    only: masterproc
  use zm_conv_intr,  only: zm_conv_init
  use abortutils,    only: endrun
  use phys_control,  only: do_waccm_phys
  
  use physics_buffer,        only: physics_buffer_desc, pbuf_get_index

  use cam_history,        only: addfld, phys_decomp
  use conv_intr_jp,  only: conv_intr_jp_init
  use physics_buffer,        only: pbuf_get_field

!------------------------------------
! Haiyang Yu
    use nnparameter, only: readnnparameter
!-------------------------------------

  implicit none

  real(r8),intent(in) :: pref_edge(plevp)        ! reference pressures at interfaces
  integer k

  ! Haiyang Yu
    call readnnparameter('atm_in')

  select case ( deep_scheme )
  case('off') !     ==> no deep convection
     if (masterproc) write(iulog,*)'convect_deep: no deep convection selected'
  case('ZM') !    1 ==> Zhang-McFarlane (default)
     if (masterproc) write(iulog,*)'convect_deep initializing Zhang-McFarlane convection'
     call zm_conv_init(pref_edge)
!xiex
  case('SCP') !   2 ==> SCP
     if (masterproc) write(iulog,*)'convect_deep initializing SCP convection'
     call conv_intr_jp_init
  case default
     if (masterproc) write(iulog,*)'WARNING: convect_deep: no deep convection scheme. May fail.'
  end select

!xiex
  call addfld ('OFFU', 'm/s',     pver, 'I', 'U',    phys_decomp)
  call addfld ('OFFV', 'm/s',     pver, 'I', 'V',    phys_decomp)
  call addfld ('OFFOMEGA','Pa/s', pver, 'I', 'OMEGA',    phys_decomp)
  call addfld ('OFFT', 'K',     pver, 'I', 'T',    phys_decomp)
  call addfld ('OFFQ', 'kg/kg',     pver, 'I', 'Q',    phys_decomp)
  call addfld ('CONVZ', 'kg/kg',  pver, 'I', 'Z',    phys_decomp)

  call addfld ('BFLST', 'K',     pver, 'I', 'BFLST',    phys_decomp)
  call addfld ('BFLSQ', 'kg/kg', pver, 'I', 'BFLSQ',    phys_decomp)

  call addfld ('MSE','J/kg     ',pver, 'I','MSE - SCP', phys_decomp)
  call addfld ('MSESAT','J/kg     ',pver, 'I','MSESAT - SCP', phys_decomp)
  call addfld ('MSEUP','J/kg     ',pver, 'I','MSEUP - SCP', phys_decomp)

!xiex
  call addfld ('MBCONVDP_P', 'K/s',       pver, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('MBCONVDP'  , 'K/s',          1, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('DLF', 'K/s',       pver, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('DLFSUM' , 'K/s',            1, 'I', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('RLIQ'   , 'K/s',            1, 'I', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('MFCONVDP', 'K/s',       pver, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('MFCONVDPSUM', 'K/s',       1, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('PRECRATESUM' , 'K/s',            1, 'I', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('QTENDSUM' , 'K/s',            1, 'I', 'T tendency - zm cond tendt',phys_decomp)


  call addfld ('STENDCONVDP', 'K/s',       pver, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('QTENDCONVDP', 'kg/kg/s',   pver, 'A', 'Q tendency - zm cond tendq',phys_decomp)

  call addfld ('STENDCONVDPCOND', 'K/s',       pver, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('QTENDCONVDPCOND', 'kg/kg/s',   pver, 'A', 'Q tendency - zm cond tendq',phys_decomp)
  call addfld ('STENDCONVDPTRANUP', 'K/s',     pver, 'A', 'T tendency - zm trans tendt',phys_decomp)
  call addfld ('QTENDCONVDPTRANUP', 'kg/kg/s', pver, 'A', 'Q tendency - zm trans tendq',phys_decomp)
  call addfld ('STENDCONVDPTRANDN', 'K/s',     pver, 'A', 'T tendency - zm trans tendt',phys_decomp)
  call addfld ('QTENDCONVDPTRANDN', 'kg/kg/s', pver, 'A', 'Q tendency - zm trans tendq',phys_decomp)
  call addfld ('STENDCONVDPEVAP', 'K/s',       pver, 'A', 'T tendency - zm cond tendt',phys_decomp)
  call addfld ('QTENDCONVDPEVAP', 'kg/kg/s',   pver, 'A', 'Q tendency - zm cond tendq',phys_decomp)

  call addfld ('STENDCONVDPCOMP', 'K/s',       pver, 'A', 'T tendency - zm compensate tendt',phys_decomp)
  call addfld ('QTENDCONVDPCOMP', 'kg/kg/s',   pver, 'A', 'Q tendency - zm compensate tendq',phys_decomp)

  call addfld ('DILUCAPE',     'J/kg',   1, 'I', &
      'Convectively available potential energy', phys_decomp)
  call addfld ('BFLSDILUCAPE', 'J/kg',   1, 'I', &
      'Convectively available potential energy', phys_decomp)
  
! yhy  
  call addfld ('NNSTEND', 'J/kg/s', pver, 'A', 'NN stend', phys_decomp)
  call addfld ('NNQTEND', 'kg/kg/s', pver, 'A', 'NN qtend', phys_decomp)
  call addfld ('NNPREC', 'm/s', 1, 'I', 'NN prec', phys_decomp)

!xiex
  bfls_t_idx = pbuf_get_index('BFLS_T')
  bfls_q_idx = pbuf_get_index('BFLS_Q')


  cldtop_idx = pbuf_get_index('CLDTOP')
  cldbot_idx = pbuf_get_index('CLDBOT')
  cld_idx    = pbuf_get_index('CLD')
  fracis_idx = pbuf_get_index('FRACIS')

  pblh_idx   = pbuf_get_index('pblh')
  tpert_idx  = pbuf_get_index('tpert')

  if (do_waccm_phys()) zmdt_idx = pbuf_get_index('ZMDT')

end subroutine convect_deep_init
!=========================================================================================
!subroutine convect_deep_tend(state, ptend, tdt, pbuf)

subroutine convect_deep_tend( &
     mcon    ,cme     ,          &
     dlf     ,pflx    ,zdu      , &
     rliq    , &
     ztodt   , &
!xiex
     state   ,ptend   ,landfrac , lhflx, pbuf)
!xiex.
!clean
!     state   ,ptend   ,landfrac ,pbuf)

   use physics_types, only: physics_state, physics_ptend, physics_tend, physics_ptend_init
   
   use constituents,   only: pcnst
   use zm_conv_intr,   only: zm_conv_tend
!xiex
   use conv_intr_jp, only: conv_intr_jp_tend
   use time_manager,  only: is_first_step
   use scamMod,       only: single_column, wfld

   use cam_history,    only: outfld
   use physconst,      only: cpair
   use phys_control,   only: do_waccm_phys
   use physics_buffer, only: physics_buffer_desc, pbuf_get_field

! Arguments
   type(physics_state), intent(in ) :: state   ! Physics state variables
   type(physics_ptend), intent(out) :: ptend   ! individual parameterization tendencies
   

   type(physics_buffer_desc), pointer :: pbuf(:)
   real(r8), intent(in) :: ztodt               ! 2 delta t (model time increment)
   real(r8), intent(in) :: landfrac(pcols)     ! Land fraction
!xiex
   real(r8), intent(in) :: lhflx(pcols)        ! latent heat flux
!xiex.
      

   real(r8), intent(out) :: mcon(pcols,pverp)  ! Convective mass flux--m sub c
   real(r8), intent(out) :: dlf(pcols,pver)    ! scattrd version of the detraining cld h2o tend
   real(r8), intent(out) :: pflx(pcols,pverp)  ! scattered precip flux at each level
   real(r8), intent(out) :: cme(pcols,pver)    ! cmf condensation - evaporation
   real(r8), intent(out) :: zdu(pcols,pver)    ! detraining mass flux

   real(r8), intent(out) :: rliq(pcols) ! reserved liquid (not yet in cldliq) for energy integrals

   real(r8), pointer :: prec(:)   ! total precipitation
   real(r8), pointer :: snow(:)   ! snow from ZM convection 

   real(r8), pointer, dimension(:) :: jctop
   real(r8), pointer, dimension(:) :: jcbot
   real(r8), pointer, dimension(:,:,:) :: cld        
   real(r8), pointer, dimension(:,:) :: ql        ! wg grid slice of cloud liquid water.
   real(r8), pointer, dimension(:,:) :: rprd      ! rain production rate
   real(r8), pointer, dimension(:,:,:) :: fracis  ! fraction of transported species that are insoluble
!xiex
   logical :: flag
   integer :: ncol, lchnk
   real(r8) :: dlfsum(pcols)
   real(r8) :: mconsum(pcols)
   real(r8) :: rho(pcols,pver)    ! detraining mass flux
   real(r8) :: dz(pcols,pver)    ! detraining mass flux
   real(r8) :: precrate(pcols,pver)    ! detraining mass flux
   real(r8) :: precratesum(pcols)
   real(r8) :: qtendsum(pcols)

   real(r8),pointer :: bfls_t(:,:)           ! temp state right after conv
   real(r8),pointer :: bfls_q(:,:)           ! state right after conv

   real(r8),pointer :: massflxbase_p(:,:)           ! state right after conv

   real(r8), pointer, dimension(:,:) :: evapcdp   ! Evaporation of deep convective precipitation

   real(r8), pointer :: pblh(:)                ! Planetary boundary layer height
   real(r8), pointer :: tpert(:)               ! Thermal temperature excess 

   real(r8) zero(pcols, pver)

   integer i, k

   real(r8), pointer, dimension(:,:) :: zmdt
   ! (Is this necessary, or can we pass zmdt to outfld directly?)
   real(r8) :: ftem(pcols,pver)              ! Temporary workspace for outfld variables

   call pbuf_get_field(pbuf, cldtop_idx, jctop )
   call pbuf_get_field(pbuf, cldbot_idx, jcbot )

!xiex
   call outfld('OFFU    ',state%u           ,pcols   , state%lchnk   )
   call outfld('OFFV    ',state%v           ,pcols   , state%lchnk   )

   if (single_column) then
       call outfld('OFFOMEGA   ',wfld,    pcols,   state%lchnk     )
   else
       call outfld('OFFOMEGA   ',state%omega,    pcols,   state%lchnk     )
   endif

   call outfld('OFFT    ',state%t           ,pcols   , state%lchnk   )
   call outfld('OFFQ    ',state%q(:,:,1)    ,pcols   , state%lchnk )

   call pbuf_get_field(pbuf, bfls_t_idx, bfls_t)
   call pbuf_get_field(pbuf, bfls_q_idx, bfls_q)

   call pbuf_get_field(pbuf, massflxbase_p_idx, massflxbase_p)


   if ( .not. is_first_step() ) then
       call outfld('BFLST', bfls_t, pcols, state%lchnk   )
       call outfld('BFLSQ', bfls_q, pcols, state%lchnk   )
   else
       bfls_t = state%t
       bfls_q = state%q(:,:,1)
       call outfld('BFLST', state%t           ,pcols   , state%lchnk )
       call outfld('BFLSQ', state%q(:,:,1)    ,pcols   , state%lchnk )

       massflxbase_p = 0._r8

   end if

   !do i=1,pcols
       !flag = .false.
       !do k=1,pver
           !if( state%zm(i,k)==0. ) flag = .true.
       !end do
       !if( flag ) then
           !write(*,*) "zm"
           !write(*,*) state%zm(i,:)
       !end if
   !end do


!test
!write(*,*) "convect_deep:dtime=",ztodt

   select case ( deep_scheme )
   case('off') !    0 ==> no deep convection
      zero = 0     
      mcon = 0
      dlf = 0
      pflx = 0
      cme = 0
      zdu = 0
      rliq = 0

      jctop = pver
      jcbot = 1._r8

      call physics_ptend_init(ptend, state%psetcols, 'convect_deep')
!
! Associate pointers with physics buffer fields
!
      call pbuf_get_field(pbuf, cld_idx,         cld,    start=(/1,1/),   kount=(/pcols,pver/) ) 
      call pbuf_get_field(pbuf, icwmrdp_idx,     ql )
      call pbuf_get_field(pbuf, rprddp_idx,      rprd )
      call pbuf_get_field(pbuf, fracis_idx,      fracis, start=(/1,1,1/), kount=(/pcols, pver, pcnst/) )
      call pbuf_get_field(pbuf, nevapr_dpcu_idx, evapcdp )
      call pbuf_get_field(pbuf, prec_dp_idx,     prec )
      call pbuf_get_field(pbuf, snow_dp_idx,     snow )

      cld = 0._r8
      ql  = 0._r8
      rprd = 0._r8
      fracis = 0._r8
      evapcdp = 0._r8
      prec = 0._r8
      snow = 0._r8


  case('ZM') !    1 ==> Zhang-McFarlane (default)
     call pbuf_get_field(pbuf, pblh_idx,  pblh)
     call pbuf_get_field(pbuf, tpert_idx, tpert)

     call zm_conv_tend( pblh    ,mcon    ,cme     , &
          tpert   ,dlf     ,pflx    ,zdu      , &
          rliq    , &
          ztodt   , &
          jctop, jcbot , &
          state   ,ptend   ,landfrac, pbuf, precrate)
!xiex
  case('SCP') !    2 ==> SCP

      zero = 0     
      mcon = 0
      cme  = 0
      dlf  = 0
      pflx = 0
      zdu  = 0
      rliq = 0

      call conv_intr_jp_tend( &
          ztodt, landfrac, lhflx, state &
         ,ptend, pbuf, dlf, mcon, precrate)

  end select

!xiex
  if( deep_scheme /= 'off' ) then
      call outfld('STENDCONVDP', ptend%s          ,pcols   , state%lchnk   )
      call outfld('QTENDCONVDP', ptend%q(:,:,1)   ,pcols   , state%lchnk   )
  end if

  ncol = state%ncol
  lchnk = state%lchnk
  do k=1,pver
      dz(:ncol,k) = state%zi(:ncol,k) - state%zi(:ncol,k+1)
  end do
  rho(:ncol,:) = state%pmid(:ncol,:)/state%t(:ncol,:)/287.0423
  dlfsum = 0.
  mconsum = 0.
  precratesum = 0.
  qtendsum = 0.
  do k=1,pver
      dlfsum(:ncol)  = dlfsum(:ncol) + dlf(:ncol,k)*rho(:ncol,k)*dz(:ncol,k)
      mconsum(:ncol)  = mconsum(:ncol) + mcon(:ncol,k)*rho(:ncol,k)*dz(:ncol,k)
      precratesum(:ncol)  = precratesum(:ncol) + precrate(:ncol,k)*rho(:ncol,k)*dz(:ncol,k)
      qtendsum(:ncol)  = qtendsum(:ncol) + ptend%q(:ncol,k,1)*rho(:ncol,k)*dz(:ncol,k)
  end do
  call outfld('DLF', dlf, pcols, lchnk)      ! RBN - CAPE output
  call outfld('DLFSUM', dlfsum, pcols, lchnk)      ! RBN - CAPE output
  call outfld('RLIQ',   rliq, pcols, lchnk)        ! RBN - CAPE output
  call outfld('MFCONVDP',   mcon(:,:pver), pcols, lchnk)        ! RBN - CAPE output
  call outfld('MFCONVDPSUM',   mconsum, pcols, lchnk)        ! RBN - CAPE output
  call outfld('PRECRATESUM', precratesum, pcols, lchnk)      ! RBN - CAPE output
  call outfld('QTENDSUM', qtendsum, pcols, lchnk)      ! RBN - CAPE output

  if( deep_scheme == 'SCP' ) then
!      mcon = 0.
  end if


  if (do_waccm_phys()) then
     call pbuf_get_field(pbuf, zmdt_idx, zmdt)
     ftem(:state%ncol,:pver) = ptend%s(:state%ncol,:pver)/cpair
     zmdt(:state%ncol,:pver) = ftem(:state%ncol,:pver)
     call outfld('ZMDT',ftem           ,pcols   ,state%lchnk   )
     call outfld('ZMDQ',ptend%q(:,:,1) ,pcols   ,state%lchnk   )
  end if


end subroutine convect_deep_tend
!=========================================================================================


subroutine convect_deep_tend_2( state,  ptend,  ztodt, pbuf)

   use physics_types, only: physics_state, physics_ptend
   
   use physics_buffer,  only: physics_buffer_desc
   use constituents, only: pcnst
   use zm_conv_intr, only: zm_conv_tend_2

! Arguments
   type(physics_state), intent(in ) :: state          ! Physics state variables
   type(physics_ptend), intent(out) :: ptend          ! indivdual parameterization tendencies
   
   type(physics_buffer_desc), pointer :: pbuf(:)

   real(r8), intent(in) :: ztodt                          ! 2 delta t (model time increment)

   if ( deep_scheme .eq. 'ZM' ) then  !    1 ==> Zhang-McFarlane (default)
      call zm_conv_tend_2( state,   ptend,  ztodt,  pbuf) 
   end if


end subroutine convect_deep_tend_2


end module convect_deep
