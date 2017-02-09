
module conv_intr_jp

use shr_kind_mod, only: r8=>shr_kind_r8
use ppgrid,       only: pver, pcols, pverp
use physconst,    only: cpair, gravit

use physics_types, only: physics_state, physics_ptend
use physics_types, only: physics_ptend_init, physics_ptend_sum
use physics_types, only: physics_update, physics_ptend_dealloc

use cam_history,  only: outfld, addfld, add_default, phys_decomp

use cam_logfile,  only: iulog

!use scp_conv,     only: scp_conv_init, scp_conv_tend
use conv_jp,     only: conv_jp_init, conv_jp_tend

implicit none
private
save

public :: conv_intr_jp_init, conv_intr_jp_register, conv_intr_jp_tend

!  indices for fields in the physics buffer
integer  :: cld_idx          = 0
integer  :: icwmrdp_idx      = 0
integer  :: rprddp_idx       = 0
integer  :: fracis_idx       = 0
integer  :: nevapr_dpcu_idx  = 0
integer  :: prec_dp_idx      = 0
integer  :: snow_dp_idx      = 0

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


contains

!------------------------------------------------------
subroutine conv_intr_jp_register
!------------------------------------------------------
!register for memory and so on
!------------------------------------------------------
    use physics_buffer, only : pbuf_add_field, dtype_r8

!used since CESM V1.2.2
!! Flux of precipitation from deep convection (kg/m2/s)
   !call pbuf_add_field('DP_FLXPRC','global',dtype_r8,(/pcols,pverp/),dp_flxprc_idx)
!! Flux of snow from deep convection (kg/m2/s)
   !call pbuf_add_field('DP_FLXSNW','global',dtype_r8,(/pcols,pverp/),dp_flxsnw_idx)
!! deep gbm cloud liquid water (kg/kg)
   !call pbuf_add_field('DP_CLDLIQ','global',dtype_r8,(/pcols,pver/), dp_cldliq_idx)
!! deep gbm cloud liquid water (kg/kg)
   !call pbuf_add_field('DP_CLDICE','global',dtype_r8,(/pcols,pver/), dp_cldice_idx)

end subroutine conv_intr_jp_register



!------------------------------------------------------
subroutine conv_intr_jp_init

   use physics_buffer, only: pbuf_get_index

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

   call conv_jp_init( pver )
   massflxbase_p_idx = pbuf_get_index('MASSFLXBASE_P')

end subroutine conv_intr_jp_init



!------------------------------------------------------
subroutine conv_intr_jp_tend( &
        ztodt, landfrac, lhflx, state &
       ,ptend_all, pbuf, dlf)

    use physics_buffer, only : pbuf_get_field, physics_buffer_desc, pbuf_old_tim_idx
    use constituents,  only: pcnst
    use scamMod,       only: single_column, wfld

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

!local
   integer  :: ncol, lchnk
   logical  :: lq(pcnst)
   integer  :: itim 

   type(physics_ptend) :: ptend_loc     ! package tendencies

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

   real(r8), pointer, dimension(:,:) :: flxprec      ! Convective-scale flux of precip at interfaces (kg/m2/s)
   real(r8), pointer, dimension(:,:) :: flxsnow      ! Convective-scale flux of snow   at interfaces (kg/m2/s)
   real(r8), pointer, dimension(:,:) :: dp_cldliq
   real(r8), pointer, dimension(:,:) :: dp_cldice

   real(r8),pointer :: bfls_t(:,:)           ! temp state right after conv
   real(r8),pointer :: bfls_q(:,:)           ! state right after conv

   real(r8), pointer, dimension(:,:) :: massflxbase_p

   real(r8) dilucape(pcols) !thickness in [m]
   real(r8) bfls_dilucape(pcols) !thickness in [m]

   real(r8) zsrf(pcols)    ! model lowest interface Z
   real(r8) dz(pcols,pver) ! model delta Z
   real(r8) z(pcols,pver)  ! Z in the level middel
   real(r8) omega(pcols,pver) ! OMEGA

   real(r8) :: stend(pcols,pver)    ! s tend
   real(r8) :: qtend(pcols,pver)    ! q tend

!for diagnostics
   real(r8) :: qliqtend(pcols,pver)     ! liquid water tendency

   real(r8) :: stendcomp(pcols,pver)    ! s tend but calculated from compensation
   real(r8) :: qtendcomp(pcols,pver)    ! q tend but calculated from compensation

   real(r8) :: outmb(pcols)       ! scattrd version of the detraining cld h2o tend
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


   integer :: i, j, k
   logical :: flag

   lchnk = state%lchnk
   ncol  = state%ncol

   itim = pbuf_old_tim_idx()
   call pbuf_get_field(pbuf, cld_idx,         cld,    start=(/1,1,itim/), kount=(/pcols,pver,1/) )
   call pbuf_get_field(pbuf, icwmrdp_idx,     ql )
   call pbuf_get_field(pbuf, rprddp_idx,      rprd )
   call pbuf_get_field(pbuf, fracis_idx,      fracis, start=(/1,1,1/),    kount=(/pcols, pver, pcnst/) )
   call pbuf_get_field(pbuf, nevapr_dpcu_idx, evapcdp )
   call pbuf_get_field(pbuf, prec_dp_idx,     prec )
   call pbuf_get_field(pbuf, snow_dp_idx,     snow )

   call pbuf_get_field(pbuf, cldtop_idx, jctop )
   call pbuf_get_field(pbuf, cldbot_idx, jcbot )

!   cld = 0._r8
   ql  = 0._r8
   rprd = 0._r8
!   fracis = 0._r8
   evapcdp = 0._r8
   prec = 0._r8
   snow = 0._r8

   call pbuf_get_field(pbuf, pblh_idx,  pblh)
   call pbuf_get_field(pbuf, tpert_idx, tpert)

!used since CESM V1.2.2
   !call pbuf_get_field(pbuf, dp_flxprc_idx, flxprec    )
   !call pbuf_get_field(pbuf, dp_flxsnw_idx, flxsnow    )
   !call pbuf_get_field(pbuf, dp_cldliq_idx, dp_cldliq  )
   !call pbuf_get_field(pbuf, dp_cldice_idx, dp_cldice  )

   !flxprec = 0._r8
   !flxsnow = 0._r8
   !dp_cldliq = 0._r8
   !dp_cldice = 0._r8

   call pbuf_get_field(pbuf, massflxbase_p_idx, massflxbase_p )

!xiex
   call pbuf_get_field(pbuf, bfls_t_idx, bfls_t)
   call pbuf_get_field(pbuf, bfls_q_idx, bfls_q)


   lq(:) = .false.
   lq(1) = .true.
   call physics_ptend_init(ptend_all, state%psetcols, 'convect_deep')
!   call physics_ptend_init(ptend_all, state%psetcols, 'convect_deep', ls=.true., lq=lq)

   call physics_ptend_init(ptend_loc, state%psetcols, 'scp_convr', ls=.true., lq=lq)

   if (single_column) then
       omega(1,:) = wfld
   else
       omega(:ncol,:) = state%omega
   endif

!for layer depth
   do k=1,pver
       dz(:ncol,k) = state%zi(:ncol,k) - state%zi(:ncol,k+1)
       z(:ncol,k)  = state%zm(:ncol,k) + state%phis(:ncol)/gravit
   end do
   zsrf(:ncol) = state%phis(:ncol)/gravit

   call conv_jp_tend( &
!input
       ncol, &
       2, 15, 1._r8*ztodt, &
       state%ulat(:ncol), landfrac(:ncol), lhflx(:ncol), &
       state%ps(:ncol), state%pmid(:ncol,:), state%pdel(:ncol,:), &
       zsrf(:ncol), z(:ncol,:), dz(:ncol,:), &
       state%t(:ncol,:), state%q(:ncol,:,1), &
       bfls_t(:ncol,:), bfls_q(:ncol,:), &
       omega(:ncol,:), pblh(:ncol), tpert(:ncol), &
!in/output
       massflxbase_p(:ncol,:), &
!output
       jctop(:ncol), jcbot(:ncol), &
       stend(:ncol,:), qtend(:ncol,:), &
       qliqtend(:ncol,:), &
       prec(:ncol), ql(:ncol,:), rprd(:ncol,:), &
       stendcomp(:ncol,:), qtendcomp(:ncol,:), &
       dilucape(:ncol), bfls_dilucape(:ncol), &
!diagnostics
       outtmp2d(:ncol), outtmp3d(:ncol,:), &
       outmb(:ncol), outmse(:ncol,:), outmsesat(:ncol,:), outmseup(:ncol,:), &
       outstend(:ncol,:), outqtend(:ncol,:), &
       outstendcond(:ncol,:), outqtendcond(:ncol,:), &
       outstendtranup(:ncol,:), outqtendtranup(:ncol,:), &
       outstendtrandn(:ncol,:), outqtendtrandn(:ncol,:), &
       outstendevap(:ncol,:), outqtendevap(:ncol,:) &
       )

   ptend_loc%s(:ncol,:)   = stend(:ncol,:)
   ptend_loc%q(:ncol,:,1) = qtend(:ncol,:)
   !ptend_loc%s(:ncol,:)   = stendcomp(:ncol,:)
   !ptend_loc%q(:ncol,:,1) = qtendcomp(:ncol,:)


   call outfld('TMP2D', outtmp2d, pcols, state%lchnk )
   call outfld('TMP3D', outtmp3d, pcols, state%lchnk )

   call outfld('MASSFLXBASE_P', massflxbase_p, pcols, state%lchnk )

   call outfld('CONVDPMB', outmb, pcols, state%lchnk )
   call outfld('MSE', outmse, pcols, state%lchnk )
   call outfld('MSESAT', outmsesat, pcols, state%lchnk )
   call outfld('MSEUP', outmseup, pcols, state%lchnk )
   call outfld('CONVZ', z, pcols, state%lchnk )

   call outfld('STENDCONVDP', stend, pcols, lchnk)
   call outfld('QTENDCONVDP', qtend, pcols, lchnk)

   call outfld('STENDCONVDPCOND', outstendcond, pcols, lchnk)
   call outfld('QTENDCONVDPCOND', outqtendcond, pcols, lchnk)
   call outfld('STENDCONVDPTRANUP', outstendtranup, pcols, lchnk)
   call outfld('QTENDCONVDPTRANUP', outqtendtranup, pcols, lchnk)
   call outfld('STENDCONVDPTRANDN', outstendtrandn, pcols, lchnk)
   call outfld('QTENDCONVDPTRANDN', outqtendtrandn, pcols, lchnk)
   call outfld('STENDCONVDPEVAP', outstendevap, pcols, lchnk)
   call outfld('QTENDCONVDPEVAP', outqtendevap, pcols, lchnk)

   call outfld('STENDCONVDPCOMP', stendcomp, pcols, lchnk)
   call outfld('QTENDCONVDPCOMP', qtendcomp, pcols, lchnk)

   call outfld('DILUCAPE', dilucape, pcols, lchnk)            ! RBN - CAPE output
   call outfld('BFLSDILUCAPE', bfls_dilucape, pcols, lchnk)   ! RBN - CAPE output

   do i = 1, ncol
       do k = 1, pver
           if ( isnan( ptend_loc%s(i,k) ) .or. &
                isnan( ptend_loc%q(i,k,1) ) ) then
               write(iulog,*) 'problem in conv_jp', i, k
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

   !ptend_loc%s = 0._r8
   !ptend_loc%q(:,:,1) = 0._r8
!   prec = 0._r8

   dlf(:ncol,1:pver)  = 0._r8
   ql(:ncol,1:pver)   = 0._r8
   rprd(:ncol,1:pver) = 0._r8

   call physics_ptend_sum(ptend_loc, ptend_all, ncol)

   call physics_ptend_dealloc(ptend_loc)

end subroutine conv_intr_jp_tend

end module conv_intr_jp

