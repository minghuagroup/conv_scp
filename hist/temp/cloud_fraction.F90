#undef CLOUD_IAP
!#define CLOUD_IAP

module cloud_fraction

  ! Cloud fraction parameterization.


  use shr_kind_mod, only: r8 => shr_kind_r8
  use spmd_utils,   only: masterproc
  use cam_logfile,  only: iulog
  use abortutils,   only: endrun
!zmh
  use mzfunctions_mod, only: zmh_ramp
  use time_manager,    only : is_first_step
  use phys_control,    only : phys_getopts

  implicit none
  private
  save

  ! Public interfaces
  public &
     cldfrc_readnl,    &! read cldfrc_nl namelist
     cldfrc_init,      &! Inititialization of cloud_fraction run-time parameters
     cldfrc_getparams, &! public access of tuning parameters
     cldfrc,           &! Computation of cloud fraction
     zmh_cldfrc_fice,           &! Computation of cloud fraction
     cldfrc_fice,           &! Computation of cloud fraction
     cldfrc_implicit_PDF   

 ! ======= zmh ==============
     character(len=16),public :: macrop_scheme !put in namelist = 'ZMH_WXC','WXC','cam'
     logical,public           :: zmh_cloud_dp = .true. !false. !.true.
     logical,public           :: zmh_cloud_sh = .true. !false. !.true.
     real(8)                  :: zmh_vsub_pbl = 1.2 !fraction distance between k and k-1
 ! ======= zmh ==============


     real(r8),public :: sh1, sh2           ! set from namelist input cldfrc_sh1, cldfrc_sh2
     real(r8),public :: dp1,dp2            ! set from namelist input cldfrc_dp1, cldfrc_dp2

  ! Private data
  real(r8), parameter :: unset_r8 = huge(1.0_r8)

  ! Physics buffer indices 
  integer :: sh_frac_idx   = 0  
  integer :: dp_frac_idx   = 0 

  ! Namelist variables
  logical  :: cldfrc_freeze_dry           ! switch for Vavrus correction
  logical  :: cldfrc_ice                  ! switch to compute ice cloud fraction
  real(r8) :: cldfrc_rhminl = unset_r8    ! minimum rh for low stable clouds
  real(r8) :: cldfrc_rhminh = unset_r8    ! minimum rh for high stable clouds
  real(r8) :: cldfrc_sh1    = unset_r8    ! parameter for shallow convection cloud fraction
  real(r8) :: cldfrc_sh2    = unset_r8    ! parameter for shallow convection cloud fraction
  real(r8) :: cldfrc_dp1    = unset_r8    ! parameter for deep convection cloud fraction
  real(r8) :: cldfrc_dp2    = unset_r8    ! parameter for deep convection cloud fraction
  real(r8) :: cldfrc_premit = unset_r8    ! top pressure bound for mid level cloud


  real(r8) :: rhminl             ! set from namelist input cldfrc_rhminl
  real(r8) :: rhminh             ! set from namelist input cldfrc_rhminh
  !real(r8) :: sh1, sh2           ! set from namelist input cldfrc_sh1, cldfrc_sh2
  !real(r8) :: dp1,dp2            ! set from namelist input cldfrc_dp1, cldfrc_dp2
  real(r8) :: premit             ! set from namelist input cldfrc_premit
  real(r8), parameter :: pnot = 1.e5_r8         ! reference pressure
  real(r8), parameter :: lapse = 6.5e-3_r8      ! U.S. Standard Atmsophere lapse rate
  real(r8), parameter :: premib_uw  = 750.e2_r8 ! bottom pressure bound of middle cloud for UW
  real(r8), parameter :: premib_cam = 750.e2_r8 ! bottom pressure bound of middle cloud for CAM
  real(r8)            :: premib                 ! bottom pressure bound of middle cloud
  real(r8), parameter :: pretop = 1.0e2_r8      ! pressure bounding high cloud

  integer :: iceopt = 4                       ! option for ice cloud closure
                                              ! 1=wang & sassen 2=schiller (iciwc)
                                              ! 3=wood & field, 4=Wilson (based on smith)
  real(r8), parameter :: icecrit = 0.95_r8    ! Critical RH for ice clouds in Wilson & Ballard closure (smaller = more ice clouds)

  integer count
  character(len=16) :: microp_scheme

  logical :: inversion_cld_off    ! Turns off stratification-based cld frc

  integer,parameter :: m = 11
  real ::a1(m),a2(m),a3(m),a4(m),a5(m)

  data a1/60467.0662571,   38.2659748,  -3.3464110,  0.2043748,   23.5693578,  3.0712319,   24.7365718,  2.4698466,   -3.3835918,  -8.4236007,  -8.8943441 /
  data a2/-60453.9019592,  -11.3765423,   -3.8289507,  0.9189849,   -5.9444995,  3.1820843,   -6.2352172,  -1.5407019,  -3.9990818,  -9.3872110,  -9.5575717 /
  data a3/-0.0000671,  -0.9672381,  1.1331052,   -1.7565115,  -1.7349849,  1.0940753  , -1.7244260,  -2.3257599 , 1.0632891 ,  1.0869241 ,  1.1436086 /
  data a4/-4.4840695,  -9.4328458,  3.0297357,  -2.4526479,  -6.0734427 , -1.9510144  ,-6.3970533 , 2.4034298   ,3.0410805  , 6.8211675  , 7.0547149 /
  data a5/-2.1490486,  -1.8008714,  -0.8706704,  0.3378804,   -1.4411766,  -1.5956140 , -1.4251393,  0.7671822  , -1.0238685,  -1.2066939,  -1.2233873 / 

!================================================================================================
  contains
!================================================================================================

subroutine cldfrc_readnl(nlfile)

   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use mpishorthand

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: subname = 'cldfrc_readnl'

   namelist /cldfrc_nl/ cldfrc_freeze_dry, cldfrc_ice, cldfrc_rhminl, cldfrc_rhminh, &
                        cldfrc_sh1       , cldfrc_sh2, cldfrc_dp1   , cldfrc_dp2   , &
                        cldfrc_premit   !zmh , macrop_scheme  ! xwang
   !-----------------------------------------------------------------------------

   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'cldfrc_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, cldfrc_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)

      ! set local variables
      rhminl = cldfrc_rhminl
      rhminh = cldfrc_rhminh
      sh1    = cldfrc_sh1
      sh2    = cldfrc_sh2
      dp1    = cldfrc_dp1
      dp2    = cldfrc_dp2
      premit = cldfrc_premit

   end if

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast(cldfrc_freeze_dry, 1, mpilog, 0, mpicom)
   call mpibcast(cldfrc_ice,        1, mpilog, 0, mpicom)
   call mpibcast(rhminl,            1, mpir8,  0, mpicom)
   call mpibcast(rhminh,            1, mpir8,  0, mpicom)
   call mpibcast(sh1   ,            1, mpir8,  0, mpicom)
   call mpibcast(sh2   ,            1, mpir8,  0, mpicom)
   call mpibcast(dp1   ,            1, mpir8,  0, mpicom)
   call mpibcast(dp2   ,            1, mpir8,  0, mpicom)
   call mpibcast(premit,            1, mpir8,  0, mpicom)
#endif

end subroutine cldfrc_readnl

!================================================================================================

subroutine cldfrc_getparams(rhminl_out, rhminh_out, premit_out)
!-----------------------------------------------------------------------
! Purpose: Return cldfrc tuning parameters
!-----------------------------------------------------------------------

   real(r8),          intent(out), optional :: rhminl_out
   real(r8),          intent(out), optional :: rhminh_out
   real(r8),          intent(out), optional :: premit_out

   if ( present(rhminl_out) )      rhminl_out = rhminl
   if ( present(rhminh_out) )      rhminh_out = rhminh
   if ( present(premit_out) )      premit_out = premit

end subroutine cldfrc_getparams

!===============================================================================

  subroutine cldfrc_init
    !
    ! Purpose:
    ! Initialize cloud fraction run-time parameters
    !
    ! Author: J. McCaa
    !    
    use dycore,        only:  dycore_is, get_resolution
    use ppgrid,        only:  pver          
    use chemistry,     only:  chem_is
    use phys_control,  only:  phys_getopts
    use phys_buffer,   only:  pbuf_get_fld_idx 

    ! horizontal grid specifier
    character(len=32) :: hgrid

    ! query interfaces for scheme settings
    character(len=16) :: shallow_scheme, eddy_scheme

    call phys_getopts(shallow_scheme_out = shallow_scheme ,&
                      eddy_scheme_out    = eddy_scheme, &
                      microp_scheme_out  = microp_scheme, & 
                      macrop_scheme_out  = macrop_scheme)  !zmh

    hgrid = get_resolution()
!zmh
    if ( macrop_scheme .ne. 'ZMH_WXC' .and. macrop_scheme .ne.'WXC' ) then
        macrop_scheme = 'cam'
    endif


    if ( shallow_scheme .eq. 'UW' .or. shallow_scheme.eq.'ZW' ) then
       premib = premib_uw
    else 
       premib = premib_cam
    endif

    iceopt = 1

    ! Turn off inversion_cld if any UW PBL scheme is being used
   if ( (eddy_scheme .eq. 'diag_TKE' ) .or. (shallow_scheme .eq.  'UW' ) &
            .or.  (shallow_scheme.eq.'ZW' )) then
      inversion_cld_off = .true.
   else
      inversion_cld_off = .false.
   endif

    if ( masterproc ) then 
       write(iulog,*)'tuning parameters cldfrc_init: inversion_cld_off',inversion_cld_off
       write(iulog,*)'tuning parameters cldfrc_init: dp1',dp1,'dp2',dp2,'sh1',sh1,'sh2',sh2
!zmh   
       if ( shallow_scheme .eq. 'UW' .or. shallow_scheme.eq.'ZW' ) then
       write(iulog,*)'tuning parameters cldfrc_init: rhminl',rhminl,'rhminh',rhminh,'premit',premit,'premib',premib
       endif 
    endif


   ! Get physics buffer indices
   sh_frac_idx = pbuf_get_fld_idx('SH_FRAC')
   dp_frac_idx = pbuf_get_fld_idx('DP_FRAC')

  end subroutine cldfrc_init

  subroutine cldfrc(lchnk   ,ncol    , pbuf, &
       pmid    ,temp    ,q       ,omga    , phis, &
       shfrc   ,deepfrc , use_shfrc, &
       cloud   ,rhcloud, clc     ,pdel    , &
       cmfmc   ,cmfmc2  ,landfrac,snowh   ,concld  ,cldst   , &
       ts      ,sst     ,ps      ,zdu     ,ocnfrac ,&
       rhu00   ,cldice  ,icecldf ,liqcldf ,relhum  ,dindex )
    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: 
    ! Compute cloud fraction 
    ! 
    ! 
    ! Method: 
    ! This calculate cloud fraction using a relative humidity threshold
    ! The threshold depends upon pressure, and upon the presence or absence 
    ! of convection as defined by a reasonably large vertical mass flux 
    ! entering that layer from below.
    ! 
    ! Author: Many. Last modified by Jim McCaa
    ! 
    !-----------------------------------------------------------------------
    use ppgrid   
    use physconst,     only: cappa, gravit, rair, tmelt
    use cldconst
    use wv_saturation, only: aqsat, aqsat_water, polysvp
    use phys_grid,     only: get_rlat_all_p, get_rlon_all_p
    use dycore,        only: dycore_is, get_resolution

   
!RBN - Need this to write shallow,deep fraction to phys buffer.
!PJR - we should probably make seperate modules for determining convective
!      clouds and make this one just responsible for relative humidity clouds
    use phys_buffer,   only: pbuf_size_max, pbuf_fld, pbuf_old_tim_idx, pbuf_get_fld_idx 

    ! Arguments
    integer, intent(in) :: lchnk                  ! chunk identifier
    integer, intent(in) :: ncol                   ! number of atmospheric columns
    integer, intent(in) :: dindex                 ! 0 or 1 to perturb rh
    type(pbuf_fld), intent(inout) :: pbuf(pbuf_size_max)
    real(r8), intent(in) :: pmid(pcols,pver)      ! midpoint pressures
    real(r8), intent(in) :: temp(pcols,pver)      ! temperature
    real(r8), intent(in) :: q(pcols,pver)         ! specific humidity
    real(r8), intent(in) :: omga(pcols,pver)      ! vertical pressure velocity
    real(r8), intent(in) :: cmfmc(pcols,pverp)    ! convective mass flux--m sub c
    real(r8), intent(in) :: cmfmc2(pcols,pverp)   ! shallow convective mass flux--m sub c
    real(r8), intent(in) :: snowh(pcols)          ! snow depth (liquid water equivalent)
    real(r8), intent(in) :: pdel(pcols,pver)      ! pressure depth of layer
    real(r8), intent(in) :: landfrac(pcols)       ! Land fraction
    real(r8), intent(in) :: ocnfrac(pcols)        ! Ocean fraction
    real(r8), intent(in) :: ts(pcols)             ! surface temperature
    real(r8), intent(in) :: sst(pcols)            ! sea surface temperature
    real(r8), intent(in) :: ps(pcols)             ! surface pressure
    real(r8), intent(in) :: zdu(pcols,pver)       ! detrainment rate from deep convection
    real(r8), intent(in) :: phis(pcols)           ! surface geopotential
    real(r8), intent(in) :: shfrc(pcols,pver)     ! cloud fraction from convect_shallow
!zmh
    real(r8), intent(in) :: deepfrc(pcols,pver)     ! cloud fraction from convect_shallow
    real(r8), intent(in) :: cldice(pcols,pver)    ! cloud ice mixing ratio
    logical,  intent(in)  :: use_shfrc

    ! Output arguments
    real(r8), intent(out) :: cloud(pcols,pver)     ! cloud fraction
    real(r8), intent(out) :: rhcloud(pcols,pver)   ! cloud fraction
    real(r8), intent(out) :: clc(pcols)            ! column convective cloud amount
    real(r8), intent(out) :: cldst(pcols,pver)     ! cloud fraction
    real(r8), intent(out) :: rhu00(pcols,pver)     ! RH threshold for cloud
    real(r8), intent(out) :: relhum(pcols,pver)    ! RH 
    real(r8), intent(out) :: icecldf(pcols,pver)   ! ice cloud fraction
    real(r8), intent(out) :: liqcldf(pcols,pver)   ! liquid cloud fraction (combined into cloud)

    !---------------------------Local workspace-----------------------------
    !
    real(r8) concld(pcols,pver)    ! convective cloud cover
    real(r8) cld                   ! intermediate scratch variable (low cld)
    real(r8) dthdpmn(pcols)         ! most stable lapse rate below 750 mb
    real(r8) dthdp                 ! lapse rate (intermediate variable)
    real(r8) es(pcols,pver)        ! saturation vapor pressure
    real(r8) qs(pcols,pver)        ! saturation specific humidity
    real(r8) rhwght                ! weighting function for rhlim transition
    real(r8) rh(pcols,pver)        ! relative humidity
    real(r8) rhdif                 ! intermediate scratch variable
    real(r8) strat                 ! intermediate scratch variable
    real(r8) theta(pcols,pver)     ! potential temperature
    real(r8) rhlim                 ! local rel. humidity threshold estimate
    real(r8) coef1                 ! coefficient to convert mass flux to mb/d
    real(r8) clrsky(pcols)         ! temporary used in random overlap calc
    real(r8) rpdeli(pcols,pver-1) ! 1./(pmid(k+1)-pmid(k))
    real(r8) rhpert                !the specified perturbation to rh

    real(r8), pointer, dimension(:,:) :: deepcu      ! deep convection cloud fraction
    real(r8), pointer, dimension(:,:) :: shallowcu   ! shallow convection cloud fraction

    logical cldbnd(pcols)          ! region below high cloud boundary

    integer i, ierror, k           ! column, level indices
    integer kp1, ifld
    integer kdthdp(pcols)
    integer numkcld                ! number of levels in which to allow clouds

    !  In Cloud Ice Content variables
    real(r8) :: a,b,c,as,bs,cs        !fit parameters
    real(r8) :: Kc                    !constant for ice cloud calc (wood & field)
    real(r8) :: ttmp                  !limited temperature
    real(r8) :: icicval               !empirical iwc value
    real(r8) :: rho                   !local air density
    real(r8) :: esl(pcols,pver)       !liq sat vapor pressure
    real(r8) :: esi(pcols,pver)       !ice sat vapor pressure
    real(r8) :: ncf,phi               !Wilson and Ballard parameters

    real(r8) thetas(pcols)                    ! ocean surface potential temperature
    real(r8) :: clat(pcols)                   ! current latitudes(radians)
    real(r8) :: clon(pcols)                   ! current longitudes(radians)

    ! Statement functions
    logical land
    land(i) = nint(landfrac(i)) == 1

    call get_rlat_all_p(lchnk, ncol, clat)
    call get_rlon_all_p(lchnk, ncol, clon)

    shallowcu => pbuf(sh_frac_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
    deepcu => pbuf(dp_frac_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)

    ! Initialise cloud fraction
    shallowcu = 0._r8
    deepcu    = 0._r8

    !==================================================================================
    ! PHILOSOPHY OF PRESENT IMPLEMENTATION
    !++ag ice3
    ! Modification to philosophy for ice supersaturation
    ! philosophy below is based on RH water only. This is 'liquid condensation'
    ! or liquid cloud (even though it will freeze immediately to ice)
    ! The idea is that the RH limits for condensation are strict only for
    ! water saturation
    !
    ! Ice clouds are formed by explicit parameterization of ice nucleation. 
    ! Closure for ice cloud fraction is done on available cloud ice, such that
    ! the in-cloud ice content matches an empirical fit
    ! thus, icecldf = min(cldice/icicval,1) where icicval = f(temp,cldice,numice)
    ! for a first cut, icicval=f(temp) only.
    ! Combined cloud fraction is maximum overlap  cloud=max(1,max(icecldf,liqcldf))
    ! No dA/dt term for ice?
    !--ag
    !
    ! There are three co-existing cloud types: convective, inversion related low-level
    ! stratocumulus, and layered cloud (based on relative humidity).  Layered and 
    ! stratocumulus clouds do not compete with convective cloud for which one creates 
    ! the most cloud.  They contribute collectively to the total grid-box average cloud 
    ! amount.  This is reflected in the way in which the total cloud amount is evaluated 
    ! (a sum as opposed to a logical "or" operation)
    !
    !==================================================================================
    ! set defaults for rhu00
    rhu00(:,:) = 2.0_r8
    ! define rh perturbation in order to estimate rhdfda
    rhpert = 0.01_r8 

    !set Wang and Sassen IWC paramters
    a=26.87_r8
    b=0.569_r8
    c=0.002892_r8
    !set schiller parameters
    as=-68.4202_r8
    bs=0.983917_r8
    cs=2.81795_r8
    !set wood and field paramters...
    Kc=75._r8

    ! Evaluate potential temperature and relative humidity
    ! If not computing ice cloud fraction then hybrid RH, if MG then water RH
    if ( .not. cldfrc_ice ) then
       call aqsat(temp, pmid, es, qs, pcols, &
                  ncol, pver, 1, pver)
    else
       call aqsat_water(temp, pmid, es, qs, pcols, &
                        ncol, pver, 1, pver)
    endif

    do k=1,pver
       do i=1,ncol

          if (cldfrc_ice) then
             !++ag calculate qsat ice from qsatw
             esl(i,k)=polysvp(temp(i,k),0)
             esi(i,k)=polysvp(temp(i,k),1)
          end if

          theta(i,k)  = temp(i,k)*(pnot/pmid(i,k))**cappa
          rh(i,k)     = q(i,k)/qs(i,k)*(1.0_r8+real(dindex,r8)*rhpert)

          !  record relhum, rh itself will later be modified related with concld
          relhum(i,k)   = rh(i,k)
          cloud(i,k)    = 0._r8
          icecldf(i,k)  = 0._r8
          liqcldf(i,k)  = 0._r8
          rhcloud(i,k)  = 0._r8
          cldst(i,k)    = 0._r8
          concld(i,k)   = 0._r8
       end do
    end do

    ! Initialize other temporary variables
    ierror = 0
    do i=1,ncol
       ! Adjust thetas(i) in the presence of non-zero ocean heights.
       ! This reduces the temperature for positive heights according to a standard lapse rate.
       if(ocnfrac(i).gt.0.01_r8) thetas(i)  = &
            ( sst(i) - lapse * phis(i) / gravit) * (pnot/ps(i))**cappa
       if(ocnfrac(i).gt.0.01_r8.and.sst(i).lt.260._r8) ierror = i
       clc(i) = 0.0_r8
    end do
    coef1 = gravit*864.0_r8    ! conversion to millibars/day

    if (ierror > 0) then
       write(iulog,*) 'COLDSST: encountered in cldfrc:', lchnk,ierror,ocnfrac(ierror),sst(ierror)
    endif

    do k=1,pver-1
       do i=1,ncol
          rpdeli(i,k) = 1._r8/(pmid(i,k+1) - pmid(i,k))
       end do
    end do

    !
    ! Estimate of local convective cloud cover based on convective mass flux
    ! Modify local large-scale relative humidity to account for presence of 
    ! convective cloud when evaluating relative humidity based layered cloud amount
    !
    do k=1,pver
       do i=1,ncol
          concld(i,k) = 0.0_r8
       end do
    end do
    !
    ! cloud mass flux in SI units of kg/m2/s; should produce typical numbers of 20%
    ! shallow and deep convective cloudiness are evaluated separately (since processes
    ! are evaluated separately) and summed
    !   
#ifndef PERGRO



    do k=1,pver-1
       do i=1,ncol
          if ( .not. use_shfrc ) then
             shallowcu(i,k) = max(0.0_r8,min(sh1*log(1.0_r8+sh2*cmfmc2(i,k+1)),0.30_r8))
          else
             shallowcu(i,k) = shfrc(i,k)
          endif
!ZMH  ==================================
          IF  (zmh_cloud_dp)then
             deepcu(i,k)    = deepfrc(i,k) 
          ELSE
             deepcu(i,k) = max(0.0_r8,min(dp1*log(1.0_r8+dp2*abs(cmfmc(i,k+1)-cmfmc2(i,k+1))),0.60_r8))
          ENDIF
! ==================================
          concld(i,k) = min(shallowcu(i,k) + deepcu(i,k),0.80_r8)


          rh(i,k) = (rh(i,k) - concld(i,k))/(1.0_r8 - concld(i,k))

       end do
    end do

#endif
    !==================================================================================
    !
    !          ****** Compute layer cloudiness ******
    !
    !====================================================================
    ! Begin the evaluation of layered cloud amount based on (modified) RH 
    !====================================================================
    !
    numkcld = pver
    do k=2,numkcld
       kp1 = min(k + 1,pver)
       do i=1,ncol

          !++ag   This is now designed to apply FOR LIQUID CLOUDS (condensation > RH water)

          cldbnd(i) = pmid(i,k).ge.pretop

          if ( pmid(i,k).ge.premib ) then
             !==============================================================
             ! This is the low cloud (below premib) block
             !==============================================================
             ! enhance low cloud activation over land with no snow cover
             if (land(i) .and. (snowh(i) <= 0.000001_r8)) then
                rhlim = rhminl - 0.10_r8
!                rhlim = rhminl
             else
                rhlim = rhminl
             endif

             rhdif = (rh(i,k) - rhlim)/(1.0_r8-rhlim)
             rhcloud(i,k) = min(0.999_r8,(max(rhdif,0.0_r8))**2)

             ! SJV: decrease cloud amount if very low water vapor content
             ! (thus very cold): "freeze dry"
             if (cldfrc_freeze_dry) then
                rhcloud(i,k) = rhcloud(i,k)*max(0.15_r8,min(1.0_r8,q(i,k)/0.0030_r8)) 
             endif

          else if ( pmid(i,k).lt.premit ) then
             !==============================================================
             ! This is the high cloud (above premit) block
             !==============================================================
             !
             rhlim = rhminh
             !
             rhdif = (rh(i,k) - rhlim)/(1.0_r8-rhlim)
             rhcloud(i,k) = min(0.999_r8,(max(rhdif,0.0_r8))**2)
          else
             !==============================================================
             ! This is the middle cloud block
             !==============================================================
             !
             !       linear rh threshold transition between thresholds for low & high cloud
             !
             rhwght = (premib-(max(pmid(i,k),premit)))/(premib-premit)
             
             if (land(i) .and. (snowh(i) <= 0.000001_r8)) then
                rhlim = rhminh*rhwght + (rhminl - 0.10_r8)*(1.0_r8-rhwght)
             else
                rhlim = rhminh*rhwght + rhminl*(1.0_r8-rhwght)
             endif
             rhdif = (rh(i,k) - rhlim)/(1.0_r8-rhlim)
             rhcloud(i,k) = min(0.999_r8,(max(rhdif,0.0_r8))**2)
          end if
          !==================================================================================
          ! WE NEED TO DOCUMENT THE PURPOSE OF THIS TYPE OF CODE (ASSOCIATED WITH 2ND CALL)
          !==================================================================================
          !      !
          !      ! save rhlim to rhu00, it handles well by itself for low/high cloud
          !      !
          rhu00(i,k)=rhlim
          !==================================================================================

          if (cldfrc_ice) then

             ! Evaluate ice cloud fraction based on in-cloud ice content

             !--------ICE CLOUD OPTION 1--------Wang & Sassen 2002
             !         Evaluate desired in-cloud water content
             !               icicval = f(temp,cldice,numice)
             !         Start with a function of temperature.
             !         Wang & Sassen 2002 (JAS), based on ARM site MMCR (midlat cirrus)
             !           parameterization valid for 203-253K
             !           icival > 0 for t>195K
             if (iceopt.lt.3) then
                if (iceopt.eq.1) then
                   ttmp=max(195._r8,min(temp(i,k),253._r8)) - 273.16_r8
                   icicval=a + b * ttmp + c * ttmp**2._r8
                   !convert units
                   rho=pmid(i,k)/(rair*temp(i,k))
                   icicval= icicval * 1.e-6_r8 / rho
                else
                   !--------ICE CLOUD OPTION 2--------Schiller 2008 (JGR)
                   !          Use a curve based on FISH measurements in
                   !          tropics, mid-lats and arctic. Curve is for 180-250K (raise to 273K?)
                   !          use median all flights

                   ttmp=max(190._r8,min(temp(i,k),273.16_r8))
                   icicval = 10._r8 **(as * bs**ttmp + cs)
                   !convert units from ppmv to kg/kg
                   icicval= icicval * 1.e-6_r8 * 18._r8 / 28.97_r8
                endif
                !set icecldfraction  for OPTION 1 or OPTION2
                icecldf(i,k) =  max(0._r8,min(cldice(i,k)/icicval,1._r8))

             else if (iceopt.eq.3) then

                !--------ICE CLOUD OPTION 3--------Wood & Field 2000 (JAS)
                ! eq 6: cloud fraction = 1 - exp (-K * qc/qsati)
        
                icecldf(i,k)=1._r8 - exp(-Kc*cldice(i,k)/(qs(i,k)*(esi(i,k)/esl(i,k))))
                icecldf(i,k)=max(0._r8,min(icecldf(i,k),1._r8))
             else
                !--------ICE CLOUD OPTION 4--------Wilson and ballard 1999
                ! inversion of smith....
                !       ncf = cldice / ((1-RHcrit)*qs)
                ! then a function of ncf....
                ncf =cldice(i,k)/((1._r8 - icecrit)*qs(i,k))
                if (ncf.le.0._r8) then
                   icecldf(i,k)=0._r8
                else if (ncf.gt.0._r8 .and. ncf.le.1._r8/6._r8) then
                   icecldf(i,k)=0.5_r8*(6._r8 * ncf)**(2._r8/3._r8)
                else if (ncf.gt.1._r8/6._r8 .and. ncf.lt.1._r8) then
                   phi=(acos(3._r8*(1._r8-ncf)/2._r8**(3._r8/2._r8))+4._r8*3.1415927_r8)/3._r8
                   icecldf(i,k)=(1._r8 - 4._r8 * cos(phi) * cos(phi))
                else
                   icecldf(i,k)=1._r8
                endif
                icecldf(i,k)=max(0._r8,min(icecldf(i,k),1._r8))
             endif
             !TEST: if ice present, icecldf=1.
             !          if (cldice(i,k).ge.1.e-8_r8) then
             !             icecldf(i,k) = 0.99_r8
             !          endif

             !!          if ((cldice(i,k) .gt. icicval) .or. ((cldice(i,k) .gt. 0._r8) .and. (icecldf(i,k) .eq. 0._r8))) then
             !          if (cldice(i,k) .gt. 1.e-8_r8) then
             !             write(iulog,*) 'i,k,pmid,rho,t,cldice,icicval,icecldf,rhcloud: ', &
             !                i,k,pmid(i,k),rho,temp(i,k),cldice(i,k),icicval,icecldf(i,k),rhcloud(i,k)
             !          endif

             !         Combine ice and liquid cloud fraction assuming maximum overlap.
             ! Combined cloud fraction is maximum overlap
             !          cloud(i,k)=min(1._r8,max(icecldf(i,k),rhcloud(i,k)))

             liqcldf(i,k)=(1._r8 - icecldf(i,k))* rhcloud(i,k)
             cloud(i,k)=liqcldf(i,k) + icecldf(i,k)
          else
             ! For RK microphysics
             cloud(i,k) = rhcloud(i,k)
          end if
       end do
    end do 
    !
    ! Add in the marine strat
    ! MARINE STRATUS SHOULD BE A SPECIAL CASE OF LAYERED CLOUD
    ! CLOUD CURRENTLY CONTAINS LAYERED CLOUD DETERMINED BY RH CRITERIA
    ! TAKE THE MAXIMUM OF THE DIAGNOSED LAYERED CLOUD OR STRATOCUMULUS
    !
    !===================================================================================
    !
    !  SOME OBSERVATIONS ABOUT THE FOLLOWING SECTION OF CODE (missed in earlier look)
    !  K700 IS SET AS A CONSTANT BASED ON HYBRID COORDINATE: IT DOES NOT DEPEND ON 
    !  LOCAL PRESSURE; THERE IS NO PRESSURE RAMP => LOOKS LEVEL DEPENDENT AND 
    !  DISCONTINUOUS IN SPACE (I.E., STRATUS WILL END SUDDENLY WITH NO TRANSITION)
    !
    !  IT APPEARS THAT STRAT IS EVALUATED ACCORDING TO KLEIN AND HARTMANN; HOWEVER,
    !  THE ACTUAL STRATUS AMOUNT (CLDST) APPEARS TO DEPEND DIRECTLY ON THE RH BELOW
    !  THE STRONGEST PART OF THE LOW LEVEL INVERSION.  
    !PJR answers: 1) the rh limitation is a physical/mathematical limitation
    !             cant have more cloud than there is RH
    !             allowed the cloud to exist two layers below the inversion
    !             because the numerics frequently make 50% relative humidity
    !             in level below the inversion which would allow no cloud
    !             2) since  the cloud is only allowed over ocean, it should
    !             be very insensitive to surface pressure (except due to 
    !             spectral ringing, which also causes so many other problems
    !             I didnt worry about it.
    !
    !==================================================================================
    if (.not.inversion_cld_off) then
    !
    ! Find most stable level below 750 mb for evaluating stratus regimes
    !
    do i=1,ncol
       ! Nothing triggers unless a stability greater than this minimum threshold is found
       dthdpmn(i) = -0.125_r8
       kdthdp(i) = 0
    end do
    !
    do k=2,pver
       do i=1,ncol
          if (pmid(i,k) >= premib .and. ocnfrac(i).gt. 0.01_r8) then
             ! I think this is done so that dtheta/dp is in units of dg/mb (JJH)
             dthdp = 100.0_r8*(theta(i,k) - theta(i,k-1))*rpdeli(i,k-1)
             if (dthdp < dthdpmn(i)) then
                dthdpmn(i) = dthdp
                kdthdp(i) = k     ! index of interface of max inversion
             end if
          end if
       end do
    end do

    ! Also check between the bottom layer and the surface
    ! Only perform this check if the criteria were not met above

    do i = 1,ncol
       if ( kdthdp(i) .eq. 0 .and. ocnfrac(i).gt.0.01_r8) then
          dthdp = 100.0_r8 * (thetas(i) - theta(i,pver)) / (ps(i)-pmid(i,pver))
          if (dthdp < dthdpmn(i)) then
             dthdpmn(i) = dthdp
             kdthdp(i) = pver     ! index of interface of max inversion
          endif
       endif
    enddo

    do i=1,ncol
       if (kdthdp(i) /= 0) then
          k = kdthdp(i)
          kp1 = min(k+1,pver)
          ! Note: strat will be zero unless ocnfrac > 0.01
          strat = min(1._r8,max(0._r8, ocnfrac(i) * ((theta(i,k700)-thetas(i))*.057_r8-.5573_r8) ) )
          !
          ! assign the stratus to the layer just below max inversion
          ! the relative humidity changes so rapidly across the inversion
          ! that it is not safe to just look immediately below the inversion
          ! so limit the stratus cloud by rh in both layers below the inversion
          !
          cldst(i,k) = min(strat,max(rh(i,k),rh(i,kp1)))
       end if
    end do
    end if  ! .not.inversion_cld_off

    do k=1,pver
       do i=1,ncol
          !
          !       which is greater; standard layered cloud amount or stratocumulus diagnosis
          !
          cloud(i,k) = max(rhcloud(i,k),cldst(i,k))
          !
          !       add in the contributions of convective cloud (determined separately and accounted
          !       for by modifications to the large-scale relative humidity.
          !
          cloud(i,k) = min(cloud(i,k)+concld(i,k), 1.0_r8)
       end do
    end do

    !
    return
  end subroutine cldfrc


  subroutine cldfrc_implicit_PDF(lchnk   ,ncol , pbuf, zm, shflx, lhflx, &
          pblht, zi, dp, lat, lon, & 
       pmid    ,temp    ,pqm1       ,pxlm1    , pxim1, &
       dtime,  rhc_diag, cloud,  cmeliq, cu_pbl_top, subgridon   )    
    !----------------------------------------------------------------------- 
    !   
    ! Purpose: 
    ! Compute cloud fraction based on implicit PDF
    !   
    ! Attribute: 
    ! Subgrid variability is defined for the whole grid box, including
    ! convective regions ; 
    ! Implicit PDF is built in (do not subject to assumed PDF family); 
    ! Representation of stratocumulus and trade cumulus clouds are separated
    ! from that of clouds in the free troposphere;   
    ! In the free troposphere, the scheme uses cloud condensation as predictor,
    ! that amounts to say the scheme only determines how this condensed water is
    ! spatially distributed within the GCM grid.
    !   
    ! Author: Xiaocong Wang, July, 2018
    !   
    !-----------------------------------------------------------------------
    use ppgrid   
    use physconst,     only: cappa, gravit, rair, tmelt, cpair, latvap, latice
!    use wv_saturation, only: qsat, qsat_water, svp_ice  ! cesm1_2_0
    use wv_saturation, only: aqsat, aqsat_water, polysvp, aqsatd
    use phys_grid,     only: get_rlat_all_p, get_rlon_all_p
    use dycore,        only: dycore_is, get_resolution


!RBN - Need this to write shallow,deep fraction to phys buffer.
!PJR - we should probably make seperate modules for determining convective
!      clouds and make this one just responsible for relative humidity clouds

    use phys_buffer,    only: pbuf_size_max, pbuf_fld, pbuf_get_fld_idx


    ! Arguments
    integer, intent(in) :: lchnk                  ! chunk identifier
    integer, intent(in) :: ncol                   ! number of atmospheric columns

    type(pbuf_fld), intent(inout) :: pbuf(pbuf_size_max)

    real(r8), intent(in) :: zm(pcols,pver) 
!zmh
    real(r8), intent(in) :: zi(pcols,pverp) 
    real(r8), intent(in) :: dp(pcols,pver) 
    real(r8), intent(in) :: pblht(pcols)  
    real(r8), intent(in) :: lat(pcols)  
    real(r8), intent(in) :: lon(pcols)  

    real(r8), intent(in) :: shflx(pcols)
    real(r8), intent(in) :: lhflx(pcols)
    real(r8), intent(in) :: pmid(pcols,pver)      ! midpoint pressures
    real(r8), intent(in) :: temp(pcols,pver)      ! temperature
    real(r8), intent(in) :: pqm1(pcols,pver)      ! specific humidity
    real(r8), intent(in) :: pxlm1(pcols,pver)     ! cloud liquid water
    real(r8), intent(in) :: pxim1(pcols,pver)     ! cloud ice

    real(r8), intent(in), optional :: cmeliq(pcols,pver)

    ! Output arguments
    real(r8), intent(out) :: cloud(pcols,pver)     ! cloud fraction
    real(r8), intent(out) :: rhc_diag(pcols,pver)  ! diagnosed RHc
    integer,  intent(out), optional :: cu_pbl_top(pcols)
    logical,  intent(out), optional :: subgridon(pcols,pver)

    !---------------------------Local workspace-----------------------------
    !
    real(r8) es(pcols,pver)        ! saturation vapor pressure
    real(r8) qs(pcols,pver)        ! saturation specific humidity
    real(r8) qsl(pcols,pver)       ! saturation specific humidity at TL
    real(r8) gam(pcols,pver)       ! dqsdt * Lv/Cpd 
    real(r8) dqsdt(pcols,pver)
    real(r8) gaml(pcols,pver)      ! dqsdt * Lv/Cpd 
    real(r8) dqsldt(pcols,pver)


    integer i, k           ! column, level indices

    !  In Cloud Ice Content variables
    real(r8) :: esl(pcols,pver)       !liq sat vapor pressure
    real(r8) :: esi(pcols,pver)       !ice sat vapor pressure

    REAL(r8) par_stdv_tmp, tmp
    REAL(r8) f, g, xr, qn, sat_deficit

!  variable defination for low level clouds, by X.Wang, May 2018
  INTEGER pbl_top(pcols)
  REAL(r8) flux_sl(pcols,pver), flux_qt(pcols,pver), grad_sl(pcols,pver), grad_qt(pcols,pver)
  REAL(r8) budget_sl(pcols,pver), budget_qt(pcols,pver), budget_sl_qt(pcols,pver)
  REAL(r8) tl(pcols,pver), sl(pcols,pver), qt(pcols,pver), tau_diag(pcols,pver), wu_avg(pcols)
  REAL(r8) aaa(pcols,pver)
  REAL(r8) rho_diag(pcols,pver)

  REAL(r8) fluxsl_pbl(pcols,pverp), fluxqt_pbl(pcols,pverp)
  REAL(r8) fluxsl_shconv(pcols,pverp), fluxqt_shconv(pcols,pverp)
  REAL(r8) cnt_shconv(pcols), cnb_shconv(pcols)

  REAL(r8) dtime
  REAL(r8) dum0, dum1,dum2,dum3,deltaz,deltaq,deltaqs,stab

  real(r8), pointer   :: tke_pbl(:,:)                           ! Turbulent kinetic energy [ m2/s2 ]
  real(r8), pointer   :: kvh_pbl(:,:)
  real(r8), pointer   :: lengi_pbl(:,:)
  real(r8), pointer   :: slflxpbl(:,:)
  real(r8), pointer   :: qtflxpbl(:,:)

  real(r8), pointer   :: wu_shconv(:,:)
  real(r8), pointer   :: slflxsh(:,:)
  real(r8), pointer   :: qtflxsh(:,:)

  real(r8), pointer   :: rprddp(:,:)
  real(r8), pointer   :: slflxdp(:,:)
  real(r8), pointer   :: qtflxdp(:,:)

  real(r8), pointer   :: icwmrdp(:,:)     
  real(r8), pointer   :: icwmrsh(:,:)

  integer  :: tke_idx, kvh_idx, lengi_idx, slflxpbl_idx, qtflxpbl_idx,  &
              wush_idx, slflxsh_idx, qtflxsh_idx, rprddp_idx, &
              icwmrdp_idx, icwmrsh_idx, slflxdp_idx, qtflxdp_idx

  kvh_idx = pbuf_get_fld_idx('kvh')
  tke_idx = pbuf_get_fld_idx('tke')
  lengi_idx = pbuf_get_fld_idx('lengi')
  wush_idx = pbuf_get_fld_idx('wush')
  slflxsh_idx = pbuf_get_fld_idx('slflxsh')
  qtflxsh_idx = pbuf_get_fld_idx('qtflxsh')

  slflxdp_idx = pbuf_get_fld_idx('slflxdp')
  qtflxdp_idx = pbuf_get_fld_idx('qtflxdp')

  slflxpbl_idx = pbuf_get_fld_idx('slflxpbl')
  qtflxpbl_idx = pbuf_get_fld_idx('qtflxpbl')

!  rprddp_idx = pbuf_get_fld_idx('RPRDDP')

  icwmrdp_idx = pbuf_get_fld_idx('ICWMRDP')
  icwmrsh_idx = pbuf_get_fld_idx('ICWMRSH')
  

!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  kvh_pbl =>  pbuf(kvh_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  tke_pbl =>  pbuf(tke_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  lengi_pbl =>  pbuf(lengi_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  slflxpbl =>  pbuf(slflxpbl_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  qtflxpbl =>  pbuf(qtflxpbl_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 

  wu_shconv =>  pbuf(wush_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  slflxsh =>  pbuf(slflxsh_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  qtflxsh =>  pbuf(qtflxsh_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 

!  rprddp =>  pbuf(rprddp_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1) 
  slflxdp =>  pbuf(slflxdp_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 
  qtflxdp =>  pbuf(qtflxdp_idx)%fld_ptr(1,1:pcols,1:pverp,lchnk,1) 

  icwmrdp =>  pbuf(icwmrdp_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1) 
  icwmrsh =>  pbuf(icwmrsh_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1) 


    ! Initialize cloud fraction
  if ( present(subgridon) ) subgridon = .false.
    cloud     = 0._r8
    rhc_diag     = rhminl
    pbl_top = pverp -1  !zmh
    cnt_shconv = pver  !pverp !zmh
    cnb_shconv = 1

!zmh
    if(is_first_step() ) then
        wu_shconv = 0._r8
        lengi_pbl = 1._r8
        slflxpbl  = 0._r8
        qtflxpbl  = 0._r8
        tke_pbl    = 0._r8
        kvh_pbl  = 0._r8
    endif


!   find top level of PBL & shconv
DO i=1,ncol
     
  DO k = pver,2,-1
     if(zm(i,k) .gt. pblht(i))then
        pbl_top(i) = k
        exit
     end if
  END DO

!if(i<0)then
!write(*,*)'i=',i
!write(*,*)'pbl_top',pbl_top(i)
!write(*,*)'zm(i,pbl_top(i))',zm(i,pbl_top(i))
!write(*,*)'zi(i,pbl_top(i))',zi(i,pbl_top(i))
!write(*,*)'pblht             ',pblht(i)
!endif

  DO k = 1,pver
     if (slflxsh(i,k).ne.0._r8) then
        cnt_shconv(i) = k
        exit
     end if
  END DO
!zmh


  DO k = pver, 1, -1
     if (slflxsh(i,k).ne.0._r8) then
        cnb_shconv(i) = k
        exit
     end if
  END DO


 DO k = 1,pver
     tl(i,k) = ( cpair * temp(i,k) - latvap * pxlm1(i,k) - latice * pxim1(i,k) )/cpair
     sl(i,k) = ( cpair * temp(i,k) + gravit * zm(i,k) - latvap * pxlm1(i,k) - latice * pxim1(i,k) )/cpair
     qt(i,k)  = pqm1(i,k) + pxlm1(i,k) + pxim1(i,k)
     rho_diag(i,k)  = pmid(i,k)/(rair*temp(i,k)*(1._r8 + 0.608_r8*pqm1(i,k)))
     flux_sl(i,k) = 0.
     flux_qt(i,k) = 0.
     grad_sl(i,k) = 0.
     grad_qt(i,k) = 0.
  END DO
!zmh
 DO k = 2,pver-1
     grad_sl(i,k) = ( sl(i,k+1) - sl(i,k-1) )/( pmid(i,k+1) - pmid(i,k-1) )
     grad_qt(i,k) = ( qt(i,k+1) - qt(i,k-1) )/( pmid(i,k+1) - pmid(i,k-1) )
 ENDDO

    fluxsl_shconv(i,1:pverp) = slflxsh(i,1:pverp)
    fluxqt_shconv(i,1:pverp) = qtflxsh(i,1:pverp)*latvap

  wu_avg(i) = 0._r8
  DO k = 1,pverp
     IF ( (k.ge.int(cnt_shconv(i)) ) .and. (k.le.int(cnb_shconv(i) ))  )  THEN
        wu_avg(i) = wu_avg(i) + ( wu_shconv(i,k) + wu_shconv(i,k+1) )*0.5_r8
!        fluxsl_shconv(i,k) = fluxsl_shconv(i,k) + slflxdp(i,k)
!        fluxqt_shconv(i,k) = fluxqt_shconv(i,k) + qtflxdp(i,k)
     END IF
!zmh to prevent overflow
        wu_avg(i) = max(wu_avg(i),0.1e-3_r8)
  END DO

 IF(cnt_shconv(i).lt.cnb_shconv(i))  wu_avg(i) = wu_avg(i)/(cnb_shconv(i) - cnt_shconv(i) + 1._r8)

!  DO k = 2,pver
!     fluxsl_pbl(i,k) = kvh_pbl(i,k)*(sl(i,k) - sl(i,k-1))/(zm(i,k-1) - zm(i,k))*  &
!                         0.5_r8*(rho_diag(i,k) + rho_diag(i,k-1))
!     fluxqt_pbl(i,k) = kvh_pbl(i,k)*(qt(i,k) - qt(i,k-1))/(zm(i,k-1) - zm(i,k))*  &
!                         0.5_r8*(rho_diag(i,k) + rho_diag(i,k-1))*latvap
!  END DO
!  fluxsl_pbl(i,1) = 0._r8
!  fluxsl_pbl(i,pverp) =  shflx(i)
!  fluxqt_pbl(i,1) = 0._r8
!  fluxqt_pbl(i,pverp) =  lhflx(i)

   fluxsl_pbl(i,1:pverp) = slflxpbl(i,1:pverp)
   fluxqt_pbl(i,1:pverp) = qtflxpbl(i,1:pverp)*latvap

 DO k = 24, pverp
  IF( (k.ge.cnt_shconv(i)) .and. (k.le.cnb_shconv(i)) ) then 
      fluxqt_shconv(i,k) = fluxqt_shconv(i,k) + qtflxdp(i,k)*5._r8 
  ELSE 
      fluxqt_pbl(i,k) = fluxqt_pbl(i,k) + qtflxdp(i,k)*5._r8
  END IF
 END DO

END DO  ! i

! cloud fraction replced by Wang 2017 
!!! scheme begin

    ! Evaluate potential temperature and relative humidity
    ! If not computing ice cloud fraction then hybrid RH, if MG then water RH

!    if ( cldfrc_ice ) then
!       call qsat_water(temp(1:ncol,top_lev:pver), pmid(1:ncol,top_lev:pver), &
!            esl(1:ncol,top_lev:pver), qs(1:ncol,top_lev:pver))

!       esi(1:ncol,top_lev:pver) = svp_ice(temp(1:ncol,top_lev:pver))
!    else
!       call qsat(temp(1:ncol,top_lev:pver), pmid(1:ncol,top_lev:pver), &
!            es(1:ncol,top_lev:pver), qs(1:ncol,top_lev:pver))
!    endif

!    if ( .not. cldfrc_ice ) then
       call aqsat(temp, pmid, es, qs, pcols, &
                  ncol, pver, 1, pver)
!    else
!       call aqsat_water(temp, pmid, es, qs, pcols, &
!                        ncol, pver, 1, pver)
!    endif

       call aqsatd(tl   ,pmid    ,esl , qsl    ,gaml     , &
               pcols   ,ncol    ,pver   ,1  ,pver    )

    dqsldt = gaml * cpair / latvap
!
!zmh initializzation needed for SCM
    par_stdv_tmp = 1.e-5

!#undef skip1
#define SHALLOW
#undef SHALLOW
!#ifdef SHALLOW

if(macrop_scheme .eq. 'ZMH_WXC') goto 411

Do k=1,pver
  DO i=1,ncol
    qn = pxlm1(i,k) + pxim1(i,k)  + icwmrdp(i,k) + icwmrsh(i,k)
    IF(qn.gt.0.) THEN
      IF(pqm1(i,k) + qn.lt.qs(i,k))  THEN
      ! qt < qs, needs iteration
        par_stdv_tmp = qs(i,k)
        sat_deficit = pqm1(i,k) + qn - qs(i,k)
        do
          call f_equation(qn, sat_deficit, par_stdv_tmp, f )
          call g_equation(qn, sat_deficit, par_stdv_tmp, g )
          if(abs(f/qn)<5.0e-3) exit
          xr=par_stdv_tmp-f/g
          par_stdv_tmp = xr
        end do
        cloud(i,k) = exp(1.00557_r8*((pqm1(i,k) + qn - qs(i,k))/par_stdv_tmp + &
                  1.941108_r8) - 3.274606_r8)
      ELSE
! qt >= qs, linear approximation
        par_stdv_tmp = (qn - (pqm1(i,k) + qn - qs(i,k))*0.779_r8)/0.194_r8
        if(par_stdv_tmp.gt.0) then
           cloud(i,k) = 1.065674_r8*(pqm1(i,k) + qn - qs(i,k))/par_stdv_tmp + 0.2664186_r8
        else
           cloud(i,k) = 0.2664186_r8*0.
        end if
           cloud(i,k) = 0.097*((pqm1(i,k) + qn - qs(i,k))/(qn-0.75*(pqm1(i,k) + qn - qs(i,k))) - 0.834) + 0.5197628
      END IF
    ELSE
      cloud(i,k) = 0.0_r8 ! (pqlm1(i,k) + pxim1(i,k)=0)
    END IF

     if(par_stdv_tmp.gt.0) then
        rhc_diag(i,k) = min( max( 1. - sqrt(6._r8)/qs(i,k)*par_stdv_tmp , 0.), 1.)
     else
        rhc_diag(i,k) = rhminh
     end if

    !IF(cloud(i,k).lt.1.e-3) cloud(i,k) = 0._r8
    IF(cloud(i,k).lt.2.e-2) cloud(i,k) = 0._r8
    cloud(i,k) = MAX(MIN(cloud(i,k),1.0_r8),0.0_r8)
  END DO ! i
END DO ! k


!#ENDIF !SHALLOW

411 CONTINUE

!#undef PBL_clouds
#define PBL_clouds
#ifdef PBL_clouds
!  update shallow cumulus and stratocumulus clouds unsing explicit variance of s derived from PBL turbulence & shallow convection

if(macrop_scheme .eq. 'WXC')then
! =============================
DO i=1,ncol
  Do k= pbl_top(i)-1,pver-1

        aaa(i,k) = 1._r8 / ( 1._r8 + dqsldt(i,k) * latvap / cpair )
        !print *, "before tau_diag", k, cnt_shconv(i), cnb_shconv(i), pbl_top(i)


        IF ( (k.ge.int(cnt_shconv(i)) ) .and. (k.le.int(cnb_shconv(i)) ) )  THEN
            flux_sl(i,k) =  (fluxsl_shconv(i,k) + fluxsl_shconv(i,k+1))*0.5_r8  + &
                           (fluxsl_pbl(i,k) + fluxsl_pbl(i,k+1))*0.5_r8
            flux_qt(i,k) =  (fluxqt_shconv(i,k) + fluxqt_shconv(i,k+1))*0.5_r8 + &
                           (fluxqt_pbl(i,k) + fluxqt_pbl(i,k+1))*0.5_r8
            tau_diag(i,k) = ( zm(i,int(cnt_shconv(i)) ) - zm(i,int(cnb_shconv(i)) ) )/wu_avg(i)
            dum1 = tau_diag(i,k)

            if ( present(subgridon) ) subgridon(i,k) = .true.

             !print *, "tau_diag, shconv", tau_diag(i,k), wu_avg(i)

        ELSE IF ( ( k.ge.pbl_top(i) ) .and. (tke_pbl(i,k) + tke_pbl(i,k+1)).ge.1.e-5_r8 ) THEN
             !print *,            ( (sqrt(2._r8*tke_pbl(i,k))  + sqrt(2._r8*tke_pbl(i,k+1)) )*0.5_r8 )  ! * 4._r8 * 10._r8
             !print *, "tau_diag, pbl", tau_diag(i,k), tke_pbl(i,k),tke_pbl(i,k+1),lengi_pbl(i,k),lengi_pbl(i,k+1)

            flux_sl(i,k) = (fluxsl_pbl(i, k) + fluxsl_pbl(i, k+1))*0.5_r8
            flux_qt(i,k) = (fluxqt_pbl(i,k) + fluxqt_pbl(i,k+1))*0.5_r8

            tau_diag(i,k) = (lengi_pbl(i,k)  + lengi_pbl(i,k+1) )*0.5_r8 / &
                         ( (sqrt(2._r8*tke_pbl(i,k))  + sqrt(2._r8*tke_pbl(i,k+1)) )*0.5_r8 )  ! * 4._r8 * 10._r8

            tau_diag(i,k) = max(tau_diag(i,k),dum1)

                     if ( present(subgridon) ) subgridon(i,k) = .true.

       ELSE
            flux_sl(i,k) = 0.
            flux_qt(i,k) = 0.
            tau_diag(i,k) = 0.
            goto 711
       END IF

      IF( (k.ne.1).and.(k.ne.pver) ) THEN

            grad_sl(i,k) = ( sl(i,k+1) - sl(i,k-1) )/( pmid(i,k+1) - pmid(i,k-1) )
            grad_qt(i,k) = ( qt(i,k+1) - qt(i,k-1) )/( pmid(i,k+1) - pmid(i,k-1) )

            budget_sl(i,k) = 2._r8 * grad_sl(i,k) * flux_sl(i,k) /cpair * gravit * tau_diag(i,k) / 2.0_r8 
            budget_qt(i,k) = 2._r8 * grad_qt(i,k) * flux_qt(i,k) /latvap * gravit * tau_diag(i,k) / 2.0_r8
            budget_sl_qt(i,k) = ( flux_sl(i,k) * grad_qt(i,k) /cpair * gravit + flux_qt(i,k) * grad_sl(i,k) /latvap * gravit ) * tau_diag(i,k) / 2.0_r8
            tmp = budget_sl(i,k) * dqsldt(i,k)**2 - 2._r8 * dqsldt(i,k) * budget_sl_qt(i,k) + budget_qt(i,k)

            IF ( tmp .gt. 0)  THEN
               par_stdv_tmp =  aaa(i,k) * sqrt(tmp) !  * sqrt(5._r8)
!             print *, "RHc", 1. - sqrt(6.)/(aaa(i,k)*qsl(i,k))*par_stdv_tmp 
            ELSE
               par_stdv_tmp = ( 1._r8 - rhminl ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)
!            par_stdv_tmp = ( 1._r8 - 0.8 ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)
            END IF
        ELSE
!               par_stdv_tmp = ( 1._r8 - rhminl ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)
!            par_stdv_tmp = ( 1._r8 - 0.8 ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)
        END IF

!            par_stdv_tmp = ( 1._r8 - 0.7 ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)

            if(qt(i,k).gt.qsl(i,k)) then
               par_stdv_tmp = par_stdv_tmp / sqrt(8._r8)
            else
!               par_stdv_tmp = par_stdv_tmp * sqrt(4._r8)
               if(qtflxdp(i,k).le.15._r8) par_stdv_tmp = par_stdv_tmp * sqrt(4._r8)
            end if
        
       rhc_diag(i,k) = min( max( 1. - sqrt(6._r8)/(aaa(i,k)*qsl(i,k))*par_stdv_tmp, 0.), 1.) 


            sat_deficit = aaa(i,k) * (qt(i,k)+dum1-qsl(i,k)-dum2 ) / par_stdv_tmp

!zmh to prevent overflow 
        sat_deficit = min(sat_deficit,2._r8)

            cloud(i,k) =  exp(1.125_r8 * ( sat_deficit + 2.44_r8 ) - 3.72_r8 )

!  end update low clouds

711    IF(cloud(i,k).lt.1.e-3) cloud(i,k) = 0._r8

       cloud(i,k) = MAX(MIN(cloud(i,k),1.0_r8),0.0_r8)
     

  END DO ! k

    if ( present(cu_pbl_top) ) cu_pbl_top(i) = min( pbl_top(i), int(cnt_shconv(i)) )

END DO ! i

ELSE ! ZMH_WXC

!ZMH_WXC =============================================================
!ZMH_WXC =============================================================

DO i=1,ncol
  Do k= pbl_top(i)-1,pver-1   !-1

        aaa(i,k) = 1._r8 / ( 1._r8 + dqsldt(i,k) * latvap / cpair )

        flux_sl(i,k) =  (fluxsl_shconv(i,k) + fluxsl_shconv(i,k+1))*0.5_r8  + &
                           (fluxsl_pbl(i,k) + fluxsl_pbl(i,k+1))*0.5_r8
        flux_qt(i,k) =  (fluxqt_shconv(i,k) + fluxqt_shconv(i,k+1))*0.5_r8 + &
                           (fluxqt_pbl(i,k) + fluxqt_pbl(i,k+1))*0.5_r8
        tau_diag(i,k) = ( zm(i,int(cnt_shconv(i)) ) - zm(i,int(cnb_shconv(i)) ) )/wu_avg(i)
        dum1 = tau_diag(i,k)

        dum2 =  ( (sqrt(2._r8*tke_pbl(i,k))  + sqrt(2._r8*tke_pbl(i,k+1)) )*0.5_r8 )
        tau_diag(i,k) = (lengi_pbl(i,k)  + lengi_pbl(i,k+1) )*0.5_r8 / max(dum2,1.0e-2)

            tau_diag(i,k) = max(tau_diag(i,k),dum1)

            if ( present(subgridon) ) subgridon(i,k) = .true.


            grad_sl(i,k) = ( sl(i,k+1) - sl(i,k-1) )/( pmid(i,k+1) - pmid(i,k-1) )
            grad_qt(i,k) = ( qt(i,k+1) - qt(i,k-1) )/( pmid(i,k+1) - pmid(i,k-1) )

            budget_sl(i,k) = 2._r8 * grad_sl(i,k) * flux_sl(i,k) /cpair * gravit * tau_diag(i,k) / 2.0_r8 
            budget_qt(i,k) = 2._r8 * grad_qt(i,k) * flux_qt(i,k) /latvap * gravit * tau_diag(i,k) / 2.0_r8
            budget_sl_qt(i,k) = ( flux_sl(i,k) * grad_qt(i,k) /cpair * gravit + flux_qt(i,k) * grad_sl(i,k) /latvap * gravit ) * tau_diag(i,k) / 2.0_r8
            tmp = budget_sl(i,k) * dqsldt(i,k)**2 - 2._r8 * dqsldt(i,k) * budget_sl_qt(i,k) + budget_qt(i,k)

            IF ( tmp .gt. 0)  THEN
               par_stdv_tmp =  aaa(i,k) * sqrt(tmp) !  * sqrt(5._r8)
            ELSE
            !   par_stdv_tmp = ( 1._r8 - rhminl ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)
               par_stdv_tmp = ( 1._r8 - 0.8 ) * aaa(i,k) * qsl(i,k) / sqrt(6._r8)
            END IF

            if(qt(i,k).gt.qsl(i,k)) then
               par_stdv_tmp = par_stdv_tmp / sqrt(8._r8)
            else
!               par_stdv_tmp = par_stdv_tmp * sqrt(4._r8)
               if(qtflxdp(i,k).le.15._r8) par_stdv_tmp = par_stdv_tmp * sqrt(4._r8)
            end if

! vertical subgrid scale

            dum1 = 1.0_r8
            dum1 = max(sqrt(cos(lat(i))),0.3_r8 )

            par_stdv_tmp = par_stdv_tmp*dum1

            stab =  1.0_r8+(temp(i,k-1) - temp(i,k+1))/(zm(i,k-1)-zm(i,k+1))/9.8e-3_r8

            stab = sqrt( min( max(stab, 0.0_r8), 1._r8) ) !  = N*sqrt(Cp*T)/g ?

            !dum0 =  stab**3 *max(min(1._r8, shflx(i)/20.),0._r8)

           ! deltaz = (zm(i,k-1)-zi(i,k+1))
           ! if(k .eq. pbl_top(i)-1) then
                !deltaz = zm(i,k)-zi(i,k+1)
                !deltaz = zm(i,k)-zm(i,k+1)
                !deltaz = zi(i,k)-zm(i,k+1)
                !deltaz = zm(i,k-1)-zm(i,k+1)
           !     deltaz = zi(i-1,k)-zm(i,k+1)
           ! endif

            dum2 = min(pqm1(i,k)/2.e-3,1.0_r8) !polar fog

            !dum0 =  stab**2 *max(min(1._r8, shflx(i)/20.),0._r8)  
            dum0 =  stab *max(min(1._r8, shflx(i)/20.),0._r8)  
            deltaz = (zm(i,k-1)-zm(i,k+1))*zmh_vsub_pbl 
            deltaz = deltaz* dum0 

            dum1 = 1.0
            !dum1   = min(stab*pqm1(i,k)/qsl(i,k), 1.0)
            !dum1   = min(sqrt(stab), 1.0) !* pqm1(i,k)/qsl(i,k)
            !dum1   = zmh_ramp(stab, 0.4_r8, 0.4_r8) 

            dum1   = zmh_ramp(stab, 0.3_r8, 0.4_r8)
            dum1 = (dum1*pqm1(i,k+1)/qsl(i,k+1))**2

            deltaq = max(qt(i,k+1) - qt(i,k), 0.0_r8) * dum1 *dum2

            deltaqs= - dqsldt(i,k)*deltaz*9.8e-3_r8 *dum1 * dum2

!            dum1 = (qt(i,k+1) - qt(i,k))*dum0
!            dum2 = - dqsldt(i,k)*min(zm(i,k-1)-zm(i,k+1),300._r8)*9.8e-3_r8 * dum0
!            dum2 = - dqsldt(i,k)*min(zi(i,k-1)-zm(i,k+1),300._r8)*9.8e-3_r8 * dum0

            sat_deficit = aaa(i,k) * (qt(i,k)+deltaq-qsl(i,k)-deltaqs ) / par_stdv_tmp

!zmh to prevent overflow 
            sat_deficit = min(sat_deficit,2._r8)

            cloud(i,k) =  exp(1.125_r8 * ( sat_deficit + 2.44_r8 ) - 3.72_r8 )
!zmh
           if(sat_deficit .lt. -1.8)then  !0.05 cld
              cloud(i,k) = 0.0
           endif


           IF(cloud(i,k).lt.1.e-3) cloud(i,k) = 0._r8


!polar fog

!           IF(abs(lat(i)) .gt. 70.*3.1416/180.) then ! 70 degrees
!               if(zm(i,k) .lt. 150.0_r8)then
!                   dum1 = (abs(lat(i)) - 70.*3.1416/180.)/(20.*3.1416/180.)
!                   dum2 = zm(i,k)/150.0
!                   cloud(i,k) = cloud(i,k)*(1.-0.5*dum1)*dum2
!               endif
!           endif

           cloud(i,k) = MAX(MIN(cloud(i,k),1.0_r8),0.0_r8)
     
           rhc_diag(i,k) = min( max( 1. - sqrt(6._r8)/(aaa(i,k)*qsl(i,k))*par_stdv_tmp, 0.), 1.) 

!write(*,*)'in_cloud k cloud',k,cloud(i,k)

           
  END DO ! k

    if ( present(cu_pbl_top) ) cu_pbl_top(i) = min( pbl_top(i), int(cnt_shconv(i)) )

END DO ! i
!ZMH_WXC =============================================================

ENDIF ! macrop_scheme end
! ======================

#ENDIF


    return
 end subroutine cldfrc_implicit_PDF

  subroutine f_equation(qn, sat_deficit, par_stdv_tmp, f)
    implicit none
    REAL(r8) f, qn, sat_deficit, par_stdv_tmp

    f = par_stdv_tmp*exp(sat_deficit/par_stdv_tmp - 1.7) - qn
  end subroutine f_equation

  subroutine g_equation(qn, sat_deficit, par_stdv_tmp, g)
    implicit none
    REAL(r8) g, qn, sat_deficit, par_stdv_tmp

    g = exp(sat_deficit/par_stdv_tmp - 1.7) +  &
        par_stdv_tmp*exp(sat_deficit/par_stdv_tmp - 1.7)*(-sat_deficit/(par_stdv_tmp**2))
  end subroutine g_equation

!yhy
  subroutine zmh_cldfrc_fice(ncol, t, fice, fsnow)
!
! Compute the fraction of the total cloud water which is in ice phase.
! The fraction depends on temperature only. 
! This is the form that was used for radiation, the code came from cldefr originally
! 
! Author: B. A. Boville Sept 10, 2002
!  modified: PJR 3/13/03 (added fsnow to ascribe snow production for convection )
!-----------------------------------------------------------------------
    use physconst, only: tmelt
    use ppgrid

! Arguments
    integer,  intent(in)  :: ncol                 ! number of active columns
    real(r8), intent(in)  :: t(pcols,pver)        ! temperature

    real(r8), intent(out) :: fice(pcols,pver)     ! Fractional ice content within cloud
    real(r8), intent(out) :: fsnow(pcols,pver)    ! Fractional snow content for convection

! Local variables
    real(r8) :: tmax_fice                         ! max temperature for cloud ice formation
    real(r8) :: tmin_fice                         ! min temperature for cloud ice formation
    real(r8) :: tmax_fsnow                        ! max temperature for transition to convective snow
    real(r8) :: tmin_fsnow                        ! min temperature for transition to convective snow

    integer :: i,k                                ! loop indexes
    integer :: top_lev = 1
!-----------------------------------------------------------------------

    tmax_fice = tmelt     - 5._r8 !        !zmh 10._r8        ! max temperature for cloud ice formation
    tmin_fice = tmax_fice - 15_r8   !zmh 30._r8    ! min temperature for cloud ice formation
    tmax_fsnow = tmelt                ! max temperature for transition to convective snow
    tmin_fsnow = tmelt - 5._r8        ! min temperature for transition to convective snow

    fice(:,:top_lev-1) = 0._r8
    fsnow(:,:top_lev-1) = 0._r8

! Define fractional amount of cloud that is ice
    do k=top_lev,pver
       do i=1,ncol

! If warmer than tmax then water phase
          if (t(i,k) > tmax_fice) then
             fice(i,k) = 0.0_r8

! If colder than tmin then ice phase
          else if (t(i,k) < tmin_fice) then
             fice(i,k) = 1.0_r8

! Otherwise mixed phase, with ice fraction decreasing linearly from tmin to tmax
          else 
             fice(i,k) =(tmax_fice - t(i,k)) / (tmax_fice - tmin_fice)
          end if

! snow fraction partitioning

! If warmer than tmax then water phase
          if (t(i,k) > tmax_fsnow) then
             fsnow(i,k) = 0.0_r8

! If colder than tmin then ice phase
          else if (t(i,k) < tmin_fsnow) then
             fsnow(i,k) = 1.0_r8

! Otherwise mixed phase, with ice fraction decreasing linearly from tmin to tmax
          else 
             fsnow(i,k) =(tmax_fsnow - t(i,k)) / (tmax_fsnow - tmin_fsnow)
          end if

       end do
    end do
  end subroutine zmh_cldfrc_fice

  subroutine cldfrc_fice(ncol, t, fice, fsnow)
    use ppgrid
    integer,  intent(in)  :: ncol                 ! number of active columns
    real(r8), intent(in)  :: t(pcols,pver)        ! temperature

    real(r8), intent(out) :: fice(pcols,pver)     ! Fractional ice content within cloud
    real(r8), intent(out) :: fsnow(pcols,pver)    ! Fractional snow content for convection

     call    zmh_cldfrc_fice(ncol, t, fice, fsnow)

       
  end subroutine cldfrc_fice
end module cloud_fraction
