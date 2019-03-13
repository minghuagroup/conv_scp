!WRF:model_layer:physics
!
!
!
!
module module_bl_gwdo_gsd
use shr_kind_mod,  only: r8 => shr_kind_r8

!public gwdo_gsd

contains
!-------------------------------------------------------------------------------
   subroutine gwdo_gsd(u3d,v3d,t3d,qv3d,p3d,p3di,pi3d,z,                       &
                  rublten,rvblten,rthblten,                                    &
                  dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl,                 &
                  dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd,                 &
                  dusfcg_ls,dvsfcg_ls,dusfcg_bl,dvsfcg_bl,dusfcg_ss,dvsfcg_ss, &
                  dusfcg_fd,dvsfcg_fd,xland,br,                                &
                  var2d,oc12d,oa2d1,oa2d2,oa2d3,oa2d4,ol2d1,ol2d2,ol2d3,ol2d4, &
                  znu,znw,p_top,dz,pblh,                                       &
                  cp,g,rd,rv,ep1,pi,                                           &
					dt,dx,dy,kpbl2d,itimestep,gwd_opt,						   &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte)
!Jinbo Xie add dy, since global model is not dx=dy
!===========================
! Jinbo Xie0
!===========================
	!dt,dx,kpbl2d,itimestep,gwd_opt,
	!&
    !                ids,ide, jds,jde, kds,kde,                                 &
    !                ims,ime, jms,jme, kms,kme,                                 &
    !                its,ite, jts,jte, kts,kte))
!=================================
!   Jinbo Xie0 Modification
!changed the index used in WRF
!to chunk in CAM
!==============================
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!                                                                       
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rublten     u tendency due to pbl parameterization (m/s/s) 
!-- rvblten     v tendency due to pbl parameterization (m/s/s)
!-- rthblten    theta tendency due to pbl parameterization (K/s)
!-- znu         eta values (sigma values)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rd          gas constant for dry air (j/kg/k)
!-- z           height above sea level (m)
!-- rv          gas constant for water vapor (j/kg/k)
!-- dt          time step (s)
!-- dx          model grid interval (m)
!-- dz          height of model layers (m)
!-- xland       land mask (1 for land, 2 for water)
!-- br          bulk richardson number in surface layer
!-- pblh        planetary boundary layer height (m)
!-- ep1         constant for virtual temperature (r_v/r_d - 1) (dimensionless)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!
!-------------------------------------------------------------------------------
  integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                 &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
  integer,  intent(in   )   ::      itimestep,gwd_opt
!

  real(r8),     intent(in   )   ::      dt,cp,g,rd,rv,ep1,pi!dt,dx,cp,g,rd,rv,ep1,pi
!==========================
!Jinbo Xie add dy
        real(r8),     intent(in   )   ::      dy
!variable
        !real(r8),   !dimension(:),  intent(in   )   ::      dx
        real(r8),    intent(in   )   ::      dx(:)
!==========================
!
  real(r8),     dimension( ims:ime, kms:kme )             ,  &
        
            intent(in   )   ::                      qv3d, &
                                                              p3d, &
                                                             pi3d, &
                                                              t3d, &
                                                                z, &
                                                               dz
  real(r8),     dimension( ims:ime, kms:kme )                    , &
     intent(in   )   ::                                 p3di
  real(r8),     dimension( ims:ime, kms:kme )                    , &
           intent(inout)   ::                   rublten, &
                                                          rvblten, &
                                                          rthblten
  real(r8),     dimension( ims:ime, kms:kme ), optional                 , &
            intent(inout)   ::  dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl,   &
                                dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd
!
  real(r8),     dimension( ims:ime, kms:kme)   ::                                    &
                                  dtaux2d_ls,dtauy2d_ls,dtaux2d_bl,dtauy2d_bl, &
                                  dtaux2d_ss,dtauy2d_ss,dtaux2d_fd,dtauy2d_fd

  real(r8),      dimension( ims:ime, kms:kme )                          , &
        
             intent(in   )   ::                        u3d, &
                                                                v3d
!
  integer,   dimension( ims:ime )                                   , &
             intent(in  )   ::             kpbl2d

  real(r8),   dimension( ims:ime )                                      , &
        intent(in  )   ::                                   pblh, &
                                                                 br, &
                                                                 xland

  real(r8),   dimension( ims:ime ), optional                            , &
             intent(inout  )   ::  dusfcg_ls,dvsfcg_ls,dusfcg_bl,dvsfcg_bl,    &
                                   dusfcg_ss,dvsfcg_ss,dusfcg_fd,dvsfcg_fd

  real(r8),   dimension( ims:ime ) ::  dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,        &
                                   dusfc_ss,dvsfc_ss,dusfc_fd,dvsfc_fd
!
  real(r8),   dimension( ims:ime )                                      , &
             intent(in  )   ::                                  var2d, &
                                                                oc12d, &
                                     oa2d1,oa2d2,oa2d3,oa2d4, &
                                     ol2d1,ol2d2,ol2d3,ol2d4
!
  !real(r8),     dimension( kms:kme )                                             , &
            real(r8),   optional                                                         , &
            intent(in  )   ::                                             znu(:), &
                                                                          znw(:)
!
  real(r8),     optional, intent(in  )   ::                           p_top
!
!local
!
  real(r8),   dimension( its:ite, kts:kte )  ::                           delprsi, &
                                                                          pdh
  real(r8),     dimension( its:ite, kts:kte+1 )   ::                         pdhi
  real(r8),   dimension( its:ite, 4 )        ::                               oa4, &
                                                                          ol4
  integer ::  i,j,k,kdt,kpblmax
!
   do k = kts,kte
     if(znu(k).gt.0.6) kpblmax = k + 1
   enddo
!
      do k = kts,kte+1
         do i = its,ite
            if(k.le.kte)pdh(i,k) = p3d(i,k)
             pdhi(i,k) = p3di(i,k)
         enddo
      enddo
!
      do k = kts,kte
        do i = its,ite
          delprsi(i,k) = pdhi(i,k)-pdhi(i,k+1)
        enddo
      enddo


        do i = its,ite
            oa4(i,1) = oa2d1(i)
            oa4(i,2) = oa2d2(i)
            oa4(i,3) = oa2d3(i)
            oa4(i,4) = oa2d4(i)
            ol4(i,1) = ol2d1(i)
            ol4(i,2) = ol2d2(i)
            ol4(i,3) = ol2d3(i)
            ol4(i,4) = ol2d4(i)
        enddo
	!=================================================================
	! Jinbo Xie1  changed all ,j out, turn 3d into 2d, for cam formation
	!=================================================================
      call gwdo2d(dudt=rublten(ims,kms),dvdt=rvblten(ims,kms)              &
             ,dthdt=rthblten(ims,kms)                                       &
              ,dtaux2d_ls=dtaux2d_ls,dtauy2d_ls=dtauy2d_ls                     &
              ,dtaux2d_bl=dtaux2d_bl,dtauy2d_bl=dtauy2d_bl                     &
              ,dtaux2d_ss=dtaux2d_ss,dtauy2d_ss=dtauy2d_ss                     &
              ,dtaux2d_fd=dtaux2d_fd,dtauy2d_fd=dtauy2d_fd                     &
              ,u1=u3d(ims,kms),v1=v3d(ims,kms)                             &
              ,t1=t3d(ims,kms),q1=qv3d(ims,kms)                            &
              ,del=delprsi(its,kts)                                            &
              ,prsi=pdhi(its,kts)                                              &
              ,prsl=pdh(its,kts),prslk=pi3d(ims,kms)                         &
              ,zl=z(ims,kms),rcl=1.0_r8                                         &
              ,xland1=xland(ims),br1=br(ims),hpbl=pblh(ims)              &
              ,dz2=dz(ims,kms)                                               &
              ,kpblmax=kpblmax                                                 &
              ,dusfc_ls=dusfc_ls,dvsfc_ls=dvsfc_ls                             &
              ,dusfc_bl=dusfc_bl,dvsfc_bl=dvsfc_bl                             &
              ,dusfc_ss=dusfc_ss,dvsfc_ss=dvsfc_ss                             &
              ,dusfc_fd=dusfc_fd,dvsfc_fd=dvsfc_fd                             &
              ,var=var2d(ims),oc1=oc12d(ims)                               &
              ,oa4=oa4,ol4=ol4                                                 &
              ,g=g,cp=cp,rd=rd,rv=rv,fv=ep1,pi=pi                              &
              ,dxmeter=dx,dymeter=dy,deltim=dt                                 &
              ,kpbl=kpbl2d(ims),kdt=itimestep,lat=j                          &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )

!Jinbo Xie
!added dymeter in here
!
   end subroutine gwdo_gsd
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine gwdo2d(dudt,dvdt,dthdt,dtaux2d_ls,dtauy2d_ls,                    &
                    dtaux2d_bl,dtauy2d_bl,dtaux2d_ss,dtauy2d_ss,               &
                    dtaux2d_fd,dtauy2d_fd,u1,v1,t1,q1,                         &
                    del,                                                       &
                    prsi,prsl,prslk,zl,rcl,                                    &
                    xland1,br1,hpbl,dz2,                                       &
                    kpblmax,dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,               &
                    dusfc_ss,dvsfc_ss,dusfc_fd,dvsfc_fd,var,oc1,oa4,ol4,       &
                    g,cp,rd,rv,fv,pi,dxmeter,dymeter,deltim,kpbl,kdt,lat,              &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte)
!===============================
! Jinbo Xie add another dymeter
!===============================

!-------------------------------------------------------------------------------
!  
!  this code handles the time tendencies of u v due to the effect of mountain 
!  induced gravity wave drag from sub-grid scale orography. this routine 
!  not only treats the traditional upper-level wave breaking due to mountain 
!  variance (alpert 1988), but also the enhanced lower-tropospheric wave 
!  breaking due to mountain convexity and asymmetry (kim and arakawa 1995). 
!  thus, in addition to the terrain height data in a model grid gox, 
!  additional 10-2d topographic statistics files are needed, including 
!  orographic standard  deviation (var), convexity (oc1), asymmetry (oa4) 
!  and ol (ol4). these data sets are prepared based on the 30 sec usgs orography
!  hong (1999). the current scheme was implmented as in hong et al.(2008)
!
!  coded by song-you hong and young-joon kim and implemented by song-you hong
!
!  program history log:
!    2014-10-01  Hyun-Joo Choi (from KIAPS)  flow-blocking drag of kim and doyle
!                              with blocked height by dividing streamline theory
!    2017-04-06  Joseph Olson (from Gert-Jan Steeneveld) added small-scale
!                    orographic grabity wave drag:
!    2017-09-15  Joseph Olson, with some bug fixes from Michael Toy: added the
!                    topographic form drag of Beljaars et al. (2004, QJRMS)
!           Activation of each component is done by specifying the integer-parameters
!           (defined below) to 0: inactive or 1: active
!                    gsd_gwd_ls = 0 or 1: large-scale
!                    gsd_gwd_bl = 0 or 1: blocking drag 
!                    gsd_gwd_ss = 0 or 1: small-scale gravity wave drag
!                    gsd_gwd_fd = 0 or 1: topographic form drag
!    2017-09-25  Michael Toy (from NCEP GFS model) added dissipation heating
!                    gsd_diss_ht_opt = 0: dissipation heating off
!                    gsd_diss_ht_opt = 1: dissipation heating on
!
!  references:
!        hong et al. (2008), wea. and forecasting
!        kim and doyle (2005), Q. J. R. Meteor. Soc.
!        kim and arakawa (1995), j. atmos. sci.
!        alpet et al. (1988), NWP conference.
!        hong (1999), NCEP office note 424.
!        steeneveld et al (2008), JAMC
!        Tsiringakis et al. (2017), Q. J. R. Meteor. Soc.
!
!  notice : comparible or lower resolution orography files than model resolution
!           are desirable in preprocess (wps) to prevent weakening of the drag
!-------------------------------------------------------------------------------
!
!  input                                                                
!        dudt (ims:ime,kms:kme)  non-lin tendency for u wind component
!        dvdt (ims:ime,kms:kme)  non-lin tendency for v wind component
!        u1(ims:ime,kms:kme) zonal wind / sqrt(rcl)  m/sec  at t0-dt
!        v1(ims:ime,kms:kme) meridional wind / sqrt(rcl) m/sec at t0-dt
!        t1(ims:ime,kms:kme) temperature deg k at t0-dt
!        q1(ims:ime,kms:kme) specific humidity at t0-dt
!
!        rcl     a scaling factor = reciprocal of square of cos(lat)
!                for gmp.  rcl=1 if u1 and v1 are wind components.
!        deltim  time step    secs                                       
!        del(kts:kte)  positive increment of pressure across layer (pa)
!                                                                       
!  output
!        dudt, dvdt    wind tendency due to gwdo
!
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer              ::  kdt,lat,latd,lond,kpblmax,                         &
                            ids,ide, jds,jde, kds,kde,                         &
                            ims,ime, jms,jme, kms,kme,                         &
                            its,ite, jts,jte, kts,kte
!
   real(r8)                 ::  g,rd,rv,fv,cp,pi,deltim,rcl!dxmeter,deltim,rcl
!=======================
!!!!Jinbo Xie, add dymeter
        real(r8)                 :: dymeter
        real(r8),dimension(:)    :: dxmeter
!======================
   real(r8)                ::  dudt(ims:ime,kms:kme),dvdt(ims:ime,kms:kme),          &
                                dthdt(ims:ime,kms:kme),&
                            dtaux2d_ls(ims:ime,kms:kme),dtauy2d_ls(ims:ime,kms:kme), &
                            dtaux2d_bl(ims:ime,kms:kme),dtauy2d_bl(ims:ime,kms:kme), &
                            dtaux2d_ss(ims:ime,kms:kme),dtauy2d_ss(ims:ime,kms:kme), &
                            dtaux2d_fd(ims:ime,kms:kme),dtauy2d_fd(ims:ime,kms:kme), &
                            u1(ims:ime,kms:kme),v1(ims:ime,kms:kme),           & 
                            t1(ims:ime,kms:kme),q1(ims:ime,kms:kme),           &
                            zl(ims:ime,kms:kme),prsl(its:ite,kts:kte),         &
                            prslk(ims:ime,kms:kme)
   real(r8),intent(in)                ::  prsi(its:ite,kts:kte+1),del(its:ite,kts:kte)
   real(r8),intent(in)                 ::  oa4(its:ite,4),ol4(its:ite,4)
!
! GSD surface drag options to regulate specific components
! Each component is tapered off automatically as a function of dx, so best to 
! keep them activated (=1).
integer, parameter ::                                                          &
   gsd_gwd_ls      = 1,       & ! large-scale gravity wave drag
   gsd_gwd_bl      = 1,       & ! blocking drag 
   gsd_gwd_ss      = 0,       & ! small-scale gravity wave drag (Steeneveld et al. 2008)
   gsd_gwd_fd      = 0,       & ! form drag (Beljaars et al. 2004, QJRMS)
   gsd_diss_ht_opt = 0
!
! added for small-scale orographic wave drag
   real(r8), dimension(its:ite,kts:kte)     :: utendwave,vtendwave,thx,thvx,za
   real(r8), dimension(ims:ime), intent(in) :: br1,hpbl,xland1
   real(r8), dimension(its:ite)             :: govrth
   real(r8), dimension(ims:ime,kms:kme), intent(in) :: dz2
   real(r8), dimension(its:ite,kts:kte+1)   :: zq
   real(r8)                 :: tauwavex0,tauwavey0,XNBV,density,tvcon,hpbl2
   integer              :: kpbl2,kvar
   real(r8), parameter      :: varmax = 200.
!
   integer              ::  kpbl(ims:ime)
   real(r8)                 ::  var(ims:ime),oc1(ims:ime),                         &
                            dusfc_ls(ims:ime),dvsfc_ls(ims:ime),               &
                            dusfc_bl(ims:ime),dvsfc_bl(ims:ime),               &
                            dusfc_ss(ims:ime),dvsfc_ss(ims:ime),               &
                            dusfc_fd(ims:ime),dvsfc_fd(ims:ime)
! Variables for scale-awareness:
! Small-scale GWD + turbulent form drag
   real(r8), parameter   :: dxmin_ss = 1000., dxmax_ss = 12000.  ! min,max range of tapering (m)
! Large-scale GWD
   real(r8), parameter   :: dxmin_ls = 3000., dxmax_ls = 13000.  ! min,max range of tapering (m)
!===========================
!!!!!Jinbo Xie
!!!!!Add y axis for taper consider
   real(r8), parameter   :: dymin_ls = 3000., dymax_ls = 13000.  ! min,maxrange of tapering (m)
   real(r8), parameter   :: dymin_ss = 3000., dymax_ss = 13000.  ! min,maxrange of tapering (m)
!==========================
   real(r8)              :: ss_taper, ls_taper  ! small- and large-scale tapering factors (-)


!
! added Beljaars orographic form drag
   real(r8), dimension(its:ite,kts:kte)     :: utendform,vtendform
   real(r8)                 :: a1,a2,wsp
! critical richardson number for wave breaking : ! larger drag with larger value
!
   real(r8),parameter       ::  ric     = 0.25  
!
   real(r8),parameter       ::  dw2min  = 1.
   real(r8),parameter       ::  rimin   = -100.
   real(r8),parameter       ::  bnv2min = 1.0e-5
   real(r8),parameter       ::  efmin   = 0.0
   real(r8),parameter       ::  efmax   = 10.0
   real(r8),parameter       ::  xl      = 4.0e4  
   real(r8),parameter       ::  critac  = 1.0e-5
   real(r8),parameter       ::  gmax    = 1.    
   real(r8),parameter       ::  veleps  = 1.0                                                 
   real(r8),parameter       ::  factop  = 0.5                                                  
   real(r8),parameter       ::  frc     = 1.0      
   real(r8),parameter       ::  ce      = 0.8     
   real(r8),parameter       ::  cg      = 0.5    
   integer,parameter    ::  kpblmin = 2
!
!  local variables
!
   integer              ::  i,k,lcap,lcapp1,nwd,idir,                          &
                            klcap,kp1,ikount,kk
!
   real(r8)                 ::  rcs,rclcs,csg,fdir,cleff,cs,rcsks,                 &
                            wdir,ti,rdz,temp,tem2,dw2,shr2,bvf2,rdelks,        &
                            wtkbj,tem,gfobnv,hd,fro,rim,temc,tem1,efact,       &
                            temv,dtaux,dtauy,eng0,eng1
!
   logical              ::  ldrag(its:ite),icrilv(its:ite),                    &
                            flag(its:ite),kloop1(its:ite)
!                                                                       
   real(r8)                 ::  taub(its:ite),taup(its:ite,kts:kte+1),             &
                            xn(its:ite),yn(its:ite),                           &
                            ubar(its:ite),vbar(its:ite),                       &
                            fr(its:ite),ulow(its:ite),                         &
                            rulow(its:ite),bnv(its:ite),                       &
                            oa(its:ite),ol(its:ite),                           &
                            roll(its:ite),dtfac(its:ite),                      &
                            brvf(its:ite),xlinv(its:ite),                      &
                            delks(its:ite),delks1(its:ite),                    &
                            bnv2(its:ite,kts:kte),usqj(its:ite,kts:kte),       &
                            taud_ls(its:ite,kts:kte),taud_bl(its:ite,kts:kte), &
                            ro(its:ite,kts:kte),                               &
                            vtk(its:ite,kts:kte),vtj(its:ite,kts:kte),         &
                            zlowtop(its:ite),velco(its:ite,kts:kte-1),         &
                            coefm(its:ite)
!
   integer              ::  kbl(its:ite),klowtop(its:ite)
!
   logical :: iope
   integer,parameter    ::  mdir=8
   integer              ::  nwdir(mdir)
   data nwdir/6,7,5,8,2,3,1,4/
!
!  variables for flow-blocking drag
!
   real(r8),parameter       :: frmax  = 10.
   real(r8),parameter       :: olmin  = 1.0e-5
   real(r8),parameter       :: odmin  = 0.1 
   real(r8),parameter       :: odmax  = 10. 
   real(r8),parameter       :: erad   = 6371.315e+3
   integer              :: komax(its:ite)
   integer              :: kblk
   real(r8)                 :: cd
   real(r8)                 :: zblk,tautem
   real(r8)                 :: pe,ke 
                                !================================
   real(r8)                 :: dely,dxy4(its:ite,4),dxy4p(its:ite,4),delx(its:ite)
                                !dxy4(4),dxy4p(4) Jinbo Xie======
   real(r8)                 :: dxy(its:ite),dxyp(its:ite)
   real(r8)                 :: ol4p(4),olp(its:ite),od(its:ite)
   real(r8)                 :: taufb(its:ite,kts:kte+1)
!
!---- constants                                                         
!                                                                       
   rcs    = sqrt(rcl) 
   cs     = 1. / sqrt(rcl)                                                     
   csg    = cs * g                                                      
   lcap   = kte                                                         
   lcapp1 = lcap + 1                                                 
   fdir   = mdir / (2.0*pi)
!
!--- calculate scale-aware tapering factors
!
!=========================================
!!Jinbo Xie  add criteria for dymeter
!Taper for small GWD only, currently assumes equal length in both direction
!Taper matters not much
#if 0
if ( dxmeter .ge. dxmax_ls .and. dymeter .ge. dymax_ls) then
!=========================================
   ls_taper = 1.
else
   if ( dxmeter .le. dxmin_ls) then
      ls_taper = 0.
   else
      ls_taper = 0.5 * ( SIN(pi*(dxmeter-0.5*(dxmax_ls+dxmin_ls))/    &
                                (dxmax_ls-dxmin_ls)) + 1. )
   end if
end if
if ( dxmeter .ge. dxmax_ss ) then
   ss_taper = 1.
else
   if ( dxmeter .le. dxmin_ss) then
      ss_taper = 0.
   else
      ss_taper = dxmax_ss * (1. - dxmin_ss/dxmeter)/(dxmax_ss-dxmin_ss)
   end if
end if
#endif
!Jinbo Xie currently use smoothed topo, taper sets to none-taper
!Jinbo Xie maybe only used when using directly derived data from 30s
ls_taper=1.
ss_taper=1.

!
!--- calculate length of grid for flow-blocking drag
!
   delx   = dxmeter
!============
! Jinbo Xie2
!============
	dely	=dymeter !Jinbo Xie, add dy, since global model dx/=dy
!============
! Jinbo Xie2
!============
!Jinbo Xie 
!varied delx,so everything needs add another dim
   do i=its,ite
   dxy4(i,1)  = delx(i)
   dxy4(i,2)  = dely
   dxy4(i,3)  = sqrt(delx(i)*delx(i) + dely*dely)
   dxy4(i,4)  = dxy4(i,3)
   dxy4p(i,1) = dxy4(i,2)
   dxy4p(i,2) = dxy4(i,1)
   dxy4p(i,3) = dxy4(i,4)
   dxy4p(i,4) = dxy4(i,3)
   end do
!
!
!-----initialize arrays                                                 
!                                                                       
   dtaux = 0.0
   dtauy = 0.0
   do i = its,ite                                                       
     klowtop(i)    = 0
     kbl(i)        = 0
   enddo                                                             
!
   do i = its,ite                                                       
     xn(i)         = 0.0
     yn(i)         = 0.0
     ubar (i)      = 0.0
     vbar (i)      = 0.0
     roll (i)      = 0.0
     taub (i)      = 0.0
     oa(i)         = 0.0
     ol(i)         = 0.0
     ulow (i)      = 0.0
     dtfac(i)      = 1.0
     ldrag(i)      = .false.
     icrilv(i)     = .false. 
     flag(i)       = .true.
   enddo                                                             
!

   do k = kts,kte
     do i = its,ite
       usqj(i,k) = 0.0
       bnv2(i,k) = 0.0
       vtj(i,k)  = 0.0
       vtk(i,k)  = 0.0
       taup(i,k) = 0.0
       taud_ls(i,k) = 0.0
       taud_bl(i,k) = 0.0
       dtaux2d_ls(i,k)= 0.0
       dtauy2d_ls(i,k)= 0.0
       dtaux2d_bl(i,k)= 0.0
       dtauy2d_bl(i,k)= 0.0
       dtaux2d_ss(i,k)= 0.0
       dtauy2d_ss(i,k)= 0.0
       dtaux2d_fd(i,k)= 0.0
       dtauy2d_fd(i,k)= 0.0
     enddo
   enddo
!
   do i = its,ite
     dusfc_ls(i) = 0.0
     dvsfc_ls(i) = 0.0
     dusfc_bl(i) = 0.0
     dvsfc_bl(i) = 0.0
     dusfc_ss(i) = 0.0
     dvsfc_ss(i) = 0.0
     dusfc_fd(i) = 0.0
     dvsfc_fd(i) = 0.0
   enddo
!
   do i = its,ite
     taup(i,kte+1) = 0.0
     xlinv(i)     = 1.0/xl                                                   
   enddo
!
!  initialize array for flow-blocking drag
!
   taufb(its:ite,kts:kte+1) = 0.0
   komax(its:ite) = 0
!
   do k = kts,kte
     do i = its,ite
       vtj(i,k)  = t1(i,k)  * (1.+fv*q1(i,k))
       vtk(i,k)  = vtj(i,k) / prslk(i,k)
       ro(i,k)   = 1./rd * prsl(i,k) / vtj(i,k) ! density kg/m**3
     enddo
   enddo
!
!  determine reference level: maximum of 2*var and pbl heights
!
   do i = its,ite
     zlowtop(i) = 2. * var(i)
   enddo
!
   do i = its,ite
     kloop1(i) = .true.
   enddo
!
   do k = kts+1,kte
     do i = its,ite
        if(kloop1(i).and.zl(i,k)-zl(i,1).ge.zlowtop(i)) then
         klowtop(i) = k+1
         kloop1(i)  = .false.
       endif
     enddo
   enddo
!
   do i = its,ite
     kbl(i)   = max(kpbl(i), klowtop(i))
     kbl(i)   = max(min(kbl(i),kpblmax),kpblmin)
   enddo
!
!  determine the level of maximum orographic height
!
   komax(:) = kbl(:)
!
   do i = its,ite
     delks(i)  = 1.0 / (prsi(i,1) - prsi(i,kbl(i)))
     delks1(i) = 1.0 / (prsl(i,1) - prsl(i,kbl(i)))
   enddo


!
!  compute low level averages within pbl
!
   do k = kts,kpblmax
     do i = its,ite
       if (k.lt.kbl(i)) then
         rcsks   = rcs     * del(i,k) * delks(i)
         rdelks  = del(i,k)  * delks(i)
         ubar(i) = ubar(i) + rcsks  * u1(i,k)      ! pbl u  mean
         vbar(i) = vbar(i) + rcsks  * v1(i,k)      ! pbl v  mean
         roll(i) = roll(i) + rdelks * ro(i,k)      ! ro mean
       endif
     enddo
   enddo
!
!     figure out low-level horizontal wind direction 
!
!             nwd  1   2   3   4   5   6   7   8
!              wd  w   s  sw  nw   e   n  ne  se
!
   do i = its,ite                                                       
     wdir   = atan2(ubar(i),vbar(i)) + pi
     idir   = mod(nint(fdir*wdir),mdir) + 1
     nwd    = nwdir(idir)
     oa(i)  = (1-2*int( (nwd-1)/4 )) * oa4(i,mod(nwd-1,4)+1)
     ol(i) = ol4(i,mod(nwd-1,4)+1) 
!
!----- compute orographic width along (ol) and perpendicular (olp)
!----- the direction of wind
!
     ol4p(1) = ol4(i,2)
     ol4p(2) = ol4(i,1)
     ol4p(3) = ol4(i,4)
     ol4p(4) = ol4(i,3)
     olp(i)  = ol4p(mod(nwd-1,4)+1) 
!
!----- compute orographic direction (horizontal orographic aspect ratio)
!
     od(i) = olp(i)/max(ol(i),olmin)
     od(i) = min(od(i),odmax)
     od(i) = max(od(i),odmin)
!
!----- compute length of grid in the along(dxy) and cross(dxyp) wind directions
!
!==========================================
!Jinbo Xie
     dxy(i)  = dxy4(i,MOD(nwd-1,4)+1)
     dxyp(i) = dxy4p(i,MOD(nwd-1,4)+1)
   enddo
!Jinbo Xie Since variable grid,change dxy4 to larger
!==========================================
!
! END INITIALIZATION; BEGIN GWD CALCULATIONS:
!
IF ( ((gsd_gwd_ls .EQ. 1).or.(gsd_gwd_bl .EQ. 1)).and.   &
               (ls_taper .GT. 1.E-02) ) THEN   !====

!                                                                       
!---  saving richardson number in usqj for migwdi                       
!
   do k = kts,kte-1                                                     
     do i = its,ite                                                     
       ti        = 2.0 / (t1(i,k)+t1(i,k+1))                                
       rdz       = 1./(zl(i,k+1) - zl(i,k))
       tem1      = u1(i,k) - u1(i,k+1)
       tem2      = v1(i,k) - v1(i,k+1)   
       dw2       = rcl*(tem1*tem1 + tem2*tem2)
       shr2      = max(dw2,dw2min) * rdz * rdz
       bvf2      = g*(g/cp+rdz*(vtj(i,k+1)-vtj(i,k))) * ti                
       usqj(i,k) = max(bvf2/shr2,rimin)                            
       bnv2(i,k) = 2.0*g*rdz*(vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
       bnv2(i,k) = max( bnv2(i,k), bnv2min )
     enddo                                                          
   enddo                                                             
!
!----compute the "low level" or 1/3 wind magnitude (m/s)                
!                                                                       
   do i = its,ite                                                       
     ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
     rulow(i) = 1./ulow(i)
   enddo                                                             
!
   do k = kts,kte-1                                                    
     do i = its,ite                                                   
       velco(i,k)  = (0.5*rcs) * ((u1(i,k)+u1(i,k+1)) * ubar(i)                &
                                + (v1(i,k)+v1(i,k+1)) * vbar(i))                 
       velco(i,k)  = velco(i,k) * rulow(i)                               
       if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
         velco(i,k) = veleps                                      
       endif
     enddo                                                          
   enddo                                                             
!                                                                       
!  no drag when critical level in the base layer                        
!                                                                       
   do i = its,ite                                                       
     ldrag(i) = velco(i,1).le.0.                                    
   enddo                                                             
!
!  no drag when velco.lt.0                                               
!                             
                                          
   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. velco(i,k).le.0.
     enddo                                                          
   enddo                                                             
!                                                                       
!  no drag when bnv2.lt.0                                               
!                                                                       
   do k = kts,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. bnv2(i,k).lt.0.
     enddo                                                          
   enddo                                                             

!                                                                       
!-----the low level weighted average ri is stored in usqj(1,1; im)      
!-----the low level weighted average n**2 is stored in bnv2(1,1; im)    
!---- this is called bnvl2 in phys_gwd_alpert_sub not bnv2                           
!---- rdelks (del(k)/delks) vert ave factor so we can * instead of /    
!                                                                       
   do i = its,ite                                                       
     wtkbj     = (prsl(i,1)-prsl(i,2)) * delks1(i)
     bnv2(i,1) = wtkbj * bnv2(i,1)                                
     usqj(i,1) = wtkbj * usqj(i,1)                                
   enddo                                                             
!
   do k = kpblmin,kpblmax                                                
     do i = its,ite                                                    
       if (k .lt. kbl(i)) then
         rdelks    = (prsl(i,k)-prsl(i,k+1)) * delks1(i)
         bnv2(i,1) = bnv2(i,1) + bnv2(i,k) * rdelks
         usqj(i,1) = usqj(i,1) + usqj(i,k) * rdelks
       endif
     enddo                                                          
   enddo                                                             
!                                                                       
   do i = its,ite                                                       
     ldrag(i) = ldrag(i) .or. bnv2(i,1).le.0.0                         
     ldrag(i) = ldrag(i) .or. ulow(i).eq.1.0                           
     ldrag(i) = ldrag(i) .or. var(i) .le. 0.0
   enddo                                                             

!                                                                       
!  set all ri low level values to the low level value          
!                                                                       
   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) usqj(i,k) = usqj(i,1)
     enddo                                                          
   enddo                                                             
!
   do i = its,ite 
     if (.not.ldrag(i))   then   
       bnv(i) = sqrt( bnv2(i,1) )                                  
       fr(i) = bnv(i)  * rulow(i) * 2. * var(i) * od(i)
       fr(i) = min(fr(i),frmax)
       xn(i)  = ubar(i) * rulow(i)
       yn(i)  = vbar(i) * rulow(i)
     endif
   enddo
!
!  compute the base level stress and store it in taub
!  calculate enhancement factor, number of mountains & aspect        
!  ratio const. use simplified relationship between standard            
!  deviation & critical hgt                                          
!
   do i = its,ite                                                       
     if (.not. ldrag(i))   then   
       efact    = (oa(i) + 2.) ** (ce*fr(i)/frc)                         
       efact    = min( max(efact,efmin), efmax )                            
!!!!!!! cleff (effective grid length) is highly tunable parameter
!!!!!!! the bigger (smaller) value produce weaker (stronger) wave drag
       cleff    = sqrt(dxy(i)**2. + dxyp(i)**2.)
!==============Jinbo Xie=============================================
       cleff    = 3. * max(dxmeter(i),cleff)!turned dxmeter to array
!==============Jinbo Xie=============================================
       coefm(i) = (1. + ol(i)) ** (oa(i)+1.)                   
       xlinv(i) = coefm(i) / cleff                                             
       tem      = fr(i) * fr(i) * oc1(i)
       gfobnv   = gmax * tem / ((tem + cg)*bnv(i))   

       if ( gsd_gwd_ls .NE. 0 ) then
          taub(i)  = xlinv(i) * roll(i) * ulow(i) * ulow(i)                       &
                   * ulow(i) * gfobnv * efact          
       else     ! We've gotten what we need for the blocking scheme
          taub(i) = 0.0
       end if
     else                                                          
       taub(i) = 0.0                                             
       xn(i)   = 0.0                                             
       yn(i)   = 0.0                                             
     endif                                                         
   enddo

ENDIF   ! (gsd_gwd_ls .EQ. 1).or.(gsd_gwd_bl .EQ. 1)
!=========================================================
! add small-scale wavedrag for stable boundary layer
!=========================================================
  XNBV=0.
  tauwavex0=0.
  tauwavey0=0.
  density=1.2
  utendwave=0.
  vtendwave=0.
  zq=0.
!
  IF ( (gsd_gwd_ss .EQ. 1).and.(ss_taper.GT.1.E-02) ) THEN
!
! declaring potential temperature
!
    do k = kts,kte
      do i = its,ite
        thx(i,k) = t1(i,k)/prslk(i,k)
      enddo
    enddo
!
    do k = kts,kte
      do i = its,ite
        tvcon = (1.+fv*q1(i,k))
        thvx(i,k) = thx(i,k)*tvcon
      enddo
    enddo
!
! Defining layer height
!
    do k = kts,kte
      do i = its,ite
        zq(i,k+1) = dz2(i,k)+zq(i,k)
      enddo
    enddo
!
    do k = kts,kte
      do i = its,ite
        za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
      enddo
    enddo

    do i=its,ite
       hpbl2 = hpbl(i)+10.
       kpbl2 = kpbl(i)
       !kvar = MIN(kpbl, k-level of var)
       kvar = 1
       do k=kts+1,MAX(kpbl(i),kts+1)
!          IF (za(i,k)>2.*var(i) .or. za(i,k)>2*varmax) then
          IF (za(i,k)>300.) then
             kpbl2 = k
             IF (k == kpbl(i)) then
                hpbl2 = hpbl(i)+10.
             ELSE
                hpbl2 = za(i,k)+10.
             ENDIF
             exit
          ENDIF
       enddo
       !kpbl2 = MAX(MIN(kpbl(i),8),kts+1)
       if((xland1(i)-1.5).le.0. .and. 2.*var(i).le.hpbl(i))then
          if(br1(i).gt.0. .and. thvx(i,kpbl2)-thvx(i,kts) > 0.)then
            cleff    = sqrt(dxy(i)**2 + dxyp(i)**2)
!            cleff    = 3. * max(dxmeter,cleff)
!            cleff    = 10. * max(dxmax_ss,cleff)  _1=10, _2=1, _3=0.5, _4=0.25, _5=0.125
            cleff    = 0.2 * max(dxmax_ss,cleff)
            coefm(i) = (1. + ol(i)) ** (oa(i)+1.)
            xlinv(i) = coefm(i) / cleff
            !govrth(i)=g/(0.5*(thvx(i,kpbl(i))+thvx(i,kts)))
            govrth(i)=g/(0.5*(thvx(i,kpbl2)+thvx(i,kts)))
            !XNBV=sqrt(govrth(i)*(thvx(i,kpbl(i))-thvx(i,kts))/hpbl(i))
            XNBV=sqrt(govrth(i)*(thvx(i,kpbl2)-thvx(i,kts))/hpbl2)
!
            !if(abs(XNBV/u1(i,kpbl(i))).gt.xlinv(i))then
            if(abs(XNBV/u1(i,kpbl2)).gt.xlinv(i))then
              !tauwavex0=0.5*XNBV*xlinv(i)*(2*MIN(var(i),75.))**2*ro(i,kts)*u1(i,kpbl(i))
              !tauwavex0=0.5*XNBV*xlinv(i)*(2.*MIN(var(i),40.))**2*ro(i,kts)*u1(i,kpbl2)
              !tauwavex0=0.5*XNBV*xlinv(i)*(2.*MIN(var(i),40.))**2*ro(i,kts)*u1(i,3)
              tauwavex0=0.5*XNBV*xlinv(i)*(2.*MIN(var(i),varmax))**2*ro(i,kvar)*u1(i,kvar)
              tauwavex0=tauwavex0*ss_taper   ! "Scale-awareness"
            else
              tauwavex0=0.
            endif
!
            !if(abs(XNBV/v1(i,kpbl(i))).gt.xlinv(i))then
            if(abs(XNBV/v1(i,kpbl2)).gt.xlinv(i))then
              !tauwavey0=0.5*XNBV*xlinv(i)*(2*MIN(var(i),75.))**2*ro(i,kts)*v1(i,kpbl(i))
              !tauwavey0=0.5*XNBV*xlinv(i)*(2.*MIN(var(i),40.))**2*ro(i,kts)*v1(i,kpbl2)
              !tauwavey0=0.5*XNBV*xlinv(i)*(2.*MIN(var(i),40.))**2*ro(i,kts)*v1(i,3)
              tauwavey0=0.5*XNBV*xlinv(i)*(2.*MIN(var(i),varmax))**2*ro(i,kvar)*v1(i,kvar)
              tauwavey0=tauwavey0*ss_taper   ! "Scale-awareness"
            else
              tauwavey0=0.
            endif
!
            do k=kts,kpbl(i) !MIN(kpbl2+1,kte-1)
!original
              !utendwave(i,k)=-1.*tauwavex0*2.*max((1.-za(i,k)/hpbl(i)),0.)/hpbl(i)
              !vtendwave(i,k)=-1.*tauwavey0*2.*max((1.-za(i,k)/hpbl(i)),0.)/hpbl(i)
!new
              utendwave(i,k)=-1.*tauwavex0*2.*max((1.-za(i,k)/hpbl2),0.)/hpbl2
              vtendwave(i,k)=-1.*tauwavey0*2.*max((1.-za(i,k)/hpbl2),0.)/hpbl2
!mod-to be used in HRRRv3/RAPv4
              !utendwave(i,k)=-1.*tauwavex0 * max((1.-za(i,k)/hpbl2),0.)**2
              !vtendwave(i,k)=-1.*tauwavey0 * max((1.-za(i,k)/hpbl2),0.)**2
            enddo
          endif
       endif
    enddo ! end i loop

    do k = kts,kte
       do i = its,ite
         dudt(i,k)  = dudt(i,k) + utendwave(i,k)
         dvdt(i,k)  = dvdt(i,k) + vtendwave(i,k)
         dtaux2d_ss(i,k) = utendwave(i,k)
         dtauy2d_ss(i,k) = vtendwave(i,k)
         dusfc_ss(i) = dusfc_ss(i) + utendwave(i,k) * del(i,k)
         dvsfc_ss(i) = dvsfc_ss(i) + vtendwave(i,k) * del(i,k)
       enddo
    enddo

ENDIF  ! end if gsd_gwd_ss == 1
!================================================================
!add Beljaars et al. (2004, QJRMS, equ. 16) form drag:
!================================================================
IF ( (gsd_gwd_fd .EQ. 1).and.(ss_taper.GT.1.E-02) ) THEN

   utendform=0.
   vtendform=0.
   zq=0.

   IF ( (gsd_gwd_ss .NE. 1).and.(ss_taper.GT.1.E-02) ) THEN
      ! Defining layer height. This is already done above is small-scale GWD is used
      do k = kts,kte
        do i = its,ite
          zq(i,k+1) = dz2(i,k)+zq(i,k)
        enddo
      enddo

      do k = kts,kte
        do i = its,ite
          za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
        enddo
      enddo
   ENDIF

   DO i=its,ite
      IF ((xland1(i)-1.5) .le. 0.) then
         !(IH*kflt**n1)**-1 = (0.00102*0.00035**-1.9)**-1 = 0.00026615161
          a1=0.00026615161*var(i)**2
!         a1=0.00026615161*MIN(var(i),varmax)**2
!         a1=0.00026615161*(0.5*var(i))**2
         ! k1**(n1-n2) = 0.003**(-1.9 - -2.8) = 0.003**0.9 = 0.005363
         a2=a1*0.005363
         DO k=kts,kte
            wsp=SQRT(u1(i,k)**2 + v1(i,k)**2)
            ! alpha*beta*Cmd*Ccorr*2.109 = 12.*1.*0.005*0.6*2.109 = 0.0759 
            utendform(i,k)=-0.0759*wsp*u1(i,k)* &
                           EXP(-(za(i,k)/1500.)**1.5)*a2*za(i,k)**(-1.2)*ss_taper
            vtendform(i,k)=-0.0759*wsp*v1(i,k)* &
                           EXP(-(za(i,k)/1500.)**1.5)*a2*za(i,k)**(-1.2)*ss_taper
            !IF(za(i,k) > 4000.) exit
         ENDDO
      ENDIF
   ENDDO

   do k = kts,kte
      do i = its,ite
         dudt(i,k)  = dudt(i,k) + utendform(i,k)
         dvdt(i,k)  = dvdt(i,k) + vtendform(i,k)
         dtaux2d_fd(i,k) = utendform(i,k)
         dtauy2d_fd(i,k) = vtendform(i,k)
         dusfc_fd(i) = dusfc_fd(i) + utendform(i,k) * del(i,k)
         dvsfc_fd(i) = dvsfc_fd(i) + vtendform(i,k) * del(i,k)
      enddo
   enddo

ENDIF  ! end if gsd_gwd_fd == 1
!=======================================================
! More for the large-scale gwd component
IF ( (gsd_gwd_ls .EQ. 1).and.(ls_taper.GT.1.E-02) ) THEN
!                                                                       
!   now compute vertical structure of the stress.
!
   do k = kts,kpblmax
      do i = its,ite
         if (k .le. kbl(i)) taup(i,k) = taub(i)
      enddo
   enddo
!
   do k = kpblmin, kte-1                   ! vertical level k loop!
      kp1 = k + 1
      do i = its,ite
!
!   unstablelayer if ri < ric
!   unstable layer if upper air vel comp along surf vel <=0 (crit lay)
!   at (u-c)=0. crit layer exists and bit vector should be set (.le.)
!
         if (k .ge. kbl(i)) then
           icrilv(i) = icrilv(i) .or. ( usqj(i,k) .lt. ric)                  &
                                 .or. (velco(i,k) .le. 0.0)
           brvf(i)  = max(bnv2(i,k),bnv2min) ! brunt-vaisala frequency squared
           brvf(i)  = sqrt(brvf(i))          ! brunt-vaisala frequency
         endif
      enddo
!
      do i = its,ite
        if (k .ge. kbl(i) .and. (.not. ldrag(i)))   then   
          if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
            temv = 1.0 / velco(i,k)
            tem1 = coefm(i)/dxy(i)*(ro(i,kp1)+ro(i,k))*brvf(i)*velco(i,k)*0.5
            hd   = sqrt(taup(i,k) / tem1)
            fro  = brvf(i) * hd * temv

!
!  rim is the minimum-richardson number by shutts (1985)
!
            tem2   = sqrt(usqj(i,k))
            tem    = 1. + tem2 * fro
            rim    = usqj(i,k) * (1.-fro) / (tem * tem)

!
!  check stability to employ the 'saturation hypothesis'
!  of lindzen (1981) except at tropospheric downstream regions
!
            if (rim .le. ric) then  ! saturation hypothesis!
              if ((oa(i) .le. 0.).or.(kp1 .ge. kpblmin )) then
                temc = 2.0 + 1.0 / tem2
                hd   = velco(i,k) * (2.*sqrt(temc)-temc) / brvf(i)
                taup(i,kp1) = tem1 * hd * hd
              endif
            else                    ! no wavebreaking!
              taup(i,kp1) = taup(i,k)
            endif
          endif
        endif
      enddo      
   enddo
!


   if(lcap.lt.kte) then                                               
      do klcap = lcapp1,kte                                          
         do i = its,ite                                                 
           taup(i,klcap) = prsi(i,klcap) / prsi(i,lcap) * taup(i,lcap)      
         enddo                                                       
      enddo                                                          
   endif      

ENDIF !END LARGE-SCALE TAU CALCULATION

!===============================================================
!COMPUTE BLOCKING COMPONENT 
!===============================================================
IF ( (gsd_gwd_bl .EQ. 1) .and. (ls_taper .GT. 1.E-02) ) THEN
                                                       
   do i = its,ite
      if(.not.ldrag(i)) then
!
!------- determine the height of flow-blocking layer
!
        kblk = 0
        pe = 0.0
        do k = kte, kpblmin, -1
          if(kblk.eq.0 .and. k.le.komax(i)) then
!Jinbo Xie
!flow block appears within the reference level
!Jinbo Xie  
!compare potential energy and kinetic energy
!Jinbo Xie
!divided by g*ro is to turn del(pa) into height
            pe = pe + bnv2(i,k)*(zl(i,komax(i))-zl(i,k))*del(i,k)/g/ro(i,k)
            ke = 0.5*((rcs*u1(i,k))**2.+(rcs*v1(i,k))**2.)
!
!---------- apply flow-blocking drag when pe >= ke 
!
            if(pe.ge.ke) then
              kblk = k
              kblk = min(kblk,kbl(i))
              zblk = zl(i,kblk)-zl(i,kts)
            endif
          endif
        enddo
        if(kblk.ne.0) then
!
!--------- compute flow-blocking stress
!

!Jinbo Xie the max(dxmax_ls,dxy(i))**2
!Jinbo Xie here is a crude estimate since the cam is uneven 0.9*1.25deg
!dxmax_ls is different than the usual one
!because the taper is very different
!Jinbo Xie dxy is a length scale mostly in the direction of the flow to the ridge
!so it is good and not needed for an uneven grid area
!ref Lott and Miller (1997) original scheme
          cd = max(2.0-1.0/od(i),0.0)
          taufb(i,kts) = 0.5 * roll(i) * coefm(i) / max(dxmax_ls,dxy(i))**2 * cd * dxyp(i)   &
                         * olp(i) * zblk * ulow(i)**2
        !changed grid box area into dy*dy
          tautem = taufb(i,kts)/float(kblk-kts)
          do k = kts+1, kblk
            taufb(i,k) = taufb(i,k-1) - tautem
          enddo
!
!----------sum orographic GW stress and flow-blocking stress
!
          ! taup(i,:) = taup(i,:) + taufb(i,:)   ! Keep taup and taufb separate for now
        endif
      endif
   enddo 

ENDIF   ! end blocking drag
!===========================================================
IF ( (gsd_gwd_ls .EQ. 1 .OR. gsd_gwd_bl .EQ. 1) .and. (ls_taper .GT. 1.E-02) ) THEN

!                                                                       
!  calculate - (g)*d(tau)/d(pressure) and deceleration terms dtaux, dtauy
!

   do k = kts,kte                                                       
     do i = its,ite                                                       
       taud_ls(i,k) = 1. * (taup(i,k+1) - taup(i,k)) * csg / del(i,k)
       taud_bl(i,k) = 1. * (taufb(i,k+1) - taufb(i,k)) * csg / del(i,k)
     enddo                                                             
   enddo                                                             

!                                                                       
!  limit de-acceleration (momentum deposition ) at top to 1/2 value 
!  the idea is some stuff must go out the 'top'                     
!                                                                       

   do klcap = lcap,kte                                               
     do i = its,ite                                                    
       taud_ls(i,klcap) = taud_ls(i,klcap) * factop
       taud_bl(i,klcap) = taud_bl(i,klcap) * factop
     enddo                                                          
   enddo                                                             

!                                                                       
!  if the gravity wave drag would force a critical line             
!  in the lower ksmm1 layers during the next deltim timestep,     
!  then only apply drag until that critical line is reached.        
!                                                                       
   do k = kts,kpblmax-1
      do i = its,ite                                                    
         if (k .le. kbl(i)) then
           if((taud_ls(i,k)+taud_bl(i,k)).ne.0.)                         &
              dtfac(i) = min(dtfac(i),abs(velco(i,k)                     &
                   /(deltim*rcs*(taud_ls(i,k)+taud_bl(i,k)))))
         endif
      enddo
   enddo
!
   do k = kts,kte                                                       
      do i = its,ite 
         taud_ls(i,k)  = taud_ls(i,k) * dtfac(i) * ls_taper
         taud_bl(i,k)  = taud_bl(i,k) * dtfac(i) * ls_taper
         ! dtaux = taud(i,k) * xn(i)
         ! dtauy = taud(i,k) * yn(i)
         dtaux2d_ls(i,k) = taud_ls(i,k) * xn(i)
         dtauy2d_ls(i,k) = taud_ls(i,k) * yn(i)
         dtaux2d_bl(i,k) = taud_bl(i,k) * xn(i)
         dtauy2d_bl(i,k) = taud_bl(i,k) * yn(i)
         dudt(i,k)  = dtaux2d_ls(i,k) + dtaux2d_bl(i,k) + dudt(i,k)
         dvdt(i,k)  = dtauy2d_ls(i,k) + dtauy2d_bl(i,k) + dvdt(i,k)
         dusfc_ls(i)  = dusfc_ls(i) + dtaux2d_ls(i,k) * del(i,k)
         dvsfc_ls(i)  = dvsfc_ls(i) + dtauy2d_ls(i,k) * del(i,k)
         dusfc_bl(i)  = dusfc_bl(i) + dtaux2d_bl(i,k) * del(i,k)
         dvsfc_bl(i)  = dvsfc_bl(i) + dtauy2d_bl(i,k) * del(i,k)
         if ( gsd_diss_ht_opt .EQ. 1 ) then
            ! Calculate dissipation heating
            ! Initial kinetic energy (at t0-dt)
            eng0 = 0.5*( (rcs*u1(i,k))**2. + (rcs*v1(i,k))**2. )
            ! Kinetic energy after wave-breaking/flow-blocking
            eng1 = 0.5*( (rcs*(u1(i,k)+(dtaux2d_ls(i,k)+dtaux2d_bl(i,k))*deltim))**2. + &
                         (rcs*(v1(i,k)+(dtauy2d_ls(i,k)+dtauy2d_bl(i,k))*deltim))**2. )
            ! Modify theta tendency
			!Jinbo Xie changed to modify ds/dt in cam and iap
            !dthdt(i,k) = dthdt(i,k) + max((eng0-eng1),0.0)/cp/deltim/prslk(i,k)
         end if
      enddo                                                          
   enddo

ENDIF                                                             


!  Finalize dusfc and dvsfc diagnoses
do i = its,ite
   dusfc_ls(i) = (-1./g*rcs) * dusfc_ls(i)
   dvsfc_ls(i) = (-1./g*rcs) * dvsfc_ls(i)
   dusfc_bl(i) = (-1./g*rcs) * dusfc_bl(i)
   dvsfc_bl(i) = (-1./g*rcs) * dvsfc_bl(i)
   dusfc_ss(i) = (-1./g*rcs) * dusfc_ss(i)
   dvsfc_ss(i) = (-1./g*rcs) * dvsfc_ss(i)
   dusfc_fd(i) = (-1./g*rcs) * dusfc_fd(i)
   dvsfc_fd(i) = (-1./g*rcs) * dvsfc_fd(i)
enddo



!
   return                                                            
   end subroutine gwdo2d
!-------------------------------------------------------------------
end module module_bl_gwdo_gsd
