module nnparameter
    use netcdf

    implicit none
    private
    save

    public :: readnnparameter, nnmodel, negqtendadj, profileadj, cal_weight, nn_type

    integer,parameter :: r8 = selected_real_kind(12)
    
    ! ============== namelist variables ================
    ! nn_type: NN types
    !   0: no NN
    !   [1, 99]: NN( MSE,MSESAT,OMEGA -> prec )
    !   [100, 199]: NN( MSE,MSESAT,OMEGA -> stend,qtend )
    !   [200, 299]: NN( MSE,MSESAT,OMEGA -> stend,qtend,prec )
    !   [1000, 1999]: NN(U,V,T,Q,QSAT,Z,windspeed,MSE,MSESAT,omega -> stend,qtend,prec)
    !   [*1]: use stend to calculate the weights for each plume
    !   [*2]: use qtend to calculate the weights for each plume
    !   [*01, *02]: use normalized profile
    !   [*11, *12]: use original profile
    integer :: nn_type = 0

    ! nn_layer: how many layers in the NN
    integer :: nn_nlayer = 0

    ! nn_fname: netcdf file containing the parameters of NN
    character(len=512) :: nn_fname = 'nn_fname'

    ! nn_ptop, nn_pbot: vertical range for calculating the weights
    real(r8) :: nn_ptop = 20000.0
    real(r8) :: nn_pbot = 95000.0
    !==================================================

    ! To be broadcasted
    integer :: nn_nfeature, nn_nlabel, nn_nlev
    integer, dimension(:), allocatable  :: nn_node
    real(r8), dimension(:), allocatable :: nn_lev      ! hPa
    real(r8), dimension(:), allocatable :: nn_xoffset, nn_yoffset, nn_xfactor, nn_yfactor
    real(r8), dimension(:,:), allocatable :: nn_w0_sea, nn_w0_land
    real(r8), dimension(:,:), allocatable :: nn_w1_sea, nn_w1_land
    real(r8), dimension(:,:), allocatable :: nn_w2_sea, nn_w2_land
    real(r8), dimension(:,:), allocatable :: nn_w3_sea, nn_w3_land
    real(r8), dimension(:), allocatable :: nn_b0_sea, nn_b0_land
    real(r8), dimension(:), allocatable :: nn_b1_sea, nn_b1_land
    real(r8), dimension(:), allocatable :: nn_b2_sea, nn_b2_land
    real(r8), dimension(:), allocatable :: nn_b3_sea, nn_b3_land

contains
   
!-----------------------------------------------------------------------------
! read namelist and initialize the nn model 
!-----------------------------------------------------------------------------
subroutine readnnparameter(nlfile)
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
   integer :: fid, dimids(nf90_max_var_dims), levid, ilayer
   integer :: xoffsetid, xfactorid, yoffsetid, yfactorid
   integer :: wid_sea, wid_land
   integer :: bid_sea, bid_land
   integer :: unitn, ierr, i, j, k
   character(len=*), parameter :: subname = 'nn_readnl'

   namelist /nn_nl/ nn_type, nn_fname, nn_nlayer, nn_ptop, nn_pbot

#if (! defined SCMDIAG)    
   if (masterproc) then
       unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'nn_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, nn_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
#endif


#ifdef SCMDIAG
      open( 10, file=trim(nlfile), status='old' )
      call shr_nl_find_group_name(10, 'nn_nl', status=ierr)
      if (ierr == 0) then
         read(10, nn_nl, iostat=ierr)
         if (ierr /= 0) then
            write(*,*) 'Haiyang: ERROR reading namelist'
         end if
      end if
      close(10)
#endif
   
#if (! defined SCMDIAG)    
    end if
#endif


#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast (nn_fname  , len(nn_fname) , mpichar, 0, mpicom)
   call mpibcast (nn_nlayer , 1, mpiint, 0, mpicom)
   call mpibcast (nn_type , 1, mpiint, 0, mpicom)
   call mpibcast (nn_ptop , 1, mpir8, 0, mpicom)
   call mpibcast (nn_pbot , 1, mpir8, 0, mpicom)
#endif

    allocate(nn_node(nn_nlayer+1))

    write(*, *) "nn_fname: ", trim(nn_fname)
    call netcdf_check( nf90_open(trim(nn_fname), NF90_NOWRITE, fid) )
    
    call netcdf_check( nf90_inq_varid(fid, "lev", levid) )
    call netcdf_check( nf90_inquire_variable(fid, levid, dimids=dimids) )
    call netcdf_check( nf90_inquire_dimension(fid, dimids(1), len = nn_nlev) )
    
    allocate( nn_lev(nn_nlev) )
    call netcdf_check( nf90_get_var(fid, levid, nn_lev, start=(/1/), count=(/nn_nlev/) ) )

    call netcdf_check( nf90_inq_varid(fid, "xoffset", xoffsetid) )
    call netcdf_check( nf90_inq_varid(fid, "xfactor", xfactorid) )
    call netcdf_check( nf90_inq_varid(fid, "yoffset", yoffsetid) )
    call netcdf_check( nf90_inq_varid(fid, "yfactor", yfactorid) )
    call netcdf_check( nf90_inquire_variable(fid, xoffsetid, dimids=dimids) )
    call netcdf_check( nf90_inquire_dimension(fid, dimids(1), len = nn_nfeature) )
    nn_node(1) = nn_nfeature
    call netcdf_check( nf90_inquire_variable(fid, yoffsetid, dimids=dimids) )
    call netcdf_check( nf90_inquire_dimension(fid, dimids(1), len = nn_nlabel) )

    allocate(nn_xoffset(nn_nfeature))
    allocate(nn_xfactor(nn_nfeature))
    allocate(nn_yoffset(nn_nlabel))
    allocate(nn_yfactor(nn_nlabel))
    call netcdf_check( nf90_get_var(fid, xoffsetid, nn_xoffset, start=(/1/), count=(/nn_nfeature/) ) )
    call netcdf_check( nf90_get_var(fid, xfactorid, nn_xfactor, start=(/1/), count=(/nn_nfeature/) ) )
    call netcdf_check( nf90_get_var(fid, yoffsetid, nn_yoffset, start=(/1/), count=(/nn_nlabel/) ) )
    call netcdf_check( nf90_get_var(fid, yfactorid, nn_yfactor, start=(/1/), count=(/nn_nlabel/) ) )
    
    do ilayer = 1, nn_nlayer, 1
        if (ilayer == 1) then
            call netcdf_check( nf90_inq_varid(fid, "w0_sea", wid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "w0_land", wid_land) )
            call netcdf_check( nf90_inq_varid(fid, "b0_sea", bid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "b0_land", bid_land) )
        end if
        if (ilayer == 2) then
            call netcdf_check( nf90_inq_varid(fid, "w1_sea", wid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "w1_land", wid_land) )
            call netcdf_check( nf90_inq_varid(fid, "b1_sea", bid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "b1_land", bid_land) )
        end if
        if (ilayer == 3) then
            call netcdf_check( nf90_inq_varid(fid, "w2_sea", wid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "w2_land", wid_land) )
            call netcdf_check( nf90_inq_varid(fid, "b2_sea", bid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "b2_land", bid_land) )
        end if
        if (ilayer == 4) then
            call netcdf_check( nf90_inq_varid(fid, "w3_sea", wid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "w3_land", wid_land) )
            call netcdf_check( nf90_inq_varid(fid, "b3_sea", bid_sea) )
            call netcdf_check( nf90_inq_varid(fid, "b3_land", bid_land) )
        end if
        call netcdf_check( nf90_inquire_variable(fid, bid_sea, dimids=dimids) )
        call netcdf_check( nf90_inquire_dimension(fid, dimids(1), len = nn_node(ilayer+1) ) )
        
        if (ilayer == 1) then
            allocate(nn_w0_sea(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_w0_land(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_b0_sea(nn_node(ilayer+1)))
            allocate(nn_b0_land(nn_node(ilayer+1)))
            call netcdf_check( nf90_get_var(fid, wid_sea, nn_w0_sea ) )
            call netcdf_check( nf90_get_var(fid, wid_land, nn_w0_land ) )
            call netcdf_check( nf90_get_var(fid, bid_sea, nn_b0_sea ) )
            call netcdf_check( nf90_get_var(fid, bid_land, nn_b0_land ) )
        end if
        if (ilayer == 2) then
            allocate(nn_w1_sea(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_w1_land(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_b1_sea(nn_node(ilayer+1)))
            allocate(nn_b1_land(nn_node(ilayer+1)))
            call netcdf_check( nf90_get_var(fid, wid_sea, nn_w1_sea ) )
            call netcdf_check( nf90_get_var(fid, wid_land, nn_w1_land ) )
            call netcdf_check( nf90_get_var(fid, bid_sea, nn_b1_sea ) )
            call netcdf_check( nf90_get_var(fid, bid_land, nn_b1_land ) )
        end if
        if (ilayer == 3) then
            allocate(nn_w2_sea(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_w2_land(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_b2_sea(nn_node(ilayer+1)))
            allocate(nn_b2_land(nn_node(ilayer+1)))
            call netcdf_check( nf90_get_var(fid, wid_sea, nn_w2_sea ) )
            call netcdf_check( nf90_get_var(fid, wid_land, nn_w2_land ) )
            call netcdf_check( nf90_get_var(fid, bid_sea, nn_b2_sea ) )
            call netcdf_check( nf90_get_var(fid, bid_land, nn_b2_land ) )
        end if
        if (ilayer == 4) then
            allocate(nn_w3_sea(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_w3_land(nn_node(ilayer+1), nn_node(ilayer)))
            allocate(nn_b3_sea(nn_node(ilayer+1)))
            allocate(nn_b3_land(nn_node(ilayer+1)))
            call netcdf_check( nf90_get_var(fid, wid_sea, nn_w3_sea ) )
            call netcdf_check( nf90_get_var(fid, wid_land, nn_w3_land ) )
            call netcdf_check( nf90_get_var(fid, bid_sea, nn_b3_sea ) )
            call netcdf_check( nf90_get_var(fid, bid_land, nn_b3_land ) )
        end if
    
    end do   ! end of loop of ilayer


! #ifdef SCMDIAG
    write(*, *) "nn_type: ", nn_type
    write(*, *) "nn_ptop: ", nn_ptop
    write(*, *) "nn_pbot: ", nn_pbot
    write(*, *) "nn_fname: ", nn_fname
    write(*, *) "nn_nlayer: ", nn_nlayer
    write(*, *) "nn_nlev: ", nn_nlev
    write(*, *) "nn_node", nn_node
    write(*, *) "nn_lev: ", nn_lev
    write(*, *) "nn_nfeature",  nn_nfeature
    write(*, *) "nn_nlabel",  nn_nlabel
    write(*, *) "nn_xoffset", nn_xoffset
    write(*, *) "nn_xfactor", nn_xfactor
    write(*, *) "nn_yoffset", nn_yoffset
    write(*, *) "nn_yfactor", nn_yfactor
! #endif


#ifdef SPMD
    call mpibcast (nn_nlev , 1, mpiint, 0, mpicom)
    call mpibcast (nn_nfeature , 1, mpiint, 0, mpicom)
    call mpibcast (nn_nlabel , 1, mpiint, 0, mpicom)
    call mpibcast (nn_node , nn_nlayer+1, mpiint, 0, mpicom)
    call mpibcast (nn_lev  , nn_nlev, mpir8, 0, mpicom)
    call mpibcast (nn_xoffset , nn_nfeature, mpir8, 0, mpicom)
    call mpibcast (nn_xfactor , nn_nfeature, mpir8, 0, mpicom)
    call mpibcast (nn_yoffset , nn_nlabel, mpir8, 0, mpicom)
    call mpibcast (nn_yfactor , nn_nlabel, mpir8, 0, mpicom)
    call mpibcast (nn_w0_sea   , nn_node(1)*nn_node(2), mpir8, 0, mpicom)
    call mpibcast (nn_w0_land  , nn_node(1)*nn_node(2), mpir8, 0, mpicom)
    call mpibcast (nn_w1_sea   , nn_node(2)*nn_node(3), mpir8, 0, mpicom)
    call mpibcast (nn_w1_land  , nn_node(2)*nn_node(3), mpir8, 0, mpicom)
    call mpibcast (nn_w2_sea   , nn_node(3)*nn_node(4), mpir8, 0, mpicom)
    call mpibcast (nn_w2_land  , nn_node(3)*nn_node(4), mpir8, 0, mpicom)
    call mpibcast (nn_w3_sea   , nn_node(4)*nn_node(5), mpir8, 0, mpicom)
    call mpibcast (nn_w3_land  , nn_node(4)*nn_node(5), mpir8, 0, mpicom)
    
#endif

end subroutine readnnparameter

!-----------------------------------------------------------------------------
! Neural network model for convection parameterization
! 1. Calculate diagnostic variables, e.g. MSE, MSESAT, etc.;
! 2. Linear interpolation from model layers to ERA pressure levels for the NN model input;
! 3. NN feedforward;
! 4. Linear interpolation on the NN output from ERA levels to model layers.
!-----------------------------------------------------------------------------
subroutine nnmodel(nlevin, landfrac, p, u, v, t, q, z, omega, &
        stend, qtend, prec)
    ! input variables
    integer, intent(in)   :: nlevin
    real(r8), intent(in)  :: landfrac       ! [0, 1]
    real(r8), intent(in)  :: p(nlevin)      ! Pa
    real(r8), intent(in)  :: u(nlevin)      ! m/s
    real(r8), intent(in)  :: v(nlevin)      ! m/s
    real(r8), intent(in)  :: t(nlevin)      ! K
    real(r8), intent(in)  :: q(nlevin)      ! kg/kg
    real(r8), intent(in)  :: z(nlevin)      ! m
    real(r8), intent(in)  :: omega(nlevin)  ! Pa/s
    ! in/output
    real(r8), intent(out)  :: stend(nlevin)      ! J/kg/s
    real(r8), intent(out)  :: qtend(nlevin)      ! kg/kg/s
    !real(r8), intent(inout)  :: qliqtend(nlevin)      ! kg/kg/s
    !real(r8), intent(inout)  :: precrate(nlevin)      ! m/s
    !real(r8), intent(inout)  :: massflux(nlevin)      ! 
    real(r8), intent(out)  :: prec  ! m/s
    ! local variables
    real(r8) :: windspeed(nlevin)
    real(r8) :: qsat(nlevin), mse(nlevin), msesat(nlevin)
    real(r8) :: interpcoef(nn_nlev, nlevin)
    real(r8) :: invar(nn_node(1)), outvar(nn_nlabel)
    real(r8) :: cftop, cfbot
    integer  :: i,j,k

    prec = 0.0
    stend = 0.0
    qtend = 0.0

    if (nn_type > 0) then
        outvar = 0.0
        interpcoef = 0.0
        
        windspeed = sqrt(u*u + v*v)

        call cal_qsat1d(t, p, qsat)
        mse    = 1004.0*t + 9.8*z + 2.501e6*q
        msesat = 1004.0*t + 9.8*z + 2.501e6*qsat

        call cal_interpcoef(nlevin, p, interpcoef)

        ! notice the order: mse -> msesat -> omega
        if ( nn_type < 1000) then
            do i = 1, nn_nlev, 1
                invar(i) = sum(interpcoef(i,:) * mse(:))
                invar(i+nn_nlev) = sum(interpcoef(i,:) * msesat(:))
                invar(i+2*nn_nlev) = sum(interpcoef(i,:) * omega(:))
            end do
        end if
        
        ! order: u -> v -> t -> q -> qsat -> z -> windspeed -> mse -> msesat -> omega
        if (nn_type >=1000 .and. nn_type<2000) then
            do i = 1, nn_nlev, 1
                invar(i)            = sum(interpcoef(i,:) * u(:))
                invar(i+nn_nlev)    = sum(interpcoef(i,:) * v(:))
                invar(i+2*nn_nlev)  = sum(interpcoef(i,:) * t(:))
                invar(i+3*nn_nlev)  = sum(interpcoef(i,:) * q(:))
                invar(i+4*nn_nlev)  = sum(interpcoef(i,:) * qsat(:))
                invar(i+5*nn_nlev)  = sum(interpcoef(i,:) * z(:) * 9.8)
                invar(i+6*nn_nlev)  = sum(interpcoef(i,:) * windspeed(:))
                invar(i+7*nn_nlev)  = sum(interpcoef(i,:) * mse(:))
                invar(i+8*nn_nlev)  = sum(interpcoef(i,:) * msesat(:))
                invar(i+9*nn_nlev)  = sum(interpcoef(i,:) * omega(:))
            end do
        end if

        call cal_nnforward(landfrac, invar, outvar)

#ifdef SCMDIAG
    write(*,*) "nnmodel:", outvar
#endif

        ! NN_prec 
        if ( nn_type < 100) then
            ! mm/day -> m/s
            prec = max( 0.0, outvar(nn_nlabel) / (86400.0*1000.0) )
            
#ifdef SCMDIAG 
                write(*,*) 'nnmodel: nn_prec = ', prec*86400*1000.0
#endif
        end if ! NN_prec

        ! NN_q1q2
        if (nn_type >= 100 .and. nn_type<200) then
            ! interpolation from ERA pressure level to model level
            do i = 1, nlevin, 1
                k = 0
                do j = 1, nn_nlev, 1
                    if ( p(i) > nn_lev(j)*100.0 ) then
                        k = j
                    end if
                end do
                if (k>0 .and. k<nn_nlev) then
                    cftop = abs(p(i)/100.0 - nn_lev(k+1)) / abs(nn_lev(k+1)-nn_lev(k))
                    cfbot = abs(p(i)/100.0 - nn_lev(k))   / abs(nn_lev(k+1)-nn_lev(k))
                    stend(i) = cftop*outvar(k) + cfbot*outvar(k+1)
                    qtend(i) = cftop*outvar(k+nn_nlev) + cfbot*outvar(k+1+nn_nlev)
                end if
                if (k == 0 .or. k==nn_nlev) then
                    stend(i) = 0.0
                    qtend(i) = 0.0
                end if
                stend(i) = max(0.0, stend(i))
                qtend(i) = min(0.0, qtend(i))
            end do
        end if  ! NN_q1q2

        ! NN_q1q2prec
        if (nn_type>=200) then
            ! interpolation from ERA pressure level to model level
            do i = 1, nlevin, 1
                k = 0
                do j = 1, nn_nlev, 1
                    if ( p(i) > nn_lev(j)*100.0 ) then
                        k = j
                    end if
                end do
                if (k>0 .and. k<nn_nlev) then
                    cftop = abs(p(i)/100.0 - nn_lev(k+1)) / abs(nn_lev(k+1)-nn_lev(k))
                    cfbot = abs(p(i)/100.0 - nn_lev(k))   / abs(nn_lev(k+1)-nn_lev(k))
                    stend(i) = cftop*outvar(k) + cfbot*outvar(k+1)
                    qtend(i) = cftop*outvar(k+nn_nlev) + cfbot*outvar(k+1+nn_nlev)
                end if
                if (k == 0 .or. k==nn_nlev) then
                    stend(i) = 0.0
                    qtend(i) = 0.0
                end if
                stend(i) = max(0.0, stend(i))
                qtend(i) = min(0.0, qtend(i))
            end do
            prec = max(0.0, outvar(nn_nlabel)/86400.0/1000.0 )   ! mm/day -> m/s
#ifdef SCMDIAG 
                write(*,*) 'nnmodel: nn_prec = ', prec*86400*1000.0
#endif
        end if

    end if  ! if nn_type > 0

end subroutine nnmodel
    
!-----------------------------------------------------------------------------
! Calculate the weights for each plume
!-----------------------------------------------------------------------------
subroutine cal_weight(nlevin, p, dp, nn_stend, stend, nn_qtend, qtend, weight)
    integer, intent(in) :: nlevin
    real(r8), intent(in) :: p(nlevin), dp(nlevin), nn_stend(nlevin), stend(nlevin), nn_qtend(nlevin), qtend(nlevin)
    real(r8), intent(out) :: weight
    real(r8) :: err_stend(nlevin), err_qtend(nlevin)
    real(r8) :: mstend, mqtend, mnnstend, mnnqtend
    real(r8) :: normstend(nlevin), normqtend(nlevin), normnnstend(nlevin), normnnqtend(nlevin)
    integer :: i, rr, rrr

    if (nn_type >= 100) then

        rr = mod(nn_type, 10)
        rrr = mod(nn_type, 100) 
        
        ! first, normalize the profile
        mstend = 0.0
        mqtend = 0.0
        mnnstend = 0.0
        mnnqtend = 0.0
        do i = 1, nlevin, 1
            if (p(i) >= nn_ptop .and. p(i) <=nn_pbot ) then
                mstend = mstend + dp(i)*abs(stend(i))/(nn_pbot - nn_ptop)
                mqtend = mqtend + dp(i)*abs(qtend(i))/(nn_pbot - nn_ptop)
                mnnstend = mnnstend + dp(i)*abs(nn_stend(i))/(nn_pbot - nn_ptop)
                mnnqtend = mnnqtend + dp(i)*abs(nn_qtend(i))/(nn_pbot - nn_ptop)
            end if
        end do
        if (mstend > 1e-12) then
            normstend = stend/mstend
        else
            normstend = stend
        end if

        if (mqtend > 1e-12) then
            normqtend = qtend / mqtend
        else
            normqtend = qtend
        end if

        if (mnnstend > 1e-12) then
            normnnstend = nn_stend/mnnstend
        else
            normnnstend = nn_stend
        end if

        if (mnnqtend > 1e-12) then
            normnnqtend = nn_qtend/mnnqtend
        else
            normnnqtend = nn_qtend
        end if
        
        ! then, calculate the differences between NN profiles and ECP profiles
        if (rrr < 10) then
            err_stend =  normstend - normnnstend
            err_qtend =  normqtend - normnnqtend
        end if
        if (rrr >= 10 .and. rr<20) then
            err_stend =  stend - nn_stend
            err_qtend =  qtend - nn_qtend

        end if

        ! calculate the weight based on the differences
        weight = 0.0
        do i = 1, nlevin, 1
            if (p(i) >= nn_ptop .and. p(i) <= nn_pbot) then
                ! use the error of stend as weight
                ! nn_type = 101, 201, 1001
                if (rr == 1) then 
                    weight = weight + dp(i) * err_stend(i) * err_stend(i)
                end if
                ! use the error of qtend as weight
                ! nn_type = 102, 202, 1002
                if (rr == 2) then 
                    weight = weight + dp(i) * err_qtend(i) * err_qtend(i)
                end if
            end if
        end do

        if (weight < 1.0e-15) then
            weight = 1.0e15
        else
            weight = 1.0/weight
        end if
    else
        weight = 1.0
    end if
end subroutine cal_weight


!-----------------------------------------------------------------------------
! Profile adjustment based on the NN_prec and prec
!-----------------------------------------------------------------------------
subroutine profileadj(nlevin, nn_prec, prec, prof1, prof2, prof3, prof4, prof5)
    integer,intent(in) :: nlevin
    real(r8),intent(in) :: nn_prec
    real(r8),intent(inout) :: prec, prof1(nlevin), prof2(nlevin), prof3(nlevin), prof4(nlevin), prof5(nlevin)
    real(r8) :: adjfac

    adjfac = 1.0
    if (nn_type >= 200 .or. (nn_type<100 .and. nn_type > 0) ) then
        if ( prec > 1e-12 .and. nn_prec > 1e-12) then
            adjfac = nn_prec/prec
            prec = nn_prec
            prof1 = prof1 * adjfac
            prof2 = prof2 * adjfac
            prof3 = prof3 * adjfac
            prof4 = prof4 * adjfac
            prof5 = prof5 * adjfac
        end if
    end if
end subroutine profileadj


!-----------------------------------------------------------------------------
! Negative moisture adjustment
!-----------------------------------------------------------------------------
subroutine negqtendadj(nlevin, q, qtend, stend, precrate, qliqtend, &
    massflux, dtime, qmin)
    integer,intent(in) :: nlevin
    real(r8),intent(in) :: q(nlevin), dtime, qmin
    real(r8),intent(inout) :: qtend(nlevin), stend(nlevin), precrate(nlevin), qliqtend(nlevin), massflux(nlevin)
    integer :: k
    real(r8) :: fac, facmin

    fac = 1.0
    facmin = 1.0
    do k=1,nlevin
        if (q(k) + qtend(k)*dtime < qmin) then
            if ( abs(qtend(k)) > 1.0e-12 ) then
                fac = (qmin - q(k))/dtime/qtend(k)
            end if
            if (fac < facmin) then
                facmin = fac
            end if
        end if
    end do

    qtend = qtend * facmin
    stend = stend * facmin
    qliqtend = qliqtend * facmin
    precrate = precrate * facmin
    massflux = massflux * facmin

end subroutine negqtendadj

!-----------------------------------------------------------------------------
! calculate saturated specific humidity (1D)
!-----------------------------------------------------------------------------
subroutine cal_qsat1d( t, p, qsat)
    real(r8) :: t(:), p(:), qsat(:)
    qsat = 0.6219705862 * 611.2*exp(17.67*(t-273.15)/(t-273.15+243.5)) / p
end subroutine cal_qsat1d


!-----------------------------------------------------------------------------
!   Calculate the coefficient for vertical interpolation from model layer
! to ERA pressure layer
!-----------------------------------------------------------------------------
subroutine cal_interpcoef(nlevin, p, interpcoef)
    integer, intent(in)  :: nlevin
    real(r8), intent(in)  :: p(nlevin)
    real(r8), intent(out) :: interpcoef(nn_nlev, nlevin)
    integer :: i, j, k

    do i = 1, nn_nlev, 1
        k = 0
        do j = 1, nlevin, 1
            if ( nn_lev(i)*100.0 > p(j) ) then
                k = j
            end if
        end do
        if (k>0 .and. k<nlevin) then
            interpcoef(i, k) = abs(nn_lev(i)*100.0 - p(k+1))/abs(p(k+1) - p(k))
            interpcoef(i,k+1)= abs(nn_lev(i)*100.0 - p(k))/abs(p(k+1) - p(k))
        end if
        if (k == nlevin) then
            interpcoef(i, k) = 1.0
        end if
    end do
end subroutine cal_interpcoef


!-----------------------------------------------------------------------------
! Neural network feedforward 
!-----------------------------------------------------------------------------
subroutine cal_nnforward(landfrac, invar, outvar)
    real(r8), intent(in)  :: landfrac
    real(r8), intent(inout)  :: invar(nn_node(1))
    real(r8), intent(inout) :: outvar(nn_nlabel)
    ! local variables
    real(r8) :: x1(nn_node(2)), x2(nn_node(3)), x3(nn_node(4))
    integer  :: i,j,k

    invar = (invar - nn_xoffset) / nn_xfactor 
    outvar = 0.0

    if (landfrac < 0.5) then
        ! hidden layer 1
        do i = 1, nn_node(2), 1
            x1(i) = max(0.0, sum(invar(:) * nn_w0_sea(i, :)) + nn_b0_sea(i) )
        end do
        ! hidden layer 2
        do i = 1, nn_node(3), 1
            x2(i) = max( 0.0, sum(x1(:) * nn_w1_sea(i, :)) + nn_b1_sea(i) )
        end do
        ! hidden layer 3
        do i = 1, nn_node(4), 1
            x3(i) = max( 0.0, sum(x2(:) * nn_w2_sea(i, :)) + nn_b2_sea(i) )
        end do
        ! output layer
        do i = 1, nn_node(5), 1
            outvar(i) = sum(x3(:) * nn_w3_sea(i, :)) + nn_b3_sea(i)
        end do
    else
        ! hidden layer
        do i = 1, nn_node(2), 1
            x1(i) = max( 0.0, sum(invar(:) * nn_w0_land(i, :)) + nn_b0_land(i) )
        end do
        ! hidden layer 2
        do i = 1, nn_node(3), 1
            x2(i) = max( 0.0, sum(x1(:) * nn_w1_land(i, :)) + nn_b1_land(i) )
        end do
        ! hidden layer 3
        do i = 1, nn_node(4), 1
            x3(i) = max( 0.0, sum(x2(:) * nn_w2_land(i, :)) + nn_b2_land(i) )
        end do
        ! output layer
        do i = 1, nn_node(5), 1
            outvar(i) = sum(x3(:) * nn_w3_land(i, :)) + nn_b3_land(i)
        end do
    end if

    outvar = outvar * nn_yfactor + nn_yoffset

    do i = 1, nn_nlabel, 1
        if (isnan(outvar(i))) then
            outvar(i) = 0.0
        end if
    end do

end subroutine cal_nnforward

!-----------------------------------------------------------------------------
! netcdf file check
!-----------------------------------------------------------------------------
subroutine netcdf_check( status )
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then 
        print *, trim(nf90_strerror(status))
        stop "Stopped"
    end if
end subroutine netcdf_check


end module nnparameter

