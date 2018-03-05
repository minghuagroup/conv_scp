module nnparameter
    use netcdf

    implicit none
    private
    save

    public :: feedforward_q1q2, feedforward_prec, feedforward_q1q2prec, readnnparameter
    public :: nnmodel, negqtendadj

    integer,parameter :: r8 = selected_real_kind(12)
    
    ! namelist variables
    integer :: nn_type = 0
    integer :: nn_nlayer = 0
    character(len=512) :: nn_fname = 'nn_fname'
    
    ! To be broadcasted
    integer :: nn_nfeature, nn_nlabel, nn_nlev
    integer, dimension(:), allocatable  :: nn_node
    real(r8), dimension(:), allocatable :: nn_lev      ! hPa
    ! complete variables
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

   namelist /nn_nl/ nn_type, nn_fname, nn_nlayer

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
! linear interpolation from model layers to ERA pressure levels
! feedforward with the NN model
!-----------------------------------------------------------------------------
subroutine feedforward_q1q2(nlevin, landfrac, p, mse, msesat, omega, q1, q2)
    integer, intent(in)   :: nlevin
    real(r8), intent(in)  :: landfrac  
    real(r8), intent(in)  :: p(nlevin)                    ! Pa
    real(r8), intent(in)  :: mse(nlevin), msesat(nlevin)  ! J/kg
    real(r8), intent(in)  :: omega(nlevin)                ! Pa/s
    real(r8), intent(out) :: q1(nlevin), q2(nlevin)       ! J/kg/s, kg/kg/s
    ! local variables
    real(r8) :: cftop, cfbot
    real(r8) :: x0(nn_node(1)), x1(nn_node(2)), x2(nn_node(3)), x3(nn_node(4)), outvar(nn_nlabel)
    integer  :: i,j,k

    x0 = 0.0
    x1 = 0.0
    x2 = 0.0
    x3 = 0.0
    outvar = 0.0

    ! interpolation from model level to ERA pressure level
    do i = 1, nn_nlev, 1
        k = 0
        do j = 1, nlevin, 1
            if ( nn_lev(i)*100.0 > p(j) ) then
                k = j
            end if
        end do
        if (k>0 .and. k<nlevin) then
            cftop = abs(nn_lev(i)*100.0 - p(k+1))/abs(p(k+1) - p(k))
            cfbot = abs(nn_lev(i)*100.0 - p(k))/abs(p(k+1) - p(k))
            x0(i) = cftop*mse(k) + cfbot*mse(k+1)
            x0(i+nn_nlev) = cftop*msesat(k) + cfbot*msesat(k+1)
            x0(i+2*nn_nlev) = cftop*omega(k) + cfbot*omega(k+1)
        end if
        if (k == 0) then
            x0(i) = mse(1)
            x0(i+nn_nlev) = msesat(1)
            x0(i+2*nn_nlev) = omega(1)
        end if
        if (k == nlevin) then
            x0(i) = mse(k)
            x0(i+nn_nlev) = msesat(k)
            x0(i+2*nn_nlev) = omega(k)
        end if
    end do
    
    !write(*, *) "input x: "
    !do i = 1, nn_nlev, 1
    !    write(*, "(F9.0, 2F9.0, F10.6)") nn_lev(i), x0(i), x0(i+nn_nlev), x0(i+nn_nlev*2)
    !end do

    x0 = (x0 - nn_xoffset) / nn_xfactor 

    if (landfrac < 0.5) then
        ! hidden layer 1
        do i = 1, nn_node(2), 1
            x1(i) = max( 0.0, sum(x0(:) * nn_w0_sea(i, :)) + nn_b0_sea(i) )
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
        ! hidden layer 1
        do i = 1, nn_node(2), 1
            x1(i) = max( 0.0, sum(x0(:) * nn_w0_land(i, :)) + nn_b0_land(i) )
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
            q1(i) = cftop*outvar(k) + cfbot*outvar(k+1)
            q2(i) = cftop*outvar(k+nn_nlev) + cfbot*outvar(k+1+nn_nlev)
        end if
        if (k == 0 .or. k==nn_nlev) then
            q1(i) = 0.0
            q2(i) = 0.0
        end if

    end do

end subroutine feedforward_q1q2


!-----------------------------------------------------------------------------
! linear interpolation from model layers to ERA pressure levels
! feedforward with the NN model
!-----------------------------------------------------------------------------
subroutine feedforward_q1q2prec(nlevin, landfrac, p, mse, msesat, omega, q1, q2, prec)
    integer, intent(in)   :: nlevin
    real(r8), intent(in)  :: landfrac  
    real(r8), intent(in)  :: p(nlevin)                    ! Pa
    real(r8), intent(in)  :: mse(nlevin), msesat(nlevin)  ! J/kg
    real(r8), intent(in)  :: omega(nlevin)                ! Pa/s
    real(r8), intent(out) :: q1(nlevin), q2(nlevin)       ! J/kg/s, kg/kg/s
    real(r8), intent(out) :: prec                         ! mm/day
    ! local variables
    real(r8) :: cftop, cfbot
    real(r8) :: x0(nn_node(1)), x1(nn_node(2)), x2(nn_node(3)), x3(nn_node(4)), outvar(nn_nlabel)
    integer  :: i,j,k

    x0 = 0.0
    x1 = 0.0
    x2 = 0.0
    x3 = 0.0
    outvar = 0.0

    ! interpolation from model level to ERA pressure level
    do i = 1, nn_nlev, 1
        k = 0
        do j = 1, nlevin, 1
            if ( nn_lev(i)*100.0 > p(j) ) then
                k = j
            end if
        end do
        if (k>0 .and. k<nlevin) then
            cftop = abs(nn_lev(i)*100.0 - p(k+1))/abs(p(k+1) - p(k))
            cfbot = abs(nn_lev(i)*100.0 - p(k))/abs(p(k+1) - p(k))
            x0(i) = cftop*mse(k) + cfbot*mse(k+1)
            x0(i+nn_nlev) = cftop*msesat(k) + cfbot*msesat(k+1)
            x0(i+2*nn_nlev) = cftop*omega(k) + cfbot*omega(k+1)
        end if
        if (k == 0) then
            x0(i) = mse(1)
            x0(i+nn_nlev) = msesat(1)
            x0(i+2*nn_nlev) = omega(1)
        end if
        if (k == nlevin) then
            x0(i) = mse(k)
            x0(i+nn_nlev) = msesat(k)
            x0(i+2*nn_nlev) = omega(k)
        end if
    end do
    
    !write(*, *) "input x: "
    !do i = 1, nn_nlev, 1
    !    write(*, "(F9.0, 2F9.0, F10.6)") nn_lev(i), x0(i), x0(i+nn_nlev), x0(i+nn_nlev*2)
    !end do

    x0 = (x0 - nn_xoffset) / nn_xfactor 

    if (landfrac < 0.5) then
        ! hidden layer 1
        do i = 1, nn_node(2), 1
            x1(i) = max( 0.0, sum(x0(:) * nn_w0_sea(i, :)) + nn_b0_sea(i) )
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
        ! hidden layer 1
        do i = 1, nn_node(2), 1
            x1(i) = max( 0.0, sum(x0(:) * nn_w0_land(i, :)) + nn_b0_land(i) )
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
            q1(i) = cftop*outvar(k) + cfbot*outvar(k+1)
            q2(i) = cftop*outvar(k+nn_nlev) + cfbot*outvar(k+1+nn_nlev)
        end if
        if (k == 0 .or. k==nn_nlev) then
            q1(i) = 0.0
            q2(i) = 0.0
        end if

    end do

    prec = max(0.0, outvar(2*nn_nlev+1)/86400.0/1000.0 )   ! mm/day -> m/s

end subroutine feedforward_q1q2prec


!-----------------------------------------------------------------------------
! linear interpolation from model layers to ERA pressure levels
! feedforward with the NN model
!-----------------------------------------------------------------------------
subroutine feedforward_prec(nlevin, landfrac, p, mse, msesat, omega, prec)
    integer, intent(in)   :: nlevin
    real(r8), intent(in)  :: landfrac
    real(r8), intent(in)  :: p(nlevin), mse(nlevin), msesat(nlevin), omega(nlevin)
    real(r8), intent(out) :: prec  ! m/s
    ! local variables
    real(r8) :: interpcoef(nn_nlev, nlevin)
    real(r8) :: x0(nn_node(1)), x1(nn_node(2)), x2(nn_node(3)), x3(nn_node(4)), outvar(nn_nlabel)
    integer  :: i,j,k

    outvar = 0.0
    interpcoef = 0.0

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
        x0(i) = sum(interpcoef(i,:) * mse(:))
        x0(i+nn_nlev) = sum(interpcoef(i,:) * msesat(:))
        x0(i+2*nn_nlev) = sum(interpcoef(i,:) * omega(:))
    end do

    x0 = (x0 - nn_xoffset) / nn_xfactor 
    
    if (landfrac < 0.5) then
        ! hidden layer 1
        do i = 1, nn_node(2), 1
            x1(i) = max(0.0, sum(x0(:) * nn_w0_sea(i, :)) + nn_b0_sea(i) )
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
            x1(i) = max( 0.0, sum(x0(:) * nn_w0_land(i, :)) + nn_b0_land(i) )
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

    ! mm/day -> m/s
    prec = max( 0.0, outvar(1) / (86400.0*1000.0) )

end subroutine feedforward_prec

!-----------------------------------------------------------------------------
! linear interpolation from model layers to ERA pressure levels
! feedforward with the NN model
!-----------------------------------------------------------------------------
subroutine nnmodel(nlevin, landfrac, p, t, q, z, omega, &
        stend, qtend, qliqtend, prec, precrate, massflux)
    ! input variables
    integer, intent(in)   :: nlevin
    real(r8), intent(in)  :: landfrac
    real(r8), intent(in)  :: p(nlevin)      ! Pa
    real(r8), intent(in)  :: t(nlevin)      ! K
    real(r8), intent(in)  :: q(nlevin)      ! kg/kg
    real(r8), intent(in)  :: z(nlevin)      ! m
    real(r8), intent(in)  :: omega(nlevin)  ! Pa/s
    ! in/output
    real(r8), intent(inout)  :: stend(nlevin)      ! J/kg/s
    real(r8), intent(inout)  :: qtend(nlevin)      ! kg/kg/s
    real(r8), intent(inout)  :: qliqtend(nlevin)      ! kg/kg/s
    real(r8), intent(inout)  :: precrate(nlevin)      ! m/s
    real(r8), intent(inout)  :: massflux(nlevin)      ! 
    real(r8), intent(inout)  :: prec  ! m/s
    ! local variables
    real(r8) :: qsat(nlevin), mse(nlevin), msesat(nlevin), nn_prec, nn_adjfac
    real(r8) :: interpcoef(nn_nlev, nlevin)
    real(r8) :: invar(nn_node(1)), outvar(nn_nlabel)
    integer  :: i,j,k

    outvar = 0.0
    interpcoef = 0.0

    call cal_qsat1d(t, p, qsat)
    mse    = 1004.0*t + 9.8*z + 2.501e6*q
    msesat = 1004.0*t + 9.8*z + 2.501e6*qsat

    call cal_interpcoef(nlevin, p, interpcoef)

    do i = 1, nn_nlev, 1
        invar(i) = sum(interpcoef(i,:) * mse(:))
        invar(i+nn_nlev) = sum(interpcoef(i,:) * msesat(:))
        invar(i+2*nn_nlev) = sum(interpcoef(i,:) * omega(:))
    end do

    call cal_nnforward(landfrac, invar, outvar)

    ! mm/day -> m/s
    nn_prec = max( 0.0, outvar(1) / (86400.0*1000.0) )
    
    if (nn_type == 1) then
#ifdef SCMDIAG 
        write(*,*) 'nnmodel:    prec = ', prec*86400*1000.0
        write(*,*) 'nnmodel: nn_prec = ', nn_prec*86400*1000.0
#endif
        if (prec*86400.0*1000.0 > 0.1) then
            nn_adjfac = nn_prec / prec
            prec = nn_prec
            stend(:) = stend(:) * nn_adjfac
            qtend(:) = qtend(:) * nn_adjfac
            massflux(:) = massflux(:) * nn_adjfac
            qliqtend(:) = 0.0
            precrate(:) = precrate(:) * nn_adjfac
        end if
    end if

end subroutine nnmodel
    

subroutine negqtendadj(nlevin, q, qtend, dtime, qmin)
    integer,intent(in) :: nlevin
    real(r8),intent(in) :: q(nlevin), dtime, qmin
    real(r8),intent(inout) :: qtend(nlevin)
    integer :: k

    do k=1,nlevin
        if (q(k) + qtend(k)*dtime < qmin) then
            qtend(k) = (qmin-q(k))/dtime
        end if
    end do

end subroutine negqtendadj

!-----------------------------------------------------------------------------
subroutine cal_qsat1d( t, p, qsat)
    real(r8) :: t(:), p(:), qsat(:)
    qsat = 0.6219705862 * 611.2*exp(17.67*(t-273.15)/(t-273.15+243.5)) / p
end subroutine cal_qsat1d

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

subroutine cal_nnforward(landfrac, invar, outvar)
    real(r8), intent(in)  :: landfrac
    real(r8), intent(inout)  :: invar(nn_node(1))
    real(r8), intent(inout) :: outvar(nn_nlabel)
    ! local variables
    real(r8) :: x1(nn_node(2)), x2(nn_node(3)), x3(nn_node(4))
    integer  :: i,j,k

    invar = (invar - nn_xoffset) / nn_xfactor 
    
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

