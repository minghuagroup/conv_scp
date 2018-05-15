program test_lapack


implicit none

    integer,parameter :: r8 = selected_real_kind(12)
    integer,parameter :: nlev = 7, nplume = 3
    real(r8) :: pcam(nlev, nplume)
    real(r8) :: pnn(nlev), cov(nplume, nplume), cross(nplume, 1)
    real(r8) :: w(nplume), work(nplume*3-1), eiginv(nplume, nplume)
    integer :: i,j,k,info

    do i = 1, nlev, 1
        pcam(i,1) = 1.0*i
        pcam(i,2) = 2.0*i
        pcam(i,3) = cos(1.0*i)
    end do
    
    pnn = -2.0*pcam(:,1) + 1.0*pcam(:,2) + 3.0*pcam(:,3)

    do i = 1, nplume, 1
        cross(i, 1) = sum(pcam(:,i) * pnn)
        do j = 1, nplume, 1
            cov(i,j) = sum(pcam(:,i)*pcam(:,j))
        end do
    end do

    do i = 1, nlev, 1
        write(*, '(4F10.2)') pcam(i, :), pnn(i)
    end do
    write(*,*) "cov and cross:"
    do i = 1, nplume, 1
        write(*,'(4F10.2)') cov(i,:), cross(i, 1)
    end do
    
    call DSYEV("V", "U", nplume, cov, nplume, w, work, nplume*3-1, info)
    write(*,*) 'eigen value and vectors:', info
    do i = 1, nplume, 1
        write(*,'(4F10.2)') cov(i,:), w(i)
    end do
    write(*,*) work

    eiginv = 0.0
    do i = 1, nplume, 1
        if (abs(w(i)) < 1e-10) then
            w(i) = 1e-10
        end if
        eiginv(i,i) = 1.0/w(i)
    end do
    
    write(*, *) 'weights: '
    write(*,*) matmul(cov, matmul(eiginv, matmul(transpose(cov), cross) ) )
    
end program test_lapack
