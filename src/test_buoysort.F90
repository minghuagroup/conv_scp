include "buoysort.F90"

program test_buoysort

    use buoysort, only : cal_fracmix, cal_buoysort, cal_qsat, conden, conden_new 

implicit none

    integer,parameter :: r8 = selected_real_kind(12)
    real(r8), parameter :: latvap = 2.501e6
    real(r8), parameter :: latice = 3.34e5
    real(r8), parameter :: tveps  = 0.608
    real(r8), parameter :: rair   = 287.0
    real(r8), parameter :: cpair  = 1004.0
    real(r8), parameter :: cridis = 10.0
    real(r8), parameter :: p0     = 100000.0
    real(r8), parameter :: grav   = 9.8
    real(r8) :: p = 60000.0
    real(r8) :: z
    real(r8) :: rho
    
    real(r8) :: te = 273.0
    real(r8) :: qve, qle, qte, qse
    real(r8) :: RHe

    real(r8) :: tc = 275.0
    real(r8) :: qvc, qlc, qtc, qsc
    real(r8) :: RHc
    real(r8) :: w = 1.0
    real(r8) :: tve, tvc, xsat, x0, xc, ent_rate, det_rate, thle, thlu, temp
    integer :: i,j,ni,nj,id_check

    z = rair*te*log(p0/p)/grav
    ni = 10

    do i = 0,10,1
        RHe = 70
        qlc = 0.002*(i-3)

        call cal_qsat(te, p, qse)
        qte = qse * RHe/100.0
        call cal_qsat(tc, p, qsc)
        qtc = qsc + qlc

        call cal_fracmix(te, qte, tc, qtc, p, w, cridis, xsat, x0, xc)
        
        write(*,"(10F10.4)") p, te, qte/qse*100, tc, qtc/qsc*100, qlc, x0, xc, xsat
    
    end do


    !write(*,*) 'z:',z,', p:',p/100
    !write(*,*) '   te,      qe,       RHe,      tc,       qc,       RHc,       xc'
    
    !do i = 1,1
    !    RH = 0.6 + (i-1)*(1.0-0.5)/ni
    !    do j = 1,21
    !        !RHC = 1.01 - 0.02
    !        RHC = 0.6 + (j-1)*0.05
    !        call cal_qsat(tu, p, qu)
    !        qu = qu*RHC
    !        if (RHC>1.0) then
    !            thlu = ( tu - latvap*qu*(RHC-1)/RHC / cpair ) * (p0/p)**(rair/cpair)
    !        else
    !            thlu = tu * (p0/p)**(rair/cpair)
    !        end if
    !        call conden(p, thlu, qu, thu, quv, qul, qui, qus, id_check)
    !        write(*, "(4F10.4)") tu, thu*(p/p0)**(rair/cpair),qu/RHC,qus

    !        call cal_qsat(te, p, qsat)
    !        qe = qsat * RH
    !        tve = te*(1+tveps*qe)
    !        thle = te * (p0/p) ** (rair/cpair)
            
    !        rho = p/rair/tve

    !        call cal_buoysort(2, cridis, z, p, rho, thle, qe, thlu, qu, wue, xc, ent_rate, det_rate)

    !        write(*, "(10F10.4)") te, qe*1000, RH*100, tu, qu*1000, RHC*100, xc
    !        !write(*, *) "=============================================="
            
    !    end do
    !end do

end program test_buoysort
