subroutine CALSIG
!----------------------------------------------------------------------------------
! Purpose: calculate SIG, SIGL, SIGC & SIGLC
! Author : ZhangHe
! Complete: 2007.9.20
! Update: ZhangHe, 2007.10.8
!----------------------------------------------------------------------------------
!!   use precision
   use Grid
   use Var_name_inic, only: hyai_in, hyam_in, hybi_in, hybm_in, ilev_in, P0_in, PS_out
   use prognostic, only: SIG, SIGL, SIGC, SIGLC, PMTOP, PINC, PLYC
   use Var_name_inic, only: ilev_out, lev_out ! czy

   implicit none

!----------------------------------Local workspace----------------------------------------------
   real*8, parameter :: ZERO = 0.0E0
   real*8, parameter :: ONE  = 1.0E0
   real*8, parameter :: HALF = 0.5E0
   real*8 :: PS_tmp(NLON,NLAT)
   real*8 :: PESCC, PESC, P0
   integer  :: I, J, K, KP, nfhy, nfsig, RECL, REC
   real*4 :: SIGC0(NLON,NLAT,NLVP)
   real*4 :: SIGLC0(NLON,NLAT,NLVP)
!------------------------------------------------------------------------------------------------
!  calculate SIG & SIGL 
!!   PMTOP  = ilev_in(1)
   PMTOP  = ZERO      !zhh 2008.5.13
   !PESCC  = ilev_in(NLVP) - PMTOP
   PESCC  = ilev_out(NLVP) - PMTOP
!!   do K  = 2, NLEV
   do K  = 1, NLEV
      !SIG (K) = (ilev_in(K) - PMTOP) / PESCC
      SIG (K) = (ilev_out(K) - PMTOP) / PESCC
   end do
!!   SIG(1)    = ZERO
   SIG(NLVP) = ONE
   do K  = 1, NLEV
      KP      = K + 1
      SIGL(K) = HALF * (SIG(KP) + SIG(K))
   end do
   SIGL(NLVP) = SIG(NLVP)
!
   do k = 1, NLEV
!!      print*, 'sig(',k,') =', sig(k), ' sigl(',k,') =', sigl(k)  
      print*, 'sig =', sig(k), 'sigl =', sigl(k), 'at k=', k  
      !print*, 'ilev_in =', ilev_in(k) 
      print*, 'ilev_out =', ilev_out(k) 
   end do
!
   !do k = 1, NLEV
   do k = 1, GLEV !czy
      print*, 'k =', k  
      print*, 'hyai =', hyai_in(k), 'hybi =', hybi_in(k)  
      print*, 'hyam =', hyam_in(k), 'hybm =', hybm_in(k)  
      print*, '-------------------------------------------------------------------'  
   end do
!   stop
!
!  calculate SIGC & SIGLC 
   P0 = P0_in / 100.0
   do J = 1, NLAT
      do I = 1, NLON 
         PS_tmp(I,J) = PS_out(I,J) / 100.0
      end do
   end do
!
   do j = 1, NLAT
      do i = 1, NLON      
         !do k = 1, NLEV
         do k = 1, GLEV! czy
            PINC(i,j,k) = hyai_in(k)*P0 + hybi_in(k)*PS_tmp(I,J)
            PLYC(i,j,k) = hyam_in(k)*P0 + hybm_in(k)*PS_tmp(I,J)
         end do
         !PINC(i,j,NLVP) = hyai_in(NLVP)*P0 + hybi_in(NLVP)*PS_tmp(I,J)
         !PLYC(i,j,NLVP) = PINC(i,j,NLVP)
         PINC(i,j,GLVP) = hyai_in(GLVP)*P0 + hybi_in(GLVP)*PS_tmp(I,J)! czy
         PLYC(i,j,GLVP) = PINC(i,j,GLVP)! czy
      end do
   end do
!
   do j = 1, NLAT
      do i = 1, NLON      
         PESC = PS_tmp(I,J) - PMTOP
         !do k = 1, NLEV
         do k = 1, GLEV! czy20170717
            SIGC(i,j,k)  = ( PINC(i,j,k) - PMTOP ) / PESC
            SIGLC(i,j,k) = ( PLYC(i,j,k) - PMTOP ) / PESC
         end do
         !SIGC(i,j,NLVP)  = ( PINC(i,j,NLVP) - PMTOP ) / PESC
         !SIGLC(i,j,NLVP) = SIGC(i,j,NLVP)
         SIGC(i,j,GLVP)  = ( PINC(i,j,GLVP) - PMTOP ) / PESC! czy
         SIGLC(i,j,GLVP) = SIGC(i,j,GLVP)! czy
      end do
   end do
! for check    
   do j = 1, NLAT
      do i = 1, NLON      
         !do k = 1, NLVP   ! zhh 2008.5.13 
         do k = 1, GLVP   ! czy
            if ( SIGC(i,j,k) < ZERO ) then
               print*, 'SIGC(',i,j,k,') =', SIGC(i,j,k)
               SIGC(i,j,k) = ZERO
            end if
            if ( SIGC(i,j,k) > ONE ) then
               print*, 'SIGC(',i,j,k,') =', SIGC(i,j,k)
               SIGC(i,j,k) = ONE
            end if
!
            if ( SIGLC(i,j,k) < ZERO ) then
               print*, 'SIGLC(',i,j,k,') =', SIGLC(i,j,k)
               SIGLC(i,j,k) = ZERO
            end if
            if ( SIGLC(i,j,k) > ONE ) then
               print*, 'SIGLC(',i,j,k,') =', SIGLC(i,j,k)
               SIGLC(i,j,k) = ONE
            end if
         end do
      end do
   end do
!
   do j = 1, NLAT
      do i = 1, NLON      
         !do k = 1, NLVP
         do k = 1, GLVP! czy
            SIGC0(i,j,k)  = SIGC(i,j,k)
            SIGLC0(i,j,k) = SIGLC(i,j,k)
         end do
      end do
   end do
!

end subroutine CALSIG  
