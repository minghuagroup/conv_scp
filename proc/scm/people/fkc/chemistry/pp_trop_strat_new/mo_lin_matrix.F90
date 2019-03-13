




      module mo_lin_matrix

      private
      public :: linmat

      contains

      subroutine linmat01( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

         mat(139) = -( rxt(2) + rxt(3) + het_rates(1) )
         mat(186) = rxt(19)

         mat(189) = -( rxt(19) + het_rates(2) )
         mat(142) = rxt(3)
         mat(204) = rxt(5)
         mat(258) = rxt(6)
         mat(218) = rxt(22) + rxt(23)

         mat(220) = -( rxt(22) + rxt(23) + rxt(25)*y(4) + rxt(26)*y(4) + rxt(28)*y(31) &
                      + rxt(29)*y(32) + rxt(30)*y(12) + rxt(31)*y(12) + rxt(32)*y(12) &
                 + het_rates(3) )
         mat(144) = rxt(2)

         mat(75) = -( het_rates(17) )
         mat(107) = rxt(11) + rxt(12)

         mat(1) = -( het_rates(5) )

         mat(173) = -( het_rates(6) )
         mat(203) = rxt(5) + .500_r8*rxt(94)
         mat(257) = rxt(7)
         mat(217) = 2.000_r8*rxt(25)*y(4)

         mat(205) = -( rxt(5) + rxt(94) + het_rates(7) )
         mat(259) = rxt(6)
         mat(66) = rxt(9) + rxt(63)
         mat(49) = rxt(16)
         mat(34) = rxt(55)

         mat(246) = -( rxt(78)*y(12) + het_rates(19) )
         mat(67) = rxt(8)
         mat(74) = rxt(10)
         mat(62) = 2.000_r8*rxt(13)
         mat(31) = rxt(14)
         mat(207) = .500_r8*rxt(94)
         mat(221) = rxt(30)*y(12)

         mat(262) = -( rxt(6) + rxt(7) + rxt(93) + het_rates(8) )
         mat(68) = rxt(8)
         mat(50) = rxt(15)
         mat(35) = rxt(55)

         mat(2) = -( het_rates(9) )
         mat(32) = 2.000_r8*rxt(92)
         mat(248) = rxt(93)
         mat(194) = .500_r8*rxt(94)

         mat(63) = -( rxt(8) + rxt(9) + rxt(63) + het_rates(10) )

         mat(33) = -( rxt(55) + rxt(92) + het_rates(11) )

         mat(91) = -( het_rates(13) )
         mat(211) = rxt(30)*y(12)
         mat(236) = rxt(78)*y(12)

         mat(69) = -( rxt(10) + het_rates(14) )

         mat(110) = -( rxt(11) + rxt(12) + het_rates(16) )
         mat(73) = rxt(10)
         mat(213) = rxt(31)*y(12) + rxt(32)*y(12)

         mat(100) = -( rxt(33) + het_rates(18) )
         mat(72) = rxt(10)
         mat(109) = 2.000_r8*rxt(11)
         mat(212) = rxt(31)*y(12)

         mat(159) = -( rxt(99) + het_rates(20) )
         mat(65) = rxt(9) + rxt(63)
         mat(102) = rxt(33)
         mat(216) = rxt(31)*y(12)

         mat(58) = -( rxt(13) + het_rates(21) )
         mat(149) = .500_r8*rxt(99)

         mat(79) = -( het_rates(54) )
         mat(234) = rxt(78)*y(12)

         mat(83) = -( het_rates(22) )
         mat(30) = rxt(14)
         mat(47) = rxt(15)
         mat(210) = 3.000_r8*rxt(28)*y(31) + 2.000_r8*rxt(29)*y(32)

         mat(3) = -( het_rates(23) )

         mat(124) = -( het_rates(24) )
         mat(48) = rxt(16)
         mat(25) = 2.000_r8*rxt(77)

         mat(4) = -( het_rates(25) )

         mat(24) = -( rxt(77) + het_rates(26) )

         mat(5) = -( het_rates(27) )

         mat(29) = -( rxt(14) + het_rates(28) )

         mat(46) = -( rxt(15) + rxt(16) + het_rates(29) )

         mat(51) = -( het_rates(30) )

         mat(36) = -( het_rates(15) )

         mat(27) = -( het_rates(34) )

         mat(42) = -( het_rates(35) )

         mat(6) = -( het_rates(38) )

         mat(7) = -( het_rates(36) )

         mat(8) = -( het_rates(37) )

         mat(9) = -( het_rates(39) )

         mat(10) = -( het_rates(40) )

         mat(11) = -( het_rates(41) )

         mat(12) = -( het_rates(42) )

         mat(13) = -( het_rates(43) )

         mat(14) = -( het_rates(44) )

         mat(15) = -( het_rates(45) )

         mat(16) = -( het_rates(46) )

         mat(17) = -( het_rates(47) )

         mat(18) = -( het_rates(48) )

         mat(19) = -( het_rates(49) )

         mat(20) = -( het_rates(50) )

         mat(21) = -( het_rates(51) )

         mat(22) = -( het_rates(52) )

         mat(23) = -( het_rates(53) )


      end subroutine linmat01

      subroutine linmat( mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(in) :: het_rates(max(1,gas_pcnst))
      real(r8), intent(inout) :: mat(nzcnt)

      call linmat01( mat, y, rxt, het_rates )

      end subroutine linmat

      end module mo_lin_matrix
