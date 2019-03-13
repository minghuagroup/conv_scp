




      module mo_nln_matrix

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: nlnmat

      contains

      subroutine nlnmat01( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------


         mat(139) = -(rxt(20)*y(2) + rxt(27)*y(3) + rxt(34)*y(18) + rxt(39)*y(19) &
                      + rxt(45)*y(20) + rxt(50)*y(6) + rxt(53)*y(7) + rxt(64)*y(22) &
                      + rxt(91)*y(30))
         mat(186) = -rxt(20)*y(1)
         mat(215) = -rxt(27)*y(1)
         mat(101) = -rxt(34)*y(1)
         mat(240) = -rxt(39)*y(1)
         mat(158) = -rxt(45)*y(1)
         mat(171) = -rxt(50)*y(1)
         mat(201) = -rxt(53)*y(1)
         mat(85) = -rxt(64)*y(1)
         mat(55) = -rxt(91)*y(1)

         mat(189) = -(rxt(20)*y(1) + 4._r8*rxt(21)*y(2) + rxt(38)*y(19) + rxt(44) &
                      *y(20) + rxt(47)*y(21) + rxt(48)*y(6) + (rxt(51) + rxt(52) &
                      ) * y(7) + rxt(58)*y(8) + rxt(67)*y(24) + rxt(83)*y(16))
         mat(142) = -rxt(20)*y(2)
         mat(243) = -rxt(38)*y(2)
         mat(161) = -rxt(44)*y(2)
         mat(61) = -rxt(47)*y(2)
         mat(174) = -rxt(48)*y(2)
         mat(204) = -(rxt(51) + rxt(52)) * y(2)
         mat(258) = -rxt(58)*y(2)
         mat(128) = -rxt(67)*y(2)
         mat(113) = -rxt(83)*y(2)

         mat(243) = mat(243) + 2.000_r8*rxt(41)*y(19)
         mat(103) = rxt(37)*y(20)
         mat(161) = mat(161) + rxt(37)*y(18)

         mat(220) = -(rxt(24)*y(54) + rxt(27)*y(1))
         mat(80) = -rxt(24)*y(3)
         mat(144) = -rxt(27)*y(3)

         mat(75) = -((rxt(79) + rxt(80)) * y(19))
         mat(233) = -(rxt(79) + rxt(80)) * y(17)

         mat(133) = .050_r8*rxt(91)*y(30)
         mat(180) = rxt(83)*y(16)
         mat(233) = mat(233) + rxt(82)*y(16)
         mat(252) = rxt(81)*y(16)
         mat(107) = rxt(83)*y(2) + rxt(82)*y(19) + rxt(81)*y(8)
         mat(52) = .050_r8*rxt(91)*y(1)


         mat(173) = -(rxt(48)*y(2) + rxt(49)*y(20) + rxt(50)*y(1) + rxt(57)*y(8) &
                      + rxt(71)*y(24) + rxt(84)*y(13))
         mat(188) = -rxt(48)*y(6)
         mat(160) = -rxt(49)*y(6)
         mat(141) = -rxt(50)*y(6)
         mat(257) = -rxt(57)*y(6)
         mat(127) = -rxt(71)*y(6)
         mat(95) = -rxt(84)*y(6)

         mat(188) = mat(188) + rxt(51)*y(7)
         mat(203) = rxt(51)*y(2)

         mat(205) = -((rxt(51) + rxt(52)) * y(2) + rxt(53)*y(1) + rxt(54)*y(8) + rxt(56) &
                      *y(19) + rxt(61)*y(20) + rxt(72)*y(24))
         mat(190) = -(rxt(51) + rxt(52)) * y(7)
         mat(143) = -rxt(53)*y(7)
         mat(259) = -rxt(54)*y(7)
         mat(244) = -rxt(56)*y(7)
         mat(162) = -rxt(61)*y(7)
         mat(129) = -rxt(72)*y(7)

         mat(143) = mat(143) + rxt(50)*y(6)
         mat(190) = mat(190) + rxt(48)*y(6) + rxt(58)*y(8)
         mat(175) = rxt(50)*y(1) + rxt(48)*y(2) + 2.000_r8*rxt(57)*y(8) + rxt(84) &
                      *y(13) + rxt(49)*y(20) + rxt(71)*y(24)
         mat(244) = mat(244) + rxt(59)*y(8) + rxt(62)*y(10)
         mat(259) = mat(259) + rxt(58)*y(2) + 2.000_r8*rxt(57)*y(6) + rxt(59)*y(19) &
                      + rxt(60)*y(20)
         mat(66) = rxt(62)*y(19)
         mat(96) = rxt(84)*y(6)
         mat(162) = mat(162) + rxt(49)*y(6) + rxt(60)*y(8)
         mat(129) = mat(129) + rxt(71)*y(6)

         mat(246) = -(rxt(38)*y(2) + rxt(39)*y(1) + rxt(40)*y(20) + (4._r8*rxt(41) &
                      + 4._r8*rxt(42)) * y(19) + rxt(43)*y(21) + rxt(56)*y(7) + rxt(59) &
                      *y(8) + rxt(62)*y(10) + (rxt(68) + rxt(69)) * y(24) + (rxt(79) &
                      + rxt(80)) * y(17) + rxt(82)*y(16) + rxt(88)*y(15) + rxt(89) &
                      *y(14) + rxt(90)*y(30) + rxt(95)*y(34) + (rxt(96) + rxt(97) &
                      ) * y(35))
         mat(192) = -rxt(38)*y(19)
         mat(145) = -rxt(39)*y(19)
         mat(164) = -rxt(40)*y(19)
         mat(62) = -rxt(43)*y(19)
         mat(207) = -rxt(56)*y(19)
         mat(261) = -rxt(59)*y(19)
         mat(67) = -rxt(62)*y(19)
         mat(130) = -(rxt(68) + rxt(69)) * y(19)
         mat(78) = -(rxt(79) + rxt(80)) * y(19)
         mat(115) = -rxt(82)*y(19)
         mat(39) = -rxt(88)*y(19)
         mat(74) = -rxt(89)*y(19)
         mat(57) = -rxt(90)*y(19)
         mat(28) = -rxt(95)*y(19)
         mat(44) = -(rxt(96) + rxt(97)) * y(19)

         mat(145) = mat(145) + rxt(34)*y(18) + rxt(45)*y(20)
         mat(192) = mat(192) + rxt(83)*y(16) + rxt(44)*y(20) + rxt(47)*y(21)
         mat(221) = 2.000_r8*rxt(24)*y(54)
         mat(177) = rxt(49)*y(20)
         mat(246) = mat(246) + .300_r8*rxt(89)*y(14)
         mat(261) = mat(261) + rxt(60)*y(20)
         mat(74) = mat(74) + .300_r8*rxt(89)*y(19)
         mat(115) = mat(115) + rxt(83)*y(2)
         mat(105) = rxt(34)*y(1) + 2.000_r8*rxt(35)*y(20)
         mat(164) = mat(164) + rxt(45)*y(1) + rxt(44)*y(2) + rxt(49)*y(6) + rxt(60) &
                      *y(8) + 2.000_r8*rxt(35)*y(18) + rxt(66)*y(22)
         mat(62) = mat(62) + rxt(47)*y(2)
         mat(81) = 2.000_r8*rxt(24)*y(3)
         mat(87) = rxt(66)*y(20)

         mat(262) = -(rxt(54)*y(7) + rxt(57)*y(6) + rxt(58)*y(2) + rxt(59)*y(19) &
                      + rxt(60)*y(20) + rxt(81)*y(16) + rxt(98)*y(35))
         mat(208) = -rxt(54)*y(8)
         mat(178) = -rxt(57)*y(8)
         mat(193) = -rxt(58)*y(8)
         mat(247) = -rxt(59)*y(8)
         mat(165) = -rxt(60)*y(8)
         mat(116) = -rxt(81)*y(8)
         mat(45) = -rxt(98)*y(8)

         mat(146) = rxt(53)*y(7)
         mat(193) = mat(193) + rxt(52)*y(7)
         mat(208) = mat(208) + rxt(53)*y(1) + rxt(52)*y(2)


         mat(194) = rxt(56)*y(19)
         mat(223) = rxt(56)*y(7)
         mat(248) = rxt(81)*y(16) + rxt(98)*y(35)
         mat(106) = rxt(81)*y(8)
         mat(40) = rxt(98)*y(8)

         mat(63) = -(rxt(62)*y(19))
         mat(231) = -rxt(62)*y(10)

         mat(197) = rxt(61)*y(20)
         mat(150) = rxt(61)*y(7)


         mat(195) = rxt(54)*y(8)
         mat(250) = rxt(54)*y(7)

         mat(91) = -(rxt(84)*y(6) + rxt(85)*y(20) + (4._r8*rxt(86) + 4._r8*rxt(87) &
                      ) * y(13))
         mat(167) = -rxt(84)*y(13)
         mat(154) = -rxt(85)*y(13)

         mat(135) = 1.860_r8*rxt(91)*y(30)
         mat(236) = .700_r8*rxt(89)*y(14)
         mat(71) = .700_r8*rxt(89)*y(19)
         mat(53) = 1.860_r8*rxt(91)*y(1)

         mat(69) = -(rxt(89)*y(19))
         mat(232) = -rxt(89)*y(14)

         mat(89) = rxt(85)*y(20)
         mat(151) = rxt(85)*y(13)

         mat(110) = -(rxt(81)*y(8) + rxt(82)*y(19) + rxt(83)*y(2))
         mat(254) = -rxt(81)*y(16)
         mat(238) = -rxt(82)*y(16)
         mat(184) = -rxt(83)*y(16)

         mat(137) = .870_r8*rxt(91)*y(30)
         mat(169) = rxt(84)*y(13)
         mat(238) = mat(238) + .300_r8*rxt(89)*y(14) + rxt(88)*y(15)
         mat(93) = rxt(84)*y(6) + (4.000_r8*rxt(86)+2.000_r8*rxt(87))*y(13)
         mat(73) = .300_r8*rxt(89)*y(19)
         mat(54) = .870_r8*rxt(91)*y(1)
         mat(37) = rxt(88)*y(19)

         mat(100) = -(rxt(34)*y(1) + (rxt(35) + rxt(36) + rxt(37)) * y(20))
         mat(136) = -rxt(34)*y(18)
         mat(155) = -(rxt(35) + rxt(36) + rxt(37)) * y(18)

         mat(183) = rxt(38)*y(19)
         mat(76) = rxt(79)*y(19)
         mat(237) = rxt(38)*y(2) + rxt(79)*y(17) + rxt(82)*y(16)
         mat(109) = rxt(82)*y(19)

         mat(159) = -((rxt(35) + rxt(36) + rxt(37)) * y(18) + rxt(40)*y(19) + rxt(44) &
                      *y(2) + rxt(45)*y(1) + 4._r8*rxt(46)*y(20) + rxt(49)*y(6) + rxt(60) &
                      *y(8) + rxt(61)*y(7) + (rxt(65) + rxt(66)) * y(22) + rxt(70) &
                      *y(24) + rxt(85)*y(13))
         mat(102) = -(rxt(35) + rxt(36) + rxt(37)) * y(20)
         mat(241) = -rxt(40)*y(20)
         mat(187) = -rxt(44)*y(20)
         mat(140) = -rxt(45)*y(20)
         mat(172) = -rxt(49)*y(20)
         mat(256) = -rxt(60)*y(20)
         mat(202) = -rxt(61)*y(20)
         mat(86) = -(rxt(65) + rxt(66)) * y(20)
         mat(126) = -rxt(70)*y(20)
         mat(94) = -rxt(85)*y(20)

         mat(140) = mat(140) + rxt(39)*y(19) + .060_r8*rxt(91)*y(30)
         mat(187) = mat(187) + rxt(83)*y(16) + rxt(47)*y(21)
         mat(77) = rxt(80)*y(19)
         mat(172) = mat(172) + rxt(84)*y(13)
         mat(241) = mat(241) + rxt(39)*y(1) + rxt(80)*y(17) + rxt(59)*y(8) + rxt(43) &
                      *y(21) + rxt(68)*y(24) + rxt(88)*y(15) + .500_r8*rxt(97)*y(35)
         mat(256) = mat(256) + rxt(59)*y(19) + rxt(81)*y(16)
         mat(94) = mat(94) + rxt(84)*y(6) + 4.000_r8*rxt(86)*y(13)
         mat(112) = rxt(83)*y(2) + rxt(81)*y(8)
         mat(60) = rxt(47)*y(2) + rxt(43)*y(19)
         mat(126) = mat(126) + rxt(68)*y(19)
         mat(56) = .060_r8*rxt(91)*y(1)
         mat(38) = rxt(88)*y(19)
         mat(43) = .500_r8*rxt(97)*y(19)

         mat(58) = -(rxt(43)*y(19) + rxt(47)*y(2))
         mat(230) = -rxt(43)*y(21)
         mat(179) = -rxt(47)*y(21)

         mat(230) = mat(230) + 2.000_r8*rxt(42)*y(19)
         mat(149) = 2.000_r8*rxt(46)*y(20)

         mat(79) = -(rxt(24)*y(3))
         mat(209) = -rxt(24)*y(54)

         mat(234) = 2.000_r8*rxt(41)*y(19) + rxt(62)*y(10) + rxt(89)*y(14) + rxt(82) &
                      *y(16) + rxt(40)*y(20) + rxt(43)*y(21)
         mat(64) = rxt(62)*y(19)
         mat(70) = rxt(89)*y(19)
         mat(108) = rxt(82)*y(19)
         mat(99) = rxt(37)*y(20)
         mat(152) = rxt(40)*y(19) + rxt(37)*y(18)
         mat(59) = rxt(43)*y(19)

         mat(83) = -(rxt(64)*y(1) + (rxt(65) + rxt(66)) * y(20))
         mat(134) = -rxt(64)*y(22)
         mat(153) = -(rxt(65) + rxt(66)) * y(22)

         mat(182) = rxt(67)*y(24)
         mat(166) = rxt(71)*y(24)
         mat(235) = rxt(68)*y(24)
         mat(123) = rxt(67)*y(2) + rxt(71)*y(6) + rxt(68)*y(19) + (4.000_r8*rxt(73) &
                       +2.000_r8*rxt(75))*y(24)


      end subroutine nlnmat01

      subroutine nlnmat02( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------



         mat(117) = 2.000_r8*rxt(74)*y(24)

         mat(124) = -(rxt(67)*y(2) + (rxt(68) + rxt(69)) * y(19) + rxt(70)*y(20) &
                      + rxt(71)*y(6) + rxt(72)*y(7) + (4._r8*rxt(73) + 4._r8*rxt(74) &
                      + 4._r8*rxt(75) + 4._r8*rxt(76)) * y(24))
         mat(185) = -rxt(67)*y(24)
         mat(239) = -(rxt(68) + rxt(69)) * y(24)
         mat(157) = -rxt(70)*y(24)
         mat(170) = -rxt(71)*y(24)
         mat(200) = -rxt(72)*y(24)

         mat(138) = rxt(64)*y(22)
         mat(157) = mat(157) + rxt(66)*y(22)
         mat(84) = rxt(64)*y(1) + rxt(66)*y(20)


         mat(118) = 2.000_r8*rxt(75)*y(24)


         mat(120) = 2.000_r8*rxt(76)*y(24)


         mat(224) = rxt(69)*y(24)
         mat(147) = rxt(65)*y(22)
         mat(82) = rxt(65)*y(20)
         mat(119) = rxt(69)*y(19)


         mat(148) = rxt(70)*y(24)
         mat(121) = rxt(70)*y(20)


         mat(196) = rxt(72)*y(24)
         mat(122) = rxt(72)*y(7)

         mat(51) = -(rxt(90)*y(19) + rxt(91)*y(1))
         mat(229) = -rxt(90)*y(30)
         mat(132) = -rxt(91)*y(30)

         mat(36) = -(rxt(88)*y(19))
         mat(227) = -rxt(88)*y(15)

         mat(88) = 2.000_r8*rxt(87)*y(13)

         mat(27) = -(rxt(95)*y(19))
         mat(226) = -rxt(95)*y(34)

         mat(226) = mat(226) + (rxt(96)+.500_r8*rxt(97))*y(35)
         mat(249) = rxt(98)*y(35)
         mat(41) = (rxt(96)+.500_r8*rxt(97))*y(19) + rxt(98)*y(8)

         mat(42) = -((rxt(96) + rxt(97)) * y(19) + rxt(98)*y(8))
         mat(228) = -(rxt(96) + rxt(97)) * y(35)
         mat(251) = -rxt(98)*y(35)



         mat(225) = rxt(95)*y(34)
         mat(26) = rxt(95)*y(19)
      end subroutine nlnmat02
      subroutine nlnmat_finit( mat, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: dti
      real(r8), intent(in) :: lmat(nzcnt)
      real(r8), intent(inout) :: mat(nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
         mat( 1) = lmat( 1)
         mat( 2) = lmat( 2)
         mat( 3) = lmat( 3)
         mat( 4) = lmat( 4)
         mat( 5) = lmat( 5)
         mat( 6) = lmat( 6)
         mat( 7) = lmat( 7)
         mat( 8) = lmat( 8)
         mat( 9) = lmat( 9)
         mat( 10) = lmat( 10)
         mat( 11) = lmat( 11)
         mat( 12) = lmat( 12)
         mat( 13) = lmat( 13)
         mat( 14) = lmat( 14)
         mat( 15) = lmat( 15)
         mat( 16) = lmat( 16)
         mat( 17) = lmat( 17)
         mat( 18) = lmat( 18)
         mat( 19) = lmat( 19)
         mat( 20) = lmat( 20)
         mat( 21) = lmat( 21)
         mat( 22) = lmat( 22)
         mat( 23) = lmat( 23)
         mat( 24) = lmat( 24)
         mat( 25) = lmat( 25)
         mat( 27) = mat( 27) + lmat( 27)
         mat( 29) = lmat( 29)
         mat( 30) = lmat( 30)
         mat( 31) = lmat( 31)
         mat( 32) = lmat( 32)
         mat( 33) = lmat( 33)
         mat( 34) = lmat( 34)
         mat( 35) = lmat( 35)
         mat( 36) = mat( 36) + lmat( 36)
         mat( 42) = mat( 42) + lmat( 42)
         mat( 46) = lmat( 46)
         mat( 47) = lmat( 47)
         mat( 48) = lmat( 48)
         mat( 49) = lmat( 49)
         mat( 50) = lmat( 50)
         mat( 51) = mat( 51) + lmat( 51)
         mat( 58) = mat( 58) + lmat( 58)
         mat( 62) = mat( 62) + lmat( 62)
         mat( 63) = mat( 63) + lmat( 63)
         mat( 65) = lmat( 65)
         mat( 66) = mat( 66) + lmat( 66)
         mat( 67) = mat( 67) + lmat( 67)
         mat( 68) = lmat( 68)
         mat( 69) = mat( 69) + lmat( 69)
         mat( 72) = lmat( 72)
         mat( 73) = mat( 73) + lmat( 73)
         mat( 74) = mat( 74) + lmat( 74)
         mat( 75) = mat( 75) + lmat( 75)
         mat( 79) = mat( 79) + lmat( 79)
         mat( 83) = mat( 83) + lmat( 83)
         mat( 91) = mat( 91) + lmat( 91)
         mat( 100) = mat( 100) + lmat( 100)
         mat( 102) = mat( 102) + lmat( 102)
         mat( 107) = mat( 107) + lmat( 107)
         mat( 109) = mat( 109) + lmat( 109)
         mat( 110) = mat( 110) + lmat( 110)
         mat( 124) = mat( 124) + lmat( 124)
         mat( 139) = mat( 139) + lmat( 139)
         mat( 142) = mat( 142) + lmat( 142)
         mat( 144) = mat( 144) + lmat( 144)
         mat( 149) = mat( 149) + lmat( 149)
         mat( 159) = mat( 159) + lmat( 159)
         mat( 173) = mat( 173) + lmat( 173)
         mat( 186) = mat( 186) + lmat( 186)
         mat( 189) = mat( 189) + lmat( 189)
         mat( 194) = mat( 194) + lmat( 194)
         mat( 203) = mat( 203) + lmat( 203)
         mat( 204) = mat( 204) + lmat( 204)
         mat( 205) = mat( 205) + lmat( 205)
         mat( 207) = mat( 207) + lmat( 207)
         mat( 210) = lmat( 210)
         mat( 211) = lmat( 211)
         mat( 212) = lmat( 212)
         mat( 213) = lmat( 213)
         mat( 216) = lmat( 216)
         mat( 217) = lmat( 217)
         mat( 218) = lmat( 218)
         mat( 220) = mat( 220) + lmat( 220)
         mat( 221) = mat( 221) + lmat( 221)
         mat( 234) = mat( 234) + lmat( 234)
         mat( 236) = mat( 236) + lmat( 236)
         mat( 246) = mat( 246) + lmat( 246)
         mat( 248) = mat( 248) + lmat( 248)
         mat( 257) = mat( 257) + lmat( 257)
         mat( 258) = mat( 258) + lmat( 258)
         mat( 259) = mat( 259) + lmat( 259)
         mat( 262) = mat( 262) + lmat( 262)
         mat( 90) = 0._r8
         mat( 92) = 0._r8
         mat( 97) = 0._r8
         mat( 98) = 0._r8
         mat( 104) = 0._r8
         mat( 111) = 0._r8
         mat( 114) = 0._r8
         mat( 125) = 0._r8
         mat( 131) = 0._r8
         mat( 156) = 0._r8
         mat( 163) = 0._r8
         mat( 168) = 0._r8
         mat( 176) = 0._r8
         mat( 181) = 0._r8
         mat( 191) = 0._r8
         mat( 198) = 0._r8
         mat( 199) = 0._r8
         mat( 206) = 0._r8
         mat( 214) = 0._r8
         mat( 219) = 0._r8
         mat( 222) = 0._r8
         mat( 242) = 0._r8
         mat( 245) = 0._r8
         mat( 253) = 0._r8
         mat( 255) = 0._r8
         mat( 260) = 0._r8
         mat( 1) = mat( 1) - dti
         mat( 2) = mat( 2) - dti
         mat( 3) = mat( 3) - dti
         mat( 4) = mat( 4) - dti
         mat( 5) = mat( 5) - dti
         mat( 6) = mat( 6) - dti
         mat( 7) = mat( 7) - dti
         mat( 8) = mat( 8) - dti
         mat( 9) = mat( 9) - dti
         mat( 10) = mat( 10) - dti
         mat( 11) = mat( 11) - dti
         mat( 12) = mat( 12) - dti
         mat( 13) = mat( 13) - dti
         mat( 14) = mat( 14) - dti
         mat( 15) = mat( 15) - dti
         mat( 16) = mat( 16) - dti
         mat( 17) = mat( 17) - dti
         mat( 18) = mat( 18) - dti
         mat( 19) = mat( 19) - dti
         mat( 20) = mat( 20) - dti
         mat( 21) = mat( 21) - dti
         mat( 22) = mat( 22) - dti
         mat( 23) = mat( 23) - dti
         mat( 24) = mat( 24) - dti
         mat( 27) = mat( 27) - dti
         mat( 29) = mat( 29) - dti
         mat( 33) = mat( 33) - dti
         mat( 36) = mat( 36) - dti
         mat( 42) = mat( 42) - dti
         mat( 46) = mat( 46) - dti
         mat( 51) = mat( 51) - dti
         mat( 58) = mat( 58) - dti
         mat( 63) = mat( 63) - dti
         mat( 69) = mat( 69) - dti
         mat( 75) = mat( 75) - dti
         mat( 79) = mat( 79) - dti
         mat( 83) = mat( 83) - dti
         mat( 91) = mat( 91) - dti
         mat( 100) = mat( 100) - dti
         mat( 110) = mat( 110) - dti
         mat( 124) = mat( 124) - dti
         mat( 139) = mat( 139) - dti
         mat( 159) = mat( 159) - dti
         mat( 173) = mat( 173) - dti
         mat( 189) = mat( 189) - dti
         mat( 205) = mat( 205) - dti
         mat( 220) = mat( 220) - dti
         mat( 246) = mat( 246) - dti
         mat( 262) = mat( 262) - dti
      end subroutine nlnmat_finit
      subroutine nlnmat( mat, y, rxt, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(r8), intent(in) :: dti
      real(r8), intent(in) :: lmat(nzcnt)
      real(r8), intent(in) :: y(gas_pcnst)
      real(r8), intent(in) :: rxt(rxntot)
      real(r8), intent(inout) :: mat(nzcnt)
      call nlnmat01( mat, y, rxt )
      call nlnmat02( mat, y, rxt )
      call nlnmat_finit( mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
