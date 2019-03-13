
! CVS: $Id: sgec.F90,v 2.1 2004/06/10 07:45:17 cvsroot Exp $

module sgec_mod

contains

  !SUBROUTINE SGEC
  FUNCTION SGEC(tssw,sssw,u10,sge)
!===================================
! SGEC
!---------------------------------------------------------------------
!
! purpose: calculate transport velocity of carbon
!
! author: Zhao Liang@lapc 2004/03/03 (original author: Xu Y F)
!
! author: Minghua Zhang 2018-08-15
!
!---------------------------------------------------------------------
!      
      REAL(R8),intent(in)  :: tssw !SST in Kelvin
      REAL(R8),intent(in)  :: sssw  !SSS in what unit ? !AT(I,J,1,2)*1000.0+35.0
      REAL(R8),intent(in)  :: u10  !10-m wind m/s
      REAL(R8),intent(out) :: sge   !in units of cm/s * umol/kg/ppm after factsg
      REAL(R8) factsg,secyr,rho00

      REAL(R8) :: tv ,sch ,arphs, tssw,sssw 
      REAL(R8) :: TSEA,SALT,dw,unitsg
!---------------------------------------------------------------------
!     factsg    = 1.0E-4/3.1536E7
!     sge       =0.05 in units of mol/m^2/yr/ppm
!     factsg    for units of umol/dm^2/s (x10, for converting dz)
!     dz        in units of cm
!     sgbrok    in units of mol/m^2/yr/ppm
!---------------------------------------------------------------------
!     sge       in units of cm/s * umol/kg/ppm after factsg
!---------------------------------------------------------------------
!     factsg    =1.0E4/secyr*10.
!---------------------------------------------------------------------
!
      rho00  = 1025.0_r8
      secyr  = 365.0_r8*86400.0_r8
      factsg = 1.0E8_r8/rho00/secyr


!---------------------------------------------------------------------
!     CALCULATION OF TRANSFER VELOCITY ACCORDING TO Wanninkhof's EQUATION
!     The values of parameters are refer to the references
!---------------------------------------------------------------------
      dw     = 1.027e3_r8
      unitsgi= 24.0*365.E-8_r8
!
          TSEA = tssw + 273.15 !SST
          SALT = sssw  !AT(I,J,1,2)*1000.0+35.0 !SSS
          sge  = 0.0
!---------------------------------------------------------------------
!     Schmit number at seawater
!---------------------------------------------------------------------
          IF(SALT.LE.0.1) then
          sch =2073.1-125.62*tssw +3.6276*tssw **2 &
                         -0.043219*tssw **3

          IF(sch .LE.0.0) THEN 
!juanxiong, please improve the line below
            PRINT*, 'notice: SCH less than 0', &
                     sch,tssw,SALT
          sch = 0.E-20_r8 ! OK?
          ENDIF

          arphs =EXP(-60.2409+9345.17/TSEA            &
                         +23.3585*LOG(TSEA/100.)+SALT     &
                         *(0.023517-0.023656*(TSEA/100.)  &
                         +0.0047036*(TSEA/100.)**2))
!---------------------------------------------------------------------
!     TV from Wanninkhof's equation in units of cm/h
!     Sge is an exchange coefficient for CO2 in units of mol/m^2/yr/ppm
!---------------------------------------------------------------------
!     for steady or instant wind,7.946
!     for climatological wind, 10.0
!---------------------------------------------------------------------

          tv  = 10.0*u10 *u10 /SQRT(sch )
          sge = tv *arphs *dw*unitsg
!---------------------------------------------------------------------
! devided by 100.0 to transform the unit from cm/s to m/s            
!---------------------------------------------------------------------
          sge =sge *factsg/100.0
!      
      RETURN
      END FUNCTION SGEC

end module sgec_mod
