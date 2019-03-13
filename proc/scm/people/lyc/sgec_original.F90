! CVS: $Id: sgec.F90,v 2.1 2004/06/10 07:45:17 cvsroot Exp $
  SUBROUTINE SGEC
!========================
! SGEC
!---------------------------------------------------------------------
!
! purpose: calculate transport velocity of carbon
!
! author: Zhao Liang@lapc 2004/03/03 (original author: Xu Y F)
!
!---------------------------------------------------------------------
#include <def-undef.h>       
!
      USE param_mod
      USE pconst_mod
      USE carbon_mod
      USE tracer_mod
      USE cforce_mod
#ifdef SPMD      
      USE msg_mod
#endif      
!
!---------------------------------------------------------------------
      IMPLICIT NONE
#include <netcdf.inc>      
!#ifdef SPMD      
!#include <mpif.h>
!#endif      
!      
      REAL tssw(IMT,JMT),sssw(IMT,JMT) !SST and SSS
      REAL factsg,secyr,rho00

      REAL tv(IMT,JMT),sch(IMT,JMT),arphs(IMT,JMT)
      REAL TSEA,SALT,dw,unitsg
!---------------------------------------------------------------------
!     factsg    =1.0E-4/3.1536E7
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
      rho00=1025.0
      secyr=365.0*86400.0
      factsg=1.0E8/rho00/secyr
!        
!$OMP PARALLEL DO PRIVATE (I,J)        
      DO J=1,JMT
        DO I=1,IMT
          tssw(I,J)=AT(I,J,1,1)
          IF(AT(I,J,1,1).gt.40.0.and.AT(I,J,1,1).lt.999.) then
            tssw(I,J)=40.0
          ENDIF    
          sssw(I,J)=AT(I,J,1,2)*1000.0+35.0
        ENDDO
      ENDDO
!---------------------------------------------------------------------
!     CALCULATION OF TRANSFER VELOCITY ACCORDING TO Wanninkhof's EQUATION
!     The values of parameters are refer to the references
!---------------------------------------------------------------------
      dw=1.027e3
      unitsg=24.0*365.E-8
!
!$OMP PARALLEL DO PRIVATE (I,J,TSEA,SALT) 
      DO J=1,JMT
        DO I=1,IMT
          TSEA=AT(I,J,1,1)+273.15 !SST
          SALT=sssw(i,j)!AT(I,J,1,2)*1000.0+35.0 !SSS
          sge(I,J)=0.0
!---------------------------------------------------------------------
!     Schmit number at seawater
!---------------------------------------------------------------------
          IF(SALT.LE.0.1.OR.VIT(I,J,1).EQ.0) CYCLE
          sch(I,J)=2073.1-125.62*tssw(I,J)+3.6276*tssw(I,J)**2 &
                         -0.043219*tssw(I,J)**3
#ifdef SPMD
          IF(sch(I,J).LE.0.0) THEN 
            PRINT*, 'mytid=',mytid,'notice: SCH less than 0', &
                     i,j,sch(i,j),tssw(i,j),SALT,itice(i,j),"SGEC"
          ENDIF
#else          
          IF(sch(I,J).LE.0.0) THEN 
            PRINT*, 'notice: SCH less than 0', &
                     i,j,sch(i,j),tssw(i,j),SALT,itice(i,j),"SGEC"
          ENDIF
#endif
          arphs(I,J)=EXP(-60.2409+9345.17/TSEA            &
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
!          tv(I,J)=7.946*w22np(I,J)*w22np(I,J)/SQRT(sch(I,J))
          tv(I,J)=10.0*w22np(I,J)*w22np(I,J)/SQRT(sch(I,J))
          sge(I,J)=tv(I,J)*arphs(I,J)*dw*unitsg
        ENDDO
      ENDDO
!---------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE (J,I)
      DO J=1,JMT
        DO I=1,IMT
!---------------------------------------------------------------------
! devided by 100.0 to transform the unit from cm/s to m/s            
!---------------------------------------------------------------------
          sge(I,J)=sge(I,J)*factsg/100.0
        ENDDO
      ENDDO
!      
      RETURN
      END SUBROUTINE SGEC
