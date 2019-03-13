! CVS: $Id: flux_pt.F90,v 2.1 2004/06/10 07:45:17 cvsroot Exp $
!------------------------
     SUBROUTINE FLUX_PT
!=======================================================
!
! purpose: give surface flux of CCs (carbon or C14)
!          (original author: Xu Y F)
!
! author: Zhao Liang@lapc 2004/03/03
!
!---------------------------------------------------------------------
#include <def-undef.h>      
!---------------------- 
!
      USE param_mod
      USE pconst_mod
      USE tracer_mod
      USE carbon_mod
      USE coutput_mod
      USE cforce_mod
      USE forc_mod   !for sea ice
!----------------------------------
#ifdef COUP
      USE buf_mod
      USE control_mod,only:ncpl
#endif

#ifdef SPMD      
      USE msg_mod
#endif      
!
!----------------------------------------------
      IMPLICIT NONE
#include <netcdf.inc>      
!#ifdef SPMD      
!#include <mpif.h>
!#endif      
! -----------------------------------------------------------------
!---------------------------------------------------------------------
!     This sub is used to calculate the FLUX of a natural carbon
!     cycle including a simple biological parameterization
!     1) pCO2 flux at surface
!     2) STC, STA 
!---------------------------------------------------------------------
      REAL T0,ES,FACTOR,FACTX
!----xu-----
      REAL tt(imt,jmt),ss(imt,jmt),tatmp(imt,jmt),tctmp(imt,jmt)
!       
#ifdef COUP
     w22np=sqrt(duu10n)
!     pressureday=patm
     pressureday=patm/1.01325*1.0E-5
!     if(mytid==0) print *,'pressureday: ',pressureday(2,2),'wind: ',w22np(2,2)
          !pressureday(i,j)=psa3(i,j,1)/1.01325*1.0E-5
#endif
!lyc 2014.09.11
#ifdef COUP
          iceday=ifrac
#else
          iceday=seaice
#endif

!---------------------------------------------------------------------
!     xu, calculation of change in PCO2A with time
!     pCO2  in the atmosphere
!---------------------------------------------------------------------
!     pco2dry from INTFOR_PT.F90 from yearly mean values
!---------------------------------------------------------------------
#ifdef COUP
!lyc 2017.1.23       pco2dry=pco2(1,1)
       pco2dry2=pco2 !for 2D-pCO2A
 !      pco2s=284.725*1.0E-6
!lyc 2017.1.23       pco2dry=284.725  !for instant purpose
     !  pco2dry2=284.725  !for instant purpose
     if(mytid==0) write(6,*) 'the atmospheric carbon dioxide concentration(ppm) :', pco2dry2(3,3)
#endif

!$OMP PARALLEL DO PRIVATE (I,J,T0,ES)
      DO J=1,JMT
        DO I=1,IMT
          IF(VIT(i,j,1) < 0.5) CYCLE
          T0 = 273.15 + AT(I,J,1,1)
          ES = EXP(20.1050 - 9.7982E-3*T0 - 6163.10/T0)
#ifdef COUP
          pco2a(I,J) = pco2dry2 * (1.0 - ES)!*pressureday
#else
          pco2a(I,J) = pco2dry * (1.0 - ES)
#endif
        ENDDO
      ENDDO
!---------------------------------------------------------------------
!     PCO2O: PARTIAL PRESSURE OF CO2-OCEAN (uatm)
!---------------------------------------------------------------------
!*********************************************************
!-------------------------------------------
      DO J=1,JMT
        DO I=1,IMT
          tt(i,j)=at(i,j,1,1)
          ss(i,j)=at(i,j,1,2)
          tctmp(i,j)=pt(i,j,1,1)
!----------------------------------------------------------------
! xu, Calculation of total alkalinity (for inorganic carbon cycle)
!------------------------------------------
!--------------------------
          tatmp(i,j)=ptb(i,j,1,4)
        ENDDO
      ENDDO
!
!---------------------------------------------------------
         CALL PCO2(pco2o,tatmp,tctmp,tt,ss)
!---------------------------------------------------------------------
!     call SGEC.F90 to calculate the exchange coefficients of co2
!---------------------------------------------------------------------
                    CALL SGEC
!---------------------------------------------------------------------
!     FIND CO2-FLUX FROM ATMOSPHERE TO OCEAN
!     PCODRY: PCO2-DRY ATMOSPHERE (PPM)
!     PCO2A: PARTIAL PRESSURE OF CO2-WET ATMOSPHERE (PPM)
!     ssfc in units of m/s *umol/kg (in LICOM)
!---------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE (I,J)
      DO J=1,JMT
        DO I=1,IMT
          dpco2o(I,J)=(pco2a(I,J)-pco2o(I,J))*VIT(I,J,1)
!
!----------------------------------------------
!       calculation of flux, ssfc
!--------------------------------------------------------
          ssfc(I,J) =(1.0-iceday(i,j))*sge(I,J)*dpco2o(I,J)
!---------------------------------------------------------
!xu for testing
!-------------
!             ssfc(i,j)=1.0e-5
!------------------------------
        ENDDO
      ENDDO
#ifdef COUP
       if(ii==1.or.ii==nss/ncpl+1) uptake(i,j)=0.0
!        if(rice(i,j)/=1.0) then
!        uptake(i,j)=uptake(i,j)+ssfc(i,j)*vit(i,j,1) *1025*10E-9*molco2/(1-rice(i,j))
!        endif
       uptake(i,j)=uptake(i,j)+ssfc(i,j)*dxdyt(j)*vit(i,j,1)*1025.0*1.0E-6
#endif
         ENDDO
      ENDDO
!
!$OMP PARALLEL DO PRIVATE(I,J,FACTX)
      DO J=1,JMT
        FACTX=DXDYT(J)*unitc*DTS
        DO I=1,IMT
          totup(I,J)=totup(I,J)+ssfc(i,j)*FACTX*VIT(I,J,1)
          tpco2o(I,J)=tpco2o(I,J)+pco2o(i,j)
          tdpco2o(I,J)=tdpco2o(I,J)+dpco2o(i,j)
        ENDDO
      ENDDO


      RETURN
      END SUBROUTINE FLUX_PT
