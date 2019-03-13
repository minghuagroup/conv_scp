      SUBROUTINE GG2LL(MP,MB,IMN,IXN,JMN,JXN,CJN,LN,FN, 
     &                 IMO,IXO,JMO,JXO,CIO,CJO,DIO,DJO,LO,FO, 
     &                 sjn,wjn,cjn1,ajn,ajo,xio,xjo,wio1,wio2,jpo, 
     &                 fpx,wpx,wtx,fp0x,PK,PKM1) 
C 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM:    GG2LL       INTERPOLATE GAUSSIAN TO LAT-LON GRID. 
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31 
C 
C ABSTRACT: A HORIZONTAL FIELD IS BILINEARLY INTERPOLATED 
C   FROM A GLOBAL GAUSSIAN GRID TO A LATITUDE-LONGITUDE GRID. 
C   THE GAUSSIAN LATITUDES MAY BE PASSED OR CALCULATED. 
C   THE INPUT GLOBAL FIELD IS ASSUMED TO RUN EASTWARD AND SOUTHWARD, 
C   STARTING AT GREENWICH AND THE NORTHERNMOST LATITUDE, 
C   BUT THE OUTPUT FIELD ORIENTATION IS DEFINED BY PASSED ARGUMENTS. 
C   THE FIELD MAY BE TAGGED A SCALAR, A VECTOR, A FLAG OR A BUDGET. 
C   A FLAG FIELD SUCH AS THE LAND-SEA MASK IS NOT INTERPOLATED 
C   BUT TAKEN FROM THE VALUES AT THE CLOSEST INPUT GRIDPOINTS. 
C   A BUDGET FIELD SUCH AS PRECIPITATION IS INTERPOLATED 
C   WHILE PRESERVING THE AREA INTEGRALS OF THE ORIGINAL FIELD. 
C   POLAR SCALARS ARE THE AVERAGE OF THE CLOSEST LATITUDE CIRCLE VALUES. 
C   POLAR VECTOR COMPONENTS ARE TAKEN FROM THE WAVENUMBER 1 COMPONENT 
C   EXTRACTED FROM THE VALUES ON THE CLOSEST LATITUDE CIRCLE. 
C   A LOGICAL BITMAP MAY BE PASSED TO MASK OUT PART OF THE INPUT FIELD. 
C   IN THIS CASE, AN APPROPRIATE OUTPUT BITMAP IS CONSTRUCTED AND 
C   THE OUTPUT FIELD IS COMPUTED ONLY WHERE THE OUTPUT BITMAP IS TRUE 
C   AND ONLY USING DATA WHERE THE INPUT BITMAP IS TRUE. 
C   WHERE THE OUTPUT BITMAP IS FALSE, THE OUTPUT IS SET TO ZERO. 
C   THE BITMAP MAY ALTERNATIVELY BE CONSIDERED AS A TWO-WAY MASK, 
C   IN WHICH CASE OUTPUT FIELD IS ALSO COMPUTED WHERE THE OUTPUT BITMAP 
C   IS FALSE ONLY USING DATA WHERE THE INPUT BITMAP IS FALSE. 
C   THE TWO-WAY MASK OPTION SHOULD NOT BE USED WITH VECTOR COMPONENTS. 
C 
C PROGRAM HISTORY LOG: 
C   92-10-31  IREDELL 
C   93-10-21  IREDELL   ALLOW VECTORS, FLAGS, BITMAP, NORTH/SOUTH FLIP 
C   94-12-05  IREDELL   ALLOW BUDGETS, OPTIONALLY CALCULATE LATITUDES, 
C                       PRECOMPUTE INDICES, GENERALIZE OUTPUT DOMAIN. 
C   95-07-11  IREDELL   REWEIGHT VECTOR INTERPOLATION NEAR POLE. 
C 
C USAGE:    CALL GG2LL(MP,MB,IMN,IXN,JMN,JXN,CJN,LN,FN, 
C    &                 IMO,IXO,JMO,JXO,CIO,CJO,DIO,DJO,LO,FO) 
C   INPUT ARGUMENT LIST: 
C     MP       - INTEGER FIELD PARAMETER IDENTIFIER 
C                (0 FOR SCALAR, 1 FOR VECTOR, 2 FOR FLAG, 3 FOR BUDGET) 
C     MB       - INTEGER BITMAP IDENTIFIER 
C                (0 FOR NO BITMAP, 1 TO INTERPOLATE BITMAP, 
C                 2 TO ALSO INTERPOLATE DATA TO BITMAP FALSES) 
C     IMN      - INTEGER INPUT LONGITUDE DIMENSION 
C     IXN      - INTEGER NUMBER TO SKIP BETWEEN INPUT LONGITUDES 
C     JMN      - INTEGER INPUT LATITUDE DIMENSION (EVEN) 
C     JXN      - INTEGER NUMBER TO SKIP BETWEEN INPUT LATITUDES 
C     CJN      - real*4 (JMN/2) GAUSSIAN LATITUDES IN DEGREES NORTH 
C                (IF CJN(1)=0., THE LATITUDES ARE CALCULATED.) 
C     LN       - LOGICAL ((IMN-1)*IXN+(JMN-1)*JXN+1) BITMAP IF MB=1 
C     FN       - real*4 ((IMN-1)*IXN+(JMN-1)*JXN+1) FIELD TO INTERPOLATE 
C     IMO      - INTEGER OUTPUT LONGITUDE DIMENSION 
C     IXO      - INTEGER NUMBER TO SKIP BETWEEN OUTPUT LONGITUDES 
C     JMO      - INTEGER OUTPUT LATITUDE DIMENSION 
C     JXO      - INTEGER NUMBER TO SKIP BETWEEN OUTPUT LATITUDES 
C     CIO      - real*4 START LONGITUDE IN DEGREES EAST 
C     CJO      - real*4 START LATITUDE IN DEGREES NORTH 
C     DIO      - real*4 LONGITUDE INCREMENT IN DEGREES EAST 
C     DJO      - real*4 LATITUDE INCREMENT IN DEGREES NORTH 
C 
C   OUTPUT ARGUMENT LIST: 
C     LO       - LOGICAL ((IMO-1)*IXO+(JMO-1)*JXO+1) BITMAP IF MB=1 
C     FO       - real*4 ((IMO-1)*IXO+(JMO-1)*JXO+1) INTERPOLATED FIELD 
C 
C SUBPROGRAMS CALLED: 
C   GLAT         COMPUTE GAUSSIAN LATITUDES 
C 
C ATTRIBUTES: 
C   LANGUAGE: CRAY FORTRAN 77 
C 
C$$$ 

      real*4 CJN(JMN/2) 
      real*4 FN((IMN-1)*IXN+(JMN-1)*JXN+1) 
      real*4 FO((IMO-1)*IXO+(JMO-1)*JXO+1) 
      LOGICAL LN((IMN-1)*IXN+(JMN-1)*JXN+1) 
      LOGICAL LO((IMO-1)*IXO+(JMO-1)*JXO+1) 
      real*4 SJN(JMN/2),WJN(JMN/2),CJN1(JMN/2) 
      real*4 PK(JMN/2),PKM1(JMN/2) 
      real*4 AJN(JMN+1),AJO(JMO+1),XIO(IMO+1),XJO(JMO+1) 
      real*4 WIO1(IMO),WIO2(IMO) 
      INTEGER JPO(JMO) 
      real*4 FPX(IMO),FP0X(IMO),WPX(IMO),WTX(IMO) 
      IJN(I,J)=(I-1)*IXN+(J-1)*JXN+1 
      IJO(I,J)=(I-1)*IXO+(J-1)*JXO+1 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  PRECOMPUTE INDICES 
      PI=ACOS(-1.) 
      IF(CJN(1).EQ.0.) THEN 
C        write(*,*) '****No Gaussian latitudes, calculate them...' 
        CALL GLAT(JMN/2,SJN,WJN,PK,PKM1) 
        DO J1=1,JMN/2 
          CJN1(J1)=(180/PI)*ASIN(SJN(J1)) 
          cjn(j1)=cjn1(j1) 
        ENDDO 
      ELSE 
        DO J1=1,JMN/2 
          CJN1(J1)=CJN(J1) 
        ENDDO 
      ENDIF 
      DO J=1,JMO 
        JPO(J)=0 
      ENDDO 
      IF(MP.NE.3) THEN 
C  SCALAR OR VECTOR OR FLAG 
        DO I=1,IMO 
          XIO(I)=MOD((CIO+(I-1)*DIO)/360*IMN+2*IMN,FLOAT(IMN))+1     !zhh
        ENDDO 
        IF(MP.EQ.0) THEN 
          DO I=1,IMO 
            I1=XIO(I) 
            WIO1(I)=I1+1-XIO(I) 
            WIO2(I)=XIO(I)-I1 
          ENDDO 
        ELSEIF(MP.EQ.1) THEN 
          DL=2*PI/IMN 
          DO I=1,IMO 
            I1=XIO(I) 
            WIO1(I)=SIN((I1+1-XIO(I))*DL)/SIN(DL) 
            WIO2(I)=SIN((XIO(I)-I1)*DL)/SIN(DL) 
          ENDDO 
        ENDIF 
CDIR$ IVDEP 
        DO J1=1,JMN/2 
          AJN(J1)=CJN1(J1) 
          AJN(JMN+1-J1)=-CJN1(J1) 
        ENDDO 
        DO J=1,JMO 
          AJO(J)=CJO+(J-1)*DJO 
        ENDDO 
        IF(DJO.LT.0.) THEN 
          JB=1 
          JE=JMO 
          JI=1 
        ELSE 
          JB=JMO 
          JE=1 
          JI=-1 
        ENDIF 
        J1=1 
        DO J=JB,JE,JI 
          IF(AJO(J).GE.AJN(1)) THEN 
            XJO(J)=1 
          ELSEIF(AJO(J).LE.AJN(JMN)) THEN 
            XJO(J)=JMN 
          ELSE 
            DO WHILE(AJO(J).LT.AJN(J1+1)) 
              J1=J1+1 
            ENDDO 
            XJO(J)=J1+(AJN(J1)-AJO(J))/(AJN(J1)-AJN(J1+1)) 
          ENDIF 
        ENDDO 
        IF(XJO(1).EQ.1.) THEN 
          JPO(1)=1 
!!        ELSEIF(XJO(1).EQ.DBLE(JMN)) THEN       !zhh  2007.6.7
        ELSEIF(XJO(1).EQ.FLOAT(JMN)) THEN       
          JPO(1)=JMN 
        ENDIF 
        IF(XJO(JMO).EQ.1.) THEN 
          JPO(JMO)=1 
!!        ELSEIF(XJO(JMO).EQ.DBLE(JMN)) THEN     !zhh  2007.6.7
        ELSEIF(XJO(JMO).EQ.FLOAT(JMN)) THEN     
          JPO(JMO)=JMN 
        ENDIF 
      ELSE 
C  BUDGET 
        DO I=1,IMO+1 
          XIO(I)=MOD((CIO+(I-1.5)*DIO)/360*IMN+2*IMN+0.5,FLOAT(IMN))+1   !zhh  
        ENDDO 
        IBX=0 
        DO I=1,IMO 
          XIB=XIO(INT(I+0.5-SIGN(0.5,DIO))) 
          XIE=XIO(INT(I+0.5+SIGN(0.5,DIO))) 
          IB=XIB 
          IE=XIE 
          IF(IE.LT.IB) IE=IE+IMN 
          IBX=MAX(IBX,IE-IB+1) 
        ENDDO 
        AJN(1)=1. 
        AJN(JMN/2+1)=0. 
        AJN(JMN+1)=-1. 
CDIR$ IVDEP 
        DO J1=2,JMN/2 
          AJN(J1)=SIN((PI/180)*0.5*(CJN1(J1-1)+CJN1(J1))) 
          AJN(JMN+2-J1)=-AJN(J1) 
        ENDDO 
        DO J=1,JMO+1 
          PJO=CJO+(J-1.5)*DJO 
          IF(PJO.GE.90.) THEN 
            AJO(J)=1. 
          ELSEIF(PJO.LE.-90.) THEN 
            AJO(J)=-1. 
          ELSE 
            AJO(J)=SIN((PI/180)*PJO) 
          ENDIF 
        ENDDO 
        IF(DJO.LT.0.) THEN 
          JB=1 
          JE=JMO+1 
          JI=1 
        ELSE 
          JB=JMO+1 
          JE=1 
          JI=-1 
        ENDIF 
        J1=1 
        DO J=JB,JE,JI 
          IF(AJO(J).GE.AJN(1)) THEN 
            XJO(J)=1 
          ELSEIF(AJO(J).LE.AJN(JMN+1)) THEN 
            XJO(J)=JMN+1 
          ELSE 
            DO WHILE(AJO(J).LT.AJN(J1+1)) 
              J1=J1+1 
            ENDDO 
            XJO(J)=J1+(AJN(J1)-AJO(J))/(AJN(J1)-AJN(J1+1)) 
          ENDIF 
        ENDDO 
        IF(XJO(1).EQ.1.) THEN 
          JPO(1)=1 
!!        ELSEIF(XJO(1).EQ.DBLE(JMN)) THEN      !zhh  2007.6.7
        ELSEIF(XJO(1).EQ.FLOAT(JMN)) THEN      
          JPO(1)=JMN 
        ENDIF 
        IF(XJO(JMO+1).EQ.1.) THEN 
          JPO(JMO)=1 
!!        ELSEIF(XJO(JMO+1).EQ.DBLE(JMN+1)) THEN   !zhh  2007.6.7
        ELSEIF(XJO(JMO+1).EQ.FLOAT(JMN+1)) THEN   
          JPO(JMO)=JMN 
        ENDIF 
      ENDIF 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  GET POLAR VALUES 
      DO J=1,JMO,JMO-1 
        IF(JPO(J).GT.0) THEN 
          IF(MB.EQ.0) THEN 
            IF(MP.EQ.0) THEN 
C  FULL SCALAR 
              J1=JPO(J) 
              FP=0. 
              DO I=1,IMN 
                FP=FP+FN(IJN(I,J1)) 
              ENDDO 
              FP=FP/IMN 
              DO I=1,IMO 
                FO(IJO(I,J))=FP 
              ENDDO 
            ELSEIF(MP.EQ.1) THEN 
C  FULL VECTOR 
              J1=JPO(J) 
              FPC=0. 
              FPS=0. 
              DO I=1,IMN 
                CI=COS(2*PI*(I-1)/IMN) 
                SI=SIN(2*PI*(I-1)/IMN) 
                FPC=FPC+CI*FN(IJN(I,J1)) 
                FPS=FPS+SI*FN(IJN(I,J1)) 
              ENDDO 
              FPC=2*FPC/IMN 
              FPS=2*FPS/IMN 
              DO I=1,IMO 
                CI=COS(2*PI*(I-1)/IMO) 
                SI=SIN(2*PI*(I-1)/IMO) 
                FO(IJO(I,J))=FPC*CI+FPS*SI 
              ENDDO 
            ELSEIF(MP.EQ.2) THEN 
C  FULL FLAG 
              J1=JPO(J) 
              DO I=1,IMO 
                FO(IJO(I,J))=FN(IJN(1,J1)) 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  FULL BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              FP=0. 
              WP=0. 
              DO J1=JB,JE 
                W=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) W=W*WJB 
                IF(J1.EQ.JE) W=W*WJE 
                DO I=1,IMN 
                  FP=FP+W*FN(IJN(I,J1)) 
                ENDDO 
                WP=WP+W*IMN 
              ENDDO 
              DO I=1,IMO 
                FO(IJO(I,J))=FP/WP 
              ENDDO 
            ENDIF 
          ELSE 
            IF(MP.EQ.0) THEN 
C  BITMAP OR TWOWAY SCALAR 
              J1=JPO(J) 
              FP=0. 
              WP=0. 
              IF(MB.EQ.2) FP0=0. 
              DO I=1,IMN 
                IF(LN(IJN(I,J1))) THEN 
                  FP=FP+FN(IJN(I,J1)) 
                  WP=WP+1. 
                ELSEIF(MB.EQ.2) THEN 
                  FP0=FP0+FN(IJN(I,J1)) 
                ENDIF 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=WP.GE.0.5*IMN 
              ENDDO 
              IF(LO(IJO(1,J))) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP/WP 
                ENDDO 
              ELSEIF(MB.EQ.2.AND.WP.LT.IMN) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP0/(IMN-WP) 
                ENDDO 
              ELSE 
                DO I=1,IMO 
                  FO(IJO(I,J))=0. 
                ENDDO 
              ENDIF 
            ELSEIF(MP.EQ.1) THEN 
C  BITMAP VECTOR 
              J1=JPO(J) 
              IP=0 
              DO I=1,IMN 
                IF(LN(IJN(I,J1))) IP=IP+1 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=IP.EQ.IMN 
              ENDDO 
              IF(LO(IJO(1,J))) THEN 
                FPC=0. 
                FPS=0. 
                DO I=1,IMN 
                  CI=COS(2*PI*(I-1)/IMN) 
                  SI=SIN(2*PI*(I-1)/IMN) 
                  FPC=FPC+CI*FN(IJN(I,J1)) 
                  FPS=FPS+SI*FN(IJN(I,J1)) 
                ENDDO 
                FPC=2*FPC/IMN 
                FPS=2*FPS/IMN 
                DO I=1,IMO 
                  CI=COS(2*PI*(I-1)/IMO) 
                  SI=SIN(2*PI*(I-1)/IMO) 
                  FO(IJO(I,J))=FPC*CI+FPS*SI 
                ENDDO 
              ELSE 
                DO I=1,IMO 
                  FO(IJO(I,J))=0. 
                ENDDO 
              ENDIF 
            ELSEIF(MP.EQ.2) THEN 
C  BITMAP OR TWOWAY FLAG 
              J1=JPO(J) 
              DO I=1,IMO 
                LO(IJO(I,J))=LN(IJN(1,J1)) 
                IF(LN(IJN(1,J1)).OR.MB.EQ.2) THEN 
                  FO(IJO(I,J))=FN(IJN(1,J1)) 
                ELSE 
                  FO(IJO(I,J))=0. 
                ENDIF 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  BITMAP OR TWOWAY BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              FP=0. 
              WP=0. 
              WT=0. 
              IF(MB.EQ.2) THEN 
                FP0=0. 
              ENDIF 
              DO J1=JB,JE 
                W=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) W=W*WJB 
                IF(J1.EQ.JE) W=W*WJE 
                DO I=1,IMN 
                  IF(LN(IJN(I,J1))) THEN 
                    FP=FP+W*FN(IJN(I,J1)) 
                    WP=WP+W 
                  ELSEIF(MB.EQ.2) THEN 
                    FP0=FP0+W*FN(IJN(I,J1)) 
                  ENDIF 
                ENDDO 
                WT=WT+W*IMN 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=WP.GE.0.5*WT 
              ENDDO 
              IF(LO(IJO(1,J))) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP/WP 
                ENDDO 
              ELSEIF(MB.EQ.2.AND.WP.LT.WT) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP0/(WT-WP) 
                ENDDO 
              ELSE 
                DO I=1,IMO 
                  FO(IJO(I,J))=0. 
                ENDDO 
              ENDIF 
            ENDIF 
          ENDIF 
        ENDIF 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  INTERPOLATE THE REST OF THE FIELD 
      DO J=1,JMO 
        IF(JPO(J).EQ.0) THEN 
          IF(MB.EQ.0) THEN 
            IF(MP.EQ.0.OR.MP.EQ.1) THEN 
C  FULL SCALAR OR VECTOR 
              J1=XJO(J) 
              J2=MIN(J1+1,JMN) 
              WJ2=XJO(J)-J1 
              WJ1=1.-WJ2 
              DO I=1,IMO 
                I1=XIO(I) 
                I2=MOD(I1,IMN)+1 
                WI1=WIO1(I) 
                WI2=WIO2(I) 
                W11=WI1*WJ1 
                W21=WI2*WJ1 
                W12=WI1*WJ2 
                W22=WI2*WJ2 
                FO(IJO(I,J))=W11*FN(IJN(I1,J1))+W21*FN(IJN(I2,J1))+ 
     &                       W12*FN(IJN(I1,J2))+W22*FN(IJN(I2,J2)) 
              ENDDO 
            ELSEIF(MP.EQ.2) THEN 
C  FULL FLAG 
              J1=NINT(XJO(J)) 
              DO I=1,IMO 
                I1=MOD(NINT(XIO(I))-1,IMN)+1 
                FO(IJO(I,J))=FN(IJN(I1,J1)) 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  FULL BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              DO I=1,IMO 
                FPX(I)=0. 
                WPX(I)=0. 
              ENDDO 
              DO J1=JB,JE 
                WJ=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) WJ=WJ*WJB 
                IF(J1.EQ.JE) WJ=WJ*WJE 
                DO I1X=1,IBX 
                  DO I=1,IMO 
                    XIB=XIO(INT(I+0.5-SIGN(0.5,DIO))) 
                    XIE=XIO(INT(I+0.5+SIGN(0.5,DIO))) 
                    IB=XIB 
                    IE=XIE 
                    WIB=IB+1-XIB 
                    WIE=XIE-IE 
                    IF(IE.LT.IB) IE=IE+IMN 
                    I1=IB+(I1X-1) 
                    IF(I1.LE.IE) THEN 
                      W=WJ 
                      IF(I1.EQ.IB) W=W*WIB 
                      IF(I1.EQ.IE) W=W*WIE 
                      I1M=MOD(I1-1,IMN)+1 
                      FPX(I)=FPX(I)+W*FN(IJN(I1M,J1)) 
                      WPX(I)=WPX(I)+W 
                    ENDIF 
                  ENDDO 
                ENDDO 
              ENDDO 
              DO I=1,IMO 
                FO(IJO(I,J))=FPX(I)/WPX(I) 
              ENDDO 
            ENDIF 
          ELSE 
            IF(MP.EQ.0.OR.MP.EQ.1) THEN 
C  BITMAP OR TWOWAY SCALAR OR VECTOR 
              J1=XJO(J) 
              J2=MIN(J1+1,JMN) 
              WJ2=XJO(J)-J1 
              WJ1=1.-WJ2 
              DO I=1,IMO 
                I1=XIO(I) 
                I2=MOD(I1,IMN)+1 
                WI1=WIO1(I) 
                WI2=WIO2(I) 
                W11=WI1*WJ1 
                W21=WI2*WJ1 
                W12=WI1*WJ2 
                W22=WI2*WJ2 
                WP=0. 
                IJ11=IJN(I1,J1) 
                IJ21=IJN(I2,J1) 
                IJ12=IJN(I1,J2) 
                IJ22=IJN(I2,J2) 
                IF(LN(IJ11)) WP=WP+W11 
                IF(LN(IJ21)) WP=WP+W21 
                IF(LN(IJ12)) WP=WP+W12 
                IF(LN(IJ22)) WP=WP+W22 
                WT=W11+W21+W12+W22 
                IJ=IJO(I,J) 
                LO(IJ)=WP.GE.0.5*WT 
                IF(LO(IJ)) THEN 
                  FP=0. 
                  IF(LN(IJ11)) FP=FP+W11*FN(IJ11) 
                  IF(LN(IJ21)) FP=FP+W21*FN(IJ21) 
                  IF(LN(IJ12)) FP=FP+W12*FN(IJ12) 
                  IF(LN(IJ22)) FP=FP+W22*FN(IJ22) 
                  FO(IJ)=FP*WT/WP 
                ELSEIF(MB.EQ.2.AND.WP.LT.WT) THEN 
                  FP=0. 
                  IF(.NOT.LN(IJ11)) FP=FP+W11*FN(IJ11) 
                  IF(.NOT.LN(IJ21)) FP=FP+W21*FN(IJ21) 
                  IF(.NOT.LN(IJ12)) FP=FP+W12*FN(IJ12) 
                  IF(.NOT.LN(IJ22)) FP=FP+W22*FN(IJ22) 
                  FO(IJ)=FP*WT/(WT-WP) 
                ELSE 
                  FO(IJ)=0. 
                ENDIF 
              ENDDO 
            ELSEIF(MP.EQ.2) THEN 
C  BITMAP OR TWOWAY FLAG 
              J1=NINT(XJO(J)) 
              DO I=1,IMO 
                I1=MOD(NINT(XIO(I))-1,IMN)+1 
                LO(IJO(I,J))=LN(IJN(I1,J1)) 
                IF(LO(IJO(I,J)).OR.MB.EQ.2) THEN 
                  FO(IJO(I,J))=FN(IJN(I1,J1)) 
                ELSE 
                  FO(IJO(I,J))=0. 
                ENDIF 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  BITMAP OR TWOWAY BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              DO I=1,IMO 
                FPX(I)=0. 
                IF(MB.EQ.2) FP0X(I)=0. 
                WPX(I)=0. 
                WTX(I)=0. 
              ENDDO 
              DO J1=JB,JE 
                WJ=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) WJ=WJ*WJB 
                IF(J1.EQ.JE) WJ=WJ*WJE 
                DO I1X=1,IBX 
                  DO I=1,IMO 
                    XIB=XIO(INT(I+0.5-SIGN(0.5,DIO))) 
                    XIE=XIO(INT(I+0.5+SIGN(0.5,DIO))) 
                    IB=XIB 
                    IE=XIE 
                    WIB=IB+1-XIB 
                    WIE=XIE-IE 
                    IF(IE.LT.IB) IE=IE+IMN 
                    I1=IB+(I1X-1) 
                    IF(I1.LE.IE) THEN 
                      W=WJ 
                      IF(I1.EQ.IB) W=W*WIB 
                      IF(I1.EQ.IE) W=W*WIE 
                      I1M=MOD(I1-1,IMN)+1 
                      IF(LN(IJN(I1M,J1))) THEN 
                        FPX(I)=FPX(I)+W*FN(IJN(I1M,J1)) 
                        WPX(I)=WPX(I)+W 
                      ELSEIF(MB.EQ.2) THEN 
                        FP0X(I)=FP0X(I)+W*FN(IJN(I1M,J1)) 
                      ENDIF 
                      WTX(I)=WTX(I)+W 
                    ENDIF 
                  ENDDO 
                ENDDO 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=WPX(I).GE.0.5*WTX(I) 
                IF(LO(IJO(I,J))) THEN 
                  FO(IJO(I,J))=FPX(I)/WPX(I) 
                ELSEIF(MB.EQ.2.AND.WPX(I).LT.WTX(I)) THEN 
                  FO(IJO(I,J))=FP0X(I)/(WTX(I)-WPX(I)) 
                ELSE 
                  FO(IJO(I,J))=0. 
                ENDIF 
              ENDDO 
            ENDIF 
          ENDIF 
        ENDIF 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN 
      END 

C----------------------------------------------------------------------- 
      SUBROUTINE GLAT(JH,SLAT,WLAT,PK,PKM1) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM:    GLAT        COMPUTE GAUSSIAN LATITUDE FUNCTIONS 
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31 
C 
C ABSTRACT: COMPUTES SINES OF GAUSSIAN LATITUDE BY ITERATION. 
C           THE GAUSSIAN WEIGHTS ARE ALSO COMPUTED. 
C 
C PROGRAM HISTORY LOG: 
C   91-10-31  MARK IREDELL 
C 
C USAGE:    CALL GLAT(JH,SLAT,WLAT) 
C 
C   INPUT ARGUMENT LIST: 
C     JH       - INTEGER NUMBER OF GAUSSIAN LATITUDES IN A HEMISPHERE 
C 
C   OUTPUT ARGUMENT LIST: 
C     SLAT     - real*4 (JH) SINES OF (POSITIVE) GAUSSIAN LATITUDE 
C     WLAT     - real*4 (JH) GAUSSIAN WEIGHTS FOR THE NH 
C 
C ATTRIBUTES: 
C   LANGUAGE: CRAY FORTRAN 
C 
C$$$ 
c
czhh      DIMENSION SLAT(JH),WLAT(JH) 
      real*4 SLAT(JH),WLAT(JH)       !zhh 2007.6.6
c     PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25,EPS=1.E-14) 
      PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25,EPS=1.E-7) 
      PARAMETER(JBZ=50) 
czhh      DIMENSION PK(JH),PKM1(JH),BZ(JBZ) 
      real*4 PK(JH),PKM1(JH),BZ(JBZ) 
      DATA BZ        / 2.4048255577,  5.5200781103, 
     $  8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, 
     $ 21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, 
     $ 33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, 
     $ 46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, 
     $ 58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, 
     $ 71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, 
     $ 84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, 
     $ 96.6052679510, 99.7468198587, 102.888374254, 106.029930916, 
     $ 109.171489649, 112.313050280, 115.454612653, 118.596176630, 
     $ 121.737742088, 124.879308913, 128.020877005, 131.162446275, 
     $ 134.304016638, 137.445588020, 140.587160352, 143.728733573, 
     $ 146.870307625, 150.011882457, 153.153458019, 156.295034268 / 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  ESTIMATE LATITUDES USING BESSEL FUNCTION 
      R=1./SQRT((2*JH+0.5)**2+C) 
      DO J=1,MIN(JH,JBZ) 
        SLAT(J)=COS(BZ(J)*R) 
      ENDDO 
      DO J=JBZ+1,JH 
        SLAT(J)=COS((BZ(JBZ)+(J-JBZ)*PI)*R) 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  CONVERGE UNTIL ALL SINES OF GAUSSIAN LATITUDE ARE WITHIN EPS 
      SPMAX=1. 
      DO WHILE(SPMAX.GT.EPS) 
        SPMAX=0. 
        DO J=1,JH 
          PKM1(J)=1. 
          PK(J)=SLAT(J) 
        ENDDO 
        DO N=2,2*JH 
          DO J=1,JH 
            PKM2=PKM1(J) 
            PKM1(J)=PK(J) 
            PK(J)=((2*N-1)*SLAT(J)*PKM1(J)-(N-1)*PKM2)/N 
          ENDDO 
        ENDDO 
        DO J=1,JH 
          SP=PK(J)*(1.-SLAT(J)**2)/(2*JH*(PKM1(J)-SLAT(J)*PK(J))) 
          SLAT(J)=SLAT(J)-SP 
          SPMAX=MAX(SPMAX,ABS(SP)) 
        ENDDO 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  COMPUTE COSINES AND GAUSSIAN WEIGHTS 
      DO J=1,JH 
        WLAT(J)=2.*(1.-SLAT(J)**2)/(2*JH*PKM1(J))**2 
      ENDDO 
      RETURN 
      END 
