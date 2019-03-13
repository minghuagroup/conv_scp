SUBROUTINE INTLAY(FIN,ZIN,KIN,FOT,ZOT,KOT)
!------------------------------------------------------------------------------------------------
! Purpose: linear INTERPOLATE FIN(KIN) INTO FOT(KOT)   VERTICALLY
! Original version : INTLAY.f (IAP 9L)
! Reconstructed    : ZhangHe
! Completed : 2005.9.7
!------------------------------------------------------------------------------------------------
!!   use precision
  
   implicit none
!------------------------------------Arguments--------------------------------------------------
   integer, intent(in)  :: KIN        ! vertical index of needing interpolation variable FIN
   integer, intent(in)  :: KOT        ! vertical index of interpolated variable FOT
   real*8,  intent(in)  :: ZIN(KIN)   ! vertical location of needing interpolation variable FIN
   real*8,  intent(in)  :: ZOT(KOT)   ! vertical location of interpolated variable FOT
   real*8,  intent(in)  :: FIN(KIN)   ! input variable needed to interpolate
   real*8,  intent(out) :: FOT(KOT)   ! output variable been interpolated
!
!----------------------------------Local workspace----------------------------------------------
   real*8 :: ZIN1, ZIN2, ZIN3, ZIN4
   real*8 :: FIN1, FIN2, FIN3, FIN4
   real*8 :: ZINI, ZINJ
   real*8 :: ZOTK
   integer :: KIM,K,L,II,IJ
!------------------------------------------------------------------------------------------------

   KIM        = KIN - 1
   ZIN1       = ZIN(1)
   ZIN2       = ZIN(2)
   ZIN3       = ZIN(KIM)
   ZIN4       = ZIN(KIN)
   FIN1       = FIN(1)
   FIN2       = FIN(2)
   FIN3       = FIN(KIM)
   FIN4       = FIN(KIN)
   DO K   = 1 ,KOT
      ZOTK        = ZOT(K)
      IF ( ZOTK.EQ.ZIN1 ) THEN
         FOT(K)   = FIN1
      ELSE IF ( ZOTK.LT.ZIN1 ) THEN
!       DO EXTRALPOLATION FOR THE UPPER BOUNDARY       ! see note P77  
         FOT(K)   =((ZIN2-ZOTK)*FIN1 - (ZIN1-ZOTK)*FIN2) / (ZIN2-ZIN1)         
      ELSE IF( ZOTK.EQ.ZIN4 ) THEN
         FOT(K)   = FIN4
      ELSE IF( ZOTK.GT.ZIN4 ) THEN
!        DO EXTRALPOLATION FOR THE LOWER BOUNDARY
         FOT(K)   =((ZOTK-ZIN3)*FIN4 - (ZOTK-ZIN4)*FIN3) / (ZIN4-ZIN3)         
      ELSE
!        FIND OUT THE NEAREST TWO POINTS
         DO L = 1 ,KIM
            II    = L
            IJ    = L + 1
            ZINI  = ZIN(II)
            ZINJ  = ZIN(IJ)
            IF ( ZOTK.GT.ZINI.AND.ZOTK.LE.ZINJ ) THEN
               GOTO 10
            ENDIF
         END DO
!      DO INTERPOLATION BETWEEN ZIN(II) & ZIN(IJ)
10       FOT(K)   =((ZINJ-ZOTK)*FIN(II) + (ZOTK-ZINI)*FIN(IJ)) / (ZINJ-ZINI)         
      ENDIF
   END DO
   RETURN
END

