SUBROUTINE INTPAV(FIN,PIN,KIN,FOT,POT,KOT)
!------------------------------------------------------------------------------------------------
! Popurse: INTERPOLATE FIN(KIN) INTO FOT(KOT) VERTICALLY
!          weighted by pressure to conserve mass vertical integration 
! Original version : INTPAV.f (IAP 9L)
! Reconstructed   : ZhangHe
! Completed : 2005.9.7
! Reference : see note P93
!------------------------------------------------------------------------------------------------
!!   use precision
!!   use Dyn_grid 

   implicit none
!------------------------------------Arguments--------------------------------------------------
   integer, intent(in)  :: KIN        ! vertical index of needing interpolation variable FIN   
   real*8,  intent(in)  :: FIN(KIN)   ! input variable needed to interpolate
   real*8,  intent(in)  :: PIN(*)     !
   real*8,  intent(in)  :: POT(*)     ! vertical layers need to interpolate to          
   integer, intent(in)  :: KOT        ! number of vertical layers of FOT
   real*8,  intent(out) :: FOT(KOT)   ! output variable been interpolated to p level
!----------------------------------Local workspace----------------------------------------------
   real*8  :: PINS, POTJ, PINI, PINJ, POTI, SFP
   integer :: KIP, K, KK, LVH, LVL, L, LVP, N
!------------------------------------------------------------------------------------------------

   KIP             = KIN + 1
   PINS            = PIN(KIP)
   LVH             = 1
   DO K = 1 ,KOT
      KK           = K + 1
      POTJ         = POT(KK)
!! it doesn't matter when KK exceed the input POT array domain. 
!     FIND OUT THE NEAREST LOWER INTERFACE
      IF ( POTJ.GE.PINS ) THEN
         LVL       = KIN
      ELSE
         DO L = LVH, KIN
            LVL    = L
            PINI   = PIN(L)
            PINJ   = PIN(L+1)
            IF ( POTJ.GE.PINI.AND.POTJ.LT.PINJ ) THEN
               GOTO 20
            ENDIF
         END DO
      ENDIF
20    IF ( LVL.EQ.LVH ) THEN
         FOT(K)    = FIN(LVH)
      ELSE
!       DO INTERPOLATION WITH PRESSURE WEIGHTED
         POTI      = POT(K)
         LVP       = LVH + 1
         SFP       = (PIN(LVP)-POTI)*FIN(LVH) + (POTJ-PIN(LVL))*FIN(LVL)            
         IF ( LVL.GT.LVP ) THEN
            DO N = LVP, LVL-1
               SFP = SFP + (PIN(N+1)-PIN(N))*FIN(N)
            END DO
         ENDIF
         FOT(K)    = SFP / (POTJ - POTI)
!       RENEW THE NEAREST HIGHER INTERFACE FOR NEXT LAYER
         LVH       = LVL
      ENDIF
   END DO

   RETURN
END
