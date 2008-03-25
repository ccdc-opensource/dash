!
!*****************************************************************************
!
      SUBROUTINE GET_LOGREF
!
! This routine relies on the tick marks file having been read in.
!
      USE ATMVAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      LOGICAL         IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0

      INTEGER         IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX

      INTEGER, EXTERNAL :: GETTIC
!     The following integers represent h,k,l,h+k,h+l,k+l and h+k+l
      INTEGER H_, K_, L_, HPK, HPL, KPL, HPKPL
!     The following integers represent the previous integers, divided by 2
!     and then multiplied by 2
      INTEGER H_m, K_m, L_m, HPKm, HPLm, KPLm, HPKPLm
      INTEGER IR, JHMAX, JHMIN, Item, IREMAIN, LL, LLM
      INTEGER IIMIN, IIMAX

      IHMIN = iHKL(1,1)
      IKMIN = iHKL(2,1)
      ILMIN = iHKL(3,1)
      IIMIN = -(iHKL(1,1) + iHKL(2,1))
      IHMAX = iHKL(1,1)
      IKMAX = iHKL(2,1)
      ILMAX = iHKL(3,1)
      IIMAX = -(iHKL(1,1) + iHKL(2,1))
      DO iR = 2, NumOfRef
        IHMIN = MIN(iHKL(1,iR),IHMIN)
        IKMIN = MIN(iHKL(2,iR),IKMIN)
        ILMIN = MIN(iHKL(3,iR),ILMIN)
        IHMAX = MAX(iHKL(1,iR),IHMAX)
        IKMAX = MAX(iHKL(2,iR),IKMAX)
        ILMAX = MAX(iHKL(3,iR),ILMAX)
! Now calculate 'i' index for hexagonals
        iTem = -(iHKL(1,iR) + iHKL(2,iR))
        IIMIN = MIN(iTem,IIMIN)
        IIMAX = MAX(iTem,IIMAX)
      ENDDO
      IHMINLT0 = (IHMIN .LT. 0)
      IKMINLT0 = (IKMIN .LT. 0)
      ILMINLT0 = (ILMIN .LT. 0)
      IHMAX = MAX(ABS(IHMIN),ABS(IHMAX))
      IKMAX = MAX(ABS(IKMIN),ABS(IKMAX))
      ILMAX = MAX(ABS(ILMIN),ABS(ILMAX))
!
!     Decides for each space group, which structure factor
!     calculation should be invoked for each particular reflection.
!     For example, in P 1 21 1, there are two different structure
!     factor expressions, depending upon whether or not "k" is even
!     or odd.  If the LOGREF entry for a particular reflection is
!     set to TRUE, this means that the reflection meets the criterion
!     which comes as a comment directly afterward.  Thus
!     LOGREF(1,IR)=K_.EQ.K_m ! k=2n
!     means that if k is even for the IR'th reflection, then LOGREF
!     for that reflection is set to true.  The following abbreviations
!     are used for reflection indices (all defined as integer)
!
!     H_    = h
!     K_    = k
!     L_    = l
!     HPK   = h+k
!     HPL   = h+l
!     KPL   = k+l
!     HPKPL = h+k+l
!
!     Note that when these abbreviations are used with an 'm' appended,
!     they hold the result of some modulus style calculation performed
!     on the related variable.  Hence K_m=2*(K_/2) followed by a test
!     of "does K_ equal K_m?" tests whether or not k is even.
!
      SELECT CASE (NumberSGTable)
        CASE (1,2,40,58,430,433,434,435,356,449,451,462,468)  ! P1,P-1,C 1 2 1,C 1 2/m 1,P3,P-3,R3(hex),R-3(hex),I-4,P-31m, P-3m1, P6, P-6
          NLGREF = 0
        CASE (469,471,481,483,485)             ! P6/m, P622, P-6m2, P-62m, P6/mmm
          NLGREF = 0
        CASE (39,57,81)                        ! P 1 21 1, P 1 21/m 1, P 1 1 b
          NLGREF = 1
          DO IR = 1, NumOfRef
            K_ = iHKL(2,IR)
            K_m = 2*(K_/2)
            LOGREF(1,IR) = K_.EQ.K_m ! k = 2n
          ENDDO
        CASE (44,50,61,67,116,176,298)         ! P 1 c 1, C 1 c 1, P 1 2/c 1, C 1 21/c 1, C 2 2 21
          NLGREF = 1                           ! C m c 21, C m c m,
          DO IR = 1, NumOfRef
            L_ = iHKL(3,IR)
            L_m = 2*(L_/2)
            LOGREF(1,IR) = (L_.EQ.L_m) ! l = 2n
          ENDDO
        CASE (62,45)                           ! P 1 2/n 1, P 1 n 1
          NLGREF = 1
          DO IR = 1, NumOfRef
            HPL = iHKL(1,IR) + iHKL(3,IR)
            HPLm = 2*(HPL/2)
            LOGREF(1,IR) = (HPL.EQ.HPLm) ! h+l = 2n
          ENDDO
        CASE (64,304)                          ! P 1 21/c 1, C m c a
          NLGREF = 1
          DO IR = 1, NumOfRef
            KPL = iHKL(2,IR) + iHKL(3,IR)
            KPLm = 2*(KPL/2)
            LOGREF(1,IR) = KPL.EQ.KPLm ! k+l = 2n
          ENDDO
        CASE (65,391)                          ! P 1 21/n 1, P -4 21 c
          NLGREF = 1
          DO IR = 1, NumOfRef
            HPKPL = iHKL(1,IR) + iHKL(2,IR) + iHKL(3,IR)
            HPKPLm = 2*(HPKPL/2)
            LOGREF(1,IR) = HPKPL.EQ.HPKPLm ! h+k+l = 2n
          ENDDO
        CASE (66,112)                          ! P 1 21/a 1, P 21 21 2
          NLGREF = 1
          DO IR = 1, NumOfRef
            HPK = iHKL(1,IR) + iHKL(2,IR)
            HPKm = 2*(HPK/2)
            LOGREF(1,IR) = HPK.EQ.HPKm ! h+k = 2n
          ENDDO
        CASE (46,52,63,69)                     ! P 1 a 1, I 1 a 1, P 1 2/a 1, I 1 2/a 1
          NLGREF = 1
          DO IR = 1, NumOfRef
            H_ = iHKL(1,IR)
            H_m = 2*(H_/2)
            LOGREF(1,IR) = (H_.EQ.H_m) ! h = 2n
          ENDDO
        CASE (115,290)                         ! P 21 21 21, P b c a
          NLGREF = 4
          DO IR = 1, NumOfRef
            HPK = iHKL(1,IR) + iHKL(2,IR)
            KPL = iHKL(2,IR) + iHKL(3,IR)
            HPKm = 2*(HPK/2)
            KPLm = 2*(KPL/2)
            LOGREF(1,IR) = (HPK.EQ.HPKm) .AND. (KPL.EQ.KPLm) ! h+k = 2n,   k+l = 2n
            LOGREF(2,IR) = (HPK.EQ.HPKm) .AND. (KPL.NE.KPLm) ! h+k = 2n,   k+l = 2n+1
            LOGREF(3,IR) = (HPK.NE.HPKm) .AND. (KPL.EQ.KPLm) ! h+k = 2n+1, k+l = 2n
            LOGREF(4,IR) = (HPK.NE.HPKm) .AND. (KPL.NE.KPLm) ! h+k = 2n+1, k+l = 2n+1
          ENDDO
        CASE (143)                             ! P c a 21
          NLGREF = 4
          DO IR = 1, NumOfRef
            H_ = iHKL(1,IR)
            L_ = iHKL(3,IR)
            H_m = 2*(H_/2)
            L_m = 2*(L_/2)
            LOGREF(1,IR) = (H_.EQ.H_m) .AND. (L_.EQ.L_m) ! h = 2n  , l = 2n
            LOGREF(2,IR) = (H_.EQ.H_m) .AND. (L_.NE.L_m) ! h = 2n  , l = 2n+1
            LOGREF(3,IR) = (H_.NE.H_m) .AND. (L_.EQ.L_m) ! h = 2n+1, l = 2n
            LOGREF(4,IR) = (H_.NE.H_m) .AND. (L_.NE.L_m) ! h = 2n+1, l = 2n+1
          ENDDO
        CASE (164,284)                         ! P n a 21, P b c n
          NLGREF = 4
          DO IR = 1, NumOfRef
            HPK = iHKL(1,IR) + iHKL(2,IR)
            L_ = iHKL(3,IR)
            HPKm = 2*(HPK/2)
            L_m = 2*(L_/2)
            LOGREF(1,IR) = (HPK.EQ.HPKm) .AND. (L_.EQ.L_m) ! h+k = 2n,   l = 2n
            LOGREF(2,IR) = (HPK.EQ.HPKm) .AND. (L_.NE.L_m) ! h+k = 2n,   l = 2n+1
            LOGREF(3,IR) = (HPK.NE.HPKm) .AND. (L_.EQ.L_m) ! h+k = 2n+1, l = 2n
            LOGREF(4,IR) = (HPK.NE.HPKm) .AND. (L_.NE.L_m) ! h+k = 2n+1, l = 2n+1
          ENDDO
        CASE (212)                             ! F d d 2
          NLGREF = 4
          DO IR = 1, NumOfRef
            HPKPL = iHKL(1,IR) + iHKL(2,IR) + iHKL(3,IR)
            IREMAIN = MOD(HPKPL,4)
            LOGREF(1,IR) = (IREMAIN.EQ.0) !h+k+l=4n
            LOGREF(2,IR) = (IREMAIN.EQ.1) !h+k+l=4n+1
            LOGREF(3,IR) = (IREMAIN.EQ.2) !h+k+l=4n+2
            LOGREF(4,IR) = (IREMAIN.EQ.3) !h+k+l=4n+3
          ENDDO
        CASE (266)                             ! P c c n
          NLGREF = 4
          DO IR = 1, NumOfRef
            HPK = iHKL(1,IR) + iHKL(2,IR)
            HPL = iHKL(1,IR) + iHKL(3,IR)
            HPKm = 2*(HPK/2)
            HPLm = 2*(HPL/2)
            LOGREF(1,IR) = (HPK.EQ.HPKm) .AND. (HPL.EQ.HPLm) ! h+k = 2n  , h+l = 2n
            LOGREF(2,IR) = (HPK.EQ.HPKm) .AND. (HPL.NE.HPLm) ! h+k = 2n  , h+l = 2n+1
            LOGREF(3,IR) = (HPK.NE.HPKm) .AND. (HPL.EQ.HPLm) ! h+k = 2n+1, h+l = 2n
            LOGREF(4,IR) = (HPK.NE.HPKm) .AND. (HPL.NE.HPLm) ! h+k = 2n+1, h+l = 2n+1
          ENDDO
        CASE (269)                             ! P b c m
          NLGREF = 4
          DO IR = 1, NumOfRef
            K_ = iHKL(2,IR)
            L_ = iHKL(3,IR)
            K_m = 2*(K_/2)
            L_m = 2*(L_/2)
            LOGREF(1,IR) = (K_.EQ.K_m) .AND. (L_.EQ.L_m) ! k = 2n  , l = 2n
            LOGREF(2,IR) = (K_.EQ.K_m) .AND. (L_.NE.L_m) ! k = 2n  , l = 2n+1
            LOGREF(3,IR) = (K_.NE.K_m) .AND. (L_.EQ.L_m) ! k = 2n+1, l = 2n
            LOGREF(4,IR) = (K_.NE.K_m) .AND. (L_.NE.L_m) ! k = 2n+1, l = 2n+1
          ENDDO
        CASE (292)                             ! P n m a
          NLGREF = 4
          DO IR = 1, NumOfRef
            HPL = iHKL(1,IR) + iHKL(3,IR)
            K_ = iHKL(2,IR)
            HPLm = 2*(HPL/2)
            K_m = 2*(K_/2)
            LOGREF(1,IR) = (HPL.EQ.HPLm) .AND. (K_.EQ.K_m) ! h+l = 2n,   k = 2n
            LOGREF(2,IR) = (HPL.EQ.HPLm) .AND. (K_.NE.K_m) ! h+l = 2n,   k = 2n+1
            LOGREF(3,IR) = (HPL.NE.HPLm) .AND. (K_.EQ.K_m) ! h+l = 2n+1, k = 2n
            LOGREF(4,IR) = (HPL.NE.HPLm) .AND. (K_.NE.K_m) ! h+l = 2n+1, k = 2n+1
          ENDDO
        CASE (360)                             ! P4/n (origin choice 2: inversion at origin)
          NLGREF = 4
          DO IR = 1, NumOfRef
            H_ = iHKL(1,IR)
            K_ = iHKL(2,IR)
            H_m = 2*(H_/2)
            K_m = 2*(K_/2)
            LOGREF(1,IR) = (H_.EQ.H_m) .AND. (K_.EQ.K_m) ! h = 2n,   k = 2n
            LOGREF(2,IR) = (H_.EQ.H_m) .AND. (K_.NE.K_m) ! h = 2n,   k = 2n+1
            LOGREF(3,IR) = (H_.NE.H_m) .AND. (K_.EQ.K_m) ! h = 2n+1, k = 2n
            LOGREF(4,IR) = (H_.NE.H_m) .AND. (K_.NE.K_m) ! h = 2n+1, k = 2n+1
          ENDDO
        CASE (362)                             ! P42/n (origin choice 2: inversion at origin)
          NLGREF = 8
          DO IR = 1, NumOfRef
            H_ = iHKL(1,IR)
            K_ = iHKL(2,IR)
            L_ = iHKL(3,IR)
            HPK = H_ + K_
            HPL = H_ + L_
            KPL = K_ + L_
            HPKm = 2*(HPK/2)
            HPLm = 2*(HPL/2)
            KPLm = 2*(KPL/2)
            LOGREF(1,IR) = (HPK.EQ.HPKm) .AND. (HPL.EQ.HPLm) .AND. (KPL.EQ.KPLm) ! h+k = 2n,   h+l = 2n,   k+l = 2n
            LOGREF(2,IR) = (HPK.EQ.HPKm) .AND. (HPL.EQ.HPLm) .AND. (KPL.NE.KPLm) ! h+k = 2n,   h+l = 2n,   k+l = 2n+1
            LOGREF(3,IR) = (HPK.EQ.HPKm) .AND. (HPL.NE.HPLm) .AND. (KPL.EQ.KPLm) ! h+k = 2n,   h+l = 2n+1, k+l = 2n
            LOGREF(4,IR) = (HPK.EQ.HPKm) .AND. (HPL.NE.HPLm) .AND. (KPL.NE.KPLm) ! h+k = 2n,   h+l = 2n+1, k+l = 2n+1
            LOGREF(5,IR) = (HPK.NE.HPKm) .AND. (HPL.EQ.HPLm) .AND. (KPL.EQ.KPLm) ! h+k = 2n+1, h+l = 2n,   k+l = 2n
            LOGREF(6,IR) = (HPK.NE.HPKm) .AND. (HPL.EQ.HPLm) .AND. (KPL.NE.KPLm) ! h+k = 2n+1, h+l = 2n,   k+l = 2n+1
            LOGREF(7,IR) = (HPK.NE.HPKm) .AND. (HPL.NE.HPLm) .AND. (KPL.EQ.KPLm) ! h+k = 2n+1, h+l = 2n+1, k+l = 2n
            LOGREF(8,IR) = (HPK.NE.HPKm) .AND. (HPL.NE.HPLm) .AND. (KPL.NE.KPLm) ! h+k = 2n+1, h+l = 2n+1, k+l = 2n+1
          ENDDO
        CASE (365)                             ! I 41/a (origin choice 2)
          NLGREF = 8
          DO iR = 1, NumOfRef
            H_ = iHKL(1,iR)
            K_ = iHKL(2,iR)
            H_m = 2*(H_/2)
            K_m = 2*(K_/2)
            HPKPL = iHKL(1,IR) + iHKL(2,IR) + iHKL(3,IR)
            IREMAIN = MOD(HPKPL,4)
            LOGREF(1,IR) = (H_.EQ.H_m) .AND. (K_.EQ.K_m) .AND. (IREMAIN.EQ.0)
            LOGREF(2,IR) = (H_.EQ.H_m) .AND. (K_.NE.K_m) .AND. (IREMAIN.EQ.0)
            LOGREF(3,IR) = (H_.NE.H_m) .AND. (K_.EQ.K_m) .AND. (IREMAIN.EQ.0)
            LOGREF(4,IR) = (H_.NE.H_m) .AND. (K_.NE.K_m) .AND. (IREMAIN.EQ.0)
            LOGREF(5,IR) = (H_.EQ.H_m) .AND. (K_.EQ.K_m) .AND. (IREMAIN.EQ.2)
            LOGREF(6,IR) = (H_.EQ.H_m) .AND. (K_.NE.K_m) .AND. (IREMAIN.EQ.2)
            LOGREF(7,IR) = (H_.NE.H_m) .AND. (K_.EQ.K_m) .AND. (IREMAIN.EQ.2)
            LOGREF(8,IR) = (H_.NE.H_m) .AND. (K_.NE.K_m) .AND. (IREMAIN.EQ.2)
            IF (H_.EQ.2 .AND. K_.EQ.2 .AND. IREMAIN.EQ.2) THEN
            ENDIF
          ENDDO
        CASE (369)                             ! P 41 21 2
          NLGREF = 4
          DO IR = 1, NumOfRef
            H_ = iHKL(1,IR)
            K_ = iHKL(2,IR)
            L_ = iHKL(3,IR)
            IREMAIN = MOD(2*H_+2*K_+L_,4)
            LOGREF(1,IR) = (IREMAIN.EQ.0) ! 2h+2k+l = 4n
            LOGREF(2,IR) = (IREMAIN.EQ.1) ! 2h+2k+l = 4n+1
            LOGREF(3,IR) = (IREMAIN.EQ.2) ! 2h+2k+l = 4n+2
            LOGREF(4,IR) = (IREMAIN.EQ.3) ! 2h+2k+l = 4n+3
          ENDDO
        CASE (431,432)                         ! P31, P32
          NLGREF = 3
          DO IR = 1, NumOfRef
            LL = MOD(iHKL(3,IR)+300,3)
            LLM = 3*(LL/3)
            LOGREF(1,IR) = (LL.EQ.0)
            LOGREF(2,IR) = (LL.EQ.1)
            LOGREF(3,IR) = (LL.EQ.2)
          ENDDO
      END SELECT
      SELECT CASE (NumberSGTable)
! Adjustments for presence of kx and hy terms
        CASE (356, 360, 362, 365, 369, 391)
          JHMIN = MIN(IHMIN,IKMIN)
          JHMAX = MAX(IHMAX,IKMAX)
          IHMIN = JHMIN
          IKMIN = JHMIN
          IHMAX = JHMAX
          IKMAX = JHMAX
          IHMINLT0 = IHMIN.LT.0
          IKMINLT0 = IKMIN.LT.0
! Adjustments for presence of 'i' index
        CASE (430:435,449,451,462,468,469,471,481,483,485)
          JHMIN = MIN(IIMIN,IHMIN,IKMIN)
          JHMAX = MAX(IIMAX,IHMAX,IKMAX)
          IF (ABS(JHMIN).GT.ABS(JHMAX)) THEN
            JHMAX = ABS(JHMIN)
          ENDIF
          IHMIN = JHMIN
          IKMIN = JHMIN
          IHMAX = JHMAX
          IKMAX = JHMAX
          IHMINLT0 = IHMIN.LT.0
          IKMINLT0 = IKMIN.LT.0
      END SELECT

      END SUBROUTINE GET_LOGREF
!
!*****************************************************************************
!
