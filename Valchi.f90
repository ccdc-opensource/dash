!
!*****************************************************************************
!
      SUBROUTINE VALCHI(CHIVAL,CurrentParameter)

      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE VARIABLES
      USE REFVAR

      IMPLICIT NONE

      REAL,    INTENT (  OUT) :: CHIVAL
      INTEGER, INTENT (IN   ) :: CurrentParameter

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      REAL              BICALC,         XICALC
      COMMON /SAREFLN2/ BICALC(MFCSTO), XICALC(MFCSTO)

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)
      
      INTEGER         NStPar
      COMMON /pextra/ NStPar

      REAL    SUM1, SUM2, RESCL, DELI, DELJ, CHIADD
      INTEGER iR, iK, II, JJ
      REAL, EXTERNAL :: FFCALC_001, FFCALC_002, FFCALC_039, FFCALC_040, FFCALC_044, FFCALC_050, &
                        FFCALC_052, FFCALC_057, FFCALC_058, FFCALC_061, FFCALC_064, FFCALC_065
      REAL, EXTERNAL :: FFCALC_066, FFCALC_067, FFCALC_069, FFCALC_112, FFCALC_115, FFCALC_116, &
                        FFCALC_143, FFCALC_164, FFCALC_176, FFCALC_212, FFCALC_266, FFCALC_269
      REAL, EXTERNAL :: FFCALC_284, FFCALC_290, FFCALC_292, FFCALC_298, FFCALC_304, FFCALC_356, &
                        FFCALC_365, FFCALC_369, FFCALC_430, FFCALC_431, FFCALC_432, FFCALC_433
      REAL, EXTERNAL :: FFCALC_434, FFCALC_435, FFCALC_449, FFCALC_451, FFCALC_462, FFCALC_468, &
                        FFCALC_469, FFCALC_471, FFCALC_481, FFCALC_483, FFCALC_485, FFCALC_DEFAULT
     
! We have two types of parameters: those affecting the fractional co-ordinates and the
! preferred orientation. If the current parameter is the preferred orientation, there is
! no need to re-do al the structure factor calculations
      IF (CurrentParameter .LE. NStPar) THEN
        CALL PRECFC
        IF (LOG_HYDROGENS) THEN
          NATOM = TotNumOfAtoms
        ELSE
          NATOM = NumOfNonHydrogens
        ENDIF
        SELECT CASE (NumberSGTable)
          CASE (1)             ! P1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_001(IR)
            ENDDO
          CASE (2)             ! P-1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_002(IR)
            ENDDO
          CASE (39)            ! P 1 21 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_039(IR)
            ENDDO
          CASE (40)             ! C 1 2 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_040(IR)
            ENDDO
          CASE (44)             ! P 1 c 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_044(IR)
            ENDDO
          CASE (50)            ! C 1 c 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_050(IR)
            ENDDO
          CASE (52)            ! I 1 a 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_052(IR)
            ENDDO
          CASE (57)            ! P 1 21/m 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_057(IR)
            ENDDO
          CASE (58)            ! C 1 2/m 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_058(IR)
            ENDDO
          CASE (61)            ! P 1 2/c 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_061(IR)   
            ENDDO
          CASE (64)            ! P 1 21/c 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_064(IR)   
            ENDDO
          CASE (65)            ! P 1 21/n 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_065(IR)
            ENDDO
          CASE (66)            ! P 1 21/a 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_066(IR)
            ENDDO
          CASE (67)            ! C 1 2/c 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_067(IR)
            ENDDO
          CASE (69)            ! I 1 2/a 1
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_069(IR)
            ENDDO
          CASE (112)           ! P 21 21 2
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_112(IR)
            ENDDO
          CASE (115)           ! P 21 21 21
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_115(IR)
            ENDDO
          CASE (116)           ! C 2 2 21
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_116(IR)
            ENDDO
          CASE (143)           ! P c a 21
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_143(IR)
            ENDDO
          CASE (164)           ! P n a 21
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_164(IR)
            ENDDO
          CASE (176)            ! C m c 21
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_176(IR)
            ENDDO
          CASE (212)            ! F d d 2
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_212(IR)
            ENDDO
          CASE (266)            ! P c c n
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_266(IR)
            ENDDO
          CASE (269)            ! P b c m
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_269(IR)
            ENDDO
          CASE (284)           ! P b c n
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_284(IR)
            ENDDO
          CASE (290)           ! P b c a
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_290(IR)
            ENDDO
          CASE (292)           ! P n m a
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_292(IR)
            ENDDO
          CASE (298)           ! C m c m
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_298(IR)
            ENDDO
          CASE (304)           ! C m c a
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_304(IR)
            ENDDO
          CASE (356)           ! I -4
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_356(IR)
            ENDDO
          CASE (365)           ! I 41/a (origin choice 2)
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_365(IR)
            ENDDO
          CASE (369)           ! P 41 21 2
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_369(IR)
            ENDDO
  ! Obscure from here on in !
          CASE (430)           ! P3
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_430(IR)
            ENDDO
          CASE (431)           ! P31
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_431(IR)
            ENDDO
          CASE (432)           ! P32
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_432(IR)
            ENDDO
          CASE (433)           ! R3 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_433(IR)
            ENDDO
          CASE (434)           ! P-3
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_434(IR)
            ENDDO
          CASE (435)           ! R-3 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_435(IR)
            ENDDO
          CASE (449)           ! P-31m hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_449(IR)
            ENDDO
          CASE (451)           ! P-3m1 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_451(IR)
            ENDDO
          CASE (462)           ! P6 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_462(IR)
            ENDDO
          CASE(468)           ! P-6 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_468(IR)
            ENDDO
          CASE (469)           ! P6/m hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_469(IR)
            ENDDO
          CASE (471)           ! P622 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_471(IR)
            ENDDO
          CASE (481)           ! P-6m2 hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_481(IR)
            ENDDO
          CASE (483)           ! P-62m hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_483(IR)
            ENDDO
          CASE (485)           ! P6/mmm hexagonal axes
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_485(IR)
            ENDDO
          CASE DEFAULT
            DO IR = 1, NumOfRef
              AICALC(IR) = FFCALC_DEFAULT(IR)
            ENDDO
        END SELECT
        NATOM = TotNumOfAtoms
      ELSE
! If we are here, the parameter that has been changed didn't affect the fractional co-ordinates.
! In the current set-up, that means that it must have been the preferred orientation.
! So: recalculate the preferred orientation correction factors.
        CALL PO_PRECFC
      ENDIF
! AICALC(1:NumOfRef) now contains the structural part of the calculated intensities
! XICALC(1:NumOfRef) now contains the preferred orientation part of the calculated intensities
! If we are using preferred orientation: correct for it.
! If not: use the calculated intensities as is
      IF (PrefParExists) THEN
        DO iR = 1, NumOfRef
          BICALC(iR) = XICALC(iR) * AICALC(iR)
        ENDDO
      ELSE
        DO iR = 1, NumOfRef
          BICALC(iR) = AICALC(iR)
        ENDDO
      ENDIF
! BICALC(1:NumOfRef) now contains the calculated intensities corrected for preferred orientation
      SUM1 = 0.0
      SUM2 = 0.0
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        SUM1 = SUM1 + BICALC(II)*WTIJ(IK)*AIOBS(JJ) + BICALC(JJ)*WTIJ(IK)*AIOBS(II)
        SUM2 = SUM2 + BICALC(II)*WTIJ(IK)*BICALC(JJ)
      ENDDO
      RESCL = 0.5*SUM1/SUM2
      CHIVAL = 0.0
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        DELI = AIOBS(II) - RESCL*BICALC(II)
        DELJ = AIOBS(JJ) - RESCL*BICALC(JJ)
        CHIADD = DELI*WTIJ(IK)*DELJ
        CHIVAL = CHIVAL + CHIADD
      ENDDO
      CHIVAL = CHIVAL/FLOAT(NumOfRef-2)

      END SUBROUTINE VALCHI
!
!*****************************************************************************
!
      SUBROUTINE PRECFC
!
! Pre-calculates sine and cosine terms for the structure factor calculation
!
      USE ATMVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      REAL            COSQS,               SINQS
      COMMON /CSQSTO/ COSQS(-20:20,3,150), SINQS(-20:20,3,150)

      LOGICAL         IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0

      INTEGER         IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER N, IH, IH1, IK, IK1, IL, IL1
      REAL    C1N, C2N, C3N, S1N, S2N, S3N

!
!     COSQS(a,1,c) holds hx terms
!     COSQS(a,2,c) holds ky terms
!     COSQS(a,3,c) holds lz terms
!
! The Xs (the co-ordinates) have been ordered in MAKEFRAC.
! NATOM must be set to the appropriate number.
      IF (LOG_HYDROGENS) THEN
        NATOM = TotNumOfAtoms
      ELSE
        NATOM = NumOfNonHydrogens
      ENDIF
      DO N = 1, NATOM
        C1N = COS(TWOPI*X(1,N))
        C2N = COS(TWOPI*X(2,N))
        C3N = COS(TWOPI*X(3,N))
        S1N = SIN(TWOPI*X(1,N))
        S2N = SIN(TWOPI*X(2,N))
        S3N = SIN(TWOPI*X(3,N))
        COSQS(0,1,N) = 1.0
        COSQS(0,2,N) = 1.0
        COSQS(0,3,N) = 1.0
        SINQS(0,1,N) = 0.0
        SINQS(0,2,N) = 0.0
        SINQS(0,3,N) = 0.0
!.. IH
        DO IH = 1, IHMAX
          IH1 = IH - 1
          COSQS(IH,1,N) = COSQS(IH1,1,N)*C1N - SINQS(IH1,1,N)*S1N !hx
          SINQS(IH,1,N) = SINQS(IH1,1,N)*C1N + COSQS(IH1,1,N)*S1N !hx
        ENDDO
        IF (IHMINLT0) THEN
          DO IH = IHMIN, -1
            COSQS(IH,1,N) =  COSQS(-IH,1,N)
            SINQS(IH,1,N) = -SINQS(-IH,1,N)
          ENDDO
        ENDIF
!.. IK
        DO IK = 1, IKMAX
          IK1 = IK - 1
          COSQS(IK,2,N) = COSQS(IK1,2,N)*C2N - SINQS(IK1,2,N)*S2N !ky
          SINQS(IK,2,N) = SINQS(IK1,2,N)*C2N + COSQS(IK1,2,N)*S2N !ky
        ENDDO
        IF (IKMINLT0) THEN
          DO IK = IKMIN, -1
            COSQS(IK,2,N) =  COSQS(-IK,2,N)
            SINQS(IK,2,N) = -SINQS(-IK,2,N)
          ENDDO
        ENDIF
!.. IL
        DO IL = 1, ILMAX
          IL1 = IL - 1
          COSQS(IL,3,N) = COSQS(IL1,3,N)*C3N - SINQS(IL1,3,N)*S3N
          SINQS(IL,3,N) = SINQS(IL1,3,N)*C3N + COSQS(IL1,3,N)*S3N
        ENDDO
        IF (ILMINLT0) THEN
          DO IL = ILMIN, -1
            COSQS(IL,3,N) =  COSQS(-IL,3,N)
            SINQS(IL,3,N) = -SINQS(-IL,3,N)
          ENDDO
        ENDIF
      ENDDO
      NATOM = TotNumOfAtoms

      END SUBROUTINE PRECFC
!
!*****************************************************************************
!
      SUBROUTINE PO_PRECFC
!
! Pre-calculates the Preferred Orientation part of the intensities
! I.e., this routine fills XICALC
! March-Dollase correction for Preferred Orientation
!
      USE PO_VAR
      USE ATMVAR
      USE REFVAR
      
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL              BICALC,         XICALC
      COMMON /SAREFLN2/ BICALC(MFCSTO), XICALC(MFCSTO)

      INTEGER           iHMUL
      COMMON /SAREFLN3/ iHMUL(MFCSTO)

      INTEGER iR, i
      REAL    PrfPar, prfcor, csqa, ssqa

      PrfPar = X(iPrfPar) ! current value of the extent of the preferred orientation
      DO iR = 1, NumOfRef
        prfcor = 0.0
        DO i = 1, iHMUL(iR)
          csqa = PrefCsqa(i,iR)
          ssqa = 1.0 - csqa
          prfcor = prfcor + (csqa*(PrfPar**2) + ssqa/PrfPar)**(-1.5)
        ENDDO
        XICALC(iR) = prfcor / FLOAT(iHMUL(iR))
      ENDDO

      END SUBROUTINE PO_PRECFC
!
!*****************************************************************************
!
