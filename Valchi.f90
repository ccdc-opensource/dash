!
!*****************************************************************************
!
      SUBROUTINE VALCHI(CHIVAL)

      INCLUDE 'PARAMS.INC'

      INTEGER         MAXK
      REAL                  FOB
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)

      INTEGER         NLGREF, IREFH
      LOGICAL                                  LOGREF
      COMMON /FCSPEC/ NLGREF, IREFH(3,MFCSPE), LOGREF(8,MFCSPE)

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      REAL              AIOBS,         AICALC
      COMMON /SAREFLNS/ AIOBS(MSAREF), AICALC(MSAREF)

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

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      CALL PRECFC
      SUM1 = 0.
      SUM2 = 0.

      INCLUDE 'AllFFCalc.inc'

      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        SUM1 = SUM1 + AICALC(II)*WTIJ(IK)*AIOBS(JJ) + AICALC(JJ)*WTIJ(IK)*AIOBS(II)
        SUM2 = SUM2 + AICALC(II)*WTIJ(IK)*AICALC(JJ)
      ENDDO
      RESCL = 0.5*SUM1/SUM2
      CHIVAL = 0.
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        DELI = AIOBS(II) - RESCL*AICALC(II)
        DELJ = AIOBS(JJ) - RESCL*AICALC(JJ)
        CHIADD = DELI*WTIJ(IK)*DELJ
        CHIVAL = CHIVAL + CHIADD
      ENDDO
      CHIVAL = CHIVAL/FLOAT(MAXK-2)

      END SUBROUTINE VALCHI
!
!*****************************************************************************
!
      SUBROUTINE PRECFC
!
!... Pre-calculates sin and cosine terms for the structure factor calculation
!
!
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

      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF, IREFH(3,MFCSPE), LOGREF(8,MFCSPE)
      COMMON /CSQSTO/ COSQS(-20:20,3,150), SINQS(-20:20,3,150)
      LOGICAL IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX
      DATA TWOPI/6.2831853071796/
!
!     cosqs(a,1,c) holds hx terms
!     cosqs(a,2,c) holds ky terms
!     cosqs(a,3,c) holds lz terms
!
      DO N = 1, NATOM
        C1N = COS(TWOPI*X(1,N))
        C2N = COS(TWOPI*X(2,N))
        C3N = COS(TWOPI*X(3,N))
        S1N = SIN(TWOPI*X(1,N))
        S2N = SIN(TWOPI*X(2,N))
        S3N = SIN(TWOPI*X(3,N))
        COSQS(0,1,N) = 1.
        COSQS(0,2,N) = 1.
        COSQS(0,3,N) = 1.
        SINQS(0,1,N) = 0.
        SINQS(0,2,N) = 0.
        SINQS(0,3,N) = 0.
!.. IH
        DO IH = 1, IHMAX
          IH1 = IH - 1
          COSQS(IH,1,N) = COSQS(IH1,1,N)*C1N - SINQS(IH1,1,N)*S1N
                                                              !hx
          SINQS(IH,1,N) = SINQS(IH1,1,N)*C1N + COSQS(IH1,1,N)*S1N
                                                              !hx
        ENDDO
        IF (IHMINLT0) THEN
          DO IH = IHMIN, -1
            COSQS(IH,1,N) = COSQS(-IH,1,N)
            SINQS(IH,1,N) = -SINQS(-IH,1,N)
          ENDDO
        ENDIF
!.. IK
        DO IK = 1, IKMAX
          IK1 = IK - 1
          COSQS(IK,2,N) = COSQS(IK1,2,N)*C2N - SINQS(IK1,2,N)*S2N
                                                              !ky
          SINQS(IK,2,N) = SINQS(IK1,2,N)*C2N + COSQS(IK1,2,N)*S2N
                                                              !ky
        ENDDO
        IF (IKMINLT0) THEN
          DO IK = IKMIN, -1
            COSQS(IK,2,N) = COSQS(-IK,2,N)
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
            COSQS(IL,3,N) = COSQS(-IL,3,N)
            SINQS(IL,3,N) = -SINQS(-IL,3,N)
          ENDDO
        ENDIF
      ENDDO

      END SUBROUTINE PRECFC
!
!*****************************************************************************
!
