!
!*****************************************************************************
!
      SUBROUTINE GETHCV(FILENAM,lenfil,ier)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: FILENAM
      INTEGER,       INTENT (IN   ) :: lenfil
      INTEGER,       INTENT (  OUT) :: ier

      INCLUDE 'PARAMS.INC'

      CHARACTER*150 LINE
      INTEGER ICOR(20), NKKOR(MCHIHS)
      REAL WTI(MFCSTO)

      INTEGER         MAXK
      REAL                  FOB
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)

      INTEGER         NLGREF, IREFH
      LOGICAL                                  LOGREF
      COMMON /FCSPEC/ NLGREF, IREFH(3,MFCSPE), LOGREF(8,MFCSPE)

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

      REAL              AIOBS,         AICALC
      COMMON /SAREFLNS/ AIOBS(MSAREF), AICALC(MSAREF)

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      LOGICAL         IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0

      INTEGER         IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX

      INTEGER KK, I, NPO, NLIN, NCOR, IR, II, JJ, IK, MINCOR, KL

      OPEN (121,FILE=FILENAM(:Lenfil),STATUS='OLD',ERR=998)
      KK = 0
      KKOR = 0
      MINCOR = 20
      IER = 0
      READ (121,2121,END=998,ERR=998) NLIN, LINE
 2121 FORMAT (Q,A)
      DO I = NLIN, 1, -1
        IF (LINE(I:I).EQ.'.') THEN
          NPO = I
          GOTO 11
        ENDIF
      ENDDO
   11 NCOR = 0
      DO I = NPO + 1, NLIN
        IF ((LINE(I-1:I-1).EQ.' ') .AND. (LINE(I:I).NE.' ')) THEN
          NCOR = NCOR + 1
        ENDIF
      ENDDO
      NCOR = NCOR - 1
      BACKSPACE (121)
      DO IR = 1, MSAREF
        READ (121,*,END=100,ERR=998) (IREFH(I,IR),I=1,3), AIOBS(IR),    &
     &                               WTI(IR), KL, (ICOR(I),I=1,NCOR)
        KK = IR
!
!.. Now work out which terms should be kept for the chi-squared calculation
!
        KKOR = KKOR + 1
        IKKOR(KKOR) = IR
        JKKOR(KKOR) = IR
        NKKOR(KKOR) = 100
        DO I = 1, NCOR
          IF (ABS(ICOR(I)).GE.MINCOR) THEN
            KKOR = KKOR + 1
            IKKOR(KKOR) = IR
            JKKOR(KKOR) = IR + I
            NKKOR(KKOR) = ICOR(I)
          ENDIF
        ENDDO
      ENDDO
  100 MAXK = KK
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        IF (II.EQ.JJ) THEN
          WTIJ(IK) = WTI(II) * WTI(JJ)
        ELSE
          WTIJ(IK) = 0.02*WTI(II)*WTI(JJ)*FLOAT(NKKOR(IK))
        ENDIF
      ENDDO
      GOTO 999
  998 ier = 1
  999 CLOSE (121)

      END SUBROUTINE GETHCV
!
!*****************************************************************************
!
