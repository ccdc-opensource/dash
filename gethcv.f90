!*==GETHCV.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
!
!*****************************************************************************
!
      SUBROUTINE GETHCV(FILENAM,lenfil,ier)
!
      CHARACTER*(*), INTENT(IN) :: FILENAM
!
      INCLUDE 'PARAMS.INC'
!
      CHARACTER*150 LINE
      INTEGER ICOR(20), NKKOR(MCHIHS)
      REAL WTI(MFCSTO)
!
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF, IREFH(3,MFCSPE), LOGREF(8,MFCSPE)
!
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
!
      COMMON /SAREFLNS/ AIOBS(MSAREF), AICALC(MSAREF)
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), S2S(MCHIHS), S4S(MCHIHS),     &
     &                IKKOR(MCHIHS), JKKOR(MCHIHS)
!
      LOGICAL IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN,  &
     &                IIMAX
!
      COMMON /CHISTOP/ NOBS, NFIT, IFIT(MCHSTP), CHIOBS, WT(MCHSTP),    &
     &                 XOBS(MCHSTP), YOBS(MCHSTP), YCAL(MCHSTP),        &
     &                 ESD(MCHSTP)
!
      COMMON /FPINF / PIK(0:50,MFPINF), KMINST(MFPINF), KMAXST(MFPINF)
!
      OPEN (121,FILE=FILENAM(:Lenfil),STATUS='OLD',ERR=998)
      KK = 0
      MREF = MSAREF
      KKOR = 0
      MINCOR = 20
      IER = 0
      READ (121,2121,END=998,ERR=998) NLIN, LINE
 2121 FORMAT (Q,A)
!
      DO I = NLIN, 1, -1
        IF (LINE(I:I).EQ.'.') THEN
          NPO = I
          GOTO 11
        ENDIF
      ENDDO
!
   11 NCOR = 0
      DO I = NPO + 1, NLIN
        IF ((LINE(I-1:I-1).EQ.' ') .AND. (LINE(I:I).NE.' ')) THEN
          NCOR = NCOR + 1
        ENDIF
      ENDDO
!
      NCOR = NCOR - 1
      BACKSPACE (121)
!
      DO IR = 1, MREF
        READ (121,*,END=100,ERR=998) (IREFH(I,IR),I=1,3), AIOBS(IR),    &
     &                               WTI(IR), KL, (ICOR(I),I=1,NCOR)
!
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
          WTIJ(IK) = WTI(II)*WTI(JJ)
        ELSE
          WTIJ(IK) = 0.02*WTI(II)*WTI(JJ)*FLOAT(NKKOR(IK))
        ENDIF
      ENDDO
      GOTO 999
  998 ier = 1
  999 CLOSE (121)
      RETURN
!
      END SUBROUTINE GETHCV
