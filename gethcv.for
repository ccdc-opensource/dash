      SUBROUTINE GETHCV(FILENAM,lenfil,ier)
C
	INCLUDE 'params.inc'
	 CHARACTER*150 LINE
      character*80  filenam
      INTEGER ICOR(20),NKKOR(MCHIHS)
      REAL WTI(MFCSTO)
C
      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)

      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17

      COMMON /SAREFLNS/AIOBS(MSAREF),AICALC(MSAREF)
      COMMON /CHISTO/ KKOR,WTIJ(MCHIHS),S2S(MCHIHS),S4S(MCHIHS),
     &IKKOR(MCHIHS),JKKOR(MCHIHS)
C
      LOGICAL IHMINLT0,IKMINLT0,ILMINLT0
      COMMON /CSQLOG/ IHMINLT0,IKMINLT0,ILMINLT0
      COMMON /CSQINT/ IHMIN,IHMAX,IKMIN,IKMAX,ILMIN,ILMAX,IIMIN,IIMAX
c
      COMMON /CHISTOP/ NOBS,NFIT,IFIT(MCHSTP),CHIOBS,
     &WT(MCHSTP),XOBS(MCHSTP),YOBS(MCHSTP),YCAL(MCHSTP),ESD(MCHSTP)

      COMMON /FPINF/PIK(0:50,MFPINF),KMINST(MFPINF),KMAXST(MFPINF)
C
!	write(56,*) ' HCVfile is ',lenfil,'"',FILENAM(:Lenfil),'"'
      OPEN(121,FILE=FILENAM(:Lenfil),STATUS='OLD',err=998)
!	write(56,*) ' HCVfile is ',lenfil,'"',FILENAM(:Lenfil),'"'
C
C
      KK=0
      MREF=MSAREF
      KKOR=0
      MINCOR=20
	IER = 0
      READ(121,2121,END=998,ERR=998) NLIN,LINE
 2121 FORMAT(Q,A)
C
      DO I=NLIN,1,-1
       IF (LINE(I:I).EQ.'.') THEN
         NPO=I
         GOTO 11
       END IF
      END DO
C
 11   NCOR=0
      DO I=NPO+1,NLIN
        IF ( (LINE(I-1:I-1).EQ.' ')
     &       .AND.(LINE(I:I).NE.' ') ) THEN
          NCOR=NCOR+1
        END IF
      END DO
C
      NCOR=NCOR-1
      BACKSPACE(121)
C
      DO IR=1,MREF
        READ(121,*,END=100,ERR=998) 
     &  (IREFH(I,IR),I=1,3),AIOBS(IR),WTI(IR),KL,(ICOR(I),I=1,NCOR)
C
        KK=IR
C
C
C.. Now work out which terms should be kept for the chi-squared calculation
C
        KKOR=KKOR+1
        IKKOR(KKOR)=IR
        JKKOR(KKOR)=IR
        NKKOR(KKOR)=100
        DO I=1,NCOR
          IF (ABS(ICOR(I)).GE.MINCOR) THEN
            KKOR=KKOR+1
            IKKOR(KKOR)=IR
            JKKOR(KKOR)=IR+I
            NKKOR(KKOR)=ICOR(I)
          END IF
        END DO
C
      END DO
C
C..
C
 100  MAXK=KK
C
      DO IK=1,KKOR
        II=IKKOR(IK)
        JJ=JKKOR(IK)
        IF (II.EQ.JJ) THEN
          WTIJ(IK)=WTI(II)*WTI(JJ)
        ELSE
          WTIJ(IK)=0.02*WTI(II)*WTI(JJ)*FLOAT(NKKOR(IK))
        END IF
      END DO
C
!	 write(56,*) ' In gethcv ',maxk,kkor
C
      GOTO 999
 998  ier=1
!	 write(56,*) ' In gethcv ',maxk,kkor
 999  CLOSE(121)
      RETURN
      END
