!
!*****************************************************************************
!
      SUBROUTINE GETPIK(FILE,lenfil,ier)

      USE VARIABLES
      
      CHARACTER*(*), INTENT (IN   ) :: FILE
      INTEGER,       INTENT (IN   ) :: lenfil
      INTEGER,       INTENT (  OUT) :: ier

      INCLUDE 'PARAMS.INC'

      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)
      LOGICAL LOGREF
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

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      LOGICAL IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0

      INTEGER         IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX

      INTEGER          NOBSA, NFITA, IFITA
      REAL                                          CHIOBSA, WTSA
      REAL             XOBSA,         YOBSA,         YCALA,         ESDA
      COMMON /CHISTOP/ NOBSA, NFITA, IFITA(MCHSTP), CHIOBSA, WTSA(MCHSTP),    &
     &                 XOBSA(MCHSTP), YOBSA(MCHSTP), YCALA(MCHSTP), ESDA(MCHSTP)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      COMMON /FPINF1/ KREFT(MFPINF), KNIPT(50,MFPINF), PIKVAL(50,MFPINF)

      COMMON /FPINF2/ NTERMS

      INTEGER I

      ier = 0
      OPEN (21,FILE=FILE(1:Lenfil),STATUS='OLD',ERR=998,IOSTAT=Istat)
      CHIOBSA = 0.
      NFITA = 0
      NTERMS = 0
      NOBSA = 0
      MMOBS = MCHSTP
      ittem = 0
      DO I = 1, MMOBS
        READ (21,*,END=200) XOBSA(I), YOBSA(I), ESDA(I), KTEM
        KREFT(I) = KTEM
        NOBSA = NOBSA + 1
!..        ESDA(I)=1./SQRT(WTSA(I))
        WTSA(I) = 1.0/ESDA(I)**2
        YCALA(I) = 0.0
!..        KTEM=KMAXST(I)-KMINST(I)
        IF (KTEM.GT.0) THEN
          NTERMS = NTERMS + KTEM
          READ (21,*,ERR=998) (KNIPT(K,I),PIKVAL(K,I),K=1,KTEM)
          NFITA = NFITA + 1
          IFITA(NFITA) = I
          CHIOBSA = CHIOBSA + WTSA(I) * YOBSA(I)**2
        ENDIF
      ENDDO
  200 CONTINUE
      CLOSE (21)
! Now:
! None of the arrays in PROFBIN has been filled: the arrays in CHISTOP were filled instead.
! YCAL has been reset to 0.0
      NBIN = NOBSA
      DO I = 1, NBIN
        XBIN(I)  = XOBSA(I)
        YOBIN(I) = YOBSA(I)
        EBIN(I)  = ESDA(I)
      ENDDO
      CALL Init_BackGround
      NoData = .FALSE.
      CALL GetProfileLimits
      RETURN
  998 ier = 1
      CLOSE (21,IOSTAT=ISTAT)

      END SUBROUTINE GETPIK
!
!*****************************************************************************
!
