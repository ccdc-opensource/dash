!
!*****************************************************************************
!
      SUBROUTINE GETPIK(FILE,lenfil,ier)

      USE ATMVAR
      USE VARIABLES
      
      CHARACTER*(*), INTENT (IN   ) :: FILE
      INTEGER,       INTENT (IN   ) :: lenfil
      INTEGER,       INTENT (  OUT) :: ier

      INCLUDE 'PARAMS.INC'

      INTEGER         MAXK
      REAL                  FOB
      COMMON /FCSTOR/ MAXK, FOB(MaxAtm_3,MFCSTO)

      INTEGER         NLGREF, iREFH
      LOGICAL                                  LOGREF
      COMMON /FCSPEC/ NLGREF, iREFH(3,MFCSPE), LOGREF(8,MFCSPE)

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

      LOGICAL         IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0

      INTEGER         IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN, IIMAX

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      COMMON /FPINF1/ KREFT(MFPINF), KNIPT(50,MFPINF), PIKVAL(50,MFPINF)

      INTEGER I

      INTEGER tKNIPT(1:500)
      REAL tPIKVAL(1:500)
      LOGICAL WrongValuesPresent

      WrongValuesPresent = .FALSE.
      ier = 0
      OPEN (21,FILE=FILE(1:Lenfil),STATUS='OLD',ERR=998,IOSTAT=Istat)
      NFITA = 0
      NBIN = 0
      ittem = 0
      DO I = 1, MOBS
        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM
! JvdS Rather a serious error here, I think. KTEM can be as much as 70.
! Some of the reflections contribute 0.000000E+00 ???
        IF (KTEM .GT. 50) WrongValuesPresent = .TRUE.
        KREFT(I) = MIN(50,KTEM)
        NBIN = NBIN + 1
!..        ESDA(I)=1./SQRT(WTSA(I))
        WTSA(I) = 1.0/EBIN(I)**2
!..        KTEM=KMAXST(I)-KMINST(I)
        IF (KTEM.GT.0) THEN
          READ (21,*,ERR=998) (tKNIPT(K),tPIKVAL(K),K=1,KTEM)
          DO j = 1, MIN(50,KTEM)
            KNIPT(j,I)  = tKNIPT(j)
            PIKVAL(j,I) = tPIKVAL(j)
          ENDDO
          NFITA = NFITA + 1
          IFITA(NFITA) = I
        ENDIF
      ENDDO
  200 CONTINUE
      CLOSE (21)
      CALL Clear_BackGround
! During the SA, only profile points that have peak contributions are calculated (there is no background
! any more at this stage). Therefore, YCBIN must be initialised to zero's
      YCBIN = 0.0
      NoData = .FALSE.
      CALL GetProfileLimits
      IF (WrongValuesPresent) CALL DebugErrorMessage('>50 contributing reflections encountered at least once.')
      RETURN
  998 ier = 1
      CLOSE (21,IOSTAT=ISTAT)

      END SUBROUTINE GETPIK
!
!*****************************************************************************
!
