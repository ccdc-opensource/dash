!
!*****************************************************************************
!
      SUBROUTINE GETPIK(FILE,lenfil,ier)

      USE VARIABLES
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: FILE
      INTEGER,       INTENT (IN   ) :: lenfil
      INTEGER,       INTENT (  OUT) :: ier

      INCLUDE 'PARAMS.INC'

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         KREFT,         KNIPT
      REAL                                             PIKVAL
      COMMON /FPINF1/ KREFT(MOBS), KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER I, J, K
      INTEGER tKNIPT(1:500)
      REAL tPIKVAL(1:500)
      LOGICAL WrongValuesPresent
      INTEGER KTEM

      WrongValuesPresent = .FALSE.
      ier = 0
      OPEN (21,FILE=FILE(1:Lenfil),STATUS='OLD',ERR=998)
      NFITA = 0
      NBIN = 0
      DO I = 1, MOBS
        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM
! JvdS Rather a serious error here, I think. KTEM can be as much as 70.
! Some of the reflections contribute 0.000000E+00 ???
        IF (KTEM .GT. 50) WrongValuesPresent = .TRUE.
        KREFT(I) = MIN(50,KTEM)
        NBIN = NBIN + 1
        WTSA(I) = 1.0/EBIN(I)**2
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
      CLOSE (21)

      END SUBROUTINE GETPIK
!
!*****************************************************************************
!
