!
!*****************************************************************************
!
      SUBROUTINE CHKMAXREF
!
! Checks if the maximum number of reflections has been exceeded
!
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'REFLNS.INC'
      INCLUDE 'statlog.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON

      LOGICAL routine_called
      SAVE    routine_called
      DATA    routine_called / .FALSE. /

      NumOfRef = maxk
      IF (NumOfRef .GT. 350) THEN
        NumOfRef = 350
        IF (.NOT. routine_called) THEN
          CALL InfoMessage('DASH has a maximum limit of 350 reflections.'//CHAR(13)//&
                           'Only the 350 lowest angle reflections will be indexed and used.')
          routine_called = .TRUE.
        ENDIF
      ENDIF
      know = NumOfRef
! Calculate peak centre of KNOW in ARGK, and its derivatives
      CALL PCCN01(2)
! argk now contains the peak position of the last reflection
      NPTS = 1
      DO WHILE ((XBIN(NPTS) .LT. argk) .AND. (NPTS .LT. MOBS))
        CALL INC(NPTS) ! NPTS is the number of points used for Pawley refinement.
      ENDDO
      argmax(1) = argk
      maxk = NumOfRef
      NBIN = NPTS
      DataSetChange = DataSetChange + 1
      CALL GetProfileLimits
      CALL Get_IPMaxMin 

      END SUBROUTINE CHKMAXREF
!
!*****************************************************************************
!
