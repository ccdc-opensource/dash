!
!*****************************************************************************
!
      SUBROUTINE CHKMAXREF(PCXX)
!
! Checks if the maximum number of reflections has been exceeded
!

      IMPLICIT NONE

      EXTERNAL PCXX

      INCLUDE 'PARAMS.INC'
      INCLUDE 'REFLNS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         NPTS
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

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

      INTEGER iorda(10)
      REAL    ardif(10) ! Difference
      REAL    aadd ! Add
      REAL    arrt ! Relative: holds the d-spacing of the previous reflections
      REAL    armx
      INTEGER II

      aadd = 0.0
      IF (maxk .GT. 360) THEN
! We've too many reflections ... must reduce
        IF (.NOT. routine_called) THEN
          CALL InfoMessage('DASH has a maximum limit of 350 reflections.'//CHAR(13)//&
                           'Only the 350 lowest angle reflections will be indexed and used.')
          routine_called = .TRUE.
        ENDIF
        know = 350   ! know is a global variable used by PCXX
        CALL PCXX(2) ! Changes argk
        arrt = argk
! Search for the maximum difference in 2 theta between two reflections between reflections 350 and 360
! JvdS was:
!O      DO II = 1, 1
        DO II = 1, 10
          know = 350 + II ! know is a global variable
          CALL PCXX(2)
          ardif(II) = argk - arrt ! argk is a global variable
          arrt = argk
        ENDDO
        CALL SORT_REAL(ardif,iorda,10)
        maxk = 349 + iorda(10)
! JvdS was:
!O      aadd = ardif(10)
        aadd = ardif(iorda(10))
      ENDIF
      know = maxk
! Calculate peak centre of know in argk, and its derivatives
      CALL pcxx(2)
! argk already contains the peak position of the very very last reflection
! We are adding aadd in order to extend the profile as far as possible including the whole peak.
      armx = argk + aadd
      II = 1
      DO WHILE ((XBIN(II) .LT. armx) .AND. (II .LT. MOBS))
        II = II + 1
      ENDDO
      NPTS = II ! NPTS is the number of points used for Pawley refinement.
!      NBIN = II
!      CALL GetProfileLimits
      argmax(1) = armx

      END SUBROUTINE CHKMAXREF
!
!*****************************************************************************
!
