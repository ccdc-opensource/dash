!
!*****************************************************************************
!
      SUBROUTINE FORTIC(PNAME,ALSQ,MATSZ,PCXX,PFXX,MAGROU,CALROU,FILNMR)
!
! *** FORTIC from FORTY - WIFD***
!
!X
!C 19B
!H Main body of a PR LSQ program, with dummy arguments
!A PNAME holds the name, probably of the calling program, to print
!A PCXX is the routine for the peak centre
!A PFXX is the routine for the peak function
!A MAGROU is the routine to deal with magnetic parameters.  this will be set to
!A        DOMAG if expecting magnetic parameters, or DUMMY if not.
!A CALROU is the routine to give the calculated function.  At present expected
!A        settings for this are CALPRM for magnetic, CALPR for non-magnetic.
!
!
      IMPLICIT NONE

      INCLUDE 'params.inc'

      EXTERNAL PCXX, PFXX, MAGROU, CALROU
      CHARACTER*6 PNAME
      INTEGER MATSZ
      REAL ALSQ(MATSZ)

      CHARACTER*10 filnmr
      COMMON /commun/ filnam_root
      CHARACTER*10 filnam_root
      
      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER OldNBin

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      filnam_root = filnmr
      CALL PREFIN(PNAME)
! SET UP PRECISE PROBLEM, AND READ MOST L CARDS:
      CALL REFSET
! THIS ROUTINE IS ONLY FOR ONE PHASE:
      CALL LOGPHA(1)
      CALL SETPR(PCXX,PFXX,MAGROU)
      IF (IBMBER .NE. 0) RETURN
! COLLECT CONSTRAINTS IMPOSED BY SYMMETRY, AND THOSE REQUESTED, AND
! SET UP PARAMETERS AS VARIABLES (NOT YET AS BASIC VARIABLES)
      CALL PARSPR(MAGROU)
! MAKE LIST OF REFLECTION INDICES:
      CALL INRFPR(PCXX,PFXX)
! Check if we have too many reflections
      OldNBin = NBIN
      CALL CHKMAXREF(PCXX)
      NBIN = OldNBin
! OUTPUT H,K,L IF REQUIRED:
      CALL HKLOUT(PCXX,ALSQ,MATSZ)

      END SUBROUTINE FORTIC
!
!*****************************************************************************
!
      SUBROUTINE FORSYM(pname,filnmr)

      CHARACTER*6 PNAME
      CHARACTER*10 filnmr

      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      CHARACTER*10 filnam_root
      COMMON /commun/ filnam_root

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      filnam_root = filnmr
      NINIT = 1
      CALL PREFIN(PNAME)
      CALL SYMOP
      IF (IBMBER .NE. 0) RETURN
      CALL OPSYM(1)

      END SUBROUTINE FORSYM
!
!*****************************************************************************
!
      SUBROUTINE MAKRHM
! Makes a number of matrices to speed up the default calculation

      INCLUDE 'SGinc\FFCALCTOP.INC'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      LOGICAL CENTRC
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13

      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      COMMON /symsto/ sctrh(24,MFCSTO), rhsto(3,24,MFCSTO)

      DIMENSION H(3)

      DO ir = 1, maxk
        DO ii = 1, 3
          h(ii) = iHKL(ii,ir)
        ENDDO
        DO i = 1, nopc
          CALL rotsym(H,RHSTO(1,i,ir),I,-1)
          sctrh(i,ir) = SCALPR(TRANS(1,I),H)
        ENDDO
      ENDDO

      END SUBROUTINE MAKRHM
!
!*****************************************************************************
!
