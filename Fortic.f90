!
!*****************************************************************************
!
      SUBROUTINE FORTIC(PNAME,ALSQ,MATSZ,PCXX,PFXX,MAGROU,FILNMR)
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
!
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      EXTERNAL PCXX, PFXX, MAGROU
      CHARACTER*6 PNAME
      INTEGER MATSZ
      REAL ALSQ(MATSZ)

      CHARACTER*10 filnmr
      COMMON /commun/ filnam_root
      CHARACTER*10 filnam_root
      
      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

!O      INTEGER OldNBin

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
!O      OldNBin = NBIN
      CALL CHKMAXREF(PCXX)
!O      NBIN = OldNBin
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

      INTEGER         NOP, NCENT, NOPC, NLAT, NGEN
      LOGICAL                                       CENTRC
      INTEGER                                               KOM13
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13

      REAL            SYM,         TRANS,       ALAT,      ORIGIN
      INTEGER                                                         KOM26
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      REAL            sctrh,            rhsto
      COMMON /symsto/ sctrh(24,MFCSTO), rhsto(3,24,MFCSTO)

      REAL H(3)
      INTEGER iR, ii, i
      REAL, EXTERNAL :: SCALPR

      DO iR = 1, NumOfRef
        DO ii = 1, 3
          h(ii) = FLOAT(iHKL(ii,iR))
        ENDDO
        DO i = 1, nopc
          CALL rotsym(H,RHSTO(1,i,iR),I,-1)
          sctrh(i,iR) = SCALPR(TRANS(1,I),H)
        ENDDO
      ENDDO

      END SUBROUTINE MAKRHM
!
!*****************************************************************************
!
