! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
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

      INCLUDE 'params.inc'

      EXTERNAL PCXX, PFXX, MAGROU
      CHARACTER*6 PNAME
      INTEGER MATSZ
      REAL ALSQ(MATSZ)

      CHARACTER*10 filnmr

      CHARACTER*10    filnam_root
      COMMON /commun/ filnam_root

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER
      INTEGER, EXTERNAL :: PREFIN
      INTEGER IDummy
      
      filnam_root = filnmr
      IDummy = PREFIN(PNAME)
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

! When we are here, MAXKK(1) contains the maximum reflection

! Check if we have too many reflections
   !   CALL CHKMAXREF
! OUTPUT H,K,L IF REQUIRED:
      CALL HKLOUT(PCXX,ALSQ,MATSZ)

      END SUBROUTINE FORTIC
!
!*****************************************************************************
!
      SUBROUTINE MAKRHM

! Makes a number of matrices to speed up the default calculation

      USE REFVAR

      IMPLICIT NONE

      INTEGER         NOP, NCENT, NOPC, NLAT, NGEN
      LOGICAL                                       CENTRC
      INTEGER                                               KOM13
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13

      REAL            SYM,         TRANS,       ALAT,      ORIGIN
      INTEGER                                                         KOM26
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      REAL            sctrh,            rhsto
      COMMON /symsto/ sctrh(24,MaxRef), rhsto(3,24,MaxRef)

      REAL, EXTERNAL :: SCALPR
      REAL H(3)
      INTEGER iR, ii, i

      DO iR = 1, NumOfRef
        DO ii = 1, 3
          h(ii) = FLOAT(iHKL(ii,iR))
        ENDDO
        DO i = 1, NOPC
          CALL rotsym(H,RHSTO(1,i,iR),I,-1)
          sctrh(i,iR) = SCALPR(TRANS(1,I),H)
        ENDDO
      ENDDO

      END SUBROUTINE MAKRHM
!
!*****************************************************************************
!
