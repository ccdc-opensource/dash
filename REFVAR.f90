!
!*****************************************************************************
!
! This module contains all reflections related data
!
      MODULE REFVAR

      IMPLICIT NONE

      INTEGER     MaxRef
      PARAMETER ( MaxRef = 10000 )
! MaxRef = maximum number of reflections.

      INTEGER NumOfRef
! NumOfRef = maximum number of reflections used during SA

      INTEGER iHKL(1:3,MaxRef)
! iHKL = h, k and l for each reflection.

      REAL    RefArgK(MaxRef)
! RefArgK = 2 theta value, corrected for zero point error, for each reflection

      REAL    DSTAR(MaxRef)
! DSTAR = d* of each reflection

! Note: exactly the same information is held in two other common blocks.
! Of these, I deleted the following one (merged it with /PROFTIC/):
! COMMON /FCSPC2/ ARGK(MFCSP2), DSTAR(MFCSP2)

      INTEGER NLGREF
      LOGICAL LOGREF(8,MaxRef)
! Logicals indicating which reflections are absent.

      REAL    AIOBS(MaxRef), AICALC(MaxRef), XICALC(MaxRef), BICALC(MaxRef)
! AIOBS = observed intensity, per reflection
! AICALC = part of the calculated intensity due to structural parameters (atoms)
! XICALC = part of the calculated intensity due to preferred orientation
! BICALC = AICALC * XICALC = calculated intensity

      REAL    WTI(MaxRef)
! Weight of the reflection

      INTEGER iHMUL(MaxRef)



!O! JCC GGCALC dimension increased to 500
!O      REAL            rHKL,           AMUL
!O      REAL            ESDOBS,         SOMEGA,       GGCALC
!O	  
!O      INTEGER         MAXKK, KMIN, KMAX, KMOD, KNOW
!O      INTEGER                                           ISMAG
!O      REAL            DKDDS
!O      INTEGER                KOM23
!O
!O      COMMON /REFLNS/ rHKL(3,MaxRef), AMUL(MaxRef),                                   &
!O	                  ESDOBS(MaxRef), SOMEGA(MaxRef), GGCALC(500),                    &
!O                      MAXKK(9), KMIN, KMAX, KMOD, KNOW, ISMAG(MFCSTO), &
!O                      DKDDS, KOM23
!O
!O      INTEGER         MAXK
!O
!O      EQUIVALENCE (MAXK,MAXKK(1))
  
      END MODULE REFVAR
!
!*****************************************************************************
!
