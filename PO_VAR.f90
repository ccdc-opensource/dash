!
!*****************************************************************************
!
! This module contains all Preferred Orientation related data
!
      MODULE PO_VAR

      LOGICAL PrefParExists

! Determines if preferred orientation correction is to be included or not

      INTEGER iPrfPar

! The number of the SA parameter corresponding to the extent of preferred orientation

      REAL PrefPars(1:4)

! PrefPars(1:3) = Orientation (to be replaced by something called 'PO_Direction(1:3)
! PrefPars(4)   = Extent of preferred orientation

      REAL PrefCsqa(48,10000) ! 10000 = MFCSTO

      END MODULE PO_VAR
!
!*****************************************************************************
!
