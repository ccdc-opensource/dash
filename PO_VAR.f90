!
!*****************************************************************************
!
! This module contains all Preferred Orientation related data
!
      MODULE PO_VAR

      IMPLICIT NONE

      LOGICAL PrefParExists
! Determines if preferred orientation correction is to be included or not.

! Now that it is possible to switch on PO during RR, closing the RR window leaves the
! program in an inconsistent state: SA does not use PO, but RR does. Therefore, when RR
! window is closed, state must be reset to original state.

      LOGICAL SA_PrefParExists
      REAL    SA_PrefPars(1:3)
! For use in Rietveld only, to enable restoring original state after closing Rietveld window.

      INTEGER iPrfPar
! The number of the SA parameter corresponding to the extent of preferred orientation.

      REAL PrefPars(1:3)
! PrefPars(1:3) = Orientation (to be replaced by something called 'PO_Direction(1:3).

      REAL PrefCsqa(48, 10000) ! 10000 = MFCSTO
! Precalculated preferred orientation corrections (generalised multiplicities).

      END MODULE PO_VAR
!
!*****************************************************************************
!
