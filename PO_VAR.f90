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
      INTEGER SA_PO_Direction(1:3)
! For use in Rietveld only, to enable restoring original state after closing Rietveld window.

      INTEGER iPrfPar
! The number of the SA parameter corresponding to the extent of preferred orientation.

      INTEGER PO_Direction(1:3)
! Orientation

      REAL PrefCsqa(48, 10000) ! 10000 = MFCSTO
! Precalculated preferred orientation corrections (generalised multiplicities).

      END MODULE PO_VAR
!
!*****************************************************************************
!
