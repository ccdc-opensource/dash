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
      SUBROUTINE FCN(X, H, CurrentParameter, IncPenalty)

      IMPLICIT NONE
      
      REAL,    INTENT (IN   ) :: X(*)
      REAL,    INTENT (  OUT) :: H
      INTEGER, INTENT (IN   ) :: CurrentParameter
      LOGICAL, INTENT (IN   ) :: IncPenalty

      INTEGER         NStPar
      COMMON /pextra/ NStPar
      
      INCLUDE 'SA_restrain.inc'

! If only e.g. the preferred orientation has changed, there is no need to 
! recalculate all the fractional co-ordinates
      IF (CurrentParameter .LE. NStPar) THEN
        CALL makefrac(X)
      ENDIF
      CALL valchi(H,CurrentParameter)
      
      IF ( IncPenalty .AND. DRestrNumb .GT. 0 ) THEN
        CALL AddPenalty()
        H = H + SpringWeight * SAPenalty
      ENDIF

      END SUBROUTINE FCN
!
!*****************************************************************************
!
