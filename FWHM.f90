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
!       ************************************************
!       PROGRAM TO FIND FULL WIDTHS OF FUNCTIONS
!       ************************************************
!
      SUBROUTINE Calculate_FWHM(FWHM, IntBreadth)

!C CAN ONLY BE CALLED FROM MULTIPEAK_FITTER()

      IMPLICIT NONE

      REAL, INTENT (  OUT) :: FWHM ! Full Width at Half Maximum
      REAL, INTENT (  OUT) :: IntBreadth ! Integral breadth

      INCLUDE 'params.inc'

      REAL            LEFT_BOUND, RIGHT_BOUND, TMAX, TOP2, ERRY
      COMMON /PARAMS/ LEFT_BOUND, RIGHT_BOUND, TMAX, TOP2, ERRY
  
      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XMINT

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      REAL TOP, V, W

!C INITIALISATION
      IBMBER = 0
      ERRY = 1.0E-5  ! ACCURACY RESTRICTION FOR PEAKTOP
      LEFT_BOUND = ZARGI(IIMIN)
      RIGHT_BOUND = ZARGI(IIMAX)
!C FIND PEAK TOP
      CALL PEAKTOP(LEFT_BOUND, RIGHT_BOUND, TMAX, TOP, ERRY)
      TOP2 = 0.5*TOP
!C FIND RHS ROOT
      CALL HALF_WIDTH(TMAX, RIGHT_BOUND, V)
      IF (IBMBER .NE. 0) RETURN
!C FIND LHS ROOT
      CALL HALF_WIDTH(LEFT_BOUND, TMAX, W)
      IF (IBMBER .NE. 0) RETURN
!C OUTPUT
      FWHM = V - W
      IntBreadth = 1.0 / TOP

      END SUBROUTINE Calculate_FWHM
!
!*****************************************************************************
!
      SUBROUTINE PEAKTOP(XMIN, XMAX, XAV, YTOP, YSMALL)
! This routine finds the maximum position of a single peak
! function, PKFUNC(X), in the region between XMIN and XMAX

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: XMIN, XMAX
      REAL, INTENT (  OUT) :: XAV, YTOP
      REAL, INTENT (IN   ) :: YSMALL

      REAL, EXTERNAL :: PKFUNC
      REAL    X(5), Y(5)
      INTEGER MAXITER, ITER
      INTEGER I, IMAX
      REAL    D2Y, YMAX

      MAXITER = 100
      X(1) = XMIN
      X(3) = 0.5*(XMIN+XMAX)
      X(5) = XMAX
      Y(1) = PKFUNC(X(1))
      Y(3) = PKFUNC(X(3))
      Y(5) = PKFUNC(X(5))
      ITER = 0
   10 ITER = ITER + 1
      IF (ITER .GT. MAXITER) GOTO 100 ! This happens now and then (due to rounding errors?), but is not a problem.
      X(2) = X(1) + 0.25*(X(5)-X(1))
      Y(2) = PKFUNC(X(2))
      X(4) = X(1) + 0.75*(X(5)-X(1))
      Y(4) = PKFUNC(X(4))
! Test whether peak top has been reached
      D2Y = ABS(2.0-(Y(5)+Y(1))/Y(3))
      IF (D2Y .LT. YSMALL) GOTO 100
      YMAX = -1.0
      DO I = 1, 5
        IF (Y(I) .GT. YMAX) THEN
          YMAX = Y(I)
          IMAX = I
        ENDIF
      ENDDO
      IF (IMAX .EQ. 1) THEN
        X(5) = X(3)
        X(3) = X(2)
        Y(5) = Y(3)
        Y(3) = Y(2)
      ELSEIF (IMAX .EQ. 5) THEN
        X(1) = X(3)
        X(3) = X(4)
        Y(1) = Y(3)
        Y(3) = Y(4)
      ELSE
        X(1) = X(IMAX-1)
        X(5) = X(IMAX+1)
        X(3) = X(IMAX)
        Y(1) = Y(IMAX-1)
        Y(5) = Y(IMAX+1)
        Y(3) = Y(IMAX)
      ENDIF
      GOTO 10
 100  XAV = X(3)
! Savitzky-Golay
      YTOP = (-3.0*Y(1) + 12.0*Y(2) + 17.0*Y(3) + 12.0*Y(4) - 3.0*Y(5)) / 35.0

      END SUBROUTINE PEAKTOP
!
!*****************************************************************************
!
      SUBROUTINE HALF_WIDTH(tX1, tX2, ROOT)
! This finds any roots of the function PKFUNC(X) in the range X1 < X < X2

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: tX1, tX2
      REAL, INTENT (  OUT) :: ROOT

      REAL            LEFT_BOUND, RIGHT_BOUND, TMAX, TOP2, ERRY
      COMMON /PARAMS/ LEFT_BOUND, RIGHT_BOUND, TMAX, TOP2, ERRY
  
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      REAL, EXTERNAL :: PKFUNC
      REAL Y1, X1, Y2, X2, X3, Y3
      INTEGER MAXITER, ITER

      MAXITER = 100
      X1 = tX1
      X2 = tX2
      Y1 = PKFUNC(X1) - TOP2
      Y2 = PKFUNC(X2) - TOP2
! Check that region contains a root
      IF ((Y1*Y2) .GT. 0.0) THEN
        IBMBER = IBMBER + 1
        RETURN
      ENDIF
!     **** LOOP ****
      ITER = 1
! Check if a root has been found
   10 IF (Y1 .EQ. 0.0) THEN
        ROOT = X1
        GOTO 999
      ENDIF
      IF (Y2 .EQ. 0.0) THEN
        ROOT = X2
        GOTO 999
      ENDIF
! Decide on how to choose next estimate of root (X3)
      IF (ABS(X2-X1) .GE. 0.0001) THEN
        X3 = 0.5*(X1+X2)
      ELSE
        X3 = ((X1*Y2)-(X2*Y1))/(Y2-Y1)
      ENDIF
! Has accuracy limit been reached
      IF (ABS(X3-X1) .LE. 1.0E-5) THEN
        ROOT = X3
        GOTO 999
      ENDIF
      IF (ABS(X3-X2) .LE. 1.0E-5) THEN
        ROOT = X3
        GOTO 999
      ENDIF
      Y3 = PKFUNC(X3) - TOP2
! Decide which to keep, X1 or X2 
      IF ((Y1*Y3) .LT. 0.0) THEN
        X2 = X3
        Y2 = Y3
      ELSE
        X1 = X3
        Y1 = Y3
      ENDIF
      ITER = ITER + 1
      IF (ITER .GT. MAXITER) THEN
        CALL DebugErrorMessage("ITER .GT. MAXITER in HALF_WIDTH()")
        ROOT = X3
        GOTO 999
      ENDIF
      GOTO 10 
  999 RETURN

      END SUBROUTINE HALF_WIDTH
!
!*****************************************************************************
!
