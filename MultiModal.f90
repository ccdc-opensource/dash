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
      SUBROUTINE ParseRawInput(iPar)
! This routine analyses the raw user/Mogul input and tries to make sense of it, e.g.:
! - if the multimodal ranges overlap to form a single range, use the single range.
! - if the bounds are given as e.g. bimodal -170.0 to 170.0, reset to -10.0 to 10.0

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iPar

      INCLUDE 'params.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                                        iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      INTEGER         nbounds
      REAL                             mm_bounds
      COMMON /MMBNDS/ nbounds(1:MVAR), mm_bounds(1:2, 1:4, 1:MVAR)

      INTEGER       ninterval,             curr_interval
      REAL                                                        ratios
      COMMON /TEST/ ninterval(1:3,1:MVAR), curr_interval(1:MVAR), ratios(1:3,1:MVAR)

      REAL angle, minimum_arc
      REAL tLB, tUB
      INTEGER ModalCase(mvar)

      IF ( ModalFlag(iPar) .LT. 2 ) &
        RETURN
      IF ( ModalFlag(iPar) .EQ. 4 ) &
        RETURN
      ratios(1, iPar) = 1.0
      ratios(2, iPar) = 1.0
      ratios(3, iPar) = 1.0
      curr_interval(iPar) = 1
      CALL AdjustBothBounds(LB(iPar), UB(iPar))
      IF ( ModalFlag(iPar) .EQ. 2 ) THEN
        ! I think that the following line gives problems for 0.0:
        ! what does that mean?
        IF ( .NOT. ( (LB(iPar)*UB(iPar)) .GE. 0.0 ) ) THEN
          ModalCase(iPar) = 1 ! Bimodal inversion
        ELSE
          ModalCase(iPar) = 2 ! Bimodal mirror
        ENDIF
      ELSE
        ModalCase(iPar) = 3 ! Trimodal
      ENDIF
      SELECT CASE ( ModalCase(iPar) )
        CASE (1) ! ########################   Bimodal inversion   ##############################
          ! Overlap between intervals?
          minimum_arc = DEG*ACOS(COS(RAD*(UB(iPar)-LB(iPar))))
          IF ( minimum_arc .GE. 179.0 ) THEN
            ModalFlag(iPar) = 1
            LB(iPar) = -180.0
            UB(iPar) = +180.0
            RETURN
          ENDIF
          ! Convert -170.0, 170.0 to -10.0, 10.0.
          IF ( (UB(iPar)-LB(iPar)) .GT. 180.0 ) THEN
            LB(iPar) = LB(iPar) + 180.0
            UB(iPar) = UB(iPar) + 180.0
            CALL AdjustBothBounds(LB(iPar), UB(iPar))
          ENDIF
          ! Determine bounds
          nbounds(iPar) = 3
          mm_bounds(1, 1, iPar) = LB(iPar)+360.0
          mm_bounds(2, 1, iPar) = 360.0
          mm_bounds(1, 2, iPar) = 0.0
          mm_bounds(2, 2, iPar) = UB(iPar)
          mm_bounds(1, 3, iPar) = LB(iPar)+180.0
          mm_bounds(2, 3, iPar) = UB(iPar)+180.0
        CASE (2) ! ########################   Bimodal mirror   ##############################
          ! We only need to test if consecutive (that's what the Mogul routines return)
          ! Make both positive
          IF ( UB(iPar) .LE. 0.0 ) THEN
            angle = LB(iPar)
            LB(iPar) = -UB(iPar)
            UB(iPar) = -angle
          ENDIF
          ! Overlap between intervals?
          IF ( ABS(LB(iPar)) .LT. 1.0 ) THEN ! E.g. 0.0 to 10.0
            ModalFlag(iPar) = 1
            LB(iPar) = -UB(iPar)
            RETURN
          ENDIF
          IF ( ABS(UB(iPar)-180.0) .LT. 1.0 ) THEN ! E.g. 170.0 to 180.0
            ModalFlag(iPar) = 1
            UB(iPar) = 360.0 - LB(iPar)
            RETURN
          ENDIF
          ! Determine bounds
          nbounds(iPar) = 2
          mm_bounds(1, 1, iPar) = LB(iPar)
          mm_bounds(2, 1, iPar) = UB(iPar)
          mm_bounds(1, 2, iPar) = -UB(iPar)+360.0
          mm_bounds(2, 2, iPar) = -LB(iPar)+360.0
        CASE (3) ! ########################   Trimodal   ##############################
          ! Overlap between intervals?
          minimum_arc = DEG*ACOS(COS(RAD*(UB(iPar)-LB(iPar))))
          IF ( minimum_arc .GE. 119.0 ) THEN
            ModalFlag(iPar) = 1
            LB(iPar) = -180.0
            UB(iPar) = +180.0
            RETURN
          ENDIF
          CALL AdjustBothBounds(LB(iPar), UB(iPar))
          DO WHILE ( LB(iPar) .LT. 0.0 )
            LB(iPar) = LB(iPar) + 120.0
            UB(iPar) = UB(iPar) + 120.0
            CALL AdjustBothBounds(LB(iPar), UB(iPar))
          ENDDO
          ! Because LB > 0.0 and UB > LB, both LB and UB are now greater than 0.0,
          ! Due to the call to normalise() in AdjustBothBounds(), UB and LB are both less than 180.0
          ! Determine bounds
          tLB = LB(iPar)
          tUB = UB(iPar)
          mm_bounds(1, 1, iPar) = tLB
          mm_bounds(2, 1, iPar) = tUB
          ! Next section...
          tLB = tLB + 120.0
          tUB = tUB + 120.0
          ! Because values were between 0.0 and 180.0, they must now be between 120.0 and 300.0,
          ! which is where we want them
          mm_bounds(1, 2, iPar) = tLB
          mm_bounds(2, 2, iPar) = tUB
          ! Next section...
          tLB = tLB + 120.0
          tUB = tUB + 120.0
          ! Because values were between 120.0 and 300.0, they must now be between 240.0 and 420.0
          ! One, none or both values can be greater than 360.0
          IF ( tLB .GT. 360.0 ) THEN ! Even the lower bound is greater than 360.0: just subtract 360.0 from both
            nbounds(iPar) = 3
            mm_bounds(1, 3, iPar) = tLB-360.0
            mm_bounds(2, 3, iPar) = tUB-360.0
          ELSE IF ( tUB .LE. 360.0 ) THEN ! Even highest value still lower than 360.0: both are fine as is
            nbounds(iPar) = 3
            mm_bounds(1, 3, iPar) = tLB
            mm_bounds(2, 3, iPar) = tUB
          ELSE ! The lower bound is less than 360.0, the upper bound is greater than 360.0: we need four bounds
            nbounds(iPar) = 4
            mm_bounds(1, 3, iPar) = tLB
            mm_bounds(2, 3, iPar) = 360.0
            mm_bounds(1, 4, iPar) = 0.0
            mm_bounds(2, 4, iPar) = tUB-360.0
          ENDIF
      END SELECT

      END SUBROUTINE ParseRawInput
!
!*****************************************************************************
!
      SUBROUTINE Normalise(angle)
! Takes a torsion angle and adds or subtracts 360.0 degrees until the angle is in the
! interval (-180.0, 180.0]

      IMPLICIT NONE

      REAL, INTENT (INOUT) :: angle

      DO WHILE ( angle .GT. +180.0 )
        angle = angle - 360.0
      ENDDO
      DO WHILE ( angle .LE. -180.0 )
        angle = angle + 360.0
      ENDDO

      END SUBROUTINE Normalise
!
!*****************************************************************************
!
      SUBROUTINE AdjustBothBounds(LB, UB)
! LB = Lower Bound, UB = Upper Bound
      IMPLICIT NONE

      REAL, INTENT (INOUT) :: LB, UB

      REAL angle

      CALL normalise(LB)
      CALL normalise(UB)
      ! Ensure Lower Bound < Upper Bound. Swap if necessary.
      IF ( UB .LT. LB ) THEN
        angle = UB
        UB = LB
        LB = angle
      ENDIF

      END SUBROUTINE AdjustBothBounds
!
!*****************************************************************************
!
! Commented out!
! As this sub is not called, but the extra ModalCase array added to
! COMMON /ModalTorsions/ makes it inconsistent to others.
!
!      SUBROUTINE AdjustSamplingForOtherRanges(angle, new_interval, iPar, random_number)
! index is index into multi modal properties COMMON block for this variable

! This routine takes a trial value and randomly changes
! it to lie in any of the two/three intervals (which may be the original interval)

! The fact that we have to pass in random_number is due to the fact that in SA_subs.f90
! random numbers are stored in a variable rather than being available through a function (and
! as a result it is possible to forget to increment IARR). TODO: make the random number generator into a function
! and make it available all throughout DASH (at the moment, I think we have four independent
! random number generators).

! interval must be initialised to the current interval. I it will be set to the new interval.

! This SUBROUTINE gets a bit confused about where the original angle was supposed to be in, I think...

!      IMPLICIT NONE
!
!      REAL,    INTENT (INOUT) :: angle
!      INTEGER, INTENT (  OUT) :: new_interval
!      INTEGER, INTENT (IN   ) :: iPar
!      REAL,    INTENT (IN   ) :: random_number
!
!      INCLUDE 'params.inc'
!
!      INTEGER                ModalFlag,       ModalCase,       RowNumber, iRadio
!      REAL                                                                        iX, iUB, iLB  
!      COMMON /ModalTorsions/ ModalFlag(mvar), ModalCase(mvar), RowNumber, iRadio, iX, iUB, iLB
!
!      INTEGER       ninterval,             curr_interval
!      REAL                                                        ratios
!      COMMON /TEST/ ninterval(1:3,1:MVAR), curr_interval(1:MVAR), ratios(1:3,1:MVAR)
!
!
!      ! NOTE: this routine assumes that "angle" is in the first interval,
!      ! regardless of the current interval for this parameter
!      new_interval = 1
!      SELECT CASE ( ModalCase(iPar) )
!        CASE (1) ! Bimodal inversion
!          IF (random_number .GT. ratios(1, iPar)/(ratios(1, iPar)+ratios(2, iPar))) THEN
!            angle = angle + 180.0
!            new_interval = 2
!          ENDIF
!        CASE (2) ! Bimodal mirror
!          IF (random_number .GT. ratios(1, iPar)/(ratios(1, iPar)+ratios(2, iPar))) THEN
!            angle = -angle
!            new_interval = 2
!          ENDIF
!        CASE (3) ! Trimodal
!          IF (random_number .GT. ratios(1, iPar)/(ratios(1, iPar)+ratios(2, iPar)+ratios(3, iPar))) THEN
!            angle = angle + 120.0
!            new_interval = new_interval + 1
!          ENDIF
!          IF (random_number .GT. (ratios(1, iPar)+ratios(2, iPar))/(ratios(1, iPar)+ratios(2, iPar)+ratios(3, iPar))) THEN
!            angle = angle + 120.0
!            new_interval = new_interval + 1
!          ENDIF
!      END SELECT
!      CALL Normalise(angle)
!
!      END SUBROUTINE AdjustSamplingForOtherRanges
!
!*****************************************************************************
!
!C      SUBROUTINE ShowBimodalDialog(IFrow, Xinitial)
!C
!C      USE WINTERACTER
!C      USE DRUID_HEADER
!C      USE ZMVAR
!C      USE VARIABLES
!C
!C      IMPLICIT NONE      
!C
!C      INTEGER, INTENT (IN   ) :: IFrow
!C      REAL,    INTENT (IN   ) :: Xinitial
!C
!C      INCLUDE 'params.inc'
!C
!C      INTEGER         nvar, ns, nt, iseed1, iseed2
!C      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2
!C
!C      REAL            X_init,       x_unique,       lb,       ub
!C      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)
!C
!C      REAL             prevx,       prevlb,       prevub
!C      LOGICAL                                                   LimsChanged
!C      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged
!C
!C      REAL, DIMENSION (3,2) :: TempBounds
!C      COMMON /TriModalBounds/  TempBounds
!C
!C      INTEGER                ModalFlag,       ModalCase,       RowNumber, iRadio
!C      REAL                                                                        iX, iUB, iLB  
!C      COMMON /ModalTorsions/ ModalFlag(mvar), ModalCase(mvar), RowNumber, iRadio, iX, iUB, iLB
!C
!C      INTEGER i, k, frag, dof
!C      INTEGER Upper, Lower
!C      REAL    Zero, OneEighty, xtem
!C
!C! Initialise variables
!C      Upper = 1
!C      Lower = 2
!C      Zero = 0.0
!C      OneEighty = 180.0
!C      ! Remember current setings, that will be restored when user presses "Cancel" or "non-modal"
!C      CALL SelectDASHDialog(IDD_SA_Modal_input2)
!C      CALL DASHWGridGetCellReal(IDF_parameter_grid_modal, 1, IFRow, iX)
!C      CALL DASHWGridGetCellReal(IDF_parameter_grid_modal, 2, IFRow, iLB)
!C      CALL DASHWGridGetCellReal(IDF_parameter_grid_modal, 3, IFRow, iUB)
!C      RowNumber = IFRow
!C      iRadio = ModalFlag(IFRow)
!C!     Given the number of the parameter want to know
!C!     which zmatrix, fragment it belongs to.
!C      frag = 0
!C      DO i = 1, maxDOF
!C        DO k = 1, nfrag
!C          IF (IFRow .EQ. zm2par(i,k)) THEN
!C            dof = i
!C            frag = k
!C            EXIT
!C          ENDIF
!C        ENDDO
!C        IF (frag .NE. 0) EXIT
!C      ENDDO
!C      CALL SelectDASHDialog(IDD_ModalDialog)
!C!     Clear Fields
!C      IF (.NOT. UseMogul) CALL WDialogClearField(IDF_MogulText)
!C      CALL WDialogClearField(IDF_ModalUpper)
!C      CALL WDialogClearField(IDF_ModalLower)
!C      CALL WDialogClearField(IDF_ReportLower1)
!C      CALL WDialogClearField(IDF_ReportLower2)
!C      CALL WDialogClearField(IDF_ReportUpper1)
!C      CALL WDialogClearField(IDF_ReportUpper2)
!C
!C!     Initialise fields 
!C      CALL WDialogPutString(IDF_TorsionName, czmpar(dof,frag))
!C      CALL WDialogPutReal(IDF_Initial, Xinitial, '(F12.5)')
!C      IF (ModalFlag(IfRow) .EQ. 1) THEN ! Not been set before
!C        CALL WDialogPutRadioButton(IDF_BiModalRadio) 
!C        IF (XInitial .GE. 0.0) THEN
!C          CALL WDialogPutReal(IDF_ModalLower, Zero, '(F12.5)')
!C          CALL WDialogPutReal(IDF_ModalUpper, OneEighty, '(F12.5)')
!C          CALL WDialogPutReal(IDF_ReportLower1, -OneEighty, '(F12.5)')
!C          CALL WDialogPutReal(IDF_ReportUpper1, Zero,'(F12.5)')
!C        ELSE
!C          CALL WDialogPutReal(IDF_ModalLower, -OneEighty, '(F12.5)')
!C          CALL WDialogPutReal(IDF_ModalUpper, Zero, '(F12.5)')
!C          CALL WDialogPutReal(IDF_ReportLower1, Zero, '(F12.5)')
!C          CALL WDialogPutReal(IDF_ReportUpper1, OneEighty, '(F12.5)')
!C        ENDIF
!C      ELSE
!C        CALL WDialogPutReal(IDF_ModalLower, lb(IFRow), '(F12.5)')
!C        CALL WDialogPutReal(IDF_ModalUpper, ub(IFRow), '(F12.5)')
!C        IF (ModalFlag(IFRow) .EQ. 2) THEN
!C          CALL WDialogPutRadioButton(IDF_BiModalRadio)
!C          IF ((UB(IFRow) * LB(IFRow)) .LT. 0.00) THEN
!C            CALL DASHWDialogGetReal(IDF_ModalUpper, xtem)
!C            CALL WDialogPutReal(IDF_ReportLower1, (xtem - 180.0))
!C            CALL DASHWDialogGetReal(IDF_ModalLower, xtem)
!C            CALL WDialogPutReal(IDF_ReportUpper1, (xtem + 180.0))
!C          ELSE
!C            CALL DASHWDialogGetReal(IDF_ModalUpper, xtem)
!C            CALL WDialogPutReal(IDF_ReportLower1, -xtem)
!C            CALL DASHWDialogGetReal(IDF_ModalLower, xtem)
!C            CALL WDialogPutReal(IDF_ReportUpper1, -xtem)
!C          ENDIF
!C        ELSEIF (ModalFlag(IFRow) .EQ. 3) THEN
!C          CALL WDialogPutRadioButton(IDF_TriModalRadio)          
!C          CALL DASHWDialogGetReal(IDF_ModalUpper, xtem)
!C          CALL DetermineTrimodalBounds(xtem, Upper)              
!C          CALL DASHWDialogGetReal(IDF_ModalLower, xtem)
!C          CALL DetermineTrimodalBounds(xtem, Lower)
!C          CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
!C          CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
!C          CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
!C          CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))          
!C        ENDIF
!C      ENDIF
!C      CALL WDialogShow(-1, -1, 0, SemiModeless)
!C
!C      END SUBROUTINE ShowBimodalDialog
!
!*****************************************************************************
!
!C      SUBROUTINE DealWithBimodalDialog
!C
!C      USE WINTERACTER
!C      USE DRUID_HEADER
!C      USE VARIABLES
!C      USE ZMVAR
!C
!C      IMPLICIT NONE 
!C           
!C      INCLUDE 'params.inc'
!C
!C      INTEGER         nvar, ns, nt, iseed1, iseed2
!C      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2
!C
!C      REAL            X_init,       x_unique,       lb,       ub
!C      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)
!C
!C      REAL             prevx,       prevlb,       prevub
!C      LOGICAL                                                   LimsChanged
!C      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged
!C
!C      REAL, DIMENSION (3,2) :: TempBounds
!C      COMMON /TriModalBounds/  TempBounds
!C
!C      INTEGER                ModalFlag,       ModalCase,       RowNumber, iRadio
!C      REAL                                                                        iX, iUB, iLB  
!C      COMMON /ModalTorsions/ ModalFlag(mvar), ModalCase(mvar), RowNumber, iRadio, iX, iUB, iLB
!C
!C      LOGICAL, EXTERNAL :: OutOfBounds
!C      INTEGER ISET
!C      INTEGER Upper, Lower
!C      REAL Zero, OneEighty
!C      REAL lower1, upper1, lower2, upper2
!C
!C! Initialise variables
!C      Upper = 1
!C      Lower = 2
!C      Zero = 0.0
!C      OneEighty = 180.0
!C      CALL PushActiveWindowID
!C      CALL SelectDASHDialog(IDD_ModalDialog)
!C      SELECT CASE (EventType)
!C        CASE (FieldChanged)
!C          SELECT CASE (EventInfo%VALUE1)
!C            CASE (IDF_BiModalRadio)
!C              CALL WDialogClearField(IDF_ReportLower2)
!C              CALL WDialogClearField(IDF_ReportUpper2)
!C              CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C              CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C              CALL BiModalCalculateOtherPairOfBounds(lower1, upper1, lower2, upper2)
!C              CALL WDialogPutReal(IDF_ModalLower, lower1)
!C              CALL WDialogPutReal(IDF_ModalUpper, upper1)
!C              CALL WDialogPutReal(IDF_ReportLower1, lower2)
!C              CALL WDialogPutReal(IDF_ReportUpper1, upper2)
!C              ModalFlag(RowNumber) = 2
!C            CASE (IDF_TriModalRadio)
!C              CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C              CALL DetermineTrimodalBounds(lower1, Lower)
!C              CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C              CALL DetermineTrimodalBounds(upper1, Upper)
!C              CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
!C              CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
!C              CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
!C              CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))
!C              ModalFlag(RowNumber) = 3
!C            CASE (IDF_Initial)
!C              CALL DASHWDialogGetReal(IDF_Initial, x_unique(RowNumber))
!C              x_unique(RowNumber) = MAX(lb(RowNumber),x_unique(RowNumber))
!C              x_unique(RowNumber) = MIN(ub(RowNumber),x_unique(RowNumber))
!C              CALL WDialogPutReal(IDF_Initial, x_unique(RowNumber), '(F12.5)')
!C            CASE (IDF_ModalLower)
!C              CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C              lower1 = MIN(ub(RowNumber), lower1) ! Why?
!C              lb(RowNumber) = lower1 ! Why?
!C              CALL WDialogPutReal(IDF_ModalLower,lb(RowNumber),'(F12.5)')
!C! How ranges are calculated depends on state of Modal RadioButton  
!C              CALL DASHWDialogGetRadioButton(IDF_BimodalRadio, ISET)
!C              SELECT CASE (ISET) ! Bimodal radiobutton active
!C                CASE (1)
!C                  ! This is an exact duplication of the code above
!C                  CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C                  CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C                  CALL BiModalCalculateOtherPairOfBounds(lower1, upper1, lower2, upper2)
!C                  CALL WDialogPutReal(IDF_ModalLower, lower1)
!C                  CALL WDialogPutReal(IDF_ModalUpper, upper1)
!C                  CALL WDialogPutReal(IDF_ReportLower1, lower2)
!C                  CALL WDialogPutReal(IDF_ReportUpper1, upper2)
!C                CASE (2) !Trimodal radiobutton active           
!C                  CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C                  CALL DetermineTrimodalBounds(lower1, Lower)
!C                  CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C                  CALL DetermineTrimodalBounds(upper1, Upper)
!C                  CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
!C                  CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
!C                  CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
!C                  CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))
!C              END SELECT
!C            CASE (IDF_ModalUpper)
!C! Check the bounding - only update if parameter is set to vary
!C              CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C              upper1 = MAX(lb(RowNumber), upper1) ! Why?
!C              ub(RowNumber) = upper1 ! Why?
!C              CALL WDialogPutReal(IDF_ModalUpper, ub(RowNumber), '(F12.5)')
!C!             How ranges are calculated depends on state of Modal RadioButton      
!C              CALL DASHWDialogGetRadioButton(IDF_BimodalRadio, ISET)
!C              SELECT CASE (ISET) ! Bimodal Radiobutton active
!C                CASE (1)
!C                ! This is the second copy of the code above
!C                  CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C                  CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C                  CALL BiModalCalculateOtherPairOfBounds(lower1, upper1, lower2, upper2)
!C                  CALL WDialogPutReal(IDF_ModalLower, lower1)
!C                  CALL WDialogPutReal(IDF_ModalUpper, upper1)
!C                  CALL WDialogPutReal(IDF_ReportLower1, lower2)
!C                  CALL WDialogPutReal(IDF_ReportUpper1, upper2)
!C                CASE (2) !Trimodal Radiobutton active
!C                  CALL DASHWDialogGetReal(IDF_ModalLower, lower1)
!C                  CALL DetermineTrimodalBounds(lower1, Lower)
!C                  CALL DASHWDialogGetReal(IDF_ModalUpper, upper1)
!C                  CALL DetermineTrimodalBounds(upper1, Upper)               
!C                  CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
!C                  CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
!C                  CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
!C                  CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))
!C                END SELECT
!C            END SELECT
!C        CASE (PushButton)
!C          SELECT CASE (EventInfo%VALUE1)
!C            CASE (IDOK)
!C!             Record parameters in appropriate arrays
!C              CALL DASHWDialogGetReal(IDF_Initial, x_unique(RowNumber))
!C              CALL DASHWDialogGetReal(IDF_ModalLower, lb(RowNumber))
!C              CALL DASHWDialogGetReal(IDF_ModalUpper, ub(RowNumber))
!C!             Check that x is in bounds
!C
!C
!C        ! Commented out for the moment: do we ever need this?
!C
!C       !       IF (OutOfBounds(RowNumber, x_unique(RowNumber))) THEN
!C       !         CALL WarningMessage('Initial value does not fall within defined ranges.')
!C       !         CALL PopActiveWindowID
!C       !         RETURN
!C       !       ENDIF
!C
!C
!C              CALL WDialogHide
!C              CALL SelectDASHDialog(IDD_SA_Modal_Input2)
!C              CALL WGridColourRow(IDF_parameter_grid_modal, RowNumber, WIN_RGB(255, 0, 0), WIN_RGB(256, 256, 256))
!C              LimsChanged = .TRUE.
!C!           Return bounds to previous values
!C            CASE (IDCANCEL)
!C              UB(RowNumber) = iUB
!C              LB(RowNumber) = iLB
!C              x_unique(RowNumber) = iX
!C              ModalFlag(RowNumber) = iRadio
!C              CALL WDialogHide
!C!           Return to "unimodal" mode. Modal torsion angle is no longer applied
!C            CASE (IDF_BiModalReset)
!C              ub(RowNumber) = OneEighty
!C              lb(RowNumber) = (-1) * OneEighty
!C              x_unique(RowNumber) = iX
!C              ModalFlag(RowNumber) = 1 
!C              CALL WDialogHide
!C              CALL SelectDASHDialog(IDD_SA_Modal_Input2)
!C              CALL WGridColourRow(IDF_parameter_grid_modal, RowNumber, WIN_RGB(256, 256, 256), WIN_RGB(256, 256, 256))                                              
!C          END SELECT
!C          CALL WDialogClearField(IDF_MogulText)
!C          prevub(RowNumber) = UB(RowNumber)
!C          prevlb(RowNumber) = LB(RowNumber)
!C          CALL SelectDASHDialog(IDD_SA_Modal_Input2)
!C          CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, RowNumber, x_unique(RowNumber))
!C          CALL WGridPutCellReal(IDF_parameter_grid_modal, 2, RowNumber, LB(RowNumber))
!C          CALL WGridPutCellReal(IDF_parameter_grid_modal, 3, RowNumber, UB(RowNumber)) 
!C          CALL WGridPutCellCheckBox(IDF_parameter_grid_modal,5, RowNumber, UnChecked)                          
!C      END SELECT
!C      CALL PopActiveWindowID
!C          
!C      END SUBROUTINE DealWithBimodalDialog
!
!*****************************************************************************
!
!C      SUBROUTINE DetermineTriModalBounds(xtem, BoundColumn)
!C
!C      IMPLICIT NONE
!C
!C      INTEGER, INTENT (IN   ) :: BoundColumn
!C
!C      REAL, DIMENSION (3,2) :: TempBounds
!C      COMMON /TriModalBounds/  TempBounds
!C
!C      REAL xtem,ttem
!C
!C      TempBounds(1,BoundColumn) = xtem
!C      IF (xtem.LT.0.0) xtem = xtem+360.0
!C      ttem = xtem + 120.0
!C      IF (ttem .GE. 360.0) ttem = ttem - 360.0
!C      IF (ttem.GE.180.0) ttem = ttem-360.0
!C      TempBounds(2,BoundColumn) = ttem                  
!C      ttem = xtem + 240.0
!C      IF (ttem .GE. 360.0) ttem = ttem - 360.0
!C      IF (ttem.GE.180.0) ttem = ttem-360.0
!C      TempBounds(3,BoundColumn) = ttem
!C      IF (xtem.GE.180.0) xtem = xtem-360.0
!C
!C      END SUBROUTINE DetermineTrimodalBounds
!
!*****************************************************************************
!
      LOGICAL FUNCTION OutOfBounds(iPar, angle)

! This Subroutine determines if a trial torsion angle value is within
! modal torsion angle ranges defined.

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iPar
      REAL,    INTENT (INOUT) :: angle

      INCLUDE 'params.inc'

      INTEGER         nbounds
      REAL                             mm_bounds
      COMMON /MMBNDS/ nbounds(1:MVAR), mm_bounds(1:2, 1:4, 1:MVAR)

      INTEGER iBound

      CALL Normalise(angle)
      ! Convert from (-180.0, 180.0] to [0.0, 360.0)
      IF ( angle .LT. 0.0 ) &
        angle = angle + 360.0
      DO iBound = 1, nbounds(iPar)
        IF ( angle .GE. mm_bounds(1, iBound, iPar) .AND. &
             angle .LE. mm_bounds(2, iBound, iPar) ) THEN
          OutOfBounds = .FALSE.
          RETURN
        ENDIF
      ENDDO
      OutOfBounds = .TRUE.

      END FUNCTION OutOfBounds
!
!*****************************************************************************
!
      SUBROUTINE BiModalCalculateOtherPairOfBounds(lower1, upper1, lower2, upper2)
! This subroutine analyses the lower and upper bound provided by the user, swaps them if
! necessary and calculates the second pair of bounds.

      IMPLICIT NONE

      REAL, INTENT (INOUT) :: lower1
      REAL, INTENT (INOUT) :: upper1
      REAL, INTENT (  OUT) :: lower2
      REAL, INTENT (  OUT) :: upper2

      REAL tReal

      ! Swap upper and lower if necessary
      IF ( lower1 .GT. upper1 ) THEN
        tReal  = lower1
        lower1 = upper1
        upper1 = tReal
      ENDIF
      IF ( lower1*upper1 .LT. 0.0 ) THEN
        ! Apply inversion
        lower2 = upper1 - 180.0
        upper2 = lower1 + 180.0
      ELSE
        ! Apply mirror
        lower2 = -upper1
        upper2 = -lower1
      ENDIF

      END SUBROUTINE BiModalCalculateOtherPairOfBounds
!
!*****************************************************************************
!
