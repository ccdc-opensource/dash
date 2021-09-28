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
      SUBROUTINE UPLOAD_RANGE

      USE DRUID_HEADER

      IMPLICIT NONE

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      LOGICAL, EXTERNAL :: FnPatternOK
      REAL          atem1, atem2
      INTEGER       iTem, iTem1, iTem2, iNext
      CHARACTER*(8) chrfmt

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_Data_Properties)
      ENDIF

      IF (.NOT. FnPatternOK()) THEN
        IF ( .NOT. IN_BATCH ) THEN
          CALL WDialogClearField(IDF_xmin)
          CALL WDialogClearField(IDF_xmax)
          CALL WDialogClearField(IDF_ymin)
          CALL WDialogClearField(IDF_ymax)
          CALL PopActiveWindowID
        ENDIF
        RETURN
      ENDIF
      atem1 = MAX(ABS(XPMIN),ABS(XPMAX))
      atem2 = 0.0001 * ABS(XPMAX-XPMIN)
      item1 = 0.5 + ALOG10(atem1)
      item2 = MAX(0.,0.5-ALOG10(atem2))
      item  = 2 + item1 + item2
      chrfmt(1:2) = '(F'
      IF (item .GE. 10) THEN
        CALL IntegerToString(item,chrfmt(3:4),'(I2)')
        inext = 5
      ELSE
        CALL IntegerToString(item,chrfmt(3:3),'(I1)')
        inext = 4
      END IF
      chrfmt(inext:inext) = '.'
      inext = inext + 1
      CALL IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext = inext + 1
      chrfmt(inext:inext) = ')'
      IF ( .NOT. IN_BATCH ) THEN
        CALL WDialogPutReal(IDF_xmin, XPMIN, chrfmt(1:inext))
        CALL WDialogPutReal(IDF_xmax, XPMAX, chrfmt(1:inext))
      ENDIF
      atem1 = MAX(ABS(ypmin),ABS(ypmax))
      atem2 = 0.0001 * ABS(ypmax-ypmin)
      item1 = 0.5 + ALOG10(atem1)
      item2 = MAX(0.,0.5-ALOG10(atem2))
      item  = 2 + item1 + item2
      chrfmt(1:2) = '(F'
      IF (item.GE.10) THEN
        CALL IntegerToString(item,chrfmt(3:4),'(I2)')
        inext = 5
      ELSE
        CALL IntegerToString(item,chrfmt(3:3),'(I1)')
        inext = 4
      ENDIF
      chrfmt(inext:inext) = '.'
      inext = inext + 1
      CALL IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext = inext + 1
      chrfmt(inext:inext) = ')'

      IF ( .NOT. IN_BATCH )  THEN
        CALL WDialogPutReal(IDF_ymin, ypmin, chrfmt(1:inext))
        CALL WDialogPutReal(IDF_ymax, ypmax, chrfmt(1:inext))
        CALL PopActiveWindowID
      ENDIF

      END SUBROUTINE UPLOAD_RANGE
!
!*****************************************************************************
!
