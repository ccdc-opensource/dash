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
      SUBROUTINE sa_move_status(ntotmov,movenow)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN   ) :: ntotmov, movenow

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      INTEGER ipcol

      IF ( in_batch ) &
        RETURN
      CALL SelectDASHDialog(IDD_SA_Action1)
      ipcol = InfoGrScreen(ColourReq)
      CALL IGrSelect(3,IDF_SAMove_picture)
      ruler = ntotmov
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(59)
      rulex1 = 0.0
      rulex2 = movenow
      CALL IGrFillPattern(Solid)
      CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
      CALL IGrColourN(159)
      rulex1 = rulex2
      rulex2 = ntotmov
      CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
      CALL IGrSelect(1,0)
      CALL IGrUnits(0.0,0.0,1.0,1.0)
      CALL IGrColourN(ipcol)

      END SUBROUTINE sa_move_status
!
!*****************************************************************************
!
