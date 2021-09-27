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
! This module contains all external RR related data
!
      MODULE TAVAR

      IMPLICIT NONE

      INTEGER iRietveldMethod
      INTEGER, PARAMETER :: INTERNAL_RB=0, FOR_TOPAS=1, FOR_GSAS=2, FOR_RIETAN=3

      CHARACTER(255) :: TOPASEXE, EXPGUIEXE, RIETANEXE, GSASINS

      CHARACTER(255) ext_RR_input_file_name

      INTEGER ext_RR_stage, ext_RR_start_dialog_id

      LOGICAL use_anisotropic_broadening, Rietan_FP

      CHARACTER(255) old_diffraction_data_file_name, old_FNAME

      INTEGER old_NumOfRef, old_SubtractBkg, NumOfBkgTerm
  
      END MODULE TAVAR
!
!*****************************************************************************
!
