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
! This file contains global variables.
! Yet another attempt to gather them.

      INTEGER            JRadOption
      COMMON /RadOption/ JRadOption

! JRadOption stores the type of radiation as entered by the user:
! 1 = X-ray lab data
! 2 = X-ray synchrotron data
! 3 = Constant Wavelength Neutrons
! 4 = Time-of-Flight Neutrons

      REAL            ALambda
      COMMON /ALABDA/ ALambda

! ALambda stores the wavelength in Angstrom
! Note that its value can be set when the anode material is known through the use of a
! function, e.g.: ALambda = WavelengthOf('Cu')

      INTEGER         IPTYPE
      COMMON /PLTYPE/ IPTYPE

! IPTYPE indicates which profiles should be plotted:
!
!  1 = Observed              profile
!  2 = Observed + Calculated profile
!
! -1 = Observed              profile to printer (hardcopy)
! -2 = Observed + Calculated profile to printer (hardcopy)

      INTEGER              IDCurrent_Cursor_Mode
      COMMON /CURSOR_MODE/ IDCurrent_Cursor_Mode

! IDCurrent_Cursor_Mode stores current mode: peak fitting, Pawley refinement or structure solution

      REAL             CHAR_SIZE, MARKER_SIZE
      COMMON /PROFDEF/ CHAR_SIZE, MARKER_SIZE

! Sizes of the characters and markers used in drawing the profiles in the main window

