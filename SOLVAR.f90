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
! This module contains all data related to solutions from the SA
!
      MODULE SOLVAR

      IMPLICIT NONE

      INTEGER     MaxAtm_5
      PARAMETER ( MaxAtm_5 = 300 )

      INTEGER     MaxRun2
      PARAMETER ( MaxRun2 = 999 )

      INTEGER iSol2Run(1:MaxRun2)

      INTEGER iSolTicked(1:MaxRun2)

      REAL ProfileChiSqd(1:MaxRun2) ! MaxRun

      REAL IntensityChiSqd(1:MaxRun2) ! MaxRun

      REAL BestValuesDoF(1:100,1:MaxRun2)  ! mvar, MaxRun

      REAL XAtmCoords(1:3,1:MaxAtm_5,0:MaxRun2) ! MaxAtm, MaxRun 
! Co-ordinates of the atoms of the asymmetric unit of the best SA solution so far per run.
! XAtmCoords(1:3,1:150,0) is for Rietveld refinement (needed by SA_STRUCTURE_OUTPUT_PDB()
! needed by ViewStructure())

      END MODULE SOLVAR
!
!*****************************************************************************
!
