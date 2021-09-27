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
      SUBROUTINE FRAC2PDB(MATRIX,A,B,C,ALFA,BETA,GAMA)

! Same as LatticeCellParameters2Lattice_2()

      REAL MATRIX(3,3)
      REAL A, B, C, ALFA, BETA, GAMA
      REAL RADEG, CA, CB, CG, SG, VOLU
      DATA RADEG /1.7453292519943296E-2/
! This routine will create the conversion matrix for fractional to orthogonal
! coordinates as defined in the PDB manual (i.e. a along X)
      CA = COS(ALFA*RADEG)
      CB = COS(BETA*RADEG)
      CG = COS(GAMA*RADEG)
      SG = SIN(GAMA*RADEG)
      VOLU = A*B*C*SQRT(1.0-CA*CA-CB*CB-CG*CG+2.0*CA*CB*CG)
      MATRIX(1,1) = A
      MATRIX(2,1) = 0.0
      MATRIX(3,1) = 0.0
      MATRIX(1,2) = B*CG
      MATRIX(2,2) = B*SG
      MATRIX(3,2) = 0.0
      MATRIX(1,3) = C*CB
      MATRIX(2,3) = C*(CA-CB*CG)/SG
      MATRIX(3,3) = VOLU/(A*B*SG)

      END SUBROUTINE FRAC2PDB
!
!*****************************************************************************
!
