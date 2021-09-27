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

      REAL            SlimValue, ScalFac
      LOGICAL                             BackRef
      COMMON /PWLYST/ SlimValue, ScalFac, BackRef

      REAL :: CellPar, ZeroPoint
	  LOGICAL CellParConstrained 
      COMMON /CELLREF/ CellPar(6), ZeroPoint, CellParConstrained(6)

      INTEGER MVAL
      PARAMETER (MVAL=50)
      INTEGER LatBrav
      INTEGER IHLR
      COMMON /LATREFCMN/ LatBrav, IHLR(3,MVAL) 

! LatBrav is the crystal system (NOT the Bravais lattice)
!
!            1 = Triclinic
!            2 = Monoclinic-a
!            3 = Monoclinic-b
!            4 = Monoclinic-c
!            5 = Orthorhombic
!            6 = Tetragonal
!            7 = Trigonal
!            8 = Rhombohedral
!            9 = Hexagonal
!           10 = Cubic
!

      CHARACTER*12   CrystalSystemString(10)
	  COMMON /CrySysStr/ CrystalSystemString

      INTEGER MaxSPGR
      PARAMETER (MaxSPGR=530)
! MaxSPGR is the maximum number of space groups including all different settings.
! These are loaded from a file at the beginning.

      INTEGER          LPosSG
      CHARACTER*12                  SGNumStr
      CHARACTER*12                                     SGHMaStr
      CHARACTER*24                                                        SGShmStr
      COMMON /SGStrCmn/ LPosSG(12), SGNumStr(MaxSPGR), SGHMaStr(MaxSPGR), SGShmStr(MaxSPGR)

! LPosSG contains the offset per crystal system in the space group list
! E.g., the monoclinic-a (crystal system nr. 2) space groups start at entry 3 and
! end at entry 37
!      LPosSG( 1) =   1
!      LPosSG( 2) =   3
!      LPosSG( 3) =  38 
!      LPosSG( 4) =  73
!      LPosSG( 5) = 108
!      LPosSG( 6) = 349
!      LPosSG( 7) = 430
!      LPosSG( 8) = 455
!      LPosSG( 9) = 462
!      LPosSG(10) = 489
!      LPosSG(11) = MaxSPGR+1
!
!      Space group number         : SGNumStr(IPosSg)
!      Space Group (IT tables)    : SGHMaStr(IPosSg)
!      Space Group Hall symbol    : SGHalStr(IPosSg)
!      Space Group explicit symbol: SGShmStr(IPosSg)

      INTEGER          NumberSGTable
      COMMON /NSGTCMN/ NumberSGTable

! NumberSGTable stores the number of the Space Group from the space group table.
! Each setting has its own number, so there are 530 entries in total.
