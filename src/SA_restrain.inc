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
! Include declarations here for SA_Restraints
! rather than hard-coded ... you know it makes sense

      INTEGER, PARAMETER :: MaxSADRestr = 5
!     Max number of bond restraints for SA

      REAL                SpringWeight, SAPenalty
      COMMON /SA_Penalty/ SpringWeight, SAPenalty

      INTEGER            DRestrNumb, DRestrAtomIDs(2,MaxSADRestr)
      REAL               DRestrLens(MaxSADRestr), DRestrWidths(MaxSADRestr), DRestrWeights(MaxSADRestr)
      COMMON /SA_DRestr/ DRestrNumb, DRestrAtomIDs, DRestrLens, DRestrWidths, DRestrWeights
