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

      INTEGER     MFCSTO
      PARAMETER ( MFCSTO = 10000 )
! For common block FCSTOR

      INTEGER     MCHIHS
      PARAMETER ( MCHIHS = 40000 )
! For common block CHISTO

      INTEGER     MAXPIK
      PARAMETER ( MAXPIK = 200000 )
! For use in common block CMN299

      INTEGER     MF4PAR
      PARAMETER ( MF4PAR = 10000 )
! For use in common block F4PARS

      INTEGER     MPRPKF
      PARAMETER ( MPRPKF = 10000 )
! For use in common block PRPKFN

      INTEGER     MOBS
      PARAMETER ( MOBS = 30000 )
! For common block PROFOBS and PROFBIN

      INTEGER MaxKtem
	  PARAMETER ( MaxKtem = 100 )
! Maximum dimension of KNIPT

      INTEGER     MAX_NPFR,      MAX_NPPR,      MAX_FITPT,         MPkDes
      PARAMETER ( MAX_NPFR = 50, MAX_NPPR = 20, MAX_FITPT = 10000, MPkDes = 6 )
! For common block PEAKFIT2

      INTEGER     ITMREF
      PARAMETER ( ITMREF = 10000 )
! For use in common block SCRAT 

      INTEGER     MTPeak
      PARAMETER ( MTPeak = 100 )
! For use in common block ALLPEAKS

      INTEGER     MaxPeaksFound
      PARAMETER ( MaxPeaksFound = 500 )
! The maximum number of peaks that the peak-finding algorithm will find

      INTEGER     MaxNumChildWin
      PARAMETER ( MaxNumChildWin = 20 )
! The Maximum number of Child Windows allowed (determined by Winteracter)
! Used in common blocks SACHILDWINDOWS and CHISQDWINDOWSUSED

      INTEGER     maxiter
      PARAMETER ( maxiter = 20000 )
! maxiter = maximum number of simulated annealing iterations (per individual run when multi run).

      INTEGER     MaxRun
      PARAMETER ( MaxRun = 999 )
! Maximum number of runs in a multirun.
! Don't forget to update the Winteracter grids and the maximum value of the spinner box accordingly.
! Note the difference with MaxRuns: that's the number of runs requested by the user.

      INTEGER     MaxBVar
      PARAMETER ( MaxBVar = 1050 )
! Maximum number of basic variables. Not quite sure what a basic variable is,
! but it has to do the number of parameters that can be refined during the Pawley
! refinement, so this is the number that limits the number of reflections used
! during the Pawley refinement. It is used implicitly in at least one other constant as well,
! so just changing 400 to 500 won't work.

      INTEGER     MaxNumRef
      PARAMETER ( MaxNumRef = MaxBVar-50 )

      INTEGER     MaxVVar
      PARAMETER ( MaxVVar = MaxBVar + 100 )
! MaxVVar = total number of variables (= parameters that are varied)
! MaxBVar = total number of basic variables = variables that do not depend on other variables

      INTEGER     MaxConstraints
      PARAMETER ( MaxConstraints = MaxBVar-100 )

      INTEGER     MaxF2VA ! "Family 2 variables"
      PARAMETER ( MaxF2VA = MaxBVar )

! For use in Quick_Pawley

      INTEGER     QPFDIM
      PARAMETER ( QPFDIM = ((MaxBVar*(MaxBVar+1)) / 2) )
! This is the number of elements in the least squares refinement matrix.

      INTEGER     MVAR
      PARAMETER ( MVAR = 100 )
! Maximum number of 'parameters' / 'variables' (= degrees of freedom) in the SA

      INTEGER     MIPK
      PARAMETER ( MIPK = MaxBVar )
! For use in common block IPKCMN

      INTEGER     MaxAtm_4
      PARAMETER	( MaxAtm_4 = 300)
! Maximum number of atoms

      INTEGER     MaxNumT
      PARAMETER	( MaxNumT = 50)
! Maximum number of Temperatures for parallel tempering

      INTEGER     MaxMogulBin
      PARAMETER ( MaxMogulBin = 20)
! Maximum number of bins for MDB
