! For common block FCSTOR

	  INTEGER MFCSTO
      PARAMETER ( MFCSTO = 10000 )

! For common block FCSPEC

      INTEGER MFCSPE
      PARAMETER ( MFCSPE = MFCSTO )

! For common block SAREFLNS

      INTEGER MSAREF
      PARAMETER ( MSAREF = MFCSTO )

! Maximum number of Simulated Annealing REFlections

! For common block CHISTO

      INTEGER MCHIHS
      PARAMETER ( MCHIHS = 40000 )

! For common block FPINF, FPINF1

      INTEGER MFPINF
! JvdS I increased this from 10000 to 15000, as that is the value it can get in GETPIK()
      PARAMETER ( MFPINF = 15000 )

! For use in common block CMN299

      INTEGER MAXPIK, MPTS
      PARAMETER ( MAXPIK = 200000, MPTS = 15000 )

! For use in common block F4PARS

      INTEGER MF4PAR
      PARAMETER ( MF4PAR = 10000 )

! For use in common block PRPKFN

      INTEGER MPRPKF
      PARAMETER ( MPRPKF = 10000 )

! GUI Parameters
! For common block PROFOBS and PROFBIN

      INTEGER MOBS
      PARAMETER ( MOBS = 15000 )

! Note: The following parameters seem to be related to MOBS (i.e., should probably have the same value)
! MPPTS
! MFPINF

! For common block PROFTIC

      INTEGER MTIC
      PARAMETER ( MTIC = MFCSTO )

! For common block PEAKFIT2

      INTEGER MAX_NPFR, MAX_NPPR, MAX_FITPT, MPkDes
      PARAMETER ( MAX_NPFR = 50, MAX_NPPR = 10, MAX_FITPT = 10000 )
      PARAMETER ( MPkDes = 6 )

! For use in common block ZSTORE

      INTEGER MPPTS
      PARAMETER ( MPPTS = 15000 )

! For use in common block REFLNZ and WWREFLNZ

      INTEGER MRFLNZ
      PARAMETER ( MRFLNZ = MFCSTO )

! For use in common block IPKCMN

      INTEGER MIPK
      PARAMETER ( MIPK = 500 )

! For use in common block SCRAT 

      INTEGER ITMREF
      PARAMETER ( ITMREF = 10000 )

! For use in common block WWREFLN (cf common block in REFLNS.INC)

      INTEGER WWREFDIM
      PARAMETER ( WWREFDIM = MFCSTO )

! For use in common block ALLPEAKS

      INTEGER MTPeak
      PARAMETER ( MTPeak = 100 )

! The Maximum number of Child Windows allowed (determined by Winteracter)
! Used in common blocks SACHILDWINDOWS and CHISQDWINDOWSUSED

      INTEGER MaxNumChildWin
      PARAMETER ( MaxNumChildWin = 20 )

! For use in common block PLTSTO2

	  INTEGER  maxiter
      PARAMETER ( maxiter = 10000 )

! maxiter = maximum number of simulated annealing iterations (per individual run when multi run).

       INTEGER MaxRun
	   PARAMETER ( MaxRun = 99 )
! Maximum number of runs in a multirun.
! Don't forget to update the Winteracter grids accordingly.
! Note the difference with MaxRuns: that's the number of runs requested by the user.

       INTEGER MaxBVar
       PARAMETER ( MaxBVar = 400 )
! Maximum number of basic variables. Not quite sure what a basic variable is,
! but it has to do the number of parameters that can be refined during the Pawley
! refinement, so this is the number that limits the number of reflections used 
! during the Pawley refinement. It is used implicitly in at least one other constant as well,
! so just changing 400 to 500 won't work.

! For use in Quick_Pawley

      INTEGER QPFDIM
      PARAMETER ( QPFDIM = (20000 + (MaxBVar * (MaxBVar+3)) / 2) )
! This is the number of elements in the least squares refinement matrix.
! The extra 20000 looks a bit stupid, but it was there in the original code so I left it in.
! Should not be necessary at all.
