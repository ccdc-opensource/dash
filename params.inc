! For common block FCSTOR

	  INTEGER MFCSTO
      PARAMETER ( MFCSTO = 10000 )

! For common block FCSPEC

      INTEGER MFCSPE
      PARAMETER ( MFCSPE = 10000 )

! For common block SAREFLNS

      INTEGER MSAREF
      PARAMETER ( MSAREF = 10000 )

! For common block CHISTO

      INTEGER MCHIHS
      PARAMETER ( MCHIHS = 40000 )

! For common block CHISTOP and CHIBEST

      INTEGER MCHSTP
      PARAMETER ( MCHSTP = 15000 )

! For common blocks CHIFS1

      INTEGER MCHFS1
      PARAMETER ( MCHFS1 = 40000 )

! For common block CHIFS2

      INTEGER MCHFS2
      PARAMETER ( MCHFS2 = 10000 )
    
! For common block PRJMAT

      INTEGER MPRJMT
      PARAMETER ( MPRJMT = 10000 )

! For common block FPINF, FPINF1

      INTEGER MFPINF
      PARAMETER ( MFPINF = 10000 )

! For use in common block CMN299

      INTEGER MAXPIK, MPTS
      PARAMETER ( MAXPIK = 200000, MPTS = 15000 )

! For use in common block POSCMN

      INTEGER MPSCMN
      PARAMETER (MPSCMN = 10000) 

! For use in common block F4PARS

      INTEGER MF4PAR
      PARAMETER ( MF4PAR = 10000 )

! For use in common block PRPKFN

      INTEGER MPRPKF
      PARAMETER ( MPRPKF = 10000 )

! For use in common block FOCDAT

      INTEGER MFOCDA
      PARAMETER ( MFOCDA = 33000 )

! GUI Parameters
! For common block PROFOBS and PROFBIN

      INTEGER MOBS
      PARAMETER ( MOBS = 15000 )

! For common block PROFTIC

      INTEGER MTIC
      PARAMETER ( MTIC = 10000 )

! For common block PEAKFIT2

      INTEGER MAX_NPFR, MAX_NPPR, MAX_FITPT, MPkDes
      PARAMETER ( MAX_NPFR = 50, MAX_NPPR = 10, MAX_FITPT = 10000 )
      PARAMETER ( MPkDes = 6 )

! For use in Quick_Pawley

      INTEGER QPFDIM
      PARAMETER ( QPFDIM = 100000 )

! For use in common block ZSTORE

      INTEGER MPPTS
      PARAMETER ( MPPTS = 15000 )

! For use in common block TICCOMM

      INTEGER MObsTic
      PARAMETER ( MObsTic = 50 )

! For use in common block REFLNZ and WWREFLNZ

      INTEGER MRFLNZ
      PARAMETER ( MRFLNZ = 10000 )

! For use in common block IPKCMN

      INTEGER MIPK
      PARAMETER ( MIPK = 500 )

! For use in common block SCRAT 

      INTEGER ITMREF
      PARAMETER ( ITMREF = 10000 )

! For use in common block WWREFLN (cf common block in REFLNS.INC)

      INTEGER WWREFDIM
      PARAMETER ( WWREFDIM = 10000 )

! For use in common block ALLPEAKS

      INTEGER MTPeak
      PARAMETER ( MTPeak = 100 )

! The Maximum number of Child Windows allowed (determined by Winteracter)
! Used in common blocks SACHILDWINDOWS and CHISQDWINDOWSUSED

      INTEGER MaxNumChildWin
      PARAMETER ( MaxNumChildWin = 20 )

! For use in common blocks ZMCOMI, ZMCOMR, ZMCOMC, ZMCOMO, FRGCOM, FRGCHA, ZMCOMG, ZMLGOT, ZMNPAR

	  INTEGER maxfrg
      PARAMETER ( maxfrg = 20 )

! maxfrg = Maximum number of fragments = individual z-matrices

      INTEGER MaxDOF
	  PARAMETER ( MaxDOF = 30 )

! MaxDOF = Maximum number of degrees of freedom per fragment (= per individual z-matrix)

! For use in common blocks ZMCOMI, ZMCOMR, ZMCOMC, ZMCOMO, FRGCOM, FRGCHA, ZMCOMG, ZMLGOT, ZMNPAR

      INTEGER maxatm
      PARAMETER ( maxatm = 100 )

! For use in common block PLTSTO2

	  INTEGER  maxiter
      PARAMETER ( maxiter = 10000 )

! maxiter = maximum number of simulated annealing iterations (per individual run when multi run).

