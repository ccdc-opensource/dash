!
!*****************************************************************************
!
      SUBROUTINE FoolCompiler
!
! This subroutines holds type declarations and descriptions of variables in common blocks.
! By making it into a subroutine, it will be compiled and can be browsed with the source browser.
! Theoretically, if you want to know a variable's use, just type its name in the source browser
! and jump to its entry in the file declarations.f90.
!
      USE ATMVAR
      USE VARIABLES
      
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

! CurrentWizardWindow = Winteracter ID of current Wizard window

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard

! These variables store the position of the Wizard dialogue windows.
! This is necessary to preserve these coordinates when a window is
! displayed, moved, closed and then displayed again.

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

! MOBS  = (=15000) Maximum number of observations, i.e. data points in powder pattern
! NOBS  = Number of data points in powder pattern
! XOBS  = 2 theta
! YOBS  = observed number of counts ('intensity') for the corresponding 2 theta value
!        (this should have been a 'struct'/'record', but that didn't exist yet in FORTRAN when this was programmed)
! EOBS  = estimated standard deviation of the observed number of counts ('intensity')
!         for the corresponding 2 theta value
!
! Note: there are other COMMON blocks holding the same variables, sometimes with the same name

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

! MOBS  = (=15000) Maximum number of observations, i.e. data points in powder pattern
! NBIN  = Number of bins
! LBIN  = width of bin (=1 always)
! XBIN  = 2 theta
! YOBIN = observed number of counts ('intensity') for the corresponding 2 theta value
!        (this should have been a 'struct'/'record', but that didn't exist yet in FORTRAN when this was programmed)
! YCBIN = calculated number of counts ('intensity') for the corresponding 2 theta value
! YBBIN = background number of counts ('intensity') for the corresponding 2 theta value
! EBIN  = estimated standard deviation of the observed number of counts ('intensity')
!         for the corresponding 2 theta value
!

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

! NPTS is approximately NBIN.
! These variables hold the profile during peak fitting and Pawley refinement.
! ZARGI = XBIN
! ZOBS = YOBIN
! ZDOBS = EBIN
! KOBZ holds for each reflection the data point closest to it.
!!! ???? Is that true? Then why not KOBZ(MFCSTO) ???

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MOBS)

! ZCAL = YCBIN

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

! ZARGK = 2 theta per reflection ?


! Hold output from Pawley refinement and multi-peak fitter.

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

! Various maxima and minima related to graphics
! XPMIN = XBIN(1), always, regardless of zoom
! XPMAX = XBIN(NBIN), always, regardless of zoom

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

! IPMIN = bin number (so, pointer into XBIN) of the first bin visible on screen
!         only useful when the user has zoomed in, otherwise IPMIN = 1
! IPMAX = bin number (so, pointer into XBIN) of the last bin visible on screen
!         only useful when the user has zoomed in, otherwise IPMAX = NBIN
! iStart = bin number (so, pointer into XBIN) of the first bin that should be drawn, i.e. MAX(IPMIN-1,1)
!          only useful when the user has zoomed in, otherwise iStart = 1
! iStop = bin number (so, pointer into XBIN) of the last bin that should be drawn, i.e. MIN(IPMAX+1,NBIN)
!         only useful when the user has zoomed in, otherwise iStop = NBIN
! nPoints = 1 + iStop - iStart


      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

! PKFNVA while fitting Peak Fit Ranges, this holds the parameters that are fitted:
!        sigma = PKFNVA(1)
!        gamma = PKFNVA(2)
!        hpsl  = PKFNVA(3)
!        hmsl  = PKFNVA(4)


!O! JCC GGCALC dimension increased to 500
!O      REAL            REFH,           AMUL
!O      REAL            ESDOBS,         SOMEGA,       GGCALC
!O	  
!O      INTEGER         MAXKK, KMIN, KMAX, KMOD, KNOW
!O      INTEGER                                           ISMAG
!O      REAL            DKDDS
!O      INTEGER                KOM23
!O
!O      COMMON /REFLNS/ REFH(3,MFCSTO), AMUL(MFCSTO),                                   &
!O	                  ESDOBS(MFCSTO), SOMEGA(MFCSTO), GGCALC(500),                    &
!O                      MAXKK(9), KMIN, KMAX, KMOD, KNOW, ISMAG(MFCSTO), &
!O                      DKDDS, KOM23
!O
!O      INTEGER         MAXK
!O
!O      EQUIVALENCE (MAXK,MAXKK(1))

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

! These variables have to do with the peak fit ranges as selected by the user
!
! MAX_NPFR (= 50) = MAXimum Number of Peak Fit Ranges
! MAX_NPPR (= MPeak = 10) = MAXimum Number of Peaks per Peak Fit Range
! XPF_Range(1,i)  = start of hatched area
! XPF_Range(2,i)  = end   of hatched area
! RangeFitYN      = has this range been fitted Yes / No
! IPF_Lo          = designates bin (so, pointer into XBIN) where the peak fit range starts.
! IPF_Hi          = designates bin (so, pointer into XBIN) where the peak fit range ends.
! NumPeakFitRange = Number of peak fit ranges (hatched areas on the screen)
! CurrentRange    = global variable used to indicate which peak fit range we are
!                   dealing with at the moment, instead of passing this as an argument
!                   across subroutines.
! IPF_Range       = Number of points in this range (should be 1 + IPF_Hi - IPF_Lo)
! NumInPFR        = Number of peaks in this fit range (if user has indicated peak positions)
! XPF_Pos         = 2 theta of the peak position, as entered by the user. This is NOT the fitted peak position.
! YPF_Pos         = Calculated number of counts of the peak position
! IPF_RPt         = pointer into XPeakFit/YPeakFit where the calculated points for this peak start
! MAX_FITPT (= 10,000) = MAXimum number of FIT PoinTs
! XPeakFit        = 2 theta of points of calculated peak (should all be equal to their corresponding XOBS/XBIN)
! YPeakFit        = y-values of points of calculated peaks
! PF_FWHM         = Normalised Full Width at Half Maximum for the peaks in this hatched area

      INTEGER        CurrHiLiPFR
      COMMON /HLPFR/ CurrHiLiPFR
! CurrHiLiPFR = Current HighLighted Peak Fit Range
! For highlighting peak fit ranges while the mouse cursor is over them.

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

! These variables have to do with the peak shape parameters as calculated per peak fit range.
! Note that one peak fit range can have more than one peak position.
!

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

! These variables have to do with the overall peak shape parameters as calculated from the
! Pawley Refinement.
!

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

! These variables have to do with the peak positions as calculated _per peak_ selected by the user.
! Note that one peak fit range can have more than one peak position.
!
! MTPeak        = (=100) maximum number of peak positions used for refinement of unit cell parameters
! NTPeak        = Total number of peaks
! AllPkPosVal   = position of the peak
! AllPkPosEsd   = estimated standard deviation of the peak
! PkProb        = probability that this peak position belongs to the reflection indices stored in IHPk
! IOrdTem       = list of pointers into AllPkPosVal ordered in ascending order
!                 so: 
!                      AllPkPosVal(IOrdTem(     1)) = lowest  2 theta at which a peak has been fitted
!                      AllPkPosVal(IOrdTem(NTPeak)) = highest 2 theta at which a peak has been fitted
! IHPk          = the most probable h, k, l indices for this peak.
!
! Calculations are done in Upload_Positions()
! Note that they use a mixture of variables from /PEAKFIT1/, /PEAKFIT2/ and /ALLPEAKS/ 
! Note that there is nothing temporary about IOrdTem
! It's probably better to get rid of IOrdTem and sort the list directly
! If the right algorithm is chosen, that would speed up multiple sorts and reduce memory consumption.

      INTEGER         IBACK, NBACK
      REAL                             ARGBAK,        BACKGD
      INTEGER                                                        KBCKGD,        NBK, LBKD
      LOGICAL                                                                                      ZBAKIN
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5), KBCKGD(100,5), NBK, LBKD(20), ZBAKIN

! IBACK  = background type? (e.g. Fourier, polynomial)
! NBACK  = Number of BACKground parameters, per source
!
! BACKGD = the actual background parameters, per source
!
! NBK    = Number of Background Cards
! LBKD   = number of parameters per background card
! ZBAKIN = whether or not to include background contributions to profile chi squared for
!          regions without peak contributions.

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

! These are the values from the SA Parameter Bound Wizard window, stored to track changes made
! by the user. Must be initialised before the SA Parameter Bound Wizard window is displayed.

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

! Note that the variable names in this COMMON block are not consistent.

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

! MVAR = 100
! XOPT = values of the parameters of the best SA solution so far

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)
! KREFT = number of reflections contributing to this bin.

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)
!
! Per bin, stores contributions per reflection.
!
      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)
! NFITA & IFITA : for speed up of calculation of profile chi-squared only
! NFITA = number of bins that have a contribution from a reflection
! IFITA = a mapping of those bins to YOBIN(MOBS) 
! WTSA  = weights
      REAL            FOB
      COMMON /FCSTOR/ FOB(MaxAtm_3,MFCSTO)

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)
! NPAR is the number of degrees of freedom that have not been fixed by the user
! IP is their mapping

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

! Curr_SA_Run = The number of the current SA run
! NumOf_SA_Runs = number of completed SA runs
! MaxRuns = maximum number of runs requested by the user. (Note difference with MaxRun)

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      INTEGER         NStPar
      COMMON /pextra/ NStPar

! Yet another variable to hold the number of SA parameters
! The full collection is now:
!
! nvar   = number of variables
! npar   = number of parameters
! NStPar = number of structural parameters (i.e. those changing the fractional co-ordinates)

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

! OutputFilesBaseName = basename for output files.
!         E.g., if OutputFilesBaseName = 'C:\Program Files\CCDC\DASH 2.0\benzene'
!         then the following files can be written out:
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.pdb'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.ccl'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.cssr'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.pro'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.log'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.chi'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.cif'
!         'C:\Program Files\CCDC\DASH 2.0\benzene_001.res'
! where 001 = SA_RunNumberStr = SA_Run_Number in format '(I3.3)'
!
! The status of OutputFilesBaseName has been upgraded from basis-for-SA-output to
! basis-for-all-output. Its name is therefore now wrong. It is the default name for
! all .sdi, .hkl, .dash etc. etc. files.


      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

! Is_SX  = .TRUE. indicates that we are dealing with single crystal data. This is currently
! relevant for the plotting of chi-sqrd vs. number of moves during the SA (powder uses the profile chi-sqrd,
! single crystal uses the intensity chi-sqrd) but may also become more important later on due
! to the subtle differences between a real powder pattern and a powder pattern generated from single cystal data.

      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA
! Flag set in the Analyse Solutions Window to indicate that the user has pressed the
! "Resume SA" button.

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

! in_batch indicates that we are in batch mode, i.e. no user input can be expected.

      END SUBROUTINE FoolCompiler
!
!*****************************************************************************
!
