!
!*****************************************************************************
!
      SUBROUTINE FoolCompiler
!
! This subroutines holds type declarations and descriptions of variables in common blocks.
! By making it into a subroutine, it will be compiled and can be browsed with the source browser.
! Theoretically, if you want to know a variables use, just type its name in the source browser
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
      REAL                         XOBS,       YOBS,       YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), YBAK(MOBS),  EOBS(MOBS)

! MOBS  = (=15000) Maximum number of observations, i.e. data points in powder pattern
! NOBS  = Number of data points in powder pattern
! XOBS  = 2 theta
! YOBS  = observed number of counts ('intensity') for the corresponding 2 theta value
!        (this should have been a 'struct'/'record', but that didn't exist yet in FORTRAN when this was programmed)
! YBAK  = background number of counts ('intensity') for the corresponding 2 theta value
! EOBS  = estimated standard deviation of the observed number of counts ('intensity')
!         for the corresponding 2 theta value
!
! Note: there are other COMMON blocks holding the same variables, sometimes with the same name

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

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
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

! NPTS is approximately NBIN.
! These variables hold the profile during peak fitting and Pawley refinement.
! ZARGI = XBIN
! ZOBS = YOBIN
! ZDOBS = EBIN
! KOBZ holds for each reflection the data point closest to it.

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MOBS)

! Hold output from Pawley refinement and multi-peak fitter.

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

! Various maxima and minima related to graphics
! XPMIN = XBIN(1)
! XPMAX = XBIN(NBIN)

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

! IPMIN = bin number (so, pointer into XBIN) of the first bin visible on screen
!         only useful when the user has zoomed in, otherwise IPMIN = 1
! IPMAX = bin number (so, pointer into XBIN) of the last bin visible on screen
!         only useful when the user has zoomed in, otherwise IPMAX = NBIN

      INTEGER         NLGREF
      LOGICAL                 LOGREF
      COMMON /FCSPEC/ NLGREF, LOGREF(8,MFCSTO)

! Logicals indicating which reflections are absent.

      REAL              BICALC,         XICALC
      COMMON /SAREFLN2/ BICALC(MFCSTO), XICALC(MFCSTO)

! AIOBS = observed intensity, per reflection
! AICALC = part of the calculated intensity due to structural parameters (atoms)
! BICALC = AICALC * XICALC = calculated intensity
! XICALC = part of the calculated intensity due to preferred orientation
 
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
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

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

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkFnVarVal,                   PkFnVarEsd,                   &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

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

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

! Note that the variable names in this COMMON block are not consistent.

      INTEGER           iHMUL
      COMMON /SAREFLN3/ iHMUL(MFCSTO)

      REAL                XAtmCoords
      COMMON /PDBOVERLAP/ XAtmCoords(1:3,1:MaxAtm_3,1:MaxRun)

! Co-ordinates of the atoms of the asymmetric unit of the best SA solution so far per run.

      DOUBLE PRECISION XOPT,       C,       XP,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), XP(MVAR), FOPT

! MVAR = 100 (the variable formerly also known as NMAX, almost subtly different from MPAR)
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

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

! RESTART = logical indicating whether multirun or not
! Curr_SA_Run = The number of the current SA run
! NumOf_SA_Runs = number of completed SA runs
! MaxRuns = maximum number of runs requested by the user. (Note difference with MaxRun)

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      REAL            bchmin, bpwval, bchpro, avchi1, avchi2, avchi3, avchi4
      INTEGER         nd1, nmpert, nd3, nd4, bmIHANDLE
      COMMON /sagdat/ bchmin, bpwval, bchpro, avchi1, avchi2, avchi3, avchi4, &
                      nd1, nmpert, nd3, nd4, bmIHANDLE

      INTEGER         NStPar
      COMMON /pextra/ NStPar

! Yet another variable to hold the number of SA parameters
! The full collection is now:
!
! nvar   = number of variables
! npar   = number of parameters
! NStPar = number of structural parameters (i.e. those changing the fractional co-ordinates)

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

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

      END SUBROUTINE FoolCompiler
!
!*****************************************************************************
!
