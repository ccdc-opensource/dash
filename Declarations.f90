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
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER    MVAR
      PARAMETER (MVAR = 100)
! Maximum number of 'parameters' / 'variables' (= degrees of freedom) in the SA

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
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

! NPTS is approximately NBIN.
! These variables hold the profile during peak fitting and Pawley refinement.
! KOBZ holds for each reflection the data point closest to it.
! As such, it should have been dimensioned KOBZ(REFDIM)

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MPPTS)

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

      INTEGER          NTIC
      INTEGER                IH
      REAL                               ARGK
      REAL                                           DSTAR
      COMMON /PROFTIC/ NTIC, IH(3,MTIC), ARGK(MTIC), DSTAR(MTIC)

! MTIC  = (=10000) maximum number of reflections. Although 350, 360 and 400 are mentioned as well.
! NTIC  = number of tick marks
! IH    = h, k and l
! ARGK  = 2 theta value, corrected for zero point error
! DSTAR = d*
!
! Note: exactly the same information is held in two other common blocks.
! Of these, I deleted the following one (merged it with /PROFTIC/):
! COMMON /FCSPC2/ ARGK(MFCSP2), DSTAR(MFCSP2)


!U      REAL REFH(3,REFDIM),AMUL(REFDIM),AICALC(REFDIM),AIOBS(REFDIM),    &
!U     &     ESDOBS(REFDIM),SOMEGA(REFDIM),GGCALC(500),DSTAR(REFDIM)
!U      INTEGER KMIN,KMAX,KMOD,KNOW
!U      INTEGER ISMAG(REFDIM)
!U      REAL DKDDS
!U      INTEGER MAXKK(9),KOM23
!U      INTEGER MAXK
!U
!U      COMMON /REFLNS/REFH,AMUL,AICALC,                                  &
!U     & AIOBS,ESDOBS,SOMEGA,GGCALC,                                      &
!U     & MAXKK,KMIN,KMAX,KMOD,KNOW,DSTAR,ISMAG,                           &
!U     & DKDDS,KOM23
!U
!U      EQUIVALENCE (MAXK,MAXKK(1))

! Note: this DSTAR is _NOT_ corrected for the zero point error.

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
! @@ RangeFitYN has not been implemented yet.
! IPF_Lo          = designates bin (so, pointer into XBIN) where the peak fit range starts.
! IPF_Hi          = designates bin (so, pointer into XBIN) where the peak fit range ends.
! NumPeakFitRange = Number of peak fit ranges (hatched areas on the screen)
! CurrentRange    = global variable used to indicate which peak fit range we are
!                   dealing with at the moment, instead of passing this as an argument
!                   across subroutines.
! IPF_Range       = Number of points in this range (should be IPF_Hi - IPF_Lo, I guess)
! NumInPFR        = Number of peaks in this fit range (if user has indicated peak positions)
! XPF_Pos         = 2 theta of the peak position, not sure about the difference wrt PkPosVal
! YPF_Pos         = Calculated number of counts of the peak position
! IPF_RPt         = pointer into XPeakFit/YPeakFit where the calculated points for this peak start
! MAX_FITPT (= 10000) = MAXimum number of FIT PoinTs
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
! If the right algorithm is chosen, that would speed up multiple sorts and reduce memory.

      INTEGER         IBACK, NBACK
      REAL                             ARGBAK,        BACKGD
      INTEGER                                                        KBCKGD,        NBK, LBKD
      LOGICAL                                                                                      ZBAKIN
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5), KBCKGD(100,5), NBK, LBKD(20), ZBAKIN

      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

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
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

! Note that the variable names in this COMMON block are not consistent.
! Note that '150' should be equal to MAXATM, which is 100

      REAL              BICALC,         XICALC
      COMMON /SAREFLN2/ BICALC(MSAREF), XICALC(MSAREF)
! BICALC(1:MAXK) = the calculated intensities corrected for preferred orientation
! XICALC(1:MAXK) = the preferred orientation part of the calculated intensities

      INTEGER           iHMUL
      COMMON /SAREFLN3/ iHMUL(MSAREF)

      REAL            XATOPT
      COMMON /posopt/ XATOPT(3,150)

! Co-ordinates of the atoms of the asymmetric unit of the best SA solution so far.

      DOUBLE PRECISION XOPT,       C,       XP,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), XP(MVAR), FOPT

! MVAR = 100 (the variable formerly also known as NMAX)
! XOPT = values of the parameters of the best SA solution so far

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER         MAXK
      REAL                  FOB
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)
! NPAR is the number of degrees of freedom that have not been fixed by the user
! IP is their mapping

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      REAL            bchmin, bpwval, bchpro, tempvl, avchi1, avchi2, avchi3, avchi4
      INTEGER         nd1, nmpert, nd3, nd4, bmIHANDLE
      COMMON /sagdat/ bchmin, bpwval, bchpro, tempvl, avchi1, avchi2, avchi3, avchi4, &
                      nd1, nmpert, nd3, nd4, bmIHANDLE

      INTEGER         NStPar
      COMMON /pextra/ NStPar

! Yet another variable to hold the number of SA parameters
! The full collection is now:
!
! nvar   = number of variables
! npar   = number of parameters
! NStPar = number of structural parameters (i.e. those changing the fractional co-ordinates)

      END SUBROUTINE FoolCompiler
!
!*****************************************************************************
!
