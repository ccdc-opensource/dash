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

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

! MOBS  = (=15000) Maximum number of observations, i.e. data points in powder pattern
! NOBS  = Number of data points in powder pattern
! XOBS  = 2 theta
! YOBS  = observed number of counts ('intensity') for the corresponding 2 theta value
!        (this should have been a 'struct'/'record', but that didn't exist yet in FORTRAN when this was programmed)
! YCAL  = calculated number of counts ('intensity') for the corresponding 2 theta value
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

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

! Various maxima and minima related to graphics

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

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
! Of these, I deleted the following one (merged it with this one):
! COMMON /FCSPC2/ ARGK(MFCSP2), DSTAR(MFCSP2)

      REAL              XPF_Range
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit

      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                                   &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

! XPF_Range
! IPF_Lo          = designates bin (so, pointer into XBIN) where the peak fit range starts.
! IPF_Hi          = designates bin (so, pointer into XBIN) where the peak fit range ends.
! NumPeakFitRange = Number of peak fit ranges (hatched areas on the screen)
! CurrentRange    = global variable used to indicate which peak fit range we are
!                   dealing with at the moment, instead of passing this as an argument
!                   across subroutines.
! IPF_Range       = 
! NumInPFR        = Number of peaks in this fit range (if user has indicated peak positions)
! XPF_Pos         = 2 theta of the peak position
! YPF_Pos         = number of counts of the peak position
! IPF_RPt
! XPeakFit
! YPeakFit

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
! IHPk          = the most probably h, k, l indices for this peak.
!
! Calculations are done in Upload_Positions()
! Note that they use a mixture of variables from /PEAKFIT1/, /PEAKFIT2/ and /ALLPEAKS/ 
! Note that there is nothing temporary about IOrdTem
! It's probably better to get rid of IOrdTem and sort the list directly
! If the right algorithm is chosen, that would speed up multiple sorts and reduce memory.

      END SUBROUTINE FoolCompiler
!
!*****************************************************************************
!
