!
!*****************************************************************************
!
      SUBROUTINE SA_Profile_Plot(num_sa_profile_plot)

      num_sa_profile_plot = num_sa_profile_plot + 1
      CALL Load_SA_PRO(num_sa_profile_plot)

      END SUBROUTINE SA_Profile_Plot
!
!*****************************************************************************
!
      SUBROUTINE Load_SA_PRO(num_sa_profile_plot)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: num_sa_profile_plot

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

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

      INTEGER         NPTS
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

      REAL            ZCAL,        ZBAK
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XDIFT, XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XDIFT, XMINT

      INTEGER          NOBSA, NFITA, IFITA
      REAL                                          CHIOBSA, WTSA
      REAL             XOBSA,         YOBSA,         YCALA,         ESDA
      COMMON /CHISTOP/ NOBSA, NFITA, IFITA(MCHSTP), CHIOBSA, WTSA(MCHSTP),    &
                       XOBSA(MCHSTP), YOBSA(MCHSTP), YCALA(MCHSTP), ESDA(MCHSTP)

      REAL             ycalbest
      COMMON /chibest/ ycalbest(MCHSTP)

      REAL YOSUM, YCSUM, RESCL
      INTEGER I, II

      NBIN = NOBSA
      YOSUM = 0.0
      YCSUM = 0.0
      DO II = 1, NFITA
        I = IFITA(II)
        YOSUM = YOSUM + YOBSA(I)
        YCSUM = YCSUM + YCALbest(I)
      ENDDO
      RESCL=YOSUM/YCSUM
      DO I = 1, NBIN
        YCALbest(i)=RESCL*YCALbest(I)
      ENDDO
      DO I = 1, NBIN
        XBIN(I)  = XOBSA(I)
        YOBIN(I) = YOBSA(I)
        YCBIN(I) = YCALBEST(I)
        EBIN(I)  = EsdA(I)
      ENDDO
      IF (num_sa_profile_plot .EQ. 1) CALL GetProfileLimits
      IPTYPE = 2
      CALL Profile_Plot
 999  CONTINUE

      END SUBROUTINE Load_SA_PRO
!
!*****************************************************************************
!
