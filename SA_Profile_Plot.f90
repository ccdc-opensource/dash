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

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),&
     YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
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
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),&
     IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),&
     NumPeakFitRange,CurrentRange,&
     IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),&
     XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),&
     IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),&
     ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      COMMON /CHISTOP/ NOBSA,NFITA,IFITA(MCHSTP),CHIOBSA,&
      WTSA(MCHSTP),XOBSA(MCHSTP),YOBSA(MCHSTP),YCALA(MCHSTP),ESDA(MCHSTP)
      COMMON /chibest/ ycalbest(MCHSTP)

      NOBS = NOBSA
      NBIN = NOBSA
      YOSUM = 0.0
      YCSUM = 0.0
      DO II=1,NFITA
        I=IFITA(II)
        YOSUM=YOSUM+YOBSA(I)
        YCSUM=YCSUM+YCALbest(I)
      END DO
      RESCL=YOSUM/YCSUM
      DO I=1,NOBS
        YCALbest(i)=RESCL*YCALbest(I)
      END DO
      DO I=1,NBIN
        XBIN(I)=XOBSA(I)
        YOBIN(I)=YOBSA(I)
        YCBIN(I)=YCALBEST(I)
        YBBIN(I)=0.0
        EBIN(I)=EsdA(I)
        XOBS(I)=XOBSA(I)
        YOBS(I)=YOBSA(I)
        YCAL(I)=YCALBEST(I)
        YBAK(I)=0.0
        EOBS(I)=EsdA(I)
      END DO
      IF (num_sa_profile_plot .EQ. 1) THEN
        XPMIN=XOBSA(1)
        XPMAX=XOBSA(1)
        YPMIN=YOBSA(1)
        YPMAX=YOBSA(1)
        DO I=1,NOBS
          XPMIN=MIN(XOBSA(I),XPMIN)
          XPMAX=MAX(XOBSA(I),XPMAX)
          YPMIN=MIN(YOBSA(I),YPMIN)
          YPMAX=MAX(YOBSA(I),YPMAX)
        END DO
        XPGMIN=XPMIN
        XPGMAX=XPMAX
        YPGMIN=YPMIN
        YPGMAX=YPMAX
        CALL UPLOAD_RANGE()
        XPGMINOLD=XPMIN
        XPGMAXOLD=XPMAX
        YPGMINOLD=YPMIN
        YPGMAXOLD=YPMAX
        IPMIN=1
        IPMAX=NBIN
        IPMINOLD=IPMIN
        IPMAXOLD=IPMAX
      END IF
      IPTYPE=2
      CALL Profile_Plot(IPTYPE)
 999  CONTINUE

      END SUBROUTINE Load_SA_PRO
!
!*****************************************************************************
!
