      SUBROUTINE Spline_Background
!U
!U! Algorithm:
!U!
!U! 1. Define all points on the profile by using user defined positions. Initially these are defined
!U! from the unit cell reflection positions using a fitness equation (as yet undefined) or from user-given 
!U! points in the profile
!U!
!U! 2. Fit a cubic spline to these points and use this for background correction
!U!
!U! Notes:
!U!
!U! This algorithm uses the information from the indexing. If your indexing is bad, then
!U! it has no chance!
!U
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U      USE VARIABLES
!U
!U      INCLUDE 'POLY_COLOURS.INC'
!U      INCLUDE 'PARAMS.INC'
!U      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
!U
!U      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
!U        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
!U        XGGMIN,XGGMAX,YGGMIN,YGGMAX
!U
!U      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
!U      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
!U
!U      REAL TicVal(Mtic),XSpline(Mtic),YSpline(Mtic),DSpline(Mtic),NSpline,Xtem,Ytem
!U      INTEGER Npos,Count,Nstep
!U      REAL Del
!U
!U      Npos = 0
!U! Calculate the mean intensity over the profile and the mean intensity 
!U! over the reflection positions
!U      DO I = 1, MTIC
!U! Modify the position 
!U        IF (I .GT. 1) THEN
!U          Del =  ARGK(I) - ARGK(I-1)
!U        ELSE
!U          Del =  ARGK(I) - XOBS(1)
!U        ENDIF
!U! we want points between the peaks that are well separated
!U        IF (Del .GT. 0.25 ) THEN  ! This is selection is hard coded here but really should be
!U                              ! dependant on the estimated peak width at this position
!U           Npos = Npos + 1
!U           TicVal(Npos) = ARGK(I) - (Del/2.0)
!U        END IF
!U      END DO
!U      Count = 1
!U      DO I = 1, NOBS-1
!U        IF (XOBS(I).LE.TicVal(Count) .AND. XOBS(I+1).GE.TicVal(Count)) THEN
!U          XSpline(Count) = XOBS(I)
!U          YSpline(Count) = YOBS(I)
!U          Count = Count + 1
!U          IF (Count .GT. Npos) GOTO 99
!U        END IF
!U      END DO
!U 99   CONTINUE
!U      CALL IGrColourN(KolNumObs)
!U! JvdS IPgNewGraph is now obsolete and should be replaced by IPgNewPlot
!U! I don't know the equivalent of 'S'
!U!      CALL IPgNewPlot(PgPolyLine,1,NBIN,??)
!U      CALL IPgNewGraph(1,Npos,' ','S','X')
!U      CALL IPgStyle(1,0,0,0,KolNumDif,0)
!U      CALL IPgXYPairs(XSpline,YSpline)
!U      CALL IGrColourN(KolNumMain)
!U
      END SUBROUTINE Spline_Background
!U
