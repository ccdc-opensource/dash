	subroutine Spline_Background

! Algorithm:
!
! 1. Define all points on the profile by using user defined positions. Initially these are defined
! from the unit cell reflection positions using a fitness equation (as yet undefined) or from user-given 
! points in the profile
!
! 2. Fit a cubic spline to these points and use this for background correction
!
! Notes:
!
! This algorithm uses the information from the indexing. If your indexing is bad, then
! it has no chance!

	USE winteracter
	USE druid_header
	USE variables
    INCLUDE 'POLY_COLOURS.INC'
    INCLUDE 'PARAMS.INC'
    COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
    COMMON /PLTYPE/ IPTYPE

	COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
    YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
    XGGMIN,XGGMAX,YGGMIN,YGGMAX

	COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
    COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)

	
	REAL TicVal(Mtic),XSpline(Mtic),YSpline(Mtic),DSpline(Mtic),NSpline,Xtem,Ytem

	Integer Npos,Count,Nstep
	REAL Del
    Npos = 0

	! Calculate the mean intensity over the profile and the mean intensity 
	! over the reflection positions




	DO I = 1, MTIC

		! Modify the position 
		IF (I .GT. 1) THEN
			Del =  ARGK(I) - ARGK(I-1)
		ELSE
			Del =  ARGK(I) - XOBS(1)
		ENDIF
		! we want points between the peaks that are well separated


		IF (Del .GT. 0.25 ) THEN  ! This is selection is hard coded here but really should be
								! dependant on the estimated peak width at this position
				Npos = Npos + 1
				TicVal(Npos) = ARGK(I) - (Del/2.0)
		END IF
	END DO

	Count = 1

	DO I = 1,NOBS-1
		IF (XOBS(I).LE.TicVal(Count) .AND. XOBS(I+1).GE.TicVal(Count)) THEN
			XSpline(Count) = XOBS(I)
			YSpline(Count) = YOBS(I)
			Count = Count + 1
			IF (Count .GT. Npos) GOTO 99
		END IF
	END DO
 99 CONTINUE

    CALL IGrColourN(KolNumObs)
!
    CALL IPgNewGraph(1,Npos,' ','S','X')

!
    CALL IPgStyle(1,0,0,0,KolNumDif,0)
	CALL IPgXYPairs(XSpline,YSpline)

	CALL IGrColourN(KolNumMain)

	end subroutine Spline_Background
