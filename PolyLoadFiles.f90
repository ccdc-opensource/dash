!
! New routines from here on ...
!

!>>
!>> JCC These subroutines are all being changed so that they return status flags
!>> if, for example, a file does not exist, or could not be opened 
!>>
!>>

!>> JCC - was
!      SUBROUTINE Load_Diffraction_File(FLEN,FNAME,NoData)
!>> is now
	  INTEGER FUNCTION Load_Diffraction_File(FLEN,FNAME,NoData)
!
!C>> JCC Also now uses winteracter functions: we could get rid of this if required

	  USE WINTERACTER
	  USE DRUID_HEADER
      CHARACTER(LEN=256),           INTENT (IN) :: FNAME
      INTEGER,                      INTENT (IN) :: FLEN
      LOGICAL, INTENT (IN OUT) :: NoData

      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PLTYPE/ IPTYPE

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
      NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

!C>> JCC The next common allows setting of SLIM, and controls the background options
	  REAL SLIMVALUE, SCALFAC
	  LOGICAL BACKREF
	  COMMON /PWLYST/ SLIMVALUE, SCALFAC, BACKREF

      include 'statlog.inc'
	  CHARACTER*255 Cline
	  INTEGER IS

      LOGICAL ReadWarning
!
!>> JCC - set return value. 1 for success
	  Load_Diffraction_File = 1
	  BACKREF = .TRUE.
!
      OPEN(10,FILE=FNAME(:FLEN),STATUS='OLD',err=999)
!C>>JCC Set the default SA output files 
	  CALL sa_SetOutputFiles(FNAME)
      I=1
!C>> Modified to handle files without esds - used to read in YOBS as the esd

 10   READ(10,'(a)',ERR=999,END=100) Cline
	  READ(Cline,*, IOSTAT = IS) XOBS(I),YOBS(I),EOBS(I)
	  IF (IS .NE. 0) THEN
       READ(Cline,*, ERR=100,END=100) XOBS(I),YOBS(I)
	   IF (YOBS(I) .GE. 0.0) THEN
		   EOBS(I)	= SQRT(YOBS(I))
	   ENDIF
	  ENDIF

! Skip negative 2-theta data and negative intensities

	  IF ( XOBS(I) .LE. 0.0 ) GOTO 10
	  IF ( YOBS(I) .LT. 0.0 ) GOTO 10

! Skip points with zero esd

      IF ( EOBS(I) .LE. 0.0 ) GOTO 10

	  IF (I.GT.1) THEN
		IF ( ABS(XOBS(I) - XOBS(I-1)) .LT. 0.0000001) THEN
			IF (.NOT.ReadWarning) THEN
				ReadWarning = .TRUE.
				CALL WMessageBox(OkOnly,ExclamationIcon,&
				                 CommonOk,&
								 "Warning: The data file contains "//&
								 "multiple observations for the same "//&
								 "2-theta"//CHAR(13)//"Only the first observation"//&
								 "will be used","Bad data file")
			END IF
			GOTO 10
		END IF
	  END IF

      I=I+1

!>> JCC Only read in a maximum of MOBS points
      IF (I.GT.MOBS) THEN
		CALL ProfileRead_TruncationWarning(FNAME,MOBS)
		GOTO 100
	  END IF

      GOTO 10
!
 100  NOBS=I-1
      CLOSE(10)
      DataSetChange=DataSetChange+1

      NumPawleyRef=0
!
      NumObsTic=0
!      NTic=0
      CurrentRange=0
      NumPeakFitRange=0
      Do I=1,MAX_NPFR
        NumInPFR(I)=0
        IPF_RPt(I)=0
      End Do
!
      XPMIN=XOBS(1)
      XPMAX=XOBS(1)
      YPMIN=YOBS(1)
      YPMAX=YOBS(1)
      DO I=1,NOBS
        XPMIN=MIN(XOBS(I),XPMIN)
        XPMAX=MAX(XOBS(I),XPMAX)
        YPMIN=MIN(YOBS(I),YPMIN)
        YPMAX=MAX(YOBS(I),YPMAX)
      END DO
!

      IF (YPMAX .GT. 100000) SCALFAC = 0.01 * YPMAX/100000
      NBIN=(NOBS/LBIN)
      DO I=1,NBIN
        IST=(I-1)*LBIN
        XADD=0.
        YOADD=0.
        VADD=0.
        DO J=1,LBIN
          JJ=J+IST
          XADD=XADD+XOBS(JJ)
          YOADD=YOADD+YOBS(JJ)
          VADD=VADD+EOBS(JJ)**2
        END DO
        XBIN(I)=XADD/FLOAT(LBIN)
        YOBIN(I)=YOADD/FLOAT(LBIN)
        YCBIN(I)=YOBIN(I)
        EBIN(I)=SQRT(VADD)/FLOAT(LBIN)
      END DO
!
      XPGMIN=XPMIN
      XPGMAX=XPMAX
      YPGMIN=YPMIN
      YPGMAX=YPMAX
      XPGMINOLD=XPMIN
      XPGMAXOLD=XPMAX
      YPGMINOLD=YPMIN
      YPGMAXOLD=YPMAX
!
      CALL UPLOAD_RANGE()
!
      IPMIN=1
      IPMAX=NBIN
      IPMINOLD=IPMIN
      IPMAXOLD=IPMAX
!
      IPTYPE=1
      CALL Profile_Plot(IPTYPE)
!
      NoData=.false.

      CALL Background_Fit

!>> JCC added in
      RETURN
 999  Continue
!>> JCC added in return status
	  Load_Diffraction_File = 0
      RETURN
      END FUNCTION Load_Diffraction_File
!
!
!
!*****************************************************************************
!
!

!>> JCC - Was
!>>      SUBROUTINE Load_TIC_File(FLEN,FNAME)
!>> is now
	  INTEGER FUNCTION Load_TIC_File(FLEN,FNAME)
      CHARACTER(LEN=256),           INTENT (IN) :: FNAME
      INTEGER,                      INTENT (IN) :: FLEN

      INCLUDE 'PARAMS.INC'
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /PLTYPE/ IPTYPE
!

!>> JCC - set return status
	  Load_TIC_File = 1
!>> JCC - add in an error trap for bad file opening
      OPEN(11,FILE=FNAME(:FLEN),STATUS='OLD',ERR=999)
      I=1
 10   READ(11,*,ERR=100,END=100) (IH(II,I),II=1,3),ARGK(I),DSTAR(I)
      I=I+1
      GOTO 10
!
 100  NTIC=I-1
      CLOSE(11)
!
!C>> JCC Doing this here can cause problems, so I\'ve moved it
!C      CALL Profile_Plot(iptype)
!
!      CALL View_Tic_File(FLEN,FNAME)
!
!>> JCC Added in the next 2 lines
      RETURN
 999  Load_TIC_File = 0
	  RETURN
      END FUNCTION Load_TIC_File
!
!
!
!*****************************************************************************
!
!>> JCC - Was
!     SUBROUTINE Load_CCL_File(FLEN,FNAME)
!>> Now
      INTEGER FUNCTION Load_CCL_File(FLEN,FNAME)
!
      CHARACTER(LEN=256),           INTENT (IN) :: FNAME
      INTEGER,                      INTENT (IN) :: FLEN
      CHARACTER(LEN=80) CCL_LINE
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
!
!>> JCC Initialise return value
!
	  Load_CCL_File = 1
      ZEROPOINT=0.
      ALAMBDA=1.5404
!
!>> JCC Add in Error trap
!
      OPEN(11,FILE=FNAME(:FLEN),STATUS='OLD', ERR = 999)
!>> JCC Set SA Output files
	  CALL sa_SetOutputFiles(FNAME)
 10   READ(11,5000,ERR=100,END=100) NLCCL,CCL_LINE
 5000 FORMAT(Q,A)
      IF (CCL_LINE(1:1).EQ.'C') THEN
        READ(CCL_LINE(2:NLCCL),*) (CELLPAR(I),I=1,6)
      Else IF (CCL_LINE(1:1).EQ.'L') THEN
        If (CCL_LINE(3:6).eq.'WVLN') Then
          READ(CCL_LINE(7:NLCCL),*) ALAMBDA
        Else If (CCL_LINE(3:6).eq.'ZERO') Then
          READ(CCL_LINE(7:NLCCL),*) zeropoint
        End If
      END IF
      GOTO 10
 100  CLOSE(11)
!
      CALL UpLoad_Crystal_Data()
!
!>> JCC Added in next few lines
	  RETURN
 999  Load_CCL_File = 0
	  RETURN
      END FUNCTION Load_CCL_File
!
!
!
!*****************************************************************************
!
!
!
!
!>> JCC - was
!      SUBROUTINE Load_PRO_File(FLEN,FNAME,NoData)
!>> Changed to
	  INTEGER FUNCTION Load_PRO_File(FLEN,FNAME,NoData)

      CHARACTER(LEN=256),           INTENT (IN) :: FNAME
      INTEGER,                      INTENT (IN) :: FLEN
      LOGICAL, INTENT (IN OUT) :: NoData

      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD


      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
      NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
!
      COMMON /PLTYPE/ IPTYPE
!
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      include 'statlog.inc'
!
!>> JCC Initialise the return status 
!
	  Load_PRO_File = 1
      OPEN(10,FILE=FNAME(:FLEN),STATUS='OLD',Err=999)
      I=1
!>> JCC Set the default output files to <fname>.cssr etc (fname gets any extension removed)
	  CALL sa_SetOutputFiles(FNAME)
 10   READ(10,*,ERR=999,END=100) XOBS(I),YBAK(I),YOBS(I),YCAL(I),EOBS(I)
      I=I+1
!>> JCC Only read in a maximum of MOBS points
      IF (I.GT.MOBS) THEN
		CALL ProfileRead_TruncationWarning(FNAME,MOBS)
		GOTO 100
	  END IF
      GOTO 10
!
 100  NOBS=I-1
      CLOSE(10)
!
      DataSetChange=DataSetChange+1
      NumPawleyRef=0
      NumObsTic=0
!      NTic=0
!      CurrentRange=0
!      NumPeakFitRange=0
!      Do I=1,MAX_NPFR
!        NumInPFR(I)=0
!        IPF_RPt(I)=0
!      End Do
!
!
      NBIN=(NOBS/LBIN)
      DO I=1,NBIN
        IST=(I-1)*LBIN
        XADD=0.
        YOADD=0.
        YCADD=0.
        YBADD=0.
        VADD=0.
        DO J=1,LBIN
          JJ=J+IST
          XADD=XADD+XOBS(JJ)
          YOADD=YOADD+YOBS(JJ)
          YCADD=YCADD+YCAL(JJ)
          YBADD=YBADD+YBAK(JJ)
          VADD=VADD+EOBS(JJ)**2
        END DO
        XBIN(I)=XADD/FLOAT(LBIN)
        YOBIN(I)=YOADD/FLOAT(LBIN)
        YCBIN(I)=YCADD/FLOAT(LBIN)
        YBBIN(I)=YBADD/FLOAT(LBIN)
        EBIN(I)=SQRT(VADD)/FLOAT(LBIN)
      END DO
!
      XPMIN=XOBS(1)
      XPMAX=XOBS(1)
      YPMIN=YOBS(1)
      YPMAX=YOBS(1)
      DO I=1,NOBS
        XPMIN=MIN(XOBS(I),XPMIN)
        XPMAX=MAX(XOBS(I),XPMAX)
        YPMIN=MIN(YOBS(I),YPMIN)
        YPMAX=MAX(YOBS(I),YPMAX)
      END DO
!
      XPGMIN=XPMIN
      XPGMAX=XPMAX
      YPGMIN=YPMIN
      YPGMAX=YPMAX
!
      CALL UPLOAD_RANGE()

      XPGMINOLD=XPMIN
      XPGMAXOLD=XPMAX
      YPGMINOLD=YPMIN
      YPGMAXOLD=YPMAX
      IPMIN=1
      IPMAX=NBIN
      IPMINOLD=IPMIN
      IPMAXOLD=IPMAX
!
      IPTYPE=2
      CALL Profile_Plot(IPTYPE)
!
      NoData=.false.
!>> JCC added in the return to prevent getting to 999 except in error
	  RETURN
 999  Continue
!
!>> JCC - set return status flag
!
	  Load_PRO_File = 0
	  RETURN
      END FUNCTION Load_PRO_File
!
!
!
      SUBROUTINE View_TIC_File(FLEN,FNAME)
!
!
      USE WINTERACTER
      USE druid_header
      TYPE(WIN_STYLE) MYWIN
      CHARACTER(LEN=256),           INTENT (IN) :: FNAME
      INTEGER,                      INTENT (IN) :: FLEN
!
      MYWIN%FLAGS  = SysMenuOn +  &       ! Turn on system menu
                     MinButton +  &       ! Show minimize button
                     MaxButton            ! Show maximize button

      MYWIN%X      = -1                      
      MYWIN%Y      = 100                  ! Put window 100 pixels down
      MYWIN%WIDTH  = 500                      
      MYWIN%HEIGHT = 500                  ! Make window 500 pixels deep
      MYWIN%TITLE  = 'Tic file'//FNAME(:FLEN)            
! Create child window that exists outside of the main window
      CALL WindowOpenChild(MYWIN, IHAND1)
      CALL WEditFile(FNAME(:FLEN),Modeless,0,ViewOnly,CourierNew)
!
      END SUBROUTINE View_TIC_File


!>> JCC  Routine to truncate data to a particular data range
!>> In practice, this varies the NOBS value so the data is not lost, just
!>> Hidden. The routine should not be called with RMaxTTheta greater
!>> than the maximum value read in
	  SUBROUTINE TruncateData(RMaxTTheta)


      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
      NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
!
      COMMON /PLTYPE/ IPTYPE
!
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      include 'statlog.inc'
	  REAL RMaxTTheta
!
	  
	  DO I = 1,NOBS
		IF (XOBS(I) .GT. RMaxTTheta) EXIT
	  END DO
	  IF (I.LE.1) RETURN

	  NOBS = I - 1
      NBIN=(NOBS/LBIN)
      DO I=1,NBIN
        IST=(I-1)*LBIN
        XADD=0.
        YOADD=0.
        YCADD=0.
        YBADD=0.
        VADD=0.
        DO J=1,LBIN
          JJ=J+IST
          XADD=XADD+XOBS(JJ)
          YOADD=YOADD+YOBS(JJ)
          YCADD=YCADD+YCAL(JJ)
          YBADD=YBADD+YBAK(JJ)
          VADD=VADD+EOBS(JJ)**2
        END DO
        XBIN(I)=XADD/FLOAT(LBIN)
        YOBIN(I)=YOADD/FLOAT(LBIN)
        YCBIN(I)=YCADD/FLOAT(LBIN)
        YBBIN(I)=YBADD/FLOAT(LBIN)
        EBIN(I)=SQRT(VADD)/FLOAT(LBIN)
      END DO
!
      XPMIN=XOBS(1)
      XPMAX=XOBS(1)
      YPMIN=YOBS(1)
      YPMAX=YOBS(1)
      DO I=1,NOBS
        XPMIN=MIN(XOBS(I),XPMIN)
        XPMAX=MAX(XOBS(I),XPMAX)
        YPMIN=MIN(YOBS(I),YPMIN)
        YPMAX=MAX(YOBS(I),YPMAX)
      END DO
!
      XPGMIN=XPMIN
      XPGMAX=XPMAX
      YPGMIN=YPMIN
      YPGMAX=YPMAX
!
      CALL UPLOAD_RANGE()

      XPGMINOLD=XPMIN
      XPGMAXOLD=XPMAX
      YPGMINOLD=YPMIN
      YPGMAXOLD=YPMAX
      IPMIN=1
      IPMAX=NBIN
      IPMINOLD=IPMIN
      IPMAXOLD=IPMAX
	  
      CALL Profile_Plot(IPTYPE)

	  END SUBROUTINE TruncateData


	  SUBROUTINE ProfileRead_TruncationWarning(filename, Npoints)
	  USE VARIABLES
	  USE WINTERACTER
	  USE DRUID_HEADER
	  character *(*) filename
	  character *7 cmobs
	  integer len_filename

	  len_filename = len_trim(filename)
	  write(cmobs,'(i7)' ) Npoints

	  CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
	  " The file "//filename(1:len_filename)//" contains greater than "//cmobs(1:len_trim(cmobs))//char(13)//&
	  " data points. Only the first "//cmobs(1:len_trim(cmobs))//" points were read",&
	  "Data truncation on read in")

      END SUBROUTINE ProfileRead_TruncationWarning


      SUBROUTINE Background_Fit

      USE VARIABLES
	  USE WINTERACTER
	  USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
      NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
!
      COMMON /PLTYPE/ IPTYPE
!
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      include 'statlog.inc'

      integer ICurSel, IXPos, IYPos, IField, IType, IBpass, Iret1
	  logical process_mainwindow_message
	  logical Quit
      TYPE(WIN_MESSAGE) :: MESSAGE

      INCLUDE 'Lattice.inc'

	  ICurSel = WinfoDialog(CurrentDialog)
 
	  CALL ToggleMenus(0)

	  CALL WDialogSelect(ID_Fit_Data_request)
	  CALL WDialogShow(-1,-1,0,Modal)


	  Iret  = WinfoDialog(ExitButton)
	  IF (Iret .EQ. IDF_Fit_Now) THEN
				CALL WDialogSelect(ID_Fit_Data_request)
				CALL WDialogHide()
				CALL Profile_Plot(IPTYPE)
      ELSE
				CALL WDialogSelect(ID_Fit_Data_request)
				CALL WDialogHide()
				CALL Profile_Plot(IPTYPE)
				CALL ToggleMenus(1)
				IF (ICurSel .NE. -1) CALL WDialogSelect(ICurSel)
				RETURN
	  END IF

	  CALL WDialogSelect(ID_Background_Fit)
	  CALL WDialogShow(-1,-1,0,Modeless)

	  IT = InfoError(1)
      QUIT = .FALSE.
	  CALL BackFit(0,0) ! Initialise
	  DO WHILE(.NOT.QUIT)
          CALL WMessage(ITYPE,MESSAGE)
		  IF (MESSAGE%WIN .EQ. 0) THEN
			QUIT = process_mainwindow_message(ITYPE,MESSAGE)
		  ELSE IF (ITYPE .EQ. PushButton) THEN
! Process it

			SELECT CASE (Message%Value1)
				CASE (IDF_Background_Apply)
				   CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
				   CALL BackFit(1,IBpass)
				   CALL WDialogFieldState(IDF_Background_Accept,Enabled)
				   CALL Profile_Plot(IPTYPE)
				CASE (IDF_Background_Accept)
				   ! Subtract the background
				   IOBS = 0
				   YPMIN = YOBS(1) - YBBIN(1)
				   YPMAX = YPMIN

				   DO I = 1,NBIN
				     DO J = 1,LBIN
					   IOBS = IOBS + 1
				       YOBS(IOBS) = YOBS(IOBS) - YBBIN(I)
					   YPMIN=MIN(YOBS(IOBS),YPMIN)
                       YPMAX=MAX(YOBS(IOBS),YPMAX)
					 END DO
					 YOBIN(I) = YOBIN(I) - YBBIN(I)
					 YBBIN(I) = 0
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
				   QUIT = .TRUE. 
				   BACKREF = .FALSE.
				CASE (IDCANCEL)
				   CALL BackFit(2,0) ! Cancel - copy the original ybbin back
				   QUIT = .TRUE.
			END SELECT
          ELSE IF (ITYPE .EQ. CloseRequest) THEN
		    QUIT = .TRUE.
		  END IF
      END DO
	  CALL WDialogSelect(ID_Background_Fit)
	  CALL WDialogHide()
	  CALL Profile_Plot(IPTYPE)
	  CALL ToggleMenus(1)
	  IF (ICurSel .NE. -1) CALL WDialogSelect(ICurSel)

	  END SUBROUTINE Background_Fit


