!
!*****************************************************************************
!
! This file contains routines to read powder diffraction files.
!
!*****************************************************************************
!
      INTEGER FUNCTION DiffractionFileBrowse
!
! This routine lets the user browse a directory for a diffraction file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
! This way, all diffraction-file-opening is dealt with by a single routine.
!
! JvdS 18 July 2001
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data/...)
!           2 if user pressed cancel
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER(LEN=512)  FILTER
      INTEGER             IFLAGS
      INTEGER             IFTYPE    ! Needed for Winteracter routine
      CHARACTER(LEN=MaxPathLength) tFileName ! Temporary filename
      INTEGER DiffractionFileOpen ! Function

      IFLAGS = LoadDialog + DirChange + PromptOn
! It seems that Winteracter cannot cope with strings of this length
!      FILTER = 'All files (*.*)|*.*|'//&
!               'All powder diffraction files|*.raw;*.rd;*.sd;*.udf;*.uxd;*.xye|'//&
!               'Bruker powder diffraction files (*.raw, *.uxd)|*.raw;*.uxd|'//&
!               'DASH powder diffraction files (*.xye)|*.xye|'//&
!               'Philips powder diffraction files (*.rd, *.sd, *.udf)|*.rd;*.sd;*.udf|'
      FILTER = 'All files (*.*)|*.*|'//&
               'All powder diffraction files|*.raw;*.rd;*.sd;*.udf;*.uxd;*.xye|'//&
               'DASH powder diffraction files (*.xye)|*.xye|'
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      tFileName = ' '
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open Powder Diffraction File',IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) THEN
        DiffractionFileBrowse = 2
        RETURN
      ENDIF
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      DiffractionFileBrowse = DiffractionFileOpen(tFileName)

      END FUNCTION DiffractionFileBrowse
!
!*****************************************************************************
!
      INTEGER FUNCTION DiffractionFileOpen(TheFileName)
!
! This routine tries to open a diffraction file.
!
! JvdS 18 July 2001
!
! INPUT   : TheFileName = the file name
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER(LEN=MaxPathLength), INTENT (INOUT) :: TheFileName

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      LOGICAL    FExists
      INTEGER    KLEN
! Note that FNAME is a global variable
      INTEGER    ISTAT
      INTEGER    DiffractionFileLoad ! Function
      LOGICAL    Confirm ! Function
      REAL       tMaxResolution
      REAL       TwoTheta2dSpacing, dSpacing2TwoTheta ! Function
      LOGICAL    FnWavelengthOK ! Function

      DiffractionFileOpen = 0
      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        RETURN
      ENDIF
!   Check if file needs saving
      IF (SAVEF) THEN
        IF (.NOT. Confirm('Program contains an unsaved project.'//CHAR(13)//'Do you wish to '// &
          'continue?')) RETURN
      ENDIF
!   If answer 'Yes'
! It is slightly odd that SAVEF is set to .FALSE. here. We are about to load the powder patttern:
! how much more necessary to save a project file can it get?
      SAVEF = .FALSE.
! This is the point of no return: the selected file will be new file, valid data or not
! Change global variable FNAME
      FNAME = TheFileName
! Update this throughout the program (Wizard + status bar)
      CALL ScrUpdateFileName
      ISTAT = DiffractionFileLoad(TheFileName)
      DiffractionFileOpen = ISTAT
      IF (ISTAT .EQ. 0) RETURN
! Enable the appropriate menus:
      CALL SetModeMenuState(1,-1,-1)
! JvdS Was:
!      CALL SetModeMenuState(1,0,0)
! however, that sometimes enables Pawley fit after just having read a new data file
! (the zeros mean: don't change state)
! Partially, this seems to be caused by nothing in the program being
! initialised when a file is read in: all the old tick marks etc. are still there.
! I think that reading in a new powder diffraction file should initialise all other
! data to 'unknown'.
      IF (LEN_TRIM(FNAME) .GT. 80) THEN
        CALL DebugErrorMessage('FNAME too long in DiffractionFileOpen')
        DashRawFile = FNAME(1:80)
      ELSE
        DashRawFile = FNAME(1:LEN_TRIM(FNAME))
      ENDIF
! Set minimum and maximum truncation values in Wizard in accordance with data read in
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
! Initialise truncation of start of powder pattern
      CALL WDialogPutReal(IDF_Min2Theta,XPMIN,'(F6.3)')
! In principle, set resolution so as to truncate after DefaultMaxResolution.
! However, if truncation resolution not attainable with current data range / wavelength,
! adjust the setting of the maximum resolution to maximum possible.
      IF (FnWavelengthOK()) THEN
        tMaxResolution = MAX(TwoTheta2dSpacing(XPMAX),DefaultMaxResolution)
      ELSE
        tMaxResolution = DefaultMaxResolution
      ENDIF
      CALL WDialogPutReal(IDF_MaxResolution,tMaxResolution)
      CALL WDialogPutReal(IDF_Max2Theta,dSpacing2TwoTheta(tMaxResolution))
      CALL PopActiveWindowID

      END FUNCTION DiffractionFileOpen
!
!*****************************************************************************
!
      INTEGER FUNCTION DiffractionFileLoad(TheFileName)
!
! This routine tries to load a diffraction file.
!
! JvdS 18 July 2001
!
! INPUT   : TheFileName = the file name
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: TheFileName

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)
! Not too pretty, but safe
      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

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

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

      INTEGER          NTIC
      INTEGER                IH
      REAL                               ARGK
      REAL                                           DSTAR
      COMMON /PROFTIC/ NTIC, IH(3,MTIC), ARGK(MTIC), DSTAR(MTIC)

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

      INTEGER          KLEN
      CHARACTER(LEN=4) EXT4
      INTEGER          Load_dat_File ! Function
      INTEGER          Load_raw_File ! Function
      INTEGER          Load_rd_File  ! Function
      INTEGER          Load_udf_File ! Function
      INTEGER          Load_uxd_File ! Function
      INTEGER          Load_xye_File ! Function
      INTEGER          ISTAT
      INTEGER          I
      INTEGER          POS
      LOGICAL          ESDsFilled
      INTEGER          INTEGRATED_GUESS, MAX_INTENSITY_INDEX

! Initialise to failure
      DiffractionFileLoad = 0
      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
! Find the last occurence of '.' in TheFileName
      POS = KLEN-1 ! Last character of TheFileName is not tested
! The longest extension allowed is four
      DO WHILE ((POS .NE. 0) .AND. (TheFileName(POS:POS) .NE. '.') .AND. (POS .NE. (KLEN-5)))
        POS = POS - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      IF (TheFileName(POS:POS) .NE. '.') RETURN
      EXT4 = '    '
      EXT4 = TheFileName(POS+1:KLEN)
      CALL ILowerCase(EXT4)
      ISTAT = 0
      ESDsFilled = .FALSE.
      SELECT CASE (EXT4)
        CASE ('dat ')
          ISTAT = Load_dat_File(TheFileName,ESDsFilled)
        CASE ('raw ')
          ISTAT = Load_raw_File(TheFileName,ESDsFilled)
        CASE ('rd  ','sd  ')
          ISTAT =  Load_rd_File(TheFileName,ESDsFilled)
        CASE ('udf ')
          ISTAT = Load_udf_File(TheFileName,ESDsFilled)
        CASE ('uxd ')
          ISTAT = Load_uxd_File(TheFileName,ESDsFilled)
        CASE ('xye ')
          ISTAT = Load_xye_File(TheFileName,ESDsFilled)
      END SELECT
      DiffractionFileLoad = ISTAT
      IF (ISTAT .EQ. 0) THEN
        CALL ErrorMessage('Could not load the file')
! When we arrive here, the state of the program becomes a little bit undetermined.
! This wasn't too bad in the old DASH code, as none of the variables was updated properly anyway,
! but it starts to become a problem since some variables are now properly initialised and some aren't
!        NoData = .TRUE.
!        FNAME = ' '
!        CALL ScrUpdateFileName
        RETURN
      ENDIF
! Set the default SA output files to <fname>.cssr etc (fname gets any extension removed)
      CALL sa_SetOutputFiles(TheFileName)
! Fill the E.S.D.s if that hasn't been taken care of yet
      IF (.NOT. ESDsFilled) THEN
        DO I = 1, NOBS
          IF (YOBS(I) .LT. 0.000001) THEN
! Number of counts can be zero, especially at low theta due to a variable slit
            EOBS(I) = 1
! @ quick fix, not necessarily correct for simulated data (quite accurate for real data)
          ELSE
            EOBS(I) = SQRT(YOBS(I))
          ENDIF
        ENDDO
      ENDIF
      DataSetChange = DataSetChange + 1
      BackRef = .TRUE.
      CALL Init_PeakFitRanges
      XPMIN = XOBS(1)
      XPMAX = XOBS(1)
      YPMIN = YOBS(1)
      YPMAX = YOBS(1)
      DO I = 1, NOBS
        XPMIN = MIN(XOBS(I),XPMIN)
        XPMAX = MAX(XOBS(I),XPMAX)
        YPMIN = MIN(YOBS(I),YPMIN)
        IF (YPMAX .LT. YOBS(I)) THEN
          MAX_INTENSITY_INDEX = I
          YPMAX = YOBS(I)
        ENDIF
      ENDDO
      INTEGRATED_GUESS = 0
      DO I = MAX(1,MAX_INTENSITY_INDEX - 5), MIN(NOBS,MAX_INTENSITY_INDEX + 5)
        INTEGRATED_GUESS = INTEGRATED_GUESS + YOBS(I)
      ENDDO
      IF (INTEGRATED_GUESS .GT. 250000) THEN
        SCALFAC = 0.01 * INTEGRATED_GUESS/250000
      ELSE IF (YPMAX .GT. 100000) THEN
        SCALFAC = 0.01 * YPMAX/100000
      ENDIF
      OriginalNOBS = NOBS
      EndNOBS = OriginalNOBS
      BackupNOBS = NOBS
      DO I = 1, NOBS
        BackupXOBS(I) = XOBS(I)
        BackupYOBS(I) = YOBS(I)
        BackupEOBS(I) = EOBS(I)
      ENDDO
      CALL Rebin_Profile
      DO I = 1, NBIN
! JvdS Assume no knowledge on background
        YBBIN(I) = 0.0
      ENDDO
      CALL GetProfileLimits
      IPTYPE = 1
      CALL Profile_Plot(IPTYPE)
      NoData = .FALSE.
      CALL ScrUpdateFileName
!      CALL FourierPattern(1,1000)

      END FUNCTION DiffractionFileLoad
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_dat_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.dat file (ASCII format used by Armel Le Bail).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      CHARACTER*255 Cline ! String containing last line read from file
      INTEGER       I, J, NumOfBins, FLEN ! Length of TheFileName
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      REAL          TempInput(8) ! Max. num. of columns is 8
      INTEGER       NumOfColumns2Read
      INTEGER       GetNumOfColumns ! Function

! Current status, initialise to 'error'
      Load_dat_File = 0
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      FLEN = LEN_TRIM(TheFileName)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
! *.dat files look like this:
!

!    5.00   0.02   77.00
!     1097      1133      1099      1076      1056      1056      1080      1101
!     1089      1026      1050      1105      1078      1072      1076      1087
!     1078      1013      1015      1066      1043      1048       996      1003
! <SNIP>
!      758       756       826       789       736       796       748       773
!      785       786       736       762       718       757       771       738
!      778
!
!12345678911234567892123456789312345678941234567895123456789612345678971234567898
!


! Read the header line
    9 READ(10,*,ERR=999,END=999) TwoThetaStart, TwoThetaStep, TwoThetaEnd
! The number of counts per 2theta should commence from here.
! Calculate how many bins we expect.
! Quick check if values have been read at all.
      IF (TwoThetaStart .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta starting value not found.')
        GOTO 999
      ENDIF
      IF (TwoThetaEnd   .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta end value not found.')
        GOTO 999
      ENDIF
      IF (TwoThetaStep  .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta step size not found.')
        GOTO 999
      ENDIF
! Note that NINT correctly rounds to the nearest whole number
      NumOfBins = 1 + NINT((TwoThetaEnd - TwoThetaStart) / TwoThetaStep)
! Check that we will not read more than MOBS data points
      IF (NumOfBins .GT. MOBS) THEN
! Warn the user
        CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
        NumOfBins = MOBS
      ENDIF
! Check that we will not read less than 1 data point
      IF (NumOfBins .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
! Fill the 2theta values first
      CurrTwoTheta = TwoThetaStart
      DO I = 1, NumOfBins
        XOBS(I) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + TwoThetaStep
      ENDDO
      READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
      I = 0
! Next, we have to determine how many columns to read from the next line.
! There are two conditions:
! 1. the number of columns actually present in the string (GetNumOfColumns)
! 2. the number of data point still to be read (NumOfBins - I)
   11 NumOfColumns2Read = MIN(GetNumOfColumns(Cline),NumOfBins - I)
      READ(Cline,*,ERR=999) TempInput(1:NumOfColumns2Read)
! Next couple of lines rather clumsy, but safe.
      DO J = 1, NumOfColumns2Read
        YOBS(I+J) = TempInput(J)
      ENDDO
      I = I + NumOfColumns2Read
      READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
      GOTO 11
  100 CLOSE(10)
      NOBS = I
      IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                   'Will continue with points actually read only.')
      IF (NOBS .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
      ESDsFilled = .FALSE.
! This is probably synchrotron data
      JRadOption = 2
      CALL Upload_Source
      Load_dat_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      CLOSE(10)

      END FUNCTION Load_dat_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_rd_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.rd or *.sd file (binary format from Philips machines).
! With permission from Philips, but please note that this file format is not public domain.
! We do not have permission to write .rd/.sd files: only to read them.
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! JvdS 25 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      INTEGER      I, NumOfBins, FLEN ! Length of TheFileName
      REAL         Lambda1
      REAL*8       TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      CHARACTER*2  Anode
      INTEGER*4    I4
      INTEGER*4    I4_2
      INTEGER*4    I4_3
      REAL*4       R4
      INTEGER*2    I2(1:4)
      INTEGER*1    I1(1:12)
      REAL*8       R8
      EQUIVALENCE  (I1(1),I2(1),I4,R4,R8)
      EQUIVALENCE  (I1(5),I4_2)
      EQUIVALENCE  (I1(9),I4_3)
      CHARACTER*4  C4
      CHARACTER*2  Version
      INTEGER      Offset
      LOGICAL      CONTINUOUS
      REAL         WavelengthOf ! Function

! Current status, initialise to 'error'
      Load_rd_File  = 0
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Lambda1       = 0.0
      Anode         = 'Xx'
      FLEN = LEN_TRIM(TheFileName)
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
      READ(UNIT=10,REC=1,ERR=999) C4
      Version = C4(1:2)
      IF ((Version .NE. 'V5') .AND. (Version .NE. 'D3') .AND. (Version .NE. 'V3')) THEN
        CALL ErrorMessage("Not a valid Philips .rd/.sd file.")
        GOTO 999
      ENDIF
      SELECT CASE (C4(3:4))
        CASE ('DI')
          CALL ErrorMessage("This file contains peak positions, not a powder pattern.")
          GOTO 999
        CASE ('BK')
          CALL ErrorMessage("This file contains a background, not a powder pattern.")
          GOTO 999
        CASE ('2D')
          CALL ErrorMessage("This file contains a second derivative, not a powder pattern.")
          GOTO 999
      END SELECT
! Anode material
      READ(UNIT=10,REC=22,ERR=999) I4
! Due to EQUIVALENCE, relevant byte is now in I1(2)
! We might directly set the wavelength here, but that is supposed to follow, this is just extra
      SELECT CASE (I1(2))
        CASE (0)
          Anode = 'Cu'
        CASE (1)
          Anode = 'Mo'
        CASE (2)
          Anode = 'Co'
        CASE (3)
          Anode = 'Fe'
        CASE (4)
          Anode = 'Cr'
      END SELECT
! The wavelength is stored as a REAL*8, but crosses a 2 byte boundary
! This means that we have to read 3 consecutive records of four bytes and shift them
      READ(UNIT=10,REC=24,ERR=999) I4
      READ(UNIT=10,REC=25,ERR=999) I4_2
      READ(UNIT=10,REC=26,ERR=999) I4_3
      DO I = 1, 10
        I1(I) = I1(I+2)
      ENDDO
      Lambda1 = R8
      READ(UNIT=10,REC=32,ERR=999) I4
! Due to EQUIVALENCE, I1(3) now contains CONTINUOUS (=TRUE=1) or STEP (=FALSE)
      CONTINUOUS = I1(3) .GT. 0
! Position 128 contains the step size as a 'packed angle'.
      READ(UNIT=10,REC=33,ERR=999) I4
! Due to EQUIVALENCE, I2(1) now contains Step Size as a 'packed angle'.
      TwoThetaStep = I2(1) / 200.0
! Due to EQUIVALENCE, I2(2) now contains Start Angle as a 'packed angle'.
      TwoThetaStart = I2(2) / 200.0
      READ(UNIT=10,REC=34,ERR=999) I4
! Due to EQUIVALENCE, I2(1) now contains Final Angle as a 'packed angle'.
      TwoThetaEnd = I2(1) / 200.0
      IF (Version .NE. 'D3') THEN
! Step Size, Start Angle and Final Angle are given as REAL*8 on 2 byte boundaries again
        READ(UNIT=10,REC=54,ERR=999) I4
        READ(UNIT=10,REC=55,ERR=999) I4_2
        READ(UNIT=10,REC=56,ERR=999) I4_3
        DO I = 1, 10
          I1(I) = I1(I+2)
        ENDDO
        TwoThetaStep = R8
        READ(UNIT=10,REC=56,ERR=999) I4
        READ(UNIT=10,REC=57,ERR=999) I4_2
        READ(UNIT=10,REC=58,ERR=999) I4_3
        DO I = 1, 10
          I1(I) = I1(I+2)
        ENDDO
        TwoThetaStart = R8
        READ(UNIT=10,REC=58,ERR=999) I4
        READ(UNIT=10,REC=59,ERR=999) I4_2
        READ(UNIT=10,REC=60,ERR=999) I4_3
        DO I = 1, 10
          I1(I) = I1(I+2)
        ENDDO
        TwoThetaEnd = R8
      ENDIF
! If it is a 'V5' file, another header follows, which also contains the anode material
! Documentation says they should always match, but advises to use the second instance
! It's the first byte of record 93
      IF (Version .EQ. 'V5') THEN
        READ(UNIT=10,REC=93,ERR=999) I4
        SELECT CASE (I1(1))
          CASE (0)
            Anode = 'Cu'
          CASE (1)
            Anode = 'Mo'
          CASE (2)
            Anode = 'Co'
          CASE (3)
            Anode = 'Fe'
          CASE (4)
            Anode = 'Cr'
        END SELECT
      ENDIF
! The number of counts per 2theta should commence from here.
! Calculate how many bins we expect.
! Quick check if values have been read at all.
      IF (TwoThetaStart .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta starting value not found.')
        GOTO 999
      ENDIF
      IF (TwoThetaEnd   .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta end value not found.')
        GOTO 999
      ENDIF
      IF (TwoThetaStep  .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta step size not found.')
        GOTO 999
      ENDIF
! If the scan was continuous (rather than step), the average 2 theta of a data point is actually
! step/2 higher than indicated in the file. The number of data points is decreased by one.
! This is the way this is stored when a .rd is converted to a .udf
      IF (CONTINUOUS) THEN
        TwoThetaStart = TwoThetaStart + (TwoThetaStep / 2.0)
        TwoThetaEnd   = TwoThetaEnd   - (TwoThetaStep / 2.0)
      ENDIF
! Note that NINT correctly rounds to the nearest whole number
      NumOfBins = 1 + NINT((TwoThetaEnd - TwoThetaStart) / TwoThetaStep)
! Check that we will not read more than MOBS data points
      IF (NumOfBins .GT. MOBS) THEN
! Warn the user
        CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
        NumOfBins = MOBS
      ENDIF
! Check that we will not read less than 1 data point
      IF (NumOfBins .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file contains no valid data.")
        GOTO 999
      ENDIF
      IF (Version .EQ. 'V5') THEN
        Offset = 202
      ELSE
        Offset = 63
      ENDIF
! Fill the 2theta values first
      CurrTwoTheta = TwoThetaStart
      DO I = 1, NumOfBins
        XOBS(I) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + TwoThetaStep
      ENDDO
! Raw data counts are stored per two bytes across a two byte boundary.
! It is possible that the last data point cannot be read.
! This is the case if the number of data points is even
      IF (MOD(NumOfBins,2) .EQ. 0) NumOfBins = NumOfBins - 1
! Read first data point
      READ(UNIT=10,REC=Offset,ERR=999) I4
! Due to EQUIVALENCE, the first data point is now in I2(2)
      YOBS(1) = (I2(2) * 0.1)**2
! NumOfBins is now odd, and ((NumOfBins/2)*2)+1 = NumOfBins
      DO I = 1, NumOfBins/2
        READ(UNIT=10,REC=Offset+I,ERR=999) I4
        YOBS(I*2  ) = (I2(1) * 0.1)**2
        YOBS(I*2+1) = (I2(2) * 0.1)**2
      ENDDO
      CLOSE(10)
      NOBS = NumOfBins
      ESDsFilled = .FALSE.
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the Anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
      CALL Set_Wavelength(Lambda1)
      Load_rd_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      CLOSE(10)

      END FUNCTION Load_rd_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_udf_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.udf file (ASCII format from Philips machines).
! With permission from Philips, but please note that this file format is not public domain.
! We do not have permission to write .udf files: only to read them.
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! JvdS 25 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      CHARACTER*255 Cline ! String containing last line read from file
      CHARACTER*255 tSubString
      INTEGER       I, NumOfBins, FLEN ! Length of TheFileName
      REAL          Lambda1
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      CHARACTER*2   Anode
      REAL          WavelengthOf ! Function

! Current status, initialise to 'error'
      Load_udf_File = 0
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Anode         = 'Xx'
      Lambda1       = 0.0
      FLEN = LEN_TRIM(TheFileName)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
! *.udf files look like this:
!
!SampleIdent,AgCOOC5H11 ,/
!Title1
!Title2
!DiffrType,PW1710,/
!DiffrNumber,1,/
!Anode,Cu,/
!LabdaAlpha1, 1.54060,/
!LabdaAlpha2, 1.54439,/
!RatioAlpha21, 0.50000,/
!DivergenceSlit,Automatic, 12,/
!MonochromatorUsed,YES ,/
!GeneratorVoltage,  40,/
!TubeCurrent,  55,/
!FileDateTime,  1-nov-:0,  21:06,/
!DataAngleRange,   3.0100,  89.9900,/
!ScanStepSize,   0.020,/
!ScanType,CONTINUOUS,/
!ScanStepTime,    5.00,/
!RawScan
!      640,      625,      729,      635,      610,      666,      610,      666
!      640,      610,      620,      625,      640,      655,      686,      610
!      635,      666,      660,      655,      702,      681,      671,      686
! <SNIP>
!      818,      894,      858,      835,      930,      870,      876,      888
!      912,      949,      912,      894,      894,      864,      924,      858
!      858,      790,      847,      900,      876,      847,/
!
!12345678911234567892123456789312345678941234567895123456789612345678971234567898
!
! Unfortunately, the widths of the fields are not fixed (although I expect that
! they were supposed to be) and it is necessary to scan for fields using the ',' as a separator.
!
! Also note that the '/' is not always present after the last data point (although I suspect
! that it was supposed to be).
!
! There can be a comma at the end of every row of counts.
!

! Read the header lines
    9 READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
      IF (Cline(1:5) .EQ. 'Anode') THEN ! Anode,Cu,/
        READ(Cline,FMT='(6X,A2)',ERR=999) Anode
        GOTO 9
      ENDIF
      IF (Cline(1:11) .EQ. 'LabdaAlpha1') THEN ! LabdaAlpha1, 1.54060,/
! Remove 'LabdaAlpha1,'
        CALL GetSubString(Cline,',',tSubString)
! Read next field, should be ' 1.54060'
        CALL GetSubString(Cline,',',tSubString)
        READ(tSubString,*,ERR=999) Lambda1
        GOTO 9
      ENDIF
      IF (Cline(1:14) .EQ. 'DataAngleRange') THEN ! DataAngleRange,   3.0100,  89.9900,/
! Remove 'DataAngleRange,'
        CALL GetSubString(Cline,',',tSubString)
! Read next field, should be '   3.0100'
        CALL GetSubString(Cline,',',tSubString)
        READ(tSubString,*,ERR=999) TwoThetaStart
! Read next field, should be '  89.9900'
        CALL GetSubString(Cline,',',tSubString)
        READ(tSubString,*,ERR=999) TwoThetaEnd
        GOTO 9
      ENDIF
      IF (Cline(1:12) .EQ. 'ScanStepSize') THEN ! ScanStepSize,   0.020,/
! Remove 'ScanStepSize,'
        CALL GetSubString(Cline,',',tSubString)
! Read next field, should be '   0.020'
        CALL GetSubString(Cline,',',tSubString)
        READ(tSubString,*,ERR=999) TwoThetaStep
        GOTO 9
      ENDIF
      IF (Cline(1:7) .EQ. 'RawScan') GOTO 10
      GOTO 9
! Here is where we can start to read the data
   10 CONTINUE
! The number of counts per 2theta should commence from here.
! Calculate how many bins we expect.
! Quick check if values have been read at all.
      IF (TwoThetaStart .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta starting value not found.')
        GOTO 999
      ENDIF
      IF (TwoThetaEnd   .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta end value not found.')
        GOTO 999
      ENDIF
      IF (TwoThetaStep  .LT. 0.000001) THEN
        CALL ErrorMessage('2 theta step size not found.')
        GOTO 999
      ENDIF
! Note that NINT correctly rounds to the nearest whole number
      NumOfBins = 1 + NINT((TwoThetaEnd - TwoThetaStart) / TwoThetaStep)
! Check that we will not read more than MOBS data points
      IF (NumOfBins .GT. MOBS) THEN
! Warn the user
        CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
        NumOfBins = MOBS
      ENDIF
! Check that we will not read less than 1 data point
      IF (NumOfBins .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
! Fill the 2theta values first
      CurrTwoTheta = TwoThetaStart
      DO I = 1, NumOfBins
        XOBS(I) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + TwoThetaStep
      ENDDO
      I = 1
   20 READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
      CALL GetSubString(Cline,',',tSubString)
      DO WHILE ((LEN_TRIM(tSubString) .NE. 0) .AND. (tSubString(1:LEN_TRIM(tSubString)) .NE. '/'))
        IF (I .GT. MOBS) GOTO 100
        READ(tSubString,*,ERR=999) YOBS(I)
        I = I + 1
        CALL GetSubString(Cline,',',tSubString)
      ENDDO
! Fetch the next line from the file
      GOTO 20
  100 CLOSE(10)
      NOBS = I - 1
      IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                   'Will continue with points actually read only.')
      IF (NOBS .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
      ESDsFilled = .FALSE.
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the Anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
      CALL Set_Wavelength(Lambda1)
      Load_udf_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      CLOSE(10)

      END FUNCTION Load_udf_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_uxd_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.uxd file (ASCII format from Bruker conversion programs).
! The routine basically assumes that the file is OK.
!
! A .uxd file is very free format: see documentation for description
! Some variables can have defaults set in the Windows registry: I think it's beyond the scope
! of DASH to find those.
!
! Note that this function should only be called from DiffractionFileLoad
!
! JvdS 25 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      CHARACTER*511 Cline ! String containing last line read from file
      INTEGER       J, MaxNumOfBins, FLEN ! Length of TheFileName
      REAL          Lambda1
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      CHARACTER*2   Anode
      INTEGER       POS
      CHARACTER*511 KeyWord
      REAL          TempInput(17) ! Max. num. of columns is 16 excluding 2theta
      INTEGER       Mode
      INTEGER       COUNTS, THETACOUNTS, THETACOUNTSTIME
      PARAMETER    (COUNTS = 1, THETACOUNTS = 2, THETACOUNTSTIME = 3)
      INTEGER       MaxNumOfColumns, NumOfColumns2Read
      INTEGER       GetNumOfColumns ! Function
      CHARACTER*1   ChrLowerCase, ChrUpperCase ! Functions
      REAL          WavelengthOf ! Function
      INTEGER       KeyWordPos, KeyWordLen, StrLen, I

! Current status, initialise to 'error'
      Load_uxd_File = 0
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Anode         = 'Xx'
      Lambda1       = 0.0
      Mode          = THETACOUNTS ! Default
      MaxNumOfBins  = MOBS
      MaxNumOfColumns = 16
      FLEN = LEN_TRIM(TheFileName)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
      I = 1
! A .uxd file has an infinite number of variations, one of them is:
!
!; J:\DIFFDAT1\Xtals-R-Us\2-hydroxyphenoxyacetic\2hpoxya-acid-psd-5-65.raw(Diffrac Plus V1.01 file) converted by XCH V1.0
!_FILEVERSION=2
!_SAMPLE='2-Hydroxyphenoxyacetic acid, light grind, 0.7mm boro, 1mm,'
!_+SAMPLE='This configuration file has been created by the Bruker AXS Config       Program imitating the old SAG-free format style for compatibility to'
!_SITE='University of Strathclyde GBR'
!_USER='Administrator'
!_MONOCHROMATOR=1
!; Transmission
!_SOLLER_SLITS_2='Y'
!_BETA_FILTER='N'
!_FIXED_ANTISLIT=0.000000
!_ANALYZER_CODE=0
!; None
!_DATEMEASURED='28-Apr-2001 02:35:57'
!_RUNTIME=41390.000000
!_WL1=1.540600
!_WL2=1.544390
!_WL3=1.392220
!_WLRATIO=0.500000
!_ANODE='Cu'
!; (Data for Range number 1)
!_DRIVE='PSDSCAN'
!_STEPTIME=10.000000
!_STEPSIZE=0.014489
!_STEPMODE='C'
!_START=5.000000
!_THETA=2.500000
!_2THETA=5.000000
!_PHI=0.000000
!_RANGE_WL=1.540600
!_COUNTS
!       7662        7506        7597        7640        7603        7543        7582        7433
!       7553        7517        7463        7317        7302        7336        7379        7303
!       7287        7335        7299        7310        7200        7143        7230        7188
! <SNIP>
!       1282        1221        1324        1197        1310        1346        1309        1352
!       1224        1366        1344        1255        1271        1376        1404        1346
!       1353        1385        1400
!
 10   READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
! Test if line empty
      StrLen = LEN_TRIM(Cline)
      IF (StrLen .EQ. 0) GOTO 10
! Determine first non-blank character
      POS = 1
      DO WHILE (Cline(POS:POS) .EQ. ' ')
        POS = POS + 1
      ENDDO
      SELECT CASE (Cline(POS:POS))
        CASE ('_')
! It's a keyword. Read up to the next '=' or EOL, ignoring spaces and converting to uppercase
          KeyWord = '_'
          POS = POS + 1
          KeyWordPos = 2
          DO WHILE ((POS .LE. StrLen) .AND. (Cline(POS:POS) .NE. '='))
            IF (Cline(POS:POS) .NE. ' ') THEN
              KeyWord(KeyWordPos:KeyWordPos) = ChrUpperCase(Cline(POS:POS))
              KeyWordPos = KeyWordPos + 1
            ENDIF
            POS = POS + 1
          ENDDO
          KeyWordLen = KeyWordPos - 1
          SELECT CASE (KeyWord(1:KeyWordLen))
            CASE ('_ACTUAL_I100','_D-I','_2THETA-I') ! It's a peak list, not a powder pattern
              CALL ErrorMessage('The file contains a peak list, not a powder diffraction pattern.')
              RETURN
            CASE ('_ANODE') ! Anode material
! There should follow a '=' followed by the anode material e.g. "'Cu'" (including the single quotes).
! First check that there is a '='. It should at be the current position in Cline
              IF (Cline(POS:POS) .EQ. '=') THEN
! Search for next non-space, starting after the '='
                POS = POS + 1
                DO WHILE ((POS .LE. StrLen) .AND. (Cline(POS:POS) .EQ. ' '))
                  POS = POS + 1
                ENDDO
! Now we are at the first non-space character after '_ANODE ='
! Check that we have at least four characters left in Cline
                IF ((POS+3) .LE. StrLen ) THEN
                  Anode(1:1) = ChrUpperCase(Cline(POS+1:POS+1))
                  Anode(2:2) = ChrLowerCase(Cline(POS+2:POS+2))
                ENDIF
              ENDIF             
            CASE ('_WL1','_RANGE_WL') ! Primary wavelength / wavelength for this data set
! There should follow a '=' followed by the wavelength in Angstrom
! First check that there is a '=', it should be at the current position in Cline.
! Also check if there are any positions left in Cline after that
              IF (((StrLen - POS) .GT. 2) .AND. (Cline(POS:POS) .EQ. '=')) THEN
! An error doesn't matter here. If there is an error, just read the next input line
                READ(Cline(POS+1:StrLen),*,ERR=10) Lambda1
              ENDIF
            CASE ('_STEPSIZE')
! The presence of this keyword defaults to COUNTS. If this is wrong, the actual data will be 
! preceded by the proper keyword and Mode will be reset accordingly
              Mode = COUNTS
              IF (((StrLen - POS) .GT. 2) .AND. (Cline(POS:POS) .EQ. '=')) THEN
! An error doesn't matter here. If there is an error, just read the next input line
                READ(Cline(POS+1:StrLen),*,ERR=10) TwoThetaStep
              ENDIF
            CASE ('_STEPMODE') ! CONTINUOUS or STEP
            CASE ('_STEPCOUNT') ! MaxNumOfBins
              IF (((StrLen - POS) .GT. 0) .AND. (Cline(POS:POS) .EQ. '=')) THEN
! An error doesn't matter here. If there is an error, just read the next input line
                READ(Cline(POS+1:StrLen),*,ERR=10) MaxNumOfBins
              ENDIF
              IF (MaxNumOfBins .GT. MOBS) THEN
! Warn the user
                CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
                MaxNumOfBins = MOBS
              ENDIF
! Check that we will not read less than 1 data point
              IF (MaxNumOfBins .EQ. 0) THEN
! The user should be warned here
                CALL ErrorMessage("The file contains no valid data.")
                RETURN
              ENDIF
            CASE ('_START')
! The presence of this keyword defaults to COUNTS. If this is wrong, the actual data will be 
! preceded by the proper keyword and Mode will be reset accordingly
              Mode = COUNTS
              IF (((StrLen - POS) .GT. 2) .AND. (Cline(POS:POS) .EQ. '=')) THEN
! An error doesn't matter here. If there is an error, just read the next input line
                READ(Cline(POS+1:StrLen),*,ERR=10) TwoThetaStart
              ENDIF
            CASE ('_COUNTS','_CPS')
              Mode = COUNTS
              IF (((StrLen - POS) .GT. 0) .AND. (Cline(POS:POS) .EQ. '=')) THEN
! An error doesn't matter here. If there is an error, just read the next input line
                READ(Cline(POS+1:StrLen),*,ERR=10) MaxNumOfColumns
              ENDIF
            CASE ('_2THETACOUNTS','_2THETACPS')
              Mode = THETACOUNTS
              IF (((StrLen - POS) .GT. 0) .AND. (Cline(POS:POS) .EQ. '=')) THEN
! An error doesn't matter here. If there is an error, just read the next input line
                READ(Cline(POS+1:StrLen),*,ERR=10) MaxNumOfColumns
              ENDIF
            CASE ('_2THETACOUNTSTIME')
              Mode = THETACOUNTSTIME
! @ I don't exactly know how to interpret this
              RETURN 
          END SELECT
        CASE ('+','-','.','0','1','2','3','4','5','6','7','8','9')
! The data starts here
           SELECT CASE (Mode)
             CASE (COUNTS)
! If _START was given, _STEPSIZE must have been present as well
               IF ((TwoThetaStart .GT. 0.000001) .AND. TwoThetaStep  .LT. 0.000001) RETURN
! And vice versa
               IF ((TwoThetaStep  .GT. 0.000001) .AND. TwoThetaStart .LT. 0.000001) RETURN
               CurrTwoTheta = TwoThetaStart
! It is possible that the number of columns has been given. This should be respected, as e.g.
! the second column might contain e.s.d.s (but that cannot be signalled to the program in this
! format). However, we cannot use the number of columns straightaway, because if there are 8 columns
! and the number of data points is not a multiple of eight, DASH will crash while reading the last line.
               I = 1
! Replace all ',' by ' '.
   11          DO POS = 1, StrLen
                 IF (Cline(POS:POS) .EQ. ',') Cline(POS:POS) = ' '
               END DO
! Next, we have to determine how many columns to read from the next line.
! There are three conditions:
! 1. the maximum number of columns as requested by the user (MaxNumColumns)
! 2. the number of columns actually present in the string (GetNumOfColumns)
! 3. the number of data points still to be read (MaxNumOfBins - I)
               NumOfColumns2Read = MIN(MaxNumOfColumns, GetNumOfColumns(Cline))
               NumOfColumns2Read = MIN(NumOfColumns2Read, 1 + MaxNumOfBins - I)
               READ(Cline,*,ERR=999) TempInput(1:NumOfColumns2Read)
! Next couple of lines rather clumsy, but safe.
               DO J = 1, NumOfColumns2Read
                 YOBS(I-1+J) = TempInput(J)
                 XOBS(I-1+J) = CurrTwoTheta
                 CurrTwoTheta = CurrTwoTheta + TwoThetaStep
               ENDDO
               I = I + NumOfColumns2Read
               READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
               GOTO 11
             CASE (THETACOUNTS)
! It's slightly odd, but it is actually possible to have a number of columns specified.
! I don't how to interpret the information 'there is a column for theta and a column for counts
! and the number of columns is five', so I'll assume the number of columns is '1' (the
! 2 theta column is not counted anyway).
               I = 1
  12           READ(Cline,*, ERR=999,END=100) XOBS(I),YOBS(I)
! First, replace all ',' by ' '
               DO POS = 1, StrLen
                 IF (Cline(POS:POS) .EQ. ',') Cline(POS:POS) = ' '
               END DO
               I = I + 1
               IF (I .GT. MOBS) THEN
                 CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
                 GOTO 100
               END IF
               READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
               GOTO 12
           END SELECT
      END SELECT
      GOTO 10
 100  NOBS = I - 1
      CLOSE(10)
      ESDsFilled = .FALSE.
! Check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        RETURN
      ENDIF
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the anode material
      IF (Lambda1 .LT. 0.00001) THEN
        IF (Anode .EQ. 'Xx') THEN
          CALL WarningMessage('This file does not specify the wavelength,'//CHAR(13)// &
                              'please update the file to contain the _WL1, _RANGE_WL or _ANODE keyword.')
        ELSE
          CALL Set_Wavelength(WavelengthOf(Anode))
        ENDIF
      ELSE
        CALL Set_Wavelength(Lambda1)
      ENDIF
      Load_uxd_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return

      END FUNCTION Load_uxd_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_xye_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.xye file (standard DASH ASCII powder pattern format).
!
! Note that this function should only be called from DiffractionFileLoad
!
! JCC
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

      CHARACTER*255 Cline
      INTEGER       I, IS, FLEN ! Length of TheFileName
      LOGICAL       ReadWarning
      REAL          WavelengthOf ! Function
      REAL          FnWavelengthOfMenuOption ! Function
      INTEGER       GetNumOfColumns ! Function
      REAL          Lambda1
      LOGICAL       OK
      INTEGER       IRadSelection

! Initialise to failure
      Load_xye_File = 0
      Lambda1       = 0.0
      ReadWarning   = .FALSE.
      FLEN = LEN_TRIM(TheFileName)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
      I = 1
! Check if wavelength available on very first line
      READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
      IF (GetNumOfColumns(Cline) .EQ. 1) THEN
        READ(Cline,*,ERR=999,END=999) Lambda1
        IF ((Lambda1 .LT. 0.0001) .OR. (Lambda1 .GT. 20.0)) THEN
          CALL ErrorMessage('First line contains only one column, but not a valid wavelength.')
          GOTO 999
        ENDIF
! JvdS Q & D hack enabling the cell parameters to be stored on the second line of the .xye file.
! ####################
        READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
        IF (GetNumOfColumns(Cline) .EQ. 6) THEN
          READ(Cline,*,ERR=999,END=999) CellPar(1), CellPar(2), CellPar(3), CellPar(4), CellPar(5), CellPar(6)
          CALL Upload_Cell_Constants
        ELSE
          CLOSE(10)
          OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
          READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
        ENDIF
! JvdS End of Q & D hack.
! ####################
      ELSE
! If we are here, the .xye file didn't contain the wavelength: ask the user if he wants to include it now
        CALL PushActiveWindowID
        CALL WDialogLoad(IDD_XYE)
        CALL WDialogSelect(IDD_XYE)
        CALL WDialogShow(-1,-1,IDOK,SemiModeless)
        OK = .FALSE.
        DO WHILE (.TRUE.)
          CALL WMessage(EventType,EventInfo)
          SELECT CASE (EventType)
            CASE (PushButton)
              SELECT CASE (EventInfo%VALUE1)
                CASE (IDOK)
                  OK = .TRUE.
                  CALL WDialogGetReal(IDF_wavelength1,Lambda1)
                  GOTO 11
                CASE (IDCANCEL)
                  OK = .FALSE.
                  GOTO 11
              END SELECT
            CASE (FieldChanged)
              SELECT CASE (EventInfo%VALUE1)
                CASE (IDF_Wavelength_Menu) ! Wavelength menu selection
                  CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
                  Lambda1 = FnWavelengthOfMenuOption(IRadSelection)
                  CALL WDialogPutReal(IDF_wavelength1,Lambda1)
                CASE (IDF_wavelength1)
                  ! Do nothing 
              END SELECT
          END SELECT
        ENDDO
 11     CALL WDialogSelect(IDD_XYE)
        CALL WDialogUnload
        CALL PopActiveWindowID
        CLOSE(10)
        OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
        OPEN(UNIT=64,FILE='TempFile.xye',ERR=12)
        GOTO 13
  12    CONTINUE
        CALL ErrorMessage('Error: no changes made.')
        CLOSE(64)
        OK = .FALSE.
  13    CONTINUE
        IF (OK) THEN
! This simply assumes that it was a valid .xye file
          WRITE(64,'(F9.5)') Lambda1
! Read a line from the original file
  14      READ(UNIT=10,FMT='(A)',ERR=12,END=15) Cline
! Write the trimmed version to another file          
          WRITE(64,'(A)') Cline(1:LEN_TRIM(Cline))
          GOTO 14
  15      CONTINUE ! EoF
          CLOSE(64)
          CLOSE(10)
! Delete old .xye file
          CALL IOsDeleteFile(TheFileName)
! Move temp file to old .xye
          CALL IOsRenameFile('TempFile.xye',TheFileName)
          OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
          I = 1
! Check if wavelength available on very first line
          READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
          IF (GetNumOfColumns(Cline) .NE. 1) THEN
            CLOSE(10)
            OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
            GOTO 12
          ENDIF
        ENDIF
      ENDIF
! Modified to handle files without esds - used to read in YOBS as the esd
 10   READ(UNIT=10,FMT='(A)',ERR=999,END=100) Cline
      READ(Cline,*, IOSTAT = IS) XOBS(I),YOBS(I),EOBS(I)
      IF (IS .NE. 0) THEN
        READ(Cline,*, ERR=100,END=100) XOBS(I),YOBS(I)
        IF (ABS(YOBS(I)) .LT. 0.0000001) THEN
          EOBS(I) = 1
        ELSE IF (YOBS(I) .GE. 0.0000001) THEN
          EOBS(I) = SQRT(YOBS(I))
        ENDIF
      ENDIF
! Skip negative 2-theta data and negative intensities
      IF (XOBS(I) .LE. 0.0) GOTO 10
      IF (YOBS(I) .LT. 0.0) GOTO 10
! Skip points with zero esd
      IF (EOBS(I) .LE. 0.0) GOTO 10
      IF (I .GT. 1) THEN
        IF (ABS(XOBS(I) - XOBS(I-1)) .LT. 0.0000001) THEN
          IF (.NOT. ReadWarning) THEN
            ReadWarning = .TRUE.
            CALL WarningMessage("The data file contains multiple observations for the same 2-theta."//CHAR(13)// &
                                "Only the first observation will be used.")
          ENDIF
          GOTO 10
        ENDIF
      ENDIF
      I = I + 1
! JCC Only read in a maximum of MOBS points
      IF (I .GT. MOBS) THEN
        CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
        GOTO 100
      ENDIF
      GOTO 10
 100  NOBS = I - 1
      CLOSE(10)
      ESDsFilled = .TRUE.
! JvdS Added check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        RETURN
      ENDIF
! If wavelength not present in file, initialise to Cu
      IF (Lambda1 .LT. 0.00001) THEN
        CALL Set_Wavelength(WavelengthOf('Cu'))
        JRadOption = 1
      ELSE
! Initialise source material to synchrotron
        JRadOption = 2
! Now add in a test: if wavelength close to known anode material,
! set source to laboratory. Otherwise, source is synchrotron.
        DO I = 2, 6
          IF (ABS(Lambda1 - FnWavelengthOfMenuOption(I)) .LT. 0.0003) JRadOption = 1
        ENDDO
      ENDIF
      CALL Upload_Source
      CALL Set_Wavelength(Lambda1)
      Load_xye_File = 1
      RETURN
 999  CONTINUE
      CLOSE(10)

      END FUNCTION Load_xye_File
!
!*****************************************************************************
!
! JCC  Routine to truncate data to a particular data range
! In practice, this varies the NOBS value so the data is not lost, just
! Hidden. The routine should not be called with RMaxTTheta greater
! than the maximum value read in
      SUBROUTINE TruncateData(RMaxTTheta)

      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

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

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

      REAL RMaxTTheta
      INTEGER I

      DO I = 1, EndNOBS
        IF (XOBS(I) .GT. RMaxTTheta) EXIT
      ENDDO
      IF (I .LE. 1) RETURN
      NOBS = I - 1
      CALL Rebin_Profile
      CALL GetProfileLimits
      CALL Profile_Plot(IPTYPE)

      END SUBROUTINE TruncateData
!
!*****************************************************************************
!
      SUBROUTINE TruncateDataStart(TheMin2Theta)

      USE VARIABLES

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheMin2Theta

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

      REAL tXOBS(MOBS),tYOBS(MOBS),tYCAL(MOBS),tYBAK(MOBS),tEOBS(MOBS)

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

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD
 
      INTEGER I, Shift

! We might have been through this routine before: undo that (in case the user wants to 
! truncate at a 2 theta value lower than currently visible) and store the result (i.e. the
! original pattern) in tXOBS.

! If EndNOBS <> OriginalNOBS, truncated data is stored after the original pattern
      IF (EndNOBS .NE. OriginalNOBS) THEN
        DO I = 1, OriginalNOBS-EndNOBS
          tXOBS(I) = XOBS(I+EndNOBS)
          tYOBS(I) = YOBS(I+EndNOBS)
          tYCAL(I) = YCAL(I+EndNOBS)
          tYBAK(I) = YBAK(I+EndNOBS)
          tEOBS(I) = EOBS(I+EndNOBS)
        ENDDO
      ENDIF
! The data that was truncated at the beginning is now part of NOBS again
      NOBS = NOBS + OriginalNOBS - EndNOBS
! Copy the rest of the pattern to tXOBS etc., including any parts truncated at the end.
      DO I = 1, EndNOBS
        tXOBS(I+OriginalNOBS-EndNOBS) = XOBS(I)
        tYOBS(I+OriginalNOBS-EndNOBS) = YOBS(I)
        tYCAL(I+OriginalNOBS-EndNOBS) = YCAL(I)
        tYBAK(I+OriginalNOBS-EndNOBS) = YBAK(I)
        tEOBS(I+OriginalNOBS-EndNOBS) = EOBS(I)
      ENDDO
      DO I = 1, NOBS
        IF (tXOBS(I) .GT. TheMin2Theta) EXIT
      ENDDO
      Shift = I-1
      EndNOBS = OriginalNOBS - Shift
      DO I = 1, EndNOBS
        XOBS(I) = tXOBS(I+Shift)
        YOBS(I) = tYOBS(I+Shift)
        YCAL(I) = tYCAL(I+Shift)
        YBAK(I) = tYBAK(I+Shift)
        EOBS(I) = tEOBS(I+Shift)
      ENDDO
      IF (Shift .NE. 0) THEN
        DO I = 1, Shift
          XOBS(EndNOBS+I) = tXOBS(I)
          YOBS(EndNOBS+I) = tYOBS(I)
          YCAL(EndNOBS+I) = tYCAL(I)
          YBAK(EndNOBS+I) = tYBAK(I)
          EOBS(EndNOBS+I) = tEOBS(I)
        ENDDO
      ENDIF
      NOBS = NOBS - Shift
      CALL Rebin_Profile
      CALL GetProfileLimits
      CALL Profile_Plot(IPTYPE)

      END SUBROUTINE TruncateDataStart
!
!*****************************************************************************
!
      SUBROUTINE GetProfileLimits

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

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

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

      INTEGER I
 
      XPMIN = XBIN(1)
      XPMAX = XBIN(NBIN)
      YPMIN = XBIN(1)
      YPMAX = XBIN(1)
      DO I = 1, NBIN
        YPMIN = MIN(YOBIN(I),YPMIN)
        YPMAX = MAX(YOBIN(I),YPMAX)
      ENDDO
      XPGMIN = XPMIN
      XPGMAX = XPMAX
      YPGMIN = YPMIN
      YPGMAX = YPMAX
      CALL UPLOAD_RANGE()
      XPGMINOLD = XPMIN
      XPGMAXOLD = XPMAX
      YPGMINOLD = YPMIN
      YPGMAXOLD = YPMAX
      IPMIN = 1
      IPMAX = NBIN
      IPMINOLD = IPMIN
      IPMAXOLD = IPMAX

      END SUBROUTINE GetProfileLimits
!
!*****************************************************************************
!
      SUBROUTINE ProfileRead_TruncationWarning(filename, Npoints)

      USE VARIABLES
      USE WINTERACTER
      USE DRUID_HEADER

      CHARACTER*(*) filename
      CHARACTER*7   cmobs
      INTEGER       len_filename

      len_filename = LEN_TRIM(filename)
      WRITE(cmobs,'(I7)') Npoints
      CALL WarningMessage(" The file "//filename(1:len_filename)//" contains greater than "//cmobs(1:LEN_TRIM(cmobs))//CHAR(13)//&
        " data points. Only the first "//cmobs(1:LEN_TRIM(cmobs))//" points were read")

      END SUBROUTINE ProfileRead_TruncationWarning
!
!*****************************************************************************
!
      SUBROUTINE Rebin_Profile()
!
! Rebins the profile
!
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INCLUDE 'statlog.inc'

      INTEGER I, J, JJ, IST
      REAL XADD, YOADD, YCADD, YBADD, VADD

      NBIN = (NOBS/LBIN)
      DO I = 1, NBIN
        IST = (I-1) * LBIN
        XADD  = 0.0
        YOADD = 0.0
        YCADD = 0.0
        YBADD = 0.0
        VADD  = 0.0
        DO J = 1, LBIN
          JJ = J + IST
          XADD  = XADD  + XOBS(JJ)
          YOADD = YOADD + YOBS(JJ)
          YCADD = YCADD + YCAL(JJ)
          YBADD = YBADD + YBAK(JJ)
          VADD  = VADD  + EOBS(JJ)**2
        ENDDO
        XBIN(I)  =  XADD/FLOAT(LBIN)
        YOBIN(I) = YOADD/FLOAT(LBIN)
        YCBIN(I) = YCADD/FLOAT(LBIN)
        YBBIN(I) = YBADD/FLOAT(LBIN)
        EBIN(I)  = SQRT(VADD)/FLOAT(LBIN)
      ENDDO
      DataSetChange = DataSetChange + 1
      CALL Get_IPMaxMin() 

      END SUBROUTINE Rebin_Profile
!
!*****************************************************************************
!
