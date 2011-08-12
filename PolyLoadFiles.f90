!
!*****************************************************************************
!
! This file contains routines to read powder diffraction files.
!
!*****************************************************************************
!
      INTEGER FUNCTION DiffractionFileBrowse()
!
! This routine lets the user browse a directory for a diffraction file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
! This way, all diffraction-file-opening is dealt with by a single routine.
!
! JvdS 18 July 2001
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data/...)
!           2 if user pressed cancel
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, EXTERNAL :: DiffractionFileOpen
      CHARACTER(LEN=200)  FILTER
      INTEGER             iFlags
      INTEGER             IFTYPE    ! Needed for Winteracter routine
      CHARACTER(LEN=MaxPathLength) tFileName

      iFlags = LoadDialog + DirChange + PromptOn
! It seems that Winteracter cannot cope with strings of this length (> 255 bytes)
!      FILTER = ALL_FILES_FILTER//&
!               'All powder diffraction files|*.raw;*.rd;*.sd;*.udf;*.uxd;*.xye|'//&
!               'Bruker powder diffraction files (*.raw, *.uxd)|*.raw;*.uxd|'//&
!               'DASH powder diffraction files (*.xye)|*.xye|'//&
!               'Philips powder diffraction files (*.rd, *.sd, *.udf)|*.rd;*.sd;*.udf|'
      FILTER = ALL_FILES_FILTER//&
               'All powder diffraction files|*.asc;*.cpi;*.mdi;*.pod;*.raw;*.rd;*.sd;*.txt;*.udf;*.uxd;*.xye;*.x01|'//&
               'DASH powder diffraction files (*.xye)|*.xye|'
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      tFileName = ' '
      CALL WSelectFile(FILTER, iFlags, tFileName, 'Open Powder Diffraction File', IFTYPE)
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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: TheFileName


      INTEGER, EXTERNAL :: DiffractionFileLoad
      LOGICAL FExists
      INTEGER KLEN
! Note that FNAME is a global variable
      INTEGER ISTAT

      LOGICAL ELECDI
      COMMON /USEELE / ELECDI

      DiffractionFileOpen = 1
      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        RETURN
      ENDIF
      
      ELECDI = .FALSE.
! Update this throughout the program (Wizard + status bar)
      FNAME = TheFileName
      ISTAT = DiffractionFileLoad(TheFileName)
      DiffractionFileOpen = ISTAT
      IF (ISTAT .NE. 0) RETURN
! Enable the appropriate menus:
      CALL SetModeMenuState(1,-1)
      DashRawFile = FNAME(1:LEN_TRIM(FNAME))
      DefaultMaxResolution = DASHDefaultMaxResolution
      CALL Update_TruncationLimits

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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!           2 for user pressed cancel
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: TheFileName

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      INTEGER, EXTERNAL :: Load_asc_File
      INTEGER, EXTERNAL :: Load_cpi_File
      INTEGER, EXTERNAL :: Load_dat_File
      INTEGER, EXTERNAL :: Load_mdi_File
      INTEGER, EXTERNAL :: Load_raw_File
      INTEGER, EXTERNAL :: Load_pod_File
      INTEGER, EXTERNAL :: Load_rd_File
      INTEGER, EXTERNAL :: Load_sci_File ! Scintag. The extension is txt, but that is bound to generate clashes.
      INTEGER, EXTERNAL :: Load_udf_File
      INTEGER, EXTERNAL :: Load_uxd_File
      INTEGER, EXTERNAL :: Load_xye_File
      INTEGER, EXTERNAL :: Load_x01_File
      INTEGER          KLEN
      CHARACTER(LEN=4) EXT4
      INTEGER          ISTAT
      INTEGER          I
      INTEGER          POS
      LOGICAL          ESDsFilled
      REAL             INTEGRATED_GUESS
      INTEGER          MAX_INTENSITY_INDEX
      REAL             tYPMIN, tYPMAX

! Initialise to failure
      DiffractionFileLoad = 1
      Is_SX = .FALSE. ! Should not be necessary
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
      NoWavelengthInXYE = .FALSE.
      SELECT CASE (EXT4)
        CASE ('asc ')
          ISTAT = Load_asc_File(TheFileName, ESDsFilled)
        CASE ('cpi ')
          ISTAT = Load_cpi_File(TheFileName, ESDsFilled)
        CASE ('dat ')
          ISTAT = Load_dat_File(TheFileName, ESDsFilled)
        CASE ('mdi ')
          ISTAT = Load_mdi_File(TheFileName, ESDsFilled)
        CASE ('pod ')
          ISTAT = Load_pod_File(TheFileName, ESDsFilled)
        CASE ('raw ')
          ISTAT = Load_raw_File(TheFileName, ESDsFilled)
        CASE ('rd  ','sd  ')
          ISTAT =  Load_rd_File(TheFileName, ESDsFilled)
        CASE ('txt ')
          ISTAT = Load_sci_File(TheFileName, ESDsFilled)
        CASE ('udf ')
          ISTAT = Load_udf_File(TheFileName, ESDsFilled)
        CASE ('uxd ')
          ISTAT = Load_uxd_File(TheFileName, ESDsFilled)
        CASE ('xye ')
          ISTAT = Load_xye_File(TheFileName, ESDsFilled)
        CASE ('x01 ')
          ISTAT = Load_x01_File(TheFileName, ESDsFilled)
        CASE DEFAULT
          ISTAT = 1
      END SELECT
      DiffractionFileLoad = ISTAT
      IF (ISTAT .NE. 0) THEN
        CALL ErrorMessage('Could not load the file')
        RETURN
      ENDIF
      IF (ISTAT .EQ. 2) RETURN ! User pressed cancel somewhere
! Fill the E.S.D.s if that hasn't been taken care of yet
      IF (.NOT. ESDsFilled) THEN
        DO I = 1, NOBS
! Number of counts can be zero, especially at low theta due to a variable slit
! @ quick fix
          
          EOBS(I) = MAX(4.4, SQRT(MAX(1.0,YOBS(I))))
          IF ( YOBS(I) .GT. 10000.0 )     &
              EOBS(I) = 0.01 * YOBS(I)

        ENDDO
      ENDIF
! Reset points that have not been read to zero.
      IF (NOBS .LT. MOBS) THEN
        DO I = NOBS+1, MOBS
          XOBS(I) = 0.0
          YOBS(I) = 0.0
          EOBS(I) = 0.0
        ENDDO
      ENDIF

      BackupXOBS = 0.0
      BackupYOBS = 0.0
      BackupEOBS = 0.0
      BackupNOBS = NOBS
      DO I = 1, NOBS
        BackupXOBS(I) = XOBS(I)
        BackupYOBS(I) = YOBS(I)
        BackupEOBS(I) = EOBS(I)
      ENDDO


      IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN
        DataSetChange = DataSetChange + 1
        BackRef = .TRUE.
        CALL Clear_UnitCell
        CALL WMenuSetState(ID_Remove_Background, ItemEnabled, WintOn)
        tYPMIN = YOBS(1)
        tYPMAX = YOBS(1)
        DO I = 1, NOBS
          tYPMIN = MIN(YOBS(I),tYPMIN)
          IF (tYPMAX .LT. YOBS(I)) THEN
            MAX_INTENSITY_INDEX = I
            tYPMAX = YOBS(I)
          ENDIF
        ENDDO
        INTEGRATED_GUESS = 0.0
        DO I = MAX(1,MAX_INTENSITY_INDEX - 5), MIN(NOBS,MAX_INTENSITY_INDEX + 5)
          INTEGRATED_GUESS = INTEGRATED_GUESS + YOBS(I)
        ENDDO
        IF (INTEGRATED_GUESS .GT. 250000.0) THEN
          ScalFac = 0.01 * INTEGRATED_GUESS/250000.0
        ELSE IF (tYPMAX .GT. 100000.0) THEN
          ScalFac = 0.01 * tYPMAX/100000.0
        ENDIF
! Assume no knowledge on background
        CALL Clear_SA
        CALL sa_SetOutputFiles(TheFileName)
      ENDIF ! iRietveldMethod

      CALL Clear_BackGround
      CALL Clear_Bins
      CALL Rebin_Profile
      IPTYPE = 1
      NoData = .FALSE.
      CALL Clear_PeakFitRanges
      CALL ScrUpdateFileName
! Grey out the "Previous Results >" button in the DICVOL Wizard window
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page8)
      CALL WDialogFieldState(IDB_PrevRes, Disabled)
      CALL PopActiveWindowID

      END FUNCTION DiffractionFileLoad
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_asc_File(TheFileName, ESDsFilled)
!
! This function tries to load a *.asc file (ASCII format from Rigaku).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER, EXTERNAL :: GetNumOfColumns
      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      INTEGER, EXTERNAL :: StrFind
      CHARACTER*255 tString ! String containing last line read from file
      CHARACTER*255 tSubString, Cline
      INTEGER       tLen, NumOfBins, NumOfBinsFile
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      INTEGER       I, J, hFile, NumOfColumns2Read
      INTEGER       MEAS_MODE ! 0 = undefined, 1 = continuous, 2 = step
      REAL          Lambda1
      REAL          TempInput(8) ! Max. num. of columns is 8

! Current status, initialise to 'error'
      Load_asc_File = 1
      Lambda1       = 0.0
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      NumOfBinsFile = 0
      MEAS_MODE = 0
      hFile = 10
      OPEN(UNIT=hFile, FILE=TheFileName, STATUS='OLD', ERR=999)
! *.asc files look like this:
!
!*TYPE		=  Raw
!*CLASS		=  ASCII CLASS
!*SAMPLE		=  ASZTK
!*COMMENT	=  Qualitative (demo)
!*FNAME		=  Asztk.raw
!*DATE		=  25-Dec-97 19:12
!
!*GROUP_COUNT	=  1                         I think that GROUP_COUNT = number of dataranges
!*GONIO		=  Wide angle goniometer, 185
!*ATTACHMENT	=  Standerd specimen attachment
!*ASC		=  0, 0, 0.000000, 0.000000
!*FILTER		=  K beta filter
!*SLIT_NAME	=  0, Divergence
!*SLIT_NAME	=  1, Scattering
!*SLIT_NAME	=  2, Receiving
!*COUNTER	=  , 0
!*POS_FORMAT	=  0
!*SCAN_AXIS	=  2theta/theta
!*MEAS_MODE	=  Continuous Scanning
!*TARGET		=  29
!*XRAY_CHAR	=  K-ALPHA1
!*WAVE_LENGTH1	=  1.54056
!*WAVE_LENGTH2	=  1.5444
!*THICKNESS	=  0, 0.000000
!*MU		=  0, 0.000000
!*SCAN_MODE	=  2theta/theta
!*SPEED_DIM	=  sec./step
!*XUNIT		=  deg.
!*YUNIT		=  cps
!*SCALE_MODE	=  1
!*REP_COUNT	=  1
!*SE_COUNT	=  12
!*SYS_ERROR	=  0, 28.394, 0.000000, 1, 1, 1
!<snip>
!*SYS_ERROR	=  11, 0.000000, 0.000000, 0, 0, 0
!*STD_MATERIAL	=  Unknown, Unknown, 5.4301, 5.4301, 5.4301, 90, 90, 90
!*LATT_CONS	=  0, Unknown, Unknown, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000
!*SEC_COUNT	=  1
!*TSPEC_SIZE	=  0
!*EXTRA_SIZE	=  0
!*MEMO		=  
!
!*BEGIN
!*GROUP		=  0                 The BEGIN/END/GROUP labels suggest that multiple data ranges are possible.
!*START		=  3
!*STOP		=  90
!*STEP		=  0.02
!*OFFSET		=  0.000000
!*SPEED		=  0.4
!*SLIT_SPEC	=  0, Slit ( 60 min ), 1.74, 10
!*SLIT_SPEC	=  1, Slit ( 60 min ), 0.94, 20
!*SLIT_SPEC	=  2, Slit ( 0.15 mm ), 0.15, 20
!*KV		=  60
!*MA		=  200
!*LOW		=  0.000000
!*HIGH		=  0.000000
!*CTEMPER	=  0, 0.000000
!*CTEMPER	=  1, 0.000000
!*CTEMPER	=  2, 0.000000
!*PAREX		=  0, 0.000000
!<snip>
!*PAREX		=  15, 0.000000
!*FULL_SCALE	=  30000
!*INDEX		=  0, 0, 0
!*COUNT		=  4351                                The number of values (bins)
!948, 952.8, 990, 962.8
!994.8, 948, 938.4, 890.4
!931.2, 897.6, 912, 907.2
!<snip>
!181.6, 173.2, 213.6, 241.2
!333.2, 417.6, 469.2, 425.6
!354, 278, 230.4, 174
!119.6, 126, 0.000000
!*END
!
!*EOF
!
!
    9 READ(hFile, FMT='(A)', ERR=999, END=999) tString
      CALL StrUpperCase(tString)
      CALL StrClean(tString, tLen)
      IF (StrFind(tString, tLen, '*WAVE_LENGTH1', 13) .NE. 0) THEN
! Extract wavelength
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        READ (tSubString, *, ERR=999) Lambda1
      ENDIF
      IF (StrFind(tString, tLen, '*MEAS_MODE', 10) .NE. 0) THEN
! Extract measurement mode (step / scan)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        IF (tSubString .EQ. "CONTINUOUS") MEAS_MODE = 1
      ENDIF
      IF (StrFind(tString, tLen, '*START', 6) .NE. 0) THEN
! Extract TwoThetaStart
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        READ (tSubString, *, ERR=999) TwoThetaStart
      ENDIF
      IF (StrFind(tString, tLen, '*STOP', 5) .NE. 0) THEN
! Extract TwoThetaEnd
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        READ (tSubString, *, ERR=999) TwoThetaEnd
      ENDIF
      IF (StrFind(tString, tLen, '*STEP', 5) .NE. 0) THEN
! Extract TwoThetaStep
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        READ (tSubString, *, ERR=999) TwoThetaStep
      ENDIF
! Detect start of data
      IF (StrFind(tString, tLen, '*COUNT =', 8) .NE. 0) THEN
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        CALL GetSubString(tString, ' ', tSubString)
        READ (tSubString, *, ERR=999) NumOfBinsFile
        GOTO 10
      ENDIF
      GOTO 9
   10 CONTINUE
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
      IF (NumOfBinsFile .EQ. 0) THEN
        CALL ErrorMessage('Number of data points not found not found.')
        GOTO 999
      ENDIF
! Note that NINT correctly rounds to the nearest whole number
      NumOfBins = 1 + NINT((TwoThetaEnd - TwoThetaStart) / TwoThetaStep)
      ! Adjust number of bins if continuous scan
      IF (MEAS_MODE .EQ. 1) NumOfBins = NumOfBins - 1
! Check that we will not read more than MOBS data points
      IF (NumOfBins .GT. MOBS) THEN
! Warn the user
        CALL ProfileRead_TruncationWarning(TheFileName, MOBS)
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
      IF (MEAS_MODE .EQ. 1) CurrTwoTheta = CurrTwoTheta - (TwoThetaStep/2.0)
      DO I = 1, NumOfBins
        XOBS(I) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + TwoThetaStep
      ENDDO
      READ(UNIT=hFile, FMT='(A)', ERR=999, END=100) Cline
      I = 0
! Next, we have to determine how many columns to read from the next line.
! There are two conditions:
! 1. the number of columns actually present in the string (GetNumOfColumns)
! 2. the number of data point still to be read (NumOfBins - I)
   11 NumOfColumns2Read = MIN(GetNumOfColumns(Cline),NumOfBins - I)
      READ(Cline, *, ERR=999) TempInput(1:NumOfColumns2Read)
! Next couple of lines rather clumsy, but safe.
      DO J = 1, NumOfColumns2Read
        YOBS(I+J) = TempInput(J)
      ENDDO
      I = I + NumOfColumns2Read
      READ(UNIT=10, FMT='(A)', ERR=999, END=100) Cline
      GOTO 11
  100 NOBS = I
      IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                   'Will continue with points actually read only.')
      IF (NOBS .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
      ESDsFilled = .FALSE.
! If wavelength present in file add in a test: if wavelength close to known anode material,
! set source to laboratory. Otherwise, source is synchrotron.
      IF (Lambda1 .GT. 0.01) THEN
! Initialise source material to synchrotron
        JRadOption = 2
        DO I = 2, 6
          IF (ABS(Lambda1 - FnWavelengthOfMenuOption(I)) .LT. 0.0003) JRadOption = 1
        ENDDO
        CALL Upload_Source
        CALL Set_Wavelength(Lambda1)
      ENDIF
      Load_asc_File = 0
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_asc_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_cpi_File(TheFileName, ESDsFilled)
!
! This function tries to load a *.cpi file (ASCII format from Sietronics).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER, EXTERNAL :: GetNumOfColumns
      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      CHARACTER*255 tString ! String containing last line read from file
      INTEGER       I, NumOfBins
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      INTEGER       hFile
      REAL          Lambda1
      CHARACTER*2   Anode

! Current status, initialise to 'error'
      Load_cpi_File = 1
      Lambda1       = 0.0
      Anode         = 'Xx'
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      hFile = 10
      OPEN(UNIT=hFile, FILE=TheFileName, STATUS='OLD', ERR=999)
! *.cpi files look like this:
!
!SIETRONICS XRD SCAN               ! File identification marker (copy exactly)
!5.000                             ! Start angle
!77.000                            ! End angle
!0.020                             ! Step size
!Cu                                ! Tube anode
!1.54060                           ! Wavelength
!29-4-2002                         ! Date of scan
!0.080                             ! Scan speed
!SMRR-1                            ! Comment
!SCANDATA                          ! Start of scan data marker (upper case)
!3410
!3537
!3406
!3278
! <SNIP>
!367
!369
!373
!328
!

! Read the header line
      READ(hFile, FMT='(A)', ERR=999, END=999) tString
      IF ((tString .NE. 'SIETRONICS XRD SCAN') .AND. (tString .NE. 'Calculated Values from PowderCell')) THEN
        CALL ErrorMessage('File identification marker missing.')
        GOTO 999
      ENDIF
      READ(hFile, *, ERR=999, END=999) TwoThetaStart
      READ(hFile, *, ERR=999, END=999) TwoThetaEnd
      READ(hFile, *, ERR=999, END=999) TwoThetaStep
      READ(hFile, '(A2)', ERR=999, END=999) Anode
      READ(hFile, *, ERR=999, END=999) Lambda1
      READ(hFile, FMT='(A)', ERR=999, END=999) tString
      DO WHILE (tString .NE. 'SCANDATA')
        READ(hFile, FMT='(A)', ERR=999, END=999) tString
      ENDDO
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
        CALL ProfileRead_TruncationWarning(TheFileName, MOBS)
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
      DO I = 1, NumOfBins
        READ(hFile,*,ERR=999,END=100) YOBS(I)
      ENDDO
  100 NOBS = I - 1
      IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                   'Will continue with points actually read only.')
      IF (NOBS .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
      ESDsFilled = .FALSE.
! If wavelength present in file add in a test: if wavelength close to known anode material,
! set source to laboratory. Otherwise, source is synchrotron.
      IF (Lambda1 .GT. 0.01) THEN
! Initialise source material to synchrotron
        JRadOption = 2
        DO I = 2, 6
          IF (ABS(Lambda1 - FnWavelengthOfMenuOption(I)) .LT. 0.0003) JRadOption = 1
        ENDDO
        CALL Upload_Source
        CALL Set_Wavelength(Lambda1)
      ENDIF
      Load_cpi_File = 0
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_cpi_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_dat_File(TheFileName, ESDsFilled)
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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER, EXTERNAL :: GetNumOfColumns
      CHARACTER*255 Cline ! String containing last line read from file
      INTEGER       I, J, NumOfBins
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      REAL          TempInput(8) ! Max. num. of columns is 8
      INTEGER       NumOfColumns2Read, hFile

! Current status, initialise to 'error'
      Load_dat_File = 1
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      hFile = 10
      OPEN(UNIT=hFile, FILE=TheFileName, STATUS='OLD', ERR=999)
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
    9 READ(hFile, *, ERR=999, END=999) TwoThetaStart, TwoThetaStep, TwoThetaEnd
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
        CALL ProfileRead_TruncationWarning(TheFileName, MOBS)
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
      READ(UNIT=hFile,FMT='(A)',ERR=999,END=100) Cline
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
  100 NOBS = I
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
      Load_dat_File = 0
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_dat_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_mdi_File(TheFileName, ESDsFilled)
!
! This function tries to load a *.mdi file (ASCII format from Materials Data Inc.).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! JvdS June 2002
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL, EXTERNAL :: WavelengthOf, FnWavelengthOfMenuOption
      CHARACTER*255 tString ! String containing last line read from file
      CHARACTER*255 tSubString
      INTEGER       I, NumOfBins, hFile, iDummy
      REAL          Lambda1
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta, rDummy
      CHARACTER*2   Anode
      CHARACTER*2 ListIn, ListOut

! Current status, initialise to 'error'
      Load_mdi_File = 1
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Anode         = 'Xx'
      Lambda1       = 0.0
      hFile         = 10
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
! *.mdi files look like this:

!       09/11/91 DIF Demo08: 10-173
! 10.000 0.0400  4.0 CU  1.540598 100.000   2251
!     120     124     108     112     128     120     112     112
!     112     120     120     120     116     120     108     116
! <SNIP>
!     168     168     160     148     160     160     176     176
!     168     176     168      -1
!.

! Read the header lines
      READ(hFile,'(A)',ERR=999,END=999) tString
      READ(hFile,*,ERR=999,END=999) TwoThetaStart, TwoThetaStep, rDummy, Anode, Lambda1, TwoThetaEnd, NumOfBins
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
! Check if number of points consistent
! Note that NINT correctly rounds to the nearest whole number
      IF (NumOfBins .NE. (1 + NINT((TwoThetaEnd - TwoThetaStart) / TwoThetaStep)) ) THEN
        CALL WarningMessage('Number of data points inconsistent.')
        NumOfBins = MIN(NumOfBins,(1 + NINT((TwoThetaEnd - TwoThetaStart) / TwoThetaStep)))
      ENDIF
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
   20 READ(hFile,FMT='(A)',ERR=999,END=100) tString
! Replace tabs by spaces
      ListIn(1:1) = CHAR(8)
      ListIn(2:2) = '.'
      ListOut(1:1) = ' '
      ListOut(2:2) = '.'
      CALL StrReplace(tString,ListIn,ListOut)
      CALL StrClean(tString, iDummy)
      CALL GetSubString(tString,' ',tSubString)
      DO WHILE (LEN_TRIM(tSubString) .NE. 0)
        IF (I .GT. MOBS) GOTO 100
        READ(tSubString,*,ERR=999) YOBS(I)
        I = I + 1
        CALL GetSubString(tString,' ',tSubString)
      ENDDO
! Fetch the next line from the file
      GOTO 20
  100 NOBS = I - 1
! Last value read should be -1: remove that point
      IF (ABS(YOBS(NOBS) + 1.0) .LT. 0.000001) NOBS = NOBS - 1
      IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                   'Will continue with points actually read only.')
      IF (NOBS .GT. NumOfBins) THEN
        CALL WarningMessage('File contained more data points than expected.'//CHAR(13)// &
                            'Will continue with expected points only.')
        NOBS = NumOfBins
      ENDIF
      IF (NOBS .EQ. 0) THEN
! The user should be warned here
        CALL ErrorMessage("The file does not contain enough data points.")
        GOTO 999
      ENDIF
      ESDsFilled = .FALSE.
! If the wavelength was not present in the file, try to interpret the Anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
! If wavelength present in file add in a test: if wavelength close to known anode material,
! set source to laboratory. Otherwise, source is synchrotron.
      IF (Lambda1 .GT. 0.01) THEN
! Initialise source material to synchrotron
        JRadOption = 2
        DO I = 2, 6
          IF (ABS(Lambda1 - FnWavelengthOfMenuOption(I)) .LT. 0.0003) JRadOption = 1
        ENDDO
        CALL Upload_Source
        CALL Set_Wavelength(Lambda1)
      ENDIF
      Load_mdi_File = 0
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_mdi_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_pod_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.pod file (ASCII format from Daresbury).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! JvdS 5 March 2002
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
! In this case, although the file does not contain ESDs, it does contain 
! an incident beam correction, so the ESDs calculated here are the 'right' ones
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

!&SRS
! SRSRUN = Sum of 9129                                                           
!SRSSTN='PD23',SRSPRJ='POWDERDF',SRSEXP='12345432',                              
! SRSTLE='Zn(TPP)(DABCO) sandwich                                     ',         
!SRSCN1='        ',SRSCN2='        ',SRSCN3='        ',                          
!&END                                                                            
! c    tth =    3.0000,                                                          
! c    ome =    1.5000,                                                          
! c   mono =   11.0359,                                                          
! c  slits =      0.00,                                                          
! c  table =    0.2000,                                                          
! c   rot1 =      0.00,                                                          
! c   rot2 =      0.00,                                                          
! c  trans =      0.00,                                                          
! c    chi =    90.000,                                                          
! c    phi =    0.0100,                                                          
! c   scat =    0.0000,                                                          
! c  spare =    0.0000,                                                          
! c  bt-fh =     0.000,                                                          
! c  bt-bh =     0.000,                                                          
! c  bt-fv =     0.000,                                                          
! c  bt-bv =     0.000,                                                          
! c  bsl-r =     5.000,                                                          
! c  bsl-l =     5.000,                                                          
! c  bsl-t =     0.500,                                                          
! c  bsl-b =     0.500,                                                          
!    TWOTHETA   OMEGA   TIME    CHAN1    CHAN2     CHAN3   SCALE
!    3000.       0.       0.   19094.       0.    1545.   1.3044
!    3010.       0.       0.   19094.       0.    1504.   1.3261
!    3020.       0.       0.   19094.       0.    1540.   1.3151
! <SNIP>
!   69980.       0.       0.   19094.       0.    1181.   0.8063
!   69990.       0.       0.   19094.       0.    1180.   0.7983
!   70000.       0.       0.   19094.       0.    1185.   0.7856
!SCAN ENDED

      INTEGER I
      CHARACTER*255 tString, tSubString
      INTEGER hFile, tLen
      REAL    TwoTheta, rDummy1, rDummy2, rDummy3, rDummy4, tSignal, tScale

! Initialise to failure
      Load_pod_File = 1
      I = 1
      hFile = 10
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
! Skip start
      tSubString = ''
      DO WHILE (tSubString .NE. 'TWOTHETA')
        READ(hFile,FMT='(A)',ERR=999,END=999) tString
        CALL StrClean(tString,tLen)
        CALL GetSubString(tString,' ',tSubString)
      ENDDO
 10   READ(hFile,FMT='(A)',ERR=999,END=100) tString
      IF (tString .EQ. 'SCAN ENDED') GOTO 100
      READ(tString,*,ERR=999) TwoTheta, rDummy1, rDummy2, rDummy3, rDummy4, tSignal, tScale
      XOBS(I) = TwoTheta / 1000.0
! CHAN3 has already been corrected for variation of incident beam intensity
      YOBS(I) = tSignal
! which has consequences for the calculation of the ESDs
      EOBS(I) = SQRT(MAX(1.0,(tSignal/tScale)))*tScale
! Skip negative 2-theta data and negative intensities
      IF (XOBS(I) .LE. 0.0) GOTO 10
      IF (YOBS(I) .LT. 0.0) GOTO 10
      CALL INC(I)
! Only read in a maximum of MOBS points
      IF (I .GT. MOBS) THEN
        CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
        GOTO 100
      ENDIF
      GOTO 10
 100  NOBS = I - 1
      ESDsFilled = .TRUE.
! JvdS Added check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        GOTO 999
      ENDIF
! This is definitely synchrotron data
      JRadOption = 2
      CALL Upload_Source
      Load_pod_File = 0
 999  CLOSE(hFile)
 
      END FUNCTION Load_pod_File
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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL, EXTERNAL :: WavelengthOf
      INTEGER      I, NumOfBins
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
      INTEGER      Offset, hFile
      LOGICAL      CONTINUOUS

! Current status, initialise to 'error'
      Load_rd_File  = 1
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Lambda1       = 0.0
      Anode         = 'Xx'
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=TheFileName,ACCESS='DIRECT',RECL=cRECLMult,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
      READ(hFile,REC=1,ERR=999) C4
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
      READ(hFile,REC=22,ERR=999) I4
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
      READ(hFile,REC=24,ERR=999) I4
      READ(hFile,REC=25,ERR=999) I4_2
      READ(hFile,REC=26,ERR=999) I4_3
      DO I = 1, 10
        I1(I) = I1(I+2)
      ENDDO
      Lambda1 = R8
      READ(hFile,REC=32,ERR=999) I4
! Due to EQUIVALENCE, I1(3) now contains CONTINUOUS (=TRUE=1) or STEP (=FALSE)
      CONTINUOUS = I1(3) .GT. 0
! Position 128 contains the step size as a 'packed angle'.
      READ(hFile,REC=33,ERR=999) I4
! Due to EQUIVALENCE, I2(1) now contains Step Size as a 'packed angle'.
      TwoThetaStep = I2(1) / 200.0
! Due to EQUIVALENCE, I2(2) now contains Start Angle as a 'packed angle'.
      TwoThetaStart = I2(2) / 200.0
      READ(hFile,REC=34,ERR=999) I4
! Due to EQUIVALENCE, I2(1) now contains Final Angle as a 'packed angle'.
      TwoThetaEnd = I2(1) / 200.0
      IF (Version .NE. 'D3') THEN
! Step Size, Start Angle and Final Angle are given as REAL*8 on 2 byte boundaries again
        READ(hFile,REC=54,ERR=999) I4
        READ(hFile,REC=55,ERR=999) I4_2
        READ(hFile,REC=56,ERR=999) I4_3
        DO I = 1, 10
          I1(I) = I1(I+2)
        ENDDO
        TwoThetaStep = R8
        READ(hFile,REC=56,ERR=999) I4
        READ(hFile,REC=57,ERR=999) I4_2
        READ(hFile,REC=58,ERR=999) I4_3
        DO I = 1, 10
          I1(I) = I1(I+2)
        ENDDO
        TwoThetaStart = R8
        READ(hFile,REC=58,ERR=999) I4
        READ(hFile,REC=59,ERR=999) I4_2
        READ(hFile,REC=60,ERR=999) I4_3
        DO I = 1, 10
          I1(I) = I1(I+2)
        ENDDO
        TwoThetaEnd = R8
      ENDIF
! If it is a 'V5' file, another header follows, which also contains the anode material
! Documentation says they should always match, but advises to use the second instance
! It's the first byte of record 93
      IF (Version .EQ. 'V5') THEN
        READ(hFile,REC=93,ERR=999) I4
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
      READ(hFile,REC=Offset,ERR=999) I4
! Due to EQUIVALENCE, the first data point is now in I2(2)
      YOBS(1) = (I2(2) * 0.1)**2
! NumOfBins is now odd, and ((NumOfBins/2)*2)+1 = NumOfBins
      DO I = 1, NumOfBins/2
        READ(hFile,REC=Offset+I,ERR=999) I4
        YOBS(I*2  ) = (I2(1) * 0.1)**2
        YOBS(I*2+1) = (I2(2) * 0.1)**2
      ENDDO
      NOBS = NumOfBins
      ESDsFilled = .FALSE.
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the Anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
      CALL Set_Wavelength(Lambda1)
      Load_rd_File = 0
! Exit code is error by default, so we can simply return
  999 CLOSE(hFile)

      END FUNCTION Load_rd_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_sci_File(TheFileName, ESDsFilled)
!
! This function tries to load a *.txt file (Scintag ASCII powder pattern format).
!
! Note that this function should only be called from DiffractionFileLoad
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled
! Comment on ESDsFilled for this file format:
!
! This is not a very well documented format, and the file never contains the
! number of seconds counted for. It does sometimes contain a column with the number
! of seconds counted for at that data point, but it is unclear if this is supposed to be
! constant throughout the file (in which case the column is superfluous).
! Therefore, all counts are always transformed to "counts per second".
! (This should be in the documentation!)
! As a result, the ESD must always be either read in or calculated per line.

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER, EXTERNAL :: GetNumOfColumns, StrFind
      REAL, EXTERNAL :: WavelengthOf
      CHARACTER*255 Cline, tString, tSubString
      INTEGER       I, NumOfBins, hFile, tLen
      LOGICAL       ReadWarning
      REAL          Lambda1
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      LOGICAL       IsCPS, ContainsESDs, ContainsAngles, Contains2ThetaInfo, CountsLineFound
      INTEGER       iCase
      REAL          cps

!C An example:
!
!Filename:
!	E:\CSR\CSR 64 Limonite Montmorillonite Synthetix materials\montmorillonite.raw
!
!File ID:
!	Uni Aless
!
!File Created:
!	February 06, 2002 - 05:31 AM
!
!Comment:
!	45 kV, 40 mA
!
!Scan Type: Normal
!Start Angle: 1.9 deg.
!Stop Angle: 69.94 deg.
!Num Points: 2269
!Step Size: 0.03 deg.
!Datafile Res: 2000 
!Scan Rate: 0.200000
!Scan Mode: Continuous
!Wavelength: 1.540562
!
!Diffractometer Optics:
!		Detector:
!			 Type: Fixed Slits
!		Tube: 
!			 Type: Fixed Slits
!
!
!Axis[0]
!	 Selected   : no 
!	 Iterative  : no 
!	 Start Angle: 0.000000 
!	 Stop Angle : 0.000000 
!	 Step Size  : 0.000000 
!	 Oscillating: no 
!
!Axis[1]
!	 Selected   : no 
!	 Iterative  : no 
!	 Start Angle: 0.000000 
!	 Stop Angle : 0.000000 
!	 Step Size  : 0.000000 
!	 Oscillating: no 
!
!<SNIP>
!
!Axis[9]
!	 Selected   : no 
!	 Iterative  : no 
!	 Start Angle: 0.000000 
!	 Stop Angle : 0.000000 
!	 Step Size  : 0.000000 
!	 Oscillating: no 
!
!
!
!
!    Deg.        Counts        Time           ESD
!
!Range 0
!   1.900         52908        9000       230.017
!   1.930         51444        9000       226.813
!   1.960         50374        9000       224.442
!
!<SNIP>
!
!  69.880          1366        9000        36.959
!  69.910          1485        9000        38.536
!  69.940          1440        9000        37.947
!
!C
!C Or:
!C
!
!    Deg.           CPS         ESD
!
!Range 0
!   1.900       5878.67        25.557
!   1.930       5716.00        25.201
!   1.960       5597.11        24.938
!
!<SNIP>
!
!  69.880        151.78         4.107
!  69.910        165.00         4.282
!  69.940        160.00         4.216
!

! Initialise to failure
      Load_sci_File = 1
      TwoThetaStart = -1.0
      TwoThetaEnd   = -1.0
      TwoThetaStep  = -1.0
      Lambda1       = WavelengthOf('Cu')
      NumOfBins     = -1
      ReadWarning   = .FALSE.
      CountsLineFound = .FALSE.
      iCase = 0
      cps = 1.0
      hFile = 10
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
    9 READ(hFile,FMT='(A)',ERR=999,END=999) tString
      CALL StrUpperCase(tString)
      IF (tString(1:11) .EQ. 'WAVELENGTH:') THEN
! Extract wavelength
        CALL StrClean(tString,tLen)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) Lambda1
      ENDIF
      IF (tString(1:12) .EQ. 'START ANGLE:') THEN
! Extract starting 2 theta angle
        CALL StrClean(tString,tLen)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) TwoThetaStart
      ENDIF
      IF (tString(1:11) .EQ. 'STOP ANGLE:') THEN
! Extract ending 2 theta angle
        CALL StrClean(tString,tLen)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) TwoThetaEnd
      ENDIF
      IF (tString(1:11) .EQ. 'NUM POINTS:') THEN
! Extract number of data points
        CALL StrClean(tString,tLen)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) NumOfBins
      ENDIF
      IF (tString(1:10) .EQ. 'STEP SIZE:') THEN
! Extract 2 theta angle step size
        CALL StrClean(tString,tLen)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) TwoThetaStep
      ENDIF
! Detect start of data
      IF (tString(1:5) .EQ. 'RANGE') GOTO 11
      CALL StrClean(tString,tLen)
      IF ((StrFind(tString,tLen,'CPS',3)    .NE. 0) .OR.  &
          (StrFind(tString,tLen,'COUNTS',6) .NE. 0)) THEN
        IsCPS = (StrFind(tString,tLen,'CPS',3) .NE. 0)
        CountsLineFound = .TRUE.
        ContainsAngles = (StrFind(tString,tLen,'DEG.',4) .NE. 0)
        ContainsESDs = (StrFind(tString,tLen,'ESD',3) .NE. 0)
        IF (IsCPS .AND. (.NOT. ContainsESDs)) THEN
          CALL ErrorMessage('This file does not contain enough information'//CHAR(13)// &
                            'to estimate the standard deviations.'//CHAR(13)//CHAR(13)// &
                            'Please re-generate this file, either:'//CHAR(13)// &
                            'with "Includes" | "ESD" ticked, and/or'//CHAR(13)// &
                            'with "Units" set to "Counts" rather than "CPS"')
          RETURN
        ENDIF
        IF (ContainsAngles) iCase = iCase + 1
        IF (.NOT. IsCPS)    iCase = iCase + 2
        IF (ContainsESDs)   iCase = iCase + 4
      ENDIF
      GOTO 9
   11 CONTINUE
! Determine where to get the 2 theta info from: from the header or from the count data
      IF (.NOT. CountsLineFound) THEN
! Problem: we don't know what to read.
        CALL ErrorMessage('No header info for columns.')
        RETURN
      ENDIF
      Contains2ThetaInfo = (TwoThetaStart .GT. 0.0 .AND.   &
                            TwoThetaStep  .GT. 0.0 .AND.   &
                            TwoThetaEnd   .GT. 0.0)
      IF ((.NOT. ContainsAngles) .AND. (.NOT. Contains2ThetaInfo)) THEN
        CALL ErrorMessage('No 2 theta information.')
        RETURN
      ENDIF
! Check consistency # of points vs. 2 theta info
      IF ((NumOfBins .GT. 0) .AND. Contains2ThetaInfo) THEN
        IF (NumOfBins .NE. 1+NINT((TwoThetaEnd-TwoThetaStart)/TwoThetaStep)) THEN
          CALL ErrorMessage('2 theta information and number of data points not consistent.')
          RETURN
        ENDIF
      ENDIF
! Fill the 2theta values first
      CurrTwoTheta = TwoThetaStart
      DO I = 1, NumOfBins
        XOBS(I) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + TwoThetaStep
      ENDDO
      I = 1
   10 READ(UNIT=hFile,FMT='(A)',ERR=999,END=100) Cline
      SELECT CASE (iCase)
        CASE (2) ! Counts, time
          READ(Cline,*,ERR=999,END=100) YOBS(I), cps
        CASE (3) ! Angle, Counts, time
          READ(Cline,*,ERR=999,END=100) XOBS(I), YOBS(I), cps
        CASE (4) ! CPS, ESD
          READ(Cline,*,ERR=999,END=100) YOBS(I), EOBS(I)
        CASE (5) ! Angle, CPS, ESD
          READ(Cline,*,ERR=999,END=100) XOBS(I), YOBS(I), EOBS(I)
        CASE (6) ! Counts, time, ESD
          READ(Cline,*,ERR=999,END=100) YOBS(I), cps, EOBS(I)
        CASE (7) ! Angle, Counts, time, ESD
          READ(Cline,*,ERR=999,END=100) XOBS(I), YOBS(I), cps, EOBS(I)
        CASE DEFAULT
          CALL ErrorMessage('Programming error in Load_sci_File()')
          RETURN
      END SELECT
      IF (cps .LT. 1) GOTO 10
      IF (.NOT. IsCPS) THEN
        cps = cps / 1000.0
        IF (.NOT. ContainsESDs) EOBS(I) = SQRT(YOBS(I))
        YOBS(I) = YOBS(I) / cps
        EOBS(I) = EOBS(I) / cps
      ENDIF
! Skip negative 2-theta data and negative intensities
      IF (XOBS(I) .LE. 0.0) GOTO 10
      IF (YOBS(I) .LT. 0.0) GOTO 10
      IF (I .GT. 1) THEN
        IF (ABS(XOBS(I) - XOBS(I-1)) .LT. 0.0000001) THEN
          IF (.NOT. ReadWarning) THEN
            ReadWarning = .TRUE.
            CALL WarningMessage("The data file contains multiple observations for the same 2-theta."//CHAR(13)// &
                                "Only one observation will be used.")
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
      IF (NumOfBins .NE. -1) THEN
        IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                     'Will continue with points actually read only.')
        IF (NOBS .GT. NumOfBins) THEN
          CALL WarningMessage('File contained more data points than expected.'//CHAR(13)// &
                              'Excess points will be ignored.')
          NOBS = NumOfBins
        ENDIF
      ENDIF
      ESDsFilled = .TRUE.
! JvdS Added check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        GOTO 999
      ENDIF
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
      CALL Set_Wavelength(Lambda1)
      Load_sci_File = 0
 999  CLOSE(hFile)

      END FUNCTION Load_sci_File
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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL, EXTERNAL :: WavelengthOf
      CHARACTER*255 Cline ! String containing last line read from file
      CHARACTER*255 tSubString
      INTEGER       I, NumOfBins, hFile
      REAL          Lambda1
      REAL          TwoThetaStart, TwoThetaEnd, TwoThetaStep, CurrTwoTheta
      CHARACTER*2   Anode

! Current status, initialise to 'error'
      Load_udf_File = 1
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Anode         = 'Xx'
      Lambda1       = 0.0
      hFile = 10
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
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
    9 READ(hFile,FMT='(A)',ERR=999,END=999) Cline
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
   20 READ(hFile,FMT='(A)',ERR=999,END=100) Cline
      CALL GetSubString(Cline,',',tSubString)
      DO WHILE ((LEN_TRIM(tSubString) .NE. 0) .AND. (tSubString(1:LEN_TRIM(tSubString)) .NE. '/'))
        IF (I .GT. MOBS) GOTO 100
        READ(tSubString,*,ERR=999) YOBS(I)
        I = I + 1
        CALL GetSubString(Cline,',',tSubString)
      ENDDO
! Fetch the next line from the file
      GOTO 20
  100 NOBS = I - 1
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
      Load_udf_File = 0
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_udf_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_uxd_File(TheFileName, ESDsFilled)
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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER, EXTERNAL :: GetNumOfColumns
      CHARACTER*1, EXTERNAL :: ChrLowerCase, ChrUpperCase
      REAL, EXTERNAL :: WavelengthOf
      CHARACTER*511 Cline ! String containing last line read from file
      INTEGER       J, MaxNumOfBins
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
      INTEGER       KeyWordPos, KeyWordLen, StrLen, I, hFile

! Current status, initialise to 'error'
      Load_uxd_File = 1
      TwoThetaStart = 0.0
      TwoThetaEnd   = 0.0
      TwoThetaStep  = 0.0
      Anode         = 'Xx'
      Lambda1       = 0.0
      Mode          = THETACOUNTS ! Default
      MaxNumOfBins  = MOBS
      MaxNumOfColumns = 16
      hFile = 10
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
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
 10   READ(hFile,FMT='(A)',ERR=999,END=100) Cline
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
              GOTO 999
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
                GOTO 999
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
               ENDDO
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
               READ(hFile,FMT='(A)',ERR=999,END=100) Cline
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
               ENDDO
               I = I + 1
               IF (I .GT. MOBS) THEN
                 CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
                 GOTO 100
               ENDIF
               READ(hFile,FMT='(A)',ERR=999,END=100) Cline
               GOTO 12
           END SELECT
      END SELECT
      GOTO 10
 100  NOBS = I - 1
      ESDsFilled = .FALSE.
! Check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        GOTO 999
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
      Load_uxd_File = 0
 999  CLOSE(hFile)
! Exit code is error by default, so we can simply return

      END FUNCTION Load_uxd_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_xye_File(TheFileName, ESDsFilled)
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
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      INTEGER, EXTERNAL :: GetNumOfColumns
      CHARACTER*255 Cline
      INTEGER       I, IS, hFile
      LOGICAL       ReadWarning
      REAL          Lambda1

! Initialise to failure
      Load_xye_File = 1
      hFile         = 10
      Lambda1       = 0.0
      ReadWarning   = .FALSE.
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      I = 1
! Check if wavelength available on very first line
      READ(UNIT=hFile,FMT='(A)',ERR=999,END=999) Cline
      IF (GetNumOfColumns(Cline) .EQ. 1) THEN
        READ(Cline,*,ERR=999,END=999) Lambda1
        IF ((Lambda1 .LT. 0.0001) .OR. (Lambda1 .GT. 20.0)) THEN
          CALL ErrorMessage('First line contains only one column, but not a valid wavelength.')
          GOTO 999
        ENDIF
      ELSE
! If we are here, the .xye file didn't contain the wavelength
        NoWavelengthInXYE = .TRUE.
        CLOSE(hFile)
        OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      ENDIF
! Modified to handle files without esds - used to read in YOBS as the esd
 10   READ(hFile,FMT='(A)',ERR=999,END=100) Cline
      READ(Cline,*, IOSTAT = IS) XOBS(I),YOBS(I),EOBS(I)
      IF (IS .NE. 0) THEN
        READ(Cline,*, ERR=100,END=100) XOBS(I),YOBS(I)
! Skip negative intensities without ESD
        IF (YOBS(I) .LT. 0.0) GOTO 10
        IF (ABS(YOBS(I)) .LT. 0.0000001) THEN
          EOBS(I) = 1
        ELSE IF (YOBS(I) .GE. 0.0000001) THEN
          EOBS(I) = MAX(SQRT(YOBS(I)), YOBS(I)/100.0)
        ENDIF
      ENDIF
! Skip negative 2-theta data
      IF (XOBS(I) .LE. 0.0) GOTO 10
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
      ESDsFilled = .TRUE.
! JvdS Added check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        GOTO 999
      ENDIF
! If wavelength present in file add in a test: if wavelength close to known anode material,
! set source to laboratory. Otherwise, source is synchrotron.
      IF (Lambda1 .GT. 0.01) THEN
! Initialise source material to synchrotron
        JRadOption = 2
        DO I = 2, 6
          IF (ABS(Lambda1 - FnWavelengthOfMenuOption(I)) .LT. 0.0003) JRadOption = 1
        ENDDO
        CALL Upload_Source
        CALL Set_Wavelength(Lambda1)
      ENDIF
      Load_xye_File = 0
 999  CLOSE(hFile)

      END FUNCTION Load_xye_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_x01_File(TheFileName, ESDsFilled)
!
! This function tries to load a *.x01 file (Bede ASCII powder pattern format).
!
! Note that this function should only be called from DiffractionFileLoad
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER, EXTERNAL :: GetNumOfColumns, StrFind
      REAL, EXTERNAL :: WavelengthOf
      CHARACTER*255 Cline, tString, tSubString
      INTEGER       I, NumOfBins, hFile, tLen
      LOGICAL       ReadWarning
      REAL          Lambda1
      REAL          cps

!DCC SCAN DATA:
!File: d:\data temp\stephan\tablet_1_0aa1.x01
!User: d1d1
!Specimen: tablet_1    Crystal:     Reflection: 0 0 4
!Wavelength: 1.54056 Å    Bragg angle: 34.5636 °    kV: 40.00    mA: 50.00
! paracetamol tablet
!Time: 15:15 Date : 1/30/2002
!Scanning axis: 2Theta Scan direction: increasing
!Scan range: 35.000000 deg
!
!Starting positions of motors: 
!          2Theta     5.000050 deg
!          Axis_1     0.000000 sec
!          Axis_3     0.000000 sec
!          Chi     0.000000 deg
!          Det_Z     1.250000 mm
!          Gonio_3     0.000000 deg
!          Omega     0.000000 deg
!          Optics_Rot     0.070000 deg
!          Optics_Z     0.000000 mm
!          Phi     0.000000 deg
!          X     -3.000000 mm
!          Y     -3.000000 mm
!          Z     0.000000 mm
!          Z1     4.600000 mm
!          Z3     0.000000 mm
!          ZCCC     4.720000 mm
!          Omega_relative     6.944294 deg
!Component axes and relative motions:
!            *Omega      1.000000
!          Omega-2Theta     0.000000 deg
!Component axes and relative motions:
!            *Omega      1.000000
!            *2Theta     2.000000
!          phi2theta     23.650000 deg
!Component axes and relative motions:
!            *Phi      1.000000
!            *2Theta     2.000000
!No. of points: 350     Counting time per point:    5.000000
!Position   Count
!     5.000050      36.600000
!     5.100050      36.000000
!     5.200050      35.200000
!     5.300050      35.200000
!     5.400050      33.000000
! <SNIP>
!     39.599950      149.200000
!     39.699950      157.800000
!     39.799950      155.400000
!     39.899950      159.800000
!     40.000000      180.600000

! Initialise to failure
      Load_x01_File = 1
      Lambda1       = WavelengthOf('Cu') ! According to Bede, this is a valid assumption
      NumOfBins     = -1
      ReadWarning   = .FALSE.
      cps = 1.0
      hFile = 10
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      I = 1
    9 READ(hFile,FMT='(A)',ERR=999,END=999) tString
      CALL StrUpperCase(tString)
      CALL StrClean(tString,tLen)
      IF (StrFind(tString,tLen,'WAVELENGTH:',11) .NE. 0) THEN
! Extract wavelength
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) Lambda1
      ENDIF
      IF (StrFind(tString,tLen,'COUNTING TIME PER POINT:',24) .NE. 0) THEN
! Extract expected number of points and counting time per point
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) NumOfBins
        NumOfBins = NumOfBins + 1 ! Don't ask me why
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        CALL GetSubString(tString,' ',tSubString)
        READ (tSubString,*,ERR=999) cps
      ENDIF
! Detect start of data
      IF (StrFind(tString,tLen,'POSITION C',10) .NE. 0) GOTO 10
      GOTO 9
 10   READ(UNIT=hFile,FMT='(A)',ERR=999,END=100) Cline
      READ(Cline,*,ERR=999,END=100) XOBS(I), YOBS(I)
! Skip negative 2-theta data and negative intensities
      IF (XOBS(I) .LE. 0.0) GOTO 10
      IF (YOBS(I) .LT. 0.0) GOTO 10
      YOBS(I) = YOBS(I) * cps
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
      IF (NumOfBins .NE. -1) THEN
        IF (NOBS .LT. NumOfBins) CALL WarningMessage('File contained less data points than expected.'//CHAR(13)// &
                                                     'Will continue with points actually read only.')
        IF (NOBS .GT. NumOfBins) THEN
          CALL WarningMessage('File contained more data points than expected.'//CHAR(13)// &
                              'Excess points will be ignored.')
          NOBS = NumOfBins
        ENDIF
      ENDIF
      ESDsFilled = .FALSE.
! JvdS Added check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        GOTO 999
      ENDIF
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
      CALL Set_Wavelength(Lambda1)
      Load_x01_File = 0
 999  CLOSE(hFile)

      END FUNCTION Load_x01_File
!
!*****************************************************************************
!
      SUBROUTINE TruncateData(TheMin2Theta, TheMax2Theta)
!
! This subroutine truncates data both at the start and at the end.
! Setting TheMin2Theta to 0.0 / TheMax2Theta to 90.0 effectively 
! restores beginning or end to original state
!
! OUTPUT : On exit, TheMin2Theta and TheMax2Theta contain the actual minimum and maximum
!
! Note: restores XOBS etc. from BackupXOBS etc., so can't be called _after_ background subtraction.
!
      USE VARIABLES

      IMPLICIT NONE

      REAL, INTENT (INOUT) :: TheMin2Theta
      REAL, INTENT (INOUT) :: TheMax2Theta

      INCLUDE 'params.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER I, Shift
      REAL    tReal

      IF (TheMin2Theta .GT. TheMax2Theta) THEN
        tReal = TheMax2Theta
        TheMax2Theta = TheMin2Theta
        TheMin2Theta = tReal
      ENDIF
! Find TheMax2Theta in BackupXOBS
      DO I = BackupNOBS, 1, -1
        IF (BackupXOBS(I) .LE. TheMax2Theta) EXIT
      ENDDO
      TheMax2Theta = BackupXOBS(I)
      NOBS = I
! Find TheMin2Theta in BackupXOBS
      DO I = 1, NOBS
        IF (BackupXOBS(I) .GE. TheMin2Theta) EXIT
      ENDDO
      TheMin2Theta = BackupXOBS(I)
      Shift = I - 1
      DO I = 1, NOBS
        XOBS(I) = BackupXOBS(I+Shift)
        YOBS(I) = BackupYOBS(I+Shift)
        EOBS(I) = BackupEOBS(I+Shift)
      ENDDO
      NOBS = NOBS - Shift
      CALL Rebin_Profile

      END SUBROUTINE TruncateData
!
!*****************************************************************************
!
      SUBROUTINE GetProfileLimits

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER I
      REAL ESD_SUM
 
      XPMIN = XBIN(1)
      XPMAX = XBIN(MAX(NBIN,1)) ! prevent down-flow when NBIN=0
      YPMIN = YOBIN(1)
      YPMAX = YOBIN(1)
      DO I = 2, NBIN
        YPMIN = MIN(YOBIN(I),YPMIN)
        YPMAX = MAX(YOBIN(I),YPMAX)
      ENDDO
      XPGMIN = XPMIN
      XPGMAX = XPMAX
      YPGMIN = YPMIN
      YPGMAX = YPMAX
      CALL UPLOAD_RANGE
      XPGMINOLD = XPMIN
      XPGMAXOLD = XPMAX
      YPGMINOLD = YPMIN
      YPGMAXOLD = YPMAX
      ! Calculate average ESD
      ESD_SUM = 0.0
      DO I = 1, NBIN
        ESD_SUM = ESD_SUM + EBIN(I)
      ENDDO
      AVGESD = ESD_SUM / NBIN

      END SUBROUTINE GetProfileLimits
!
!*****************************************************************************
!
      SUBROUTINE ProfileRead_TruncationWarning(filename, Npoints)

      USE WINTERACTER
      USE VARIABLES

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
      SUBROUTINE Rebin_Profile
!
! Rebins the profile
!
      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER I, J, JJ, IST
      REAL XADD, YOADD, EADD

      NBIN = (NOBS/LBIN)
      DO I = 1, NBIN
        IST = (I-1) * LBIN
        XADD  = 0.0
        YOADD = 0.0
        EADD  = 0.0
        DO J = 1, LBIN
          JJ = J + IST
          XADD  = XADD  + XOBS(JJ)
          YOADD = YOADD + YOBS(JJ)
          EADD  = EADD  + EOBS(JJ)**2
        ENDDO
        XBIN(I)  =  XADD/FLOAT(LBIN)
        YOBIN(I) = YOADD/FLOAT(LBIN)
        YCBIN(I) = 0.0
        YBBIN(I) = 0.0
        EBIN(I)  = SQRT(EADD) / FLOAT(LBIN)
      ENDDO
      DataSetChange = DataSetChange + 1
      CALL GetProfileLimits
      CALL Get_IPMaxMin 

      END SUBROUTINE Rebin_Profile
!
!*****************************************************************************
!
