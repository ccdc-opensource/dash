! This file contains routines to read powder diffraction files.

      SUBROUTINE ScrUpdateFileName
!
! This routine updates all occurrences of the filename, both
! on the status bar and in the wizard.
!
! JvdS 17 July 2001
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

! Note that FNAME is a global variable in VARIABLES, defined in PCDruid_Main.f90

! Remember current dialogue window
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page3)
      CALL WDialogPutString(IDF_PWa_DataFileName_String,FNAME)
      CALL WDialogSelect(IDD_PW_Page2)
      CALL WDialogPutString(IDF_PW_DataFileName_String,FNAME)
      CALL PopActiveWindowID
! Update the status bar at the bottom of the screen.
      STATBARSTR(1) = FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))

      END SUBROUTINE ScrUpdateFileName
!
!*****************************************************************************
!
      CHARACTER*1 FUNCTION ChrLowerCase(TheChar)

      IMPLICIT NONE

      CHARACTER*1, INTENT (IN   ) :: TheChar

      CHARACTER*26 lc, UC
      PARAMETER (lc = 'abcdefghijklmnopqrstuvwxyz')
      PARAMETER (UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      INTEGER POS

      ChrLowerCase = TheChar
      DO POS = 1, 26
        IF (UC(POS:POS) .EQ. TheChar) THEN
          ChrLowerCase = lc(POS:POS)
          RETURN
        ENDIF
      END DO

      END FUNCTION ChrLowerCase
!
!*****************************************************************************
!
      CHARACTER*1 FUNCTION ChrUpperCase(TheChar)

      IMPLICIT NONE

      CHARACTER*1, INTENT (IN   ) :: TheChar

      CHARACTER*26 lc, UC
      PARAMETER (lc = 'abcdefghijklmnopqrstuvwxyz')
      PARAMETER (UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      INTEGER POS

      ChrUpperCase = TheChar
      DO POS = 1, 26
        IF (lc(POS:POS) .EQ. TheChar) THEN
          ChrUpperCase = UC(POS:POS)
          RETURN
        ENDIF
      END DO

      END FUNCTION ChrUpperCase
!
!*****************************************************************************
!
      REAL FUNCTION WavelengthOf(TheAnodeMaterial)
!
! This function return the wavelength of an X-ray tube given the material the anode is made of.
!
! JvdS 29 July 2001
!
! INPUT   : TheAnodeMaterial = the anode material, e.g. 'Cu'
!
! RETURNS : The wavelength as provided by the International Centre for Diffraction Data
!           0.0 if material not recognised
!
      IMPLICIT NONE

      CHARACTER*2, INTENT (IN   ) :: TheAnodeMaterial ! Chemical symbol for anode material, e.g. 'Cu'

      CHARACTER*1 ChrUpperCase ! Function
      CHARACTER*1 ChrLowerCase ! Function
      CHARACTER*2 tAnodeMaterial ! To remove call by value / call by reference ambiguity

      tAnodeMaterial(1:1) = ChrUpperCase(TheAnodeMaterial(1:1))
      tAnodeMaterial(2:2) = ChrLowerCase(TheAnodeMaterial(2:2))
      SELECT CASE (tAnodeMaterial)
           CASE ('Cu')
             WavelengthOf = 1.54056
           CASE ('Mo')
             WavelengthOf = 0.70930
           CASE ('Co')
             WavelengthOf = 1.78897
           CASE ('Fe')
             WavelengthOf = 1.93604
           CASE ('Cr')
             WavelengthOf = 2.28970
           CASE DEFAULT
             WavelengthOf = 0.0
         END SELECT

      END FUNCTION  WavelengthOf
!
!*****************************************************************************
!
      INTEGER FUNCTION GetNumOfColumns(TheString)
!
! This function determines the number of columns in a string
! a column is a consecutive sequence of non-blank characters embedded in two blank-characters
!
! JvdS 27 July 2001
!
! INPUT   : TheString = the string
!
! RETURNS : Number of columns
!

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: TheString

      INTEGER POS
      INTEGER tNumOfColumns
      INTEGER StrLen

      StrLen = LEN_TRIM(TheString)
      tNumOfColumns = 0
      POS = 1
      DO WHILE (POS .LE. StrLen)
! Skip spaces
        DO WHILE ((POS .LE. StrLen) .AND. (TheString(POS:POS) .EQ. ' '))
          POS = POS + 1
        END DO
! If we hit a non-space: it's a column
        IF ((POS .LE. StrLen) .AND. (TheString(POS:POS) .NE. ' ')) tNumOfColumns = tNumOfColumns + 1
! Scan past rest of column (find next space / end of string)
        DO WHILE ((POS .LE. StrLen) .AND. (TheString(POS:POS) .NE. ' '))
          POS = POS + 1
        END DO
      END DO
      GetNumOfColumns = tNumOfColumns

      END FUNCTION GetNumOfColumns
!
!*****************************************************************************
!
      SUBROUTINE Diffraction_File_Browse(NoData)
!
! This routine lets the user browse a directory for a diffraction file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
! This way, all diffraction-file-opening is dealt with by a single routine.
!
! JvdS 18 July 2001
!
! OUTPUT  : NoData set to .FALSE. if data read in
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, INTENT (IN OUT) :: NoData

      CHARACTER(LEN=512) :: FILTER
      INTEGER       ::   IFLAGS
      INTEGER            IFTYPE    ! Needed for Winteracter routine
      CHARACTER(LEN=MaxPathLength) tFileName ! Temporary filename

      IFLAGS = LoadDialog + DirChange + PromptOn
! It seems that Winteracter cannot cope with strings of this length
!      FILTER = 'All files (*.*)|*.*|'//&
!               'All powder diffraction files (*.raw, *.rd, '//&
!               '*.sd, *.udf, *.uxd, *.xye)|*.raw;*.rd;*.sd;*.udf;*.uxd;*.xye|'//&
!               'Bruker powder diffraction files (*.raw, *.uxd)|*.raw;*.uxd|'//&
!               'DASH powder diffraction files (*.xye)|*.xye|'//&
!               'Philips powder diffraction files (*.rd, *.sd, *.udf)|*.rd;*.sd;*.udf|'
      FILTER = 'All files (*.*)|*.*|'//&
               'All powder diffraction files (*.raw, *.rd, '//&
               '*.sd, *.udf, *.uxd, *.xye)|*.raw;*.rd;*.sd;*.udf;*.uxd;*.xye|'//&
               'DASH powder diffraction files (*.xye)|*.xye|'
      tFileName = ' '
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open Powder Diffraction File',IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      CALL Diffraction_File_Open(tFileName,NoData)
      RETURN

      END SUBROUTINE Diffraction_File_Browse
!
!*****************************************************************************
!
      REAL FUNCTION Radians2Degrees(TheAngle)     

      REAL, INTENT (IN) :: TheAngle

      Radians2Degrees = TheAngle * (30.0 / ASIN(0.5))

      END FUNCTION Radians2Degrees
!
!*****************************************************************************
!
      REAL FUNCTION Degrees2Radians(TheAngle)     

      REAL, INTENT (IN) :: TheAngle

      Degrees2Radians = TheAngle * (ASIN(0.5) / 30.0)

      END FUNCTION Degrees2Radians
!
!*****************************************************************************
!
      SUBROUTINE Diffraction_File_Open(TheFileName,NoData)
!
! This routine tries to open a diffraction file.
!
! JvdS 18 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL,                      INTENT (IN OUT) :: NoData
      CHARACTER(LEN=MaxPathLength), INTENT (IN OUT) :: TheFileName

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC' ! Contains alambda, the wavelength

      INTEGER NBIN, LBIN
      REAL    XBIN, YOBIN, YCBIN, YBBIN, EBIN
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)

      LOGICAL            FExists
      INTEGER       ::   KLEN
! Note that FNAME is a global variable
      INTEGER            ISTAT
      INTEGER            Diffraction_File_Load ! Function
      LOGICAL            FnWavelengthOK ! Function
      REAL               UpperResolution
      REAL               tMax2Theta
      REAL               Radians2Degrees ! Function
      LOGICAL            Confirm ! Function

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
      END IF
!   If answer 'Yes'
! It is slightly odd that SAVEF is set to .FALSE. here. We are about to load the powder patttern:
! how much more necessary to save a project file can it get?
      SAVEF = .FALSE.
! This is the point of no return: the selected file will be new file, valid data or not
! Change global variable FNAME
      FNAME = TheFileName
! Update this throughout the program (Wizard + status bar)
      CALL ScrUpdateFileName
      ISTAT = Diffraction_File_Load(TheFileName,NoData)
      IF (ISTAT .EQ. 0) RETURN
! A pattern has been loaded, and the background may have been subtracted. This is the rigth time
! to truncate the data to, say, 1.5 A resolution.
! It's a good idea to subtract the background before truncating: that way, after the SA, the
! whole pattern can be Rietveld-refined without changing the background.
! Check if the wavelength is available.
      UpperResolution = 1.5 ! d-value in Angstrom
      IF (FnWavelengthOK() .AND. (ALambda .LT. 2.5)) THEN
        tMax2Theta = 2 * Radians2Degrees(ASIN(ALambda/(2*UpperResolution)))
        IF (XBIN(NBIN) .GT. tMax2Theta) THEN
          IF (Confirm('Would you like to truncate the data '//&
                      'to 1.5 A resolution (recommended)?')) CALL TruncateData(tMax2Theta)
        ENDIF
      ENDIF
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
      DashRawFile = FNAME
      RETURN

      END SUBROUTINE Diffraction_File_Open
!
!*****************************************************************************
!
      INTEGER FUNCTION Diffraction_File_Load(TheFileName,NoData)
!
! This routine tries to load a diffraction file.
!
! JvdS 18 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

!      IMPLICIT NONE

      LOGICAL,                      INTENT (IN OUT) :: NoData
      CHARACTER(LEN=MaxPathLength), INTENT (IN OUT) :: TheFileName

      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)

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

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      INTEGER          KLEN
      CHARACTER(LEN=4) EXT4
      INTEGER          Load_raw_File ! Function
      INTEGER          Load_rd_File  ! Function
      INTEGER          Load_udf_File ! Function
      INTEGER          Load_uxd_File ! Function
      INTEGER          Load_xye_File ! Function
      INTEGER          ISTAT
      INTEGER          I, J, JJ
      INTEGER          POS
      LOGICAL          ESDsFilled

      BACKREF = .TRUE.
      Diffraction_File_Load = 0
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
      Diffraction_File_Load = ISTAT
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
!C>>JCC Set the default SA output files to <fname>.cssr etc (fname gets any extension removed)
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
        END DO
      ENDIF
      DataSetChange = DataSetChange + 1
      NumPawleyRef = 0
      NumObsTic = 0
!      NTic=0
      CurrentRange = 0
      NumPeakFitRange = 0
      DO I = 1, MAX_NPFR
        NumInPFR(I) = 0
        IPF_RPt(I) = 0
      END DO
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
          YPMAX=YOBS(I)
        END IF
      END DO
      INTEGRATED_GUESS = 0
      DO I = MAX(1,MAX_INTENSITY_INDEX - 5), MIN(NOBS,MAX_INTENSITY_INDEX + 5)
        INTEGRATED_GUESS = INTEGRATED_GUESS + YOBS(I)
      END DO
      IF (INTEGRATED_GUESS .GT. 250000) THEN
        SCALFAC = 0.01 * INTEGRATED_GUESS/250000
      ELSE IF (YPMAX .GT. 100000) THEN
        SCALFAC = 0.01 * YPMAX/100000
      END IF
      NBIN = (NOBS/LBIN)
      DO I = 1, NBIN
        IST = (I-1)*LBIN
        XADD  = 0.0
        YOADD = 0.0
        VADD  = 0.0
        DO J = 1, LBIN
          JJ = J + IST
          XADD  = XADD+XOBS(JJ)
          YOADD = YOADD+YOBS(JJ)
          VADD  = VADD+EOBS(JJ)**2
        END DO
        XBIN(I)  = XADD/FLOAT(LBIN)
        YOBIN(I) = YOADD/FLOAT(LBIN)
        YCBIN(I) = YOBIN(I)
        EBIN(I)  = SQRT(VADD)/FLOAT(LBIN)
! JvdS Assume no knowledge on background
        YBBIN(I) = 0.0
      END DO
      XPGMIN = XPMIN
      XPGMAX = XPMAX
      YPGMIN = YPMIN
      YPGMAX = YPMAX
      XPGMINOLD = XPMIN
      XPGMAXOLD = XPMAX
      YPGMINOLD = YPMIN
      YPGMAXOLD = YPMAX
      CALL UPLOAD_RANGE()
      IPMIN = 1
      IPMAX = NBIN
      IPMINOLD = IPMIN
      IPMAXOLD = IPMAX
      IPTYPE = 1
      CALL Profile_Plot(IPTYPE)
      NoData = .FALSE.
      CALL ScrUpdateFileName
      CALL Background_Fit

      END FUNCTION Diffraction_File_Load
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_raw_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.raw file (binary format from Bruker machines).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from Diffraction_File_Load
!
! JvdS 23 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!         : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=MaxPathLength), INTENT (IN)  :: TheFileName
      LOGICAL,                      INTENT (OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      INCLUDE 'statlog.inc'

      INTEGER     I, Shift, FLEN ! Length of TheFileName
      REAL*8      TwoThetaStart, TwoThetaStep, CurrTwoTheta
      REAL        Lambda1
      INTEGER*4   I4, NumOfBins
      INTEGER*4   I4_2
      REAL*4      R4
      INTEGER*2   I2(1:4)
      INTEGER*1   I1(1:8)
      EQUIVALENCE (I1(1),I2(1),I4,R4)
      EQUIVALENCE (I2(3),I4_2)
      REAL*8      R8
      INTEGER*4   RecReal(1:2)
      EQUIVALENCE (R8,RecReal(1))  ! This way we can read a REAL*8 from a file with record length 4
      CHARACTER*4 C4
      INTEGER*4   NumOfDataRanges  ! 1 data range = 1 powder pattern
      INTEGER*4   Offset, CurrDataRange
      INTEGER*2   SizeOfHeader

! Current status, initialise to 'error'
      Load_raw_File = 0
      TwoThetaStart = 0.0
      TwoThetaStep  = 0.0
      FLEN = LEN_TRIM(TheFileName)
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
! First check the version, which follows from the third byte in the file. Bytes 1-3 are: RAW
! "RAW " = version 1. The oldest version, requested by Bruker not the be supported any more.
! "RAW2" = version 2.
! "RAW1" = version 3. The most recent version.
      READ(UNIT=10,REC=1,ERR=999) C4
      IF (C4(1:3) .NE. 'RAW') THEN
        CALL ErrorMessage("Not a valid Bruker .raw file.")
        RETURN
      ENDIF
      SELECT CASE (C4(4:4))
        CASE (' ')
! Warn the user
          CALL ErrorMessage("This version of the .raw format is no longer supported."//&
                           CHAR(13)//"Please convert to a newer version and try again.")
          RETURN
        CASE ('1')
! A version 3 file, the most recent and most complicated format
! 'current file status'. 1 = done, 2 = active, 3 = aborted, 4 = interrupted
          READ(UNIT=10,REC=3,ERR=999) I4
          IF (I4 .NE. 1) THEN
! The user should be warned here
            CALL ErrorMessage("Current file status is Active, Aborted or Interrupted.")
            RETURN
          ENDIF
! Number of completed data ranges. I assume that that is the number of powder patterns in this file
! (multiple data sets are possible in this file format)
          READ(UNIT=10,REC=4,ERR=999) NumOfDataRanges

! We can read the primary wavelength (Angstroms) here, but it is also specified per data range
! Assuming a monochromated beam, this is the wavelength we would want
!          READ(UNIT=10,REC=157,ERR=999) RecReal(1)
!          READ(UNIT=10,REC=158,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the wavelength in Angstroms
! The complete file header is 712 bytes, so start reading at record (712 DIV 4) + RecNumber
          Offset = 178
! @ Now there should be a loop over the number of data ranges
          DO CurrDataRange = 1, NumOfDataRanges
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
! Length of Range Header Structure in bytes. Must be 304.
            READ(UNIT=10,REC=Offset+1,ERR=999) I4
            IF (I4 .NE. 304) THEN
! The user should be warned here
              CALL ErrorMessage("Length of Range Header Structure must be 304.")
              RETURN
            ENDIF
! Number of 'data records'
            READ(UNIT=10,REC=Offset+2,ERR=999) NumOfBins
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
              RETURN
            ENDIF
! Starting angle for 2 theta drive in degrees
            READ(UNIT=10,REC=Offset+5,ERR=999) RecReal(1)
            READ(UNIT=10,REC=Offset+6,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the starting angle in degrees
            TwoThetaStart = R8
! Read scan mode: 0 = step, 1 = continuous
 !           READ(UNIT=10,REC=Offset+43,ERR=999) I4
! Step size in degrees
            READ(UNIT=10,REC=Offset+45,ERR=999) RecReal(1)
            READ(UNIT=10,REC=Offset+46,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the step size in degrees
            TwoThetaStep = R8
            IF (TwoThetaStep .LT. 0.000001) RETURN
! Next REAL*8 contains primary wavelength (Angstroms) for this data range
! Assuming a monochromated beam, this is the wavelength we would want
            READ(UNIT=10,REC=Offset+61,ERR=999) RecReal(1)
            READ(UNIT=10,REC=Offset+62,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the wavelength in Angstroms
! And now store this value as the experimental wavelength
            Lambda1 = R8
            CALL UpdateWavelength(Lambda1)
! Data record length. Must be 4.
            READ(UNIT=10,REC=Offset+64,ERR=999) I4
            IF (I4 .NE. 4) THEN
! The user should be warned here
              CALL ErrorMessage("Record length must be 4.")
              RETURN
            ENDIF
! Length of supplementary header (bytes)
            READ(UNIT=10,REC=Offset+65,ERR=999) I4
! Check if length of supplementary header is a multiple of four.
            IF (MOD(I4,4) .NE. 0) THEN
! The user should be warned here
              CALL ErrorMessage("Length of Supplementary Header is not a multiple of 4.")
              RETURN
            ENDIF
! Skip all supplementary headers of the current data range
            Offset = Offset + I4 / 4
! Skip the header of the current data range
            Offset = Offset + 76
! The data begin from here as REAL*4 records
! Fill the 2 theta values first
            CurrTwoTheta = TwoThetaStart
            DO I = 1, NumOfBins
              XOBS(I) = CurrTwoTheta
              CurrTwoTheta = CurrTwoTheta + TwoThetaStep
            ENDDO
            DO I = 1, NumOfBins
              READ(UNIT=10,REC=Offset+I,ERR=999) R4
              YOBS(I) = R4
            END DO
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
          END DO
        CASE ('2')
! A version 2 file.
! Next two bytes are the number of data ranges
          READ(UNIT=10,REC=2,ERR=999) I4
! Due to the EQUIVALENCE statement, I2(1) holds the first two bytes of I4.
! Because of 'backwords storage', this should work.
          NumOfDataRanges = I2(1)
! The next thing we need from the file header is Lambda 1
! Unfortunately, mean while one data item had a length of two bytes.
! Due to some EQUIVALENCE statements, this can be read as follows:
          READ(UNIT=10,REC=48,ERR=999) I4
          READ(UNIT=10,REC=49,ERR=999) I4_2
! Shift by two bytes
          I2(1) = I2(2)
          I2(2) = I2(3)
! R4 now holds the wavelength
          Lambda1 = R4
          CALL UpdateWavelength(Lambda1)
! The complete file header is 256 bytes, so start reading at record (256 DIV 4) + RecNumber
          Offset = 64
! @ Now there should be a loop over the number of data ranges
          DO CurrDataRange = 1, NumOfDataRanges
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
! The next two bytes are the length of the Range Header in bytes.
            READ(UNIT=10,REC=Offset+1,ERR=999) I4
! Due to EQUIVALENCE, I2(1) holds the header length
            SizeOfHeader = I2(1)
! Due to EQUIVALENCE, I2(2) holds the number of 'data records'
            NumOfBins = I2(2)
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
              RETURN
            ENDIF
! The step size. A REAL*4 in this format
            READ(UNIT=10,REC=Offset+4,ERR=999) R4
            TwoThetaStep = R4
            IF (TwoThetaStep .LT. 0.000001) RETURN
! Six times the start points of the range
! @@@@ Which one is 2 theta ?????
            READ(UNIT=10,REC=Offset+6,ERR=999) R4
            TwoThetaStart = R4
! Fill the 2 theta values
            CurrTwoTheta = TwoThetaStart
            DO I = 1, NumOfBins
              XOBS(I) = CurrTwoTheta
              CurrTwoTheta = CurrTwoTheta + TwoThetaStep
            END DO
! Now we can start reading the raw data. The complete header can consist of any number
! of bytes, and we can only read per four. The data is in REAL*4 format. This may require some
! shifting of the bytes read in. It is actually possible that the last datapoint cannot be read.
            IF (MOD(SizeOfHeader,4) .NE. 0) THEN
              NumOfBins = NumOfBins - 1
              OffSet = Offset + 1 + (SizeOfHeader / 4)
! Due to the way integer division in FORTRAN works, 
! the fractional part of SizeOfHeader / 4 is discarded
              Shift = SizeOfHeader - MOD(SizeOfHeader,4)
              READ(UNIT=10,REC=Offset,ERR=999) I4
              DO I = 1, NumOfBins
                READ(UNIT=10,REC=Offset+I,ERR=999) I4_2
                I1(1) = I1(1+Shift)
                I1(2) = I1(2+Shift)
                I1(3) = I1(3+Shift)
                I1(4) = I1(4+Shift)
                YOBS(I) = R4
                I1(1) = I1(1+(4-Shift))
                I1(2) = I1(2+(4-Shift))
                I1(3) = I1(3+(4-Shift))
                I1(4) = I1(4+(4-Shift))
              END DO
            ELSE
! When we are here, the size of the header was a multiple of 4 and we can read the data easily
              OffSet = Offset + SizeOfHeader / 4
              DO I = 1, NumOfBins
                READ(UNIT=10,REC=Offset+I,ERR=999) R4
                YOBS(I) = R4
              END DO
            ENDIF
          END DO
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
        CASE DEFAULT
          CALL ErrorMessage('Unrecognised *.raw format.')
          RETURN
      END SELECT
      CLOSE(10)
      NOBS = NumOfBins
      ESDsFilled = .FALSE.
! This is definitely laboratory data
      CALL SetSourceDataState(1)
      Load_raw_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      RETURN

      END FUNCTION Load_raw_File
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
! Note that this function should only be called from Diffraction_File_Load
!
! JvdS 25 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!         : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=MaxPathLength), INTENT (IN)  :: TheFileName
      LOGICAL,                      INTENT (OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      INCLUDE 'statlog.inc'

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
        RETURN
      ENDIF
      SELECT CASE (C4(3:4))
        CASE ('DI')
          CALL ErrorMessage("This file contains peak positions, not a powder pattern.")
          RETURN
        CASE ('BK')
          CALL ErrorMessage("This file contains a background, not a powder pattern.")
          RETURN
        CASE ('2D')
          CALL ErrorMessage("This file contains a second derivative, not a powder pattern.")
          RETURN
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
      IF (TwoThetaStart .LT. 0.000001) RETURN
      IF (TwoThetaEnd   .LT. 0.000001) RETURN
      IF (TwoThetaStep  .LT. 0.000001) RETURN
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
        RETURN
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
      CALL SetSourceDataState(1)
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the Anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
      CALL UpdateWavelength(Lambda1)
      Load_rd_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      RETURN

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
! Note that this function should only be called from Diffraction_File_Load
!
! JvdS 25 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!         : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=MaxPathLength), INTENT (IN)  :: TheFileName
      LOGICAL,                      INTENT (OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      INCLUDE 'statlog.inc'

      CHARACTER*255 Cline ! String containing last line read from file
      INTEGER       J, NumOfBins, CurrLineNr, FLEN ! Length of TheFileName
      INTEGER       NumOfLines
      REAL          Lambda1
      INTEGER*4     TempInput(1:8)
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

! Read the header lines
   9  READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
      IF (Cline(1:5) .EQ. 'Anode') THEN ! Anode,Cu,/
        READ(Cline,FMT='(6X,A2)',ERR=999) Anode
        GOTO 9
      ENDIF
      IF (Cline(1:11) .EQ. 'LabdaAlpha1') THEN ! LabdaAlpha1, 1.54060,/
        READ(Cline,FMT='(12X,F8.5)',ERR=999) Lambda1
        GOTO 9
      ENDIF
      IF (Cline(1:14) .EQ. 'DataAngleRange') THEN ! DataAngleRange,   3.0100,  89.9900,/
        READ(Cline,FMT='(15X,F9.4,1X,F9.4)',ERR=999) TwoThetaStart, TwoThetaEnd
        GOTO 9
      ENDIF
      IF (Cline(1:12) .EQ. 'ScanStepSize') THEN ! ScanStepSize,   0.020,/
        READ(Cline,FMT='(13X,F8.3)',ERR=999) TwoThetaStep
        GOTO 9
      ENDIF
      IF (Cline(1:7) .EQ. 'RawScan') GOTO 10
      GOTO 9
! Here is where we can start to read the data
 10   CONTINUE
! The number of counts per 2theta should commence from here.
! Calculate how many bins we expect.
! Quick check if values have been read at all.
      IF (TwoThetaStart .LT. 0.000001) RETURN
      IF (TwoThetaEnd   .LT. 0.000001) RETURN
      IF (TwoThetaStep  .LT. 0.000001) RETURN
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
        RETURN
      ENDIF
! Integer division: fractional part is discarded. NumOfLines contains the number of
! lines containing 8 columns
      NumOfLines = NumOfBins / 8
! Fill the 2theta values first
      CurrTwoTheta = TwoThetaStart
      DO CurrLineNr = 1, NumOfBins
        XOBS(CurrLineNr) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + TwoThetaStep
      ENDDO
      DO CurrLineNr = 1, NumOfLines
        READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
        READ(Cline,FMT='(7(I9,1X),I9)',ERR=999) TempInput
! Next couple of lines rather clumsy, but safe.
        DO J = 1, 8
          YOBS(((CurrLineNr - 1) * 8) + J) = FLOAT(TempInput(J))
        ENDDO
      ENDDO
! The loop could take care of lines containing 8 columns,
! there might be 1 line left containing less than 8 columns
      READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
! The following line worked perfectly for a week, then suddenly gave a fatal error ????
!      READ(Cline,FMT='(I9,1X)',ERR=999) TempInput(1:(NumOfBins - (NumOfLines * 8)))
      READ(Cline,FMT='(7(I9,1X),I9)',ERR=999) TempInput(1:(NumOfBins - (NumOfLines * 8)))
! Next couple of lines rather clumsy, but safe.
      CurrLineNr = NumOfLines + 1
      DO J = 1, (NumOfBins - (NumOfLines * 8))
        YOBS(((CurrLineNr - 1) * 8) + J) = FLOAT(TempInput(J))
      ENDDO
      CLOSE(10)
      NOBS = NumOfBins
      ESDsFilled = .FALSE.
! This is definitely laboratory data
      CALL SetSourceDataState(1)
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the Anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
      CALL UpdateWavelength(Lambda1)
      Load_udf_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      RETURN

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
! Note that this function should only be called from Diffraction_File_Load
!
! JvdS 25 July 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!         : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=MaxPathLength), INTENT (IN)  :: TheFileName
      LOGICAL,                      INTENT (OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      INCLUDE 'statlog.inc'

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
      PARAMETER     (COUNTS = 1, THETACOUNTS = 2, THETACOUNTSTIME = 3)
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
      END DO
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
          END DO
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
                END DO
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
  11           DO POS = 1, StrLen
                 IF (Cline(POS:POS) .EQ. ',') Cline(POS:POS) = ' '
               END DO
! Next, we have to determine how many columns to read from the next line.
! There are three conditions:
! 1. the maximum number of columns as requested by the user (MaxNumColumns)
! 2. the number of columns actually present in the string (GetNumOfColumns)
! 3. the number of data point still to be read (MaxNumOfBins - I)
               NumOfColumns2Read = MIN(MaxNumOfColumns, GetNumOfColumns(Cline))
               NumOfColumns2Read = MIN(NumOfColumns2Read, MaxNumOfBins - I)
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
      CALL SetSourceDataState(1)
! Try to set the wavelength
! If the wavelength was not present in the file, try to interpret the anode material
      IF (Lambda1 .LT. 0.00001) Lambda1 = WavelengthOf(Anode)
      CALL UpdateWavelength(Lambda1)
      Load_uxd_File = 1
      RETURN
 999  CONTINUE
! Exit code is error by default, so we can simply return
      RETURN

      END FUNCTION Load_uxd_File
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_xye_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.xye file (standard DASH ASCII powder pattern format).
!
! Note that this function should only be called from Diffraction_File_Load
!
! JCC
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : NoData set to .FALSE. if data read in
!         : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 1 for success
!           0 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=MaxPathLength), INTENT (IN)  :: TheFileName
      LOGICAL,                      INTENT (OUT) :: ESDsFilled

      INCLUDE 'PARAMS.INC'

      INTEGER NOBS
      REAL    XOBS, YOBS, YCAL, YBAK, EOBS
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)

      CHARACTER*255 Cline
      INTEGER       I, IS, FLEN ! Length of TheFileName
      LOGICAL       ReadWarning
      INTEGER       GetNumOfColumns ! Function
      REAL          Lambda1

! Initialise to failure
      Load_xye_File = 0
      ReadWarning   = .FALSE.
      FLEN = LEN_TRIM(TheFileName)
      OPEN(UNIT=10,FILE=TheFileName(1:FLEN),STATUS='OLD',err=999)
      I = 1
! Check if wavelength available on very first line
      READ(UNIT=10,FMT='(A)',ERR=999,END=999) Cline
      IF (GetNumOfColumns(Cline) .EQ. 1) THEN
        READ(Cline,*,ERR=999,END=999) Lambda1
        CALL UpdateWavelength(Lambda1)
      ELSE
        READ(Cline,*, IOSTAT = IS) XOBS(I),YOBS(I),EOBS(I)
        IF (IS .NE. 0) THEN
          READ(Cline,*, ERR=999,END=999) XOBS(I),YOBS(I)
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
        I = I + 1
      ENDIF
!C>> Modified to handle files without esds - used to read in YOBS as the esd
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
            CALL ErrorMessage("Warning: The data file contains multiple observations for the same "//&
                              "2-theta"//CHAR(13)//"Only the first observation"//&
                              "will be used")
          END IF
          GOTO 10
        END IF
      END IF
      I = I + 1
!>> JCC Only read in a maximum of MOBS points
      IF (I .GT. MOBS) THEN
        CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
        GOTO 100
      END IF
      GOTO 10
 100  NOBS = I - 1
      CLOSE(10)
      ESDsFilled = .TRUE.
! JvdS Added check for number of observations = 0
      IF (NOBS .EQ. 0) THEN
        CALL ErrorMessage("The file contains no valid data.")
        RETURN
      ENDIF
      Load_xye_File = 1
      RETURN
 999  CONTINUE
      RETURN

      END FUNCTION Load_xye_File
!
!*****************************************************************************
!
    SUBROUTINE SDI_file_Open(NoData)
!
!   This subroutine processes Open selection
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, INTENT (IN OUT) :: NoData

      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS

      INCLUDE 'GLBVAR.INC'

      LOGICAL FExists
      INTEGER Iflen
      LOGICAL Confirm ! Function

! Check if file needs saving
      IF (SAVEF) THEN
        IF (.NOT. Confirm('Program contains an unsaved project.'//CHAR(13)//'Do you wish to '// &
          'continue?')) RETURN
      END IF
! If answer 'Yes'
      SAVEF = .FALSE.
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'DASH Pawley files (*.sdi)|*.sdi|'
! JvdS The next line resets the global variable to 'no file'
! This is wrong if the user presses 'cancel' in the open file window.
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open DASH pawley file')
      IFlen = LEN_TRIM(FNAME)
      IF (IFlen .EQ. 0) RETURN
      INQUIRE(FILE=FNAME(1:IFlen),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//FNAME(1:IFlen)//" does not exist!")
        RETURN
      ENDIF
      CALL OpenHCVPIKTIC(FNAME(1:IFlen),NoData) 
      IF (NODATA) THEN
        CALL ErrorMessage("Could not read the project file "//FNAME(1:IFlen)//&
                          CHAR(13)//"successfully.")
        RETURN
      END IF
      STATBARSTR(1)=FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
! Enable all menu functions
      CALL SetModeMenuState(1,1,1)
      RETURN

      END SUBROUTINE SDI_file_Open
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_TIC_File(FLEN,TheFileName)

      CHARACTER(LEN=256),  INTENT (IN) :: TheFileName
      INTEGER,             INTENT (IN) :: FLEN

      INCLUDE 'PARAMS.INC'
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      INTEGER I, II

!>> JCC - set return status
      Load_TIC_File = 1
!>> JCC - add in an error trap for bad file opening
      OPEN(11,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
      I=1
 10   READ(11,*,ERR=100,END=100) (IH(II,I),II=1,3),ARGK(I),DSTAR(I)
      I=I+1
      GOTO 10
 100  NTIC=I-1
      CLOSE(11)
!
!      CALL View_Tic_File(FLEN,TheFileName)
!
      RETURN
 999  Load_TIC_File = 0
      RETURN
      END FUNCTION Load_TIC_File
!
!*****************************************************************************
!
!U      INTEGER FUNCTION Load_CCL_File(FLEN,TheFileName)
!U!
!U      CHARACTER(LEN=256),           INTENT (IN) :: TheFileName
!U      INTEGER,                      INTENT (IN) :: FLEN
!U      CHARACTER(LEN=80) CCL_LINE
!U      INCLUDE 'GLBVAR.INC' ! Contains ALambda
!U      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT
!U      INTEGER I
!U      REAL  WaveLengthOf ! Function
!U!
!U!>> JCC Initialise return value
!U!
!U      Load_CCL_File = 1
!U      ZEROPOINT = 0.0
!U      ALambda = WaveLengthOf('Cu')
!U!
!U!>> JCC Add in Error trap
!U!                          
!U      OPEN(11,FILE=TheFileName(:FLEN),STATUS='OLD', ERR = 999)
!U!>> JCC Set SA Output files
!U      CALL sa_SetOutputFiles(TheFileName)
!U   10 READ(11,5000,ERR=100,END=100) NLCCL,CCL_LINE
!U 5000 FORMAT(Q,A)
!U      IF (CCL_LINE(1:1) .EQ. 'C') THEN
!U        READ(CCL_LINE(2:NLCCL),*) (CELLPAR(I),I=1,6)
!U      ELSE IF (CCL_LINE(1:1) .EQ. 'L') THEN
!U        IF (CCL_LINE(3:6) .EQ. 'WVLN') THEN
!U          READ(CCL_LINE(7:NLCCL),*) ALambda
!U        ELSE IF (CCL_LINE(3:6) .EQ. 'ZERO') THEN
!U          READ(CCL_LINE(7:NLCCL),*) zeropoint
!U        END IF
!U      END IF
!U      GOTO 10
!U  100 CLOSE(11)
!U!
!U      CALL UpLoad_Crystal_Data()
!U!
!U!>> JCC Added in next few lines
!U      RETURN
!U 999  Load_CCL_File = 0
!U      RETURN
!U      END FUNCTION Load_CCL_File
!U
!*****************************************************************************
!
!>> JCC  Routine to truncate data to a particular data range
!>> In practice, this varies the NOBS value so the data is not lost, just
!>> Hidden. The routine should not be called with RMaxTTheta greater
!>> than the maximum value read in
! JvdS And how about discarding data from the beginning of the diffraction pattern
! (when varibale slits have been used, for example).
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

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      REAL RMaxTTheta
      INTEGER I, J, JJ

      DO I = 1,NOBS
        IF (XOBS(I) .GT. RMaxTTheta) EXIT
      END DO
      IF (I .LE. 1) RETURN
      NOBS = I - 1
      NBIN=(NOBS/LBIN)
      DO I = 1, NBIN
        IST=(I-1)*LBIN
        XADD=0.
        YOADD=0.
        YCADD=0.
        YBADD=0.
        VADD=0.
        DO J = 1, LBIN
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
      CALL Profile_Plot(IPTYPE)

      END SUBROUTINE TruncateData
!
!*****************************************************************************
!
      SUBROUTINE ProfileRead_TruncationWarning(filename, Npoints)
      USE VARIABLES
      USE WINTERACTER
      USE DRUID_HEADER
      CHARACTER *(*) filename
      CHARACTER *7   cmobs
      INTEGER        len_filename

      len_filename = LEN_TRIM(filename)
      WRITE(cmobs,'(i7)' ) Npoints
      CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
        " The file "//filename(1:len_filename)//" contains greater than "//cmobs(1:LEN_TRIM(cmobs))//char(13)//&
        " data points. Only the first "//cmobs(1:LEN_TRIM(cmobs))//" points were read",&
        "Data truncation on read in")

      END SUBROUTINE ProfileRead_TruncationWarning
!
!*****************************************************************************
!
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

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      INCLUDE 'statlog.inc'

      INTEGER I, IT, J, IBpass
      LOGICAL QUIT

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

! JvdS When called from within the Wizard, this entire routine should just be a step
! JvdS within the Wizard. It's quite stand-alone now.

! Remember current dialogue window
      CALL PushActiveWindowID
! Grey out all other menus
      CALL ToggleMenus(0)
! The reason behind this greying out seems to be that the next dialogue
! window cannot be made modal because it needs to draw the calculated background 
! to the main screen. And the user should be able to zoom in on parts of the graph
! to decide whether or not to accept the background.
      CALL WDialogSelect(ID_Background_Fit)
! Initialise the background
      CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
      CALL BackFit(IBpass)
      CALL Profile_Plot(IPTYPE)
      CALL WDialogShow(-1,-1,0,Modeless)
! JvdS The next line queries the current status, but is it handled properly?
      IT = InfoError(1)
      QUIT = .FALSE.
      DO WHILE(.NOT. QUIT)
        CALL GetEvent
        IF (EventInfo%WIN .EQ. 0) THEN
          CALL process_mainwindow_message
        ELSE IF (EventType .EQ. PushButton) THEN
! Process it
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Background_Apply)
              CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
              CALL BackFit(IBpass)
              CALL Profile_Plot(IPTYPE)
            CASE (IDF_Background_Accept)
! Subtract the background
              IOBS = 0
              YPMIN = YOBS(1) - YBBIN(1)
              YPMAX = YPMIN
              DO I = 1, NBIN
                DO J = 1, LBIN
                  IOBS = IOBS + 1
                  YOBS(IOBS) = YOBS(IOBS) - YBBIN(I)
                  YPMIN = MIN(YOBS(IOBS),YPMIN)
                  YPMAX = MAX(YOBS(IOBS),YPMAX)
                END DO
                YOBIN(I) = YOBIN(I) - YBBIN(I)
                YBBIN(I) = 0.0
              END DO
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
              QUIT = .TRUE. 
              BACKREF = .FALSE.
            CASE (IDCANCEL)
! If user Cancels, assume no knowledge on background
              DO I = 1, NBIN
                YBBIN(I) = 0.0
              END DO
              QUIT = .TRUE.
          END SELECT
        END IF
      END DO
      CALL WDialogSelect(ID_Background_Fit)
      CALL WDialogHide()
      CALL Profile_Plot(IPTYPE)
! JvdS I think the following line introduces a bug.
! Assume we've just loaded a .xye file through the wizard (that's why this routine was called).
! It is now possible to load another .xye file through the icon on the main menu.
! That causes some states to be defined twice, and the wizard window locks up.
      CALL ToggleMenus(1)
      CALL PopActiveWindowID

      END SUBROUTINE Background_Fit
