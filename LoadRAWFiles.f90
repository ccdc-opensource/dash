!
!*****************************************************************************
!
      INTEGER FUNCTION Load_raw_File(TheFileName,ESDsFilled)
!
! This function tries to load a *.raw file (binary format from either STOE or Bruker machines).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from DiffractionFileLoad
!
! JvdS Oct 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : ESDsFilled set to .TRUE. if the file contained ESDs, .FALSE. otherwise
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (  OUT) :: ESDsFilled

      INCLUDE 'GLBVAR.INC'

      CHARACTER*4 C4
      INTEGER iRange, CurrRange
      INTEGER Load_rawSTOE_File, Load_rawBruker_File ! Functions
      INTEGER tNumOfRanges
      CHARACTER*80 TitleOfRange(250) ! 8 = maximum number of ranges in a STOE file
      CHARACTER*20, EXTERNAL :: Integer2String
      REAL    t2ThetaStep(250)
      INTEGER iHighlightList(250)
      LOGICAL LoadRange(250), AtLeastOneSelected
      REAL    Smallest2ThetaStep
      INTEGER I, hFile

! Current status, initialise to 'error'
      Load_raw_File = 1
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=TheFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
! Check if true RAW file, and which type
      READ(UNIT=hFile,REC=1,ERR=999) C4
      CLOSE(hFile)
      IF (C4(1:3) .NE. 'RAW') THEN
        CALL ErrorMessage('Not a valid STOE or Bruker .raw file.')
        RETURN
      ENDIF
! First check the version, which follows from the fourth byte in the file. Bytes 1-3 are: RAW
! "RAW_" = STOE
! "RAW " = Bruker version 1. The oldest version, requested by Bruker not the be supported any more.
! "RAW2" = Bruker version 2.
! "RAW1" = Bruker version 3. The most recent version.
      SELECT CASE (C4(4:4))
        CASE ('_')
! STOE files can contain multiple data ranges, up to a maximum of eight.
! Scan the file, get the number of data ranges and their titles.
          CALL GetDataRangesSTOE(TheFileName,tNumOfRanges,TitleOfRange)
          IF (tNumOfRanges .EQ. 0) THEN
            CALL ErrorMessage('File contains no data.')
            RETURN
          ENDIF
          IF (tNumOfRanges .EQ. 1) THEN
            iRange = 1
          ELSE
            CALL PushActiveWindowID
            CALL WDialogLoad(IDD_DataRangeSTOE)
            CALL WDialogSelect(IDD_DataRangeSTOE)
            CALL WDialogPutMenu(IDF_DataRangeMenu,TitleOfRange,tNumOfRanges,1)
            CALL WDialogShow(-1,-1,IDOK,Modal)
            IF (WInfoDialog(ExitButton) .NE. IDOK) THEN
              Load_raw_File = 2
              CALL WDialogUnload
              CALL PopActiveWindowID
              RETURN
            ENDIF
            CALL WDialogGetMenu(IDF_DataRangeMenu,iRange)
! irange is the data range to be read.
            CALL WDialogUnload
            CALL PopActiveWindowID
          ENDIF
          Load_raw_File = Load_rawSTOE_File(TheFileName,iRange)
          ESDsFilled = .FALSE.
          IF (Load_raw_File .NE. 1) RETURN
        CASE (' ','1','2')
! Bruker files can contain multiple data ranges.
! Scan the file, get the number of data ranges and their titles.
          CALL GetDataRangesBruker(TheFileName,tNumOfRanges,TitleOfRange,t2ThetaStep)
          IF (tNumOfRanges .EQ. 0) THEN
            CALL ErrorMessage('File contains no data.')
            RETURN
          ENDIF
          IF (tNumOfRanges .EQ. 1) THEN
            LoadRange(1) = .TRUE.
          ELSE
            CALL PushActiveWindowID
            CALL WDialogLoad(IDD_DataRangeBruker)
            CALL WDialogSelect(IDD_DataRangeBruker)
            DO CurrRange = 1, tNumOfRanges 
              iHighlightList(CurrRange) = 1
            ENDDO
            CALL WDialogPutMenu(IDF_DataRangeMenu,TitleOfRange,tNumOfRanges,iHighlightList)
            CALL WDialogShow(-1,-1,IDOK,Modal)
            IF (WInfoDialog(ExitButton) .NE. IDOK) THEN
              Load_raw_File = 2
              CALL WDialogUnload
              CALL PopActiveWindowID
              RETURN
            ENDIF
            CALL WDialogGetMenu(IDF_DataRangeMenu,iHighlightList)
! The array iHighlightList now contains '1' for every data range to be read, '0' otherwise.
            CALL WDialogUnload
            CALL PopActiveWindowID
            AtLeastOneSelected = .FALSE.
            DO I = 1, tNumOfRanges
              LoadRange(I) = (iHighlightList(I) .EQ. 1)
              IF (LoadRange(I)) AtLeastOneSelected = .TRUE.
            ENDDO
            IF (.NOT. AtLeastOneSelected) THEN
              CALL ErrorMessage('Please select at least one range.')
              RETURN
            ENDIF
          ENDIF
          Smallest2ThetaStep = 90.0
          DO I = 1, tNumOfRanges
            IF (t2ThetaStep(I) .LT. Smallest2ThetaStep) Smallest2ThetaStep = t2ThetaStep(I)
          ENDDO
          Smallest2ThetaStep = Smallest2ThetaStep - 0.00005 ! In case of rounding errors
          Load_raw_File = Load_rawBruker_File(TheFileName,LoadRange,Smallest2ThetaStep)
          ESDsFilled = .TRUE.
          IF (Load_raw_File .NE. 1) RETURN
      END SELECT
! This is definitely laboratory data
      JRadOption = 1
      CALL Upload_Source
      Load_raw_File = 0
      RETURN
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_raw_File
!
!*****************************************************************************
!
      SUBROUTINE GetDataRangesBruker(TheFileName, TheNumOfRanges, TitleOfRange, The2ThetaStep)
!
! This function tries to build a list of 'dataranges' present in a *.raw file (binary format from Bruker machines).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from Load_raw_File
!
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : TheNumOfRanges = the number of valid data ranges (i.e. including those with e.g. zero data points)
!           TitleOfRange = some sort of identifier for that range
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: TheNumOfRanges
      CHARACTER*80,  INTENT (  OUT) :: TitleOfRange(*)
      REAL,          INTENT (  OUT) :: The2ThetaStep(*)

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER     I, Shift, hFile
      REAL*8      TwoThetaStart, TwoThetaStep
      REAL        Lambda1, t2ThetaStart, t2ThetaEnd
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
      CHARACTER*80 tString
      REAL StepTime

      TwoThetaStart = 0.0
      TwoThetaStep  = 0.0
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=TheFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
! First check the version, which follows from the fourth byte in the file. Bytes 1-3 are: RAW
! "RAW " = version 1. The oldest version, requested by Bruker not the be supported any more.
! "RAW2" = version 2.
! "RAW1" = version 3. The most recent version.
      READ(hFile,REC=1,ERR=999) C4
      SELECT CASE (C4(4:4))
        CASE (' ')
! Warn the user
          CALL ErrorMessage("This version of the .raw format is no longer supported."//&
                           CHAR(13)//"Please convert to a newer version and try again.")
        CASE ('1')
! A version 3 file, the most recent and most complicated format
! Number of completed data ranges.
! There seem to be 2 uses for this:
! 1. Measure the same range over and over again (time series)
! 2. Measure different ranges at different resolutions / counting times
          READ(hFile,REC=4,ERR=999) NumOfDataRanges
          TheNumOfRanges = NumOfDataRanges
! The complete file header is 712 bytes, so start reading at record (712 DIV 4) + RecNumber
          Offset = 178
! Loop over the number of data ranges
          DO CurrDataRange = 1, NumOfDataRanges
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
! Length of Range Header Structure in bytes. Must be 304.
            READ(hFile,REC=Offset+1,ERR=999) I4
            IF (I4 .NE. 304) THEN
! The user should be warned here
              CALL ErrorMessage("Length of Range Header Structure must be 304.")
              GOTO 999
            ENDIF
! Number of 'data records'
            READ(hFile,REC=Offset+2,ERR=999) NumOfBins
! Check that we will not read less than 1 data point
            IF (NumOfBins .EQ. 0) THEN
              CALL ErrorMessage("The file contains no valid data.")
              GOTO 999
            ENDIF
! Starting angle for 2 theta drive in degrees
            READ(hFile,REC=Offset+5,ERR=999) RecReal(1)
            READ(hFile,REC=Offset+6,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the starting angle in degrees
            TwoThetaStart = R8
            t2ThetaStart = SNGL(TwoThetaStart)
! Read scan mode: 0 = step, 1 = continuous
            READ(hFile,REC=Offset+43,ERR=999) I4
! Step size in degrees
            READ(hFile,REC=Offset+45,ERR=999) RecReal(1)
            READ(hFile,REC=Offset+46,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the step size in degrees
            TwoThetaStep = R8
            IF (TwoThetaStep .LT. 0.000001) GOTO 999
            READ(hFile,REC=Offset+49,ERR=999) StepTime
            The2ThetaStep(CurrDataRange) = SNGL(TwoThetaStep)
            t2ThetaEnd = t2ThetaStart + SNGL(TwoThetaStep) * NumOfBins
! Next REAL*8 contains primary wavelength (Angstroms) for this data range
! Assuming a monochromated beam, this is the wavelength we would want
            READ(hFile,REC=Offset+61,ERR=999) RecReal(1)
            READ(hFile,REC=Offset+62,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the wavelength in Angstroms
            Lambda1 = R8
! Check that the same wavelength has been used for all data ranges
            IF (CurrDataRange .EQ. 1) THEN
! Store this value as the experimental wavelength
              CALL Set_Wavelength(Lambda1)
            ELSE
              IF (ABS(Lambda1-ALambda) .GT. 0.0001) THEN
                CALL ErrorMessage('More than one wavelength used, reading aborted.')
                GOTO 999
              ENDIF
            ENDIF
! Data record length. Must be 4.
            READ(hFile,REC=Offset+64,ERR=999) I4
            IF (I4 .NE. 4) THEN
! The user should be warned here
              CALL ErrorMessage("Record length must be 4.")
              GOTO 999
            ENDIF
! Length of supplementary header (bytes)
            READ(hFile,REC=Offset+65,ERR=999) I4
! Check if length of supplementary header is a multiple of four.
            IF (MOD(I4,4) .NE. 0) THEN
! The user should be warned here
              CALL ErrorMessage("Length of Supplementary Header is not a multiple of 4.")
              GOTO 999
            ENDIF
            WRITE(tString,'(I3,1X,A1,2(F8.3,1X,A1),F9.6,1X,A1,F7.2)') CurrDataRange, CHAR(9), t2ThetaStart, &
              CHAR(9), t2ThetaEnd, CHAR(9), The2ThetaStep(CurrDataRange), CHAR(9), StepTime
            TitleOfRange(CurrDataRange) = tString
! Skip all supplementary headers of the current data range
            Offset = Offset + I4 / 4
! Skip the header of the current data range
            Offset = Offset + 76
! The data begin from here as REAL*4 records
! Next data range starts after this one
            Offset = Offset + NumOfBins
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
          ENDDO
        CASE ('2')
! A version 2 file.
! Next two bytes are the number of data ranges
          READ(hFile,REC=2,ERR=999) I4
! Due to the EQUIVALENCE statement, I2(1) holds the first two bytes of I4.
! Because of 'backwords storage', this should work.
          NumOfDataRanges = I2(1)
          TheNumOfRanges = NumOfDataRanges
! The next thing we need from the file header is Lambda 1
! Unfortunately, mean while one data item had a length of two bytes.
! Due to some EQUIVALENCE statements, this can be read as follows:
          READ(hFile,REC=48,ERR=999) I4
          READ(hFile,REC=49,ERR=999) I4_2
! Shift by two bytes
          I2(1) = I2(2)
          I2(2) = I2(3)
! R4 now holds the wavelength
          Lambda1 = R4
          CALL Set_Wavelength(Lambda1)
! The complete file header is 256 bytes, so start reading at record (256 DIV 4) + RecNumber
          Offset = 64
! @ Now there should be a loop over the number of data ranges
          DO CurrDataRange = 1, NumOfDataRanges
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
! The next two bytes are the length of the Range Header in bytes.
            READ(hFile,REC=Offset+1,ERR=999) I4
! Due to EQUIVALENCE, I2(1) holds the header length
            SizeOfHeader = I2(1)
! Due to EQUIVALENCE, I2(2) holds the number of 'data records'
            NumOfBins = I2(2)
! Check that we will not read less than 1 data point
            IF (NumOfBins .EQ. 0) THEN
! The user should be warned here
              CALL ErrorMessage("The file contains no valid data.")
              GOTO 999
            ENDIF
! The step size. A REAL*4 in this format
            READ(hFile,REC=Offset+4,ERR=999) R4
            TwoThetaStep = R4
            The2ThetaStep(CurrDataRange) = TwoThetaStep
            IF (TwoThetaStep .LT. 0.000001) RETURN
! Six times the start points of the range
! @@@@ Which one is 2 theta ?????
            READ(hFile,REC=Offset+6,ERR=999) R4
            TwoThetaStart = R4
            t2ThetaStart = TwoThetaStart
! Now we can start reading the raw data. The complete header can consist of any number
! of bytes, and we can only read per four. The data is in REAL*4 format. This may require some
! shifting of the bytes read in. It is actually possible that the last datapoint cannot be read.
            IF (MOD(SizeOfHeader,4) .NE. 0) THEN
              NumOfBins = NumOfBins - 1

              OffSet = Offset + 1 + (SizeOfHeader / 4)
! Due to the way integer division in FORTRAN works, 
! the fractional part of SizeOfHeader / 4 is discarded
              Shift = SizeOfHeader - MOD(SizeOfHeader,4)
              READ(hFile,REC=Offset,ERR=999) I4
              DO I = 1, NumOfBins
                READ(hFile,REC=Offset+I,ERR=999) I4_2
                I1(1) = I1(1+Shift)
                I1(2) = I1(2+Shift)
                I1(3) = I1(3+Shift)
                I1(4) = I1(4+Shift)
                YOBS(I) = R4
                I1(1) = I1(1+(4-Shift))
                I1(2) = I1(2+(4-Shift))
                I1(3) = I1(3+(4-Shift))
                I1(4) = I1(4+(4-Shift))

              ENDDO
            ELSE
! When we are here, the size of the header was a multiple of 4 and we can read the data easily
              OffSet = Offset + SizeOfHeader / 4

              DO I = 1, NumOfBins
                READ(hFile,REC=Offset+I,ERR=999) R4
                YOBS(I) = R4
              ENDDO

            ENDIF

          ENDDO
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
        CASE DEFAULT
          CALL ErrorMessage('Unrecognised *.raw format.')
      END SELECT
 999  CLOSE(hFile)

      END SUBROUTINE GetDataRangesBruker
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_rawBruker_File(TheFileName,LoadRange,Smallest2ThetaStep)
!
! This function tries to load a *.raw file (binary format from Bruker machines).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from Load_raw_File
!
! JvdS 23 July 2001
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
      LOGICAL,       INTENT (IN   ) :: LoadRange(*)
      REAL,          INTENT (IN   ) :: Smallest2ThetaStep

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER     tNOBS1, tNOBS2, hFile
      REAL        tXOBS1(2*MOBS), tYOBS1(2*MOBS), tSECS1(2*MOBS)
      REAL        tXOBS2(2*MOBS), tYOBS2(2*MOBS), tSECS2(2*MOBS)
      INTEGER     iP(2*MOBS)
      INTEGER     I, Shift
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
      REAL        StepTime, Last2Theta
      LOGICAL     IsFirstRange

! Current status, initialise to 'error'
      Load_rawBruker_File = 1
      TwoThetaStart = 0.0
      TwoThetaStep  = 0.0
      IsFirstRange = .TRUE.
      tNOBS1 = 0
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(hFile,FILE=TheFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
! First check the version, which follows from the fourth byte in the file. Bytes 1-3 are: RAW
! "RAW " = version 1. The oldest version, requested by Bruker not the be supported any more.
! "RAW2" = version 2.
! "RAW1" = version 3. The most recent version.
      READ(hFile,REC=1,ERR=999) C4
      SELECT CASE (C4(4:4))
        CASE (' ')
! Warn the user
          CALL ErrorMessage("This version of the .raw format is no longer supported."//&
                           CHAR(13)//"Please convert to a newer version and try again.")
          GOTO 999
        CASE ('1')
! A version 3 file, the most recent and most complicated format
!? 'current file status'. 1 = done, 2 = active, 3 = aborted, 4 = interrupted
!?          READ(UNIT=10,REC=3,ERR=999) I4
!?          IF (I4 .NE. 1) THEN
!?! The user should be warned here
!?            CALL ErrorMessage("Current file status is Active, Aborted or Interrupted.")
!?            RETURN
!?          ENDIF
! Number of completed data ranges.
! There seem to be 2 uses for this:
! 1. Measure the same range over and over again (time series)
! 2. Measure different ranges at different resolutions / counting times
          READ(hFile,REC=4,ERR=999) NumOfDataRanges
! The complete file header is 712 bytes, so start reading at record (712 DIV 4) + RecNumber
          Offset = 178
! Loop over the number of data ranges
          DO CurrDataRange = 1, NumOfDataRanges
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
! Length of Range Header Structure in bytes. Must be 304.
            READ(hFile,REC=Offset+1,ERR=999) I4
            IF (I4 .NE. 304) THEN
! The user should be warned here
              CALL ErrorMessage("Length of Range Header Structure must be 304.")
              GOTO 999
            ENDIF
! Number of 'data records'
            READ(hFile,REC=Offset+2,ERR=999) NumOfBins
! Check that we will not read more than MOBS data points
            IF (NumOfBins .GT. MOBS) THEN
! Warn the user
              CALL ProfileRead_TruncationWarning(TheFileName,MOBS)
              NumOfBins = MOBS
            ENDIF
! Check that we will not read less than 1 data point
            IF (NumOfBins .EQ. 0) THEN
              CALL ErrorMessage("The file contains no valid data.")
              GOTO 999
            ENDIF
! Starting angle for 2 theta drive in degrees
            READ(hFile,REC=Offset+5,ERR=999) RecReal(1)
            READ(hFile,REC=Offset+6,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the starting angle in degrees
            TwoThetaStart = R8
! Read scan mode: 0 = step, 1 = continuous
            READ(hFile,REC=Offset+43,ERR=999) I4
! Step size in degrees
            READ(hFile,REC=Offset+45,ERR=999) RecReal(1)
            READ(hFile,REC=Offset+46,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the step size in degrees
            TwoThetaStep = R8
            IF (TwoThetaStep .LT. 0.000001) GOTO 999
            READ(hFile,REC=Offset+49,ERR=999) StepTime
! Next line to avoid DASH from unexpectedly rescaling the data if only a single pattern is present
            IF (NumOfDataRanges .EQ. 1) StepTime = 1.0
! Next REAL*8 contains primary wavelength (Angstroms) for this data range
! Assuming a monochromated beam, this is the wavelength we would want
            READ(hFile,REC=Offset+61,ERR=999) RecReal(1)
            READ(hFile,REC=Offset+62,ERR=999) RecReal(2)
! Due to the EQUIVALENCE statement, R8 now holds the wavelength in Angstroms
            Lambda1 = R8
! Check that the same wavelength has been used for all data ranges
            IF (LoadRange(CurrDataRange)) THEN
              IF (IsFirstRange) THEN
! Store this value as the experimental wavelength
                CALL Set_Wavelength(Lambda1)
                IsFirstRange = .FALSE.
              ELSE
                IF (ABS(Lambda1-ALambda) .GT. 0.0001) THEN
                  CALL ErrorMessage('More than one wavelength used, reading aborted.')
                  GOTO 999
                ENDIF
              ENDIF
            ENDIF
! Data record length. Must be 4.
            READ(hFile,REC=Offset+64,ERR=999) I4
            IF (I4 .NE. 4) THEN
! The user should be warned here
              CALL ErrorMessage("Record length must be 4.")
              GOTO 999
            ENDIF
! Length of supplementary header (bytes)
            READ(hFile,REC=Offset+65,ERR=999) I4
! Check if length of supplementary header is a multiple of four.
            IF (MOD(I4,4) .NE. 0) THEN
! The user should be warned here
              CALL ErrorMessage("Length of Supplementary Header is not a multiple of 4.")
              GOTO 999
            ENDIF
! Skip all supplementary headers of the current data range
            Offset = Offset + I4 / 4
! Skip the header of the current data range
            Offset = Offset + 76
! The data begin from here as REAL*4 records
            IF (LoadRange(CurrDataRange)) THEN
! Append to existing data
! Fill the 2 theta values first
              CurrTwoTheta = TwoThetaStart
              DO I = 1, NumOfBins
                tXOBS1(tNOBS1 + I) = CurrTwoTheta
                CurrTwoTheta = CurrTwoTheta + TwoThetaStep
              ENDDO
              DO I = 1, NumOfBins
                READ(hFile,REC=Offset+I,ERR=999) R4
                tYOBS1(tNOBS1 + I) = R4
                tSECS1(tNOBS1 + I) = StepTime
              ENDDO
              tNOBS2 = tNOBS1 + NumOfBins
! Now sort the resulting data according to 2 theta
              CALL SORT_REAL(tXOBS1,iP,tNOBS2)
! Now write sorted data to tXOBS2, tYOBS2, tEOBS2
              DO I = 1, tNOBS2
                tXOBS2(I) = tXOBS1(iP(I))
                tYOBS2(I) = tYOBS1(iP(I))
                tSECS2(I) = tSECS1(iP(I))
              ENDDO
              tXOBS1 = 0.0
              tYOBS1 = 0.0
              tSECS1 = 0.0
! Now compare consecutive datapoints to see if they should be merged (i.e., have nearly equal 2 theta values)
              tXOBS1(1) = tXOBS2(1)
              tYOBS1(1) = tYOBS2(1)
              tSECS1(1) = tSECS2(1)
              Last2Theta = tXOBS2(1)
              tNOBS1 = 1
              DO I = 2, tNOBS2
                IF (ABS(tXOBS2(I) - Last2Theta) .GE. Smallest2ThetaStep) THEN
                  tNOBS1 = tNOBS1 + 1
                  Last2Theta = tXOBS2(I)
                ENDIF
                tXOBS1(tNOBS1) = (tSECS1(tNOBS1)*tXOBS1(tNOBS1) + tSECS2(I)*tXOBS2(I)) / (tSECS1(tNOBS1)+tSECS2(I))
                tYOBS1(tNOBS1) = tYOBS1(tNOBS1) + tYOBS2(I)
                tSECS1(tNOBS1) = tSECS1(tNOBS1) + tSECS2(I)
              ENDDO
            ENDIF
! Next data range starts after this one
            Offset = Offset + NumOfBins
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
          ENDDO
          NumOfBins = tNOBS1
          DO I = 1, NumOfBins
            XOBS(I) = tXOBS1(I)
! Poisson statistics
            YOBS(I) = tYOBS1(I) / tSECS1(I)
            EOBS(I) = SQRT(tYOBS1(I)) / tSECS1(I)
          ENDDO
        CASE ('2')
! A version 2 file.
! Next two bytes are the number of data ranges
          READ(hFile,REC=2,ERR=999) I4
! Due to the EQUIVALENCE statement, I2(1) holds the first two bytes of I4.
! Because of 'backwords storage', this should work.
          NumOfDataRanges = I2(1)
! The next thing we need from the file header is Lambda 1
! Unfortunately, mean while one data item had a length of two bytes.
! Due to some EQUIVALENCE statements, this can be read as follows:
          READ(hFile,REC=48,ERR=999) I4
          READ(hFile,REC=49,ERR=999) I4_2
! Shift by two bytes
          I2(1) = I2(2)
          I2(2) = I2(3)
! R4 now holds the wavelength
          Lambda1 = R4
          CALL Set_Wavelength(Lambda1)
! The complete file header is 256 bytes, so start reading at record (256 DIV 4) + RecNumber
          Offset = 64
! @ Now there should be a loop over the number of data ranges
          DO CurrDataRange = 1, NumOfDataRanges
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
! The next two bytes are the length of the Range Header in bytes.
            READ(hFile,REC=Offset+1,ERR=999) I4
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
              GOTO 999
            ENDIF
! The step size. A REAL*4 in this format
            READ(hFile,REC=Offset+4,ERR=999) R4
            TwoThetaStep = R4
            IF (TwoThetaStep .LT. 0.000001) RETURN
! Six times the start points of the range
! @@@@ Which one is 2 theta ?????
            READ(hFile,REC=Offset+6,ERR=999) R4
            TwoThetaStart = R4
! Fill the 2 theta values
            CurrTwoTheta = TwoThetaStart
            DO I = 1, NumOfBins
              XOBS(I) = CurrTwoTheta
              CurrTwoTheta = CurrTwoTheta + TwoThetaStep
            ENDDO
! Now we can start reading the raw data. The complete header can consist of any number
! of bytes, and we can only read per four. The data is in REAL*4 format. This may require some
! shifting of the bytes read in. It is actually possible that the last datapoint cannot be read.
            IF (MOD(SizeOfHeader,4) .NE. 0) THEN
              NumOfBins = NumOfBins - 1
              OffSet = Offset + 1 + (SizeOfHeader / 4)
! Due to the way integer division in FORTRAN works, 
! the fractional part of SizeOfHeader / 4 is discarded
              Shift = SizeOfHeader - MOD(SizeOfHeader,4)
              READ(hFile,REC=Offset,ERR=999) I4
              DO I = 1, NumOfBins
                READ(hFile,REC=Offset+I,ERR=999) I4_2
                I1(1) = I1(1+Shift)
                I1(2) = I1(2+Shift)
                I1(3) = I1(3+Shift)
                I1(4) = I1(4+Shift)
                YOBS(I) = R4
                I1(1) = I1(1+(4-Shift))
                I1(2) = I1(2+(4-Shift))
                I1(3) = I1(3+(4-Shift))
                I1(4) = I1(4+(4-Shift))
              ENDDO
            ELSE
! When we are here, the size of the header was a multiple of 4 and we can read the data easily
              OffSet = Offset + SizeOfHeader / 4
              DO I = 1, NumOfBins
                READ(hFile,REC=Offset+I,ERR=999) R4
                YOBS(I) = R4
              ENDDO
            ENDIF
          ENDDO
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
        CASE DEFAULT
          CALL ErrorMessage('Unrecognised *.raw format.')
          GOTO 999
      END SELECT
      NOBS = NumOfBins
      Load_rawBruker_File = 0
! Exit code is error by default, so we can simply return
 999  CLOSE(hFile)

      END FUNCTION Load_rawBruker_File
!
!*****************************************************************************
!
      SUBROUTINE GetDataRangesSTOE(TheFileName,tNumOfRanges,TitleOfRange)
!
! This function tries to interpret the data ranges in a *.raw file (binary format from STOE machines).
!
! See Load_rawSTOE_File for comments
!
! JvdS Oct 2001
!
! INPUT   : TheFileName = the file name
!
! OUTPUT  : tNumOfRanges = the number of valid data ranges (i.e. including those with e.g. zero data points)
!           TitleOfRange = some sort of identifier for that range
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE
! Prevent the compiler from realigning the records
!DEC$ OPTIONS /ALIGN=RECORDS=PACKED

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: tNumOfRanges
      CHARACTER*80,  INTENT (  OUT) :: TitleOfRange(8)

! ------------------------------------------------
! MAX_RANGE : maximum number of ranges in one file
! ------------------------------------------------

      INTEGER    MAX_RANGE
      PARAMETER (MAX_RANGE = 8)

! -----------------------------------------------------------
! Raw Data Header
! -----------------------------------------------------------

      TYPE RAW_HEADER
        CHARACTER*8    Ident       ! file ID & version = RAW_REVN
        CHARACTER*8    ProgName    ! name of creating program
        CHARACTER*16   FileDate    ! creation date : "dd-mon-yy hh:mm"
        CHARACTER*80   Title       ! title
        CHARACTER*192  UserText    ! comment
        INTEGER*2      DifrType    ! diffractometer type
        INTEGER*2      AngResol    ! angular resolution ( steps/degree )
        INTEGER*2      Monochrm    ! monochromator
        INTEGER*2      Anode       ! anode material
        INTEGER*2      Detector    ! type of detector used
        INTEGER*2      M_Dist      ! distance monochromator to tube [mm]
        INTEGER*2      C_Dist      ! distance sample to counter
        INTEGER*2      Tube_kV     ! generator setting ( kV )
        INTEGER*2      Tube_mA     ! generator setting ( mA )
        REAL*4         Wavelen1    ! wavelength ( alpha1 )
        REAL*4         Wavelen2    ! wavelength ( alpha2 )
        REAL*4         DMono       ! d* of monochromator reflection
        INTEGER*2      PSDaddr(4)  ! first & last PSD addresses
        REAL*4         PSDstep(2)  ! PSD stepwidths
        REAL*4         CapDiam     ! diameter of capillary
        REAL*4         SamTrans    ! transmission factor of the sample
        INTEGER*2      ScanMode    ! scan mode ( Transm. / Refl. / Capillary )
        INTEGER*2      ScanType    ! scan type ( 2th:omg / 2th / omg / free )
        INTEGER*2      PsdMode     ! PSD   mode ( stationary / moving )
        INTEGER*2      OmgMode     ! omega mode ( fixed / moving )
        INTEGER*2      ScanUse     ! scan usage
        INTEGER*2      Nadd        ! number of points to add ( PSD only )
        INTEGER*2      Correct     ! raw data correction(s) applied
        INTEGER*2      SeqNum(2)   ! sample number .. of .., when used with
                                   ! sample changer or other serial measurement
        INTEGER*2      NRange      ! number of ranges to be measured
        INTEGER*2      NR_done     ! number of ranges actually measured
        INTEGER*2      NR_curr     ! number of range currently being measured,
                                   ! used as 'measurement in progress' flag
        INTEGER*2      NSample     ! number of samples
        REAL*4         VarOmg(3)   ! Omega( begin, end, step ) for 'VAROMG'
        REAL*4         SPos1(3)    ! SPos1( begin, end, step ) for 'SPOS'
        REAL*4         SPos2(3)    ! SPos2( begin, end, step ) for 'SPOS'
        INTEGER*2      FileNum     ! first file number for serial measurement
        INTEGER*2      NFiles      ! number of files
        INTEGER*2      SameTR      ! Multi-Sample : same title/ranges for all samples ?
        INTEGER*2      TWait       !                waiting time [min]
        INTEGER*2      IFree(6)    ! free space left
        CHARACTER*16   ModDate     ! date of last modification
        CHARACTER*6    ModProg     ! name of modifying program
        INTEGER*2      ModSave     ! not used
        INTEGER*2      PsdSave     ! not used
        INTEGER*4      NTotal      ! total number of data points in the file
        INTEGER*2      DataType    ! type of data in the file
        INTEGER*2      History     ! bit mask showing data modification
                                   ! for all ranges in the file
        INTEGER*2      NDatRec     ! Number of data records
        INTEGER*2      LastRec     ! Total file size in records
        INTEGER*2      NExtra      ! additional records
        INTEGER*2      HeadRec(1:MAX_RANGE) ! Record numbers of range headers ( start at 1 )
        INTEGER*2      DataRec(1:MAX_RANGE) ! Number of data records for each range
      END TYPE ! 512 bytes

! -------------------------------------------------------
! Range header ( for every scan range actually measured )
! -------------------------------------------------------

! background data
      TYPE OLDBGSTUFF
        INTEGER*2 BG_Pos(64)
        INTEGER*4 BG_Int(64)
      END TYPE

      TYPE RANGE_INFO
        CHARACTER*16 StartTime ! start of CD ( date and time )
        CHARACTER*16 EndTime   ! end   of CD
        INTEGER*2    Status    ! completion status
        INTEGER*2    NPoints   ! number of data points measured
        INTEGER*2    NBackgr   ! number of BG points
        INTEGER*2    DataForm  ! data format : long / short
        REAL*4       CntFactor ! scaling factor ( since RAW_1.05 )
        REAL*4       Begin(2)  ! range begin ( 2theta / omega )
        REAL*4       End(2)    ! range end
        REAL*4       Step(2)   ! stepwidths
        REAL*4       StepTime  ! measuring time per step
        REAL*4       Omega     ! omega position for 'VAROMG'
        REAL*4       SamPos1   ! sample position 1
        REAL*4       SamPos2   ! sample position 2
        REAL*4       T_Avg     ! average temperature of sample
        REAL*4       T_Var     ! temperature variation
        REAL*4       BeamInt   ! primary beam intensity ( for QUANT )
        REAL*4       BeamVar   ! primary beam variation
        REAL*4       DivSlBeg  ! Divergence slit width at scan begin
        REAL*4       DivSlEnd  ! Divergence slit width at scan end
        REAL*4       RecSlit   ! Receiving slit width ( constant )
        REAL*4       SamArea   ! const. Sample area ( > 0 : variable div.slit )
        INTEGER*2    NSamPos   ! number of values used with 'SAMPOS'
        INTEGER*2    History   ! bit mask showing data modifications
        INTEGER*4    MinCount  ! minimum count
        INTEGER*4    MaxCount  ! maximum count
        TYPE(OLDBGSTUFF) BACKGROUND ! We don't need the background stuff
      END TYPE ! 512 bytes

      TYPE(RAW_HEADER) Header
      TYPE(RANGE_INFO) RangeInfo

      INTEGER hFile
      CHARACTER*128 tString
      INTEGER RangeNr, NumberOfRanges
!DEC$ END OPTIONS

      tNumOfRanges = 0
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 128 (=512 bytes)
      hFile = 11
      OPEN(UNIT=hFile,FILE=TheFileName,ACCESS='DIRECT',RECL=128,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
      READ(hFile,REC=1,ERR=999) Header
      IF (Header%NTotal .EQ. 0) THEN
        CALL ErrorMessage('File contains no data.')
        GOTO 999
      ENDIF
      NumberOfRanges = Header%NR_done
      IF (NumberOfRanges .GT. MAX_RANGE) THEN
        CALL DebugErrorMessage('NumberOfRanges .GT. MAX_RANGE in GetDataRangesSTOE')
        NumberOfRanges = MAX_RANGE
      ENDIF
      DO RangeNr = 1, NumberOfRanges
! Read the header for this range
        READ(hFile,REC=Header%HeadRec(RangeNr),ERR=999) RangeInfo
        IF (RangeInfo%NPoints .NE. 0) THEN
          tNumOfRanges = tNumOfRanges + 1
          tString = ' '
          WRITE(tString(1:80),'(A9,I1,A12,F7.3,A3,F7.3,A7,F6.2)') &
 'Range nr ',tNumOfRanges,', 2 theta = ',RangeInfo%Begin(1),' - ',RangeInfo%End(1),' <T> = ',RangeInfo%T_Avg
          TitleOfRange(tNumOfRanges)(1:80) = tString(1:80)
        ENDIF
      ENDDO
 999  CLOSE(hFile)
! Exit code is error by default, so we can simply return

      END SUBROUTINE GetDataRangesSTOE
!
!*****************************************************************************
!
      INTEGER FUNCTION Load_rawSTOE_File(TheFileName,irange)
!
! This function tries to load a *.raw file (binary format from STOE machines).
! The routine basically assumes that the file is OK.
!
! Note that this function should only be called from Load_raw_File
!
! JvdS Oct 2001
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
! Prevent the compiler from realigning the records
!DEC$ OPTIONS /ALIGN=RECORDS=PACKED

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (IN   ) :: irange

      INCLUDE 'PARAMS.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER  DIF_TR
      INTEGER  DIF_BB
      INTEGER  DIF_TT
      INTEGER  DIF_DM
      INTEGER  DIF_FIX
      INTEGER  MON_SEC
      INTEGER  MON_GER
      INTEGER  MON_GRA
      INTEGER  MON_QUARTZ
      INTEGER  MON_xxx 
      INTEGER  MON_GE111
      INTEGER  MON_GE220
      INTEGER  MON_GAAS
      INTEGER  MON_MOTOR
      INTEGER  MON_GER_MOT
      INTEGER  ANO_AG
      INTEGER  ANO_MO
      INTEGER  ANO_CU
      INTEGER  ANO_CO
      INTEGER  ANO_FE
      INTEGER  ANO_CR
      INTEGER  ANO_XX
      INTEGER  DET_SCICNT
      INTEGER  DET_CURVED
      INTEGER  DET_LINEAR
      INTEGER  DET_IP
      INTEGER  MOD_TRANS
      INTEGER  MOD_REFL
      INTEGER  MOD_DEBYE
      INTEGER  MOD_PSDFIX
      INTEGER  MOD_PSDMOV
      INTEGER  MOD_OMGFIX
      INTEGER  MOD_OMGMOV
      INTEGER  SCT_2THETA
      INTEGER  SCT_OMEGA
      INTEGER  SCT_2THOMG
      INTEGER  SCT_FREE
      INTEGER  SCU_VANILLA
      INTEGER  SCU_MULTI
      INTEGER  SCU_VAROMG
      INTEGER  SCU_SAMPOS
      INTEGER  SCU_TEMPR
      INTEGER  SCU_QUANT
      INTEGER  SCU_REPEAT
      INTEGER  DAT_MEASUR
      INTEGER  DAT_THEORY
      INTEGER  HST_PSDCOR
      INTEGER  HST_CALIB
      INTEGER  HST_THSCAL
      INTEGER  HST_SMOOTH
      INTEGER  HST_BGSUB
      INTEGER  HST_STRIP2
      INTEGER  HST_RAWMOD
      INTEGER  HST_ABSCOR
      INTEGER  SCF_LONG
      INTEGER  SCF_SHORT

! Diffractometers
      PARAMETER (DIF_TR  = 1)  ! Transmission diffractometer
      PARAMETER (DIF_BB  = 2)  ! Bragg-Brentano
      PARAMETER (DIF_TT  = 3)  ! Theta-Theta
      PARAMETER (DIF_DM  = 4)  ! Double-Monochromator
      PARAMETER (DIF_FIX = 5)  ! Stage with fixed PSD ( no diffractometer )

! Monochromators ( if motorised: MON_foo + MON_MOTOR )
      PARAMETER (MON_SEC    = 1)  ! secondary monochromator
      PARAMETER (MON_GER    = 2)  ! curved Germanium (111)
      PARAMETER (MON_GRA    = 3)  ! curved Graphite  (002)
      PARAMETER (MON_QUARTZ = 4)  ! curved Quartz    (101)  ( since RAW_1.05 )
      PARAMETER (MON_xxx    = 5)  ! ...
      PARAMETER (MON_GE111  = 6)  ! DM: Germanium (111)
      PARAMETER (MON_GE220  = 7)  ! DM: Germanium (220)
      PARAMETER (MON_GAAS   = 8)  ! DM: Gallium Arsenide (400)

      PARAMETER (MON_MOTOR  = 100)  ! since RAW_1.05, only in combination with MON_foo

! now obsolete
      PARAMETER (MON_GER_MOT = 4)  ! only in RAW_1.04 ( converted to MON_GER )

! Anode materials
      PARAMETER (ANO_AG = 1)  ! Silver
      PARAMETER (ANO_MO = 2)  ! Molybdenum
      PARAMETER (ANO_CU = 3)  ! Copper
      PARAMETER (ANO_CO = 4)  ! Cobalt
      PARAMETER (ANO_FE = 5)  ! Iron
      PARAMETER (ANO_CR = 6)  ! Chromium
      PARAMETER (ANO_XX = 7)  ! user defined

! detector type
      PARAMETER (DET_SCICNT = 1)  ! scintillation counter
      PARAMETER (DET_CURVED = 2)  ! curved PSD
      PARAMETER (DET_LINEAR = 3)  ! linear PSD
      PARAMETER (DET_IP     = 4)  ! image plate PSD

! scan modes
      PARAMETER (MOD_TRANS = 1)  ! Transmission
      PARAMETER (MOD_REFL  = 2)  ! Reflection
      PARAMETER (MOD_DEBYE = 3)  ! Debye-Scherrer ( Capillary )

! PSD modes
      PARAMETER (MOD_PSDFIX = 1)  ! fixed (stationary) PSD
      PARAMETER (MOD_PSDMOV = 2)  ! moving PSD
      PARAMETER (MOD_OMGFIX = 1)  ! fixed  omega
      PARAMETER (MOD_OMGMOV = 2)  ! moving omega

! scan types
      PARAMETER (SCT_2THETA = 1)  ! 2theta scan ( fixed omega )
      PARAMETER (SCT_OMEGA  = 2)  !  omega scan ( fixed 2theta )
      PARAMETER (SCT_2THOMG = 3)  ! 2theta/omega scan ( 2:1 )
      PARAMETER (SCT_FREE   = 4)  ! independent ( any combination of 2th/omg )
   
! scan usage
      PARAMETER (SCU_VANILLA = 0)  ! "normal" single measurement
      PARAMETER (SCU_MULTI   = 1)  ! Multiple Samples ( SampleChanger )
      PARAMETER (SCU_VAROMG  = 2)  ! Variable Omega
      PARAMETER (SCU_SAMPOS  = 3)  ! Sample Position
      PARAMETER (SCU_TEMPR   = 4)  ! Temperature ( via HTC )
      PARAMETER (SCU_QUANT   = 5)  ! Quantitative Analysis
      PARAMETER (SCU_REPEAT  = 6)  ! Scan Repetition

! type of data stored in the raw data file
      PARAMETER (DAT_MEASUR = 1)
      PARAMETER (DAT_THEORY = 2)  ! generate by Theo

! raw data history ( bit numbers )
! bits 0-3  apply to all ranges (  Rawd.History )
!      4-15 are range specific  ( Range.History )

      PARAMETER (HST_PSDCOR = 0)  ! PSD corrections
      PARAMETER (HST_CALIB  = 1)  ! Correction for instrument errors

      PARAMETER (HST_THSCAL = 4)  ! 2theta scale changed
      PARAMETER (HST_SMOOTH = 5)  ! Smoothing
      PARAMETER (HST_BGSUB  = 6)  ! Background subtracted
      PARAMETER (HST_STRIP2 = 7)  ! Alpha2 stripping
      PARAMETER (HST_RAWMOD = 8)  ! Other modifications ( RAWDAT )
      PARAMETER (HST_ABSCOR = 9)  ! absorption correction

! raw data word length
      PARAMETER (SCF_LONG  = 0)   ! 32 bit integers 
      PARAMETER (SCF_SHORT = 1)   ! 16 bit

! --------------------------------------------------------------------------
!
! RAW data files : binary, 512 byte records
!
! record 1     : Raw data header
!        2 + 3 : Scan range info for all ranges to be measured
!                ( since RAW_1.05, record 3 was not used before )
!
!        4 ..  : additional (RAW_HEADER.NExtra) records
!
!        NExtra + 4:   Range Header for measured data ( range 1 )
!                 5:   First data record for range 1
!
! Raw data are total counts stored as 16 or 32 bit integers depending on the
! maximum intensity, indicated by RANGEINFO.DataForm = SCF_LONG or SCF_SHORT.
!
! --------------------------------------------------------------------------
!
! Revision history :
!
! 1.03 : Rawd.ModDate, Rawd.ModProg, Range.History and
!        Range.NSamPos have been added.
!        Definition of 'Scan' record removed, it is only
!        used in POW and HTC.
!
! 1.04 : For Multi-sample files, records 4 thru 7 of the
!        master file contain individual sample titles,
!        files and ranges
!
! 1.05 : Conversion to C/C++
!
!        New SCAN_INFO ( now MAX_RANGE * 128 Byte ) in records 2 and 3 !
!
!        RangeInfo.CntFactor : Scaling factor for raw data
! 
! 1.06 : -> RAW_HEADER106
!        CapDiam, ModSave, PsdSave removed
!        ModProg increased from char[6] to char[8] 
! --------------------------------------------------------------------------

! this is the identifier for the current version 1.06
      CHARACTER*8 RAW_REVN
      PARAMETER  (RAW_REVN = 'RAW_1.06')

! older versions are mark "RAW_1.05", etc.

! ------------------------------------------------
! MAX_RANGE : maximum number of ranges in one file
! MAX_DATA  : maximum number of points per range
! ------------------------------------------------

      INTEGER    MAX_RANGE
      PARAMETER (MAX_RANGE = 8)
      INTEGER    MAX_DATA
      PARAMETER (MAX_DATA = 16384)

! -----------------------------------------------------------
! Raw Data Header
!
! Note: Before RAW_1.05 char[] were FORTRAN strings which
!       are padded with blanks, they are NOT zero-terminated.
!       for RAW_1.06 see RAW_HEADER106, 
! -----------------------------------------------------------

! JvdS I have used:
!
!  short = INTEGER*2  (16 bits)
!  long  = INTEGER*4  (32 bits)
!  float = REAL*4

      TYPE RAW_HEADER
        CHARACTER*8    Ident       ! file ID & version = RAW_REVN
        CHARACTER*8    ProgName    ! name of creating program
        CHARACTER*16   FileDate    ! creation date : "dd-mon-yy hh:mm"
        CHARACTER*80   Title       ! title
        CHARACTER*192  UserText    ! comment
        INTEGER*2      DifrType    ! diffractometer type
        INTEGER*2      AngResol    ! angular resolution ( steps/degree )
        INTEGER*2      Monochrm    ! monochromator
        INTEGER*2      Anode       ! anode material
        INTEGER*2      Detector    ! type of detector used
        INTEGER*2      M_Dist      ! distance monochromator to tube [mm]
        INTEGER*2      C_Dist      ! distance sample to counter
        INTEGER*2      Tube_kV     ! generator setting ( kV )
        INTEGER*2      Tube_mA     ! generator setting ( mA )
        REAL*4         Wavelen1    ! wavelength ( alpha1 )
        REAL*4         Wavelen2    ! wavelength ( alpha2 )
        REAL*4         DMono       ! d* of monochromator reflection
        INTEGER*2      PSDaddr(4)  ! first & last PSD addresses
        REAL*4         PSDstep(2)  ! PSD stepwidths
        REAL*4         CapDiam     ! diameter of capillary
        REAL*4         SamTrans    ! transmission factor of the sample
        INTEGER*2      ScanMode    ! scan mode ( Transm. / Refl. / Capillary )
        INTEGER*2      ScanType    ! scan type ( 2th:omg / 2th / omg / free )
        INTEGER*2      PsdMode     ! PSD   mode ( stationary / moving )
        INTEGER*2      OmgMode     ! omega mode ( fixed / moving )
        INTEGER*2      ScanUse     ! scan usage
        INTEGER*2      Nadd        ! number of points to add ( PSD only )
        INTEGER*2      Correct     ! raw data correction(s) applied
        INTEGER*2      SeqNum(2)   ! sample number .. of .., when used with
                                   ! sample changer or other serial measurement
        INTEGER*2      NRange      ! number of ranges to be measured
        INTEGER*2      NR_done     ! number of ranges actually measured
        INTEGER*2      NR_curr     ! number of range currently being measured,
                                   ! used as 'measurement in progress' flag
        INTEGER*2      NSample     ! number of samples
        REAL*4         VarOmg(3)   ! Omega( begin, end, step ) for 'VAROMG'
        REAL*4         SPos1(3)    ! SPos1( begin, end, step ) for 'SPOS'
        REAL*4         SPos2(3)    ! SPos2( begin, end, step ) for 'SPOS'
        INTEGER*2      FileNum     ! first file number for serial measurement
        INTEGER*2      NFiles      ! number of files
        INTEGER*2      SameTR      ! Multi-Sample : same title/ranges for all samples ?
        INTEGER*2      TWait       !                waiting time [min]
        INTEGER*2      IFree(6)    ! free space left
        CHARACTER*16   ModDate     ! date of last modification
        CHARACTER*6    ModProg     ! name of modifying program
        INTEGER*2      ModSave     ! not used
        INTEGER*2      PsdSave     ! not used
        INTEGER*4      NTotal      ! total number of data points in the file
        INTEGER*2      DataType    ! type of data in the file
        INTEGER*2      History     ! bit mask showing data modification
                                   ! for all ranges in the file
        INTEGER*2      NDatRec     ! Number of data records
        INTEGER*2      LastRec     ! Total file size in records
        INTEGER*2      NExtra      ! additional records
        INTEGER*2      HeadRec(1:MAX_RANGE) ! Record numbers of range headers ( start at 1 )
        INTEGER*2      DataRec(1:MAX_RANGE) ! Number of data records for each range
      END TYPE ! 512 bytes

      TYPE RAW_HEADER106
        CHARACTER*8    Ident       ! file ID & version = RAW_REVN
        CHARACTER*8    ProgName    ! name of creating program
        CHARACTER*16   FileDate    ! creation date : "dd-mon-yy hh:mm"
        CHARACTER*80   Title       ! title
        CHARACTER*192  UserText    ! comment
        INTEGER*2      DifrType    ! diffractometer type ( = DIF_foo )
        INTEGER*2      AngResol    ! angular resolution ( steps/degree )
        INTEGER*2      Monochrm    ! monochromator ( = MON_foo )
        INTEGER*2      Anode       ! anode material ( = ANO_foo )
        INTEGER*2      Detector    ! type of detector used ( = DET_foo )
        INTEGER*2      M_Dist      ! distance monochromator to tube [mm]
        INTEGER*2      C_Dist      ! distance sample to counter
        INTEGER*2      Tube_kV     ! generator setting ( kV )
        INTEGER*2      Tube_mA     ! generator setting ( mA )
        REAL*4         Wavelen1    ! wavelength alpha1
        REAL*4         Wavelen2    !            alpha2
        REAL*4         DMono       ! d* of monochromator reflection
        INTEGER*2      PSDaddr(4)  ! first & last PSD addresses
        REAL*4         PSDstep(2)  ! PSD stepwidths
        CHARACTER*4    Free1       ! was CapDiam in version < 1.06
        REAL*4         SamTrans    ! transmission factor of the sample ( I/I(o) for Transmission )
                                   ! otherwise mu*T or mu*R
        INTEGER*2      ScanMode    ! scan mode ( = MOD_foo )
        INTEGER*2      ScanType    ! scan type ( = SCT_foo )
        INTEGER*2      PsdMode     ! PSD   mode ( = MOD_PSDfoo )
        INTEGER*2      OmgMode     ! omega mode ( = MOD_OMGfoo )
        INTEGER*2      ScanUse     ! scan usage ( = SCU_foo )
        INTEGER*2      Nadd        ! number of points to add ( PSD only )
        INTEGER*2      Correct     ! raw data correction(s) applied ( = COR_foo )
        INTEGER*2      SeqNum(2)   ! sample number .. of .., when used with
                                   ! sample changer or other serial measurement
        INTEGER*2      NRange      ! number of ranges to be measured
        INTEGER*2      NR_done     ! number of ranges actually measured
        INTEGER*2      NR_curr     ! number of range currently being measured,
                                   ! used as 'measurement in progress' flag
        INTEGER*2      NSample     ! number of samples
        REAL*4         VarOmg(3)   ! Omega( begin, end, step ) for 'VAROMG'
        REAL*4         SPos1(3)    ! SPos1( begin, end, step ) for 'SPOS'
        REAL*4         SPos2(3)    ! SPos2( begin, end, step ) for 'SPOS'
        INTEGER*2      FileNum     ! first file number for serial measurement
        INTEGER*2      NFiles      ! total number of files       
        INTEGER*2      SameTR      ! Multi-Sample : same title/ranges for all samples ?
        INTEGER*2      SizeTR      !                size of datablock ( record 4 )
        INTEGER*2      TWait       !                waiting time [min]
        CHARACTER*16   ModDate     ! date of last modification as dd-mon-yy hh:mm
        CHARACTER*8    ModProg     ! name of modifying program
        CHARACTER*12   Free2
        INTEGER*4      NTotal      ! total number of data points in the file
        INTEGER*2      DataType    ! type of data
        INTEGER*2      History     ! bit mask showing data modification
                                   ! for all ranges in the file
        INTEGER*2      NDatRec     ! number of data records ( incl. header )
        INTEGER*2      LastRec     ! total file size in records         
        INTEGER*2      NExtra      ! number of additional records
        INTEGER*2      HeadRec(MAX_RANGE) ! record numbers of range headers ( start at 1 )
        INTEGER*2      DataRec(MAX_RANGE) ! number of data records for each range
      END TYPE ! 512 byte

! -------------------------------------------------------
! Range header ( for every scan range actually measured )
! -------------------------------------------------------

! background data
      TYPE OLDBGSTUFF
        INTEGER*2 BG_Pos(64)
        INTEGER*4 BG_Int(64)
      END TYPE

! with version 1.05 the number of background points were
! decrease to get room for some enhancements
      TYPE NEWBGSTUFF
        INTEGER*2 BG_Pos(60)
        REAL*4    BG_Smooth
        REAL*4    BG_Factor
        INTEGER*4 BG_Int(60)
        REAL*4    SamPos(4)
      END TYPE

      TYPE RANGE_INFO
        CHARACTER*16 StartTime ! start of CD ( date and time )
        CHARACTER*16 EndTime   ! end   of CD
        INTEGER*2    Status    ! completion status
        INTEGER*2    NPoints   ! number of data points measured
        INTEGER*2    NBackgr   ! number of BG points
        INTEGER*2    DataForm  ! data format : long / short
        REAL*4       CntFactor ! scaling factor ( since RAW_1.05 )
        REAL*4       Begin(2)  ! range begin ( 2theta / omega )
        REAL*4       End(2)    ! range end
        REAL*4       Step(2)   ! stepwidths
        REAL*4       StepTime  ! measuring time per step
        REAL*4       Omega     ! omega position for 'VAROMG'
        REAL*4       SamPos1   ! sample position 1
        REAL*4       SamPos2   ! sample position 2
        REAL*4       T_Avg     ! average temperature of sample
        REAL*4       T_Var     ! temperature variation
        REAL*4       BeamInt   ! primary beam intensity ( for QUANT )
        REAL*4       BeamVar   ! primary beam variation
        REAL*4       DivSlBeg  ! Divergence slit width at scan begin
        REAL*4       DivSlEnd  ! Divergence slit width at scan end
        REAL*4       RecSlit   ! Receiving slit width ( constant )
        REAL*4       SamArea   ! const. Sample area ( > 0 : variable div.slit )
        INTEGER*2    NSamPos   ! number of values used with 'SAMPOS'
        INTEGER*2    History   ! bit mask showing data modifications
        INTEGER*4    MinCount  ! minimum count
        INTEGER*4    MaxCount  ! maximum count
!O      union BACKGROUND   ! union = EQUIVALENCE
!O      {
!O         OLDBGSTUFF background_old ! version 1.05 and below
!O         NEWBGSTUFF background_new ! version 1.06
!O      }
        TYPE(NEWBGSTUFF) BACKGROUND ! We don't need the background stuff
      END TYPE ! 512 bytes

      TYPE(RAW_HEADER) Header
      TYPE(RANGE_INFO) RangeInfo

      INTEGER    RAW_BLOCKSIZE
      PARAMETER (RAW_BLOCKSIZE = 512)

      INTEGER*2 work_I2(RAW_BLOCKSIZE/2)
      INTEGER*4 work_I4(RAW_BLOCKSIZE/4)
      EQUIVALENCE(work_I2(1),work_I4(1))

      REAL*4  factor
      INTEGER nblk, RecNr, k, j, n
      INTEGER I, tFileHandle
      REAL    CurrTwoTheta, Lambda1
!DEC$ END OPTIONS

! Current status, initialise to 'error'
      Load_rawSTOE_File = 1
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 128 (=512 bytes)
      tFileHandle = 11
      OPEN(UNIT=tFileHandle,FILE=TheFileName,ACCESS='DIRECT',RECL=128,FORM='UNFORMATTED',STATUS='OLD',ERR=999)
      READ(UNIT=tFileHandle,REC=1,ERR=999) Header
      IF (Header%NTotal .EQ. 0) THEN
        CALL ErrorMessage('File contains no data.')
        GOTO 999
      ENDIF
      IF (irange .GT. Header%NR_done) THEN
        CALL ErrorMessage('Range has not been measured yet.')
        GOTO 999
      ENDIF
      RecNr = Header%HeadRec(iRange)
      READ(UNIT=tFileHandle,REC=RecNr,ERR=999) RangeInfo
      RecNr = RecNr + 1
      IF (RangeInfo%NPoints .EQ. 0) THEN
        CALL ErrorMessage('Range contains no data.')
        GOTO 999
      ENDIF
      nblk = RAW_BLOCKSIZE / 4
      IF (RangeInfo%DataForm .EQ. SCF_SHORT) nblk = nblk * 2
      factor = 1.0 / MAX(RangeInfo%CntFactor,1.0)
      k = 0
      j = 0
      DO WHILE (k .LT. RangeInfo%NPoints)
        READ(UNIT=tFileHandle,REC=RecNr,ERR=999) work_I4
        RecNr = RecNr + 1
        n = MIN(nblk,RangeInfo%NPoints-k)
        IF (RangeInfo%DataForm .EQ. SCF_SHORT) THEN
          DO i = 1, n
            j = j + 1
            YOBS(j) = factor * FLOAT(work_I2(i))
          ENDDO
        ELSE
          DO i = 1, n
            j = j + 1
            YOBS(j) = factor * FLOAT(work_I4(i))
          ENDDO
        ENDIF
        k = k + nblk
      ENDDO
      Lambda1 = Header%Wavelen1
      CALL Set_Wavelength(Lambda1)
      NOBS = j
! Now fill XOBS
      CurrTwoTheta = RangeInfo%Begin(1)
      DO i = 1, NOBS
        XOBS(i) = CurrTwoTheta
        CurrTwoTheta = CurrTwoTheta + RangeInfo%Step(1)
      ENDDO
      Load_rawSTOE_File = 0
 999  CLOSE(tFileHandle)
! Exit code is error by default, so we can simply return

      END FUNCTION Load_rawSTOE_File
!
!*****************************************************************************
!
