!
!*****************************************************************************
!
      SUBROUTINE SDIFileBrowse
!
! This routine lets the user browse a directory for an SDI file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER(LEN=60) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName

      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|'//&
               'DASH Pawley files (*.sdi)|*.sdi|'
      tFileName = ' '
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open DASH Pawley file',IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under Windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      CALL SDIFileOpen(tFileName)

      END SUBROUTINE SDIFileBrowse
!
!*****************************************************************************
!
      SUBROUTINE SDIFileOpen(TheFileName)
!
! This routine tries to open an SDI file.
!
! INPUT   : TheFileName = the file name
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INCLUDE 'GLBVAR.INC'

      LOGICAL FExists
      INTEGER KLEN

      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        RETURN
      ENDIF
! This is the point of no return: the selected file will be new file, valid data or not
! Change global variable FNAME
      FNAME = TheFileName
      CALL SDIFileLoad(FNAME(1:KLEN)) 
! Next line is necessary due to the way ScrUpdateFileName in SDIFileLoad works
      FNAME = TheFileName
      IF (NoData) THEN
        CALL ErrorMessage("Could not read the project file "//FNAME(1:KLEN)//&
                          CHAR(13)//"successfully.")
        RETURN
      ENDIF
! Disable Pawley refinement button if we are 'PastPawley'
      IF (PastPawley) CALL SetModeMenuState(-1,-1)
      STATBARSTR(1) = FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
! update the file name of the project in the SA pop up
      CALL SetSAFileName(FNAME(1:KLEN))
      
      END SUBROUTINE SDIFileOpen
!
!*****************************************************************************
!
      SUBROUTINE SDIFileLoad(SDIFile)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) ::  SDIFile

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      CHARACTER(LEN = MaxPathLength) :: line

      INTEGER nl
      CHARACTER*12 KeyChar

      INTEGER i
      INTEGER ihcver,ipiker,iloger,idsler, isst, ised
      INTEGER, EXTERNAL :: GetCrystalSystem, GETTIC
      INTEGER tFileHandle
      LOGICAL TicExists
      LOGICAL HcvExists
      LOGICAL PikExists
      LOGICAL DslExists

! JCC Set to success in all cases
      ihcver = 0
      iloger = 0
      ipiker = 0
      idsler = 0
      IF (LEN_TRIM(SDIFile) .GT. MaxPathLength) THEN
        CALL DebugErrorMessage('LEN_TRIM(SDIFile) too long in SDIFileLoad')
      ENDIF
! Now open all the appropriate PIK and HCV files
      tFileHandle = 10
      OPEN(tFileHandle,FILE=SDIFile(1:LEN_TRIM(SDIFile)),STATUS='old',ERR=999)
      CALL sa_SetOutputFiles(SDIFile)
      TicExists = .FALSE.
      HcvExists = .FALSE.
      PikExists = .FALSE.
      DslExists = .FALSE.
 10   line = ' '
      READ(tFileHandle,'(A)',END=100) line
      nl = LEN_TRIM(line)
      CALL ILowerCase(line(:nl))
      CALL INextString(line,keychar)
      SELECT CASE (KeyChar(1:3))
        CASE ('tic')
          CALL ILocateString(line,isst,ised)
          DashTicFile = line(isst:)
          TicExists = .TRUE.
        CASE ('hcv')
          CALL ILocateString(line,isst,ised)
          DashHcvFile = line(isst:)
          HcvExists = .TRUE.
        CASE ('hkl')
          CALL ILocateString(line,isst,ised)
          DashHklFile = line(isst:)
        CASE ('pik')
          CALL ILocateString(line,isst,ised)
          DashPikFile = line(isst:)
          PikExists = .TRUE.
        CASE ('raw')
          CALL ILocateString(line,isst,ised)
          DashRawFile = line(isst:)
        CASE ('dsl')
          CALL ILocateString(line,isst,ised)
          DashDslFile = line(isst:)
          DslExists = .TRUE.
        CASE ('cel') ! Cell parameters
          DO I = 1, 6
            CALL INextReal(line,CellPar(i))
          ENDDO
          CALL Upload_Cell_Constants()
        CASE ('spa')
          CALL INextInteger(line,NumberSGTable)
! Set the crystal system
          LatBrav = GetCrystalSystem(NumberSGTable)
          CALL Upload_CrystalSystem
          CALL FillSymmetry
        CASE ('paw')
          CALL INextReal(line,PAWLEYCHISQ)
      END SELECT
      GOTO 10 
 100  CONTINUE
      CLOSE(tFileHandle)
      IF (DslExists) THEN
        CALL GETDSL(DashDslFile,idsler)
        DslExists = (idsler .EQ. 0)
      ENDIF
      IF (TicExists) THEN
        TicExists = (GETTIC(DashTicFile) .EQ. 0)
        CALL GET_LOGREF
      ENDIF
      IF (HcvExists) THEN
        CALL GETHCV(DashHcvFile,ihcver)
        HcvExists = (ihcver .EQ. 0)
      ENDIF
      IF (PikExists) THEN
        CALL GETPIK(DashPikFile,ipiker)
        PikExists = (ipiker .EQ. 0)
        IF (PikExists) THEN
          FNAME = ''
          CALL ScrUpdateFileName
        ENDIF
      ENDIF
      CALL Clear_PeakFitRanges
      IPTYPE = 1
      CALL Profile_Plot
! enable the buttons,
      IF (.NOT. NoData) THEN
        IF (idsler .EQ. 0) THEN
          CALL SetModeMenuState(0,1)
        ELSE
          CALL SetModeMenuState(0,-1)
        ENDIF
      ENDIF

 999  END SUBROUTINE SDIFileLoad
!
!*****************************************************************************
!
      SUBROUTINE GETDSL(TheFileName,Ierr)
! Routines for reading a 'DSL' file. This file contains
! The additional data that is part of the Winteracter front end: Namely
! radiation type/wavelength etc.

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: Ierr

      CHARACTER*128 line
      CHARACTER*3   KeyChar
      INTEGER       Idum, nl
      REAL          Temp
      INTEGER       Itemp
      INTEGER I, hFile

      Ierr = 0
! Open the file
      hFile = 77
      OPEN (UNIT=hFile, FILE=TheFileName, STATUS='OLD', ERR=999)
! Loop over all records
      DO WHILE ( .TRUE. )
 10     READ(hFile,'(A)',END=100,ERR=999) line
        nl = LEN_TRIM(line)
        CALL ILowerCase(line(:nl))
        CALL INextString(line,keychar)
        SELECT CASE(KeyChar(1:LEN_TRIM(keychar)))
          CASE ('rad')
            I = InfoError(1) ! reset the errors
! Read the wavelength
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL INextInteger(line,itemp)
            IF (InfoError(1) .EQ. 0) THEN
              JRadOption = itemp
            ELSE
! default = X-ray lab data
              JRadOption = 1
            END IF
            CALL Upload_Source
! Now we know all there is to know about the wavelength and source: update it
            CALL Set_Wavelength(Temp)
          CASE ('zer')
! Zero point
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            ZeroPoint = Temp
            CALL Upload_ZeroPoint
          CASE ('sli')
! Pawley SLIM parameter
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            SlimValue = Temp 
            CALL WDialogSelect(IDD_Pawley_Status)
            CALL WDialogPutReal(IDF_Slim_Parameter,Temp,'(F7.3)')
          CASE ('sca')
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            ScalFac = Temp        
        END SELECT
      END DO       
 100  CONTINUE
      BACKREF = .FALSE.
      CLOSE(77)
      RETURN
! Error if we get here
  999 Ierr = 1
      CLOSE(77,IOSTAT=IDUM)

      END SUBROUTINE GETDSL
!
!*****************************************************************************
!
      INTEGER FUNCTION GETTIC(TheFileName)
!
! Reads the tick mark file (h, k, l, 2theta and d* per reflection)
!
! RETURNS 1 for failure
!         0 for success
!
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER iR, I, iLen, hFile

! Initialise to failure
      GETTIC = 1
      hFile = 31
      iLen = LEN_TRIM(TheFileName)
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      NumOfRef = 0
      DO iR = 1, MaxRef
        READ (hFile,*,ERR=999,END=200) (iHKL(I,iR),I=1,3), RefArgK(iR), DSTAR(iR)
        CALL INC(NumOfRef)
      ENDDO
  200 CONTINUE
      CLOSE(hFile)
      GETTIC = 0
 999  RETURN

      END FUNCTION GETTIC
!
!*****************************************************************************
!
      SUBROUTINE GETHCV(TheFileName,ier)

      USE ATMVAR
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: ier

      INCLUDE 'PARAMS.INC'

      CHARACTER*150 LINE
      INTEGER NKKOR(MCHIHS)

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER         IHCOV
      COMMON /CORHES/ IHCOV(30,MaxRef)

      INTEGER KK, I, NLIN, NCOR, iR, II, JJ, IK, MINCOR, KL
      INTEGER, EXTERNAL :: GetNumOfColumns

      OPEN (121,FILE=TheFileName,STATUS='OLD',ERR=998)
      KK = 0
      KKOR = 0
      MINCOR = 20
      IER = 0
      DO iR = 1, MFCSTO
        READ (121,2121,END=100,ERR=998) NLIN, LINE
   2121 FORMAT (Q,A)
        NCOR = GetNumOfColumns(LINE) - 6
        READ (LINE(1:NLIN),*,END=998,ERR=998) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), KL, (IHCOV(I,iR),I=1,NCOR)
        KK = iR
! Now work out which terms should be kept for the chi-squared calculation
        KKOR = KKOR + 1
        IKKOR(KKOR) = iR
        JKKOR(KKOR) = iR
        NKKOR(KKOR) = 100
        DO I = 1, NCOR
          IF (ABS(IHCOV(I,iR)).GE.MINCOR) THEN
            KKOR = KKOR + 1
            IKKOR(KKOR) = iR
            JKKOR(KKOR) = iR + I
            NKKOR(KKOR) = IHCOV(I,iR)
          ENDIF
        ENDDO
      ENDDO
  100 NumOfRef = KK
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        IF (II .EQ. JJ) THEN
          WTIJ(IK) = WTI(II) * WTI(JJ)
        ELSE
          WTIJ(IK) = 0.02*WTI(II)*WTI(JJ)*FLOAT(NKKOR(IK))
        ENDIF
      ENDDO
      GOTO 999
  998 ier = 1
  999 CLOSE (121)

      END SUBROUTINE GETHCV
!
!*****************************************************************************
!
      SUBROUTINE GETPIK(TheFileName,ier)

      USE VARIABLES
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: ier

      INCLUDE 'PARAMS.INC'

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      INTEGER I, J, K
      INTEGER tKNIPT(1:500)
      REAL tPIKVAL(1:500)
      LOGICAL WrongValuesPresent
      INTEGER KTEM

      WrongValuesPresent = .FALSE.
      ier = 0
      OPEN (21,FILE=TheFileName,STATUS='OLD',ERR=998)
      NFITA = 0
      NBIN = 0
      DO I = 1, MOBS
        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM
! JvdS Rather a serious error here, I think. KTEM can be as much as 70.
! Some of the reflections contribute 0.000000E+00 ???
        IF (KTEM .GT. 50) WrongValuesPresent = .TRUE.
        KREFT(I) = MIN(50,KTEM)
        NBIN = NBIN + 1
        WTSA(I) = 1.0/EBIN(I)**2
        IF (KTEM.GT.0) THEN
          READ (21,*,ERR=998) (tKNIPT(K),tPIKVAL(K),K=1,KTEM)
          DO j = 1, MIN(50,KTEM)
            KNIPT(j,I)  = tKNIPT(j)
            PIKVAL(j,I) = tPIKVAL(j)
          ENDDO
          NFITA = NFITA + 1
          IFITA(NFITA) = I
        ENDIF
      ENDDO
  200 CONTINUE
      CLOSE (21)
      CALL Clear_BackGround
! During the SA, only profile points that have peak contributions are calculated (there is no background
! any more at this stage). Therefore, YCBIN must be initialised to zero's
      YCBIN = 0.0
      NoData = .FALSE.
      CALL GetProfileLimits
      IF (WrongValuesPresent) CALL DebugErrorMessage('>50 contributing reflections encountered at least once.')
      RETURN
  998 ier = 1
      CLOSE (21)

      END SUBROUTINE GETPIK
!
!*****************************************************************************
!
