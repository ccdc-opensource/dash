! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
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

      IMPLICIT NONE

      CHARACTER(LEN=60) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName
      LOGICAL,EXTERNAL:: SDIFileOpen
      LOGICAL Dummy

      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = ALL_FILES_FILTER//&
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
      Dummy = SDIFileOpen(tFileName)

      END SUBROUTINE SDIFileBrowse
!
!*****************************************************************************
!
      LOGICAL FUNCTION SDIFileOpen(TheFileName)
!
! This routine tries to open an SDI file.
!
! INPUT   : TheFileName = the file name
!
! RETURNS : TRUE for success
!           FALSE for error

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      LOGICAL, EXTERNAL :: SDIFileLoad

      LOGICAL FExists, tStat
      INTEGER KLEN

      ! Initialise to failure
      SDIFileOpen = .FALSE.
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
      tStat = SDIFileLoad(FNAME(1:KLEN))
! Next line is necessary due to the way ScrUpdateFileName in SDIFileLoad works
      FNAME = TheFileName
      IF ((.NOT. tStat) .OR. NoData) THEN
        CALL ErrorMessage("Could not read the project file "//FNAME(1:KLEN)//&
                          CHAR(13)//"successfully.")
        RETURN
      ENDIF
! Disable Pawley refinement button if we are 'PastPawley'
      IF (PastPawley) CALL SetModeMenuState(-1,-1)
      CALL WindowOutStatusBar(1,FNAME)
! Update the file name of the project in the SA pop up
      CALL ScrUpdateFileNameSDIFile(FNAME(1:KLEN))
      SDIFileOpen = .TRUE.
      
      END FUNCTION SDIFileOpen
!
!*****************************************************************************
!
      LOGICAL FUNCTION SDIFileLoad(SDIFile)

! RETURNS : TRUE for success
!           FALSE for error

      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) ::  SDIFile

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'Reflns.inc'

      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      REAL WLGTH
      EQUIVALENCE (WLGTH,ALAMBD(1,1))

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      INTEGER, EXTERNAL :: GetCrystalSystem, GETTIC
      CHARACTER(LEN = MaxPathLength) :: line
      INTEGER nl
      CHARACTER*12 KeyChar
      INTEGER i
      INTEGER ihcver,ipiker,iloger,idsler
      INTEGER iHandle
      LOGICAL TicExists
      LOGICAL HcvExists
      LOGICAL PikExists
      LOGICAL DslExists
 
      LOGICAL ELECDI
      COMMON /USEELE / ELECDI
      
      ELECDI = .FALSE.

      ! Initialise to failure
      SDIFileLoad = .FALSE.
! Set to success in all cases
      ihcver = 0
      iloger = 0
      ipiker = 0
      idsler = 0
! Now open all the appropriate PIK and HCV files
      iHandle = 10
      OPEN(iHandle,FILE=SDIFile(1:LEN_TRIM(SDIFile)),STATUS='old',ERR=999)
      CALL sa_SetOutputFiles(SDIFile)
      TicExists = .FALSE.
      HcvExists = .FALSE.
      PikExists = .FALSE.
      DslExists = .FALSE.
      CALL Clear_PeakFitRanges
      Is_SX = .FALSE.
 10   line = ' '
      READ(iHandle,'(A)',END=100,ERR=999) line
      nl = LEN_TRIM(line)
      CALL INextString(line,keychar)
      CALL ILowerCase(keychar)
      SELECT CASE (KeyChar(1:3))
        CASE ('tic')
          DashTicFile = line(ILocateChar(line):)
          TicExists = .TRUE.
          CALL StripPathIfInvalid(DashTicFile)
        CASE ('hcv')
          DashHcvFile = line(ILocateChar(line):)
          HcvExists = .TRUE.
          CALL StripPathIfInvalid(DashHcvFile)
        CASE ('pik')
          DashPikFile = line(ILocateChar(line):)
          PikExists = .TRUE.
          CALL StripPathIfInvalid(DashPikFile)
        CASE ('raw')
          DashRawFile = line(ILocateChar(line):)
          CALL StripPathIfInvalid(DashRawFile)
        CASE ('dsl')
          DashDslFile = line(ILocateChar(line):)
          DslExists = .TRUE.
          CALL StripPathIfInvalid(DashDslFile)
        CASE ('cel') ! Cell parameters
          DO I = 1, 6
            CALL INextReal(line,CellPar(i))
          ENDDO
          CALL Upload_Cell_Constants
        CASE ('spa')
          CALL INextInteger(line,NumberSGTable)
! Set the crystal system
          LatBrav = GetCrystalSystem(NumberSGTable)
! Update cpdbops, required by special_position.exe for most external Rietveld programs 
! Call FillSymmetry_2() instead and move to DealWithWizardRietveldRefinement()
!          CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
          CALL Upload_CrystalSystem
        CASE ('paw')
          CALL INextReal(line, PAWLEYCHISQ)
        CASE ('sin') ! Single crystal
          Is_SX = .TRUE.
        CASE ('ele') ! Electron diffraction
          ELECDI = .TRUE.
      END SELECT
      GOTO 10 
 100  CLOSE(iHandle)
      IF (DslExists) THEN
        CALL GETDSL(DashDslFile, idsler)
        DslExists = (idsler .EQ. 0)
      ENDIF
      IF (TicExists) THEN
        TicExists = (GETTIC(DashTicFile) .EQ. 0)
      ENDIF
      IF (HcvExists) THEN
        CALL GETHCV(DashHcvFile, ihcver)
        HcvExists = (ihcver .EQ. 0)
      ENDIF
      IF (PikExists) THEN
        CALL GETPIK(DashPikFile, ipiker)
        PikExists = (ipiker .EQ. 0)
        IF (PikExists) THEN
          FNAME = ''
          CALL ScrUpdateFileName
        ENDIF
      ENDIF
      IPTYPE = 1
      CALL Profile_Plot
! Enable the buttons,
      IF ( .NOT. IN_BATCH ) THEN
        IF (.NOT. NoData) THEN
          IF (idsler .EQ. 0) THEN
            CALL SetModeMenuState(0,1)
          ELSE
            CALL SetModeMenuState(0,-1)
          ENDIF
        ENDIF
        CALL SelectDASHDialog(IDD_ViewPawley)
        CALL WDialogPutReal(IDF_Sigma1, PeakShapeSigma(1), '(F10.4)')
        CALL WDialogPutReal(IDF_Sigma2, PeakShapeSigma(2), '(F10.4)')
        CALL WDialogPutReal(IDF_Gamma1, PeakShapeGamma(1), '(F10.4)')
        CALL WDialogPutReal(IDF_Gamma2, PeakShapeGamma(2), '(F10.4)')
        CALL WDialogPutReal(IDF_HPSL,   PeakShapeHPSL,     '(F10.4)')
        CALL WDialogPutReal(IDF_HMSL,   PeakShapeHMSL,     '(F10.4)')
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts, NOBS)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs, NumOfRef)
      ENDIF

      MAXK = NumOfRef
      WLGTH = ALambda
      
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq, PAWLEYCHISQ, '(F12.3)')

      CALL Clear_SA
! Grey out the "Previous Results >" button in the DICVOL Wizard window
      IF ( .NOT. IN_BATCH ) THEN
        CALL SelectDASHDialog(IDD_PW_Page8)
        CALL WDialogFieldState(IDB_PrevRes, Disabled)
      ENDIF

      DefaultMaxResolution = 0.1 ! Force using maximum attainable
      CALL Update_TruncationLimits
      CALL GET_LOGREF
      SDIFileLoad = .TRUE.
      RETURN
 999  CALL ErrorMessage('Error reading .sdi file.')
      CLOSE(iHandle) 

      END FUNCTION SDIFileLoad
!
!*****************************************************************************
!
      SUBROUTINE GETDSL(TheFileName, iErr)
! Routines for reading a 'DSL' file. This file contains
! The additional data that is part of the Winteracter front end: Namely
! radiation type/wavelength etc.

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: iErr

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

      CHARACTER*128 line
      CHARACTER*3   KeyChar
      REAL          Temp
      INTEGER       nl, iTem, I, hFile

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      iErr = 1
! Open the file
      hFile = 77
      OPEN (UNIT=hFile, FILE=TheFileName, STATUS='OLD', ERR=999)
! Loop over all records
      DO WHILE ( .TRUE. )
        READ(hFile,'(A)',END=100,ERR=999) line
        nl = LEN_TRIM(line)
        CALL ILowerCase(line(:nl))
        CALL INextString(line,keychar)
        SELECT CASE(KeyChar(1:LEN_TRIM(keychar)))
          CASE ('rad')
            I = InfoError(1) ! reset the errors
! Read the wavelength
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL INextInteger(line,iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            JRadOption = iTem
            CALL Upload_Source
! Now we know all there is to know about the wavelength and source: update it
            CALL Set_Wavelength(Temp)
          CASE ('sig') ! Sigma
            I = InfoError(1) ! reset the errors
! Sigma 1
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            PeakShapeSigma(1) = Temp
            CALL INextReal(line,Temp) ! ESD
            IF (InfoError(1) .NE. 0) GOTO 999  
! Sigma 2
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            PeakShapeSigma(2) = Temp
          CASE ('gam') ! Gamma
            I = InfoError(1) ! reset the errors
! Gamma 1
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            PeakShapeGamma(1) =  Temp
            CALL INextReal(line,Temp) ! ESD
            IF (InfoError(1) .NE. 0) GOTO 999  
! Gamma 2
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            PeakShapeGamma(2) = Temp
          CASE ('asy') ! HMSL/HPSL asymmetry parameters
            I = InfoError(1) ! reset the errors
! HPSL
            CALL INextReal(line,Temp) ! HPSL
            IF (InfoError(1) .NE. 0) GOTO 999                          
            PeakShapeHPSL = Temp
            IF ( .NOT. IN_BATCH ) THEN
              CALL SelectDASHDialog(IDD_HPSL_info)
              CALL WDialogPutReal(IDF_HPSL, PeakShapeHPSL, '(F10.4)')
            ENDIF
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp) ! ESD
            IF (InfoError(1) .NE. 0) GOTO 999  
! HMSL
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            PeakShapeHMSL = Temp
            IF ( .NOT. IN_BATCH ) THEN
              CALL SelectDASHDialog(IDD_HMSL_info)
              CALL WDialogPutReal(IDF_HMSL, PeakShapeHMSL, '(F10.4)')
            ENDIF
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
            IF ( .NOT. IN_BATCH ) THEN
              CALL SelectDASHDialog(IDD_Pawley_Status)
              CALL WDialogPutReal(IDF_Slim_Parameter,Temp,'(F7.3)')
            ENDIF
          CASE ('sca')
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            ScalFac = Temp        
        END SELECT
      ENDDO       
 100  BackRef = .FALSE.
      iErr = 0
  999 CLOSE(hFile)

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

      INTEGER iR, I, hFile

! Initialise to failure
      GETTIC = 1
      hFile = 31
      OPEN(UNIT=hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      NumOfRef = 0
      DO iR = 1, MaxRef
        READ (hFile,*,ERR=999,END=200) (iHKL(I,iR),I=1,3), RefArgK(iR), DSTAR(iR)
        CALL INC(NumOfRef)
      ENDDO
  200 CONTINUE
      GETTIC = 0
 999  CLOSE(hFile)

      END FUNCTION GETTIC
!
!*****************************************************************************
!
      SUBROUTINE GETHCV(TheFileName,iErr)

      USE ATMVAR
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: iErr

      INCLUDE 'params.inc'

      CHARACTER*255 LINE
      INTEGER NKKOR(MCHIHS)

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER         IHCOV
      COMMON /CORHES/ IHCOV(30,MaxRef)

      INTEGER, EXTERNAL :: GetNumOfColumns
      INTEGER KK, I, NLIN, NCOR, iR, II, JJ, IK, MINCOR, KL
      INTEGER hFile

      iErr = 1
      hFile = 121
      OPEN(hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      KK = 0
      KKOR = 0
      MINCOR = 20
      DO iR = 1, MFCSTO
        READ(hFile,'(A)',END=100,ERR=999) LINE
        NLIN = LEN_TRIM(LINE)
        NCOR = GetNumOfColumns(LINE) - 6
        READ(LINE(1:NLIN),*,END=999,ERR=999) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), KL, (IHCOV(I,iR),I=1,NCOR)
        KK = iR
! Now work out which terms should be kept for the chi-squared calculation
        KKOR = KKOR + 1
        IKKOR(KKOR) = iR
        JKKOR(KKOR) = iR
        NKKOR(KKOR) = 100
        DO I = 1, NCOR
          IF (ABS(IHCOV(I,iR)) .GE. MINCOR) THEN
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
      iErr = 0
  999 CLOSE(hFile)

      END SUBROUTINE GETHCV
!
!*****************************************************************************
!
      SUBROUTINE GETPIK(TheFileName, iErr)

      USE VARIABLES
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (  OUT) :: iErr

      INCLUDE 'params.inc'

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(MaxKTem,MOBS), PIKVAL(MaxKTem,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      INTEGER I, J, K
      INTEGER tKNIPT(1:500)
      REAL tPIKVAL(1:500)
      LOGICAL WrongValuesPresent
      INTEGER KTEM, hFile
      REAL EOBSSQ
      iErr = 1
      hFile = 21
      OPEN (hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      WrongValuesPresent = .FALSE.
      NFITA = 0
      NOBS = 0
      DO I = 1, MOBS
        READ (hFile,*,END=200,ERR=999) XOBS(I), YOBS(I), EOBS(I), KTEM
! JvdS Rather a serious error here, I think. KTEM can be as much as 70.
! Some of the reflections contribute 0.000000E+00 ???
        IF (KTEM .GT. MaxKTem) WrongValuesPresent = .TRUE.
        KREFT(I) = MIN(MaxKTem,KTEM)
        NOBS = NOBS + 1

! JCC Another error: it seems that EOBS can be zero,
!
        EOBSSQ = EOBS(I)**2
        IF ( EOBSSQ .LT. 0.0000001 ) THEN
          EOBSSQ = 0.0000001
        ENDIF

        WTSA(I) = 1.0/EOBSSQ
        IF (KTEM .GT. 0) THEN
          READ (hFile,*,ERR=999) (tKNIPT(K),tPIKVAL(K),K=1,KTEM)
          DO j = 1, MIN(MaxPik,KTEM)
            KNIPT(j,I)  = tKNIPT(j)
            PIKVAL(j,I) = tPIKVAL(j)
          ENDDO
          NFITA = NFITA + 1
          IFITA(NFITA) = I
        ENDIF
      ENDDO
  200 CONTINUE
      BackupXOBS = 0.0
      BackupYOBS = 0.0
      BackupEOBS = 0.0
      BackupNOBS = NOBS
      DO I = 1, NOBS
        BackupXOBS(I) = XOBS(I)
        BackupYOBS(I) = YOBS(I)
        BackupEOBS(I) = EOBS(I)
      ENDDO
      CALL Clear_BackGround
      NoData = .FALSE.
      CALL Clear_Bins
      CALL Rebin_Profile
      IF (WrongValuesPresent) CALL DebugErrorMessage('>50 contributing reflections encountered at least once.')
      iErr = 0
  999 CLOSE (hFile)

      END SUBROUTINE GETPIK
!
!*****************************************************************************
!
