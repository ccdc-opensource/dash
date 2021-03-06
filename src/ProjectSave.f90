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
! Subroutines concerned with the project file
!
!*****************************************************************************
!
      INTEGER FUNCTION PrjSaveAs()
!
! RETURNS 0 for success
!
      USE WINTERACTER
      USE VARIABLES
      USE PRJVAR

      IMPLICIT NONE      

      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFLAGS
      
! Save the project file
      PrjSaveAs = 1 ! Failure
      iFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Project files (*.dash)|*.dash|'
      tFileName = PrjFileName
      CALL WSelectFile(FILTER,iFLAGS,tFileName,'Save project file')
      IF ((WinfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
        PrjFileName = tFileName
        CALL PrjReadWrite(PrjFileName, cWrite)
        CALL sa_SetOutputFiles(PrjFileName)
        PrjSaveAs = 0 ! Yeah, right
      ENDIF

      END FUNCTION PrjSaveAs
!
!*****************************************************************************
!
      SUBROUTINE PrjFileBrowse
!
! This routine lets the user browse a directory for a project file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrapper around the PrjFileOpen routine
! such that it lets the user visually select a file first.
!
      USE VARIABLES
      USE PRJVAR

      IMPLICIT NONE

      CHARACTER(LEN=150) FILTER
      INTEGER           iFlags, iFType 
      CHARACTER(LEN=MaxPathLength) tFileName

      iFlags = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = ALL_FILES_FILTER//&
               'DASH project files (*.dash)|*.dash|'//&
               'DASH 3.0 project files (*.dash30)|*.dash30|'
      tFileName = ''
! iFType specifies which of the file types in the list is the default
      iFType = 2
      tFileName = PrjFileName
      CALL WSelectFile(FILTER, iFlags, tFileName, 'Open DASH project file', iFType)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under Windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      CALL PrjFileOpen(tFileName)

      END SUBROUTINE PrjFileBrowse
!
!*****************************************************************************
!
      SUBROUTINE PrjFileOpen(ThePrjFile)
!
! This routine tries to open a project file.
!
! INPUT   : ThePrjFile = the project file name
!
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ThePrjFile

      LOGICAL FExists
      INTEGER iLen

      iLen = LEN_TRIM(ThePrjFile)
      IF (iLen .EQ. 0) RETURN
      INQUIRE(FILE=ThePrjFile(1:iLen),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//ThePrjFile(1:iLen)//" does not exist.")
        RETURN
      ENDIF
      CALL PrjFileLoad(ThePrjFile) 
      
      END SUBROUTINE PrjFileOpen
!
!*****************************************************************************
!
      SUBROUTINE PrjFileLoad(ThePrjFile)

      USE PRJVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ThePrjFile

      CALL PrjReadWrite(ThePrjFile, cRead)
      CALL sa_SetOutputFiles(ThePrjFile)

      END SUBROUTINE PrjFileLoad
!
!*****************************************************************************
!

      SUBROUTINE PrjReadWrite(ThePrjFile, ReadOrWrite)

      USE PRJVAR

      CHARACTER*(*), INTENT (IN   ) :: ThePrjFile
      INTEGER,       INTENT (IN   ) :: ReadOrWrite

      CHARACTER*(255) DirName, FileName, Extension
      INTEGER ExtLen

      LOGICAL SUCCESS
      LOGICAL, EXTERNAL :: PrjReadWriteImpl

      INTEGER InitialHydrogenTreatment

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      InitialHydrogenTreatment = HydrogenTreatment

      ExtLen = 6
      CALL SplitPath2(ThePrjFile, DirName, FileName, Extension, ExtLen)
      CALL StrUpperCase(Extension)

      IF ( EXTENSION .NE. "DASH30" ) THEN
          SUCCESS = PrjReadWriteImpl(ThePrjFile, ReadOrWrite, .FALSE. )
      ELSE
          SUCCESS = .FALSE.
      ENDIF

      IF ( .NOT. SUCCESS ) THEN
        HydrogenTreatment = InitialHydrogenTreatment
        SUCCESS = PrjReadWriteImpl(ThePrjFile, ReadOrWrite, .TRUE. )    
      ENDIF

      IF ( .NOT. SUCCESS ) THEN
         IF  ( ReadOrWrite .EQ. cRead ) THEN
             CALL ErrorMessage('Error reading project file.')
         ELSE
             CALL ErrorMessage('Error writing project file.')
         ENDIF
      ENDIF

      END SUBROUTINE PrjReadWrite

      LOGICAL FUNCTION PrjReadWriteImpl(ThePrjFile, ReadOrWrite, ForceOldVersion)
!
! This subroutine saves the project file.
!
      USE dash_gui_resources
      USE VARIABLES
      USE PRJVAR
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ThePrjFile
      INTEGER,       INTENT (IN   ) :: ReadOrWrite
      LOGICAL, INTENT (IN   )       :: ForceOldVersion

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(MaxKTem,MOBS), PIKVAL(MaxKTem,MOBS)

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER         ErrCounter
      COMMON /CMN008/ ErrCounter

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      INTEGER, EXTERNAL :: GetCrystalSystem
      INTEGER I, j, RW, MAJOR_IDX, MINOR_IDX, PATCH_IDX, END_IDX
      INTEGER MajorVersion,MinorVersion,PatchVersion
      CHARACTER*(255) tString, csol
      REAL PatchLevel
      REAL, PARAMETER :: TOLER = 1E-6

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode


      BFIOErrorCode = 0
      
      ErrCounter = 0
      CALL PushActiveWindowID
      iPrjReadOrWrite = ReadOrWrite
      RW = iPrjReadOrWrite
      hPrjFile = 10

      PrjReadWriteImpl = .TRUE.
 
      OPEN(UNIT=hPrjFile,FILE=ThePrjFile,ACCESS='DIRECT',RECL=cRECLMult,FORM='UNFORMATTED',ERR=999)
      iPrjRecNr = 1
! Read / Write the header
      tString = TRIM(ProgramVersion)//' project file'
      CALL FileRWString(hPrjFile, iPrjRecNr, RW, tString)
      
      MAJOR_IDX = 6
      END_IDX = LEN(TRIM(tString))
      PATCH_IDX = LEN(TRIM(tString))
      DO I = MAJOR_IDX,LEN(TRIM(tString))
           IF ( tString(I:I) .EQ. '.' ) THEN
              MINOR_IDX = I
              EXIT
           ENDIF
      ENDDO
        
      DO I = MINOR_IDX+1,LEN(TRIM(tString))
           IF ( tString(I:I) .EQ. '.' ) THEN
              PATCH_IDX = I
              EXIT
           ENDIF
      ENDDO
        
      DO I = PATCH_IDX+1,LEN(TRIM(tString))
           IF ( tString(I:I) .EQ. ' ' ) THEN
              END_IDX = I
              EXIT
           ENDIF
      ENDDO        

      READ (tString(MAJOR_IDX:MINOR_IDX-1),'(I)', ERR=999) MajorVersion
      READ (tString(MINOR_IDX+1:PATCH_IDX-1),'(I)', ERR=999) MinorVersion    
      IF ( PATCH_IDX .NE. LEN(TRIM(tString)) ) THEN
            READ (tString(PATCH_IDX+1:END_IDX-1),'(I)', ERR=999) PatchVersion            
      ELSE
            PatchVersion = 0
      ENDIF
      
      IF ( BFIOErrorCode .EQ. 1 ) GOTO 999
      ! If read, store program version for later reference

! Read / Write radiation source
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, JRadOption)
! Read / Write Wavelength
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, ALambda)
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) THEN
        CALL Upload_Source
        CALL Upload_Wavelength
      ENDIF
! Read / Write unit cell
      DO I = 1, 6
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, CellPar(I))
      ENDDO
! Read / Write zero-point
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, ZeroPoint)
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) &
        CALL Upload_ZeroPoint
! Read / Write space group
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumberSGTable)
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) THEN
        LatBrav = GetCrystalSystem(NumberSGTable)
        CALL Upload_CrystalSystem
      ENDIF
      CALL PrjErrTrace
! Read / Write Pawley refinement related stuff
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PAWLEYCHISQ)
      CALL FileRWLogical(hPrjFile, iPrjRecNr, RW, Is_SX)
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PeakShapeSigma(1))
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PeakShapeSigma(2))
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PeakShapeGamma(1))
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PeakShapeGamma(2))
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PeakShapeHPSL)
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PeakShapeHMSL)
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, SlimValue)
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, ScalFac)
! RAW .\Example.xye ! @@@@ include ??
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NBIN)
! Read / Write the .pik file
!            WRITE (IPK,*) ARGI, OBS - YBACK, DOBS, NTEM
!        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) CALL WizardWindowShow(IDD_SAW_Page5)
! Read / Write observed pattern minus the background fitted during the Pawley refinement.
! This is the observed pattern read in by GETPIK.
      CALL PrjErrTrace
      DO I = 1, NBIN
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, XBIN(I))
      ENDDO
      CALL PrjErrTrace
      DO I = 1, NBIN
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, YOBIN(I))
      ENDDO
      CALL PrjErrTrace
      DO I = 1, NBIN
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, EBIN(I))
      ENDDO
      IF (RW .EQ. cRead) THEN
        LBIN = 1
        XOBS = 0.0
        YOBS = 0.0
        EOBS = 0.0
        NOBS = NBIN
        BackupXOBS = 0.0
        BackupYOBS = 0.0
        BackupEOBS = 0.0
        BackupNOBS = NBIN
        DO I = 1, NBIN
          BackupXOBS(I) = XBIN(I)
          BackupYOBS(I) = YOBIN(I)
          BackupEOBS(I) = EBIN(I)
          XOBS(I)       = XBIN(I)
          YOBS(I)       = YOBIN(I)
          EOBS(I)       = EBIN(I)
        ENDDO
      ENDIF
      IF (RW .EQ. cRead) THEN
        DO I = 1, NBIN
          WTSA(I) = 1.0 / EBIN(I)**2
        ENDDO
      ENDIF
      NFITA = 0
      DO I = 1, NBIN
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, KREFT(I))
        IF (KREFT(I).GT.0) THEN
          DO j = 1, KREFT(I)
            CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, KNIPT(j,I))
            CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PIKVAL(j,I))
          ENDDO
          NFITA = NFITA + 1
          IFITA(NFITA) = I
        ENDIF
      ENDDO
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) THEN
        NoData = .FALSE.
        CALL GetProfileLimits
        CALL Get_IPMaxMin 
        CALL Update_TruncationLimits
        IPTYPE = 1
! Calculate tick marks
        PastPawley = .FALSE.
        CALL Generate_TicMarks
        PastPawley = .TRUE.
        CALL Profile_Plot
      ENDIF
      CALL PrjErrTrace
      CALL PrjReadWriteIntensities
      CALL PrjErrTrace
! Read / Write the Z-matrices
! Must read/write hydrogen treatment first, because that is necessary to
! calculate the atomic weightings. This wasn't written out in version 3.0, so need
! to add a check.
      IF ( (RW .EQ. cWrite) .OR. (MajorVersion .GT. 3 ) .OR. (MajorVersion .EQ. 3 .AND. MinorVersion .GT. 0) ) THEN
        IF ( .NOT. ForceOldVersion ) THEN
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, HydrogenTreatment)
          IF ( BFIOErrorCode .EQ. 1 ) GOTO 999
        ENDIF
      ENDIF

      CALL PrjReadWriteZmatrices
      IF ( BFIOErrorCode .EQ. 1 ) GOTO 999

      CALL PrjErrTrace
! Read / Write Preferred Orientation
      CALL PrjReadWritePO
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) THEN
        CALL FillSymmetry_2
        CALL GET_LOGREF
        CALL MakRHm
      ENDIF
      CALL PrjErrTrace
      IF (RW .EQ. cRead) THEN
        CALL InitSADistRestraint
! Update ranges and fixed yes/no per parameter
        CALL SA_Parameter_Set
      ENDIF
! Read / Write MDB and SA Distance restraints. Version 3.2 onward
      IF ( (RW .EQ. cWrite) .OR. (MajorVersion .GT. 3) .OR. (MajorVersion .EQ. 3 .AND. MinorVersion .GE. 2)  ) THEN
        CALL PrjReadWriteParameterBoundsIncludeMDB
        IF ( BFIOErrorCode .EQ. 1 ) GOTO 999
        CALL PrjErrTrace
        CALL PrjReadWriteSADistRestraint
        IF ( BFIOErrorCode .EQ. 1 ) GOTO 999
        CALL PrjErrTrace
      ENDIF
! Read / Write solutions
      CALL PrjReadWriteSolutions
      CALL PrjErrTrace
      IF ( (RW .EQ. cRead) .AND. (.NOT. in_batch) ) THEN
! Grey out the "Save... chi sqrd progress"
        CALL SelectDASHDialog(IDD_OutputSolutions)
        CALL WDialogFieldState(IDF_GROUP2, Disabled)
        CALL WDialogFieldState(IDB_OutputChiSqd, Disabled)
        CALL WDialogFieldState(IDF_LABEL5, Disabled)
        CALL WDialogFieldState(IDF_LABEL3, Disabled)
        CALL CloseOutputSolutionsChildWindows
        CALL SelectDASHDialog(IDD_SAW_Page5)
        CALL WDialogFieldState(IDB_Prog3,Disabled)
        CALL SelectDASHDialog(IDD_ViewPawley)
        CALL WDialogPutReal(IDF_Sigma1, PeakShapeSigma(1), '(F10.4)')
        CALL WDialogPutReal(IDF_Sigma2, PeakShapeSigma(2), '(F10.4)')
        CALL WDialogPutReal(IDF_Gamma1, PeakShapeGamma(1), '(F10.4)')
        CALL WDialogPutReal(IDF_Gamma2, PeakShapeGamma(2), '(F10.4)')
        CALL WDialogPutReal(IDF_HPSL,   PeakShapeHPSL,     '(F10.4)')
        CALL WDialogPutReal(IDF_HMSL,   PeakShapeHMSL,     '(F10.4)')
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts, NBIN)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs, NumOfRef)
        CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq, PAWLEYCHISQ, '(F12.3)')
! Grey out the "Previous Results >" button in the DICVOL Wizard window
        CALL SelectDASHDialog(IDD_PW_Page8)
        CALL WDialogFieldState(IDB_PrevRes, Disabled)
        BackRef = .FALSE.
        CALL SetModeMenuState(0,1)
! Change global variable FNAME
        FNAME = '' ! clear diffraction file entry
! Update this throughout the program (Wizard + status bar)
        CALL ScrUpdateFileName
        FNAME = ThePrjFile
! Update the status bar at the bottom of the screen.
         CALL WindowOutStatusBar(1, FNAME)
      ENDIF

      CLOSE(hPrjFile)
      CALL PopActiveWindowID
      RETURN

  999 CLOSE(hPrjFile)
      CALL PopActiveWindowID
      
      PrjReadWriteImpl = .FALSE.

      RETURN

      END FUNCTION PrjReadWriteImpl
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteSolutions
!
! Read or writes information on solutions to / from binary project file.
!
      USE PRJVAR
      USE SOLVAR
      USE ATMVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      LOGICAL, EXTERNAL :: Get_AutoAlign
      INTEGER ii, I, J, RW
      REAL X(MVAR)

! Read or Write?
      RW = iPrjReadOrWrite
! Number of solutions
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumOf_SA_Runs)
! Read / Write solutions
      IF (NumOf_SA_Runs .NE. 0) THEN
        DO I = 1, NumOf_SA_Runs
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iSol2Run(I))
          DO J = 1, nvar
            CALL FileRWReal(hPrjFile, iPrjRecNr, RW, BestValuesDoF(J,I))
            X(J) = BestValuesDoF(J,I)
          ENDDO
          CALL FileRWReal(hPrjFile, iPrjRecNr, RW, ProfileChiSqd(I))
          CALL FileRWReal(hPrjFile, iPrjRecNr, RW, IntensityChiSqd(I))
          IF (iPrjReadOrWrite .EQ. cRead) THEN
! Fill XAtmCoords
            CALL makefrac(X)
! Fractional co-ordinates are now in Xato.
            DO ii = 1, NATOM
              XAtmCoords(1,ii,I) = Xato(1,ii)
              XAtmCoords(2,ii,I) = Xato(2,ii)
              XAtmCoords(3,ii,I) = Xato(3,ii)
            ENDDO
! Re-align if so asked for by user
            IF (Get_AutoAlign()) THEN
              Curr_SA_Run = I ! Used by Align()
              CALL Align
            ENDIF
          ENDIF
        ENDDO
        IF (iPrjReadOrWrite .EQ. cRead) THEN
          iSolTicked = 1
          CALL Update_Solutions
        ENDIF
      ENDIF

      END SUBROUTINE PrjReadWriteSolutions
!
!*****************************************************************************
!
      SUBROUTINE PrjErrTrace

      USE PRJVAR

      IMPLICIT NONE

      INTEGER         ErrCounter
      COMMON /CMN008/ ErrCounter

      CHARACTER*20, EXTERNAL :: Integer2String
      INTEGER tInteger
      
      tInteger = 10101
      CALL FileRWInteger(hPrjFile,iPrjRecNr,iPrjReadOrWrite,tInteger)
      ErrCounter = ErrCounter + 1
      IF ((iPrjReadOrWrite .EQ. cRead) .AND. (tInteger .NE. 10101))  &
         CALL DebugErrorMessage('Prj read error at '//Integer2String(ErrCounter))

      END SUBROUTINE PrjErrTrace
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteIntensities
!
! Reads or writes information on intensities to / from binary project file.
!
      USE PRJVAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER         IHCOV
      COMMON /CORHES/ IHCOV(30,MaxRef)

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER I, RW, iR, NCOR
      INTEGER NKKOR(MCHIHS)
      INTEGER MINCOR, IK, II, JJ

! Read or Write?
      RW = iPrjReadOrWrite
      KKOR = 0
      MINCOR = 20
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumOfRef)
      DO iR = 1, NumOfRef
! Observed intensity
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, AIOBS(iR))
! Weight of this intensity
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, WTI(iR))
        IF (RW .EQ. cWrite) THEN
! Determine number of correlations to write out
          NCOR = 14
          DO WHILE ( NCOR .GE. 1 ) 
            IF ( IHCOV(NCOR,iR) .NE. 0 ) GOTO 100
            NCOR = NCOR - 1
          ENDDO
 100      CONTINUE
        ENDIF
! Number of correlations
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NCOR)
        IF (NCOR .GT. 0) THEN
          DO I = 1, NCOR
            CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, IHCOV(I,iR))
          ENDDO
        ENDIF
! Now work out which terms should be kept for the chi-squared calculation
        KKOR = KKOR + 1
        IKKOR(KKOR) = iR
        JKKOR(KKOR) = iR
        NKKOR(KKOR) = 100
        IF (NCOR .GT. 0) THEN
          DO I = 1, NCOR
            IF (ABS(IHCOV(I,iR)).GE.MINCOR) THEN
              KKOR = KKOR + 1
              IKKOR(KKOR) = iR
              JKKOR(KKOR) = iR + I
              NKKOR(KKOR) = IHCOV(I,iR)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        IF (II .EQ. JJ) THEN
          WTIJ(IK) = WTI(II) * WTI(JJ)
        ELSE
          WTIJ(IK) = 0.02*WTI(II)*WTI(JJ)*FLOAT(NKKOR(IK))
        ENDIF
      ENDDO

      END SUBROUTINE PrjReadWriteIntensities
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWritePO
!
! Reads or writes information on preferred orientation to / from binary project file.
!
      USE PRJVAR
      USE PO_VAR

      IMPLICIT NONE

      INTEGER i, RW

! Read or Write?
      RW = iPrjReadOrWrite
      CALL FileRWLogical(hPrjFile, iPrjRecNr, RW, PrefParExists)
! Read/write preferred orientation parameters anyway, to enable the user
! to switch PO on and off without having to re-enter direction etc.
      DO i = 1, 3
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, PO_Direction(i))
      ENDDO
! Update the appropriate Wizard window
      IF (iPrjReadOrWrite .EQ. cRead) CALL Update_PO

      END SUBROUTINE PrjReadWritePO
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteZmatrices
!
! Reads or writes information on Z-matrices to / from binary project file.
!
      USE PRJVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      INTEGER, EXTERNAL :: ElmSymbol2CSD
      INTEGER iFrg, RW, iAtomNr, BondNr


      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      LOGICAL, EXTERNAL :: FileErrorOccurred


! Read or Write?
      RW = iPrjReadOrWrite
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, nfrag)
      IF ( RW .EQ. cRead .AND. nFrag .GT. maxfrg ) THEN
          BFIOErrorCode = 1
          RETURN
      ENDIF

      DO iFrg = 1, nFrag
        CALL FileRWString (hPrjFile, iPrjRecNr, RW, frag_file(iFrg))
        IF ( FileErrorOccurred() ) RETURN
! The path may come from a file created in other machine or platform!
        IF ( RW .EQ. cRead ) &
          CALL StripPathIfInvalid(frag_file(iFrg))

        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, izmpar(iFrg))
        IF ( FileErrorOccurred() ) RETURN

        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, icomflg(iFrg))
        IF ( FileErrorOccurred() ) RETURN

        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, natoms(iFrg))
        IF ( FileErrorOccurred() ) RETURN

        DO iAtomNr = 1, natoms(iFrg)
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, ioptb(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iopta(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, ioptt(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iz1(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iz2(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iz3(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, blen(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, alph(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, bet(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWString (hPrjFile, iPrjRecNr, RW, ElSym(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          IF (RW .EQ. cRead) THEN
            zmElementCSD(iAtomNr,iFrg) = ElmSymbol2CSD(ElSym(iAtomNr,iFrg))
          ENDIF
          CALL FileRWString (hPrjFile, iPrjRecNr, RW, OriginalLabel(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, tiso(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, occ(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, izmoid(iAtomNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          izmbid(izmoid(iAtomNr,iFrg),iFrg) = iAtomNr ! the back mapping
          CALL FileRWLogical(hPrjFile, iPrjRecNr, RW, UseQuaternions(iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxDef(iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxAtm(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxAtm(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotAxFrac(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotAxFrac(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotAxFrac(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxPlnAtm(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxPlnAtm(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxPlnAtm(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(0,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrDef(iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrFrac(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrFrac(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrFrac(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrEuler(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrEuler(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrEuler(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(0,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(0,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(1,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(2,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(3,iFrg))
          IF ( FileErrorOccurred() ) RETURN

        ENDDO
        IF (RW .EQ. cRead) THEN
          CALL zmDoAdmin(iFrg)
        ENDIF
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumberOfBonds(iFrg))
        IF ( FileErrorOccurred() ) RETURN

        DO BondNr = 1, NumberOfBonds(iFrg)
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, BondType(BondNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, Bonds(1,BondNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN

          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, Bonds(2,BondNr,iFrg))
          IF ( FileErrorOccurred() ) RETURN
        ENDDO
      ENDDO
      IF (RW .EQ. cRead) THEN 
        CALL UpdateZmatrixSelection
        CALL Create_AtomicWeightings(HydrogenTreatment)
      ENDIF

      END SUBROUTINE PrjReadWriteZmatrices
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteParameterBoundsIncludeMDB
!
! Reads or writes information on MDB to / from binary project file.
!
      USE PRJVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                                          iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      INTEGER NumMogulBins(MVAR), MogulBins(MaxMogulBin, MVAR)
      REAL MogulDistributions(-180:180, MVAR)
      COMMON /MDB/ NumMogulBins, MogulBins, MogulDistributions

      LOGICAL, EXTERNAL :: FileErrorOccurred

      INTEGER I, J, RW
      
! Read or Write?
      RW = iPrjReadOrWrite
      DO I = 1, nvar
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, ModalFlag(I))
        CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, lb(I))
        CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, ub(I))
        IF ( FileErrorOccurred() ) EXIT
        IF (ModalFlag(I) .NE. 4) CYCLE
        DO J = -180, 180
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, MogulDistributions(J, I))
        ENDDO
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumMogulBins(I))
        DO J = 1, MaxMogulBin
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, MogulBins(J, I))
        ENDDO
      ENDDO

      END SUBROUTINE PrjReadWriteParameterBoundsIncludeMDB
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteSADistRestraint
!
! Reads or writes information on SA distace restraint to / from binary project file.
!
      USE PRJVAR

      IMPLICIT NONE
 
      INCLUDE 'SA_restrain.inc'

      LOGICAL, EXTERNAL :: FileErrorOccurred

      INTEGER I, RW
      
! Read or Write?
      RW = iPrjReadOrWrite
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, DRestrNumb)
      IF ( FileErrorOccurred() ) RETURN
      DO I = 1, DRestrNumb
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, DRestrAtomIDs(1, I))
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, DRestrAtomIDs(2, I))
        CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, DRestrLens(I))
        CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, DRestrWidths(I))
        CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, DRestrWeights(I))
        IF ( FileErrorOccurred() ) EXIT
      ENDDO
      
      END SUBROUTINE PrjReadWriteSADistRestraint
!
!*****************************************************************************
!
