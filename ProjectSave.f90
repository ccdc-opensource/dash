! Subroutines concerned with the project file
!
!*****************************************************************************
!
      INTEGER FUNCTION PrjSaveAs

! A wrapper around PrjSave to let the user choose the filename.
!
! RETURNS 0 for success
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PRJVAR

      IMPLICIT NONE      

      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFLAGS
      INTEGER, EXTERNAL :: PrjSave
      
! Save the project file
      PrjSaveAs = 1 ! Failure
      iFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Project files (*.dash)|*.dash|'
      tFileName = PrjFileName
      CALL WSelectFile(FILTER,iFLAGS,tFileName,'Save project file')
      IF ((WinfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
        PrjFileName = tFileName
        PrjSaveAs = PrjSave()
      ENDIF

      END FUNCTION PrjSaveAs
!
!*****************************************************************************
!
      INTEGER FUNCTION PrjSave
!
! If project file name exists, overwrite without warning.
! If project file name blank, ask for a name.
!
! RETURNS 0 for success
!
      USE PRJVAR

      IMPLICIT NONE

      INTEGER, EXTERNAL :: PrjSaveAs

      IF (LEN_TRIM(PrjFileName) .EQ. 0) THEN
        PrjSave = PrjSaveAs()
      ELSE
        CALL PrjReadWrite(PrjFileName,cWrite)
        PrjSave = 0 ! Yeah, right
      ENDIF

      END FUNCTION PrjSave
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
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PRJVAR

      IMPLICIT NONE

      CHARACTER(LEN=60) FILTER
      INTEGER           IFLAGS, iFType 
      CHARACTER(LEN=MaxPathLength) tFileName

      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|'//&
               'DASH project files (*.dash)|*.dash|'
      tFileName = ''
! iFType specifies which of the file types in the list is the default
      iFType = 2
      tFileName = PrjFileName
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open DASH project file',iFType)
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

      CALL PrjReadWrite(ThePrjFile,cRead)

      END SUBROUTINE PrjFileLoad
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWrite(ThePrjFile,ReadOrWrite)
!
! This subroutine saves the project file.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PRJVAR
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ThePrjFile
      INTEGER,       INTENT (IN   ) :: ReadOrWrite

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER         ErrCounter
      COMMON /CMN008/ ErrCounter

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER I, j, tInteger, RW, tCurrentWizardWindow
      LOGICAL tLogical
      REAL    tReal
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: GetCrystalSystem
      CHARACTER*255 tString

      ErrCounter = 0
      CALL PushActiveWindowID
      iPrjReadOrWrite = ReadOrWrite
      RW = iPrjReadOrWrite
      hPrjFile = 10
      OPEN(UNIT=hPrjFile,FILE=ThePrjFile,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      iPrjRecNr = 1
! Read / Write the header
      tString = ProgramVersion//' project file'
      CALL FileRWString(hPrjFile,iPrjRecNr,RW,tString)
! Read / Write Wizard Window
      IF (RW .EQ. cWrite) THEN
        CALL FileWriteInteger(hPrjFile,iPrjRecNr,CurrentWizardWindow)
      ELSE
        CALL FileReadInteger(hPrjFile,iPrjRecNr,tCurrentWizardWindow)
      ENDIF
! Read / Write radiation source
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,JRadOption)
! Read / Write Wavelength
      CALL FileRWReal(hPrjFile,iPrjRecNr,RW,ALambda)
      IF (RW .EQ. cRead) THEN
        CALL Upload_Source
        CALL Upload_Wavelength
      ENDIF
! We Read / Write the original pattern + all information to get the processed pattern:
! - Truncation limits
! - Background subtraction parameters
! - LBIN
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,BackupNOBS)
      IF (BackupNOBS .EQ. 0) THEN
        NoData = .TRUE.
      ELSE
        NoData = .FALSE.
        DO I = 1, BackupNOBS
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BackupXOBS(I))
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BackupYOBS(I))
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BackupEOBS(I))
        ENDDO
! Read / Write LBIN
        CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,LBIN)
! Read / Write start / end
        CALL WDialogSelect(IDD_PW_Page5)
        IF (RW .EQ. cWrite) THEN
          CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_TruncateStartYN))
          CALL WDialogGetReal(IDF_Min2Theta,tReal)
          CALL FileWriteReal(hPrjFile,iPrjRecNr,tReal)
          CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_TruncateEndYN))
          CALL WDialogGetReal(IDF_Max2Theta,tReal)
          CALL FileWriteReal(hPrjFile,iPrjRecNr,tReal)
        ELSE
          CALL FileReadLogical(hPrjFile,iPrjRecNr,tLogical)
          CALL WDialogPutCheckBoxLogical(IDF_TruncateStartYN,tLogical)
          CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
          CALL WDialogPutReal(IDF_Min2Theta,tReal)
          CALL FileReadLogical(hPrjFile,iPrjRecNr,tLogical)
          CALL WDialogPutCheckBoxLogical(IDF_TruncateEndYN,tLogical)
          CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
          CALL WDialogPutReal(IDF_Max2Theta,tReal)
        ENDIF
! Read / Write the parameters for the background algorithm
        CALL WDialogSelect(IDD_PW_Page6)
        IF (RW .EQ. cWrite) THEN
          CALL WDialogGetInteger(IDF_NumOfIterations,tInteger)
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL WDialogGetInteger(IDF_WindowWidth,tInteger)
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_UseMCYN))
        ELSE
          CALL FileReadInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL WDialogPutInteger(IDF_NumOfIterations,tInteger)
          CALL FileReadInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL WDialogPutInteger(IDF_WindowWidth,tInteger)
          CALL FileReadLogical(hPrjFile,iPrjRecNr,tLogical)
          CALL WDialogPutCheckBoxLogical(IDF_UseMCYN,tLogical)
        ENDIF
        IF (RW .EQ. cRead) THEN
          CALL WizardApplyDiffractionFileInput
          CALL WizardApplyProfileRange
          CALL WizardApplyBackground
          IPTYPE = 1
        ENDIF
      ENDIF
! Read / Write unit cell
      DO I = 1, 6
        CALL FileRWReal(hPrjFile,iPrjRecNr,RW,CellPar(I))
      ENDDO
! Read / Write zero-point
      CALL FileRWReal(hPrjFile,iPrjRecNr,RW,ZeroPoint)
! Read / Write space group
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumberSGTable)
! Calculate tick marks
      IF (RW .EQ. cRead) THEN
        LatBrav = GetCrystalSystem(NumberSGTable)
        CALL Upload_CrystalSystem
        CALL FillSymmetry
        PastPawley = .FALSE.
        CALL Generate_TicMarks
        CALL GET_LOGREF
      ENDIF
! Excluded regions

! Read / Write Pawley refinement related stuff
! Read / Write the peak fit ranges
      CALL PrjReadWritePeakFitRanges
! We _must_ read the Peak Fit Ranges after the data needed to generate the tick marks (unit cell,
! zero point, wavelength, space group, powder pattern) because it needs the tick marks
! to assign a reflection to each peak position.
! Read / Write the .pik file
!            WRITE (IPK,*) ARGI, OBS - YBACK, DOBS, NTEM
!        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM
      IF (RW .EQ. cRead) THEN
        CALL WizardWindowShow(tCurrentWizardWindow)
! Is this "PastPawley"? (has consequences for e.g. drawing of peak fit ranges)
        PastPawley = ((CurrentWizardWindow .EQ. IDD_SAW_Page1) .OR.     &
                      (CurrentWizardWindow .EQ. IDD_SAW_Page2) .OR.     &
                      (CurrentWizardWindow .EQ. IDD_SA_input2) .OR.     &
                      (CurrentWizardWindow .EQ. IDD_SA_input3))
      ENDIF
      IF (PastPawley) THEN
        CALL FileRWReal(hPrjFile,iPrjRecNr,RW,PAWLEYCHISQ)
! If we are this way down the file, NBIN was determined by original pattern + truncation + LBIN.
! However, after we have done a Pawley fit, data points beyond the 350th reflection have been discarded,
! and NBIN has a new value.
        CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NBIN)
! Read / Write observed pattern minus the background fitted during the Pawley refinement.
! This is the observed pattern read in by GETPIK.
        DO I = 1, NBIN
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,YOBIN(I))
        ENDDO
        IF (RW .EQ. cRead) THEN
          DO I = 1, NBIN
            WTSA(I) = 1.0 / EBIN(I)**2
          ENDDO
        ENDIF
        NFITA = 0
        DO I = 1, NBIN
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,KREFT(I))
          IF (KREFT(I).GT.0) THEN
            DO j = 1, KREFT(I)
              CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,KNIPT(j,I))
              CALL FileRWReal(hPrjFile,iPrjRecNr,RW,PIKVAL(j,I))
            ENDDO
            NFITA = NFITA + 1
            IFITA(NFITA) = I
          ENDIF
        ENDDO
      ENDIF
      IF (RW .EQ. cRead) CALL Profile_Plot

      CALL PrjErrTrace

      IF (PastPawley) THEN
        CALL PrjReadWriteIntensities
      ENDIF

      CALL PrjErrTrace

! Read / Write the Z-matrices
      CALL PrjReadWriteZmatrices

      CALL PrjErrTrace

! Read / Write Preferred Orientation
      CALL PrjReadWritePO

      CALL PrjErrTrace

! Read / Write range and fixed yes/no per parameter
      CALL SA_Parameter_Set
! Read / Write solutions
      CALL PrjReadWriteSolutions

      CALL PrjErrTrace



      CLOSE(hPrjFile)
      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing project file.')
      CLOSE(hPrjFile)
      CALL PopActiveWindowID

      END SUBROUTINE PrjReadWrite
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteSolutions
!
! Read or writes information on peak fit ranges to / from binary project file.
!
      USE PRJVAR
      USE ZMVAR
      USE ATMVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL            BestValuesDoF
      COMMON /SOLCOM/ BestValuesDoF(1:mvar,1:MaxRun)

      REAL                XAtmCoords
      COMMON /PDBOVERLAP/ XAtmCoords(1:3,1:maxatm,1:MaxRun)

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
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER ii, I, J, RW
      DOUBLE PRECISION X(MVAR)

! Read or Write?
      RW = iPrjReadOrWrite
! Number of solutions
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumOf_SA_Runs)
! Read / Write solutions
      IF (NumOf_SA_Runs .NE. 0) THEN
        DO I = 1, NumOf_SA_Runs
          DO J = 1, nvar
            CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BestValuesDoF(J,I))
            X(J) = DBLE(BestValuesDoF(J,I))
          ENDDO
          IF (iPrjReadOrWrite .EQ. cRead) THEN
! Fill pdbAtmCoords
            CALL makefrac(X)
! Fractional co-ordinates are now in Xato.
            DO ii = 1, NATOM
              XAtmCoords(1,ii,I) = Xato(1,ii)
              XAtmCoords(2,ii,I) = Xato(2,ii)
              XAtmCoords(3,ii,I) = Xato(3,ii)
            ENDDO
          ENDIF
        ENDDO
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

      INTEGER tInteger
      CHARACTER*20, EXTERNAL :: Integer2String
      
      tInteger = 10101
      CALL FileRWInteger(hPrjFile,iPrjRecNr,iPrjReadOrWrite,tInteger)
      ErrCounter = ErrCounter + 1
      IF ((iPrjReadOrWrite .EQ. cRead) .AND. (tInteger .NE. 10101))  &
         CALL DebugErrorMessage('Prj read error '//Integer2String(ErrCounter))

      END SUBROUTINE PrjErrTrace
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWritePeakFitRanges
!
! Read or writes information on peak fit ranges to / from binary project file.
!
      USE PRJVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkFnVarVal,                   PkFnVarEsd,                   &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      INTEGER iPFR, iPeak, iPkDes, iPoint, RW

! Read or Write?
      RW = iPrjReadOrWrite
! If no Peak Fit Ranges, write 0 and exit   
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumPeakFitRange)
      IF (NumPeakFitRange .NE. 0) THEN
        DO iPFR = 1, NumPeakFitRange
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPF_Range(1,iPFR))
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPF_Range(2,iPFR))
          CALL FileRWLogical(hPrjFile,iPrjRecNr,RW,RangeFitYN(iPFR))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,IPF_Lo(iPFR))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,IPF_Hi(iPFR))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumInPFR(iPFR))
          DO iPeak = 1, NumInPFR(iPFR)
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPF_Pos(iPeak,iPFR))
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,YPF_Pos(iPeak,iPFR))
          ENDDO
          IF (RangeFitYN(iPFR)) THEN
            DO iPkDes = 1, MPkDes
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkFnVal(iPkDes,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkFnEsd(iPkDes,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkFnCal(iPkDes,iPFR))
            ENDDO
            DO iPeak = 1, NumInPFR(iPFR)
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkAreaVal(iPeak,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkAreaEsd(iPeak,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkPosVal(iPeak,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkPosEsd(iPeak,iPFR))
            ENDDO
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkPosAv(iPFR))
          ENDIF
! Calculate IPF_Range (only necessary on read)
          IPF_Range(iPFR) = 1 + IPF_Hi(iPFR) - IPF_Lo(iPFR)
        ENDDO
! Calculate IPF_RPt
        IPF_RPt(1) = 0
        DO iPFR = 1, NumPeakFitRange
          IPF_RPt(iPFR+1) = IPF_RPt(iPFR) + IPF_Range(iPFR)
        ENDDO
        DO iPoint = 1, IPF_RPt(NumPeakFitRange+1)
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPeakFit(iPoint))
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,YPeakFit(iPoint))
        ENDDO
      ENDIF
      IF (RW .EQ. cRead) THEN
! Update 'View'|'Peak Positions'...
        CALL Upload_Positions ! Calculates COMMON /ALLPEAKS/
!... and 'View'|'Peak Widths' tabs
        CALL Upload_Widths
      ENDIF

      END SUBROUTINE PrjReadWritePeakFitRanges
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteIntensities
!
! Reads or writes information on preferred orientation to / from binary project file.
!
      USE PRJVAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

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
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumOfRef)
      DO iR = 1, NumOfRef
! Observed intensity
        CALL FileRWReal(hPrjFile,iPrjRecNr,RW,AIOBS(iR))
! Weight of this intensity
        CALL FileRWReal(hPrjFile,iPrjRecNr,RW,WTI(iR))
        IF (RW .EQ. cWrite) THEN
! Determine number of correlations to write out
          NCOR = 14
          DO WHILE ((NCOR .GE. 1) .AND. (IHCOV(NCOR,iR) .EQ. 0))
            NCOR = NCOR - 1
          ENDDO
        ENDIF
! Number of correlations
        CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NCOR)
        IF (NCOR .GT. 0) THEN
          DO I = 1, NCOR
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,IHCOV(I,iR))
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
      CALL FileRWLogical(hPrjFile,iPrjRecNr,RW,PrefParExists)
! Read/write preferred orientation parameters anyway, to enable the user
! to switch PO on and off without having to re-enter direction etc.
      DO i = 1, 4
        CALL FileRWReal(hPrjFile,iPrjRecNr,RW,PrefPars(i))
      ENDDO
! Update the appropriate Wizard window
      IF (iPrjReadOrWrite .EQ. cRead) CALL Update_PO

      END SUBROUTINE PrjReadWritePO
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWriteZmatrices
!
! Reads or writes information on peak fit ranges to / from binary project file.
!
      USE PRJVAR
      USE ZMVAR

      IMPLICIT NONE

      INTEGER iFrg, RW, iAtomNr
      REAL    tReal

! Read or Write?
      RW = iPrjReadOrWrite
      CALL WDialogSelect(IDD_SAW_Page1)
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,nfrag)
      DO iFrg = 1, maxfrg
        CALL FileRWLogical(hPrjFile,iPrjRecNr,RW,gotzmfile(iFrg))
        IF (gotzmfile(iFrg)) THEN
          IF (RW .EQ. cWrite) CALL WDialogGetInteger(IDFzmNumber(iFrg),zmNumberOfCopies(iFrg))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,zmNumberOfCopies(iFrg))
          IF (RW .EQ. cRead ) CALL WDialogPutInteger(IDFzmNumber(iFrg),zmNumberOfCopies(iFrg))
          CALL FileRWString (hPrjFile,iPrjRecNr,RW,frag_file(iFrg))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,izmpar(iFrg))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,icomflg(iFrg))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,natoms(iFrg))
          DO iAtomNr = 1, natoms(iFrg)
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,ioptb(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iopta(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,ioptt(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iz1(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iz2(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iz3(iAtomNr,iFrg))
            IF (RW .EQ. cWrite) THEN
              CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(blen(iAtomNr,iFrg)))
              CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(alph(iAtomNr,iFrg)))
              CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(bet(iAtomNr,iFrg)))
            ELSE
              CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
              blen(iAtomNr,iFrg) = DBLE(tReal)
              CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
              alph(iAtomNr,iFrg) = DBLE(tReal)
              CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
              bet(iAtomNr,iFrg) = DBLE(tReal)
            ENDIF
            CALL FileRWString (hPrjFile,iPrjRecNr,RW,asym(iAtomNr,iFrg))
            CALL FileRWString (hPrjFile,iPrjRecNr,RW,OriginalLabel(iAtomNr,iFrg))
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,tiso(iAtomNr,iFrg))
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,occ(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,izmoid(iAtomNr,iFrg))
            izmbid(izmoid(iAtomNr,iFrg),iFrg) = iAtomNr ! the back mapping
          ENDDO
          CALL zmDoAdmin(iFrg)
! Precalculate the bonds
          CALL zmGenerateBonds(iFrg)
        ENDIF
      ENDDO
      IF (RW .EQ. cRead) CALL UpdateZmatrixSelection

      END SUBROUTINE PrjReadWriteZmatrices
!
!*****************************************************************************
!
