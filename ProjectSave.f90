! Subroutines concerned with the project file
!
!*****************************************************************************
!
      INTEGER FUNCTION PrjSaveAs
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
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

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

      INTEGER, EXTERNAL :: GetCrystalSystem
      INTEGER I, j, RW
      CHARACTER*(255) tString 

      ErrCounter = 0
      CALL PushActiveWindowID
      iPrjReadOrWrite = ReadOrWrite
      RW = iPrjReadOrWrite
      hPrjFile = 10
      OPEN(UNIT=hPrjFile,FILE=ThePrjFile,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      iPrjRecNr = 1
! Read / Write the header
      tString = ProgramVersion//' project file'
      CALL FileRWString(hPrjFile, iPrjRecNr, RW, tString)
! Read / Write radiation source
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, JRadOption)
! Read / Write Wavelength
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, ALambda)
      IF (RW .EQ. cRead) THEN
        CALL Upload_Source
        CALL Upload_Wavelength
      ENDIF
! Read / Write unit cell
      DO I = 1, 6
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, CellPar(I))
      ENDDO
! Read / Write zero-point
      CALL FileRWReal(hPrjFile, iPrjRecNr, RW, ZeroPoint)
      IF (RW .EQ. cRead) CALL Upload_ZeroPoint
! Read / Write space group
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumberSGTable)
      IF (RW .EQ. cRead) THEN
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
      IF (RW .EQ. cRead) CALL WizardWindowShow(IDD_SAW_Page5)
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
      IF (RW .EQ. cRead) THEN
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
      CALL PrjReadWriteZmatrices
      CALL PrjErrTrace
! Read / Write Preferred Orientation
      CALL PrjReadWritePO
      IF (RW .EQ. cRead) THEN
        CALL FillSymmetry_2
        CALL GET_LOGREF
        CALL MakRHm
      ENDIF
      CALL PrjErrTrace
! Update ranges and fixed yes/no per parameter
      IF (RW .EQ. cRead) CALL SA_Parameter_Set
! Read / Write solutions
      CALL PrjReadWriteSolutions
      CALL PrjErrTrace
      IF (RW .EQ. cRead) THEN
! Grey out the "Save... chi sqrd progress"
        CALL WDialogSelect(IDD_OutputSolutions)
        CALL WDialogFieldState(IDF_GROUP2, Disabled)
        CALL WDialogFieldState(IDB_OutputChiSqd, Disabled)
        CALL WDialogFieldState(IDF_LABEL5, Disabled)
        CALL WDialogFieldState(IDF_LABEL3, Disabled)
        CALL CloseOutputSolutionsChildWindows
        CALL WDialogSelect(IDD_SAW_Page5)
        CALL WDialogFieldState(IDB_Prog3,Disabled)
        CALL WDialogSelect(IDD_ViewPawley)
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
        CALL WDialogSelect(IDD_PW_Page8)
        CALL WDialogFieldState(IDB_PrevRes, Disabled)
        BackRef = .FALSE.
        CALL SetModeMenuState(0,1)
! Change global variable FNAME
        FNAME = ThePrjFile
! Update this throughout the program (Wizard + status bar)
        CALL ScrUpdateFileName
      ENDIF
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
! Read or writes information on solutions to / from binary project file.
!
      USE PRJVAR
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

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
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

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
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumOfRef)
      DO iR = 1, NumOfRef
! Observed intensity
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, AIOBS(iR))
! Weight of this intensity
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, WTI(iR))
        IF (RW .EQ. cWrite) THEN
! Determine number of correlations to write out
          NCOR = 14
          DO WHILE ((NCOR .GE. 1) .AND. (IHCOV(NCOR,iR) .EQ. 0))
            NCOR = NCOR - 1
          ENDDO
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
        CALL FileRWReal(hPrjFile, iPrjRecNr, RW, PrefPars(i))
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

      INTEGER, EXTERNAL :: ElmSymbol2CSD, Get_HydrogenTreatment
      INTEGER iFrg, RW, iAtomNr, BondNr

! Read or Write?
      RW = iPrjReadOrWrite
      CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, nfrag)
      DO iFrg = 1, nFrag
        CALL FileRWString (hPrjFile, iPrjRecNr, RW, frag_file(iFrg))
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, izmpar(iFrg))
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, icomflg(iFrg))
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, natoms(iFrg))
        DO iAtomNr = 1, natoms(iFrg)
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, ioptb(iAtomNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iopta(iAtomNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, ioptt(iAtomNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iz1(iAtomNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iz2(iAtomNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, iz3(iAtomNr,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, blen(iAtomNr,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, alph(iAtomNr,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, bet(iAtomNr,iFrg))
          CALL FileRWString (hPrjFile, iPrjRecNr, RW, ElSym(iAtomNr,iFrg))
          IF (RW .EQ. cRead) THEN
            zmElementCSD(iAtomNr,iFrg) = ElmSymbol2CSD(ElSym(iAtomNr,iFrg))
          ENDIF
          CALL FileRWString (hPrjFile, iPrjRecNr, RW, OriginalLabel(iAtomNr,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, tiso(iAtomNr,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, occ(iAtomNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, izmoid(iAtomNr,iFrg))
          izmbid(izmoid(iAtomNr,iFrg),iFrg) = iAtomNr ! the back mapping
          CALL FileRWLogical(hPrjFile, iPrjRecNr, RW, UseQuaternions(iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxDef(iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxAtm(1,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxAtm(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotAxFrac(1,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotAxFrac(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotAxFrac(3,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxPlnAtm(1,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxPlnAtm(2,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRotAxPlnAtm(3,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(0,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(1,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRotationQs(3,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrDef(iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrFrac(1,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrFrac(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrFrac(3,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrEuler(1,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrEuler(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrEuler(3,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(0,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(1,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmSingleRAIniOrQuater(3,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(0,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(1,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(2,iFrg))
          CALL FileRWReal   (hPrjFile, iPrjRecNr, RW, zmInitialQs(3,iFrg))
        ENDDO
        IF (RW .EQ. cRead) THEN
          CALL zmDoAdmin(iFrg)
        ENDIF
        CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, NumberOfBonds(iFrg))
        DO BondNr = 1, NumberOfBonds(iFrg)
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, BondType(BondNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, Bonds(1,BondNr,iFrg))
          CALL FileRWInteger(hPrjFile, iPrjRecNr, RW, Bonds(2,BondNr,iFrg))
        ENDDO
      ENDDO
      IF (RW .EQ. cRead) THEN 
        CALL UpdateZmatrixSelection
        CALL Create_AtomicWeightings(Get_HydrogenTreatment())
      ENDIF

      END SUBROUTINE PrjReadWriteZmatrices
!
!*****************************************************************************
!
