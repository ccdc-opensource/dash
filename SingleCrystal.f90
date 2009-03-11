!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowSingleCrystalData1

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL, EXTERNAL :: Confirm

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SX_Page1)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
              CALL EndWizard
              CALL Download_SpaceGroup(IDD_SX_Page1)
              CALL Download_Cell_Constants(IDD_SX_Page1)
            CASE (IDBACK)
              CALL Download_SpaceGroup(IDD_SX_Page1)
              CALL Download_Cell_Constants(IDD_SX_Page1)
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
              CALL CheckUnitCellConsistency
              IF (NumberSGTable .EQ. LPosSG(LatBrav)) CALL WarningMessage('Space-group symmetry has not been reset.')
              CALL WizardWindowShow(IDD_SX_Page1a)
            CASE (IDAPPLY)
              CALL Download_SpaceGroup(IDD_SX_Page1)
              CALL Download_Cell_Constants(IDD_SX_Page1)
              CALL CheckUnitCellConsistency
            CASE (IDB_Delabc)
              CALL Clear_UnitCell_WithConfirmation
            CASE (IDBBROWSE) ! Read unit cell
              CALL UnitCellParametersFileBrowse
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL DASHWDialogGetReal(IDF_a_latt, CellPar(1))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_b_latt)
              CALL DASHWDialogGetReal(IDF_b_latt, CellPar(2))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_c_latt)
              CALL DASHWDialogGetReal(IDF_c_latt, CellPar(3))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_alp_latt)
              CALL DASHWDialogGetReal(IDF_alp_latt, CellPar(4))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_bet_latt)
              CALL DASHWDialogGetReal(IDF_bet_latt, CellPar(5))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_gam_latt)
              CALL DASHWDialogGetReal(IDF_gam_latt, CellPar(6))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_Crystal_System_Menu)
              CALL DASHWDialogGetMenu(IDF_Crystal_System_Menu, LatBrav)
              CALL Upload_CrystalSystem
              CALL Generate_TicMarks
            CASE (IDF_Space_Group_Menu)
              CALL Download_SpaceGroup(IDD_SX_Page1)
              CALL Generate_TicMarks
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowSingleCrystalData1
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowSingleCrystalData1a ! Resolution

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SX_Page1a)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SX_Page1)
            CASE (IDNEXT)
              CALL SelectDASHDialog(IDD_ViewPawley)
              CALL WDialogPutReal(IDF_MaxResolution, SXMaxResolution)
              CALL SelectDASHDialog(IDD_SX_Page2)
              CALL WDialogFieldStateLogical(IDNEXT, .FALSE.)
              PastPawley = .TRUE.
              CALL WizardWindowShow(IDD_SX_Page2)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowSingleCrystalData1a
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowSingleCrystalData2

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER, EXTERNAL :: HKLFFileBrowse, HKLFFileOpen
      LOGICAL, EXTERNAL :: SaveProject
      CHARACTER(LEN=MaxPathLength) CTEMP
      INTEGER iErr

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SX_Page2)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SX_Page1a)
            CASE (IDNEXT)
              IF (SaveProject()) CALL ShowWizardWindowZmatrices
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (ID_PW_DF_Open)
              CALL DASHWDialogGetString(IDF_PW_DataFileName_String, CTEMP)
              iErr = HKLFFileOpen(CTEMP)
              CALL WDialogFieldStateLogical(IDNEXT, iErr .EQ. 0)
            CASE (IDBBROWSE)
              iErr = HKLFFileBrowse()
! Don't change if the user pressed 'Cancel' (ISTAT = 2)
              IF      (iErr .EQ. 0) THEN
                CALL WDialogFieldState(IDNEXT, Enabled)
              ELSE IF (iErr .EQ. 1) THEN
                CALL WDialogFieldState(IDNEXT, Disabled)
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowSingleCrystalData2
!
!*****************************************************************************
!
      INTEGER FUNCTION HKLFFileBrowse()
!
! This routine lets the user browse a directory for an HKLF file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
!
! 0 = OK
! 1 = error
! 2 = user pressed cancel.
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, EXTERNAL :: HKLFFileOpen
      CHARACTER(LEN=60) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName

      HKLFFileBrowse = 1
      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = ALL_FILES_FILTER//&
               'SHELX .hkl files (*.hkl)|*.hkl|'
      tFileName = ' '
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open single crystal data file',IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) THEN
        HKLFFileBrowse = 2
        RETURN
      ENDIF
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under Windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      HKLFFileBrowse = HKLFFileOpen(tFileName)

      END FUNCTION HKLFFileBrowse
!
!*****************************************************************************
!
      INTEGER FUNCTION HKLFFileOpen(TheFileName)
!
! This routine tries to open an HKLF file.
!
! INPUT   : TheFileName = the file name
!
! 0 = OK
! 1 = error
!
      USE DRUID_HEADER
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER, EXTERNAL :: HKLFFileLoad
      LOGICAL FExists
      INTEGER KLEN

      HKLFFileOpen = 1
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
      IF (HKLFFileLoad(FNAME(1:KLEN)) .NE. 0) RETURN
! Next line is necessary due to the way ScrUpdateFileName in SDIFileLoad works
      FNAME = TheFileName
      IF (NoData) THEN
        CALL ErrorMessage("Could not read the hkl file "//FNAME(1:KLEN)//&
                          CHAR(13)//"successfully.")
        RETURN
      ENDIF
! Disable Pawley refinement button
      CALL SetModeMenuState(-1,-1)
      CALL WindowOutStatusBar(1, FNAME)

      ! @@ ??
! Update the file name
      CALL SelectDASHDialog(IDD_SX_Page2)
      CALL WDialogPutString(IDF_PW_DataFileName_String, FNAME(1:KLEN))
      HKLFFileOpen = 0
      
      END FUNCTION HKLFFileOpen
!
!*****************************************************************************
!
      INTEGER FUNCTION HKLFFileLoad(TheFileName)
!
! Gets single crystal data and generates a false diffraction pattern
!
! 0 = OK
! 1 = error
!
      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER         jHKL           
      REAL                            WTJ,         AJOBS
      COMMON /SXFSTO/ jHKL(3,MFCSTO), WTJ(MFCSTO), AJOBS(MFCSTO)

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(MaxKTem,MOBS), PIKVAL(MaxKTem,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER ISIG5, IArgKK
      INTEGER KXIMIN(MOBS), KXIMAX(MOBS)
      INTEGER KK, I, NLIN, iR, jR, J, K, hFile
      INTEGER KTEM, K1, K2
      REAL    ARGIMIN, ARGIMAX, ARGISTP, ARGT, FWHM, C0, Gaussian !, Lorentzian
      CHARACTER*150 LINE
      LOGICAL RecalculateESDs, IgnoreLT, AvgFriedelPairs
      REAL    CutOff
      LOGICAL Keep(1:MFCSTO)

      HKLFFileLoad = 1
      hFile = 121
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SX_Page2)
      RecalculateESDs = DASHWDialogGetCheckBoxLogical(IDF_RecalcESDs)
      IgnoreLT = DASHWDialogGetCheckBoxLogical(IDF_IgnLT)
      IF (IgnoreLT) CALL DASHWDialogGetReal(IDF_CutOff, CutOff)
      AvgFriedelPairs = DASHWDialogGetCheckBoxLogical(IDF_AvgFriedelPairs)
      CALL PopActiveWindowID
      Is_SX = .TRUE.
      OPEN(hFile, FILE=TheFileName, STATUS='OLD', ERR=998)
      iR = 1
      DO KK = 1, MFCSTO
        READ(hFile, '(A)', END=100, ERR=998) LINE
        NLIN = LEN_TRIM(LINE)
        IF (LEN_TRIM(LINE) .NE. 0) THEN
!C SHELX .hkl files are terminated by a line containing h = k = l = 0 or h = k = l = 99
          READ(LINE(1:NLIN), *, END=998, ERR=998) (jHKL(I,iR),I=1,3)
          IF (((jHKL(1,iR) .EQ.  0) .AND. &
               (jHKL(2,iR) .EQ.  0) .AND. &
               (jHKL(3,iR) .EQ.  0)) .OR. &
              ((jHKL(1,iR) .EQ. 99) .AND. &
               (jHKL(2,iR) .EQ. 99) .AND. &
               (jHKL(3,iR) .EQ. 99)))     &
               GOTO 100
!C No cross correlation ...
          READ(LINE(1:NLIN), *, END=998, ERR=998) (jHKL(I,iR),I=1,3), AJOBS(iR), WTJ(iR)
!C F2 and sig(F2)
          IF (RecalculateESDs) WTJ(iR) = MAX(MAX(4.4, SQRT(MAX(0.0, AJOBS(iR)))), WTJ(iR))
          IF (.NOT. (IgnoreLT .AND. ((AJOBS(iR)/WTJ(iR)) .LT. CutOff))) THEN
            WTJ(iR) = 1.0 / WTJ(iR)
            iR = iR + 1
          ENDIF
        ENDIF
      ENDDO
  100 NumOfRef = iR-1
      IF (NumOfRef .EQ. 0) THEN
        CALL ErrorMessage("No reflections found.")
        RETURN
      ENDIF
! Average Friedel related pairs
      DO iR = 1, NumOfRef
        Keep(iR) = .TRUE.
      ENDDO
      IF (AvgFriedelPairs) THEN
        DO iR = 1, NumOfRef-1
          IF (Keep(iR)) THEN
            DO jR = iR+1, NumOfRef
              IF (Keep(jR)) THEN
                IF (((jHKL(1,iR) .EQ. -jHKL(1,jR)) .AND. &
                     (jHKL(2,iR) .EQ. -jHKL(2,jR)) .AND. &
                     (jHKL(3,iR) .EQ. -jHKL(3,jR))) .OR. &
                    ((jHKL(1,iR) .EQ.  jHKL(1,jR)) .AND. &
                     (jHKL(2,iR) .EQ.  jHKL(2,jR)) .AND. &
                     (jHKL(3,iR) .EQ.  jHKL(3,jR)))) THEN
                  AJOBS(iR) = (AJOBS(iR) + AJOBS(jR)) / 2.0
                  WTJ(iR) = 1.0/SQRT((1.0/WTJ(iR))**2 + (1.0/WTJ(jR))**2)
                  Keep(jR) = .FALSE.
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!C We've got the lattice constants, symmetry etc. already.
!C Let's order the reflections in increasing 2 theta and fill the array ArgKK
      CALL OrderReflections(Keep)
      DO iR = 1, NumOfRef
        IKKOR(iR) = iR
        JKKOR(iR) = iR
      ENDDO
      KKOR = NumOfRef
      DO iR = 1, NumOfRef
!C No correlation, so II .EQ. JJ by definition.
        WTIJ(iR) = WTI(iR) * WTI(iR)
      ENDDO
      CLOSE(hFile)
!C Now synthesise a simple diffraction pattern at 1 Angstrom
      NBIN = 5000
      ARGIMIN = RefArgK(1)-1.0
      ARGIMAX = RefArgK(NumOfRef)+1.0
      ARGISTP = (ARGIMAX-ARGIMIN)/FLOAT(NBIN-1)
      FWHM = 6.0 * ARGISTP
      C0 = 4.0 * LOG(2.0)
      ISIG5 = 30
      DO I = 1, NBIN
        KXIMIN(I) = 0
      ENDDO
      DO K = 1, NumOfRef
        IArgKK = 1 + NINT((RefArgK(K)-ARGIMIN)/ARGISTP)
        DO I = MAX(1, IArgKK-ISIG5), MIN(5000, IArgKK+ISIG5)
          IF (KXIMIN(I) .EQ. 0) KXIMIN(I) = K
          KXIMAX(I) = K
        ENDDO
      ENDDO
      NFITA = 0
      DO I = 1, NBIN
        XBIN(I) = ARGIMIN + FLOAT(I-1)*ARGISTP
        YOBIN(I) = 0.0
        YCBIN(I) = 0.0
        YBBIN(I) = 0.0
        EBIN(I)  = 0.0
        IF (KXIMIN(I) .EQ. 0) THEN
!C No peaks at this point
          EBIN(I) = 1.0
          WTSA(I) = 1.0
          KREFT(I) = 0
        ELSE
          NFITA = NFITA + 1
          IFITA(NFITA) = I
          KREFT(I) = MIN(MaxPik, 1 + KXIMAX(I) - KXIMIN(I))
          J = 0
          DO K = KXIMIN(I), KXIMAX(I)
            ARGT = (XBIN(I)-RefArgK(K)) / (FWHM)
            J = J + 1
!C JCC - needs bound check on J here
            IF ( J .GT. MaxPik ) GOTO 567

            KNIPT(J,I) = K
            Gaussian = (SQRT(C0)/(FWHM*SQRT(PI))) * EXP(-C0*(ARGT**2))
        !    Lorentzian = (2.0/(PI*FWHM)) * (1.0/(1.0+(4.0*(ARGT**2))))
            ! Now use a pseudo-Voigt
            PIKVAL(J,I) =  Gaussian !0.125*Lorentzian + 0.875*Gaussian
            YOBIN(I) = YOBIN(I) + AIOBS(K)*PIKVAL(J,I)
          ENDDO
          EBIN(I) = MAX(1.0, 0.1*ABS(YOBIN(I)))
          WTSA(I) = 1.0 / EBIN(I)**2
        ENDIF
      ENDDO
!C Write out a fake .pik file
      OPEN(UNIT=hFile, FILE='polyp.pik', STATUS='UNKNOWN', ERR=999)
      DO I = 1, NBIN
        KTEM = KREFT(I)
        WRITE(hFile,*) XBIN(I), YOBIN(I), EBIN(I), KTEM
        K1 = KXIMIN(I)
        K2 = KXIMAX(I)
        IF (KTEM.GT.0) WRITE (hFile,*) (KNIPT(KK,I),PIKVAL(KK,I),KK=1,KTEM)
      ENDDO
      CLOSE(hFile)
!C Write out a fake .hcv file
      OPEN(UNIT=hFile, FILE='polyp.hcv', STATUS='UNKNOWN', ERR=999)
      DO iR = 1, NumOfRef
        WRITE(hFile,101) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), iR
  101   FORMAT (3I5,1X,F12.3,1X,F12.4,1X,I5)
      ENDDO
      CLOSE(hFile)
      CALL Clear_BackGround
      NoData = .FALSE.
      CALL GetProfileLimits
      CALL Get_IPMaxMin 
      CALL Set_Wavelength(1.0)
      PAWLEYCHISQ = 1.0
      CALL SelectDASHDialog(IDD_ViewPawley)
 !     CALL WDialogPutReal(IDF_Sigma1,PeakShapeSigma(1),'(F10.4)')
 !     CALL WDialogPutReal(IDF_Sigma2,PeakShapeSigma(2),'(F10.4)')
 !     CALL WDialogPutReal(IDF_Gamma1,PeakShapeGamma(1),'(F10.4)')
 !     CALL WDialogPutReal(IDF_Gamma2,PeakShapeGamma(2),'(F10.4)')
      CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts, NBIN)
      CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs, NumOfRef)
      CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq, PAWLEYCHISQ, '(F12.3)')
      CALL GET_LOGREF
      IPTYPE = 1
      CALL Profile_Plot
      CALL sa_SetOutputFiles(TheFileName)
      HKLFFileLoad = 0
      RETURN
  998 CLOSE(hFile)
      CALL ErrorMessage("Error opening .hkl file.")
      RETURN
  999 CLOSE(hFile)
      CALL ErrorMessage("Error writing .pik/.hcv file.")
      RETURN
  567 CLOSE(hFile)
      CALL ErrorMessage("Error writing .pik/.hcv file: Bounds exceeded ")

      END FUNCTION HKLFFileLoad
!
!*****************************************************************************
!
      SUBROUTINE OrderReflections(Keep)

! Calculates 2 theta for every reflection.

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: Keep(*)

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      CHARACTER*6 xxx
      CHARACTER*10 fname_2

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)
 
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      INTEGER iSym, I

      IBMBER = 0
      OPEN(42,file='polyo.ccl',status='unknown')
      WRITE(42,4210) 
 4210 FORMAT('N Determining reflection order ')
      IF (NumberSGTable .GE. 1) THEN
        CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
        IF (nsymmin .GT. 0) THEN
          DO iSym = 1, nsymmin
            WRITE(42,4235) symline(iSym)
 4235       FORMAT('S ',A)
          ENDDO
        ENDIF
      ENDIF
      WRITE(42,4240) (cellpar(I),I=1,6)
 4240 FORMAT('C ',6f10.5)
      CLOSE(42)
      fname_2 = 'polyo'
      xxx = 'ORDREF'
      CALL FORORD(xxx, fname_2, Keep)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)

      END SUBROUTINE OrderReflections
!
!*****************************************************************************
!
      SUBROUTINE FORORD(pname,filnmr,Keep)

      USE WINTERACTER
      USE DRUID_HEADER
      USE REFVAR
      USE VARIABLES
#ifdef __G95__
      USE FOR_G95
#endif

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: Keep(*)

      CHARACTER*6 PNAME
      CHARACTER*10 filnmr

      INCLUDE 'params.inc'

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)

      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      CHARACTER*10 filnam_root
      COMMON /commun/ filnam_root

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      INTEGER         jHKL           
      REAL                            WTJ,         AJOBS
      COMMON /SXFSTO/ jHKL(3,MFCSTO), WTJ(MFCSTO), AJOBS(MFCSTO)

      INTEGER, EXTERNAL :: PREFIN
      INTEGER IDummy

      REAL   DStarTem(MFCSTO)
      INTEGER iR, jR, I, hFile
      INTEGER iOrdTem(MFCSTO), KK
      REAL    H(1:3), DERS(1:6), ArgKKtem(MFCSTO), cut_off_2theta

      filnam_root = filnmr
      NINIT = 1
      IDummy = PREFIN(PNAME)
      CALL SYMOP
      CALL RECIP
      IF (IBMBER .NE. 0) RETURN
      CALL OPSYM(1)
      DO iR = 1, NumOfRef
        DO I = 1, 3
          H(i) = FLOAT(jHKL(I,iR))
        ENDDO
        CALL CELDER(H, DERS)
!C wavelength = 1 Angstrom
        IF (sthl .LT. -1.0) THEN
          sthl = -1.0
          CALL DebugErrorMessage("sthl .LT. -1.0")
        ENDIF
        IF (sthl .GT. 1.0) THEN
          sthl = 1.0
          CALL DebugErrorMessage("sthl .GT. 1.0")
        ENDIF
        ArgKKtem(iR) = 2.0 * ASIND(sthl)
        DStarTem(iR) = 2.0 * sthl
      ENDDO
      CALL Sort_Real(ArgKKtem, iOrdTem, NumOfRef)
      cut_off_2theta = 2.0 * ASIND(1.0/(2.0*SXMaxResolution))
      KK = 0
      DO iR = 1, NumOfRef
        jR = iOrdTem(iR)
        IF (Keep(jR)) THEN
          IF (ArgKKTem(jR) .LE. cut_off_2theta) THEN
            KK = KK + 1
            AIOBS(KK) = AJOBS(jR)
            WTi(KK) = WTj(jR)
            DSTAR(KK) = dstartem(jR)
            RefArgK(KK) = ArgKKTem(jR)
            DO I = 1, 3
              iHKL(I,KK) = jHKL(i,jR)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      NumOfRef = KK
!C Write out a fake .tic file
      hFile = 121
      OPEN(UNIT=hFile,FILE='polyp.tic',STATUS='UNKNOWN',ERR=999)
      DO iR = 1, NumOfRef
        WRITE(hFile,*,ERR=999) (iHKL(I,iR),I=1,3), RefArgK(iR), DSTAR(iR)
      ENDDO
      CLOSE(hFile)
      RETURN
  999 CLOSE(hFile)
      CALL ErrorMessage("Error writing .tic file.")

      END SUBROUTINE FORORD
!
!*****************************************************************************
!