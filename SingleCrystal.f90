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
      CALL WDialogSelect(IDD_SX_Page1)
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
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL WDialogGetReal(IDF_a_latt,CellPar(1))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_b_latt)
              CALL WDialogGetReal(IDF_b_latt,CellPar(2))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_c_latt)
              CALL WDialogGetReal(IDF_c_latt,CellPar(3))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_alp_latt)
              CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_bet_latt)
              CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_gam_latt)
              CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_Crystal_System_Menu)
              CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
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
      CALL WDialogSelect(IDD_SX_Page1a)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SX_Page1)
            CASE (IDNEXT)
              CALL WDialogSelect(IDD_SX_Page2)
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
      INCLUDE 'lattice.inc'

      INTEGER IRadSelection
      CHARACTER(LEN=MaxPathLength) CTEMP
      REAL    Temp
      INTEGER iErr
      INTEGER, EXTERNAL :: HKLFFileBrowse, HKLFFileOpen
      LOGICAL, EXTERNAL :: SaveProject

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SX_Page2)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SX_Page1a)
            CASE (IDNEXT)
              CALL ShowWizardWindowZmatrices
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (ID_PW_DF_Open)
              CALL WDialogGetString(IDF_PW_DataFileName_String, CTEMP)
              iErr = HKLFFileOpen(CTEMP)
              CALL WDialogFieldStateLogical(IDBSAVE, iErr .EQ. 0)
            CASE (IDBBROWSE)
              iErr = HKLFFileBrowse()
! Don't change if the user pressed 'Cancel' (ISTAT = 2)
              IF      (iErr .EQ. 0) THEN
                CALL WDialogFieldState(IDBSAVE, Enabled)
              ELSE IF (iErr .EQ. 1) THEN
                CALL WDialogFieldState(IDBSAVE, Disabled)
              ENDIF
            CASE (IDBSAVE)
              IF (SaveProject()) CALL WDialogFieldState(IDNEXT, Enabled)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_LabX_Source,IDF_SynX_Source)
              CALL WDialogGetRadioButton(IDF_LabX_Source, JRadOption)
              CALL Upload_Source
              CALL Generate_TicMarks 
            CASE (IDF_wavelength1)
              CALL WDialogGetReal(IDF_wavelength1, Temp)
              CALL Set_Wavelength(Temp)
              CALL Generate_TicMarks 
            CASE (IDF_Wavelength_Menu)
              CALL WDialogGetMenu(IDF_Wavelength_Menu, IRadSelection)
              CALL SetWavelengthToSelection(IRadSelection)
              CALL Generate_TicMarks 
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowSingleCrystalData2
!
!*****************************************************************************
!
      INTEGER FUNCTION HKLFFileBrowse
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

      CHARACTER(LEN=60) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName
      INTEGER, EXTERNAL :: HKLFFileOpen

      HKLFFileBrowse = 1
      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|'//&
               'DASH Pawley files (*.hklf)|*.hklf|'
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

      LOGICAL FExists
      INTEGER KLEN
      INTEGER, EXTERNAL :: HKLFFileLoad

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
        CALL ErrorMessage("Could not read the hklf file "//FNAME(1:KLEN)//&
                          CHAR(13)//"successfully.")
        RETURN
      ENDIF
! Disable Pawley refinement button
      CALL SetModeMenuState(-1,-1)
      CALL WindowOutStatusBar(1,FNAME)

      ! @@ ??
! Update the file name
      CALL WDialogSelect(IDD_SX_Page2)
      CALL WDialogPutString(IDF_PW_DataFileName_String, FNAME(1:KLEN))
      HKLFFileOpen = 0
      
      END FUNCTION HKLFFileOpen
!
!*****************************************************************************
!
      INTEGER FUNCTION HKLFFileLoad(TheFileName)
!
!.. Gets single crystal data and generates a false diffraction pattern
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

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      CHARACTER*150 LINE
      INTEGER NKKOR(MCHIHS)

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

!.. New store common ...
!C MCHIHS can be replaced by MFCSTO in this COMMON
      INTEGER         jHKL           
      REAL                            WTJ,         AJOBS
      COMMON /SXFSTO/ jHKL(3,MCHIHS), WTJ(MCHIHS), AJOBS(MFCSTO)

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

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER IARGIMIN, IARGISTP, ISIG5, IArgKK
      INTEGER KXIMIN(MOBS), KXIMAX(MOBS), IXKMIN(MFCSTO), IXKMAX(MFCSTO)
      INTEGER KK, I, NLIN, iR, J, K, hFile
      INTEGER KTEM, K1, K2
      REAL    ARGIMIN, ARGIMAX, ARGISTP, SIGMA, ADSIG, ARGT

      HKLFFileLoad = 1
      hFile = 121
      OPEN(hFile,FILE=TheFileName,STATUS='OLD',ERR=999)
      KK = 0
      DO iR = 1, MFCSTO
        READ(hFile,'(Q,A)',END=100,ERR=999) NLIN, LINE
!.. No cross correlation ...
        READ(LINE(1:NLIN),*,END=999,ERR=999) (jHKL(I,iR),I=1,3), AJOBS(iR), WTJ(iR)
!.. F2 and sig(F2)
        WTJ(iR) = 1.0 / WTJ(iR)
        KK = iR
      ENDDO
  100 NumOfRef = KK
!O      WRITE(76,*) ' Number of reflections ',NumOfRef
!
!.. We've got the lattice constants, symmetry etc. already.
!
!.. Let's order the reflections in increasing 2 theta and fill the array ArgKK
      CALL OrderReflections

      DO iR = 1, NumOfRef
        IKKOR(iR) = iR
        JKKOR(iR) = iR
        NKKOR(iR) = 100
      ENDDO
      KKOR = NumOfRef

      DO iR = 1, NumOfRef
!C No correlation, so II .EQ. JJ by definition.
        WTIJ(iR) = WTI(iR) * WTI(iR)
      ENDDO
      CLOSE(hFile)
!O      WRITE(76,*) ' Number of reflections ', NumOfRef
!O      DO iR = 1, NumOfRef
!O        WRITE(76,*) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR)
!O      ENDDO
!.. Now synthesise a simple diffraction pattern at 1 Angstrom
      NBIN = 5000
      ARGIMIN = RefArgK(1)-1.0
      ARGIMAX = RefArgK(NumOfRef)+1.0
      IARGIMIN = 1000 * ARGIMIN
      ARGIMIN = 0.001 * FLOAT(IARGIMIN)
      IARGISTP = 1 + NINT(1000.0*(ARGIMAX-ARGIMIN)/FLOAT(NBIN))
      ARGISTP = 0.001 * FLOAT(IARGISTP)
      ARGIMAX = ARGIMIN + FLOAT(NBIN-1)*ARGISTP
      SIGMA = 3.0 * ARGISTP
      ADSIG = 0.39894/SIGMA
      ISIG5 = 15 !*IARGISTP
!O      WRITE(76,*) ' min/max argkk ',argkk(1),argkk(NumOfRef)
!O      WRITE(76,*) ' 2 theta range ',ARGIMIN,ARGIMAX,ARGISTP,SIGMA,ISIG5
      DO I = 1, NBIN
        KXIMIN(I) = 0
      ENDDO
      DO K = 1, NumOfRef
        IArgKK = 1 + (RefArgK(K)-ARGIMIN)/ARGISTP
        IXKMIN(K) = IArgKK - ISIG5
        IXKMAX(K) = IArgKK + ISIG5
!O        WRITE(76,*) K, ArgKK(K), IArgKK, IXKMIN(K), IXKMAX(K)
        DO I = IXKMIN(K), IXKMAX(K)
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
!.. No peaks at this point
          EBIN(I) = 1.0
          WTSA(I) = 1.0
          KREFT(I) = 0
        ELSE
          NFITA = NFITA + 1
          IFITA(NFITA) = I
          KREFT(I) = 1 + KXIMAX(I) - KXIMIN(I)
          J = 0
          DO K = KXIMIN(I), KXIMAX(I)
            ARGT = (XBIN(I)-RefArgK(K)) / SIGMA
            J = J + 1
            KNIPT(J,I) = K
            PIKVAL(J,I) = ADSIG * EXP(-0.5*ARGT*ARGT)
            YOBIN(I) = YOBIN(I) + AIOBS(K)*PIKVAL(J,I)
!		  EBIN(I)=EBIN(I)+PIKVAL(J,I)/WTI(I)
          ENDDO
          EBIN(I) = MAX(1.0, 0.1*ABS(YOBIN(I)))
          WTSA(I) = 1.0 / EBIN(I)**2
!		EBIN(I)=SQRT(EBIN(I))
        ENDIF
      ENDDO

!C Write out a fake .pik file
      OPEN(UNIT=hFile,FILE='polyp.pik',STATUS='UNKNOWN',ERR=999)
      DO I = 1, NBIN
        KTEM = KREFT(I)
        WRITE(hFile,*) XBIN(I), YOBIN(I), EBIN(I), KTEM
        K1 = KXIMIN(I)
        K2 = KXIMAX(I)
        IF (KTEM.GT.0) WRITE (hFile,*) (KNIPT(KK,I),PIKVAL(KK,I),KK=1,KTEM)
      ENDDO
      CLOSE(hFile)
!C Write out a fake .hcv file
!        READ(LINE(1:NLIN),*,END=999,ERR=999) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), KL, (IHCOV(I,iR),I=1,NCOR)
      OPEN(UNIT=hFile,FILE='polyp.hcv',STATUS='UNKNOWN',ERR=999)
      DO iR = 1, NumOfRef
        WRITE(hFile,*) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), iR
      ENDDO
      CLOSE(hFile)
!C Write out a fake .hkl file
! For powders, this is used for space group determination only
!        READ(LINE(1:NLIN),*,END=999,ERR=999) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), KL, (IHCOV(I,iR),I=1,NCOR)
  !O    OPEN(UNIT=hFile,FILE='polyp.hcv',STATUS='UNKNOWN',ERR=999)
  !O    DO iR = 1, NumOfRef
  !O      WRITE(hFile,*) (iHKL(I,iR),I=1,3), AIOBS(iR), WTI(iR), iR
  !O    ENDDO
  !O    CLOSE(hFile)

      CALL Clear_BackGround
      NoData = .FALSE.
      CALL Set_Wavelength(1.0)
      CALL GetProfileLimits
      CALL Get_IPMaxMin 
      PAWLEYCHISQ = 1.0
      CALL WDialogSelect(IDD_ViewPawley)
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
      HKLFFileLoad = 0
      RETURN
  999 CLOSE(hFile)
      CALL ErrorMessage("Error writing .pik/.hcv file.")

      END FUNCTION HKLFFileLoad
!
!*****************************************************************************
!
      SUBROUTINE OrderReflections

! Covers the eventuality of the default space group option.
! We need to determine the number of symmetry operators etc.

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      CHARACTER*6 xxx
      CHARACTER*10 fname

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
      fname = 'polyo'
      xxx = 'ORDREF'
      CALL FORORD(xxx,fname)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)

      END SUBROUTINE OrderReflections
!
!*****************************************************************************
!
      SUBROUTINE FORORD(pname,filnmr)

      USE WINTERACTER
      USE DRUID_HEADER
      USE REFVAR

      IMPLICIT NONE

      CHARACTER*6 PNAME
      CHARACTER*10 filnmr

      INCLUDE 'PARAMS.INC'

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)

      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      CHARACTER*10 filnam_root
      COMMON /commun/ filnam_root

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

!.. New store common ...
      INTEGER         jHKL           
      REAL                            WTJ,         AJOBS
      COMMON /SXFSTO/ jHKL(3,MCHIHS), WTJ(MCHIHS), AJOBS(MFCSTO)

      REAL   DStarTem(MFCSTO)
      INTEGER iR, jR, I, hFile, NewNumOfRef
      INTEGER iOrdTem(MFCSTO)
      REAL    H(1:3), DERS(1:6), ArgKKtem(MFCSTO), Resolution, cut_off_2theta

      filnam_root = filnmr
      NINIT = 1
      CALL PREFIN(PNAME)
      CALL SYMOP
      CALL RECIP
      IF (IBMBER .NE. 0) RETURN
      CALL OPSYM(1)
!O      WRITE(76,*) ' Number of reflections ', NumOfRef
      DO iR = 1, NumOfRef
        DO I = 1, 3
          H(i) = FLOAT(jHKL(I,iR))
        ENDDO
        CALL CELDER(H, DERS)
!C wavelength = 1 Angstrom
!O        WRITE(76,*) '**',(jHKL(I,iR),I=1,3), DSTAR2, sthl
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
!O        WRITE(76,*) '**>',ArgKKtem(iR)
      ENDDO
      CALL Sort_Real(ArgKKtem, iOrdTem, NumOfRef)
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SX_Page1a)
      CALL WDialogGetReal(IDF_MaxResolution, Resolution)
      CALL PopActiveWindowID
      cut_off_2theta = 2.0 * ASIND(1.0/(2.0*Resolution))
      NewNumOfRef = 0
      DO iR = 1, NumOfRef
        jR = iOrdTem(iR)
        IF (ArgKKTem(jR) .LE. cut_off_2theta) THEN
          NewNumOfRef = NewNumOfRef + 1
          AIOBS(iR) = AJOBS(jR)
          WTi(iR) = WTj(jR)
          DSTAR(iR) = dstartem(jR)
          RefArgK(iR) = ArgKKTem(jR)
          DO I = 1, 3
            iHKL(I,iR) = jHKL(i,jR)
          ENDDO
        ENDIF
!O        WRITE(76,*) '>> ',iR,'>> ', (iHKL(I,iR),I=1,3), RefArgK(iR), AIOBS(iR), 1.0/(WTI(iR))
      ENDDO
      NumOfRef = NewNumOfRef
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