!
!*****************************************************************************
!
      SUBROUTINE ShowPawleyFitWindow
!
! This routine pops up the window for the Pawley refinement
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      LOGICAL         InWizard, InWizardWindow
      INTEGER                                   CurrentWizardWindow
      COMMON /Wizard/ InWizard, InWizardWindow, CurrentWizardWindow

      INTEGER PawleyErrorLog ! Function
      INTEGER IDUMMY
      INTEGER NTCycles

! JvdS This window popping up automatically makes it impossible to 
! play around with indexing. Therefore: when in one of the indexing windows, ignore
! this routine.
      IF ((CurrentWizardWindow .EQ. IDD_PW_Page9) .OR. (CurrentWizardWindow .EQ. IDD_PW_Page8)) RETURN
      CALL SetModeMenuState(-1,1)
      CALL SelectMode(ID_Pawley_Refinement_Mode)
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
      CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
! If the background has been subtracted after the pattern was read in, then the
! order of the background polynomial defaults to 2, otherwise to 10.
      IF (.NOT. BACKREF) THEN
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,2)
      ELSE
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,10)
      ENDIF
      CALL WDialogClearField(IDF_Pawley_Cycle_Number)
      CALL WDialogClearField(IDF_Pawley_Refinement_Number)
      IDUMMY = PawleyErrorLog(2) ! Reset the log messages
      NumPawleyRef = 0
      CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Disabled)
      CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefSigm1_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefSigm2_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefGamm1_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefGamm2_Check,Unchecked)
      NTCycles = 3
      CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
      CALL IOsDeleteFile('polyp.niw')
      CALL WizardWindowShow(IDD_Pawley_Status)
      CALL PopActiveWindowID

      END SUBROUTINE ShowPawleyFitWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithPawleyFitWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      REAL            CELL,       V,     ORTH,        CPARS
      INTEGER                                                     KCPARS
      REAL                                                                   CELESD,        CELLSD
      INTEGER                                                                                            KOM4
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2), KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      REAL            ZEROSP
      INTEGER                        KZROSP
      REAL                                          DKDZER
      INTEGER                                                  NZERSP
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

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

      INTEGER         NPTS
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

      REAL            ZCAL,        ZBAK
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XDIFT, XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XDIFT, XMINT

! Save the boxes from Pawley fit to Pawley fit
      REAL RLastValues(3)
      INTEGER ILastValues(2)
      DATA RLastValues / 0.0,0.0,0.0 /
      DATA ILastValues / 0,0/
      SAVE RLastValues,ILastValues
! Local variables logging errors in the pawley fit
      INTEGER IDUMMY, ipt
      INTEGER PawleyErrorLog  
      INTEGER Quick_Pawley_Fit ! Function
      REAL    DEGREE ! Function
      INTEGER ieocc, II, JJ
      LOGICAL LastValuesSet
      SAVE    LastValuesSet
      LOGICAL SaveProject ! Function
      INTEGER Ilen, IER
      CHARACTER*80 SDIFile

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardCommon
            CASE (IDF_PawRef_Refine)
              CALL WritePawleyRefinementFile
              ieocc  = Quick_Pawley_Fit()
              ipt = 0
              CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
              CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
              SELECT CASE (ieocc)
                CASE (0,-2)
! An error occurred, so pop up a box to say so and then skip this refinement
                  CALL ErrorMessage("The refinement was unsuccessful!"//CHAR(13)//                  &
                                    "Possible causes could be too many peak parameters"//CHAR(13)// &
                                    "or bad data at high angles.")
! Reset the R-values if possible
                  IF (LastValuesSet) THEN
                    CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)') 
                    CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
                    CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
                    CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                    CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                    CALL retrieve_polybackup()
                  ENDIF
! JCC Need to back-copy the arrays here 
! Also decrement the number of Pawley refinements since it failed
                  NumPawleyRef = NumPawleyRef - 1
                  CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
! We want to ignore the newly created .ccn and use the old .ccl, therefore, copy .ccl to .niw
                  CALL IOSCopyFile('polyp.ccl','polyp.niw')
                  CALL WDialogClearField(IDF_Pawley_Cycle_Number)
                  IDUMMY = PawleyErrorLog(2) ! Reset the log messages
                  CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
                  CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
                  CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
                  IF (NumPawleyRef .GT. 0) THEN
                    CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
                    CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
                  ENDIF
                  CALL PopActiveWindowID
                  RETURN
                CASE (-1)
                  NumPawleyRef = NumPawleyRef - 1
! Return to data viewing
                  CALL EndWizardCommon
! This handles cases where the number of reflections is exceeded
                  CALL ErrorMessage("Sorry, can only Pawley refine a maximum of 400 reflections."//CHAR(13)// &
                                    "You must truncate your data set.")
                  CALL PopActiveWindowID
                  RETURN
                CASE DEFAULT
                  IF (PawleyErrorLog(2) .GT. 0) CALL PawleyWarning ! Check the log messages and reset
              END SELECT
              CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Accept,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
              CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
            CASE (IDB_PawRef_Accept)
! update the profile and stay with the Pawley refinement
! upload the cell constants and zeropoint from the Pawley refinement
              DO II=1,3
                CELLPAR(II) = CELL(II,1,1)
                JJ=II+3
                CELLPAR(JJ) = DEGREE(ACOS(CELL(II,2,1)))
              ENDDO
              CALL Upload_Cell_Constants()
              ZeroPoint = ZEROSP(1,1,1)
              CALL Upload_ZeroPoint() 
              CALL Generate_TicMarks()
! The CCSL code has written out a new input file for the next Pawley refinement--polyp.ccn
! As the user has accepted the fit, we can use this file to generate our new input file.
! To flag this to the subroutine, we create the file 'polyp.niw'
              CALL IOsCopyFile('polyp.ccn','polyp.niw')
              CALL Load_Pawley_Pro
! JCC Save the settings
              CALL WDialogSelect(IDD_Pawley_Status)
              CALL WDialogGetReal(IDF_Pawley_Cycle_Rwp,RLastValues(1)) 
              CALL WDialogGetReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2))
              CALL WDialogGetReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3))
              CALL WDialogGetInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
              CALL WDialogGetInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
              LastValuesSet = .TRUE.
              CALL make_polybackup
! Set the file names to point to the poly files
              CALL set_saFileNames('polyp')
! Disable the Solve button until the user does a Save
              CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
              CALL WDialogSelect(IDD_Pawley_Status)
              IF (LastValuesSet) THEN
                CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
              ENDIF
              CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
              CALL SetModeMenuState(0,0)
! JCC Only change the setting if this is the second Pawley fit
              IF (NumPawleyRef .EQ. 1) THEN
                CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
                CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Enabled)
                CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Enabled)
                CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Enabled)
                CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Enabled)
                CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
                CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Checked)
                CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Checked)
                CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,5)
              ENDIF
            CASE (IDB_PawRef_Reject)
              CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
! JCC Reset the R-values if possible
              IF (LastValuesSet) THEN
                CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)') 
                CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
                CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
                CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                CALL retrieve_polybackup
                CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
              ENDIF
              CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
              NumPawleyRef = NumPawleyRef - 1
              CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
            CASE (IDB_PawRef_Save)
              IF (SaveProject()) CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
            CASE (IDF_PawRef_Solve)
! Emulate loading .SDI file for next window
              CALL WDialogSelect(IDD_SAW_Page1)
! If FromPawleyFit read in the HCV, PIK and TIC files from POLYP
              Ilen = LEN_TRIM(DashPikFile)
              SDIFile = DashPikFile(1:Ilen-3)//'sdi'
              CALL WDialogPutString(IDF_SA_Project_Name,SDIFile)
! JvdS GET_LOGREF and GETTIC do the same thing.
              CALL GET_LOGREF(DashTicFile,LEN_TRIM(DashTicFile),IER)
              CALL GETHCV(DashHcvFile,LEN_TRIM(DashHcvFile),IER)
              CALL GETPIK(DashPikFile,LEN_TRIM(DashPikFile),IER)
              NoData = .FALSE.
              CALL ShowWizardWindowZmatrices
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithPawleyFitWindow
!
!*****************************************************************************
!
      SUBROUTINE WritePawleyRefinementFile

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'

      CHARACTER(LEN=80) :: BackStr

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

      INTEGER msymmin
      PARAMETER (msymmin = 10)
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                     symline
      COMMON /symgencmn/ nsymmin, symmin(4,4,msymmin), symline(msymmin)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
      
      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      
      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

      INTEGER          NTIC
      INTEGER                IH
      REAL                               ARGK
      REAL                                           DSTAR
      COMMON /PROFTIC/ NTIC, IH(3,MTIC), ARGK(MTIC), DSTAR(MTIC)

      REAL XRANMIN, XRANMAX
      SAVE XRANMIN, XRANMAX
      INTEGER NPawBack

      INTEGER NPawBack_OLD
      SAVE    NPawBack_OLD ! To test if number of background parameters has changed

      CHARACTER*4 ChRadOption(4)
      DATA CHRADOPTION /'LABX','SYNX','SYNX','TOFN'/

      INTEGER I
      LOGICAL FnUnitCellOK ! Function
      LOGICAL FnWaveLengthOK ! Function
      REAL    WavelengthOf ! Function
      INTEGER NTCycles
      INTEGER JNB, NBLIN, INB, ITEM, ISYM, IRTYP
      INTEGER N1, N2, K1, KNB
      INTEGER tFileHandle
      LOGICAL UsePrevious, FirstVaryLine
      INTEGER nl
      CHARACTER*255 Line
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

! Are these checks in place here? If one of them fails, we shouldn't have been here in the first place.
!
!.. We should only proceed with this if we have good cell constants 
!.. If no wavelength then assume Cu Ka1 wvln=1.54051
!..
!.. Write out the data file ...
!.. We should check if there are data to write out!
      IF (NBIN .LE. 0) RETURN
      IF (.NOT. FnUnitCellOK()) RETURN
      IF (NTIC .EQ. 0) RETURN
!.. Allow a maximum of 300 reflections
! JvdS Why?
      IF (NTIC .GT. 300) THEN
        xranmax = MIN(xpmax,ARGK(300))
      ELSE
        xranmax = xpmax
      ENDIF
      xranmin = xpmin
! JCC Original code - seems to cause the reflection loss bug
!        xranmax=xbin(nbin)

! Substituting with this line seems to fix this bug? Is this a reasonable fix?
! JvdS @ Doesn't this undo all of the above range checking?
! JvdS if there >300 reflections / tic marks, all of them will be included this way?
      xranmax = xpmax
      IF (NumInternalDSC .NE. DataSetChange) THEN
        tFileHandle = 41
        OPEN(tFileHandle,file='polyp.dat',status='unknown')
        DO I = 1, NBIN
          IF (XBIN(I) .GT. xranmax) GOTO 4110
          WRITE(tFileHandle,'(F10.4,F12.2,F12.2)') XBIN(I), YOBIN(I), EBIN(I)
        ENDDO
 4110   CLOSE(tFileHandle)
      ENDIF
      NumInternalDSC = DataSetChange
      NumPawleyRef = NumPawleyRef + 1
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)    
      INQUIRE(FILE = 'polyp.niw', exist=UsePrevious)
      IF (UsePrevious) THEN
        OPEN(42,file='polyp.ccl',status='unknown')
        OPEN(43,file='polyp.niw',status='old')
        FirstVaryLine = .TRUE.
   10   READ(43,5300,END=900) nl, line
 5300   FORMAT(Q,A)
        SELECT CASE (line(1:1))
          CASE ('I')
            WRITE(42,4240) NTCycles
 4240       FORMAT('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0')
          CASE ('L')
            SELECT CASE (line(3:6))
              CASE('RTYP')
                CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
                IRtyp = 2 - Item
                WRITE(42,4245) IRTYP, xranmin, xranmax
 4245           FORMAT('L RTYP  'I3,2F10.3,'  0.001')
              CASE ('VARY')
                IF (FirstVaryLine) THEN
                  WRITE(42,'(A)') 'L VARY ONLY ALL INTS'
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefBack_Check)) WRITE(42,'(A)') 'L VARY ALL BACK '
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefCell_Check)) WRITE(42,'(A)') 'L VARY ALL CELL '
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefZero_Check)) WRITE(42,'(A)') 'L VARY ZERO 1 '
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm1_Check)) WRITE(42,'(A)') 'L VARY SIGM 1'
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm2_Check)) WRITE(42,'(A)') 'L VARY SIGM 2'
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm1_Check)) WRITE(42,'(A)') 'L VARY GAMM 1'
                  IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm2_Check)) WRITE(42,'(A)') 'L VARY GAMM 2'
                ENDIF
                FirstVaryLine = .FALSE.
              CASE ('BACK')
                CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
! If the background should not be varied, or the number of parameters hasn't changed,
! just copy the old lines.
                IF ((Item.EQ.0) .OR. (NPawBack.EQ.NPawBack_OLD)) THEN
                  WRITE(42,'(A)') line(1:nl)              
                ELSE
! Otherwise, set all back ground parameters to zero
! Read all old background lines
                  nblin = 1 + (NPawBack_OLD-1)/5
                  IF (nblin .GT. 1) THEN
                    DO inb = 2, nblin ! we have already read the first line
                      READ(43,5300,END=900) nl, line
                    ENDDO
                  ENDIF
! Write all zeros for the new background
                  nblin = 1 + (NPawBack-1)/5
                  DO inb = 1, nblin
                    n1 = 5*(inb-1)
                    n2 = MIN(n1+5,NPawBack)-n1
                    backstr = 'L BACK 2'
                    knb = 7
                    IF (inb.EQ.1) knb=9
                    DO jnb = 1, n2
                      k1 = knb + 12*(jnb-1)
                      backstr(k1:k1+11) = '      0.000'
                    ENDDO
                    WRITE(42,'(A)') backstr
                  ENDDO
                  NPawBack_OLD = NPawBack
                ENDIF
              CASE DEFAULT
                WRITE(42,'(A)') line(1:nl)              
            END SELECT
          CASE DEFAULT
            WRITE(42,'(A)') line(1:nl)
        END SELECT
        GOTO 10
 900    CLOSE(42)
        CLOSE(43)
        CALL IOsDeleteFile('polyp.niw')
! The file we have just created will remain the new input file until either
! a. the user starts the Pawley refinement from scratch (i.e. exits and re-opens the dialogue)
! b. the user presses 'Accept' to accept the refined parameters
        CALL IOsCopyFile('polyp.ccl', 'polyp.niw')
        CALL PopActiveWindowID
      ELSE
        CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
        tFileHandle = 41
        OPEN(tFileHandle,FILE='polyp.ccl',status='unknown')
        WRITE(tFileHandle,4210) 
 4210 FORMAT('N Polyfitter file for quick Pawley refinement')
        WRITE(tFileHandle,4220) (CellPar(I),I=1,6)
 4220 FORMAT('C ',3F10.5,3F10.3)
        WRITE(tFileHandle,4230) 
 4230 FORMAT('F C 2 2.31 20.8439 1.02 10.2075 1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0') 
        IF (NumberSGTable .GE. 1) THEN
          CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
          IF (nsymmin .GT. 0) THEN
            DO isym = 1, nsymmin
              WRITE(tFileHandle,4235) symline(isym)
 4235         FORMAT('S ',a)
            ENDDO
          ENDIF
        ENDIF
        WRITE(tFileHandle,4241) NTCycles, ChRadOption(JRadOption)
 4241   FORMAT('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0'/              &
        'L REFI PAWL'/                                                    &
        'L SORC ', A4/                                                    &
        'L WGHT 3')
        CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
        IRtyp = 2 - Item
        WRITE(tFileHandle,4246) IRTYP, xranmin, xranmax
 4246   FORMAT('L RTYP  'I3,2F10.3,'  0.001')
        IF (.NOT. FnWaveLengthOK()) ALambda = WavelengthOf('Cu')
        WRITE(tFileHandle,4250) ALambda
 4250   FORMAT('L WVLN ',F10.5)
        IF ((ZeroPoint .LT. -1.0) .OR. (ZeroPoint .GT. 1.0)) ZeroPoint = 0.0
        WRITE(tFileHandle,4260) ZeroPoint
 4260   FORMAT('L ZERO ',F10.5)
!        WRITE(tFileHandle,"('L EXCL ',F7.3,1X,F7.3)") 0.5, 43.0
        CALL WDialogGetReal(IDF_Slim_Parameter,SLIMVALUE)
        WRITE(tFileHandle,4270) SCALFAC, SLIMVALUE
 4270   FORMAT('L SCAL   ',F7.5,/                                         &
     &  'L SLIM ',F5.2,' '/                                               &
     &  'L REFK 10.0'/                                                    &
     &  'L PKCN TYPE 1'/                                                  &
     &  'L PKFN TYPE 3'/                                                  &
     &  'L PKFN LIMS 0.005')
        WRITE(tFileHandle,4271) PkFnVarVal(1,1), PkFnVarVal(2,1)
        WRITE(tFileHandle,4272) PkFnVarVal(1,2), PkFnVarVal(2,2)
        WRITE(tFileHandle,4273) PkFnVarVal(1,3)
        WRITE(tFileHandle,4274) PkFnVarVal(1,4)
 4271   FORMAT('L PKFN SIGM ',2F8.4)
 4272   FORMAT('L PKFN GAMM ',2F8.4)
 4273   FORMAT('L PKFN HPSL ',F8.4)
 4274   FORMAT('L PKFN HMSL ',F8.4)
        IF (.NOT. BACKREF) THEN
          NPawBack =  2
        ELSE
          NPawBack = 10
        ENDIF
        NPawBack_OLD = NPawBack
        nblin = 1 + (NPawBack-1)/5
        DO inb = 1, nblin
          n1 = 5*(inb-1)
          n2 = MIN(n1+5,NPawBack)-n1
          backstr = 'L BACK 2'
          knb = 7
          IF (inb.EQ.1) knb=9
          DO jnb = 1, n2
            k1 = knb + 12*(jnb-1)
            backstr(k1:k1+11) = '      0.000'
          ENDDO
          WRITE(tFileHandle,'(A)') backstr
        ENDDO
        WRITE(tFileHandle,'(A)') 'L VARY ONLY ALL INTS'
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefBack_Check )) WRITE(tFileHandle,'(A)') 'L VARY ALL BACK'
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefCell_Check )) WRITE(tFileHandle,'(A)') 'L VARY ALL CELL'
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefZero_Check )) WRITE(tFileHandle,'(A)') 'L VARY ZERO 1 '
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm1_Check)) WRITE(tFileHandle,'(A)') 'L VARY SIGM 1'
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm2_Check)) WRITE(tFileHandle,'(A)') 'L VARY SIGM 2'
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm1_Check)) WRITE(tFileHandle,'(A)') 'L VARY GAMM 1'
        IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm2_Check)) WRITE(tFileHandle,'(A)') 'L VARY GAMM 2'
        CLOSE(tFileHandle)
      ENDIF   

      END SUBROUTINE WritePawleyRefinementFile
!
!*****************************************************************************
!
      INTEGER FUNCTION Quick_Pawley_Fit

      USE WINTERACTER

! DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
! A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES

      INCLUDE 'PARAMS.INC'
      
      EXTERNAL PCCN01,PFCN03,DUMMY,CALPR
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      DIMENSION ALSQ(QPFDIM)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      COMMON/iounit/lpt,iti,ito,iplo,luni,iout
      INTEGER matsz
      CHARACTER*6 xxx
      CHARACTER*10 fname
      INTEGER FORTY

      fname='polyp'
      xxx='CN11LS'
      MATSZ=QPFDIM
      NINIT=1
! JCC trap the return status
      CALL make_polybackup ! make a backup of the polyp files
      Quick_Pawley_Fit = FORTY(xxx,ALSQ,MATSZ,PCCN01,PFCN03,DUMMY,CALPR,fname)
! JCC Trap for an error on file opening
      IF (ICRYDA .NE. -1) CALL CLOFIL(ICRYDA)
      IF (IO10 .NE. -1)   CALL CLOFIL(IO10)
      CALL CLOFIL(lpt)

      END FUNCTION Quick_Pawley_Fit
!
!*****************************************************************************
!
      SUBROUTINE Load_Pawley_PRO

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         NPTS
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

      REAL            ZCAL,        ZBAK
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)

      INTEGER I

      NBIN = NPTS
      DO I = 1, NBIN
        XBIN(I)  = ZARGI(I)
        YOBIN(I) = ZOBS(I)
        YCBIN(I) = ZCAL(I)
        YBBIN(I) = ZBAK(I)
        EBIN(I)  = ZDOBS(I)
      ENDDO
      IPTYPE = 2
      CALL Profile_Plot

      END SUBROUTINE Load_Pawley_PRO
!
!*****************************************************************************
!
      SUBROUTINE make_polybackup

      USE WINTERACTER

      LOGICAL           copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE.
      copyhkl = .FALSE.

! Make a backup copy of the polyp.pik file to recover in event of an error
      INQUIRE(FILE = 'polyp.pik', exist=copypik)
      inferr = InfoError(1)
      IF (copypik) THEN
        CALL IOsCopyFile('polyp.pik', 'polyp.pbk')
        IF (InfoError(1) .NE. 0) copypik = .FALSE.
      ENDIF
      INQUIRE(FILE = 'polyp.tic', exist=copytic)
      inferr = InfoError(1)
      IF (copytic) THEN
        CALL IOsCopyFile('polyp.tic', 'polyp.tbk')
        IF (InfoError(1) .NE. 0) copytic = .FALSE.
      ENDIF
      INQUIRE(FILE = 'polyp.hcv', exist=copyhcv)
      inferr = InfoError(1)
      IF (copyhcv) THEN
        CALL IOsCopyFile('polyp.hcv', 'polyp.hbk')
        IF (InfoError(1) .NE. 0) copyhcv = .FALSE.
      ENDIF
      INQUIRE(FILE = 'polyp.hkl', exist=copyhkl)
      inferr = InfoError(1)
      IF (copyhkl) THEN
        CALL IOsCopyFile('polyp.hkl', 'polyp.hbl')
        IF (InfoError(1) .NE. 0) copyhkl = .FALSE.
      ENDIF

      END SUBROUTINE make_polybackup
!
!*****************************************************************************
!
      SUBROUTINE retrieve_polybackup

      LOGICAL           copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      IF (copypik) CALL IOsCopyFile('polyp.pbk','polyp.pik')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.pbk polyp.pik unsuccessful')
      IF (copytic) CALL IOsCopyFile('polyp.tbk','polyp.tic')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.tbk polyp.tic unsuccessful')
      IF (copyhcv) CALL IOsCopyFile('polyp.hbk','polyp.hcv')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.hbk polyp.hcv unsuccessful')
      IF (copyhkl) CALL IOsCopyFile('polyp.hbl','polyp.hkl')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.hbl polyp.hkl unsuccessful')

      END SUBROUTINE retrieve_polybackup
!
!*****************************************************************************
!
      SUBROUTINE delete_polybackup

      LOGICAL           copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      IF (copypik) CALL IOsDeleteFile('polyp.pbk')
      IF (copytic) CALL IOsDeleteFile('polyp.tbk')
      IF (copyhcv) CALL IOsDeleteFile('polyp.hbk')
      IF (copyhkl) CALL IOsDeleteFile('polyp.hbl')
      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE. 
      copyhkl = .FALSE. 

      END SUBROUTINE delete_polybackup
!
!*****************************************************************************
!
      SUBROUTINE set_saFileNames(base)

      USE VARIABLES

      CHARACTER*(*), INTENT (IN   ) :: base

      INTEGER n

      n = LEN_TRIM(base)
      IF (n .GT. 75) THEN
        CALL DebugErrorMessage('base too long in set_saFileNames')
        n = 75
      ENDIF
      WRITE(DashHcvFile,'(A,A)') base(1:n),'.hcv'
      WRITE(DashPikFile,'(A,A)') base(1:n),'.pik'
      WRITE(DashTicFile,'(A,A)') base(1:n),'.tic'

      END SUBROUTINE set_saFileNames
!
!*****************************************************************************
!
      SUBROUTINE CreateSDIFile(SDIFileName)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: SDIFileName

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER LSDI, iDot, I, L1, L4
!
!.. First copy the .pik .tic and .hcv files
!
      LSDI = LEN_TRIM(SDIFileName)
      IF (LSDI .GT. 80) THEN
        CALL DebugErrorMessage('SDIFileName too long in CreateSDIFile')
        LSDI = 80
      ENDIF
      IF (LSDI .EQ. 0) THEN
        CALL ErrorMessage('Filename not provided.'//CHAR(13)//'Try again!')
        RETURN
      ENDIF
	DashPikFile = ' '
      DashTicFile = ' '
      DashHcvFile = ' '
      DashDslFile = ' '
      IDot = 0
      DO I = LSDI, 1, -1
        IF (SDIFileName(I:I) .EQ. '.') THEN
          IDot = I
          GOTO 50
        ENDIF
      ENDDO
 50   DashPikFile(1:LSDI) = SDIFileName(1:LSDI)
      DashTicFile(1:LSDI) = SDIFileName(1:LSDI)
      DashHcvFile(1:LSDI) = SDIFileName(1:LSDI)
      DashDslFile(1:LSDI) = SDIFileName(1:LSDI)
      IF (IDot .EQ. 0) THEN
        L1 = LSDI + 1
        L4 = LSDI + 4
      ELSE
        L1 = LSDI - 3
        L4 = LSDI
      ENDIF
      DashPikFile(L1:L4)='.pik'
      DashTicFile(L1:L4)='.tic'
      DashHcvFile(L1:L4)='.hcv'
      DashDslFile(L1:L4)='.dsl'
      CALL WRTDSL(DashDslFile,L4) ! Ignore any errors
      CALL IOSCopyFile('polyp.pik',DashPikFile)
      CALL IOSCopyFile('polyp.tic',DashTicFile)
      CALL IOSCopyFile('polyp.hcv',DashHcvFile)
      OPEN(81,file=SDIFileName(1:LSDI),status='unknown')
      WRITE(81,8110) DashTicFile(1:LEN_TRIM(DashTicFile))
      WRITE(81,8120) DashHcvFile(1:LEN_TRIM(DashHcvFile))
      WRITE(81,8130) DashPikFile(1:LEN_TRIM(DashPikFile))
      WRITE(81,8136) DashRawFile(1:LEN_TRIM(DashRawFile))
      WRITE(81,8135) DashDslFile(1:LEN_TRIM(DashDslFile))
      WRITE(81,8140) (CellPar(I),I=1,6)
      WRITE(81,8150) NumberSGTable,SGNumStr(NumberSGTable),SGHMaStr(NumberSGTable)
      WRITE(81,8160) PAWLEYCHISQ
 8110 FORMAT(' TIC ',A)
 8120 FORMAT(' HCV ',A)
 8130 FORMAT(' PIK ',A)
 8135 FORMAT(' DSL ',A)
 8136 FORMAT(' RAW ',A)
 8140 FORMAT(' Cell ',3F10.5,3F10.4)
 8150 FORMAT(' SpaceGroup ',I4,4X,A12,A12)
 8160 FORMAT(' PawleyChiSq ',F10.2)
      CLOSE(81)

      END SUBROUTINE CreateSDIFile
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveProject

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      CHARACTER(LEN=80) :: SDIFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER IFLAGS
      
!.. Save the project
      SaveProject = .FALSE.
      IFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Diffraction information files (*.sdi)|*.sdi|'
      SDIFileName = ' '
      CALL WSelectFile(FILTER,IFLAGS,SDIFileName,'Save diffraction information for structure solution')
      IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (SDIFileName .NE. ' ')) THEN
        CALL CreateSDIFile(SDIFileName)
        CALL Set_saFileNames(SDIFileName(1:LEN_TRIM(SDIFileName) - 4))
        CALL sa_SetOutputFiles(SDIFileName)
        SaveProject = .TRUE.
      ENDIF

      END FUNCTION SaveProject
!
!*****************************************************************************
!
