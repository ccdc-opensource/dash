!
!*****************************************************************************
!
      SUBROUTINE Quick_Pawley

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
!
!   Type declarations
!
      INCLUDE 'PARAMS.INC'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      LOGICAL PawleyOptionChosen 

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),                      &
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),                            &
     &YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,           &
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,            &
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      INCLUDE 'GLBVAR.INC'
      INTEGER ieocc
      INTEGER Quick_Pawley_Fit

!.. CCSL common blocks included - take care!
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6), &
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /PRZERO/ZEROSP(6,9,5),KZROSP(6,                            &
     & 9,5),DKDZER(6),NZERSP(9,5)
      REAL    CHISQ,RWPOBS,RWPEXP
      COMMON /PRCHISQ/ CHISQ,RWPOBS,RWPEXP

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),                                &
     &NumPeakFitRange,CurrentRange,                                     &
     &IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),                           &
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),       &
     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),     &
     &itypot(mobstic),iordot(mobstic),                                  &
     &uobstic(20,mobstic),zobstic(20,mobstic)

! Save the boxes from Pawley fit to Pawley fit
      REAL RLastValues(3)
      INTEGER ILastValues(2)
      DATA RLastValues / 0.0,0.0,0.0 /
      DATA ILastValues / 0,0/
      SAVE RLastValues,ILastValues
! Local variables logging errors in the pawley fit
      INTEGER PawleyEigError
      INTEGER PawleyErrorLog  

      ItemX=IXPos_IDD_Pawley_Status
      ItemY=IYPos_IDD_Pawley_Status
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogShow(ITemX,ItemY,0,Modeless)
      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Skip,Enabled)
 555  CALL WDialogClearField(IDF_Pawley_Cycle_Number)
      CALL WDialogClearField(IDF_Pawley_Refinement_Number)
      PawleyEigError = PawleyErrorLog(2) ! Reset the log messages

      CALL Quick_Pawley_Preparation 

      IF (SkipPawleyRef) THEN
!.. We don't want to stay in Pawley mode anymore - back to peak fitting mode
        CALL WDialogHide()
      RETURN
      ELSE
        ieocc  = Quick_Pawley_Fit()
! Check for an error
        IF (ieocc .eq. 0 .OR. ieocc .eq. -2) THEN
! An error occurred, so pop up a box to say so and then
! skip this refinement
          CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk,           &
             "The refinement was unsuccessful!"//CHAR(13)//            &
                 "Possible causes could be too many peak parameters"   &
              //CHAR(13)//"or bad data at high angles.",               &
             "Ill-conditioned refinement")

! Reset the R-values if possible
          IF (LastValuesSet) THEN
            CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(f12.2)') 
            CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
            CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
            CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
            CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
            CALL retrieve_polybackup()
          END IF
!>> JCC Need to back-copy the arrays here 
!>> Also decrement the number of Pawley refinements since it failed
            NumPawleyRef = NumPawleyRef - 1
            GOTO 555
        ELSE IF (ieocc .eq. -1) THEN
            NumPawleyRef = NumPawleyRef - 1
!>> Return to data viewing
            CALL WDialogHide()
!>> This handles cases where the number of reflections is exceeded
            CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk,         &
                          "Sorry, can only Pawley refine a maximum"//  &
                          "of 400 reflections."//CHAR(13)//            &
                          "You must truncate your data set"//CHAR(13), &
                                  "Refinement not possible")
!         IF (winfoDialog(4) .EQ. 1) THEN
!                 CALL TruncateData(20.0)
!         END IF
          RETURN
        ELSE
          PawleyEigError = PawleyErrorLog(2) ! Check the log messages and reset
          IF (PawleyEigError .GT. 0) CALL PawleyWarning
! Question - should we get the backup back here rather than allow users to continue
! with this?
        ENDIF
        ipt=0
        CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
        CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
        CALL WDialogFieldState(IDB_PawRef_Skip,Disabled)
        CALL WDialogFieldState(IDB_PawRef_Accept,Enabled)
        CALL WDialogFieldState(IDB_PawRef_Reject,Enabled)
!       IF (.NOT.LastValuesSet) THEN
            CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
            CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
!       END IF
        PawleyOptionChosen = .FALSE.
        DO WHILE(.NOT. PawleyOptionChosen)
          CALL GetEvent
          SELECT CASE (EventType)
            CASE (PushButton)
              IDNumber=EventInfo%Value1
              SELECT CASE (IDNumber)
                CASE (IDB_PawRef_Reject)
                  PawleyOptionChosen = .TRUE.
!>> JCC Err, if we reject, we should not enable structure solution
!>> go back to the start of the loop
! Was                    goto 444
! Now
                CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
                CALL WDialogFieldState(IDB_PawRef_Skip,Enabled)
!>> JCC Reset the R-values if possible
                IF (LastValuesSet) THEN
                  CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)') 
                  CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
                  CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
                  CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                  CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                  CALL retrieve_polybackup
                END IF
                GOTO 555
                CASE (IDB_PawRef_Accept)
!.. update the profile and stay with the Pawley refinement
!.. upload the cell constants and zeropoint from the Pawley refinement
                  DO II=1,3
                    CELLPAR(II)=CELL(II,1,1)
                    JJ=II+3
                    CELLPAR(JJ)=DEGREE(ARCCOS(CELL(II,2,1)))
                  END DO
                  CALL Upload_Cell_Constants()
                  ZEROPOINT = ZEROSP(1,1,1)
                  CALL Upload_Zero_Point() 
                  CALL Generate_TicMarks()
                  CALL Load_Pawley_Pro
!>> JCC Save the settings
                  CALL WDialogSelect(IDD_Pawley_Status)
                  CALL WDialogGetReal(IDF_Pawley_Cycle_Rwp,RLastValues(1)) 
                  CALL WDialogGetReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2))
                  CALL WDialogGetReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3))
                  CALL WDialogGetInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                  CALL WDialogGetInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                  LastValuesSet = .TRUE.
                  CALL make_polybackup
!>> Set the file names to point to the poly files
                  CALL set_saFileNames('polyp')
!>> Disable the Solve button until the user does a Save
                  CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
                  GOTO 444
              END SELECT
          END SELECT
      END DO
!.. We can only get here if we have accepted the refinement
!.. update and on to structure solution
 444  CALL WDialogSelect(IDD_Pawley_Status)
      IF (LastValuesSet) THEN
!        CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
        CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
      END IF
      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Skip,Enabled)
      CALL SetModeMenuState(0,0,1)

      GOTO 555
      END IF

      END SUBROUTINE Quick_Pawley
!
!*****************************************************************************
!
      SUBROUTINE Quick_Pawley_Preparation
!.. This is the routine that prepares the Quick Pawley file
!.. Multiple checks before attempting to perform quick Pawley
!.. We need 
!..    (i)   lattice constants
!..    (ii)  space group
!..    (iii) wavelength
!..    (iv)  diffraction file for range limits 
!..            (strictly not necessary - we could put in a 2 theta max of 60 degrees
!..             and redo the tic marks when we load in the data.)
!..   Check the lattice constants
!..   Check the wavelength
!..   Check the space group

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC' ! Contains JRadOption
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'Lattice.inc'

      CHARACTER(LEN=80) :: BackStr

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),                        &
     &PkFnEsd(MPkDes,Max_NPFR),PkFnCal(MPkDes,Max_NPFR),                &
     &PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes),                        &
     &PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR),        &
     &PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),          &
     &PkPosAv(MAX_NPFR)

      parameter (msymmin=10)
      character*20 symline
      common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),                      &
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)                                  
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),                            &
     &YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)                    
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,           &
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,            &
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      COMMON /GRDBCK/IBACK,NBACK(5),ARGBAK(100,5),                      &
     & BACKGD(100,5),KBCKGD(100,5),NBK,LBKD(20),ZBAKIN
      LOGICAL ZBAKIN

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      COMMON /PAWREFCMN/ XRANMIN,XRANMAX,NPawBack

      CHARACTER*4 ChRadOption(4)
      DATA CHRADOPTION /'LABX','SYNX','SYNX','TOFN'/

      INTEGER    :: I, IDNUMBER
      INTEGER SaveProject
      LOGICAL FnUnitCellOK ! Function
      LOGICAL FnWaveLengthOK ! Function
      REAL    WavelengthOf ! Function

      IF (.NOT. BACKREF) THEN
        NPawBack = 2
        CALL WDialogFieldState(IDF_IDF_PawRef_NBack,Disabled)
      ENDIF
!... Check what's happening in IDD_Pawley_Status
      CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
! Now check on which button was pressed ...
      IF (NumPawleyRef .EQ. 0) THEN
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefCell_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefZero_Check,Enabled)
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
        NTCycles=3
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
!>> JCC Only change the setting if this is the second Pawley fit
      ELSE IF (NumPawleyRef .EQ. 1) THEN
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Enabled)
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Checked)
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)
        NTCycles=5
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
      END IF
      CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
      SkipPawleyRef = .FALSE.
      CALL delete_polybackup
      DO WHILE (.NOT. SkipPawleyRef)
        CALL GetEvent
        SELECT CASE (EventType)
          CASE (PushButton)
            IDNumber = EventInfo%Value1
            SELECT CASE (IDNumber)
              CASE (IDF_PawRef_Refine)
                GOTO 888
              CASE (IDB_PawRef_Skip)
                SkipPawleyRef = .TRUE.
              CASE (IDB_PawRef_Save)
                IF (SaveProject() .EQ. 1) THEN
                  CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
                  CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
                END IF
              CASE (IDF_PawRef_Solve)
                CALL Load_Pawley_Pro
                CALL WDialogHide()
                IXPos_IDD_Pawley_Status = WInfoDialog(6)
                IYPos_IDD_Pawley_Status = WInfoDialog(7)
                IXPos_IDD_SA_Input = IXPos_IDD_Pawley_Status
                IYPos_IDD_SA_Input = IYPos_IDD_Pawley_Status
                FromPawleyFit = .TRUE.
                CALL Pawley_Limits_Save()
                CALL SA_Main()
!.. Reload the Pawley profile ...
                CALL Pawley_Limits_Restore()
                CALL Load_Pawley_Pro
                SkipPawleyRef = .TRUE.
            END SELECT
        END SELECT
      END DO
      IF (SkipPawleyRef) RETURN
 888  NumPawleyRef = NumPawleyRef + 1
      CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
      CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)    
!
!... End of check in IDD_Pawley_Status
!
!      write(76,*) ' Space group number         : ',SGNumStr(IPosSg)
!      write(76,*) ' Space Group (IT tables)    : ',SGHMaStr(IPosSg)
!      write(76,*) ' Space Group Hall symbol    : ',SGHalStr(IPosSg)
!      write(76,*) ' Space Group explicit symbol: ',SGShmStr(IPosSg)
!
!.. We should only proceed with this if we have good cell constants 
!.. If no wavelength then assume Cu Ka1 wvln=1.54051
!..
!.. Write out the data file ...
!.. We should check if there are data to write out!
      IF (nbin .GT. 0) THEN
!.. Allow a maximum of 300 reflections
        IF (ntic .EQ. 0) RETURN
        IF (ntic .GT. 300) THEN
          xranmax = MIN(xpmax,argk(300))
        ELSE
          xranmax = xpmax
        END IF
        xranmin = xpmin

! JCCOriginal code - seems to cause the reflection loss bug
!        xranmax=xbin(nbin)

! Substituting with this line seems to fix this bug? Is this a reasonable fix?
! JvdS @ Doesn't this undo all of the above range checking?
! JvdS if there >300 reflections / tic marks, all of them will be included this way?
        xranmax = xpmax
        IF (NumInternalDSC .NE. DataSetChange) THEN
          OPEN(41,file='polyp.dat',status='unknown')
          DO i=1,nbin
            IF (xbin(i) .GT. xranmax) GOTO 4110
            WRITE(41,4100) xbin(i),yobin(i),ebin(i)
 4100       FORMAT(F10.4,2F12.2)
          END DO
 4110     CLOSE(41)
        END IF
        NumInternalDSC=DataSetChange
      ELSE
        RETURN
      END IF
      IF (.NOT. FnUnitCellOK()) RETURN
      OPEN(42,FILE='polyp.ccl',status='unknown')
      WRITE(42,4210) 
 4210 FORMAT('N Polyfitter file for quick Pawley refinement')
      WRITE(42,4220) (CellPar(I),I=1,6)
 4220 FORMAT('C ',3F10.5,3F10.3)
      WRITE(42,4230) 
 4230 FORMAT('F C 2 2.31 20.8439 1.02 10.2075 ',                        &
      '1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0') 
      IF (NumberSGTable.ge.1) THEN
        CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
        IF (nsymmin .GT. 0) THEN
          DO isym=1,nsymmin
            WRITE(42,4235) symline(isym)
 4235       FORMAT('S ',a)
          END DO
        END IF
      END IF
      WRITE(42,4240) NTCycles, ChRadOption(JRadOption)
 4240 FORMAT('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0'/              &
      'L REFI PAWL'/                                                    &
      'L SORC ', A4/                                                    &
      'L WGHT 3')
      CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
      IRtyp = 2-Item
      WRITE(42,4245) IRTYP,xranmin,xranmax
 4245 FORMAT('L RTYP  'I3,2F10.3,'  0.001')
      IF (.NOT. FnWaveLengthOK()) ALambda = WavelengthOf('Cu')
      WRITE(42,4250) ALambda
 4250 FORMAT('L WVLN ',F10.5)
      IF ((ZeroPoint .LT. -1.0) .OR. (ZeroPoint .GT. 1.0)) ZeroPoint = 0.0
      WRITE(42,4260) ZeroPoint
 4260 FORMAT('L ZERO ',F10.5)
      CALL WDialogGetReal(IDF_Slim_Parameter,SLIMVALUE)
      WRITE(42,4270) SCALFAC,SLIMVALUE
 4270 FORMAT('L SCAL   ',F7.5,/                                         &
     &'L SLIM ',F5.2,' '/                                               &
     &'L REFK 10.0'/                                                    &
     &'L PKCN TYPE 1'/                                                  &
     &'L PKFN TYPE 3'/                                                  &
     &'L PKFN LIMS 0.005')
!>> JCC Need to check these variables, and set them to some decent defaults
!>> Currently the default values are all zero, which invariably fail.
      WRITE(42,4271) PkFnVarVal(1,1),PkFnVarVal(2,1)
      WRITE(42,4272) PkFnVarVal(1,2),PkFnVarVal(2,2)
      WRITE(42,4273) PkFnVarVal(1,3)
      WRITE(42,4274) PkFnVarVal(1,4)

 4271 FORMAT('L PKFN SIGM ',2F8.4)
 4272 FORMAT('L PKFN GAMM ',2F8.4)
 4273 FORMAT('L PKFN HPSL ',F8.4)
 4274 FORMAT('L PKFN HMSL ',F8.4)
!>>
!>> JCC Error! NumPawleyRef hasalready been incremented ...
! Was      If (NumPawleyRef.eq.0) then
! Now
      IF (NumPawleyRef .EQ. 1) THEN
        DO ipb = 1, NPawBack
          backgd(ipb,1) = 0.0
        END DO
      ELSE
        IF (NPawBack.gt.NBack(1)) THEN
          DO ipb = NBack(1), NPawBack
            backgd(ipb,1) = 0.0
          END DO
        END IF
      END IF
      nblin=1+(NPawBack-1)/5
      kk=0
      DO inb=1,nblin
        n1=5*(inb-1)
        n2=MIN(n1+5,NPawBack)-n1
        backstr='L BACK 2'
        knb=7
        IF (inb.eq.1) knb=9
        DO jnb=1,n2
          k1=knb+12*(jnb-1)
          kk=kk+1
          WRITE(backstr(k1:k1+11),'(F11.3)') backgd(kk,1)
        END DO
        WRITE(42,4280) backstr
 4280 FORMAT(a)
      END DO
      WRITE(42,4300) 
 4300 FORMAT('L VARY ONLY ALL INTS')
      CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
      IF (Item .EQ. 1) WRITE(42,4310)
 4310 FORMAT('L VARY ALL BACK ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefCell_Check,Item)
      IF (Item .EQ. 1) WRITE(42,4320)
 4320 FORMAT('L VARY ALL CELL ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefZero_Check,Item)
      IF (Item .EQ. 1) WRITE(42,4330)
 4330 FORMAT('L VARY ZERO 1 ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefSigm1_Check,ISigm1)
      CALL WDialogGetCheckBox(IDF_PawRef_RefSigm2_Check,ISigm2)
      CALL WDialogGetCheckBox(IDF_PawRef_RefGamm1_Check,IGamm1)
      CALL WDialogGetCheckBox(IDF_PawRef_RefGamm2_Check,IGamm2)
      IF (ISigm1.EQ.1) WRITE(42,4410)
      IF (ISigm2.EQ.1) WRITE(42,4420)
      IF (IGamm1.EQ.1) WRITE(42,4430)
      IF (IGamm2.EQ.1) WRITE(42,4440)
 4410 FORMAT('L VARY SIGM 1')
 4420 FORMAT('L VARY SIGM 2')
 4430 FORMAT('L VARY GAMM 1')
 4440 FORMAT('L VARY GAMM 2')
      CLOSE(42)    

      END SUBROUTINE Quick_Pawley_Preparation
!
!*****************************************************************************
!
      INTEGER FUNCTION Quick_Pawley_Fit

      USE WINTERACTER

! DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
! A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES
!
      INCLUDE 'PARAMS.INC'
      
      EXTERNAL PCCN01,PFCN03,DUMMY,CALPR
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      DIMENSION ALSQ(QPFDIM)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),            &
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      COMMON/iounit/lpt,iti,ito,iplo,luni,iout
      INTEGER matsz
      CHARACTER*6 xxx
      CHARACTER*10 fname
!>> JCC Declaration: FORTY is now an integer function
      INTEGER FORTY

      fname='polyp'
      xxx='CN11LS'
      MATSZ=QPFDIM
      NINIT=1
!>> JCC trap the return status
      CALL make_polybackup ! make a backup of the polyp files
      Quick_Pawley_Fit = FORTY(xxx,ALSQ,MATSZ,PCCN01,PFCN03,DUMMY,CALPR,fname)
!>> JCC Trap for an error on file opening
      IF (ICRYDA .NE. -1) CALL CLOFIL(ICRYDA)
      IF (IO10 .NE. -1)   CALL CLOFIL(IO10)
      CALL CLOFIL(lpt)

      END FUNCTION Quick_Pawley_Fit
!
!*****************************************************************************
!
      SUBROUTINE Load_Pawley_PRO

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),                      &
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),                &
     &YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,           &
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,            &
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),                                &
     &NumPeakFitRange,CurrentRange,                                     &
     &IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),                           &
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),       &
     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),     &
     &itypot(mobstic),iordot(mobstic),                                  &
     &uobstic(20,mobstic),zobstic(20,mobstic)

!      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      NOBS=NPTS
      NBIN=NPTS
      DO I=1,NBIN
        XBIN(I)=ZARGI(I)
        YOBIN(I)=ZOBS(I)
        YCBIN(I)=ZCAL(I)
        YBBIN(I)=ZBAK(I)
        EBIN(I)=ZDOBS(I)
        XOBS(I)=ZARGI(I)
        YOBS(I)=ZOBS(I)
        YCAL(I)=ZCAL(I)
        YBAK(I)=ZBAK(I)
        EOBS(I)=ZDOBS(I)
      END DO
      IPTYPE=2
      CALL Profile_Plot(IPTYPE)

      END SUBROUTINE Load_Pawley_PRO
!
!*****************************************************************************
!
      SUBROUTINE PAWLEY_LIMITS_SAVE

      INCLUDE 'params.inc'
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,           &
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,            &
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      COMMON /PAWPROFRAN/ XPPMIN,XPPMAX,YPPMIN,YPPMAX,                  &
     &XPPGMIN,XPPGMAX,                                                  &
     &YPPGMIN,YPPGMAX,XPPGMINOLD,XPPGMAXOLD,YPPGMINOLD,YPPGMAXOLD
      COMMON /PAWPROFIPM/ IPPMIN,IPPMAX,IPPMINOLD,IPPMAXOLD
      COMMON /PAWPROFMOR/ NPPFR 
    
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,                &
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),              &
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)    

      NPPFR=NumPeakFitRange
      NumPeakFitRange=0
      XPPMIN=XPMIN
      XPPMAX=XPMAX
      YPPMIN=YPMIN
      YPPMAX=YPMAX
      XPPGMIN=XPGMIN
      XPPGMAX=XPGMAX
      YPPGMIN=YPGMIN
      YPPGMAX=YPGMAX

      XPPGMINOLD=XPGMINOLD
      XPPGMAXOLD=XPGMAXOLD
      YPPGMINOLD=YPGMINOLD
      YPPGMAXOLD=YPGMAXOLD
      IPPMIN=IPMIN
      IPPMAX=IPMAX
      IPPMINOLD=IPMINOLD
      IPPMAXOLD=IPMAXOLD

      END SUBROUTINE PAWLEY_LIMITS_SAVE
!
!*****************************************************************************
!
      SUBROUTINE PAWLEY_LIMITS_RESTORE

      INCLUDE 'PARAMS.INC'

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,           &
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,            &
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      COMMON /PAWPROFRAN/ XPPMIN,XPPMAX,YPPMIN,YPPMAX,                  &
     &XPPGMIN,XPPGMAX,                                                  &
     &YPPGMIN,YPPGMAX,XPPGMINOLD,XPPGMAXOLD,YPPGMINOLD,YPPGMAXOLD
      COMMON /PAWPROFIPM/ IPPMIN,IPPMAX,IPPMINOLD,IPPMAXOLD
      COMMON /PAWPROFMOR/ NPPFR 
   
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,                &
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),              &
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)      

      NumPeakFitRange=NPPFR

      XPMIN=XPPMIN
      XPMAX=XPPMAX
      YPMIN=YPPMIN
      YPMAX=YPPMAX
      XPGMIN=XPPGMIN
      XPGMAX=XPPGMAX
      YPGMIN=YPPGMIN
      YPGMAX=YPPGMAX

      XPGMINOLD=XPPGMINOLD
      XPGMAXOLD=XPPGMAXOLD
      YPGMINOLD=YPPGMINOLD
      YPGMAXOLD=YPPGMAXOLD
      IPMIN=IPPMIN
      IPMAX=IPPMAX
      IPMINOLD=IPPMINOLD
      IPMAXOLD=IPPMAXOLD

      END SUBROUTINE PAWLEY_LIMITS_RESTORE
!
!*****************************************************************************
!
      SUBROUTINE make_polybackup

      USE WINTERACTER

      LOGICAL copypik, copytic, copyhcv
      COMMON / PBCKUP / copypik, copytic, copyhcv

      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE.

!>> Make a backup copy of the polyp.pik file to recover in event of an error
      INQUIRE(FILE = 'polyp.pik', exist=copypik)
      inferr = InfoError(1)
      IF (copypik) THEN
            CALL IOScopyFile('polyp.pik', 'polyp.pbk')
            inferr = InfoError(1)
            IF (inferr .NE. 0) copypik = .FALSE.
      END IF

      INQUIRE(FILE = 'polyp.hcv', exist=copyhcv)
      inferr = InfoError(1)
      IF (copyhcv) THEN
            CALL IOScopyFile('polyp.hcv', 'polyp.hbk')
            inferr = InfoError(1)
            if (inferr .NE. 0) copyhcv = .FALSE.
      END IF

      INQUIRE(FILE = 'polyp.tic', exist=copytic)
      inferr = InfoError(1)
      IF (copytic) THEN
            CALL IOScopyFile('polyp.tic', 'polyp.tbk')
            inferr = InfoError(1)
            IF (inferr .NE. 0) copytic = .FALSE.
      END IF

      END SUBROUTINE make_polybackup
!
!*****************************************************************************
!
      SUBROUTINE retrieve_polybackup

      LOGICAL copypik, copytic, copyhcv
      COMMON / PBCKUP / copypik, copytic, copyhcv

      IF (copypik) CALL IOSCopyFile('polyp.pbk','polyp.pik')
      IF (copytic) CALL IOSCopyFile('polyp.tbk','polyp.tic')
      IF (copyhcv) CALL IOSCopyFile('polyp.hbk','polyp.hcv')

      END SUBROUTINE retrieve_polybackup
!
!*****************************************************************************
!
      SUBROUTINE delete_polybackup

      LOGICAL copypik, copytic, copyhcv
      COMMON / PBCKUP / copypik, copytic, copyhcv

      IF (copypik) CALL IOSDeleteFile('polyp.pbk')
      IF (copytic) CALL IOSDeleteFile('polyp.tbk')
      IF (copyhcv) CALL IOSDeleteFile('polyp.hbk')
      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE. 

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

      CHARACTER*(*), INTENT (IN   ) :: SDIFileName

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP
      INTEGER Ieocc
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
        END IF
      END DO
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
      END IF
      DashPikFile(L1:L4)='.pik'
      DashTicFile(L1:L4)='.tic'
      DashHcvFile(L1:L4)='.hcv'
      DashDslFile(L1:L4)='.dsl'
      CALL WRTDSL(DashDslFile,L4,Ieocc) ! Ignore any errors
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
      WRITE(81,8160) PawleyChiSq
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
      INTEGER FUNCTION SaveProject

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      CHARACTER(LEN=80) :: SDIFileName
!      CHARACTER(LEN=255) :: Currentdir
      CHARACTER(LEN=45) :: FILTER
      INTEGER IFLAGS
      
!.. Save the project
      SaveProject = 0
!      CALL IOsDirName(Currentdir)
      IFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Diffraction information files (*.sdi)|*.sdi|'
      SDIFileName = ' '
      CALL WSelectFile(FILTER,IFLAGS,SDIFileName,'Save diffraction information for structure solution')
!.. Go back to original directory
!      CALL IOsDirChange(Currentdir)
      IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (SDIFileName .NE. ' ')) THEN
        CALL CreateSDIFile(SDIFileName)
        CALL Set_saFileNames(SDIFileName(1:LEN_TRIM(SDIFileName) - 4))
        SaveProject = 1
      END IF

      END FUNCTION SaveProject
!
!*****************************************************************************
!
