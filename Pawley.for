      subroutine Quick_Pawley
c
      USE WINTERACTER
      USE druid_header
!
!   Type declarations
!
      TYPE(WIN_MESSAGE) :: MESSAGE
c
      INCLUDE 'DialogPosCmn.inc'
	INCLUDE 'params.inc'
c
      character*80 profile
      logical nodata,PawleyOptionChosen 
      COMMON /PLTYPE/ IPTYPE
c

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),
     &YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      REAL :: CELLPAR,ZEROPOINT,ALAMBDA
C>> JCC Declarations
	INTEGER ieocc
	INTEGER Quick_Pawley_Fit
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
C
C.. CCSL common blocks included - take care!
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /PRZERO/ZEROSP(6,9,5),KZROSP(6,
     & 9,5),DKDZER(6),NZERSP(9,5)
      COMMON /PRCHISQ/ CHISQ,RWPOBS,RWPEXP

C>> JCC Save the boxes from Pawley fit to Pawley fit
	REAL RLastValues(3)
	INTEGER ILastValues(2)
	LOGICAL LastValuessSet
	DATA RLastValues / 0.0,0.0,0.0 /
	DATA ILastValues / 0,0/
	DATA LastValuesSet / .FALSE. /
	SAVE RLastValues,ILastValues,LastValuesSet

C>> Local variables logging errors in the pawley fit
      INTEGER PawleyEigError
	INTEGER PawleyErrorLog	
C
 
C
      INCLUDE 'statlog.inc'
C

!C>> JCC The next common allows low-level setting of SLIM (not accessible through the front
!C     end, but may be in future. I've allowed reading via the dsl file)
	  REAL SLIMVALUE, SCALFAC
	  LOGICAL BACKREF
	  COMMON /PWLYST/ SLIMVALUE, SCALFAC, BACKREF


c

      ItemX=IXPos_IDD_Pawley_Status
      ItemY=IYPos_IDD_Pawley_Status
C>> JCC       CALL WDialogLoad(IDD_Pawley_Status)
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogShow(ITemX,ItemY,0,Modeless)
!      write(76,*) ' IDD_Pawley_Status ',IDD_Pawley_Status
c
      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Skip,Enabled)
 555  CALL WDialogClearField(IDF_Pawley_Cycle_Number)
      CALL WDialogClearField(IDF_Pawley_Refinement_Number)
C>> JCC Leave this information in place always
C      CALL WDialogClearField(IDF_Pawley_Cycle_Rwp)
C      CALL WDialogClearField(IDF_Pawley_Cycle_RwpExp)
C      CALL WDialogClearField(IDF_Pawley_Cycle_ChiSq)
C      CALL WDialogClearField(IDF_Pawley_Cycle_NumPts)
C      CALL WDialogClearField(IDF_Pawley_Cycle_NumRefs)

	PawleyEigError = PawleyErrorLog(2) ! Reset the log messages
      call Quick_Pawley_Preparation 
      if (SkipPawleyRef) then
c.. We don't want to stay in Pawley mode anymore - back to peak fitting mode
        call WDialogHide()
	return
      else
        ieocc  = Quick_Pawley_Fit()
C>> JCC Check for an error
	  if (ieocc .eq. 0 .OR. ieocc .eq. -2) then
C>> An error occurred, so pop up a box to say so and then
C>> skip this refinement
		Call WMessageBox(OkOnly, ExclamationIcon, CommonOk,
     &        "The refinement was unsuccessful!"//CHAR(13)//
     &		"Possible causes could be too many peak parameters"
     &         //CHAR(13)//"or bad data at high angles.",
     &        "Ill-conditioned refinement")

C Now
		
C>> JCC Reset the R-values if possible
		IF (LastValuesSet) THEN
			CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,
     &                         RLastValues(1),'(f12.2)') 
			call WDialogPutReal(IDF_Pawley_Cycle_ChiSq,
     &                         RLastValues(2),'(F12.3)')
              call WDialogPutReal(IDF_Pawley_Cycle_RwpExp,
     &                         RLastValues(3),'(F12.2)')
			call WDialogPutInteger(
     &                         IDF_Pawley_Cycle_NumPts,ILastValues(1))
			call WDialogPutInteger(
     &                         IDF_Pawley_Cycle_NumRefs,ILastValues(2))

!			CALL Unload_Pawley_Pro()
			CALL retrieve_polybackup()
		END IF

C>> JCC Need to back-copy the arrays here 


C>> Also decrement the number of Pawley refinements since it failed
		NumPawleyRef = NumPawleyRef - 1
		goto 555
	  else if (ieocc .eq. -1) then
		NumPawleyRef = NumPawleyRef - 1
C>> Return to data viewing
		call WDialogHide()
C>> This handles cases where the number of reflections is exceeded
		Call WMessageBox(OkOnly, ExclamationIcon, CommonOk,
     &                     "Sorry, can only Pawley refine a maximum"//
     &                     "of 400 reflections."//CHAR(13)//
     &                     "You must truncate your data set"//CHAR(13),
     &				     "Refinement not possible")
C	    IF (winfoDialog(4) .EQ. 1) THEN
C			CALL TruncateData(20.0)
C	    END IF
	    return

        else
		PawleyEigError = PawleyErrorLog(2) ! Check the log messages and reset
	    IF ( PawleyEigError .GT. 0) THEN
			CALL PawleyWarning
	    END IF
! Question - should we get the backup back here rather than allow users to continue
! with this?
	  endif

!	  write(76,*) ' Finished Quick_Pawley_Fit'
	
        ipt=0
        CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
        CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
        CALL WDialogFieldState(IDB_PawRef_Skip,Disabled)
        CALL WDialogFieldState(IDB_PawRef_Accept,Enabled)
        CALL WDialogFieldState(IDB_PawRef_Reject,Enabled)

!	  IF (.NOT.LastValuesSet) THEN
		CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
		CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
!	  END IF
c
        PawleyOptionChosen=.false.
        DO WHILE(.NOT.PawleyOptionChosen)
            CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Pawley loop ',itype,Message%Value1,Message%Value2
            SELECT CASE (ITYPE)
              CASE (Expose,Resize)
				CALL Redraw()
! 
              CASE (MouseButDown)
                    CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
              CASE (KeyDown)
                    CALL Check_KeyDown(MESSAGE)
!
              CASE (PushButton)
                IDNumber=Message%Value1
                SELECT CASE (IDNumber)
                  CASE(IDB_PawRef_Reject)
                    PawleyOptionChosen=.true.
C>> JCC Err, if we reject, we should not enable structure solution
C>> go back to the start of the loop
C Was                    goto 444
C Now
					CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
					CALL WDialogFieldState(IDB_PawRef_Skip,Enabled)
C>> JCC Reset the R-values if possible
					IF (LastValuesSet) THEN
						CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,
     &                         RLastValues(1),'(f12.2)') 
						call WDialogPutReal(IDF_Pawley_Cycle_ChiSq,
     &                         RLastValues(2),'(F12.3)')
                          call WDialogPutReal(IDF_Pawley_Cycle_RwpExp,
     &                         RLastValues(3),'(F12.2)')
						call WDialogPutInteger(
     &                         IDF_Pawley_Cycle_NumPts,ILastValues(1))
						call WDialogPutInteger(
     &                         IDF_Pawley_Cycle_NumRefs,ILastValues(2))
!						CALL Unload_Pawley_Pro()
						CALL retrieve_polybackup
				    END IF
					goto 555
                  CASE(IDB_PawRef_Accept)
C.. update the profile and stay with the Pawley refinement
C.. upload the cell constants and zeropoint from the Pawley refinement
                    DO II=1,3
                      CELLPAR(II)=CELL(II,1,1)
                      JJ=II+3
                      CELLPAR(JJ)=DEGREE(ARCCOS(CELL(II,2,1)))
                    END DO
                    ZEROPOINT=ZEROSP(1,1,1)
C>> JCC         Call WDialogLoad(IDD_Peak_Positions)
        Call WDialogSelect(IDD_Peak_Positions)
        Call WDialogPutReal(IDF_a_refine,CellPar(1),'(F10.5)')
        Call WDialogPutReal(IDF_b_refine,CellPar(2),'(F10.5)')      
        Call WDialogPutReal(IDF_c_refine,CellPar(3),'(F10.5)')      
        Call WDialogPutReal(IDF_alp_refine,CellPar(4),'(F10.3)')      
        Call WDialogPutReal(IDF_bet_refine,CellPar(5),'(F10.3)')      
        Call WDialogPutReal(IDF_gam_refine,CellPar(6),'(F10.3)')
        Call WDialogPutReal(IDF_zeropt_refine,ZeroPoint,'(F10.4)')
                  call Generate_TicMarks()
                  call Load_Pawley_Pro
C>> JCC Save the settings
				CALL WDialogSelect(IDD_Pawley_Status)
			    CALL WDialogGetReal(IDF_Pawley_Cycle_Rwp,
     &                         RLastValues(1)) 
				call WDialogGetReal(IDF_Pawley_Cycle_ChiSq,
     &                         RLastValues(2))
                  call WDialogGetReal(IDF_Pawley_Cycle_RwpExp,
     &                         RLastValues(3))
				call WDialogGetInteger(
     &                         IDF_Pawley_Cycle_NumPts,ILastValues(1))
				call WDialogGetInteger(
     &                         IDF_Pawley_Cycle_NumRefs,ILastValues(2))
				LastValuesSet = .TRUE.
		        call make_polybackup
C>> Set the file names to point to the poly files
				call set_saFileNames('polyp')

C>> Disable the Solve button until the user does a Save
		        CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
                  goto 444
              END SELECT
          END SELECT
      END DO
C.. We can only get here if we have accepted the refinement
C.. update and on to structure solution
 444  CALL WDialogSelect(IDD_Pawley_Status)
	IF (LastValuesSet) THEN
C		CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
		CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
	END IF
	CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Skip,Enabled)
!      write(76,*) ' Now able to solve '
	CALL SetModeMenuState(0,0,1)

      goto 555
      end if
c
      end
c
c
c
      subroutine Quick_Pawley_Preparation
C.. This is the routine that prepares the Quick Pawley file
C.. Multiple checks before attempting to perform quick Pawley
C.. We need 
C..    (i)   lattice constants
C..    (ii)  space group
C..    (iii) wavelength
C..    (iv)  diffraction file for range limits 
C..            (strictly not necessary - we could put in a 2 theta max of 60 degrees
C..             and redo the tic marks when we load in the data.)
C..   Check the lattice constants
C..   Check the wavelength
C..   Check the space group
!
      use Winteracter
      use druid_header
!
!
!   Type declarations
!
      TYPE(WIN_MESSAGE) :: MESSAGE
!
      LOGICAL :: NODATA
      LOGICAL           :: SKIP    = .FALSE.
      LOGICAL :: GOTCELL(6),GOTALLCELL, BackFrom2
      INTEGER           :: I,ITYPE,IDNUMBER,IPW_Option
     
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmn.inc'
	INCLUDE 'params.inc'
C
      CHARACTER(LEN=80) :: BackStr,SDIFileName
!

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),
     &PkFnEsd(MPkDes,Max_NPFR),PkFnCal(MPkDes,Max_NPFR),
     &PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes),
     &PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR),
     &PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),
     &PkPosAv(MAX_NPFR)
C
C>> JCC Cell/Lattice declarations now in an include file
	INCLUDE 'Lattice.inc'
	character(len=45) FILTER
      parameter (msymmin=10)
      character*20 symline
	common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
c
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),
     &YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!
      COMMON /GRDBCK/IBACK,NBACK(5),ARGBAK(100,5),
     & BACKGD(100,5),KBCKGD(100,5),NBK,LBKD(20),ZBAKIN
      LOGICAL ZBAKIN
!
      COMMON /PLTYPE/ IPTYPE
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
!
      COMMON /PAWREFCMN/ XRANMIN,XRANMAX,NPawBack
!
	CHARACTER*4 ChRadOption(4)
	DATA CHRADOPTION /'LABX','SYNX','SYNX','TOFN'/
	COMMON /RadOption/ JRadOption

	INTEGER SaveProject


	IF (.NOT. BACKREF) THEN
		NPawBack = 2
	    CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)
	END IF

C
C... Check what's happening in IDD_Pawley_Status
C
        CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
        CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
! Now check on which button was pressed ...
      If (NumPawleyRef.eq.0) then
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefCell_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefZero_Check,Enabled)
cccc gone        CALL WDialogFieldState(IDF_PawRef_RefWid_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Disabled)
c
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Unchecked)
cccc gone        CALL WDialogPutCheckBox(IDF_PawRef_RefWid_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefSigm1_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefSigm2_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefGamm1_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefGamm2_Check,Unchecked)
        NTCycles=3
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
C>> JCC Only change the setting if this is the second Pawley fit
      ElseIf (NumPawleyRef .EQ. 1) then
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefCell_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefZero_Check,Enabled)
cccc gone        CALL WDialogFieldState(IDF_PawRef_RefWid_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Enabled)
c
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Checked)
cccc gone        CALL WDialogPutCheckBox(IDF_PawRef_RefWid_Check,Unchecked)
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)
        CALL WDialogPutCheckBox(IDF_PawRef_RefSigm1_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefSigm2_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefGamm1_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefGamm2_Check,Unchecked)
        NTCycles=5
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
	Else
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefCell_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefZero_Check,Enabled)
cccc gone        CALL WDialogFieldState(IDF_PawRef_RefWid_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Enabled)
c
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
cccc gone        CALL WDialogPutCheckBox(IDF_PawRef_RefWid_Check,Unchecked)
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)


      End If
	IF (.NOT. BACKREF) THEN
	    CALL WDialogFieldState(IDF_IDF_PawRef_NBack,Disabled)
	ENDIF
      CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
C
      SkipPawleyRef=.false.
	CALL delete_polybackup
      DO WHILE(.NOT.SkipPawleyRef)
        CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Pawley loop ',itype,Message%Value1,Message%Value2
        SELECT CASE (ITYPE)
          CASE (Expose,Resize)
            CALL Redraw()
!
              CASE (MouseButDown)
                    CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
              CASE (KeyDown)
                  CALL Check_KeyDown(MESSAGE)
!
          CASE (PushButton)
            IDNumber=Message%Value1
!	write(76,*) ' IDNumber in Pawley loop is ',IDNUMBER
            SELECT CASE (IDNumber)
              CASE(IDF_PawRef_Refine)
                Goto 888
              CASE(IDB_PawRef_Skip)
                SkipPawleyRef=.true.
              CASE(IDB_PawRef_Save)
			  IF ( SaveProject() .EQ. 1) THEN
	              CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
				  CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
	          END IF
              CASE(IDF_PawRef_Solve)
                call Load_Pawley_Pro
                call WDialogHide()
                IXPos_IDD_Pawley_Status = WInfoDialog(6)
                IYPos_IDD_Pawley_Status = WInfoDialog(7)
!                IWidth_IDD_Pawley_Status = WInfoDialog(8)
!                IHeight_IDD_Pawley_Status = WInfoDialog(9)
                IXPos_IDD_SA_Input = IXPos_IDD_Pawley_Status
                IYPos_IDD_SA_Input = IYPos_IDD_Pawley_Status
!                IWidth_IDD_SA_Input = IWidth_IDD_Pawley_Status
!                IHeight_IDD_SA_Input = IHeight_IDD_Pawley_Status
                FromPawleyFit=.true.
                CALL Pawley_Limits_Save()
                call SA_Main()
!.. Reload the Pawley profile ...
                CALL Pawley_Limits_Restore()
                call Load_Pawley_Pro
                SkipPawleyRef=.true.
            END SELECT
        END SELECT
      END DO
      If (SkipPawleyRef) Return
 888  NumPawleyRef=NumPawleyRef+1
      call WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
      CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)    
C
C... End of check in IDD_Pawley_Status
C
!      write(76,*) ' Space group number         : ',SGNumStr(IPosSg)
!      write(76,*) ' Space Group (IT tables)    : ',SGHMaStr(IPosSg)
!      write(76,*) ' Space Group Hall symbol    : ',SGHalStr(IPosSg)
!      write(76,*) ' Space Group explicit symbol: ',SGShmStr(IPosSg)
C
C.. We should only proceed with this if we have good cell constants 
C.. If no wavelength then assume Cu Ka1 wvln=1.54051
C..
C.. Write out the data file ...
C.. We should check if there are data to write out!
!      write(76,*) ' In QP ',Nbin,NumInternalDSC,DataSetChange
      IF (nbin .GT. 0) THEN
C.. Allow a maximum of 300 reflections
        IF (ntic .EQ. 0) RETURN
        IF (ntic .GT. 300) THEN
          xranmax=MIN(xpmax,argk(300))
        ELSE
          xranmax=xpmax
        END IF
        xranmin=xpmin


! JCCOriginal code - seems to cause the reflection loss bug
!        xranmax=xbin(nbin)

! Substituting with this line seems to fix this bug? Is this a reasonable fix?
! JvdS @ Doesn't this undo all of the above range checking?
! JvdS if there >300 reflections / tic marks, all of them will be included this way?
	  xranmax=xpmax


        IF (NumInternalDSC .NE. DataSetChange) THEN
!          write(76,*) ' Writing polyf.dat',NumInternalDSC,DataSetChange
          OPEN(41,file='polyp.dat',status='unknown')
          DO i=1,nbin
            IF (xbin(i) .GT. xranmax) GOTO 4110
            WRITE(41,4100) xbin(i),yobin(i),ebin(i)
 4100       FORMAT(f10.4,2f12.2)
          END DO
 4110     CLOSE(41)
        END IF
        NumInternalDSC=DataSetChange
      ELSE
        RETURN
      END IF
      IF (.NOT. CELLOK) RETURN
C 
      OPEN(42,file='polyp.ccl',status='unknown')
      WRITE(42,4210) 
 4210 FORMAT('N Polyfitter file for quick Pawley refinement')
      WRITE(42,4220) (CellPar(i),i=1,6)
 4220 FORMAT('C ',3f10.5,3f10.3)
      write(42,4230) 
 4230 format('F C 2 2.31 20.8439 1.02 10.2075 ',
     &'1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0') 
      if (IPosSG.ge.1) then
        call DecodeSGSymbol(SGShmStr(IPosSg))
!        write(76,*) nsymmin,' symmetry operators'
        if (nsymmin.gt.0) then
          do isym=1,nsymmin
            write(42,4235) symline(isym)
 4235       format('S ',a)
          end do
        end if
      end if
      write(42,4240) NTCycles, ChRadOption(JRadOption)
 4240 format('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0'/  
     &'L REFI PAWL'/ 
     &'L SORC ', A4/
     &'L WGHT 3')
        CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
      IRtyp=2-Item
      write(42,4245) IRTYP,xranmin,xranmax
 4245 format('L RTYP  'i3,2f10.3,'  0.001')
      if (WVLNOK) then
        write(42,4250) ALambda
      else
        ALambda=1.54051
        write(42,4250) ALambda
      end if
 4250 format('L WVLN ',f10.5)
      if (zeropoint.gt.-1.0.and.zeropoint.lt.1.0) then
        write(42,4260) zeropoint
      else
        zeropoint=0.0
        write(42,4260) zeropoint
      end if
 4260 format('L ZERO ',f10.5)
C>> JCC Was
C      write(42,4270) SLIMVALUE
C 4270 format('L SCAL   0.01000'/
C     &'L SLIM 0.5'/
C     &'L REFK 10.0'/
C     &'L PKCN TYPE 1'/
C     &'L PKFN TYPE 3'/
C     &'L PKFN LIMS 0.005')
C>> Now
	CALL WDialogGetReal(IDF_Slim_Parameter,SLIMVALUE)
      write(42,4270) SCALFAC,SLIMVALUE
 4270 format('L SCAL   ',f7.5,/
     &'L SLIM ',f5.2,' '/
     &'L REFK 10.0'/
     &'L PKCN TYPE 1'/
     &'L PKFN TYPE 3'/
     &'L PKFN LIMS 0.005')
C>> JCC Need to check these variables, and set them to some decent defaults
C>> Currently the default values are all zero, which invariably fail.
C>> 
      write(42,4271) PkFnVarVal(1,1),PkFnVarVal(2,1)
      write(42,4272) PkFnVarVal(1,2),PkFnVarVal(2,2)
      write(42,4273) PkFnVarVal(1,3)
      write(42,4274) PkFnVarVal(1,4)

 4271 format('L PKFN SIGM ',2f8.4)
 4272 format('L PKFN GAMM ',2f8.4)
 4273 format('L PKFN HPSL ',f8.4)
 4274 format('L PKFN HMSL ',f8.4)
C>>
C>> JCC Error! NumPawleyRef hasalready been incremented ...
C Was      If (NumPawleyRef.eq.0) then
C Now
	If (NumPawleyRef.eq.1) then
        do ipb=1,NPawBack
          backgd(ipb,1)=0.0
        end do
      Else
        If (NPawBack.gt.NBack(1)) then

          do ipb=NBack(1),NPawBack
            backgd(ipb,1)=0.0
          end do
        End If
      End If
!        write(76,*) ' NPawBack ',NPawBack
        nblin=1+(NPawBack-1)/5
        kk=0
        do inb=1,nblin
          n1=5*(inb-1)
          n2=min(n1+5,NPawBack)-n1
          backstr='L BACK 2'
          knb=7
          if (inb.eq.1) knb=9
          do jnb=1,n2
            k1=knb+12*(jnb-1)
            kk=kk+1
            write(backstr(k1:k1+11),'(f11.3)') backgd(kk,1)
          end do
          write(42,4280) backstr
!          write(76,4280) backstr
 4280 format(a)
        end do
      write(42,4300) 
 4300 format('L VARY ONLY ALL INTS')
      CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
      If (Item.eq.1) write(42,4310)
 4310 format('L VARY ALL BACK ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefCell_Check,Item)
      If (Item.eq.1) write(42,4320)
 4320 format('L VARY ALL CELL ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefZero_Check,Item)
      If (Item.eq.1) write(42,4330)
 4330 format('L VARY ZERO 1 ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefSigm1_Check,ISigm1)
      CALL WDialogGetCheckBox(IDF_PawRef_RefSigm2_Check,ISigm2)
      CALL WDialogGetCheckBox(IDF_PawRef_RefGamm1_Check,IGamm1)
      CALL WDialogGetCheckBox(IDF_PawRef_RefGamm2_Check,IGamm2)
      If (ISigm1.eq.1) write(42,4410)
      If (ISigm2.eq.1) write(42,4420)
      If (IGamm1.eq.1) write(42,4430)
      If (IGamm2.eq.1) write(42,4440)
 4410     format('L VARY SIGM 1')
 4420     format('L VARY SIGM 2')
 4430     format('L VARY GAMM 1')
 4440     format('L VARY GAMM 2')
      close(42)    
c
      end
C
C
C>> JCC Was
C LEVEL 50      subroutine Quick_Pawley_Fit
C      subroutine Quick_Pawley_Fit
C>> Now
	integer function Quick_Pawley_Fit
C>> JCC 
	use winteracter
C>> Changed to allow the return of an error status

C DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
C A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES
C
	include 'params.inc'
	
      EXTERNAL PCCN01,PFCN03,DUMMY,CALPR
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      DIMENSION ALSQ(QPFDIM)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      common/iounit/lpt,iti,ito,iplo,luni,iout
      integer matsz
      character*6 xxx
      character*10 fname
C>> JCC Declaration: FORTY is now an integer function
	integer FORTY
	logical cancopy
	integer inferr
C
      fname='polyp'
      xxx='CN11LS'
      MATSZ=QPFDIM
      NINIT=1
!      write(76,*) ' In Quick_Pawley_Fit '
C>> JCC trap the return status
	call make_polybackup ! make a backup of the polyp files
      Quick_Pawley_Fit = FORTY(xxx,ALSQ,MATSZ,PCCN01,
     &						 PFCN03,DUMMY,CALPR,fname)

C>> JCC Trap for an error on file opening
	IF (ICRYDA .NE. -1) CALL CLOFIL(ICRYDA)
      IF (IO10 .NE. -1)   CALL CLOFIL(IO10)
      CALL CLOFIL(lpt)
	return
C
      END
!*****************************************************************************
!
!



      SUBROUTINE Load_Pawley_PRO
!
!      CHARACTER(LEN=256),           INTENT (IN) :: FNAME
!      INTEGER,                      INTENT (IN) :: FLEN
!      LOGICAL, INTENT (IN OUT) :: NoData

	include 'params.inc'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),
     &YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),
     &NumPeakFitRange,CurrentRange,
     &IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
!

      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),
     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT
!
      COMMON /PLTYPE/ IPTYPE
!

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),
     &itypot(mobstic),iordot(mobstic),
     &uobstic(20,mobstic),zobstic(20,mobstic)

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      include 'statlog.inc'
!
      LBIN=1
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
!
      IPTYPE=2
      CALL Profile_Plot(IPTYPE)
!
      NoData=.false.
 999  Continue
      END SUBROUTINE Load_Pawley_PRO
c
c
c
      integer function New_Pawley_Refinement()
c
      USE WINTERACTER
      USE druid_header
c

	include 'params.inc'
      character*80 profile
      logical nodata,PawleyOptionChosen 
C>> JCC added in error check declarations
	integer ieocc
      COMMON /PLTYPE/ IPTYPE
c

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),
     &YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmn.inc'
c
C>> JCC       CALL WDialogLoad(IDD_Pawley_Status)
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogShow(IXPos_IDD_Pawley_Status,
     &IYPos_IDD_Pawley_Status,0,Modeless)
!      write(76,*) ' IDD_Pawley_Status (go with CCN) ',IDD_Pawley_Status
c
      call Quick_Pawley_ReRefine() 
      New_Pawley_Refinement = Quick_Pawley_Fit()

      end function New_Pawley_Refinement
c
c
c
c
c
c
      subroutine Quick_Pawley_ReRefine
C.. This is the routine that reads the CCN file, checks for changes from 
C.. the Pawley status dialogue and writes out a new CCL file
C..
C.. Multiple checks before attempting to perform quick Pawley
C.. We need 
C..    (i)   lattice constants
C..    (ii)  space group
C..    (iii) wavelength
C..    (iv)  diffraction file for range limits 
C..            (strictly not necessary - we could put in a 2 theta max of 60 degrees
C..             and redo the tic marks when we load in the data.)
C..   Check the lattice constants
C..   Check the wavelength
C..   Check the space group
!
      use Winteracter
      use druid_header
!
!
!   Type declarations
!
      TYPE(WIN_MESSAGE) :: MESSAGE
!
      LOGICAL :: NODATA
      LOGICAL           :: SKIP    = .FALSE.
      LOGICAL :: GOTCELL(6),GOTALLCELL, BackFrom2
      INTEGER           :: I,ITYPE,IDNUMBER,IPW_Option
      INCLUDE 'statlog.inc'
	INCLUDE 'params.inc'
C>> JCC Cell/lattice declarations now in an include file
	INCLUDE 'Lattice.inc'
C
      CHARACTER(LEN=80) :: BackStr
      CHARACTER(LEN=80) :: Line
      LOGICAL :: FirstVaryLine
!
      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),
     &PkFnEsd(MPkDes,Max_NPFR),PkFnCal(MPkDes,Max_NPFR),
     &PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes),
     &PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR),
     &PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),
     &PkPosAv(MAX_NPFR)
C
c
      parameter (msymmin=10)
      character*20 symline
	common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
c

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),
     &YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),
     &YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!
      COMMON /PLTYPE/ IPTYPE

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /PAWREFCMN/ XRANMIN,XRANMAX,NPawBack


	IF (.NOT. BACKREF) NPawBack = 2
C
C... Check what's happening in IDD_Pawley_Status
C
! Now check on which button was pressed ...
      If (NumPawleyRef.eq.0) then
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefCell_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefZero_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefWid_Check,Disabled)
c
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Unchecked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefWid_Check,Unchecked)
        NTCycles=3
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
	  IF (.NOT. BACKREF) THEN
		CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)
	    CALL WDialogFieldState(IDF_IDF_PawRef_NBack,Disabled)
	  ENDIF
      Else
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefInts_Check,Disabled)
        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefCell_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefZero_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefWid_Check,Enabled)
c
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefWid_Check,Unchecked)
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,NPawBack)

	      
        NTCycles=5
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
	  IF (.NOT. BACKREF) THEN
	    CALL WDialogFieldState(IDF_IDF_PawRef_NBack,Disabled)
	  ENDIF

      End If
      CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
C
      SkipPawleyRef=.false.
      DO WHILE(.NOT.SkipPawleyRef)
        CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Pawley loop ',itype,Message%Value1,Message%Value2
        SELECT CASE (ITYPE)
          CASE (Expose,Resize)
            CALL Redraw()
!
              CASE (MouseButDown)
                    CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
              CASE (KeyDown)
                  CALL Check_KeyDown(MESSAGE)
!
          CASE (PushButton)
            IDNumber=Message%Value1
!	write(76,*) ' IDNumber in Pawley ReRefine loop is ',IDNUMBER
            SELECT CASE (IDNumber)
              CASE(IDF_PawRef_Refine)
                Goto 888
              CASE(IDB_PawRef_Skip)
                SkipPawleyRef=.true.
            END SELECT
c          CASE(FieldChanged)
c            IDNumber=Message%Value1
c            SELECT CASE (IDNumber)
c              CASE (IDF_IDF_PawRef_NBack)
c                CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
c              CASE (IDF_Pawley_Total_Cycles)
c                CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)
c              CASE (IDF_PawRef_UseInts_Check)
c        CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Checked)
c              CASE (IDF_PawRef_RefInts_Check)
c        CALL WDialogGetCheckBox(IDF_PawRef_RefInts_Check,Checked)
c              CASE (IDF_PawRef_RefBack_Check)
c        CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Checked)
c              CASE (IDF_PawRef_RefCell_Check)
c        CALL WDialogGetCheckBox(IDF_PawRef_RefCell_Check,Checked)
c              CASE (IDF_PawRef_RefZero_Check)
c        CALL WDialogGetCheckBox(IDF_PawRef_RefZero_Check,Checked)
c              CASE (IDF_PawRef_RefWid_Check)
c        CALL WDialogGetCheckBox(IDF_PawRef_RefWid_Check,Unchecked)
c            END SELECT
        END SELECT
      END DO
      If (SkipPawleyRef) Return
 888  NumPawleyRef=NumPawleyRef+1
      Call WDialogGetCheckBox(IDF_PawRef_RefWid_Check,IWid_Check)
      If (IWid_Check.eq.Checked) then
       CALL WDialogGetRadioButton(IDF_PawRef_RefWid_Radio1,IWid_Option)
!       write(76,*) ' IWid_Option ',IWid_Option
      End If
      CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)    
C
C... End of check in IDD_Pawley_Status
C
C.. We should only proceed with this if we have good cell constants 
C.. If no wavelength then assume Cu Ka1 wvln=1.54051
C..
C
!      write(76,*) 'About to open  polyp.ccl and polyp.ccn'
      open(42,file='polyp.ccl',status='unknown')
      open(43,file='polyp.ccn',status='old')
      FirstVaryLine=.true.
C..
   10 read(43,5300,end=900) nl,line
 5300 format(q,a)
      SELECT CASE (line(1:1))
        CASE ('I')
          write(42,4240) NTCycles
 4240     format('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0')
        CASE ('L')
          SELECT CASE (line(3:6))
            CASE('RTYP')
              CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
              IRtyp=2-Item
              write(42,4245) IRTYP,xranmin,xranmax
 4245         format('L RTYP  'i3,2f10.3,'  0.001')
            CASE('VARY')
              If (FirstVaryLine) then
!
      write(42,4300) 
 4300 format('L VARY ONLY ALL INTS')
      CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
      If (Item.eq.1) write(42,4310)
 4310 format('L VARY ALL BACK ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefCell_Check,Item)
      If (Item.eq.1) write(42,4320)
 4320 format('L VARY ALL CELL ')
      CALL WDialogGetCheckBox(IDF_PawRef_RefZero_Check,Item)
      If (Item.eq.1) write(42,4330)
 4330 format('L VARY ZERO 1 ')
      Call WDialogGetCheckBox(IDF_PawRef_RefWid_Check,IWid_Check)
!      write(76,*) ' IWid_Check = ',IWid_Check,Checked
      If (IWid_Check.eq.Checked) then
       CALL WDialogGetRadioButton(IDF_PawRef_RefWid_Radio1,IWid_Option)
!      write(76,*) ' IWid_Check:IWid_Option = ',IWid_Option
       SELECT CASE (IWid_Option)
         CASE(1)
          write(42,4410)
 4410     format('L VARY SIGM 1')
         CASE(2)
          write(42,4420)
 4420     format('L VARY SIGM 2')
         CASE(3)
          write(42,4430)
 4430     format('L VARY GAMM 1')
         CASE(4)
          write(42,4440)
 4440     format('L VARY GAMM 2')
       END SELECT 
      end if
!
              End If
              FirstVaryLine=.false.
            CASE DEFAULT
              write(42,5310) line(:nl)              
          END SELECT
        CASE DEFAULT
          write(42,5310) line(:nl)
 5310     format(a)
      END SELECT
c      write(76,*) ' NPawBack ',NPawBack
c      nblin=1+(NPawBack-1)/5
c      do inb=1,nblin
c        n1=5*(inb-1)
c        n2=min(n1+5,NPawBack)-n1
c        backstr='L BACK 2'
c        knb=7
c        if (inb.eq.1) knb=9
c        do jnb=1,n2
c          k1=knb+4*(jnb-1)
c          backstr(k1:k1+4)=' 0.0'
c        end do
c        write(42,4280) backstr
c 4280 format(a)
c      end do
      goto 10
 900  close(42)
      close(43)    
c
      end
c
c
c
      subroutine CreateSDIFile(SDIFileName)
c
      USE WINTERACTER
      USE druid_header
	USE Variables
c
C>> JCC Cell/Lattice definitions now in an include file
	INCLUDE 'Lattice.inc'
c
      character*80 SDIFileName,pikfile,ticfile,hcvfile,dslfile
      COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP
      include 'statlog.inc'
C>> JCC defns
	INTEGER Ieocc
c
c.. First copy the .pik .tic and .hcv files
c
      LSDI=len_trim(SDIFileName)
c
      If (LSDI.eq.0) then
        CALL WMessageBox(OKOnly,InformationIcon,CommonOK,
     &       'Filename not provided.'//CHAR(13)//
     &       'Try again!',
     &       'Save Diffraction Information File')
      Else
        IDot=0
        Do I=LSDI,1,-1
          If (SDIFileName(I:I).eq.'.') Then
            IDot=I
            Goto 50
          End If
        End Do
 50     pikfile(:LSDI)=SDIFileName(:LSDI)
        ticfile(:LSDI)=SDIFileName(:LSDI)
        hcvfile(:LSDI)=SDIFileName(:LSDI)
C>> JCC added
	  dslfile(:LSDI)=SDIFileName(:LSDI)
        If (IDot.eq.0) Then
          L1=LSDI+1
          L4=LSDI+4
        Else
          L1=LSDI-3
          L4=LSDI
        End If
        pikfile(L1:L4)='.pik'
        ticfile(L1:L4)='.tic'
        hcvfile(L1:L4)='.hcv'
C>> JCC added
	  dslfile(L1:L4)='.dsl'
	  CALL WRTDSL(dslfile,L4,Ieocc) ! Ignore any errors
c
        Call IOSCopyFile('polyp.pik',pikfile)
        Call IOSCopyFile('polyp.tic',ticfile)
        Call IOSCopyFile('polyp.hcv',hcvfile)
c
        open(81,file=SDIFileName(:LSDI),status='unknown')
        write(81,8110) ticfile
        write(81,8120) hcvfile
        write(81,8130) pikfile
	  write (81,8136) DashRawFile
C>> JCC
	  write(81,8135) dslfile
        write(81,8140) (cellpar(i),i=1,6)
        NSG=NumberSGTable
        write(81,8150) NSG,SGNumStr(NSG),SGHMaStr(NSG)
        write(81,8160) PawleyChiSq
 8110   format(' TIC ',a)
 8120   format(' HCV ',a)
 8130   format(' PIK ',a)
 8135   format(' DSL ',a)
 8136   format(' RAW ',a)
 8140   format(' Cell ',3f10.5,3f10.4)
 8150   format(' SpaceGroup ',i4,4x,a12,a12)
 8160   format(' PawleyChiSq ',f10.2)
        close(81)
      End If
c
      end
C
C
      SUBROUTINE PAWLEY_LIMITS_SAVE
!

	include 'params.inc'
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!
      COMMON /PAWPROFRAN/ XPPMIN,XPPMAX,YPPMIN,YPPMAX,
     &XPPGMIN,XPPGMAX,
     &YPPGMIN,YPPGMAX,XPPGMINOLD,XPPGMAXOLD,YPPGMINOLD,YPPGMAXOLD,
     &XPGGMIN,XPGGMAX,YPGGMIN,YPGGMAX
      COMMON /PAWPROFIPM/ IPPMIN,IPPMAX,IPPMINOLD,IPPMAXOLD
      COMMON /PAWPROFMOR/ NPPFR 
!
    
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, 
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), 
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), 
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)    
!
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
!
      XPPGMINOLD=XPGMINOLD
      XPPGMAXOLD=XPGMAXOLD
      YPPGMINOLD=YPGMINOLD
      YPPGMAXOLD=YPGMAXOLD
      IPPMIN=IPMIN
      IPPMAX=IPMAX
      IPPMINOLD=IPMINOLD
      IPPMAXOLD=IPMAXOLD
!
      END
!
      SUBROUTINE PAWLEY_LIMITS_RESTORE
!

	include 'params.inc'

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!
      COMMON /PAWPROFRAN/ XPPMIN,XPPMAX,YPPMIN,YPPMAX,
     &XPPGMIN,XPPGMAX,
     &YPPGMIN,YPPGMAX,XPPGMINOLD,XPPGMAXOLD,YPPGMINOLD,YPPGMAXOLD,
     &XPGGMIN,XPGGMAX,YPGGMIN,YPGGMAX
      COMMON /PAWPROFIPM/ IPPMIN,IPPMAX,IPPMINOLD,IPPMAXOLD
      COMMON /PAWPROFMOR/ NPPFR 
!
   
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, 
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), 
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), 
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)      
!
      NumPeakFitRange=NPPFR
!
      XPMIN=XPPMIN
      XPMAX=XPPMAX
      YPMIN=YPPMIN
      YPMAX=YPPMAX
      XPGMIN=XPPGMIN
      XPGMAX=XPPGMAX
      YPGMIN=YPPGMIN
      YPGMAX=YPPGMAX
!
      XPGMINOLD=XPPGMINOLD
      XPGMAXOLD=XPPGMAXOLD
      YPGMINOLD=YPPGMINOLD
      YPGMAXOLD=YPPGMAXOLD
      IPMIN=IPPMIN
      IPMAX=IPPMAX
      IPMINOLD=IPPMINOLD
      IPMAXOLD=IPPMAXOLD
!
      END


	SUBROUTINE make_polybackup
	use winteracter
	logical copypik, copytic, copyhcv
	common / PBCKUP / copypik, copytic, copyhcv
	copypik = .false.
	copytic = .false.
	copyhcv = .false.

C>> Make a backup copy of the polyp.pik file to recover in event of an error
	INQUIRE(FILE = 'polyp.pik', exist=copypik)
	inferr = InfoError(1)
	IF (copypik) THEN
		CALL IOScopyFile('polyp.pik', 'polyp.pbk')
		inferr = InfoError(1)
		if (inferr .NE. 0) copypik = .FALSE.
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
		if (inferr .NE. 0) copytic = .FALSE.
	END IF

	END SUBROUTINE make_polybackup

	SUBROUTINE retrieve_polybackup
	logical copypik, copytic, copyhcv
	common / PBCKUP / copypik, copytic, copyhcv
	IF (copypik) CALL IOSCopyFile('polyp.pbk','polyp.pik')
	IF (copytic) CALL IOSCopyFile('polyp.tbk','polyp.tic')
	IF (copyhcv) CALL IOSCopyFile('polyp.hbk','polyp.hbk')

	END SUBROUTINE retrieve_polybackup

	SUBROUTINE delete_polybackup
	logical copypik, copytic, copyhcv
	common / PBCKUP / copypik, copytic, copyhcv
	IF (copypik) CALL IOSDeleteFile('polyp.pbk')
	IF (copytic) CALL IOSDeleteFile('polyp.tbk')
	IF (copyhcv) CALL IOSDeleteFile('polyp.hbk')
	copypik = .false.
	copytic = .false.
	copyhcv = .false.	
	END SUBROUTINE delete_polybackup

	SUBROUTINE set_saFileNames(base)
	use variables
	use winteracter
	integer n
	character*(*) base
	n = len_trim(base)
	write(DashHcvFile,'(A,A)') base(1:n),'.hcv'
	write(DashPikFile,'(A,A)') base(1:n),'.pik'
	write(DashTicFile,'(A,A)') base(1:n),'.tic'

	end SUBROUTINE set_saFileNames


	integer function SaveProject
	use winteracter
	use druid_header
	use variables
      character(len=255) :: SDIFileName
	character(len=255) :: Currentdir
	character(len=45) :: FILTER
	integer IFLAGS
	
c.. Save the project
	SaveProject = 0
c      CALL IOsDirName(Currentdir)
      IFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Diffraction information files (*.sdi)|*.sdi|'
	DO I = 1,len_trim(SDIFileName)
	      SDIFileName(I:I)=' '
	END DO

      CALL WSelectFile(FILTER,IFLAGS,SDIFileName,
     &'Save diffraction information for structure solution')

c.. Go back to original directory
c      CALL IOsDirChange(Currentdir)
	IF (WinfoDialog(4) .EQ. CommonOk .AND. 
     &    SDIFileName .NE. ' ') THEN
		Call CreateSDIFile(SDIFileName)
		CALL Set_saFileNames(SDIFileName(1:len_trim(SDIFileName) - 4))
	    SaveProject = 1
	END IF

	end 	function SaveProject