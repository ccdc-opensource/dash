! JvdS The following routine is 733 lines
!
      SUBROUTINE SA_MAIN()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
!
! Define some parameters to match those in the resource file
!
      CHARACTER*80 SDIFile
      REAL         rpos
      INTEGER      ipos
      INTEGER      IFlags

      INCLUDE 'PARAMS.INC'
      INCLUDE 'IZMCheck.inc'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      INTEGER IDFZMCheck(CheckSize),IZMNumber(CheckSize)
      INTEGER IDBZMBrowse(CheckSize),IDFZMpars(CheckSize),IDFZMFile(CheckSize)
      INTEGER II

      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,CSH,FSTAR,XP,FOPT
      COMMON /sacmn/ XOPT(NMAX),CSH(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT

      INTEGER  MAXEVL, IPRINT

      PARAMETER (mvar=100)
      DOUBLE PRECISION x,lb,ub,vm,xpreset
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      DOUBLE PRECISION prevub, prevlb ! For saving the previous range
      COMMON /pvalues/ prevub(mvar), prevlb(mvar)
      COMMON /presetr/ xpreset(mvar)
      LOGICAL log_preset
      COMMON /presetl/ log_preset

      DOUBLE PRECISION T0,rt
      COMMON /saparl/ T0,rt
      INTEGER  NS, NT, IER, ISEED1, ISEED2
      COMMON /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2

      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen, alph, bet, f2cmat
      INTEGER ioptb, iopta, ioptt, iz1, iz2, iz3
      COMMON /zmcomi/ ntatm, natoms(maxfrg), &
        ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
        iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
        bet(maxatm,maxfrg),f2cmat(3,3)
      CHARACTER*3 asym
      COMMON /zmcomc/ asym(maxatm,maxfrg)
      INTEGER nfrag
      COMMON /frgcom/ nfrag
      CHARACTER*80 frag_file
      COMMON /frgcha/ frag_file(maxfrg)
      CHARACTER*36 czmpar
      COMMON /zmnpar/ izmpar(maxfrg),&
        czmpar(MaxDOF,maxfrg),kzmpar(MaxDOF,maxfrg),xzmpar(MaxDOF,maxfrg)
      LOGICAL gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)

      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150),&
        TF(150),KTF(150),SITE(150),KSITE(150),&
        ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
!
!C>> JCC Added in declarations
! The implementation has changed - this is now a function
      INTEGER Read_One_Zm
      INTEGER zmread
      INTEGER NextEnabled
      LOGICAL LimsChanged
      DATA LimsChanged / .FALSE. /
      SAVE LimsChanged
      LOGICAL ZmStateChanged
      LOGICAL NoZmatrix
      LOGICAL Confirm ! Function
      INTEGER IHANDLE
      INTEGER IZMVB(1:CheckSize)

!.. If FromPawleyFit read in the HCV, PIK and TIC files from POLYP
      IF (FromPawleyFit) THEN
! JvdS GET_LOGREF and GETTIC do the same thing.
        CALL GET_LOGREF(DashTicFile,LEN_TRIM(DashTicFile),IER)
        CALL GETHCV(DashHcvFile,LEN_TRIM(DashHcvFile),IER)
        CALL GETPIK(DashPikFile,LEN_TRIM(DashPikFile),IER)
      END IF
      CALL SA_Defaults()
      IDFZMCheck(1)=IDF_ZM_file_check1
      IDFZMCheck(2)=IDF_ZM_file_check2
      IDFZMCheck(3)=IDF_ZM_file_check3
      IDFZMCheck(4)=IDF_ZM_file_check4
      IDFZMCheck(5)=IDF_ZM_file_check5
      IDFZMFile(1)=IDF_ZMatrix_file1
      IDFZMFile(2)=IDF_ZMatrix_file2
      IDFZMFile(3)=IDF_ZMatrix_file3
      IDFZMFile(4)=IDF_ZMatrix_file4
      IDFZMFile(5)=IDF_ZMatrix_file5
      IDBZMBrowse(1)=IDB_ZMatrix_Browse1
      IDBZMBrowse(2)=IDB_ZMatrix_Browse2
      IDBZMBrowse(3)=IDB_ZMatrix_Browse3
      IDBZMBrowse(4)=IDB_ZMatrix_Browse4
      IDBZMBrowse(5)=IDB_ZMatrix_Browse5
      IDFZMpars(1)=IDF_ZM_pars1
      IDFZMpars(2)=IDF_ZM_pars2
      IDFZMpars(3)=IDF_ZM_pars3
      IDFZMpars(4)=IDF_ZM_pars4
      IDFZMpars(5)=IDF_ZM_pars5
!>> copy the flags used
      DO ii = 1, CheckSize
        IZMNumber(ii) = IDBZMBrowse(ii)
      END DO
      IZMVB(1) = IDB_ZMatrixView1
      IZMVB(2) = IDB_ZMatrixView2
      IZMVB(3) = IDB_ZMatrixView3
      IZMVB(4) = IDB_ZMatrixView4
      IZMVB(5) = IDB_ZMatrixView5

! Message loop for the first SA window
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
      CALL WDialogSelect(IDD_SAW_Page1)
!C>> JCC I've moved this lot to a separate subroutine. We want to 
!C>> Remember the files/data etc, so only call this on first entry
      CALL ClearZmatrices(IDFZMFile,IDFZMPars,IDFZMCheck,IDBZMBrowse)
      NoZmatrix = .TRUE.
 222  CONTINUE
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
      CALL WDialogSelect(IDD_SAW_Page1)
      IF (FromPawleyFit) THEN
!C>> JCC Added
        NoData = .FALSE.
        CALL WDialogClearField(IDF_SA_Project_Name)
        CALL WDialogFieldState(IDF_SA_Project_Name,Disabled)
        CALL WDialogFieldState(IDB_SA_Project_Browse,Disabled)
        CALL WDialogFieldState(IDB_SA_Project_Open,Disabled)
        CALL WDialogFieldState(IDF_SA_project_name_label,Disabled)
      ELSE
        CALL WDialogFieldState(IDF_SA_Project_Name,Enabled)
        CALL WDialogFieldState(IDB_SA_Project_Browse,Enabled)
        CALL WDialogFieldState(IDB_SA_Project_Open,Enabled)
        CALL WDialogFieldState(IDF_SA_project_name_label,Enabled)
      END IF
      IF (ConvOn) THEN
        CALL WDialogFieldState(IDF_SA_Project_Import,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_SA_Project_Import,Disabled)
      END IF
      ZmStateChanged = .TRUE.
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE (.TRUE.)                  ! Loop until user terminates
! Let's check the z-matrix check boxes
        IF (ZmStateChanged) THEN
          DO II = 1, CheckSize
            CALL WDialogGetCheckBox(IDFZMCheck(II),IZMCheck(II))
          END DO
          DO II = 1, CheckSize
            IF ((IZMCheck(II) .EQ. Checked) .OR. gotzmfile(II) ) THEN
              CALL WDialogFieldState(IDFZMFile(II),Enabled)
              CALL WDialogFieldState(IDBZMBrowse(II),Enabled)
              CALL WDialogFieldState(IDFZMPars(II),Enabled)
              NextEnabled = II + 1
            ELSE
              CALL WDialogFieldState(IDFZMFile(II),Disabled)
              CALL WDialogFieldState(IDBZMBrowse(II),Disabled)
              CALL WDialogFieldState(IDFZMPars(II),Disabled)
              IF (II .LT. 5) THEN
                DO jj = II + 1, CheckSize
                  CALL WDialogFieldState(IDFZMCheck(jj),Disabled)
                  CALL WDialogPutCheckBox(IDFZMCheck(jj),Unchecked)
                  CALL WDialogFieldState(IDFZMFile(jj),Disabled)
                  CALL WDialogFieldState(IDBZMBrowse(jj),Disabled)
                  CALL WDialogFieldState(IDFZMPars(jj),Disabled)
                END DO
              END IF
            END IF
          END DO
          IF (NextEnabled .LT. CheckSize) THEN
            CALL WDialogPutCheckBox(IDFZMCheck(NextEnabled),Unchecked)
          END IF
          ZmStateChanged = .FALSE.
        END IF
        CALL WDialogSelect(IDD_SAW_Page1)
! Enable or disable the "Next" button
        IF (NoZmatrix) THEN
          CALL WDialogFieldState(IDNEXT,Disabled)
        ELSE
          CALL WDialogFieldState(IDNEXT,Enabled)
        END IF
! Start the message loop
        CALL GetEvent
        SELECT CASE (EventType)
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
!C>> JCC Add in new 'clear' button
            CASE (IDF_clear_zmatrix)
              CALL ClearZmatrices(IDFZMFile,IDFZMPars,IDFZMCheck,IDBZMBrowse)
              NoZmatrix = .TRUE.
              ZmStateChanged = .TRUE.                               
            CASE (IDBACK)
! Go back to the Pawley refinement or the initial wizard
! JvdS Started to add SA to Wizard
!              CALL WDialogSelect(IDD_SA_input1)
              CALL WDialogSelect(IDD_SAW_Page1)
! Window will be removed, save current position
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
              IPTYPE = 2
              RETURN
            CASE (IDCANCEL)
              CALL WDialogSelect(IDD_SAW_Page1)
! Window will be removed, save current position
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              IPTYPE = 2
              CALL SetWizardState(1)
              RETURN
            CASE (IDNEXT)
! Go to the next stage of the SA input
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
              CALL WDialogSelect(IDD_SAW_Page1)
! Window will be removed, save current position
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL SA_Parameter_Set
              GOTO 444
            CASE (IDB_SA_Project_Browse)
             IF (.NOT. FromPawleyFit) CALL SDIFileBrowse
            CASE (IDB_SA_Project_Open)
              CALL WDialogSelect(IDD_SAW_Page1)
              SDIFile = ' '
              CALL WDialogGetString(IDF_SA_Project_Name,SDIFile)
              CALL SDIFileOpen(SDIFile)
            CASE (IDB_SA_Project_Import)
!>> JCC Import .. convert a mol/pdb/mol2 file into a zmatrix
              CALL ImportZmatrix
            CASE (IDB_ZMatrix_Browse1, IDB_ZMatrix_Browse2, IDB_ZMatrix_Browse3, &
                  IDB_ZMatrix_Browse4, IDB_ZMatrix_Browse5)
              ZmStateChanged = .TRUE.
              ifrg = 1
              DO WHILE (ifrg .LT. CheckSize .AND. IZMNumber(ifrg) .NE. EventInfo%VALUE1)
                ifrg = ifrg + 1
              ENDDO
              gotzmfile(ifrg) = .FALSE.
              frag_file(ifrg) = ' '
              IFlags = PromptOn + DirChange + AppendExt
              CALL WSelectFile('z-matrix file (*.zmatrix)|*.zmatrix|', &
                   IFlags,frag_file(ifrg),'Load z-matrix file')
!C>> JCC Need to check here to see if the user hit cancel
! So I added a check here
              IF (frag_file(ifrg) .NE. ' ') THEN
                zmread = Read_One_ZM(ifrg)
                IF (zmread .EQ. 0) THEN ! successful read
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
                  CALL WDialogSelect(IDD_SAW_Page1)
                  CALL WDialogPutString(IDFZMFile(ifrg),frag_file(ifrg))
! done within Read_One_ZM >>              gotzmfile(ifrg)=.true.
!>> Now update the front end widget
                  DO II = 1, CheckSize
                    CALL WDialogGetCheckBox(IDFZMCheck(ii),IZMCheck(ii))
                  END DO
                  CALL UpdateZmatrixSelection(IDFZMPars)
! Set the next dialogue on
                  IF (ifrg .LT. CheckSize) THEN
                    CALL WDialogFieldState(IDFZMCheck(ifrg+1),Enabled)
                    CALL WDialogFieldState(IDFZMCheck(ifrg+1),Enabled)
                  END IF
!>> JCC traps for zmatrix reading
                  NoZmatrix = .FALSE.
                ELSE 
                   CALL FileErrorPopup(frag_file(ifrg),zmread)
                END IF ! If the read on the zmatrix was ok
              END IF  ! If the user selected a file
! View individual z-matrices in Mercury
            CASE (IDB_ZMatrixView1, IDB_ZMatrixView2, IDB_ZMatrixView3, IDB_ZMatrixView4, IDB_ZMatrixView5)
              ifrg = 1
              DO WHILE (ifrg .LT. CheckSize .AND. IZMVB(ifrg) .NE. EventInfo%VALUE1)
                ifrg = ifrg + 1
              ENDDO
              IF (.NOT. gotzmfile(ifrg)) THEN
                CALL ErrorMessage('File not found.')
              ELSE
                CALL ViewZmatrix(ifrg)
              ENDIF
          END SELECT
          CASE (FieldChanged)
            SELECT CASE(EventInfo%VALUE1)
!>> JCC Also act on selection of check box
              CASE (IDF_ZM_file_check1,IDF_ZM_file_check2,IDF_ZM_file_check3,&
                IDF_ZM_file_check4,IDF_ZM_file_check5)
! Update the selection
                DO II = 1, 5
                  CALL WDialogGetCheckBox(IDFZMCheck(ii),IZMCheck(ii))
                END DO
                CALL UpdateZmatrixSelection(IDFZMPars)
                ZmStateChanged = .TRUE.        
            END SELECT
        END SELECT
      END DO
!.. We are now on window number 2
 444  CALL WDialogSelect(IDD_SA_input2)
      CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,Modeless)
      DO WHILE (.TRUE.)                                ! Loop until user terminates
        CALL GetEvent
        SELECT CASE (EventType)
!.. Interact with the main window and look at the Pawley refinement...
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_SA2_cancel)
! Go back to the Pawley refinement or the initial wizard
                IXPos_IDD_SA_Input = WInfoDialog(6)
                IYPos_IDD_SA_Input = WInfoDialog(7)
                CALL WDialogHide()
                IPTYPE = 2
                RETURN
              CASE (IDBACK)
! Go back to the 1st window
!>> JCC Check if the limits have changed and warn about it 
                IF (LimsChanged) THEN
                  IF (Confirm("Note: Going back will erase the edits made to the current parameters, overwrite changes?")) LimsChanged = .FALSE.
                END IF
                IF (.NOT. LimsChanged) THEN
                  IXPos_IDD_SA_Input = WInfoDialog(6)
                  IYPos_IDD_SA_Input = WInfoDialog(7)
                  CALL WDialogHide()
                  GOTO 222
                END IF
              CASE (IDNEXT)
! Go to the next stage of the SA input
                IXPos_IDD_SA_Input = WInfoDialog(6)
                IYPos_IDD_SA_Input = WInfoDialog(7)
                CALL WDialogHide() 
!C>> JCC               Call SA_Parameter_Update(CheckSize,IZMCheck)
                GOTO 777
            END SELECT
          CASE (FieldChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_parameter_grid)
                CALL WGridPos(EventInfo%X,IFCol,IFRow)
                SELECT CASE (IFCol)
                CASE(1)
!.. parameter
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,5,IFRow,ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem = MAX(SNGL(lb(IFRow)),xtem)
                    IF (ABS(xtem - x(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    X(IFRow)=DBLE(MIN(SNGL(ub(IFRow)),xtem))
                    CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,sngl(x(IFRow)),'(F12.5)')
                  END IF
                CASE(2)
!.. lower bound
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,5,IFRow,ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem = MIN(SNGL(ub(IFRow)),xtem)
                    IF (ABS(xtem - lb(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    lb(IFRow) = DBLE(xtem)
                    prevlb(IFRow) = lb(IFRow)
                    CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,SNGL(lb(IFRow)),'(F12.5)')
                    xtem = MAX(lb(IFRow),x(IFRow))
                    X(IFRow) = DBLE(xtem)
                    CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,SNGL(x(IFRow)),'(F12.5)')
                  END IF
                CASE(3)
!.. upper bound
!>> JCC Check the bounding - only update if parameter is set to vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,5,IFRow,ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem = MAX(SNGL(lb(IFRow)),xtem)
                    IF (ABS(xtem - ub(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    ub(IFRow) = DBLE(xtem)
                    prevub(IFRow) = ub(IFRow)
                    CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,SNGL(ub(IFRow)),'(F12.5)')
                    xtem = MIN(ub(IFRow),x(IFRow))
                    X(IFRow) = DBLE(xtem)
                    CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,SNGL(x(IFRow)),'(F12.5)')
                  END IF
                CASE(4, 5)
!.. fix or vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,IFCol,IFRow,ICHK)
                  IF ( (IFCol .EQ. 4 .AND. ICHK .EQ. Checked) .OR. &
                       (IFCol .EQ. 5 .AND. ICHK .EQ. UnChecked) ) THEN
!                   JCHK = 1 - ICHK
                    CALL WGridPutCellCheckBox(IDF_parameter_grid,4,IFRow,Checked)
                    CALL WGridPutCellCheckBox(IDF_parameter_grid,5,IFRow,UnChecked)
                    CALL WGridGetCellReal(IDF_parameter_grid,1,IFRow,xtem)
                    lb(IFRow) = DBLE(xtem-1.e-5)
                    ub(IFRow) = DBLE(xtem+1.e-5)
                    CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,SNGL(lb(IFRow)),'(F12.5)')
                    CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,SNGL(ub(IFRow)),'(F12.5)')
                    CALL WGridStateCell(IDF_parameter_grid,1,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid,2,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid,3,IFRow,DialogReadOnly)
                  ELSE
!                     JCHK=1-ICHK
                    CALL WGridPutCellCheckBox(IDF_parameter_grid,4,IFRow,UnChecked)
                    CALL WGridPutCellCheckBox(IDF_parameter_grid,5,IFRow,Checked)
                    lb(IFRow)=prevlb(IFRow)
                    ub(IFRow)=prevub(IFRow)
                    CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,SNGL(lb(IFRow)),'(F12.5)')
                    CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,SNGL(ub(IFRow)),'(F12.5)')
                    CALL WGridStateCell(IDF_parameter_grid,1,IFRow,Enabled)
                    CALL WGridStateCell(IDF_parameter_grid,2,IFRow,Enabled)
                    CALL WGridStateCell(IDF_parameter_grid,3,IFRow,Enabled)
                  END IF
                  LimsChanged = .TRUE.
!                Case(5)
!.. vary
!                  Call WGridGetCellCheckBox(IDF_parameter_grid,IFCol,IFRow,ICHK)

!>> JCC Check the status here. This ensures that only messages that yield this grid position
!>> are processed if the parameter is actually checked.
!                         IF (ICHK .EQ. Checked) THEN
!                       JCHK=1-ICHK
!                               Call WGridPutCellCheckBox(IDF_parameter_grid,4,IFRow,JCHK)
!                               lb(IFRow)=prevlb(IFRow)
!                               ub(IFRow)=prevub(IFRow)  
!                               CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,sngl(lb(IFRow)),'(F12.5)')
!                               CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,sngl(ub(IFRow)),'(F12.5)')
!                               CALL WGridStateCell(IDF_parameter_grid,1,IFRow,Enabled)
!                               CALL WGridStateCell(IDF_parameter_grid,2,IFRow,Enabled)
!                               CALL WGridStateCell(IDF_parameter_grid,3,IFRow,Enabled)

!                               LimsChanged = .TRUE.
!                         END IF
                END SELECT ! IFCol
            END SELECT ! EventInfo%Value1 Field Changed Options
        END SELECT  ! ITYPE
      END DO
!.. We are now on window number 3
 777  CALL WDialogSelect(IDD_SA_input3)
      CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,Modeless)
      T0 = 0.0
      RPOS = T0
      CALL WDialogPutReal(IDF_SA_T0,RPOS,'(F7.2)')
      IPOS = 1000 - NINT(RPOS)
      CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
      RT = 0.02
      RPOS = RT
      CALL WDialogPutReal(IDF_SA_Tredrate,RPOS,'(F6.3)')
      IPOS = 501 - NINT(1000.*RPOS)
      CALL WDialogPutTrackbar(IDF_SA_Tredrate_trackbar,IPOS)
      JPOS = 20
      CALL WDialogPutInteger(IDF_SA_NS,JPOS)
      IPOS = 101 - JPOS
      NS = JPOS
      CALL WDialogPutTrackbar(IDF_SA_NS_trackbar,IPOS)
      JPOS = 25
      CALL WDialogPutInteger(IDF_SA_NT,JPOS)
      IPOS = 101 - JPOS
      NT = JPOS
      CALL WDialogPutTrackbar(IDF_SA_NT_trackbar,IPOS)
      NMoves = NT * NS * NVAR
      CALL WDialogPutInteger(IDF_SA_Moves,NMoves)
      DO WHILE (.TRUE.)                                ! Loop until user terminates
        CALL GetEvent
        SELECT CASE (EventType)
!.. Interact with the main window and look at the Pawley refinement...
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_SA3_cancel,IDCANCEL)
! Go back to the Pawley refinement or the initial wizard
                IPTYPE = 2
! Window is going to be removed: save current position
                IXPos_IDD_Wizard = WInfoDialog(6)
                IYPos_IDD_Wizard = WInfoDialog(7)
                CALL WDialogHide()
                RETURN
              CASE (IDB_SA3_back)
! Go back to the 2nd window
! Window is going to be removed: save current position
                IXPos_IDD_Wizard = WInfoDialog(6)
                IYPos_IDD_Wizard = WInfoDialog(7)
                CALL WDialogHide()
                GOTO 444
              CASE (IDF_PrintSA)
                CALL WriteSAParametersToFile
                CALL WindowOpenChild(IHANDLE)
                CALL WEditFile('SA_PARAMS.TXT',Modeless,0,FileMustExist+ViewOnly+NoToolbar+NoFileNewOpen,4)
              CASE (IDB_SA3_finish)
! We've finished the SA input
! Window is going to be removed: save current position
                IXPos_IDD_Wizard = WInfoDialog(6)
                IYPos_IDD_Wizard = WInfoDialog(7)
                CALL WDialogHide()
                GOTO 888
            END SELECT
          CASE (FieldChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE(IDF_SA_T0_trackbar)
                IF (EventInfo%VALUE2 .EQ. IDF_SA_T0_trackbar) THEN
                  CALL WDialogSelect(IDD_SA_input3)
                  CALL WDialogGetTrackBar(IDF_SA_T0_trackbar,IPOS)
                  RPOS = 1000 - IPOS
                  T0 = RPOS
                  CALL WDialogPutReal(IDF_SA_T0,RPOS,'(F7.2)')
                END IF
              CASE (IDF_SA_T0) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetReal(IDF_SA_T0,RPOS)
                T0 = RPOS
                IPOS=1000 - NINT(RPOS)
                CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
              CASE (IDF_SA_Tredrate_trackbar)
                IF (EventInfo%VALUE2 .EQ. IDF_SA_Tredrate_trackbar) THEN
                  CALL WDialogSelect(IDD_SA_input3)
                  CALL WDialogGetTrackBar(IDF_SA_Tredrate_trackbar,IPOS)
                  RPOS = 0.001 * (501.-FLOAT(IPOS))
                  RT = RPOS
                  CALL WDialogPutReal(IDF_SA_Tredrate,RPOS,'(F6.3)')
                END IF
              CASE (IDF_SA_Tredrate) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetReal(IDF_SA_Tredrate,RPOS)
                RT = RPOS
                IPOS = 501 - NINT(1000.0 * RPOS)
                CALL WDialogPutTrackbar(IDF_SA_Tredrate_trackbar,IPOS)
              CASE (IDF_SA_NS_trackbar)
                IF (EventInfo%VALUE2 .EQ. IDF_SA_NS_trackbar) THEN
                  CALL WDialogSelect(IDD_SA_input3)
                  CALL WDialogGetTrackBar(IDF_SA_NS_trackbar,IPOS)
                  JPOS = 101 - IPOS
                  CALL WDialogPutInteger(IDF_SA_NS,JPOS)
                  NS = JPOS
                  KPOS = NS * NT * NVAR
                  CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
                END IF
              CASE (IDF_SA_NS) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetInteger(IDF_SA_NS,JPOS)
                IPOS =101 - JPOS
                NS = JPOS
                CALL WDialogPutTrackbar(IDF_SA_NS_trackbar,IPOS)
                KPOS = NS * NT * NVAR
                CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
              CASE (IDF_SA_NT_trackbar)
                IF (EventInfo%VALUE2 .EQ. IDF_SA_NT_trackbar) THEN
                  CALL WDialogSelect(IDD_SA_input3)
                  CALL WDialogGetTrackBar(IDF_SA_NT_trackbar,IPOS)
                  JPOS = 101 - IPOS
                  CALL WDialogPutInteger(IDF_SA_NT,JPOS)
                  NT = JPOS
                  KPOS = NS * NT * NVAR
                  CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
                END IF
              CASE (IDF_SA_NT) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetInteger(IDF_SA_NT,JPOS)
                IPOS = 101 - JPOS
                NT = JPOS
                CALL WDialogPutTrackbar(IDF_SA_NT_trackbar,IPOS)
                KPOS = NS * NT * NVAR
                CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
              CASE (IDF_SA_RandomSeed1) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetInteger(IDF_SA_RandomSeed1,ISeed1)
              CASE (IDF_SA_RandomSeed2) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetInteger(IDF_SA_RandomSeed2,ISeed2)
              CASE (IDF_SA_RandomSeed3) 
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetInteger(IDF_SA_RandomSeed3,ISeed3)
            END SELECT
        END SELECT
      END DO ! End of Message loop
!... We've finished the three SA input pages
 888  CONTINUE
      CALL MakRHm()
      CALL CalCosArx()
      CALL BeginSA(IMyExit)
      JMyExit = IMyExit
      IMyExit = 0
      SELECT CASE (JMyExit)
        CASE(1)
          GOTO 222
        CASE(2)
          GOTO 444
        CASE(3)
          GOTO 777
      END SELECT

      END SUBROUTINE SA_MAIN
!
!*****************************************************************************
!
      SUBROUTINE WriteSAParametersToFile

      USE WINTERACTER
      USE DRUID_HEADER
      
      IMPLICIT NONE


      INCLUDE 'PARAMS.INC'

      CHARACTER*80    frag_file
      COMMON /frgcha/ frag_file(maxfrg)


      INTEGER tFileHandle, I
      REAL    R

      CALL PushActiveWindowID
      tFileHandle = 10
      OPEN(tFileHandle,FILE='SA_PARAMS.TXT',ERR=999)
      WRITE(tFileHandle,'("Parameters for simulated annealing in DASH")',ERR=999)
      CALL WDialogSelect(IDD_SA_input3)
      CALL WDialogGetInteger(IDF_SA_RandomSeed1,I)
      WRITE(tFileHandle,'("Random seed 1 = ",I5)',ERR=999)  I
      CALL WDialogGetInteger(IDF_SA_RandomSeed2,I)
      WRITE(tFileHandle,'("Random seed 2 = ",I5)',ERR=999)  I
      CALL WDialogGetReal(IDF_SA_T0,R)
      IF (R .EQ. 0.0) THEN
        WRITE(tFileHandle,'("Initial temperature to be estimated by DASH")',ERR=999)
      ELSE
        WRITE(tFileHandle,'("Initial temperature = ",F)',ERR=999)  R
      ENDIF


      CLOSE(tFileHandle)
      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Could not access temporary file.')
      CLOSE(tFileHandle)
      CALL PopActiveWindowID

      END SUBROUTINE WriteSAParametersToFile
!
!*****************************************************************************
!
      SUBROUTINE ViewZmatrix(ifrg)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: ifrg

      INCLUDE 'PARAMS.INC'

      CHARACTER*80    frag_file
      COMMON /frgcha/ frag_file(maxfrg)

      INTEGER         nfrag
      COMMON /frgcom/ nfrag

      INTEGER ntatm, natoms, ioptb, iopta, ioptt, iz1, iz2, iz3
      COMMON /zmcomi/ ntatm, natoms(maxfrg), ioptb(maxatm,maxfrg),      &
     &                iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),       &
     &                iz1(maxatm,maxfrg), iz2(maxatm,maxfrg),           &
     &                iz3(maxatm,maxfrg)

      DOUBLE PRECISION blen, alph, bet, f2cmat
      COMMON /zmcomr/ blen(maxatm,maxfrg), alph(maxatm,maxfrg),         &
     &                bet(maxatm,maxfrg), f2cmat(3,3)

      INTEGER         icomflg
      COMMON /zmcomg/ icomflg(maxfrg)

      CHARACTER*3     asym
      COMMON /zmcomc/ asym(maxatm,maxfrg)


      INTEGER I,J,M
      LOGICAL exists
      CHARACTER*17 temp_file
      DATA temp_file /'Temp_Zmatrix.mol2'/


      CHARACTER*255 dirname, filename, curdir


      REAL*8 CART(MAXATM,3)
      REAL*8 XC, YC, ZC, XNORM

      INTEGER IHANDLE, NATS, ICFRG, OutputFile
      INTEGER Res2Mol2 ! Function

      NATS = NATOMS(IFRG)
      CALL MAKEXYZ(NATS,BLEN(1,IFRG),ALPH(1,IFRG),BET(1,IFRG),        &
     &             IZ1(1,IFRG),IZ2(1,IFRG),IZ3(1,IFRG),CART(1,1),     &
     &             CART(1,2),CART(1,3))
! Determine origin for rotations
      ICFRG = ICOMFLG(IFRG)
! If user set centre of mass flag to 0, then use the molecule's centre of mass
      IF (ICFRG.EQ.0) THEN
        XC = 0.0D0
        YC = 0.0D0
        ZC = 0.0D0
        DO I = 1, NATS
          XC = XC + CART(I,1)
          YC = YC + CART(I,2)
          ZC = ZC + CART(I,3)
        ENDDO
        XNORM = 1.0D0/DFLOAT(NATS)
        XC = XC*XNORM
        YC = YC*XNORM
        ZC = ZC*XNORM
! Otherwise, use atom number ICFRG
      ELSE
        XC = CART(ICFRG,1)
        YC = CART(ICFRG,2)
        ZC = CART(ICFRG,3)
      ENDIF
! Subtract the origin from all atom positions
      DO I = 1, NATS
        CART(I,1) = CART(I,1) - XC
        CART(I,2) = CART(I,2) - YC
        CART(I,3) = CART(I,3) - ZC
      ENDDO

! Now write  'Temp_Zmatrix.res'
!TITLBA09_2                                  
!CELL   0.0000   4.0207   4.5938  21.0298  90.7123  69.6944  80.3612
!LATT  1
!O1    0    0.19295   -0.33996    0.06295    1.00000    0.02476
!O2    0   -0.17961   -0.00616    0.13265    1.00000    0.02000
!C3    0   -0.10381   -0.30317    0.43567    1.00000    0.08073
!H4    0   -0.29456   -0.41679    0.45015    1.00000    0.10000
!H5    0    0.14042   -0.43747    0.42625    1.00000    0.10000
!AG19  0   -0.28653    0.22594    0.03911    1.00000    0.04943
!H20   0   -0.13171   -0.14728    0.47266    1.00000    0.10000
!END 
      
      OutputFile = 3
      OPEN(UNIT=OutputFile,file='Temp_Zmatrix.res',form='formatted',ERR=999)
      WRITE(OutputFile,'(A)') 'TITL Temporary file written by DASH'
      WRITE(OutputFile,'(A)') 'CELL 1.54 1.0 1.0 1.0 90.0 90.0 90.0'
      DO I = 1, NATS
        WRITE(OutputFile,333,ERR=999) asym(I,IFRG), CART(I,1), CART(I,2), CART(I,3)
  333   FORMAT(A3,1X,'0',1X,F10.5,1X,F10.5,1X,F10.5,1X,'1.00000    1.00000')
      ENDDO
      WRITE(OutputFile,'(A)') 'END'
      CLOSE(OutputFile)
! Convert .res to mol2
      I = Res2Mol2('Temp_Zmatrix.res')
      IF (I .EQ. 0) GOTO 999
! Show the mol2 file 
      I = LEN_TRIM(ViewExe)
      J = LEN_TRIM(ViewArg)
      INQUIRE(FILE = ViewExe(1:I), EXIST=exists)
      IF (exists) THEN
        CALL SplitPath(ViewExe,dirname, filename)
        CALL IOsDirName(curdir)
        CALL IOsDirChange(dirname)
        M = InfoError(1)
        IF (J .GT. 0) THEN
          CALL IOSCommand( &
             filename(1:LEN_TRIM(filename))//' '//ViewArg(1:J)//' '// &
             '"'//curdir(1:LEN_TRIM(curdir))//DIRSPACER//temp_file(1:LEN_TRIM(temp_file))//'"',0)
        ELSE
          CALL IOSCommand( &
               filename(1:LEN_TRIM(filename))//' '// &
               '"'//curdir(1:LEN_TRIM(curdir))//DIRSPACER//temp_file(1:LEN_TRIM(temp_file))//'"',0)
        END IF
        CALL IOsDirChange(curdir)
        M = InfoError(1)
        IF (M .EQ. ErrOSCommand) THEN
          CALL WMessageBox(OkOnly, InformationIcon, CommonOk, &
                  "DASH could not launch your viewer. The viewer executable is currently configured"//&
  CHAR(13)//"to launch the program "//ViewExe(1:I)//&
  CHAR(13)//"To change the configuration you should edit the file "//&
  CHAR(13)//INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//"Dash.cfg"//&
  CHAR(13)//"and then restart DASH","Viewer incorrectly installed")
        ELSE IF (M .EQ. 0) THEN
          ViewAct = .TRUE.
        END IF
      ELSE
        CALL WMessageBox(OkOnly, InformationIcon, CommonOk, &
                  "DASH could not find your viewer. The viewer executable is currently configured"//&
  CHAR(13)//"to launch the program "//ViewExe(1:I)//&
  CHAR(13)//"To change the configuration you should edit the file "//&
  CHAR(13)//INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//"Dash.cfg"//&
  CHAR(13)//"and then restart DASH","No such viewer")
      END IF

! Show the z-matrix file in an editor window
  999 CALL WindowOpenChild(IHANDLE)
      CALL WEditFile(frag_file(ifrg),Modeless,0,FileMustExist+ViewOnly+NoToolbar+NoFileNewOpen,4)

      END SUBROUTINE ViewZmatrix
!
!*****************************************************************************
!
      SUBROUTINE SA_Parameter_Set

      USE WINTERACTER
      USE DRUID_HEADER

!C>> JCC Add in checking: only use z-matrices that the user has selected!
      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'IZMCheck.inc'

      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,CSH,FSTAR,XP,FOPT
      COMMON /sacmn/ XOPT(NMAX),CSH(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT

      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen,alph,bet,f2cmat
!>> JCC Handle via the PDB standard
      DOUBLE PRECISION f2cpdb
      COMMON /pdbcat/ f2cpdb(3,3)

      INTEGER ioptb,iopta,ioptt,iz1,iz2,iz3
      COMMON /zmcomi/ ntatm,natoms(maxfrg),&
        ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
        iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
        bet(maxatm,maxfrg),f2cmat(3,3)
      CHARACTER*3 asym
      COMMON /zmcomc/ asym(maxatm,maxfrg)

      COMMON /frgcom/ nfrag
      PARAMETER (mvar=100)
      DOUBLE PRECISION x,lb,ub,vm,xpreset
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      DOUBLE PRECISION prevub, prevlb ! For saving the previous range
      COMMON /pvalues/ prevub(mvar), prevlb(mvar)

      COMMON /presetr/ xpreset(mvar)
      LOGICAL log_preset
      COMMON /presetl/ log_preset

      DOUBLE PRECISION T0,rt
      COMMON /saparl/ T0,rt
      INTEGER  NS, NT, ISEED1, ISEED2
      INTEGER  MAXEVL, IPRINT
      COMMON /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2

      CHARACTER*36 parlabel(mvar)

      CHARACTER*36 czmpar
      COMMON /zmnpar/ izmpar(maxfrg),&
            czmpar(MaxDOF,maxfrg),kzmpar(MaxDOF,maxfrg),xzmpar(MaxDOF,maxfrg)
      LOGICAL gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)

      DOUBLE PRECISION dcel(6)

      DO I = 1, 6
        dcel(I) = DBLE(CellPar(I))
      END DO
      CALL frac2cart(f2cmat,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
      CALL frac2pdb(f2cpdb,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
      CALL CREATE_FOB()
      kk = 0
!C>> JCC Run through all possible fragments
      DO ifrg = 1, CheckSize
!C>> Only include those that are now checked
        IF (IZMCheck(ifrg) .EQ. Checked) THEN
          DO ii = 1, izmpar(ifrg)
            kk = kk + 1
            x(kk)=xzmpar(ii,ifrg)
            parlabel(kk)=czmpar(ii,ifrg)
            SELECT CASE(kzmpar(ii,ifrg))
              CASE (1) !.. position
                lb(kk) = 0.0
                ub(kk) = 1.0
                vm(kk) = 0.1
              CASE (2) !.. quaternion
                lb(kk) = -1.0
                ub(kk) =  1.0
                vm(kk) =  0.1
              CASE (3) !.. torsion
                IF      (x(kk) .LT. 0.0 .AND. x(kk) .GT. -180.0) THEN
                  lb(kk) =  -180.0
                  ub(kk) =   180.0
                ELSE IF (x(kk) .GT. 0.0 .AND. x(kk) .LT.  360.0) THEN
                  lb(kk) =   0.0
                  ub(kk) = 360.0
                ELSE 
                  lb(kk) = x(kk) - 180.0
                  ub(kk) = x(kk) + 180.0
                END IF              
                vm(kk) = 10.0
              CASE (4) !.. angle
                lb(kk) = x(kk) - 10.0
                ub(kk) = x(kk) + 10.0
                vm(kk) = 1.0
              CASE (5) !.. bond
                lb(kk) = 0.9*x(kk)
                ub(kk) = x(kk)/0.9
                vm(kk) = 0.1*(ub(kk)-lb(kk))
            END SELECT
          END DO
!C>> JCC End of check on selection
        END IF
      END DO
      nvar = kk
!.. Now fill the grid
      CALL WDialogSelect(IDD_SA_input2)
      CALL WGridRows(IDF_parameter_grid,nvar)
      DO i = 1, nvar
         CALL WGridLabelRow(IDF_parameter_grid,i,parlabel(i))
         CALL WGridPutCellReal(IDF_parameter_grid,1,i,SNGL(x(i)),'(F12.5)')
         CALL WGridPutCellReal(IDF_parameter_grid,2,i,SNGL(lb(i)),'(F12.5)')
         CALL WGridPutCellReal(IDF_parameter_grid,3,i,SNGL(ub(i)),'(F12.5)')
         CALL WGridPutCellCheckBox(IDF_parameter_grid,4,i,Unchecked)
         CALL WGridPutCellCheckBox(IDF_parameter_grid,5,i,Checked)
         CALL WGridStateCell(IDF_parameter_grid,1,i,Enabled)
         CALL WGridStateCell(IDF_parameter_grid,2,i,Enabled)
         CALL WGridStateCell(IDF_parameter_grid,3,i,Enabled)
         prevub(i) = ub(i)
         prevlb(i) = lb(i)
      END DO

      END SUBROUTINE SA_Parameter_Set
!
!*****************************************************************************
!
!C>> JCC This subroutine handles the various types of status error that can arise 
!C>> during a reading of a file and produces a suitable message to say what went wrong.
      SUBROUTINE FileErrorPopup(FileName, ErrorStatus)

      USE WINTERACTER

      INCLUDE 'iosdef.for'

      INTEGER       ErrorStatus
      CHARACTER*(*) FileName
      INTEGER       lenstr

      lenstr = LEN_TRIM(FileName)
      SELECT CASE(ErrorStatus)
        CASE (FOR$IOS_FILNOTFOU) 
          CALL ErrorMessage("The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//" does not exist.")
        CASE (FOR$IOS_OPEFAI)
          CALL ErrorMessage("The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//" could not be opened.")
        CASE (FOR$IOS_PERACCFIL)
          CALL ErrorMessage("You do not have permission to access the file "//FileName(1:lenstr))
        CASE DEFAULT
          CALL ErrorMessage("The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//"was not read successfully.")
      END SELECT

      END SUBROUTINE FileErrorPopup
!
!*****************************************************************************
!
      SUBROUTINE ClearZmatrices(IDFZMFile,IDFZMPars,IDFZMCheck,IDBZMBrowse)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'
      INCLUDE 'IZMCheck.inc'

      INTEGER IDFZMFile(CheckSize), IDFZMPars(CheckSize)
      INTEGER IDFZMCheck(CheckSize), IDBZMBrowse(CheckSize)
      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen,alph,bet,f2cmat
      INTEGER ioptb,iopta,ioptt,iz1,iz2,iz3
      COMMON /zmcomi/ ntatm,natoms(maxfrg),&
        ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
        iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
        bet(maxatm,maxfrg),f2cmat(3,3)
      CHARACTER*3 asym
      COMMON /zmcomc/ asym(maxatm,maxfrg)
      INTEGER nfrag
      COMMON /frgcom/ nfrag
      CHARACTER*36 czmpar
      COMMON /zmnpar/ izmpar(maxfrg),&
        czmpar(MaxDOF,maxfrg),kzmpar(MaxDOF,maxfrg),xzmpar(MaxDOF,maxfrg)
      LOGICAL gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)

! Blow away the selected z-matrices
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
      CALL WDialogSelect(IDD_SAW_Page1)
      DO II = 1, 5
        izmpar(ii) = 0
        natoms(ii) = 0
        IF (gotzmfile(ii)) THEN
          CALL WDialogClearField(IDFZMFile(ii))
          CALL WDialogClearField(IDFZMPars(ii))
          CALL WDialogPutCheckBox(IDFZMCheck(ii),Unchecked)
          CALL WDialogClearField(IDFZMFile(II))
          CALL WDialogClearField(IDFZMPars(II))
        ENDIF
        CALL WDialogFieldState(IDFZMFile(II),Disabled)
        CALL WDialogFieldState(IDFZMCheck(II),Disabled)
        CALL WDialogFieldState(IDBZMBrowse(II),Disabled)
        CALL WDialogFieldState(IDFZMPars(II),Disabled)
      END DO
      DO ii = 1, maxfrg
        gotzmfile(ii) = .FALSE.
      END DO
      CALL WDialogFieldState(IDFZMFile(1),Enabled)
      CALL WDialogPutCheckBox(IDFZMCheck(1),checked)
      CALL WDialogFieldState(IDFZMCheck(1),ReadOnly)
      CALL WDialogFieldState(IDBZMBrowse(1),Enabled)
      CALL WDialogFieldState(IDFZMPars(1),ReadOnly)
      CALL WDialogClearField(IDF_ZM_allpars)
      CALL UpdateZmatrixSelection(IDFZMpars)
      CALL WDialogSelect(IDD_SA_input2)
! Clear the grid too
      CALL WDialogClearField(IDF_parameter_grid)
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
      CALL WDialogSelect(IDD_SAW_Page1)

      END SUBROUTINE ClearZmatrices
!
!*****************************************************************************
!
      SUBROUTINE UpdateZmatrixSelection(IDFZMpars)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'
      INCLUDE 'IZMCheck.inc'

      INTEGER IDFZMpars(CheckSize)
      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen, alph, bet, f2cmat
      INTEGER     ioptb, iopta, ioptt, iz1, iz2, iz3
      COMMON /zmcomi/ ntatm,natoms(maxfrg),&
        ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
        iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
        bet(maxatm,maxfrg),f2cmat(3,3)
      CHARACTER*3 asym
      COMMON /zmcomc/ asym(maxatm,maxfrg)
      INTEGER nfrag
      COMMON /frgcom/ nfrag
      CHARACTER*36 czmpar
      COMMON /zmnpar/ izmpar(maxfrg),&
        czmpar(MaxDOF,maxfrg),kzmpar(MaxDOF,maxfrg),xzmpar(MaxDOF,maxfrg)
      LOGICAL gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)
      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150),&
        TF(150),KTF(150),SITE(150),KSITE(150),&
        ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17

      INTEGER izmtot

      nfrag  = 0
      izmtot = 0
      ntatm  = 0
!C>> JCC Changes here - account for unchecked zmatrices which have been read in but are unselected
      DO ii = 1, 5
        IF (gotzmfile(ii) .AND. IZMCheck(ii) .EQ. Checked) THEN
          nfrag = nfrag + 1
          ntatm = ntatm + natoms(ii)
          izmtot = izmtot + izmpar(ii)
          CALL WDialogPutInteger(IDFZMpars(ii),izmpar(ii))
        END IF
      END DO
      natom = ntatm             
      CALL WDialogPutInteger(IDF_ZM_allpars,izmtot)
      RETURN

      END SUBROUTINE UpdateZmatrixSelection
!
!*****************************************************************************
!
      SUBROUTINE ImportZmatrix
 
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER            I, IFlags, ISEL, Ilen, Istart, Istat, Nzm
      INTEGER            POS
      CHARACTER(LEN=4)   :: EXT4
      CHARACTER(LEN=255) :: FilterStr, F
      CHARACTER(LEN=512) :: Zmfiles
      CHARACTER(LEN=5)   :: fmt     
      CHARACTER(LEN=512) :: Info = 'You can import molecules from mol2, mol or pdb files into DASH.'//CHAR(13)//&
                                   'When you click on OK, you will be prompted for a file in one'//CHAR(13)//&
                                   'of these formats. DASH will create separate z-matrix files for'//CHAR(13)//&
                                   'each chemical residue present in the first entry in the file.'//CHAR(13)//&
                                   'In multiple entry files the first entry will be read only.'
      INTEGER Res2Mol2, CSSR2Mol2 ! Function

      CALL WMessageBox(OKCancel, InformationIcon, CommonOK, Info, "Create Z-matrix")
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
      IFlags = LoadDialog + DirChange + AppendExt
      FilterStr = "All files (*.*)|*.*|"//&
                  "Molecular model files|*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"//&
                  "Protein DataBank files (*.pdb)|*.pdb|"//&
                  "Mol2 files (*.mol2, *.ml2)|*.mol2;*.ml2|"//&
                  "mdl mol files|*.mol;*.mdl|"//&
                  "SHELX files (*.res)|*.res|"//&
                  "cssr files (*.cssr)|*.cssr|"
      ISEL = 2
      FNAME = ' '
      CALL WSelectFile(FilterStr, IFLAGS, FNAME,"Select a file for conversion",ISEL)
      Ilen = LEN_TRIM(FNAME)
      IF (Ilen .EQ. 0) RETURN
! Find the last occurence of '.' in TheFileName
      POS = Ilen-1 ! Last character of TheFileName is not tested
! The longest extension allowed is four
      DO WHILE ((POS .NE. 0) .AND. (FNAME(POS:POS) .NE. '.') .AND. (POS .NE. (Ilen-5)))
        POS = POS - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      IF (FNAME(POS:POS) .NE. '.') RETURN
      EXT4 = '    '
      EXT4 = FNAME(POS+1:Ilen)
      CALL ILowerCase(EXT4)
      SELECT CASE (EXT4)
        CASE ('cssr')
          ISTAT = CSSR2Mol2(FNAME)
          IF (ISTAT .NE. 1) RETURN
! Replace 'cssr' by 'mol2'
          FNAME = FNAME(1:Ilen-4)//'mol2'
          Ilen = LEN_TRIM(FNAME)
          fmt = '-mol2'
        CASE ('res ')
          ISTAT = Res2Mol2(FNAME)
          IF (ISTAT .NE. 1) RETURN
! Replace 'res' by 'mol2'
          FNAME = FNAME(1:Ilen-3)//'mol2'
          Ilen = LEN_TRIM(FNAME)
          fmt = '-mol2'
        CASE ('pdb ')
          fmt = '-pdb'
        CASE ('mol2','ml2 ')
          fmt = '-mol2'
        CASE ('mol ','mdl ')
          fmt = '-mol'
      END SELECT
! Run silently, 
      CALL IOSDeleteFile('MakeZmatrix.log')
      Istat = InfoError(1) ! Clear any errors 
      Istart = 1
      DO I = 1, Ilen
        IF (FNAME(I:I) .EQ. DIRSPACER) Istart = I + 1
      END DO
      CALL WCursorShape(CurHourGlass)
      CALL IOSCommand(CONVEXE(1:LEN_TRIM(CONVEXE))//' '//fmt(1:LEN_TRIM(fmt))//' "'//FNAME(Istart:Ilen)//'"',3)
      CALL WCursorShape(CurCrossHair)
! Check return status
      OPEN(UNIT=145, FILE='MakeZmatrix.log',STATUS='OLD',IOSTAT = ISTAT)
      IF ((InfoError(1) .EQ. ErrOSCommand) .OR. (ISTAT .NE. 0)) THEN
! An error occurred: get the return status
! IECODE = InfoError(3)
        CALL ErrorMessage("Sorry, could not create z-matrices")
! Prompt with files created
      ELSE ! All Ok: Need to read in the file names
        Ilen = 1
        DO WHILE (Ilen .LT. 512)
          Nzm = Nzm + 1
          READ (145,'(A)',ERR=20,END=20) F
          ZmFiles(Ilen:512) = CHAR(13)//F(1:LEN_TRIM(F))
          Ilen = LEN_TRIM(ZmFiles) + 1
        END DO
 20     CONTINUE
        CALL WMessageBox(OKOnly, InformationICon, CommonOk, &
                         "Generated the following zmatrices successfully:"//CHAR(13)//&
                         ZmFiles(1:Ilen)//CHAR(13)//CHAR(13)//&
                         "You can load them by clicking on the zmatrix browse buttons"//CHAR(13)//&
                         "in the SA setup window",&
                         "Generation Successful")
        CLOSE(145)
      END IF
      RETURN

      END SUBROUTINE ImportZmatrix
!
!*****************************************************************************
!
