!
!*******************************************************************************
!
      SUBROUTINE SaSummary
! ep July 2001 
! Called from Begin_Sa subroutine.  Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC' 
       
!ep    need the common block to identify the number rows in the grid          
      LOGICAL RESTART
      INTEGER SA_Run_Number
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult
!     required to handle the profile graphs plotted in child windows
      INTEGER  SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)
        
      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
      CALL WDialogShow(-1,-1,0,Modeless)
      DO WHILE (.TRUE.)
        CALL GetEvent
        SELECT CASE (EventType)
! will close the profile plot window
          CASE (CloseRequest)
            CALL WindowCloseChild(EventInfo%win)
            SAUsedChildWindows(EventInfo%win) = 0
! exposing or resizing of profile plot windows - will replot
          CASE (expose, resize)
            CALL plot_pro_file(EventInfo%win)
! Ok button of summary window pushed, dialog closed
          CASE (PushButton)
            IF (EventInfo%VALUE1 .EQ.IDCANCEL) THEN
              IF (EventInfo%win .EQ. IDD_SA_Multi_Completed_ep) THEN
                CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
                CALL WDialogHide()
                EXIT
              ENDIF
            ENDIF
            IF (EventInfo%VALUE1 .EQ. IDOK_ep) THEN
! Closes all SA profile child windows which are still open when OK button clicked
              DO i = 1, MaxNumChildWin
                IF (SAUsedChildWindows(i).eq.1) THEN
                  CALL WindowCloseChild(i)
                  SAUsedChildWindows(i) = 0
                END IF
              END DO
              CALL WDialogHide()
              EXIT
            END IF
        END SELECT
!ep allows you to view pdb file of SA Solutions, each clicked
!   check box in fresh mercury window
        DO irow = 1,maxruns
          CALL WGridGetCellCheckBox(IDF_SA_summary,2,irow,istatus)
          IF (istatus.eq.1) THEN
! calls subroutine which opens Mercury window with .pdb file
            CALL viewpdb(irow)
! calls subroutine which plots observed diffraction pattern with calculated pattern
            CALL organise_sa_result_data(irow)
            CALL WGridPutCellCheckBox(IDF_SA_Summary,2,irow,Unchecked)
            istatus = 0
          ENDIF
        END DO               
      END DO

      END SUBROUTINE SaSummary
!
!*******************************************************************************
!
      SUBROUTINE Viewpdb(irow)
! ep July 2001
! This subroutine is the Viewbest subroutine (in sa_refresh.for) hacked about a bit.
! Each time a "View" button is clicked in the summary window, this subroutine will
! open a new Mercury window and display the relevant .pdb file
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INTEGER IROW
      INTEGER I,M
      LOGICAL exists
      CHARACTER*255 Grid_Buffer
      CHARACTER*255 dirname, filename, curdir
      
      I = len_trim(ViewExe)
!      J = len_trim(ViewArg)
!     Get grid_buffer which contains the name of the correct pdb file
      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      CALL WGridGetCellString(IDF_SA_Summary,1,irow,Grid_Buffer)      
      INQUIRE(FILE = ViewExe(1:I), EXIST=exists)
      IF (exists) THEN
! Splitpath returns with the directory in which Mercury resides (dirname) and 
! filename = mercury.exe
        CALL SplitPath(ViewExe,dirname, filename)     
!
! gets working directory name i.e. where .pdb files have been saved
        CALL IOsDirName(curdir)
        CALL IOsDirChange(dirname)
        M = InfoError(1)
!ep     By taking out the if, then statement, mercury will open with a fresh window
!           for each SA solution check box clicked.
!ep       IF (J.GT.0) THEN
!                 CALL IOSCommand( &
!                 filename(1:len_trim(filename))//' '//ViewArg(1:J)//' '// &
!                 '"'//curdir(1:len_trim(curdir))//DIRSPACER//temp_file(1:len_trim(temp_file))//'"',0)
!ep     if want to open each SA solution in same mercury window then uncomment
!           the if, then statement and use the following 3 lines
!ep               CALL IOSCommand( &
!                 filename(1:len_trim(filename))//' '//ViewArg(1:J)//' '// &
!                 '"'//grid_buffer(1:len_trim(grid_buffer))//'"',0)
!ep       ELSE
!                 CALL IOSCommand( &
!                 filename(1:len_trim(filename))//' '// &
!                 '"'//curdir(1:len_trim(curdir))//DIRSPACER//temp_file(1:len_trim(temp_file))//'"',0)

! opens Mercury with the file specified through grid_buffer, filename = mercury.exe
               CALL IOSCommand(filename(1:LEN_TRIM(filename))//' '// &
                '"'//grid_buffer(1:LEN_TRIM(grid_buffer))//'"',0)
!         END IF
        CALL IOsDirChange(curdir)

        M = InfoError(1)
        IF (M .EQ. ErrOSCommand) THEN
          CALL WMessageBox(OkOnly, InformationIcon, CommonOk, &
                    "DASH could not launch your viewer. The viewer executable is currently configured"//&
                CHAR(13)//"to launch the program "//ViewExe(1:I)//&
                CHAR(13)//"To change the configuration you should edit the file "//&
                CHAR(13)//INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//"Dash.cfg"//&
                CHAR(13)//"and then restart DASH","Viewer incorrectly installed")
        ELSE IF (M.EQ.0) THEN
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

      END  SUBROUTINE Viewpdb
!
!*******************************************************************************
!
