      SUBROUTINE Plot_Alter
!
!  Enable button up and mouse movement events
!
      USE WINTERACTER
      USE VARIABLES

      TYPE(WIN_STYLE)    WINDOW
!
      CHARACTER (LEN=20) XCURST,YCURST
      INCLUDE 'PARAMS.INC'
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!
      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)

      INCLUDE 'GLBVAR.INC'

      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
      CALL WCursorShape(CurCrossHair)
! JCC Set the scale correctly. 
      CALL IPgUnits(xpgmin,ypgmin,xpgmax,ypgmax)
      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1),ygcur(1),xcur(1),ycur(1))
      XPGMINOLD = XPGMIN
      XPGMAXOLD = XPGMAX
      YPGMINOLD = YPGMIN
      YPGMAXOLD = YPGMAX
      IPMINOLD = IPMIN
      IPMAXOLD = IPMAX
      IMOV = 0
      DO
        CALL GetEvent
        IF (EventInfo%WIN .EQ. 0) THEN
          CALL IPgUnitsFromGrUnits(EventInfo%GX,EventInfo%GY,xcur(2),ycur(2))
          xmint = MIN(xcur(1),xcur(2))
          xmaxt = MAX(xcur(1),xcur(2))
          ymint = MIN(ycur(1),ycur(2))
          ymaxt = MAX(ycur(1),ycur(2))
          IF (.NOT. (xmint.EQ.xmaxt .OR. ymint.EQ.ymaxt)) THEN
            IF (xmaxt-xmint .LE. 200.0) THEN
              CALL IRealToString(xmint,statbarstr(4)(1:),'(f10.3)')
              CALL IRealToString(xmaxt,statbarstr(5)(1:),'(f10.3)')
            ELSE
              CALL IRealToString(xmint,statbarstr(4)(1:),'(f10.1)')
              CALL IRealToString(xmaxt,statbarstr(5)(1:),'(f10.1)')
            END IF
            IF (ymaxt-ymint .LE. 100.0) THEN
              CALL IRealToString(ymint,statbarstr(6)(1:),'(f10.3)')
              CALL IRealToString(ymaxt,statbarstr(7)(1:),'(f10.3)')
            ELSE
              CALL IRealToString(ymint,statbarstr(6)(1:),'(f10.1)')
              CALL IRealToString(ymaxt,statbarstr(7)(1:),'(f10.1)')
            END IF
            DO ISB = 4, 7
              CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
            END DO
          END IF
          SELECT CASE (EventType)
!U            CASE (Expose,Resize)
!U              CALL Redraw()
!U! JvdS @ The following line is now never dealt with
!U              IMOV = 0        
            CASE (MouseMove)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              IMOV = IMOV + 1
              CALL IGrPlotMode('EOR')
              CALL IGrColourN(KolNumRectSelect)
              IF (IMOV .EQ. 1) THEN
!                CALL IGrColourN(224)
                CALL IGrFillPattern(0,1,1)
                CALL IGrRectangle(xgcur(1),ygcur(1),xgcur(2),ygcur(2))
                xgcurold = xgcur(2)
                ygcurold = ygcur(2)
              ELSE
                CALL IGrFillPattern(0,1,1)
                CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
                CALL IGrRectangle(xgcur(1),ygcur(1),xgcur(2),ygcur(2))
                xgcurold = xgcur(2)
                ygcurold = ygcur(2)
              END IF 
              CALL IGrPlotMode(' ')
              CALL IGrColourN(InfoGrScreen(PrevColReq))
            CASE (MouseButUp)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              CALL IGrPlotMode(' ')
              CALL IGrColourN(KolNumRectSelect)
              CALL WMessageEnable(MouseMove, Disabled)
              CALL WMessageEnable(MouseButUp, Disabled)
              IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                CALL IGrFillPattern(0,1,1)
                CALL IGrRectangle(xgcur(1),ygcur(1),xgcur(2),ygcur(2))
                IF (ABS(XCUR(2)-XCUR(1)).LT.0.003*(XPGMAX-XPGMIN)) EXIT
                IF (ABS(YCUR(2)-YCUR(1)).LT.0.003*(YPGMAX-YPGMIN)) EXIT
                XPGMIN=MIN(XCUR(1),XCUR(2))
                XPGMAX=MAX(XCUR(1),XCUR(2))  
                YPGMIN=MIN(YCUR(1),YCUR(2))
                YPGMAX=MAX(YCUR(1),YCUR(2))
!              ELSE IF (zmessage%value1.eq.RightButton) then
!                xpgmin=xpmin
!                xpgmax=xpmax
!                ypgmin=ypmin
!                ypgmax=ypmax
              END IF
              CALL IGrColourN(InfoGrScreen(PrevColReq))
              CALL WCursorShape(CurArrow)
              CALL Get_IPMaxMin()
              CALL Profile_Plot(IPTYPE)
              EXIT  
          END SELECT
        END IF
      END DO

      END SUBROUTINE Plot_Alter
!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown

      USE WINTERACTER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INCLUDE 'GLBVAR.INC'

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)

! acts on various KeyDown options
      KeyNumber = EventInfo%VALUE1
      if (KeyNumber .NE. KeyBackSpace) then
        XPGMINOLD=XPGMIN
        XPGMAXOLD=XPGMAX
        YPGMINOLD=YPGMIN
        YPGMAXOLD=YPGMAX
        IPMINOLD=IPMIN
        IPMAXOLD=IPMAX
      end if
!
      SELECT CASE (KeyNumber)
         CASE(KeyPageLeft)
           xpgdif=xpgmax-xpgmin
           xpgmin=max(xpmin,xpgmin-0.02*xpgdif)
           xpgmax=xpgmin+xpgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyPageRight)
! We're going to move the graph to the right if we can
           xpgdif=xpgmax-xpgmin
           xpgmax=min(xpmax,xpgmax+0.02*xpgdif)
           xpgmin=xpgmax-xpgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyCursorLeft)
! We're going to move the graph to the left if we can
           xpgdif=xpgmax-xpgmin
           xpgmin=max(xpmin,xpgmin-0.25*xpgdif)
           xpgmax=xpgmin+xpgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyCursorRight)
! We're going to move the graph to the right if we can
           xpgdif=xpgmax-xpgmin
           xpgmax=min(xpmax,xpgmax+0.25*xpgdif)
           xpgmin=xpgmax-xpgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyLeftExtreme)
! We're going to move the graph as far left as we can
           xpgdif=xpgmax-xpgmin
           xpgmin=xpmin
           xpgmax=xpgmin+xpgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyRightExtreme)
! We're going to move the graph as far right as we can
           xpgdif=xpgmax-xpgmin
           xpgmax=xpmax
           xpgmin=xpgmax-xpgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)            
         CASE(KeyPageDown)
! We're going to expand the xscale by sqrt(2) if we can
           xpgdif=xpgmax-xpgmin
           xpgav=0.5*(xpgmax+xpgmin)
           xtem=min(0.5*(xpmax-xpmin),0.7071*xpgdif)
           xpgmin=xpgav-xtem
           xpgmax=xpgav+xtem
           if (xpgmin.lt.xpmin) then
             xpgmin=xpmin
             xpgmax=xpgmin+2.*xtem
           else if (xpgmax.gt.xpmax) then
             xpgmax=xpmax
             xpgmin=xpgmax-2.*xtem
           end if
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyPageUp)
! We're going to contract the xscale by sqrt(2)
           xpgdif=xpgmax-xpgmin
           xpgav=0.5*(xpgmax+xpgmin)
           xtem=0.3536*xpgdif
           xpgmin=xpgav-xtem
           xpgmax=xpgav+xtem
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyCursorDown)
! We're going to move the graph down if we can
           ypgdif=ypgmax-ypgmin
           ypgmin=max(ypmin,ypgmin-0.25*ypgdif)
           ypgmax=ypgmin+ypgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE(KeyCursorUp)
! We're going to move the graph up if we can
           ypgdif=ypgmax-ypgmin
           ypgmax=min(ypmax,ypgmax+0.25*ypgdif)
           ypgmin=ypgmax-ypgdif
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE (KeyUpExtreme)
! We're going to scale to min/max y over the current range
           ii = ypgmin
           ypgmin=ypgmax
           ypgmax=ii
           do ii=1,nobs
             if(xobs(ii).ge.xpgmin.and.xobs(ii).le.xpgmax) then
               ypgmin=min(yobs(ii),ypgmin)
               ypgmax=max(yobs(ii),ypgmax)
             end if
           end do
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE (KeyBackspace)
! Undo last zoom action
           xpgmint=xpgmin
           xpgmaxt=xpgmax
           ypgmint=ypgmin
           ypgmaxt=ypgmax
           xpgmin=xpgminold
           xpgmax=xpgmaxold
           ypgmin=ypgminold
           ypgmax=ypgmaxold
           xpgminold=xpgmint
           xpgmaxold=xpgmaxt
           ypgminold=ypgmint
           ypgmaxold=ypgmaxt
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)
         CASE (KeyHome)
! Back to full profile range
           xpgmin=xpmin
           xpgmax=xpmax
           ypgmin=ypmin
           ypgmax=ypmax
           CALL Get_IPMaxMin() 
           CALL Profile_Plot(IPTYPE)        
      END SELECT

      END SUBROUTINE Check_KeyDown
!
!*****************************************************************************
!
      SUBROUTINE Create_DicvolIndexFile
     
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'params.inc'

      COMMON /ALLPEAKS/ NTPeak,AllPkPosVal(MTPeak),AllPkPosEsd(MTPeak),&
      PkArgK(MTPeak),PkTicDif(MTPeak),PkProb(MTPeak), &
      IOrdTem(MTPeak),IHPk(3,MTPeak),IArgK(MTPeak)

      INTEGER IFTYPE
      INTEGER            :: IFLAGS,KLEN
      CHARACTER(LEN=75) :: FILTER
      REAL Rvpar(2), Rcpar(3), Lambda, Rdens, Rmolwt, Rfom, Rexpzp, Reps
      INTEGER Isystem(6), UseErr, I, Iord

! Get a file name

! Extract the information from the dialog
      CALL PushActiveWindowID
      IFLAGS = SaveDialog  + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|DICVOL files (*.dat)|*.dat|'
      FNAME=' '
      IFTYPE = 2
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Enter DICVOL file name',IFTYPE)
      KLEN = LEN_TRIM(FNAME)
      IF (KLEN .EQ. 0) RETURN
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogGetReal(IDF_Indexing_Lambda, Lambda)
      CALL WDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
      CALL WDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
      CALL WDialogGetReal(IDF_Indexing_MaxLen, Rcpar(1))
      CALL WDialogGetReal(IDF_Indexing_MinAng, Rcpar(2))
      CALL WDialogGetReal(IDF_Indexing_MaxAng, Rcpar(3))
      CALL WDialogGetReal(IDF_Indexing_Density, Rdens)
      CALL WDialogGetReal(IDF_Indexing_MolWt,   Rmolwt)
      CALL WDialogGetReal(IDF_Indexing_Fom,     Rfom)
      CALL WDialogGetReal(IDF_Indexing_zero,    Rexpzp)
      CALL WDialogGetCheckBox(IDF_Indexing_Cubic,      Isystem(1))
      CALL WDialogGetCheckBox(IDF_Indexing_Tetra,      Isystem(2))
      CALL WDialogGetCheckBox(IDF_Indexing_Hexa,       Isystem(3))
      CALL WDialogGetCheckBox(IDF_Indexing_Ortho,      Isystem(4))
      CALL WDialogGetCheckBox(IDF_Indexing_Monoclinic, Isystem(5))
      CALL WDialogGetCheckBox(IDF_Indexing_Triclinic,  Isystem(6))
      CALL WDialogGetCheckBox(IDF_Indexing_UseErrors,  UseErr)
! Write it out 
      OPEN(UNIT=117,FILE=FNAME(1:KLEN),STATUS='UNKNOWN',ERR=99)
      WRITE(117,*,ERR=100) 'DICVOL input file created by DASH'
      WRITE(117,'(8(I3,1X))',ERR=100)  NTPeak, 2, (Isystem(i),i=1,6)
      WRITE(117,'(7(F8.2,1X))',ERR=100)  Rcpar(1),Rcpar(1),Rcpar(1),Rvpar(1),Rvpar(2),Rcpar(2),Rcpar(3)
      WRITE(117,'(F10.6,1X,3(F8.4,1X))',ERR=100)  Lambda, Rmolwt, Rdens, Rdens/50.0
      IF (UseErr .EQ. 1) THEN
        Reps = 1.0
      ELSE
        Reps = 0.0
      END IF
      WRITE(117,'(F3.1,1X,F6.2,1X,F9.6)',ERR=100) Reps, Rfom, Rexpzp
      IF (UseErr .EQ. 1) THEN
        DO I = 1, NTPeak
          IOrd = IOrdTem(i)
          WRITE(117,'(F12.4,1X,F12.4)',ERR=100) AllPkPosVal(IOrd), AllPkPosEsd(IOrd)*10.0
        END DO
      ELSE
        DO I = 1, NTPeak
          IOrd = IOrdTem(i)
          WRITE(117,'(F12.4)',ERR=100) AllPkPosVal(IOrd)
        END DO
      END IF        
      CLOSE(117)
      CALL PopActiveWindowID
      RETURN
 99   CONTINUE
      CALL ErrorMessage("Sorry, could not open the file"//CHAR(13)//FNAME(1:KLEN))
      CALL PopActiveWindowID
      RETURN
 100  CONTINUE
      CALL ErrorMessage("Sorry, could not write to the file"//CHAR(13)//FNAME(1:KLEN))
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE Create_DicvolIndexFile
!
!*****************************************************************************
!
!U      SUBROUTINE PeakFind_Manual(IDVal_Current)
!U
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U      USE VARIABLES
!U
!U      TYPE(WIN_STYLE)    WINDOW
!U      LOGICAL FinishMenuMode
!U
!U      INCLUDE 'GLBVAR.INC'
!U
!U      DO
!U        CALL WMessage(EventType,EventInfo)
!U        SELECT CASE (EventType)
!U          CASE (MouseButDown)
!U            IF      (EventInfo%VALUE1 .EQ. LeftButton) THEN
!U              CALL Plot_Alter(EventInfo%GX,EventInfo%GY)
!U            ELSE IF (EventInfo%VALUE1 .EQ. RightButton) THEN
!U! Get to work on the cross-hair movement
!U              CALL MOVE_CROSSHAIR(EventInfo%GX,EventInfo%GY)
!U            END IF
!U          CASE (MenuSelect)
!U            IDVal_Current=EventInfo%Value1               
!U            IF (FinishMenuMode(IDVal_Current,ID_CrossHair_Cursor_Mode)) THEN
!U              CALL WMessageEnable(MouseMove, Disabled)
!U              CALL WMessageEnable(MouseButUp, Disabled)
!U              STATBARSTR(8)='Standard cursor'
!U              CALL WindowOutStatusBar(8,STATBARSTR(8))
!U              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
!U              IDCurrent_Cursor_mode=ID_Default_Mode
!U              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
!U              RETURN
!U            END IF
!U          CASE (KeyDown)                
!U            IF (EventInfo%VALUE1 .EQ. KeyEscape) THEN
!U              CALL WMessageEnable(MouseMove, Disabled)
!U              CALL WMessageEnable(MouseButUp, Disabled)
!U              STATBARSTR(8)='Standard cursor'
!U              CALL WindowOutStatusBar(8,STATBARSTR(8))
!U              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
!U              IDCurrent_Cursor_mode=ID_Default_Mode
!U              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
!U              RETURN
!U            ELSE
!U              CALL Check_KeyDown(EventInfo)
!U            END IF
!U          CASE (Expose,Resize)
!U            CALL Redraw()
!U        END SELECT
!U      END DO
!U
!U      END SUBROUTINE PeakFind_Manual
!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE MOVE_CROSSHAIR(xgtem,ygtem)
!U!
!U      USE WINTERACTER
!U
!U      TYPE(WIN_STYLE)    WINDOW
!U      TYPE(WIN_MESSAGE)  ZMESSAGE
!U
!U      INCLUDE 'PARAMS.INC'
!U
!U      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
!U
!U      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
!U      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
!U      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
!U        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
!U        XGGMIN,XGGMAX,YGGMIN,YGGMAX
!U      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!U!
!U      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
!U
!U      INCLUDE 'GLBVAR.INC'
!U
!U      LOGICAL HVLINE_THERE
!U
!U      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
!U        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
!U      REAL XXTEM(MOBSTIC),YYTEM(MOBSTIC),UUTEM(20,MOBSTIC),ZZTEM(20,MOBSTIC)
!U      INTEGER ITTEM(MOBSTIC),IOTEM(MOBSTIC)
!U      CHARACTER*100 HKLSTR
!U      CHARACTER*4   CHRFORM
!U      REAL     ARGKPLT(10),DSPLT(10)
!U      INTEGER IHPLT(3,10)
!U      INCLUDE 'Poly_Colours.inc'
!U      REAL    CHAR_SIZE,MARKER_SIZE
!U      LOGICAL ERROR_BAR
!U      COMMON /PROFDEF/ERROR_BAR,CHAR_SIZE,MARKER_SIZE 
!U!
!U! Get ready to put up the big cursor
!U      HVLINE_THERE=.FALSE.
!U      CALL WMessageEnable(MouseMove, Enabled)
!U      CALL WMessageEnable(MouseButUp, Enabled)
!U      CALL WCursorShape(CurCrossHair)
!U      CALL IPgUnitsToGrUnits(xpgmin,ypgmin,gxmin,gymin)
!U      CALL IPgUnitsToGrUnits(xpgmax,ypgmax,gxmax,gymax)
!U      gxav=0.5*(gxmax+gxmin)
!U      gyav=0.5*(gymax+gymin)
!U      gxwd=0.001*(gxmax-gxmin)
!U      gywd=0.001*(gymax-gymin)
!U      xgcur(1)=xgtem
!U      ygcur(1)=ygtem
!U      CALL IPgUnitsFromGrUnits(xgtem,ygtem,xcur(1),ycur(1))
!U      CALL WMessageEnable(MouseMove, Enabled)
!U      CALL WMessageEnable(MouseButUp, Enabled)
!U      IMOV = 0
!U      DO
!U        CALL WMessage(IZTYPE,ZMESSAGE)
!U          xgcur(2)=zmessage%GX
!U          ygcur(2)=zmessage%GY
!U          CALL IPgUnitsFromGrUnits(zmessage%GX,zmessage%GY,xcur(2),ycur(2))
!U          SELECT CASE (IZTYPE)
!U            CASE (Expose,Resize)
!U              CALL Redraw()
!U            CASE (KeyDown)
!U              KeyNumber=ZMessage%Value1
!U              CALL Check_KeyDown_PeakFind_Inner(KeyNumber,xcur(2),ycur(2))
!U            CASE (MouseButDown)
!U              IMOV = 0
!U            CASE (MouseMove)
!U! Set up the cross-hairs for peak finding
!U                  imov=imov+1
!U                  if (imov.eq.1) then
!U! Draw cross-hair
!U                    CALL IGrColourN(KolNumLargeCrossHair)
!U                    gxt0=xgcur(2)
!U                    gxt1=xgcur(2)-gxwd
!U                    gxt2=xgcur(2)+gxwd
!U                    gyt0=ygcur(2)
!U                    gyt1=ygcur(2)-gywd
!U                    gyt2=ygcur(2)+gywd
!U                    CALL IGrMoveTo(gxt1,gyt0)
!U                    CALL IGrLineTo(gxt2,gyt0)
!U                    CALL IGrMoveTo(gxt0,gyt1)
!U                    CALL IGrLineTo(gxt0,gyt2)
!U                  else
!U! Remove old cross-hair
!U                    if (imov.eq.2) then
!U                      call profile_plot(IPTYPE)
!U                      CALL IGrPlotMode('EOR')
!U                    else
!U                      CALL IGrMoveTo(gxmin,ygcurold)
!U                      CALL IGrLineTo(gxmax,ygcurold)
!U                      CALL IGrMoveTo(xgcurold,gymin)
!U                      CALL IGrLineTo(xgcurold,gymax)
!U!                     CALL IGrPlotMode('EOR')
!U                    endif
!U! Paint new cross-hair
!U                    CALL IGrColourN(KolNumLargeCrossHair)
!U!                    CALL IGrColourN(224)
!U                    CALL IGrMoveTo(gxmin,ygcur(2))
!U                    CALL IGrLineTo(gxmax,ygcur(2))
!U                    CALL IGrMoveTo(xgcur(2),gymin)
!U                    CALL IGrLineTo(xgcur(2),gymax)
!U                  end if
!U                  xgcurold=xgcur(2)
!U                  ygcurold=ygcur(2)
!U                if (xpgmax-xpgmin.le.200.) then
!U                  CALL IRealToString(xcur(2),statbarstr(2)(1:),'(f10.3)')
!U                else
!U                  CALL IRealToString(xcur(2),statbarstr(2)(1:),'(f10.1)')
!U                end if
!U                if (ypgmax-ypgmin.le.100.) then
!U                  CALL IRealToString(ycur(2),statbarstr(3)(1:),'(f10.3)')
!U                else
!U                  CALL IRealToString(ycur(2),statbarstr(3)(1:),'(f10.1)')
!U                end if
!U                DO ISB=2,3
!U                  CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
!U                END DO 
!U            CASE (MouseButUp)
!U! MouseButUp action for peak finding
!U! Remove old cross-hair
!U                CALL IGrMoveTo(gxmin,ygcurold)
!U                CALL IGrLineTo(gxmax,ygcurold)
!U                CALL IGrMoveTo(xgcurold,gymin)
!U                CALL IGrLineTo(xgcurold,gymax)
!U                CALL IGrPlotMode(' ')
!U                DO ISB=2,3
!U                 statbarstr(isb)='          '
!U                 CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
!U                END DO                
!U               CALL Profile_Plot(IPTYPE)
!U               RETURN 
!U            END SELECT
!U          END DO 
!U!
!U      END SUBROUTINE MOVE_CROSSHAIR
!U!
!*****************************************************************************
!
      SUBROUTINE Fit_PeakTop(xtem)
!
      EXTERNAL CHIQUAD
      REAL CHIQUAD
!
      INCLUDE 'PARAMS.INC'
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!
      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)

      INCLUDE 'GLBVAR.INC'

      LOGICAL HVLINE_THERE
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
!
      REAL VAR(3),DVAR(3),COVAR(3,3)
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)
!
      axdif=abs(xobs(nobs)-xobs(1))
      klose=1
      do ii=1,nobs
        atem=abs(xtem-xobs(ii))
        if (atem.le.axdif) then
          axdif=atem
          klose=ii
        end if
      end do
!
      nval=9
      imin=klose-4
      imax=klose+4
      if (imin.lt.1) then
        imin=1
        imax=9
      end if
      if (imax.gt.nobs) then
        imax=nobs
        imin=nobs-8
      end if
!
      xav=0.5*(xobs(imax)+xobs(imin)) 
      xran=0.5*(xobs(imax)-xobs(imin))      
      do i=1,9
        ii=imin+i-1
        xval(i)=(xobs(ii)-xav)/xran
        yval(i)=yobs(ii)
        eval(i)=eobs(ii)
      end do
!
      nvar=3
      var(1)=0.5*(yobs(imax)+yobs(imin))-yobs(imin+3)
      var(2)=0.5*(yobs(imax)-yobs(imin))
      var(3)=yobs(imin+3)
      dvar(1)=0.1*var(1)
      dvar(2)=0.1*var(2)
      dvar(3)=0.1*var(3)
!
      call simopt(var,dvar,covar,nvar,chiquad)
      xb=-0.5*var(2)/var(1)
      yb=var(1)*xb*xb+var(2)*xb+var(3)
      uran=xobs(imax)-xobs(imin)
      xobstic(numobstic)=xav+xran*xb
      yobstic(numobstic)=yb
      do i=1,9
        utt=xobs(imin)+0.125*float(i-1)*uran
        uobstic(i,numobstic)=utt
        ut=(utt-xav)/xran
        zobstic(i,numobstic)=var(1)*ut*ut+var(2)*ut+var(3)
      END DO

      END SUBROUTINE fit_peaktop
!
!*****************************************************************************
!
!U      SUBROUTINE Rebin_Profile()
!U!
!U! Rebins the profile
!U!
!U      INCLUDE 'PARAMS.INC'
!U      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
!U      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
!U
!U      INCLUDE 'statlog.inc'
!U!
!U      NBIN=(NOBS/LBIN)
!U      DO I=1,NBIN
!U        IST=(I-1)*LBIN
!U        XADD=0.
!U        YOADD=0.
!U        YCADD=0.
!U        YBADD=0.
!U        VADD=0.
!U        DO J=1,LBIN
!U          JJ=J+IST
!U          XADD=XADD+XOBS(JJ)
!U          YOADD=YOADD+YOBS(JJ)
!U          YCADD=YCADD+YCAL(JJ)
!U          YBADD=YBADD+YBAK(JJ)
!U          VADD=VADD+EOBS(JJ)**2
!U        END DO
!U        XBIN(I)=XADD/FLOAT(LBIN)
!U        YOBIN(I)=YOADD/FLOAT(LBIN)
!U        YCBIN(I)=YCADD/FLOAT(LBIN)
!U        YBBIN(I)=YBADD/FLOAT(LBIN)
!U        EBIN(I)=SQRT(VADD)/FLOAT(LBIN)
!U      END DO
!U      DataSetChange=DataSetChange+1
!U      CALL Get_IPMaxMin() 
!U!
!U      END SUBROUTINE Rebin_Profile
!
!*****************************************************************************
!
      SUBROUTINE Get_IPMaxMin()
!
      INCLUDE 'PARAMS.INC'
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
!
!
      do i=1,nbin
        if (xbin(i).gt.xpgmin) then
          ipmin=i
          goto 110
        end if
      end do
  110 do i=nbin,1,-1
        if (xbin(i).lt.xpgmax) then
          ipmax=i
          goto 112
        end if
      end do
  112 continue
!
      END SUBROUTINE Get_IPMaxMin
!
!*****************************************************************************
!
      SUBROUTINE PeakFit(IDVal_Current)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      LOGICAL FinishMenuMode

      INCLUDE 'GLBVAR.INC'

      DO
        CALL GetEvent
        IF (EventInfo%WIN .NE. 0) THEN ! Message from a dialogue
          SELECT CASE (EventType)
            CASE (CloseRequest)
              CALL WDialogSelect(IDD_Structural_Information)
              CALL WDialogHide()
          END SELECT
        ELSE
          SELECT CASE (EventType)
            CASE (MouseButDown)
              IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                CALL Plot_Alter
              ELSE IF(EventInfo%VALUE1 .EQ. RightButton) THEN
! Get to work on the cross-hair movement - fitting this time
                CALL Move_CrossHair_Fit
              END IF
            CASE (MenuSelect)
              IDVal_Current=EventInfo%Value1
              IF (FinishMenuMode(IDVal_Current,ID_Peak_Fitting_Cursor_Mode)) THEN
                CALL WMessageEnable(MouseMove, Disabled)
                CALL WMessageEnable(MouseButUp, Disabled)
                STATBARSTR(8)='Standard cursor'
                CALL WindowOutStatusBar(8,STATBARSTR(8))
                CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
                IDCurrent_Cursor_mode=ID_Default_Mode
                CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
                RETURN
              END IF
            CASE (KeyDown)                
              IF (EventInfo%VALUE1.EQ.KeyEscape) THEN
                CALL WMessageEnable(MouseMove, Disabled)
                CALL WMessageEnable(MouseButUp, Disabled)
                STATBARSTR(8)='Standard cursor'
                CALL WindowOutStatusBar(8,STATBARSTR(8))
                CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
                IDCurrent_Cursor_mode=ID_Default_Mode
                CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
                RETURN
              ELSE
                CALL Check_KeyDown
                CALL Check_KeyDown_PeakFit
              END IF
          END SELECT 
        END IF
      END DO

      END SUBROUTINE PeakFit
!
!*****************************************************************************
!
      SUBROUTINE Move_CrossHair_Fit

      USE WINTERACTER
      USE VARIABLES

      TYPE(WIN_STYLE)    WINDOW

      INCLUDE 'PARAMS.INC'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
       YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
       XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)

      INCLUDE 'GLBVAR.INC'

      LOGICAL HVLINE_THERE

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      REAL XXTEM(MOBSTIC),YYTEM(MOBSTIC),UUTEM(20,MOBSTIC),ZZTEM(20,MOBSTIC)
      INTEGER ITTEM(MOBSTIC),IOTEM(MOBSTIC)
      CHARACTER*100 HKLSTR
      CHARACTER*4   CHRFORM
      REAL ARGKPLT(10),DSPLT(10)
      INTEGER IHPLT(3,10)
      INCLUDE 'Poly_Colours.inc'
      COMMON /CURVAL/ XCurFirst,YCurFirst

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
      NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
      REAL XPF_PTEM(MAX_NPPR,MAX_NPFR),YPF_PTEM(MAX_NPPR,MAX_NPFR)
      REAL XXFTEM(2,MAX_NPFR),YPkFitTem(MAX_FITPT)
      INTEGER IILOTEM(MAX_NPFR),IIHITEM(MAX_NPFR),IIRANGT(MAX_NPFR),NNTEM(MAX_NPFR)
      INTEGER IPF_RPtTem(MAX_NPFR) 
!
! Get ready to put up the big cursor
      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
      CALL WCursorShape(CurCrossHair)
      CALL IPgUnitsToGrUnits(xpgmin,ypgmin,gxmin,gymin)
      CALL IPgUnitsToGrUnits(xpgmax,ypgmax,gxmax,gymax)
!
      gxav=0.5*(gxmax+gxmin)
      gyav=0.5*(gymax+gymin)
      gxwd=0.001*(gxmax-gxmin)
      gywd=0.001*(gymax-gymin)

      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1),ygcur(1),xcur(1),ycur(1))
      xcurfirst=xcur(1)
      ycurfirst=ycur(1)
      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
!
! The first WMessage loop is solely concerned with determining the range
! over which we will fit the Bragg peak(s) so we will only check out
! Expose,Resize, MouseMove, MouseButUp and a very limited number of
! KeyDown options at this first stage
      IMOV = 0
      DO
        CALL GetEvent
          xgcur(2) = EventInfo%GX
          ygcur(2) = EventInfo%GY
          CALL IPgUnitsFromGrUnits(xgcur(2),ygcur(2),xcur(2),ycur(2))
          SELECT CASE (EventType)
!U            CASE (Expose,Resize)
!U              CALL Redraw()
            CASE (KeyDown)
              KeyNumber = EventInfo%Value1
              CALL Check_KeyDown_PeakFit_Inner(KeyNumber,xcur(1),xcur(2))
            CASE (MouseButDown)
              imov=0
            CASE (MouseMove)
! Set up the cross-hairs for peak finding
                  imov=imov+1
                  if (imov.eq.1) then
! Draw cross-hair
                    CALL IGrColourN(KolNumLargeCrossHair)
                    CALL IGrFillPattern(Hatched,Medium,DiagUp)
                    CALL IGrRectangle(xgcur(1),gymin,xgcur(2),gymax)
                    CALL IGrFillPattern(Outline,Medium,DiagUp)
                  else
! Remove old cross-hair
                    if (imov.eq.2) then
                      call profile_plot(IPTYPE)
                      CALL IGrPlotMode('EOR')
                    else
                      CALL IGrFillPattern(Hatched,Medium,DiagUp)
                      CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
                      CALL IGrFillPattern(Outline,Medium,DiagUp)
                    endif
! Paint new cross-hair
                    CALL IGrColourN(KolNumLargeCrossHair)
                    CALL IGrFillPattern(Hatched,Medium,DiagUp)
                    CALL IGrRectangle(xgcur(1),gymin,xgcur(2),gymax)
                    CALL IGrFillPattern(Outline,Medium,DiagUp)
                  end if
                  xgcurold=xgcur(2)
                if (xpgmax-xpgmin.le.200.) then
                  CALL IRealToString(xcur(2),statbarstr(2)(1:),'(f10.3)')
                else
                  CALL IRealToString(xcur(2),statbarstr(2)(1:),'(f10.1)')
                end if
                if (ypgmax-ypgmin.le.100.) then
                  CALL IRealToString(ycur(2),statbarstr(3)(1:),'(f10.3)')
                else
                  CALL IRealToString(ycur(2),statbarstr(3)(1:),'(f10.1)')
                end if
                DO ISB=2,3
                  CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
                END DO 
            CASE (MouseButUp)
! MouseButUp action for selecting the peak fitting region
! Remove old cross-hair
                CALL IGrFillPattern(Hatched,Medium,DiagUp)
                CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
                CALL IGrFillPattern(Outline,Medium,DiagUp)
                XPFR1=min(xcur(1),xcur(2))
                XPFR2=max(xcur(1),xcur(2))
! Determine peak fitting range
                IPFL1=1
                do ii=1,nbin
                  if (xbin(ii).ge.XPFR1) then
                    IPFL1=ii
                    goto 55
                  end if
                end do
 55             IPFL2=nbin
                do ii=nbin,1,-1
                  if (xbin(ii).le.XPFR2) then
                    IPFL2=ii
                    goto 60
                  end if
                end do
 60             continue
                IPFRANGE=1+IPFL2-IPFL1
             If (IPFRANGE.lt.15) then
                 CALL WMessageBox(OKOnly,ExclamationIcon,CommonOK, &
                 'Not enough points for peak fitting!'//CHAR(13)// &
                 'Try a larger range.', &
                 'Peak fitting range information')
                          CALL IGrPlotMode(' ')
             Else
                NumPeakFitRange=NumPeakFitRange+1
                XPF_Range(1,NumPeakFitRange)=XPFR1
                XPF_Range(2,NumPeakFitRange)=XPFR2
                IPF_Lo(NumPeakFitRange)=IPFL1
                IPF_Hi(NumPeakFitRange)=IPFL2
                IPF_Range(NumPeakFitRange)=1+IPF_Hi(NumPeakFitRange)-IPF_Lo(NumPeakFitRange)
! Now we have the range in terms of the profile point index
              CALL IGrPlotMode(' ')
              DO ISB=2,3
                statbarstr(isb)='          '
                CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
              END DO                
              CALL Profile_Plot(IPTYPE)
            End If
            RETURN 
        END SELECT
      END DO 

      END SUBROUTINE MOVE_CROSSHAIR_FIT
!
!*****************************************************************************
!
!U      SUBROUTINE Check_KeyDown_PeakFind(MESSAGE)
!U
!U      USE WINTERACTER
!U
!U      TYPE(WIN_MESSAGE) :: MESSAGE
!U      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
!U
!U! acts on various KeyDown options that are specific for peakfitting
!U      KeyNumber = MESSAGE%VALUE1
!U      xgcur(2) = MESSAGE%GX
!U      ygcur(2) = MESSAGE%GY
!U      CALL IPgUnitsFromGrUnits(xgcur(2),ygcur(2),xcur(2),ycur(2))
!U      CALL Check_KeyDown_PeakFind_Inner(KeyNumber,xcur(2),ycur(2))
!U
!U      END SUBROUTINE Check_KeyDown_PeakFind
!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown_PeakFit

      USE WINTERACTER
      USE VARIABLES

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
      COMMON /CURVAL/ XCurFirst,YCurFirst

! acts on various KeyDown options that are specific for peakfitting
      KeyNumber=EventInfo%Value1
      xgcur(2)=EventInfo%GX
      ygcur(2)=EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(2),ygcur(2),xcur(2),ycur(2))
      CALL Check_KeyDown_PeakFit_Inner(KeyNumber,XCurFirst,xcur(2))

      END SUBROUTINE Check_KeyDown_PeakFit
!
!*****************************************************************************
!
!U      SUBROUTINE Check_KeyDown_PeakFind_Inner(KeyNumber,xval,yval)
!U
!U      USE WINTERACTER
!U
!U      INCLUDE 'PARAMS.INC'
!U
!U      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
!U      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
!U      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
!U        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
!U        XGGMIN,XGGMAX,YGGMIN,YGGMAX
!U      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!U
!U      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
!U        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
!U      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
!U      REAL XXTEM(MOBSTIC),YYTEM(MOBSTIC),UUTEM(20,MOBSTIC),ZZTEM(20,MOBSTIC)
!U
!U      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
!U
!U      INCLUDE 'GLBVAR.INC'
!U
!U      INTEGER ITTEM(MOBSTIC),IOTEM(MOBSTIC)
!U      CHARACTER*100 HKLSTR
!U      CHARACTER*4   CHRFORM
!U      REAL ARGKPLT(10),DSPLT(10)
!U      INTEGER IHPLT(3,10)
!U      INCLUDE 'Poly_Colours.inc'
!U      REAL CHAR_SIZE,MARKER_SIZE
!U      LOGICAL ERROR_BAR
!U      COMMON /PROFDEF/ERROR_BAR,CHAR_SIZE,MARKER_SIZE
!U
!U      LOGICAL Confirm ! Function
!U
!U      xcur(2)=xval
!U      ycur(2)=yval
!U      IF (KeyNumber.eq.KeyReturn) THEN
!U        numobstic=numobstic+1
!U! Simple cursor location
!U        itypot(numobstic)=0
!U        xobstic(numobstic)=xcur(2)
!U        axdif=abs(xobs(nobs)-xobs(1))
!U        do ii=1,nobs
!U          atem=abs(xcur(2)-xobs(ii))
!U          if (atem.le.axdif) then
!U            axdif=atem
!U            yobstic(numobstic)=yobs(ii)
!U          end if
!U        end do
!U                call Upload_Positions()
!U                CALL IGrPlotMode(' ')
!U                call Profile_Plot(IPTYPE)
!U                CALL IGrPlotMode('EOR')
!U              ELSE IF (KeyNumber.eq.KeyInsert) THEN
!U                numobstic=numobstic+1
!U                itypot(numobstic)=1
!U! Fit to the top of the peak
!U                call fit_peaktop(xcur(2))
!U                CALL IGrPlotMode(' ')
!U                call Profile_Plot(IPTYPE)
!U                CALL IGrPlotMode('EOR')
!U              ELSE IF (KeyNumber.eq.KeyDeleteUnder) THEN
!U! Delete the nearest peak but ask first ...
!U                IF (NUMOBSTIC.EQ.0) THEN
!U                 CALL ErrorMessage('No tickmarks to delete!')
!U                ELSE
!U                 IF (Confirm('Do you really want to'//CHAR(13)//' delete this tickmark?')) THEN
!U! Delete the closest tickmark ...
!U                  ATEM=ABS(XOBSTIC(1)-XCUR(2))
!U                  LTEM=1
!U                  DO II=1,NUMOBSTIC
!U                    AATEM=ABS(XOBSTIC(II)-XCUR(2))
!U                    IF (AATEM.LT.ATEM) THEN
!U                      LTEM=II
!U                      ATEM=AATEM
!U                    END IF
!U                  END DO
!U                  KK=0
!U                  DO II=1,NUMOBSTIC
!U                    IF (II.NE.LTEM) THEN
!U                      KK=KK+1
!U                      XXTEM(KK)=XOBSTIC(II)
!U                      YYTEM(KK)=YOBSTIC(II)
!U                      ITTEM(KK)=ITYPOT(II)
!U                      IOTEM(KK)=IORDOT(II)
!U                      DO JJ=1,9
!U                        UUTEM(JJ,KK)=UOBSTIC(JJ,II)
!U                        ZZTEM(JJ,KK)=ZOBSTIC(JJ,II)
!U                      END DO
!U                    END IF
!U                  END DO
!U                  NUMOBSTIC=NUMOBSTIC-1
!U                  DO II=1,NUMOBSTIC
!U                      KK=II
!U                      XOBSTIC(II)=XXTEM(KK)
!U                      YOBSTIC(II)=YYTEM(KK)
!U                      ITYPOT(II)=ITTEM(KK)
!U                      IORDOT(II)=IOTEM(KK)
!U                      DO JJ=1,9
!U                        UOBSTIC(JJ,II)=UUTEM(JJ,KK)
!U                        ZOBSTIC(JJ,II)=ZZTEM(JJ,KK)
!U                      END DO
!U                  END DO                                  
!U                 END IF
!U                 CALL Upload_Positions()
!U                         CALL Upload_Widths()
!U                END IF
!U              ELSE IF (KeyNumber.eq.77 .or. KeyNumber.eq.109) THEN
!U! KeyNumber=M/m
!U! Find the Miller index of the nearest Bragg peak ...
!U                IF (NTIC.GT.0) THEN
!U                  ATEM=ABS(ARGK(1)-XCUR(2))
!U                  DO II=1,NTIC
!U                    AATEM=ABS(ARGK(II)-XCUR(2))
!U                    ATEM=MIN(ATEM,AATEM)
!U                  END DO
!U                  NNP=0
!U                  DO II=1,NTIC
!U                    AATEM=ABS(ARGK(II)-XCUR(2))
!U                    IF (AATEM.EQ.ATEM) THEN
!U                      NNP=NNP+1
!U                      DO I3=1,3
!U                       IHPLT(I3,NNP)=IH(I3,II)
!U                      END DO
!U                      ARGKPLT(NNP)=ARGK(II)
!U                      DSPLT(NNP)=1./DSTAR(II)
!U                    END IF
!U                  END DO
!U                  hklstr=' '
!U                  CHRFORM='(I1)'
!U                  ILOC=0
!U                  DO IPP=1,NNP
!U                    ILOC=ILOC+1
!U                    HKLSTR(ILOC:ILOC)='('
!U                    DO I3=1,3
!U                      IF (IHPLT(I3,IPP).LE.-10.) THEN
!U                        ISF=3
!U                      ELSE IF (IHPLT(I3,IPP).LT.0.AND.IHPLT(I3,IPP).GT.-10.) THEN
!U                        ISF=2
!U                      ELSE IF (IHPLT(I3,IPP).GE.0.AND.IHPLT(I3,IPP).LT.10.) THEN
!U                        ISF=1
!U                      ELSE
!U                        ISF=2
!U                      END IF
!U                      CALL IntegerToString(ISF,CHRFORM(3:3),'(I1)')
!U                      CALL IntegerToString(IHPLT(I3,IPP),&
!U                         HKLSTR(ILOC+1:ILOC+ISF),CHRFORM)
!U                      ILOC=ILOC+ISF+1
!U                      HKLSTR(ILOC:ILOC)=' '
!U                    END DO
!U                    HKLSTR(ILOC:ILOC)=')'
!U                  END DO
!U                  CALL WindowOutStatusBar(2,HKLSTR)
!U                  CALL IGrCharJustify('L')
!U                  CALL IGrColourN(KolNumCTic)
!U                  CHSTEM=0.75*Char_Size
!U                  CALL IGrCharSize(chstem,chstem)
!U                  CALL IGrCharRotate(90.)
!U                  xtem=xcur(2)-0.02*(xpgmax-xpgmin)
!U                  ytem=ycur(2)+0.01*(ypgmax-ypgmin)
!U                  CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
!U                  CALL IGrCharOut(xgtem,ygtem,HKLSTR(:LEN_TRIM(HKLSTR)))
!U                  CALL IGrCharRotate(0.)
!U                  hklstr=' '
!U          IF (argkplt(nnp).ge.200.) THEN
!U            CALL IRealToString(ARGKPLT(NNP),&
!U            HKLSTR(1:),'(F10.2)')
!U          ELSE
!U            CALL IRealToString(ARGKPLT(NNP),&
!U            HKLSTR(1:),'(F10.4)')
!U          END IF
!U          CALL WindowOutStatusBar(3,HKLSTR)
!U        END IF
!U      ELSE
!U        CALL IGrPlotMode(' ')
!U        RETURN
!U      ENDIF
!U
!U      END SUBROUTINE Check_KeyDown_PeakFind_Inner
!U!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown_PeakFit_Inner(KeyNumber,xcur1,xcur2)

      USE WINTERACTER
      USE DRUID_HEADER

      TYPE(WIN_STYLE)    WINDOW

      INCLUDE 'PARAMS.INC'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)

      INCLUDE 'GLBVAR.INC'

      LOGICAL HVLINE_THERE

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      REAL XXTEM(MOBSTIC),YYTEM(MOBSTIC),UUTEM(20,MOBSTIC),ZZTEM(20,MOBSTIC)
      INTEGER ITTEM(MOBSTIC),IOTEM(MOBSTIC)
      CHARACTER*100 HKLSTR
      CHARACTER*4   CHRFORM
      REAL ARGKPLT(10),DSPLT(10)
      INTEGER IHPLT(3,10)
      INCLUDE 'Poly_Colours.inc'

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
        NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
        XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
        IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
      REAL XPF_PTEM(MAX_NPPR,MAX_NPFR),YPF_PTEM(MAX_NPPR,MAX_NPFR)
      REAL XXFTEM(2,MAX_NPFR),XPkFitTem(MAX_FITPT),YPkFitTem(MAX_FITPT)
      INTEGER IILOTEM(MAX_NPFR),IIHITEM(MAX_NPFR),IIRANGT(MAX_NPFR),NNTEM(MAX_NPFR)
      INTEGER IPF_RPtTem(MAX_NPFR)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      REAL PkFnValTem(MPkDes,Max_NPFR),PkFnEsdTem(MPkDes,Max_NPFR), &
!      PkFnCalTem(MPkDes,Max_NPFR),PkFnVarValTem(3,MPkDes),PkFnVarEsdTem(3,MPkDes), &
      PkAreaValTem(MAX_NPPR,MAX_NPFR),PkAreaEsdTem(MAX_NPPR,MAX_NPFR), &
      PkPosValTem(MAX_NPPR,MAX_NPFR),PkPosEsdTem(MAX_NPPR,MAX_NPFR),PkPosAvTem(MAX_NPFR)

      INTEGER NPeaksFitted, ICurSel
      LOGICAL Check_TicMark_Data

      xcur(1) = xcur1
      xcur(2) = xcur2
      IF (KeyNumber .EQ. KeyDeleteUnder) THEN
! Delete the nearest peak fitting range but ask first ...
                IF (NumPeakFitRange .EQ. 0) THEN
                 CALL ErrorMessage('No peak fitting ranges to delete!')
                ELSE
                 CALL WMessageBox(YesNo,QuestionIcon,CommonYes, &
                 'Do you really want to'//CHAR(13)//' delete this peak fitting range?', &
                 'Delete peak fitting confirmation')
                 IF (WInfoDialog(4).EQ.CommonYes) THEN
! Delete the closest peak fitting range ...
                  DO II = 1, NumPeakFitRange
                    IF (XCUR(2).GE.XPF_Range(1,II) .AND. &
                        XCUR(2).LE.XPF_Range(2,II) ) THEN
! The cursor is sitting inside the peak range - remove the range
! and shuffle all the regions that are already there.
                      LTEM = II
                    END IF
                  END DO
                  KK=0
                  KR=0
                  DO II=1,NumPeakFitRange
                    IF (II.NE.LTEM) THEN
                      KK=KK+1
                      XXFTEM(1,KK) = XPF_Range(1,II)
                      XXFTEM(2,KK) = XPF_Range(2,II)
                      IILOTEM(KK)= IPF_Lo(II)
                      IIHITEM(KK)= IPF_Hi(II)
                      IIRANGT(KK)= IPF_Range(II)
                      NNTEM(KK)=   NumInPFR(II)
                      PkPosAvTem(KK)=PkPosAv(II)
                      DO IP=1,MPkDes
                        PkFnValTem(IP,KK)=PkFnVal(IP,II)
                        PkFnEsdTem(IP,KK)=PkFnEsd(IP,II)
                      END DO
                      DO IP=1,NumInPFR(II)
                        XPF_PTEM(IP,KK)=XPF_Pos(IP,II)
                        YPF_PTEM(IP,KK)=YPF_Pos(IP,II)
                        PkPosValTem(IP,KK)=PkPosVal(IP,II)
                        PkPosEsdTem(IP,KK)=PkPosEsd(IP,II)
                        PkAreaValTem(IP,KK)=PkAreaVal(IP,II)
                        PkAreaEsdTem(IP,KK)=PkAreaEsd(IP,II)
                      END DO
!                      IPF_RPtTem(KK)=KR
                      DO IC=1,IPF_Range(II)
                        KR=KR+1
                        XPkFitTem(KR)=XPeakFit(IPF_RPt(II)+IC)
                        YPkFitTem(KR)=YPeakFit(IPF_RPt(II)+IC)
                      END DO
                    END IF
                  END DO
                  KR=0
                  NumPeakFitRange=NumPeakFitRange-1
                  If (NumPeakFitRange.gt.0) then
                    DO II=1,NumPeakFitRange
                      KK=II
                      XPF_Range(1,II)=XXFTEM(1,KK)
                      XPF_Range(2,II)=XXFTEM(2,KK)
                      IPF_Lo(II)=IILOTEM(KK)
                      IPF_Hi(II)=IIHITEM(KK)
                      IPF_Range(II)=IIRANGT(KK)
                      NumInPFR(II)=NNTEM(KK)
                      PkPosAv(II)=PkPosAvTem(II)
                      DO IP=1,MPkDes
                        PkFnVal(IP,II)=PkFnValTem(IP,II)
                        PkFnEsd(IP,II)=PkFnEsdTem(IP,II)
                      END DO
                      DO IP=1,NumInPFR(II)
                        XPF_Pos(IP,II)=XPF_PTEM(IP,II)
                        YPF_Pos(IP,II)=YPF_PTEM(IP,II)
                        PkPosVal(IP,II)=PkPosValTem(IP,II)
                        PkPosEsd(IP,II)=PkPosEsdTem(IP,II)
                        PkAreaVal(IP,II)=PkAreaValTem(IP,II)
!>> JCC - Whoops ... I reckon theres a bracket in the wrong place here ...
!                        PkAreaEsd(IP,II)=PkAreaEsd(TemIP,II)
!>> Changed to
                                 PkAreaEsd(IP,II)=PkAreaEsdTem(IP,II)
!>> Much better me thinks!                             
                      END DO
                      IPF_RPt(II)=KR
                      DO IC=1,IPF_Range(II)
                        KR=KR+1
                        XPeakFit(KR)=XPkFitTem(KR)
                        YPeakFit(KR)=YPkFitTem(KR)
                      END DO
                    END DO
                  END IF
                  II=NumPeakFitRange+1
                  NumInPFR(II)=0
                  IPF_RPt(II)=KR
!>> JCC Next line to zero the deleted range value completely
                          IPF_RPt(II+1)=0 
                  XPF_Range(1,II)=-9999.0 
                  XPF_Range(2,II)=-9999.0
                  DO IC=1,IPF_Range(II)
                    KR=KR+1
                    XPeakFit(KR)=-9999.0
                    YPeakFit(KR)=0.
                  END DO
                  IPF_Range(II)=0                            
                 END IF ! WInfoDialog(4).EQ.CommonYes
                END IF  ! NumPeakFitRange.eq.0
                CALL IGrPlotMode(' ')
                CALL Profile_Plot(IPTYPE)
!                CALL IGrPlotMode('EOR')
                        CALL Upload_Widths
                        CALL Upload_Positions
              ELSE IF (KeyNumber.ge.49 .and. KeyNumber.le.57) THEN
! KeyNumber=1-9: locating peak positions...
! Are we in a peak range?
                IF (NumPeakFitRange.gt.0) then
                  InRange=0
                  DO II=1,NumPeakFitRange
                    IF (XCur(2).GE.XPF_Range(1,II) .AND. &
                        XCur(2).LE.XPF_Range(2,II) ) THEN
! The cursor is sitting inside a peak range - go for it!
                      InRange=II
                    END IF
                  END DO
                  IF (InRange.ne.0) THEN
                    NTPeak=KeyNumber-48
                    NTem=NumInPFR(InRange)+1
                    IF(NTPeak.gt.NTem) THEN
! We've gone for too big a number - ignore
                    ELSE IF(NTPeak.eq.NTem) THEN
! Here's the next peak
                      NumInPFR(InRange)=NTem
                      XPF_Pos(NTem,InRange)=XCur(2)
                      ATem=ABS(XCur(2)-XBin(IPF_Lo(InRange)))
                      DO IP=IPF_Lo(InRange),IPF_Hi(InRange)
                        ANew=ABS(XCur(2)-XBin(IP))
                        IF (ANew.le.ATem) THEN
                          ATem=ANew
                          YPF_Pos(NTem,InRange)=YOBin(IP)
                        END IF
                      END DO
                    ELSE
! Reposition an existing peak
                      XPF_Pos(NTPeak,InRange)=XCur(2)
                      ATem=ABS(XCur(2)-XBin(IPF_Lo(InRange)))
                      DO IP=IPF_Lo(InRange),IPF_Hi(InRange)
                        ANew=ABS(XCur(2)-XBin(IP))
                        IF (ANew.le.ATem) THEN
                          ATem=ANew
                          YPF_Pos(NTPeak,InRange)=YOBin(IP)
                        END IF
                      END DO
                    END IF ! NTPeak.eq.NTem
                  END IF ! InRange.ne.0
                          
                END IF ! NumPeakFitRange.gt.0
! We've got ourselves a new initial peak position
                CALL IGrPlotMode(' ')
                CALL Profile_Plot(IPTYPE)
                CALL IGrPlotMode('EOR')
              ELSE IF (KeyNumber.eq.48 .or. KeyNumber.eq.KeyReturn) THEN
! KeyNumber=0 or KeyReturn: get ready to fit peaks ...
! Check if in a peak range - if not tell the user...
                IF (NumPeakFitRange.gt.0) then
                  InRange=0
                  DO II=1,NumPeakFitRange
                    IF (XCur(2).GE.XPF_Range(1,II) .AND. &
                        XCur(2).LE.XPF_Range(2,II) ) THEN
! The cursor is sitting inside a peak range - go for it!
                      InRange=II
                    END IF
                  END DO
                  IF (InRange .EQ. 0) THEN
! Tell the user to place the cursor in the range to be fitted.
                    CALL ErrorMessage('Place the cursor in a peak fitting range.')
                  ELSE
! We're ready to fit the Bragg peaks
                    CurrentRange=InRange
! One or more peaks to be fitted - initial positions determined by user
! If NumInPFR(InRange).eq.0 we're going to search & fit a single peak
                    CALL WCursorShape(CurHourGlass)
                    CALL MultiPeak_Fitter()
                    CALL WCursorShape(CurCrossHair)
                  END IF ! InRange.eq.0
                END IF ! NumPeakFitRange.gt.0                
                CALL IGrPlotMode(' ')
                CALL Profile_Plot(IPTYPE)
!                CALL IGrPlotMode('EOR')  
              ELSE
                CALL IGrPlotMode(' ')

              ENDIF
!


!     ICurSel = WinfoDialog(CurrentDialog)
!      IIII = InfoError(1)
!      CALL WDialogSelect(IDD_Peak_Positions)
!      IIII = InfoError(1)
!      IF ( NPeaksFitted .GE. 1) THEN
!                 CALL WDialogFieldState(ID_Index_Output,1)
!                        IIII = InfoError(1)
!      ELSE
!               CALL WDialogFieldState(ID_Index_Output,0)
!                        IIII = InfoError(1)
!      END IF
!      IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)

       IF ( Check_TicMark_Data() ) THEN
!>> JCC Track the number of fittable peaks
          NPeaksFitted = 0
          DO II=1,NumPeakFitRange
                  NPeaksFitted = NPeaksFitted + NumInPFR(II)
          END DO
            IF ( NPeaksFitted .GE. 3 ) THEN
                  CALL SetModeMenuState(1,1,0)
            ELSE
                  CALL SetModeMenuState(1,0,0)
            END IF
       ELSE
            CALL SetModeMenuState(1,-1,0)
       END IF

      RETURN
      END SUBROUTINE Check_KeyDown_PeakFit_Inner
!
!*****************************************************************************
!
      LOGICAL FUNCTION FinishMenuMode(IDV_Menu_Now,IDV_Current_Menu_Option)

      USE WINTERACTER
      USE DRUID_HEADER
!C>> JCC Was 17
      INTEGER, PARAMETER :: Num_Menu_Options = 27
!C>> JCC Dimension changed to Param value
      INTEGER, SAVE :: IDV_Menu_Option(Num_Menu_Options)
      INTEGER IDV_Menu_Now, IDV_Current_Menu_Option

! JvdS Lines labelled W generated a compiler warning that the variables used had not been initialised.

      IDV_Menu_Option( 1) = ID_Default_Mode 
!W      IDV_Menu_Option( 2) = ID_CrossHair_Cursor_Mode 
!W      IDV_Menu_Option( 3) = ID_Peak_Fitting_Cursor_Mode 
      IDV_Menu_Option( 4) = ID_get_lattice_constants 
      IDV_Menu_Option( 5) = ID_get_peak_widths 
      IDV_Menu_Option( 6) = ID_import_pro_file 
!W      IDV_Menu_Option( 7) = ID_import_tic_file 
      IDV_Menu_Option( 8) = ID_get_peak_intensities 
!W      IDV_Menu_Option( 9) = ID_import_uxd_file 
!W      IDV_Menu_Option(10) = ID_import_CCL  
!W      IDV_Menu_Option(11) = ID_import_ICSD
!W      IDV_Menu_Option(12) = ID_import_CIF 
!W      IDV_Menu_Option(13) = ID_export_CIF 
!W      IDV_Menu_Option(14) = ID_export_CCL 
      IDV_Menu_Option(15) = ID_help_about_Polyfitter 
      IDV_Menu_Option(16) = ID_PolyFitter_Help 
      IDV_Menu_Option(17) = ID_Polyfitter_Tips 
!C>> JCC Has to have these too 
      IDV_Menu_Option(18) = ID_Default_Mode 
      IDV_Menu_Option(19) = ID_import_xye_file 
      IDV_Menu_Option(20) = ID_file_exit 
      IDV_Menu_Option(21) = ID_file_print 
      IDV_Menu_Option(22) = ID_Pawley_Refinement_Mode 
      IDV_Menu_Option(23) = ID_get_data_properties 
      IDV_Menu_Option(24) = ID_get_crystal_symmetry 
      IDV_Menu_Option(25) = ID_get_peak_positions
      IDV_Menu_Option(26) = ID_Structure_Solution_mode
      IDV_Menu_Option(26) = ID_Start_Wizard
      IF (IDV_Menu_Now.EQ.IDV_Current_Menu_Option) Then
        FinishMenuMode=.FALSE.
      ELSE
        DO I = 1, Num_Menu_Options
          IF (IDV_Menu_Now .EQ. IDV_Menu_Option(I)) THEN
            FinishMenuMode = .TRUE.
            GOTO 100
          END IF
        END DO     
      END IF
 100  RETURN

      END FUNCTION  FinishMenuMode
