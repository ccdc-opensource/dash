!
!*****************************************************************************
!
      SUBROUTINE Plot_Alter
!
!  Enable button up and mouse movement events
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
      INTEGER IMOV, ISB
      REAL XMINT, XMAXT, YMINT, YMAXT, xgcurold, ygcurold


      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
! JCC Set the scale correctly. 
      CALL IPgUnits(xpgmin,ypgmin,xpgmax,ypgmax)
      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1),ygcur(1),xcur(1),ycur(1))
      XPGMINOLD = XPGMIN
      XPGMAXOLD = XPGMAX
      YPGMINOLD = YPGMIN
      YPGMAXOLD = YPGMAX
      IMOV = 0
      DO WHILE (.TRUE.)
        CALL GetEvent
        IF (EventInfo%WIN .EQ. 0) THEN
          CALL IPgUnitsFromGrUnits(EventInfo%GX,EventInfo%GY,xcur(2),ycur(2))
          xmint = MIN(xcur(1),xcur(2))
          xmaxt = MAX(xcur(1),xcur(2))
          ymint = MIN(ycur(1),ycur(2))
          ymaxt = MAX(ycur(1),ycur(2))
          IF (.NOT. (xmint.EQ.xmaxt .OR. ymint.EQ.ymaxt)) THEN
            CALL IRealToString(xmint,statbarstr(4)(1:),'(F10.3)')
            CALL IRealToString(xmaxt,statbarstr(5)(1:),'(F10.3)')
            IF (ymaxt-ymint .LE. 100.0) THEN
              CALL IRealToString(ymint,statbarstr(6)(1:),'(F10.3)')
              CALL IRealToString(ymaxt,statbarstr(7)(1:),'(F10.3)')
            ELSE
              CALL IRealToString(ymint,statbarstr(6)(1:),'(F10.1)')
              CALL IRealToString(ymaxt,statbarstr(7)(1:),'(F10.1)')
            ENDIF
            DO ISB = 4, 7
              CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
            ENDDO
          ENDIF
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
              ENDIF 
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
                IF (ABS(XCUR(2)-XCUR(1)).LT.0.003*(XPGMAX-XPGMIN)) RETURN
                IF (ABS(YCUR(2)-YCUR(1)).LT.0.003*(YPGMAX-YPGMIN)) RETURN
                XPGMIN=MIN(XCUR(1),XCUR(2))
                XPGMAX=MAX(XCUR(1),XCUR(2))  
                YPGMIN=MIN(YCUR(1),YCUR(2))
                YPGMAX=MAX(YCUR(1),YCUR(2))
!              ELSE IF (zmessage%value1.eq.RightButton) then
!                xpgmin=xpmin
!                xpgmax=xpmax
!                ypgmin=ypmin
!                ypgmax=ypmax
              ENDIF
              CALL IGrColourN(InfoGrScreen(PrevColReq))
              CALL Get_IPMaxMin()
              CALL Profile_Plot
              RETURN  
          END SELECT
        ENDIF
      ENDDO

      END SUBROUTINE Plot_Alter
!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown

      USE WINTERACTER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

! acts on various KeyDown options for the main window
      IF (EventInfo%VALUE1 .NE. KeyBackSpace) THEN
        XPGMINOLD = XPGMIN
        XPGMAXOLD = XPGMAX
        YPGMINOLD = YPGMIN
        YPGMAXOLD = YPGMAX
      ENDIF
      SELECT CASE (EventInfo%VALUE1)
        CASE (KeyPageLeft)
          xpgdif=xpgmax-xpgmin
          xpgmin=MAX(xpmin,xpgmin-0.02*xpgdif)
          xpgmax=xpgmin+xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyPageRight)
! We're going to move the graph to the right if we can
          xpgdif=xpgmax-xpgmin
          xpgmax=MIN(xpmax,xpgmax+0.02*xpgdif)
          xpgmin=xpgmax-xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyCursorLeft)
! We're going to move the graph to the left if we can
          xpgdif=xpgmax-xpgmin
          xpgmin=MAX(xpmin,xpgmin-0.25*xpgdif)
          xpgmax=xpgmin+xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyCursorRight)
! We're going to move the graph to the right if we can
          xpgdif=xpgmax-xpgmin
          xpgmax=MIN(xpmax,xpgmax+0.25*xpgdif)
          xpgmin=xpgmax-xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyLeftExtreme)
! We're going to move the graph as far left as we can
          xpgdif=xpgmax-xpgmin
          xpgmin=xpmin
          xpgmax=xpgmin+xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyRightExtreme)
! We're going to move the graph as far right as we can
          xpgdif=xpgmax-xpgmin
          xpgmax=xpmax
          xpgmin=xpgmax-xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot      
        CASE (KeyPageDown)
! We're going to expand the xscale by sqrt(2) if we can
          xpgdif=xpgmax-xpgmin
          xpgav=0.5*(xpgmax+xpgmin)
          xtem=MIN(0.5*(xpmax-xpmin),0.7071*xpgdif)
          xpgmin=xpgav-xtem
          xpgmax=xpgav+xtem
          IF (xpgmin.LT.xpmin) THEN
            xpgmin=xpmin
            xpgmax=xpgmin+2.*xtem
          ELSE IF (xpgmax.GT.xpmax) THEN
            xpgmax=xpmax
            xpgmin=xpgmax-2.*xtem
          ENDIF
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyPageUp)
! We're going to contract the xscale by sqrt(2)
          xpgdif=xpgmax-xpgmin
          xpgav=0.5*(xpgmax+xpgmin)
          xtem=0.3536*xpgdif
          xpgmin = xpgav - xtem
          xpgmax = xpgav + xtem
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyCursorDown)
! We're going to move the graph down if we can
          ypgdif = ypgmax - ypgmin
          ypgmin = MAX(ypmin,ypgmin-0.25*ypgdif)
          ypgmax = ypgmin + ypgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyCursorUp)
! We're going to move the graph up if we can
          ypgdif = ypgmax - ypgmin
          ypgmax = MIN(ypmax,ypgmax+0.25*ypgdif)
          ypgmin = ypgmax - ypgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (KeyUpExtreme)
! We're going to scale to min/max y over the current range
          ii = ypgmin
          ypgmin=ypgmax
          ypgmax=ii
          DO ii=1,NBIN
            IF(XBIN(ii).GE.xpgmin .AND. XBIN(ii).LE.xpgmax) THEN
              ypgmin=MIN(yobin(ii),ypgmin)
              ypgmax=MAX(yobin(ii),ypgmax)
            ENDIF
          ENDDO
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
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
          CALL Profile_Plot
        CASE (KeyHome)
! Back to full profile range
          xpgmin=xpmin
          xpgmax=xpmax
          ypgmin=ypmin
          ypgmax=ypmax
          CALL Get_IPMaxMin() 
          CALL Profile_Plot     
      END SELECT

      END SUBROUTINE Check_KeyDown
!
!*****************************************************************************
!
      SUBROUTINE Create_DicvolIndexFile
     
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'

      COMMON /ALLPEAKS/ NTPeak, AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak), &
        PkProb(MTPeak), IOrdTem(MTPeak), IHPk(3,MTPeak)

      INTEGER IFTYPE
      INTEGER IFLAGS, KLEN
      CHARACTER(LEN=75) :: FILTER
      REAL    Rvpar(2), Rcpar(5), Lambda, Rdens, Rmolwt, Rfom, Rexpzp, Reps
      INTEGER Isystem(6), UseErr, I, Iord
      REAL    Epsilon

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
      CALL WDialogGetReal(IDF_wavelength1, Lambda)
      CALL WDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
      CALL WDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
      CALL WDialogGetReal(IDF_Indexing_Maxa, Rcpar(1))
      CALL WDialogGetReal(IDF_Indexing_Maxb, Rcpar(2))
      CALL WDialogGetReal(IDF_Indexing_Maxc, Rcpar(3))
      CALL WDialogGetReal(IDF_Indexing_MinAng, Rcpar(4))
      CALL WDialogGetReal(IDF_Indexing_MaxAng, Rcpar(5))
      CALL WDialogGetReal(IDF_Indexing_Density, Rdens)
      CALL WDialogGetReal(IDF_Indexing_MolWt,   Rmolwt)
      CALL WDialogGetReal(IDF_Indexing_Fom,     Rfom)
      CALL WDialogGetReal(IDF_ZeroPoint,        Rexpzp)
      CALL WDialogGetCheckBox(IDF_Indexing_Cubic,      Isystem(1))
      CALL WDialogGetCheckBox(IDF_Indexing_Tetra,      Isystem(2))
      CALL WDialogGetCheckBox(IDF_Indexing_Hexa,       Isystem(3))
      CALL WDialogGetCheckBox(IDF_Indexing_Ortho,      Isystem(4))
      CALL WDialogGetCheckBox(IDF_Indexing_Monoclinic, Isystem(5))
      CALL WDialogGetCheckBox(IDF_Indexing_Triclinic,  Isystem(6))
      CALL WDialogGetRadioButton(IDF_Indexing_UseErrors,  UseErr)
      CALL WDialogGetReal(IDF_eps,Epsilon)
! Write it out 
      OPEN(UNIT=117,FILE=FNAME(1:KLEN),STATUS='UNKNOWN',ERR=99)
      WRITE(117,*,ERR=100) 'DICVOL input file created by DASH'
      WRITE(117,'(8(I3,1X))',ERR=100)  NTPeak, 2, (Isystem(i),i=1,6)
      WRITE(117,'(7(F8.2,1X))',ERR=100)  Rcpar(1),Rcpar(2),Rcpar(3),Rvpar(1),Rvpar(2),Rcpar(4),Rcpar(5)
      WRITE(117,'(F10.6,1X,3(F8.4,1X))',ERR=100)  Lambda, Rmolwt, Rdens, Rdens/50.0
      IF (UseErr .EQ. 2) THEN
        Reps = 1.0
      ELSE
        Reps = Epsilon
      ENDIF
      WRITE(117,'(F5.3,1X,F6.2,1X,F9.6)',ERR=100) Reps, Rfom, Rexpzp
      IF (UseErr .EQ. 2) THEN
        DO I = 1, NTPeak
          IOrd = IOrdTem(i)
          WRITE(117,'(F12.4,1X,F12.4)',ERR=100) AllPkPosVal(IOrd), AllPkPosEsd(IOrd)*10.0
        ENDDO
      ELSE
        DO I = 1, NTPeak
          IOrd = IOrdTem(i)
          WRITE(117,'(F12.4)',ERR=100) AllPkPosVal(IOrd)
        ENDDO
      ENDIF        
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

      END SUBROUTINE Create_DicvolIndexFile
!
!*****************************************************************************
!
      SUBROUTINE Get_IPMaxMin()

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      DO I = 1, NBIN
        IF (XBIN(I) .GT. XPGMIN) THEN
          IPMIN = I
          GOTO 110
        ENDIF
      ENDDO
  110 DO I = NBIN, 1, -1
        IF (XBIN(I) .LT. XPGMAX) THEN
          IPMAX = I
          GOTO 112
        ENDIF
      ENDDO
  112 CONTINUE

      END SUBROUTINE Get_IPMaxMin
!
!*****************************************************************************
!
      SUBROUTINE Move_CrossHair_Fit

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NTIC
      INTEGER                IH
      REAL                               ARGK
      REAL                                           DSTAR
      COMMON /PROFTIC/ NTIC, IH(3,MTIC), ARGK(MTIC), DSTAR(MTIC)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL XCUR(2), YCUR(2), XGCUR(2), YGCUR(2)

      COMMON /CURVAL/ XCurFirst

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
 
      REAL xgcurold
      SAVE xgcurold

! Get ready to put up the big cursor
      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
      CALL IPgUnitsToGrUnits(xpgmin,ypgmin,gxmin,gymin)
      CALL IPgUnitsToGrUnits(xpgmax,ypgmax,gxmax,gymax)
      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1),ygcur(1),xcur(1),ycur(1))
      XCurFirst = xcur(1)
      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
! The first WMessage loop is solely concerned with determining the range
! over which we will fit the Bragg peak(s) so we will only check out
! Expose, Resize, MouseMove, MouseButUp and a very limited number of
! KeyDown options at this first stage
      xgcurold = xgcur(1)
      CALL IGrPlotMode('EOR')
      CALL IGrColourN(KolNumLargeCrossHair)
      CALL IGrFillPattern(Hatched,Medium,DiagUp)
      CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
      CALL IGrFillPattern(Outline,Medium,DiagUp)
      CALL IGrPlotMode(' ')
      DO WHILE (.TRUE.)
        CALL GetEvent
        xgcur(2) = EventInfo%GX
        ygcur(2) = EventInfo%GY
        CALL IPgUnitsFromGrUnits(xgcur(2),ygcur(2),xcur(2),ycur(2))
        SELECT CASE (EventType)
          CASE (KeyDown)
            CALL Check_KeyDown_PeakFit_Inner
          CASE (MouseMove)
! Set up the cross-hairs for peak finding
! Draw cross-hair
! Remove old cross-hair
            CALL IGrPlotMode('EOR')
            CALL IGrColourN(KolNumLargeCrossHair)
            CALL IGrFillPattern(Hatched,Medium,DiagUp)
            CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
            CALL IGrFillPattern(Outline,Medium,DiagUp)
! Paint new cross-hair
            CALL IGrFillPattern(Hatched,Medium,DiagUp)
            CALL IGrRectangle(xgcur(1),gymin,xgcur(2),gymax)
            CALL IGrFillPattern(Outline,Medium,DiagUp)
            xgcurold = xgcur(2)
            CALL IGrPlotMode(' ')
            CALL IRealToString(xcur(2),statbarstr(2)(1:),'(F10.3)')
            IF (ypgmax-ypgmin.le.100.) THEN
              CALL IRealToString(ycur(2),statbarstr(3)(1:),'(F10.3)')
            ELSE
              CALL IRealToString(ycur(2),statbarstr(3)(1:),'(F10.1)')
            ENDIF
            DO ISB = 2, 3
              CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
            ENDDO 
          CASE (MouseButUp)
! MouseButUp action for selecting the peak fitting region
! Remove old cross-hair
            CALL IGrPlotMode('EOR')
            CALL IGrFillPattern(Hatched,Medium,DiagUp)
            CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
            CALL IGrFillPattern(Outline,Medium,DiagUp)
            CALL IGrPlotMode(' ')
            XPFR1 = MIN(xcur(1),xcur(2))
            XPFR2 = MAX(xcur(1),xcur(2))
! Determine peak fitting range
            IPFL1 = 1
            DO ii = 1, NBIN
              IF (XBIN(ii).GE.XPFR1) THEN
                IPFL1 = ii
                GOTO 55
              ENDIF
            ENDDO
 55         IPFL2 = NBIN
            DO ii = NBIN, 1, -1
              IF (XBIN(ii).LE.XPFR2) THEN
                IPFL2 = ii
                GOTO 60
              ENDIF
            ENDDO
 60         CONTINUE
            IPFRANGE = 1 + IPFL2 - IPFL1
            IF (IPFRANGE .LT. 15) THEN
              CALL ErrorMessage('Not enough points for peak fitting!'//CHAR(13)//'Try a larger range.')
            ELSE
              NumPeakFitRange = NumPeakFitRange + 1
! Ungrey 'Delete all peak fit ranges' button on toolbar
              CALL WMenuSetState(ID_ClearPeakFitRanges,ItemEnabled,WintOn)
! Ungrey 'Clear Peaks' button in Wizard window
              CALL PushActiveWindowID
              CALL WDialogSelect(IDD_PW_Page10)
              CALL WDialogFieldState(IDF_ClearPeakFitRanges,Enabled)
              CALL PopActiveWindowID
              XPF_Range(1,NumPeakFitRange) = XPFR1
              XPF_Range(2,NumPeakFitRange) = XPFR2
              IPF_Lo(NumPeakFitRange) = IPFL1
              IPF_Hi(NumPeakFitRange) = IPFL2
              IPF_Range(NumPeakFitRange) = 1 + IPF_Hi(NumPeakFitRange) - IPF_Lo(NumPeakFitRange)
! Now we have the range in terms of the profile point index
              DO ISB = 2, 3
                statbarstr(isb)='          '
                CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
              ENDDO                
!O              CALL Profile_Plot
              CALL IGrColourN(KolNumPanelDark)
              CALL IGrFillPattern(Hatched,Medium,DiagUp)
              CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
              CALL IGrFillPattern(Outline,Medium,DiagUp)
            ENDIF
            RETURN 
        END SELECT
      ENDDO 

      END SUBROUTINE MOVE_CROSSHAIR_FIT
!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown_PeakFit_Inner

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL XCUR(2), YCUR(2), XGCUR(2), YGCUR(2)

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

      REAL XPF_PTEM(MAX_NPPR,MAX_NPFR),YPF_PTEM(MAX_NPPR,MAX_NPFR)
      REAL XXFTEM(2,MAX_NPFR),XPkFitTem(MAX_FITPT),YPkFitTem(MAX_FITPT)
      INTEGER IILOTEM(MAX_NPFR),IIHITEM(MAX_NPFR),IIRANGT(MAX_NPFR),NNTEM(MAX_NPFR)

      COMMON /CURVAL/ XCurFirst
      
      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      REAL PkFnValTem(MPkDes,Max_NPFR),PkFnEsdTem(MPkDes,Max_NPFR), &
!      PkFnCalTem(MPkDes,Max_NPFR),PkFnVarValTem(3,MPkDes),PkFnVarEsdTem(3,MPkDes), &
      PkAreaValTem(MAX_NPPR,MAX_NPFR),PkAreaEsdTem(MAX_NPPR,MAX_NPFR), &
      PkPosValTem(MAX_NPPR,MAX_NPFR),PkPosEsdTem(MAX_NPPR,MAX_NPFR),PkPosAvTem(MAX_NPFR)

      LOGICAL Confirm ! Function

      xcur(1) = XCurFirst
      xgcur(2) = EventInfo%GX
      ygcur(2) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(2),ygcur(2),xcur(2),ycur(2))
      IF (EventInfo%VALUE1 .EQ. KeyDeleteUnder) THEN
! Delete the nearest peak fitting range but ask first ...
        IF (NumPeakFitRange .EQ. 0) THEN
          CALL ErrorMessage('No peak fitting ranges to delete!')
        ELSE
          IF (Confirm('Do you really want to'//CHAR(13)//' delete this peak fitting range?')) THEN
! Delete the closest peak fitting range ...
            DO II = 1, NumPeakFitRange
              IF (XCUR(2).GE.XPF_Range(1,II) .AND. XCUR(2).LE.XPF_Range(2,II) ) THEN
! The cursor is sitting inside the peak range - remove the range
! and shuffle all the regions that are already there.
                 LTEM = II
              ENDIF
            ENDDO
            KK = 0
            KR = 0
            DO II = 1, NumPeakFitRange
              IF (II.NE.LTEM) THEN
                KK = KK + 1
                XXFTEM(1,KK) = XPF_Range(1,II)
                XXFTEM(2,KK) = XPF_Range(2,II)
                IILOTEM(KK)= IPF_Lo(II)
                IIHITEM(KK)= IPF_Hi(II)
                IIRANGT(KK)= IPF_Range(II)
                NNTEM(KK)=   NumInPFR(II)
                PkPosAvTem(KK)=PkPosAv(II)
                DO IP = 1, MPkDes
                  PkFnValTem(IP,KK)=PkFnVal(IP,II)
                  PkFnEsdTem(IP,KK)=PkFnEsd(IP,II)
                ENDDO
                DO IP = 1, NumInPFR(II)
                  XPF_PTEM(IP,KK)=XPF_Pos(IP,II)
                  YPF_PTEM(IP,KK)=YPF_Pos(IP,II)
                  PkPosValTem(IP,KK)=PkPosVal(IP,II)
                  PkPosEsdTem(IP,KK)=PkPosEsd(IP,II)
                  PkAreaValTem(IP,KK)=PkAreaVal(IP,II)
                  PkAreaEsdTem(IP,KK)=PkAreaEsd(IP,II)
                ENDDO
!                      IPF_RPtTem(KK)=KR
                DO IC=1,IPF_Range(II)
                  KR=KR+1
                  XPkFitTem(KR)=XPeakFit(IPF_RPt(II)+IC)
                  YPkFitTem(KR)=YPeakFit(IPF_RPt(II)+IC)
                ENDDO
              ENDIF
            ENDDO
            KR = 0
            NumPeakFitRange = NumPeakFitRange - 1
            IF (NumPeakFitRange.EQ.0) THEN
! Grey out 'Delete all peak fit ranges' button on toolbar
              CALL WMenuSetState(ID_ClearPeakFitRanges,ItemEnabled,WintOff)
! Grey out 'Clear Peaks' button in Wizard window
              CALL PushActiveWindowID
              CALL WDialogSelect(IDD_PW_Page10)
              CALL WDialogFieldState(IDF_ClearPeakFitRanges,Disabled)
              CALL PopActiveWindowID
            ELSE
              DO II=1,NumPeakFitRange
                KK=II
                XPF_Range(1,II)=XXFTEM(1,KK)
                XPF_Range(2,II)=XXFTEM(2,KK)
                IPF_Lo(II)=IILOTEM(KK)
                IPF_Hi(II)=IIHITEM(KK)
                IPF_Range(II)=IIRANGT(KK)
                NumInPFR(II) = NNTEM(KK)
                PkPosAv(II) = PkPosAvTem(II)
                DO IP = 1, MPkDes
                  PkFnVal(IP,II) = PkFnValTem(IP,II)
                  PkFnEsd(IP,II) = PkFnEsdTem(IP,II)
                ENDDO
                DO IP = 1, NumInPFR(II)
                  XPF_Pos(IP,II) = XPF_PTEM(IP,II)
                  YPF_Pos(IP,II) = YPF_PTEM(IP,II)
                  PkPosVal(IP,II) = PkPosValTem(IP,II)
                  PkPosEsd(IP,II) = PkPosEsdTem(IP,II)
                  PkAreaVal(IP,II) = PkAreaValTem(IP,II)
                  PkAreaEsd(IP,II) = PkAreaEsdTem(IP,II)
                ENDDO
                IPF_RPt(II) = KR
                DO IC = 1, IPF_Range(II)
                  KR = KR + 1
                  XPeakFit(KR) = XPkFitTem(KR)
                  YPeakFit(KR) = YPkFitTem(KR)
                ENDDO
              ENDDO
            ENDIF
            II = NumPeakFitRange + 1
            NumInPFR(II) = 0
            IPF_RPt(II) = KR
! JCC Next line to zero the deleted range value completely
            IPF_RPt(II+1) = 0 
            XPF_Range(1,II) = -9999.0 
            XPF_Range(2,II) = -9999.0
            DO IC = 1, IPF_Range(II)
              KR = KR + 1
              XPeakFit(KR) = -9999.0
              YPeakFit(KR) = 0.0
            ENDDO
            IPF_Range(II) = 0                            
          ENDIF ! WInfoDialog(4).EQ.CommonYes
        ENDIF ! NumPeakFitRange.eq.0
        CALL IGrPlotMode(' ')
        CALL Profile_Plot
!                CALL IGrPlotMode('EOR')
        CALL Upload_Positions
        CALL Upload_Widths
      ELSE IF (EventInfo%VALUE1.GE.49 .AND. EventInfo%VALUE1.LE.57) THEN
! KeyNumber=1-9: locating peak positions...
! Are we in a peak range?
        IF (NumPeakFitRange.GT.0) THEN
          InRange = 0
          DO II = 1, NumPeakFitRange
            IF (XCur(2).GE.XPF_Range(1,II) .AND. XCur(2).LE.XPF_Range(2,II) ) THEN
! The cursor is sitting inside a peak range - go for it!
              InRange=II
            ENDIF
          ENDDO
          IF (InRange .NE. 0) THEN
            NTPeak = EventInfo%VALUE1 - 48
            NTem = NumInPFR(InRange) + 1
            IF (NTPeak .GT. NTem) THEN
! We've gone for too big a number - ignore
            ELSE IF(NTPeak.EQ.NTem) THEN
! Here's the next peak
              NumInPFR(InRange) = NTem
              XPF_Pos(NTem,InRange) = XCur(2)
              ATem = ABS(XCur(2)-XBin(IPF_Lo(InRange)))
              DO IP = IPF_Lo(InRange), IPF_Hi(InRange)
                ANew = ABS(XCur(2)-XBin(IP))
                IF (ANew.LE.ATem) THEN
                  ATem = ANew
                  YPF_Pos(NTem,InRange)=YOBin(IP)
                ENDIF
              ENDDO
            ELSE
! Reposition an existing peak
              XPF_Pos(NTPeak,InRange) = XCur(2)
              ATem = ABS(XCur(2) - XBin(IPF_Lo(InRange)))
              DO IP = IPF_Lo(InRange), IPF_Hi(InRange)
                ANew = ABS(XCur(2) - XBin(IP))
                IF (ANew.LE.ATem) THEN
                  ATem = ANew
                  YPF_Pos(NTPeak,InRange) = YOBin(IP)
                ENDIF
              ENDDO
            ENDIF ! NTPeak.eq.NTem
          ENDIF ! InRange.ne.0
        ENDIF ! NumPeakFitRange.gt.0
! We've got ourselves a new initial peak position
        CALL IGrPlotMode(' ')
        CALL Profile_Plot
        CALL IGrPlotMode('EOR')
      ELSE IF (EventInfo%VALUE1.EQ.48 .OR. EventInfo%VALUE1.EQ.KeyReturn) THEN
! KeyNumber=0 or KeyReturn: get ready to fit peaks ...
! Check if in a peak range - if not tell the user...
        IF (NumPeakFitRange.GT.0) then
          InRange = 0
          DO II = 1, NumPeakFitRange
            IF (XCur(2).GE.XPF_Range(1,II) .AND. XCur(2).LE.XPF_Range(2,II) ) THEN
! The cursor is sitting inside a peak range - go for it!
              InRange = II
            ENDIF
          ENDDO
          IF (InRange .EQ. 0) THEN
! Tell the user to place the cursor in the range to be fitted.
            CALL ErrorMessage('Place the cursor in a peak fitting range.')
          ELSE
! We're ready to fit the Bragg peaks
            CurrentRange = InRange
! One or more peaks to be fitted - initial positions determined by user
! If NumInPFR(InRange).eq.0 we're going to search & fit a single peak
            CALL WCursorShape(CurHourGlass)
            CALL MultiPeak_Fitter()
            CALL WCursorShape(CurCrossHair)
          ENDIF ! InRange.eq.0
        ENDIF ! NumPeakFitRange.gt.0                
        CALL IGrPlotMode(' ')
        CALL Profile_Plot
      ELSE
        CALL IGrPlotMode(' ')
      ENDIF
      CALL CheckIfWeCanDoAPawleyRefinement
      CALL CheckIfWeCanIndex

      END SUBROUTINE Check_KeyDown_PeakFit_Inner
!
!*****************************************************************************
!
      SUBROUTINE CheckIfWeCanIndex

      USE WINTERACTER
      USE DRUID_HEADER

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

      INTEGER I, NPeaksFitted
      INTEGER IndexOption

      NPeaksFitted = 0
      DO I = 1, NumPeakFitRange
        NPeaksFitted = NPeaksFitted + NumInPFR(I)
      ENDDO
      CALL PushActiveWindowID
      IF (NPeaksFitted .GE. 10) THEN
        CALL WDialogSelect(IDD_PW_Page7)
        CALL WDialogFieldState(IDNEXT,Enabled)
        CALL WDialogSelect(IDD_PW_Page8)
        CALL WDialogFieldState(IDNEXT,Enabled) ! The 'Run >' button
      ELSE
        CALL WDialogSelect(IDD_PW_Page7)
        CALL WDialogGetRadioButton(IDF_RADIO3,IndexOption) ! 'Index now' or 'Enter known cell'
        IF (IndexOption .EQ. 2) THEN
          CALL WDialogFieldState(IDNEXT,Enabled)
        ELSE
          CALL WDialogFieldState(IDNEXT,Disabled)
        ENDIF
        CALL WDialogSelect(IDD_PW_Page8)
        CALL WDialogFieldState(IDNEXT,Disabled) ! The 'Run >' button
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE CheckIfWeCanIndex
!
!*****************************************************************************
!
      SUBROUTINE CheckIfWeCanDoAPawleyRefinement

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WeCanDoAPawleyRefinement

      CALL PushActiveWindowID
      IF (WeCanDoAPawleyRefinement()) THEN
        CALL SetModeMenuState(0,1)
        CALL WDialogSelect(IDD_PW_Page10)
        CALL WDialogFieldState(IDNEXT,Enabled)
        CALL WDialogSelect(IDD_Pawley_Status)
        CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      ELSE
        CALL SetModeMenuState(0,-1)
        CALL WDialogSelect(IDD_PW_Page10)
        CALL WDialogFieldState(IDNEXT,Disabled)
        CALL WDialogSelect(IDD_Pawley_Status)
        CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE CheckIfWeCanDoAPawleyRefinement
!
!*****************************************************************************
!
      LOGICAL FUNCTION WeCanDoAPawleyRefinement

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

      LOGICAL, EXTERNAL :: Check_TicMark_Data
      INTEGER I, NPeaksFitted

! JCC Track the number of fittable peaks
      NPeaksFitted = 0
      DO I = 1, NumPeakFitRange
        NPeaksFitted = NPeaksFitted + NumInPFR(I)
      ENDDO
      WeCanDoAPawleyRefinement = (Check_TicMark_Data() .AND. (NPeaksFitted .GE. 3))

      END FUNCTION WeCanDoAPawleyRefinement
!
!*****************************************************************************
!
