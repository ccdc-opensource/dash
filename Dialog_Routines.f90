!
!*****************************************************************************
!
      SUBROUTINE UPLOAD_RANGE()

      USE WINTERACTER
      USE DRUID_HEADER

      CHARACTER*8 chrfmt

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX

      atem1=max(abs(xpmin),abs(xpmax))
!C>> JCC Dont continue if range is silly
      if (atem1.LE.0.000000001) RETURN
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Data_Properties)
      atem2=0.0001*abs(xpmax-xpmin)
      item1=0.5+alog10(atem1)
      item2=max(0.,0.5-alog10(atem2))
      item=2+item1+item2
      chrfmt(1:2)='(F'
      IF (item .GE. 10) THEN
        CALL IntegerToString(item,chrfmt(3:4),'(I2)')
        inext = 5
      ELSE
        CALL IntegerToString(item,chrfmt(3:3),'(I1)')
        inext = 4
      END IF
      chrfmt(inext:inext)='.'
      inext = inext + 1
      CALL IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext = inext + 1
      chrfmt(inext:inext)=')'
      CALL WDialogPutReal(IDF_xmin,xpmin,chrfmt(1:inext))
      CALL WDialogPutReal(IDF_xmax,xpmax,chrfmt(1:inext))
      atem1=max(abs(ypmin),abs(ypmax))
      atem2=0.0001*abs(ypmax-ypmin)
      item1=0.5+alog10(atem1)
      item2=max(0.,0.5-alog10(atem2))
      item=2+item1+item2
      chrfmt(1:2)='(F'
      if (item.ge.10) then
        call IntegerToString(item,chrfmt(3:4),'(I2)')
        inext=5
      else
        call IntegerToString(item,chrfmt(3:3),'(I1)')
        inext=4
      end if
      chrfmt(inext:inext)='.'
      inext=inext+1
      call IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext=inext+1
      chrfmt(inext:inext)=')'
      CALL WDialogPutReal(IDF_ymin,ypmin,chrfmt(1:inext))
      CALL WDialogPutReal(IDF_ymax,ypmax,chrfmt(1:inext))
      CALL PopActiveWindowID

      END SUBROUTINE UPLOAD_RANGE
!
!*****************************************************************************
!
      LOGICAL FUNCTION PlotErrorBars
!
! This function retrieves the value of the 'plot error bars' checkbox in the plot options panel
!
! JvdS 7 Aug 2001
!
! RETURNS : .TRUE.  if user requested the error bars     to be plotted
!           .FALSE. if user requested the error bars not to be plotted
!
      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER tPlotErrorBars

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      CALL WDialogGetCheckBox(IDF_ErrorBar_Check,tPlotErrorBars) 
      PlotErrorBars = (tPlotErrorBars .EQ. 1)
      CALL PopActiveWindowID

      END FUNCTION PlotErrorBars
!
!*****************************************************************************
!
      LOGICAL FUNCTION PlotBackground
!
! This function retrieves the value of the 'plot background' checkbox in the plot options panel
!
! JvdS 22 Jul 2001
!
! RETURNS : .TRUE.  if user requested the background     to be plotted
!           .FALSE. if user requested the background not to be plotted
!
      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER tPlotBackground

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      CALL WDialogGetCheckBox(IDF_background_check,tPlotBackground)
      PlotBackground = (tPlotBackground .EQ. 1)
      CALL PopActiveWindowID

      END FUNCTION PlotBackground
!
!*****************************************************************************
!
