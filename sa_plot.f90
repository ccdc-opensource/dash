      subroutine SA_PLOT(iter)
!
      use winteracter
      use druid_header
      character*80 linstr
!
      parameter (maxiter=10000)
      common /pltstore/ xiter(maxiter),tstore(maxiter),&
      foptstore(maxiter),fpavstore(maxiter)
      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)

	  integer IcurSel
!
!
!
      if (iter.le.1) return
      ICurSel = WInfoDialog(CurrentDialog)

!
      CALL WDialogSelect(IDD_Anneal_Status)
      CALL IGrSelect(3, IDF_Status_Graph)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear
      CALL IGrCharFont(1)
!      CALL IGrCharSpacing('P')
      CALL IGrColourN(175)
!
! Check on whether we want log or linear y axis
      CALL WDialogGetRadioButton(IDF_Anneal_LogY,IRAD_AnnealLogLinY)
      SELECT CASE (IRAD_AnnealLogLinY)
          CASE (1)
! Observed and calculated profiles
            CALL IPgScaling('LIN','LOG')
          CASE (2)
! The difference profile
            CALL IPgScaling('LIN','LIN')
      END SELECT


      CALL IPgNewGraph(4,iter,' ',' ','XY')
!
!  draw axes and add scales
!


      CALL IPgAxes
      xgmin=1.
      xgmax=float(iter)
      CALL IRealMaxMin1(tstore,iter,tmin,tmax)
      CALL IRealMaxMin1(foptstore,iter,fmin,fmax)
      CALL IRealMaxMin1(fpavstore,iter,amin,amax)
      CALL IRealMaxMin1(chiprobest,iter,cmin,cmax)
      ygmin=0.9*min(tmin,fmin,amin,cmin)
      ygmax=1.1*max(tmax,fmax,amax,cmax)
      CALL IPgArea(.12,.12,.9,.95)
      CALL IPgUnits(xgmin,ygmin,xgmax,ygmax)
      CALL IGrCharSize(1.2,1.2)
!      CALL IPgYUserScale(yticks,31)
      CALL IPgBorder()
      CALL IPgXScale('TN')
      CALL IPgXScaleTop('T')
      CALL IGrCharSize(1.2,1.2)
      CALL IPgYScaleLeft('TN')
      CALL IPgYScaleRight('T')
!
      CALL IGrCharSize(1.2,1.3)
      CALL IPgXLabel('iteration number','C')
      CALL IPgYLabelLeft('chi-squared & temperature','C9')
      CALL IPgStyle(1,0,0,0,16,0)
      CALL IPgStyle(2,0,0,0,112,0)
      CALL IPgStyle(3,0,0,0,80,0)
      CALL IPgStyle(4,0,0,0,192,0)
      CALL IPgXYPairs(xiter,tstore)
      CALL IPgXYPairs(xiter,foptstore)
      CALL IPgXYPairs(xiter,fpavstore) 
      CALL IPgXYPairs(xiter,chiprobest)
!
	  IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)
	  return
      end