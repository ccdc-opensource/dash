!
      subroutine Plot_SA_profile()
!************
!
!
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.,0.,1.,1.)
      CALL IGrAreaClear
      CALL IGrCharFont(1)
      CALL IGrColourN(159)
      CALL IPgScaling('LIN','LIN')
!
!
      CALL WDialogGetRadioButton(IDF_ProfileType_Radio1,IRAD_ProfileType)
      SELECT CASE (IRAD_ProfileType)
          CASE (1)
! Observed and calculated profiles
              CALL IPgNewGraph(3,nobs,' ',' ','XY')
              yshift=0.5*(ypgmax-ypgmin)
          CASE (2)
! The difference profile
              CALL IPgNewGraph(2,nobs,' ',' ','XY')
              yshift=0.5*(ypmax-ypmin)
      END SELECT
!
      DO I=1,NOBS
        ydif(i)=yshift+yobs(i)-ycalbest(i)
      END DO
!
!  draw axes and add scales
!
      CALL IPgAxes
      CALL IPgArea(.12,.12,.9,.95)
      CALL IPgUnits(xpgmin,ypgmin,xpgmax,ypgmax)
      CALL IPgClipRectangle('P')
      CALL IPgBorder()
      CALL IGrCharSize(1.,1.)
      CALL IPgXScale('TN')
      CALL IPgXScaleTop('T')
      CALL IGrCharSize(1.,1.2)
      CALL IPgYScaleLeft('TN')
      CALL IPgYScaleRight('T')
      CALL IGrCharSize(1.,1.2)
      CALL IPgXLabel('2 theta','C')
!
      SELECT CASE (IRAD_ProfileType)
          CASE (1)
! Observed and calculated profiles
            CALL IPgYLabelLeft('Counts','C9')
            CALL IPgStyle(1,0,0,0,4,0)
            CALL IPgStyle(2,0,3,0,0,31)
            CALL IPgMarker( 2, 14)
            CALL IPgStyle(3,0,0,0,159,0)
            CALL IPgXYPairs(xobs,ydif)
            CALL IGrCharSize(.3,.3)
            CALL IPgXYPairs(xobs,yobs)
            CALL IPgXYPairs(xobs,ycalbest)
          CASE (2)
! The difference profile
            CALL IPgYLabelLeft('Difference profile','C9')
            CALL IPgStyle(1,0,0,0,4,0)
            CALL IPgStyle(2,0,0,0,159,0)
            CALL IPgXYPairs(xobs,yobs)
            CALL IPgXYPairs(xobs,ydif)
      END SELECT
      end subroutine Plot_SA_profile