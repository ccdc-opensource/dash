!
!*****************************************************************************
!
      SUBROUTINE sa_move_status(ntotmov,movenow)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN   ) :: ntotmov, movenow
      INTEGER ipcol

      CALL WDialogSelect(IDD_SA_Action1)
      ipcol = InfoGrScreen(ColourReq)
      CALL IGrSelect(3,IDF_SAMove_picture)
      ruler = ntotmov
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(59)
      rulex1 = 0.0
      rulex2 = movenow
      CALL IGrFillPattern(Solid)
      CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
      CALL IGrColourN(159)
      rulex1 = rulex2
      rulex2 = ntotmov
      CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
      CALL IGrSelect(1,0)
      CALL IGrUnits(0.0,0.0,1.0,1.0)
      CALL IGrColourN(ipcol)

      END SUBROUTINE sa_move_status
!
!*****************************************************************************
!
