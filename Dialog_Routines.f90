!
!*****************************************************************************
!
      SUBROUTINE UPLOAD_RANGE

      USE DRUID_HEADER

      IMPLICIT NONE

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      LOGICAL, EXTERNAL :: FnPatternOK
      REAL          atem1, atem2
      INTEGER       iTem, iTem1, iTem2, iNext
      CHARACTER*(8) chrfmt

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_Data_Properties)
      ENDIF

      IF (.NOT. FnPatternOK()) THEN
        IF ( .NOT. IN_BATCH ) THEN
          CALL WDialogClearField(IDF_xmin)
          CALL WDialogClearField(IDF_xmax)
          CALL WDialogClearField(IDF_ymin)
          CALL WDialogClearField(IDF_ymax)
          CALL PopActiveWindowID
        ENDIF
        RETURN
      ENDIF
      atem1 = MAX(ABS(XPMIN),ABS(XPMAX))
      atem2 = 0.0001 * ABS(XPMAX-XPMIN)
      item1 = 0.5 + ALOG10(atem1)
      item2 = MAX(0.,0.5-ALOG10(atem2))
      item  = 2 + item1 + item2
      chrfmt(1:2) = '(F'
      IF (item .GE. 10) THEN
        CALL IntegerToString(item,chrfmt(3:4),'(I2)')
        inext = 5
      ELSE
        CALL IntegerToString(item,chrfmt(3:3),'(I1)')
        inext = 4
      END IF
      chrfmt(inext:inext) = '.'
      inext = inext + 1
      CALL IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext = inext + 1
      chrfmt(inext:inext) = ')'
      IF ( .NOT. IN_BATCH ) THEN
        CALL WDialogPutReal(IDF_xmin, XPMIN, chrfmt(1:inext))
        CALL WDialogPutReal(IDF_xmax, XPMAX, chrfmt(1:inext))
      ENDIF
      atem1 = MAX(ABS(ypmin),ABS(ypmax))
      atem2 = 0.0001 * ABS(ypmax-ypmin)
      item1 = 0.5 + ALOG10(atem1)
      item2 = MAX(0.,0.5-ALOG10(atem2))
      item  = 2 + item1 + item2
      chrfmt(1:2) = '(F'
      IF (item.GE.10) THEN
        CALL IntegerToString(item,chrfmt(3:4),'(I2)')
        inext = 5
      ELSE
        CALL IntegerToString(item,chrfmt(3:3),'(I1)')
        inext = 4
      ENDIF
      chrfmt(inext:inext) = '.'
      inext = inext + 1
      CALL IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext = inext + 1
      chrfmt(inext:inext) = ')'

      IF ( .NOT. IN_BATCH )  THEN
        CALL WDialogPutReal(IDF_ymin, ypmin, chrfmt(1:inext))
        CALL WDialogPutReal(IDF_ymax, ypmax, chrfmt(1:inext))
        CALL PopActiveWindowID
      ENDIF

      END SUBROUTINE UPLOAD_RANGE
!
!*****************************************************************************
!
