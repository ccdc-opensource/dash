!
!*****************************************************************************
!
      SUBROUTINE SA_OUTPUT(T,CHIMIN,CHIAV,CHIESD,dxvav,xvsig,flav,N,NUP,NDOWN,ntotmov)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      REAL T, CHIMIN, CHIAV, CHIESD
      INTEGER N, NUP, NDOWN, ntotmov
      REAL dxvav(*),xvsig(*),flav(*)

      INCLUDE 'params.inc'

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      INTEGER         Curr_SA_Iteration
      COMMON /ITRINF/ Curr_SA_Iteration

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      REAL ctem, tempvl
      REAL bchpro, bchmin, bpwval, avchi1, avchi2, avchi3, avchi4
      REAL tenow1, tenow2, ruler, rulex1, rulex2
      REAL, PARAMETER :: rminh = 0.01
      REAL, PARAMETER :: rmaxh = 0.99
!      INTEGER I

      IF ( in_batch ) &
        RETURN
      CALL SelectDASHDialog(IDD_SA_Action1)
      CALL WDialogPutReal(IDF_curr_temp, T, '(F8.2)')
      CALL WDialogPutReal(IDF_min_chisq, chimin, '(F8.2)')
      CALL WDialogPutReal(IDF_profile_chisq2, CHIPROBEST, '(F8.2)')
! best chi-squared scale
      bchmin = ALOG10(MAX(1.0,chimin))
! profile chi-squared scale
      bpwval = ALOG10(MAX(1.0,PAWLEYCHISQ))
      bchpro = ALOG10(MAX(1.0,CHIPROBEST))
      CALL WDialogPutReal(IDF_av_chisq,chiav,'(F8.2)')
      CALL WDialogPutInteger(IDF_total_moves,ntotmov)
      CALL WDialogPutInteger(IDF_SA_total_moves_label,nmpert)
! Temperature scale
      tempvl = ALOG10(MAX(1.0, T))
      avchi1 = ALOG10(MAX(1.0, chiav-chiesd))
      ctem = MAX(1.0, chiav+chiesd)
      avchi2 = ALOG10(MIN(10000.0, ctem))
      avchi3 = ALOG10(MAX(1.0, chiav-0.1*chiesd))
      ctem = MAX(1.0, chiav+0.1*chiesd)
      avchi4 = ALOG10(MIN(10000.0, ctem))
      CALL IGrFillPattern(Solid)
! Temperature
      CALL IGrSelect(3,IDF_T_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,1)
      CALL IGrColourN(95) ! Lightgreen
      tenow1 = tempvl-0.03
      tenow2 = tempvl+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Minimum chi squared
      CALL IGrSelect(3,IDF_minchisq_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,1)
      CALL IGrColourN(95) ! Lightgreen
      tenow1 = bchmin-0.03
      tenow2 = bchmin+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Average chi-squared
      CALL IGrSelect(3,IDF_avchisq_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,1)
      CALL IGrColourN(95) ! Lightgreen
      CALL IGrRectangle(avchi1,rminh,avchi2,rmaxh)
      CALL IGrColourN(128)
      CALL IGrRectangle(avchi3,rminh,avchi4,rmaxh)
! Profile chi squared
      CALL IGrSelect(3,IDF_prochisq_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,1)
      CALL IGrColourN(63)  ! Yellow
      tenow1 = bpwval-0.03
      tenow2 = bpwval+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
      CALL IGrColourN(95) ! Lightgreen
      tenow1 = bchpro-0.03
      tenow2 = bchpro+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Total moves
      CALL IGrSelect(3,IDF_SATotalMoves_picture)
      ruler = FLOAT(MaxMoves)
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(63)  ! Yellow
      rulex1 = 0.0
      rulex2 = FLOAT(ntotmov)
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159) ! Blue
      rulex1 = rulex2
      rulex2 = ruler
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
! Uphill, downhill, rejected
      CALL IGrSelect(3,IDF_SA_move_distribution)
      ruler = FLOAT(nmpert)
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      IF (Curr_SA_Iteration .EQ. 1) THEN
        CALL IGrColourN(208) ! Black
        CALL IGrRectangle(0.0,rminh,ruler,rmaxh)
      ELSE
        CALL IGrColourN(95) ! Lightgreen
        rulex1 = 0.0
        rulex2 = FLOAT(ndown)
        CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
        CALL IGrColourN(159) ! Blue
        rulex1 = rulex2
        rulex2 = rulex2 + FLOAT(nup)
        CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
        CALL IGrColourN(31) ! Red
        rulex1 = rulex2
        rulex2 = ruler
        CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      ENDIF
      CALL IGrSelect(1,0)
      CALL IGrUnits(0.0,0.0,1.0,1.0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
! Following lines write values to dialogue that hasn't been loaded into memory,
! nor is it displayed.
!U      CALL SelectDASHDialog(IDD_Parameter_Status_2)
!U      DO I = 1, N
!U        CALL WGridPutCellReal(IDF_CPL_grid,1,I,xopt(i),'(F12.5)')
!U      ENDDO
!U      DO I = 1, N
!U        CALL WGridPutCellReal(IDF_CPL_grid,2,I,flav(i),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_CPL_grid,3,I,vm(i),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_CPL_grid,4,I,dxvav(i),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_CPL_grid,5,I,xvsig(i),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_CPL_grid,6,I,lb(i),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_CPL_grid,7,I,ub(i),'(F12.5)')
!U      ENDDO

      END SUBROUTINE SA_OUTPUT
!
!*****************************************************************************
!
