!
!*****************************************************************************
!
      SUBROUTINE SA_OUTPUT(kopt,T,CHIMIN,CHIAV,CHIESD,&
      xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,&
      nmpert,ntotmov,iteration)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER kopt
      REAL T, CHIMIN, CHIAV, CHIESD
      INTEGER n, NUP, NDOWN, NREJ, nmpert, ntotmov, iteration
      REAL*8 xopt(*),dxvav(*),xvsig(*),flav(*),lb(*),ub(*),vm(*)

      INCLUDE 'PARAMS.INC'

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)
      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      REAL bchmin, bpwval, bchpro, tempvl
      REAL avchi1, avchi2, avchi3, avchi4
      INTEGER nd1, nd2, nd3, nd4, bmIHANDLE
      COMMON / sagdat / bchmin, bpwval, bchpro, &
                tempvl, avchi1, avchi2, avchi3, avchi4, nd1, &
                nd2, nd3, nd4, bmIHANDLE

      REAL ctem

      CALL WDialogSelect(IDD_SA_Action1)
      CALL WDialogPutReal(IDF_curr_temp,T,'(F8.2)')
      CALL WDialogPutReal(IDF_min_chisq,chimin,'(F8.2)')
      CALL WDialogPutReal(IDF_profile_chisq2,CHIPROBEST(iteration),'(F8.2)')
! best chi-squared scale
      bchmin = ALOG10(MAX(1.0,chimin))
! profile chi-squared scale
      bpwval = ALOG10(MAX(1.0,PAWLEYCHISQ))
      bchpro = ALOG10(MAX(1.0,CHIPROBEST(iteration)))
! What is 'kopt' supposed to do?
      IF (kopt .EQ. 1) THEN
        CALL WDialogPutReal(IDF_av_chisq,chiav,'(F8.2)')
        CALL WDialogPutInteger(IDF_total_moves,ntotmov)
        CALL WDialogPutInteger(IDF_SA_total_moves_label,nmpert)
! Temperature scale
        tempvl = ALOG10(MAX(1.0,t))
        avchi1 = ALOG10(MAX(1.0,chiav-chiesd))
        ctem = MAX(1.0,chiav+chiesd)
        avchi2 = ALOG10(MIN(10000.0,ctem))
        avchi3 = ALOG10(MAX(1.0,chiav-0.1*chiesd))
        ctem = MAX(1.0,chiav+0.1*chiesd)
        avchi4 = ALOG10(MIN(10000.0,ctem))
        nd1 = ndown
        nd3 = ntotmov
        nd4 = nup
      ENDIF
      CALL Sa_Output_Gr
! Following lines write values to dialogue that hasn't been loaded into memory,
! nor is it displayed.
!U      CALL WDialogSelect(IDD_Parameter_Status)
!U      DO I = 1, n
!U        CALL WGridPutCellReal(IDF_CPL_grid,1,I,SNGL(xopt(i)),'(F12.5)')
!U      END DO
!U      IF (kopt .EQ. 1) THEN
!U        DO I = 1, n
!U          CALL WGridPutCellReal(IDF_CPL_grid,2,I,SNGL(flav(i)),'(F12.5)')
!U          CALL WGridPutCellReal(IDF_CPL_grid,3,I,SNGL(vm(i)),'(F12.5)')
!U          CALL WGridPutCellReal(IDF_CPL_grid,4,I,SNGL(dxvav(i)),'(F12.5)')
!U          CALL WGridPutCellReal(IDF_CPL_grid,5,I,SNGL(xvsig(i)),'(F12.5)')
!U          CALL WGridPutCellReal(IDF_CPL_grid,6,I,SNGL(lb(i)),'(F12.5)')
!U          CALL WGridPutCellReal(IDF_CPL_grid,7,I,SNGL(ub(i)),'(F12.5)')
!U        END DO
!U      END IF

      END SUBROUTINE SA_OUTPUT
!
!*****************************************************************************
!
      SUBROUTINE sa_output_gr()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL bchmin, bpwval, bchpro, tempvl
      REAL avchi1, avchi2, avchi3, avchi4
      INTEGER nd1, nd2, nd3, nd4, bmIHANDLE
      COMMON / sagdat / bchmin, bpwval, bchpro, &
                tempvl, avchi1, avchi2, avchi3, avchi4, nd1, &
                nd2, nd3, nd4, bmIHANDLE

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      REAL tenow1, tenow2, ruler, rulex1, rulex2
  
      REAL, PARAMETER ::  rminh = 0.01
      REAL, PARAMETER ::  rmaxh = 0.99

      CALL IGrFillPattern(Solid)
! Temperature
      CALL IGrSelect(3,IDF_T_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,0)
      CALL IGrColourN(95) ! Lightgreen
      tenow1 = tempvl-0.03
      tenow2 = tempvl+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Minimum chi squared
      CALL IGrSelect(3,IDF_minchisq_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,0)
      CALL IGrColourN(95) ! Lightgreen
      tenow1 = bchmin-0.03
      tenow2 = bchmin+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Average chi-squared
      CALL IGrSelect(3,IDF_avchisq_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,0)
      CALL IGrColourN(95) ! Lightgreen
      CALL IGrRectangle(avchi1,rminh,avchi2,rmaxh)
      CALL IGrColourN(128)
      CALL IGrRectangle(avchi3,rminh,avchi4,rmaxh)
! Profile chi squared
      CALL IGrSelect(3,IDF_prochisq_picture)
      CALL IGrUnits(0.0,0.0,4.0,1.0)
      CALL WBitmapPut(bmIHANDLE,0,0)
      CALL IGrColourN(63)
      tenow1 = bpwval-0.03
      tenow2 = bpwval+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
      CALL IGrColourN(95) ! Lightgreen
      tenow1 = bchpro-0.03
      tenow2 = bchpro+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Total moves
      CALL IGrSelect(3,IDF_SATotalMoves_picture)
      IF (RESTART) THEN
        ruler = FLOAT(MaxMoves)
      ELSE
        ruler = MaxIter * nd2
      ENDIF
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(59)  ! Yellow
      rulex1 = 0.0
      rulex2 = FLOAT(nd3)
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159) ! Blue
      rulex1 = rulex2
      rulex2 = ruler
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
! Uphill, downhill, rejected
      CALL IGrSelect(3,IDF_SA_move_distribution)
      ruler = FLOAT(nd2)
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(95) ! Lightgreen
      rulex1 = 0.0
      rulex2 = FLOAT(nd1)
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159) ! Blue
      rulex1 = rulex2
      rulex2 = rulex2 + FLOAT(nd4)
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(31) ! Red
      rulex1 = rulex2
      rulex2 = ruler
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrSelect(1,0)
      CALL IGrUnits(0.0,0.0,1.0,1.0)
      CALL IGrArea(0.0,0.0,1.0,1.0)

      END SUBROUTINE sa_output_gr
!
!*****************************************************************************
!
