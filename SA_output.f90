!
!*****************************************************************************
!
      SUBROUTINE SA_OUTPUT(kopt,T,CHIMIN,CHIAV,CHIESD,&
      xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,&
      nmpert,ntotmov,iteration)

      USE WINTERACTER
      USE DRUID_HEADER

      REAL*8 xopt(*),dxvav(*),xvsig(*),flav(*),lb(*),ub(*),vm(*)

      INCLUDE 'PARAMS.INC'

      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)
      COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP

      REAL temin,temax, bchmin, bpwval, bchpro, tempvl
      REAL avchi1, avchi2, avchi3, avchi4
      INTEGER nd1, nd2, nd3, nd4
      COMMON / sagdat / temin, temax, bchmin, bpwval, bchpro, &
                tempvl, avchi1, avchi2, avchi3, avchi4, nd1, &
                nd2, nd3, nd4

      CALL WDialogSelect(IDD_SA_Action1)
      CALL WDialogPutReal(IDF_curr_temp,T,'(F8.2)')
      CALL WDialogPutReal(IDF_min_chisq,chimin,'(F8.2)')
      CALL WDialogPutReal(IDF_profile_chisq2,CHIPROBEST(iteration),'(F8.2)')

!.. best chi-squared scale ...
      bchmin=alog10(max(1.0,chimin))
      temin = 0.0
      temax = 4.0
 
!.. profile chi-squared scale ...
  
      bpwval = alog10(MAX(1.0,PawleyChiSq))
      bchpro = alog10(MAX(1.0,CHIPROBEST(iteration)))
      IF (kopt .EQ. 1) THEN
        CALL WDialogPutReal(IDF_av_chisq,chiav,'(F8.2)')
        CALL WDialogPutReal(IDF_rms_chisq,chiesd,'(F8.2)')
        CALL WDialogPutInteger(IDF_total_moves,ntotmov)
        CALL WDialogPutInteger(IDF_moves_per_T,nmpert)
        CALL WDialogPutInteger(IDF_downhill_moves,ndown)
        CALL WDialogPutInteger(IDF_rej_up_moves,nrej)
        CALL WDialogPutInteger(IDF_acc_up_moves,nup)
        CALL WDialogPutInteger(IDF_SA_total_moves_label,nmpert)
!.. Temperature scale ...
        tempvl = alog10(MAX(1.0,t))
        avchi1 = alog10(MAX(1.0,chiav-chiesd))
        ctem = MAX(1.0,chiav+chiesd)
        avchi2 = alog10(MIN(10000.0,ctem))
        temin = 0.0
        temax = 4.0
        avchi3 = alog10(MAX(1.0,chiav-0.1*chiesd))
        ctem = MAX(1.0,chiav+0.1*chiesd)
        avchi4 = alog10(MIN(10000.0,ctem))
        nd1 = ndown
        nd2 = nmpert
        nd3 = ntotmov
        nd4 = nup
      END IF
      CALL Sa_Output_Gr
      CALL WDialogSelect(IDD_Parameter_Status)
      DO I = 1, n
        CALL WGridPutCellReal(IDF_CPL_grid,1,I,SNGL(xopt(i)),'(F12.5)')
      END DO
      IF (kopt .EQ. 1) THEN
        DO I = 1, n
          CALL WGridPutCellReal(IDF_CPL_grid,2,I,SNGL(flav(i)),'(F12.5)')
          CALL WGridPutCellReal(IDF_CPL_grid,3,I,SNGL(vm(i)),'(F12.5)')
          CALL WGridPutCellReal(IDF_CPL_grid,4,I,SNGL(dxvav(i)),'(F12.5)')
          CALL WGridPutCellReal(IDF_CPL_grid,5,I,SNGL(xvsig(i)),'(F12.5)')
          CALL WGridPutCellReal(IDF_CPL_grid,6,I,SNGL(lb(i)),'(F12.5)')
          CALL WGridPutCellReal(IDF_CPL_grid,7,I,SNGL(ub(i)),'(F12.5)')
        END DO
      END IF

      END SUBROUTINE SA_OUTPUT
!
!*****************************************************************************
!
      SUBROUTINE sa_output_gr()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL temin,temax, bchmin, bpwval, bchpro, tempvl
      REAL avchi1, avchi2, avchi3, avchi4
      INTEGER nd1, nd2, nd3, nd4
      COMMON / sagdat / temin, temax, bchmin, bpwval, bchpro, &
                tempvl, avchi1, avchi2, avchi3, avchi4, nd1, &
                nd2, nd3, nd4
      LOGICAL RESTART
      INTEGER SA_Run_Number
      INTEGER MaxRuns, MinMoves, MaxMoves
      REAL    ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult
      CHARACTER*255 temperfile
      REAL tenow1, tenow2, ruler, rulex1, rulex2
      INTEGER iemax, ilt
  
      REAL, PARAMETER ::  rminh = 0.01
      REAL, PARAMETER ::  rmaxh = 0.99

      temperfile = INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'Images'//DIRSPACER//'temperature1.bmp'
      ilt = LEN_TRIM(temperfile)
! Temperature
      CALL IGrSelect(3,IDF_T_picture)
      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))


      CALL IGrColourN(95) ! Lightgreen
      tenow1=tempvl-0.03
      tenow2=tempvl+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
! Minimum chi squared
      CALL IGrSelect(3,IDF_minchisq_picture)
      CALL IGrFillPattern(Solid)
      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))

      CALL IGrColourN(95) ! Lightgreen
      tenow1=bchmin-0.03
      tenow2=bchmin+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
!.. average chi-squared scale ...
      CALL IGrSelect(3,IDF_avchisq_picture)

      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))
      CALL IGrColourN(95) ! Lightgreen
      CALL IGrRectangle(avchi1,rminh,avchi2,rmaxh)
      CALL IGrColourN(128)
      CALL IGrRectangle(avchi3,rminh,avchi4,rmaxh)
      CALL IGrColourN(95) ! Lightgreen
! Profile chi squared
      CALL IGrSelect(3,IDF_prochisq_picture)
      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))
      CALL IGrColourN(63)
      tenow1=bpwval-0.03
      tenow2=bpwval+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
      CALL IGrColourN(95) ! Lightgreen

      tenow1=bchpro-0.03
      tenow2=bchpro+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)

! total moves
      CALL IGrSelect(3,IDF_SATotalMoves_picture)
!O      iemax=1+alog10(MAX(1.,FLOAT(nd3)))
!O      ruler=10.**iemax
      ruler = MaxMoves
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(59)  ! Yellow
      rulex1=0.0
      rulex2=nd3
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159) ! Blue
      rulex1=rulex2
      rulex2=ruler
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
! Uphill, downhill, rejected
      CALL IGrSelect(3,IDF_SA_move_distribution)
      ruler=nd2
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(95) ! Lightgreen
      rulex1=0.0
      rulex2=nd1
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159) ! Blue
      rulex1=rulex2
      rulex2=rulex2+nd4
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(31) ! Red
      rulex1=rulex2
      rulex2=nd2
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)

      CALL IGrSelect(1,0)
      CALL IGrUnits(0.,0.,1.,1.)
      CALL IGrArea(0.,0.,1.,1.)
      RETURN

      END SUBROUTINE sa_output_gr
!
!*****************************************************************************
!
