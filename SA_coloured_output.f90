      subroutine SA_OUTPUT(kopt,T,CHIMIN,CHIAV,CHIESD,&
     xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,LNOBDS,NNEW,&
     nmpert,ntotmov,iteration)
!
!
      USE WINTERACTER
      USE DRUID_HEADER
!
	real*8 xopt(*),dxvav(*),xvsig(*),flav(*),lb(*),ub(*),vm(*)
!
!
      parameter (maxiter=10000)
      common /pltstore/ xiter(maxiter),tstore(maxiter),&
      foptstore(maxiter),fpavstore(maxiter)
      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)
      character*80 temperfile
!
      CALL WDialogSelect(IDD_SA_Action1)
      CALL WDialogPutReal(IDF_curr_temp,T,'(f8.2)')
      CALL WDialogPutReal(IDF_min_chisq,chimin,'(f8.2)')
      CALL WDialogPutReal(IDF_profile_chisq2,CHIPROBEST(iteration),'(f8.2)')
!
      CALL IGrFillPattern(Solid)
      temperfile='temperature1.bmp'
!
!.. best chi-squared scale ...
        CALL IGrSelect(3,IDF_minchisq_picture)
        tenow=alog10(chimin)
        temin=0.0
        temax=4.0
        CALL IGrUnits(temin,0.0,temax,1.0)
        CALL IGrLoadImage(temperfile)
        CALL IGrColourN(223)
        CALL IGrRectangle(0.975,0.0,1.025,1.0)
        CALL IGrRectangle(1.975,0.0,2.025,1.0)
        CALL IGrRectangle(2.975,0.0,3.025,1.0)
        CALL IGrColourN(52)
        tenow1=tenow-0.05
        tenow2=tenow+0.05
        CALL IGrRectangle(tenow1,0.0,tenow2,1.0)
!	write(56,*) ' best chi ',temin,tenow,temax
!.. profile chi-squared scale ...
        CALL IGrSelect(3,IDF_prochisq_picture)
        tenow=alog10(CHIPROBEST(iteration))
        temin=0.0
        temax=4.0
        CALL IGrUnits(temin,0.0,temax,1.0)
        CALL IGrLoadImage(temperfile)
        CALL IGrColourN(223)
        CALL IGrRectangle(0.975,0.0,1.025,1.0)
        CALL IGrRectangle(1.975,0.0,2.025,1.0)
        CALL IGrRectangle(2.975,0.0,3.025,1.0)
        CALL IGrColourN(52)
        tenow1=tenow-0.05
        tenow2=tenow+0.05
        CALL IGrRectangle(tenow1,0.0,tenow2,1.0)
!
      if (kopt.eq.1) then
       CALL WDialogPutReal(IDF_av_chisq,chiav,'(f8.2)')
       CALL WDialogPutReal(IDF_rms_chisq,chiesd,'(f8.2)')
       CALL WDialogPutInteger(IDF_total_moves,ntotmov)
       CALL WDialogPutInteger(IDF_moves_per_T,nmpert)
       CALL WDialogPutInteger(IDF_downhill_moves,ndown)
       CALL WDialogPutInteger(IDF_rej_up_moves,nrej)
       CALL WDialogPutInteger(IDF_acc_up_moves,nup)
       CALL WDialogPutInteger(IDF_SA_total_moves_label,nmpert)
!.. Temperature scale ...
        CALL IGrSelect(3,IDF_T_picture)
        tenow=alog10(t)
        temin=0.0
        temax=4.0
        CALL IGrUnits(temin,0.0,temax,1.0)
        CALL IGrLoadImage(temperfile)
        CALL IGrColourN(223)
        CALL IGrRectangle(0.975,0.0,1.025,1.0)
        CALL IGrRectangle(1.975,0.0,2.025,1.0)
        CALL IGrRectangle(2.975,0.0,3.025,1.0)
        CALL IGrColourN(52)
        tenow1=tenow-0.05
        tenow2=tenow+0.05
        CALL IGrRectangle(tenow1,0.0,tenow2,1.0)
!.. average chi-squared scale ...
        CALL IGrSelect(3,IDF_avchisq_picture)
        tenow1=alog10(max(1.,chiav-chiesd))
        tenow2=alog10(max(10000.,chiav+chiesd))
        temin=0.0
        temax=4.0
        CALL IGrUnits(temin,0.0,temax,1.0)
        CALL IGrLoadImage(temperfile)
        CALL IGrColourN(223)
        CALL IGrRectangle(0.975,0.0,1.025,1.0)
        CALL IGrRectangle(1.975,0.0,2.025,1.0)
        CALL IGrRectangle(2.975,0.0,3.025,1.0)
        CALL IGrColourN(52)
        CALL IGrRectangle(tenow1,0.0,tenow2,1.0)
        CALL IGrColourN(128)
        tenow1=alog10(max(1.,chiav-0.1*chiesd))
        tenow2=alog10(min(10000.,chiav+0.1*chiesd))
        CALL IGrRectangle(tenow1,0.0,tenow2,1.0)
        CALL IGrColourN(52)
!	write(56,*) ' av chi ',temin,tenow1,tenow2,temax
!
        CALL IGrSelect(3,IDF_SA_move_distribution)
        ruler=nmpert
        CALL IGrUnits(0.0,0.0,ruler,1.0)
        CALL IGrColourN(95)
        rulex1=0.0
        rulex2=ndown
        CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
        CALL IGrColourN(159)
        rulex1=rulex2
        rulex2=rulex2+nup
        CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
        CALL IGrColourN(31)
        rulex1=rulex2
        rulex2=nmpert
        CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
! total moves
        CALL IGrSelect(3,IDF_SATotalMoves_picture)
        iemax=1+alog10(float(ntotmov))
        ruler=10**iemax
        CALL IGrUnits(0.0,0.0,ruler,1.0)
        CALL IGrColourN(52)
        rulex1=0.0
        rulex2=ntotmov
        CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
        CALL IGrColourN(159)
        rulex1=rulex2
        rulex2=ruler
        CALL IGrRectangle(rulex1,0.0,rulex2,1.0)
!	write(56,*) ' NTotMov ',ntotmov,ruler,IDF_SATotalMoves_picture
      end if
!
      CALL IGrSelect(1,0)
!
      CALL WDialogSelect(IDD_Parameter_Status)
      do i=1,n
        CALL WGridPutCellReal(IDF_CPL_grid,1,i,sngl(xopt(i)),'(f12.5)')
      end do
      if (kopt.eq.1) then
       do i=1,n
        CALL WGridPutCellReal(IDF_CPL_grid,2,i,sngl(flav(i)),'(f12.5)')
        CALL WGridPutCellReal(IDF_CPL_grid,3,i,sngl(vm(i)),'(f12.5)')
        CALL WGridPutCellReal(IDF_CPL_grid,4,i,sngl(dxvav(i)),'(f12.5)')
        CALL WGridPutCellReal(IDF_CPL_grid,5,i,sngl(xvsig(i)),'(f12.5)')
        CALL WGridPutCellReal(IDF_CPL_grid,6,i,sngl(lb(i)),'(f12.5)')
        CALL WGridPutCellReal(IDF_CPL_grid,7,i,sngl(ub(i)),'(f12.5)')
       end do
      end if
!
      END