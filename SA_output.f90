     subroutine SA_OUTPUT(kopt,T,CHIMIN,CHIAV,CHIESD,&
     xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,LNOBDS,NNEW,&
     nmpert,ntotmov,iteration)
!
!
      USE WINTERACTER
      USE DRUID_HEADER
      COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP
!
	real*8 xopt(*),dxvav(*),xvsig(*),flav(*),lb(*),ub(*),vm(*)
!
!
      parameter (maxiter=10000)
      common /pltstore/ xiter(maxiter),tstore(maxiter),&
      foptstore(maxiter),fpavstore(maxiter)
      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)


	  REAL temin,temax, bchmin, bpwval, bchpro, tempvl
	  REAL avchi1, avchi2, avchi3, avchi4
	  INTEGER nd1, nd2, nd3, nd4
	  COMMON / sagdat / temin, temax, bchmin, bpwval, bchpro, &
	          tempvl, avchi1, avchi2, avchi3, avchi4, nd1, &
	          nd2, nd3, nd4

	  CALL WDialogSelect(IDD_SA_Completed)
      CALL WDialogPutReal(IDF_SA_Complete_pcs,CHIPROBEST(iteration),'(f8.2)')
      CALL WDialogPutReal(IDF_SA_Complete_ics,chimin,'(f8.2)')

      CALL WDialogSelect(IDD_SA_Action1)
      CALL WDialogPutReal(IDF_curr_temp,T,'(f8.2)')
      CALL WDialogPutReal(IDF_min_chisq,chimin,'(f8.2)')
      CALL WDialogPutReal(IDF_profile_chisq2,CHIPROBEST(iteration),'(f8.2)')

!


	

 
!
!.. best chi-squared scale ...
        bchmin=alog10(max(1.,chimin))
        temin=0.0
        temax=4.0
 
!.. profile chi-squared scale ...
  
        bpwval =alog10(max(1.,PawleyChiSq))


        bchpro=alog10(max(1.,CHIPROBEST(iteration)))

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

       tempvl=alog10(max(1.,t))
       avchi1=alog10(max(1.,chiav-chiesd))
       ctem=max(1.,chiav+chiesd)
       avchi2=alog10(min(10000.,ctem))
       temin=0.0
       temax=4.0
       avchi3=alog10(max(1.,chiav-0.1*chiesd))
       ctem=max(1.,chiav+0.1*chiesd)
       avchi4=alog10(min(10000.,ctem))
	   nd1 = ndown
	   nd2 = nmpert
	   nd3 = ntotmov
	   nd4 = nup
      end if
!
	  CALL Sa_Output_Gr

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

      END



	  subroutine sa_output_gr()
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
      character*255 temperfile
	  REAL tenow1, tenow2, ruler, rulex1, rulex2
	  integer iemax, ilt
  
      REAL, PARAMETER ::  rminh = 0.01
	  REAL, PARAMETER ::  rmaxh = 0.99


      temperfile= INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//'Images'//DIRSPACER//'temperature1.bmp'
	  ilt = len_trim(temperfile)


	  CALL IGrSelect(3,IDF_minchisq_picture)
	  CALL IGrFillPattern(Solid)
      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))

      CALL IGrColourN(95)
      tenow1=bchmin-0.03
      tenow2=bchmin+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)


      CALL IGrSelect(3,IDF_prochisq_picture)
      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))
      CALL IGrColourN(63)
      tenow1=bpwval-0.03
      tenow2=bpwval+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
      CALL IGrColourN(95)

	  tenow1=bchpro-0.03
      tenow2=bchpro+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)


      CALL IGrSelect(3,IDF_T_picture)
      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))


      CALL IGrColourN(95)
      tenow1=tempvl-0.03
      tenow2=tempvl+0.03
      CALL IGrRectangle(tenow1,rminh,tenow2,rmaxh)
!.. average chi-squared scale ...
      CALL IGrSelect(3,IDF_avchisq_picture)

      CALL IGrUnits(temin,0.0,temax,1.0)
      CALL IGrLoadImage(temperfile(1:ilt))
      CALL IGrColourN(95)
      CALL IGrRectangle(avchi1,rminh,avchi2,rmaxh)
      CALL IGrColourN(128)
      CALL IGrRectangle(avchi3,rminh,avchi4,rmaxh)
      CALL IGrColourN(95)

      CALL IGrSelect(3,IDF_SA_move_distribution)
      ruler=nd2
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(95)
      rulex1=0.0
      rulex2=nd1
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159)
      rulex1=rulex2
      rulex2=rulex2+nd4
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(31)
      rulex1=rulex2
      rulex2=nd2
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
! total moves
      CALL IGrSelect(3,IDF_SATotalMoves_picture)
      iemax=1+alog10(max(1.,float(nd3)))
      ruler=10.**iemax
      CALL IGrUnits(0.0,0.0,ruler,1.0)
      CALL IGrColourN(52)
      rulex1=0.0
      rulex2=nd3
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)
      CALL IGrColourN(159)
      rulex1=rulex2
      rulex2=ruler
      CALL IGrRectangle(rulex1,rminh,rulex2,rmaxh)

      CALL IGrSelect(1,0)
	  CALL IGrUnits(0.,0.,1.,1.)
	  CALL IGrArea(0.,0.,1.,1.)
!
	  return
	  end subroutine sa_output_gr
