      SUBROUTINE sa_upload
!U!
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U!
!U! Define some parameters to match those in the resource file
!U!
!U      double precision cen,sig
!U      logical gaussb
!U      character*80  inf_file,zm_file
!U      double precision x,lb,ub,vm,xpreset
!U      double precision T0,rt,eps,target_value
!U      common /inffil/ lfinf,lfzm,inf_file,zm_file
!U      parameter (maxfrg=20)
!U      common /frgcom/ nfrag,lfrag(maxfrg)
!U      character*80 frag_file
!U      common /frgcha/ frag_file(maxfrg)
!U      parameter (mvar=100)
!U      common /gaubou/ cen(mvar),sig(mvar)
!U      common /gaulog/ gaussb(mvar)
!U      character*80  torfile
!U      logical ltorfil
!U      common /torfcm/ torfile(mvar)
!U      common /torlog/ ltorfil(mvar)
!U      common /jitter/ rjittr
!U      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
!U      common /presetr/ xpreset(mvar)
!U      logical log_preset
!U      common /presetl/ log_preset
!U      common /saparl/ T0,rt,eps,target_value
!U      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
!U      common /shadl/ log_shad(mvar)
!U      common /shadi/ kshad(mvar)
!U!
!U!.. check if parameters have been specified - if not use defaults.
!U      CALL WDialogSelect(IDD_SA_Action2)
!U      CALL WDialogSelect(IDD_input_parameters)
!U      CALL WDialogPutInteger(IDF_npar,nvar)
!U      CALL WGridRows(IDF_parameter_grid,nvar)
!U      DO i=1,nvar
!U        IF (LOG_PRESET) THEN
!U          CALL WGridPutCellReal(IDF_parameter_grid,7,i,sngl(xpreset(i)),'(F12.5)')
!U          CALL WGridPutCellCheckBox(IDF_parameter_grid,6,i,Checked)
!U        ELSE
!U          CALL WGridPutCellCheckBox(IDF_parameter_grid,6,i,Unchecked)
!U        END IF
!U        CALL WGridPutCellReal(IDF_parameter_grid,2,i,sngl(lb(i)),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_parameter_grid,3,i,sngl(ub(i)),'(F12.5)')
!U        CALL WGridPutCellReal(IDF_parameter_grid,4,i,sngl(vm(i)),'(F12.5)')
!U      END DO
!U      CALL WDialogSelect(IDD_input_protocol)
!U      CALL WDialogPutReal(IDF_T0,sngl(T0),'(F7.2)')
!U      CALL WDialogPutTrackBar(IDF_T0_trackbar,nint(sngl(T0)))
!U      RT1000=sngl(RT*1000.)
!U      CALL WDialogPutReal(IDF_TREDRATE,RT1000,'(F7.2)')
!U      IRT=RT1000
!U      CALL WDialogPutTrackbar(IDF_Tredrate_trackbar,IRT)
!U      CALL WDialogPutReal(IDF_targetchisq,sngl(target_value),'(F7.2)')
!U      CALL WDialogPutTrackbar(IDF_targetchisq_trackbar,nint(sngl(target_value)))
!U      CALL WDialogPutInteger(IDF_ISEED1,iseed1)
!U      CALL WDialogPutInteger(IDF_ISEED2,iseed2)
!U! Jvds @ Where's number 3?
!U      CALL WDialogPutInteger(IDF_NT,NT)
!U      CALL WDialogPutInteger(IDF_NS,NS)
!U!
!U! Initialise the total number of moves box
!U      CALL WDialogSelect(IDD_SA_Action1)
!U      CALL WDialogPutInteger(IDF_TOTAL_MOVES,NT*NS*NVAR)
!U!
      END SUBROUTINE sa_upload
