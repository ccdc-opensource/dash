      subroutine sa_upload
!
  USE WINTERACTER
  USE DRUID_HEADER
!
! Define some parameters to match those in the resource file
!
!
! Declare window-type and message variables
!
!  TYPE(WIN_STYLE)    WINDOW
!  TYPE(WIN_MESSAGE)  MESSAGE
  character*80 file
  real         rpos,eps1000
  integer      ipos,istart,iend,npar,ieps
!
      character*132 line
      character*80  sa_file
      logical   log_inf_file,log_nvar,log_bounds,log_reduce
      logical log_eps,log_ns,log_nt,log_neps,log_maxevl,log_iprint
      logical log_iseed1,log_iseed2,log_T0,log_target_value
      logical log_frag_file
      double precision cen,sig
      logical gaussb
      character*80  inf_file,zm_file
      double precision x,lb,ub,vm,xpreset
      double precision T0,rt,eps,target_value
      common /inffil/ lfinf,lfzm,inf_file,zm_file
      parameter (maxfrg=20)
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      parameter (mvar=100)
      common /gaubou/ cen(mvar),sig(mvar)
      common /gaulog/ gaussb(mvar)
      character*80  torfile
      logical ltorfil
      common /torfcm/ torfile(mvar)
      common /torlog/ ltorfil(mvar)
      common /jitter/ rjittr
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
!
      common /saparl/ T0,rt,eps,target_value
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
!
!
!
!.. check if parameters have been specified - if not use defaults.
            CALL WDialogSelect(IDD_SA_Action2)
            CALL WDialogSelect(IDD_input_parameters)
            CALL WDialogPutInteger(IDF_npar,nvar)
            CALL WGridRows(IDF_parameter_grid,nvar)
            DO i=1,nvar
              IF (LOG_PRESET) THEN
                CALL WGridPutCellReal(IDF_parameter_grid,7,i,sngl(xpreset(i)),'(F12.5)')
                CALL WGridPutCellCheckBox(IDF_parameter_grid,6,i,Checked)
              ELSE
                CALL WGridPutCellCheckBox(IDF_parameter_grid,6,i,Unchecked)
              END IF
              CALL WGridPutCellReal(IDF_parameter_grid,2,i,sngl(lb(i)),'(F12.5)')
              CALL WGridPutCellReal(IDF_parameter_grid,3,i,sngl(ub(i)),'(F12.5)')
              CALL WGridPutCellReal(IDF_parameter_grid,4,i,sngl(vm(i)),'(F12.5)')
            END DO
            CALL WDialogSelect(IDD_input_protocol)
            CALL WDialogPutReal(IDF_T0,sngl(T0),'(F7.2)')
            CALL WDialogPutTrackBar(IDF_T0_trackbar,nint(sngl(T0)))
            RT1000=sngl(RT*1000.)
            CALL WDialogPutReal(IDF_TREDRATE,RT1000,'(F7.2)')
            IRT=RT1000
            CALL WDialogPutTrackbar(IDF_Tredrate_trackbar,IRT)
            CALL WDialogPutReal(IDF_targetchisq,sngl(target_value),'(F7.2)')
            CALL WDialogPutTrackbar(IDF_targetchisq_trackbar,nint(sngl(target_value)))
            CALL WDialogPutInteger(IDF_ISEED1,iseed1)
            CALL WDialogPutInteger(IDF_ISEED2,iseed2)
            CALL WDialogPutInteger(IDF_NT,NT)
            CALL WDialogPutInteger(IDF_NS,NS)
!
! Initialise the total number of moves box
            CALL WDialogSelect(IDD_SA_Action1)
            CALL WDialogPutInteger(IDF_TOTAL_MOVES,NT*NS*NVAR)
!
      end