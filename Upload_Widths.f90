!
!*****************************************************************************
!
      SUBROUTINE Upload_Widths()

      USE WINTERACTER
      USE DRUID_HEADER 

      INCLUDE 'PARAMS.INC'

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
      IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
      CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
      PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
      PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
      PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      REAL FitPar(MPkDes),FitEsd(MPkDes)
      INTEGER IOrdTem(MAX_NPFR)

!C>> JCC This is for testing for mathematical errors used to PAUSE the program: The pause seemed to
!C>> be causing a repeated CMD window to appear on screen ....
      INTEGER IBMBER
      COMMON / CCSLER / IBMBER 

      CALL SORT_REAL(PkPosAv,IOrdTem,NumPeakFitRange)
      CALL PushActiveWindowID
! Write out sigmas
      CALL WDialogSelect(IDD_Sigma_info)
      CALL WDialogClearField(IDD_Sigma_Grid)
      CALL WGridRows(IDF_Sigma_Grid,NumPeakFitRange)
      IF (NumPeakFitRange .GT. 0) THEN
        DO I = 1, NumPeakFitRange
          iord = IOrdTem(I)
          CALL WGridPutCellReal(IDF_Sigma_Grid,1,I,PkPosAv(iord),'(F12.3)')
          CALL WGridPutCellReal(IDF_Sigma_Grid,2,I,PkFnVal(1,iord),'(F12.5)')
          CALL WGridPutCellReal(IDF_Sigma_Grid,3,I,PkFnEsd(1,iord),'(F12.5)')
        END DO
        IF (NumPeakFitRange .GE. 3) THEN
! Let's fit Sigma
          NumSigmaPar = 2
          CALL Fit_Sigma(FitPar,FitEsd,NumSigmaPar)
          PkFnVarVal(1,1) = ABS(FitPar(1))
          PkFnVarVal(2,1) = ABS(FitPar(2))
          PkFnVarEsd(1,1) = FitEsd(1)
          PkFnVarEsd(2,1) = FitEsd(2)
          CALL WDialogPutReal(IDF_Sigma1,PkFnVarVal(1,1),'(F10.4)')
          CALL WDialogPutReal(IDF_Sigma2,PkFnVarVal(2,1),'(F10.4)')
          DO I = 1, NumPeakFitRange
            iord = IOrdTem(I)
            CALL WGridPutCellReal(IDF_Sigma_Grid,4,i,PkFnCal(1,iord),'(F12.5)')
          END DO
        END IF
      ELSE
        DO I = 1, 4
          CALL WGridClearCell(IDF_Sigma_Grid,I,1)
        END DO
      END IF
! Write out gammas
      CALL WDialogSelect(IDD_Gamma_info)
      CALL WDialogClearField(IDD_Gamma_Grid)
      CALL WGridRows(IDF_Gamma_Grid,NumPeakFitRange)
      IF (NumPeakFitRange .GT. 0) THEN
        DO I = 1, NumPeakFitRange
          iord = IOrdTem(I)
          CALL WGridPutCellReal(IDF_Gamma_Grid,1,I,PkPosAv(iord),'(F12.3)')
          CALL WGridPutCellReal(IDF_Gamma_Grid,2,I,PkFnVal(2,iord),'(F12.5)')
          CALL WGridPutCellReal(IDF_Gamma_Grid,3,I,PkFnEsd(2,iord),'(F12.5)')
        END DO
        IF (NumPeakFitRange .GE. 3) THEN
! Let's fit Gamma
          NumGammaPar = 2
          CALL Fit_Gamma(FitPar,FitEsd,NumGammaPar)
          PkFnVarVal(1,2) = FitPar(1)
          PkFnVarVal(2,2) = FitPar(2)
          PkFnVarEsd(1,2) = FitEsd(1)
          PkFnVarEsd(2,2) = FitEsd(2)
          CALL WDialogPutReal(IDF_Gamma1,PkFnVarVal(1,2),'(F10.4)')
          CALL WDialogPutReal(IDF_Gamma2,PkFnVarVal(2,2),'(F10.4)')
          DO I = 1, NumPeakFitRange
            iord = IOrdTem(i)
            CALL WGridPutCellReal(IDF_Gamma_Grid,4,I,PkFnCal(2,iord),'(F12.5)')
          END DO
        END IF
      ELSE
        DO I = 1, 4
          CALL WGridClearCell(IDF_Gamma_Grid,I,1)
        END DO
      END IF
! Write out HPSL
      CALL WDialogSelect(IDD_HPSL_info)
      CALL WDialogClearField(IDD_HPSL_Grid)
      CALL WGridRows(IDF_HPSL_Grid,NumPeakFitRange)
      IF (NumPeakFitRange .GT. 0) THEN
        DO I = 1, NumPeakFitRange
          iord = IOrdTem(I)
          CALL WGridPutCellReal(IDF_HPSL_Grid,1,I,PkPosAv(iord),'(F12.3)')
          CALL WGridPutCellReal(IDF_HPSL_Grid,2,I,PkFnVal(3,iord),'(F12.5)')
          CALL WGridPutCellReal(IDF_HPSL_Grid,3,I,PkFnEsd(3,iord),'(F12.5)')
        END DO
        IF (NumPeakFitRange .GE. 2) THEN
! Let's fit HPSL
          NumHPSLPar = 1
          IPtPS =3 
          IBMBER = 0
          CALL Fit_Constant(FitPar,FitEsd,NumHPSLPar,IPtPS)
!C>> JCC If you have two ranges selected, but only no peaks fitted, this goes wrong
!C>> So trap for a numerical error
          IF (IBMBER .EQ. 1) THEN
            IBMBER = 0
            CALL PopActiveWindowID
            RETURN
          ENDIF
          PkFnVarVal(1,3) = MAX(0.0002,FitPar(1))
          PkFnVarEsd(1,3) = FitEsd(1)
          CALL WDialogPutReal(IDF_HPSL1,PkFnVarVal(1,3),'(F10.4)')
          DO I = 1, NumPeakFitRange
            iord = IOrdTem(I)
            CALL WGridPutCellReal(IDF_HPSL_Grid,4,i,PkFnCal(3,iord),'(F12.5)')
          END DO
        END IF
      ELSE
        DO I = 1, 4
          CALL WGridClearCell(IDF_HPSL_Grid,I,1)
        END DO
      END IF
! Write out HMSL
      CALL WDialogSelect(IDD_HMSL_info)
      CALL WDialogClearField(IDD_HMSL_Grid)
      CALL WGridRows(IDF_HMSL_Grid,NumPeakFitRange)
      IF (NumPeakFitRange .GT. 0) THEN
        DO I = 1, NumPeakFitRange
          iord = IOrdTem(I)
          CALL WGridPutCellReal(IDF_HMSL_Grid,1,i,PkPosAv(iord),'(F12.3)')
          CALL WGridPutCellReal(IDF_HMSL_Grid,2,i,PkFnVal(4,iord),'(F12.5)')
          CALL WGridPutCellReal(IDF_HMSL_Grid,3,i,PkFnEsd(4,iord),'(F12.5)')
        END DO
! Let's fit HMSL
        IF (NumPeakFitRange.ge.2) THEN
          NumHMSLPar = 1
          IPtPS = 4
          IBMBER = 0
          CALL Fit_Constant(FitPar,FitEsd,NumHMSLPar,IPtPS)
!C>> JCC If you have two ranges selected, but only no peaks fitted, this goes wrong
!C>> So trap for a numerical error
          IF (IBMBER .EQ. 1) THEN
            IBMBER = 0
            CALL PopActiveWindowID
            RETURN
          ENDIF
          PkFnVarVal(1,4) = MAX(0.0001,FitPar(1))
          PkFnVarEsd(1,4) = FitEsd(1)
          CALL WDialogPutReal(IDF_HMSL1,PkFnVarVal(1,4),'(F10.4)')
          DO I = 1, NumPeakFitRange
            iord = IOrdTem(I)
            CALL WGridPutCellReal(IDF_HMSL_Grid,4,I,PkFnCal(4,iord),'(F12.5)')
          END DO
        END IF
      ELSE
        DO I = 1, 4
          CALL WGridClearCell(IDF_HMSL_Grid,I,1)
        END DO
      END IF
!.. Warn if HPSL is less than HMSL
      IF (NumPeakFitRange .GE. 2) THEN
        IF (PkFnVarVal(1,4) .GT. PkFnVarVal(1,3)) THEN
!          CALL WMessageBox(YesNo,ExclamationIcon,CommonYes , &
!         'HMSL is greater than HPSL.'//CHAR(13)//&
!         'Check for bad values in peak width list.'//CHAR(13)// &
!         'An expediency: do you wish to swap values?', &
!         'Unphysical axial divergence parameters')
!         IF (WInfoDialog(4) .EQ. 1) THEN
           ptem3 = PkFnVarVal(1,3)
           ptem4 = PkFnVarVal(1,4)
           PkFnVarVal(1,3) = ptem4
           PkFnVarVal(1,4) = ptem3
!         END IF
        END IF
      END IF
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Widths
!
!*****************************************************************************
!
      SUBROUTINE Fit_Sigma(X,DX,N)

      REAL Chisq_Sigma
      EXTERNAL Chisq_Sigma

      INCLUDE 'PARAMS.INC'

      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL X(MPAR),DX(MPAR),COV(MMPAR)

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
      IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
      CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
      PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
      PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
      PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

!.. Observations
      NVAL=NumPeakFitRange
      DO I = 1, NVal
        XVal(I)=PkPosAv(I)
        YVal(I)=PkFnVal(1,I)
        Emin=0.01*ABS(YVal(I))
        EVal(I)=MAX(Emin,PkFnEsd(1,I))
      END DO
!.. Variables
      N = 2
      X(1) = 0.01
      DX(1) = 0.01
      X(2) = 0.01
      DX(2) = 0.01
      CALL SIMOPT(X,DX,COV,N,Chisq_Sigma)
      DO I = 1, N
        II=I+(I-1)*N
        DX(I)=SQRT(AMAX1(0.,COV(II)))
      END DO
      DO I = 1, NVal
        PkFnCal(1,I) = ZVal(I)
      END DO

      END SUBROUTINE Fit_Sigma
!
!*****************************************************************************
!
      SUBROUTINE Fit_Gamma(X,DX,N)

      REAL Chisq_Gamma
      EXTERNAL Chisq_Gamma

      INCLUDE 'PARAMS.INC'

      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL X(MPAR),DX(MPAR),COV(MMPAR)

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
      IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
      CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
      PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
      PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
      PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

!.. Observations
      NVAL=NumPeakFitRange
      DO I=1,NVal
        XVal(I)=PkPosAv(I)
        YVal(I)=PkFnVal(2,I)
        Emin=ABS(0.01*YVal(I))
        EVal(I)=MAX(Emin,PkFnEsd(2,I))
      END DO
!.. Variables
      N=2
      X(1)=0.01
      DX(1)=0.01
      X(2)=0.01
      DX(2)=0.01
      CALL SIMOPT(X,DX,COV,N,Chisq_Gamma)
      DO I=1,N
        II=I+(I-1)*N
        DX(I)=SQRT(AMAX1(0.,COV(II)))
      END DO
      Do I=1,NVal
        PkFnCal(2,I)=ZVal(I)
      END DO

      END SUBROUTINE Fit_Gamma
!
!*****************************************************************************
!
      SUBROUTINE Fit_Constant(X,DX,N,IPtPS)

      REAL Chisq_Constant
      EXTERNAL Chisq_Constant

      INCLUDE 'PARAMS.INC'

      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL X(MPAR),DX(MPAR),COV(MMPAR)

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
      IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
      CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
      PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
      PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
      PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

!C>> JCC This is for testing for mathematical errors used to PAUSE the program: THe pause seemed to#
!C>> be causing a repeated CMD window to appear on screen ....
      INTEGER IBMBER
      COMMON / CCSLER / IBMBER 

!.. Observations
      NVAL=NumPeakFitRange
      Do I=1,NVal
        XVal(I)=PkPosAv(I)
        YVal(I)=PkFnVal(IPtPS,I)
        Emin=0.01*abs(YVal(i))
        EVal(I)=max(Emin,PkFnEsd(IPtPS,I))
      End Do
!.. Variables
      N=1
      X(1)=0.01
      DX(1)=0.01
      CALL SIMOPT(X,DX,COV,N,Chisq_Constant)
        IF (IBMBER .EQ. 1) RETURN
      DO I=1,N
        II=I+(I-1)*N
        DX(I)=SQRT(AMAX1(0.,COV(II)))
      END DO
      Do I=1,NVal
        PkFnCal(IPtPS,i)=ZVal(i)
      END DO

      END SUBROUTINE Fit_Constant
!
!*****************************************************************************
!
      FUNCTION Chisq_Sigma(N,P)

      PARAMETER (MPAR=50)
      REAL Chisq_Sigma,P(MPAR)
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      Chisq_Sigma = 0.0
      DO I = 1, NVAL
        XI = XVAL(I)
        halfxi = 0.5*xi
        secth = 1.0 / COSD(halfxi)
        tanth = TAND(halfxi)
        ZI = SQRT(MAX(0.,(P(1)*secth)**2+(P(2)*tanth)**2))
        zval(i) = zi
        CTEM = (ZI-YVAL(I))/EVAL(I)
        Chisq_Sigma = Chisq_Sigma + CTEM*CTEM
      END DO
      RETURN

      END FUNCTION Chisq_Sigma
!
!*****************************************************************************
!
      FUNCTION Chisq_Gamma(N,P)

      PARAMETER (MPAR=50)
      REAL Chisq_Gamma,P(MPAR)
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      Chisq_Gamma = 0.0
      DO I=1,NVAL
        XI=XVAL(I)
        halfxi=0.5*xi
        secth=1./cosd(halfxi)
        tanth=tand(halfxi)
        ZI=P(1)*secth+P(2)*tanth
        zval(i)=zi
        CTEM=(ZI-YVAL(I))/EVAL(I)
        Chisq_Gamma=Chisq_Gamma+CTEM*CTEM
      END DO
      RETURN

      END FUNCTION Chisq_Gamma
!
!*****************************************************************************
!
      FUNCTION Chisq_Constant(N,P)

      PARAMETER (MPAR=50)
      REAL Chisq_Constant,P(MPAR)
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      Chisq_Constant = 0.0
      DO I=1,NVAL
        XI=XVAL(I)
        ZI=P(1)
        zval(i)=zi
        CTEM=(ZI-YVAL(I))/EVAL(I)
        Chisq_Constant=Chisq_Constant+CTEM*CTEM
      END DO
      RETURN

      END FUNCTION Chisq_Constant
!
!*****************************************************************************
!
