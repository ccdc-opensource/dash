!
!*****************************************************************************
!
      SUBROUTINE SimulatedAnnealing

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE PO_VAR
      USE ZMVAR
      USE SOLVAR
      USE RRVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      REAL             XOPT,       C,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), FOPT

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      REAL          RULB
      COMMON /RULB/ RULB(Mvar)

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)

      INTEGER         NATOM
      REAL                   XATO
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, XATO(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      INTEGER         Curr_SA_Iteration
      COMMON /ITRINF/ Curr_SA_Iteration

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL            bchmin, bpwval, bchpro, avchi1, avchi2, avchi3, avchi4
      INTEGER         nd1, nmpert, nd3, nd4, bmIHANDLE
      COMMON /sagdat/ bchmin, bpwval, bchpro, avchi1, avchi2, avchi3, avchi4, &
                      nd1, nmpert, nd3, nd4, bmIHANDLE

      INTEGER              iMyExit, num_new_min
      COMMON / CMN000001 / iMyExit, num_new_min

      REAL, EXTERNAL :: EXPREP
      LOGICAL, EXTERNAL :: Get_AutoLocalMinimisation, IsEventWaiting, Get_AutoAlign
      LOGICAL, EXTERNAL :: CheckTerm, OutOfBounds
      INTEGER NACC
      INTEGER NACP(MVAR)
      LOGICAL MAKET0
      REAL FPSUM0, FPSUM1, FPSUM2, FPAV, FPSD
      REAL RATIO, DX, F, FP, P, PP
      REAL RANIN
      INTEGER NUP, NDOWN, NREJ, H, I, J, M, II
      INTEGER MRAN, MRAN1, IARR, IAR1
      REAL T
      INTEGER NumTrialsPar(MVAR), NumParPerTrial, iParNum
      REAL RANARR(30000), RANAR1(30000)
      REAL DXVAV(mvar), XVSIG(mvar), FLAV(mvar)
      REAL X0SUM(mvar), XSUM(mvar), XXSUM(mvar)
      REAL XDSS(mvar), A0SUM(mvar)
      INTEGER IM, IA, IC
      INTEGER JRAN, num_old_min, NTOTMOV
      INTEGER III, IH, KK, iFrg
      INTEGER Last_NUP, Last_NDOWN
      CHARACTER*3 CNruns,CMruns
      LOGICAL PrevRejected, CurrParsInclPO, PrevParsInclPO
      INTEGER TotNumTrials, TotNumRetrials
      CHARACTER*2 RowLabelStr
      REAL XP(MVAR)
      REAL xtem, tempupper, templower, tempupper2, templower2
      REAL Sgn
      LOGICAL Use_Sharpen
      REAL ITFRange ! For blurring when using Sharpen
      REAL Initial_T ! As calculated during intial run, not as set by the user.

      Use_Sharpen = .FALSE.
      ITFRange = 10.0 ! I.e. ITF runs from 50.0 -> 1.0
      NumParPerTrial = 1
      TotNumTrials = 0
      TotNumRetrials = 0
      Curr_SA_Run = 0
      NumOf_SA_Runs = 0
      DO I = 1, 99
        iSol2Run(I) = I
      ENDDO
! Set up a random number generator store
! Use a quick and dirty one from NR
      CALL RANX2Init
      IM = 29282
      IA = 1255
      IC = 6173
      JRAN = 1
      DO I = 1, 30000
        JRAN = MOD(JRAN*IA+IC,IM)
        RANARR(I) = FLOAT(JRAN)/FLOAT(IM) ! RANARR now between  0.0 and 1.0
        RANIN = 2.0 * RANARR(I) - 1.0     ! RANIN  now between -0.1 and 1.0
        CALL RANX2E(RANIN,RANAR1(I))
      ENDDO
      IM = 7875
      IA = 211
      IC = 1663
! vm is adjusted during the SA. So re-initialise every time the SA is started to
! ensure that starting the SA more than once with the same parameters will give
! identical results.
      kk = 0
      DO iFrg = 1, nFrag
        DO ii = 1, izmpar(iFrg)
          kk = kk + 1
          SELECT CASE (kzmpar(ii,iFrg))
            CASE (1) ! translation
              vm(kk) = 0.1
            CASE (2) ! quaternion
              vm(kk) = 0.1
            CASE (3) ! torsion
              vm(kk) = 10.0
            CASE (4) ! angle
              vm(kk) = 1.0
            CASE (5) ! bond
              vm(kk) = 0.1*(ub(kk)-lb(kk))
            CASE (6) ! single rotation axis
              vm(kk) = 0.1
          END SELECT
        ENDDO
      ENDDO
      IF (PrefParExists) THEN
        kk = kk + 1
        vm(kk) = 0.01
      ENDIF
      CALL FillRULB(nvar) !calcs upper and lower bounds for parameters
      NPAR = 0
      DO I = 1, nvar
        IF (RULB(I) .GT. 1.0E-3) THEN
! NPAR is the number of parameters that are allowed to vary (i.e., not fixed)
! so in a way, NPAR is the number of parameters that are parameters.
! Note that confusingly enough, in the CCSL code the definitions of "parameter" and "variable"
! is the opposite way round.
          NPAR = NPAR + 1
          IP(NPAR) = I
        ENDIF
      ENDDO
      nmpert = NT * NS * NPAR ! Number of Moves per Temperature
      CALL OpenChiSqPlotWindow
! ####################################
!   Starting point for multiple runs
! ####################################
    1 CONTINUE ! The start point for multiple runs.
! Set initial values.
      iMyExit = 0
      Curr_SA_Run = Curr_SA_Run + 1
      WRITE (SA_RunNumberStr,'(I3.3)') Curr_SA_Run
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Action1)
      WRITE(CNruns,'(I3)') Curr_SA_Run
      WRITE(CMruns,'(I3)') MaxRuns
      CNruns = ADJUSTL(CNruns)
      CMruns = ADJUSTL(CMruns)
      CALL WDialogPutString(IDD_SA_RunLabel,'Simulated annealing run number '//CNRuns(1:LEN_TRIM(CNruns))// &
                                            ' of '//CMRuns(1:LEN_TRIM(CMRuns)))
      CALL PopActiveWindowID
      num_new_min = 0
      num_old_min = -1
      MAKET0 = (T0.LE.0.0)  ! T0 is estimated each run of a multirun
      IF (MAKET0) THEN
        T = 100000.0
      ELSE
        T = T0
      ENDIF
! Initialise the random number generator RANMAR.
! Increment the seeds for each SA run
      CALL RMARInit(ISEED1+Curr_SA_Run,ISEED2+Curr_SA_Run)
! Initialise all degrees of freedom either to a preset value or to
! a random value
      CALL MAKXIN(nvar)
      NACC = 0
      NTOTMOV = 0
      DO I = 1, nvar
        XOPT(I) = X(I)
   !     DO I = 1, nvar
          NACP(I) = 0
          NumTrialsPar(I) = 0
    !    ENDDO
        C(I) = 2.0
      ENDDO
! Evaluate the function with input X and return value as F.
      IF (PrefParExists) CALL PO_PRECFC(X(iPrfPar))
      CALL FCN(X,F,0)
      DO II = 1, NATOM
        DO III = 1, 3
          XAtmCoords(III,II,Curr_SA_Run) = XATO(III,II)
        ENDDO
      ENDDO
      FOPT = F
! Evaluate the profile chi-squared as well
      CALL valchipro(CHIPROBEST)
      CALL WDialogSelect(IDD_Summary)
      CALL WGridRows(IDF_SA_Summary, Curr_SA_Run)
      WRITE(RowLabelStr,'(I2)') Curr_SA_Run
      CALL WGridLabelRow(IDF_SA_summary, Curr_SA_Run, RowLabelStr)
      CALL WGridPutCellInteger (IDF_SA_Summary,1,Curr_SA_Run, NumOf_SA_Runs+1) 
      CALL WGridPutCellCheckBox(IDF_SA_Summary,3,Curr_SA_Run, 1)
      CALL WGridPutCellReal    (IDF_SA_Summary,4,Curr_SA_Run, CHIPROBEST, '(F7.2)')
      CALL WGridPutCellReal    (IDF_SA_Summary,5,Curr_SA_Run, FOPT, '(F7.2)')
      CALL WDialogPutInteger(IDF_Limit1, 1)
      CALL WDialogPutInteger(IDF_Limit2, Curr_SA_Run)
      PrevRejected = .TRUE.
! Plot the profile
      CALL Profile_Plot
      MRAN  = ISEED1 + Curr_SA_Run
      MRAN1 = ISEED2 + Curr_SA_Run
      Last_NUP   = nmpert / 2
      Last_NDOWN = nmpert / 2
      Curr_SA_Iteration = 0
      CALL ChiSqPlot_UpdateIterAndChiProBest(Curr_SA_Iteration)
! ##########################################
!   Starting point for multiple iterations
! ##########################################
  100 CONTINUE
      Curr_SA_Iteration = Curr_SA_Iteration + 1
      IF (Use_Sharpen) THEN
        IF (MAKET0) THEN
          RR_ITF = 1.0 !ITFRange
        ELSE
          RR_ITF = MAX(1.0, (T/(100.0*Initial_T)) * (ITFRange-1.0) + 1.0)
        ENDIF
        CALL CreateFobITF
        CALL FCN(X,F,0)
        CALL FCN(XOPT,FOPT,0)
      ENDIF
      NUP = 0
      NREJ = 0
      NDOWN = 0
      DO II = 1, nvar
        X0SUM(II) = 0.0
        A0SUM(II) = 0.0
        XSUM(II) = 0.0
        XXSUM(II) = 0.0
        XDSS(II) = 0.0
      ENDDO
      FPSUM0 = 0.0
      FPSUM1 = 0.0
      FPSUM2 = 0.0
! Update the SA status window
      CALL SA_OUTPUT(T,FOPT,FPAV,FPSD,dxvav,xvsig,flav,nvar,Last_NUP,Last_NDOWN,NREJ,ntotmov)
      CALL sa_move_status(nmpert,0)
! ##########################################
!   Starting point for multiple moves
! ##########################################
      DO M = 1, NT
! MRAN RANGE IS 0 -> IM=7875
        MRAN = MOD(MRAN*IA+IC,IM)
        IARR = MRAN + 1
        MRAN1 = MOD(MRAN1*IA+IC,IM)
        IAR1 = MRAN1 + 1
        DO J = 1, NS
          DO IH = 1, NPAR
            DO I = 1, nvar
              XP(I) = X(I)
            ENDDO
            CurrParsInclPO = .FALSE.
            DO iParNum = 1, NumParPerTrial
              H = IP(1+INT(RANARR(IARR)*NPAR))
              IARR = IARR + 1
              NumTrialsPar(H) = NumTrialsPar(H) + 1
              TotNumTrials = TotNumTrials + 1
! Generate XP, the trial value of X. Note use of VM to choose XP.
              DX = RANAR1(IAR1) * VM(H)
              IAR1 = IAR1 + 1
              XP(H) = X(H) + DX
! If modal ranges defined for torsions use random number to
! select from which range the value of XP will be derived.
              IF (ModalFlag(H) .EQ. 2) THEN !Bimodal
                IF (RANARR(IARR) .GT. 0.5) THEN
                  IF (UB(H) * LB(H) .LT. 0.00) THEN
                    XP(H) = XP(H) + 180.00
                    CALL ThreeSixtyToOneEighty(XP(H))
                  ELSE
                    XP(H) = -XP(H)
                  ENDIF
                ENDIF            
                IARR = IARR + 1 
              ELSEIF (ModalFlag(H) .EQ. 3) THEN !Trimodal
                xtem = XP(H)
                CALL OneEightyToThreeSixty(xtem)
                IF ((RANARR(IARR) .GE. 0.33) .AND. (RANARR(IARR).LT. 0.66)) THEN
                  xtem = xtem + 120.00
                ELSEIF ((RANARR(IARR) .GE. 0.66) .AND. (RANARR(IARR).LT. 1.00)) THEN
                  xtem = xtem + 240.00
                ENDIF
                IARR = IARR + 1
                IF (Xtem .GE. 360.00) THEN
                  Xtem = xtem - 360.00
                ENDIF
                CALL ThreeSixtyToOneEighty(xtem)
                XP(H) = xtem
              ENDIF
! If XP is out of bounds, select a point in bounds for the trial.
              SELECT CASE(ModalFlag(H))
                CASE (1) ! used for all parameters except modal torsion angles 
                  IF ((XP(H).LT.LB(H)) .OR. (XP(H).GT.UB(H))) THEN
                    TotNumRetrials = TotNumRetrials + 1
                    XP(H) = LB(H) + RULB(H) * RANARR(IARR)
                    IARR = IARR + 1
                  ENDIF   
                CASE (2) ! bimodal ranges
! Complicated-looking calcs of inbounds torsion angles is to try and guarantee a good
! sampling of all user defined ranges.  
                  IF (OutOfBounds(H, XP(H))) THEN
                    IF (UB(H) * LB(H) .LT. 0.00) THEN ! range such as -170 to 170 defined                                                  
                      TempUpper = UB(H)         ! so use 0-360 degree scale
                      TempLower = LB(H)
                      TempLower2 = TempUpper - 180.00
                      TempUpper2 = TempLower + 180.00
                      CALL OneEightyToThreeSixty(TempUpper)
                      CALL OneEightyToThreeSixty(TempLower)
                      CALL OneEightyToThreeSixty(TempUpper2)
                      CALL OneEightyToThreeSixty(TempLower2)
                      xtem = XP(H)
                      CALL OneEightytoThreeSixty(xtem)
                      Sgn = SIGN(1.0, XP(H))
                      IF (RANARR(IARR) .LT. 0.5) THEN 
                        IARR = IARR + 1               
                        xtem = MAX(TempUpper, TempUpper2) + (RULB(H) * RANARR(IARR))
                        IF (sgn .LT. 0.0) THEN
                          xtem = xtem -180.00
                        ENDIF
                      ELSE
                        IARR = IARR + 1
                        xtem = MIN(TempUpper, TempUpper2) - (RULB(H) * RANARR(IARR))
                        IF (sgn .LT. 0.0) THEN
                          xtem = xtem + 180.00
                        ENDIF
                      ENDIF
                      IARR = IARR + 1 
                      IF (xtem .GT. 360.00) THEN
                        xtem = xtem - 360.00
                      ENDIF
                      CALL ThreeSixtyToOneEighty(xtem)
                      XP(H) = xtem
                    ELSEIF (UB(H) * LB(H) .GE. 0.00) THEN ! range such as 30-90 degs or -30- -90 defined
                      Sgn = SIGN(1.0, XP(H))
                      IF (RANARR(IARR) .LT. 0.5) THEN
                        IARR = IARR + 1
                        XP(H) = LB(H) + (RULB(H) * RANARR(IARR))
                        XP(H) = XP(H) * (-Sgn)
                      ELSE
                        IARR = IARR + 1 
                        XP(H) = -LB(H) - (RULB(H) * RANARR(IARR))
                        XP(H) = XP(H) * Sgn
                      ENDIF 
                      IARR = IARR + 1
                    ENDIF
                  ENDIF
                CASE(3) !trimodal ranges
                  IF (OutOfBounds(H, XP(H))) THEN ! calculate new value in one of three allowed ranges
                    xtem = MINVAL(Tempbounds, MASK = Tempbounds .GE. 0.0) + RULB(H) * RANARR(IARR) 
                    IARR = IARR + 1
                    IF ((RANARR(IARR) .GT. 0.33) .AND. (RANARR(IARR) .LE. 0.66)) THEN
                      xtem = xtem - 120.00
                    ELSEIF ((RANARR(IARR) .GT. 0.66) .AND. (RANARR(IARR) .LE. 1.00)) THEN
                      xtem = xtem -240.00
                      IF (xtem .LT. -180.00) THEN
                        xtem = 360.00 + xtem
                      ENDIF
                    ENDIF
                    IARR = IARR + 1
                  ENDIF
                  XP(H) = xtem
              END SELECT
              IF (kzmpar2(H) .EQ. 7) CurrParsInclPO = .TRUE.
            ENDDO

! Evaluate the function with the trial point XP and return as FP.
            IF (NumParPerTrial .EQ. 1) THEN
              IF (PrevRejected) THEN
                IF (PrevParsInclPO) THEN
                  IF (CurrParsInclPO) THEN
                    CALL FCN(XP,FP,iPrfPar)
                  ELSE
                    CALL PO_PRECFC(XP(iPrfPar))
                    CALL FCN(XP,FP,0)
                  ENDIF
                ELSE
                  IF (CurrParsInclPO) CALL PO_PRECFC(XP(iPrfPar))
                  CALL FCN(XP,FP,0)
                ENDIF
              ELSE
                CALL FCN(XP,FP,H) ! Becomes H(1)
              ENDIF
            ELSE
              IF ((CurrParsInclPO) .OR. (PrevParsInclPO .AND. PrevRejected)) CALL PO_PRECFC(XP(iPrfPar))
              CALL FCN(XP,FP,0)
            ENDIF
            PrevParsInclPO = CurrParsInclPO
            XDSS(H) = XDSS(H) + (FP-F)**2
            PrevRejected = .FALSE.
            IF (FP .LE. F) THEN
              X(H) = XP(H)
              F = FP
              NACC = NACC + 1
              NACP(H) = NACP(H) + 1
              NUP = NUP + 1
! If lower than any other point, record as new optimum.
              IF (FP .LT. FOPT) THEN
                DO I = 1, nvar
                  XOPT(I) = XP(I)
                ENDDO
                DO II = 1, NATOM
                  DO III = 1, 3
                    XAtmCoords(III,II,Curr_SA_Run) = XATO(III,II)
                  ENDDO
                ENDDO
                num_new_min = num_new_min + 1
                CALL valchipro(CHIPROBEST)
                FOPT = FP
                CALL WDialogSelect(IDD_Summary)
                CALL WGridPutCellReal(IDF_SA_Summary, 4, Curr_SA_Run, CHIPROBEST, '(F7.2)')
                CALL WGridPutCellReal(IDF_SA_Summary, 5, Curr_SA_Run, FOPT, '(F7.2)')
! Update the SA status window
                CALL SA_OUTPUT(T,FOPT,FPAV,FPSD,dxvav,xvsig,flav,NVAR,Last_NUP,Last_NDOWN,NREJ,ntotmov)
              ENDIF
! If the point is greater, use the Metropolis criterion to decide on
! acceptance or rejection.
            ELSE
              P = EXPREP((F-FP)/T)
              PP = RANARR(IARR)
              IARR = IARR + 1
              IF (PP .LT. P) THEN
                X(H) = XP(H)
                F = FP
                NACC = NACC + 1
                NACP(H) = NACP(H) + 1
                NDOWN = NDOWN + 1
              ELSE
                NREJ = NREJ + 1
                PrevRejected = .TRUE.
              ENDIF
            ENDIF

            FPSUM1 = FPSUM1 + F
            FPSUM2 = FPSUM2 + F*F
            A0SUM(H) = A0SUM(H) + 1.0

            X0SUM(H) = X0SUM(H) + 1.0
            XSUM(H) = XSUM(H) + X(H)
            XXSUM(H) = XXSUM(H) + X(H)**2
          ENDDO ! Loop over parameters
          CALL sa_move_status(nmpert,m*NPAR*NS)
        ENDDO ! Loop over NS
! Adjust VM so that approximately half of all evaluations are accepted.
        DO II = 1, NPAR
          I = IP(II)
          RATIO = FLOAT(NACP(I))/FLOAT(NumTrialsPar(I))
          IF (RATIO .GT. 0.6) THEN
            VM(I) = VM(I)*(1.0+C(I)*(RATIO-0.6)/0.4)
          ELSEIF (RATIO .LT. 0.4) THEN
            VM(I) = VM(I)/(1.0+C(I)*((0.4-RATIO)/0.4))
          ENDIF
          IF (VM(I) .GT. RULB(I)) THEN
            VM(I) = RULB(I)
          ENDIF
          IF (VM(I) .LT. 0.01*RULB(I)) THEN
            VM(I) = 0.01*RULB(I)
          ENDIF
        ENDDO
        CALL PeekEvent
        DO WHILE (iMyExit .EQ. 6) ! Pause
          CALL GetEvent
          IF (EventInfo%WIN .EQ. IDD_Pause) THEN
            CALL WDialogSelect(IDD_Pause)
            CALL WDialogUnload
            CALL WDialogSelect(IDD_SA_Action1)
            CALL WDialogFieldState(IDF_Pause_Annealing, Enabled)
            iMyExit = 0
          ENDIF
        ENDDO
        IF (iMyExit .NE. 0) GOTO 999 ! Exit all loops and jump straight to the end
      ENDDO ! Loop over moves per iteration (NT)
      FPSUM0 = FPSUM0 + FLOAT(NS*NT*NPAR)
      Last_NDOWN = NDOWN
      Last_NUP   = NUP
! Calculate the average energy and deviation
      FPAV = FPSUM1/FPSUM0
      FPSD = SQRT(MAX(0.0,(FPSUM2/FPSUM0)-(FPAV**2)))
      DO I = 1, nvar
        IF (X0SUM(I) .GT. 0.0) THEN
          DXVAV(I) = 1.0 !XSUM(I)/X0SUM(I)
          XVSIG(I) = 1.0 !SQRT(MAX(0.0,(XXSUM(I)/X0SUM(I))-(DXVAV(I)*DXVAV(I))))
          FLAV(I) = 1.0 !SQRT(MAX(0.0,XDSS(I)/A0SUM(I)))
        ENDIF
      ENDDO
      ntotmov = ntotmov + nmpert
      IF (num_new_min .NE. num_old_min) CALL Profile_Plot ! plot the profile
      num_old_min = num_new_min
      CALL SA_OUTPUT(T,FOPT,FPAV,FPSD,dxvav,xvsig,FLAV,nvar,Last_NUP,Last_NDOWN,NREJ,ntotmov)
! If we have asked for an initial temperature to be calculated then do so
      IF (MAKET0) THEN
! Start temperature increased by 50% as asked for
        T = FPSD*1.5
        Initial_T = T
! Having done an SA cycle at T = 100000, the initial values are now randomised.
! If the user had requested the values supplied to be used, reset them.
        CALL MAKXIN(nvar)
        MAKET0 = .FALSE.
        CALL ChiSqPlot_UpdateIterAndChiProBest(Curr_SA_Iteration)
        GOTO 100
      ENDIF
! If termination criteria are not met, prepare for another loop.
! We will use the energy fluctuation to reduce the temperature
      T = T/(1.0+(RT*T)/(3.0*FPSD))
      DO I = 1, nvar
        X(I) = XOPT(I)
      ENDDO
      F = FOPT
  999 CONTINUE ! This is where we jump to if the user pressed 'Stop' during the SA
               ! The variable imyexit has been set to 3, 4 or 5  (3 = stop SA, 4 = start next run, 5 = Edit)
               ! If we didn't jump to this point, just passed it, imyexit is 0
      CALL ChiSqPlot_UpdateIterAndChiProBest(Curr_SA_Iteration)
!  Check termination criteria.
!  Terminate SA if appropriate.
      IF (CheckTerm(NTOTMOV) .OR. (iMyExit .NE. 0)) THEN
! End of a run in a multi-run. This is the place for a final local minimisation
        NumOf_SA_Runs = Curr_SA_Run
! Get AutoLocalMinimisation from the Configuration Window
        IF ((iMyExit .NE. 5) .AND. Get_AutoLocalMinimisation()) THEN
          CALL LocalMinimise(.TRUE.)
          CALL ChiSqPlot_UpdateIterAndChiProBest(Curr_SA_Iteration)
        ENDIF
        CALL ChiSqPlot_EndOfSARun
! ep  Saves calculated and observed diffraction patterns in .pro file 
        CALL Sa_soln_store
! Align structure.  Will get to this point whether autominimise enabled or not.
        IF (Get_AutoAlign()) CALL Align
! Store optimum crystal structure        
        CALL Log_SARun_Entry
        CALL SA_STRUCTURE_OUTPUT(T, XOPT, ntotmov)
        IF ((Curr_SA_Run .LT. MaxRuns) .AND. (iMyExit .NE. 3) .AND. (iMyExit .NE. 5)) THEN
          GOTO 1 ! Next run
        ENDIF
      ELSE
        GOTO 100 ! Next iteration
      ENDIF

      END SUBROUTINE SimulatedAnnealing
!
!*****************************************************************************
!
      REAL FUNCTION EXPREP(RDUM)
! This function replaces exp to avoid under- and overflows

      REAL, INTENT (IN   ) :: RDUM

      IF (RDUM .GT. 88.0) THEN
        EXPREP = 1.6516363E+38
      ELSEIF (RDUM .LT. -103.0) THEN
        EXPREP = 0.0
      ELSE
        EXPREP = EXP(RDUM)
      ENDIF

      END FUNCTION EXPREP
!
!*****************************************************************************
!
      SUBROUTINE RMARInit(IJ,KL)
!  This subroutine and the next function generate random numbers. See
!  the comments for SA for more information. The only changes from the
!  orginal code is that (1) the test to make sure that RMARIN runs first
!  was taken out since SA assures that this is done (this test didn't
!  compile under IBM's VS Fortran) and (2) typing ivec as integer was
!  taken out since ivec isn't used. With these exceptions, all following
!  lines are original.
!
! This is the initialization routine for the random number generator
!     RANMAR()
! NOTE: The seed variables can have values between:    0 <= IJ <= 31328
!                                                      0 <= KL <= 30081
      IMPLICIT NONE
      
      INTEGER, INTENT (INOUT) :: IJ, KL
      
      REAL            U,     C, CD, CM
      INTEGER                           I97, J97
      COMMON /raset1/ U(97), C, CD, CM, I97, J97

      INTEGER ii, jj, i, j, k, l, m
      REAL    s, t

      IF ((IJ.LT.0) .OR. (IJ.GT.31328) .OR. (KL.LT.0) .OR. (KL.GT.30081)) THEN
        CALL InfoMessage('Seeds for random number generator must be '// &
            'between 0 and 30081 and have been reset to 1375 and 29767.')
        IJ = 1375
        KL = 29767
      ENDIF
      i = MOD(IJ/177,177) + 2
      j = MOD(IJ,177) + 2
      k = MOD(KL/169,178) + 1
      l = MOD(KL,169)
      DO ii = 1, 97
        s = 0.0
        t = 0.5
        DO jj = 1, 24
          m = MOD(MOD(i*j,179)*k,179)
          i = j
          j = k
          k = m
          l = MOD(53*l+1,169)
          IF (MOD(l*m,64).GE.32) THEN
            s = s + t
          ENDIF
          t = 0.5*t
        ENDDO
        U(ii) = s
      ENDDO
      C = 362436.0/16777216.0
      CD = 7654321.0/16777216.0
      CM = 16777213.0/16777216.0
      I97 = 97
      J97 = 33

      END SUBROUTINE RMARInit
!
!*****************************************************************************
!
      REAL FUNCTION RANMAR

      IMPLICIT NONE

      REAL            U,     C, CD, CM
      INTEGER                           I97, J97
      COMMON /raset1/ U(97), C, CD, CM, I97, J97

      REAL uni

      uni = U(I97) - U(J97)
      IF (uni.LT.0.0) uni = uni + 1.0
      U(I97) = uni
      I97 = I97 - 1
      IF (I97.EQ.0) I97 = 97
      J97 = J97 - 1
      IF (J97.EQ.0) J97 = 97
      C = C - CD
      IF (C.LT.0.0) C = C + CM
      uni = uni - C
      IF (uni.LT.0.0) uni = uni + 1.0
      RANMAR = uni

      END FUNCTION RANMAR
!
!*****************************************************************************
!
      SUBROUTINE RANX2E(DRIN, DROUT)

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: drin
      REAL, INTENT (  OUT) :: drout

      REAL            xs, yran
      COMMON /x2eran/ xs, yran(200)

      REAL rin, xl, xsn, xm, xh, yl, yh, ym, xlo, xmo, xho, ylo, ymo, yho, rout
      INTEGER jn, j

      rin = ABS(drin)
      jn = 1
      DO j = 199, 1, -1
        IF (rin .GT. yran(j)) THEN
          jn = j
          GOTO 10
        ENDIF
      ENDDO
! rin is between yran(jn) and yran(jn+1) now iterate
   10 xl = xs*FLOAT(jn-1)
      xsn = 0.5*xs
      xm = xl + xsn
      xh = xm + xsn
      yl = yran(jn)
      yh = yran(jn+1)
      ym = 1.0 - (0.5*xm*xm+xm+1.)*EXP(-xm)
   15 xlo = xl
      xmo = xm
      xho = xh
      ylo = yl
      ymo = ym
      yho = yh
      IF (ABS(yh-yl) .LT. 1.0E-3) GOTO 20
      IF (rin .GE. ym) THEN
        xl = xmo
        xh = xho
        yl = ymo
        yh = yho
      ELSE
        xl = xlo
        xh = xmo
        yl = ylo
        yh = ymo
      ENDIF
      xm = 0.5*(xh+xl)
      ym = 1. - (0.5*xm*xm+xm+1.)*EXP(-xm)
      GOTO 15
   20 rout = xm
      IF (drin.LT.0.0) rout = -rout
      drout = rout

      END SUBROUTINE RANX2E
!
!*****************************************************************************
!
      SUBROUTINE RANX2Init

      IMPLICIT NONE

      REAL            xs, yran
      COMMON /x2eran/ xs, yran(200)

      INTEGER I
      REAL    xi

      xs = 0.1
      DO i = 1, 200
        xi = xs*FLOAT(i-1)
        yran(i) = 1. - (0.5*xi*xi+xi+1.)*EXP(-xi)
      ENDDO

      END SUBROUTINE RANX2Init
!
!*****************************************************************************
!
      SUBROUTINE MAKXIN(N)

      USE WINTERACTER
      USE DRUID_HEADER
   
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 

      INTEGER, INTENT (IN   ) :: N

      REAL          RULB
      COMMON /RULB/ RULB(Mvar)

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      REAL, EXTERNAL :: RANMAR
      INTEGER IV

! Get the "IDF_RandomInitVal" checkbox
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Modal_input2)
      IF (WDialogGetCheckBoxLogical(IDF_RandomInitVal)) THEN
        DO IV = 1, N
          X(IV) = LB(IV) + RULB(IV)*RANMAR()
        ENDDO
      ELSE
        DO IV = 1, N
          CALL WGridGetCellReal(IDF_parameter_grid_modal, 1, IV, X(IV))
        ENDDO
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE MAKXIN
!
!*****************************************************************************
!
      LOGICAL FUNCTION CheckTerm(Nmoves)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Nmoves

      INCLUDE 'PARAMS.INC'

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      REAL             XOPT,       C,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      REAL Best_CHI

      IF (Is_SX) THEN
        Best_CHI = FOPT
      ELSE
        Best_CHI = CHIPROBEST
      ENDIF
      CheckTerm = ((Nmoves .GT. MaxMoves) .OR. (Best_CHI .LT. (ChiMult*PAWLEYCHISQ)))

      END FUNCTION CheckTerm
!
!*****************************************************************************
!
      SUBROUTINE FillRULB(nvar)

! This subroutine determines the upper and lower bounds for all parameters including
! modal torsion angles.

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER, INTENT (INOUT) :: nvar

      REAL          RULB
      COMMON /RULB/ RULB(Mvar)

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(mvar), lb(mvar), ub(mvar), vm(mvar)

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      REAL TempUpper, TempLower
      INTEGER I
      LOGICAL ONeEightyScale

      DO I = 1, nvar
        RULB(I) = UB(I) - LB(I)
        IF (ModalFlag(I) .EQ. 2) THEN
          CALL CheckBiModalBounds(I, OneEightyScale)
          IF (.NOT. OneEightyScale) THEN
            tempUpper = UB(I)
            tempLower = LB(I)
            CALL OneEightyToThreeSixty(tempUpper)
            CALL OneEightyToThreeSixty(tempLower)
            RULB(I) = ABS(tempUpper - tempLower)
          ENDIF
        ENDIF
        IF (ModalFlag(I) .EQ. 3) THEN
          tempUpper = UB(I)
          CALL DetermineTriModalBounds(tempUpper, 1)
          tempLower = LB(I)
          CALL DetermineTriModalBounds(tempLower, 2)
          CALL CheckTriModalBounds(OneEightyScale)
          IF (.NOT. OneEightyScale) THEN ! A range such as -170 to 170 has been defined
            tempUpper = UB(I)
            tempLower = LB(I)
            CALL OneEightyToThreeSixty(TempUpper)
            CALL OneEightyToThreeSixty(TempLower)
            RULB(I) = ABS(TempUpper - TempLower)
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE FillRULB
!
!*****************************************************************************
!
