! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      SUBROUTINE SimulatedAnnealing

      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE ATMVAR
      USE PO_VAR
      USE ZMVAR
      USE SOLVAR
      USE RRVAR
      USE PRJVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'Lattice.inc'

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, NS, NT, iSeed1, iSeed2
      COMMON /sapars/ nvar, NS, NT, iSeed1, iSeed2

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
      COMMON /POSNS / NATOM, XATO(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
     &                KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
     &                SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

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

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      INTEGER              iMyExit 
      LOGICAL                       NewOptimumFound, WasMinimised, TestEarlyTermFlag
      COMMON / CMN000001 / iMyExit, NewOptimumFound, WasMinimised, TestEarlyTermFlag

      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL         SimplexOnly
      COMMON /SIMPONLY/ SimplexOnly

      
      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      REAL, EXTERNAL :: EXPREP, RANMAR_2, GetMaxwellRandomNumber, GetMDBRandomTorsion
      LOGICAL, EXTERNAL :: IsEventWaiting, Get_AutoAlign
      LOGICAL, EXTERNAL :: CheckTerm, OutOfBounds, GoodSolution
      CHARACTER*20, EXTERNAL :: GetSeed1SuffixString
      CHARACTER(MaxPathLength) tTmpSaveName
      INTEGER NumTrialsPar(MVAR), NumParPerTrial, iParNum
      INTEGER NACP(MVAR)
      LOGICAL MAKET0
      REAL FPSUM0, FPSUM1, FPSUM2, FPAV, FPSD
      REAL RATIO, DX, F, FP, P, PP
      INTEGER NUP, NDOWN, H, I, J, M, II
      REAL T
      REAL DXVAV(mvar), XVSIG(mvar), FLAV(mvar)
      REAL X0SUM(mvar), XSUM(mvar), XXSUM(mvar)
      REAL XDSS(mvar), A0SUM(mvar)
      INTEGER NTOTMOV
      INTEGER III, IH, KK, iFrg
      INTEGER Last_NUP, Last_NDOWN
      CHARACTER*3 CNruns,CMruns
      LOGICAL PrevRejected, CurrParsInclPO, PrevParsInclPO
      LOGICAL tTestEarlyTerm, tOptimumedSinceTET
      INTEGER TotNumTrials
      CHARACTER*3 RowLabelStr
      REAL XP(MVAR)

      REAL xtem, tempupper, templower, tempupper2
      REAL Initial_T ! As calculated during intial run, not as set by the user.
      REAL VM(MVAR)
      REAL InitialProChiSqrd
      REAL ProgressIndicator ! 0.0 = just begun, 1.0 = best profile chi-sqrd <= Pawley chi-sqrd
      

      CALL SetTmpSaveFileName(tTmpSaveName)
      
      WasMinimised = .FALSE.
      NumParPerTrial = 1
      TotNumTrials = 0
      IF (.NOT. Resume_SA) THEN
        Curr_SA_Run = 0
        NumOf_SA_Runs = 0
        DO I = 1, MaxRun
          iSol2Run(I) = I
        ENDDO
      ENDIF
      iSolTicked = 1
      CALL RANX2Init
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
! vm is adjusted during the SA. So re-initialise every time the SA is started to
! ensure that starting the SA more than once with the same parameters will give
! identical results.
        kk = 0
        DO iFrg = 1, nFrag
          DO ii = 1, izmpar(iFrg)
            kk = kk + 1
            SELECT CASE (kzmpar(ii,iFrg))
              CASE (1) ! translation
                VM(kk) = 0.1
              CASE (2) ! quaternion
                VM(kk) = 0.1
              CASE (3) ! torsion
                VM(kk) = 10.0
              CASE (4) ! angle
                VM(kk) = 1.0
              CASE (5) ! bond
                VM(kk) = 0.1*(ub(kk)-lb(kk))
              CASE (6) ! single rotation axis
                VM(kk) = 0.1
            END SELECT
          ENDDO
        ENDDO
        IF ( PrefParExists ) THEN
          kk = kk + 1
          VM(kk) = 0.01
        ENDIF
      CALL OpenChiSqPlotWindow
      Resume_SA = .FALSE.

! ####################################
!   Starting point for multiple runs
! ####################################
    1 CONTINUE ! The start point for multiple runs.
! Set initial values.
      iMyExit = 0
      Curr_SA_Run = Curr_SA_Run + 1
      WRITE (SA_RunNumberStr, '(I3.3)') Curr_SA_Run
      IF ( .NOT. in_batch ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_Action1)
        WRITE(CNruns, '(I3)') Curr_SA_Run
        WRITE(CMruns, '(I3)') MaxRuns
        CNruns = ADJUSTL(CNruns)
        CMruns = ADJUSTL(CMruns)
        CALL WDialogPutString(IDD_SA_RunLabel,'Simulated annealing run number '//CNRuns(1:LEN_TRIM(CNruns))// &
                                              ' of '//CMRuns(1:LEN_TRIM(CMRuns)))
        CALL PopActiveWindowID
      ENDIF
      
      MAKET0 = (T0.LE.0.0)  ! T0 is estimated each run of a multirun
      IF (MAKET0) THEN
        T = 100000.0
      ELSE
        T = T0
      ENDIF
! Increment the seeds for each SA run
      CALL RMARInit(iSEED1+Curr_SA_Run, iSEED2+Curr_SA_Run)
! Initialise all degrees of freedom either to a preset value or to
! a random value
      NTOTMOV = 0
      CALL MAKXIN
      DO I = 1, nvar
        XOPT(I) = x_unique(I)
        C(I) = 2.0
      ENDDO
      CALL SetSpringWeight( 0.0 )
! Evaluate the function with input X and return value as F.
      IF (PrefParExists) CALL PO_PRECFC(x_unique(iPrfPar))
      CALL FCN(x_unique,F,0, .TRUE.)
      DO II = 1, NATOM
        DO III = 1, 3
          XAtmCoords(III,II,Curr_SA_Run) = XATO(III,II)
        ENDDO
      ENDDO
      FOPT = F
      NewOptimumFound = .FALSE.
      tOptimumedSinceTET = .FALSE.
! Evaluate the profile chi-squared as well
      CALL valchipro(CHIPROBEST)
      InitialProChiSqrd = CHIPROBEST
      IF ( .NOT. in_batch ) THEN
        CALL ChiSqPlot_UpdateIterAndChiProBest(1)
        CALL SelectDASHDialog(IDD_Summary)
        CALL WGridRows(IDF_SA_Summary, Curr_SA_Run)
        WRITE(RowLabelStr,'(I3)') Curr_SA_Run
        CALL WGridLabelRow(IDF_SA_summary, Curr_SA_Run, RowLabelStr)
        CALL WGridPutCellInteger (IDF_SA_Summary, 1, Curr_SA_Run, NumOf_SA_Runs+1) 
        CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, Curr_SA_Run, 1)
        CALL WGridPutCellReal    (IDF_SA_Summary, 4, Curr_SA_Run, CHIPROBEST, '(F7.2)')
        CALL WGridPutCellReal    (IDF_SA_Summary, 5, Curr_SA_Run, FOPT, '(F7.2)')
        CALL WDialogPutInteger(IDF_Limit1, 1)
        CALL WDialogPutInteger(IDF_Limit2, Curr_SA_Run)
! Plot the profile
        CALL Profile_Plot
      ENDIF
      PrevRejected = .TRUE.
! Initialise Marsaglia random number generator
      CALL RMARInit_2(ISEED1 + Curr_SA_Run, ISEED2 + Curr_SA_Run)
      Last_NUP   = nmpert / 2
      Last_NDOWN = nmpert / 2
      Curr_SA_Iteration = 0
      DO I = 1, nvar
        NACP(I) = 0
        NumTrialsPar(I) = 0
      ENDDO

      

! ##########################################
!   Starting point for multiple iterations
! ##########################################
  100 CONTINUE  
      Curr_SA_Iteration = Curr_SA_Iteration + 1
      IF ( .NOT. SimplexOnly ) THEN
          
      ! Next line is wrong for single crystal, which should use the intensity chi-sqrd
      ProgressIndicator = 1.0 -( (CHIPROBEST - (ChiMult*PAWLEYCHISQ)) / (InitialProChiSqrd - (ChiMult*PAWLEYCHISQ)))
      CALL SetSpringWeight( ProgressIndicator )
      NUP = 0
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
      CALL SA_OUTPUT(T,FOPT,FPAV,FPSD,dxvav,xvsig,flav,nvar,Last_NUP,Last_NDOWN,ntotmov)
      CALL sa_move_status(nmpert,0)
! ##########################################
!   Starting point for multiple moves
! ##########################################
      
      
      DO M = 1, NT
        DO J = 1, NS
          DO IH = 1, NPAR
          
            DO I = 1, nvar
              XP(I) = x_unique(I)
            ENDDO
            CurrParsInclPO = .FALSE.
            DO iParNum = 1, NumParPerTrial
              H = IP(1+INT(RANMAR_2()*NPAR))
              NumTrialsPar(H) = NumTrialsPar(H) + 1
              TotNumTrials = TotNumTrials + 1
! Generate XP, the trial value of X. Note use of VM to choose XP.
              IF (ModalFlag(H) .EQ. 4) THEN ! MDB
                ! Keep in range: calling, eg. LocalMinimise() may break this
                XP(H) = MOD(XP(H), 360.0)
                IF (XP(H) .LT. -180.0) XP(H) = XP(H) + 360.0
                IF (XP(H) .GT. +180.0) XP(H) = XP(H) - 360.0
                XP(H) = GetMDBRandomTorsion(RANMAR_2(), XP(H), VM(H), H) 
                GOTO 555
              ENDIF
              DX = GetMaxwellRandomNumber() * VM(H)
              XP(H) = XP(H) + DX
! If modal ranges defined for torsions use random number to
! select from which range the value of XP will be derived.
              IF (ModalFlag(H) .EQ. 2) THEN ! Bimodal
                IF (RANMAR_2() .GT. 0.5) THEN
                  IF (UB(H) * LB(H) .LT. 0.0) THEN
                    XP(H) = XP(H) + 180.0
                  ELSE
                    XP(H) = -XP(H)
                  ENDIF
                ENDIF            
                CALL ThreeSixtyToOneEighty(XP(H))
              ELSEIF (ModalFlag(H) .EQ. 3) THEN ! Trimodal
                xtem = XP(H)
                CALL OneEightyToThreeSixty(xtem)
                PP = RANMAR_2()
                IF ((PP .GE. 1.0/3.0) .AND. (PP .LT. 2.0/3.0)) THEN
                  xtem = xtem + 120.00
                ELSEIF ( PP .GE. 2.0/3.0 ) THEN
                  xtem = xtem + 240.00
                ENDIF
                IF (Xtem .GE. 360.00) THEN
                   Xtem = xtem - 360.00
                endif
                CALL ThreeSixtyToOneEighty(xtem)
                XP(H) = xtem
              ENDIF
! If XP is out of bounds, select a point in bounds for the trial.
              SELECT CASE(ModalFlag(H))
                CASE (0,1) ! used for all parameters except modal torsion angles
                  IF (IsFullRangeTrans(H)) THEN
! We don't actually have to do anything here: just skipping the out-of-bounds test suffices
                    IF (XP(H) .LT. 0.0) XP(H) = XP(H) + 1.0
                    IF (XP(H) .GT. 1.0) XP(H) = XP(H) - 1.0
                  ELSE IF (IsFullRangeTorsion(H)) THEN
! We don't actually have to do anything here: just skipping the out-of-bounds test suffices
                    XP(H) = MOD(XP(H), 360.0)
                    IF (XP(H) .LT. -180.0) XP(H) = XP(H) + 360.0
                    IF (XP(H) .GT. +180.0) XP(H) = XP(H) - 360.0
                  ELSE
                    IF ((XP(H).LT.LB(H)) .OR. (XP(H).GT.UB(H))) THEN
                      XP(H) = LB(H) + RULB(H) * RANMAR_2()
                    ENDIF
                  ENDIF
                CASE (2) ! Bimodal ranges
! Complicated-looking calcs of inbounds torsion angles is to try to guarantee a good
! sampling of all user defined ranges.  
                  IF (OutOfBounds(H, XP(H))) THEN
                    IF (UB(H) * LB(H) .LT. 0.0) THEN ! Range such as -170 to 170 defined                                                  
                      TempLower = LB(H)              ! so use 0 to 360 degree scale
                      TempUpper = UB(H)
                      TempUpper2 = TempLower + 180.0
                      CALL OneEightyToThreeSixty(TempUpper)
                      CALL OneEightyToThreeSixty(TempUpper2)
                      IF (RANMAR_2() .LT. 0.5) THEN 
                        XP(H) = MAX(TempUpper, TempUpper2) + (RULB(H) * RANMAR_2())
                      ELSE
                        XP(H) = MIN(TempUpper, TempUpper2) - (RULB(H) * RANMAR_2())
                      ENDIF
                    ELSEIF (UB(H) * LB(H) .GE. 0.0) THEN ! Range such as 30 to 90 degs or -30 to -90 defined
                      XP(H) = LB(H) + (RULB(H) * RANMAR_2())
                      IF (RANMAR_2() .LT. 0.5) XP(H) = -XP(H)
                    ENDIF
                    CALL ThreeSixtyToOneEighty(XP(H))
                  ENDIF
                CASE (3) ! Trimodal ranges
                  IF (OutOfBounds(H, XP(H))) THEN ! Calculate new value in one of three allowed ranges
                    xtem = MINVAL(Tempbounds, MASK = Tempbounds .GE. 0.0) + RULB(H) * RANMAR_2() 
                    PP = RANMAR_2()
                    IF ((PP .GT. 1.0/3.0) .AND. (PP .LE. 2.0/3.0)) THEN
                      xtem = xtem - 120.00
                    ELSEIF ( PP .GT. 2.0/3.0 ) THEN
                      xtem = xtem -240.00
                      IF (xtem .LT. -180.00) THEN
                        xtem = 360.00 + xtem
                      ENDIF
                    ENDIF
                  ENDIF
                  XP(H) = xtem
              END SELECT
 555          CONTINUE
              IF (kzmpar2(H) .EQ. 7) CurrParsInclPO = .TRUE.
            ENDDO

! Evaluate the function with the trial point XP and return as FP.
            IF (NumParPerTrial .EQ. 1) THEN
              IF (PrevRejected) THEN
                IF (PrevParsInclPO) THEN
                  IF (CurrParsInclPO) THEN
                    CALL FCN(XP,FP,iPrfPar, .TRUE.)
                  ELSE
                    CALL PO_PRECFC(XP(iPrfPar))
                    CALL FCN(XP,FP,0, .TRUE.)
                  ENDIF
                ELSE
                  IF (CurrParsInclPO) CALL PO_PRECFC(XP(iPrfPar))
                  CALL FCN(XP,FP,0, .TRUE.)
                ENDIF
              ELSE
                CALL FCN(XP,FP,H, .TRUE.)
              ENDIF
            ELSE
              IF ((CurrParsInclPO) .OR. (PrevParsInclPO .AND. PrevRejected)) CALL PO_PRECFC(XP(iPrfPar))
              CALL FCN(XP,FP,0, .TRUE.)
            ENDIF
            PrevParsInclPO = CurrParsInclPO

            FPSUM1 = FPSUM1 + FP
            FPSUM2 = FPSUM2 + FP*FP
            A0SUM(H) = A0SUM(H) + 1.0

            XDSS(H) = XDSS(H) + (FP-F)**2
            PrevRejected = .FALSE.
            IF (FP .LE. F) THEN
              x_unique(H) = XP(H)
              F = FP
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
                NewOptimumFound = .TRUE.
                tOptimumedSinceTET = .TRUE.
                CALL valchipro(CHIPROBEST)
                ProgressIndicator = 1.0 -( (CHIPROBEST - (ChiMult*PAWLEYCHISQ)) / (InitialProChiSqrd - (ChiMult*PAWLEYCHISQ)))
                FOPT = FP
                IF ( .NOT. in_batch ) THEN
                  CALL SelectDASHDialog(IDD_Summary)
                  CALL WGridPutCellReal(IDF_SA_Summary, 4, Curr_SA_Run, CHIPROBEST, '(F7.2)')
                  CALL WGridPutCellReal(IDF_SA_Summary, 5, Curr_SA_Run, FOPT, '(F7.2)')
! Update the SA status window
                ENDIF
                CALL SA_OUTPUT(T,FOPT,FPAV,FPSD,dxvav,xvsig,flav,NVAR,Last_NUP,Last_NDOWN,ntotmov)
              ENDIF
! If the point is greater, use the Metropolis criterion to decide on
! acceptance or rejection.
            ELSE
              P = EXPREP((F-FP)/T)
              IF (RANMAR_2() .LT. P) THEN
                x_unique(H) = XP(H)
                F = FP
                NACP(H) = NACP(H) + 1
                NDOWN = NDOWN + 1
              ELSE
                PrevRejected = .TRUE.
              ENDIF
            ENDIF

      !O      FPSUM1 = FPSUM1 + F
      !O      FPSUM2 = FPSUM2 + F*F
      !O      A0SUM(H) = A0SUM(H) + 1.0

            X0SUM(H) = X0SUM(H) + 1.0
            XSUM(H) = XSUM(H) + x_unique(H)
            XXSUM(H) = XXSUM(H) + x_unique(H)**2
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
        
        IF ( .NOT. in_batch ) &
            CALL PeekEvent

        IF (WasMinimised) THEN
          F = FOPT          
          WasMinimised = .FALSE.
        ENDIF
        
        DO WHILE (iMyExit .EQ. 6) ! Pause
          CALL GetEvent
          IF (EventInfo%WIN .EQ. IDD_Pause) THEN
            CALL UnloadDASHDialog(IDD_Pause)
            CALL SelectDASHDialog(IDD_SA_Action1)
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
      FPSD = SQRT( ABS ( (FPSUM2/FPSUM0)-(FPAV**2) ) )
      ! FPSD = SQRT((FPSUM2/FPSUM0)+(FPAV**2))
      DO I = 1, nvar
        IF (X0SUM(I) .GT. 0.0) THEN
          DXVAV(I) = 1.0 !XSUM(I)/X0SUM(I)
          XVSIG(I) = 1.0 !SQRT(MAX(0.0,(XXSUM(I)/X0SUM(I))-(DXVAV(I)*DXVAV(I))))
          FLAV(I) = 1.0 !SQRT(MAX(0.0,XDSS(I)/A0SUM(I)))
        ENDIF
      ENDDO
      ntotmov = ntotmov + nmpert
      IF ( NewOptimumFound ) CALL Profile_Plot ! plot the profile
      NewOptimumFound = .FALSE.
      CALL SA_OUTPUT(T,FOPT,FPAV,FPSD,dxvav,xvsig,FLAV,nvar,Last_NUP,Last_NDOWN,ntotmov)
! If we have asked for an initial temperature to be calculated then do so
      IF (MAKET0) THEN
! Start temperature increased by 50% as asked for
        T = FPSD*1.5
        Initial_T = T
! Having done an SA cycle at T = 100000, the initial values are now randomised.
! If the user had requested the values supplied to be used, reset them.
        CALL MAKXIN
        MAKET0 = .FALSE.
        CALL ChiSqPlot_UpdateIterAndChiProBest(Curr_SA_Iteration)
        GOTO 100
      ENDIF
! If termination criteria are not met, prepare for another loop.
! We will use the energy fluctuation to reduce the temperature
      T = T/(1.0+(RT*T)/(3.0*FPSD))

! Rounding can cause this
      IF ( iMyExit.EQ.0 .AND. T .LE. 0.000001 ) THEN
          iMyExit = 4 ! Start next 
      ENDIF
   !   IF (MOD(Curr_SA_Iteration,5) .EQ. 0) CALL LocalMinimise(.TRUE.)

   !   CALL LocalMinimise(.TRUE.)

      DO I = 1, nvar
        x_unique(I) = XOPT(I)
      ENDDO
      F = FOPT
  999 CONTINUE ! This is where we jump to if the user pressed 'Stop' during the SA
               ! The variable imyexit has been set to 3, 4 or 5  (3 = stop SA, 4 = start next run, 5 = Edit)
               ! If we didn't jump to this point, just passed it, imyexit is 0
      CALL ChiSqPlot_UpdateIterAndChiProBest(Curr_SA_Iteration)
!  Check termination criteria.
!  Terminate SA if appropriate.
!      IF ((iMyExit .NE. 0) .OR. CheckTerm(NTOTMOV, AutoMinimise .AND. MOD(Curr_SA_Iteration,5) .EQ. 0)) THEN
      IF (iMyExit .EQ. 0) THEN
        tTestEarlyTerm = AutoMinimise .AND. tOptimumedSinceTET .AND. MOD(Curr_SA_Iteration,5) .EQ. 0
        IF ( .NOT. in_batch ) &
          tTestEarlyTerm = tTestEarlyTerm .AND. TestEarlyTermFlag
        IF ( tTestEarlyTerm ) tOptimumedSinceTET = .FALSE.
        IF (.NOT. CheckTerm(NTOTMOV, tTestEarlyTerm)) &
          GOTO 100 ! Next iteration
      ENDIF
      
      ENDIF ! Simplex only test
      
! End of a run in a multi-run. This is the place for a final local minimisation
      NumOf_SA_Runs = Curr_SA_Run
! Get AutoLocalMinimisation from the Configuration Window
      IF ( (iMyExit .NE. 5) .AND. ( AutoMinimise .OR. SimplexOnly ) ) THEN
        CALL LocalMinimise(.TRUE., .FALSE.)
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
! Temp save project
        CALL PrjReadWrite(tTmpSaveName, cWrite)
        GOTO 1 ! Next run
      ENDIF
! Delete temp saved project
      CALL IosDeleteFile(tTmpSaveName)

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
      REAL FUNCTION RANMAR()

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
      SUBROUTINE RMARInit_2(IJ_in,KL_in)
! RMARInit_2 and RANMAR_2 are another instances of RMARInit and RANMAR, by
! using a set of global varibles in common block raset2.
!
! See comments in RMARInit and RANMAR for details. 
!
      IMPLICIT NONE
      
      INTEGER, INTENT (IN   ) :: IJ_in, KL_in
      
      REAL            U,     C, CD, CM
      INTEGER                           I97, J97
      COMMON /raset2/ U(97), C, CD, CM, I97, J97

      INTEGER IJ, KL
      INTEGER ii, jj, i, j, k, l, m
      REAL    s, t

      IJ = ABS(IJ_in)
      IF ( IJ.GT.31328 ) IJ = ABS(31328 - MOD( IJ, 31328 ) )
      
      KL = ABS(KL_in)
      IF ( KL.GT.30081 ) KL = ABS(30081 - MOD( KL, 30081 ) )

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

      END SUBROUTINE RMARInit_2
!
!*****************************************************************************
!
      REAL FUNCTION RANMAR_2()

      IMPLICIT NONE

      REAL            U,     C, CD, CM
      INTEGER                           I97, J97
      COMMON /raset2/ U(97), C, CD, CM, I97, J97

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
      RANMAR_2 = uni

      END FUNCTION RANMAR_2

!*****************************************************************************
!
      SUBROUTINE RANX2Init
!
! Fills yran with an integrated Maxwell distribution that will be used to bias the step size during the SA
!
      IMPLICIT NONE

      REAL              xs, yran
      COMMON / x2eran / xs, yran(200)

      INTEGER I
      REAL    xi

      xs = 0.1
      DO i = 1, 200
        xi = xs*FLOAT(i-1)
        yran(i) = 1.0 - (0.5*xi*xi+xi+1.0)*EXP(-xi)
      ENDDO

      END SUBROUTINE RANX2Init
!
!*****************************************************************************
!
      SUBROUTINE RANX2E(DRIN, DROUT)

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: drin
      REAL, INTENT (  OUT) :: drout

      REAL              xs, yran
      COMMON / x2eran / xs, yran(200)

      REAL rin, xl, xsn, xm, xh, yl, yh, ym, xlo, xmo, xho, ylo, ymo, yho, rout
      INTEGER jn, j

! Use bisection to find the inverse distribution
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
      ym = 1.0 - (0.5*xm*xm+xm+1.0)*EXP(-xm)
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
      ym = 1.0 - (0.5*xm*xm+xm+1.0)*EXP(-xm)
      GOTO 15
   20 rout = xm
      IF (drin .LT. 0.0) rout = -rout
      drout = rout

      END SUBROUTINE RANX2E
!
!*****************************************************************************
!
      SUBROUTINE MAKXIN

      USE WINTERACTER
      USE dash_gui_resources
   
      IMPLICIT NONE

      INCLUDE 'params.inc' 

      INTEGER         nvar, NS, NT, iSeed1, iSeed2
      COMMON /sapars/ nvar, NS, NT, iSeed1, iSeed2

      REAL          RULB
      COMMON /RULB/ RULB(Mvar)

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      REAL, EXTERNAL :: RANMAR
      LOGICAL, EXTERNAL :: OutOfBounds
      REAL random_number
      REAL xtem, tempupper, templower, tempupper2
      INTEGER I, II


      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_Modal_input2)
      ENDIF

      DO I = 1, NVAR
        x_unique(I) = X_init(I)
      ENDDO
      IF ( RandomInitVal ) THEN
        DO II = 1, NPAR
          I = IP(II)
          x_unique(I) = LB(I) + RULB(I)*RANMAR()
!F          IF (ModalFlag(I) .EQ. 2 .OR. ModalFlag(I) .EQ. 3) THEN
!F! If modal ranges defined for torsions use random number to
!F! select from which range the value of XP will be derived.
!F            CALL AdjustSamplingForOtherRanges(x_unique(I), I, RANMAR())
!F          ENDIF
          IF ( ModalFlag(I) .EQ. 2 ) THEN ! Bimodal ranges
! Complicated-looking calcs of inbounds torsion angles is to try and guarantee a good
! sampling of all user defined ranges.  
            IF ( OutOfBounds(I, x_unique(I)) ) THEN
              IF ( UB(I)*LB(I) .LT. 0.0 ) THEN ! Range such as -170 to 170 defined                                                  
                TempLower = LB(I)              ! so use 0 to 360 degree scale
                TempUpper = UB(I)
                TempUpper2 = TempLower + 180.0
                CALL OneEightyToThreeSixty(TempUpper)
                CALL OneEightyToThreeSixty(TempUpper2)
                IF ( RANMAR() .LT. 0.5 ) THEN 
                  x_unique(I) = MAX(TempUpper, TempUpper2) + (RULB(I) * RANMAR())
                ELSE
                  x_unique(I) = MIN(TempUpper, TempUpper2) - (RULB(I) * RANMAR())
                ENDIF
              ELSE ! Range such as 30 to 90 degs or -30 to -90 defined
                x_unique(I) = LB(I) + (RULB(I)*RANMAR())
                IF (RANMAR() .LT. 0.5) x_unique(I) = -x_unique(I)
              ENDIF
              CALL ThreeSixtyToOneEighty(x_unique(I))
            ENDIF
          ELSE IF ( ModalFlag(I) .EQ. 3 ) THEN ! Trimodal ranges
            IF ( OutOfBounds(I, x_unique(I)) ) THEN ! calculate new value in one of three allowed ranges
              xtem = MINVAL(Tempbounds, MASK = Tempbounds .GE. 0.0) + RULB(I)*RANMAR() 
              random_number = RANMAR()
              IF ( (random_number .GT. 1.0/3.0) .AND. (random_number .LE. 2.0/3.0) ) THEN
                xtem = xtem - 120.0
              ELSEIF ( random_number .GT. 2.0/3.0 ) THEN
                xtem = xtem -240.0
                IF ( xtem .LT. -180.0 ) THEN
                  xtem = 360.0 + xtem
                ENDIF
              ENDIF
            ENDIF
            x_unique(I) = xtem
          ENDIF
        ENDDO
      ENDIF
      IF ( .NOT. IN_BATCH ) &
        CALL PopActiveWindowID

      END SUBROUTINE MAKXIN
!
!*****************************************************************************
!
      LOGICAL FUNCTION CheckTerm(Nmoves, TestLocalMin)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Nmoves
      LOGICAL, INTENT (IN   ) :: TestLocalMin

      INCLUDE 'params.inc'

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      REAL LMF, LMChiPro
      COMMON /Local_Minimise/LMF, LMChiPro

      REAL Best_CHI, LM_Best_CHI

      CheckTerm = .TRUE.

      IF (Is_SX) THEN
        Best_CHI = FOPT
      ELSE
        Best_CHI = CHIPROBEST
      ENDIF
      IF (Nmoves .GT. MaxMoves) &
        RETURN
      IF (Best_CHI .LT. (ChiMult*PAWLEYCHISQ)) &
        RETURN
      IF (TestLocalMin) THEN
        IF (Best_CHI .LT. (3*ChiMult*PAWLEYCHISQ)) THEN
          CALL LocalMinimise(.TRUE., .TRUE.)
          IF (Is_SX) THEN
            LM_Best_CHI = LMF
          ELSE
            LM_Best_CHI = LMChiPro
          ENDIF
          IF (LM_Best_CHI .LT. (ChiMult*PAWLEYCHISQ)) &
            RETURN
        ENDIF
      ENDIF

      CheckTerm =  .FALSE.

      END FUNCTION CheckTerm

      LOGICAL FUNCTION GoodSolution()

      IMPLICIT NONE

      INCLUDE 'params.inc'


      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST


      REAL Best_CHI

      GoodSolution = .TRUE.

      IF (Is_SX) RETURN
      GoodSolution = ( CHIPROBEST .LT. 10.0*PAWLEYCHISQ )

      END FUNCTION GoodSolution
!
!*****************************************************************************
!
      SUBROUTINE FillRULB(nvar)

! This subroutine determines the upper and lower bounds for all parameters including
! modal torsion angles.

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER, INTENT (INOUT) :: nvar

      REAL          RULB
      COMMON /RULB/ RULB(Mvar)

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                                        iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      REAL TempUpper, TempLower
      INTEGER I
      LOGICAL OneEightyScale

      DO I = 1, nvar
        RULB(I) = UB(I) - LB(I)
        IF (ModalFlag(I) .EQ. 2) THEN
        ! JvdS I think error here if e.g. LB = -5.0, UB = 95.0
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
      REAL FUNCTION GetMaxwellRandomNumber()

! Extracted from SimulatedAnnealing()
! This function returns a pseudo random number which made following
! a Maxwell distribution by calling to RANX2E().
! Note: before use this function, RANX2Init() MUST be called to initialise the arrays
! required by RANX2E(), and RMARInit_2() to initialise common block for RANMAR_2()

      IMPLICIT NONE

      REAL, EXTERNAL :: RANMAR_2
      REAL RANIN, RANAR1

      RANIN = 2.0 * RANMAR_2() - 1.0     ! RANIN  now between -1.0 and 1.0
      CALL RANX2E(RANIN,RANAR1)          ! RANAR1 now follows a Maxwell distribution
      GetMaxwellRandomNumber = RANAR1

      END FUNCTION GetMaxwellRandomNumber
!
!*****************************************************************************
!
      CHARACTER*20 FUNCTION GetSeed1SuffixString()

      IMPLICIT NONE

      INTEGER         nvar, NS, NT, iSeed1, iSeed2
      COMMON /sapars/ nvar, NS, NT, iSeed1, iSeed2

      CHARACTER*20, EXTERNAL :: Integer2String

      GetSeed1SuffixString = '_'//Integer2String(iSeed1)
      RETURN
      END FUNCTION GetSeed1SuffixString
!
!*****************************************************************************
!
