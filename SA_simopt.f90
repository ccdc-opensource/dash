!
!*****************************************************************************
!
! Avoid using REAL function type, as accidentally CALL a real function 
! may lead to FPU stack corruption in Intel Fortran (IF10).
!
! For Auto, if set TestOnlyForAuto, global CHIPROBEST, FOPT and XOPT are not updated.
! LMF and LMChiPro are made available through Local_Minimise common block.
      SUBROUTINE LocalMinimise(Auto, TestOnlyForAuto)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: Auto, TestOnlyForAuto

      INCLUDE 'params.inc'

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      REAL          RULB
      COMMON /RULB/ RULB(Mvar)

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)

      INTEGER              iMyExit 
      LOGICAL                       NewOptimumFound, WasMinimised, TestEarlyTermFlag
      COMMON / CMN000001 / iMyExit, NewOptimumFound, WasMinimised, TestEarlyTermFlag

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

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

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      REAL LMF, LMChiPro
      COMMON /Local_Minimise/LMF, LMChiPro

      LOGICAL, EXTERNAL :: Confirm
      REAL, EXTERNAL :: SA_FCN
      CHARACTER*80 chistr
      INTEGER I, II, III, N
      LOGICAL tAccept, tLOG_HYDROGENS, tUpdate
      REAL    DFTEM
      LOGICAL DesorbHydrogens
      REAL    XSIM(MVAR), DXSIM(MVAR)

      IF (Auto .AND. (.NOT. AutoMinimise)) RETURN
 
      IF ( .NOT. IN_BATCH ) &
        CALL WCursorShape(CurHourGlass)

      tLOG_HYDROGENS = LOG_HYDROGENS
      DesorbHydrogens = .FALSE.
      IF (Auto .AND. UseHAutoMin) THEN
        LOG_HYDROGENS = .TRUE.
!C If we have absorbed the hydrogens and want to use them now, we must do some re-administrating.
        IF (HydrogenTreatment .EQ. 2) THEN
          DesorbHydrogens = .TRUE.
          CALL create_fob(.FALSE.)
        ENDIF
      ENDIF
      N = NPAR
      DO II = 1, N
        I = IP(II)
        XSIM(II) = XOPT(I)
!C DXSIM = initial step sizes.
        DXSIM(II) = SA_SimplexDampingFactor*0.1*RULB(I)
      ENDDO
      CALL SA_SIMOPT(XSIM,DXSIM,N,LMF)
      IF (Auto) THEN
        tAccept = .TRUE.
        tUpdate = .NOT. TestOnlyForAuto
      ELSE
        chistr = 'Chi-squared = 0000.00'
        WRITE(chistr(15:21),'(F7.2)') LMF
        tAccept = Confirm(CHISTR//CHAR(13)//'Press Yes to proceed with Simplex results.')
        tUpdate = tAccept
      ENDIF
      LOG_HYDROGENS = tLOG_HYDROGENS
      IF (DesorbHydrogens) CALL create_fob(.TRUE.)
      IF (tAccept) THEN
        IF (.NOT. Auto) WasMinimised = .TRUE.
        DO II = 1, NATOM
          DO III = 1, 3
            XAtmCoords(III,II,Curr_SA_Run) = XATO(III,II)
          ENDDO
        ENDDO
        CALL valchipro(LMChiPro)
        IF (tUpdate) THEN
          DO II = 1, N
            I = IP(II)
            XOPT(I) = XSIM(II)
            x_unique(I) = XSIM(II)
          ENDDO
          FOPT = LMF
          CHIPROBEST = LMChiPro
          NewOptimumFound = .TRUE.
          IF ( .NOT. IN_BATCH ) THEN
            CALL SelectDASHDialog(IDD_SA_Action1)
            CALL WDialogPutReal(IDF_min_chisq, FOPT, '(F8.2)')
            CALL WDialogPutReal(IDF_profile_chisq2, CHIPROBEST, '(F8.2)')
            CALL SelectDASHDialog(IDD_Summary)
            CALL WGridPutCellReal(IDF_SA_Summary, 4, Curr_SA_Run, CHIPROBEST, '(F7.2)')
            CALL WGridPutCellReal(IDF_SA_Summary, 5, Curr_SA_Run, FOPT, '(F7.2)')
    !U      CALL SelectDASHDialog(IDD_Parameter_Status)
    !U      DO i = 1, nvar
    !U        CALL WGridPutCellReal(IDF_CPL_grid,1,i,xopt(i),'(F12.5)')
    !U        DO icol = 2, 7
    !U          CALL WGridClearCell(IDF_CPL_grid,icol,i)
    !U        ENDDO
    !U      ENDDO
          ENDIF
        ENDIF
      ENDIF
      IF ((.NOT. (tAccept .AND. tUpdate)) .AND. PrefParExists) THEN
        CALL PO_PRECFC(x_unique(iPrfPar))
        CALL FCN(x_unique,DFTEM,0,.FALSE.)
      ENDIF

      IF ( .NOT. IN_BATCH ) &
        CALL WCursorShape(CurCrossHair)

      END SUBROUTINE LocalMinimise
!
!*****************************************************************************
!
      REAL FUNCTION SA_FCN(N,P)

      USE PO_VAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      REAL P(MVAR)
      INTEGER N

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL DBLEP(MVAR)
      REAL CHIANS
      INTEGER I, II

      DO I = 1, NVAR
        DBLEP(I) = XOPT(I)
      ENDDO
      DO II = 1, N
        I = IP(II)
        DBLEP(I) = P(II)
      ENDDO
      IF (PrefParExists) CALL PO_PRECFC(DBLEP(iPrfPar))
      CALL FCN(DBLEP, CHIANS, 0, .FALSE.)
      SA_FCN = CHIANS

      END FUNCTION SA_FCN
!
!*****************************************************************************
!
      SUBROUTINE SA_SIMOPT(X,DX,N,TheChiSqd)
!
! Purpose
!     A SIMPLEX-based routine which optimises the values of the N
! parameters pertaining to the components of the vector X
!
! Parameters
!   ARGUMENT  TYPE  I/O  DIMENSION  DESCRIPTION
!    N        I*4    I       -      No. of parameters to be optimised.
!    X        R*4    I       N      Initial guess.
!    X        R*4    O       N      The optimal answer.
!    DX       R*4    I       N      Initial step-lengths for X.
!
! Other Requirements
!     The user must provide a FUNCTION SA_FCN(N,X) which evalutes
! Chi-squared (un-normalised) given the vector X(N).
!
! History
!     D. S. Sivia    9 Feb 1995  Initial release.
!
      INCLUDE 'params.inc'

      REAL, INTENT (  OUT) :: TheChiSqd

      INTEGER     MAXITR
      PARAMETER ( MAXITR = MVAR )
      REAL X(*), DX(*)
      REAL V((MVAR+1)*MAXITR), EX(3*MVAR), C(MVAR+1)
      INTEGER IR(MVAR+1) ! Maximum index I could find: N+1. Maximum value for N should be MVAR
                         ! IR seems to hold a sorted list of pointers into C
      EXTERNAL SA_FCN
      
      INTEGER iPass

      iPass = 0
   10 CONTINUE
      CALL VCOPY(X,V,N)
      CHIMIN = SA_FCN(N,V)
      ZERO = 0.0
      ITER = 0
    1 ITER = ITER + 1
      IF (ITER.GT.MAXITR) THEN
   !     CALL DebugErrorMessage('ITER.GT.MAXITR in SA_SIMOPT')
        GOTO 999
      ENDIF
      CALL SASIMPLEX(V,N,DX,EX,C,IR,N*1000)
      CALL VCOPY(EX,V,N)
      CHI = SA_FCN(N,V)
! Convergence criterion
      IF (CHI .GT. CHIMIN) THEN
        CALL DebugErrorMessage('CHI .GT. CHIMIN in SA_SIMOPT')
        GOTO 1
      ENDIF
      IF ((1.0-CHI/CHIMIN) .GT. 0.00001) THEN
        CHIMIN = CHI
        GOTO 1
      ENDIF
  999 CALL VCOPY(V,X,N)
      TheChiSqd = CHI
      iPass = iPass + 1
      IF (iPass .LT. 3) GOTO 10 ! Repeat 3 times

      END SUBROUTINE SA_SIMOPT
!
!*****************************************************************************
!
      SUBROUTINE SASIMPLEX(V,N,D,EX,C,IR,MX)

      REAL V(N,*), EX(N,*), C(*), D(*)
      INTEGER IR(*)
      LOGICAL SLOW
      DATA ALPHA, BETA, GAMA/1.0, 0.5, 2.0/

      SLOW = .FALSE.
      N3 = 3*N
      CAIM = 0.0
      C(1) = SA_FCN(N,V(1,1))
      CALL SASIMP0(V,D,C,IR,N)
      CMIN = C(IR(N+1))
      ITER = 0
      NOLUCK = 0
      IRSTRT = 0
   10 ITER = ITER + 1
      NOLUCK = NOLUCK + 1
      IF (ITER.GE.MX .OR. NOLUCK.GT.100) THEN
        CALL SSORT(C,IR,N)
        CALL VCOPY(V(1,IR(N+1)),EX,N)
        IF (ITER.LT.250) THEN
          NOLUCK = 0
          DO ID = 1, N
!O            D(ID) = D(ID)/10.0
            D(ID) = D(ID)/2.0
          ENDDO
          CALL VCOPY(EX,V,N)
          C(1) = SA_FCN(N,V(1,1))
          CALL SASIMP0(V,DSMALL,C,IR,N)
          CMIN = C(IR(N+1))
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL VRFILL(EX,0.,N3)
      CALL XCENT(V,EX(1,3),IR(1),N)
      CALL XZERO(V(1,IR(1)),EX,EX(1,3),N,ALPHA)
      C0 = SA_FCN(N,EX)
      CL = C(IR(N+1))
      CH = C(IR(1))
      CS = C(IR(2))
      IF (C0.LT.CL) THEN
        CALL SAEXPAND(CL,EX,EX(1,2),EX(1,3),N,C0,GAMA)
      ELSEIF (C0.GT.CS) THEN
        CALL SACONTRACT(CH,CS,C0,C00,EX,EX(1,2),EX(1,3),V(1,IR(1)),BETA,N)
        IF (C00.LT.CH .AND. C00.LT.C0) THEN
          CALL VCOPY(EX(1,2),EX,N)
          C0 = C00
        ELSE
          CALL SACONT2(IR(N+1),V,C,N,IR)
          IRSTRT = IRSTRT + 1
          IF (C(IR(N+1)).LT.CMIN) CMIN = C(IR(N+1))
          IF (IRSTRT.GE.5) THEN
            NOLUCK = 0
            CALL SSORT(C,IR,N)
            CALL VCOPY(V(1,IR(N+1)),EX,N)
            RETURN
          ENDIF
        ENDIF
      ENDIF
      CALL VCOPY(EX,V(1,IR(1)),N)
      C(IR(1)) = C0
      CALL SSORT(C,IR,N)
      IF (C(IR(N+1)).LT.CMIN) THEN
        DROP = (CMIN-C(IR(N+1)))/CMIN
        IF (ABS(DROP).LT.1.0E-4) SLOW = .TRUE.
        NOLUCK = 0
        CMIN = C(IR(N+1))
      ENDIF
      IF (CMIN.GT.CAIM .AND. .NOT.SLOW) GOTO 10
      CALL VCOPY(V(1,IR(N+1)),EX,N)

      END SUBROUTINE SASIMPLEX
!
!*****************************************************************************
!
      SUBROUTINE SASIMP0(V,D,C,IR,N)

      REAL V(N,*), C(*), D(*)
      INTEGER IR(*)

      DO I = 2, N + 1
        CALL VCOPY(V,V(1,I),N)
        V(I-1,I) = V(I-1,I) + D(I-1)
        C(I) = SA_FCN(N,V(1,I))
      ENDDO
      CALL SSORT(C,IR,N)

      END SUBROUTINE SASIMP0
!
!*****************************************************************************
!
      SUBROUTINE SAEXPAND(CL,X0,X00,XC,N,C0,G)

      REAL X0(*), X00(*), XC(*)

      DO I = 1, N
        X00(I) = G*(X0(I)-XC(I)) + XC(I)
      ENDDO
      C00 = SA_FCN(N,X00)
      IF (C00.LT.CL) THEN
        CALL VCOPY(X00,X0,N)
        C0 = C00
      ENDIF

      END SUBROUTINE SAEXPAND
!
!*****************************************************************************
!
      SUBROUTINE SACONTRACT(CH,CS,C0,C00,X0,X00,XC,XH,B,N)

      REAL X0(*), X00(*), XC(*), XH(*)

      IF (C0.LT.CH) THEN
        DO I = 1, N
          X00(I) = B*(X0(I)-XC(I)) + XC(I)
        ENDDO
      ELSE
        DO I = 1, N
          X00(I) = B*(XH(I)-XC(I)) + XC(I)
        ENDDO
      ENDIF
      C00 = SA_FCN(N,X00)

      END SUBROUTINE SACONTRACT
!
!*****************************************************************************
!
      SUBROUTINE SACONT2(K,V,C,N,IR)

      REAL V(N,*), C(*)
      INTEGER IR(*)

      DO J = 1, N + 1
        IF (J.EQ.K) GOTO 20
        DO I = 1, N
          V(I,J) = 0.5*(V(I,J)+V(I,K))
          C(J) = SA_FCN(N,V(1,J))
        ENDDO
   20 ENDDO
      CALL SSORT(C,IR,N)

      END SUBROUTINE SACONT2
!
!*****************************************************************************
!
