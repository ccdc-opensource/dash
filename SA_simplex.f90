!
!*****************************************************************************
!
      SUBROUTINE LocalMinimise(Auto)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: Auto

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION XOPT,       C,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), FOPT

      DOUBLE PRECISION x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL*4 XSIM(MVAR), DXSIM(MVAR)

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)

      INTEGER              iMyExit, num_new_min
      COMMON / CMN000001 / iMyExit, num_new_min

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
      COMMON /POSNS / NATOM, XATO(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL, EXTERNAL :: Get_AutoLocalMinimisation, Confirm, UseHydrogensDuringAuto
      CHARACTER*80 chistr
      INTEGER I, II, III, N
      REAL, EXTERNAL :: SA_FCN
      LOGICAL tAccept, tLOG_HYDROGENS
      REAL    FTEM
      DOUBLE PRECISION tX147, DFTEM

      IF (Auto .AND. (.NOT. Get_AutoLocalMinimisation())) RETURN
      CALL WCursorShape(CurHourGlass)
      tLOG_HYDROGENS = LOG_HYDROGENS
      IF (Auto .AND. UseHydrogensDuringAuto()) THEN
        LOG_HYDROGENS = .TRUE.
      ENDIF
      N = NPAR
      IF (PrefParExists) THEN
!O! Jvds 4 July 2002. Problem here: PO_PRECFC() uses X(MVAR), the current values, whereas we want to use 
!O! XOPT(MVAR), the best values so far.
!O! Quick fix...
        tX147 = X(iPrfPar)
        X(iPrfPar) = XOPT(iPrfPar)
        CALL PO_PRECFC
        IF ((UB(iPrfPar) - LB(iPrfPar)) .GT. 1.0D-3) N = NPAR - 1
      ENDIF
      DO II = 1, N
        I = IP(II)
        XSIM(II) = SNGL(XOPT(I))
! DXSIM = initial step sizes.
        DXSIM(II) = SA_SimplexDampingFactor*0.1*SNGL(UB(I) - LB(I))
      ENDDO
      CALL SA_SIMOPT(XSIM,DXSIM,N,FTEM)
      IF (Auto) THEN
        tAccept = .TRUE.
      ELSE
        chistr = 'Chi-squared = 0000.00'
        WRITE(chistr(15:21),'(F7.2)') FTEM
        tAccept = Confirm(CHISTR//CHAR(13)//'Press Yes to proceed with Simplex results.')
      ENDIF
      LOG_HYDROGENS = tLOG_HYDROGENS
      IF (tAccept) THEN
        DO II = 1, N
          I = IP(II)
          XOPT(I) = DBLE(XSIM(II))
          X(I) = DBLE(XSIM(II))
        ENDDO
        FOPT = DBLE(FTEM)
        CALL WDialogSelect(IDD_SA_Action1)
        CALL WDialogPutReal(IDF_min_chisq,SNGL(FOPT),'(F8.2)')
        FOPT = -FOPT
        DO II = 1, NATOM
          DO III = 1, 3
            XAtmCoords(III,II,Curr_SA_Run) = XATO(III,II)
          ENDDO
        ENDDO
        CALL valchipro(CHIPROBEST)
        num_new_min = num_new_min + 1
        CALL WDialogSelect(IDD_SA_Action1)
        CALL WDialogPutReal(IDF_profile_chisq2,CHIPROBEST,'(F8.2)')
  !U      CALL WDialogSelect(IDD_Parameter_Status)
  !U      DO i = 1, nvar
  !U        CALL WGridPutCellReal(IDF_CPL_grid,1,i,SNGL(xopt(i)),'(F12.5)')
  !U        DO icol = 2, 7
  !U          CALL WGridClearCell(IDF_CPL_grid,icol,i)
  !U        ENDDO
  !U      ENDDO
      ELSE
        IF (PrefParExists) THEN
          X(iPrfPar) = tX147
          CALL PO_PRECFC
          CALL FCN(X,DFTEM,0)
        ENDIF
      ENDIF
      CALL WCursorShape(CurCrossHair)

      END SUBROUTINE LocalMinimise
!
!*****************************************************************************
!
