!
!*****************************************************************************
!
      INTEGER FUNCTION WriteSAParametersToFile(TheFileName)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE PO_VAR
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER tFileHandle, I, kk, iFrg, ilen, II, Fixed
      REAL    R, x, lb, ub
      CHARACTER(MaxPathLength) tSDIFile
      CHARACTER*17 DateStr
      REAL    MaxMoves1, tMaxMoves
      INTEGER MaxMoves2
      CHARACTER*20, EXTERNAL :: Integer2String
      CHARACTER*20 MaxMovesStr
      INTEGER, EXTERNAL :: DateToday
      CHARACTER*36 tString36

      WriteSAParametersToFile = 1 ! Error
      CALL PushActiveWindowID
      tFileHandle = 10
      OPEN(tFileHandle,FILE=TheFileName,ERR=999)
      WRITE(tFileHandle,'("  Parameters for simulated annealing in ",A8)',ERR=999) ProgramVersion
      CALL Date2String(DateToday(),DateStr)
      WRITE(tFileHandle,'(A)',ERR=999) "  Date = "//DateStr(1:LEN_TRIM(DateStr))
      CALL WDialogSelect(IDD_SAW_Page1)
      CALL WDialogGetString(IDF_SA_Project_Name,tSDIFile)
      WRITE(tFileHandle,'("  SDI file = ",A)',ERR=999) tSDIFile(1:LEN_TRIM(tSDIFile))
! Profile range (including maximum resolution), wavelength, unit cell parameters, zero point

      CALL WDialogSelect(IDD_SA_input2)
      kk = 0
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
! Write the name of the file
          ilen = LEN_TRIM(frag_file(iFrg))
          WRITE(tFileHandle,'(A)',ERR=999) '  Z-matrix '//frag_file(iFrg)(1:ilen)
          WRITE(tFileHandle,'(A,I2)',ERR=999) ' Number of copies : ',  zmNumberOfCopies(iFrg)
          DO ii = 1, izmpar(ifrg)
            kk = kk + 1
            ilen = LEN_TRIM(czmpar(ii,iFrg))
            CALL WGridGetCellReal    (IDF_parameter_grid,1,kk,x)
            CALL WGridGetCellCheckBox(IDF_parameter_grid,4,kk,Fixed)
            IF (Fixed .EQ. 1) THEN
              WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,A5)",ERR=999) czmpar(ii,iFrg),x,'Fixed'
            ELSE
              CALL WGridGetCellReal(IDF_parameter_grid,2,kk,lb)
              CALL WGridGetCellReal(IDF_parameter_grid,3,kk,ub)
              WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,F12.5,1X,F12.5)",ERR=999) czmpar(ii,iFrg),x,lb,ub
            ENDIF
          ENDDO
        ENDIF
      ENDDO
! Preferred Orientation
      IF (PrefParExists) THEN
        WRITE(tFileHandle,'("  March-Dollase preferred orientation correction will be applied.")',ERR=999)
        WRITE(tFileHandle,'("  Orientation: a* = ",F6.3,", b* = ",F6.3,", c* = ",F6.3)',ERR=999) (PrefPars(ii),ii=1,3)
        kk = kk + 1
        CALL WGridGetCellReal(IDF_parameter_grid,2,kk,lb)
        CALL WGridGetCellReal(IDF_parameter_grid,3,kk,ub)
        CALL WGridGetCellReal(IDF_parameter_grid,1,kk,x)
        tString36 = 'Preferred Orientation'
        WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,F12.5,1X,F12.5)",ERR=999) tString36,x,lb,ub
      ENDIF
! Total number of parameters for this problem
! Number of atoms
      CALL WDialogSelect(IDD_SA_input3)
      CALL WDialogGetInteger(IDF_SA_RandomSeed1,I)
      WRITE(tFileHandle,'("  Random seed 1 = ",I5)',ERR=999) I
      CALL WDialogGetInteger(IDF_SA_RandomSeed2,I)
      WRITE(tFileHandle,'("  Random seed 2 = ",I5)',ERR=999) I
      CALL WDialogGetReal(IDF_SA_T0,R)
      IF (R .EQ. 0.0) THEN
        WRITE(tFileHandle,'("  Initial temperature = to be estimated by DASH")',ERR=999)
      ELSE
        WRITE(tFileHandle,'("  Initial temperature = ",F9.2)',ERR=999) R
      ENDIF
      CALL WDialogGetReal(IDF_SA_Tredrate,R)
      WRITE(tFileHandle,'("  Cooling rate = ",F8.4)',ERR=999) R
      CALL WDialogGetInteger(IDF_SA_NS,I)
      WRITE(tFileHandle,'("  N1 = ",I5)',ERR=999) I
      CALL WDialogGetInteger(IDF_SA_NT,I)
      WRITE(tFileHandle,'("  N2 = ",I5)',ERR=999) I
      CALL WDialogGetInteger(IDF_SA_Moves,I)
      WRITE(tFileHandle,'("  Number of moves at each temperature = ",I5)',ERR=999) I
      CALL WDialogGetInteger(IDF_SA_MaxRepeats,I) ! Number of runs
      IF (I .EQ. 1) THEN
        WRITE(tFileHandle,'("  Single run, runs until user stops it")',ERR=999)
      ELSE
        WRITE(tFileHandle,'("  Number of runs = ",I5)',ERR=999) I
        CALL WDialogGetReal(IDF_MaxMoves1,MaxMoves1)
        IF (MaxMoves1 .LT.   0.001) MaxMoves1 =   0.001
        IF (MaxMoves1 .GT. 100.0  ) MaxMoves1 = 100.0
        CALL WDialogGetInteger(IDF_MaxMoves2,MaxMoves2)
        IF (MaxMoves2 .LT. 1) MaxMoves2 = 1
        IF (MaxMoves2 .GT. 8) MaxMoves2 = 8
        tMaxMoves = MaxMoves1 * (10**FLOAT(MaxMoves2))
        IF (tMaxMoves .LT. 10.0) tMaxMoves = 10.0
        IF (tMaxMoves .GT.  2.0E9) tMaxMoves = 2.0E9
        I = NINT(tMaxMoves)
        MaxMovesStr = Integer2String(I)
        ilen = LEN_TRIM(MaxMovesStr)
! 1000000000 ==> 1000000,000
        IF (ilen .GT. 3) THEN
          MaxMovesStr(ilen-1:ilen+1) = MaxMovesStr(ilen-2:ilen)
          MaxMovesStr(ilen-2:ilen-2) = ','
        ENDIF
! 1000000,000 ==> 1000,000,000
        IF (ilen .GT. 6) THEN
          MaxMovesStr(ilen-4:ilen+2) = MaxMovesStr(ilen-5:ilen+1)
          MaxMovesStr(ilen-5:ilen-5) = ','
        ENDIF
! 1000,000,000 ==> 1,000,000,000
        IF (ilen .GT. 9) THEN
          MaxMovesStr(ilen-7:ilen+3) = MaxMovesStr(ilen-8:ilen+2)
          MaxMovesStr(ilen-8:ilen-8) = ','
        ENDIF
        WRITE(tFileHandle,'("  Maximum number of moves per run = ",A)',ERR=999) MaxMovesStr(1:LEN_TRIM(MaxMovesStr))
        CALL WDialogGetReal(IDF_SA_ChiTest,R)
        WRITE(tFileHandle,'("  A run will stop when the profile chi² is less than ",   &
                F6.2," · ",F7.3," = ",F8.4)',ERR=999) R, PAWLEYCHISQ, R*PAWLEYCHISQ
      ENDIF

      CLOSE(tFileHandle)
      CALL PopActiveWindowID
      WriteSAParametersToFile = 0 ! success
      RETURN
  999 CALL ErrorMessage('Could not access temporary file.')
      CLOSE(tFileHandle)
      CALL PopActiveWindowID

      END FUNCTION WriteSAParametersToFile
!
!*****************************************************************************
!
      SUBROUTINE ViewZmatrix(iFrg)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER I
      CHARACTER(MaxPathLength) temp_file
      REAL*8 CART(1:3,1:MAXATM)
      INTEGER, EXTERNAL :: WriteMol2
      LOGICAL, EXTERNAL :: Get_ColourFlexibleTorsions
      INTEGER atom
      INTEGER Element
      INTEGER NumOfFlexTorsions
      INTEGER tLength, BondNr
      INTEGER, EXTERNAL :: ElmSymbol2CSD

      natcry = NATOMS(iFrg)
      CALL MAKEXYZ_2(natcry,BLEN(1,iFrg),ALPH(1,iFrg),BET(1,iFrg),IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),CART)
      DO I = 1, natcry
        axyzo(I,1) = SNGL(CART(1,I))
        axyzo(I,2) = SNGL(CART(2,I))
        axyzo(I,3) = SNGL(CART(3,I))
        aelem(I) = ElmSymbol2CSD(asym(I,iFrg)(1:2))
        atomlabel(I) = OriginalLabel(I,iFrg)
      ENDDO
      nbocry = NumberOfBonds(iFrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr,iFrg)
        bond(BondNr,1) = Bonds(1,BondNr,iFrg)
        bond(BondNr,2) = Bonds(2,BondNr,iFrg)
      ENDDO
! Q & D hack to display flexible torsion angles in different colors by forcing different
! element types.
      IF (Get_ColourFlexibleTorsions() .AND. (natcry.GE.4)) THEN
        DO I = 1, natcry
          aelem(I) = 1       ! Carbon        Grey
        ENDDO
        NumOfFlexTorsions = 0
        DO atom = 4, natcry
          IF (ioptt(atom,iFrg) .EQ. 1) THEN
            NumOfFlexTorsions = NumOfFlexTorsions + 1
            SELECT CASE(NumOfFlexTorsions)
              CASE (1)
                Element = 23 ! Cobalt        Blue
              CASE (2)
                Element = 64 ! Oxygen        Red
              CASE (3)
                Element = 81 ! Sulphur       Yellow
              CASE (4)
                Element = 21 ! Chlorine      Green
              CASE (5)
                Element = 56 ! Nitrogen      Light blue
              CASE (6)
                Element = 16 ! Bromine       Brown
              CASE (7)
                Element = 43 ! Iodine        Pink
              CASE (8)
                Element =  2 ! Hydrogen      White
              CASE (9)
                Element = 66 ! Phosphorus
            END SELECT
            aelem(atom) = Element
            aelem(IZ1(atom,iFrg)) = Element
            aelem(IZ2(atom,iFrg)) = Element
            aelem(IZ3(atom,iFrg)) = Element
          ENDIF
        ENDDO
      ENDIF
      tLength = LEN_TRIM(frag_file(iFrg))
      temp_file = frag_file(iFrg)(1:tLength-8)//'_temp.mol2'
! Show the mol2 file
      IF (WriteMol2(temp_file,.TRUE.,iFrg) .EQ. 1) THEN
        CALL ViewStructure(temp_file)
      ELSE
        CALL DebugErrorMessage('Error writing temporary file.')
      ENDIF
      CALL IOSDeleteFile(temp_file)

      END SUBROUTINE ViewZmatrix
!
!*****************************************************************************
!
      SUBROUTINE SA_Parameter_Set

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PO_VAR
      USE ZMVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      DOUBLE PRECISION x,       lb,       ub,       vm
      COMMON /values/  x(mvar), lb(mvar), ub(mvar), vm(mvar)

      DOUBLE PRECISION prevub,       prevlb       ! For saving the previous range
      COMMON /pvalues/ prevub(mvar), prevlb(mvar)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER         NStPar
      COMMON /pextra/ NStPar

      DOUBLE PRECISION T0, rt
      COMMON /saparl/  T0, rt

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      CHARACTER*36 parlabel(mvar)
      DOUBLE PRECISION dcel(6)
      INTEGER I, II, kk, iFrg, iFrgCopy, tk, iH, iK, iL
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, Get_UseCrystallographicCoM
      REAL    tLattice(1:3,1:3)
      REAL    Beta_m, Alpha_m, q0m, q1m, q2m, q3m
      REAL    Length
      INTEGER iAxis, iAtmNr
      REAL*8  CART(1:3,1:MAXATM)
      REAL    Origin(1:3)
      REAL    Point1(1:3), Point2(1:3), Point3(1:3) 
      REAL    zmSingleRotationAxis(1:3)

      CALL PushActiveWindowID
! Calculate the unit cell axes in terms of the orthogonal lattice from
! the unit cell parameters
      CALL LatticeCellParameters2Lattice(CellPar(1), CellPar(2), CellPar(3), &
                                         CellPar(4), CellPar(5), CellPar(6), tLattice)
      DO I = 1, 6
        dcel(I) = DBLE(CellPar(I))
      ENDDO
      f2cmat = DBLE(tLattice)
! Calculate the reciprocal lattice
      CALL DGMINV(f2cmat,c2fmat,3)
      CALL frac2pdb(f2cpdb,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
      CALL CREATE_FOB
      CALL Create_AtomicWeightings
! Per Z-matrix, determine whether to use quaternions or a single axis
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          CALL zmDoAdmin(iFrg)
          IF (.NOT. UseQuaternions(iFrg)) THEN
! Initialise the parts of the quaternions of the single rotation axis that are due to the axis
            SELECT CASE (zmSingleRotAxDef(iFrg))
              CASE (1) ! to atom number
! We need the Cartesian co-ordinates of this atom
                CALL MAKEXYZ_2(natoms(iFrg),BLEN(1,iFrg),ALPH(1,iFrg),BET(1,iFrg),IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),CART)
! Now we need the co-ordinates of the origin of the rotations
                IF (icomflg(iFrg) .EQ. 0) THEN ! C.O.M.
                  Origin(1) = 0.0
                  Origin(2) = 0.0
                  Origin(3) = 0.0
                  IF (Get_UseCrystallographicCoM()) THEN
                    DO iAtmNr = 1, natoms(iFrg)
                      Origin(1) = Origin(1) + AtomicWeighting(iAtmNr,iFrg) * SNGL(CART(1,iAtmNr))
                      Origin(2) = Origin(2) + AtomicWeighting(iAtmNr,iFrg) * SNGL(CART(2,iAtmNr))
                      Origin(3) = Origin(3) + AtomicWeighting(iAtmNr,iFrg) * SNGL(CART(3,iAtmNr))
                    ENDDO
                  ELSE
                    DO iAtmNr = 1, natoms(iFrg)
                      Origin(1) = Origin(1) + SNGL(CART(1,iAtmNr))
                      Origin(2) = Origin(2) + SNGL(CART(2,iAtmNr))
                      Origin(3) = Origin(3) + SNGL(CART(3,iAtmNr))
                    ENDDO  
                  ENDIF
                  Origin(1) = Origin(1) / natoms(iFrg)
                  Origin(2) = Origin(2) / natoms(iFrg)
                  Origin(3) = Origin(3) / natoms(iFrg)
                ELSE
                  Origin(1) = SNGL(CART(1,icomflg(iFrg)))
                  Origin(2) = SNGL(CART(2,icomflg(iFrg)))
                  Origin(3) = SNGL(CART(3,icomflg(iFrg)))
                ENDIF
                zmSingleRotationAxis(1) = SNGL(CART(1,zmSingleRotAxAtm(iFrg))) - Origin(1)
                zmSingleRotationAxis(2) = SNGL(CART(2,zmSingleRotAxAtm(iFrg))) - Origin(2)
                zmSingleRotationAxis(3) = SNGL(CART(3,zmSingleRotAxAtm(iFrg))) - Origin(3)
              CASE (2) ! Fractional
! The variable zmSingleRotationAxis holds the fractional co-ordinates,
! we need orthogonal co-ordinates => convert
                zmSingleRotationAxis(1) = zmSingleRotAxFrac(1,iFrg)*tLattice(1,1) +     &
                                               zmSingleRotAxFrac(2,iFrg)*tLattice(1,2) +     &
                                               zmSingleRotAxFrac(3,iFrg)*tLattice(1,3)
                zmSingleRotationAxis(2) = zmSingleRotAxFrac(1,iFrg)*tLattice(2,1) +     &
                                               zmSingleRotAxFrac(2,iFrg)*tLattice(2,2) +     &
                                               zmSingleRotAxFrac(3,iFrg)*tLattice(2,3)
                zmSingleRotationAxis(3) = zmSingleRotAxFrac(1,iFrg)*tLattice(3,1) +     &
                                               zmSingleRotAxFrac(2,iFrg)*tLattice(3,2) +     &
                                               zmSingleRotAxFrac(3,iFrg)*tLattice(3,3)
              CASE (3) ! Normal to plane defined by three atoms
! We need the Cartesian co-ordinates of these atoms
                CALL MAKEXYZ_2(natoms(iFrg),BLEN(1,iFrg),ALPH(1,iFrg),BET(1,iFrg),IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),CART)
                Point1 = SNGL(CART(:,zmSingleRotAxAtms(1,iFrg)))
                Point2 = SNGL(CART(:,zmSingleRotAxAtms(2,iFrg)))
                Point3 = SNGL(CART(:,zmSingleRotAxAtms(3,iFrg)))
                Point1 = Point1 - Point2
                Point3 = Point3 - Point2
                CALL VectorCrossProduct(Point1,Point3,zmSingleRotationAxis(1))
            END SELECT
! Normalise the axis
            Length = SQRT(zmSingleRotationAxis(1)**2 + &
                          zmSingleRotationAxis(2)**2 + &
                          zmSingleRotationAxis(3)**2     )
            DO iAxis = 1, 3
              zmSingleRotationAxis(iAxis) = zmSingleRotationAxis(iAxis) / Length
              IF (zmSingleRotationAxis(iAxis) .GT.  0.99999) zmSingleRotationAxis(iAxis) =  0.99999
              IF (zmSingleRotationAxis(iAxis) .LT. -0.99999) zmSingleRotationAxis(iAxis) = -0.99999
            ENDDO
! Calculate the orientation of the axis
! Note: Alpha_m and Beta_m in radians
            Beta_m  = ACOS(zmSingleRotationAxis(3))
            IF (ABS(zmSingleRotationAxis(3)) .GT. 0.99998) THEN
! The axis coincides with the z-axis, so alpha becomes undefined: set alpha to 0.0
              Alpha_m = 0.0
! It turns out that we can get problems with rounding errors here
            ELSE IF ((-zmSingleRotationAxis(2)/SIN(Beta_m)) .GT.  0.99999) THEN
              Alpha_m = 0.0
            ELSE IF ((-zmSingleRotationAxis(2)/SIN(Beta_m)) .LT. -0.99999) THEN
              Alpha_m = PI
            ELSE
              Alpha_m = ACOS(-zmSingleRotationAxis(2)/SIN(Beta_m))
              IF ((ASIN((zmSingleRotationAxis(1)))/SIN(Beta_m)) .LT. 0.0) Alpha_m = TWOPI - Alpha_m
            ENDIF
! It's an axis, so Gamma_m can be set to 0.0
            q0m = COS(0.5*Beta_m) * COS(0.5*Alpha_m)
            q1m = SIN(0.5*Beta_m) * COS(0.5*Alpha_m)
            q2m = SIN(0.5*Beta_m) * SIN(0.5*Alpha_m)
            q3m = COS(0.5*Beta_m) * SIN(0.5*Alpha_m)
! Now we know the quaternions which, when applied, would give the direction of the axis of rotation.
! In order for this axis to be aligned with the z-axis, we must apply the inverse of this rotation.
! Hence we want:
! 1. apply inverse of rotation of axis
! 2. apply rotation about the single axis, which now coincides with z
! 3. apply rotation of axis to recover the original orientation of the molecule (rotated about the single axis)
!
! {1*q0m + i*q1m + j*q2m + k*q3m} * {1*q0a + k*q3a} * {1*q0m - i*q1m - j*q2m - k*q3m} =
!
! {1*q0a + i*q3a*2*(q1m*q3m+q0m*q2m) + j*q3a*2*(q2m*q3m-q0m*q1m) + k*q3a*((q0m**2)-(q1m**2)-(q2m**2)+(q3m**2))}
!
! q0a and q3a are the parameters that are varied during the SA and cannot be multiplied in until 
! the actual evaluation of chi-squared. The other factors depend on the orientation of the axis
! only, which is known:
            zmSingleRotationQs(0,iFrg) = DBLE(1.0)
            zmSingleRotationQs(1,iFrg) = DBLE(2.0 * (q1m*q3m + q0m*q2m))
            zmSingleRotationQs(2,iFrg) = DBLE(2.0 * (q2m*q3m - q0m*q1m))
            zmSingleRotationQs(3,iFrg) = DBLE((q0m**2) - (q1m**2) - (q2m**2) + (q3m**2))
          ENDIF
        ENDIF
      ENDDO
      tk = 0
      kk = 0
      TotNumZMatrices = 0
! JCC Run through all possible fragments
      DO iFrg = 1, maxfrg
! Only include those that are now checked
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            TotNumZMatrices = TotNumZMatrices + 1
            DO ii = 1, izmpar(iFrg)
              kk = kk + 1
              zm2Par(ii,iFrgCopy,iFrg) = kk
              Par2iFrg(kk)     = iFrg
              Par2iFrgCopy(kk) = iFrgCopy
              x(kk) = xzmpar(ii,iFrg)
              parlabel(kk) = czmpar(ii,iFrg)
              SELECT CASE (kzmpar(ii,iFrg))
                CASE (1) ! position
                  kzmpar2(kk) = 1
                  lb(kk) = 0.0
                  ub(kk) = 1.0
                CASE (2,6) ! quaternion
                  IF (UseQuaternions(iFrg)) THEN
                    kzmpar(ii,iFrg) = 2 ! quaternion instead of single axis 
                    kzmpar2(kk) = 2
                    lb(kk) = -1.0
                    ub(kk) =  1.0
                  ELSE
                    kzmpar(ii,iFrg) = 6 ! single axis instead of quaternion 
                    kzmpar2(kk) = 6
! At the moment a Z-matrix containing more than one atom is read in, four
! parameters are reserved for 'rotations'. When the rotation is restricted to
! a single axis, only two of these parameters are necessary. By setting
! upper bound - lower bound to 0.000001, these parameters will be considered 'fixed'
! and will not be picked during the SA, nor will they be optimised during the simplex minimisation.
                    tk = tk + 1
                    SELECT CASE (tk)
                      CASE (1,2) 
                        lb(kk) = -1.0
                        ub(kk) =  1.0
                      CASE (3,4) 
                        lb(kk) =  0.0
                        ub(kk) =  0.000001
                    END SELECT
                    IF (tk .EQ. 4) tk = 0
                  ENDIF
                CASE (3) ! torsion
                  kzmpar2(kk) = 3
                  IF      ((x(kk) .GT. -180.0) .AND. (x(kk) .LT. 180.0)) THEN
                    lb(kk) =  -180.0
                    ub(kk) =   180.0
                  ELSE IF (x(kk) .GT. 0.0 .AND. x(kk) .LT.  360.0) THEN
                    lb(kk) =   0.0
                    ub(kk) = 360.0
                  ELSE 
                    lb(kk) = x(kk) - 180.0
                    ub(kk) = x(kk) + 180.0
                  ENDIF              
                CASE (4) ! angle
                  kzmpar2(kk) = 4
                  lb(kk) = x(kk) - 10.0
                  ub(kk) = x(kk) + 10.0
                CASE (5) ! bond
                  kzmpar2(kk) = 5
                  lb(kk) = 0.9*x(kk)
                  ub(kk) = x(kk)/0.9
              END SELECT
            ENDDO
          ENDDO
!JCC End of check on selection
        ENDIF
      ENDDO
      NStPar = kk
      CALL WDialogSelect(IDD_SAW_Page2)
      PrefParExists = WDialogGetCheckBoxLogical(IDF_Use_PO)
! Set up preferred orientation. This can't be the first parameter: it must be appended to the rest.
      IF (PrefParExists) THEN
        CALL WDialogGetInteger(IDF_PO_a,iH)
        CALL WDialogGetInteger(IDF_PO_b,iK)
        CALL WDialogGetInteger(IDF_PO_c,iL)
        PrefPars(1) = FLOAT(iH)
        PrefPars(2) = FLOAT(iK)
        PrefPars(3) = FLOAT(iL)
        kk = kk + 1
        Par2iFrg(kk) = 0
        kzmpar2(kk) = 7 ! preferred orientation
        x(kk) = 1.0 ! no preferred orientation
        parlabel(kk) = 'Preferred Orientation'
        lb(kk) =  0.8
        ub(kk) =  1.0
        iPrfPar = kk
      ENDIF
      nvar = kk
! Now fill the grid
      CALL WDialogSelect(IDD_SA_input2)
      CALL WGridRows(IDF_parameter_grid,nvar)
      DO i = 1, nvar
        CALL WGridLabelRow(IDF_parameter_grid,i,parlabel(i))
        CALL WGridPutCellReal(IDF_parameter_grid,1,i,SNGL(x(i)),'(F12.5)')
        CALL WGridPutCellReal(IDF_parameter_grid,2,i,SNGL(lb(i)),'(F12.5)')
        CALL WGridPutCellReal(IDF_parameter_grid,3,i,SNGL(ub(i)),'(F12.5)')
        CALL WGridPutCellCheckBox(IDF_parameter_grid,4,i,Unchecked)
        CALL WGridStateCell(IDF_parameter_grid,1,i,Enabled)
        CALL WGridStateCell(IDF_parameter_grid,2,i,Enabled)
        CALL WGridStateCell(IDF_parameter_grid,3,i,Enabled)
        prevub(i) = ub(i)
        prevlb(i) = lb(i)
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE SA_Parameter_Set
!
!*****************************************************************************
!
! JCC This subroutine handles the various types of status error that can arise 
! during a reading of a file and produces a suitable message to say what went wrong.
      SUBROUTINE FileErrorPopup(FileName, ErrorStatus)

      USE WINTERACTER

      INCLUDE 'iosdef.for'

      INTEGER       ErrorStatus
      CHARACTER*(*) FileName
      INTEGER       lenstr

      lenstr = LEN_TRIM(FileName)
      SELECT CASE(ErrorStatus)
        CASE (FOR$IOS_FILNOTFOU) 
          CALL ErrorMessage("The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//" does not exist.")
        CASE (FOR$IOS_OPEFAI)
          CALL ErrorMessage("The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//" could not be opened.")
        CASE (FOR$IOS_PERACCFIL)
          CALL ErrorMessage("You do not have permission to access the file "//FileName(1:lenstr))
        CASE DEFAULT
          CALL ErrorMessage("The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//"was not read successfully.")
      END SELECT

      END SUBROUTINE FileErrorPopup
!
!*****************************************************************************
!
      SUBROUTINE UpdateZmatrixSelection

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER NumberOfDOF, izmtot, iFrg, tInteger
      CHARACTER(MaxPathLength) DirName
      CHARACTER*80 FileName

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page1)
      nfrag  = 0
      izmtot = 0
      NATOM  = 0
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          nfrag = nfrag + 1
          CALL WDialogGetInteger(IDFzmNumber(iFrg),tInteger)
          NATOM = NATOM + tInteger * natoms(iFrg)
          IF (natoms(iFrg) .EQ. 1) THEN
            NumberOfDOF = 3 ! It's an atom
          ELSE
            NumberOfDOF = izmpar(iFrg) - 1 ! Count the quaternions as three, not four
          ENDIF
          izmtot = izmtot + NumberOfDOF
! Enable 'Number of' field
          CALL WDialogFieldState(IDFzmNumber(iFrg),Enabled)
! Due to lack of space: display the name of file only, without its full path
          CALL SplitPath(frag_file(iFrg),DirName,FileName)
          CALL WDialogPutString(IDFZMFile(iFrg),FileName)
! Enable 'Delete' button
          CALL WDialogFieldState(IDBZMDelete(iFrg),Enabled)
! Enable 'View' button
          CALL WDialogFieldState(IDBZMView(iFrg),Enabled)
! Enable 'Edit...' button
          CALL WDialogFieldState(IDBzmEdit(iFrg),Enabled)
          CALL WDialogPutInteger(IDFZMpars(iFrg),NumberOfDOF)
        ELSE
          izmpar(iFrg) = 0
          natoms(iFrg) = 0
! Initialise 'Number of' field to 0
          CALL WDialogPutInteger(IDFzmNumber(iFrg),0)
! Disable 'Number of' field
          CALL WDialogFieldState(IDFzmNumber(iFrg),Disabled)
          CALL WDialogClearField(IDFZMFile(iFrg))
! Disable 'View' button
          CALL WDialogFieldState(IDBZMView(iFrg),Disabled)
! Disable 'Delete' button
          CALL WDialogFieldState(IDBZMDelete(iFrg),Disabled)
! Disable 'Edit...' button
          CALL WDialogFieldState(IDBzmEdit(iFrg),Disabled)
          CALL WDialogClearField(IDFZMpars(iFrg))
        ENDIF
      ENDDO
! JvdS @@ Following is wrong (we need a valid .sdi as well), but 
! a. identical to release version
! b. it's difficult to keep track of the validity of the .sdi file
      CALL WDialogFieldStateLogical(IDNEXT,nfrag .NE. 0)
      IF (izmtot .EQ. 0) THEN            
        CALL WDialogClearField(IDF_ZM_allpars)
      ELSE
        CALL WDialogPutInteger(IDF_ZM_allpars,izmtot)
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE UpdateZmatrixSelection
!
!*****************************************************************************
!
      SUBROUTINE ImportZmatrix(TheFileName)
!
! If TheFileName is empty, user will be prompted for a file.
! 
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER            I, iFlags, iSelection
      CHARACTER(LEN=255) :: FilterStr
      CHARACTER(LEN=512) :: Zmfiles
      CHARACTER(LEN=512) :: Info = 'You can import molecules from res, cssr, mol2, mol or pdb files into DASH.'//CHAR(13)//&
                                   'When you click on OK, you will be prompted for a file in one'//CHAR(13)//&
                                   'of these formats. DASH will create separate Z-matrix files for'//CHAR(13)//&
                                   'each chemical residue present in the first entry in the file.'//CHAR(13)//&
                                   'In multiple entry files the first entry will be read only.'
      INTEGER, EXTERNAL :: Res2Mol2, CSSR2Mol2
      CHARACTER(MaxPathLength) tFileName
      INTEGER tNumZMatrices
      CHARACTER(80) tZmatrices
      DIMENSION tZmatrices(10)
      
      tFileName = TheFileName
      IF (LEN_TRIM(tFileName) .EQ. 0) THEN
        CALL InfoMessage(Info)
        IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
        iFlags = LoadDialog + DirChange + AppendExt
        FilterStr = "All files (*.*)|*.*|"//&
                    "Molecular model files|*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"//&
                    "Protein DataBank files (*.pdb)|*.pdb|"//&
                    "Mol2 files (*.mol2, *.ml2)|*.mol2;*.ml2|"//&
                    "mdl mol files|*.mol;*.mdl|"//&
                    "SHELX files (*.res)|*.res|"//&
                    "cssr files (*.cssr)|*.cssr|"
        iSelection = 2
        tFileName = ''
        CALL WSelectFile(FilterStr, iFlags, tFileName,"Select a file for conversion",iSelection)
        IF ((WInfoDialog(ExitButtonCommon) .NE. CommonOK) .OR. (LEN_TRIM(tFileName) .EQ. 0)) RETURN
      ENDIF
      CALL zmConvert(tFileName,tNumZMatrices,tZmatrices)
      IF (tNumZMatrices .EQ. 0) THEN
! An error occurred
        CALL ErrorMessage("Sorry, could not create Z-matrices.")
! Prompt with files created
      ELSE
        ZmFiles = ''
        DO I = 1, tNumZMatrices    
          ZmFiles = ZmFiles(1:LEN_TRIM(ZmFiles))//CHAR(13)//tZmatrices(I)(1:LEN_TRIM(tZmatrices(I)))
        ENDDO
        CALL InfoMessage("Generated the following Z-matrices successfully:"//CHAR(13)//&
                         ZmFiles(1:LEN_TRIM(ZmFiles))//CHAR(13)//CHAR(13)//&
                         "You can load them by clicking on the Z-matrix browse buttons"//CHAR(13)//&
                         "in the Molecular Z-Matrices window.")
      ENDIF

      END SUBROUTINE ImportZmatrix
!
!*****************************************************************************
!
      SUBROUTINE zmConvert(TheInputFile,TheNumOfZmatrices,TheZmatrices)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheInputFile
      INTEGER,       INTENT (  OUT) :: TheNumOfZmatrices
      CHARACTER(80), INTENT (  OUT) :: TheZmatrices
      DIMENSION TheZmatrices(10)

      INTEGER iLen, iPos
      CHARACTER*4 ExtensionStr
      INTEGER, EXTERNAL :: Res2Mol2, CSSR2Mol2
      CHARACTER(MaxPathLength) tInputFile ! to resolve call by reference/value ambiguity
      CHARACTER(5) fmt
      INTEGER iStat, iStart, I

      TheNumOfZmatrices = 0
      tInputFile = TheInputFile
      iLen = LEN_TRIM(tInputFile)
! Find the last occurence of '.' in tInputFile
      iPos = iLen - 1 ! Last character of tInputFile is not tested
! The longest extension possible is four
      DO WHILE ((iPos .NE. 0) .AND. (tInputFile(iPos:iPos) .NE. '.') .AND. (iPos .NE. (iLen-5)))
        iPos = iPos - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      IF (tInputFile(iPos:iPos) .NE. '.') THEN
        CALL ErrorMessage('Invalid extension.') 
        RETURN
      ENDIF
      ExtensionStr = '    '
      ExtensionStr = tInputFile(iPos+1:iLen)
      CALL ILowerCase(ExtensionStr)
      SELECT CASE (ExtensionStr)
        CASE ('cssr')
          IF (CSSR2Mol2(tInputFile) .NE. 1) RETURN
! Replace 'cssr' by 'mol2'
          tInputFile = tInputFile(1:iLen-4)//'mol2'
          iLen = LEN_TRIM(tInputFile)
          fmt = '-mol2'
        CASE ('res ')
          IF (Res2Mol2(tInputFile) .NE. 1) RETURN
! Replace 'res' by 'mol2'
          tInputFile = tInputFile(1:iLen-3)//'mol2'
          iLen = LEN_TRIM(tInputFile)
          fmt = '-mol2'
        CASE ('pdb ')
          fmt = '-pdb'
        CASE ('mol2','ml2 ')
          fmt = '-mol2'
        CASE ('mol ','mdl ')
          fmt = '-mol'
      END SELECT
! Run silently, 
      CALL IOSDeleteFile('MakeZmatrix.log')
      iStat = InfoError(1) ! Clear any errors 
      iStart = 1
      DO I = 1, iLen
        IF (tInputFile(I:I) .EQ. DIRSPACER) iStart = I + 1
      ENDDO
      CALL WCursorShape(CurHourGlass)
      CALL IOSCommand(InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'zmconv.exe'// &
        ' '//fmt(1:LEN_TRIM(fmt))//' "'//tInputFile(iStart:iLen)//'"',3)
! Check return status
      OPEN(UNIT=145, FILE='MakeZmatrix.log',STATUS='OLD',IOSTAT = iStat)
      IF ((InfoError(1) .EQ. ErrOSCommand) .OR. (iStat .NE. 0)) THEN
        CALL WCursorShape(CurCrossHair)
! An error occurred
        CALL ErrorMessage("Sorry, could not create Z-matrices.")
! Prompt with files created
      ELSE ! All OK: Need to read in the file names
        CALL WCursorShape(CurCrossHair)
        DO WHILE (TheNumOfZmatrices .LT. 10)
          READ (145,'(A)',ERR=20,END=20) TheZmatrices(TheNumOfZmatrices+1)
          IF (LEN_TRIM(TheZmatrices(TheNumOfZmatrices+1)) .NE. 0) CALL INC(TheNumOfZmatrices)
        ENDDO
 20     CLOSE(145)
      ENDIF

      END SUBROUTINE zmConvert
!
!*****************************************************************************
!
