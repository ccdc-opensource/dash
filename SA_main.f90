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

      CHARACTER*20, EXTERNAL :: Integer2String
      INTEGER, EXTERNAL :: DateToday
      INTEGER tFileHandle, I, kk, iFrg, ilen, II, Fixed
      REAL    R, x, lb, ub
      CHARACTER(MaxPathLength) tSDIFile
      CHARACTER*17 DateStr
      REAL    MaxMoves1, tMaxMoves
      INTEGER MaxMoves2, tLen
      CHARACTER*20 MaxMovesStr
      CHARACTER*36 tString36

      WriteSAParametersToFile = 1 ! Error
      CALL PushActiveWindowID
      tFileHandle = 10
      OPEN(tFileHandle,FILE=TheFileName,ERR=999)
      WRITE(tFileHandle,'("  Parameters for simulated annealing in ",A8)',ERR=999) ProgramVersion
      CALL Date2String(DateToday(),DateStr,tLen)
      WRITE(tFileHandle,'(A)',ERR=999) "  Date = "//DateStr(1:tLen)
      CALL WDialogSelect(IDD_SAW_Page1)
      CALL WDialogGetString(IDF_SA_Project_Name,tSDIFile)
      WRITE(tFileHandle,'("  SDI file = ",A)',ERR=999) tSDIFile(1:LEN_TRIM(tSDIFile))
! Profile range (including maximum resolution), wavelength, unit cell parameters, zero point
      CALL WDialogSelect(IDD_SA_Modal_input2)
      kk = 0
      DO iFrg = 1, nFrag
! Write the name of the file
        ilen = LEN_TRIM(frag_file(iFrg))
        WRITE(tFileHandle,'(A)',ERR=999) '  Z-matrix '//frag_file(iFrg)(1:ilen)
        DO ii = 1, izmpar(ifrg)
          kk = kk + 1
          ilen = LEN_TRIM(czmpar(ii,iFrg))
          CALL WGridGetCellReal    (IDF_parameter_grid_modal,1,kk,x)
          CALL WGridGetCellCheckBox(IDF_parameter_grid_modal,4,kk,Fixed)
          IF (Fixed .EQ. 1) THEN
            WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,A5)",ERR=999) czmpar(ii,iFrg),x,'Fixed'
          ELSE
            CALL WGridGetCellReal(IDF_parameter_grid_modal,2,kk,lb)
            CALL WGridGetCellReal(IDF_parameter_grid_modal,3,kk,ub)
            WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,F12.5,1X,F12.5)",ERR=999) czmpar(ii,iFrg),x,lb,ub
          ENDIF
        ENDDO
      ENDDO
! Preferred Orientation
      IF (PrefParExists) THEN
        WRITE(tFileHandle,'("  March-Dollase preferred orientation correction will be applied.")',ERR=999)
        WRITE(tFileHandle,'("  Orientation: a* = ",F6.3,", b* = ",F6.3,", c* = ",F6.3)',ERR=999) (PrefPars(ii),ii=1,3)
        kk = kk + 1
        CALL WGridGetCellReal(IDF_parameter_grid_modal,2,kk,lb)
        CALL WGridGetCellReal(IDF_parameter_grid_modal,3,kk,ub)
        CALL WGridGetCellReal(IDF_parameter_grid_modal,1,kk,x)
        tString36 = 'Preferred Orientation'
        WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,F12.5,1X,F12.5)",ERR=999) tString36,x,lb,ub
      ENDIF
! Total number of parameters for this problem
! Number of atoms
      CALL WDialogSelect(IDD_SA_input3_2)
      CALL WDialogGetInteger(IDF_SA_RandomSeed1,I)
      WRITE(tFileHandle,'("  Random seed 1 = ",I5)',ERR=999) I
      CALL WDialogGetInteger(IDF_SA_RandomSeed2,I)
      WRITE(tFileHandle,'("  Random seed 2 = ",I5)',ERR=999) I
      CALL WDialogGetReal(IDF_SA_T0, R)
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
      SUBROUTINE SA_Parameter_Set
!
! This routine initialises things after all Z-matrices are known. So that is 1. when
! leaving the Z-matrices window, 2. when loading a project file.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PO_VAR
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER         NStPar
      COMMON /pextra/ NStPar

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB
      
      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, Get_HydrogenTreatment
      REAL, EXTERNAL :: Degrees2Radians
      CHARACTER*36 parlabel(mvar)
      INTEGER I, II, kk, iFrg, iH, iK, iL
      REAL    Point1(1:3), Point2(1:3), Point3(1:3) 
      REAL    Axis(1:3)
      REAL    Q(0:3)
      REAL    v1(1:3), v2(1:3)
      REAL    Alpha, Beta, Gamma
      REAL    Q1(0:3), Q2(0:3)

      CALL PushActiveWindowID
! Calculate the unit cell axes in terms of the orthogonal lattice from
! the unit cell parameters
      CALL LatticeCellParameters2Lattice(CellPar(1), CellPar(2), CellPar(3), &
                                         CellPar(4), CellPar(5), CellPar(6), f2cmat)
! Calculate the reciprocal lattice
      CALL InverseMatrix(f2cmat, c2fmat, 3)
      CALL frac2pdb(f2cpdb, CellPar(1), CellPar(2), CellPar(3), CellPar(4), CellPar(5), CellPar(6))
      CALL CREATE_FOB(Get_HydrogenTreatment() .EQ. 2)
      CALL Create_AtomicWeightings(Get_HydrogenTreatment())
! Per Z-matrix, determine whether to use quaternions or a single axis
      DO iFrg = 1, nFrag
        CALL zmDoAdmin(iFrg)
        IF (.NOT. UseQuaternions(iFrg)) THEN
! Calculate the single axis
          SELECT CASE (zmSingleRotAxDef(iFrg))
            CASE (1) ! two atom numbers
! We need the Cartesian co-ordinates of these two atoms
              CALL makexyz(natoms(iFrg), BLEN(1,iFrg), ALPH(1,iFrg), BET(1,iFrg), IZ1(1,iFrg), IZ2(1,iFrg), IZ3(1,iFrg), axyzo)
              Axis(1) = axyzo(1,zmSingleRotAxAtm(2,iFrg)) - axyzo(1,zmSingleRotAxAtm(1,iFrg))
              Axis(2) = axyzo(2,zmSingleRotAxAtm(2,iFrg)) - axyzo(2,zmSingleRotAxAtm(1,iFrg))
              Axis(3) = axyzo(3,zmSingleRotAxAtm(2,iFrg)) - axyzo(3,zmSingleRotAxAtm(1,iFrg))
            CASE (2) ! Fractional
! The variable zmSingleRotAxFrac holds the fractional co-ordinates,
! we need orthogonal co-ordinates => convert
              CALL PremultiplyVectorByMatrix(f2cmat, zmSingleRotAxFrac(1,iFrg), Axis)
            CASE (3) ! Normal to plane defined by three atoms
! We need the Cartesian co-ordinates of these atoms
              CALL makexyz(natoms(iFrg), BLEN(1,iFrg), ALPH(1,iFrg), BET(1,iFrg), IZ1(1,iFrg), IZ2(1,iFrg), IZ3(1,iFrg), axyzo)
              Point1 = axyzo(:,zmSingleRotAxPlnAtm(1,iFrg))
              Point2 = axyzo(:,zmSingleRotAxPlnAtm(2,iFrg))
              Point3 = axyzo(:,zmSingleRotAxPlnAtm(3,iFrg))
              Point1 = Point1 - Point2
              Point3 = Point3 - Point2
              CALL VectorCrossProduct(Point1, Point3, Axis)
          END SELECT
! Calculate initial orientation
          SELECT CASE (zmSingleRAIniOrDef(iFrg))
            CASE (1) ! Define from axis (only possible when axis is defined from atoms, not from another axis)
              v1(1) = Axis(1)
              v1(2) = Axis(2)
              v1(3) = Axis(3)
              CALL PremultiplyVectorByMatrix(f2cmat, zmSingleRAIniOrFrac(1,iFrg), v2) ! frac -> cart
              CALL Vector2Quaternion(v1, Q1)
              CALL Vector2Quaternion(v2, Q2)
              CALL QuaternionInverse(Q1)
              CALL QuaternionMultiply(Q2, Q1, zmInitialQs(0, iFrg))
            CASE (2) ! Defined as Euler angles => convert to Quaternions
              Alpha = zmSingleRAIniOrEuler(1, iFrg)
              Beta  = zmSingleRAIniOrEuler(2, iFrg)
              Gamma = zmSingleRAIniOrEuler(3, iFrg)
              zmInitialQs(0, iFrg) = COS(0.5*Degrees2Radians(Beta)) * COS(0.5*Degrees2Radians(Alpha+Gamma))
              zmInitialQs(1, iFrg) = SIN(0.5*Degrees2Radians(Beta)) * COS(0.5*Degrees2Radians(Alpha-Gamma))
              zmInitialQs(2, iFrg) = SIN(0.5*Degrees2Radians(Beta)) * SIN(0.5*Degrees2Radians(Alpha-Gamma))
              zmInitialQs(3, iFrg) = COS(0.5*Degrees2Radians(Beta)) * SIN(0.5*Degrees2Radians(Alpha+Gamma))
            CASE (3) ! Defined as quaternions
              zmInitialQs(0, iFrg) = zmSingleRAIniOrQuater(0,iFrg)
              zmInitialQs(1, iFrg) = zmSingleRAIniOrQuater(1,iFrg)
              zmInitialQs(2, iFrg) = zmSingleRAIniOrQuater(2,iFrg)
              zmInitialQs(3, iFrg) = zmSingleRAIniOrQuater(3,iFrg)
          END SELECT
          CALL Vector2Quaternion(Axis, Q)
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
          zmSingleRotationQs(0,iFrg) = 1.0
          zmSingleRotationQs(1,iFrg) = 2.0 * (Q(1)*Q(3) + Q(0)*Q(2))
          zmSingleRotationQs(2,iFrg) = 2.0 * (Q(2)*Q(3) - Q(0)*Q(1))
          zmSingleRotationQs(3,iFrg) = (Q(0)**2) - (Q(1)**2) - (Q(2)**2) + (Q(3)**2)
        ENDIF
      ENDDO
      kk = 0
! Run through all possible fragments
      DO iFrg = 1, nFrag
        DO ii = 1, izmpar(iFrg)
          kk = kk + 1
          zm2Par(ii,iFrg) = kk
          Par2iFrg(kk)    = iFrg
          x(kk) = xzmpar(ii,iFrg)
          parlabel(kk) = czmpar(ii,iFrg)
          ModalFlag(kk) = 0 ! Initialise to 0 meaning "not a torsion"
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
                lb(kk) = -1.0
                ub(kk) =  1.0
              ENDIF
            CASE (3) ! torsion
              kzmpar2(kk) = 3
              ModalFlag(kk) = 1
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
      NStPar = kk
      CALL WDialogSelect(IDD_SAW_Page2)
      PrefParExists = WDialogGetCheckBoxLogical(IDF_Use_PO)
! Set up preferred orientation. This can't be the first parameter: it must be appended to the rest.
      IF (PrefParExists) THEN
        CALL WDialogGetInteger(IDF_PO_a, iH)
        CALL WDialogGetInteger(IDF_PO_b, iK)
        CALL WDialogGetInteger(IDF_PO_c, iL)
        PrefPars(1) = FLOAT(iH)
        PrefPars(2) = FLOAT(iK)
        PrefPars(3) = FLOAT(iL)
        kk = kk + 1
        Par2iFrg(kk) = 0
        kzmpar2(kk) = 7 ! preferred orientation
        x(kk) = 1.0 ! preferred orientation
        parlabel(kk) = 'Preferred Orientation'
        lb(kk) =  0.5
        ub(kk) =  2.0
        iPrfPar = kk
      ENDIF
      nvar = kk
! Now fill the grid
      CALL WDialogSelect(IDD_SA_Modal_input2)
      CALL WGridRows(IDF_parameter_grid_modal,nvar)
      DO i = 1, nvar
        CALL WGridLabelRow(IDF_parameter_grid_modal, i, parlabel(i))
        CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, i, x(i), '(F12.5)')
        CALL WGridPutCellReal(IDF_parameter_grid_modal, 2, i, lb(i), '(F12.5)')
        CALL WGridPutCellReal(IDF_parameter_grid_modal, 3, i, ub(i), '(F12.5)')
        CALL WGridPutCellCheckBox(IDF_parameter_grid_modal, 4, i, Unchecked)
        CALL WGridPutCellCheckBox(IDF_parameter_grid_modal, 5, i, Unchecked)
        CALL WGridStateCell(IDF_parameter_grid_modal, 1, i, Enabled)
        CALL WGridStateCell(IDF_parameter_grid_modal, 2, i, Enabled)
        CALL WGridStateCell(IDF_parameter_grid_modal, 3, i, Enabled)
!        prevub(i) = ub(i) ! taken out of this version of DASH by CJ?
!        prevlb(i) = lb(i)
      ENDDO
! Tick "Randomise initial values"
      CALL WDialogPutCheckBoxLogical(IDF_RandomInitVal, .TRUE.)
      Resume_SA = .FALSE.
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

! The following variables are there to allow the dialogue fields in the
! window dealing with Z-matrices to be handled by DO...ENDDO loops.
! The field identifiers assigned by Winteracter are not necessarily consecutive, 
! but these mappings are.

      INTEGER        IDFZMFile,                                                &
                     IDBZMDelete,                    IDBZMBrowse,              &
                     IDBZMView,                      IDBZMEdit,                &
                     IDFZMpars
      COMMON /IDFZM/ IDFZMFile(1:maxfrginterface),                                      &
                     IDBZMDelete(1:maxfrginterface), IDBZMBrowse(1:maxfrginterface),    &
                     IDBZMView(1:maxfrginterface),   IDBZMEdit(1:maxfrginterface),      &
                     IDFZMpars(1:maxfrginterface)

      INTEGER NumberOfDOF, izmtot, iFrg
      CHARACTER*(MaxPathLength) DirName
      CHARACTER*(80) FileName

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page1)
      izmtot = 0
      NATOM  = 0
      DO iFrg = 1, nFrag
        NATOM = NATOM + natoms(iFrg)
        IF (natoms(iFrg) .EQ. 1) THEN
          NumberOfDOF = 3 ! It's an atom
        ELSE
          NumberOfDOF = izmpar(iFrg) - 1 ! Count the quaternions as three, not four
        ENDIF
        izmtot = izmtot + NumberOfDOF
! Due to lack of space: display the name of file only, without its full path
        CALL SplitPath(frag_file(iFrg),DirName, FileName)
        CALL WDialogPutString(IDFZMFile(iFrg), FileName)
! Enable 'Delete' button
        CALL WDialogFieldState(IDBZMDelete(iFrg), Enabled)
! Enable 'View' button
        CALL WDialogFieldState(IDBZMView(iFrg), Enabled)
! Enable 'Edit...' button
        CALL WDialogFieldState(IDBzmEdit(iFrg), Enabled)
        CALL WDialogPutInteger(IDFZMpars(iFrg), NumberOfDOF)
      ENDDO
! Clear remainder
      DO iFrg = nFrag+1, maxfrginterface
        CALL WDialogClearField(IDFZMFile(iFrg))
! Disable 'View' button
        CALL WDialogFieldState(IDBZMView(iFrg), Disabled)
! Disable 'Delete' button
        CALL WDialogFieldState(IDBZMDelete(iFrg), Disabled)
! Disable 'Edit...' button
        CALL WDialogFieldState(IDBzmEdit(iFrg), Disabled)
        CALL WDialogClearField(IDFZMpars(iFrg))
      ENDDO
! JvdS @@ Following is wrong (we need a valid .sdi as well), but 
! a. identical to release version
! b. it's difficult to keep track of the validity of the .sdi file
      CALL WDialogFieldStateLogical(IDNEXT,nfrag .NE. 0)
      CALL WDialogFieldStateLogical(IDB_PO,nfrag .NE. 0) 
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
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER            I, iFlags, iSelection
      CHARACTER(LEN=255) :: FilterStr
      CHARACTER(LEN=512) :: Zmfiles
      CHARACTER(LEN=512) :: Info = 'You can import molecules from cif, res, cssr, mol2, mol'//CHAR(13)//&
                                   'or pdb files into DASH.'//CHAR(13)//&
                                   'When you click on OK, you will be prompted for a file in one'//CHAR(13)//&
                                   'of these formats. DASH will create separate Z-matrix files for'//CHAR(13)//&
                                   'each chemical residue present in the first entry in the file.'//CHAR(13)//&
                                   'In multiple entry files the first entry will be read only.'
      INTEGER, EXTERNAL :: CSSR2Mol2
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
                    "Molecular model files|*.cif;*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"
!O                    "CIF files (*.cif)|*.cif|"//&
!O                    "Protein DataBank files (*.pdb)|*.pdb|"//&
!O                    "Mol2 files (*.mol2, *.ml2)|*.mol2;*.ml2|"//&
!O                    "mdl mol files|*.mol;*.mdl|"//&
!O                    "SHELX files (*.res)|*.res|"//&
!O                    "cssr files (*.cssr)|*.cssr|"
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
      SUBROUTINE zmConvert(TheInputFile, TheNumOfZmatrices, TheZmatrices)

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheInputFile
      INTEGER,       INTENT (  OUT) :: TheNumOfZmatrices
      CHARACTER(80), INTENT (  OUT) :: TheZmatrices
      DIMENSION TheZmatrices(10)

      INTEGER, EXTERNAL :: CSSR2Mol2
      INTEGER iLen, iPos
      CHARACTER*4 ExtensionStr
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
        CASE ('cif ')
          fmt = '-cif'
        CASE ('cssr')
          IF (CSSR2Mol2(tInputFile) .NE. 1) RETURN
! Replace 'cssr' by 'mol2'
          tInputFile = tInputFile(1:iLen-4)//'mol2'
          iLen = LEN_TRIM(tInputFile)
          fmt = '-mol2'
        CASE ('res ')
          fmt = '-res'
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
