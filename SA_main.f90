!
!*****************************************************************************
!
      INTEGER FUNCTION WriteSAParametersToFile(TheFileName)

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER tFileHandle, I, kk, ifrg, ilen, II, Fixed
      REAL    R, x, lb, ub
      CHARACTER*80 tSDIFile
      CHARACTER*8  tDate      ! '20010215' for 15 Feb 2001
      CHARACTER*17 DateStr
      CHARACTER*9  MonthStr
      REAL    MaxMoves1, tMaxMoves
      INTEGER MaxMoves2

      WriteSAParametersToFile = 1 ! Error
      CALL PushActiveWindowID
      tFileHandle = 10
      OPEN(tFileHandle,FILE=TheFileName,ERR=999)
      WRITE(tFileHandle,'("  Parameters for simulated annealing in DASH")',ERR=999)
      CALL DATE_AND_TIME(tDate)
      DateStr = ''
      IF (tDate(7:7) .EQ. '0') THEN 
        DateStr(1:1) = tDate(8:8)       ! DateStr = '7'
      ELSE
        DateStr(1:2) = tDate(7:8)       ! DateStr = '12'
      ENDIF
      SELECT CASE (tDate(5:6))
        CASE ('01')
          MonthStr = 'January'
        CASE ('02')
          MonthStr = 'February'
        CASE ('03')
          MonthStr = 'March'
        CASE ('04')
          MonthStr = 'April'
        CASE ('05')
          MonthStr = 'May'
        CASE ('06')
          MonthStr = 'June'
        CASE ('07')
          MonthStr = 'July'
        CASE ('08')
          MonthStr = 'August'
        CASE ('09')
          MonthStr = 'September'
        CASE ('10')
          MonthStr = 'October'
        CASE ('11')
          MonthStr = 'November'
        CASE ('12')
          MonthStr = 'December'
      END SELECT
      DateStr = DateStr(1:LEN_TRIM(DateStr))//' '//MonthStr(1:LEN_TRIM(MonthStr))//' '//tDate(1:4)
      WRITE(tFileHandle,'(A)',ERR=999) "  Date = "//DateStr(1:LEN_TRIM(DateStr))
      CALL WDialogSelect(IDD_SAW_Page1)
      CALL WDialogGetString(IDF_SA_Project_Name,tSDIFile)
      WRITE(tFileHandle,'("  SDI file = ",A)',ERR=999) tSDIFile(1:LEN_TRIM(tSDIFile))
! Profile range (including maximum resolution), wavelength, unit cell parameters, zero point

      CALL WDialogSelect(IDD_SA_input2)
      kk = 0
      DO ifrg = 1, maxfrg
        IF (gotzmfile(ifrg)) THEN
! Write the name of the file
          ilen = LEN_TRIM(frag_file(ifrg))
          WRITE(tFileHandle,'(A)',ERR=999) '  Z-matrix '//frag_file(ifrg)(1:ilen)
          DO ii = 1, izmpar(ifrg)
            kk = kk + 1
            ilen = LEN_TRIM(czmpar(ii,ifrg))
            CALL WGridGetCellReal    (IDF_parameter_grid,1,kk,x)
            CALL WGridGetCellCheckBox(IDF_parameter_grid,4,kk,Fixed)
            IF (Fixed .EQ. 1) THEN
              WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,A5)",ERR=999) czmpar(ii,ifrg),x,'Fixed'
            ELSE
              CALL WGridGetCellReal(IDF_parameter_grid,2,kk,lb)
              CALL WGridGetCellReal(IDF_parameter_grid,3,kk,ub)
              WRITE(tFileHandle,"('    ',A36,1X,F12.5,1X,F12.5,1X,F12.5)",ERR=999) czmpar(ii,ifrg),x,lb,ub
            ENDIF
          ENDDO
        ENDIF
      ENDDO
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
        IF (MaxMoves1 .LT.    0.001) MaxMoves1 =    0.001
        IF (MaxMoves1 .GT. 1000.0  ) MaxMoves1 = 1000.0
        CALL WDialogGetInteger(IDF_MaxMoves2,MaxMoves2)
        IF (MaxMoves2 .LT.  1) MaxMoves2 =  1
        IF (MaxMoves2 .GT. 10) MaxMoves2 = 10
        tMaxMoves = MaxMoves1 * (10**FLOAT(MaxMoves2))
        IF (tMaxMoves .LT. 10.0) tMaxMoves = 10.0
        IF (tMaxMoves .GT. 10.0E12) tMaxMoves = 10.0E12
        I = NINT(tMaxMoves)
        WRITE(tFileHandle,'("  Maximum number of moves per run = ",I10)',ERR=999) I
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
      SUBROUTINE ViewZmatrix(ifrg)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: ifrg

      INTEGER I
      CHARACTER*85 temp_file
      CHARACTER*2  AtmElement(1:MAXATM_2)
      REAL*8 CART(maxatm,3)
      INTEGER IHANDLE
      INTEGER, EXTERNAL :: WriteMol2
      LOGICAL, EXTERNAL :: ColourFlexibleTorsions
      INTEGER atom
      INTEGER Element
      INTEGER NumOfFlexTorsions
      INTEGER tLength, BondNr

      natcry = NATOMS(ifrg)
      CALL MAKEXYZ(natcry,BLEN(1,ifrg),ALPH(1,ifrg),BET(1,ifrg),      &
     &             IZ1(1,ifrg),IZ2(1,ifrg),IZ3(1,ifrg),CART(1,1),     &
     &             CART(1,2),CART(1,3))
! Conversion of asym to aelem : very dirty, but works
      DO I = 1, natcry
        axyzo(I,1) = SNGL(CART(I,1))
        axyzo(I,2) = SNGL(CART(I,2))
        axyzo(I,3) = SNGL(CART(I,3))
        AtmElement(I)(1:2) = asym(I,ifrg)(1:2)
        atomlabel(I) = OriginalLabel(I,ifrg)
      ENDDO
      CALL AssignCSDElement(AtmElement)
      nbocry = NumberOfBonds(ifrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr,ifrg)
        bond(BondNr,1) = Bonds(1,BondNr,ifrg)
        bond(BondNr,2) = Bonds(2,BondNr,ifrg)
      ENDDO
! Q & D to display flexible torsion angles in different colors by forcing different
! element types.
      IF (ColourFlexibleTorsions() .AND. (natcry.GE.4)) THEN
        DO I = 1, natcry
          aelem(I) = 1       ! Carbon        Grey
        ENDDO
        NumOfFlexTorsions = 0
        DO atom = 4, natcry
          IF (ioptt(atom,ifrg) .EQ. 1) THEN
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
            aelem(IZ1(atom,ifrg)) = Element
            aelem(IZ2(atom,ifrg)) = Element
            aelem(IZ3(atom,ifrg)) = Element
          ENDIF
        ENDDO
      ENDIF
      tLength = LEN_TRIM(frag_file(ifrg))
      temp_file = frag_file(ifrg)(1:tLength-8)//'_temp.mol2'
! Show the mol2 file
      IF (WriteMol2(temp_file) .EQ. 1) CALL ViewStructure(temp_file)
      CALL IOSDeleteFile(temp_file)
! Show the z-matrix file in an editor window
      CALL WindowOpenChild(IHANDLE)
      CALL WEditFile(frag_file(ifrg),Modeless,0,FileMustExist+ViewOnly+NoToolbar+NoFileNewOpen,4)
      CALL SetChildWinAutoClose(IHANDLE)

      END SUBROUTINE ViewZmatrix
!
!*****************************************************************************
!
      SUBROUTINE SA_Parameter_Set

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER NMAX
      PARAMETER (NMAX = 100)
      DOUBLE PRECISION XOPT,CSH,XP,FOPT
      COMMON /sacmn/ XOPT(NMAX),CSH(NMAX),XP(NMAX),FOPT

! JCC Handle via the PDB standard
      DOUBLE PRECISION f2cpdb
      COMMON /pdbcat/ f2cpdb(3,3)

      INTEGER mvar
      PARAMETER (mvar=100)
      DOUBLE PRECISION x,lb,ub,vm,xpreset
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      COMMON /presetr/ xpreset(mvar)

      DOUBLE PRECISION prevub, prevlb ! For saving the previous range
      COMMON /pvalues/ prevub(mvar), prevlb(mvar)

      LOGICAL log_preset
      COMMON /presetl/ log_preset

      DOUBLE PRECISION T0,rt
      COMMON /saparl/ T0,rt
      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

      CHARACTER*36 parlabel(mvar)
      DOUBLE PRECISION dcel(6)
      INTEGER I, II, KK, ifrg

      CALL PushActiveWindowID
      DO I = 1, 6
        dcel(I) = DBLE(CellPar(I))
      ENDDO
      CALL frac2cart(f2cmat,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
      CALL frac2pdb(f2cpdb,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
      CALL CREATE_FOB()
      kk = 0
! JCC Run through all possible fragments
      DO ifrg = 1, maxfrg
! Only include those that are now checked
        IF (gotzmfile(ifrg)) THEN
          DO ii = 1, izmpar(ifrg)
            kk = kk + 1
            x(kk) = xzmpar(ii,ifrg)
            parlabel(kk) = czmpar(ii,ifrg)
            SELECT CASE(kzmpar(ii,ifrg))
              CASE (1) !.. position
                lb(kk) = 0.0
                ub(kk) = 1.0
                vm(kk) = 0.1
              CASE (2) !.. quaternion
                lb(kk) = -1.0
                ub(kk) =  1.0
                vm(kk) =  0.1
              CASE (3) !.. torsion
                IF      (x(kk) .LT. 0.0 .AND. x(kk) .GT. -180.0) THEN
                  lb(kk) =  -180.0
                  ub(kk) =   180.0
                ELSE IF (x(kk) .GT. 0.0 .AND. x(kk) .LT.  360.0) THEN
                  lb(kk) =   0.0
                  ub(kk) = 360.0
                ELSE 
                  lb(kk) = x(kk) - 180.0
                  ub(kk) = x(kk) + 180.0
                ENDIF              
                vm(kk) = 10.0
              CASE (4) !.. angle
                lb(kk) = x(kk) - 10.0
                ub(kk) = x(kk) + 10.0
                vm(kk) = 1.0
              CASE (5) !.. bond
                lb(kk) = 0.9*x(kk)
                ub(kk) = x(kk)/0.9
                vm(kk) = 0.1*(ub(kk)-lb(kk))
            END SELECT
          ENDDO
!JCC End of check on selection
        ENDIF
      ENDDO
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
        CALL WGridPutCellCheckBox(IDF_parameter_grid,5,i,Checked)
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
      SUBROUTINE ClearZmatrices

      USE ZMVAR

      IMPLICIT NONE

! Blow away the z-matrices
      gotzmfile = .FALSE.
      CALL UpdateZmatrixSelection

      END SUBROUTINE ClearZmatrices
!
!*****************************************************************************
!
      SUBROUTINE UpdateZmatrixSelection

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER NumberOfDOF, izmtot, ifrg

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page1)
      nfrag  = 0
      izmtot = 0
      ntatm  = 0
      DO ifrg = 1, maxfrg
        IF (gotzmfile(ifrg)) THEN
          nfrag = nfrag + 1
          ntatm = ntatm + natoms(ifrg)
          IF (natoms(ifrg) .EQ. 1) THEN
            NumberOfDOF = 3 ! It's an atom
          ELSE
            NumberOfDOF = izmpar(ifrg) - 1 ! Count the quaternions as three, not four
          ENDIF
          izmtot = izmtot + NumberOfDOF
          CALL WDialogPutInteger(IDFZMpars(ifrg),NumberOfDOF)
          CALL WDialogPutString(IDFZMFile(ifrg),frag_file(ifrg))
! Enable 'View' button
          CALL WDialogFieldState(IDBZMView(ifrg),Enabled)
        ELSE
          izmpar(ifrg) = 0
          natoms(ifrg) = 0
          CALL WDialogClearField(IDFZMpars(ifrg))
          CALL WDialogClearField(IDFZMFile(ifrg))
! Disable 'View' button
          CALL WDialogFieldState(IDBZMView(ifrg),Disabled)
        ENDIF
      ENDDO
      natom = ntatm
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
      SUBROUTINE ImportZmatrix
 
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER            I, IFlags, ISEL, Ilen, Istart, Istat, Nzm
      INTEGER            POS
      CHARACTER(LEN=4)   :: EXT4
      CHARACTER(LEN=255) :: FilterStr, F
      CHARACTER(LEN=512) :: Zmfiles
      CHARACTER(LEN=5)   :: fmt     
      CHARACTER(LEN=512) :: Info = 'You can import molecules from mol2, mol or pdb files into DASH.'//CHAR(13)//&
                                   'When you click on OK, you will be prompted for a file in one'//CHAR(13)//&
                                   'of these formats. DASH will create separate z-matrix files for'//CHAR(13)//&
                                   'each chemical residue present in the first entry in the file.'//CHAR(13)//&
                                   'In multiple entry files the first entry will be read only.'
      INTEGER, EXTERNAL :: Res2Mol2, CSSR2Mol2

      CALL WMessageBox(OKCancel, InformationIcon, CommonOK, Info, "Create Z-matrix")
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
      IFlags = LoadDialog + DirChange + AppendExt
      FilterStr = "All files (*.*)|*.*|"//&
                  "Molecular model files|*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"//&
                  "Protein DataBank files (*.pdb)|*.pdb|"//&
                  "Mol2 files (*.mol2, *.ml2)|*.mol2;*.ml2|"//&
                  "mdl mol files|*.mol;*.mdl|"//&
                  "SHELX files (*.res)|*.res|"//&
                  "cssr files (*.cssr)|*.cssr|"
      ISEL = 2
      FNAME = ' '
      CALL WSelectFile(FilterStr, IFLAGS, FNAME,"Select a file for conversion",ISEL)
      Ilen = LEN_TRIM(FNAME)
      IF (Ilen .EQ. 0) RETURN
! Find the last occurence of '.' in TheFileName
      POS = Ilen-1 ! Last character of TheFileName is not tested
! The longest extension allowed is four
      DO WHILE ((POS .NE. 0) .AND. (FNAME(POS:POS) .NE. '.') .AND. (POS .NE. (Ilen-5)))
        POS = POS - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      IF (FNAME(POS:POS) .NE. '.') RETURN
      EXT4 = '    '
      EXT4 = FNAME(POS+1:Ilen)
      CALL ILowerCase(EXT4)
      SELECT CASE (EXT4)
        CASE ('cssr')
          ISTAT = CSSR2Mol2(FNAME)
          IF (ISTAT .NE. 1) RETURN
! Replace 'cssr' by 'mol2'
          FNAME = FNAME(1:Ilen-4)//'mol2'
          Ilen = LEN_TRIM(FNAME)
          fmt = '-mol2'
        CASE ('res ')
          ISTAT = Res2Mol2(FNAME)
          IF (ISTAT .NE. 1) RETURN
! Replace 'res' by 'mol2'
          FNAME = FNAME(1:Ilen-3)//'mol2'
          Ilen = LEN_TRIM(FNAME)
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
      Istat = InfoError(1) ! Clear any errors 
      Istart = 1
      DO I = 1, Ilen
        IF (FNAME(I:I) .EQ. DIRSPACER) Istart = I + 1
      ENDDO
      CALL WCursorShape(CurHourGlass)
      CALL IOSCommand(CONVEXE(1:LEN_TRIM(CONVEXE))//' '//fmt(1:LEN_TRIM(fmt))//' "'//FNAME(Istart:Ilen)//'"',3)
      CALL WCursorShape(CurCrossHair)
! Check return status
      OPEN(UNIT=145, FILE='MakeZmatrix.log',STATUS='OLD',IOSTAT = ISTAT)
      IF ((InfoError(1) .EQ. ErrOSCommand) .OR. (ISTAT .NE. 0)) THEN
! An error occurred: get the return status
! IECODE = InfoError(3)
        CALL ErrorMessage("Sorry, could not create z-matrices")
! Prompt with files created
      ELSE ! All Ok: Need to read in the file names
        Ilen = 1
        DO WHILE (Ilen .LT. 512)
          Nzm = Nzm + 1
          READ (145,'(A)',ERR=20,END=20) F
          ZmFiles(Ilen:512) = CHAR(13)//F(1:LEN_TRIM(F))
          Ilen = LEN_TRIM(ZmFiles) + 1
        ENDDO
 20     CONTINUE
        CALL WMessageBox(OKOnly, InformationICon, CommonOk, &
                         "Generated the following zmatrices successfully:"//CHAR(13)//&
                         ZmFiles(1:Ilen)//CHAR(13)//CHAR(13)//&
                         "You can load them by clicking on the zmatrix browse buttons"//CHAR(13)//&
                         "in the SA setup window",&
                         "Generation Successful")
        CLOSE(145)
      ENDIF

      END SUBROUTINE ImportZmatrix
!
!*****************************************************************************
!
