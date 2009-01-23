!C Contains code to do with reading crystal structures, using the output from zmconv.exe
!
!*****************************************************************************
!
      INTEGER FUNCTION XtalFileBrowse
!
! This routine lets the user browse a directory for a crystal structure file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data/...)
!           2 if user pressed cancel
! 
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, EXTERNAL :: XtalFileOpen
      CHARACTER(LEN=160) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName

      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|'//&
               'Crystal structure files|*.cif;*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|'
      tFileName = ' '
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open crystal structure file',IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) THEN
        XtalFileBrowse = 2
        RETURN
      ENDIF
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under Windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      XtalFileBrowse = XtalFileOpen(tFileName)

      END FUNCTION XtalFileBrowse
!
!*****************************************************************************
!
      INTEGER FUNCTION XtalFileOpen(TheFileName)
!
! This routine tries to open a file containing a crystal structure.
!
! INPUT   : TheFileName = the file name
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data/...)
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER, EXTERNAL :: XtalFileLoad
      LOGICAL FExists
      INTEGER KLEN

      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        XtalFileOpen = 1
        RETURN
      ENDIF
      XtalFileOpen = XtalFileLoad(TheFileName) 
      
      END FUNCTION XtalFileOpen
!
!*****************************************************************************
!
      INTEGER FUNCTION XtalFileLoad(TheFileName)
!
! RETURNS : 0 for success
!           1 for error (could be file not found/file in use/no valid data/...)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE RRVAR
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'LATTICE.INC'

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      INTEGER, EXTERNAL :: Read_One_Zm
      LOGICAL, EXTERNAL :: FnUnitCellOK, nearly_equal, RunZmConv
      INTEGER iHandle
      CHARACTER(MaxPathLength) :: line
      CHARACTER(MaxPathLength) :: globFile
      CHARACTER(MaxPathLength) :: ExtensionStr
      INTEGER nl, iStat
      CHARACTER(20) KeyChar
      INTEGER iFrg
      INTEGER KK, I, ExtLen
      INTEGER iLen
      REAL unit_cell_parameters(1:6)
      LOGICAL OK

      XtalFileLoad = 1 ! Initialise to error
      IF (.NOT. RunZmConv(TheFileName, .FALSE.)) RETURN
! Check return status
      OPEN(UNIT=145, FILE='MakeZmatrix.log',STATUS='OLD',IOSTAT = iStat)
      CLOSE(145)
      IF (iStat .NE. 0) THEN
        CALL ErrorMessage("Sorry, could not create Z-matrices.")
        RETURN
      ENDIF
!C Replace extension by .glob
      ExtLen = 5 ! Maximum length of a valid extension
      CALL FileGetExtension(TheFileName, ExtensionStr, ExtLen)
      IF ((ExtLen .LT. 1) .OR. (ExtLen .GT. 5)) THEN
        CALL ErrorMessage("Error parsing file extension")
        RETURN
      ENDIF
      iLen = LEN_TRIM(TheFileName)
      globFile = TheFileName(1:iLen-ExtLen)//"glob"
!C Load .glob file
      iHandle = 10
      OPEN(iHandle, FILE=globFile(1:LEN_TRIM(globFile)), STATUS='OLD', ERR=999)
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page6)
      CALL WDialogPutString(IDF_Xtal_File_Name, TheFileName)
      CALL PopActiveWindowID
      iFrg = 0
 10   line = ''
      READ(iHandle, '(A)', END=100, ERR=999) line
      nl = LEN_TRIM(line)
      CALL ILowerCase(line(1:nl))
      CALL INextString(line, KeyChar)
      SELECT CASE (KeyChar(1:3))
        CASE ('cel')                                ! Cell parameters
          DO I = 1, 6
            CALL INextReal(line, unit_cell_parameters(i))
          ENDDO
          ! Check if we have unit cell parameters available
          IF (FnUnitCellOK()) THEN
            OK = .TRUE.
            DO I = 1, 6
              IF (.NOT. nearly_equal(CellPar(i), unit_cell_parameters(i), 0.5)) OK = .FALSE.
            ENDDO
            IF (.NOT. OK) THEN
              CALL ErrorMessage("Unit-cell parameters from .sdi file and from crystal structure are inconsistent."//  &
              CHAR(13)//CHAR(10)//"The chi-squared values will be nonsensical.")
              CLOSE(iHandle)
              RETURN
            ENDIF
          ENDIF
        CASE ('spa')
     ! Ignore
        CASE ('sym')
     ! Ignore
        CASE ('z-m')                                ! Z-matrix file
          iFrg = iFrg + 1
          IF (iFrg .GT. maxfrg) GOTO 100
          frag_file(iFrg) = line(ILocateChar(line):)
          IF (Read_One_ZM(iFrg) .NE. 0) THEN ! successful read
            CALL ErrorMessage("Error while reading Z-matrix file "//frag_file(iFrg)(1:LEN_TRIM(frag_file(iFrg))))
            iFrg = iFrg - 1 
            CLOSE(iHandle)
            RETURN
          ENDIF ! If the read on the Z-matrix was ok
        CASE ('cen')                                ! "Centre of mass"
          IF ( .NOT. UseCCoM ) THEN
            IF (iFrg .NE. 0) THEN
            ! @@ This will go wrong if the filename contains a space
              CALL INextString(line, KeyChar)
              CALL INextString(line, KeyChar)
              CALL INextReal(line, RR_tran(1,iFrg))
              CALL INextReal(line, RR_tran(2,iFrg))
              CALL INextReal(line, RR_tran(3,iFrg))
            ENDIF
          ENDIF
        CASE ('cry')                                ! "Crystallographic centre of mass"
          IF ( UseCCoM ) THEN
            IF (iFrg .NE. 0) THEN
              CALL INextString(line, keychar)
              CALL INextString(line, keychar)
              CALL INextString(line ,keychar)
              CALL INextReal(line, RR_tran(1,iFrg))
              CALL INextReal(line, RR_tran(2,iFrg))
              CALL INextReal(line, RR_tran(3,iFrg))
            ENDIF
          ENDIF
        CASE ('qua')                                ! "Quaternion"
          IF (iFrg .NE. 0) THEN
            CALL INextReal(line, RR_rot(1,iFrg))
            CALL INextReal(line, RR_rot(2,iFrg))
            CALL INextReal(line, RR_rot(3,iFrg))
            CALL INextReal(line, RR_rot(4,iFrg))
          ENDIF
      END SELECT
      GOTO 10 
 100  CLOSE(iHandle)
      nFrag = iFrg
      !C Basically a Q&D hack: fill BestValuesDoF(:,1) (where "1" is the first SA run) because
      !C we already have a routine that can
      !C translate BestValuesDoF into a fully functional Rietveld refinement window.
      KK = 0
      DO iFrg = 1, nFrag
        ! Translations
        BestValuesDoF(KK+1,1) = RR_tran(1,iFrg)
        BestValuesDoF(KK+2,1) = RR_tran(2,iFrg)
        BestValuesDoF(KK+3,1) = RR_tran(3,iFrg)
        KK = KK +3
        ! Rotations
        IF (natoms(iFrg) .GT. 1) THEN
          BestValuesDoF(KK+1,1) = RR_rot(1,iFrg)
          BestValuesDoF(KK+2,1) = RR_rot(2,iFrg)
          BestValuesDoF(KK+3,1) = RR_rot(3,iFrg)
          BestValuesDoF(KK+4,1) = RR_rot(4,iFrg)
          KK = KK +4
        ENDIF
        ! Torsions
        DO I = 1, natoms(iFrg)
          IF (IOPTB(I,iFrg) .EQ. 1) THEN
            KK = KK + 1
            BestValuesDoF(KK,1) = BLEN(I,iFrg)
          ENDIF
          IF (IOPTA(I,iFrg) .EQ. 1) THEN
            KK = KK + 1
            BestValuesDoF(KK,1) = ALPH(I,iFrg)
          ENDIF
          IF (IOPTT(I,iFrg) .EQ. 1) THEN
            KK = KK + 1
            BestValuesDoF(KK,1) = BET(I,iFrg)
          ENDIF
        ENDDO
      ENDDO
      CALL UpdateZmatrixSelection
      CALL InitSADistRestraint
      CALL SA_Parameter_Set1(3) ! Has to be setup with explict hydrogen treatment
      CALL Clear_SA
      XtalFileLoad = 0
      RETURN
 999  CALL ErrorMessage('Error reading crystal structure file.')
      CLOSE(iHandle) 

      END FUNCTION XtalFileLoad
!
!*****************************************************************************
!
      SUBROUTINE UnitCellParametersFileBrowse
!
! This routine lets the user browse a directory for a crystal structure file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=120) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName

      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|'//&
               'Crystal structure files|*.cif;*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.ins;*.res;*.cssr|'
      tFileName = ' '
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Open crystal structure file', IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under Windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      CALL UnitCellParametersFileOpen(tFileName)

      END SUBROUTINE UnitCellParametersFileBrowse
!
!*****************************************************************************
!
      SUBROUTINE UnitCellParametersFileOpen(TheFileName)
!
! This routine tries to open a file containing a crystal structure.
!
! INPUT   : TheFileName = the file name
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      LOGICAL FExists
      INTEGER KLEN

      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        RETURN
      ENDIF
      CALL UnitCellParametersFileLoad(TheFileName) 
      
      END SUBROUTINE UnitCellParametersFileOpen
!
!*****************************************************************************
!
      SUBROUTINE UnitCellParametersFileLoad(TheFileName)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'LATTICE.INC'

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER, EXTERNAL :: CSSR2Mol2, proposed_crystal_system
      LOGICAL, EXTERNAL :: RunZmConv
      INTEGER iHandle
      CHARACTER(MaxPathLength) :: line
      CHARACTER(MaxPathLength) :: globFile
      CHARACTER(MaxPathLength) :: ExtensionStr
      INTEGER nl
      CHARACTER(20) KeyChar
      INTEGER I, ExtLen
      INTEGER iLen

      IF (.NOT. RunZmConv(TheFileName, .TRUE.)) RETURN
!C Replace extension by .glob
      ExtLen = 5 ! Maximum length of a valid extension
      CALL FileGetExtension(TheFileName, ExtensionStr, ExtLen)
      IF ((ExtLen .LT. 1) .OR. (ExtLen .GT. 5)) THEN
        CALL ErrorMessage("Error parsing file extension")
        RETURN
      ENDIF
      iLen = LEN_TRIM(TheFileName)
      globFile = TheFileName(1:iLen-ExtLen)//"glob"
!C Load .glob file
      iHandle = 10
      OPEN(iHandle, FILE=globFile(1:LEN_TRIM(globFile)), STATUS='OLD', ERR=999)
 10   line = ''
      READ(iHandle, '(A)', END=100, ERR=999) line
      nl = LEN_TRIM(line)
      CALL ILowerCase(line(1:nl))
      CALL INextString(line,keychar)
      SELECT CASE (KeyChar(1:3))
        CASE ('cel') ! Cell parameters
          DO I = 1, 6
            CALL INextReal(line, CellPar(i))
          ENDDO
          LatBrav = proposed_crystal_system()
          CALL Upload_CrystalSystem
          ! Upload_CrystalSystem() calls UpdateCell()
          ! UpdateCell() calls GenerateTickMarks()
        CASE ('spa') ! Space group
          ! Ignore
        CASE ('sym') ! Symmetry operators
          ! Ignore
      END SELECT
      GOTO 10 
 100  CLOSE(iHandle)
      RETURN
 999  CALL ErrorMessage('Error reading crystal structure file.')
      CLOSE(iHandle) 

      END SUBROUTINE UnitCellParametersFileLoad
!
!*****************************************************************************
!
