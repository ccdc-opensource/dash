!C Contains code to do with reading crystal structures, using the output from zmconv.exe
!
!*****************************************************************************
!
      SUBROUTINE XtalFileBrowse
!
! This routine lets the user browse a directory for a crystal structure file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

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
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under Windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      CALL XtalFileOpen(tFileName)

      END SUBROUTINE XtalFileBrowse
!
!*****************************************************************************
!
      SUBROUTINE XtalFileOpen(TheFileName)
!
! This routine tries to open a file containing a crystal structure.
!
! INPUT   : TheFileName = the file name
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INCLUDE 'GLBVAR.INC'

      LOGICAL FExists
      INTEGER KLEN

      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        RETURN
      ENDIF
      CALL XtalFileLoad(TheFileName) 
      
      END SUBROUTINE XtalFileOpen
!
!*****************************************************************************
!
      SUBROUTINE XtalFileLoad(TheFileName)

!C Note that the variables BLEN, ALPH and BET in ZMVAR do not allow storing of different
!C variables for copies!. This has not been a problem yet, but this will become a problem!

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE RRVAR
      USE SOLVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER iHandle
      CHARACTER(MaxPathLength) :: line
      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(MaxPathLength) :: globFile
      CHARACTER(MaxPathLength) :: ExtensionStr
      INTEGER nl, iStat, iStart
      CHARACTER(20) KeyChar
      CHARACTER(5) fmt
      INTEGER iFrg, iFrgCopy
      INTEGER KK, I, ExtLen
      INTEGER iLen, iPos
      INTEGER, EXTERNAL :: Read_One_Zm
      LOGICAL, EXTERNAL :: Get_UseCrystallographicCoM
      INTEGER, EXTERNAL :: CSSR2Mol2

      tFileName = TheFileName
      iLen = LEN_TRIM(tFileName)
! Find the last occurence of '.' in tFileName
      iPos = iLen - 1 ! Last character of tFileName is not tested
! The longest extension possible is four
      DO WHILE ((iPos .NE. 0) .AND. (tFileName(iPos:iPos) .NE. '.') .AND. (iPos .NE. (iLen-5)))
        iPos = iPos - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      IF (tFileName(iPos:iPos) .NE. '.') THEN
        CALL ErrorMessage('Invalid extension.') 
        RETURN
      ENDIF
      ExtensionStr = tFileName(iPos+1:iLen)
      CALL ILowerCase(ExtensionStr)
      SELECT CASE (ExtensionStr)
        CASE ('cif ')
          fmt = '-cif'
        CASE ('cssr')
          IF (CSSR2Mol2(tFileName) .NE. 1) RETURN
! Replace 'cssr' by 'mol2'
          tFileName = tFileName(1:iLen-4)//'mol2'
          iLen = LEN_TRIM(tFileName)
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
        IF (tFileName(I:I) .EQ. DIRSPACER) iStart = I + 1
      ENDDO
      CALL WCursorShape(CurHourGlass)
      CALL IOSCommand(InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'zmconv.exe'// &
        ' '//fmt(1:LEN_TRIM(fmt))//' "'//tFileName(iStart:iLen)//'"',3)
        CALL WCursorShape(CurCrossHair)
! Check return status
      OPEN(UNIT=145, FILE='MakeZmatrix.log',STATUS='OLD',IOSTAT = iStat)
      CLOSE(145)
      IF ((InfoError(1) .EQ. ErrOSCommand) .OR. (iStat .NE. 0)) THEN
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
      iLen = LEN_TRIM(tFileName)
      globFile = tFileName(1:iLen-ExtLen)//"glob"
      iHandle = 10
      OPEN(iHandle,FILE=globFile(1:LEN_TRIM(globFile)),STATUS='OLD',ERR=999)
      DO iFrg = 1, maxfrg
        gotzmfile(iFrg) = .FALSE.
      ENDDO
      iFrg = 0
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page6)
      CALL WDialogPutString(IDF_Xtal_File_Name,TheFileName)
      CALL PopActiveWindowID
 10   line = ''
      READ(iHandle,'(A)',END=100,ERR=999) line
      nl = LEN_TRIM(line)
      CALL ILowerCase(line(1:nl))
      CALL INextString(line,keychar)
      SELECT CASE (KeyChar(1:3))
        CASE ('cel')                                ! Cell parameters
     ! Ignore
     !     DO I = 1, 6
     !       CALL INextReal(line,CellPar(i))
     !     ENDDO
     !     CALL Upload_Cell_Constants
        CASE ('spa')
     ! Ignore
     !     DashHcvFile = line(ILocateChar(line):)
     !     HcvExists = .TRUE.
        CASE ('sym')
     ! Ignore
     !     DashHklFile = line(ILocateChar(line):)
        CASE ('z-m')                                ! Z-matrix file
          iFrg = iFrg + 1
          ! @@ Following lines do not take copies of Z-matrices into account
          zmNumberOfCopies(iFrg) = 1
          IF (iFrg .GT. maxfrg) GOTO 100
          frag_file(iFrg) = line(ILocateChar(line):)
          IF (Read_One_ZM(iFrg) .EQ. 0) THEN ! successful read
            gotzmfile(iFrg) = .TRUE.
          ELSE 
            gotzmfile(iFrg) = .FALSE. 
            CALL ErrorMessage("Error while reading Z-matrix file "//frag_file(iFrg)(1:LEN_TRIM(frag_file(iFrg))))
          ENDIF ! If the read on the Z-matrix was ok
        CASE ('cen')                                ! "Centre of mass"
          IF (.NOT. Get_UseCrystallographicCoM()) THEN
            IF (iFrg .NE. 0) THEN
              CALL INextString(line,keychar)
              CALL INextString(line,keychar)
              CALL INextReal(line,RR_tran(1,iFrg,1))
              CALL INextReal(line,RR_tran(2,iFrg,1))
              CALL INextReal(line,RR_tran(3,iFrg,1))
            ENDIF
          ENDIF
        CASE ('cry')                                ! "Crystallographic centre of mass"
          IF (Get_UseCrystallographicCoM()) THEN
            IF (iFrg .NE. 0) THEN
              CALL INextString(line,keychar)
              CALL INextString(line,keychar)
              CALL INextString(line,keychar)
              CALL INextReal(line,RR_tran(1,iFrg,1))
              CALL INextReal(line,RR_tran(2,iFrg,1))
              CALL INextReal(line,RR_tran(3,iFrg,1))
            ENDIF
          ENDIF
        CASE ('qua')                                ! "Quaternion"
          IF (iFrg .NE. 0) THEN
            CALL INextReal(line,RR_rot(1,iFrg,1))
            CALL INextReal(line,RR_rot(2,iFrg,1))
            CALL INextReal(line,RR_rot(3,iFrg,1))
            CALL INextReal(line,RR_rot(4,iFrg,1))
          ENDIF
      END SELECT
      GOTO 10 
 100  CLOSE(iHandle)
      !C Basically a Q&D hack: fill BestValuesDoF(:,1) because we already have a routine that can 
      !C translate BestValuesDoF into a fully functional Rietveld refinement window.
      KK = 0
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            ! Translations
            BestValuesDoF(KK+1,1) = RR_tran(1,iFrg,iFrgCopy)
            BestValuesDoF(KK+2,1) = RR_tran(2,iFrg,iFrgCopy)
            BestValuesDoF(KK+3,1) = RR_tran(3,iFrg,iFrgCopy)
            KK = KK +3
            ! Rotations
            BestValuesDoF(KK+1,1) = RR_rot(1,iFrg,iFrgCopy)
            BestValuesDoF(KK+2,1) = RR_rot(2,iFrg,iFrgCopy)
            BestValuesDoF(KK+3,1) = RR_rot(3,iFrg,iFrgCopy)
            BestValuesDoF(KK+4,1) = RR_rot(4,iFrg,iFrgCopy)
            KK = KK +4
            ! Torsions
            DO I = 1, natoms(iFrg)
              IF (IOPTB(I,iFrg) .EQ. 1) THEN
                KK = KK + 1
                BestValuesDoF(KK,1) = BLEN(I,iFrg) ! No copies taken into account!!!!
              ENDIF
              IF (IOPTA(I,iFrg) .EQ. 1) THEN
                KK = KK + 1
                BestValuesDoF(KK,1) = ALPH(I,iFrg) ! No copies taken into account!!!!
              ENDIF
              IF (IOPTT(I,iFrg) .EQ. 1) THEN
                KK = KK + 1
                BestValuesDoF(KK,1) = BET(I,iFrg) ! No copies taken into account!!!!
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      CALL SA_Parameter_Set
      CALL Create_AtomicWeightings
      CALL FillSymmetry_2
      CALL GET_LOGREF
      RETURN
 999  CALL ErrorMessage('Error reading crystal structure file.')
      CLOSE(iHandle) 

      END SUBROUTINE XtalFileLoad
!
!*****************************************************************************
!
