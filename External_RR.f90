!
!*****************************************************************************
!
      SUBROUTINE StartExternalRR

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      LOGICAL, EXTERNAL :: CheckTOPASFileName, WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: WriteTOPASFilePawley, WriteGSASFiles, Launch_GSAS
      INTEGER, EXTERNAL :: WriteRIETANFiles, Launch_RIETAN
      CHARACTER(LEN=45) :: FILTER
      CHARACTER(MaxPathLength) :: tDirName, tFileName
      INTEGER iFlags

      iFlags = SaveDialog + AppendExt + PromptOn
      ext_RR_stage = 1
      SELECT CASE ( iRietveldMethod ) 
        CASE ( FOR_TOPAS )
          ext_RR_input_file_name = TRIM(OutputFilesBaseName)//'.inp'
!         CALL CheckTOPASFileName(ext_RR_input_file_name)
          FILTER = 'TOPAS input file (*.inp)|*.inp|'
          CALL WSelectFile(FILTER, iFlags, ext_RR_input_file_name, 'Save TOPAS input file')
          IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (LEN_TRIM(ext_RR_input_file_name) .NE. 0)) THEN
!           IF ( CheckTOPASFileName(ext_RR_input_file_name) ) &
!             CALL InfoMessage("TOPAS cannot cope with file names containing dots,"//CHAR(13)//&
!                              "DASH has replaced these by underscores.")
          IF ( WriteTOPASFilePawley(ext_RR_input_file_name) .EQ. 0 ) THEN
            ext_RR_stage = ext_RR_stage + 1
            CALL WDialogSelect(IDD_SAW_Page7_TOPAS)
            CALL WDialogPutString(IDF_Ext_RR_inp_file_name, ext_RR_input_file_name)
            CALL WDialogPutCheckBoxLogical(IDC_UseDASHRecommendation, .TRUE.)
            CALL WDialogFieldState(IDC_Anisotropic_broadening, Enabled)
            CALL UpdateTOPASCheckBoxes()
            CALL WizardWindowShow(IDD_SAW_Page7_TOPAS)
          ENDIF
        ENDIF
      CASE ( FOR_GSAS )
        ext_RR_input_file_name = TRIM(OutputFilesBaseName)//'.exp'
        FILTER = 'GSAS exp file (*.exp)|*.exp|'
        CALL WSelectFile(FILTER, iFlags, ext_RR_input_file_name, 'Save GSAS exp file')
        IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (LEN_TRIM(ext_RR_input_file_name) .NE. 0)) THEN
          CALL SplitPath(ext_RR_input_file_name, tDirName, tFileName)
          IF ( scan(TRIM(tFileName), ' ') .NE. 0 ) GOTO 999
          CALL WDialogLoad(IDD_SAW_Page7_GSAS)
          CALL UpdateGSASCheckBoxes()
          CALL WDialogPutInteger(IDF_NCYCL, 3)
          IF ( WriteGSASFiles(ext_RR_input_file_name) .EQ. 0 ) THEN
            IF ( Launch_GSAS(ext_RR_input_file_name, .FALSE.) .EQ. 0 ) THEN
              ext_RR_stage = ext_RR_stage + 1
              CALL WDialogSelect(IDD_SAW_Page7_GSAS)
              CALL WDialogPutString(IDF_Ext_RR_inp_file_name, ext_RR_input_file_name)
              CALL WDialogPutCheckBoxLogical(IDC_UseDASHRecommendation, .TRUE.)
              CALL UpdateGSASCheckBoxes()
              CALL WizardWindowShow(IDD_SAW_Page7_GSAS)
            ENDIF
          ENDIF
        ENDIF
      CASE ( FOR_RIETAN )
        is_Rietan_FP = ( INDEX(RIETANEXE, '-FP') .GT. 0 .OR. INDEX(RIETANEXE, '-fp') .GT. 0 )
        ext_RR_input_file_name = TRIM(OutputFilesBaseName)//'.ins'
        FILTER = 'RIETAN ins file (*.ins)|*.ins|'
        CALL WSelectFile(FILTER, iFlags, ext_RR_input_file_name, 'Save RIETAN ins file')
        IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (LEN_TRIM(ext_RR_input_file_name) .NE. 0)) THEN
          CALL WDialogLoad(IDD_SAW_Page7_RIETAN)
          CALL UpdateRIETANCheckBoxes()
          CALL WDialogPutInteger(IDF_NCYCL, 10)
          IF ( WriteRIETANFiles(ext_RR_input_file_name) .EQ. 0 ) THEN
            IF ( Launch_RIETAN(ext_RR_input_file_name) .EQ. 0 ) THEN
              ext_RR_stage = ext_RR_stage + 1
              CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
              CALL WDialogPutString(IDF_Ext_RR_inp_file_name, ext_RR_input_file_name)
              CALL WDialogPutCheckBoxLogical(IDC_UseDASHRecommendation, .TRUE.)
              CALL UpdateRIETANCheckBoxes()
              CALL WizardWindowShow(IDD_SAW_Page7_RIETAN)
            ENDIF
          ENDIF
        ENDIF
      END SELECT
      RETURN

 999  CALL ErrorMessage(TRIM(tFileName)//CHAR(13)// &
                        'GSAS can not handle file name with space.')
      RETURN

      END SUBROUTINE StartExternalRR
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardExtRR

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_RR_External)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SAW_Page6a)
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
              ENDIF
              CALL EndWizardPastPawley
            CASE (IDNEXT)
!              iRietveldMethod = FOR_TOPAS
              ext_RR_stage = 1
              CALL WizardWindowShow(IDD_PW_Page3)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardExtRR
!
!*****************************************************************************
!
      SUBROUTINE CopyPattern2Backup

      USE DRUID_HEADER
      USE TAVAR
      USE REFVAR
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                   XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS, XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL               TALAMBDA
      INTEGER                      TARADIATION, TANOBS
      REAL                                              TAXOBS,       TAYOBS,       TAEOBS
      COMMON /TAPROFOBS/ TALAMBDA, TARADIATION, TANOBS, TAXOBS(MOBS), TAYOBS(MOBS), TAEOBS(MOBS)

      INTEGER ExtLength
      CHARACTER(MaxPathLength) tDirName, tFileNameRoot
      CHARACTER*8 tExtension
      LOGICAL exists

      ! Perhaps this pair of functions should also set and reset the iRietveldMethod variable
      TANOBS = NOBS
      TAXOBS(1:NOBS) = XOBS(1:NOBS)
      TAYOBS(1:NOBS) = YOBS(1:NOBS)
      TAEOBS(1:NOBS) = EOBS(1:NOBS)
      TALAMBDA = ALambda
      TARADIATION = JRadOption
      old_NumOfRef = NumOfRef 
      NumOfRef = 0
      CALL Profile_Plot
      CALL PushActiveWindowID
      ! Must clear old file name and grey out the 'Next' button
      CALL WDialogSelect(IDD_PW_Page3)
      CALL WDialogGetString(IDF_PWa_DataFileName_String, old_diffraction_data_file_name)
!      CALL WDialogClearField(IDF_PWa_DataFileName_String)
      IF ( LEN_TRIM(old_diffraction_data_file_name) .GT. 0 ) THEN
        ExtLength = MIN(LEN_TRIM(old_diffraction_data_file_name), LEN(tExtension))
        CALL SplitPath2(old_diffraction_data_file_name, tDirName, tFileNameRoot, &
                        tExtension, ExtLength)
        IF ( tExtension .EQ. 'dash' ) THEN
          tFileNameRoot = TRIM(tFileNameRoot)//'.xye'
          INQUIRE(FILE=tFileNameRoot, EXIST=exists)
          IF ( .NOT. exists ) tFileNameRoot = ' '
          CALL WDialogPutString(IDF_PWa_DataFileName_String, TRIM(tFileNameRoot))
        ENDIF
      ELSE IF ( LEN_TRIM(DashRawFile) .GT. 0 ) THEN
        CALL WDialogPutString(IDF_PWa_DataFileName_String, DashRawFile)
      ENDIF
      CALL WDialogFieldState(IDNEXT, Disabled)
      ! Uncheck the "Truncate pattern at end" checkbox (but we don't store its current state)
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogPutCheckBoxLogical(IDF_TruncateEndYN, .FALSE.)
      CALL WDialogFieldState(IDF_Max2Theta, Disabled)
      CALL WDialogFieldState(IDF_MaxResolution, Disabled)
      CALL WDialogFieldState(IDB_Convert, Disabled)
      ! Enable the "Monochromated" checkbox
      CALL WDialogSelect(IDD_PW_Page4)
      CALL WDialogFieldState(IDC_Monochromated, Enabled)
      CALL PopActiveWindowID

      END SUBROUTINE CopyPattern2Backup
!
!*****************************************************************************
!
      SUBROUTINE CopyBackup2Pattern

      USE DRUID_HEADER
      USE WINTERACTER
      USE TAVAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                   XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS, XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL               TALAMBDA
      INTEGER                      TARADIATION, TANOBS
      REAL                                              TAXOBS,       TAYOBS,       TAEOBS
      COMMON /TAPROFOBS/ TALAMBDA, TARADIATION, TANOBS, TAXOBS(MOBS), TAYOBS(MOBS), TAEOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      NOBS = TANOBS
      XOBS(1:NOBS) = TAXOBS(1:NOBS)
      YOBS(1:NOBS) = TAYOBS(1:NOBS)
      EOBS(1:NOBS) = TAEOBS(1:NOBS)
      ALambda = TALAMBDA
      JRadOption = TARADIATION
      NumOfRef = old_NumOfRef
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page3)
      CALL WDialogPutString(IDF_PWa_DataFileName_String, old_diffraction_data_file_name)
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogPutCheckBoxLogical(IDF_TruncateEndYN, .TRUE.)
      CALL WDialogFieldState(IDF_Max2Theta, Enabled)
      CALL WDialogFieldState(IDF_MaxResolution, Enabled)
      CALL WDialogFieldState(IDB_Convert, Enabled)
      ! Disable the "Monochromated" checkbox
      CALL WDialogSelect(IDD_PW_Page4)
      CALL WDialogPutCheckBox(IDC_Monochromated, Checked)
      CALL WDialogFieldState(IDC_Monochromated, Disabled)
      CALL PopActiveWindowID
      ! Must also restore Rebin_Profile
      LBIN = 1
      CALL Rebin_Profile()
      CALL Profile_Plot

      END SUBROUTINE CopyBackup2Pattern
!
!*****************************************************************************
!
      SUBROUTINE find_aromatic_assembly(the_assembly)

      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: the_assembly(1:MaxAtm_3)

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 300)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

      INTEGER, EXTERNAL :: PopAA
      LOGICAL, EXTERNAL :: assembly_contains, AAStackEmpty, has_aromatic_bond
      INTEGER Ncon, i, iAtom, iAtom2
      INTEGER Icon(30), Icob(30)

   10 CONTINUE
      ! Get the atom from the top of the stack...
      iAtom = PopAA()
      ! Check that it is not yet in the assembly
      IF ( assembly_contains(the_assembly, iAtom) ) GOTO 20
      ! Add the atom to the assembly
      CALL assembly_add(the_assembly, iAtom)
      ! Get its neighbours
      CALL SAMCON(iAtom, Ncon, Icon, Icob, 0)
      ! Loop over neighbours
      DO i = 1, Ncon
        iAtom2 = Icon(i)
        ! Check that the neighbour is not in the assembly yet
        IF ( .NOT. assembly_contains(the_assembly, iAtom2) ) THEN
          ! If the neighbour has no aromatic bonds, add it to the assembly
          IF ( .NOT. has_aromatic_bond(iAtom2) ) THEN
            CALL assembly_add(the_assembly, iAtom2)
          ELSE
            ! Otherwise, push the neighbour onto the stack
            CALL PushAA(iAtom2)
          ENDIF
        ENDIF
      ENDDO
   20 CONTINUE
      ! If stack now empty, RETURN
      IF ( AAStackEmpty() ) RETURN
      GOTO 10

      END SUBROUTINE find_aromatic_assembly
!
!*****************************************************************************
!
      SUBROUTINE PushAA(the_value)
! AA = Aromatic Assembly
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: the_value

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 300)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

! Check if stack full
      IF ( AAStackPtr .EQ. 0 ) THEN
        CALL DebugErrorMessage('AAStackPtr full.')
        RETURN
      ENDIF
! If not, store value
      AAStack(AAStackPtr) = the_value
! Dec(StackPtr)
      AAStackPtr = AAStackPtr - 1

      END SUBROUTINE PushAA
!
!*****************************************************************************
!
      INTEGER FUNCTION PopAA()

      IMPLICIT NONE

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 300)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

! Check if stack empty
      IF ( AAStackPtr .EQ. MaxAAStack ) THEN
        CALL DebugErrorMessage('AAStack empty.')
        PopAA = 0
        RETURN
      ENDIF
! If not, Inc(StackPtr)
      AAStackPtr = AAStackPtr + 1
      PopAA = (AAStack(AAStackPtr))

      END FUNCTION PopAA
!
!*****************************************************************************
!
      LOGICAL FUNCTION AAStackEmpty()

      IMPLICIT NONE

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 300)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

      AAStackEmpty = ( AAStackPtr .EQ. MaxAAStack )

      END FUNCTION AAStackEmpty
!
!*****************************************************************************
!
      LOGICAL FUNCTION PutAtomsForSpecailPosition()

      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      LOGICAL, EXTERNAL :: PutAtoms, SPWriteAtom
      INTEGER hSP_in_file
      PARAMETER ( hSP_in_file = 115 )
      INTEGER ii

      ! Initialise to failure
      PutAtomsForSpecailPosition = .TRUE.

      OPEN(UNIT=hSP_in_file, FILE="special_positions.in", STATUS='unknown', ERR=999)
      WRITE(hSP_in_file, '(A)', ERR=999) 'TOLE 0.15'
      WRITE(hSP_in_file, '(A,6(F8.4,1X))', ERR=999) 'CELL ', (CellPar(ii),ii=1,6)
      DO ii = 1, npdbops
        WRITE (hSP_in_file, '(A)', ERR=999) 'SYMM '//TRIM(cpdbops(ii))
      ENDDO
      IF ( PutAtoms(hSP_in_file, SPWriteAtom) ) GOTO 999
      CLOSE(hSP_in_file)
      ! Run the special positions program
      CALL IOSCommand(TRIM(InstallationDirectory)//'special_positions.exe '// &
        '"special_positions.in"', ProcSilent+ProcBlocked)
      PutAtomsForSpecailPosition = .FALSE.

  999 RETURN

      END FUNCTION PutAtomsForSpecailPosition
!
!*****************************************************************************
!
      LOGICAL FUNCTION SPWriteAtom(hFile, iElement, Label, XYZ, Occ, Biso)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iElement
      CHARACTER*(*), INTENT (IN) :: Label
      REAL, INTENT (IN) :: XYZ(3), Occ, Biso

      INTEGER i

      SPWriteAtom = .TRUE.
      WRITE(hFile, '(A,3(X,F9.5),2(X,F6.3))', ERR=999) 'SITE '//Label, (XYZ(i),i=1,3), Occ, Biso
      SPWriteAtom = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElement .EQ. 0 ) RETURN 

      END FUNCTION SPWriteAtom
!
!*****************************************************************************
!
      LOGICAL FUNCTION PutAtoms(hFile, WriteAtom)

      USE ATMVAR
      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile

      LOGICAL, EXTERNAL :: WriteAtom
      CHARACTER*20, EXTERNAL :: Integer2String

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER iTotal, iFrg, iAtom
      CHARACTER(7) LabelStr

      ! Initialise to failure
      PutAtoms = .TRUE.
      iTotal = 0
      DO iFrg = 1, nFrag
        DO iAtom = 1, natoms(iFrg)
          CALL GenerateAtomLabel(zmElementCSD(iAtom,iFrg), iTotal+iAtom, LabelStr)
          IF ( WriteAtom(hFile, zmElementCSD(iAtom,iFrg), LabelStr, &
                         Xato(1,OrderedAtm(iTotal + iAtom)), occ(iAtom,iFrg), &
                         tiso(iAtom,iFrg)) ) GOTO 999
        ENDDO
        iTotal = iTotal + natoms(iFrg)
      ENDDO
      PutAtoms = .FALSE.
  999 RETURN

      END FUNCTION PutAtoms
!
!*****************************************************************************
!
      LOGICAL FUNCTION PutRestraints(hFile, WriteDistance, WriteAngle, WritePlane)

      USE ATMVAR
      USE ZMVAR
      USE RRVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile

      LOGICAL, EXTERNAL :: WriteDistance, WriteAngle, WritePlane

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 300)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

      INTEGER, EXTERNAL :: StrFind, assembly_size
      CHARACTER*20, EXTERNAL :: Integer2String
      LOGICAL, EXTERNAL :: assembly_contains, has_aromatic_bond
      INTEGER j
      INTEGER iTotal, iFrg, iAtom, iAtom1, iAtom2
      REAL    distance, angle
      INTEGER Ncon, j1, j2
      INTEGER Icon(30), Icob(30)
      INTEGER flatten_members(MaxAtm_3), flatten_elements(MaxAtm_3)
      INTEGER nFlatten
      ! We need to remember which atoms have been assigned to an assembly
      INTEGER sum_of_assemblies(1:MaxAtm_3)
      INTEGER current_assembly(1:MaxAtm_3)

      ! Initialise to failure
      PutRestraints = .TRUE.
      iTotal = 0
      DO iFrg = 1, nFrag
        ! Convert internal coordinates to orthogonal coordinates
        CALL makexyz(natoms(iFrg),RR_blen(1,iFrg),RR_alph(1,iFrg),RR_bet(1,iFrg),        &
                     IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),axyzo)
        DO iAtom = 1, natoms(iFrg)
          aelem(iAtom) = zmElementCSD(iAtom,iFrg)
        ENDDO
        natcry = natoms(iFrg)
        ! Detect bonds and their types (to find benzene rings for Flatten macro)
        CALL SAMABO()
        ! ##### Distance restraints #####
        DO j = 1, nbocry
          iAtom1 = bond(j,1)
          iAtom2 = bond(j,2)
          CALL PLUDIJ(iAtom1, iAtom2, distance)
          IF ( WriteDistance(hFile, iTotal+iAtom1, iTotal+iAtom2, &
                             zmElementCSD(iAtom1,iFrg), zmElementCSD(iAtom2,iFrg), &
                             distance) ) GOTO 999
        ENDDO
        ! ##### Angle restraints #####
        DO iAtom = 1, natoms(iFrg)
          CALL SAMCON(iAtom, Ncon, Icon, Icob, 0)
! NCON        output number of connected atoms for iAtom
! ICON (1:30) output list of atoms connected to iAtom
! ICOB (1:30) output bond types for each connection in ICON
          DO j1 = 1, Ncon-1
            DO j2 = j1+1, Ncon
              iAtom1 = Icon(j1)
              iAtom2 = Icon(j2)
              CALL SAMANF(iAtom1, iAtom, iAtom2, angle)
              IF ( WriteAngle(hFile, iTotal+iAtom1, iTotal+iAtom, iTotal+iAtom2, &
                              zmElementCSD(iAtom1,iFrg), zmElementCSD(iAtom,iFrg), &
                              zmElementCSD(iAtom2,iFrg), angle) ) GOTO 999
            ENDDO
          ENDDO
        ENDDO
        ! ##### Flatten #####
        sum_of_assemblies = 0
        ! Loop over atoms
        DO iAtom = 1, natoms(iFrg)
          ! If in a previous assembly, continue to next atom
          IF ( assembly_contains(sum_of_assemblies, iAtom) ) CYCLE
          ! If atom does not have any aromatic bond attached to it, continue to next atom
          IF ( .NOT. has_aromatic_bond(iAtom) ) CYCLE
          ! Otherwise, use this atom as the start of the algorithm.
          current_assembly = 0 ! Empty the current assembly
          AAStackPtr = MaxAAStack ! Empty the stack
          ! Push the atom onto the stack
          CALL PushAA(iAtom)
          ! Then call the subroutine who does all the work.
          CALL find_aromatic_assembly(current_assembly)
          ! Write out this assembly with a flatten command
          ! We could add a test to check that the atoms that we have added are in a plane to start with
          ! This is not necessarily so: e.g. in a buckyball or in one of those fused benzene rings spiral
          nFlatten = 0
          DO j = 1, MaxAtm_3
            iAtom1 = current_assembly(j)
            IF ( iAtom1 .NE. 0 ) THEN
              nFlatten = nFlatten + 1
              flatten_members(nFlatten) = iTotal+iAtom1
              flatten_elements(nFlatten) = zmElementCSD(iAtom1,iFrg)
            ENDIF
          ENDDO
          IF ( nFlatten .GT. 3 ) THEN
            IF ( WritePlane(hFile, nFlatten, flatten_members, flatten_elements) ) GOTO 999
          ENDIF
          ! Add current assembly to sum_of_assemblies
          DO j = 1, nFlatten
            CALL assembly_add(sum_of_assemblies, current_assembly(j))
          ENDDO
        ENDDO  !iAtom
        iTotal = iTotal + natoms(iFrg)
      ENDDO  ! iFrg
      PutRestraints = .FALSE.
  999 RETURN

      END FUNCTION PutRestraints
!
!*****************************************************************************
!
      SUBROUTINE Launch_Viewer(file_name, ext_str)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: file_name, ext_str

      CHARACTER(MaxPathLength) tDirName, tFileName
      CHARACTER*8 tExtension
      INTEGER ExtLength
      LOGICAL exists

      ExtLength = LEN(tExtension)
      CALL SplitPath2(file_name, tDirName, tFileName, tExtension, ExtLength)
      tFileName = TRIM(tDirName)//TRIM(tFileName)//ext_str
      INQUIRE(FILE=tFileName, EXIST=exists)
      IF ( .NOT. exists ) GOTO 999
      CALL ViewStructure(tFileName)
      RETURN
999   CALL ErrorMessage('DASH could not launch viewer because the required file does not '//&
                        'exist:'//CHAR(13)//&
                        TRIM(tFileName)//CHAR(13)// &
                        'To produce this file, run the external refine program '// &
                        CHAR(13)// &
                        'at least once with atoms included')
      RETURN

      END SUBROUTINE Launch_Viewer
!
!*****************************************************************************
!
