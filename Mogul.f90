      
      SUBROUTINE WriteMogulMol2(iFRow)

! Writes Mol2 file for MOGUL.  
! Calls GetTorsionLineNumbers	
  
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFRow

      INTEGER, EXTERNAL :: WriteMol2
      INTEGER I,K
      CHARACTER(MaxPathLength) temp_file
      INTEGER tLength, BondNr
	  INTEGER iFrg, DoF


!     Given the number of the parameter want to know which zmatrix, fragment it belongs to.
      iFrg = 0
      DO i = 1, maxDOF
        DO k = 1, nfrag
          IF (IFRow .EQ. zm2par(i,k)) THEN
            DoF = i
            iFrg = k
            EXIT
          ENDIF
        ENDDO
        IF (iFrg .NE. 0) EXIT
      ENDDO


      natcry = NATOMS(iFrg)
      CALL makexyz(natcry,BLEN(1, iFrg),ALPH(1, iFrg),BET(1, iFrg),IZ1(1, iFrg),IZ2(1, iFrg),IZ3(1, iFrg),axyzo)
      DO I = 1, natcry
        aelem(I) = zmElementCSD(I, iFrg)
        atomlabel(I) = OriginalLabel(I, iFrg)
      ENDDO
      nbocry = NumberOfBonds(iFrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr, iFrg)
        bond(BondNr,1) = Bonds(1,BondNr, iFrg)
        bond(BondNr,2) = Bonds(2,BondNr, iFrg)
      ENDDO
      tLength = LEN_TRIM(frag_file(iFrg))
      temp_file = frag_file(iFrg)(1:tLength-8)//'_mogul.mol2'

! Write mol2 file
      IF (WriteMol2(temp_file,.FALSE., iFrg) .EQ. 1) THEN
        CALL GetTorsionLineNumbers(temp_file, IFrg, DoF)
      ELSE
        CALL DebugErrorMessage('Error writing temporary file.')
      ENDIF


      END SUBROUTINE WriteMogulMol2

!*****************************************************************

      SUBROUTINE GetTorsionLineNumbers(temp_file, iFrg, DoF)

! For Torsion Angle, gets corresponding line numbers of atoms from Mol2
! file.  MOGUL does not use Atom Labels but AtomIDs.
! Calls WriteMogulScript.
	  
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg, DoF
	  CHARACTER(MaxPathLength), INTENT(IN   ) :: temp_file

	  CHARACTER*36 TempTorsionLabel
      
	  CHARACTER*5, Atom(4)
	  INTEGER Marker(5), AtomID(4)
	  INTEGER I,J

	  TempTorsionLabel = czmpar(DoF, iFrg)
      I = 0
	  J = 1
	  DO WHILE (I .LE. 36)
        I = I + 1
	    IF(TempTorsionLabel(I:I) .EQ. "(" ) THEN
		  Marker(J) = I
		  J = J+1
		ENDIF
	    IF(TempTorsionLabel(I:I) .EQ. ":" ) THEN
		  Marker(J) = I
		  J = J + 1
		ENDIF		
	    IF(TempTorsionLabel(I:I) .EQ. ")" ) THEN
		  Marker(J) = I
		  EXIT
		ENDIF
      ENDDO


      Atom(1) = TempTorsionLabel(Marker(1)+1 : Marker(2)-1) 
	  Atom(2) = TempTorsionLabel(Marker(2)+1 : Marker(3)-1)
	  Atom(3) = TempTorsionLabel(Marker(3)+1 : Marker(4)-1)
	  Atom(4) = TempTorsionLabel(Marker(4)+1 : Marker(5)-1)

      DO J = 1,4 ! Mogul does not use atom labels but number of atom in Mol2 file 
	    DO I = 1, MaxDoF
          IF(Atom(J) .EQ. AtomLabel(izmbid(I,IFrg))) THEN
            AtomID(J) = I
			EXIT 
		  ENDIF
		ENDDO
	  ENDDO

	  CALL WriteMogulScript(temp_file, AtomID)  

	  END SUBROUTINE GetTorsionLineNumbers
	 
	 
!*****************************************************************	 
	 
	  SUBROUTINE WriteMogulScript(temp_file, AtomID)

! Writes the Mogul Script file which contains instructions for Mogul such
! as molecule file name, output filename, Torsion Angle Fragment.
! Calls Mogul
	  
	  USE WINTERACTER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

	  INTEGER, DIMENSION(4), INTENT (IN   )   :: AtomID
	  CHARACTER(MaxPathLength), INTENT(IN   ) :: temp_file


      INTEGER I
      CHARACTER(MaxPathLength) CurrentDirectory, Script_file, MogulOutputFile
	  INTEGER tLength

      CALL IOsDirName(CurrentDirectory)

      tLength = LEN_TRIM(temp_file)
      Script_file = temp_file(1:tLength-10)//'script.qf'
	  MogulOutputFile = temp_file(1:tLength-10)//'mogul.out'

!	  Script_file = CurrentDirectory(1:LEN_TRIM(CurrentDirectory))//DIRSPACER//'Mogul_script.qf'
!     MogulOutputFile = CurrentDirectory(1:LEN_TRIM(CurrentDirectory))//DIRSPACER//'MogulOutput.txt'
	  OPEN(240,FILE=Script_file,STATUS='UNKNOWN', ERR = 999)
      WRITE(240,10) temp_file
10    FORMAT(('MOGUL MOLECULE'), 1X, A255)
      WRITE(240,20) MogulOutputFile 
20	  FORMAT(('MOGUL OUTPUT_FILE '), A255)
	  WRITE(240,30) (AtomID(I), I = 1,4)
30    FORMAT(('TORSION '), 4(I3,1X))
      WRITE(240,40)
40	  FORMAT(('MOGUL GUI OPEN'))
	  
	  CLOSE (240)

      CALL Mogul(Script_file)
	  RETURN	   

999   CALL ErrorMessage('Error generating Mogul Script file')
      RETURN

	  END SUBROUTINE WriteMogulScript

!********************************************************************************

	  SUBROUTINE Mogul(Script_file)

! Calls command to execute Mogul.  Path to Mogul in Configuration Window
	  
	  USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES


      IMPLICIT NONE

	  CHARACTER(MaxPathLength), INTENT(IN   ) :: Script_file
      INTEGER I,M
      LOGICAL exists

      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical

	  CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogGetString(IDF_MogulExe,MOGULEXE)
      CALL PopActiveWindowID
      I = LEN_TRIM(MOGULEXE)

      IF (I .NE. 0) UseMogul = .TRUE. 
	  IF (UseMogul .EQ. .FALSE.) RETURN

      IF (I .EQ. 0) THEN
	    IF (Confirm('Do you intend to use Mogul?')) THEN
		  UseMogul = .TRUE.
          CALL ErrorMessage("DASH could not launch Mogul. The path to the Mogul exe is not specified."//CHAR(13)//&
                          "This can be changed in the Configuration... window"//CHAR(13)//&
                          "under Options in the menu bar.")      
        
          RETURN
		ELSE
		 UseMogul = .FALSE.
		 RETURN
		ENDIF
      ENDIF
      INQUIRE(FILE = MOGULEXE(1:I),EXIST=exists)
      IF (.NOT. exists) GOTO 999
      M = InfoError(1) ! Clear errors
      CALL IOSCommand(MOGULEXE(1:I)//' -ins '//'"'//Script_file(1:LEN_TRIM(Script_file))//'"')
      IF (InfoError(1) .NE. 0) GOTO 999
      RETURN
999   CALL ErrorMessage("DASH could not launch Mogul. The Mogul executable is currently configured"//CHAR(13)//&
                        "to launch the program "//MOGULEXE(1:I)//CHAR(13)//&
                        "This can be changed in the Configuration... window"//CHAR(13)//&
                        "under Options in the menu bar.")


      END SUBROUTINE Mogul



!********************************************************************************
