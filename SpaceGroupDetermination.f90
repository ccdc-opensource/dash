      
      SUBROUTINE SpaceGroupDeterminationCode(LatBrav, PawleyChiSqd)

      USE VARIABLES
      USE WINTERACTER
      
      IMPLICIT NONE

      INTEGER LatBrav, errcode
      CHARACTER(MaxPathLength) :: CurrentDirectory
      CHARACTER(MaxPathLength), PARAMETER :: ExtSymEXE = 'ExtinctionSymbol'//CCDC_EXE_EXT
      INTEGER IHANDLE
      REAL PawleyChiSqd
      REAL Correction
      LOGICAL Exists

      INQUIRE(FILE=TRIM(InstallationDirectory)//DIRSPACER//ExtSymEXE,EXIST=exists)
      IF (.NOT. exists) GOTO 777

      CALL IosDirName(CurrentDirectory)

! Write parameter_input file to working directory
      OPEN(220,FILE=TRIM(CurrentDirectory)//DIRSPACER//'parameter_input.asc',STATUS='UNKNOWN', ERR = 999)
      WRITE(220,*)
      WRITE(220,'("Below simply specify the input data file name and the Laue class of the data.")')
      WRITE(220,*)
      WRITE(220,*)
      WRITE(220,'("INPUT DATA FILE:")')
      WRITE(220,'(A)') (TRIM(CurrentDirectory)//DIRSPACER//'polyp.hkl')
      WRITE(220,*)
      WRITE(220,'("LAUE CLASS:")')
      SELECT CASE(LatBrav)
         CASE(1)
           CALL ErrorMessage('Triclinic--Space Group Determination program not applicable')
           CLOSE(220)
           CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'parameter_input.asc')
          RETURN
         CASE(2)
           WRITE(220,'("a")')
         CASE(3)
           WRITE(220,'("b")')
         CASE(4)
           WRITE(220,'("c")')
         CASE(5)
           WRITE(220,'("o")')
         CASE(6)
           WRITE(220,'("t")')
         CASE(7)
           WRITE(220,'("r")')
         CASE(8)
           WRITE(220,'("r")')
         CASE(9)
           WRITE(220,'("h")')
         CASE(10)
           WRITE(220,'("d")')
      END SELECT 
      WRITE(220,*)
      WRITE(220,*)('------------------------------------------------------------')
      WRITE(220,*)
      WRITE(220,*)('The possible Laue class symbols which can be used above are:')
      WRITE(220,*)('           "o" - orthorhombic')
      WRITE(220,*)('           "b" - monoclinic - unique axis b')
      WRITE(220,*)('           "c" - monoclinic - unique axis c')
      WRITE(220,*)('           "a" - monoclinic - unique axis a')
      WRITE(220,*)('           "t" - tetragonal')
      WRITE(220,*)('           "r" - rhombohedral')
      WRITE(220,*)('           "h" - hexagonal')
      WRITE(220,*)('           "d" - cubic')
      CLOSE(220)
! Calculate increase in error: (PawleyChisqd)^1/2 * sigma
     IF ( PawleyChiSqd .GT. 1.0 ) THEN
       Correction = (SQRT(PawleyChiSqd))*3
     ELSE 
       Correction = 1.0
     ENDIF

! Write ADVANCED.ASC to working directory
      OPEN(220,FILE=TRIM(CurrentDirectory)//DIRSPACER//'advanced.asc',STATUS='UNKNOWN', ERR = 999)
      WRITE(220,*)
      WRITE(220,'("The parameters below can be used to affect the execution time of the program (with a ")')
      WRITE(220,'("corresponding effect on the accuracy of the calculated numbers) and to possibly modify ")')
      WRITE(220,'("the input data file if required  ")')
      WRITE(220,*)
      WRITE(220,'("MODIFY INPUT DATA FILE")')
      WRITE(220,'("----------------------")')
!      WRITE(220,'("1.0         increase error")')
      WRITE(220, 230) Correction
230   FORMAT(F5.1)
      WRITE(220,*)
      WRITE(220,*)
      WRITE(220,'("ADJUST EXECUTION SPEED AND ACCURACY OF CALCULATED NUMBERS")')
      WRITE(220,'("--------------------------------------------------------")')
      WRITE(220,'("40          correlation cutoff value")')
      WRITE(220,'("100000      max Monte Carlo iterations")')
      WRITE(220,'("2           tolerance")')
      WRITE(220,*)
      WRITE(220,*)
      WRITE(220,'("--------------------------------------------------------")')
      CLOSE(220)

! Check parameter file exists
     INQUIRE(FILE=TRIM(CurrentDirectory)//DIRSPACER//'parameter_input.asc',EXIST=exists)
     IF (.NOT. exists) GOTO 999
! Check ADVANCED.ASC exists.... 
     INQUIRE(FILE=TRIM(CurrentDirectory)//DIRSPACER//'advanced.asc',EXIST=exists)
     IF (.NOT. exists) GOTO 999
! Before calling the executable in the DASH installation directory.
     CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'table.asc')
     errcode = InfoError(1) 
     CALL IOsCommand(TRIM(InstallationDirectory)//ExtSymEXE,2)
     errcode = InfoError(1) 
#ifdef _WIN32
     IF (errcode .EQ. ErrOsCommand) GOTO 888 ! error ocurred 
#else
     ! ExtSymEXE always returns non-zero. This makes an ErrOsCommand returned by
     ! IOsCommand(), but only on Linux.
     IF (errcode .EQ. ErrOsCommand .AND. .FALSE.) GOTO 888 ! error ocurred 
#endif
     INQUIRE(FILE=TRIM(CurrentDirectory)//DIRSPACER//'table.asc',EXIST=exists)
     IF (.NOT. exists) GOTO 999
     
     CALL WindowOpenChild(IHANDLE)
     CALL WEditFile(TRIM(CurrentDirectory)//DIRSPACER//'table.asc',Modeless,0,FileMustExist,4)
! This editor window is not view only.  The three consequences outlined by JvdS for the SAParameter
! window apply:
! 1. The file can be edited. The user can add a title, for instance.
! 2. The file can be saved using the 'Save as...' option from the menu and hence it 
! can be called something a little more sensible. The original table.asc will still 
! exist though.
! 3. The file cannot be accessed by DASH while it is being viewed by the user. 

     CALL SetChildWinAutoClose(IHANDLE)
! Deletes the Default parameter file.  Try to keep this hidden from user?  Means that they
! won't be able to change the parameters     
!     CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'advanced.asc')

     RETURN

999  CALL ErrorMessage('Error with generation or reading of required input/output files')
     RETURN
888  CALL ErrorMessage('Error occurred during execution of Space Group Determination Program')
     RETURN
777  CALL ErrorMessage('ExtinctionSymbol.exe not found in installation directory')
     RETURN
     END SUBROUTINE SpaceGroupDeterminationCode

!*********************************************************************************
     SUBROUTINE SpaceGroupFileDialog 
     
     USE VARIABLES
     USE WINTERACTER
     
     IMPLICIT NONE

     CHARACTER(MaxPathLength) :: CurrentDirectory
     LOGICAL, EXTERNAL :: Confirm
     LOGICAL Exists

     IF (Confirm('Would you like to remove files generated'//CHAR(13)// &
          'during space group determination?')) THEN
       CALL IosDirName(CurrentDirectory)
       CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'advanced.asc')
       CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'table.asc')
       CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'parameter_input.asc')
       INQUIRE(FILE=TRIM(CurrentDirectory)//DIRSPACER//'peak_ignored.asc',EXIST=exists)
       IF (exists) THEN
         CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'peak_ignored.asc')
       ENDIF
       INQUIRE(FILE=TRIM(CurrentDirectory)//DIRSPACER//'uncorrelated_peaks.asc',EXIST=exists)
       IF (exists) THEN
         CALL IosDeleteFile(TRIM(CurrentDirectory)//DIRSPACER//'uncorrelated_peaks.asc')
       ENDIF
     ELSE
       RETURN
     ENDIF

     END SUBROUTINE SpaceGroupFileDialog
