!
!*****************************************************************************
!
      SUBROUTINE LaunchTutorial(Tutorial_ID)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      use kernel32
      use dfwinty
 
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Tutorial_ID

      CHARACTER(MaxPathLength) ManualPath, DocumentationPath, FileDir
      CHARACTER(MaxPathLength) DestineDir

      CHARACTER(MaxPathLength) TutorialFileName
      CHARACTER(1)             NumberStr

      INTEGER d

      DestineDir = TRIM(AppDataDirectory)//'DASH_files'
      IF (.NOT. IOsDirExists(DestineDir)) CALL IOsDirMake(DestineDir)
      CALL WSelectDir(DirCreate, DestineDir, 'Directory to save tutorial files')
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK .OR. LEN_TRIM(DestineDir) .LE. 0) RETURN

      SELECT CASE (Tutorial_ID)
        CASE (ID_Tutorial_1)
          NumberStr = '1'
        CASE (ID_Tutorial_2)
          NumberStr = '2'
        CASE (ID_Tutorial_3)
          NumberStr = '3'
        CASE (ID_Tutorial_4)
          NumberStr = '4'
        CASE (ID_Tutorial_5)
          NumberStr = '5'
        CASE (ID_Tutorial_6)
          NumberStr = '6'
      END SELECT

      TutorialFileName = "tutorial-"//TRIM(NumberStr)//".html"

      DocumentationPath = TRIM(InstallationDirectory)//"Documentation"
      ManualPath        = TRIM(DocumentationPath)//DIRSPACER//"manual"//DIRSPACER//"portable_html"

      FileDir = TRIM(DocumentationPath)//DIRSPACER//"Tutorial"//NumberStr//DIRSPACER//"Data files"
      CALL IOsDirChange(TRIM(FileDir))
      CALL IOsCopyFile('Tutorial_'//NumberStr//'.xye',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('Tutorial_'//NumberStr//'.raw',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('Tutorial_'//NumberStr//'*.mol2',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('Tutorial_'//NumberStr//'*.zmatrix',TRIM(DestineDir)//DIRSPACER)

      CALL IOsDirChange(TRIM(ManualPath))

      CALL WHelpFile(' ') ! In case the help file is open already
      CALL WHelpFile(TutorialFileName)

      d=WinExec('cmd /c "'//TRIM(TutorialFileName)//'" 'C,SW_HIDE)


      CALL IOsDirChange(TRIM(DestineDir))


      END SUBROUTINE LaunchTutorial
!
!*****************************************************************************
!
