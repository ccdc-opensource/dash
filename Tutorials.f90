!
!*****************************************************************************
!
      SUBROUTINE LaunchTutorial(Tutorial_ID)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Tutorial_ID

      CHARACTER*255 DirString, FileDir
      CHARACTER*255 Files(100)
      CHARACTER*255 CurDir
      INTEGER       NFiles
      INTEGER       I

      NFiles = 100
      CALL IOsDirName(CurDir)
      SELECT CASE (Tutorial_ID)
        CASE (ID_Tutorial_1)
          DirString = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"Documentation"//DIRSPACER//"tutorial1"
        CASE (ID_Tutorial_2)
          DirString = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"Documentation"//DIRSPACER//"tutorial2"
        CASE (ID_Tutorial_3)
          DirString = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"Documentation"//DIRSPACER//"tutorial3"
        CASE (ID_Tutorial_4)
          DirString = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"Documentation"//DIRSPACER//"tutorial4"
        CASE (ID_Tutorial_5)
          DirString = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"Documentation"//DIRSPACER//"tutorial5"
      END SELECT
      FileDir = DirString(1:LEN_TRIM(DirString))//DIRSPACER//"data files"
      CALL IOsDirList(FileDir(1:LEN_TRIM(FileDir)),"",Files,Nfiles)
      CALL IOsDirChange(FileDir(1:LEN_TRIM(FileDir)))
      DO I = 1, Nfiles
        CALL IOsCopyFile(Files(I)(1:LEN_TRIM(Files(I))),CURDIR(1:LEN_TRIM(CURDIR))//DIRSPACER)
      ENDDO
      CALL IOsDirChange(CurDir(1:LEN_TRIM(CurDir)))
      CALL WHelpFile(' ') ! In case the help file is open already
      CALL WHelpFile(DirString(1:LEN_TRIM(DirString))//DIRSPACER//"index.htm")

      END SUBROUTINE LaunchTutorial
!
!*****************************************************************************
!
