!
!*****************************************************************************
!
      SUBROUTINE LaunchTutorial(Tutorial_ID)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Tutorial_ID

      CHARACTER(MaxPathLength) DirString, FileDir
      CHARACTER(MaxPathLength) Files(100)
      CHARACTER(MaxPathLength) CurDir
      INTEGER                  NFiles, I
      CHARACTER(1)             NumberStr

      NFiles = 100
      CALL IOsDirName(CurDir)
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
      END SELECT
      DirString = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"Documentation"//DIRSPACER//"Tutorial"//NumberStr
      FileDir = DirString(1:LEN_TRIM(DirString))//DIRSPACER//"Data files"
      CALL IOsDirList(FileDir(1:LEN_TRIM(FileDir)),"",Files,Nfiles)
      CALL IOsDirChange(FileDir(1:LEN_TRIM(FileDir)))
      DO I = 1, Nfiles
        CALL IOsCopyFile(Files(I)(1:LEN_TRIM(Files(I))),CURDIR(1:LEN_TRIM(CURDIR))//DIRSPACER)
      ENDDO
      CALL IOsDirChange(CurDir(1:LEN_TRIM(CurDir)))
      CALL WHelpFile(' ') ! In case the help file is open already
      CALL WHelpFile(DirString(1:LEN_TRIM(DirString))//DIRSPACER//"Tutorial"//NumberStr//".chm")

      END SUBROUTINE LaunchTutorial
!
!*****************************************************************************
!
