! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      SUBROUTINE LaunchTutorial(Tutorial_ID)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
#ifdef _WIN32
      USE KERNEL32
      USE dfwinty
#endif
 
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

      FileDir = TRIM(DocumentationRoot)//DIRSPACER//"Tutorial"//NumberStr//DIRSPACER//"Data files"
      CALL IOsDirChange(TRIM(FileDir))
      CALL IOsCopyFile('Tutorial_'//NumberStr//'.xye',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('Tutorial_'//NumberStr//'.raw',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('Tutorial_'//NumberStr//'*.mol2',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('Tutorial_'//NumberStr//'*.zmatrix',TRIM(DestineDir)//DIRSPACER)

      CALL IOsDirChange(TRIM(DocumentationHTMLdirectory))

      CALL WHelpFile(' ') ! In case the help file is open already
      CALL WHelpFile(TutorialFileName)

#ifdef _WIN32
      d=WinExec('cmd /c "'//TRIM(TutorialFileName)//'" 'C,SW_HIDE)
#else
#endif

      CALL IOsDirChange(TRIM(DestineDir))


      END SUBROUTINE LaunchTutorial
!
!*****************************************************************************
!
