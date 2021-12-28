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
      USE dash_gui_resources
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Tutorial_ID

      CHARACTER(MaxPathLength) DocumentationPath
      CHARACTER(MaxPathLength) DestineDir
      CHARACTER(2)             NumberStr

      INTEGER d

      DestineDir = TRIM(AppDataDirectory)//DIRSPACER//'DASH_files'
      IF (.NOT. IOsDirExists(DestineDir)) CALL IOsDirMake(DestineDir)
      CALL WSelectDir(DirCreate, DestineDir, 'Directory to save tutorial files')
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK .OR. LEN_TRIM(DestineDir) .LE. 0) RETURN

      SELECT CASE (Tutorial_ID)
        CASE (ID_Tutorial_1)
          NumberStr = '01'
        CASE (ID_Tutorial_2)
          NumberStr = '02'
        CASE (ID_Tutorial_3)
          NumberStr = '03'
        CASE (ID_Tutorial_4)
          NumberStr = '04'
        CASE (ID_Tutorial_5)
          NumberStr = '05'
        CASE (ID_Tutorial_6)
          NumberStr = '06'
      END SELECT

      CALL IOsDirChange(TRIM(DocumentationRoot)//DIRSPACER//"tutorials"//DIRSPACER//"tutorial-"//NumberStr//DIRSPACER//"data")
      CALL IOsCopyFile('*.xye',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('*.raw',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('*.mol2',TRIM(DestineDir)//DIRSPACER)
      CALL IOsCopyFile('*.zmatrix',TRIM(DestineDir)//DIRSPACER)

      CALL IOsOpenDocument("https://github.com/ccdc-opensource/dash/wiki/Tutorial"//TRIM(NumberStr))

      CALL IOsDirChange(TRIM(DestineDir))
      CALL IOsOpenDocument(TRIM(DestineDir))


      END SUBROUTINE LaunchTutorial
!
!*****************************************************************************
!
