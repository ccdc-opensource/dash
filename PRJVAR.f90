!
!*****************************************************************************
!
! This module contains all project file related data
!
      MODULE PRJVAR

      IMPLICIT NONE

      INTEGER hPrjFile            ! Handle to the project file (the FORTRAN UNIT)
      INTEGER iPrjRecNr
      INTEGER iPrjReadOrWrite     ! Variable indicating whether we are reading or writing
  
      INTEGER     cWrite
      PARAMETER ( cWrite = 0)

      INTEGER     cRead
      PARAMETER ( cRead = 1)

      CHARACTER*255 PrjFileName   ! The name of the project file

      END MODULE PRJVAR
!
!*****************************************************************************
!
