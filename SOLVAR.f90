!
!*****************************************************************************
!
! This module contains all data related to solutions from the SA
!
      MODULE SOLVAR

      IMPLICIT NONE

      INTEGER iSol2Run(1:99)

      INTEGER iSolTicked(1:99)

      REAL ProfileChiSqd(1:99) ! MaxRun

      REAL IntensityChiSqd(1:99) ! MaxRun

      REAL BestValuesDoF(1:100,1:99)  ! mvar, MaxRun

      REAL XAtmCoords(1:3,1:150,1:99) ! MaxAtm, MaxRun 
! Co-ordinates of the atoms of the asymmetric unit of the best SA solution so far per run.

      END MODULE SOLVAR
!
!*****************************************************************************
!
