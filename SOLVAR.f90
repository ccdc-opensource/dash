!
!*****************************************************************************
!
! This module contains all data related to solutions from the SA
!
      MODULE SOLVAR

      IMPLICIT NONE

      REAL ProfileChiSqd(1:99) ! MaxRun

      REAL IntensityChiSqd(1:99) ! MaxRun

      REAL BestValuesDoF(1:100,1:99)  ! mvar, MaxRun

      REAL XAtmCoords(1:3,1:150,1:99) ! MaxAtm, MaxRun 
! Co-ordinates of the atoms of the asymmetric unit of the best SA solution so far per run.

      INTEGER iSolOrder(1:99)

      END MODULE SOLVAR
!
!*****************************************************************************
!
