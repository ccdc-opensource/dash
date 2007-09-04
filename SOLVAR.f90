!
!*****************************************************************************
!
! This module contains all data related to solutions from the SA
!
      MODULE SOLVAR

      IMPLICIT NONE

      INTEGER     MaxAtm_5
      PARAMETER ( MaxAtm_5 = 300 )

      INTEGER     MaxRun2
      PARAMETER ( MaxRun2 = 999 )

      INTEGER iSol2Run(1:MaxRun2)

      INTEGER iSolTicked(1:MaxRun2)

      REAL ProfileChiSqd(1:MaxRun2) ! MaxRun

      REAL IntensityChiSqd(1:MaxRun2) ! MaxRun

      REAL BestValuesDoF(1:100,1:MaxRun2)  ! mvar, MaxRun

      REAL XAtmCoords(1:3,1:MaxAtm_5,0:MaxRun2) ! MaxAtm, MaxRun 
! Co-ordinates of the atoms of the asymmetric unit of the best SA solution so far per run.
! XAtmCoords(1:3,1:150,0) is for Rietveld refinement (needed by SA_STRUCTURE_OUTPUT_PDB()
! needed by ViewStructure())

      END MODULE SOLVAR
!
!*****************************************************************************
!
