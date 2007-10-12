!
!*****************************************************************************
!
! This module contains all external RR related data
!
      MODULE TAVAR

      IMPLICIT NONE

      INTEGER iRietveldMethod
      INTEGER, PARAMETER :: INTERNAL_RB=0, FOR_TOPAS=1, FOR_GSAS=2, FOR_RIETAN=3

	  CHARACTER(255) :: TOPASEXE, EXPGUIEXE, RIETANEXE

      CHARACTER(255) ext_RR_input_file_name

      INTEGER ext_RR_stage

      LOGICAL use_anisotropic_broadening, Rietan_FP

      CHARACTER(255) old_diffraction_data_file_name

      INTEGER old_NumOfRef, old_SubtractBkg
  
      END MODULE TAVAR
!
!*****************************************************************************
!
