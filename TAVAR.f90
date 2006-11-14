!
!*****************************************************************************
!
! This module contains all TOPAS related data
!
      MODULE TAVAR

      IMPLICIT NONE

      LOGICAL For_TOPAS

	  CHARACTER(255) :: TOPASEXE

      CHARACTER(255) TOPAS_input_file_name

      INTEGER TOPAS_stage

      LOGICAL use_anisotropic_broadening

      CHARACTER(255) old_diffraction_data_file_name

      INTEGER old_NumOfRef
  
      END MODULE TAVAR
!
!*****************************************************************************
!
