!
!*****************************************************************************
!
#ifdef __G95__
#define C_STR_CONSTANT(s)  s//CHAR(0)
#else
#define C_STR_CONSTANT(s)  s ## C
#endif
!
!*****************************************************************************
!
      SUBROUTINE CheckLicence
#ifdef __G95__
#else

      USE CCDC_LICENSE_BINDINGS
      
      IMPLICIT NONE
      
      CALL IS_LICENSED_OR_EXIT
  
#endif
      END SUBROUTINE CheckLicence

    
      SUBROUTINE InitializeLicensing
#ifdef __G95__
#else
      USE CCDC_LICENSE_BINDINGS      
      IMPLICIT NONE
      
      CALL INITIALIZE_LICENSING 
#endif
      END SUBROUTINE InitializeLicensing

      SUBROUTINE FinalizeLicensing
#ifdef __G95__
#else
      USE CCDC_LICENSE_BINDINGS      
      IMPLICIT NONE
      
      CALL FINALIZE_LICENSING 
#endif
      END SUBROUTINE FinalizeLicensing