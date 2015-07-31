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

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_BOOL, C_LOC
      USE CCDC_LICENSE_BINDINGS
      
      IMPLICIT NONE

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch
      
      INTEGER (C_INT) :: USE_GUI
    
      IF ( in_batch ) THEN

        USE_GUI = 0   
        IF ( CCDC_IS_LICENSED(USE_GUI) .EQ. 1 ) THEN
          ! Have a CCDC licence so all is sweet
          RETURN
        ENDIF
           
        CALL AppendBatchLogFile('Error: Can not find a valid CSD licence')
        CALL DoExit
      ENDIF

      USE_GUI = 0   
      IF ( CCDC_IS_LICENSED(USE_GUI) .EQ. 1 ) THEN
          ! Have a CCDC licence so all is sweet
          RETURN
      ENDIF

      USE_GUI = 1
      DO WHILE (1) 
        !CALL ErrorMessage("Unable to find a suitable licence to the CSD System to allow you to use DASH."
        !                   "Please enter the required information")
        IF ( CCDC_IS_LICENSED(USE_GUI) .EQ. 1 ) THEN
            ! Have a CCDC Academic licence so all is sweet 
            RETURN 
        ELSE
            CALL DoExit
        ENDIF        
      ENDDO

#endif
      END SUBROUTINE CheckLicence
