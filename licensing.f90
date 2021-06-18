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

      COMMON /BATEXE/ in_batch
      INTEGER, DIMENSION(2) :: ID
      
      M = WInfoError(3) ! Clear errors
      CALL IOSCommand( TRIM(InstallationDirectory)//DIRSPACER//'zmconv'//DIRSPACER//'dash_csd_connector'//CCDC_EXE_EXT , ProcSilent, IDPROC=ID)      
      DO
          CALL IOsCommandCheck(ID, ISTATUS, IEXCOD)
          IF (ISTATUS==0) EXIT
          CALL IOsWait(5)
      END DO

      IF (IEXCOD .EQ. 0) THEN
          ! Have a CCDC licence so all is sweet
          RETURN
      ENDIF
       
      IF ( in_batch ) THEN
        CALL AppendBatchLogFile('Error: Can not find a valid DASH licence')
      ELSE
        ! This is only one of the many reasons DASH might not be licensed.
        CALL ErrorMessage("FATAL: Your licence does not include use of the 'dash' feature."//CHAR(13)// &
                          "If you have a question about your licence details, please contact admin@ccdc.cam.ac.uk")
      ENDIF

      CALL DoExit
#endif
      END SUBROUTINE CheckLicence
