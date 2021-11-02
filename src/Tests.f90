    SUBROUTINE TestSuite
    
! This is a poor mans set of test suite which we could later split out into a proper suite,

    INTEGER*4 LUIN
    LUIN = 42
! Deliberate crash on error note so we know something is wrong
    OPEN(UNIT=LUIN,FILE='TestSuite.log')
    CALL TestZMConvWorkflow(LUIN)

    END SUBROUTINE TestSuite
    
    
    
!-------------------------------------------------------------------------------------------------------------------------------------
! Utility Functions 
    
    SUBROUTINE GetTestFile(BaseFileName,OutFileName)
    
    USE VARIABLES 
    
    CHARACTER*(*), INTENT (IN   ) :: BaseFileName
    CHARACTER*(*), INTENT (OUT   ) :: OutFileName    
    OutFileName = ''
!------------------------------------------------------------------
! TODO - remove need for hard wired path: get build directory from the environment somehow
!
    WRITE(OutFileName ,'(A)') 'E:'//DIRSPACER//'builds'//DIRSPACER//'dash'//DIRSPACER//'tests'//DIRSPACER//'testdata'//DIRSPACER//BaseFileName(1:LEN(BaseFileName))
    END SUBROUTINE GetTestFile
    


!-----------------------------------------------------------------------------------------------------------------------------------
! Test Functions

    SUBROUTINE TestZMConvWorkflow(LUIN)
      
    USE DFWIN

    USE VARIABLES
    
    LOGICAL, EXTERNAL :: RunZmConv
    CHARACTER*(1024) OutFileName
    
    WRITE(LUIN,'(A)')'Start Tests for ZMConv Workflow'

! Lets first check to see if the CSD Python API is installed

    CALL GetPathToCSDPythonAPIFromRegistry
    CALL GetTestFile('AABHTZ.mol2',OutFileName )
    WRITE(LUIN,'(A)')'Test input file:', OutFileName(1:LEN_TRIM(OutFileName))
     
! If the CSD Python API is installed, we should expect this to be set to something other than NOTFOUND 
    IF ( PYAPIEXE .EQ. 'NOTFOUND') THEN
        WRITE(LUIN,'(A)') 'WARNING: Python API is not installed - further tests not run'
        RETURN
    ELSE IF (INDEX(PYAPIEXE,'miniconda') .NE. 0 ) THEN
        WRITE(LUIN,'(A,A)') 'SUCCESS: Python API is Installed - ', PYAPIEXE(1:LEN_TRIM(PYAPIEXE))
    ELSE
        WRITE(LUIN,'(A,A)') 'FAIL: Invalid Return Text from Registry Query - further tests not run', PYAPIEXE(1:LEN_TRIM(PYAPIEXE))
        RETURN
    ENDIF

! The CSD Python API is installed, so we can run the workflow and test the outputs
    IF ( .NOT. RunZmConv( OutFileName,.FALSE.) ) THEN
        WRITE(LUIN,'(A)') 'FAIL: ZMConv didnt work'
    ELSE
        WRITE(LUIN,'(A)') 'SUCCESS: ZMConv ran ...'
    ENDIF
    
    END SUBROUTINE TestZMConvWorkflow
    
    
    
    