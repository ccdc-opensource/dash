!
!*****************************************************************************
!
! JvdS The following global variables seem to be needed:
!
! - filename (this is now done partially by 'FNAME', which is also used as a dummy)

! NoData is regularly set to .FALSE. but is it ever set to .TRUE.?
!
! The declarations have now been rewritten in a format SPAG can understand
!
      MODULE VARIABLES

      USE WINTERACTER
!
!   Shared variables for any routine with 'USE VARIABLES'
!
      IMPLICIT NONE

      INTEGER MaxPathLength
      PARAMETER (MaxPathLength = 255)

      CHARACTER(MaxPathLength) ::  FNAME
      DATA FNAME / ' ' /     ! Current filename

      CHARACTER(MaxPathLength) ::  InstallationDirectory
      CHARACTER(MaxPathLength) ::  DocumentationHTMLdirectory
      CHARACTER(MaxPathLength) ::  DocumentationRoot
      CHARACTER(MaxPathLength) ::  StartUpDirectory
      CHARACTER(MaxPathLength) ::  AllUsersProfileDirectory
      CHARACTER(MaxPathLength) ::  AppDataDirectory
      CHARACTER(MaxPathLength) ::  PathToLicenseFile

#ifdef _WIN32
      CHARACTER(1),  PARAMETER :: DIRSPACER= '\' ! Windows spacer
      CHARACTER(4),  PARAMETER :: CCDC_EXE_EXT ='.exe'
      CHARACTER(20), PARAMETER :: ALL_FILES_FILTER ='All files (*.*)|*.*|'
#else
      CHARACTER(1),  PARAMETER :: DIRSPACER = '/' ! Unix spacer
      CHARACTER(2),  PARAMETER :: CCDC_EXE_EXT ='.x'
      CHARACTER(16), PARAMETER :: ALL_FILES_FILTER ='All files (*)|*|'
#endif

! FORTRAN standard 1997:
! For direct access, for unformatted input/output, the RECL is measured in 
! processor-dependent units.
#ifdef __G95__
! G95 (version 0.92): RECL is expressed in bytes
      INTEGER, PARAMETER :: cRECLMult = 4
#else
! Intel/DEC: RECL is expressed in 4-byte units (longwords)
      INTEGER, PARAMETER :: cRECLMult = 1
#endif

      CHARACTER(MaxPathLength) :: VIEWEXE
      CHARACTER(20)            :: VIEWARG
      CHARACTER(MaxPathLength) :: MOGULEXE
      CHARACTER(MaxPathLength) :: DICVOLEXE
      INTEGER                  :: DICVOL_ver
      INTEGER, PARAMETER       :: DICVOL_internal = 0, DICVOL04 = 4, DICVOL06 = 6
      CHARACTER(MaxPathLength) :: McMailleEXE
      INTEGER                  :: iMcMailleNgridOpt

      LOGICAL UseMogul
      DATA UseMogul / .TRUE. /
! Set to false when Mogul not used for Modal Torsion Angle Ranges.
! Stops the user constantly being reminded that there isn't a path
! to a Mogul exe specified

      CHARACTER(10) :: ProgramVersion
      DATA ProgramVersion / 'DASH 3.3.5' /

! File information; Names of files used by DASH For I/O
      CHARACTER(MaxPathLength)  DashTicFile
      CHARACTER(MaxPathLength)  DashHcvFile
      CHARACTER(MaxPathLength)  DashPikFile
      CHARACTER(MaxPathLength)  DashRawFile
      CHARACTER(MaxPathLength)  DashDslFile

! New license information structure    
      TYPE License_Info
        CHARACTER(80) KeyStr
        INTEGER   ExpiryDate
        INTEGER   DaysLeft
        INTEGER   SerialNumber
        INTEGER   LicenceType
        INTEGER   Valid
      END TYPE
      INTEGER NodeKey, DemoKey, SiteKey
      PARAMETER (NodeKey = 1)
      PARAMETER (DemoKey = 2)
      PARAMETER (SiteKey = 3)

      INTEGER            EventType
      TYPE(WIN_MESSAGE)  EventInfo
! These global variables hold the last event reported by Winteracter

      LOGICAL NoData
      DATA NoData / .TRUE. /
! .FALSE. when a powder diffraction pattern has been read in

      LOGICAL UseConfigFile
! This is the first item read from the configuration file (if present).
! If set to .FALSE., the rest of the configuration file will be skipped.
      
      REAL SA_SimplexDampingFactor
! Damping factor for the local minimisation during / after a simulated annealing run

      REAL        DASHDefaultMaxResolution
      PARAMETER ( DASHDefaultMaxResolution = 1.75 )
      REAL DefaultMaxResolution
! The maximum resolution cut-off, in Angstrom, for this powder pattern.
! This is equal to DASHDefaultMaxResolution for new patterns, and equal to whatever was used
! for patterns from a .sdi file. 

      REAL SXMaxResolution 
! The maximum resolution cut-off, in Angstrom, for Single Crystal data.
! The reason this must be held in a variable is that we have too many dialogue windows to keep in memory,
! and the dialogue window displaying this variable must therefore be swapped in and out of memory.

      INTEGER iRietveldMethodOpt
! The chosen Rietveld refinement method, "Rigid-body", "TOPAS"......
! The reason this must be held in a variable is that we have too many dialogue windows to keep in memory,
! and the dialogue window displaying this variable must therefore be swapped in and out of memory.

      LOGICAL lRebin
      INTEGER iBinWidth
! Whether or not to re-bin and with what bin-width
! The reason these must be held in variables is that we have too many dialogue windows to keep in memory,
! and the dialogue window displaying these variables must therefore be swapped in and out of memory.

      INTEGER RR_SA_Sol
! The SA solution that will be used for Rietveld refinement. Crystal structures read in from an external
! file are stored in RR_SA_Sol = 1

      LOGICAL PastPawley
! The code used to calculate the tickmarks does so by emulating a Rietveld refinement.
! As a Rietveld Refinement needs scatterers, a single carbon atom is provided in the 
! CCSL input file. This is read by the CCSL code and sets the number of atoms to 1, overriding
! all Z-matrices.
! Therefore, calculating the tickmarks destroys the Z-matrices.
! Due to the way Winteracter and DASH work, the tickmarks are re-calculated every time
! a user switches between tabs in the 'View' dialogue.
! The variable 'PastPawley' is there to indicate that the Pawley refinement has finished and that
! no changes to any of the variables involved in calculating the tickmarks
! (wavelength, zeropoint and unit cell parameters) is expected any more.
! This variable is then tested before calculating the tickmarks.

      LOGICAL NoWavelengthInXYE
! Set to .TRUE. whenever an .xye file is loaded that doesn't contain the wavelength
! When the wavelength is set, this variable is tested and the wavelength is
! written to the file.

      END MODULE VARIABLES
!
!*****************************************************************************
!
