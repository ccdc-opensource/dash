!
!*****************************************************************************
!
! JvdS The following global variables seem to be needed:
!
! - start of profile (to indicate range)
! - end  of profile
! - filename (this is now done partially by 'FNAME', which is also used as a dummy)

! NoData is regularly set to .FALSE. but is it ever set to .TRUE.?

      MODULE VARIABLES

      USE WINTERACTER
!
!   Shared variables for any routine with 'USE VARIABLES'
!
      IMPLICIT NONE
!
      LOGICAL                       :: SAVEF = .FALSE. ! File needs saving

      INTEGER, PARAMETER :: MaxPathLength =  255
      LOGICAL                       :: PLOTT = .FALSE. ! Graphic plotted?
      CHARACTER(LEN=MaxPathLength)  :: FNAME = ' '     ! Current filename
! Added these in for portability reasons       
      CHARACTER(LEN=MaxPathLength)  :: INSTDIR =     'C:\Program Files\DASH' ! Default installation directory
      CHARACTER(LEN=21)             :: SPACEGROUPS = 'SpaceGroupSymbols.dat' ! Table name
      CHARACTER                     :: DIRSPACER   = '\' ! Windows spacer
      CHARACTER(LEN=8)              :: CONFIG      = 'Dash.cfg'
! External binaries
      CHARACTER(LEN=MaxPathLength)  :: VIEWEXE     = 'C:\Program Files\DASH\mercury.exe'
      CHARACTER(LEN=MaxPathLength)  :: CONVEXE     = 'C:\Program Files\DASH\zmconv.exe'
      CHARACTER(LEN=20)             :: VIEWARG     = ''

      LOGICAL ViewOn
      LOGICAL ConvOn       ! Set if external z-matix conversion program available
      LOGICAL ViewAct
      LOGICAL AutoUpdate

! File information; Names of files used by DASH For I/O
      CHARACTER*80 :: DashTicFile
      CHARACTER*80 :: DashHcvFile
      CHARACTER*80 :: DashPikFile
      CHARACTER*80 :: DashRawFile
      CHARACTER*80 :: DashDslFile

      LOGICAL TicExists
      LOGICAL HcvExists
      LOGICAL PikExists
      LOGICAL RawExists
      LOGICAL DslExists

! New license information structure    
      TYPE License_Info
        INTEGER  :: Day
        INTEGER  :: Month
        INTEGER  :: Year
        INTEGER  :: DateCode
        INTEGER  :: SerialNumber
        INTEGER  :: LicenseType
        INTEGER  :: Valid
      END TYPE
      INTEGER NodeKey,DemoKey,SiteKey
      PARAMETER (NodeKey = 1)
      PARAMETER (DemoKey = 2)
      PARAMETER (SiteKey = 3)

      INTEGER           :: EventType
      TYPE(WIN_MESSAGE) :: EventInfo
! These global variables hold the last event reported by Winteracter

      LOGICAL NoData
! .TRUE. when a powder diffraction pattern has been read in

      INTEGER OriginalNOBS ! Original number of data points read in for the raw powder pattern
      INTEGER EndNOBS
! When truncating the powder pattern at the start, DASH stores the data points that were removed
! _after_ the original pattern. EndNOBS points to the original end of the pattern.
! OriginalNOBS is never changed, so the point between EndNOBS and Original NOBS are
! data points that were reomved from the start of the pattern.

      LOGICAL UseConfigFile
! This is the first item read from the configuration file (if present).
! If set to .FALSE., the rest of the configuration file will be skipped.
      
      LOGICAL SavePDB, SaveCSSR, SaveCCL, SaveRES
! Flags to decide which molecular model files are written out when a best solution is found

      LOGICAL AutoLocalMinimisation
! When set, each run in a multi run ends with a local minimisation

      END MODULE VARIABLES
!
!*****************************************************************************
!
