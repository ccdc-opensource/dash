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

      CHARACTER                ::  DIRSPACER
      DATA DIRSPACER / '\' / ! Windows spacer

      CHARACTER(MaxPathLength) :: VIEWEXE
      CHARACTER(20)            :: VIEWARG

      CHARACTER(8) :: ProgramVersion
      DATA ProgramVersion / 'DASH 4.0' /

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

      REAL DefaultMaxResolution
! The maximum resolution cut-off, in Angstrom, set as default when a new powder pattern is read in.

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
