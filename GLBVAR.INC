! This file contains global variables.
! Yet another attempt to gather them.

      INTEGER JRadOption
      COMMON /RadOption/ JRadOption

! JRadOption stores the type of radiation as entered by the user:
! 1 = X-ray lab data
! 2 = X-ray synchrotron data
! 3 = Constant Wavelength Neutrons
! 4 = Time-of-Flight Neutrons

      REAL ALambda
      COMMON /ALABDA/ ALambda

! ALambda stores the wavelength in Angstrom
! Note that its value can be set when the anode material is known through the use of a
! function, e.g.: ALambda = WavelengthOf('Cu')

	  CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)

! STATBARSTR stores the status bar shown at the bottom of the main window in DASH.
!
!  STATBARSTR ( 1) = Filename
!  STATBARSTR ( 2) =
!  STATBARSTR ( 3) =
!  STATBARSTR ( 4) =
!  STATBARSTR ( 5) =
!  STATBARSTR ( 6) =
!  STATBARSTR ( 7) =
!  STATBARSTR ( 8) =
!  STATBARSTR ( 9) =
!  STATBARSTR (10) =

      INTEGER IPTYPE
      COMMON /PLTYPE/ IPTYPE

! IPTYPE indicates which profiles should be plotted:
!
!  1 = Observed              profile
!  2 = Observed + Calculated profile
!
! -1 = Observed              profile to printer (hardcopy)
! -2 = Observed + Calculated profile to printer (hardcopy)

      INTEGER IDCurrent_Cursor_Mode
      COMMON /CURSOR_MODE/ IDCurrent_Cursor_Mode

! IDCurrent_Cursor_Mode stores ?

      LOGICAL DoSaRedraw
      COMMON /SARDRW/ DoSaRedraw

! Indicates whether the Simulated Annealing window should be redrawn

      REAL CHAR_SIZE,MARKER_SIZE
      COMMON /PROFDEF/CHAR_SIZE,MARKER_SIZE

! Sizes of the characters and markers used in drawin the profiles in the main window

