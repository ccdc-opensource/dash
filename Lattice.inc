! Definitaions for the lattice widgets

      INTEGER :: NCS_Options
      parameter (NCS_Options=11)
      character*40 CS_Options(NCS_Options)
      Data CS_Options(1)/'Not known'/
	  Data CS_Options(2)/'Triclinic'/
	  Data CS_Options(3)/'Monoclinic (a-axis)'/
	  Data CS_Options(4)/'Monoclinic (b-axis)'/
      Data CS_Options(5)/'Monoclinic (c-axis)'/
	  Data CS_Options(6)/'Orthorhombic'/
	  Data CS_Options(7)/'Tetragonal'/
	  Data CS_Options(8)/'Trigonal'/
      Data CS_Options(9)/'Rhombohedral'/
	  Data CS_Options(10)/'Hexagonal'/
	  Data CS_Options(11)/'Cubic'/
!C>> JCC Add some radiation type strings

	  integer,parameter:: NRad_Types = 3
	  REAL :: RadWaveLengths(NRad_Types)
	  Data RadWaveLengths(1)/1.5405/
	  Data RadWaveLengths(2)/1.7889/
	  Data RadWaveLengths(3)/0.7093/


!C>> JCC The next common allows low-level setting of SLIM (not accessible through the front
!C     end, but may be in future. I've allowed reading via the dsl file)
	  REAL SLIMVALUE, SCALFAC
	  LOGICAL BACKREF
	  COMMON /PWLYST/ SLIMVALUE,SCALFAC,BACKREF

      INTEGER :: LatBrav
      REAL :: CELLPAR,ZEROPOINT,ALAMBDA 
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
	  COMMON /LATREFCMN/ LatBrav 
!
	  INTEGER :: IOption
      INTEGER :: NumSG,IPosSG,LPosSG,NumBrSg,ISPosSG,ISG,JSG
      CHARACTER(LEN=24) :: SGHMaBrStr(530)
      CHARACTER(LEN=12) :: SGNumStr
      CHARACTER(LEN=12) :: SGHMaStr
      CHARACTER(LEN=18) :: SGHalStr
      CHARACTER(LEN=24) :: SGShmStr
! These are broken into two to make this include fortran and f90 compatible
      common /SGStrCmn1/ NumSG,IPosSG,LPosSG(12),SGNumStr(530)
      common /SGStrCmn2/ SGHMaStr(530),SGHalStr(530),SGShmStr(530)
