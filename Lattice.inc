! Definitions for the lattice widgets

!U      INTEGER :: NCS_Options
!U      parameter (NCS_Options=11)
!U      character*40 CS_Options(NCS_Options)
!U      Data CS_Options(1)/'Not known'/
!U      Data CS_Options(2)/'Triclinic'/
!U      Data CS_Options(3)/'Monoclinic (a-axis)'/
!U      Data CS_Options(4)/'Monoclinic (b-axis)'/
!U      Data CS_Options(5)/'Monoclinic (c-axis)'/
!U      Data CS_Options(6)/'Orthorhombic'/
!U      Data CS_Options(7)/'Tetragonal'/
!U      Data CS_Options(8)/'Trigonal'/
!U      Data CS_Options(9)/'Rhombohedral'/
!U      Data CS_Options(10)/'Hexagonal'/
!U      Data CS_Options(11)/'Cubic'/

!C>> JCC The next common allows low-level setting of SLIM (not accessible through the front
!C     end, but may be in future. I've allowed reading via the dsl file)
      REAL SLIMVALUE, SCALFAC
      LOGICAL BACKREF
      COMMON /PWLYST/ SLIMVALUE,SCALFAC,BACKREF

! ALambda is now in GLBVAR.INC

      REAL :: CELLPAR, ZEROPOINT 
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT

      INTEGER MVAL
      PARAMETER (MVAL=50)
      INTEGER LatBrav
      INTEGER IHLR
      COMMON /LATREFCMN/ LatBrav,IHLR(3,MVAL) 
!
      INTEGER :: IOption
      INTEGER :: NumSG,IPosSG,LPosSG,NumBrSg,ISPosSG,ISG,JSG
      CHARACTER(LEN=24) :: SGHMaBrStr(530)
      CHARACTER(LEN=12) :: SGNumStr
      CHARACTER(LEN=12) :: SGHMaStr
      CHARACTER(LEN=18) :: SGHalStr
      CHARACTER(LEN=24) :: SGShmStr
! These are broken into two to make this include fortran and f90 compatible
      COMMON /SGStrCmn1/ NumSG,IPosSG,LPosSG(12),SGNumStr(530)
      COMMON /SGStrCmn2/ SGHMaStr(530),SGHalStr(530),SGShmStr(530)
