
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
      INTEGER MaxSPGR
      PARAMETER (MaxSPGR=530)
! MaxSPGR is the maximum number of space groups including all different settings.
! These are loaded from a file at the beginning.

! Following lines : local variables in include
      INTEGER :: NumBrSg, ISPosSG, ISG, JSG
      CHARACTER(LEN=24) :: SGHMaBrStr(MaxSPGR)

      INTEGER           :: IPosSG, LPosSG
      CHARACTER(LEN=12) :: SGNumStr
      CHARACTER(LEN=12) :: SGHMaStr
!U      CHARACTER(LEN=18) :: SGHalStr
      CHARACTER(LEN=24) :: SGShmStr
! These are broken into three to make this include fortran and f90 compatible
      COMMON /SGStrCmn1/ IPosSG, LPosSG(12), SGNumStr(MaxSPGR)
!U      COMMON /SGStrCmn2/ SGHMaStr(MaxSPGR), SGHalStr(MaxSPGR)
!U      COMMON /SGStrCmn3/ SGShmStr(MaxSPGR)
      COMMON /SGStrCmn2/ SGHMaStr(MaxSPGR), SGShmStr(MaxSPGR)
