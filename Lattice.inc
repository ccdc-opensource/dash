
!C>> JCC The next common allows low-level setting of SLIM (not accessible through the front
!C     end, but may be in future. I've allowed reading via the dsl file)
      REAL SLIMVALUE, SCALFAC
      LOGICAL BACKREF
      COMMON /PWLYST/ SLIMVALUE,SCALFAC,BACKREF

      REAL :: CellPar, ZeroPoint
	  LOGICAL CellParConstrained 
      COMMON /CELLREF/ CellPar(6), ZeroPoint, CellParConstrained(6)

      INTEGER MVAL
      PARAMETER (MVAL=50)
      INTEGER LatBrav
      INTEGER IHLR
      COMMON /LATREFCMN/ LatBrav,IHLR(3,MVAL) 

! LatBrav :  1 = Triclinic
!            2 = Monoclinic-a
!            3 = Monoclinic-b
!            4 = Monoclinic-c
!            5 = Orthorhombic
!            6 = Tetragonal
!            7 = Trigonal
!            8 = Rhombohedral
!            9 = Hexagonal
!           10 = Cubic

      INTEGER MaxSPGR
      PARAMETER (MaxSPGR=530)
! MaxSPGR is the maximum number of space groups including all different settings.
! These are loaded from a file at the beginning.

      INTEGER           :: LPosSG
      CHARACTER(LEN=12) :: SGNumStr
      CHARACTER(LEN=12) :: SGHMaStr
      CHARACTER(LEN=24) :: SGShmStr
      COMMON /SGStrCmn/ LPosSG(12), SGNumStr(MaxSPGR),                  &
     &                  SGHMaStr(MaxSPGR), SGShmStr(MaxSPGR)

! LPosSG contains the offset per crystal system in the space group list
! E.g., the monoclinic-a (crystal system nr. 2) space groups start at entry 3 and
! end at entry 37
!      LPosSG( 1) =   1
!      LPosSG( 2) =   3
!      LPosSG( 3) =  38 
!      LPosSG( 4) =  73
!      LPosSG( 5) = 108
!      LPosSG( 6) = 349
!      LPosSG( 7) = 430
!      LPosSG( 8) = 455
!      LPosSG( 9) = 462
!      LPosSG(10) = 489
!      LPosSG(11) = MaxSPGR+1
