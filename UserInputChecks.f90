!
!*****************************************************************************
!
      LOGICAL FUNCTION FnWavelengthOK
!
! Checks if wavelength available and acceptable
!
! JvdS 29 July 2001
!
! RETURNS : .TRUE.  if the value of 'ALambda' can be used
!           .FALSE. if the value of 'ALambda' is ridiculous
!
      IMPLICIT NONE
      
      INCLUDE 'GLBVAR.INC' ! Contains ALambda
      INCLUDE 'lattice.inc' ! Contains wavelength

      FnWavelengthOK = ((ALambda .GT. 0.1) .AND. (ALambda .LT. 20.0))

      END FUNCTION FnWavelengthOK
!
!*****************************************************************************
!
      LOGICAL FUNCTION FnPatternOK
!
! Checks if diffraction pattern available and acceptable
!
! RETURNS : .TRUE.  if - we have data at all
!                      - we have more than 15 points (that's the minimum needed for fitting a single peak)
!                      - we have more than 1 degree 2 theta
!           .FALSE. otherwise
!
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      
      FnPatternOK = .FALSE.
      IF (NBIN .LT. 15) RETURN
      IF ((XPMAX-XPMIN) .LT. 1.0) RETURN
      FnPatternOK = .TRUE.

      END FUNCTION FnPatternOK
!
!*****************************************************************************
!
      LOGICAL FUNCTION ValidCellAxisLength(TheValue)
!
! This function establishes if a REAL is a valid length for a unit cell axis
!
! INPUT   : TheValue = the alleged cell parameter
!
! RETURNS : .TRUE.  if 0.000001 < TheValue < 1000.0
!           .FALSE. otherwise
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheValue

      ValidCellAxisLength = ((TheValue .GT. 0.000001) .AND. (TheValue .LT. 1000.0))

      END FUNCTION ValidCellAxisLength
!
!*****************************************************************************
!
      REAL FUNCTION UnitCellVolume(The_a, The_b, The_c, TheAlpha, TheBeta, TheGamma)
!
! Calculates the unit cell volume given the unit cell parameters.
!
! RETURNS : Unit cell volume
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) ::  The_a, The_b, The_c, TheAlpha, TheBeta, TheGamma

      REAL, EXTERNAL :: Degrees2Radians
      REAL    calp, cbet, cgam, arg
      
      calp = COS(Degrees2Radians(TheAlpha))
      cbet = COS(Degrees2Radians(TheBeta))
      cgam = COS(Degrees2Radians(TheGamma))
      arg = 1.0 + 2.0*calp*cbet*cgam-(calp**2+cbet**2+cgam**2)
      UnitCellVolume = The_a*The_b*The_c*SQRT(MAX(0.0,arg))

      END FUNCTION UnitCellVolume
!
!*****************************************************************************
!
      LOGICAL FUNCTION FnUnitCellOK
!
! Checks if all cell parameters available and acceptable
!
! JvdS 01 Aug 2001
!
! RETURNS : .TRUE.  if the cell parameters can be used
!           .FALSE. if the cell parameters are ridiculous
!
      IMPLICIT NONE

      INCLUDE 'lattice.inc'

      LOGICAL, EXTERNAL :: ValidCellAxisLength
      REAL,    EXTERNAL :: UnitCellVolume
      INTEGER I
      
! Initialise to 'unit cell is not OK'
      FnUnitCellOK = .FALSE.
! Check if the user has at least entered a value for every cell parameter
      DO I = 1, 3
        IF (.NOT. ValidCellAxisLength(CellPar(I))) RETURN
      ENDDO
      DO I = 4, 6
        IF (CellPar(I) .LT. 0.001) RETURN
      ENDDO
! Check if the unit cell volume makes sense
      IF (UnitCellVolume(CellPar(1),CellPar(2),CellPar(3),CellPar(4),CellPar(5),CellPar(6)) .LT. 10.0) RETURN
      FnUnitCellOK = .TRUE.

      END FUNCTION FnUnitCellOK
!
!*****************************************************************************
!
      SUBROUTINE CheckUnitCellConsistency

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL, EXTERNAL :: FnUnitCellOK, Confirm
      LOGICAL, EXTERNAL :: NearlyEqual
      LOGICAL ABC_Same, AB_Same, AC_Same, BC_Same, Ang_Same, Alp_90, Bet_90, Gam_90, Gam_120
      INTEGER tLatBrav

!            1 = Triclinic
!            2 = Monoclinic-a
!            3 = Monoclinic-b
!            4 = Monoclinic-c
!            5 = Orthorhombic
!            6 = Tetragonal
!            7 = Trigonal
!            8 = Rhombohedral
!            9 = Hexagonal
!           10 = Cubic

      IF (.NOT. FnUnitCellOK()) RETURN
      AB_Same  = NearlyEqual(CellPar(2),CellPar(1))
      BC_Same  = NearlyEqual(CellPar(3),CellPar(2))
      AC_Same  = NearlyEqual(CellPar(3),CellPar(1))
      ABC_Same = (AB_Same .AND. BC_Same) 
      Alp_90   = NearlyEqual(CellPar(4), 90.0)
      Bet_90   = NearlyEqual(CellPar(5), 90.0)
      Gam_90   = NearlyEqual(CellPar(6), 90.0)
      Gam_120  = NearlyEqual(CellPar(6),120.0)
      Ang_Same = NearlyEqual(CellPar(6),CellPar(5)) .AND. &
                 NearlyEqual(CellPar(5),CellPar(4))
      IF (ABC_Same .AND. Ang_Same) THEN
        IF (Alp_90) THEN
          tLatBrav = 10 ! Cubic
          GOTO 10
        ELSE
          tLatBrav = 8 ! Rhombohedral
          GOTO 10
        ENDIF
      ENDIF
      IF (AB_Same) THEN
        IF (Ang_Same .AND. Alp_90) THEN
          tLatBrav = 6 ! Tetragonal
          GOTO 10
        ELSE IF (Alp_90 .AND. Bet_90 .AND. Gam_120) THEN
          tLatBrav = 9 ! Hexagonal
          GOTO 10
        ENDIF
      ENDIF
      IF (Ang_Same .AND. Alp_90) THEN
        tLatBrav = 5 ! Orthorhombic
        GOTO 10
      ENDIF
      IF (           Alp_90 .AND.       Bet_90 .AND. .NOT. Gam_90) THEN
        tLatBrav = 4 ! Monoclinic-c
        GOTO 10
      ELSE IF (      Alp_90 .AND. .NOT. Bet_90 .AND.       Gam_90) THEN
        tLatBrav = 3 ! Monoclinic-b
        GOTO 10
      ELSE IF (.NOT. Alp_90 .AND.       Bet_90 .AND.       Gam_90) THEN
        tLatBrav = 2 ! Monoclinic-a
        GOTO 10
      ENDIF
      tLatBrav = 1 ! Triclinic
   10 CONTINUE
! Now, tLatBrav holds the crystal system as determined from the unit cell parameters.
! Compare it to the crystal system as set by the user (LatBrav) and issue a warning
! message if they don't match.
      IF (LatBrav .EQ. tLatBrav) RETURN
      IF (Confirm('The unit cell parameters point to a crystal system of higher symmetry.'//CHAR(13)// &
          'Would you like to set the crystal system to '// &
          CrystalSystemString(tLatBrav)(1:LEN_TRIM(CrystalSystemString(tLatBrav)))//' ?')) THEN
        LatBrav = tLatBrav
        CALL Upload_CrystalSystem
      ENDIF

      END SUBROUTINE CheckUnitCellConsistency
!
!*****************************************************************************
!
