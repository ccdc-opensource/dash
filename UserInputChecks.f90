!
!*****************************************************************************
!
      LOGICAL FUNCTION FnWavelengthOK()
!
! Checks if wavelength available and acceptable
!
! JvdS 29 July 2001
!
! RETURNS : .TRUE.  if the value of 'ALambda' can be used
!           .FALSE. if the value of 'ALambda' is ridiculous
!
      INCLUDE 'GLBVAR.INC' ! Contains ALambda
      INCLUDE 'lattice.inc' ! Contains wavelength

      FnWavelengthOK = ((ALambda .GT. 0.1) .AND. (ALambda .LT. 20.0))

      END FUNCTION FnWavelengthOK
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
! JvdS Oct 2001
!
! RETURNS : Unit cell volume
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) ::  The_a, The_b, The_c, TheAlpha, TheBeta, TheGamma

      REAL    calp, cbet, cgam, arg
      REAL, EXTERNAL :: Degrees2Radians
      
      calp = COS(Degrees2Radians(TheAlpha))
      cbet = COS(Degrees2Radians(TheBeta))
      cgam = COS(Degrees2Radians(TheGamma))
      arg = 1.0 + 2.0*calp*cbet*cgam-(calp**2+cbet**2+cgam**2)
      UnitCellVolume = The_a*The_b*The_c*SQRT(MAX(0.0,arg))

      END FUNCTION UnitCellVolume
!
!*****************************************************************************
!
      LOGICAL FUNCTION FnUnitCellOK()
!
! Checks if all cell parameters available and acceptable
!
! JvdS 01 Aug 2001
!
! RETURNS : .TRUE.  if the cell parameters can be used
!           .FALSE. if the cell parameters are ridiculous
!
      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC' ! Contains ALambda
      INCLUDE 'lattice.inc'

      INTEGER I
      LOGICAL, EXTERNAL :: ValidCellAxisLength
      REAL,    EXTERNAL :: UnitCellVolume
      
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
