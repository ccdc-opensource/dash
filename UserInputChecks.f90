!
!*****************************************************************************
!
      LOGICAL FUNCTION FnWavelengthOK()
!
! Checks if wavelength available and acceptable
!
! JvdS 29 July 2001
!
! RETURNS : .TRUE.  if the value of 'alambda' can be used
!           .FALSE. if the value of 'alambda' is ridiculous
!
      INCLUDE 'lattice.inc' ! Contains wavelength
      INCLUDE 'GLBVAR.INC' ! Contains ALambda

      FnWavelengthOK = ((ALambda .GT. 0.1) .AND. (ALambda .LT. 20.0))

      END FUNCTION FnWavelengthOK
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
      REAL    a,b,c,d2r,calp,cbet,cgam,arg,vcell
      
! Initialise to 'cell is not OK'
      FnUnitCellOK = .FALSE.
! Check if the user has at least entered a value for every cell parameter
      DO I = 1, 6
        IF (CellPar(I) .LT. 0.000001) RETURN
      END DO
! Check if the unit cell volume makes sense
! d2r converts degrees to radians
      d2r = ATAN(1.0)/45.0
      a = cellpar(1)
      b = cellpar(2)
      c = cellpar(3)
      calp = COS(d2r*cellpar(4))
      cbet = COS(d2r*cellpar(5))
      cgam = COS(d2r*cellpar(6))
      arg = 1.0 + 2.0*calp*cbet*cgam-(calp**2+cbet**2+cgam**2)
      VCELL = a*b*c*SQRT(MAX(0.0,arg))
      IF (VCELL .LT. 10) RETURN
      FnUnitCellOK = .TRUE.
      RETURN

      END FUNCTION FnUnitCellOK
!
!*****************************************************************************
!
