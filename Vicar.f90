!
!*****************************************************************************
!
! JvdS The original routine forced the matrix holding the co-ordinates to be
! defined as (MAXATM,3) rather than the more efficient (3,MAXATM).
! As this routine is called for every move in the SA, I thought it better to rewrite it.
!
      SUBROUTINE makexyz_2(n,blen,alph,bet,iz1,iz2,iz3,Cartesian)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: n ! Number of atoms
      INTEGER iz1(*), iz2(*), iz3(*)
      DOUBLE PRECISION blen(*), alph(*), bet(*), Cartesian(1:3,*)

      REAL*8 radian, pi, sqrtpi, twosix, logten
      PARAMETER (radian=57.29577951308232088D0)
      PARAMETER (pi=3.141592653589793238D0)
      PARAMETER (sqrtpi=1.772453850905516027D0)
      PARAMETER (twosix=1.122462048309372981D0)
      PARAMETER (logten=2.302585092994045684D0)

! Local
! replace radian with rad=(1/radian)
      INTEGER i, k, i1, i2, i3
      REAL*8 bond, angle1, angle2, sign, rad
!
! cosmetic change to demo cvs
      IF (N.LT.1) RETURN
      rad = 1.0/radian
!     First atom is placed at the origin
      Cartesian(1,1) = 0.0D0
      Cartesian(2,1) = 0.0D0
      Cartesian(3,1) = 0.0D0
! Second atom is placed along the z-axis
      IF (N.LT.2) GOTO 999
      Cartesian(1,2) = 0.0D0
      Cartesian(2,2) = 0.0D0
      Cartesian(3,2) = blen(2)
! Third atom is placed in the x,z-plane
      IF (N.LT.3) GOTO 999
      Cartesian(1,3) = blen(3)*SIN(alph(3)*rad)
      Cartesian(2,3) = 0.0D0
      IF (iz1(3).EQ.1) THEN
        Cartesian(3,3) = blen(3)*COS(alph(3)*rad)
      ELSE
        Cartesian(3,3) = Cartesian(3,2) - blen(3)*COS(alph(3)*rad)
      ENDIF
! As long as atoms remain linear with the first
! two atoms, keep placing them along the z-axis
      i = 3
      IF (N.GT.3) THEN
        DO WHILE (NINT(Cartesian(1,i)*10000).EQ.0)
          i = i + 1
          i1 = iz1(i)
          i2 = iz2(i)
          IF (Cartesian(3,i1).GT.Cartesian(3,i2)) THEN
            sign = 1.0D0
          ELSE
            sign = -1.0D0
          ENDIF
          Cartesian(1,i) = blen(i)*SIN(alph(i)*rad)
          Cartesian(2,i) = 0.0D0
          Cartesian(3,i) = Cartesian(3,i1) - sign*blen(i)*COS(alph(i)*rad)
        ENDDO
      ENDIF
! Loop over each atom in turn, finding its coordinates
      k = i + 1
      IF (k.LE.n) THEN
        DO i = k, n
          i1 = iz1(i)
          i2 = iz2(i)
          i3 = iz3(i)
          bond = blen(i)
          angle1 = alph(i)
          angle2 = bet(i)
          CALL xyzatm_2(Cartesian,i,i1,bond,i2,angle1,i3,angle2)
        ENDDO
      ENDIF
  999 RETURN

      END SUBROUTINE MAKEXYZ_2
!
!*****************************************************************************
!
      SUBROUTINE xyzatm_2(Cartesian,i,i1,bond,i2,angle1,i3,angle2)
!
!     "xyzatm" computes the Cartesian coordinates of a single
!     atom from its defining internal coordinate values

      IMPLICIT NONE
!
! Arguments
      REAL*8 Cartesian(1:3,*)
      REAL*8 bond, angle1, angle2
      INTEGER i, i1, i2, i3
! Constants
      REAL*8 radian, pi
      PARAMETER (radian=57.29577951308232088D0)
      PARAMETER (pi=3.141592653589793238D0)
      REAL*8 small
      PARAMETER (small=1.D-8)
! Local
      REAL*8 ang_1, ang_2
      REAL*8 sin_1, cos_1, sin_2, cos_2
      REAL*8 cosine, one_over_sine, norm, eps, sinarg
      REAL*8 u1(3), u2(3), u3(3), u4(3), rad
!
! convert the angle values from degrees to radians;
! then find their sine and cosine values
!
      eps = 0.00000001D0
      rad = 1.0/radian
      ang_1 = angle1*rad
      ang_2 = angle2*rad
      sin_1 = SIN(ang_1)
      cos_1 = COS(ang_1)
      sin_2 = SIN(ang_2)
      cos_2 = COS(ang_2)
      u1(1) = Cartesian(1,i2) - Cartesian(1,i3)
      u1(2) = Cartesian(2,i2) - Cartesian(2,i3)
      u1(3) = Cartesian(3,i2) - Cartesian(3,i3)
      norm = 1.0/SQRT(u1(1)*u1(1)+u1(2)*u1(2)+u1(3)*u1(3))
      u1 = u1 * norm
      u2(1) = Cartesian(1,i1) - Cartesian(1,i2)
      u2(2) = Cartesian(2,i1) - Cartesian(2,i2)
      u2(3) = Cartesian(3,i1) - Cartesian(3,i2)
      norm = 1.0/SQRT(u2(1)*u2(1)+u2(2)*u2(2)+u2(3)*u2(3))
      u2 = u2 * norm
      u3(1) = u1(2)*u2(3) - u1(3)*u2(2)
      u3(2) = u1(3)*u2(1) - u1(1)*u2(3)
      u3(3) = u1(1)*u2(2) - u1(2)*u2(1)
! Dot product of u1 and u2. As |u1| = |u2| = 1, this is the cosine of the angle between u1 and u2
      cosine = u1(1)*u2(1) + u1(2)*u2(2) + u1(3)*u2(3)
! Now there follows a test if the cosine of an angle > 1.0
! I have to brush up on my mathematics here: I would never have added this test.
      IF (ABS(cosine).LT.1.0D0) THEN
        one_over_sine = 1.0/SQRT(1.0D0-cosine**2)
      ELSE
        sinarg = DMAX1(small,cosine**2-1.0D0)
        one_over_sine = 1.0/SQRT(sinarg)
      ENDIF
      u3 = u3 * one_over_sine
      u4(1) = u3(2)*u2(3) - u3(3)*u2(2)
      u4(2) = u3(3)*u2(1) - u3(1)*u2(3)
      u4(3) = u3(1)*u2(2) - u3(2)*u2(1)
      Cartesian(1,i) = Cartesian(1,i1) + bond*(-u2(1)*cos_1 + u4(1)*sin_1*cos_2 + u3(1)*sin_1*sin_2)
      Cartesian(2,i) = Cartesian(2,i1) + bond*(-u2(2)*cos_1 + u4(2)*sin_1*cos_2 + u3(2)*sin_1*sin_2)
      Cartesian(3,i) = Cartesian(3,i1) + bond*(-u2(3)*cos_1 + u4(3)*sin_1*cos_2 + u3(3)*sin_1*sin_2)

      END SUBROUTINE XYZATM_2
!
!*****************************************************************************
!

