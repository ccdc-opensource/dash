!O!
!O!*****************************************************************************
!O!
!O      SUBROUTINE FRAC2CART(MATRIX,A,B,C,ALFA,BETA,GAMA)
!O
!O      REAL*8 MATRIX(3,3), A, B, C, ALFA, BETA, GAMA
!O      REAL*8 RADEG, CA, CB, CG, SA, ASA, TERM, SMALL, ZERO
!O      DATA RADEG, SMALL, ZERO/1.7453292519943296D-2, 1.0D-20, 0.0D0/
!O
!O      CA = DCOS(ALFA*RADEG)
!O      CB = DCOS(BETA*RADEG)
!O      CG = DCOS(GAMA*RADEG)
!O      SA = DSIN(ALFA*RADEG)
!O      ASA = A/SA
!O      TERM = DSQRT(ABS(1.0D0-CA**2-CB**2-CG**2+2.0D0*CA*CB*CG)+SMALL)
!O      MATRIX(1,1) = ASA*TERM
!O      MATRIX(2,1) = ZERO
!O      MATRIX(3,1) = ZERO
!O      MATRIX(1,2) = -ASA*(CA*CB-CG)
!O      MATRIX(2,2) = B*SA
!O      MATRIX(3,2) = ZERO
!O      MATRIX(1,3) = A*CB
!O      MATRIX(2,3) = B*CA
!O      MATRIX(3,3) = C
!O
!O      END SUBROUTINE FRAC2CART
!
!*****************************************************************************
!
      SUBROUTINE FRAC2PDB(MATRIX,A,B,C,ALFA,BETA,GAMA)

      REAL MATRIX(3,3)
      REAL*8 A, B, C, ALFA, BETA, GAMA
      REAL*8 RADEG, CA, CB, CG, SG, VOLU, ZERO
      DATA RADEG, ZERO/1.7453292519943296D-2, 0.0D0/
! This routine will create the conversion matrix for fractional to orthogonal
! coordinates as defined in the PDB manual
      CA = DCOS(ALFA*RADEG)
      CB = DCOS(BETA*RADEG)
      CG = DCOS(GAMA*RADEG)
      SG = DSIN(GAMA*RADEG)
      VOLU = A*B*C*SQRT(1-CA*CA-CB*CB-CG*CG+2*CA*CB*CG)
      MATRIX(1,1) = SNGL(A)
      MATRIX(2,1) = SNGL(ZERO)
      MATRIX(3,1) = SNGL(ZERO)
      MATRIX(1,2) = SNGL(B*CG)
      MATRIX(2,2) = SNGL(B*SG)
      MATRIX(3,2) = SNGL(ZERO)
      MATRIX(1,3) = SNGL(C*CB)
      MATRIX(2,3) = SNGL(C*(CA-CB*CG)/SG)
      MATRIX(3,3) = SNGL(VOLU/(A*B*SG))

      END SUBROUTINE FRAC2PDB
!
!*****************************************************************************
!
