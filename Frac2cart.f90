!O!
!O!*****************************************************************************
!O!
!O      SUBROUTINE FRAC2CART(MATRIX,A,B,C,ALFA,BETA,GAMA)
!O
!O      REAL MATRIX(3,3), A, B, C, ALFA, BETA, GAMA
!O      REAL RADEG, CA, CB, CG, SA, ASA, TERM
!O      DATA RADEG /1.7453292519943296E-2 /
!O
!O      CA = COS(ALFA*RADEG)
!O      CB = COS(BETA*RADEG)
!O      CG = COS(GAMA*RADEG)
!O      SA = SIN(ALFA*RADEG)
!O      ASA = A/SA
!O      TERM = SQRT(ABS(1.0E0-CA**2-CB**2-CG**2+2.0*CA*CB*CG)+SMALL)
!O      MATRIX(1,1) = ASA*TERM
!O      MATRIX(2,1) = 0.0
!O      MATRIX(3,1) = 0.0
!O      MATRIX(1,2) = -ASA*(CA*CB-CG)
!O      MATRIX(2,2) = B*SA
!O      MATRIX(3,2) = 0.0
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
      REAL A, B, C, ALFA, BETA, GAMA
      REAL RADEG, CA, CB, CG, SG, VOLU
      DATA RADEG /1.7453292519943296E-2/
! This routine will create the conversion matrix for fractional to orthogonal
! coordinates as defined in the PDB manual (i.e. a along X)
      CA = COS(ALFA*RADEG)
      CB = COS(BETA*RADEG)
      CG = COS(GAMA*RADEG)
      SG = SIN(GAMA*RADEG)
      VOLU = A*B*C*SQRT(1.0-CA*CA-CB*CB-CG*CG+2.0*CA*CB*CG)
      MATRIX(1,1) = A
      MATRIX(2,1) = 0.0
      MATRIX(3,1) = 0.0
      MATRIX(1,2) = B*CG
      MATRIX(2,2) = B*SG
      MATRIX(3,2) = 0.0
      MATRIX(1,3) = C*CB
      MATRIX(2,3) = C*(CA-CB*CG)/SG
      MATRIX(3,3) = VOLU/(A*B*SG)

      END SUBROUTINE FRAC2PDB
!
!*****************************************************************************
!
