!
!*****************************************************************************
!
      SUBROUTINE FRAC2PDB(MATRIX,A,B,C,ALFA,BETA,GAMA)

! Same as LatticeCellParameters2Lattice_2()

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
