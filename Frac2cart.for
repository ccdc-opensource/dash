CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FRAC2CART(MATRIX,A,B,C,ALFA,BETA,GAMA)
C     -------------------------------------------------
C
      REAL*8 MATRIX(3,3),A,B,C,ALFA,BETA,GAMA
      REAL*8 RADEG,CA,CB,CG,SA,ASA,TERM,SMALL,ONE,TWO,ZERO
      DATA   RADEG,SMALL,ZERO /1.7453292519943296D-2,1.0D-20,0.0D0/
C
      CA=DCOS(ALFA*RADEG)
      CB=DCOS(BETA*RADEG)
      CG=DCOS(GAMA*RADEG)
      SA=DSIN(ALFA*RADEG)
      ASA=A/SA
      TERM=DSQRT(ABS(1.0D0-CA**2-CB**2-CG**2+2.0D0*CA*CB*CG)+SMALL)
      MATRIX(1,1)=ASA*TERM
      MATRIX(2,1)=ZERO
      MATRIX(3,1)=ZERO
      MATRIX(1,2)=-ASA*(CA*CB-CG)
      MATRIX(2,2)=B*SA
      MATRIX(3,2)=ZERO
      MATRIX(1,3)=A*CB
      MATRIX(2,3)=B*CA
      MATRIX(3,3)=C
      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C>> JCC 
      SUBROUTINE FRAC2PDB(MATRIX,A,B,C,ALFA,BETA,GAMA)
      REAL*8 MATRIX(3,3),A,B,C,ALFA,BETA,GAMA
      REAL*8 RADEG,CA,CB,CG,SG,VOLU,SMALL,ZERO
      DATA   RADEG,SMALL,ZERO /1.7453292519943296D-2,1.0D-20,0.0D0/
C This routine will create the conversion matrix for fractional to orthogonal
C coordinates as defined in the PDB manual
      CA=DCOS(ALFA*RADEG)
      CB=DCOS(BETA*RADEG)
      CG=DCOS(GAMA*RADEG)
      SG=DSIN(GAMA*RADEG)
	
	VOLU = A*B*C*SQRT(1 -CA*CA - CB*CB -CG*CG + 2*CA*CB*CG)
	MATRIX(1,1) = A
	MATRIX(1,2) = B*CG
	MATRIX(1,3) = C*CB
	MATRIX(2,1) = ZERO
	MATRIX(2,2) = B*SG
	MATRIX(2,3) = C*(CA - CB*CG)/SG
	MATRIX(3,1) = ZERO
	MATRIX(3,2) = ZERO
	MATRIX(3,3) = VOLU/(A*B*SG)
	END


	