C LEVEL 3      SUBROUTINE ABCALC(IR,AFCAL,BFCAL)
      SUBROUTINE ABCALC(IR,AFCAL,BFCAL)
C
C *** FCALC by JCM 19 Jul 83 ***
C
CX
CC 4B
CH Calculates the COMPLEX nuclear structure factor for the reflection H.
CA On entry H is a 1x3 read vector holding h,k,l.
CA On exit FCALC holds the COMPLEX nuclear structure factor
CP PREFIN, RECIP, SYMOP, SETFOR, ATOPOS and SETANI must be called before the
CP first call to FCALC.  (All these except PREFIN are all in SETFC)
CD Forms sin theta/lambda and leaves it in STHL in /BRAGG
CD Cycles over atomic positions, then over symmetry operators, forming
CD COMPLEX FCALC by the usual formula.
CD
CD Applies scattering factor, site occupation factor, multiplicity of atom and
CD individual isotropic or anisotropic temperature factors.
C
	INCLUDE 'params.inc'
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /FCSTOR/ MAXK,FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)
      COMMON /CSQSTO/ COSQS(-20:20,3,150),SINQS(-20:20,3,150)
C
      INCLUDE 'FFCALC.INC'
C
      RETURN
      END

