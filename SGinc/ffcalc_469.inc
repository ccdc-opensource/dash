C	 Structure factor calculations for space group P6/m
C Loop is performed over the atoms in the asymmetric unit
C See get_logref.inc for a description of the LOGREF conditions
      AFCAL=0.
	  IH=IREFH(1,IR)
      IK=IREFH(2,IR)
      II=-(IH+IK)
      IL=IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
        DO N = 1,NATOM
         CHKI1=COSQS(IH,1,N)*COSQS(IK,2,N)-SINQS(IH,1,N)*SINQS(IK,2,N)
         CHKI2=COSQS(IK,1,N)*COSQS(II,2,N)-SINQS(IK,1,N)*SINQS(II,2,N)
         CHKI3=COSQS(II,1,N)*COSQS(IH,2,N)-SINQS(II,1,N)*SINQS(IH,2,N)
         CHKI=CHKI1+CHKI2+CHKI3
         CLZ=COSQS(IL,3,N)
         AFCAL=AFCAL+(CHKI*CLZ)*FOB(N,IR)
        END DO
      ELSE
        DO NS = 1,NSATOM
         N=ISATOM(NS)
         CHKI1=COSQS(IH,1,N)*COSQS(IK,2,N)-SINQS(IH,1,N)*SINQS(IK,2,N)
         CHKI2=COSQS(IK,1,N)*COSQS(II,2,N)-SINQS(IK,1,N)*SINQS(II,2,N)
         CHKI3=COSQS(II,1,N)*COSQS(IH,2,N)-SINQS(II,1,N)*SINQS(IH,2,N)
         CHKI=CHKI1+CHKI2+CHKI3
         CLZ=COSQS(IL,3,N)
         AFCAL=AFCAL+(CHKI*CLZ)*FOB(N,IR)
        END DO
      END IF
      FFCALC_469=AFCAL*AFCAL
