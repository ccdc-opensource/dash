C Structure factor calculations for space group P1
C Loop is performed over the atoms in the asymmetric unit
      AFCAL=0.
      BFCAL=0.
      IH=IREFH(1,IR)
      IK=IREFH(2,IR)
      IL=IREFH(3,IR)
C
      IF (LOG_HYDROGENS) THEN
        DO N = 1,NATOM
         C3F=COSQS(IL,3,N)*FOB(N,IR)
         S3F=SINQS(IL,3,N)*FOB(N,IR)
         XCC=COSQS(IK,2,N)*C3F
         XSC=SINQS(IK,2,N)*C3F
         XCS=COSQS(IK,2,N)*S3F
         XSS=SINQS(IK,2,N)*S3F
         CCC=COSQS(IH,1,N)*XCC
         CSS=COSQS(IH,1,N)*XSS
         SCS=SINQS(IH,1,N)*XCS
         SSC=SINQS(IH,1,N)*XSC
         SCC=SINQS(IH,1,N)*XCC
         CSC=COSQS(IH,1,N)*XSC
         CCS=COSQS(IH,1,N)*XCS
         SSS=SINQS(IH,1,N)*XSS
         AFCAL=AFCAL+CCC-(CSS+SCS+SSC)
         BFCAL=BFCAL+SCC+CSC+CCS-SSS
        END DO
      ELSE
        DO NS = 1,NSATOM
         N=ISATOM(NS)
         C3F=COSQS(IL,3,N)*FOB(N,IR)
         S3F=SINQS(IL,3,N)*FOB(N,IR)
         XCC=COSQS(IK,2,N)*C3F
         XSC=SINQS(IK,2,N)*C3F
         XCS=COSQS(IK,2,N)*S3F
         XSS=SINQS(IK,2,N)*S3F
         CCC=COSQS(IH,1,N)*XCC
         CSS=COSQS(IH,1,N)*XSS
         SCS=SINQS(IH,1,N)*XCS
         SSC=SINQS(IH,1,N)*XSC
         SCC=SINQS(IH,1,N)*XCC
         CSC=COSQS(IH,1,N)*XSC
         CCS=COSQS(IH,1,N)*XCS
         SSS=SINQS(IH,1,N)*XSS
         AFCAL=AFCAL+CCC-(CSS+SCS+SSC)
         BFCAL=BFCAL+SCC+CSC+CCS-SSS
        END DO
      END IF
C..
      FFCALC_001=AFCAL*AFCAL+BFCAL*BFCAL
