C Structure factor calculations for space group P n m a
C Loop is performed over the atoms in the asymmetric unit
C See get_logref.inc for a description of the LOGREF conditions
      AFCAL=0.
      IH=IREFH(1,IR)
      IK=IREFH(2,IR)
      IL=IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
       IF (LOGREF(1,IR)) THEN
        DO N = 1,NATOM
         AFCAL=AFCAL+COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        END DO
      ELSE IF (LOGREF(2,IR)) THEN
        DO N = 1,NATOM
         AFCAL=AFCAL-SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        END DO
       ELSE IF (LOGREF(3,IR)) THEN
        DO N = 1,NATOM
         AFCAL=AFCAL-SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        END DO
       ELSE IF (LOGREF(4,IR)) THEN
        DO N = 1,NATOM
         AFCAL=AFCAL-COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        END DO
       END IF
      ELSE
       IF (LOGREF(1,IR)) THEN
        DO NS = 1,NATOM
         N=ISATOM(NS)
         AFCAL=AFCAL+COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        END DO
      ELSE IF (LOGREF(2,IR)) THEN
        DO NS = 1,NATOM
         N=ISATOM(NS)
         AFCAL=AFCAL-SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        END DO
       ELSE IF (LOGREF(3,IR)) THEN
        DO NS = 1,NATOM
         N=ISATOM(NS)
         AFCAL=AFCAL-SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        END DO
       ELSE IF (LOGREF(4,IR)) THEN
        DO NS = 1,NATOM
         N=ISATOM(NS)
         AFCAL=AFCAL-COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        END DO
       END IF
      END IF
      FFCALC_292=AFCAL*AFCAL
