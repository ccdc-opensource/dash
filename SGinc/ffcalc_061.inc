C Structure factor calculations for space group P 1 2/c 1
C Loop is performed over the atoms in the asymmetric unit
C See get_logref.inc for a description of the LOGREF conditions
      AFCAL=0.
      IH=IREFH(1,IR)
      IK=IREFH(2,IR)
      IL=IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
          IF (LOGREF(1,IR)) THEN
          DO N=1,NATOM
            AFCAL=AFCAL+
     &        (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)*SINQS(IL,3,N))
     &        *COSQS(IK,2,N)*FOB(N,IR)
            END DO
          ELSE
          DO N=1,NATOM
            AFCAL=AFCAL-
     &        (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)*SINQS(IL,3,N))
     &        *SINQS(IK,2,N)*FOB(N,IR)
          END DO
          ENDIF
        ELSE
          IF (LOGREF(1,IR)) THEN
          DO NS=1,NSATOM
            N=ISATOM(NS)
            AFCAL=AFCAL+
     &        (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)*SINQS(IL,3,N))
     &        *COSQS(IK,2,N)*FOB(N,IR)
            END DO
          ELSE
          DO NS=1,NSATOM
            N=ISATOM(NS)
            AFCAL=AFCAL-
     &        (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)*SINQS(IL,3,N))
     &        *SINQS(IK,2,N)*FOB(N,IR)
          END DO
          ENDIF
        END IF
      FFCALC_061=AFCAL*AFCAL



