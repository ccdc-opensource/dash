C SUM OVER ATOMS IN ASYMMETRIC UNIT:
C.. For P21/c
      AFCAL=0.
      IH=IREFH(1,IR)
      IK=IREFH(2,IR)
      IL=IREFH(3,IR)
C.. LOGREF = .TRUE. means that k+l=2n
      IF (LOGREF(1,IR)) THEN
        DO N = 1,NATOM
         AFCAL=AFCAL+
     &   (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)*SINQS(IL,3,N))*
     &    COSQS(IK,2,N)*FOB(N,IR)
        END DO
      ELSE
        DO N = 1,NATOM
         AFCAL=AFCAL-
     &   (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)*SINQS(IL,3,N))*
     &    SINQS(IK,2,N)*FOB(N,IR)
        END DO
      END IF
      FFCALC=AFCAL*AFCAL
