!*==AA0004.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
! Structure factor calculations for space group C 1 2 1
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
      AFCAL = 0.
      BFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      IL = IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)   &
     &            *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)   &
     &            *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO NS = 1, NSATOM
          N = ISATOM(NS)
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)   &
     &            *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)   &
     &            *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_040 = AFCAL*AFCAL + BFCAL*BFCAL
