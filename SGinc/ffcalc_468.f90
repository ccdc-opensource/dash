!*==AA0042.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
!	 Structure factor calculations for space group P-6
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
      AFCAL = 0.
      BFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      II = -(IH+IK)
      IL = IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
        DO N = 1, NATOM
          CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)           &
     &            *SINQS(II,2,N)
          CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)           &
     &            *SINQS(IH,2,N)
          CHKI = CHKI1 + CHKI2 + CHKI3
          SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)           &
     &            *SINQS(II,2,N)
          SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)           &
     &            *SINQS(IH,2,N)
          SHKI = SHKI1 + SHKI2 + SHKI3
          CLZ = COSQS(IL,3,N)
          AFCAL = AFCAL + (CHKI*CLZ)*FOB(N,IR)
          BFCAL = BFCAL + (SHKI*CLZ)*FOB(N,IR)
        ENDDO
      ELSE
        DO NS = 1, NSATOM
          N = ISATOM(NS)
          CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)           &
     &            *SINQS(II,2,N)
          CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)           &
     &            *SINQS(IH,2,N)
          CHKI = CHKI1 + CHKI2 + CHKI3
          SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)           &
     &            *SINQS(II,2,N)
          SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)           &
     &            *SINQS(IH,2,N)
          SHKI = SHKI1 + SHKI2 + SHKI3
          CLZ = COSQS(IL,3,N)
          AFCAL = AFCAL + (CHKI*CLZ)*FOB(N,IR)
          BFCAL = BFCAL + (SHKI*CLZ)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_468 = AFCAL*AFCAL + BFCAL*BFCAL
