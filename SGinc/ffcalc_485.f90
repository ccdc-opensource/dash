!*==AA0047.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
!
!	 Structure factor calculations for space group P6/mmm
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
!
!
      AFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      II = -(IH+IK)
      IL = IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
        DO N = 1, NATOM
!
          CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)           &
     &            *SINQS(II,2,N)
          CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)           &
     &            *SINQS(IH,2,N)
!
          CHKI = CHKI1 + CHKI2 + CHKI3
!
          CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)           &
     &            *SINQS(IH,2,N)
          CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(II,2,N)
          CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)           &
     &            *SINQS(IK,2,N)
!
          CKHI = CKHI1 + CKHI2 + CKHI3
!
          RPHCC = CHKI + CKHI
!
          CLZ = COSQS(IL,3,N)
!
          AFCAL = AFCAL + (RPHCC*CLZ)*FOB(N,IR)
!
        ENDDO
      ELSE
        DO NS = 1, NSATOM
          N = ISATOM(NS)
!
          CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)           &
     &            *SINQS(II,2,N)
          CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)           &
     &            *SINQS(IH,2,N)
!
          CHKI = CHKI1 + CHKI2 + CHKI3
!
          CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)           &
     &            *SINQS(IH,2,N)
          CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(II,2,N)
          CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)           &
     &            *SINQS(IK,2,N)
!
          CKHI = CKHI1 + CKHI2 + CKHI3
!
          RPHCC = CHKI + CKHI
!
          CLZ = COSQS(IL,3,N)
!
          AFCAL = AFCAL + (RPHCC*CLZ)*FOB(N,IR)
!
        ENDDO
      ENDIF
      FFCALC_485 = AFCAL*AFCAL
