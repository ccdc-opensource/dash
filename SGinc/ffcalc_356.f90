!*==AA0030.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
! Structure factor calculations for space group I-4
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
      AFCAL = 0.
      BFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      IL = IREFH(3,IR)
      DO N = 1, NATOM
        term1 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)             &
     &          *COSQS(IK,1,N)
        term2 = SINQS(IH,1,N)*SINQS(IK,2,N) - SINQS(IH,2,N)             &
     &          *SINQS(IK,1,N)
        term3 = COSQS(IL,3,N)
        term4 = (term1-term2)*term3
        AFCAL = AFCAL + term4*fob(n,ir)
        term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)             &
     &          *COSQS(IK,1,N)
        term2 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)             &
     &          *SINQS(IK,1,N)
        term3 = SINQS(IL,3,N)
        term4 = (term1-term2)*term3
        BFCAL = BFCAL + term4*fob(n,ir)
      ENDDO
      FFCALC_356 = AFCAL*AFCAL + BFCAL*BFCAL
