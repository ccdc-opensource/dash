!*==AA0032.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
!
! Structure factor calculations for space group P 41 21 2
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
      AFCAL = 0.
      BFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      IL = IREFH(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term2 = COSQS(IL,3,N)
          term3 = 2.*term1*term2
          AFCAL = AFCAL + term3*fob(n,ir)
          term1 = SINQS(IH,1,N)*SINQS(IK,2,N) - SINQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term2 = SINQS(IL,3,N)
          term3 = 2.*term1*term2
          BFCAL = BFCAL - term3*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + SINQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term2 = COSQS(IH,1,N)*SINQS(IK,2,N) + COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          term5 = COSQS(IH,1,N)*SINQS(IK,2,N) - COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term6 = SINQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term7 = SINQS(IL,3,N)
          term8 = (term5+term6)*term7
          AFCAL = AFCAL + (term4-term8)*fob(n,ir)
          term4 = (term1+term2)*term3
          term8 = (term5-term6)*term7
          BFCAL = BFCAL + (term4+term8)*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term2 = COSQS(IL,3,N)
          term3 = 2.*term1*term2
          AFCAL = AFCAL - term3*fob(n,ir)
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term2 = SINQS(IL,3,N)
          term3 = 2.*term1*term2
          BFCAL = BFCAL + term3*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + SINQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term2 = COSQS(IH,1,N)*SINQS(IK,2,N) + COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          term5 = COSQS(IH,1,N)*SINQS(IK,2,N) - COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term6 = SINQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term7 = SINQS(IL,3,N)
          term8 = (term5+term6)*term7
          AFCAL = AFCAL - (term4+term8)*fob(n,ir)
          term4 = (term1+term2)*term3
          term8 = (term5-term6)*term7
          BFCAL = BFCAL + (term4-term8)*fob(n,ir)
        ENDDO
      ENDIF
!       write(76,*) ih,ik,il,AFCAL*AFCAL+BFCAL*BFCAL
      FFCALC_369 = AFCAL*AFCAL + BFCAL*BFCAL
