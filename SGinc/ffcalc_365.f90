!*==AA0031.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
! Structure factor calculations for space group I 41/a (origin choice 2)
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
      AFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      IL = IREFH(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term2 = SINQS(IH,1,N)*SINQS(IK,2,N) - SINQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          AFCAL = AFCAL + term4*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = COSQS(IH,2,N)*COSQS(IK,1,N) + SINQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term4 = COSQS(IL,3,N)
          term5 = term1*term2 - term3*term4
          AFCAL = AFCAL - term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = SINQS(IH,2,N)*COSQS(IK,1,N) - COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term4 = SINQS(IL,3,N)
          term5 = term1*term2 - term3*term4
          AFCAL = AFCAL + term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*SINQS(IK,2,N) - COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term2 = SINQS(IH,1,N)*COSQS(IK,2,N) + SINQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term3 = SINQS(IL,3,N)
          term4 = (term1+term2)*term3
          AFCAL = AFCAL - term4*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(5,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term2 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          AFCAL = AFCAL + term4*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(6,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = COSQS(IH,2,N)*COSQS(IK,1,N) + SINQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term4 = COSQS(IL,3,N)
          term5 = term1*term2 + term3*term4
          AFCAL = AFCAL - term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(7,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)           &
     &            *SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = SINQS(IH,2,N)*COSQS(IK,1,N) - COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term4 = SINQS(IL,3,N)
          term5 = term1*term2 + term3*term4
          AFCAL = AFCAL + term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(8,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*SINQS(IK,2,N) + COSQS(IH,2,N)           &
     &            *SINQS(IK,1,N)
          term2 = SINQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,2,N)           &
     &            *COSQS(IK,1,N)
          term3 = SINQS(IL,3,N)
          term4 = (term1+term2)*term3
          AFCAL = AFCAL - term4*fob(n,ir)
        ENDDO
      ENDIF
      FFCALC_365 = AFCAL*AFCAL
