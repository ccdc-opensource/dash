!*==AA0022.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
!
!
!
! Structure factor calculations for space group F d d 2
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
!
! Note the use here of a prefactor of '2' for logrefs of 1 and 3
! to take into account the double weighting of these SF expression
! as seen in Vol B
!
      AFCAL = 0.
      BFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      IL = IREFH(3,IR)
      IF (LOG_HYDROGENS) THEN
        IF (LOGREF(1,IR)) THEN
          DO N = 1, NATOM
            AFCAL = AFCAL + 2.*COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)&
     &              *FOB(N,IR)
            BFCAL = BFCAL + 2.*COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)&
     &              *FOB(N,IR)
          ENDDO
        ELSEIF (LOGREF(2,IR)) THEN
          DO N = 1, NATOM
            AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)          &
     &              -COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)          &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N))         &
     &              *FOB(N,IR)
            BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)          &
     &              +COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)          &
     &              +SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N))         &
     &              *FOB(N,IR)
          ENDDO
        ELSEIF (LOGREF(3,IR)) THEN
          DO N = 1, NATOM
            AFCAL = AFCAL - 2.*SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)&
     &              *FOB(N,IR)
            BFCAL = BFCAL - 2.*SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)&
     &              *FOB(N,IR)
          ENDDO
        ELSEIF (LOGREF(4,IR)) THEN
          DO N = 1, NATOM
            AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)          &
     &              +COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)          &
     &              +SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N))         &
     &              *FOB(N,IR)
            BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)          &
     &              -COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)          &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N))         &
     &              *FOB(N,IR)
          ENDDO
        ENDIF
      ELSE
        IF (LOGREF(1,IR)) THEN
          DO NS = 1, NSATOM
            N = ISATOM(NS)
            AFCAL = AFCAL + 2.*COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)&
     &              *FOB(N,IR)
            BFCAL = BFCAL + 2.*COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)&
     &              *FOB(N,IR)
          ENDDO
        ELSEIF (LOGREF(2,IR)) THEN
          DO NS = 1, NSATOM
            N = ISATOM(NS)
            AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)          &
     &              -COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)          &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N))         &
     &              *FOB(N,IR)
            BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)          &
     &              +COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)          &
     &              +SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N))         &
     &              *FOB(N,IR)
          ENDDO
        ELSEIF (LOGREF(3,IR)) THEN
          DO NS = 1, NSATOM
            N = ISATOM(NS)
            AFCAL = AFCAL - 2.*SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)&
     &              *FOB(N,IR)
            BFCAL = BFCAL - 2.*SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)&
     &              *FOB(N,IR)
          ENDDO
        ELSEIF (LOGREF(4,IR)) THEN
          DO NS = 1, NSATOM
            N = ISATOM(NS)
            AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)          &
     &              +COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)          &
     &              +SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N))         &
     &              *FOB(N,IR)
            BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)  &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)          &
     &              -COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)          &
     &              -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N))         &
     &              *FOB(N,IR)
          ENDDO
        ENDIF
      ENDIF
      FFCALC_212 = AFCAL*AFCAL + BFCAL*BFCAL
