!*==AA0002.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
! Structure factor calculations for space group P-1
! Loop is performed over the atoms in the asymmetric unit
      AFCAL = 0.
      IH = IREFH(1,IR)
      IK = IREFH(2,IR)
      IL = IREFH(3,IR)
!..
      IF (LOG_HYDROGENS) THEN
!..
        DO N = 1, NATOM
          C3F = COSQS(IL,3,N)*FOB(N,IR)
          S3F = SINQS(IL,3,N)*FOB(N,IR)
          CCC = COSQS(IH,1,N)*COSQS(IK,2,N)*C3F
          CSS = COSQS(IH,1,N)*SINQS(IK,2,N)*S3F
          SCS = SINQS(IH,1,N)*COSQS(IK,2,N)*S3F
          SSC = SINQS(IH,1,N)*SINQS(IK,2,N)*C3F
          AFCAL = AFCAL + CCC - (CSS+SCS+SSC)
        ENDDO
!..
      ELSE
!..
        DO NS = 1, NSATOM
          N = ISATOM(NS)
          C3F = COSQS(IL,3,N)*FOB(N,IR)
          S3F = SINQS(IL,3,N)*FOB(N,IR)
          CCC = COSQS(IH,1,N)*COSQS(IK,2,N)*C3F
          CSS = COSQS(IH,1,N)*SINQS(IK,2,N)*S3F
          SCS = SINQS(IH,1,N)*COSQS(IK,2,N)*S3F
          SSC = SINQS(IH,1,N)*SINQS(IK,2,N)*C3F
          AFCAL = AFCAL + CCC - (CSS+SCS+SSC)
        ENDDO
!..
      ENDIF
      FFCALC_002 = AFCAL*AFCAL
