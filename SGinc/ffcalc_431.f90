!*==AA0034.f90  processed by SPAG 6.11Dc at 13:43 on 17 Sep 2001
! SUM OVER ATOMS IN ASYMMETRIC UNIT:
!.. For P31
      AFCAL = 0.
      BFCAL = 0.
!      CFCAL=0.
!      DFCAL=0.
      RH = 6.283185307179*FLOAT(IREFH(1,IR))
      RK = 6.283185307179*FLOAT(IREFH(2,IR))
      RI = -(RH+RK)
      RL = 6.283185307179*FLOAT(IREFH(3,IR))
      VL = RL/3.
      IF (LOG_HYDROGENS) THEN
        DO N = 1, NATOM
          XV = X(1,N)
          YV = X(2,N)
          RLZV = RL*X(3,N)
          ARG1 = RH*XV + RK*YV + RLZV
          ARG2 = RK*XV + RI*YV + RLZV + VL
          ARG3 = RI*XV + RH*YV + RLZV - VL
          AFCAL = AFCAL + (COS(ARG1)+COS(ARG2)+COS(ARG3))*FOB(N,IR)
          BFCAL = BFCAL + (SIN(ARG1)+SIN(ARG2)+SIN(ARG3))*FOB(N,IR)
!         ARG2=RK*XV+RI*YV+RLZV-VL
!         ARG3=RI*XV+RH*YV+RLZV+VL
!         CFCAL=CFCAL+(COS(ARG1)+COS(ARG2)+COS(ARG3))*FOB(N,IR)
!         DFCAL=DFCAL+(SIN(ARG1)+SIN(ARG2)+SIN(ARG3))*FOB(N,IR)
        ENDDO
      ELSE
        DO NS = 1, NSATOM
          N = ISATOM(NS)
          XV = X(1,N)
          YV = X(2,N)
          RLZV = RL*X(3,N)
          ARG1 = RH*XV + RK*YV + RLZV
          ARG2 = RK*XV + RI*YV + RLZV + VL
          ARG3 = RI*XV + RH*YV + RLZV - VL
          AFCAL = AFCAL + (COS(ARG1)+COS(ARG2)+COS(ARG3))*FOB(N,IR)
          BFCAL = BFCAL + (SIN(ARG1)+SIN(ARG2)+SIN(ARG3))*FOB(N,IR)
!         ARG2=RK*XV+RI*YV+RLZV-VL
!         ARG3=RI*XV+RH*YV+RLZV+VL
!         CFCAL=CFCAL+(COS(ARG1)+COS(ARG2)+COS(ARG3))*FOB(N,IR)
!         DFCAL=DFCAL+(SIN(ARG1)+SIN(ARG2)+SIN(ARG3))*FOB(N,IR)
        ENDDO
      ENDIF
!      RAT=0.0
!      FF1=AFCAL*AFCAL+BFCAL*BFCAL
!      FF2=CFCAL*CFCAL+DFCAL*DFCAL
!      FFCALC_431=RAT*FF1+(1.-RAT)*FF2
      FFCALC_431 = AFCAL*AFCAL + BFCAL*BFCAL
