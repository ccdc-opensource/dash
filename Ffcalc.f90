! This file contains the calculations of the squares of the structure factors.
! Loops are performed over the atoms in the asymmetric unit
! See get_logref.f90 for a description of the LOGREF conditions
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_001(IR)                  ! P 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      REAL C3F, S3F, XCC, XSC, XCS, XSS, CCC, CSS, SCS, SSC, SCC, CSC, CCS, SSS

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        C3F = COSQS(IL,3,N)*FOB(N,IR)
        S3F = SINQS(IL,3,N)*FOB(N,IR)
        XCC = COSQS(IK,2,N)*C3F
        XSC = SINQS(IK,2,N)*C3F
        XCS = COSQS(IK,2,N)*S3F
        XSS = SINQS(IK,2,N)*S3F
        CCC = COSQS(IH,1,N)*XCC
        CSS = COSQS(IH,1,N)*XSS
        SCS = SINQS(IH,1,N)*XCS
        SSC = SINQS(IH,1,N)*XSC
        SCC = SINQS(IH,1,N)*XCC
        CSC = COSQS(IH,1,N)*XSC
        CCS = COSQS(IH,1,N)*XCS
        SSS = SINQS(IH,1,N)*XSS
        AFCAL = AFCAL + CCC - (CSS+SCS+SSC)
        BFCAL = BFCAL + SCC + CSC + CCS - SSS
      ENDDO
      FFCALC_001 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_001
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_002(IR)                  ! P -1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      REAL C3F, S3F, CCC, CSS, SCS, SSC

      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        C3F = COSQS(IL,3,N)*FOB(N,IR)
        S3F = SINQS(IL,3,N)*FOB(N,IR)
        CCC = COSQS(IH,1,N)*COSQS(IK,2,N)*C3F
        CSS = COSQS(IH,1,N)*SINQS(IK,2,N)*S3F
        SCS = SINQS(IH,1,N)*COSQS(IK,2,N)*S3F
        SSC = SINQS(IH,1,N)*SINQS(IK,2,N)*C3F
        AFCAL = AFCAL + CCC - (CSS+SCS+SSC)
      ENDDO
      FFCALC_002 = AFCAL*AFCAL

      END FUNCTION FFCALC_002
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_039(IR)                  ! P 1 21 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N) &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N) &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N) &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_039 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_039
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_040(IR)                  ! C 1 2 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)   &
                *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)   &
                *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
      ENDDO
      FFCALC_040 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_040
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_044(IR)                  ! P 1 c 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_044 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_044
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_045(IR)                  ! P 1 n 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_045 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_045
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_046(IR)                  ! P 1 a 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_046 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_046
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_050(IR)                  ! C 1 c 1

      INCLUDE 'SGinc\FFCALCTOP.inc'

      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_050 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_050
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_052(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group I 1 a 1
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_052 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_052
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_057(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P1 21/m 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_057 = AFCAL*AFCAL

      END FUNCTION FFCALC_057
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_058(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group C 1 2/m 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)    &
                *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
      ENDDO
      FFCALC_058 = AFCAL*AFCAL

      END FUNCTION FFCALC_058
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_061(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 1 2/c 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_061 = AFCAL*AFCAL

      END FUNCTION FFCALC_061
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_062(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 1 2/n 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_062 = AFCAL*AFCAL

      END FUNCTION FFCALC_062
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_064(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 1 21/c 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_064 = AFCAL*AFCAL

      END FUNCTION FFCALC_064
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_065(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 1 21/n 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_065 = AFCAL*AFCAL

      END FUNCTION FFCALC_065
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_066(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 1 21/a 1

      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_066 = AFCAL*AFCAL

      END FUNCTION FFCALC_066
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_067(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group c 1 2/c 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_067 = AFCAL*AFCAL

      END FUNCTION FFCALC_067
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_069(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group I 1 2/a 1
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IL,3,N)-SINQS(IH,1,N)  &
                  *SINQS(IL,3,N))*COSQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - (SINQS(IH,1,N)*COSQS(IL,3,N)+COSQS(IH,1,N)  &
                  *SINQS(IL,3,N))*SINQS(IK,2,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_069 = AFCAL*AFCAL

      END FUNCTION FFCALC_069
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_112(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 21 21 2
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_112 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_112
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_115(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 21 21 21
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + SINQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_115 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_115
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_116(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group C 2 2 21
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + SINQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_116 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_116
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_143(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P c a 21
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + SINQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_143 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_143
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_164(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P n a 21
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + SINQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_164 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_164
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_176(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group C m c 21
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + COSQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_176 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_176
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_212(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group F d d 2
!
! Note the use here of a prefactor of '2' for logrefs of 1 and 3
! to take into account the double weighting of these SF expression
! as seen in Vol B
!
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + 2.*COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL + 2.*COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)  &
                  -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)          &
                  -COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)          &
                  -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N))         &
                  *FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)  &
                  -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)          &
                  +COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)          &
                  +SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N))         &
                  *FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - 2.*SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
          BFCAL = BFCAL - 2.*SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)  &
                  -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)          &
                  +COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)          &
                  +SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N))         &
                  *FOB(N,IR)
          BFCAL = BFCAL + (COSQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)  &
                  -SINQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)          &
                  -COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)          &
                  -SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N))         &
                  *FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_212 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_212
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_266(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P c c n
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_266 = AFCAL*AFCAL

      END FUNCTION FFCALC_266
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_269(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P b c m
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_269 = AFCAL*AFCAL

      END FUNCTION FFCALC_269
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_284(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P b c n
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_284 = AFCAL*AFCAL

      END FUNCTION FFCALC_284
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_290(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P b c a
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_290 = AFCAL*AFCAL

      END FUNCTION FFCALC_290
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_292(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P n m a
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*SINQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - SINQS(IH,1,N)*COSQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_292 = AFCAL*AFCAL

      END FUNCTION FFCALC_292
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_298(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group C m c m
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_298 = AFCAL*AFCAL

      END FUNCTION FFCALC_298
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_304(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group C m c a
! Loop is performed over the atoms in the asymmetric unit
! See get_logref.inc for a description of the LOGREF conditions
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          AFCAL = AFCAL + COSQS(IH,1,N)*COSQS(IK,2,N)*COSQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          AFCAL = AFCAL - COSQS(IH,1,N)*SINQS(IK,2,N)*SINQS(IL,3,N)*FOB(N,IR)
        ENDDO
      ENDIF
      FFCALC_304 = AFCAL*AFCAL

      END FUNCTION FFCALC_304
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_356(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group I-4
      AFCAL = 0.
      BFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        term1 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)*COSQS(IK,1,N)
        term2 = SINQS(IH,1,N)*SINQS(IK,2,N) - SINQS(IH,2,N)*SINQS(IK,1,N)
        term3 = COSQS(IL,3,N)
        term4 = (term1-term2)*term3
        AFCAL = AFCAL + term4*fob(n,ir)
        term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)*COSQS(IK,1,N)
        term2 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)*SINQS(IK,1,N)
        term3 = SINQS(IL,3,N)
        term4 = (term1-term2)*term3
        BFCAL = BFCAL + term4*fob(n,ir)
      ENDDO
      FFCALC_356 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_356
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_360(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 4/n (origin choice 2, inversion at origin)
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN     ! h = 2n,   k = 2n
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = 4.0*COSQS(IL,3,N)
          term3 = COSQS(IK,1,N)*COSQS(IH,2,N) + SINQS(IK,1,N)*SINQS(IH,2,N)
          term5 = term2*term1 + term2*term3
          AFCAL = AFCAL + term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN ! h = 2n,   k = 2n+1
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = 4.0*SINQS(IL,3,N)
          term3 = SINQS(IK,1,N)*COSQS(IH,2,N) - COSQS(IK,1,N)*SINQS(IH,2,N)
          term5 = -term2*term1 - term2*term3
          AFCAL = AFCAL + term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN ! h = 2n+1, k = 2n
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = 4.0*SINQS(IL,3,N)
          term3 = SINQS(IK,1,N)*COSQS(IH,2,N) - COSQS(IK,1,N)*SINQS(IH,2,N)
          term5 = -term2*term1 + term2*term3
          AFCAL = AFCAL + term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN ! h = 2n+1, k = 2n+1
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = 4.0*COSQS(IL,3,N)
          term3 = COSQS(IK,1,N)*COSQS(IH,2,N) + SINQS(IK,1,N)*SINQS(IH,2,N)
          term5 = term2*term1 - term2*term3
          AFCAL = AFCAL + term5*fob(N,iR)
        ENDDO
      ENDIF
      FFCALC_360 = AFCAL*AFCAL

      END FUNCTION FFCALC_360
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_362(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 42/n (origin choice 2, inversion at origin)
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN ! h+k = 2n,   h+l = 2n,   k+l = 2n
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = COSQS(IK,1,N)*COSQS(IH,2,N) + SINQS(IK,1,N)*SINQS(IH,2,N)
          term5 = term2*term1 + term2*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN ! h+k = 2n,   h+l = 2n,   k+l = 2n+1
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = SINQS(IK,1,N)*COSQS(IH,2,N) - COSQS(IK,1,N)*SINQS(IH,2,N)
          term4 = SINQS(IL,3,N)
          term5 = term2*term1 - term4*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN ! h+k = 2n,   h+l = 2n+1, k+l = 2n
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = SINQS(IK,1,N)*COSQS(IH,2,N) - COSQS(IK,1,N)*SINQS(IH,2,N)
          term4 = SINQS(IL,3,N)
          term5 = term2*term1 + term4*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN ! h+k = 2n,   h+l = 2n+1, k+l = 2n+1
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = COSQS(IK,1,N)*COSQS(IH,2,N) + SINQS(IK,1,N)*SINQS(IH,2,N)
          term5 = term2*term1 - term2*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(5,IR)) THEN ! h+k = 2n+1, h+l = 2n,   k+l = 2n
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = COSQS(IK,1,N)*COSQS(IH,2,N) + SINQS(IK,1,N)*SINQS(IH,2,N)
          term4 = COSQS(IL,3,N)
          term5 = -term2*term1 + term4*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(6,IR)) THEN ! h+k = 2n+1, h+l = 2n,   k+l = 2n+1
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = SINQS(IK,1,N)*COSQS(IH,2,N) - COSQS(IK,1,N)*SINQS(IH,2,N)
          term5 = -term2*term1 + term2*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(7,IR)) THEN ! h+k = 2n+1, h+l = 2n+1, k+l = 2n
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = SINQS(IK,1,N)*COSQS(IH,2,N) - COSQS(IK,1,N)*SINQS(IH,2,N)
          term5 = -term2*term1 - term2*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ELSEIF (LOGREF(8,IR)) THEN ! h+k = 2n+1, h+l = 2n+1, k+l = 2n+1
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = COSQS(IK,1,N)*COSQS(IH,2,N) + SINQS(IK,1,N)*SINQS(IH,2,N)
          term4 = COSQS(IL,3,N)
          term5 = -term2*term1 - term4*term3
          AFCAL = AFCAL + 4.0*term5*fob(N,iR)
        ENDDO
      ENDIF
      FFCALC_362 = AFCAL*AFCAL

      END FUNCTION FFCALC_362
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_365(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group I 41/a (origin choice 2)
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)*COSQS(IK,1,N)
          term2 = SINQS(IH,1,N)*SINQS(IK,2,N) - SINQS(IH,2,N)*SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          AFCAL = AFCAL + term4*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = COSQS(IH,2,N)*COSQS(IK,1,N) + SINQS(IH,2,N)*SINQS(IK,1,N)
          term4 = COSQS(IL,3,N)
          term5 = term1*term2 - term3*term4
          AFCAL = AFCAL - term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = SINQS(IH,2,N)*COSQS(IK,1,N) - COSQS(IH,2,N)*SINQS(IK,1,N)
          term4 = SINQS(IL,3,N)
          term5 = term1*term2 - term3*term4
          AFCAL = AFCAL + term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*SINQS(IK,2,N) - COSQS(IH,2,N)*SINQS(IK,1,N)
          term2 = SINQS(IH,1,N)*COSQS(IK,2,N) + SINQS(IH,2,N)*COSQS(IK,1,N)
          term3 = SINQS(IL,3,N)
          term4 = (term1+term2)*term3
          AFCAL = AFCAL - term4*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(5,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)*COSQS(IK,1,N)
          term2 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)*SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          AFCAL = AFCAL + term4*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(6,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
          term2 = SINQS(IL,3,N)
          term3 = COSQS(IH,2,N)*COSQS(IK,1,N) + SINQS(IH,2,N)*SINQS(IK,1,N)
          term4 = COSQS(IL,3,N)
          term5 = term1*term2 + term3*term4
          AFCAL = AFCAL - term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(7,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          term2 = COSQS(IL,3,N)
          term3 = SINQS(IH,2,N)*COSQS(IK,1,N) - COSQS(IH,2,N)*SINQS(IK,1,N)
          term4 = SINQS(IL,3,N)
          term5 = term1*term2 + term3*term4
          AFCAL = AFCAL + term5*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(8,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*SINQS(IK,2,N) + COSQS(IH,2,N)*SINQS(IK,1,N)
          term2 = SINQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,2,N)*COSQS(IK,1,N)
          term3 = SINQS(IL,3,N)
          term4 = (term1+term2)*term3
          AFCAL = AFCAL - term4*fob(n,ir)
        ENDDO
      ENDIF
      FFCALC_365 = AFCAL*AFCAL

      END FUNCTION FFCALC_365
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_369(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P 41 21 2
      AFCAL = 0.
      BFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)*COSQS(IK,1,N)
          term2 = COSQS(IL,3,N)
          term3 = 2.*term1*term2
          AFCAL = AFCAL + term3*fob(n,ir)
          term1 = SINQS(IH,1,N)*SINQS(IK,2,N) - SINQS(IH,2,N)*SINQS(IK,1,N)
          term2 = SINQS(IL,3,N)
          term3 = 2.*term1*term2
          BFCAL = BFCAL - term3*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(2,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + SINQS(IH,2,N)*COSQS(IK,1,N)
          term2 = COSQS(IH,1,N)*SINQS(IK,2,N) + COSQS(IH,2,N)*SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          term5 = COSQS(IH,1,N)*SINQS(IK,2,N) - COSQS(IH,2,N)*SINQS(IK,1,N)
          term6 = SINQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,2,N)*COSQS(IK,1,N)
          term7 = SINQS(IL,3,N)
          term8 = (term5+term6)*term7
          AFCAL = AFCAL + (term4-term8)*fob(n,ir)
          term4 = (term1+term2)*term3
          term8 = (term5-term6)*term7
          BFCAL = BFCAL + (term4+term8)*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(3,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)*SINQS(IK,1,N)
          term2 = COSQS(IL,3,N)
          term3 = 2.*term1*term2
          AFCAL = AFCAL - term3*fob(n,ir)
          term1 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)*COSQS(IK,1,N)
          term2 = SINQS(IL,3,N)
          term3 = 2.*term1*term2
          BFCAL = BFCAL + term3*fob(n,ir)
        ENDDO
      ELSEIF (LOGREF(4,IR)) THEN
        DO N = 1, NATOM
          term1 = SINQS(IH,1,N)*COSQS(IK,2,N) + SINQS(IH,2,N)*COSQS(IK,1,N)
          term2 = COSQS(IH,1,N)*SINQS(IK,2,N) + COSQS(IH,2,N)*SINQS(IK,1,N)
          term3 = COSQS(IL,3,N)
          term4 = (term1-term2)*term3
          term5 = COSQS(IH,1,N)*SINQS(IK,2,N) - COSQS(IH,2,N)*SINQS(IK,1,N)
          term6 = SINQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,2,N)*COSQS(IK,1,N)
          term7 = SINQS(IL,3,N)
          term8 = (term5+term6)*term7
          AFCAL = AFCAL - (term4+term8)*fob(N,IR)
          term4 = (term1+term2)*term3
          term8 = (term5-term6)*term7
          BFCAL = BFCAL + (term4-term8)*fob(N,IR)
        ENDDO
      ENDIF
      FFCALC_369 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_369
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_391(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P -4 21 c
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      IL = iHKL(3,IR)
      IF (LOGREF(1,IR)) THEN
        DO N = 1, NATOM
          term1 = COSQS(IL,3,N)
          term2 = COSQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,2,N)*COSQS(IK,1,N)
          AFCAL = AFCAL + 4.0*term1*term2*fob(N,IR)
          term1 = SINQS(IL,3,N)
          term2 = SINQS(IH,1,N)*SINQS(IK,2,N) + SINQS(IH,2,N)*SINQS(IK,1,N)
          BFCAL = BFCAL - 4.0*term1*term2*fob(N,IR)
        ENDDO
      ELSE
        DO N = 1, NATOM
          term1 = COSQS(IL,3,N)
          term2 = SINQS(IH,2,N)*SINQS(IK,1,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
          AFCAL = AFCAL + 4.0*term1*term2*fob(N,IR)
          term1 = SINQS(IL,3,N)
          term2 = COSQS(IH,1,N)*COSQS(IK,2,N) - COSQS(IH,2,N)*COSQS(IK,1,N)
          BFCAL = BFCAL + 4.0*term1*term2*fob(N,IR)
        ENDDO
      ENDIF
      FFCALC_391 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_391
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_430(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

!.. For P3
      AFCAL = 0.
      BFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        SHKI = SHKI1 + SHKI2 + SHKI3
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ-SHKI*SLZ)*FOB(N,IR)
        BFCAL = BFCAL + (CHKI*SLZ+SHKI*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_430 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_430
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_431(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

!.. For P31
      AFCAL = 0.
      BFCAL = 0.
      RH = 6.283185307179*FLOAT(iHKL(1,IR))
      RK = 6.283185307179*FLOAT(iHKL(2,IR))
      RI = -(RH+RK)
      RL = 6.283185307179*FLOAT(iHKL(3,IR))
      VL = RL/3.
      DO N = 1, NATOM
        XV = X(1,N)
        YV = X(2,N)
        RLZV = RL*X(3,N)
        ARG1 = RH*XV + RK*YV + RLZV
        ARG2 = RK*XV + RI*YV + RLZV + VL
        ARG3 = RI*XV + RH*YV + RLZV - VL
        AFCAL = AFCAL + (COS(ARG1)+COS(ARG2)+COS(ARG3))*FOB(N,IR)
        BFCAL = BFCAL + (SIN(ARG1)+SIN(ARG2)+SIN(ARG3))*FOB(N,IR)
      ENDDO
      FFCALC_431 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_431
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_432(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

!.. For P32
      AFCAL = 0.
      BFCAL = 0.
      RH = 6.283185307179*FLOAT(iHKL(1,IR))
      RK = 6.283185307179*FLOAT(iHKL(2,IR))
      RI = -(RH+RK)
      RL = 6.283185307179*FLOAT(iHKL(3,IR))
      VL = RL/3.
      DO N = 1, NATOM
        XV = X(1,N)
        YV = X(2,N)
        RLZV = RL*X(3,N)
        ARG1 = RH*XV + RK*YV + RLZV
        ARG2 = RK*XV + RI*YV + RLZV - VL
        ARG3 = RI*XV + RH*YV + RLZV + VL
        AFCAL = AFCAL + (COS(ARG1)+COS(ARG2)+COS(ARG3))*FOB(N,IR)
        BFCAL = BFCAL + (SIN(ARG1)+SIN(ARG2)+SIN(ARG3))*FOB(N,IR)
      ENDDO
      FFCALC_432 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_432
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_433(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group R3 hexagonal axes
      AFCAL = 0.
      BFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        SHKI = SHKI1 + SHKI2 + SHKI3
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ-SHKI*SLZ)*FOB(N,IR)
        BFCAL = BFCAL + (CHKI*SLZ+SHKI*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_433 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_433
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_434(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P-3
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        SHKI = SHKI1 + SHKI2 + SHKI3
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ-SHKI*SLZ)*FOB(N,IR)
      ENDDO
      FFCALC_434 = AFCAL*AFCAL

      END FUNCTION FFCALC_434
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_435(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group R-3 hexagonal axes
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        SHKI = SHKI1 + SHKI2 + SHKI3
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ-SHKI*SLZ)*FOB(N,IR)
      ENDDO
      FFCALC_435 = AFCAL*AFCAL

      END FUNCTION FFCALC_435
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_449(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P-31m
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI = SHKI1 + SHKI2 + SHKI3
        CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)*SINQS(IH,2,N)
        CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)*SINQS(II,2,N)
        CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)*SINQS(IK,2,N)
        SKHI1 = SINQS(IK,1,N)*COSQS(IH,2,N) + COSQS(IK,1,N)*SINQS(IH,2,N)
        SKHI2 = SINQS(IH,1,N)*COSQS(II,2,N) + COSQS(IH,1,N)*SINQS(II,2,N)
        SKHI3 = SINQS(II,1,N)*COSQS(IK,2,N) + COSQS(II,1,N)*SINQS(IK,2,N)
        CKHI = CKHI1 + CKHI2 + CKHI3
        SKHI = SKHI1 + SKHI2 + SKHI3
        RPHCC = CHKI + CKHI
        RPHSS = SHKI + SKHI
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (RPHCC*CLZ-RPHSS*SLZ)*FOB(N,IR)
      ENDDO
      FFCALC_449 = AFCAL*AFCAL

      END FUNCTION FFCALC_449
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_451(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P-3m1
      AFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI = SHKI1 + SHKI2 + SHKI3
        CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)*SINQS(IH,2,N)
        CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)*SINQS(II,2,N)
        CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)*SINQS(IK,2,N)
        SKHI1 = SINQS(IK,1,N)*COSQS(IH,2,N) + COSQS(IK,1,N)*SINQS(IH,2,N)
        SKHI2 = SINQS(IH,1,N)*COSQS(II,2,N) + COSQS(IH,1,N)*SINQS(II,2,N)
        SKHI3 = SINQS(II,1,N)*COSQS(IK,2,N) + COSQS(II,1,N)*SINQS(IK,2,N)
        CKHI = CKHI1 + CKHI2 + CKHI3
        SKHI = SKHI1 + SKHI2 + SKHI3
        RPHCC = CHKI + CKHI
        RMHSS = SHKI - SKHI
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (RPHCC*CLZ-RMHSS*SLZ)*FOB(N,IR)
      ENDDO
      FFCALC_451 = AFCAL*AFCAL

      END FUNCTION FFCALC_451
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_462(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P6
      AFCAL = 0.
      BFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ)*FOB(N,IR)
        BFCAL = BFCAL + (CHKI*SLZ)*FOB(N,IR)
      ENDDO
      FFCALC_462 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_462
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_468(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P-6
      AFCAL = 0.
      BFCAL = 0.
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        SHKI = SHKI1 + SHKI2 + SHKI3
        CLZ = COSQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ)*FOB(N,IR)
        BFCAL = BFCAL + (SHKI*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_468 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_468
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_469(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P6/m
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        CLZ = COSQS(IL,3,N)
        AFCAL = AFCAL + (CHKI*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_469 = AFCAL*AFCAL

      END FUNCTION FFCALC_469
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_471(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P622
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)*SINQS(IH,2,N)
        CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)*SINQS(II,2,N)
        CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)*SINQS(IK,2,N)
        CKHI = CKHI1 + CKHI2 + CKHI3
        RPHCC = CHKI + CKHI
        RMHCC = CHKI - CKHI
        CLZ = COSQS(IL,3,N)
        SLZ = SINQS(IL,3,N)
        AFCAL = AFCAL + (RPHCC*CLZ)*FOB(N,IR)
        BFCAL = BFCAL + (RMHCC*SLZ)*FOB(N,IR)
      ENDDO
      FFCALC_471 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_471
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_481(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P-6m2
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI = SHKI1 + SHKI2 + SHKI3
        CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)*SINQS(IH,2,N)
        CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)*SINQS(II,2,N)
        CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)*SINQS(IK,2,N)
        SKHI1 = SINQS(IK,1,N)*COSQS(IH,2,N) + COSQS(IK,1,N)*SINQS(IH,2,N)
        SKHI2 = SINQS(IH,1,N)*COSQS(II,2,N) + COSQS(IH,1,N)*SINQS(II,2,N)
        SKHI3 = SINQS(II,1,N)*COSQS(IK,2,N) + COSQS(II,1,N)*SINQS(IK,2,N)
        CKHI = CKHI1 + CKHI2 + CKHI3
        SKHI = SKHI1 + SKHI2 + SKHI3
        RPHCC = CHKI + CKHI
        RMHSS = SHKI - SKHI
        CLZ = COSQS(IL,3,N)
        AFCAL = AFCAL + (RPHCC*CLZ)*FOB(N,IR)
        BFCAL = BFCAL + (RMHSS*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_481 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_481
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_483(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P -6 2 m
      AFCAL = 0.0
      BFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        SHKI1 = SINQS(IH,1,N)*COSQS(IK,2,N) + COSQS(IH,1,N)*SINQS(IK,2,N)
        SHKI2 = SINQS(IK,1,N)*COSQS(II,2,N) + COSQS(IK,1,N)*SINQS(II,2,N)
        SHKI3 = SINQS(II,1,N)*COSQS(IH,2,N) + COSQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        SHKI = SHKI1 + SHKI2 + SHKI3
        CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)*SINQS(IH,2,N)
        CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)*SINQS(II,2,N)
        CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)*SINQS(IK,2,N)
        SKHI1 = SINQS(IK,1,N)*COSQS(IH,2,N) + COSQS(IK,1,N)*SINQS(IH,2,N)
        SKHI2 = SINQS(IH,1,N)*COSQS(II,2,N) + COSQS(IH,1,N)*SINQS(II,2,N)
        SKHI3 = SINQS(II,1,N)*COSQS(IK,2,N) + COSQS(II,1,N)*SINQS(IK,2,N)
        CKHI = CKHI1 + CKHI2 + CKHI3
        SKHI = SKHI1 + SKHI2 + SKHI3
        RPHCC = CHKI + CKHI
        RPHSS = SHKI + SKHI
        CLZ = COSQS(IL,3,N)
        AFCAL = AFCAL + (RPHCC*CLZ)*FOB(N,IR)
        BFCAL = BFCAL + (RPHSS*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_483 = AFCAL*AFCAL + BFCAL*BFCAL

      END FUNCTION FFCALC_483
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_485(IR)

      INCLUDE 'SGinc\FFCALCTOP.inc'

! Structure factor calculations for space group P6/mmm
      AFCAL = 0.0
      IH = iHKL(1,IR)
      IK = iHKL(2,IR)
      II = -(IH+IK)
      IL = iHKL(3,IR)
      DO N = 1, NATOM
        CHKI1 = COSQS(IH,1,N)*COSQS(IK,2,N) - SINQS(IH,1,N)*SINQS(IK,2,N)
        CHKI2 = COSQS(IK,1,N)*COSQS(II,2,N) - SINQS(IK,1,N)*SINQS(II,2,N)
        CHKI3 = COSQS(II,1,N)*COSQS(IH,2,N) - SINQS(II,1,N)*SINQS(IH,2,N)
        CHKI = CHKI1 + CHKI2 + CHKI3
        CKHI1 = COSQS(IK,1,N)*COSQS(IH,2,N) - SINQS(IK,1,N)*SINQS(IH,2,N)
        CKHI2 = COSQS(IH,1,N)*COSQS(II,2,N) - SINQS(IH,1,N)*SINQS(II,2,N)
        CKHI3 = COSQS(II,1,N)*COSQS(IK,2,N) - SINQS(II,1,N)*SINQS(IK,2,N)
        CKHI = CKHI1 + CKHI2 + CKHI3
        RPHCC = CHKI + CKHI
        CLZ = COSQS(IL,3,N)
        AFCAL = AFCAL + (RPHCC*CLZ)*FOB(N,IR)
      ENDDO
      FFCALC_485 = AFCAL*AFCAL

      END FUNCTION FFCALC_485
!
!*****************************************************************************
!
      REAL FUNCTION FFCALC_DEFAULT(IR)

      USE REFVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iR

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      REAL            FOB
      COMMON /FCSTOR/ FOB(150,MaxRef)

      INTEGER         NOP, NCENT, NOPC, NLAT, NGEN
      LOGICAL                                       CENTRC
      INTEGER                                               KOM13
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13

      REAL            sctrh,            rhsto
      COMMON /symsto/ sctrh(24,MaxRef), rhsto(3,24,MaxRef)

      INTEGER     NAC,       NBC
      REAL                                  FARCOS
      PARAMETER ( NAC = 256, NBC = 100*NAC, FARCOS = 256.0 )

      REAL            COSAR0,           COSAR1,           COSAR2
      COMMON /COSARS/ COSAR0(-NBC:NBC), COSAR1(-NBC:NBC), COSAR2(-NBC:NBC)

      REAL            SINAR0,           SINAR1,           SINAR2
      COMMON /SINARS/ SINAR0(-NBC:NBC), SINAR1(-NBC:NBC), SINAR2(-NBC:NBC)

      REAL AFCALC, BFCALC, SUMA, SUMB, V, PV
      INTEGER N, I, IV

      AFCALC = 0.0
! Firstly if we are centric then calculate only cosine terms
      IF (CENTRC) THEN
        DO N = 1, NATOM
          SUMA = 0.0
! SUM OVER SYMMETRY EQUIVALENTS:
          DO I = 1, NOPC
! V is 2pi*(h*x+t)
!C RHSTO holds the reciprocal space vector (h,k,l) per symmetry operator per reflection
!C (so in a way, it's the h,k,l that are transformed rather than the atomic co-ordinates)
            V = (X(1,N)*RHSTO(1,I,IR)+X(2,N)*RHSTO(2,I,IR)+X(3,N)*RHSTO(3,I,IR)+SCTRH(I,IR))*FARCOS
            IV = V
            PV = V - FLOAT(IV)
!C The COSARs are just a look-up table to speed up the goniometric calculations
            SUMA = SUMA + COSAR0(IV) + PV*(COSAR1(IV)+PV*COSAR2(IV))
          ENDDO
          AFCALC = AFCALC + SUMA*FOB(N,IR)
        ENDDO
        FFCALC_DEFAULT = AFCALC*AFCALC
      ELSE
! Deal with the non-centric case
        BFCALC = 0.0
        DO N = 1, NATOM
          SUMA = 0.0
          SUMB = 0.0
! SUM OVER SYMMETRY EQUIVALENTS:
          DO I = 1, NOPC
! V is 2pi*(h*x+t)  2*pi*((R*x+t)*h) = 2*pi*(x*(h*R+t))
            V = (X(1,N)*RHSTO(1,I,IR)+X(2,N)*RHSTO(2,I,IR)+X(3,N)*RHSTO(3,I,IR)+SCTRH(I,IR))*FARCOS
            IV = V
            PV = V - FLOAT(IV)
            SUMA = SUMA + COSAR0(IV) + PV*(COSAR1(IV)+PV*COSAR2(IV))
            SUMB = SUMB + SINAR0(IV) + PV*(SINAR1(IV)+PV*SINAR2(IV))
          ENDDO
          AFCALC = AFCALC + SUMA*FOB(N,IR)
          BFCALC = BFCALC + SUMB*FOB(N,IR)
        ENDDO
        FFCALC_DEFAULT = AFCALC*AFCALC + BFCALC*BFCALC
      ENDIF

      END FUNCTION FFCALC_DEFAULT
!
!*****************************************************************************
!
      SUBROUTINE CALCOSARX

      IMPLICIT NONE

      INTEGER     NAC,       NBC
      REAL                                  FARCOS
      PARAMETER ( NAC = 256, NBC = 100*NAC, FARCOS = 256.0 )

      REAL            COSAR0,           COSAR1,           COSAR2
      COMMON /COSARS/ COSAR0(-NBC:NBC), COSAR1(-NBC:NBC), COSAR2(-NBC:NBC)

      REAL            SINAR0,           SINAR1,           SINAR2
      COMMON /SINARS/ SINAR0(-NBC:NBC), SINAR1(-NBC:NBC), SINAR2(-NBC:NBC)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL    AMUL, X, XP, XM
      INTEGER I

      AMUL = TWOPI / FARCOS
      DO I = -NBC, NBC
        X  = AMUL * FLOAT(I)
        XP = AMUL * FLOAT(I+1)
        XM = AMUL * FLOAT(I-1)
        COSAR0(I) = COS(X)
        COSAR1(I) = 0.5 * (COS(XP) - COS(XM))
        COSAR2(I) = 0.5 * (COS(XP) + COS(XM) - 2.0 * COSAR0(I))
        SINAR0(I) = SIN(X)
        SINAR1(I) = 0.5 * (SIN(XP) - SIN(XM))
        SINAR2(I) = 0.5 * (SIN(XP) + SIN(XM) - 2.0 * SINAR0(I))
      ENDDO

      END SUBROUTINE CALCOSARX
!
!*****************************************************************************
!
