!
!*****************************************************************************
!
      SUBROUTINE RR_VALCHI(CHIVAL)

      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE VARIABLES
      USE REFVAR
      USE RRVAR

      IMPLICIT NONE

      REAL,    INTENT (  OUT) :: CHIVAL

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      INTEGER         KKOR
      REAL                  WTIJ
      INTEGER                             IKKOR,         JKKOR
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), IKKOR(MCHIHS), JKKOR(MCHIHS)

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
     &                KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
     &                SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)
      
      INTEGER         NStPar
      COMMON /pextra/ NStPar

      REAL, EXTERNAL :: FFCALC_001, FFCALC_002, FFCALC_039, FFCALC_040, FFCALC_044, FFCALC_050, &
                        FFCALC_052, FFCALC_057, FFCALC_058, FFCALC_061, FFCALC_062, FFCALC_064, FFCALC_065
      REAL, EXTERNAL :: FFCALC_066, FFCALC_067, FFCALC_069, FFCALC_112, FFCALC_115, FFCALC_116, &
                        FFCALC_143, FFCALC_164, FFCALC_176, FFCALC_212, FFCALC_266, FFCALC_269
      REAL, EXTERNAL :: FFCALC_284, FFCALC_290, FFCALC_292, FFCALC_298, FFCALC_304, FFCALC_356, &
                        FFCALC_360, FFCALC_362, FFCALC_365, FFCALC_369, FFCALC_391, FFCALC_430
      REAL, EXTERNAL :: FFCALC_431, FFCALC_432, FFCALC_433, FFCALC_434, FFCALC_435, FFCALC_449, &
                        FFCALC_451, FFCALC_462, FFCALC_468, FFCALC_469, FFCALC_471, FFCALC_481, &
                        FFCALC_483, FFCALC_485, FFCALC_DEFAULT
      REAL    SUM1, SUM2, RESCL, DELI, DELJ, CHIADD
      INTEGER iR, iK, II, JJ
     
      CALL PRECFC
      IF (LOG_HYDROGENS) THEN
        NATOM = TotNumOfAtoms
      ELSE
        NATOM = NumOfNonHydrogens
      ENDIF
! Update the global isotropic temperature factors if needed.
      IF (RR_ioptITF .EQ. 1) THEN
        CALL CreateFobITF
      ENDIF
      SELECT CASE (NumberSGTable)
        CASE (1)             ! P1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_001(IR)
          ENDDO
        CASE (2)             ! P-1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_002(IR)
          ENDDO
        CASE (39)            ! P 1 21 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_039(IR)
          ENDDO
        CASE (40)             ! C 1 2 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_040(IR)
          ENDDO
        CASE (44)             ! P 1 c 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_044(IR)
          ENDDO
        CASE (50)            ! C 1 c 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_050(IR)
          ENDDO
        CASE (52)            ! I 1 a 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_052(IR)
          ENDDO
        CASE (57)            ! P 1 21/m 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_057(IR)
          ENDDO
        CASE (58)            ! C 1 2/m 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_058(IR)
          ENDDO
        CASE (61)            ! P 1 2/c 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_061(IR)   
          ENDDO
        CASE (62)            ! P 1 2/n 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_062(IR)   
          ENDDO
        CASE (64)            ! P 1 21/c 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_064(IR)   
          ENDDO
        CASE (65)            ! P 1 21/n 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_065(IR)
          ENDDO
        CASE (66)            ! P 1 21/a 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_066(IR)
          ENDDO
        CASE (67)            ! C 1 2/c 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_067(IR)
          ENDDO
        CASE (69)            ! I 1 2/a 1
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_069(IR)
          ENDDO
        CASE (112)           ! P 21 21 2
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_112(IR)
          ENDDO
        CASE (115)           ! P 21 21 21
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_115(IR)
          ENDDO
        CASE (116)           ! C 2 2 21
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_116(IR)
          ENDDO
        CASE (143)           ! P c a 21
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_143(IR)
          ENDDO
        CASE (164)           ! P n a 21
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_164(IR)
          ENDDO
        CASE (176)            ! C m c 21
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_176(IR)
          ENDDO
        CASE (212)            ! F d d 2
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_212(IR)
          ENDDO
        CASE (266)            ! P c c n
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_266(IR)
          ENDDO
        CASE (269)            ! P b c m
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_269(IR)
          ENDDO
        CASE (284)           ! P b c n
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_284(IR)
          ENDDO
        CASE (290)           ! P b c a
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_290(IR)
          ENDDO
        CASE (292)           ! P n m a
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_292(IR)
          ENDDO
        CASE (298)           ! C m c m
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_298(IR)
          ENDDO
        CASE (304)           ! C m c a
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_304(IR)
          ENDDO
        CASE (356)           ! I -4
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_356(IR)
          ENDDO
        CASE (360)           ! P 4/n (origin choice 2)
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_360(IR)
          ENDDO
        CASE (362)           ! P 42/n (origin choice 2)
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_362(IR)
          ENDDO
        CASE (365)           ! I 41/a (origin choice 2)
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_365(IR)
          ENDDO
        CASE (369)           ! P 41 21 2
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_369(IR)
          ENDDO
        CASE (391)           ! P -4 21 c
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_391(IR)
          ENDDO
  ! Obscure from here on in !
        CASE (430)           ! P3
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_430(IR)
          ENDDO
        CASE (431)           ! P31
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_431(IR)
          ENDDO
        CASE (432)           ! P32
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_432(IR)
          ENDDO
        CASE (433)           ! R3 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_433(IR)
          ENDDO
        CASE (434)           ! P-3
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_434(IR)
          ENDDO
        CASE (435)           ! R-3 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_435(IR)
          ENDDO
        CASE (449)           ! P-31m hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_449(IR)
          ENDDO
        CASE (451)           ! P-3m1 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_451(IR)
          ENDDO
        CASE (462)           ! P6 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_462(IR)
          ENDDO
        CASE(468)           ! P-6 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_468(IR)
          ENDDO
        CASE (469)           ! P6/m hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_469(IR)
          ENDDO
        CASE (471)           ! P622 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_471(IR)
          ENDDO
        CASE (481)           ! P-6m2 hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_481(IR)
          ENDDO
        CASE (483)           ! P-62m hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_483(IR)
          ENDDO
        CASE (485)           ! P6/mmm hexagonal axes
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_485(IR)
          ENDDO
        CASE DEFAULT
          DO IR = 1, NumOfRef
            AICALC(IR) = FFCALC_DEFAULT(IR)
          ENDDO
      END SELECT
      NATOM = TotNumOfAtoms
! recalculate the preferred orientation correction factors.
! Fill Preferred Orientation part
      IF (PrefParExists .AND. RR_ioptPO) CALL PO_PRECFC(RR_PO)
! AICALC(1:NumOfRef) now contains the structural part of the calculated intensities
! XICALC(1:NumOfRef) now contains the preferred orientation part of the calculated intensities
! If we are using preferred orientation: correct for it.
! If not: use the calculated intensities as is
      IF (PrefParExists) THEN
        DO iR = 1, NumOfRef
          BICALC(iR) = XICALC(iR) * AICALC(iR)
        ENDDO
      ELSE
        DO iR = 1, NumOfRef
          BICALC(iR) = AICALC(iR)
        ENDDO
      ENDIF
! BICALC(1:NumOfRef) now contains the calculated intensities corrected for preferred orientation
      SUM1 = 0.0
      SUM2 = 0.0
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        SUM1 = SUM1 + BICALC(II)*WTIJ(IK)*AIOBS(JJ) + BICALC(JJ)*WTIJ(IK)*AIOBS(II)
        SUM2 = SUM2 + BICALC(II)*WTIJ(IK)*BICALC(JJ)
      ENDDO
      RESCL = 0.5*SUM1/SUM2
      CHIVAL = 0.0
      DO IK = 1, KKOR
        II = IKKOR(IK)
        JJ = JKKOR(IK)
        DELI = AIOBS(II) - RESCL*BICALC(II)
        DELJ = AIOBS(JJ) - RESCL*BICALC(JJ)
        CHIADD = DELI*WTIJ(IK)*DELJ
        CHIVAL = CHIVAL + CHIADD
      ENDDO
      CHIVAL = CHIVAL/FLOAT(NumOfRef-2)

      END SUBROUTINE RR_VALCHI
!
!*****************************************************************************
!
      REAL FUNCTION RR_FCN(N,P)

      USE RRVAR

      IMPLICIT NONE

      INTEGER N
      REAL P(*)

      INTEGER i
      REAL snglh

      DO I = 1, RR_MVAR
        RR_Params(i) = P(i)
      ENDDO
      CALL Params2RRVAR
      CALL RR_makefrac
      CALL RR_valchi(snglh)
      RR_FCN = snglh

      END FUNCTION RR_FCN
!
!*****************************************************************************
!
      SUBROUTINE RR_SIMOPT(X,DX,N,TheChiSqd)
!
! Purpose
!     A SIMPLEX-based routine which optimises the values of the N
! parameters pertaining to the components of the vector X
!
! Parameters
!   ARGUMENT  TYPE  I/O  DIMENSION  DESCRIPTION
!    N        I*4    I       -      No. of parameters to be optimised.
!    X        R*4    I       N      Initial guess.
!    X        R*4    O       N      The optimal answer.
!    DX       R*4    I       N      Initial step-lengths for X.
!
! Other Requirements
!     The user must provide a FUNCTION RR_FCN(N,X) which evalutes
! Chi-squared (un-normalised) given the vector X(N).
!
! History
!     D. S. Sivia    9 Feb 1995  Initial release.
!
      IMPLICIT NONE

  !    INCLUDE 'PARAMS.INC'

      REAL, INTENT (  OUT) :: TheChiSqd

      INTEGER     RR_MVAR
      PARAMETER ( RR_MVAR = 200 )
      INTEGER     MAXITR
      PARAMETER ( MAXITR = RR_MVAR )
      REAL X(*), DX(*)
      REAL V((RR_MVAR+1)*MAXITR), EX(3*RR_MVAR), C(RR_MVAR+1)
      INTEGER IR(RR_MVAR+1) ! Maximum index I could find: N+1. Maximum value for N should be RR_MVAR
                         ! IR seems to hold a sorted list of pointers into C
      INTEGER N, ITER
      REAL CHI, CHIMIN, ZERO
      REAL, EXTERNAL :: RR_FCN

      CALL VCOPY(X,V,N)
      CHIMIN = RR_FCN(N,V)
      ZERO = 0.0
      ITER = 0
    1 ITER = ITER + 1
      IF (ITER.GT.MAXITR) THEN
        CALL DebugErrorMessage('ITER.GT.MAXITR in RR_SIMOPT')
        GOTO 999
      ENDIF
      CALL RRSIMPLEX(V,N,DX,EX,C,IR,N*1000)
      CALL VCOPY(EX,V,N)
      CHI = RR_FCN(N,V)
! Convergence criterion
      IF (CHI .GT. CHIMIN) THEN
        CALL DebugErrorMessage('CHI .GT. CHIMIN in RR_SIMOPT')
        GOTO 1
      ENDIF
      IF ((1.0-CHI/CHIMIN) .GT. 0.000001) THEN
        CHIMIN = CHI
        GOTO 1
      ENDIF
  999 CALL VCOPY(V,X,N)
      TheChiSqd = CHI

      END SUBROUTINE RR_SIMOPT
!
!*****************************************************************************
!
      SUBROUTINE RRSIMPLEX(V,N,D,EX,C,IR,MX)

      REAL V(N,*), EX(N,*), C(*), D(*)
      INTEGER IR(*)
      LOGICAL SLOW
      DATA ALPHA, BETA, GAMA/1.0, 0.5, 2.0/

      SLOW = .FALSE.
      N3 = 3*N
      CAIM = 0.0
      C(1) = RR_FCN(N,V(1,1))
      CALL RRSIMP0(V,D,C,IR,N)
      CMIN = C(IR(N+1))
      ITER = 0
      NOLUCK = 0
      IRSTRT = 0
   10 ITER = ITER + 1
      NOLUCK = NOLUCK + 1
      IF (ITER.GE.MX .OR. NOLUCK.GT.100) THEN
        CALL SSORT(C,IR,N)
        CALL VCOPY(V(1,IR(N+1)),EX,N)
        IF (ITER.LT.250) THEN
          NOLUCK = 0
          DO ID = 1, N
!O            D(ID) = D(ID)/10.0
            D(ID) = D(ID)/2.0
          ENDDO
          CALL VCOPY(EX,V,N)
          C(1) = RR_FCN(N,V(1,1))
          CALL RRSIMP0(V,DSMALL,C,IR,N)
          CMIN = C(IR(N+1))
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL VRFILL(EX,0.,N3)
      CALL XCENT(V,EX(1,3),IR(1),N)
      CALL XZERO(V(1,IR(1)),EX,EX(1,3),N,ALPHA)
      C0 = RR_FCN(N,EX)
      CL = C(IR(N+1))
      CH = C(IR(1))
      CS = C(IR(2))
      IF (C0.LT.CL) THEN
        CALL RREXPAND(CL,EX,EX(1,2),EX(1,3),N,C0,GAMA)
      ELSEIF (C0.GT.CS) THEN
        CALL RRCONTRACT(CH,CS,C0,C00,EX,EX(1,2),EX(1,3),V(1,IR(1)),BETA,N)
        IF (C00.LT.CH .AND. C00.LT.C0) THEN
          CALL VCOPY(EX(1,2),EX,N)
          C0 = C00
        ELSE
          CALL RRCONT2(IR(N+1),V,C,N,IR)
          IRSTRT = IRSTRT + 1
          IF (C(IR(N+1)).LT.CMIN) CMIN = C(IR(N+1))
          IF (IRSTRT.GE.5) THEN
            NOLUCK = 0
            CALL SSORT(C,IR,N)
            CALL VCOPY(V(1,IR(N+1)),EX,N)
            RETURN
          ENDIF
        ENDIF
      ENDIF
      CALL VCOPY(EX,V(1,IR(1)),N)
      C(IR(1)) = C0
      CALL SSORT(C,IR,N)
      IF (C(IR(N+1)).LT.CMIN) THEN
        DROP = (CMIN-C(IR(N+1)))/CMIN
        IF (ABS(DROP).LT.1.0E-4) SLOW = .TRUE.
        NOLUCK = 0
        CMIN = C(IR(N+1))
      ENDIF
      IF (CMIN.GT.CAIM .AND. .NOT.SLOW) GOTO 10
      CALL VCOPY(V(1,IR(N+1)),EX,N)

      END SUBROUTINE RRSIMPLEX
!
!*****************************************************************************
!
      SUBROUTINE RRSIMP0(V,D,C,IR,N)

      REAL V(N,*), C(*), D(*)
      INTEGER IR(*)

      DO I = 2, N + 1
        CALL VCOPY(V,V(1,I),N)
        V(I-1,I) = V(I-1,I) + D(I-1)
        C(I) = RR_FCN(N,V(1,I))
      ENDDO
      CALL SSORT(C,IR,N)

      END SUBROUTINE RRSIMP0
!
!*****************************************************************************
!
      SUBROUTINE RREXPAND(CL,X0,X00,XC,N,C0,G)

      REAL X0(*), X00(*), XC(*)

      DO I = 1, N
        X00(I) = G*(X0(I)-XC(I)) + XC(I)
      ENDDO
      C00 = RR_FCN(N,X00)
      IF (C00.LT.CL) THEN
        CALL VCOPY(X00,X0,N)
        C0 = C00
      ENDIF

      END SUBROUTINE RREXPAND
!
!*****************************************************************************
!
      SUBROUTINE RRCONTRACT(CH,CS,C0,C00,X0,X00,XC,XH,B,N)

      REAL X0(*), X00(*), XC(*), XH(*)

      IF (C0.LT.CH) THEN
        DO I = 1, N
          X00(I) = B*(X0(I)-XC(I)) + XC(I)
        ENDDO
      ELSE
        DO I = 1, N
          X00(I) = B*(XH(I)-XC(I)) + XC(I)
        ENDDO
      ENDIF
      C00 = RR_FCN(N,X00)

      END SUBROUTINE RRCONTRACT
!
!*****************************************************************************
!
      SUBROUTINE RRCONT2(K,V,C,N,IR)

      REAL V(N,*), C(*)
      INTEGER IR(*)

      DO J = 1, N + 1
        IF (J.EQ.K) GOTO 20
        DO I = 1, N
          V(I,J) = 0.5*(V(I,J)+V(I,K))
          C(J) = RR_FCN(N,V(1,J))
        ENDDO
   20 ENDDO
      CALL SSORT(C,IR,N)

      END SUBROUTINE RRCONT2
!
!*****************************************************************************
!
