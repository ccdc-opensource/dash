! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      REAL FUNCTION MULTIPEAK_CHISQ(NPAR,P)

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER, INTENT (IN   ) :: NPAR
      REAL,    INTENT (IN   ) :: P(MVAR)

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON

      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

      REAL              PKCONV
      COMMON /WWPRSAVZ/ PKCONV(2048,9)

      INCLUDE 'Reflns.inc'

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MOBS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XMINT

      INTEGER         MN, MN2
      COMMON /ZSTOR2/ MN, MN2

      INTEGER     MPeak
      PARAMETER ( MPeak = 20 )

      INTEGER         NPEAK
      REAL                   AREA,        XPOS       , P2
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), P2(MVAR)

      LOGICAL         LERANL
      COMMON /PKCOM3/ LERANL

!F      REAL              XPF_Range
!F      LOGICAL                                       RangeFitYN
!F      INTEGER           IPF_Lo,                     IPF_Hi
!F      INTEGER           NumPeakFitRange,            CurrentRange
!F      INTEGER           IPF_Range
!F      INTEGER           NumInPFR
!F      REAL              XPF_Pos,                    YPF_Pos
!F      INTEGER           IPF_RPt
!F      REAL              XPeakFit,                   YPeakFit
!F      REAL              PF_FWHM
!F      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
!F                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
!F                        NumPeakFitRange,            CurrentRange,                &
!F                        IPF_Range(MAX_NPFR),                                     &
!F                        NumInPFR(MAX_NPFR),                                      & 
!F                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
!F                        IPF_RPt(MAX_NPFR),                                       &
!F                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
!F                        PF_FWHM(MAX_NPFR)

      INTEGER I, II, III, JJ, JARGI, KK, IV, NPEAK2, IARGI
      REAL    DTARG, CCHI, TARGI, YBACK, POFF, CHIADD, PTEM
      REAL    C3FN(3)

!F      IPF_RPt(1) = 0
!F      DO II = 1, NumPeakFitRange
!F        IPF_RPt(II+1) = IPF_RPt(II) + IPF_Range(II)
!F      ENDDO
! Profile refinement stage:
! Single peak-fitting code
      DO I = 1, NPEAK
        KK = 4 + 2*I
        AREA(I) = P(KK+1)
        ZARGK(I) = P(KK+2)
      ENDDO
      DO IV = 1, NPKGEN(1,1) !JPHASE,JSOURC)
        PKFNVA(IV) = P(IV + 2)
      ENDDO
! FFT CALCULATION STAGE IN PROFILE REFINEMENT
! THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
! DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
      NPEAK2 = 1 + NPEAK/2
      ARGK = ZARGK(NPEAK2)
      CALL PF_FCSUB3(MN)
! FFT OVER
      CCHI = 0.0
      DO II = IIMIN, IIMAX  ! Loop over all points in this hatched area
        TARGI = ZARGI(II)   ! 2 theta value of this point
        YBACK = P(1) + P(2)*(TARGI-XMINT)
        ZCAL(II) = YBACK
        DO JJ = 1, NPEAK
          ARGK = ZARGK(JJ)    ! 2 theta value of the position of this peak
          DTARG = TARGI - ARGK
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
          JARGI = NINT(DTARG/ZXDELT)
          IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
          POFF = DTARG/ZXDELT - FLOAT(JARGI)
! WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS
          C3FN(1) = 0.5*POFF*(POFF-1.0)
          C3FN(2) = 1.0 - (POFF**2)
          C3FN(3) = 0.5*POFF*(POFF+1.0)
          YNORM = 0.0
          DO I = 1, 3
            III = IARGI + I - 2
            IF (III .LT. 1 .OR. III .GT. 2048) THEN
              CALL DebugErrorMessage('III .LT. 1 .OR. III .GT. 2048')
            ELSE
              YNORM = YNORM + C3FN(I)*PKCONV(III,1)
            ENDIF
          ENDDO
          ZCAL(II) = ZCAL(II) + AREA(JJ)*YNORM
        ENDDO
        CHIADD = (ZOBS(II)-ZCAL(II))/ZDOBS(II)
!F        YPeakFit(IPF_RPt(CurrentRange) + II) = ZCAL(II)
        CCHI = CCHI + CHIADD*CHIADD
      ENDDO  !  II LOOP
!F      DO I = 1, NPTS
!F        II = IPF_RPt(CurrentRange) + I
!F        XPeakFit(II) = ZARGI(I)
!F        YPeakFit(II) = ZCAL(I)
!F      ENDDO
!F      RangeFitYN(CurrentRange) = .TRUE.
!F      CALL Plot_PeakFit_Info
! Penalise against any negative peak widths
      IF (.NOT.LERANL) THEN
        DO IV = 1, NPKGEN(1,1)
          PTEM = 100.*PKFNVA(IV)
          IF (PTEM.LT.0.0) CCHI = CCHI + PTEM*PTEM
        ENDDO
      ENDIF
      MULTIPEAK_CHISQ = CCHI

      END FUNCTION MULTIPEAK_CHISQ
!
!*****************************************************************************
!
      REAL FUNCTION PKFUNC(X)

      IMPLICIT NONE

      INCLUDE 'params.inc'

      REAL, INTENT (IN   ) :: X         ! Position (in degrees 2 theta) at which to calculate YNORM

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON

      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

      REAL              PKCONV
      COMMON /WWPRSAVZ/ PKCONV(2048,9)

      INCLUDE 'Reflns.inc'

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XMINT

      INTEGER         MN, MN2
      COMMON /ZSTOR2/ MN, MN2

      INTEGER     MPeak
      PARAMETER ( MPeak = 20 )

      INTEGER         NPEAK
      REAL                   AREA,        XPOS       , P2
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), P2(MVAR)

      INTEGER I, III, JARGI, IV, IARGI
      REAL    DTARG, POFF
      REAL    C3FN(3)

! Profile refinement stage:
! Single peak-fitting code
      DTARG = X - P2(8) ! 2 theta value of this point - 2 theta value of the position of this peak
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
      JARGI = NINT(DTARG/ZXDELT)
      IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = DTARG/ZXDELT - FLOAT(JARGI)
! WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS
      C3FN(1) = 0.5*POFF*(POFF-1.0)
      C3FN(2) = 1.0 - (POFF**2)
      C3FN(3) = 0.5*POFF*(POFF+1.0)
      YNORM = 0.0
      DO I = 1, 3
        III = IARGI + I - 2
        IF (III .LT. 1 .OR. III .GT. 2048) THEN
          CALL DebugErrorMessage('III .LT. 1 .OR. III .GT. 2048')
        ELSE
          YNORM = YNORM + C3FN(I)*PKCONV(III,1)
        ENDIF
      ENDDO
      PKFUNC = YNORM

      END FUNCTION PKFUNC
!
!*****************************************************************************
!
      SUBROUTINE PF_FCSUB3(MNS)
!
!X   For use with PFCN03 constant wavelength data with finite detector height
!C 19B
!H
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: MNS

      INCLUDE 'params.inc'
      INCLUDE 'Reflns.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON

      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

      REAL              PKCONV
      COMMON /WWPRSAVZ/ PKCONV(2048,9)

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      LOGICAL         LERANL
      COMMON /PKCOM3/ LERANL

      LOGICAL NEAR90
      REAL SIG, GAM, HPS, HMS, DENTEM, C2TEM, CTEM, GTEM, CLTEM, TANRA, DENASY, BET1, BET2, BETM, BETP2
      REAL BETP, BETM2, BETPI, BETPK, ARG, AFII, SII, VAL, SVAL, ARGP1, ARGM1, FRCP, FRSP, FRCM, FRSM
      REAL SINP, SINM, COSP, COSM, XTEM
      INTEGER MN2, MN2M1, MN2P1, I, II, J, INV
      REAL  FR(2048,8), FI(2048,8), FRT(2048), FIT(2048)
      COMPLEX CFFT(8), CFF

      IF (LERANL) THEN
        SIG = ABS(PKFNVA(1))
        GAM = ABS(PKFNVA(2))
        HPS = ABS(PKFNVA(3))
        HMS = ABS(PKFNVA(4))
      ELSE
        SIG = PKFNVA(1)
        GAM = PKFNVA(2)
        HPS = PKFNVA(3)
        HMS = PKFNVA(4)
      ENDIF
      DENTEM = (FLOAT(MNS)*ZXDEL(KNOW))
      C2TEM  = PI / DENTEM
      CTEM   = 2.0 * C2TEM
      GTEM   = CTEM * SIG   ! Gaussian
      CLTEM  = C2TEM * GAM  ! Lorentzian
! TO DEAL WITH (A) 90 DEGREES AND (B) ABOVE ALL WE WILL DO IS
! (A) SET FR(I,3)=1 AND ALL ELSE TO ZERO AND
! (B) SWITCH THE SIGN OF THE IMAGINARY COMPONENTS
      NEAR90 = (ABS(ARGK-90.0).LT.2.0)
      IF (.NOT. NEAR90) THEN
        TANRA = ABS(TAN(RAD*ARGK))
        DENASY = 0.5*(HPS-HMS)*(HPS+HMS)
! BET1 AND BETPI CHANGE SIGN AT 90 DEGREES
! BET2, BETP, BETM, BETP2 AND BETM2 DO NOT
        BET1 = 0.5*RAD*DENTEM*TANRA
        BET2 = SQRT(BET1)
        BETP = HPS/BET2
        BETM = HMS/BET2
        BETP2 = PIBY2*BETP*BETP
        BETM2 = PIBY2*BETM*BETM
        BETPI = BET1/PI
      ENDIF
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
! GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,1) = EXP(-0.5*ARG*ARG)
        FI(I,1) = 0.0
! LORENTZIAN
        AFII = ABS(FLOAT(II))
!F        ARG = CLTEM*AFII
        ARG = MAX(0.0,CLTEM*AFII)
        FR(I,2) = EXP(-ARG)
        FI(I,2) = 0.0
! ASYMMETRY FUNCTION FOR UMBRELLA EFFECT
        IF (II.EQ.0 .OR. NEAR90) THEN
          FR(I,3) = 1.0
          FI(I,3) = 0.0
        ELSE
          SII = SQRT(AFII)
          VAL = FLOAT(II)
          SVAL = SII/VAL
          ARGP1 = BETP*SII
          ARGM1 = BETM*SII
          CALL FRENEL(ARGP1,FRCP,FRSP)
          CALL FRENEL(ARGM1,FRCM,FRSM)
          FRCP = FRCP*BET2/SII
          FRSP = FRSP*BET2*SVAL
          FRCM = FRCM*BET2/SII
          FRSM = FRSM*BET2*SVAL
          BETPK = BETPI/VAL
          SINP = SIN(BETP2*VAL)
          SINM = SIN(BETM2*VAL)
          COSP = COS(BETP2*VAL)
          COSM = COS(BETM2*VAL)
! BET1 AND BETPI CHANGE SIGN AT 90 DEGREES
! BET2, BETP, BETM, BETP2 AND BETM2 DO NOT
          FR(I,3) =  ((HPS*FRCP-HMS*FRCM)-BETPK*(SINP-SINM))/DENASY
          FI(I,3) = -((HPS*FRSP-HMS*FRSM)+BETPK*(COSP-COSM))/DENASY
          IF (ARGK .GT. 90.0) THEN
            FI(I,3) = -FI(I,3)
          ENDIF
        ENDIF
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 1, 3 !NPKGEN(JPHASE,JSOURC)
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
        ENDDO
        CFF = CFFT(1)*CFFT(2)*CFFT(3)
        FRT(I) = REAL(CFF)
        FIT(I) = AIMAG(CFF)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION
      INV = 1
      CALL FT01A(MNS,INV,FRT,FIT)
      XTEM = 1./ZXDEL(KNOW)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKCONV(II,1) = FRT(I)*XTEM
      ENDDO

      END SUBROUTINE PF_FCSUB3
!
!*****************************************************************************
!
