!*==MULTIPEAK_CHISQ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
      REAL FUNCTION MULTIPEAK_CHISQ(NPAR,P)
!
      INCLUDE 'PARAMS.INC'
!
      PARAMETER (NW=4)
      DIMENSION C3FN(3)
      COMMON /WWCARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9),     &
     &                  ICDN(26,9), IERR, IO10, SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
      COMMON /WWPHASE/ NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),       &
     &                 SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /WWPRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),       &
     &                  LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),    &
     &                  NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
!
      COMMON /WWPRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),  &
     &                  DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),       &
     &                  ARGSTP(5), PCON
!
      COMMON /WWPRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),  &
     &                  DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),       &
     &                  NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ,      &
     &                  REFUSE, CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,  &
     &                  NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /WWPRSAVZ/ PKCONV(2048,9)
!
      COMMON /WWREFLNS/ REFH(3,WWREFDIM), AMUL(WWREFDIM),               &
     &                  AICALC(WWREFDIM), AIOBS(WWREFDIM),              &
     &                  ESDOBS(WWREFDIM), SOMEGA(WWREFDIM), GGCALC(300),&
     &                  MAXKK(9), KMIN, KMAX, KMOD, KNOW,               &
     &                  DSTAR(WWREFDIM), ISMAG(WWREFDIM), DKDDS, KOM23
!
      EQUIVALENCE (MAXK,MAXKK(1))
      COMMON /WWSOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),   &
     &                  NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),    &
     &                  NPCSOU(9,5)
!
!.. Note only 3 phases specifically hardwired here
      COMMON /WWREFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
!
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS),    &
     &                ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)
      REAL            ZCAL !,        ZBAK
      COMMON /YSTORE/ ZCAL(MPPTS) !, ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XDIFT, XMINT
      COMMON /ZSTOR2/ MN, MN2
!
      PARAMETER (MPAR=50)
      REAL P(MPAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT, X(MPT), Y(MPT), E(MPT)
      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)
!
      LOGICAL LERANL
      COMMON /PKCOM3/ LERANL
!.. Profile refinement stage:
!.. Single peak-fitting code
!
      DO I = 1, NPEAK
        KK = 4 + 2*I
        AREA(I) = P(KK+1)
        ZARGK(I) = P(KK+2)
      ENDDO
      DO IV = 1, NPKGEN(1,1)
                         !JPHASE,JSOURC)
        IVV = IV + 2
        PKFNVA(IV) = P(IVV)
      ENDDO
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
      NPEAK2 = 1 + NPEAK/2
      ARGK = ZARGK(NPEAK2)
      CALL PF_FCSUB3(MN)
!.. FFT OVER
!.. FIND THE PEAK MAXIMUM VALUE AND THEN WORK OUT THE PEAK LIMITS
      CCHI = 0.
      DO II = IIMIN, IIMAX
        TARGI = ZARGI(II)
        YBACK = P(1) + P(2)*(TARGI-XMINT) !/XDIFT
  !      ZBAK(II) = YBACK
        ZCAL(II) = YBACK
        DO JJ = 1, NPEAK
          ARGK = ZARGK(JJ)
          DTARG = TARGI - ARGK
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
          JARGI = NINT(DTARG/ZXDELT)
          IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
          POFF = DTARG/ZXDELT - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS
          C3FN(1) = 0.5*POFF*(POFF-1.0)
          C3FN(2) = 1.0 - POFF**2
          C3FN(3) = 0.5*POFF*(POFF+1.0)
          YNORM = 0.0
          DO I = 1, 3
            III = IARGI + I - 2
            PKTEM = PKCONV(III,1)
            YNORM = YNORM + C3FN(I)*PKTEM
          ENDDO
          YCALC = AREA(JJ)*YNORM + YBACK
          ZCAL(II) = ZCAL(II) + YCALC
        ENDDO
        CHIADD = (ZOBS(II)-ZCAL(II))/ZDOBS(II)
        CCHI = CCHI + CHIADD*CHIADD
      ENDDO  !  II LOOP
!
!.. Penalise against any negative peak widths
      IF (.NOT.LERANL) THEN
        DO IV = 1, NPKGEN(1,1)
          PTEM = 100.*PKFNVA(IV)
          IF (PTEM.LT.0.0) CCHI = CCHI + PTEM*PTEM
        ENDDO
      ENDIF
      MULTIPEAK_CHISQ = CCHI
!
      END FUNCTION MULTIPEAK_CHISQ
!*==PF_FCSUB3.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
!
      SUBROUTINE PF_FCSUB3(MNS)
!
!
!X   For use with PFCN03 constant wavelength data with finite detector height
!C 19B
!H
!
      INCLUDE 'PARAMS.INC'
!
      LOGICAL NEAR90
      COMPLEX CFFT, CFF
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
      COMMON /WWPHASE/ NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),       &
     &                 SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /WWPRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),  &
     &                  DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),       &
     &                  ARGSTP(5), PCON
      COMMON /WWPRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),  &
     &                  DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),       &
     &                  NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ,      &
     &                  REFUSE, CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,  &
     &                  NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /WWPRSAVZ/ PKCONV(2048,9)
!      COMMON /WWPRSAVF/PKLIST(2048,9,200),ZXDEL(200),PKCONV(2048,9),
!     & ARGNOT(50),PKNOT(64,9,50),XPDKNT(50)
!
      COMMON /WWREFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
!
      COMMON /WWREFLNS/ REFH(3,WWREFDIM), AMUL(WWREFDIM),               &
     &                  AICALC(WWREFDIM), AIOBS(WWREFDIM),              &
     &                  ESDOBS(WWREFDIM), SOMEGA(WWREFDIM), GGCALC(300),&
     &                  MAXKK(9), KMIN, KMAX, KMOD, KNOW,               &
     &                  DSTAR(WWREFDIM), ISMAG(WWREFDIM), DKDDS, KOM23
      EQUIVALENCE (MAXK,MAXKK(1))
      COMMON /WWSOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),   &
     &                  NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),    &
     &                  NPCSOU(9,5)
      DIMENSION CFFT(8), FR(2048,8), FI(2048,8), FRT(2048), FIT(2048)
!
      LOGICAL LERANL
      COMMON /PKCOM3/ LERANL

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
!
      DENTEM = (FLOAT(MNS)*ZXDEL(KNOW))
      C2TEM = PI/DENTEM
      CTEM = 2.*C2TEM
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
!.. TO DEAL WITH (A) 90 DEGREES AND (B) ABOVE ALL WE WILL DO IS
!.. (A) SET FR(I,3)=1 AND ALL ELSE TO ZERO AND
!.. (B) SWITCH THE SIGN OF THE IMAGINARY COMPONENTS
      NEAR90 = (ABS(ARGK-90.).LT.2.0)
      IF (.NOT.NEAR90) THEN
        TANRA = ABS(TAN(RAD*ARGK))
        DENASY = 0.5*(HPS-HMS)*(HPS+HMS)
!.. BET1 AND NETPI CHANGE SIGN AT 90 DEGREES
!.. BET2, BETP, BETM, BETP2 AND BETM2 DO NOT
        BET1 = 0.5*RAD*DENTEM*TANRA
        BET2 = SQRT(BET1)
        BETP = HPS/BET2
        BETM = HMS/BET2
        BETP2 = PIBY2*BETP*BETP
        BETM2 = PIBY2*BETM*BETM
        BETPI = BET1/PI
      ENDIF
!
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
!.. GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,1) = EXP(-0.5*ARG*ARG)
        FI(I,1) = 0.
!!!        DR(I,1)= -ARG*ARG*FR(I,1)/SIG
!!!        DI(I,1)= 0.
!.. LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = MAX(0.,CLTEM*AFII)
        FR(I,2) = EXP(-ARG)
        FI(I,2) = 0.
!!!        DR(I,2)= -C2TEM*AFII*FR(I,2)
!!!        DI(I,2)= 0.
!.. ASYMMETRY FUNCTION FOR UMBRELLA EFFECT
        IF (II.EQ.0 .OR. NEAR90) THEN
          FR(I,3) = 1.
          FI(I,3) = 0.
!!!          DR(I,3)=0.
!!!          DR(I,4)=0.
!!!          DI(I,3)=0.
!!!          DI(I,4)=0.
        ELSE
          SII = SQRT(AFII)
          VAL = FLOAT(II)
          SVAL = SII/VAL
          ARGP1 = BETP*SII
          ARGM1 = BETM*SII
          CALL PF_FRENEL(ARGP1,FRCP,FRSP)
          CALL PF_FRENEL(ARGM1,FRCM,FRSM)
          FRCP = FRCP*BET2/SII
          FRSP = FRSP*BET2*SVAL
          FRCM = FRCM*BET2/SII
          FRSM = FRSM*BET2*SVAL
          BETPK = BETPI/VAL
          SINP = SIN(BETP2*VAL)
          SINM = SIN(BETM2*VAL)
          COSP = COS(BETP2*VAL)
          COSM = COS(BETM2*VAL)
!.. BET1 AND BETPI CHANGE SIGN AT 90 DEGREES
!.. BET2, BETP, BETM, BETP2 AND BETM2 DO NOT
!
          FR(I,3) = ((HPS*FRCP-HMS*FRCM)-BETPK*(SINP-SINM))/DENASY
          FI(I,3) = -((HPS*FRSP-HMS*FRSM)+BETPK*(COSP-COSM))/DENASY
!!!          DR(I,3)= (FRCP-HPS*FR(I,3))/DENASY
!!!          DI(I,3)= -(FRSP-HPS*FI(I,3))/DENASY
!!!          DR(I,4)= (HMS*FR(I,3)-FRCM)/DENASY
!!!          DI(I,4)= -(HMS*FI(I,3)-FRSM)/DENASY
          IF (ARGK.GT.90.) THEN
            FI(I,3) = -FI(I,3)
!!!            DI(I,3)=-DI(I,3)
!!!            DI(I,4)=-DI(I,4)
          ENDIF
        ENDIF
      ENDDO
!
!.. NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 1, 3
                 !NPKGEN(JPHASE,JSOURC)
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
!!!        IF(J.NE.4) CFFT(J)=CMPLX(FR(I,J),FI(I,J))
!!!        DFFT(J)=CMPLX(DR(I,J),DI(I,J))
        ENDDO
!!!        DDT(1)=DFFT(1)*CFFT(2)*CFFT(3)
!!!        DDT(2)=DFFT(2)*CFFT(1)*CFFT(3)
!!!        DDT(3)=DFFT(3)*CFFT(1)*CFFT(2)
!!!        DDT(4)=DFFT(4)*CFFT(1)*CFFT(2)
!!!        DO 4 J=1,NPKGEN(JPHASE,JSOURC)
!!!         DR(I,J)=REAL(DDT(J))
!!!         DI(I,J)=AIMAG(DDT(J))
!!!   4  CONTINUE
        CFF = CFFT(1)*CFFT(2)*CFFT(3)
        FRT(I) = REAL(CFF)
        FIT(I) = AIMAG(CFF)
      ENDDO
!
!.. DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL WWFT01A(MNS,INV,FRT,FIT)
!
!!!      DO 5 J=1,NPKGEN(JPHASE,JSOURC)
!!!   5   CALL WWFT01A(MNS,INV,DR(1,J),DI(1,J))
!.. WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./ZXDEL(KNOW)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKCONV(II,1) = FRT(I)*XTEM
!!!      DO 7 J=1,NPKGEN(JPHASE,JSOURC)
!!!      JJ=J+1
!!!      PKCONV(II,JJ)=DR(I,J)*XTEM
!!!   7  CONTINUE
      ENDDO
!
      RETURN
      END SUBROUTINE PF_FCSUB3
!*==PF_FRENEL.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE PF_FRENEL(Z,FRCOS,FRSIN)
      SUBROUTINE PF_FRENEL(Z,FRCOS,FRSIN)
!
! *** PF_FRENEL by WIFD 23 Feb 93 ***
!
!X
!C 9C
!H Calculates ?
!A On entry Z holds the argument
!A On exit FRCOS, FRSIN hold ?
!
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
!
      FZ = (1.+Z*0.926)/(2.+Z*(1.792+Z*3.104))
      GZ = 1./(2.+Z*(4.142+Z*(3.492+Z*6.670)))
!
      ARG = PIBY2*Z*Z
      SINARG = SIN(ARG)
      COSARG = COS(ARG)
      FRCOS = 0.5 + FZ*SINARG - GZ*COSARG
      FRSIN = 0.5 - FZ*COSARG - GZ*SINARG
!
      RETURN
      END SUBROUTINE PF_FRENEL
