!*==FDCN01.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!CSL Mark 4 Update 54 4-July-95
!
!
!               C A M B R I D G E   C R Y S T A L L O G R A P H Y
!
!                      S U B R O U T I N E   L I B R A R Y
!
!
!
!   Parameter ATFS altered from 20     to 50
!   Parameter ATOM altered from 50     to 150
!   Parameter BVAR altered from 200    to 400
!   Parameter CSTR altered from 20     to 300
!   Parameter F2VA altered from 200    to 300
!   Parameter MATS altered from 3000   to 80000
!   Parameter OLAP altered from 70     to 200
!   Parameter OMAX altered from 200    to 300
!   Parameter PHAS altered from 1      to 9
!   Parameter PSLK altered from 300    to 1000
!   Parameter PVAR altered from 1000   to 2000
!   Parameter REFS altered from 1000   to 10000
!   Parameter SLAK altered from 20     to 500
!   Parameter SORC altered from 1      to 5
!   Parameter VVAR altered from 250    to 500
!                  P E A K   F U N C T I O N   A N D
!               R E L A T E D   S U B R O U T I N E S
!            F O R   P R O F I L E   R E F I N E M E N T
!
!
!
!*****************************************************************************
!
      SUBROUTINE FDCN01(IOPT)
!
!  *** FDCN01 updated by JCM/WIFD 2 Mar 89 ***
!
!X
!C 19B
!H Function descriptors for constant wavelength, with derivatives
!A On entry IOPT=1 if only function descriptors are required
!A               2 if both descriptors and derivatives are required
!P Expects ARGK in /PRPKCN to hold 2thetaK degrees, and the peak
!P function species values in PKFNSP in /PRPKFN to have been read by
!P calls of PFCN0x
!
!D Used when CN, constant wavelength
!D Simple Gaussian, Cagliotti width variation
!D If SAPS and KNOW points to a reflection for which SIGS  is
!D being refined, uses value in F4PAR(2 instead of calculated value.
!
      INCLUDE 'PARAMS.INC'
!
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
!
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
! WE DO NOT WANT THE CALCULATION IF:
!  A) SAPS, AND
!  B) IT IS NOT THE SPECIAL INITIAL CYCLE, AND
!  C) WE ARE REFINING THIS SIGMA:
      TANTH = TAN(RAD*ARGK/2.)
      PKFNVA(1) = (PKFNSP(1,1,JPHASE,JSOURC)*TANTH+PKFNSP(1,2,JPHASE,   &
     &            JSOURC))*TANTH + PKFNSP(1,3,JPHASE,JSOURC)
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(1)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(1) = F4PAR(2,KNOW)
        ENDIF
      ENDIF
      IF (IOPT.EQ.1) GOTO 100
!
! DERIVATIVES OF PKFNVA(1) WRT THE 3 PKFNSP PARAMETERS
      DERPFN(1,1) = TANTH*TANTH
      DERPFN(1,2) = TANTH
      DERPFN(1,3) = 1.
  100 RETURN
      END SUBROUTINE FDCN01
!
!*****************************************************************************
!
      SUBROUTINE FDCN03(IOPT)
!
!
!X
!C 19B
!H Function descriptors for constant wavelength data with derivatives
!A On entry IOPT=1 if only function descriptors are required
!A               2 if both descriptors and derivatives are required
!P Expects ARGK in /PRPKCN to hold 2thetaK degrees, and the peak
!P function species values in PKFNSP in /PRPKFN to have been read by
!P calls of PFCN0x
!
!D Used with constant wavelength neutron or synchrotron radiation.
!D Voigt folded with VL&Y peak shape asymmetry
!D If SAPS and KNOW points to a reflection for which SIGS is
!D being refined, uses value in F4PAR(2) instead of calculated value.
!
!
      INCLUDE 'PARAMS.INC'
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
!
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
!
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
! For the moment, same variable names as TOF case...
!
! WE DO NOT WANT THE CALCULATION IF:
!  A) SAPS, AND
!  B) IT IS NOT THE SPECIAL INITIAL CYCLE, AND
!  C) WE ARE REFINING THIS PEAK VARIABLE:
      TANTH = TAN(RAD*ARGK/2.)
      SIGU = PKFNSP(1,1,JPHASE,JSOURC)
      SIGW = PKFNSP(1,2,JPHASE,JSOURC)
      TANSQ = TANTH**2
      SECSQ = 1. + TANSQ
      SIGMA = SQRT(SECSQ*SIGU**2+TANSQ*SIGW**2)
      PKFNVA(1) = SIGMA
      SECTH = SQRT(SECSQ)
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC)*SECTH + PKFNSP(2,2,JPHASE,JSOURC)*TANTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC)
      PKFNVA(4) = PKFNSP(4,1,JPHASE,JSOURC)
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(1)
          F4PAR(3,KNOW) = PKFNVA(2)
!          F4PAR(4,KNOW)=PKFNVA(3)
!          F4PAR(5,KNOW)=PKFNVA(4)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(1) = F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(2) = F4PAR(3,KNOW)
!          IF (KF4PAR(4,KNOW) .GT. 0) PKFNVA(3)=F4PAR(4,KNOW)
!          IF (KF4PAR(5,KNOW) .GT. 0) PKFNVA(4)=F4PAR(5,KNOW)
        ENDIF
      ENDIF
      IF (IOPT.EQ.1) GOTO 100
!
! DERIVATIVES OF PKFNVA ARRY WRT PKFNSP ARRAY:
      DERPFN(1,1) = SIGU*SECSQ/SIGMA
      DERPFN(1,2) = SIGW*TANSQ/SIGMA
      DERPFN(2,1) = SECTH
      DERPFN(2,2) = TANTH
      DERPFN(3,1) = 1.
      DERPFN(4,1) = 1.
  100 RETURN
      END SUBROUTINE FDCN03
!
!*****************************************************************************
!
      SUBROUTINE FDLX01(IOPT)
!
! *** FDLX01 updated by WIFD/JCM 7 Mar 89 ***
!
!X
!C 19B
!H Function descriptors for X-Ray data, with derivatives
!A On entry IOPT=1 if only function descriptors are required
!A               2 if both descriptors and derivatives are required
!P Expects ARGK in /PRPKCN to hold 2thetaK degrees, and the peak
!P function species values in PKFNSP in /PRPKFN to have been read by
!P calls of PFCN0x
!
!D Used when LX, LAB X-Ray
!D Simple Gaussian, Cagliotti width variation
!D If SAPS and KNOW points to a reflection for which SIGS is
!D being refined, uses value in F4PAR(2 instead of calculated value.
!
      INCLUDE 'PARAMS.INC'
!
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
!
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
! For the moment, same variable names as TOF case...
!
! WE DO NOT WANT THE CALCULATION IF:
!  A) SAPS, AND
!  B) IT IS NOT THE SPECIAL INITIAL CYCLE, AND
!  C) WE ARE REFINING THIS SIGMA:
      TANTH = TAN(RAD*ARGK/2.)
      PKFNVA(1) = (PKFNSP(1,1,JPHASE,JSOURC)*TANTH+PKFNSP(1,2,JPHASE,   &
     &            JSOURC))*TANTH + PKFNSP(1,3,JPHASE,JSOURC)
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(1)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(1) = F4PAR(2,KNOW)
        ENDIF
      ENDIF
      IF (IOPT.EQ.1) GOTO 100
      DERPFN(1,1) = TANTH*TANTH
      DERPFN(1,2) = TANTH
      DERPFN(1,3) = 1.
  100 RETURN
      END SUBROUTINE FDLX01
!
!*****************************************************************************
!
      SUBROUTINE FDLX02(IOPT)
!
! *** FDLX02 updated by WIFD/JCM 7 Mar 89 ***
!
!X
!C 19B
!H Function descriptors for X-Ray data type 2, with derivatives
!A On entry IOPT=1 if only function descriptors are required
!A               2 if both descriptors and derivatives are required
!P Expects ARGK in /PRPKCN to hold 2thetaK degrees, and the peak
!P function species values in PKFNSP in /PRPKFN to have been read by
!P calls of PFCN0x
!
!D Used when LX, LAB X-Ray
!D Simple Gaussian, Cagliotti width variation
!D If SAPS and KNOW points to a reflection for which SIGS is
!D being refined, uses value in F4PAR(2 instead of calculated value.
!
!
      INCLUDE 'PARAMS.INC'
!
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
!
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
!
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
! For the moment, same variable names as TOF case...
!
! WE DO NOT WANT THE CALCULATION IF:
!  A) SAPS, AND
!  B) IT IS NOT THE SPECIAL INITIAL CYCLE, AND
!  C) WE ARE REFINING THIS PEAK VARIABLE:
      TANTH = TAN(ARGK/2.)
      PKFNVA(1) = (PKFNSP(1,1,JPHASE,JSOURC)*TANTH+PKFNSP(1,2,JPHASE,   &
     &            JSOURC))*TANTH + PKFNSP(1,3,JPHASE,JSOURC)
      SECTH = SQRT(1.+TANTH*TANTH)
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC)                             &
     &            *TANTH + PKFNSP(2,2,JPHASE,JSOURC)*SECTH
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(1)
          F4PAR(3,KNOW) = PKFNVA(2)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(1) = F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(2) = F4PAR(3,KNOW)
        ENDIF
      ENDIF
      IF (IOPT.EQ.1) GOTO 100
!
! DERIVATIVES OF PKFNVA ARRY WRT PKFNSP ARRAY:
      DERPFN(1,1) = TANTH*TANTH
      DERPFN(1,2) = TANTH
      DERPFN(1,3) = 1.
      DERPFN(2,1) = TANTH
      DERPFN(2,2) = SECTH
  100 RETURN
      END SUBROUTINE FDLX02
!
!*****************************************************************************
!
      SUBROUTINE FDTF01(IOPT)
!
! *** FDTF01 updated by WIFD/JCM 1 Jun 89 ***
!
!X
!C 19B
!H Evaluates peak function descriptors and their derivatives wrt peak function
!H species for Time of Flight (TOF), type 1
!A IOPT on entry requests option:
!A    If IOPT=1, just calculates peak descriptors
!A    If IOPT=2, calculates descriptors and their derivatives
!P Expects ALAMBD(ILAMB,KSOURC) (or WLNGTH for only one lambda) set in /DGEOM
!D The peak function descriptors are SWCH, TAUF, TAUS, and SIGM.
!
      INCLUDE 'PARAMS.INC'
!
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
!
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
! SWCH:
      ATEM = PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
      BTEM = PKFNSP(1,1,JPHASE,JSOURC)*ATEM
      PKFNVA(1) = 0.5*ERFNC(BTEM)
! TAUF AND TAUS:
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
      WSQ = WLGTH*WLGTH
! C49: PKFNVA(3) IS SIGMA NOT SIGMA SQUARED
! C54: AND IT IS NOW PKFNVA(4)
! WE DO NOT WANT THE CALCULATION IF:
!  A) SAPS, AND
!  B) IT IS NOT THE SPECIAL INITIAL CYCLE, AND
!  C) WE ARE REFINING THIS PEAK WIDTH:
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      PKFNVA(4) = SIGMA
      F4VAL(2,KNOW) = SIGMA
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(4)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(4) = F4PAR(2,KNOW)
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.1) GOTO 100
!
! DERIVATIVES:
      CTEM = -0.564189584*EXP(-BTEM*BTEM)
      DERPFN(1,1) = ATEM*CTEM
      DERPFN(1,2) = CTEM*PKFNSP(1,1,JPHASE,JSOURC)
      DERPFN(2,1) = 1.
      DERPFN(2,2) = WLGTH
      DERPFN(3,1) = 1.
      DERPFN(3,2) = WLGTH
      D41 = 0.5/SIGMA
      DERPFN(4,1) = D41
      DERPFN(4,2) = WSQ*D41
      DERPFN(4,3) = WSQ*WSQ*D41
  100 RETURN
      END SUBROUTINE FDTF01
!
!*****************************************************************************
!
      SUBROUTINE FDTF02(IOPT)
!
! *** FDTF02 updated by WIFD/JCM 7 Mar 89 ***
!
! THIS SUBROUTINE EVALUATES THE PEAK FUNCTION DESCRIPTORS (PKFNVA)
! IN TERMS OF AND THEIR DERIVATIVES (DERPFN) WRT PKFNSP.
!
! EXPECTS ALAMBD(ILAMB,KSOURC) TO BE SET IN /DGEOM.
!
! IF IOPT=1, JUST CALCULATES THE VALUES OF THE PEAK DESCRIPTORS.  IF IOPT=2,
! CALCULATES ALSO THEIR DERIVATIVES WRT THE REFINABLE PARAMETERS.
!
! (LATER IT SHOULD USE THE FACT THAT IF LOGICAL REFUSE IS TRUE ON ENTRY, VALUES
! OF THE PEAK DESCRIPTORS SHOULD HAVE BEEN SAVED IN COMMON /PRSAVE, AND MAY NOW
! BE USED AGAIN.
!
!
      INCLUDE 'PARAMS.INC'
!
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
!.. SWITCH
      ATEM = PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
      BTEM = PKFNSP(1,1,JPHASE,JSOURC)*ATEM
      PKFNVA(1) = 0.5*ERFNC(BTEM)
!.. TAUF & TAUS
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
!.. C49: PKFNVA(4) IS SIGMA NOT SIGMA SQUARED
      WSQ = WLGTH*WLGTH
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      GAMMA = PKFNSP(5,1,JPHASE,JSOURC)                                 &
     &        + (PKFNSP(5,2,JPHASE,JSOURC)+PKFNSP(5,3,JPHASE,JSOURC)    &
     &        *WLGTH)*WLGTH
      PKFNVA(4) = SIGMA
      PKFNVA(5) = GAMMA
      F4VAL(2,KNOW) = SIGMA
      F4VAL(3,KNOW) = GAMMA
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(4)
          F4PAR(3,KNOW) = PKFNVA(5)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(4) = F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(5) = F4PAR(3,KNOW)
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.1) GOTO 100
      CTEM = -0.564189584*EXP(-BTEM*BTEM)
      DERPFN(1,1) = ATEM*CTEM
      DERPFN(1,2) = CTEM*PKFNSP(1,1,JPHASE,JSOURC)
      DERPFN(2,1) = 1.
      DERPFN(2,2) = WLGTH
      DERPFN(3,1) = 1.
      DERPFN(3,2) = WLGTH
      D41 = 0.5/SIGMA
      DERPFN(4,1) = D41
      DERPFN(4,2) = WSQ*D41
      DERPFN(4,3) = WSQ*WSQ*D41
      DERPFN(5,1) = 1.
      DERPFN(5,2) = WLGTH
      DERPFN(5,3) = WSQ
  100 RETURN
      END SUBROUTINE FDTF02
!
!*****************************************************************************
!
      SUBROUTINE FDTF03(IOPT)
!
! *** FDTF03 updated by WIFD/JCM 1 Jun 89 ***
!
! THIS SUBROUTINE EVALUATES THE PEAK FUNCTION DESCRIPTORS (PKFNVA)
! IN TERMS OF AND THEIR DERIVATIVES (DERPFN) WRT PKFNSP.
!
! EXPECTS ALAMBD(ILAMB,KSOURC) TO BE SET IN /DGEOM.
!
! IF IOPT=1, JUST CALCULATES THE VALUES OF THE PEAK DESCRIPTORS.  IF IOPT=2,
! CALCULATES ALSO THEIR DERIVATIVES WRT THE REFINABLE PARAMETERS.
!
! (LATER IT SHOULD USE THE FACT THAT IF LOGICAL REFUSE IS TRUE ON ENTRY, VALUES
! OF THE PEAK DESCRIPTORS SHOULD HAVE BEEN SAVED IN COMMON /PRSAVE, AND MAY NOW
! BE USED AGAIN.
!
!
      INCLUDE 'PARAMS.INC'
!
      DIMENSION CDUMP(10)
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
!.. SWITCH
      ATEM = PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
      BTEM = PKFNSP(1,1,JPHASE,JSOURC)*ATEM
      PKFNVA(1) = 0.5*ERFNC(BTEM)
!.. TAUF & TAUS
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
!.. C49: PKFNVA(4) IS SIGMA NOT SIGMA SQUARED
      WSQ = WLGTH*WLGTH
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      GAMMA = PKFNSP(5,1,JPHASE,JSOURC)                                 &
     &        + (PKFNSP(5,2,JPHASE,JSOURC)+PKFNSP(5,3,JPHASE,JSOURC)    &
     &        *WLGTH)*WLGTH
!.. C AXIS QUADRATIC TERM
      EL = REFH(3,KNOW)
      CALL CELDER(REFH(1,KNOW),CDUMP)
      D61 = ARGK*CPARS(3,2)*EL*EL/DSTAR2
      PKFNVA(4) = SIGMA
      PKFNVA(5) = GAMMA
      PKFNVA(6) = D61*PKFNSP(6,1,JPHASE,JSOURC)
      F4VAL(2,KNOW) = SIGMA
      F4VAL(3,KNOW) = GAMMA
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(4)
          F4PAR(3,KNOW) = PKFNVA(5)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(4) = F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(5) = F4PAR(3,KNOW)
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.1) GOTO 100
      CTEM = -0.564189584*EXP(-BTEM*BTEM)
      DERPFN(1,1) = ATEM*CTEM
      DERPFN(1,2) = CTEM*PKFNSP(1,1,JPHASE,JSOURC)
      DERPFN(2,1) = 1.
      DERPFN(2,2) = WLGTH
      DERPFN(3,1) = 1.
      DERPFN(3,2) = WLGTH
      D41 = 0.5/SIGMA
      DERPFN(4,1) = D41
      DERPFN(4,2) = WSQ*D41
      DERPFN(4,3) = WSQ*WSQ*D41
      DERPFN(5,1) = 1.
      DERPFN(5,2) = WLGTH
      DERPFN(5,3) = WSQ
      DERPFN(6,1) = D61
  100 RETURN
      END SUBROUTINE FDTF03
!
!*****************************************************************************
!
      SUBROUTINE FDTF04(IOPT)
!
! *** FDTF04 updated by WIFD May 89 ***
!
!X
!C 19B
!H Forms Peak function descriptors & derivatives for POLARIS data.
!
!A On entry IOPT indicates the opion reuired:
!A If IOPT=1, just calculates the values of the peak descriptors.
!A If IOPT=2, calculates also their derivatives wrt the refinable parameters.
!P Expects ALAMBD(ILAMB) the wavelength, to be set in /DGEOM.
!D Evaluates the peak function descriptors (PKFNVA) in terms of PKFNSP,
!D the Peak Function Species, and the derivatives (DERPFN) of PKFNVA wrt PKFNSP.
!
! (LATER IT SHOULD USE THE FACT THAT IF LOGICAL REFUSE IS TRUE ON ENTRY, VALUES
! OF THE PEAK DESCRIPTORS SHOULD HAVE BEEN SAVED IN COMMON /PRSAVE, AND MAY NOW
! BE USED AGAIN.  I WILL INVESTIGATE THIS REMARK SOON - JUDY).
!
!
      INCLUDE 'PARAMS.INC'
!
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
!.. SWITCH
!      ATEM= PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
!      BTEM= PKFNSP(1,1,JPHASE,JSOURC)*ATEM
!      PKFNVA(1)= 0.5*ERFNC(BTEM)
      WSQ = WLGTH*WLGTH
      SWITCH = EXP(-PKFNSP(1,1,JPHASE,JSOURC)/WSQ)
      PKFNVA(1) = SWITCH
!.. TAUF & TAUS
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
!.. C49: PKFNVA(4) IS SIGMA NOT SIGMA SQUARED
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      GAMMA = PKFNSP(5,1,JPHASE,JSOURC)                                 &
     &        + (PKFNSP(5,2,JPHASE,JSOURC)+PKFNSP(5,3,JPHASE,JSOURC)    &
     &        *WLGTH)*WLGTH
      PKFNVA(4) = SIGMA
      PKFNVA(5) = GAMMA
      PKFNVA(6) = PKFNSP(6,1,JPHASE,JSOURC)
      F4VAL(3,KNOW) = SIGMA
      F4VAL(2,KNOW) = PKFNVA(1)
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(1)
          F4PAR(3,KNOW) = PKFNVA(4)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(1) = F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(4) = F4PAR(3,KNOW)
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.2) THEN
!      CTEM= -0.564189584*EXP(-BTEM*BTEM)
!      DERPFN(1,1)= ATEM*CTEM
!      DERPFN(1,2)= CTEM*PKFNSP(1,1,JPHASE,JSOURC)
        DERPFN(1,1) = -SWITCH/WSQ
        DERPFN(2,1) = 1.
        DERPFN(2,2) = WLGTH
        DERPFN(3,1) = 1.
        DERPFN(3,2) = WLGTH
        D41 = 0.5/SIGMA
        DERPFN(4,1) = D41
        DERPFN(4,2) = WSQ*D41
        DERPFN(4,3) = WSQ*WSQ*D41
        DERPFN(5,1) = 1.
        DERPFN(5,2) = WLGTH
        DERPFN(5,3) = WSQ
        DERPFN(6,1) = 1.
      ENDIF
      RETURN
      END SUBROUTINE FDTF04
!
!*****************************************************************************
!
      SUBROUTINE FDTF05(IOPT)
!
! *** FDTF05 updated by WIFD/JCM 7 Mar 89 ***
!
! THIS SUBROUTINE EVALUATES THE PEAK FUNCTION DESCRIPTORS (PKFNVA)
! IN TERMS OF AND THEIR DERIVATIVES (DERPFN) WRT PKFNSP.
!
! EXPECTS ALAMBD(ILAMB,KSOURC) TO BE SET IN /DGEOM.
!
! IF IOPT=1, JUST CALCULATES THE VALUES OF THE PEAK DESCRIPTORS.  IF IOPT=2,
! CALCULATES ALSO THEIR DERIVATIVES WRT THE REFINABLE PARAMETERS.
!
! (LATER IT SHOULD USE THE FACT THAT IF LOGICAL REFUSE IS TRUE ON ENTRY, VALUES
! OF THE PEAK DESCRIPTORS SHOULD HAVE BEEN SAVED IN COMMON /PRSAVE, AND MAY NOW
! BE USED AGAIN.
!
      INCLUDE 'PARAMS.INC'
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
!
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
!.. SWITCH
      ATEM = PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
      BTEM = PKFNSP(1,1,JPHASE,JSOURC)*ATEM
      PKFNVA(1) = 0.5*ERFNC(BTEM)
!.. TAUF & TAUS
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
!.. C49: PKFNVA(4) IS SIGMA NOT SIGMA SQUARED
      WSQ = WLGTH*WLGTH
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      GAMMA = PKFNSP(5,1,JPHASE,JSOURC)                                 &
     &        + (PKFNSP(5,2,JPHASE,JSOURC)+PKFNSP(5,3,JPHASE,JSOURC)    &
     &        *WLGTH)*WLGTH
      PKFNVA(4) = SIGMA
      PKFNVA(5) = GAMMA
!.. THIS IS SPECIFIC TO TETRAGONAL SYSTEMS!!!
      R1 = REFH(1,KNOW)**2 + REFH(2,KNOW)**2
      R2 = REFH(3,KNOW)**2
      AV = CELL(1,1,1)
      CV = CELL(3,1,1)
      V1 = SQRT(R1/AV**2+R2/CV**2)
      V2 = SQRT(R1/(AV+0.001*PKFNSP(7,1,JPHASE,JSOURC))                 &
     &     **2+R2/(CV+0.001*PKFNSP(7,2,JPHASE,JSOURC))**2)
      EFAC = (REFH(3,KNOW)/(CV*V1))**2
      ESIGM = EFAC*SQRT                                                 &
     &        ((PKFNSP(6,1,JPHASE,JSOURC)+PKFNSP(6,2,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      PKFNVA(6) = ESIGM
      STTEM = 252.777*PKCNSP(1,JPHASE,JSOURC)*TWSNTH(JSOURC)
      PKFNVA(7) = STTEM*(1./V2-1./V1)
      PKFNVA(8) = PKFNSP(8,1,JPHASE,JSOURC)
!
      F4VAL(2,KNOW) = SIGMA
      F4VAL(3,KNOW) = GAMMA
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(4)
          F4PAR(3,KNOW) = PKFNVA(5)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(4) = F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(5) = F4PAR(3,KNOW)
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.1) GOTO 100
      CTEM = -0.564189584*EXP(-BTEM*BTEM)
      DERPFN(1,1) = ATEM*CTEM
      DERPFN(1,2) = CTEM*PKFNSP(1,1,JPHASE,JSOURC)
      DERPFN(2,1) = 1.
      DERPFN(2,2) = WLGTH
      DERPFN(3,1) = 1.
      DERPFN(3,2) = WLGTH
      D41 = 0.5/SIGMA
      DERPFN(4,1) = D41
      DERPFN(4,2) = WSQ*D41
      DERPFN(4,3) = WSQ*WSQ*D41
      DERPFN(5,1) = 1.
      DERPFN(5,2) = WLGTH
      DERPFN(5,3) = WSQ
      IF (ESIGM.LE.0.) THEN
        DERPFN(6,1) = 0.
        DERPFN(6,2) = 0.
      ELSE
        D61 = 0.5/ESIGM
        D61 = D61*EFAC**2
        DERPFN(6,1) = WSQ*D61
        DERPFN(6,2) = WSQ*WSQ*D61
      ENDIF
      DERPFN(7,1) = (0.001*STTEM*R1)/(AV*V2)**3
!CC     &((AV+0.001*PKFNSP(7,1,JPHASE,JSOURC))*V2)**3
      DERPFN(7,2) = (0.001*STTEM*R2)/(CV*V2)**3
!CC     &((CV+0.001*PKFNSP(7,2,JPHASE,JSOURC))*V2)**3
      DERPFN(8,1) = 1.
  100 RETURN
      END SUBROUTINE FDTF05
!
!*****************************************************************************
!
      SUBROUTINE FDTF08(IOPT)
!
! *** FDTF08 (formerly 04) updated by WIFD May 89 ***
!
! THIS SUBROUTINE EVALUATES THE PEAK FUNCTION DESCRIPTORS (PKFNVA)
! IN TERMS OF AND THEIR DERIVATIVES (DERPFN) WRT PKFNSP.
!
! EXPECTS ALAMBD(ILAMB,KSOURC) TO BE SET IN /DGEOM.
!
! IF IOPT=1, JUST CALCULATES THE VALUES OF THE PEAK DESCRIPTORS.  IF IOPT=2,
! CALCULATES ALSO THEIR DERIVATIVES WRT THE REFINABLE PARAMETERS.
!
! (LATER IT SHOULD USE THE FACT THAT IF LOGICAL REFUSE IS TRUE ON ENTRY, VALUES
! OF THE PEAK DESCRIPTORS SHOULD HAVE BEEN SAVED IN COMMON /PRSAVE, AND MAY NOW
! BE USED AGAIN.
!
      INCLUDE 'PARAMS.INC'
!
      DIMENSION CREFH(6)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
!.. SWITCH
      ATEM = PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
      BTEM = PKFNSP(1,1,JPHASE,JSOURC)*ATEM
      PKFNVA(1) = 0.5*ERFNC(BTEM)
!.. TAUF & TAUS
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
!.. C49: PKFNVA(4) IS SIGMA NOT SIGMA SQUARED
      WSQ = WLGTH*WLGTH
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      PKFNVA(4) = SIGMA
      DSP = 1./DSTAR(KNOW)
      DSP2 = DSP*DSP
      DSP3 = DSP*DSP2
      ARGST = 0.
      ARGPS = 0.
      DO I = 1, 3
        CREFH(I) = CPARS(I,2)*REFH(I,KNOW)**2
        I3 = I + 3
        IL = 9 - I3
        I1 = (8-I3)/2
        I2 = IL - I1
        CREFH(I3) = 2.*CPARS(I3,2)*REFH(I1,KNOW)*REFH(I2,KNOW)
      ENDDO
      DO I = 1, 6
        ARGST = ARGST + PKFNSP(5,I,JPHASE,JSOURC)*CREFH(I)
        ARGPS = ARGPS + PKFNSP(6,I,JPHASE,JSOURC)*CREFH(I)
      ENDDO
      ARGST = SQRT(AMAX1(0.,ARGST))
      ARGPS = SQRT(AMAX1(0.,ARGPS))
      GAMST = DSP2*ARGST
      GAMPS = DSP3*ARGPS
      PKFNVA(5) = GAMST
      PKFNVA(6) = GAMPS
      F4VAL(3,KNOW) = SIGMA
      F4VAL(2,KNOW) = GAMST + GAMPS
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          F4PAR(2,KNOW) = PKFNVA(5) + PKFNVA(6)
          F4PAR(3,KNOW) = PKFNVA(4)
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(5) = 0.5*F4PAR(2,KNOW)
          IF (KF4PAR(2,KNOW).GT.0) PKFNVA(6) = 0.5*F4PAR(2,KNOW)
          IF (KF4PAR(3,KNOW).GT.0) PKFNVA(4) = F4PAR(3,KNOW)
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.1) GOTO 100
      CTEM = -0.564189584*EXP(-BTEM*BTEM)
      DERPFN(1,1) = ATEM*CTEM
      DERPFN(1,2) = CTEM*PKFNSP(1,1,JPHASE,JSOURC)
      DERPFN(2,1) = 1.
      DERPFN(2,2) = WLGTH
      DERPFN(3,1) = 1.
      DERPFN(3,2) = WLGTH
      D41 = 0.5/SIGMA
      DERPFN(4,1) = D41
      DERPFN(4,2) = WSQ*D41
      DERPFN(4,3) = WSQ*WSQ*D41
      IF (ARGST.EQ.0.) THEN
        DO I = 1, 6
          DERPFN(5,I) = 0.
        ENDDO
      ELSE
        DO I = 1, 6
          DERPFN(5,I) = 0.5*DSP2*CREFH(I)/ARGST
        ENDDO
      ENDIF
!
      IF (ARGPS.EQ.0.) THEN
        DO I = 1, 6
          DERPFN(6,I) = 0.
        ENDDO
      ELSE
        DO I = 1, 6
          DERPFN(6,I) = 0.5*DSP3*CREFH(I)/ARGPS
        ENDDO
      ENDIF
  100 RETURN
      END SUBROUTINE FDTF08
!
!*****************************************************************************
!
      SUBROUTINE FDTF12(IOPT)
!
! *** FDTF12 by JBF 2 March 94 ***
!
!X
!C 19B
!H Forms Peak function descriptors & derivatives as for FDTF02 but with
!H alternative SIGM and GAMM parameters for magnetic peaks data.
!
!A On entry IOPT indicates the opion required:
!A If IOPT=1, just calculates the values of the peak descriptors.
!A If IOPT=2, calculates also their derivatives wrt the refinable parameters.
!A
!P Expects ALAMBD(ILAMB,KSOURC) the wavelength, to be set in /DGEOM.
!D Evaluates the peak function descriptors (PKFNVA) in terms of PKFNSP,
!D the Peak Function Species, and the derivatives (DERPFN) of PKFNVA wrt PKFNSP.
!
! (LATER IT SHOULD USE THE FACT THAT IF LOGICAL REFUSE IS TRUE ON ENTRY, VALUES
! OF THE PEAK DESCRIPTORS SHOULD HAVE BEEN SAVED IN COMMON /PRSAVE, AND MAY NOW
! BE USED AGAIN.  I WILL INVESTIGATE THIS REMARK SOON - JUDY).
!
      INCLUDE 'PARAMS.INC'
!
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
!.. SWITCH
      ATEM = PKFNSP(1,2,JPHASE,JSOURC) - 1./WLGTH
      BTEM = PKFNSP(1,1,JPHASE,JSOURC)*ATEM
      PKFNVA(1) = 0.5*ERFNC(BTEM)
!.. TAUF & TAUS
      PKFNVA(2) = PKFNSP(2,1,JPHASE,JSOURC) + PKFNSP(2,2,JPHASE,JSOURC) &
     &            *WLGTH
      PKFNVA(3) = PKFNSP(3,1,JPHASE,JSOURC) + PKFNSP(3,2,JPHASE,JSOURC) &
     &            *WLGTH
!.. C49: PKFNVA(4) IS SIGMA NOT SIGMA SQUARED
      WSQ = WLGTH*WLGTH
      SIGMA = SQRT(PKFNSP(4,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(4,2,JPHASE,JSOURC)+PKFNSP(4,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      SIGMM = SQRT(PKFNSP(6,1,JPHASE,JSOURC)                            &
     &        +(PKFNSP(6,2,JPHASE,JSOURC)+PKFNSP(6,3,JPHASE,JSOURC)*WSQ)&
     &        *WSQ)
      GAMMA = PKFNSP(5,1,JPHASE,JSOURC)                                 &
     &        + (PKFNSP(5,2,JPHASE,JSOURC)+PKFNSP(5,3,JPHASE,JSOURC)    &
     &        *WLGTH)*WLGTH
      GAMMM = PKFNSP(7,1,JPHASE,JSOURC)                                 &
     &        + (PKFNSP(7,2,JPHASE,JSOURC)+PKFNSP(7,3,JPHASE,JSOURC)    &
     &        *WLGTH)*WLGTH
      PKFNVA(4) = SIGMA
      PKFNVA(5) = GAMMA
      PKFNVA(6) = SIGMM
      PKFNVA(7) = GAMMM
! USE ISMAG TO SWITCH BETWEEN SIGMA/SIGMM AND GAMMA/GAMMM
      IF (ISMAG(KNOW).EQ.0) THEN
        F4VAL(2,KNOW) = SIGMA
        F4VAL(3,KNOW) = GAMMA
      ELSE
        F4VAL(2,KNOW) = SIGMM
        F4VAL(3,KNOW) = GAMMM
      ENDIF
      IF (SAPS .OR. APES) THEN
        IF (PRECYC) THEN
          IF (ISMAG(KNOW).EQ.0) THEN
            F4PAR(2,KNOW) = PKFNVA(4)
            F4PAR(3,KNOW) = PKFNVA(5)
          ELSE
            F4PAR(2,KNOW) = PKFNVA(6)
            F4PAR(3,KNOW) = PKFNVA(7)
          ENDIF
        ELSE
          IF (KF4PAR(2,KNOW).GT.0) THEN
            IF (ISMAG(KNOW).EQ.0) THEN
              PKFNVA(4) = F4PAR(2,KNOW)
            ELSE
              PKFNVA(6) = F4PAR(2,KNOW)
            ENDIF
          ENDIF
          IF (KF4PAR(3,KNOW).GT.0) THEN
            IF (ISMAG(KNOW).EQ.0) THEN
              PKFNVA(5) = F4PAR(3,KNOW)
            ELSE
              PKFNVA(7) = F4PAR(3,KNOW)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
! IOPT=1 EXIT HERE AS ONLY PEAK DESCRIPTORS ARE WANTED (NOT DERIVATIVES)
      IF (IOPT.EQ.1) GOTO 100
      CTEM = -0.564189584*EXP(-BTEM*BTEM)
      DERPFN(1,1) = ATEM*CTEM
      DERPFN(1,2) = CTEM*PKFNSP(1,1,JPHASE,JSOURC)
      DERPFN(2,1) = 1.
      DERPFN(2,2) = WLGTH
      DERPFN(3,1) = 1.
      DERPFN(3,2) = WLGTH
      D41 = 0.5/SIGMA
      DERPFN(4,1) = D41
      DERPFN(4,2) = WSQ*D41
      DERPFN(4,3) = WSQ*WSQ*D41
      DERPFN(5,1) = 1.
      DERPFN(5,2) = WLGTH
      DERPFN(5,3) = WSQ
      D61 = 0.5/SIGMM
      DERPFN(6,1) = D61
      DERPFN(6,2) = WSQ*D61
      DERPFN(6,3) = WSQ*WSQ*D61
      DERPFN(7,1) = 1.
      DERPFN(7,2) = WLGTH
      DERPFN(7,3) = WSQ
  100 RETURN
      END SUBROUTINE FDTF12
!
!*****************************************************************************
!
      SUBROUTINE FTSB12(MNS)
!
! *** FTSB12 by JBF MAR 94 For N and M peaks with different SIGM and GAMM ***
!
      INCLUDE 'PARAMS.INC'
!
      COMPLEX CFFT, DFFT, DDT, CFE, CFT
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
! JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SCRAT / CFFT(8), DFFT(8), DDT(8), CFE, CFT, FR(256,8),    &
     &                FI(256,8), DR(256,8), DI(256,8), FRE(256),        &
     &                FIE(256), FRT(256), FIT(256)
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
!
      IF (ISMAG(KNOW).EQ.0) THEN
        SIG = PKFNVA(4)
        GAM = PKFNVA(5)
      ELSE
        SIG = PKFNVA(6)
        GAM = PKFNVA(7)
      ENDIF
      SW1 = 1. - SW
!
      C2TEM = PI/(FLOAT(MNS)*XPKDEL(KMOD))
      CTEM = 2.*C2TEM
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
!
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
!.. FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        FR(I,2) = 1./(1.+ARG*ARG)
        FI(I,2) = ARG*FR(I,2)
        ATEMF = CTEM*FLOAT(II)*FR(I,2)**2
        DR(I,2) = -2.*ARG*ATEMF
        DI(I,2) = (1.-ARG*ARG)*ATEMF
!.. SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
!.. GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
!.. LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM*AFII
        FR(I,5) = EXP(-ARG)
        FI(I,5) = 0.
        DR(I,5) = -C2TEM*AFII*FR(I,5)
        DI(I,5) = 0.
      ENDDO
!
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
! LIMITS ARE SPECIFIC TO THIS VERSION WHERE EITHER 4&5 OR 6&7 ARE APPROPRIATE
        DO J = 2, NPKGEN(JPHASE,JSOURC) - 2
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CFE = CFFT(2)*CFFT(4)*CFFT(5)
        CFT = CFFT(3)*CFFT(4)*CFFT(5)
        DDT(2) = SW*DFFT(2)*CFFT(4)*CFFT(5)
        DDT(3) = SW1*DFFT(3)*CFFT(4)*CFFT(5)
        DDT(4) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(4)*CFFT(5)
        DDT(5) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(5)*CFFT(4)
! LIMITS ARE SPECIFIC TO THIS VERSION WHERE EITHER 4&5 OR 6&7 ARE APPROPRIATE
        DO J = 2, NPKGEN(JPHASE,JSOURC) - 2
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
      ENDDO
!
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRE,FIE)
      CALL FT01A(MNS,INV,FRT,FIT)
!
! LIMITS ARE SPECIFIC TO THIS VERSION WHERE EITHER 4&5 OR 6&7 ARE APPROPRIATE
      DO J = 2, NPKGEN(JPHASE,JSOURC) - 2
        CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKADD(II,1) = (SW*FRE(I)+SW1*FRT(I))*XTEM
        PKADD(II,2) = (FRE(I)-FRT(I))*XTEM
! STORE IN PKADD AS APPROPRIATE
!      DO 7 J=2,NPKGEN(JPHASE,JSOURC)
!      JJ=J+1
!      PKADD(II,JJ)= DR(I,J)*XTEM
!   7  CONTINUE
        PKADD(II,3) = DR(I,2)*XTEM
        PKADD(II,4) = DR(I,3)*XTEM
        IF (ISMAG(KNOW).EQ.0) THEN
          PKADD(II,5) = DR(I,4)*XTEM
          PKADD(II,6) = DR(I,5)*XTEM
          PKADD(II,7) = 0.0
          PKADD(II,8) = 0.0
        ELSE
          PKADD(II,5) = 0.0
          PKADD(II,6) = 0.0
          PKADD(II,7) = DR(I,4)*XTEM
          PKADD(II,8) = DR(I,5)*XTEM
        ENDIF
      ENDDO

      END SUBROUTINE FTSB12
!
!*****************************************************************************
!
      SUBROUTINE FTSUB2(MNS)
!
! *** FTSUB2 by WIFD Jun 88 ***
!
      INCLUDE 'PARAMS.INC'

      COMPLEX CFFT, DFFT, DDT, CFE, CFT
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      INCLUDE 'REFLNS.INC'
      COMMON /SCRAT / CFFT(8), DFFT(8), DDT(8), CFE, CFT, FR(256,8),    &
     &                FI(256,8), DR(256,8), DI(256,8), FRE(256),        &
     &                FIE(256), FRT(256), FIT(256)
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)

      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
      SIG = PKFNVA(4)
      GAM = PKFNVA(5)
      SW1 = 1. - SW
      C2TEM = PI/(FLOAT(MNS)*XPKDEL(KMOD))
      CTEM = 2.*C2TEM
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
! FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        FR(I,2) = 1./(1.+ARG*ARG)
        FI(I,2) = ARG*FR(I,2)
        ATEMF = CTEM*FLOAT(II)*FR(I,2)**2
        DR(I,2) = -2.*ARG*ATEMF
        DI(I,2) = (1.-ARG*ARG)*ATEMF
! SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
! GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
! LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM*AFII
        FR(I,5) = EXP(-ARG)
        FI(I,5) = 0.
        DR(I,5) = -C2TEM*AFII*FR(I,5)
        DI(I,5) = 0.
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CFE = CFFT(2)*CFFT(4)*CFFT(5)
        CFT = CFFT(3)*CFFT(4)*CFFT(5)
        DDT(2) = SW*DFFT(2)*CFFT(4)*CFFT(5)
        DDT(3) = SW1*DFFT(3)*CFFT(4)*CFFT(5)
        DDT(4) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(4)*CFFT(5)
        DDT(5) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(5)*CFFT(4)
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRE,FIE)
      CALL FT01A(MNS,INV,FRT,FIT)
      DO J = 2, NPKGEN(JPHASE,JSOURC)
        CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKADD(II,1) = (SW*FRE(I)+SW1*FRT(I))*XTEM
        PKADD(II,2) = (FRE(I)-FRT(I))*XTEM
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          JJ = J + 1
          PKADD(II,JJ) = DR(I,J)*XTEM
        ENDDO
      ENDDO

      END SUBROUTINE FTSUB2
!
!*****************************************************************************
!
      SUBROUTINE FTSUB3(MNS)
!
! *** FTSUB2 by WIFD ***
!
      INCLUDE 'PARAMS.INC'

      COMPLEX CFFT, DFFT, DDT, CFE, CFT
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      INCLUDE 'REFLNS.INC'
      COMMON /SCRAT / CFFT(8), DFFT(8), DDT(8), CFE, CFT, FR(256,8),    &
     &                FI(256,8), DR(256,8), DI(256,8), FRE(256),        &
     &                FIE(256), FRT(256), FIT(256)
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)

      LOGICAL CANSMA

      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
      SIG = PKFNVA(4)
      GAM = PKFNVA(5)
      CAN = PKFNVA(6)
      CANSMA = PKFNVA(6).LT.0.01*XPKDEL(KMOD)
      SW1 = 1. - SW
      C2TEM = PI/(FLOAT(MNS)*XPKDEL(KMOD))
      CTEM = 2.*C2TEM
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
      CATEM = CTEM*CAN
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
! FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        FR(I,2) = 1./(1.+ARG*ARG)
        FI(I,2) = ARG*FR(I,2)
        ATEMF = CTEM*FLOAT(II)*FR(I,2)**2
        DR(I,2) = -2.*ARG*ATEMF
        DI(I,2) = (1.-ARG*ARG)*ATEMF
! SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
! GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
! LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM*AFII
        FR(I,5) = EXP(-ARG)
        FI(I,5) = 0.
        DR(I,5) = -C2TEM*AFII*FR(I,5)
        DI(I,5) = 0.
! TRUNCATED QUADRATIC FUNCTION
        ARG = CATEM*FLOAT(II)
        IF (CANSMA .OR. II.EQ.0) THEN
          FR(I,6) = 1.
          FI(I,6) = 0.75*ARG
          DR(I,6) = 0.
          DI(I,6) = 0.75*CTEM*FLOAT(II)
        ELSE
          COSARG = COS(ARG)
          SINARG = SIN(ARG)
          OVARG = 1./ARG
          CVT1 = 2.*OVARG*OVARG
          CVT2 = OVARG*(1.-CVT1)
          FR(I,6) = 3.*(CVT1*COSARG+CVT2*SINARG)
          FI(I,6) = 3.*(CVT1*(SINARG-OVARG)-CVT2*COSARG)
          TCAN = 3./CAN
          CVT3 = 1. - 3.*CVT1
          CVT4 = 3.*CVT2
          DR(I,6) = TCAN*(CVT3*COSARG-CVT4*SINARG)
          DI(I,6) = TCAN*(CVT3*SINARG+CVT4*COSARG)
        ENDIF
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CFE = CFFT(2)*CFFT(4)*CFFT(5)*CFFT(6)
        CFT = CFFT(3)*CFFT(4)*CFFT(5)*CFFT(6)
        DDT(2) = SW*DFFT(2)*CFFT(4)*CFFT(5)*CFFT(6)
        DDT(3) = SW1*DFFT(3)*CFFT(4)*CFFT(5)*CFFT(6)
        DDT(4) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(4)*CFFT(5)*CFFT(6)
        DDT(5) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(5)*CFFT(4)*CFFT(6)
        DDT(6) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(6)*CFFT(4)*CFFT(5)
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRE,FIE)
      CALL FT01A(MNS,INV,FRT,FIT)
      DO J = 2, NPKGEN(JPHASE,JSOURC)
        CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKADD(II,1) = (SW*FRE(I)+SW1*FRT(I))*XTEM
        PKADD(II,2) = (FRE(I)-FRT(I))*XTEM
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          JJ = J + 1
          PKADD(II,JJ) = DR(I,J)*XTEM
        ENDDO
      ENDDO

      END SUBROUTINE FTSUB3
!
!*****************************************************************************
!
      SUBROUTINE FTSUB4(MNS)
!
! *** FTSUB4 BY WIFD MAY 89***
!
!X
!C 19B
!H
      INCLUDE 'PARAMS.INC'

      COMPLEX CFFT, DFFT, DDT, CFE, CFT, CFED, CFEDI, DFED, CIARG, DPED
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      DIMENSION CFFT(8), DFFT(8), DDT(8), FR(256,8), FI(256,8),         &
     &          DR(256,8), DI(256,8), FRE(256), FIE(256), FRT(256),     &
     &          FIT(256)

      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
      SIG = PKFNVA(4)
      GAM = PKFNVA(5)
      POW = PKFNVA(6)
      POW1 = POW + 1.
      SW1 = 1. - SW
      C2TEM = PI/(FLOAT(MNS)*XPKDEL(KMOD))
      CTEM = 2.*C2TEM
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
! FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        CFEDI = CMPLX(1.,-ARG)
        CFED = CFEDI**(-POW1)
        FR(I,2) = REAL(CFED)
        FI(I,2) = AIMAG(CFED)
        CIARG = CMPLX(0.,CTEM*FLOAT(II))
        DFED = POW1*CIARG*CFED/CFEDI
        DR(I,2) = REAL(DFED)
        DI(I,2) = AIMAG(DFED)
        DPED = -CFED*CLOG(CFEDI)
        DR(I,6) = REAL(DPED)
        DI(I,6) = AIMAG(DPED)
! SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
! GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
! LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM*AFII
        FR(I,5) = EXP(-ARG)
        FI(I,5) = 0.
        DR(I,5) = -C2TEM*AFII*FR(I,5)
        DI(I,5) = 0.
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          IF (J.NE.6) CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CFE = CFFT(2)*CFFT(4)*CFFT(5)
        CFT = CFFT(3)*CFE
        CFED = SW1 + SW*CFFT(3)
        DDT(2) = CFED*DFFT(2)*CFFT(4)*CFFT(5)
        DDT(6) = CFED*DFFT(6)*CFFT(4)*CFFT(5)
        DDT(3) = SW*CFFT(2)*DFFT(3)*CFFT(4)*CFFT(5)
        CFED = CFED*CFFT(2)
        DDT(4) = CFED*DFFT(4)*CFFT(5)
        DDT(5) = CFED*CFFT(4)*DFFT(5)
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRE,FIE)
      CALL FT01A(MNS,INV,FRT,FIT)
      DO J = 2, NPKGEN(JPHASE,JSOURC)
        CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKADD(II,1) = (SW1*FRE(I)+SW*FRT(I))*XTEM
        PKADD(II,2) = (FRT(I)-FRE(I))*XTEM
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          JJ = J + 1
          PKADD(II,JJ) = DR(I,J)*XTEM
        ENDDO
      ENDDO

      END SUBROUTINE FTSUB4
!
!*****************************************************************************
!
      SUBROUTINE FTSUB5(MNS)
!
! *** FTSUB5 by WIFD Jun 88 ***
!
      INCLUDE 'PARAMS.INC'

      COMPLEX CFFT, DFFT, DDT, CFE, CFT, CTM1, CTM2, CFE2, CFT2, CF67
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      INCLUDE 'REFLNS.INC'
      COMMON /SCRAT / CFFT(8), DFFT(8), DDT(8), CFE, CFT, FR(256,8),    &
     &                FI(256,8), DR(256,8), DI(256,8), FRE(256),        &
     &                FIE(256), FRT(256), FIT(256), FRE2(256), FIE2(256)&
     &                , FRT2(256), FIT2(256)
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)

      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
      SIG = PKFNVA(4)
      GAM = PKFNVA(5)
      ESIG = PKFNVA(6)
      SHFT = PKFNVA(7)
      SZ = PKFNVA(8)
      SW1 = 1. - SW
      SZ1 = 1. - SZ
      C2TEM = PI/(FLOAT(MNS)*XPKDEL(KMOD))
      CTEM = 2.*C2TEM
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
      EGTEM = CTEM*ESIG
      STEM = CTEM*SHFT
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
! FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        FR(I,2) = 1./(1.+ARG*ARG)
        FI(I,2) = ARG*FR(I,2)
        ATEMF = CTEM*FLOAT(II)*FR(I,2)**2
        DR(I,2) = -2.*ARG*ATEMF
        DI(I,2) = (1.-ARG*ARG)*ATEMF
! SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
! GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
! LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM*AFII
        FR(I,5) = EXP(-ARG)
        FI(I,5) = 0.
        DR(I,5) = -C2TEM*AFII*FR(I,5)
        DI(I,5) = 0.
! EXTRA GAUSSIAN
        ARG = EGTEM*FLOAT(II)
        FR(I,6) = EXP(-0.5*ARG*ARG)
        FI(I,6) = 0.
        IF (ESIG.EQ.0.) THEN
          DR(I,6) = 0.
        ELSE
          DR(I,6) = -ARG*ARG*FR(I,6)/ESIG
        ENDIF
        DI(I,6) = 0.
! SHIFT
        ARG = STEM*FLOAT(II)
        FR(I,7) = COS(ARG)
        FI(I,7) = SIN(ARG)
        DR(I,7) = -CTEM*FI(I,7)
        DI(I,7) = CTEM*FR(I,7)
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 2, NPKGEN(JPHASE,JSOURC) - 1
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CF67 = CFFT(6)*CFFT(7)
        CTM1 = SZ + SZ1*CF67
        CFE = CFFT(2)*CFFT(4)*CFFT(5)
        CFT = CFFT(3)*CFFT(4)*CFFT(5)
        CFE2 = CFE*CF67
        CFT2 = CFT*CF67
        DDT(2) = SW*DFFT(2)*CFFT(4)*CFFT(5)*CTM1
        DDT(3) = SW1*DFFT(3)*CFFT(4)*CFFT(5)*CTM1
        DDT(4) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(4)*CFFT(5)*CTM1
        DDT(5) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(5)*CFFT(4)*CTM1
        CTM2 = SZ1*(SW*CFFT(2)+SW1*CFFT(3))*CFFT(4)*CFFT(5)
        DDT(6) = CTM2*DFFT(6)*CFFT(7)
        DDT(7) = CTM2*CFFT(6)*DFFT(7)
        DO J = 2, NPKGEN(JPHASE,JSOURC) - 1
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
        FRE2(I) = REAL(CFE2)
        FIE2(I) = AIMAG(CFE2)
        FRT2(I) = REAL(CFT2)
        FIT2(I) = AIMAG(CFT2)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRE,FIE)
      CALL FT01A(MNS,INV,FRT,FIT)
      CALL FT01A(MNS,INV,FRE2,FIE2)
      CALL FT01A(MNS,INV,FRT2,FIT2)
      DO J = 2, NPKGEN(JPHASE,JSOURC) - 1
        CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        SFE = SZ*FRE(I) + SZ1*FRE2(I)
        SFT = SZ*FRT(I) + SZ1*FRT2(I)
        PKADD(II,1) = (SW*SFE+SW1*SFT)*XTEM
        PKADD(II,2) = (SFE-SFT)*XTEM
        DO J = 2, NPKGEN(JPHASE,JSOURC) - 1
          JJ = J + 1
          PKADD(II,JJ) = DR(I,J)*XTEM
        ENDDO
        PKADD(II,NPKGEN(JPHASE,JSOURC)+1)                               &
         = ((SW*FRE(I)+SW1*FRT(I))-(SW*FRE2(I)+SW1*FRT2(I)))*XTEM
      ENDDO

      END SUBROUTINE FTSUB5
!
!*****************************************************************************
!
      SUBROUTINE FTSUB8(MNS)
!
! *** FTSUB8 (formerly 4) by WIFD May 89 ***
!
      INCLUDE 'PARAMS.INC'
      COMPLEX CFFT, DFFT, DDT, CFE, CFT
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      INCLUDE 'REFLNS.INC'
      COMMON /SCRAT / CFFT(8), DFFT(8), DDT(8), CFE, CFT, FR(256,8),    &
     &                FI(256,8), DR(256,8), DI(256,8), FRE(256),        &
     &                FIE(256), FRT(256), FIT(256)
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)

      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
      SIG = PKFNVA(4)
      GAM1 = PKFNVA(5)
      GAM2 = PKFNVA(6)
      SW1 = 1. - SW
      C2TEM = PI/(FLOAT(MNS)*XPKDEL(KMOD))
      CTEM = 2.*C2TEM
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
      CLTEM1 = C2TEM*GAM1
      CLTEM2 = C2TEM*GAM2
      MN2 = MNS/2
      MN2M1 = MN2 - 1
      MN2P1 = MN2 + 1
      DO I = 1, MNS
        II = MOD(I+MN2,MNS) - MN2P1
! FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        FR(I,2) = 1./(1.+ARG*ARG)
        FI(I,2) = ARG*FR(I,2)
        ATEMF = CTEM*FLOAT(II)*FR(I,2)**2
        DR(I,2) = -2.*ARG*ATEMF
        DI(I,2) = (1.-ARG*ARG)*ATEMF
! SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
! GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
! LORENTZIAN - STRAIN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM1*AFII
        FR(I,5) = EXP(-ARG)
        FI(I,5) = 0.
        DR(I,5) = -C2TEM*AFII*FR(I,5)
        DI(I,5) = 0.
! LORENTZIAN - PARTICLE SIZE
        AFII = ABS(FLOAT(II))
        ARG = CLTEM2*AFII
        FR(I,6) = EXP(-ARG)
        FI(I,6) = 0.
        DR(I,6) = -C2TEM*AFII*FR(I,6)
        DI(I,6) = 0.
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CFE = CFFT(2)*CFFT(4)*CFFT(5)*CFFT(6)
        CFT = CFFT(3)*CFFT(4)*CFFT(5)*CFFT(6)
        DDT(2) = SW*DFFT(2)*CFFT(4)*CFFT(5)*CFFT(6)
        DDT(3) = SW1*DFFT(3)*CFFT(4)*CFFT(5)*CFFT(6)
        DDT(4) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(4)*CFFT(5)*CFFT(6)
        DDT(5) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(5)*CFFT(4)*CFFT(6)
        DDT(6) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(6)*CFFT(4)*CFFT(5)
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRE,FIE)
      CALL FT01A(MNS,INV,FRT,FIT)
      DO J = 2, NPKGEN(JPHASE,JSOURC)
        CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKADD(II,1) = (SW*FRE(I)+SW1*FRT(I))*XTEM
        PKADD(II,2) = (FRE(I)-FRT(I))*XTEM
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          JJ = J + 1
          PKADD(II,JJ) = DR(I,J)*XTEM
        ENDDO
      ENDDO

      END SUBROUTINE FTSUB8
!
!*****************************************************************************
!
      SUBROUTINE PCCN01(N)
!
! *** PCCN01 updated by JCM 28 Feb 88 ***
!
!H Multiple entry routine to deal with the Peak centre for Constant Wavelength
!A On entry N requests one of the following:
!A N=1: (Not used yet) read L PKCN card
!A N=2: Calculate Peak Centre in ARGK, and its derivatives
!A N=3: (Not used yet) apply shifts
!A N=4: (Not used yet) write new card
!A N=5: Calculate Peak Centre in ARGK alone
!A N=6: Set SRGK=maximum sin theta/lambda, given ARGMAX(JSOURC) = max 2theta.
!A N=7: (Not used yet) set up to use PFXX(7)
!P For entries 2,5, WLGTH must hold lambda, and DSTAR(KNOW) d*
!
      INCLUDE 'PARAMS.INC'
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)

      GOTO (100,2,100,100,2,6,100), N
! N=2OR 5:
! ARGK=ZERO+ARCSIN(......)
    2 TEM = 0.5*WLGTH*DSTAR(KNOW)
! REMEMBER, 2THETA:
      ARGK = ZEROSP(1,JPHASE,JSOURC) + 2.*DEG*ASIN(TEM)
      IF (N.EQ.5) GOTO 100
! CHAIN RULE: D(ARGK)/D(D*)SQ = D(ARGK)/D(D*) * D(D*)/D(D*)SQ
!      DKDDS=0.5*WLGTH/(DSTAR(KNOW)*SQRT(1.-TEM*TEM))
      DKDDS = 0.5*WLGTH*WLGTH/SIN(RAD*ARGK)
! CONVERSION FACTOR...
      DKDDS = DEGREE(DKDDS)
      DKDZER(1) = 1.
      GOTO 100
! MAX SIN THETA/LAMBDA:
    6 STHMXX(JSOURC) = SIN(RADIAN(ARGMAX(JSOURC)/2.))/WLGTH
  100 RETURN
      END SUBROUTINE PCCN01
!
!*****************************************************************************
!
      SUBROUTINE PCLX01(N)
!
! *** PCLX01 updated by WIFD/JCM 28 Feb 88 ***
!
!H Multiple entry routine to deal with Peak centres for X-Ray:
!A On entry N requests one of the following:
!A N=1: (Not yet used) read L PKCN card
!A N=2: calculate Peak Centre, ARGK, and its derivative wrt ZERO
!A N=3: (Not yet used) apply shifts
!A N=4: (Not yet used) write new card
!A N=5: calculate Peak Centre, ARGK
!A N=6: Calculate maximum sin theta/lambda from maximum 2theta
!A N=7: (Not yet used) prepare to call PFXX(7)
!
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /XRAYC / ARGK2

      GOTO (100,2,100,100,2,6,100), N
! N=2 OR 5:
! ARGK=ZERO+ARCSIN(......)
    2 TEM1 = 0.5*ALAMBD(1,JSOURC)*DSTAR(KNOW)
      TEM2 = 0.5*ALAMBD(2,JSOURC)*DSTAR(KNOW)
! REMEMBER, 2THETA:
      ARGK = ZEROSP(1,JPHASE,JSOURC) + 2.*DEGREE(ASIN(TEM1))
      ARGK2 = ZEROSP(1,JPHASE,JSOURC) + 2.*DEGREE(ASIN(TEM2))
      IF (N.EQ.5) GOTO 100
! CHAIN RULE: D(ARGK)/D(D*)SQ = D(ARGK)/D(D*) * D(D*)/D(D*)SQ
      DKDDS = DEGREE(0.5*WLGTH/(DSTAR(KNOW)*SQRT(1.-TEM1*TEM2)))
      DKDZER(1) = 1.
      GOTO 100
! MAX SIN THETA/LAMBDA:
    6 STHMXX(JSOURC) = SIN(RADIAN(ARGMAX(JSOURC)/2.))/WLGTH
  100 RETURN

      END SUBROUTINE PCLX01
!
!*****************************************************************************
!
      SUBROUTINE PCTF01(N)
!
! *** PCTF01 updated by JCM 28 Feb 88 ***
!
!H Multiple entry routine dealing with all aspects of peak centre parameters
!A On entry N requests one of the following:
!A N=1: Read 2 peak centre parameters from an L PKCN card
!A N=2: Calculate Peak Centre, ARGK, i.e. time of flight k, and its
!A      derivatives wrt ZERO, the Peak Centre parameters and the wavelength.
!A N=3: Apply shift to parameter ISPC
!A N=4:Write out new L PKCN card
!A N=5: Calculate Peak Centre, ARGK, i.e. time of flight k
!A N=6: Calculate STHLMX, max sin theta/lambda, from the minimum tof
!A N=7: Calculate WLGTH = lambda given d* in DSTAR(KNOW)
!
!P Entries 2,5 and 7 require DSTAR(KNOW)=d*
!P Entry 6 requires ARGK=minimum tof
!
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
      GOTO (1,2,3,4,2,6,2), N
!
! GIVEN AN 'L PKCN' CARD IN COMMON /SCRACH/, READ REST OF IT:
    1 CALL RDREAL(PKCNSP(1,JPHASE,KSOURC),7,IPT,80,IER)
      CALL RDREAL(PKCNSP(2,JPHASE,KSOURC),IPT,IPT,80,IER)
      WRITE (LPT,2000) PKCNSP(1,JPHASE,KSOURC), PKCNSP(2,JPHASE,KSOURC)
 2000 FORMAT (/' Total flightpath = ',F12.5,' metres '/                 &
     &        ' Apparent flightpath/wavelength variation equivalent to '&
     &        /'  ',F12.5,' microseconds (T-o-f) per Angstrom squared ')
      NPKCSP(JPHASE,KSOURC) = 2
      PCON = 252.777
      GOTO 100
!
! ENTRIES 2,5,7:
    2 WLGTH = TWSNTH(JSOURC)/DSTAR(KNOW)
      IF (N.EQ.7) GOTO 100
      IF (N.EQ.5) GOTO 5
! DERIVATIVE OF T (ARGK) WRT ZERO POINT:
      DKDZER(1) = 1.
! DERIVATIVE OF T (ARGK) WRT FIRST PEAK CENTRE PARAMETER:
      DTDPCN(1) = WLGTH*PCON
! DERIVATIVE OF T (ARGK) WRT SECOND PEAK CENTRE PARAMETER:
      DTDPCN(2) = WLGTH*WLGTH
! DERIVATIVE OF T (ARGK) WRT WAVELENGTH:
      DTDWL = PKCNSP(1,JPHASE,JSOURC)                                   &
     &        *PCON + 2.*WLGTH*PKCNSP(2,JPHASE,JSOURC)
! ENTRY 5: CALCULATE ARGK
    5 ARGK = ZEROSP(1,JPHASE,JSOURC) + PCON*PKCNSP(1,JPHASE,JSOURC)     &
     &       *WLGTH + PKCNSP(2,JPHASE,JSOURC)*WLGTH*WLGTH
      GOTO 100
!
! APPLY SHIFT IN COEFFICIENT:
    3 CALL ADJUST(PKCNSP(ISPC,JPHASE,JSOURC))
      GOTO 100
!
! WRITE OUT NEW 'L PKCN' CARD FOR TOF:
    4 WRITE (NEWIN,2001) PKCNSP(1,JPHASE,JSOURC),                       &
     &                   PKCNSP(2,JPHASE,JSOURC)
 2001 FORMAT ('L PKCN',2F10.4)
      GOTO 100
!
! MAKE STHLMX FROM TMIN (IN ARGK)
    6 STHMXX(JSOURC) = SINTH*PKCNSP(1,JPHASE,JSOURC)                    &
     &                 *PCON/(ARGMIN(JSOURC)-ZEROSP(1,JPHASE,JSOURC))
  100 RETURN
      END SUBROUTINE PCTF01
!
!*****************************************************************************
!
      SUBROUTINE PCTF91(N)
!
! *** PCTF91 updated by JCM 28 Feb 88 ***
!
!H Multiple entry routine dealing with all aspects of peak centre parameters
!A On entry N requests one of the following:
!A N=1: Read 2 peak centre parameters from an L PKCN card
!A N=2: Calculate Peak Centre, ARGK, i.e. time of flight k, and its
!A      derivatives wrt ZERO, the Peak Centre parameters and the wavelength.
!A N=3: Apply shift to parameter ISPC
!A N=4:Write out new L PKCN card
!A N=5: Calculate Peak Centre, ARGK, i.e. time of flight k
!A N=6: Calculate STHLMX, max sin theta/lambda, from the minimum tof
!A N=7: Calculate WLGTH = lambda given d* in DSTAR(KNOW)
!A N=8: Calculate WLGTH = lambda given ARGK
!
!P Entries 2,5 and 7 require DSTAR(KNOW)=d*
!P Entry 6 requires ARGK=minimum tof
!
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
      GOTO (1,2,3,4,2,6,2,8), N
!
! GIVEN AN 'L PKCN' CARD IN COMMON /SCRACH/, READ REST OF IT:
    1 CALL RDREAL(PKCNSP(1,JPHASE,JSOURC),7,IPT,80,IER)
      CALL RDREAL(PKCNSP(2,JPHASE,JSOURC),IPT,IPT,80,IER)
      WRITE (LPT,2000) PKCNSP(1,JPHASE,JSOURC), PKCNSP(2,JPHASE,JSOURC)
 2000 FORMAT (/' Peak Centre Parameters =',2F10.4)
      NPKCSP(JPHASE,JSOURC) = 2
      PCON = 252.777
      GOTO 100
!
! ENTRIES 2,5,7:
    2 WLGTH = TWSNTH(JSOURC)/DSTAR(KNOW)
      IF (N.EQ.7) GOTO 100
      IF (N.EQ.5) GOTO 5
! DERIVATIVE OF T (ARGK) WRT ZERO POINT:
      DKDZER(1) = 1.
! DERIVATIVE OF T (ARGK) WRT FIRST PEAK CENTRE PARAMETER:
      DTDPCN(1) = WLGTH*PCON
! DERIVATIVE OF T (ARGK) WRT SECOND PEAK CENTRE PARAMETER:
      DTDPCN(2) = WLGTH*WLGTH
! DERIVATIVE OF T (ARGK) WRT WAVELENGTH:
      DTDWL = PKCNSP(1,JPHASE,JSOURC)                                   &
     &        *PCON + 2.*WLGTH*PKCNSP(2,JPHASE,JSOURC)
! ENTRY 5: CALCULATE ARGK
    5 ARGK = ZEROSP(1,JPHASE,JSOURC) + PCON*PKCNSP(1,JPHASE,JSOURC)     &
     &       *WLGTH + PKCNSP(2,JPHASE,JSOURC)*WLGTH*WLGTH
      GOTO 100
!
! APPLY SHIFT IN COEFFICIENT:
    3 CALL ADJUST(PKCNSP(ISPC,JPHASE,JSOURC))
      GOTO 100
!
! WRITE OUT NEW 'L PKCN' CARD FOR TOF:
    4 WRITE (NEWIN,2001) PKCNSP(1,JPHASE,JSOURC),                       &
     &                   PKCNSP(2,JPHASE,JSOURC)
 2001 FORMAT ('L PKCN',2F10.4)
      GOTO 100
!
! MAKE STHLMX FROM TMIN (IN ARGK)
    6 STHMXX(JSOURC) = SINTH*PKCNSP(1,JPHASE,JSOURC)                    &
     &                 *PCON/(ARGMIN(JSOURC)-ZEROSP(1,JPHASE,JSOURC))
      GOTO 100
!
! CALCULATES WLGTH GIVEN ARGK : USED IN INITIAL CALCULATION OF
! PEAKSHAPE FOR SUBSEQUENT INTERPOLATION
    8 BTEM = PKCNSP(1,JPHASE,JSOURC)*PCON
      RAT1 = (ARGK-ZEROSP(1,JPHASE,JSOURC))/BTEM
      RAT2 = PKCNSP(2,JPHASE,JSOURC)/BTEM
      WLGTH = RAT1*(1.-RAT1*RAT2)
      GOTO 100
!
  100 RETURN
      END SUBROUTINE PCTF91
!
!*****************************************************************************
!
      SUBROUTINE PFCN01(N)
!
! *** PFCN01 updated by JCM 27 May 89 ***
!
!H Multiple entry routine to do various calculations for constant
!H wavelength data, Gaussian peak
!A On entry N=1,2,5,6, or 7
!A N=1 asks to initialise the program to do CN, peak function 01
!A N=2 form peak function in YNORM, and its derivatives wrt parameters
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a significant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
      PARAMETER (NW=4)
      CHARACTER*4 WDCN01(NW)
      DIMENSION IWCN01(3,NW)
      DATA WDCN01/'SIGM', 'U', 'V', 'W'/
      DATA IWCN01/3, 3, 0, 3, 3, 1, 3, 3, 2, 3, 3, 3/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE CN, PEAK TYPE 01"
   10 NPKGEN(JPHASE,JSOURC) = 1
!*?? SORT OUT NGEN4
      NGEN4(JPHASE,JSOURC) = 2
      LF3SP(3,JPHASE,JSOURC) = 3
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDCN01(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDCN01,IWCN01,NW)
      GOTO 100
!
! N=2: PROFILE REFINEMENT STAGE
    2 CALL FDCN01(2)
      DEL = ARGI - ARGK
      SIGDSQ = 1./PKFNVA(1)
      DELSQ = DEL*DEL
      ARGTEM = DELSQ*SIGDSQ
      ANORM = SQRT(SIGDSQ*0.159154943)
      YNORM = ANORM*EXP(-0.5*ARGTEM)
!
! DYNDKQ IS THE DERIVATIVE OF YNORM WRT TK (ARGK), DIVIDED BY YNORM.  THE 6
! LETTER NAME DOES NOT ALLOW US TO EXPRESS ALL THIS, BUT THE Q IS IMPORTANT
! BECAUSE IT INDICATES 'QUOTIENT', IE THE DERIVATIVE IS DIVIDED BY THE FUNCTION
!
! FIRST, (1/Y)(DY/D(ARGK))  K MEANS WRT ARGK
! THIS COMES FROM (1/Y)*(DY/D(DEL))*(D(DEL)/(D(ARGK))
! THUS, NO NEGATIVE SIGN
      DYNDKQ = DEL*SIGDSQ
! THEN, (1/Y)(DY/D(SIGMA-SQUARED))
      DYNDVQ(1) = 0.5*SIGDSQ*(ARGTEM-1.)
      GOTO 100
!
! N=5: IS REFLECTION IN RANGE?
    5 CALL FDCN01(1)
      DEL = ARGI - ARGK
      TEMSQ = TOLER(1,JPHASE,JSOURC)*PKFNVA(1)
      REFUSE = DEL*DEL.GT.TEMSQ
      GOTO 100
!
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
!
    6 CALL FDCN01(1)
      DELT = ABS(AKLO-AKHI)
      STRKT = DELT.LE.STRTOL*PKFNVA(1)
      SLACK = 0.
      IF (DELT.GE.SLKTOL*PKFNVA(1)) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*PKFNVA(1)))**2)
      GOTO 100
!
    7 CALL FDCN01(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      GOTO 100
!
  100 RETURN
      END SUBROUTINE PFCN01
!
!*****************************************************************************
!
      SUBROUTINE PFLX01(N)
!
! *** PFLX01 updated by JCM 27 May 89 ***
!
!H Multiple entry routine to do various calculations for X-ray data,
!H Gaussian peak
!
!A On entry N=1,2,5,6, or 7
!A N=1: Initialise program to do data source LX, peak function 01
!A N=2 form peak function in YNORM, and its derivatives wrt paramet ers
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /XRAYC / ARGK2
      PARAMETER (NW=5)
      CHARACTER*4 WDLX01(NW)
      DIMENSION IWLX01(3,NW)
      DATA WDLX01/'SIGM', 'U', 'V', 'W', 'TTHM'/
      DATA IWLX01/3, 3, 0, 3, 3, 1, 3, 3, 2, 3, 3, 3, 6, 1, 2/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE LX, PEAK TYPE 01"
   10 NPKGEN(JPHASE,JSOURC) = 1
      NGEN4(JPHASE,JSOURC) = 2
      LF3SP(3,JPHASE,JSOURC) = 3
      LF6SP(1,JSOURC) = 2
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDLX01(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDLX01,IWLX01,NW)
      GOTO 100
!
! N=2: PROFILE REFINEMENT STAGE
    2 CALL FDLX01(2)
      DEL = ARGI - ARGK
      SIGDSQ = 1./PKFNVA(1)
      DELSQ = DEL*DEL
      ARGTEM = DELSQ*SIGDSQ
      DEL2 = ARGI - ARGK2
      DELSQ2 = DEL2*DEL2
      ARGTM2 = DELSQ2*SIGDSQ
      EXPON = EXP(-0.5*ARGTEM)
      EXPON2 = EXP(-0.5*ARGTM2)
      ANORM = SQRT(SIGDSQ*0.159154943)
      YNORM = ANORM*(EXPON+0.5*EXPON2)
!
! DYNDKQ IS THE DERIVATIVE OF YNORM WRT TK (ARGK), DIVIDED BY YNORM.  THE 6
! LETTER NAME DOES NOT ALLOW US TO EXPRESS ALL THIS, BUT THE Q IS IMPORTANT
! BECAUSE IT INDICATES 'QUOTIENT', IE THE DERIVATIVE IS DIVIDED BY THE FUNCTION
! BUT BE CAREFUL FOR ZERO YNORM
      IF (YNORM.EQ.0.) THEN
        DYNDKQ = 0.
        DYNDVQ(1) = 0.
      ELSE
! FIRST, (1/Y)(DY/D(ARGK))  K MEANS WRT ARGK
! THIS COMES FROM (1/Y)*(DY/D(DEL))*(D(DEL)/(D(ARGK))
! THUS, NO NEGATIVE SIGN
        ATEM = 0.5*RADIAN(ARGK-ZEROSP(1,JPHASE,JSOURC))
        RTEM = ALAMBD(2,JSOURC)/ALAMBD(1,JSOURC)
        UTEM = RTEM*SIN(ATEM)
        DX2DXK = RTEM*COS(ATEM)/SQRT(1.-UTEM*UTEM)
        DYNDKQ = (ANORM*SIGDSQ*(DEL*EXPON+0.5*DEL2*EXPON2*DX2DXK))/YNORM
! THEN, (1/Y)(DY/D(SIGMA-SQUARED))
        BTEM = (ANORM*(ARGTEM*EXPON+0.5*ARGTM2*EXPON2))/YNORM
        DYNDVQ(1) = 0.5*SIGDSQ*(BTEM-1.)
      ENDIF
      GOTO 100
!
! N=5: IS REFLECTION IN RANGE?
!
    5 CALL FDLX01(1)
      DEL = ARGI - ARGK
      DEL2 = ARGI - ARGK2
      TEMSQ = TOLER(1,JPHASE,JSOURC)*PKFNVA(1)
      IF (DEL.LE.0.) THEN
        REFUSE = DEL*DEL.GT.TEMSQ
      ELSE
        IF (DEL2.LE.0.) THEN
          REFUSE = .FALSE.
        ELSE
          REFUSE = DEL2*DEL2.GT.TEMSQ
        ENDIF
      ENDIF
!
      GOTO 100
!
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
!
    6 CALL FDLX01(1)
      DELT = ABS(AKLO-AKHI)
      STRKT = DELT.LE.STRTOL*PKFNVA(1)
      SLACK = 0.
      IF (DELT.GE.SLKTOL*PKFNVA(1)) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*PKFNVA(1)))**2)
      GOTO 100
!
    7 CALL FDLX01(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      GOTO 100
!
  100 RETURN
      END SUBROUTINE PFLX01
!
!*****************************************************************************
!
      SUBROUTINE PFLX02(N)
!
! *** PFLX02 updated by JCM 27 May 89 ***
!
!H Multiple entry routine for various calculations for X-Ray data,
!H Voigt peak function
!
!A On entry N=1,2,5,6, or 7
!A N=1: Set up program for data source LX, peak function 02
!A N=2: form peak function in YNORM, and its derivatives wrt paramet ers
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      COMMON /DGEOM / IGEOM, UM(9), NLR, ANGLIN(3), ALAMBD(5,5), NLAMB, &
     &                ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /XRAYC / ARGK2
      PARAMETER (NW=6)
      CHARACTER*4 WDLX02(NW)
      DIMENSION IWLX02(3,NW)
      DATA WDLX02/'SIGM', 'GAMM', 'U', 'V', 'W', 'TTHM'/
      DATA IWLX02/3, 3, 0, 3, 4, 0, 3, 3, 1, 3, 3, 2, 3, 3, 3, 6, 1, 2/
!
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE LX, PEAK TYPE 02"
   10 NPKGEN(JPHASE,JSOURC) = 2
      NGEN4(JPHASE,JSOURC) = 2
      LF3SP(3,JPHASE,JSOURC) = 3
      LF3SP(4,JPHASE,JSOURC) = -2
      LF6SP(1,JSOURC) = 2
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDLX02(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDLX02,IWLX02,NW)
      GOTO 100
!
! N=2: PROFILE REFINEMENT STAGE
    2 CALL FDLX02(2)
      DEL1 = ARGI - ARGK
      DEL2 = ARGI - ARGK2
      SIGMA = SQRT(PKFNVA(1))
      GAMMA = PKFNVA(2)
      CALL VOIGT(DEL1,SIGMA,GAMMA,YVAL1,DERX1,DERS1,DERG1)
      CALL VOIGT(DEL2,SIGMA,GAMMA,YVAL2,DERX2,DERS2,DERG2)
      YNORM = YVAL1 + 0.5*YVAL2
!
! DYNDKQ IS THE DERIVATIVE OF YNORM WRT TK (ARGK), DIVIDED BY YNORM.  THE 6
! LETTER NAME DOES NOT ALLOW US TO EXPRESS ALL THIS, BUT THE Q IS IMPORTANT
! BECAUSE IT INDICATES 'QUOTIENT', IE THE DERIVATIVE IS DIVIDED BY THE FUNCTION
! BUT BE CAREFUL FOR ZERO YNORM
      IF (YNORM.EQ.0.) THEN
        DYNDKQ = 0.
        DYNDVQ(1) = 0.
        DYNDVQ(2) = 0.
      ELSE
! FIRST, (1/Y)(DY/D(ARGK))  K MEANS WRT ARGK
! THIS COMES FROM (1/Y)*(DY/D(DEL))*(D(DEL)/(D(ARGK))
! THUS, NO NEGATIVE SIGN
        ATEM = 0.5*RADIAN(ARGK-ZEROSP(1,JPHASE,JSOURC))
        RTEM = ALAMBD(2,JSOURC)/ALAMBD(1,JSOURC)
        UTEM = RTEM*SIN(ATEM)
        DX2DXK = RTEM*COS(ATEM)/SQRT(1.-UTEM*UTEM)
        DYNDKQ = -(DERX1+0.5*DX2DXK*DERX2)/YNORM
! THEN, (1/Y)(DY/D(SIGMA-SQUARED))
        DYNDVQ(1) = (DERS1+0.5*DERS2)/(2.*SIGMA*YNORM)
        DYNDVQ(2) = (DERG1+0.5*DERG2)/YNORM
      ENDIF
      GOTO 100
!
! N=5: IS REFLECTION IN RANGE?
!
    5 CALL FDLX02(1)
      DEL1 = ARGI - ARGK
      DEL2 = ARGI - ARGK2
      TEMSQ = TOLER(1,JPHASE,JSOURC)*PKFNVA(1)
      TEMGA = (TOLER(2,JPHASE,JSOURC)*PKFNVA(2))**2
      IF (DEL1.LE.0.) THEN
        REFUSE = DEL1*DEL1.GT.(TEMSQ+TEMGA)
      ELSE
        IF (DEL2.LE.0.) THEN
          REFUSE = .FALSE.
        ELSE
          REFUSE = DEL2*DEL2.GT.(TEMSQ+TEMGA)
        ENDIF
      ENDIF
!
      GOTO 100
!
!
    6 CALL FDLX02(1)
      DELT = ABS(AKLO-AKHI)
      STRKT = DELT.LE.STRTOL*PKFNVA(1)
      SLACK = 0.
      IF (DELT.GE.SLKTOL*PKFNVA(1)) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*PKFNVA(1)))**2)
      GOTO 100
!
    7 CALL FDLX02(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      GOTO 100
!
  100 RETURN
      END SUBROUTINE PFLX02
!
!*****************************************************************************
!
      SUBROUTINE PFTF01(N)
!
! *** PFTF01 updated by JCM 27 May 89 ***
!
!H Multiple entry Time of Flight Peak Function routine with FFT
!H With Gaussian double exponential Peak Function
!A Multiple entry, dependent on N:
!A N=1: Set up program for data source TOF, peak function type 01
!A N=2: Calculate normalised peak function to YNORM, and its derivatives wrt
!A      anything which could be a parameter.
!
!A N=5: Sets REFUSE to say whether or not given reflection is to be used
!A      i.e. whether this reflection is near enough to this intensity to
!A      contribute to it.
!
!A N=6: CAILS entry to select slack or strict
!
!A N=7: SAPS entry to obey FDTFXX to set up a value for PFFNVA
!A      to hold SIGM
!
!
      INCLUDE 'PARAMS.INC'
      COMPLEX CFFT, DFFT, DDT, CFE, CFT
      LOGICAL TESTOV
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /SCRAT / CFFT(8), DFFT(8), DDT(8), CFE, CFT, FR(256,8),    &
     &                FI(256,8), DR(256,8), DI(256,8), FRE(256),        &
     &                FIE(256), FRT(256), FIT(256), C3FN(3), C3DN(3)
      PARAMETER (NW=4)
      CHARACTER*4 WDTF01(NW)
      DIMENSION IWTF01(3,NW)
      DATA WDTF01/'SWCH', 'TAUF', 'TAUS', 'SIGM'/
      DATA IWTF01/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE TOF, PEAK TYPE 01"
   10 NPKGEN(JPHASE,JSOURC) = 4
      NGEN4(JPHASE,JSOURC) = 2
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF01(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF01,IWTF01,NW)
      GOTO 100
!
! PROFILE REFINEMENT STAGE:
    2 CALL FDTF01(2)
      MN = 64
      MN2 = MN/2
!
!.. FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
!.. ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
!.. DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
      IF (REFUSE) GOTO 26
!
!.. THE PEAK HAS NOT OCCURRED ALREADY - CALCULATE THE COMPLETE PEAK
!.. SHAPE OF THE KTH PEAK BY FFT.
!
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
!.. FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      XPKDEL(KMOD) = SQRT(TEMSQ)/FLOAT(MN2)
!
!.. NOW SET UP FAST FOURIER TRANSFORM
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
!
      SW = PKFNVA(1)
      TAUF = PKFNVA(2)
      TAUS = PKFNVA(3)
      SIG = PKFNVA(4)
      SW1 = 1. - SW
!
      CTEM = TWOPI/(FLOAT(MN)*XPKDEL(KMOD))
      FTEMF = CTEM*TAUF
      FTEMS = CTEM*TAUS
      GTEM = CTEM*SIG
!
      MN2P1 = MN2 + 1
      DO I = 1, MN
        II = MOD(I+MN2,MN) - MN2P1
!.. FAST EXPONENTIAL DECAY
        ARG = FTEMF*FLOAT(II)
        FR(I,2) = 1./(1.+ARG*ARG)
        FI(I,2) = ARG*FR(I,2)
        ATEMF = CTEM*FLOAT(II)*FR(I,2)**2
        DR(I,2) = -2.*ARG*ATEMF
        DI(I,2) = (1.-ARG*ARG)*ATEMF
!.. SLOW EXPONENTIAL DECAY
        ARG = FTEMS*FLOAT(II)
        FR(I,3) = 1./(1.+ARG*ARG)
        FI(I,3) = ARG*FR(I,3)
        ATEMS = CTEM*FLOAT(II)*FR(I,3)**2
        DR(I,3) = -2.*ARG*ATEMS
        DI(I,3) = (1.-ARG*ARG)*ATEMS
!.. GAUSSIAN
        ARG = GTEM*FLOAT(II)
        FR(I,4) = EXP(-0.5*ARG*ARG)
        FI(I,4) = 0.
        DR(I,4) = -ARG*ARG*FR(I,4)/SIG
        DI(I,4) = 0.
      ENDDO
!
!.. NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MN
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        CFE = CFFT(2)*CFFT(4)
        CFT = CFFT(3)*CFFT(4)
        DDT(2) = SW*DFFT(2)*CFFT(4)
        DDT(3) = SW1*DFFT(3)*CFFT(4)
        DDT(4) = (SW*CFFT(2)+SW1*CFFT(3))*DFFT(4)
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        FRE(I) = REAL(CFE)
        FIE(I) = AIMAG(CFE)
        FRT(I) = REAL(CFT)
        FIT(I) = AIMAG(CFT)
      ENDDO
!
!.. DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MN,INV,FRE,FIE)
      CALL FT01A(MN,INV,FRT,FIT)
!
      DO J = 2, NPKGEN(JPHASE,JSOURC)
        CALL FT01A(MN,INV,DR(1,J),DI(1,J))
      ENDDO
!.. WRITE FUNCTION AND DERIVATIVES TO ARRAY PKLIST
      XTEM = 1./XPKDEL(KMOD)
      DO I = 1, MN
        II = MOD(I+MN2,MN)
        PKLIST(II,1,KMOD) = (SW*FRE(I)+SW1*FRT(I))*XTEM
        PKLIST(II,2,KMOD) = (FRE(I)-FRT(I))*XTEM
        DO J = 2, NPKGEN(JPHASE,JSOURC)
          JJ = J + 1
          PKLIST(II,JJ,KMOD) = DR(I,J)*XTEM
        ENDDO
      ENDDO
!
!.. FFT OVER
!
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
   26 JARGI = NINT((ARGI-ARGK)/XPKDEL(KMOD))
      IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = (ARGI-ARGK)/XPKDEL(KMOD) - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
!
      YNORM = 0.
      DYNDKQ = 0.
      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
!
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
!
!.. NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
      ENDIF
      GOTO 100
!
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF01(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      IF (DEL) 510, 510, 520
  520 AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
  510 REFUSE = DEL*DEL.GT.TEMSQ
      GOTO 100
!
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
!
    6 CALL FDTF01(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
!
! ENTRY 7 IS A WAY OF CALLING A SPECIFIC FDXX MATCHING PFXX.  ITS
! CALL IS PRECEDED BY A CALL TO PCXX(5) TO GET WLGTH FOR TOF,
! AND IT EXPECTS KNOW TO HOLD A CURRENT K:
    7 CALL FDTF01(1)
      F4PAR(2,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN
      END SUBROUTINE PFTF01
!
!*****************************************************************************
!
      SUBROUTINE PFTF02(N)
!
! *** PFTF02 updated by JCM 27 May 89 ***
!
!H Multiple entry Time of Flight Peak Function routine with FFT
!H and Voigt double exponential peak function
!A On entry N=1,2,5,6 or 7:
!A N=1: Set up program for data source TOF, peak function 02:
!A N=2: form peak function in YNORM, and its derivatives wrt paramet ers
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      LOGICAL TESTOV
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /FFCONS/ C1D45, C20D45, C64D45, C1D3, C4D3
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /SCRAT / C3FN(3), C3DN(3)
      PARAMETER (NW=5)
      CHARACTER*4 WDTF02(NW)
      DIMENSION IWTF02(3,NW)
      DATA WDTF02/'SWCH', 'TAUF', 'TAUS', 'SIGM', 'GAMM'/
      DATA IWTF02/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0, 3, 7, 0/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE TOF, PEAK TYPE 02"
   10 NPKGEN(JPHASE,JSOURC) = 5
      NGEN4(JPHASE,JSOURC) = 3
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF3SP(7,JPHASE,JSOURC) = -3
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF02(I)
      ENDDO
      C1D45 = 1./45.
      C20D45 = 20./45.
      C64D45 = 64./45.
      C1D3 = 1./3.
      C4D3 = 4./3.
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF02,IWTF02,NW)
      GOTO 100
!
! PROFILE REFINEMENT STAGE:
    2 CALL FDTF02(2)
!.. FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
!.. ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
!.. DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
      IF (REFUSE) GOTO 26
!
!.. THE PEAK HAS NOT OCCURRED ALREADY - CALCULATE THE COMPLETE PEAK
!.. SHAPE OF THE KTH PEAK BY FFT.
!
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
!.. FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
      MN = 64
      MN2 = MN/2
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      XPKDEL(KMOD) = (SQRT(TEMSQ)+CAULIM)/FLOAT(MN2)
!
!.. NOW SET UP FAST FOURIER TRANSFORM
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
!
!*      IEAGLE=NFFT
!*      IF (IEAGLE .EQ. 0) IEAGLE=NINT(TOLER(1,JPHASE,JSOURC))
      IEAGLE = NINT(TOLER(1,JPHASE,JSOURC))
      IF (IEAGLE.EQ.3) THEN
        MNS = MN
        CALL FTSUB2(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = (C1D45)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB2(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) - (C20D45)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 4*MN
        CALL FTSUB2(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (C64D45)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSEIF (IEAGLE.EQ.2) THEN
        MNS = MN
        CALL FTSUB2(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = -(C1D3)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB2(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (C4D3)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSE
        MNS = MN
        CALL FTSUB2(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKADD(II,J)
          ENDDO
        ENDDO
      ENDIF
!.. FFT OVER
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
   26 JARGI = NINT((ARGI-ARGK)/XPKDEL(KMOD))
      IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = (ARGI-ARGK)/XPKDEL(KMOD) - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
      YNORM = 0.
      DYNDKQ = 0.
      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
! NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
      ENDIF
      GOTO 100
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF02(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      IF (DEL) 510, 510, 520
  520 AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
  510 TEMLIM = CAULIM + SQRT(TEMSQ)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDTF02(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
    7 CALL FDTF02(1)
      F4PAR(2,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFTF02
!
!*****************************************************************************
!
      SUBROUTINE PFTF03(N)
!
! *** PFTF03 updated by JCM 29 May 89 ***
!
!H Multiple entry Time of Flight Peak Function routine with FFT
!H and Peak Function with Voigt/double exponential/truncated quadratic
!H convolution
!A On entry N=1,2,5,6 or 7:
!A N=1: Set up program for data source TOF, peak function 03:
!A N=2: form peak function in YNORM, and its derivatives wrt paramet ers
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
      INCLUDE 'PARAMS.INC'

      LOGICAL TESTOV
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      PARAMETER (NW=6)
      COMMON /SCRAT / C3FN(3), C3DN(3)
      CHARACTER*4 WDTF03(NW)
      DIMENSION IWTF03(3,NW)
      DATA WDTF03/'SWCH', 'TAUF', 'TAUS', 'SIGM', 'GAMM', 'CANI'/
      DATA IWTF03/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0, 3, 7, 0, 3, 8, 0/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
! N=0: SET UP "DATA SOURCE TOF, PEAK TYPE 03"
   10 NPKGEN(JPHASE,JSOURC) = 6
      NGEN4(JPHASE,JSOURC) = 2
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF3SP(7,JPHASE,JSOURC) = -3
      LF3SP(8,JPHASE,JSOURC) = -1
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF03(I)
      ENDDO
      GOTO 100
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF03,IWTF03,NW)
      GOTO 100
! PROFILE REFINEMENT STAGE:
    2 CALL FDTF03(2)
! FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
! ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
! DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
      IF (REFUSE) GOTO 26
! THE PEAK HAS NOT OCCURRED ALREADY - CALCULATE THE COMPLETE PEAK
! SHAPE OF THE KTH PEAK BY FFT.
!
! FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
! FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
      MN = 64
      MN2 = MN/2
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5) + TOLER(6,JPHASE,JSOURC)&
     &         *PKFNVA(6)
      XPKDEL(KMOD) = (SQRT(TEMSQ)+CAULIM)/FLOAT(MN2)
! NOW SET UP FAST FOURIER TRANSFORM
! THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
! DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
      IEAGLE = NFFT
      IF (IEAGLE.EQ.0) IEAGLE = NINT(TOLER(1,JPHASE,JSOURC))
      IF (IEAGLE.EQ.3) THEN
        MNS = MN
        CALL FTSUB3(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = (1./45.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB3(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) - (20./45.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 4*MN
        CALL FTSUB3(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (64./45.)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSEIF (IEAGLE.EQ.2) THEN
        MNS = MN
        CALL FTSUB3(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = -(1./3.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB3(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (4./3.)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSE
        MNS = MN
        CALL FTSUB3(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKADD(II,J)
          ENDDO
        ENDDO
      ENDIF
! FFT OVER
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
   26 JARGI = NINT((ARGI-ARGK)/XPKDEL(KMOD))
      IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = (ARGI-ARGK)/XPKDEL(KMOD) - FLOAT(JARGI)
! WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
      YNORM = 0.
      DYNDKQ = 0.
      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
!.. NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
      ENDIF
      GOTO 100
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF03(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5) + TOLER(6,JPHASE,JSOURC)&
     &         *PKFNVA(6)
      IF (DEL) 510, 510, 520
  520 AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
  510 TEMLIM = CAULIM + SQRT(TEMSQ)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDTF03(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
    7 CALL FDTF03(1)
      F4PAR(2,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFTF03
!
!*****************************************************************************
!
      SUBROUTINE PFTF04(N)
!
! *** PFTF04 updated by WIFD/JCM 19 Jul 88 ***
!
!X
!C 19B
!H Multiple entry Time of Flight Peak Function (type 04) routine with FFT.
!A On entry N=1 means read and interpret a L PKFN card.  These cards have a
!A second word, one of a vocabulary of:
!A     SWCH
!A     TAUF
!A     TAUS
!A     SIGM
!A     GAMM
!A     POWR
!A followed by the width parameters involved in this particular peak
!A descriptor.
!A
!A Entry N=2 forms the normalised peak function, and puts it in YNORM.  It also
!A gives all necessary derivatives of YNORM with respect to anything that could
!A be a parameter.
!A
!A Entry N=3 applies a shift to genus IGEN, species ISPC.
!A
!A Entry N=4 outputs a new L PKFN card.
!A
!A Entry N=5 sets LOGICAL REFUSE to say whether or not reflection to be used,
!A i.e. whether this reflection is near enough to this intensity to contribute
!A to it.
!
!
      INCLUDE 'PARAMS.INC'
      PARAMETER (NW=6)
      CHARACTER*4 WDTF04(NW)
      LOGICAL TESTOV
      DIMENSION C3FN(3), C3DN(3), IWTF04(3,NW)
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9),       &
     &                ICDN(26,9), IERR, IO10, SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      DATA WDTF04/'SWCH', 'TAUF', 'TAUS', 'SIGM', 'GAMM', 'POWR'/
      DATA IWTF04/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0, 3, 7, 0, 3, 8, 0/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE TOF, PEAK TYPE 04"
   10 NPKGEN(JPHASE,JSOURC) = 6
      NGEN4(JPHASE,JSOURC) = 3
      LF3SP(3,JPHASE,JSOURC) = -1
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF3SP(7,JPHASE,JSOURC) = -3
      LF3SP(8,JPHASE,JSOURC) = -1
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF04(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF04,IWTF04,NW)
      GOTO 100
!
! PROFILE REFINEMENT STAGE:
    2 CALL FDTF04(2)
!
!.. FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
!.. ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
!.. DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
      IF (REFUSE) GOTO 26
!
!.. THE PEAK HAS NOT OCCURRED ALREADY - CALCULATE THE COMPLETE PEAK
!.. SHAPE OF THE KTH PEAK BY FFT.
!
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
!.. FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
      MN = 64
      MN = 64
      MN2 = MN/2
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      AVTAU = TOLER(2,JPHASE,JSOURC)*PKFNVA(2) + PKFNVA(1)              &
     &        *TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      XPKDEL(KMOD) = (SQRT(TEMSQ)+CAULIM)/FLOAT(MN2)
!
!.. NOW SET UP FAST FOURIER TRANSFORM
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
!
      IEAGLE = NINT(TOLER(1,JPHASE,JSOURC))
      IF (IEAGLE.EQ.3) THEN
        MNS = MN
        CALL FTSUB4(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = (1./45.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB4(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) - (20./45.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 4*MN
        CALL FTSUB4(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (64./45.)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSEIF (IEAGLE.EQ.2) THEN
        MNS = MN
        CALL FTSUB4(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = -(1./3.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB4(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (4./3.)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSE
        MNS = MN
        CALL FTSUB4(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKADD(II,J)
          ENDDO
        ENDDO
      ENDIF
!
!
!.. FFT OVER
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
   26 JARGI = NINT((ARGI-ARGK)/XPKDEL(KMOD))
      IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = (ARGI-ARGK)/XPKDEL(KMOD) - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
!
      YNORM = 0.
      DYNDKQ = 0.
      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
!
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
!
!.. NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
      ENDIF
      GOTO 100
!
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF04(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      IF (DEL) 510, 510, 520
  520 AVTAU = TOLER(2,JPHASE,JSOURC)*PKFNVA(2) + PKFNVA(1)              &
     &        *TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
! 520  AVTAU= PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2) + (1.-PKFNVA(1))*
!     +       TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
  510 TEMLIM = CAULIM + SQRT(TEMSQ)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
!
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
!
    6 CALL FDTF04(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = TOLER(2,JPHASE,JSOURC)*PKFNVA(2) + PKFNVA(1)              &
     &        *TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
!      AVTAU= PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + PKFNVA(5) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
!
    7 CALL FDTF04(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      F4PAR(3,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFTF04
!
!*****************************************************************************
!
      SUBROUTINE PFTF05(N)
!
! *** PFTF05 updated by JCM 27 May 89 ***
!
!H Multiple entry Time of Flight Peak Function routine with FFT
!H and Voigt double exponential peak function
!A On entry N=1,2,5,6 or 7:
!A N=1: Set up program for data source TOF, peak function 05:
!A N=2: form peak function in YNORM, and its derivatives wrt paramet ers
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      LOGICAL TESTOV
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /FFCONS/ C1D45, C20D45, C64D45, C1D3, C4D3
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /SCRAT / C3FN(3), C3DN(3)
      PARAMETER (NW=8)
      CHARACTER*4 WDTF05(NW)
      DIMENSION IWTF05(3,NW)
      DATA WDTF05/'SWCH', 'TAUF', 'TAUS', 'SIGM', 'GAMM', 'ESIG',       &
     &     'SHFT', 'SIZE'/
      DATA IWTF05/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0, 3, 7, 0, 3, 8, 0, &
     &     3, 9, 0, 3, 10, 0/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE TOF, PEAK TYPE 05"
   10 NPKGEN(JPHASE,JSOURC) = 8
      NGEN4(JPHASE,JSOURC) = 3
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF3SP(7,JPHASE,JSOURC) = -3
      LF3SP(8,JPHASE,JSOURC) = -2
      LF3SP(9,JPHASE,JSOURC) = -2
      LF3SP(10,JPHASE,JSOURC) = -1
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF05(I)
      ENDDO
      C1D45 = 1./45.
      C20D45 = 20./45.
      C64D45 = 64./45.
      C1D3 = 1./3.
      C4D3 = 4./3.
      GOTO 100
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF05,IWTF05,NW)
      GOTO 100
! PROFILE REFINEMENT STAGE:
    2 CALL FDTF05(2)
!.. FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
!.. ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
!.. DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
      IF (REFUSE) GOTO 26
!.. THE PEAK HAS NOT OCCURRED ALREADY - CALCULATE THE COMPLETE PEAK
!.. SHAPE OF THE KTH PEAK BY FFT.
!
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
!.. FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
      MN = 64
      MN2 = MN/2
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      XPKDEL(KMOD) = (SQRT(TEMSQ)+CAULIM+ABS(PKFNVA(7)))/FLOAT(MN2)
!
!.. NOW SET UP FAST FOURIER TRANSFORM
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
!
!*      IEAGLE=NFFT
!*      IF (IEAGLE .EQ. 0) IEAGLE=NINT(TOLER(1,JPHASE,JSOURC))
      IEAGLE = NINT(TOLER(1,JPHASE,JSOURC))
      IF (IEAGLE.EQ.3) THEN
        MNS = MN
        CALL FTSUB5(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = (C1D45)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB5(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) - (C20D45)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 4*MN
        CALL FTSUB5(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (C64D45)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSEIF (IEAGLE.EQ.2) THEN
        MNS = MN
        CALL FTSUB5(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = -(C1D3)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB5(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (C4D3)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSE
        MNS = MN
        CALL FTSUB5(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKADD(II,J)
          ENDDO
        ENDDO
      ENDIF
!.. FFT OVER
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
   26 JARGI = NINT((ARGI-ARGK)/XPKDEL(KMOD))
      IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = (ARGI-ARGK)/XPKDEL(KMOD) - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
!
      YNORM = 0.
      DYNDKQ = 0.
      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
!
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
!.. NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
      ENDIF
      GOTO 100
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF05(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      TEMSQ2 = TEMSQ + (TOLER(6,JPHASE,JSOURC)*PKFNVA(6))**2
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      IF (DEL) 510, 510, 520
  520 AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      IF (PKFNVA(7).GT.0.) CAULIM = CAULIM + PKFNVA(7)
  510 IF (PKFNVA(7).LE.0.) CAULIM = CAULIM - PKFNVA(7)
      TEMLIM = CAULIM + SQRT(TEMSQ)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDTF05(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
!
    7 CALL FDTF05(1)
      F4PAR(2,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFTF05
!
!*****************************************************************************
!
      SUBROUTINE PFTF08(N)
!
! *** PFTF08 (formerly 04) updated by JCM 29 May 89 ***
!
!H Multiple entry Time of Flight Peak Function routine with FFT
!H and peak function with Voigt/double exponential convolution where the
!H Lorentzian contribution ofthe Voigt has the facility for refining
!H anisotropic strain and particle size effects
!A On entry N=1,2,5,6 or 7:
!A N=1: Set up program for data source TOF, peak function 08
!A N=2: form peak function in YNORM, and its derivatives wrt paramet ers
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      LOGICAL TESTOV
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /SCRAT / C3FN(3), C3DN(3)
      PARAMETER (NW=6)
      CHARACTER*4 WDTF08(NW)
      DIMENSION IWTF08(3,NW)
      DATA WDTF08/'SWCH', 'TAUF', 'TAUS', 'SIGM', 'GSTR', 'GPSZ'/
      DATA IWTF08/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0, 3, 7, 0, 3, 8, 0/
!
      GOTO (10,1,2,100,100,5,6,7), N + 1
!
! N=0: SET UP "DATA SOURCE TOF, PEAK TYPE 08"
   10 NPKGEN(JPHASE,JSOURC) = 6
      NGEN4(JPHASE,JSOURC) = 3
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF3SP(7,JPHASE,JSOURC) = -6
      LF3SP(8,JPHASE,JSOURC) = -6
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF08(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF08,IWTF08,NW)
      GOTO 100
!
! PROFILE REFINEMENT STAGE:
    2 CALL FDTF08(2)
!.. FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
!.. ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
!.. DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
      IF (REFUSE) GOTO 26
!
!.. THE PEAK HAS NOT OCCURRED ALREADY - CALCULATE THE COMPLETE PEAK
!.. SHAPE OF THE KTH PEAK BY FFT.
!
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
!.. FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
      MN = 64
      MN2 = MN/2
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5) + TOLER(6,JPHASE,JSOURC)&
     &         *PKFNVA(6)
      XPKDEL(KMOD) = (SQRT(TEMSQ)+CAULIM)/FLOAT(MN2)
!
!.. NOW SET UP FAST FOURIER TRANSFORM
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
!
      IEAGLE = NFFT
      IF (IEAGLE.EQ.0) IEAGLE = NINT(TOLER(1,JPHASE,JSOURC))
      IF (IEAGLE.EQ.3) THEN
        MNS = MN
        CALL FTSUB8(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = (1./45.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB8(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) - (20./45.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 4*MN
        CALL FTSUB8(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (64./45.)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSEIF (IEAGLE.EQ.2) THEN
        MNS = MN
        CALL FTSUB8(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = -(1./3.)*PKADD(II,J)
          ENDDO
        ENDDO
        MNS = 2*MN
        CALL FTSUB8(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKLIST(I,J,KMOD) + (4./3.)*PKADD(II,J)
          ENDDO
        ENDDO
      ELSE
        MNS = MN
        CALL FTSUB8(MNS)
        II = MNS/2 - MN2
        DO I = 1, MN
          II = II + 1
          DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
            PKLIST(I,J,KMOD) = PKADD(II,J)
          ENDDO
        ENDDO
      ENDIF
!
!.. FFT OVER
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
   26 JARGI = NINT((ARGI-ARGK)/XPKDEL(KMOD))
      IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = (ARGI-ARGK)/XPKDEL(KMOD) - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
      YNORM = 0.
      DYNDKQ = 0.
      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
!.. NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
      ENDIF
      GOTO 100
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF08(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5) + TOLER(6,JPHASE,JSOURC)&
     &         *PKFNVA(6)
      IF (DEL) 510, 510, 520
  520 AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
  510 TEMLIM = CAULIM + SQRT(TEMSQ)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDTF08(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + PKFNVA(5) + PKFNVA(6) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
    7 CALL FDTF08(1)
      F4PAR(2,KNOW) = PKFNVA(5) + PKFNVA(6)
      F4PAR(3,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFTF08
!
!*****************************************************************************
!
      SUBROUTINE PFTF92(N)
!
! *** PFTF92 updated by JCM 27 May 89 ***
!
!H Multiple entry Time of Flight Peak Function routine with FFT
!H and Voigt double exponential peak function
!A On entry N=1,2,3,5,6 or 7:
!A N=1: Set up program for data source TOF, peak function 02:
!A N=2: form peak function in YNORM, and its derivatives wrt paramet ers
!A N=3: form peak function for reference positions for future interpolation
!A      if appropriate.
!A N=5: Sets LOGICAL REFUSE to determine if reflection makes a sign ificant
!A      contribution to the profile at ARGI
!A N=6: CAILS entry to determine whether near reflections should be related
!A      by a strict or slack relationship.
!A N=7: SAPS entry to obey FDXXXX to set up a value(s) for PKFNVA
!
!
      INCLUDE 'PARAMS.INC'
      LOGICAL TESTOV
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVF/ PKLIST(256,9,200), XPKDEL(200), PKADD(256,9),     &
     &                ARGNOT(50), PKNOT(64,9,50), XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      COMMON /SCRAT / C3FN(3), C3DN(3)
      PARAMETER (NW=5)
      CHARACTER*4 WDTF02(NW)
      DIMENSION IWTF02(3,NW)
      DATA WDTF02/'SWCH', 'TAUF', 'TAUS', 'SIGM', 'GAMM'/
      DATA IWTF02/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0, 3, 7, 0/
!
      GOTO (10,1,2,3,100,5,6,7), N + 1
!
! N=1: SET UP "DATA SOURCE TOF, PEAK TYPE 02"
   10 NPKGEN(JPHASE,JSOURC) = 5
      NGEN4(JPHASE,JSOURC) = 3
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -2
      LF3SP(6,JPHASE,JSOURC) = -3
      LF3SP(7,JPHASE,JSOURC) = -3
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDTF02(I)
      ENDDO
      GOTO 100
!
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDTF02,IWTF02,NW)
      GOTO 100
!
! PROFILE REFINEMENT STAGE:
! SELECTED PEAK SHAPE CALCULATION
!.. FIRSTLY DECIDE WHETHER THE KTH PEAK IS ALREADY STORED IN THE
!.. ARRAY PKLIST. AT THIS POINT REFUSE = (K .EQ. KPOINT(KMOD))
!.. DETERMINED IN SUBROUTINE CALTF1 WHICH CALLS THIS ENTRY (IOPT=2)
    2 IF (REFUSE) GOTO 26
      MN = 64
      MN2 = MN/2
!... THE KTH PEAK HAS NOT BEEN CALCULATED - DO SO BY INTERPOLATION
!... DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
!
      KNTH1 = KNTHIS + 1
      IF (ARGK.GE.ARGNOT(KNTH1)) KNTHIS = KNTH1
!.. WORK OUT ARGK OFFSET FROM NEAREST KNOTS FOR INTERPOLATION
      POFF = POFMUL*ALOG(ARGK/ARGNOT(KNTHIS))
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
!
      XPKDEL(KMOD) = 0.
      DO I = 1, 3
        INOT = KNTHIS + I - 2
        XPKDEL(KMOD) = XPKDEL(KMOD) + C3FN(I)*XPDKNT(INOT)
      ENDDO
      DO JG = 1, NPKGEN(JPHASE,JSOURC) + 1
        DO IMN = 1, MN
          PKLIST(IMN,JG,KMOD) = 0.
          DO I = 1, 3
            INOT = KNTHIS + I - 2
            PKLIST(IMN,JG,KMOD) = PKLIST(IMN,JG,KMOD) + C3FN(I)         &
     &                            *PKNOT(IMN,JG,INOT)
          ENDDO
        ENDDO
      ENDDO
!
   26 ARGIMK = (ARGI-ARGK)/XPKDEL(KMOD)
      JARGI = NINT(ARGIMK)
      IARGI = JARGI + MN2 + 1
!.. WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
      POFF = ARGIMK - FLOAT(JARGI)
!.. WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
      C3FN(1) = 0.5*POFF*(POFF-1.)
      C3FN(2) = 1. - POFF**2
      C3FN(3) = 0.5*POFF*(POFF+1.)
      C3DN(1) = POFF - 0.5
      C3DN(2) = -2.*POFF
      C3DN(3) = POFF + 0.5
      YNORM = 0.
      DYNDKQ = 0.
!      CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        DYNDVQ(I) = 0
      ENDDO
      DO I = 1, 3
        II = IARGI + I - 2
        PKTEM = PKLIST(II,1,KMOD)
        YNORM = YNORM + C3FN(I)*PKTEM
        DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
        DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
          NPKD1 = NPKD + 1
          DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKLIST(II,NPKD1,KMOD)
        ENDDO
      ENDDO
!.. NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
      IF (TESTOV(2.,YNORM)) THEN
        DYNDKQ = 0.
!        CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
        DO I = 1, NPKGEN(JPHASE,JSOURC)
          DYNDVQ(I) = 0
        ENDDO
      ELSE
        DYNDKQ = DYNDKQ/(YNORM*XPKDEL(KMOD))
        IF (.NOT.NOPKRF) THEN
          DO I = 1, NPKGEN(JPHASE,JSOURC)
            DYNDVQ(I) = DYNDVQ(I)/YNORM
          ENDDO
!         CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
        ENDIF
      ENDIF
      GOTO 100
!.. CALCULATE THE COMPLETE PEAK SHAPE OF THE SELECTED PEAK BY FFT.
!
!.. FFT CALCULATION STAGE IN PROFILE REFINEMENT
!
!.. FIRST DETERMINE FFT LIMITS FOR PEAK SHAPE
    3 KNOTS = 10
      RKNOT = 1./FLOAT(KNOTS)
      RATN = (ARGMAX(JSOURC)/ARGMIN(JSOURC))**RKNOT
      POFMUL = FLOAT(KNOTS)/ALOG(ARGMAX(JSOURC)/ARGMIN(JSOURC))
      MN = 64
      MN2 = MN/2
!.. MUST DO THIS EVERY TIME IF SIGS ETC NOT CHANGED
      KNTHIS = 2
      DO KNOT = 1, KNOTS + 3
        KNOT2 = KNOT - 2
        ARGK = ARGMIN(JSOURC)*RATN**KNOT2
        ARGNOT(KNOT) = ARGK
        CALL PCTF91(8)
        CALL FDTF02(2)
        TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
        AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)              &
     &          + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
        TEMSQ = TEMSQ + AVTAU*AVTAU
        CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
        XPDKNT(KNOT) = (SQRT(TEMSQ)+CAULIM)/FLOAT(MN2)
        KMOD = 1
        XPKDEL(KMOD) = XPDKNT(KNOT)
!.. NOW SET UP FAST FOURIER TRANSFORM
!.. THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
!.. DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
        IEAGLE = NFFT
        IF (IEAGLE.EQ.0) IEAGLE = NINT(TOLER(1,JPHASE,JSOURC))
        IF (IEAGLE.EQ.3) THEN
          MNS = MN
          CALL FTSUB2(MNS)
          II = MNS/2 - MN2
          DO I = 1, MN
            II = II + 1
            DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
              PKNOT(I,J,KNOT) = (1./45.)*PKADD(II,J)
            ENDDO
          ENDDO
          MNS = 2*MN
          CALL FTSUB2(MNS)
          II = MNS/2 - MN2
          DO I = 1, MN
            II = II + 1
            DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
              PKNOT(I,J,KNOT) = PKNOT(I,J,KNOT) - (20./45.)*PKADD(II,J)
            ENDDO
          ENDDO
          MNS = 4*MN
          CALL FTSUB2(MNS)
          II = MNS/2 - MN2
          DO I = 1, MN
            II = II + 1
            DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
              PKNOT(I,J,KNOT) = PKNOT(I,J,KNOT) + (64./45.)*PKADD(II,J)
            ENDDO
          ENDDO
        ELSEIF (IEAGLE.EQ.2) THEN
          MNS = MN
          CALL FTSUB2(MNS)
          II = MNS/2 - MN2
          DO I = 1, MN
            II = II + 1
            DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
              PKNOT(I,J,KNOT) = -(1./3.)*PKADD(II,J)
            ENDDO
          ENDDO
          MNS = 2*MN
          CALL FTSUB2(MNS)
          II = MNS/2 - MN2
          DO I = 1, MN
            II = II + 1
            DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
              PKNOT(I,J,KNOT) = PKNOT(I,J,KNOT) + (4./3.)*PKADD(II,J)
            ENDDO
          ENDDO
        ELSE
          MNS = MN
          CALL FTSUB2(MNS)
          II = MNS/2 - MN2
          DO I = 1, MN
            II = II + 1
            DO J = 1, NPKGEN(JPHASE,JSOURC) + 1
              PKNOT(I,J,KNOT) = PKADD(II,J)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!.. FFT OVER
      GOTO 100
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDTF02(1)
      DEL = ARGI - ARGK
      TEMSQ = (TOLER(4,JPHASE,JSOURC)*PKFNVA(4))**2
      CAULIM = TOLER(5,JPHASE,JSOURC)*PKFNVA(5)
      IF (DEL) 510, 510, 520
  520 AVTAU = PKFNVA(1)*TOLER(2,JPHASE,JSOURC)*PKFNVA(2)                &
     &        + (1.-PKFNVA(1))*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)
      TEMSQ = TEMSQ + AVTAU*AVTAU
  510 TEMLIM = CAULIM + SQRT(TEMSQ)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDTF02(1)
      DELT = ABS(AKLO-AKHI)
      AVTAU = PKFNVA(1)*PKFNVA(2) + (1.-PKFNVA(1))*PKFNVA(3)
! SQRT 8 LOG 2 AND LOG 2:
      FWHME = SQL2X8*PKFNVA(4) + ALOG2*AVTAU
      STRKT = DELT.LT.STRTOL*FWHME
      SLACK = 0.
      IF (DELT.GE.SLKTOL*FWHME) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*FWHME))**2)
      GOTO 100
!
    7 CALL FDTF02(1)
      F4PAR(2,KNOW) = PKFNVA(4)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFTF92
!
!*****************************************************************************
!
