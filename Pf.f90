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

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))

      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
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

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

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
! DERIVATIVES OF PKFNVA(1) WRT THE 3 PKFNSP PARAMETERS
      DERPFN(1,1) = TANTH*TANTH
      DERPFN(1,2) = TANTH
      DERPFN(1,3) = 1.0
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
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
!
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
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

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

! For the moment, same variable names as TOF case...
!
! WE DO NOT WANT THE CALCULATION IF:
!  A) SAPS, AND
!  B) IT IS NOT THE SPECIAL INITIAL CYCLE, AND
!  C) WE ARE REFINING THIS PEAK VARIABLE:
      TANTH = TAN(RAD*ARGK/2.) ! tan(theta)
      SIGU = PKFNSP(1,1,JPHASE,JSOURC)
      SIGW = PKFNSP(1,2,JPHASE,JSOURC)
      TANSQ = TANTH**2         ! tan(theta) squared
      SECSQ = 1. + TANSQ       ! sec(theta) squared
      SIGMA = SQRT(SECSQ*SIGU**2+TANSQ*SIGW**2)
      PKFNVA(1) = SIGMA
      SECTH = SQRT(SECSQ)      ! sec(theta)
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
      SUBROUTINE PCCN01(N)

      USE REFVAR
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

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))

      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)
      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      GOTO (100,2,100,100,2,6,100), N
! N=2OR 5:
! ARGK=ZERO+ARCSIN(......)
    2 TEM = 0.5 * WLGTH * DSTAR(KNOW)
! REMEMBER, 2THETA:
      ARGK = ZEROSP(1,1,1) + 2.*DEG*ASIN(TEM)
      IF (N.EQ.5) GOTO 100
! CHAIN RULE: D(ARGK)/D(D*)SQ = D(ARGK)/D(D*) * D(D*)/D(D*)SQ
!      DKDDS=0.5*WLGTH/(DSTAR(KNOW)*SQRT(1.-TEM*TEM))
      DKDDS = 0.5*WLGTH*WLGTH/SIN(RAD*ARGK)
! CONVERSION FACTOR...
      DKDDS = DEGREE(DKDDS)
      DKDZER(1) = 1.
      GOTO 100
! MAX SIN THETA/LAMBDA:
    6 STHMXX(1) = SIN(RADIAN(ARGMAX(1)/2.))/WLGTH
  100 RETURN

      END SUBROUTINE PCCN01
!
!*****************************************************************************
!
      SUBROUTINE PCTF01(N)

      USE REFVAR
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
      INCLUDE 'PARAMS.INC'
      
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)
      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      GOTO (1,2,3,4,2,6,2), N
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
      DTDWL = PKCNSP(1,JPHASE,JSOURC)*PCON + 2.*WLGTH*PKCNSP(2,JPHASE,JSOURC)
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
    4 WRITE (NEWIN,2001) PKCNSP(1,JPHASE,JSOURC),PKCNSP(2,JPHASE,JSOURC)
 2001 FORMAT ('L PKCN',2F10.4)
      GOTO 100
!
! MAKE STHLMX FROM TMIN (IN ARGK)
    6 STHMXX(JSOURC) = SINTH*PKCNSP(1,JPHASE,JSOURC)*PCON/(ARGMIN(JSOURC)-ZEROSP(1,JPHASE,JSOURC))
  100 RETURN

      END SUBROUTINE PCTF01
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

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
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

      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      PARAMETER (NW=4)
      CHARACTER*4 WDCN01(NW)
      DIMENSION IWCN01(3,NW)
      DATA WDCN01/'SIGM', 'U', 'V', 'W'/
      DATA IWCN01/3, 3, 0, 3, 3, 1, 3, 3, 2, 3, 3, 3/

      GOTO (10,1,2,100,100,5,6,7), N + 1
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
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDCN01,IWCN01,NW)
      GOTO 100
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
! N=5: IS REFLECTION IN RANGE?
    5 CALL FDCN01(1)
      DEL = ARGI - ARGK
      TEMSQ = TOLER(1,JPHASE,JSOURC)*PKFNVA(1)
      REFUSE = DEL*DEL.GT.TEMSQ
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDCN01(1)
      DELT = ABS(AKLO-AKHI)
      STRKT = DELT.LE.STRTOL*PKFNVA(1)
      SLACK = 0.
      IF (DELT.GE.SLKTOL*PKFNVA(1)) GOTO 100
      SLACK = EXP(1.-(DELT/(SLKTOL*PKFNVA(1)))**2)
      GOTO 100
    7 CALL FDCN01(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFCN01
!
!*****************************************************************************
!
