!
!*****************************************************************************
!
      INTEGER FUNCTION FORTY(PNAME,ALSQ,MATSZ,PCXX,PFXX,MAGROU,CALROU,filnmr)
!
! This is the subroutine that does the Pawley refinement.
!
! RETURNS : 1 for success
!
! *** FORTY updated by JCM 15 JAN 93 ***
!
!X
!C 19B
!H Main body of a PR LSQ program, with dummy arguments
! PR LSQ = Profile Refinement Least SQuares
!A PNAME holds the name, probably of the calling program, to print
!A PCXX is the routine for the peak centre
!A PFXX is the routine for the peak function
!A MAGROU is the routine to deal with magnetic parameters.  this will be set to
!A        DOMAG if expecting magnetic parameters, or DUMMY if not.
!A CALROU is the routine to give the calculated function.  At present expected
!A        settings for this are CALPRM for magnetic, CALPR for non-magnetic.
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE
!
! Interaction here with Winteracter !!!!!!
!
      CHARACTER*6 PNAME
      INTEGER MATSZ
      REAL ALSQ(MATSZ)
      LOGICAL DFLTPR, PRNCYC
      EXTERNAL PCXX, PFXX, MAGROU, CALROU
      CHARACTER*10 filnmr

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL            DERIVV
      INTEGER                      LVARV
      COMMON /DERVAR/ DERIVV(500), LVARV

      INTEGER         IBACK, NBACK
      REAL                             ARGBAK,        BACKGD
      INTEGER         KBCKGD,        NBK, LBKD
      LOGICAL                                       ZBAKIN
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      REAL            SHIFT, XOLD, XNEW, ESD
      INTEGER                                 IFAM, IGEN, ISPC, NEWIN
      INTEGER         KPACK, LKH
      REAL                        SHESD
      INTEGER                            ISHFT
      REAL                                      AVSHFT, AMAXSH
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      REAL            OBS, DOBS, GCALC, YCALC, DIFF
      INTEGER                                        ICODE
      REAL                                                  SUMWD
      INTEGER                                                      NOBS
      INTEGER         IWGH
      REAL                     WTC,    WT, SQRTWT, WDIFF, YBACK, YPEAK
      REAL            YMAX, CSQTOT
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT

      INTEGER       IWGHT
      EQUIVALENCE (IWGHT,IWGH(1))

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

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

      REAL            SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS
      INTEGER                                               IZCT
      REAL                                                        P5
      INTEGER         IOP1, IOP2, KMI,    KMA
      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)

      INTEGER         IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR
      INTEGER         MODEOB,    IPRNT,     MAXCOR, IONLY
      LOGICAL                                                 SIMUL
      LOGICAL         MAG, MPL, FIXED, DONE
      REAL                                   CONV
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      INTEGER      MODER
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

      INTEGER         IPK
      REAL                 PIK
      COMMON /IPKCMN/ IPK, PIK(MIPK)

      INTEGER         KIPT,       KNIPT
      REAL                                       ZNORM,         DZNDKQ
      REAL            DZNDVQ
      INTEGER                           JOCCR
      COMMON /CMN299/ KIPT(MOBS), KNIPT(MAXPIK), ZNORM(MAXPIK), DZNDKQ(MAXPIK), &
                      DZNDVQ(9,MAXPIK), JOCCR(MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      INTEGER         NOBSNOW
      COMMON /CMNNOW/ NOBSNOW

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      REAL            CummChiSqd
      COMMON /CMN007/ CummChiSqd(MOBS)

      CHARACTER*80    MESSAG*100, NAMFIL*100
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD
      EQUIVALENCE (ICARD,MESSAG)

      CHARACTER*10    filnam_root
      COMMON /commun/ filnam_root
!
! JCC add error handling declarations
      INTEGER IEOCC
      INTEGER, EXTERNAL :: PREFIN
! JCC This is for testing for mathematical errors in the CCSL that used to STOP the program
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      EXTERNAL RUNPAR, VARSPR
      INTEGER ISCR, NFLIP, Npt30, IPT, NTEM
      INTEGER K1, K2, KK

      IBMBER = 0
  !    CALL DebugRoutine
! JvdS I set ZBAKIN to .TRUE.
! With the excluded regions code reinstated, this is necessary to 
! ensure that we obtain the same results as with the release version.
      ZBAKIN = .TRUE.
! JCC Initialise return value to successful (=1) any other value is failure
      FORTY = 1
      IPK = 0
      filnam_root = filnmr
      IEOCC = PREFIN(PNAME)
! Check the return status
      IF (IEOCC.NE.1) GOTO 900
! SET UP PRECISE PROBLEM, AND READ MOST L CARDS:
      CALL REFSET
! DISCOVER WHETHER SLACK CONSTRAINTS:
      CALL GEOMIN(0)
! JCC Trap for any problems
      IF (IBMBER.GT.0) GOTO 950
! THIS ROUTINE IS ONLY FOR ONE PHASE:
      CALL LOGPHA(1)
      CALL SETPR(PCXX,PFXX,MAGROU)
! JCC Trap for any problems
      IF (IBMBER.GT.0) GOTO 950
! COLLECT CONSTRAINTS IMPOSED BY SYMMETRY, AND THOSE REQUESTED, AND
! SET UP PARAMETERS AS VARIABLES (NOT YET AS BASIC VARIABLES)
      CALL PARSPR(MAGROU)
! JCC Trap for any problems
      IF (IBMBER.GT.0) GOTO 950
! MAKE LIST OF REFLECTION INDICES:
      CALL INRFPR(PCXX,PFXX)
! READ OBS DATA AND SEND OUT TO TEMPORARY UNIT FOR REINPUT EACH CYCLE
      CALL INOBPR(ISCR,NFLIP,PCXX,PFXX)
  !    CALL CHKMAXREF
! CALCULATE THE STEP-SIZE FOR PEAK-SHAPE CALCULATION
      CALL XDELPR
      CALL QNUMPP
! Open the .PIK file
      MESSAG = 'peak contribution (.pik) file'
      NAMFIL = '.PIK'
      IPK = 80
      CALL OPNFIL(IPK,113)
      DONE = .FALSE.
      DO ICYC = NCYC1, LASTCY
 !D     DO ICYC = 1, 1
! *** Winteracter calls ***
        CALL WDialogSelect(IDD_ViewPawley)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,NPTS)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,MAXK)
        CALL WDialogSelect(IDD_Pawley_Status)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_Number,ICYC)
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,LASTCY)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,NPTS)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,MAXK)
! JCC Add in check on number of reflections here, so that code doesn't bomb out in the Pawley attempt
        IF (MAXK.GT.350) THEN
          FORTY = -1
          IF (IPK.NE.0) CLOSE (IPK)
          RETURN
        ENDIF
        IF (PRECYC .AND. ICYC.NE.NCYC1) THEN
          SIMUL = .FALSE.
          PRECYC = .FALSE.
        ENDIF
! IF NEEDED, CAIL/SAPS/APES PROCESSING ON EACH CYCLE
! FOR NOW, IMPOSE PHASE 1, SOURCE 1 WHICH WE HOPE STAY THERE:
        JPHASE = 1
        JSOURC = 1
! CALCULATE PEAK & PEAK DERIVATIVE CONTRIBUTIONS
        IF (AKNOTS.LE.0.) THEN
          CALL PFXX(2) !PIKCAL
        ELSE
          CALL PFXX(3)
        ENDIF
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
        IF (.NOT.RIET) CALL FAM4PR(2,PCXX,PFXX)
        IF (IBMBER.GT.0) GOTO 950
        CALL VARMAK(DFLTPR,RUNPAR,VARSPR)
!* this won't actually do - we want varspm if magnetic
! FOR NOW, IMPOSE PHASE 1, SOURCE 1 WHICH WE HOPE STAY THERE:
        JPHASE = 1
        JSOURC = 1
        IF (IBMBER.GT.0) GOTO 950
! IF ANY SLACK CONSTRAINTS, SET UP:
        CALL GEOMCO(1)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
        CALL LOGSET
! INITIALISE R FACTOR SUMS:
        CALL RFACS(1)
        CALL RFACPR(1,PCXX)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! SET UP POINTERS IN TRIANGULAR MATRIX AND CLEAR OUT LSQ MATRIX AND RHS
        CALL MATSET(ALSQ,MATSZ)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! COUNT USED OBSERVATIONS :
        NOBS = 0
!* NOTICE HERE IF SLONLY
        REWIND ISCR
!
! INOBPR HAS TURNED THE VARIOUS FORMATS FOR INPUT INTO ONE, AND DECIDED WHICH
! REFLECTIONS CONTRIBUTE.
!
!
! NEW ENTRY TO ARRANGE INTERPOLATION FOR SPEED IF APPROPRIATE:
!      CALL PFXX(3)
!
!  2   READ (ISCR,END=3) ARGI,OBS,DOBS,WT,ICODE,KMIN,KMAX
!
! CLEAR YCALC TO COLLECT QUANTITY COMPARABLE WITH OBS, AND DERIVV TO COLLECT
! THE CORRESPONDING DERIVATIVES OF YCALC WRT ALL VARIABLES:
! (DONE HERE IN CASE NO CONTRIBUTING REFLNS)
! *** Winteracter call ***
        CALL WDialogRangeProgressBar(IDF_Pawley_Progress_Bar,1,NPTS)
        Npt30 = NPTS/30
        DO IPT = 1, NPTS
          IF (npt30*(ipt/npt30).EQ.ipt) THEN
            CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
          ENDIF
          YBACK = 0.0
          YPEAK = 0.0
          YCALC = 0.0
          NOBSNOW = IPT
          KMAX = KREFT(NOBSNOW) ! Number of peaks contributing to this data point
          ARGI = ZARGI(NOBSNOW)
          OBS = ZOBS(NOBSNOW)
          DOBS = ZDOBS(NOBSNOW)
          WT = ZWT(NOBSNOW)
          ICODE = ICODEZ(NOBSNOW)
          IF (LVARV.GT.0 .AND. .NOT.SIMUL) CALL GMZER(DERIVV,1,LVARV)
!
! DEAL WITH CASE YCALC=0 (EITHER BY BEING EXCLUDED OR BY HAVING NO CONTRIBUTING
! REFLECTIONS) - NB ICODE IS NON-ZERO FOR DO **NOT** USE:

          IF (KMAX.EQ.0 .AND. .NOT.ZBAKIN) ICODE = -1
! When we are here
!   ICODE = -1   : no contributing reflections
!   ICODE =  0   : normal
!   ICODE =  1   : excluded region (set in 'CONTRI' in Pr.f90)
! JvdS This is where ICODE is tested. ICODE is used to signal that the point
! is part of an excluded region. The code labelled !O is the old code, 
! which did nothing.
!O          IF (ICODE.NE.0) THEN
!O!          CALL RFACPR(5,PCXX)
!O!          GO TO 2 ! This used to jump to the point where the next input line was read
!O          ENDIF
!
! CALCULATE FUNCTION TO MATCH OBSERVED, AND ITS DERIVATIVES, AND DO SOME
! STATISTICS:
! ICODE .NE. 0 means: no contribution, either excluded region or no peak
          IF (KMAX.NE.0 .AND. (ICODE.EQ.0)) CALL CALROU(PCXX,PFXX) ! Fills YPEAK
! ICODE = 1 means: excluded region
          IF (ICODE .NE. 1) CALL BACKPR(2) ! Fills YBACK
          YCALC = YBACK + YPEAK
          YCBIN(NOBSNOW) = YCALC
          YBBIN(NOBSNOW) = YBACK
! MAKE DERIVATIVES WRT BASIC VARIABLES FROM THOSE WRT VARIABLES:
          CALL RELATE
! DIFFERENCE:
          IF (SIMUL) OBS = YCALC
          DIFF = OBS - YCALC
! FROM WEIGHT GET SQRTWT AND WDIFF INTO COMM0N:
          CALL WGHTLS(3,ARGI)
! ADD IN TO R FACTORS:
          IF (ICODE.EQ.0) THEN
            CALL RFACPR(2,PCXX) ! YCALC .NE. 0.0
          ELSE
            CALL RFACPR(5,PCXX) ! YCALC .EQ. 0.0, i.e. excluded region or just no peaks
          ENDIF
          CummChiSqd(iPT) = SUMWD
! JCC Trap for any problems
          IF (IBMBER.GT.0) GOTO 950
! ADD DERIVATIVES IN TO LSQ MATRIX:
          CALL MATTOT(ALSQ,MATSZ)
          IF (DONE) THEN
            IF (ICODE .EQ. 1) THEN
              NTEM = 0                ! Excluded region
            ELSE
              NTEM = KREFT(NOBSNOW)
            ENDIF
            K1 = KIPT(NOBSNOW) + 1
            K2 = KIPT(NOBSNOW+1)
! We can only read in 50 values, so no need to write out more than that
            IF (NTEM .GT. 50) THEN
              K2 = K2 - (NTEM - 50)
              NTEM = 50
            ENDIF
            WRITE (IPK,*) ARGI, OBS - YBACK, DOBS, NTEM
!C                       XBIN(I), YOBIN(I), EBIN(I), KTEM
            IF (NTEM.GT.0) WRITE (IPK,*) (KNIPT(KK),PIK(KNIPT(KK)),KK=K1,K2)
          ENDIF
          NOBS = NOBS + 1
! NEXT OBSERVATION:
!...      GO TO 2
   10     CONTINUE
        ENDDO ! IPT LOOP TO NPTS
! HERE ON NO MORE PROFILE OBSERVATIONS - PRINT R FACTORS:
        CALL RFACPR(3,PCXX)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! SLACK CONSTRAINTS - FIRST PAWLEY-TYPE:
        CALL PAWLS(ALSQ,MATSZ,3)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
!* JOIN HERE IF ONLY SLACK:
! THEN GEOMETRICAL:
        CALL GEOMLS(ALSQ,MATSZ)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! COMBINED CHI SQUARED:
        CALL RFACS(6)
! IF CAIL, OUTPUT EIGENVALUES & EIGENVECTORS IF REQUIRED BY "I PREE"
        IF (DONE) CALL EIGEN(ALSQ,MATSZ)
        IF (DONE) CALL HESCOR(ALSQ,MATSZ)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! INVERT MATRIX:
        CALL MATINV(ALSQ,MATSZ)
! CALCULATE SHIFTS AND ESD'S:
        CALL MATSHF(ALSQ,MATSZ)
! JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! APPLY SHIFTS AND PRINT:
        CALL APSHPR(ALSQ,MATSZ,PCXX,PFXX,MAGROU)
! IF PREVIOUS CYCLE HAD CONVERGED ACCORDING TO I CONV, FINISH:
        IF (DONE) GOTO 39
        DONE = (AMAXSH.LT.CONV .OR. ICYC.EQ.LASTCY-1 .OR. NCYC.EQ.1)
! OUTPUT NEW CRYSTAL DATA FOR PENULTIMATE CYCLE:
        IF (DONE) CALL NWINPR(PCXX,PFXX,MAGROU)
! *** Winteracter calls ***
        CALL WDialogSelect(IDD_ViewPawley)
        CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,PAWLEYCHISQ,'(F12.3)')
        CALL WDialogSelect(IDD_Pawley_Status)
        CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,PAWLEYCHISQ,'(F12.3)')
        CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RWPOBS,'(F12.2)')
        CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RWPEXP,'(F12.2)')
      ENDDO
! PRINT CORRELATION MATRIX:
   39 CONTINUE
      CALL MATCOR(ALSQ,MATSZ)
! OUTPUT H,K,L IF REQUIRED:
      CALL HKLOUT(PCXX,ALSQ,MATSZ)
      CALL CLOFIL(ISCR)
      IF (PRNCYC(4)) CALL CLOFIL(IOP2)
      CLOSE (IPK)
! JCC Trap for any problems
	IF (IBMBER .NE. 0) CALL DebugErrorMessage('Error in FORTY after HKLOUT')
  !    CALL DebugRoutine
      RETURN
! JCC Added in error handling here
  900 CONTINUE
      FORTY = 0
      IF (IPK.NE.0) CLOSE (ipk)
      RETURN
  950 FORTY = -2
      IF (IPK.NE.0) CLOSE (ipk)
      IBMBER = 0

      END FUNCTION FORTY
!
!*****************************************************************************
!
      SUBROUTINE PFCN03(N)

      USE REFVAR

!
! *** PFCN03 from updated PFCN01 by WIFD/JCM 19 Jul 88 ***
!
!X
!C 19B
!H Multiple entry asymmetric Voigt profile for neutron/synchrotron
!H radiation (type 01) routine with FFT.
!A second word, one of a vocabulary of:
!A     SIGM
!A     GAMM
!A     HPSL
!A     HMSL
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
      INCLUDE 'PARAMS.INC'

      PARAMETER (NW=4)
      CHARACTER*4 WDCN03(NW)
      LOGICAL TESTOV
      DIMENSION C3FN(3), C3DN(3), IWCN03(3,NW)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
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

      COMMON /PRSAVZ/ PKCONV(512,9)
      COMMON /PRKNOT/ ARGKNT(50), PKKNOT(512,9,50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL ZTEM(MOBS), RTEM(3,MFCSTO), TF4PAR(MF4PAR)
      LOGICAL         PFNVAR
      COMMON /PFNINF/ PFNVAR(8,9,5)

      REAL ARTEM(6)
      INTEGER KORD(MFCSTO)

      INTEGER         KIPT,       KNIPT
      REAL                                       ZNORM,         DZNDKQ
      REAL            DZNDVQ
      INTEGER                           JOCCR
      COMMON /CMN299/ KIPT(MOBS), KNIPT(MAXPIK), ZNORM(MAXPIK), DZNDKQ(MAXPIK), &
                      DZNDVQ(9,MAXPIK), JOCCR(MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      DIMENSION ZNORMT(MAXPIK), DZNDKQT(MAXPIK), DZNDVQT(9,MAXPIK), KARGO(MAXPIK), KARGK(MAXPIK)

      DATA WDCN03/'SIGM', 'GAMM', 'HPSL', 'HMSL'/
      DATA IWCN03/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0/
      CHARACTER*20 Integer2String

      GOTO (10,1,2,3,100,5,6,7), N + 1
! N=0: SET UP "DATA SOURCE CN/SR, PEAK TYPE 01"
   10 NPKGEN(JPHASE,JSOURC) = 4
      NGEN4(JPHASE,JSOURC) = 3
      LF3SP(3,JPHASE,JSOURC) = -2
      LF3SP(4,JPHASE,JSOURC) = -2
      LF3SP(5,JPHASE,JSOURC) = -1
      LF3SP(6,JPHASE,JSOURC) = -1
      LF6SP(1,JSOURC) = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        PWD(I,JPHASE,JSOURC) = WDCN03(I)
      ENDDO
      GOTO 100
! N=1: ADD PACKED VOCABULARY FOR THIS SOURCE, THIS PHASE, TO THE MAIN LIST:
    1 CALL VOCAB(WDCN03,IWCN03,NW)
      GOTO 100
! PROFILE REFINEMENT STAGE:
    2 MN = 512
      MN2 = MN/2
      CALL DebugErrorMessage('In buggy code')
! Peak positions may have changed - check and re-sort if necessary
      DO IR = 1, MAXK
        KNOW = IR
        CALL CELDER(rHKL(1,KNOW),ARTEM)
        DSTAR(KNOW) = SQRT(DSTAR2)
        CALL PCXX(5)
        ZARGK(IR) = ARGK
      ENDDO
      CALL SORT_REAL(ZARGK,KORD,MAXK)
! Set the number of peaks at each point to zero
      DO II = 1, NPTS
        KREFT(II) = 0
      ENDDO
      DO IR = 1, MAXK
        IRT = KORD(IR)
        ZTEM(IR) = ZARGK(IRT)
        DO I = 1, 3
          RTEM(I,IR) = rHKL(I,IRT)
        ENDDO
        IF (CAIL) THEN
          TF4PAR(IR) = F4PAR(1,IRT)
        ENDIF
      ENDDO
      DO IR = 1, MAXK
        ZARGK(IR) = ZTEM(IR)
        DO I = 1, 3
          rHKL(I,IR) = RTEM(I,IR)
        ENDDO
        IF (CAIL) THEN
          F4PAR(1,IR) = TF4PAR(IR)
        ENDIF
      ENDDO
      IOBS = 1
      DO IRT = 1, MAXK
        IR = IRT
        DO I = IOBS, NPTS
          IF (ZARGI(I).GT.ZARGK(IR)) THEN
            KOBZ(IR) = I
            GOTO 30
          ENDIF
        ENDDO
   30   CONTINUE
      ENDDO
      KOUNT = 0
      DO KNOW = 1, MAXK
        ARGK = ZARGK(KNOW)
        CALL FDCN03(2)
! FFT CALCULATION STAGE IN PROFILE REFINEMENT
! THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
! DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
        CALL FCSUB3(MN)
! FFT OVER
! FIND THE PEAK MAXIMUM VALUE AND THEN WORK OUT THE PEAK LIMITS
        PKMAX = PKCONV(1,1)
        IPMAX = 1
        DO I = 1, MN
          IF (PKCONV(I,1).GE.PKMAX) THEN
            PKMAX = PKCONV(I,1)
            IPMAX = I
          ENDIF
        ENDDO
        PKCRIT = TOLR(1,JSOURC)
        PKCT = PKCRIT*PKMAX
        IMINT = 1
        DO I = 2, MN
          IF (PKCONV(I,1).GE.PKCT) THEN
            IMINT = I
            GOTO 1510
          ENDIF
        ENDDO
 1510   IMAXT = MN
        DO I = MN - 1, 1, -1
          IF (PKCONV(I,1).GE.PKCT) THEN
            IMAXT = I
            GOTO 1520
          ENDIF
        ENDDO
 1520   ARIMIN = ARGK + ZXDEL(KNOW)*FLOAT(IMINT-IPMAX)
        ARIMAX = ARGK + ZXDEL(KNOW)*FLOAT(IMAXT-IPMAX)
        IIMAX = NPTS
        DO I = KOBZ(KNOW), NPTS
          IF (ZARGI(I).GE.ARIMAX) THEN
            IIMAX = I
            GOTO 42
          ENDIF
        ENDDO
   42   IIMIN = 1
        DO I = KOBZ(KNOW), 1, -1
          IF (ZARGI(I).LE.ARIMIN) THEN
            IIMIN = I
            GOTO 44
          ENDIF
        ENDDO
   44   DO II = IIMIN, IIMAX
          KOUNT = KOUNT + 1
          KARGO(KOUNT) = II
          KARGK(KOUNT) = KNOW
          KREFT(II) = KREFT(II) + 1
          IF (KREFT(II).EQ.1) JOCCR(II) = KOUNT
          TARGI = ZARGI(II)
          TARGK = ZARGK(KNOW)
          DTARG = TARGI - TARGK
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
          JARGI = NINT((DTARG)/ZXDEL(KNOW))
          IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
          POFF = DTARG/ZXDEL(KNOW) - FLOAT(JARGI)
! WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
          C3FN(1) = 0.5*POFF*(POFF-1.0)
          C3FN(2) = 1.0 - (POFF**2)
          C3FN(3) = 0.5*POFF*(POFF+1.0)
          C3DN(1) = POFF - 0.5
          C3DN(2) = -2.0*POFF
          C3DN(3) = POFF + 0.5
          YNORM = 0.
          DYNDKQ = 0.
          CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
          DO I = 1, 3
            III = IARGI + I - 2
            PKTEM = PKCONV(III,1)
            YNORM = YNORM + C3FN(I)*PKTEM
            DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
            DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
              NPKD1 = NPKD + 1
              DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKCONV(III,NPKD1)
            ENDDO
          ENDDO
! NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
          IF (TESTOV(2.,YNORM)) THEN
            DYNDKQ = 0.
            CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
          ELSE
            DYNDKQ = DYNDKQ/(YNORM*ZXDEL(KNOW))
            CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
          ENDIF
          ZNORMT(KOUNT) = YNORM
          DZNDKQT(KOUNT) = DYNDKQ
          DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
            DZNDVQT(NPKD,KOUNT) = DYNDVQ(NPKD)
          ENDDO
        ENDDO  !  II LOOP
      ENDDO  ! KNOW LOOP
      KKT = 0
      KIPT(1) = 0
      DO II = 1, NPTS
        KIPT(II+1) = KIPT(II) + KREFT(II)
        IF (KREFT(II).NE.0) THEN
          KK = 0
          DO JJ = JOCCR(II), KOUNT
            IF (KARGO(JJ).EQ.II) THEN
              KKT = KKT + 1
              KK = KK + 1
              ZNORM(KKT) = ZNORMT(JJ)
              DZNDKQ(KKT) = DZNDKQT(JJ)
              KNIPT(KKT) = KARGK(JJ)
              DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
                DZNDVQ(NPKD,KKT) = DZNDVQT(NPKD,JJ)
              ENDDO
              IF (KK.EQ.KREFT(II)) GOTO 680
            ENDIF
          ENDDO
        ENDIF
  680   CONTINUE
      ENDDO
      GOTO 100
!
! PROFILE REFINEMENT STAGE with KNOTS:
!
    3 MN = 512
      MN2 = MN/2
! Peak positions may have changed - check and re-sort if necessary
      DO IR = 1, MAXK
        KNOW = IR
        CALL CELDER(rHKL(1,KNOW),ARTEM)
        DSTAR(KNOW) = SQRT(DSTAR2)
        CALL PCXX(5)
        ZARGK(IR) = ARGK
      ENDDO
      CALL SORT_REAL(ZARGK,KORD,MAXK)
! Now store the sorted values sequentially
      DO IR = 1, MAXK
        IRT = KORD(IR)
        ZTEM(IR) = ZARGK(IRT)
        DO I = 1, 3
          RTEM(I,IR) = rHKL(I,IRT)
        ENDDO
        IF (CAIL) THEN
          TF4PAR(IR) = F4PAR(1,IRT)
        ENDIF
      ENDDO
! Now copy the sequential, sorted values back to their original COMMON blocks
      DO IR = 1, MAXK
        ZARGK(IR) = ZTEM(IR)
        DO I = 1, 3
          rHKL(I,IR) = RTEM(I,IR)
        ENDDO
        IF (CAIL) THEN
          F4PAR(1,IR) = TF4PAR(IR)
        ENDIF
      ENDDO
! rHKL and ZARGK have now been sorted.
! Set the number of contributing peaks at each data point to zero
      DO II = 1, NPTS
        KREFT(II) = 0
      ENDDO
      IOBS = 1
      DO IR = 1, MAXK
        DO I = IOBS, NPTS
          IF (ZARGI(I).GT.ZARGK(IR)) THEN
            KOBZ(IR) = I
            IOBS = I
            EXIT
          ENDIF
        ENDDO
      ENDDO
! Now do the knots
      KOUNT = 0
      IF (AKNOTS.LE.1.) THEN
        KNOTS = NINT(AKNOTS*MAXK)
      ELSE
        KNOTS = NINT(AKNOTS)
      ENDIF
      KNOTS = MIN(50,KNOTS)
      KNOTS = MAX(5,KNOTS)
      TTMIN = ZARGK(1)
      TTMAX = ZARGK(MAXK)
      TTDIF = TTMAX - TTMIN
      TTINC = TTDIF/FLOAT(KNOTS-1)
      DO KNOW = 1, KNOTS
        ARGK = TTMIN + FLOAT(KNOW-1)*TTINC
        ARGKNT(KNOW) = ARGK
! Calculate Sigma, Gamma, HPSL, HMSL and their derivatives at this ARGK
        CALL FDCN03(2)
! FFT CALCULATION STAGE IN PROFILE REFINEMENT
! THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
! DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
! Fills PKCONV
        CALL FCSUB3(MN)
        MN21 = MN2 - 1
        DO II = 1, MN
          PKKNOT(II,1,KNOW) = PKCONV(II,1)
        ENDDO
        DO JJ = 1, NPKGEN(JPHASE,JSOURC)
          IF (PFNVAR(JJ,JPHASE,JSOURC)) THEN
            JJ1 = JJ + 1
            DO II = 1, MN
              PKKNOT(II,JJ1,KNOW) = PKCONV(II,JJ1)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      KOUNT = 0
      JKNOT = 1
      DO KNOW = 1, MAXK
        ARGK = ZARGK(KNOW)
! Do interpolation ... and come out with PKCONV
! The first peak is at the first knot
! and the last peak at the last knot.
        DO IK = JKNOT, KNOTS
          IF (ARGK.LT.ARGKNT(IK)) THEN
            JK = IK
            GOTO 50
          ENDIF
        ENDDO
        JK = KNOTS
   50   JKNOT = JK
        JK0 = MAX(2,JKNOT)
        JK0 = MIN(JK0,KNOTS-1)
        POFF = (ARGK-ARGKNT(JK0))/TTINC
! Now calculate the weights of the interpolation. M = minus one, 0 = this point, P = plus one
        C3FNM = 0.5*POFF*(POFF-1.0)
        C3FN0 = 1.0 - (POFF**2)
        C3FNP = 0.5*POFF*(POFF+1.0)
        DO II = 1, MN
          PKCONV(II,1) = C3FNM*PKKNOT(II,1,JK0-1) + C3FN0*PKKNOT(II,1,JK0) + C3FNP*PKKNOT(II,1,JK0+1)
        ENDDO
        DO JJ = 1, NPKGEN(JPHASE,JSOURC)
          IF (PFNVAR(JJ,JPHASE,JSOURC)) THEN
            JJ1 = JJ + 1
            DO II = 1, MN
              PKCONV(II,JJ1) = C3FNM*PKKNOT(II,JJ1,JK0-1) + C3FN0*PKKNOT(II,JJ1,JK0) + C3FNP*PKKNOT(II,JJ1,JK0+1)
            ENDDO
          ENDIF
        ENDDO
! FIND THE PEAK MAXIMUM VALUE AND THEN WORK OUT THE PEAK LIMITS
        PKMAX = PKCONV(1,1)
        IPMAX = 1
        DO I = 2, MN
          IF (PKCONV(I,1).GE.PKMAX) THEN
            PKMAX = PKCONV(I,1)
            IPMAX = I
          ENDIF
        ENDDO
        PKCRIT = TOLR(1,JSOURC)
        PKCT = PKCRIT*PKMAX
        DO I = 2, MN
          IF (PKCONV(I,1).GE.PKCT) THEN
            IMINT = I
            GOTO 3510
          ENDIF
        ENDDO
 3510   DO I = MN - 1, 1, -1
          IF (PKCONV(I,1).GE.PKCT) THEN
            IMAXT = I
            GOTO 3520
          ENDIF
        ENDDO
 3520   ARIMIN = ARGK + ZXDEL(KNOW)*FLOAT(IMINT-IPMAX)
        ARIMAX = ARGK + ZXDEL(KNOW)*FLOAT(IMAXT-IPMAX)
! JvdS I think this solves JCC's comment below on boundaries being exceeded.
        DO I = KOBZ(KNOW), NPTS
          IF (ZARGI(I).GE.ARIMAX) THEN
            IIMAX = I - 1 ! I is just _outside_ the peak, so we want I-1 to be the last point of the peak
            GOTO 3542
          ENDIF
        ENDDO
! When we arrive here, the limits of the peak were actually outside the powder pattern
        IIMAX = NPTS
 3542   DO I = KOBZ(KNOW), 1, -1
          IF (ZARGI(I).LE.ARIMIN) THEN
            IIMIN = I + 1 ! I is just _outside_ the peak, so we want I+1 to be the last point of the peak
            GOTO 3544
          ENDIF
        ENDDO
! When we arrive here, the limits of the peak were actually outside the powder pattern
        IIMIN = 1
 3544   DO II = IIMIN, IIMAX
          KOUNT = KOUNT + 1 ! IF (KOUNT .GT. MAXPIK) CALL DebugErrorMessage('KOUNT .GT. MAXPIK')
          KARGO(KOUNT) = II
          KARGK(KOUNT) = KNOW
          KREFT(II) = KREFT(II) + 1
          IF (KREFT(II).EQ.1) JOCCR(II) = KOUNT
          TARGI = ZARGI(II)
          TARGK = ZARGK(KNOW)
          DTARG = TARGI - TARGK
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
          JARGI = NINT((DTARG)/ZXDEL(KNOW))
          IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
          POFF = DTARG/ZXDEL(KNOW) - FLOAT(JARGI)
! WORK OUT INTERPOLATION COEFFICIENTS FOR FUNCTIONS AND ARGK DERIVATIVE
          C3FN(1) = 0.5*POFF*(POFF-1.0)
          C3FN(2) = 1.0 - (POFF**2)
          C3FN(3) = 0.5*POFF*(POFF+1.0)
          C3DN(1) = POFF - 0.5
          C3DN(2) = -2.0*POFF
          C3DN(3) = POFF + 0.5
          YNORM = 0.
          DYNDKQ = 0.
          CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
          DO I = 1, 3
            III = IARGI + I - 2
! JCC @@ Can get an array bound overflow error here so I've trapped for it temporarily.
! May want to check out why ...
! Added a hack ...
! JvdS It should be better now, but it's not quite solved yet.
            IF (III.LE.512 .AND. III.GT.0) THEN
              PKTEM = PKCONV(III,1)
              YNORM = YNORM + C3FN(I)*PKTEM
              DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
              DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
                NPKD1 = NPKD + 1
                DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKCONV(III,NPKD1)
              ENDDO
            ELSE
              CALL DebugErrorMessage('(III.LE.512 .AND. III.GT.0) in forty.f90, III = '//Integer2String(III))
! JCC End of hack ...
            ENDIF
          ENDDO
! NOW CHECK IF YNORM IS ZERO BEFORE EVALUATING QUOTIENT DERIVATIVES
          IF (TESTOV(2.,YNORM)) THEN
            DYNDKQ = 0.
            CALL GMZER(DYNDVQ,1,NPKGEN(JPHASE,JSOURC))
          ELSE
            DYNDKQ = DYNDKQ/(YNORM*ZXDEL(KNOW))
            CALL GMSCA(DYNDVQ,DYNDVQ,1./YNORM,1,NPKGEN(JPHASE,JSOURC))
          ENDIF
          ZNORMT(KOUNT) = YNORM
          DZNDKQT(KOUNT) = DYNDKQ
          DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
            DZNDVQT(NPKD,KOUNT) = DYNDVQ(NPKD)
          ENDDO
        ENDDO  !  II LOOP
      ENDDO  ! KNOW LOOP
      KKT = 0
      KIPT(1) = 0
      DO II = 1, NPTS
        KIPT(II+1) = KIPT(II) + KREFT(II)
        IF (KREFT(II).NE.0) THEN
          KK = 0
          DO JJ = JOCCR(II), KOUNT
            IF (KARGO(JJ).EQ.II) THEN
              KKT = KKT + 1
              KK = KK + 1
              ZNORM(KKT) = ZNORMT(JJ)
              DZNDKQ(KKT) = DZNDKQT(JJ)
              KNIPT(KKT) = KARGK(JJ)
              DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
                DZNDVQ(NPKD,KKT) = DZNDVQT(NPKD,JJ)
              ENDDO
              IF (KK.EQ.KREFT(II)) GOTO 3680
            ENDIF
          ENDDO
        ENDIF
 3680   CONTINUE
      ENDDO
      GOTO 100
! PRE-PROFILE REFINEMENT STAGE
    5 CALL FDCN03(1)
      DEL = ARGI - ARGK
      SIGLIM = TOLER(1,JPHASE,JSOURC)*PKFNVA(1)
      CAULIM = TOLER(2,JPHASE,JSOURC)*PKFNVA(2)
      ASYLIM = 0.5*DEG*TOLER(3,JPHASE,JSOURC)*PKFNVA(3)**2/TAN(RAD*ARGK)
      TEMLIM = ASYLIM
      IF (DEL) 510, 510, 520
  510 TEMLIM = ASYLIM
  520 TEMLIM = TEMLIM + 0.5*(SIGLIM+CAULIM)
      REFUSE = ABS(DEL).GT.TEMLIM
      GOTO 100
! N=6 *** CAILS *** SETTING UP SLACK AND STRICT CONSTRAINTS
    6 CALL FDCN03(1)
      DELT = ABS(AKLO-AKHI)
      STRKT = DELT.LT.2.0*STRTOL*ZXDEL(KNOW)
      SLACK = 0.
      GOTO 100
    7 CALL FDCN03(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      F4PAR(3,KNOW) = PKFNVA(2)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFCN03
!
!*****************************************************************************
!
      SUBROUTINE FCSUB3(MNS)
!
!X   For use with PFCN03 constant wavelength data with finite detector height
!C 19B
!H
      INCLUDE 'PARAMS.INC'

      LOGICAL NEAR90
      COMPLEX CFFT, DFFT, DDT, CFF
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

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

      COMMON /PRSAVZ/ PKCONV(512,9)

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DIMENSION CFFT(8), DFFT(8), DDT(8), FR(512,8), FI(512,8),         &
     &          DR(512,8), DI(512,8), FRT(512), FIT(512)
      LOGICAL         PFNVAR
      COMMON /PFNINF/ PFNVAR(8,9,5)

      SIG = PKFNVA(1)
      GAM = PKFNVA(2)
      HPS = PKFNVA(3)
      HMS = PKFNVA(4)
      DENTEM = (FLOAT(MNS)*ZXDEL(KNOW))
      C2TEM = PI/DENTEM
      CTEM = 2.*C2TEM
      GTEM = CTEM*SIG
      CLTEM = C2TEM*GAM
! TO DEAL WITH (A) 90 DEGREES AND (B) ABOVE ALL WE WILL DO IS
! (A) SET FR(I,3)=1 AND ALL ELSE TO ZERO AND
! (B) SWITCH THE SIGN OF THE IMAGINARY COMPONENTS
      NEAR90 = (ABS(ARGK-90.).LT.2.0)
      IF (.NOT.NEAR90) THEN
        TANRA = ABS(TAN(RAD*ARGK))
        DENASY = 0.5*(HPS-HMS)*(HPS+HMS)
! BET1 AND NETPI CHANGE SIGN AT 90 DEGREES
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
        FI(I,1) = 0.
        DR(I,1) = -ARG*ARG*FR(I,1)/SIG
        DI(I,1) = 0.
! LORENTZIAN
        AFII = ABS(FLOAT(II))
        ARG = CLTEM*AFII
        FR(I,2) = EXP(-ARG)
        FI(I,2) = 0.
        DR(I,2) = -C2TEM*AFII*FR(I,2)
        DI(I,2) = 0.
!.. ASYMMETRY FUNCTION FOR UMBRELLA EFFECT
! JvdS Assuming that II is an INTEGER, it should be tested against "0", not "0."
        IF (II.EQ.0 .OR. NEAR90) THEN
          FR(I,3) = 1.
          DR(I,3) = 0.
          DR(I,4) = 0.
          FI(I,3) = 0.
          DI(I,3) = 0.
          DI(I,4) = 0.
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
          FR(I,3) = ((HPS*FRCP-HMS*FRCM)-BETPK*(SINP-SINM))/DENASY
          FI(I,3) = -((HPS*FRSP-HMS*FRSM)+BETPK*(COSP-COSM))/DENASY
          DR(I,3) = (FRCP-HPS*FR(I,3))/DENASY
          DI(I,3) = -(FRSP-HPS*FI(I,3))/DENASY
          DR(I,4) = (HMS*FR(I,3)-FRCM)/DENASY
          DI(I,4) = -(HMS*FI(I,3)-FRSM)/DENASY
          IF (ARGK.GT.90.) THEN
            FI(I,3) = -FI(I,3)
            DI(I,3) = -DI(I,3)
            DI(I,4) = -DI(I,4)
          ENDIF
        ENDIF
      ENDDO
! NOW FORM PRODUCTS IN FOURIER SPACE
      DO I = 1, MNS
        DO J = 1, NPKGEN(JPHASE,JSOURC)
          IF (J.NE.4) CFFT(J) = CMPLX(FR(I,J),FI(I,J))
          DFFT(J) = CMPLX(DR(I,J),DI(I,J))
        ENDDO
        DDT(1) = DFFT(1)*CFFT(2)*CFFT(3)
        DDT(2) = DFFT(2)*CFFT(1)*CFFT(3)
        DDT(3) = DFFT(3)*CFFT(1)*CFFT(2)
        DDT(4) = DFFT(4)*CFFT(1)*CFFT(2)
        DO J = 1, NPKGEN(JPHASE,JSOURC)
          DR(I,J) = REAL(DDT(J))
          DI(I,J) = AIMAG(DDT(J))
        ENDDO
        CFF = CFFT(1)*CFFT(2)*CFFT(3)
        FRT(I) = REAL(CFF)
        FIT(I) = AIMAG(CFF)
      ENDDO
! DO INVERSE TRANSFORMS OF FUNCTION AND DERIVATIVES
      INV = 1
      CALL FT01A(MNS,INV,FRT,FIT)
      DO J = 1, NPKGEN(JPHASE,JSOURC)
        IF (PFNVAR(J,JPHASE,JSOURC)) CALL FT01A(MNS,INV,DR(1,J),DI(1,J))
      ENDDO
! WRITE FUNCTION AND DERIVATIVES TO ARRAY PKADD
      XTEM = 1./ZXDEL(KNOW)
      DO I = 1, MNS
        II = MOD(I+MN2M1,MNS) + 1
        PKCONV(II,1) = FRT(I)*XTEM
        DO J = 1, NPKGEN(JPHASE,JSOURC)
          JJ = J + 1
          PKCONV(II,JJ) = DR(I,J)*XTEM
        ENDDO
      ENDDO

      END SUBROUTINE FCSUB3
!
!*****************************************************************************
!
