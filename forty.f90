!*==FORTY.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
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

!
!
!.. Interaction here with Winteracter !!!!!!
!
      LOGICAL DFLTPR, PRNCYC
      EXTERNAL DFLTPR, PCXX, PFXX, MAGROU, CALROU, RUNPAR, VARSPR,      &
     &         PRNCYC
!
      INCLUDE 'params.inc'
!
      CHARACTER*6 PNAME
      DIMENSION ALSQ(MATSZ)
      COMMON /DERVAR/ DERIVV(500), LVARV
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
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
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
! JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
      LOGICAL LOGIPK
      COMMON /IPKCMN/ LOGIPK, IPK, PIK(MIPK)
!
!
      COMMON /CMN299/ KIPT(MPTS), KNIPT(MAXPIK), ZNORM(MAXPIK),         &
     &                DZNDKQ(MAXPIK), DZNDVQ(9,MAXPIK), IOCCR(MPTS),    &
     &                JOCCR(MPTS)
!
      COMMON /CMNNOW/ NOBSNOW
!
!.. Note only 3 phases specifically hardwired here
      COMMON /REFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
!
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS),    &
     &                ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XDIFT, XMINT
!
      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP
!
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      CHARACTER*10 filnmr
      COMMON /commun/ filnam_root
      CHARACTER*10 filnam_root
!
!------------------ For debugging only
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6),    &
     &                DTDWL, NPKCSP(9,5), ARGMIN(5), ARGMAX(5),         &
     &                ARGSTP(5), PCON
!
!----------------------
!
!
! JCC add error handling declarations
      INTEGER IEOCC
      INTEGER PREFIN
! JCC This is for testing for mathematical errors in the CCSL that used to STOP the program
      INTEGER IBMBER
      COMMON /CCSLER/ IBMBER
!
      ICalled = 0
!
! JCC Initialise return value to successful (=1) any other value is failure
      FORTY = 1
      IPK = 0
      filnam_root = filnmr
!
      IEOCC = PREFIN(PNAME)
! Check the return status
      IF (IEOCC.NE.1) GOTO 900
!
!
! SET UP PRECISE PROBLEM, AND READ MOST L CARDS:
      CALL REFSET
! DISCOVER WHETHER SLACK CONSTRAINTS:
      CALL GEOMIN(0)
! JCC Trap for any problems
      IF (IBMBER.GT.0) GOTO 950
! THIS ROUTINE IS ONLY FOR ONE PHASE:
      CALL LOGPHA(1)
      CALL SETPR(PCXX,PFXX,MAGROU)
!
! JCC Trap for any problems
      IF (IBMBER.GT.0) GOTO 950
!
! COLLECT CONSTRAINTS IMPOSED BY SYMMETRY, AND THOSE REQUESTED, AND
! SET UP PARAMETERS AS VARIABLES (NOT YET AS BASIC VARIABLES)
      CALL PARSPR(MAGROU)
! JCC Trap for any problems
      IF (IBMBER.GT.0) GOTO 950
!
!
! MAKE LIST OF REFLECTION INDICES:
      CALL INRFPR(PCXX,PFXX)
!
! READ OBS DATA AND SEND OUT TO TEMPORARY UNIT FOR REINPUT EACH CYCLE
      CALL INOBPR(ISCR,NFLIP,PCXX,PFXX)
!
      CALL CHKMAXREF(PCXX)
!
! CALCULATE THE STEP-SIZE FOR PEAK-SHAPE CALCULATION
      CALL XDELPR
!
      CALL QNUMPP
!
! Open the .PIK file
      MESSAG = 'peak contribution (.pik) file'
      NAMFIL = '.PIK'
      IPK = 80
      CALL OPNFIL(IPK,113)
      DONE = .FALSE.
!
      DO ICYC = NCYC1, LASTCY
!      WRITE (ITO,2000) ICYC
! 2000   FORMAT (' >>> Starting cycle',I4)
!
!.. *** Winteracter calls ***
        CALL WDialogPutInteger(IDF_Pawley_Cycle_Number,ICYC)
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,LASTCY)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,NPts)
        CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,MaxK)
!
!>> JCC Add in check on number of reflections here, so that code doesnt bomb out in the Pawley attempt
        IF (MaxK.GT.400) GOTO 910
        IF (PRECYC .AND. ICYC.NE.NCYC1) THEN
          SIMUL = .FALSE.
          PRECYC = .FALSE.
        ENDIF
!
! IF NEEDED, CAIL/SAPS/APES PROCESSING ON EACH CYCLE
! FOR NOW, IMPOSE PHASE 1, SOURCE 1 WHICH WE HOPE STAY THERE:
        JPHASE = 1
        JSOURC = 1
!
!.. CALCULATE PEAK & PEAK DERIVATIVE CONTRIBUTIONS
        IF (AKNOTS.LE.0.) THEN
          CALL PFXX(2)
                     !PIKCAL
        ELSE
          CALL PFXX(3)
        ENDIF
!
!
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
!
        IF (.NOT.RIET) CALL FAM4PR(2,PCXX,PFXX)
        CALL VARMAK(DFLTPR,RUNPAR,VARSPR)
!* this won't actually do - we want varspm if magnetic
! FOR NOW, IMPOSE PHASE 1, SOURCE 1 WHICH WE HOPE STAY THERE:
        JPHASE = 1
        JSOURC = 1
!
! IF ANY SLACK CONSTRAINTS, SET UP:
        CALL GEOMCO(1)
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
!
        CALL LOGSET
!
! INITIALISE R FACTOR SUMS:
        CALL RFACS(1)
        CALL RFACPR(1,PCXX)
!
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
!
! SET UP POINTERS IN TRIANGULAR MATRIX AND CLEAR OUT LSQ MATRIX AND RHS
        CALL MATSET(ALSQ,MATSZ)
!
!>> JCC Trap for any problems
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
!..      CALL PFXX(3)
!
!...  2   READ (ISCR,END=3) ARGI,OBS,DOBS,WT,ICODE,KMIN,KMAX
!
! CLEAR YCALC TO COLLECT QUANTITY COMPARABLE WITH OBS, AND DERIVV TO COLLECT
! THE CORRESPONDING DERIVATIVES OF YCALC WRT ALL VARIABLES:
! (DONE HERE IN CASE NO CONTRIBUTING REFLNS)
!.. *** Winteracter call ***
        CALL WDialogRangeProgressBar(IDF_Pawley_Progress_Bar,1,Npts)
        Npt30 = Npts/30
        DO IPT = 1, NPTS
          IF (npt30*(ipt/npt30).EQ.ipt) THEN
            CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
          ENDIF
          YBACK = 0.
          YPEAK = 0.
          YCALC = 0.
          NOBSNOW = IPT
          KMAX = IOCCR(NOBSNOW)
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
          IF (ICODE.NE.0) THEN
!          CALL RFACPR(5,PCXX)
!          GO TO 2
          ENDIF
!
! CALCULATE FUNCTION TO MATCH OBSERVED, AND ITS DERIVATIVES, AND DO SOME
! STATISTICS:
          IF (KMAX.NE.0) CALL CALROU(PCXX,PFXX)
          CALL BACKPR(2)
          YCALC = YBACK + YPEAK
          ZCAL(NOBSNOW) = YCALC
          ZBAK(NOBSNOW) = YBACK
!
! MAKE DERIVATIVES WRT BASIC VARIABLES FROM THOSE WRT VARIABLES:
          CALL RELATE
!
! DIFFERENCE:
          IF (SIMUL) OBS = YCALC
          DIFF = OBS - YCALC
! FROM WEIGHT GET SQRTWT AND WDIFF INTO COMM0N:
          IF (NOBS.EQ.532) INOTH = 1
          CALL WGHTLS(3,ARGI)
!
! ADD IN TO R FACTORS:
          CALL RFACPR(2,PCXX)
!
!>> JCC Trap for any problems
          IF (IBMBER.GT.0) GOTO 950
! ADD DERIVATIVES IN TO LSQ MATRIX:
          CALL MATTOT(ALSQ,MATSZ)
          IF (DONE) THEN
            NTEM = IOCCR(NOBSNOW)
            K1 = KIPT(NOBSNOW) + 1
            K2 = KIPT(NOBSNOW+1)
            KD = 1 + K2 - K1
            WRITE (IPK,*) ARGI, OBS - YBACK, DOBS, NTEM
            IF (NTEM.GT.0) WRITE (IPK,*) (KNIPT(KK),PIK(KNIPT(KK)),KK=K1,K2)
          ENDIF
          NOBS = NOBS + 1
! NEXT OBSERVATION:
!...      GO TO 2
        ENDDO
             ! IPT LOOP TO NPTS
!
! HERE ON NO MORE PROFILE OBSERVATIONS - PRINT R FACTORS:
        CALL RFACPR(3,PCXX)
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! SLACK CONSTRAINTS - FIRST PAWLEY-TYPE:
        CALL PAWLS(ALSQ,MATSZ,3)
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
!* JOIN HERE IF ONLY SLACK:
! THEN GEOMETRICAL:
        CALL GEOMLS(ALSQ,MATSZ)
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! COMBINED CHI SQUARED:
        CALL RFACS(6)
! IF CAIL, OUTPUT EIGENVALUES & EIGENVECTORS IF REQUIRED BY "I PREE"
        IF (DONE) CALL EIGEN(ALSQ,MATSZ)
        IF (DONE) CALL HESCOR(ALSQ,MATSZ)
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! INVERT MATRIX:
        CALL MATINV(ALSQ,MATSZ)
! CALCULATE SHIFTS AND ESD'S:
        CALL MATSHF(ALSQ,MATSZ)
!>> JCC Trap for any problems
        IF (IBMBER.GT.0) GOTO 950
! APPLY SHIFTS AND PRINT:
        CALL APSHPR(ALSQ,MATSZ,PCXX,PFXX,MAGROU)
! IF PREVIOUS CYCLE HAD CONVERGED ACCORDING TO I CONV, FINISH:
        IF (DONE) GOTO 39
        DONE = (AMAXSH.LT.CONV .OR. ICYC.EQ.LASTCY-1 .OR. NCYC.EQ.1)
! OUTPUT NEW CRYSTAL DATA FOR PENULTIMATE CYCLE:
        IF (DONE) CALL NWINPR(PCXX,PFXX,MAGROU)
!.. *** Winteracter calls ***
        CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,PAWLEYCHISQ,'(F12.3)')
        CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RWPOBS,'(F12.2)')
        CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RWPEXP,'(F12.2)')
      ENDDO
! PRINT CORRELATION MATRIX:
   39 CALL MATCOR(ALSQ,MATSZ)
! OUTPUT H,K,L IF REQUIRED:
      CALL HKLOUT(PCXX,ALSQ,MATSZ)
      CALL CLOFIL(ISCR)
      IF (PRNCYC(4)) CALL CLOFIL(IOP2)
      CLOSE (IPK)
!>> JCC Trap for any problems
	IF (IBMBER .GT. 0) CALL DebugErrorMessage('Error in FORTY after HKLOUT')
      IBMBER = 0   ! Ignore a problem that can occur in HKLOUT - Ken/Bill to check out please
      RETURN
!>> JCC Added in error handling here
  900 CONTINUE
      FORTY = 0
      IF (IPK.NE.0) CLOSE (ipk,IOSTAT=istat)
      RETURN
  910 FORTY = -1
      IF (IPK.NE.0) CLOSE (ipk,IOSTAT=istat)
      RETURN
  950 FORTY = -2
      IF (IPK.NE.0) CLOSE (ipk,IOSTAT=istat)
      IBMBER = 0

      END FUNCTION FORTY
!*==PFCN03.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
      SUBROUTINE PFCN03(N)
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
      INCLUDE 'params.inc'
!
      PARAMETER (NW=4)
      CHARACTER*4 WDCN03(NW)
      LOGICAL TESTOV
      DIMENSION C3FN(3), C3DN(3), IWCN03(3,NW)
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5),  &
     &                DSTAR2, TWOTHD(5), DIFANG(6)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9),       &
     &                ICDN(26,9), IERR, IO10, SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
!
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
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /PRSAVZ/ PKCONV(512,9)
      COMMON /PRKNOT/ ARGKNT(50), PKKNOT(512,9,50)
!      COMMON /PRSAVF/PKLIST(512,9,200),ZXDEL(200),PKCONV(512,9),
!     & ARGNOT(50),PKNOT(64,9,50),XPDKNT(50)
      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
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
!.. Note only 3 phases specifically hardwired here
      COMMON /REFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
!
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS),    &
     &                ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)
!
      REAL ZTEM(MPPTS), RTEM(3,MRFLNZ), TF4PAR(MF4PAR)
!
      LOGICAL PFNVAR
      COMMON /PFNINF/ NUMPFP(8,9,5), PFNVAR(8,9,5)
!
      REAL ARTEM(6)
      INTEGER KORD(MRFLNZ)
!
      COMMON /CMN299/ KIPT(MPTS), KNIPT(MAXPIK), ZNORM(MAXPIK),         &
     &                DZNDKQ(MAXPIK), DZNDVQ(9,MAXPIK), IOCCR(MPTS),    &
     &                JOCCR(MPTS)                            !,
!
      COMMON /CMN300/ ZNORMT(MAXPIK), DZNDKQT(MAXPIK), DZNDVQT(9,MAXPIK)&
     &                , KARGO(MAXPIK), KARGK(MAXPIK)
!
      DATA WDCN03/'SIGM', 'GAMM', 'HPSL', 'HMSL'/
      DATA IWCN03/3, 3, 0, 3, 4, 0, 3, 5, 0, 3, 6, 0/
!
      GOTO (10,1,2,3,100,5,6,7), N + 1
!
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
! Peak positions may have changed - check and re-sort if necessary
      DO IR = 1, MAXK
        KNOW = IR
        CALL CELDER(REFH(1,KNOW),ARTEM)
        DSTAR(KNOW) = SQRT(DSTAR2)
        CALL PCXX(5)
        ZARGK(IR) = ARGK
      ENDDO
      CALL SORTX(ZARGK,KORD,MAXK)
! Set the number of peaks at each point to zero
      DO II = 1, NPTS
        IOCCR(II) = 0
      ENDDO
      DO IR = 1, MAXK
        IRT = KORD(IR)
        ZTEM(IR) = ZARGK(IRT)
        DO I = 1, 3
          RTEM(I,IR) = REFH(I,IRT)
        ENDDO
        IF (CAIL) THEN
          TF4PAR(IR) = F4PAR(1,IRT)
        ENDIF
      ENDDO
      DO IR = 1, MAXK
        ZARGK(IR) = ZTEM(IR)
        DO I = 1, 3
          REFH(I,IR) = REFH(I,IR)
        ENDDO
        IF (CAIL) THEN
          F4PAR(1,IR) = TF4PAR(IR)
        ENDIF
      ENDDO
      IOBS = 1
      DO IRT = 1, MAXK
!        IR=KORD(IRT)
        IR = IRT
        DO I = IOBS, NPTS
          IF (ZARGI(I).GT.ZARGK(IR)) THEN
            KOBZ(IR) = I
            GOTO 30
          ENDIF
        ENDDO
   30   CONTINUE
      ENDDO
!      IPOINT(1)=0
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
 1510   IMAX = MN
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
          IOCCR(II) = IOCCR(II) + 1
          IF (IOCCR(II).EQ.1) JOCCR(II) = KOUNT
          TARGI = ZARGI(II)
          TARGK = ZARGK(KNOW)
          DTARG = TARGI - TARGK
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
          JARGI = NINT((DTARG)/ZXDEL(KNOW))
          IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
          POFF = DTARG/ZXDEL(KNOW) - FLOAT(JARGI)
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
        KIPT(II+1) = KIPT(II) + IOCCR(II)
        IF (IOCCR(II).NE.0) THEN
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
              IF (KK.EQ.IOCCR(II)) GOTO 680
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
        CALL CELDER(REFH(1,KNOW),ARTEM)
        DSTAR(KNOW) = SQRT(DSTAR2)
        CALL PCXX(5)
        ZARGK(IR) = ARGK
      ENDDO
      CALL SORTX(ZARGK,KORD,MAXK)
! Set the number of peaks at each point to zero
      DO II = 1, NPTS
        IOCCR(II) = 0
      ENDDO
      DO IR = 1, MAXK
        IRT = KORD(IR)
        ZTEM(IR) = ZARGK(IRT)
        DO I = 1, 3
          RTEM(I,IR) = REFH(I,IRT)
        ENDDO
        IF (CAIL) THEN
          TF4PAR(IR) = F4PAR(1,IRT)
        ENDIF
      ENDDO
      DO IR = 1, MAXK
        ZARGK(IR) = ZTEM(IR)
        DO I = 1, 3
          REFH(I,IR) = RTEM(I,IR)
        ENDDO
        IF (CAIL) THEN
          F4PAR(1,IR) = TF4PAR(IR)
        ENDIF
      ENDDO
      IOBS = 1
      DO IRT = 1, MAXK
!        IR=KORD(IRT)
        IR = IRT
        DO I = IOBS, NPTS
          IF (ZARGI(I).GT.ZARGK(IR)) THEN
            KOBZ(IR) = I
            GOTO 3030
          ENDIF
        ENDDO
 3030   CONTINUE
      ENDDO
! Now do the knots
      KOUNT = 0
      IF (AKNOTS.LE.1.) THEN
        KNOTS = AKNOTS*MAXK
      ELSE
        KNOTS = AKNOTS
      ENDIF
      KNOTS = MIN(50,KNOTS)
      KNOTS = MAX(5,KNOTS)
      TTMIN = MIN(ZARGK(1),ZARGK(MAXK))
      TTMAX = MAX(ZARGK(1),ZARGK(MAXK))
      TTDIF = TTMAX - TTMIN
      TTINC = TTDIF/FLOAT(KNOTS-1)
      DO KNOW = 1, KNOTS
        ARGK = TTMIN + FLOAT(KNOW-1)*TTINC
        ARGKNT(KNOW) = ARGK
        CALL FDCN03(2)
! FFT CALCULATION STAGE IN PROFILE REFINEMENT
! THE INDIVIDUAL COMPONENTS FOR CONVOLUTION ARE IMMEDIATELY
! DESCRIBED IN FOURIER SPACE (GETS RID OF DISCONTINUITY PROBLEMS)
        CALL FCSUB3(MN)
        MN21 = MN2 - 1
        DO II = 1, MN
          PKKNOT(II,1,KNOW) = PKCONV(II,1)
          XDIFT = ZXDEL(1)*(II-MN21)
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
        JK = 1
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
        C3FNM = 0.5*POFF*(POFF-1.)
        C3FN0 = 1. - POFF**2
        C3FNP = 0.5*POFF*(POFF+1.)
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
            GOTO 3510
          ENDIF
        ENDDO
 3510   IMAX = MN
        DO I = MN - 1, 1, -1
          IF (PKCONV(I,1).GE.PKCT) THEN
            IMAXT = I
            GOTO 3520
          ENDIF
        ENDDO
 3520   ARIMIN = ARGK + ZXDEL(KNOW)*FLOAT(IMINT-IPMAX)
        ARIMAX = ARGK + ZXDEL(KNOW)*FLOAT(IMAXT-IPMAX)
        IIMAX = NPTS
        DO I = KOBZ(KNOW), NPTS
          IF (ZARGI(I).GE.ARIMAX) THEN
            IIMAX = I
            GOTO 3542
          ENDIF
        ENDDO
 3542   IIMIN = 1
        DO I = KOBZ(KNOW), 1, -1
          IF (ZARGI(I).LE.ARIMIN) THEN
            IIMIN = I
            GOTO 3544
          ENDIF
        ENDDO
 3544   DO II = IIMIN, IIMAX
          KOUNT = KOUNT + 1
          KARGO(KOUNT) = II
          KARGK(KOUNT) = KNOW
          IOCCR(II) = IOCCR(II) + 1
          IF (IOCCR(II).EQ.1) JOCCR(II) = KOUNT
          TARGI = ZARGI(II)
          TARGK = ZARGK(KNOW)
          DTARG = TARGI - TARGK
! DO THE INTERPOLATIONS FOR YNORM AND DERIVATIVES FROM PKLIST
          JARGI = NINT((DTARG)/ZXDEL(KNOW))
          IARGI = JARGI + MN2 + 1
! WORK OUT ARGI OFFSET FROM "X(JARGI)" FOR INTERPOLATION
          POFF = DTARG/ZXDEL(KNOW) - FLOAT(JARGI)
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
            III = IARGI + I - 2
! JCC @ Can get an array bound overflow error here so I've trapped for it temporarily.
! May want to check out why ...
! Added a hack ...
            IF (III.LE.512 .AND. III.GT.0) THEN
              PKTEM = PKCONV(III,1)
              YNORM = YNORM + C3FN(I)*PKTEM
              DYNDKQ = DYNDKQ - C3DN(I)*PKTEM
              DO NPKD = 1, NPKGEN(JPHASE,JSOURC)
                NPKD1 = NPKD + 1
                DYNDVQ(NPKD) = DYNDVQ(NPKD) + C3FN(I)*PKCONV(III,NPKD1)
              ENDDO
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
        KIPT(II+1) = KIPT(II) + IOCCR(II)
        IF (IOCCR(II).NE.0) THEN
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
              IF (KK.EQ.IOCCR(II)) GOTO 3680
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
!
    7 CALL FDCN03(1)
      F4PAR(2,KNOW) = PKFNVA(1)
      F4PAR(3,KNOW) = PKFNVA(2)
      GOTO 100
  100 RETURN

      END SUBROUTINE PFCN03
!*==FCSUB3.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
      SUBROUTINE FCSUB3(MNS)
!
!
!X   For use with PFCN03 constant wavelength data with finite detector height
!C 19B
!H
      INCLUDE 'PARAMS.INC'
!
      LOGICAL NEAR90
      COMPLEX CFFT, DFFT, DDT, CFF
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
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
      COMMON /PRSAVZ/ PKCONV(512,9)
!      COMMON /PRSAVF/PKLIST(512,9,200),ZXDEL(200),PKCONV(512,9),
!     & ARGNOT(50),PKNOT(64,9,50),XPDKNT(50)
      COMMON /REFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
      INCLUDE 'REFLNS.INC'
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
      DIMENSION CFFT(8), DFFT(8), DDT(8), FR(512,8), FI(512,8),         &
     &          DR(512,8), DI(512,8), FRT(512), FIT(512)
      LOGICAL PFNVAR
      COMMON /PFNINF/ NUMPFP(8,9,5), PFNVAR(8,9,5)
!
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
