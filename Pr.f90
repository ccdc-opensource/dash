!
!*****************************************************************************
!
      SUBROUTINE ABCRPR(N)
!
! *** ABCRPR updated by WIFD 22 Feb 89 ***
!
!H Multiple entry routine to deal with all aspects of absorption correction
!H for Profile Refinement
!A On entry N gives action required:
!A    N=1 interpret L ABSC card
!A    N=2 calculate absorption correction and its parameter's derivative
!A    N=3 apply shift to parameter
!A    N=4 write out new L ABSC card
!A    N=5 deal with absence of L ABSC card
!A    N=6 fix parameter if no card given
!
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

      COMMON /PRABSC/ NABTYP(5), ABSPR(2,5), KABSPR(2,5), ABSCOR,       &
     &                DERABQ(2), NABSPR(5)
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      GOTO (1,2,3,4,5,6), N
      GOTO 100
! HAVE IN COMM0N /SCRACH/ A CARD STARTING 'L ABSC' - READ REST:
    1 CALL RDINTG(NABTYP(JSOURC),7,IPT,80,IER)
      IF (NABTYP(JSOURC).EQ.0) THEN
        CALL MESS(LPT,0,'No absorption correction')
        GOTO 50
      ENDIF
      CALL RDNUMS(ABSPR(1,JSOURC),IPT,2,NABSPR(JSOURC),IER)
      WRITE (LPT,2000) (ABSPR(II,JSOURC),II=1,NABSPR(JSOURC))
 2000 FORMAT (/' Absorption coefficient(s)=',2F12.4)
      IF (ABS(ABSPR(1,JSOURC))+ABS(ABSPR(2,JSOURC)).EQ.0.) GOTO 10
      GOTO 100
! CALCULATE FUNCTION WHICH WILL BE P4 IN CALxx, AND ITS DERIVATIVE
    2 IF (NABTYP(JSOURC).EQ.0) GOTO 100
! THIS BIT NOT YET WRITTEN FOR CN:
      IF (.NOT.TOF) GOTO 10
! TIME OF FLIGHT:
      WABSPR = WLGTH*ABSPR(2,JSOURC) + ABSPR(1,JSOURC)
      IF (WABSPR.LE.0.001) THEN
        WA = WABSPR/3.
        ABSCOR = 1. - WABSPR*(1.-2.*WA)
        DERABQ(1) = (4.*WA-1.)/ABSCOR
      ELSE
        EXPABS = EXP(-2.*WABSPR)
        ABSCOR = (1.-EXPABS)/(2.*WABSPR)
        DERABQ(1) = (EXPABS/ABSCOR-1.)/WABSPR
      ENDIF
      DERABQ(2) = DERABQ(1)*WLGTH
      GOTO 100
! APPLY SHIFT IN COEFFICIENT:
    3 IF (JPHASE.EQ.1) CALL ADJUST(ABSPR(ISPC,JSOURC))
      GOTO 100
! WRITE OUT NEW 'L ABSC' CARD FOR PR:
    4 WRITE (NEWIN,2001) NABTYP(JSOURC), ABSPR(1,JSOURC), ABSPR(2,JSOURC)
 2001 FORMAT ('L ABSC',I2,2F10.4)
!* NEED METHOD OF PUTTING OUT *S
      GOTO 100
! DEAL WITH ABSENCE OF 'L ABSC' CARD:
    5 CALL MESS(LPT,1,'No L ABSC card - assuming no absorption correction')
      NABTYP(JSOURC) = 0
   50 ABSPR(1,JSOURC) = 0.
      ABSPR(2,JSOURC) = 0.
   10 ABSCOR = 1.
      DERABQ(1) = 0.
      DERABQ(2) = 0.
      GOTO 100
! FIX ABS COR IF NO CARD WAS GIVEN:
    6 IF (NABTYP(JSOURC).EQ.0) CALL ADDFX5(6,2,0,1,JSOURC,4)
      GOTO 100
      ENTRY ABCPR8(NP,NV)
! RECORD NP'TH PARAMETER AS VARIABLE NUMBER NV:
      KABSPR(NP,JSOURC) = NV
      GOTO 100
      ENTRY ABCPR9
! RECORD ALL PARAMETERS FIXED:
      DO I = 1, 2
        DO J = 1, NSOURC
          KABSPR(I,J) = 0
        ENDDO
      ENDDO
  100 RETURN

      END SUBROUTINE ABCRPR
!
!*****************************************************************************
!
      SUBROUTINE ADDPAW(NSLTYP,ISGEN,ISP,AM,WEIGHT)
!
! *** ADDPAW updated by JCM 8 Mar 89 ***
!
!C 19B
!H Add slack constraint to list for Pawley type constrained PR
!A On entry NSLTYP=type of constraint:
!A      NSLTYP=3 means 2 constrained clumps of intensities as found by FAM4PR
!A On entry ISGEN  is the GENUS of the variable to be slackly constrained.
!A On entry ISP(1) is the species of the first intensity of the first clump
!A          ISP(2) is the species of the first intensity of the second clump
!A          AM  is a 1x2 real array holding the coefficents of the constraint
!A          WEIGHT is to weight this "observation" by 1/sigma squared.
!
!D Adds constraint to list for type NSLTYP slack constraints
!N Only at present for NSLTYP=3, so expects specifically only 2 clumps of
!N intensities to be linked.
!
      DIMENSION ISP(2), AM(2)
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP, NSKTOT, KOM24

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      NSKTOT = NSKTOT + 1
      CALL ERRCHK(2,NSLAK(NSLTYP),1000,0,'Pawley-type slack constraints')
      IF (IBMBER .NE. 0) RETURN
      NSLTEM = NSLAK(NSLTYP)
      IGSLAK(NSLTEM) = ISGEN
      ISPSLK(1,NSLTEM) = ISP(1)
      ISPSLK(2,NSLTEM) = ISP(2)
      AMSLAK(1,NSLTEM) = AM(1)
      AMSLAK(2,NSLTEM) = AM(2)
      WTSLAK(NSLTEM) = WEIGHT

      END SUBROUTINE ADDPAW
!
!*****************************************************************************
!
      SUBROUTINE ADF4G2(PAR)
!
! *** ADF4G2 by WIFD 2 Jun 89 ***
!
!H Like ADJUST, but special to deal with family 4, genus 2
!A On entry PAR is the parameter to be updated
!
      INCLUDE 'PARAMS.INC'

      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      XOLD = PAR
      XTEM = (XOLD+SHIFT)/F4VAL(IGEN,ISPC)
      ARG = 0.2*(XTEM-1.)
      SHINE = (ARG+SQRT(ARG*ARG+1.))**25
      XNEW = 0.2*F4VAL(IGEN,ISPC)*ALOG(1.+148.4131591*SHINE)
      PAR = XNEW

      END SUBROUTINE ADF4G2
!
!*****************************************************************************
!
      SUBROUTINE APSHPR(ALSQ,MATSZ,PCXX,PFXX,MAGSHF)
!
! *** APSHPR updated by PJB 1 Feb 1994 ***
!
!X
!C 19B
!H Applies shifts to parameters for PR with slack constraints
!
!P On entry, array BLSQ holds shifts, and DERIVB holds esds
!D Deals with redundant variables by interpreting constraints
!D Finds and applies fudge factors
!D Identifies species of parameter, then calls specific routines actually
!D    to apply shifts, thus not needing all COMMONs to be explicit here
!D
!D If slack constraints are used, just before exit recalculates ends of bonds
!O Prints original value, shift, esd and new value for each basic variable
!O Puts this printing in lines for everything except family 2 parameters,
!O    and in clumps per atom for family 2
!
      EXTERNAL PCXX, PFXX, MAGSHF
      LOGICAL SHFCEL, HEAD, NPROP
      DIMENSION DPROP(3)
      CHARACTER*4 LNAM1, LNAM2
      COMMON /ATBLOC/ NAME, IPNAME(12)
      CHARACTER*4 NAME, IPNAME
      COMMON /ATBLOK/ IBUFF, PNEW(12), PESD(12), PSHIFT(12), POLD(12), PSESD(12)
      COMMON /DERBAS/ DERIVB(400), LVARB
      COMMON /DERVAR/ DERIVV(500), LVARV
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /MATDAT/ MATPNT(401), BLSQ(400)
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP, NSKTOT, KOM24
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (SIMUL) GOTO 100
      WRITE (LPT,2000) ICYC
 2000 FORMAT (/////'1 ++++++++++ Shifts in variables for cycle',I3,' ++++++++++ ')
! START PRINT CONTROL:
      IG = 0
      IBUFF = 0
      HEAD = .FALSE.
      JPHASE = 0
      JSOURC = 0
      SHFCEL = .FALSE.
! CLEAR SHIFT AVERAGING:
      CALL FETSHF(1,0.,0.)
! SCAN ALL VARIABLES:
      DO I = 1, LVARV
! J=WHICH BASIC VARIABLE IS THIS I'TH VARIABLE
        J = LVRBS(I)
! KPACK=PACKED SPEC OF THIS PARAMETER
        KPACK = LVRPR(I)
        IF (J.LE.0) THEN
! VARIABLE IS REDUNDANT, BY CONSTRAINT NUMBER -J:
          CALL SHFESD(-J)
        ELSE
! BASIC VARIABLE:
          SHIFT = BLSQ(J)
          ESD = DERIVB(J)
        ENDIF
! KEEP CURRENT PHASE & SOURCE:
        JP = JPHASE
        JS = JSOURC
! UNPACK PARAMETER SPEC:
        CALL KUNPAK(KPACK,IFAM,IGEN,ISPC,JPHASE,JSOURC)
        IF (JPHASE.NE.IPHASE) THEN
! NEW PHASE:
          IF (MULFAS) THEN
! TIDY PREVIOUS:
            IF (SHFCEL) THEN
              CALL RECELL(1,1)
              CALL CELSDP(ALSQ,MATSZ)
            ENDIF
! ADJUST ENDS OF ANY BONDS INVOLVING ALTERED COORDINATES:
            CALL GEOMCO(2)
            CALL PHMOVE(-1,IPHASE)
            WRITE (LPT,2011) JPHASE
 2011       FORMAT (/' Phase',I3,' :')
            CALL PHMOVE(1,JPHASE)
            CALL LOGPHA(JPHASE)
          ENDIF
          SHFCEL = .FALSE.
        ENDIF
        IF (IFAM.EQ.3 .OR. IFAM.EQ.6 .AND. (JS.NE.JSOURC)) CALL LOGSOU(JSOURC)
! BRANCH ON FAMILY:
        GOTO (11,12,13,14,15,16), IFAM
        CALL ERRMES(-1,0,'family too big in APSHPR')
        IF (IBMBER .NE. 0) RETURN
! FAMILY 1, GENUS 1 - MISCELLANEOUS SINGLY NAMED SPECIES (TFAC, A* ETC,
! EXTN,PROR,SPHA)
   11   GOTO (31,35,35,35,35,35,35,36,37,38,39,39,39), ISPC
        CALL ERRMES(-1,0,'species too big in APSHPR')
        IF (IBMBER .NE. 0) RETURN
! TFAC:
   31   CALL LLTFAC(3)
        GOTO 40
! FAMILY 1 GENUS 1 ALSO CONTAINS THE CELL PARAMETERS:
   35   CALL CELSHF(ISPC-1)
        SHFCEL = .TRUE.
        GOTO 40
! EXTINCTION CORRECTION PARAMETER EXTN:
   36   CALL EXCRPR(3)
        GOTO 40
! PREFERRED ORIENTATION
   37   CALL PREFOR(3)
        GOTO 40
! FAMILY 1, GENUS 1, SPECIES 10 - SCALE FOR PHASE, SPHA:
   38   CALL LPSCAL(3)
        GOTO 40
! FAMILY 1 GENUS 1, SPECIES 11,12,13 PROPAGATION VECTOR
   39   CALL PROPAG(3,ISPC-10)
        DPROP(ISPC-10) = SHIFT
        NPROP = .TRUE.
        GOTO 40
! FAMILY 6: MISCELLANEOUS SOURCE DEPENDENT;
   16   GOTO (61,62,63), IGEN
        CALL ERRMES(-1,0,'genus too big in APSHPR')
        IF (IBMBER .NE. 0) RETURN
! FAMILY 6 GENUS 1 - SINGLY NAMED, SOURCE-DEPENDENT SPECIES (SCAL,TTHM)
   61   GOTO (51,52), ISPC
        CALL ERRMES(-1,0,'species too big in APSHPR')
        IF (IBMBER .NE. 0) RETURN
! FAMILY 6, GENUS 1, SPECIES 1 - SCALE FOR SOURCE, SCAL:
   51   CALL LSSCAL(3)
        GOTO 40
! MONOCHROMATOR 2 THETA FOR LX:
   52   CALL TTHMLX(3)
        GOTO 40
! FAMILY 6, GENUS 2 - ABSC:
   62   CALL ABCRPR(3)
        GOTO 40
! FAMILY 6, GENUS 3 - BACK:
   63   CALL BACKPR(3)
        GOTO 40
! FAMILY 2 - THESE ARE ALL TO DO WITH THE STRUCTURE FACTOR:
! STOP APPLYING THE SHIFT MORE THAN ONCE TO RELATED SCATS:
   12   IF (J.LT.0 .AND. ISPC.EQ.10) GOTO 1
        IF (ISPC.LE.12) CALL F2SHFT
        IF (ISPC.GT.12) CALL MAGSHF(3)
        GOTO 40
! FAMILY 3 - ZERO POINT, PEAK CENTRE AND PEAK FUNCTION PARAMETERS:
! IF ANY FAMILY 2 SHIFTS LEFT IN BUFFER, PRINT THEM:
   13   CALL PRBLOK
! GENUS 1=ZERO POINT, 2=PEAK CENTRE, REST ARE PEAK FUNCTION:
        GOTO (41,42), IGEN
        GOTO 43
! JvdS this seems to be where the results of the Pawley Refinement are written to the global variables
! ZERO:
   41   CALL ZEROPR(3)
        GOTO 40
! PEAK CENTRE PARAMETERS DEPEND ON TYPE OF REFINEMENT:
   42   CALL PCXX(3)
        GOTO 40
! REMAINING PEAK FUNCTION PARAMETERS:
   43   CALL PFALL(3)
        GOTO 40
! FAMILY 4 - LONG VECTORS (SO FAR, INTS, SIGS, GAMS . . IN PAWLEY)
   14   CALL FAM4PR(3,PCXX,PFXX)
        GOTO 40
   15   CONTINUE
! FAMILY 5 ARE MULTIPOLES, EXCLUDED FOR NOW:
        GOTO 1
! COMM0N EXIT TO PRINT SHIFTS:
   40   CALL FETSHF(2,SHIFT,ESD)
        CALL PARNAM(LNAM1,LNAM2,3,KPACK)
        IF (IFAM.NE.2) GOTO 6
! FOR FAMILY 2, DETECT CHANGE OF GENUS (ATOM)
        IF (IG.NE.IGEN) THEN
          HEAD = .FALSE.
          CALL PRBLOK
! PUT FIRST ENTRY FOR NEW ATOM INTO BUFFERS:
          NAME = LNAM1
          IG = IGEN
        ENDIF
        IF (IBUFF.GE.12) CALL PRBLOK
        IBUFF = IBUFF + 1
        IPNAME(IBUFF) = LNAM2
        PNEW(IBUFF) = XNEW
        PESD(IBUFF) = ESD
        PSHIFT(IBUFF) = SHIFT
        POLD(IBUFF) = XOLD
        PSESD(IBUFF) = SHESD
        GOTO 1
! HERE TO PRINT NOT FAMILY 2 SHIFT AS BEFORE:
    6   IF (.NOT.HEAD) CALL MESS(LPT,1,'  Variable       New      '//   &
     &        '     Esd          Shift          Old          Shift/Esd ')
        HEAD = .TRUE.
        WRITE (LPT,2006) LNAM1, LNAM2, XNEW, ESD, SHIFT, XOLD, SHESD
 2006   FORMAT (' ',1X,A4,1X,A4,5G14.5)
    1 ENDDO
      CALL PRBLOK
      IF (SHFCEL) THEN
        CALL RECELL(1,1)
        CALL CELSDP(ALSQ,MATSZ)
      ENDIF
      IF (NPROP) CALL REINDX(DPROP)
! ADJUST ENDS OF ANY BONDS INVOLVING ALTERED COORDINATES:
      CALL GEOMCO(2)
      CALL PHMOVE(-1,NPHASE)
      CALL FETSHF(3,0.,0.)
  100 RETURN

      END SUBROUTINE APSHPR
!
!*****************************************************************************
!
      SUBROUTINE BACKIN
!
! *** BACKIN by JCM 18 Jun 85 ***
!
! INTERPRETS ONE 'L BACK' CARD - USED IN PROFILE REFINEMENT INPUT
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      IPT = 7
      N = NBACK(KSOURC) + 1
! IF NOT FIRST L BACK CARD READ BRANCH ON WHETHER IBACK .GT./.LT. 0
      IF (ILREAD(11).GT.1 .AND. IBACK.LT.0) GOTO 150
      IF (ILREAD(11).GT.1 .AND. IBACK.GT.0) GOTO 200
! IF FIRST L BACK CARD FIND OUT WHAT IBACK IS
      IF (ILREAD(11).EQ.1) CALL RDINTG(IBACK,IPT,IPT,80,IER)
! BRANCH ON WHETHER IBACK .GT./.LT. 0
      IF (IBACK.LT.0) GOTO 160
! IBACK .GT. 0 :      BACKGROUND REFINEMENT
      WRITE (LPT,2500) IBACK
 2500 FORMAT (/' Background refinement type ',I5)
      GOTO (310,320,330,340), IBACK
      CALL ERRIN2(IBACK,1,'Type of background refinement','illegal - 1, 2, 3 or 4 only')
      GOTO 100
  310 CALL MESS(LPT,1,'Polynomial function')
      GOTO 400
  320 CALL MESS(LPT,1,'Chebyschev polynomial (1st kind) function')
      GOTO 400
  330 CALL MESS(LPT,1,'Chebyschev polynomial (2nd kind) function')
      GOTO 400
  340 CALL MESS(LPT,1,'Fourier cosine series function')
      GOTO 400
  400 NBK = 0
! 1ST BACKGROUND CARD NOW JOINS UP WITH FURTHER BACKGROUND CARDS
  200 NBK = NBK + 1
      LBKD(NBK) = N
      CALL RDNUMS(BACKGD(N,KSOURC),IPT,20,NUM,IER)
      IF (IER.NE.0) IERR = IERR + 1
      NBACK(KSOURC) = NBACK(KSOURC) + NUM
      IER = IERR
      CALL ERRCHK(1,NBACK(KSOURC),100,1,'background parameters')
      IF (IER.NE.IERR) GOTO 100
      LBKD(NBK+1) = NBACK(KSOURC) + 1
      CALL MESS(LPT,1,'Background parameter(s) :')
      CALL PRILIS(BACKGD(1,KSOURC),N,NBACK(KSOURC))
      GOTO 100
! NOT 1ST CARD AND IBACK .LT. 0 : READ TYPE OF BACKGROUND WANTED -
! OCCURS ON EVERY CARD, SAME ON EACH IF IBACK .LT. 0:
  150 CALL RDINTG(IB,IPT,IPT,80,IER)
! 1ST CARD AND IBACK .LT. 0 JUMPS TO THIS POINT
  160 IF (NBACK(KSOURC).EQ.0) GOTO 2
      IF (IB.EQ.IBACK) GOTO 1
      WRITE (LPT,3001) IBACK, IB
      IERR = IERR + 1
      GOTO 100
    2 IF ((IBACK.GE.0) .OR. (IBACK.LE.-3)) THEN
        CALL ERRIN2(IBACK,2,'type of background approximation','not allowed - only -1 or -2')
        GOTO 100
      ENDIF
! READ NEXT NUMBER PAIR ON CARD:
    1 CALL RDREAL(ARGBAK(NBACK(KSOURC)+1,KSOURC),IPT,IPT,80,IER)
! JUMP ON END OF CARD INFORMATION:
      IF (IER.EQ.100) GOTO 7
      IER = IERR
      CALL ERRCHK(2,NBACK(KSOURC),100,1,'background table entries')
      IF (IER.NE.IERR) GOTO 100
      CALL RDREAL(BACKGD(NBACK(KSOURC),KSOURC),IPT,IPT,80,IER)
      IF (IER.NE.100) GOTO 6
      CALL ERRRE2(ARGBAK(NBACK(KSOURC),KSOURC),2,'no background value with arg=',' ')
      GOTO 10
    6 IF (NBACK(KSOURC).EQ.1) GOTO 1
      IF (ARGBAK(NBACK(KSOURC),KSOURC).GT.ARGBAK(NBACK(KSOURC)-1,KSOURC)) GOTO 1
      WRITE (LPT,3004) ARGBAK(NBACK(KSOURC),KSOURC), ARGBAK(NBACK(KSOURC)-1,KSOURC)
      IERR = IERR + 1
      GOTO 100
    7 IF (NBACK(KSOURC).EQ.N-1) THEN
        CALL ERRMES(1,1,'no non-zero numbers given on L BACK card')
        GOTO 100
      ENDIF
   10 IF (N.NE.1) GOTO 3
      WRITE (LPT,2000) IBACK
 2000 FORMAT (/' Background approximation type ',I5)
      GOTO (11,12), -IBACK
      CALL ERRIN2(IBACK,2,'Type of background','illegal')
      GOTO 100
   11 CALL MESS(LPT,1,'Linear interpolation required')
      GOTO 4
   12 CALL MESS(LPT,1,'Cubic spline approximation to be used')
    4 CALL MESS(LPT,1,'Argument    Background')
    3 WRITE (LPT,2001) (ARGBAK(J,KSOURC),BACKGD(J,KSOURC),J=N, NBACK(KSOURC))
 2001 FORMAT (1X,F12.2,F12.2)
      GOTO 100
  100 RETURN
 3001 FORMAT (/' ERROR ** background type altered from',I4,' to',I4)
 3004 FORMAT (/' ERROR ** arguments for backgrounds not in strictly ascending order'/' ',F12.2,' follows',F12.2)

      END SUBROUTINE BACKIN
!
!*****************************************************************************
!
      SUBROUTINE BACKPR(N)
!
! *** BACKPR by WIFD 20 Aug 86
!
! MULTIPLE ENTRY ROUTINE TO DEAL WITH ALL ASPECTS OF BACKGROUNDS FOR CN
!
! ON ENTRY, N=1 MEANS INTERPRET L BACK CARD
!           N=2
!             IBACK .LT. 0
!               MEANS SET YBACK (IN COMM0N /OBSCAL) TO BE BACKGROUND AT
!               ARGI (IN COMMON /PRPKFN)
!             IBACK .GT. 0
!               MEANS CALCULATE YBACK AND ITS DERIVATIVES
!           N=3 APPLICABLE ONLY FOR IBACK .GT. 0
!               MEANS APPLY SHIFTS TO BACKGROUND PARAMETERS
!           N=4 APPLICABLE ONLY FOR IBACK .GT. 0
!               MEANS WRITE OUT NEW L BACK CARD
!               (IBACK .LT. 0 LEADS TO TRIVIAL OUTPUT OF LBACK CARD
!
! USES IBACK TO INDICATE MODE OF FINDING BACKGROUND;
! IF IBACK .LT. 0 THEN BACKGROUND APPROXIMATION IS REQUIRED.
! AT PRESENT IBACK=-1 OR -2
! AVAILABLE,     -1  MEANING LINEAR INTERPOLATION IN TABLE USING ALNINT
!                -2  MEANING USE CUBIC SPLINE PREVIOUSLY SET UP
!                (0 WILL MEAN READ VALUES FROM FILE ?)
! IF IBACK .GT. 0 THEN BACKGROUND REFINEMENT IS REQUIRED
!
!
      INCLUDE 'PARAMS.INC'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      COMMON /DERVAR/ DERIVV(500), LVARV

      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      INTEGER         IBACK, NBACK
      REAL                             ARGBAK,        BACKGD
      INTEGER         KBCKGD,        NBK, LBKD
      LOGICAL                                       ZBAKIN
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
                      KBCKGD(100,5), NBK, LBKD(20), ZBAKIN

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))

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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      REAL            SCOEFF
      COMMON /SPLBCK/ SCOEFF(100,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      GOTO (1,2,3,4), N
      RETURN
! READ ONE L BACK CARD:
    1 CALL BACKIN
      RETURN
! CALCULATE YBACK VALUE AT ARGI: BIFURCATE ON WHETHER IBACK LT/EQ/GT 0
    2 IF (IBACK) 200, 250, 300
! BACKGROUND INTERPOLATION APPROXIMATIONS
  200 GOTO (11,12), -IBACK
   11 YBACK = ALNINT(ARGBAK(1,JSOURC),BACKGD(1,JSOURC),ARGI,NBACK(JSOURC))
      RETURN
   12 YBACK = SPLINT(-1,NBACK(JSOURC),ARGBAK(1,JSOURC),BACKGD(1,JSOURC),SCOEFF(1,JSOURC),ARGI)
      RETURN
  250 CONTINUE
! BACKGROUND READ FROM FILE
      RETURN
! BACKGROUND AND DERIVATIVES CALCULATED FROM BACKGROUND FUNCTION
  300 GOTO (310,320,330,340), IBACK
! SIMPLE POLYNOMIAL MODEL
  310 YBACK = 0.
      AMUL = 1.
      BMUL = (ARGI-ARGMIN(JSOURC))/(ARGMAX(JSOURC)-ARGMIN(JSOURC))
      DO I = 1, NBACK(JSOURC)
        YBACK = YBACK + BACKGD(I,JSOURC)*AMUL
        L = KBCKGD(I,JSOURC)
        IF (L.NE.0) DERIVV(L) = DERIVV(L) + AMUL
        AMUL = AMUL*BMUL
      ENDDO
      RETURN
! CHEBYSCHEV POLYNOMIAL (OF THE FIRST KIND) MODEL
  320 YBACK = BACKGD(1,JSOURC)
      L = KBCKGD(1,JSOURC)
      IF (L.NE.0) DERIVV(L) = DERIVV(L) + 1.
      IF (NBACK(JSOURC).LE.1) RETURN
! XCHEB IS DEFINED BETWEEN -1 AND 1
      XCHEB = (2.*ARGI-(ARGMAX(JSOURC)+ARGMIN(JSOURC))) /(ARGMAX(JSOURC)-ARGMIN(JSOURC))
      CHEB0 = 0.
      CHEB1 = 1.
      DO I = 2, NBACK(JSOURC)
        CHEB2 = 2.*XCHEB*CHEB1 - CHEB0
        IF (I.EQ.2) CHEB2 = 0.5*CHEB2
        YBACK = YBACK + BACKGD(I,JSOURC)*CHEB2
        L = KBCKGD(I,JSOURC)
        IF (L.NE.0) DERIVV(L) = DERIVV(L) + CHEB2
        CHEB0 = CHEB1
        CHEB1 = CHEB2
      ENDDO
      RETURN
! CHEBYSCHEV POLYNOMIAL (OF THE SECOND KIND) MODEL
  330 YBACK = BACKGD(1,JSOURC)
      L = KBCKGD(1,JSOURC)
      IF (L.NE.0) DERIVV(L) = DERIVV(L) + 1.
      IF (NBACK(JSOURC).LE.1) RETURN
! XCHEB IS DEFINED BETWEEN -1 AND 1
      XCHEB = (2.*ARGI-(ARGMAX(JSOURC)+ARGMIN(JSOURC))) /(ARGMAX(JSOURC)-ARGMIN(JSOURC))
      CHEB0 = 0.
      CHEB1 = 1.
      DO I = 2, NBACK(JSOURC)
        CHEB2 = 2.*XCHEB*CHEB1 - CHEB0
        YBACK = YBACK + BACKGD(I,JSOURC)*CHEB2
        L = KBCKGD(I,JSOURC)
        IF (L.NE.0) DERIVV(L) = DERIVV(L) + CHEB2
        CHEB0 = CHEB1
        CHEB1 = CHEB2
      ENDDO
      RETURN
! FOURIER COSINE SERIES
  340 YBACK = BACKGD(1,JSOURC)
      L = KBCKGD(1,JSOURC)
      IF (L.NE.0) DERIVV(L) = DERIVV(L) + 1.
      IF (NBACK(JSOURC).LE.1) RETURN
! XTEM IS DEFINED BETWEEN 0 AND TWOPI
      XTEM = (ARGI-ARGMIN(JSOURC))/(ARGMAX(JSOURC)-ARGMIN(JSOURC))
      DO I = 2, NBACK(JSOURC)
        COSNX = COS(TWOPI*FLOAT(I-1)*XTEM)
        YBACK = YBACK + BACKGD(I,JSOURC)*COSNX
        L = KBCKGD(I,JSOURC)
        IF (L.NE.0) DERIVV(L) = DERIVV(L) + COSNX
      ENDDO
      GOTO 100
! APPLY SHIFTS TO BACKGROUND PARAMETERS
    3 IF (IBACK.GT.0) THEN
        IF (MULFAS .AND. JPHASE.GT.1) RETURN
        CALL ADJUST(BACKGD(ISPC,JSOURC))
      ENDIF
      GOTO 100
! WRITE NEW L BACK CARD (APPLICABLE ONLY FOR IBACK .GT. 0)
    4 DO I = 1, NBK
        IF (LBKD(I).GT.0) GOTO 42
      ENDDO
!O      WRITE (LPT,3001) NBK, LBKD
!O 3001 FORMAT (/' PROGRAM ERROR *** Too many background cards being written out - NBK =',I4/' LBKD holds ',10I3)
!O      STOP
      CALL DebugErrorMessage('Too many background cards being written out.')
      IBMBER = IBMBER + 1
      RETURN
   42 N1 = LBKD(I)
      N2 = LBKD(I+1) - 1
      LBKD(I) = -LBKD(I)
      IF (I.EQ.1) WRITE (NEWIN,2004) IBACK, (BACKGD(J,JSOURC),J=N1,N2)
 2004 FORMAT ('L BACK',1X,I4,4X,6(F10.5,1X))
      IF (I.NE.1) WRITE (NEWIN,2005) (BACKGD(J,JSOURC),J=N1,N2)
 2005 FORMAT ('L BACK',1X,7(F10.5,1X))
      RETURN
      ENTRY BACKP8(NP,NV)
! RECORD THAT PARAMETER NP IS VARIABLE NUMBER NV:
      KBCKGD(NP,JSOURC) = NV
      RETURN
      ENTRY BACKP9
! RECORD ALL PARAMETERS FIXED:
      DO J = 1, NSOURC
        DO I = 1, NBACK(J)
          KBCKGD(I,J) = 0
        ENDDO
      ENDDO
  100 RETURN

      END SUBROUTINE BACKPR
!
!*****************************************************************************
!
      SUBROUTINE CALPR(PCXX,PFXX)

      USE REFVAR

!
!  *** CALPR updated by JCM 17 Apr 89 ***
!
!H Gives calculated function to match observed for all PR applications
!A PCXX is the name of the routine giving peak centre
!A PFXX is the name of the routine giving peak function
!P on entry in /PRPKFN/ expects ARGI, the argument for the I'th observation
!
!D Forms GCALC, then summed to make YPEAK
!D Adds derivatives of GCALC into previously cleared DERIVV, and via RFACPR
!D gives GGCALC and contributions to SOMEGA.
!
!D GCALC is made from several functions multiplied together.
!D We write GCALC = P1*P2*P3* . . . where:
!D    P1 is an outside multiplying factor containing scale, Lp, multiplicity,
!D       overal tf, etc - it does not contain any structure parameters
!D    P2 is a function of the structure factor, usually Fc squared.
!D    P3 is an extinction correction
!D    P4 is an absorption correction
!D    P5 is the peak function
!D
!D YPEAK is then the sum over contributing k values of GCALC.
!
!D Should work for all refinement types (RIET, CAILS, SAPS . .)
!D                 all data sources (TOF, CN, LX . . )
!
      EXTERNAL PCXX, PFXX
      LOGICAL TESTOV
      COMPLEX FCALC

      INCLUDE 'PARAMS.INC'

      DIMENSION DERIVA(500), CDERS(6), DERIV4(5)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /DERVAR/ DERIVV(500), LVARV
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /FCAL  / FC, FCMOD, COSAL, SINAL, FCDERS(300), DERIVT(300)
      COMPLEX FC, DERIVT
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OVER  / ITFAC, OTFAC(10), KOTFAC(10), NTFAC, JTFAC, KOM15
      EQUIVALENCE (TFAC,OTFAC(1))
      EQUIVALENCE (KTFAC,KOTFAC(1))

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)

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
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      COMMON /PRABSC/ NABTYP(5), ABSPR(2,5), KABSPR(2,5), ABSCOR,       &
     &                DERABQ(2), NABSPR(5)
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PREORI/ NPRTYP, PRFDIR(3), PRFLEN, PRFPAR, KPRFPR, PRFCOR, DERPRQ
      COMMON /PREXTN/ NEXTYP, EXTPR, KEXTPR, EXTCO, DEREXQ, DXDFQ
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

      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)
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

      COMMON /TTHMNC/ TTHMON(5), KTHMON(5), C2TMON(5), S4TMON(5),       &
     &                OPCMON(5), ALPCOR, DLPCOR

      INTEGER         KIPT,       KNIPT
      REAL                                       ZNORM,         DZNDKQ
      REAL            DZNDVQ
      INTEGER                           JOCCR
      COMMON /CMN299/ KIPT(MOBS), KNIPT(MAXPIK), ZNORM(MAXPIK), DZNDKQ(MAXPIK), &
                      DZNDVQ(9,MAXPIK), JOCCR(MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      COMMON /CMNNOW/ NOBSNOW

      INTEGER         IPK
      REAL                 PIK
      COMMON /IPKCMN/ IPK, PIK(MIPK)

! IF X-RAY, DEAL FIRST WITH LP FOR THIS ARGI:
      IF (LX) CALL TTHMLX(2)
! CYCLE OVER K:
      K1 = KIPT(NOBSNOW) + 1
      K2 = KIPT(NOBSNOW+1)
      KMIN = KNIPT(K1)
      DO KK = K1, K2
        KNOW = KNIPT(KK)
        KMOD = KNOW
        ARGK = ZARGK(KNOW)
! FORM D* SQUARED,S SQUARED, AND DERIVATIVES WRT RECIPROCAL CELL
! PARAMETERS:
        CALL CELDER(rHKL(1,KNOW),CDERS)
        DSTAR(KNOW) = SQRT(DSTAR2)
        CALL PCXX(2)
        L2 = NVARF(2,JPHASE,1)
        L123 = NVARF(1,JPHASE,JSOURC) + L2 + NVARF(3,JPHASE,JSOURC) *NSOURC
        L1ST = LVFST1(1,JPHASE,1)
! FORM P1 AND SOME FAMILY 1 QUOTIENT DERIVATIVES (ALL /P1):
! EACH DATA TYPE HAS DIFFERENT FACTOR:
        IF (TOF) FAC = 1.0/(DSTAR2*DSTAR2)
        IF (CN) FAC = 1.0/(SIN(RAD*ARGK)*SIN(0.5*RAD*ARGK))
        IF (SR) FAC = 1.0/(SIN(RAD*ARGK)*SIN(0.5*RAD*ARGK))
        IF (LX) FAC = ALPCOR
!	This next line was the original line, but it's been commented
!	out for numerical stability reasons and replaced by
!	the line above
!        IF (LX) FAC=ALPCOR*V(2)*V(2) ! V(2) = Volume of reciprocal unit cell.
        P1 = SCALEP(JPHASE)*SCALES(JSOURC)*EXP(-2.0*SSQRD*TFAC)*AMUL(KNOW)*FAC
! TFAC DERIVATIVE:
        L = KTFAC
        IF (L.NE.0) DERIVA(L) = -2.0*SSQRD
! SCALE DERIVATIVES:
        L = KSCALS(JSOURC)
        IF (L .NE. 0) DERIVA(L) = 1.0/SCALES(JSOURC)
        L = KSCALP(JPHASE)
        IF (L .NE. 0) DERIVA(L) = 1.0/SCALEP(JPHASE)
! PREFERRED ORIENTATION:
        IF (NPRTYP .GT. 0) THEN
          L = KPRFPR
          CALL PREFOR(2)
          P1 = P1 * PRFCOR
          IF (L .NE. 0) DERIVA(L) = DERPRQ
        ENDIF
! IF X-RAY, MONOCHROMATOR ANGLE:
        IF (LX) THEN
          L = KTHMON(JSOURC)
          IF (L .NE. 0) DERIVA(L) = DLPCOR
        ENDIF
! NEXT DO P2, THE PART DEPENDENT ON THE STRUCTURE FACTOR:
! IF CAIL, P2 IS SIMPLY F4PAR(1,KNOW):
        IF (CAIL .OR. APES) THEN
          IF (KF4PAR(1,KNOW) .EQ. 0) THEN
            F4PAR(1,KNOW) = 1.0
          ELSE
            DERIV4(1) = 1.0
          ENDIF
          IF (F4PAR(1,KNOW) .NE. 0.0) DERIV4(1) = 1.0/F4PAR(1,KNOW)
          P2 = F4PAR(1,KNOW)
        ELSE
! SET BASE FOR DERIVATIVES:
          III = LVFST1(2,JPHASE,1)
! NEW K:
! IF NO FAMILY 2 VARIABLES, NO NEED FOR LFCALC:
          IF (L2 .GT. 0) THEN
            CALL LFCALC(rHKL(1,KNOW))
            F = 0.0
            IF (.NOT. TESTOV(2.0,FCMOD)) F = 2.0/FCMOD
            P2 = FCMOD*FCMOD
! CONVERT DERIVATIVES FOR FAMILY 2 FROM BEING 'OF FCMOD' TO 'OF P2'
! DP2/DV = DMODFC/DV * DP2/DMODFC - AND ALL ARE DIVIDED BY P2
! GIVING D(GCALC)/DV OVER GCALC=(DP2/DV OVER P2)
            DO I = 1, L2
              DERIVA(III+I) = F*FCDERS(I)
            ENDDO
          ELSE
            FC = FCALC(rHKL(1,KNOW))
            P2 = FC*CONJG(FC)
            FCMOD = SQRT(P2)
          ENDIF
        ENDIF
! P3:
        CALL EXCRPR(2)
        P3 = EXTCO
        DP2DFQ = DP2DFQ + DXDFQ
        L = KEXTPR
        IF (L.GT.0) DERIVA(L) = DEREXQ
! P4:
        CALL ABCRPR(2)
        P4 = ABSCOR
        DO IA = 1, 2
          L = KABSPR(IA,JSOURC)
          IF (L.GT.0) DERIVA(L) = DERABQ(IA)
        ENDDO
! P5 - PEAK FUNCTION:
        YNORM = ZNORM(KK)
        DYNDKQ = DZNDKQ(KK)
        DO I = 1, NPKGEN(JPHASE,JSOURC)
          DYNDVQ(I) = DZNDVQ(I,KK)
        ENDDO
        P5 = YNORM
! ADJUST DERIVATIVES WRT ZERO POINT:
        DO I = 1, NZERSP(JPHASE,JSOURC)
          L = KZROSP(I,JPHASE,JSOURC)
          IF (L.GT.0) DERIVA(L) = DYNDKQ*DKDZER(I)
        ENDDO
! ADJUST DERIVATIVES WRT PEAK CENTRE PARAMETERS, IF ANY:
        DO I = 1, NPKCSP(JPHASE,JSOURC)
          L = KPCNSP(I,JPHASE,JSOURC)
          IF (L.GT.0) DERIVA(L) = DYNDKQ*DTDPCN(I)
        ENDDO
! TAKE DERIVATIVES AS FORMED BY PFXX AND PUT INTO DERIVA
! ^^^^^ WE COULD CONSIDER ADDING CONTRIBUTIONS IN SIGS AND GAMS RATHER THAN
! ^^^^^ JUST SWITCHING BETWEEN A FUNCTIONAL VARIATION AND A SPECIAL REFINEMENT
! ^^^^^ I.E. S^2 = S1^2 + S2^2
        DO I = 1, NPKGEN(JPHASE,JSOURC)
          DDDTEM = DYNDVQ(I)
! IF SAPS OR APES, AND REFINING THIS SIGS/GAMS:
          IF (RIET .OR. CAIL) GOTO 29
! ^^^^^ INSTEAD OF 4 IN THE FOLLOWING LINE WE MAY WISH, AT A LATER DATE,
! ^^^^^ TO CONNECT THE GENERA OF FAMILIES 2 & 4 ASSOCIATED WITH THE
! ^^^^^ SAME VARIABLE. I.E. INSTEAD OF 4 WE HAVE I4PD(I)
!      IF (I .NE. 4) GO TO 29
! ^^^^^ THE FOLLOWING IS A BIT OBSCURE AND QUIRKY
          IF (TOF) THEN
            IG = I - 2
          ELSE
            IG = I + 1
          ENDIF
          IF (IG.NE.2 .AND. IG.NE.3) GOTO 29
          IF (KF4PAR(IG,KNOW).GT.0) THEN
!*** CHECK THIS - SHOULD BE DERIVATIVE OF P5 WRT SIGS
! ^^^^^ NEXT LINE MAY HAVE DPDDPD(I,2) MULTIPLIED IN AT A LATER DATE.
            DERIV4(IG) = DDDTEM
            GOTO 12
          ENDIF
   29     DO J = 1, NPKFSP(I,JPHASE,JSOURC)
! ^^^^^ NEXT LINE MAY HAVE DPDDPD(I,1) MULTIPLIED IN AT A LATER DATE.
            L = KPFNSP(I,J,JPHASE,JSOURC)
            IF (L.GT.0) DERIVA(L) = DDDTEM*DERPFN(I,J)
          ENDDO
   12   ENDDO
! NOW CELL PARAMETERS, A* - F*, WHOSE DERIVATIVES SO FAR ARE OF D*SQUARED:
        IF (CN .OR. LX .OR. SR) FAC = DKDDS
        IF (TOF) FAC = DTDWL*(-0.5*WLGTH)/DSTAR2
        DO I = 1, 6
! CHAIN RULE: YNORM - ARGK - D*SQRD - CELL PARAM.
          L = KCPARS(I)
          IF (L.GT.0) DERIVA(L) = DYNDKQ*CDERS(I)*FAC
        ENDDO
        GCALC = P1*P2*P3*P4*P5
        PIK(KNOW) = P1*P3*P4*P5
        YPEAK = YPEAK + GCALC
! PUT APPROXIMATION TO I(CALC) INTO COMM0N:
        AICALC(KNOW) = P1*P2
! SET GGCALC(K FOR THIS I), AND ADD P5 IN TO SOMEGA(K):
        CALL RFACPR(6,PCXX)
! FAMILIES 1,2,3:
! THIS ONLY WORKS IF WE DO NOT HAVE 2 CAIL PHASES:
        DO I = 1, L123
          DERIVV(L1ST+I) = DERIVV(L1ST+I) + DERIVA(L1ST+I)*GCALC
        ENDDO
! FAMILY 4:
        IF (.NOT.RIET) THEN
! WE NOW PICK UP WHICHEVER FAMILY 4 GENERA ARE BEING REFINED
!* THIS WON'T BE RIGHT YET BUT COULD BE WORKED ON:
!* (AND ONCE RIGHT, IT COULD BE SET ONCE ONLY RATHER THAN EVERY TIME):
          L4ST = 1
          L4FIN = 1
          IF (.NOT.CAIL) L4FIN = 4
          DO I = L4ST, L4FIN
            L4 = KF4PAR(I,KNOW)
            IF (L4.NE.0) DERIVV(L4) = DERIVV(L4) + DERIV4(I)*GCALC
          ENDDO
        ENDIF
! FAMILY 6:
! SCALE DERIVATIVES:
        L = KSCALS(JSOURC)
        IF (L.GT.0) DERIVV(L) = DERIVV(L) + DERIVA(L)*GCALC
        DO IA = 1, 2
          L = KABSPR(IA,JSOURC)
          IF (L.GT.0) DERIVV(L) = DERIVV(L) + DERIVA(L)*GCALC
        ENDDO
      ENDDO

      END SUBROUTINE CALPR
!
!*****************************************************************************
!
      SUBROUTINE CONTRI(PCXX,PFXX,IUNI)

      USE REFVAR

!
! *** CONTRI BY JCM 26 Sep 90 ***
!
!H Determines which (if any) reflections contribute to a given data point
!H And does other processing, writing to temporary file
!
      EXTERNAL PCXX, PFXX

      INCLUDE 'PARAMS.INC'

      LOGICAL CONT, EXCLD
      DIMENSION TEMP(6)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /EXCREG/ NEXCL(5), EXCLUD(40,5)
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))

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

      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)
      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

! NON-ZERO CODE FOR EXCLUDED REGIONS:
      ICODE = 0
      IF (NEXCL(JSOURC).NE.0) THEN
        IF (EXCLD(ARGI,EXCLUD(1,JSOURC),NEXCL(JSOURC))) ICODE = 1
      ENDIF
! DISCOVER LOWER AND UPPER LIMITS OF REFLECTION NUMBERS CONTRIBUTING TO THIS:
      CONT = .FALSE.
      IF (ICODE.EQ.0) THEN
! THE 10 IS ARBITRARY:
        KM = KMIN - 10
        IF (KM.LT.1) KM = 1
        DO KNOW = KM, MAXKK(JPHASE)
          CALL CELDER(rHKL(1,KNOW),TEMP)
          DSTAR(KNOW) = SQRT(DSTAR2)
          CALL PCXX(5)
          CALL PFXX(5)
          IF (.NOT.REFUSE) THEN
            CONT = .TRUE.
! SET 'WE HAVE AT LEAST ONE CONTRIBUTING REFLECTION':
          ELSE
            IF (ARGK.GT.ARGI) GOTO 2
            KMIN = KNOW + 1
          ENDIF
        ENDDO
! KMAX=0 IF NONE CONTRIBUTING, OTHERWISE = NUMBER OF LAST CONTRIBUTING:
    2   KMAX = KNOW - 1
      ENDIF
      IF (.NOT.CONT) KMAX = 0
! GET WEIGHT INTO WT:
      IF (JPHASE.EQ.1) CALL WGHTLS(2,ARGI)
! COLLECT LARGEST:
!* NOT YET USED & NOT YET IN COMMON
      YMAX = AMAX1(YMAX,OBS)
! WRITE OUT TO SCRATCH DATASET, UNFORMATTED, FOR REINPUT EVERY CYCLE:
      IF (JPHASE.EQ.1) NOBS = NOBS + 1
      KMI(JPHASE) = KMIN
      KMA(JPHASE) = KMAX
      WRITE (IUNI) ARGI, OBS, DOBS, WT, ICODE, (KMI(J),KMA(J),J=1,NPHASE)
      ZARGI(NOBS) = ARGI
      ZOBS(NOBS) = OBS
      ZDOBS(NOBS) = DOBS
      ZWT(NOBS) = WT
      ICODEZ(NOBS) = ICODE
      NPTS = NOBS

      END SUBROUTINE CONTRI
!
!*****************************************************************************
!
      LOGICAL FUNCTION DFLTPR(IFAM,IGEN,ISPC)
!
! *** DFLTPR updated by JCM 17 Apr 89 ***
!
!H Gives default setting of fix/vary for parameters of PR LSQ (CN, LX, TOF . .)
!A IFAM,IGEN,ISPC on entry specify parameter
!A DFLTPR on exit is TRUE if the parameter is by default varied, FALSE fixed
!D Called as substitute for DEFALT out of VARMAK
!
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19

      DFLTPR = .TRUE.
! VARY ANYTHING GIVEN IN FAMILIES 1, 3  AND 4:
      GOTO (100,2,100,100,100,100), IFAM
      GOTO 100
! IN FAMILY 2 MOSTLY VARY, BUT FIX SITE:
    2 IF (ISPC.LT.10) GOTO 100
      IF (ISPC.EQ.11) GOTO 101
! IF VARYING ITF BY DEFAULT, CHECK THERE IS NOT ALSO ATF:
      IF (ISPC.EQ.12) THEN
        IF (IAPT(IGEN).EQ.0) GOTO 100
        GOTO 101
      ENDIF
      IF (ISPC.GT.12) THEN
        IF (MAGAT(IGEN).NE.0) GOTO 100
      ENDIF
! FIX:
  101 DFLTPR = .FALSE.
  100 RETURN

      END FUNCTION DFLTPR
!
!*****************************************************************************
!
      SUBROUTINE EIGEN(ALSQ,MATSZ)
!
! *** EIGEN redefined by JCM 14 Jan 93 ***
!
!X
!C 19B
!H Writes eigenvalues and eigenvectors of normal matrix for CAIL
!H intensities to a file, and/or the matrix itself.
!
!A ALSQ and MATSZ specify the LSQ matrix in the usual way.
!P For a CAIL refinement, NVARF must hold information about family 4 parameters
!P On an I card read by IICD2, the integer after PREE signals:
!P      containing 1= print eigenvalues, send both values & vectors
!P                    to binary file .EIG
!P      containing 2= as 1, but print both values & vectors.
!P      containing 100= print matrix to binary file .IHM (for Intesity Hessian
!P                    matrix.
!O Writes eigenvalues of the matrix on the printer output
!O Writes eigenvalues and eigenvectors, unformatted, to a file whose name
!O is requested intetractively, and whose default extension is .EIG
!O Writes the original matrix to a file whose name is requested interactively,
!O and whose default extension is .IHM.
!
      DIMENSION ALSQ(MATSZ)
      DIMENSION A(400,400), D(400), E(400)

      INCLUDE 'PARAMS.INC'

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

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
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

! OUT IF LIST NOT WANTED:
      IF (SIMUL .OR. .NOT.CAIL .OR. IPRNT(6).EQ.0) GOTO 100
      I1 = IPRNT(6)/100
      I2 = IPRNT(6) - I1*100
! SET UP MATRIX FROM INVERSE LSQ MATRIX:
! NUMBER OF BASIC VARIABLES IN FAMILY 4:
!** ??? SUSPECT NEED FOR CYCLE OVER PHASES:
      N = NBARF(4,JPHASE,1)
      L = LBFST1(4,JPHASE,1)
      DO I = 1, N
        DO J = I, N
          A(I,J) = ELEMAT(ALSQ,MATSZ,I+L,J+L)
          A(J,I) = A(I,J)
        ENDDO
      ENDDO
! PRINT MATRIX IF REQUIRED:
      IF (I1.NE.0) THEN
        MESSAG = 'intensity Hessian matrix'
        NAMFIL = '.IHM'
        IHM = NOPFIL(1112)
        DO I = 1, N
          WRITE (IHM) (A(L,I),L=1,N)
        ENDDO
      ENDIF
      IF (I2.NE.0) THEN
        MESSAG = 'eigenvalues & vectors'
        NAMFIL = '.EIG'
        MAT = NOPFIL(1112)
! HOUSEHOLDER:
        CALL TRED2(A,N,400,D,E,.TRUE.)
! QL:
        CALL TQLI(D,E,N,400,A,.TRUE.)
! PRINT EIGENVALUES:
        CALL MESS(LPT,2,'Eigenvalues of normal LSQ matrix relating to INTS:')
        DO I = 1, N
          IF (I2.EQ.1) WRITE (LPT,2001) D(I)
 2001     FORMAT (1X,G12.3)
          IF (I2.EQ.2) WRITE (LPT,2002) D(I), (A(L,I),L=1,N)
 2002     FORMAT (/1X,G12.3/(1X,7G11.3))
! AND UNFORMATTED TO UNIT MAT:
          WRITE (MAT) D(I), (A(L,I),L=1,N)
        ENDDO
      ENDIF
  100 RETURN

      END SUBROUTINE EIGEN
!
!*****************************************************************************
!
      SUBROUTINE EXCLIN
!
! *** EXCLIN by JCM 18 Jun 85 ***
!
! INTERPRETS ONE L EXCL CARD
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /EXCREG/ NEXCL(5), EXCLUD(40,5)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      IPT = 7
      N = NEXCL(KSOURC) + 1
! READ PAIRS OF NUMBERS TILL END OF CARD:
    1 CALL RDREAL(EXCLUD(NEXCL(KSOURC)+1,KSOURC),IPT,IPT,80,IER)
      IF (IER.EQ.100) THEN
        WRITE (LPT,2000) (EXCLUD(I,KSOURC),I=N,NEXCL(KSOURC))
 2000   FORMAT (/' Exclude '/(1X,F12.2,' TO',F12.2/))
        RETURN
      ENDIF
! CHECK NOT TOO MANY:
      IER = IERR
      CALL ERRCHK(1,NEXCL(KSOURC),40,1,'excluded regions')
      IF (IER.NE.IERR) RETURN
      CALL RDREAL(EXCLUD(NEXCL(KSOURC)+2,KSOURC),IPT,IPT,80,IER)
      NEXCL(KSOURC) = NEXCL(KSOURC) + 2
      IF (NEXCL(KSOURC).EQ.2) GOTO 1
      IF (EXCLUD(NEXCL(KSOURC)-1,KSOURC).LT.EXCLUD(NEXCL(KSOURC)-2,KSOURC)) THEN
        WRITE (LPT,3000) EXCLUD(NEXCL(KSOURC),KSOURC),EXCLUD(NEXCL(KSOURC)-1,KSOURC)
 3000   FORMAT (/' ERROR ** boundaries for excluded regions not in strictly ascending order'/' Pair',F12.2,' and', F12.2)
        RETURN
      ENDIF
      IF (EXCLUD(NEXCL(KSOURC),KSOURC).GT.EXCLUD(NEXCL(KSOURC)-1,KSOURC)) GOTO 1
      IERR = IERR + 1

      END SUBROUTINE EXCLIN
!
!*****************************************************************************
!
      SUBROUTINE EXCRPR(N)
!
! *** EXCRPR by JCM 9 May 88 ***
!
!H Multiple entry routine to deal with all aspects of extinction corrections
!H  for profile refinement
!
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /FCAL  / FC, FCMOD, COSAL, SINAL, FCDERS(300), DERIVT(300)
      COMPLEX FC, DERIVT
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PREXTN/ NEXTYP, EXTPR, KEXTPR, EXTCO, DEREXQ, DXDFQ
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      GOTO (1,2,3,4,5,6), N
! GIVEN AN 'L EXTN' CARD IN COMM0N /SCRACH/, READ REST OF IT:
    1 CALL RDINTG(NEXTYP,7,IPT,80,IER)
! DO NOT LOOK FURTHER AT NEXTYP YET AS 1 IS ASSUMED
      IF (TOF) THEN
        WRITE (LPT,2000) NEXTYP
 2000   FORMAT (/' Type ',I2,' extinction correction -- simple Bragg',  &
     &          ' model'/'      (ref. Sabine, Aust. J. Phys. 1985,38,507.) ')
      ENDIF
      CALL RDREAL(EXTPR,IPT,IPT,80,IER)
      WRITE (LPT,2005) EXTPR
 2005 FORMAT (/' Extinction correction parameter =',F10.4,' microns ')
      IF (EXTPR.EQ.0.) GOTO 10
      GOTO 100
! FORM EXTINCTION CORRECTION WHICH WILL BE P3 IN CALxx, AND DERIVATIVE:
    2 IF (EXTPR.EQ.0.) GOTO 10
! ONLY FOR TOF AT PRESENT:
      IF (.NOT.TOF) GOTO 10
      UCOF = 0.75*WLGTH*V(2)
      UTEM = UCOF*FCMOD
      UALP = UCOF*EXTPR
      UEXT = UTEM*EXTPR
      EXTCO = 1./(1.+UEXT*UEXT)
      DEREXQ = -EXTCO*UTEM*UEXT
      DXDFQ = -EXTCO*UALP*UEXT
      EXTCO = SQRT(EXTCO)
      GOTO 100
! APPLY SHIFT IN COEFFICIENT:
    3 CALL ADJUST(EXTPR)
      GOTO 100
! WRITE OUT NEW 'L EXTN' CARD FOR TOF:
    4 WRITE (NEWIN,2001) NEXTYP, EXTPR
 2001 FORMAT ('L EXTN',2X,I2,1X,F10.4)
      GOTO 100
! DEAL WITH ABSENCE OF 'L EXTN' CARD:
    5 CALL MESS(LPT,1,'No L EXTN card - assuming no extinction correction')
      NEXTYP = 0
      EXTPR = 0.
   10 EXTCO = 1.0
      DEREXQ = 0.
      DXDFQ = 0.
      GOTO 100
! FIX EXT COR IF NO CARD GIVEN:
    6 IF (NEXTYP.EQ.0) CALL ADDFX5(1,1,8,1,1,4)
      GOTO 100
      ENTRY EXCPR8(NV)
! RECORD THAT THE EXTN CORRECTION PARAMETER IS VARIABLE NUMBER NV:
      KEXTPR = NV
      GOTO 100
      ENTRY EXCPR9
! RECORD THAT THE EXTN CORRECTION PARAMETER IS FIXED:
      KEXTPR = 0
  100 RETURN

      END SUBROUTINE EXCRPR
!
!*****************************************************************************
!
      SUBROUTINE FAM4PR(N,PCXX,PFXX)

      USE REFVAR

!
! *** FAM4PR updated by PJB 1 Feb 1994 ***
!
!C 19B
!H Multiple entry subroutine for CAIL, SAPS operations
!
      INCLUDE 'PARAMS.INC'

      EXTERNAL PCXX, PFXX
      DIMENSION KK1(2), AM(2), BM(2), IH(3), SUMPKN(5)
      LOGICAL TOOWEE
      REAL ARTEM(6)
      DIMENSION ISTRIK(ITMREF), ISLAK(ITMREF), ARCLUM(ITMREF),          &
     &          ICLUMP(ITMREF), CLUMUL(ITMREF), TOOWEE(ITMREF)
!* CAN WE USE /SCRAT/ FOR THAT LOT ?? **
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
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

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
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
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      CHARACTER*10 CONTYP(5)
      DATA CONTYP/' ', ' STRICT * ', 'SLACK TO *', ' FIXED *', ' STRICT - '/

      GOTO (10,20,30,100,50), N
! ENTRY FROM INPLPR TO READ LIMITS FROM L SLIM CARD:
   10 CALL RDREAL(STRTOL,7,IPT,80,IER)
      CALL RDREAL(SLKTOL,IPT,IPT,80,IER)
      IF (SAPS .OR. APES) THEN
        CALL RDREAL(WEELEV,IPT,IPT,80,IER)
        WRITE (LPT,2029) WEELEV
 2029   FORMAT (/' Intensity/ESD slack constraint criterion = ',F10.3)
      ENDIF
      WRITE (LPT,2019) STRTOL, SLKTOL
 2019 FORMAT (/' Limit for strict constraints=',F10.3/'        for slack constraints=',F10.3)
      GOTO 100
! ENTRY FROM MAIN PROGRAMS TO DETERMINE CONSTRAINTS EACH CYCLE:
   20 IF (PRECYC) GOTO 100
      CALL JGMZER(ISTRIK,1,MAXKK(JPHASE))
      CALL JGMZER(ISLAK,1,MAXKK(JPHASE))
      NSPCPS(4,JPHASE) = MAXKK(JPHASE)
! REMOVE ALL CONSTRAINTS ON FAMILY 4 PARS - THIS WILL TAKE OUT ANY
! THE USER GAVE ON CARDS ALSO:
! ^^^^^ HOWEVER, WE MUST BE ABLE TO ALLOW USER TO FIX AND RELATE PARAMETERS
! ^^^^^ AND KEEP THEM SO.
      KK1(1) = KPAK(4,0,0,0,0)
      CALL SUBCON(2,KK1,AM,4)
      NSLAK(3) = 0
      ITST = 0
      AM(1) = 1.
      AM(2) = -1.
! SCAN FIRST FOR STRICT:
      KNOW = 1
! ^^^^^ NOTE CHANGE OF LABEL 2
      CALL CELDER(rHKL(1,KNOW),ARTEM)
      DSTAR(KNOW) = SQRT(DSTAR2)
      CALL PCXX(5)
    2 AKLO = ARGK
      KNOW = KNOW + 1
      IF (KNOW.GT.MAXKK(JPHASE)) GOTO 1
      CALL CELDER(rHKL(1,KNOW),ARTEM)
      DSTAR(KNOW) = SQRT(DSTAR2)
      CALL PCXX(5)
      AKHI = ARGK
! PFXX(6) GIVES SEPARATION OF ADJACENT REFLECTION POSITIONS,
! AND FLAGS AS STRICT OR SLACK ACCORDING TO STRTOL OR SLKTOL
      CALL PFXX(6)
      IF (STRKT) THEN
        ITST = ITST + 1
        ISTRIK(KNOW-1) = 1
! RELATE PARAMETER KNOW-1 AND PARAMETER KNOW:
        IGST = 1
        IF (SAPS) IGST = 2
        DO IG = IGST, NGENPS(4,JPHASE)
          KK1(1) = KPAK(4,IG,KNOW-1,JPHASE,1)
          KK1(2) = KPAK(4,IG,KNOW,JPHASE,1)
          CALL ADDCON(2,KK1,AM,4)
        ENDDO
      ENDIF
      GOTO 2
! SCAN AGAIN FOR SLACK:
    1 K = 1
      AM(2) = 1.
! K IS THE FIRST OF A POTENTIAL CLUMP:
      NCLUMP = 0
    3 KNOW = K
      ISLAK(K) = 0
      CALL PCXX(5)
      SUMMUL = AMUL(K)
      SUMMAR = SUMMUL*ARGK
! SUM INTENSITIES BUT ALSO ESDs.
      SUMMIN = SUMMUL*F4PAR(1,K)
      SUMESD = SUMMUL*F4PESD(1,K)
      IF (SAPS .OR. APES) THEN
        DO IG = 2, NGENPS(4,JPHASE)
          SUMPKN(IG) = F4PAR(IG,K)*SUMMUL
        ENDDO
      ENDIF
! K IS THE BASE FROM WHICH WE TEST (POSSIBLY) SEVERAL ADJACENT:
! K1 RECORDS THE "PREVIOUS" NON-FIXED K:
      K1 = K
      DO L = K + 1, MAXKK(JPHASE)
! IF ALREADY FIXED, IGNORE:
        IF (ISTRIK(L).EQ.2) GOTO 5
! COLLECT A CLUMP OF ALL REFLECTIONS STRICTLY CONSTRAINED TO THE BASE K:
        IF (ISTRIK(K1).NE.1) GOTO 11
! CURRENT REFLECTION IS STRICTLY CONSTRAINED TO PREVIOUS (NON-FIXED) ONE:
        SUMMUL = SUMMUL + AMUL(L)
        KNOW = L
        CALL PCXX(5)
        SUMMAR = SUMMAR + AMUL(L)*ARGK
        DELTIN = AMUL(L)*F4PAR(1,L)
        SUMMIN = SUMMIN + DELTIN
        SUMESD = SUMESD + AMUL(L)*F4PESD(1,L)
        IF (SAPS .OR. APES) THEN
          DO IG = 2, NGENPS(4,JPHASE)
            SUMPKN(IG) = SUMPKN(IG) + AMUL(L)*F4PAR(IG,L)
          ENDDO
        ENDIF
        K1 = L
    5 ENDDO
      L = MAXKK(JPHASE) + 1
! END OF CLUMP:
   11 NCLUMP = NCLUMP + 1
      ICLUMP(NCLUMP) = K
      ICLUMP(NCLUMP+1) = L
      ARCLUM(NCLUMP) = SUMMAR/SUMMUL
      CLUMUL(NCLUMP) = SUMMUL
! NOW FORCE ALL INTS IN CLUMP TO BE THE MEAN VALUE:
      FAV = SUMMIN/SUMMUL
      DO I = K, L - 1
        F4PAR(1,I) = FAV
      ENDDO
      IF (SAPS .OR. APES) THEN
! CHECK IF THE INTENSITY OF THIS CLUMP IS TOO SMALL -
! THIS IS ONLY NEEDED FOR SAPS OR APES.
        TOOWEE(NCLUMP) = SUMMIN/SUMESD.LT.WEELEV
        DO I = K, L - 1
          DO IG = 2, NGENPS(4,JPHASE)
            IF (TOOWEE(NCLUMP)) CALL ADDFX5(4,IG,I,JPHASE,1,4)
            F4PAR(IG,I) = SUMPKN(IG)/SUMMUL
          ENDDO
        ENDDO
      ENDIF
! DETECT SLACK CONSTRAINT BETWEEN THIS CLUMP AND ANY PREVIOUS:
      AKHI = ARCLUM(NCLUMP)
      KK1(1) = ICLUMP(NCLUMP)
      BM(1) = CLUMUL(NCLUMP)
      DO I = NCLUMP - 1, 1, -1
        AKLO = ARCLUM(I)
        CALL PFXX(6)
! IF THIS IS NOT SLACK, NONE FURTHER WILL BE:
        IF (SLACK.EQ.0.) GOTO 33
! A SLACK CONSTRAINT:
        KK1(2) = ICLUMP(I)
! TAKE ACTION ON SLACK CONSTRAINTS FOR INTENSITIES IF CAIL OR APES
        IF (CAIL .OR. APES) THEN
          ISLAK(K) = ISLAK(K) + 1
! EVALUATE WEIGHT AND THEN INCLUDE THE EXTRA WEIGHTING:
          WEIGHT = 3./(F4PAR(1,ICLUMP(NCLUMP))+F4PAR(1,ICLUMP(I)))**2
          WEIGHT = WEIGHT*SLACK
          BM(2) = CLUMUL(I)
          CALL ADDPAW(3,1,KK1,BM,WEIGHT)
        ENDIF
! TAKE ACTION ON SLACK CONSTRAINTS FOR PEAK WIDTHS FOR SAPS AND APES ONLY
! IF INTENSITIES OF EITHER CLUMP ARE NOT TOO SMALL.
        IF (SAPS .OR. APES) THEN
          IF (TOOWEE(I) .OR. TOOWEE(NCLUMP)) GOTO 13
          DO IG = 2, NGENPS(4,JPHASE)
            ISLAK(K) = ISLAK(K) + 1
! ^^^^^ THE FOLLOWING WEIGHTING IS RATHER RANDOM AT PRESENT
            WEIGHT = 400./(F4PAR(IG,ICLUMP(NCLUMP))+F4PAR(IG,ICLUMP(I)))**2
            WEIGHT = WEIGHT*SLACK
            CALL ADDPAW(3,IG,KK1,AM,WEIGHT)
          ENDDO
        ENDIF
   13 ENDDO
! NO MORE SLACK CONSTRAINTS START FROM CURRENT CLUMP:
   33 K = L
      IF (K.LE.MAXKK(JPHASE)) GOTO 3
! FINALLY A PRINTING CYCLE:
! ^^^^^ ALL THIS PRINTING NEEDS TIDYING UP
      WRITE (LPT,2000) ICYC
 2000 FORMAT (//' Processing reflections on cycle ',I4)
      IF (FIXED .AND. CAIL) CALL MESS(LPT,1,                            &
     &            ' No.  h    k    l  INTENSITY     Posn         Flags ')
      IF (FIXED .AND. .NOT.CAIL) CALL MESS(LPT,1,' No.  h    k    l  INTENSITY     Posn    SIGS     Flags ')
      IF (.NOT.FIXED .AND. CAIL) CALL MESS(LPT,1,' No.   h       k       l     INTENSITY     Posn         Flags ')
      IF (.NOT.FIXED .AND. .NOT.CAIL) CALL MESS(LPT,1,' No.   h       k       l     INTENSITY     Posn    SIGS'//'     Flags ')
! ^^^^^ WE CAN WRITE THE ABOVE LINE WITH SIGS / GAMS ETC.
! ^^^^^ USING DYNAMIC FORMATTING - SEE SUBROUTINE HKLOUT.
!
! ^^^^^ THE FOLLOWING DO LOOP NEEDS COMPLETELY REVAMPED
      DO KNOW = 1, MAXKK(JPHASE)
        NTYP1 = 1
        NTYP2 = 1
        CALL PCXX(5)
        IF (FIXED) CALL INDFIX(rHKL(1,KNOW),IH)
        IF (ISTRIK(KNOW).EQ.1) NTYP1 = 2
        IF (KNOW.GT.1 .AND. ISTRIK(KNOW-1).EQ.1 .AND. ISTRIK(KNOW).NE.1) NTYP1 = 5
! ^^^^^ BE CAREFUL OF THE NEXT LINE
        IF (ISTRIK(KNOW).EQ.2) NTYP1 = 4
        IF (ISLAK(KNOW).NE.0) NTYP2 = 3
        IF (CAIL) THEN
          IF (NTYP2.EQ.3) THEN
            IF (FIXED) THEN
              WRITE (LPT,2001) KNOW, IH, F4PAR(1,KNOW), ARGK, CONTYP(NTYP1), CONTYP(NTYP2), ISLAK(KNOW)
            ELSE
              WRITE (LPT,2003) KNOW, (rHKL(I,KNOW),I=1,3), F4PAR(1,KNOW)&
     &                         , ARGK, CONTYP(NTYP1), CONTYP(NTYP2), ISLAK(KNOW)
            ENDIF
          ELSE
            IF (FIXED) THEN
              WRITE (LPT,2001) KNOW, IH, F4PAR(1,KNOW), ARGK, CONTYP(NTYP1)
            ELSE
              WRITE (LPT,2003) KNOW, (rHKL(I,KNOW),I=1,3), F4PAR(1,KNOW), ARGK, CONTYP(NTYP1)
            ENDIF
          ENDIF
        ENDIF
        IF (.NOT.CAIL) THEN
          IF (NTYP2.EQ.3) THEN
            IF (NGENPS(4,JPHASE).EQ.2) THEN
              IF (FIXED) THEN
                WRITE (LPT,2002) KNOW, IH, F4PAR(1,KNOW), ARGK,         &
     &                           F4PAR(2,KNOW), CONTYP(NTYP1),          &
     &                           CONTYP(NTYP2), ISLAK(KNOW)
              ELSE
                WRITE (LPT,2004) KNOW, (rHKL(I,KNOW),I=1,3),            &
     &                           F4PAR(1,KNOW), ARGK, F4PAR(2,KNOW),    &
     &                           CONTYP(NTYP1), CONTYP(NTYP2), ISLAK(KNOW)
              ENDIF
            ELSEIF (NGENPS(4,JPHASE).EQ.3) THEN
              IF (FIXED) THEN
                WRITE (LPT,2202) KNOW, IH, F4PAR(1,KNOW), ARGK,         &
     &                           (F4PAR(IG,KNOW),IG=2,3), CONTYP(NTYP1),&
     &                           CONTYP(NTYP2), ISLAK(KNOW)
              ELSE
                WRITE (LPT,2204) KNOW, (rHKL(I,KNOW),I=1,3),            &
     &                           F4PAR(1,KNOW), ARGK,                   &
     &                           (F4PAR(IG,KNOW),IG=2,3), CONTYP(NTYP1),&
     &                           CONTYP(NTYP2), ISLAK(KNOW)
              ENDIF
            ENDIF
          ELSE
            IF (NGENPS(4,JPHASE).EQ.2) THEN
              IF (FIXED) THEN
                WRITE (LPT,2002) KNOW, IH, F4PAR(1,KNOW), ARGK, F4PAR(2,KNOW), CONTYP(NTYP1)
              ELSE
                WRITE (LPT,2004) KNOW, (rHKL(I,KNOW),I=1,3),            &
     &                           F4PAR(1,KNOW), ARGK, F4PAR(2,KNOW),    &
     &                           CONTYP(NTYP1)
              ENDIF
            ELSEIF (NGENPS(4,JPHASE).EQ.3) THEN
              IF (FIXED) THEN
                WRITE (LPT,2202) KNOW, IH, F4PAR(1,KNOW), ARGK, (F4PAR(IG,KNOW),IG=2,3), CONTYP(NTYP1)
              ELSE
                WRITE (LPT,2204) KNOW, (rHKL(I,KNOW),I=1,3),            &
     &                           F4PAR(1,KNOW), ARGK,                   &
     &                           (F4PAR(IG,KNOW),IG=2,3), CONTYP(NTYP1)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      WRITE (LPT,2008) MAXKK(JPHASE), ITST, NSLAK(3), MAXKK(JPHASE) - ITST
 2008 FORMAT (//,' Total of ',I5,' reflections ',/'          ',I5,      &
     &        ' strict constraints ',/'          ',I5,                  &
     &        ' slack constraints ',//'  making  ',I5,                  &
     &        ' possible variable intensities '/)
      GOTO 100
! ENTRY FROM NWINPR TO APPLY SHIFT:
   30 IF (IGEN.EQ.2) THEN
        CALL ADF4G2(F4PAR(IGEN,ISPC))
      ELSE
        CALL ADJUST(F4PAR(IGEN,ISPC))
      ENDIF
      F4PESD(IGEN,ISPC) = ESD
      GOTO 100
! ENTRY FROM SETPR TO SET UP DEFAULTS:
   50 STRTOL = 2.0
      SLKTOL = 1.
      WEELEV = 3.
      WRITE (LPT,2018) STRTOL, SLKTOL, WEELEV
 2018 FORMAT (/' No L SLIM card read -'/' Limit for strict constraints='&
     &        ,F10.3/'        for slack constraints=',                  &
     &        F10.3/'        for intensity/ESDs   =',F10.3)
      GOTO 100
      ENTRY FM4PR8(NG,NS,NV)
! SET PARAMETER TO BE VARIABLE NV:
      KF4PAR(NG,NS) = NV
      GOTO 100
      ENTRY FM4PR9
! CLEAR ALL FAMILY 4 PARAMETERS TO BE FIXED (EVEN IF NONE INVOLVED):
      DO I = 1, 3
        DO J = 1, ITMREF
          KF4PAR(I,J) = 0
        ENDDO
      ENDDO
  100 RETURN
 2001 FORMAT (1X,I3,3I5,F12.3,1X,F12.3,2A10,I4)
 2003 FORMAT (1X,I3,3F8.3,F12.3,1X,F12.3,2A10,I4)
 2002 FORMAT (1X,I3,3I5,F10.3,1X,F12.3,1X,F10.3,2A12,I4)
 2004 FORMAT (1X,I3,3F8.3,F10.3,1X,F12.3,1X,F10.3,2A12,I4)
 2202 FORMAT (1X,I3,3I5,F10.3,1X,F12.3,2(1X,F10.3),2A12,I4)
 2204 FORMAT (1X,I3,3F8.3,F10.3,1X,F12.3,2(1X,F10.3),2A12,I4)

      END SUBROUTINE FAM4PR
!
!*****************************************************************************
!
      SUBROUTINE FRENEL(Z,FRCOS,FRSIN)
!
! *** FRENEL by WIFD 23 Feb 93 ***
!
!X
!C 9C
!H Calculates ?
!A On entry Z holds the argument
!A On exit FRCOS, FRSIN hold ?
!
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      FZ = (1.+Z*0.926)/(2.+Z*(1.792+Z*3.104))
      GZ = 1./(2.+Z*(4.142+Z*(3.492+Z*6.670)))
      ARG = PIBY2*Z*Z
      SINARG = SIN(ARG)
      COSARG = COS(ARG)
      FRCOS = 0.5 + FZ*SINARG - GZ*COSARG
      FRSIN = 0.5 - FZ*COSARG - GZ*SINARG

      END SUBROUTINE FRENEL
!
!*****************************************************************************
!
      SUBROUTINE HKLOUT(PCXX,ALSQ,MATSZ)

      USE REFVAR

! *** HKLOUT updated by PJB 1 Feb 1994 ***
!
!H Writes h,k,l list, possibly plus other info, to unit LKH
!P On entry, reflection indices must be in rHKL in /REFLNS/
!P IREF, various LOGICALS in /REFIPR give type of refinement -
!P in particular RIET, CAIL, SAPS, APES . .
!
      EXTERNAL PCXX

      INCLUDE 'PARAMS.INC'

      DIMENSION ALSQ(MATSZ)
      DIMENSION IH(3), ADIAG(MaxBVar), ICOV(30)
      CHARACTER*80 FMT1, FMT2, FMT3

      PARAMETER (IREFSM=2000)

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON

      COMMON /HCVCMN/ LCV, ICORL(15,IREFSM), ICLUMP(IREFSM)

      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

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

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DATA FMT1/'(3I5  ,F10.3,F10.4,I5,30I4)'/
      DATA FMT2/'(3I5  ,3(F10.3,F10.4))'/
      DATA FMT3/'(3I5  ,F10.3,F10.4,I5,30I5)'/
! OUT IF LIST NOT WANTED:
      IF (SIMUL) GOTO 999
      IF (IABS(MODERR(JSOURC)).NE.2 .AND. RIET) GOTO 999
!O      MESSAG = 'HKL listing'
      NAMFIL = '.HKL'
      CALL OPNFIL(LKH,113)
      IF (CAIL) THEN
!O        MESSAG = 'HCV listing'
        NAMFIL = '.HCV'
        LCV = 72
        CALL OPNFIL(LCV,113)
      ENDIF
!O      MESSAG = 'reflection positions file'
      NAMFIL = '.TIC'
      ITK = 73
      CALL OPNFIL(ITK,113)
! IF CAIL, GET DIAGONAL LSQ MATRIX INVERSE ELEMEMTS:
      IF (CAIL) THEN
        DO I = 1, LVARB
          ADIAG(I) = SQRT(ELEMAT(ALSQ,MATSZ,I,I))
        ENDDO
      ENDIF
! IF LX, ALTER FORMATS:
      IF (LX) THEN
        FMT1(10:10) = '5'
        FMT1(16:16) = '5'
        FMT2(12:12) = '5'
        FMT2(18:18) = '5'
      ENDIF
! FLOATING FORMATS
      IF (.NOT.FIXED) THEN
        FMT1(3:6) = 'F8.3'
        FMT2(3:6) = 'F8.3'
      ENDIF
      DO I = 1, MAXKK(JPHASE)
        IF (FIXED) CALL INDFIX(rHKL(1,I),IH)
        KNOW = I
        CALL PCXX(2)
        IF (ARGK.LT.180.0) THEN
          WRITE (ITK,1700) (IH(II),II=1,3), ARGK, DSTAR(I)
 1700     FORMAT (3I4,F12.5,F10.6)
        ELSE
          WRITE (ITK,1710) (IH(II),II=1,3), ARGK, DSTAR(I)
 1710     FORMAT (3I4,F12.3,F10.6)
        ENDIF
        IF (RIET) THEN
          IF (FIXED) THEN
            WRITE (LKH,FMT2) IH
          ELSE
            WRITE (LKH,FMT2) (rHKL(J,I),J=1,3)
          ENDIF
        ENDIF
! CAIL:
        IF (CAIL) THEN
          IF (IPRNT(5).EQ.0) THEN
            IF (FIXED) THEN
              WRITE (LKH,FMT2) IH, F4PAR(1,I), F4PESD(1,I)
            ELSE
              WRITE (LKH,FMT2) (rHKL(J,I),J=1,3), F4PAR(1,I), F4PESD(1,I)
            ENDIF
          ENDIF
          IF (IPRNT(5).GT.0) THEN
! SET NO COVARIANCES:
            CALL JGMZER(ICOV,1,30)
! WHICH VARIABLE IS THIS PARAMETER?
            K = KF4PAR(1,I)
! IF FIXED, NO COVARIANCES:
            IF (K.EQ.0) GOTO 89
! WHICH BASIC VARIABLE (IE ROW OF LSQ MATRIX) IS THIS VARIABLE?
            I1 = K
   90       K = LVRBS(I1)
! STARTING INTS MAY BE STRICTLY RELATED TO ONE PREVIOUS:
            IF (K.LT.0) THEN
              I1 = I1 - 1
              GOTO 90
            ENDIF
! IF FIRST INTS, RECORD OFFSET FOR PRINTING CLUMP NUMBER:
            IF (I.EQ.1) KBASE = K - 1
! NOW FIND THE NEXT IPRNT(5) BASICS AFTER K:
            L1 = K + IPRNT(5)
            IF (L1.GT.L4END(JPHASE)) L1 = L4END(JPHASE)
            DO L = K + 1, L1
              ICOV(L-K) = NINT(100.*ELEMAT(ALSQ,MATSZ,K,L)/(ADIAG(K)*ADIAG(L)))
            ENDDO
!  89      CIITEM=F4PESD(1,I)
   89       CONTINUE
! hkl file after pawley refinement writes here.  Using FMT3 so Space Group Determination
! program can read correlations
            WRITE (LKH,FMT3) IH, F4PAR(1,I), adiag(K), K - KBASE, (ICOV(L),L=1,IPRNT(5))
            ICLUMP(I) = K - KBASE
            DO L = 1, IPRNT(5)
              ICORL(L,I) = ICOV(L)
            ENDDO
          ENDIF
        ENDIF
!* THIS WON'T DO - NGEN4 IS AT PRESENT IN COMMON WHICH IS SWOPPED - SORT OUT
        IF (SAPS .OR. APES) WRITE (LKH,FMT2) IH,(F4PAR(IG,I),F4PESD(IG,I),IG=1,NGEN4(JPHASE,JSOURC))
      ENDDO
      IF (CAIL) CALL HKL2HCV(IPRNT(5))
      CALL CLOFIL(LKH)
      IF (CAIL) CALL CLOFIL(LCV)
      CALL CLOFIL(ITK)
  999 RETURN

      END SUBROUTINE HKLOUT
!
!*****************************************************************************
!
      SUBROUTINE IICD2
!
! *** IICD2 updated by JCM 13 May 90 ***
!
      CHARACTER*4 INEED(6)
      CHARACTER*2 INEX(2)
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      COMMON /IINFO / IIN, ACOEFF(20)
      COMMON /IINFOW/ IIREAD(20)
      CHARACTER*4 IIREAD
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      DATA INEED/'PRFC', 'PRFO', 'PRPR', 'PRCV', 'PREE', 'ZBAK'/
      DATA INEX/'In', 'Ex'/
!
! FIRST READ STANDARD LSQ OPTIONS NCYC, CYC1, PRIN, MCOR:
      CALL IICD1
! REMAINING OPTIONS PECULIAR TO PR:
! DEFAULTS:
      IPRNT(2) = 0
      IPRNT(3) = 0
      IPRNT(4) = 0
      IPRNT(5) = 0
      IPRNT(6) = 0
      DO I = 1, 6
        DO J = 1, IIN
          IF (INEED(I).EQ.IIREAD(J)) GOTO 3
        ENDDO
        GOTO 1
    3   IF (I.EQ.6) THEN
          ZBAKIN = ACOEFF(J).EQ.1.
        ELSE
          IPRNT(I+1) = NINT(ACOEFF(J))
        ENDIF
    1 ENDDO
      IF (IPRNT(2).NE.0) THEN
        CALL MESS(LPT,1,'Reflection information to be printed ')
        CALL DEPRIN(IPRNT(2))
      ENDIF
      IF (IPRNT(3).NE.0) THEN
        CALL MESS(LPT,1,'Reflections to be output to file for Fourier')
        CALL DEPRIN(IPRNT(3))
      ENDIF
      IF (IPRNT(4).NE.0) THEN
        CALL MESS(LPT,1,'Profile including zeros to be output to file')
        CALL DEPRIN(IPRNT(4))
      ENDIF
      IF (IPRNT(5).NE.0) WRITE (LPT,2003) IPRNT(5)
 2003 FORMAT (' Print',I3,' covariances between I and successive',      &
     &        ' intensities on .HKL file')
      IF (IPRNT(6).NE.0) CALL MESS(LPT,1,                               &
     &       'CAIL intensity eigenvalues and eigenvectors to be sent to .EIG file')
      I = 2
      IF (ZBAKIN) I = 1
      CALL MESS(LPT,1,INEX(I)//'clude points at which Y(peak)=0 while collecting counts')

      END SUBROUTINE IICD2
!
!*****************************************************************************
!
      SUBROUTINE INOBPR(ISCR,NFLOP,PCXX,PFXX)
!
! *** INOBPR updated BY JBF and PJB 8 Mar 1994 ***
!
!H Read PR observation data, and write to scratch dataset
! MODEOB gives type of data: 1 3 FP numbers ARGI OBS DOBS
!                            2 Binary ARGI OBS DOBS ICODE
!                            3 ILL Grenoble CN powder data format
!
      EXTERNAL PCXX, PFXX

      INCLUDE 'PARAMS.INC'

      LOGICAL ENDIP
      DIMENSION INOBS(10), NN(10), ISCR(2)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))

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

      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      
      INCLUDE 'REFLNS.INC'
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

! MAKE SCRATCH FILE ON WHICH TO HAND OVER OBSERVATIONS TO MAIN:
      ISCR(1) = NOPFIL(1005)
! AND IF MULTI-PHASE, ANOTHER ONE FOR ALTERNATE USE:
      IF (MULFAS) ISCR(2) = NOPFIL(1005)
      NFLIP = 1
      NFLOP = 2
      NOBS = 0
! CYCLE OVER SOURCES:
      DO JSOUR = 1, NSOURC
        JSOURC = JSOUR
! SET UP WHETHER TOF, CN ETC:
        CALL LOGSOU(JSOURC)
        IF (SIMUL .AND. .NOT.PRECYC) THEN
! INITIALISE SCRATCH FILE QUANTITIES FOR SIMULATION:
          IF (TOF) THEN
            OPDTOT = 1. + ARGSTP(JSOURC)
            ARGI = ARGMIN(JSOURC)/OPDTOT
          ENDIF
          OBS = 0.
          DOBS = 1.
          ICODE = 0
        ELSE
! OPEN OBSERVATIONS FILE
          MESSAG = 'Observations file'
          IF (JSOURC.GT.1) MESSAG = 'Next obs file'
          M = 11
          IF (MODEOB(JSOURC).EQ.2) M = 1011
          CALL OPNFIL(LUNI,M)
        ENDIF
! COLLECT MAXIMUM OBS:
        YMAX = 0.
! SCAN ALL PHASES:
        DO JP = 1, NPHASE
          JPHASE = JP
          IF (JPHASE.GT.1) THEN
            REWIND (ISCR(NFLOP))
            REWIND (ISCR(NFLIP))
!* DO WE NEED TO DUMP OUT THE EXISTING PHASE?
            CALL PHMOVE(1,JPHASE)
          ENDIF
! START COUNT UP CONTRIBUTING REFLECTIONS - OBSERVATIONS MUST BE IN SEQUENCE:
          KMIN = 1
          KOUNT = 0
!* NB WE HAVE NO WAY OF SIMULATING ANYTHING BUT TF AT PRESENT
          IF (MODEOB(JSOURC).EQ.3) THEN
! GRENOBLE FORMAT FOR CONSTANT WAVELENGTH NEUTRON DIFFRACTOMETERS
! READ 4 LINES OF COMMENT AND IGNORE;  THEN READ IN FORMAT 10(I2,I8)
            READ (LUNI,1010) ICARD
 1010       FORMAT (A80,///)
            CALL MESS(LPT,1,'Data file in Grenoble sum file format:')
            CALL MESS(LPT,1,'Title reads: ')
            CALL MESS(LPT,0,ICARD)
          ELSEIF (MODEOB(JSOURC).EQ.4) THEN
! THIS IS SOMETIMES USED FOR LABORATORY X-RAY DATA TOO
            DO I = 1, 10
              NN(I) = 1
            ENDDO
          ENDIF
! GET NEXT OBSERVATION:
   10     IF (SIMUL .AND. .NOT.PRECYC) THEN
            IF (TOF) THEN
              ARGI = ARGI*OPDTOT
              IF (ARGI.GT.ARGMAX(JSOURC)) GOTO 50
            ELSE
              ARGI = ARGMIN(JSOURC) + FLOAT(KOUNT)*ARGSTP(JSOURC)
              KOUNT = KOUNT + 1
            ENDIF
            GOTO 11
          ENDIF
! PHASES OTHER THAN FIRST:
          IF (JPHASE.GT.1) THEN
! READ FROM THE OTHER TEMPORARY FILE - KMI & KMA ARE PARTIALLY FILLED:
            READ (ISCR(NFLOP),END=50) ARGI, OBS, DOBS, WT, ICODE, (KMI(I),KMA(I),I=1,NPHASE)
            GOTO 11
          ELSE
! BRANCH ON DIFFERENT INPUT FORMATS FOR OBSERVATIONS:
            GOTO (60,1,2,3,4), MODEOB(JSOURC) + 1
          ENDIF
! USER'S OWN ROUTINE SUPPLIED FOR NON-STANDARD INPUT IF REQUIRED:
   60     CALL QPRIN(ARGI,OBS,DOBS,ICODE,ENDIP)
          IF (ENDIP) GOTO 50
          GOTO 11
! TYPE 1: 3 FLOATING POINT NUMBERS:
    1     READ (LUNI,1001,END=50) ICARD
 1001     FORMAT (A80)
          CALL RDREAL(ARGI,1,IPT,80,IER)
          CALL RDREAL(OBS,IPT,IPT,80,IER)
          CALL RDREAL(DOBS,IPT,IPT,80,IER)
          IF (IER.EQ.100) DOBS = SQRT(OBS)
          GOTO 11
! MODE 2 INPUT (FOR ANY DATA SOURCE) - BINARY:
    2     READ (LUNI,END=50) ARGI, OBS, DOBS, ICODE
   11     CALL CONTRI(PCXX,PFXX,ISCR(NFLIP))
          GOTO 10
! MODEOB(JSOURC) = 3 IS GRENOBLE SUM FILE FORMAT (USUALLY CN):
    3     READ (LUNI,1000,END=50) (NN(I),INOBS(I),I=1,10)
 1000     FORMAT (10(I2,I6))
          GOTO 19
! MODEB(JSOURC) = 4 (USUALLY LX):
    4     READ (LUNI,1002,END=50) (INOBS(I),I=1,10)
 1002     FORMAT (10I8)
! PROCESS A WHOLE LINE OF 10 ENTRIES:
   19     DO I = 1, 10
            ARGI = ARGMIN(JSOURC) + FLOAT(KOUNT)*ARGSTP(JSOURC)
            KOUNT = KOUNT + 1
            IF (NN(I).EQ.0 .AND. MODEOB(JSOURC).EQ.3) GOTO 12
            OBS = FLOAT(INOBS(I))
            DOBS = SQRT(OBS/FLOAT(NN(I)))
            CALL CONTRI(PCXX,PFXX,ISCR(NFLIP))
   12     ENDDO
          GOTO 10
! END OF INPUT:
   50     CONTINUE
! SWITCH TEMPORARY DATASETS:
          CALL FLIP(NFLIP,NFLOP)
! END OF ONE PHASE
        ENDDO
! END OF ONE SOURCE OF OBS:
        IF (.NOT.SIMUL .OR. PRECYC) CALL CLOFIL(LUNI)
      ENDDO
! LEAVES NFLOP AS ONE TO READ FROM:

      END SUBROUTINE INOBPR
!
!*****************************************************************************
!
      SUBROUTINE INPLP0(PCXX,PFXX)
!
! *** INPLP0 updated by JCM 28 Dec 92 ***
!
!H Reads L cards for Profile refinement, phase 0 (phase independent cards)
!D Reads most L cards, ignoring FIX, VARY, RELA, FUDG which will be read later.
!
!D If called from RDPHA0, records information off the following:
!D    SCAL   Scale factor
!D    RTYP   Mode of presentation of input reflection data
!D    WGHT   Weighting scheme (one day take this out into STLSP0 and use WGHTLS)
!D    PKCN   Peak centre parameters (which NB are being kept as one
!D           for each phase at present, just in case)
!D    ZERO   Zero point (ditto)
!D    OTYP   Mode of presentation of input observation data
!D    BACK   Background approximation - may be several cards
!D    EXCL   Excluded regions - may be several cards
!D    WVLN   Wavelengths (for constant wavelength or X-Ray)
!D    ABSC   Absorption correction coefficient
!D    EXTN   Extinction correction coefficient
!D    PROR   Preferred orientation coefficient
!D    THE2   2 theta for time of flight
!D    TTHM   Monochromator 2 theta angle (X-Ray)
!
      EXTERNAL PCXX, PFXX
      CHARACTER*2 INEX(2)
      CHARACTER*14 ARGTYP(2)
      CHARACTER*4 IWD, LTABLE(22), LTICTB(5), LPH0TB(14)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OMITPR/ MIS, AMISS(3,100), KOM12
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT

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
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DATA LTABLE/'TFAC', 'SCAL', 'SLIM', 'RTYP', 'SPHA', 'WGHT',       &
     &     'PKCN', 'ZERO', 'PKFN', 'OTYP', 'BACK', 'EXCL', 'WVLN',      &
     &     'ABSC', 'EXTN', 'PROR', 'TTHM', 'THE2', 'OMIT', 'REFK', ' ', &
     &     ' '/
      DATA LPH0TB/'SCAL', 'RTYP', 'WGHT', 'PKCN', 'ZERO', 'OTYP',       &
     &     'BACK', 'EXCL', 'WVLN', 'ABSC', 'EXTN', 'PROR', 'TTHM',      &
     &     'THE2'/
      DATA LTICTB/'RTYP', 'PKCN', 'ZERO', 'WVLN', 'THE2'/
      DATA INEX/'In', 'Ex'/
      DATA ARGTYP/'Time of flight', '2 theta'/

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! SET 'NO L CARDS READ':
      CALL JGMZER(ILREAD,1,22)
! READ ALL 'L' CARDS:
      IF (MULFAS) CALL P0TEMP(.TRUE.)
      ID = IABS(INREAD(12))
      NCARD = ICDNO(12)
      IF (NCARD.LE.0) THEN
        CALL ERRMES(1,1,'No "L" cards given')
        GOTO 100
      ENDIF
      DO ICD = 1, NCARD
        CALL CARDIN(ID)
        ID = ID + NYZ
        CALL RDWORD(IWD,LEN,3,IPT,80,0,IER)
! IGNORE L CARD IF NOT ONE WE WANT:
        L = NCFIND(IWD,LTABLE,22)
        IF (L.LE.0) GOTO 3
!* NB IF WE EVER WANT THESE AFTER THE INPUT PHASE WE MUST DO BETTER THAN THIS
        IF (TIC) THEN
          L = NCFIND(IWD,LTICTB,5)
          IF (L.LE.0) GOTO 3
          GOTO (14,17,18,23,28), L
        ENDIF
! NOW DO WE WANT IT FOR PHASE 0?
        LL = NCFIND(IWD,LPH0TB,14)
        IF (LL.LE.0) GOTO 3
        ILREAD(L) = ILREAD(L) + 1
        GOTO (12,14,16,17,18,20,21,22,23,24,25,26,27,28), LL
! L SCAL - THIS IS THE SCALE OF A SOURCE:
   12   CALL LSSCAL(1)
        GOTO 3
! L RTYP:
   14   CALL RDINTG(MODERR(KSOURC),IPT,IPT,80,IER)
        MMODER = IABS(MODERR(KSOURC))
        IF (MMODER.GT.3) GOTO 7
        IA = 1
        IF (.NOT.TOF) IA = 2
        CALL RDREAL(ARGMIN(KSOURC),IPT,IPT,80,IER)
        CALL RDREAL(ARGMAX(KSOURC),IPT,IPT,80,IER)
        CALL RDREAL(ARGSTP(KSOURC),IPT,IPT,80,IER)
        WRITE (LPT,2009) ARGTYP(IA), ARGMIN(KSOURC)
 2009   FORMAT (//' Data limits considered:'/' Minimum ',A14,' =',F12.2)
        IF (ARGMAX(KSOURC).NE.0.) WRITE (LPT,2006) ARGTYP(IA), ARGMAX(KSOURC)
 2006   FORMAT (' Maximum ',A14,' =',F12.2)
        IF (ARGSTP(KSOURC).NE.0.) WRITE (LPT,2007) ARGTYP(IA), ARGSTP(KSOURC)
 2007   FORMAT (' Step in ',A14,' =',F12.2)
        GOTO (71,72,73), MMODER
    7   CALL ERRIN2(MODERR(KSOURC),2,'reflection input type','not allowed')
        GOTO 3
   71   CALL MESS(LPT,1,'Reflection indices input as sets of 3 I5 integers from .HKL file')
        GOTO 10
   72   CALL MESS(LPT,1,'Reflection indices to be generated by program then output to file .HKL')
        GOTO 10
   73   CALL MESS(LPT,1,'Reflection indices to be generated by program - regenerate next run')
   10   I = 2
        IF (MODERR(KSOURC).LT.0) I = 1
        CALL MESS(LPT,1,INEX(I)//'clude any space group absences')
        GOTO 3
! L WGHT:
   16   CALL RDINTG(IWGH(KSOURC),IPT,IPT,80,IER)
        IF (IWGH(KSOURC).GT.3 .OR. IWGH(KSOURC).LE.0) GOTO 8
        GOTO (41,42,43), IWGH(KSOURC)
    8   CALL ERRIN2(IWGH(KSOURC),2,'weighting scheme','not allowed - type 1, 2 or 3 only')
        GOTO 3
! UNIT WEIGHTS:
   41   CALL MESS(LPT,1,'Unit weights')
        GOTO 3
! WEIGHT TO BE USED AS READ:
   42   CALL MESS(LPT,1,'Weights to be used as read from reflection data')
        GOTO 3
! SIGMA READ, WEIGHT IS 1/SIGMA SQUARED:
   43   CALL MESS(LPT,1,'Sigma read from reflection data - weight is 1/sigma squared')
        GOTO 3
! L PKCN:
   17   CALL RDWORD(IWD,LEN,IPT,IPT,80,-1,IER)
        IF (IWD.NE.'TYPE') CALL PCXX(1)
!* TEMPORARY - PFXX - IE PFALL - IGNORES ITS OWN 'TYPE' CARDS
        GOTO 3
! L ZERO:
   18   CALL ZEROPR(1)
        GOTO 3
! L OTYP:
   20   CALL RDINTG(MODEOB(KSOURC),IPT,IPT,80,IER)
        IF (MODEOB(KSOURC).LT.0 .OR. MODEOB(KSOURC).GT.4) THEN
          CALL ERRIN2(MODEOB(KSOURC),2,'mode of giving obs data','unacceptable')
          GOTO 3
        ENDIF
        GOTO (61,62,63,64,65), MODEOB(KSOURC) + 1
   61   CALL MESS(LPT,1,'Observations to be input by user''s routine QPRIN')
        GOTO 3
   62   CALL MESS(LPT,1,'Observations to be input as tof,Yobs,DYobs,(scale),(code), in formats F10 and I5')
        GOTO 3
   63   CALL MESS(LPT,1,'Observations to be input as tof,Yobs,DYobs,'// &
     &            '(scale),(code), unformatted from binary file')
        GOTO 3
   64   CALL MESS(LPT,1,'Observations to be input as sets of n,obs in format 10(I2,I6)')
        GOTO 3
   65   CALL MESS(LPT,1,'Observations to be input as sets of obs in format 10(I8)')
        GOTO 3
! L BACK:
   21   CALL BACKPR(1)
        GOTO 3
!*??? ought these to be per phase?
! L EXCL:
   22   CALL EXCLIN
        GOTO 3
! L WVLN:
   23   IF (TOF) GOTO 3
        CALL RDNUMS(ALAMBD(1,KSOURC),IPT,5,NLAMB,IER)
        WRITE (LPT,2002) NLAMB, (ALAMBD(I,KSOURC),I=1,NLAMB)
 2002   FORMAT (' ',I3,' Wavelength(s): ',5(1X,F9.6))
        GOTO 3
! L ABSC:
   24   CALL ABCRPR(1)
        GOTO 3
! L EXTN:
   25   CALL EXCRPR(1)
        GOTO 3
! L PROR:
   26   CALL PREFOR(1)
        GOTO 3
! L TTHM:
   27   CALL TTHMLX(1)
        GOTO 3
! L THE2:
   28   CALL RDREAL(TWOTHD(KSOURC),IPT,IPT,80,IER)
        SINTH = SIN(RADIAN(TWOTHD(KSOURC)/2.))
        TWSNTH(KSOURC) = 2.*SINTH
        WRITE (LPT,2020) TWOTHD(KSOURC), TWSNTH(KSOURC)
 2020   FORMAT (/' 2 theta =',F10.3,' degrees;  2 sin theta =',F10.5)
        GOTO 3
    3 ENDDO
  100 IF (MULFAS) CALL P0TEMP(.FALSE.)

      END SUBROUTINE INPLP0
!
!*****************************************************************************
!
      SUBROUTINE INPLPR(PCXX,PFXX)
!
! *** INPLPR updated by JCM 30 Jan 92 ***
!
!H Reads most L cards for Profile refinement, in sequence
!D Reads L cards, ignoring FIX, VARY, RELA, FUDG which will be read later,
!D and all the cards which would be phase-independent in multiphase, which
!D have been read already by INPLP0.
!
!D If called from STLSPR, records information off the following:
!D    TFAC   An overall temperature factor (applied squared in this case)
!CD    SCAL   Scale factors
!CD    RTYP   Mode of presentation of input reflection data
!CD    WGHT   Weighting scheme
!CD    PKCN   Peak centre parameters
!CD    ZERO   Zero point
!D    PKFN   Peak function parameters
!CD    OTYP   Mode of presentation of input observation data
!CD    BACK   Background approximation - may be several cards
!CD    EXCL   Excluded regions - may be several cards
!CD    WVLN   Wavelengths (for constant wavelength or X-Ray)
!CD    ABSC   Absorption correction coefficient
!CD    EXTN   Extinction correction coefficient
!CD    PROR   Preferred orientation coefficient
!CD    THE2   2 theta for time of flight
!CD    TTHM   Monochromator 2 theta angle (X-Ray)
!D    SLIM   Limits for CAIL - SAPS - APES
!D    OMIT   h k l values for reflection to omit this run
!
!CD If called from PICTIC, interprets only the 5 cards:
!CD    RTYP, PKCN, ZERO WVLN, THE2
!
      INCLUDE 'PARAMS.INC'

      EXTERNAL PCXX, PFXX
      CHARACTER*2 INEX(2)
      CHARACTER*14 ARGTYP(2)
      CHARACTER*4 IWD, LTABLE(22), LTICTB(5), LPHNTB(6)
      DIMENSION IH(3)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OMITPR/ MIS, AMISS(3,100), KOM12
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT

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

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DATA LTABLE/'TFAC', 'SCAL', 'SLIM', 'RTYP', 'SPHA', 'WGHT',       &
     &     'PKCN', 'ZERO', 'PKFN', 'OTYP', 'BACK', 'EXCL', 'WVLN',      &
     &     'ABSC', 'EXTN', 'PROR', 'TTHM', 'THE2', 'OMIT', 'REFK', ' ', &
     &     ' '/
      DATA LTICTB/'RTYP', 'PKCN', 'ZERO', 'WVLN', 'THE2'/
      DATA LPHNTB/'TFAC', 'PKFN', 'SLIM', 'OMIT', 'SPHA', 'REFK'/
      DATA INEX/'In', 'Ex'/
      DATA ARGTYP/'Time of flight', '2 theta'/
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER
!
!C SET 'NO L CARDS READ':
!
! READ ALL 'L' CARDS:
      INREAD(12) = -IABS(INREAD(12))
      ID = IABS(INREAD(12))
      NCARD = ICDNO(12)
      IF (NCARD.LE.0) THEN
        CALL ERRMES(1,1,'No "L" cards given')
        GOTO 100
      ENDIF
      DO ICD = 1, NCARD
        CALL CARDIN(ID)
        ID = ID + NYZ
        CALL RDWORD(IWD,LEN,3,IPT,80,0,IER)
! IGNORE L CARD IF NOT ONE WE WANT:
        L = NCFIND(IWD,LTABLE,22)
        IF (L.LE.0) GOTO 3
! NOW FIND OUT IF WE REALLY WANT IT:
        IF (.NOT.TIC) THEN
          LL = NCFIND(IWD,LPHNTB,6)
          IF (LL.EQ.0) GOTO 3
          ILREAD(L) = ILREAD(L) + 1
          GOTO (11,19,13,29,15,30), LL
        ELSE
          LL = NCFIND(IWD,LTICTB,5)
          IF (LL.LE.0) GOTO 3
          ILREAD(L) = ILREAD(L) + 1
          GOTO (14,17,18,23,28), LL
        ENDIF
! L TFAC:
   11   CALL LLTFAC(1)
        GOTO 3
!C
!C L SCAL - THIS IS THE SCALE OF A SOURCE:
!  12  CALL LSSCAL(1)
!      GO TO 3
!
! L SLIM:
   13   CALL FAM4PR(1,PCXX,PFXX)
        GOTO 3
!
!*???only once?
! L RTYP:
   14   CALL RDINTG(MODERR(KSOURC),IPT,IPT,80,IER)
        MMODER = IABS(MODERR(KSOURC))
        IF (MMODER.GT.3) GOTO 7
        IA = 1
        IF (.NOT.TOF) IA = 2
        CALL RDREAL(ARGMIN(KSOURC),IPT,IPT,80,IER)
        CALL RDREAL(ARGMAX(KSOURC),IPT,IPT,80,IER)
        CALL RDREAL(ARGSTP(KSOURC),IPT,IPT,80,IER)
        WRITE (LPT,2009) ARGTYP(IA), ARGMIN(KSOURC)
 2009   FORMAT (//' Data limits considered:'/' Minimum ',A14,' =',F12.2)
        IF (ARGMAX(KSOURC).NE.0.) WRITE (LPT,2006) ARGTYP(IA), ARGMAX(KSOURC)
 2006   FORMAT (' Maximum ',A14,' =',F12.2)
        IF (ARGSTP(KSOURC).NE.0.) WRITE (LPT,2007) ARGTYP(IA), ARGSTP(KSOURC)
 2007   FORMAT (' Step in ',A14,' =',F12.2)
        GOTO (71,72,73), MMODER
    7   CALL ERRIN2(MODERR(KSOURC),2,'reflection input type','not allowed')
        GOTO 3
   71   CALL MESS(LPT,1,'Reflection indices input as sets of 3 I5 integers from .HKL file')
        GOTO 10
   72   CALL MESS(LPT,1,'Reflection indices to be generated by program then output to file .HKL')
        GOTO 10
   73   CALL MESS(LPT,1,'Reflection indices to be generated by program - regenerate next run')
   10   I = 2
        IF (MODERR(KSOURC).LT.0) I = 1
        CALL MESS(LPT,1,INEX(I)//'clude any space group absences')
        GOTO 3
! L SPHA:
   15   CALL LPSCAL(1)
        GOTO 3
! L PKCN:
   17   CALL RDWORD(IWD,LEN,IPT,IPT,80,-1,IER)
        IF (IWD.NE.'TYPE') CALL PCXX(1)
!* TEMPORARY - PFXX - IE PFALL - IGNORES ITS OWN 'TYPE' CARDS
        GOTO 3
! L ZERO:
   18   CALL ZEROPR(1)
        GOTO 3
! L PKFN:
   19   CALL PFALL(1)
        GOTO 3
! L WVLN:
   23   IF (TOF) GOTO 3
        CALL RDNUMS(ALAMBD(1,KSOURC),IPT,5,NLAMB,IER)
        WRITE (LPT,2002) NLAMB, (ALAMBD(I,KSOURC),I=1,NLAMB)
 2002   FORMAT (' ',I3,' Wavelength(s): ',5(1X,F9.6))
        GOTO 3
! L THE2:
   28   CALL RDREAL(TWOTHD(KSOURC),IPT,IPT,80,IER)
        SINTH = SIN(RADIAN(TWOTHD(KSOURC)/2.))
        TWSNTH(KSOURC) = 2.*SINTH
        WRITE (LPT,2020) TWOTHD(KSOURC), TWSNTH(KSOURC)
 2020   FORMAT (/' 2 theta =',F10.3,' degrees;  2 sin theta =',F10.5)
        GOTO 3
! L OMIT:
   29   DO I = 1, 3
          CALL RDINTG(IH(I),IPT,IPT,80,IER)
        ENDDO
        IER = IERR
        CALL ERRCHK(2,MIS,100,0,'omitted reflections')
        IF (IBMBER .NE. 0) RETURN
        IF (IER.NE.IERR) GOTO 3
        CALL INDFLO(AMISS(1,MIS),IH)
        WRITE (LPT,2000) IH
 2000   FORMAT (' Reflection',3I5,' to be EXcluded from the refinement')
        GOTO 3
! L REFK:
   30   CALL RDREAL(AKNOTS,IPT,IPT,80,IER)
        IF (AKNOTS.LE.1.) THEN
          WRITE (LPT,2051) AKNOTS
 2051     FORMAT (/' Knots required at spacing of',F7.3,' times number of reflections')
        ELSE
          WRITE (LPT,2052) NINT(AKNOTS)
 2052     FORMAT (/' Knots required at',I4,' peaks')
        ENDIF
    3 ENDDO
  100 RETURN

      END SUBROUTINE INPLPR
!
!*****************************************************************************
!
      SUBROUTINE INRFPR(PCXX,PFXX)

      USE REFVAR

!
! *** INRFPR updated by JBF 4 July 1955 ***
!
!C 19B
!H Organises reflection data for all sorts of PR
!A PCXX must be set to the required Peak Centre subroutine
!A PFXX must be set to the required Peak Function subroutine
!P MODER must have been set to give type of reflection input
!P MAG must be .FALSE. for non-magnetic, .TRUE. for magnetic
!P Symmetry must be set by SYMOP and SYMUNI
!P IREF etc must be set in /REFINE/ giving type of refinement
!
!D Obtains list of h,k,l s, either by reading them in from some
!D previous run, or generating them.  Fills in /REFLNS/ arrays rHKL,
!D AMUL, and if Pawley-type and reading h,k,l, reads F4PAR(1, in /F4PARS/
!D also, and possibly F4PAR(2.
!D
!D If entered with TIC = .TRUE. generates reflections.
!
      LOGICAL NOMORE, ISPABS, SFC, MAGABS, MAGNET
      CHARACTER*1 ICHR
      CHARACTER*129 VFMT
      CHARACTER*131 VFMM
      COMPLEX FCALC, FCAL
      EXTERNAL PCXX, PFXX

      INCLUDE 'PARAMS.INC'

      DIMENSION IH(3), H(3), TEMREF(3,ITMREF), IORDER(ITMREF),          &
     &          TEMMUL(ITMREF), ARG(ITMREF), TF4P(6,ITMREF), TEMP(6),   &
     &          ARGN(ITMREF), ANT(ITMREF)

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
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /OMITPR/ MIS, AMISS(3,100), KOM12

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

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DATA VFMT/                                                        &
     &'(/'' Reflections to be used:''/'' Serial  h'',3X,''k'',3X,''l'',2&
     &X,''  Argument  D-spacing M'',7X,''F = A + iB'',5X,    ''F*F     I&
     &ntensity'')'/
      DATA VFMM/                                                        &
     &'(/'' Reflections to be used:''/'' Serial  h'',3X,''k'',3X,''l'',2&
     &X,''  Argument  D-spacing M'',3X,''    F*F   '',6X,    ''Q*Q      &
     &  Intensity'')'/
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER
!
! DECIDE WHETHER INDICES MAY HAVE FRACTIONAL PARTS:
!
      FIXED = (.NOT.MAG .OR. (IPROP.LE.0))
      IF (.NOT.FIXED) THEN
        VFMM(43:43) = '5'
        VFMM(50:50) = '5'
        VFMM(57:57) = '4'
      ENDIF
! DECIDE WHETHER FCS MAY BE PRINTED:
      IF (TIC) THEN
        SFC = (ICDNO(1).NE.0 .AND. ICDNO(6).NE.0)
      ELSE
        SFC = (RIET .OR. SAPS)
      ENDIF
      IF (.NOT.SFC) THEN
        VFMT(86:) = ')'
        VFMT(86:) = ')'
      ENDIF
! COUNT REFLECTIONS:
      MAXKK(JPHASE) = 0
! FOR ALL SOURCES:
      DO JSOUR = 1, NSOURC
        JSOURC = JSOUR
        CALL LOGSOU(JSOURC)
! IF MODER +VE, SPACE GROUP ABSENCES TO BE EXCLUDED:
        MMODER = IABS(MODERR(JSOURC))
! SET SINTHETA/LAMBDA LIMIT
        SM = STHMXX(JSOURC)
        DSTAMX = 2.*SM
! IF REQUIRED, OPEN FILE ON WHICH TO READ H,K,L,INTS:
        IF (MMODER.EQ.1 .AND. .NOT.TIC) THEN
          MESSAG = 'File to read h,k,l list'
          NAMFIL = '.HKL'
          INHKL = -9999
          CALL OPNFIL(INHKL,111)
        ELSE
!  SET UP TO GENERATE NUCLEAR HKLS:
          CALL SETGEN(SM)
        ENDIF
        MAGREF = 1
        IF (MMODER.EQ.1 .AND. .NOT.TIC) MAGREF = 3
        NFLAG = -9999
! TYPE 1 READS LIST OF HKL'S TO USE FROM FILE:
! IF CAIL, READ INTS FOR EACH REFLECTION FROM PREVIOUS CYCLES
! IF SAPS/APES, READ INTS FOR EACH REFLECTION, AND POSSIBLY ALSO SIGS
! N.B. BOTH INTS AND SIGS HAVE THEIR SIGMAS THERE ALSO
    2   GOTO (46,47,49), MAGREF
   49   N2G4L = NUMS
        CALL RDDATA(INHKL,IH,H,TF4P(1,MAXKK(JPHASE)+1),-6,NUMS)
        IF (NUMS.EQ.-9999) GOTO 4
        MUL = MULBOX(H)
! JOIN TO TEST WHETHER WANTED:
        GOTO 50
! HERE TO GENERATE ALL HKL'S WITHIN GIVEN MAX THETA:
   46   CALL GENMUL(H,NOMORE,MUL)
        IF (NOMORE) THEN
          IF (MAG .AND. IPROP.NE.0) THEN
            MAGREF = 2
            GOTO 2
          ELSE
            GOTO 4
          ENDIF
        ENDIF
        GOTO 50
   47   CALL GENMAG(H,NOMORE,MUL,SM,NFLAG)
        IF (NOMORE) THEN
! RESTORE FULL SYMMETRY IF NECESSARY (JBF modification)
          IF (NKSTAR.GT.1) CALL SYMBAK
          GOTO 4
        ENDIF
! IF ASKED, IGNORE SPACE GROUP ABSENCES:
   50   IF (.NOT.MAG .AND. MODERR(JSOURC).GT.0) THEN
          IF (ISPABS(H)) GOTO 2
        ENDIF
        DSTAR(1) = VCTMOD(1.,H,2)
        IF (DSTAR(1).GT.DSTAMX) GOTO 2
! CHECK WHETHER INDICES ARE IN LIST TO OMIT:
        M = 1
        IF (MIS.GT.0) CALL EQVEC(AMISS,H,MIS,M,0)
        IF (M.LE.MIS) GOTO 2
! CHECK LIMITS OF ARGK:
        KNOW = 1
        CALL PCXX(5)
        IF (ARGK.LT.ARGMIN(JSOURC) .OR. (ARGMAX(JSOURC).NE.0. .AND. ARGK.GT.ARGMAX(JSOURC))) GOTO 2
! CHECK REFLECTION IN GIVEN ASYMMETRIC UNIT:
        IF (MUL.EQ.0) THEN
          WRITE (LPT,2000) H
 2000     FORMAT (/' Reflection',3F5.1,' not in given unit -- IGNORED')
          GOTO 2
        ENDIF
! HERE TO ACCEPT A SET OF INDICES:
        CALL ERRCHK(2,MAXKK(JPHASE),ITMREF,0,'reflections')
        IF (IBMBER .NE. 0) RETURN
        CALL GMEQ(H,TEMREF(1,MAXKK(JPHASE)),1,3)
        TEMMUL(MAXKK(JPHASE)) = FLOAT(MUL)
        ARG(MAXKK(JPHASE)) = ARGK
        GOTO 2
! ALL INDICES STORED NOW
    4   IF (MMODER.EQ.1) CALL CLOFIL(INHKL)
      ENDDO
      IF (MAXKK(JPHASE).LE.0) THEN
        CALL ERRMES(1,0,'no reflections found in data limits')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
! SORT INTO ORDER
      CALL SORT_REAL(ARG,IORDER,MAXKK(JPHASE))
      IF (TIC .OR. (IPRNT(2).GT.0)) THEN
        IF (SFC) THEN
          DO I = 1, 3
            H(I) = 0.
          ENDDO
          FCAL = FCALC(H)
          FC = CABS(FCAL)
          FCSQ = FC*FC
          WRITE (LPT,2050) FC, FCSQ
 2050     FORMAT (//' Nuclear F(0,0,0)   is   ',F10.2/'        F(0,0,0)^2 is ',F12.2//)
        ENDIF
        IF (MAG) THEN
          WRITE (LPT,VFMM)
        ELSE
          WRITE (LPT,VFMT)
        ENDIF
      ENDIF
!** EXPAND THIS IF FLOATING
! ^^^^^ WRITE MORE IF I OR SIGS READ
! THIS IS OUTPUT IF ANY PRINTING AT ALL IS REQUESTED FOR REFLNS
!
      IF (SAPS .OR. APES) THEN
        NG4L = N2G4L/2
        PRECYC = NG4L.LT.NGENPS(4,JPHASE)
        IF (PRECYC) SIMUL = .TRUE.
      ENDIF
      KNOW = 0
      KTIC = 0
      DO KSORT = 1, MAXKK(JPHASE)
        IF (KNOW.GT.0) THEN
          DO K = KNOW, 1, -1
            IF (ARG(IORDER(KSORT)).GT.ARGN(K)) GOTO 143
            DO I = 1, 3
              IF (ABS(TEMREF(I,IORDER(KSORT))-rHKL(I,K)) .GT. 0.0001) GOTO 144
            ENDDO
! SAME H,K,L WITH SAME ARG - IGNORE:
            GOTO 6
  144     ENDDO
        ENDIF
  143   KNOW = KNOW + 1
        CALL GMEQ(TEMREF(1,IORDER(KSORT)),rHKL(1,KNOW),1,3)
        IF (FIXED) CALL INDFIX(rHKL(1,KNOW),IH)
        CALL CELDER(rHKL(1,KNOW),TEMP)
        DSTAR(KNOW) = SQRT(DSTAR2)
        DSP = 1./DSTAR(KNOW)
        AMUL(KNOW) = TEMMUL(IORDER(KSORT))
        MUL = NINT(AMUL(KNOW))
! ^^^^ THE FOLLOWING DO LOOP IS NEW
        IF (CAIL .OR. SAPS .OR. APES) THEN
          DO IG = 1, NGENPS(4,JPHASE)
            IT2 = 2*IG
            IT1 = IT2 - 1
            F4PAR(IG,KNOW) = TF4P(IT1,IORDER(KSORT))
            F4PESD(IG,KNOW) = TF4P(IT2,IORDER(KSORT))
          ENDDO
        ENDIF
        ARGK = ARG(IORDER(KSORT))
        ARGN(KNOW) = ARGK
        ZARGK(KNOW) = ARGK
        ICHR = ' '
        IF (MODER.LT.0) THEN
          IF (ISPABS(rHKL(1,KNOW))) ICHR = '*'
        ENDIF
        MAGNET = .NOT.MAGABS(rHKL(1,KNOW),IKK)
        IF (MAGNET) THEN
          ISMAG(KNOW) = IKK
        ELSE
          ISMAG(KNOW) = 0
        ENDIF
! ARE WE DOING A STRUCTURE FACTOR CALCULATION?
        IF (SFC) THEN
! SET UP MULTIPLIERS OF F*F TO GIVE OBSERVED INTENSITY
          IF (TOF) THEN
            FAC = 1./(DSTAR2*DSTAR2)
          ELSE
            IF (CN) FAC = 1./(SIN(RAD*ARGK)*SIN(0.5*RAD*ARGK))
            IF (LX) FAC = FAC*(1.+COS(DEG*ARGK)**2)
          ENDIF
! NOW CALCULATE NUCLEAR F*F IF REQUIRED
          STHL = 0.5/DSP
          IF (ISMAG(KNOW).EQ.0) THEN
            FCAL = FCALC(rHKL(1,KNOW))
            FC = CABS(FCAL)
            AF = REAL(FCAL)
            BF = AIMAG(FCAL)
            FCSQ = REAL(FCAL*CONJG(FCAL))
! IF TIC, WE STORE SEPARATE DETAILS OF NUCLEAR AND MAGNETIC INTENSITIES IN
! COMMON /REFLNS/ USING AICALC FOR ARG, AND AIOBS FOR NUCLEAR (+VE) OR
! MAGNETIC (-VE) INTENSITIES.
            IF (TIC) THEN
              CALL ERRCHK(2,KTIC,ITMREF,0,'intensity contributions')
              IF (IBMBER .NE. 0) RETURN
              AICALC(KTIC) = ARGK
              AIOBS(KTIC) = FAC*FCSQ*FLOAT(MUL)
            ENDIF
          ELSE
            FCSQ = 0.
          ENDIF
! AND MAGNETIC Q*Q IF NECESSARY
          IF (MAGNET) THEN
            CALL FMCALC(rHKL(1,KNOW),FMCMOD,FMCSQR)
            IF (TIC) THEN
              CALL ERRCHK(2,KTIC,ITMREF,0,'intensity contributions')
              IF (IBMBER .NE. 0) RETURN
              AICALC(KTIC) = ARGK
              AIOBS(KTIC) = -FAC*FMCSQR*FLOAT(MUL)
            ENDIF
          ELSE
            FMCSQR = 0.0
          ENDIF
          ANT(KNOW) = FAC*FLOAT(MUL)*(FCSQ+FMCSQR)
! NOW THE PRINTING IF REQUIRED
          IF (TIC .OR. (IPRNT(2).GT.0)) THEN
            IF (.NOT.MAG) THEN
              WRITE (LPT,2002) KNOW, ICHR, (IH(J),J=1,3), ARGK, DSP, MUL, AF, BF, FCSQ, ANT(KNOW)
 2002         FORMAT (' ',I4,A1,3I4,F12.3,F10.5,I3,3F9.3,F12.3)
            ELSE
              IF (IPROP.LE.0) THEN
                WRITE (LPT,2006) KNOW, ICHR, (IH(J),J=1,3), ARGK, DSP, MUL, FCSQ, FMCSQR, ANT(KNOW)
 2006           FORMAT (' ',I4,A1,3I4,F12.3,F10.5,I3,2(3X,F9.3),3X,F12.2)
              ELSE
                WRITE (LPT,2012) KNOW, ICHR, (rHKL(J,KNOW),J=1,3), ARGK, DSP, MUL, FCSQ, FMCSQR, ANT(KNOW)
 2012           FORMAT (' ',I4,A1,3F6.2,F12.3,F10.5,I3,2(3X,2F9.3),F12.2)
              ENDIF
            ENDIF
          ENDIF
        ELSE
! HERE IF WE CAN DO NO CALCULATIONS OF INTENSITY
          IF (TIC .OR. (IPRNT(2).GT.0)) THEN
            IF (.NOT.MAG .AND. .NOT.ISPABS(rHKL(1,KNOW))) THEN
! IF TIC, WE STORE SEPARATE DETAILS OF NUCLEAR AND MAGNETIC REFLECTIONS IN
! COMMON /REFLNS/ USING AICALC FOR ARG, AND AIOBS FOR NUCLEAR (+VE) OR
! MAGNETIC (-VE) REFLECTIONS.
              IF (TIC) THEN
                CALL ERRCHK(2,KTIC,ITMREF,0,'intensity contributions')
                IF (IBMBER .NE. 0) RETURN
                AICALC(KTIC) = ARGK
                AIOBS(KTIC) = 100.*MUL
              ENDIF
            ENDIF
! AND MAGNETIC F*F IF NECESSARY
            IF (MAG .AND. .NOT.MAGABS(rHKL(J,KNOW),IKK)) THEN
              IF (TIC) THEN
                CALL ERRCHK(2,KTIC,ITMREF,0,'intensity contributions')
                IF (IBMBER .NE. 0) RETURN
                AICALC(KTIC) = ARGK
                AIOBS(KTIC) = -100.*MUL
              ENDIF
            ENDIF
! AND NOW THE WRITING
            IF (FIXED) THEN
              WRITE (LPT,2004) KNOW, ICHR, (IH(J),J=1,3), ARGK, DSP, MUL
 2004         FORMAT (' ',I4,A1,1X,3I4,3X,F12.3,1X,F10.5,1X,I3)
            ELSE
              WRITE (LPT,2014) KNOW, ICHR, (rHKL(J,KNOW),J=1,3), ARGK, DSP, MUL
 2014         FORMAT (' ',I4,A1,3F6.2,F12.3,1X,F10.5,1X,I3)
            ENDIF
          ENDIF
        ENDIF
! PAWLEY-TYPE SETTING UP - IF NO F4PAR(1 READ SET IT TO BE 1:
        IF (CAIL .AND. F4PAR(1,KNOW).EQ.0.) F4PAR(1,KNOW) = 1.
        IF (RIET .OR. CAIL) GOTO 6
! IF SAPS OR APES, WE HAVE VARIOUS CASES:
!  A) ONCE WE HAVE STARTED, I, SIGI, SIGS AND SIGSIGS ARE ALL READ FROM .HKL
!  B) A .HKL FILE MAY HAVE I, SIGI BUT NO SGSQ AND SIGSIGS;  IN THAT CASE
!     THE INITIAL SIGS VALUES CAN BE CALCULATED BY FDXX (VIA PFXX(7))
!  C) IF THERE IS NO .HKL FILE, AT THIS POINT WE SHALL HAVE NO I AND NO SIGS.
!     IN THAT CASE WE NEED AN INITIAL CYCLE TO GENERATE I AND SIGI.
!     SIGS INITIALLY CAN COME VIA PFXX, AS IN (B).
!
! SAPS AND APES:
! INITIAL CYCLE NEEDED FOR VALUES OF I:
        IF (SAPS .OR. APES) THEN
          IF (PRECYC) THEN
! WE HAVE I VALUES, BUT NOT SIGS:
            CALL PCXX(5)
            CALL PFXX(7)
          ENDIF
        ENDIF
    6 ENDDO
! FINISHED LIST. STORE NUMBER OF INTENSITIES IN MAXKK
      MAXKK(JPHASE) = KNOW
      IF (TIC) MAXKK(JPHASE) = KTIC

      END SUBROUTINE INRFPR
!
!*****************************************************************************
!
      SUBROUTINE LOGPHA(N)
!
! *** LOGPHA by JCM 16 May 90 ***
!
!H Converts integer descibing refinement type to and from logicals
!A On entry N=which phase
!D Takes METHOD from /SOURCE/ and sets one of the logicals RIET, CAIL, SAPS,
!D APES, RAPS etc.
!
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      RIET = METHOD(N).EQ.1
      CAIL = METHOD(N).EQ.2
      SAPS = METHOD(N).EQ.3
      APES = METHOD(N).EQ.4
      RAPS = METHOD(N).EQ.5

      END SUBROUTINE LOGPHA
!
!*****************************************************************************
!
      SUBROUTINE LOGSOU(N)
!
! *** LOGSOU updated by JCM 23 Feb 93 ***
!
!H Converts integer descibing data source to logicals
!A On entry N=which source
!D Sets one of the logicals TOF, CN, LX, SR, ED
!
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      TOF = NDASOU(N).EQ.1
      CN = NDASOU(N).EQ.2
      LX = NDASOU(N).EQ.3
      SR = NDASOU(N).EQ.4
      ED = NDASOU(N).EQ.5

      END SUBROUTINE LOGSOU
!
!*****************************************************************************
!
      SUBROUTINE LOGSET
!
! *** LOGSET updated by JCM 14 Nov 90 ***
!
!H Sets up logicals needed for PR after variables have been made
!D Sets NOPKRF for "no peak refine", that is, there are no peak descriptor
!D parameters being refined on this cycle.
!D Sets CYC1 for "this is cycle 1" (CYC1 is thus also .TRUE. if this is
!D the pre-cycle for SAPS or APES.)
!
      INCLUDE 'PARAMS.INC'

      COMMON /CONSTR/ JCONST, JROWPT(301), JCMAT(200), AMOUNT(200), NEXTJ
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6


      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))

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

      CYC1 = (ICYC.EQ.NCYC1)
! SET NOPKRF TO BE TRUE IF NO PEAK FUNCTION PARAMETERS ARE TO BE REFINED:
      NOPKRF = .FALSE.
      DO JP = 1, NPHASE
        DO JS = 1, NSOURC
          DO I = 1, NPKGEN(JP,JS)
            DO J = 1, NPKFSP(I,JP,JS)
              IF (KPFNSP(I,J,JP,JS).GT.0) GOTO 9
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      NOPKRF = .TRUE.
    9 CONTINUE
!*
! IF CAIL, SET UP 2 VECTORS OF BASIC VARIABLE NUMBERS FOR INTS:
      IF (CAIL) THEN
! AND OVER ALL PHASES:
        DO JP = 1, NPHASE
          DO K = 1, MAXKK(JP)
! NB=WHICH BASIC (OR REDUNDANT) VARIABLE THIS "INTS" IS:
            NB = KF4PAR(1,K)
            IF (NB.GT.0) NB = LVRBS(NB)
! IF REDUNDANT, IT SHOULD BE BY A SIMPLE CONSTRAINT - SET NB TO BE THE
! BASIC VARIABLE INVOLVED:
            IF (NB.LT.0) NB = JCMAT(JROWPT(-NB))
            NBASF4(K,1,JP) = NB
            NBASF4(K,2,JP) = NB
          ENDDO
! NOW FILL IN ANY ZEROS (EXCEPT AT THE TWO ENDS) -  ( ,1)=THE FIRST
! NON-ZERO FOR INCREASING K, ( ,2)= THE FIRST NON-ZERO FOR DECREASING K:
          DO K = 1, MAXKK(JP)
            IF (NBASF4(K,1,JP).GT.0) GOTO 5
            DO I = K + 1, MAXKK(JP)
              IF (NBASF4(I,1,JP).EQ.0) GOTO 6
              NBASF4(K,1,JP) = NBASF4(I,1,JP)
              GOTO 4
    6       ENDDO
    4       DO I = K - 1, 1, -1
              IF (NBASF4(I,2,JP).EQ.0) GOTO 7
              NBASF4(K,2,JP) = NBASF4(I,2,JP)
              GOTO 5
    7       ENDDO
    5     ENDDO
! POINT TO LAST FAMILY 4 BASIC VARIABLE:
          L4END(JP) = LBFST1(4,JP,1) + NBARF(4,JP,1)
        ENDDO
      ENDIF

      END SUBROUTINE LOGSET
!
!*****************************************************************************
!
      SUBROUTINE LPSCAL(N)
!
! *** LPSCAL MK4 BY JCM AUG 89 ***
!
!H Deals with the Profile LSQ parameter SPHA, scale for this phase.
!A On entry N gives the required action:
!A   N=1 reads in an L SPHA card and records the scale factor for this phase
!A   N=3 applies a shift to a specific scale
!A   N=4 writes out a new L SPHA card to unit NEWIN
!A   N=5 deals with absence of L SPHA card
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)


      GOTO (1,100,3,4,5), N
    1 CALL RDREAL(SCALEP(JPHASE),7,IPT,80,IER)
      IF (IER.NE.0) IERR = IERR + 1
      WRITE (LPT,2000) JPHASE, SCALEP(JPHASE)
 2000 FORMAT (/' Scale factor for phase',I4,' =',F10.4)
      GOTO 100
    3 CALL ADJUST(SCALEP(JPHASE))
      GOTO 100
! NEW L SPHA CARD:
    4 WRITE (NEWIN,2005) (SCALEP(JPHASE))
 2005 FORMAT ('L SPHA',F10.5)
      GOTO 100
! N=5 - DEAL WITH NO SCAL CARDS:
    5 SCALEP(JPHASE) = 1.0
      GOTO 100
      ENTRY LPSCA8(NV)
! RECORD THAT THIS PHASE'S SCALE IS VARIABLE NUMBER NV:
      KSCALP(JPHASE) = NV
      GOTO 100
      ENTRY LPSCA9
! RECORD THAT THIS PHASE'S SCALE IS FIXED:
      KSCALP(JPHASE) = 0
  100 RETURN

      END SUBROUTINE LPSCAL
!
!*****************************************************************************
!
      SUBROUTINE LSETPR(PCXX,PFXX)
!
! *** LSETPR updated for MK4 by JCM 25 Jan 91 ***
!
!X
!C 6A
!H Set up specific LSQ problem - copy vocabulary to standard COMMON
!P The problem must have already been specified to the extent of:
!P    NFAM=number of families
!P    NGENPS(I,JPHASE)=number of genera in family I, phase JPHASE
!P    NSPCPS(I,JPHASE)=number of species in each genus of family I, phase JPHASE
!D Packs the LSPEC integers to 1 word, with phase and source, usually both
!D zero.
!D Copies both arrays LSPEC and LWORDS into standard arrays IWDSPC  and
!D LSQWD in /WDSPEC/ and /WORDS/.
!D Sets up pointers to starts of families of parameters.
!D Sets up the packing of IFAM, IGEN, ISPC, PHASE, SOURCE  into one integer
!D Sets up the permanent fix, vary and constraint lists
!
      EXTERNAL PFXX, PCXX
      EXTERNAL F2PARS
      EXTERNAL PRPARS
      DIMENSION LPAK(5)
      COMMON /F2NAMS/ F2NAME(40)
      CHARACTER*4 F2NAME
      COMMON /F2NUMS/ NF2NUM(3,40)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LINKAG/ NUMFV, NUMPAK, KKFV(200), KTYPFV(200), KSTFV(200),&
     &                KTIME(200), KUNPFV(5,30), NTIME, NUMCON,          &
     &                KKCON(500), AMCON(500), KPTCON(201), KSTCON(200), &
     &                KTPCON(200)
      COMMON /LSETDA/ MFAM, MGEN, MSPC, LASTST
      COMMON /LSQPAK/ KKPACK(10,3)

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
      COMMON /PRNAMS/ PRNAME(14)
      CHARACTER*4 PRNAME
      COMMON /PRNUMS/ NPRNUM(3,14)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /WDSPC / IWDNUM, IWDSPC(60)
      COMMON /WORDS / LSQWD(60)
      CHARACTER*4 LSQWD

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! ONLY ENTRY IF SINGLE PHASE, OR FIRST ENTRY IF MULTIPHASE:
      IF (JPHASE.EQ.1) THEN
        MFAM = 0
        MGEN = 0
        MSPC = 0
! PREPARE FIX LIST AND CONSTRAINT LIST FOR CALLS OF ADDFIX, ADDCON
        NUMFV = 0
        NUMPAK = 0
        NUMCON = 0
        KPTCON(1) = 1
      ENDIF
      CALL ERRCHK(1,NFAM,6,0,'LSQ families of parameters')
      IF (IBMBER .NE. 0) RETURN
! STARTS OF FAMILIES OF PARAMETERS:
      IF (NFAM.GT.MFAM) MFAM = NFAM
      DO I = 1, NFAM
        IF (NGENPS(I,JPHASE).GT.MGEN) MGEN = NGENPS(I,JPHASE)
        IF (NSPCPS(I,JPHASE).GT.MSPC) MSPC = NSPCPS(I,JPHASE)
      ENDDO
! IF LAST PHASE:
      IF (JPHASE.EQ.NPHASE) THEN
! SET UP PACKING OF IFAM,IGEN,ISPC,PHASE,SOURCE:
        LPAK(1) = MFAM
        LPAK(2) = MGEN
        LPAK(3) = MSPC
        LPAK(4) = NPHASE
        LPAK(5) = NSOURC
! THIS INITIALISES ALL SUBSEQUENT CALLS OF KPAK, KUNPAK
        L = 3
        IF (MULONE) L = 5
        CALL NPACK(N,LPAK,L,0,KKPACK)
! WORDS RELEVANT TO THIS PROBLEM FROM DATA INTO COMMON:
        KPHASE = 0
        KSOURC = 0
! SET UP VOCABULARY: FIRST, THE CORE FOR ALL STRUCTURE FACTOR LSQ:
        IWDNUM = 0
        CALL VOCAB(F2NAME,NF2NUM,40)
! THEN A DEFAULT PROFILE VOCABULARY:
        CALL VOCAB(PRNAME,NPRNUM,14)
! ADD SPECIAL VOCABULARY LINKED TO PEAK FUNCTION:
        DO JSOUR = 1, NSOURC
          JSOURC = JSOUR
          KPHASE = 0
          KSOURC = JSOURC
          CALL PFXX(1)
        ENDDO
      ENDIF

      END SUBROUTINE LSETPR
!
!*****************************************************************************
!
      BLOCKDATA PRPARS
      COMMON /PRNAMS/ PRNAME(14)
      CHARACTER*4 PRNAME
      COMMON /PRNUMS/ NPRNUM(3,14)
      DATA PRNAME/'PKFN', 'FAM4', 'FAM6', 'EXTN', 'PROR', 'SPHA',       &
     &     'ZERO', 'PKCN', 'INTS', 'SIGS', 'GAMS', 'SCAL', 'ABSC',      &
     &     'BACK'/
      DATA NPRNUM/3, 0, 0, 4, 0, 0, 6, 0, 0, 1, 1, 8, 1, 1, 9, 1, 1, 10,&
     &     3, 1, 0, 3, 2, 0, 4, 1, 0, 4, 2, 0, 4, 3, 0, 6, 1, 1, 6, 2,  &
     &     0, 6, 3, 0/
      END BLOCKDATA PRPARS
!
!*****************************************************************************
!
      SUBROUTINE LSSCAL(N)
!
! *** LSSCAL MK4 by JCM Aug 89 ***
!
! DOES MOST OF THE DEALING WITH THE PARAMETER IN LSQ CALLED 'SCAL' FOR SCALE
! FACTORS.
!
!* FOR THE MOMENT WE HAVE LOST N=0 - SO THERE MUST BE AN L SCAL CARD
! N=0 DEALS WITH THE INTRODUCTION OF A SINGLE SCALE FACTOR OF 1., TO BE REFINED,
!     IF NO L SCAL CARDS HAVE BEEN READ.
! N=1 READS IN AN L SCAL CARD AND RECORDS THE SCALE FACTORS
! N=2 DOES NOT FOR THE MOMENT EXIST.  IF IT DID, IT WOULD DO THE 'CALCULATE'
!     STEP, I.E. SEND OUT A CALCULATED VALUE TO USE IN THE REFINEMENT. SINCE
!     THIS IS ACTUALLY A TRIVIAL OPERATION, IT IS DONE IN THE VARIOUS
!     SUBR0UTINES 'CALCXX' RATHER THAN BY A SPECIAL CALL TO LSSCAL.
! N=3 APPLIES A SHIFT TO A SPECIFIC SCALE(ISPC)
! N=4 WRITES OUT A NEW L SCAL CARD
! N=-4 WRITES OUT A NEWLY GENERATED SCALE CARD (IF THERE HAD BEEN NONE AT ALL,
!      SO ONE HAD BEEN MADE
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (N.EQ.0) THEN
        CALL ERRMES(3,1,'L SCAL')
        GOTO 100
      ENDIF
      GOTO (1,100,3,4), N
    1 CALL RDNUMS(SCALES,7,5,NUM,IER)
      IF (IER.NE.0) IERR = IERR + 1
      IER = IERR
      IF (IER.NE.IERR) GOTO 100
      CALL MESS(LPT,1,'Scale factor(s) :')
      CALL PRILIS(SCALES,1,NUM)
      GOTO 100
! TAKE CARE ONLY TO APPLY THE SHIFT ONCE:
    3 IF (JPHASE.EQ.1) CALL ADJUST(SCALES(JSOURC))
      GOTO 100
! NEW L SCAL CARD:
    4 WRITE (NEWIN,2005) (SCALES(J),J=1,NSOURC)
 2005 FORMAT ('L SCAL',8F10.5)
!* PROVISION FOR *S NEEDED
      GOTO 100
      ENTRY LSSCA8(NV)
! RECORD THIS SOURCE'S SCALE IS VARIABLE NUMBER NV:
      KSCALS(JSOURC) = NV
      GOTO 100
      ENTRY LSSCA9
! RECORD THIS SOURCE'S SCALE IS FIXED:
      KSCALS(JSOURC) = 0
  100 RETURN

      END SUBROUTINE LSSCAL
!
!*****************************************************************************
!
      SUBROUTINE NWINPR(PCXX,PFXX,MAGSHF)
!
! *** NWINPR updated by PJB 1 Feb 1994 ***
!
!C 19B
!H Output updated Crystal Data file to unit NEWIN
!
!P All necessary shifts must have already been applied, say by APSHPR
!D Rereads all cards;  selects those which contain variables
!O Writes all new and unchanged cards to unit NEWIN, using calls to
!O parameter-specific routines
!
      EXTERNAL PCXX, PFXX, MAGSHF

      INCLUDE 'PARAMS.INC'

      CHARACTER*4 WORD, CHANGE(13)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PHAS0 / INRLP0, ICDLP0, INRLP1, ICDLP1, NCDF0

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
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DATA CHANGE/'TFAC', 'SCAL', 'RTYP', 'PKCN', 'PKFN', 'ZERO',       &
     &     'ATOM', 'BACK', 'ABSC', 'EXTN', 'PROR', 'TTHM', 'SPHA'/
!
      IF (SIMUL) GOTO 100
! OPEN NEW CD FILE:
      CALL NEWCD
! START RECORD COUNT IN IO10:
      ID = 0
      JPHASE = 1
      CALL PHMOVE(1,1)
    1 ID = ID + 1
      IF (MULFAS .AND. ID.EQ.NCDF0+1) WRITE (NEWIN,2005)
      IF (ID.GT.NTOTAL(JPHASE)) THEN
        IF (JPHASE.EQ.NPHASE) GOTO 100
        WRITE (NEWIN,2005)
        JPHASE = JPHASE + 1
        CALL PHMOVE(1,JPHASE)
      ENDIF
      READ (IO10,REC=ID,FMT=1000) ICARD
 1000 FORMAT (A80)
      L = LETTER(ICARD(1:1))
      IF (L.EQ.3) THEN
! OUTPUT NEW C CARD WITH NEW VALUES:
        CALL CELNEW
        GOTO 1
      ELSEIF (L.EQ.12) THEN
! L CARD - CONSULT LIST OF WORDS INDICATING CARDS TO CHANGE
        CALL RDWORD(WORD,IWDLEN,3,IPT,80,0,IER)
        N = NCFIND(WORD,CHANGE,13)
        IF (N.EQ.0) GOTO 2
        GOTO (21,22,23,24,25,26,27,28,29,30,31,32,33), N
      ELSEIF (L.EQ.1 .OR. L.EQ.6 .OR. L.EQ.20) THEN
! TREAT A, F AND T ALIKE:
! NOT IF CAIL/APES - THERE WILL BE NO STRUCTURE PARAMETERS:
        IF (CAIL .OR. APES) GOTO 2
        CALL F2NEW(L)
        GOTO 1
      ELSEIF (L.EQ.17) THEN
! "Q" CARD:
        IF (.NOT.MAG) GOTO 2
        IF (CAIL .OR. APES) THEN
          CALL PROPAG(4,NEWIN)
        ELSE
          CALL MAGSHF(4)
        ENDIF
        GOTO 1
      ELSEIF (L.EQ.9) THEN
! OUTPUT NEW I CARD:
        CALL OTPUTI
        GOTO 1
      ENDIF
      GOTO 2
! TFAC:
   21 CALL LLTFAC(4)
      GOTO 1
! SCAL:
   22 CALL LSSCAL(4)
      GOTO 1
! RTYP:
   23 MM = MODERR(JSOURC)
      IF (IABS(MM).NE.3) MM = ISIGN(1,MM)
      IF (TOF) THEN
        WRITE (NEWIN,2001) MM, ARGMIN(JSOURC), ARGMAX(JSOURC)
 2001   FORMAT ('L RTYP',I5,2F10.2)
      ELSE
        WRITE (NEWIN,2002) MM, ARGMIN(JSOURC), ARGMAX(JSOURC),ARGSTP(JSOURC)
 2002   FORMAT ('L RTYP',I5,3F10.3)
      ENDIF
      GOTO 1
! PKCN:
   24 CALL RDWORD(WORD,LEN,IPT,IPT,80,-1,IER)
      IF (WORD.EQ.'TYPE') GOTO 2
      CALL PCXX(4)
!* TEMPORARY - PFXX - IE PFALL - IGNORES ITS OWN 'TYPE' CARDS
      GOTO 1
! PKFN:
   25 CALL PFALL(4)
      GOTO 1
! ZERO:
   26 CALL ZEROPR(4)
      GOTO 1
! L ATOM
   27 CALL GEOMCO(3)
      GOTO 1
! L BACK
   28 IF (IBACK.LE.0) GOTO 2
      CALL BACKPR(4)
      GOTO 1
! ABSC
   29 CALL ABCRPR(4)
      GOTO 1
! EXTN
   30 CALL EXCRPR(4)
      GOTO 1
! PROR
   31 CALL PREFOR(4)
      GOTO 1
! TTHM
   32 CALL TTHMLX(4)
      GOTO 1
! SPHA:
   33 CALL LPSCAL(4)
      GOTO 1
! COPY UNCHANGED CARD:
    2 WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
 2000 FORMAT (80A1)
      GOTO 1
  100 CLOSE (NEWIN)
 2005 FORMAT ('**')

      END SUBROUTINE NWINPR
!
!*****************************************************************************
!
      SUBROUTINE P0TEMP(L)
!
! *** P0TEMP by JCM 23 Jan 92 ***
!
!H Sets or unsets phase 0 for L cards temporarily
!
      LOGICAL L
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /PHAS0 / INRLP0, ICDLP0, INRLP1, ICDLP1, NCDF0

      IF (L) THEN
        INRLP1 = INREA(12,1)
        ICDLP1 = ICDN(12,1)
        INREAD(12) = INRLP0
        ICDNO(12) = ICDLP0
      ELSE
        INREAD(12) = INRLP1
        ICDNO(12) = ICDLP1
      ENDIF

      END SUBROUTINE P0TEMP
!
!*****************************************************************************
!
      SUBROUTINE PARSPR(MAGPAR)
!
! *** PARSPR updated by PJB and JBF Mar 93 ***
!
!H Collect all parameter fix and vary and relate info for 1 phase PR LSQ
!A On entry MAGPAR is replaced by the name of the subroutine to deal with
!A species 13,14,15 of family 2.  If non-magnetic, these should not occur,
!A so MAGPAR=DUMMY.  For magnetic, MAGPAR=DOMAG (or MAGCNL from PARSPR).
!P On entry JPHASE and JSOURC should hold phase and source.
!P Type of refinement, RIET, CAIL, SAPS, APES 1, must be set in /REFIPR
!
      EXTERNAL MAGPAR
      DIMENSION ISPVEC(10)

      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

! ABSORB EXISTING CONSTRAINTS ON CELL PARAMETERS DUE TO SYMMETRY:
      CALL CELREL(1,1,2)
! ABSORB EXISTING CONSTRAINTS ON STRUCTURE PARAMETERS DUE TO SYMMETRY:
      IF (RIET .OR. SAPS) THEN
! SPECIES OF X:
        ISPVEC(1) = 1
! SPECIES OF B11:
        ISPVEC(2) = 4
! SPECIES OF SCAT:
        ISPVEC(3) = 10
        CALL F2RELA(2,ISPVEC)
      ELSE
! FIX ALL FAMILY 2 IF CAIL:
        CALL ADDFX5(2,0,0,JPHASE,1,5)
      ENDIF
! DEAL WITH ABSENCE OF L TFAC CARD:
      IFAM = 1
      IGEN = 1
      ISPC = 1
      CALL LLTFAC(6)
! DEAL WITH ABSENCE OF L ABSC CARD:
      DO JSOUR = 1, NSOURC
        JSOURC = JSOUR
        CALL ABCRPR(6)
      ENDDO
! DEAL WITH ABSENCE OF L EXTN CARD:
      CALL EXCRPR(6)
! DEAL WITH ABSENCE OF L PROR CARD:
      CALL PREFOR(6)
! IF XRAY, DEAL WITH ABSENCE OF 'L TTHM' CARD:
      IF (LX) CALL TTHMLX(6)
! DEAL WITH ALL MAGNETIC CONSTRAINTS IF APPROPRIATE:
      CALL MAGPAR(5)
      JSOURC = 1
! IF MULTIPHASE, THERE IS A PHASE 0 WHICH MUST ALSO BE SCANNED:
      IF (MULFAS .AND. JPHASE.EQ.1) THEN
        CALL P0TEMP(.TRUE.)
! READ ALL 'L RELA' CARDS:
        CALL RDRELA
! READ ALL 'L FUDG' CARDS:
        CALL FUDGIN
! READ ALL 'L FIX' AND 'L VARY' CARDS:
        CALL RDFV
        CALL P0TEMP(.FALSE.)
      ENDIF
! READ ALL 'L RELA' CARDS:
      CALL RDRELA
! READ ALL 'L FUDG' CARDS:
      CALL FUDGIN
! READ ALL 'L FIX' AND 'L VARY' CARDS:
      CALL RDFV

      END SUBROUTINE PARSPR
!
!*****************************************************************************
!
      SUBROUTINE PAWLS(ALSQ,MATSZ,NSLTYP)
!
! *** PAWLS updated by JCM 10 Mar 89 ***
!
!C 19B
!H Application of Pawley-type slack constraints
!H Called from MAIN program to add to the LSQ matrix once per cycle
!
!A ALSQ and MATSZ are handed all through LSQ programs in this fashion
!A      - they are needed here for the call of MATTOT
!A NSLTYP=which type of constraint - only 3 at present:
!
!P On entry, /SLAKDA/ should contain NSLAK(NSLTYP)=actual number of slack
!P constraints of this type present.
!P On entry, /PAWLPR/ holds the paramters specs for these constraint
!P in ISPSLK with weights in WTSLAK.
!
!D For each slack constraint in turn, proceeds exactly as though these are
!D      conventional observations and calculated functions;  makes basic
!D      variable derivatives, gets weights, and adds totals in to LSQ matrix.
!
      INCLUDE 'PARAMS.INC'

      LOGICAL PRNCYC
      DIMENSION ALSQ(MATSZ)
      COMMON /DERVAR/ DERIVV(500), LVARV
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP, NSKTOT, KOM24

! OUT IF SIMULATION CYCLE:
      IF (SIMUL) GOTO 100
! OUT IF NOT TYPE 3
      IF (NSLTYP.NE.3) GOTO 100
! OUT IF NO SLACK CONSTRAINTS:
      IF (NSLAK(NSLTYP).EQ.0) GOTO 100
! HEADING FOR PRINTING IF REQUESTED:
      IF (PRNCYC(8)) THEN
        CALL MESS(LPT,1,'  Slack Constraints')
        CALL MESS(LPT,1,'  No.   Obs      Calc      Diff      Weight')
      ENDIF
! FOR NOW ONLY TYPE 3,  PAWLEY SLACK CONSTRAINTS FROM CAIL OR SAPS:
! COUNT ALL SLACK CONSTRAINTS:
      DO ISK = 1, NSLAK(NSLTYP)
! CLEAR WHOLE DERIVATIVE VECTOR - ONLY A FEW ITEMS WILL BE FILLED BY
! ANY PARTICULAR CONSTRAINT:
!** SO POSSIBLY WE WANT TO ADD 3 ITEMS TO THE LSQ MATRIX AND 1 TO THE RHS
!** DIRECTLY, RATHER THAN PRETEND THAT THESE ARE OBSERVATIONS ***
!
! ^^^^^ THE FOLLOWING SECTION NEEDS ATTENTION
! ^^^^^ WE COULD ALSO DO YCALC = F1/F2 -A1/A2 I.E. CONSTRAIN THE SIG/GAM RATIO
        DO LV = 1, LVARV
          DERIVV(LV) = 0.
        ENDDO
        IG = IGSLAK(ISK)
        YCALC = AMSLAK(IG,ISK)*F4PAR(IG,ISPSLK(1,ISK)) - AMSLAK(2,ISK)*F4PAR(IG,ISPSLK(2,ISK))
        DERIVV(KF4PAR(IG,ISPSLK(1,ISK))) = AMSLAK(1,ISK)
        DERIVV(KF4PAR(IG,ISPSLK(2,ISK))) = -AMSLAK(2,ISK)
        CALL RELATE
        OBS = YCALC
        DIFF = 0.
! WEIGHT
! FOR NOW, EXTRA WEIGHT IS UNITY:
        SLAKWT(NSLTYP) = 1.
        WT = WTSLAK(ISK)*SLAKWT(NSLTYP)*SLAKWT(NSLTYP)
        SQRTWT = SQRT(WT)
        WDIFF = SQRTWT*DIFF
        IF (PRNCYC(8)) WRITE (LPT,2001) ISK, OBS, YCALC, DIFF, WTSLAK(ISK)
 2001   FORMAT (1X,I4,F10.4,F10.4,F10.5,G12.4)
        CALL MATTOT(ALSQ,MATSZ)
! ADD IN  SLACK CONSTRAINT STATISTICS:
        ISLKTP = NSLTYP
        CALL RFACS(4)
      ENDDO
! PRINT SLACK CONSTRAINT STATISTICS:
      CALL RFACS(5)
  100 RETURN

      END SUBROUTINE PAWLS
!
!*****************************************************************************
!
      SUBROUTINE PCXX(N)
!
! **** PCXX by JCM 25 Apr 90 ***
!
!H An explicit routine to cater for multisource peak centres
!

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

      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

! AT PRESENT, EACH PEAK CENTRE TYPE IS 1:
      IF (TOF) CALL PCTF01(N)
      IF (CN .OR. SR .OR. LX) CALL PCCN01(N)
!	NEXT LINE 'IF (LX) CALL PCLX01(N)' INTENDED FOR
!	MULTIPLE LAMBDA STUFF.  COMMENTED OUT FOR THE MO, JUNE99
!      IF (LX) CALL PCLX01(N)
      RETURN
      ENTRY PCXX8(NP,NV)
! SET PEAK CENTRE PARAMETER NP IS VARIABLE NV:
      KPCNSP(NP,JPHASE,JSOURC) = NV
      RETURN
      ENTRY PCXX9
! SET ALL PEAK CENTRE PARAMETERS FIXED:
      DO JP = 1, NPHASE
        DO JS = 1, NSOURC
          DO I = 1, NPKCSP(JP,JS)
            KPCNSP(I,JP,JS) = 0
          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE PCXX
!
!*****************************************************************************
!
      SUBROUTINE PFALL(N)
!
! *** PFALL updated by JCM 24 Jan 91 ***
!
!H Multiple entry routine to deal with aspects of L PKFN cards common
!H to all peak function types
!A On entry N=1,3 or 4 (as a relic from PFXX entries)
!A     N=1 means interpret the L PKFN card in ICARD
!A     N=3 means apply a shift to a parameter of genus IGEN, species ISPC
!A     N=4 means write out to unit NEWIN a new L PKFN card
!P For N=1 or 4:
!P The card has already been read into /SCRACH/
!P The array PWD holds the allowed vocabulary
!P NPKGEN(JPHASE,JSOURC) holds the total number of words which are names
!P for family 3 genera, and also words to be found after L PKFN.
!D For N=1:
!D Given a card in /SCRACH/ which starts L PKFN, read the remaining  information
!D For N=3:
!D Applies shift (in SHIFT in /NEWOLD/) to peak function parameter in
!D PKFNSP, using genus and species from /NEWOLD/
!D For N=4:
!D Given a card in /SCRACH/ which starts L PKFN, reads the next word to
!D decide the type of card, then writes a new card to unit NEWIN.
!N Used to be entries 1,3,4 of PFXX
!
!
      INCLUDE 'PARAMS.INC'

      CHARACTER*4 WORD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

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

      COMMON /PWORDS/ PWD(10,9,5)
      CHARACTER*4 PWD
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DIMENSION ATEMP(6)

      GOTO (1,100,3,4), N
! READ NEXT WORD ON CARD (ASSUMES THAT "L PKFN" HAS NO EXTRA SPACES):
    1 CALL RDWORD(WORD,IWDLEN,8,IPT,80,0,IER)
      IF (WORD.EQ.'TYPE') RETURN
      IF (WORD.EQ.'LIMS') THEN
        CALL RDREAL(TOLR(1,JSOURC),IPT,IPT,80,IER)
        NO = 1
        DO I = 1, NO
          IF (TOLR(I,JSOURC).LE.1.) THEN
            WRITE (LPT,2008) TOLR(I,JSOURC)
 2008       FORMAT (/' Peak limits at ',F6.3,' of peak height')
          ELSE
            WRITE (LPT,2009) TOLR(I,JSOURC)
 2009       FORMAT (/' Peak limits at ',F6.1,' full widths at half maximum')
          ENDIF
        ENDDO
        RETURN
      ENDIF
      IF (WORD.EQ.'NFFT') THEN
        CALL RDINTG(NFFT,IPT,IPT,80,IER)
        WRITE (LPT,2007) NFFT
 2007   FORMAT (/' Number of fast Fourier transforms =',I2)
        RETURN
      ENDIF
      L = NCFIND(WORD,PWD(1,JPHASE,JSOURC),NPKGEN(JPHASE,JSOURC))
!        PKFNSP(L,I,JPHASE,JSOURC)
! L = 1 : 'SIGM', I = 1, 2
! L = 2 : 'GAMM', I = 1, 2
! L = 3 : 'HPSL', I = 1
! L = 4 : 'HMSL', I = 1
! JPHASE = 1, JSOURC = 1
      IF (L.LE.0) THEN
        CALL ERRCH2(WORD,2,'word','after PKFN not recognised')
        RETURN
      ENDIF
! READ ALL REMAINING NUMBERS ON CARD - SETS NUM TO BE HOW MANY:
      CALL RDNUMS(ATEMP,IPT,7,NUM,IER)
      DO I = 1, NUM
        PKFNSP(L,I,JPHASE,JSOURC) = ATEMP(I)
      ENDDO
      NPKFSP(L,JPHASE,JSOURC) = NUM
! PRINT VALUES:
      WRITE (LPT,2000) WORD, NUM, (ATEMP(I),I=1,NUM)
 2000 FORMAT (/' Peak Descriptor ',A4,' - ',I3,' parameters:',6F10.4)
      RETURN
! APPLY SHIFT:
    3 CALL ADJUST(PKFNSP(IGEN-2,ISPC,JPHASE,JSOURC))
      GOTO 100
! WRITE NEW L PKFN CARD TO UNIT NEWIN, GIVEN OLD CARD IN /SCRACH:
    4 CALL RDWORD(WORD,IWDLEN,8,IPT,80,0,IER)
! PWD is an array of type CHARACTER holding those parameters of the peak shape
! that are variable, i.e. SIGM, GAMM, HPSL, HMSL.
! The type of peak shape used is not a variable, and is written out as it was read in
      L = NCFIND(WORD,PWD(1,JPHASE,JSOURC),NPKGEN(JPHASE,JSOURC))
      IF (L.EQ.0) THEN
        WRITE (NEWIN,2002) (ICARD(I:I),I=1,LENGT(ICARD))
 2002   FORMAT (80A1)
      ELSE
! When we are here, we are writing out Gamma, sigma, HPSL or HMSL.
! Gamma and sigma each have two components.
        NUM = NPKFSP(L,JPHASE,JSOURC)
        WRITE (NEWIN,2001) WORD, (PKFNSP(L,I,JPHASE,JSOURC),I=1,NUM)
!** NEEDS *S IF MULTISOURCE
 2001   FORMAT ('L PKFN ',A4,6F10.4)
      ENDIF
      RETURN
      ENTRY PFALL8(NG,NS,NV)
! SET PARAMETER OF PEAK FUNCTION TO BE VARIABLE NV:
      KPFNSP(NG-2,NS,JPHASE,JSOURC) = NV
      RETURN
      ENTRY PFALL9
! SET ALL PARAMETERS OF PEAK FUNCTION TO BE FIXED:
      DO JP = 1, NPHASE
        DO JS = 1, NSOURC
          DO I = 1, NPKGEN(JP,JS)
            DO J = 1, NPKFSP(I,JP,JS)
              KPFNSP(I,J,JP,JS) = 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
  100 RETURN

      END SUBROUTINE PFALL
!
!*****************************************************************************
!
      SUBROUTINE PFXX(N)
!
! **** PFXX updated by JCM 23 Feb 93 ***
!
!H An explicit routine to cater for multisource peak functions
!

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      IF (TOF) THEN
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.1) CALL PFTF01(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.2) CALL PFTF02(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.3) CALL PFTF03(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.4) CALL PFTF04(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.5) CALL PFTF05(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.8) CALL PFTF08(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.92) CALL PFTF92(N)
      ELSEIF (CN) THEN
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.1) CALL PFCN01(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.2) CALL PFCN03(N)
     !   IF (NPFSOU(JPHASE,JSOURC).EQ.3) CALL PFCN03(N)
      ELSEIF (LX) THEN
        IF (NPFSOU(JPHASE,JSOURC).EQ.1) CALL PFCN01(N)
        IF (NPFSOU(JPHASE,JSOURC).EQ.2) CALL PFCN03(N)
        IF (NPFSOU(JPHASE,JSOURC).EQ.3) CALL PFCN03(N)
      ELSEIF (SR) THEN
        IF (NPFSOU(JPHASE,JSOURC).EQ.1) CALL PFCN01(N)
        IF (NPFSOU(JPHASE,JSOURC).EQ.2) CALL PFCN03(N)
        IF (NPFSOU(JPHASE,JSOURC).EQ.3) CALL PFCN03(N)
      ENDIF

      END SUBROUTINE PFXX
!
!*****************************************************************************
!
      SUBROUTINE PHCINI(KOMM,N)
!
! *** PHCINI BY JCM 6 JUL 87 ***
!
!H Initialises COMMON equivalanced to KOMM by finding its end in N
!
!A On entry KOMM is the array equivalenced to the required COMMON
!A On exit  N is set to point to the last element of KOMM
!
      DIMENSION KOMM(1)

      I = 0
    2 N = -42
    1 I = I + 1
      IF (KOMM(I).NE.-42) GOTO 1
      N = I
      IF (KOMM(I).NE.I) GOTO 2

      END SUBROUTINE PHCINI
!
!*****************************************************************************
!
      SUBROUTINE PHFIND(KOMM,N,ISCR)
!
! *** PHFIND BY JCM 6 JUL 87 **
!
!H Read COMMON block whose contents are equivalenced to KOMM from unit ISCR
!
!A KOMM is an array which has been equivalenced to the entire required
!A   COMMON block.
!A N is the actual number of numbers to transfer
!A ISCR is the number of the unit from which to read KOMM
!A      assumed positioned and reading sequentially
!P A call of PHLOSE must have put the COMMON on to unit ISCR first.
!
      DIMENSION KOMM(N)

      READ (ISCR) KOMM

      END SUBROUTINE PHFIND
!
!*****************************************************************************
!
      SUBROUTINE PHLOSE(KOMM,N,ISCR)
!
! *** PHLOSE BY JCM 6 JUL 87 **
!
!H Write COMMON block whose contents are equivalenced to KOMM to unit ISCR
!
!A KOMM is an array which has been equivalenced to the entire required
!A   COMMON block.
!A N is the actual number of numbers to transfer
!A ISCR is the number of the unit to which to write KOMM
!
      DIMENSION KOMM(N)

      WRITE (ISCR) KOMM

      END SUBROUTINE PHLOSE
!
!*****************************************************************************
!
      SUBROUTINE PHMOVE(IO,N)

      USE REFVAR
!
! *** PHMOVE BY JCM 7 FEB 88 ***
!
!H Initialise, write out or read back whole/part phase number N
!A On entry IO =0 for initialise phase moving
!A               -2 for write out phase N, just after reading it from
!A                  the crystal data file, and adjust NATOM etc
!A               -1 for write out phase N
!A                1 for read back phase N, readjust NATOM etc, and set IPHASE=N
!A           N is the required phase number.
!
!
      INCLUDE 'PARAMS.INC'

      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      DIMENSION KOMM1(1)
      EQUIVALENCE (KOMM1(1),ATF(1,1))
      COMMON /ANSCAT/ NAMODE(20), FDASH(20), KOM2
      COMPLEX FDASH
      DIMENSION KOMM2(1)
      EQUIVALENCE (KOMM2(1),NAMODE(1))
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3
      DIMENSION KOMM3(1)
      EQUIVALENCE (KOMM3(1),IPTCEL(1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      DIMENSION KOMM4(1)
      EQUIVALENCE (KOMM4(1),CELL(1,1,1))
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      DIMENSION KOMM7(1)
      EQUIVALENCE (KOMM7(1),NFORMF(1))
      COMMON /FORMD2/ NBKF(20,9), NMFNM(9)
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      DIMENSION KOMM8(1)
      EQUIVALENCE (KOMM8(1),FRIEDL)
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      DIMENSION KOMM10(1)
      EQUIVALENCE (KOMM10(1),NASYM)
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      DIMENSION KOMM6(1)
      EQUIVALENCE (KOMM6(1),NGEN4(1,1))
      COMMON /GUNIT / MARK(3,2), BSYM(3,3), IBOX, KOM11
      DIMENSION KOMM11(1)
      EQUIVALENCE (KOMM11(1),MARK(1,1))
      COMMON /HKLGEN/ STEP(3,3), PT(3,3), VECEND(3,3), PRPT(3,3),       &
     &                NPRIM(2,2), NP, LFAC(2), MCOUNT(2), KOM5
      DIMENSION KOMM5(1)
      EQUIVALENCE (KOMM5(1),STEP(1,1))
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      DIMENSION KOMM18(1)
      EQUIVALENCE (KOMM18(1),ILREA(1,1))
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
      DIMENSION KOMM19(1)
      EQUIVALENCE (KOMM19(1),NMAG)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      DIMENSION KOMM13(1)
      EQUIVALENCE (KOMM13(1),NOP)
      COMMON /NTITL / NTITLE, KOM14
      DIMENSION KOMM14(1)
      EQUIVALENCE (KOMM14(1),NTITLE)
      COMMON /OMITPR/ MIS, AMISS(3,100), KOM12
      DIMENSION KOMM12(1)
      EQUIVALENCE (KOMM12(1),MIS)
      COMMON /OVER  / ITFAC, OTFAC(10), KOTFAC(10), NTFAC, JTFAC, KOM15
      EQUIVALENCE (TFAC,OTFAC(1))
      EQUIVALENCE (KTFAC,KOTFAC(1))
      DIMENSION KOMM15(1)
      EQUIVALENCE (KOMM15(1),ITFAC)
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      DIMENSION KOMM16(1)
      EQUIVALENCE (KOMM16(1),AKLO)
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
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      DIMENSION KOMM17(1)
      EQUIVALENCE (KOMM17(1),NATOM)
      COMMON /POSNS2/ NATO(9)
!O      INCLUDE 'REFLNS.INC'
!O      DIMENSION KOMM23(1)
!O      EQUIVALENCE (KOMM23(1),rHKL(1,1))
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      DIMENSION KOMM21(1)
      EQUIVALENCE (KOMM21(1),PROP(1))
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      DIMENSION KOMM24(1)
      EQUIVALENCE (KOMM24(1),NSLAK(1))
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
      DIMENSION KOMM25(1)
      EQUIVALENCE (KOMM25(1),NSTYP)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      DIMENSION KOMM26(1)
      EQUIVALENCE (KOMM26(1),SYM(1,1,1))
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
     &                KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      DIMENSION KOMM20(1)
      EQUIVALENCE (KOMM20(1),MTSYM(1))
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
      DIMENSION KOMM22(1)
      EQUIVALENCE (KOMM22(1),MULTAB(1,1))

      COMMON /FONAM / FONA(20,9), FONAME(20)
      CHARACTER*4 FONAME, FONA
      COMMON /TITLE / ITITLE
      CHARACTER*80 ITITLE

! IF NOT ACTUALLY MULTIPHASE, EXIT:
      IF (.NOT.MULFAS) GOTO 100
! BRANCH ON WRITE, INITIALISE OR READ:
      IF (IO.EQ.0) THEN
! INITIALISE:
! ALL NUMBER COMMON:
        CALL PHCINI(KOMM1,KOM1)
        CALL PHCINI(KOMM2,KOM2)
        CALL PHCINI(KOMM3,KOM3)
        CALL PHCINI(KOMM4,KOM4)
        CALL PHCINI(KOMM5,KOM5)
        CALL PHCINI(KOMM6,KOM6)
        CALL PHCINI(KOMM7,KOM7)
        CALL PHCINI(KOMM8,KOM8)
        CALL PHCINI(KOMM10,KOM10)
        CALL PHCINI(KOMM11,KOM11)
        CALL PHCINI(KOMM12,KOM12)
        CALL PHCINI(KOMM13,KOM13)
        CALL PHCINI(KOMM14,KOM14)
        CALL PHCINI(KOMM15,KOM15)
        CALL PHCINI(KOMM16,KOM16)
        CALL PHCINI(KOMM17,KOM17)
        CALL PHCINI(KOMM18,KOM18)
        CALL PHCINI(KOMM19,KOM19)
        CALL PHCINI(KOMM20,KOM20)
        CALL PHCINI(KOMM21,KOM21)
        CALL PHCINI(KOMM22,KOM22)
        CALL PHCINI(KOMM23,KOM23)
        CALL PHCINI(KOMM24,KOM24)
        CALL PHCINI(KOMM25,KOM25)
        CALL PHCINI(KOMM26,KOM26)
!   ALL CHARACTER COMMON IS MOVED AROUND BY EXPLICIT NAME.
! SET UP THE UNITS ON WHICH TO DUMP EACH PHASE:
        DO I = 1, NPHASE
          NPHUNI(I) = NOPFIL(1005)
        ENDDO
        GOTO 100
      ENDIF
! READ PHASE N OR WRITE PHASE N:
! SET ISCR = UNIT NUMBER FOR PHASE - SEQUENTIAL, UNFORMATTED
      ISCR = NPHUNI(N)
! BRANCH ON READ/WRITE:
      IF (IO.LT.0) GOTO 2
! IF ALREADY THERE, EXIT:
      IF (N.EQ.IPHASE) GOTO 100
! READ IN ALL COMMONS IN SEQUENCE:
! FULL PHASE SWOP:
      READ (ISCR) ITITLE
      CALL PHFIND(KOMM1,KOM1,ISCR)
      CALL PHFIND(KOMM2,KOM2,ISCR)
      CALL PHFIND(KOMM3,KOM3,ISCR)
      CALL PHFIND(KOMM4,KOM4,ISCR)
      CALL PHFIND(KOMM5,KOM5,ISCR)
      CALL PHFIND(KOMM6,KOM6,ISCR)
      CALL PHFIND(KOMM7,KOM7,ISCR)
      CALL PHFIND(KOMM8,KOM8,ISCR)
      CALL PHFIND(KOMM10,KOM10,ISCR)
      CALL PHFIND(KOMM11,KOM11,ISCR)
      CALL PHFIND(KOMM12,KOM12,ISCR)
      CALL PHFIND(KOMM13,KOM13,ISCR)
      CALL PHFIND(KOMM14,KOM14,ISCR)
      CALL PHFIND(KOMM15,KOM15,ISCR)
      CALL PHFIND(KOMM16,KOM16,ISCR)
      CALL PHFIND(KOMM17,KOM17,ISCR)
      CALL PHFIND(KOMM18,KOM18,ISCR)
      CALL PHFIND(KOMM19,KOM19,ISCR)
      CALL PHFIND(KOMM20,KOM20,ISCR)
      CALL PHFIND(KOMM21,KOM21,ISCR)
      CALL PHFIND(KOMM22,KOM22,ISCR)
      CALL PHFIND(KOMM23,KOM23,ISCR)
      CALL PHFIND(KOMM24,KOM24,ISCR)
      CALL PHFIND(KOMM25,KOM25,ISCR)
      CALL PHFIND(KOMM26,KOM26,ISCR)
! A NEW BIT WHICH I HOPE IS RIGHT - SET UP SINGLE PHASE ITEMS:
      NATOM = NATO(N)
      DO I = 1, NATOM
        ATNAME(I) = ATNA(I,N)
      ENDDO
      NUMFNM = NMFNM(N)
      DO I = 1, NUMFNM
        FONAME(I) = FONA(I,N)
        NBAKF(I) = NBKF(I,N)
      ENDDO
! AND DO MAGNETIC PHASES (JBF 2-8-94)
      MAG = PHMAG(N)
      FIXED = (.NOT.MAG .OR. IPROP.LE.0)
! OK FOR THE MOMENT, BUT WE COULD SWAP LESS COMMON IF .NOT. MAG
      CALL LOGPHA(N)
      IPHASE = N
      GOTO 101
! WRITE PHASE N:
    2 WRITE (ISCR) ITITLE
      CALL PHLOSE(KOMM1,KOM1,ISCR)
      CALL PHLOSE(KOMM2,KOM2,ISCR)
      CALL PHLOSE(KOMM3,KOM3,ISCR)
      CALL PHLOSE(KOMM4,KOM4,ISCR)
      CALL PHLOSE(KOMM5,KOM5,ISCR)
      CALL PHLOSE(KOMM6,KOM6,ISCR)
      CALL PHLOSE(KOMM7,KOM7,ISCR)
      CALL PHLOSE(KOMM8,KOM8,ISCR)
      CALL PHLOSE(KOMM10,KOM10,ISCR)
      CALL PHLOSE(KOMM11,KOM11,ISCR)
      CALL PHLOSE(KOMM12,KOM12,ISCR)
      CALL PHLOSE(KOMM13,KOM13,ISCR)
      CALL PHLOSE(KOMM14,KOM14,ISCR)
      CALL PHLOSE(KOMM15,KOM15,ISCR)
      CALL PHLOSE(KOMM16,KOM16,ISCR)
      CALL PHLOSE(KOMM17,KOM17,ISCR)
      CALL PHLOSE(KOMM18,KOM18,ISCR)
      CALL PHLOSE(KOMM19,KOM19,ISCR)
      CALL PHLOSE(KOMM20,KOM20,ISCR)
      CALL PHLOSE(KOMM21,KOM21,ISCR)
      CALL PHLOSE(KOMM22,KOM22,ISCR)
      CALL PHLOSE(KOMM23,KOM23,ISCR)
      CALL PHLOSE(KOMM24,KOM24,ISCR)
      CALL PHLOSE(KOMM25,KOM25,ISCR)
      CALL PHLOSE(KOMM26,KOM26,ISCR)
      IF (IO.EQ.-2) THEN
! INITIAL ENTRY - PUT IN PLACE UNCHANGING VECTORS WHICH ARE PHASE DEPENDENT:
        NATO(N) = NATOM
        DO I = 1, NATOM
          ATNA(I,N) = ATNAME(I)
        ENDDO
        NMFNM(N) = NUMFNM
        DO I = 1, NUMFNM
          FONA(I,N) = FONAME(I)
          NBKF(I,N) = NBAKF(I)
        ENDDO
      ENDIF
  101 REWIND ISCR
  100 RETURN

      END SUBROUTINE PHMOVE
!
!*****************************************************************************
!
      SUBROUTINE PREFOR(N)

      USE REFVAR
!
! *** PREFOR by WIFD 10 Jun 1987 ***
!
! MULTIPLE ENTRY ROUTINE TO DEAL WITH ALL ASPECTS OF THE PREFERRED
! ORIENTATION CORRECTION
! FOR SINGLE FRAME TIME OF FLIGHT LSQ ("TOF").
!
      INCLUDE 'PARAMS.INC'
      
      DIMENSION REFHT(3,48), PHASES(48)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /PREORI/ NPRTYP, PRFDIR(3), PRFLEN, PRFPAR, KPRFPR, PRFCOR,&
     &                DERPRQ
! JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
!
      GOTO (1,2,3,4,5,6), N
! HAVE IN COMM0N /SCRACH/ A CARD STARTING 'L PROR' - READ REST:
    1 CALL RDINTG(NPRTYP,7,IPT,80,IER)
      IF (NPRTYP.EQ.0) THEN
        CALL MESS(LPT,1,'No preferred orientation')
        GOTO 50
      ENDIF
      CALL RDREAL(PRFPAR,IPT,IPT,80,IER)
      WRITE (LPT,2000) PRFPAR
 2000 FORMAT (/' Preferred orientation coefficient=',F12.4)
! READ ALL REMAINING NUMBERS ON CARD - SETS NUM TO BE HOW MANY:
      CALL GMZER(PRFDIR,1,3)
      CALL RDNUMS(PRFDIR,IPT,3,NUM,IER)
      WRITE (LPT,2010) (PRFDIR(I),I=1,3)
 2010 FORMAT (' Preferred orientation direction=',3F4.0)
      PRFLEN = VCTMOD(1.,PRFDIR,2)
      GOTO 100
! CALCULATE FUNCTION WHICH WILL BE PART OF P1 IN CALPR
! AND ITS DERIVATIVE
    2 IF (NPRTYP.EQ.0) GOTO 100
      CALL SYMREF(rHKL(1,KNOW),REFHT,IREFT,PHASES)
      PRFCOR = 0.
      DERPRQ = 0.
      REFLEN = VCTMOD(1.,rHKL(1,KNOW),2)
      ATEM = 1./(REFLEN*PRFLEN)
      DO IR = 1, IREFT
        SPRD = SCLPRD(PRFDIR,REFHT(1,IR),2)
        CSQA = (ATEM*SPRD)**2
        SSQA = 1. - CSQA
        BTEM = CSQA*PRFPAR**2 + SSQA/PRFPAR
        BTEM = 1./SQRT(BTEM)
        PRFCOR = PRFCOR + BTEM**3
        DERPRQ = DERPRQ - 1.5*BTEM**5*(2.*PRFPAR*CSQA-SSQA/PRFPAR**2)
      ENDDO
      DERPRQ = DERPRQ/PRFCOR
      PRFCOR = PRFCOR/FLOAT(IREFT)
!** SHOULD DERPRQ BE TIMES THIS ALSO?
      GOTO 100
! APPLY SHIFT IN COEFFICIENT:
    3 CALL ADJUST(PRFPAR)
      GOTO 100
! WRITE OUT NEW 'L PROR' CARD:
    4 WRITE (NEWIN,2001) NPRTYP, PRFPAR, (PRFDIR(I),I=1,3)
 2001 FORMAT ('L PROR',I5,F10.4,3F4.0)
      GOTO 100
! DEAL WITH ABSENCE OF 'L PROR' CARD:
    5 CALL MESS(LPT,1,'No L PROR card - assuming no preferred orientation correction')
      NPRTYP = 0
   50 PRFPAR = 1.
      PRFCOR = 1.
      DERPRQ = 0.
      PRFDIR(1) = 0.
      PRFDIR(2) = 0.
      PRFDIR(3) = 1.
      GOTO 100
! FIX PROR COR IF NO CARD WAS GIVEN, OR TYPE 0 READ:
    6 IF (NPRTYP.EQ.0) CALL ADDFX5(1,1,9,1,1,4)
      GOTO 100
      ENTRY PREFO8(NV)
! RECORD THAT THE PREFERRED ORIENTATION PARAMETER IS VARIABLE NUMBER NV:
      KPRFPR = NV
      GOTO 100
      ENTRY PREFO9
! RECORD THAT THE PREFERRED ORIENTATION PARAMETER IS FIXED:
      KPRFPR = 0
      GOTO 100
  100 RETURN

      END SUBROUTINE PREFOR
!
!*****************************************************************************
!
      SUBROUTINE QPRIN(ARGI,OBS,DOBS,ICODE,ENDIP)
!
! *** QPRIN DUMMY by JCM 10 May 88 ***
!
!H User to replace this by his own routine to read observation, if needed
!
      LOGICAL ENDIP
! Just fooling the compiler to stop it from generating warnings
     
      ENDIP = .TRUE. ! end of input
      ARGI  = 0.0
      OBS   = 0.0
      DOBS  = 0.0
      ICODE = 1

      END SUBROUTINE QPRIN
!
!*****************************************************************************
!
      SUBROUTINE REFSET
!
! *** REFSET updated by JBF July 95 ***
!
!X
!C 19A
!H Finds L REFI and L SORC cards and deduces which refinement required
!H Also L *Sn PKCN and L *Sn PKFN cards.
!D Entered once at the start of a multi-job.  Constructs what were for
!D non-multi-jobs the program name, the peak centre subroutine name, and
!D the peak function name.
!D
!D Reads the first cdf and finds L SORC which should be followed by a
!D string of data sources like TF, CN, LX, ED - one per source;  keeps these
!D in NDASOU.
!D Read every cdf - one per phase - and picks up information:
!D       from L REFI which type of refinement (RIET, CAIL, . .) for this phase
!D       from L *Sn PKCN TYPE <number> which peak centre approximation number
!D                  for this phase and source n,
!*CD Altered to expect only one set of L *Sn PKCN cards, therefore on phase 0
!D       from L *Sn PKFN TYPE <number> which peak function approximation number
!D                  for this phase and source n.
!D
!D If used with an old-style L REFI card, expects a signed packed integer.
!O Prints out what it has read. NB No longer sets LOGICAL TIC.
!
      CHARACTER*4 SWORDS(5), WORD
      CHARACTER*27 SORCWD(5), TYPEWD(6)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /WHEN  / TIM(2), MAIN
      CHARACTER*5 TIM
      CHARACTER*6 MAIN
      DATA SWORDS/'NTOF', 'NCON', 'LABX', 'SYNX', 'EDIS'/
      DATA SORCWD/'Time of flight neutron',                             &
     &     'Constant wavelength neutron', 'Laboratory X-ray',           &
     &     'Synchrotron X-Ray', 'Energy Dispersive'/
      DATA TYPEWD/'RIET', 'PAWL', 'SAPS', 'APES', 'RAPS', 'PEWS'/

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      CALL WRLINE(1,60,'-',1)
      DO JPHASE = 1, NPHASE
! ARRANGE TO ADDRESS THE CORRECT CRYSTAL DATA FILE:
        IF (JPHASE.NE.1) THEN
          CALL VCSWOP(INREA(1,JPHASE),INREA(1,1),26)
          CALL VCSWOP(ICDN(1,JPHASE),ICDN(1,1),26)
        ELSE
          IF (MULFAS) CALL P0TEMP(.TRUE.)
          CALL FINDCD('L','SORC',4,0,L)
          IF (L.EQ.0) THEN
            NSOURC = 1
!.. set to SYNX
            NDASOU(1) = 4
            CALL MESS(LPT,1,'Default source of data       : '// SORCWD(NDASOU(1)))
            GOTO 4
          ENDIF
! READ ALL SOURCES FROM THE ONE CARD ON CDF FOR FIRST PHASE:
          NSOURC = 0
          IPT = 7
    3     CALL RDWORD(WORD,LEN,IPT,IPT,80,0,IER)
          IF (IER.EQ.100) GOTO 4
          CALL ERRCHK(2,NSOURC,5,0,'PR data sources')
          IF (IBMBER .NE. 0) RETURN
          NDASOU(NSOURC) = NCFIND(WORD,SWORDS,5)
          IF (NDASOU(NSOURC).EQ.0) THEN
            I = 2
            CALL ERRCH2(WORD,I,'data source word <','> not recognised')
!.. set to SYNX
            NTEM = 4
            NDASOU(NSOURC) = NTEM
            CALL MESS(LPT,1,'Default source of data       : '//SORCWD(NTEM))
          ELSE
            IF (NSOURC.EQ.1) CALL MESS(LPT,1,'Source of data       : '//SORCWD(NDASOU(1)))
            IF (NSOURC.GT.1) WRITE (LPT,2005) NSOURC, SORCWD(NDASOU(NSOURC))
 2005       FORMAT (' Source of data no.',I3,' : ',A27)
          ENDIF
          GOTO 3
    4     MULSOU = NSOURC.GT.1
          MULONE = MULFAS .OR. MULSOU
! READ "L PKCN" *Sn "TYPE" CARDS - THESE ARE IN PHASE 0 IF MULTI :
          K = 0
    6     CALL FINDCD('L','PKCN',4,K,L)
          IF (L.LE.0) GOTO 7
          K = L
          IPT = 7
          CALL RDWORD(WORD,LEN,IPT,IPT,80,1,IER)
          IF (WORD.EQ.'TYPE') THEN
            CALL RDINTG(NPCSOU(JPHASE,KSOURC),IPT,IPT,80,IER)
            DO IJ = 2, NPHASE
              NPCSOU(IJ,KSOURC) = NPCSOU(1,KSOURC)
            ENDDO
! ONE DAY CHECK COMPATIBILITY
          ENDIF
          GOTO 6
! CHECK ALL PEAK CENTRE TYPES SET:
    7     DO I = 1, NSOURC
            IF (NPCSOU(JPHASE,I).EQ.0) THEN
! Default peak centre function is 1
              NPCSOU(JPHASE,I) = 1
            ELSE
              IF (.NOT.MULONE) THEN
                WRITE (LPT,2011) NPCSOU(1,1)
              ELSE
                IF (.NOT.MULSOU) THEN
                  WRITE (LPT,2011) NPCSOU(JPHASE,1)
                ELSE
                  WRITE (LPT,2013) I, NPCSOU(JPHASE,I)
 2013             FORMAT (' For source',I3,' Peak centre type =',I3)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          IF (MULFAS .AND. JPHASE.EQ.1) CALL P0TEMP(.FALSE.)
        ENDIF
            ! above lines associated with JPHASE.eq.1
! FOR EACH PHASE, SHOULD BE AN L REFI CARD:
        CALL FINDCD('L','REFI',4,0,L)
        IF (L.EQ.0) THEN
          CALL MESS(LPT,1,'No L REFI card assuming Rietveld analysis')
          METHOD(JPHASE) = 1
        ELSE
          CALL RDWORD(WORD,LEN,7,IPT,80,0,IER)
          METHOD(JPHASE) = NCFIND(WORD,TYPEWD,6)
          IF (METHOD(JPHASE).EQ.0) THEN
            CALL ERRCH2(WORD,2,'refinement type word','not recognised - assuming Rietveld analysis')
            METHOD(JPHASE) = 1
          ELSE
            IF (MULFAS) THEN
              WRITE (LPT,2001) JPHASE, TYPEWD(METHOD(JPHASE))
 2001         FORMAT (/' Phase',I3,' : Refinement type ',A27)
            ELSE
              CALL MESS(LPT,1,'Refinement type '//TYPEWD(METHOD(JPHASE)))
            ENDIF
          ENDIF
        ENDIF
! READ "L PKFN" *Sn "TYPE" CARDS:
        K = 0
    8   CALL FINDCD('L','PKFN',4,K,L)
        IF (L.LE.0) GOTO 12
        K = L
        IPT = 7
        CALL RDWORD(WORD,LEN,IPT,IPT,80,1,IER)
        IF (WORD.NE.'TYPE') GOTO 8
        CALL RDINTG(NPFSOU(JPHASE,KSOURC),IPT,IPT,80,IER)
! CHECK COMPATIBILITY
        GOTO 8
! CHECK ALL PEAK FUNCTION TYPES SET:
   12   DO I = 1, NSOURC
          IF (NPFSOU(JPHASE,I).EQ.0) THEN
! Default is peak function is 3
            NPFSOU(JPHASE,I) = 3
          ELSE
            IF (.NOT.MULONE) THEN
              WRITE (LPT,2021) NPFSOU(1,1)
            ELSE
              IF (.NOT.MULSOU) THEN
                WRITE (LPT,2021) NPFSOU(JPHASE,1)
              ELSE
                WRITE (LPT,2023) I, NPFSOU(JPHASE,I)
 2023           FORMAT (' For source',I3,' Peak function type =',I3)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      CALL WRLINE(1,60,'-',1)
! PUT VECTORS BACK:
      DO JPHASE = NPHASE, 2, -1
        CALL VCSWOP(INREA(1,JPHASE),INREA(1,1),26)
        CALL VCSWOP(ICDN(1,JPHASE),ICDN(1,1),26)
      ENDDO
! FOR USES OTHER THAN MULTIPHASE LSQ (EG TIC) - HOPE THIS DOES NOT MESS UP THE
! REST:
      CALL LOGSOU(1)
      CALL LOGPHA(1)
      RETURN
 2011 FORMAT (' Peak centre type =',I3)
 2021 FORMAT (' Peak function type =',I3)

      END SUBROUTINE REFSET
!
!*****************************************************************************
!
      SUBROUTINE RFACPR(IRFAC,PCXX)

      USE REFVAR

!
! *** RFACPR updated by JBF 13-Jan-95 ***
!
!H Multiple entry routine to deal with Profile refinement R Factors
!A On entry IRAFC indicates the actions required:
!A   IRFAC=1  set up - clear totals
!A   IRFAC=2  add in contributions, given values, differences and weights
!A   IRFAC=3  just precedes entry 4
!A   IRFAC=4 if required, send to file H,K,L, F CALC, F OBS, DIFF
!A   IRFAC=5 as entry 2, but dealing with a zero Ycalc
!A   IRFAC=6 used for every K out of CALXX - save GCALC and add in to SOMEGA
!A   IRFAC= 11,12,13 as for 1,2,3 but for multiphase applications
!
!
      INCLUDE 'PARAMS.INC'

      EXTERNAL PCXX
      COMPLEX FCALC
      LOGICAL PRNCYC, TESTOV, LATABS
      DIMENSION IH(3)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /DERBAS/ DERIVB(400), LVARB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /FCAL  / FC, FCMOD, COSAL, SINAL, FCDERS(300), DERIVT(300)
      COMPLEX FC, DERIVT
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OVER  / ITFAC, OTFAC(10), KOTFAC(10), NTFAC, JTFAC, KOM15
      EQUIVALENCE (TFAC,OTFAC(1))
      EQUIVALENCE (KTFAC,KOTFAC(1))

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

      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      INCLUDE 'REFLNS.INC'
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      CHARACTER*5 TIM
      CHARACTER*6 MAIN
      COMMON /WHEN  / TIM(2), MAIN

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      REAL            CummChiSqd
      COMMON /CMN007/ CummChiSqd(MOBS)

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      SAVE SMYOB, SWYOBS
      CHARACTER*20 Integer2String

      GOTO (1,2,3,4,5,6,100,100,100,100,11,12,13), IRFAC
! INITIAL ENTRY : CLEARS ALL IF SINGLE PHASE
    1 SUMWD = 0.0
      SMYC = 0.0
      SMYD = 0.0
      SMYO = 0.0
      SMYOB = 0.0
      SWYOBS = 0.0
      SMWYOS = 0.0
! COUNT ZERO Y CALCS
      IZCT = 0
! OPEN FILE TO RECEIVE PROFILE IF NEEDED:
      MESSAG = 'File to output Profiles'
      NAMFIL = '.PRO'
      IF (PRNCYC(4)) CALL OPNFIL(IOP2,113)
      IF (MULFAS) GOTO 100
   11 SMIO = 0.0
      SMID = 0.0
      RINUM = 0.
      RIDEN = 0.
      DO I = 1, MFCSTO
        AIOBS(I) = 0.0
        ESDOBS(I) = 0.0
        SOMEGA(I) = 0.0
! TO INDICATE WHETHER OR NOT USED
        AICALC(I) = -9999.
      ENDDO
      IF (PRNCYC(1)) CALL MESS(LPT,1,'Argument    Yobs     Ycalc     Diff First and last reflns')
! OPEN FILE FOR FOURIER INPUT IF REQUIRED:
      MESSAG = 'File for h k l Fc Fo'
      NAMFIL = '.FOU'
      IF (PRNCYC(3)) CALL OPNFIL(IOP1,112)
      GOTO 100
! DEAL WITH ZERO YCALC:
    5 IZCT = IZCT + 1
! IF REQUESTED ON "L REFI" CARD, OMIT ZEROS FROM STATISTICS:
      IF (.NOT.ZBAKIN) GOTO 29
! ADDING IN ENTRY: THIS IS THE ENTIRE ENTRY IF SINGLE PHASE
    2 SUMWD = SUMWD + WDIFF*WDIFF
      SMYC = SMYC + ABS(YPEAK)
      SMYD = SMYD + ABS(DIFF)
      SMYO = SMYO + ABS(OBS)
      OB = OBS - YBACK
      SMYOB = SMYOB + ABS(OB)
      SWYOBS = SWYOBS + WT*OB*OB
      SMWYOS = SMWYOS + WT*OBS*OBS
      IF (.NOT.PRNCYC(1)) GOTO 29
      IF (IZCT.NE.0) WRITE (LPT,2001) IZCT
      IZCT = 0
      WRITE (LPT,2002) ARGI, OBS, YCALC, DIFF, KMIN, KMAX
 2002 FORMAT (' ',F12.2,2F12.4,F14.4,(2X,I5,2X,I5,2X))
! IF REQUESTED, WRITE OUT PROFILE FOR FUTURE REINPUT (INCLUDING ZERO YCALCS:
   29 IF (PRNCYC(4)) WRITE (IOP2,2003) ARGI, YBACK, OBS, YCALC, DOBS
 2003 FORMAT (F12.4,4G12.4)
      IF (MAIN.EQ.'FORTY1' .OR. MULFAS) GOTO 100
! REMAINING COUNTS - OR SEPARATE ENTRY PER PHASE, IF MULTIPHASE:
   12 KK = KMIN
      OB = OBS - YBACK
      DO J = KMIN, KMAX
        IF (TESTOV(GGCALC(J-KMIN+1),YPEAK)) GOTO 33
        TEMP = GGCALC(J-KMIN+1)/YPEAK
        IF (TESTOV(TEMP,WT)) GOTO 33
! COLLECT INTEGRATED INTENSITIES:
        AIOBS(J) = AIOBS(J) + TEMP*OB
!63: ONE TEMP REMOVED:
        ESDOBS(J) = ESDOBS(J) + TEMP/WT
   33 ENDDO
      GOTO 100
! AT CYCLE END, MAKE I OBS AND SUM THEM, AND DIFFS, THEN PRINT R FACTORS ETC:
! ALL OBEYED FOR SINGLE PHASE:
    3 IF ((IZCT.NE.0) .AND. PRNCYC(1)) WRITE (LPT,2001) IZCT
! NOT IF SIMULATION:
      IF (SIMUL) GOTO 13
! PRINT R FACTORS FOR END OF CYCLE:
      CALL MESS(LPT,1,'R Factors:')
      IF (TESTOV(SMYD,SMYO)) THEN
        CALL MESS(LPT,0,'- not available because denominators zero')
        GOTO 13
      ENDIF
      WRITE (LPT,2016) 100.*SMYD/SMYO
 2016 FORMAT (/' Profile R factors'/' 100 (Sum Y Diffs/Y Obs) =                  ',F8.2)
      WRITE (LPT,2021) 100.*SMYD/SMYOB
 2021 FORMAT (' 100 (Sum Y Diffs/(Sum Y Obs - Y Back)) =   ',F8.2)
      WRITE (LPT,2012) 100.*SQRT(SUMWD/SMWYOS)
 2012 FORMAT (/' Weighted Profile R factors'/                           &
     &        ' 100 Sqrt(Sum wt diffs sqrd/Sum wt obs sqrd)  ',         &
     &        '=        ',F8.2)
      WRITE (LPT,2022) 100.*SQRT(SUMWD/SWYOBS)
 2022 FORMAT (' 100 Sqrt(Sum wt diffs sqrd/Sum wt (obs-back) ','sqrd)  = ',F8.2)
      FREE = FLOAT(NOBS-LVARB)
      WRITE (LPT,2013) 100.*SQRT(FREE/SMWYOS)
 2013 FORMAT (/' Expected R factor =                  ',F8.2)
      WRITE (LPT,2023) 100.*SQRT(FREE/SWYOBS)
 2023 FORMAT (' Expected R factor (using obs-back) = ',F8.2)
      WRITE (LPT,2014) FREE
 2014 FORMAT (/' N-P+C =',F8.0)
      RWPOBS = 100.*SQRT(SUMWD/SWYOBS)
      RWPEXP = 100.*SQRT(FREE/SWYOBS)
      PAWLEYCHISQ = SUMWD/FREE
      DO ii = 1, NPTS
        CummChiSqd(ii) = CummChiSqd(ii) / SUMWD * ypmax
      ENDDO
      WRITE (LPT,2004) PAWLEYCHISQ, NOBS, LVARB
 2004 FORMAT (' Chi squared =',F10.4,' for',I6,' observations',' and',  &
     &        I4,' basic variables')
      WRITE (LPT,2019) SMYD, SMYO, SMYOB, SMYC, SMWYOS, SWYOBS, SUMWD
 2019 FORMAT (//6X,'SumYdif    SumYobs     SumYobs     SumYcalc    ',   &
     &        'Sum w obs sqrd '/18X,'as read     -Yback',20X,           &
     &        'as read'/1X,3F12.2,1X,F12.2,1X,F12.2//2X,                &
     &        'Sum w obs sqrd    ','SumWdiff sq'/6X,'- Yback'/1X,2F13.2)
      IF (MAIN.EQ.'FORTY1' .OR. MULFAS) GOTO 100
! PRINTING ENTRY PER PHASE IF MULTIPHASE, OR EVERY TIME IF SINGLE:
   13 DO K = 1, MAXKK(JPHASE)
! IGNORE IF NOT USED:
        IF (AICALC(K).EQ.-9999.) GOTO 24
! IGNORE IF DIVIDING SUM HAS STAYED ZERO:
        IF (SOMEGA(K).EQ.0.) GOTO 24
        AIOBS(K) = AIOBS(K)/SOMEGA(K)
        SMIO = SMIO + AIOBS(K)
        D = ABS(AIOBS(K)-ABS(AICALC(K)))
        SMID = SMID + D
        IF (.NOT.TESTOV(SOMEGA(K),ESDOBS(K))) THEN
          TEMP = SOMEGA(K)*SOMEGA(K)/ESDOBS(K)
          RINUM = RINUM + D*D*TEMP
          RIDEN = RIDEN + AIOBS(K)*AIOBS(K)*TEMP
        ENDIF
   24 ENDDO
      IF (TESTOV(SMID,SMIO)) GOTO 14
      IF (NVARF(2,JPHASE,1).EQ.0) GOTO 14
      IF (MULFAS) WRITE (LPT,2080) JPHASE
 2080 FORMAT (/' Phase',I3,' :')
      WRITE (LPT,2011) 100.*SMID/SMIO
 2011 FORMAT (/' Integrated Intensity R factor'/' 100 (Sum I Diffs/Sum I Obs) = ',F8.2)
! WEIGHTED, SQUARED INTENSITY R FACTOR:
      WRITE (LPT,2029) 100.*RINUM/RIDEN
 2029 FORMAT (/' Weighted, squared Integrated Intensity R factor'/      &
     &        ' 100 (Sqrd sum I Diffs/Sqrd sum I Obs)'/                 &
     &        ' weighted by 1/sigma sqrd     = ',F8.2)
! CHI SQUARED FOR INTENSITIES:
      WRITE (LPT,2028) RINUM/(FLOAT(MAXKK(JPHASE)-NVARF(2,JPHASE,1))), MAXKK(JPHASE), NVARF(2,JPHASE,1)
 2028 FORMAT (/' Chi squared for Intensities =',F10.4,' for',I5,' reflections and',I4,' structure variables')
! AND SUMS OF IDIFFS, IOBS:
      WRITE (LPT,2024) SMID, SMIO
 2024 FORMAT (' Sum I diffs =',G12.2,6X,'Sum I obs =',G12.2)
! AND OTHER CONSTITUENT PARTS:
      WRITE (LPT,2027) RINUM, RIDEN
 2027 FORMAT (/' Sum weighted I diffs sqrd =',F12.2,' Sum weighted',' I obs sqrd =',F12.2)
!  PRINT IOBS, ICALC ETC.
   14 IF (.NOT.PRNCYC(2) .AND. .NOT.PRECYC) GOTO 4
      IF (PRECYC) THEN
        DO I = 1, MAXKK(JPHASE)
          F4PAR(1,I) = 0.0
          F4PESD(1,I) = 0.0
        ENDDO
      ENDIF
      IF (MAG .AND. .NOT.FIXED) CALL MESS(LPT,1,'      h       k       l      Argument      I(obs)       I(calc)'//          &
     &    '         Diff        Esd(obs)     F*F       Q*Q')
      IF (MAG .AND. FIXED) CALL MESS(LPT,1,'    h    k    l      Argument      I(obs)       I(calc)'// &
     &                 '         Diff        Esd(obs)     F*F       Q*Q')
      IF (.NOT.MAG) CALL MESS(LPT,1,'    h    k    l    Argument      I(obs)       I(calc)         Diff        Esd(obs)     F*F')
      DO K = 1, MAXKK(JPHASE)
        CALL INDFIX(rHKL(1,K),IH)
        IF (AICALC(K).NE.-9999.) GOTO 26
        IF (FIXED) THEN
          WRITE (LPT,2008) IH
 2008     FORMAT (1X,3I5,' not used')
        ELSE
          WRITE (LPT,2009) (rHKL(I,K),I=1,3)
 2009     FORMAT (1X,3F8.3,' not used')
        ENDIF
        GOTO 25
   26   IF (SOMEGA(K).NE.0.) GOTO 27
        WRITE (LPT,2007) IH
 2007   FORMAT (1X,3I5,' gave zero denominator')
        GOTO 25
   27   AIDIFF = AIOBS(K) - AICALC(K)
!63: TEMPORARY AS ESDOBS COMING -VE:
        ESDOB = 0.
        IF (ESDOBS(K).GE.0.) ESDOB = SQRT(ESDOBS(K))/SOMEGA(K)
! FOR PCTF1 WHICH REFERS TO DSTAR2(KNOW) TO GET WAVELENGTH:
        KNOW = K
        CALL PCXX(2)
!   BY JBF 5/1/95 TO REMOVE A NUCLEAR CALCULATION IF SHOULD BE ABSENT
        FNSQ = 0.0
        IF (.NOT.LATABS(rHKL(1,K))) THEN
          FC = FCALC(rHKL(1,K))
          FNSQ = FC*CONJG(FC)
        ENDIF
        IF (MAG) THEN
          CALL FMCALC(rHKL(1,K),FMCMOD,FMCSQR)
          IF (.NOT.FIXED) THEN
            WRITE (LPT,2042) (rHKL(I,K),I=1,3), ARGK, AIOBS(K), AICALC(K), AIDIFF, ESDOB, FNSQ, FMCSQR
 2042       FORMAT (1X,3F8.3,F12.2,4F14.4,2F10.3)
          ELSE
            WRITE (LPT,2043) IH, ARGK, AIOBS(K), AICALC(K), AIDIFF, ESDOB, FNSQ, FMCSQR
 2043       FORMAT (1X,3I5,F12.2,4F14.4,2F10.3)
          ENDIF
        ELSE
          WRITE (LPT,2006) IH, ARGK, AIOBS(K), AICALC(K), AIDIFF, ESDOB, FNSQ
 2006     FORMAT (1X,3I5,F12.2,4F14.4,F10.3)
        ENDIF
        IF (PRECYC) THEN
          F4PAR(1,K) = AIOBS(K)
          F4PESD(1,K) = ESDOB
        ENDIF
   25 ENDDO
! SEND H,K,L, F CALC, F OBS AND DIFF TO FILE FOR FOURIER
    4 IF (.NOT.PRNCYC(3)) GOTO 100
      DO K = 1, MAXKK(JPHASE)
        IF (AICALC(K).EQ.-9999.) GOTO 22
        IF (SOMEGA(K).EQ.0.) GOTO 22
        FCA = 0.
        IF (AICALC(K).GT.0.) FCA = SQRT(AICALC(K))
        FOBS = 0.
        IF (AIOBS(K).GT.0.) FOBS = SQRT(AIOBS(K))
!  IS THIS A PROBLEM JBF 5-1-95
        FC = (0.0,0.0)
        IF (.NOT.LATABS(rHKL(1,K))) THEN
          FC = FCALC(rHKL(1,K))
        ENDIF
        STHL = VCTMOD(0.5,rHKL(1,K),2)
        E = EXP(-TFAC*STHL*STHL)
        CALL INDFIX(rHKL(1,K),IH)
        A = REAL(FC)
        B = AIMAG(FC)
        FCMOD = SQRT(A*A+B*B)
        IF (.NOT.TESTOV(FOBS,FCA)) FOBS = FCMOD*E*FOBS/FCA
        IF (CENTRC) THEN
          WRITE (IOP1,2030) IH, A, FOBS
        ELSE
          WRITE (IOP1,2030) IH, FC, FOBS
        ENDIF
   22 ENDDO
! READY FOR REINPUT BY FOURIER:
      REWIND IOP1
      GOTO 100
! ENTRY FROM CALXX TO KEEP GCALC OF ALL K FOR THIS ONE I, AND ADD IN
! CONTRIBUTIONS TO SOMEGA - KNOW HOLDS CURRENT K:
! @@ JCC There is an array bound error here sometimes - needs fixing.
! It seems that KMIN is far bigger than KNOW
!   6  GGCALC(KNOW-KMIN+1)=GCALC
! For now just check the bound and skip if its outside the range ..
! JvdS I think I have solved this by changing the assigment of KMIN at the start of CALPR
! JvdS 25 Feb 2002. It still happened.
    6 CONTINUE
      II = KNOW - KMIN + 1
      IF (II.GT.0 .AND. II.LE.500) THEN
        GGCALC(II) = GCALC
      ELSE
        CALL DebugErrorMessage('II.GT.0 .AND. II.LE.500 in RFACPR, II = '//Integer2String(II))
      ENDIF
      SOMEGA(KNOW) = SOMEGA(KNOW) + P5
      GOTO 100
  100 RETURN
 2001 FORMAT (' (',I5,' zeros )')
 2030 FORMAT (3I5,3F10.3)

      END SUBROUTINE RFACPR
!
!*****************************************************************************
!
      SUBROUTINE RUNPAR(IFAM,IGEN,ISPC)
!
! *** RUNPAR corrected by PJB and JBF Jun 93 ***
!
!H Control cycling over all parameters in LSQ
!
!A IFAM on exit contains family of "next" parameter, or -1 if end
!A IGEN on exit contains genus of "next" parameter
!A ISPC on exit contains species of "next" parameter
!A JPHASE on exit contains the phase of the "next" parameter.
!A JSOURC on exit contains the source of the "next" parameter.
!A On entry, if IFAM=0 the cycle is to be initialised
!A       and otherwise IFAM,IGEN,ISPC,JPHASE,JSOURC contain the "previous"
!A       values.
!
!D If JPHASE is changed, the new phase is read into store.
!D If SAPS, expects family 4 geners to start at 2, not 1
!D For family 4, cycles genus before species to keep together variables suitable
!D to make a banded matrix.
!
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

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
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

! INITIALISE ENTRY:
      IF (IFAM.EQ.0) THEN
        JPHASE = 1
        GOTO 5
      ENDIF
! ENTRY TO ADVANCE - IF FAMILY 4, WANT TO ADVANCE IGEN NOT ISPC FIRST:
      IF (IFAM.NE.4) GOTO 6
! THIS LETS OUT THE FIRST FAMILY 4 MEMBER AS USUAL
      IGEN = IGEN + 1
    8 IF (IGEN.LE.NGENPS(IFAM,JPHASE)) GOTO 100
! FOR FAMILY 4, NEXT SPECIES:
!** @@ I THINK SPECIES 1 OF GAMS DISAPPEARS - INVESTIGATE
      ISPC = ISPC + 1
      IGEN = 1
      IF (SAPS) IGEN = 2
      IF (ISPC.LE.NSPCPS(4,JPHASE)) GOTO 8
! FAMILY 4 END:
      ISPC = 1
      GOTO 4
! FAMILIES OTHER THAN 4 JUST AS IN PARRUN:
    6 ISPC = ISPC + 1
! CHECK NOT TOO MANY SPECIES FOR THIS FAMILY+GENUS:
    3 IF (ISPC.GT.NSPCPS(IFAM,JPHASE)) GOTO 2
! ALSO, FAMILIES 1, 3 AND 6 HAVE INDIVIDUAL GENERA OF DIFFERING LENGTHS:
      IF (IFAM.EQ.1 .AND. ISPC.GT.IABS(LF1SP(IGEN))) GOTO 2
      IF (IFAM.EQ.3 .AND. ISPC.GT.IABS(LF3SP(IGEN,JPHASE,JSOURC))) GOTO 2
      IF (IFAM.EQ.6 .AND. ISPC.GT.IABS(LF6SP(IGEN,JSOURC))) GOTO 2
      GOTO 100
! NEXT GENUS:
    2 IGEN = IGEN + 1
    1 ISPC = 1
      IF (IGEN.LE.NGENPS(IFAM,JPHASE)) GOTO 3
! ARE THERE OTHER SOURCES?
      IF (IFAM.NE.3 .AND. IFAM.NE.6) GOTO 4
! NEXT SOURCE:
      JSOURC = JSOURC + 1
      IF (JSOURC.LE.NSOURC) GOTO 7
    4 IFAM = IFAM + 1
      JSOURC = 1
    7 IGEN = 1
      IF (SAPS .AND. IFAM.EQ.4) IGEN = 2
! IF LAST PHASE AND FAMILY 6, TO END:
      IF (JPHASE.GT.1 .AND. IFAM.EQ.6) GOTO 42
! IN CASE NGENPS(IFAM,JPHASE) = 0
      IF (IFAM.LE.NFAM) GOTO 1
! NEXT PHASE:
   42 IF (JPHASE.GE.NPHASE) GOTO 101
      JPHASE = JPHASE + 1
    5 IFAM = 0
      IF (MULFAS) CALL PHMOVE(1,JPHASE)
      GOTO 4
  101 IFAM = -1
  100 RETURN

      END SUBROUTINE RUNPAR
!
!*****************************************************************************
!
      SUBROUTINE SETPR(PCXX,PFXX,MAGSET)
!
! *** SETPR updated by PJB 1 Feb 1994 ***
!
!C 12C
!H Sets up sizes and pointers for PR LSQ for one phase.
!H Also reads in most of CDF to find out exactly what it is doing
!A PCXX is the subroutine to deal with peak centres
!A PFXX is the subroutine to deal with peak functions
!A MAGSET is a subroutine to deal with magnetic setting up, if needed.
!A            MAGSET=DUMMY for non-magnetic, DOMAG for magnetic.
!D Sets up the COMM0N /PRBLEM with NFAM, NGENPS, NSPCPS, LF1SP, LF3SP etc
!D then call LSETPR to set up packing of parameter names, etc
!
      INCLUDE 'PARAMS.INC'

      EXTERNAL PFXX, PCXX, MAGSET
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POSNS2/ NATO(9)
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
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /SCLDAT/ ISCALE, NSCALE, SCALE(20), KSCALE(20), NSCL,      &
     &                LSCD(10)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      DO JSOUR = 1, NSOURC
        JSOURC = JSOUR
        CALL LOGSOU(JSOURC)
! SET UP WHICH DATA SOURCE, WHICH PEAK FUNCTION:
        CALL PFXX(0)
! UNLESS CAIL OR APES SET UP REST OF STRUCTURE FACTOR CALCULATION:
        IF (RIET .OR. SAPS) THEN
!* TO USE SLONLY, TAKE SETFCM APART & USE INDIVDUALLY
          CALL SETFCM(MAGSET)
          IF (PHMAG(JPHASE)) THEN
            CALL MAGCNC
            CALL MAGSET(2)
          ENDIF
        ELSE
          CALL RECIP
          CALL OPSYM(1)
! ENSURE POSSIBLE PROPAGATION VECTOR IS READ EVEN IF CAILS
          CALL PROPAG(1,INOUT)
          MAG = (INOUT.EQ.1)
        ENDIF
        IF (IBMBER .NE. 0) RETURN
! SET UP ASYMMETRIC UNIT:
        CALL SYMUNI
        IF (IBMBER .NE. 0) RETURN
! READ I AND MOST L CARDS:
        IF (.NOT.MULFAS) CALL STLSP0(PCXX,PFXX)
        CALL STLSPR(PCXX,PFXX)
! ADJUST WORDS AND THEIR SPECS TO FIT PRECISE PROBLEM:
!* READ L ADDP CARDS AND ADD TO TEMWRD AND ITMWD OR WHATEVER
!* AND IF FAMILY 3 OR 1, EXPECT A VALUE FOR LFnSP() ON CARD ALSO.
! SPECIFY PROBLEM:
        NGENPS(1,JPHASE) = 1
        NSPCPS(1,JPHASE) = 20
        NGENPS(6,JPHASE) = 3
        NSPCPS(6,JPHASE) = 100
        NFAM = 6
        IF (RIET) THEN
          NGENPS(4,JPHASE) = 0
          NSPCPS(4,JPHASE) = 0
        ELSE
          IF (CAIL) THEN
            NGENPS(4,JPHASE) = 1
          ELSE
            NGENPS(4,JPHASE) = NGEN4(JPHASE,JSOURC)
          ENDIF
! SPECIES IN FAMILY 4 ALSO SET WHEN MAX K KNOWN - THIS IS FOR PACKING KK:
          NSPCPS(4,JPHASE) = ITMREF
        ENDIF
! IN CASE USER GIVES A CARDS ETC FOR A CAIL PHASE, AND L RELA ETC:
!      IF (RIET .OR. SAPS) THEN
        NGENPS(2,JPHASE) = NATO(JPHASE)
! ALLOW ALL MAGNETIC:
        NSPCPS(2,JPHASE) = 22
!      ELSE
! NO STRUCTURE PARAMETERS IN CAIL OR APES:
!        NGENPS(2,JPHASE)=0
!        NSPCPS(2,JPHASE)=0
!      ENDIF
! FAMILY 5 ARE MULTIPOLES, AND EXCLUDED:
        NGENPS(5,JPHASE) = 0
        NSPCPS(5,JPHASE) = 0
! NOW DEAL WITH FAMILY 3
        NGENPS(3,JPHASE) = 10
        NSPCPS(3,JPHASE) = 6
! SPECIES TYPES FOR EACH GENUS OF FAMILY 1:
        LF1SP(1) = 13
! AND FOR FAMILY 3:
        LF3SP(1,JPHASE,JSOURC) = -1
        LF3SP(2,JPHASE,JSOURC) = -2
! LF3SP(3,4, ETC) SET INDIVIDUALLY IN PFXX:
! NPKGEN(JPHASE,JSOURC) IS NUMBER OF PEAK FUNCTION GENERA, AND SET INDIVIDUALLY
! IN PFXX
        L1 = NPKGEN(JPHASE,JSOURC) + 3
        L2 = NGENPS(3,JPHASE)
        DO I = L1, L2
          LF3SP(I,JPHASE,JSOURC) = 0
        ENDDO
! LF6SP(1,JSOURC) VARIES ACCORDING TO PEAK FUNCTION:
        LF6SP(2,JSOURC) = -2
        LF6SP(3,JSOURC) = -NBACK(JSOURC)
      ENDDO
      CALL LSETPR(PCXX,PFXX)

      END SUBROUTINE SETPR
!
!*****************************************************************************
!
      SUBROUTINE STLSP0(PCXX,PFXX)
!
! *** STLSP0 updated by JCM 28 Dec 92 ***
!
      EXTERNAL PCXX, PFXX

      INCLUDE 'PARAMS.INC'

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /EXCREG/ NEXCL(5), EXCLUD(40,5)
      COMMON /FUDG  / NFUDGE, IFDGPT(20), FUDGE1(20), FUDGE2(20), IFDTYP(20)
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OMITPR/ MIS, AMISS(3,100), KOM12

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

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /SCLDAT/ ISCALE, NSCALE, SCALE(20), KSCALE(20), NSCL, LSCD(10)
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP, NSKTOT, KOM24
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /SPLBCK/ SCOEFF(100,5)
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! READ THE I CARDS AND ALL THE (RELEVANT) L CARDS.
      IF (INREAD(9).GT.0) CALL INPUTI
! READ & INTERPRET 'I' CARDS - ALSO SETS (AND REPORTS) SIMUL:
      CALL IICD2
! INITIALISE COUNTS FOR THINGS OF WHICH MORE THAN 1 MAY APPEAR:
      MIS = 0
      NFUDGE = 0
! DEFAULT NO. OF FFT:
      NFFT = 1
      DO J = 1, NSOURC
        NBACK(J) = 0
        NEXCL(J) = 0
! DEFAULTS, AS YET UNPRINTED:
        TOLR(1,J) = 0.01
        TOLR(2,J) = 0.01
      ENDDO
! READ AND INTERPRET ALL PHASE-INDEPENDENT L CARDS:
      CALL INPLP0(PCXX,PFXX)
! CYCLE OVER SOURCES:
!      DO 3 JSOURC=1,NSOURC
! SET TOF, CN ETC:
      IF (JSOURC .NE. 1) CALL DebugErrorMessage('JSOURC .NE. 1 in STLSP0()')
      CALL LOGSOU(JSOURC)
! DEFAULT WEIGHTING IS TYPE 3:
      IF (ILREA(6,JSOURC).EQ.0) THEN
        IWGH(JSOURC) = 3
        CALL MESS(LPT,1,'No L WGHT card given - assuming 1/s^2 weights')
      ENDIF
! DEFAULT IF NO L OTYP CARD GIVING FORMAT TYPE OF OBSERVATION DATA:
      IF (ILREA(10,JSOURC).EQ.0) THEN
        MODEOB(JSOURC) = 1
        CALL MESS(LPT,1,'No L OTYP card - assuming observations given in mode 1 (format 3F)')
      ENDIF
! L BACK CARDS NEEDED:
      IF (ILREA(11,JSOURC).EQ.0) CALL ERRMES(2,1,'L BACK cards')
! SET UP SPLINE COEFFICIENTS IF REQUIRED:
      IF (IBACK.EQ.-2) CALL SPLINE(NBACK(JSOURC),ARGBAK(1,JSOURC), BACKGD(1,JSOURC),SCOEFF(1,JSOURC))
! REMARK IF NO EXCLUDED ZONES:
      IF (ILREA(12,JSOURC).EQ.0) CALL MESS(LPT,1,'No excluded regions')
! DEFAULT KNOTS (NOT PRINTED IN CASE NOT NEEDED AT ALL)
! WIFD 23-Jun-99 AKNOTS=-1 means no knots
!        IF (ILREAD(20) .EQ. 0) AKNOTS=-1.
! NOW THE CARDS WHICH ARE DIFFERENT ACCORDING TO SOURCE OF DATA:
! TOF AND/OR ED:
! NEED L THE2 CARD GIVING 2 THETA DEGREES:
      IF ((TOF.OR.ED) .AND. ILREA(18,JSOURC).EQ.0) CALL ERRMES(2,1,'L THE2 card giving counter angle')
! CN, SR AND/OR LX:
! NEED L WVLN CARD:
      IF ((CN.OR.LX.OR.SR) .AND. ILREA(13,JSOURC).EQ.0) CALL ERRMES(2,1,'L WVLN card giving wavelength')
! LX:
! NEED L TTHM:
      IF (LX .AND. ILREA(17,JSOURC).EQ.0) CALL TTHMLX(5)
! DEFAULT IF NO L SCAL:
      IF (ILREA(2,JSOURC).EQ.0) CALL LSSCAL(0)
! DEFAULT IF NO L SLIM CARD AND CAIL, SAPS OR APES:
!        IF (ILREAD(3) .EQ. 0 .AND. .NOT. RIET) CALL FAM4PR(5)
! FOR TOF MUST HAVE L PKCN CARD:
      IF (TOF .AND. ILREAD(7).EQ.0) THEN
        CALL ERRMES(2,1,'L PKCN card giving peak centre')
      ELSE
! COPY PKCN COEFFICIENTS FOR ALL PHASES.. AT PRESENT PKCN IS PHASE-INDEPENDENT,
! BUT I HAVE A SUSPICIOUS NATURE . .
        DO IJ = 2, NPHASE
          PKCNSP(1,IJ,KSOURC) = PKCNSP(1,1,KSOURC)
          PKCNSP(2,IJ,KSOURC) = PKCNSP(2,1,KSOURC)
        ENDDO
      ENDIF
! MUST HAVE L ZERO CARD:
      IF (ILREAD(8).EQ.0) THEN
        CALL ERRMES(2,1,'L ZERO card giving zero point')
      ELSE
! COMMENT AS FOR PKCN ABOVE:
        DO IJ = 2, NPHASE
          ZEROSP(1,IJ,KSOURC) = ZEROSP(1,1,KSOURC)
        ENDDO
      ENDIF
! DEFAULT IF NO L ABSC CARD FOR ABSORPTION CORRECTION:
      IF (ILREA(14,JSOURC).EQ.0) CALL ABCRPR(5)
! DEFAULT IF NO RTYP CARD - TYPE 1:
      IF (ILREA(4,JSOURC).EQ.0) THEN
        MODERR(JSOURC) = 1
        CALL MESS(LPT,1,'No L RTYP card - assuming  reflection indices input as a list of 3I5 h,k,l on given file')
      ENDIF
      CALL PCXX(6)
      WRITE (LPT,2008) STHMXX(JSOURC)
 2008 FORMAT (/' Maximum sin theta is',F10.4)
      CONTINUE
! DEFAULT IF NO L EXTN CARD FOR EXTINCTION CORRECTION:
      IF (ILREAD(15).EQ.0) CALL EXCRPR(5)
! DEFAULT IF NO L PROR CARD FOR PREFERRED ORIENTATION
      IF (ILREAD(16).EQ.0) CALL PREFOR(5)

      END SUBROUTINE STLSP0
!
!*****************************************************************************
!
      SUBROUTINE STLSPR(PCXX,PFXX)
!
! *** STLSPR updated by JCM Jun 92 ***
!
      EXTERNAL PCXX, PFXX

      INCLUDE 'PARAMS.INC'

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /EXCREG/ NEXCL(5), EXCLUD(40,5)
      COMMON /FUDG  / NFUDGE, IFDGPT(20), FUDGE1(20), FUDGE2(20),       &
     &                IFDTYP(20)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LREAD / ILREA(22,5), KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OMITPR/ MIS, AMISS(3,100), KOM12

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
      COMMON /SCLDAT/ ISCALE, NSCALE, SCALE(20), KSCALE(20), NSCL,      &
     &                LSCD(10)
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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /SPLBCK/ SCOEFF(100,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! READ AND INTERPRET ALL L CARDS EXCEPT SLAK ETC., FUDG,REFI, FIX,
! VARY, RELA:
      CALL INPLPR(PCXX,PFXX)
! MUST HAVE L PKFN CARDS:
      IF (ILREAD(9).EQ.0) CALL ERRMES(2,1,'L PKFN cards giving peak function')
! NOW FOR THIS PHASE:
! CLEAR CONSTRAINT COUNTS FOR GEOMETRY AND PAWLEY:
      NUMBON = 0
      NSKTOT = 0
      CALL JGMZER(NSLAK,1,4)
! DEFAULT IF NO L TFAC:
      IF (ILREAD(1).EQ.0) CALL LLTFAC(5)
! DEFALT IF NO L SPHA (FOR INSTANCE, IF NOT MULTI):
      IF (ILREAD(5).EQ.0) CALL LPSCAL(5)
      CALL GEOMIN(1)
      CALL ERRMES(0,0,'for Profile Refinement')

      END SUBROUTINE STLSPR
!
!*****************************************************************************
!
      SUBROUTINE TTHMLX(N)
!
! *** TTHMLX updated by JCM 4 Apr 89 ***
!
!H Multiple entry routine to deal with all aspects of 2 theta
!H   monochromator for constant wavelength X Ray Profile Refinement
!
!
      INCLUDE 'PARAMS.INC'

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      REAL            ALAMBD
      INTEGER                      NLAMB
      COMMON /DGEOM / ALAMBD(5,5), NLAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /FCAL  / FC, FCMOD, COSAL, SINAL, FCDERS(300), DERIVT(300)
      COMPLEX FC, DERIVT
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

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

      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /TTHMNC/ TTHMON(5), KTHMON(5), C2TMON(5), S4TMON(5),       &
     &                OPCMON(5), ALPCOR, DLPCOR

      GOTO (1,2,3,4,5,6), N
! GIVEN AN 'L TTHM' CARD IN COMM0N /SCRACH/, READ REST OF IT:
    1 CALL RDREAL(TTHMON(JSOURC),7,IPT,80,IER)
      WRITE (LPT,2000) TTHMON(JSOURC)
 2000 FORMAT (/' Monochromator 2 theta angle = ',F10.5)
      GOTO 19
! Entry from CALPR for Lorentz-Polarisation correction for Lab X-ray data:
    2 DTEM = RADIAN(ARGI-ZEROSP(1,JPHASE,JSOURC))
      STEM = SIN(DTEM)
      CTEM = COS(DTEM)
      SHTEM = SIN(0.5*DTEM)
      CCTEM = C2TMON(JSOURC)*CTEM
      CCTEM = 1. + CCTEM*CCTEM
      ALPCOR = CCTEM/(STEM*SHTEM*OPCMON(JSOURC))
      DLPCOR = RADIAN(S4TMON(JSOURC)*STEM*STEM)/(CCTEM*OPCMON(JSOURC))
      GOTO 100
! APPLY SHIFT IN COEFFICIENT:
    3 IF (JPHASE.NE.1) GOTO 100
      CALL ADJUST(TTHMON(JSOURC))
   19 C2TMON(JSOURC) = COS(RADIAN(TTHMON(JSOURC)))
      S4TMON(JSOURC) = SIN(2.*RADIAN(TTHMON(JSOURC)))
      OPCMON(JSOURC) = 1. + C2TMON(JSOURC)*C2TMON(JSOURC)
      GOTO 100
! WRITE OUT NEW 'L TTHM' CARD FOR LX:
    4 WRITE (NEWIN,2001) TTHMON(JSOURC)
 2001 FORMAT ('L TTHM',F10.5)
!** WE WILL NEED OUTPUT FACILITIES FOR THESE *S CARDS AS WELL AS IN
      GOTO 100
! DEAL WITH ABSENCE OF 'L TTHM' CARD:
    5 CALL MESS(LPT,1,'No L TTHM card - assuming monochromator 2 theta = 0')
      TTHMON(JSOURC) = 0.0
      GOTO 19
! FIX TTHMON  IF NO CARD GIVEN:
    6 IF (ABS(TTHMON(JSOURC)).LT.0.001) CALL ADDFX5(6,1,2,1,JSOURC,4)
      GOTO 100
      ENTRY THMLX8(NV)
! RECORD PARAMETER AS VARIABLE NUMBER NV:
      KTHMON(JSOURC) = NV
      GOTO 100
      ENTRY THMLX9
! RECORD PARAMETER AS FIXED:
      KTHMON(JSOURC) = 0
  100 RETURN

      END SUBROUTINE TTHMLX
!
!*****************************************************************************
!
      SUBROUTINE VARSPR
!
! *** VARSPR by JCM 7 Dec 90 ***
!
!X
!C 19B
!H Records which parameters are which variables for PR
!
!D Identifies species of parameter, then calls specific routines actually
!D    to set variables, thus not needing all COMMONs to be explicit here
!D
!N This exists separately from VARSPM, magnetic.  The other places where we
!N need the distinction, a function name MAGxxx is used as an argument.  But
!N this is called from VARMAK, and at present it is easier done this way.
!
      COMMON /DERBAS/ DERIVB(400), LVARB
      COMMON /DERVAR/ DERIVV(500), LVARV
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      LOGICAL         RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      IF (SIMUL) GOTO 100
! SET ALL VARIABLES FIXED:
      DO JPHASE = 1, NPHASE
        CALL PHMOVE(1,JPHASE)
        CALL LTFAC9
        CALL CELVAR(0,0)
        CALL F2VAR9
        CALL FM4PR9
        CALL LPSCA9
        CALL PHMOVE(-1,JPHASE)
      ENDDO
! BE SURE TO PUT THESE IN THE APPROPRIATE "ALL PHASE" OR "ALL SOURCE" LOOPS:
      CALL PFALL9
      CALL EXCPR9
      CALL PREFO9
      CALL LSSCA9
      CALL THMLX9
      CALL ABCPR9
      CALL BACKP9
      CALL ZERPR9
      CALL PCXX9
! SET STARTING PHASE & SOURCE:
      JP = 0
      JS = 0
! SCAN ALL VARIABLES:
      DO I = 1, LVARV
! UNPACK PARAMETER SPEC:
        CALL KUNPAK(LVRPR(I),IFAM,IGEN,ISPC,JPHASE,JSOURC)
        IF (JPHASE.NE.IPHASE) THEN
          CALL PHMOVE(-1,IPHASE)
          CALL PHMOVE(1,JPHASE)
          CALL LOGPHA(JPHASE)
        ENDIF
        IF ((IFAM.EQ.3.OR.IFAM.EQ.6) .AND. (JS.NE.JSOURC)) CALL LOGSOU(JSOURC)
        JP = JPHASE
        JS = JSOURC
! BRANCH ON FAMILY:
        GOTO (11,12,13,14,15,16), IFAM
! FAMILY 1, GENUS 1 - MISCELLANEOUS SINGLY NAMED SPECIES (TFAC, A* ETC,
! EXTN,PROR,SPHA)
   11   GOTO (31,35,35,35,35,35,35,36,37,38), ISPC
! TFAC:
   31   CALL LTFAC8(I)
        GOTO 1
! FAMILY 1 GENUS 1 ALSO CONTAINS THE CELL PARAMETERS:
   35   CALL CELVAR(ISPC-1,I)
        GOTO 1
! EXTINCTION CORRECTION PARAMETER EXTN:
   36   CALL EXCPR8(I)
        GOTO 1
! PREFERRED ORIENTATION:
   37   CALL PREFO8(I)
        GOTO 1
! FAMILY 1, GENUS 1, SPECIES 10 - SCALE FOR PHASE, SPHA:
   38   CALL LPSCA8(I)
        GOTO 1
! FAMILY 6: MISCELLANEOUS SOURCE DEPENDENT;
   16   GOTO (61,62,63), IGEN
! FAMIL6 GENUS 1 - SINGLY NAMED, SOURCE-DEPENDENT SPECIES (SCAL,TTHM)
   61   GOTO (51,52), ISPC
! FAMILY 6, GENUS 1, SPECIES 1 - SCALE FOR SOURCE, SCAL:
   51   CALL LSSCA8(I)
        GOTO 1
! MONOCHROMATOR 2 THETA FOR LX:
   52   CALL THMLX8(I)
        GOTO 1
! FAMILY 6, GENUS 2 - ABSC:
   62   CALL ABCPR8(ISPC,I)
        GOTO 1
! FAMILY 6, GENUS 3 - BACK:
   63   CALL BACKP8(ISPC,I)
        GOTO 1
! FAMILY 2 - THESE ARE ALL TO DO WITH THE STRUCTURE FACTOR:
   12   IF (ISPC.LE.12) CALL F2VAR8(IGEN,ISPC,I)
        GOTO 1
! FAMILY 3 - ZERO POINT, PEAK CENTRE AND PEAK FUNCTION PARAMETERS:
! GENUS 1=ZERO POINT, 2=PEAK CENTRE, REST ARE PEAK FUNCTION:
   13   GOTO (41,42), IGEN
        GOTO 43
! ZERO:
   41   CALL ZERPR8(ISPC,I)
        GOTO 1
! PEAK CENTRE PARAMETERS DEPEND ON TYPE OF REFINEMENT:
   42   CALL PCXX8(ISPC,I)
        GOTO 1
! REMAINING PEAK FUNCTION PARAMETERS:
   43   CALL PFALL8(IGEN,ISPC,I)
        GOTO 1
! FAMILY 4 - LONG VECTORS (SO FAR, INTS, SIGS, GAMS . . IN PAWLEY)
   14   CALL FM4PR8(IGEN,ISPC,I)
        GOTO 1
   15   CONTINUE
! FAMILY 5 ARE MULTIPOLES, EXCLUDED FOR NOW:
        GOTO 1
    1 ENDDO
      IF (MULFAS) CALL PHMOVE(-1,IPHASE)
  100 RETURN

      END SUBROUTINE VARSPR
!
!*****************************************************************************
!
      SUBROUTINE VCSWOP(N1,N2,J)
!
! *** VCSWOP BY JCM 3 FEB 88 ***
!
!X
!C 12C
!H Swops 2 integer vectors
!A On entry N1 and N2 are integer vectors each of length at least J
!A On exit their elements have been interchanged from  1 to J
!
      DIMENSION N1(J), N2(J)

      DO I = 1, J
        NTEMP = N1(I)
        N1(I) = N2(I)
        N2(I) = NTEMP
      ENDDO

      END SUBROUTINE VCSWOP
!
!*****************************************************************************
!
      SUBROUTINE WRLINE(N1,LEN,CHAR,N2)
!
! *** WRLINE by JCM 31 Jan 91 ***
!
!X
!H Writes a line of a specified character to unit LPT
!A On entry N1 is the number of empty lines required before.
!A          LEN is the required line length
!A          CHAR is the *1 character to be repeated
!A          N2 is the number of empty lines required after.
!O Writes LEN copies of CHAR to unit LPT
!
      CHARACTER*1 CHAR
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      DO I = 1, N1
        WRITE (LPT,2001)
      ENDDO
      WRITE (LPT,2000) (CHAR,I=1,LEN)
 2000 FORMAT (1X,120A1)
      DO I = 1, N2
        WRITE (LPT,2001)
      ENDDO
      RETURN
 2001 FORMAT (1X)

      END SUBROUTINE WRLINE
!
!*****************************************************************************
!
      SUBROUTINE ZEROPR(N)
!
! *** ZEROPR by JCM 9 May 88 ***
!
! MULTIPLE ENTRY ROUTINE DEALING WITH ALL ASPECTS OF ZERO POINT(S)
!
! ENTRY 2 ('USE') IS SO SIMPLE THAT IT IS EXPECTED TO BE DONE IN THE CALLING
! ROUTINE, E.G. CALTF1
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

      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6),          &
     &                NZERSP(9,5)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      GOTO (1,2,3,4), N
! GIVEN AN 'L ZERO' CARD IN COMM0N /SCRACH/, READ REST OF IT:
    1 CALL RDREAL(ZEROSP(1,JPHASE,JSOURC),7,IPT,80,IER)
      WRITE (LPT,2000) ZEROSP(1,JPHASE,JSOURC)
 2000 FORMAT (/' Zero point =',F10.4)
      NZERSP(JPHASE,JSOURC) = 1
      GOTO 100
    2 CONTINUE
! ENTRY 2 DUMMY - DONE IN CALL TO PCTF1:
      GOTO 100
! APPLY SHIFT IN COEFFICIENT:
    3 CALL ADJUST(ZEROSP(ISPC,JPHASE,JSOURC))
      GOTO 100
! WRITE OUT NEW 'L ZERO' CARD FOR TOF:
    4 WRITE (NEWIN,2001) ZEROSP(1,JPHASE,JSOURC)
 2001 FORMAT ('L ZERO',F10.4)
      GOTO 100
      ENTRY ZERPR8(NP,NV)
! SET PARAMETER NP TO BE VARIABLE NV
      KZROSP(NP,JPHASE,JSOURC) = NV
      GOTO 100
      ENTRY ZERPR9
! SET ALL ZEROPOINT PARAMETERS FIXED:
      DO I = 1, NZERSP(JPHASE,JSOURC)
        KZROSP(I,JPHASE,JSOURC) = 0
      ENDDO
      GOTO 100
  100 RETURN

      END SUBROUTINE ZEROPR
!
!*****************************************************************************
!
