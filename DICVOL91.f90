!*==DICVOL91.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!
!       ======================================================================
!     ==========================================================================
!
!     DDDDDDD   IIII   CCCCCC   VV    VV   OOOOOO   LL         999999        11
!     DD    DD   II   CC    CC  VV    VV  OO    OO  LL        99    99     1111
!     DD    DD   II   CC        VV    VV  OO    OO  LL        99    99   111 11
!     DD    DD   II   CC        VV    VV  OO    OO  LL         9999999       11
!     DD    DD   II   CC         VV  VV   OO    OO  LL              99       11
!     DD    DD   II   CC    CC    VVVV    OO    OO  LL    LL  99    99       11
!     DDDDDDD   IIII   CCCCCC      VV      OOOOOO   LLLLLLLL   999999       1111
!
!     ==========================================================================
!       ======================================================================
!
!     DICVOL91-VERSION PC :  30/06/92
!
!     (VERSION WITH SCREEN AND FILE OUTPUT - 19/03/93)
!
!     TRIAL AND ERROR METHOD FOR THE AUTOMATIC INDEXING OF POWDER DIFFRACTION
!     PATTERNS - VARIATION OF PARAMETERS BY SUCCESSIVE DICHOTOMY.
!     THIS VERSION INCLUDES PARTITION OF VOLUME SPACE.
!     THE FOLLOWING CRYSTALLINE SYSTEMS CAN BE EXAMINED :
!     CUBIC, TETRAGONAL,HEXAGONAL, ORTHORHOMBIC, MONOCLINIC
!     AND TRICLINIC.
!
!     PRECURSOR PROGRAMS :   P1 AND P2 (REF. 1), DICVOL (REF. 2)
!
!     DICVOL91 HAS BEEN WRITTEN BY D. LOUER AND A. BOULTIF (REF. 3)
!
!     FOR FURTHER INFORMATION CONTACT
!               D. LOUER
!               LABORATOIRE DE CRISTALLOCHIMIE
!               UNIVERSITE DE RENNES I
!               CAMPUS DE BEAULIEU
!               35042 RENNES CEDEX    -FRANCE-
!               TEL.   (33) 99-28-62-48
!               FAX    (33) 99-38-34-87
!               E-MAIL  LOUER@CICB.FR
!
!      ---------------------------------------------------------------
!      |            G E N E R A L   I N S T R U C T I O N S          |
!      ---------------------------------------------------------------
!
!      DICVOL PROPOSES SOLUTIONS; THE USER DISPOSES OF THEM.
!      THE AUTHORS DISCLAIM RESPONSABILITY FOR ALL USE OF THIS
!      PROGRAM, WHETHER GOOD, BAD OR INDIFFERENT.
!
!      DATA CARDS
!      ----------
!
!  CARD 1  TITLE                                       FORMAT(16A4)
!
!
!  CARD 2  N,ITYPE,JC,JT,JH,JO,JM,JTR                  FREE FORMAT
!
!          N               NUMBER OF LINES USED.
!          ITYPE           SPACING DATA TYPE.
!                      =2  2-THETA ANGLE IN DEGREES.
!          JC          =0  CUBIC SYSTEM IS NOT TESTED.
!                      =1  CUBIC SYSTEM IS TESTED.
!          JT          =0  TETRAGONAL SYSTEM IS NOT TESTED.
!                      =1  TETRAGONAL SYSTEM IS TESTED.
!          JH          =0  HEXAGONAL SYSTEM IS NOT TESTED.
!                      =1  HEXAGONAL SYSTEM IS TESTED.
!          JO          =0  ORTHORHOMBIC SYSTEM IS NOT TESTED.
!                      =1  ORTHORHOMBIC SYSTEM IS TESTED.
!          JM          =0  MONOCLINIC SYSTEM IS NOT TESTED.
!                      =1  MONOCLINIC SYSTEM IS TESTED.
!          JTR         =0  TRICLINIC SYSTEM IS NOT TESTED.
!                      =1  TRICLINIC SYSTEM IS TESTED.
!
!
!  CARD 3  AMAX,BMAX,CMAX,VOLMIN,VOLMAX,BEMIN,BEMAX    FREE FORMAT
!
!          AMAX    MAXIMUM VALUE OF UNIT CELL DIMENSION A IN ANGSTROMS.
!                  (IF AMAX= 0.0 DEFAULT= 20. ANGSTROMS)
!          BMAX    MAXIMUM VALUE OF UNIT CELL DIMENSION B IN ANGSTROMS.
!                  (IF BMAX= 0.0 DEFAULT= 20. ANGSTROMS)
!          CMAX    MAXIMUM VALUE OF UNIT CELL DIMENSION C IN ANGSTROMS.
!                  (IF CMAX= 0.0 DEFAULT= 20. ANGSTROMS)
!          VOLMIN  MINIMUM VOLUME FOR TRIAL UNIT CELLS IN ANGSTROMS**3.
!          VOLMAX  MAXIMUM VOLUME FOR TRIAL UNIT CELLS IN ANGSTROMS**3.
!                  (IF VOLMAX= 0.0 DEFAULT= 1500. ANGSTROMS**3)
!          BEMIN   MINIMUM ANGLE FOR UNIT CELL IN DEGREES
!                  (IF BEMIN= 0.0 DEFAULT= 90. DEGREES).
!          BEMAX   MAXIMUM ANGLE FOR UNIT CELL IN DEGREES
!                  (IF BEMAX= 0.0 DEFAULT= 125. DEGREES).
!
!
!  CARD 4  WAVE,POIMOL,DENS,DELDEN                     FREE FORMAT
!
!          WAVE    WAVELENGTH IN ANGSTROMS (DEFAULT=0.0 IF CU K ALPHA1).
!          POIMOL  MOLECULAR WEIGHT OF ONE FORMULA UNIT IN A.M.U.
!                  (DEFAULT =0.0 IF FORMULA WEIGHT NOT KNOWN).
!          DENS    MEASURED DENSITY IN G.CM(-3)
!                  (DEFAULT =0.0 IF DENSITY NOT KNOWN).
!          DELDEN  ABSOLUTE ERROR IN MEASURED DENSITY.
!
!
!  CARD 5  EPS,FOM                                     FREE FORMAT
!
!          EPS       =0.0  THE ABSOLUTE ERROR ON EACH OBSERVED LINE
!                          IS TAKEN TO .03 DEG. 2THETA, WHATEVER THE
!                          SPACING DATA TYPE (ITYPE IN CARD 2).
!                    =1.0  THE ABSOLUTE ERROR ON EACH OBSERVED LINE IS
!                          INPUT INDIVIDUALLY IN THE FOLLOWING CARDS,
!                          TOGETHER WITH THE OBSERVED 'D(I)', ACCORDING
!                          WITH THE SPACING DATA UNIT.
!                    EPS NE 0.0 AND 1.0
!                          THE ABSOLUTE ERROR IS TAKEN AS A CONSTANT
!                          (=EPS),IN DEG. 2THETA, WHATEVER THE SPACING
!                          DATA TYPE (ITYPE IN CARD 2).
!          FOM             LOWER FIGURE OF MERIT M(N) REQUIRED FOR PRINTED
!                          SOLUTION(S) (DEFAULT=0.0 IF LOWER M(N)=5.0).
!
!
!  CARD 6 TO 6+N  D(I),EPSIL(I)                        FREE FORMAT
!
!          (ONE FOR EACH OBSERVED LINE, UP TO N)
!          D(I)    VALUE DESCRIBING THE OBSERVED POSITION
!                  OF THIS LINE ACCORDING TO 'ITYPE'.
!          EPSIL   ABSOLUTE ERROR IN 'D(I)', ACCORDING TO 'ITYPE',
!                  ONLY IF EPS=1.0 (CARD 5).
!  NOTE:
!          IF ITYPE=1,2,4 THE VALUES OF 'D(I)' AND 'EPSIL(I)' MUST
!          BE LISTED IN INCREASING ORDER.
!          IF ITYPE=3 THEY MUST BE IN DECREASING ORDER.
!
!  REFERENCES:
!     1.-  LOUER, D. AND LOUER, M., METHODE D'ESSAIS ET ERREURS POUR
!          L'INDEXATION AUTOMATIQUE DES DIAGRAMMES DE POUDRE,
!	   J. APPL. CRYST. 5, 271-275 (1972).
!     2.-  LOUER, D. AND VARGAS, R., INDEXATION AUTOMATIQUE DES
!          DIAGRAMMES DE POUDRE PAR DICHOTOMIES SUCCESSIVES,
!	   J. APPL. CRYST. 15, 542-545 (1982).
!     3.-  BOULTIF, A. AND LOUER, D., INDEXING OF POWDER DIFFRACTION
!	   PATTERNS FOR LOW SYMMETRY LATTICES BY THE SUCCESSIVE
!	   DICHOTOMY METHOD, J. APPL. CRYST. 24, 987-993 (1991).
!     4.-  DE WOLFF, P.M., A SIMPLIFIED CRITERION FOR THE RELIABILITY
!	   OF A POWDER PATTERN INDEXING, J. APPL. CRYST. 5, 108-113 (1968).
!     5.-  SMITH, G. S. AND SNYDER, R. L., F(N): A CRITERION FOR RATING
!	   POWDER DIFFRACTION PATTERNS AND EVALUATING THE RELIABILITY
!	   OF POWDER-PATTERN INDEXING, J. APPL. CRYST. 12, 60-65 (1979).
!     6.-  MIGHELL, A.D., HUBBARD, C.R. AND STALIK, J.K., NBS*AIDS80:
!          A FORTRAN PROGRAM FOR CRYSTALLOGRAPHIC DATA EVALUATION.
!          NAT. BUR. STAND. (U.S.) TECH. NOTE 1141 (1981). (NBS*AIDS83
!          IS AN EXPANDED VERSION OF NBS*AIDS80).
!     7.-  LOUER, D., AUTOMATIC INDEXING: PROCEDURES AND APPLICATIONS,IN
!          'ACCURACY IN POWDER DIFFRACTION II', NIST, SPEC. PUBL. No. 846,
!          GAITHERSBURG, MD, USA, pp. 92-104, 1992.
!        *****************************************************
!        *                                                   *
!        *         THE USE OF DICVOL91 (VERSION 1.0)         *
!        *                                                   *
!        *****************************************************
!
!      USE AS INPUT DATA THE FIRST 20 LINES (ALTHOUGH THIS NUMBER
!      CAN BE DIFFERENT). AVOID SPURIOUS LINES OR TOO INACCURATE DATA.
!      ALL SYMMETRIES CAN BE EXAMINED IN ONE RUN. HOWEVER, IT IS
!      RECOMMENDED TO USE A TWO- OR THREE-STAGE PROCEDURE :
!        1- SEARCH IN HIGH SYMMETRIES DOWN TO ORTHORHOMBIC.
!           CARD 2 : N,ITYPE,1,1,1,1,0,0
!        2- SEARCH IN MONOCLINIC SYMMETRY.
!           CARD 2 : N,ITYPE,0,0,0,0,1,0
!        3- IF NECESSARY, SEARCH IN TRICLINIC SYMMETRY.
!           CARD 2 : N,ITYPE,0,0,0,0,0,1
!
!      NOTE THAT FOR SOLUTIONS WITH TRICLINIC SYMMETRY THE USE OF A
!      REDUCTION CELL PROGRAM IS USUALLY USEFULL TO OBTAIN THE
!      CONVENTIONAL UNIT CELL (E.G. CDF-SRCH/JCPDS).
!
!     NOTE - CASE OF TRIGONAL SYMMETRY WITH RHOMBOEDRAL LATTICE:
!	    THE PATTERN IS INDEXED WITH AN HEXAGONAL LATTICE, HAVING
!	    A UNIT CELL VOLUME THREE TIMES GREATER. OWING TO THE
!	    STRATEGY BASED ON THE PATITION OF VOLUME SPACE (SHELLS OF
!	    400 A**3) YOU CAN MISS THE SOLUTION IF, BY ACCIDENT, A
!	    SMALLER (PSEUDO) SOLUTION IS FOUND IN AN OTHER SYMMETRY.
!	    IF YOU SUSPECT SUCH A CASE, RUN DICVOL91 WITH THE HEXAGONAL
!	    CASE ONLY (JC=0, JT=0, JH=1, JO=0, JM=0, JTR=0).
!
!  RECOMMENDATIONS
!   ---------------
!   1- PLEASE SPEND TIME TO ENSURE THE QUALITY OF YOUR OBSERVED DATA.
!      WITH ACCURATE DATA THE SUCCESS RATE OF DICVOL91 IS VERY HIGH
!      (SEE REF. 3).
!      WITH BAD DATA THE CHANCE TO OBTAIN THE CORRECT SOLUTION IS VERY
!      SMALL AND THE CALCULATION CAN BE TIME-CONSUMMING.
!
!      WITH MODERN X-RAY POWDER DIFFRACTOMETERS (THE USE OF MONOCHROMATIC
!      RADIATION IS STRONGLY RECOMMENDED) ABSOLUTE ERRORS ON PEAK
!      POSITIONS LOWER THAN 0.03 DEGREES 2-THETA CAN BE ROUTINELY OBTAINED.
!      FOR INDEXING PURPOSES, ERRORS SHOULD NOT EXCEED 0.03 DEG. 2-THETA
!      [IN EXCEPTIONAL CASES, A FEW LINES WITH GREATER ERROR CAN BE
!      INTRODUCED IN INPUT DATA. IN THIS CASE, USE THE PARAMETER EPS= 1.
!      (CARD 5), AND ENTER INDIVIDUAL ERRORS ON EACH LINE(CARD 6 TO 6+N)].
!
!      WITH HIGH RESOLUTION POWDER DIFFRACTION DATA (CONVENTIONAL OR,
!      PARTICULARLY, SYNCHROTRON X-RAY SOURCES) THE ABSOLUTE ERROR IS
!      USUALLY LESS THAN 0.02 (OR EVEN 0.01) DEG. 2-THETA; CONSEQUENTLY,
!      EPS=0.02 (OR EVEN EPS=0.01) IS THEN RECOMMENDED; THE CONVERGENCE
!      OF THE DICHOTOMY PROCEDURE WILL BE IMPROVED. HOWEVER, BE SURE THAT
!      THIS CONDITION IS TRUE FOR ALL THE LINES USED AS INPUT DATA.
!
!   2- IF DICVOL91 HAS FOUND A SOLUTION FROM THE FIRST 20 (OR N) LINES,
!      THIS SOLUTION MUST INDEXED THE COMPLETE POWDER DIFFRACTION
!      PATTERN FOR THIS PURPOSE, THE PROGRAM NBS*AIDS83 (REF. 6) CAN BE
!      USED.
!
!           ----------          ------------------          ----------
!           |        |=========>|     CUBIC      |=========>|        |
!           |        |          ------------------          |        |
!           |        |                   V                  |        |
!           |        |              ----------              |        |
!           |        |              | CUDHKL |              |        |
!           |        |              ----------              |    A   |
!           |        |                                      |    F   |
!           |        |                                      |    F   |
!           |        |                                      |    I   |
!           |        |          ------------------          |    N   |
!           |        |=========>|   TETRAGONAL   |=========>|    E   |
!           |    P   |          ------------------          |    M   |
!           |    R   |                   V                  |    E   |
!           |    O   |              ----------              |    N   |
!           |    G   |              | TEDHKL |              |    T   |
!           |    R   |              ----------              |        |
!           |    A   |                                      |        |
!           |    M   |                                      |    D   |
!           |    M   |                                      |    E   |
!           |    E   |          ------------------          |    S   |
!           |        |=========>|    HEXAGONAL   |=========>|        |
!           |        |          ------------------          |        |
!           |    P   |                   V                  |    P   |
!           |    R   |              ----------              |    A   |
!           |    I   |              | TEDHKL |              |    R   |
!           |    N   |              ----------              |    A   |
!           |    C   |                                      |    M   |
!           |    I   |                                      |    E   |
!           |    P   |                                      |    T   |
!           |    A   |          ------------------          |    R   |
!           |    L   |=========>|  ORTHORHOMBIC  |=========>|    E   |
!           |        |          ------------------          |    S   |
!           |        |                   V                  |        |
!           |        |              ----------              |        |
!           |        |              | ORDHKL |              |        |
!           |        |              ----------              |        |
!           |        |                                      |        |
!           |        |                                      |        |
!           |        |          ------------------          |    R   |
!           |        |=========>|   MONOCLINIC   |=========>|    E   |
!           |        |          ------------------          |    S   |
!           |        |                   V                  |        |
!           |        |              ----------              |        |
!           |        |              | MODHKL |              |        |
!           |        |              ----------              |        |
!           |        |                                      |        |
!           |        |                                      |        |
!           |        |          ------------------          |        |
!           |        |=========>|   TRICLINIC    |=========>|        |
!           |        |          ------------------          ----------
!           |        |                   V                       V
!           |        |              ----------              ----------
!           |        |              | TRIHKL |              | PASAJE |
!           ----------              ----------              ----------
!     ------------------------------------------------------------------
!     |              P R O G R A M M E   P R I N C I P A L             |
!     ------------------------------------------------------------------
!
! CONSTANTS        : ALL CAPITALS
!
! Local Variables  : Initial Capital
!
! global variables : lowercase
!
      SUBROUTINE DICVOL91(Jc, Jt, Jh, Jo, Jm, Jtr, Volmin, Volmax, Poimol, Dens, Delden)

      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Jc, Jt, Jh, Jo, Jm, Jtr
      REAL,    INTENT (INOUT) :: Volmin, Volmax
      REAL,    INTENT (IN   ) :: Poimol, Dens, Delden
!
! Local variables
!
      REAL, INTRINSIC :: AMAX1, AMIN1
      REAL :: Amaxi, Angulo, Avog, Bmaxi, Cmaxi,   &
     &        Di, Difvol, Dos, Dosinv, Epsili, Epsqi, Pasvol, Pasvol1,       &
     &        Petiamaxi, Peticmax, Peticmaxi, Pr, Q11, Qi, Thi, Vmax, Vmin, Vmoi, &
     &        Vn, Voinf, Volmaxc, Vosup, Vplu, Vsupm, Vunitm, Vunitp, Vv, Wave, X, Zb, Zz
      INTEGER :: I, Ichoix, Jfl, Klv, Kvol, Kz1, Kz2, Kzt, Na, Nac, Nb, Nc, Nr, Nvol
      INTEGER, INTRINSIC :: NINT
!
!     Reading of the data
      iw = 117
      OPEN(iw,FILE='DICVOL.OUT',ERR=1900)
99001 FORMAT (A)
      IF ( amax.EQ.0.0 ) amax = 20.0
      IF ( Bmax.EQ.0.0 ) Bmax = 20.0
      IF ( Cmax.EQ.0.0 ) Cmax = 20.0
      IF ( Volmax.EQ.0.0 ) Volmax = 1500.0
      Wave = wave2 * 2
      pas = 0.4
      IF ( fom .EQ. 0.0 ) fom = 5.0
!     preparation and writing out of the data
 100  bb = 0.0
      cc = 0.0
      beta = 0.0
      jcount = 0
      nini = n
      WRITE (iw,99003)
99003 FORMAT (/36X,'INPUT DATA')
      dth = 0.
      WRITE (iw,99005)
99005 FORMAT (12X,'EXPERIMENTAL',35X,'EXPERIMENTAL'/14X,'2-THETA',42X,'ERROR'/)
      WRITE (iw,99034) (d(I),epsil(I),I=1,n)
99034 FORMAT (11X,F10.3,36X,F10.3)
      Dos = 1.0
      Dosinv = 0.5
      DO I = 1, n
        th(I) = Dos*d(I)
        Angulo = pirad*Dosinv*d(I)
        d(I) = wave2/SIN(Angulo)
        dth = dth + Dos*epsil(I)
        epsil(I) = d(I)*epsil(I)*pirad*Dosinv/TAN(Angulo)
      ENDDO
      Vn = .6/(1./n-.0052)*d(n)**3
 200  DO I = 1, n
        Pr = d(I)*d(I)
        q(I) = 1./Pr
        epsq(I) = 2.*epsil(I)/(Pr*d(I))
      ENDDO
      dth = dth/n
 300  Nr = NINT(d(1)/d(2))
      IF ( Nr.NE.1 ) THEN
        IF ( ABS((Nr*d(2)-d(1))).LT.(epsil(1)+Nr*epsil(2)) ) THEN
          Di = d(2)
          Thi = th(2)
          Qi = q(2)
          Epsili = epsil(2)
          Epsqi = epsq(2)
          DO I = 2, n - 1
            d(I) = d(I+1)
            th(I) = th(I+1)
            q(I) = q(I+1)
            epsil(I) = epsil(I+1)
            epsq(I) = epsq(I+1)
          ENDDO
          d(n) = Di
          th(n) = Thi
          q(n) = Qi
          epsil(n) = Epsili
          epsq(n) = Epsqi
          n = n - 1
          IF ( n.EQ.0 ) THEN
            WRITE (iw,*) ' EXPERIMENTAL ERROR TOO LARGE !'
            GOTO 2000
          ENDIF
          GOTO 300
        ENDIF
      ENDIF
      DO I = 1, n
        kq(I) = coeff*q(I)
        kepsq(I) = coeff*epsq(I)
      ENDDO
      IF ( Jm.NE.0 ) THEN
        IF ( Bemin.EQ.0.0 ) Bemin = 90.
        IF ( Bemax.EQ.0.0 ) Bemax = 125.
        WRITE (iw,99009) amax, Bmax, Volmin, Cmax, Bemin, Volmax, Bemax
99009   FORMAT (//21X,42('*')//4X,3('-'),'----  PARAMETER LIMITS  ----',4('-'),5X,5('-'),'---  VOLUME LIMITS  ---',&
     &          6('-')/4X,'|',33X,'|',5X,'|',32X,'|'/4X,'|',4X,'A MAXIMUM    =',F8.2,' A',5X,'|',5X,'|',32X,'|'/4X,&
     &          '|',4X,'B MAXIMUM',4X,'=',F8.2,' A',5X,'|',5X,'|',2X,'VOLUME MINIMUM =',F8.2,' A**3 |'/4X,'|',4X,  &
     &          'C MAXIMUM    =',F8.2,' A',5X,'|',5X,'|',32X,'|'/4X,'|',4X,'BETA MINIMUM =',F8.2,' Deg.',2X,'|',5X,&
     &          '|',2X,'VOLUME MAXIMUM =',F8.2,' A**3 |'/4X,'|',4X,'BETA MAXIMUM =',F8.2,' Deg.',2X,'|',5X,'|',32X,&
     &          '|'/4X,'|',33X,'|',5X,'|',32X,'|'/4X,35('-'),5X,34('-')//)
      ELSEIF ( (Jc+Jt+Jh+Jo).EQ.0 ) THEN
        WRITE (iw,99010) Volmin, Volmax
99010   FORMAT (//21X,42('*')//22X,9('-'),'---  VOLUME LIMITS  ---',8('-')/22X,'|',38X,'|'/22X,'|',38X,'|'/22X,'|',&
     &          3X,' VOLUME MINIMUM =',F8.2,' A**3',5X,'|'/22X,'|',38X,'|'/22X,'|',3X,' VOLUME MAXIMUM =',F8.2,    &
     &          ' A**3',5X,'|'/22X,'|',38X,'|'/22X,'|',38X,'|'/22X,40('-')//)
      ELSE
        WRITE (iw,99011) amax, Volmin, Bmax, Volmax, Cmax
99011   FORMAT (//21X,42('*')//4X,3('-'),'----  PARAMETER LIMITS  ----',4('-'),5X,5('-'),'---  VOLUME LIMITS  ---',&
     &          6('-')/4X,'|',33X,'|',5X,'|',32X,'|'/4X,'|',8X,'A MAXIMUM =',F6.2,' A',6X,'|',5X,'|',32X,'|'/4X,   &
     &          '|',33X,'|',5X,'|',2X,'VOLUME MINIMUM =',F8.2,' A**3 |'/4X,'|',8X,'B MAXIMUM =',F6.2,' A',6X,'|',  &
     &          5X,'|',32X,'|'/4X,'|',33X,'|',5X,'|',2X,'VOLUME MAXIMUM =',F8.2,' A**3 |'/4X,'|',8X,'C MAXIMUM =', &
     &          F6.2,' A',6X,'|',5X,'|',32X,'|'/4X,'|',33X,'|',5X,'|',32X,'|'/4X,35('-'),5X,34('-')//)
      ENDIF
      WRITE (iw,99012) Wave
99012 FORMAT (31X,'WAVELENGTH =',F9.6)
      WRITE (iw,99013) nini, fom
99013 FORMAT (/5X,'LOWER FIGURE OF MERIT REQUIRED FOR PRINTED SOLUTION(S) :  M(',I3,') = ',F5.1/)
!
!     With or without density ?
!
      kdens = 1
      X = Poimol*Dens*Delden
      IF ( X.GT.1.0E-04 ) THEN
        kdens = 2
        WRITE (iw,99014) Poimol, Dens, Delden
99014   FORMAT (//5X,'MOLECULAR WEIGHT =',F9.4,5X,'MEASURED DENSITY =',F8.4,' G.CM(-3)'/37X,'DENSITY ERROR    =',  &
     &          F8.4,' G.CM(-3)'/)
      ENDIF
99016 FORMAT (/' ',4X,'MOLECULAR WEIGHT =',F9.4,9X,'DENSITY=',F7.4,7X,'PRECISION=',F7.4)
99017 FORMAT (//' ',55X,10('*')/' ',10X,'A MAXIMUM =',F6.2,' A'/' ',8X,'B MAXIMUM =',F6.2,' A'/' ',8X,             &
     &        ' MAXIMUM ='F6.2,' A'/' ',8X,'VOLUME MINIMUM =',F9.2,' A**3'/' ',5X,'VOLUME MAXIMUM =',F9.2,' A**3')
99018 FORMAT (/' ',10X,'BETA MINIMUM =',F9.2/' ',10X,'BETA MAXIMUM =',F9.2)
!
!     Shared part
!
      v = Volmax
      Volmaxc = Volmax
      Vv = v
      pas2 = pas/2.
      pas4 = pas/4.
      pas8 = pas/8.
      pas16 = pas/16.
      pas32 = pas/32.
      pas64 = pas/64.
      Vsupm = Volmin
      Amin = d(1) - epsil(1)
      dd1 = Amin
      Zb = 2.*epsil(2) + epsil(1)
      Zz = ABS(2.*d(2)-d(1))
      IF ( Zz.LE.Zb ) THEN
        Bmin = d(3) - epsil(3)
        dd2 = Bmin
      ELSE
        Bmin = d(2) - epsil(2)
        dd2 = Bmin
      ENDIF
      Cmin = 2.5
      Na = (amax-Amin)/pas + 1.
      Nb = (Bmax-Bmin)/pas + 1.
      Nc = (Cmax-Cmin)/pas + 1.
      IF ( kdens.EQ.2 ) THEN
!
!     Treatment with density
!
        Avog = 0.602253
        rap = Poimol/Avog
        Vunitp = rap/(Dens-Delden)
        Vunitm = rap/(Dens+Delden)
        IF ( Jc.EQ.0 ) GOTO 1200
!        WRITE (iw,99030)
!99030 FORMAT (/3X,'SEARCH OF CUBIC SOLUTION(S)'/3X,27('*')//)
!        WRITE (iw,99020)
!99020 FORMAT (8X,'CUBIC SYSTEM')
        DO kz = 1, 50
          vinf = kz*Vunitm
          vsup = kz*Vunitp
          IF ( vsup.GE.Volmin ) THEN
            IF ( vsup.GT.Volmaxc ) vsup = Volmaxc
            IF ( vinf.GE.Volmaxc ) GOTO 1100
            IF ( vinf.LT.Volmin ) vinf = Volmin
!            WRITE (iw,99028) vinf, vsup
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
            CALL CUBIQU(Na)
            Difvol = ABS(Vv-v)
            IF ( Difvol.GT.1E-12 ) GOTO 1200
!            WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
          ENDIF
        ENDDO
        GOTO 1100
      ELSE
!
!    Treatment without density
!
        vsup = Volmin
        Pasvol = 400.
        Nvol = (Volmaxc-Volmin)/Pasvol
        Nvol = Nvol + 1
        IF ( Jc.NE.0 ) THEN
!          WRITE (iw,99030)
!99030 FORMAT (/3X,'SEARCH OF CUBIC SOLUTION(S)'/3X,27('*')//)
          vinf = Volmin
          vsup = Volmaxc
!          WRITE (iw,99028) vinf, vsup
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
!          WRITE (iw,99020)
!99020 FORMAT (8X,'CUBIC SYSTEM')
          CALL CUBIQU(Na)
          Difvol = ABS(Vv-v)
          IF ( Difvol.LE.1E-12 ) THEN
!            WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
          ENDIF
!          WRITE (iw,99025)
!99025 FORMAT (/2X,'END OF SEARCH FOR CUBIC SOLUTION(S)'/2X,35('*'))
          vsup = Vsupm
          Vv = v
          Volmaxc = AMIN1(Volmaxc,v)
        ENDIF
        IF ( Jt.NE.0 .OR. Jh.NE.0 .OR. Jo.NE.0 ) THEN
!          WRITE (iw,99031)
!99031 FORMAT (//3X,'SEARCH OF TETRAGONAL AND/OR HEXAGONAL AND/OR ORTHORHOMBIC SOLUTION(S)'/3X,69('*')/)
          DO Kvol = 1, Nvol
            vinf = vsup
            vsup = vinf + Pasvol
            IF ( vsup.GT.Volmaxc ) vsup = Volmaxc
            IF ( vinf.GE.Volmaxc ) GOTO 320
!            WRITE (iw,99028) vinf, vsup
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
            IF ( Jt.NE.0 ) THEN
!              WRITE (iw,99022)
!99022 FORMAT (8X,'TETRAGONAL SYSTEM')
              Ichoix = -1
              CALL TETHEX(Nc,Ichoix)
              Difvol = ABS(Vv-v)
              IF ( Difvol.LE.1E-12 ) THEN
!                WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
              ENDIF
            ENDIF
            IF ( Jh.NE.0 ) THEN
              Vv = v
!              WRITE (iw,99023)
!99023 FORMAT (8X,'HEXAGONAL SYSTEM')
              Ichoix = 1
              CALL TETHEX(Nc,Ichoix)
              Difvol = ABS(Vv-v)
              IF ( Difvol.LE.1E-12 ) THEN
!                WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
              ENDIF
            ENDIF
            IF ( Jo.NE.0 ) THEN
              Vv = v
!              WRITE (iw,99024)
!99024 FORMAT (8X,'ORTHORHOMBIC SYSTEM')
              CALL ORTHOR(Na,Nb,Nc)
              Difvol = ABS(Vv-v)
              IF ( Difvol.GT.1E-12 ) GOTO 320
!              WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
            ENDIF
          ENDDO
 320      CONTINUE
!          WRITE (iw,99026)
!99026 FORMAT (//2X,'END OF SEARCH FOR TETRAGONAL AND/OR HEXAGONAL AND/OR ORTHORHOMBIC SOLUTION(S)'/2X,77('*'))
          Vv = v
          Volmaxc = AMIN1(Volmaxc,v)
        ENDIF
        IF ( Jm.EQ.0 ) GOTO 600
!        WRITE (iw,99032)
!99032 FORMAT (//3X,'SEARCH OF MONOCLINIC SOLUTION(S)'/3X,32('*')/)
!        WRITE (iw,99027)
!99027 FORMAT (8X,'MONOCLINIC SYSTEM'/8X,17('-'))
        Bemin = Bemin*pirad
        Bemax = Bemax*pirad
        Petiamaxi = amax
        Peticmaxi = Cmax
        Amin = Bmin*SIN(Bemax)
        Bmin = Cmin
        Cmin = Cmin*SIN(Bemax)
        Nac = NINT((Amin-Cmin)/pas)
        IF ( (Nac*pas).LT.(Amin-Cmin) ) Nac = Nac + 1
        Cmin = Amin - Nac*pas
        amax = amax*SIN(Bemin)
        Cmax = Cmax*SIN(Bemin)
        Amaxi = amax
        Bmaxi = Bmax
        Cmaxi = Cmax
        Q11 = 2./SQRT(q(1)-epsq(1))
        amax = AMIN1(((Q11+.5)*SIN(Bemin)),amax)
        Bmax = AMIN1(Q11+.5,Bmax)
        IF ( amax.GT.Amaxi ) amax = Amaxi
        IF ( Bmax.GT.Bmaxi ) Bmax = Bmaxi
        IF ( Cmax.GT.Cmaxi ) Cmax = Cmaxi
        Cmax = AMIN1(amax,Cmax)
        mc = 1
        petiamax = AMIN1(Petiamaxi,amax/SIN(Bemin))
        Peticmax = AMIN1(Peticmaxi,Cmax/SIN(Bemin))
        WRITE (iw,99035) petiamax, Bmax, Peticmax
      ENDIF
 400  Na = (amax-Amin)/pas
      Nb = (Bmax-Bmin)/pas
      Nc = (Cmax-Cmin)/pas
      vsup = Vsupm
      petiamax = AMIN1(Petiamaxi,amax/SIN(Bemin))
      Peticmax = AMIN1(Peticmaxi,Cmax/SIN(Bemin))
      IF ( mc.EQ.0 ) WRITE (iw,99036) petiamax, Bmax, Peticmax
      petiamax = AMIN1(Petiamaxi,amax/SIN(Bemax))
      Peticmax = AMIN1(Peticmaxi,Cmax/SIN(Bemax))
      DO Kvol = 1, Nvol
        vinf = vsup
        vsup = vinf + Pasvol
        IF ( vsup.GT.Volmaxc ) vsup = Volmaxc
        IF ( vinf.LT.Volmaxc ) THEN
!          WRITE (iw,99028) vinf, vsup
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
          CALL MONOC1(Na,Nb,Nc)
          Difvol = ABS(Vv-v)
          IF ( Difvol.GT.1E-12 ) GOTO 500
!          WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
        ENDIF
      ENDDO
      amaxm = amax
      bmaxm = Bmax
      cmaxm = Cmax
      Jfl = Jfl + 1
      IF ( amax.LT.Amaxi .OR. Bmax.LT.Bmaxi .OR. Cmax.LT.Cmaxi ) THEN
        amax = amax + 8.
        Bmax = Bmax + 8.
        Cmax = Cmax + 8.
        IF ( amax.GT.Amaxi ) amax = Amaxi
        IF ( Bmax.GT.Bmaxi ) Bmax = Bmaxi
        IF ( Cmax.GT.Cmaxi ) Cmax = Cmaxi
        IF ( Cmax.GT.amax ) Cmax = amax
        mc = 0
        GOTO 400
      ENDIF
 500  CONTINUE
!      WRITE (iw,99029)
!99029 FORMAT (/2X,'END OF SEARCH FOR MONOCLINIC SOLUTIONS'/2X,38('*'))
 600  IF ( Jtr.EQ.0 ) GOTO 2000
!      WRITE (iw,99033)
!99033 FORMAT (//3X,'SEARCH OF TRICLINIC SOLUTION(S)'/3X,31('*')/)
!      WRITE (iw,99037)
!99037 FORMAT (8X,'TRICLINIC SYSTEM')
      amax = Amaxi
      Bmax = Bmaxi
      Cmax = Cmaxi
      Vv = v
      Volmaxc = AMIN1(Volmaxc,v)
      Vmoi = AMAX1(Vn*.7,Volmin)
      Vplu = AMIN1(Vn*1.3,Volmaxc)
      Klv = 1
      IF ( Vplu.LT.Vmoi ) THEN
        Vmoi = Volmin
        Vplu = Volmaxc
        Klv = 3
      ENDIF
      Vmoii = Vmoi
 700  Pasvol1 = AMIN1(200.,(Vplu-Vmoi))
      IF ( Vmoi.EQ.Vn*.7 .AND. Vplu.EQ.Vn*1.3 ) Pasvol1 = Vplu - Vmoi
      Nvol = (Vplu-Vmoi)/Pasvol1
      DO I = 0, Nvol
        Vmin = Vmoi + I*Pasvol1
        Vmax = Vmin + Pasvol1
        IF ( Vmin.GE.Vplu ) THEN
          SELECT CASE (Klv)
          CASE (1)
            GOTO 800
          CASE (2)
            GOTO 900
          CASE (3)
            GOTO 1000
          CASE DEFAULT
          END SELECT
        ENDIF
        IF ( Vmax.GT.Vplu ) Vmax = Vplu
!        WRITE (iw,99028) Vmin, Vmax
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
        CALL TRICLINI1(Vmin,Vmax)
        Difvol = ABS(v-Vv)
        IF ( Difvol.GT.1E-12 ) GOTO 1000
!        WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
      ENDDO
      SELECT CASE (Klv)
      CASE (2)
        GOTO 900
      CASE (3)
        GOTO 1000
      CASE DEFAULT
      END SELECT
 800  IF ( Vplu.NE.Volmaxc ) THEN
        Vmoi = Vplu
        Vplu = Volmaxc
        Klv = 2
        GOTO 700
      ENDIF
 900  IF ( Vmoii.NE.Volmin ) THEN
        Vmoi = Volmin
        Vplu = Vmoii
        Klv = 3
        GOTO 700
      ENDIF
 1000 CONTINUE
!      WRITE (iw,99038)
!99038 FORMAT (//2X,'END OF SEARCH FOR TRICLINIC SOLUTIONS'/2X,37('*'))
      GOTO 2000
 1100 CONTINUE
!      WRITE (iw,99025)
!99025 FORMAT (/2X,'END OF SEARCH FOR CUBIC SOLUTION(S)'/2X,35('*'))
      Vv = v
      Volmaxc = AMIN1(Volmaxc,v)
 1200 IF ( Jt.NE.0 .OR. Jh.NE.0 .OR. Jo.NE.0 ) THEN
!        WRITE (iw,99031)
!99031 FORMAT (//3X,'SEARCH OF TETRAGONAL AND/OR HEXAGONAL AND/OR ORTHORHOMBIC SOLUTION(S)'/3X,69('*')/)
        DO kz = 1, 50
          vinf = kz*Vunitm
          vsup = kz*Vunitp
          IF ( vsup.GE.Volmin ) THEN
            IF ( vinf.GE.Volmaxc ) GOTO 1250
            IF ( vsup.GT.Volmaxc ) vsup = Volmaxc
            IF ( vinf.GE.Volmaxc ) GOTO 1100
            IF ( vinf.LT.Volmin ) vinf = Volmin
!            WRITE (iw,99028) vinf, vsup
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
            IF ( Jt.NE.0 ) THEN
              Vv = v
!              WRITE (iw,99022)
!99022 FORMAT (8X,'TETRAGONAL SYSTEM')
              Ichoix = -1
              CALL TETHEX(Nc,Ichoix)
              Difvol = ABS(Vv-v)
              IF ( Difvol.LE.1E-12 ) THEN
!                WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
!                TetragonalSolutionFound = .FALSE.
              ELSE
!                TetragonalSolutionFound = .TRUE.
              ENDIF
            ENDIF
            IF ( Jh.NE.0 ) THEN
              Vv = v
!              WRITE (iw,99023)
!99023 FORMAT (8X,'HEXAGONAL SYSTEM')
              Ichoix = 1
              CALL TETHEX(Nc,Ichoix)
              Difvol = ABS(Vv-v)
              IF ( Difvol.LE.1E-12 ) THEN
!                WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
!                HexagonalSolutionFound = .FALSE.
              ELSE
!                HexagonalSolutionFound = .TRUE.
              ENDIF
            ENDIF
            IF ( Jo.NE.0 ) THEN
              Vv = v
!              WRITE (iw,99024)
!99024 FORMAT (8X,'ORTHORHOMBIC SYSTEM')
              CALL ORTHOR(Na,Nb,Nc)
              Difvol = ABS(Vv-v)
              IF ( Difvol.GT.1E-12 ) THEN
!                OrthorhombicSolutionFound = .TRUE.
                GOTO 1250
              ELSE
!                OrthorhombicSolutionFound = .FALSE.
!                WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
 1250   CONTINUE
!        WRITE (iw,99026)
!99026 FORMAT (//2X,'END OF SEARCH FOR TETRAGONAL AND/OR HEXAGONAL AND/OR ORTHORHOMBIC SOLUTION(S)'/2X,77('*'))
      ENDIF
      IF ( Jm.EQ.0 ) GOTO 1500
!      WRITE (iw,99032)
!99032 FORMAT (//3X,'SEARCH OF MONOCLINIC SOLUTION(S)'/3X,32('*')/)
!      WRITE (iw,99027)
!99027 FORMAT (8X,'MONOCLINIC SYSTEM'/8X,17('-'))
      Bemin = Bemin*pirad
      Bemax = Bemax*pirad
      Petiamaxi = amax
      Peticmaxi = Cmax
      Amin = Bmin*SIN(Bemax)
      Bmin = Cmin
      Cmin = Cmin*SIN(Bemax)
      Nac = NINT((Amin-Cmin)/pas)
      IF ( (Nac*pas).LT.(Amin-Cmin) ) Nac = Nac + 1
      Cmin = Amin - Nac*pas
      Vv = v
      amax = amax*SIN(Bemin)
      Cmax = Cmax*SIN(Bemin)
      Amaxi = amax
      Bmaxi = Bmax
      Cmaxi = Cmax
      Q11 = 2.0/SQRT(q(1)-epsq(1))
      amax = AMIN1((Q11+.5)*SIN(Bemin),amax)
      Bmax = AMIN1(Q11+.5,Bmax)
      IF ( amax.GT.Amaxi ) amax = Amaxi
      IF ( Bmax.GT.Bmaxi ) Bmax = Bmaxi
      IF ( Cmax.GT.Cmaxi ) Cmax = Cmaxi
      Cmax = AMIN1(amax,Cmax)
      mc = 1
      petiamax = AMIN1(Petiamaxi,amax/SIN(Bemin))
      Peticmax = AMIN1(Peticmaxi,Cmax/SIN(Bemin))
      WRITE (iw,99035) petiamax, Bmax, Peticmax
 1300 Na = (amax-Amin)/pas
      Nb = (Bmax-Bmin)/pas
      Nc = (Cmax-Cmin)/pas
      petiamax = AMIN1(Petiamaxi,amax/SIN(Bemin))
      Peticmax = AMIN1(Peticmaxi,Cmax/SIN(Bemin))
      IF ( mc.EQ.0 ) WRITE (iw,99035) petiamax, Bmax, Peticmax
      petiamax = AMIN1(Petiamaxi,amax/SIN(Bemax))
      Peticmax = AMIN1(Peticmaxi,Cmax/SIN(Bemax))
      DO kz = 1, 50
        vinf = kz*Vunitm
        vsup = kz*Vunitp
        IF ( vsup.GE.Volmin ) THEN
          IF ( vinf.LT.Volmaxc ) THEN
            IF ( vsup.GT.Volmaxc ) vsup = Volmaxc
            IF ( vinf.LT.Volmin ) vinf = Volmin
!            WRITE (iw,99028) vinf, vsup
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
            CALL MONOC1(Na,Nb,Nc)
            Difvol = ABS(Vv-v)
            IF ( Difvol.GT.1E-12 ) GOTO 1400
!            WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
          ENDIF
        ENDIF
      ENDDO
      amaxm = amax
      bmaxm = Bmax
      cmaxm = Cmax
      IF ( amax.NE.Amaxi .OR. Bmax.NE.Bmaxi .OR. Cmax.NE.Cmaxi ) THEN
        amax = amax + 8.0
        Bmax = Bmax + 8.0
        Cmax = Cmax + 8.0
        IF ( amax.GT.Amaxi ) amax = Amaxi
        IF ( Bmax.GT.Bmaxi ) Bmax = Bmaxi
        IF ( Cmax.GT.Cmaxi ) Cmax = Cmaxi
        IF ( Cmax.GT.amax  ) Cmax = amax
        mc = 0
        GOTO 1300
      ENDIF
 1400 CONTINUE
!      WRITE (iw,99029)
!99029 FORMAT (/2X,'END OF SEARCH FOR MONOCLINIC SOLUTIONS'/2X,38('*'))
 1500 IF ( Jtr.EQ.0 ) GOTO 2000
!      WRITE (iw,99033)
!99033 FORMAT (//3X,'SEARCH OF TRICLINIC SOLUTION(S)'/3X,31('*')/)
!      WRITE (iw,99037)
!99037 FORMAT (8X,'TRICLINIC SYSTEM')
      amax = Amaxi
      Bmax = Bmaxi
      Cmax = Cmaxi
      Vv = v
      Volmaxc = AMIN1(Volmaxc,v)
      Vmin = .7*Vn
      Vmax = 1.3*Vn
      IF ( Vmin.LT.Volmin  ) Vmin = Volmin
      IF ( Vmax.GT.Volmaxc ) Vmax = Volmaxc
      Voinf = Vmin
      Vosup = Vmax
      Kzt = 1
      Kz1 = 0
 1600 DO kz = 1, 50
        IF ( Kzt.NE.1 .OR. kz.LT.Kz1 .OR. kz.GT.Kz2 ) THEN
          Vmin = kz*Vunitm
          Vmax = kz*Vunitp
          IF ( Vmax.GE.Voinf ) THEN
            IF ( Vmin.GT.Vosup ) GOTO 1700
            IF ( Vmax.GT.Volmaxc ) Vmax = Volmaxc
!            WRITE (iw,99028) Vmin, Vmax
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
!            WRITE (iw,99037)
!99037 FORMAT (8X,'TRICLINIC SYSTEM')
            IF ( Kz1.EQ.0 ) Kz1 = kz
            CALL TRICLINI1(Vmin,Vmax)
            Difvol = ABS(Vv-v)
            IF ( Difvol.GT.1E-12 ) GOTO 1800
!            WRITE (iw,99021)
!99021 FORMAT (/,36X,'NO SOLUTION'/)
          ENDIF
        ENDIF
      ENDDO
 1700 IF ( Voinf.NE.Volmin .OR. Vosup.NE.Volmaxc ) THEN
        Voinf = Volmin
        Vosup = Volmaxc
        Kzt = 1
        Kz2 = kz - 1
        GOTO 1600
      ENDIF
 1800 CONTINUE
!      WRITE (iw,99038)
!99038 FORMAT (//2X,'END OF SEARCH FOR TRICLINIC SOLUTIONS'/2X,37('*'))
      GOTO 2000
 1900 CALL ErrorMessage('Error accessing input / output file')
      CLOSE (IW)
      RETURN
 2000 WRITE (iw,99019)
99019 FORMAT (/14X,'DICVOL91 : USEFUL REFERENCES'/14X,'--------'/16X,                                              &
     &        '* LOUER, D. & LOUER, M. (1972). J. APPL. CRYST. 5, 271-275.'/16X,                                   &
     &        '* BOULTIF, A. & LOUER, D. (1991). J. APPL. CRYST. 24, 987-993.')
!99020 FORMAT (8X,'CUBIC SYSTEM')
!99021 FORMAT (/,36X,'NO SOLUTION'/)
!99022 FORMAT (8X,'TETRAGONAL SYSTEM')
!99023 FORMAT (8X,'HEXAGONAL SYSTEM')
!99024 FORMAT (8X,'ORTHORHOMBIC SYSTEM')
!99025 FORMAT (/2X,'END OF SEARCH FOR CUBIC SOLUTION(S)')
!99026 FORMAT (//2X,'END OF SEARCH FOR TETRAGONAL AND/OR HEXAGONAL AND/OR ORTHORHOMBIC SOLUTION(S)'/2X,77('*'))
!99027 FORMAT (8X,'MONOCLINIC SYSTEM'/8X,17('-'))
!99028 FORMAT (/2X,'VOLUME DOMAIN BEING SCANNED :'/2X,27('=')/15X,'LOWER BOUND = ',F7.2,' A**3',5X,                 &
!     &        'HIGHER BOUND = ',F7.2,' A**3'/)
!99029 FORMAT (/2X,'END OF SEARCH FOR MONOCLINIC SOLUTIONS')
!99030 FORMAT (/3X,'SEARCH OF CUBIC SOLUTION(S)'/3X,27('*')//)
!99031 FORMAT (//3X,'SEARCH OF TETRAGONAL AND/OR HEXAGONAL AND/OR ORTHORHOMBIC SOLUTION(S)'/3X,69('*')/)
!99032 FORMAT (//3X,'SEARCH OF MONOCLINIC SOLUTION(S)'/3X,32('*')/)
!99033 FORMAT (//3X,'SEARCH OF TRICLINIC SOLUTION(S)'/3X,31('*')/)
99035 FORMAT (/,3X,'SEARCH OF MONOCLINIC SOLUTION(S) WITHIN THE LIMITS ON LINEAR PARAMETERS'/3X,71('*')/3X,        &
     &        '(SLIGHT TOLERANCE ACCEPTED):','  A MAX=',F7.3,'   B MAX=',F7.3,'   C MAX=',F7.3)
99036 FORMAT (/,3X,'EXTENSION OF THE SEARCH OF MONOCLINIC SOLUTION(S)'/3X,                                         &
     &        'WITHIN THE LIMITS ON LINEAR PARAMETERS :'/14X,'A MAX =',F8.3,6X,'B MAX =',F8.3,6X,'C MAX =',F8.3,6X,&
     &        F8.3)
!99037 FORMAT (8X,'TRICLINIC SYSTEM')
!99038 FORMAT (//2X,'END OF SEARCH FOR TRICLINIC SOLUTIONS')
      CLOSE(IW)
      END SUBROUTINE DICVOL91
