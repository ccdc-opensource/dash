!
!*****************************************************************************
!
!CSL Mark 4 Update 54 4-July-95
!
!
!               C A M B R I D G E   C R Y S T A L L O G R A P H Y
!
!                      S U B R O U T I N E   L I B R A R Y
!
!
!                       M A R K  4       L I B R A R Y
!
!             TO BE DESCRIBED IN THE USER'S MANUAL 'CCSL MARK 4'.
!
!
!
!
      SUBROUTINE ADDANG(NAME,N1,N2,N3,NA,IE)
!
! *** ADDANG updated by JCM 25 Jul 91 ***
!
!X
!C 8A
!H Finds an angle in the tables for geometric constraints, or adds it if absent.
!A On entry:
!A           NAME is the angle name, A4, which may be empty
!A           N1 is the number of the bond opposite the angle in the tables
!A           N2 is the number of one bond at the angle
!A           N3 is the number of the other bond at the angle
!A On exit, NA is the number of this angle in the angle names in ANGNAM
!A          IE is an error indicator, =0 if OK
!D If an angle already exists in the tables opposite N1, ignores
!D NAME and sends out NA=its position.
!D If there is no such angle, counts up in NUMANG in /SLKGEO, and adds
!D the new angle.  If in this case NAME is empty, invents a name.
!N The bonds N2 and N3 are held in INANG so that INANG( ,2) < INANG( ,3)
!
      CHARACTER*4 NAME, MAKNAM
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      IE = 0
! IS ANGLE ALREADY GIVEN IN TRIANGLE N1,N2,N3 OPPOSITE N1?
      NMIN = MIN(N2,N3)
      NMAX = MAX(N2,N3)
      DO I = 1, NUMANG
        IF (NMIN.EQ.INANG(I,2) .AND. NMAX.EQ.INANG(I,3)) THEN
          IF (NAME.NE.ANGNAM(I) .AND. NAME.NE.'    ') THEN
            WRITE (LPT,3000) NAME, ANGNAM(I)
            IE = 1
            GOTO 100
          ENDIF
          NA = I
          GOTO 100
        ENDIF
      ENDDO
! NEW ANGLE - COUNT NUMBER OF BOND ANGLES:
      CALL ERRCHK(2,NUMANG,100,1,'angles for geometric constraints')
! CHECK NAME OF ANGLE DOES NOT CLASH WITH ANY ATOM OR BOND NAMES:
      L1 = 0
      IF (NTARNM.GT.0) L1 = NCFIND(NAME,ATTNAM,NTARNM)
      L2 = NCFIND(NAME,BONNAM,NUMBON)
      IF (L1+L2+IATOM(NAME).GT.0) THEN
        CALL ERRCH2(NAME,1,'Angle name','also used elsewhere')
        IE = 1
        GOTO 100
      ENDIF
! ANGLE MAY HAVE BEEN GIVEN WITHOUT A NAME:
      ANGNAM(NUMANG) = NAME
      IF (NAME.EQ.'    ') ANGNAM(NUMANG) = MAKNAM('Ang',NUMANG)
      NA = NUMANG
      INANG(NA,1) = N1
      INANG(NA,2) = NMIN
      INANG(NA,3) = NMAX
  100 RETURN
 3000 FORMAT (/' ERROR ** ',A4,' and',A4,' refer to same angle')

      END SUBROUTINE ADDANG
!
!*****************************************************************************
!
      SUBROUTINE ADDATM(NAME,IA,XA,ISA,ILA,CELA,N)
!
! *** ADDATM updated by JCM 9 Jun 91 ***
!
!X
!C 8A
!H Finds an atom in the tables for geometric constraints, or adds it if absent.
!A On entry:
!A          NAME=the atom name, or is empty if a name is to be invented
!A          IA=the number of the base atom from which it is derived
!A          XA(1:3) hold its actual fractional coordinates
!A          ISA is the symmetry operator making it from base (-ve if needed)
!A          ILA is the lattice translation making it from base
!A          CELA(1:3) hold the cell translations making it from base
!A On exit  N=which entry in the atom tables it is
!D Searches for NAME in the existing table; if found, checks that all the
!D other components are the same, and exits with N=where found.
!D If NAME is empty , still does the check on all other components.
!D If not found, counts in NTARNM in /SLKGEO, and adds NAME and all
!D other components to tables, exitting with N=NTARNM
!
      CHARACTER*4 NAME, MAKNAM
      DIMENSION XA(3), CELA(3)
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
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

! IF NAME IS EMPTY, SEARCH FOR THE REST:
      IF (NAME.EQ.'    ') THEN
        DO I = 1, NTARNM
          IF (IABASE(I).NE.IA) GOTO 19
          DO J = 1, 3
            IF (CELLTR(J,I).NE.CELA(J)) GOTO 19
            IF (XSLAK(J,I).NE.XA(J)) GOTO 19
          ENDDO
          IF (ISYM(I).NE.ISA) GOTO 19
          IF (ILAT(I).NE.ILA) GOTO 19
! ATOM FOUND:
          N = I
          GOTO 100
   19   ENDDO
        GOTO 17
      ENDIF
! SEARCH FOR NAME IN THOSE FROM A CARDS:
      N1 = IATOM(NAME)
      IF (N1.LE.0) GOTO 1
      IF (N1.NE.IA) GOTO 2
      IF (ISA.NE.1) GOTO 2
      IF (ILA.NE.1) GOTO 2
      DO I = 1, 3
        IF (XA(I).NE.X(I,N1)) GOTO 2
        IF (CELA(I).NE.0.) GOTO 2
      ENDDO
! ATOM IDENTICAL - OK:
      GOTO 1
    2 CALL ERRCH2(NAME,1,'L ATOM card name','same as one on A card')
      GOTO 100
!
! THIS PUTS EVEN THE ORIGINAL ATOMS INTO THIS TABLE, WHICH IS WHAT WE WANT
! WHEN ACTUALLY APPLYING THE CONSTRAINTS.  WHEN GENERATING CARDS IN SLKGEN
! WE DON'T MIND.
    1 N = 0
      IF (NTARNM.GT.0) N = NCFIND(NAME,ATTNAM,NTARNM)
      IF (N.GT.0) THEN
        DO I = 1, 3
          IF (XA(I).NE.XSLAK(I,N)) GOTO 5
        ENDDO
        GOTO 100
    5   CALL ERRCH2(NAME,1,'Name','occurs on 2 L ATOM cards')
        GOTO 100
      ENDIF
   17 CALL ERRCHK(2,NTARNM,500,1,'names on L ATOM cards')
      N = NTARNM
! IF NO NAME GIVEN, INVENT ONE:
      ATTNAM(N) = NAME
      IF (NAME.EQ.'    ') ATTNAM(N) = MAKNAM('SK0',N)
      IABASE(N) = IA
      DO I = 1, 3
        CELLTR(I,N) = CELA(I)
        XSLAK(I,N) = XA(I)
      ENDDO
      ISYM(N) = ISA
      ILAT(N) = ILA
  100 RETURN

      END SUBROUTINE ADDATM
!
!*****************************************************************************
!
      SUBROUTINE ADDBON(NAME,NA1,NA2,NA)
!
! *** ADDBON updated by JCM 21 Jul 91 ***
!
!X
!C 8A
!H Finds a bond in the tables for geometric constraints, or adds it if absent.
!A On entry:
!A           NAME is either the bond name, A4, or it is empty, meaning
!A                that the name is irrelevant
!A           NA1 is the number of the atom at one end in the tables
!A           NA2 is the number of the atom at the other end
!A On exit, NA is the number of this bond in the bond tables
!D If a bond already exists in the tables from NA1 to NA2, ignores
!D NAME and sends out NA=its position.
!D If there is no such bond, counts up in NUMBON in /SLKGEO, and adds
!D the new bond.  If in this case NAME is empty, invents a name.
!N The ends of a bond are in IATM(,1:2) with (,1) less than (,2)
!
      CHARACTER*4 NAME, MAKNAM
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
!
! IS BOND ALREADY GIVEN FROM NA1 TO NA2?
      NMIN = MIN(NA1,NA2)
      NMAX = MAX(NA1,NA2)
      DO I = 1, NUMBON
        IF (NMIN.EQ.IATM(I,1) .AND. NMAX.EQ.IATM(I,2)) THEN
          IF (NAME.NE.BONNAM(I) .AND. NAME.NE.'    ') THEN
            WRITE (LPT,3000) NAME, BONNAM(I)
            IERR = IERR + 1
            GOTO 100
          ENDIF
          NA = I
          GOTO 100
        ENDIF
      ENDDO
! NEW BOND - COUNT NUMBER OF INVOLVED BONDS:
      CALL ERRCHK(2,NUMBON,500,1,'bonds for geometric constraints')
! CHECK NAME OF BOND DOES NOT CLASH WITH ANY ATOM NAMES:
      L1 = 0
      IF (NTARNM.GT.0) L1 = NCFIND(NAME,ATTNAM,NTARNM)
      IF (L1+IATOM(NAME).GT.0) THEN
        CALL ERRCH2(NAME,1,'Bond name','is also an atom name')
        GOTO 100
      ENDIF
! BOND MAY BE GIVEN WITHOUT A NAME:
      BONNAM(NUMBON) = NAME
      IF (NAME.EQ.'    ') BONNAM(NUMBON) = MAKNAM('Bnd',NUMBON)
      NA = NUMBON
      IATM(NA,1) = NMIN
      IATM(NA,2) = NMAX
  100 RETURN
 3000 FORMAT (/' ERROR ** ',A4,' and',A4,' refer to same bond')

      END SUBROUTINE ADDBON
!
!*****************************************************************************
!
      SUBROUTINE ADDCON(NPAR,KK1,AM,NSTAT)
!
! *** ADDCON corrected by PJB 30-May-1995 ***
!
!X
!C 6C
!H Adds a constraint to the list held in LSQ programs.
!A On entry:
!A          NPAR=number of parameters involved in constraint
!A          KK1 is an array holding the NPAR parameter specs
!A          AM is a corresponding array of amounts
!A          NSTAT is the status to be given to the constraint:
!A   NSTAT=4 : user supplied (may be later removed)
!A   NSTAT=5 : program supplied (may not be later removed)
!
!D Puts the constraint into a standard form, with the KK increasing, and the
!D amount corresponding to the smallest KK as 1.  Thus can tell if it has had
!D this constraint before, and if so merely gives it the new status.
!
!N There is also an entry SUBCON to take one out.  If the first element of
!N KK1 for SUBCON is incomplete, this will scan all constraints looking for
!N any whose KKs are ALL encompassed by KK1(1), and delete them.  It is used
!N to clear out family 4 each cycle for CAILS type refinement
!
      LOGICAL KSAME, KWHOLE, KWIPE
      DIMENSION KK1(NPAR), AM(NPAR)
      DIMENSION KK2(20), KK3(20), AM2(20), KUN(5)
      COMMON /LENINT/ NBITS
      COMMON /LINKAG/ NUMFV, NUMPAK, KKFV(200), KTYPFV(200), KSTFV(200),&
     &                KTIME(200), KUNPFV(5,30), NTIME, NUMCON,          &
     &                KKCON(500), AMCON(500), KPTCON(201), KSTCON(200), &
     &                KTPCON(200)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! ADDING ENTRY:
      IADD = 1
      GOTO 1
      ENTRY RELCON(NPAR,KK1,AM,NSTAT)
! ENTRY TO REPLACE AN EXISTING CONSTRAINT:
      IADD = 0
      GOTO 1
      ENTRY SUBCON(NPAR,KK1,AM,NSTAT)
! SUBTRACTING ENTRY:
      IADD = -1
! DISCOVER IF WIPING WHOLE SET:
    1 KWIPE = .NOT.(KWHOLE(KK1(1),KUN))
      IF (KWIPE) GOTO 5
! PUT INTO STANDARD FORM OF ASCENDING KK WITH NEW AM(1)=1.:
      CALL GMEQ(KK1,KK2,1,NPAR)
      DO I = 1, NPAR
        KK3(I) = MINIM(KK2,NPAR,N)
        AM2(I) = AM(N)
        IF (I.GT.1) AM2(I) = AM2(I)/AM2(1)
        KK2(N) = 2**(NBITS-1) - 1
      ENDDO
      AM2(1) = 1.
! SCAN EXISTING CONSTRAINTS:
    5 DO I = 1, NUMCON
! LOOKING FOR MATCHES:
        KPAR = KPTCON(I+1) - KPTCON(I)
        IF (KPAR.NE.NPAR .AND. .NOT.KWIPE) GOTO 3
        DO J = 1, KPAR
          IF (KWIPE) THEN
            IF (.NOT.(KSAME(KK1(1),KKCON(KPTCON(I)+J-1)))) GOTO 3
          ELSE
            IF (KK3(J).NE.KKCON(KPTCON(I)+J-1)) GOTO 3
! PARAMETER MATCH - NOW COEFFICIENT?
! IF THE NEW CONSTRAINT HAS THE SAME PARAMETERS BUT DIFFERENT COEFFICIENTS..
! THIS CAN MEAN "REMOVE THE OLD ONE AND REPLACE BY THE NEW" (ENTRY RELCON)
! OR SOME INCONSISTENCY LEADING TO A FIXING (ENTRY ADDCON)
            IF (IADD.EQ.0) GOTO 4
            IF (ABS(AM2(J)-AMCON(KPTCON(I)+J-1)).GT.0.00001) GOTO 3
          ENDIF
    4   ENDDO
! WE HAVE IT ALREADY - WERE WE ADDING OR SUBTRACTING OR REPLACING?
        IF (IADD) 6, 7, 101
! SUBTRACTING AN EXISTING:
    6   KSTCON(I) = 0
        GOTO 3
! REPLACING WITH NEW COEFFICIENTS
    7   DO J = 1, KPAR
          AMCON(KPTCON(I)+J-1) = AM2(J)
        ENDDO
        GOTO 101
    3 ENDDO
! IF NOT FOUND, BUT SUBTRACTING, EXIT:
      IF (IADD.EQ.-1) GOTO 100
! IT IS A REALLY NEW CONSTRAINT - ADD IT:
      CALL ERRCHK(2,NUMCON,200,0,'LSQ constraints')
      IF (IBMBER .NE. 0) RETURN
      KPTCON(NUMCON+1) = KPTCON(NUMCON) + NPAR
      DO I = 1, NPAR
        KKCON(KPTCON(NUMCON)+I-1) = KK3(I)
        AMCON(KPTCON(NUMCON)+I-1) = AM2(I)
      ENDDO
      KTPCON(NUMCON) = 1
      I = NUMCON
! ADDING (OR REPLACING) AN EXISTING JOINS HERE:
  101 KSTCON(I) = NSTAT
  100 RETURN

      END SUBROUTINE ADDCON
!
!*****************************************************************************
!
      SUBROUTINE ADDPLN(NIN,N)

      RETURN
      END SUBROUTINE ADDPLN
!
!*****************************************************************************
!
      SUBROUTINE ADDTOR(NAME,N1,N2,N3,N4,N5,N6,NT,IE)
!
! *** ADDTOR by JCM 16 Oct 90 ***
!
!X
!C 8A
!H Finds a torsion angle in the tables for geometric constraints, or adds it
!H if absent.
!A On entry:
!A           NAME is the torsion angle name, A4, which may be empty
!A           N1 is the number of the first bond of the pair of non-
!A              intersecting bonds between which the angle is required
!A           N2 is the number of the "axis" bond which joins one atom of N1
!A              to one atom of N3
!A           N3 is the number of the other bond of the pair defining the angle
!A           N4 is the number of the "free" bond joining the two other ends
!A              of N1 and N3
!A           N5 is the third side of the triangle formed by N1 and N2
!A           N6 is the third side of the triangle formed by N3 and N2
!A On exit, NT is the number of this angle in the angle names in ANGTOR
!A          IE is an error indicator, =0 if OK
!D If the angle already exists in the tables, ignores NAME and sends out
!D NT=its position.
!D If there is no such angle, counts up in NUMTOR in /SLKGEO, and adds
!D the new angle.  If in this case NAME is empty, invents a name.
!N The bonds N1 and N3 are held in INTOR;  which is which depends on the axis
!N N2, because it must join atoms (say A2 to A3) so that A2 < A3.  A2 is
!N defined to be on N1, and A3 on N3
!N
!
      CHARACTER*4 NAME
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      IE = 0
! IS TORSION ANGLE ALREADY GIVEN BETWEEN N1 AND N3 WITH N2 AS AXIS?
      NMIN = MIN(N1,N3)
      NMAX = MAX(N1,N3)
      DO I = 1, NUMTOR
        IF (NMIN.EQ.INTOR(I,1) .AND. N2.EQ.INTOR(I,2) .AND. NMAX.EQ.INTOR(I,3)) THEN
          IF (NAME.NE.TORNAM(I) .AND. NAME.NE.'    ') THEN
            WRITE (LPT,3000) NAME, TORNAM(I)
            IE = 1
            GOTO 100
          ENDIF
          NT = I
          GOTO 100
        ENDIF
      ENDDO
! NEW ANGLE - COUNT NUMBER OF TORSION ANGLES:
      CALL ERRCHK(2,NUMTOR,100,1,'torsion angles for geometric constraints')
! CHECK NAME OF TORSION ANGLE DOES NOT CLASH WITH ANY OTHER NAMES:
      L1 = 0
      IF (NTARNM.GT.0) L1 = NCFIND(NAME,ATTNAM,NTARNM)
      L2 = NCFIND(NAME,BONNAM,NUMBON)
      IF (NUMANG.GT.0) L3 = NCFIND(NAME,ANGNAM,NUMANG)
      IF (L1+L2+L3+IATOM(NAME).GT.0) THEN
        CALL ERRCH2(NAME,1,'Torsion angle name','also used elsewhere')
        IE = 1
        GOTO 100
      ENDIF
! ANGLE MAY NOT BE NAMED:
      IF (NAME.EQ.'    ') THEN
        WRITE (TORNAM(NUMTOR),2000) NUMTOR
 2000   FORMAT ('>',I3)
      ELSE
        TORNAM(NUMTOR) = NAME
      ENDIF
      NT = NUMTOR
! N2 IS THE AXIS (FROM A2 TO A3):
      INTOR(NT,2) = N2
!* N1 MUST JOIN A1 AND A2, AND N3 MUST JOIN A3 AND A4 - BUT I DON'T THINK
!* THIS IS VITAL - I BELIEVE IT TO BE ONLY NOMENCLATURE
      INTOR(NT,1) = NMIN
      INTOR(NT,3) = NMAX
! COMPLETE THE TRIANGLE "N1", N2, AND SET K2 TO POINT TO THEIR COMMON ATOM:
      CALL BONTRI(NMIN,N2,INTOR(NT,5),K2,IEE)
! COMPLETE THE TRIANGLE "N3", N2, AND SET K3 TO POINT TO THEIR COMMON ATOM:
      CALL BONTRI(NMAX,N2,INTOR(NT,6),K3,IEEE)
      IE = IEE + IEEE
      IF (IE.EQ.0) THEN
! IDENTIFY ENDS OF THE "FREE" BOND:
        K1 = IATM(NMIN,1)
        IF (K1.EQ.K2) K1 = IATM(NMIN,2)
        K4 = IATM(NMAX,1)
        IF (K4.EQ.K3) K4 = IATM(NMAX,2)
! ADD FREE BOND TO LIST:
        CALL ADDBON('    ',K1,K4,INTOR(NT,4))
      ENDIF
  100 RETURN
 3000 FORMAT (/' ERROR ** ',A4,' and',A4,' refer to same torsion angle')

      END SUBROUTINE ADDTOR
!
!*****************************************************************************
!
      SUBROUTINE ADJUST(PAR)

      USE WINTERACTER
      USE DRUID_HEADER
!
! *** ADJUST updated by JCM 11 Jan 88 ***
!
!X
!C 6C
!H Applies a (possibly fudged) shift to a given LSQ parameter.
!A On entry PAR is the parameter to be updated
!P In /NEWOLD/ SHIFT on entry is the shift from the LSQ matrix inversion
!P             IFAM,IGEN,ISPC specify the parameter, also packed in KPACK
!P In /FUDG/ is a list of all required fudge factors and their types
!
!D Sets XOLD = the existing value of PAR
!D Makes a tentative Xby applying SHIFT to XOLD
!D Scans IFDGPT to see if parameter has a fudge;  if so, branches on
!D its type in IFDTYP, and adjusts XNEW accordingly.
!D Finally sets XNEW into the parameter PAR.
!
      COMMON /FUDG  / NFUDGE, IFDGPT(20), FUDGE1(20), FUDGE2(20),       &
     &                IFDTYP(20)
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      REAL FudgeFactor

      XOLD = PAR
! Jvds Assuming that the instabilities in the Pawley refinement are caused
! by the shift applied being too large, this would be the right place to adjust that.
! Rather than setting FUDGE1 factors, how about just multiplying everything by a user set
! damping factor.
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogGetReal(IDF_FudgeFactor,FudgeFactor)
      XNEW = XOLD + FudgeFactor*SHIFT
! EXIT IF NO FUDGES AT ALL:
      IF (NFUDGE.EQ.0) GOTO 101
!
!* WE NEED EVENTUAL PROVISION FOR PARTICULAR PHASES & SOURCES:
! PACK FAMILY AND GENUS, NO PARTICULAR SPECIES:
      JFG = KPAK(IFAM,IGEN,0,0,0)
! PACK FAMILY AND SPECIES, NO PARTICULAR GENUS:
      JFS = KPAK(IFAM,0,ISPC,0,0)
! PACK FAMILY ALONE:
      IFAMM = KPAK(IFAM,0,0,0,0)
! PACK GENUS ALONE:
      IGENN = KPAK(0,IGEN,0,0,0)
! PACK SPECIES ALONE:
      ISPCC = KPAK(0,0,ISPC,0,0)
!
! SCAN ALL FUDGES FOR A MATCH WITH ANY OF:
!        COMPLETE SPECIFICATION IN KPACK
!        FAMILY PLUS GENUS IN JFG
!        FAMILY PLUS SPECIES IN JFS
!        FAMILY ALONE IN IFAMM
!        GENUS ALONE IN IGENN
!        SPECIES ALONE IN ISPCC
      DO I = 1, NFUDGE
        IF (KPACK.EQ.IFDGPT(I)) GOTO 2
        IF (JFG.EQ.IFDGPT(I)) GOTO 2
        IF (JFS.EQ.IFDGPT(I)) GOTO 2
        IF (IFAMM.EQ.IFDGPT(I)) GOTO 2
        IF (IGENN.EQ.IFDGPT(I)) GOTO 2
        IF (ISPCC.EQ.IFDGPT(I)) GOTO 2
      ENDDO
      GOTO 101
! THERE IS A FACTOR:
    2 GOTO (11,12,13,12), IFDTYP(I)
! SIMPLE MULTIPLICATIVE FACTOR:
   11 XNEW = XOLD + FUDGE1(I)*SHIFT
      GOTO 101
! LOWER LIMIT FROM "GE" (MAY BE COMBINED WITH UPPER LIMIT):
   12 IF (XNEW.LT.FUDGE1(I)) XNEW = FUDGE1(I)
      IF (IFDTYP(I).EQ.2) GOTO 101
! UPPER LIMIT FROM "LE":
   13 IF (XNEW.GT.FUDGE2(I)) XNEW = FUDGE2(I)
      GOTO 101
  101 PAR = XNEW

      END SUBROUTINE ADJUST
!
!*****************************************************************************
!
      SUBROUTINE ANGLST(I1)
!
! *** ANGLST updated by JCM 11 Sep 92 ***
!
!X
!C 8B
!H Lists all angles at one source atom made by a given list of bonds.
!
!A I1 is the number of the source atom (which will belong to the original list)
!
!P A list of bonds starting from atom I1 must be in COMMON /BONDLA.
!P There will be NB bonds stored in BSAVE, with the specifications of the
!P destination atoms in N2SAVE (held negatively if the atom is not in
!P the original list, but a symmetry relation), and the coordinate differences
!P in DXSAVE.
!
!D If there are at least 2 bonds, scans all pairs of bonds & prints out the
!D angle at atom I1 between the pair.  Destination atoms may be in any of
!D the 27 cells around the central asymmetric unit.
!
!O Writes to unit LPT the list of angles, with specifications of destination
!O        atoms if not original
!
      CHARACTER*16 CH1, CH2
      DIMENSION K1(6), K2(6), XD1(3), XD2(3)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      COMMON /BONDLA/ NB, BSAVE(100), DXSAVE(3,100), NBSAVE(100),       &
     &                N2SAVE(100), N3SAVE(100), I1LAST, BMAX, BMIN,     &
     &                BBMAX, SDMAX, ANG1, ANG2, BON2, SD1, SD2, BONOUT, &
     &                LSK, SLK
      LOGICAL SLK, BONOUT
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
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
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      IF (NB.LE.1) GOTO 100
      IF (NB.EQ.2) CALL MESS(LPT,1,'Angle at atom '//ATNAME(I1)//' :')
      IF (NB.GT.2) CALL MESS(LPT,1,'Angles around atom '//ATNAME(I1)    &
     &                       //' :')
      CALL MESS(LPT,0,'At1-At2-At3      Angle       Atom1 if not'//     &
     &          ' original                  Atom3 if not original')
      DO I = 1, NB
        II = I + 1
        IF (II.GT.NB) GOTO 1
        DO J = II, NB
          COSTH = SCLPRD(DXSAVE(1,I),DXSAVE(1,J),1)/(BSAVE(I)*BSAVE(J))
          CALL SINCOS(COSTH,SINTH,'ANGLST')
          ANG = DEGREE(ATAN2(SINTH,COSTH))
! IF SLACK CONSTRAINTS BEING OUTPUT, ANG MAY BE NEEDED HERE:
          IF (.NOT.SLK) GOTO 42
          IF (ANG.GT.ANG1+SD1 .OR. ANG.LT.ANG1-SD1) GOTO 42
! PICK UP BONDS SURROUNDING ANGLE & MAKE 3RD BOND OF TRIANGLE:
          NB1 = NBSAVE(I)
          NB2 = NBSAVE(J)
          CALL BONTRI(NB1,NB2,NB3,NA,IE)
          N = NUMANG
          CALL ADDANG('    ',NB3,NB1,NB2,NANG,IE)
          IF (NANG.GT.N) WRITE (LSK,2020) ANGNAM(NANG), BONNAM(NB1), BONNAM(NB2), ANG2, SD2
 2020     FORMAT ('L ANGL ',3(A4,2X),F10.2,F10.3)
   42     IF ((N2SAVE(I).LT.0) .OR. (N2SAVE(J).LT.0)) GOTO 3
          WRITE (LPT,2002) ATNAME(N2SAVE(I)), ATNAME(I1), ATNAME(N2SAVE(J)), ANG
 2002     FORMAT (1X,A4,'-',A4,'-',A4,F9.2)
          GOTO 2
! ONE OR BOTH ATOMS NEED FURTHER SPECIFICATION:
    3     DO K = 1, 3
            XD2(K) = X(K,I1)
! TAKEN OUT C41:
!      CALL FRACT(XD2(K),Y,N)
            XD1(K) = XD2(K) - DXSAVE(K,I)
            XD2(K) = XD2(K) - DXSAVE(K,J)
          ENDDO
! UNPACK SPECIFICATIONS:
          IF (N2SAVE(I).LT.0) CALL ATSPEC(-N2SAVE(I),K1,CH1)
          IF (N2SAVE(J).LT.0) CALL ATSPEC(-N2SAVE(J),K2,CH2)
          IF (N2SAVE(J).GT.0) WRITE (LPT,2001) ATNAME(K1(1)), ATNAME(I1)&
     &                               , ATNAME(N2SAVE(J)), ANG, CH1, XD1
 2001     FORMAT (1X,A4,'-',A4,'-',A4,F9.2,2X,A16,3F8.4)
          IF (N2SAVE(I).GT.0) WRITE (LPT,2003) ATNAME(N2SAVE(I)),       &
     &                               ATNAME(I1), ATNAME(K2(1)), ANG,    &
     &                               CH2, XD2
 2003     FORMAT (1X,A4,'-',A4,'-',A4,F9.2,44X,A16,3F8.4)
          IF ((N2SAVE(I).LT.0) .AND. (N2SAVE(J).LT.0)) WRITE (LPT,2010) &
     &        ATNAME(K1(1)), ATNAME(I1), ATNAME(K2(1)), ANG, CH1, XD1,  &
     &        CH2, XD2
 2010     FORMAT (1X,A4,'-',A4,'-',A4,F9.2,2X,A16,3F8.4,2X,A16,3F8.4)
    2   ENDDO
    1 ENDDO
  100 RETURN
      END SUBROUTINE ANGLST
!
!*****************************************************************************
!
      FUNCTION ANGRAD(A,B,IR)
!
! *** ANGRAD by JCM 26 Sep 84 ***
!
!X
!C 1B
!H Calculates the angle in radians between two vectors, in either space.
!
!A A is a real 3-sized array which on entry holds the first vector
!A B is a real 3-sized array which on entry holds the second vector
!A IR on entry =1 for real space, 2 for reciprocal
!
!P RECIP must have set up the cell parameters.
!D ANGRAD on exit is set to be the angle in radians between vectors A and B.
!D Uses -A.B/moduli if real space, because expects A and B to be plane
!D normals, and the required angle to be between planes.
!
      DIMENSION A(3), B(3)

      C = SCLPRD(A,B,IR)
      C = C/(VCTMOD(1.0,A,IR)*VCTMOD(1.0,B,IR))
      IF (IR.EQ.1) C = -C
      ANGRAD = ACOS(C)

      END FUNCTION ANGRAD
!
!*****************************************************************************
!
      FUNCTION ANITF(H,N)
!
! *** ANITF by JCM 19 Jul 83 ***
!
!X
!C 4B
!H Forms the contribution to the anisotropic temperature factor on an
!H atom N from indices H.
!
!A H is a 3-size real array holding h,k,l on entry
!A N on entry = which atomic position
!
!P SETANI must have been obeyed to set up in the COMMON /ANISO:
!P      IAPT(N) =I for 'Nth atom has Ith temperature factor in array ATF', or
!P              =0 for 'Nth atom has no anisotropic temperature factor.
!D SETANI has converted the user's coefficients to standard betas in the array
!D ATF, in the expression exp-(beta11*h*h + 2.*beta23*k*l + etc.) so that
!D ANITF need use only this single expression.
!N Note the 2's in the expression.
!
      DIMENSION H(3)
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
!
      IA = IAPT(N)
      ANITF = 1.
      IF (IA.EQ.0) GOTO 100
! OUT IF NO ATF ON THIS ATOM
      A = ATF(1,IA)*H(1)*H(1) + ATF(2,IA)*H(2)*H(2) + ATF(3,IA)*H(3)    &
     &    *H(3) + 2.*ATF(4,IA)*H(2)*H(3) + 2.*ATF(5,IA)*H(1)*H(3)       &
     &    + 2.*ATF(6,IA)*H(1)*H(2)
      ANITF = EXP(-A)
  100 RETURN

      END FUNCTION ANITF
!
!*****************************************************************************
!
      SUBROUTINE ASUNIT(H,HIN,N,M)
!
! *** ASUNIT by JCM 3 Jul 84 ***
!
!X
!C 1B
!H Produces reflection indices in the asymmetric unit, related to those given.
!P SYMUNI must have set up the asymmetric unit.
!A On entry H is a 3-sized vector containing h,k,l, which may be anywhere
!A in reciprocal space.
!A On exit HIN is a 3-sized vector related by symmetry to H, and in (or on)
!A             the asymmetric unit.
!A           M is its multiplicity.
!A           N is the number of the symmetry operator which takes H into
!A             HIN.  N is -ve if (-x,-y,-z) involved, or 0 if error.
!
!D Takes account of Friedel - for non-centrosymmetric groups, Friedel is
!D *** ONLY *** assumed if the user has given an I FRIE card with a non-zero
!D number.  If HIN is related to H by an operator belonging to the Friedel-
!D related set (which are not stored explicitly in  COMMON SYMDA), then N is
!D set negatively.
!D
!D If there has been an error in the formation of the asymmetric unit, and
!D H does not therefore transform into the unit using any of the space group
!D operators, N is set=0
!
      DIMENSION H(3), HIN(3)
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
!
! ARRANGE TO SCAN + AND - IF FRIEDEL:
      KEND = 1
      IF (FRIEDL) KEND = 2
      DO K = 1, KEND
        DO I = 1, NOPC
          CALL ROTSYM(H,HIN,I,2)
          IF (K.EQ.2) CALL GMREV(HIN,HIN,3,1)
          MM = MULBOX(HIN)
          IF (MM.NE.0) GOTO 101
        ENDDO
      ENDDO
! ERROR - REFLECTION NEVER TRANSFORMS INTO ASYMMETRIC UNIT:
      N = 0
      M = 0
      GOTO 100
! FOUND: SET N TO POINT TO OPERATOR, -VE IF IN FRIEDEL SET:
  101 N = (3-2*K)*I
      M = MM
  100 RETURN

      END SUBROUTINE ASUNIT
!
!*****************************************************************************
!
      SUBROUTINE ATOPOS
!
! ***  ATOPOS by JCM 15 Jul 83 ***
!
!X
!C 4A
!H Reads and interprets all given A cards.
!P PREFIN must have first read the Crystal Data File
!P SYMOP must have already input the space group symmetry.
!
!D Reads a sequence of A cards.  The fixed format for these is:
!D A1,1X,A4,4F10,1X,A4,F10 (but nowadays free format is used).
!D Each card should contain:
!D   An atom label of up to 4 characters, starting with a letter
!D   X,Y and Z coordinates (which may be given as fractions if appropriate,
!D   e.g.  1/4  or 2/3)
!D   an isotropic temperature factor
!D   a possible scattering factor label (if different from starting letters
!D                                       of atom label)
!D   a possible site occupation factor (set = 1 if read as 0, so that it can
!D   be omitted altogether if wished.
!D
!D These may be read in free format, with spaces terminating items, provided
!D that they do not extend to the right of their corresponding fixed format
!D fields.  The scattering factor label and site occupation factor are both
!D optional, which means that if anything occurs after the isotropic temperature
!D factor, it is a label if it starts with a letter, or a number if a digit.
!D
!D Calculates the numbers of atoms of each type in the unit cell, and if the
!D position is special records the generators of its subgroup.
!D
!D Keeps lists of atom labels and scattering factor labels.
!
      CHARACTER*4 LABA, LABS
      LOGICAL LATVEC
      DIMENSION XIN(3)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /FONAM / FONA(20,9), FONAME(20)
      CHARACTER*4 FONAME, FONA
      COMMON /FORMD2/ NBKF(20,9), NMFNM(9)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

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
      COMMON /POSNS2/ NATO(9)
      COMMON /SCRAT / NTORD(24), Z(6)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),  KOM22

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! ARRANGE INPUT OF "S" CARDS IF NOT DONE
      IF (INREAD(19).GT.0) THEN
        CALL SYMOP
        IF (IBMBER .NE. 0) RETURN
      ENDIF
! SET "A CARDS READ":
      INREAD(1) = -IABS(INREAD(1))
! INITIALISE NAME TABLES FOR ATOM AND SCATT NAMES:
      NUMANM = 0
      NUMFNM = 0
! NUMBER OF A CARDS:
      NACARD = ICDNO(1)
      NATOM = 0
      IF (NACARD.EQ.0) THEN
        CALL MESS(LPT,1,'No atomic positions have been read from Crystal Data File')
        RETURN
      ENDIF
      CALL MESS(LPT,1,'Atoms in the unit cell are')
      CALL MESS(LPT,0,' Mult Name       X         Y         Z        ITF      Site  Scat Fac Sub-Group')
!     NPOS=NUMBER OF GENERAL EQUIVALENT POSITIONS POSSIBLE
      NPOS = NLAT*NOP
! READ A CARDS ONE BY ONE:
      ID = IABS(INREAD(1))
      DO IAC = 1, NACARD
        CALL INPUTA(ID,LABA,LBALEN,LABS,LBSLEN,XIN,TIN,SIIN,IER)
        ID = ID + NYZ
! ADD ATOM NAME TO TABLE - THE NAME WILL NOT USUALLY BE THERE ALREADY,
! BUT WE DO NOT AT THIS STAGE MIND IF IT IS:
        N = LMATCH(LABA,ATNAME,NUMANM,150)
! WAS THIS AN SD CARD OR AN ORDINARY A CARD?
        IF (SDREAD) THEN
          CALL GMEQ(XIN,SDX(1,N),1,3)
          SDTF(N) = TIN
          SDSITE(N) = SIIN
!* LATER, PRINT
          GOTO 1
        ENDIF
! ATOMIC POSITON:
        CALL ERRCHK(2,NATOM,150,0,'atomic positions')
        IF (IBMBER .NE. 0) RETURN
        NATO(JPHASE) = NATOM
        CALL GMEQ(XIN,X(1,N),1,3)
        TF(N) = TIN
        SITE(N) = SIIN
! ADD SCATTERING FACTOR NAME TO TABLE, OR FIND IT THERE ALREADY:
        LKEEP = NUMFNM
        L = LMATCH(LABS,FONAME,NUMFNM,20)
        NFORMF(N) = L
! IF FIRST ATOM OF THIS FACTOR, KEEP A BACKWARDS POINTER ALSO (FOR LSQ)
        IF (L.GT.LKEEP) THEN
          NBKF(L,JPHASE) = N
          NBAKF(L) = N
          NMFNM(JPHASE) = L
        ENDIF
! DEAL WITH POSSIBLE SPECIAL POSITION:
        M = 0
!  OPERATE WITH EACH SYMMETRY ELEMENT IN TURN:
        ISGEN(1,N) = 1
        MC = 0
!  TEST CENTRE OF SYMMETRY
        IF (CENTRC) THEN
          DO I = 1, 3
            Z(I) = 2.*X(I,N)
          ENDDO
          IF (LATVEC(Z)) MC = 1
        ENDIF
!  NOW EACH SYMMETRY OPERATOR
        NTORD(1) = 1
        DO NO = 2, NOPC
          NTORD(NO) = 0
          CALL ROTSYM(X(1,N),Z,NO,1)
!  ADD TRANSLATIONAL PART OF SYMMETRY ELEMENT:
          CALL GMADD(Z,TRANS(1,NO),Z,1,3)
          ISIG = 1
          CALL GMSUB(X(1,N),Z,Z(4),1,3)
          IF (LATVEC(Z(4))) GOTO 6
! IF CENTROSYMMETRIC, TRY RELATED POSITION ALSO:
          IF (.NOT.CENTRC) GOTO 2
          ISIG = -1
          CALL GMADD(X(1,N),Z,Z(4),1,3)
          IF (.NOT.LATVEC(Z(4))) GOTO 2
    6     NTORD(NO) = IABS(NORD(NO))
          IF (NTORD(NO).GT.10) NTORD(NO) = NTORD(NO) - 100
          NTORD(NO) = ISIG*(NTORD(NO))
!  COUNT FOR ORDER OF SUBGROUP
          ISGEN(1,N) = ISGEN(1,N) + 1
    2   ENDDO
        M = NPOS/(ISGEN(1,N)*(1+MC))
        IF (MC.EQ.1) ISGEN(1,N) = -ISGEN(1,N)
!  GET GENERATORS OF SUBGROUP
        CALL GENELM(NTORD,ISGEN(1,N))
        IF (ISGEN(1,N).GT.0) THEN
          IF (ISGEN(2,N).EQ.1 .AND. ISGEN(3,N).EQ.0) THEN
            WRITE (LPT,2003) M, ATNAME(N), (X(I,N),I=1,3), TF(N), SITE(N), FONAME(L)
          ELSE
            WRITE (LPT,2003) M, ATNAME(N), (X(I,N),I=1,3), TF(N), SITE(N), FONAME(L), (ISGEN(I,N),I=2,3)
          ENDIF
        ELSE
          WRITE (LPT,2004) M, ATNAME(N), (X(I,N),I=1,3), TF(N), SITE(N), FONAME(L), (ISGEN(I,N),I=2,3)
 2004     FORMAT (1X,I4,2X,A4,1X,5F10.4,3X,A4,2I3,' -1')
        ENDIF
        AMULT(N) = FLOAT(M)/FLOAT(NOP)
    1 ENDDO
  100 RETURN
 2003 FORMAT (1X,I4,2X,A4,1X,5F10.4,3X,A4,2I3)
      END SUBROUTINE ATOPOS
!
!*****************************************************************************
!
      SUBROUTINE ATSPEC(N,K,CH)
!
! *** ATSPEC updated by JCM 12 Nov 89 ***
!
!X
!C 8C
!H Makes the 16-character specification of a symmetry related atom from
!H its packed specification.
!
!P Specification must have been packed by a call of NPACK
!
!A N on entry is the packed integer giving atom specification
!A K is a 6-sized integer array which on exit is filled as follows:
!A     K(1)=which atom number was the original
!A     K(2)=which symmetry operator gave current position, -ve if also
!A          needed (-x,-y,-z)
!A     K(3)=which lattice translation
!A     K(4)=which cell in x direction (-1, 0 or +1)
!A     K(5)=which cell in y direction (-1, 0 or +1)
!A     K(6)=which cell in z direction (-1, 0 or +1)
!A CH is a A16 string; on exit it holds a printable representation of K
!
      CHARACTER*16 CH
      DIMENSION K(6), IDIG(5)
      COMMON /ATNMPK/ ATPACK(10,3)
      INTEGER ATPACK

      CALL NPACK(N,K,6,2,ATPACK)
      CH = ' '
      IF (K(2).NE.1) THEN
        IF (K(2).LT.0) CH(1:1) = '-'
        CH(2:2) = 'S'
        CALL INTDIG(K(2),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(3:3),2,0)
      ENDIF
      IF (K(3).NE.1) THEN
        CH(6:6) = 'L'
        CALL INTDIG(K(3),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(7:7),1,0)
      ENDIF
      IF (IABS(K(4))+IABS(K(5))+IABS(K(6)).NE.0) THEN
        CH(9:9) = '('
        IF (K(4).LT.0) CH(10:10) = '-'
        CALL INTDIG(K(4),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(11:11),1,0)
        IF (K(5).LT.0) CH(12:12) = '-'
        CALL INTDIG(K(5),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(13:13),1,0)
        IF (K(6).LT.0) CH(14:14) = '-'
        CALL INTDIG(K(6),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(15:15),1,0)
        CH(16:16) = ')'
      ENDIF

      END SUBROUTINE ATSPEC
!
!*****************************************************************************
!
      SUBROUTINE AXIS(R,A)
!
! *** AXIS by PJB/JCM 28 Jun 83 ***
!
!X
!C 1C
!H Finds the axis of a given rotation matrix.
!A On entry R is a 3x3 rotation matrix
!A On exit  A is a 1x3 vector holding its axis
!
      DIMENSION R(3,3), S(3,3), A(3)

! COPY R TO LOCAL S:
      CALL GMEQ(R,S,3,3)
! SET D= + OR -1 AS DETERMINANT OF R (AN EIGENVALUE):
      D = 1.
      IF (DETER3(S).LT.0.) D = -1.
! FORM R MINUS D*(UNIT MATRIX):
      DO I = 1, 3
        S(I,I) = S(I,I) - D
      ENDDO
! I,J,K COUNT CYCLICALLY OVER 1,2,3:
      J = 2
      K = 3
      DO I = 1, 3
        A(I) = S(J,J)*S(K,K) - S(J,K)*S(K,J)
        IF (ABS(A(I)).LT.10.E-4) GOTO 2
! A(I) HAS "DIAGONAL" ELEMENT OF VECTOR OF COFACTORS - IF NON-ZERO,
! VECTOR IS ACCEPTABLE, BUT IF ZERO GO ON TO LOOK AT NEXT
        A(J) = -S(J,I)*S(K,K) + S(K,I)*S(J,K)
        A(K) = -S(J,J)*S(K,I) + S(K,J)*S(J,I)
        GOTO 100
    2   J = K
        K = I
      ENDDO
  100 CALL FCTOR(A,N)

      END SUBROUTINE AXIS
!
!*****************************************************************************
!
      LOGICAL FUNCTION BINDIG(N,NBIN)
!
! *** BINDIG by JCM 13 Nov 91 ***
!
!X
!C 11C
!H Tests for the presence of a given binary digit within an integer.
!A On entry N is the integer to be tested
!A          NBIN is a binary digit (in decimal form, e.g. 1, 2, 4, 8 etc)
!A On exit BINDIG is TRUE if N contains NBIN, FALSE if not.
!N NBIN is not checked;  if it is not pure binary there will be strange results.
!
      LOGICAL EVEN

      IF (NBIN.NE.0) THEN
        CALL PARITY(IABS(N/NBIN),I,EVEN)
        BINDIG = .NOT.EVEN
      ELSE
        BINDIG = .TRUE.
      ENDIF

      END FUNCTION BINDIG
!
!*****************************************************************************
!
      SUBROUTINE BONCOS(B1,B2,B3,ANGLE,COSTH,SINTH,DADB)
!
! *** BONCOS by JCM 18 Oct 90 ***
!
!X
!C 8B
!H Given 3 bonds forming a triangle, calculates the angle opposite the first,
!H its sine and cosine, and its derivatives wrt all 3 bonds.
!A On entry B1, B2, B3 are the values of the bonds (not their pointers)
!A On exit ANGLE is the angle opposite B1 in radians
!A         COSTH is its sine
!A         SINTH is its cosine
!A         DADB is a 1x3 array holding the derivatives of ANGLE wrt B1,B2,B3
!
      DIMENSION DADB(3)

      COSTH = (B2*B2+B3*B3-B1*B1)/(2.*B2*B3)
      CALL SINCOS(COSTH,SINTH,'BONCOS')
! ANGLE IN RADIANS:
      ANGLE = (ACOS(COSTH))
! DERIVATIVES OF COS THETA WRT B1, THEN B2, THEN B3
      DADB(1) = -B1/(B2*B3)
      DADB(2) = 1./B3 - COSTH/B2
      DADB(3) = 1./B2 - COSTH/B3
! CONVERT TO BE DERIVATIVES OF THETA RADIANS WRT BONDS:
      DO I = 1, 3
        DADB(I) = (-DADB(I)/SINTH)
      ENDDO

      END SUBROUTINE BONCOS
!
!*****************************************************************************
!
      FUNCTION BONDA(I1,I2,I3)
!
! *** BONDA by JCM 1 Oct 86 ***
!
!X
!C 8B
!H Calculates the angle between two bonds.
!A On entry I1 is the serial number of one atom in list in /POSNS/
!A          I2 is the serial number of the central atom
!A          I3 is the serial number of the third atom
!A On exit BONDA is the angle between the bonds, in degrees
!D Calculates the angle between bonds joining atoms I1-I2 and I2-I3.
!D Does no symmetry operations at all.
!N It would probably be useful also to have a routine which accepts as
!N arguments X1(1:3), X2(1:3) and X3(1:3), position coordinates.
!
      DIMENSION D12(3), D23(3)
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

      DO I = 1, 3
        D12(I) = X(I,I1) - X(I,I2)
        D23(I) = X(I,I3) - X(I,I2)
      ENDDO
      B12 = VCTMOD(1.,D12,1)
      B23 = VCTMOD(1.,D23,1)
      COSTH = SCLPRD(D12,D23,1)/(B12*B23)
      CALL SINCOS(COSTH,SINTH,'BONDA')
      BONDA = DEGREE(ATAN2(SINTH,COSTH))

      END FUNCTION BONDA
!
!*****************************************************************************
!
      SUBROUTINE BONDER(N)
!
! *** BONDER updated by JCM 22 Oct 90 ***
!
!X
!C 8B
!H Calculate a bond and its derivatives for slack constraints.
!A On entry N points to which bond of the list in /SLKGEO is wanted
!D On exit, in /SLKGEO, BCALC(N) = bond length
!D Also sets up the 12 derivatives (whether needed or not) in /SLKGEO
!D        DERPOS(3,N,2)=derivatives of BCALC wrt x,y,z (in original
!D                      atom lists), both ends.
!D        DERCEL(6,N)  =derivatives of BCALC wrt A*, B*, C* etc
!D                      the cell quadratic products in reciprocal space
!
      DIMENSION DX(3), TEMP(3), TEMP1(6)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

! N IS THE BOND - SET N1 & N2 TO POINT TO ITS END ATOMS:
      N1 = IATM(N,1)
      N2 = IATM(N,2)
! DIFFERENCE VECTOR - DIFFERENCES BETWEEN ACTUAL COORDS OF ENDS:
      CALL GMSUB(XSLAK(1,N1),XSLAK(1,N2),DX,1,3)
! BOND:
      BCALC(N) = VCTMOD(1.,DX,1)
! DERIVATIVES WRT 6 CELL QUADRATIC PRODUCTS, FIRST IN REAL SPACE:
      J = 2
      K = 3
      DO I = 1, 3
        DERCEL(I,N) = DX(I)*DX(I)/(2.*BCALC(N))
        DERCEL(I+3,N) = DX(J)*DX(K)/BCALC(N)
        J = K
        K = I
      ENDDO
! THEN CONVERTED TO RECIPROCAL:
      CALL MultiplyMatrices(TOSTAR,DERCEL(1,N),TEMP1,6,6,1)
      CALL GMEQ(TEMP1,DERCEL(1,N),1,6)
! DERIVATIVES WRT GIVEN COORDS:
! INTO TEMP1 PUT a(a*DX + b cos gamma *DY + c cos beta *DZ)
!                b(a cos gamma *DX + b*DY + c cos alpha *DZ)
!                c(a cos beta *DX + b cos alpha *DY + c*DZ)
! WHICH IS Bond*(dBond/dX1, dBond/dY1, dBond/dZ1) OR
! ALSO    -Bond*(dBond/dX2, dBond/dY2, dBond/dZ2)
!
! VECTOR a*DX, b*DY, c*DZ:
      DO I = 1, 3
        TEMP(I) = CELL(I,1,1)*DX(I)
      ENDDO
! CONVERT USING COSINE MATRIX:
      CALL MultiplyMatrices(COSIN,TEMP,TEMP1,3,3,1)
      DO I = 1, 3
        TEMP1(I) = TEMP1(I)*CELL(I,1,1)
      ENDDO
! CONVERT DERIVS WRT ACTUAL COORDS TO THOSE WRT PARAMETERS OF REFINEMENT
      CALL ROTSYM(TEMP1,DERPOS(1,N,1),IABS(ISYM(N1)),1)
! IF CENTRE INVOLVED, REVERSE:
      IF (ISYM(N1).LT.0) CALL GMREV(DERPOS(1,N,1),DERPOS(1,N,1),1,3)
! AND FOR SECOND ATOM, REVERSING SIGNS:
      CALL ROTSYM(TEMP1,DERPOS(1,N,2),IABS(ISYM(N2)),1)
      IF (ISYM(N2).GT.0) CALL GMREV(DERPOS(1,N,2),DERPOS(1,N,2),1,3)
! FINALLY DIVIDE BY THE BOND, AS THE EXPRESSION WE HAVE DIFFERENTIATED IS
! BOND SQUARED, AND WE HAVE CANCELLED A 2:
      DO I = 1, 3
        DO J = 1, 2
          DERPOS(I,N,J) = DERPOS(I,N,J)/BCALC(N)
        ENDDO
      ENDDO

      END SUBROUTINE BONDER
!
!*****************************************************************************
!
      SUBROUTINE BONTRI(N1,N2,N3,K,IE)
!
! *** BONTRI by JCM 17 Oct 1990 ***
!
!X
!C 8B
!H Given two bonds with a common atom, completes the triangle and identifies
!H the atom.
!A On entry N1 and N2 are the pointers to 2 bonds (in the list generated by
!A          L ATOM and L BOND cards)
!A On exit N3 points to the third bond completing the triangle
!A         K points to the common atom, opposite N3 in the triangle.
!A         and N1 and N2 are possibly interchanged; the result is N1 < N2.
!A         IE is an error indicator, set = 0 if OK, non-zero if error.
!
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      IE = 0
! FIND THIRD BOND:
      IF (N1.GT.N2) CALL FLIP(N2,N1)
      I1 = 0
      DO I = 1, 2
        DO J = 1, 2
          IF (IATM(N1,I).EQ.IATM(N2,J)) THEN
            K = IATM(N1,I)
            I1 = 3 - I
            I2 = 3 - J
          ENDIF
        ENDDO
      ENDDO
      IF (I1.EQ.0) THEN
        WRITE (LPT,3031) BONNAM(N1), BONNAM(N2)
        IE = 1
        GOTO 100
      ENDIF
! ADD THIRD SIDE OF TRIANGLE TO LIST OF INVOLVED BONDS;  IF IT IS NOT
! ALREADY THERE IT WILL BE GIVEN A BOGUS NAME:
      CALL ADDBON('    ',IATM(N1,I1),IATM(N2,I2),N3)
  100 RETURN
 3031 FORMAT (/'  ERROR ** bonds ',A4,' and ',A4,' have no common atom')

      END SUBROUTINE BONTRI
!
!*****************************************************************************
!
      SUBROUTINE CARDIN(IDEN)
!
! *** CARDIN by JCM 2 Feb 88 ***
!
!X
!C 13C
!H Finds the record number IDEN in the DIRECT ACCESS file on unit IO10,
!H which is a copy of the Crystal Data File.
!A On entry IDEN holds the number of the required record.
!
!D Reads card in format A80 to ICARD, leaving this to be interpreted elsewhere.
!D Ignores Y and Z cards, which were not counted when PREFIN read the data.
!D Counts those it ignores in NYZ, and so that calling routines know if last
!D card is Y or Z, sets NYZ=-1 in those cases.
!D
!D Complains if record IDEN is not present.
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      NYZ = 0
! CHECK NOT TRYING TO READ SILLY RECORD:
      ILOW = 0
      IF (JPHASE.GT.1) ILOW = NTOTAL(JPHASE-1)
      IHI = NTOTAL(JPHASE)
      IF (IDEN.LE.ILOW .OR. IDEN.GT.IHI) THEN
        CALL ERRIN2(IDEN,0,'record ',' requested from crystal data is not there')!
        IF (IBMBER .NE. 0) RETURN
      ENDIF
! CATCH ALSO CDFS ENDING IN Y OR Z CARDS:
    1 IF (IDEN+NYZ.GT.IHI) GOTO 101
      READ (IO10,REC=IDEN+NYZ,FMT=1001) ICARD
 1001 FORMAT (A80)
      NYZ = NYZ + 1
      IF (ICARD(1:1).EQ.'Y' .OR. ICARD(1:1).EQ.'Z') GOTO 1
      GOTO 100
  101 NYZ = -1
  100 RETURN

      END SUBROUTINE CARDIN
!
!*****************************************************************************
!
      SUBROUTINE CDFIN(NUMCDF,ID,ENDIP)
!
! *** CDFIN updated by JCM 21 Jan 92 ***
!
!X
!C 13C
!H Reads in one Crystal Data File and copies it to direct access unit number
!H IO10, starting at record number ID.
!A  On entry NUMCDF = which crystal data file is required;  1 in MK3
!A        ID is the previously used record of DIRECT ACCESS file IO10
!A        in COMMON /CARDRC/ ICRYDA=unit from which to read
!A On exit  ID has been advanced and gives total records on IO10
!A        ENDIP is TRUE if the end of the file hs been reached.
!D Writes to /CARDRC/ the following:
!D        NTOTAL(NUMCDF)=number of records for phase NUMCDF
!D        INREA(26,NUMCDF)=absolute starting addresses for letters
!D        ICDN(26,NUMCDF)=numbers of cards starting with letters
!D Also detects presence of I OUTP item, and unless a non-zero value is
!D already in IOUT in /IOUNIT/, interprets the integer after the I OUTP.
!D
!D Also detects if it is reading phase 1 of a multiphase job, because it must
!D form the union of this with "phase 0"
!D
!O Writes Y cards straight to unit LPT.
!I Reads crystal data sets from unit ICRYDA.
!O Lists on LPT numbers of each type of card read.
!N Modified to ignore empty cdfs (e.g. if the file inadvertently starts with
!N a non-letter)
!
      CHARACTER*4 MWORD
      CHARACTER*1 ISS
      CHARACTER*10 FILNOM
      LOGICAL GETM, ENDIP

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))

      CHARACTER*1     LETUP,     LETLOW,     ISPCE, IDIGIT,     ISMBOL
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)

      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      COMMON /PHAS0 / INRLP0, ICDLP0, INRLP1, ICDLP1, NCDF0

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IDIN = ID
      ENDIP = .FALSE.
      N1 = NUMCDF
! UNLESS PHASE 1 FOLLOWING PHASE 0, FOR EACH LETTER, SET NO CARDS YET READ:
      IF (.NOT.MULFAS .OR. (NPHASE.NE.1)) THEN
        DO I = 1, 26
          ICDN(I,N1) = 0
          INREA(I,N1) = 1
        ENDDO
      ELSE
! CLEAR ONLY THE COUNT OF L CARDS:
        ICDN(12,N1) = 0
        INREA(12,N1) = 1
! AND NOTE THE END OF PHASE 0, FOR THE NEW CDF:
        NCDF0 = NTOTAL(1)
      ENDIF
! SET HAVE NOT READ CARD STARTING "M GET" ASKING TO UNDUMP PREVIOUS MAP:
      GETM = .FALSE.
! THE DATASET IS COPIED ACROSS WITH Y AND Z CARDS,
! AND THE STARTING LINE NUMBER FOR EACH LETTER IS RECORDED IN INREA.
! ICDN SAYS HOW MANY CARDS OF THAT LETTER HAVE BEEN READ:
! INLET HOLDS SERIAL NUMBER OF PREVIOUS STORED CARD'S LETTER:
      INLET = -999
    8 READ (ICRYDA,1000,END=2) ICARD
! IGNORE BLANK LINES ANYWHERE:
      LEN = LENGT(ICARD)
      IF (LEN.EQ.0) GOTO 8
      L = LETTER(ICARD(1:1))
! THIS GIVES AN ANSWER IN RANGE 1-26, BUT ACCEPTS LOWER CASE
      IF (L.EQ.0) GOTO 12
! LETTER RECOGNISED:
      IF (L.NE.25) GOTO 3
! Y CARDS COPIED ON TO OUTPUT:
      WRITE (LPT,2004) (ICARD(J:J),J=2,LEN)
 2004 FORMAT (' ',79A1)
      GOTO 7
! Z CARDS TOTALLY IGNORED:
    3 IF (L.EQ.26) GOTO 7
! CARDS OTHER THAN Y AND Z:
! JUMP IF SAME INITIAL LETTER AS PREVIOUSLY STORED CARD:
      IF (INLET.EQ.L) GOTO 4
! NEW BLOCK - CHECK NOT HAD ANY OF THIS LETTER BEFORE:
      IF (ICDN(L,N1).NE.0) GOTO 6
      INREA(L,N1) = ID + 1
      INLET = L
! JOIN HERE IN MIDDLE OF BLOCK:
    4 ICDN(L,N1) = ICDN(L,N1) + 1
      IF (L.EQ.13) THEN
        CALL RDWORD(MWORD,ITEMP,3,IPT,80,0,IER)
        GETM = (MWORD.EQ.'GET')
      ENDIF
    7 ID = ID + 1
      WRITE (IO10,REC=ID,FMT=1000) ICARD
      GOTO 8
! ERROR ON SHUFFLED CARDS:
    6 CALL ERRCH2(ICARD(1:1),0,'more than one group of cards labelled ',' found on crystal data file')
      IF (IBMBER .NE. 0) RETURN
! END OF CRYSTAL DATA:
    2 ENDIP = .TRUE.
   12 IF (.NOT.ENDIP .AND. ID.EQ.IDIN) GOTO 8
      IF (GETM) CALL MAJUST
      IF (N1.EQ.1) CALL MESS(LPT,1,'Data read by PREFIN from file '//FILNOM(ICRYDA))
      IF (N1.GT.1) CALL MESS(LPT,1,'Next phase data read by PREFIN from file '//FILNOM(ICRYDA))
      DO I = 1, 26
        IF (ICDN(I,N1).EQ.0) GOTO 5
        ISS = ' '
        IF (ICDN(I,N1).GT.1) ISS = 's'
        WRITE (LPT,2001) ICDN(I,N1), ISS, LETUP(I)
 2001   FORMAT (5X,I4,' card',A1,'  labelled ',A1)
    5 ENDDO
      NTOTAL(N1) = ID
      RETURN
 1000 FORMAT (A80)

      END SUBROUTINE CDFIN
!
!*****************************************************************************
!
      SUBROUTINE CDSCAN(CH,WORDS,LEN,K,LCD,NW)
!
! *** CDSCAN updated by JCM 2 Feb 88 ***
!
!X
!C 13C
!H Finds the next card which starts with the letter given in CH, and has
!H then a word which is one of the collection given in WORDS.
!A On entry CH is the A1 character to start the card
!A          WORDS is an A4 array of length LEN with possible next words
!A          K is 0 if to search from start, otherwise where to start
!A           - i.e. we search from the K+1th card of the crystal data
!A On exit  LCD = which card, if found
!A             = -1 if none found starting CH
!A             = 0 if some start CH, but none of WORDS come next
!A          NW = which word of WORDS was found
!D sets a copy of the found card into ICARD
!
      CHARACTER*1 CH
      CHARACTER*4 WORDS(LEN), WORD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
      L = -1
      I = LETTER(CH)
! IF NO CH CARDS AT ALL, EXIT:
      IF (ICDNO(I).EQ.0) GOTO 101
      ID = K + 1
      IF (K.EQ.0) ID = IABS(INREAD(I))
! READ NEXT CARD:
    1 IF (ID.GT.NTOTAL(JPHASE)) GOTO 102
      CALL CARDIN(ID)
!  WE HAVE TO DO SOMETHING ABOUT CDFS ENFDING Y OR Z
      IF (NYZ.EQ.-1) GOTO 102
      ID = ID + NYZ
      L = ID - 1
! CHECK WE STILL HAVE CH CARDS:
      IF (ICARD(1:1).NE.CH) GOTO 102
! READ WORD STARTING AT POSITION 3:
      CALL RDWORD(WORD,J,3,IPT,80,0,IER)
! SCAN POSSIBLE VOCABULARY OF NEXT WORDS:
      DO NW = 1, LEN
        IF (WORD.EQ.WORDS(NW)) GOTO 101
      ENDDO
! "WORD" ON THIS CARD DOES NOT MATCH - TRY NEXT "CH" CARD:
      GOTO 1
  102 L = 0
  101 LCD = L

      END SUBROUTINE CDSCAN
!
!*****************************************************************************
!
      SUBROUTINE CELDER(H,DERS)
!
! *** CELDER updated by JCM  7 Sep 88 ***
!
!X
!C 6B
!H From h,k,l calculates d* squared and its derivatives, and sets SSQRD.
!A On entry H is a real 1x3 vector holding h,k,l
!A On exit DERS is a real 1x6 array holding the derivatives of d* squared
!A              wrt A*, B*, C*, D*, E*, F*
!P CPARS in /CELPAR/ must contain A*, B*, C*, D*, E*, F*
!D Also sets SSQRD in /BRAGG/ to be s squared (=d* squared/4)
!D STHL in /BRAGG/ to be sin theta/lambda, s, or d*/2
!D and DSTAR2 in /BRAGG/ to be d* squared
!
      DIMENSION H(3), DERS(6)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      DSDS = 0.0
      J = 2
      K = 3
      DO I = 1, 3
        DERS(I) = H(I)*H(I)
        DERS(I+3) = 2.*H(J)*H(K)
        DSDS = DSDS + DERS(I)*CPARS(I,2) + DERS(I+3)*CPARS(I+3,2)
        J = K
        K = I
      ENDDO
      SSQRD = DSDS/4.
      DSTAR2 = DSDS
      STHL = SQRT(SSQRD)

      END SUBROUTINE CELDER
!
!*****************************************************************************
!
      SUBROUTINE CELMAT(TOSTAR)
!
! *** CELMAT by JCM 17 Aug 89 ***
!
!X
!C 1B
!H Sets up the matrix to convert derivatives wrt A,B,C . . (cell quadratic
!H products in real space) to derivatives wrt A*, B*, C*, . . in
!H reciprocal space.
!A On exit TOSTAR is the required 6x6 matrix
!P On entry CPARS(1:6,1) contain the real space cell quadratic products,
!P A=a sqrd, B=b sqrd, C=c sqrd D=b c cos alpha, etc
!
      DIMENSION TOSTAR(6,6)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
!
! FIRST COPY OUT A B C D E F FOR SANITY:
      A = CPARS(1,1)
      B = CPARS(2,1)
      C = CPARS(3,1)
      D = CPARS(4,1)
      E = CPARS(5,1)
      F = CPARS(6,1)
! THERE WILL BE CLEVERER WAYS OF DOING THIS, BUT LETS GET IT RIGHT FIRST:
      TOSTAR(1,1) = -A*A
      TOSTAR(1,2) = -F*F
      TOSTAR(1,3) = -E*E
      TOSTAR(1,4) = -F*E
      TOSTAR(1,5) = -E*A
      TOSTAR(1,6) = -A*F
      TOSTAR(2,1) = -F*F
      TOSTAR(2,2) = -B*B
      TOSTAR(2,3) = -D*D
      TOSTAR(2,4) = -B*D
      TOSTAR(2,5) = -D*F
      TOSTAR(2,6) = -F*B
      TOSTAR(3,1) = -E*E
      TOSTAR(3,2) = -D*D
      TOSTAR(3,3) = -C*C
      TOSTAR(3,4) = -D*C
      TOSTAR(3,5) = -C*E
      TOSTAR(3,6) = -E*D
!
      TOSTAR(4,1) = -2.*E*F
      TOSTAR(4,2) = -2.*B*D
      TOSTAR(4,3) = -2.*D*C
      TOSTAR(4,4) = -B*C - D*D
      TOSTAR(4,5) = -C*F - D*E
      TOSTAR(4,6) = -B*E - D*F
      TOSTAR(5,1) = -2.*A*E
      TOSTAR(5,2) = -2.*D*F
      TOSTAR(5,3) = -2.*C*E
      TOSTAR(5,4) = -C*F - D*E
      TOSTAR(5,5) = -A*C - E*E
      TOSTAR(5,6) = -A*D - E*F
      TOSTAR(6,1) = -2.*A*F
      TOSTAR(6,2) = -2.*B*F
      TOSTAR(6,3) = -2.*D*E
      TOSTAR(6,4) = -B*E - D*F
      TOSTAR(6,5) = -A*D - E*F
      TOSTAR(6,6) = -A*B - F*F

      END SUBROUTINE CELMAT
!
!*****************************************************************************
!
      SUBROUTINE CELNEW
!
! *** CELNEW updated by PJB 14-Dec-94 ***
!
!X
!C 6C
!H Writes out a new C card after cell parameter refinement.
!P CELL in /CELPAR/ should contain a,b,c, cos alpha, beta, gamma
!P CELESD  in /CELPAR/ should contain their esds
!O Writes to unit NEWIN a new C card
!N Does not preserve any blanks originally left for symmetry.
!
      DIMENSION ANG(3)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

! MAY BE SD CARD:
      IF (ICARD(3:4).EQ.'SD') THEN
        WRITE (NEWIN,2001) (CELESD(I,I,1),I=1,6)
 2001   FORMAT ('C SD',3F10.5,3F10.3)
        GOTO 100
      ENDIF
      DO I = 1, 3
        ANG(I) = DEGREE(ACOS(CELL(I,2,1)))
      ENDDO
      WRITE (NEWIN,2000) (CELL(I,1,1),I=1,3), (ANG(I),I=1,3)
 2000 FORMAT ('C ',3F10.5,3F10.3)
  100 RETURN

      END SUBROUTINE CELNEW
!
!*****************************************************************************
!
      SUBROUTINE CELREL(IFAM,IGEN,ISPC)
!
! *** CELREL updated by JCM 2 May 90 ***
!
!X
!C 6A
!H Transfers any  relations which exist between cell parameters from
!H their own COMMON to the general "contraint/fixing" COMMON.
!A On entry IFAM, IGEN, ISPC designate the first parameter, A*
!D Moves the constraint and/or fixing information from /CELFIX/ to
!D join the general initial fix/constrain info.
!D Also keep starting family, genus, species for later consultation in
!D NCELF,NCELG,NCELS
!
!N Cell parameters are source-independent (KSOURC=1) but may be phase dependent
!N B*, C* etc are assumed to have ISPC going sequentially up in 1's from A*
!
      DIMENSION NCOUNT(6)
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)


! SET UP LIST OF 6 KK VALUES:
      DO I = 1, 6
        NCOUNT(I) = KPAK(IFAM,IGEN,ISPC+I-1,JPHASE,1)
      ENDDO
      CALL FIXREL(6,IPTCEL,AMCELL,NCOUNT,5)
      NCELF = IFAM
      NCELG = IGEN
      NCELS = ISPC
      END SUBROUTINE CELREL
!
!*****************************************************************************
!
      SUBROUTINE CELSDP(ALSQ,MATSZ)
!
! *** CELSDP by JCM 17 Aug 89 ***
!
!X
!C 6C
!H Prints the esds of real cell parameters after a cycle of refinement.
!A ALSQ, MATSZ need to be handed around in routine calls
!P ALSQ must hold the inverse LSQ matrix
!P CPARS holds A,B,C,D,E,F, the real cell quadratic products, and
!P CELL the result of reading a C card.
!O If a variance is negative, writes 0.
!
      DIMENSION ALSQ(MATSZ)
      CHARACTER*1 SI(3)
      CHARACTER*5 AN(3)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      DATA SI/'a', 'b', 'c'/
      DATA AN/'alpha', ' beta', 'gamma'/

      CALL MATCEL(ALSQ,MATSZ)
      CALL MESS(LPT,2,' ************* LATTICE CONSTANTS *************')
      CALL NEWLIN(LPT)
      DO I = 1, 6
        C = SQRT(AMAX1(CELLSD(I,I),0.))
        IF (I.LT.4) WRITE (LPT,2001) SI(I), CELL(I,1,1), C
 2001   FORMAT ('        ',A1,' = ',F8.5,' +/- ',F6.5)
        IF (I.GE.4) WRITE (LPT,2002) AN(I-3),DEGREE(ACOS(CELL(I-3,2,1))), C
 2002   FORMAT ('    ',A5,' = ',F8.4,' +/- ',F6.4)
      ENDDO

      END SUBROUTINE CELSDP
!
!*****************************************************************************
!
      SUBROUTINE CELSHF(N)
!
! *** CELSHF updated by JCM 10 Feb 87 ***
!
!X
!C 6C
!H Applies a shift to a cell quadratic product.
!A N= which parameter; 1=A*, 2=B*, 3=C* etc
!
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
!
      CALL ADJUST(CPARS(N,2))
      GOTO 100
! TO SET ALL CELL PARAMETERS FIXED, OR VARY ONE:
      ENTRY CELVAR(N,NV)
      IF (N.EQ.0) THEN
        DO I = 1, 6
          KCPARS(I) = 0
        ENDDO
      ELSE
        KCPARS(N) = NV
      ENDIF
  100 RETURN

      END SUBROUTINE CELSHF
!
!*****************************************************************************
!
      SUBROUTINE CENTRE(LUNIT,N,TXT,NWIDE)
!
! *** CENTRE by JCM 12 Sep 92 ***
!
!X
!C 13C
!H Writes on unit LUNIT the given message, centred & preceded by N empty lines.
!A On entry TXT is a CHARACTER variable holding the message,
!A          N is in integer requesting N empty lines before the message, and
!A            may be 0, for no lines
!A            or > 98, for a page throw.
!A          LUNIT is the unit on which to write.
!A          NWIDE is the width of page in which the text is to be centred.
!O Writes to unit LUNIT N empty lines or a page throw, then the given text with
!O a "space" carriage control, centred within NWIDE spaces.
!
      CHARACTER*(*) TXT

      IF (N.LT.99) THEN
        DO I = 1, N
          WRITE (LUNIT,2000)
 2000     FORMAT (1X)
        ENDDO
      ELSE
        WRITE (LUNIT,2002)
 2002   FORMAT ('1')
      ENDIF
      L = LENGT(TXT)
      M = 0
! THIS SHOULD ROUND BY MOVING THE TEXT 1 PLACE LEFTWARDS:
      IF (L.LT.NWIDE) M = (NWIDE-L)/2
      WRITE (LUNIT,2001) (' ',I=1,M), (TXT(I:I),I=1,L)
 2001 FORMAT (1X,200A1)

      END SUBROUTINE CENTRE
!
!*****************************************************************************
!
      SUBROUTINE CLOFIL(LUN)
!
! *** CLOFIL by PJB Jan 86 ***
!
!X
!C 13C
!H Closes the FORTRAN unit LUN and returns the CCSL unit to the pool.
!A On entry LUN is the number of an existing FORTRAN unit, now finished with.
!P NOPFIL (or OPNFIL) should have set up LUN when the file was opened.
!D Closes LUN;  releases its table entries IOTAB in /LOONEY and FILNAM in
!D /FINAME.
!D
!D If LUN was not in such table entries, does nothing.
!
      COMMON /FINAME/ FILNAM(15)
      CHARACTER*10 FILNAM
      COMMON /LOONEY/ IOTAB(15), LUNTAB(15)

      CLOSE (LUN)
      DO I = 1, 15
        IF (LUNTAB(I).EQ.LUN) THEN
          IOTAB(I) = 0
          FILNAM(I) = ' '
          RETURN
        ENDIF
      ENDDO

      END SUBROUTINE CLOFIL
!
!*****************************************************************************
!
      FUNCTION CONATF(N,IA)
!
! *** CONATF by JCM 16 Nov 84 ***
!
!X
!C 4B
!H Produces for a single coefficient of an anisotropic temperature
!H factor, its conversion factor from the internally used betas, in
!H order to communicate with the user.
!A On entry IA says which ATF
!A           N says which of the coefficients, in the program's ordering
!A             11 22 33 23 13 12
!A On exit CONATF is the required muliplicative conversion factor.
!N It is inefficient, but not often used.
!
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      I = IATYP(IA)
      GOTO (2,2,3,4,5), I
! BRANCH ON TYPE OF ATF GIVEN BY USER - THESE TYPES ARE AS IN MK2, BUT 1 WAS
! FOUND TO BE BAD AND WAS REMOVED.
!
!  TYPE 2 - AS IN HEWAT PROFILE REFINEMENT -  NO COSINES
    2 FAC = 4.
      GOTO 30
!  TYPE 3 U'S AS IN EXP-2*PI*PI(ETC)
    3 FAC = 1./(TWOPI*PI)
   30 GOTO (21,21,21,24,25,26), N
   21 C = FAC/CPARS(N,2)
      GOTO 101
   24 C = FAC/(CELL(2,1,2)*CELL(3,1,2))
      GOTO 101
   25 C = FAC/(CELL(1,1,2)*CELL(3,1,2))
      GOTO 101
   26 C = FAC/(CELL(1,1,2)*CELL(2,1,2))
      GOTO 101
!  TYPE 4 - ONLY THE 2'S MISSING
    4 GOTO (41,41,41,42,42,42), N
   41 C = 1.0
      GOTO 101
   42 C = 2.0
      GOTO 101
    5 C = 1.0
  101 CONATF = C

      END FUNCTION CONATF
!
!*****************************************************************************
!
      SUBROUTINE DEPRIN(IPRNT)
!
! *** DEPRIN by JCM 11 Feb 80 ***
!
!X
!C 6C
!H Decodes an integer which describes the frequency of LSQ printing
!H required, and outputs this frequency.
!A On entry IPRNT = 0 for 'never'
!A                  1 for 'first cycle'
!A                  2 for 'last cycle'
!A                  3 for 'first and last cycles'
!A                  4 for 'every cycle'
!P An explanatory message is assumed to have been previously output
!D Interprets IPRNT as read from, usually, I card under such headings as
!D    'PRIN', 'PRFC', 'PRSK' etc, depending on the calling program
!O Outputs suitable message
!
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
!
      IF (IPRNT.EQ.1) THEN
        CALL MESS(LPT,0,'first cycle')
      ELSEIF (IPRNT.EQ.2) THEN
        CALL MESS(LPT,0,'last cycle')
      ELSEIF (IPRNT.EQ.3) THEN
        CALL MESS(LPT,0,'first and last cycles')
      ELSEIF (IPRNT.EQ.4) THEN
        CALL MESS(LPT,0,'every cycle')
      ELSE
        CALL MESS(LPT,0,'never')
      ENDIF

      END SUBROUTINE DEPRIN
!
!*****************************************************************************
!
      SUBROUTINE DUMMY(DUM)
!
! *** DUMMY by JCM 21 Mar 89 ***
!
!C 16C
!H Does absolutely nothing;  used as a default in routine calls.
!D The only way we can avoid unnecessary routines being loaded is to
!D pass their names through the system as arguments of other routines.
!D Sometimes we wish to set such arguments to avoid doing anything.
!
      RETURN
      END SUBROUTINE DUMMY
!
!*****************************************************************************
!
      FUNCTION ELEMAT(ALSQ,MATSZ,I,J)
!
! *** ELEMAT by JCM 16 Jul 87 ***
!
!X
!C 6C
!H Gets a matrix element from the triangular LSQ matrix.
!A ALSQ holds the symmetrical triangular LSQ matrix
!A MATSZ is its dimension
!A I,J ask for the particular element, as though ALSQ were square
!A ELEMAT is set on exit to the element I,J
!N ALSQ and MATSZ are passed through the whole of the LSQ system as arguments,
!N enabling MATSZ to be set and ALSQ to be dimensioned in MAIN programs.
!
      INCLUDE 'PARAMS.INC'

      DIMENSION ALSQ(MATSZ)

      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      IF ((I .GT. MaxBVar) .OR. (J .GT. MaxBVar)) CALL DebugErrorMessage('Index into ALSQ out of range in ELEMAT')
      IF (J .LT. I) THEN
        IND = MM(J) + I
      ELSE
        IND = MM(I) + J
      ENDIF
      ELEMAT = ALSQ(IND)

      END FUNCTION ELEMAT
!
!*****************************************************************************
!
      SUBROUTINE EQOP(R,T,N,L)
!
! *** EQOP by JCM 28 Jun 83 ***
!
!X
!C 1B
!H Checks whether a rotation matrix and a translation vector of a symmetry
!H operator are already in a list, and adds them if not.  Also finds
!H lattice translations.
!A On entry R holds a 3x3 rotation matrix (part of a space group symmetry
!A            operator)
!A          T holds a 1x3 translation vector for the same operator.
!A          N is the number of entries in the list in /SCRAT so far
!A          L is the number of non-primitive lattice vectors so far.
!A On exit N and or L may have been increased by 1.  N may also indicate
!A which element of TSYM matched R.
!
!D Checks whether R is already in table TSYM in /SCRAT.  If not, R and T are
!D added to TSYM and TTRANS in /SCRAT, and N is incremented.
!D
!D If R occurs in TSYM table, examines T in case it gives a new lattice
!D translation.  If it does, adds that to the permanent array ALAT in /SYMDA
!D and increments L. Returns pointer to matching TSYM in N.
!D
!D Checks are made that  N<=48 and L<=4.
!
      DIMENSION R(3,3), T(3)
      COMMON /SCRAT / TSYM(3,3,48), TTRANS(3,48), MLTAB(48,48), VEC(3)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      NN = N
      DO I = 1, NN
        DO J = 1, 3
          DO K = 1, 3
            IF (ABS(TSYM(J,K,I)-R(J,K)).GT..0001) GOTO 1
          ENDDO
        ENDDO
! MATRIX THE SAME - CHECK TRANSLATION VECTOR
        IS = 0
        DO K = 1, 3
          VEC(K) = AMOD(TTRANS(K,I)-T(K)+1.,1.)
          IF (VEC(K).GT..0001) IS = 1
        ENDDO
        IF (IS.EQ.0) GOTO 6
        NL1 = L
        CALL EQPOS(ALAT,VEC,NL1,NL2,4)
        IF (IBMBER .NE. 0) RETURN
        IF (NL2.EQ.L+1) L = L + 1
    6   N = I
        RETURN
    1 ENDDO
! A NEW ROTATION MATRIX HAS BEEN FOUND:
      CALL ERRCHK(2,N,48,0,'symmetry operators')
      IF (IBMBER .NE. 0) RETURN
      CALL GMEQ(R,TSYM(1,1,N),3,3)
      CALL GMEQ(T,TTRANS(1,N),1,3)

      END SUBROUTINE EQOP
!
!*****************************************************************************
!
      SUBROUTINE EQPOS(VEC1,VEC2,N1,N2,M)
!
! *** EQPOS updated by JCM 13 Apr 86 ***
!
!X
!C 11C
!H Checks whether the given atom position already occurs in a given list,
!H and adds the new one if not.
!A On entry VEC2 holds a 1x3 vector giving a real space postion.
!A          VEC1 is a table of 1x3 vectors, of size (3,M)
!A          N1 is the number of entries in VEC1 so far
!D Determines whether VEC2 occurs in VEC1, disregarding multiples of unit cells.
!D If VEC2 gives a new position it is added to the list VEC1 and N2 is set to
!D N1+1.  A check is made that the total number of positions does not exceed
!D M, the maximum allowed.
!D
!D All elements of VEC1 are put into the range 0 =< X <1.
!D
!D If VEC2 does occur in the list VEC1, N2 is set to its position there.
!N M must be at least 1.
!
      DIMENSION VEC1(3,M), VEC2(3)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
!
      IF (N1.LT.1) GOTO 4
      DO I = 1, N1
        DO J = 1, 3
          A = AMOD(ABS(VEC1(J,I)-VEC2(J)),1.)
          IF ((A.GT..0005) .AND. (A.LT..9995)) GOTO 1
        ENDDO
! MATCH FOUND - JUMP:
        GOTO 101
    1 ENDDO
! A NEW VECTOR FOUND IN VEC2 - STORE IT IN VEC1:
    4 I = N1 + 1
      IF (I.LE.M) GOTO 5
      WRITE (LPT,3000) M, ((VEC1(J1,I1),J1=1,3),I1=1,M)
      CALL BMBOUT
      RETURN
    5 CALL GMEQ(VEC2,VEC1(1,I),1,3)
      CALL FRAC3(VEC1(1,I))
  101 N2 = I
      RETURN
 3000 FORMAT (/' ERROR ** more than',I3,' equivalent positions ','found in EQPOS - list so far is:'/(1X,3F12.4))
      END SUBROUTINE EQPOS
!
!*****************************************************************************
!
      SUBROUTINE EQPPOS(VEC1,VEC2,N1,N2,M)
!
! *** EQPPOS corrected by PJB 31-May-1994 ***
!
!X
!C 11C
!H Checks whether the given position vector is related by lattice translation
!H to one already in the given list.
!A On entry VEC2 holds a 1x3 vector giving a real space postion.
!A          VEC1 is a table of 1x3 vectors, of size (3,M)
!A          N1 is the number of entries in VEC1 so far
!D Determines whether VEC2 occurs in VEC1, disregarding multiples of
!D lattice vectors.
!D If VEC2 gives a new position it is added to the list VEC1 and N2 is set to
!D N1+1.  A check is made that the total number of positions does not exceed
!D M, the maximum allowed.
!D
!D All elements of VEC1 are put into the range 0 =< X <1.
!D
!D If VEC2 does occur in the list VEC1, N2 is set to its position there.
!N M must be at least 1.
!
      DIMENSION VEC1(3,M), VEC2(3), TVEC(3)
      LOGICAL LATVEC
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
!
      IF (N1.LT.1) GOTO 4
      DO I = 1, N1
        CALL GMSUB(VEC1(1,I),VEC2,TVEC,3,1)
        IF (LATVEC(TVEC)) GOTO 101
! MATCH FOUND - JUMP:
      ENDDO
! A NEW VECTOR FOUND IN VEC2 - STORE IT IN VEC1:
    4 I = N1 + 1
      IF (I.GT.M) THEN
        WRITE (LPT,3000) M, ((VEC1(J1,I1),J1=1,3),I1=1,M)
        CALL BMBOUT
        RETURN
      ENDIF
      CALL GMEQ(VEC2,VEC1(1,I),1,3)
      CALL FRAC3(VEC1(1,I))
  101 N2 = I
      RETURN
 3000 FORMAT (/' ERROR ** more than',I3,' equivalent positions ','found in EQPOS - list so far is:'/(1X,3F12.4))

      END SUBROUTINE EQPPOS
!
!*****************************************************************************
!
      SUBROUTINE EQRLV(VEC1,VEC2,N1,N2,M)
!
! *** EQRLV by PJB Jun 88 ***
!
!X
!C 11C
!H Checks whether vectors differ by a reciprocal lattice vector.
!
!A On entry VEC1 holds a list of 1x3 vectors
!A          VEC2 holds a single 1x3 vector
!A          N1 is the number of vectors in the list VEC1
!A          M is positive if it is required to add VEC2 to list if unique.
!A On exit N2 points to the position of VEC2 in the list VEC1
!D Checks whether VEC2 is identical to, or differs by a reciprocal
!D lattice vector from any of the N1 vectors stored in VEC1.  If on entry
!D M>0 and if VEC2 is unique it is added to the list VEC1 and N2 is
!D set to N1+1; otherwise N2=which vector it matched.
!D
!D A check is made that the total number of vectors in VEC1 is <= M ,
!D the maximum allowed.
!D
!D If on entry M=0, N2 is set as above, but the new vector is not added to
!D the list.
!
      DIMENSION VEC1(3,1), VEC2(3), TVEC(3)
      LOGICAL LATABS
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      N = N1
      IF (N.LT.1) GOTO 4
      DO I = 1, N
        CALL GMSUB(VEC1(1,I),VEC2,TVEC,3,1)
        IF (LATABS(TVEC)) GOTO 1
! HAVE FOUND DUPLICATE - IGNORE & SET N2 TO POINT TO IT:
        GOTO 101
    1 ENDDO
! HAVE NEW VECTOR:
    4 I = N + 1
      IF (M.EQ.0) GOTO 101
      IF (I.LE.M) GOTO 5
      WRITE (LPT,3000) M, ((VEC1(J,I),J=1,3),I=1,M)
      CALL BMBOUT
      RETURN
! TO STORE NEW VECTOR:
    5 CALL GMEQ(VEC2,VEC1(1,I),1,3)
  101 N2 = I
      RETURN
 3000 FORMAT (' ERROR ** more than',I3,'equivalent vectors ','formed - vectors so far are'/(1X,3E12.5))
      END SUBROUTINE EQRLV
!
!*****************************************************************************
!
      SUBROUTINE EQVEC(VEC1,VEC2,N1,N2,M)
!
! *** EQVEC updated by JCM 22 Oct 86 ***
!
!X
!C 11C
!H Finds a given vector in given table of vectors, or adds it as a new one.
!A On entry VEC1 holds a list of 1x3 vectors
!A          VEC2 holds a single 1x3 vector
!A          N1 is the number of vectors in the list VEC1
!A          M is positive if it is required to add VEC2 to list if unique.
!A On exit N2 points to the position of VEC2 in the list VEC1
!D Checks whether VEC2 is identical to any of the N1 vectors stored in VEC1.
!D If on entry M>0 and if VEC2 is unique it is added to the list VEC1 and N2
!D is set to N1+1; otherwise N2=which vector it matched.
!D
!D A check is made that the total number of vectors in VEC1 is <= M ,
!D the maximum allowed.
!D
!D If on entry M=0, N2 is set as above, but the new vector is not added to
!D the list.
!
!     Change by KS, Oct 95.  VEC1 size changed from (3,1) to (3,*)
!     The need for this was picked up by an array bounds check
      DIMENSION VEC1(3,*), VEC2(3)
      LOGICAL GMSAME
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      N = N1
      IF (N.LT.1) GOTO 4
      DO I = 1, N
        IF (.NOT.GMSAME(VEC1(1,I),VEC2,3,0.0001)) GOTO 1
! HAVE FOUND DUPLICATE - IGNORE & SET N2 TO POINT TO IT:
        GOTO 101
    1 ENDDO
! HAVE NEW VECTOR:
    4 I = N + 1
      IF (M.EQ.0) GOTO 101
      IF (I.LE.M) GOTO 5
      WRITE (LPT,3000) M, ((VEC1(J,I),J=1,3),I=1,M)
      CALL BMBOUT
      RETURN
! TO STORE NEW VECTOR:
    5 CALL GMEQ(VEC2,VEC1(1,I),1,3)
  101 N2 = I
      RETURN
 3000 FORMAT (' ERROR ** more than',I3,'equivalent vectors formed - vectors so far are'/(1X,3E12.5))

      END SUBROUTINE EQVEC
!
!*****************************************************************************
!
      SUBROUTINE ERRATM(NAME,NACT,MESS)
!
! *** ERRATM by JCM 25 Sep 89 ***
!
!X
!C 13C
!H Writes an error message to say that the given name is not an atom name;
!H there is a choice of subsequent action.
!A On entry, NAME is the A4 non-atom name
!A On entry NACT says which action is required:
!A    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
!A    NACT -ve means complain and exit
!A    NACT =0 means complain and stop
!A   IABS(NACT)=1 just gives atom name
!A   IABS(NACT)=2 also writes out ICARD from /SCRACH
!A On entry MESS is the message specific to this error state
!O Writes message on unit LPT.
!
      CHARACTER*(*) MESS
      CHARACTER*4 NAME
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      L = LENGT(MESS)
      IF (NACT.GT.0) IERR = IERR + 1
      WRITE (LPT,3001) NAME, (MESS(I:I),I=1,L)
      IF (IABS(NACT).EQ.2) THEN
        WRITE (LPT,2001) ICARD
      ENDIF
      IF (NACT.EQ.0) CALL BMBOUT
      RETURN
 3001 FORMAT (/' ERROR ** ',A4,' is not an atom name - on ',80A1)
 2001 FORMAT (' Card says:'/1X,A80)

      END SUBROUTINE ERRATM
!
!*****************************************************************************
!
      SUBROUTINE ERRCH2(WORD,NACT,MESS1,MESS2)
!
! *** ERRCH2 by JCM 25 Sep 89 ***
!
!X
!C 13C
!H Write an error message which involves a given WORD between 2 messages;
!H there is a choice of subsequent action.
!A On entry, WORD is the A4 word to print
!A On entry NACT says which action is required:
!A    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
!A    NACT -ve means complain and exit
!A    NACT =0 means complain and stop
!A Absolute values for NACT on entry are:
!A          1 means simply write MESS1, WORD, MESS2
!A          2 means follow these on the next line by ICARD
!A On entry MESS1 is the message before WORD
!A On entry MESS2 is the message after WORD
!P If ABS(NACT)=2, ICARD in /SCRACH/ must contain the A80 card read
!O Writes message on unit LPT.
!
      CHARACTER*34 FORM
      CHARACTER*(*) MESS1, MESS2
      CHARACTER*(*) WORD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'('' ERROR ** '',80A1,1X,A4 ,1X,80A1)'/

      CALL DebugErrorMessage('ERRCH2 called')
      LW = LENGT(WORD)
      IF (LW.EQ.0) LW = 1
      WRITE (FORM(24:25),2000) LW
      L1 = LENGT(MESS1)
      IF (L1.EQ.0) L1 = 1
      WRITE (FORM(15:16),2000) L1
      L2 = LENGT(MESS2)
      IF (NACT.GT.0) IERR = IERR + 1
      WRITE (LPT,FORM) (MESS1(I:I),I=1,L1), WORD, (MESS2(I:I),I=1,L2)
      IF (IABS(NACT).EQ.2) THEN
        WRITE (LPT,2001) ICARD
      ENDIF
      IF (NACT.EQ.0) CALL BMBOUT
      RETURN
 2000 FORMAT (I2)
 2001 FORMAT (' Card says:'/1X,A80)

      END SUBROUTINE ERRCH2
!
!*****************************************************************************
!
      SUBROUTINE ERRCHK(NTYP,NVALUE,NBOUND,NACT,MESS)
!
! *** ERRCHK by JCM 4 Oct 88 ***
!
!X
!C 13C
!H (Possibly increases and) checks a value, giving if appropriate an error
!H message;  there is a choice of subsequent action.
!A On entry NTYP=type of check required:
!A    NTYP=1 simply check NVALUE for being NOT GREATER THAN NBOUND
!A    NTYP=2 increment NVALUE by 1, then as type 1
!A On entry NVALUE is the integer to be checked
!A          NBOUND is its upper bound
!A On entry NACT says which action is required if the test fails:
!A    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
!A    NACT -ve means complain and exit
!A    NACT =0 means complain and stop
!A On entry MESS is the message specific to this error state
!
!D The error message starts " ERROR ** ", and finishes with MESS.
!D If NTYP=1, NVALUE is printed.
!O Outputs the required message on unit LPT
!
      CHARACTER*(*) MESS
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      IF (NTYP.NE.1) NVALUE = NVALUE + 1
      IF (NVALUE.LE.NBOUND) RETURN
      IF (NACT.GT.0) IERR = IERR + 1
      L = LENGT(MESS)
      IF (NTYP.EQ.1) THEN
        WRITE (LPT,3001) NVALUE, (MESS(I:I),I=1,L)
      ENDIF
      WRITE (LPT,3000) NBOUND, (MESS(I:I),I=1,L)
      IF (NACT.EQ.0) CALL BMBOUT
 3001 FORMAT (/' ',I6,80A1)
 3000 FORMAT (/' ERROR ** there is an upper limit of',I6,' on number of ',80A1)

      END SUBROUTINE ERRCHK
!
!*****************************************************************************
!
      SUBROUTINE ERRIN2(INT,NACT,MESS1,MESS2)
!
! *** ERRIN2 by JCM 25 Sep 89 ***
!
!X
!C 13C
!H Writes an error message which involves a given integer INT between 2
!H messages;  there is a choice of subsequent action.
!A On entry, INT is the integer to print
!A On entry NACT says which action is required:
!A    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
!A    NACT -ve means complain and exit
!A    NACT =0 means complain and stop
!A   IABS(NACT)=1 means just give message
!A   IABS(NACT)=2 means also print contents of /SCRACH/
!A On entry MESS1 is the message before INT
!A On entry MESS2 is the message after INT
!O Writes message on unit LPT.
!
      CHARACTER*33 FORM
      CHARACTER*(*) MESS1, MESS2
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'('' ERROR ** '',80A1,1X,I5,1X,80A1)'/

      L1 = LENGT(MESS1)
      IF (L1.EQ.0) L1 = 1
      WRITE (FORM(15:16),2000) L1
 2000 FORMAT (I2)
      L2 = LENGT(MESS2)
      IF (NACT.GT.0) IERR = IERR + 1
      WRITE (LPT,FORM) (MESS1(I:I),I=1,L1), INT, (MESS2(I:I),I=1,L2)
      IF (IABS(NACT).EQ.2) THEN
        WRITE (LPT,2001) ICARD
      ENDIF
      IF (NACT.EQ.0) CALL BMBOUT
      RETURN
 2001 FORMAT (' Card says:'/1X,A80)
 
      END SUBROUTINE ERRIN2
!
!*****************************************************************************
!
      SUBROUTINE ERRMES(NTYP,NACT,MESS)
!
! *** ERRMES updated by JCM 10 Nov 89 ***
!
!X
!C 13C
!H Writes an error message, with choice of action on exit.
!A On entry, NTYP=type of message:
!A      NTYP=0 If IERR not 0 write "Errors in input" followed by MESS and stop
!A      NTYP=1      write "ERROR **" followed by MESS
!A      NTYP=-1     write "PROGRAM ERROR **" followed by MESS
!A      ABS(NTYP)=2 write "ERROR ** need" followed by MESS
!A      ABS(NTYP)=3 write "ERROR ** need card" followed by MESS
!A On entry NACT says which action is then required
!A    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
!A    NACT -ve means complain and exit
!A    NACT =0 means complain and stop
!A On entry MESS is the message specific to this error state
!O Writes message on unit LPT.
!
      CHARACTER*(*) MESS
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      L = LENGT(MESS)
      IF (NTYP.EQ.0) THEN
        IF (IERR.NE.0) THEN
          WRITE (LPT,3000) IERR, (MESS(I:I),I=1,L)
 3000     FORMAT (///' *** ',I4,' fatal error(s) in input ',80A1)
          CALL BMBOUT
        ENDIF
        RETURN
      ENDIF
      IF (NACT.GT.0) IERR = IERR + 1
      IF (NTYP.EQ.1) THEN
        WRITE (LPT,3001) (MESS(I:I),I=1,L)
 3001   FORMAT (/' ERROR ** ',80A1)
      ELSEIF (IABS(NTYP).EQ.2) THEN
        WRITE (LPT,3002) (MESS(I:I),I=1,L)
 3002   FORMAT (/' ERROR ** need ',80A1)
      ELSEIF (IABS(NTYP).EQ.3) THEN
        WRITE (LPT,3003) (MESS(I:I),I=1,L)
 3003   FORMAT (/' ERROR ** need card ',80A1)
      ELSEIF (NTYP.EQ.-1) THEN
        WRITE (LPT,3004) (MESS(I:I),I=1,L)
 3004   FORMAT (/' PROGRAM ERROR ** ',80A1)
      ENDIF
      IF (NACT.EQ.0) CALL BMBOUT

      END SUBROUTINE ERRMES
!
!*****************************************************************************
!
      SUBROUTINE ERRRE2(X,NACT,MESS1,MESS2)
!
! *** ERRRE2 by JCM 25 Sep 89 ***
!
!X
!C 13C
!H Writes an error message which involves a given real X between 2
!H messages;  there is a choice of subsequent action.
!A On entry, X is the real number to print
!A On entry NACT says which action is required:
!A    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
!A    NACT -ve means complain and exit
!A    NACT =0 means complain and stop
!A   IABS(NACT)=1 means just give message
!A   IABS(NACT)=2 means also print contents of /SCRACH/
!A On entry MESS1 is the message before X
!A On entry MESS2 is the message after X
!O Writes message on unit LPT.
!
      CHARACTER*36 FORM
      CHARACTER*(*) MESS1, MESS2
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'('' ERROR ** '',80A1,1X,G12.4,1X,80A1)'/

      L1 = LENGT(MESS1)
      IF (L1.EQ.0) L1 = 1
      WRITE (FORM(15:16),2000) L1
 2000 FORMAT (I2)
      L2 = LENGT(MESS2)
      IF (NACT.GT.0) IERR = IERR + 1
      WRITE (LPT,FORM) (MESS1(I:I),I=1,L1), X, (MESS2(I:I),I=1,L2)
      IF (IABS(NACT).EQ.2) THEN
        WRITE (LPT,2001) ICARD
      ENDIF
      IF (NACT.EQ.0) CALL BMBOUT
      RETURN
 2001 FORMAT (' Card says:'/1X,A80)

      END SUBROUTINE ERRRE2
!
!*****************************************************************************
!
      LOGICAL FUNCTION EXCLD(A,B,M)
!
! *** EXCLD by JCM 17 Jan 85 ***
!
!X
!C 11C
!H Determines whether a number occurs within any of a set of given ranges.
!A On entry A is a single element.
!A          B is an array of M/2 pairs of numbers, B1 and B2 say.
!A            Each B1 must be < or = its own B2, but the B's need not all
!A            be in ascending order.
!A On exit EXCLD is.TRUE. if A occurs within any of the ranges B1 to B2,
!A            both inclusive.
!
!N If M should be 0 it should be given as 1
!
      DIMENSION B(M)
      EXCLD = .FALSE.
      IF (M.LT.2) GOTO 100
      DO I = 2, M, 2
        IF (A.LT.B(I-1)) GOTO 1
        IF (A.GT.B(I)) GOTO 1
        EXCLD = .TRUE.
        GOTO 100
    1 ENDDO
  100 RETURN

      END FUNCTION EXCLD
!
!*****************************************************************************
!
      SUBROUTINE UXEXPAND(IBUF,OBUF)
!
! *** EXPAND new by PJB Mar-28-1994 ***
!C 13C
!H Expands UNIX pathnames by substituting for environment variables
!A IBUF is a character variable containing the path name to be expanded
!A on output the character variable OBUF contains the expanded pathname
!N OBUF must be given a length by the calling program which is sufficient
!N to hold the expanded path
!
      CHARACTER*(*) IBUF, OBUF

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      L = LENGT(IBUF)
      M = LEN(OBUF)
      I = 1
      J = 1
    1 IDOLL = INDEX(IBUF(I:),'$')
      IF (IDOLL.EQ.0) THEN
        OBUF(J:) = IBUF(I:L)
        GOTO 100
      ENDIF
      IP = IDOLL - 1
      IF (IP.GT.0) OBUF(J:) = IBUF(I:I+IP-1)
      J = J + IP
      IF (IBUF(IP:IP).EQ.CHAR(92)) THEN
        OBUF(J:J) = '$'
        J = J + 1
        I = I + IP + 2
      ELSE
        IP = IP + 2
        DO I = IP, L
          LET = LETTER(IBUF(I:I))
          IF (LET.GT.0) GOTO 2
          INT = NDIGIT(IBUF(I:I))
          IF (INT.GT.-1) GOTO 2
          GOTO 3
    2   ENDDO
    3   CONTINUE
        J = LENGT(OBUF)
        J = J + 1
        CALL ERRCHK(1,J,M,0,'Expanded path name too long')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      IF (I.LE.L) GOTO 1
  100 RETURN

      END SUBROUTINE UXEXPAND
!
!*****************************************************************************
!
      SUBROUTINE EXTINC(N,F)
!
! *** EXTINC by JCM 23 Jan 85 ***
!
!X
!C 2B
!H Multi-entry routine to deal with all aspects of single crystal extinction
!H corrections.
!A On entry N indicates action required:
!A     N=1 Read and interpret an E card
!A         (This is also ENTRY EXTIN1)
!A     N=2 Calculate an extinction correction, given F=mod(FC)
!A     N=3 as 2, and also calculate divided derivatives
!A     N=4 Apply shift to DOMR
!A     N=5 Apply shift to MOSC
!A         (These are also ENTRY EXTIN3(NP) where NP=1 for DOMR, 2 for MOSC)
!A     N=6 Output new E card
!A         (This is also ENTRY EXTIN4)
!A
!A     ENTRY EXTIN8(NP,NV) sets DOMR (NP=1) or MOSC (NP=2) to be variable NV
!A     ENTRY EXTIN9 sets both DOMR and MOSC fixed.
!P Entries 2 through 6 require that the extinction is set up by an entry 1.
!P Entries 2 and 3 expect in the array CEXT in /EXTN/ the 4 coefficients as
!P described in Becker & Coppens (1974) Acta Cryst A30 p129.
!P Normally entry 3 would be from an LSQ job via CALCSF
!P          entries 4 & 5 from an LSQ job via APSHSF
!P          entry 6 from an LSQ job via NWINSF
!
!D Entry 1 reads DOMR and MOSC and IEXTYP into /EXTN, setting LOGICALS
!D         GAUSS and LOREN
!D Entry 2 calculates EXTCOR, which is SQRT(Y) in the theory above, using either
!D         the Lorenztian (IEXTYP=1) or Gaussian (IEXTYP=2) model.
!
!D Entry 3 calculates in addition the derivatives:
!D         DEX/DR (R is DOMR)   DEX/DG (G is MOSC) and DEX/DF (F is mod(FC))
!D         and all these are required divided by EX itself.  They are
!D         therefore put into variables ending Q for "quotient"
!O Entry 6 writes a new E card to unit NEWIN
!
      COMMON /EXTN  / IEXTYP, DOMR, KDOMR, AMOSC, KMOSC, EXTCOR, CEXT(4)&
     &                , DEXDFQ, DEXDRQ, DEXDGQ, LOREN, GAUSS
      LOGICAL LOREN, GAUSS
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      GOTO (1,2,2,4,5,6), N
      ENTRY EXTIN1
! READ E CARD:
    1 CALL INPUTE
      GOTO 100
! CALCULATE EXTCOR - WITH DERIVATIVES IF N=3:
    2 IF (IEXTYP.EQ.0) GOTO 10
      A = 1.5*DOMR/CEXT(2)
      B = CEXT(1)*F*F/1.5
      C = 1.5*AMOSC
      H = 2.*AMOSC*AMOSC
      IF (LOREN) D = 1./(1.+A/C)
      IF (GAUSS) D = 1./(SQRT(1.+A*A/H))
      X = B*A*D
      X2 = 2.*X
      XX = X*X
      C4 = 1. + CEXT(4)*X
      YY = 1./(1.+X2+CEXT(3)*XX/C4)
      Y = SQRT(YY)
      EXTCOR = SQRT(Y)
      IF (N.EQ.2) GOTO 100
! DERIVATIVES:
      IF (LOREN) FACTOR = 1./C
      IF (GAUSS) FACTOR = A*D/H
      FACTOR = FACTOR*A*D
      DNUM = 2. + X2*(2.*CEXT(4)+CEXT(3)) + CEXT(4) *XX*(2.*CEXT(4)+CEXT(3))
      E = -YY*DNUM*X/(4.*C4*C4)
      DEXDRQ = E*(1.-FACTOR)/DOMR
      DEXDGQ = E*FACTOR/AMOSC
      DEXDFQ = E*2./F
      GOTO 100
! ENTRY 2 OR 3 - NO EXTINCTION CORRECTION:
   10 EXTCOR = 1.
      DEXDRQ = 0.
      DEXDGQ = 0.
      DEXDFQ = 0.
      GOTO 100
      ENTRY EXTIN3(NP)
      GOTO (4,5), NP
! APPLY SHIFT TO DOMR:
    4 CALL ADJUST(DOMR)
      GOTO 100
! APPLY SHIFT TO MOSC:
    5 CALL ADJUST(AMOSC)
      GOTO 100
      ENTRY EXTIN4
! NEW E CARD:
    6 WRITE (NEWIN,2000) IEXTYP, DOMR, AMOSC
 2000 FORMAT ('E',I5,2F10.4)
      GOTO 100
      ENTRY EXTIN8(NP,NV)
      IF (NP.EQ.1) KDOMR = NV
      IF (NP.EQ.2) KMOSC = NV
      GOTO 100
      ENTRY EXTIN9
      KDOMR = 0
      KMOSC = 0
  100 RETURN

      END SUBROUTINE EXTINC
!
!*****************************************************************************
!
      SUBROUTINE F2NEW(L)
!
! *** F2NEW updated by JCM 6 Feb 90 ***
!
!X
!C 6B
!H Outputs a new LSQ family 2 (structure parameters) card (for A, T
!H or F cards).
!A On entry L is the position in the alphabet of the first letter of the card:
!A L = 1, 6 OR 20 for A, F OR T.
!P The card should have been read to ICARD in /SCRACH/.
!O Outputs to unit NEWIN  a new card, with altered parameters if necessary.
!
      CHARACTER*4 LABA, LABS, LABF
      DIMENSION A(6)
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
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
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (L.EQ.1) GOTO 1
      IF (L.EQ.6) GOTO 2
      IF (L.EQ.20) GOTO 3
      CALL ERRIN2(L,0,'F2NEW entered with L=',' ')
      IF (IBMBER .NE. 0) RETURN
! 'A' CARD:
    1 CALL INPUTA(0,LABA,LBALEN,LABS,LBSLEN,A,TOLD,SOLD,IER)
! REREAD CARD TO FIND OUT WHICH ATOM, AND WHETHER SCAT LABEL EXPLICIT OR NOT:
      IR = IATOM(LABA)
      LABF = ' '
      IF (LBSLEN.GT.0) LABF = LABS
! MAY BE SD CARD:
      IF (SDREAD) THEN
        WRITE (NEWIN,2020) LABA, (SDX(J,IR),J=1,3), SDTF(IR), LABF, SDSITE(IR)
 2020   FORMAT ('A SD ',A4,4F10.5,1X,A4,F10.5)
      ELSE
        IF (SITE(IR).EQ.1.) WRITE (NEWIN,2000) LABA, (X(J,IR),J=1,3), TF(IR), LABF
        IF (SITE(IR).NE.1.) WRITE (NEWIN,2000) LABA, (X(J,IR),J=1,3), TF(IR), LABF, SITE(IR)
      ENDIF
      GOTO 100
! 'F' CARD:
    2 CALL INPUTF(0,LABF,LBFLEN,NTYP,IPT,IER)
! REREAD CARD TO DISCOVER WHICH FACTOR AND TYPE:
      IF (NTYP.NE.1) GOTO 101
      IR = ISCAT(LABF)
      WRITE (NEWIN,2001) LABF, NTYP, CMULT(IR)
 2001 FORMAT ('F ',A4,I5,F10.5)
      GOTO 100
! 'T' CARD:
    3 CALL INPUTT(0,LABA,LBALEN,NTYP,A,IER)
! REREAD CARD TO DISCOVER WHICH:
      IR = IATOM(LABA)
      IA = IAPT(IR)
      DO I = 1, 6
        A(I) = ATF(I,IA)*CONATF(I,IA)
      ENDDO
      WRITE (NEWIN,2002) LABA, IATYP(IAPT(IR)), A
 2002 FORMAT ('T ',A4,I5,6F10.5)
      GOTO 100
! COPY OUT UNCHANGED CARD:
  101 WRITE (NEWIN,2003) (ICARD(I:I),I=1,LENGT(ICARD))
 2003 FORMAT (80A1)
  100 RETURN
 2000 FORMAT ('A ',A4,4F10.5,1X,A4,F10.5)

      END SUBROUTINE F2NEW
!
!*****************************************************************************
!
      BLOCKDATA F2PARS
!
! *** F2PARS updated by PJB 23-Sept-93 ***
!
      COMMON /F2NAMS/ F2NAME(40)
      CHARACTER*4 F2NAME
      COMMON /F2NUMS/ NF2NUM(3,40)
      DATA F2NAME/'X', 'Y', 'Z', 'B11', 'B22', 'B33', 'B23', 'B13',     &
     &     'B12', 'SCAT', 'SITE', 'ITF', 'PSI1', 'PSI2', 'PSI3', 'PSI4',&
     &     'THET', 'PHI', 'THE1', 'PHI1', 'MU', 'MU1', 'TFAC', 'A*',    &
     &     'B*', 'C*', 'D*', 'E*', 'F*', 'KX', 'KY', 'KZ', 'FAM1',      &
     &     'FAM2', 'XYZ', 'BIJ', 'XYZT', 'CELL', 'XYZB', 'XYZS'/
      DATA NF2NUM/2, 0, 1, 2, 0, 2, 2, 0, 3, 2, 0, 4, 2, 0, 5, 2, 0, 6, &
     &     2, 0, 7, 2, 0, 8, 2, 0, 9, 2, 0, 10, 2, 0, 11, 2, 0, 12, 2,  &
     &     0, 13, 2, 0, 14, 2, 0, 15, 2, 0, 16, 2, 0, 17, 2, 0, 18, 2,  &
     &     0, 19, 2, 0, 20, 2, 0, 21, 2, 0, 22, 1, 1, 1, 1, 1, 2, 1, 1, &
     &     3, 1, 1, 4, 1, 1, 5, 1, 1, 6, 1, 1, 7, 1, 1, 11, 1, 1, 12, 1,&
     &     1, 13, 1, 0, 0, 2, 0, 0, -1, 0, 0, -2, 0, 0, -3, 0, 0, -4, 0,&
     &     0, -5, 0, 0, -6, 0, 0/
      END BLOCKDATA F2PARS
!
!*****************************************************************************
!
      SUBROUTINE F2RELA(IFAM,ISPVEC)
!
! *** F2RELA updated by JCM 8 Sep 88 ***
!
!X
!C 6B
!H Collects all structure factor type constraints implied by the symmetry.
!A IFAM gives family number;  so far this is 2 for structure parameters,
!A      but it may one day be more general
!A ISPVEC is a vector holding miscellaneous pointers saying which parameters
!A        within family IFAM, genus IR(=which atom) the following are:
!A        (1) x position coord
!A        (2) B11 first atf coefficient
!A        (3) f, the scattering factor
!A        with room for others which may be required later
!P JPHASE, JSOURC hold phase and source
!
!D Space group symmetry generated constraints are each between 2 parameters
!D only, and refer to x, y, z coordinates, or to anisotropic coefficients
!D Some of the relations found may lead to fixings rather than constraints
!D (Later - if magnetic, do the constraints on the magnetic pars here
!D also)
!D
!D We also chain together here the scattering factors of like atoms
!D and fix any non-existent atfs.  In the process, we check that the given
!D atfs have the correct symmetry to start with.
!
      DIMENSION ISPVEC(10)
      DIMENSION RMAT(3,3), NFIX3(3), FIX3(3), NFIX6(6), FIX6(6)
      DIMENSION KK1(2), AM(2), NCOUNT(6)
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

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
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
!
! SCAN ALL ATOMS, PICKING UP RELATIONS BETWEEN POSITION PARAMETERS, ANISOTROPIC
! COEFFICIENTS, AND SCATTERING FACTORS BELONGING TO OTHER ATOMS ALSO:
      IF (NATOM.LE.0) GOTO 100
! IF NO ATFS AT ALL, FIX ALL:
      IF (ICDNO(20).EQ.0) THEN
        DO I = 1, 6
          CALL ADDFX5(IFAM,0,ISPVEC(2)+I-1,JPHASE,1,5)
        ENDDO
      ENDIF
      DO IR = 1, NATOM
! CLEAR OUT ALL FIX/RELA INFO FOR THIS ATOM:
        DO K = 1, 6
          NFIX6(K) = 9999
          IF (K.GT.3) GOTO 2
          NFIX3(K) = 9999
    2   ENDDO
! JUMP IF NOT SPECIAL:
        IF (ISGEN(1,IR).EQ.1) GOTO 6
! JUMP IF NOT SPECIAL BECAUSE OF A CENTRE OF SYMMETRY AT THE ORIGIN:
        IF (ISGEN(1,IR).GT.0) GOTO 4
! FIX ALL POSITION:
        DO I = 1, 3
          CALL FIXPAR(I,NFIX3)
        ENDDO
! ATF ON ATOM ON CENTRE:
        IF (IAPT(IR).EQ.0) GOTO 4
        CALL GMUNI(RMAT,3)
        CALL GMREV(RMAT,RMAT,3,3)
        CALL RELSM6(RMAT,NFIX6,FIX6)
! TAKE FIRST (OF POSSIBLE 2) SYMMETRY ELEMENTS MAKING THIS POSITION SPECIAL:
    4   DO I = 2, 3
          K = IABS(ISGEN(I,IR))
          CALL GMEQ(SYM(1,1,K),RMAT,3,3)
          IF (ISGEN(I,IR).LT.0) CALL GMREV(RMAT,RMAT,3,3)
          CALL RELSM3(RMAT,NFIX3,FIX3)
! IF THERE IS AN ATF, FIND ITS RELATIONS ALSO:
          IF (IAPT(IR).NE.0) CALL RELSM6(RMAT,NFIX6,FIX6)
! IS THERE A SECOND GENERATOR OF THE SUB-GROUP WHICH MAKES THIS ATOM SPECIAL?
          IF (ISGEN(3,IR).EQ.0) GOTO 6
        ENDDO
! ALL POSITION AND ATF RELATIONS COLLECTED IN TEMPORARY SPACE - USE:
    6   DO I = 1, 3
          NCOUNT(I) = KPAK(IFAM,IR,ISPVEC(1)+I-1,JPHASE,1)
        ENDDO
        CALL FIXREL(3,NFIX3,FIX3,NCOUNT,5)
        IF (IAPT(IR).NE.0) THEN
          DO I = 1, 6
            NCOUNT(I) = KPAK(IFAM,IR,ISPVEC(2)+I-1,JPHASE,1)
          ENDDO
          CALL FIXREL(6,NFIX6,FIX6,NCOUNT,5)
        ENDIF
! NOW LINK SCATTERING FACTORS FOR THOSE ATOMS WITH SAME FACTOR:
        IS = NFORMF(IR)
        IT = NBAKF(IS)
        IF (IT.NE.IR) THEN
          KK1(1) = KPAK(IFAM,IR,ISPVEC(3),JPHASE,1)
          KK1(2) = KPAK(IFAM,IT,ISPVEC(3),JPHASE,1)
          AM(1) = 1.
          AM(2) = -1.
          CALL ADDCON(2,KK1,AM,5)
        ENDIF
! AND FIX ALL NON-EXISTENT ATF COEFFS, CHECKING ANY WITH FIX/RELA INFO:
        IF (ICDNO(20).EQ.0) GOTO 3
        IF (IAPT(IR).EQ.0) THEN
          DO I = ISPVEC(2), ISPVEC(2) + 5
            CALL ADDFX5(IFAM,IR,I,JPHASE,1,5)
          ENDDO
          GOTO 3
        ENDIF
        DO I = 1, 6
          IF (NFIX6(I).EQ.9999) GOTO 9
          IF (NFIX6(I).EQ.0) THEN
!     CHECK THAT ATF COEFF ALSO = 0 & ADJUST IF NOT
            GOTO 9
          ENDIF
          I1 = I
   10     I2 = NFIX6(IABS(I1))
          IF (I2.EQ.I) GOTO 9
!     CHECK THAT COEFF I1 AND COEFF I2 HAVE CORRECT RELATIONSHIP
          I1 = I2
          GOTO 10
    9   ENDDO
    3 ENDDO
  100 RETURN

      END SUBROUTINE F2RELA
!
!*****************************************************************************
!
      SUBROUTINE F2SHFT
!
! *** F2SHFT updated by JCM 10 Feb 87 ***
!
!X
!C 6B
!H Applies a shift to a particular family 2 (structure) parameter.
!
!P On entry in/NEWOLD/ IGEN=which atom and ISPC=which parameter
!P       SHIFT is the LSQ matrix inversion shift
!P       ESD is the its esd.
!
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
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
!
      GOTO (1,1,1,4,4,4,4,4,4,10,11,12), ISPC
! X, Y OR Z:
    1 CALL ADJUST(X(ISPC,IGEN))
      SDX(ISPC,IGEN) = ESD
      GOTO 100
! B11, B22 ETC:
    4 IA = IAPT(IGEN)
      FAC = CONATF(ISPC-3,IA)
      ATF(ISPC-3,IA) = ATF(ISPC-3,IA)*FAC
      SHIFT = SHIFT*FAC
      ESD = ESD*FAC
      CALL ADJUST(ATF(ISPC-3,IA))
      ATF(ISPC-3,IA) = ATF(ISPC-3,IA)/FAC
      GOTO 100
! FORM/SCATTERING FACTOR:
   10 CALL ADJUST(CMULT(NFORMF(IGEN)))
      GOTO 100
! SITE OCCUPATION FACTOR:
   11 CALL ADJUST(SITE(IGEN))
      SDSITE(IGEN) = ESD
      GOTO 100
! ISOTROPIC TEMPERATURE FACTOR:
   12 CALL ADJUST(TF(IGEN))
      SDTF(IGEN) = ESD
  100 RETURN

      END SUBROUTINE F2SHFT
!
!*****************************************************************************
!
      SUBROUTINE F2VAR8(NG,NS,NV)
!
! *** F2VAR8 by JCM 17 Nov 90 ***
!
!X
!C 6A
!H Records varying information for a particular family 2 (structure) parameter.
!
!A On entry NG is the genus (which atom)
!A          NS is the species
!A          NV is which variable it will be
!D Records the information for future consultation
!
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
!
      GOTO (1,1,1,4,4,4,4,4,4,10,11,12), NS
! X, Y OR Z:
    1 KX(NS,NG) = NV
      GOTO 100
! B11, B22 ETC:
    4 KATF(NS-3,IAPT(NG)) = NV
      GOTO 100
! FORM/SCATTERING FACTOR:
   10 KCMULT(NG) = NV
      GOTO 100
! SITE OCCUPATION FACTOR:
   11 KSITE(NG) = NV
      GOTO 100
! ISOTROPIC TEMPERATURE FACTOR:
   12 KTF(NG) = NV
      GOTO 100
! TO CLEAR ALL FAMILY 2 VARIABLES TO BE FIXED:
      ENTRY F2VAR9
      DO IR = 1, 150
        DO I = 1, 3
          KX(I,IR) = 0
        ENDDO
        KCMULT(IR) = 0
        KSITE(IR) = 0
        KTF(IR) = 0
      ENDDO
      DO IR = 1, 50
        DO I = 1, 6
          KATF(I,IR) = 0
        ENDDO
      ENDDO
  100 RETURN

      END SUBROUTINE F2VAR8
!
!*****************************************************************************
!
      SUBROUTINE FACTGP(ISTAB,IFTAB,NFAC)
!
! *** FACTGP by PJB 13 Feb 90 ***
!
!X
!C 1A
!H Finds the factor elements which generate a space group from one of its
!H sub-groups.
!D To extract the factors  F(i) of space group G given a subgroup S
!D such that sum of F(i)xS=G
!
!A On entry:
!A          ISTAB(I) is positive if element I is in the sub-group.
!A          ISTAB(I) is negative if S is non-centric and only the centre
!A                   related partner is in S
!A          ISTAB(1) is zero if the sub-group is non-centrosymmetric.
!A On exit:
!A          IFTAB(I) defines the factorisation.
!A          IFTAB(I) is =1 if I is in S, I if I is one of F(i)
!A                   for all other elements IFTAB(N)=I where MULTAB(I,J)=N
!A                   and J is an element of S.
!A                   Negative values of the entries indicate that it is the
!A                   centre related partner that is required.
!A        IFTAB(1) = NFAC
!A          abs(NFAC) is the number of factors; NFAC is negative
!A                    if S is centro-symmetric.
!N The factors found by FACTGP need not necessarily form a complete group
!N Note also the existence of FACGRP which extracts complete factor groups
!
      DIMENSION ISTAB(24), IFTAB(24)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
!
!  MAKE INITIAL IFTAB FROM ISTAB
      IFTAB(1) = 1
      DO I = 2, NOPC
        IF (ISTAB(I).NE.0) THEN
          IFTAB(I) = ISIGN(1,ISTAB(I))
        ELSE
          IFTAB(I) = 0
        ENDIF
      ENDDO
      NFAC = 1
      DO I = 2, NOPC
! POTENTIAL FACTOR ELEMENT:
        IF (IFTAB(I).NE.0) GOTO 2
        IFTAB(I) = I
        NFAC = NFAC + 1
        DO K = 2, NOPC
          IF (IABS(IFTAB(K)).NE.1) GOTO 9
          J = MULTAB(K,I)
          IFTAB(J) = I*IFTAB(K)
    9   ENDDO
    2 ENDDO
      IF (ISTAB(1).NE.0) NFAC = -NFAC
      IFTAB(1) = NFAC

      END SUBROUTINE FACTGP
!
!*****************************************************************************
!
      COMPLEX FUNCTION FCALC(H)
!
! *** FCALC by JCM 19 Jul 83 ***
!
!X
!C 4B
!H Calculates the COMPLEX nuclear structure factor for the reflection H.
!A On entry H is a 1x3 read vector holding h,k,l.
!A On exit FCALC holds the COMPLEX nuclear structure factor
!P PREFIN, RECIP, SYMOP, SETFOR, ATOPOS and SETANI must be called before the
!P first call to FCALC.  (All these except PREFIN are all in SETFC)
!D Forms sin theta/lambda and leaves it in STHL in /BRAGG
!D Cycles over atomic positions, then over symmetry operators, forming
!D COMPLEX FCALC by the usual formula.
!D
!D Applies scattering factor, site occupation factor, multiplicity of atom and
!D individual isotropic or anisotropic temperature factors.
!
      COMPLEX SUM1, TERM, FORMFA, FO
      DIMENSION RH(3), H(3)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
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
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
!
      FCALC = CMPLX(0.,0.)
! CALCULATE SINTHETA/LAMBDA:
      STHL = VCTMOD(0.5,H,2)
! INITIALISE J, WHICH SAYS WHICH FORM FACTOR USED ON "PREVIOUS" ATOM:
      J = 0
! SUM OVER ATOMS IN CELL:
      DO N = 1, NATOM
        SUM1 = CMPLX(0.,0.)
! SUM OVER SYMMETRY EQUIVALENTS:
        DO I = 1, NOPC
          CALL ROTSYM(H,RH,I,-1)
          F1 = SCALPR(X(1,N),RH) + SCALPR(TRANS(1,I),H)
          TERM = CEXP(CMPLX(0.,TWOPI*F1))
! CALCULATE ANISOTROPIC TEMPERATURE FACTOR IF REQUIRED:
          TERM = TERM*ANITF(RH,N)
          SUM1 = SUM1 + TERM
        ENDDO
! IN CASE ANOM SCATT AND CENTROSYMMETRIC:
        IF (CENTRC) SUM1 = SUM1 + CONJG(SUM1)
        SUM1 = SUM1*AMULT(N)*SITE(N)
        IF (NFORMF(N).EQ.J) GOTO 1
! CALCULATE A NEW FORM FACTOR:
        J = NFORMF(N)
        FO = FORMFA(STHL,J)
    1   FCALC = FCALC + SUM1*FO*EXP(-TF(N)*STHL*STHL)
      ENDDO

      END FUNCTION FCALC
!
!*****************************************************************************
!
      SUBROUTINE FCTOR(H,N)
!
! *** FCTOR by PJB/JCM 28 Jun 83 ***
!
!X
!C 1C
!H Finds the highest common factor of a set of indices and reduces them
!H by that factor.
!A On entry H is a 1x3 array holding 3 reals, usually h,k,l
!A On exit N is the integer highest common factor of the elements of H, assumed
!A integral, and H has been divided through by N
!
      DIMENSION H(3), AH(3)

! USE ONLY MODULI:
      N = 0
      DO I = 1, 3
        AH(I) = ABS(H(I))
      ENDDO
! NMAX= LARGEST:
      NMAX = MAX1(AH(1),AH(2),AH(3))
! FOR 0,0,0 EXIT WITH N=0
      IF (NMAX.EQ.0) GOTO 100
      FN = FLOAT(NMAX)
! NOW SCAN EACH POSSIBLE HCF, DOWNWARDS:
      DO NN = 2, NMAX
        DO I = 1, 3
! TO 2 IF FN IS NOT A FACTOR OF ANY ONE ELEMENT:
          IF (AMOD(AH(I),FN).GT.10.E-4) GOTO 2
        ENDDO
! IF HERE, HCF FOUND; REDUCE ELEMENTS BY IT:
        DO I = 1, 3
          H(I) = H(I)/FN
        ENDDO
        GOTO 5
    2   FN = FN - 1.
      ENDDO
      FN = 1.
    5 N = NINT(FN)
  100 RETURN

      END SUBROUTINE FCTOR
!
!*****************************************************************************
!
      SUBROUTINE FETSHF(N,SH,ES)
!
! *** FETSHF updated by JCM 21 Mar 89 ***
!
!X
!C 6C
!H Fettles a shift and esd for printing and counts, in the application
!H of shifts in LSQ.
!A  On entry N indicates which action is required:
!A  N=1 initialise
!A  N=2 add in to totals
!A  N=3 print at cycle end
!A On entry 2 SH holds the shift
!A            ES holds the esd
!P In /NEWOLD/ ISHFT = number of shifts dealt with so far in this cycle,
!P             AVSHFT=the sum of their SHIFT/ESD so far,
!P             AMAXSH=the maximum SHIFT/ESD so far.
!D Updates ISHFT, AVSHFT and AMAXSH
!
      LOGICAL TESTOV
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      GOTO (1,2,3), N
! INITIALISE:
    1 AVSHFT = 0.
      ISHFT = 0
      AMAXSH = 0.
      GOTO 100
! ADD IN TOTALS:
    2 SHESD = 0.
      IF (.NOT.TESTOV(SH,ES)) SHESD = ABS(SH/ES)
      ISHFT = ISHFT + 1
      AVSHFT = AVSHFT + SHESD
      AMAXSH = MAX(AMAXSH,SHESD)
      GOTO 100
! PRINT AT CYCLE END:
    3 WRITE (LPT,2002) ICYC, AVSHFT/ISHFT, AMAXSH
 2002 FORMAT (//' Average SHIFT/ESD for cycle ',I3,' =',G14.5/' Maximum SHIFT/ESD = ',G14.5)
  100 RETURN

      END SUBROUTINE FETSHF
!
!*****************************************************************************
!
      CHARACTER*10 FUNCTION FILNOM(LUN)
!
! *** FILNOM by PJB Jan 86 **
!
!X
!C 13C
!H Returns the name of the file on FORTRAN unit LUN.
!A LUN on entry holds an input/output unit number
!A FILNOM is an A10 character variable which on exit holds the file name
!P NOPFIL (or OPNFIL) must have attached the unit number to the name in LUNTAB
!O If unit LUN is not in the table LUNTAB, an error message is given
!
      COMMON /FINAME/ FILNAM(15)
      CHARACTER*10 FILNAM
      COMMON /LOONEY/ IOTAB(15), LUNTAB(15)

      I = NFIND(LUN,LUNTAB,15)
      IF (I.EQ.0) THEN
        CALL ERRIN2(LUN,-1,'in FILNOM - unit','not in table LUNTAB')
      ELSE
        FILNOM = FILNAM(I)
      ENDIF

      END FUNCTION FILNOM
!
!*****************************************************************************
!
      SUBROUTINE FILPRO(DEFT,IU,LFIL)
!
! *** FILPRO updated by PJB for UNIX 28-Mar-1994 ***
!
!X
!C 13C
!H Makes sense of general file names, under VMS AND UNIX.
!
      CHARACTER*100 DEFT
      CHARACTER*100 BUFF
      CHARACTER*1 SEP(4)
      LOGICAL DOT
      DIMENSION IMTAB(4), JTAB(4), JMTAB(4), IPOS(5), MARK(5)
      COMMON /FINAME/ FILNAM(15)
      CHARACTER*10 FILNAM
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

!VMS
      DATA IMTAB, JTAB, JMTAB/4, 1, 2, 3, 2, 4, 3, 1, 2, 3, 4, 1/
      DATA IPOS/1, 5, 17, 23, 61/
      DATA SEP/'.', ':', '[', ']'/
!
! BRANCH ON SYSTEM, AS SET BY INITIL:
      IF (NSYSTM.EQ.4) GOTO 50
      IF (NSYSTM.LT.3) GOTO 60
! THIS SECTION FOR UNIX
! EXPAND NAMFIL AS INPUT IN BUF
!UNIX
      CALL UXEXPAND(NAMFIL,BUFF)
! FRIG DEFAULT DISC
      L = LENGT(DEFT(5:10))
      IF (L.GT.0) THEN
        IF (DEFT(5:5).EQ.'$') THEN
          M = 6
        ELSE
          M = 5
        ENDIF
        L = L + 4
        IF (DEFT(L:L).EQ.':') L = L - 1
!UNIX
!         CALL GETENV(DEFT(M:L),NAMFIL)
        IFP = LENGT(NAMFIL) + 1
        NAMFIL(IFP:IFP) = '/'
        IFP = IFP + 1
      ELSE
        IFP = 1
      ENDIF
! DEAL WITH DEFAULTS FOR PATH
      IF (LENGT(DEFT(11:30)).GT.0) THEN
!UNIX
        CALL UXEXPAND(DEFT(11:),NAMFIL(IFP:))
        IFP = LENGT(NAMFIL) + 1
      ENDIF
! ADD FILE PART
      L = LENGT(BUFF) + IFP - 1
      CALL ERRCHK(1,L,100,0,'File path too long')
      IF (IBMBER .NE. 0) RETURN
      NAMFIL(IFP:) = BUFF
      IFP = L + 1
! CHECK FOR EXTENSION
      DOT = (DEFT(1:1).NE.'.')
      DO I = L, 1, -1
        IF (NAMFIL(I:I).EQ.'/') GOTO 31
        IF (NAMFIL(I:I).NE.'.') GOTO 30
        DOT = .TRUE.
   30 ENDDO
   31 IF (.NOT.DOT) THEN
        L = L + LENGT(DEFT(1:4))
        CALL ERRCHK(1,L,100,0,'File path too long')
        IF (IBMBER .NE. 0) RETURN
        CALL UPONE(DEFT(1:4),NSYSTM)
        NAMFIL(IFP:) = DEFT(1:4)
      ENDIF
      LFIL = L - I
      CALL ERRCHK(1,LFIL,10,0,'Leaf name too long')
      IF (IBMBER .NE. 0) RETURN
      FILNAM(IU) = NAMFIL(I+1:L)
      LFIL = L
      GOTO 100
! THIS SECTION FOR VMS
   60 CALL JGMZER(MARK,1,4)
      L = LENG(NAMFIL,100)
      MARK(5) = L + 1
!  SEPARATE UP THE FILE-NAME
      JJ = 0
      DO I = 1, L
        DO J = 1, 4
          IF (NAMFIL(I:I).NE.SEP(JMTAB(J))) GOTO 2
!  SKIP SEPARATORS INSIDE SQUARE BRACKETS
          IF (JJ.GT.0) THEN
            IF (JMTAB(JJ).EQ.3 .AND. JMTAB(J).NE.4) GOTO 1
          ENDIF
!  FORCE CORRECT ORDER
          IF (JJ.GE.J) GOTO 20
          JJ = J
          MARK(J) = I
          GOTO 1
    2   ENDDO
    1 ENDDO
!  PROCESS EACH PART SEPARATELY
      DO IP = 1, 4
        GOTO (6,7,8,9), IP
!  EXTENSION
    6   M = MARK(IMTAB(IP))
        IF (M.NE.0) THEN
          JP = IMTAB(IP) + 1
          GOTO 10
        ELSE
          IF (DEFT(1:1).NE.'.') DEFT(1:4) = '.DAT'
          GOTO 5
        ENDIF
!  DISC
    7   JP = IMTAB(IP)
        MM = MARK(JP)
        IF (MM.EQ.0) GOTO 5
        GOTO 12
!  NAME
    8   JP = IMTAB(1)
        GOTO 12
!  DIRECTORY PATH
    9   M = MARK(IMTAB(IP-1))
        MM = MARK(IMTAB(IP))
        IF (M.EQ.0) GOTO 5
        IF (M.GE.MM) GOTO 20
        GOTO 4
!  WRITE THE DIFFERENT PARTS TO DEFT
!  SEARCH BACKWARDS FOR NEXT SEPARATOR
   12   DO I = JP - 1, 1, -1
          M = MARK(I) + 1
          IF (M.GT.1) GOTO 10
        ENDDO
        M = 1
!  AND THEN FORWARDS IF NECESSARY
   10   IF (IP.EQ.2) GOTO 4
        DO I = JP, 5
          MM = MARK(I) - 1
          IF (MM.GT.0) GOTO 4
        ENDDO
        MM = L
    4   DEFT(IPOS(IP):IPOS(IP+1)-1) = NAMFIL(M:MM)
    5 ENDDO
!  CONDENSE AND WRITE BACK TO NAMFIL
      IP = 1
      DO JP = 1, 4
        I = JTAB(JP)
        L = LENG(DEFT(IPOS(I):),IPOS(I+1)-IPOS(I))
        NAMFIL(IP:) = DEFT(IPOS(I):IPOS(I+1)-1)
        IF (I.EQ.3) M = IP
        IP = IP + L
        IF (I.EQ.1) MM = IP - 1
      ENDDO
      FILNAM(IU) = NAMFIL(M:MM)
      LFIL = IP - 1
      GOTO 100
!  ERROR
   20 CALL ErrorMessage('Illegal file-name : '//NAMFIL)
      LFIL = 0
      GOTO 100
! IBM, FOR NOW:
   50 DO I = 1, 40
        IF (NAMFIL(I:I).EQ.'.') GOTO 52
      ENDDO
      I = 0
   52 FILNAM(IU) = NAMFIL(I+1:I+10)
      LFIL = 40
  100 RETURN

      END SUBROUTINE FILPRO
!
!*****************************************************************************
!
      SUBROUTINE FINDCD(CH,WORD,LEN,K,LCD)
!
! *** FINDCD updated by JCM 2 Feb 88 ***
!
!X
!C 13C
!H Searches for a card starting with letter CH and with WORD in columns 3-6.
!A On entry CH is a single character with which the card is required to start
!A          WORD is an A4 character variable required in columns 3-6
!A          LEN is the number of characters of WORD required to match (=<4)
!A          K points to the last read card:
!A                   K=0 means start at the beginning of the CH cards
!A                   K>0 implies that the Kth card was a CH card
!A On exit  LCD indicates whether such a card has been found:
!A                   LCD=-1 if no cards are found starting CH
!A                      = 0 if some start CH, but no "WORD"
!A                      >=1 if card found, and then LCD is its position in
!A                          the Crystal Data File.
!D The search starts at the K+1th card of the whole crystal data
!D If a card is found, a copy of the card in A80 FORMAT is in ICARD in /SCRACH
!
!
      CHARACTER*1 CH
      CHARACTER*4 WORD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
      L = -1
      I = LETTER(CH)
! IF NO CH CARDS AT ALL, EXIT:
      IF (ICDNO(I).EQ.0) GOTO 101
      ID = K + 1
      IF (K.EQ.0) ID = IABS(INREAD(I))
! READ NEXT CARD:
    1 IF (ID.GT.NTOTAL(JPHASE)) GOTO 102
      CALL CARDIN(ID)
! WE HAVE TO DO SOMETHING ABOUT CDFS ENDING Y OR Z
      IF (NYZ.EQ.-1) GOTO 102
      ID = ID + NYZ
      L = ID - 1
! CHECK WE STILL HAVE CH CARDS:
      IF (ICARD(1:1).NE.CH) GOTO 102
! IGNORE INTERVENING SPACES:
      DO IST = 2, 80
        IF (ICARD(IST:IST).NE.' ') GOTO 87
      ENDDO
      GOTO 1
   87 IF (ICARD(IST:IST+LEN-1).NE.WORD(1:LEN)) GOTO 1
! FOUND CARD:
      GOTO 101
  102 L = 0
  101 LCD = L

      END SUBROUTINE FINDCD
!
!*****************************************************************************
!
      SUBROUTINE FIXPAR(NP,NFIX)
!
! *** FIXPAR by JCM 13 Jul 83 ***
!
!X
!C 6A
!H Records an instruction to fix a LSQ parameter.
!A On entry NP= which parameter to fix, assuming that there is a new
!A      numbering of parameters so that NP is an address in the array
!A      NFIX.  All parameters which may be involved with NP must also
!A      have addresses within NFIX.
!A      NFIX is an integer array containing potential chaining information
!A           for the relevant parameters.
!D Records the fixing of the given parameter, and any chained to it.
!
      DIMENSION NFIX(1)
!
      I = NP
    1 IOLD = I
      I = NFIX(I)
      NFIX(IOLD) = 0
! I=WHAT WAS THERE FOR CHAINING
      IF ((I.NE.0) .AND. (I.NE.9999)) GOTO 1
! UNCHAIN IF FIXED PAR WAS ALREADY RELATED TO ANOTHER
! OUT IF PAR ALREADY FIXED, OR PAR UNREFERENCED SO FAR

      END SUBROUTINE FIXPAR
!
!*****************************************************************************
!
      SUBROUTINE FIXREL(N,NFIX,FIX,KKLIST,NSTAT)
!
! *** FIXREL by JCM 11 Jan 88 ***
!
!X
!C 6A
!H Takes a temporary set of fix/relate information and adds it to the
!H permanent information.
!A          NFIX, FIX contain their temporary fix/relate info out of
!A                    FIXPAR, RELPAR
!A          KKLIST is a list of KK (parameter spec) values corresponding to
!A                    the entries in NFIX
!A          NSTAT is the status to be given to any FIX or CON  info
!
      DIMENSION NFIX(N), FIX(N), KKLIST(N), KK1(2), AM(2)
!
      DO I = 1, N
! 9999 MEANS NOTHING KNOWN ABOUT THIS PARAMETER:
        IF (NFIX(I).EQ.9999) GOTO 1
! -VE MEANS "THIS PARAMETER ALREADY DEALT WITH BY THIS ROUTINE"
        IF (NFIX(I).LT.0) GOTO 1
! 0 MEANS "FIXED":
        IF (NFIX(I).EQ.0) THEN
          CALL ADDFIX(KKLIST(I),NSTAT)
          GOTO 1
        ENDIF
! PARAMETER INVOLVED IN (AT LEAST 1) CONSTRAINT:
        I1 = I
    2   I2 = IABS(NFIX(I1))
! IF CHAIN CLOSED, THIS IS NOT AN EXTRA CONSTRAINT:
        IF (I2.EQ.I) GOTO 1
        KK1(1) = KKLIST(I)
        KK1(2) = KKLIST(I2)
        AM(1) = FIX(I)
        AM(2) = -FIX(I2)
        CALL ADDCON(2,KK1,AM,NSTAT)
        NFIX(I2) = -NFIX(I2)
!
        I1 = I2
        GOTO 2
    1 ENDDO
!
! RESTORE NFIX +VE:
      DO I = 1, N
        NFIX(I) = IABS(NFIX(I))
      ENDDO
      RETURN
      END SUBROUTINE FIXREL
!
!*****************************************************************************
!
      SUBROUTINE FIXUNI(A,NDO)
!
! *** FIXUNI updated by JCM 26 Sep 84 ***
!
!X
!C 1A
!H Deals with one potential plane face of asymmetric unit, while the unit
!H is being formed.
!
!A On entry NDO indicates the required action:
!A     If NDO is -ve, removes plane number -NDO.
!A     If NDO is +ve, it is the status of plane A, which is to be added
!A            if possible.
!A     A contains the direction cosines of the normal to the offered plane.
!
!D Calls TRYUNI to test particular possible units.  Tests "NICE" on return:
!D   NICE=0  OK, we have a unit of right size with 1 typical refln in it
!D   NICE=1  Unit too big - continue
!D   NICE=-1 Unit not possible - either it is too small, or there is no
!D           typical reflection there at all.
!D
!D TRYUNI also sends back VOL=number of times too big/small unit is, or
!D VOL=0. if there are 3 planes but they form a hinge.
!D
!D Called repeatedly from SYMUNI, which decides what to offer or remove.
!D FIXUNI deals with the "NICE=-1" and the hinge conditions before
!D returning to SYMUNI.
!
      DIMENSION A(3), PTEMP(3)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRAT / AXI(3,24,2), MIRROR(24), D(3,3), PL1(3), PL2(3),  &
     &                PL3(3), HT(3), ASY(3,4), NSTAT(4), NOPL, NICE,    &
     &                VOL, MOP1, MOP2

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (NDO.GT.0) GOTO 1
      J = -NDO
      IF (NSTAT(J).EQ.0) RETURN
! REMOVE PLANE IF THERE:
      NSTAT(J) = 0
      NOPL = NOPL - 1
      RETURN
! ADD PLANE A IF POSSIBLE;  IF NONE THERE ALREADY, SIMPLY ACCEPT:
    1 CALL FCTOR(A,N)
      IF (NOPL.GT.0) GOTO 2
      NOPL = 1
      NSTAT(1) = NDO
      NNEW = 1
      CALL GMEQ(A,ASY(1,1),1,3)
      GOTO 10
! SOMETHING THERE ALREADY - IF 4, TOO MANY:
    2 IF (NOPL.LT.4) GOTO 3
      WRITE (LPT,3000) A
      CALL BMBOUT
      RETURN
! CHECK WE DO NOT ALREADY HAVE A OR -A:
    3 DO I = 1, 4
        IF (NSTAT(I).EQ.0) GOTO 4
        CALL VECPRD(A,ASY(1,I),PTEMP)
        IF (VCTMOD(1.0,PTEMP,1).LT.0.0001) RETURN
    4 ENDDO
! A IS NEW;  ADD IT, EVEN IF FOR NOW TO POSITION 4:
      DO NNEW = 1, 4
        IF (NSTAT(NNEW).EQ.0) GOTO 8
      ENDDO
! SHOULD NOT BE HERE - ALL 4 SLOTS IN ASY (AND HENCE NSTAT) SHOULD NOT BE FULL:
      WRITE (LPT,3001) NOPL
      CALL BMBOUT
      RETURN
    8 NOPL = NOPL + 1
      NSTAT(NNEW) = NDO
      CALL GMEQ(A,ASY(1,NNEW),1,3)
! NOW TEST NEW CONFIGURATION - IF 4 PLANES JOIN PART WHICH TAKES ONLY 3:
   10 IF (NOPL.GE.4) GOTO 11
      CALL TRYUNI(0)
      IF (IBMBER .NE. 0) RETURN
      IF (NICE) 12, 100, 13
! PLANE NOT TO BE USED - REMOVE:
   12 NOPL = NOPL - 1
      NSTAT(NNEW) = 0
      GOTO 100
! PLANE OK BUT VOL TOO BIG (OR A HINGE):
   13 IF (NOPL.NE.3) GOTO 100
! IF ONLY 1 OR 2 PLANES, NOTHING MORE WE CAN DO; (4 PLANES NOT HERE)
      IF (VOL.NE.0.) GOTO 100
! HERE IF 3 PLANES ALL JOINING AT HINGE:
   11 NOPL = NOPL - 1
! WE OFFER TO TRYUNI SUBSETS OF WHAT WE HAVE IN ASY, OMITTING ONE PLANE AT ONCE
      OLDVOL = 100.
      DO I = 1, 4
! NOT IF PLANE NOT THERE:
        IF (NSTAT(I).EQ.0) GOTO 14
! DO NOT OMIT A MANDATORY PLANE:
        IF (NSTAT(I).EQ.2) GOTO 14
! OR THE NEWLY OFFERED ONE:
        IF (I.EQ.NNEW) GOTO 14
        NSTATS = NSTAT(I)
        NSTAT(I) = 0
        CALL TRYUNI(0)
        IF (IBMBER .NE. 0) RETURN
        IF (NICE) 5, 100, 15
! IF NICE EVER BECOMES -1, OMIT NEW PLANE:
    5   NSTAT(I) = NSTATS
        NSTAT(NNEW) = 0
        GOTO 100
! VOL IS STILL TOO LARGE:
   15   IF (VOL.GT.OLDVOL) GOTO 6
! KEEP SMALLEST FOUND VOL
        OLDVOL = VOL
! PUT PLANE BACK:
        N1 = I
    6   NSTAT(I) = NSTATS
   14 ENDDO
! STILL TOO BIG, BUT SHOULD HAVE SMALLEST VOL OF HINGE IN OLDVOL, AND THE PLANE
! WE OMITTED TO GET IT IN N1:
      NSTAT(N1) = 0
! TIDY UP AGAIN:
      CALL TRYUNI(0)
  100 RETURN
 3000 FORMAT (/' *** PROGRAM ERROR - OFFERING PLANE',3F5.0,' TO FIXUNI, WITH 4 PLANES ALREADY')
 3001 FORMAT (/' *** PROGRAM ERROR IN FIXUNI - ASY FULL BUT NOPL=',I3)

      END SUBROUTINE FIXUNI
!
!*****************************************************************************
!
      SUBROUTINE FIXVAR(FX,IFAM,IGEN,ISPC,KP,KS,NSTAT)
!
! *** FIXVAR by JCM 9 Nov 90 ***
!
!X
!C 6C
!H Adds a request to fix (or vary) a parameter to the lists held in setting
!H up LSQ environments.
!A On entry:
!A For entries FIXVAR, ADDFX5, ADDVR5:
!A    IFAM, IGEN, ISPC are the family, genus & species of the parameter
!A    KP, KS are the phase & source if relevant, or may be zero
!A For entry FVKPAK, ADDFIX, ADDVAR
!A          KK is the packed parameter spec, possibly incomplete
!A          NSTAT is the status of the request (5=not changeable)
!A                                             (4=changeable)
!P LSETUP must have initialised the list
!D Ensures an entry in the fix/vary list for the given parameter.
!D If there already was one for exactly this parameter, alters it as
!D requested, checking that the request is reasonable.
!D Records whether the request was fix or vary in KSTFV, as + or -
!D Records status of request also in KSTFV.
!D Records time of request in KTIME, so that conflict can be resolved.
!D Keeps count of total number of such requests in NUMFV
!D Sets KTYPFV to 0 if the packed KK was complete (ie specific), or an
!D address in the table KUNPFV into which the incomplete KK has been
!D unpacked for future reference.
!N Also entries ADDFIX, ADDVAR, FVKPAK
!
      LOGICAL FX, FIX, KWHOLE
      CHARACTER*4 FV(2), NAM1, NAM2
      DIMENSION K(5)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LINKAG/ NUMFV, NUMPAK, KKFV(200), KTYPFV(200), KSTFV(200),&
     &                KTIME(200), KUNPFV(5,30), NTIME, NUMCON,          &
     &                KKCON(500), AMCON(500), KPTCON(201), KSTCON(200), &
     &                KTPCON(200)

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

      DATA FV/'fix', 'vary'/

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! EITHER FIX OR VARY, GIVEN FAMILY, GENUS & SPECIES WITH PHASE & SOURCE
      KK = KPAK(IFAM,IGEN,ISPC,KP,KS)
      FIX = FX
      GOTO 1
! FIXING ENTRY WITH UNPACKED 5:
      ENTRY ADDFX5(IFAM,IGEN,ISPC,KP,KS,NSTAT)
      KK = KPAK(IFAM,IGEN,ISPC,KP,KS)
      FIX = .TRUE.
      GOTO 1
! VARYING ENTRY WITH UNPACKED 5:
      ENTRY ADDVR5(IFAM,IGEN,ISPC,KP,KS,NSTAT)
      KK = KPAK(IFAM,IGEN,ISPC,KP,KS)
      FIX = .FALSE.
      GOTO 1
! FIXING ENTRY WITH PACKED PARAMETER SPEC:
      ENTRY ADDFIX(KKK,NSTAT)
      FIX = .TRUE.
      GOTO 10
! VARYING ENTRY WITH PACKED PARAMETER SPEC:
      ENTRY ADDVAR(KKK,NSTAT)
      FIX = .FALSE.
      GOTO 10
! EITHER FIX OR VARY ENTRY, WITH PACKED KK:
      ENTRY FVKPAK(KKK,NSTAT,FX)
      FIX = FX
   10 KK = KKK
! IS IT THERE ALREADY?
    1 IF (NUMFV.EQ.0) GOTO 2
      N = NFIND(KK,KKFV,NUMFV)
      IF (N.EQ.0) GOTO 2
! IF THERE, RECORD OLD AND NEW FIX OR VARY INDICATORS:
      I1 = 1
      IF (.NOT.FIX) I1 = 2
      I2 = 1
      IF (KSTFV(N).LT.0) I2 = 2
! CANNOT LOWER STATUS:
      IF (NSTAT.GE.IABS(KSTFV(N))) GOTO 3
! THIS IS A SIMPLIFICATION COVERING EXISTING CASES:
      CALL PARNAM(NAM1,NAM2,3,KK)
      CALL MESS(LPT,0,'Ignored request to '//FV(I1)//' parameter '//NAM1//NAM2//' - fixed by symmetry')
      GOTO 100
! A NEW REQUEST - ACCEPT IT:
    2 CALL ERRCHK(2,NUMFV,200,0,'fix or vary requests')
      IF (IBMBER .NE. 0) RETURN
      N = NUMFV
      KKFV(N) = KK
! RECORD WHETHER OR NOT KK COMPLETE:
      KTYPFV(N) = 0
      IF (.NOT.KWHOLE(KK,K)) THEN
        CALL ERRCHK(2,NUMPAK,30,0,'incomplete fix/vary requests')
        IF (IBMBER .NE. 0) RETURN
        KTYPFV(N) = NUMPAK
        CALL GMEQ(K,KUNPFV(1,NUMPAK),1,5)
      ENDIF
! JOIN HERE TO UPDATE AN EXISTING ENTRY:
    3 KSTFV(N) = NSTAT
      IF (.NOT.FIX) KSTFV(N) = -NSTAT
      KTIME(N) = NTICK(NTIME)
  100 RETURN

      END SUBROUTINE FIXVAR
!
!*****************************************************************************
!
      SUBROUTINE FLIP(I,J)
!
! *** FLIP by JCM 4 Feb 88 ***
!
!X
!C 16C
!H Exchanges the integers I and J.
!A On entry I and J holds certain values
!A On exit their values have been exchanged.
!
      K = I
      I = J
      J = K

      END SUBROUTINE FLIP
!
!*****************************************************************************
!
      COMPLEX FUNCTION FORMFA(AK,II)
!
! *** FORMFA updated by JCM 25 Jan 91 ***
!
!X
!C 4B
!H Calculates form or scattering factors.
!A On entry AK=sin theta/lambda if relevant
!A          II=which factor is required
!A On exit  FORMFA holds the factor.
!P SETFOR must have read and interpreted F cards.
!
!D Allows 5 types of scattering factor, depending on MODE(I):
!D    MODE(I)=1 neutron nuclear factor, the COMPLEX value (CMULT(I),0.)
!D            2 exponential series of NT(I) terms held in F(,I)
!D            3 interpolation in table of NT(I) entries held in S(,I) and F(,I)
!D            4 as 2, but also times sin theta/lambda squared.
!D            5 Form factor to be calculated from radial wave-functions
!D              given on W cards and read by RADFUN (allowed by SETFOR
!D              but not included in FORMFA;  see FORFAC)
!D    Types 2,3 and 4 may also be multiplied by a constant in CMULT(I)
!D
!D If NAMODE(I)=1, an anomalous scattering factor is added from FDASH(,I)
!O For type 3, if AK is given outside the range in S(,I), an error message is
!O given and the routine stops.
!
      COMMON /ANSCAT/ NAMODE(20), FDASH(20), KOM2
      COMPLEX FDASH
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
!
      I = II
      M = MODE(I)
      GOTO (1,2,3,2,100), M
!     NEUTRON NUCLEAR SCATTERING:
    1 G = 1.0
      GOTO 20
!     FORM FACTORS AS EXPONENTIAL SERIES:
    2 N = NT(I)
      G = F(N,I)
      N = N - 2
      DO L = 1, N, 2
        ARG = F(L+1,I)*AK*AK
        IF (ARG.LE.174) G = G + F(L,I)*EXP(-ARG)
      ENDDO
      IF (M.EQ.4) G = G*AK*AK
      GOTO 20
!     FORM FACTORS GIVEN IN A TABLE:
    3 N = NT(I)
      IF ((AK.LE.S(N,I)) .AND. (AK.GE.S(1,I))) GOTO 14
      WRITE (LPT,3000) AK, I
      CALL BMBOUT
      RETURN
   14 IF (N.LT.5) THEN
        NN = N
        MM = 1
        GOTO 11
      ENDIF
      D1 = ABS(AK-S(1,I))
      DO J = 2, N
        D = ABS(AK-S(J,I))
        IF (D.GT.D1) GOTO 13
        D1 = D
      ENDDO
      M = N
      GOTO 8
   13 M = J - 1
      IF (M.GT.2) GOTO 8
      MM = 1
      GOTO 12
    8 IF (M+2.GT.N) THEN
        MM = N - 4
      ELSE
        MM = M - 2
      ENDIF
   12 NN = 5
   11 CALL TB02A(S(MM,I),F(MM,I),AK,G,NN)
   20 FORMFA = CMPLX(G*CMULT(I),0.)
! ANOMALOUS SCATTERING IF GIVEN:
  100 IF (NAMODE(I).EQ.1) FORMFA = FORMFA + FDASH(I)
      RETURN
 3000 FORMAT (/' ERROR **  sin theta/lamda =',F6.3,                     &
     &        ' outside range of table for form factor number',I3)
      END FUNCTION FORMFA
!
!*****************************************************************************
!
      SUBROUTINE FUDGET(IPT,ITYP,F1,F2)
!
! *** FUDGET by JCM 10 Feb 87 ***
!
!X
!C 6C
!H Reads a fudge factor from a card having already read a parameter
!H specification.
!
!A IPT on entry points at next char on card ICARD
!A IPT on exit has been advanced by the amount read
!A ITYP on exit = type of factor read
!A F1 on exit = first no. read if appropriate
!A F2 on exit = second no. read if appropriate
!P Card is held in /SCRACH/ in ICARD
!
!D Reads one of:
!D    a number into F1 (setting ITYP=1)
!D    "GE" number into F1 (setting ITYP=2)
!D    "LE" number into F2 (setting ITYP=3)
!D    both the above, setting ITYP=4
!
      CHARACTER*10 WORD
      ITYP = 1
      IPKEEP = IPT
      CALL RDREAL(F1,IPT,IPT,80,IER)
! IF NUMBER READ, TYPE 1 SIMPLE MULTIPLICATIVE FACTOR:
      IF (IER.EQ.0) GOTO 100
!
! WORD READ - EXPECT GE OR LE
      IPT = IPKEEP
   42 IPKEEP = IPT
      CALL RDWORD(WORD,LEN,IPT,IPT,80,0,IER)
      IF (WORD.NE.'GE') GOTO 41
      IF (ITYP.GT.1) ITYP = 4
      IF (ITYP.EQ.1) ITYP = 2
      CALL RDREAL(F1,IPT,IPT,80,IER)
      GOTO 42
   41 IF (WORD.NE.'LE') GOTO 43
      IF (ITYP.GT.1) ITYP = 4
      IF (ITYP.EQ.1) ITYP = 3
      CALL RDREAL(F2,IPT,IPT,80,IER)
      GOTO 42
!
! NEITHER GE NOR LE - MAY BE NEXT PARAMETER SPEC:
   43 IPT = IPKEEP
  100 RETURN
      END SUBROUTINE FUDGET
!
!*****************************************************************************
!
      SUBROUTINE FUDGIN
!
! *** FUDGIN updated by JCM 27 Apr 92 ***
!
!X
!C 6A
!H Interprets all L FUDG cards.
!D Sets NFUDGE in /FUDG/ to be the number of fudge factors read.
!D Reads from the cards sets of <parameter specification> <fudge factor>
!D The parameter specification may be any of those described under PARRD
!D The fudge factor may be one of:
!D    1)  A simple multiplicative factor
!D    2)  GE followed by a lower limit
!D    3)  LE followed by an upper limit
!D    4)  both 2) and 3) in either order
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FUDG  / NFUDGE, IFDGPT(20), FUDGE1(20), FUDGE2(20),       &
     &                IFDTYP(20)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      IN = 0
! NEXT "L FUDG" CARD:
   31 CALL FINDCD('L','FUDG',4,IN,L)
      IF (L.LE.0) GOTO 100
      CALL MESS(LPT,0,'Fudge factor:')
      CALL MESS(LPT,0,ICARD(7:80))
      IN = L
      IPT = 7
! READ NEXT PARAMETER SPEC ON CARD:
    3 CALL PARRD(IPT,IPT,K,IFAM,IGEN,ISPC)
      IF (K) 1, 31, 2
! K +VE - A PACKED PARAMETER SPEC:
    2 IER = IERR
      CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
      IF (IER.NE.IERR) GOTO 100
! READ FUDGE FACTOR:
      IFDGPT(NFUDGE) = K
      CALL FUDGET(IPT,IFDTYP(NFUDGE),FUDGE1(NFUDGE),FUDGE2(NFUDGE))
      GOTO 3
! K -VE - A WORD LIKE ALL(-100), ONLY (-99)
    1 I = -K - 98
      GOTO (11,12), I
! 'ONLY' SHOULD BE A MISTAKE HERE:
   11 CALL ERRMES(1,1,'"ONLY" not allowed on L FUDG card')
      GOTO 3
! 'ALL' SHOULD BE ACCOMPANIED BY EITHER +VE IFAM, MEANING THAT A (POSSIBLY
!  PARTIAL) PARAMETER SPEC IS GIVEN, OR -VE IFAM, MEANING THAT A COMPOSITE
! WORD FOLLOWED THE 'ALL':
! IFAM=-1 XYZ, -2 BIJ, -3 XYZT, -4 CELL, -5 XYZB, -6 XYZS
! BY CONVENTION - THIS MUST HAVE BEEN BUILT IN TO THE MAIN PROGRAM
   12 IF (IFAM.LE.0) GOTO 5
      K = KPAK(IFAM,IGEN,ISPC,KPHASE,KSOURC)
      GOTO 2
    5 GOTO (21,22,21,24,25,21), -IFAM
! 'XYZB' - SET SPECIES 1-9 (ALL GENERA, FAMILY 2)
   25 L1 = 1
      GOTO 19
! 'BIJ' - SET SPECIES 4-9 (ALL GENERA, FAMILY 2)
   22 L1 = 4
   19 L2 = 9
      N1 = 2
      N2 = 0
      GOTO 20
! 'CELL' - SET SPECIES 2-7, FAMILY 1, GENUS 1:
   24 L1 = 2
      L2 = 7
      N1 = 1
      N2 = 1
      GOTO 20
! 'XYZ' - SET SPECIES 1-3 (ALL GENERA, FAMILY 2) - ALSO XYZT WITH ADDED 12
! AND XYZS WITH ADDED 11
   21 L1 = 1
      L2 = 3
      N1 = 2
      N2 = 0
   20 CALL FUDGET(IPT,ITYP,F1,F2)
      DO I = L1, L2
        IER = IERR
        CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
        IF (IER.NE.IERR) GOTO 100
        IFDTYP(NFUDGE) = ITYP
        FUDGE1(NFUDGE) = F1
        FUDGE2(NFUDGE) = F2
        IFDGPT(NFUDGE) = KPAK(N1,N2,I,KPHASE,1)
      ENDDO
      IF (IFAM.EQ.-3) THEN
        IER = IERR
        CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
        IF (IER.NE.IERR) GOTO 100
        IFDTYP(NFUDGE) = ITYP
        FUDGE1(NFUDGE) = F1
        FUDGE2(NFUDGE) = F2
        IFDGPT(NFUDGE) = KPAK(2,0,12,KPHASE,1)
      ELSEIF (IFAM.EQ.-6) THEN
        IER = IERR
        CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
        IF (IER.NE.IERR) GOTO 100
        IFDTYP(NFUDGE) = ITYP
        FUDGE1(NFUDGE) = F1
        FUDGE2(NFUDGE) = F2
        IFDGPT(NFUDGE) = KPAK(2,0,11,KPHASE,1)
      ENDIF
      GOTO 3
  100 RETURN

      END SUBROUTINE FUDGIN
!
!*****************************************************************************
!
      SUBROUTINE GENELM(NSUB,ISGEN)
!
! *** GENELM corrected by JCM 18 Sep 89 ***
!
!X
!C 1B
!H Finds the generators of a subgroup of a space group.
!A On entry NSUB is an integer array holding the orders of the elements of the
!A               subgroup, with zeros for those elements not in the subgroup.
!A          ISGEN(1) is the multiplicity of the subgroup
!A On exit  ISGEN(2) (and (3) if necessary) contain the labels of the
!A               generators.
!
      LOGICAL FIRST
      DIMENSION NSUB(24), ISGEN(3), ISIG(2)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

!  JUMP OUT FOR P1 OR P-1:
      ISGEN(2) = 1
      ISGEN(3) = 0
      IF (IABS(ISGEN(1)).EQ.1) GOTO 100
! PUT ZEROS INTO NSUB FOR MULTIPLE OPERATIONS OF A SINGLE OPERATOR:
      DO NO = 2, NOPC
        M = NSUB(NO)
        IF (M.EQ.0) GOTO 1
        N = NO
        DO J = 3, IABS(M)
          N = MULTAB(NO,N)
          NSUB(N) = 0
        ENDDO
    1 ENDDO
      NGEN = 1
! COUNT DOWN THROUGH ORDERS OF POSSIBLE SYMMETRY ELEMENTS:
      DO NN = 1, 5
        IORD = 7 - NN
        IF (IORD.EQ.5) GOTO 4
!  OPERATORS OF ORDER 5 DONT OCCUR
        FIRST = .TRUE.
        DO NNN = 1, NOPC
          IF (IABS(NSUB(NNN)).NE.IORD) GOTO 5
!  OPERATOR OF ORDER IORD FOUND - MARK IT USED, KEEPING ITS SIGN:
          ISIG(NGEN) = ISIGN(1,NSUB(NNN))
          NSUB(NNN) = 0
! IF NOT FIRST OF THIS ORDER, WE NEED A SECOND GENERATOR TO PRODUCE IT - JUMP:
          IF (.NOT.FIRST) GOTO 6
! THE FIRST OPERATOR OF THIS ORDER:
! JUMP IF ALREADY HAVE A GENERATOR OF HIGHER ORDER:
          IF (NGEN.EQ.2) GOTO 10
          FIRST = .FALSE.
          ISGEN(2) = NNN*ISIG(1)
          ISG = NNN
          NGEN = 2
          GOTO 5
!  HERE IF NOT FIRST OF THIS ORDER:
    6     DO M = 1, NOP
! FIND IN M THE OPERATOR WHICH MAKES CURRENT ONE FROM FIRST GENERATOR:
            IF (MULTAB(ISG,M).EQ.NNN) GOTO 3
          ENDDO
! I THINK IT IS AN ERROR IF WE GET HERE?
          GOTO 100
    5   ENDDO
    4 ENDDO
!  NO SECOND GENERATOR - RETURN
      GOTO 100
!  SET SECOND GENERATOR:
   10 ISGEN(3) = NNN*ISIG(2)
      GOTO 100
    3 ISGEN(3) = M*ISIG(1)*ISIG(2)
  100 RETURN

      END SUBROUTINE GENELM
!
!*****************************************************************************
!
      SUBROUTINE GENMUL(H,NOMORE,M)
!
! *** GENMUL by JCM 18 Jun 85 ***
!
!X
!C 3B
!H Gives next useful set of h,k,l and multiplicity, scanning the asymmetric
!H unit of reciprocal space.
!
!A On exit  H is a 1x3 real vector holding next generated values of h,k,l
!A          NOMORE is TRUE if there are no more to be found
!A          M is the multiplicity of the found reflection.
!P SYMOP should have read the space group symmetry.
!P SYMUNI should have found the asymmetric unit of reciprocal space.
!P SETGEN should have set up stepping over asymmetric unit in /HKLGEN
!
!D Uses "previous" h,k,l in PT in /HKLGEN to move to a new one.
!D Rejects lattice absences (but not space group absences - do those outside
!D                 using ISPABS if required),
!D         h,k,l outside asymmetric unit,
!D         h,k,l for which sin theta/lambda is greater than STHLMX,
!D         and h,k,l for which sin theta is around zero (to reject 0,0,0)
!D
!D Allows for non-primitive stepping vectors in array STEP, by use of inter-
!D mediate primitive steps as calculated by PRMTIV on leaving SETGEN.
!D
!D Leaves value of sin theta/lambda in STHL in /BRAGG
!N Exactly like GETGEN, but sends out M and STHL also
!
      LOGICAL NOMORE, LATABS
      DIMENSION H(3), C(2), VEC(3)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /HKLGEN/ STEP(3,3), PT(3,3), VECEND(3,3), PRPT(3,3),       &
     &                NPRIM(2,2), NP, LFAC(2), MCOUNT(2), KOM5

      NOMORE = .FALSE.
    4 DO L = 1, 3
        CALL GMADD(PT(1,L),STEP(1,L),H,1,3)
        IF (SCALPR(H,VECEND(1,L))-1.) 2, 2, 1
    1 ENDDO
! NEXT 3D LATTICE IF NON-PRIMITIVE:
! SIMULATE 'DO MCOUNT(1)=1,LFAC(1)'
      MCOUNT(1) = MCOUNT(1) + 1
      IF (MCOUNT(1).LE.LFAC(1)) GOTO 8
      MCOUNT(1) = 1
! SIMULATE 'DO MCOUNT(2)=1,LFAC(2)
      MCOUNT(2) = MCOUNT(2) + 1
      IF (MCOUNT(2).GT.LFAC(2)) GOTO 101
! MAKE COEFFICIENTS OF STEP VECTORS FOR NEXT PRIMITIVE STEP:
    8 DO I = 1, 2
        C(I) = FLOAT(MOD((MCOUNT(1)-1)*NPRIM(I,1)*LFAC(2)+(MCOUNT(2)-1)*&
     &         NPRIM(I,2)*LFAC(1),NP))/FLOAT(NP)
      ENDDO
      DO I = 1, 3
        VEC(I) = C(1)*STEP(I,1) + C(2)*STEP(I,2) + FLOAT(MCOUNT(2)-1) *STEP(I,3)/FLOAT(LFAC(2))
      ENDDO
      DO L = 1, 3
        CALL GMADD(PRPT(1,L),VEC,PT(1,L),1,3)
      ENDDO
      GOTO 4
!  NEW VALUES ARE IN H; RESET PT
    2 DO J = 1, L
        CALL GMEQ(H,PT(1,J),1,3)
      ENDDO
      M = MULBOX(H)
      IF (M.EQ.0) GOTO 4
      STHL = VCTMOD(0.5,H,2)
      IF (STHL-STHLMX) 6, 6, 4
    6 IF (STHL-10.E-5) 4, 5, 5
    5 IF (LATABS(H)) GOTO 4
      GOTO 100
!  IF HERE HAVE NO MORE VALUES OF H,K,L TO OFFER
  101 NOMORE = .TRUE.
  100 RETURN

      END SUBROUTINE GENMUL
!
!*****************************************************************************
!
      SUBROUTINE GEOMCO(N)
!
! *** GEOMCO updated by JCM 24 Jan 90 ***
!
!X
!C 6C
!H Multiple entry routine for geometric slack constraints.
!
!A N on entry specifies action required:
!A    N=1 initialise: copy out constraint information for access later
!A        and read print instructions from I PRSK (this is used by Pawley
!A        slacks as well).
!A    N=2 entered from APSHSF: recalculate actual coordinates at bond ends
!A        after shift application to position coordinates
!A    N=3 entered from NWINSF: put out new L ATOM card
!
!P Slack constraints must have been set up in /SLKGEO/ via GEOMIN
!P Parameters and variables must be set up via PARSSF
!
      INCLUDE 'PARAMS.INC'
      
      LOGICAL ONCARD
      CHARACTER*4 NAME
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
!
! NOT IF NO SLACK CONSTRAINTS:
      IF (NSLAK(1).EQ.0) GOTO 100
      GOTO (1,2,3), N
! SETTING UP ENTRY:
    1 IPRNT(8) = 0
      IF (ONCARD('I','PRSK',A)) IPRNT(8) = NINT(A)
      CALL MESS(LPT,1,'Printing of bond "obs" and calc values wanted')
      CALL DEPRIN(IPRNT(8))
! SET UP MATRIX TO CONVERT DERIVATIVES WRT A,B,C ETC TO DERIVATIVES
! WRT A*, B*, C* ETC:
      CALL CELMAT(TOSTAR)
! SET UP THE MATRIX OF 1S AND COSINES FOR THESE DERIVATIVES:
      CALL GMUNI(COSIN,3)
      COSIN(1,2) = CELL(3,2,1)
      COSIN(2,1) = CELL(3,2,1)
      COSIN(1,3) = CELL(2,2,1)
      COSIN(3,1) = CELL(2,2,1)
      COSIN(2,3) = CELL(1,2,1)
      COSIN(3,2) = CELL(1,2,1)
      RETURN
! ENTRY FROM APSHSF AFTER ALL SHIFTS APPLIED TO POSITION COORDINATES:
    2 DO I = 1, NTARNM
        CALL XTRANS(IABASE(I),XSLAK(1,I),ISYM(I),ILAT(I),CELLTR(1,I))
      ENDDO
      RETURN
! ENTRY FROM NWINSF ON FINDING L ATOM CARD - IDENTIFY ATOM:
    3 CALL RDWORD(NAME,LEN,7,IPT,80,0,IER)
      L = NCFIND(NAME,ATTNAM,NTARNM)
      WRITE (NEWIN,2000) NAME, ATNAME(IABASE(L)), ISYM(L), ILAT(L), (CELLTR(I,L),I=1,3)
 2000 FORMAT ('L ATOM ',1X,A4,1X,A4,2I5,3F4.0)
  100 RETURN

      END SUBROUTINE GEOMCO
!
!*****************************************************************************
!
      SUBROUTINE GEOMIN(N)
!
! *** GEOMIN updated by JCM 8 Mar 91 ***
!
!X
!C 6A
!H Reads L cards for bond slack constraints.
!A On entry N=0 if this is an initial, very early entry to read the L SLAK
!A            card and decide whether there are any slack constraints.
!A          N=1 for all other cards
!
!D For N=0 reads and interprets first a possible L SLAK card, setting in
!D particular SLONLY if there are ONLY slack constraints, and NO observations.
!D For N=1 reads possibly several cards starting:
!D    L ATOM  define named atom not on A card for use in constraints,
!D    L BOND  define named bond between 2 given atoms;
!D            also used for type 1, bond=given length with sigma,
!D    L ANGL  define named angle between 2 given bonds, and implying the
!D            use of a third bond;
!D            also used for type 2, angle = given size in degrees, with sigma
!D    L EQUB  type 3, 2 bonds are of equal length, with sigma
!D    L LINE  type 4, 2 bonds are in a straight line (angle=180 degrees), with
!D            sigma,
!D    L TORS  define named torsion angle  between 2 non-intersecting bonds,
!D            needing to be given a bond joining the two, and implying
!D            the use of the 3 further bonds joining the 4 atoms;
!D            also used for type 5, torsion angle between 2 non-intersecting
!D            bonds = given size in degrees, with sigma
!D    L EQUA  type 6, 2 angles (each between 2 bonds) are equal, with sigma
!D    L PLAN  type 7;  an experimental type which for the moment requests that
!D            the atoms which follow (4 or more of them) are to be planar, with
!D            sigma.
!
      CHARACTER*4 NAME, ANAME, BNAME
      DIMENSION XS(3), CS(3), NEND(2), NIN(20)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
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
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
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

      GOTO (91,92), N + 1
! SET UP AS IF NO GEOMETRIC SLACK CONSTRAINTS:
   91 NSLAK(1) = 0
      NUMBON = 0
      NTARNM = 0
      NUMANG = 0
      NUMTOR = 0
      SLONLY = .FALSE.
! SEEK L SLAK CARD:
      CALL FINDCD('L','SLAK',4,0,L)
      IF (L.LE.0) THEN
        CALL MESS(LPT,1,'No geometric slack constraints')
        GOTO 100
      ENDIF
! FOR NOW READ TYPE (WHETHER OR NOT TO EXPECT CONVENTIONAL LSQ OBSERVATIONS
! AS WELL AS SLACK CONSTRAINTS), AND EXTRA MULTIPLICATIVE WEIGHTING
! FACTOR IF TYPE 2 -  EVENTUALLY MAY BE MORE ON THIS CARD
      CALL RDINTG(NSTYP,7,IPT,80,IER)
! UNITS DIGIT OF NSTYP IS 1 FOR BOND ALONE, 2 FOR LSQ OBS PLUS BONDS
! TENS DIGIT OF NSTYP IS WEIGHTING TYPE SPECIFICALLY FOR GEOMETRIC:
!   10=UNIT WEIGHTS (NOT NORMALLY SENSIBLE)
!   20=WEIGHTS TO BE USED AS READ (IE READ 1/SIGMA SQRD)
!   30=READ SIGMA, USE WEIGHT=1/SIGMA SQRD
      NST1 = NSTYP/10
      SLONLY = NSTYP - NST1*10.EQ.1
      IF (SLONLY) THEN
        CALL MESS(LPT,1,'Refinement using bond slack constraints only')
        CALL MESS(LPT,0,'If irrelevant L cards (REFI, MODE, WGHT, '//   &
     &            'TFAC, SCAL) have been given they will be ignored')
        SLAKWT(1) = 1.
        GOTO 40
      ELSE
        CALL MESS(LPT,1,'Refinement using bond slack constraints and conventional observations')
        GOTO 20
      ENDIF
   98 CALL ERRIN2(NSTYP,2,'slack constraint type','not allowed')
      GOTO 100
   20 CALL RDREAL(SLAKWT(1),IPT,IPT,80,IER)
      IF (SLAKWT(1).EQ.0.) SLAKWT(1) = 1.
      WRITE (LPT,2005) SLAKWT(1)
 2005 FORMAT (' Extra weighting factor = ',F12.4)
! HOLD IT AS SQRT, WHICH IS HOW IT IS NEEDED:
      SLAKWT(1) = SQRT(SLAKWT(1))
   40 IF (NST1.LE.0 .OR. NST1.GT.3) GOTO 98
      GOTO (41,42,43), NST1
   41 CALL MESS(LPT,0,'Unit weights')
      GOTO 100
   42 CALL MESS(LPT,0,'Weights used directly as read')
      GOTO 100
   43 CALL MESS(LPT,0,'Read sigma, weight by 1/(sigma sqrd)')
      GOTO 100
! READ L ATOM CARDS:
   92 K = 0
   81 CALL FINDCD('L','ATOM',4,K,L)
      K = L
      IF (L.LE.0) GOTO 51
! READ TARGET ATOM NAME:
      CALL RDWORD(NAME,LEN,7,IPT,80,0,IER)
! READ ATOM SPECIFICATION;  FIND RELATED ATOM IN ASSY UNIT:
      CALL RDATOM(IPT,IA,XS,IS,IL,CS)
! ADD NAME AND ATOM SPEC TO LISTS:
      CALL ADDATM(NAME,IA,XS,IS,IL,CS,ITMP)
      GOTO 81
! READ L BOND CARDS:
   51 K = 0
    2 CALL FINDCD('L','BOND',4,K,L)
      K = L
      IF (L.LE.0) GOTO 52
! READ NAME OF BOND:
      CALL RDWORD(BNAME,LEN,7,IPT,80,0,IER)
! READ ATOM NAMES AT 2 ENDS:
      CALL RDBOND(IPT,NEND,IE)
      IF (IE.NE.0) GOTO 2
! ADD BOND TO LIST:
      CALL ADDBON(BNAME,NEND(1),NEND(2),NB)
! READ REQUIRED "OBSERVED" BOND, AND ITS ESD:
      CALL RDREAL(BOBS(NSLAK(1)+1),IPT,IPT,80,IER)
      IF (IER.EQ.100) GOTO 2
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      IF (IBMBER .NE. 0) RETURN
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
! COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT = NSKTOT + 1
! TYPE 1 GEOMETRIC SLACK CONSTRAINTS: BOND=CONSTANT
      ITYPSK(NSLAK(1)) = 1
! 1 BOND INVOLVED:
      NINVB(NSLAK(1)) = 1
! AND WHICH ONE IT IS:
      INVBON(1,NSLAK(1)) = NB
      WRITE (LPT,2001) BONNAM(NB), ATTNAM(IATM(NB,1)), ATTNAM(IATM(NB,2))
 2001 FORMAT (/' BOND slack constraint ',A4,' between atoms ',1X,A4,' and ',A4,':')
      WRITE (LPT,2002) (XSLAK(J,IATM(NB,1)),J=1,3),                     &
     &                 (XSLAK(J,IATM(NB,2)),J=1,3), BOBS(NSLAK(1)), EOBS(NSLAK(1))
 2002 FORMAT (' Actual  coordinates are:',3F10.5,' and ',               &
     &        3F10.5/'           "Observed" bond =',F10.4,' with esd ', F10.5)
      GOTO 2
! READ L ANGL CARDS:
   52 IF (NUMBON.EQ.0) GOTO 100
      K = 0
    6 CALL FINDCD('L','ANGL',4,K,L)
      K = L
      IF (L.LE.0) GOTO 53
! READ NAME OF ANGLE:
      CALL RDWORD(ANAME,LEN,7,IPT,80,0,IER)
! READ 2 BOND NAMES TO DEFINE ANGLE, AND FIND THE THIRD SIDE OF TRIANGLE:
      CALL RDANGL(IPT,N1,N2,N3,NCOM,IE)
      IF (IE.NE.0) GOTO 6
! ADD ANGLE TO LIST:
      CALL ADDANG(ANAME,N1,N2,N3,NA,IE)
      IF (IE.NE.0) GOTO 6
! RECORD CONSTRAINT - THE "OPPOSITE" SIDE COMES FIRST:
      CALL RDREAL(BOBS(NSLAK(1)+1),IPT,IPT,80,IER)
      IF (IER.EQ.100) GOTO 6
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      IF (IBMBER .NE. 0) RETURN
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
! COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT = NSKTOT + 1
! TYPE 2 GEOMETRIC SLACK CONSTRAINT - ANGLE=CONSTANT, NOT 0 OR 180
      ITYPSK(NSLAK(1)) = 2
! 3 BONDS INVOLVED (THOUGH ONLY 2 READ):
      NINVB(NSLAK(1)) = 3
      INVBON(1,NSLAK(1)) = N1
      INVBON(2,NSLAK(1)) = N2
      INVBON(3,NSLAK(1)) = N3
      WRITE (LPT,2021) ANAME, BONNAM(N2), BONNAM(N3)
 2021 FORMAT (/' ANGL slack constraint ',A4,' between bonds ',1X,A4,' and ',A4,':')
      WRITE (LPT,2022) ATTNAM(NCOM), BOBS(NSLAK(1)), EOBS(NSLAK(1))
 2022 FORMAT ('            At atom ',A4/'           "Observed" angle =',&
     &        F10.1,' with esd ',F10.2)
      GOTO 6
! READ L EQUB CARDS:
   53 K = 0
   63 CALL FINDCD('L','EQUB',4,K,L)
      K = L
      IF (L.LE.0) GOTO 54
! READ FIRST BOND NAME:
      CALL RDWORD(BNAME,LEN,7,IPT,80,0,IER)
      N1 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N1.LE.0) GOTO 11
! READ SECOND BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N2 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N2.GT.0) GOTO 12
   11 CALL ERRCH2(BNAME,2,' ','is not a bond name')
      GOTO 63
! RECORD CONSTRAINT
   12 CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      IF (IBMBER .NE. 0) RETURN
! THE OBS IS ZERO HERE:
      BOBS(NSLAK(1)) = 0.
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
! COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT = NSKTOT + 1
! TYPE 3 GEOMETRIC SLACK CONSTRAINT - EQUAL BONDS:
      ITYPSK(NSLAK(1)) = 3
! 2 BONDS INVOLVED:
      NINVB(NSLAK(1)) = 2
      INVBON(1,NSLAK(1)) = N1
      INVBON(2,NSLAK(1)) = N2
      WRITE (LPT,2031) BONNAM(N1), BONNAM(N2), EOBS(NSLAK(1))
 2031 FORMAT (/' EQUB slack constraint - bond ',A4,' approx = bond ',A4,' with esd ',F10.5)
      GOTO 63
! READ L LINE CARDS:
   54 K = 0
   64 CALL FINDCD('L','LINE',4,K,L)
      K = L
      IF (L.LE.0) GOTO 55
      IPT = 7
      CALL RDANGL(IPT,N1,N2,N3,NCOM,IE)
      IF (IE.NE.0) GOTO 64
! RECORD CONSTRAINT:
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      IF (IBMBER .NE. 0) RETURN
      BOBS(NSLAK(1)) = 0.
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
! COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT = NSKTOT + 1
! TYPE 4 GEOMETRIC SLACK CONSTRAINT - BOND=SUM OF 2 OTHERS:
      ITYPSK(NSLAK(1)) = 4
! 3 BONDS INVOLVED (THOUGH ONLY 2 READ):
      NINVB(NSLAK(1)) = 3
      INVBON(1,NSLAK(1)) = N1
      INVBON(2,NSLAK(1)) = N2
      INVBON(3,NSLAK(1)) = N3
      WRITE (LPT,2041) BONNAM(N2), BONNAM(N3), EOBS(NSLAK(1))
 2041 FORMAT (/' LINE slack constraint - bonds ',A4,' and ',A4,         &
     &        'in an approximate straight line with esd ',F10.5)
      GOTO 64
! READ L TORS CARDS:
   55 K = 0
   65 CALL FINDCD('L','TORS',4,K,L)
      K = L
      IF (L.LE.0) GOTO 56
! READ NAME OF TORSION ANGLE:
      CALL RDWORD(ANAME,LEN,7,IPT,80,0,IER)
! READ 3 BOND NAMES TO DEFINE ANGLE:
! READ FIRST BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N1 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N1.LE.0) GOTO 22
! READ SECOND BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N2 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N2.LE.0) GOTO 22
! READ THIRD BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N3 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N3.GT.0) GOTO 23
   22 CALL ERRCH2(BNAME,2,' ','is not a bond name')
      GOTO 65
! ADD ANGLE TO LIST:
   23 CALL ADDTOR(ANAME,N1,N2,N3,N4,N5,N6,NT,IE)
      IF (IE.GT.0) GOTO 65
! RECORD CONSTRAINT :
      CALL RDREAL(BOBS(NSLAK(1)+1),IPT,IPT,80,IER)
      IF (IER.EQ.100) GOTO 65
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      IF (IBMBER .NE. 0) RETURN
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
! COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT = NSKTOT + 1
! TYPE 5 GEOMETRIC SLACK CONSTRAINT - TORSION ANGLE=CONSTANT, NOT 0 OR 180
      ITYPSK(NSLAK(1)) = 5
! 6 BONDS INVOLVED (THOUGH ONLY 3 READ):
      NINVB(NSLAK(1)) = 6
      INVBON(1,NSLAK(1)) = N1
      INVBON(2,NSLAK(1)) = N2
      INVBON(3,NSLAK(1)) = N3
      INVBON(4,NSLAK(1)) = N4
      INVBON(5,NSLAK(1)) = N5
      INVBON(6,NSLAK(1)) = N6
      WRITE (LPT,2051) ANAME, BONNAM(N1), BONNAM(N3)
 2051 FORMAT (/' TORS slack constraint ',A4,' between bonds ',1X,A4, ' and ',A4,':')
      WRITE (LPT,2052) BONNAM(N2), BOBS(NSLAK(1)), EOBS(NSLAK(1))
 2052 FORMAT ('   with common axis ',A4/'           "Observed" angle =',F10.1,' with esd ',F10.2)
      GOTO 65
! READ L EQUA CARDS:
   56 K = 0
   66 CALL FINDCD('L','EQUA',4,K,L)
      K = L
      IF (L.LE.0) GOTO 57
! READ FIRST ANGLE NAME:
      CALL RDWORD(ANAME,LEN,7,IPT,80,0,IER)
      K1 = NCFIND(ANAME,ANGNAM,NUMANG)
      IF (K1.LE.0) GOTO 24
! READ SECOND ANGLE NAME:
      CALL RDWORD(ANAME,LEN,IPT,IPT,80,0,IER)
      K2 = NCFIND(ANAME,ANGNAM,NUMANG)
      IF (K2.GT.0) GOTO 25
   24 CALL ERRCH2(ANAME,2,' ','is not a bond angle name')
      GOTO 66
! RECORD CONSTRAINT
   25 CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      IF (IBMBER .NE. 0) RETURN
! THE OBS IS ZERO HERE:
      BOBS(NSLAK(1)) = 0.
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
! COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT = NSKTOT + 1
! TYPE 3 GEOMETRIC SLACK CONSTRAINT - EQUAL BONDS:
      ITYPSK(NSLAK(1)) = 6
! 6 BONDS INVOLVED (THOUGH THEY MAY NOT ALL BE DISTINCT):
      NINVB(NSLAK(1)) = 6
      INVBON(1,NSLAK(1)) = INANG(K1,1)
      INVBON(2,NSLAK(1)) = INANG(K1,2)
      INVBON(3,NSLAK(1)) = INANG(K1,3)
      INVBON(4,NSLAK(1)) = INANG(K2,1)
      INVBON(5,NSLAK(1)) = INANG(K2,2)
      INVBON(6,NSLAK(1)) = INANG(K2,3)
      WRITE (LPT,2061) ANGNAM(K1), ANGNAM(K2), EOBS(NSLAK(1))
 2061 FORMAT (/' EQUA slack constraint - angle ',A4,' approx = angle ', &
     &        A4,' with esd ',F10.5)
      GOTO 66
! READ L PLAN CARDS:
   57 K = 0
   67 CALL FINDCD('L','PLAN',4,K,L)
      K = L
      IF (L.LE.0) GOTO 100
! DISCOVER HOW MANY ATOMS INVOLVED:
      N = 0
      IPT = 7
   31 CALL RDWORD(ANAME,LEN,IPT,IPT,80,0,IER)
! USE THE ENTRY WHICH DETECTS A READ NUMBER, & SET EOBS
      IF (IER.EQ.100) GOTO 32
      CALL ERRCHK(2,N,20,0,'atoms constrained to be planar')
      IF (IBMBER .NE. 0) RETURN
      NIN(N) = NCFIND(ANAME,ATTNAM,NTARNM)
      IF (NIN(N).EQ.0) THEN
        CALL ERRATM(ANAME,2,'L PLAN card')
        GOTO 67
      ENDIF
      GOTO 31
   32 CALL ADDPLN(NIN,N)
!      ITYPSK( ETC TO BE SET EITHER HERE OR IN ADDPLN
      GOTO 67
  100 RETURN

      END SUBROUTINE GEOMIN
!
!*****************************************************************************
!
      SUBROUTINE GEOMLS(ALSQ,MATSZ)
!
! *** GEOMLS updated by JCM 2 Oct 90 ***
!
!X
!C 6B
!H Calculates bond lengths and derivatives for geometrical slack
!H constraints.
!
!A ALSQ and MATSZ are handed all through LSQ programs in this fashion
!A      - they are needed here for the call of MATTOT
!
!P On entry, COMMON /SLKGEO/ should contain:
!P      XSLAK holding actual x,y,z coordinates for all atoms involved in
!P            bonds
!P      ISYM holding the number of the symmetry operator which takes
!P      original coords into actual, -ve if by -x,-y,-z also
!P      ILAT holding the number of the lattice translation
!P      CELLTR the cell translations
!P
!P The symmetry must have been set up in SYMOP, and the original positions
!P      read from the A cards by ATOPOS.  The cell parameters must have
!P      been read by RECIP.
!
!D Called from MAIN programs to add to the LSQ matrix once per cycle.
!D First, for every involved bond, calculates the bond length and its
!D 12 derivatives, remembering that the actual position coordinates involved
!D may be related to those which are being refined.
!D
!D Then scans all geometrical constraints, forming calculated function
!D (which for types 1, 2, bonds, angles, are already there).
!D Proceeds exactly as though these are conventional observations and
!D calculated functions;  makes basic variable derivatives, gets weights,
!D and adds totals in to LSQ matrix.
!O Prints obs and calc list if requested on I card.
!
      INCLUDE 'PARAMS.INC'
      
      CHARACTER*5 F1
      CHARACTER*20 F2, F4, F5, F7
      CHARACTER*22 F3
      CHARACTER*10 F6
      CHARACTER*24 F8
      CHARACTER*100 FORMA
      LOGICAL PRNCYC, RADS, HEAD, TESTOV, PRIN
      DIMENSION ALSQ(MATSZ)
      DIMENSION DETH2(3), DETH3(3)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      REAL            DERIVV
      INTEGER                          LVARV
      COMMON /DERVAR/ DERIVV(MaxVVar), LVARV

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))

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

      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
      DATA F1, F2, F3, F4, F5, F6, F7, F8/'  No.',                      &
     &     '   Obs       Calc   ', '    Diff      Esd     ',            &
     &     '    Atom1     Atom2', '    Bond1     Bond2', ' Bond sum ',  &
     &     '   Angle1     Angle2', ' Ang1 bonds   Ang2 bonds'/

! OUT IF NO SLACK CONSTRAINTS:
      IF (NSLAK(1).EQ.0) GOTO 100
! HEADING FOR OBS/CALC BOND PRINTING IF REQUESTED:
      PRIN = PRNCYC(8)
      IF (PRIN) CALL MESS(LPT,1,'  Slack Constraints:')
! TYPE 1, GEOMETRICAL SLACK CONSTRAINTS - TYPE 3 IS PAWLEY-TYPE:
      ISLKTP = 1
! COUNT ALL INVOLVED BONDS:
      DO IB = 1, NUMBON
        CALL BONDER(IB)
      ENDDO
! COUNT ALL GEOMETRICAL SLACK CONSTRAINTS:
      DO ISK = 1, NSLAK(1)
! CLEAR WHOLE DERIVATIVE VECTOR - ONLY A FEW ITEMS WILL BE FILLED BY
! ANY PARTICULAR BOND:
        IF (LVARV.GT.0) CALL GMZER(DERIVV,1,LVARV)
! SET UP POINTERS TO INVOLVED BONDS:
        DO I = 1, NINVB(ISK)
          NVB(I) = INVBON(I,ISK)
        ENDDO
! WHICH TYPE IS THIS CONSTRAINT?
        GOTO (31,32,33,34,35,36,37), ITYPSK(ISK)
!
!* ERROR - TYPE NOT IMPLEMENTED
!
! BOND LENGTH:
   31   YCALC = BCALC(NVB(1))
        DERBON(1) = 1.
        GOTO 50
! TYPE 2 - ANGLE BETWEEN 2 BONDS:
   32   B1 = BCALC(NVB(1))
        B2 = BCALC(NVB(2))
        B3 = BCALC(NVB(3))
! YCALC = THETA RADIANS:
        CALL BONCOS(B1,B2,B3,YCALC,COSTH,SINTH,DERBON)
        GOTO 50
! TYPE 3 - EQUAL BONDS:
   33   YCALC = BCALC(NVB(1)) - BCALC(NVB(2))
        DERBON(1) = 1.
        DERBON(2) = -1.
        GOTO 50
! TYPE 4 - LINE:
   34   YCALC = BCALC(NVB(1)) - BCALC(NVB(2)) - BCALC(NVB(3))
        DERBON(1) = 1.
        DERBON(2) = -1.
        DERBON(3) = -1.
        GOTO 50
! TYPE 5 - TORS:
   35   B1 = BCALC(NVB(1))
        B2 = BCALC(NVB(2))
        B3 = BCALC(NVB(3))
        B4 = BCALC(NVB(4))
        B5 = BCALC(NVB(5))
        B6 = BCALC(NVB(6))
! DEFINE ANGLE THETA2 BETWEEN B1 & B2:
        CALL BONCOS(B5,B1,B2,TEMP,COSTH2,SINTH2,DETH2)
! AND ANGLE THETA3 BETWEEN B2 AND B3:
        CALL BONCOS(B6,B2,B3,TEMP,COSTH3,SINTH3,DETH3)
        COTTH2 = COSTH2/SINTH2
        COTTH3 = COSTH3/SINTH3
! THE EXPRESSION FOR COS PHI, THE TORSION ANGLE, IS NOW
! C=B/D + COT THETA2 * COT THETA3, WHERE THE NUMERATOR, B, AND DENOMINATOR,
!                                  D, ARE GIVEN BY:
        B = B5*B5 - B4*B4 + B6*B6 - B2*B2
        D = 2.*B1*B3*SINTH2*SINTH3
        C = B/D + COTTH2*COTTH3
        CALL SINCOS(C,S,'GEOMLS')
        YCALC = ACOS(C)
! DERIVATIVES OF C WRT ALL 6 BONDS:
        CSEQ2 = 1./(SINTH2*SINTH2)
        CSEQ3 = 1./(SINTH3*SINTH3)
        X2 = B*COTTH2 + COTTH3*CSEQ2
        X3 = B*COTTH3 + COTTH2*CSEQ3
        DERBON(1) = -B/B1 - DETH2(2)*X2
        DERBON(2) = -2.*B1/D - DETH2(3)*X2 - DETH3(2)*X3
        DERBON(3) = -B/B3 - DETH3(3)*X3
        DERBON(4) = -2.*B4/D
        DERBON(5) = 2.*B5/D - DETH2(1)*X2
        DERBON(6) = 2.*B6/D - DETH3(1)*X3
! CONVERT TO BEING DERIVATIVES OF PHI:
        DO I = 1, 6
          DERBON(I) = -DERBON(I)/S
        ENDDO
        GOTO 50
! TYPE 6 - EQUAL ANGLES:
   36   B1 = BCALC(NVB(1))
        B2 = BCALC(NVB(2))
        B3 = BCALC(NVB(3))
        B4 = BCALC(NVB(4))
        B5 = BCALC(NVB(5))
        B6 = BCALC(NVB(6))
        CALL BONCOS(B1,B2,B3,TH1,COSTH1,SINTH1,DERBON)
        CALL BONCOS(B4,B5,B6,TH2,COSTH2,SINTH2,DETH2)
        YCALC = TH1 - TH2
        DO I = 1, 3
          DERBON(I+3) = -DETH2(I)
        ENDDO
        GOTO 50
   37   CONTINUE
! TYPE 7 - PLANE:
        GOTO 50
! JOIN HERE WITH YCALC, AND DERBON= D(YCALC)/EACH INVOLVED BOND -
! MAKE DERIVATIVES OF YCALC WRT ANY RELEVANT VARIABLE:
   50   DO I = 1, 6
! MAKE 6 DERIVATIVES OF YCALC WRT 6 RECIPROCAL CELL QUADRATIC PARS:
          L = KCPARS(I)
          IF (L.GT.0) THEN
            S = 0.
            DO J = 1, NINVB(ISK)
              S = S + DERBON(J)*DERCEL(I,NVB(J))
            ENDDO
            DERIVV(L) = S
          ENDIF
        ENDDO
! DERIVATIVES OF YCALC WRT THE POSITION COORDS OF THE 3 ATOMS:
! COUNT INVOLVED BONDS:
        DO IB = 1, NINVB(ISK)
! COUNT X,Y,Z
          DO I = 1, 3
! COUNT BOTH ENDS OF A BOND:
            DO J = 1, 2
              L = KX(I,IABASE(IATM(NVB(IB),J)))
              IF (L.GT.0) DERIVV(L) = DERIVV(L) + DERPOS(I,NVB(IB),J)*DERBON(IB)
            ENDDO
          ENDDO
        ENDDO
        CALL RELATE
        OBS = BOBS(ISK)
! FOR ANGLES, USER TALKS IN DEGREES, PROGRAM WORKS IN RADIANS:
        RADS = (ITYPSK(ISK).EQ.2 .OR. ITYPSK(ISK).EQ.5 .OR. ITYPSK(ISK).EQ.6)
        IF (RADS) OBS = RADIAN(OBS)
        DIFF = OBS - YCALC
! WEIGHT
        GOTO (21,22,23), NST1
   21   SQRTWT = 1.
        GOTO 20
   22   SQRTWT = SQRT(EOBS(ISK))
        GOTO 29
   23   IF (TESTOV(1.,EOBS(ISK))) THEN
          SQRTWT = 0.
        ELSE
          SQRTWT = 1./EOBS(ISK)
        ENDIF
   29   IF (RADS) SQRTWT = DEGREE(SQRTWT)
   20   IF (.NOT.SLONLY) SQRTWT = SQRTWT*SLAKWT(1)
        WT = SQRTWT*SQRTWT
        WDIFF = SQRTWT*DIFF
        IF (PRIN) THEN
          IF (ISK.EQ.1) THEN
            HEAD = .TRUE.
          ELSE
            HEAD = (ITYPSK(ISK).NE.ITYPSK(ISK-1))
            F1 = ' '
            F3 = ' '
          ENDIF
          GOTO (61,62,63,64,65,66,67), ITYPSK(ISK)
   61     IF (HEAD) THEN
            FORMA = '('''//F1//F2//F3//F4//''')'
            WRITE (LPT,FORMA)
          ENDIF
          WRITE (LPT,2001) BONNAM(NVB(1)), OBS, YCALC, DIFF, EOBS(ISK), &
     &                     ATTNAM(IATM(NVB(1),1)), ATTNAM(IATM(NVB(1),2))
 2001     FORMAT (1X,A4,F10.4,F10.4,F10.5,G12.4,6X,A4,6X,A4)
          GOTO 60
   62     IF (HEAD) THEN
            FORMA = '('''//F1//F2//F3//F5//''')'
            WRITE (LPT,FORMA)
          ENDIF
          CALL ADDANG('    ',NVB(1),NVB(2),NVB(3),IA,IE)
          WRITE (LPT,2009) ANGNAM(IA), BOBS(ISK), DEGREE(YCALC),        &
     &                     DEGREE(DIFF), EOBS(ISK), BONNAM(NVB(2)), BONNAM(NVB(3))
          GOTO 60
   63     IF (HEAD) THEN
            FORMA = '('''//F1//F5//F3//F5//''')'
            WRITE (LPT,FORMA)
          ENDIF
          WRITE (LPT,2011) BCALC(NVB(1)), BCALC(NVB(2)), DIFF, EOBS(ISK)&
     &                     , BONNAM(NVB(1)), BONNAM(NVB(2))
 2011     FORMAT (1X,'EQUB',F10.4,F10.4,F10.5,G12.4,6X,A4,6X,A4)
          GOTO 60
   64     IF (HEAD) THEN
            FORMA = '('''//F1//F5//F3//F6//F5//''')'
            WRITE (LPT,FORMA)
          ENDIF
          WRITE (LPT,2002) ISK, BCALC(NVB(2)), BCALC(NVB(3)), DIFF,     &
     &                     EOBS(ISK), BCALC(NVB(1)), BONNAM(NVB(2)), BONNAM(NVB(3))
 2002     FORMAT (1X,'LINE',3F10.4,F10.5,G12.4,6X,A4,6X,A4)
          GOTO 60
   65     IF (HEAD) THEN
            FORMA = '('''//F1//F2//F3//F5//''')'
            WRITE (LPT,FORMA)
          ENDIF
          CALL ADDTOR('    ',NVB(1),NVB(2),NVB(3),NVB(4),NVB(5),NVB(6),IT,IE)
          WRITE (LPT,2009) TORNAM(IT), BOBS(ISK), DEGREE(YCALC),        &
     &                     DEGREE(DIFF), EOBS(ISK), BONNAM(NVB(1)), BONNAM(NVB(3))
          GOTO 60
   66     IF (HEAD) THEN
            FORMA = '('''//F1//F7//F3//F8//''')'
            WRITE (LPT,FORMA)
          ENDIF
          WRITE (LPT,2003) DEGREE(TH1), DEGREE(TH2), DEGREE(DIFF),      &
     &                     EOBS(ISK), BONNAM(NVB(2)), BONNAM(NVB(3)),   &
     &                     BONNAM(NVB(5)), BONNAM(NVB(6))
 2003     FORMAT (1X,'EQUA',2F10.2,F10.3,G12.4,2(2X,A4,'^ ',A4,2X))
          GOTO 60
   67     CONTINUE
          GOTO 60
        ENDIF
   60   CALL MATTOT(ALSQ,MATSZ)
! ADD IN BOND-TYPE SLACK CONSTRAINT STATISTICS:
        CALL RFACS(4)
      ENDDO
! PRINT BOND-TYPE SLACK CONSTRAINT STATISTICS:
      CALL RFACS(5)
  100 RETURN
 2009 FORMAT (1X,A4,F10.2,F10.2,F10.3,G12.4,6X,A4,6X,A4)

      END SUBROUTINE GEOMLS
!
!*****************************************************************************
!
      SUBROUTINE GETGEN(H,NOMORE)
!
! *** GETGEN by PJB 27 Jun 84 ***
!
!X
!C 3B
!H Gives the next useful set of h,k,l scanning the asymmetric unit of
!H reciprocal space.
!
!A On exit  H is a 1x3 real vector holding next generated values of h,k,l
!A          NOMORE is TRUE if there are no more to be found
!P SYMOP should have read the space group symmetry.
!P SYMUNI should have found the asymmetric unit of reciprocal space.
!P SETGEN should have set up stepping over asymmetric unit in /HKLGEN
!
!D Uses "previous" h,k,l in PT in /HKLGEN to move to a new one.
!D Rejects lattice absences (but not space group absences - do those outside
!D                 using ISPABS if required),
!D         h,k,l outside asymmetric unit,
!D         h,k,l for which sin theta/lambda is greater than STHLMX,
!D         and h,k,l for which sin theta is around zero (to reject 0,0,0)
!D
!D Allows for non-primitive stepping vectors in array STEP, by use of inter-
!D mediate primitive steps as calculated by PRMTIV on leaving SETGEN.
!N There also exists GENMUL which sends out M also
!
      LOGICAL NOMORE, LATABS
      DIMENSION H(3), C(2), VEC(3)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /HKLGEN/ STEP(3,3), PT(3,3), VECEND(3,3), PRPT(3,3),       &
     &                NPRIM(2,2), NP, LFAC(2), MCOUNT(2), KOM5

      NOMORE = .FALSE.
    4 DO L = 1, 3
        CALL GMADD(PT(1,L),STEP(1,L),H,1,3)
        IF (SCALPR(H,VECEND(1,L))-1.) 2, 2, 1
    1 ENDDO
! NEXT 3D LATTICE IF NON-PRIMITIVE:
! SIMULATE 'DO MCOUNT(1)=1,LFAC(1)'
      MCOUNT(1) = MCOUNT(1) + 1
      IF (MCOUNT(1).LE.LFAC(1)) GOTO 8
      MCOUNT(1) = 1
! SIMULATE 'DO MCOUNT(2)=1,LFAC(2)
      MCOUNT(2) = MCOUNT(2) + 1
      IF (MCOUNT(2).GT.LFAC(2)) GOTO 101
! MAKE COEFFICIENTS OF STEP VECTORS FOR NEXT PRIMITIVE STEP:
    8 DO I = 1, 2
        C(I) = FLOAT(MOD((MCOUNT(1)-1)*NPRIM(I,1)*LFAC(2)+(MCOUNT(2)-1)*&
     &         NPRIM(I,2)*LFAC(1),NP))/FLOAT(NP)
      ENDDO
      DO I = 1, 3
        VEC(I) = C(1)*STEP(I,1) + C(2)*STEP(I,2) + FLOAT(MCOUNT(2)-1)*STEP(I,3)/FLOAT(LFAC(2))
      ENDDO
      DO L = 1, 3
        CALL GMADD(PRPT(1,L),VEC,PT(1,L),1,3)
      ENDDO
      GOTO 4
!  NEW VALUES ARE IN H; RESET PT
    2 DO J = 1, L
        CALL GMEQ(H,PT(1,J),1,3)
      ENDDO
      IF (MULBOX(H).EQ.0) GOTO 4
      STHL = VCTMOD(0.5,H,2)
      IF (STHL-STHLMX) 6, 6, 4
    6 IF (STHL-10.E-5) 4, 5, 5
    5 IF (LATABS(H)) GOTO 4
      RETURN
!  IF HERE HAVE NO MORE VALUES OF H,K,L TO OFFER
  101 NOMORE = .TRUE.
  100 RETURN
      END SUBROUTINE GETGEN
!
!*****************************************************************************
!
      FUNCTION IATOM(ANAME)
!
! *** IATOM updated for MK4 by JCM 8 Feb 90 ***
!
!X
!C 11C
!H Identifies an atom name in a given list.
!A On entry ANAME is an A4 CHARACTER atom name, left justified
!A On exit  IATOM=0 if this is not a name in the existing list, or
!A               =a positive integer, being the number of the atom whose
!A                name this is, if found.
!P The list must be set up in array ATNAME in /ATNAM by, e.g. ATOPOS
!
      CHARACTER*4 ANAME
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
!
      IF (NATOM.EQ.0) THEN
        IATOM = 0
      ELSE
        IATOM = NCFIND(ANAME,ATNAME,NATOM)
      ENDIF
      RETURN
      END FUNCTION IATOM
!
!*****************************************************************************
!
      SUBROUTINE IICD1
!
! *** IICD1 updated by JCM 7 Aug 92 ***
!
!X
!C 6A
!H Interprets basic I cards to drive any LSQ.
!
!D Deals with defaults, recognises words on I cards, reads values after them,
!D and prints out the information.
!D
!D Vocabulary recognised:
!D NCYC <number of cycles of refinement wanted> : default 3
!D CYC1 <number to be given to first cycle> : default 1
!D PRIN <request for general printing on the LPT file>
!D        followed by an integer giving the frequency of printing:
!D           0= never
!D           1=first cycle
!D           2=last cycle
!D           3= first and last cycles
!D           4= every cycle
!D MCOR <maximum correlation>
!D       or 0 for "print whole matrix"
!D       or -ve number for "print nothing"
!D PRDM <integer> ; a non-zero integer requests Deposited Material printing
!D in the same cycle that a new crystal data file is output.
!D CONV <real> to stop LSQ cycling if max (shift/esd) < CONV : default 0.01
!
      CHARACTER*4 INEED(6)
      LOGICAL ONCARD
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      DATA INEED/'NCYC', 'CYC1', 'PRIN', 'MCOR', 'PRDM', 'CONV'/

! SET UP DEFAULTS FOR VALUES POSSIBLY TO BE READ FROM I CARDS:
      NCYC = 3
      NCYC1 = 1
      IPRNT(1) = 2
      MAXCOR = 70
      IPRNT(7) = 0
      CONV = 0.01
! INTERPRET ANY QUANTITIES GIVEN ON I CARD:
      DO I = 1, 6
        IF (.NOT.ONCARD('I',INEED(I),A)) GOTO 18
! WORD RECOGNISED - BRANCH ON WHAT TO DO WITH DATA:
        GOTO (11,12,13,14,15,16), I
! NCYC:
   11   NCYC = NINT(A)
        GOTO 18
! CYC1:
   12   NCYC1 = NINT(A)
        GOTO 18
! PRIN:
   13   IPRNT(1) = NINT(A)
        GOTO 18
! MCOR:
   14   MAXCOR = NINT(A)
        GOTO 18
! PRDM:
   15   IPRNT(7) = NINT(A)
        GOTO 18
! CONV:
   16   CONV = A
        GOTO 18
   18 ENDDO
! ARRANGE LAST CYCLE NUMBER, AND WHETHER SIMPLY SIMUATION:
      SIMUL = .FALSE.
      LASTCY = NCYC1 + NCYC - 1
      IF (LASTCY.LT.NCYC1) THEN
        LASTCY = NCYC1
        SIMUL = .TRUE.
      ENDIF
! WRITE OUT VALUES FROM POTENTIAL I CARD:
      IF (.NOT.SIMUL) THEN
        WRITE (LPT,2001) NCYC, NCYC1
 2001   FORMAT (/1X,I4,' cycles of refinement with first numbered',I4)
      ELSE
        WRITE (LPT,2011) NCYC1
 2011   FORMAT (/' Cycle number ',I4,' is a simulation')
      ENDIF
      CALL MESS(LPT,1,'Printing of obs and calc values wanted')
      N = IPRNT(1)
      CALL DEPRIN(N)
      IF (MAXCOR.LT.0) CALL MESS(LPT,1,'No correlations to be printed')
      IF (MAXCOR.EQ.0) CALL MESS(LPT,1,'Whole correlation matrix to be printed')
      IF (MAXCOR.GT.0) WRITE (LPT,2005) MAXCOR
 2005 FORMAT (/' Correlations exceeding',I4,' per cent to be printed')
      WRITE (LPT,2006) CONV
 2006 FORMAT (/' Cycles to stop if max(shift/esd) < ',F10.4)
      IF (IPRNT(7).NE.0) CALL MESS(LPT,1,'File for Deposited Material to be output on same cycle as new CDF')
      END SUBROUTINE IICD1
!
!*****************************************************************************
!
      SUBROUTINE INBOX(H,IN)
!
! *** INBOX by PJB/JCM 3 Aug 83 ***
!
!X
!C 3B
!H Determines whether the given reflection is inside, on, or outside
!H the reciprocal space asymmetric unit.
!A On entry H holds h,k,l
!A On exit  IN = -1 if outside asymmetric unit
!A                0 if inside it
!A              = number of plane if on a plane
!A              = 10+number of edge if on an edge
!P SYMOP must have read the space group
!P SYMUNI must have set up the asymmetric unit
!N Assumes that NASYM (number of planes bounding the asymmetric unit) is not
!N greater than 3
!
      DIMENSION H(3)
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10

      I = 0
      IN = 0
      IE = 3
      IF (NASYM.EQ.0) GOTO 100
      DO J = 1, NASYM
        TEST = (ASYM(1,J)*H(1)+ASYM(2,J)*H(2)+ASYM(3,J)*H(3))
        IF (ABS(TEST).LT.10.E-4) TEST = 0.0
        IF (TEST) 2, 3, 4
    2   IN = -1
        GOTO 100
    3   I = I + 1
        IP = J
        GOTO 1
    4   IE = J
    1 ENDDO
      IF (I.EQ.2) IN = IE + 10
      IF (I.EQ.1) IN = IP
  100 RETURN

      END SUBROUTINE INBOX
!
!*****************************************************************************
!
      SUBROUTINE INDFIX(H,K)
!
! *** INDFIX by JCM 20 Jul 83 ***
!
!X
!C 3C
!H Converts 3 real items in H to be integers in K.
!A On entry H is a 1x3 real vector
!A On exit  K is a 1x3 integer vector containing the numbers in H, rounded.
!
      DIMENSION H(3), K(3)
!
      DO I = 1, 3
        K(I) = NINT(H(I))
      ENDDO

      END SUBROUTINE INDFIX
!
!*****************************************************************************
!
      SUBROUTINE INDFLO(H,K)
!
! *** INDFLO by JCM 22 Nov 83 ***
!
!X
!C 3C
!H Converts 3 integers in K into floating point reals in H.
!A On entry K is a 1x3 integer vector
!A On exit  H is a 1x3 real vector containing the elements of K floated.
!
      DIMENSION H(3), K(3)

      DO I = 1, 3
        H(I) = FLOAT(K(I))
      ENDDO

      END SUBROUTINE INDFLO
!
!*****************************************************************************
!
      SUBROUTINE INITIL(PROGRM)
!
! *** INITIL updated by PJB for Unix 24-Oct-1994 ***
!
!X
!C 13C
!H Initialises the CCSL system.
!A On entry PROGRM holds A6 name of calling program
!D INITIL is obeyed at the start of a job, either by an explicit call or
!D (more usually) from a call of PREFIN.
!D It sets up various default start conditions, some of which are machine
!D dependent.  The user may wish to override some of these later.
!D
!D The symbols permitted on input are copied into /CHARS;  this will one day be
!D done in a BLOCK DATA statement.
!D  The system is set up to be not BATCH, already ini tialised.
!D The MAIN program name is kept;  the hardware (RVAX, DIVA etc) is kept; the
!D wordlength is set;  the filenames are cleared;  the file unit numbers are
!D initialised.
!D
!D For DIVA, unit LPT is opened automatically to <Program name>.LIS
!D For other systems, the name for unit LPT is requested, and it is then opened
!D
!D The constants pi, log2 etc are put into /CONSTA
!O The banner headline is put out on LPT
!
      LOGICAL ISOPEN
      CHARACTER*23 STARS
      CHARACTER*(*) PROGRM
      CHARACTER*1 IDIGI1, LETTS, ISMBO1, ISPC1
      DIMENSION IDIGI1(10), LETTS(52), ISMBO1(21)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10),         &
     &                ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /FINAME/ FILNAM(15)
      CHARACTER*10 FILNAM
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LENINT/ NBITS
      COMMON /LOONEY/ IOTAB(15), LUNTAB(15)
      COMMON /MAPGT / ZGTVAL(20), ZCGT, IGT, IZGT, IDUMPG
      COMMON /MAPRD / ZRDVAL(20), ZCRD, IRD, IZRD, IDUMPR
      COMMON /MAPSV / ZSVVAL(20), ZCSV, ISV, IZSV, NDUMPS, NSAV
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /PRSTAT/ SMYC, SMYD, SMYO, SMIO, SMID, SMWYOS, IZCT, P5,   &
     &                IOP1, IOP2, KMI(9), KMA(9)
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /WHEN  / TIM(2), MAIN
      CHARACTER*5 TIM
      CHARACTER*6 MAIN
      DATA IDIGI1/'1', '2', '3', '4', '5', '6', '7', '8', '9', '0'/
      DATA LETTS/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', &
     &     'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',  &
     &     'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',  &
     &     'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',  &
     &     'v', 'w', 'x', 'y', 'z'/
      DATA ISPC1/' '/
      DATA ISMBO1/'.', ',', ':', ';', '!', '?', '''', '"', '$', '/',    &
     &     '(', ')', '|', '-', '+', '=', '*', '#', '&', '>', '<'/

      INTEGER, EXTERNAL :: DateToday
      CHARACTER*8  tDate      ! '20010215' for 15 Feb 2001
      CHARACTER*17 DateStr
      CHARACTER*9  MonthStr
      INTEGER tLen
!
! FOR THE CONVENIENCE OF THOSE WHO FIND THE ABOVE STATEMENT EYE-CROSSING,
! THE ELEMENTS OF THE ARRAY ISMBOL WILL BE:
!  1 IS .  (FULL STOP OR DECIMAL POINT  OR DOT)
!  2 IS ,  (COMMA)
!  3 IS :  (COLON OR TWO DOTS)
!  4 IS ;  (SEMI-COLON OR DOT AND COMMA)
!  5 IS !  (EXCLAMATION MARK OR SHRIEK)
!  6 IS ?  (QUESTION MARK OR QUERY)
!  7 IS '  (APOSTROPHE OR SINGLE QUOTE)
!  8 IS "  (DOUBLE QUOTES OR CONVERSATION MARKS)
!  9 IS $  (DOLLAR)
! 10 IS /  (SLASH OR OBLIQUE OR STROKE OR SOLIDUS)
! 11 IS (  (OPENING PARENTHESIS OR LEFT BRACKET)
! 12 IS )  (CLOSING PARENTHESIS OR RIGHT BRACKET)
! 13 IS |  (VERTICAL BAR OR LOGICAL "OR")
! 14 IS -  (MINUS OR DASH)
! 15 IS +  (PLUS)
! 16 IS =  (EQUALS)
! 17 IS *  (STAR OR ASTERISK OR MULTIPLICATION SIGN)
! 18 IS #  (SHARP OR HASH)
! 19 IS &  (AMPERSAND OR AND)
! 20 IS >  (GREATER THAN OR CLOSING ANGLE BRACKET)
! 21 IS <  (LESS THAN OR OPENING ANGLE BRACKET)
!
! THE SEQUENCE ABOVE HAS BEEN TAKEN FROM THE CAMBRIDGE IBM 3081 NEW
! SOFTWARE FOR PLOTTERS, AND THE PLOTTING OF CHARACTERS IN THIS PACKAGE WILL
! USE TABLES FROM THAT SOFTWARE.  THE LAST TWO CHARACTERS DO NOT OCCUR IN
! THAT TABLE.
!
! IF THE SYSTEM IS IMPLEMENTED ON A NEW MACHINE, IT IS AS WELL TO CHECK THAT
! THE ABOVE TABLE MAKES SENSE - E.G. THAT WHERE THE WORD SAYS "DOLLAR" THE
! SYMBOL IS AN S WITH A VERTICAL LINE THROUGH IT, ETC.  IF NOT, SOME
! ADJUSTMENTS WILL HAVE TO BE MADE.  CHARACTER NUMBER 9 WILL ALWAYS PLOT AS
! DOLLAR, BUT IF THE ABOVE TABLE IS NOT RIGHT THE PRINTING SYMBOL WILL BE
! DIFFERENT.  THIS SHOULD ONLY HAPPEN WITH LESSER USED CHARACTERS, NOT LETTERS,
! DIGITS ETC.
!
! DO NOT OBEY INITIL TWICE IN ONE JOB, UNLESS FORCED TO BY MAIN:
      IF (NINIT.EQ.42) GOTO 100
! SET THAT INITIL HAS BEEN OBEYED;  THIS RELIES ON NINIT ** NOT **
! BEING SET TO 42 BY ANY OTHER METHOD:
      NINIT = 42
! SET NOT IN BATCH;  BATCH JOBS MUST "CALL BATCH" ** AFTER ** THEIR
! "CALL INITIL"
! SAVE NAME OF CALLING PROGRAM
      MAIN = PROGRM
! SET UP CHARACTERS INTO COMMON:
      DO I = 1, 26
        LETUP(I) = LETTS(I)
        LETLOW(I) = LETTS(I+26)
      ENDDO
      DO I = 1, 10
        IDIGIT(I) = IDIGI1(I)
      ENDDO
      ISPCE = ISPC1
      DO I = 1, 21
        ISMBOL(I) = ISMBO1(I)
      ENDDO
! SET UP "SINGLE PHASE":
      NPHASE = 1
      IPHASE = 1
      JPHASE = 1
      KPHASE = 1
      MULFAS = .FALSE.
      MULONE = .FALSE.
! AND "SINGLE SOURCE":
      NSOURC = 1
      JSOURC = 1
      KSOURC = 1
      MULSOU = .FALSE.
!  SET UP DEFAULTS FOR MACHINE DEPENDENT QUANTITIES:
! SET (FOR RUN TIME) THE SYSTEM FOR WHICH CCSL WAS GENERATED:
      NSYSTM = 0
!VMSO      NSYSTM=1
!UNIX
      NSYSTM = 3
!3084      NSYSTM=4
      CALL UPONE(PROGRM,NSYSTM)
!
! NUMBER OF BITS IN AN INTEGER (OR WORD) MINUS AT LEAST 1:
      NBITS = 31
! DEFAULTS FOR MULTIPHASE, MULTISOURCE - SET NEITHER OF THESE:
      MULFAS = .FALSE.
      MULSOU = .FALSE.
! DEAL WITH THE LINE PRINTER OUTPUT FILE, LPT.
!  AND INPUT OUTPUT TABLES FOR USE BY OPNFIL AND NOPFIL:
      DO I = 1, 15
        IOTAB(I) = 0
!  CLEAR FILE-NAMES
        FILNAM(I) = ' '
        LUNTAB(I) = 19 + I
      ENDDO
!RAL
      LUNTAB(3) = 40
!
!  SET RESERVED IOUNITS
!
! LPT IS SPECIAL BECAUSE WE NEED TO WRITE TO IT HERE AND NOW, WITHOUT
! GIVING THE MAIN PROGRAM A CHANCE TO ALTER ITS VALUE.  OTHER UNITS WILL BE
! CLEARED HERE, BUT MAY BE SET LATER BY MAIN, BEFORE THEY ARE FIRST USED.
! WE NEED TO CATER FOR A PROGRAM WHICH STARTS:
!      LPT=8
!      CALL INITIL(PROGRM)
! FOR RVAX, IF BOTH LPT > 0 AND UNIT LPT OPEN, USE THAT VALUE OF
! LPT, AND DO NOT REOPEN IT.  OTHERWISE, USE LPT=12
      NAMFIL = '.LIS'
      CALL UPONE(NAMFIL,3)
      IF (LPT.GT.0) THEN
        INQUIRE (LPT,OPENED=ISOPEN)
        IF (.NOT.ISOPEN) CALL OPNFIL(LPT,2143)
      ELSE
        LPT = 12
        CALL OPNFIL(LPT,2143)
      ENDIF
! CLEAR ALL OTHER NAMED UNITS, SO THAT IF THEY ARE SET BY MAIN OR
! BATCH WE WILL NOTICE THIS:
      ICRYDA = -9999
      NEWIN = -9999
      LKH = -9999
      LUNI = -9999
      IDUMPR = -9999
      IDUMPG = -9999
      NDUMPS = -9999
      IOP1 = -9999
      IOP2 = -9999
      STARS = '* * * * * * * * * * * *'
      DO I = 1, LENGT(MAIN)
        STARS(5+2*I:5+2*I) = MAIN(I:I)
      ENDDO
      WRITE (LPT,2000) STARS
 2000 FORMAT (/20X,A23//8X,' Cambridge Crystallography Subroutine Library     Mark 4.12')
! OBTAIN DATE AND TIME:
      CALL DATE_AND_TIME(tDate)
      CALL Date2String(DateToday(),DateStr,tLen)
      SELECT CASE (tDate(5:6))
        CASE ('01')
          MonthStr = 'JAN'
        CASE ('02')
          MonthStr = 'FEB'
        CASE ('03')
          MonthStr = 'MAR'
        CASE ('04')
          MonthStr = 'APR'
        CASE ('05')
          MonthStr = 'MAY'
        CASE ('06')
          MonthStr = 'JUN'
        CASE ('07')
          MonthStr = 'JUL'
        CASE ('08')
          MonthStr = 'AUG'
        CASE ('09')
          MonthStr = 'SEP'
        CASE ('10')
          MonthStr = 'OCT'
        CASE ('11')
          MonthStr = 'NOV'
        CASE ('12')
          MonthStr = 'DEC'
      END SELECT
      CALL TIME(TIM(1))
      WRITE (LPT,2001) TIM(1), DateStr(1:tLen)
 2001 FORMAT (/20X,'Job run at ',A5,' on ',A)
  100 RETURN

      END SUBROUTINE INITIL
!
!*****************************************************************************
!
      SUBROUTINE INPUTA(ID,LABA,LBALEN,LABS,LBSLEN,X,T,S,IER)
!
! *** INPUTA updated by JCM 26 Sep 89 ***
!
!X
!C 4A
!H Reads an A card and prepares it for further processing.
!A On entry ID points to the card on unit IO10.  If ID=0 it is assumed
!A             that the card is already in ICARD in /SCRACH
!A On exit LABA is the A4 label of the atom, starting with a letter
!A         LBALEN is the number of characters in LABA
!A         LABS is the A4 label of the scattering factor expected
!A         LBSLEN is the number of characters in LABS
!A         X(1:3) holds EITHER the x,y,z fractional coordinates of the atom
!A                      OR (if the card starts "C SD") their standard
!A                          deviations.
!A         T similarly holds EITHER the isotropic temperature factor
!A                           OR (if the card starts "C SD") its esd
!A         S similarly holds EITHER the site occupation factor
!A                           OR (if the card starts "C SD") its esd
!A         IER=number of errors detected
!D Items must occur in the sequence "C" optional "SD" <label>
!D x,y,z itf, optional DIFFERENT scattering factor label, optional site
!D occupation factor.
!D If the site is left blank, S=1. is assumed.
!
      CHARACTER*4 LABA, LABS
      DIMENSION X(3)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
      IER = 0
! IF ID=0 ON ENTRY, CARD EXPECTED ALREADY IN /SCRACH/; OTHERWISE READ IT:
      IF (ID.NE.0) CALL CARDIN(ID)
      SDREAD = (ICARD(3:5).EQ.'SD ')
! IPT MOVES ALONG CARD AS ITEMS TAKEN FROM IT:
      IPT = 3
      IF (SDREAD) IPT = 6
      CALL RDWORD(LABA,LBALEN,IPT,IPT,80,0,IE)
!*I THINK THIS IS BELT AND BRACES . .
      IF (IE.NE.0) GOTO 8
      IF (LETTER(LABA(1:1)).NE.0) GOTO 1
    8 CALL ERRCH2(LABA,-2,' ','as an atom label does not start with a letter')
      IER = IER + 1
    1 DO I = 1, 3
        IPKEEP = IPT
        CALL RDREAL(X(I),IPT,IPT,80,IE)
        IF ((IE.NE.0) .AND. (IE.NE.100)) THEN
          CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in coordinate ',' on A card')
          IER = IER + 1
        ENDIF
      ENDDO
      CALL RDREAL(T,IPT,IPT,80,IE)
      IF ((IE.NE.0) .AND. (IE.NE.100)) THEN
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in isotropic t f ',' on A card')
        IER = IER + 1
      ENDIF
      IPKEEP = IPT
! SET EXPLICIT SITE=0. SO THAT WE KNOW IF WE HAVE READ ONE:
      S = 0.
! NOW LOOK FOR SCATTERING FACTOR LABEL AND/OR SITE OCCUPATION FACTOR:
      CALL RDWORD(LABS,LBSLEN,IPT,IPT,80,0,IE)
! IF NOTHING MORE ON CARD, GO TO SET S=1 AND DEDUCE SCAT LABEL:
      IF (IE.EQ.100) GOTO 3
! IF SOMETHING ON CARD, BUT NOT A LABEL, REREAD IT AS SITE:
      IF (IE.NE.0) IPT = IPKEEP
      CALL RDREAL(S,IPT,IPT,80,IE)
      IF ((IE.NE.0) .AND. (IE.NE.100)) THEN
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in site occ factor ',' on A card')
        IER = IER + 1
      ENDIF
! A SITE OF ZERO, OR ABSENT, INDICATES 1:
    3 IF (S.EQ.0.) S = 1.
! IF SCAT LABEL READ, FINISH:
      IF (LBSLEN.GT.0) GOTO 100
! NO EXPLICIT LABEL, SO MUST TAKE FIRST LETTERS OF ATOM LABEL:
      L = 0
    6 L = L + 1
      IF (L.GT.LBALEN) GOTO 7
      IF (LETTER(LABA(L:L)).EQ.0) GOTO 7
      LABS(L:L) = LABA(L:L)
      GOTO 6
! IF SCAT LABEL TAKEN AS FIRST LETTERS OF ATOM LABEL, SHOW BY -VE LENGTH:
    7 LBSLEN = 1 - L
  100 RETURN
      END SUBROUTINE INPUTA
!
!*****************************************************************************
!
      SUBROUTINE INPUTC(ID,CELL)
!
! *** INPUTC updated by JCM 26 Sep 89 ***
!
!X
!C 1A
!H Reads one "C" card.
!A On entry ID points to the card on file IO10
!A On exit CELL(1:6) holds the 6 numbers read.
!D What the read numbers are depends on the presence of "SD".  If no "SD"
!D these will be a,b,c, alpha degrees, beta degrees, gamma degrees
!D (where anything on the RIGHT of the card may be omitted if it is
!D deducible from the symmetry).
!D If the card starts "C SD" the 6 numbers are esds.
!N Note the loss of the facility in fixed format to omit any deducible
!N numbers which are NOT on the RIGHT of the card.  The reading routines
!N will carry on until they find something, or till the card end.
!
      DIMENSION CELL(6)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      CALL CARDIN(ID)
      IPT = 3
      SDREAD = (ICARD(3:4).EQ.'SD')
      IF (SDREAD) IPT = 5
      CALL GMZER(CELL,1,6)
      DO J = 1, 6
        IPKEEP = IPT
        CALL RDREAL(CELL(J),IPT,IPT,80,IER)
        IF (IER.EQ.100) GOTO 100
        IF (IER.EQ.0) GOTO 1
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in cell parameter','on "C" card')
    1 ENDDO
  100 RETURN
      END SUBROUTINE INPUTC
!
!*****************************************************************************
!
      SUBROUTINE INPUTE
!
! *** INPUTE updated by JCM 29 Aug 91 ***
!
!X
!C 2A
!H Reads and interprets an E card, and sets up extinction calculations.
!D The card gives IEXTYP, an integer giving the extinction model to be used:
!D     IEXTYP = 1 Becker and Coppens Lorentzian model, 1 DOMR
!D     IEXTYP = 2 Becker and Coppens Gaussian model, 1 DOMR
!D     IEXTYP = 3 Becker and Coppens Lorentzian model, 3 DOMRs, FOVLP
!D     IEXTYP = 4 Becker and Coppens Gaussian model, 1 DOMRs, FOVLP
!D Sets LOGICALs LOREN and GAUSS.
!D After IEXTYP come the parameters of the model.  In the present options
!D 1,2 these are DOMR ("r" in the theory) and MOSC ("g" in the theory), the
!D domain radius and mosaic spread parameters.
!D For options 3,4 reads 3 values for DOMR, then MOSC and FOVLP, in the case
!D where coherence between nuclear and magnetic scattering must be taken
!D into account.
!I Reads an E card.
!O Writes its findings on unit LPT.
!
      DIMENSION A(5)

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /EXTN  / IEXTYP, DOMR, KDOMR, AMOSC, KMOSC, EXTCOR, CEXT(4)&
     &                , DEXDFQ, DEXDRQ, DEXDGQ, LOREN, GAUSS
      LOGICAL LOREN, GAUSS
      COMMON /EXTRAE/ DOMRI(3), FOVLP, KDOMRI(3), KFOVLP
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      INREAD(5) = -IABS(INREAD(5))
      GOTO (1,2), ICDNO(5) + 1
      CALL ERRMES(1,0,'only one E card allowed')
      IF (IBMBER .NE. 0) RETURN
    1 IEXTYP = 0
      GOTO 10
    2 CALL GMZER(DOMRI,3,1)
      CALL CARDIN(IABS(INREAD(5)))
      CALL RDINTG(IEXTYP,2,IPT,80,IER)
   10 LOREN = (IEXTYP.EQ.1 .OR. IEXTYP.EQ.3)
      GAUSS = (IEXTYP.EQ.2 .OR. IEXTYP.EQ.4)
      IF (IEXTYP.EQ.0) THEN
        CALL MESS(LPT,1,'No extinction correction')
        GOTO 100
      ELSEIF (IEXTYP.EQ.1 .OR. IEXTYP.EQ.2) THEN
        CALL RDREAL(DOMR,IPT,IPT,80,IER)
        CALL RDREAL(AMOSC,IPT,IPT,80,IER)
      ELSEIF (IEXTYP.EQ.3 .OR. IEXTYP.EQ.4) THEN
        CALL RDNUMS(A,IPT,5,NUM,IER)
        IF (NUM.NE.5) THEN
          CALL ERRIN2(NUM,1,'Too few numbers on E card - 5 needed','were read')
          GOTO 100
        ELSE
          CALL GMEQ(A,DOMRI,3,1)
!* IN CASE YOU WERE RELYING ON IT:
          DOMR = DOMRI(1)
          AMOSC = A(4)
          FOVLP = A(5)
        ENDIF
      ELSE
        CALL ERRIN2(IEXTYP,-2,'extinction type','not defined - assuming no extinction')
        IEXTYP = 0
        GOTO 100
      ENDIF
      IF (LOREN) WRITE (LPT,2002) IEXTYP
 2002 FORMAT (/' Extinction type is',I3,' Becker and Coppens',' Lorentzian model')
      IF (GAUSS) WRITE (LPT,2003) IEXTYP
 2003 FORMAT (/' Extinction type is',I3,' Becker and Coppens',' Gaussian model')
      IF (IEXTYP.LE.2) WRITE (LPT,2004) DOMR, AMOSC
 2004 FORMAT ('  Domain radius =',F10.4,'  Mosaic spread =',F10.4)
      IF (IEXTYP.EQ.3 .OR. IEXTYP.EQ.4) WRITE (LPT,2005) A
 2005 FORMAT (' Domain radii:',3F10.4,' Mosaic spread:',F10.4,          &
     &        ' Coherent overlap fraction:',F10.4)
  100 RETURN
      END SUBROUTINE INPUTE
!
!*****************************************************************************
!
      SUBROUTINE INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
!
! *** INPUTF by JCM 1 Mar 84 ***
!
!X
!C 4A
!H Reads and partially interprets an "F" card.
!A On entry ID points in the binary copy of the Crystal Data File to the
!A required "F" card, or iz 0, indicating that the card must be read here.
!A On exit:
!A    LABF, A4,  contains a scattering factor label
!A    LBFLEN is the number of actual characters in LABF
!A    NTYP is an integer giving the type of factor expected
!A    IPT points in the card in /SCRACH/ to the next data item
!A    IER is an error indicator, =0 if no errors.
!
      CHARACTER*4 LABF
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
      IER = 0
      IF (ID.NE.0) CALL CARDIN(ID)
! IPT MOVES ALONG CARD AS ITEMS TAKEN FROM IT:
      CALL RDWORD(LABF,LBFLEN,3,IPT,80,0,IE)
      IF (IE.NE.0) GOTO 8
      IF (LETTER(LABF(1:1)).NE.0) GOTO 1
    8 CALL ERRCH2(ICARD(3:6),-2,' ','read from "F" card where label expected')
      IER = IER + 1
    1 IPKEEP = IPT
      CALL RDINTG(NTYP,IPT,IPT,80,IE)
      IF ((IE.NE.0) .AND. (IE.NE.100)) THEN
        IER = IER + 1
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in form factor type','on "F" card')
      ENDIF

      END SUBROUTINE INPUTF
!
!*****************************************************************************
!
      SUBROUTINE INPUTI
!
! *** INPUTI updated by JCM 14 Jul 86 ***
!
!X
!C 13C
!H Gathers information from a user's interactive instruction card.
!D Absorbs into COMMON /IINFO/ sets of information from all I cards.
!D Sets IIN=number of items found on all cards.
!D
!D To be read by INPUTI, the I card must be of the form:
!D   I WORD1  NUM1  WORD2  NUM2  WORD3  NUM3  etc
!D (so if the user wants some other form he must read the card himself).
!D
!D The "NUMS" may be reals or integers.  They will be read to reals, but may be
!D fixed later according to their "WORDS".
!D
!D It does not matter what the vocabulary of words is.  INPUTI can
!D read any set of instructions in this format, and is expected to be called
!D for least squares application, for ARRNGE and for an increasing number of
!D MAIN programs requiring to be driven this way.
!D
!D An example of an I card for SFLSQ (structure factor least squares) is:
!D      I NCYC 6  CYC1  1     PRIN 2  MCOR  55
!D for which INPUTI simply produces 4 sets of (WORD, REAL) in COMMON, and
!D it is up to its calling routine (in this case STLSSF) to interpret these to
!D mean 6 cycles, first one numbered 1, print structure factor listing at last
!D cycle and correlations of over 55 per cent.
!
!I Reads all I cards.
!
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IINFO / IIN, ACOEFF(20)
      COMMON /IINFOW/ IIREAD(20)
      CHARACTER*4 IIREAD

      INREAD(9) = -IABS(INREAD(9))
      NCARDS = ICDNO(9)
! IIN=NUMBER OF ITEMS READ FROM I CARDS:
      IIN = 0
      IF (NCARDS.LE.0) GOTO 100
      ID = IABS(INREAD(9))
      DO I = 1, NCARDS
        CALL CARDIN(ID)
        ID = ID + NYZ
        IPT = 2
    3   IF (IIN.LE.20) GOTO 4
        CALL ERRMES(1,1,'more than 20 items on I cards')
        GOTO 100
    4   CALL RDWORD(IIREAD(IIN+1),NTEMP,IPT,IPT,80,0,IER)
        IF (IER.EQ.100) GOTO 2
        IIN = IIN + 1
        CALL RDREAL(ACOEFF(IIN),IPT,IPT,80,IER)
        GOTO 3
    2 ENDDO
  100 RETURN

      END SUBROUTINE INPUTI
!
!*****************************************************************************
!
      SUBROUTINE INPUTN(NOUT)
!
! *** INPUTN updated by JCM 6 Apr 89 ***
!
!X
!C 13C
!H Deals with the "N" card giving the Crystal Data File title.
!A On entry NOUT= an output unit number on to which to write the title
!A                or -1 to indicate title already present in /SCRACH
!D Puts title into ITITLE and the number of characters in it to NTITLE
!I The first time in a run INPUTN is called with +ve NOUT, it reads a single
!I card starting N, and takes the next 79 chars as a title to be put out where
!I the user chooses.  In particular it is written on plotted Fourier maps.
!
!O At this first and every other call of INPUTN, it will write out the title
!O on unit NOUT, unless NOUT=-1 when it is written to unit LPT.
!
!N The "N" is not held in ITITLE
!
      CHARACTER*8 NOTTLE
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NTITL / NTITLE, KOM14
      COMMON /TITLE / ITITLE
      CHARACTER*80 ITITLE
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA NOTTLE/'UNTITLED'/
! IF NOUT=-1, COPY TITLE FROM SCRACH:
      IF (NOUT.EQ.-1) THEN
        NTITLE = LENGT(ICARD)
        ITITLE(1:NTITLE) = ICARD(1:NTITLE)
        GOTO 101
      ENDIF
! IF TITLE BEEN INPUT BY PREVIOUS CALL, JUST COPY OUT:
      IF (INREAD(14).LT.0) GOTO 101
! SET "N CARD READ ONCE":
      INREAD(14) = -IABS(INREAD(14))
! IF NO "N" CARD GO TO PUT IN "UNTITLED":
      IF (ICDNO(14).LT.1) THEN
        ITITLE = NOTTLE
        NTITLE = 8
        GOTO 100
      ENDIF
! READ N CARD:
      CALL CARDIN(IABS(INREAD(14)))
! NTITLE GIVES NO. OF PRINTING CHARS IN TITLE:
      NTITLE = LENGT(ICARD) - 1
      ITITLE(1:NTITLE) = ICARD(2:NTITLE+1)
  101 IF (NOUT.GE.0) WRITE (NOUT,2000) (ITITLE(I:I),I=1,NTITLE)
      IF (NOUT.LT.0) WRITE (LPT,2000) (ITITLE(I:I),I=1,NTITLE)
  100 RETURN
 2000 FORMAT (1X,79A1)

      END SUBROUTINE INPUTN
!
!*****************************************************************************
!
      SUBROUTINE INPUTS(ID,R,T)
!
! *** INPUTS improved by JCM 30 Aug 92 ***
!
!X
!C 1A
!H Reads and interprets one "S" card containing a space group operator.
!H Can also interpret an S GRUP card containing a space group specification.
!A On entry ID points to an "S" card in the binary copy of the Crystal Data
!A file, or if 0 indicates that the card is already present in /SCRACH/,
!A       or if -ve indicates a scratch unit on which to find S cards.
!A On exit R is a 3x3 matrix containing the rotation, and T is a 1x3 vector
!A containing the translation.
!D Interprets symmetry operators within the character set:
!D         X Y Z x y z 1 2 3 4 5 6 + - / ,
!D and ignores characters outside that set.
!D
!D Within fixed format:
!D An operator has 3 fields, being in columns 2-21,22-41 and 42-61, and within
!D each field the element of the space group symmetry operator is given.  A
!D fraction is given as digit/digit, and any reasonable looking combination
!D (within the vocabulary above) is interpreted.
!D
!D It is also possible to override the need to use 20 columns per field,
!D by finishing a field with a comma, e.g.
!D    S  -X,-Y,Z
!
!I Reads an "S" card
!N The findings of SYMOP (which repeatedly calls INPUTS) may be printed by
!N a call of OPSYM.
!
      DIMENSION R(3,3), T(3)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10),         &
     &                ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
! CLEAR R AND T TO ZERO
      CALL GMZER(R,3,3)
      CALL GMZER(T,1,3)
! SET NO ERRORS & READ IN CARD:
      IER = 0
      IF (ID.LT.0) READ (-ID,1000) ICARD
 1000 FORMAT (A80)
      IF (ID.GT.0) CALL CARDIN(ID)
! L COUNTS ALONG CARD, EITHER IN FIELDS OF 20 OR TO NEXT COMMA
      L = 1
      DO J = 1, 3
        SIGN = 1.
        A = 0.
        B = 0.
        DO K = 1, 20
          L = L + 1
          DO M = 1, 6
            IF (ICARD(L:L).NE.IDIGIT(M)) GOTO 3
            A = FLOAT(M)
            GOTO 2
    3     ENDDO
          IF (ICARD(L:L).EQ.'/') B = A*SIGN
          DO IXYZ = 24, 26
            IF ((ICARD(L:L).EQ.LETUP(IXYZ)) .OR. (ICARD(L:L).EQ.LETLOW(IXYZ))) R(J,IXYZ-23) = SIGN
          ENDDO
          IF (ICARD(L:L).EQ.'+') SIGN = 1.
          IF (ICARD(L:L).EQ.'-') SIGN = -1.0
          IF (ICARD(L:L).EQ.',') GOTO 4
    2   ENDDO
    4   IF (A.NE.0.) T(J) = B/A
! CHECK THAT SOMETHING HAS BEEN PUT INTO THE ROTATION MATRIX ROW:
        IF ((ABS(R(J,1))+ABS(R(J,2))+ABS(R(J,3))).EQ.0.) IER = IER + 1
      ENDDO
! CHECK THAT ROTATION MATRIX HAS NO ZERO COLUMN:
      DO I = 1, 3
        IF ((ABS(R(1,I))+ABS(R(2,I))+ABS(R(3,I))).EQ.0.) IER = IER + 1
      ENDDO
! IF ERRORS, REPORT & SET IERR
      IF (IER.EQ.0) GOTO 100
      WRITE (LPT,3000) R, T, ICARD
      IERR = IERR + 1
  100 RETURN
 3000 FORMAT (/' ERROR ** on S card resulting in matrix',3(3F5.1/),     &
     &        ' and vector',3F5.1,' from card saying'/A80)

      END SUBROUTINE INPUTS
!
!*****************************************************************************
!
      SUBROUTINE INPUTT(ID,LABA,LBALEN,NTYP,A,IER)
!
! *** INPUTT by JCM 8 Feb 84 ***
!
!X
!C 4A
!H Reads and interprets one "T" card.
!A On entry ID points to the required card in the bionary copy of the Crystal
!A data file, or if 0 indicates that the card is already present in /SCRACH/.
!A On exit:
!A    LABA, A4, holds the label read after "T space"
!A    LBALEN is the number of characters in LABA
!A    NTYP is an integer giving the type of factor expected
!A    A is a 1x6 array of coefficients, in the standard sequence:
!A             A11, A22, A33, A23, A13, A12
!A    IER is an error indicator, =0 if no errors.
!
!D The exact interpretation of the 6 coefficients is left to the SUBROUTINE
!D (usually SETANI), which calls INPUTT.
!I Reads a "T" card.
!
      CHARACTER*4 LABA
      DIMENSION A(6)
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      IER = 0
      IF (ID.NE.0) CALL CARDIN(ID)
! IPT MOVES ALONG CARD AS ITEMS TAKEN FROM IT:
      CALL RDWORD(LABA,LBALEN,3,IPT,80,0,IE)
      IF (IE.NE.0 .OR. LETTER(LABA(1:1)).EQ.0) THEN
        CALL ERRATM(ICARD(3:6),-2,'"T" card')
        GOTO 101
      ENDIF
      IPKEEP = IPT
      CALL RDINTG(NTYP,IPT,IPT,80,IE)
      IF ((IE.NE.0) .AND. (IE.NE.100)) THEN
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,' ','read for atf type')
        GOTO 101
      ENDIF
      DO I = 1, 6
        CALL RDREAL(A(I),IPT,IPT,80,IE)
        IF ((IE.NE.0) .AND. (IE.NE.100)) THEN
          CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,' ','read for atf coeff')
          GOTO 101
        ENDIF
      ENDDO
      GOTO 100
  101 IER = IER + 1
  100 RETURN

      END SUBROUTINE INPUTT
!
!*****************************************************************************
!
      SUBROUTINE INPUTU(HT)
!
! *** INPUTU by JCM 22 Nov 84 ***
!
!X
!C 1A
!H Reads and interprets a "U" card, giving a typical reflection to define
!H the reciprocal asymmetric unit.
!A On exit HT is a 1x3 vector holding the 3 indices.
!D Reads a single "U" card, giving 3 real numbers for h,k,l, the indices
!D of the typical reflection which the user wishes to be inside the chosen
!D asymmetric unit.
!D
!D If no "U" card is given, 13,11,10 is assumed (being all positive, with
!D h > k > l).  If the given indices are special, they will be on the edge
!D of an asymmetric unit, and the chosen unit might not be exactly as
!D wished.
!
!I Reads one "U" card.
!O Writes its findings to unit LPT.
!
      DIMENSION HT(3), K(3)
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
!
      INREAD(21) = -IABS(INREAD(21))
      HT(1) = 13.
      HT(2) = 11.
      HT(3) = 10.
! IF NO "U" CARD, USE DEFAULT 13,11,10:
      IF (ICDNO(21).EQ.0) GOTO 101
! READ U CARD:
      IER1 = 0
      CALL CARDIN(IABS(INREAD(21)))
! SCAN ALONG CARD FINDING 3 NUMBERS:
      L = 2
      DO J = 1, 3
        CALL RDREAL(HT(J),L,L,80,IER)
! IF READ ALL SPACES (UP TO FIXED FORMAT FIELD BARRIER) ASSUME 0:
        IF (IER.EQ.100) IER = 0
! OTHERWISE, IF IER CAME NON-ZERO, WE MET AN UNEXPECTED CHARACTER:
        IF (IER.NE.0) CALL ERRIN2(J,2,'reading index number', 'on U card')
      ENDDO
! CHECK NOT 0,0,0 READ WHICH WOULD BE NO HELP:
      IF (HT(1)*HT(2)*HT(3).NE.0.) GOTO 101
      CALL ERRMES(1,-1,'indices 0,0,0 read from "U" card')
      GOTO 100
  101 CALL INDFIX(HT,K)
      WRITE (LPT,2000) K
 2000 FORMAT (/' Indices',3I4,' to be used for typical reflection ',    &
     &        'inside asymmetric unit')
  100 RETURN
      END SUBROUTINE INPUTU
!
!*****************************************************************************
!
      SUBROUTINE INTCHR(IDIG,NDIG,ICHR,NCHR,MODE)
!
! *** INTCHR updated by JCM 12 Nov 89 ***
!
!X
!C 13C
!H Converts digits to characters, either left- or right-justified.
!A On entry IDIG is an integer array holding NDIG digits
!A          NCHR is the number of characters in ICHR
!A          MODE=0 requests left justification
!A               1 requests right justification
!A On exit  ICHR is a character string holding NDIG characters, which
!A               correspond to the digits in IDIG, properly justified.
!
      CHARACTER*(*) ICHR
      DIMENSION IDIG(NDIG)
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      IF (NDIG.LE.NCHR) GOTO 1
      WRITE (LPT,3000) NDIG, NCHR
      CALL BMBOUT
      RETURN
    1 NLEN = LEN(ICHR)
      J = 0
      IF (MODE.EQ.1) J = NLEN - NCHR
      ICHR = ' '
      DO I = 1, NDIG
        J = J + 1
        K = IDIG(I)
        IF (K.EQ.0) K = 10
        ICHR(J:J) = IDIGIT(K)
      ENDDO
      RETURN
 3000 FORMAT (/' ERROR ** in INTCHR -',I4,' digits into',I4,' chars')

      END SUBROUTINE INTCHR
!
!*****************************************************************************
!
      SUBROUTINE INTDIG(N,IDIG,NDIG)
!
! *** INTDIG by JCM 8 Jun 82 ***
!
!X
!C 13C
!H Unpacks an integer into its individual digits.
!A On entry N is the integer to be unpacked
!A On exit  IDIG is an integer array holding the individual digits of abs(N)
!A          NDIG is the number of elements of NDIG, maximum 5
!
!O If the integer is too big, says so & exits with nothing stored
!
      DIMENSION IDIG(5), ITENS(4)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      DATA ITENS/10, 100, 1000, 10000/
!
      IN = IABS(N)
      ND = 1
      K = 4
    3 IF ((IN.LT.ITENS(K)) .AND. (ND.EQ.1)) GOTO 1
      IDIG(ND) = IN/ITENS(K)
      IF (IDIG(ND).GE.10) THEN
        CALL ERRIN2(N,2,'integer','too large for INTDIG')
        GOTO 100
      ENDIF
      IN = IN - IDIG(ND)*ITENS(K)
      ND = ND + 1
    1 K = K - 1
      IF (K.NE.0) GOTO 3
      IDIG(ND) = IN
      NDIG = ND
  100 RETURN

      END SUBROUTINE INTDIG
!
!*****************************************************************************
!
      SUBROUTINE INVENT(U,H,ANS)
!
! *** INVENT by PJB/JCM 8 Aug 83 ***
!
!X
!C 1C
!H Given a plane U and an axis H, produces a direction ANS which is in the
!H plane but not parallel to the axis.
!
      DIMENSION U(3), H(3), ANS(3), VEC(3,2), COMPA(3,2)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! SET UP DEFAULT VALUES:
      CALL GMZER(VEC,3,2)
! EXAMINE ELEMENTS OF INCOMING PLANE;  NZ=NO. OF ZEROS+1,
! LZ POINTS TO ZERO IF ONE OF THEM ONLY, LN POINTS TO NON-ZERO
! IF TWO ZEROS:
      NZ = 1
      DO I = 1, 3
        IF (NINT(U(I)).NE.0.) GOTO 5
        NZ = NZ + 1
        LZ = I
        GOTO 4
    5   LN = I
    4 ENDDO
! SET IH,IK,IL TO POINT CYCLICALLY 1,2,3 AS APPROPRIATE
      IF (NZ.GT.3) THEN
        CALL ERRMES(-1,0,'zero vector given to INVENT')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      IH = LN
      IF (NZ.EQ.2) IH = LZ
      IK = MOD(IH,3) + 1
      IL = MOD(IK,3) + 1
      GOTO (1,2,3), NZ
! BRANCH TO SET UP TWO POSSIBLE VECTORS FOR ANS IN VEC
! NO ZEROS AT ALL:
    1 VEC(IH,1) = -U(IK)
      VEC(IK,1) = U(IH)
      VEC(IK,2) = -U(IL)
      VEC(IL,2) = U(IK)
      GOTO 7
! ONE ZERO PRECISELY:
    2 VEC(IH,1) = 1.
      VEC(IK,2) = -U(IL)
      VEC(IL,2) = U(IK)
      GOTO 7
! TWO ZEROS PRECISELY:
    3 VEC(IK,1) = 1.
      VEC(IL,2) = 1.
! JOIN HERE TO PICK GOOD ONE:
    7 CALL GMEQ(H,COMPA(1,1),1,3)
      CALL GMREV(H,COMPA(1,2),1,3)
      DO N = 1, 2
        CALL GMEQ(VEC(1,N),ANS,1,3)
! PUT POTENTIAL ANSWER IN
        CALL INBOX(ANS,IN)
        IF (IN.LT.0) CALL GMREV(ANS,ANS,1,3)
! IF VECTOR OUTSIDE ASYMMETRIC UNIT, REVERSE IT
        CALL FCTOR(ANS,NFAC)
        CALL EQVEC(COMPA,ANS,2,M,0)
        IF (M.EQ.3) GOTO 100
! IF RESULT IS NOT COINCIDENT WITH GIVEN H, OK - EXIT
      ENDDO
! IF HERE, BOTH POTENTIAL ANSWERS HAVE PROVED PARALLEL TO H
      WRITE (LPT,3001) U, H, VEC
      CALL BMBOUT
      RETURN
  100 RETURN
 3001 FORMAT (/' ERROR ** IN INVENT - PLANE',3F10.2,' AND ',            &
     &        'EXCLUDED AXIS',3F10.4,' HAVE GIVEN VECTORS:'/2(3F10.4/))

      END SUBROUTINE INVENT
!
!*****************************************************************************
!
      FUNCTION IPOPE(N)
!
!
! *** IPOPE rewritten by PJB July 1993 ***
!
!X
!C 13C
!H Machine specific routine to interpret error codes during file opening.
!A On entry N=an error code produced as a result of trying to open a file.
!A On exit IPOPE is set as follows:
!A    1 for an 'old' file which does not exist
!A    2 for a 'new' file which does exist
!A    3 for a bad file name
!A    4 file already open
!A    0 for anything else
!
      DIMENSION IOS(4), NSWI(4)
!UNIX
      DATA NERR, NSWI, IOS/3, 1, 2, 3, 3, 2, 126, 107, 145/
!VMSO      DATA NERR,NSWI,IOS/3, 1,2,3,0, 29,43,30,0/
!
      I = NFIND(N,IOS,NERR)
      IF (I.NE.0) I = NSWI(I)
!     Temp hack by ken for Unix
      IF (N.EQ.126 .OR. N.EQ.128 .OR. N.EQ.10) I = 2
!     126 for Irix, 128 for Linux, 10 for Digital Unix
      IPOPE = I

      END FUNCTION IPOPE
!
!*****************************************************************************
!
      FUNCTION ISCAT(FNAME)
!
! *** ISCAT updated by JCM 23 Sep 86 ***
!
!X
!C 11C
!H Searches for a potential scattering factor name in the table.
!A On entry FNAME is an A4 CHARACTER name, which could be a scattering factor
!A                name, left justified
!A On exit  ISCAT =0 if FNAME does not occur in the list in /FONAME
!A                =position in the list if found.
!P The scattering factor list must have been set up by, say, ATOPOS
!
      CHARACTER*4 FNAME
      COMMON /FONAM / FONA(20,9), FONAME(20)
      CHARACTER*4 FONAME, FONA
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
!
      IF (NUMFNM.EQ.0) THEN
        ISCAT = 0
      ELSE
        ISCAT = NCFIND(FNAME,FONAME,NUMFNM)
      ENDIF

      END FUNCTION ISCAT
!
!*****************************************************************************
!
      LOGICAL FUNCTION ISPABS(H)
!
! *** ISPABS corrected by WIFD Jan 87 ***
!
!X
!C 1B
!H Checks space group absences.
!A H on entry is a 3-sized array containing h,k,l for a reflection
!A ISPABS on exit is .TRUE. if the hkl reflection is absent, .FALSE. if present
!P The symmetry must have been previously set up by SYMOP
!
      DIMENSION H(3), EH(3)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

! IF P1 OR P BAR1, NO ABSENCES:
      IF (NOPC.EQ.1) GOTO 101
      ISPABS = .TRUE.
      DO IC = 1, NCENT
        DO I = 1, NOPC
          CALL ROTSYM(H,EH,I,-1)
          IF (IC.EQ.2) CALL GMREV(EH,EH,1,3)
          DO J = 1, 3
            IF (ABS(H(J)-EH(J)).GT.10.E-4) GOTO 1
          ENDDO
! REFLECTION TRANSFORMS INTO ITSELF - CHECK TRANSLATION
          A = SCALPR(TRANS(1,I),H)
          IF (ABS(AMOD((ABS(A)+0.01),1.)).GT.0.1) GOTO 100
    1   ENDDO
      ENDDO
! PRESENT:
  101 ISPABS = .FALSE.
  100 RETURN

      END FUNCTION ISPABS
!
!*****************************************************************************
!
      FUNCTION KPAK(IFAM,IGEN,ISPC,KP,KS)
!
! *** KPAK for MK4 by JCM 7 Nov 90 ***
!
!X
!C 6C
!H Pack a LSQ parameter specification on to integer.
!A On entry IFAM = family number
!A          IGEN = genus number
!A          ISPC = species number
!A          KP = Phase number (1 if single phase, but packed as 0)
!A          KS = Source number (1 if single source, but packed as 0)
!A On exit KPAK contains IFAM, IGEN, ISPC, (and KP and KS if multi)
!A         packed according to LSQ conventions
!P LSETUP must have set up the packing scheme
!N there is an inverse routine KUNPAK
!
      DIMENSION LPAK(5)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /LSQPAK/ KKPACK(10,3)

      N = 3
      LPAK(1) = IFAM
      LPAK(2) = IGEN
      LPAK(3) = ISPC
      IF (MULONE) THEN
        LPAK(4) = KP
        LPAK(5) = KS
        N = 5
      ENDIF
      CALL NPACK(KPAK,LPAK,N,1,KKPACK)

      END FUNCTION KPAK
!
!*****************************************************************************
!
      LOGICAL FUNCTION KSAME(KK1,KK2)
!
! *** KSAME updated for MK4 by JCM 10 Feb 90
!
!X
!C 6C
!H Tells if two LSQ parameter specifications are the same, allowing wild
!H card elements.
!A On entry KK1 is a parameter spec, possibly incomplete
!A          KK2 is a parameter spec, possibly incomplete
!A On exit KSAME is .TRUE. if all the unpacked elements in the two
!A    specifications are the same, with 0 being the same as anything
!
      LOGICAL KWHOLE, KW1, KW2
      DIMENSION K1(5), K2(5)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      IF (KK1.EQ.KK2) GOTO 101
      KSAME = .FALSE.
      KW1 = KWHOLE(KK1,K1)
      KW2 = KWHOLE(KK2,K2)
      IF (KW1 .AND. KW2) GOTO 100
! ONE OF THEM AT LEAST HAS WILD CARDS:
      N = 3
      IF (MULONE) N = 5
      DO I = 1, N
        IF (K1(I).EQ.0) GOTO 3
        IF (K2(I).EQ.0) GOTO 3
        IF (K1(I).EQ.K2(I)) GOTO 3
        GOTO 100
    3 ENDDO
  101 KSAME = .TRUE.
  100 RETURN
      END FUNCTION KSAME
!
!*****************************************************************************
!
      SUBROUTINE KUNPAK(KK,IFAM,IGEN,ISPC,KP,KS)
!
! *** KUNPAK by JCM 8 Nov 90 ***
!
!X
!C 6C
!H Unpacks a LSQ parameter specification from single integer.
!A On entry KK holds packed parameter specification
!A On exit IFAM holds family number
!A         IGEN holds genus number
!A         ISPC holds species number
!A         KP holds phase (unless single phase, when 1)
!A         KS holds source(unless single source, when 1)
!P KK  must have been made via a call of KPAK set up by LSETUP
!D Unpacks KK according to bases previously set
!N There is an inverse routine KPAK, and a routine PUNPAK which takes no
!N account of phase and source.
!
      DIMENSION LPAK(5)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /LSQPAK/ KKPACK(10,3)
!
      N = 3
      IF (MULONE) N = 5
      CALL NPACK(KK,LPAK,N,2,KKPACK)
      IFAM = LPAK(1)
      IGEN = LPAK(2)
      ISPC = LPAK(3)
      KP = 1
      KS = 1
      IF (MULFAS) KP = LPAK(4)
      IF (MULSOU) KS = LPAK(5)

      END SUBROUTINE KUNPAK
!
!*****************************************************************************
!
      LOGICAL FUNCTION KWHOLE(KK,K)
!
! *** KWHOLE updated by JCM 4 Dec 91 ***
!
!X
!C 6C
!H Says if KK is a whole packed parameter specification, or whether there
!H are wild card elements.
!A On entry KK is a parameter specification, possibly with zeros
!A On exit array K has the unpacked elements of KK
!
!D A complete KK will contain phase and source information.  As these will
!D often be zero (in the cases "not multiphase"/"not multisource") they are
!D treated differently from the other elements.
!D If "not multiphase" then no account is taken of the phase element
!D If "not multisource" then no account is taken of the source element
!D Otherwise, on exit KWHOLE is .TRUE. if all unpacked elements are non-zero
!D and otherwise .FALSE.
!
      DIMENSION K(5)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
!
! UNPACK:
! USED TO UNPACK PHASE & SOURCE INTO KP & KS - NOW DOES NOT - MAY DISCOVER WHY.
      CALL KUNPAK(KK,K(1),K(2),K(3),K(4),K(5))
      KWHOLE = .TRUE.
      DO I = 1, 3
        IF (K(I).EQ.0) KWHOLE = .FALSE.
      ENDDO
      IF (MULFAS .AND. K(4).EQ.0) KWHOLE = .FALSE.
      IF (MULSOU .AND. K(5).EQ.0) KWHOLE = .FALSE.

      END FUNCTION KWHOLE
!
!*****************************************************************************
!
      LOGICAL FUNCTION LATABS(H)
!
! *** LATABS updated by PJB Sep 87 ***
!
!X
!C 1B
!H Checks h,k,l for being a (nuclear) lattice absence.
!A On entry H is 1x3 real array of h,k,l
!A On exit  LATABS = .TRUE. if h,k,l absent, .FALSE. if present
!P SYMOP should have set up the lattice information in /SYMDA, /NSYM
!
!N Deals with non-integral h,k,l also, giving the answer "absent"
!
      DIMENSION H(3)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
!
! MAKE SURE INDICES ARE INTEGERS
      LATABS = .TRUE.
      DO I = 1, 3
        IF (ABS(FLOAT(NINT(H(I)))-H(I)).GT..0001) GOTO 100
      ENDDO
      IF (NLAT.EQ.1) GOTO 2
!
!  SPECIAL ABSENCES FOR NON-PRIMITIVE LATTICES
      DO I = 2, NLAT
        A = SCALPR(H,ALAT(1,I))
        IF (ABS(FLOAT(NINT(A))-A).GT.0.0001) GOTO 100
      ENDDO
    2 LATABS = .FALSE.
  100 RETURN
      END FUNCTION LATABS
!
!*****************************************************************************
!
      LOGICAL FUNCTION LATVEC(X)
!
! *** LATVEC by PJB 5 Nov 84 ***
!
!X
!C 1B
!H Tests for the presence of a lattice vector.
!A On entry X is a real 1x3 vector
!A On exit  LATVEC is. TRUE. if X is a lattice vector, .FALSE. otherwise
!P SYMOP must have set up the lattice information in /SYMDA, /NSYM
!
      DIMENSION X(3)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      DO I = 1, NLAT
        DO J = 1, 3
          A = AMOD(ABS(ALAT(J,I)-X(J)),1.)
          IF ((A.GT..0001) .AND. (A.LT..9999)) GOTO 1
        ENDDO
        LATVEC = .TRUE.
! MATCH FOUND - JUMP:
        GOTO 100
    1 ENDDO
      LATVEC = .FALSE.
  100 RETURN
      END FUNCTION LATVEC
!
!*****************************************************************************
!
      FUNCTION LENG(NTEXT,L)
!
! *** LENG by PJB 13 Apr 85 ***
!
!X
!C 13C
!H Determines the length of a text string, omitting trailing spaces.
!A On entry NTEXT is an A1 character array of dimension L
!A On exit  LENG is the number of visible characters in NTEXT
!
      CHARACTER*1 NTEXT
      DIMENSION NTEXT(L)

      I = L + 1
    1 I = I - 1
      IF (I.LE.0) GOTO 101
      IF (NTEXT(I).EQ.' ') GOTO 1
  101 LENG = I

      END FUNCTION LENG
!
!*****************************************************************************
!
      INTEGER FUNCTION LENGT(CHAR)
!
! *** LENGT by JCM 13 Nov 87 ***
!
!X
!C 13C
!H Determines the length of a character variable, omitting the final spaces.
!A On entry CHAR is a character variable
!A On exit  LENGT is the number of visible characters in CHAR
!
      CHARACTER*(*) CHAR

      LENGT = LEN_TRIM(CHAR)

      END FUNCTION LENGT
!
!*****************************************************************************
!
      FUNCTION LETTER(I)
!
! *** LETTER by JCM 7 Oct 83 ***
!
!X
!C 13C
!H Determines whether a character is a letter.
!A On entry I is an A1 character
!A On exit  LETTER=0 if I is not a letter, otherwise which letter in range 1-26
!N Small letters and capitals treated alike.
!
      CHARACTER*1 I
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL

      DO J = 1, 26
        IF (I.EQ.LETUP(J)) GOTO 2
        IF (I.EQ.LETLOW(J)) GOTO 2
      ENDDO
      J = 0
    2 LETTER = J

      END FUNCTION LETTER
!
!*****************************************************************************
!
      SUBROUTINE LFCALC(H)
!
! *** LFCALC updated by JCM 22 Sep 87 ***
!
!X
!C 6B
!H Calculates a nuclear structure factor and its derivatives.
!A On entry H is a 1x3 array containing h,k,l
!P RECIP, SYMOP, SETANI, and SETFOR must have been obeyed to set up
!P the structure factor calculations.  (These are contained in SETFC).
!P The LSQ environment must have been set up by a suitable MAIN program (like
!P SFLSQ) which has called LSETUP and VARMAK.
!P
!D On exit in /FCAL/
!D    FC is the complex structure factor
!D    FCMOD is its modulus
!D    COSAL is the cosine of its phase
!D    SINAL is the sine of its phase
!D    FCDERS is an array of derivatives of FCMOD wrt all family 2 (structure
!D           parameters.  These are NOT multiplied or divided by anything
!D           else;  compare LMCALC
!D The above will all be zero if h,k,l gives a lattice absence;  note that
!D such would not be true of FCALC, the similar routine which does not cater
!D for LSQ
!
      INCLUDE 'PARAMS.INC'
      
      COMPLEX SUM1, TERM, FORM, HR, FORMFA
      LOGICAL TESTOV, LATABS
      DIMENSION RH(3), H(3)
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1

      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      COMPLEX         FC
      REAL                FCMOD, COSAL, SINAL, FCDERS
      COMPLEX                                                   DERIVT
      COMMON /FCAL  / FC, FCMOD, COSAL, SINAL, FCDERS(MaxF2VA), DERIVT(MaxF2VA)

      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)

      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
!
! CLEAR ANSWERS IN CASE ABSENT:
!
!  FC COLLECTS THE CONVENTIONAL STRUCTURE FACTOR, COMPLEX:
      FC = CMPLX(0.,0.)
! CLEAR MODULUS AND ANGLES:
      FCMOD = 0.
      COSAL = 0.
      SINAL = 0.
! CLEAR DERIVATIVES:
      L2 = NVARF(2,JPHASE,1)
      IF (L2.GT.0) CALL GMZER(FCDERS,1,L2)
! OUT IF ABSENT:
      IF (LATABS(H)) GOTO 100
! SET FIRST SCATTERING FACTOR:
      IFF = 0
! GET OFFSET TO REACH THESE VARIABLES:
      LO = LVFST1(2,JPHASE,1)
! CLEAR DERIVATIVE VECTOR:
      IF (L2.GT.0) CALL CGMZER(DERIVT,1,L2)
! CYCLE OVER INDEPENDENT ATOMS:
      DO IR = 1, NATOM
        SUM1 = CMPLX(0.,0.)
        IF (NFORMF(IR).NE.IFF) THEN
          IFF = NFORMF(IR)
          FORM = FORMFA(STHL,IFF)
        ENDIF
! INNER LOOP OVER SYMMETRY EQUIVALENTS:
        DO IS = 1, NOPC
          CALL ROTSYM(H,RH,IS,-1)
          F1 = TWOPI*(SCALPR(X(1,IR),RH)+SCALPR(TRANS(1,IS),H))
          ERS = ANITF(RH,IR)
! ANISOTROPIC T F (=1. IF NOT THERE) NEEDED SEPARATELY FOR LSQ:
          ARS = COS(F1)*ERS
          BRS = SIN(F1)*ERS
          TERM = CMPLX(ARS,BRS)
          SUM1 = SUM1 + TERM
!
!  NOW WE DO THE INNERMOST SUMS FOR THE DERIVATIVES OF MODFC WRT
! VARIABLES XYZ AND ALL BIJ.  WE USE THE
! COMPLEX VECTOR DERIVT FOR THE DERIVATIVES OF THE REAL & IMAGINARY PARTS
! OF THE COMPLEX FC WRT EACH VARIABLE IN TURN.
          DO I = 1, 3
            L = KX(I,IR)
            IF (L.NE.0) DERIVT(L-LO) = RH(I)*CMPLX(-BRS,ARS) + DERIVT(L-LO)
          ENDDO
! IF ANY ATF (NOT A LOOP, FOR SPEED):
          IA = IAPT(IR)
          IF (IA.NE.0) THEN
            L = KATF(1,IA)
            IF (L.NE.0) DERIVT(L-LO) = RH(1)*RH(1)*TERM + DERIVT(L-LO)
            L = KATF(2,IA)
            IF (L.NE.0) DERIVT(L-LO) = RH(2)*RH(2)*TERM + DERIVT(L-LO)
            L = KATF(3,IA)
            IF (L.NE.0) DERIVT(L-LO) = RH(3)*RH(3)*TERM + DERIVT(L-LO)
            L = KATF(4,IA)
            IF (L.NE.0) DERIVT(L-LO) = RH(3)*RH(2)*TERM + DERIVT(L-LO)
            L = KATF(5,IA)
            IF (L.NE.0) DERIVT(L-LO) = RH(1)*RH(3)*TERM + DERIVT(L-LO)
            L = KATF(6,IA)
            IF (L.NE.0) DERIVT(L-LO) = RH(1)*RH(2)*TERM + DERIVT(L-LO)
          ENDIF
        ENDDO
! END OF INNERMOST CYCLE OVER SYMMETRY
! IF CENTROSYMMETRIC, COMPENSATE FOR USING ONLY HALF NUMBER OF OPERATORS:
        IF (CENTRC) SUM1 = SUM1 + CONJG(SUM1)
        FAC = AMULT(IR)*EXP(-TF(IR)*SSQRD)
        HR = FAC*FORM*SITE(IR)
! HR IS PRODUCT OF ATOM DEPENDENT BUT SYMMETRY INDEPENDENT FACTORS
        FC = FC + HR*SUM1
!  NOW WE TIDY UP THE XYZ AND BIJ DERIVATIVES, ALLOWING FOR CENTRE:
        DO I = 1, 3
          L = KX(I,IR)
          IF (L.GT.0) THEN
            DERIVT(L-LO) = TWOPI*HR*DERIVT(L-LO)
            IF (CENTRC) DERIVT(L-LO) = DERIVT(L-LO) + CONJG(DERIVT(L-LO))
          ENDIF
          IF (IA.NE.0) THEN
            L = KATF(I,IA)
            IF (L.GT.0) THEN
              DERIVT(L-LO) = -HR*DERIVT(L-LO)
              IF (CENTRC) DERIVT(L-LO) = DERIVT(L-LO) + CONJG(DERIVT(L-LO))
            ENDIF
            L = KATF(I+3,IA)
            IF (L.GT.0) THEN
              DERIVT(L-LO) = -HR*2.*DERIVT(L-LO)
              IF (CENTRC) DERIVT(L-LO) = DERIVT(L-LO) + CONJG(DERIVT(L-LO))
            ENDIF
          ENDIF
        ENDDO
! FORM FACTOR:
        L = KCMULT(IR)
        IF (L.GT.0) DERIVT(L-LO) = SITE(IR)*FAC*SUM1
! SITE OCCUPATION:
        L = KSITE(IR)
        IF (L.GT.0) DERIVT(L-LO) = FORM*FAC*SUM1
! ISOTROPIC TEMPERATURE FACTOR:
        L = KTF(IR)
        IF (L.GT.0) DERIVT(L-LO) = -SSQRD*HR*SUM1
      ENDDO
! END OF CYCLE OVER ATOMIC POSITIONS
!
!  TIDY FCALC AND COLLECT TRUE D(MODFC)/D(VARIABLE)
!
      A = REAL(FC)
      B = AIMAG(FC)
      FCMOD = SQRT(A*A+B*B)
      IF (.NOT.TESTOV(A,FCMOD)) THEN
        COSAL = A/FCMOD
        SINAL = B/FCMOD
      ENDIF
      DO I = 1, L2
! NO SUMMING - THESE ARE THE ACTUAL DERIVATIVES NOT DIVIDED BY ANYTHING, AS
! THEY ARE SUBJECT TO THE CHAIN RULE NEXT, NOT LOGARITHMIC DIFFERENTIATION:
        FCDERS(I) = REAL(DERIVT(I))*COSAL + AIMAG(DERIVT(I))*SINAL
      ENDDO
  100 RETURN
      END SUBROUTINE LFCALC
!
!*****************************************************************************
!
      SUBROUTINE LLTFAC(N)
!
! *** LLTFAC updated JCM 13 Jan 88 ***
!
!X
!C 6A
!H Multiple entry routine which deals with overall isotropic temperature
!H factors in Least Squares.
!
!A On entry N indicates the action required:
!A N=1 reads in L TFAC card and stores value of overall isotropic
!A     temperature factor in TFAC.
!A N=2 is not assigned.
!A N=3 applies a shift to TFAC.
!A N=4 writes out a new L TFAC card.
!A N=5 deals with the default if no L TFAC card is given.
!A N=6 fixes a zero TFAC which is implied by the absence of an L TFAC card.
!A ENTRY LTFAC8(NV) sets KTFAC to indicate TFAC is variable number NV
!A ENTRY LTFAC9 sets KTFAC to indicate TFAC fixed
!
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /OVER  / ITFAC, OTFAC(10), KOTFAC(10), NTFAC, JTFAC, KOM15
      EQUIVALENCE (TFAC,OTFAC(1))
      EQUIVALENCE (KTFAC,KOTFAC(1))

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
!
      GOTO (1,100,3,4,5,6), N
!
! INTERPRET L TFAC CARD ALREADY IN ICARD:
    1 CALL RDREAL(TFAC,7,IPT,80,IER)
      IF (IER.NE.0) IERR = IERR + 1
      NTFAC = 1
      WRITE (LPT,2000) TFAC
 2000 FORMAT (/' Overall isotropic temperature factor =',F10.4)
      GOTO 100
! APPLY SHIFT:
    3 CALL ADJUST(TFAC)
      GOTO 100
! NEW L TFAC CARD:
    4 WRITE (NEWIN,2001) TFAC
 2001 FORMAT ('L TFAC',F10.4)
      GOTO 100
! NO L TFAC CARD:
    5 TFAC = 0.
      CALL MESS(LPT,1,'No L TFAC card read - assuming TFAC=0. and fixed')
      NTFAC = 0
      GOTO 100
! FIX TFAC IF NONE GIVEN:
    6 IF (NTFAC.EQ.0) CALL ADDFX5(IFAM,IGEN,ISPC,JPHASE,1,4)
      GOTO 100
      ENTRY LTFAC8(NV)
! SET TFAC VARIED:
      KTFAC = NV
      GOTO 100
      ENTRY LTFAC9
! SET TFAC FIXED:
      KTFAC = 0
  100 RETURN
      END SUBROUTINE LLTFAC
!
!*****************************************************************************
!
      FUNCTION LMATCH(LABEL,NAMTAB,NUM,NBOUND)
!
! *** LMATCH updated by JCM 23 Sep 86 ***
!
!X
!C 11C
!H Matches an A4 item in given table, adding it if it is not there already.
!
!A On entry LABEL is the A4 item to be matched
!A          NAMTAB is the A4 array in which to search (and add)
!A          NUM is the number of entries so far (0 is allowed)
!A          NBOUND is the dimension of NAMTAB
!A On exit LMATCH is set to the address of LABEL in NAMTAB
!A          NUM is increased by 1 if an entry is added.
!O Writes an error message if the table becomes overfull.
!
      CHARACTER*4 LABEL, NAMTAB
      DIMENSION NAMTAB(NBOUND)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

! IF NO ENTRIES, NO MATCH:
      IF (NUM.EQ.0) GOTO 2
      L = NCFIND(LABEL,NAMTAB,NUM)
      IF (L.GT.0) GOTO 101
! NO MATCH - ADD NEW NAME TO TABLE:
    2 IF (NUM.LT.NBOUND) GOTO 3
      WRITE (LPT,3000) NBOUND, LABEL
      CALL BMBOUT
      RETURN
    3 NUM = NUM + 1
      L = NUM
      NAMTAB(NUM) = LABEL
  101 LMATCH = L
      RETURN
 3000 FORMAT (/' ERROR ** IN LMATCH - TABLE WITH',I5,' ENTRIES',' FULL - TRYING TO ADD ',A4)
      END FUNCTION LMATCH
!
!*****************************************************************************
!
      SUBROUTINE MAKGRP(IGSB,IOPS,MODE,PRODCT)
!
! *** MAKGRP modified by PJB 31-May-1994 ***
!
!X
!C 1A
!H Generates the subgroup of a space group from the given generators.
!A On entry, IGSB holds the generators
!A    IABS(IGSB(1)) is the number of operators in the subgroup, divided
!A                  by 2 if the group contains a centre of symmetry.
!A                  If 1 it may indicate that the number of operators in
!A                  the sub-group is not known and should be calculated
!A                  by the subroutine
!A    IGSB(1)       is negative if the group contains a centre of symmetry
!A    IGSB(2)       is the identifying number of the first generator; it is
!A                  negative if it is the centrosymmetrically related one.
!A    IGSB(3)       is as IGSB(2) but for the second generator if there is one
!A    If MODE=1     the subroutine PRODCT is called with to allow evaluation
!A                  of a possible representation of the group.
!A    PRODCT        is an externally defined subroutine. May be DUMMY.
!A On exit, IOPS is a table which contains an entry for each operator of the
!A               full group. It is positive and non-zero if the operator
!A               belongs to the sub-group, and negative if it is only the
!A               centro-symmetric  operator which is in the sub-group.
!A   IGSB(1)     is set as above using the multiplicity of the sub-group
!A               found by the sub-routine.
!P SYMOP
!
      EXTERNAL PRODCT
      DIMENSION IGSB(3), IOPS(24), JOPS(24)
      LOGICAL PERM
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

! SETUP
      CALL JGMZER(IOPS,1,NOPC)
      CALL JGMZER(JOPS,1,NOPC)
      JOPS(1) = 1
      IOPS(1) = 1
      NO = 1
!  EXTRACT INFORMATION ABOUT GENERATORS
      NOPG = IABS(IGSB(1))
      IF (NOPG.EQ.1) NOPG = NOPC
      NG1 = IABS(IGSB(2))
      IF (NG1.EQ.1) GOTO 101
      NO1 = IABS(NORD(NG1))
      IF (NO1.GT.100) NO1 = NO1 - 100
      NG2 = IABS(IGSB(3))
!  PUT IN GENERATORS
      JOPS(2) = NG1
      IOPS(NG1) = ISIGN(1,IGSB(2))
      JOPS(NO1+1) = NG2
      IF (NG2.NE.0) IOPS(NG2) = ISIGN(1,IGSB(3))
!  AND MULTIPLES OF FIRST GENERATOR
      ISIG = IOPS(NG1)
      DO NO = 3, NO1
        I = MULTAB(JOPS(NO-1),NG1)
        ISIG = ISIG*IOPS(NG1)
        JOPS(NO) = I
        IF (MODE.EQ.1) CALL PRODCT(JOPS(NO-1),NG1,I)
        IOPS(I) = ISIG
      ENDDO
      NO = NO1
      IF (NG2.EQ.0) GOTO 101
!  GENERATE THE REST OF THE SUB-GROUP
      N1 = 2
      N2 = NO1 + 1
      NO = N2
    5 J = JOPS(N1)
    4 JJ = JOPS(N2)
      PERM = .TRUE.
      I = MULTAB(J,JJ)
      ISIG = IOPS(J)*IOPS(JJ)
    3 IF (IOPS(I).EQ.0) THEN
        NO = NO + 1
        JOPS(NO) = I
        IF (MODE.EQ.1) THEN
          IF (PERM) THEN
            CALL PRODCT(J,JJ,I)
          ELSE
            CALL PRODCT(JJ,J,I)
          ENDIF
        ENDIF
        IOPS(I) = ISIG
        IF (NO.GE.NOPG) GOTO 101
      ENDIF
      IF (PERM) THEN
        PERM = .FALSE.
        I = MULTAB(JJ,J)
        GOTO 3
      ENDIF
      N2 = N2 + 1
      IF (N2.LE.NO) GOTO 4
      N1 = N1 + 1
      N2 = MAX(NO1+1,N1)
      IF (N1.LE.NO) GOTO 5
! SET THE MULTIPLICITY TO THAT FOUND
  101 IGSB(1) = ISIGN(NO,IGSB(1))

      END SUBROUTINE MAKGRP
!
!*****************************************************************************
!
      CHARACTER*4 FUNCTION MAKNAM(CHAR,N)
!
! *** MAKNAM BY JCM 8 JUL 91 ***
!
!X
!C 13C
!H Makes an A4 name from the given character and the digits of the given number.
!A On entry CHAR holds either a single character, to fill the name from the
!A               left, repeatedly, or 2,3 or 4 characters, which are used to
!A               fill in on the left unchanged.
!A          N is an integer
!A On exit MAKNAM is a *4 name with the digits of N at the right hand side,
!A          filled with the characters from CHAR.
!
      CHARACTER*4 NAME, CH
      CHARACTER*(*) CHAR

      CH = CHAR
      IF (LEN(CHAR).EQ.1) CH = CHAR//CHAR//CHAR//CHAR
      WRITE (NAME,2000) N
 2000 FORMAT (I4)
      DO I = 1, 4
        IF (NAME(I:I).EQ.' ') NAME(I:I) = CH(I:I)
      ENDDO
      MAKNAM = NAME

      END FUNCTION MAKNAM
!
!*****************************************************************************
!
      SUBROUTINE MATCEL(ALSQ,MATSZ)
!
! *** MATCEL updated by JCM 15 Nov 90 ***
!
!X
!C 6B
!H After a cycle of LSQ gives variance-covariance matrix for cell A* B* etc
!H in both real and reciprocal space, and the same for abc, alpha,beta,gamma.
!A The Least Squares matrix ALSQ(MATSZ) must be handed throughout the system
!A as the argument of routine calls in order to make it easily altered.
!P  The LSQ matrix ALSQ is assumed to hold the inverse matrix after a
!P cycle of refinement.
!D Fills in array CELESD(6,6,2) for the variance-covariances of the
!D quadratic products in both spaces, and CELLSD(6,6) for a,b,etc.
!
      INCLUDE 'PARAMS.INC'
      
      DIMENSION ALSQ(MATSZ)
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /CONSTR/ JCONST, JROWPT(301), JCMAT(200), AMOUNT(200), NEXTJ

      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)

      DIMENSION TOSTAR(6,6), TOSTTR(6,6), CELTMP(6,6), BGTOSM(6,6),     &
     &          SMTOBG(6,6), SMBGTR(6,6)
!
! CLEAR OUT ALL ANSWERS
      CALL GMZER(CELESD(1,1,2),6,6)
! FIRST DO DIAGONALS, WHICH MAY BE NEEDED FOR OFF-DIAGONALS:
      DO I = 1, 6
        LI = KCPARS(I)
! DO NOTHING IF ELEMENT I IS NOT A VARIABLE:
        IF (LI.GT.0) THEN
          II = LVRBS(LI)
          A = 1.
! DEAL WITH CASE OF REDUNDANT VARIABLE:
          IF (II.LT.0) THEN
            A = AMOUNT(JROWPT(-II))
            II = JCMAT(JROWPT(-II))
          ENDIF
          CELESD(I,I,2) = ELEMAT(ALSQ,MATSZ,II,II)*A*A
        ENDIF
      ENDDO
!
! OFF-DIAGONAL TERMS:
      DO I = 1, 5
        LI = KCPARS(I)
! ONLY IF I'TH IS A VARIABLE;
        IF (LI.GT.0) THEN
          II = LVRBS(LI)
          AI = 1.
! DEAL WITH I'TH BEING REDUNDANT:
          IF (II.LT.0) THEN
            AI = AMOUNT(JROWPT(-II))
            II = JCMAT(JROWPT(-II))
          ENDIF
          DO J = I + 1, 6
            LJ = KCPARS(J)
! ONLY IF J'TH IS A VARIABLE;
            IF (LJ.GT.0) THEN
              JJ = LBSVR(LJ)
              AJ = 1.
! DEAL WITH J'TH BEING REDUNDANT:
              IF (JJ.LT.0) THEN
                AJ = AMOUNT(JROWPT(-JJ))
                JJ = JCMAT(JROWPT(-JJ))
              ENDIF
!
              IF (II.NE.JJ) THEN
                CELESD(I,J,2) = ELEMAT(ALSQ,MATSZ,II,JJ)*AI*AJ
              ELSE
! IF BOTH I AND J ARE REDUNDANT AND RELATED TO SAME BASIC:
                CELESD(I,J,2) = SQRT(CELESD(I,I,2)*CELESD(J,J,2))
              ENDIF
! SYMMETRICALLY RELATED MATRIX ELEMENT:
              CELESD(J,I,2) = CELESD(I,J,2)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!
! SET UP MATRIX TO CONVERT DERIVATIVES WRT A,B,C ETC TO DERIVATIVES
! WRT A* B* C* ETC:
      CALL CELMAT(TOSTAR)
!
! GET THE TRANSPOSE OF TOSTAR (TOSTTR):
      CALL GMTRAN(TOSTAR,TOSTTR,6,6)
! CELESD(I,J,1)=TOSTAR(I,K)*CELESD(K,L,2)*TOSTTR(L,J)
      CALL MultiplyMatrices(CELESD(1,1,2),TOSTTR,CELTMP,6,6,6)
      CALL MultiplyMatrices(TOSTAR,CELTMP,CELESD,6,6,6)
! EVALUATE THE VARIANCE-COVARIANCE MATRIX FOR A B C ALPHA BETA GAMMA
! FIRSTLY FORM LITTLE A,B,C
      ASMALL = CELL(1,1,1)
      BSMALL = CELL(2,1,1)
      CSMALL = CELL(3,1,1)
      CALL GMZER(BGTOSM,6,6)
      BGTOSM(1,1) = 2.*ASMALL
      BGTOSM(2,2) = 2.*BSMALL
      BGTOSM(3,3) = 2.*CSMALL
!     BGTOSM(4,4) HAS THE FORM -(180/PI)*B*C*SIN(ALPHA) ETC.
      BGTOSM(4,4) = -RADIAN(BSMALL*CSMALL*CELL(1,3,1))
      BGTOSM(5,5) = -RADIAN(ASMALL*CSMALL*CELL(2,3,1))
      BGTOSM(6,6) = -RADIAN(ASMALL*BSMALL*CELL(3,3,1))
      BGTOSM(2,4) = CPARS(4,1)/BSMALL
      BGTOSM(3,4) = CPARS(4,1)/CSMALL
      BGTOSM(1,5) = CPARS(5,1)/ASMALL
      BGTOSM(3,5) = CPARS(5,1)/CSMALL
      BGTOSM(1,6) = CPARS(6,1)/ASMALL
      BGTOSM(2,6) = CPARS(6,1)/BSMALL
      CALL InverseMatrix(BGTOSM,SMTOBG,6)
      CALL GMTRAN(SMTOBG,SMBGTR,6,6)
! CELLSD(I,J)=SMTOBG(I,K)*CELESD(K,L,1)*SMBGTR(L,J)
      CALL MultiplyMatrices(CELESD,SMBGTR,CELTMP,6,6,6)
      CALL MultiplyMatrices(SMTOBG,CELTMP,CELLSD,6,6,6)

      END SUBROUTINE MATCEL
!
!*****************************************************************************
!
      SUBROUTINE MATCOR(ALSQ,MATSZ)
!
! *** MATCOR updated by JCM 11 Aug 88 **
!
!X
!C 6C
!H After a Least Squares cycle, prints correlations from the inverse matrix.
!
!A The matrix ALSQ is dimensioned everywhere except in MAIN programs as
!A ALSQ(MATSZ), and handed through as a routine argument.
!
      INCLUDE "PARAMS.INC"

      CHARACTER*4 IPNAM1(2), IPNAM2(2), IUPPER(MaxBVar), LOWER(MaxBVar)
      DIMENSION ALSQ(MATSZ), ICORR(MaxBVar)

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB


      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      EQUIVALENCE (MODER,MODERR(1))

      IF (SIMUL) RETURN
! IF MAXCOR  -VE, NO PRINTING AT ALL:
      IF (MAXCOR.LT.0) RETURN
      DO I = 1, LVARB
        BLSQ(I) = 1.0/SQRT(ALSQ(MM(I)+I))
      ENDDO
! BLSQ USED TEMPORARILY TO HOLD DIAGONAL - NOT NEEDED AS RHS NOW
!
! MAXCOR=0 ASKS FOR WHOLE CORRELATION MATRIX:
      IF (MAXCOR.EQ.0) GOTO 2
!
! HERE TO PRINT CORRELATION ABOVE MAXCOR PER CENT:
      IHEAD = 0
      DO I = 1, LVARB
        I1 = I + 1
        DO J = I1, LVARB
          IND = MM(I) + J
          ICOR = NINT(100.*ALSQ(IND)*BLSQ(I)*BLSQ(J))
          IF (IABS(ICOR).LT.MAXCOR) GOTO 3
! WRITE HEADING BEFORE FIRST CORRELATION FOUND TO BE LARGE ENOUGH:
          IF (IHEAD.EQ.0) WRITE (LPT,2004) MAXCOR
 2004     FORMAT (/' Correlations above',I4,' per cent:')
          IHEAD = 1
!* PROVISION FOR PHASE AND SOURCE CHANGES:
          CALL PARNAM(IPNAM1(1),IPNAM1(2),1,I)
          CALL PARNAM(IPNAM2(1),IPNAM2(2),1,J)
          WRITE (LPT,2005) IPNAM1(1), IPNAM1(2), IPNAM2(1), IPNAM2(2), ICOR
 2005     FORMAT (' ',2(1X,A4),' - ',2(1X,A4),'    ',I4)
    3   ENDDO
      ENDDO
      IF (IHEAD.EQ.0) WRITE (LPT,2006) MAXCOR
 2006 FORMAT (/' No correlations found above',I4,' per cent')
      RETURN
! WHOLE MATRIX:
    2 CALL MESS(LPT,1,'Correlation matrix :')
      CALL NEWLIN(LPT)
      DO I = 1, LVARB
! PUT INTO IUPPER AND LOWER THE PRINTING NAMES OF ALL BASIC VARIABLES:
        CALL PARNAM(IUPPER(I),LOWER(I),1,I)
      ENDDO
      NV = 0
    7 NV = NV + 20
      NST = NV - 19
      IF (NV.GT.LVARB) NV = LVARB
      NO = NV - NST + 1
! NST=STARTING POINT, NV=END POINT, NO=NUMBER OF ITEMS IN THIS PRINTING
! WRITE HEADING FOR ONE PRINTING ON TWO LINES:
      WRITE (LPT,2001) (IUPPER(I),I=NST,NV)
 2001 FORMAT (' ',10X,20(1X,A4))
      WRITE (LPT,2003) (LOWER(I),I=NST,NV)
 2003 FORMAT (' ',12X,20(1X,A4))
      DO I = 1, LVARB
        K = NST - 1
        DO J = 1, NO
          K = K + 1
          ICORR(J) = NINT(100.*ELEMAT(ALSQ,MATSZ,I,K)*BLSQ(I)*BLSQ(K))
        ENDDO
        WRITE (LPT,2002) (IUPPER(I),(LOWER(I)),J=1,2), (ICORR(J),J=1,NO)
 2002   FORMAT (' ',2(1X,A4),T112,2(1X,A4),T12,20I5)
      ENDDO
! END OF ONE PRINTING
      IF (NV.LT.LVARB) GOTO 7

      END SUBROUTINE MATCOR
!
!*****************************************************************************
!
      SUBROUTINE MATINV(ALSQ,MATSZ)
!
! *** MATINV updated by JCM 2 Jun 89 ***
!
!X
!C 6C
!H Inverts the matrix in ALSQ, of which the upper triangle only is held.
!A ALSQ has the dimension MATSZ, which is set in MAIN programs and handed down
!A to here as a parameter of the routine calls.
!
!N No interchanges are done - a positive definite matrix should not need them.
!
!N This should be modified some day to operate on the right hand side BLSQ also.
!
!N It is intended to replace it by a QR inversion routine.
!
      INCLUDE "PARAMS.INC"

      CHARACTER*4 BS, VR
      LOGICAL TESTOV, OVER
      DIMENSION ALSQ(MATSZ)

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      IF (SIMUL) RETURN
! DIMENSION OF MATRIX:
      N = LVARB
      IF (N) 100, 100, 11
! SCALE MATRIX TO ALLOW FOR PARAMETERS OF DIFFERING SCALES:
   11 DO I = 1, N
        IR = MATPNT(I+1)
        OVER = TESTOV(1.0,ALSQ(IR+I))
        IF (OVER) THEN
          CALL PARNAM(BS,VR,1,I)
          CALL DebugErrorMessage('Element 0.0 in MATINV')
          WRITE (LPT,3000) I, BS, VR
          DERIVB(I) = 0.0
        ELSE
          DERIVB(I) = 1.0/SQRT(ALSQ(IR+I))
        ENDIF
        DO J = 1, I
          IS = MATPNT(J+1)
          ALSQ(IS+I) = ALSQ(IS+I)*DERIVB(I)
        ENDDO
        DO J = I, N
          ALSQ(IR+J) = ALSQ(IR+J)*DERIVB(I)
        ENDDO
        IF (OVER) THEN
          DERIVB(I) = 1.0
          ALSQ(IR+I) = 1.0
! JvdS As all the other BLSQ assignments had been commented out, I decided to do this one as well.
!          BLSQ(I) = 0.0
        ENDIF
      ENDDO
      DO I = 1, N
        I1 = I + 1
        IR = MATPNT(I1)
        IS = MATPNT(I)
        ALSQ(IR+I) = 1./ALSQ(IR+I)
! NEXT      BLSQ(I) = BLSQ(I)*ALSQ(IR+I)
        IF (I.EQ.N) GOTO 1
! OR NEXT DO LOOP WILL FAIL
        DO K = I1, N
          ALSQ(IS+K) = -ALSQ(IR+I)*ALSQ(IR+K)
        ENDDO
        DO J = I1, N
          IQ = MATPNT(J+1)
! NEXT      BLSQ(J) = BLSQ(J)-BLSQ(I)*ALSQ(IR+J)
          DO K = J, N
            ALSQ(IQ+K) = ALSQ(IQ+K) + ALSQ(IS+K)*ALSQ(IR+J)
          ENDDO
        ENDDO
    1 ENDDO
      IF (N-1) 101, 101, 12
! INVERSE OF ALSQ:
   12 N1 = N - 1
      DO IFOR = 1, N1
        I = N - IFOR
        I1 = I + 1
        IR = MATPNT(I1)
        IS = MATPNT(I)
        DO J = I1, N
          ALSQ(IR+J) = 0.
        ENDDO
        DO J = I1, N
          IQ = MATPNT(J+1)
          DO K = J, N
            ALSQ(IR+K) = ALSQ(IR+K) + ALSQ(IS+J)*ALSQ(IQ+K)
            IF (J.NE.K) ALSQ(IR+J) = ALSQ(IR+J) + ALSQ(IS+K)*ALSQ(IQ+K)
          ENDDO
          ALSQ(IR+I) = ALSQ(IR+I) + ALSQ(IS+J)*ALSQ(IR+J)
        ENDDO
      ENDDO
!
! BACK SUBSTITUTE FOR BLSQ (MAY BE INTENDED FOR LABEL 12):
! NEXT      DO 13 IFOR=1,N1
! NEXT      I=N+IFOR
! NEXT      I1=I+1
! NEXT      IR=MATPNT(I)
! NEXT      DO 14 J=I1,N
! NEXT      BLSQ(I)=BLSQ(I)-ALSQ(IR+J)*BLSQ(J)
! NEXT  14  CONTINUE
! NEXT  13  CONTINUE
!
! RESCALE MATRIX TO ALLOW FOR PARAMETERS OF DIFFERING SCALES:
  101 DO I = 1, N
        IR = MATPNT(I+1)
        DO J = 1, I
          IS = MATPNT(J+1)
          ALSQ(IS+I) = ALSQ(IS+I)*DERIVB(I)
        ENDDO
        DO J = I, N
          ALSQ(IR+J) = ALSQ(IR+J)*DERIVB(I)
        ENDDO
      ENDDO
  100 RETURN
 3000 FORMAT (/' WARNING in MATINV ** zero diagonal element for basic variable number',I5,2X,A4,1X,A4)

      END SUBROUTINE MATINV
!
!*****************************************************************************
!
      SUBROUTINE MATSET(ALSQ,MATSZ)
!
! *** MATSET updated by JCM 11 Aug 88 ***
!
!X
!C 6C
!H Sets up pointers into a Least Squares matrix, and clears the matrix and the
!H corresponding right hand side vector.
!A The matrix ALSQ is dimensioned everywhere except in MAIN programs as
!A ALSQ(MATSZ).
!
      INCLUDE "PARAMS.INC"
      
      DIMENSION ALSQ(MATSZ)

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB


      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      IF (SIMUL) RETURN
! SET UP POINTERS:
      MATPNT(1) = 0
      DO I = 1, LVARB
        MATPNT(I+1) = MATPNT(I) + LVARB - I + 1
      ENDDO
      NMAT = (LVARB+1)*(LVARB+2)/2 ! JvdS Oct 2003 I suspect a typo here: should that have been (LVARB-1)*(LVARB+2)/2
      ! or simply (N*N+N)/2 ?
      IF (MATSZ.LT.NMAT) THEN
        WRITE (LPT,3000) MATSZ, NMAT, LVARB
 3000 FORMAT (/' ERROR ** MATSZ given as',I6,' and needs to be  at least',I6,' for',I5,' basic variables')
        CALL BMBOUT
        RETURN
      ENDIF
! CLEAR MATRIX AND RHS:
      CALL GMZER(ALSQ,1,MATSZ)
      CALL GMZER(BLSQ,1,LVARB)

      END SUBROUTINE MATSET
!
!*****************************************************************************
!
      SUBROUTINE MATSHF(ALSQ,MATSZ)
!
! *** MATSHF updated by JCM 30 Sep 88 ***
!
!X
!C 6C
!H From an inverted Least Squares matrix, calculates shifts in basic variables.
!A The Least Squares matrix ALSQ is dimensioned everywhere except in MAIN
!A programs as ALSQ(MATSZ), and both ALSQ and MATSZ are then handed through the
!A system as the arguments of routines.
!A On entry ALSQ must contain the inverse of the LSQ matrix.
!
!P The vector BLSQ must hold the rhs of the Least Squares equations.
!P SUMWD in /OBSCAL/ holds the sum of weighted differences,
!P NOBS in /OBSCAL/ = number of observations used,
!P LVARB in /DERBAS/= number of basic variables.
!
!D Calculates shifts from the inverse matrix (ALSQ) and the rhs (BLSQ),
!D putting the answers in BLSQ and the esds in DERIVB.
!
!N Eventually MATINV will modify BLSQ to give shifts, but for now we must
!N form them separately.
!
      INCLUDE "PARAMS.INC"

      CHARACTER*4 BS, VR
      DIMENSION ALSQ(MATSZ), SHIFTS(MaxBVar)

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB


      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /RSTATS/ RNUM, RDEN, RSNUM, RSDEN, RWNUM, RWDEN, RWSDEN,   &
     &                CHI2
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24

      IF (SIMUL) RETURN
! SCALE FOR ESD'S:
      CHI2 = SUMWD/(NOBS-LVARB)
      CHITOT = CHI2
      IF (NSKTOT.NE.0) CHITOT = CSQTOT
      DO I = 1, LVARB
        IR = MM(I)
        SHIFTS(I) = 0.
        IF (ALSQ(IR+I).LT.0.) THEN
          CALL PARNAM(BS,VR,1,I)
          WRITE (LPT,3000) I, BS, VR
          CALL BMBOUT
          RETURN
        ENDIF
        DERIVB(I) = SQRT(CHITOT*ALSQ(IR+I))
        DO J = I, LVARB
          SHIFTS(I) = SHIFTS(I) + BLSQ(J)*ALSQ(IR+J)
        ENDDO
        I1 = I - 1
        IF (I1) 1, 1, 4
    4   DO J = 1, I1
          SHIFTS(I) = SHIFTS(I) + BLSQ(J)*ALSQ(MM(J)+I)
        ENDDO
    1 ENDDO
! FOR NOW:
      DO I = 1, LVARB
        BLSQ(I) = SHIFTS(I)
      ENDDO
 3000 FORMAT (/' ERROR ** ill-conditioning starts at basic variable',' number',I5,2X,A4,1X,A4)

      END SUBROUTINE MATSHF
!
!*****************************************************************************
!
      SUBROUTINE MATTOT(ALSQ,MATSZ)
!
! *** MATTOT updated by JCM 17 Oct 89 ***
!
!X
!C 6C
!H Add in contributions to LSQ matrix and RHS for one observation.
!A All reference to the lsq matrix ALSQ is made through routines with
!A arguments ALSQ and MATSZ.
!A This enables ALSQ to be dimensioned everywhere except in MAIN as ALSQ(MATSZ)
!
!P On entry DERIVB holds the derivatives wrt all basic variables
!P          DIFF holds OBS-CALC
!P          SQRTWT holds the square root of the weight
!P          SIMUL is TRUE if this is only a simulation cycle
!
      INCLUDE "PARAMS.INC"

      DIMENSION ALSQ(MATSZ)

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB


      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      IF (SIMUL) RETURN
      SQWDIF = SQRTWT*DIFF
      DO I = 1, LVARB
        DERIVB(I) = SQRTWT*DERIVB(I)
      ENDDO
      DO I = 1, LVARB
        IF (DERIVB(I).EQ.0.) GOTO 1
        IR = MM(I)
        DO J = I, LVARB
          ALSQ(IR+J) = ALSQ(IR+J) + DERIVB(I)*DERIVB(J)
        ENDDO
        BLSQ(I) = BLSQ(I) + SQWDIF*DERIVB(I)
    1 ENDDO

      END SUBROUTINE MATTOT
!
!*****************************************************************************
!
      SUBROUTINE MESS(LUNIT,N,TXT)
!
! *** MESS updated by JCM 12 Sep 92 ***
!
!X
!C 13C
!H Writes on unit LUNIT the given message, preceded by N empty lines.
!A On entry TXT is a CHARACTER variable holding the message,
!A          N is in integer requesting N empty lines before the message, and
!A            may be 0, for no lines
!A            or > 98, for a page throw.
!O Writes to unit LUNIT N empty lines or a page throw, then the given text with
!O a "space" carriage control.
!
      CHARACTER*(*) TXT
!
      IF (N.LT.99) THEN
        DO I = 1, N
          WRITE (LUNIT,2000)
 2000     FORMAT (1X)
        ENDDO
      ELSE
        WRITE (LUNIT,2002)
 2002   FORMAT ('1')
      ENDIF
      WRITE (LUNIT,2001) (TXT(I:I),I=1,LENGT(TXT))
 2001 FORMAT (1X,200A1)

      END SUBROUTINE MESS
!
!*****************************************************************************
!
      FUNCTION MINIM(LIST,N,M)
!
! *** MINIM by JCM 15 Jan 88 ***
!
!X
!C 11C
!H Finds the position and value of the minimum in a list of integers.
!A On entry LIST is a list of integers
!A          N is the number of entries in the list
!A On exit  MINIM is set to the value of the smallest menber of LIST
!A      where LIST(M)=MINIM
!
      DIMENSION LIST(N)
      COMMON /LENINT/ NBITS
!
      MIN = 2**(NBITS-1) - 1
      DO I = 1, N
        IF (LIST(I).GE.MIN) GOTO 1
        MIN = LIST(I)
        IKEEP = I
    1 ENDDO
      MINIM = MIN
      M = IKEEP

      END FUNCTION MINIM
!
!*****************************************************************************
!
      SUBROUTINE MTPROD(I,J,N)
!
! *** MTPROD new by PJB Nov 90 ***
!
!x
!C 17C
!H Puts entries into the table of time inversion operators.
!A I, J, and N are operator numbers. The entry in MTSYM for N is
!A             set to the product of that for I and J.
!N Called from MAGSYM through MAKGRP using the internal subroutine
!N name PRODCT.
!
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL, KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL

      MTSYM(N) = MTSYM(I)*MTSYM(J)

      END SUBROUTINE MTPROD
!
!*****************************************************************************
!
      FUNCTION MULBOX(H)
!
! *** MULBOX updated by JCM 14 Jun 88 ***
!
!X
!C 3C
!H Tests indices for being in the asymmetric unit, and gives multiplicity.
!
!A On entry H(3) is a real 1x3 vector holding the reflection indices.
!D The function is set to the multiplicity of the reflection if the reciprocal
!D lattice point represented by H is inside the exact asymmetric unit.
!D If the point is not inside the function is returned as zero.
!
      DIMENSION H(3)
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      COMMON /GUNIT / MARK(3,2), BSYM(3,3), IBOX, KOM11

!  DETECT 000
      MULT = 1
      IBOX = 999
      IF (ABS(H(1))+ABS(H(2))+ABS(H(3)).LT.10.E-5) GOTO 101
!  OTHERWISE START WITH MULT FOR GENERAL POSITION
      MULT = NMUL
!  VALUE FOR GENERAL POINT
      CALL INBOX(H,IBOX)
      IN = IBOX
      IF (IN) 102, 101, 3
    3 IF (IN.GT.10) GOTO 4
!  ON A PLANE
      M1 = MARK(IN,1) + 1
      GOTO (102,101,2,5,5), M1
    2 MULT = MULT/2
      GOTO 101
!  WE ONLY WANT HALF THE PLANE
    5 TEST = SCALPR(H,BSYM(1,IN))
      IF (TEST+.0001.LT.0.) GOTO 102
!  DIVIDE MULTIPLICITY IF ON DIAD AXIS
      IF (ABS(TEST).GE..0001) GOTO 101
      IF (MARK(1,1).EQ.4) GOTO 7
      MULT = MULT/2
      GOTO 101
!  BIT TO ACCEPT ONLY ONE HALF OF CENTRE LINE FOR P-1
    7 IF (SCALPR(H,BSYM(1,2)).LT.0.) GOTO 102
      GOTO 101
!  ON AN AXIS
    4 IN = IN - 10
      IF (MARK(IN,2).EQ.0) GOTO 102
      MULT = MULT/IABS(MARK(IN,2))
      IF (MARK(IN,2).GT.0 .OR. NASYM.NE.2) GOTO 101
!  WE ONLY WANT HALF THE AXIS
      IF (SCALPR(H,EDGE(1,IN)).GE.0.) GOTO 101
!  HERE IF NOT STRICTLY INSIDE
  102 MULT = 0
  101 MULBOX = MULT

      END FUNCTION MULBOX
!
!*****************************************************************************
!
      FUNCTION NCFIND(CH,CTABLE,NBOUND)
!
! *** NCFIND by JCM 15 Jul 86 ***
!
!X
!C 11C
!H Searches for a particular word in a table of words.
!A CH is the word for which to search
!A CTABLE is a table containing NBOUND words
!D Sets the function value to zero of CH is not in the table or
!D to the position of CH in the table if it is there.
!
      CHARACTER*(*) CH, CTABLE(NBOUND)

      DO L = 1, NBOUND
        IF (CH.EQ.CTABLE(L)) GOTO 101
      ENDDO
      L = 0
  101 NCFIND = L

      END FUNCTION NCFIND
!
!*****************************************************************************
!
      FUNCTION NDIGIT(I)
!
! *** NDIGIT by JCM 11 Oct 83 ***
!
!X
!C 13C
!H Identifies a character as a digit or not.
!A I is a single A1 character
!D The function is set to -1 of I is not a digit, otherwise to the digit
!D value.
!
      CHARACTER*1 I
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL

      DO J = 1, 10
        IF (I.EQ.IDIGIT(J)) GOTO 2
      ENDDO
      J = -1
    2 NDIGIT = J
      IF (NDIGIT.EQ.10) NDIGIT = 0

      END FUNCTION NDIGIT
!
!*****************************************************************************
!
      SUBROUTINE NEWCD
!
! *** NEWCD by JCM 10 Mar 86 ***
!
!X
!C 6C
!H Opens a file on to which to write a new Crystal Data File after a Least
!H Squares refinement.
!D Asks interactively for a file name, and opens this with default extension
!H .CCN at RAL, .CRY at ILL, for unit NEWIN in /NEWOLD/
!
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      MESSAG = 'New Crystal Data file '
      NAMFIL = '.CCN'
      CALL OPNFIL(NEWIN,113)

      END SUBROUTINE NEWCD
!
!*****************************************************************************
!
      SUBROUTINE NEWLIN(LUNIT)
!
! *** NEWLIN by JCM 14 Sep 92 ***
!
!X
!C 13C
!H Writes a newline to the output unit LUNIT
!
      WRITE (LUNIT,2000)
 2000 FORMAT (' ')

      END SUBROUTINE NEWLIN
!
!*****************************************************************************
!
      FUNCTION NFIND(N,NTABLE,NBOUND)
!
! *** NFIND by JCM 17 Apr 84 ***
!
!X
!C 11C
!H Searches for integer N in a table.
!A On entry N is the integer for which to search.
!A          NTABLE is an array of NBOUND integers.
!A          NBOUND is the number of entries in the table.
!D The function NFIND is set to zero if N is not in the table, or to
!D the number of the matching entry if one is found.
!
      DIMENSION NTABLE(NBOUND)
      DO L = 1, NBOUND
        IF (N.EQ.NTABLE(L)) GOTO 101
      ENDDO
      L = 0
  101 NFIND = L

      END FUNCTION NFIND
!
!*****************************************************************************
!
      FUNCTION NOPFIL(MODE)
!
! *** NOPFIL updated by PJB 24-Oct-1994 ***
!
!X
!C 13C
!H Opens a file on a FORTRAN unit for the first time in this job.
!
!A On entry MODE defines the file-type and indicates how to obtain the file name
!A  MODE may have up to 5 digits:
!A      the least significant, MODE1, indicates the file-type
!A      the tens digit,        MODE2, shows how to obtain the file-name
!A      the hundreds digit,    MODE3, deals with default extensions
!A      the thousands digit,   MODE4, deals with formatted or unformatted files
!A      the most significant,  MODE5, deals with sequential or direct access,
!A                                    and odd special cases.
!A
!A  MODE1 = 1 for a read file
!A        = 2 for a write file status new
!A        = 3 for a write file status undefined (UNKNOWN)
!A        = 4 for a write file to be modified (APPEND for sequential files)
!A        = 5 for a scratch file.
!A
!A  MODE2 = 0 Give standard messages for read or write files; machine specific
!A            information like disc, extension and ppn may be included in
!A            the user's response.
!A        = 1 Message in MESSAG otherwise as 0
!A        = 2 Find file-name in MESSAG. Report file opened.
!A        = 3 as 2 but don't give file-opened message.
!A        = 4 as 0 but don't give file-opened message.
!A
!A  MODE3 = 0 Use default disc and ppn, default extension .DAT
!A        = 1 Use defaults for extension, disc, and ppn found in characters
!A            1-4, 5-10, and 11-30 respectively of NAMFIL. If disc or ppn
!A            are absent default as system.
!A        = 2 Use file-name exactly as given.
!A
!A  MODE4 = 0 for formatted files with no FORTRAN carriage controls
!A        = 1 for unformatted files
!A        = 2 for formatted files with FORTRAN carriage controls (line printer)
!A
!A  MODE5 = 0 for sequential file access
!A        = 1 for direct access files
!A        = 2 for the special case for GENIE of sequential, RECL=128, the VMS-
!A            specific "RECORDTYPE='FIXED'".  This will expect MODE4=1 for
!A            unformatted, and will take account of MODEs 1,2 and 3.
!
!D Opens file, if possible, according to instructions in MODE.
!D NOPFIL is set to the logical unit assigned.
!D NOPFIL=0 indicates that no data have been given in response to the
!D interactive request for a file name. This may be useful as a way
!D of reading several files in sequence, with RETURN given as response when no
!D more are wanted.
!
!D If on exit NOPFIL=-1, an error has occurred from which recovery was not
!D possible.
!D
!D 15 units are provided by the system; the numbers by which they are known
!D to the Operating System are in the array LUNTAB.
!D The array IOTAB marks the units available; IOTAB=0 for available
!D units, IOTAB=MODE for units in use.
!
      DIMENSION LM(2)
      LOGICAL BRUCE
      CHARACTER*7 FILSTA(3)
      CHARACTER*10 FILACC(3)
      CHARACTER*12 FILFOR(2)
      CHARACTER*100 DEXT
      CHARACTER*40 SAVMES
      CHARACTER*26 MESS(2)
      CHARACTER*32 WFMT
      CHARACTER*80 FMT
      COMMON /FINAME/ FILNAM(15)
      CHARACTER*10 FILNAM
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LOONEY/ IOTAB(15), LUNTAB(15)
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /WHEN  / TIM(2), MAIN
      CHARACTER*5 TIM
      CHARACTER*6 MAIN
      INTEGER ISPAG
      DATA LM/13, 14/
      DATA FILSTA/'OLD', 'NEW', 'UNKNOWN'/
      DATA FILACC/'SEQUENTIAL', 'DIRECT', 'APPEND'/
      DATA FILFOR/'FORMATTED', 'UNFORMATTED'/
      DATA MESS/'Give name of Input file  ','Give name for Output file '/
!
!     filnam_root is the root filename (e.g. 'ct130k') communicated
!     in from FORTY via common block commun.
      COMMON /commun/ filnam_root
      CHARACTER*10 filnam_root

      LOGICAL, EXTERNAL :: Confirm

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! UNSCRAMBLE MODE
      M = MODE
      MODE5 = M/10000
      M = M - MODE5*10000
      MODE4 = M/1000
      M = M - MODE4*1000
      MODE3 = M/100
      M = M - MODE3*100
      MODE2 = M/10
      MODE1 = M - MODE2*10
! SET DEFAULTS FORMATTED, SEQUENTIAL, AND NEW FOR WRITE, OLD FOR READ
      IS = MODE1
      IA = 1
      IF = 1
! SET FORMAT AND ACCESS
      IF (MODE1.EQ.4) THEN
! APPEND
        IS = 3
        IA = 3
      ENDIF
! ADJUST FORMATTED/UNFORMATTED
      IF (MODE4.EQ.1) IF = 2
! ADJUST SEQUENTIAL/DIRECT FOR ACCESS
      IF (MODE5.EQ.1) IA = 2
! IO SET FOR INPUT OR OUTPUT FILE MESSAGE
      IO = 1
      IF (MODE1.GT.1) IO = 2
! NOW FIND A UNIT
      DO IU = 1, 15
        IF (IOTAB(IU).EQ.0) GOTO 2
      ENDDO
      CALL ERRMES(1,0,'*** ERROR No more logical units available')
      IF (IBMBER .NE. 0) RETURN
    2 LUN = LUNTAB(IU)
      IF (MODE1.EQ.5) THEN
! OPEN SCRATCH FILE
        IF (IA.EQ.1) OPEN (UNIT=LUN,ACCESS=FILACC(IA),FORM=FILFOR(IF),STATUS='SCRATCH')
! FOR NOW, IF DIRECT ACCESS, RECL=80 - THIS WILL NEED ATTENTION
        IF (IA.EQ.2) OPEN (UNIT=LUN,ACCESS=FILACC(IA),FORM=FILFOR(IF),STATUS='SCRATCH',RECL=80)
        GOTO 101
      ENDIF
!  OTHERWISE GET THE FILE-NAME
!  DEAL WITH EXTENSIONS
      IF (MODE3.EQ.0) DEXT = '.DAT'
      IF (MODE3.EQ.1) DEXT = NAMFIL
      IF (MODE3.EQ.2) DEXT = ' '
      IF (MODE2.NE.1) THEN
! PRESERVE FILE NAME IF IT CAME IN MESSAG
        IF (MODE2.EQ.2 .OR. MODE2.EQ.3) NAMFIL = MESSAG
! PUT ENOUGH OF STANDARD MESSAGE INTO MESSAG TO USE ALSO WHEN REPORTING
        MESSAG = MESS(IO)(LM(IO)+1:)
      ENDIF
      LMES = LENG(MESSAG,100) + 1
! AVOID SENDING MESSAGE IF NOT WANTED
      IF (MODE2.EQ.2 .OR. MODE2.EQ.3) GOTO 21
! JOIN HERE IF FIRST TRY FAILED, & IT MAY BE USEFUL TO TRY .CRY FOR .CCL:
!    6   WRITE (ITO,VFMT) MESS(IO)(1:LM(IO)),MESSAG
!
      ISPAG = 0
!
    6 CONTINUE
!   The above line added to supress unwanted output in the
!   noninteractive version
!>> JCC
!>> It seems that the program can get caught in an infinite spaghetti loop here
!>> so I've added in a pass count check.
      ISPAG = ISPAG + 1
      IF (ISPAG.GT.10) THEN
        NOPFIL = -1
        CALL DebugErrorMessage('ISPAG.GT.10 in NOPFIL')
        GOTO 100
      ENDIF
! SPECIAL BRUCE BLIP TO USE BOTH .CCL AND .CRY FILES WITHOUT KNOWING WHICH:
      BRUCE = .FALSE.
!      Put filnam_root into namfil instead of reading
!      it in from standard input
      WRITE (NAMFIL,'(A6)') filnam_root
!  PROCESS FILE NAME
   21 IF (LENG(NAMFIL,100).EQ.0) GOTO 40
   89 CALL FILPRO(DEXT,IU,LNAM)
!  CHECK FOR ERROR IN FORM OF FILE-NAME
      IF (LNAM.EQ.0) GOTO 50
! SAVE MESSAGE IN SAVMES
      SAVMES = MESSAG(1:40)
!  OPEN FILE
   30 CONTINUE
! IF GENIE FILE, VAX-SPECIFIC BECAUSE OF RECORDTYPE:
!VMS
      IF (MODE5.EQ.2) THEN
        OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE,RECL=128)
        GOTO 22
      ENDIF
      IF (MODE1.EQ.1) THEN
! MUST SPECIFY READONLY FOR READ FILES ON VAX TO AVOID PROTECTION FAILURES
! WHEN READING FROM FOREIGN DIRECTORIES
        OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE)
      ELSE
        IF (MODE4.EQ.0) THEN
          OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE)
        ELSE
          OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE)
        ENDIF
      ENDIF
   22 IF (MODE2.EQ.3 .OR. MODE2.EQ.4) GOTO 101
!  REPORT DONE
      IF (LMES.GT.40) LMES = 40
      DO J = 1, LMES
        IF (SAVMES(J:J).NE.'(') GOTO 31
        IL = J - 1
        GOTO 32
   31 ENDDO
      IL = LMES
   32 WRITE (WFMT,2002) IL, LNAM
 2002 FORMAT ('(/1X,A',I2,',1X,A',I2,','' opened'')')
      WRITE (LPT,WFMT) SAVMES(1:IL), NAMFIL(1:LNAM)
      GOTO 101
! NO RESPONSE - 'RETURN' TYPED
! DETECT SPECIALLY THE RESPONSE "RETURN" ON RVAX REQUEST FOR O/P FILE:
   40 IF (LUN.EQ.LPT .AND. MODE.EQ.2142) THEN
        NAMFIL = MAIN//'.LIS'
        GOTO 89
      ENDIF
      NOPFIL = 0
      GOTO 100
! 'OPEN' ERROR - CHECK FIRST FOR BRUCE BLIP:
   52 IF (BRUCE .OR. MODE.NE.111) GOTO 152
      BRUCE = .TRUE.
      IF (DEXT(2:4).EQ.'CRY') THEN
        DEXT(2:4) = 'CCL'
      ELSEIF (DEXT(2:4).EQ.'CCL') THEN
        DEXT(2:4) = 'CRY'
      ENDIF
      L1 = LENG(NAMFIL,100)
      NAMFIL(L1-2:L1) = DEXT(2:4)
      I = INDEX(FILNAM(IU),'.')
      FILNAM(IU)(I+1:I+3) = DEXT(2:4)
      GOTO 30
! IDENTIFY ERROR IN MACHINE DEPENDENT TABLE
  152 IOS = IPOPE(IOE)
      GOTO (55,56,53,54,60), IOS + 1
! ERROR TYPE 1 - 'OLD' FILE FOUND NOT TO EXIST:
   56 WRITE (FMT,601) LNAM
  601 FORMAT ('('' File '',A',I2,','' does not exist'')')
   62 WRITE (LPT,FMT) NAMFIL(1:LNAM)
   50 IF (MODE2.LT.2 .OR. MODE2.EQ.4) GOTO 6
! NO WAY TO OFFER USER ANOTHER TRY:
      NOPFIL = -1
      GOTO 100
! ERROR TYPE 2 - 'NEW' FILE EXISTS ALREADY:
   53 IF (.NOT. Confirm('Existing file '//NAMFIL(1:LNAM)//' will be overwritten, OK?')) GOTO 50
! ALLOW TO TRY SAME FILE AGAIN WITH STATUS OLD
      IS = 1
      GOTO 30
! ERROR TYPE 3 - BAD FILE NAME (COVERS VARIOUS SYNTACTICAL ERRORS):
   54 WRITE (FMT,603) LNAM
  603 FORMAT ('('' *** ERROR Bad file name '',A',I2,','' IOSTAT='',I4)')
      GOTO 61
! ERROR TYPE 4 - FILE ALREADY OPEN
   60 WRITE (FMT,604) LNAM
  604 FORMAT ('('' *** ERROR File '',A',I2,','' already open'')')
      GOTO 62
! ERROR TYPE 0 - OTHERS
   55 WRITE (FMT,600) LNAM
  600 FORMAT ('('' *** ERROR opening file '',A',I2,','' IOSTAT='',I4)')
   61 WRITE (LPT,FMT) NAMFIL, IOE
      GOTO 50
! SUCCESSFUL EXIT
  101 NOPFIL = LUN
      IOTAB(IU) = MODE
  100 MESSAG = ' '
      END FUNCTION NOPFIL
!
!*****************************************************************************
!
      SUBROUTINE NPACK(NPK,L,N,MODE,LPACK)
!
! *** NPACK updated by JCM 11 Jan 88 ***
!
!X
!C 16C
!H Deals with the packing and unpacking of up to 10 integers in/out of one
!H integer.
!A On entry MODE gives the required operation:
!A
!A   MODE=0: set up for subsequent entries.  The array L contains,
!A           for each item to be packed/unpacked, a number MAX(I).  If
!A           MAX(I) is positive, items in position I in the packing are
!A           expected to be in the range 0 to MAX(I).
!A
!A           if MAX(I) is negative, items in the I'th position in the packing
!A           are expected to be in the range MAX(I) to -MAX(I).
!A
!A           the array L is of dimension N; NPK is irrelevant.
!A           LPACK is a (10,3) array to be used as working space.
!A
!A   MODE=1: Pack an array of items given in L into the integer NPK, according
!A           to the information set up by a mode 0 entry.
!A
!A   MODE=2: Unpack into an array L the items previously packed into NPK.
!
      DIMENSION LPACK(10,3)
      DIMENSION L(N)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LENINT/ NBITS

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (N.LE.0) THEN
        CALL ERRIN2(N,0,'misuse of NPACK - N=',' ')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      GOTO (21,22,23), MODE + 1
! SETTING UP:
   21 AM = 1.
      DO I = 1, N
        LPACK(I,1) = L(I)
        IF (LPACK(I,1).LT.0) GOTO 10
! POSITIVE MAX VALUE:
        LPACK(I,2) = 0
        LPACK(I,3) = LPACK(I,1) + 1
        GOTO 13
! NEGATIVE 'MAX' VALUE:
   10   LPACK(I,2) = -LPACK(I,1)
        LPACK(I,3) = -2*LPACK(I,1) + 1
   13   AM = AM*FLOAT((IABS(LPACK(I,1))+LPACK(I,2)+1))
      ENDDO
! TEST LARGEST POSSIBLE PACKED NUMBER WILL FIT IN MACHINE INTEGER, LESS SIGN
      IF (AM.LT.2.**NBITS) GOTO 100
      WRITE (LPT,3002) AM, NBITS
      GOTO 100
! MODE=1:  PACKING:
   22 NPK1 = 0
      DO I = 1, N
        LP = L(I)
        IF (LPACK(I,1).LT.0) LP = IABS(L(I))
        IF (0.LE.LP .AND. LP.LE.IABS(LPACK(I,1))) GOTO 4
        WRITE (LPT,3003) L(I), LPACK(I,1)
        GOTO 100
    4   NPK1 = NPK1*LPACK(I,3) + L(I) + LPACK(I,2)
      ENDDO
      NPK = NPK1
      GOTO 100
! MODE 2:  UNPACKING:
   23 NPK1 = NPK
      DO I = 1, N
        J = N - I + 1
        K = NPK1/LPACK(J,3)
        L(J) = NPK1 - LPACK(J,3)*K - LPACK(J,2)
        NPK1 = K
      ENDDO
  100 RETURN
 3002 FORMAT (/' ERROR ** LARGEST POSSIBLE PACKED NUMBER IS',G14.7,     &
              ' AND WILL NOT FIT INTO',I5,' BITS')
 3003 FORMAT (/' ERROR ** ITEM',I8,' CANNOT BE PACKED USING',' MAX=',I8)
      END SUBROUTINE NPACK
!
!*****************************************************************************
!
      FUNCTION NSIGFG(DX)
!
! *** NSIGFG new by PJB 9 Mar 1994 ***
!
!H Returns the number of figures to print after the decimal point based on the
!H ESD dx
!A On entry DX is n ESD
!D The value of the ESD DX is used to determine the number of figures to print
!D after the decimal point. NSIGFG is returned in the range 1-5. NSIGFG is used
!D for printing parameters, shifs and ESd's after a least squares cycle.
!
      DEL = ABS(DX)
      IFIG = 1
    1 ISIG = IFIX(DEL)
      IF (ISIG.GT.0 .OR. IFIG.EQ.5) GOTO 2
      IFIG = IFIG + 1
      DEL = 10*DEL
      GOTO 1
    2 NSIGFG = IFIG
      RETURN
      END FUNCTION NSIGFG
!
!*****************************************************************************
!
      FUNCTION NSYMBL(I)
!
! *** NSYMBL by JCM 11 Oct 83 ***
!
!X
!C 13C
!H Finds whether the character I is one of the symbols recognised by the system.
!A I is an A1 character
!D The function is set to zero if I is not one of the 21 characters held
!D in the table ISMBOL in COMMON /CHARS/, otherwise it is set to the number
!D of the matching symbol.
!
      CHARACTER*1 I
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL

      DO J = 1, 21
        IF (I.EQ.ISMBOL(J)) GOTO 2
      ENDDO
      J = 0
    2 NSYMBL = J

      END FUNCTION NSYMBL
!
!*****************************************************************************
!
      FUNCTION NTICK(NTIME)
!
! *** NTICK by JCM 7 Jan 88 ***
!
!X
!C 16C
!H Advances its argument by 1 and sets the function to that value also.
!A NTIME on entry has some value, which on exit has been increased by 1
!D Used to keep track of the order of events in a job.
!
      NTIME = NTIME + 1
      NTICK = NTIME

      END FUNCTION NTICK
!
!*****************************************************************************
!
      SUBROUTINE NUMDEN(X,N1,N2,KI,KO)
!
! *** NUMDEN by JCM ***
!
!X
!C 13C
!H Converts a real number to the numerator and denominator of a fraction.
!A On entry X is a real number with absolute value normally less than 1
!A KI=0 If any range of fraction is allowed
!A  =1 If denominators of 2,3,4 and 6 only allowed
!A
!A On exit
!A KO=0 If X was 0 - in this case N1=N2=0
!A  =+1 If X was a +ve fraction < 1 . N1= numerator, N2=denominator
!A  =-1 If ABS(X) was as above but x was -ve.  N1, N2 set for ABS(X)
!A  =+2 If X WAS >1.  N1, N2 set for fractional part of X
!A  =-2 If X WAS < -1.  N1, N2 set for ABS(X)
!A  =99 If X would not convert to a fraction with single digit numerator
!A      and denominator, to 4 decimals
!A =-99 If X was as above and -ve
!N Used for printing fractions in OPSYM
!
      XX = X
      IEND = KI + 9 - 4*KI
      N1 = 0
      N2 = 0
      KO = 0
      IF (XX.EQ.0) RETURN
! KEEP SIGN OF NUMBER IN NEG - THEN WORK WITH MODULUS:
      NEG = IFIX(SIGN(1.0,XX))
      XX = ABS(XX)
      NBIG = 1
      IF (XX.LT.1.) GOTO 1
! REDUCE TO FRACTION LESS THAN 1:
      CALL FRACT(XX,Y,J)
      NBIG = 2
    1 DO IDEN = 2, IEND
! AVOID 5 IF ASKED TO CONSIDER ONLY 2,3,4,6:
        IF ((IEND.EQ.6) .AND. IDEN.EQ.5) GOTO 2
        ID1 = IDEN - 1
        DO INUM = 1, ID1
          IF (ABS((FLOAT(INUM)/FLOAT(IDEN))-XX).LT.0.0001) GOTO 4
          ITEMP = INUM/IDEN
          ATEMP = FLOAT(ITEMP)
          BTEMP = ATEMP - XX
          CTEMP = ABS(BTEMP)
        ENDDO
    2 ENDDO
! HERE NO FRACTION WILL FIT
      NBIG = 99
      GOTO 5
    4 N1 = INUM
      N2 = IDEN
    5 KO = NBIG*NEG
      RETURN
      END SUBROUTINE NUMDEN
!
!*****************************************************************************
!
      SUBROUTINE NWINT2
!
! *** NWINT2 updated by JCM 10 May 88 ***
!
!X
!C 7B
!H Writes out a new Crystal Data File for main program T2LSQ.
!P Should only be called in the context of T2LSQ, after a cycle
!P of refinement in which its parameters have been adjusted.
!D Copies out the file with new values for cell parameters and an
!D L ZERO card.
!
!O Writes the new file to unit NEWIN.
!
      CHARACTER*4 WD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /ZEROPT/ ZERO, KZERO

      ID = 0
    1 ID = ID + 1
      IF (ID.GT.NTOTAL(1)) GOTO 100
      READ (IO10,REC=ID,FMT=1000) ICARD
 1000 FORMAT (A80)
      L = LETTER(ICARD(1:1))
      IF (L.EQ.3) GOTO 3
      IF (L.EQ.12) GOTO 11
      IF (L.NE.9) GOTO 2
! OUTPUT NEW I CARD:
      CALL OTPUTI
      GOTO 1
! OUTPUT NEW C CARD WITH NEW VALUES:
    3 CALL CELNEW
      GOTO 1
! L CARD - DISCOVER IF "ZERO"
   11 CALL RDWORD(WD,LEN,3,IPT,80,0,IER)
      IF (WD.NE.'ZERO') GOTO 2
      WRITE (NEWIN,2005) ZERO
 2005 FORMAT ('L ZERO',F10.4)
      GOTO 1
! COPY UNCHANGED CARD:
    2 WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
 2000 FORMAT (80A1)
      GOTO 1
!
  100 RETURN
      END SUBROUTINE NWINT2
!
!*****************************************************************************
!
      LOGICAL FUNCTION ONCARD(C,WORD,A)
!
! *** ONCARD corrected by JCM 6 Mar 89 ***
!
!X
!C 13C
!H Finds a card which starts with the given letter and word, and reads
!H one number from it.
!
!A On entry C is the first letter of card sought,
!A          WORD is the A4 word to be found anywhere on any card starting
!A               with the given letter C
!A On exit  A is a real number, set (if the word was found) to the number
!A               following WORD.
!D ONCARD is set .TRUE. if the word is found, otherwise .FALSE.
!
      CHARACTER*1 C
      CHARACTER*4 WORD, WD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IINFO / IIN, ACOEFF(20)
      COMMON /IINFOW/ IIREAD(20)
      CHARACTER*4 IIREAD

      ONCARD = .TRUE.
      IF (C.NE.'I') GOTO 1
! I IS SPECIAL:
      IF (INREAD(9).GT.0) CALL INPUTI
      DO I = 1, IIN
        IF (WORD.EQ.IIREAD(I)) GOTO 3
      ENDDO
      GOTO 101
    3 A = ACOEFF(I)
      GOTO 100
! OTHER THAN I:
    1 L = LETTER(C)
      ID = IABS(INREAD(L))
      NCARD = ICDNO(L)
      IF (NCARD.LE.0) GOTO 101
      DO I = 1, NCARD
        CALL CARDIN(ID)
        ID = ID + NYZ
        IPT = 3
! ALLOW ANYTHING TO BE A WORD - THIS MAY HIT AGAINST > 10 CHARS
    5   CALL RDWORD(WD,LEN,IPT,IPT,80,-42,IER)
        IF (WD.EQ.WORD) GOTO 6
        IF (IER.NE.100 .AND. IPT.LE.80) GOTO 5
      ENDDO
      GOTO 101
! FOUND WORD - READ NUMBER:
    6 CALL RDREAL(A,IPT,IPT,80,IER)
      GOTO 100
  101 ONCARD = .FALSE.
  100 RETURN
      END FUNCTION ONCARD
!
!*****************************************************************************
!
      SUBROUTINE OPNFIL(L,M)
!
! *** OPNFIL updated by JCM 1 Aug 88 ***
!
!X
!C 13C
!H Opens file L according to requirements given in M;  L may be preset.
!
!A If on input L=-9999, behaves as L=NOPFIL(M)
!A If L >= 0, behaves similarly, but uses unit number L
!A Sets L=unit number used
!A For meanings of M, see NOPFIL
!
!O Opens unit (either L or chosen from table in IOTAB in /LOONEY)
!O Fills in tables in /LOONEY and /FINAME
!
      LOGICAL HASNAM, ISOPEN
      CHARACTER*40 FILTEM
      COMMON /FINAME/ FILNAM(15)
      CHARACTER*10 FILNAM
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LOONEY/ IOTAB(15), LUNTAB(15)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (L.EQ.-9999) THEN
        L = NOPFIL(M)
        RETURN
      ENDIF
! REQUEST TO OPEN SPECIFIC UNIT NUMBER:
! L MUST BE IN TABLE SO THAT NOPFIL CAN FIND IT:
    1 LL = 0
      K = 0
      DO I = 1, 15
! SET LL TO POINT TO FOUND L:
        IF (LUNTAB(I).EQ.L) LL = I
! SET K TO POINT TO FIRST VACANT SLOT:
        IF (IOTAB(I).EQ.0 .AND. K.EQ.0) K = I
      ENDDO
      IF (LL.EQ.0) GOTO 3
! L WAS ALREADY THERE - MAKE IT THE NEXT FREE:
      ITM = LUNTAB(K)
      LUNTAB(K) = L
      IOTAB(K) = IOTAB(LL)
      LUNTAB(LL) = ITM
      IOTAB(LL) = 0
      FILTEM = FILNAM(LL)
      FILNAM(LL) = FILNAM(K)
      FILNAM(K) = FILTEM
      GOTO 4
! L NOT IN TABLE - CHECK SUITABILITY:
    3 IF (L.LT.0) CALL ERRIN2(L,0,'cannot open unit','in OPNFIL')
      IF (IBMBER .NE. 0) RETURN
      LUNTAB(K) = L
    4 CONTINUE
      INQUIRE (L,NAMED=HASNAM,NAME=FILTEM,OPENED=ISOPEN)
      IOTAB(K) = 0
      IF (.NOT.ISOPEN) GOTO 7
      IOTAB(K) = M
      IF (HASNAM) FILNAM(K) = FILTEM
      RETURN
! NOTE TO PRUNE FILTEM IF NECESSARY
    7 L1 = NOPFIL(M)
      IF (L1.EQ.L) RETURN
      WRITE (LPT,3001) L, L1
 3001 FORMAT (/' *** PROGRAM ERROR ** OPNFIL CALL OF NOPFIL HAS RETURNED ',2I5)
      CALL BMBOUT

      END SUBROUTINE OPNFIL
!
!*****************************************************************************
!
      SUBROUTINE OPSYM(ISYM)
!
! *** OPSYM by JCM 11 Apr 83 ***
!
!X
!C 1B
!H Prints out the symmetry operators in either real or reciprocal space.
!A If ISYM=1  Prints out the NLAT non-primitive lattice translations and
!A            the NOPC coordinates of general equivalent positions held in
!A            ALAT and SYM in COMMON /SYMDA/
!A If ISYM=2  The printing is required in reciprocal space, and equivalent
!A            reflection indices and their relative phases are printed.
!D The sequence numbers of the operators are printed.  These are useful
!D for referring to specific operators.
!
!O The relevant operator list is written to unit LPT.
!
      CHARACTER*1 ISIG
      CHARACTER*15 IPH
      CHARACTER*1 LET(3,2), IICHAR(3,9), INUM(3), IMID(3), IDEN(3)
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10),         &
     &                ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / TSYM(3,3,48), TTRANS(3,48), MLTAB(48,48), TRANS1(3), TEMSYM(3,3)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

! SET UP X Y Z AND H K L IN ARRAY LET:
      DO I = 1, 3
        LET(I,1) = LETLOW(I+23)
        LET(I,2) = LETLOW(I+9)
      ENDDO
      LET(1,2) = LETLOW(8)
! SET + OR +-
      ISIG = ISPCE
      IF (CENTRC .OR. ((ISYM.EQ.2).AND.FRIEDL)) ISIG = '-'
! HEADING FOR RECIPROCAL SPACE:
      IF (ISYM.EQ.2) THEN
        WRITE (LPT,2000) ISIG
 2000   FORMAT (/' Equivalent reflections are:  +',A1,20X,'with  ','relative phases 2pi times:')
      ELSE
! HEADING FOR REAL SPACE:
        CALL MESS(LPT,1,'General equivalent positions are:')
        DO I = 1, NLAT
          DO J = 1, 3
            INUM(J) = ISPCE
            IMID(J) = IDIGIT(10)
            IDEN(J) = ISPCE
! IN CASE IT IS ZERO
! NOW PREPARE PRINTING OF FRACTION IN LATTICE VECTOR
! SET 'CONSIDER ONLY DENOMINATORS OF 2,3,4,6'
            CALL NUMDEN(ALAT(J,I),NNUM,NDEN,1,IRR)
            IF (IRR.EQ.0) GOTO 14
            IF (IRR.NE.1) GOTO 15
            INUM(J) = IDIGIT(NNUM)
            IDEN(J) = IDIGIT(NDEN)
            IMID(J) = '/'
   14     ENDDO
          WRITE (LPT,2002) (INUM(J),IMID(J),IDEN(J),J=1,3), ISIG
 2002     FORMAT (2X,2(4X,3A1,13X),4X,3A1,4X,'  +',A1)
          GOTO 13
! IN CASE ELEMENT OF LATTICE VECTOR NOT A FRACTION:
   15     WRITE (LPT,2003) (ALAT(J,I),J=1,3), ISIG
 2003     FORMAT (2X,2(F10.4,10X),F10.4,4X,'  +',A1)
   13   ENDDO
        WRITE (LPT,2004)
 2004   FORMAT (1X)
      ENDIF
      DO NO = 1, NOPC
! IP COUNTS ACROSS ARRAY OF CHARACTERS FOR PHASE, IF RECIPROCAL
        IP = 0
        IF (ISYM.EQ.2) THEN
! IF RECIPROCAL SPACE, USE INVERSE MATRIX TRANSPOSED, AND THE NEGATED
! TRANSLATION VECTOR CORRESPONDING TO THAT MATRIX:
          DO I = 1, 3
            TRANS1(I) = -TRANS(I,INVERS(NO))
          ENDDO
          CALL GMEQ(SYM(1,1,INVERS(NO)),TEMSYM,3,3)
          CALL TRANSQ(TEMSYM,3)
        ELSE
! IF REAL SPACE, USE OPERATOR AS STORED:
          CALL GMEQ(TRANS(1,NO),TRANS1,1,3)
          CALL GMEQ(SYM(1,1,NO),TEMSYM,3,3)
        ENDIF
        DO J = 1, 3
          L = 1
          CALL FRACT(TRANS1(J),ATEMP,ITEMP)
          IF (TRANS1(J).GE.0.00001) THEN
            DO M = 2, 6
              X = FLOAT(M)*TRANS1(J) + 5.E-5
              IF (AMOD(X,1.).GT..0001) GOTO 3
              IQ = INT(X)
              IICHAR(J,1) = IDIGIT(IQ)
              IICHAR(J,2) = '/'
              IICHAR(J,3) = IDIGIT(M)
              L = 4
              GOTO 5
    3       ENDDO
          ENDIF
    5     DO M = 1, 3
            IF (TEMSYM(J,M).EQ.0) GOTO 8
            IF (TEMSYM(J,M).LT.0) THEN
              IICHAR(J,L) = '-'
            ELSE
              IICHAR(J,L) = '+'
              IF ((L.EQ.1) .OR. ((ISYM.EQ.2).AND.(L.EQ.4))) IICHAR(J,L) = ISPCE
            ENDIF
            IICHAR(J,L+1) = LET(M,ISYM)
            L = L + 2
    8     ENDDO
          IF (L.NE.10) THEN
            DO K = L, 9
              IICHAR(J,K) = ISPCE
            ENDDO
          ENDIF
          IF (ISYM.NE.2) GOTO 2
          IF (MOD(L,2).NE.0) GOTO 2
          DO IJ = 1, 3
            IP = IP + 1
            IPH(IP:IP) = IICHAR(J,IJ)
            DO K = IJ, 6, 3
              IICHAR(J,K) = IICHAR(J,K+3)
            ENDDO
          ENDDO
          IP = IP + 2
          IPH(IP-1:IP-1) = LET(J,2)
          IPH(IP:IP) = '+'
    2   ENDDO
        IF (ISYM.EQ.2) THEN
          IF (IP.LE.0) THEN
            IPH(1:1) = IDIGIT(10)
            IP = 2
          ENDIF
          IPH(IP:15) = ' '
          WRITE (LPT,2006) NO, ((IICHAR(J,K),K=1,6),J=1,3), IPH(1:14)
 2006     FORMAT (1X,I2,3(5X,6A1,6X),10X,A14)
        ELSE
          WRITE (LPT,2005) NO, ((IICHAR(J,K),K=1,9),J=1,3)
 2005     FORMAT (1X,I2,3(5X,9A1,6X))
        ENDIF
      ENDDO

      END SUBROUTINE OPSYM
!
!*****************************************************************************
!
      SUBROUTINE ORTHG(IOP)
!
! *** ORTHG updated by JCM 11 Aug 88 ***
!
!X
!C 1A
!H Calculates matrices for the transformation of vectors in real or reciprocal
!H space, between crystallographic and orthogonal axes.
!A On entry IOP=1 means do not print result; =2 means print
!D The orthogonal set are defined as follows:
!D    X is parallel to a*
!D    Z is parallel to c
!D    Y makes up a right handed set.
!D
!D Thus H(orth) = h(cryst) times matrix ORTH.
!D H is is real space for matrix ORTH(,,1), and reciprocal for (,,2)
!
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      DO M = 1, 2
        DO I = 1, 3
          DO J = 1, 3
            ORTH(I,J,M) = 0.0
          ENDDO
        ENDDO
        N = 3 - M
        J = 5 - 2*M
        K = 2*M - 1
        ORTH(J,J,M) =  CELL(J,1,M)
        ORTH(2,J,M) =  CELL(K,2,M) * CELL(2,1,M)
        ORTH(2,2,M) =  CELL(K,3,M) * CELL(2,1,M)
        ORTH(K,J,M) =  CELL(2,2,M) * CELL(K,1,M)
        ORTH(K,2,M) = -CELL(2,3,M) * CELL(J,2,N) * CELL(K,1,M)
        ORTH(K,K,M) =  CELL(2,3,M) * CELL(J,3,N) * CELL(K,1,M)
      ENDDO
      IF (IOP.EQ.2) WRITE (LPT,2000) ORTH
 2000 FORMAT (/' Matrices for transformation of vectors to',            &
     &        ' orthogonal axes'/' Real space:'/3(3F10.4/),             &
     &        /' Reciprocal space:'/3(3F10.4/))

      END SUBROUTINE ORTHG
!
!*****************************************************************************
!
      SUBROUTINE ORTHO(H,OH,IR)
!
! *** ORTHO corrected by PJB 25 Jun 86 ***
!
!X
!C 1B
!H Carries out conversions between crystallographic and orthogonal axes.
!A H(3) is the input vector
!A OH(3) is the transformed vector
!A IR indicates which conversion is required:
!D If IR=1  H is a real space vector on crystallographic axes and OH a vector
!D          on the standard orthogonal axes
!D    IR=2  H is a reciprocal vector on crystallographic axes and OH is on
!D          the orthogonal axes
!D    IR=-1 H is on orthogonal axes and OH on real crystallographic axes
!D    IR=-2 H is on orthogonal axes and OH on reciprocal crystallographic axes
!P RECIP should have read the cell  dimensions.
!N The standard orthogonal axes are defined with x parallel to (100)
!N z parallel to [001] and y making up a righ-handed orthogonal set.
!N The matrices used in the conversions are those printed out by RECIP
!
      DIMENSION H(3), OH(3)

      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      IF (IR.GT.0) CALL MultiplyMatrices(H,ORTH(1,1,IR),OH,1,3,3)
      IF (IR.LT.0) CALL MultiplyMatrices(ORTH(1,1,3+IR),H,OH,3,3,1)

      END SUBROUTINE ORTHO
!
!*****************************************************************************
!
      SUBROUTINE OTPUTI
!
! *** OTPUTI updated by JCM 14 Jul 86 ***
!
!X
!C 6B
!H Outputs a new I card after a LSQ refinement, updating the cycle number.
!O Writes the new card to unit NEWIN.
!
      CHARACTER*80 ITEMP
      CHARACTER*4 WORD
      DIMENSION IDIG(5)
      COMMON /IINFOW/ IIREAD(20)
      CHARACTER*4 IIREAD
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      DO I = 1, 20
        IF (IIREAD(I).EQ.'CYC1') GOTO 2
      ENDDO
      GOTO 8
! CYC1 READ:
! REPLACE BY CURRENTLY REQUIRED CYC1
    2 IPT1 = 2
! SCAN CARD, AS IT MAY NOT BE THIS ONE THAT CYC1 IS ON:
    5 CALL RDWORD(WORD,NTEMP,IPT1,IPT,80,0,IER)
      IF (IER.EQ.100) GOTO 8
! IGNORE NUMBER:
      CALL RDREAL(A,IPT,IPT1,80,IER)
      IF (WORD.NE.'CYC1') GOTO 5
! FOUND CYC1:
      ITEMP(1:IPT-1) = ICARD(1:IPT-1)
      CALL INTDIG(LASTCY+1,IDIG,NDIG)
      IF (NDIG.LT.IPT1-IPT) GOTO 7
! NUMBER HAS MORE DIGITS - CHECK CARD END - IF CARD HAPPENS TO BE FULL WE
! COULD USE ANOTHER CARD, OR SHUFFLE ALONG LOOKING FOR 2 SPACES, BUT WE SHALL
! WAIT UNTIL THIS HAPPENS:
      IF (ICARD(80:80).EQ.' ') GOTO 7
      CALL ERRMES(-1,0,'new I card cannot be written')
      RETURN
    7 CALL INTCHR(IDIG,NDIG,ITEMP(IPT:IPT),80-IPT,0)
      ITEMP(IPT+NDIG+1:80) = ICARD(IPT1:IPT1+IPT+NDIG+1)
      ICARD = ITEMP
    8 WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
 2000 FORMAT (80A1)
  100 RETURN
      END SUBROUTINE OTPUTI
!
!*****************************************************************************
!
      SUBROUTINE PARITY(N,M,EVEN)
!
! *** PARITY by JCM 26 Sep 85
!
!X
!C 11C
!H Finds out whether N is odd or even.
!A On entry N is the integer to be tested
!A On exit  EVEN is true if N is even, false if N is odd
!A          If N is even M=N/2; if odd M=(N-1)/2
!
      LOGICAL EVEN

      M1 = N/2
      M2 = (N-1)/2
      EVEN = (M1.NE.M2)
      IF (N.LE.0) EVEN = .NOT.EVEN
      M = M2
      IF (EVEN) M = M1

      END SUBROUTINE PARITY
!
!*****************************************************************************
!
      SUBROUTINE PARNAM(IPNAM1,IPNAM2,N,M)
!
! *** PARNAM updated by JCM 8 May 90 ***
!
!X
!C 6C
!H Obtains the printing name of a LSQ parameter.
!A On entry N specifies what the integer M is:
!A      N=1 means M is the number of a basic variable
!A      N=2 means M is the number of a variable
!A      N=3 means M is a packed parameter specification
!A On exit IPNAM1 contains the A4 genus name,
!A         IPNAM2 contains the A4 species name
!P LSETUP must have put the vocabulary into /WORDS/ etc
!P VARMAK must have set up variable structure and pointers
!D On exit KPHASE in /PHASE/ and KSOURC in /SOURCE/ have been given the
!D current values of the phase and source.
!D From family, genus and species, decides source of genus and species names
!D A large, +ve value for the species type indicates only 1 name, not 2
!D Picks up names via PRIWRD
!
      INCLUDE 'PARAMS.INC'
      
      CHARACTER*4 IPNAM1, IPNAM2
      LOGICAL ONENAM
      DIMENSION LPAK(5)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /MPODA / NMPAT, NMPOL, MPATAB(20), MPNMTB(150), NCLUMP,    &
     &                KCLUMP(100), MPTAB(21), POLAMP(200,6), KPOLMP(200)&
     &                , NCMAT, CONMAT(600,2)
      COMMON /MPODAC/ MPNAM(200)
      CHARACTER*4 MPNAM

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)

      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      MM = M
      IPNAM1 = ' '
      IPNAM2 = ' '
      GOTO (1,2,3), N
! N=1 - M SPECIFIES BASIC VARIABLE - WHICH VARIABLE WAS IT?
    1 MM = LBSVR(MM)
! N=2 - M SPECIFIES VARIABLE - WHICH PARAMETER WAS IT?
    2 MM = LVRPR(MM)
! N=3 - M IS A PACKED PARAMETER - UNPACK IT:
    3 CALL KUNPAK(MM,IFAM,IGEN,ISPC,KPHASE,KSOURC)
! THIS MENDS NON-MULTI, BUT MAY NEED ADJUSTING FOR MULTI:
      IF (KPHASE.EQ.0) KPHASE = 1
      IF (KSOURC.EQ.0) KSOURC = 1
      ONENAM = (IGEN.EQ.1 .AND. (IFAM.EQ.1.OR.IFAM.EQ.6))
! BRANCH ON FAMILY:
      GOTO (11,12,13,14,15,16), IFAM
      CALL ERRIN2(IFAM,0,'PARNAM asked for name in family',' ')
      IF (IBMBER .NE. 0) RETURN
! FAMILY 3 - SET ISP TO BE ITS SPECIES TYPE:
   13 ISP = LF3SP(IGEN,KPHASE,KSOURC)
      GOTO 30
! FAMILY 4 - "VERY LONG VECTORS" - SET SPECIES TYPE:
   14 ISP = -NSPCPS(4,KPHASE)
      GOTO 30
! FAMILY 1 - SET ISP TO BE ITS SPECIES TYPE:
   11 ISP = LF1SP(IGEN)
      GOTO 30
! FAMILY 6 - SET ISP TO BE SPECIES TYPE:
   16 ISP = LF6SP(IGEN,KSOURC)
! COPY GENUS NAME:
   30 IF (ONENAM) THEN
        IPNAM1 = ' '
        GOTO 23
      ELSE
        CALL PRIWRD(IFAM,IGEN,0,IPNAM1,1)
      ENDIF
      IF (IPNAM1.EQ.'XXXX') GOTO 22
!
! PICK UP SPECIES NAME BY CONSULTING, FIRST, TO SEE WHAT SORT OF SPECIES.
! POSSIBILITIES FOR ISP ARE:
!    -VE MEANS SPECIES ARE POSITIVE INTEGERS, 1, 2 ETC, UP TO IABS(ISP)
!     0 MEANS THERE ARE IN FACT NO SPECIES
!    +VE MEANS THAT EACH SPECIES HAS A NAME (LIKE TFAC, ASYM, PROR  . . ,
!   OR X, Y, Z, B11 . . . OR A*, B*, . . .) TO BE FOUND IN THE GENERAL WORD TABLE
      IF (ISP) 21, 22, 23
!
! IF 0, COMPLAIN.
   22 WRITE (LPT,3001) M, N, IFAM, IGEN, ISPC
! IF +VE, NAME ALREADY PACKED IN MM FOR LOOK-UP IN PRIWRD:
   23 CALL PRIWRD(IFAM,IGEN,ISPC,IPNAM2,0)
! MAY NOT ALL BE PRESENT - TRY JUST SPECIES NAME IF NOT FOUND:
      IF (IPNAM2.NE.'XXXX') GOTO 100
      CALL PRIWRD(IFAM,0,ISPC,IPNAM2,0)
      IF (IPNAM2.EQ.'XXXX') GOTO 22
      GOTO 100
! IF -VE WANT TO TURN ISPC INTO DIGITS:
   21 CALL INTDIG(ISPC,LPAK,NDIG)
      CALL INTCHR(LPAK,NDIG,IPNAM2,4,0)
      GOTO 100
! FAMILY 2 IS SPECIAL BECAUSE ITS GENUS NAMES ARE ATOM NAMES:
   12 IF (MULFAS) THEN
        IPNAM1 = ATNA(IGEN,KPHASE)
      ELSE
        IPNAM1 = ATNAME(IGEN)
      ENDIF
!    AS IN FAMILY 1, FOR THE SPECIES WE HAVE POINTERS INTO /WORDS/.
      CALL PRIWRD(IFAM,0,ISPC,IPNAM2,0)
      GOTO 100
! FAMILY 5 - EXPECT 1 LONG GENUS AT FIRST:
   15 CALL MF5ADD(ISPC,IG,IS,-1)
! GENUS NAME IS ATOM NAME : IG AT PRESENT IS MULTIPOLE ATOM NAME:
      IF (MULFAS) THEN
        IPNAM1 = ATNA(MPATAB(IG),KPHASE)
      ELSE
        IPNAM1 = ATNAME(MPATAB(IG))
      ENDIF
! SPECIES NAME COMES FROM TABLE:
      IPNAM2 = MPNAM(ISPC)
  100 RETURN
 3001 FORMAT (/' ERROR ** PARNAM CANNOT FIND NAME - M,N,IFAM,IGEN,ISPC =',5I6)

      END SUBROUTINE PARNAM
!
!*****************************************************************************
!
      SUBROUTINE PARRD(IPT1,IPT2,K,IFAM,IGEN,ISPC)
!
! *** PARRD updated by JCM 3 Aug 92 ***
!
!X
!C 6C
!H Reads a LSQ parameter specification from a given card at given point.
!A On entry IPT1 points to the starting character on the card in /SCRACH/
!A On exit  IPT2 points to the next unread character on the card in /SCRACH/
!A          K is the packed parameter unless K is -ve:
!A K=-99 'ONLY' read
!A K=-100 'ALL' read as first word, in which case IFAM is expected to
!A                 be set, and possibly one but not both of ISPC and IGEN
!A K=-100 and IFAM -ve means that the word after 'ALL' was composite
!A        Composite words have come from MAIN program table with -ve small
!A        entries.  For SFLSQ and the like,
!A      IFAM=-1 'ALL XYZ'
!A      IFAM=-2 'ALL BIJ'
!A      IFAM=-3 'ALL XYZT'
!A      IFAM=-4 'ALL CELL'
!A      IFAM=-5 'ALL XYZB'
!A      IFAM=-6 'ALL XYZS'
!
!A K=-101  'XYZ' as 2nd word;  1st word was genus in IGEN
!A K=-102  'BIJ' as 2nd word;  1st word was genus in IGEN
!A K=-103  'XYZT' as 2nd word;  1st word was genus in IGEN
!A K=-104  'CELL' as 2nd word;  1st word was genus in IGEN
!A K=-105  'XYZB' as 2nd word;  1st word was genus in IGEN
!A K=-106  'XYZS' as 2nd word;  1st word was genus in IGEN
!
!P The tables used must be set up by LSETUP
!
!D PARRD expects only a limited vocabulary, identified via TBLFND, being
!D    a known genus followed by a species name
!D    'ONLY'
!D    'ALL' followed by anything making sense
!D    a genus name followed by a composite word like 'XYZ', 'BIJ', 'XYZT' 'XYZB'
!D    a species name alone, in the special case of family 1 or 6, genus 1
!D    (and for PR, if SCAL is read, ignores a 1 if it follows) -
!D    wherever it makes sense.
!
      CHARACTER*4 LWD1, LWD2
      COMMON /MPODA / NMPAT, NMPOL, MPATAB(20), MPNMTB(150), NCLUMP,    &
     &                KCLUMP(100), MPTAB(21), POLAMP(200,6), KPOLMP(200)&
     &                , NCMAT, CONMAT(600,2)
      COMMON /MPODAC/ MPNAM(200)
      CHARACTER*4 MPNAM

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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

! IN CASE MULTI, SET SOURCE AND PHASE AS DEFAULT:
      KPHASE = JPHASE
      KSOURC = JSOURC
! IDENTIFY FIRST NAME - READ AND OFFER TO TBLFND:
      K = 0
      IFAM = 0
      IGEN = 0
      ISPC = 0
! THIS CALL MAY FLIP KSOURC OR KPHASE:
      CALL RDWORD(LWD1,L1,IPT1,IPT,80,1,IER)
! IF EMPTY WORD, END OF LINE:
      IF (IER.EQ.100) GOTO 100
! FIND WORD IF POSSIBLE:
      CALL TBLFND(LWD1,K,IFAM1,IGEN1,ISPC1,KP,KS)
! RECORD ANY PHASE OR SOURCE INFORMATION GLEANED BY TBLFND:
      IF (KP.NE.0) KPHASE = KP
      IF (KS.NE.0) KSOURC = KS
! IF K IS ZERO, TBLFND COULD NOT RECOGNISE AT ALL:
      IF (K.EQ.0) GOTO 99
! IF TBLFND FOUND WORD, IT HAS A PARALLEL TABLE OF INTEGERS SAYING WHAT THE
! WORDS ARE.  NEGATIVE K INDICATES A NON-PARAMETER WORD (LIKE ALL, ONLY
! - THESE TWO ARE THE ONLY ONES ALLOWED AS FIRST WORD)
!
! IF K +VE, TBLFND HAS FOUND EITHER A GENUS OR A SPECIES NAME.  IF SPECIES,
! SHOULD BELONG TO FAMILY 1 GENUS 1, AND TBLFND SHOULD HAVE DETECTED THIS
! AND SET GENUS AND FAMILY.
!
! SO ACCEPTABLE FIRST WORDS ARE:
!     A SPECIES NAME (IMPLICITLY FOR FAMILY1, GENUS 1 - READ NO MORE)
!     A GENUS NAME (SETTING IFAM & IGEN, EXPECTS TO READ SPECIES, OR BIJ OR
!                   XYZ ETC. NEXT)
!     'ALL' (EXPECTS ANOTHER WORD)
!     'ONLY' ( READ NO MORE)
!
! 'ONLY'
      IF (K.EQ.-99) GOTO 101
! SPECIES NAME - SPECIFICATION COMPLETE IF FAM=1 OR 6, GEN=1:
      IF ((IFAM1.EQ.1.OR.IFAM1.EQ.6) .AND. IGEN1.EQ.1 .AND. ISPC1.NE.0) THEN
        IFAM = IFAM1
        IGEN = IGEN1
        ISPC = ISPC1
! IF SCAL IGNORE ANY SUBSEQUENT 1:
        IF (IFAM.EQ.6) THEN
          CALL RDINTG(I,IPT,IPT2,80,IER)
          IF (I.EQ.1) IPT = IPT2
        ENDIF
        GOTO 102
      ENDIF
! CHECK ENOUGH INFO TO PROCEED WITH SECOND WORD:
      IF (K.LT.-102) GOTO 99
      IF ((IFAM1.EQ.0.OR.IGEN1.EQ.0) .AND. (K.GT.0)) GOTO 99
! NOW WE LOOK AT SECOND WORD AND CHECK COMPATIBLE WITH FIRST (-1 ON INPUT
! ALLOWS AN INTEGER TO BE READ AS A WORD):
      CALL RDWORD(LWD2,L2,IPT,IPT,80,-1,IER)
      IF (IER.EQ.100) GOTO 99
      CALL TBLFND(LWD2,IANS2,IFAM2,IGEN2,ISPC2,KP,KS)
      IF (KP.NE.0) KPHASE = KP
      IF (KS.NE.0) KSOURC = KS
! WORDS ALLOWED IN SECOND PLACE ARE:
!      A SPECIES NAME FITTING THE FIRST GENUS NAME
!      A COMPOSITE WORD FOLLOWING 'ALL' OR A GENUS NAME - THE COMPOSITE WORDS
!                  ARE 'BIJ', 'XYZ' OR 'XYZT' ETC. FOR SFLSQ
!      SOMETHING ELSE FOLLOWING 'ALL' - THIS MAY BE A FAMILY NAME,
!                  A GENUS NAME OR A SPECIES NAME
!
! ALL COMPOSITE WORDS ARE ALLOWED EXCEPT 'ALL' AND 'ONLY':
      IF (IANS2.EQ.-100 .OR. IANS2.EQ.-99) GOTO 99
! IF TBLFND COULD NOT FIND IT, IT MAY BELONG TO A USER'S TABLE IN FAMILY 5:
      IF (IANS2.EQ.0) THEN
! BUT ONLY IF MULTIPOLE:
        IF (.NOT.MPL) GOTO 99
        IF (IFAM1.NE.2) GOTO 99
        IFAM1 = 5
        IG = MPNMTB(IGEN1)
        IGEN1 = 1
        ISPC2 = NCFIND(LWD2,MPNAM(MPTAB(IG)),MPTAB(IG+1)-MPTAB(IG))
        IF (ISPC2.EQ.0) GOTO 99
! COUNT FROM BEGINNING OF TABLE:
        ISPC2 = ISPC2 + MPTAB(IG) - 1
        GOTO 5
      ENDIF
! IF FIRST WORD IS 'ALL':
      IF (K.EQ.-100) THEN
        IF (IANS2.LT.0) GOTO 4
! 'ALL' FOLLOWED BY GENUS OR SPECIES:
        IFAM = IFAM2
        IGEN = IGEN2
        ISPC = ISPC2
        GOTO 101
! BOTH WORDS ARE COMPOSITE (EG 'ALL BIJ', 'ALL XYZ')
    4   IFAM = IANS2
        GOTO 101
      ENDIF
! FIRST IS NOT 'ALL', SO SHOULD HAVE BEEN GENUS:
      IF (IANS2.GT.0) GOTO 5
! GENUS FOLLOWED BY 'XYZ' OR 'BIJ' OR 'XYZT'
      K = IANS2 - 100
      IFAM = IFAM1
      IGEN = IGEN1
      ISPC = ISPC1
      GOTO 101
! CANNOT MAKE SENSE OF PARAMETER SPEC:
   99 CALL ERRIN2(IPT,2,'cannot read parameter at point',' ')
      IPT2 = 81
      GOTO 100
! GENUS FOLLOWED BY SPECIES:
    5 IFAM = IFAM1
      IGEN = IGEN1
      ISPC = ISPC2
  102 K = KPAK(IFAM,IGEN,ISPC,KPHASE,KSOURC)
  101 IPT2 = IPT
  100 RETURN

      END SUBROUTINE PARRD
!
!*****************************************************************************
!
      SUBROUTINE PLN3AD(I,J,K)
!
! *** PLN3AD by JCM 3 Oct 84 ***
!
!X
!C 1A
!H A specialist routine used during the formation of the reciprocal unit
!H cell, to offer up 3 planes as boundaries, in cubic space groups.
!A On entry I,J,K specify 3 planes, by pointing to symmetry elements.
!A                The array AXI(,I,,) holds the axes of these elements
!A                in reciprocal space, and these axes are normal to the
!A                planes in question
!
!D The 3 axes are first oriented so that the angles between the planes
!D are acute.
!
      DIMENSION A(3), B(3), C(3)
      COMMON /SCRAT / AXI(3,24,2), MIRROR(24), D(3,3), PL1(3), PL2(3),  &
     &                PL3(3), HT(3), ASY(3,4), NSTAT(4), NOPL, NICE,    &
     &                VOL, MOP1, MOP2

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      CALL GMEQ(AXI(1,I,2),A,1,3)
      CALL GMEQ(AXI(1,J,2),B,1,3)
      CALL GMEQ(AXI(1,K,2),C,1,3)
! MAKE AN ACUTE ANGLE BETWEN A AND B:
      IF (SCALPR(A,B).LT.0.) CALL GMREV(B,B,1,3)
! MAKE AN ACUTE ANGLE BETWEN A AND C:
      IF (SCALPR(A,C).LT.0.) CALL GMREV(C,C,1,3)
      CALL VECPRD(B,C,PL1)
      CALL VECPRD(C,A,PL2)
      CALL VECPRD(A,B,PL3)
      CALL FIXUNI(PL1,3)
      IF (IBMBER .NE. 0) RETURN
      CALL FIXUNI(PL2,3)
      IF (IBMBER .NE. 0) RETURN
      CALL FIXUNI(PL3,3)
      IF (IBMBER .NE. 0) RETURN

      END SUBROUTINE PLN3AD
!
!*****************************************************************************
!
      SUBROUTINE POLUNI
!
! *** POLUNI by PJB 8 Aug 83 ***
!
!X
!C 1A
!H A specialist routine to "polish" the edges of a found asymmetric unit
!H by specifying exactly how its faces and edges should be treated.
!
!P POLUNI is called from the end of SYMUNI, and would not be useful
!P outside this context.
!
!D Sets the array MARK in /GUNIT/ to indicate the exact treatment of
!D faces and edges of the reciprocal cell asymmetric unit in order to
!D deduce the multiplicites of reflections occurring on them.
!
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      COMMON /GUNIT / MARK(3,2), BSYM(3,3), IBOX, KOM11
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / AXI(3,24,2), MIRROR(24), D(3,3), PL1(3), PL2(3),  &
     &                PL3(3), HT(3), ASY(3,4), NSTAT(4), NOPL, NICE,    &
     &                VOL, MOP1, MOP2
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
!
!  CLEAR MARK TO 1
      DO I = 1, 3
        DO J = 1, 2
          MARK(I,J) = 1
        ENDDO
      ENDDO
!  DEAL WITH P1 AND P-1
      IF ((NOP.EQ.1) .AND. .NOT.FRIEDL) GOTO 100
      IF (NOPC.GT.1) GOTO 14
!  FIND ANY LINE IN THE PLANE
      CALL INVENT(ASYM(1,1),ASYM(1,1),BSYM(1,1))
      CALL VECPRD(ASYM(1,1),BSYM(1,1),BSYM(1,2))
      MARK(1,1) = 4
      GOTO 100
!  WORK OVER SYMMETRY ELEMENTS
   14 DO N = 2, NOPC
        IF (IABS(NORD(N)).GT.100) GOTO 1
        IORD = IABS(NORD(N))
        IF (MIRROR(N).EQ.0) GOTO 2
!  PROCEDURE IF MIRROR PLANE,EITHER END OF AXIS WILL DO
        DO IR = 1, 2
          CALL EQVEC(ASYM,AXI(1,N,1),NASYM,M,0)
          IF (M.GT.NASYM) GOTO 15
!  MARK PLANE AS MIRROR
          MARK(M,1) = 2
          GOTO 16
   15     CALL GMREV(AXI(1,N,1),AXI(1,N,1),3,1)
        ENDDO
!  JUMP IF NOT A DIAD AXIS
   16   IF (NORD(N).EQ.-2 .AND. .NOT.FRIEDL) GOTO 1
!  PROCEDURE FOR SYMMETRY AXES
    2   CALL INBOX(AXI(1,N,2),IN)
        IF (IN) 1, 3, 4
!  SYMMETRY AXIS INSIDE UNIT - ERROR\
    3   WRITE (LPT,3000) (AXI(I,N,1),I=1,3), N
        CALL BMBOUT
        RETURN
!  AXIS IS ON UNIT
    4   IF (IN.GT.10) GOTO 5
        IF (IORD.GT.2) GOTO 3
!  HERE FOR DIAD AXIS ON A PLANE - NOT AT CORNER
        CALL VECPRD(ASYM(1,IN),AXI(1,N,2),BSYM(1,IN))
        MARK(IN,1) = 3
!  DECIDE WHICH EDGE TO INCLUDE
        IF (NASYM.GT.2) GOTO 8
        MARK(3,2) = -IABS(MARK(3,2))
        GOTO 1
    8   M1 = MOD(IN,3) + 1
        M2 = MOD(M1,3) + 1
        M = M1
        L = M2
        IF (SCALPR(BSYM(1,IN),EDGE(1,M)).GT.0.) GOTO 13
        M = M2
        L = M1
!  CHECK MARK NOT ZERO ALREADY
   13   IF (MARK(L,2).NE.0) MARK(M,2) = 0
        GOTO 1
!  AXIS ON EDGE
    5   IN = IN - 10
        IF (NASYM.GT.2) GOTO 11
        IF (IORD.NE.3 .OR. .NOT.FRIEDL) GOTO 11
!  SPECIAL BLISTER FOR BAR 3 AXIS
        MARK(IN,2) = -3*IABS(MARK(IN,2))
        RANG = TWOPI/6.
        GOTO 9
   11   MARK(IN,2) = ISIGN(MARK(IN,2)*IORD,NORD(N))
        IF (MARK(IN,2).LT.0 .AND. .NOT.FRIEDL) MARK(IN,2) = MARK(IN,2)/2
        IF (IORD.EQ.2) GOTO 1
        RANG = TWOPI/IORD
        IF (NASYM.EQ.2) GOTO 9
        IF (NORD(N).LT.0 .AND. .NOT.FRIEDL) RANG = RANG*2.
    9   IF (ABS(RANG-ANG(IN)).GT..0001) GOTO 1
        M = MOD(IN,3) + 1
!  MARK POINTS ON PLANE AND AXIS NOT TO BE USED
        IF (NASYM.LT.3) GOTO 12
!  NO EDGES
!  IS AN EDGE ALREADY MARKED ZERO?
        M1 = MOD(M,3) + 1
        IF (MARK(M1,2).EQ.0) GOTO 12
!  IF THAT NOT ZERO MARK THE OTHER ONE
        MARK(M,2) = 0
        M = M1
   12   MARK(M,1) = 0
    1 ENDDO
!  PUT IN EXTRA DIVISION IF A MIRROR PLANE GOES THROUGH AN EDGE
      IF (NASYM.LT.2) GOTO 100
!  NO EDGES:
      J = 2
      K = 3
      DO I = 1, 3
        IF (MARK(I,1).EQ.2 .OR. MARK(J,1).EQ.2) MARK(K,2) = MARK(K,2)*2
        IF (NASYM.EQ.2) GOTO 100
        J = K
        K = I
      ENDDO
  100 RETURN
 3000 FORMAT (/' ERROR ** IN POLUNI - SYMMETRY AXIS ',3F5.1,' INSIDE UNIT - OPERATOR NUMBER',I4)

      END SUBROUTINE POLUNI
!
!*****************************************************************************
!
      SUBROUTINE PRBLOK
!
! *** PRBLOK Modified by PJB 9-Mar-1994 ***
!
!X
!C 6C
!H Prints a block of shifts in parameters all relating to the same atom
!H in LSQ applications involving structure parameters.
!P IBUFF in /ATBLOK/ is the number of shifts to be printed. The calling
!P routine APSHxx must control IBUFF and store shift information in the
!P arrays in /ATBLOK and /ATBLOC.
!
!D If IBUFF=0 exits.  Otherwise prints shifts across the page for one specific
!D atom as stored.  Sets IBUFF=0
!
!O Prints blocks of new parameter, esd, shift, old parameter, shift/esd
!O labelled appropriately.
!
      CHARACTER*80 FBUF
      CHARACTER*6 LABEL(4)

      COMMON /ATBLOC/ NAME, IPNAME(12)
      CHARACTER*4 NAME, IPNAME
      COMMON /ATBLOK/ IBUFF, PNEW(12), PESD(12), PSHIFT(12), POLD(12),  &
     &                PSESD(12)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      DATA LABEL/'   NEW', '   ESD', ' SHIFT', '   OLD'/

      IF (IBUFF.EQ.0) GOTO 100
      WRITE (LPT,2001) NAME, (IPNAME(II),II=1,IBUFF)
 2001 FORMAT (/2X,A4,12(3X,A4,2X))
      FBUF(1:4) = '(A6,'
      J = 5
      DO II = 1, IBUFF
        WRITE (FBUF(J:),2002) NSIGFG(PESD(II))
 2002   FORMAT ('F9.'I1,',')
        J = J + 5
      ENDDO
      FBUF(J-1:) = ')'
      WRITE (LPT,FBUF) LABEL(1), (PNEW(II),II=1,IBUFF)
      WRITE (LPT,FBUF) LABEL(2), (PESD(II),II=1,IBUFF)
      WRITE (LPT,FBUF) LABEL(3), (PSHIFT(II),II=1,IBUFF)
      WRITE (LPT,FBUF) LABEL(4), (POLD(II),II=1,IBUFF)
      WRITE (LPT,2006) (PSESD(II),II=1,IBUFF)
 2006 FORMAT (' SH/SD',12F9.5)
      IBUFF = 0
  100 RETURN

      END SUBROUTINE PRBLOK
!
!*****************************************************************************
!
      INTEGER FUNCTION PREFIN(PROGRM)
!
! *** PREFIN updated by JCM 28 Apr 92 ***
!
!X
!C 13A
!H Makes the Crystal Data File readable in a random order by writing it
!H to a scratch file.
!A On entry PROGRM is the A6 name of the calling program, to head the output
!
!D Creates a direct access file number IO10, writes to it the given
!D Crystal Data File as A80 records, and keeps an index so that subsequently
!D any "card" may be read or re-read as required.
!D
!D Sets ICDNO(1-26) to be the number of "cards" read for each letter, except Y
!D      and Z, which are passed through anywhere (printing out the Y).
!D Sets INREAD(1-26) to be the start record for each letter chunk.
!D Sets NTOTL to be the total number of records read.
!D Sets IERR=0 so that errors later may reset it.
!D
!D Also initialises system by a call to INITIL.  Jobs wishing to intervene
!D to change parameters must say, e.g.:
!D      CALL INITIL('NAME')
!D      change whatever wanted
!D      CALL PREFIN('NAME')
!
!D PREFIN reads "cards" labelled with a letter from A-Z in column 1,  from a
!D dataset called the "crystal data".  This is read from logical unit
!D ICRYDA, which may be assigned to a specific unit number in
!D the main program, or left unassigned so that the call to OPNFIL
!D will assign a unit number and ask interactively for a file name.
!D
!D    The "cards" give information about the current problem.
!D    Letters used so far are:
!D    A   atomic positions (read by ATOPOS)
!D    B   bond length and angle instructions (read by  main program BONDS)
!D    C   cell dimensions (read by RECIP)
!D    D   diffraction or diffractometer data (read by SETDC)
!D    E   extinction correction data (read by EXTINC)
!D    F   form factors (read by SETFOR)
!D    G   Gauss integration and other data for abs corr (read by SETGAU)
!D    I   instruction "cards" (read by main programs)
!D    J   multipole information (read by INPUTJ)
!D    L   least squares refinement data (read by, e.g., INPLSF)
!D    M   data for calculation of fourier maps (read by SETFOU)
!D    N   title (a single "card, read by INPUTN)
!D    P   polarisation data (read by SETPOL)
!D    Q   data for magnetic structures (read by DOMAG)
!D    S   symmetry "cards" (read by SYMOP)
!D    T   anisotropic temperature factors  (read bY SETANI)
!D    U   indices supplied to force use of a particular unit asymmetric
!D        unit (read by SYMUNI)
!D    V   choice of representation of the space group
!D    W   wave function data (read by INPUTW)
!D    X   entirely under user's control
!D    Y   comment repeated on ouput LPT
!D    Z   comment not repeated on output
!D
!D    All "cards" with the same initial letter must come together but the
!D    groups may be in any order.
!D
!D    Except in a few instances such as  "F" "cards", where if a table of
!D    values is given, SETFOR expects  "cards" to be in the correct sequence,
!D    the "cards" within a group may be in any order.
!D
!D    The restriction about having all "cards" of one letter together does not
!D    apply to "cards" starting Y or Z.
!D
!D    A completely blank line is ignored.
!D
!D    An end of file, or any "card" with a non letter symbol in column 1
!D    terminates a set of crystal data. Any further data in the file may
!D    be read subsequently by the user's program.
!D
!D If an "M GET" card is given, the user wishes to take his basic data, and any
!D saved Fourier maps, from a dataset supplied, not from this crystal data.  The
!D SUBROUTINE MAJUST is called to adjust his file IO10 in this case.
!
!I Reads in the Crystal Data File
!O Lists on the lineprinter output a summary of what it found.
!
      CHARACTER*(*) PROGRM
      LOGICAL ENDIP
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
! JCC Initialise return value ( successful = 1, anything else is failure)
      PREFIN = 1
!
! INITIALISE WHOLE SYSTEM - DATE, TIME, CONSTANTS, I/O UNIT NUMBERS ETC:
! UNLESS THAT HAS ALREADY BEEN DONE ONCE IN THE JOB
      CALL INITIL(PROGRM)
! OPEN CRYSTAL DATA
    9 ENDIP = .FALSE.
      MESSAG = 'Crystal data file'
      NAMFIL = '.CCL'
      CALL OPNFIL(ICRYDA,111)
! Check the value of ICRYDA. An error may have occurred on opening the file
      IF (ICRYDA.EQ.-1) THEN
        PREFIN = 0
        RETURN
      ENDIF
! OPEN SCRATCH FILE TO HOLD COPY OF CRYSTAL DATA:
      IO10 = NOPFIL(10005)
! SET NO ERRORS DETECTED ON INPUT - SYSTEM PLOUGHS ON AS LONG AS IT CAN:
      IERR = 0
! START COUNT IN OUTPUT:
      ID = 0
      CALL CDFIN(1,ID,ENDIP)
      NTOTL = NTOTAL(1)

      END FUNCTION PREFIN
!
!*****************************************************************************
!
      SUBROUTINE PRILIS(AVAL,IPT1,IPT2)
!
! *** PRILIS updated by JCM 22 Aug 86 ***
!
!X
!C 13C
!H Prints a list of real numbers held in an array, 5 per line.
!A On entry AVAL holds the required real numbers,
!A          IPT1 points to the first to be printed,
!A          IPT2 points to the last to be printed.
!
!O Writes out AVAL(IPT1 to IPT2), in format G12.5, 5 per line.
!
      DIMENSION AVAL(IPT2), ATEMP(20)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      IN = 0
      DO I = IPT1, IPT2
        IN = IN + 1
        IF (IN.GT.20) THEN
          WRITE (LPT,2000) ATEMP
          IN = 1
        ENDIF
        ATEMP(IN) = AVAL(I)
      ENDDO
! WRITE ANY SINGLE VALUES GATHERED INTO ATEMP:
      IF (IN.GT.0) WRITE (LPT,2000) (ATEMP(I),I=1,IN)
      RETURN
 2000 FORMAT (4(1X,5G12.5/))
      END SUBROUTINE PRILIS
!
!*****************************************************************************
!
      SUBROUTINE PRIPLN(A,IR)
!
! *** PRIPLN updated by JCM 12 Nov 89 ***
!
!X
!C 13C
!H Given the normal to a plane face in A, prints the equation of the plane.
!A On entry A is a 1x3 array containing 3 elements of the normal.
!A          IR=1 indicates that the normal is in terms of h,k,l, and
!A          IR=2 in terms of x,y,z (for later use).
!O Writes on unit LPT the equation of the plane.
!N At present every plane is assumed to go through the origin.
!
      CHARACTER*12 NEQN
      DIMENSION A(3)
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      IPT = 0
      NEQN = ' '
      DO I = 1, 3
        IF (A(I)) 3, 2, 4
! NEGATIVE TERM:
    3   IPT = IPT + 1
        NEQN(IPT:IPT) = '-'
        GOTO 5
! POSITIVE TERM - OMIT EXPLICIT PLUS IF AT START:
    4   IF (IPT.EQ.0) GOTO 5
        IPT = IPT + 1
        NEQN(IPT:IPT) = '+'
    5   K = IABS(NINT(A(I)))
        IF (K.NE.1) THEN
! COEFFICIENT OTHER THAN 1:
          IPT = IPT + 1
          NEQN(IPT:IPT) = IDIGIT(K)
        ENDIF
        IPT = IPT + 1
! PICK LETTER:
        IF (IR.NE.2) THEN
          L = I + 9
          IF (I.EQ.1) L = 8
        ELSE
! WANT X,Y,Z:
          L = I + 22
        ENDIF
        NEQN(IPT:IPT) = LETLOW(L)
    2 ENDDO
! TIDY:
      IF (IPT.EQ.0) GOTO 100
      IF ((IPT.EQ.3) .AND. (NEQN(2:2).EQ.'-')) THEN
        NEQN(4:4) = NEQN(3:3)
        NEQN(3:3) = '='
        NEQN(2:2) = '>'
      ELSE
        NEQN(IPT+1:IPT+1) = '>'
        NEQN(IPT+2:IPT+2) = '='
        NEQN(IPT+3:IPT+3) = IDIGIT(10)
      ENDIF
      WRITE (LPT,2000) NEQN
 2000 FORMAT (13X,A12)
  100 RETURN
      END SUBROUTINE PRIPLN
!
!*****************************************************************************
!
      SUBROUTINE PRIVAR
!
! *** PRIVAR updated for MK4 by JCM Aug 89 ***
!
!X
!C 6C
!H Prints a list of basic variables, and constraint relations, for LSQ.
!P VARMAK must have been obeyed to create the lists
!O For most basic variables, lists their names.  For family 4, ("very long
!O      vectors") only prints ranges.
!O Then lists the strict constraints.
!O Any phases and sources are indicated by *P and *S
!
      INCLUDE 'PARAMS.INC'
      
      CHARACTER*1 IJOIN(8)
      CHARACTER*4 IPR1(8), IPR2(8), NAM1, NAM2, NOLD
      LOGICAL F4, F5
      DIMENSION K1(8), K2(8)
      COMMON /CONSTR/ JCONST, JROWPT(301), JCMAT(200), AMOUNT(200), NEXTJ

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB

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


      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)


      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      IF (LVARB.LE.0) THEN
        CALL MESS(LPT,1,'No variables')
        GOTO 100
      ENDIF
      WRITE (LPT,2001) LVARB
 2001 FORMAT (/'    ',I5,' basic variable(s) :'/)
! NUMBER OF ITEMS PER LINE OF OUTPUT:
      LINE = 7
      F4 = .FALSE.
      IGENF4 = 0
      ISP1F4 = 0
      ISPNF4 = 0
      IC = 0
      JPH = 0
      JSO = 0
      DO IB = 1, LVARB
        K = LVRPR(LBSVR(IB))
! SAVE PREVIOUS PHASE AND SOURCE:
        KPHASE = JPH
        KSOURC = JSO
        CALL KUNPAK(K,IFAM,IGEN,ISPC,JPH,JSO)
        IF (MULFAS .AND. KPHASE.NE.JPH) THEN
          WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
          IC = 0
          WRITE (LPT,2006) JPH
 2006     FORMAT (/' Phase',I3)
        ENDIF
        IF (IFAM.NE.4) GOTO 5
! FAMILY 4 - LONG VECTORS, INCONVENIENT TO PRINT:
! GET PRINTING NAME:
        NOLD = NAM2
        CALL PARNAM(NAM1,NAM2,3,K)
        IF (IGEN.EQ.IGENF4) GOTO 8
! NEW GENUS (INCLUDING START)
        IF (F4 .AND. ISP1F4.NE.ISPNF4) THEN
! PART-ENTRY ALREADY IN BUFFERS (BUT IF ONLY 1 IN RANGE, IS ALREADY THERE):
! PUT END VALUE OF RANGE INTO BUFF2:
          IPR2(IC) = NOLD
          IJOIN(IC) = '-'
        ENDIF
! SET "HAVE HAD SOME FAMILY 4"
        F4 = .TRUE.
! PUT GENUS NAME FOR FAMILY 4 INTO BUFFER BY ITSELF:
        IGENF4 = IGEN
        IF (IC.GE.LINE-1) THEN
          WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
          IC = 0
        ENDIF
        IC = IC + 1
        IPR1(IC) = ' '
        IPR2(IC) = NAM1
        IJOIN(IC) = ' '
! PUT LOWER END OF RANGE INTO BUFF:
   14   IF (IC.GE.LINE-1) THEN
          WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
          IC = 0
        ENDIF
        IC = IC + 1
        IPR1(IC) = NAM2
        IPR2(IC) = ' '
        IJOIN(IC) = ' '
        ISP1F4 = ISPC
        ISPNF4 = ISPC
        GOTO 4
! SAME GENUS - IS IT NEXT SPECIES?
    8   IF (ISPNF4+1.EQ.ISPC) THEN
          ISPNF4 = ISPC
          GOTO 4
        ENDIF
! PUT RANGE END INTO BUFFER:
        IF (ISP1F4.NE.ISPNF4) THEN
          IPR2(IC) = NOLD
          IJOIN(IC) = '-'
          IF (IC.GE.LINE) THEN
            WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
            IC = 0
          ENDIF
        ENDIF
        GOTO 14
! NOT FAMILY 4:
    5   IF (ISP1F4.NE.ISPNF4) THEN
          ISPNF4 = ISP1F4
          IPR2(IC) = NAM2
          IJOIN(IC) = '-'
        ENDIF
        IF (IC.GE.LINE) THEN
          WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
          IC = 0
        ENDIF
        IC = IC + 1
        CALL PARNAM(IPR1(IC),IPR2(IC),1,IB)
        IJOIN(IC) = ' '
        IF (IC.GE.LINE) THEN
          WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
          IC = 0
        ENDIF
    4 ENDDO
! MAY BE REMNANTS OF FAMILY 4 NOT IN BUFFER:
      IF (F4 .AND. (ISP1F4.NE.ISPNF4)) THEN
        IPR2(IC) = NAM2
        IJOIN(IC) = '-'
      ENDIF
      IF (IC.NE.0) WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
! CONSTRAINT LIST:
      IF (JCONST.LE.0) GOTO 100
      WRITE (LPT,2003) JCONST
 2003 FORMAT (/' ',I5,' constraint(s) - relations between shifts in variables are :'/)
      DO J = JCONST, 1, -1
        JROW = JROWPT(J)
        JNEXT = JROWPT(J+1) - 1
        JR = JNEXT - JROW + 1
        CALL PARNAM(NAM1,NAM2,2,LRDVR(J))
! KPHASE IS SET BY PARNAM:
        KP = KPHASE
        KS = KSOURC
        IF (JR.GT.8) THEN
          WRITE (LPT,3000) NAM1, NAM2
          JNEXT = JROW + 7
          JR = 8
        ENDIF
        F4 = .TRUE.
        F5 = .TRUE.
        DO M = 1, JR
          CALL PARNAM(IPR1(M),IPR2(M),1,JCMAT(M+JROW-1))
          K1(M) = KPHASE
          K2(M) = KSOURC
          IF (KPHASE.NE.KP) F4 = .FALSE.
          IF (KSOURC.NE.KS) F5 = .FALSE.
        ENDDO
        IF (JR.NE.1 .OR. ABS(AMOUNT(JROW)-1.).GT.0.00001) THEN
          IF (.NOT.MULFAS) WRITE (LPT,2004) NAM1, NAM2, AMOUNT(JROW),   &
     &                            IPR1(1), IPR2(1),                     &
     &                            ('+',AMOUNT(I+JROW-1),IPR1(I),IPR2(I),I=2,JR)
 2004     FORMAT (1X,A4,1X,A4,' = ',F10.3,' times ',A4,1X,A4,           &
     &            (1X,A1,1X,F10.3,' times ',A4,1X,A4))
          IF (MULFAS) WRITE (LPT,2008) NAM1, NAM2, KP, AMOUNT(JROW),    &
     &                                 IPR1(1), IPR2(1), K1(1),         &
     &                                 ('+',AMOUNT(I+JROW-1),IPR1(I),   &
     &                                 IPR2(I),K1(I),I=2,JR)
 2008     FORMAT (1X,A4,1X,A4,' *P',I1,' = ',F10.3,' times ',A4,1X,A4,  &
     &            ' *P',I1,(1X,A1,1X,F10.3,' times ',A4,1X,A4,' *P',I1))
        ELSE
          IF (MULFAS) WRITE (LPT,2007) NAM1, NAM2, KP, IPR1(1), IPR2(1), K1(1)
 2007     FORMAT (1X,A4,1X,A4,' *P',I1,' = ',A4,1X,A4,' *P',I1)
          IF (.NOT.MULFAS) WRITE (LPT,2005) NAM1, NAM2, IPR1(1), IPR2(1)
 2005     FORMAT (1X,A4,1X,A4,' = ',A4,1X,A4)
        ENDIF
      ENDDO
  100 RETURN
 2002 FORMAT (' ',10(A4,A1,A4,2X))
 3000 FORMAT (/' ERROR ** redundant variable',2(A4),'related to > 8 basics - cannot yet print')
      END SUBROUTINE PRIVAR
!
!*****************************************************************************
!
      SUBROUTINE PRIWRD(IFAM,IGEN,ISPC,NAME,MODE)
!
! *** PRIWRD updated for MK4 by JCM 8 May 90 ***
!
!X
!C 6C
!H Finds the name of the packed (possibly part) LSQ parameter from
!H the built-in table of parameter names.
!A On entry IFAM, IGEN, ISPC are family, genus and species of the parameter.
!A          MODE=0 requests left justify, =1 right justify.
!A On exit  NAME is the A4 name from the table in /WORDS/, or 'XXXX'
!P The table must have been set up by LSETUP
!
      CHARACTER*4 NAME
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

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

      COMMON /WDSPC / IWDNUM, IWDSPC(60)
      COMMON /WORDS / LSQWD(60)
      CHARACTER*4 LSQWD

      NAME = 'XXXX'
! THE PHASE IS IRRELEVANT - EVERY PHASE HAS THE SAME VOCABULARY HERE
! IF MULTISOURCE, AND FAMILY 3 THE SOURCE IS RELEVANT:
      IF (MULONE) THEN
        K = 0
        IF (IFAM.EQ.3) K = KSOURC
      ENDIF
      IPACK = KPAK(IFAM,IGEN,ISPC,0,K)
      IPT = NFIND(IPACK,IWDSPC,IWDNUM)
      IF (IPT.LE.0) RETURN
      IF (MODE.EQ.0) THEN
        NAME = LSQWD(IPT)
      ELSE
! RIGHT JUSTIFY:
        J = 4
        NAME = ' '
        DO I = 4, 1, -1
          IF (LSQWD(IPT)(I:I).NE.' ') THEN
            NAME(J:J) = LSQWD(IPT)(I:I)
            J = J - 1
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE PRIWRD
!
!*****************************************************************************
!
      SUBROUTINE PRMTIV
!
! *** PRMTIV by JCM 11 Oct 84 ***
!
!X
!C 3A
!H A specialist routine for use in generating h,k,l indices where the
!H natural stepping vectors do not define a primitive cell.
!P Called from SETGEN
!D Sets up coefficients NPRIM(2,2), MCOUNT(2), LFAC(2) in /HKLGEN/
!D to make primitive stepping vectors from the existing steps.
!
      DIMENSION VEC(3,2)
      COMMON /HKLGEN/ STEP(3,3), PT(3,3), VECEND(3,3), PRPT(3,3),       &
     &                NPRIM(2,2), NP, LFAC(2), MCOUNT(2), KOM5

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

!  CHECK WHETHER PRIMITIVE AND SET UP INTERVENING STEPS IF NOT:
      DO I = 1, 2
        DO J = 1, 2
          NPRIM(I,J) = 0
        ENDDO
        MCOUNT(I) = 1
        LFAC(I) = 1
      ENDDO
      IF (NP.EQ.1) GOTO 100
!  CHECK WHETHER BASE PLANE IS PRIMITIVE
      CALL VECPRD(STEP(1,1),STEP(1,2),VEC(1,1))
      CALL FCTOR(VEC(1,1),M)
      IF (M.EQ.1) GOTO 1
      CALL GMEQ(STEP(1,2),VEC(1,1),1,3)
      DO N1 = 2, M
        CALL GMADD(VEC(1,1),STEP(1,1),VEC(1,1),1,3)
        CALL FCTOR(VEC(1,1),J)
        IF (J.EQ.M) GOTO 11
      ENDDO
      WRITE (LPT,3000) M, J, (VEC(I,1),I=1,3), STEP
      CALL BMBOUT
      RETURN
   11 NPRIM(1,1) = N1 - 1
      NPRIM(2,1) = 1
      LFAC(1) = M
!  IS THIS ENOUGH?
      IF (M.EQ.NP) GOTO 100
!  INTERVENING LAYERS ON THIRD AXIS:
    1 L = NP/M
      CALL GMEQ(STEP(1,3),VEC(1,2),1,3)
      DO N2 = 1, L
        CALL GMEQ(VEC(1,2),VEC(1,1),1,3)
        DO N1 = 1, L
          CALL FCTOR(VEC(1,1),J)
          IF (J.EQ.L) GOTO 5
          CALL GMADD(VEC(1,1),STEP(1,1),VEC(1,1),1,3)
        ENDDO
        CALL GMADD(VEC(1,2),STEP(1,2),VEC(1,2),1,3)
      ENDDO
!  ERROR IF WE GET HERE
      WRITE (LPT,3001) L, M, J, ((VEC(I,II),I=1,3),II=1,2), STEP
      CALL BMBOUT
      RETURN
    5 NPRIM(1,2) = N1 - 1
      NPRIM(2,2) = N2 - 1
      LFAC(2) = L
  100 RETURN
 3000 FORMAT (/' ERROR ** IN PRMTIV FINDING BASE PLANE VECTOR - ',      &
     &        ' M,J=',2I4,' VEC1=',3F5.1,' STEP ARRAY IS'/1X,9F5.1)
 3001 FORMAT (/' ERROR ** in PRMTIV making second vector',' - L,M,J=',  &
     &        3I4/' Vectors so far are:',6F5.1/' Step array',' is',9F5.1)
      END SUBROUTINE PRMTIV
!
!*****************************************************************************
!
      LOGICAL FUNCTION PRNCYC(N)
!
! *** PRNCYC by JCM 17 Nov 84 ***
!
!X
!C 6C
!H Decides whether printing (of various different quantities in LSQ)
!H is needed during the current LSQ cycle.
!A On entry N is an integer specifying which member of the array IPRNT
!A            in /REFINE/ is to be consulted.  Values of N already
!A            assigned in some standard LSQ applications are:
!A N=1: PRIN - routine IICD1 (called by most LSQ applications) scans any
!A             I cards for a "PRIN" item;  it it finds one, it reads the
!A             integer which follows to IPRNT(1), and uses it as an
!A             indicator of the frequency with which general "obs and
!A             calc" lists are printed during the refinement.  These are:
!A     0 = no printing
!A     1 = print during first cycle
!A     2 = print during last cycle
!A     3 = print during first and last cycles
!A     4 = print every cycle
!A N=2: PRFC - similar to N=1, but used in the Profile Refinement system
!A             to control the printing of reflection information.
!A N=3: PRFO - as N=2, for a file to be reinput to the Fourier routines.
!A N=4: PRPR - as N=2, for a file containing the profile, to be reinput,
!A             e.g., to GENIE.
!A N=5: PRCV - as N=2, but only obeyed on last cycle.  The integer
!A             following "PRCV" is an indication of how many covariances
!A             from the inverse LSQ matrix to print on the .HKL file.
!A             So PRNCYC would be irrelevant here.
!A N=6: PREE - as N=2, for a file containing the eigenvalues and
!A             eigenvectors of that part of the inverse LSQ matrix
!A             relevant to intensities.  PRNCYC is again irrelevant,
!A             the integer here indicating how much material is sent to
!A             the line-printer file.
!A N=7: PRDM - read by IICD1, called by most LSQ applications.  Requests
!A             printing during the last cycle of h,k,l Fo Fc to a a
!A             .DEP file for "deposited Material".  This file may be
!A             subsequently interpreted by the main program DEPOS.
!A N=8: PRSK - read by GEOMCO, called by structure LSQ applications.
!A             Requests the printing of an "obs and calc" list for any
!A             slack constraints, including both geometrical constraints
!A             and those in Profile Refinement of Pawley-type.
!A
!A The user would be free to use higher values of N, e.g. from 20 downwards,
!A for his own print control.  An example of such use occurs in the main
!A program GRLSQ, in which N=2 is used in a different context from the above.
!
!P IPRNT(N) in /REFINE/ must contain an integer with a value in the
!P range 0-4, as described above.  ICYC in /REFINE/ must be the current
!P cycle number.
!
!D The function PRNCYC is set TRUE if printing is required, taking
!D of both IPRNT(N) and ICYC.
!
      LOGICAL FIRST, LAST
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      IP = IPRNT(N)
! IP=0 MEANS NO PRINTING:
      PRNCYC = .FALSE.
      IF (IP .GT. 0) THEN
        FIRST = (ICYC.EQ.NCYC1)
! DONE IS SET IF REFINEMENT HAS CONVERGED ACCORDING TO I CONV CARD:
        LAST = (ICYC.EQ.LASTCY) .OR. DONE
        PRNCYC = (IP.EQ.4) .OR. ((IP.EQ.3) .AND. (LAST.OR.FIRST)) .OR.  &
     &           ((IP.EQ.2) .AND. LAST) .OR. ((IP.EQ.1) .AND. (FIRST))
      ENDIF

      END FUNCTION PRNCYC
!
!*****************************************************************************
!
      FUNCTION ARANGE(X,A,B)
!
! *** RANGE by JCM 23 Sep 87 ***
!
!X
!C 11C
!H Puts a number into a given range.
!A On entry X is the current value of a real variable.
!A          A and B are the limits between which RANGE is to be set.
!D X must be periodic in (A-B).  A is included in the range, but B excluded.
!N A is not necessarily > or < B.
!
      P = ABS(A-B)
      IF (A.GT.B) THEN
        ALOW = B
        AUP = A
      ELSE
        ALOW = A
        AUP = B
      ENDIF
      R = X
    1 IF (R.LE.AUP) GOTO 2
      R = R - P
      GOTO 1
    2 IF (R.GE.ALOW) GOTO 101
      R = R + P
      GOTO 2
  101 IF (R.EQ.B) R = A
      ARANGE = R

      END FUNCTION ARANGE
!
!*****************************************************************************
!
      SUBROUTINE RDANGL(IPT,N1,N2,N3,NCOM,IE)
!
! *** RDANGL by JCM 15 Oct 90 ***
!
!X
!C 8A
!H Reads a specification of a bond angle, by reading the names of 2 intersecting
!H bonds;  makes the third bond involved.
!A On entry IPT is the position at which to start reading, and it is updated.
!A On exit N1, N2 and N3 specify the 3 bonds making up the triangle containing
!A         the angle.  The angle is opposite N1, enclosed by N2 and N3,
!A         with N2 < N3.
!A         NCOM points to the common atom.
!A         IE is an error indicator - IE=0 if OK, not 0 if error.
!
!D Reads and identifies 2 bond names;  finds their common atom; makes the
!D third bond of the triangle and adds it to the bond list.
!
!O Reports an error if bonds are not in list, or if bonds have no atom in
!O common.
!
      CHARACTER*4 BNAME
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      IE = 0
! READ FIRST BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N2 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N2.LE.0) GOTO 11
! READ SECOND BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N3 = NCFIND(BNAME,BONNAM,NUMBON)
      IF (N3.GT.0) GOTO 12
   11 CALL ERRCH2(BNAME,2,' ','is not a bond name')
      IE = 1
      GOTO 100
! FIND THIRD SIDE:
   12 CALL BONTRI(N2,N3,N1,NCOM,IE)
  100 RETURN
      END SUBROUTINE RDANGL
!
!*****************************************************************************
!
      SUBROUTINE RDATOM(IPT,IA,XACT,ISYMM,ILATT,CS)
!
! *** RDATOM updated by JCM 7 Sep 90 ***
!
!X
!C 8A
!H Reads the specification of an atom for slack constraint purposes
!A On entry IPT points to the first character to read
!A On exit IA holds which base atom the new one is related to
!A         XACT holds the actual x,y,z coordinates
!A         ISYMM, ILATT and CS hold the transformation which takes the
!A                   base atom into the actual coordinates
!P On entry ICARD in /SCRACH holds the card read
!D Reads an atom name, which must be the same as one on an A card.
!D This may be followed either by 3 coordinates, x,y,z or by a
!D symmetry operator number, a lattice translation number, and 3
!D cell shifts.
!D
!D In either case, the actual destination atom, related to that on the
!D A card, is identified.
!
      DIMENSION XACT(3), CS(3)
      CHARACTER*4 NAME
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC

! READ NAME OF BASE ATOM:
      CALL RDWORD(NAME,LEN,IPT,IPT,80,0,IER)
      IA = IATOM(NAME)
      IF (IA.LE.0) THEN
        CALL ERRATM(NAME,1,'on L ATOM card')
        GOTO 100
      ENDIF
! READ SYMMETRY NUMBER (POSSIBLY -), LATTICE NUMBER AND 3 CELL TRANSLATIONS:
      IPKEEP = IPT
      ILATT = 1
      DO I = 1, 3
        CS(I) = 0.0
      ENDDO
      CALL RDINTG(ISYMM,IPT,IPT,80,IER)
! MAY HAVE READ INTEGER, NOTHING OR DECIMAL NUMBER FOR X COORD:
      IF (IER.NE.-1) GOTO 10
! INSTEAD OF AN INTEGER WE HAVE TRIED TO READ A REAL.  X Y AND Z GIVEN.
      IPT = IPKEEP
      DO I = 1, 3
        CALL RDREAL(XACT(I),IPT,IPT,80,IER)
      ENDDO
      CALL XROOT(IA,XACT,ISYMM,ILATT,CS)
      IF (ISYMM.NE.0) GOTO 2
      WRITE (LPT,3009) ATNAME(IA), XACT
      GOTO 99
! MAY BE SIMPLE LABEL FOR ATOM AS ON A CARD, OR PART SPEC:
! IF INTEGER READ IT IS SPECIFICATION FOR SYMMETRY OPERATOR:
   10 IF (IER.EQ.100 .OR. ISYMM.EQ.0) THEN
        ISYMM = 1
        GOTO 2
      ENDIF
! CHECK SYMMETRY OPERATOR NUMBER:
      IF (IABS(ISYMM).GT.NOPC) THEN
        WRITE (LPT,3003) ISYMM, NOPC
        GOTO 99
      ENDIF
      IF (ISYMM.LT.0 .AND. .NOT.CENTRC) THEN
        CALL ERRMES(1,-1,'-ve symmetry operator in non-centric group')
        GOTO 99
      ENDIF
! READ AND CHECK LATTICE NUMBER (ALLOWED TO BE MISSING):
      CALL RDINTG(ILATT,IPT,IPT,80,IER)
      IF (IER.EQ.100 .OR. ILATT.EQ.0) GOTO 2
!
      IF (ILATT.GT.NLAT .OR. ILATT.LE.0) THEN
        WRITE (LPT,3005) ILATT, NLAT
        GOTO 99
      ENDIF
! READ AND CHECK CELL TRANSLATIONS:
      DO K = 1, 3
        CALL RDREAL(CS(K),IPT,IPT,80,IER)
        IF (IER.NE.0) GOTO 2
      ENDDO
! MAKE ACTUAL COORDS FOR BOTH POSITIONS:
    2 CALL XTRANS(IA,XACT,ISYMM,ILATT,CS)
      GOTO 100
   99 IERR = IERR + 1
  100 RETURN
 3009 FORMAT (/' ERROR ** ',A4,' WILL NOT TRANSFORM INTO ',3F10.4)
 3003 FORMAT (/' ERROR ** symmetry operator number',I4,' requested but only',I4,' present')
 3005 FORMAT (/' ERROR ** lattice translation number',I4,' requested but only',I4,' present')

      END SUBROUTINE RDATOM
!
!*****************************************************************************
!
      SUBROUTINE RDBOND(IPT,NEND,IE)
!
! *** RDBOND by JCM 15 Oct 1990 ***
!
!X
!C 8A
!H Reads a specification of a bond, by reading the names of the atoms at each
!H end.
!A On entry IPT is a starting position on the card.
!A On exit  NEND is an integer array of 2 elements which holds the numbers
!A             of the ends so identified in the list of L ATOM cards.
!A          IE is an error indicator - on exit IE=0 if OK, not 0 if error.
!
!D Given an L BOND card in /SCRACH/, reads the next 2 atom names.  An atom
!D may belong to the list already given on L ATOM cards, or it may be the
!D name of an atom in the asymmetric unit, on an A card.  In the latter case
!D it is added to the L ATOM list.
!
!O Reports an error if the atom names occur in neither list.
!
      CHARACTER*4 NAME
      DIMENSION NEND(2), CZ(3)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /SLKGEC/ ATTNAM(500), BONNAM(500), ANGNAM(100), TORNAM(100)
      CHARACTER*4 ATTNAM, BONNAM, ANGNAM, TORNAM
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY

      IE = 0
! READ 2 ATOM NAMES:
      DO J = 1, 2
        CALL RDWORD(NAME,LEN,IPT,IPT,80,0,IER)
        NEND(J) = 0
        IF (NTARNM.GT.0) NEND(J) = NCFIND(NAME,ATTNAM,NTARNM)
        IF (NEND(J).GT.0) GOTO 1
        N = IATOM(NAME)
        IF (N.EQ.0) THEN
          CALL ERRATM(NAME,2,'L BOND card')
          IE = 1
          GOTO 100
        ENDIF
! NAME OFF A CARD USED - ADD IT TO TARGET TABLE:
        CALL GMZER(CZ,1,3)
        CALL ADDATM(ATNAME(N),N,X(1,N),1,1,CZ,NEND(J))
    1 ENDDO
  100 RETURN

      END SUBROUTINE RDBOND
!
!*****************************************************************************
!
      SUBROUTINE RDDATA(NUNIT,K,H,F,IN,IOU)
!
! *** RDDATA by JCM 5 Apr 89 ***
!
!X
!C 13C
!H Reads in free format h,k,l (possibly floating) and a list of values,
!H allowing for a possible title.
!A On entry NUNIT is the unit from which to read
!A          IN indicates other incoming information:
!A          IN +ve means allow one line of text (recognised by containing
!A             any letter other than E)
!A          IN -ve means expect only numbers - this entry would be used for
!A             reading lines of a files other than the first.
!A          IN absolute value gives maximum number of numbers to read to F
!A On exit H(1:3) contains the first 3 numbers read, real
!A         K(1:3) contains the same numbers, fixed to integers.
!A         F() contains all the subsequent numbers on the line, reals.
!A             (maximum abs(IN) of them)
!A         IOU, absolute value, holds (as its units digit) the number of
!A               numbers read to the array F.  No array bound check is done
!A               on F at present.
!A         IOU is -9999 if end of file reached, so nothing new read
!A         IOU, sign, is +ve if no errors detected
!A                         -ve if some reading error occurred, or title
!A                            out of context.
!A         IOU, absolute value, has 100 added if title read.  In this case
!A                         the routine reads in the next line also.
!
!D Ignores empty lines.  If IN > 0 and finds text line, adopts it as a title
!
      DIMENSION H(3), K(3), F(1)
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IOU = -9999
      READTI = 0
    4 READ (NUNIT,1000,END=100) ICARD
 1000 FORMAT (A80)
      IPT = 1
      IOU = 1
      L = LENGT(ICARD)
      IF (L.EQ.0) GOTO 4
      DO I = L, 1, -1
        M = LETTER(ICARD(I:I))
        IF (M.EQ.5) GOTO 3
        IF (M.GT.0) THEN
! ASSUME TITLE GIVEN:
          IF (IN.LT.0 .OR. READTI.EQ.1) THEN
            CALL ERRMES(1,0,'letters on data file')
            IF (IBMBER .NE. 0) RETURN
          ENDIF
          CALL INPUTN(-1)
          READTI = 1
          GOTO 4
        ENDIF
    3 ENDDO
      DO I = 1, 3
        CALL RDREAL(H(I),IPT,IPT,80,IER)
      ENDDO
      CALL INDFIX(H,K)
! READ AS MANY NUMBERS AS REMAIN:
      I = 0
    1 I = I + 1
      IF (I.GT.IABS(IN)) GOTO 5
! READ FIRST IN CASE EMPTY:
      CALL RDREAL(A,IPT,IPT,80,IER)
      IF (IER.EQ.0) THEN
        F(I) = A
        GOTO 1
      ENDIF
      IF (IER.NE.100) IOU = -1
    5 IOU = IOU*(I-1+100*READTI)
  100 RETURN

      END SUBROUTINE RDDATA
!
!*****************************************************************************
!
      SUBROUTINE RDFV
!
! *** RDFV updated by JCM 4 Jul 88 ***
!
!X
!C 6A
!H Reads all the user's L FIX and L VARY cards in sequence.
!
!D The cards read here start either L FIX or L VARY.  Next come as many
!D parameter specifications as the user wishes.  A parameter specification
!D in this context is one of:
!
!D   ONLY
!D   ALL <F>                       where <F> is a family name
!D   ALL <G>                       where <G> is genus name
!D   ALL <S>                       where <S> is species name.
!D   ALL <W>                       where <W> is a word for a
!D                                  number of species as set up by the
!D                                  main program, e.g. "XYZ".
!D   <S>
!D   <G> <S>
!D    <G> <W>
!D
!D Examples:
!D L FIX ONLY SCAL 1  TFAC
!D L VARY ALL SITE
!D L VARY NA1 X  NA2 XYZ  O1 ITF
!D L FIX ALL BIJ
!D L VARY ONLY C1 B11   C1 B22   C1 B33   C2 XYZT  ALL C3
!D L VARY ALL FAM1
!D
!D In identifying a parameter, RDFV then stores the instruction to fix
!D or vary it accordingly.  Such instructions are not actually certain
!D to be used until the routine VARMAK is called, which surveys them all
!D and may discard some in favour of others.
!
!N It should be noted that even in this "general" routine used by all
!N LSQ programs, family 1 genus 1 is special, in having no genus name
!N (so that it includes items with single names, e.g. "TFAC").  Also
!N family 2 is special, being the crystal structure parameters.
!N When an atom name is found, it is taken to be a genus name in family 2.
!
!I Reads in L FIX and L VARY cards.
!
      CHARACTER*4 NFV(2)
      LOGICAL FX
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LINKAG/ NUMFV, NUMPAK, KKFV(200), KTYPFV(200), KSTFV(200),&
     &                KTIME(200), KUNPFV(5,30), NTIME, NUMCON,          &
     &                KKCON(500), AMCON(500), KPTCON(201), KSTCON(200), &
     &                KTPCON(200)

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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      DATA NFV/'FIX', 'VARY'/

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! COUNT CARDS - MAY BE NONE:
      IN = 0
! LOOK FOR EITHER L FIX OR L VARY:
    8 CALL CDSCAN('L',NFV,2,IN,L,NW)
      IF (L.LE.0) GOTO 100
      FX = (NW.EQ.1)
      IN = L
      IPT = 7
! READ NEXT PARAMETER SPECIFICATION ON CARD:
    7 CALL PARRD(IPT,IPT,KK,IFAM,IGEN,ISPC)
! KK ZERO MEANS NO MORE ON THIS CARD:
      IF (KK.EQ.0) GOTO 8
! KK POSITIVE IS A PARAMETER SPEC.  ALL OF IFAM,IGEN AND ISPC SHOULD BE
! NON-ZERO.  ANY MORE COMPLICATED SPECS (LIKE 'ALL X' OR 'ALL FAM1') HAVE COME
! OUT WITH KK NEGATIVE.
      IF (KK.LT.0) GOTO 1
! WE HAVE WHOLE SPEC - FIX OR VARY THE PARAMETER AS REQUIRED:
      CALL FVKPAK(KK,4,FX)
! IF VARY, NOW DO AND ALL WHO SAIL IN HER:
      IF (FX) GOTO 7
      DO I = 1, NUMCON
        IF (KPTCON(I+1)-KPTCON(I).NE.2) GOTO 4
        KK1 = KKCON(KPTCON(I))
        KK2 = KKCON(KPTCON(I)+1)
        IF (KK2.EQ.KK) GOTO 15
        KK2 = KKCON(KPTCON(I))
        KK1 = KKCON(KPTCON(I)+1)
        IF (KK2.NE.KK) GOTO 4
! VARIED PAR IS SIMPLY RELATED TO ANOTHER: VARY THAT TOO
   15   CALL FVKPAK(KK1,4,FX)
    4 ENDDO
      GOTO 7
! KK LARGE & -VE MEANS WORDS  LIKE ONLY, ALL, XYZ BIJ - BRANCH:
    1 I100 = -KK - 98
! 1=ONLY  2=ALL  3=XYZ IN SFLSQ ETC, 4=BIJ DITTO, 5=XYZT DITTO
      GOTO (11,12,13,14,13,13,13), I100
! UNFORSEEN VALUE:
      CALL ERRMES(1,0,'unforseen word in RDFV')
      IF (IBMBER .NE. 0) RETURN
! 'ONLY'
   11 IF (IONLY(JPHASE).NE.0) GOTO 7
      IONLY(JPHASE) = 1
      IF (.NOT.FX) IONLY(JPHASE) = 2
      GOTO 7
! 'ALL'
   12 IF (IFAM.EQ.0) THEN
        WRITE (LPT,3000) KK, IFAM, IGEN, ISPC
        CALL BMBOUT
        RETURN
      ENDIF
! -VE IFAM MEANS "ALL <W>" READ, LIKE ALL BIJ:
      IF (IFAM.GT.0) GOTO 29
      I1 = IABS(IFAM)
      GOTO (21,22,21,24,25,21), I1
      CALL ERRMES(1,0,'unforseen word after ALL in RDFV')
      IF (IBMBER .NE. 0) RETURN
! ALL XYZB:
   25 L1 = 1
      GOTO 19
! ALL XYZ OR ALL XYZT OR ALL XYZS:
   21 DO J = 1, 3
        CALL FIXVAR(FX,2,0,J,KPHASE,KSOURC,4)
      ENDDO
      IF (I1.EQ.3) CALL FIXVAR(FX,2,0,12,KPHASE,KSOURC,4)
      IF (I1.EQ.6) CALL FIXVAR(FX,2,0,11,KPHASE,KSOURC,4)
      GOTO 7
! ALL BIJ:
   22 L1 = 4
   19 DO J = L1, 9
        CALL FIXVAR(FX,2,0,J,KPHASE,KSOURC,4)
      ENDDO
      GOTO 7
! ALL CELL:
   24 DO J = 1, 6
        CALL FIXVAR(FX,NCELF,NCELG,NCELS+J-1,KPHASE,KSOURC,4)
      ENDDO
      GOTO 7
   29 IF (ISPC.NE.0) GOTO 3
      IF (IGEN.NE.0) GOTO 10
! FIX ALL OF FAMILY:
      CALL FIXVAR(FX,IFAM,0,0,KPHASE,KSOURC,4)
      GOTO 7
! OPERATE ON ALL SPECIES OF GIVEN GENUS:
   10 CALL FIXVAR(FX,IFAM,IGEN,0,KPHASE,KSOURC,4)
      GOTO 7
! KNOWN FAMILY, UNKNOWN GENUS:
    3 CALL FIXVAR(FX,IFAM,0,ISPC,KPHASE,KSOURC,4)
      GOTO 7
! <G> XYZ OR <G> XYZT :
   13 L1 = 3
      IF (I100.EQ.6) L1 = 9
      DO I = 1, L1
        CALL FIXVAR(FX,IFAM,IGEN,I,KPHASE,KSOURC,4)
      ENDDO
      IF (I100.EQ.5) CALL FIXVAR(FX,IFAM,IGEN,12,KPHASE,KSOURC,4)
      IF (I100.EQ.7) CALL FIXVAR(FX,IFAM,IGEN,11,KPHASE,KSOURC,4)
      GOTO 7
! <G> BIJ:
   14 DO I = 4, 9
        CALL FIXVAR(FX,IFAM,IGEN,I,KPHASE,KSOURC,4)
      ENDDO
      GOTO 7
  100 RETURN
 3000 FORMAT (/' *** PROGRAM ERROR in RDFV ** ''ALL'' read, but',' KK,IFAM,IGEN,ISPC=',4I5)

      END SUBROUTINE RDFV
!
!*****************************************************************************
!
      SUBROUTINE RDINTG(N,IPT1,IPT2,IPTEND,IER)
!
! *** RDINTG by JCM 10 Oct 83 ***
!
!X
!C 13C
!H Reads an integer in free format from a character string.
!A On entry IPT1 points to the first character in the string to consider
!A          IPTEND points to the last to be considered
!A On exit  N is the integer which was read
!A          IPT2 points to the first character after the terminating
!A               character, unless there was an erroneous character,
!A               in which case IPT2 points to it.
!D          IER = 0 if no errors were found
!D              = 100 if N=0 was derived from all spaces (so note IER
!D                   non-zero is not always indicative of a fatal error)
!D              = number in range 1-52 if a letter was found (the value of
!D               IER indicates which letter)
!D              = small -ve number (being an address in table ISMBOL in
!D               COMMON /CHARS/) if a symbol was found out of context.
!D              =-100 if the character found did not occur in any table
!P Before entry the character string from which the integer is to be read
!P is read into /SCRACH/
!D The string is expected to contain only digits, a possible sign,
!D and a space or a comma to terminate.(The number is also terminated
!D after the character pointed to by IPTEND.)
!
      CHARACTER*1 IC
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      IPT = IPT1
      IER = 0
      N = 0
      ISIG = 0
! ISIG SAYS WHETHER ANYTHING SIGNIFICANT YET READ, THEN HOLDS SIGN
    1 IF (IPT.GT.IPTEND) GOTO 101
      IC = ICARD(IPT:IPT)
! IGNORE INITIAL SPACES;  TERMINATE ON FINAL SPACE:
      IF ((IC.EQ.' ') .AND. (ISIG.EQ.0)) GOTO 3
      IF ((IC.EQ.' ') .AND. (ISIG.NE.0)) GOTO 102
! TEST FOR DIGIT FIRST, AS MOST LIKELY CHARACTER TO OCCUR:
      N1 = NDIGIT(IC)
      IF (N1.LT.0) GOTO 2
! DIGIT:
      IF (ISIG.EQ.0) ISIG = 1
      N = N*10 + N1
! THIS OVERFLOWS IF TOO BIG A NUMBER IS GIVEN
      GOTO 3
! NEITHER DIGIT NOR SPACE:
    2 N2 = NSYMBL(IC)
! JUMP IF SYMBOL (NOT LETTER):
      IF (N2.GT.0) GOTO 6
! IF HERE, ERROR, PROBABLY LETTER:
      L = LETTER(IC)
      IF (L.GT.0) IER = L
      IF (L.EQ.0) IER = -100
      GOTO 101
! SYMBOL: + AND - EXPECTED AT START;  COMMA COULD TERMINATE
    6 IF (N2.EQ.2) GOTO 102
      IF ((N2.EQ.14) .AND. (ISIG.EQ.0)) GOTO 8
      IF ((N2.EQ.15) .AND. (ISIG.EQ.0)) GOTO 3
      IER = -N2
      GOTO 101
! MINUS:
    8 ISIG = -1
! NEXT CHARACTER:
    3 IPT = IPT + 1
      GOTO 1
  102 IPT = IPT + 1
  101 N = N*ISIG
      IPT2 = IPT
      IF ((ISIG.EQ.0) .AND. (IER.EQ.0)) IER = 100

      END SUBROUTINE RDINTG
!
!*****************************************************************************
!
      SUBROUTINE RDNUMS(A,IPT1,NBOUND,NUM,IER)
!
! *** RDNUMS corrected by JCM 23 Apr 92 ***
!
!X
!C 13C
!H Reads all the numbers on a line in free format.
!A On entry IPT1 points to the first character in the string to be considered
!A On exit NUM is the number of numbers read
!A         A(1:NBOUND) is an array into which NUM numbers have been read.
!A         IER = 1 if any non-reals read, except "STEP", (and stops reading)
!A         IER = 2 if more than NBOUND numbers were read
!A         IER = 3 if a zero STEP length is requested
!A         IER = 4 if a negative number of STEPS is requested
!
!P Before entry the character string (maximum 80 characters) from which
!P the numbers are to be read must be in the character array in /SCRACH/.
!
!D The character string may contain just a simple string of numbers or
!D may have anywhere the word STEP followed by 3 numbers.  This is treated
!D like a FORTRAN "DO" loop:   the first number is an initial value
!D                             the second number is a final value
!D                             the third number is a step length
!D All the values implied by the "STEP" function are put into array A
!D The "STEP" length may be negative so long as the final value is less than
!D the initial one.
!
!D Stops when it has only blank card left, so it reads any non-blank
!D numbers no matter where they are on the card.  It cannot be used for
!D the (little used) facility which allows fixed format in which blanks
!D mean zeros.
!
!N Beware rounding error in the use of "STEP".
!
! STOPS WHEN IT HAS ONLY BLANK CARD LEFT, SO IT READS ANY NON-BLANK NUMBERS NO
! MATTER WHERE THEY ARE ON THE CARD, & IS NO USE FOR "C" CARDS.
!
!
      CHARACTER*4 IWORD
      LOGICAL TESTOV
      DIMENSION A(NBOUND)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      IPT = IPT1
      IER = 0
      NUM = 0
    1 CALL RDREAL(X,IPT,IPT,80,IE)
      IF ((IE.EQ.100) .OR. IPT.GT.80) GOTO 100
      IF (IE.EQ.0) GOTO 2
! IF HERE, IT IS EITHER "STEP" OR AN ERROR:
      IF (IE.NE.19) GOTO 4
      IP = IPT
      CALL RDWORD(IWORD,IWDLEN,IP,IPT,IP+3,0,IE)
      IF (IWORD.NE.'STEP') GOTO 4
      CALL RDREAL(X,IPT,IPT,80,IE)
      IF (IE.NE.0) GOTO 4
      CALL RDREAL(Y,IPT,IPT,80,IE)
      IF (IE.NE.0) GOTO 4
      CALL RDREAL(Z,IPT,IPT,80,IE)
      IF (IE.NE.0) GOTO 4
      IF (Z.GT.0.) GOTO 7
      Z = -Z
      X1 = X
      X = Y
      Y = X1
    7 IF (.NOT.TESTOV(Y,Z)) GOTO 6
      WRITE (LPT,3002) X, Y, Z
      IER = 3
      GOTO 100
    6 NPT = NINT((Y-X+10.E-5)/Z) + 1
      IF (NPT.GE.1) GOTO 8
      WRITE (LPT,3003) X, Y, Z
      IER = 4
      GOTO 100
    8 N1 = NUM + NPT
      IF (N1.GT.NBOUND) GOTO 5
      DO I = 1, NPT
        A(NUM+I) = X + FLOAT(I-1)*Z
      ENDDO
      NUM = N1
      GOTO 1
! CANNOT MAKE SENSE OF CARD:
    4 IER = 1
      CALL ERRIN2(IPT,2,'cannot interpret card at point','expecting real number or "STEP"')
      GOTO 100
    2 NUM = NUM + 1
      IF (NUM.LE.NBOUND) GOTO 3
    5 CALL ERRIN2(NBOUND,2,'more than','numbers read by RDNUMS')
      IER = 2
      GOTO 100
    3 A(NUM) = X
      GOTO 1
  100 RETURN
 3002 FORMAT (' ERROR ** zero step length from',3F10.4)
 3003 FORMAT (' ERROR ** step',3F10.4,' gives -ve number of points')
      END SUBROUTINE RDNUMS
!
!*****************************************************************************
!
      SUBROUTINE RDREAL(X,IPT1,IPT2,IPTEND,IER)
!
! *** RDREAL updated by JCM 11 Sep 91 ***
!
!X
!C 13C
!H Reads a real number in free format from a character string.
!A On entry IPT1 points to the first character in the string to consider
!A          IPTEND points to the last to be considered
!A On exit  X is the real number read,
!A          IPT2 points to the first character after the terminating character
!A               unless there was an erroneous character, in which case
!A               IPT2 points to it.
!A          IER =   0 if no errors were found
!A              = 100 if X=0 was derived from all spaces (so note IER
!A                    non-zero is not always indicative of a fatal error)
!A              = number in range 1-52 if a letter was found (the value of
!A                IER indicates which letter)
!A              = small -ve number (being an address in table ISMBOL in
!A                COMMON /CHARS/) if a symbol was found out of context.
!A              =-100 if the character found did not occur in any table
!A              =-101 if number of form N/M but M=0
!A              =-102 if more than 8 digits after decimal point
!P Before entry the character string (maximum 80 characters) from which
!P the number is to be read must be in the character array ICARD in /SCRACH/.
!D The string is expected to contain only digits, a possible sign,
!D a possible decimal point and a space or a comma to terminate.(The
!D number is also terminated after the character pointed to by IPTEND.)
!D A comma by itself will be read as 0. (So, e.g., on C cards, tetragonal
!D may come as 4.3,,4.5)
!D The routine will also read numbers in the form M/N (e.g. "2/3"),
!D and integers without the decimal point.
!
      CHARACTER*1 IC
      DOUBLE PRECISION TENTBL, XX, YY
      DIMENSION TENTBL(8)
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA TENTBL/0.1D0, 0.01D0, 0.001D0, 0.0001D0, 0.00001D0, 0.000001D0, 0.0000001D0, 0.00000001D0/

      IPT = IPT1
      IER = 0
      XX = 0.0
      ISIG = 0
      SIG = 1.0
! ISIG SAYS WHETHER ANYTHING SIGNIFICANT YET READ, SIG HOLDS SIGN
      IPSH = 0
! IPSH SAYS WHETHER POINT OR SLASH READ
    1 IF (IPT.GT.IPTEND) GOTO 5
      IC = ICARD(IPT:IPT)
! IGNORE INITIAL SPACES;  TERMINATE ON FINAL SPACE:
      IF ((IC.EQ.' ') .AND. (ISIG.EQ.0)) GOTO 3
      IF ((IC.EQ.' ') .AND. (ISIG.NE.0)) GOTO 15
! TEST FOR DIGIT FIRST, AS MOST LIKELY CHARACTER TO OCCUR:
      N = NDIGIT(IC)
      IF (N.LT.0) GOTO 2
! DIGIT:
      XX = 10.*XX + FLOAT(N)
      GOTO 17
! NEITHER DIGIT NOR SPACE:
    2 N = NSYMBL(IC)
! JUMP IF SYMBOL (NOT LETTER):
      IF (N.GT.0) GOTO 6
! IF HERE, ERROR, PROBABLY LETTER:
      L = LETTER(IC)
      IF (L.GT.0) IER = L
      IF (L.EQ.0) IER = -100
      GOTO 5
! SYMBOL: + AND - EXPECTED AT START;  COMMA COULD TERMINATE
! COULD ALSO BE DECIMAL POINT OR SLASH:
    6 IF (N.EQ.2) GOTO 15
      IF ((N.EQ.14) .AND. (ISIG.EQ.0)) GOTO 8
      IF ((N.EQ.15) .AND. (ISIG.EQ.0)) GOTO 17
      IF ((N.EQ.1) .AND. (IPSH.EQ.0)) GOTO 7
      IF ((N.EQ.10) .AND. (IPSH.EQ.0)) GOTO 9
      IER = -N
      GOTO 5
! DECIMAL POINT:
    7 IPSH = 1
! NOW READ FRACTION:
      GOTO 10
! SLASH:
    9 IPSH = -1
! NOW READ DENOMINATOR:
      GOTO 10
! MINUS:
    8 SIG = -1.
! SET "SOMETHING OTHER THAN SPACE READ"
   17 ISIG = 1
! NEXT CHARACTER:
    3 IPT = IPT + 1
      GOTO 1
! READ EITHER FRACTIONAL PART OR DENOMINATOR:
   10 YY = 0.
      ISIG = 1
      IT = 0
   11 IPT = IPT + 1
      IF (IPT.GT.IPTEND) GOTO 5
      IC = ICARD(IPT:IPT)
! FIRST LOOK FOR DIGITS:
      N = NDIGIT(IC)
      IF (N.LT.0) GOTO 12
! DIGIT:
      IT = IT + 1
      IF (IT.LE.8) GOTO 13
      IER = -102
      GOTO 5
   13 YY = FLOAT(N)*TENTBL(IT) + YY
      GOTO 11
! SPACE TERMINATES:
   12 IF (IC.EQ.' ') GOTO 15
! OTHERWISE, COMMA TERMINATES AND EVERYTHING ELSE ERROR:
      NS = NSYMBL(IC)
      IF (NS.EQ.2) GOTO 15
      L = LETTER(IC)
      IF (NS.GT.0) IER = -NS
      IF (L.GT.0) IER = L
      IF (IER.EQ.0) IER = -100
      GOTO 5
! END - XX HAS LEFT HAND PART,  POSSIBLY YY HAS RIGHT HAND PART:
   15 IPT = IPT + 1
    5 IF (IPSH.EQ.1) XX = XX + YY
      IF (IPSH.NE.-1) GOTO 101
! DEAL WITH N/M:
      IF (YY.GT.0.) GOTO 16
      IF (IER.EQ.0) IER = -101
      GOTO 101
   16 XX = XX/(YY*10.**IT)
  101 X = SNGL(XX)*SIG
      IPT2 = IPT
      IF ((ISIG.EQ.0) .AND. (IER.EQ.0) .AND. (IC.NE.',')) IER = 100

      END SUBROUTINE RDREAL
!
!*****************************************************************************
!
      SUBROUTINE RDRELA
!
! *** RDRELA updated JCM 13 Jan 88 ***
!
!X
!C 6A
!H Reads and interprets all user-supplied L RELA cards for constraints.
!
!D Deals with L RELA cards of type:
!D   L RELA 1  : followed by <a1> <p1>  <a2> <p2> where a1, a2 are
!D               constants and p1, p2 are parameter specifications.
!D               This is to be interpreted as:
!D               a1 x shift in p1 = a2 x shift in p2
!D
!D   L RELA 2  : followed by a string of <a1> <p1>  <a2> <p2>  ,a3> <p3> . .
!D               for as many as are needed.  This is to be interpreted as:
!D    a1 x shift in p1 + a2 x shift in p2 + a3 x shift in p3 + etc = 0.
!D
!D For each card the relation is read and stored.  It is not actually
!D absorbed until routine VARMAK is obeyed, when it may be modified in
!D the light of other FIX, VARY or RELA instructions.
!
!N Note that types 1 and 2 are not identical for a relation involving just
!N two parameters.
!
      DIMENSION AM(10), KK1(10)

! SET NO TYPE 2 CONSTRAINTS:
!
! READ ALL 'L RELA' CARDS:
      IN = 0
    1 CALL FINDCD('L','RELA',4,IN,L)
      IF (L.LE.0) GOTO 100
! INTERPRET NEXT CARD:
      IN = L
      CALL RDINTG(IRTYP,7,IPT,80,IER)
      IF (IRTYP.LE.0 .OR. IRTYP.GE.3) THEN
        CALL ERRIN2(IRTYP,2,'Relation type','not implemented')
        GOTO 1
      ENDIF
      GOTO (11,12), IRTYP
! TYPE 1 CONSTRAINT - A1 TIMES P1 = A2 TIMES P2
   11 CALL RDREAL(AM(1),IPT,IPT,80,IER)
      CALL PARRD(IPT,IPT,KK1(1),IFAM1,IGEN1,ISPC1)
! PARAMETER SPEC INTO KK1(1) - MUST BE A GENUS NAME+SPECIES NAME SO KK1(1) +VE:
      IF (KK1(1).LE.0) GOTO 99
      CALL RDREAL(AM(2),IPT,IPT,80,IER)
      CALL PARRD(IPT,IPT,KK1(2),IFAM2,IGEN2,ISPC2)
      IF (KK1(2).LE.0) GOTO 99
      AM(2) = -AM(2)
      CALL ADDCON(2,KK1,AM,1)
      GOTO 1
! TYPE 2 - A LINEAR COMBINATION OF PARAMETERS, WITH CONSTANT COEFFICIENTS,
!          MUST BE CONSTANT.  ALL PARAMETERS ON ONE CARD - CONSTANTS FIRST
! READ AS MANY PAIRS OF CONSTANT, PARAMETER SPEC AS GIVEN:
   12 IPARS = 0
    5 IPARS = IPARS + 1
      CALL RDREAL(AM(IPARS),IPT,IPT,80,IER)
      IF (IER.EQ.100) GOTO 21
      CALL PARRD(IPT,IPT,KK1(IPARS),IFAM,IGEN,ISPC)
      IF (KK1(IPARS).LE.0) GOTO 99
      GOTO 5
! NOW ABSORB TYPE 2 CONSTRAINT:
   21 CALL ADDCON(IPARS-1,KK1,AM,4)
      GOTO 1
! ERRORS:
   99 CALL ERRIN2(IPT,2,'cannot interpret L RELA card at point',' ')
      GOTO 1
  100 RETURN
      END SUBROUTINE RDRELA
!
!*****************************************************************************
!
      SUBROUTINE RDWORD(WORD,IWDLEN,IPT1,IPT2,IPTEND,IANY,IER)
!
! *** RDWORD updated by JCM 28 Apr 90 ***
!
!X
!C 13C
!H Reads the next word from a character string.
!
!A WORD    is *(*), and set on exit to contain the next readable word
!A IWDLEN  integer, is set to the total number of characters read
!A On entry IPT1 points to the first character position to consider in ICARD
!A          IPTEND points to the last character position to consider.
!A          IANY indicates whether or not the "word" may start with a
!A               a non-letter.  If IANY is negative, any character may
!A               start the word, but if IANY = 0 it must start with a letter.
!A               If IANY is positive, a special entry used by multi-source
!A               multiphase Profile Refinement is invoked.  If *Sn or *Pn
!A               (n an integer) is read where a word is expected, n is
!A               transferred to KSOURC or KPHASE as appropriate, and the
!A               NEXT word is read as normal.
!A
!A On exit WORD contains the next readable word in ICARD terminated by
!A              a space.
!A         IWDLEN is set to the total number of characters read
!A         IPT2 points to the character position in ICARD after
!A              the one which terminated the word.
!A          IER is an error indicator:
!A IER =   0  no errors found
!A IER =   100 word contained all spaces (ie nothing left in field to consider)
!A IER =   number in range 1-10: initial character of word is a digit (IANY>=0)
!A IER =   small negative number: initial symbol out of context (the
!A         number is an address in the table ISMBOL in /CHARS/)
!A IER =  -100: initial symbol out of context, not found in any table
!
!P Expects ICARD in /SCRACH/ to contain enough characters.
!D Ignores spaces till a non-space;  then absorbs word until it finds
!D      either a space, or
!D      the end of the permitted field as given in IPTEND, or
!D      WORD is full (in which case it reads characters and counts them
!D      in IWDLEN, but does not store them
!
      CHARACTER*(*) WORD
      CHARACTER*1 IC
      LOGICAL SAID

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      MAX = LEN(WORD)
      IPT = IPT1
    4 WORD = ' '
      IER = 0
      IWDLEN = 0
    1 IF (IPT.GT.IPTEND) GOTO 101
      IC = ICARD(IPT:IPT)
! IGNORE INITIAL SPACES;  TERMINATE ON FINAL SPACE:
      IF ((IC.EQ.' ') .AND. (IWDLEN.EQ.0)) GOTO 3
      IF ((IC.EQ.' ') .AND. (IWDLEN.NE.0)) GOTO 102
! TEST FOR LETTER FIRST, AS MOST LIKELY CHARACTER TO OCCUR:
      N1 = LETTER(IC)
      IF (N1.LE.0) GOTO 2
! LETTER, OR SUBSEQUENT NON-SPACE:
    5 IWDLEN = IWDLEN + 1
      IF (IWDLEN.LE.MAX) WORD(IWDLEN:IWDLEN) = IC
      GOTO 3
! NEITHER LETTER NOR SPACE:
! IF INITIAL LETTER READ, OR IF IER IS -VE ON ENTRY,  ANY SYMBOL IS  ACCEPTABLE:
    2 IF (IWDLEN.GT.0 .OR. IANY.LT.0) GOTO 5
      N1 = NSYMBL(IC)
! JUMP IF SYMBOL (NOT LETTER):
      IF (N1.GT.0) GOTO 6
      N1 = NDIGIT(IC)
      IF (N1.GT.0) IER = N1
! BY NOW IT IS AN UNRECOGNISABLE SYMBOL:
      IF (N1.EQ.0) IER = -100
      GOTO 101
! SYMBOL AT START OF WORD:
    6 IF (IANY.GT.0) THEN
        IF (N1.EQ.17) THEN
          IF (SAID(ICARD(IPT+1:IPT+1),'P')) THEN
            CALL RDINTG(KPHASE,IPT+2,IPT,80,IER)
            GOTO 4
          ELSEIF (SAID(ICARD(IPT+1:IPT+1),'S')) THEN
            CALL RDINTG(KSOURC,IPT+2,IPT,80,IER)
            GOTO 4
          ENDIF
        ENDIF
      ENDIF
      IER = -N1
      GOTO 101
! NEXT CHARACTER:
    3 IPT = IPT + 1
      GOTO 1
  102 IPT = IPT + 1
  101 IF ((IWDLEN.EQ.0) .AND. (IER.EQ.0)) IER = 100
      IPT2 = IPT

      END SUBROUTINE RDWORD
!
!*****************************************************************************
!
      SUBROUTINE RECELL(N,M)
!
! *** RECELL by JCM 13 Jul 83 ***
!
!X
!C 1B
!H Makes real or reciprocal space cell parameters from the others.
!A On entry:
!A   N=1 means make real space parameters; N=2 reciprocal
!A   M=1 means start from the 6 quadratic products in CPARS, A,B,C,D,E,F
!A       if real or A*,B*,C*,D*,E*,F* if reciprocal.
!A       ( A=a squared, B=b squared, D=b c cos alpha, etc.,
!A       and A*=a* squared, B*=b* squared, D*=b* c* cos alpha*, etc.,
!A       where a,b,c, etc are the cell parameters in real space,
!A       a*,b*,c* etc are in reciprocal.
!A
!A   M=2 means start from cell parameters.
!
!D The remaining annotation assumes, for the sake of clarity,
!D that N=1 and M=1.
!D
!D Accepts in /CELPAR/ the 6 quadratic products A* B* C* D* E* F*;
!D Makes first the usual cell parameters a* b* c* alpa* beta* gamma*,
!D then cos alpha*, cos beta*, cos gamma*.
!D (join here if on entry M=2)
!D Makes sin alpha*, sin beta*, sin gamma*:
!D Then makes the 9 corresponding quantities in real space, a,b,c,
!D cos alpha, cos beta, cos gamma, sin alpha, sin beta, sin gamma.
!D
!D Makes the volumes of both real & reciprocal cells in V(1) and V(2).
!D Now for entry M=1, we already have one set of quadratic products;
!D make the other set, and for entry M=2, make both sets.
!D
!D Finally forms the orthogonal matrices used in transformation of axes,
!D by a call of ORTHG.
!D
!D Called at end of RECIP (N=2,M=2), and at the end of a LSQ cycle which
!D refines cell parameters (N=1,M=1)
!
!O Writes its findings on unit LPT.
!
      DOUBLE PRECISION C
      DIMENSION ANGLE(3,2)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      NOUT = N
      MM = M
      IF ((NOUT.NE.1) .AND. (NOUT.NE.2)) THEN
        CALL ERRMES(-1,0,'RECELL entry neither 1 nor 2')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      IN = 3 - NOUT
      IF (MM.EQ.2) GOTO 5
! IF WE HAVE A,B, OR A* B* ETC. WE MUST FIRST MAKE FROM THEM THE
! CELL SIDES AND ANGLES TO CORRESPOND WITH ENTRY M=2
      DO I = 1, 3
        IF (CPARS(I,IN).GT.0.) GOTO 7
        CALL ERRIN2(I,-1,'Cell side','has negative square - set to 0')
        CELL(I,1,IN) = 0.
        GOTO 6
    7   CELL(I,1,IN) = SQRT(CPARS(I,IN))
    6 ENDDO
      J = 2
      K = 3
! FORM COSINES:
      DO I = 1, 3
        CELL(I,2,IN) = CPARS(I+3,IN)/(CELL(J,1,IN)*CELL(K,1,IN))
        IF (ABS(CELL(I,2,IN)).LE.1.) GOTO 8
        CALL ERRIN2(I,-1,'Cell angle','has cosine of modulus > 1 - set to 1')
        CELL(I,2,IN) = SIGN(1.0,CELL(I,2,IN))
    8   J = K
        K = I
      ENDDO
! FORM SINES - JOIN HERE IF M=2 - THEN START ON OTHER SPACE:
    5 DO I = 1, 3
        CALL SINCOS(CELL(I,2,IN),CELL(I,3,IN),'RECELL')
        ANGLE(I,IN) = DEGREE(ATAN2(CELL(I,3,IN),CELL(I,2,IN)))
      ENDDO
      J = 2
      K = 3
      DO I = 1, 3
        C = DBLE((CELL(J,2,IN))*CELL(K,2,IN)-CELL(I,2,IN))/(CELL(J,3,IN)*CELL(K,3,IN))
        CELL(I,2,NOUT) = SNGL(C)
        CALL SINCOS(CELL(I,2,NOUT),CELL(I,3,NOUT),'RECELL')
        ANGLE(I,NOUT) = DEGREE(ATAN2(CELL(I,3,NOUT),CELL(I,2,NOUT)))
        J = K
        K = I
      ENDDO
! UNIT CELL VOLUME:
      V(IN) = CELL(1,1,IN)*CELL(2,1,IN)*CELL(3,1,IN)*CELL(1,3,NOUT)*CELL(2,3,IN)*CELL(3,3,IN)
      V(NOUT) = 1/V(IN)
! CELL SIDES IN OTHER SPACE:
      DO I = 1, 3
        CELL(I,1,NOUT) = CELL(J,1,IN)*CELL(K,1,IN)*CELL(I,3,IN)/V(IN)
        J = K
        K = I
      ENDDO
      IF (MM.EQ.1) GOTO 10
! IF M=1 SET UP QUADRATIC PRODUCTS IN OTHER SPACE -
! IF M=2 SET UP QUADRATIC PRODUCTS IN BOTH SPACES:
      DO I = 1, 3
        CPARS(I,IN) = CELL(I,1,IN)*CELL(I,1,IN)
        CPARS(I+3,IN) = CELL(J,1,IN)*CELL(K,1,IN)*CELL(I,2,IN)
        J = K
        K = I
      ENDDO
   10 DO I = 1, 3
        CPARS(I,NOUT) = CELL(I,1,NOUT)*CELL(I,1,NOUT)
        CPARS(I+3,NOUT) = CELL(J,1,NOUT)*CELL(K,1,NOUT)*CELL(I,2,NOUT)
        J = K
        K = I
      ENDDO
      WRITE (LPT,2001) (CELL(I,1,1),I=1,3), (ANGLE(I,1),I=1,3), V(1),   &
     &                 (CELL(I,1,2),I=1,3), (ANGLE(I,2),I=1,3), V(2)
 2001 FORMAT (/' Real cell      ',3F10.4,3F8.2/' Volume = ',            &
     &        F10.4//' Reciprocal cell',3F10.4,3F8.2/' Volume = ', E12.4/)
      IF ((NOUT.EQ.2) .OR. (MM.EQ.1)) WRITE (LPT,2002) (CPARS(I,1),I=1,6)
 2002 FORMAT (/' Real cell quadratic products:'/                        &
     &       ' A (=a  sqrd)     B            C   D (=b c cos alpha)  E ','         F '/1X,6F12.5)
      IF ((NOUT.EQ.2) .OR. (MM.EQ.2)) WRITE (LPT,2000) (CPARS(I,2),I=1,6)
 2000 FORMAT (/' Reciprocal cell quadratic products:'/  ' A*(=a* sqrd)     B*          C*  D*(=b*c*cos alpha*)  E*'&
     &       ,'         F*'/1X,6F12.5)
      CALL ORTHG(M)
!* SOMEWHERE HERE MAKE ESDS TO PRINT LATER

      END SUBROUTINE RECELL
!
!*****************************************************************************
!
      SUBROUTINE RECIP
!
! *** RECIP by JCM 13 Jul 83 ***
!
!X
!C 1A
!H Reads the lattice parameters and forms the reciprocal cell.
!D The real cell parameters are read from the "C" card in the order
!D a, b, c (in Angstroms) alpha, beta, gamma (in degrees).
!D The reciprocal cell parameters,cell volume and orthogonal transformations
!D are calculated.
!D Cell edges and angles which are fixed by symmetry need not be given
!D on the "C" card.  Redundant parameters at the right hand end of a card
!D may be omitted.  Those not at the end may be omitted, but a comma
!D should be present to show that something was there.
!D     E.g for a cubic cell:       C   3.456
!D         for a tetragonal cell:  C  1.234  ,  2.345
!D         for a monoclinic cell:  C  1.234 2.345 3.456 , 88.43
!D Redundant values may of course be present in the ordinary way,
!D e.g. C 3.456 3.456 3.456  90  90  90
!
!D On exit, CELL() in COMMON /CELPAR/ contains a,b,c,cos(alpha,beta,gamma),
!D sin(same),a*,b*,c*,cos(alpha*,beta*,gamma*), sin(same)
!D RECELL has been called to set up symmetry relations and  orthogonal
!D matrices and fill in CPARS
!
!O Write to unit LPT any constraints which the symmetry places on the
!O real space cell parameters.
!
      CHARACTER*42 HEADNG
      CHARACTER*5 LABEL(6)
      DIMENSION CIN(6)
      LOGICAL HEAD
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      DATA HEADNG/'Symmetry constraints on lattice parameters'/
      DATA LABEL/'  a', '  b', '  c', 'alpha', ' beta', 'gamma'/

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      HEAD = .FALSE.
      IF (INREAD(19).GT.0) THEN
        CALL SYMOP
        IF (IBMBER .NE. 0) RETURN
      ENDIF
! READ C CARDS:
      NCELL = ICDNO(3)
      IF (NCELL.EQ.0) THEN
        CALL ERRMES(3,0,'starting "C" with cell parameters')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      ID = IABS(INREAD(3))
      DO IC = 1, NCELL
        CALL INPUTC(ID,CIN)
        ID = ID + NYZ
        IF (SDREAD) THEN
          DO I = 1, 6
            CELLSD(I,I) = CIN(I)
          ENDDO
        ELSE
          I = 0
          DO J = 1, 2
            DO K = 1, 3
              I = I + 1
              CELL(K,J,1) = CIN(I)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      INREAD(3) = -IABS(INREAD(3))
! DEAL WITH a:
      IP = 1
! COMPLAIN IF EITHER a FIXED OR a NOT GIVEN ON CARD
      IF (IPTCEL(1).NE.0) GOTO 30
   10 CALL ERRCH2(LABEL(IP),0,'in symmetry in RECIP -','would be fixed')
      IF (IBMBER .NE. 0) RETURN
   30 IF (CELL(1,1,1).NE.0.) GOTO 31
! ERROR IF ANY ESSENTIAL ITEM MISSING FROM C CARD:
   21 CALL ERRCH2(LABEL(IP),0,'in symmetry in RECIP - ','found zero')
      IF (IBMBER .NE. 0) RETURN
! NOW b and c:
   31 DO IP = 2, 3
        IF (IPTCEL(IP).NE.9999) GOTO 8
! JUMP IF RELATION FOUND INVOLVING EITHER b OR c
   11   IF (CELL(IP,1,1).EQ.0.) GOTO 21
! COMPLAIN ON b OR c NOT GIVEN ON CARD WHEN NEEDED
        GOTO 9
! CELL SIDES MAY NOT BE FIXED:
    8   IF (IPTCEL(IP).EQ.0) GOTO 10
! UNCHAIN TO FIND PREVIOUS PARAMETER USED IN RELATION
        IP1 = IP
   12   IP1 = IPTCEL(IP1)
        IF (IP1-IP) 13, 11, 12
! CELL SIDE RELATION MUST BE SIMPLE EQUALITY:
   13   A = SQRT((AMCELL(IP1)/AMCELL(IP)))
        IF (ABS(A-1.).GT.0.0001) THEN 
          CALL ERRMES(-1,0,'cell side relation found other than equality')
          IF (IBMBER .NE. 0) RETURN
        ENDIF
        IF (.NOT.HEAD) CALL MESS(LPT,1,HEADNG)
        HEAD = .TRUE.
        WRITE (LPT,2002) LABEL(IP), LABEL(IP1)
 2002   FORMAT (45X,A5,' =',A5)
        CELL(IP,1,1) = CELL(IP1,1,1)
    9 ENDDO
! NOW CROSS TERMS - MAY BE FREE, FIXED (90 OR 120), OR RELATED TO ANOTHER TERM.
! 120 IS DETECTED BY RELATION OF A CROSS TERM TO A CELL SIDE TERM.
      J = 2
      K = 3
      DO IP = 4, 6
        IF (IPTCEL(IP).EQ.9999 .AND. CELL(IP-3,2,1).EQ.0.) GOTO 21
! TURN ANGLES INTO COSINES;  THIS WILL TURN 0 INTO 1 FOR THOSE OMITTED
        CELL(IP-3,2,1) = COS(RADIAN(CELL(IP-3,2,1)))
! JUMP IF ANGLE FREE:
        IF (IPTCEL(IP).EQ.9999) GOTO 22
! JUMP IF ANGLE RELATED :
        IF (IPTCEL(IP).NE.0) GOTO 16
! CROSS TERM FIXED IMPLIES ANGLE IS 90:
        CELL(IP-3,2,1) = 0.
        IF (.NOT.HEAD) CALL MESS(LPT,1,HEADNG)
        HEAD = .TRUE.
        WRITE (LPT,2003) LABEL(IP)
 2003   FORMAT (45X,A5,' = 90')
        GOTO 22
! CROSS TERM IS INVOLVED IN A RELATION - FIND PREVIOUS CHAIN MEMBER:
   16   IP1 = IP
   17   IP1 = IPTCEL(IP1)
        IF (IP1-IP) 19, 22, 17
! RELATED TO PREVIOUS CELL PARAMETER - JUMP IF THAT ALSO A CROSS TERM:
   19   IF (IP1.GE.4) GOTO 18
! IF RELATED TO A*, B* OR C*, IMPLIES ANGLE IS 120.  THIS MAY BE ALTERED
! TO USE REAL SPACE CELL PARAMETERS - AT PRESENT THE MINUS SIGN IS
! NECESSARY BECAUSE THE RELATIONS HAVE BEEN OBTAINED IN RECIPROCAL SPACE
! NOT REAL, AND THE IMPLIED ANGLE IS 60 NOT 120.
        CELL(IP-3,2,1) = -(AMCELL(IP1)/(AMCELL(IP))*CELL(IP1,1,1)*CELL(IP1,1,1)/(CELL(J,1,1)*CELL(K,1,1)))
        IF (.NOT.HEAD) CALL MESS(LPT,1,HEADNG)
        HEAD = .TRUE.
        WRITE (LPT,2004) LABEL(IP)
 2004   FORMAT (45X,A5,'= 120')
        GOTO 22
! CROSS TERM RELATED TO A PREVIOUS ONE -
! PREVIOUS MUST HAVE BEEN GIVEN ON C CARD:
   18   IF (ABS(CELL(IP1-3,2,1)-1.).LT.0.0001) GOTO 21
        CELL(IP-3,2,1) = (AMCELL(IP1)/AMCELL(IP))*CELL(IP-3,1,1)*CELL(IP1-3,2,1)/CELL(IP1-3,1,1)
        IF (.NOT.HEAD) CALL MESS(LPT,1,HEADNG)
        HEAD = .TRUE.
        WRITE (LPT,2005) LABEL(IP), LABEL(IP1)
 2005   FORMAT (45X,A5,'=',A5)
   22   J = K
        K = IP - 3
      ENDDO
! SET UP REMAINING QUANTITIES CONNECTED WITH CELL PARAMETERS:
      CALL RECELL(2,2)

      END SUBROUTINE RECIP
!
!*****************************************************************************
!
      SUBROUTINE REINDX(DPROP)

      USE REFVAR

!
! *** REINDX updated by PJB 1 Feb 1994 ***
!
!H To reindex a set of reflections after a least squares cycle in which
!H the propagation vector changes
!
!A On entry DPROP(3) is the change in propagation vector
!
      DIMENSION DPROP(3)

      INCLUDE 'PARAMS.INC'
      INCLUDE 'REFLNS.INC'

      DO KNOW = 1, MAXK
        IF (ISMAG(KNOW).EQ.0) GOTO 1
        IF (ISMAG(KNOW).GT.0) THEN
          CALL GMADD(rHKL(1,KNOW),DPROP,rHKL(1,KNOW),3,1)
        ELSE
          CALL GMSUB(rHKL(1,KNOW),DPROP,rHKL(1,KNOW),3,1)
        ENDIF
    1 ENDDO

      END SUBROUTINE REINDX
!
!*****************************************************************************
!
      SUBROUTINE RELATE
!
! *** RELATE updated by JCM 11 Aug 88 ***
!
!X
!C 6C
!H In LSQ programs, converts a vector of derivatives wrt variables into the
!H vector of derivatives wrt basic variables.
!P /DERVAR/ must hold NVARV derivatives of some calculated function wrt
!P all variables, in DERIVV.
!P The constraint information must be set up in /CONSTR/ by a call of
!P VARMAK.
!
!D Applies the (strict) constraints to the vector DERIVV to convert it into
!D a vector DERIVB in /DERBAS/, of LVARB derivatives of the same calculated
!D function wrt basic variables.
!
      INCLUDE 'PARAMS.INC'
      
      COMMON /CONSTR/ JCONST, JROWPT(301), JCMAT(200), AMOUNT(200), NEXTJ

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB

      REAL            DERIVV
      INTEGER                          LVARV
      COMMON /DERVAR/ DERIVV(MaxVVar), LVARV

      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)

      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      IF (SIMUL .OR. (LVARV.LE.0)) RETURN
      DO I = 1, LVARB
        DERIVB(I) = DERIVV(LBSVR(I))
      ENDDO
! ADD DERIVATIVES FOR RELATED VARIABLES:
      DO J = 1, JCONST
        JROW = JROWPT(J)
        JNEXT = JROWPT(J+1) - 1
        DO K = JROW, JNEXT
          I = JCMAT(K)
          DERIVB(I) = DERIVB(I) + AMOUNT(K)*DERIVV(LRDVR(J))
        ENDDO
      ENDDO

      END SUBROUTINE RELATE
!
!*****************************************************************************
!
      SUBROUTINE RELPAR(N1,A1,N2,A2,NFIX,FIX)
!
! *** RELPAR by JCM 13 Jul 83 ***
!
!X
!C 6A
!H In the setting up of LSQ  applications, relates two parameters by
!H a simple linear relationship.
!A On entry N1 = the serial number of parameter 1 in the array NFIX
!A          N2 = the serial number of parameter 2 in the array NFIX
!A          A1 = a constant multiplier for parameter 1
!A          A2 = a constant multiplier for parameter 2
!A          the arrays NFIX and AFIX hold a (temporary) structure for
!A          chained parameters including the two given here.  They could
!A          refer, e.g., to 3 atom position coordinates, or to 6 cell
!A          quadratic products.
!
!D RELPAR absorbs into NFIX and FIX the relation:
!D     A1 x shift in parameter N1 = A2 x shift in parameter N2
!D dealing, if necessary, with any other similar relationships already
!D present, by setting up chains.
!
      DIMENSION NFIX(1), FIX(1)

      I1 = N1
      I2 = N2
      IOLD1 = NFIX(I1)
      IOLD2 = NFIX(I2)
! IOLD1 AND IOLD2 ARE WHAT IS ALREADY THERE.  THESE WILL BE ONE OF
!     0=FIXED
!      9999=FREE - NO REFERENCE HAS BEEN MADE TO THIS PARAMETER SO FAR
!     N, A FORWARD REFERENCE IN A CHAIN
      IF ((IOLD1.NE.9999) .OR. (IOLD2.NE.9999)) GOTO 1
! IF HERE, BOTH WERE FREE; SET UP 2-ELEMENT CHAIN AND EXIT
      NFIX(I1) = I2
      NFIX(I2) = I1
      FIX(I1) = A1
      FIX(I2) = A2
      GOTO 100
! IF HERE, ONE/BOTH PARS WERE FIXED/RELATED:
    1 IF (IOLD1.NE.0) GOTO 2
! P1 WAS ALREADY FIXED;  FIX (POSSIBLE CHAIN CONTAINING) P2
      CALL FIXPAR(I2,NFIX)
      GOTO 100
    2 IF (IOLD2.NE.0) GOTO 3
! SIMILARLY IF P2 FIXED, FIX (POSSIBLE CHAIN CONTAINING) P1
      CALL FIXPAR(I1,NFIX)
      GOTO 100
    3 IF (IOLD1.NE.9999) GOTO 4
! IF HERE, P2 IS CHAINED BUT P1 FREE; ADD P1 TO CHAIN
      NFIX(I1) = NFIX(I2)
      NFIX(I2) = I1
      FIX(I1) = A1*FIX(I2)/A2
      GOTO 100
    4 IF (IOLD2.NE.9999) GOTO 5
! SIMILARLY, P1 CHAINED BUT P2 FREE
      NFIX(I2) = NFIX(I1)
      NFIX(I1) = I2
      FIX(I2) = A2*FIX(I1)/A1
      GOTO 100
! IF HERE, BOTH P1 AND P2 ALREADY BELONGED TO CHAINS.  DISCOVER
! WHETHER SAME OR DIFFERENT CHAINS.
    5 L = IOLD2
    6 IF (L.EQ.I1) GOTO 8
! TO 8 IF SAME CHAIN
      IF (L.EQ.I2) GOTO 7
! TO 7 IF DIFFERENT CHAINS
      L = NFIX(L)
! UNCHAIN
      GOTO 6
! HERE ON 2 SEPARATE CHAINS TO BE MERGED.  FIRST SCALE CHAIN 1 BY
! CHAIN 2
    7 A = A1*FIX(I2)/(A2*FIX(I1))
      CALL SCLCHN(I1,A,NFIX,FIX)
! CROSS LINKS
      IT = NFIX(I1)
      NFIX(I1) = NFIX(I2)
      NFIX(I2) = IT
      GOTO 100
! HERE ON SAME CHAIN - SHOULD HAVE SAME SCALE RATIOS
    8 IF (ABS(A1*FIX(I2)-A2*FIX(I1)).GE.0.0001) CALL FIXPAR(I1,NFIX)
  100 RETURN
      END SUBROUTINE RELPAR
!
!*****************************************************************************
!
      SUBROUTINE RELSM3(R,NFIX,FIX)
!
! *** RELSM3 updated by JCM 13 Feb 90 ***
!
!X
!C 6A
!H Forms a complete set of relations imposed by symmetry on the given
!H 3 parameters of a LSQ application.
!A On entry R holds a generalised symmetry rotation operator.
!A On exit arrays NFIX and FIX hold the relationships found.
!
!D Adds any found relationships to the general collection which will
!D eventually be used by routine VARMAK.
!
      DIMENSION R(3,3), NFIX(3), FIX(3)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      DO K = 1, 3
! COUNTS 3 EQUATIONS
        IZ = 0
! COUNTS NON-ZERO COEFFICIENTS FOUND IN EACH EQUATION
        DO I = 1, 3
! COUNTS COEFFICIENTS WITHIN AN EQUATION
          A = R(K,I)
          IF (I.EQ.K) A = A - 1.
          IF (ABS(A).LT.1.E-5) GOTO 2
          IZ = IZ + 1
          IF (IZ.NE.1) GOTO 3
          N1 = I
          A1 = A
! N1 AND A1 RECORD POSITION AND VALUE OF 1ST NON-ZERO
          GOTO 2
    3     IF (IZ.EQ.2) GOTO 4
          WRITE (LPT,3000) A1, A2, A
          CALL BMBOUT
          RETURN
    4     N2 = I
          A2 = -A
! RECORD 2ND NON-ZERO IN N2,A2
    2   ENDDO
        IF (IZ.EQ.1) CALL FIXPAR(N1,NFIX)
        IF (IZ.EQ.2) CALL RELPAR(N1,A1,N2,A2,NFIX,FIX)
      ENDDO
      RETURN
 3000 FORMAT (/' ERROR ** in RELSM3 finding symmetry relations ',       &
     &        'between parameters',/' row of matrix ',                  &
     &        'minus unit matrix is',3F8.2)
      END SUBROUTINE RELSM3
!
!*****************************************************************************
!
      SUBROUTINE RELSM6(R,NFIX,FIX)
!
! *** RELSM6 updated by JCM 13 Feb 90 ***
!
!X
!C 6A
!H Forms a complete set of relations imposed by symmetry on the given
!H 6 parameters of a LSQ application.
!A On entry R holds a generalised symmetry rotation operator.
!A On exit arrays NFIX and FIX hold the relationships found.
!
!D Adds any found relationships to the general collection which will
!D eventually be used by routine VARMAK.
!
      DIMENSION R(3,3), NFIX(6), FIX(6)
      DIMENSION LKUP(3,3)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      DATA LKUP/1, 6, 5, 6, 2, 4, 5, 4, 3/

      DO K = 1, 3
        DO L = K, 3
! COUNTS 6 EQUATIONS
          IZ = 0
! COUNTS NON-ZERO COEFFICIENTS FOUND IN EACH EQUATION
          DO I = 1, 3
            DO J = I, 3
! COUNTS COEFFICIENTS WITHIN AN EQUATION
              A = R(K,I)*R(L,J)
              IF (I.NE.J) A = A + R(L,I)*R(K,J)
              IF ((I.EQ.K) .AND. (J.EQ.L)) A = A - 1.
              A = FLOAT(NINT(A))
              IF (A.EQ.0.) GOTO 2
              IZ = IZ + 1
              IF (IZ.NE.1) GOTO 3
              N1 = LKUP(I,J)
              A1 = A
! N1 AND A1 RECORD POSITION AND VALUE OF 1ST NON-ZERO
              GOTO 2
    3         IF (IZ.EQ.2) GOTO 4
              WRITE (LPT,3000) A1, A2, A
              CALL BMBOUT
              RETURN
    4         N2 = LKUP(I,J)
              A2 = -A
! RECORD 2ND NON-ZERO IN N2,A2
    2       ENDDO
          ENDDO
          IF (IZ.EQ.1) CALL FIXPAR(N1,NFIX)
          IF (IZ.EQ.2) CALL RELPAR(N1,A1,N2,A2,NFIX,FIX)
        ENDDO
      ENDDO
      RETURN
 3000 FORMAT (/' ERROR ** in RELSM6 finding symmetry relations ',       &
     &        'between parameters',/' row of matrix ','minus unit matrix is',3F8.2)
      END SUBROUTINE RELSM6
!
!*****************************************************************************
!
      SUBROUTINE RFACS(IN)
!
! *** RFACS updated by JCM 14 Mar 89 ***
!
!X
!C 6B
!H A multiple entry routine to deal with all aspects of R Factor
!H calculations and statistics for single crystal observations, and
!h for slack constraints.
!A On entry, IN indicates the calculation required:
!A   IN=1  Set up all quantities, clearing to zero
!A   IN=2  Add in contributions for conventional Obs R Factors
!A   IN=3  Print out for conventional Obs R Factors
!A   IN=4  Add in contributions for slack constraint type ISLKTP in /SLAKDA/
!A   IN=5  Print out for slack constraint, type ISLKTP
!A   IN=6  Print out for both obs and slack constraints
!
!D If IWGHT=1 we expect that the weights are unity: we try to avoid
!D unnecessary R factors in this case.
!D The R Factors are:
!D       R1=simple R factor, 100*sum[ABS(diffs)]/sum[obs]
!D       R2=squared R factor, 100*sum[sqrd diffs]/sum[sqrd obs]
!D       R3=simple weighted R factor,
!D                      1OO*sum[ABS(weighted(diffs))]/sum[weighted(obs)]
!D       R4=Squared weighted R factor
!
!O For entries 3,5 and 6 prints R factors on unit LPT.
!
      INCLUDE 'PARAMS.INC'
      
      LOGICAL TESTOV

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /RSTATS/ RNUM, RDEN, RSNUM, RSDEN, RWNUM, RWDEN, RWSDEN,   &
     &                CHI2
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

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      GOTO (1,2,3,4,5,6), IN
      CALL ERRIN2(IN,0,'in RFACS call - type','not written')
      IF (IBMBER .NE. 0) RETURN
! INITIAL ENTRY
    1 RNUM = 0.0
      RDEN = 0.0
      RSNUM = 0.0
      RSDEN = 0.0
      RWNUM = 0.0
      RWDEN = 0.0
      SUMWD = 0.0
      RWSDEN = 0.0
! TYPES OF SLACK CONSTRAINT:
      CALL GMZER(SLKSWD,1,4)
      GOTO 100
! ADDING IN ENTRY
    2 D = DIFF*DIFF
      G = OBS*OBS
      RNUM = RNUM + ABS(DIFF)
      RDEN = RDEN + ABS(OBS)
      RSNUM = RSNUM + D
      RSDEN = RSDEN + G
      RWNUM = RWNUM + ABS(WDIFF)
      RWDEN = RWDEN + SQRTWT*ABS(OBS)
      SUMWD = SUMWD + WT*D
      RWSDEN = RWSDEN + WT*G
      GOTO 100
! PRINT
    3 IF (TESTOV(RNUM,RDEN)) THEN
        CALL MESS(LPT,1,'R Factors not available as denominators zero')
        GOTO 100
      ENDIF
      R1 = 100.0*RNUM/RDEN
      R2 = 100.0*RSNUM/RSDEN
      WRITE (LPT,2001) R1, R2
 2001 FORMAT (/' R1=Sum diffs/Sum obs =',G12.4/' R2=Sum squares diffs/Sum squares obs = ',G12.4)
      CHISQ = SUMWD/FLOAT(NOBS-LVARB)
      WRITE (LPT,2003) CHISQ, SUMWD
      WRITE (LPT,2033) NOBS, LVARB
 2033 FORMAT (' For',I6,' observations and',I4,' basic variables')
      IF (IWGHT.EQ.1) GOTO 100
      R3 = 100.0*RWNUM/RWDEN
      R4 = 100.0*SUMWD/RWSDEN
      WRITE (LPT,2002) R3, R4
 2002 FORMAT (' R3=Sum weighted diffs/Sum weighted obs = ',G12.4/       &
     &       ' R4=Sum squares weighted diffs/Sum squares weighted obs =',G12.4)
      GOTO 100
! SPECIAL ENTRY FROM SLACK CONSTRAINT CALCULATION - ADD IN:
    4 SLKSWD(ISLKTP) = SLKSWD(ISLKTP) + WT*DIFF*DIFF
      GOTO 100
! PRINTING FOR SLACK CONSTRAINTS:
    5 WRITE (LPT,2010) ISLKTP
!* NEED DATA STATEMENT WITH DIFFERENT TYPES
      CSQTOT = SLKSWD(ISLKTP)/FLOAT(NSLAK(ISLKTP))
      WRITE (LPT,2011) CSQTOT, NSLAK(ISLKTP)
      GOTO 100
! PRINTING FOR SLACK CONSTRAINTS AND CONVENTIONAL OBS TOGETHER:
    6 IF (NSKTOT.EQ.0 .OR. SLONLY) GOTO 100
      CNUM = SUMWD
      CDEN = FLOAT(NOBS-LVARB)
      DO ISK = 1, 4
        IF (NSLAK(ISK).EQ.0) GOTO 36
! WE ONLY WANT THIS FIGURE IF WE HAVE BOTH CONVENTIONAL OBS AND SLACK
! CONSTRAINTS:
        CNUM = CNUM + SLAKWT(ISK)*SLKSWD(ISK)
        CDEN = CDEN + SLAKWT(ISK)*FLOAT(NSLAK(ISK))
   36 ENDDO
      CSQTOT = CNUM/CDEN
      WRITE (LPT,2005) CSQTOT
  100 RETURN
 2003 FORMAT (' Chi squared =',G12.3,' Sum weighted diffs sqrd =',G12.3)
 2010 FORMAT (/' For slack constraints type',I4)
 2011 FORMAT (/' Chi squared=',G12.3,' for',I4,' constraints')
 2005 FORMAT (' Chi squared for conventional obs',' and all slack constraints = ',G12.3)

      END SUBROUTINE RFACS
!
!*****************************************************************************
!
      SUBROUTINE ROTOSM(H,RH,IOP,ISS)
!
! *** ROTOSM corrected by PJB 31-May-1994 ***
!
!X
!C 1B
!H Calculates the effect of the rotation matrix of a symmetry operator,
!H on a vector given on orthogonal axes.
!A On entry  H(1:3)is a real vector in standard orthogonal coordinates
!A           IOP  is the number of a symmetry operator, negative for one
!A                related by a centre of symmetry to the stored one
!A           ISS  defines the mode of operation:
!A ISS = 0  Set-up by calculating the whole set of symmetry operations in
!A          orthogonal co-ordinates
!A ISS > 0  Calculate the effect of the symmetry operator IOP.
!A ISS < 0  Calculate the effect of the inverse of the symmetry operator IOP.
!A On exit   RH(1:3) is the vector obtained by operating with the
!A                rotation matrix of the element IOP on H.
!P SYMOP and RECIP must have set up the symmetry and cell parameters.
!P ROTOSM must be called with ISS=0 before any other use is made of it
!D The call with ISS=0 sets the orthogonal symmetry elements into
!D COMMON /ORTSYM/
!
!N ENTRY CROTO(CH,CRH,IOP,ISS) does the rotation for the complex
!N vectors CH and CRH.
!N
!N Note that in orthogonal coordinates there is no distinction between
!N real and reciprocal space
!
      DIMENSION H(3), RH(3), TEMP(3,3)
      COMPLEX CH(3), CRH(3)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      COMMON /ORTSYM/ SYMORT(3,3,24), NFLAG
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22

      I = IABS(IOP)
      IF (ISS.NE.0 .AND. NFLAG.EQ.42) GOTO 1
! PUT SYMMETRY ROTATIONS ONTO ORTHOGONAL AXES
! USE SYMORT(,,1) TEMPORARILY FOR TRANSPOSE:
      CALL GMEQ(ORTH(1,1,1),SYMORT(1,1,1),3,3)
      CALL TRANSQ(SYMORT(1,1,1),3)
      DO NO = 2, NOPC
        CALL MultiplyMatrices(SYM(1,1,NO),ORTH(1,1,2),TEMP,3,3,3)
        CALL MultiplyMatrices(SYMORT(1,1,1),TEMP,SYMORT(1,1,NO),3,3,3)
      ENDDO
      CALL GMUNI(SYMORT(1,1,1),3)
      NFLAG = 42
      IF (ISS.EQ.0) GOTO 100
    1 IF (ISS.LT.0) I = INVERS(I)
      CALL MultiplyMatrices(SYMORT(1,1,I),H,RH,3,3,1)
      IF (IOP.LT.0) CALL GMREV(RH,RH,3,1)
      GOTO 100
!  ENTRY FOR COMPLEX VECTORS
      ENTRY CROTO(CH,CRH,IOQ,IST)
      I = IABS(IOQ)
      IF (IST.LT.0) I = INVERS(I)
      CALL RCMPRD(SYMORT(1,1,I),CH,CRH,3,3,1)
      IF (IOQ.LT.0) CALL GMREV(CRH,CRH,2,3)
      GOTO 100
  100 RETURN

      END SUBROUTINE ROTOSM
!
!*****************************************************************************
!
      SUBROUTINE ROTSYM(H,RH,IOP,ISS)
!
! *** ROTSYM by JCM 11 Apr 83 ***
!
!X
!C 1B
!H Rotates the vector H into RH by the given symmetry operator.
!A On entry  H(1:3) is a 1x3 vector (in either space)
!A          IOP is the serial number of a symmetry rotation matrix
!A          ISS is positive for pre-multiplication by rotation matrix,
!A              negative for post-multiplication
!A              the absolute value of ISS is 1 for real and 2 for
!A              reciprocal space
!A On exit  RH(1:3) is the 1x3 vector resulting from multiplying H by
!A              the rotation matrix as specified by ISS
!P SYMOP to set the symmetry matrices
!
      DIMENSION H(3), RH(3)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22

      I = IOP
      IS = ISS
! IF RECIPROCAL SPACE REQUESTED, MATRIX USED MUST BE INVERSE TRANSPOSED OF
! THAT REFERRING TO REAL SPACE.  THE INVERSE IS OBTAINED BY INDIRECT ADDRESSING
! VIA ARRAY INVERS, AND THE TRANSPOSE BY REVERSING THE SEQUENCE OF THE
! MULTIPLICATION:
      IF (IABS(IS).EQ.2) I = INVERS(I)
      IF (IS.LT.0) IS = IS + 5
      GOTO (1,2,1,2), IS
! PRE-MULTIPLICATION
    1 CALL MultiplyMatrices(SYM(1,1,I),H,RH,3,3,1)
      GOTO 100
! POST-MULTPLICATION
    2 CALL MultiplyMatrices(H,SYM(1,1,I),RH,1,3,3)
  100 RETURN

      END SUBROUTINE ROTSYM
!
!*****************************************************************************
!
      LOGICAL FUNCTION SAID(INCHAR,WANT)
!
! *** SAID by JCM 27 Sep 87 ***
!
!X
!C 11C
!H Decides whether the character variables INCHAR and WANT are the same,
!H ignoring any distinction between upper and lower case.
!A On entry INCHAR is an A1 character to test
!A          WANT is the other A1 char to test
!D Sets SAID .TRUE. if INCHAR and WANT are equal, ignoring the case
!D if they are letters.
!
      CHARACTER*1 INCHAR, WANT

      L = LETTER(INCHAR)
      M = LETTER(WANT)
      SAID = (L.EQ.M .AND. L.NE.0) 

      END FUNCTION SAID
!
!*****************************************************************************
!
      SUBROUTINE SCLCHN(NP,A,NFIX,FIX)
!
! *** SCLCHN by JCM 13 Jul 83 ***
!
!X
!C 6A
!H A specialist routine used during the setting up of constraints on LSQ
!H variables.  Scales a chain of connected variables by the given constant.
!A On entry NP points to the first element of the chain in the arrays
!A             NFIX, FIX.
!A          A is the real constant by which the chain is to be scaled,
!A          NFIX is an integer array holding pointers within itself to
!A             describe the existing chain,
!A          FIX is a real array of multipliers of the corresponding
!A             elements of NFIX.
!D Scales all the elements of FIX which correspond to NFIX(NP) and those
!D elements which are chained to it.
!
      DIMENSION NFIX(1), FIX(1)

      J1 = NP
    1 FIX(J1) = A*FIX(J1)
      J1 = IABS(NFIX(J1))
      IF (J1.NE.NP) GOTO 1

      END SUBROUTINE SCLCHN
!
!*****************************************************************************
!
      FUNCTION SCLPRD(H1,H2,IR)
!
! *** SCLPRD by JCM 26 Apr 84 ***
!
!X
!C 1B
!H Forms the scalar product of two vectors referred to crystal axes.
!A On entry H1 and H2 are two 1x3 vectors referred to the crystal axes.
!A             They are in real space if IR=1, reciprocal if IR=2.
!D The function is set to be the scalar product of H1 and H2
!P RECIP must have been obeyed to set up the cell parameters.
!
      DOUBLE PRECISION SCL
      DIMENSION H1(3), H2(3)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      SCL = 0.
      J = 2
      K = 3
      DO I = 1, 3
        SCL = SCL + H1(I)*H2(I)*CPARS(I,IR) + (H1(J)*H2(K)+H1(K)*H2(J)) * CPARS(I+3,IR)
        J = K
        K = I
      ENDDO
      SCLPRD = SNGL(SCL)

      END FUNCTION SCLPRD
!
!*****************************************************************************
!
      SUBROUTINE SETANI
!
! *** SETANI by JCM 23 Sep 83 ***
!
!X
!C 4A
!H Reads "T" cards to set up for calculation of anisotropic temperature factors.
!D Reads cards starting "T"  giving the atom name, type of atf required
!D and 6 coefficients in order a11, a22, a33, a23, a13, a12.
!D SETANI converts the 6 coefficients to type 5 for internal use, stores
!D them in ATF in COMMON /ANISO/ and sets up vector IATYP which says which
!D type they started as.
!
!P RECIP and ATOPOS must have read the cell parameters and atomic positions.
!I Causes "T" cards to be read from the copy of the Crystal Data
!I File on unit IO10.
!O Writes its findings to unit LPT.
!N Types of atf at present allowed are:
!N  0: Convert given itf to an atf, then proceed as if type 2.
!N  2: exp-{1/4(a11 ha* ha* + . . +a12 ha* kb* + . . )}
!N  3: exp-{2*pi*pi(a11 ha* ha* + . .+2a12 ha* kb*  + . .)}
!N  4: exp-(a11 h h + . . +a12 h k + . .)
!N  5: exp-(a11 h h + . . +2a12 h k + . . )
!
!N Numbers 3 and 4 are out of Stout and Jensen pp449-450,
!N number 2 is used in Hewat profile refinement
!N and 5 is the form used internally in this program.
!
      CHARACTER*4 LABB
      DIMENSION ACOEFF(6)
      COMMON /ANISO / ATF(6,50), KATF(6,50), IAPT(150), IATYP(50), KOM1
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
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

! IT IS NECESSARY THAT ATOPOS HAS SET UP THE ATOM LABELS:
      IF (INREAD(1).GE.0) CALL ATOPOS
! SET NO ATF ON ANY ATOM:
      CALL JGMZER(IAPT,1,NATOM)
! SET "T CARDS READ"
      INREAD(20) = -IABS(INREAD(20))
!
! READ NUMBER OF ANISOTROPIC TEMPERATURE FACTORS (NO. OF "T" CARDS):
      NCARD = ICDNO(20)
      IF (NCARD.EQ.0) THEN
        CALL MESS(LPT,1,'No anisotropic temperature factors given')
        GOTO 100
      ENDIF
      IER = IERR
      CALL ERRCHK(1,NCARD,50,1,'anisotropic temperature factors')
      IF (IERR.GT.IER) GOTO 100
      CALL MESS(LPT,1,'Anisotropic temperature factors:')
      CALL MESS(LPT,0,'atom  type    11        22        33        23        13        12')
! READ T CARDS ONE BY ONE:
      ID = IABS(INREAD(20))
      DO IP = 1, NCARD
        CALL INPUTT(ID,LABB,LBBLEN,IATYP(IP),ATF(1,IP),IER)
        IF (IER.NE.0) IERR = IERR + 1
        ID = ID + NYZ
        J = IATOM(LABB)
        IF (J.LE.0) THEN
          CALL ERRATM(LABB,2,'on T card')
          GOTO 100
        ENDIF
! NAME FOR ATF MATCHED WITH NAME IN ATOM LIST:
        IAPT(J) = IP
        IT = IATYP(IP)
        IF (IT.NE.0) GOTO 11
! CONVERT ITF ALREADY READ TO BE AN ATF, AND SET ITF TO ZERO:
        DO I = 1, 3
          ATF(I,IP) = TF(J)
          ATF(I+3,IP) = TF(J)*CELL(I,2,2)
        ENDDO
        TF(J) = 0.
        GOTO 12
! CHECK IF TYPE 1, AND FORCE THESE TO BE 2:
   11   IF (IT.NE.1) GOTO 3
        CALL MESS(LPT,1,' *** WARNING ** ATF type 1 redefined to exclude cosines - type 2 assumed')
   12   IATYP(IP) = 2
        IT = 2
! JOIN HERE ON ALL OTHER TYPES:
    3   WRITE (LPT,2002) LABB, IATYP(IP), (ATF(I,IP),I=1,6)
 2002   FORMAT (' ',A4,I5,6F10.4)
        GOTO (22,22,23,24,5), IT
   22   FAC = 0.25
        GOTO 30
   23   FAC = TWOPI*PI
   30   ACOEFF(1) = FAC*CPARS(1,2)
        ACOEFF(2) = FAC*CPARS(2,2)
        ACOEFF(3) = FAC*CPARS(3,2)
        ACOEFF(4) = FAC*CELL(3,1,2)*CELL(2,1,2)
        ACOEFF(5) = FAC*CELL(1,1,2)*CELL(3,1,2)
        ACOEFF(6) = FAC*CELL(2,1,2)*CELL(1,1,2)
        GOTO 25
   24   DO I = 1, 3
          ACOEFF(3+I) = 0.5
          ACOEFF(I) = 1.
        ENDDO
        GOTO 25
! CONVERT ATF() FOR INTERNAL USE;  CONATF WILL CONVERT BACK
   25   DO I = 1, 6
          ATF(I,IP) = ATF(I,IP)*ACOEFF(I)
        ENDDO
    5 ENDDO
  100 RETURN
      END SUBROUTINE SETANI
!
!*****************************************************************************
!
      SUBROUTINE SETFOR
!
! *** SETFOR updated by JCM 24 Nov 91 ***
!
!X
!C 4A
!H Sets up data for scattering or form factor calculations.
!D Reads "F" cards which all start  "F <name> ITYP"
!D Where <name> is a character string of maximum length 4 introduced
!D and terminated by spaces. <name> is the label of the scattering
!D factor and will often be a chemical symbol. The factor applies to
!D all atoms whose name starts with the scattering factor name. In the
!D atom name the character following <name> must be a non-letter.
!D For example - a set of A cards:
!D A Pb1
!D A FreD
!D A Pb2
!D A P4
!D A O14A
!D A O14B
!D would need 4 scattering factors, labelled:
!D F Pb      (applies to Pb1 and Pb2)
!D F P       (applies to P4)
!D F O       (applies to O14A and O14B)
!D F FreD    (applies to FreD)
!D
!D ITYP is an integer indicating the way in which the form-factor will be
!D           given
!D ITYP = 0  means "this is a multiplicative factor".  It is useful
!D           when the user wants to scale his structure factors up or down,
!D           without altering his other "F" cards.
!D For ITYP positive, at present 4 values are defined:
!D ITYP = 1  Neutron nuclear scattering.  In this case the scattering
!D           length is given on the same card after ITYP
!D ITYP = 2  Form factors given by series expansion of exponential terms,
!D           of the form:    sum[a(n)*exp{-a(n+1)*s*s}] + c
!D                           with  n=1,2,3 or 4  and  s=sin(theta)/lambda
!D           the coefficients a(n) and c are given on one, or possibly two
!D           "F" cards.  The cards each start "F <name> ITYP" and the 5, 7
!D           or 9 coefficients follow ITYP in flexible FORMAT. If it is
!D           necessary to use two cards they should be consecutive.
!D ITYP = 3  Form factors given tabulated against s=sin(theta)/lambda.
!D           the tabulated values, pairs of s and f in ascending s, follow.
!D           If multiple cards are needed each should start "F <name> 3" and
!D           they must be consecutive.
!D ITYP = 4  As 2, but the result is multiplied by sin(theta)/lambda squared
!D           This is the form of expansion used for the <jl> form factors
!D           with l not zero.
!D ITYP = 5  Obtain form factor from radial functions supplied on
!D           W RADF cards.
!D ITYP negative indicates an anomalous scattering factor.
!D       At present only type -1 is defined, which is the straightforward
!D       complex anomalous scattering factor.  Two numbers follow the -1
!D       on the "F" card; they are f' and f" the real and imaginary parts
!D       of the complex multiplicative factor to be applied to this atom
!D       (in addition to any other scattering factor).
!
!I Reads "F" cards from the copy of the Crystal Data File on unit IO10.
!O Writes its findings to unit LPT.
!N If "slack constraints only" is set, allows "no F cards".
!
      CHARACTER*4 LABF, LSAV
      LOGICAL ANOM
      COMMON /ANSCAT/ NAMODE(20), FDASH(20), KOM2
      COMPLEX FDASH
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FONAM / FONA(20,9), FONAME(20)
      CHARACTER*4 FONAME, FONA
      COMMON /FORGRP/ NATFOR(20,150), NAFPNT(20)
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
!
! ATOPOS MUST BE OBEYED TO SET UP REQUIRED SCATTERING FACTOR NAMES IN THE TABLE
! FONAME
      IF (INREAD(1).GT.0) CALL ATOPOS
! SET "F" CARDS READ:
      INREAD(6) = -IABS(INREAD(6))
! CLEAR SCALE FACTORS, POINTERS FOR BOTH SORTS OF SCATTERING FACTORS
! AND COUNTS OF NUMBERS OF TERMS (IN CASE TYPES 2 OR 3):
      DO I = 1, 20
        CMULT(I) = 1.0
        NT(I) = 0
        MODE(I) = 0
        NAMODE(I) = 0
      ENDDO
! CLEAR FACTORS-ATOMS TABLES:
      DO I = 1, 20
        NAFPNT(I) = 0
        DO J = 1, 150
          NATFOR(I,J) = 0
        ENDDO
      ENDDO
! SET "NO ANOMALOUS SCATTERING AT ALL" AND KEEP NUMBER OF FACTORS TO FIND:
      ANOM = .FALSE.
      NFKEEP = NUMFNM
! ID WORKS THROUGH "F" CARDS SEQUENTIALLY:
      NFREAD = 0
      ID = IABS(INREAD(6))
! NCARD = NUMBER OF "F" CARDS ON CRYSTAL DATA FILE:
      NCARD = ICDNO(6)
      IF (NCARD.EQ.0) THEN
        IF (.NOT.SLONLY) CALL ERRMES(1,1,'No cards labelled F read')
        GOTO 100
      ENDIF
    2 CALL INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      NFREAD = NFREAD + 1
      ID = ID + NYZ
    5 IF (IER.NE.0) IERR = IERR + 1
      IPKEEP = IPT
! INPUTF READS THE LABEL, THE TYPE AND THE WHOLE CARD TO ICARD AS A1, AND
! LEAVES IPT POINTING TO WHERE TO CONTINUE READING.
!
! FIND LABEL IN TABLE MADE UP BY ATOPOS - IF THE NEW LABEL IS NOT THERE, IT
! DOES NOT APPLY TO ANY GIVEN POSITIONS.  IF A NEW LABEL IS ADDED, AND OVER-
! FILLS THE TABLE (INDICATING TOO MANY FACTORS FOR THE SPACE ALLOWED IN
! COMMON /FORMDA/), LMATCH WILL COMPLAIN:
      LKEEP = NUMFNM
      LFAC = LMATCH(LABF,FONAME,NUMFNM,20)
      IF (LKEEP.NE.NUMFNM) THEN
        CALL MESS(LPT,1,'WARNING ** no atom card asks for factor labelled '//LABF)
      ENDIF
! NTYP IS -VE FOR ANOMALOUS, 0 FOR A SCALE AND +VE FOR A SCATTERING FACTOR:
      IF (NTYP) 11, 12, 13
! ANOMALOUS SCATTERING:
   11 ANOM = .TRUE.
      IF (NAMODE(LFAC).NE.0) THEN
        CALL ERRCH2(LABF,2,'reading 2nd anomalous scattering factor for',' ')
        GOTO 6
      ENDIF
      NAMODE(LFAC) = IABS(NTYP)
      IF (NAMODE(LFAC).EQ.1) GOTO 3
      WRITE (LPT,3003) NTYP, LABF
      IERR = IERR + 1
      GOTO 6
! RECORD MODE IN NAMODE, AND FOR NOW EXPECT ONLY 1 - THEN READ FDASH AND FDDASH
    3 I = NAMODE(LFAC)
      CALL RDREAL(XX,IPT,IPT,80,IER)
      IF (IER.NE.0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
        GOTO 6
      ENDIF
      IPKEEP = IPT
      CALL RDREAL(Y,IPT,IPT,80,IER)
      IF (IER.NE.0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
      ELSE
        FDASH(LFAC) = CMPLX(XX,Y)
      ENDIF
      GOTO 6
! SIMPLE SCALE FACTOR:
   12 CALL RDREAL(XX,IPT,IPT,80,IER)
      IF (IER.NE.0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
        GOTO 6
      ENDIF
      CMULT(LFAC) = CMULT(LFAC)*XX
      GOTO 6
! SCATTERING FACTOR:
   13 IF (NTYP.LE.5) GOTO 7
      WRITE (LPT,3002) NTYP, LABF
      IERR = IERR + 1
      GOTO 6
    7 IF (MODE(LFAC).EQ.0) GOTO 20
      CALL ERRCH2(LABF,2,'reading second scattering factor for',' ')
      GOTO 6
   20 MODE(LFAC) = NTYP
      GOTO (21,22,23,22,6), NTYP
! TYPE 1 - NEUTRON NUCLEAR SCATTERING - READ IN 1 MORE NUMBER:
   21 CALL RDREAL(XX,IPT,IPT,80,IER)
      IF (IER.NE.0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
        GOTO 6
      ENDIF
      CMULT(LFAC) = CMULT(LFAC)*XX
      GOTO 6
! HERE ON TYPE 2 - EXPONENTIAL APPROXIMATION:
   22 IF (NT(LFAC).LT.9) GOTO 9
   15 CALL ERRCH2(LABF,2,'>9 coefficients for exponential approx for factor',' ')
      GOTO 6
! READ HOWEVER MANY NUMBERS THERE ARE ON THE CARD:
    9 IPKEEP = IPT
      CALL RDREAL(XX,IPT,IPT,80,IER)
! IF IER IS RETURNED AS 100 WE HAVE READ ALL SPACES:
      IF (IER.EQ.100) GOTO 10
      IF (IER.NE.0) THEN
        CALL ERRIN2(IPKEEP,2,'cannot read number from "F" card at point',' ')
        GOTO 6
      ENDIF
      NT(LFAC) = NT(LFAC) + 1
      IF (NT(LFAC).GT.9) GOTO 15
      F(NT(LFAC),LFAC) = XX
      IF (IPT.LT.80) GOTO 9
! END OF NUMBERS ON CARD - CHECK THERE ARE MORE CARDS:
   10 IF (NFREAD.GE.NCARD) GOTO 16
      LSAV = LABF
      CALL INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      NFREAD = NFREAD + 1
      ID = ID + NYZ
      IF (NTYP.NE.2) GOTO 5
      IF (LSAV.NE.LABF) GOTO 5
! WE HAVE ANOTHER CARD FOR THE SAME FACTOR:
      GOTO 9
! HERE ON TYPE 3 - READ POSSIBLY SEVERAL MORE F CARDS
   23 IF (NT(LFAC).LT.40) GOTO 8
   35 CALL ERRCH2(LABF,1,'more than 40 entries in table for factor',' ')
      GOTO 6
! READ HOWEVER MANY NUMBERS THERE ARE ON THE CARD:
    8 IPKEEP = IPT
      CALL RDREAL(XX,IPT,IPT,80,IER)
! IF IER IS RETURNED AS 100 WE HAVE READ ALL SPACES:
      IF (IER.EQ.100) GOTO 30
      IF (IER.NE.0) THEN
        CALL ERRIN2(IPKEEP,2,'cannot read number from "F" card at point',' ')
        GOTO 6
      ENDIF
      IF (NT(LFAC).EQ.0) LL = 1
      LL = -LL
      IF (LL.GT.0) GOTO 33
      NT(LFAC) = NT(LFAC) + 1
      IF (NT(LFAC).GT.40) GOTO 35
      S(NT(LFAC),LFAC) = XX
      F(NT(LFAC),LFAC) = 9999.
      IF (NT(LFAC).EQ.1) GOTO 36
      IF (S(NT(LFAC),LFAC).GT.S(NT(LFAC)-1,LFAC)) GOTO 36
      WRITE (LPT,3011) NT(LFAC), S(NT(LFAC),LFAC), S(NT(LFAC)-1,LFAC), LABF
      GOTO 6
   33 F(NT(LFAC),LFAC) = XX
   36 IF (IPT.LT.80) GOTO 8
!
! END OF NUMBERS ON CARD - CHECK THERE ARE MORE:
   30 IF (NFREAD.GE.NCARD) GOTO 16
      LSAV = LABF
      CALL INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      NFREAD = NFREAD + 1
      ID = ID + NYZ
      IF (NTYP.NE.3) GOTO 5
      IF (LSAV.NE.LABF) GOTO 5
! WE HAVE ANOTHER CARD FOR THE SAME FACTOR:
      GOTO 8
! HERE WHEN CARD HAS BEEN DEALT WITH - ARE THERE MORE?
    6 IF (NFREAD.LT.NCARD) GOTO 2
! NOW WRITE OUT ALL INFORMATION ASSIMILATED:
   16 CALL MESS(LPT,1,'Scattering factors read for given atomic positions:')
      CALL MESS(LPT,0,'Name  Type          Description')
! SCAN ALL FACTORS AS DECODED BY ATOPOS:
      DO LFAC = 1, NFKEEP
        IF (MODE(LFAC).LE.0) THEN
          CALL ERRCH2(FONAME(LFAC),1,'factor','needed but not read')
          GOTO 41
        ENDIF
        M = MODE(LFAC)
        GOTO (43,44,45,46,47), M
   43   WRITE (LPT,2001) FONAME(LFAC), M, CMULT(LFAC)
 2001   FORMAT (/1X,A4,I5,'  Neutron nuclear scattering factor value', F10.4)
        GOTO 40
   44   N1 = NT(LFAC)
        WRITE (LPT,2002) FONAME(LFAC), M, N1, CMULT(LFAC), (F(I,LFAC),I=1,N1)
        GOTO 40
   45   N1 = NT(LFAC)
        WRITE (LPT,2003) FONAME(LFAC), M, N1, CMULT(LFAC), (S(I,LFAC),F(I,LFAC),I=1,N1)
 2003   FORMAT (/1X,A4,I5,'  Form factors given in table with',I5,  ' entries multiplied by',F10.4/11X,                     &
     &          'Table is:'/(1X,5(F10.2,F10.4)))
        IF (F(NT(LFAC),LFAC).NE.9999.) GOTO 40
        CALL MESS(LPT,1,'WARNING ** an odd number of numbers was read as the table of S and F')
        GOTO 40
   46   N1 = NT(LFAC)
        WRITE (LPT,2002) FONAME(LFAC), M, N1, CMULT(LFAC), (F(I,LFAC),I=1,N1)
        CALL MESS(LPT,0, '          Factor is finally multiplied by s squared')
        GOTO 40
   47   WRITE (LPT,2030) FONAME(LFAC), M
 2030   FORMAT (/1X,A4,I5,'  Form factor to be calculated from radial',' wave functions given on W RADF cards')
        GOTO 40
! WRITE ANOMALOUS SCATTERING FACTOR IF PRESENT:
   40   IF (NAMODE(LFAC).NE.0) WRITE (LPT,2004) FONAME(LFAC), FDASH(LFAC)
 2004   FORMAT (' ',A4,' also has anomalous scattering factor - coeff','icients:',2F10.4)
! FORM LIST OF ATOM LABELS TO WHICH THIS FACTOR APPLIES:
        IPT = 0
        ICARD = ' '
        DO I = 1, NATOM
          IF (NFORMF(I).NE.LFAC) GOTO 48
! ALSO, FILL IN TABLES FOR GROUPS OF ATOMS:
          NAFPNT(LFAC) = NAFPNT(LFAC) + 1
          NATFOR(LFAC,NAFPNT(LFAC)) = I
! PUT ATOM NAME INTO BUFFER IF IT WILL FIT:
          IF (IPT.GT.54) THEN
            WRITE (LPT,2005) (ICARD(IJ:IJ),IJ=1,LENGT(ICARD))
            ICARD = ' '
            IPT = 0
          ENDIF
          LEN = LENGT(ATNAME(I))
          ICARD(IPT+1:IPT+LEN) = ATNAME(I)(1:LEN)
          IPT = IPT + LEN + 1
   48   ENDDO
        IF (IPT.NE.0) WRITE (LPT,2005) (ICARD(IJ:IJ),IJ=1,LENGT(ICARD))
   41 ENDDO
  100 RETURN
 3003 FORMAT (/' ERROR ** type',I4,' anomalous scattering ',            &
     &        'factor not recognised for factor labelled ',A4)
 3002 FORMAT (/' ERROR ** type',I4,' scattering factor not',' recognised for factor labelled ',A4)
 3011 FORMAT (/' ERROR ** TERM',I5,' OF TABLE IS',F10.4,                &
     &        ' AND IS LESS THAN THE PREVIOUS TERM',F10.4,' FOR FACTOR ',A4)
 2002 FORMAT (/1X,A4,I5,' Series expansion form factor with',I4,        &
     &        ' terms multiplied by',F10.4/11X,'Coefficients are:'/11X,9F10.4)
 2005 FORMAT (12X,'applies to ',80A1)

      END SUBROUTINE SETFOR
!
!*****************************************************************************
!
      SUBROUTINE SETGEN(S)
!
! *** SETGEN corrected by JBF 15 Aug 94 ***
!
!X
!C 3A
!H Sets up the generation of a complete set of reflection indices.
!A On entry S is the maximum value of Sin(theta)/lambda required.
!P SYMUNI to define the asymmetric unit
!D Uses the defined asymmetric unit and given S to set up COMMON /HKLGEN/
!D ready for repeated entries to GETGEN.  GETGEN will generate all hkl's in
!D the given asymmetric unit with sin(theta)/lambda < S.
!
!D S is put into COMMON /BRAGG/ here.
!
      DIMENSION TEMP(3)
      LOGICAL ONCARD
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      COMMON /HKLGEN/ STEP(3,3), PT(3,3), VECEND(3,3), PRPT(3,3),       &
     &                NPRIM(2,2), NP, LFAC(2), MCOUNT(2), KOM5
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      STHLMX = S
      IF (S.NE.0.) GOTO 5
! IF S ZERO, TRY TO FIND "SMAX" ON AN I CARD:
      IF (.NOT.ONCARD('I','SMAX',STHLMX)) THEN
! NEED SMAX:
        CALL ERRMES(2,1,'max sin theta/lambda to generate hkl values')
        GOTO 100
      ENDIF
!     SET UP DEFAULT VALUES
    5 CALL GMZER(PT,3,3)
      CALL GMZER(STEP,3,3)
      CALL GMZER(PRPT,3,3)
!     SWITCH ACCORDING TO THE NUMBER OF SYMMETRY PLANES:
      N1 = NASYM + 1
      GOTO (1,2,3,4), N1
! NO PLANES:
    1 CALL GMUNI(STEP,3)
      GOTO 10
! ONE PLANE:
    2 CALL GMEQ(ASYM(1,1),STEP(1,3),1,3)
      CALL INVENT(STEP(1,3),STEP(1,3),STEP(1,1))
! THAT MIDDLE VALUE OF STEP IS JUST SOME VECTOR NOT IN THE PLANE
      CALL INVENT(STEP(1,3),STEP(1,1),STEP(1,2))
      GOTO 10
! TWO PLANES:
    3 CALL GMEQ(EDGE(1,3),STEP(1,3),1,3)
      CALL INVENT(ASYM(1,1),STEP(1,3),STEP(1,1))
      CALL INVENT(ASYM(1,2),STEP(1,3),STEP(1,2))
      GOTO 10
! THREE PLANES:
    4 CALL GMEQ(EDGE,STEP,3,3)
! JOIN - DETERMINE VOLUME OF CELL
   10 DO I = 1, 3
        CALL FCTOR(STEP(1,I),N)
      ENDDO
      NP = NINT(DETER3(STEP))
      IF (NP.GT.0) GOTO 11
      NP = -NP
      CALL GMEQ(STEP(1,1),TEMP,1,3)
      CALL GMEQ(STEP(1,2),STEP(1,1),1,3)
      CALL GMEQ(TEMP,STEP(1,2),1,3)
! SET UP LIMITS AND STARTING VALUES:
   11 J = 2
      K = 3
      DO I = 1, 3
        CALL VECPRD(STEP(1,J),STEP(1,K),VECEND(1,I))
        ALNGTH = 2.*STHLMX*VCTMOD(1.,VECEND(1,I),1)
        LENGTH = IFIX(ALNGTH/N) + 1
        DO L = 1, 3
          VECEND(L,I) = VECEND(L,I)/ALNGTH
! DEPENDING ON THE NUMBER OF BOUNDING PLANES OF THE ASYMMETRIC
! UNIT, THE PREVIOUS POINT VECTOR STARTS AT 0 OR FAR END OF VECTOR:
          IF (NASYM.EQ.3) GOTO 15
          IF ((NASYM.EQ.2) .AND. (I.NE.3)) GOTO 15
          IF ((NASYM.EQ.1) .AND. (I.EQ.3)) GOTO 15
          PRPT(L,3) = PRPT(L,3) - FLOAT(LENGTH)*STEP(L,I)
   15   ENDDO
        J = K
        K = I
      ENDDO
      CALL GMSUB(PRPT(1,3),STEP(1,1),PRPT(1,1),3,1)
      CALL GMEQ(PRPT(1,3),PRPT(1,2),1,3)
! START POINT IN PT ALSO:
      CALL GMEQ(PRPT,PT,3,3)
      CALL PRMTIV
  100 RETURN

      END SUBROUTINE SETGEN
!
!*****************************************************************************
!
      SUBROUTINE SHFESD(J)
!
! *** SHFESD by JCM 7 May 86 ***
!
!X
!C 6C
!H During the application of LSQ shifts, calculates the shift and ESD for
!H a "redundant" variable.
!
!A On entry J points to the constraint information in /CONSTR/
!A            for this redundant variable (which is related to a set
!A            of basic variables by the given constraint J)
!
!D On exit  SHIFT is set to the linear combination of relevant shifts
!D                in basic variables,
!D          ESD is set to the square root of the sum of squares of their esds.
!
      INCLUDE "PARAMS.INC"
      
      COMMON /CONSTR/ JCONST, JROWPT(301), JCMAT(200), AMOUNT(200), NEXTJ

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB


      INTEGER MM(MaxBVar)
      INTEGER         MATPNT
      REAL                               BLSQ
      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
      EQUIVALENCE (MM(1),MATPNT(2))

      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      JROW = JROWPT(J)
      JNEXT = JROWPT(J+1) - 1
      SHIFT = 0.
      ESD = 0.
      DO M = JROW, JNEXT
        SHIFT = SHIFT + AMOUNT(M)*BLSQ(JCMAT(M))
        ESD = ESD + (AMOUNT(M)*DERIVB(JCMAT(M)))**2
      ENDDO
      ESD = SQRT(ESD)

      END SUBROUTINE SHFESD
!
!*****************************************************************************
!
      SUBROUTINE SPACE(NT,NSCR,NGENS)
!
! *** SPACE corrected by PJB  02-Nov-1994 ***
!
!x
!C 1A
!H Interprets symbols from an S card which are the space group name.
!A On entry NT is a string of characters representing a space group short name.
!A There must be at least one space between elements, e.g. P21 21 21.
!A On exit NSCR is the unit on which S cards have been temporarily written.
!A         NGENS is the number of generators.
!D Generates S cards for subequent interpretation by SYMOP.
!N This is not the best or simplest way of doing this, but it came from a
!N working program.
!N *** SPACE adapted from an anonymous program by JCM 22 Aug 92 ***
!
      CHARACTER*1 LATLET(7), NDIGS(7), SPALET(6)
      CHARACTER*4 FIRST, MID, LAST
      CHARACTER*16 NT
      CHARACTER*12 IN
      CHARACTER*4 NTEX(21)
      CHARACTER*4 TEXB(9)
      LOGICAL TRI, MONO, ORTHO, TETRA, HEXA, CUBIC, CENTRE
      LOGICAL AFACE, BFACE, CFACE, BODY, RHOMB, FACE, PRIM, SLASH, MINUS
      LOGICAL EMPTY(3), LETT1(3), OUTSID
      LOGICAL FIRDIG(6), FIRA, FIRB, FIRC, FIRM, FIRN, FIRD, FIR21
      LOGICAL MIDA, MIDB, MIDC, MIDD, MIDM, MIDN, MID2, MID21
      LOGICAL LASTA, LASTB, LASTC, LASTD, LASTM, LASTN, LAST2
      DIMENSION MGES(3,3,21), NET(3), NES(3), MGET(3,13), MSS(48,3,3),  &
     &          MTS(48,3), MSSS(3,3), MTSS(3), MTSA(3,3), NSV(3,18),    &
     &          NSW(3)
      EQUIVALENCE (FIRST,IN(1:4))
      EQUIVALENCE (MID,IN(5:8))
      EQUIVALENCE (LAST,IN(9:12))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      DATA NDIGS/'0', '1', '2', '3', '4', '5', '6'/
      DATA LATLET/'A', 'B', 'C', 'I', 'R', 'F', 'P'/
      DATA SPALET/'A', 'B', 'C', 'M', 'N', 'D'/
      DATA MGES/1, 3*0, 1, 3*0, 1, -1, 3*0, -1, 3*0, 1, 0, 1, 0, 2* - 1,&
     &     3*0, 1, 0, 1, 0, -1, 4*0, 1, 0, 1, 3*0, 2*1, 2*0, 2*1, 0, -1,&
     &     4*0, 1, -1, 3*0, -1, 3*0, -1, 1, 3*0, 1, 3*0, -1, 0, -1, 0,  &
     &     2*1, 3*0, -1, 0, -1, 0, 1, 4*0, 2* - 1, 3*0, 1, 3*0, 3* - 1, &
     &     0, 1, 4*0, -1, 1, 3*0, -1, 3*0, 1, 0, 1, 0, 1, 4*0, -1, 0,   &
     &     -1, 0, -1, 4*0, 2*1, 2*0, 2* - 1, 3*0, 2* - 1, 2*0, 2*1, 3*0,&
     &     1, -1, 3*0, 1, 3*0, 1, 0, 1, 0, 1, 4*0, 2*1, 3*0, -1, 3*0,   &
     &     -1, 0, -1, 0, -1, 4*0, -1/
! MGES HOLDS:
!   1:  1  0  0     2: -1  0  0    3:  0  1  1    4:  0  1  0    5:  0  1  0
!       0  1  0         0 -1  0       -1 -1  0       -1  0  0        0  0  1
!       0  0  1         0  0  1        0  0  1        0  0  1        1  0  0
!
!   6:  1  1  0     7: -1  0  0    8:  1  0  0    9:  0 -1  0   10:  0 -1  0
!      -1  0  0         0 -1  0        0  1  0        1  1  0        1  0  0
!       0  0  1         0  0 -1        0  0 -1        0  0 -1        0  0 -1
!
!  11: -1  0  0    12: -1 -1  0   13:  1  0  0   14:  0  1  0   15:  0 -1  0
!       0  1  0         1  0  0        0 -1  0        1  0  0       -1  0  0
!       0  0 -1         0  0 -1        0  0  1        0  0 -1        0  0  1
!
!  16:  1  0  0    17: -1  0  0   18: -1  0  0   19:  0  1  0   20:  1  0  0
!      -1 -1  0         1  1  0        0  1  0        1  0  0        0 -1  0
!       0  0 -1         0  0  1        0  0  1        0  0  1        0  0 -1
!
!  21:  0 -1  0
!      -1  0  0
!       0  0 -1
!
      DATA NTEX/3*'    ', '+1/8', '+1/6', '    ', '+1/4', '    ',       &
     &     '+1/3', '+3/8', 2*'    ', '+1/2', 3*'    ', '+2/3', '    ',  &
     &     '+3/4', '    ', '+5/6'/
      DATA TEXB/'  -Z', '  -Y', ' X-Y', '  -X', '    ', '   X', ' Y-X', '   Y', '   Z'/
      DATA MGET/0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 12, 12, 12, 0,&
     &     12, 12, 12, 0, 12, 12, 12, 0, 6, 6, 6, 0, 6, 6, 6, 0, 6, 6,  &
     &     6, 18, 6, 6/
! MGET HOLDS:
!           1:     0    0    0                8:    1/2  1/2  1/2
!           2:    1/2   0    0                9:     0   1/4  1/4
!           3:     0   1/2   0               10:    1/4   0   1/4
!           4:     0    0   1/2              11:    1/4  1/4   0
!           5:     0   1/2  1/2              12:    1/4  1/4  1/4
!           6:    1/2   0   1/2              13:    3/4  1/4  1/4
!           7:    1/2  1/2   0
!
      DATA NSV/6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 4, 0, 0, 8, 9, 9, 0, 6, &
     &     6, 6, 6, 6, 0, 6, 18, 0, 6, 18, 3, 6, 18, 6, 6, 18, 9, 12, 6,&
     &     9, 0, 12, 0, 0, 18, 0, 0, 0, 0, 0, 12, 0, 12, 12, 12/
! NSV HOLDS:
!           1:    1/4   0    0               10:    1/4  3/4  1/8
!           2:     0   1/4   0               11:    1/4  3/4  1/4
!           3:     0    0   1/4              12:    1/4  3/4  3/8
!           4:     0    0   1/6              13:    1/2  1/4  3/8
!           5:     0    0   1/3              14:     0   1/2   0
!           6:    3/8  3/8   0               15:     0   3/4   0
!           7:    1/4  1/4  1/4              16:     0    0    0
!           8:    1/4  1/4   0               17:     0   1/2   0
!           9:    1/4  3/4   0               18:    1/2  1/2  1/2

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

!     READ THE SYMBOL, DETERMINE BRAVAIS LATTICE AND CRYSTAL FAMILY.
      JS = 1
      JR = 0
! DEFAULT MONOCLINIC:
      MSYS = 2
      IN = '000000000000'
      MINUS = .FALSE.
      SLASH = .FALSE.
! PUT THE SYMBOL INTO UPPER CASE:
      CALL UPPER(NT)
      DO IS = 1, 16
        IF (NT(IS:IS).NE.' ') GOTO 10
      ENDDO
      CALL ERRMES(1,0,'No space group symbol')
      IF (IBMBER .NE. 0) RETURN
   10 NBR = NCFIND(NT(IS:IS),LATLET,7)
      IF (NBR.EQ.0) THEN
        CALL ERRCH2(NT(IS:IS),0,'lattice letter','not recognised')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      AFACE = (NBR.EQ.1)
      BFACE = (NBR.EQ.2)
      CFACE = (NBR.EQ.3)
      BODY = (NBR.EQ.4)
      RHOMB = (NBR.EQ.5)
      FACE = (NBR.EQ.6)
      PRIM = (NBR.EQ.7)
      OUTSID = .TRUE.
      DO I = IS + 1, 16
        IF (OUTSID .AND. NT(I:I).EQ.' ') GOTO 1
        IF (JS.LE.12) IN(JS:JS) = NT(I:I)
        IF (IN(JS:JS).EQ.'-') THEN
          IF (JS.NE.1) THEN
            CALL ERRMES(1,0,'Minus other than at start of first symbol')
            IF (IBMBER .NE. 0) RETURN
          ELSE
            MINUS = .TRUE.
            JS = JS - 1
          ENDIF
        ENDIF
        IF (IN(JS:JS).EQ.'/') THEN
          IF (JS.NE.2 .AND. JS.NE.3) THEN
            CALL ERRMES(1,0,'Slash other than within first symbol')
            IF (IBMBER .NE. 0) RETURN
          ELSE
            SLASH = .TRUE.
          ENDIF
        ENDIF
        IF (NT(I:I).EQ.' ') THEN
          OUTSID = .TRUE.
          IF (JS.LE.12) IN(JS:JS) = '0'
          JR = JR + 1
          JS = 4*JR
        ELSE
          OUTSID = .FALSE.
        ENDIF
        JS = JS + 1
        IF (NT(I:I).EQ.'3' .OR. NT(I:I).EQ.'6') MSYS = 5
    1 ENDDO
      DO J = 1, 3
        I = 4*J - 3
        EMPTY(J) = IN(I:I).EQ.'0'
        LETT1(J) = (NCFIND(IN(I:I),SPALET,6).GT.0)
      ENDDO
      IF (EMPTY(1)) THEN
        CALL ERRMES(1,0,'No space group symbols')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      DO I = 1, 6
        FIRDIG(I) = (FIRST(1:1).EQ.NDIGS(I+1))
      ENDDO
      FIRA = (FIRST(1:1).EQ.'A')
      FIRB = (FIRST(1:1).EQ.'B')
      FIRC = (FIRST(1:1).EQ.'C')
      FIRM = (FIRST(1:1).EQ.'M')
      FIRN = (FIRST(1:1).EQ.'N')
      FIRD = (FIRST(1:1).EQ.'D')
      FIR21 = (FIRST(1:2).EQ.'21')
      MIDA = (MID(1:1).EQ.'A')
      MIDB = (MID(1:1).EQ.'B')
      MIDC = (MID(1:1).EQ.'C')
      MIDD = (MID(1:1).EQ.'D')
      MIDM = (MID(1:1).EQ.'M')
      MIDN = (MID(1:1).EQ.'N')
      MID2 = (MID(1:1).EQ.'2')
      MID21 = (MID(1:2).EQ.'21')
      LAST2 = (LAST(1:1).EQ.'2')
      LASTA = (LAST(1:1).EQ.'A')
      LASTB = (LAST(1:1).EQ.'B')
      LASTC = (LAST(1:1).EQ.'C')
      LASTD = (LAST(1:1).EQ.'D')
      LASTM = (LAST(1:1).EQ.'M')
      LASTN = (LAST(1:1).EQ.'N')
!
      IF (FIRDIG(4)) MSYS = 4
      IF (MID(1:1).EQ.'3') MSYS = 6
      IF (MSYS.EQ.2 .AND. FIRST(1:1).GE.'2' .AND. MID(1:1).GE.'2') MSYS = 3
      IF (FIRDIG(1) .AND. EMPTY(2)) MSYS = 1
      TRI = (MSYS.EQ.1)
      MONO = (MSYS.EQ.2)
      ORTHO = (MSYS.EQ.3)
      TETRA = (MSYS.EQ.4)
      HEXA = (MSYS.EQ.5)
      CUBIC = (MSYS.EQ.6)
!       FOR MONOCLINIC SETTING WITH ONLY 1 FIELD, FORCE B AXIS UNIQUE:
      IF (MONO .AND. EMPTY(2)) THEN
        MID = FIRST
        FIRST = '    '
        EMPTY(2) = .FALSE.
      ENDIF
!       SELECTION OF GENERATORS:
      NG = 0
      KL = 0
      DO I = 1, 3
        NES(I) = 1
        NET(I) = 0
        DO J = 1, 3
          MTSA(I,J) = 0
        ENDDO
      ENDDO
! FORM FIRST 2 (DECMAL) DIGITS OF FIRST SYMBOL IN NDIG1 AND NSCRW1.  IF THEY
! WERE NOT DIGITS, THIS IS MEANINGLESS.  IF THERE IS NO SCREW NSCRW1 WILL BE
! EITHER -1 (IF / PRESENT) OR 0 (IF FIRST SYMBOL IS IN FACT A SINGLE DIGIT).
      NDIG1 = ICHAR(FIRST(1:1)) - ICHAR('0')
      NSCRW1 = ICHAR(FIRST(2:2)) - ICHAR('0')
      IF (TRI .OR. TETRA .OR. HEXA .OR. CUBIC) THEN
!        POINT GROUPS 1,-1,3,-3,4,-4,6,-6,4/M,6/M.
        NET(1) = NDIG1
        KL = 24*NSCRW1/NDIG1
        IF (MINUS) NET(1) = NDIG1 + 6
        IF (NDIG1.EQ.3) NG = 1
        IF (NDIG1.GE.3 .AND. NDIG1.LE.6 .AND. NSCRW1.GT.0) MTSA(1,3) = KL
        IF (EMPTY(2)) THEN
          NG = 1
          IF (SLASH) THEN
            NET(2) = 8
            IF (FIRST(4:4).EQ.'A' .OR. FIRST(3:3).EQ.'A') NES(2) = 2
            IF (FIRST(4:4).EQ.'N' .OR. FIRST(3:3).EQ.'N') NES(2) = 7
            NG = 2
          ENDIF
          GOTO 400
        ENDIF
      ENDIF
!       MONOCLINIC-ORTHORHOMBIC. ((OR MORE THAN ONE AXIS))
      DO I = 1, 3
        DO J = 1, 4
          L = 4*(I-1) + J
          M = L + 4
          IF (M.GT.12) M = M - 12
!      MONOCLINIC - ORTHORHOMBIC ACTUALLY STARTS HERE
          IF (MONO .OR. ORTHO) THEN
!   DETECT 2-FOLD AXIS NOT FOLLOWED BY A LETTER:
            IF (IN(L:L).EQ.'2' .AND. (NCFIND(IN(M:M),SPALET,6).EQ.0)) THEN
              NG = NG + 1
              NET(NG) = 29 - 9*I
              IF (IN(L+1:L+1).EQ.'1') NES(NG) = I + 1
              IF (NG.EQ.2 .AND. LAST(2:2).EQ.'1') MTSA(2,3) = 12
              IF (NG.EQ.2) GOTO 400
            ENDIF
!   WE HAVE A LETTER:
            LETL = NCFIND(IN(L:L),SPALET,6)
            IF (LETL.GT.0) THEN
              NG = NG + 1
              NET(NG) = 23 - 5*I
!* TURNS A, B OR C INTO 2, 3 OR 4:
              IF (LETL.GE.1 .AND. LETL.LE.3) NES(NG) = LETL + 1
              IF (IN(L:L).EQ.'N') NES(NG) = I + 4
              IF (IN(L:L).EQ.'D') NES(NG) = I + 8
            ENDIF
!       3. TETRAGONAL-HEXAGONAL-CUBIC
          ELSEIF (IN(M:M).GT.'1') THEN
            IF (I.NE.3 .OR. SLASH .OR. (CUBIC.AND.(LETT1(1).OR.EMPTY(3)))) THEN
              NG = NG + 1
              IF (NG.EQ.4) NG = 3
!  FIRST CYCLE  :
              IF (I.EQ.1) THEN
                IF (CUBIC) THEN
                  NET(1) = 5
                ELSEIF (MID2) THEN
                  NET(NG) = 36 - 4*MSYS
                  IF (MID21) NES(1) = 2
                ELSEIF (LETT1(2)) THEN
                  NET(NG) = 22 - MSYS
                ENDIF
!   SECOND CYCLE :
              ELSEIF (I.EQ.2) THEN
                IF (LAST2) NET(NG) = 21
                IF (LETT1(3)) NET(NG) = 19
                IF (CUBIC) THEN
                  IF (LAST2) NET(2) = 14
                  IF (MINUS) NET(2) = 15
                ENDIF
              ELSEIF (I.EQ.3) THEN
                IF (NCFIND(IN(M:M),SPALET,6).GT.0) NET(NG) = 8
                IF (IN(M:M).EQ.'2') NET(NG) = 2
              ENDIF
              IF (FIR21) NES(2) = 6
              IF (MID2 .AND. LAST2 .AND. I.EQ.1 .AND. NSCRW1.GT.0) MTSA(2,3) = 24 - MTSA(1,3)
              IF (.NOT.FIRDIG(3)) MTSA(1,3) = 0
              IF (NET(2).EQ.14 .AND. NSCRW1.GT.0) THEN
                MTSA(2,1) = 24 - KL
                MTSA(2,2) = KL
                MTSA(2,3) = KL
              ENDIF
              N = NCFIND(IN(M:M),LATLET,3)
              IF (N.GT.0) NES(NG) = N + 1
              IF (IN(M:M).EQ.'N') THEN
                NES(NG) = 4 + I
                IF (I.EQ.2) NES(NG) = 8
              ELSEIF (IN(M:M).EQ.'D') THEN
                NES(NG) = 14 - I
              ENDIF
              IF (LASTD .AND. CUBIC) THEN
                IF (FIRA) THEN
! GROUP 230 - I A 3 D:
                  NES(2) = 12
                ELSE
! GROUP 220 - I -4 3 D:
                  NES(2) = 13
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!       COMPLETE SYMMETRY OPERATIONS
  400 NGENS = NG
      DO K = 1, NG
        DO I = 1, 3
          MTS(K,I) = MGET(I,NES(K)) + MTSA(K,I)
          DO J = 1, 3
            MSS(K,I,J) = MGES(I,J,NET(K))
          ENDDO
        ENDDO
      ENDDO
      NSS = -1
      K = 0
  610 K = K + 1
      IF (K.GT.NG) GOTO 617
      L = 1
      IF (MSS(K,1,1)+MSS(K,2,2)+MSS(K,3,3).EQ.-3) NSS = K
      IF (MSS(K,1,1)+MSS(K,2,2)+MSS(K,3,3).EQ.3) NU = K
  616 IF (L.GT.NGENS) GOTO 610
      DO I = 1, 3
        ITN = MTS(K,I)
        DO J = 1, 3
          N = 0
          ITN = ITN + MSS(K,I,J)*MTS(L,J)
          DO M = 1, 3
            N = N + MSS(K,I,M)*MSS(L,M,J)
          ENDDO
          ITN = MOD(ITN+24,24)
          MSSS(I,J) = N
          MTSS(I) = ITN
        ENDDO
      ENDDO
      DO KK = 1, NG
        N = 0
        DO I = 1, 3
          DO J = 1, 3
            IF (MSSS(I,J).NE.MSS(KK,I,J)) N = 1
          ENDDO
        ENDDO
        IF (N.EQ.0) GOTO 618
      ENDDO
      NG = NG + 1
      DO I = 1, 3
        MTS(NG,I) = MTSS(I)
        DO J = 1, 3
          MSS(NG,I,J) = MSSS(I,J)
        ENDDO
      ENDDO
  618 L = L + 1
      IF (K.EQ.NSS) THEN
        DO I = 1, 3
          J = I + 1
          IK = I + 2
          IF (J.GT.3) J = J - 3
          IF (IK.GT.3) IK = IK - 3
!  REMOVE LATTICE TRANSLATIONS -  FIRST I THEN F,A,B,C:
          IF (BODY .AND. I.EQ.1) THEN
            IF (MTS(NSS,1).GE.12 .AND. MTS(NSS,2).GE.12 .AND. MTS(NSS,3).GE.12) THEN
              DO IJ = 1, 3
                MTS(NSS,IJ) = MTS(NSS,IJ) - 12
              ENDDO
            ENDIF
          ELSEIF (FACE .OR. NBR.EQ.I) THEN
            IF (MTS(NSS,J).GE.12 .AND. MTS(NSS,IK).GE.12) THEN
              DO II = 1, 3
                MTS(NSS,II) = MTS(NSS,II) - MGET(II,I+4)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      GOTO 616
!       DETERMINE CENTROSYMMETRY AND SHIFT VECTOR TO A CENTRE OF SYMM..
  617 CENTRE = (NSS.GT.0)
      DO I = 1, 3
! RE-USE OF ARRAY MTSA, WHICH WAS THE SCREW AXIS TRANSLATION:
        MTSA(1,I) = 0
      ENDDO
      IF (CENTRE) THEN
        DO K = 1, 3
          MTSA(1,K) = MOD((MTS(NSS,K)/2)+24,24)
        ENDDO
      ENDIF
!   DETERMINE THE REF. NUMBER SHIFT VECTOR TO AN ORIGIN OF I.T..
      IIS = 1
      DO I = 1, 3
        NSW(I) = 0
      ENDDO
! NOW SET MS TO PICK SPECIAL INDIVIDUAL VECTORS TO GET THE ORIGIN RIGHT:
      MS = 16
      IF (IIS.EQ.1) THEN
        IF (ORTHO) THEN
!          ORTHORHOMBIC         ********
          IF (FIRST(2:2).EQ.'1' .AND. MID(2:2).EQ.'1') THEN
! GROUP P21 21 2 (7 MORE ORTHO GROUPS & P42 N M ALSO HAVE MS=8):
            IF (LAST(2:2).EQ.'0') MS = 8
! GROUPS P (& I)21 21 21 (P N C 2 & A B M 2 ALSO HAVE MS=2):
            IF (LAST(2:2).EQ.'1') MS = 2
          ENDIF
! GROUPS P (& A & I)B A 2, PN A 21, PN N 2 (THE NEXT 5 ORTHO WITH MS=8):
!    (BUT NB THIS DOES TOO MANY WITH MIDA):
          IF ((MIDA.OR.(FIRN.AND.MIDN)) .AND. LAST2) THEN
            MS = 8
! GROUPS PC A 21 AND P (& A & I) M A 2 (THE ONLY 4 GROUPS WITH MS=1):
            IF (FIRM .OR. FIRC) MS = 1
          ENDIF
! GROUPS CM M A & CM C A (THE LAST 2 OF THE 8 ORTHOS WITH MS=8):
          IF (CFACE .AND. FIRM .AND. LASTA) MS = 8
! GROUPS A B M 2 & P N C 2 (P&I 21 21 21 HAVE BEEN SET ALREADY):
          IF ((FIRN.OR.FIRB) .AND. (MIDC.OR.MIDM) .AND. LAST2) MS = 2
! GROUPS I B C A AND I M M A (IA 3, F41 3 2 & I A 3 D ALSO HAVE MS=7):
          IF (BODY .AND. LASTA) MS = 7
! GROUP F D D 2 (THE ONLY GROUP WITH MS=6):
          IF (MIDD .AND. LAST2) MS = 6
        ELSEIF (TETRA) THEN
!                 TETRAGONAL         *********
          IF (.NOT.SLASH) THEN
! GROUP 80 : I41 (AND OTHERS WHICH GET RESET LATER)
            IF (BODY .AND. NSCRW1.EQ.1) MS = 9
            IF ((MID2.AND.LAST2) .AND. (NSCRW1.EQ.1.OR.NSCRW1.EQ.3)) THEN
! GROUPS 91,92 : P41 2 2, P43 2 2 (AND OTHERS WHICH GET RESET LATER):
              MS = 3
! GROUP 98 : I41 2 2 (96, P43 21 2 NEEDS MS=12 BUT IT IS NOT SET HERE):
              IF (BODY) MS = 12
            ENDIF
! THE FOLLOWING LOOP LOOKS FOR 1,2 AND 3 - AND, NB, 0, MEANING "NO DIGIT":
! THESE SET:
! MS=9 FOR GROUPS 90,113 : P 4 21 2, P-4 21 M (AND OTHERS RESET LATER)
! MS=10 JUST FOR GROUP 90 : P41 21 2 (THE ONLY GROUP WITH MS=10)
! MS=11 JUST FOR GROUP 94 : P42 21 2 (114, P-4 21 C NEEDS MS=11 BUT NOT HERE)
! MS=12 JUST FOR GROUP 96 : P43 21 2 (98, I41 2 2 NEEDS MS=12 BUT ALREADY HAS)
            IF (MID21) THEN
              DO I = 1, 4
                IF (NSCRW1.EQ.I-1) MS = 8 + I
              ENDDO
            ENDIF
            IF (LASTC) THEN
! GROUP 112 : P-4 2 C (AND 114 : P-4 21 C WHICH IS IMMEDIATELY RESET)
              IF (MID2) MS = 3
! GROUP 114 : P -4 21 C
              IF (MID21) MS = 11
            ENDIF
! GROUP I-4 2 D (THE ONLY GROUP WITH MS=13) (& OTHERS WHICH ARE LATER RESET):
            IF (LASTD) MS = 13
! GROUP 116 : P-4 C 2 (AND 120 : I -4 C 2 WHICH IS LATER RESET)
            IF (MIDC .AND. MINUS) MS = 3
            IF (MIDN) THEN
! GROUP P 42 N M (THE OTHER 8 FOR MS=8 ARE ORTHRHOMBIC):
              IF (NSCRW1.EQ.2) MS = 8
! GROUPS P 4 N C & P -4 N 2 (THERE ARE 3 MORE WITH MS=9):
              IF (NSCRW1.EQ.0) MS = 9
            ENDIF
! GROUP I 41 M D (THE ONLY GROUP WITH MS=15):
            IF (MIDM .AND. LASTD) MS = 15
          ENDIF
! GROUPS P 4 B M, P42 B C, I4 C M, I41 C D, P -4 B 2, I -4 C 2, P4/N B M,
!        P4/M B M, P42/N B C, P42/M B C & I 4/M C M (& I41/A C D WHICH IS RESET
!        LATER):
          IF (MIDB .OR. BODY .AND. MIDC) MS = 14
! GROUPS P42/N & P42/N N M:
          IF (FIRST(4:4).EQ.'N' .AND. (EMPTY(2).OR.MIDN)) MS = 14
! GROUPS P4/M N C & P4/N N C:
          IF (MIDN .AND. (FIRST(3:3).EQ.'M'.OR.FIRST(3:3).EQ.'N')) MS = 14
          IF (FIRST(4:4).EQ.'A') THEN
! GROUP 141 : I 41/A M D (THE ONLY GROUP WITH MS=17)
            IF (MIDM) MS = 17
! GROUP 142 : I 41/A C D (THE ONLY GROUP WITH MS=18)
            IF (MIDC) MS = 18
          ENDIF
        ELSE
! ALL TYPES EXCEPT ORTHO AND TETRA:
! GROUPS P31 1 2, P31 2 1, P32 1 2, P32 1 2 (THE ONLY GROUPS GIVING MS=4 OR 5):
          IF (FIRDIG(3) .AND. .NOT.EMPTY(2)) THEN
            IF (NSCRW1.EQ.1) MS = 4
            IF (NSCRW1.EQ.2) MS = 5
          ENDIF
! CUBIC GROUPS F41 3 2 & IA 3 & IA 3 D (2 ORTHO GROUPS ALSO HAVE MS=7):
          IF (FACE .AND. NSCRW1.EQ.1 .OR. BODY .AND. FIRA) MS = 7
        ENDIF
      ENDIF
!       APPLY THE SHIFT OF ORIGIN TO ALL OPERATIONS.
!
!* IIS IS ALWAYS 1 OR 2 NOW
      IF (CENTRE .OR. IIS.GT.0) THEN
        DO K = 1, 3
          IF (IIS.NE.1) NSV(K,MS) = 0
! MTSA HAS THE HALF ORIGIN SHIFT IN IT BY NOW:
          NSW(K) = NSV(K,MS) + MTSA(1,K) + NSW(K)
          NSW(K) = MOD(NSW(K)+24,24)
        ENDDO
        DO I = 1, NG
          DO J = 1, 3
            L = MTS(I,J)
            DO K = 1, 3
              L = L - (MSS(NU,J,K)-MSS(I,J,K))*NSW(K)
            ENDDO
            MTS(I,J) = MOD(L+144,24)
          ENDDO
        ENDDO
      ENDIF
!       NORMALIZATION OF CENTRING TYPE.
      DO I = 1, NG
        DO J = 1, 3
          K = MOD(J,3) + 1
          KI = MOD(K,3) + 1
          IF (BODY .AND. J.EQ.1) THEN
            IF (MTS(I,1).GE.12 .AND. MTS(I,2).GE.12 .AND. MTS(I,3).GE.12) THEN
              DO IJ = 1, 3
                MTS(I,IJ) = MTS(I,IJ) - 12
              ENDDO
            ENDIF
          ELSEIF (FACE .OR. NBR.EQ.J) THEN
            IF (MTS(I,K).GE.12 .AND. MTS(I,KI).GE.12) THEN
              DO II = 1, 3
                MTS(I,II) = MTS(I,II) - MGET(II,J+4)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
! OUTPUT:
!
! OUTPUT S CARD GENERATORS TO SCRATCH FILE:
      DO I = 1, NGENS
        WRITE (NSCR,5000) (TEXB(MSS(I,J,1)+MSS(I,J,2)*3+MSS(I,J,3)*4+5), NTEX(MTS(I,J)+1),J=1,3)
 5000   FORMAT ('S ',2(2A4,','),2A4)
      ENDDO
! ADJUST NGENS IF FURTHER S CARDS ARE TO BE WRITTEN
! CENTROSYMMETRIC OPERATOR:
      IF (CENTRE) THEN
        NGENS = NGENS + 1
        WRITE (NSCR,5001)
 5001   FORMAT ('S -X, -Y, -Z')
      ENDIF
! NON-PRIMITIVE LATTICES:
      IF (NBR.LT.7) NGENS = NGENS + 1
      IF (NBR.EQ.6) NGENS = NGENS + 2
      IF (AFACE .OR. FACE) WRITE (NSCR,5002)
 5002 FORMAT ('S X, 1/2+Y, 1/2+Z')
      IF (BFACE .OR. FACE) WRITE (NSCR,5003)
 5003 FORMAT ('S 1/2+X, Y, 1/2+Z')
      IF (CFACE .OR. FACE) WRITE (NSCR,5004)
 5004 FORMAT ('S 1/2+X, 1/2+Y, Z')
      IF (BODY) WRITE (NSCR,5005)
 5005 FORMAT ('S 1/2+X, 1/2+Y, 1/2+Z')
      IF (RHOMB) WRITE (NSCR,5006)
 5006 FORMAT ('S 2/3+X, 2/3+Y, 1/3+Z'/'S 1/3+X, 2/3+Y, 2/3+Z')
      REWIND (NSCR)
      RETURN
 4006 FORMAT (2X,I2,'.',1X,2(A4,A4,' ,'),A4,A4,4X,I2,'.',1X, 2(A4,A4,' ,'),A4,A4)

      END SUBROUTINE SPACE
!
!*****************************************************************************
!
      SUBROUTINE SPGNAM(N,NAME)
!
! *** SPGNAM by JCM 30 Aug 92 ***
!
!X
!C 1A
!H Decodes space group symbol or integer from S GRUP card
!A On entry N is the space group number,
!A            or, if 0, means that the group symbol is given.
!A On exit NAME is 16 characters of short space group symbol.
!P On entry the card is in /SCRACH/
!
      CHARACTER*16 NAME
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      CHARACTER*16 SGSYMB(230), TRIMON(15), ORTHO(59), TETRA(68)
      CHARACTER*16 TRIHEX(52), CUBIC(36)
      EQUIVALENCE (SGSYMB(1),TRIMON(1))
      EQUIVALENCE (SGSYMB(16),ORTHO(1))
      EQUIVALENCE (SGSYMB(75),TETRA(1))
      EQUIVALENCE (SGSYMB(143),TRIHEX(1))
      EQUIVALENCE (SGSYMB(195),CUBIC(1))
      DATA TRIMON/'P1', 'P-1', 'P2', 'P21', 'C2', 'PM', 'PC', 'CM',     &
     &     'CC', 'P2/M', 'P21/M', 'C2/M', 'P2/C', 'P21/C', 'C2/C'/
      DATA ORTHO/'P2 2 2', 'P2 2 21', 'P21 21 2', 'P21 21 21',          &
     &     'C2 2 21', 'C2 2 2', 'F2 2 2', 'I2 2 2', 'I21 21 21',        &
     &     'PM M 2', 'PM C 21', 'PC C 2', 'PM A 2', 'PC A 21', 'PN C 2',&
     &     'PM N 21', 'PB A 2', 'PN A 21', 'PN N 2', 'CM M 2',          &
     &     'CM C 21', 'CC C 2', 'AM M 2', 'AB M 2', 'AM A 2', 'AB A 2', &
     &     'FM M 2', 'FD D 2', 'IM M 2', 'IB A 2', 'IM A 2', 'PM M M',  &
     &     'PN N N', 'PC C M', 'PB A N', 'PM M A', 'PN N A', 'PM N A',  &
     &     'PC C A', 'PB A M', 'PC C N', 'PB C M', 'PN N M', 'PM M N',  &
     &     'PB C N', 'PB C A', 'PN M A', 'CM C M', 'CM C A', 'CM M M',  &
     &     'CC C M', 'CM M A', 'CC C A', 'FM M M', 'FD D D', 'IM M M',  &
     &     'IB A M', 'IB C A', 'IM M A'/
      DATA TETRA/'P4', 'P41', 'P42', 'P43', 'I4', 'I41', 'P-4', 'I-4',  &
     &     'P4/M', 'P42/M', 'P4/N', 'P42/N', 'I4/M', 'I41/A', 'P4 2 2', &
     &     'P4 21 2', 'P41 2 2', 'P41 21 2', 'P42 2 2', 'P42 21 2',     &
     &     'P43 2 2', 'P43 21 2', 'I4 2 2', 'I41 2 2', 'P4 M M',        &
     &     'P4 B M', 'P42 C M', 'P42 N M', 'P4 C C', 'P4 N C',          &
     &     'P42 M C', 'P42 B C', 'I4 M M', 'I4 C M', 'I41 M D',         &
     &     'I41 C D', 'P-4 2 M', 'P-4 2 C', 'P-4 21 M', 'P-4 21 C',     &
     &     'P-4 M 2', 'P-4 C 2', 'P-4 B 2', 'P-4 N 2', 'I-4 M 2',       &
     &     'I-4 C 2', 'I-4 2 M', 'I-4 2 D', 'P4/M M M', 'P4/M C C',     &
     &     'P4/N B M', 'P4/N N C', 'P4/M B M', 'P4/M N C', 'P4/N M M',  &
     &     'P4/N C C', 'P42/M M C', 'P42/M C M', 'P42/N B C',           &
     &     'P42/N N M', 'P42/M B C', 'P42/M N M', 'P42/N M C',          &
     &     'P42/N C M', 'I4/M M M', 'I4/M C M', 'I41/A M D',            &
     &     'I41/A C D'/
      DATA TRIHEX/'P3', 'P31', 'P32', 'R3', 'P-3', 'R-3', 'P3 1 2',     &
     &     'P3 2 1', 'P31 1 2', 'P31 2 1', 'P32 1 2', 'P32 2 1', 'R3 2',&
     &     'P3 M 1', 'P3 1 M', 'P3 C 1', 'P3 1 C', 'R3 M', 'R3 C',      &
     &     'P-3 1 M', 'P-3 1 C', 'P-3 M 1', 'P-3 C 1', 'R-3 M', 'R-3 C',&
     &     'P6', 'P61', 'P65', 'P62', 'P64', 'P63', 'P-6', 'P6/M',      &
     &     'P63/M', 'P6 2 2', 'P61 2 2', 'P65 2 2', 'P62 2 2',          &
     &     'P64 2 2', 'P63 2 2', 'P6 M M', 'P6 C C', 'P63 C M',         &
     &     'P63 M C', 'P-6 M 2', 'P-6 C 2', 'P-6 2 M', 'P-6 2 C',       &
     &     'P6/M M M', 'P6/M C C', 'P63/M C M', 'P63/M M C'/
      DATA CUBIC/'P2 3', 'F2 3', 'I2 3', 'P21 3', 'I21 3', 'PM 3',      &
     &     'PN 3', 'FM 3', 'FD 3', 'IM 3', 'PA 3', 'IA 3', 'P4 3 2',    &
     &     'P42 3 2', 'F4 3 2', 'F41 3 2', 'I4 3 2', 'P43 3 2',         &
     &     'P41 3 2', 'I41 3 2', 'P-4 3 M', 'F-4 3 M', 'I-4 3 M',       &
     &     'P-4 3 N', 'F-4 3 C', 'I-4 3 D', 'PM 3 M', 'PN 3 N',         &
     &     'PM 3 N', 'PN 3 M', 'FM 3 M', 'FM 3 C', 'FD 3 M', 'FD 3 C',  &
     &     'IM 3 M', 'IA 3 D'/

      IF (N.GT.0) THEN
        NAME = SGSYMB(N)
      ELSE
        I = 7
    1   IF (ICARD(I:I).EQ.' ') THEN
          I = I + 1
          IF (I.LE.80) GOTO 1
          NAME = ' '
        ELSE
          NAME = ICARD(I:I+15)
        ENDIF
      ENDIF

      END SUBROUTINE SPGNAM
!
!*****************************************************************************
!
      SUBROUTINE SUBSYM(ITAB)
!
! *** SUBSYM corrected by PJB 24-Oct-94 ***
!
!X
!C 1B
!H Replaces all the symmetry parameters by those of a subgroup.
!
!A On entry ITAB is a table of dimension NOPC in which ITAB(I)=+-1
!A               if element I (or the -ve element) is in the subgroup
!A          IABS(ITAB(I)) is not 1 otherwise
!A          ITAB(1)=-1 if the sub-group is non-centrosymmetric.
!P SYMOP
!D Replaces the full symmetry by the configurational symmetry of a
!D magnetic structure.
!D Stores all necessary quantities in the COMMON /OLDSYM/.
!N The routine SYMBAK may be used to restore the stored symmetry.
!
      DIMENSION ITAB(24), IGTAB(3), KTAB(24), MTAB(24), NTAB(24)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /OLDSYM/ OSYM(3,3,24), OTRANS(3,24), JNVERS(24), JNORD(24),&
     &                MOLTAB(24,24), IOGEN(3), NOPONT(24), NOPO, NCENTO, NOPCO
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

      CALL MESS(LPT,1, 'Full symmetry replaced by configurational symmetry')
! STORE NUMBERS
      NOPO = NOP
      NCENTO = NCENT
      NOPCO = NOPC
! COPY ALL SYMMETRY ELEMENTS:
      CALL GMEQ(SYM,OSYM,9,NOPC)
      CALL GMEQ(TRANS,OTRANS,3,NOPC)
      CALL JGMEQ(INVERS,JNVERS,1,NOPC)
      CALL JGMEQ(NORD,JNORD,1,NOPC)
      CALL JGMEQ(MULTAB,MOLTAB,24,NOPC)
      CALL JGMEQ(IGEN,IOGEN,1,3)
      CALL JGMEQ(ITAB,NTAB,1,NOPC)
!  IS THERE A CENTRE OF SYMMETRY
      NCENT = 1
      IF (ITAB(1).EQ.1) NCENT = 2
      CENTRC = (NCENT.EQ.2)
! COUNT ELEMENTS OF THE EXTRACTED SUBGROUP IN IS:
      IS = 1
! NOPONT(NEW ELEMENT)=OLD ELEMENT ADDRESS:
      NOPONT(1) = 1
! KTAB(OLD ELEMENT)=NEW ELEMENT ADDRESS:
      CALL JGMZER(KTAB,NOPC,1)
      KTAB(1) = 1
! COUNT DOWN ORDERS OF SYMMETRY ELEMENTS:
      DO M = 6, 2, -1
        IF (M.EQ.5) GOTO 8
        CALL JGMZER(MTAB,NOPC,1)
! COUNT ELEMENTS OF THE ORIGINAL GROUP:
        DO I = 2, NOPC
! PICK OUT ONLY ELEMENTS OF THE SUBGROUP:
          IF (IABS(NTAB(I)).NE.1) GOTO 1
! THEN PICK ONLY THOSE OF ORDER M:
          IF (MOD(IABS(JNORD(I)),100).NE.M) GOTO 1
          II = I
! COUNT ALL POWERS OF THIS ELEMENT UP TO M:
          DO K = 1, M - 1
            IF (NTAB(II).EQ.0) GOTO 9
            IS = IS + 1
            MORD = MOD(JNORD(II),100)
            IF (K.NE.1) THEN
              NORD(IS) = ISIGN(IABS(MORD)+100,MORD)
            ELSE
              NORD(IS) = MORD
            ENDIF
            IF (NTAB(II).EQ.-1) THEN
              NOPONT(IS) = -INVERS(II)
              KTAB(INVERS(II)) = IS
              NORD(IS) = -NORD(IS)
            ELSE
              NOPONT(IS) = II
              KTAB(II) = IS
            ENDIF
            NTAB(II) = 0
            MTAB(II) = NORD(IS)
    9       II = MOLTAB(II,I)
          ENDDO
    1   ENDDO
    8 ENDDO
! RECONSTITUTE SYMMETRY, NOW ONLY FOR SUBGROUP:
      NOPC = IS
      DO I = 2, NOPC
        IC = IABS(NOPONT(I))
        CALL GMEQ(OSYM(1,1,IC),SYM(1,1,I),3,3)
        CALL GMEQ(OTRANS(1,IC),TRANS(1,I),3,1)
!  WAS THERE AN INVERSION?
        IF (NOPONT(I).LT.0) THEN
          CALL GMREV(SYM(1,1,I),SYM(1,1,I),3,3)
          INVERS(I) = KTAB(IC)
        ENDIF
        INVERS(I) = KTAB(JNVERS(IC))
        DO J = 2, NOPC
          ISS = IABS(NOPONT(J))
          M = MOLTAB(IC,ISS)
          MULTAB(I,J) = KTAB(M)
        ENDDO
      ENDDO
      NOP = NOPC*NCENT
      CALL JGMEQ(NORD,KTAB,NOPC,1)
      IGTAB(1) = ITAB(1)*NOPC
      CALL GENELM(MTAB,IGTAB)
      CALL JGMEQ(IGTAB(2),IGEN,2,1)
      NGEN = 1
      IF (IGEN(2).NE.0) NGEN = 2

      END SUBROUTINE SUBSYM
!
!*****************************************************************************
!
      SUBROUTINE SYMBAK
!
! *** SYMBAK by PJB 1 May 92 ***
!
!X
!C 1B
!H Restores the original symmetry operators after a call to SUBSYM.
!
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /OLDSYM/ OSYM(3,3,24), OTRANS(3,24), JNVERS(24), JNORD(24),&
     &                MOLTAB(24,24), IOGEN(3), NOPONT(24), NOPO, NCENTO, NOPCO
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

      CALL MESS(LPT,1,'Restoring full symmetry')
! RESTORE NUMBERS
      NOP = NOPO
      NOPC = NOPCO
      NCENT = NCENTO
      CENTRC = (NCENT.EQ.2)
! COPY ALL SYMMETRY ELEMENTS:
      CALL GMEQ(OSYM,SYM,9,NOPC)
      CALL GMEQ(OTRANS,TRANS,3,NOPC)
      CALL JGMEQ(JNVERS,INVERS,1,NOPC)
      CALL JGMEQ(JNORD,NORD,1,NOPC)
      CALL JGMEQ(MOLTAB,MULTAB,24,NOPC)
      CALL JGMEQ(IOGEN,IGEN,1,3)

      END SUBROUTINE SYMBAK
!
!*****************************************************************************
!
      SUBROUTINE SYMCEN(NC)
!
! *** SYMCEN by JCM 11 Jul 83 ***
!
!X
!C 1A
!H A specialist routine used during the input of space group symmetry
!H to discover whether there is a centre of symmetry.
!
!P Called at the start of routine SYMGEN.
!
!D Takes the temporary copy of the space group as generated in the
!D scratch COMMON /SCRAT/ and scans it for a centre of symmetry at the
!D origin.
!D
!D Sets NCENT=1 for non-centrosymmetric, 2 for centrosymmetric.
!D      CENTRC=.TRUE. if centrosymmetric, .FALSE. otherwise
!D      NC=0 if non-centrosymmetric, or the number of the (-x,-y,-z)
!D           operator.
!D      NOPC=NOP/NCENT.
!
!O Writes its findings on unit LPT.
!
      CHARACTER*5 WORD(2)
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / TSYM(3,3,48), TTRANS(3,48), MLTAB(48,48), ITAB(24)&
     &                , JTAB(48), NNORD(48), D(3,3)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      DATA WORD/'Non-c', '    C'/

      NCENT = 1
      NC = 0
      CALL GMZER(ORIGIN,1,3)
      DO NO = 1, NOP
        DO I = 1, 3
          DO J = 1, 3
            IF (ABS(TSYM(I,J,1)+TSYM(I,J,NO)).GT..0001) GOTO 1
          ENDDO
        ENDDO
        IF (ABS(TTRANS(1,NO))+ABS(TTRANS(2,NO))+ABS(TTRANS(3,NO)).EQ.0.) GOTO 4
! CENTRE OF SYMMETRY FOUND NOT AT ORIGIN:
        CALL GMEQ(TTRANS(1,NO),ORIGIN,1,3)
        CALL GMSCA(ORIGIN,ORIGIN,0.5,1,3)
! FOR NOW, JUST KEEP TRANSLATION VECTOR ASSOCIATED WITH -X,-Y,-Z
        WRITE (LPT,2001) ORIGIN
 2001   FORMAT (/' Centre at:',3F10.4,' Fcs complex')
        GOTO 3
!  CENTRE OF SYMMETRY FOUND AT ORIGIN:
    4   NCENT = 2
        NC = NO
        GOTO 3
    1 ENDDO
    3 WRITE (LPT,2000) WORD(NCENT), NOP
 2000 FORMAT (/' ',A5,'entrosymmetric space group with',I3,' operator(s)')
      NOPC = NOP/NCENT
      CENTRC = NCENT.EQ.2
      CALL ERRCHK(1,NOPC,24,0,'symmetry operators')

      END SUBROUTINE SYMCEN
!
!*****************************************************************************
!
      SUBROUTINE SYMFRI
!
! *** SYMFRI updated  by JCM 14 Jul 86 ***
!
!X
!C 1A
!H Reads and interprets an item "FRIE" on an I card.
!
!P Called at the end of the specialist routine SYMTID, which is used to
!P tidy the symmetry operators towards the end of their input.
!
!D Finds the item "FRIE" if it is present, and reads a following integer.
!D Sets the LOGICAL FRIEDL to be TRUE if Friedel's law is to be assumed,
!D that h,k,l is equivalent to -h,-k-l.
!d
!I The item "I FRIE n" has n=0 for "do NOT assume Friedel's law".  The
!I default  is to assume the law.
!O Reports its findings on unit LPT.
!N used for non-centrosymmetric structures only.
!
      LOGICAL ONCARD
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC

      FRIEDL = .TRUE.
      IF (CENTRC) GOTO 100
! IF NON-CENTROSYMMETRIC, LOOK FOR I FRIE CARD:
      IF (ONCARD('I','FRIE',A)) THEN
! AND IF I FRIE CARD GIVEN, TAKE INSTRUCTION FROM IT:
        FRIEDL = (A.NE.0.)
        IF (FRIEDL) THEN
          CALL MESS(LPT,1,'Friedel''s law assumed - hkl equivalent to -h-k-l')
          GOTO 100
        ENDIF
      ENDIF
      FRIEDL = .FALSE.
      CALL MESS(LPT,1,'Friedel''s law NOT assumed - hkl distinct from -h-k-l')
  100 RETURN

      END SUBROUTINE SYMFRI
!
!*****************************************************************************
!
      SUBROUTINE SYMGEN
!
! *** SYMGEN by PJB/JCM 8 Jul 83 ***
!
!X
!C 1A
!H Produces the generators of a space group which has been read by SYMOP.
!
!P The entire group must be in the COMMON /SCRAT/, with TSYM holding
!P the rotation matrices and TRANS the corresponding translation vectors.
!P /SCRAT/ also holds the temporary multiplication table for operators,
!P MULTAB.
!
!D SYMGEN has 4 main sections.  The first has been separated off as routine
!D SYMCEN, and the last as SYMTID.
!D
!D SYMCEN identifies a centre of symmetry at the origin if there is one.
!D
!D Part 2 fills in the vector NNORD as a temporary list of the orders
!D of each of the operators.
!D Various elements of NNORD are set to zero:
!D       firstly, all those centrosymetrically related to others,
!D       next, for those of orders 3,4 or 6, zeros are inserted for
!D       the element cubed, to the power 4, etc.
!D Part 3 sets up the pointer vector ITAB, 1xNOPC sized, and JTAB
!D 1xNOP sized.  It makes a list of the generators of the group in IGEN,
!D counting them in NGEN.  A new numbering of operators in made in JTAB,
!D and used to index ITAB.  NUMOP counts up as elements of the group are
!D generated, stopping at NOPC.
!
!D SYMTID tidies the information out of the temporary arrays in /SCRAT/
!D into /SYMDA/ and /SYMTAB/, and sets up a table of pointers to
!D invers elements, INVERS.
!
      LOGICAL FIRST
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / TSYM(3,3,48), TTRANS(3,48), MLTAB(48,48), ITAB(24)&
     &                , JTAB(48), NNORD(48), D(3,3)
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! FIRST LOOK FOR CENTRE OF SYMMETRY:
      CALL SYMCEN(NC)
      IF (IBMBER .NE. 0) RETURN
! CLEAR TABLE OF ORDERS OF ELEMENTS:
      DO N = 1, NOP
        NNORD(N) = 1
      ENDDO
! DISCOVER ORDERS & FILL IT TABLE, REJECTING CENTROSYMMETRICALLY
! RELATED OPERATORS, AND REJECTING HIGHER POWERS OF THOSE OF ORDER
! 3, 4 OR 6:
      NGEN = 1
      IGEN(1) = 1
      IF (NOP.EQ.1) GOTO 18
      DO NO = 2, NOP
        IF (NNORD(NO).EQ.0 .OR. NNORD(NO).GT.10) GOTO 1
! N WILL GIVE THE NUMBER OF THE LATEST GENERATED OPERATOR:
        N = 1
! M WILL GIVE THE ORDER:
        M = 1
   16   N = MLTAB(NO,N)
!  REJECT OPERATORS WHICH GENERATE A CENTRE:
        IF (N.EQ.NC) GOTO 1
! IF UNIT OPERATOR (NUMBER 1) HAS BEEN GENERATED, WE NOW HAVE THE
! ORDER OF OPERATOR NUMBER "NO":
        IF (N.EQ.1) GOTO 15
        M = M + 1
        GOTO 16
! FILL IT ORDERS VECTOR;  CANCEL C-RELATED IF THERE IS ONE:
   15   NNORD(NO) = M
        IF (CENTRC) NNORD(MLTAB(NC,NO)) = 0
        IF (M.LE.2) GOTO 1
! HOLD ORDER FOR HIGHER POWERS OF THOSE OF ORDER GREATER THAN 2
! WITH 100 ADDED
        N = NO
        DO J = 3, M
          N = MLTAB(NO,N)
          IF (CENTRC) NNORD(MLTAB(NC,N)) = 0
          JJ = J - 1
          IF (2*JJ.GT.M) JJ = M - JJ
          NNORD(N) = M
          IF (MOD(M,JJ).EQ.0) NNORD(N) = M/JJ
          NNORD(N) = NNORD(N) + 100
        ENDDO
    1 ENDDO
! PART 3:
! NOW FILL IN VECTORS ITAB AND JTAB TO GIVE THE CROSS-REFERENCES BETWEEN
! OLD AND NEW NUMBERINGS OF THE OPERATORS:
      NGEN = 0
      IGEN(1) = 0
   18 NUMOP = 1
      ITAB(1) = 1
      JTAB(1) = 1
! HERE MAY BE TRICLINIC: IF NOP=1 WE HAVE P1, AND WANT 1 GENERATOR BEING
! THE IDENTITY OPERATOR.  IF NOP=2 WE HAVE P-1, AND (BECAUSE OF THE
! WAY WE ARE STORING OPERATORS) WANT 0 GENERATORS.
      IF (NOPC.EQ.1) GOTO 101
! HERE AT LEAST MONOCLINIC; CLEAR REMAINING ITEMS IN VECTORS:
      IF (NOP.GT.1) CALL JGMZER(JTAB(2),1,NOP-1)
      IF (NOPC.GT.1) CALL JGMZER(ITAB(2),1,NOPC-1)
! NOW COUNT DOWN THROUGH ORDERS OF POSSIBLE SYMMETRY ELEMENTS:
      DO NN = 1, 5
        IORD = 7 - NN
!  OPERATORS OF ORDER 5 NOT ALLOWED
        IF (IORD.EQ.5) GOTO 2
        FIRST = .TRUE.
        DO NNN = 1, NOP
          IF (NNORD(NNN).NE.IORD) GOTO 3
!  WE HAVE FOUND AN OPERATOR OF ORDER IORD
!  HAS THIS OPERATOR BEEN USED ALREADY?
          IF (JTAB(NNN).NE.0) GOTO 3
! AND IS IT THE FIRST?
          IF (.NOT.FIRST) GOTO 4
!  THE FIRST OPERATOR OF THIS ORDER
          FIRST = .FALSE.
          M = NNN
          GOTO 9
!  HERE IF NOT FIRST:
    4     DO M = 1, NOP
            IF (MLTAB(ITAB(IGEN(NGEN)),M).EQ.NNN) GOTO 31
          ENDDO
          WRITE (LPT,3000) NNN, NGEN
          CALL BMBOUT
          RETURN
   31     IF (NNORD(M).EQ.0 .OR. NNORD(M).GT.10) GOTO 3
          IF (JTAB(M).NE.0) GOTO 3
!  MAKE IT A GENERATOR
    9     NGEN = NGEN + 1
          NUMOP = NUMOP + 1
          IGEN(NGEN) = NUMOP
          ITAB(NUMOP) = M
          JTAB(M) = NUMOP
          IF (NUMOP.GE.NOPC) GOTO 101
          NUM = NUMOP - 1
!  REMAKE MULTIPLICATION TABLE SAVING NEW ORDER
    7     NUM = NUM + 1
          J = ITAB(NUM)
          DO N = 1, NUM
            I = ITAB(N)
            MUL = MLTAB(I,J)
! IPERM ENSURES OPERATORS ARE COMBINED BOTH WAYS ROUND:
            DO IPERM = 1, 2
              IF (JTAB(MUL).NE.0) GOTO 8
              NUMOP = NUMOP + 1
              ITAB(NUMOP) = MUL
              JTAB(MUL) = NUMOP
              IF (NUMOP.GE.NOPC) GOTO 101
    8         MUL = MLTAB(J,I)
            ENDDO
          ENDDO
          IF (NUMOP.GT.NUM) GOTO 7
    3   ENDDO
    2 ENDDO
! PART 4 - TIDY UP INFORMATION INTO PERMANENT SPACE:
  101 CALL SYMTID(NC)
      RETURN
 3000 FORMAT (' ERROR ** in SYMGEN - operator',I4,' not found in table MLTAB;  no. of generators so far -',I4)

      END SUBROUTINE SYMGEN
!
!*****************************************************************************
!
      SUBROUTINE SYMOP
!
! *** SYMOP updated by JCM 30 Aug 92 ***
!
!X
!C 1A
!H Reads all the symmetry operators or a space group specification from
!H "S" cards, and generates the space group.
!
!D Reads all "S" cards and stores (ultimately):
!D      the space group rotation matrices in SYM in /SYMDA
!D      the translation vectors associated with them in TRANS in /SYMDA
!D      the lattice translation in ALAT in /SYMDA
!D      NOP=total number of operators, in /NSYM
!D      NOPC= the number of operators actually stored (half NOP if
!D            centrosymmetric with origin at centre), in /NSYM
!D      NLAT= the number of lattice vectors, in /NSYM
!D      NCENT=1 if non-centrosymmetric, 2 if centrosymmetric, in /NSYM
!D      NGEN= the number of generators of the space group, in /NSYM
!
!D Only the generating elements of the space group need be given.
!D The position X,Y,Z is assumed, and non-primitive lattice translations
!D are taken care of.
!D
!D Operators related by a centre are eliminated.
!
!I Reads all the "S" cards from the copy of the Crystal Data File held
!I on unit IO10.
!
!N The complete group of operators formed may be printed out using OPSYM.
!
      LOGICAL ONCARD, SGIVEN
      CHARACTER*16 SNAME
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / TSYM(3,3,48), TTRANS(3,48), MLTAB(48,48), R(3,3), T(3)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! INPUT NUMBER OF S CARDS:
      NNSYM = ICDNO(19)
! SET "S CARDS READ":
      INREAD(19) = -IABS(INREAD(19))
! INITIALISE AS THOUGH P1:
      NLAT = 1
      NOP = 1
      CALL GMZER(ALAT(1,1),1,3)
      CALL GMZER(TTRANS(1,1),1,3)
      CALL GMUNI(TSYM(1,1,1),3)
! ALAT HOLDS (UP TO 4) LATTICE TRANSLATION VECTORS
! TTRANS HOLDS (IN THIS FIRST STAGE) THE TRANSLATION VECTORS OF EACH OPERATOR
! TSYM HOLDS (IN THIS FIRST STAGE) THE ROTATION MATRICES OF EACH OPERATOR
      IF (NNSYM.EQ.0) CALL MESS(LPT,1,'No cards starting S have been read - assuming space group P1')
      ID = IABS(INREAD(19))
! FIRST, IS THERE AN S GRUP CARD
      SGIVEN = (ONCARD('S','GRUP',A))
      IF (SGIVEN) THEN
        CALL SPGNAM(NINT(A),SNAME)
        NSPC = NOPFIL(5)
        CALL SPACE(SNAME,NSPC,NGENS)
        NNSYM = NGENS
        ID = -NSPC
      ENDIF
! READ AND INTERPRET S CARDS ONE BY ONE:
      DO NCARD = 1, NNSYM
        CALL INPUTS(ID,R,T)
        IF (.NOT.SGIVEN) ID = ID + NYZ
        NO = NOP
! DISCOVER WHETHER R,T IS A NEW OPERATOR:
        CALL EQOP(R,T,NO,NLAT)
        IF (IBMBER .NE. 0) RETURN
        IF (NO.LE.NOP) GOTO 4
! IF HERE, NEW OPERATOR:
    5   NOP = NOP + 1
        DO N = 2, NOP
          N1 = NOP
          N2 = N
! USE IPERM TO TRY MULTIPLICATION BOTH WAYS ROUND:
          DO IPERM = 1, 2
            CALL MultiplyMatrices(TSYM(1,1,N1),TTRANS(1,N2),T,3,3,1)
            CALL MultiplyMatrices(TSYM(1,1,N1),TSYM(1,1,N2),R,3,3,3)
            CALL GMADD(T,TTRANS(1,N1),T,1,3)
            CALL FRAC3(T)
            NP = NO
            CALL EQOP(R,T,NP,NLAT)
            IF (IBMBER .NE. 0) RETURN
! FILL IN MULTIPLICATION TABLE - THIS IS WHY WE NEED TO KNOW WHICH OPERATOR
! IT WAS IF THERE IS A MATCH IN EQOP:
            MLTAB(N1,N2) = NP
            IF (NP.GT.NO) NO = NP
            IF (N1.EQ.N2) GOTO 6
            N1 = N
            N2 = NOP
          ENDDO
    6   ENDDO
! THE NEXT TEST REPEATS THE PROCESS ON ANY NEWLY CREATED
! OPERATORS - WE CANNOT ALTER NOP AT THE END OF A DO LOOP:
        IF (NO-NOP) 4, 4, 5
    4 ENDDO
! HERE WHEN ALL CARDS READ:
      DO N = 1, NOP
        MLTAB(1,N) = N
        MLTAB(N,1) = N
      ENDDO
      IF (NLAT.LT.2) GOTO 102
!     FORM COMPLETE TRANSLATION GROUP
      DO N = 2, NOP
        M1 = NLAT
        CALL MultiplyMatrices(TSYM(1,1,N),ALAT(1,2),T,3,3,1)
        CALL FRAC3(T)
        CALL EQPOS(ALAT,T,M1,M2,4)
        IF (IBMBER .NE. 0) RETURN
        IF (M2.GT.NLAT) NLAT = M2
      ENDDO
      J = 2
      I = 2
    1 M1 = NLAT
      CALL GMADD(ALAT(1,I),ALAT(1,J),T,1,3)
      CALL FRAC3(T)
      CALL EQPOS(ALAT,T,M1,M2,4)
      IF (IBMBER .NE. 0) RETURN
      IF (M2.GT.NLAT) NLAT = M2
      J = J + 1
      IF (J.LE.NLAT) GOTO 1
      I = I + 1
      J = I
      IF (I.LE.NLAT) GOTO 1
!  COMPLETE GROUP FORMED - NOW FIND GENERATORS
  102 IF (IERR.NE.0) THEN
        CALL ERRMES(1,0,'on S cards')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      CALL SYMGEN

      END SUBROUTINE SYMOP
!
!*****************************************************************************
!
      SUBROUTINE SYMREF(HIN,HOUT,NREFS,PHASE)
!
! *** SYMREF corrected by PJB (davinda) 22-Apr-1994 ***
!
!X
!C 1B
!H Generates a set of equivalent reflections and related phases.
!
!P The space group symmetry must be set up by a call of SYMOP
!
!A On entry HIN holds an array of 3 reals, h,k,l
!A On exit  HOUT is a 2-D real array of sets of related h,k,l values.
!A          NREFS is set to the number of generated sets (including h,k,l)
!A          PHASE holds an array of scalar products, hx+ky+lz, with h,k,l
!A                corresponding to the array HIN, and x,y,z being the
!A                translation vector corresponding to the symmetry operator
!A                which produced it (in fact, the negated vector for the
!A                inverse operator).
!
      DIMENSION HIN(3), HOUT(3,48), EH(3), PHASE(48), TR(3)
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22
!
      NREFS = 0
      DO J = 1, NCENT
        DO I = 1, NOPC
          CALL ROTSYM(HIN,EH,I,2)
          IF (J.EQ.2) CALL GMREV(EH,EH,3,1)
          CALL EQVEC(HOUT,EH,NREFS,M,NOP)
          IF (M.GT.NREFS) THEN
            NREFS = M
            CALL GMREV(TRANS(1,INVERS(I)),TR,3,1)
! I suppose this is right?
            IF (J.EQ.2) CALL GMREV(TR,TR,3,1)
            PHASE(M) = SCALPR(EH,TR)
          ENDIF
        ENDDO
      ENDDO

      END SUBROUTINE SYMREF
!
!*****************************************************************************
!
      SUBROUTINE SYMTID(NC)
!
! *** SYMTID by JCM 11 Jul 83 ***
!
!X
!C 1A
!H Tidies all the arrays connected with the space group symmetry.
!P A specialist routine called at the end of routine SYMGEN.
!
!D Copies out of the scratch COMMON /SCRAT/:
!D     rotation matrices TSYM to SYM in /SYMDA/
!D     translation vectors TTRANS to TRANS in /SYMDA/
!D     orders of elements NNORD into NORD in /SYMTAB/
!D     multiplication table from MLTAB to MULTAB in /SYMTAB/
!D
!D An inverting element is indicated by a -ve entry in NORD.
!D
!D Also sets up the constraints imposed on the cell parameters In
!D both spaces) by the symmetry.  This is done here in case the user
!D is about to use routines SCLPRD and VCTMOD, which refer to the
!D cell parameter quadratic products.  If RECIP has not yet been obeyed,
!D the actual cell parameters are not available, so sensible guesses
!D (consistent with the symmetry) are inserted.
!
!O Writes to unit LPT a brief report of the generators.
!
      COMMON /CELFIX/ IPTCEL(6), AMCELL(6), NCELF, NCELG, NCELS, KOM3
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / TSYM(3,3,48), TTRANS(3,48), MLTAB(48,48), ITAB(24)&
     &                , JTAB(48), NNORD(48), D(3,3)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),  KOM22

      CALL MESS(LPT,1,'The group is generated by the')
      IF (NGEN.GT.0) WRITE (LPT,2001) NGEN, (IGEN(I),I=1,NGEN)
 2001 FORMAT ('+',31X,I3,' element(s) numbered:',3I3)
      IF (CENTRC .AND. NOP.GT.2) CALL MESS(LPT,0,'and the')
      IF (CENTRC) CALL MESS(LPT,0,'centre of symmetry.')
!  STORE DEFINITIVE SYMMETRY OPERATORS  IN NEW SEQUENCE;  ALSO ORDERS OF
!  ELEMENTS :
      DO N1 = 1, NOPC
        CALL GMEQ(TSYM(1,1,ITAB(N1)),SYM(1,1,N1),3,3)
        CALL GMEQ(TTRANS(1,ITAB(N1)),TRANS(1,N1),1,3)
        NORD(N1) = NNORD(ITAB(N1))
        IF ((NORD(N1).EQ.0) .AND. (NC.GT.0)) NORD(N1) = NNORD(MLTAB(ITAB(N1),NC))
!  MAKE THE ORDERS OF INVERTING ELEMENTS NEGATIVE:
        IF (DETER3(SYM(1,1,N1)).LT.0.) NORD(N1) = -NORD(N1)
! STORE MULTIPLICATION TABLE AND INVERSES:
        DO N2 = 1, NOPC
          MULTAB(N2,N1) = JTAB(MLTAB(ITAB(N2),ITAB(N1)))
          IF (MULTAB(N2,N1).EQ.1) INVERS(N1) = N2
        ENDDO
      ENDDO
! SET UP CONSTRAINTS ON CELL PARAMETERS:
! CLEAR CHAIN INFORMATION FOR ALL CELL PARAMETERS
      IF (INREAD(3).LT.0) GOTO 101
      DO I = 1, 6
        IPTCEL(I) = 9999
      ENDDO
! CYCLE OVER ALL SYMMETRY LOOKING FOR RELATIONS
      IF (NOPC.EQ.1) GOTO 7
      DO NO = 1, NGEN
        CALL RELSM6(SYM(1,1,IGEN(NO)),IPTCEL,AMCELL)
      ENDDO
! PUT SUITABLE NUMBERS IN CPARS TO KEEP SCLPRD AND VCTMOD HAPPY EVEN IF RECIP
! NOT OBEYED BEFORE SYMUNI:
    7 DO I = 1, 2
        DO J = 1, 3
          CPARS(J,I) = 1.0
          CPARS(J+3,I) = 0.0
          IF (IPTCEL(J+3).NE.0 .AND. IPTCEL(J+3).LT.4) CPARS(J+3,I) = 0.5*FLOAT(2*I-3)
        ENDDO
      ENDDO
! READ I FRIE CARD IF RELEVANT, SETTING LOGICAL FRIEDL FOR "FRIEDEL TO HOLD":
  101 CALL SYMFRI

      END SUBROUTINE SYMTID
!
!*****************************************************************************
!
      SUBROUTINE SYMUNI
!
! *** SYMUNI updated by PJB 14 Jun 88 ***
!
!X
!C 1A
!H Selects a reciprocal space asymmetric unit fitting the symmetry.
!P SYMOP should have been obeyed, leaving NORD holding the orders of the
!P       symmetry operators, INVERS pointing at their inverse elements,
!P       FRIEDL set and the operators in SYM and TRANS.
!D Sets up in /FUNIT:
!D         NASYM planes bounding an asymmetric unit in ASYM
!D         EDGE to hold edges
!D         ANG to hold angles
!D         NMUL to hold fraction of reciprocal space used
!D By a call of UNITID makes the found unit hold the required typical h,k,l
!D whose default is 13,11,10 (i.e. all positive, h > k > l)
!D
!D By a call of POLUNI fills in /GUNIT with indicators used in finding the
!D multiplicity of a reflection.
!D
!D Builds up tentative unit in COMMON /SCRAT in ASY, communicating with other
!D routines like FIXUNI via /SCRAT.
!D
!D Works from a list of mirror planes in /SCRAT;  a unit may not contain A
!D mirror.  Notes also if point group is cubic, and takes those separately.
!D
!D Uses a list of axes of symmetry elements in AXI in /SCRAT.
!D If not cubic, and mirror planes are not enough to form a unit of the correct
!D size, uses such axes to form the remaining planes.  If there is only 1 axis,
!D takes an arbitrary plane containing it, and symmetry related planes.
!D
!D If there are more than one, the first is "principal" and must either be
!D perpendicular to a unit plane, or contained in it (all planes).  Planes
!D are taken through this axis and those of other elements in turn.
!D
!D Point group are dealt with as follows:
!D
!D Triclinic: 1 and -1 are singled out; -1 is given an arbitrary plane.
!D Cubic: 3 planes are needed, each containing two axes.  If no 4 axis,we have:
!D    either  23 for which we need three 3 axes (distinguished by NMUL=12)
!D    or      m3 for which we need one 3 and two 2's
!D
!D    If a 4 is present, then 4 3 3 will do for 432 and -43m,
!D           but m3m needs 4 3 2 (and is detected by having NMUL=48).
!D
!D Mirrors: use of mirrors as planes will finish:
!D       monoclinic: m
!D       orthorhombic: mm2 and mmm
!D       tetragonal: 4mm and 4/mmm
!D       trigonal 3m
!D       hexagonal: 6mm, -6m2 and 6/mmm
!D
!D Single axis: a number of planes through a single axis will finish:
!D       monoclinic: 2 and 2/m
!D       orthorhombic: no more
!D       tetragonal: 4, -4 and 4/m (which has also 1 mirror)
!D       trigonal: 3 and -3
!D       hexagonal:  6, -6, 6/m (the last two having 1 mirror also)
!D
!D Principal axis: planes through principal axis and a "2" axis finish:
!D       orthorhombic: 222
!D       tetragonal: 422 and -42m (which has 1 mirror also)
!D       trigonal: 32
!D       hexagonal: 622
!D
!D This leaves -3m, which belongs to the "principal axis" category above, but
!D because we have removed the centrosymmetrically related operators has no
!D explicit "2" axes left.  There are two ways of dealing with this - a plane
!D perpendicular to the principal axis, or a halving of the angle between
!D existing planes.  For the moment the former is taken.
!D
!O If the procedure outlined above does not produce a sensible outcome, writes
!O error mesages and stops.
!
!N This highlights a general point.  For most point groups it is evident how
!N many planes must be used to slice space into suitable units, but for a
!N subset of them (422, -42m, -3, 32, -3m, 622 : for laymen, those which:
!N          are non-cubic
!N          have at least 6 spots
!N          have both black and white spots
!N          but not superimposed
!N the space may be carved into two different sets of units, one with 3 faces
!N and one with 2.  The above methods will generally give preference to 2,
!N except in the case of -3m as described.
!N
!N The user may not get exactly the unit he wants for this reason.  He is
!N allowed to specify typical indices of a reflection to be inside the finished
!N unit, but this will not influence the carving in the cases mentioned.  I
!N have in mind to allow him also to offer a plane, so that, e.g., in
!N 422 if he offers the equator plane, he will get the broader wedge of space
!N cut off by that plane, instead of the narrower wedge "infinite" in both
!N directions.
!
      DIMENSION XAX(3), ZAX(3)
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / AXI(3,24,2), MIRROR(24), D(3,3), PL1(3), PL2(3),  &
     &                PL3(3), HT(3), ASY(3,4), NSTAT(4), NOPL, NICE,    &
     &                VOL, MOP1, MOP2
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
      DATA XAX, ZAX/1.0, 0., 0., 0., 0., 1./

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! READ IN POSSIBLE U CARD GIVING 3 TYPICAL INDICES REQUIRED BY USER TO
! BE WITHIN THE FINISHED ASYMMETRIC UNIT.  IF NO U CARD USE 13,11,10:
      CALL INPUTU(HT)
! CLEAR SPACE FOR FIXUNI TO BUILD UP NOPL PLANES IN ASY WITH STATUS NSTAT:
      NOPL = 0
      CALL GMZER(ASY(1,1),3,4)
      CALL JGMZER(NSTAT,1,4)
! NMUL=FRACTION OF UNIT INVOLVED AND MUST TAKE ACCOUNT OF FRIEDEL:
      NMUL = NOP
      IF (FRIEDL) NMUL = NOPC*2
! JUMP IF POINT GROUP 1, NO PLANES AT ALL REQUIRED:
      IF (NMUL.EQ.1) GOTO 102
! JUMP IF NOT POINT GROUP -1:
      IF (NOPC.GT.1) GOTO 2
! SPECIAL FOR P-1 OR P1 WITH FRIEDEL - PUT IN 0 0 1 AND ACCEPT:
      CALL FIXUNI(ZAX,1)
      IF (IBMBER .NE. 0) RETURN
      GOTO 102
! HERE FOR MONOCLINIC ONWARDS:
! SET UP AXES OF SYMMETRY ELEMENTS, AND MARK MIRROR PLANES IF THEY EXIST:
! NOTE LASTAX=NO. OF LAST ELEMENT OF LIST (IRRELEVANT ELEMENTS ARE ALREADY 0)
! NP2 = FIRST "2", NP3 = FIRST "3" (THERE ARE NO -3) AND NP4 = FIRST "4"
! AND COUNT "3" AXES IN N3, BEAUSE CUBIC HAS 4 OF THESE AND NOTHING ELSE HAS >1
! ALSO, NPP2 IS SET TO POINT TO THE FIRST +2 (NOT -2) FOR THE SPECIAL CASE -3M.
    2 LASTAX = 0
      N3 = 0
      NP2 = 0
      NPP2 = 0
      NP3 = 0
      NP4 = 0
      DO N = 2, NOPC
        MIRROR(N) = 0
        IF (IABS(NORD(N)).GT.100) THEN
          CALL GMZER(AXI(1,N,1),3,1)
          CALL GMZER(AXI(1,N,2),3,1)
          GOTO 3
        ENDIF
!  GET THE AXIS IN REAL SPACE
        CALL AXIS(SYM(1,1,N),AXI(1,N,1))
!  GET THE AXIS IN RECIPROCAL COORDINATES
        CALL GMEQ(SYM(1,1,INVERS(N)),D,3,3)
        CALL TRANSQ(D,3)
        CALL AXIS(D,AXI(1,N,2))
! PICK OUT USEFUL AXES:
        LASTAX = N
        IF (NORD(N).EQ.2) NPP2 = N
        I = IABS(NORD(N))
        IF (I.EQ.3) N3 = N3 + 1
        IF (I.EQ.2 .AND. NP2.EQ.0) NP2 = N
        IF (I.EQ.3 .AND. NP3.EQ.0) NP3 = N
        IF (I.EQ.4 .AND. NP4.EQ.0) NP4 = N
! MARK MIRRORS:
        IF (NORD(N).EQ.-6 .OR. NORD(N).EQ.-2) GOTO 4
        IF (.NOT.FRIEDL) GOTO 3
        IF (NORD(N).EQ.3) GOTO 3
    4   MIRROR(N) = 1
    3 ENDDO
! IF PRINCIPAL AXIS IS A "2" MOVE NP2 PAST IT:
      IF (NP2.EQ.2) NP2 = 3
! JUMP IF NOT CUBIC:
      IF (N3.NE.4) GOTO 5
! JUMP IF NO "4" AXIS:
      IF (NP4.EQ.0) GOTO 6
! PICK PLANE THROUGH 4 AND ANY 3:
      CALL VECPRD(AXI(1,NP4,2),AXI(1,NP3,2),PL1)
! AND INSIST ON KEEPING IT:
      CALL FIXUNI(PL1,2)
      IF (IBMBER .NE. 0) RETURN
! NOW DISTINGUISH BETWEEN M3M AND THE REST;  4 3 3 FOR M3M IS NOT SMALL ENOUGH:
      NSTART = NP3 + 1
      NS = 3
      IF (NMUL.LT.48) GOTO 19
      NSTART = NP2
      NS = 2
! SCAN REMAINING OPERATORS LOOKING ONLY AT SUITABLE AXES:
   19 DO J = NSTART, LASTAX
        IF (IABS(NORD(J)).NE.NS) GOTO 7
! MAKE 2 MORE PLANES, EACH THROUGH NEW AXIS AND ONE OF ORIGINAL:
        CALL VECPRD(AXI(1,J,2),AXI(1,NP4,2),PL1)
        CALL VECPRD(AXI(1,NP3,2),AXI(1,J,2),PL2)
        CALL FIXUNI(PL1,1)
        IF (IBMBER .NE. 0) RETURN
        CALL FIXUNI(PL2,1)
        IF (IBMBER .NE. 0) RETURN
        IF (NICE) 8, 102, 8
! IF NICE IS EVER 0 WE ARE HOME - OTHERWISE TAKE NEXT 3RD AXIS:
    8   CALL FIXUNI(PL1,-2)
        IF (IBMBER .NE. 0) RETURN
        CALL FIXUNI(PL2,-3)
        IF (IBMBER .NE. 0) RETURN
    7 ENDDO
!  SHOULD NOT GET HERE:
      GOTO 11
! CUBIC WITH NO 4 AXIS - IS IT 23 OR 3M?:
    6 IF (NMUL.NE.12) GOTO 20
! 23 - UNIT MUST HAVE THREE 3 AXES AS EDGES, ANY ANY 3 SHOULD DO:
      I = NP3
      J = I
   21 J = J + 1
      IF (J.GT.LASTAX) GOTO 11
      IF (NORD(J).NE.3) GOTO 21
      K = J
   22 K = K + 1
      IF (K.GT.LASTAX) GOTO 11
      IF (NORD(K).NE.3) GOTO 22
      CALL PLN3AD(I,J,K)
      IF (NICE) 11, 102, 11
! M3 - WANT ONE 3 AXIS AND TWO 2S:
   20 DO J = NP2, LASTAX
! PICK OUT PAIRS OF 2 AXES TO PUT WITH 3:
        IF (NORD(J).NE.2) GOTO 9
        IF (J.EQ.LASTAX) GOTO 11
        J1 = J + 1
        DO K = J1, LASTAX
          IF (NORD(K).NE.2) GOTO 39
          CALL PLN3AD(NP3,J,K)
          IF (NICE) 10, 102, 10
! IF DO NOT MAKE GOOD CELL, TAKE ALL 3 OUT AGAIN:
   10     CALL FIXUNI(PL1,-1)
          IF (IBMBER .NE. 0) RETURN
          CALL FIXUNI(PL2,-2)
          IF (IBMBER .NE. 0) RETURN
          CALL FIXUNI(PL3,-3)
          IF (IBMBER .NE. 0) RETURN
   39   ENDDO
    9 ENDDO
! SHOULD HAVE MADE GOOD UNIT BY NOW:
   11 WRITE (LPT,3000) NP2, NP3, NP4
      CALL BMBOUT
      RETURN
! NEITHER TRICLINIC NOR CUBIC - TRY ALL MIRRORS:
    5 DO N = 2, LASTAX
        IF (MIRROR(N).EQ.0) GOTO 12
        CALL FIXUNI(AXI(1,N,1),1)
        IF (IBMBER .NE. 0) RETURN
        IF (NICE) 12, 102, 12
   12 ENDDO
! MIRRORS INADEQUATE - PICK OUT THOSE WITH ONLY 1 SYMMETRY ELEMENT (OTHER THAN
! THE UNIT, A CENTRE, OR THOSE GENERATED FROM THE ONE):
      IF (LASTAX.NE.2) GOTO 13
! THIS ASSUMES THAT SUCH AN AXIS IS ELEMENT 2 - SO LONG AS WE HAVE COME IN VIA
! SYMOP, SYMGEN IT WILL BE:
! TAKE PLANE THROUGH AXIS AND X AXIS (OR Z AXIS):
   18 CALL VECPRD(AXI(1,2,2),XAX,PL1)
      IF (VCTMOD(1.0,PL1,1).LT.0.0001) CALL VECPRD(AXI(1,2,2),ZAX,PL1)
! MAKE THIS FIRST PLANE MANDATORY:
      CALL FIXUNI(PL1,2)
      IF (IBMBER .NE. 0) RETURN
      IF (NICE) 15, 102, 15
! NEED ANOTHER PLANE - SWING IT:
   15 DO I = 2, NOPC
        CALL ROTSYM(PL1,PL2,I,1)
        CALL FIXUNI(PL2,1)
        IF (NICE) 14, 102, 14
   14 ENDDO
! SHOULD BE ENOUGH:
      CALL ERRMES(-1,0,'in SYMUNI - single axis not enough')
      IF (IBMBER .NE. 0) RETURN
! HAVE PRINCIPAL AXES PLUS OTHERS - ARE THERE ANY 2'S?
   13 IF (NPP2.EQ.0) GOTO 16
! MAKE PLANES THROUGH PRINCIPAL AND A 2 AND OFFER IN TURN:
      DO J = NP2, LASTAX
        IF (NORD(J).NE.2) GOTO 17
        CALL VECPRD(AXI(1,2,2),AXI(1,J,2),PL1)
        CALL FIXUNI(PL1,1)
        IF (IBMBER .NE. 0) RETURN
        IF (NICE) 17, 102, 17
   17 ENDDO
! THIS CONTINUAL OFFERING DOES NOT REMOVE THE PLANES IT DOES NOT LIKE, AND
! MAY CAUSE TROUBLE.
! SHOULD HAVE BEEN ENOUGH - IF NOT ONLY CASE SHOULD BE CENTRE NOT AT ORIGIN:
      IF (.NOT.CENTRC) GOTO 18
      CALL ERRMES(-1,0,'in SYMUNI - principal axis plus 2 axes not enough')
      IF (IBMBER .NE. 0) RETURN
! WE SHOULD BE AT -3M HERE, WITH 2 MIRRORS, BUT UNIT TOO BIG.  HALVE IT:
   16 CALL FIXUNI(AXI(1,2,1),1)
      IF (IBMBER .NE. 0) RETURN
      IF (NICE.NE.0) THEN
        CALL ERRMES(-1,0,'reached end of SYMUNI')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
! SUCCESSFUL UNIT - TIDY IT AND TRANSFORM TO HOLD TYPICAL REFLECTION:
  102 CALL UNITID
! FINAL UNIT - MARK EDGES FOR MULTIPLICITY:
      CALL POLUNI
      RETURN
 3000 FORMAT (/' *** PROGRAM ERROR in SYMUNI - all cubic axes scanned',' - NP2, NP3, NP4=',3I3)

      END SUBROUTINE SYMUNI
!
!*****************************************************************************
!
      SUBROUTINE TBLFND(NAME,IANS,IFAM,IGEN,ISPC,KP,KS)
!
! *** TBLFND updated by JCM 8 May 90 ***
!
!X
!C 6C
!H Looks for an A4 NAME in every table it can find, trying to identify it as
!H part of a LSQ parameter name.
!A On entry NAME is the target name to find
!A On exit IANS gives the answer:
!A  IANS=0 means cannot find NAME anywhere
!A  IANS=large -ve means that NAME is not part of a parameter specification,
!A        but some other word recognised on a FIX/VARY card, like:
!A        'ONLY' (-99) or 'ALL' (-100)
!A  IANS=small -ve means that NAME is a word recognised from the user's
!A        table, and IANS has been picked up from the user's parallel table
!A        table of small -ve integers or parameter specs
!A  IANS=+ve means that NAME was part of a parameter specification;  it may
!A        be a family, genus or species name, and from it may have been
!A        deduced other things (like family name from genus name)
!A
!A        In this case, as many of IFAM, IGEN and ISPC as can be set on exit
!A        are set.
!
!P Expects LSQ to have been set up by LSETUP;  if structure parameters are to
!P be LSQ parameters, expects tables of atom names in /ATNAM and form factor
!P names in /FONAM
!P
!P If Multiphase, expects KPHASE to hold phase required for this one word.
!
      CHARACTER*4 NAME, ITB(2)
      DIMENSION ITBSPC(2)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
      COMMON /FONAM / FONA(20,9), FONAME(20)
      CHARACTER*4 FONAME, FONA
      COMMON /FORMDA/ NFORMF(150), MODE(20), NT(20), F(40,20), S(40,20),&
     &                CMULT(20), KCMULT(150), NBAKF(20), NUMFNM, KOM7
      COMMON /FORMD2/ NBKF(20,9), NMFNM(9)
      INTEGER         NINIT, NBATCH, NSYSTM
      LOGICAL                                MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /WDSPC / IWDNUM, IWDSPC(60)
      COMMON /WORDS / LSQWD(60)
      CHARACTER*4 LSQWD
      DATA ITB/'ALL', 'ONLY'/
      DATA ITBSPC/ - 100, -99/

! CLEAR ANSWERS:
      IFAM = 0
      IGEN = 0
      ISPC = 0
      KP = 0
      KS = 0
! FIRST TRY TO FIND IN MAIN WORD TABLE WHICH WAS SET UP IN THE MAIN PROGRAM TO
! SPECIFY THE PROBLEM:
      L = NCFIND(NAME,LSQWD,IWDNUM)
      IF (L.GT.0) THEN
! FOUND IN WORD TABLE - PARALLEL ARRAY IWDSPC SHOULD BE PACKED INFO
! AS TO WHAT THIS WORD ACTUALLY IS:
        IANS = IWDSPC(L)
        IF (IANS.GT.0) CALL KUNPAK(IANS,IFAM,IGEN,ISPC,KP,KS)
        GOTO 100
      ENDIF
! NEXT TRY TBLFND'S OWN TABLE OF USEFUL WORDS APPLICABLE TO MOST LSQ:
      L = NCFIND(NAME,ITB,2)
      IF (L.GT.0) THEN
        IANS = ITBSPC(L)
        GOTO 100
      ENDIF
! TRY DIGITS (SPECIES NAMES INVOLVED IN FAMILY 1, OR 3 ETC. ):
      N = 0
      DO J = 1, 4
! A DIGIT WILL BE LEFT-JUSTIFIED:
        IF (NAME(J:J).NE.' ') THEN
          DO I = 1, 10
            IF (NAME(J:J).EQ.IDIGIT(I)) GOTO 3
          ENDDO
        ENDIF
        IF (N.EQ.0) GOTO 5
        GOTO 9
    3   IF (I.EQ.10) I = 0
        N = 10*N + I
      ENDDO
    9 ISPC = N
! GO TO PACK INTO IANS
      GOTO 102
! OTHER WORDS REFER TO FAMILY 2 OR MORE - CHECK THERE MAY BE SOME:
    5 IF (NFAM.LT.2) GOTO 101
      IF (MULFAS) THEN
        L = NCFIND(NAME,ATNA(1,KPHASE),NATO(KPHASE))
      ELSE
        L = IATOM(NAME)
      ENDIF
      IF (L.GT.0) THEN
        IGEN = L
        IFAM = 2
        GOTO 102
      ENDIF
! MAY BE SCATTERING FACTOR NAME:
      IF (MULFAS) THEN
        ISC = NCFIND(NAME,FONA(1,KPHASE),NMFNM(KPHASE))
      ELSE
        ISC = ISCAT(NAME)
      ENDIF
      IF (ISC.GT.0) IGEN = NBKF(ISC,KPHASE)
      IF (IGEN.EQ.0) GOTO 101
      ISPC = 10
      IFAM = 2
! IFAM, IGEN, ISPC OK - PACK INTO IANS:
  102 IANS = KPAK(IFAM,IGEN,ISPC,KPHASE,0)
      KP = KPHASE
      GOTO 100
! WORD CANNOT BE FOUND:
  101 IANS = 0
  100 RETURN

      END SUBROUTINE TBLFND
!
!*****************************************************************************
!
      LOGICAL FUNCTION TESTOV(A,B)
!
! *** TESTOV by JCM 22 Nov 83 ***
!
!X
!C 11C
!H Tests a floating division for potential overflow.
!A On entry A is the real numerator
!A          B is the real denominator
!A on exit  TESTOV is set .TRUE. if A/B would overflow, .FALSE. if not.
!
      TESTOV = ((A+B).EQ.A)

      END FUNCTION TESTOV
!
!*****************************************************************************
!
      SUBROUTINE TRYUNI(NCHK)
!
! *** TRYUNI by JCM 25 Sep 84 ***
!
!X
!C 1A
!H A specialist routine used in the setting up of the asymmetric unit, to
!H try out a given set of planes as faces of the unit.
!A On entry NCHK non-zero indicates that a check on the given representative
!A indices HT (see below) is to be performed whatever the given unit
!A is like.
!
!P The scratch COMMON /SCRAT/ is set up as follows:
!P A tentative asymmetric unit is held as NOPL planes in the array ASY,
!P the dimension of 4 allowing for "dead" planes in case we need to
!P revive them.
!
!P The status of each plane is in NSTAT:
!P       0=not there
!P       1=possible
!P       25=mandatory
!
!P HT is a 1x3 array holding the indices of a typical reflection which
!P must occur within (that is, not on the faces or the edges of) the
!P finished asymmetric unit.
!
!
!D TRYUNI does not necessarily produce the correct unit, but it checks
!D that any unit it sends out has 1 and only 1 relative of HT in it.
!
!D A unit which is too small, or which fails the above test, is
!D flagged on exit by NICE=-1.
!D An acceptable unit is falgged by NICE=1.
!
!D NOPL is expected to be 1, 2 or 3.  Copies NOPL planes out of the
!D temporary array ASY to the (eventually) permanent array ASYM in
!D /FUNIT/.
!
!D If there is more than 1 plane, the edge(s) and angle(s) involved are
!D set up, together with AMUL = the proportion of reciprocal space
!D occupied by the given unit.
!D
!D We then have VOL=AMUL*NMUL, and aim at VOL=1.
!D
!D If there are 3 planes forming a hinge, sets VOL=0., and NICE=1, and
!D leaves the configuration for the calling routine (FIXUNI) to deal with.
!D
!D If VOL=1., checks that the unit contains 1 and only 1 relative of
!D the representative indices in HT.
!D
!D If NCHK is non-zero does this check in any case, counting the number
!D of relatives both inside and outside.  This caould be useful if the
!D user is ever allowe to specify his own unit.
!
      DIMENSION NNSTAT(3)
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /FRIED / FRIEDL, KOM8
      LOGICAL FRIEDL
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SCRAT / AXI(3,24,2), MIRROR(24), D(3,3), PL1(3), PL2(3),  &
     &                PL3(3), HT(3), ASY(3,4), NSTAT(4), NOPL, NICE,    &
     &                VOL, MOP1, MOP2

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      NASYM = NOPL
      IF (NASYM.LE.0 .OR. NASYM.GE.4) THEN
        CALL ERRIN2(NASYM,0,'in PROGRAM using TRYUNI with NOPL=',' ')
        IF (IBMBER .NE. 0) RETURN
      ENDIF
      I = 1
      DO J = 1, NASYM
    3   IF (NSTAT(I).NE.0) GOTO 4
        I = I + 1
        IF (I.LT.5) GOTO 3
        CALL ERRMES(-1,0,'TRYUNI not given enough planes')
        IF (IBMBER .NE. 0) RETURN
    4   CALL GMEQ(ASY(1,I),ASYM(1,J),1,3)
        NNSTAT(J) = NSTAT(I)
        I = I + 1
      ENDDO
! SET UP PI/3 FOR DETECTION OF ANGLE, NICE INITIALISED TO 'TOO BIG', AND
! CLEAR INDICATOR THAT WE HAVE PI/3 OR 2PI/3 ANGLE:
      PIBY3 = PI/3.
      NICE = 1
      N3AX = 0
! BRANCH ON 1, 2 OR 3 PLANES:
   25 GOTO (7,8,9), NASYM
! SINGLE PLANE - HALF SPACE:
    7 AMUL = 0.5
      GOTO 10
! TWO PLANES - MAY FIRST NEED TO REVERSE ONE FOR SMALLER ANGLE:
    8 ANG(3) = ANGRAD(ASYM(1,1),ASYM(1,2),1)
      IF (ABS(ANG(3)-PIBY3).LT.0.0001) N3AX = 3
      IF (ABS(ANG(3)-2.*PIBY3).LT.0.0001) N3AX = 3
! DO NOT TRY TO REVERSE IF STATUS 3:
      IF (NNSTAT(1).EQ.3 .AND. NNSTAT(2).EQ.3) GOTO 11
      IF ((NICE.NE.-1) .AND. (ANG(3).LE.PIBY2)) GOTO 11
      IF ((NICE.EQ.-1) .AND. (ANG(3).GE.PIBY2)) GOTO 11
! ANGLE MUST BE ACUTE BETWEEN PLANES EXCEPT FOR CERTAIN CASES WHERE
! 2PI/3 NOT PI/3 IS WANTED
      NREV = 2
      IF (NNSTAT(2).EQ.3) NREV = 1
      CALL GMREV(ASYM(1,NREV),ASYM(1,NREV),1,3)
      ANG(3) = PI - ANG(3)
   11 AMUL = ANG(3)/TWOPI
      CALL VECPRD(ASYM(1,1),ASYM(1,2),EDGE(1,3))
! TAKE OUT ANY COMMON FACTOR:
      CALL FCTOR(EDGE(1,3),N)
      GOTO 10
! THREE PLANES - MAKE ANGLES SMALLER AS ABOVE, MAKE EDGES AND ANGLES:
    9 SUM = -PI
      NFLIP = 0
      J2 = 2
      J3 = 3
      DO J = 1, 3
        ANG(J) = ANGRAD(ASYM(1,J2),ASYM(1,J3),1)
        IF (ABS(ANG(J)-PIBY3).LT.0.0001) N3AX = J
        IF (ABS(ANG(J)-2.*PIBY3).LT.0.0001) N3AX = J
! DO NOT TEST 3RD ANGLE AS WE CANNOT COPE IF IT IS TOO BIG:
        IF (NFLIP.GT.1) GOTO 13
        IF (NNSTAT(J2).EQ.3 .AND. NNSTAT(J3).EQ.3) GOTO 13
        IF ((NICE.NE.-1) .AND. (ANG(J).LE.PIBY2)) GOTO 13
        IF ((NICE.EQ.-1) .AND. (ANG(J).GE.PIBY2)) GOTO 13
        NREV = J3
        IF (NNSTAT(J3).EQ.3) NREV = J2
        CALL GMREV(ASYM(1,NREV),ASYM(1,NREV),1,3)
! CHOOSE PLANE TO REVERSE IF NECESSARY
        ANG(J) = PI - ANG(J)
        NFLIP = NFLIP + 1
   13   SUM = SUM + ANG(J)
! MAKE EDGE, POINTING SAME WAY AS OPPOSITE PLANE:
        CALL VECPRD(ASYM(1,J2),ASYM(1,J3),EDGE(1,J))
        IF (SCALPR(EDGE(1,J),ASYM(1,J)).LT.0.0) CALL GMREV(EDGE(1,J),EDGE(1,J),1,3)
        CALL FCTOR(EDGE(1,J),N)
        J2 = J3
        J3 = J
      ENDDO
      AMUL = SUM/FOURPI
! DETECT HINGE - 3 PLANES, BUT WITH A COMMON EDGE:
      CALL EQVEC(EDGE(1,1),EDGE(1,3),2,M,0)
      IF (M.GE.3) GOTO 10
! MARK HINGE TO BE DEALT WITH OUTSIDE:
      VOL = 0.
      GOTO 100
! JOIN - WE HAVE A UNIT WITH 1, 2 OR 3 PLANES (NOT HINGE), AND WHERE RELEVANT
! EDGES AND ANGLES HAVE BEEN STORED;  MUL=FRACTION OCCUPIED:
   10 VOL = AMUL*FLOAT(NMUL)
! IF VOL IS 1 WE HAVE A UNIT OF THE RIGHT SIZE:
      IF (ABS(VOL-1.).LT.0.00001) NICE = 0
! IF VOL IS TOO SMALL WE MAY HAVE TO REMOVE A PLANE, OR WE MAY HAVE TO STOP
! REDUCING THE ANGLE BETWEEN TWO PLANES TO PI/3 AND LEAVE IT AT 2PI/3:
      IF (VOL.LT.0.9998) NICE = -1
      IF (NICE.EQ.-1 .AND. N3AX.NE.0) GOTO 25
! THIS MAY LOOP, BUT THE SECOND TIME WE LOOK AT SUCH A UNIT IT SHOULD BE OK.
      IF (NCHK.EQ.0 .AND. NICE.NE.0) GOTO 100
! IF VOLUME OK, OR IF ASKED TO CHECK ANYWAY, COUNT RELATIVES OF TYPICAL REFLN:
      MOP1 = 0
      IC = 1
      IF (FRIEDL) IC = 2
! IF FRIEDEL, SCAN -HT AS WELL AS HT:
      CALL GMREV(HT,PL2,3,1)
      DO NC = 1, IC
        CALL GMREV(PL2,PL2,3,1)
! SCAN ALL SYMMETRY OPERATORS STORED:
        DO N1 = 1, NOPC
          CALL ROTSYM(PL2,PL1,N1,2)
          CALL INBOX(PL1,IN)
! TEST RELATIVE FOR BEING IN GIVEN BOX - "ON" AS AN ANSWER IS AN ERROR:
          IF (IN.GT.0) THEN
            WRITE (LPT,3002) HT
            CALL BMBOUT
            RETURN
          ENDIF
          IF (IN.GE.0) THEN
! MOP1=NUMBER OF RELATIVES INSIDE BOX:
! MOP2 WILL HOLD "WHICH OPERATOR PUTS HT INTO BOX", -VELY IF BY A CENTRE ALSO:
            MOP1 = MOP1 + 1
            MOP2 = N1*(3-2*NC)
          ENDIF
        ENDDO
      ENDDO
! MOP1=0 MEANS WRONG BOX:
      IF (MOP1.EQ.0) NICE = -1
  100 RETURN
 3002 FORMAT (/' *** ERROR - typical reflection',3F5.0,' not general')

      END SUBROUTINE TRYUNI
!
!*****************************************************************************
!
      SUBROUTINE UNITID
!
! *** UNITID by JCM 26 Sep 84 ***
!
!X
!C 1A
!H A specialist routine called when an asymmetric unit has been found,
!H to tidy the unit, its faces and edges.
!
!P Must be called from SYMUNI, after calls of TRYUNI have found a
!P satisfactory unit, with NASYM faces in the array ASYM, edges in the array
!P EDGE and angles in the array ANG, all in /FUNIT/.
!
!P TRYUNI has also set MOP2 to point to the symmetry operator which
!P transforms the typical reflection HT into their relative in the
!P asymmetric unit.
!
!D Transforms the asymmetric unit as found back so that it contains
!D HT, writes out the equations of the final planes, and sets up the
!D edges again.
!
!O Write the equations of the planes bounding the asymmetric unit
!O to unit LPT.
!
      COMMON /FUNIT / NASYM, ASYM(3,3), EDGE(3,3), ANG(3), NMUL, KOM10
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SCRAT / AXI(3,24,2), MIRROR(24), D(3,3), PL1(3), PL2(3),  &
     &                PL3(3), HT(3), ASY(3,4), NSTAT(4), NOPL, NICE,    &
     &                VOL, MOP1, MOP2

      MO = IABS(MOP2)
      IF (NASYM.LE.0) THEN
! SPECIAL FOR P1, NO FRIEDEL:
        CALL MESS(LPT,1,'No symmetrical equivalents')
        GOTO 100
      ENDIF
      WRITE (LPT,2001) NASYM
 2001 FORMAT (/' Asymmetric unit has',I3,' plane(s):')
      DO I = 1, NASYM
        CALL ROTSYM(ASYM(1,I),PL1,MO,-2)
        IF (MOP2.LT.0) CALL GMREV(PL1,PL1,1,3)
        CALL GMEQ(PL1,ASYM(1,I),1,3)
        CALL PRIPLN(ASYM(1,I),1)
      ENDDO
! REMAKE EDGES:
      IF (NASYM.LT.2) GOTO 100
      J2 = 2
      J3 = 3
      DO J = 1, 3
        IF (NASYM.NE.3 .AND. J.NE.3) GOTO 4
        CALL VECPRD(ASYM(1,J2),ASYM(1,J3),EDGE(1,J))
        IF (SCALPR(EDGE(1,J),ASYM(1,J)).LT.0.) CALL GMREV(EDGE(1,J),EDGE(1,J),1,3)
        CALL FCTOR(EDGE(1,J),N)
    4   J2 = J3
        J3 = J
      ENDDO
  100 RETURN

      END SUBROUTINE UNITID
!
!*****************************************************************************
!
      SUBROUTINE UPONE(CH,ISYS)
!
!C 13C
! *** UPONE new by PJB 28-Mar-1994 ***
!
!H Puts character strings into all uppercase or all lower case depending on ISYS
!A CH contains the string for conversion
!A If ISYS=3 (Unix) converts to lowwer case
!A All other values of ISYS convert CH to upper case
!
! TO LOWER CASE FOR UNIX UPPER FOR VMS
      CHARACTER*(*) CH
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL
!
      N = LENGT(CH)
      DO I = 1, N
        DO J = 1, 26
! UNIX NEEDS LOWER CASE
          IF (ISYS.EQ.3) THEN
            IF (CH(I:I).NE.LETUP(J)) GOTO 2
            CH(I:I) = LETLOW(J)
          ELSE
            IF (CH(I:I).NE.LETLOW(J)) GOTO 2
            CH(I:I) = LETUP(J)
          ENDIF
          GOTO 1
    2   ENDDO
    1 ENDDO

      END SUBROUTINE UPONE
!
!*****************************************************************************
!
      SUBROUTINE UPPER(C)
!
! *** UPPER by JCM 3 Aug 92 ***
!
!X
!C 13C
!H Replaces any lower case letters in C by upper case
!
      CHARACTER*(*) C
      COMMON /CHARS / LETUP(26), LETLOW(26), ISPCE, IDIGIT(10), ISMBOL(21)
      CHARACTER*1 LETUP, LETLOW, ISPCE, IDIGIT, ISMBOL

      L = LENGT(C)
      DO I = 1, L
        M = LETTER(C(I:I))
        IF (M.GT.0) C(I:I) = LETUP(M)
      ENDDO

      END SUBROUTINE UPPER
!
!*****************************************************************************
!
      BLOCKDATA VARFMT
      COMMON /VARFOR/ RLINE1, RLINE2
      CHARACTER*24 RLINE2
      CHARACTER*16 RLINE1

      DATA RLINE1, RLINE2/'(5X,3F5.0,I5,I5)', '(15X,2F10.5,10X,2F10.5)'/

      END BLOCKDATA VARFMT
!
!*****************************************************************************
!
      SUBROUTINE VARMAK(DEFALT,GETPAR,VARSXX)
!
! *** VARMAK updated by JCM 14 Nov 90 ***
!
!X
!C 6A
!H Makes variables for a LSQ cycle from given FIX/VARY and CONSTRAINT lists.
!A DEFALT is a dummy name for a subroutine to be obeyed if no other
!A        information is available about a parameter, and which gives
!A        out the values .TRUE./.FALSE. for fixed/varied by default.
!A GETPAR is a dummy name for a subroutine to be obeyed to get the next
!A        parameter.  It should be replaced by PARRUN in the single-
!A        crystal applications, RUNPAR for Profile Refinement.
!A VARSXX is the dummy name for a subroutine to be obeyed when all the
!A        variables have been designated, to record "which variable is this
!A        parameter" against all possible parameters for this application.
!A        This method of recording the information is new to MK4.
!
!P Parameter naming must have been set up by LSETUP
!P All fixing, varying and constraining information must have been set up
!P into the COMMON /LINKAG/ using routines ADDFIX, ADDCON, SUBCON
!P ADDVAR
!
!D Scans all available information about all parameters.  Sets up tables
!D of: LVRPR "which parameter is this variable" (holds a packed KK value)
!D     LBSVR "which variable is this basic variable" (all +ve)
!D     LVRBS "which basic is this variable" (+=basic, -=redundant)
!D     LRDVR "which variable is this redundant variable"
!D
!D Stores all the constraints relevant to this cycle in /CONSTR/
!D
!D Sets up various useful quantities:
!D     LVARB=number of basic variables
!D     LVARV=number of variables
!D     NVARF()=numbers of variables in each phase in each family
!D     NBARF()=numbers of basics in each phase in each family
!D     LVFST1()=1 BEFORE starting place in a derivative vector for a
!D              particular family of a particular phase, i.e. which is the last
!D              variable before the first variable of that family.
!D     LBFST()=1 BEFORE starting place in a basic variable vector for a
!D              particular family of a particular phase, i.e. which is the last
!D              variable before the first basic variable of that family.
!D Calls VARSXX (application dependent) to store what used to be held in the
!D big array LPRVR, i.e. "which variable is this parameter?"
!
!O Calls PRIVAR to print out its findings.
!
      LOGICAL DEFALT
      EXTERNAL DEFALT, GETPAR, VARSXX

      INCLUDE 'PARAMS.INC'

      LOGICAL FOUND, KSAME, FX
      DIMENSION KKCOL(500), KBVCOL(500), A(500,200), KPRVR(2000)
      DIMENSION KREDUN(200)
      COMMON /CONSTR/ JCONST, JROWPT(301), JCMAT(200), AMOUNT(200), NEXTJ

      REAL            DERIVB
      INTEGER                          LVARB
      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB

      REAL            DERIVV
      INTEGER                          LVARV
      COMMON /DERVAR/ DERIVV(MaxVVar), LVARV

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /LINKAG/ NUMFV, NUMPAK, KKFV(200), KTYPFV(200), KSTFV(200),&
     &                KTIME(200), KUNPFV(5,30), NTIME, NUMCON,          &
     &                KKCON(500), AMCON(500), KPTCON(201), KSTCON(200), &
     &                KTPCON(200)

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      INTEGER         LVRBS,          LVRPR,          LBSVR,          LRDVR
      COMMON /POINTS/ LVRBS(MaxVVar), LVRPR(MaxVVar), LBSVR(MaxBVar), LRDVR(MaxConstraints)

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

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! START COUNTS OF VARIABLES AND BASIC VARIABLES:
      LVARV = 0
      LVARB = 0
! NOT IF ONLY SIMULATION:
      IF (SIMUL) GOTO 100
! COUNT FIXED PARAMETERS:
      LFIX = 0
      DO I = 1, NPHASE
        DO J = 1, NFAM
          DO K = 1, NSOURC
! CLEAR COUNT OF VARIABLES & BASICS/FAMILY/PHASE/SOURCE
            NBARF(J,I,K) = 0
            NVARF(J,I,K) = 0
! CLEAR POINTERS TO STARTS-1 OF FAMILY VARIABLES  & BASICS PER PHASE/SOURCE:
            LVFST1(J,I,K) = -1
            LBFST1(J,I,K) = -1
          ENDDO
        ENDDO
      ENDDO
! FIRST SCAN FIX & VARY LISTS AND FIX WHERE POSSIBLE:
! START A SCAN OF PARAMETERS:
      IFAM = 0
! NEXT PARAMETER:
    2 CALL GETPAR(IFAM,IGEN,ISPC)
      IF (IFAM.EQ.-1) GOTO 3
      FOUND = .FALSE.
      KTIM = 0
      KSTAT = 0
! GET AND KK FOR THIS PARAMETER:
      KK = KPAK(IFAM,IGEN,ISPC,JPHASE,JSOURC)
! SCAN FIXES & VARIES, BOTH SPECIFIC & BLANKET, FOR MATCHES:
      DO I = 1, NUMFV
        IF (.NOT.KSAME(KK,KKFV(I))) GOTO 1
        IF (KTIME(I).GT.KTIM .AND. IABS(KSTFV(I)).GE.KSTAT) THEN
          KTIM = KTIME(I)
          KSTAT = IABS(KSTFV(I))
          FX = (KSTFV(I).GT.0)
        ENDIF
        FOUND = .TRUE.
    1 ENDDO
! NO MENTION SO FAR IF FOUND STILL FALSE:
      IF (.NOT.FOUND) THEN
! FIRST SEE IF 'ONLY' OCCURRED ON 'FIX' CARD:
        IF (IONLY(JPHASE).EQ.1) THEN
          FX = .FALSE.
! OR ON 'VARY' CARD:
        ELSEIF (IONLY(JPHASE).EQ.2) THEN
          FX = .TRUE.
        ELSE
! OTHERWISE DEFAULT:
          FX = .NOT.(DEFALT(IFAM,IGEN,ISPC))
        ENDIF
      ENDIF
! RECORD KK FOR "CERTAINLY FIXED" (OF WHICH THE OPPOSITE IS "PROBABLY VARIED")
      IF (FX) THEN
        CALL ERRCHK(2,LFIX,2000,0,'LSQ parameters in VARMAK')
        IF (IBMBER .NE. 0) RETURN
        KPRVR(LFIX) = KK
      ENDIF
! NEXT PARAMETER
      GOTO 2
! SECOND LIST ALL PARAMETERS OCCURRING IN CONSTRAINTS:
    3 NPAR = 0
      DO I = 1, NUMCON
! NOT IF CONSTRAINT DELETED:
        IF (KSTCON(I).EQ.0) GOTO 41
! SCAN EVERY PARAMETER SPEC IN THIS CONSTRAINT:
        DO K = KPTCON(I), KPTCON(I+1) - 1
! NOT IF FIXED:
          IF (LFIX.EQ.0) GOTO 46
          IF (NFIND(KKCON(K),KPRVR,LFIX).GT.0) GOTO 42
   46     IF (NPAR.EQ.0) GOTO 43
! IS IT THERE ALREADY?
          IF (NFIND(KKCON(K),KKCOL,NPAR).GT.0) GOTO 42
! ADD TO LIST:
   43     CALL ERRCHK(2,NPAR,500,0,'parameters in strict constraints')
          IF (IBMBER .NE. 0) RETURN
! IN DESCENDING SEQUENCE (SO THAT REDUNDANTS WILL BE LATER PARAMETERS)
          DO J = 1, NPAR - 1
            IF (KKCOL(J).GT.KKCON(K)) GOTO 44
            DO L = NPAR, J + 1, -1
              KKCOL(L) = KKCOL(L-1)
            ENDDO
            KKCOL(J) = KKCON(K)
            GOTO 42
   44     ENDDO
          KKCOL(NPAR) = KKCON(K)
!      DO 44 L=NPAR-1,1,-1
!      IF (KKCOL(L) .GT. KKCON(K)) GO TO 45
!  44  KKCOL(L)=KKCOL(L-1)
!      L=1
!  45  KKCOL(L)=KKCON(K)
   42   ENDDO
   41 ENDDO
! FILL IN MATRIX OF COEFFICIENTS IN CONSTRAINTS:
! IN MATRIX A, FIRST SUBSCRIPT = PARAMETER, SECOND=CONSTRAINT
      CALL GMZER(A,500,200)
      NCON = 0
      DO I = 1, NUMCON
        IF (KSTCON(I).EQ.0) GOTO 51
        CALL ERRCHK(2,NCON,200,0,'strict constraints')
        IF (IBMBER .NE. 0) RETURN
        DO K = KPTCON(I), KPTCON(I+1) - 1
! NOT IF ALREADY FIXED:
          IF (LFIX.EQ.0) GOTO 53
          IF (NFIND(KKCON(K),KPRVR,LFIX).GT.0) GOTO 52
   53     A(NFIND(KKCON(K),KKCOL,NPAR),NCON) = AMCON(K)
   52   ENDDO
   51 ENDDO
! THIS MATRIX MAY CONTAIN REDUNDANT OR INCONSISTENT CONSTRAINTS, OR
! SIMPLY CONSTRAINTS NOT IN THE BEST FORM FOR DESIGNATING REDUNDANT
! VARIABLES.  PERFORM GAUSSIAN ELIMINATION ON IT:
      SMALL = 0.000001
      DO NP = 1, NCON
        DO J = NP, NPAR
          DO I = NP, NCON
            IF (ABS(A(J,I)).GT.SMALL) GOTO 63
          ENDDO
        ENDDO
! NO MORE NON-ZERO COEFFICIENTS LEFT - OUT
        GOTO 64
! PIVOT FOUND:
! SWOP COLUMNS J AND NP (EVEN IF THEY ARE THE SAME), AND SCALE:
   63   KTEMP = KKCOL(J)
        KKCOL(J) = KKCOL(NP)
        KKCOL(NP) = KTEMP
        PIVOT = A(J,I)
        DO K = 1, NCON
          TEMP = A(J,K)
          A(J,K) = A(NP,K)
          A(NP,K) = TEMP
        ENDDO
! NOW SWOP ROWS I AND NP:
        DO K = 1, NPAR
          TEMP = A(K,I)
          A(K,I) = A(K,NP)
          A(K,NP) = TEMP/PIVOT
        ENDDO
! NOW SCAN ALL CONSTRAINTS, OMITTING THE SECTION BETWEEN NP AND I
! WHICH WE ALREADY KNOW TO HAVE ZEROS, AND ELIMINATE:
        DO L = 1, NCON
          IF (L.GE.NP .AND. L.LE.I) GOTO 71
          IF (ABS(A(NP,L)).LT.SMALL) GOTO 71
          DO M = NP + 1, NPAR
            A(M,L) = A(M,L) - A(NP,L)*A(M,NP)
          ENDDO
   71   ENDDO
      ENDDO
   64 NCON = NP - 1
      IF (NPAR.LT.NCON) THEN
        WRITE (LPT,3010) NPAR, NCON
        CALL BMBOUT
        RETURN
      ENDIF
! THIRD MARK THE FIRST NCON PARAMETERS FROM THE ELIMINATED
! MATRIX AS "NOT BASIC" (MAY BE FIXED OR REDUNDANT)
      DO I = 1, NCON
        M = -I
        DO J = NCON + 1, NPAR
! A NON-ZERO IN THIS PANEL MEANS A CONSTRAINT, NOT A FIXING:
          IF (ABS(A(J,I)).GT.SMALL) GOTO 60
        ENDDO
! HERE WE HAVE DISCOVERED THAT THE PARAMETER IS ACTUALLY FIXED:
        CALL ERRCHK(2,LFIX,2000,0,'LSQ parameters in VARMAK')
        IF (IBMBER .NE. 0) RETURN
        KPRVR(LFIX) = KKCOL(I)
        KREDUN(I) = 0
        GOTO 68
! HERE WE HAVE DISCOVERED THAT THE PARAMETER IS REDUNDANT:
   60   KREDUN(I) = KKCOL(I)
   68 ENDDO
! FOURTH SCAN PARAMETERS AGAIN, DESIGNATING BASICS AND REDUNDANTS:
      IFAM = 0
! NEXT PARAMETER:
   21 CALL GETPAR(IFAM,IGEN,ISPC)
! GETPAR RETURNS IFAM -1 IF ALL FINISHED:
      IF (IFAM.EQ.-1) GOTO 22
      KK = KPAK(IFAM,IGEN,ISPC,JPHASE,JSOURC)
!
! IF PARAMETER IS FIXED, LEAVE IT SO:
      IF (LFIX.EQ.0) GOTO 23
      IF (NFIND(KK,KPRVR,LFIX).GT.0) GOTO 21
! IF NOT DESIGNATED "FIX" IT MUST BE A VARIABLE:
   23 CALL ERRCHK(2,LVARV,MaxBVar,0,'variables in LSQ')
      IF (IBMBER .NE. 0) RETURN
! COUNT VARIABLES/FAMILY/PHASE:
      NVARF(IFAM,JPHASE,JSOURC) = NVARF(IFAM,JPHASE,JSOURC) + 1
! RECORD STARTS OF FAMILIES IN ANY VARIABLES VECTOR, -1:
      IF (LVFST1(IFAM,JPHASE,JSOURC).EQ.-1) THEN
        LVFST1(IFAM,JPHASE,JSOURC) = LVARV - 1
      ENDIF
!
!** and check that F2VA has not been exceeded
!
! RECORD "WHICH PARAMETER IS THIS VARIABLE?"
      LVRPR(LVARV) = KK
! IF REDUNDANT, DEFER FURTHER DETAILS TILL WE HAVE LABELLED ALL BASICS:
      IF (NCON.NE.0) THEN
        N = NFIND(KK,KREDUN,NCON)
        IF (N.GT.0) THEN
          KREDUN(N) = LVARV
          GOTO 21
        ENDIF
      ENDIF
! MAKE IT BASIC:
      CALL ERRCHK(2,LVARB,MaxBVar,0,'basic variables in LSQ')
      IF (IBMBER .NE. 0) RETURN
! RECORD CROSS POINTERS FOR VARIABLES AND BASIC VARIABLES:
      LVRBS(LVARV) = LVARB
      LBSVR(LVARB) = LVARV
! RECORD STARTS OF FAMILIES IN ANY BASIC VARIABLES VECTOR, -1:
      IF (LBFST1(IFAM,JPHASE,JSOURC).EQ.-1) THEN
        LBFST1(IFAM,JPHASE,JSOURC) = LVARB - 1
      ENDIF
! ALL THE PARAMETERS INVOLVED IN THE RIGHT HAND SIDES OF CONSTRAINTS ARE
! BASIC - SEE IF THIS ONE IS THERE, AND RECORD WHICH VARIABLE IT IS IF SO:
      IF (NPAR.GT.0) THEN
        N = NFIND(KK,KKCOL,NPAR)
        IF (N.GT.0) KBVCOL(N) = LVARB
      ENDIF
! RECORD BASIC VARIABLES/FAMILY/PHASE:
      NBARF(IFAM,JPHASE,1) = NBARF(IFAM,JPHASE,1) + 1
      GOTO 21
! A BIT OF OVERKILL UNTIL I DECIDE ON A PERMANENT STRUCTURE:
   22 IV = LVARV
      IB = LVARB
      DO JJU = NPHASE, 1, -1
        DO IJU = NFAM, 1, -1
          DO KJU = NSOURC, 1, -1
            IF (LBFST1(IJU,JJU,KJU).EQ.-1) THEN
              LBFST1(IJU,JJU,KJU) = IB
            ELSE
              IB = LBFST1(IJU,JJU,KJU)
            ENDIF
            IF (LVFST1(IJU,JJU,KJU).EQ.-1) THEN
              LVFST1(IJU,JJU,KJU) = IV
            ELSE
              IV = LVFST1(IJU,JJU,KJU)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
! FINALLY ABSORB ALL CONSTRAINTS FOR USE THIS CYCLE:
      JCONST = 0
      NEXTJ = 1
      JROWPT(1) = 1
      DO I = 1, NCON
        LV = KREDUN(I)
        IF (LV.EQ.0) GOTO 30
        CALL ERRCHK(2,JCONST,300,0,'strict constraints')
        IF (IBMBER .NE. 0) RETURN
! RECORD CROSS POINTERS FOR VARIABLES AND REDUNDANT VARIABLES:
        LVRBS(LV) = -JCONST
        LRDVR(JCONST) = LV
        DO J = NCON + 1, NPAR
          IF (ABS(A(J,I)).LT.SMALL) GOTO 31
! PUT CONSTRAINT INTO TABLE TO USE THIS CYCLE:
          JCMAT(NEXTJ) = KBVCOL(J)
          AMOUNT(NEXTJ) = -A(J,I)
          NEXTJ = NEXTJ + 1
          JROWPT(JCONST+1) = NEXTJ
   31   ENDDO
   30 ENDDO
! FETTLE STARTS OF FAMILIES FOR THOSE WITH NO MEMBERS:
!**???
!
! PRINT OUT WHAT WE HAVE DONE:
      CALL PRIVAR
! CALL APPLICATION-DEPENDENT ROUTINE TO FILL IN "WHICH VARIABLE IS THIS
! PARAMETER?" FOR ALL PARAMETERS:
      CALL VARSXX
  100 RETURN
 3010 FORMAT (' ERROR ** ',I4,' parameters in ',I4,' constraints')

      END SUBROUTINE VARMAK
!
!*****************************************************************************
!
      FUNCTION VCTMOD(SCALE,H,IR)
!
! *** VCTMOD by JCM 26 Apr 84 ***
!
!X
!C 1B
!H Calculates the modulus of the vector H, in either space.
!A On entry H is a 1x3 array holding the given vector
!A          SCALE is the number by which to multiply the answer
!A          IR indicates the required space.  The modulus is calculated
!A             in real space if IR=1, reciprocal if IR=2.
!
!N Multiplies by the input quantity SCALE - useful if SCALE is equal
!N to say, lambda/2
!
      DOUBLE PRECISION VEC
      DIMENSION H(3)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      VEC = 0.
      J = 2
      K = 3
      DO I = 1, 3
        VEC = VEC + H(I)*H(I)*CPARS(I,IR) + H(J)*H(K)*2.*CPARS(I+3,IR)
        J = K
        K = I
      ENDDO
      VEC = SCALE*DSQRT(VEC)
      VCTMOD = SNGL(VEC)

      END FUNCTION VCTMOD
!
!*****************************************************************************
!
      SUBROUTINE VOCAB(WORD,MEAN,NW)
!
! *** VOCAB by JCM 4 Aug 90 ***
!
!X
!C 6A
!H Adds a given set of vocabulary and meanings to the Least Squares
!H total vocabulary.
!A On entry WORD is an array of NW A4 words
!A          MEAN is a corresponding (3,NW) array of integers, being the family,
!A               genus and species assigned to the word
!P In /WDSPC/ IWDNUM should hold the existing number of entries in LSQWD.
!P If used in a PR context:
!P In /PHASE/ KPHASE should hold the relevant phase (or zero if for all phases)
!P In /SOURCE/ KSOURC should hold the relevant source (or zero for all sources)
!D Copies the word array to LSQWD, starting at entry IWDNUM+1
!D If the first of a triplet in MEAN is negative, copies it to IWDSPC
!D Otherwise packs the triplet according to preset packing to IWDSPC
!D Updates IWDNUM, checking it.
!
      CHARACTER*4 WORD(NW)
      DIMENSION MEAN(3,NW)

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

      COMMON /WDSPC / IWDNUM, IWDSPC(60)
      COMMON /WORDS / LSQWD(60)
      CHARACTER*4 LSQWD

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! CHECK ARRAY SIZES:
      CALL ERRCHK(1,IWDNUM+NW,60,0,'words specifying LSQ problem')
      IF (IBMBER .NE. 0) RETURN
      DO I = 1, NW
        LSQWD(I+IWDNUM) = WORD(I)
        IF (MEAN(1,I).LT.0) THEN
          IWDSPC(I+IWDNUM) = MEAN(1,I)
        ELSE
          IWDSPC(I+IWDNUM) = KPAK(MEAN(1,I),MEAN(2,I),MEAN(3,I),KPHASE,KSOURC)
        ENDIF
      ENDDO
      IWDNUM = IWDNUM + NW

      END SUBROUTINE VOCAB
!
!*****************************************************************************
!
      SUBROUTINE WGHTLS(N,ARG)
!
! *** WGHTLS updated by JCM 23 Mar 92 ***
!
!X
!C 6C
!H Performs various operations to do with weights for LSQ, either for PR
!H or simpler applications.
!A On entry N=1 means find an L WGHT card and read from it the type of
!A              weighting to be used, with default = 1.  ARG is irrelevant.
!A          N=2 means from ARG, IWGHT etc make a weight to go with this OBS
!A          N=3 means given WT, get WDIFF and SQRTWT.  ARG is included for
!A              diagnostic purposes only, and is sometimes used as the
!A              argument, or sometimes the observation.
!
      LOGICAL ONCARD
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      GOTO (10,20,30), N
! FIND AND INTERPRET AN L WGHT CARD, OR THE LACK OF IT:
   10 IF (.NOT.ONCARD('L','WGHT',W)) THEN
        CALL MESS(LPT,1,'No L WGHT card -')
        GOTO 101
      ELSE
        IWGH(KSOURC) = NINT(W)
        IF (IWGH(KSOURC).GT.3 .OR. IWGH(KSOURC).LE.0) THEN
          CALL ERRIN2(IWGH(KSOURC),1,'weighting scheme',' not allowed -')
          GOTO 101
        ENDIF
      ENDIF
      GOTO (41,42,43), IWGH(KSOURC)
! UNIT WEIGHTS:
   41 CALL MESS(LPT,1,'Unit weights')
      GOTO 100
! WEIGHT TO BE USED AS READ:
   42 CALL MESS(LPT,1,'Weights to be used as read from reflection data')
      GOTO 100
! SIGMA READ, WEIGHT IS 1/SIGMA SQUARED:
   43 CALL MESS(LPT,1,'Sigma read from reflection data - weight is 1/sigma squared')
      GOTO 100
! INITIAL VALUE FOR WT - USED TO BE SUBROUTINE WTINPR:
   20 GOTO (1,2,3), IWGH(JSOURC)
! UNIT WEIGHTS:
    1 WT = 1.0
      GOTO 100
! DOBS READ AND TO BE USED AS WEIGHT:
    2 WT = DOBS
      GOTO 100
! SIGMA READ (INTO DOBS) - USE 1/SIGMA SQUARED:
    3 IF (DOBS.NE.0.) THEN
        WT = 1./(DOBS*DOBS)
      ELSE
        WRITE (LPT,3000) ARG, OBS
 3000   FORMAT (/' WARNING ** zero weight found for point ',2(F10.3,2X),' -- weight set to unity ')
        WT = 1.0
      ENDIF
      GOTO 100
   30 SQRTWT = SQRT(WT)
      WDIFF = SQRTWT*DIFF
      GOTO 100
  101 IWGH(KSOURC) = 1
      CALL MESS(LPT,0,'assuming unit weights')
  100 RETURN

      END SUBROUTINE WGHTLS
!
!*****************************************************************************
!
      SUBROUTINE XROOT(IA,XX,IS,IL,C)
!
! *** XROOT updated by JCM 18 Oct 87 ***
!
!X
!C 8B
!H Finds the symmetry operations which take the given source atom into
!H the given coordinates.
!
!A On entry IA is the number of the atom in the given list on A cards
!A          XX is a 1x3 real array holding the given x,y,z to be identified.
!A On exit IS the number of the necessary symmetry operator, set -ve if
!A            the (-x, -y, -z) operator is also required;  if no such operator
!A            is found, IS=0
!A         IL is the number of the necessary lattice translation.
!A         C is a 1x3 real array holding any necessary cell translations,
!A           which will be whole numbers.
!
!P The symmetry should have been read by SYMOP, and the atomic positions
!P by ATOPOS.
!
!N There is an inverse subroutine XTRANS
!N Does not now involve the putting of a transformed atom back into
!N a central cell, because this had awkward repercussions in slack
!N constraints.
!
      DIMENSION XX(3), X1(3), X2(3), X3(3), C(3)
      LOGICAL GMSAME
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
!
! CYCLE OVER CENTRE
      DO IC = 1, NCENT
! CYCLE OVER OPERATORS WITHOUT CENTRE:
        DO IS = 1, NOPC
          CALL ROTSYM(X(1,IA),X1,IS,1)
          CALL GMADD(X1,TRANS(1,IS),X1,1,3)
          IF (IC.EQ.2) CALL GMREV(X1,X1,1,3)
! CYCLE OVER LATTICE TRANSLATIONS:
          DO IL = 1, NLAT
            CALL GMADD(X1,ALAT(1,IL),X2,1,3)
! DO NOT NOW PUT INTO CENTRAL CELL:
!      CALL FRAC3(X2)
! SCAN ALL 27 CELLS:
            DO NCELZ = 1, 5
              C(3) = FLOAT(NCELZ-3)
              DO NCELY = 1, 5
                C(2) = FLOAT(NCELY-3)
                DO NCELX = 1, 5
                  C(1) = FLOAT(NCELX-3)
                  CALL GMADD(X2,C,X3,1,3)
! X3 SHOULD EVENTUALLY MATCH OUR TARGET:
                  IF (GMSAME(XX,X3,3,0.0001)) GOTO 101
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
! IF HERE, WE HAVE NO MATCH:
      IS = 0
  101 IF (IC.EQ.2) IS = -IS

      END SUBROUTINE XROOT
!
!*****************************************************************************
!
      SUBROUTINE XTRANS(IAT,XX,IS,IL,C)
!
! *** XTRANS corrected by JCM 18 Oct 87 ***
!
!X
!C 8B
!H Transforms a given atomic position by given symmetry operator and
!H lattice and cell translations.
!
!A On entry IAT = which atom
!A          IS = which symmetry operator, -ve if -x,-y,-z
!A          IL = which lattice translation
!A          C is a 1x3 real array holding cell translations in the x, y and
!A            z directions, which will usually be integers.
!A On exit  XX is a 1x3 real array holding the transformed coordinates
!P The symmetry should have been set up by SYMOP, and the atomic positions
!P by ATOPOS.
!
!N There is an inverse subroutine XROOT
!N Altered not to put atom into central cell after symmetry transformations
!N because this has awkward repercussions in slack constraints
!
      DIMENSION XX(3), C(3)
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
!
! ROTATE BY SYMMETRY OPERATOR:
      CALL ROTSYM(X(1,IAT),XX(1),IABS(IS),1)
! TRANSLATION VECTOR ASSOCIATED WITH THIS SYMMETRY OPERATOR:
      CALL GMADD(XX(1),TRANS(1,IABS(IS)),XX(1),1,3)
! CENTRE OF SYMMETRY IF APPLICABLE:
      IF (IS.LT.0) CALL GMREV(XX,XX,1,3)
! LATTICE TRANSLATION:
      CALL GMADD(XX(1),ALAT(1,IL),XX(1),1,3)
! DO NOT NOW PUT INTO CENTRAL CELL:
!      CALL FRAC3(XX)
! CELL TRANSLATIONS:
      CALL GMADD(XX(1),C(1),XX(1),1,3)

      END SUBROUTINE XTRANS
!
!*****************************************************************************
!
      SUBROUTINE BMBOUT
! BMBOUT BoMBs OUT of the code. Essentially this replaces STOP in the code but
! implements a pseudo-error throwing mechanism. We cant have STOP being called while
! the GUI is running, since this will annoy users a lot
!
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER
      DATA IBMBER/0/
!
! If you want to stop on an error, then comment out the next line
!     STOP
!
      IBMBER = 1 ! The alternative: track with a flag

      END SUBROUTINE BMBOUT
!
!*****************************************************************************
!
