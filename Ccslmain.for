CCSL Mark 4 Update 54 4-July-95
C
C
C
C
C
C               C A M B R I D G E   C R Y S T A L L O G R A P H Y
C
C                      S U B R O U T I N E   L I B R A R Y
C
C
C
C
C
C
C
C
C
C   Parameter ATFS altered from 20     to 50    
C   Parameter ATOM altered from 50     to 150   
C   Parameter BVAR altered from 200    to 400   
C   Parameter CSTR altered from 20     to 300   
C   Parameter F2VA altered from 200    to 300   
C   Parameter MATS altered from 3000   to 80000 
C   Parameter OLAP altered from 70     to 200   
C   Parameter OMAX altered from 200    to 300   
C   Parameter PHAS altered from 1      to 9     
C   Parameter PSLK altered from 300    to 1000  
C   Parameter PVAR altered from 1000   to 2000  
C   Parameter REFS altered from 1000   to 10000 
C   Parameter SLAK altered from 20     to 500   
C   Parameter SORC altered from 1      to 5     
C   Parameter VVAR altered from 250    to 500   
C
C                       M A R K  4       L I B R A R Y
C
C             TO BE DESCRIBED IN THE USER'S MANUAL 'CCSL MARK 4'.
C

C
!U      BLOCK DATA ABSHED
!U      COMMON /HEDABS/HEDAB
!U      CHARACTER*104 HEDAB
!U      DATA HEDAB/'(6X,''h'',4X,''k'',4X,''l'',5X,''   Iobs       Icor
!U     &      Trn Fac      & Theta     Omega       Nu      ''/)'/
!U      END
C
C

C
C
C LEVEL 3      SUBROUTINE ADDANG(NAME,N1,N2,N3,NA,IE)
      SUBROUTINE ADDANG(NAME,N1,N2,N3,NA,IE)
C
C *** ADDANG updated by JCM 25 Jul 91 ***
C
CX
CC 8A
CH Finds an angle in the tables for geometric constraints, or adds it if absent.
CA On entry:
CA           NAME is the angle name, A4, which may be empty
CA           N1 is the number of the bond opposite the angle in the tables
CA           N2 is the number of one bond at the angle
CA           N3 is the number of the other bond at the angle
CA On exit, NA is the number of this angle in the angle names in ANGNAM
CA          IE is an error indicator, =0 if OK
CD If an angle already exists in the tables opposite N1, ignores
CD NAME and sends out NA=its position.
CD If there is no such angle, counts up in NUMANG in /SLKGEO, and adds
CD the new angle.  If in this case NAME is empty, invents a name.
CN The bonds N2 and N3 are held in INANG so that INANG( ,2) < INANG( ,3)
C
      CHARACTER *4 NAME,MAKNAM
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      IE=0
C IS ANGLE ALREADY GIVEN IN TRIANGLE N1,N2,N3 OPPOSITE N1?
      NMIN=MIN(N2,N3)
      NMAX=MAX(N2,N3)
      DO 1 I=1,NUMANG
      IF (NMIN .EQ. INANG(I,2) .AND. NMAX .EQ. INANG(I,3)) THEN
        IF (NAME .NE. ANGNAM(I) .AND. NAME .NE. '    ') THEN
          WRITE (LPT,3000) NAME,ANGNAM(I)
          WRITE (ITO,3000) NAME,ANGNAM(I)
3000      FORMAT (/' ERROR ** ',A4,' and',A4,' refer to same angle')
          IE=1
          GO TO 100
        ENDIF
        NA=I
        GO TO 100
      ENDIF
   1  CONTINUE
C
C NEW ANGLE - COUNT NUMBER OF BOND ANGLES:
C%
C      CALL ERRCHK(2,NUMANG,%SANG%,1,'angles for geometric constraints')
      CALL ERRCHK(2,NUMANG,100,1,'angles for geometric constraints')
C
C CHECK NAME OF ANGLE DOES NOT CLASH WITH ANY ATOM  OR BOND NAMES:
      L1=0
      IF (NTARNM .GT. 0) L1=NCFIND(NAME,ATTNAM,NTARNM)
      L2=NCFIND(NAME,BONNAM,NUMBON)
      IF (L1 + L2 + IATOM(NAME) .GT. 0) THEN
        CALL ERRCH2(NAME,1,'Angle name','also used elsewhere')
        IE=1
        GO TO 100
      ENDIF
C
C ANGLE MAY HAVE BEEN GIVEN WITHOUT A NAME:
      ANGNAM(NUMANG)=NAME
      IF (NAME .EQ. '    ') ANGNAM(NUMANG)=MAKNAM('Ang',NUMANG)
      NA=NUMANG
      INANG(NA,1)=N1
      INANG(NA,2)=NMIN
      INANG(NA,3)=NMAX
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE ADDATM(NAME,IA,XA,ISA,ILA,CELA,N)
      SUBROUTINE ADDATM(NAME,IA,XA,ISA,ILA,CELA,N)
C
C *** ADDATM updated by JCM 9 Jun 91 ***
C
CX
CC 8A
CH Finds an atom in the tables for geometric constraints, or adds it if absent.
CA On entry:
CA          NAME=the atom name, or is empty if a name is to be invented
CA          IA=the number of the base atom from which it is derived
CA          XA(1:3) hold its actual fractional coordinates
CA          ISA is the symmetry operator making it from base (-ve if needed)
CA          ILA is the lattice translation making it from base
CA          CELA(1:3) hold the cell translations making it from base
CA On exit  N=which entry in the atom tables it is
CD Searches for NAME in the existing table; if found, checks that all the
CD other components are the same, and exits with N=where found.
CD If NAME is empty , still does the check on all other components.
CD If not found, counts in NTARNM in /SLKGEO, and adds NAME and all
CD other components to tables, exitting with N=NTARNM
C
      CHARACTER *4 NAME,MAKNAM
      DIMENSION XA(3),CELA(3)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
C IF NAME IS EMPTY, SEARCH FOR THE REST:
      IF (NAME .EQ. '    ') THEN
        DO 19 I=1,NTARNM
        IF (IABASE(I) .NE. IA) GO TO 19
        DO 18 J=1,3
        IF (CELLTR(J,I) .NE. CELA(J)) GO TO 19
        IF (XSLAK(J,I) .NE. XA(J)) GO TO 19
  18    CONTINUE
        IF (ISYM(I) .NE. ISA) GO TO 19
        IF (ILAT(I) .NE. ILA) GO TO 19
C ATOM FOUND:
        N=I
        GO TO 100
  19    CONTINUE
        GO TO 17
      ENDIF
C
C SEARCH FOR NAME IN THOSE FROM A CARDS:
      N1=IATOM(NAME)
      IF (N1 .LE. 0) GO TO 1
C
      IF (N1 .NE. IA) GO TO 2
      IF (ISA .NE. 1) GO TO 2
      IF (ILA .NE. 1) GO TO 2
      DO 3 I=1,3
      IF (XA(I) .NE. X(I,N1)) GO TO 2
      IF (CELA(I) .NE. 0.) GO TO 2
   3  CONTINUE
C
C ATOM IDENICAL - OK:
      GO TO 1
C
   2  CALL ERRCH2(NAME,1,'L ATOM card name',
     & 'same as one on A card')
      GO TO 100
C
C THIS PUTS EVEN THE ORIGINAL ATOMS INTO THIS TABLE, WHICH IS WHAT WE WANT
C WHEN ACTUALLY APPLYING THE CONSTRAINTS.  WHEN GENERATING CARDS IN SLKGEN
C WE DON'T MIND.
   1  N=0
      IF (NTARNM .GT. 0) N=NCFIND(NAME,ATTNAM,NTARNM)
      IF (N .GT. 0) THEN
        DO 4 I=1,3
        IF (XA(I) .NE. XSLAK(I,N)) GO TO 5
   4    CONTINUE
        GO TO 100
C
   5    CALL ERRCH2(NAME,1,'Name','occurs on 2 L ATOM cards')
        GO TO 100
      ENDIF
C
C%
C  17  CALL ERRCHK(2,NTARNM,%SLAK%,1,'names on L ATOM cards')
  17  CALL ERRCHK(2,NTARNM,500,1,'names on L ATOM cards')
      N=NTARNM
C
C IF NO NAME GIVEN, INVENT ONE:
      ATTNAM(N)=NAME
      IF (NAME .EQ. '    ') ATTNAM(N)=MAKNAM('SK0',N)
      IABASE(N)=IA
      DO 7 I=1,3
      CELLTR(I,N)=CELA(I)
   7  XSLAK(I,N)=XA(I)
      ISYM(N)=ISA
      ILAT(N)=ILA
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE ADDBON(NAME,NA1,NA2,NA)
      SUBROUTINE ADDBON(NAME,NA1,NA2,NA)
C
C *** ADDBON updated by JCM 21 Jul 91 ***
C
CX
CC 8A
CH Finds a bond in the tables for geometric constraints, or adds it if absent.
CA On entry:
CA           NAME is either the bond name, A4, or it is empty, meaning
CA                that the name is irrelevant
CA           NA1 is the number of the atom at one end in the tables
CA           NA2 is the number of the atom at the other end
CA On exit, NA is the number of this bond in the bond tables
CD If a bond already exists in the tables from NA1 to NA2, ignores
CD NAME and sends out NA=its position.
CD If there is no such bond, counts up in NUMBON in /SLKGEO, and adds
CD the new bond.  If in this case NAME is empty, invents a name.
CN The ends of a bond are in IATM(,1:2) with (,1) less than (,2)
C
      CHARACTER *4 NAME,MAKNAM
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
C IS BOND ALREADY GIVEN FROM NA1 TO NA2?
      NMIN=MIN(NA1,NA2)
      NMAX=MAX(NA1,NA2)
      DO 1 I=1,NUMBON
      IF (NMIN .EQ. IATM(I,1) .AND. NMAX .EQ. IATM(I,2)) THEN
        IF (NAME .NE. BONNAM(I) .AND. NAME .NE. '    ') THEN
          WRITE (LPT,3000) NAME,BONNAM(I)
          WRITE (ITO,3000) NAME,BONNAM(I)
3000      FORMAT (/' ERROR ** ',A4,' and',A4,' refer to same bond')
          IERR=IERR+1
          GO TO 100
        ENDIF
        NA=I
        GO TO 100
      ENDIF
   1  CONTINUE
C
C NEW BOND - COUNT NUMBER OF INVOLVED BONDS:
C%
C      CALL ERRCHK(2,NUMBON,%SLAK%,1,'bonds for geometric constraints')
      CALL ERRCHK(2,NUMBON,500,1,'bonds for geometric constraints')
C
C CHECK NAME OF BOND DOES NOT CLASH WITH ANY ATOM NAMES:
      L1=0
      IF (NTARNM .GT. 0) L1=NCFIND(NAME,ATTNAM,NTARNM)
      IF (L1 + IATOM(NAME) .GT. 0) THEN
        CALL ERRCH2(NAME,1,'Bond name','is also an atom name')
        GO TO 100
      ENDIF
C
C BOND MAY BE GIVEN WITHOUT A NAME:
      BONNAM(NUMBON)=NAME
      IF (NAME .EQ. '    ') BONNAM(NUMBON)=MAKNAM('Bnd',NUMBON)
      NA=NUMBON
      IATM(NA,1)=NMIN
      IATM(NA,2)=NMAX
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE ADDCON(NPAR,KK1,AM,NSTAT)
      SUBROUTINE ADDCON(NPAR,KK1,AM,NSTAT)
C
C *** ADDCON corrected by PJB 30-May-1995 ***
C
CX
CC 6C
CH Adds a constraint to the list held in LSQ programs.
CA On entry:
CA          NPAR=number of parameters involved in constraint
CA          KK1 is an array holding the NPAR parameter specs
CA          AM is a corresponding array of amounts
CA          NSTAT is the status to be given to the constraint:
CA   NSTAT=4 : user supplied (may be later removed)
CA   NSTAT=5 : program supplied (may not be later removed)
C
CD Puts the constraint into a standard form, with the KK increasing, and the
CD amount corresponding to the smallest KK as 1.  Thus can tell if it has had
CD this constraint before, and if so merely gives it the new status.
C
CN There is also an entry SUBCON to take one out.  If the first element of
CN KK1 for SUBCON is incomplete, this will scan all constraints looking for
CN any whose KKs are ALL encompassed by KK1(1), and delete them.  It is used
CN to clear out family 4 each cycle for CAILS type refinement
C
      LOGICAL KSAME,KWHOLE,KWIPE
      DIMENSION KK1(NPAR),AM(NPAR)
      DIMENSION KK2(20),KK3(20),AM2(20),KUN(5)
      COMMON /LENINT/NBITS
      COMMON /LINKAG/NUMFV,NUMPAK,KKFV(200),KTYPFV(200),KSTFV(200),
     & KTIME(200),KUNPFV(5,30),NTIME,NUMCON,KKCON(500),AMCON(500),
     & KPTCON(201),KSTCON(200),KTPCON(200)
C
C ADDING ENTRY:
      IADD=1
      GO TO 1
C
      ENTRY RELCON(NPAR,KK1,AM,NSTAT)
C ENTRY TO REPLACE AN EXISTING CONSTRAINT:
      IADD=0
      GO TO 1
C
      ENTRY SUBCON(NPAR,KK1,AM,NSTAT)
C SUBTRACTING ENTRY:
      IADD=-1
C
C DISCOVER IF WIPING WHOLE SET:
   1  KWIPE=.NOT. (KWHOLE(KK1(1),KUN))
      IF (KWIPE) GO TO 5
C
C PUT INTO STANDARD FORM OF ASCENDING KK WITH NEW AM(1)=1.:
      CALL GMEQ(KK1,KK2,1,NPAR)
      DO 2 I=1,NPAR
      KK3(I)=MINIM(KK2,NPAR,N)
      AM2(I)=AM(N)
      IF (I .GT. 1) AM2(I)=AM2(I)/AM2(1)
      KK2(N)=2**(NBITS-1)-1
   2  CONTINUE
      AM2(1)=1.
C
C SCAN EXISTING CONSTRAINTS:
   5  DO 3 I=1,NUMCON
C LOOKING FOR MATCHES:
      KPAR=KPTCON(I+1)-KPTCON(I)
      IF (KPAR .NE. NPAR .AND. .NOT. KWIPE) GO TO 3
      DO 4 J=1,KPAR
      IF (KWIPE) THEN
        IF (.NOT. (KSAME(KK1(1),KKCON(KPTCON(I)+J-1)))) GO TO 3
      ELSE
        IF (KK3(J) .NE. KKCON(KPTCON(I)+J-1)) GO TO 3
C PARAMETER MATCH - NOW COEFFICIENT?
C IF THE NEW CONSTRAINT HAS THE SAME PARAMETERS BUT DIFFERENT COEFFICIENTS..
C THIS CAN MEAN "REMOVE THE OLD ONE AND REPLACE BY THE NEW" (ENTRY RELCON)
C OR SOME INCONSISTENCY LEADING TO A FIXING (ENTRY ADDCON)
        IF (IADD.EQ.0) GO TO 4
        IF (ABS(AM2(J)-AMCON(KPTCON(I)+J-1)) .GT. 0.00001) GO TO 3
      ENDIF
   4  CONTINUE
C
C WE HAVE IT ALREADY - WERE WE ADDING OR SUBTRACTING OR REPLACING?
      IF (IADD) 6,7,101
C
C SUBTRACTING AN EXISTING:
   6  KSTCON(I)=0
      GO TO 3
C REPLACING WITH NEW COEFFICIENTS
    7 DO 8 J=1,KPAR
      AMCON(KPTCON(I)+J-1)=AM2(J)
    8 CONTINUE
      GO TO 101
C
   3  CONTINUE
C
C IF NOT FOUND, BUT SUBTRACTING, EXIT:
      IF (IADD.EQ.-1) GO TO 100
C
C IT IS A REALLY NEW CONSTRAINT - ADD IT:
C%
C      CALL ERRCHK(2,NUMCON,%ALLC%,0,'LSQ constraints')
      CALL ERRCHK(2,NUMCON,200,0,'LSQ constraints')
      KPTCON(NUMCON+1)=KPTCON(NUMCON)+NPAR
      DO 9 I=1,NPAR
      KKCON(KPTCON(NUMCON)+I-1)=KK3(I)
      AMCON(KPTCON(NUMCON)+I-1)=AM2(I)
   9  CONTINUE
      KTPCON(NUMCON)=1
      I=NUMCON
C
C ADDING (OR REPLACING) AN EXISTING JOINS HERE:
 101  KSTCON(I)=NSTAT
C
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUITNE ADDPLN(NIN,N)
      SUBROUTINE ADDPLN(NIN,N)
C
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE ADDTOR(NAME,N1,N2,N3,N4,N5,N6,NT,IE)
      SUBROUTINE ADDTOR(NAME,N1,N2,N3,N4,N5,N6,NT,IE)
C
C *** ADDTOR by JCM 16 Oct 90 ***
C
CX
CC 8A
CH Finds a torsion angle in the tables for geometric constraints, or adds it
CH if absent.
CA On entry:
CA           NAME is the torsion angle name, A4, which may be empty
CA           N1 is the number of the first bond of the pair of non-
CA              intersecting bonds between which the angle is required
CA           N2 is the number of the "axis" bond which joins one atom of N1
CA              to one atom of N3
CA           N3 is the number of the other bond of the pair defining the angle
CA           N4 is the number of the "free" bond joining the two other ends
CA              of N1 and N3
CA           N5 is the third side of the triangle formed by N1 and N2
CA           N6 is the third side of the triangle formed by N3 and N2
CA On exit, NT is the number of this angle in the angle names in ANGTOR
CA          IE is an error indicator, =0 if OK
CD If the angle already exists in the tables, ignores NAME and sends out
CD NT=its position.
CD If there is no such angle, counts up in NUMTOR in /SLKGEO, and adds
CD the new angle.  If in this case NAME is empty, invents a name.
CN The bonds N1 and N3 are held in INTOR;  which is which depends on the axis
CN N2, because it must join atoms (say A2 to A3) so that A2 < A3.  A2 is
CN defined to be on N1, and A3 on N3
CN
C
      CHARACTER *4 NAME
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      IE=0
C IS TORSION ANGLE ALREADY GIVEN BETWEEN N1 AND N3 WITH N2 AS AXIS?
      NMIN=MIN(N1,N3)
      NMAX=MAX(N1,N3)
      DO 1 I=1,NUMTOR
      IF (NMIN .EQ. INTOR(I,1) .AND. N2 .EQ. INTOR(I,2) .AND.
     & NMAX .EQ. INTOR(I,3)) THEN
        IF (NAME .NE. TORNAM(I) .AND. NAME .NE. '    ') THEN
          WRITE (LPT,3000) NAME,TORNAM(I)
          WRITE (ITO,3000) NAME,TORNAM(I)
3000      FORMAT (/' ERROR ** ',A4,' and',A4,
     &    ' refer to same torsion angle')
          IE=1
          GO TO 100
        ENDIF
        NT=I
        GO TO 100
      ENDIF
   1  CONTINUE
C
C NEW ANGLE - COUNT NUMBER OF TORSION ANGLES:
C%
C      CALL ERRCHK(2,NUMTOR,%TANG%,1,'torsion angles for geometric',
      CALL ERRCHK(2,NUMTOR,100,1,'torsion angles for geometric
     & constraints')
C
C CHECK NAME OF TORSION ANGLE DOES NOT CLASH WITH ANY OTHER NAMES:
      L1=0
      IF (NTARNM .GT. 0) L1=NCFIND(NAME,ATTNAM,NTARNM)
      L2=NCFIND(NAME,BONNAM,NUMBON)
      IF (NUMANG .GT. 0) L3=NCFIND(NAME,ANGNAM,NUMANG)
      IF (L1 + L2 + L3 + IATOM(NAME) .GT. 0) THEN
        CALL ERRCH2(NAME,1,'Torsion angle name','also used elsewhere')
        IE=1
        GO TO 100
      ENDIF
C
C ANGLE MAY NOT BE NAMED:
      IF (NAME .EQ. '    ') THEN
        WRITE (TORNAM(NUMTOR),2000) NUMTOR
2000    FORMAT ('>',I3)
      ELSE
        TORNAM(NUMTOR)=NAME
      ENDIF
      NT=NUMTOR
C
C N2 IS THE AXIS (FROM A2 TO A3):
      INTOR(NT,2)=N2
** N1 MUST JOIN A1 AND A2, AND N3 MUST JOIN A3 AND A4 - BUT I DON'T THINK
** THIS IS VITAL - I BELIEVE IT TO BE ONLY NOMENCLATURE
      INTOR(NT,1)=NMIN
      INTOR(NT,3)=NMAX
C COMPLETE THE TRIANGLE "N1", N2, AND SET K2 TO POINT TO THEIR COMMON ATOM:
      CALL BONTRI(NMIN,N2,INTOR(NT,5),K2,IEE)
C COMPLETE THE TRIANGLE "N3", N2, AND SET K3 TO POINT TO THEIR COMMON ATOM:
      CALL BONTRI(NMAX,N2,INTOR(NT,6),K3,IEEE)
      IE=IEE+IEEE
      IF (IE .EQ. 0) THEN
C IDENTIFY ENDS OF THE "FREE" BOND:
        K1=IATM(NMIN,1)
        IF (K1 .EQ. K2) K1=IATM(NMIN,2)
        K4=IATM(NMAX,1)
        IF (K4 .EQ. K3) K4=IATM(NMAX,2)
C ADD FREE BOND TO LIST:
        CALL ADDBON('    ',K1,K4,INTOR(NT,4))
      ENDIF
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE ADJUST(PAR)
      SUBROUTINE ADJUST(PAR)
C
C *** ADJUST updated by JCM 11 Jan 88 ***
C
CX
CC 6C
CH Applies a (possibly fudged) shift to a given LSQ parameter.
CA On entry PAR is the parameter to be updated
CP In /NEWOLD/ SHIFT on entry is the shift from the LSQ matrix inversion
CP             IFAM,IGEN,ISPC specify the parameter, also packed in KPACK
CP In /FUDG/ is a list of all required fudge factors and their types
C
CD Sets XOLD = the existing value of PAR
CD Makes a tentative Xby applying SHIFT to XOLD
CD Scans IFDGPT to see if parameter has a fudge;  if so, branches on
CD its type in IFDTYP, and adjusts XNEW accordingly.
CD Finally sets XNEW into the parameter PAR.
C
      COMMON /FUDG/NFUDGE,IFDGPT(20),FUDGE1(20),FUDGE2(20),
     & IFDTYP(20)
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
C
      XOLD=PAR
      XNEW=XOLD+SHIFT
C EXIT IF NO FUDGES AT ALL:
      IF (NFUDGE .EQ. 0) GO TO 101
C
C
** WE NEED EVENTUAL PROVISION FOR PARTICULAR PHASES & SOURCES:
C PACK FAMILY AND GENUS, NO PARTICULAR SPECIES:
      JFG=KPAK(IFAM,IGEN,0,0,0)
C PACK FAMILY AND SPECIES, NO PARTICULAR GENUS:
      JFS=KPAK(IFAM,0,ISPC,0,0)
C PACK FAMILY ALONE:
      IFAMM=KPAK(IFAM,0,0,0,0)
C PACK GENUS ALONE:
      IGENN=KPAK(0,IGEN,0,0,0)
C PACK SPECIES ALONE:
      ISPCC=KPAK(0,0,ISPC,0,0)
C
C SCAN ALL FUDGES FOR A MATCH WITH ANY OF:
C        COMPLETE SPECIFICATION IN KPACK
C        FAMILY PLUS GENUS IN JFG
C        FAMILY PLUS SPECIES IN JFS
C        FAMILY ALONE IN IFAMM
C        GENUS ALONE IN IGENN
C        SPECIES ALONE IN ISPCC
      DO 1 I=1,NFUDGE
      IF (KPACK .EQ. IFDGPT(I)) GO TO 2
      IF (JFG .EQ. IFDGPT(I)) GO TO 2
      IF (JFS .EQ. IFDGPT(I)) GO TO 2
      IF (IFAMM.EQ. IFDGPT(I)) GO TO 2
      IF (IGENN .EQ. IFDGPT(I)) GO TO 2
      IF (ISPCC .EQ. IFDGPT(I)) GO TO 2
   1  CONTINUE
      GO TO 101
C
C THERE IS A FACTOR:
   2  GO TO (11,12,13,12) , IFDTYP(I)
C
C SIMPLE MULTIPLICATIVE FACTOR:
  11  XNEW=XOLD+FUDGE1(I)*SHIFT
      GO TO 101
C
C LOWER LIMIT FROM "GE" (MAY BE COMBINED WITH UPPER LIMIT):
  12  IF (XNEW .LT. FUDGE1(I)) XNEW=FUDGE1(I)
      IF (IFDTYP(I) .EQ. 2) GO TO 101
C
C UPPER LIMIT FROM "LE":
  13  IF (XNEW .GT. FUDGE2(I)) XNEW=FUDGE2(I)
      GO TO 101
C
 101  PAR=XNEW
      RETURN
      END
C
C
C LEVEL 4      SUBROUTINE ANGDIR(H,ANG)
      SUBROUTINE ANGDIR(H,ANG)
C
C *** ANGDIR by PJB Sep 87 ***
C
CX
CC 2B
CH Calculates D3 or  4-circle angles from direction cosines.
CA On entry H (of dimension 3) holds h,k,l for the required reflection
CA On exit, ANG (of dimension 4) must hold some useful useful afgles.
CP SETDC must have set IGEOM, UM matrix, NLR for left/right
CN Only written for geometry types 6,7,8,11
C
      DIMENSION DIR(3,2),V(3,2),H(3),ANG(4),T(3),XP(3),TT(3)
      COMMON /DGEOM/IGEOM,UM(9),NLR,ANGLIN(3),ALAMBD(5,5),
     & NLAMB,ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
C
      GO TO (50,50,50,50,50,10,10,10,50,50,10), IGEOM
C
C ERROR IN GEOMETRY TYPE:
  50  CALL ERRIN2(IGEOM,0,'Geometry type',
     & 'not in ANGDIR; only 4circle 6,7,11 or general normal beam 8')
C
   10 CALL GETDC(H,DIR)
      DO 1 I=1,2
      CALL GMPRD(UM,DIR(1,I),V(1,I),3,3,1)
    1 CONTINUE
C  REVERSE INCIDENT BEAM
      CALL GMREV(V(1,1),V(1,1),1,3)
C
      COSY=SCALPR(V(1,1),V(1,2))
      ANG(1)=-SIGN(ARCCOS(COSY),FLOAT(NLR))
      GO TO (50,50,50,50,50,20,20,11,50,50,20), IGEOM
C
C  GENERAL NORMAL BEAM CASE
   11 ANG(2)=ATAN2(V(1,1),V(2,1))
      X=V(3,2)
      V(3,2)=0.
      CALL UNIVEC(V(1,2),D)
      ANG(3)=ATAN2(X,D)
      GO TO 100
C
C  FOUR-CIRCLE
C MAKE T THE VERTICAL DIRECTION IN THE DIFFRACTING POSITION
   20 CALL VECPRD(V(1,2),V(1,1),T)
      CALL UNIVEC(T,D)
C  CHI FROM Z COORDINATE OF T
      ANG(3)=ARCCOS(T(3))
C  TT IS THE NORMAL TO THE CHI CIRCLE (T X Z)
      TT(1)=-T(2)
      TT(2)=T(1)
      TT(3)=0.
      CALL UNIVEC(TT,D)
C  TEST IF TT IS ON THE SAME SIDE AS INCIDENT BEAM
      CS2=SCALPR(TT,V(1,1))
      IF (CS2.LT.0) THEN
        ANG(3)=-ANG(3)
C  IF NOT REVERSE IT AND CHI
        CALL GMREV(TT,TT,3,1)
      ENDIF
      CALL VECPRD(V(1,1),TT,XP)
      SCS2=SCALPR(T,XP)
      ANG(2)=ARCCOS(ABS(CS2))
      IF (SCS2.LT.0) ANG(2)=-ANG(2)
      ANG(4)=ATAN2(-TT(2),TT(1))
      IF (ABS(NLR).EQ.1) GO TO 100
C  REVERSE ALL ANGLES IF NECESSARY
      CALL GMREV(ANG,ANG,1,4)
      GO TO 100
C
  100 RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE ANGLST(I1)
      SUBROUTINE ANGLST(I1)
C
C *** ANGLST updated by JCM 11 Sep 92 ***
C
CX
CC 8B
CH Lists all angles at one source atom made by a given list of bonds.
C
CA I1 is the number of the source atom (which will belong to the original list)
C
CP A list of bonds starting from atom I1 must be in COMMON /BONDLA.
CP There will be NB bonds stored in BSAVE, with the specifications of the
CP destination atoms in N2SAVE (held negatively if the atom is not in
CP the original list, but a symmetry relation), and the coordinate differences
CP in DXSAVE.
C
CD If there are at least 2 bonds, scans all pairs of bonds & prints out the
CD angle at atom I1 between the pair.  Destination atoms may be in any of
CD the 27 cells around the central asymmetric unit.
C
CO Writes to unit LPT the list of angles, with specifications of destination
CO        atoms if not original
C
      CHARACTER *16 CH1,CH2
      DIMENSION K1(6),K2(6),XD1(3),XD2(3)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /BONDLA/NB,BSAVE(100),DXSAVE(3,100),NBSAVE(100),
     & N2SAVE(100),N3SAVE(100),I1LAST,BMAX,BMIN,BBMAX,SDMAX,
     & ANG1,ANG2,BON2,SD1,SD2,BONOUT,LSK,SLK
      LOGICAL SLK,BONOUT
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      IF (NB.LE.1) GO TO 100
      IF (NB.EQ.2) CALL MESS(LPT,1,'Angle at atom '//ATNAME(I1)//' :')
      IF (NB.GT.2) CALL MESS(LPT,1,
     & 'Angles around atom '//ATNAME(I1)//' :')
      CALL MESS(LPT,0,'At1-At2-At3      Angle       Atom1 if not'//
     & ' original                  Atom3 if not original')
      DO 1 I=1,NB
      II=I+1
      IF (II .GT. NB) GO TO 1
      DO 2 J=II,NB
      COSTH = SCLPRD(DXSAVE(1,I),DXSAVE(1,J),1)/(BSAVE(I)*BSAVE(J))
      CALL SINCOS(COSTH,SINTH,'ANGLST')
      ANG = DEGREE(ATAN2(SINTH,COSTH))
C
C IF SLACK CONSTRAINTS BEING OUTPUT, ANG MAY BE NEEDED HERE:
      IF (.NOT. SLK) GO TO 42
      IF (ANG .GT. ANG1+SD1 .OR. ANG .LT. ANG1-SD1) GO TO 42
C
C PICK UP BONDS SURROUNDING ANGLE & MAKE 3RD BOND OF TRIANGLE:
      NB1=NBSAVE(I)
      NB2=NBSAVE(J)
      CALL BONTRI(NB1,NB2,NB3,NA,IE)
      N=NUMANG
      CALL ADDANG('    ',NB3,NB1,NB2,NANG,IE)
      IF (NANG .GT. N) WRITE (LSK,2020) ANGNAM(NANG),
     & BONNAM(NB1),BONNAM(NB2),ANG2,SD2
2020  FORMAT ('L ANGL ',3(A4,2X),F10.2,F10.3)
C
  42  IF ((N2SAVE(I) .LT. 0) .OR. (N2SAVE(J) .LT. 0)) GO TO 3
      WRITE (LPT,2002)ATNAME(N2SAVE(I)),ATNAME(I1),ATNAME(N2SAVE(J)),ANG
2002  FORMAT (1X,A4,'-',A4,'-',A4,F9.2)
      GO TO 2
C
C ONE OR BOTH ATOMS NEED FURTHER SPECIFICATION:
   3  DO 4 K=1,3
      XD2(K)=X(K,I1)
C TAKEN OUT C41:
C      CALL FRACT(XD2(K),Y,N)
      XD1(K)=XD2(K)-DXSAVE(K,I)
      XD2(K)=XD2(K)-DXSAVE(K,J)
   4  CONTINUE
C UNPACK SPECIFICATIONS:
      IF (N2SAVE(I) .LT. 0) CALL ATSPEC(-N2SAVE(I),K1,CH1)
      IF (N2SAVE(J) .LT. 0) CALL ATSPEC(-N2SAVE(J),K2,CH2)
      IF (N2SAVE(J) .GT. 0) WRITE (LPT,2001) ATNAME(K1(1)),
     & ATNAME(I1),ATNAME(N2SAVE(J)),ANG,CH1,XD1
2001  FORMAT (1X,A4,'-',A4,'-',A4,F9.2,2X,A16,3F8.4)
      IF (N2SAVE(I) .GT. 0) WRITE (LPT,2003) ATNAME(N2SAVE(I)),
     & ATNAME(I1),ATNAME(K2(1)),ANG,CH2,XD2
2003  FORMAT (1X,A4,'-',A4,'-',A4,F9.2,44X,A16,3F8.4)
      IF ((N2SAVE(I) .LT. 0) .AND. (N2SAVE(J) .LT. 0)) WRITE (LPT,2010)
     & ATNAME(K1(1)),ATNAME(I1),ATNAME(K2(1)),ANG,CH1,XD1,CH2,XD2
2010  FORMAT (1X,A4,'-',A4,'-',A4,F9.2,2X,A16,3F8.4,2X,A16,3F8.4)
   2  CONTINUE
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 3      FUNCTION ANGRAD(A,B,IR)
      FUNCTION ANGRAD(A,B,IR)
C
C *** ANGRAD by JCM 26 Sep 84 ***
C
CX
CC 1B
CH Calculates the angle in radians between two vectors, in either space.
C
CA A is a real 3-sized array which on entry holds the first vector
CA B is a real 3-sized array which on entry holds the second vector
CA IR on entry =1 for real space, 2 for reciprocal
C
CP RECIP must have set up the cell parameters.
CD ANGRAD on exit is set to be the angle in radians between vectors A and B.
CD Uses -A.B/moduli if real space, because expects A and B to be plane
CD normals, and the required angle to be between planes.
C
      DIMENSION A(3),B(3)
C
      C=SCLPRD(A,B,IR)
      C=C/(VCTMOD(1.0,A,IR)*VCTMOD(1.0,B,IR))
      IF (IR .EQ. 1) C=-C
      ANGRAD=ARCCOS(C)
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION ANITF(H,N)
      FUNCTION ANITF(H,N)
C
C *** ANITF by JCM 19 Jul 83 ***
C
CX
CC 4B
CH Forms the contribution to the anisotropic temperature factor on an
CH atom N from indices H.
C
CA H is a 3-size real array holding h,k,l on entry
CA N on entry = which atomic position
C
CP SETANI must have been obeyed to set up in the COMMON /ANISO:
CP      IAPT(N) =I for 'Nth atom has Ith temperature factor in array ATF', or
CP              =0 for 'Nth atom has no anisotropic temperature factor.
CD SETANI has converted the user's coefficients to standard betas in the array
CD ATF, in the expression exp-(beta11*h*h + 2.*beta23*k*l + etc.) so that
CD ANITF need use only this single expression.
CN Note the 2's in the expression.
C
      DIMENSION H(3)
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
C
      IA=IAPT(N)
      ANITF=1.
      IF (IA .EQ. 0) GO TO 100
C OUT IF NO ATF ON THIS ATOM
      A=ATF(1,IA)*H(1)*H(1)+ATF(2,IA)*H(2)*H(2)+ATF(3,IA)*H(3)*
     & H(3)+2.*ATF(4,IA)*H(2)*H(3)+2.*ATF(5,IA)*H(1)*H(3)+2.*ATF(6,IA)*
     & H(1)*H(2)
      ANITF=EXP(-A)
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE APSHDS
      SUBROUTINE APSHDS
C
C *** APSHDS updated by PJB 29-Sept-93 ***
C
CX
CC 7B
CH Applies shifts for during d-spacing refinement.
C
CP Only of use from MAIN program DSLSQ,DSMLSQ, or similar;
CP only expects one family of parameters, containing the 6 reciprocal cell
CP quadratic products and the three components of the propagation vector.
CP Expects shifts (one for each basic variable) in array BLSQ, with
CP corresponding ESDs in array DERIVB.
C
CD Applies shifts, possibly fudged, dealing with
CD constraints if necessary.  Recalculates all cell quantities and
CD if the propagation vector has changed, calls REINDX to reindex
CD the reflection indices in /REFLNS/
C
CO Writes to LPT the old and new values, the shift and its esd.
C
      DIMENSION DPROP(3)
      LOGICAL NPROP
      CHARACTER *4 LNAM1,LNAM2
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
C
      WRITE (LPT,2000) ICYC
2000  FORMAT (/////'1Shifts in variables for cycle',I3/
     & '   Variable       New           Esd          Shift        ',
     & '  Old    ')
C
C
C INITIALISE SHIFT COUNT AND SUMS
      CALL FETSHF(0,0.,0.)
C CLEAR PROPAGATION VECTOR SHIFT
      CALL GMZER(DPROP,3,1)
      NPROP=.FALSE.
C
C CYCLE OVER VARIABLES
      DO 1 I=1,LVARV
      J=LVRBS(I)
      KPACK=LVRPR(I)
C IF VARIABLE IS BASIC:
      IF (J .GT. 0) THEN
        SHIFT=BLSQ(J)
        ESD=DERIVB(J)
C OTHERWISE IT IS REDUNDANT, BY CONSTRAINT NUMBER -J:
      ELSE
        CALL SHFESD(-J)
      ENDIF
C
      CALL FETSHF(2,SHIFT,ESD)
      CALL PARNAM(LNAM1,LNAM2,3,KPACK)
      CALL PUNPAK(KPACK,IFAM,IGEN,ISPC)
      IF (ISPC.LT.6) THEN
        CALL ADJUST(CPARS(ISPC,2))
        WRITE (LPT,2001) LNAM1,LNAM2,CPARS(ISPC,2),ESD,SHIFT,XOLD
      ELSE
        CALL ADJUST(PROP(ISPC-6))
        WRITE (LPT,2001) LNAM1,LNAM2,PROP(ISPC-6),ESD,SHIFT,XOLD
        DPROP(ISPC-6)=SHIFT
        NPROP=.TRUE.
      ENDIF
2001  FORMAT (' ',1X,A4,1X,A4,4G14.5)
   1  CONTINUE
      CALL FETSHF(3,SHIFT,ESD)
C RECALCULATE OTHER CELL-RELATED QUANTITIES:
      CALL RECELL(1,1)
      IF (NPROP) CALL REINDX(DPROP)
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE APSHFW
      SUBROUTINE APSHFW
C
C *** APSHFW updated by JCM 10 Feb 87 ***
C
CX
CC 7B
CH Applies shifts for Forsyth & Wells scattering factor coefficient
CH refinement.
CP Only useful if called from FWLSQ or similar.
CD Scans variables, applies shifts with possible fudges.
CO Prints new value, shift, esd and old value.
C
      CHARACTER *4 LNAM1,LNAM2
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /FWVALS/NVALS,COEFFS(9)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      WRITE (LPT,2000) ICYC
2000  FORMAT (/////'1Shifts in variables for cycle',I3/
     & '   Variable       New           Esd          Shift        ',
     & '  Old    ')
      DO 1 I=1,LVARV
      J=LVRBS(I)
      KPACK=LVRPR(I)
      CALL PARNAM(LNAM1,LNAM2,3,KPACK)
      SHIFT=BLSQ(J)
      ESD=DERIVB(J)
      CALL ADJUST(COEFFS(I))
      COEFFS(I)=XNEW
      WRITE (LPT,2001) LNAM1,LNAM2,XNEW,ESD,SHIFT,XOLD
2001  FORMAT (' ',1X,A4,1X,A4,4G14.5)
   1  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE APSHMP(MAGROU)
      SUBROUTINE APSHMP(MAGROU)
C
C *** APSHMP updated by JCM 20 Mar 90 **
C
CX
CC 7B
CH Applies shifts to all variables in multipole LSQ, and prints the results.
CP Shifts are in array BLSQ, and ESDs in DERIVB (to save space)
C
CD Identifies each variable as a type of parameter, and call individual
CD routines to apply shift (possibly fudged).
C
CD For redundant variables, calculates shift from constituent parts of
CD relevant constraint.
C
CO Prints old and new values, shift and esd, with parameter name
CO If family 2 (structure parameter), does printing in blocks
C
      EXTERNAL MAGROU
      CHARACTER *4 LNAM1,LNAM2
      LOGICAL HEAD,FAM5
      COMMON /ATBLOC/NAME,IPNAME(12)
      CHARACTER *4 NAME,IPNAME
      COMMON /ATBLOK/IBUFF,PNEW(12),PESD(12),PSHIFT(12),POLD(12),
     & PSESD(12)
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      WRITE (LPT,2000) ICYC
2000  FORMAT (/////'1Shifts in variables for cycle',I3)
C
C SET UP QUANTITIES TO USE WITH DIFFERENT STYLES OF OUTPUT:
      IG=0
      IBUFF=0
      HEAD=.FALSE.
      FAM5=.FALSE.
C CLEAR SHIFT AVERAGING:
      CALL FETSHF(1,0.,0.)
C CLEAR VECTOR OF SHIFTS IN PROGRAM VERSION OF MULTIPOLES:
      CALL GMZER(POLAMP(1,4),1,NMPOL)
      CALL GMZER(POLAMP(1,6),1,NMPOL)
      I=0
C
C SCAN ALL VARIABLES:
    1 I=I+1
      IF (I.GT.LVARV) GO TO 101
      J=LVRBS(I)
      KPACK=LVRPR(I)
C IF VARIABLE IS BASIC:
      IF (J .GT. 0) THEN
        SHIFT=BLSQ(J)
        ESD=DERIVB(J)
C OTHERWISE IT IS REDUNDANT, BY CONSTRAINT NUMBER -J:
      ELSE
        CALL SHFESD(-J)
      ENDIF
C
      CALL PUNPAK(KPACK,IFAM,IGEN,ISPC)
      IF (J .LT. 0 .AND. IFAM .EQ. 2 .AND. ISPC .EQ. 10) GO TO 1
C
C BRANCH ON FAMILY:
      GO TO (11,12,99,99,50), IFAM
  99  CALL ERRIN2(IFAM,0,'Family','not in multipoles')
C
  11  GO TO (21,22) , IGEN
C
C FAMILY 1, GENUS 1 - MISCELLANEOUS SPECIES (TFAC,DOMR,MOSC):
  21  IF (ISPC .GT. 7) GO TO 32
C
C TFAC:
      IF (ISPC .EQ. 1) CALL LLTFAC(3)
      GO TO 40
C
C DOMR OR MOSC (EXTINCTION CORRECTION PARAMETERS)
  32  CALL EXTINC(ISPC-4,0.)
      GO TO 40
C
C FAMILY 1, GENUS 2 - IN SF THIS IS 'SCAL':
  22  CALL LLSCAL(3)
      GO TO 40
C
C FAMILY 2 - THESE ARE ALL TO DO WITH THE STRUCTURE FACTOR:
  12  IF (ISPC .LT. 13) THEN
        CALL F2SHFT
      ELSE
C MAGNETIC PARAMETERS:
        CALL MAGROU(3)
      ENDIF
      GO TO 40
C
C FAMILY 5: MULTIPOLES
   50 IF (.NOT.FAM5) THEN
C PRINT ANY REMNANTS FROM BUFFER:
        CALL PRBLOK
        CALL MESS(LPT,1,'Multipole parameter shifts:')
        FAM5=.TRUE.
      ENDIF
      POLAMP(ISPC,4)=SHIFT
      POLAMP(ISPC,6)=ESD
      GO TO 1
C
C COMMON EXIT TO PRINT SHIFTS:
  40  CALL FETSHF(2,SHIFT,ESD)
      CALL PARNAM(LNAM1,LNAM2,3,KPACK)
      IF (IFAM .NE. 2) GO TO 6
C DETECT CHANGE OF GENUS (ATOM)
      IF (IG .EQ. IGEN) GO TO 7
      HEAD=.FALSE.
      CALL PRBLOK
C PUT FIRST ENTRY FOR NEW ATOM INTO BUFFERS:
      NAME=LNAM1
      IG=IGEN
   7  IF (IBUFF .GE. 12) CALL PRBLOK
      IBUFF=IBUFF+1
      IPNAME(IBUFF)=LNAM2
      PNEW(IBUFF)=XNEW
      PESD(IBUFF)=ESD
      PSHIFT(IBUFF)=SHIFT
      POLD(IBUFF)=XOLD
      PSESD(IBUFF)=SHESD
      GO TO 1
C
C HERE TO PRINT TYPE 1 SHIFT AS BEFORE:
   6  IF (.NOT. HEAD) CALL MESS(LPT,1,'  Variable       New'//
     & '           Esd          Shift          Old           Shift/Esd')
      HEAD=.TRUE.
      WRITE (LPT,2006) LNAM1,LNAM2,XNEW,ESD,SHIFT,XOLD,SHESD
2006  FORMAT (' ',1X,A4,1X,A4,5G14.5)
      GO TO 1
C
 101  CALL PRBLOK
C APPLY ALL SHIFTS, AND PRINT IN USER UNITS:
      CALL PRMBLK
      CALL FETSHF(3,0.,0.)
      RETURN
      END
C
C

C

C

C

C

C
C
C LEVEL 2      SUBROUTINE ASK(MESS)
      SUBROUTINE ASK(MESS)
C
C *** ASK updated by PJB 17-Jun-1994 ***
C
CX
CC 13C
CH Writes a message on unit ITO and reads an interactive answer to /SCRACH/.
CA On entry MESS is the message.
CI On exit the answer typed to the terminal unit ITI is in ICARD, A80.
CO Writes the message on unit ITO, using a FORMAT finishing $ if this is
CO allowed by the FORTRAN system being used.
C
      CHARACTER *16 FORM
      CHARACTER *(*) MESS
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'(1X,80A1,'' : ''$)'/
C
      L1=LENGT(MESS)
      IF (L1 .EQ. 0) L1=1
C ANY SYSTEM WHICH WILL REALLY NOT TAKE $ SHOULD HAVE IT REMOVED:
C3084      FORM(16:16)=' '
      WRITE (FORM(5:6),2000) L1
2000  FORMAT (I2)
      WRITE (ITO,FORM) (MESS(I:I),I=1,L1)
CUNIX
C           CALL FLUSH(ITO)
      READ (ITI,1000) ICARD
1000  FORMAT (A80)
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE ASUNIT(H,HIN,N,M)
      SUBROUTINE ASUNIT(H,HIN,N,M)
C
C *** ASUNIT by JCM 3 Jul 84 ***
C
CX
CC 1B
CH Produces reflection indices in the asymmetric unit, related to those given.
CP SYMUNI must have set up the asymmetric unit.
CA On entry H is a 3-sized vector containing h,k,l, which may be anywhere
CA in reciprocal space.
CA On exit HIN is a 3-sized vector related by symmetry to H, and in (or on)
CA             the asymmetric unit.
CA           M is its multiplicity.
CA           N is the number of the symmetry operator which takes H into
CA             HIN.  N is -ve if (-x,-y,-z) involved, or 0 if error.
C
CD Takes account of Friedel - for non-centrosymmetric groups, Friedel is
CD *** ONLY *** assumed if the user has given an I FRIE card with a non-zero
CD number.  If HIN is related to H by an operator belonging to the Friedel-
CD related set (which are not stored explicitly in  COMMON SYMDA), then N is
CD set negatively.
CD
CD If there has been an error in the formation of the asymmetric unit, and
CD H does not therefore transform into the unit using any of the space group
CD operators, N is set=0
C
      DIMENSION H(3),HIN(3)
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C
C ARRANGE TO SCAN + AND - IF FRIEDEL:
      KEND=1
      IF (FRIEDL) KEND=2
      DO 1 K=1,KEND
      DO 1 I=1,NOPC
      CALL ROTSYM(H,HIN,I,2)
      IF (K .EQ. 2) CALL GMREV(HIN,HIN,3,1)
      MM=MULBOX(HIN)
      IF (MM .NE. 0) GO TO 101
C
   1  CONTINUE
C
C ERROR - REFLECTION NEVER TRANSFORMS INTO ASYMMETRIC UNIT:
      N=0
      M=0
      GO TO 100
C
C FOUND: SET N TO POINT TO OPERATOR, -VE IF IN FRIEDEL SET:
 101  N=(3-2*K)*I
      M=MM
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE ATOMS(IR,N,ALIST,MAX)
      SUBROUTINE ATOMS(IR,N,ALIST,MAX)
C
C *** ATOMS by JCM 3 Apr 88 ***
C
CX
CC 4B
CH Makes a real space unit cell full of related atomic positions.
C
CA On entry IR=an atom number
CA          MAX is the maximum number of positions ALIST can hold
CA On exit  N is the number of distinct positions related to the original,
CA               still within the original unit cell.
CA          ALIST(3,) holds N distinct atomic positions, the first being
CA               the given position, translated into (0,1) if necessary
C
      DIMENSION ALIST(3,MAX)
      DIMENSION XX(3),CEL(3)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
      N=0
C INITIAL POSITION:
      CALL GMEQ(X(1,IR),XX,1,3)
C
C OVER CENTRE IF PRESENT:
      DO 1 IC=1,NCENT
C OVER OTHER OPERATORS:
      DO 2 NS = 1,NOPC
      IS=NS*(3-2*IC)
C LATTICE TRANSLATIONS:
      DO 3 IL=1,NLAT
C NO NEIGHBOURING CELLS:
      CALL GMZER(CEL,1,3)
      CALL XTRANS(IR,XX,IS,IL,CEL)
      CALL FRAC3(XX)
      CALL EQPOS(ALIST,XX,N,M,MAX)
      IF (M .GT. N)  N=M
   3   CONTINUE
   2   CONTINUE
   1   CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 10      SUBROUTINE ATOPOS
      SUBROUTINE ATOPOS
C
C ***  ATOPOS by JCM 15 Jul 83 ***
C
CX
CC 4A
CH Reads and interprets all given A cards.
CP PREFIN must have first read the Crystal Data File
CP SYMOP must have already input the space group symmetry.
C
CD Reads a sequence of A cards.  The fixed format for these is:
CD A1,1X,A4,4F10,1X,A4,F10 (but nowadays free format is used).
CD Each card should contain:
CD   An atom label of up to 4 characters, starting with a letter
CD   X,Y and Z coordinates (which may be given as fractions if appropriate,
CD   e.g.  1/4  or 2/3)
CD   an isotropic temperature factor
CD   a possible scattering factor label (if different from starting letters
CD                                       of atom label)
CD   a possible site occupation factor (set = 1 if read as 0, so that it can
CD   be omitted altogether if wished.
CD
CD These may be read in free format, with spaces terminating items, provided
CD that they do not extend to the right of their corresponding fixed format
CD fields.  The scattering factor label and site occupation factor are both
CD optional, which means that if anything occurs after the isotropic temperature
CD factor, it is a label if it starts with a letter, or a number if a digit.
CD
CD Calculates the numbers of atoms of each type in the unit cell, and if the
CD position is special records the generators of its subgroup.
CD
CD Keeps lists of atom labels and scattering factor labels.
C
      CHARACTER *4 LABA,LABS
      LOGICAL LATVEC
      DIMENSION XIN(3)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /FORMD2/NBKF(20,9),NMFNM(9)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /POSNS2/NATO(9)
C%
C      COMMON /SCRAT/NTORD(%SYMO%),Z(6)
      COMMON /SCRAT/NTORD(24),Z(6)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C ARRANGE INPUT OF "S" CARDS IF NOT DONE
      IF (INREAD(19) .GT. 0) CALL SYMOP
C
C SET "A CARDS READ":
      INREAD(1) = -IABS(INREAD(1))
C
C INITIALISE NAME TABLES FOR ATOM AND SCATT NAMES:
      NUMANM=0
      NUMFNM=0
C
C NUMBER OF A CARDS:
      NACARD=ICDNO(1)
      NATOM=0
      IF (NACARD .EQ. 0) THEN
        CALL MESS(LPT,1,'No atomic positions have been read from'//
     &  ' Crystal Data File')
        GO TO 100
      ENDIF
C
      CALL MESS(LPT,1,'Atoms in the unit cell are')
      CALL MESS(LPT,0,' Mult Name       X         Y         Z'//
     & '        ITF      Site  Scat Fac Sub-Group')
C
C     NPOS=NUMBER OF GENERAL EQUIVALENT POSITIONS POSSIBLE
      NPOS = NLAT*NOP
C
C READ A CARDS ONE BY ONE:
      ID=IABS(INREAD(1))
      DO 1 IAC= 1,NACARD
      CALL INPUTA(ID,LABA,LBALEN,LABS,LBSLEN,XIN,TIN,SIIN,IER)
      ID=ID+NYZ
C ADD ATOM NAME TO TABLE - THE NAME WILL NOT USUALLY BE THERE ALREADY,
C BUT WE DO NOT AT THIS STAGE MIND IF IT IS:
C%
C      N=LMATCH(LABA,ATNAME,NUMANM,%ATOM%)
      N=LMATCH(LABA,ATNAME,NUMANM,150)
C WAS THIS AN SD CARD OR AN ORDINARY A CARD?
      IF (SDREAD) THEN
        CALL GMEQ(XIN,SDX(1,N),1,3)
        SDTF(N)=TIN
        SDSITE(N)=SIIN
** LATER, PRINT
        GO TO 1
      ENDIF
C ATOMIC POSITON:
C%
C      CALL ERRCHK(2,NATOM,%ATOM%,0,'atomic positions')
      CALL ERRCHK(2,NATOM,150,0,'atomic positions')
      NATO(JPHASE)=NATOM
      CALL GMEQ(XIN,X(1,N),1,3)
      TF(N)=TIN
      SITE(N)=SIIN
C ADD SCATTERING FACTOR NAME TO TABLE, OR FIND IT THERE ALREADY:
      LKEEP=NUMFNM
C%
C      L=LMATCH(LABS,FONAME,NUMFNM,%FORM%)
      L=LMATCH(LABS,FONAME,NUMFNM,20)
      NFORMF(N)=L
C IF FIRST ATOM OF THIS FACTOR, KEEP A BACKWARDS POINTER ALSO (FOR LSQ)
      IF (L .GT. LKEEP) THEN
        NBKF(L,JPHASE)=N
        NBAKF(L)=N
        NMFNM(JPHASE)=L
      ENDIF
C
C DEAL WITH POSSIBLE SPECIAL POSITION:
      M = 0
C
C  OPERATE WITH EACH SYMMETRY ELEMENT IN TURN:
      ISGEN(1,N)=1
      MC=0
C
C  TEST CENTRE OF SYMMETRY
      IF (CENTRC) THEN
        DO 4 I=1,3
    4   Z(I)=2.*X(I,N)
        IF (LATVEC(Z)) MC=1
      ENDIF
C
C  NOW EACH SYMMETRY OPERATOR
      NTORD(1)=1
      DO 2 NO=2,NOPC
      NTORD(NO)=0
      CALL ROTSYM(X(1,N),Z,NO,1)
C
C  ADD TRANSLATIONAL PART OF SYMMETRY ELEMENT:
      CALL GMADD(Z,TRANS(1,NO),Z,1,3)
      ISIG=1
      CALL GMSUB(X(1,N),Z,Z(4),1,3)
      IF (LATVEC(Z(4))) GO TO 6
C
C IF CENTROSYMMETRIC, TRY RELATED POSITION ALSO:
      IF (.NOT. CENTRC) GO TO 2
      ISIG=-1
      CALL GMADD(X(1,N),Z,Z(4),1,3)
      IF (.NOT. LATVEC(Z(4))) GO TO 2
C
    6 NTORD(NO)=IABS(NORD(NO))
      IF (NTORD(NO).GT.10) NTORD(NO)=NTORD(NO)-100
      NTORD(NO)=ISIG*(NTORD(NO))
C
C  COUNT FOR ORDER OF SUBGROUP
      ISGEN(1,N)=ISGEN(1,N)+1
   2  CONTINUE
C
    7 M=NPOS/(ISGEN(1,N)*(1+MC))
      IF (MC.EQ.1) ISGEN(1,N)=-ISGEN(1,N)
C
C  GET GENERATORS OF SUBGROUP
      CALL GENELM(NTORD,ISGEN(1,N))
C
      IF (ISGEN(1,N) .GT. 0) THEN
        IF (ISGEN(2,N).EQ.1 .AND. ISGEN(3,N) .EQ.0) THEN
          WRITE (LPT,2003) M,ATNAME(N),(X(I,N),I=1,3),TF(N),
     &    SITE(N),FONAME(L)
        ELSE
          WRITE (LPT,2003) M,ATNAME(N),(X(I,N),I=1,3),TF(N),
     &    SITE(N),FONAME(L),(ISGEN(I,N),I=2,3)
        ENDIF
      ELSE
        WRITE (LPT,2004) M,ATNAME(N),(X(I,N),I=1,3),TF(N),
     &  SITE(N),FONAME(L),(ISGEN(I,N),I=2,3)
      ENDIF
2003  FORMAT (1X,I4,2X,A4,1X,5F10.4,3X,A4,2I3)
2004  FORMAT (1X,I4,2X,A4,1X,5F10.4,3X,A4,2I3,' -1')
      AMULT(N) = FLOAT(M)/FLOAT(NOP)
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE ATSPEC(N,K,CH)
      SUBROUTINE ATSPEC(N,K,CH)
C
C *** ATSPEC updated by JCM 12 Nov 89 ***
C
CX
CC 8C
CH Makes the 16-character specification of a symmetry related atom from
CH its packed specification.
C
CP Specification must have been packed by a call of NPACK
C
CA N on entry is the packed integer giving atom specification
CA K is a 6-sized integer array which on exit is filled as follows:
CA     K(1)=which atom number was the original
CA     K(2)=which symmetry operator gave current position, -ve if also
CA          needed (-x,-y,-z)
CA     K(3)=which lattice translation
CA     K(4)=which cell in x direction (-1, 0 or +1)
CA     K(5)=which cell in y direction (-1, 0 or +1)
CA     K(6)=which cell in z direction (-1, 0 or +1)
CA CH is a A16 string; on exit it holds a printable representation of K
C
      CHARACTER *16 CH
      DIMENSION K(6),IDIG(5)
      COMMON /ATNMPK/ATPACK(10,3)
      INTEGER ATPACK
C
      CALL NPACK(N,K,6,2,ATPACK)
      CH=' '
      IF (K(2) .NE. 1) THEN
        IF (K(2) .LT. 0) CH(1:1)='-'
        CH(2:2)='S'
        CALL INTDIG(K(2),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(3:3),2,0)
      ENDIF
      IF (K(3) .NE. 1) THEN
        CH(6:6)='L'
        CALL INTDIG(K(3),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(7:7),1,0)
      ENDIF
      IF (IABS(K(4))+IABS(K(5))+IABS(K(6)) .NE. 0) THEN
        CH(9:9)='('
        IF (K(4) .LT.0) CH(10:10)='-'
        CALL INTDIG(K(4),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(11:11),1,0)
        IF (K(5) .LT.0) CH(12:12)='-'
        CALL INTDIG(K(5),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(13:13),1,0)
        IF (K(6) .LT.0) CH(14:14)='-'
        CALL INTDIG(K(6),IDIG,NDIG)
        CALL INTCHR(IDIG,NDIG,CH(15:15),1,0)
        CH(16:16)=')'
      ENDIF
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE AXIS(R,A)
      SUBROUTINE AXIS(R,A)
C
C *** AXIS by PJB/JCM 28 Jun 83 ***
C
CX
CC 1C
CH Finds the axis of a given rotation matrix.
CA On entry R is a 3x3 rotation matrix
CA On exit  A is a 1x3 vector holding its axis
C
      DIMENSION R(3,3),S(3,3),A(3)
C
C COPY R TO LOCAL S:
      CALL GMEQ(R,S,3,3)
C
C SET D= + OR -1 AS DETERMINANT OF R (AN EIGENVALUE):
      D=1.
      IF (DETER3(S) .LT. 0.) D=-1.
C
C FORM R MINUS D*(UNIT MATRIX):
      DO 1 I=1,3
    1 S(I,I)=S(I,I)-D
C
C I,J,K COUNT CYCLICALLY OVER 1,2,3:
      J=2
      K=3
      DO 3 I=1,3
      A(I)=S(J,J)*S(K,K)-S(J,K)*S(K,J)
      IF (ABS(A(I)).LT.10.E-4) GO TO 2
C A(I) HAS "DIAGONAL" ELEMENT OF VECTOR OF COFACTORS - IF NON-ZERO,
C VECTOR IS ACCEPTABLE, BUT IF ZERO GO ON TO LOOK AT NEXT
      A(J)=-S(J,I)*S(K,K)+S(K,I)*S(J,K)
      A(K)=-S(J,J)*S(K,I)+S(K,J)*S(J,I)
      GO TO 100
    2 J=K
    3 K=I
 100  CALL FCTOR(A,N)
      RETURN
      END
C
C
C
C LEVEL 2      LOGICAL FUNCTION BINDIG(N,NBIN)
      LOGICAL FUNCTION BINDIG(N,NBIN)
C
C *** BINDIG by JCM 13 Nov 91 ***
C
CX
CC 11C
CH Tests for the presence of a given binary digit within an integer.
CA On entry N is the integer to be tested
CA          NBIN is a binary digit (in decimal form, e.g. 1, 2, 4, 8 etc)
CA On exit BINDIG is TRUE if N contains NBIN, FALSE if not.
CN NBIN is not checked;  if it is not pure binary there will be strange results.
C
      LOGICAL EVEN
C
      IF (NBIN .NE. 0) THEN
        CALL PARITY(IABS(N/NBIN),I,EVEN)
        BINDIG=.NOT. EVEN
      ELSE
        BINDIG=.TRUE.
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE BITSET(I,NC,TEST,SET)
      SUBROUTINE BITSET(I,NC,TEST,SET)
C
C *** BITSET by JCM 25 Nov 82 ***
C
CX
CC 15C
CH Sets or tests a single bit in a word.
CA On entry I,NC pick out the required bit in array IBIT
CA          SET is a LOGICAL.
CA On exit, if SET is TRUE, sets relevant single bit into array IBIT.
CA          if FALSE, sets LOGICAL TEST to indicate whether relevant bit is
CA                    already present in IBIT, and removes it if it is.
C
      LOGICAL TEST,SET
      COMMON /BITMAP/IBIT(108,4),NWORDS
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS
C
      J = (NC-1)/NBITS + 1
      IF (J .LE. NWORDS) GO TO 3
      WRITE (LPT,3000) J,NWORDS,NC,NBITS
      WRITE (ITO,3000) J,NWORDS,NC,NBITS
3000  FORMAT (/' ERROR ** in BITSET - integer',I4,' called, but ',
     & 'only',I4,' available.  NC, NBITS=',2I4)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
   3  IB = MOD(NC,NBITS)
      IF (IB .EQ. 0) IB = NBITS
      M=ITPOS(IB)
      IF (SET) GO TO 1
      TEST = (LOGAND(IBIT(I,J),M) .NE. 0)
      IF (TEST) IBIT(I,J)=IBIT(I,J)-M
      GO TO 100
C
   1  IBIT(I,J)=LOGOR(IBIT(I,J),M)
 100  RETURN
      END
C
C
C
C
C
C LEVEL 3      SUBROUTINE BONCOS(B1,B2,B3,ANGLE,COSTH,SINTH,DADB)
      SUBROUTINE BONCOS(B1,B2,B3,ANGLE,COSTH,SINTH,DADB)
C
C *** BONCOS by JCM 18 Oct 90 ***
C
CX
CC 8B
CH Given 3 bonds forming a triangle, calculates the angle opposite the first,
CH its sine and cosine, and its derivatives wrt all 3 bonds.
CA On entry B1, B2, B3 are the values of the bonds (not their pointers)
CA On exit ANGLE is the angle opposite B1 in radians
CA         COSTH is its sine
CA         SINTH is its cosine
CA         DADB is a 1x3 array holding the derivatives of ANGLE wrt B1,B2,B3
C
      DIMENSION DADB(3)
C
      COSTH=(B2*B2 + B3*B3 - B1*B1)/(2.*B2*B3)
      CALL SINCOS(COSTH,SINTH,'BONCOS')
C ANGLE IN RADIANS:
      ANGLE=(ARCCOS(COSTH))
C
C DERIVATIVES OF COS THETA WRT B1, THEN B2, THEN B3
      DADB(1)=-B1/(B2*B3)
      DADB(2)=1./B3 - COSTH/B2
      DADB(3)=1./B2 - COSTH/B3
C CONVERT TO BE DERIVATIVES OF THETA RADIANS WRT BONDS:
      DO 1 I=1,3
   1  DADB(I)=(-DADB(I)/SINTH)
      RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION BONDA(I1,I2,I3)
      FUNCTION BONDA(I1,I2,I3)
C
C *** BONDA by JCM 1 Oct 86 ***
C
CX
CC 8B
CH Calculates the angle between two bonds.
CA On entry I1 is the serial number of one atom in list in /POSNS/
CA          I2 is the serial number of the central atom
CA          I3 is the serial number of the third atom
CA On exit BONDA is the angle between the bonds, in degrees
CD Calculates the angle between bonds joining atoms I1-I2 and I2-I3.
CD Does no symmetry operations at all.
CN It would probably be useful also to have a routine which accepts as
CN arguments X1(1:3), X2(1:3) and X3(1:3), position coordinates.
C
      DIMENSION D12(3),D23(3)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
      DO 1 I=1,3
      D12(I)=X(I,I1)-X(I,I2)
      D23(I)=X(I,I3)-X(I,I2)
   1  CONTINUE
      B12=VCTMOD(1.,D12,1)
      B23=VCTMOD(1.,D23,1)
      COSTH=SCLPRD(D12,D23,1)/(B12*B23)
      CALL SINCOS(COSTH,SINTH,'BONDA')
      BONDA=DEGREE(ATAN2(SINTH,COSTH))
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE BONDER(N)
      SUBROUTINE BONDER(N)
C
C *** BONDER updated by JCM 22 Oct 90 ***
C
CX
CC 8B
CH Calculate a bond and its derivatives for slack constraints.
CA On entry N points to which bond of the list in /SLKGEO is wanted
CD On exit, in /SLKGEO, BCALC(N) = bond length
CD Also sets up the 12 derivatives (whether needed or not) in /SLKGEO
CD        DERPOS(3,N,2)=derivatives of BCALC wrt x,y,z (in original
CD                      atom lists), both ends.
CD        DERCEL(6,N)  =derivatives of BCALC wrt A*, B*, C* etc
CD                      the cell quadratic products in reciprocal space
C
      DIMENSION DX(3),TEMP(3),TEMP1(6)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
C N IS THE BOND - SET N1 & N2 TO POINT TO ITS END ATOMS:
      N1=IATM(N,1)
      N2=IATM(N,2)
C DIFFERENCE VECTOR - DIFFERENCES BETWEEN ACTUAL COORDS OF ENDS:
      CALL GMSUB(XSLAK(1,N1),XSLAK(1,N2),DX,1,3)
C BOND:
      BCALC(N)=VCTMOD(1.,DX,1)
C
C DERIVATIVES WRT 6 CELL QUADRATIC PRODUCTS, FIRST IN REAL SPACE:
      J=2
      K=3
      DO 2 I=1,3
      DERCEL(I,N)=DX(I)*DX(I)/(2.*BCALC(N))
      DERCEL(I+3,N)=DX(J)*DX(K)/BCALC(N)
      J=K
   2  K=I
C THEN CONVERTED TO RECIPROCAL:
      CALL GMPRD(TOSTAR,DERCEL(1,N),TEMP1,6,6,1)
      CALL GMEQ(TEMP1,DERCEL(1,N),1,6)
C
C DERIVATIVES WRT GIVEN COORDS:
C INTO TEMP1 PUT a(a*DX + b cos gamma *DY + c cos beta *DZ)
C                b(a cos gamma *DX + b*DY + c cos alpha *DZ)
C                c(a cos beta *DX + b cos alpha *DY + c*DZ)
C WHICH IS Bond*(dBond/dX1, dBond/dY1, dBond/dZ1) OR
C ALSO    -Bond*(dBond/dX2, dBond/dY2, dBond/dZ2)
C
C VECTOR a*DX, b*DY, c*DZ:
      DO 11 I=1,3
  11  TEMP(I)=CELL(I,1,1)*DX(I)
C CONVERT USING COSINE MATRIX:
      CALL GMPRD(COSIN,TEMP,TEMP1,3,3,1)
      DO 12 I=1,3
  12  TEMP1(I)=TEMP1(I)*CELL(I,1,1)
C CONVERT DERIVS WRT ACTUAL COORDS TO THOSE WRT PARAMETERS OF REFINEMENT
      CALL ROTSYM(TEMP1,DERPOS(1,N,1),IABS(ISYM(N1)),1)
C IF CENTRE INVOLVED, REVERSE:
      IF (ISYM(N1).LT.0) CALL GMREV(DERPOS(1,N,1),
     & DERPOS(1,N,1),1,3)
C AND FOR SECOND ATOM, REVERSING SIGNS:
      CALL ROTSYM(TEMP1,DERPOS(1,N,2),IABS(ISYM(N2)),1)
      IF (ISYM(N2) .GT. 0) CALL GMREV(DERPOS(1,N,2),
     & DERPOS(1,N,2),1,3)
C FINALLY DIVIDE BY THE BOND, AS THE EXPRESSION WE HAVE DIFFERENTIATED IS
C BOND SQUARED, AND WE HAVE CANCELLED A 2:
      DO 21 I=1,3
      DO 21 J=1,2
  21  DERPOS(I,N,J)=DERPOS(I,N,J)/BCALC(N)
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE BONTRI(N1,N2,N3,K,IE)
      SUBROUTINE BONTRI(N1,N2,N3,K,IE)
C
C *** BONTRI by JCM 17 Oct 1990 ***
C
CX
CC 8B
CH Given two bonds with a common atom, completes the triangle and identifies
CH the atom.
CA On entry N1 and N2 are the pointers to 2 bonds (in the list generated by
CA          L ATOM and L BOND cards)
CA On exit N3 points to the third bond completing the triangle
CA         K points to the common atom, opposite N3 in the triangle.
CA         and N1 and N2 are possibly interchanged; the result is N1 < N2.
CA         IE is an error indicator, set = 0 if OK, non-zero if error.
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      IE=0
C FIND THIRD BOND:
      IF (N1 .GT. N2) CALL FLIP(N2,N1)
      I1=0
      DO 3 I=1,2
      DO 3 J=1,2
      IF (IATM(N1,I) .EQ. IATM(N2,J)) THEN
        K=IATM(N1,I)
        I1=3-I
        I2=3-J
      ENDIF
   3  CONTINUE
      IF (I1 .EQ. 0) THEN
        WRITE (LPT,3031) BONNAM(N1),BONNAM(N2)
        WRITE (ITO,3031) BONNAM(N1),BONNAM(N2)
3031    FORMAT (/'  ERROR ** bonds ',A4,' and ',A4,
     &  ' have no common atom')
        IE=1
        GO TO 100
      ENDIF
C ADD THIRD SIDE OF TRIANGLE TO LIST OF INVOLVED BONDS;  IF IT IS NOT
C ALREADY THERE IT WILL BE GIVEN A BOGUS NAME:
      CALL ADDBON('    ',IATM(N1,I1),IATM(N2,I2),N3)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CALCDS(H)
      SUBROUTINE CALCDS(H)
C
C *** CALCDS by JCM 31 Jan 85 ***
C
CX
CC 7B
CH Calculates  d star squared, and its derivatives wrt reciprocal
CH cell quadratic products.
CA On entry H is a 1x3 vector holding h,k,l
CP In /CELPAR/ CPARS(1:6,2) should hold the 6 reciprocal quadratic products
CP             A* = a* squared
CP             D* =  b* c* cos alpha* etc
CP             KCPARS(1:6) should hold fix/vary information for each of the 6
CP             reciprocal cell quadratic products.
CD Sets GCALC in /OBSCAL/ to be d star squared, the d spacing squared,
CD and DERIVV in /DERVAR/ to be the derivative, where relevant
C
      DIMENSION H(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
C
      GCALC=0.
      J=2
      K=3
      DO 1 I=1,3
      C1=H(I)*H(I)
      C2=2.*H(J)*H(K)
      GCALC=GCALC+C1*CPARS(I,2)+C2*CPARS(I+3,2)
      L1=KCPARS(I)
      L2=KCPARS(I+3)
      IF (L1 .GT. 0) DERIVV(L1)=C1
      IF (L2 .GT. 0) DERIVV(L2)=C2
      J=K
   1  K=I
C
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CALCFW(S)
      SUBROUTINE CALCFW(S)
C
C *** CALCFW by JCM 19 Nov 84 ***
C
CX
CC 7B
CH Calculates the Forsyth & Wells exponential function which approximates
CH to observed scattering factor curves.
CA On entry S is sin theta.
C
CP The relevant number of coeffivients (usually 5, 7 or 9) are held in
CP /FWVALS/ as COEFFS(1:NVALS)
CD Used in FWLSQ as a very simple LSQ application, with no constraints.
CD Sets GCALC = Forsyth & Wells sum of exponential terms,
CD and DERIVV in /DERVAR/ as NVALS derivatives.
CN PJB says it doesn't work;  this must be investigated.
C
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /FWVALS/NVALS,COEFFS(9)
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
C
      SSQRD=S*S
      GCALC=COEFFS(NVALS)
      DERIVV(NVALS)=1.0
C
      DO 1 I=3,NVALS,2
      J=NVALS-I+2
      TEX=EXP(-COEFFS(J)*SSQRD)
      TERM=COEFFS(J-1)*TEX
      GCALC=GCALC+TERM
      DERIVV(J)=-SSQRD*TERM
   1  DERIVV(J-1)=TEX
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE CALCGR(H,FAC)
      SUBROUTINE CALCGR(H,FAC)
C
C *** CALCGR by JCM 18 Feb 85 ***
C
CX
CC 7B
CH Gives the calculated function for grouped single crystal Least Squares.
CA On entry H is a 1x3 real array holding h,k,l
CA          FAC is the factor by which to multiply GCALC before summing
CP SETFC (or its constituent parts SYMOP, RECIP etc) must have set up the
CP    structure factor calculation.
CP EXTINC must have set up the extinction corrections
CP LSETUP, PARSSF and VARMAK must have set up the Least Squares
CP STLSSF must have read the L cards.
CD Forms GCALC and sums it in YCALC, multiplied by FAC. Also derivatives of
CD GCALC with respect to any parameters, and summs them likewise into DERIVV.
CD GCALC is the product of 3 functions, P1, P2, P3
CD P1 is the scale * overall itf * FAC (at present in GRLSQ FAC=multiplicity
CD    times d to the power 4)
CD P2 is the square of the structure factor.
CD P3 is an extinction correction, possibly absent.
CN Only IREF=1 makes sense here.
C
      DIMENSION H(3)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /EXTN/IEXTYP,DOMR,KDOMR,AMOSC,KMOSC,EXTCOR,CEXT(4),
     & DEXDFQ,DEXDRQ,DEXDGQ,LOREN,GAUSS
      LOGICAL LOREN,GAUSS
      COMMON /FCAL/FC,FCMOD,COSAL,SINAL,FCDERS(300),DERIVT(300)
      COMPLEX FC,DERIVT
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /OVER/ITFAC,OTFAC(10),KOTFAC(10),NTFAC,JTFAC,KOM15
      EQUIVALENCE (TFAC,OTFAC(1)),(KTFAC,KOTFAC(1))
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
      COMMON /SCLDAT/ISCALE,NSCALE,SCALE(20),KSCALE(20),
     & NSCL,LSCD(10)
C%
C      DIMENSION DERIVA(%VVAR%)
      DIMENSION DERIVA(500)
C
      STHL = VCTMOD(0.5,H,2)
C STHL=SIN THETA/LAMBDA;  SSQRD = IT SQUARED
      SSQRD = STHL*STHL
C
C CLEAR INTERNAL DERIVATIVES:
      IF (LVARV .GT. 0) CALL GMZER(DERIVA,1,LVARV)
C
C FORM P1, THE PART OF GCALC INDEPENDENT OF MOD(FC)
   1  P1=SCALE(ISCALE)*EXP(-TFAC*SSQRD)*FAC
      IF (KTFAC .GT. 0) DERIVA(KTFAC)=-SSQRD
      L=KSCALE(ISCALE)
      IF (L .NE. 0) DERIVA(L)=1./SCALE(ISCALE)
C NEXT DO P2:
      CALL LFCALC(H)
      P2=FCMOD*FCMOD
      DP2DFQ=2./FCMOD
C
C P3:
      CALL EXTINC(3,FCMOD)
      P3=EXTCOR*EXTCOR
      IF (KDOMR .NE. 0) DERIVA(KDOMR)=2.*DEXDRQ
      IF (KMOSC .NE. 0) DERIVA(KMOSC)=2.*DEXDGQ
C
C CONVERT DERIVATIVES FOR FAMILY 2 FROM BEING 'OF FCMOD' AS THEY ARE OUT OF
C LFCALC, USING:
C DP2/DV = DMODFC/DV * DP2/DMODFC - AND ALL ARE DIVIDED BY P2
C DP3/DV = DMODFC/DV * DP3/DMODFC - AND ALL ARE DIVIDED BY P3
C GIVING D(GCALC)/DV OVER GCALC=(DP2/DV OVER P2 + DP3/DV OVER P3)
      DO 4 I=1,NVARF(2,1,1)
   4  DERIVA(LVFST1(2,1,1)+I)=FCDERS(I)*(DP2DFQ+2.*DEXDFQ)
C
   3  GCALC = P1*P2*P3
      YCALC=YCALC+GCALC
C
      DO 6 I=1,LVARV
   6  DERIVV(I)=DERIVV(I) + DERIVA(I)*GCALC
 100  RETURN
      END
C
C


C

C



C
C
C LEVEL 3      SUBROUTINE CARDIN(IDEN)
      SUBROUTINE CARDIN(IDEN)
C
C *** CARDIN by JCM 2 Feb 88 ***
C
CX
CC 13C
CH Finds the record number IDEN in the DIRECT ACCESS file on unit IO10,
CH which is a copy of the Crystal Data File.
CA On entry IDEN holds the number of the required record.
C
CD Reads card in format A80 to ICARD, leaving this to be interpreted elsewhere.
CD Ignores Y and Z cards, which were not counted when PREFIN read the data.
CD Counts those it ignores in NYZ, and so that calling routines know if last
CD card is Y or Z, sets NYZ=-1 in those cases.
CD
CD Complains if record IDEN is not present.
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      NYZ=0
C
C CHECK NOT TRYING TO READ SILLY RECORD:
      ILOW=0
      IF (JPHASE .GT. 1) ILOW=NTOTAL(JPHASE-1)
      IHI=NTOTAL(JPHASE)
      IF (IDEN .LE. ILOW .OR. IDEN .GT. IHI) CALL ERRIN2(IDEN,0,
     & 'record','requested from crystal data is not there')
C
C CATCH ALSO CDFS ENDING IN Y OR Z CARDS:
   1  IF (IDEN+NYZ .GT. IHI) GO TO 101
      READ (IO10,REC=IDEN+NYZ,FMT=1001) ICARD
1001  FORMAT (A80)
      NYZ=NYZ+1
      IF (ICARD(1:1) .EQ. 'Y' .OR. ICARD(1:1) .EQ. 'Z') GO TO 1
      GO TO 100
C
 101  NYZ=-1
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE CDFIN(NUMCDF,ID,ENDIP)
      SUBROUTINE CDFIN(NUMCDF,ID,ENDIP)
C
C *** CDFIN updated by JCM 21 Jan 92 ***
C
CX
CC 13C
CH Reads in one Crystal Data File and copies it to direct access unit number
CH IO10, starting at record number ID.
CA  On entry NUMCDF = which crystal data file is required;  1 in MK3
CA        ID is the previously used record of DIRECT ACCESS file IO10
CA        in COMMON /CARDRC/ ICRYDA=unit from which to read
CA On exit  ID has been advanced and gives total records on IO10
CA        ENDIP is TRUE if the end of the file hs been reached.
CD Writes to /CARDRC/ the following:
CD        NTOTAL(NUMCDF)=number of records for phase NUMCDF
CD        INREA(26,NUMCDF)=absolute starting addresses for letters
CD        ICDN(26,NUMCDF)=numbers of cards starting with letters
CD Also detects presence of I OUTP item, and unless a non-zero value is
CD already in IOUT in /IOUNIT/, interprets the integer after the I OUTP.
CD
CD Also detects if it is reading phase 1 of a multiphase job, because it must
CD form the union of this with "phase 0"
CD
CO Writes Y cards straight to unit LPT.
CI Reads crystal data sets from unit ICRYDA.
CO Lists on LPT numbers of each type of card read.
CN Modified to ignore empty cdfs (e.g. if the file inadvertently starts with
CN a non-letter)
C
      CHARACTER *4 MWORD
      CHARACTER *1 ISS
      CHARACTER *10 FILNOM
      LOGICAL GETM,ENDIP,ONCARD
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PHAS0/INRLP0,ICDLP0,INRLP1,ICDLP1,NCDF0
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IDIN=ID
      ENDIP=.FALSE.
      N1=NUMCDF
C UNLESS PHASE 1 FOLLOWING PHASE 0, FOR EACH LETTER, SET NO CARDS YET READ:
      IF (.NOT. MULFAS .OR. (NPHASE .NE. 1)) THEN
        DO 1 I = 1,26
        ICDN(I,N1) = 0
    1   INREA(I,N1) = 1
      ELSE
C CLEAR ONLY THE COUNT OF L CARDS:
        ICDN(12,N1)=0
        INREA(12,N1)=1
C AND NOTE THE END OF PHASE 0, FOR THE NEW CDF:
        NCDF0=NTOTAL(1)
      ENDIF
C SET HAVE NOT READ CARD STARTING "M GET" ASKING TO UNDUMP PREVIOUS MAP:
      GETM=.FALSE.
C THE DATASET IS COPIED ACROSS WITH Y AND Z CARDS,
C AND THE STARTING LINE NUMBER FOR EACH LETTER IS RECORDED IN INREA.
C ICDN SAYS HOW MANY CARDS OF THAT LETTER HAVE BEEN READ:
C INLET HOLDS SERIAL NUMBER OF PREVIOUS STORED CARD'S LETTER:
      INLET=-999
   8  READ (ICRYDA,1000,END=2) ICARD
1000  FORMAT (A80)
C IGNORE BLANK LINES ANYWHERE:
      LEN=LENGT(ICARD)
      IF (LEN .EQ. 0) GO TO 8
      L=LETTER(ICARD(1:1))
C THIS GIVES AN ANSWER IN RANGE 1-26, BUT ACCEPTS LOWER CASE
      IF (L .EQ. 0) GO TO 12
C
C LETTER RECOGNISED:
      IF (L .NE. 25) GO TO 3
C Y CARDS COPIED ON TO OUTPUT:
      WRITE (LPT,2004) (ICARD(J:J),J=2,LEN)
2004  FORMAT (' ',79A1)
      GO TO 7
C
C Z CARDS TOTALLY IGNORED:
   3  IF (L .EQ. 26) GO TO 7
C
C CARDS OTHER THAN Y AND Z:
C JUMP IF SAME INITIAL LETTER AS PREVIOUSLY STORED CARD:
      IF (INLET .EQ. L) GO TO 4
C
C NEW BLOCK - CHECK NOT HAD ANY OF THIS LETTER BEFORE:
      IF (ICDN(L,N1) .NE. 0) GO TO 6
      INREA(L,N1) = ID+1
      INLET=L
C JOIN HERE IN MIDDLE OF BLOCK:
   4  ICDN(L,N1) = ICDN(L,N1) + 1
      IF (L .EQ. 13) THEN
        CALL RDWORD(MWORD,ITEMP,3,IPT,80,0,IER)
        GETM = (MWORD .EQ. 'GET')
      ENDIF
   7  ID=ID+1
      WRITE (IO10,REC=ID,FMT=1000) ICARD
      GO TO 8
C
C ERROR ON SHUFFLED CARDS:
   6  CALL ERRCH2(ICARD(1:1),0,
     & 'more than one group of cards labelled',
     & 'found on crystal data file')
C
C END OF CRYSTAL DATA:
   2  ENDIP=.TRUE.
  12  IF (.NOT. ENDIP .AND. ID .EQ. IDIN) GO TO 8
      IF (GETM) CALL MAJUST
      IF (N1 .EQ. 1) CALL MESS(LPT,1,
     & 'Data read by PREFIN from file '//FILNOM(ICRYDA))
      IF (N1 .GT. 1) CALL MESS(LPT,1,
     & 'Next phase data read by PREFIN from file '//FILNOM(ICRYDA))
      DO 5 I = 1,26
      IF (ICDN(I,N1) .EQ. 0) GO TO 5
      ISS=' '
      IF (ICDN(I,N1) .GT. 1) ISS='s'
      WRITE (LPT,2001) ICDN(I,N1),ISS,LETUP(I)
2001  FORMAT (5X,I4,' card',A1,'  labelled ',A1)
    5 CONTINUE
C
      NTOTAL(N1)=ID
      IF (IOUT .EQ. 0) THEN
        IF (ONCARD('I','OUTP',A)) IOUT=NINT(A)
        IF (IOUT .NE. 0) WRITE (LPT,2000) IOUT
 2000   FORMAT (/' Diagnostic output at number',I5,' required'/)
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE CDSCAN(CH,WORDS,LEN,K,LCD,NW)
      SUBROUTINE CDSCAN(CH,WORDS,LEN,K,LCD,NW)
C
C *** CDSCAN updated by JCM 2 Feb 88 ***
C
CX
CC 13C
CH Finds the next card which starts with the letter given in CH, and has
CH then a word which is one of the collection given in WORDS.
CA On entry CH is the A1 character to start the card
CA          WORDS is an A4 array of length LEN with possible next words
CA          K is 0 if to search from start, otherwise where to start
CA           - i.e. we search from the K+1th card of the crystal data
CA On exit  LCD = which card, if found
CA             = -1 if none found starting CH
CA             = 0 if some start CH, but none of WORDS come next
CA          NW = which word of WORDS was found
CD sets a copy of the found card into ICARD
C
      CHARACTER *1 CH
      CHARACTER *4 WORDS(LEN),WORD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      L=-1
      I=LETTER(CH)
C
C IF NO CH CARDS AT ALL, EXIT:
      IF (ICDNO(I) .EQ. 0) GO TO 101
      ID=K+1
      IF (K .EQ. 0) ID=IABS(INREAD(I))
C
C READ NEXT CARD:
   1  IF (ID .GT. NTOTAL(JPHASE)) GO TO 102
      CALL CARDIN(ID)
C  WE HAVE TO DO SOMETHING ABOUT CDFS ENFDING Y OR Z
      IF (NYZ .EQ. -1) GO TO 102
      ID=ID+NYZ
      L=ID-1
C CHECK WE STILL HAVE CH CARDS:
      IF (ICARD(1:1) .NE. CH) GO TO 102
C READ WORD STARTING AT POSITION 3:
      CALL RDWORD(WORD,J,3,IPT,80,0,IER)
C SCAN POSSIBLE VOCABULARY OF NEXT WORDS:
      DO 2 NW=1,LEN
      IF (WORD .EQ. WORDS(NW)) GO TO 101
   2  CONTINUE
C "WORD" ON THIS CARD DOES NOT MATCH - TRY NEXT "CH" CARD:
      GO TO 1
 102  L=0
 101  LCD=L
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CELDER(H,DERS)
      SUBROUTINE CELDER(H,DERS)
C
C *** CELDER updated by JCM  7 Sep 88 ***
C
CX
CC 6B
CH From h,k,l calculates d* squared and its derivatives, and sets SSQRD.
CA On entry H is a real 1x3 vector holding h,k,l
CA On exit DERS is a real 1x6 array holding the derivatives of d* squared
CA              wrt A*, B*, C*, D*, E*, F*
CP CPARS in /CELPAR/ must contain A*, B*, C*, D*, E*, F*
CD Also sets SSQRD in /BRAGG/ to be s squared (=d* squared/4)
CD STHL in /BRAGG/ to be sin theta/lambda, s, or d*/2
CD and DSTAR2 in /BRAGG/ to be d* squared
C
      DIMENSION H(3),DERS(6)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
C
      DSDS=0.
      J=2
      K=3
      DO 1 I=1,3
      DERS(I)=H(I)*H(I)
      DERS(I+3)=2.*H(J)*H(K)
      DSDS=DSDS+DERS(I)*CPARS(I,2)+DERS(I+3)*CPARS(I+3,2)
      J=K
   1  K=I
      SSQRD=DSDS/4.
      DSTAR2=DSDS
      STHL=SQRT(SSQRD)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CELMAT(TOSTAR)
      SUBROUTINE CELMAT(TOSTAR)
C
C *** CELMAT by JCM 17 Aug 89 ***
C
CX
CC 1B
CH Sets up the matrix to convert derivatives wrt A,B,C . . (cell quadratic
CH products in real space) to derivatives wrt A*, B*, C*, . . in
CH reciprocal space.
CA On exit TOSTAR is the required 6x6 matrix
CP On entry CPARS(1:6,1) contain the real space cell quadratic products,
CP A=a sqrd, B=b sqrd, C=c sqrd D=b c cos alpha, etc
C
      DIMENSION TOSTAR(6,6)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
C
C FIRST COPY OUT A B C D E F FOR SANITY:
      A=CPARS(1,1)
      B=CPARS(2,1)
      C=CPARS(3,1)
      D=CPARS(4,1)
      E=CPARS(5,1)
      F=CPARS(6,1)
C THERE WILL BE CLEVERER WAYS OF DOING THIS, BUT LETS GET IT RIGHT FIRST:
      TOSTAR(1,1)=-A*A
      TOSTAR(1,2)=-F*F
      TOSTAR(1,3)=-E*E
      TOSTAR(1,4)=-F*E
      TOSTAR(1,5)=-E*A
      TOSTAR(1,6)=-A*F
      TOSTAR(2,1)=-F*F
      TOSTAR(2,2)=-B*B
      TOSTAR(2,3)=-D*D
      TOSTAR(2,4)=-B*D
      TOSTAR(2,5)=-D*F
      TOSTAR(2,6)=-F*B
      TOSTAR(3,1)=-E*E
      TOSTAR(3,2)=-D*D
      TOSTAR(3,3)=-C*C
      TOSTAR(3,4)=-D*C
      TOSTAR(3,5)=-C*E
      TOSTAR(3,6)=-E*D
C
      TOSTAR(4,1)=-2.*E*F
      TOSTAR(4,2)=-2.*B*D
      TOSTAR(4,3)=-2.*D*C
      TOSTAR(4,4)=-B*C-D*D
      TOSTAR(4,5)=-C*F-D*E
      TOSTAR(4,6)=-B*E-D*F
      TOSTAR(5,1)=-2.*A*E
      TOSTAR(5,2)=-2.*D*F
      TOSTAR(5,3)=-2.*C*E
      TOSTAR(5,4)=-C*F-D*E
      TOSTAR(5,5)=-A*C-E*E
      TOSTAR(5,6)=-A*D-E*F
      TOSTAR(6,1)=-2.*A*F
      TOSTAR(6,2)=-2.*B*F
      TOSTAR(6,3)=-2.*D*E
      TOSTAR(6,4)=-B*E-D*F
      TOSTAR(6,5)=-A*D-E*F
      TOSTAR(6,6)=-A*B-F*F
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE CELNEW
      SUBROUTINE CELNEW
C
C *** CELNEW updated by PJB 14-Dec-94 ***
C
CX
CC 6C
CH Writes out a new C card after cell parameter refinement.
CP CELL in /CELPAR/ should contain a,b,c, cos alpha, beta, gamma
CP CELESD  in /CELPAR/ should contain their esds
CO Writes to unit NEWIN a new C card
CN Does not preserve any blanks originally left for symmetry.
C
      DIMENSION ANG(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
C MAY BE SD CARD:
      IF (ICARD(3:4) .EQ. 'SD') THEN
        WRITE (NEWIN,2001) (CELESD(I,I,1),I=1,6)
2001    FORMAT ('C SD',3F10.5,3F10.3)
        GO TO 100
      ENDIF
C
      DO 1 I=1,3
   1  ANG(I)=DEGREE(ARCCOS(CELL(I,2,1)))
      WRITE (NEWIN,2000) (CELL(I,1,1),I=1,3),(ANG(I),I=1,3)
2000  FORMAT ('C ',3F10.5,3F10.3)
 100  RETURN
      END
C
C
C
C
C LEVEL 9      SUBROUTINE CELREL(IFAM,IGEN,ISPC)
      SUBROUTINE CELREL(IFAM,IGEN,ISPC)
C
C *** CELREL updated by JCM 2 May 90 ***
C
CX
CC 6A
CH Transfers any  relations which exist between cell parameters from
CH their own COMMON to the general "contraint/fixing" COMMON.
CA On entry IFAM, IGEN, ISPC designate the first parameter, A*
CD Moves the constraint and/or fixing information from /CELFIX/ to
CD join the general initial fix/constrain info.
CD Also keep starting family, genus, species for later consultation in
CD NCELF,NCELG,NCELS
C
CN Cell parameters are source-independent (KSOURC=1) but may be phase dependent
CN B*, C* etc are assumed to have ISPC going sequentially up in 1's from A*
C
      DIMENSION NCOUNT(6)
      COMMON /CELFIX/IPTCEL(6),AMCELL(6),NCELF,NCELG,NCELS,KOM3
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
C
C SET UP LIST OF 6 KK VALUES:
      DO 5 I=1,6
   5  NCOUNT(I)=KPAK(IFAM,IGEN,ISPC+I-1,JPHASE,1)
      CALL FIXREL(6,IPTCEL,AMCELL,NCOUNT,5)
      NCELF=IFAM
      NCELG=IGEN
      NCELS=ISPC
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE CELSDP(ALSQ,MATSZ)
      SUBROUTINE CELSDP(ALSQ,MATSZ)
C
C *** CELSDP by JCM 17 Aug 89 ***
C
CX
CC 6C
CH Prints the esds of real cell parameters after a cycle of refinement.
CA ALSQ, MATSZ need to be handed around in routine calls
CP ALSQ must hold the inverse LSQ matrix
CP CPARS holds A,B,C,D,E,F, the real cell quadratic products, and
CP CELL the result of reading a C card.
CO If a variance is negative, writes 0.
C
      DIMENSION ALSQ(MATSZ)
      CHARACTER *1 SI(3)
      CHARACTER *5 AN(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      DATA SI/'a','b','c'/
      DATA AN/'alpha',' beta','gamma'/
C
      CALL MATCEL(ALSQ,MATSZ)
      CALL MESS(LPT,2,
     & ' ************* LATTICE CONSTANTS *************')
      CALL NEWLIN(LPT)
C
      DO 1 I=1,6
      C=SQRT(AMAX1(CELLSD(I,I),0.))
      IF (I .LT. 4) WRITE(LPT,2001) SI(I),CELL(I,1,1),C
 2001 FORMAT('        ',A1,' = ',F8.5,' +/- ',F6.5)
      IF (I .GE. 4) WRITE(LPT,2002) AN(I-3),
     & DEGREE(ACOS(CELL(I-3,2,1))),C
2002  FORMAT('    ',A5,' = ',F8.4,' +/- ',F6.4)
   1  CONTINUE
C
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE CELSHF(N)
      SUBROUTINE CELSHF(N)
C
C *** CELSHF updated by JCM 10 Feb 87 ***
C
CX
CC 6C
CH Applies a shift to a cell quadratic product.
CA N= which parameter; 1=A*, 2=B*, 3=C* etc
C
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
C
      CALL ADJUST(CPARS(N,2))
      GO TO 100
C
C
C TO SET ALL CELL PARAMETERS FIXED, OR VARY ONE:
      ENTRY CELVAR(N,NV)
      IF (N .EQ. 0) THEN
        DO 1 I=1,6
   1    KCPARS(I)=0
      ELSE
        KCPARS(N)=NV
      ENDIF
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE CENTRE(LUNIT,N,TXT,NWIDE)
      SUBROUTINE CENTRE(LUNIT,N,TXT,NWIDE)
C
C *** CENTRE by JCM 12 Sep 92 ***
C
CX
CC 13C
CH Writes on unit LUNIT the given message, centred & preceded by N empty lines.
CA On entry TXT is a CHARACTER variable holding the message,
CA          N is in integer requesting N empty lines before the message, and
CA            may be 0, for no lines
CA            or > 98, for a page throw.
CA          LUNIT is the unit on which to write.
CA          NWIDE is the width of page in which the text is to be centred.
CO Writes to unit LUNIT N empty lines or a page throw, then the given text with
CO a "space" carriage control, centred within NWIDE spaces.
C
      CHARACTER *(*) TXT
C
      IF (N .LT. 99) THEN
        DO 1 I=1,N
   1    WRITE (LUNIT,2000)
2000    FORMAT (1X)
      ELSE
        WRITE (LUNIT,2002)
2002    FORMAT ('1')
      ENDIF
      L=LENGT(TXT)
      M=0
C THIS SHOULD ROUND BY MOVING THE TEXT 1 PLACE LEFTWARDS:
      IF (L .LT. NWIDE) M=(NWIDE-L)/2
      WRITE (LUNIT,2001) (' ',I=1,M),(TXT(I:I),I=1,L)
2001  FORMAT (1X,200A1)
      RETURN
      END
C
C

C
C
C LEVEL 1      SUBROUTINE CLOFIL(LUN)
      SUBROUTINE CLOFIL(LUN)
C
C *** CLOFIL by PJB Jan 86 ***
C
CX
CC 13C
CH Closes the FORTRAN unit LUN and returns the CCSL unit to the pool.
CA On entry LUN is the number of an existing FORTRAN unit, now finished with.
CP NOPFIL (or OPNFIL) should have set up LUN when the file was opened.
CD Closes LUN;  releases its table entries IOTAB in /LOONEY and FILNAM in
CD /FINAME.
CD
CD If LUN was not in such table entries, does nothing.
C
      COMMON /FINAME/FILNAM(15)
      CHARACTER *10 FILNAM
      COMMON /LOONEY/IOTAB(15),LUNTAB(15)
C
      CLOSE (LUN)
C%
C      DO 1 I=1,%FILE%
      DO 1 I=1,15
      IF (LUNTAB(I).NE.LUN) GO TO 1
      IOTAB(I)=0
      FILNAM(I)=' '
      GO TO 100
    1 CONTINUE
  100 RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION CONATF(N,IA)
      FUNCTION CONATF(N,IA)
C
C *** CONATF by JCM 16 Nov 84 ***
C
CX
CC 4B
CH Produces for a single coefficient of an anisotropic temperature
CH factor, its conversion factor from the internally used betas, in
CH order to communicate with the user.
CA On entry IA says which ATF
CA           N says which of the coefficients, in the program's ordering
CA             11 22 33 23 13 12
CA On exit CONATF is the required muliplicative conversion factor.
CN It is inefficient, but not often used.
C
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      I=IATYP(IA)
      GO TO (2,2,3,4,5) , I
C BRANCH ON TYPE OF ATF GIVEN BY USER - THESE TYPES ARE AS IN MK2, BUT 1 WAS
C FOUND TO BE BAD AND WAS REMOVED.
C
C  TYPE 2 - AS IN HEWAT PROFILE REFINEMENT -  NO COSINES
   2  FAC=4.
      GO TO 30
C
C  TYPE 3 U'S AS IN EXP-2*PI*PI(ETC)
   3  FAC=1./(TWOPI*PI)
  30  GO TO (21,21,21,24,25,26) , N
  21  C=FAC/CPARS(N,2)
      GO TO 101
  24  C=FAC/(CELL(2,1,2)*CELL(3,1,2))
      GO TO 101
  25  C=FAC/(CELL(1,1,2)*CELL(3,1,2))
      GO TO 101
  26  C=FAC/(CELL(1,1,2)*CELL(2,1,2))
      GO TO 101
C
C
C  TYPE 4 - ONLY THE 2'S MISSING
   4  GO TO (41,41,41,42,42,42) , N
  41  C=1.0
      GO TO 101
  42  C=2.0
      GO TO 101
C
C
   5  C=1.
 101  CONATF=C
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE CONVMP(MODE)
      SUBROUTINE CONVMP(MODE)
C
C *** CONVMP updated by JCM 20 Apr 90 ***
C
CX
CC 7B
CH Converts between user values and LSQ parameters for multipoles.
CA On entry MODE = the action required:
CA MODE=1 convert user values in POLAMP(,1) to LSQ parameters in POLAMP(,2)
CA MODE=2 convert LSQ obtained ESD's to user EDS's, and
CA        convert LSQ shifts in POLAMP(,4) to user values in POLAMP(,3)
CP In /MPODA/ CONMAT must have been set up by ORTFUN
CP            POLAMP must contain the user values, read by ORTFUN
CP            KCLUMP must contain clump sizes set in ORTFUN
CP            NCLUMP must be number of clumps set in ORTFUN
C
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
C
      DIMENSION TMP(13)
C
      IA=1
      ICMAT=1
C SCAN NCLUMP CLUMPS:
      DO 4 K=1,NCLUMP
      JJ=KCLUMP(K)
      IF (MODE .EQ. 1) THEN
        CALL GMPRD(CONMAT(ICMAT,2),POLAMP(IA,1),POLAMP(IA,2),JJ,JJ,1)
      ELSE
        CALL GMPRD(CONMAT(ICMAT,1),POLAMP(IA,4),POLAMP(IA,3),JJ,JJ,1)
C  PROCEDURE FOR ESD'S
        DO 2 I=1,JJ
        TMP(I)=0
        IJ=I-1
        DO 3 J=1,JJ
        TMP(I)=TMP(I)+(POLAMP(IA+J-1,6)*CONMAT(ICMAT+IJ,1))**2
        IJ=IJ+JJ
    3   CONTINUE
        POLAMP(IA+I-1,5)=SQRT(TMP(I))
    2   CONTINUE
      ENDIF
C
      IA=IA+KCLUMP(K)
      ICMAT=ICMAT+KCLUMP(K)*KCLUMP(K)
   4  CONTINUE
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE DEPRIN(IPRNT)
      SUBROUTINE DEPRIN(IPRNT)
C
C *** DEPRIN by JCM 11 Feb 80 ***
C
CX
CC 6C
CH Decodes an integer which describes the frequency of LSQ printing
CH required, and outputs this frequency.
CA On entry IPRNT = 0 for 'never'
CA                  1 for 'first cycle'
CA                  2 for 'last cycle'
CA                  3 for 'first and last cycles'
CA                  4 for 'every cycle'
CP An explanatory message is assumed to have been previously output
CD Interprets IPRNT as read from, usually, I card under such headings as
CD    'PRIN', 'PRFC', 'PRSK' etc, depending on the calling program
CO Outputs suitable message
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (IPRNT .EQ. 1) THEN
        CALL MESS(LPT,0,'first cycle')
      ELSE IF (IPRNT .EQ. 2) THEN
        CALL MESS(LPT,0,'last cycle')
      ELSE IF (IPRNT .EQ. 3) THEN
        CALL MESS(LPT,0,'first and last cycles')
      ELSE IF (IPRNT .EQ. 4) THEN
        CALL MESS(LPT,0,'every cycle')
      ELSE
        CALL MESS(LPT,0,'never')
      ENDIF
      RETURN
      END
C
C



C
C
C LEVEL 4      SUBROUTINE DIJROT(D,ALPHA,BETA,GAMMA,KMAX)
      SUBROUTINE DIJROT(D,ALPHA,BETA,GAMMA,KMAX)
C
C *** DIJROT renamed by PJB C17 15-Sept-93 ***
C
CX
CC 9C
CH Calculates the matrix D(i,j) for the Euler rotations alpha,beta,gamma
CH of the eigenfunctions of angular momentum l.
CA On entry KMAX gives the multiplicity = 2l+1, and hence the dimension of D.
CA ALPHA, BETA GAMMA in radians are the rotations
CA On exit, D is a COMPLEX matrix
CO If IOUT in /IOUNIT/ is > 100, outputs D.
CN     i = l+m1+1,j = l+m2+1, so that
CN     when i=j=k,m1=m2=l and when i=j=1,m1=m2=-l
C
      DIMENSION C(21),S(21)
      COMPLEX D(KMAX,KMAX),EALPHA(21),EGAMMA(21),EAL,EGA
      EQUIVALENCE (C,EALPHA),(S,EGAMMA)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      SINB = SIN(0.5*BETA)
      COSB = COS(0.5*BETA)
      S(1)=1.
      C(1)=1.
      DO 8 K=2,KMAX
      S(K)=S(K-1)*SINB
    8 C(K)=C(K-1)*COSB
      D(1,1) = CMPLX(COS(0.5*BETA),0.)
      D(1,2) = -CMPLX(SIN(0.5*BETA),0.)
      D(2,2) = D(1,1)
      D(2,1) = -D(1,2)
      IF (KMAX .LT. 3) GO TO 4
      K = 2
    3 DO 1 M = 1,K
      I = K-M+1
      DO 2 J = 1,K
      X = SQRT((FLOAT(K-I+1))/FLOAT(K-J+1))
      D(I,J) = X*D(I,J)*COSB
      IF (I .EQ. 1) GO TO 2
      X = SQRT((FLOAT(I-1))/FLOAT(K-J+1))
      D(I,J) = D(I,J) + X*D(I-1,J)*SINB
    2 CONTINUE
    1 CONTINUE
      K = K + 1
      DO 5 I = 1,K
      X = SQRT((FACT(K-1))/(FACT(K-I)*FACT(I-1)))
      SIGN = FLOAT(1-2*MOD(K-I,2))
      D(I,K) = SIGN*X*C(I)*S(K-I+1)
      D(K,I) = SIGN*D(I,K)
    5 CONTINUE
      IF (IOUT.EQ.100) THEN
        DO 20 I = 1,K
   20   WRITE (LPT,2000) (D(I,J),J=1,K)
2000    FORMAT (7(F9.4,F8.4))
        WRITE (LPT,2001)
2001    FORMAT (' ')
      ENDIF
      IF (K .LT. KMAX) GO TO 3
    4 M = (K-1)/2 + 1
      CALL TRIG (EALPHA,ALPHA,M)
      CALL TRIG (EGAMMA,GAMMA,M)
      DO 6 I = 1,K
      DO 7 J = 1,K
      EAL = EALPHA(IABS(I-M)+1)
      IF (I .LT. M) EAL = CONJG(EAL)
      EGA = EGAMMA(IABS(J-M)+1)
      IF (J .LT. M) EGA = CONJG(EGA)
      D(I,J) = D(I,J)*EGA*EAL
    7 CONTINUE
    6 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE DUMMY
      SUBROUTINE DUMMY(DUM)
C
C *** DUMMY by JCM 21 Mar 89 ***
C
CC 16C
CH Does absolutely nothing;  used as a default in routine calls.
CD The only way we can avoid unnecessary routines being loaded is to
CD pass their names through the system as arguments of other routines.
CD Sometimes we wish to set such arguments to avoid doing anything.
C
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION ELEMAT(ALSQ,MATSZ,I,J)
      FUNCTION ELEMAT(ALSQ,MATSZ,I,J)
C
C *** ELEMAT by JCM 16 Jul 87 ***
C
CX
CC 6C
CH Gets a matrix element from the triangular LSQ matrix.
CA ALSQ holds the symmetrical triangular LSQ matrix
CA MATSZ is its dimension
CA I,J ask for the particular element, as though ALSQ were square
CA ELEMAT is set on exit to the element I,J
CN ALSQ and MATSZ are passed through the whole of the LSQ system as arguments,
CN enabling MATSZ to be set and ALSQ to be dimensioned in MAIN programs.
C
C%
C      DIMENSION ALSQ(MATSZ),MM(%BVAR%)
      DIMENSION ALSQ(MATSZ),MM(400)
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      EQUIVALENCE (MM(1),MATPNT(2))
C
      IND=MM(I)+J
      IF (J .LT. I) IND=MM(J)+I
      ELEMAT=ALSQ(IND)
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE EQOP(R,T,N,L)
      SUBROUTINE EQOP(R,T,N,L)
C
C *** EQOP by JCM 28 Jun 83 ***
C
CX
CC 1B
CH Checks whether a rotation matrix and a translation vector of a symmetry
CH operator are already in a list, and adds them if not.  Also finds
CH lattice translations.
CA On entry R holds a 3x3 rotation matrix (part of a space group symmetry
CA            operator)
CA          T holds a 1x3 translation vector for the same operator.
CA          N is the number of entries in the list in /SCRAT so far
CA          L is the number of non-primitive lattice vectors so far.
CA On exit N and or L may have been increased by 1.  N may also indicate
CA which element of TSYM matched R.
C
CD Checks whether R is already in table TSYM in /SCRAT.  If not, R and T are
CD added to TSYM and TTRANS in /SCRAT, and N is incremented.
CD
CD If R occurs in TSYM table, examines T in case it gives a new lattice
CD translation.  If it does, adds that to the permanent array ALAT in /SYMDA
CD and increments L. Returns pointer to matching TSYM in N.
CD
CD Checks are made that  N<=48 and L<=4.
C
      DIMENSION R(3,3),T(3)
C%
C      COMMON /SCRAT/TSYM(3,3,%SY*2%),TTRANS(3,%SY*2%),
      COMMON /SCRAT/TSYM(3,3,48),TTRANS(3,48),
C%
C     & MLTAB(%SY*2%,%SY*2%),VEC(3)
     & MLTAB(48,48),VEC(3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
      NN=N
      DO 1 I=1,NN
      DO 2 J=1,3
      DO 3 K=1,3
      IF (ABS(TSYM(J,K,I)-R(J,K)) .GT..0001) GO TO 1
    3 CONTINUE
    2 CONTINUE
C
C MATRIX THE SAME - CHECK TRANSLATION VECTOR
      IS=0
      DO 4 K=1,3
      VEC(K) = AMOD(TTRANS(K,I)-T(K)+1.,1.)
      IF (VEC(K) .GT. .0001) IS=1
    4 CONTINUE
      IF (IS .EQ. 0) GO TO 6
    5 NL1 = L
      CALL EQPOS(ALAT,VEC,NL1,NL2,4)
      IF (NL2 .EQ. L+1) L = L+1
    6 N=I
      GO TO 100
    1 CONTINUE
C
C A NEW ROTATION MATRIX HAS BEEN FOUND:
C%
C      CALL ERRCHK(2,N,%SY*2%,0,'symmetry operators')
      CALL ERRCHK(2,N,48,0,'symmetry operators')
      CALL GMEQ(R,TSYM(1,1,N),3,3)
      CALL GMEQ(T,TTRANS(1,N),1,3)
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE EQPOS(VEC1,VEC2,N1,N2,M)
      SUBROUTINE EQPOS(VEC1,VEC2,N1,N2,M)
C
C *** EQPOS updated by JCM 13 Apr 86 ***
C
CX
CC 11C
CH Checks whether the given atom position already occurs in a given list,
CH and adds the new one if not.
CA On entry VEC2 holds a 1x3 vector giving a real space postion.
CA          VEC1 is a table of 1x3 vectors, of size (3,M)
CA          N1 is the number of entries in VEC1 so far
CD Determines whether VEC2 occurs in VEC1, disregarding multiples of unit cells.
CD If VEC2 gives a new position it is added to the list VEC1 and N2 is set to
CD N1+1.  A check is made that the total number of positions does not exceed
CD M, the maximum allowed.
CD
CD All elements of VEC1 are put into the range 0 =< X <1.
CD
CD If VEC2 does occur in the list VEC1, N2 is set to its position there.
CN M must be at least 1.
C
      DIMENSION VEC1(3,M),VEC2(3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (N1 .LT. 1) GO TO 4
      DO 1 I=1,N1
      DO 2 J=1,3
      A = AMOD(ABS(VEC1(J,I)-VEC2(J)),1.)
      IF ((A .GT. .0005) .AND. (A .LT. .9995))GO TO 1
    2 CONTINUE
C MATCH FOUND - JUMP:
      GO TO 101
    1 CONTINUE
C
C A NEW VECTOR FOUND IN VEC2 - STORE IT IN VEC1:
    4 I = N1+1
      IF (I .LE. M) GO TO 5
      WRITE (LPT,3000) M,((VEC1(J1,I1),J1=1,3),I1=1,M)
      WRITE (ITO,3000) M,((VEC1(J1,I1),J1=1,3),I1=1,M)
3000  FORMAT (/' ERROR ** more than',I3,' equivalent positions ',
     & 'found in EQPOS - list so far is:'/(1X,3F12.4))
C>> JCC 
C   Was      STOP
C>> Handle through further function
      CALL BMBOUT
      RETURN

   5  CALL GMEQ(VEC2,VEC1(1,I),1,3)
      CALL FRAC3(VEC1(1,I))
 101  N2=I
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE EQPPOS(VEC1,VEC2,N1,N2,M)
      SUBROUTINE EQPPOS(VEC1,VEC2,N1,N2,M)
C
C *** EQPPOS corrected by PJB 31-May-1994 ***
C
CX
CC 11C
CH Checks whether the given position vector is related by lattice translation
CH to one already in the given list.
CA On entry VEC2 holds a 1x3 vector giving a real space postion.
CA          VEC1 is a table of 1x3 vectors, of size (3,M)
CA          N1 is the number of entries in VEC1 so far
CD Determines whether VEC2 occurs in VEC1, disregarding multiples of
CD lattice vectors.
CD If VEC2 gives a new position it is added to the list VEC1 and N2 is set to
CD N1+1.  A check is made that the total number of positions does not exceed
CD M, the maximum allowed.
CD
CD All elements of VEC1 are put into the range 0 =< X <1.
CD
CD If VEC2 does occur in the list VEC1, N2 is set to its position there.
CN M must be at least 1.
C
      DIMENSION VEC1(3,M),VEC2(3),TVEC(3)
      LOGICAL LATVEC
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (N1 .LT. 1) GO TO 4
      DO 1 I=1,N1
      CALL GMSUB(VEC1(1,I),VEC2,TVEC,3,1)
      IF (LATVEC(TVEC)) GO TO 101
C MATCH FOUND - JUMP:
    1 CONTINUE
C
C A NEW VECTOR FOUND IN VEC2 - STORE IT IN VEC1:
    4 I = N1+1
      IF (I .GT. M) THEN
        WRITE (LPT,3000) M,((VEC1(J1,I1),J1=1,3),I1=1,M)
        WRITE (ITO,3000) M,((VEC1(J1,I1),J1=1,3),I1=1,M)
3000    FORMAT (/' ERROR ** more than',I3,' equivalent positions ',
     & 'found in EQPOS - list so far is:'/(1X,3F12.4))
C>> JCC Handle through separate function
C Was        STOP
C Now
       CALL BMBOUT
       RETURN
      ENDIF
C
      CALL GMEQ(VEC2,VEC1(1,I),1,3)
      CALL FRAC3(VEC1(1,I))
 101  N2=I
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE EQRLV(VEC1,VEC2,N1,N2,M)
      SUBROUTINE EQRLV(VEC1,VEC2,N1,N2,M)
C
C *** EQRLV by PJB Jun 88 ***
C
CX
CC 11C
CH Checks whether vectors differ by a reciprocal lattice vector.
C
CA On entry VEC1 holds a list of 1x3 vectors
CA          VEC2 holds a single 1x3 vector
CA          N1 is the number of vectors in the list VEC1
CA          M is positive if it is required to add VEC2 to list if unique.
CA On exit N2 points to the position of VEC2 in the list VEC1
CD Checks whether VEC2 is identical to, or differs by a reciprocal
CD lattice vector from any of the N1 vectors stored in VEC1.  If on entry
CD M>0 and if VEC2 is unique it is added to the list VEC1 and N2 is
CD set to N1+1; otherwise N2=which vector it matched.
CD
CD A check is made that the total number of vectors in VEC1 is <= M ,
CD the maximum allowed.
CD
CD If on entry M=0, N2 is set as above, but the new vector is not added to
CD the list.
C
      DIMENSION VEC1(3,1),VEC2(3),TVEC(3)
      LOGICAL LATABS
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      N=N1
      IF (N .LT. 1) GO TO 4
      DO 1 I=1,N
      CALL GMSUB(VEC1(1,I),VEC2,TVEC,3,1)
      IF (LATABS(TVEC)) GO TO 1
C HAVE FOUND DUPLICATE - IGNORE & SET N2 TO POINT TO IT:
      GO TO 101
   1  CONTINUE
C
C HAVE NEW VECTOR:
   4  I = N+1
      IF (M .EQ. 0) GO TO 101
      IF (I .LE. M) GO TO 5
      WRITE (LPT,3000) M,((VEC1(J,I),J=1,3),I=1,M)
      WRITE (ITO,3000) M,((VEC1(J,I),J=1,3),I=1,M)
 3000 FORMAT (' ERROR ** more than',I3,'equivalent vectors ',
     & 'formed - vectors so far are'/(1X,3E12.5))
C>> JCC Handle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C TO STORE NEW VECTOR:
   5  CALL GMEQ(VEC2,VEC1(1,I),1,3)
 101  N2=I
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE EQVEC(VEC1,VEC2,N1,N2,M)
      SUBROUTINE EQVEC(VEC1,VEC2,N1,N2,M)
C
C *** EQVEC updated by JCM 22 Oct 86 ***
C
CX
CC 11C
CH Finds a given vector in given table of vectors, or adds it as a new one.
CA On entry VEC1 holds a list of 1x3 vectors
CA          VEC2 holds a single 1x3 vector
CA          N1 is the number of vectors in the list VEC1
CA          M is positive if it is required to add VEC2 to list if unique.
CA On exit N2 points to the position of VEC2 in the list VEC1
CD Checks whether VEC2 is identical to any of the N1 vectors stored in VEC1.
CD If on entry M>0 and if VEC2 is unique it is added to the list VEC1 and N2
CD is set to N1+1; otherwise N2=which vector it matched.
CD
CD A check is made that the total number of vectors in VEC1 is <= M ,
CD the maximum allowed.
CD
CD If on entry M=0, N2 is set as above, but the new vector is not added to
CD the list.
C
C     Change by KS, Oct 95.  VEC1 size changed from (3,1) to (3,*)
C     The need for this was picked up by an array bounds check
      DIMENSION VEC1(3,*),VEC2(3)
      LOGICAL GMSAME
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      N=N1
      IF (N .LT. 1) GO TO 4
      DO 1 I=1,N
      IF (.NOT. GMSAME(VEC1(1,I),VEC2,3,0.0001)) GO TO 1
C HAVE FOUND DUPLICATE - IGNORE & SET N2 TO POINT TO IT:
      GO TO 101
   1  CONTINUE
C
C HAVE NEW VECTOR:
   4  I = N+1
      IF (M .EQ. 0) GO TO 101
      IF (I .LE. M) GO TO 5
      WRITE (LPT,3000) M,((VEC1(J,I),J=1,3),I=1,M)
      WRITE (ITO,3000) M,((VEC1(J,I),J=1,3),I=1,M)
 3000 FORMAT (' ERROR ** more than',I3,'equivalent vectors ',
     & 'formed - vectors so far are'/(1X,3E12.5))
C>> JCC Handle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C TO STORE NEW VECTOR:
   5  CALL GMEQ(VEC2,VEC1(1,I),1,3)
 101  N2=I
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ERRATM(NAME,NACT,MESS)
      SUBROUTINE ERRATM(NAME,NACT,MESS)
C
C *** ERRATM by JCM 25 Sep 89 ***
C
CX
CC 13C
CH Writes an error message to say that the given name is not an atom name;
CH there is a choice of subsequent action.
CA On entry, NAME is the A4 non-atom name
CA On entry NACT says which action is required:
CA    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
CA    NACT -ve means complain and exit
CA    NACT =0 means complain and stop
CA   IABS(NACT)=1 just gives atom name
CA   IABS(NACT)=2 also writes out ICARD from /SCRACH
CA On entry MESS is the message specific to this error state
CO Writes message on units LPT and ITO.
C
      CHARACTER *(*) MESS
      CHARACTER *4 NAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      L=LENGT(MESS)
      IF (NACT .GT. 0) IERR=IERR+1
      WRITE (LPT,3001) NAME,(MESS(I:I),I=1,L)
      WRITE (ITO,3001) NAME,(MESS(I:I),I=1,L)
3001  FORMAT (/' ERROR ** ',A4,' is not an atom name - on ',80A1)
      IF (IABS(NACT) .EQ. 2) THEN
        WRITE (LPT,2001) ICARD
        WRITE (ITO,2001) ICARD
2001    FORMAT (' Card says:'/1X,A80)
      ENDIF
C
C>> JCC Handle through extra function
C Was    IF (NACT .EQ. 0)  STOP
C Now
      IF (NACT .EQ. 0) CALL BMBOUT
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ERRCH2(WORD,NACT,MESS1,MESS2)
      SUBROUTINE ERRCH2(WORD,NACT,MESS1,MESS2)
C
C *** ERRCH2 by JCM 25 Sep 89 ***
C
CX
CC 13C
CH Write an error message which involves a given WORD between 2 messages;
CH there is a choice of subsequent action.
CA On entry, WORD is the A4 word to print
CA On entry NACT says which action is required:
CA    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
CA    NACT -ve means complain and exit
CA    NACT =0 means complain and stop
CA Absolute values for NACT on entry are:
CA          1 means simply write MESS1, WORD, MESS2
CA          2 means follow these on the next line by ICARD
CA On entry MESS1 is the message before WORD
CA On entry MESS2 is the message after WORD
CP If ABS(NACT)=2, ICARD in /SCRACH/ must contain the A80 card read
CO Writes message on units LPT and ITO.
C
      CHARACTER *34 FORM
      CHARACTER *(*) MESS1,MESS2
      CHARACTER *(*) WORD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'('' ERROR ** '',80A1,1X,A4 ,1X,80A1)'/
C
      LW=LENGT(WORD)
      IF (LW .EQ. 0) LW=1
      WRITE (FORM(24:25),2000) LW
      L1=LENGT(MESS1)
      IF (L1 .EQ. 0) L1=1
      WRITE (FORM(15:16),2000) L1
2000  FORMAT (I2)
      L2=LENGT(MESS2)
      IF (NACT .GT. 0) IERR=IERR+1
      WRITE (LPT,FORM) (MESS1(I:I),I=1,L1),WORD,(MESS2(I:I),I=1,L2)
      WRITE (ITO,FORM) (MESS1(I:I),I=1,L1),WORD,(MESS2(I:I),I=1,L2)
      IF (IABS(NACT) .EQ. 2) THEN
        WRITE (LPT,2001) ICARD
        WRITE (ITO,2001) ICARD
2001    FORMAT (' Card says:'/1X,A80)
      ENDIF
C>> JCC Handle through external function 
C>> Was
C      IF (NACT .EQ. 0) STOP
C>> Now
       IF (NACT .EQ. 0) CALL BMBOUT
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ERRCHK(NTYP,NVALUE,NBOUND,NACT,MESS)
      SUBROUTINE ERRCHK(NTYP,NVALUE,NBOUND,NACT,MESS)
C
C *** ERRCHK by JCM 4 Oct 88 ***
C
CX
CC 13C
CH (Possibly increases and) checks a value, giving if appropriate an error
CH message;  there is a choice of subsequent action.
CA On entry NTYP=type of check required:
CA    NTYP=1 simply check NVALUE for being NOT GREATER THAN NBOUND
CA    NTYP=2 increment NVALUE by 1, then as type 1
CA On entry NVALUE is the integer to be checked
CA          NBOUND is its upper bound
CA On entry NACT says which action is required if the test fails:
CA    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
CA    NACT -ve means complain and exit
CA    NACT =0 means complain and stop
CA On entry MESS is the message specific to this error state
C
CD The error message starts " ERROR ** ", and finishes with MESS.
CD If NTYP=1, NVALUE is printed.
CO Outputs the required message on units LPT and ITO
C
      CHARACTER *(*) MESS
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (NTYP .NE. 1) NVALUE=NVALUE+1
      IF (NVALUE .LE. NBOUND) GO TO 100
C
      IF (NACT .GT. 0) IERR=IERR+1
      L=LENGT(MESS)
      IF (NTYP .EQ. 1) THEN
        WRITE (LPT,3001) NVALUE,(MESS(I:I),I=1,L)
        WRITE (ITO,3001) NVALUE,(MESS(I:I),I=1,L)
3001    FORMAT (/' ',I6,80A1)
      ENDIF
      WRITE (LPT,3000) NBOUND,(MESS(I:I),I=1,L)
      WRITE (ITO,3000) NBOUND,(MESS(I:I),I=1,L)
3000  FORMAT (/' ERROR ** there is an upper limit of',I6,
     & ' on number of ',80A1)
C>> JCC Handle thru' external function
C>> Was     IF (NACT .EQ. 0) STOP
C>> Now
      IF (NACT .EQ. 0) CALL BMBOUT

 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ERRIN2(INT,NACT,MESS1,MESS2)
      SUBROUTINE ERRIN2(INT,NACT,MESS1,MESS2)
C
C *** ERRIN2 by JCM 25 Sep 89 ***
C
CX
CC 13C
CH Writes an error message which involves a given integer INT between 2
CH messages;  there is a choice of subsequent action.
CA On entry, INT is the integer to print
CA On entry NACT says which action is required:
CA    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
CA    NACT -ve means complain and exit
CA    NACT =0 means complain and stop
CA   IABS(NACT)=1 means just give message
CA   IABS(NACT)=2 means also print contents of /SCRACH/
CA On entry MESS1 is the message before INT
CA On entry MESS2 is the message after INT
CO Writes message on units LPT and ITO.
C
      CHARACTER *33 FORM
      CHARACTER *(*) MESS1,MESS2
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'('' ERROR ** '',80A1,1X,I5,1X,80A1)'/
C
      L1=LENGT(MESS1)
      IF (L1 .EQ. 0) L1=1
      WRITE (FORM(15:16),2000) L1
2000  FORMAT (I2)
      L2=LENGT(MESS2)
      IF (NACT .GT. 0) IERR=IERR+1
      WRITE (LPT,FORM) (MESS1(I:I),I=1,L1),INT,(MESS2(I:I),I=1,L2)
      WRITE (ITO,FORM) (MESS1(I:I),I=1,L1),INT,(MESS2(I:I),I=1,L2)
C
      IF (IABS(NACT) .EQ. 2) THEN
        WRITE (LPT,2001) ICARD
        WRITE (ITO,2001) ICARD
2001    FORMAT (' Card says:'/1X,A80)
      ENDIF

C>> JCC Handle thru' external function
C>> Was     IF (NACT .EQ. 0) STOP
C>> Now
      IF (NACT .EQ. 0) CALL BMBOUT
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ERRMES(NTYP,NACT,MESS)
      SUBROUTINE ERRMES(NTYP,NACT,MESS)
C
C *** ERRMES updated by JCM 10 Nov 89 ***
C
CX
CC 13C
CH Writes an error message, with choice of action on exit.
CA On entry, NTYP=type of message:
CA      NTYP=0 If IERR not 0 write "Errors in input" followed by MESS and stop
CA      NTYP=1      write "ERROR **" followed by MESS
CA      NTYP=-1     write "PROGRAM ERROR **" followed by MESS
CA      ABS(NTYP)=2 write "ERROR ** need" followed by MESS
CA      ABS(NTYP)=3 write "ERROR ** need card" followed by MESS
CA On entry NACT says which action is then required
CA    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
CA    NACT -ve means complain and exit
CA    NACT =0 means complain and stop
CA On entry MESS is the message specific to this error state
CO Writes message on units LPT and ITO.
C
      CHARACTER *(*) MESS
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      L=LENGT(MESS)
      IF (NTYP .EQ. 0) THEN
        IF (IERR .NE. 0) THEN
          WRITE (LPT,3000) IERR,(MESS(I:I),I=1,L)
          WRITE (ITO,3000) IERR,(MESS(I:I),I=1,L)
3000      FORMAT (///' *** ',I4,' fatal error(s) in input ',80A1)
C>> JCC Handle through extra function
C Was      STOP
C Now
             CALL BMBOUT
             RETURN

        ELSE
         GO TO 100
        ENDIF
      ENDIF
C
      IF (NACT .GT. 0) IERR=IERR+1
      IF (NTYP .EQ. 1) THEN
      WRITE (LPT,3001) (MESS(I:I),I=1,L)
      WRITE (ITO,3001) (MESS(I:I),I=1,L)
3001  FORMAT (/' ERROR ** ',80A1)
      ELSE IF (IABS(NTYP) .EQ. 2) THEN
      WRITE (LPT,3002) (MESS(I:I),I=1,L)
      WRITE (ITO,3002) (MESS(I:I),I=1,L)
3002  FORMAT (/' ERROR ** need ',80A1)
      ELSE IF (IABS(NTYP) .EQ. 3) THEN
      WRITE (LPT,3003) (MESS(I:I),I=1,L)
      WRITE (ITO,3003) (MESS(I:I),I=1,L)
3003  FORMAT (/' ERROR ** need card ',80A1)
      ELSE IF (NTYP .EQ. -1) THEN
      WRITE (LPT,3004) (MESS(I:I),I=1,L)
      WRITE (ITO,3004) (MESS(I:I),I=1,L)
3004  FORMAT (/' PROGRAM ERROR ** ',80A1)
      ENDIF
C

C>> JCC Handle thru' external function
C>> Was     IF (NACT .EQ. 0) STOP
C>> Now
      IF (NACT .EQ. 0) CALL BMBOUT
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ERRRE2(X,NACT,MESS1,MESS2)
      SUBROUTINE ERRRE2(X,NACT,MESS1,MESS2)
C
C *** ERRRE2 by JCM 25 Sep 89 ***
C
CX
CC 13C
CH Writes an error message which involves a given real X between 2
CH messages;  there is a choice of subsequent action.
CA On entry, X is the real number to print
CA On entry NACT says which action is required:
CA    NACT +ve means increase IERR in /CARDRC/ by 1, complain and exit
CA    NACT -ve means complain and exit
CA    NACT =0 means complain and stop
CA   IABS(NACT)=1 means just give message
CA   IABS(NACT)=2 means also print contents of /SCRACH/
CA On entry MESS1 is the message before X
CA On entry MESS2 is the message after X
CO Writes message on units LPT and ITO.
C
      CHARACTER *36 FORM
      CHARACTER *(*) MESS1,MESS2
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA FORM/'('' ERROR ** '',80A1,1X,G12.4,1X,80A1)'/
C
      L1=LENGT(MESS1)
      IF (L1 .EQ. 0) L1=1
      WRITE (FORM(15:16),2000) L1
2000  FORMAT (I2)
      L2=LENGT(MESS2)
      IF (NACT .GT. 0) IERR=IERR+1
      WRITE (LPT,FORM) (MESS1(I:I),I=1,L1),X,(MESS2(I:I),I=1,L2)
      WRITE (ITO,FORM) (MESS1(I:I),I=1,L1),X,(MESS2(I:I),I=1,L2)
C
      IF (IABS(NACT) .EQ. 2) THEN
        WRITE (LPT,2001) ICARD
        WRITE (ITO,2001) ICARD
2001    FORMAT (' Card says:'/1X,A80)
      ENDIF
C>> JCC Handle thru' external function
C>> Was     IF (NACT .EQ. 0) STOP
C>> Now
      IF (NACT .EQ. 0) CALL BMBOUT
 100  RETURN
      END
C
C

C
C
C LEVEL 2      SUBROUTINE EULSYM(ANG,SYM,ROT)
      SUBROUTINE EULSYM(ANG,SYM,ROT)
C
C *** EULSYM by PJB ***
C
CX
CC 1B
CH Finds the Euler angles correponding to a symmetry rotation.
CA On entry SYM holds a 3x3 symmetry matrix
CA    ROT 3x3x2 contains the matrix relating the axes for the Euler rotations
CA        to the axes of the symmetry matrices and its inverse.
CA On exit ANG holds the corresponding Euler angles alpha,beta and gamma
CA        in radians.
C
      DIMENSION ANG(3),SYM(3,3),ORSYM(3,3),TEMP(3,3),ROT(3,3,2)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
C TRANSFORM SYMMETRY ROTATIONS WITH MATRIX ROT
      CALL GMPRD(ROT(1,1,1),SYM,TEMP,3,3,3)
      CALL GMPRD(TEMP,ROT(1,1,2),ORSYM,3,3,3)
C
      IF (ABS(ABS(ORSYM(3,3))-1.).LT. .0001) THEN
        IF (ORSYM(3,3).LT.0.) THEN
          ANG(2)=PI
          ANG(3)=PI
        ELSE
          ANG(2)=0.
          ANG(3)=0.
        ENDIF
        ANG(1)=ATAN2(ORSYM(1,2),ORSYM(1,1))
      ELSE
        ANG(1)=ATAN2(ORSYM(3,2),ORSYM(3,1))
        ANG(3)=ATAN2(ORSYM(2,3),-ORSYM(1,3))
        IF (ABS(ORSYM(3,2)).LT.0.0001) THEN
          S=ORSYM(3,1)/COS(ANG(1))
        ELSE
          S=ORSYM(3,2)/SIN(ANG(1))
        ENDIF
        ANG(2)=ATAN2(S,ORSYM(3,3))
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 1      LOGICAL FUNCTION EXCLD(A,B,M)
      LOGICAL FUNCTION EXCLD(A,B,M)
C
C *** EXCLD by JCM 17 Jan 85 ***
C
CX
CC 11C
CH Determines whether a number occurs within any of a set of given ranges.
CA On entry A is a single element.
CA          B is an array of M/2 pairs of numbers, B1 and B2 say.
CA            Each B1 must be < or = its own B2, but the B's need not all
CA            be in ascending order.
CA On exit EXCLD is.TRUE. if A occurs within any of the ranges B1 to B2,
CA            both inclusive.
C
CN If M should be 0 it should be given as 1
C
      DIMENSION B(M)
      EXCLD=.FALSE.
      IF (M .LT. 2) GO TO 100
      DO 1 I=2,M,2
      IF (A .LT. B(I-1)) GO TO 1
      IF (A .GT. B(I))   GO TO 1
      EXCLD=.TRUE.
      GO TO 100
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE UXEXPAND(IBUF,OBUF)
      SUBROUTINE UXEXPAND(IBUF,OBUF)
C
C *** EXPAND new by PJB Mar-28-1994 ***
CC 13C
CH Expands UNIX pathnames by substituting for environment variables
CA IBUF is a character variable containing the path name to be expanded
CA on output the character variable OBUF contains the expanded pathname
CN OBUF must be given a length by the calling program which is sufficient
CN to hold the expanded path
C
      CHARACTER *(*) IBUF,OBUF
C
      L=LENGT(IBUF)
      M=LEN(OBUF)
C
      I=1
      J=1
    1 IDOLL=INDEX(IBUF(I:),'$')
      IF (IDOLL.EQ.0) THEN
        OBUF(J:)=IBUF(I:L)
        GO TO 100
      ENDIF
      IP=IDOLL-1
      IF (IP.GT.0) OBUF(J:)=IBUF(I:I+IP-1)
      J=J+IP
      IF (IBUF(IP:IP).EQ.CHAR(92)) THEN
        OBUF(J:J)='$'
        J=J+1
        I=I+IP+2
      ELSE
        IP=IP+2
        DO 2 I=IP,L
        LET=LETTER(IBUF(I:I))
        IF (LET.GT.0) GO TO 2
        INT=NDIGIT(IBUF(I:I))
        IF (INT.GT.-1) GO TO 2
        GO TO 3
    2   CONTINUE
    3   CONTINUE
CUNIX
C        CALL GETENV(IBUF(IP:I-1),OBUF(J:))
        J=LENGT(OBUF)
        J=J+1
        CALL ERRCHK(1,J,M,0,'Expanded path name too long')
      ENDIF
      IF (I.LE.L) GO TO 1
  100 RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE EXTINC(N,F)
      SUBROUTINE EXTINC(N,F)
C
C *** EXTINC by JCM 23 Jan 85 ***
C
CX
CC 2B
CH Multi-entry routine to deal with all aspects of single crystal extinction
CH corrections.
CA On entry N indicates action required:
CA     N=1 Read and interpret an E card
CA         (This is also ENTRY EXTIN1)
CA     N=2 Calculate an extinction correction, given F=mod(FC)
CA     N=3 as 2, and also calculate divided derivatives
CA     N=4 Apply shift to DOMR
CA     N=5 Apply shift to MOSC
CA         (These are also ENTRY EXTIN3(NP) where NP=1 for DOMR, 2 for MOSC)
CA     N=6 Output new E card
CA         (This is also ENTRY EXTIN4)
CA
CA     ENTRY EXTIN8(NP,NV) sets DOMR (NP=1) or MOSC (NP=2) to be variable NV
CA     ENTRY EXTIN9 sets both DOMR and MOSC fixed.
CP Entries 2 through 6 require that the extinction is set up by an entry 1.
CP Entries 2 and 3 expect in the array CEXT in /EXTN/ the 4 coefficients as
CP described in Becker & Coppens (1974) Acta Cryst A30 p129.
CP Normally entry 3 would be from an LSQ job via CALCSF
CP          entries 4 & 5 from an LSQ job via APSHSF
CP          entry 6 from an LSQ job via NWINSF
C
CD Entry 1 reads DOMR and MOSC and IEXTYP into /EXTN, setting LOGICALS
CD         GAUSS and LOREN
CD Entry 2 calculates EXTCOR, which is SQRT(Y) in the theory above, using either
CD         the Lorenztian (IEXTYP=1) or Gaussian (IEXTYP=2) model.
C
CD Entry 3 calculates in addition the derivatives:
CD         DEX/DR (R is DOMR)   DEX/DG (G is MOSC) and DEX/DF (F is mod(FC))
CD         and all these are required divided by EX itself.  They are
CD         therefore put into variables ending Q for "quotient"
CO Entry 6 writes a new E card to unit NEWIN
C
      COMMON /EXTN/IEXTYP,DOMR,KDOMR,AMOSC,KMOSC,EXTCOR,CEXT(4),
     & DEXDFQ,DEXDRQ,DEXDGQ,LOREN,GAUSS
      LOGICAL LOREN,GAUSS
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
C
      GO TO (1,2,2,4,5,6) , N
C
      ENTRY EXTIN1
C
C READ E CARD:
   1  CALL INPUTE
      GO TO 100
C
C CALCULATE EXTCOR - WITH DERIVATIVES IF N=3:
   2  IF (IEXTYP .EQ. 0) GO TO 10
      A=1.5*DOMR/CEXT(2)
      B=CEXT(1)*F*F/1.5
      C=1.5*AMOSC
      H=2.*AMOSC*AMOSC
C
      IF (LOREN) D=1./(1.+A/C)
      IF (GAUSS) D=1./(SQRT(1.+A*A/H))
C
      X=B*A*D
      X2=2.*X
      XX=X*X
      C4=1.+CEXT(4)*X
      YY=1./(1.+X2+CEXT(3)*XX/C4)
      Y=SQRT(YY)
      EXTCOR=SQRT(Y)
      IF (N .EQ. 2) GO TO 100
C
C DERIVATIVES:
      IF (LOREN) FACTOR=1./C
      IF (GAUSS) FACTOR=A*D/H
      FACTOR=FACTOR*A*D
      DNUM=2.+X2*(2.*CEXT(4)+CEXT(3)) + CEXT(4)*XX*(2.*CEXT(4)+CEXT(3))
      E = -YY*DNUM*X/(4.*C4*C4)
      DEXDRQ = E*(1.-FACTOR)/DOMR
      DEXDGQ = E*FACTOR/AMOSC
      DEXDFQ = E*2./F
      GO TO 100
C
C ENTRY 2 OR 3 - NO EXTINCTION CORRECTION:
  10  EXTCOR=1.
      DEXDRQ=0.
      DEXDGQ=0.
      DEXDFQ=0.
      GO TO 100
C
      ENTRY EXTIN3(NP)
      GO TO (4,5) , NP
C
C APPLY SHIFT TO DOMR:
   4  CALL ADJUST(DOMR)
      GO TO 100
C
C APPLY SHIFT TO MOSC:
   5  CALL ADJUST(AMOSC)
      GO TO 100
C
      ENTRY EXTIN4
C
C NEW E CARD:
   6  WRITE (NEWIN,2000) IEXTYP,DOMR,AMOSC
2000  FORMAT ('E',I5,2F10.4)
      GO TO 100
C
      ENTRY EXTIN8(NP,NV)
      IF (NP .EQ. 1) KDOMR=NV
      IF (NP .EQ. 2) KMOSC=NV
      GO TO 100
C
      ENTRY EXTIN9
      KDOMR=0
      KMOSC=0
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE F2NEW(L)
      SUBROUTINE F2NEW(L)
C
C *** F2NEW updated by JCM 6 Feb 90 ***
C
CX
CC 6B
CH Outputs a new LSQ family 2 (structure parameters) card (for A, T
CH or F cards).
CA On entry L is the position in the alphabet of the first letter of the card:
CA L = 1, 6 OR 20 for A, F OR T.
CP The card should have been read to ICARD in /SCRACH/.
CO Outputs to unit NEWIN  a new card, with altered parameters if necessary.
C
C
      CHARACTER *4 LABA,LABS,LABF
      DIMENSION A(6)
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IF (L .EQ. 1) GO TO 1
      IF (L .EQ. 6) GO TO 2
      IF (L .EQ. 20) GO TO 3
      CALL ERRIN2(L,0,'F2NEW entered with L=',' ')
C
C 'A' CARD:
   1  CALL INPUTA(0,LABA,LBALEN,LABS,LBSLEN,A,TOLD,SOLD,IER)
C REREAD CARD TO FIND OUT WHICH ATOM, AND WHETHER SCAT LABEL EXPLICIT OR NOT:
      IR=IATOM(LABA)
      LABF=' '
      IF (LBSLEN .GT. 0) LABF=LABS
C MAY BE SD CARD:
      IF (SDREAD) THEN
        WRITE (NEWIN,2020) LABA,(SDX(J,IR),J=1,3),
     &   SDTF(IR),LABF,SDSITE(IR)
2020    FORMAT ('A SD ',A4,4F10.5,1X,A4,F10.5)
      ELSE
        IF (SITE(IR) .EQ. 1.) WRITE (NEWIN,2000)
     &   LABA,(X(J,IR),J=1,3),TF(IR),LABF
        IF (SITE(IR) .NE. 1.) WRITE (NEWIN,2000)
     &   LABA,(X(J,IR),J=1,3),TF(IR),LABF,SITE(IR)
2000    FORMAT ('A ',A4,4F10.5,1X,A4,F10.5)
      ENDIF
      GO TO 100
C
C 'F' CARD:
   2  CALL INPUTF(0,LABF,LBFLEN,NTYP,IPT,IER)
C REREAD CARD TO DISCOVER WHICH FACTOR AND TYPE:
      IF (NTYP .NE. 1) GO TO 101
      IR=ISCAT(LABF)
      WRITE (NEWIN,2001) LABF,NTYP,CMULT(IR)
2001  FORMAT ('F ',A4,I5,F10.5)
      GO TO 100
C
C 'T' CARD:
   3  CALL INPUTT(0,LABA,LBALEN,NTYP,A,IER)
C REREAD CARD TO DISCOVER WHICH:
      IR=IATOM(LABA)
      IA=IAPT(IR)
      DO 9 I=1,6
   9  A(I)=ATF(I,IA)*CONATF(I,IA)
      WRITE (NEWIN,2002) LABA,IATYP(IAPT(IR)),A
2002  FORMAT ('T ',A4,I5,6F10.5)
      GO TO 100
C
C COPY OUT UNCHANGED CARD:
 101  WRITE (NEWIN,2003) (ICARD(I:I),I=1,LENGT(ICARD))
2003  FORMAT (80A1)
C
 100  RETURN
      END
C
C
C
C
C LEVEL 1      BLOCK DATA F2PARS
      BLOCK DATA F2PARS
C
C *** F2PARS updated by PJB 23-Sept-93 ***
C
      COMMON /F2NAMS/F2NAME(40)
      CHARACTER *4 F2NAME
      COMMON /F2NUMS/NF2NUM(3,40)
      DATA F2NAME/
     & 'X','Y','Z','B11','B22','B33',
     & 'B23','B13','B12','SCAT','SITE','ITF',
     & 'PSI1','PSI2','PSI3','PSI4',
     & 'THET','PHI','THE1','PHI1','MU','MU1',
     & 'TFAC','A*','B*','C*','D*','E*','F*','KX','KY','KZ',
     & 'FAM1','FAM2','XYZ','BIJ','XYZT','CELL','XYZB','XYZS'/
      DATA NF2NUM/
     & 2,0,1, 2,0,2, 2,0,3, 2,0,4, 2,0,5, 2,0,6,
     & 2,0,7, 2,0,8, 2,0,9, 2,0,10, 2,0,11, 2,0,12,
     & 2,0,13, 2,0,14, 2,0,15, 2,0,16,
     & 2,0,17, 2,0,18, 2,0,19, 2,0,20, 2,0,21, 2,0,22,
     & 1,1,1, 1,1,2, 1,1,3, 1,1,4, 1,1,5, 1,1,6, 1,1,7,
     & 1,1,11, 1,1,12, 1,1,13,
     & 1,0,0, 2,0,0, -1,0,0, -2,0,0, -3,0,0, -4,0,0, -5,0,0, -6,0,0/
      END
C
C
C
C
C LEVEL 9      SUBROUTINE F2RELA(IFAM,ISPVEC)
      SUBROUTINE F2RELA(IFAM,ISPVEC)
C
C *** F2RELA updated by JCM 8 Sep 88 ***
C
CX
CC 6B
CH Collects all structure factor type constraints implied by the symmetry.
CA IFAM gives family number;  so far this is 2 for structure parameters,
CA      but it may one day be more general
CA ISPVEC is a vector holding miscellaneous pointers saying which parameters
CA        within family IFAM, genus IR(=which atom) the following are:
CA        (1) x position coord
CA        (2) B11 first atf coefficient
CA        (3) f, the scattering factor
CA        with room for others which may be required later
CP JPHASE, JSOURC hold phase and source
C
CD Space group symmetry generated constraints are each between 2 parameters
CD only, and refer to x, y, z coordinates, or to anisotropic coefficients
CD Some of the relations found may lead to fixings rather than constraints
CD (Later - if magnetic, do the constraints on the magnetic pars here
CD also)
CD
CD We also chain together here the scattering factors of like atoms
CD and fix any non-existent atfs.  In the process, we check that the given
CD atfs have the correct symmetry to start with.
C
      DIMENSION ISPVEC(10)
      DIMENSION RMAT(3,3),NFIX3(3),FIX3(3),NFIX6(6),FIX6(6)
      DIMENSION KK1(2),AM(2),NCOUNT(6)
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C SCAN ALL ATOMS, PICKING UP RELATIONS BETWEEN POSITION PARAMETERS, ANISOTROPIC
C COEFFICIENTS, AND SCATTERING FACTORS BELONGING TO OTHER ATOMS ALSO:
      IF (NATOM .LE. 0) GO TO 100
C IF NO ATFS AT ALL, FIX ALL:
      IF (ICDNO(20) .EQ. 0) THEN
        DO 15 I=1,6
  15    CALL ADDFX5(IFAM,0,ISPVEC(2)+I-1,JPHASE,1,5)
      ENDIF
      DO 3 IR=1,NATOM
C CLEAR OUT ALL FIX/RELA INFO FOR THIS ATOM:
      DO 2 K=1,6
      NFIX6(K)=9999
      IF (K .GT. 3) GO TO 2
      NFIX3(K)=9999
   2  CONTINUE
C
C JUMP IF NOT SPECIAL:
      IF (ISGEN(1,IR) .EQ. 1) GO TO 6
C JUMP IF NOT SPECIAL BECAUSE OF A CENTRE OF SYMMETRY AT THE ORIGIN:
      IF (ISGEN(1,IR) .GT. 0) GO TO 4
C FIX ALL POSITION:
      DO 5 I=1,3
   5  CALL FIXPAR(I,NFIX3)
C ATF ON ATOM ON CENTRE:
      IF (IAPT(IR) .EQ. 0) GO TO 4
      CALL GMUNI(RMAT,3)
      CALL GMREV(RMAT,RMAT,3,3)
      CALL RELSM6(RMAT,NFIX6,FIX6)
C
C
C TAKE FIRST (OF POSSIBLE 2) SYMMETRY ELEMENTS MAKING THIS POSITION SPECIAL:
   4  DO 1 I=2,3
      K=IABS(ISGEN(I,IR))
      CALL GMEQ(SYM(1,1,K),RMAT,3,3)
      IF (ISGEN(I,IR) .LT. 0) CALL GMREV(RMAT,RMAT,3,3)
      CALL RELSM3(RMAT,NFIX3,FIX3)
C IF THERE IS AN ATF, FIND ITS RELATIONS ALSO:
      IF (IAPT(IR) .NE. 0) CALL RELSM6(RMAT,NFIX6,FIX6)
C
C IS THERE A SECOND GENERATOR OF THE SUB-GROUP WHICH MAKES THIS ATOM SPECIAL?
      IF (ISGEN(3,IR) .EQ. 0) GO TO 6
   1  CONTINUE
C ALL POSITION AND ATF RELATIONS COLLECTED IN TEMPORARY SPACE - USE:
   6  DO 16 I=1,3
  16  NCOUNT(I)=KPAK(IFAM,IR,ISPVEC(1)+I-1,JPHASE,1)
      CALL FIXREL(3,NFIX3,FIX3,NCOUNT,5)
      IF (IAPT(IR) .NE. 0) THEN
        DO 17 I=1,6
  17    NCOUNT(I)=KPAK(IFAM,IR,ISPVEC(2)+I-1,JPHASE,1)
        CALL FIXREL(6,NFIX6,FIX6,NCOUNT,5)
      ENDIF
C
C NOW LINK SCATTERING FACTORS FOR THOSE ATOMS WITH SAME FACTOR:
      IS=NFORMF(IR)
      IT=NBAKF(IS)
      IF (IT .NE. IR) THEN
        KK1(1)=KPAK(IFAM,IR,ISPVEC(3),JPHASE,1)
        KK1(2)=KPAK(IFAM,IT,ISPVEC(3),JPHASE,1)
        AM(1)=1.
        AM(2)=-1.
        CALL ADDCON(2,KK1,AM,5)
      ENDIF
C
C AND FIX ALL NON-EXISTENT ATF COEFFS, CHECKING ANY WITH FIX/RELA INFO:
      IF (ICDNO(20) .EQ. 0) GO TO 3
      IF (IAPT(IR) .EQ. 0) THEN
        DO 8 I=ISPVEC(2),ISPVEC(2)+5
   8    CALL ADDFX5(IFAM,IR,I,JPHASE,1,5)
        GO TO 3
      ENDIF
C
   7  DO 9 I=1,6
      IF (NFIX6(I) .EQ. 9999) GO TO 9
      IF (NFIX6(I) .EQ. 0) THEN
*     CHECK THAT ATF COEFF ALSO = 0 & ADJUST IF NOT
        GO TO 9
      ENDIF
      I1=I
  10  I2=NFIX6(IABS(I1))
      IF (I2 .EQ.I) GO TO 9
*     CHECK THAT COEFF I1 AND COEFF I2 HAVE CORRECT RELATIONSHIP
      I1=I2
      GO TO 10
C
   9  CONTINUE
   3  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE F2SHFT
      SUBROUTINE F2SHFT
C
C *** F2SHFT updated by JCM 10 Feb 87 ***
C
CX
CC 6B
CH Applies a shift to a particular family 2 (structure) parameter.
C
CP On entry in/NEWOLD/ IGEN=which atom and ISPC=which parameter
CP       SHIFT is the LSQ matrix inversion shift
CP       ESD is the its esd.
C
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
      GO TO (1,1,1,4,4,4,4,4,4,10,11,12) , ISPC
C
C X, Y OR Z:
   1  CALL ADJUST(X(ISPC,IGEN))
      SDX(ISPC,IGEN)=ESD
      GO TO 100
C
C B11, B22 ETC:
   4  IA=IAPT(IGEN)
      FAC=CONATF(ISPC-3,IA)
      ATF(ISPC-3,IA)=ATF(ISPC-3,IA)*FAC
      SHIFT=SHIFT*FAC
      ESD=ESD*FAC
      CALL ADJUST(ATF(ISPC-3,IA))
      ATF(ISPC-3,IA)=ATF(ISPC-3,IA)/FAC
      GO TO 100
C
C FORM/SCATTERING FACTOR:
  10  CALL ADJUST(CMULT(NFORMF(IGEN)))
      GO TO 100
C
C SITE OCCUPATION FACTOR:
  11  CALL ADJUST(SITE(IGEN))
      SDSITE(IGEN)=ESD
      GO TO 100
C
C ISOTROPIC TEMPERATURE FACTOR:
  12  CALL ADJUST(TF(IGEN))
      SDTF(IGEN)=ESD
C
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE F2VAR8(NG,NS,NV)
      SUBROUTINE F2VAR8(NG,NS,NV)
C
C *** F2VAR8 by JCM 17 Nov 90 ***
C
CX
CC 6A
CH Records varying information for a particular family 2 (structure) parameter.
C
CA On entry NG is the genus (which atom)
CA          NS is the species
CA          NV is which variable it will be
CD Records the information for future consultation
C
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
      GO TO (1,1,1,4,4,4,4,4,4,10,11,12) , NS
C
C X, Y OR Z:
   1  KX(NS,NG)=NV
      GO TO 100
C
C B11, B22 ETC:
   4  KATF(NS-3,IAPT(NG))=NV
      GO TO 100
C
C FORM/SCATTERING FACTOR:
  10  KCMULT(NG)=NV
      GO TO 100
C
C SITE OCCUPATION FACTOR:
  11  KSITE(NG)=NV
      GO TO 100
C
C ISOTROPIC TEMPERATURE FACTOR:
  12  KTF(NG)=NV
      GO TO 100
C
C
C TO CLEAR ALL FAMILY 2 VARIABLES TO BE FIXED:
      ENTRY F2VAR9
      DO 50 IR=1,NATOM
      DO 51 I=1,3
  51  KX(I,IR)=0
      DO 52 I=1,6
  52  KATF(I,IR)=0
      KCMULT(IR)=0
      KSITE(IR)=0
      KTF(IR)=0
  50  CONTINUE
C
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FACGRP(ISTAB,IFTAB,NFAC)
      SUBROUTINE FACGRP(ISTAB,IFTAB,NFAC)
C
C *** FACGRP updated by PJB 13 Feb 90 ***
C
CX
CC 1A
CH Extracts factor groups from a space group.
CD To extract the factor groups F(i) of space group G given a subgroup S
CD such that sum of F(i)xS=G
C
CA On entry ISTAB(I) is non-zero if element I is in the sub-group.
CA          ISTAB(1) is zero if the sub-group is non-centrosymmetric.
CA On exit IFTAB(I) defines the factor groups.
CA          IFTAB(I) is zero if I is in S, 1 if I is in F(i), and for all
CA          other elements IFTAB(N)= J where MULTAB(I,J)=N and I is an element
CA          of F
CA abs(NFAC) is the number of elements in the factor groups; NFAC is negative
CA          if it is centro-symmetric.
C
      LOGICAL NEW
      DIMENSION ISTAB(24),IFTAB(24),ITRY(24)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C  MAKE INITIAL IFTAB FROM ISTAB
      IFTAB(1)=1
      DO 1 I=2,NOPC
      IF (ISTAB(I).NE.0) THEN
        IFTAB(I)=-999
      ELSE
        IFTAB(I)=0
      ENDIF
    1 CONTINUE
C
      DO 2 I=2,NOPC
C POTENTIAL NEW ELEMENT OF A FACTOR GROUP:
      IF (IFTAB(I).NE.0) GO TO 2
C CONSTRUCT ITS SUBGROUP IN ITRY:
      DO 9 K=2,NOPC
   9  ITRY(K)=0
      ITRY(1)=1
      ITRY(I)=1
   7  NEW=.FALSE.
      DO 8 K=2,NOPC
      DO 8 J=2,NOPC
      IF (ITRY(J) .EQ. 0) GO TO 8
      IF (ITRY(K) .EQ. 0) GO TO 8
      JK=MULTAB(J,K)
      IF (ITRY(JK) .GT. 0) GO TO 8
      NEW=.TRUE.
      ITRY(JK)=1
   8  CONTINUE
      IF (NEW) GO TO 7
C
C NOW TRY NEW SUBGROUP AS A FACTOR GROUP:
      DO 6 J=2,NOPC
      IF (ITRY(J) .EQ. 0 .OR. IFTAB(J) .EQ. 0) GO TO 6
      GO TO 2
   6  CONTINUE
C
C NEW GROUP OK: ADD IT:
      DO 5 K=2,NOPC
      IF (ITRY(K) .EQ. 0) GO TO 5
      IFTAB(K)=1
      NFAC=NFAC+1
      DO 3 J=2,NOPC
      IF (ISTAB(J).EQ.0) GO TO 3
      IJ=MULTAB(K,J)
      IF (IFTAB(IJ).EQ.0) IFTAB(IJ)=J
    3 CONTINUE
   5  CONTINUE
    2 CONTINUE
      IF (ISTAB(1).EQ.0) NFAC=-NFAC
      DO 4 I=1,NOPC
      IF (IFTAB(I).EQ.-999) IFTAB(I)=0
    4 CONTINUE
C
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FACTGP(ISTAB,IFTAB,NFAC)
      SUBROUTINE FACTGP(ISTAB,IFTAB,NFAC)
C
C *** FACTGP by PJB 13 Feb 90 ***
C
CX
CC 1A
CH Finds the factor elements which generate a space group from one of its
CH sub-groups.
CD To extract the factors  F(i) of space group G given a subgroup S
CD such that sum of F(i)xS=G
C
CA On entry:
CA          ISTAB(I) is positive if element I is in the sub-group.
CA          ISTAB(I) is negative if S is non-centric and only the centre
CA                   related partner is in S
CA          ISTAB(1) is zero if the sub-group is non-centrosymmetric.
CA On exit:
CA          IFTAB(I) defines the factorisation.
CA          IFTAB(I) is =1 if I is in S, I if I is one of F(i)
CA                   for all other elements IFTAB(N)=I where MULTAB(I,J)=N
CA                   and J is an element of S.
CA                   Negative values of the entries indicate that it is the
CA                   centre related partner that is required.
CA        IFTAB(1) = NFAC
CA          abs(NFAC) is the number of factors; NFAC is negative
CA                    if S is centro-symmetric.
CN The factors found by FACTGP need not necessarily form a complete group
CN Note also the existence of FACGRP which extracts complete factor groups
C
C%
C      DIMENSION ISTAB(%SYMO%),IFTAB(%SYMO%)
      DIMENSION ISTAB(24),IFTAB(24)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C  MAKE INITIAL IFTAB FROM ISTAB
      IFTAB(1)=1
      DO 1 I=2,NOPC
      IF (ISTAB(I).NE.0) THEN
        IFTAB(I)=ISIGN(1,ISTAB(I))
      ELSE
        IFTAB(I)=0
      ENDIF
    1 CONTINUE
C
      NFAC=1
      DO 2 I=2,NOPC
C POTENTIAL FACTOR ELEMENT:
      IF (IFTAB(I).NE.0) GO TO 2
      IFTAB(I)=I
      NFAC=NFAC+1
      DO 9 K=2,NOPC
      IF (IABS(IFTAB(K)).NE.1) GO TO 9
      J=MULTAB(K,I)
      IFTAB(J)=I*IFTAB(K)
    9 CONTINUE
    2 CONTINUE
      IF (ISTAB(1).NE.0) NFAC=-NFAC
      IFTAB(1)=NFAC
C
  100 RETURN
      END
C
C
C
C
C LEVEL 3      COMPLEX FUNCTION FCALC(H)
      COMPLEX FUNCTION FCALC(H)
C
C *** FCALC by JCM 19 Jul 83 ***
C
CX
CC 4B
CH Calculates the COMPLEX nuclear structure factor for the reflection H.
CA On entry H is a 1x3 read vector holding h,k,l.
CA On exit FCALC holds the COMPLEX nuclear structure factor
CP PREFIN, RECIP, SYMOP, SETFOR, ATOPOS and SETANI must be called before the
CP first call to FCALC.  (All these except PREFIN are all in SETFC)
CD Forms sin theta/lambda and leaves it in STHL in /BRAGG
CD Cycles over atomic positions, then over symmetry operators, forming
CD COMPLEX FCALC by the usual formula.
CD
CD Applies scattering factor, site occupation factor, multiplicity of atom and
CD individual isotropic or anisotropic temperature factors.
C
      COMPLEX SUM1,TERM,FORMFA,FO
      DIMENSION RH(3),H(3)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
      FCALC = CMPLX(0.,0.)
C
C CALCULATE SINTHETA/LAMBDA:
      STHL = VCTMOD(0.5,H,2)
C
C INITIALISE J, WHICH SAYS WHICH FORM FACTOR USED ON "PREVIOUS" ATOM:
      J = 0
C
C SUM OVER ATOMS IN CELL:
      DO 1 N = 1,NATOM
      SUM1 = CMPLX(0.,0.)
C
C SUM OVER SYMMETRY EQUIVALENTS:
      DO 3 I = 1,NOPC
      CALL ROTSYM(H,RH,I,-1)
      F1=SCALPR(X(1,N),RH)+SCALPR(TRANS(1,I),H)
      TERM = CEXP(CMPLX(0.,TWOPI*F1))
C CALCULATE ANISOTROPIC TEMPERATURE FACTOR IF REQUIRED:
      TERM = TERM*ANITF(RH,N)
    3 SUM1 = SUM1 + TERM
C IN CASE ANOM SCATT AND CENTROSYMMETRIC:
      IF (CENTRC) SUM1=SUM1+CONJG(SUM1)
C
      SUM1 = SUM1*AMULT(N)*SITE(N)
      IF (NFORMF(N) .EQ.J) GO TO 1
C CALCULATE A NEW FORM FACTOR:
      J = NFORMF(N)
      FO = FORMFA(STHL,J)
    1 FCALC = FCALC + SUM1*FO*EXP(-TF(N)*STHL*STHL)
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE FCTOR(H,N)
      SUBROUTINE FCTOR(H,N)
C
C *** FCTOR by PJB/JCM 28 Jun 83 ***
C
CX
CC 1C
CH Finds the highest common factor of a set of indices and reduces them
CH by that factor.
CA On entry H is a 1x3 array holding 3 reals, usually h,k,l
CA On exit N is the integer highest common factor of the elements of H, assumed
CA integral, and H has been divided through by N
C
      DIMENSION H(3),AH(3)
C
C USE ONLY MODULI:
      N=0
      DO 1 I=1,3
      AH(I)=ABS(H(I))
   1  CONTINUE
C
C NMAX= LARGEST:
      NMAX=MAX1(AH(1),AH(2),AH(3))
C FOR 0,0,0 EXIT WITH N=0
      IF (NMAX .EQ. 0) GO TO 100
      FN=FLOAT(NMAX)
C
C NOW SCAN EACH POSSIBLE HCF, DOWNWARDS:
      DO 2 NN=2,NMAX
      DO 3 I=1,3
C TO 2 IF FN IS NOT A FACTOR OF ANY ONE ELEMENT:
      IF (AMOD(AH(I),FN).GT.10.E-4) GO TO 2
    3 CONTINUE
C IF HERE, HCF FOUND; REDUCE ELEMENTS BY IT:
      DO 4 I=1,3
    4 H(I)=H(I)/FN
      GO TO 5
    2 FN=FN-1.
      FN=1.
    5 N=NINT(FN)
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE FETSHF(N,SH,ES)
      SUBROUTINE FETSHF(N,SH,ES)
C
C *** FETSHF updated by JCM 21 Mar 89 ***
C
CX
CC 6C
CH Fettles a shift and esd for printing and counts, in the application
CH of shifts in LSQ.
CA  On entry N indicates which action is required:
CA  N=1 initialise
CA  N=2 add in to totals
CA  N=3 print at cycle end
CA On entry 2 SH holds the shift
CA            ES holds the esd
CP In /NEWOLD/ ISHFT = number of shifts dealt with so far in this cycle,
CP             AVSHFT=the sum of their SHIFT/ESD so far,
CP             AMAXSH=the maximum SHIFT/ESD so far.
CD Updates ISHFT, AVSHFT and AMAXSH
C
      LOGICAL TESTOV
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      GO TO (1,2,3) , N
C
C INITIALISE:
   1  AVSHFT=0.
      ISHFT=0
      AMAXSH=0.
      GO TO 100
C
C ADD IN TOTALS:
   2  SHESD=0.
      IF (.NOT. TESTOV(SH,ES)) SHESD=ABS(SH/ES)
      ISHFT=ISHFT+1
      AVSHFT=AVSHFT+SHESD
      AMAXSH=MAX(AMAXSH,SHESD)
      GO TO 100
C
C PRINT AT CYCLE END:
   3  WRITE (LPT,2002) ICYC,AVSHFT/ISHFT,AMAXSH
2002  FORMAT (//' Average SHIFT/ESD for cycle ',I3,' =',G14.5/
     & ' Maximum SHIFT/ESD = ',G14.5)
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE FETTLE(X,IFIELD,IFDIG)
      SUBROUTINE FETTLE(X,IFIELD,IFDIG)
C
C *** FETTLE by JCM 8 Jun 82 ***
C
CX
CC 13C
CH Decides field width and fractional part of real number in order to print it.
CA On entry X holds the real number.
CA On exit IFDIG is the minimum number of digits in which the
CA               fractional part of X may be printed (up to 4)
CA         IFIELD is the corresponding necessary total field width.
C
      DIMENSION ITEMP(5)
      A=ABS(X)
      CALL FRACT(A,B,N)
      CALL INTDIG(NINT(B),ITEMP,IFIELD)
      CALL INTDIG(NINT(A*10.**4),ITEMP,NDIG)
      IFDIG=0
      IF ((NDIG .EQ. 1) .AND. (ITEMP(1) .EQ. 0)) GO TO 1
      IFDIG=4
   2  IF (NDIG .EQ. 0) GO TO 1
      IF (ITEMP(NDIG) .NE. 0) GO TO 1
      IFDIG=IFDIG-1
      NDIG=NDIG-1
      GO TO 2
   1  IF (IFDIG .NE. 0) IFIELD=IFIELD+1+IFDIG
      IF (X .LT.0.) IFIELD=IFIELD+1
      RETURN
      END
C
C
C
C
C LEVEL 3      CHARACTER*10 FUNCTION FILNOM(LUN)
      CHARACTER*10 FUNCTION FILNOM(LUN)
C
C *** FILNOM by PJB Jan 86 **
C
CX
CC 13C
CH Returns the name of the file on FORTRAN unit LUN.
CA LUN on entry holds an input/output unit number
CA FILNOM is an A10 character variable which on exit holds the file name
CP NOPFIL (or OPNFIL) must have attached the unit number to the name in LUNTAB
CO If unit LUN is not in the table LUNTAB, an error message is given
C
      COMMON /FINAME/FILNAM(15)
      CHARACTER *10 FILNAM
      COMMON /LOONEY/IOTAB(15),LUNTAB(15)
C
C%
C      I=NFIND(LUN,LUNTAB,%FILE%)
      I=NFIND(LUN,LUNTAB,15)
      IF (I .EQ. 0) THEN
        CALL ERRIN2(LUN,-1,'in FILNOM - unit','not in table LUNTAB')
      ELSE
        FILNOM=FILNAM(I)
      ENDIF
  100 RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE FILPRO(DEFT,IU,LFIL)
      SUBROUTINE FILPRO(DEFT,IU,LFIL)
C
C *** FILPRO updated by PJB for UNIX 28-Mar-1994 ***
C
CX
CC 13C
CH Makes sense of general file names, under VMS AND UNIX.
C
C%
C      CHARACTER*%FNAM% DEFT
      CHARACTER*100 DEFT
C%
C      CHARACTER*%FNAM% BUFF
      CHARACTER*100 BUFF
      CHARACTER*1 SEP(4)
      LOGICAL DOT
      DIMENSION IMTAB(4),JTAB(4),JMTAB(4),IPOS(5),MARK(5)
      COMMON /FINAME/FILNAM(15)
      CHARACTER *10 FILNAM
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
CVMS
      DATA  IMTAB,JTAB,JMTAB/4,1,2,3,2,4,3,1,2,3,4,1/
C TO KEEP FUJITSU COMPILER HAPPY
C3084      DATA  IMTAB,JTAB,JMTAB/4,1,2,3,2,4,3,1,2,3,4,1/
      DATA IPOS/1,5,17,23,61/
      DATA SEP/'.',':','[',']'/
C
C BRANCH ON SYSTEM, AS SET BY INITIL:
      IF (NSYSTM .EQ. 4) GO TO 50
      IF (NSYSTM.LT.3) GO TO 60
C THIS SECTION FOR UNIX
C EXPAND NAMFIL AS INPUT IN BUF
CUNIX
       CALL UXEXPAND(NAMFIL,BUFF)
C FRIG DEFAULT DISC
       L=LENGT(DEFT(5:10))
       IF (L .GT.0 ) THEN
         IF (DEFT(5:5).EQ.'$') THEN
          M=6
       ELSE
          M=5
       ENDIF
         L=L+4
         IF (DEFT(L:L) .EQ.':') L=L-1
CUNIX
C         CALL GETENV(DEFT(M:L),NAMFIL)
         IFP=LENGT(NAMFIL)+1
         NAMFIL(IFP:IFP)='/'
         IFP=IFP+1
       ELSE
         IFP=1
       ENDIF
C DEAL WITH DEFAULTS FOR PATH
       IF(LENGT(DEFT(11:30)).GT.0) THEN
CUNIX
         CALL UXEXPAND(DEFT(11:),NAMFIL(IFP:))
         IFP=LENGT(NAMFIL)+1
       ENDIF
C ADD FILE PART
      L=LENGT(BUFF) +IFP-1
C%
C      CALL ERRCHK(1,L,%FNAM%,0,'File path too long')
      CALL ERRCHK(1,L,100,0,'File path too long')
      NAMFIL(IFP:)=BUFF
      IFP=L+1
C CHECK FOR EXTENSION
      DOT=(DEFT(1:1).NE.'.')
      DO 30 I=L,1,-1
      IF (NAMFIL(I:I).EQ.'/') GO TO 31
      IF (NAMFIL(I:I) .NE.'.') GO TO 30
      DOT=.TRUE.
   30 CONTINUE
   31 IF (.NOT.DOT) THEN
        L=L+LENGT(DEFT(1:4))
C%
C        CALL ERRCHK(1,L,%FNAM%,0,'File path too long')
        CALL ERRCHK(1,L,100,0,'File path too long')
        CALL UPONE(DEFT(1:4),NSYSTM)
        NAMFIL(IFP:)=DEFT(1:4)
      ENDIF
      LFIL=L-I
      CALL ERRCHK(1,LFIL,10,0,'Leaf name too long')
      FILNAM(IU)=NAMFIL(I+1:L)
      LFIL=L
      GO TO 100

C THIS SECTION FOR VMS
   60 CALL JGMZER(MARK,1,4)
C%
C      L=LENG(NAMFIL,%FNAM%)
      L=LENG(NAMFIL,100)
      MARK(5)=L+1
C
C  SEPARATE UP THE FILE-NAME
      JJ=0
      DO 1 I=1,L
      DO 2 J=1,4
      IF (NAMFIL(I:I).NE.SEP(JMTAB(J))) GO TO 2
C  SKIP SEPARATORS INSIDE SQUARE BRACKETS
      IF (JJ .GT. 0) THEN
        IF (JMTAB(JJ).EQ.3 .AND. JMTAB(J).NE.4) GO TO 1
      ENDIF
C  FORCE CORRECT ORDER
      IF (JJ.GE.J) GO TO 20
      JJ=J
      MARK(J)=I
      GO TO 1
    2 CONTINUE
    1 CONTINUE
C
C  PROCESS EACH PART SEPARATELY
      DO 5 IP=1,4
      GO TO (6,7,8,9), IP
C
C  EXTENSION
    6 M=MARK(IMTAB(IP))
      IF (M.NE.0) THEN
      JP=IMTAB(IP)+1
      GO TO 10
      ELSE
      IF (DEFT(1:1).NE.'.') DEFT(1:4)='.DAT'
      GO TO 5
      ENDIF
C
C  DISC
    7 JP=IMTAB(IP)
      MM=MARK(JP)
      IF (MM.EQ.0) GO TO 5
      GO TO 12
C
C  NAME
    8 JP=IMTAB(1)
      GO TO 12
C
C  DIRECTORY PATH
    9 M=MARK(IMTAB(IP-1))
      MM=MARK(IMTAB(IP))
      IF (M.EQ.0) GO TO 5
      IF (M.GE.MM) GO TO 20
      GO TO 4
C
C  WRITE THE DIFFERENT PARTS TO DEFT
C  SEARCH BACKWARDS FOR NEXT SEPARATOR
   12 DO 11 I=JP-1,1,-1
      M=MARK(I)+1
      IF (M.GT.1) GO TO 10
   11 CONTINUE
      M=1
C
C  AND THEN FORWARDS IF NECESSARY
   10 IF (IP.EQ.2) GO TO 4
      DO 3 I=JP,5
      MM=MARK(I)-1
      IF (MM.GT.0) GO TO 4
    3 CONTINUE
      MM=L
    4 DEFT(IPOS(IP):IPOS(IP+1)-1)=NAMFIL(M:MM)
C
    5 CONTINUE
C
C  CONDENSE AND WRITE BACK TO NAMFIL
      IP=1
      DO 14 JP=1,4
      I=JTAB(JP)
      L=LENG(DEFT(IPOS(I):),IPOS(I+1)-IPOS(I))
      NAMFIL(IP:)=DEFT(IPOS(I):IPOS(I+1)-1)
      IF (I.EQ.3) M=IP
      IP=IP+L
      IF (I.EQ.1) MM=IP-1
   14 CONTINUE
C
      FILNAM(IU)=NAMFIL(M:MM)
      LFIL=IP-1
      GO TO 100
C
C  ERROR
   20 CALL MESS(ITO,0,'Illegal file-name : '//NAMFIL)
CVMS


      CALL MESS(ITO,0,'Should be DISK:[DIRECTORY PATH]NAME.EXT')
      LFIL=0
      GO TO 100
C
C IBM, FOR NOW:
  50  DO 51 I=1,40
      IF (NAMFIL(I:I) .EQ. '.') GO TO 52
  51  CONTINUE
      I=0
C
  52  FILNAM(IU)=NAMFIL(I+1:I+10)
      LFIL=40
C
  100 RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE FINDCD(CH,WORD,LEN,K,LCD)
      SUBROUTINE FINDCD(CH,WORD,LEN,K,LCD)
C
C *** FINDCD updated by JCM 2 Feb 88 ***
C
CX
CC 13C
CH Searches for a card starting with letter CH and with WORD in columns 3-6.
CA On entry CH is a single character with which the card is required to start
CA          WORD is an A4 character variable required in columns 3-6
CA          LEN is the number of characters of WORD required to match (=<4)
CA          K points to the last read card:
CA                   K=0 means start at the beginning of the CH cards
CA                   K>0 implies that the Kth card was a CH card
CA On exit  LCD indicates whether such a card has been found:
CA                   LCD=-1 if no cards are found starting CH
CA                      = 0 if some start CH, but no "WORD"
CA                      >=1 if card found, and then LCD is its position in
CA                          the Crystal Data File.
CD The search starts at the K+1th card of the whole crystal data
CD If a card is found, a copy of the card in A80 FORMAT is in ICARD in /SCRACH
C
C
      CHARACTER *1 CH
      CHARACTER *4 WORD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      L=-1
      I=LETTER(CH)
C
C IF NO CH CARDS AT ALL, EXIT:
      IF (ICDNO(I) .EQ. 0) GO TO 101
      ID=K+1
      IF (K .EQ. 0) ID=IABS(INREAD(I))
C
C READ NEXT CARD:
   1  IF (ID .GT. NTOTAL(JPHASE)) GO TO 102
      CALL CARDIN(ID)
C WE HAVE TO DO SOMETHING ABOUT CDFS ENDING Y OR Z
      IF (NYZ .EQ. -1) GO TO 102
      ID=ID+NYZ
      L=ID-1
C CHECK WE STILL HAVE CH CARDS:
      IF (ICARD(1:1) .NE. CH) GO TO 102
C IGNORE INTERVENING SPACES:
      DO 88 IST=2,80
      IF (ICARD(IST:IST) .NE. ' ') GO TO 87
  88  CONTINUE
      GO TO 1
C
  87  IF (ICARD(IST:IST+LEN-1) .NE. WORD(1:LEN)) GO TO 1
C FOUND CARD:
      GO TO 101
 102  L=0
 101  LCD=L
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FIXPAR(NP,NFIX)
      SUBROUTINE FIXPAR(NP,NFIX)
C
C *** FIXPAR by JCM 13 Jul 83 ***
C
CX
CC 6A
CH Records an instruction to fix a LSQ parameter.
CA On entry NP= which parameter to fix, assuming that there is a new
CA      numbering of parameters so that NP is an address in the array
CA      NFIX.  All parameters which may be involved with NP must also
CA      have addresses within NFIX.
CA      NFIX is an integer array containing potential chaining information
CA           for the relevant parameters.
CD Records the fixing of the given parameter, and any chained to it.
C
      DIMENSION NFIX(1)
C
      I=NP
   1  IOLD=I
      I=NFIX(I)
      NFIX(IOLD)=0
C I=WHAT WAS THERE FOR CHAINING
      IF ((I .NE. 0) .AND. (I .NE. 9999)) GO TO 1
C UNCHAIN IF FIXED PAR WAS ALREADY RELATED TO ANOTHER
C OUT IF PAR ALREADY FIXED, OR PAR UNREFERENCED SO FAR
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE FIXREL(N,NFIX,FIX,KKLIST,NSTAT)
      SUBROUTINE FIXREL(N,NFIX,FIX,KKLIST,NSTAT)
C
C *** FIXREL by JCM 11 Jan 88 ***
C
CX
CC 6A
CH Takes a temporary set of fix/relate information and adds it to the
CH permanent information.
CA          NFIX, FIX contain their temporary fix/relate info out of
CA                    FIXPAR, RELPAR
CA          KKLIST is a list of KK (parameter spec) values corresponding to
CA                    the entries in NFIX
CA          NSTAT is the status to be given to any FIX or CON  info
C
      DIMENSION NFIX(N),FIX(N),KKLIST(N),KK1(2),AM(2)
C
      DO 1 I=1,N
C 9999 MEANS NOTHING KNOWN ABOUT THIS PARAMETER:
      IF (NFIX(I) .EQ. 9999) GO TO 1
C -VE MEANS "THIS PARAMETER ALREADY DEALT WITH BY THIS ROUTINE"
      IF (NFIX(I) .LT. 0) GO TO 1
C 0 MEANS "FIXED":
      IF (NFIX(I) .EQ. 0) THEN
        CALL ADDFIX(KKLIST(I),NSTAT)
        GO TO 1
      ENDIF
C PARAMETER INVOLVED IN (AT LEAST 1) CONSTRAINT:
      I1=I
   2  I2=IABS(NFIX(I1))
C IF CHAIN CLOSED, THIS IS NOT AN EXTRA CONSTRAINT:
      IF (I2 .EQ. I) GO TO 1
      KK1(1)=KKLIST(I)
      KK1(2)=KKLIST(I2)
      AM(1)=FIX(I)
      AM(2)=-FIX(I2)
      CALL ADDCON(2,KK1,AM,NSTAT)
      NFIX(I2)=-NFIX(I2)
C
      I1=I2
      GO TO 2
   1  CONTINUE
C
C RESTORE NFIX +VE:
      DO 3 I=1,N
   3  NFIX(I)=IABS(NFIX(I))
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE FIXUNI(A,NDO)
      SUBROUTINE FIXUNI(A,NDO)
C
C *** FIXUNI updated by JCM 26 Sep 84 ***
C
CX
CC 1A
CH Deals with one potential plane face of asymmetric unit, while the unit
CH is being formed.
C
CA On entry NDO indicates the required action:
CA     If NDO is -ve, removes plane number -NDO.
CA     If NDO is +ve, it is the status of plane A, which is to be added
CA            if possible.
CA     A contains the direction cosines of the normal to the offered plane.
C
CD Calls TRYUNI to test particular possible units.  Tests "NICE" on return:
CD   NICE=0  OK, we have a unit of right size with 1 typical refln in it
CD   NICE=1  Unit too big - continue
CD   NICE=-1 Unit not possible - either it is too small, or there is no
CD           typical reflection there at all.
CD
CD TRYUNI also sends back VOL=number of times too big/small unit is, or
CD VOL=0. if there are 3 planes but they form a hinge.
CD
CD Called repeatedly from SYMUNI, which decides what to offer or remove.
CD FIXUNI deals with the "NICE=-1" and the hinge conditions before
CD returning to SYMUNI.
C
      DIMENSION A(3),PTEMP(3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C%
C      COMMON /SCRAT/AXI(3,%SYMO%,2),MIRROR(%SYMO%),D(3,3),PL1(3),PL2(3),PL3(3),
      COMMON /SCRAT/AXI(3,24,2),MIRROR(24),D(3,3),PL1(3),PL2(3),PL3(3),
     & HT(3),ASY(3,4),NSTAT(4),NOPL,NICE,VOL,MOP1,MOP2
C
      IF (NDO .GT. 0) GO TO 1
      J=-NDO
      IF (NSTAT(J) .EQ. 0) GO TO 100
C REMOVE PLANE IF THERE:
      NSTAT(J)=0
      NOPL=NOPL-1
      GO TO 100
C
C ADD PLANE A IF POSSIBLE;  IF NONE THERE ALREADY, SIMPLY ACCEPT:
   1  CALL FCTOR(A,N)
      IF (NOPL .GT. 0) GO TO 2
      NOPL=1
      NSTAT(1)=NDO
      NNEW=1
      CALL GMEQ(A,ASY(1,1),1,3)
      GO TO 10
C
C SOMETHING THERE ALREADY - IF 4, TOO MANY:
   2  IF (NOPL .LT. 4) GO TO 3
      WRITE (LPT,3000) A
      WRITE (ITO,3000) A
3000  FORMAT (/' *** PROGRAM ERROR - OFFERING PLANE',3F5.0,
     & ' TO FIXUNI, WITH 4 PLANES ALREADY')
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
C CHECK WE DO NOT ALREADY HAVE A OR -A:
   3  DO 4 I=1,4
      IF (NSTAT(I) .EQ. 0) GO TO 4
      CALL VECPRD(A,ASY(1,I),PTEMP)
      IF (VCTMOD(1.0,PTEMP,1) .LT. 0.0001) GO TO 100
   4  CONTINUE
C
C A IS NEW;  ADD IT, EVEN IF FOR NOW TO POSITION 4:
      DO 7 NNEW=1,4
      IF (NSTAT(NNEW) .EQ. 0) GO TO 8
   7  CONTINUE
C SHOULD NOT BE HERE - ALL 4 SLOTS IN ASY (AND HENCE NSTAT) SHOULD NOT BE FULL:
      WRITE (LPT,3001) NOPL
      WRITE (ITO,3001) NOPL
3001  FORMAT (/' *** PROGRAM ERROR IN FIXUNI - ASY FULL BUT NOPL=',I3)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
    8  NOPL=NOPL+1
      NSTAT(NNEW)=NDO
      CALL GMEQ(A,ASY(1,NNEW),1,3)
C
C NOW TEST NEW CONFIGURATION - IF 4 PLANES JOIN PART WHICH TAKES ONLY 3:
  10  IF (NOPL .GE. 4) GO TO 11
      CALL TRYUNI(0)
      IF (NICE) 12,100,13
C
C PLANE NOT TO BE USED - REMOVE:
  12  NOPL=NOPL-1
      NSTAT(NNEW)=0
      GO TO 100
C
C PLANE OK BUT VOL TOO BIG (OR A HINGE):
  13  IF (NOPL .NE. 3) GO TO 100
C IF ONLY 1 OR 2 PLANES, NOTHING MORE WE CAN DO; (4 PLANES NOT HERE)
      IF (VOL .NE. 0.) GO TO 100
C HERE IF 3 PLANES ALL JOINING AT HINGE:
  11  NOPL=NOPL-1
C WE OFFER TO TRYUNI SUBSETS OF WHAT WE HAVE IN ASY, OMITTING ONE PLANE AT ONCE
      OLDVOL=100.
      DO 14 I=1,4
C NOT IF PLANE NOT THERE:
      IF (NSTAT(I) .EQ. 0) GO TO 14
C DO NOT OMIT A MANDATORY PLANE:
      IF (NSTAT(I) .EQ. 2) GO TO 14
C OR THE NEWLY OFFERED ONE:
      IF (I .EQ. NNEW) GO TO 14
      NSTATS=NSTAT(I)
      NSTAT(I)=0
      CALL TRYUNI(0)
      IF (NICE) 5,100,15
C
C IF NICE EVER BECOMES -1, OMIT NEW PLANE:
   5  NSTAT(I)=NSTATS
      NSTAT(NNEW)=0
      GO TO 100
C
C VOL IS STILL TOO LARGE:
  15  IF (VOL .GT. OLDVOL) GO TO 6
C KEEP SMALLEST FOUND VOL
      OLDVOL=VOL
C PUT PLANE BACK:
      N1=I
   6  NSTAT(I)=NSTATS
  14  CONTINUE
C
C STILL TOO BIG, BUT SHOULD HAVE SMALLEST VOL OF HINGE IN OLDVOL, AND THE PLANE
C WE OMITTED TO GET IT IN N1:
      NSTAT(N1)=0
C TIDY UP AGAIN:
      CALL TRYUNI(0)
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE FIXVAR(FX,IFAM,IGEN,ISPC,KP,KS,NSTAT)
      SUBROUTINE FIXVAR(FX,IFAM,IGEN,ISPC,KP,KS,NSTAT)
C
C *** FIXVAR by JCM 9 Nov 90 ***
C
CX
CC 6C
CH Adds a request to fix (or vary) a parameter to the lists held in setting
CH up LSQ environments.
CA On entry:
CA For entries FIXVAR, ADDFX5, ADDVR5:
CA    IFAM, IGEN, ISPC are the family, genus & species of the parameter
CA    KP, KS are the phase & source if relevant, or may be zero
CA For entry FVKPAK, ADDFIX, ADDVAR
CA          KK is the packed parameter spec, possibly incomplete
CA          NSTAT is the status of the request (5=not changeable)
CA                                             (4=changeable)
CP LSETUP must have initialised the list
CD Ensures an entry in the fix/vary list for the given parameter.
CD If there already was one for exactly this parameter, alters it as
CD requested, checking that the request is reasonable.
CD Records whether the request was fix or vary in KSTFV, as + or -
CD Records status of request also in KSTFV.
CD Records time of request in KTIME, so that conflict can be resolved.
CD Keeps count of total number of such requests in NUMFV
CD Sets KTYPFV to 0 if the packed KK was complete (ie specific), or an
CD address in the table KUNPFV into which the incomplete KK has been
CD unpacked for future reference.
CN Also entries ADDFIX, ADDVAR, FVKPAK
C
      LOGICAL FX,FIX,KWHOLE
      CHARACTER *4 FV(2),NAM1,NAM2
      DIMENSION K(5)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LINKAG/NUMFV,NUMPAK,KKFV(200),KTYPFV(200),KSTFV(200),
     & KTIME(200),KUNPFV(5,30),NTIME,NUMCON,KKCON(500),AMCON(500),
     & KPTCON(201),KSTCON(200),KTPCON(200)
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      DATA FV/'fix','vary'/
C
C EITHER FIX OR VARY, GIVEN FAMILY, GENUS & SPECIES WITH PHASE & SOURCE
      KK=KPAK(IFAM,IGEN,ISPC,KP,KS)
      FIX=FX
      GO TO 1
C
C FIXING ENTRY WITH UNPACKED 5:
      ENTRY ADDFX5(IFAM,IGEN,ISPC,KP,KS,NSTAT)
      KK=KPAK(IFAM,IGEN,ISPC,KP,KS)
      FIX=.TRUE.
      GO TO 1
C
C VARYING ENTRY WITH UNPACKED 5:
      ENTRY ADDVR5(IFAM,IGEN,ISPC,KP,KS,NSTAT)
      KK=KPAK(IFAM,IGEN,ISPC,KP,KS)
      FIX=.FALSE.
      GO TO 1
C
C FIXING ENTRY WITH PACKED PARAMETER SPEC:
      ENTRY ADDFIX(KKK,NSTAT)
      FIX=.TRUE.
      GO TO 10
C
C VARYING ENTRY WITH PACKED PARAMETER SPEC:
      ENTRY ADDVAR(KKK,NSTAT)
      FIX=.FALSE.
      GO TO 10
C
C EITHER FIX OR VARY ENTRY, WITH PACKED KK:
      ENTRY FVKPAK(KKK,NSTAT,FX)
      FIX=FX
  10  KK=KKK
C IS IT THERE ALREADY?
   1  IF (NUMFV .EQ. 0) GO TO 2
      N=NFIND(KK,KKFV,NUMFV)
      IF (N .EQ. 0) GO TO 2
C IF THERE, RECORD OLD AND NEW FIX OR VARY INDICATORS:
      I1=1
      IF (.NOT. FIX) I1=2
      I2=1
      IF (KSTFV(N) .LT. 0) I2=2
C CANNOT LOWER STATUS:
      IF (NSTAT .GE. IABS(KSTFV(N))) GO TO 3
C
C THIS IS A SIMPLIFICATION COVERING EXISTING CASES:
      CALL PARNAM(NAM1,NAM2,3,KK)
      CALL MESS(LPT,0,'Ignored request to '//FV(I1)//' parameter '//
     & NAM1//NAM2//' - fixed by symmetry')
      GO TO 100
C
C A NEW REQUEST - ACCEPT IT:
C%
C   2  CALL ERRCHK(2,NUMFV,%FXVA%,0,'fix or vary requests')
   2  CALL ERRCHK(2,NUMFV,200,0,'fix or vary requests')
      N=NUMFV
      KKFV(N)=KK
C RECORD WHETHER OR NOT KK COMPLETE:
      KTYPFV(N)=0
      IF (.NOT. KWHOLE(KK,K)) THEN
C%
C        CALL ERRCHK(2,NUMPAK,%FVPK%,0,'incomplete fix/vary requests')
        CALL ERRCHK(2,NUMPAK,30,0,'incomplete fix/vary requests')
        KTYPFV(N)=NUMPAK
        CALL GMEQ(K,KUNPFV(1,NUMPAK),1,5)
      ENDIF
C
C JOIN HERE TO UPDATE AN EXISTING ENTRY:
   3  KSTFV(N)=NSTAT
      IF (.NOT. FIX) KSTFV(N)=-NSTAT
      KTIME(N)=NTICK(NTIME)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FLIP(I,J)
      SUBROUTINE FLIP(I,J)
C
C *** FLIP by JCM 4 Feb 88 ***
C
CX
CC 16C
CH Exchanges the integers I and J.
CA On entry I and J holds certain values
CA On exit their values have been exchanged.
C
      K=I
      I=J
      J=K
      RETURN
      END
C
C
C
C
C LEVEL 2      COMPLEX FUNCTION FORMFA(AK,II)
      COMPLEX FUNCTION FORMFA(AK,II)
C
C *** FORMFA updated by JCM 25 Jan 91 ***
C
CX
CC 4B
CH Calculates form or scattering factors.
CA On entry AK=sin theta/lambda if relevant
CA          II=which factor is required
CA On exit  FORMFA holds the factor.
CP SETFOR must have read and interpreted F cards.
C
CD Allows 5 types of scattering factor, depending on MODE(I):
CD    MODE(I)=1 neutron nuclear factor, the COMPLEX value (CMULT(I),0.)
CD            2 exponential series of NT(I) terms held in F(,I)
CD            3 interpolation in table of NT(I) entries held in S(,I) and F(,I)
CD            4 as 2, but also times sin theta/lambda squared.
CD            5 Form factor to be calculated from radial wave-functions
CD              given on W cards and read by RADFUN (allowed by SETFOR
CD              but not included in FORMFA;  see FORFAC)
CD    Types 2,3 and 4 may also be multiplied by a constant in CMULT(I)
CD
CD If NAMODE(I)=1, an anomalous scattering factor is added from FDASH(,I)
CO For type 3, if AK is given outside the range in S(,I), an error message is
CO given and the routine stops.
C
      COMMON /ANSCAT/NAMODE(20),FDASH(20),KOM2
      COMPLEX FDASH
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      I=II
      M = MODE(I)
      GO TO (1,2,3,2,100), M
C
C     NEUTRON NUCLEAR SCATTERING:
   1  G = 1.0
      GO TO 20
C
C     FORM FACTORS AS EXPONENTIAL SERIES:
    2 N = NT(I)
      G = F(N,I)
      N = N-2
      DO 5 L = 1,N,2
      ARG = F(L+1,I)*AK*AK
      IF (ARG .LE. 174) G = G + F(L,I)*EXP(-ARG)
    5 CONTINUE
      IF (M .EQ. 4) G=G*AK*AK
      GO TO 20
C
C     FORM FACTORS GIVEN IN A TABLE:
    3 N = NT(I)
      IF ((AK .LE. S(N,I)) .AND. (AK .GE. S(1,I))) GO TO 14
      WRITE (LPT,3000) AK,I
      WRITE (ITO,3000) AK,I
3000  FORMAT(/' ERROR **  sin theta/lamda =',F6.3,
     & ' outside range of table for form factor number',I3)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
  14  IF (N .LT. 5) THEN
        NN = N
        MM = 1
        GO TO 11
      ENDIF
      D1 = ABS(AK-S(1,I))
      DO 6 J = 2,N
      D = ABS(AK-S(J,I))
      IF (D .GT. D1) GO TO 13
      D1 = D
    6 CONTINUE
C
      M = N
      GO TO 8
C
   13 M = J-1
      IF (M.GT.2) GO TO 8
      MM = 1
      GO TO 12
    8 IF (M+2.GT.N) THEN
        MM = N-4
      ELSE
        MM = M-2
      ENDIF
   12 NN = 5
   11 CALL TB02A (S(MM,I),F(MM,I),AK,G,NN)
C
  20  FORMFA=CMPLX(G*CMULT(I),0.)
C ANOMALOUS SCATTERING IF GIVEN:
 100  IF (NAMODE(I) .EQ. 1) FORMFA=FORMFA+FDASH(I)
      RETURN
      END
C
C
C
C
C LEVEL 5      FUNCTION FORMFC(AK,L,MODE,JAT)
      FUNCTION FORMFC(AK,L,MODE,JAT)
C
C *** FORMFC updated by PJB/JBF 3 Sep 89 ***
C
CX
CC 4B
CH Calculates form factor integrals from radial wave functions.
CA On entry AK=4*pi* sin theta/lamda.
CA          MODE=0 requests FORMFC to be <Jl>
CA          MODE=1 requests <Gl>, the orbital integral
CA          MODE=2 requests the wavefunction factor.
CA On exit FORMFC contains the COMPLEX factor
CP RADFUN should have read the radial wave function
C
      COMMON /RADINT/N(20,5),FF(2,20,5),NTERMS(5),
     & IRADF(5),NRADF
C
      IAT=NFIND(JAT,IRADF,NRADF)
      FORMFC = 0.
      IMAX = NTERMS(IAT)
      DO 1 I = 1,IMAX
      IF (MODE .EQ. 2) THEN
        FORMFC = FORMFC + FF(1,I,IAT)*EXPINT(AK,FF(2,I,IAT),N(I,IAT),L)
        GO TO 1
      ENDIF
C
      DO 2 J = 1,IMAX
      MAX = N(I,IAT)+N(J,IAT)
      NUM = MAX+1
      P = FF(2,I,IAT) + FF(2,J,IAT)
      IF (MODE .EQ.0) GO TO 3
      FAC = 1/P
      G = 0
      DO 5 M = 1,NUM
      G = G + FAC*EXPINT(AK,P,MAX-M,L)
    5 FAC = FAC*FLOAT(MAX-M+1)/P
      FORMFC = FORMFC + FF(1,I,IAT)*FF(1,J,IAT)*G*2.
      IF (MODE .EQ.1) GO TO 2
    3 FORMFC = FORMFC + FF(1,I,IAT)*FF(1,J,IAT)*EXPINT(AK,P,MAX,L)
    2 CONTINUE
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE FUDGET(IPT,ITYP,F1,F2)
      SUBROUTINE FUDGET(IPT,ITYP,F1,F2)
C
C *** FUDGET by JCM 10 Feb 87 ***
C
CX
CC 6C
CH Reads a fudge factor from a card having already read a parameter
CH specification.
C
CA IPT on entry points at next char on card ICARD
CA IPT on exit has been advanced by the amount read
CA ITYP on exit = type of factor read
CA F1 on exit = first no. read if appropriate
CA F2 on exit = second no. read if appropriate
CP Card is held in /SCRACH/ in ICARD
C
CD Reads one of:
CD    a number into F1 (setting ITYP=1)
CD    "GE" number into F1 (setting ITYP=2)
CD    "LE" number into F2 (setting ITYP=3)
CD    both the above, setting ITYP=4
C
      CHARACTER *10 WORD
      ITYP=1
      IPKEEP=IPT
      CALL RDREAL(F1,IPT,IPT,80,IER)
C IF NUMBER READ, TYPE 1 SIMPLE MULTIPLICATIVE FACTOR:
      IF (IER .EQ. 0) GO TO 100
C
C WORD READ - EXPECT GE OR LE
      IPT=IPKEEP
  42  IPKEEP=IPT
      CALL RDWORD(WORD,LEN,IPT,IPT,80,0,IER)
      IF (WORD .NE. 'GE') GO TO 41
      IF (ITYP .GT. 1) ITYP=4
      IF (ITYP .EQ. 1) ITYP=2
      CALL RDREAL(F1,IPT,IPT,80,IER)
      GO TO 42
  41  IF (WORD .NE. 'LE') GO TO 43
      IF (ITYP .GT. 1) ITYP=4
      IF (ITYP .EQ. 1) ITYP=3
      CALL RDREAL(F2,IPT,IPT,80,IER)
      GO TO 42
C
C NEITHER GE NOR LE - MAY BE NEXT PARAMETER SPEC:
  43  IPT=IPKEEP
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE FUDGIN
      SUBROUTINE FUDGIN
C
C *** FUDGIN updated by JCM 27 Apr 92 ***
C
CX
CC 6A
CH Interprets all L FUDG cards.
CD Sets NFUDGE in /FUDG to be the number of fudge factors read.
CD Reads from the cards sets of <parameter specification> <fudge factor>
CD The parameter specification may be any of those described under PARRD
CD The fudge factor may be one of:
CD    1)  A simple multiplicative factor
CD    2)  GE followed by a lower limit
CD    3)  LE followed by an upper limit
CD    4)  both 2) and 3) in either order
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FUDG/NFUDGE,IFDGPT(20),FUDGE1(20),FUDGE2(20),
     & IFDTYP(20)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
      IN=0
C NEXT "L FUDG" CARD:
  31  CALL FINDCD('L','FUDG',4,IN,L)
      IF (L .LE. 0) GO TO 100
      CALL MESS(LPT,0,'Fudge factor:')
      CALL MESS(LPT,0,ICARD(7:80))
      IN=L
      IPT=7
C READ NEXT PARAMETER SPEC ON CARD:
   3  CALL PARRD(IPT,IPT,K,IFAM,IGEN,ISPC)
      IF (K) 1,31,2
C
C K +VE - A PACKED PARAMETER SPEC:
   2  IER=IERR
C%
C      CALL ERRCHK(2,NFUDGE,%FUDG%,0,'fudge factors')
      CALL ERRCHK(2,NFUDGE,20,0,'fudge factors')
      IF (IER .NE. IERR) GO TO 100
C
C READ FUDGE FACTOR:
   6  IFDGPT(NFUDGE)=K
      CALL FUDGET(IPT,IFDTYP(NFUDGE),FUDGE1(NFUDGE),FUDGE2(NFUDGE))
      GO TO 3
C
C K -VE - A WORD LIKE ALL(-100), ONLY (-99)
   1  I=-K-98
      GO TO (11,12) , I
C
C 'ONLY' SHOULD BE A MISTAKE HERE:
  11  CALL ERRMES(1,1,'"ONLY" not allowed on L FUDG card')
      GO TO 3
C
C 'ALL' SHOULD BE ACCOMPANIED BY EITHER +VE IFAM, MEANING THAT A (POSSIBLY
C  PARTIAL) PARAMETER SPEC IS GIVEN, OR -VE IFAM, MEANING THAT A COMPOSITE
C WORD FOLLOWED THE 'ALL':
C IFAM=-1 XYZ, -2 BIJ, -3 XYZT, -4 CELL, -5 XYZB, -6 XYZS
C BY CONVENTION - THIS MUST HAVE BEEN BUILT IN TO THE MAIN PROGRAM
  12  IF (IFAM .LE. 0) GO TO 5
      K=KPAK(IFAM,IGEN,ISPC,KPHASE,KSOURC)
      GO TO 2
C
   5   GO TO (21,22,21,24,25,21) , -IFAM
C
C 'XYZB' - SET SPECIES 1-9 (ALL GENERA, FAMILY 2)
  25  L1=1
      GO TO 19
C
C 'BIJ' - SET SPECIES 4-9 (ALL GENERA, FAMILY 2)
  22  L1=4
  19  L2=9
      N1=2
      N2=0
      GO TO 20
C
C 'CELL' - SET SPECIES 2-7, FAMILY 1, GENUS 1:
  24  L1=2
      L2=7
      N1=1
      N2=1
      GO TO 20
C
C 'XYZ' - SET SPECIES 1-3 (ALL GENERA, FAMILY 2) - ALSO XYZT WITH ADDED 12
C AND XYZS WITH ADDED 11
  21  L1=1
      L2=3
      N1=2
      N2=0
  20  CALL FUDGET(IPT,ITYP,F1,F2)
      DO 4 I=L1,L2
      IER=IERR
C%
C      CALL ERRCHK(2,NFUDGE,%FUDG%,1,'fudge factors')
      CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
      IF (IER .NE. IERR) GO TO 100
      IFDTYP(NFUDGE)=ITYP
      FUDGE1(NFUDGE)=F1
      FUDGE2(NFUDGE)=F2
      IFDGPT(NFUDGE)=KPAK(N1,N2,I,KPHASE,1)
   4  CONTINUE
      IF (IFAM .EQ. -3) THEN
        IER=IERR
C%
C        CALL ERRCHK(2,NFUDGE,%FUDG%,1,'fudge factors')
        CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
        IF (IER .NE. IERR) GO TO 100
        IFDTYP(NFUDGE)=ITYP
        FUDGE1(NFUDGE)=F1
        FUDGE2(NFUDGE)=F2
        IFDGPT(NFUDGE)=KPAK(2,0,12,KPHASE,1)
      ELSE IF (IFAM .EQ. -6) THEN
        IER=IERR
C%
C        CALL ERRCHK(2,NFUDGE,%FUDG%,1,'fudge factors')
        CALL ERRCHK(2,NFUDGE,20,1,'fudge factors')
        IF (IER .NE. IERR) GO TO 100
        IFDTYP(NFUDGE)=ITYP
        FUDGE1(NFUDGE)=F1
        FUDGE2(NFUDGE)=F2
        IFDGPT(NFUDGE)=KPAK(2,0,11,KPHASE,1)
      ENDIF
      GO TO 3
C
 100  RETURN
      END
C
C

C
C
C LEVEL 3      SUBROUTINE GAUSPT(NN,GPT,GWT)
      SUBROUTINE GAUSPT(NN,GPT,GWT)
C
C *** GAUSPT by JCM 26 Sep 85 ***
C
CX
CC 2A
CH Sets up Gauss points and weights for use in 3D integration.
CA On entry NN=number of Gauss points & weights required.
CA On exit the array GPT contains the necessary Gaus points
CA        and the array GWT contains the corresponding weights.
C
CD The array GTABLE contains n-1 numbers for each n allowed.  If n is even,
CD these are X1, W1, X2, W2, . . Xm (where 2m=n), and if n is odd they are
CD X1, W1, X2, W2, . . Xm, Wm (where 2m=n-1).
CD
CD This subroutine generates the two full arrays GPT(1:n) from the Xi and
CD GWT(1:n) from the Wi.  For all except the 'centre' points the algorithm
CD is simple:
CD
CD i<m     Xi, Wi as stored;
CD i>m+1:  Xi=1-Xj and Wi=Wj, where i+j=n+1
CD
CD For the 'centre' points:
CD For n odd:  i=m    Xi, Wi as stored;
CD             i=m+1  Xi=0.5, Wi=1-sum of previous weights*2
CD     n even: i=m    Xi as stored, Wi=0.5-sum of previous weights.
CD
CN The allowed values of n are 1-11 inclusive, 14, 17, 20, 24, 32, 40
C
      LOGICAL EVEN
      DIMENSION GPT(NN),GWT(NN)
      DIMENSION GTABLE(196),IST(17),NGPT(6)
C
C N=2:
      DATA GTABLE(1)/.2886751346/
C N=3:
      DATA GTABLE(2),GTABLE(3)/.1127016654,  .2777777778/
C N=4:
      DATA GTABLE(4),GTABLE(5)/.0694318441,  .1739274225/
      DATA GTABLE(6)/.3300094782/
C N=5:
      DATA GTABLE(7),GTABLE(8)/.0469100770,  .1184634425/
      DATA GTABLE(9),GTABLE(10)/.2307653450,  .2393143353/
C N=6:
      DATA GTABLE(11),GTABLE(12)/.0337652429,  .0856622462/
      DATA GTABLE(13),GTABLE(14)/.1669395307,  .1803807865/
      DATA GTABLE(15)/.3806904069/
C N=7:
      DATA GTABLE(16),GTABLE(17)/.0254460439,  .0647424831/
      DATA GTABLE(18),GTABLE(19)/.1292344072,  .1398526957/
      DATA GTABLE(20),GTABLE(21)/.2970774243,  .1909150252/
C N=8:
      DATA GTABLE(22),GTABLE(23)/.0198550718,  .0506142681/
      DATA GTABLE(24),GTABLE(25)/.1016667613,  .1111905172/
      DATA GTABLE(26),GTABLE(27)/.2372337950,  .1568533229/
      DATA GTABLE(28)/.4082826788/
C N=9:
      DATA GTABLE(29),GTABLE(30)/.0159198803,  .0406371941/
      DATA GTABLE(31),GTABLE(32)/.0819844463,  .0903240804/
      DATA GTABLE(33),GTABLE(34)/.1933142837,  .1303053482/
      DATA GTABLE(35),GTABLE(36)/.3378732883,  .1561735385/
C N=10:
      DATA GTABLE(37),GTABLE(38)/.0130467358,  .0333356721/
      DATA GTABLE(39),GTABLE(40)/.0674683166,  .0747256746/
      DATA GTABLE(41),GTABLE(42)/.1602952159,  .1095431812/
      DATA GTABLE(43),GTABLE(44)/.2833023030,  .1346333597/
      DATA GTABLE(45)/.4255628305/
C N=11:
C TO BE ENTERED TO (46)-(55)
C N=14:
C TO BE ENTERED TO (56)-(68)
C N=17:
C TO BE ENTERED TO (69)-(84)
C N=20:
C TO BE ENTERED TO (85)-(103)
C N=24:
C TO BE ENTERED TO (104)-(126)
C N=32:
C TO BE ENTERED TO (127)-(157)
C N=40:
C TO BE ENTERED TO (158)-(196)
C
      DATA IST/0,1,2,4,7,11,16,22,29,37,46,56,69,85,104,127,158/
      DATA NGPT/14,17,20,24,32,40/
C
      N=NN
C CHECK N IS IN ALLOWED VOCABULARY:
      L=N-10
      IF (N .LE. 11) GO TO 5
      L=NFIND(N,NGPT,6)
      IF (L .EQ. 0) CALL ERRIN2(N,0,' ',
     & ' points not allowed in Gauss table')
C
C IPT POINTS TO FIRST GTABLE ENTRY FOR THIS N:
   5  IPT=IST(L+10)
      IF (N .GT. 1) GO TO 9
      GPT(1)=0.5
      GWT(1)=1.0
      GO TO 100
C
C SET EVEN TO BE TRUE IF N IS EVEN. M=N/2 IF N EVEN, (N-1)/2 IF N ODD.
   9  CALL PARITY(N,M,EVEN)
      DO 1 I=1,N
      IF (I .GE. M) GO TO 2
C
C FIRST POINTS AND WEIGHTS ARE STORED READY TO USE:
   4  GWT(I)=GTABLE(IPT+2*I-1)
      GPT(I)=GTABLE(IPT+2*I-2)
      GO TO 1
C
C I HAS REACHED AT LEAST M:
   2  IF (I .GT. M) GO TO 3
C IF N ODD, ARE AT (N-1)/2 WHICH IS AS ABOVE:
      IF (.NOT. EVEN) GO TO 4
C
C IF N EVEN, CENTRE TWO POINTS ARE AS ABOVE, BUT WEIGHTS ARE CALCULATED FROM
C THE FACT THAT THEIR SUM IS 1:
      GPT(I)=GTABLE(IPT+2*I-2)
C CHECK ADDRESSING **
      CALL SUMVEC(GTABLE(IPT),2,N-2,2,WSUM)
      GWT(I)=0.5 - WSUM
      GO TO 1
C
C I IS PAST M:
   3  IF (I .GT. M+1) GO TO 6
      IF (EVEN) GO TO 6
C
C MIDPOINT FOR N ODD:
C CHECK ADDRESSING **
      CALL SUMVEC(GTABLE(IPT),2,N-1,2,WSUM)
      GWT(I)=1.-2.*WSUM
      GPT(I)=0.5
      GO TO 1
C
C PAST MIDDLE:
   6  J=N+1-I
      GWT(I)=GWT(J)
      GPT(I)=1.-GPT(J)
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GENELM(NSUB,ISGEN)
      SUBROUTINE GENELM(NSUB,ISGEN)
C
C *** GENELM corrected by JCM 18 Sep 89 ***
C
CX
CC 1B
CH Finds the generators of a subgroup of a space group.
CA On entry NSUB is an integer array holding the orders of the elements of the
CA               subgroup, with zeros for those elements not in the subgroup.
CA          ISGEN(1) is the multiplicity of the subgroup
CA On exit  ISGEN(2) (and (3) if necessary) contain the labels of the
CA               generators.
C
      LOGICAL FIRST
C%
C      DIMENSION NSUB(%SYMO%),ISGEN(3),ISIG(2)
      DIMENSION NSUB(24),ISGEN(3),ISIG(2)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C  JUMP OUT FOR P1 OR P-1:
      ISGEN(2)=1
      ISGEN(3)=0
      IF (IABS(ISGEN(1)).EQ. 1) GO TO 100
C PUT ZEROS INTO NSUB FOR MULTIPLE OPERATIONS OF A SINGLE OPERATOR:
      DO 1 NO=2,NOPC
      M=NSUB(NO)
      IF (M.EQ.0) GO TO 1
      N=NO
      DO 2 J=3,IABS(M)
      N=MULTAB(NO,N)
    2 NSUB(N)=0
    1 CONTINUE
C
      NGEN=1
C
C COUNT DOWN THROUGH ORDERS OF POSSIBLE SYMMETRY ELEMENTS:
      DO 4 NN=1,5
      IORD=7-NN
      IF (IORD.EQ.5) GO TO 4
C  OPERATORS OF ORDER 5 DONT OCCUR
      FIRST=.TRUE.
      DO 5 NNN=1,NOPC
      IF (IABS(NSUB(NNN)).NE.IORD) GO TO 5
C  OPERATOR OF ORDER IORD FOUND - MARK IT USED, KEEPING ITS SIGN:
      ISIG(NGEN)=ISIGN(1,NSUB(NNN))
      NSUB(NNN)=0
C
C IF NOT FIRST OF THIS ORDER, WE NEED A SECOND GENERATOR TO PRODUCE IT - JUMP:
      IF (.NOT.FIRST) GO TO 6
C THE FIRST OPERATOR OF THIS ORDER:
C JUMP IF ALREADY HAVE A GENERATOR OF HIGHER ORDER:
      IF (NGEN.EQ.2) GO TO 10
      FIRST=.FALSE.
      ISGEN(2)=NNN*ISIG(1)
      ISG=NNN
      NGEN=2
      GO TO 5
C  HERE IF NOT FIRST OF THIS ORDER:
    6 DO 8 M=1,NOP
C FIND IN M THE OPERATOR WHICH MAKES CURRENT ONE FROM FIRST GENERATOR:
      IF (MULTAB(ISG,M).EQ.NNN) GO TO 3
    8 CONTINUE
C I THINK IT IS AN ERROR IF WE GET HERE?
      GO TO 100
    5 CONTINUE
    4 CONTINUE
C
C  NO SECOND GENERATOR - RETURN
      GO TO 100
C
C  SET SECOND GENERATOR:
   10 ISGEN(3)=NNN*ISIG(2)
      GO TO 100
C
    3 ISGEN(3)=M*ISIG(1)*ISIG(2)
C
  100 RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE GENMUL(H,NOMORE,M)
      SUBROUTINE GENMUL(H,NOMORE,M)
C
C *** GENMUL by JCM 18 Jun 85 ***
C
CX
CC 3B
CH Gives next useful set of h,k,l and multiplicity, scanning the asymmetric
CH unit of reciprocal space.
C
CA On exit  H is a 1x3 real vector holding next generated values of h,k,l
CA          NOMORE is TRUE if there are no more to be found
CA          M is the multiplicity of the found reflection.
CP SYMOP should have read the space group symmetry.
CP SYMUNI should have found the asymmetric unit of reciprocal space.
CP SETGEN should have set up stepping over asymmetric unit in /HKLGEN
C
CD Uses "previous" h,k,l in PT in /HKLGEN to move to a new one.
CD Rejects lattice absences (but not space group absences - do those outside
CD                 using ISPABS if required),
CD         h,k,l outside asymmetric unit,
CD         h,k,l for which sin theta/lambda is greater than STHLMX,
CD         and h,k,l for which sin theta is around zero (to reject 0,0,0)
CD
CD Allows for non-primitive stepping vectors in array STEP, by use of inter-
CD mediate primitive steps as calculated by PRMTIV on leaving SETGEN.
CD
CD Leaves value of sin theta/lambda in STHL in /BRAGG
CN Exactly like GETGEN, but sends out M and STHL also
C
      LOGICAL NOMORE,LATABS
      DIMENSION H(3),C(2),VEC(3)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /HKLGEN/STEP(3,3),PT(3,3),VECEND(3,3),PRPT(3,3),
     & NPRIM(2,2),NP,LFAC(2),MCOUNT(2),KOM5
C
      NOMORE = .FALSE.
    4 DO 1 L = 1,3
      CALL GMADD(PT(1,L),STEP(1,L),H,1,3)
      IF (SCALPR(H,VECEND(1,L))-1.) 2,2,1
    1 CONTINUE
C
C NEXT 3D LATTICE IF NON-PRIMITIVE:
C SIMULATE 'DO MCOUNT(1)=1,LFAC(1)'
      MCOUNT(1)=MCOUNT(1)+1
      IF (MCOUNT(1) .LE. LFAC(1)) GO TO 8
      MCOUNT(1)=1
C SIMULATE 'DO MCOUNT(2)=1,LFAC(2)
      MCOUNT(2)=MCOUNT(2)+1
      IF (MCOUNT(2) .GT. LFAC(2)) GO TO 101
C
C MAKE COEFFICIENTS OF STEP VECTORS FOR NEXT PRIMITIVE STEP:
   8  DO 7 I=1,2
       C(I)=FLOAT(MOD((MCOUNT(1)-1)*NPRIM(I,1)*LFAC(2)+(MCOUNT(2)-1)*
     & NPRIM(I,2)*LFAC(1),NP))/FLOAT(NP)
   7  CONTINUE
C
      DO 10 I=1,3
  10  VEC(I)=C(1)*STEP(I,1)+C(2)*STEP(I,2)+FLOAT(MCOUNT(2)-1)
     & *STEP(I,3)/FLOAT(LFAC(2))
      DO 9 L=1,3
      CALL GMADD(PRPT(1,L),VEC,PT(1,L),1,3)
   9  CONTINUE
      GO TO 4
C
C  NEW VALUES ARE IN H; RESET PT
    2 DO 3 J = 1,L
   3  CALL GMEQ(H,PT(1,J),1,3)
      M=MULBOX(H)
      IF (M .EQ. 0) GO TO 4
      STHL = VCTMOD(0.5,H,2)
      IF (STHL-STHLMX) 6,6,4
    6 IF (STHL-10.E-5) 4,5,5
    5 IF (LATABS(H)) GO TO 4
      GO TO 100
C
C  IF HERE HAVE NO MORE VALUES OF H,K,L TO OFFER
 101  NOMORE = .TRUE.
 100  RETURN
      END
C
C
C
C
C LEVEL 2      CHARACTER *4 FUNCTION GENNAM(NAME)
      CHARACTER *4 FUNCTION GENNAM(NAME)
C
C *** GENNAM by JCM 26 Mar 91 ***
C
CX
CC 13C
CH Finds all the starting letters of an atom name.
CA On entry NAME is an A4 CHARACTER variable
CA On exit  GENNAM is an A4 CHARACTER variable being all the leftmost letters
CA          of NAME
C
      CHARACTER *4 NAME
C
      GENNAM=' '
      DO 1 I=1,4
      IF (LETTER(NAME(I:I)) .LE. 0) GO TO 100
      GENNAM=NAME(1:I)
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE GEOMCO(N)
      SUBROUTINE GEOMCO(N)
C
C *** GEOMCO updated by JCM 24 Jan 90 ***
C
CX
CC 6C
CH Multiple entry routine for geometric slack constraints.
C
CA N on entry specifies action required:
CA    N=1 initialise: copy out constraint information for access later
CA        and read print instructions from I PRSK (this is used by Pawley
CA        slacks as well).
CA    N=2 entered from APSHSF: recalculate actual coordinates at bond ends
CA        after shift application to position coordinates
CA    N=3 entered from NWINSF: put out new L ATOM card
C
CP Slack constraints must have been set up in /SLKGEO/ via GEOMIN
CP Parameters and variables must be set up via PARSSF
C
      LOGICAL ONCARD
      CHARACTER *4 NAME
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CELFIX/IPTCEL(6),AMCELL(6),NCELF,NCELG,NCELS,KOM3
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
C NOT IF NO SLACK CONSTRAINTS:
      IF (NSLAK(1) .EQ. 0) GO TO 100
C
      GO TO (1,2,3) , N
C
C SETTING UP ENTRY:
   1  IPRNT(8)=0
      IF (ONCARD('I','PRSK',A)) IPRNT(8)=NINT(A)
      CALL MESS(LPT,1,'Printing of bond "obs" and calc values wanted')
      CALL DEPRIN(IPRNT(8))
C
C SET UP MATRIX TO CONVERT DERIVATIVES WRT A,B,C ETC TO DERIVATIVES
C WRT A*, B*, C* ETC:
      CALL CELMAT(TOSTAR)
C
C SET UP THE MATRIX OF 1S AND COSINES FOR THESE DERIVATIVES:
      CALL GMUNI(COSIN,3)
      COSIN(1,2)=CELL(3,2,1)
      COSIN(2,1)=CELL(3,2,1)
      COSIN(1,3)=CELL(2,2,1)
      COSIN(3,1)=CELL(2,2,1)
      COSIN(2,3)=CELL(1,2,1)
      COSIN(3,2)=CELL(1,2,1)
C
      GO TO 100
C
C ENTRY FROM APSHSF AFTER ALL SHIFTS APPLIED TO POSITION COORDINATES:
   2  DO 31 I=1,NTARNM
  31  CALL XTRANS(IABASE(I),XSLAK(1,I),ISYM(I),ILAT(I),
     & CELLTR(1,I))
      GO TO 100
C
C ENTRY FROM NWINSF ON FINDING L ATOM CARD - IDENTIFY ATOM:
   3  CALL RDWORD(NAME,LEN,7,IPT,80,0,IER)
      L=NCFIND(NAME,ATTNAM,NTARNM)
      WRITE (NEWIN,2000) NAME,ATNAME(IABASE(L)),ISYM(L),ILAT(L),
     & (CELLTR(I,L),I=1,3)
2000  FORMAT ('L ATOM ',1X,A4,1X,A4,2I5,3F4.0)
      GO TO 100
C
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE GEOMIN(N)
      SUBROUTINE GEOMIN(N)
C
C *** GEOMIN updated by JCM 8 Mar 91 ***
C
CX
CC 6A
CH Reads L cards for bond slack constraints.
CA On entry N=0 if this is an initial, very early entry to read the L SLAK
CA            card and decide whether there are any slack constraints.
CA          N=1 for all other cards
C
CD For N=0 reads and interprets first a possible L SLAK card, setting in
CD particular SLONLY if there are ONLY slack constraints, and NO observations.
CD For N=1 reads possibly several cards starting:
CD    L ATOM  define named atom not on A card for use in constraints,
CD    L BOND  define named bond between 2 given atoms;
CD            also used for type 1, bond=given length with sigma,
CD    L ANGL  define named angle between 2 given bonds, and implying the
CD            use of a third bond;
CD            also used for type 2, angle = given size in degrees, with sigma
CD    L EQUB  type 3, 2 bonds are of equal length, with sigma
CD    L LINE  type 4, 2 bonds are in a straight line (angle=180 degrees), with
CD            sigma,
CD    L TORS  define named torsion angle  between 2 non-intersecting bonds,
CD            needing to be given a bond joining the two, and implying
CD            the use of the 3 further bonds joining the 4 atoms;
CD            also used for type 5, torsion angle between 2 non-intersecting
CD            bonds = given size in degrees, with sigma
CD    L EQUA  type 6, 2 angles (each between 2 bonds) are equal, with sigma
CD    L PLAN  type 7;  an experimental type which for the moment requests that
CD            the atoms which follow (4 or more of them) are to be planar, with
CD            sigma.
C
      CHARACTER *4 NAME,ANAME,BNAME
C%
C      DIMENSION XS(3),CS(3),NEND(2),NIN(%PLAN%)
      DIMENSION XS(3),CS(3),NEND(2),NIN(20)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      GO TO (91,92) , N+1
C SET UP AS IF NO GEOMETRIC SLACK CONSTRAINTS:
  91  NSLAK(1)=0
      NUMBON=0
      NTARNM=0
      NUMANG=0
      NUMTOR=0
      SLONLY=.FALSE.
C
C SEEK L SLAK CARD:
      CALL FINDCD('L','SLAK',4,0,L)
      IF (L .LE. 0) THEN
        CALL MESS(LPT,1,'No geometric slack constraints')
        GO TO 100
      ENDIF
C
C FOR NOW READ TYPE (WHETHER OR NOT TO EXPECT CONVENTIONAL LSQ OBSERVATIONS
C AS WELL AS SLACK CONSTRAINTS), AND EXTRA MULTIPLICATIVE WEIGHTING
C FACTOR IF TYPE 2 -  EVENTUALLY MAY BE MORE ON THIS CARD
   1  CALL RDINTG(NSTYP,7,IPT,80,IER)
C UNITS DIGIT OF NSTYP IS 1 FOR BOND ALONE, 2 FOR LSQ OBS PLUS BONDS
C TENS DIGIT OF NSTYP IS WEIGHTING TYPE SPECIFICALLY FOR GEOMETRIC:
C   10=UNIT WEIGHTS (NOT NORMALLY SENSIBLE)
C   20=WEIGHTS TO BE USED AS READ (IE READ 1/SIGMA SQRD)
C   30=READ SIGMA, USE WEIGHT=1/SIGMA SQRD
C
      NST1=NSTYP/10
      SLONLY=NSTYP-NST1*10 .EQ. 1
      IF (SLONLY) THEN
        CALL MESS(LPT,1,'Refinement using bond slack constraints '//
     &  'only')
        CALL MESS(LPT,0,'If irrelevant L cards (REFI, MODE, WGHT, '//
     &  'TFAC, SCAL) have been given they will be ignored')
        SLAKWT(1)=1.
        GO TO 40
      ELSE
        CALL MESS(LPT,1,'Refinement using bond slack constraints '//
     &  'and conventional observations')
        GO TO 20
      ENDIF
C
  98  CALL ERRIN2(NSTYP,2,'slack constraint type','not allowed')
      GO TO 100
C
  20  CALL RDREAL(SLAKWT(1),IPT,IPT,80,IER)
      IF (SLAKWT(1) .EQ. 0.) SLAKWT(1)=1.
      WRITE (LPT,2005) SLAKWT(1)
2005  FORMAT (' Extra weighting factor = ',F12.4)
C HOLD IT AS SQRT, WHICH IS HOW IT IS NEEDED:
      SLAKWT(1)=SQRT(SLAKWT(1))
C
  40  IF (NST1 .LE. 0 .OR. NST1 .GT. 3) GO TO 98
      GO TO (41,42,43) ,NST1
  41  CALL MESS(LPT,0,'Unit weights')
      GO TO 100
C
  42  CALL MESS(LPT,0,'Weights used directly as read')
      GO TO 100
C
  43  CALL MESS(LPT,0,'Read sigma, weight by 1/(sigma sqrd)')
      GO TO 100
C
C READ L ATOM CARDS:
  92  K=0
  81  CALL FINDCD('L','ATOM',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 51
C
C READ TARGET ATOM NAME:
      CALL RDWORD(NAME,LEN,7,IPT,80,0,IER)
C READ ATOM SPECIFICATION;  FIND RELATED ATOM IN ASSY UNIT:
      CALL RDATOM(IPT,IA,XS,IS,IL,CS)
C ADD NAME AND ATOM SPEC TO LISTS:
      CALL ADDATM(NAME,IA,XS,IS,IL,CS,ITMP)
      GO TO 81
C
C READ L BOND CARDS:
  51  K=0
   2  CALL FINDCD('L','BOND',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 52
C
C READ NAME OF BOND:
      CALL RDWORD(BNAME,LEN,7,IPT,80,0,IER)
C
C READ ATOM NAMES AT 2 ENDS:
      CALL RDBOND(IPT,NEND,IE)
      IF (IE .NE. 0) GO TO 2
C
C ADD BOND TO LIST:
      CALL ADDBON(BNAME,NEND(1),NEND(2),NB)
C
C READ REQUIRED "OBSERVED" BOND, AND ITS ESD:
      CALL RDREAL(BOBS(NSLAK(1)+1),IPT,IPT,80,IER)
      IF (IER .EQ. 100) GO TO 2
C%
C      CALL ERRCHK(2,NSLAK(1),%SLAK%,0,'geometric slack constraints')
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
C COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT=NSKTOT+1
C TYPE 1 GEOMETRIC SLACK CONSTRAINTS: BOND=CONSTANT
      ITYPSK(NSLAK(1))=1
C 1 BOND INVOLVED:
      NINVB(NSLAK(1))=1
C AND WHICH ONE IT IS:
      INVBON(1,NSLAK(1))=NB
C
      WRITE (LPT,2001) BONNAM(NB),ATTNAM(IATM(NB,1)),
     & ATTNAM(IATM(NB,2))
2001  FORMAT (/' BOND slack constraint ',A4,' between atoms ',
     & 1X,A4,' and ',A4,':')
      WRITE (LPT,2002)(XSLAK(J,IATM(NB,1)),J=1,3),
     & (XSLAK(J,IATM(NB,2)),J=1,3),BOBS(NSLAK(1)),EOBS(NSLAK(1))
2002  FORMAT (' Actual  coordinates are:',3F10.5,' and ',3F10.5/
     & '           "Observed" bond =',F10.4,' with esd ',F10.5)
      GO TO 2
C
C READ L ANGL CARDS:
  52  IF (NUMBON .EQ. 0) GO TO 100
      K=0
   6  CALL FINDCD('L','ANGL',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 53
C READ NAME OF ANGLE:
      CALL RDWORD(ANAME,LEN,7,IPT,80,0,IER)
C
C READ 2 BOND NAMES TO DEFINE ANGLE, AND FIND THE THIRD SIDE OF TRIANGLE:
      CALL RDANGL(IPT,N1,N2,N3,NCOM,IE)
      IF (IE .NE. 0) GO TO 6
C ADD ANGLE TO LIST:
      CALL ADDANG(ANAME,N1,N2,N3,NA,IE)
      IF (IE .NE. 0) GO TO 6
C
C RECORD CONSTRAINT - THE "OPPOSITE" SIDE COMES FIRST:
      CALL RDREAL(BOBS(NSLAK(1)+1),IPT,IPT,80,IER)
      IF (IER .EQ. 100) GO TO 6
C%
C      CALL ERRCHK(2,NSLAK(1),%SLAK%,0,'geometric slack constraints')
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
C COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT=NSKTOT+1
C TYPE 2 GEOMETRIC SLACK CONSTRAINT - ANGLE=CONSTANT, NOT 0 OR 180
      ITYPSK(NSLAK(1))=2
C 3 BONDS INVOLVED (THOUGH ONLY 2 READ):
      NINVB(NSLAK(1))=3
      INVBON(1,NSLAK(1))=N1
      INVBON(2,NSLAK(1))=N2
      INVBON(3,NSLAK(1))=N3
      WRITE (LPT,2021) ANAME,BONNAM(N2),BONNAM(N3)
2021  FORMAT (/' ANGL slack constraint ',A4,' between bonds ',
     & 1X,A4,' and ',A4,':')
      WRITE (LPT,2022) ATTNAM(NCOM),BOBS(NSLAK(1)),EOBS(NSLAK(1))
2022  FORMAT ('            At atom ',A4/
     & '           "Observed" angle =',F10.1,' with esd ',F10.2)
      GO TO 6
C
C READ L EQUB CARDS:
  53  K=0
  63  CALL FINDCD('L','EQUB',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 54
C READ FIRST BOND NAME:
      CALL RDWORD(BNAME,LEN,7,IPT,80,0,IER)
      N1=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N1 .LE. 0) GO TO 11
C READ SECOND BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N2=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N2 .GT. 0) GO TO 12
C
  11  CALL ERRCH2(BNAME,2,' ','is not a bond name')
      GO TO 63
C
C RECORD CONSTRAINT
C%
C  12  CALL ERRCHK(2,NSLAK(1),%SLAK%,0,'geometric slack constraints')
  12  CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
C THE OBS IS ZERO HERE:
      BOBS(NSLAK(1))=0.
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
C COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT=NSKTOT+1
C TYPE 3 GEOMETRIC SLACK CONSTRAINT - EQUAL BONDS:
      ITYPSK(NSLAK(1))=3
C 2 BONDS INVOLVED:
      NINVB(NSLAK(1))=2
      INVBON(1,NSLAK(1))=N1
      INVBON(2,NSLAK(1))=N2
      WRITE (LPT,2031) BONNAM(N1),BONNAM(N2),EOBS(NSLAK(1))
2031  FORMAT (/' EQUB slack constraint - bond ',A4,' approx = bond ',
     &  A4,' with esd ',F10.5)
      GO TO 63
C
C READ L LINE CARDS:
  54  K=0
  64  CALL FINDCD('L','LINE',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 55
      IPT=7
      CALL RDANGL(IPT,N1,N2,N3,NCOM,IE)
      IF (IE .NE. 0) GO TO 64
C
C RECORD CONSTRAINT:
C%
C      CALL ERRCHK(2,NSLAK(1),%SLAK%,0,'geometric slack constraints')
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      BOBS(NSLAK(1))=0.
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
C COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT=NSKTOT+1
C TYPE 4 GEOMETRIC SLACK CONSTRAINT - BOND=SUM OF 2 OTHERS:
      ITYPSK(NSLAK(1))=4
C 3 BONDS INVOLVED (THOUGH ONLY 2 READ):
      NINVB(NSLAK(1))=3
      INVBON(1,NSLAK(1))=N1
      INVBON(2,NSLAK(1))=N2
      INVBON(3,NSLAK(1))=N3
      WRITE (LPT,2041) BONNAM(N2),BONNAM(N3),EOBS(NSLAK(1))
2041  FORMAT (/' LINE slack constraint - bonds ',A4,' and ',A4,
     &  'in an approximate straight line with esd ',F10.5)
      GO TO 64
C
C READ L TORS CARDS:
  55  K=0
  65  CALL FINDCD('L','TORS',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 56
C READ NAME OF TORSION ANGLE:
      CALL RDWORD(ANAME,LEN,7,IPT,80,0,IER)
C
C READ 3 BOND NAMES TO DEFINE ANGLE:
C READ FIRST BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N1=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N1 .LE. 0) GO TO 22
C READ SECOND BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N2=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N2 .LE. 0) GO TO 22
C READ THIRD BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N3=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N3 .GT. 0) GO TO 23
C
  22  CALL ERRCH2(BNAME,2,' ','is not a bond name')
      GO TO 65
C
C ADD ANGLE TO LIST:
  23  CALL ADDTOR(ANAME,N1,N2,N3,N4,N5,N6,NT,IE)
      IF (IE .GT. 0) GO TO 65
C
C RECORD CONSTRAINT :
      CALL RDREAL(BOBS(NSLAK(1)+1),IPT,IPT,80,IER)
      IF (IER .EQ. 100) GO TO 65
C%
C      CALL ERRCHK(2,NSLAK(1),%SLAK%,0,'geometric slack constraints')
      CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
C COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT=NSKTOT+1
C TYPE 5 GEOMETRIC SLACK CONSTRAINT - TORSION ANGLE=CONSTANT, NOT 0 OR 180
      ITYPSK(NSLAK(1))=5
C 6 BONDS INVOLVED (THOUGH ONLY 3 READ):
      NINVB(NSLAK(1))=6
      INVBON(1,NSLAK(1))=N1
      INVBON(2,NSLAK(1))=N2
      INVBON(3,NSLAK(1))=N3
      INVBON(4,NSLAK(1))=N4
      INVBON(5,NSLAK(1))=N5
      INVBON(6,NSLAK(1))=N6
      WRITE (LPT,2051) ANAME,BONNAM(N1),BONNAM(N3)
2051  FORMAT (/' TORS slack constraint ',A4,' between bonds ',
     & 1X,A4,' and ',A4,':')
      WRITE (LPT,2052) BONNAM(N2),BOBS(NSLAK(1)),EOBS(NSLAK(1))
2052  FORMAT ('   with common axis ',A4/
     & '           "Observed" angle =',F10.1,' with esd ',F10.2)
      GO TO 65
C
C READ L EQUA CARDS:
  56  K=0
  66  CALL FINDCD('L','EQUA',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 57
C
C READ FIRST ANGLE NAME:
      CALL RDWORD(ANAME,LEN,7,IPT,80,0,IER)
      K1=NCFIND(ANAME,ANGNAM,NUMANG)
      IF (K1 .LE. 0) GO TO 24
C READ SECOND ANGLE NAME:
      CALL RDWORD(ANAME,LEN,IPT,IPT,80,0,IER)
      K2=NCFIND(ANAME,ANGNAM,NUMANG)
      IF (K2 .GT. 0) GO TO 25
C
  24  CALL ERRCH2(ANAME,2,' ','is not a bond angle name')
      GO TO 66
C
C RECORD CONSTRAINT
C%
C  25  CALL ERRCHK(2,NSLAK(1),%SLAK%,0,'geometric slack constraints')
  25  CALL ERRCHK(2,NSLAK(1),500,0,'geometric slack constraints')
C THE OBS IS ZERO HERE:
      BOBS(NSLAK(1))=0.
      CALL RDREAL(EOBS(NSLAK(1)),IPT,IPT,80,IER)
C COUNT ALSO TOTAL CONSTRAINTS:
      NSKTOT=NSKTOT+1
C TYPE 3 GEOMETRIC SLACK CONSTRAINT - EQUAL BONDS:
      ITYPSK(NSLAK(1))=6
C 6 BONDS INVOLVED (THOUGH THEY MAY NOT ALL BE DISTINCT):
      NINVB(NSLAK(1))=6
      INVBON(1,NSLAK(1))=INANG(K1,1)
      INVBON(2,NSLAK(1))=INANG(K1,2)
      INVBON(3,NSLAK(1))=INANG(K1,3)
      INVBON(4,NSLAK(1))=INANG(K2,1)
      INVBON(5,NSLAK(1))=INANG(K2,2)
      INVBON(6,NSLAK(1))=INANG(K2,3)
      WRITE (LPT,2061) ANGNAM(K1),ANGNAM(K2),EOBS(NSLAK(1))
2061  FORMAT (/' EQUA slack constraint - angle ',A4,' approx = angle ',
     &  A4,' with esd ',F10.5)
      GO TO 66
C
C READ L PLAN CARDS:
  57  K=0
  67  CALL FINDCD('L','PLAN',4,K,L)
      K=L
      IF (L .LE. 0) GO TO 100
C
C DISCOVER HOW MANY ATOMS INVOLVED:
      N=0
      IPT=7
  31  CALL RDWORD(ANAME,LEN,IPT,IPT,80,0,IER)
** USE THE ENTRY WHICH DETECTS A READ NUMBER, & SET EOBS
      IF (IER .EQ. 100) GO TO 32
C%
C      CALL ERRCHK(2,N,%PLAN%,0,'atoms constrained to be planar')
      CALL ERRCHK(2,N,20,0,'atoms constrained to be planar')
      NIN(N)=NCFIND(ANAME,ATTNAM,NTARNM)
      IF (NIN(N) .EQ. 0) THEN
        CALL ERRATM(ANAME,2,'L PLAN card')
        GO TO 67
      ENDIF
      GO TO 31
C
  32  CALL ADDPLN(NIN,N)
**      ITYPSK( ETC TO BE SET EITHER HERE OR IN ADDPLN
      GO TO 67
C
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE GEOMLS(ALSQ,MATSZ)
      SUBROUTINE GEOMLS(ALSQ,MATSZ)
C
C *** GEOMLS updated by JCM 2 Oct 90 ***
C
CX
CC 6B
CH Calculates bond lengths and derivatives for geometrical slack
CH constraints.
C
CA ALSQ and MATSZ are handed all through LSQ programs in this fashion
CA      - they are needed here for the call of MATTOT
C
CP On entry, COMMON /SLKGEO/ should contain:
CP      XSLAK holding actual x,y,z coordinates for all atoms involved in
CP            bonds
CP      ISYM holding the number of the symmetry operator which takes
CP      original coords into actual, -ve if by -x,-y,-z also
CP      ILAT holding the number of the lattice translation
CP      CELLTR the cell translations
CP
CP The symmetry must have been set up in SYMOP, and the original positions
CP      read from the A cards by ATOPOS.  The cell parameters must have
CP      been read by RECIP.
C
CD Called from MAIN programs to add to the LSQ matrix once per cycle.
CD First, for every involved bond, calculates the bond length and its
CD 12 derivatives, remembering that the actual position coordinates involved
CD may be related to those which are being refined.
CD
CD Then scans all geometrical constraints, forming calculated function
CD (which for types 1, 2, bonds, angles, are already there).
CD Proceeds exactly as though these are conventional observations and
CD calculated functions;  makes basic variable derivatives, gets weights,
CD and adds totals in to LSQ matrix.
CO Prints obs and calc list if requested on I card.
C
C
      CHARACTER *5 F1
      CHARACTER *20 F2,F4,F5,F7
      CHARACTER *22 F3
      CHARACTER *10 F6
      CHARACTER *24 F8
      CHARACTER *100 FORMA
      LOGICAL PRNCYC,RADS,HEAD,TESTOV,PRIN
      DIMENSION ALSQ(MATSZ)
      DIMENSION DETH2(3),DETH3(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
      DATA F1,F2,F3,F4,F5,F6,F7,F8/
     & '  No.',
     & '   Obs       Calc   ',
     & '    Diff      Esd     ',
     & '    Atom1     Atom2',
     & '    Bond1     Bond2',
     & ' Bond sum ',
     & '   Angle1     Angle2',
     & ' Ang1 bonds   Ang2 bonds'/
C
C OUT IF NO SLACK CONSTRAINTS:
      IF (NSLAK(1) .EQ. 0) GO TO 100
C
C HEADING FOR OBS/CALC BOND PRINTING IF REQUESTED:
      PRIN=PRNCYC(8)
      IF (PRIN) CALL MESS(LPT,1,'  Slack Constraints:')
C
C TYPE 1, GEOMETRICAL SLACK CONSTRAINTS - TYPE 3 IS PAWLEY-TYPE:
      ISLKTP=1
C COUNT ALL INVOLVED BONDS:
      DO 5 IB=1,NUMBON
   5  CALL BONDER(IB)
C
C COUNT ALL GEOMETRICAL SLACK CONSTRAINTS:
      DO 1 ISK=1,NSLAK(1)
C
C CLEAR WHOLE DERIVATIVE VECTOR - ONLY A FEW ITEMS WILL BE FILLED BY
C ANY PARTICULAR BOND:
      IF (LVARV .GT. 0) CALL GMZER(DERIVV,1,LVARV)
C
C SET UP POINTERS TO INVOLVED BONDS:
      DO 2 I=1,NINVB(ISK)
   2  NVB(I)=INVBON(I,ISK)
C
C WHICH TYPE IS THIS CONSTRAINT?
      GO TO (31,32,33,34,35,36,37) , ITYPSK(ISK)
C
** ERROR - TYPE NOT IMPLEMENTED
C
C BOND LENGTH:
  31  YCALC=BCALC(NVB(1))
      DERBON(1)=1.
      GO TO 50
C
C TYPE 2 - ANGLE BETWEEN 2 BONDS:
  32  B1=BCALC(NVB(1))
      B2=BCALC(NVB(2))
      B3=BCALC(NVB(3))
C YCALC = THETA RADIANS:
      CALL BONCOS(B1,B2,B3,YCALC,COSTH,SINTH,DERBON)
      GO TO 50
C
C TYPE 3 - EQUAL BONDS:
  33  YCALC=BCALC(NVB(1))-BCALC(NVB(2))
      DERBON(1)=1.
      DERBON(2)=-1.
      GO TO 50
C
C TYPE 4 - LINE:
  34  YCALC=BCALC(NVB(1))-BCALC(NVB(2))-BCALC(NVB(3))
      DERBON(1)=1.
      DERBON(2)=-1.
      DERBON(3)=-1.
      GO TO 50
C
C TYPE 5 - TORS:
  35  B1=BCALC(NVB(1))
      B2=BCALC(NVB(2))
      B3=BCALC(NVB(3))
      B4=BCALC(NVB(4))
      B5=BCALC(NVB(5))
      B6=BCALC(NVB(6))
C DEFINE ANGLE THETA2 BETWEEN B1 & B2:
      CALL BONCOS(B5,B1,B2,TEMP,COSTH2,SINTH2,DETH2)
C AND ANGLE THETA3 BETWEEN B2 AND B3:
      CALL BONCOS(B6,B2,B3,TEMP,COSTH3,SINTH3,DETH3)
      COTTH2=COSTH2/SINTH2
      COTTH3=COSTH3/SINTH3
C
C THE EXPRESSION FOR COS PHI, THE TORSION ANGLE, IS NOW
C C=B/D + COT THETA2 * COT THETA3, WHERE THE NUMERATOR, B, AND DENOMINATOR,
C                                  D, ARE GIVEN BY:
      B=B5*B5-B4*B4+B6*B6-B2*B2
      D=2.*B1*B3*SINTH2*SINTH3
      C=B/D+COTTH2*COTTH3
      CALL SINCOS(C,S,'GEOMLS')
      YCALC=ARCCOS(C)
C
C DERIVATIVES OF C WRT ALL 6 BONDS:
      CSEQ2=1./(SINTH2*SINTH2)
      CSEQ3=1./(SINTH3*SINTH3)
      X2=B*COTTH2 + COTTH3*CSEQ2
      X3=B*COTTH3 + COTTH2*CSEQ3
C
      DERBON(1)=-B/B1-DETH2(2)*X2
      DERBON(2)=-2.*B1/D-DETH2(3)*X2-DETH3(2)*X3
      DERBON(3)=-B/B3-DETH3(3)*X3
      DERBON(4)=-2.*B4/D
      DERBON(5)=2.*B5/D-DETH2(1)*X2
      DERBON(6)=2.*B6/D-DETH3(1)*X3
C CONVERT TO BEING DERIVATIVES OF PHI:
      DO 51 I=1,6
  51  DERBON(I)=-DERBON(I)/S
      GO TO 50
C
C TYPE 6 - EQUAL ANGLES:
  36  B1=BCALC(NVB(1))
      B2=BCALC(NVB(2))
      B3=BCALC(NVB(3))
      B4=BCALC(NVB(4))
      B5=BCALC(NVB(5))
      B6=BCALC(NVB(6))
      CALL BONCOS(B1,B2,B3,TH1,COSTH1,SINTH1,DERBON)
      CALL BONCOS(B4,B5,B6,TH2,COSTH2,SINTH2,DETH2)
      YCALC=TH1-TH2
      DO 52 I=1,3
  52  DERBON(I+3)=-DETH2(I)
      GO TO 50
C
C TYPE 7 - PLANE:
  37  GO TO 50
C
C JOIN HERE WITH YCALC, AND DERBON= D(YCALC)/EACH INVOLVED BOND -
C MAKE DERIVATIVES OF YCALC WRT ANY RELEVANT VARIABLE:
  50  DO 41 I=1,6
C MAKE 6 DERIVATIVES OF YCALC WRT 6 RECIPROCAL CELL QUADRATIC PARS:
      L=KCPARS(I)
      IF (L .GT. 0) THEN
        S=0.
        DO 49 J=1,NINVB(ISK)
  49    S=S+DERBON(J)*DERCEL(I,NVB(J))
        DERIVV(L)=S
      ENDIF
  41  CONTINUE
C
C DERIVATIVES OF YCALC WRT THE POSITION COORDS OF THE 3 ATOMS:
C COUNT INVOLVED BONDS:
      DO 42 IB=1,NINVB(ISK)
C COUNT X,Y,Z
      DO 42 I=1,3
C COUNT BOTH ENDS OF A BOND:
      DO 42 J=1,2
      L=KX(I,IABASE(IATM(NVB(IB),J)))
      IF (L.GT.0) DERIVV(L) = DERIVV(L)+
     & DERPOS(I,NVB(IB),J)*DERBON(IB)
  42  CONTINUE
C
      CALL RELATE
      OBS=BOBS(ISK)
C FOR ANGLES, USER TALKS IN DEGREES, PROGRAM WORKS IN RADIANS:
      RADS = (ITYPSK(ISK).EQ.2 .OR. ITYPSK(ISK).EQ.5 .OR.
     &  ITYPSK(ISK).EQ.6)
      IF (RADS) OBS=RADIAN(OBS)
      DIFF=OBS-YCALC
C
C WEIGHT
      GO TO (21,22,23) ,NST1
  21  SQRTWT=1.
      GO TO 20
C
  22  SQRTWT=SQRT(EOBS(ISK))
      GO TO 29
C
  23  IF (TESTOV(1.,EOBS(ISK))) THEN
        SQRTWT=0.
      ELSE
        SQRTWT=1./EOBS(ISK)
      ENDIF
  29  IF (RADS) SQRTWT=DEGREE(SQRTWT)
  20  IF (.NOT. SLONLY) SQRTWT=SQRTWT*SLAKWT(1)
      WT=SQRTWT*SQRTWT
      WDIFF=SQRTWT*DIFF
      IF (PRIN) THEN
        IF (ISK .EQ. 1) THEN
          HEAD = .TRUE.
        ELSE
          HEAD =(ITYPSK(ISK) .NE. ITYPSK(ISK-1))
          F1=' '
          F3=' '
        ENDIF
        GO TO (61,62,63,64,65,66,67) , ITYPSK(ISK)
C
  61    IF (HEAD) THEN
          FORMA='('''//F1//F2//F3//F4//''')'
          WRITE(LPT,FORMA)
        ENDIF
        WRITE(LPT,2001) BONNAM(NVB(1)),OBS,YCALC,DIFF,EOBS(ISK),
     &   ATTNAM(IATM(NVB(1),1)),ATTNAM(IATM(NVB(1),2))
2001    FORMAT (1X,A4,F10.4,F10.4,F10.5,G12.4,6X,A4,6X,A4)
        GO TO 60
C
  62    IF (HEAD)  THEN
          FORMA='('''//F1//F2//F3//F5//''')'
          WRITE(LPT,FORMA)
        ENDIF
        CALL ADDANG('    ',NVB(1),NVB(2),NVB(3),IA,IE)
        WRITE(LPT,2009) ANGNAM(IA),BOBS(ISK),DEGREE(YCALC),DEGREE(DIFF),
     &   EOBS(ISK),BONNAM(NVB(2)),BONNAM(NVB(3))
2009    FORMAT (1X,A4,F10.2,F10.2,F10.3,G12.4,6X,A4,6X,A4)
        GO TO 60
C
  63    IF (HEAD) THEN
          FORMA='('''//F1//F5//F3//F5//''')'
          WRITE(LPT,FORMA)
        ENDIF
        WRITE(LPT,2011) BCALC(NVB(1)),BCALC(NVB(2)),DIFF,EOBS(ISK),
     &   BONNAM(NVB(1)),BONNAM(NVB(2))
2011    FORMAT (1X,'EQUB',F10.4,F10.4,F10.5,G12.4,6X,A4,6X,A4)
        GO TO 60
C
  64    IF (HEAD) THEN
          FORMA='('''//F1//F5//F3//F6//F5//''')'
          WRITE (LPT,FORMA)
        ENDIF
        WRITE(LPT,2002) ISK,BCALC(NVB(2)),BCALC(NVB(3)),
     &   DIFF,EOBS(ISK),BCALC(NVB(1)),BONNAM(NVB(2)),BONNAM(NVB(3))
2002    FORMAT (1X,'LINE',3F10.4,F10.5,G12.4,6X,A4,6X,A4)
        GO TO 60
C
  65    IF (HEAD) THEN
          FORMA='('''//F1//F2//F3//F5//''')'
          WRITE (LPT,FORMA)
        ENDIF
        CALL ADDTOR('    ',NVB(1),NVB(2),NVB(3),NVB(4),NVB(5),NVB(6),
     &  IT,IE)
        WRITE(LPT,2009) TORNAM(IT),BOBS(ISK),DEGREE(YCALC),DEGREE(DIFF),
     &  EOBS(ISK),BONNAM(NVB(1)),BONNAM(NVB(3))
        GO TO 60
C
  66    IF (HEAD) THEN
          FORMA='('''//F1//F7//F3//F8//''')'
          WRITE(LPT,FORMA)
        ENDIF
        WRITE(LPT,2003) DEGREE(TH1),DEGREE(TH2),DEGREE(DIFF),
     &  EOBS(ISK),BONNAM(NVB(2)),BONNAM(NVB(3)),
     &  BONNAM(NVB(5)),BONNAM(NVB(6))
2003    FORMAT (1X,'EQUA',2F10.2,F10.3,G12.4,2(2X,A4,'^ ',A4,2X))
        GO TO 60
C
  67  GO TO 60
C
      ENDIF
  60  CALL MATTOT(ALSQ,MATSZ)
C
C ADD IN BOND-TYPE SLACK CONSTRAINT STATISTICS:
      CALL RFACS(4)
   1  CONTINUE
C PRINT BOND-TYPE SLACK CONSTRAINT STATISTICS:
      CALL RFACS(5)
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE GETDC(H,DIREC)
      SUBROUTINE GETDC(H,DIREC)
C
C *** updated by PJB 24-Apr-1995 ***
C
CX
CC 2B
CH Calculates direction cosines of the incident and diffracted beams
CH used by absorption correction type integrals
CA On entry H is a 1x3 real vector holding h,k,l
CA On exit  DIREC is a (3,2) real array holding the direction cosines
CA                of the incident (in (,1)) and diffracted (in (,2)) beams
CP SETDC should have set up the calculation by interpreting "D" cards.
CD Calculates the direction cosines with respect to the CCSL orthogonal axes.
C
      DIMENSION H(3),DIREC(3,2),P(2),W(3),U(6)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /DGEOM/IGEOM,UM(9),NLR,ANGLIN(3),ALAMBD(5,5),
     & NLAMB,ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
C SGNROT to be applied to given angles to get conventional rotations
      SGNROT=FLOAT(3-2*IABS(NLR))
C SGNDIF to be applied to calculated angles which depend on which side
C the diffracted beam comes out
      SGNDIF=-FLOAT(ISIGN(1,NLR))
C
      GO TO (21,21,21,50,50,22,22,22,50,50,22,22),IGEOM
C
  50  CALL ERRIN2(IGEOM,0,'GETDC does not deal with type',
     & '- only types 1,2,3,6,7,8,11 and 12')
C
C TYPES 6,7,8 11 AND 12 NEED Z DIFFRACTOMETER AXIS
  22  DO 23 I=1,3
  23  U(I)=UM(3*I)
      GO TO 24
C
C TYPES 1,2,3 NEED Z UP AXIS
  21  CALL GMEQ(UM(1),U(1),1,3)
C
  24  CALL ORTHO(H,U(4),2)
      CALL UNIVEC(U(4),DSTAR)
C PUT THE CORRECT SIGN ON THETA
      SINTH=SGNDIF*WLGTH*DSTAR*0.5
      CALL SINCOS(SINTH,COSTH,'GETDC1')
      P(1)=SCALPR(U(1),U(4))

      CALL SINCOS(P(1),P(2),'GETDC2')
C
      IF (IGEOM.EQ.12) GO TO 30
C DEFAULT FOR 4-CIRCLE BISECTING AND EQUIINCLINATION
      CSY=0.
      SNY=SGNDIF
      GO TO (1,2,3,50,50,3,4,1,50,50,3),IGEOM
C
C NORMAL BEAM (MAXIM AND D3) GEOMETRY
   1  CSY=(SINTH*P(1))/(COSTH*P(2))
      CALL SINCOS(CSY,SNY,'GETDC3')
      SNY=SNY*SGNDIF
      GO TO 3
C
C GENERAL 5-CIRCLE
   30 COSRHO=SIN(DIFANG(5))/(2.*ABS(SINTH))
C ASSUME NU ALWAYS MEASURED + PARALLEL TO "Z"
      CALL SINCOS(COSRHO,SINRHO,'GETDC6')
      SINX=SINTH/SINRHO
      CALL SINCOS(SINX,COSX,'GETDC9')
C FOR NEGATIVE DIFFRACTION ANGLES NEED 180-X
      COSX=COSX*SGNDIF
      XANG=ATAN2(SINX,COSX)
      SGNB=SIGN(1.,XANG-SGNROT*DIFANG(2))
      COSA=SINTH*COSRHO/(COSTH*SINRHO)
      CALL SINCOS(SINA,COSA,'GETDC7')
      CALL TRIAN1(COS(DIFANG(3)),P(1),COSRHO,COSB,1)
      CALL SINCOS(COSB,SINB,'GETDC8')
      COSY=COSA*COSB-SGNB*SINA*SINB
      SINY=SINA*COSB+SGNB*COSA*SINB
      GO TO 3

C GENERAL 4-CIRCLE
    4 DIFF=SINTH-SIN(DIFANG(1)/2)
      IF (ABS(DIFF).GT..01) THEN
        WRITE (MESSAG,1000) h,DIFF
 1000 FORMAT ('Calculated and observed sintheta''s for',
     &3F6.2, 'differ by',F8.4)
        CALL ERRMES(1,-1,MESSAG)
      ENDIF
      A=((DIFANG(1)/2)-DIFANG(2))*SGNROT
      SNY=SGNDIF*SIN(A)*SIN(DIFANG(3))
      CALL SINCOS(SNY,CSY,'GETDC5')
C
C 4-CIRCLE BISECTING AND EQUI-INC JOIN HERE
   3  CALL VECPRD(U(1),U(4),W)
C  FOR HIGH CHI
      IF (IGEOM.EQ.11) CALL GMREV(W,W,3,1)
      GO TO 2
C
C NORMAL BEAM EQUATORIAL (MINIM) JOINS HERE
   2  DO 11 I=1,3
      CALL TRIAN1(U(I),P(1),U(3+I),CSA,1)
      CALL SINCOS(CSA,SNA,'GETDC4')
      GO TO (7,8,7,50,50,7,7,7,50),IGEOM
C
    7 SNA=SIGN(SNA,W(I))
      CSA=CSA*CSY-SNA*SNY
      GO TO 9
C
C MINIM (NORMAL BEAM EQUATORIAL)
   8  CSA=SGNDIF*CSA
C MAXIM, D3, 4-CIRCLE BISECTING AND EQUI ALL JOIN
   9  DO 10 J=1,2
      CALL TRIAN1(DIREC(I,J),U(I+3),SINTH,CSA,2)
  10  CSA=-CSA
  11  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE GETGEN(H,NOMORE)
      SUBROUTINE GETGEN(H,NOMORE)
C
C *** GETGEN by PJB 27 Jun 84 ***
C
CX
CC 3B
CH Gives the next useful set of h,k,l scanning the asymmetric unit of
CH reciprocal space.
C
CA On exit  H is a 1x3 real vector holding next generated values of h,k,l
CA          NOMORE is TRUE if there are no more to be found
CP SYMOP should have read the space group symmetry.
CP SYMUNI should have found the asymmetric unit of reciprocal space.
CP SETGEN should have set up stepping over asymmetric unit in /HKLGEN
C
CD Uses "previous" h,k,l in PT in /HKLGEN to move to a new one.
CD Rejects lattice absences (but not space group absences - do those outside
CD                 using ISPABS if required),
CD         h,k,l outside asymmetric unit,
CD         h,k,l for which sin theta/lambda is greater than STHLMX,
CD         and h,k,l for which sin theta is around zero (to reject 0,0,0)
CD
CD Allows for non-primitive stepping vectors in array STEP, by use of inter-
CD mediate primitive steps as calculated by PRMTIV on leaving SETGEN.
CN There also exists GENMUL which sends out M also
C
      LOGICAL NOMORE,LATABS
      DIMENSION H(3),C(2),VEC(3)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /HKLGEN/STEP(3,3),PT(3,3),VECEND(3,3),PRPT(3,3),
     & NPRIM(2,2),NP,LFAC(2),MCOUNT(2),KOM5
C
      NOMORE = .FALSE.
    4 DO 1 L = 1,3
      CALL GMADD(PT(1,L),STEP(1,L),H,1,3)
      IF (SCALPR(H,VECEND(1,L))-1.) 2,2,1
    1 CONTINUE
C
C NEXT 3D LATTICE IF NON-PRIMITIVE:
C SIMULATE 'DO MCOUNT(1)=1,LFAC(1)'
      MCOUNT(1)=MCOUNT(1)+1
      IF (MCOUNT(1) .LE. LFAC(1)) GO TO 8
      MCOUNT(1)=1
C SIMULATE 'DO MCOUNT(2)=1,LFAC(2)
      MCOUNT(2)=MCOUNT(2)+1
      IF (MCOUNT(2) .GT. LFAC(2)) GO TO 101
C
C MAKE COEFFICIENTS OF STEP VECTORS FOR NEXT PRIMITIVE STEP:
   8  DO 7 I=1,2
       C(I)=FLOAT(MOD((MCOUNT(1)-1)*NPRIM(I,1)*LFAC(2)+(MCOUNT(2)-1)*
     & NPRIM(I,2)*LFAC(1),NP))/FLOAT(NP)
   7  CONTINUE
C
      DO 10 I=1,3
  10  VEC(I)=C(1)*STEP(I,1)+C(2)*STEP(I,2)+FLOAT(MCOUNT(2)-1)
     & *STEP(I,3)/FLOAT(LFAC(2))
      DO 9 L=1,3
      CALL GMADD(PRPT(1,L),VEC,PT(1,L),1,3)
   9  CONTINUE
      GO TO 4
C
C  NEW VALUES ARE IN H; RESET PT
    2 DO 3 J = 1,L
   3  CALL GMEQ(H,PT(1,J),1,3)
      IF (MULBOX(H) .EQ. 0) GO TO 4
      STHL = VCTMOD(0.5,H,2)
      IF (STHL-STHLMX) 6,6,4
    6 IF (STHL-10.E-5) 4,5,5
    5 IF (LATABS(H)) GO TO 4
      GO TO 100
C
C  IF HERE HAVE NO MORE VALUES OF H,K,L TO OFFER
 101  NOMORE = .TRUE.
 100  RETURN
      END
C
C

C
C
C LEVEL 2      FUNCTION IATOM(ANAME)
      FUNCTION IATOM(ANAME)
C
C *** IATOM updated for MK4 by JCM 8 Feb 90 ***
C
CX
CC 11C
CH Identifies an atom name in a given list.
CA On entry ANAME is an A4 CHARACTER atom name, left justified
CA On exit  IATOM=0 if this is not a name in the existing list, or
CA               =a positive integer, being the number of the atom whose
CA                name this is, if found.
CP The list must be set up in array ATNAME in /ATNAM by, e.g. ATOPOS
C
      CHARACTER *4 ANAME
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
      IF (NATOM .EQ. 0) THEN
        IATOM=0
      ELSE
        IATOM=NCFIND(ANAME,ATNAME,NATOM)
      ENDIF
      RETURN
      END
C
C

C
C
C LEVEL 6      SUBROUTINE IICD1
      SUBROUTINE IICD1
C
C *** IICD1 updated by JCM 7 Aug 92 ***
C
CX
CC 6A
CH Interprets basic I cards to drive any LSQ.
C
CD Deals with defaults, recognises words on I cards, reads values after them,
CD and prints out the information.
CD
CD Vocabulary recognised:
CD NCYC <number of cycles of refinement wanted> : default 3
CD CYC1 <number to be given to first cycle> : default 1
CD PRIN <request for general printing on the LPT file>
CD        followed by an integer giving the frequency of printing:
CD           0= never
CD           1=first cycle
CD           2=last cycle
CD           3= first and last cycles
CD           4= every cycle
CD MCOR <maximum correlation>
CD       or 0 for "print whole matrix"
CD       or -ve number for "print nothing"
CD PRDM <integer> ; a non-zero integer requests Deposited Material printing
CD in the same cycle that a new crystal data file is output.
CD CONV <real> to stop LSQ cycling if max (shift/esd) < CONV : default 0.01
C
      CHARACTER *4 INEED(6)
      LOGICAL ONCARD
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      DATA INEED/'NCYC','CYC1','PRIN','MCOR','PRDM','CONV'/
C
C SET UP DEFAULTS FOR VALUES POSSIBLY TO BE READ FROM I CARDS:
      NCYC=3
      NCYC1=1
      IPRNT(1)=2
      MAXCOR=70
      IPRNT(7)=0
      CONV=0.01
C
C INTERPRET ANY QUANTITIES GIVEN ON I CARD:
      DO 18 I=1,6
      IF (.NOT. ONCARD('I',INEED(I),A)) GO TO 18
C
C WORD RECOGNISED - BRANCH ON WHAT TO DO WITH DATA:
   3  GO TO (11,12,13,14,15,16) , I
C
C NCYC:
  11  NCYC=NINT(A)
      GO TO 18
C
C CYC1:
  12  NCYC1=NINT(A)
      GO TO 18
C
C PRIN:
  13  IPRNT(1)=NINT(A)
      GO TO 18
C
C MCOR:
  14  MAXCOR=NINT(A)
      GO TO 18
C
C PRDM:
  15  IPRNT(7)=NINT(A)
      GO TO 18
C
C CONV:
  16  CONV=A
      GO TO 18
C
  18  CONTINUE
C
C ARRANGE LAST CYCLE NUMBER, AND WHETHER SIMPLY SIMUATION:
      SIMUL=.FALSE.
      LASTCY=NCYC1+NCYC-1
      IF (LASTCY .LT. NCYC1) THEN
        LASTCY=NCYC1
        SIMUL=.TRUE.
      ENDIF
C WRITE OUT VALUES FROM POTENTIAL I CARD:
   1  IF (.NOT. SIMUL) THEN
      WRITE (LPT,2001) NCYC,NCYC1
2001  FORMAT (/1X,I4,' cycles of refinement with first numbered',I4)
      ELSE
      WRITE (LPT,2011) NCYC1
2011  FORMAT (/' Cycle number ',I4,' is a simulation')
      ENDIF
      CALL MESS(LPT,1,'Printing of obs and calc values wanted')
      N=IPRNT(1)
      CALL DEPRIN(N)
C
      IF (MAXCOR.LT.0) CALL MESS(LPT,1,'No correlations to be printed')
C
      IF (MAXCOR .EQ. 0) CALL MESS(LPT,1,
     & 'Whole correlation matrix to be printed')
C
      IF (MAXCOR .GT. 0) WRITE (LPT,2005) MAXCOR
2005  FORMAT (/' Correlations exceeding',I4,' per cent to be printed')
C
      WRITE (LPT,2006) CONV
2006  FORMAT (/' Cycles to stop if max(shift/esd) < ',F10.4)
C
      IF (IPRNT(7) .NE. 0) CALL MESS(LPT,1,'File for Deposited'//
     & ' Material to be output on same cycle as new CDF')
C
 100  RETURN
      END
C
C

C
C
C
C LEVEL 1      SUBROUTINE INBOX(H,IN)
      SUBROUTINE INBOX(H,IN)
C
C *** INBOX by PJB/JCM 3 Aug 83 ***
C
CX
CC 3B
CH Determines whether the given reflection is inside, on, or outside
CH the reciprocal space asymmetric unit.
CA On entry H holds h,k,l
CA On exit  IN = -1 if outside asymmetric unit
CA                0 if inside it
CA              = number of plane if on a plane
CA              = 10+number of edge if on an edge
CP SYMOP must have read the space group
CP SYMUNI must have set up the asymmetric unit
CN Assumes that NASYM (number of planes bounding the asymmetric unit) is not
CN greater than 3
C
      DIMENSION H(3)
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
C
      I=0
      IN=0
      IE=3
      IF (NASYM .EQ. 0) GO TO 100
      DO 1 J=1,NASYM
      TEST=(ASYM(1,J)*H(1)+ASYM(2,J)*H(2)+ASYM(3,J)*H(3))
      IF (ABS(TEST).LT.10.E-4) TEST=0.0
      IF (TEST) 2,3,4
   2  IN=-1
      GO TO 100
   3  I=I+1
      IP=J
      GO TO 1
    4 IE=J
   1  CONTINUE
      IF (I.EQ.2) IN=IE+10
      IF (I.EQ.1) IN=IP
 100  RETURN
      END
C
C


C
C
C LEVEL 2      SUBROUTINE INDFIX(H,K)
      SUBROUTINE INDFIX(H,K)
C
C *** INDFIX by JCM 20 Jul 83 ***
C
CX
CC 3C
CH Converts 3 real items in H to be integers in K.
CA On entry H is a 1x3 real vector
CA On exit  K is a 1x3 integer vector containing the numbers in H, rounded.
C
      DIMENSION H(3),K(3)
C
      DO 1 I=1,3
   1  K(I)=NINT(H(I))
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE INDFLO(H,K)
      SUBROUTINE INDFLO(H,K)
C
C *** INDFLO by JCM 22 Nov 83 ***
C
CX
CC 3C
CH Converts 3 integers in K into floating point reals in H.
CA On entry K is a 1x3 integer vector
CA On exit  H is a 1x3 real vector containing the elements of K floated.
C
      DIMENSION H(3),K(3)
C
      DO 1 I=1,3
   1  H(I)=FLOAT(K(I))
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE INITIL(PROGRM)
      SUBROUTINE INITIL(PROGRM)
C
C *** INITIL updated by PJB for Unix 24-Oct-1994 ***
C
CX
CC 13C
CH Initialises the CCSL system.
CA On entry PROGRM holds A6 name of calling program
CD INITIL is obeyed at the start of a job, either by an explicit call or
CD (more usually) from a call of PREFIN.
CD It sets up various default start conditions, some of which are machine
CD dependent.  The user may wish to override some of these later.
CD
CD The symbols permitted on input are copied into /CHARS;  this will one day be
CD done in a BLOCK DATA statement.
CD  The system is set up to be not BATCH, already ini tialised.
CD The MAIN program name is kept;  the hardware (RVAX, DIVA etc) is kept; the
CD wordlength is set;  the filenames are cleared;  the file unit numbers are
CD initialised.
CD
CD For DIVA, unit LPT is opened automatically to <Program name>.LIS
CD For other systems, the name for unit LPT is requested, and it is then opened
CD
CD The constants pi, log2 etc are put into /CONSTA
CO The banner headline is put out on LPT
C
      LOGICAL ISOPEN
      CHARACTER *23 STARS
      CHARACTER *(*) PROGRM
      CHARACTER *1 IDIGI1,LETTS,ISMBO1,ISPC1
      DIMENSION IDIGI1(10),LETTS(52),ISMBO1(21)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FINAME/FILNAM(15)
      CHARACTER *10 FILNAM
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS
      COMMON /LOONEY/IOTAB(15),LUNTAB(15)
      COMMON /MAPGT/ZGTVAL(20),ZCGT,IGT,IZGT,IDUMPG
      COMMON /MAPRD/ZRDVAL(20),ZCRD,IRD,IZRD,IDUMPR
      COMMON /MAPSV/ZSVVAL(20),ZCSV,ISV,IZSV,NDUMPS,NSAV
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRSTAT/SMYC,SMYD,SMYO,SMIO,SMID,SMWYOS,IZCT,
     & P5,IOP1,IOP2,KMI(9),KMA(9)
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      COMMON /WHEN/DAT,TIM(2),MAIN
      CHARACTER *5 TIM
      CHARACTER *10 DAT
      CHARACTER *6 MAIN
      DATA IDIGI1/'1','2','3','4','5','6','7','8','9','0'/
      DATA LETTS /'A','B','C','D','E','F','G','H','I','J','K','L',
     & 'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b',
     & 'c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r',
     & 's','t','u','v','w','x','y','z'/
      DATA ISPC1/' '/
      DATA ISMBO1/'.',',',':',';','!','?','''','"','$','/','(',
     & ')','|','-','+','=','*','#','&','>','<'/
C
C FOR THE CONVENIENCE OF THOSE WHO FIND THE ABOVE STATEMENT EYE-CROSSING,
C THE ELEMENTS OF THE ARRAY ISMBOL WILL BE:
C  1 IS .  (FULL STOP OR DECIMAL POINT  OR DOT)
C  2 IS ,  (COMMA)
C  3 IS :  (COLON OR TWO DOTS)
C  4 IS ;  (SEMI-COLON OR DOT AND COMMA)
C  5 IS !  (EXCLAMATION MARK OR SHRIEK)
C  6 IS ?  (QUESTION MARK OR QUERY)
C  7 IS '  (APOSTROPHE OR SINGLE QUOTE)
C  8 IS "  (DOUBLE QUOTES OR CONVERSATION MARKS)
C  9 IS $  (DOLLAR)
C 10 IS /  (SLASH OR OBLIQUE OR STROKE OR SOLIDUS)
C 11 IS (  (OPENING PARENTHESIS OR LEFT BRACKET)
C 12 IS )  (CLOSING PARENTHESIS OR RIGHT BRACKET)
C 13 IS |  (VERTICAL BAR OR LOGICAL "OR")
C 14 IS -  (MINUS OR DASH)
C 15 IS +  (PLUS)
C 16 IS =  (EQUALS)
C 17 IS *  (STAR OR ASTERISK OR MULTIPLICATION SIGN)
C 18 IS #  (SHARP OR HASH)
C 19 IS &  (AMPERSAND OR AND)
C 20 IS >  (GREATER THAN OR CLOSING ANGLE BRACKET)
C 21 IS <  (LESS THAN OR OPENING ANGLE BRACKET)
C
C THE SEQUENCE ABOVE HAS BEEN TAKEN FROM THE CAMBRIDGE IBM 3081 NEW
C SOFTWARE FOR PLOTTERS, AND THE PLOTTING OF CHARACTERS IN THIS PACKAGE WILL
C USE TABLES FROM THAT SOFTWARE.  THE LAST TWO CHARACTERS DO NOT OCCUR IN
C THAT TABLE.
C
C IF THE SYSTEM IS IMPLEMENTED ON A NEW MACHINE, IT IS AS WELL TO CHECK THAT
C THE ABOVE TABLE MAKES SENSE - E.G. THAT WHERE THE WORD SAYS "DOLLAR" THE
C SYMBOL IS AN S WITH A VERTICAL LINE THROUGH IT, ETC.  IF NOT, SOME
C ADJUSTMENTS WILL HAVE TO BE MADE.  CHARACTER NUMBER 9 WILL ALWAYS PLOT AS
C DOLLAR, BUT IF THE ABOVE TABLE IS NOT RIGHT THE PRINTING SYMBOL WILL BE
C DIFFERENT.  THIS SHOULD ONLY HAPPEN WITH LESSER USED CHARACTERS, NOT LETTERS,
C DIGITS ETC.
C
C DO NOT OBEY INITIL TWICE IN ONE JOB, UNLESS FORCED TO BY MAIN:
      IF (NINIT .EQ. 42) GO TO 100
C SET THAT INITIL HAS BEEN OBEYED;  THIS RELIES ON NINIT ** NOT **
C BEING SET TO 42 BY ANY OTHER METHOD:
      NINIT=42
C
C SET NOT IN BATCH;  BATCH JOBS MUST "CALL BATCH" ** AFTER ** THEIR
C "CALL INITIL"
C
C SAVE NAME OF CALLING PROGRAM
      MAIN=PROGRM
C
C SET UP CHARACTERS INTO COMMON:
      DO 1 I=1,26
      LETUP(I)=LETTS(I)
   1  LETLOW(I)=LETTS(I+26)
      DO 2 I=1,10
   2  IDIGIT(I)=IDIGI1(I)
      ISPCE=ISPC1
      DO 3 I=1,21
   3  ISMBOL(I)=ISMBO1(I)
C
C SET UP "SINGLE PHASE":
      NPHASE=1
      IPHASE=1
      JPHASE=1
      KPHASE=1
      MULFAS=.FALSE.
      MULONE=.FALSE.
C AND "SINGLE SOURCE":
      NSOURC=1
      JSOURC=1
      KSOURC=1
      MULSOU=.FALSE.
C
C  SET UP DEFAULTS FOR MACHINE DEPENDENT QUANTITIES:
C SET (FOR RUN TIME) THE SYSTEM FOR WHICH CCSL WAS GENERATED:
      NSYSTM=0
CVMSO      NSYSTM=1
CUNIX
      NSYSTM=3
C3084      NSYSTM=4
      CALL UPONE(PROGRM,NSYSTM)
C
C NUMBER OF BITS IN AN INTEGER (OR WORD) MINUS AT LEAST 1:
      NBITS=31
C
C DEFAULTS FOR MULTIPHASE, MULTISOURCE - SET NEITHER OF THESE:
      MULFAS=.FALSE.
      MULSOU=.FALSE.
C
C DEAL WITH THE LINE PRINTER OUTPUT FILE, LPT.
C  AND INPUT OUTPUT TABLES FOR USE BY OPNFIL AND NOPFIL:
C%
C      DO 4 I=1,%FILE%
      DO 4 I=1,15
      IOTAB(I)=0
C  CLEAR FILE-NAMES
      FILNAM(I)=' '
    4 LUNTAB(I)=19+I
CRAL
      LUNTAB(3)=40
C
C  SET RESERVED IOUNITS
C
C TELETYPE IN:
      ITI=5
C TELETYPE OUT:
      ITO=6
C PLOTTER, IF NEEDED EVER:
      IPLO=0
C LPT IS SPECIAL BECAUSE WE NEED TO WRITE TO IT HERE AND NOW, WITHOUT
C GIVING THE MAIN PROGRAM A CHANCE TO ALTER ITS VALUE.  OTHER UNITS WILL BE
C CLEARED HERE, BUT MAY BE SET LATER BY MAIN, BEFORE THEY ARE FIRST USED.
C WE NEED TO CATER FOR A PROGRAM WHICH STARTS:
C      LPT=8
C      CALL INITIL(PROGRM)
C FOR RVAX, IF BOTH LPT > 0 AND UNIT LPT OPEN, USE THAT VALUE OF
C LPT, AND DO NOT REOPEN IT.  OTHERWISE, USE LPT=12
      NAMFIL='.LIS'
CUNIX
       CALL UPONE(NAMFIL,3)
      IF (LPT .GT. 0) THEN
CVMS
      INQUIRE(LPT,OPENED=ISOPEN)
CVMS
      IF (.NOT. ISOPEN) CALL OPNFIL(LPT,2143)
C3084      CALL OPNFIL(LPT,3)
      ELSE
      LPT=12
CILL      MESSAG=PROGRM
CILL      CALL OPNFIL(LPT,2132)
CRAL
      CALL OPNFIL(LPT,2143)
C3084      CALL OPNFIL(LPT,2243)
      ENDIF
C
C CLEAR ALL OTHER NAMED UNITS, SO THAT IF THEY ARE SET BY MAIN OR
C BATCH WE WILL NOTICE THIS:
      ICRYDA=-9999
      NEWIN=-9999
      LKH=-9999
      LUNI=-9999
      IDUMPR=-9999
      IDUMPG=-9999
      NDUMPS=-9999
      IOP1=-9999
      IOP2=-9999
C
      STARS='* * * * * * * * * * * *'
      DO 88 I=1,LENGT(MAIN)
      STARS(5+2*I:5+2*I)=MAIN(I:I)
  88  CONTINUE
      WRITE (LPT,2000) STARS
2000  FORMAT (/20X,A23//8X,' Cambridge ',
C%
C     & 'Crystallography Subroutine Library     Mark %MARK%.%VERS%')
     & 'Crystallography Subroutine Library     Mark 4.12')
C OBTAIN DATE AND TIME:
CVMS
! JvdS Was:
!      CALL DATE(DAT)
      DAT = '03-AUG-01'
CUNIX
      DAT(10:)=' '
CVMS
      CALL TIME(TIM(1))
CVMS
      WRITE (LPT,2001) TIM(1),DAT
CVMS
 2001 FORMAT (/20X,'Job run at ',A5,' on ',A10)
C
C CONSTANTS TO MACHINE ACCURACY:
      PI=4.0*ATAN(1.0)
      RAD=PI/180.
      DEG=180./PI
      TWOPI=2.*PI
      FOURPI=4.*PI
      PIBY2=PI/2.
      ALOG2=ALOG(2.)
      SQL2X8=SQRT(8.*ALOG2)
      VALMUB=0.2695
 100  RETURN
      END
C
C
C



C
C
C LEVEL 4      SUBROUTINE INPUTA(ID,LABA,LBALEN,LABS,LBSLEN,X,T,S,IER)
      SUBROUTINE INPUTA(ID,LABA,LBALEN,LABS,LBSLEN,X,T,S,IER)
C
C *** INPUTA updated by JCM 26 Sep 89 ***
C
CX
CC 4A
CH Reads an A card and prepares it for further processing.
CA On entry ID points to the card on unit IO10.  If ID=0 it is assumed
CA             that the card is already in ICARD in /SCRACH
CA On exit LABA is the A4 label of the atom, starting with a letter
CA         LBALEN is the number of characters in LABA
CA         LABS is the A4 label of the scattering factor expected
CA         LBSLEN is the number of characters in LABS
CA         X(1:3) holds EITHER the x,y,z fractional coordinates of the atom
CA                      OR (if the card starts "C SD") their standard
CA                          deviations.
CA         T similarly holds EITHER the isotropic temperature factor
CA                           OR (if the card starts "C SD") its esd
CA         S similarly holds EITHER the site occupation factor
CA                           OR (if the card starts "C SD") its esd
CA         IER=number of errors detected
CD Items must occur in the sequence "C" optional "SD" <label>
CD x,y,z itf, optional DIFFERENT scattering factor label, optional site
CD occupation factor.
CD If the site is left blank, S=1. is assumed.
C
      CHARACTER *4 LABA,LABS
      DIMENSION X(3)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IER=0
C IF ID=0 ON ENTRY, CARD EXPECTED ALREADY IN /SCRACH/; OTHERWISE READ IT:
      IF (ID .NE. 0) CALL CARDIN(ID)
      SDREAD=(ICARD(3:5) .EQ. 'SD ')
C IPT MOVES ALONG CARD AS ITEMS TAKEN FROM IT:
      IPT=3
      IF (SDREAD) IPT=6
      CALL RDWORD(LABA,LBALEN,IPT,IPT,80,0,IE)
**I THINK THIS IS BELT AND BRACES . .
      IF (IE .NE. 0) GO TO 8
      IF (LETTER(LABA(1:1)) .NE. 0) GO TO 1
   8  CALL ERRCH2(LABA,-2,' ',
     & 'as an atom label does not start with a letter')
      IER=IER+1
C
   1  DO 2 I=1,3
      IPKEEP=IPT
      CALL RDREAL(X(I),IPT,IPT,80,IE)
      IF ((IE .NE. 0) .AND. (IE .NE. 100)) THEN
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in coordinate',
     &   'on A card')
        IER=IER+1
      ENDIF
   2  CONTINUE
      CALL RDREAL (T,IPT,IPT,80,IE)
      IF ((IE .NE. 0) .AND. (IE .NE. 100)) THEN
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in isotropic t f',
     &   'on A card')
        IER=IER+1
      ENDIF
      IPKEEP=IPT
C SET EXPLICIT SITE=0. SO THAT WE KNOW IF WE HAVE READ ONE:
      S=0.
C
C NOW LOOK FOR SCATTERING FACTOR LABEL AND/OR SITE OCCUPATION FACTOR:
      CALL RDWORD(LABS,LBSLEN,IPT,IPT,80,0,IE)
C IF NOTHING MORE ON CARD, GO TO SET S=1 AND DEDUCE SCAT LABEL:
      IF (IE .EQ. 100) GO TO 3
C IF SOMETHING ON CARD, BUT NOT A LABEL, REREAD IT AS SITE:
      IF (IE .NE. 0) IPT=IPKEEP
      CALL RDREAL(S,IPT,IPT,80,IE)
      IF ((IE .NE. 0) .AND. (IE .NE. 100)) THEN
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in site occ factor',
     &   'on A card')
        IER=IER+1
      ENDIF
C
C A SITE OF ZERO, OR ABSENT, INDICATES 1:
   3  IF (S .EQ. 0.) S=1.
C
C IF SCAT LABEL READ, FINISH:
      IF (LBSLEN .GT. 0) GO TO 100
C NO EXPLICIT LABEL, SO MUST TAKE FIRST LETTERS OF ATOM LABEL:
      L=0
   6  L=L+1
      IF (L .GT. LBALEN) GO TO 7
      IF (LETTER(LABA(L:L)) .EQ. 0) GO TO 7
      LABS(L:L)=LABA(L:L)
      GO TO 6
C
C IF SCAT LABEL TAKEN AS FIRST LETTERS OF ATOM LABEL, SHOW BY -VE LENGTH:
   7  LBSLEN=1-L
C
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTC(ID,CELL)
      SUBROUTINE INPUTC(ID,CELL)
C
C *** INPUTC updated by JCM 26 Sep 89 ***
C
CX
CC 1A
CH Reads one "C" card.
CA On entry ID points to the card on file IO10
CA On exit CELL(1:6) holds the 6 numbers read.
CD What the read numbers are depends on the presence of "SD".  If no "SD"
CD these will be a,b,c, alpha degrees, beta degrees, gamma degrees
CD (where anything on the RIGHT of the card may be omitted if it is
CD deducible from the symmetry).
CD If the card starts "C SD" the 6 numbers are esds.
CN Note the loss of the facility in fixed format to omit any deducible
CN numbers which are NOT on the RIGHT of the card.  The reading routines
CN will carry on until they find something, or till the card end.
C
      DIMENSION CELL(6)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      CALL CARDIN(ID)
      IPT=3
      SDREAD=(ICARD(3:4) .EQ. 'SD')
      IF (SDREAD) IPT=5
      CALL GMZER(CELL,1,6)
      DO 1 J=1,6
      IPKEEP=IPT
      CALL RDREAL(CELL(J),IPT,IPT,80,IER)
      IF (IER .EQ. 100) GO TO 100
      IF (IER .EQ. 0) GO TO 1
      CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in cell parameter',
     & 'on "C" card')
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE INPUTD
      SUBROUTINE INPUTD
C
C *** INPUTD updated by JCM 14 Oct 86 ***
C
CX
CC 2A
CH Reads and interprets all D cards, for general diffraction information.
C
CD Absorbs all the D cards given.  It will normally be called by a
CD setting up routine (e.g. SETDC) which then checks whether it has had
CD enough D cards to make sense, and gives default values for those missing.
CD
CD The syntax of D cards is thus like that of M, G, L cards;  a word of up
CD to 4 symbols (usually letters, and the first must be a letter) follows the
CD "D space" which starts the card, and what follows after that depends on
CD the word.  Possible words recognised are:
C
CD D GEOM followed by an integer, IGEOM.
CD        Possible values for IGEOM are:
CD   1 = normal beam
CD   2 = normal beam equatorial
CD   3 = equi-inclination Weissenberg
CD   4 = precession (no further code for this)
CD   5 = anti equi-inclination Weissenberg
CD   6 = 4 circle diffractometer, bisecting geometry
CD   7 = 4 circle diffractometer, angles given
CD   8 = normal beam, general orientation
CD  10 = powder data, X-rays
CD  11 = 4 circle diffractometer, high chi geometry
CD  12 = SXD geometry
CD
CD D WVLN followed by the wavelength, in same units as the lattice constants
CD        (possibly several wavelength values)
CD
CD D L/R  followed by an integer, the 'left or right' indicator:
CD        +ve means incident beam diffracted to the right,
CD        -ve means to the left. Value 1 means  that angles are
CD        measured clockwise about positive diffractometer axes, and 2
CD        means anti-clockwise.
CD
CD D TH2M followed by the value of 2theta monochromator, in degrees.
CD
CD D UMAT followed by 9 numbers, giving the UB matrix (as defined in the
CD        Manual under D cards).  If all 9 do not fit one card,
CD        several cards may be given;  for example, on 3 cards each with
CD        3 numbers.
CD
CD D ANGP followed by the precession angle in degrees.
CD
CD D ROTA for types 1-5, followed by 3 indices, being the zone-axis symbols
CD        of the zone parallel to the rotation axis.
CD
CD D CHIA for type 2 only, followed by 3 integers and a real;  the integers
CD        give another axis, perpendicular to that on the "D ROTA" card, and
CD        the real is the angle between this and the normal to the chi circle,
CD        in degrees.
C
CI Reads all D cards
C
CO Writes its findings to unit LPT
C
      CHARACTER *27 X27
C%
C      CHARACTER *4 DTABLE(%DCRD%),DWD
      CHARACTER *4 DTABLE(8),DWD
      DIMENSION ITEMP(9),UMAT(9)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /DGEOM/IGEOM,UM(9),NLR,ANGLIN(3),ALAMBD(5,5),
     & NLAMB,ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /DREAD/IDREAD(8)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA DTABLE/'GEOM','WVLN','L/R','TH2M','UMAT','ANGP',
     & 'ROTA','CHIA'/
      DATA X27/'                           '/
C
C SET "NO D CARDS READ":
C%
C      CALL JGMZER(IDREAD,1,%DCRD%)
      CALL JGMZER(IDREAD,1,8)
C
C CLEAR COUNT OF UB MATRIX ELEMENTS READ:
      NMAT=0
C
C READ ALL D CARDS:
      INREAD(4)=-IABS(INREAD(4))
      ID=IABS(INREAD(4))
      NCARD=ICDNO(4)
      IF (NCARD .LE. 0) THEN
        CALL MESS(LPT,1,'No D cards given')
        GO TO 100
      ENDIF
C
      DO 3 ICD=1,NCARD
C
C UNLIKE MK2, MK3 READS ITS D CARDS IN SEQUENCE AND DECIDES WHAT TO DO THEN
C ON THE EVIDENCE OF WHICH 4 LETTER WORD FOLLOWS THE D.
C
      CALL CARDIN(ID)
      ID=ID+NYZ
      CALL RDWORD(DWD,LEN,3,IPT,80,0,IER)
C%
C  42  L=NCFIND(DWD,DTABLE,%DCRD%)
  42  L=NCFIND(DWD,DTABLE,8)
      IF (L .LE. 0) THEN
        CALL ERRCH2(DWD,2,'word','on D card not recognised')
        GO TO 3
      ENDIF
C
C SET "HAVE READ PARTICULAR WORD" (COUNTING NUMBER OF CARDS), THEN BRANCH:
      IDREAD(L)=IDREAD(L)+1
      GO TO (31,32,33,34,35,36,37,38) , L
C
C D GEOM:
  31  CALL RDINTG(IGEOM,IPT,IPT,80,IER)
      IF (IGEOM .GT. 0 .AND. IGEOM .LT. 12) GO TO 8
      CALL ERRIN2(IGEOM,2,'data type','not allowed')
      GO TO 3
C
   8  WRITE (LPT,2020) IGEOM
2020  FORMAT (/' Data geometry type',I5,' -')
      GO TO (11,12,13,14,15,16,17,18,19,20,21), IGEOM
  11  CALL MESS(LPT,0,X27//'normal beam')
      GO TO 43
  12  CALL MESS(LPT,0,X27//'normal beam equatorial')
      GO TO 43
  13  CALL MESS(LPT,0,X27//'equi-inclination Weissenberg')
      GO TO 43
  14  CALL MESS(LPT,0,X27//'precession camera')
      GO TO 43
  15  CALL MESS(LPT,0,X27//'anti-equinclination Weissenberg')
      GO TO 43
  16  CALL MESS(LPT,0,X27//'4 circle diffractometer bisecting geometry')
      GO TO 43
   17 CALL MESS(LPT,0,X27//'4 circle diffractometer angles to be given')
      GO TO 43
  18  CALL MESS(LPT,0,X27//'D3 polarised neutron diffractometer')
      GO TO 43
  19  CALL MESS(LPT,0,X27//'powder data - neutrons')
      GO TO 43
  20  CALL MESS(LPT,0,X27//'powder data - X rays')
      GO TO 43
   21 CALL MESS(LPT,0,X27//'4-circle diffractometer: high chi setting')
      GO TO 43
C
C WVLN:
C%
C  32  CALL RDNUMS(ALAMBD,IPT,%LAMB%,NLAMB,IER)
  32  CALL RDNUMS(ALAMBD,IPT,5,NLAMB,IER)
      IF (NLAMB .GT. 0) GO TO 52
      CALL ERRMES(1,1,'no values on D WVLN card')
      GO TO 3
C
  52  IF (NLAMB .GT. 1) WRITE (LPT,2011) (ALAMBD(I,1),I=1,NLAMB)
2011  FORMAT (/' Wavelength values',5F12.5)
      IF (NLAMB .EQ. 1) WRITE (LPT,2012) WLGTH
2012  FORMAT (/' Wavelength =',F12.5)
      GO TO 3
C
C L/R:
  33  CALL RDINTG(NLR,IPT,IPT,80,IER)
      IF (IABS(NLR) .LE. 2) GO TO 51
      CALL ERRIN2(NLR,2,'left/right indicator is',
     & 'and should be +/-1 or +/-2')
      GO TO 43
C
  51  IF (NLR .GT. 0) CALL MESS(LPT,1,'Beam diffracted to right')
      IF (NLR .LT. 0) CALL MESS(LPT,1,'Beam diffracted to left')
      IF (IABS(NLR).GT.1) THEN
        CALL MESS(LPT,0,X27//'positive angles correspond to'//
     &  ' anti-clockwise rotations')
      ELSE
        CALL MESS(LPT,0,X27//'positive angles correspond to'//
     &  ' clockwise rotations')
      ENDIF
      GO TO 43
C
C TH2M:
  34  CALL RDREAL(ANGLIN(2),IPT,IPT,80,IER)
      WRITE (LPT,2041) ANGLIN(2)
2041  FORMAT(' Monochromator 2theta = ',F10.4,' degrees')
      ANGLIN(2)=COS(RADIAN(ANGLIN(2)))
      GO TO 43
C
C UMAT:
  35  CALL RDNUMS(UMAT(NMAT+1),IPT,9,NM,IER)
      NMAT=NMAT+NM
C CANNOT CHECK TILL ALL THERE
      GO TO 3
C
C ANGP:
  36  CALL RDREAL(ANGLIN(1),IPT,IPT,80,IER)
      WRITE (LPT,2061) ANGLIN(1)
2061  FORMAT(' Precession angle = ',F10.4,' degrees')
C FOR NOW WE DO NO MORE WITH PRECESSSION
      GO TO 43
C
C ROTA:
  37  CALL RDNUMS(UM,IPT,3,N,IER)
      CALL INDFIX(UM,ITEMP)
      WRITE (LPT,2071) (ITEMP(I),I=1,3)
2071  FORMAT (/' Zone axis parallel to rotation axis is',3I5)
      GO TO 3
C
C CHIA:
  38  CALL RDNUMS(UM(4),IPT,4,N,IER)
      CALL INDFIX(UM(4),ITEMP)
      WRITE (LPT,2081) (ITEMP(I),I=1,3),ANGLIN(1)
2081  FORMAT (/' Axis',3I5,' makes angle',F10.4,' with chi circle')
C
C KEEP ANGLE IN RADIANS:
      ANGLIN(1)=RADIAN(UM(7))
      GO TO 3
C
C HERE WHEN ONE ITEM OF THE <WORD> <NUMBER> TYPE READ - LOOK FOR OTHERS:
  43  CALL RDWORD(DWD,LEN,IPT,IPT,80,0,IER)
      IF (IER .EQ. 0) GO TO 42
      IF (IER .NE. 100) CALL ERRIN2(IPT,2,'word expected at point',
     & 'on D card')
   3  CONTINUE
C
C ALL D CARDS READ:
C
      IF (NMAT .NE. 0) THEN
C CHECK IF ANY UB MATRIX ELEMENTS AT ALL, HAVE HAD 9:
        IF (NMAT .NE. 9) THEN
          CALL ERRIN2(NMAT,2,'only','elements of UB matrix read')
        ELSE
          CALL GMEQ(UMAT,UM,3,3)
          WRITE (LPT,2051) UM
2051      FORMAT (/' UB Matrix: ',3F10.5/2(12X,3F10.5/))
        ENDIF
      ENDIF
C
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE INPUTE
      SUBROUTINE INPUTE
C
C *** INPUTE updated by JCM 29 Aug 91 ***
C
CX
CC 2A
CH Reads and interprets an E card, and sets up extinction calculations.
CD The card gives IEXTYP, an integer giving the extinction model to be used:
CD     IEXTYP = 1 Becker and Coppens Lorentzian model, 1 DOMR
CD     IEXTYP = 2 Becker and Coppens Gaussian model, 1 DOMR
CD     IEXTYP = 3 Becker and Coppens Lorentzian model, 3 DOMRs, FOVLP
CD     IEXTYP = 4 Becker and Coppens Gaussian model, 1 DOMRs, FOVLP
CD Sets LOGICALs LOREN and GAUSS.
CD After IEXTYP come the parameters of the model.  In the present options
CD 1,2 these are DOMR ("r" in the theory) and MOSC ("g" in the theory), the
CD domain radius and mosaic spread parameters.
CD For options 3,4 reads 3 values for DOMR, then MOSC and FOVLP, in the case
CD where coherence between nuclear and magnetic scattering must be taken
CD into account.
CI Reads an E card.
CO Writes its findings on unit LPT.
C
      DIMENSION A(5)
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /EXTN/IEXTYP,DOMR,KDOMR,AMOSC,KMOSC,EXTCOR,CEXT(4),
     & DEXDFQ,DEXDRQ,DEXDGQ,LOREN,GAUSS
      LOGICAL LOREN,GAUSS
      COMMON /EXTRAE/DOMRI(3),FOVLP,KDOMRI(3),KFOVLP
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      INREAD(5) = -IABS(INREAD(5))
      GO TO (1,2) , ICDNO(5)+1
      CALL ERRMES(1,0,'only one E card allowed')
C
   1  IEXTYP=0
      GO TO 10
C
   2  CALL GMZER(DOMRI,3,1)
      CALL CARDIN(IABS(INREAD(5)))
      CALL RDINTG(IEXTYP,2,IPT,80,IER)
  10  LOREN=(IEXTYP.EQ.1 .OR. IEXTYP .EQ.3)
      GAUSS=(IEXTYP.EQ.2 .OR. IEXTYP .EQ.4)
      IF (IEXTYP .EQ. 0) THEN
        CALL MESS(LPT,1,'No extinction correction')
        GO TO 100
      ELSE IF (IEXTYP .EQ. 1 .OR. IEXTYP .EQ. 2) THEN
        CALL RDREAL(DOMR,IPT,IPT,80,IER)
        CALL RDREAL(AMOSC,IPT,IPT,80,IER)
      ELSE IF (IEXTYP .EQ. 3 .OR. IEXTYP .EQ. 4) THEN
        CALL RDNUMS(A,IPT,5,NUM,IER)
        IF (NUM .NE. 5) THEN
          CALL ERRIN2(NUM,1,'Too few numbers on E card - 5 needed',
     &     'were read')
          GO TO 100
        ELSE
          CALL GMEQ(A,DOMRI,3,1)
** IN CASE YOU WERE RELYING ON IT:
          DOMR=DOMRI(1)
          AMOSC=A(4)
          FOVLP=A(5)
        ENDIF
C
      ELSE
        CALL ERRIN2(IEXTYP,-2,'extinction type',
     &   'not defined - assuming no extinction')
        IEXTYP = 0
        GO TO 100
      ENDIF
C
      IF (LOREN) WRITE (LPT,2002) IEXTYP
2002  FORMAT (/' Extinction type is',I3,' Becker and Coppens',
     & ' Lorentzian model')
      IF (GAUSS)  WRITE (LPT,2003) IEXTYP
2003  FORMAT (/' Extinction type is',I3,' Becker and Coppens',
     & ' Gaussian model')
 101  IF (IEXTYP .LE. 2) WRITE (LPT,2004) DOMR,AMOSC
2004  FORMAT ('  Domain radius =',F10.4,'  Mosaic spread =',F10.4)
      IF (IEXTYP .EQ. 3 .OR. IEXTYP .EQ. 4) WRITE (LPT,2005) A
2005  FORMAT (' Domain radii:',3F10.4,' Mosaic spread:',F10.4,
     &  ' Coherent overlap fraction:',F10.4)
C
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      SUBROUTINE INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
C
C *** INPUTF by JCM 1 Mar 84 ***
C
CX
CC 4A
CH Reads and partially interprets an "F" card.
CA On entry ID points in the binary copy of the Crystal Data File to the
CA required "F" card, or iz 0, indicating that the card must be read here.
CA On exit:
CA    LABF, A4,  contains a scattering factor label
CA    LBFLEN is the number of actual characters in LABF
CA    NTYP is an integer giving the type of factor expected
CA    IPT points in the card in /SCRACH/ to the next data item
CA    IER is an error indicator, =0 if no errors.
C
      CHARACTER *4 LABF
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IER=0
      IF (ID .NE. 0) CALL CARDIN(ID)
C IPT MOVES ALONG CARD AS ITEMS TAKEN FROM IT:
      CALL RDWORD(LABF,LBFLEN,3,IPT,80,0,IE)
      IF (IE .NE. 0) GO TO 8
      IF (LETTER(LABF(1:1)) .NE. 0) GO TO 1
   8  CALL ERRCH2(ICARD(3:6),-2,' ',
     & 'read from "F" card where label expected')
      IER=IER+1
C
   1  IPKEEP=IPT
      CALL RDINTG(NTYP,IPT,IPT,80,IE)
      IF ((IE .NE. 0) .AND. (IE .NE. 100)) THEN
        IER=IER+1
        CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,'in form factor type',
     &   'on "F" card')
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTG
      SUBROUTINE INPUTG
C
C *** INPUTG updated by JCM 14 Oct 86 ***
C
CX
CC 2A
CH Reads and interprets all "G" cards, for Gaussian integration of various
CH integrals over a crystal defined by its plane faces.
C
CD Absorbs all the "G" cards given.  It will normally be called by a
CD setting up routine (e.g. SETABS) which then checks whether it has had
CD enough "G" cards to make sense, and gives default values for any missing.
CD
CD The syntax of "G" cards is thus like that of M, D, L cards; a word of up to
CD 4 symbols (usually letters, and the first must be a letter) follows the
CD "G space" which starts the card, and what follows after that depends on
CD the word.  Possible words recognised are:
CD
CD G FACE followed by an integer and other numbers: a specification of a plane
CD        face of the crystal, according to the integer.
CD        (1 means read A,B,C,D in the equation AX+BY+CZ>=D)
CD
CD G PNTS followed by 3 integers which are the number of Gauss points required
CD        for integration in the x, y, and z directions.  These may be any
CD        integer in the range 1-20,32,40 (the list is being extended.)
CD
CD        The MAIN program may wish to alter these during a run.  This would
CD        involve resetting NL,NM and NN in COMMON /GAUSS/, and obeying
CD        SETGAU again.
CD
CD        If no G PNTS card is given, 5 points will be taken in all 3 directions
CD
CD G MU followed by the coefficient of absorption.  Its units must tie up with
CD        A,B,C,D above. (This card is not essential; if the MAIN program
CD        calls for different values of mu, these are simply written to AMU
CD        in the COMMON /ABSDAT/)
CD
CD G MODE followed by an integer which gives mode of use of ABSOR or ABMULT,
CD        i.e. which integral(s) are to be calculated.
C
CI Reads in all "G" cards.
CO Writes its findings to unit LPT.
C
C%
C      CHARACTER *4 GWD,GTABLE(%GCRD%)
      CHARACTER *4 GWD,GTABLE(4)
      COMMON /ABSDAT/AMU,MODEA
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CPLANE/AA(15),BB(15),CC(15),DD(15),NP
      COMMON /GAUSS/XX(1000),YY(1000),ZZ(1000),WW(1000),NL,NM,NN,NQ
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /GREAD/IGREAD(4)
      DATA GTABLE/'FACE','PNTS','MU','MODE'/
C
C SET "NO G CARDS READ":
C%
C      CALL JGMZER(IGREAD,1,%GCRD%)
      CALL JGMZER(IGREAD,1,4)
C
C INITIALISE COUNTS OF ITEMS WHICH MAY COME ON MORE THAN 1 CARD:
      NP=0
C
C READ ALL "G" CARDS:
      INREAD(7)=-IABS(INREAD(7))
      ID=IABS(INREAD(7))
      NCARD=ICDNO(7)
      IF (NCARD .LE. 0) CALL ERRMES(2,0,'"G" cards')
C
      DO 3 ICD=1,NCARD
C
      CALL CARDIN(ID)
      ID=ID+NYZ
      CALL RDWORD(GWD,LEN,3,IPT,80,0,IER)
C%
C  42  L=NCFIND(GWD,GTABLE,%GCRD%)
  42  L=NCFIND(GWD,GTABLE,4)
      IF (L .LE. 0) THEN
        CALL ERRCH2(GWD,2,'cannot recognise word',
     &   'on "G" card')
        GO TO 3
      ENDIF
C
C SET "HAVE READ PARTICULAR WORD" (COUNTING NUMBER OF CARDS), THEN BRANCH:
      IGREAD(L)=IGREAD(L)+1
      GO TO (31,32,33,34) , L
C
C FACE:
  31  IER=IERR
C%
C      CALL ERRCHK(2,NP,%CPLN%,0,'plane faces')
      CALL ERRCHK(2,NP,15,0,'plane faces')
      IF (IER .NE. IERR) GO TO 3
C
      CALL RDREAL(AA(NP),IPT,IPT,80,IER)
      CALL RDREAL(BB(NP),IPT,IPT,80,IER)
      CALL RDREAL(CC(NP),IPT,IPT,80,IER)
      CALL RDREAL(DD(NP),IPT,IPT,80,IER)
      IF (DD(NP) .GT. 0.) GO TO 3
      AA(NP)=-AA(NP)
      BB(NP)=-BB(NP)
      CC(NP)=-CC(NP)
      DD(NP)=-DD(NP)
      GO TO 3
C
C PNTS:
  32  CALL RDINTG(NL,IPT,IPT,80,IER)
      CALL RDINTG(NM,IPT,IPT,80,IER)
      CALL RDINTG(NN,IPT,IPT,80,IER)
      WRITE (LPT,2000) NL,NM,NN
2000  FORMAT (/' Gauss points for integration in x,y and ',
     & 'z directions are',3I5)
      GO TO 3
C
C MU:
  33  CALL RDREAL(AMU,IPT,IPT,80,IER)
      WRITE (LPT,2003) AMU
2003  FORMAT (/' Coefficient of absorption is ',F10.5)
      GO TO 43
C
C MODE:
  34  CALL RDINTG(MODEA,IPT,IPT,80,IER)
      WRITE (LPT,2004) MODEA
2004  FORMAT (/' Absorption integral mode',I3)
      IF (MODEA .GT. 0 .AND. MODEA .LT. 8) GO TO 29
      CALL ERRMES(1,1,'No integral of this number')
      GO TO 43
C
  29  GO TO (21,22,23,24,25,26,27) , MODEA
C
  21  CALL MESS(LPT,0,'Conventional absorption correction')
      GO TO 43
  22  CALL MESS(LPT,0,'Depolarisation - integral using path in only')
      GO TO 43
  23  CALL MESS(LPT,0,'Extinction - derivative integral')
      GO TO 43
  24  CALL MESS(LPT,0,'Absorption+depolarisation')
      GO TO 43
  25  CALL MESS(LPT,0,'Absorption+extinction')
      GO TO 43
  26  CALL MESS(LPT,0,'Depolarisation+extinction')
      GO TO 43
  27  CALL MESS(LPT,0,'Absorption,depolarisation and extinction')
      GO TO 43
C
C HERE TO CONTINUE READING AFTER <WORD> <NUMBER> FORMAT:
  43  CALL RDWORD(GWD,LEN,IPT,IPT,80,0,IER)
      IF (IER .EQ. 100) GO TO 3
      GO TO 42
   3  CONTINUE
C
      WRITE (LPT,2001) NP
2001  FORMAT (/' Equations of',I3,' faces (with no + signs):')
      DO 7 I=1,NP
      WRITE (LPT,2002) AA(I),BB(I),CC(I),DD(I)
2002  FORMAT (1X,F10.5,'x',F10.5,'y',F10.5,'z >=',F10.5)
   7  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTI
      SUBROUTINE INPUTI
C
C *** INPUTI updated by JCM 14 Jul 86 ***
C
CX
CC 13C
CH Gathers information from a user's interactive instruction card.
CD Absorbs into COMMON /IINFO/ sets of information from all I cards.
CD Sets IIN=number of items found on all cards.
CD
CD To be read by INPUTI, the I card must be of the form:
CD   I WORD1  NUM1  WORD2  NUM2  WORD3  NUM3  etc
CD (so if the user wants some other form he must read the card himself).
CD
CD The "NUMS" may be reals or integers.  They will be read to reals, but may be
CD fixed later according to their "WORDS".
CD
CD It does not matter what the vocabulary of words is.  INPUTI can
CD read any set of instructions in this format, and is expected to be called
CD for least squares application, for ARRNGE and for an increasing number of
CD MAIN programs requiring to be driven this way.
CD
CD An example of an I card for SFLSQ (structure factor least squares) is:
CD      I NCYC 6  CYC1  1     PRIN 2  MCOR  55
CD for which INPUTI simply produces 4 sets of (WORD, REAL) in COMMON, and
CD it is up to its calling routine (in this case STLSSF) to interpret these to
CD mean 6 cycles, first one numbered 1, print structure factor listing at last
CD cycle and correlations of over 55 per cent.
C
CI Reads all I cards.
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IINFO/IIN,ACOEFF(20)
      COMMON /IINFOW/IIREAD(20)
      CHARACTER *4 IIREAD
C
      INREAD(9)=-IABS(INREAD(9))
      NCARDS=ICDNO(9)
C IIN=NUMBER OF ITEMS READ FROM I CARDS:
      IIN=0
      IF (NCARDS .LE. 0) GO TO 100
      ID=IABS(INREAD(9))
      DO 2 I=1,NCARDS
      CALL CARDIN(ID)
      ID=ID+NYZ
      IPT=2
C
C%
C   3  IF (IIN .LE. %ICRD%) GO TO 4
   3  IF (IIN .LE. 20) GO TO 4
C%
C      CALL ERRMES(1,1,'more than %ICRD% items on I cards')
      CALL ERRMES(1,1,'more than 20 items on I cards')
      GO TO 100
C
   4  CALL RDWORD(IIREAD(IIN+1),NTEMP,IPT,IPT,80,0,IER)
      IF (IER .EQ. 100) GO TO 2
      IIN=IIN+1
      CALL RDREAL(ACOEFF(IIN),IPT,IPT,80,IER)
      GO TO 3
   2  CONTINUE
C
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTJ(ID,NTYP,IAT,IPT,IER)
      SUBROUTINE INPUTJ(ID,NTYP,IAT,IPT,IER)
C
C *** INPUTJ corrected by PJB c17 17 Sept 1993 ***
C
CX
CC 18A
CH Reads individual J cards.
CA On entry ID is the position in the CDF of the J card, or 0 (= card present)
CA On exit NTYP = a number indicating what kind of J card was read:
CA      NTYP=1 for MPOL ( l,m and amplitudes of its + and - combinations)
CA      NTYP=2 for FORM names of <jL> form factors with L values
CA On exit IAT = number corresponding to the atom or form factor la bel,
CA               if one was read.
CA On exit IPT = the next column on the card to be interpreted.
CA On exit IER = the error indicator, = 0 for no error.
C
      CHARACTER*4 WORD,WORD1,JWORD(2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA NWRD/2/
      DATA JWORD/'MPOL','FORM'/
C
C
C SET NO ERROR
      IER=0
      ITYP=0
C
C IF ID=0, EXPECT CARD ALREADY PRESENT:
      IF (ID.NE.0) CALL CARDIN(ID)
      IPT1=3
      CALL RDWORD(WORD,LWORD,IPT1,IPT,80,0,IE)
      IF (IE.EQ.0) THEN
        JAT=ISCAT(WORD)
        IAT=IATOM(WORD)
        CALL RDWORD(WORD1,LWORD,IPT,IPT,80,0,IE)
        NTYP=NCFIND(WORD1,JWORD,NWRD)
        IF (NTYP.EQ.0) THEN
          CALL ERRCH2(WORD1,2,'The word "',
     &    '" on a J card is not an allowed J word ')
          GO TO 99
        ENDIF
        IF (IAT.EQ.0.AND.NTYP.EQ.1) THEN
          CALL ERRCH2(WORD,2,'The first word "',
     &    '" on a J MPOL card is not an atom name ')
          GO TO 99
        ENDIF
        IF (JAT.EQ.0.AND.NTYP.EQ.2) THEN
          CALL ERRCH2(WORD,2,'The first word "',
     &    '" on a J FORM card is not a form-factor name ')
          GO TO 99
        ENDIF
        IF (NTYP .EQ. 2) IAT=JAT
        GO TO 100
      ELSE
        CALL ERRCH2(ICARD(IPT1:IPT-1),2,'Illegal word','on J card')
      ENDIF
C
   99 IER=IER+1
C
  100 RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTN(NOUT)
      SUBROUTINE INPUTN(NOUT)
C
C *** INPUTN updated by JCM 6 Apr 89 ***
C
CX
CC 13C
CH Deals with the "N" card giving the Crystal Data File title.
CA On entry NOUT= an output unit number on to which to write the title
CA                or -1 to indicate title already present in /SCRACH
CD Puts title into ITITLE and the number of characters in it to NTITLE
CI The first time in a run INPUTN is called with +ve NOUT, it reads a single
CI card starting N, and takes the next 79 chars as a title to be put out where
CI the user chooses.  In particular it is written on plotted Fourier maps.
C
CO At this first and every other call of INPUTN, it will write out the title
CO on unit NOUT, unless NOUT=-1 when it is written to unit LPT.
C
CN The "N" is not held in ITITLE
C
      CHARACTER *8 NOTTLE
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NTITL/NTITLE,KOM14
      COMMON /TITLE/ITITLE
      CHARACTER *80 ITITLE
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA NOTTLE/'UNTITLED'/
C
C IF NOUT=-1, COPY TITLE FROM SCRACH:
      IF (NOUT .EQ. -1) THEN
        NTITLE=LENGT(ICARD)
        ITITLE(1:NTITLE)=ICARD(1:NTITLE)
        GO TO 101
      ENDIF
C
C IF TITLE BEEN INPUT BY PREVIOUS CALL, JUST COPY OUT:
      IF (INREAD(14) .LT. 0) GO TO 101
C
C SET "N CARD READ ONCE":
      INREAD(14) =  -IABS(INREAD(14))
C
C IF NO "N" CARD GO TO PUT IN "UNTITLED":
      IF (ICDNO(14) .LT. 1) THEN
        ITITLE=NOTTLE
        NTITLE=8
        GO TO 100
      ENDIF
C
C READ N CARD:
      CALL CARDIN(IABS(INREAD(14)))
C NTITLE GIVES NO. OF PRINTING CHARS IN TITLE:
      NTITLE=LENGT(ICARD)-1
      ITITLE(1:NTITLE)=ICARD(2:NTITLE+1)
 101  IF (NOUT .GE. 0)WRITE (NOUT,2000) (ITITLE(I:I),I=1,NTITLE)
      IF (NOUT .LT. 0)WRITE (LPT,2000) (ITITLE(I:I),I=1,NTITLE)
2000  FORMAT(1X,79A1)
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTS(ID,R,T)
      SUBROUTINE INPUTS(ID,R,T)
C
C *** INPUTS improved by JCM 30 Aug 92 ***
C
CX
CC 1A
CH Reads and interprets one "S" card containing a space group operator.
CH Can also interpret an S GRUP card containing a space group specification.
CA On entry ID points to an "S" card in the binary copy of the Crystal Data
CA file, or if 0 indicates that the card is already present in /SCRACH/,
CA       or if -ve indicates a scratch unit on which to find S cards.
CA On exit R is a 3x3 matrix containing the rotation, and T is a 1x3 vector
CA containing the translation.
CD Interprets symmetry operators within the character set:
CD         X Y Z x y z 1 2 3 4 5 6 + - / ,
CD and ignores characters outside that set.
CD
CD Within fixed format:
CD An operator has 3 fields, being in columns 2-21,22-41 and 42-61, and within
CD each field the element of the space group symmetry operator is given.  A
CD fraction is given as digit/digit, and any reasonable looking combination
CD (within the vocabulary above) is interpreted.
CD
CD It is also possible to override the need to use 20 columns per field,
CD by finishing a field with a comma, e.g.
CD    S  -X,-Y,Z
C
CI Reads an "S" card
CN The findings of SYMOP (which repeatedly calls INPUTS) may be printed by
CN a call of OPSYM.
C
      DIMENSION R(3,3),T(3)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
C CLEAR R AND T TO ZERO
      CALL GMZER(R,3,3)
      CALL GMZER(T,1,3)
C SET NO ERRORS & READ IN CARD:
      IER=0
      IF (ID .LT. 0) READ (-ID,1000) ICARD
1000  FORMAT(A80)
      IF (ID .GT. 0) CALL CARDIN(ID)
C
C L COUNTS ALONG CARD, EITHER IN FIELDS OF 20 OR TO NEXT COMMA
      L=1
      DO 1 J = 1,3
      SIGN = 1.
      A = 0.
      B = 0.
      DO 2 K = 1,20
      L = L + 1
      DO 3 M = 1,6
      IF (ICARD(L:L) .NE. IDIGIT(M)) GO TO 3
      A=FLOAT(M)
      GO TO 2
   3  CONTINUE
      IF (ICARD(L:L) .EQ. '/') B = A*SIGN
      DO 6 IXYZ=24,26
      IF ((ICARD(L:L).EQ.LETUP(IXYZ)) .OR. (ICARD(L:L).EQ.LETLOW(IXYZ)))
     &  R(J,IXYZ-23) = SIGN
   6  CONTINUE
      IF (ICARD(L:L) .EQ. '+') SIGN = 1.
      IF (ICARD(L:L) .EQ. '-') SIGN = -1.0
      IF (ICARD(L:L) .EQ. ',') GO TO 4
   2  CONTINUE
   4  IF (A .NE. 0.) T(J) = B/A
C CHECK THAT SOMETHING HAS BEEN PUT INTO THE ROTATION MATRIX ROW:
      IF ((ABS(R(J,1))+ABS(R(J,2))+ABS(R(J,3))) .EQ. 0.) IER=IER+1
   1  CONTINUE
C
C CHECK THAT ROTATION MATRIX HAS NO ZERO COLUMN:
      DO 5 I=1,3
      IF ((ABS(R(1,I))+ABS(R(2,I))+ABS(R(3,I))) .EQ. 0.) IER=IER+1
   5  CONTINUE
C
C IF ERRORS, REPORT & SET IERR
      IF (IER .EQ. 0) GO TO 100
      WRITE (LPT,3000) R,T,ICARD
      WRITE (ITO,3000) R,T,ICARD
3000  FORMAT (/' ERROR ** on S card resulting in matrix',
     & 3(3F5.1/),' and vector',3F5.1,' from card saying'/A80)
      IERR=IERR+1
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTT(ID,LABA,LBALEN,NTYP,A,IER)
      SUBROUTINE INPUTT(ID,LABA,LBALEN,NTYP,A,IER)
C
C *** INPUTT by JCM 8 Feb 84 ***
C
CX
CC 4A
CH Reads and interprets one "T" card.
CA On entry ID points to the required card in the bionary copy of the Crystal
CA data file, or if 0 indicates that the card is already present in /SCRACH/.
CA On exit:
CA    LABA, A4, holds the label read after "T space"
CA    LBALEN is the number of characters in LABA
CA    NTYP is an integer giving the type of factor expected
CA    A is a 1x6 array of coefficients, in the standard sequence:
CA             A11, A22, A33, A23, A13, A12
CA    IER is an error indicator, =0 if no errors.
C
CD The exact interpretation of the 6 coefficients is left to the SUBROUTINE
CD (usually SETANI), which calls INPUTT.
CI Reads a "T" card.
C
      CHARACTER *4 LABA
      DIMENSION A(6)
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IER=0
      IF (ID .NE. 0) CALL CARDIN(ID)
C IPT MOVES ALONG CARD AS ITEMS TAKEN FROM IT:
      CALL RDWORD(LABA,LBALEN,3,IPT,80,0,IE)
      IF (IE .NE. 0  .OR. LETTER(LABA(1:1)) .EQ. 0) THEN
        CALL ERRATM(ICARD(3:6),-2,'"T" card')
      GO TO 101
      ENDIF
C
      IPKEEP=IPT
      CALL RDINTG(NTYP,IPT,IPT,80,IE)
      IF ((IE .NE. 0) .AND. (IE .NE. 100)) THEN
      CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,' ','read for atf type')
      GO TO 101
      ENDIF
      DO 2 I=1,6
      CALL RDREAL(A(I),IPT,IPT,80,IE)
      IF ((IE .NE. 0) .AND. (IE .NE. 100)) THEN
      CALL ERRCH2(ICARD(IPKEEP:IPT-1),-2,' ','read for atf coeff')
      GO TO 101
      ENDIF
   2  CONTINUE
      GO TO 100
C
 101  IER=IER+1
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTU(HT)
      SUBROUTINE INPUTU(HT)
C
C *** INPUTU by JCM 22 Nov 84 ***
C
CX
CC 1A
CH Reads and interprets a "U" card, giving a typical reflection to define
CH the reciprocal asymmetric unit.
CA On exit HT is a 1x3 vector holding the 3 indices.
CD Reads a single "U" card, giving 3 real numbers for h,k,l, the indices
CD of the typical reflection which the user wishes to be inside the chosen
CD asymmetric unit.
CD
CD If no "U" card is given, 13,11,10 is assumed (being all positive, with
CD h > k > l).  If the given indices are special, they will be on the edge
CD of an asymmetric unit, and the chosen unit might not be exactly as
CD wished.
C
CI Reads one "U" card.
CO Writes its findings to unit LPT.
C
      DIMENSION HT(3),K(3)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      INREAD(21)=-IABS(INREAD(21))
      HT(1)=13.
      HT(2)=11.
      HT(3)=10.
C IF NO "U" CARD, USE DEFAULT 13,11,10:
      IF (ICDNO(21) .EQ. 0) GO TO 101
C
C READ U CARD:
      IER1=0
      CALL CARDIN(IABS(INREAD(21)))
C
C SCAN ALONG CARD FINDING 3 NUMBERS:
      L=2
      DO 1 J=1,3
      CALL RDREAL(HT(J),L,L,80,IER)
C IF READ ALL SPACES (UP TO FIXED FORMAT FIELD BARRIER) ASSUME 0:
      IF (IER .EQ. 100) IER=0
C OTHERWISE, IF IER CAME NON-ZERO, WE MET AN UNEXPECTED CHARACTER:
      IF (IER .NE. 0) CALL ERRIN2(J,2,'reading index number',
     &  'on U card')
   1  CONTINUE
C
C CHECK NOT 0,0,0 READ WHICH WOULD BE NO HELP:
      IF (HT(1)*HT(2)*HT(3) .NE. 0.) GO TO 101
      CALL ERRMES(1,-1,'indices 0,0,0 read from "U" card')
      GO TO 100
C
 101  CALL INDFIX(HT,K)
      WRITE (LPT,2000) K
2000  FORMAT (/' Indices',3I4,' to be used for typical reflection ',
     & 'inside asymmetric unit')
 100  RETURN
      END
C
C
C
C
C LEVEL 4       SUBROUTINE INPUTW(IAT,WORD,ID,IPT)
      SUBROUTINE INPUTW(IAT,WORD,ID,IPT)
C
C *** INPUTW updated by JCM 11 Oct 89 ***
C
CX
CC 4A
CH To read the "W" card indicated by ID, as far as atom label and word.
CA IAT on exit is which atom label, or scattering factor label identified
CA WORD is A4, on exit holding which word read after atom label
CA ID on entry points to card to read
CA IPT on exit points to next position on card to read
CD If the WORD is 'RADF', expects the label to be a scattering factor
CD label.
C
      CHARACTER*4 WORD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
C
      CALL CARDIN(ID)
C  IN CASE COMMENT CARDS MIXED IN
      ID=ID+NYZ
      CALL RDWORD(WORD,LWORD,2,IPT1,80,0,IER)
      IF (IER.NE.0) GO TO 101
      IAT=IATOM(WORD)
      JAT=ISCAT(WORD)
      IF (IAT.EQ.0 .AND. JAT .EQ. 0) GO TO 101
      CALL RDWORD(WORD,LWORD,IPT1,IPT,80,0,IER)
      IF (IER .NE. 0) GO TO 101
      IF (WORD .EQ. 'RADF') IAT=JAT
      IF (IAT .NE. 0) GO TO 100
  101 CALL ERRMES(1,2,'looking for word on W card')
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE INTCHR(IDIG,NDIG,ICHR,NCHR,MODE)
      SUBROUTINE INTCHR(IDIG,NDIG,ICHR,NCHR,MODE)
C
C *** INTCHR updated by JCM 12 Nov 89 ***
C
CX
CC 13C
CH Converts digits to characters, either left- or right-justified.
CA On entry IDIG is an integer array holding NDIG digits
CA          NCHR is the number of characters in ICHR
CA          MODE=0 requests left justification
CA               1 requests right justification
CA On exit  ICHR is a character string holding NDIG characters, which
CA               correspond to the digits in IDIG, properly justified.
C
      CHARACTER *(*) ICHR
      DIMENSION IDIG(NDIG)
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (NDIG .LE. NCHR) GO TO 1
      WRITE (LPT,3000) NDIG,NCHR
      WRITE (ITO,3000) NDIG,NCHR
3000  FORMAT (/' ERROR ** in INTCHR -',I4,' digits into',I4,
     & ' chars')
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
   1  NLEN=LEN(ICHR)
      J=0
      IF (MODE .EQ. 1) J=NLEN-NCHR
      ICHR=' '
      DO 2 I=1,NDIG
      J=J+1
      K=IDIG(I)
      IF (K .EQ. 0) K=10
   2  ICHR(J:J)=IDIGIT(K)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE INTDIG(N,IDIG,NDIG)
      SUBROUTINE INTDIG(N,IDIG,NDIG)
C
C *** INTDIG by JCM 8 Jun 82 ***
C
CX
CC 13C
CH Unpacks an integer into its individual digits.
CA On entry N is the integer to be unpacked
CA On exit  IDIG is an integer array holding the individual digits of abs(N)
CA          NDIG is the number of elements of NDIG, maximum 5
C
CO If the integer is too big, says so & exits with nothing stored
C
      DIMENSION IDIG(5),ITENS(4)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      DATA ITENS/10,100,1000,10000/
C
      IN=IABS(N)
      ND=1
      K=4
   3  IF ((IN .LT. ITENS(K)) .AND. (ND .EQ. 1)) GO TO 1
      IDIG(ND) = IN/ITENS(K)
      IF (IDIG(ND) .GE. 10) THEN
        CALL ERRIN2(N,2,'integer','too large for INTDIG')
        GO TO 100
      ENDIF
   2  IN = IN-IDIG(ND)*ITENS(K)
      ND=ND+1
   1  K=K-1
      IF (K .NE. 0) GO TO 3
      IDIG(ND)=IN
      NDIG=ND
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE INVENT(U,H,ANS)
      SUBROUTINE INVENT(U,H,ANS)
C
C *** INVENT by PJB/JCM 8 Aug 83 ***
C
CX
CC 1C
CH Given a plane U and an axis H, produces a direction ANS which is in the
CH plane but not parallel to the axis.
C
      DIMENSION U(3),H(3),ANS(3),VEC(3,2),COMPA(3,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
C SET UP DEFAULT VALUES:
      CALL GMZER(VEC,3,2)
C EXAMINE ELEMENTS OF INCOMING PLANE;  NZ=NO. OF ZEROS+1,
C LZ POINTS TO ZERO IF ONE OF THEM ONLY, LN POINTS TO NON-ZERO
C IF TWO ZEROS:
      NZ=1
      DO 4 I=1,3
      IF (NINT(U(I)) .NE. 0.) GO TO 5
      NZ=NZ+1
      LZ=I
      GO TO 4
   5  LN=I
   4  CONTINUE
C
C SET IH,IK,IL TO POINT CYCLICALLY 1,2,3 AS APPROPRIATE
      IF (NZ .GT. 3) CALL ERRMES(-1,0,'zero vector given to INVENT')
      IH=LN
      IF (NZ .EQ. 2) IH=LZ
      IK=MOD(IH,3)+1
      IL=MOD(IK,3)+1
      GO TO (1,2,3) , NZ
C BRANCH TO SET UP TWO POSSIBLE VECTORS FOR ANS IN VEC
C
C NO ZEROS AT ALL:
   1  VEC(IH,1)=-U(IK)
      VEC(IK,1)= U(IH)
      VEC(IK,2)=-U(IL)
      VEC(IL,2)= U(IK)
      GO TO 7
C
C ONE ZERO PRECISELY:
   2  VEC(IH,1)=1.
      VEC(IK,2)=-U(IL)
      VEC(IL,2)= U(IK)
      GO TO 7
C
C TWO ZEROS PRECISELY:
   3  VEC(IK,1)=1.
      VEC(IL,2)=1.
C
C JOIN HERE TO PICK GOOD ONE:
   7  CALL GMEQ(H,COMPA(1,1),1,3)
      CALL GMREV(H,COMPA(1,2),1,3)
      DO 6 N=1,2
      CALL GMEQ(VEC(1,N),ANS,1,3)
C PUT POTENTIAL ANSWER IN
      CALL INBOX(ANS,IN)
      IF (IN .LT. 0) CALL GMREV(ANS,ANS,1,3)
C IF VECTOR OUTSIDE ASYMMETRIC UNIT, REVERSE IT
      CALL FCTOR(ANS,NFAC)
      CALL EQVEC(COMPA,ANS,2,M,0)
      IF (M .EQ. 3) GO TO 100
C IF RESULT IS NOT COINCIDENT WITH GIVEN H, OK - EXIT
   6  CONTINUE
C
C IF HERE, BOTH POTENTIAL ANSWERS HAVE PROVED PARALLEL TO H
      WRITE (LPT,3001) U,H,VEC
      WRITE (ITO,3001) U,H,VEC
3001  FORMAT (/' ERROR ** IN INVENT - PLANE',3F10.2,' AND ',
     & 'EXCLUDED AXIS',3F10.4,' HAVE GIVEN VECTORS:'/2(3F10.4/))
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
 100  RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION IPOPE(N)
      FUNCTION IPOPE(N)
C
C
C *** IPOPE rewritten by PJB July 1993 ***
C
CX
CC 13C
CH Machine specific routine to interpret error codes during file opening.
CA On entry N=an error code produced as a result of trying to open a file.
CA On exit IPOPE is set as follows:
CA    1 for an 'old' file which does not exist
CA    2 for a 'new' file which does exist
CA    3 for a bad file name
CA    4 file already open
CA    0 for anything else
C
      DIMENSION IOS(4),NSWI(4)
CUNIX
      DATA NERR,NSWI,IOS/3, 1,2,3,3, 2,126,107,145/
CVMSO      DATA NERR,NSWI,IOS/3, 1,2,3,0, 29,43,30,0/

      I=NFIND(N,IOS,NERR)
      IF (I.NE.0) I=NSWI(I)
C     Temp hack by ken for Unix
      IF (N.EQ.126 .OR. N.EQ.128 .OR. N.EQ.10) I=2
C     126 for Irix, 128 for Linux, 10 for Digital Unix 
      IPOPE=I
      RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION ISCAT(FNAME)
      FUNCTION ISCAT(FNAME)
C
C *** ISCAT updated by JCM 23 Sep 86 ***
C
CX
CC 11C
CH Searches for a potential scattering factor name in the table.
CA On entry FNAME is an A4 CHARACTER name, which could be a scattering factor
CA                name, left justified
CA On exit  ISCAT =0 if FNAME does not occur in the list in /FONAME
CA                =position in the list if found.
CP The scattering factor list must have been set up by, say, ATOPOS
C
      CHARACTER *4 FNAME
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
C
      IF (NUMFNM .EQ. 0) THEN
        ISCAT=0
      ELSE
        ISCAT=NCFIND(FNAME,FONAME,NUMFNM)
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 3      LOGICAL FUNCTION ISPABS(H)
      LOGICAL FUNCTION ISPABS(H)
C
C *** ISPABS corrected by WIFD Jan 87 ***
C
CX
CC 1B
CH Checks space group absences.
CA H on entry is a 3-sized array containing h,k,l for a reflection
CA ISPABS on exit is .TRUE. if the hkl reflection is absent, .FALSE. if present
CP The symmetry must have been previously set up by SYMOP
C
      DIMENSION H(3),EH(3)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C IF P1 OR P BAR1, NO ABSENCES:
      IF (NOPC .EQ. 1) GO TO 101
      ISPABS=.TRUE.
      DO 1 IC=1,NCENT
      DO 1 I=1,NOPC
      CALL ROTSYM(H,EH,I,-1)
      IF (IC .EQ. 2) CALL GMREV(EH,EH,1,3)
      DO 2 J=1,3
      IF (ABS(H(J)-EH(J)) .GT. 10.E-4) GO TO 1
   2  CONTINUE
C
C REFLECTION TRANSFORMS INTO ITSELF - CHECK TRANSLATION
      A=SCALPR(TRANS(1,I),H)
      IF (ABS(AMOD((ABS(A)+0.01),1.)) .GT. 0.1) GO TO 100
   1  CONTINUE
C
C PRESENT:
 101  ISPABS=.FALSE.
 100  RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION ITPOS(IPOS)
      FUNCTION ITPOS(IPOS)
C
C *** ITPOS 24 Nov 82 by JCM ***
C
CX
CC 15C
CH Sets ITPOS=a single bit (a one), in position IPOS, counting from the right.
CA IPOS is assumed non-zero on entry, and is unchanged.
CN It is also assumed that IPOS is not so large that integer
CN overflow will occur.  If it does, try reducing the number of
CN bits used in an integer (e.g.avoid the last, sign, bit) by reducing
CN the value of NBITS in COMMON /CONTUR/
C
      ITPOS=1
      IF (IPOS .EQ. 1) GO TO 100
      DO 1 I=2,IPOS
   1  ITPOS = 2*ITPOS
 100  RETURN
      END
C
C
C     All calls to function JFIX now replaced by calls to NINT
C     because it performs the same function and is consistent
C     across compilers.  Note that NINT meant something different
C     to Digital Fortran V6.0 on Windows NT.  Hence why the calls
C     had to be removed
C
C
C
C LEVEL 1      SUBROUTINE JMPOL
      SUBROUTINE JMPOL
C
C *** JMPOL by JCM 11 Feb 91 ***
C
CX
CC 7B
CH Writes to unit NEWIN all J MPOL cards after a multipole refinement.
C
      DIMENSION N2(4)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
C
C
      DO 1 MP=1,NMPAT
      N1=MPATAB(MP)
      N3=0
      DO 2 J=MPTAB(MP),MPTAB(MP+1)-1
      N3=N3+1
      IF (N3 .GT. 4) THEN
        WRITE (NEWIN,2000) ATNAME(N1),
     &  (MPNAM(N2(I)),POLAMP(N2(I),1),I=1,4)
2000    FORMAT ('J ',A4,' MPOL',4(1X,A4,F10.4))
        N3=1
      ENDIF
      N2(N3)=J
   2  CONTINUE
      IF (N3 .GT. 0) WRITE (NEWIN,2000) ATNAME(N1),
     & (MPNAM(N2(I)),POLAMP(N2(I),1),I=1,N3)
   1  CONTINUE
C
      RETURN
      END
C
C
C
C
C LEVEL 4      FUNCTION KPAK(IFAM,IGEN,ISPC,KP,KS)
      FUNCTION KPAK(IFAM,IGEN,ISPC,KP,KS)
C
C *** KPAK for MK4 by JCM 7 Nov 90 ***
C
CX
CC 6C
CH Pack a LSQ parameter specification on to integer.
CA On entry IFAM = family number
CA          IGEN = genus number
CA          ISPC = species number
CA          KP = Phase number (1 if single phase, but packed as 0)
CA          KS = Source number (1 if single source, but packed as 0)
CA On exit KPAK contains IFAM, IGEN, ISPC, (and KP and KS if multi)
CA         packed according to LSQ conventions
CP LSETUP must have set up the packing scheme
CN there is an inverse routine KUNPAK
C
      DIMENSION LPAK(5)
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /LSQPAK/KKPACK(10,3)
C
      N=3
      LPAK(1)=IFAM
      LPAK(2)=IGEN
      LPAK(3)=ISPC
      IF (MULONE) THEN
        LPAK(4)=KP
        LPAK(5)=KS
        N=5
      ENDIF
      CALL NPACK(KPAK,LPAK,N,1,KKPACK)
      RETURN
      END
C
C
C
C
C LEVEL 6      LOGICAL FUNCTION KSAME(KK1,KK2)
      LOGICAL FUNCTION KSAME(KK1,KK2)
C
C *** KSAME updated for MK4 by JCM 10 Feb 90
C
CX
CC 6C
CH Tells if two LSQ parameter specifications are the same, allowing wild
CH card elements.
CA On entry KK1 is a parameter spec, possibly incomplete
CA          KK2 is a parameter spec, possibly incomplete
CA On exit KSAME is .TRUE. if all the unpacked elements in the two
CA    specifications are the same, with 0 being the same as anything
C
       LOGICAL KWHOLE,KW1,KW2
       DIMENSION K1(5),K2(5)
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
C
      IF (KK1 .EQ. KK2) GO TO 101
      KSAME=.FALSE.
      KW1=KWHOLE(KK1,K1)
      KW2=KWHOLE(KK2,K2)
      IF (KW1 .AND. KW2) GO TO 100
C ONE OF THEM AT LEAST HAS WILD CARDS:
      N=3
      IF (MULONE) N=5
      DO 3 I=1,N
      IF (K1(I) .EQ. 0) GO TO 3
      IF (K2(I) .EQ. 0) GO TO 3
      IF (K1(I) .EQ. K2(I)) GO TO 3
      GO TO 100
   3  CONTINUE
 101  KSAME=.TRUE.
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE KUNPAK(KK,IFAM,IGEN,ISPC,KP,KS)
      SUBROUTINE KUNPAK(KK,IFAM,IGEN,ISPC,KP,KS)
C
C *** KUNPAK by JCM 8 Nov 90 ***
C
CX
CC 6C
CH Unpacks a LSQ parameter specification from single integer.
CA On entry KK holds packed parameter specification
CA On exit IFAM holds family number
CA         IGEN holds genus number
CA         ISPC holds species number
CA         KP holds phase (unless single phase, when 1)
CA         KS holds source(unless single source, when 1)
CP KK  must have been made via a call of KPAK set up by LSETUP
CD Unpacks KK according to bases previously set
CN There is an inverse routine KPAK, and a routine PUNPAK which takes no
CN account of phase and source.
C
      DIMENSION LPAK(5)
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /LSQPAK/KKPACK(10,3)
C
      N=3
      IF (MULONE) N=5
      CALL NPACK(KK,LPAK,N,2,KKPACK)
      IFAM=LPAK(1)
      IGEN=LPAK(2)
      ISPC=LPAK(3)
      KP=1
      KS=1
      IF (MULFAS) KP=LPAK(4)
      IF (MULSOU) KS=LPAK(5)
      RETURN
      END
C
C
C
C
C LEVEL 5      LOGICAL FUNCTION KWHOLE(KK,K)
      LOGICAL FUNCTION KWHOLE(KK,K)
C
C *** KWHOLE updated by JCM 4 Dec 91 ***
C
CX
CC 6C
CH Says if KK is a whole packed parameter specification, or whether there
CH are wild card elements.
CA On entry KK is a parameter specification, possibly with zeros
CA On exit array K has the unpacked elements of KK
C
CD A complete KK will contain phase and source information.  As these will
CD often be zero (in the cases "not multiphase"/"not multisource") they are
CD treated differently from the other elements.
CD If "not multiphase" then no account is taken of the phase element
CD If "not multisource" then no account is taken of the source element
CD Otherwise, on exit KWHOLE is .TRUE. if all unpacked elements are non-zero
CD and otherwise .FALSE.
C
      DIMENSION K(5)
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
C
C UNPACK:
* USED TO UNPACK PHASE & SOURCE INTO KP & KS - NOW DOES NOT - MAY DISCOVER WHY.
      CALL KUNPAK(KK,K(1),K(2),K(3),K(4),K(5))
      KWHOLE=.TRUE.
      DO 1 I=1,3
      IF (K(I) .EQ. 0) KWHOLE=.FALSE.
   1  CONTINUE
      IF (MULFAS .AND. K(4) .EQ. 0) KWHOLE=.FALSE.
      IF (MULSOU .AND. K(5) .EQ. 0) KWHOLE=.FALSE.
      RETURN
      END
C
C
C
C
C LEVEL 3      LOGICAL FUNCTION LATABS(H)
      LOGICAL FUNCTION LATABS(H)
C
C *** LATABS updated by PJB Sep 87 ***
C
CX
CC 1B
CH Checks h,k,l for being a (nuclear) lattice absence.
CA On entry H is 1x3 real array of h,k,l
CA On exit  LATABS = .TRUE. if h,k,l absent, .FALSE. if present
CP SYMOP should have set up the lattice information in /SYMDA, /NSYM
C
CN Deals with non-integral h,k,l also, giving the answer "absent"
C
      DIMENSION H(3)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C MAKE SURE INDICES ARE INTEGERS
      LATABS=.TRUE.
      DO 3 I=1,3
      IF (ABS(FLOAT(NINT(H(I)))-H(I)).GT..0001) GO TO 100
    3 CONTINUE
      IF (NLAT .EQ. 1) GO TO 2
C
C  SPECIAL ABSENCES FOR NON-PRIMITIVE LATTICES
      DO 1 I=2,NLAT
      A=SCALPR(H,ALAT(1,I))
      IF (ABS(FLOAT(NINT(A))-A) .GT. 0.0001) GO TO 100
   1  CONTINUE
   2  LATABS=.FALSE.
 100  RETURN
      END
C
C

C
C
C LEVEL 1      LOGICAL FUNCTION LATVEC(X)
      LOGICAL FUNCTION LATVEC(X)
C
C *** LATVEC by PJB 5 Nov 84 ***
C
CX
CC 1B
CH Tests for the presence of a lattice vector.
CA On entry X is a real 1x3 vector
CA On exit  LATVEC is. TRUE. if X is a lattice vector, .FALSE. otherwise
CP SYMOP must have set up the lattice information in /SYMDA, /NSYM
C
      DIMENSION X(3)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
      DO 1 I=1,NLAT
      DO 2 J=1,3
      A = AMOD(ABS(ALAT(J,I)-X(J)),1.)
      IF ((A .GT. .0001) .AND. (A .LT. .9999))GO TO 1
    2 CONTINUE
      LATVEC=.TRUE.
C MATCH FOUND - JUMP:
      GO TO 100
    1 CONTINUE
C
      LATVEC=.FALSE.
  100 RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION LENG(NTEXT,L)
      FUNCTION LENG(NTEXT,L)
C
C *** LENG by PJB 13 Apr 85 ***
C
CX
CC 13C
CH Determines the length of a text string, omitting trailing spaces.
CA On entry NTEXT is an A1 character array of dimension L
CA On exit  LEN is the number of visible characters in NTEXT
C
      CHARACTER*1 NTEXT
      DIMENSION NTEXT(L)
C
      I=L+1
   1  I=I-1
      IF (I .LE. 0) GO TO 101
      IF (NTEXT(I) .EQ. ' ') GO TO 1
  101 LENG=I
  100 RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION LENGT(CHAR)
      FUNCTION LENGT(CHAR)
C
C *** LENGT by JCM 13 Nov 87 ***
C
CX
CC 13C
CH Determines the length of a character variable, omitting the final spaces.
CA On entry CHAR is a character variable
CA On exit  LENGT is the number of visible characters in CHAR
C
      CHARACTER*(*) CHAR
C
      I=LEN(CHAR)+1
   1  I=I-1
      IF (I .LE. 0) GO TO 101
      IF (CHAR(I:I) .EQ. ' ') GO TO 1
  101 LENGT=I
  100 RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION LETTER(I)
      FUNCTION LETTER(I)
C
C *** LETTER by JCM 7 Oct 83 ***
C
CX
CC 13C
CH Determines whether a character is a letter.
CA On entry I is an A1 character
CA On exit  LETTER=0 if I is not a letter, otherwise which letter in range 1-26
CN Small letters and capitals treated alike.
C
      CHARACTER *1 I
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
C
      DO 1 J=1,26
      IF (I .EQ. LETUP(J)) GO TO 2
      IF (I .EQ. LETLOW(J)) GO TO 2
   1  CONTINUE
      J=0
   2  LETTER=J
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE LFCALC(H)
      SUBROUTINE LFCALC(H)
C
C *** LFCALC updated by JCM 22 Sep 87 ***
C
CX
CC 6B
CH Calculates a nuclear structure factor and its derivatives.
CA On entry H is a 1x3 array containing h,k,l
CP RECIP, SYMOP, SETANI, and SETFOR must have been obeyed to set up
CP the structure factor calculations.  (These are contained in SETFC).
CP The LSQ environment must have been set up by a suitable MAIN program (like
CP SFLSQ) which has called LSETUP and VARMAK.
CP
CD On exit in /FCAL/
CD    FC is the complex structure factor
CD    FCMOD is its modulus
CD    COSAL is the cosine of its phase
CD    SINAL is the sine of its phase
CD    FCDERS is an array of derivatives of FCMOD wrt all family 2 (structure
CD           parameters.  These are NOT multiplied or divided by anything
CD           else;  compare LMCALC
CD The above will all be zero if h,k,l gives a lattice absence;  note that
CD such would not be true of FCALC, the similar routine which does not cater
CD for LSQ
C
      COMPLEX SUM1,TERM,FORM,HR,FORMFA
      LOGICAL TESTOV,LATABS
      DIMENSION RH(3),H(3)
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FCAL/FC,FCMOD,COSAL,SINAL,FCDERS(300),DERIVT(300)
      COMPLEX FC,DERIVT
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C CLEAR ANSWERS IN CASE ABSENT:
C
C  FC COLLECTS THE CONVENTIONAL STRUCTURE FACTOR, COMPLEX:
      FC=CMPLX(0.,0.)
C CLEAR MODULUS AND ANGLES:
      FCMOD=0.
      COSAL=0.
      SINAL=0.
C CLEAR DERIVATIVES:
      L2=NVARF(2,JPHASE,1)
      IF (L2 .GT. 0) CALL GMZER(FCDERS,1,L2)
C
C OUT IF ABSENT:
      IF (LATABS(H)) GO TO 100
C
C SET FIRST SCATTERING FACTOR:
      IFF=0
C
C GET OFFSET TO REACH THESE VARIABLES:
      LO=LVFST1(2,JPHASE,1)
C
C CLEAR DERIVATIVE VECTOR:
      IF (L2 .GT. 0) CALL CGMZER(DERIVT,1,L2)
C
C CYCLE OVER INDEPENDENT ATOMS:
      DO 1 IR=1,NATOM
      SUM1=CMPLX(0.,0.)
      IF (NFORMF(IR) .NE. IFF) THEN
        IFF=NFORMF(IR)
        FORM=FORMFA(STHL,IFF)
      ENDIF
C
C INNER LOOP OVER SYMMETRY EQUIVALENTS:
      DO 3 IS=1,NOPC
      CALL ROTSYM(H,RH,IS,-1)
      F1=TWOPI*(SCALPR(X(1,IR),RH)+SCALPR(TRANS(1,IS),H))
      ERS=ANITF(RH,IR)
C ANISOTROPIC T F (=1. IF NOT THERE) NEEDED SEPARATELY FOR LSQ:
      ARS=COS(F1)*ERS
      BRS=SIN(F1)*ERS
      TERM=CMPLX(ARS,BRS)
      SUM1=SUM1+TERM
C
C  NOW WE DO THE INNERMOST SUMS FOR THE DERIVATIVES OF MODFC WRT
C VARIABLES XYZ AND ALL BIJ.  WE USE THE
C COMPLEX VECTOR DERIVT FOR THE DERIVATIVES OF THE REAL & IMAGINARY PARTS
C OF THE COMPLEX FC WRT EACH VARIABLE IN TURN.
C
      DO 7 I=1,3
      L=KX(I,IR)
      IF (L .NE. 0) DERIVT(L-LO)=RH(I)*CMPLX(-BRS,ARS) + DERIVT(L-LO)
   7  CONTINUE
C
C IF ANY ATF (NOT A LOOP, FOR SPEED):
      IA=IAPT(IR)
      IF (IA .NE. 0) THEN
        L=KATF(1,IA)
        IF (L.NE.0) DERIVT(L-LO)=RH(1)*RH(1)*TERM + DERIVT(L-LO)
        L=KATF(2,IA)
        IF (L.NE.0) DERIVT(L-LO)=RH(2)*RH(2)*TERM + DERIVT(L-LO)
        L=KATF(3,IA)
        IF (L.NE.0) DERIVT(L-LO)=RH(3)*RH(3)*TERM + DERIVT(L-LO)
        L=KATF(4,IA)
        IF (L.NE.0) DERIVT(L-LO)=RH(3)*RH(2)*TERM + DERIVT(L-LO)
        L=KATF(5,IA)
        IF (L.NE.0) DERIVT(L-LO)=RH(1)*RH(3)*TERM + DERIVT(L-LO)
        L=KATF(6,IA)
        IF (L.NE.0) DERIVT(L-LO)=RH(1)*RH(2)*TERM + DERIVT(L-LO)
      ENDIF
   3  CONTINUE
C END OF INNERMOST CYCLE OVER SYMMETRY
C
C IF CENTROSYMMETRIC, COMPENSATE FOR USING ONLY HALF NUMBER OF OPERATORS:
      IF (CENTRC) SUM1=SUM1+CONJG(SUM1)
C
      FAC=AMULT(IR)*EXP(-TF(IR)*SSQRD)
      HR=FAC*FORM*SITE(IR)
C HR IS PRODUCT OF ATOM DEPENDENT BUT SYMMETRY INDEPENDENT FACTORS
      FC=FC + HR*SUM1
C
C  NOW WE TIDY UP THE XYZ AND BIJ DERIVATIVES, ALLOWING FOR CENTRE:
      DO 8 I=1,3
      L=KX(I,IR)
      IF (L.GT.0) THEN
        DERIVT(L-LO)=TWOPI*HR*DERIVT(L-LO)
        IF (CENTRC) DERIVT(L-LO)=DERIVT(L-LO)+CONJG(DERIVT(L-LO))
      ENDIF
      IF (IA .NE. 0) THEN
        L=KATF(I,IA)
        IF (L.GT.0) THEN
          DERIVT(L-LO)=-HR*DERIVT(L-LO)
          IF (CENTRC) DERIVT(L-LO)=DERIVT(L-LO)+CONJG(DERIVT(L-LO))
        ENDIF
        L=KATF(I+3,IA)
        IF (L.GT.0) THEN
          DERIVT(L-LO)=-HR*2.*DERIVT(L-LO)
          IF (CENTRC) DERIVT(L-LO)=DERIVT(L-LO)+CONJG(DERIVT(L-LO))
        ENDIF
      ENDIF
   8  CONTINUE
C FORM FACTOR:
      L=KCMULT(IR)
      IF (L.GT.0) DERIVT(L-LO)=SITE(IR)*FAC*SUM1
C SITE OCCUPATION:
      L=KSITE(IR)
      IF (L.GT.0) DERIVT(L-LO)=FORM*FAC*SUM1
C ISOTROPIC TEMPERATURE FACTOR:
      L=KTF(IR)
      IF (L.GT.0) DERIVT(L-LO)=-SSQRD*HR*SUM1
   1  CONTINUE
C END OF CYCLE OVER ATOMIC POSITIONS
C
C  TIDY FCALC AND COLLECT TRUE D(MODFC)/D(VARIABLE)
C
      A = REAL(FC)
      B = AIMAG(FC)
      FCMOD = SQRT(A*A+B*B)
      IF (.NOT. TESTOV(A,FCMOD)) THEN
        COSAL=A/FCMOD
        SINAL=B/FCMOD
      ENDIF
C
      DO 6 I=1,L2
C NO SUMMING - THESE ARE THE ACTUAL DERIVATIVES NOT DIVIDED BY ANYTHING, AS
C THEY ARE SUBJECT TO THE CHAIN RULE NEXT, NOT LOGARITHMIC DIFFERENTIATION:
      FCDERS(I)=REAL(DERIVT(I))*COSAL + AIMAG(DERIVT(I))*SINAL
   6  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE LLSCAL(N)
      SUBROUTINE LLSCAL(N)
C
C *** LLSCAL updated by JCM 10 Feb 87 ***
C
CX
CC 6A
CH Multiple entry routine which deals with scale factors in Least Squares.
C
CA On entry, N indicates the required action:
CA N=0 deals with the introduction of a single scale factor with the value 1,
CA     to be refined, when no L SCAL cards are given.
CA N=1 reads in an L SCAL card and records the scale factors from it.
CA N=2 is not assigned.
CA N=3 applies a shift to a specific SCALE(ISPC), with ISPC in /NEWOLD/.
CA N=4 writes out a new L SCAL card to unit NEWIN.
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LREAD/ILREA(22,5),KOM18
      DIMENSION ILREAD(22)
      EQUIVALENCE (ILREAD(1),ILREA(1,1))
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCLDAT/ISCALE,NSCALE,SCALE(20),KSCALE(20),
     & NSCL,LSCD(10)
C
      IF (N) 11,10,12
C
C N=0 - DEAL WITH NO SCAL CARDS:
  10  NSCALE=1
      SCALE(1)=1.0
      CALL MESS(LPT,1,
     & 'No L SCAL cards given - one scale factor of 1. assumed')
      GO TO 100
C
C N=-4 - IF A SCALE FACTOR WAS INRODUCED, MAKE NEW SCAL CARD:
  11  IF (ILREAD(2) .NE. 0) GO TO 100
C THIS IS NEVER CALLED, I THINK, BUT POSSIBLY SHOULD BE
      WRITE (NEWIN,2001) SCALE(1)
2001  FORMAT ('L SCAL',F10.5)
      GO TO 100
C
  12  GO TO (1,100,3,4) , N
C
   1  IF (ILREAD(2) .EQ. 1) NSCALE=0
      IF (ILREAD(2) .EQ. 1) NSCL=0
      NSCL=NSCL+1
      ISKEEP=NSCALE+1
      LSCD(NSCL)=ISKEEP
C%
C      CALL RDNUMS(SCALE(ISKEEP),7,%SCAL%,NUM,IER)
      CALL RDNUMS(SCALE(ISKEEP),7,20,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      NSCALE=NSCALE+NUM
      IER=IERR
C%
C      CALL ERRCHK(1,NSCALE,%SCAL%,1,'scale factors')
      CALL ERRCHK(1,NSCALE,20,1,'scale factors')
      IF (IER .NE. IERR) GO TO 100
C
      LSCD(NSCL+1)=NSCALE+1
      CALL MESS(LPT,1,'Scale factor(s) :')
      CALL PRILIS(SCALE,ISKEEP,NSCALE)
      GO TO 100
C
   3  CALL ADJUST(SCALE(ISPC))
      GO TO 100
C
C NEW L SCAL CARD:
C WE NEED TO KNOW WHICH FACTORS WERE INVOLVED ON THIS CARD.  IT IS PROBABLY
C SENSIBLE TO HANG ON TO THIS INFORMATION AT THE POINT AT WHICH WE HAVE IT,
C THAT IS, WHEN THE CARD IS FIRST INPUT.  ARRAY LSCD(I) HOLDS SERIAL NUMBER
C OF FIRST FACTOR ON I'TH CARD;  LSCD(NSCL+1)=1 OFF END;  THERE ARE NSCL
C L SCAL CARDS.
C
C WHEN A CARD HAS BEEN OUTPUT, THIS IS INDICATED BY TURNING LSCD() NEGATIVE.
C
   4  DO 19 I=1,NSCL
      IF (LSCD(I) .GT. 0) GO TO 18
  19  CONTINUE
      WRITE (LPT,3001) LSCD,NSCL
      WRITE (ITO,3001) LSCD,NSCL
3001  FORMAT (/' *** PROGRAM ERROR IN LLSCAL - TRYING TO WRITE MORE',
     & ' CARDS THAN THERE WERE - NSCL, LSCD=',I4,10I3)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
  18  N1=LSCD(I)
      N2=LSCD(I+1)-1
      LSCD(I)=-LSCD(I)
      WRITE (NEWIN,2005) (SCALE(J),J=N1,N2)
2005  FORMAT ('L SCAL',8F10.5)
      GO TO 100
C
      ENTRY LSCAL8(NP,NV)
      KSCALE(NP)=NV
      GO TO 100
C
      ENTRY LSCAL9
      DO 9 I=1,NSCALE
   9  KSCALE(I)=0
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE LLTFAC(N)
      SUBROUTINE LLTFAC(N)
C
C *** LLTFAC updated JCM 13 Jan 88 ***
C
CX
CC 6A
CH Multiple entry routine which deals with overall isotropic temperature
CH factors in Least Squares.
C
CA On entry N indicates the action required:
CA N=1 reads in L TFAC card and stores value of overall isotropic
CA     temperature factor in TFAC.
CA N=2 is not assigned.
CA N=3 applies a shift to TFAC.
CA N=4 writes out a new L TFAC card.
CA N=5 deals with the default if no L TFAC card is given.
CA N=6 fixes a zero TFAC which is implied by the absence of an L TFAC card.
CA ENTRY LTFAC8(NV) sets KTFAC to indicate TFAC is variable number NV
CA ENTRY LTFAC9 sets KTFAC to indicate TFAC fixed
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /OVER/ITFAC,OTFAC(10),KOTFAC(10),NTFAC,JTFAC,KOM15
      EQUIVALENCE (TFAC,OTFAC(1)),(KTFAC,KOTFAC(1))
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
C
      GO TO (1,100,3,4,5,6) , N
C
C INTERPRET L TFAC CARD ALREADY IN ICARD:
   1  CALL RDREAL(TFAC,7,IPT,80,IER)
      IF (IER .NE. 0) IERR=IERR+1
      NTFAC=1
      WRITE (LPT,2000) TFAC
2000  FORMAT (/' Overall isotropic temperature factor =',F10.4)
      GO TO 100
C
C APPLY SHIFT:
   3  CALL ADJUST(TFAC)
      GO TO 100
C
C NEW L TFAC CARD:
   4  WRITE (NEWIN,2001) TFAC
2001  FORMAT ('L TFAC',F10.4)
      GO TO 100
C
C NO L TFAC CARD:
   5  TFAC=0.
      CALL MESS(LPT,1,
     & 'No L TFAC card read - assuming TFAC=0. and fixed')
      NTFAC=0
      GO TO 100
C
C FIX TFAC IF NONE GIVEN:
   6  IF (NTFAC .EQ. 0) CALL ADDFX5(IFAM,IGEN,ISPC,JPHASE,1,4)
      GO TO 100
C
      ENTRY LTFAC8(NV)
C
C SET TFAC VARIED:
      KTFAC=NV
      GO TO 100
C
      ENTRY LTFAC9
C
C SET TFAC FIXED:
      KTFAC=0
 100  RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION LMATCH(LABEL,NAMTAB,NUM,NBOUND)
      FUNCTION LMATCH(LABEL,NAMTAB,NUM,NBOUND)
C
C *** LMATCH updated by JCM 23 Sep 86 ***
C
CX
CC 11C
CH Matches an A4 item in given table, adding it if it is not there already.
C
CA On entry LABEL is the A4 item to be matched
CA          NAMTAB is the A4 array in which to search (and add)
CA          NUM is the number of entries so far (0 is allowed)
CA          NBOUND is the dimension of NAMTAB
CA On exit LMATCH is set to the address of LABEL in NAMTAB
CA          NUM is increased by 1 if an entry is added.
CO Writes an error message if the table becomes overfull.
C
      CHARACTER *4 LABEL,NAMTAB
      DIMENSION NAMTAB(NBOUND)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
C IF NO ENTRIES, NO MATCH:
      IF (NUM .EQ. 0) GO TO 2
C
      L=NCFIND(LABEL,NAMTAB,NUM)
      IF (L .GT. 0) GO TO 101
C NO MATCH - ADD NEW NAME TO TABLE:
   2  IF (NUM .LT. NBOUND) GO TO 3
      WRITE (LPT,3000) NBOUND,LABEL
      WRITE (ITO,3000) NBOUND,LABEL
3000  FORMAT (/' ERROR ** IN LMATCH - TABLE WITH',I5,' ENTRIES',
     & ' FULL - TRYING TO ADD ',A4)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN      
C
   3  NUM=NUM+1
      L=NUM
      NAMTAB(NUM)=LABEL
 101  LMATCH=L
      RETURN
      END
C
C

C
C
C
C LEVEL 1      FUNCTION LOGAND(I,J)
      FUNCTION LOGAND(I,J)
C
C *** LOGAND IN FORTRAN by JCM 10 Oct 83 ***
C
CC 15C
CH Performs logical "and" on 2 whole integers.
CA On entry I and J are the integers whose "and" is wanted.
CA On exit LOGAND is I and J, taken bit by bit.
CD Takes the logical "and" of all of the bits in I and J, except the sign
CD digit, relying on the value of NBITS in /LENINT/ being set to one fewer
CD than the number of bits in an integer (at most).  NBITS is set in INITIL
CD as a machine specific quantity.
CN This is now part of standard FORTRAN
C
      LOGICAL DIFFI,DIFFJ
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS
C
      IF ((I .LT. 0) .OR. (J .LT. 0)) THEN
        WRITE (LPT,3000) I,J,NBITS
        WRITE (ITO,3000) I,J,NBITS
3000    FORMAT (' ERROR ** in FUNCTION LOGAND - attempt to use',
     &   ' sign bit - check value of NBITS in COMMON /LENINT/ - ',
     &   'I,J,NBITS =',3I5)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
      ENDIF
C
C FORM ANSWER IN IANS.  PUT SAMPLE BIT INTO IBT:
      IANS=0
      IBT=1
      II=I
      JJ=J
C
C COUNT OVER POSSIBLE BITS:
      DO 1 IB=1,NBITS
      DIFFI= .FALSE.
      I1=II/2
      I2=2*I1
C IF HALVING THEN DOUBLING HAS PRODUCED NO CHANGE, II WAS EVEN:
      IF (I2-II) 2,3,4
C JUST IN CASE SOME MACHINE ROUNDS IT UP:
   4  I1=I1-1
   2  DIFFI= .TRUE.
C
C SAME FOR JJ:
   3  DIFFJ= .FALSE.
      J1=JJ/2
      J2=2*J1
      IF (J2-JJ) 5,6,7
   7  J1=J1-1
   5  DIFFJ= .TRUE.
C
C IF INDIVIDUAL BITS BEING EXAMINED ARE THE SAME, ADD IN BIT TO ANSWER
   6  IF (DIFFI .AND. DIFFJ) IANS=IANS+IBT
      IF (IB .NE. NBITS) IBT=IBT*2
C WE OMIT DOUBLING LAST TIME ROUND IN CASE OF OVERFLOW
      II=I1
      JJ=J1
   1  CONTINUE
      LOGAND=IANS
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION LOGOR(I,J)
      FUNCTION LOGOR(I,J)
C
C *** LOGOR IN FORTRAN by JCM 10 Oct 83 ***
C
CC 15C
CH Performs logical "or" on 2 whole integers.
CA On entry I and J are the integers whose "or" is wanted.
CA On exit LOGAND is I or J, taken bit by bit.
CD Takes the logical "or" of all of the bits in I and J, except the sign
CD digit, relying on the value of NBITS in /LENINT/ being set to one fewer
CD than the number of bits in an integer (at most).  NBITS is set in INITIL
CD as a machine specific quantity.
CN This is now part of standard FORTRAN
C
      LOGICAL DIFFI,DIFFJ
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS
C
      IF ((I .LT. 0) .OR. (J .LT. 0)) THEN
        WRITE (LPT,3000) I,J,NBITS
        WRITE (ITO,3000) I,J,NBITS
3000    FORMAT (' ERROR ** in FUNCTION LOGOR - attempt to use',
     &   ' sign bit - check value of NBITS in COMMON /LENINT/ - ',
     &   'I,J,NBITS =',3I5)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
        CALL BMBOUT
        RETURN
      ENDIF
C
C FORM ANSWER IN IANS.  PUT SAMPLE BIT INTO IBT:
      IANS=0
      IBT=1
      II=I
      JJ=J
C
C COUNT OVER POSSIBLE BITS:
      DO 1 IB=1,NBITS
      DIFFI= .FALSE.
      I1=II/2
      I2=2*I1
C IF HALVING THEN DOUBLING HAS PRODUCED NO CHANGE, II WAS EVEN:
      IF (I2-II) 2,3,4
C JUST IN CASE SOME MACHINE ROUNDS IT UP:
   4  I1=I1-1
   2  DIFFI= .TRUE.
C
C SAME FOR JJ:
   3  DIFFJ= .FALSE.
      J1=JJ/2
      J2=2*J1
      IF (J2-JJ) 5,6,7
   7  J1=J1-1
   5  DIFFJ= .TRUE.
C
C IF INDIVIDUAL BITS BEING EXAMINED HAVE A 1, ADD IN BIT TO ANSWER
   6  IF (DIFFI .OR. DIFFJ) IANS=IANS+IBT
      IF (IB .NE. NBITS) IBT=IBT*2
C WE OMIT DOUBLING LAST TIME ROUND IN CASE OF OVERFLOW
      II=I1
      JJ=J1
   1  CONTINUE
      LOGOR=IANS
      RETURN
      END
C
C

C

C
C
C LEVEL 2       SUBROUTINE MAKGRP(IGSB,IOPS,MODE,PRODCT)
      SUBROUTINE MAKGRP(IGSB,IOPS,MODE,PRODCT)
C
C *** MAKGRP modified by PJB 31-May-1994 ***
C
CX
CC 1A
CH Generates the subgroup of a space group from the given generators.
CA On entry, IGSB holds the generators
CA    IABS(IGSB(1)) is the number of operators in the subgroup, divided
CA                  by 2 if the group contains a centre of symmetry.
CA                  If 1 it may indicate that the number of operators in
CA                  the sub-group is not known and should be calculated
CA                  by the subroutine
CA    IGSB(1)       is negative if the group contains a centre of symmetry
CA    IGSB(2)       is the identifying number of the first generator; it is
CA                  negative if it is the centrosymmetrically related one.
CA    IGSB(3)       is as IGSB(2) but for the second generator if there is one
CA    If MODE=1     the subroutine PRODCT is called with to allow evaluation
CA                  of a possible representation of the group.
CA    PRODCT        is an externally defined subroutine. May be DUMMY.
CA On exit, IOPS is a table which contains an entry for each operator of the
CA               full group. It is positive and non-zero if the operator
CA               belongs to the sub-group, and negative if it is only the
CA               centro-symmetric  operator which is in the sub-group.
CA   IGSB(1)     is set as above using the multiplicity of the sub-group
CA               found by the sub-routine.
CP SYMOP
C
      EXTERNAL PRODCT
      DIMENSION IGSB(3),IOPS(24),JOPS(24)
      LOGICAL PERM
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C SETUP
      CALL JGMZER(IOPS,1,NOPC)
      CALL JGMZER(JOPS,1,NOPC)
      JOPS(1)=1
      IOPS(1)=1
      NO=1
C
C  EXTRACT INFORMATION ABOUT GENERATORS
      NOPG=IABS(IGSB(1))
      IF (NOPG.EQ.1) NOPG=NOPC
      NG1=IABS(IGSB(2))
      IF (NG1.EQ.1) GO TO 101
      NO1=IABS(NORD(NG1))
      IF (NO1.GT.100) NO1=NO1-100
      NG2=IABS(IGSB(3))
C  PUT IN GENERATORS
      JOPS(2)=NG1
      IOPS(NG1)=ISIGN(1,IGSB(2))
      JOPS(NO1+1)=NG2
      IF (NG2.NE.0) IOPS(NG2)=ISIGN(1,IGSB(3))
C  AND MULTIPLES OF FIRST GENERATOR
      ISIG=IOPS(NG1)
      DO 2 NO=3,NO1
      I=MULTAB(JOPS(NO-1),NG1)
      ISIG=ISIG*IOPS(NG1)
      JOPS(NO)=I
      IF (MODE.EQ.1) CALL PRODCT(JOPS(NO-1),NG1,I)
    2 IOPS(I)=ISIG
      NO=NO1
      IF (NG2.EQ.0) GO TO 101
C
C  GENERATE THE REST OF THE SUB-GROUP
      N1=2
      N2=NO1+1
      NO=N2
C
    5 J=JOPS(N1)
    4 JJ=JOPS(N2)
      PERM=.TRUE.
      I=MULTAB(J,JJ)
      ISIG=IOPS(J)*IOPS(JJ)
    3 IF (IOPS(I).EQ.0) THEN
        NO=NO+1
        JOPS(NO)=I
        IF (MODE.EQ.1) THEN
          IF (PERM) THEN
            CALL PRODCT(J,JJ,I)
          ELSE
            CALL PRODCT(JJ,J,I)
          ENDIF
        ENDIF
        IOPS(I)=ISIG
        IF (NO.GE.NOPG) GO TO 101
      ENDIF
      IF (PERM) THEN
        PERM=.FALSE.
        I=MULTAB(JJ,J)
        GO TO 3
      ENDIF
      N2=N2+1
      IF (N2.LE.NO) GO TO 4
      N1=N1+1
      N2=MAX(NO1+1,N1)
      IF (N1.LE.NO) GO TO 5
C
C SET THE MULTIPLICITY TO THAT FOUND
  101 IGSB(1)=ISIGN(NO,IGSB(1))
  100 RETURN
      END
C
C
C
C
C LEVEL 1      CHARACTER *4 FUNCTION MAKNAM(CHAR,N)
      CHARACTER *4 FUNCTION MAKNAM(CHAR,N)
C
C *** MAKNAM BY JCM 8 JUL 91 ***
C
CX
CC 13C
CH Makes an A4 name from the given character and the digits of the given number.
CA On entry CHAR holds either a single character, to fill the name from the
CA               left, repeatedly, or 2,3 or 4 characters, which are used to
CA               fill in on the left unchanged.
CA          N is an integer
CA On exit MAKNAM is a *4 name with the digits of N at the right hand side,
CA          filled with the characters from CHAR.
C
      CHARACTER *4 NAME,CH
      CHARACTER *(*) CHAR
C
      CH=CHAR
      IF (LEN(CHAR) .EQ. 1) CH=CHAR//CHAR//CHAR//CHAR
      WRITE (NAME,2000) N
2000  FORMAT (I4)
      DO 1 I=1,4
      IF (NAME(I:I) .EQ. ' ') NAME(I:I)=CH(I:I)
   1  CONTINUE
      MAKNAM=NAME
      RETURN
      END
C
C
C
C
C
C LEVEL 4      SUBROUTINE MATCEL(ALSQ,MATSZ)
      SUBROUTINE MATCEL(ALSQ,MATSZ)
C
C *** MATCEL updated by JCM 15 Nov 90 ***
C
CX
CC 6B
CH After a cycle of LSQ gives variance-covariance matrix for cell A* B* etc
CH in both real and reciprocal space, and the same for abc, alpha,beta,gamma.
CA The Least Squares matrix ALSQ(MATSZ) must be handed throughout the system
CA as the argument of routine calls in order to make it easily altered.
CP  The LSQ matrix ALSQ is assumed to hold the inverse matrix after a
CP cycle of refinement.
CD Fills in array CELESD(6,6,2) for the variance-covariances of the
CD quadratic products in both spaces, and CELLSD(6,6) for a,b,etc.
C
      DIMENSION ALSQ(MATSZ)
      COMMON /CELFIX/IPTCEL(6),AMCELL(6),NCELF,NCELG,NCELS,KOM3
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /CONSTR/JCONST,JROWPT(301),JCMAT(200),AMOUNT(200),
     & NEXTJ
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      DIMENSION TOSTAR(6,6),TOSTTR(6,6),CELTMP(6,6),
     & BGTOSM(6,6),SMTOBG(6,6),SMBGTR(6,6)
C
C CLEAR OUT ALL ANSWERS
      CALL GMZER(CELESD(1,1,2),6,6)
C FIRST DO DIAGONALS, WHICH MAY BE NEEDED FOR OFF-DIAGONALS:
      DO 1 I=1,6
      LI=KCPARS(I)
C DO NOTHING IF ELEMENT I IS NOT A VARIABLE:
      IF (LI .GT. 0) THEN
        II=LVRBS(LI)
        A=1.
C DEAL WITH CASE OF REDUNDANT VARIABLE:
        IF (II .LT. 0) THEN
          A=AMOUNT(JROWPT(-II))
          II=JCMAT(JROWPT(-II))
        ENDIF
        CELESD(I,I,2)=ELEMAT(ALSQ,MATSZ,II,II)*A*A
      ENDIF
   1  CONTINUE
C
C OFF-DIAGONAL TERMS:
      DO 2 I=1,5
      LI=KCPARS(I)
C ONLY IF I'TH IS A VARIABLE;
      IF (LI .GT. 0) THEN
        II=LVRBS(LI)
        AI=1.
C DEAL WITH I'TH BEING REDUNDANT:
        IF (II .LT. 0) THEN
          AI=AMOUNT(JROWPT(-II))
          II=JCMAT(JROWPT(-II))
        ENDIF
        DO 3 J=I+1,6
        LJ=KCPARS(J)
C ONLY IF J'TH IS A VARIABLE;
        IF (LJ .GT. 0) THEN
          JJ=LBSVR(LJ)
          AJ=1.
C DEAL WITH J'TH BEING REDUNDANT:
          IF (JJ .LT. 0) THEN
            AJ=AMOUNT(JROWPT(-JJ))
            JJ=JCMAT(JROWPT(-JJ))
          ENDIF
C
          IF (II .NE. JJ) THEN
            CELESD(I,J,2)=ELEMAT(ALSQ,MATSZ,II,JJ)*AI*AJ
          ELSE
C IF BOTH I AND J ARE REDUNDANT AND RELATED TO SAME BASIC:
            CELESD(I,J,2)=SQRT(CELESD(I,I,2)*CELESD(J,J,2))
          ENDIF
C SYMMETRICALLY RELATED MATRIX ELEMENT:
          CELESD(J,I,2)=CELESD(I,J,2)
        ENDIF
   3    CONTINUE
      ENDIF
   2  CONTINUE
C
C SET UP MATRIX TO CONVERT DERIVATIVES WRT A,B,C ETC TO DERIVATIVES
C WRT A* B* C* ETC:
      CALL CELMAT(TOSTAR)
C
C GET THE TRANSPOSE OF TOSTAR (TOSTTR):
      CALL GMTRAN(TOSTAR,TOSTTR,6,6)
C CELESD(I,J,1)=TOSTAR(I,K)*CELESD(K,L,2)*TOSTTR(L,J)
      CALL GMPRD(CELESD(1,1,2),TOSTTR,CELTMP,6,6,6)
      CALL GMPRD(TOSTAR,CELTMP,CELESD,6,6,6)
C EVALUATE THE VARIANCE-COVARIANCE MATRIX FOR A B C ALPHA BETA GAMMA
C FIRSTLY FORM LITTLE A,B,C
      ASMALL=CELL(1,1,1)
      BSMALL=CELL(2,1,1)
      CSMALL=CELL(3,1,1)
C
      CALL GMZER(BGTOSM,6,6)
      BGTOSM(1,1)=2.*ASMALL
      BGTOSM(2,2)=2.*BSMALL
      BGTOSM(3,3)=2.*CSMALL
C     BGTOSM(4,4) HAS THE FORM -(180/PI)*B*C*SIN(ALPHA) ETC.
      BGTOSM(4,4)=-RADIAN(BSMALL*CSMALL*CELL(1,3,1))
      BGTOSM(5,5)=-RADIAN(ASMALL*CSMALL*CELL(2,3,1))
      BGTOSM(6,6)=-RADIAN(ASMALL*BSMALL*CELL(3,3,1))
      BGTOSM(2,4)=CPARS(4,1)/BSMALL
      BGTOSM(3,4)=CPARS(4,1)/CSMALL
      BGTOSM(1,5)=CPARS(5,1)/ASMALL
      BGTOSM(3,5)=CPARS(5,1)/CSMALL
      BGTOSM(1,6)=CPARS(6,1)/ASMALL
      BGTOSM(2,6)=CPARS(6,1)/BSMALL
      CALL GMINV(BGTOSM,SMTOBG,6)
      CALL GMTRAN(SMTOBG,SMBGTR,6,6)
C CELLSD(I,J)=SMTOBG(I,K)*CELESD(K,L,1)*SMBGTR(L,J)
      CALL GMPRD(CELESD,SMBGTR,CELTMP,6,6,6)
      CALL GMPRD(SMTOBG,CELTMP,CELLSD,6,6,6)
C
      RETURN
      END
C
C


C
C LEVEL 7      SUBROUTINE MATCOR(ALSQ,MATSZ)
      SUBROUTINE MATCOR(ALSQ,MATSZ)
C
C *** MATCOR updated by JCM 11 Aug 88 **
C
CX
CC 6C
CH After a Least Squares cycle, prints correlations from the inverse matrix.
C
CA The matrix ALSQ is dimensioned everywhere except in MAIN programs as
CA ALSQ(MATSZ), and handed through as a routine argument.
C
C%
C      CHARACTER *4 IPNAM1(2),IPNAM2(2),IUPPER(%BVAR%),LOWER(%BVAR%)
      CHARACTER *4 IPNAM1(2),IPNAM2(2),IUPPER(400),LOWER(400)
C%
C      DIMENSION ALSQ(MATSZ),MM(%BVAR%),ICORR(%BVAR%)
      DIMENSION ALSQ(MATSZ),MM(400),ICORR(400)
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      EQUIVALENCE (MM(1),MATPNT(2))
C
      IF (SIMUL) GO TO 100
C IF MAXCOR  -VE, NO PRINTING AT ALL:
       IF (MAXCOR .LT. 0) GO TO 100
      DO 1 I=1,LVARB
   1  BLSQ(I)=1./SQRT(ALSQ(MM(I)+I))
C BLSQ USED TEMPORARILY TO HOLD DIAGONAL - NOT NEEDED AS RHS NOW
C
C MAXCOR=0 ASKS FOR WHOLE CORRELATION MATRIX:
      IF (MAXCOR .EQ. 0) GO TO 2
C
C HERE TO PRINT CORRELATION ABOVE MAXCOR PER CENT:
      IHEAD=0
      DO 3 I=1,LVARB
      I1=I+1
      DO 3 J=I1,LVARB
      IND=MM(I)+J
      ICOR=NINT(100.*ALSQ(IND)*BLSQ(I)*BLSQ(J))
      IF (IABS(ICOR) .LT. MAXCOR) GO TO 3
C WRITE HEADING BEFORE FIRST CORRELATION FOUND TO BE LARGE ENOUGH:
      IF (IHEAD .EQ. 0) WRITE (LPT,2004) MAXCOR
2004  FORMAT (/' Correlations above',I4,' per cent:')
      IHEAD=1
** PROVISION FOR PHASE AND SOURCE CHANGES:
      CALL PARNAM(IPNAM1(1),IPNAM1(2),1,I)
      CALL PARNAM(IPNAM2(1),IPNAM2(2),1,J)
      WRITE (LPT,2005) IPNAM1(1),IPNAM1(2),
     & IPNAM2(1),IPNAM2(2),ICOR
2005  FORMAT (' ',2(1X,A4),' - ',2(1X,A4),'    ',I4)
   3  CONTINUE
      IF (IHEAD .EQ. 0) WRITE (LPT,2006) MAXCOR
2006  FORMAT (/' No correlations found above',I4,' per cent')
      GO TO 100
C
C WHOLE MATRIX:
   2  CALL MESS(LPT,1,'Correlation matrix :')
      CALL NEWLIN(LPT)
      DO 4 I=1,LVARB
C PUT INTO IUPPER AND LOWER THE PRINTING NAMES OF ALL BASIC VARIABLES:
      CALL PARNAM(IUPPER(I),LOWER(I),1,I)
   4  CONTINUE
C
      NV=0
   7  NV=NV+20
      NST=NV-19
      IF (NV .GT. LVARB) NV=LVARB
      NO=NV-NST+1
C NST=STARTING POINT, NV=END POINT, NO=NUMBER OF ITEMS IN THIS PRINTING
C WRITE HEADING FOR ONE PRINTING ON TWO LINES:
      WRITE (LPT,2001) (IUPPER(I),I=NST,NV)
2001  FORMAT (' ',10X,20(1X,A4))
      WRITE (LPT,2003) (LOWER(I),I=NST,NV)
2003  FORMAT (' ',12X,20(1X,A4))
C
      DO 9 I=1,LVARB
      K=NST-1
      DO 5 J=1,NO
      K=K+1
      ICORR(J)=NINT(100.*ELEMAT(ALSQ,MATSZ,I,K)*BLSQ(I)*BLSQ(K))
   5  CONTINUE
C
      WRITE (LPT,2002) (IUPPER(I),(LOWER(I)),J=1,2),
     & (ICORR(J),J=1,NO)
2002  FORMAT (' ',2(1X,A4),T112,2(1X,A4),T12,20I5)
   9  CONTINUE
C END OF ONE PRINTING
C
      IF (NV .LT. LVARB) GO TO 7
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE MATINV(ALSQ,MATSZ)
      SUBROUTINE MATINV(ALSQ,MATSZ)
C
C *** MATINV updated by JCM 2 Jun 89 ***
C
CX
CC 6C
CH Inverts the matrix in ALSQ, of which the upper triangle only is held.
CA ALSQ has the dimension MATSZ, which is set in MAIN programs and handed down
CA to here as a parameter of the routine calls.
C
CN No interchanges are done - a positive definite matrix should not need them.
C
CN This should be modified some day to operate on the right hand side BLSQ also.
C
CN It is intended to replace it by a QR inversion routine.
C
      CHARACTER *4 BS,VR
      LOGICAL TESTOV,OVER
      DIMENSION ALSQ(MATSZ)
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      IF (SIMUL) GO TO 100
C DIMENSION OF MATRIX:
      N=LVARB
      IF (N) 100,100,11
C
C SCALE MATRIX TO ALLOW FOR PARAMETERS OF DIFFERING SCALES:
  11  DO 21 I=1,N
      IR=MATPNT(I+1)
      OVER=TESTOV(1.,ALSQ(IR+I))
      IF (OVER) THEN
        CALL PARNAM(BS,VR,1,I)
        WRITE (LPT,3000) I,BS,VR
        WRITE (76,3000) I,BS,VR
3000    FORMAT(/' WARNING ** zero diagonal element for basic',
     &   ' variable number',I5,2X,A4,1X,A4)
        DERIVB(I)=0.
      ELSE
        DERIVB(I)=1./SQRT(ALSQ(IR+I))
      ENDIF
      DO 22 J=1,I
      IS=MATPNT(J+1)
  22  ALSQ(IS+I)=ALSQ(IS+I)*DERIVB(I)
      DO 23 J=I,N
  23  ALSQ(IR+J)=ALSQ(IR+J)*DERIVB(I)
      IF (OVER) THEN
        DERIVB(I)=1.
        ALSQ(IR+I)=1.
        BLSQ(I)=0.
      ENDIF
  21  CONTINUE
C
      DO 1 I=1,N
      I1=I+1
      IR=MATPNT(I1)
      IS=MATPNT(I)
      ALSQ(IR+I) = 1./ALSQ(IR+I)
C NEXT      BLSQ(I)=BLSQ(I)*ALSQ(IR+I)
      IF (I .EQ. N) GO TO 1
C OR NEXT DO LOOP WILL FAIL
      DO 2 K=I1,N
      ALSQ(IS+K)=-ALSQ(IR+I)*ALSQ(IR+K)
   2  CONTINUE
      DO 3 J=I1,N
      IQ=MATPNT(J+1)
C
C NEXT      BLSQ(J)=BLSQ(J)-BLSQ(I)*ALSQ(IR+J)
      DO 4 K=J,N
   4  ALSQ(IQ+K) = ALSQ(IQ+K) + ALSQ(IS+K)*ALSQ(IR+J)
   3  CONTINUE
   1  CONTINUE
      IF (N-1) 101,101,12
C
C INVERSE OF ALSQ:
  12  N1=N-1
      DO 5 IFOR=1,N1
      I=N-IFOR
      I1=I+1
      IR=MATPNT(I1)
      IS=MATPNT(I)
      DO 6 J=I1,N
   6  ALSQ(IR+J) = 0.
      DO 7 J=I1,N
      IQ=MATPNT(J+1)
      DO 8 K=J,N
      ALSQ(IR+K) = ALSQ(IR+K) + ALSQ(IS+J)*ALSQ(IQ+K)
      IF (J .NE. K) ALSQ(IR+J) = ALSQ(IR+J) + ALSQ(IS+K)*ALSQ(IQ+K)
   8  CONTINUE
      ALSQ(IR+I) = ALSQ(IR+I) + ALSQ(IS+J)*ALSQ(IR+J)
   7  CONTINUE
   5  CONTINUE
C
C BACK SUBSTITUTE FOR BLSQ (MAY BE INTENDED FOR LABEL 12):
C NEXT      DO 13 IFOR=1,N1
C NEXT      I=N+IFOR
C NEXT      I1=I+1
C NEXT      IR=MATPNT(I)
C NEXT      DO 14 J=I1,N
C NEXT      BLSQ(I)=BLSQ(I)-ALSQ(IR+J)*BLSQ(J)
C NEXT  14  CONTINUE
C NEXT  13  CONTINUE
C
C RESCALE MATRIX TO ALLOW FOR PARAMETERS OF DIFFERING SCALES:
 101  DO 31 I=1,N
      IR=MATPNT(I+1)
      DO 32 J=1,I
      IS=MATPNT(J+1)
  32  ALSQ(IS+I)=ALSQ(IS+I)*DERIVB(I)
      DO 33 J=I,N
  33  ALSQ(IR+J)=ALSQ(IR+J)*DERIVB(I)
  31  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE MATSET(ALSQ,MATSZ)
      SUBROUTINE MATSET(ALSQ,MATSZ)
C
C *** MATSET updated by JCM 11 Aug 88 ***
C
CX
CC 6C
CH Sets up pointers into a Least Squares matrix, and clears the matrix and the
CH corresponding right hand side vector.
CA The matrix ALSQ is dimensioned everywhere except in MAIN programs as
CA ALSQ(MATSZ).
C
      DIMENSION ALSQ(MATSZ)
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))

C
      IF (SIMUL) GO TO 100
C SET UP POINTERS:
      MATPNT(1)=0
      DO 2 I=1,LVARB
      MATPNT(I+1)=MATPNT(I)+LVARB-I+1
   2  CONTINUE
      NMAT=(LVARB+1)*(LVARB+2)/2
      IF (MATSZ .LT. NMAT) THEN
        WRITE (LPT,3000) MATSZ, NMAT,LVARB
        WRITE (ITO,3000) MATSZ, NMAT,LVARB
3000    FORMAT(/' ERROR ** MATSZ given as',I6,' and needs to be',
     &  '  at least',I6,' for',I5,' basic variables')
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
        CALL BMBOUT
        RETURN
      ENDIF
C
C CLEAR MATRIX AND RHS:
      CALL GMZER(ALSQ,1,MATSZ)
      CALL GMZER(BLSQ,1,LVARB)
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE MATSHF(ALSQ,MATSZ)
      SUBROUTINE MATSHF(ALSQ,MATSZ)
C
C *** MATSHF updated by JCM 30 Sep 88 ***
C
CX
CC 6C
CH From an inverted Least Squares matrix, calculates shifts in basic variables.
CA The Least Squares matrix ALSQ is dimensioned everywhere except in MAIN
CA programs as ALSQ(MATSZ), and both ALSQ and MATSZ are then handed through the
CA system as the arguments of routines.
CA On entry ALSQ must contain the inverse of the LSQ matrix.
C
CP The vector BLSQ must hold the rhs of the Least Squares equations.
CP SUMWD in /OBSCAL/ holds the sum of weighted differences,
CP NOBS in /OBSCAL/ = number of observations used,
CP LVARB in /DERBAS/= number of basic variables.
C
CD Calculates shifts from the inverse matrix (ALSQ) and the rhs (BLSQ),
CD putting the answers in BLSQ and the esds in DERIVB.
C
CN Eventually MATINV will modify BLSQ to give shifts, but for now we must
CN form them separately.
C
      CHARACTER *4 BS,VR
C%
C      DIMENSION ALSQ(MATSZ),MM(%BVAR%),SHIFTS(%BVAR%)
      DIMENSION ALSQ(MATSZ),MM(400),SHIFTS(400)
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /RSTATS/RNUM,RDEN,RSNUM,RSDEN,RWNUM,RWDEN,RWSDEN,CHI2
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
      EQUIVALENCE (MM(1),MATPNT(2))
C
      IF (SIMUL) GO TO 100
C SCALE FOR ESD'S:
      CHI2=SUMWD/(NOBS-LVARB)
      CHITOT=CHI2
      IF (NSKTOT .NE. 0) CHITOT=CSQTOT
C
      DO 1 I=1,LVARB
      IR=MM(I)
      SHIFTS(I)=0.
      IF (ALSQ(IR+I) .LT. 0.) THEN
        CALL PARNAM(BS,VR,1,I)
        WRITE (LPT,3000) I,BS,VR
        WRITE (ITO,3000) I,BS,VR
3000    FORMAT (/' ERROR ** ill-conditioning starts at basic variable',
     & ' number',I5,2X,A4,1X,A4)

C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
        
      ENDIF
C
      DERIVB(I)=SQRT(CHITOT*ALSQ(IR+I))
C
      DO 2 J=I,LVARB
   2  SHIFTS(I) = SHIFTS(I) + BLSQ(J)*ALSQ(IR+J)
      I1=I-1
      IF (I1) 1,1,4
   4  DO 5 J=1,I1
   5  SHIFTS(I) = SHIFTS(I) + BLSQ(J)*ALSQ(MM(J)+I)
   1  CONTINUE
C
C FOR NOW:
      DO 3 I=1,LVARB
   3  BLSQ(I)=SHIFTS(I)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE MATTOT(ALSQ,MATSZ)
      SUBROUTINE MATTOT(ALSQ,MATSZ)
C
C *** MATTOT updated by JCM 17 Oct 89 ***
C
CX
CC 6C
CH Add in contributions to LSQ matrix and RHS for one observation.
CA All reference to the lsq matrix ALSQ is made through routines with
CA arguments ALSQ and MATSZ.
CA This enables ALSQ to be dimensioned everywhere except in MAIN as ALSQ(MATSZ)
C
CP On entry DERIVB holds the derivatives wrt all basic variables
CP          DIFF holds OBS-CALC
CP          SQRTWT holds the square root of the weight
CP          SIMUL is TRUE if this is only a simulation cycle
C
C%
C      DIMENSION ALSQ(MATSZ),MM(%BVAR%)
      DIMENSION ALSQ(MATSZ),MM(400)
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      EQUIVALENCE (MM(1),MATPNT(2))
C
      IF (SIMUL) GO TO 100
      SQWDIF=SQRTWT*DIFF
      DO 3 I=1,LVARB
   3  DERIVB(I)=SQRTWT*DERIVB(I)
      DO 1 I=1,LVARB
      IF (DERIVB(I) .EQ. 0.) GO TO 1
      IR=MM(I)
      DO 2 J=I,LVARB
   2  ALSQ(IR+J)=ALSQ(IR+J)+DERIVB(I)*DERIVB(J)
      BLSQ(I)=BLSQ(I)+SQWDIF*DERIVB(I)
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE MESS(LUNIT,N,TXT)
      SUBROUTINE MESS(LUNIT,N,TXT)
C
C *** MESS updated by JCM 12 Sep 92 ***
C
CX
CC 13C
CH Writes on unit LUNIT the given message, preceded by N empty lines.
CA On entry TXT is a CHARACTER variable holding the message,
CA          N is in integer requesting N empty lines before the message, and
CA            may be 0, for no lines
CA            or > 98, for a page throw.
CO Writes to unit LUNIT N empty lines or a page throw, then the given text with
CO a "space" carriage control.
C
      CHARACTER *(*) TXT
C
      IF (N .LT. 99) THEN
        DO 1 I=1,N
   1    WRITE (LUNIT,2000)
2000    FORMAT (1X)
      ELSE
        WRITE (LUNIT,2002)
2002    FORMAT ('1')
      ENDIF
      WRITE (LUNIT,2001) (TXT(I:I),I=1,LENGT(TXT))
2001  FORMAT (1X,200A1)
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION MINIM(LIST,N,M)
      FUNCTION MINIM(LIST,N,M)
C
C *** MINIM by JCM 15 Jan 88 ***
C
CX
CC 11C
CH Finds the position and value of the minimum in a list of integers.
CA On entry LIST is a list of integers
CA          N is the number of entries in the list
CA On exit  MINIM is set to the value of the smallest menber of LIST
CA      where LIST(M)=MINIM
C
      DIMENSION LIST(N)
      COMMON /LENINT/NBITS
C
      MIN=2**(NBITS-1)-1
      DO 1 I=1,N
      IF (LIST(I) .GE. MIN) GO TO 1
      MIN=LIST(I)
      IKEEP=I
   1  CONTINUE
      MINIM=MIN
      M=IKEEP
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE MPCON(MAT,KMAX)
      SUBROUTINE MPCON(MAT,KMAX)
C
C *** MPCON by PJB ***
C
CX
CC 18A
CH Finds the symmetry constraints on multipoles.
CA MAT the multipole atom number
CA KMAX =  2l+1
C
      EXTERNAL DUMMY
      DIMENSION ANG(3),ROTN(3,3)
C
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /QROT/ROT(3,3,10)
      COMMON/SCRAT2/OROT(3,3,2),ISUB(24),MATP,ORMAT(169),
     & FUN(169),ORT(169),ANOR(13),USED(13),INDX(13),JNDX(13),DIJ(196)
      LOGICAL USED,LODD
      COMPLEX DIJ
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      IODD=2-MOD(KMAX/2,2)
      NPTS=KMAX*KMAX
      IAT=MPATAB(MAT)
      IF (MAT.EQ.MATP) GO TO 9
C  NEW ATOM
      MATP=MAT
      CALL GMEQ(ROT(1,1,MAT),OROT(1,1,1),3,3)
      CALL GMEQ(ROT(1,1,MAT),OROT(1,1,2),3,3)
      CALL TRINV3(OROT(1,1,2),D)
      CALL TRANSQ(OROT(1,1,2),3)
      CALL MAKGRP(ISGEN(1,IAT),ISUB,0,DUMMY)
      WRITE (LPT,110) (I*ISUB(I),I=1,NOPC)
  110 FORMAT (//' Elements of Sub-group: '/1X,24I5)
    9 CALL GMZER(FUN,NPTS,2)
      DO 1 N=1,NOPC
      IF (ISUB(N).EQ.0) GO TO 1
      ISIG=NORD(N)*ISUB(N)
C  REMEMBER THE QUANTUM ROTATIONS ARE SET FOR RECIPROCAL SPACE
      CALL GMEQ(SYM(1,1,INVERS(N)),ROTN,3,3)
      CALL TRANSQ(ROTN,3)
C  USE ONLY PROPER ROTATIONS
      IF (NORD(N).LT.0) CALL GMREV(ROTN,ROTN,3,3)
      CALL EULSYM(ANG,ROTN,OROT)
      CALL DIJROT(DIJ,ANG(1),ANG(2),ANG(3),KMAX)
      IF (IOUT.GT.150) WRITE (LPT,4016) KMAX/2,N,
     & (DEGREE(ANG(I)),I=1,3)
4016  FORMAT (/' L =',I2,' Operator No:',I3,' Euler Angles:',3F8.2)
      L=0
      LODD=MOD(KMAX/2,2).EQ.1
      DO 2 J=1,KMAX
C  REMOVE L ODD  IF CENTROSYMMETRIC
      IF (LODD .AND. ISGEN(1,IAT).LT.0) THEN
        DO 5 I=1,KMAX
    5   DIJ(L+I)=CMPLX(0.,0.)
      ELSE
C FOR IMPROPER ROTATIONS reverse signs IF L is odd
        IF (ISIG.LT.0 .AND. LODD) THEN
          DO 4 I=1,KMAX
    4     DIJ(L+I)=-DIJ(L+I)
        ENDIF
      ENDIF
      IF (IOUT.GT.200) WRITE(LPT,4010) (DIJ(L+I),I=1,KMAX)
4010  FORMAT (12(1X,7(2F8.4,2X)/))
      L=L+KMAX
    2 CONTINUE
      CALL REAORB(DIJ,ORT,KMAX)
      IF (IOUT.GT.200) THEN
      CALL MESS(LPT,1,'Dij matrix for real functions:')
      L=0
        DO 60 I=1,KMAX
        WRITE (LPT,4012) (ORT(J),J=L+1,L+KMAX)
        L=L+KMAX
   60   CONTINUE
      ENDIF
      CALL GMADD(FUN,ORT,FUN,1,NPTS)
    1 CONTINUE
      IF (IOUT .GT.100) CALL MESS(LPT,2,'Sum Matrix')
      FAC=1./FLOAT(IABS(ISGEN(1,IAT)))
      CALL GMSCA(FUN,FUN,FAC,1,NPTS)
      L=0
      IF (IOUT.GT.100)THEN
        DO 25 I=1,KMAX
        WRITE (LPT,4012) (FUN(J),J=L+1,L+KMAX)
        L=L+KMAX
   25   CONTINUE
      ENDIF
      CALL GMNORM(FUN,ANOR,KMAX,KMAX)
      CALL GMEQ(FUN,ORT,KMAX,KMAX)
      CALL TRANSQ(ORT,KMAX)
      CALL GMPRD(ORT,FUN,ORMAT,KMAX,KMAX,KMAX)
      IF (IOUT.GT.180) THEN
        CALL MESS(LPT,1,'Normalised sum matrix:')
        L=0
        DO 27 I=1,KMAX
        WRITE (LPT,4012) (FUN(J),J=L+1,L+KMAX)
        L=L+KMAX
   27   CONTINUE
        CALL MESS(LPT,1,'Product Matrix:')
        L=0
        DO 26 I=1,KMAX
        WRITE (LPT,4012) (ORMAT(L+J),J=1,KMAX)
4012    FORMAT (1X,15F8.4)
        L=L+KMAX
   26   CONTINUE
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE MPFORM(IDT,NJ)
      SUBROUTINE MPFORM(IDT,NJ)
C
C *** MPFORM updated by JCM Aug 92 ***
C
CX
CC 18A
CH Finds out which radial form factors to apply to which atom and L value.
CP ATOPOS etc.
CA IDT is the position in the CDF of the first J card
CA NJ the number of J cards
CD Called normally by PFSET
CD Sets MPLFOR(I,J) in COMMON /POLFOR/ to be the number of the form
CD factor to use for multipole atom I and L value J, if negative a radial
CD wave function is to be used to compute the form factors
C
      CHARACTER*4 WORD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /POLFOR/MPLFOR(20,6),NUMGEN,PFORFA(7),LMAX
C
C%
C      CALL JGMZER(MPLFOR,%MPFO%,%MPFL%)
      CALL JGMZER(MPLFOR,20,6)
      NAMFRE=NUMFNM+1
      DO 1 J=1,NJ
      CALL INPUTJ(IDT,NTYP,IAT,IPT,IE)
      IF (NTYP.NE.2) GO TO 2
      IERR=IERR+IE
    3 CALL RDWORD(WORD,LEN,IPT,IPT1,80,0,IE)
      IF (IE.EQ.100) GO TO 2
      IF (IE.NE.0) CALL ERRMES(1,1,' in form-factor name')
      IF (WORD.EQ.'RADF') THEN
C%
C        DO 6 I=1,%MPFL%
        DO 6 I=1,6
    6   MPLFOR(IAT,I)=-999
      ELSE
        IFORM=NCFIND(WORD,FONAME,NAMFRE-1)
        IF (IFORM.EQ.0) THEN
          FONAME(NAMFRE)=WORD
          IFORM=NAMFRE
C%
C          CALL ERRCHK(2,NAMFRE,%MPFO%,0,'Too many form-factors')
          CALL ERRCHK(2,NAMFRE,20,0,'Too many form-factors')
        ENDIF
        CALL RDINTG(L,IPT1,IPT,80,IE)
        IF (IE.NE.0) CALL ERRMES(1,1,' in form-factor L value')
        IF (L .GT. 0) MPLFOR(IAT,L)=IFORM
        GO TO 3
      ENDIF
    2 IDT=IDT+NYZ
    1 CONTINUE
      NUMGEN=NUMFNM
      NUMFNM=NAMFRE-1
C
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE MPOVAR(I,J)
      SUBROUTINE MPOVAR(I,J)
C
C *** MPOVAR by JCM 9 Feb 91 ***
C
CX
CC 7A
CH Records whether each multipole parameter is fixed or varied.
CA On entry I=which parameter (or 0 if it is the initial entry to fix all)
CA          J=which variable it will be (or 0 if it is the initial entry)
CD If this is the initial entry records all multipoles fixed by clearing the
CD array KPOLMP in /MPODA.  Otherwise, records there that parameter I is
CD variable J.
C
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      IF (J .EQ. 0) THEN
        DO 1 K=1,NMPOL
   1    KPOLMP(K)=0
      ELSE
        KPOLMP(I)=J
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE MTPROD(I,J,N)
      SUBROUTINE MTPROD(I,J,N)
C
C *** MTPROD new by PJB Nov 90 ***
C
Cx
CC 17C
CH Puts entries into the table of time inversion operators.
CA I, J, and N are operator numbers. The entry in MTSYM for N is
CA             set to the product of that for I and J.
CN Called from MAGSYM through MAKGRP using the internal subroutine
CN name PRODCT.
C
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
C
      MTSYM(N)=MTSYM(I)*MTSYM(J)
      RETURN
      END
C
C
C
C
C LEVEL 3      FUNCTION MULBOX(H)
      FUNCTION MULBOX(H)
C
C *** MULBOX updated by JCM 14 Jun 88 ***
C
CX
CC 3C
CH Tests indices for being in the asymmetric unit, and gives multiplicity.
C
CA On entry H(3) is a real 1x3 vector holding the reflection indices.
CD The function is set to the multiplicity of the reflection if the reciprocal
CD lattice point represented by H is inside the exact asymmetric unit.
CD If the point is not inside the function is returned as zero.
C
      DIMENSION H(3)
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
      COMMON /GUNIT/MARK(3,2),BSYM(3,3),IBOX,KOM11
C
C  DETECT 000
      MULT=1
      IBOX=999
      IF (ABS(H(1))+ABS(H(2))+ABS(H(3)).LT.10.E-5) GO TO 101
C
C  OTHERWISE START WITH MULT FOR GENERAL POSITION
      MULT=NMUL
C  VALUE FOR GENERAL POINT
C
      CALL INBOX(H,IBOX)
      IN=IBOX
      IF (IN) 102,101,3
C
    3 IF (IN.GT.10) GO TO 4
C  ON A PLANE
      M1=MARK(IN,1)+1
      GO TO (102,101,2,5,5) , M1
    2 MULT=MULT/2
      GO TO 101
C
C  WE ONLY WANT HALF THE PLANE
    5 TEST= SCALPR(H,BSYM(1,IN))
      IF (TEST+.0001 .LT.0.) GO TO 102
C  DIVIDE MULTIPLICITY IF ON DIAD AXIS
      IF (ABS(TEST).GE..0001) GO TO 101
      IF (MARK(1,1).EQ.4) GO TO 7
      MULT=MULT/2
      GO TO 101
C  BIT TO ACCEPT ONLY ONE HALF OF CENTRE LINE FOR P-1
   7  IF (SCALPR(H,BSYM(1,2)).LT.0.) GO TO 102
      GO TO 101
C
C  ON AN AXIS
    4 IN=IN-10
      IF (MARK(IN,2).EQ.0) GO TO 102
      MULT=MULT/IABS(MARK(IN,2))
      IF (MARK(IN,2).GT.0 .OR. NASYM .NE.2) GO TO 101
C  WE ONLY WANT HALF THE AXIS
      IF (SCALPR(H,EDGE(1,IN)).GE.0.) GO TO 101
C
C  HERE IF NOT STRICTLY INSIDE
 102  MULT=0
 101  MULBOX=MULT
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE NAMPOL(NAME,K,L)
      SUBROUTINE NAMPOL(NAME,K,L)
C
C *** NAMPOL by PJB ***
C
CX
CC 18C
CH Creates the label for a given multipole.
CA On entry K=2l+1 and L goes from 1 to K for m from m+ to m-
CA On exit  NAME is the A4 label
C
      CHARACTER*4 NAME,SIG(3)*1
      DATA SIG/'+',' ','-'/
C
      IL=K/2
      M=IABS(L-IL-1)
      I=1
      IF (L.GT.IL+1) I=3
      IF (L.EQ.IL+1) I=2
      WRITE (NAME,10) IL,M,SIG(I)
   10 FORMAT ('Y',2I1,A1)
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NCFIND(CH,CTABLE,NBOUND)
      FUNCTION NCFIND(CH,CTABLE,NBOUND)
C
C *** NCFIND by JCM 15 Jul 86 ***
C
CX
CC 11C
CH Searches for a particular word in a table of words.
CA CH is the word for which to search
CA CTABLE is a table containing NBOUND words
CD Sets the function value to zero of CH is not in the table or
CD to the position of CH in the table if it is there.
C
      CHARACTER *(*) CH,CTABLE(NBOUND)
C
      DO 1 L=1,NBOUND
      IF (CH .EQ. CTABLE(L)) GO TO 101
   1  CONTINUE
      L=0
 101  NCFIND=L
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NCHINT(CH)
      FUNCTION NCHINT(CH)
C
C *** NCHINT by JCM 27 Apr 90 ***
C
CX
CC 13C
CH Converts an ASCII character into an integer
CA On entry CH is the A1 character
CA On exit NCHINT is its decimal equivalent
C
      CHARACTER *1 CH
C
      NCHINT=ICHAR(CH)-48
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NDIGIT(I)
      FUNCTION NDIGIT(I)
C
C *** NDIGIT by JCM 11 Oct 83 ***
C
CX
CC 13C
CH Identifies a character as a digit or not.
CA I is a single A1 character
CD The function is set to -1 of I is not a digit, otherwise to the digit
CD value.
C
      CHARACTER *1 I
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
C
      DO 1 J=1,10
      IF (I .EQ. IDIGIT(J)) GO TO 2
   1  CONTINUE
      J=-1
   2  NDIGIT=J
      IF (NDIGIT .EQ. 10) NDIGIT=0
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE NEWCD
      SUBROUTINE NEWCD
C
C *** NEWCD by JCM 10 Mar 86 ***
C
CX
CC 6C
CH Opens a file on to which to write a new Crystal Data File after a Least
CH Squares refinement.
CD Asks interactively for a file name, and opens this with default extension
CH .CCN at RAL, .CRY at ILL, for unit NEWIN in /NEWOLD/
C
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      MESSAG='New Crystal Data file '
CRAL
      NAMFIL='.CCN'
CILL      NAMFIL='.CRY'
      CALL OPNFIL(NEWIN,113)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE NEWLIN(LUNIT)
      SUBROUTINE NEWLIN(LUNIT)
C
C *** NEWLIN by JCM 14 Sep 92 ***
C
CX
CC 13C
CH Writes a newline to the output unit LUNIT
C
      WRITE(LUNIT,2000)
2000  FORMAT (' ')
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE NEWPAG(LUNIT)
      SUBROUTINE NEWPAG(LUNIT)
C
C *** NEWPAG by JCM 14 Sep 92 ***
C
CX
CC 13C
CH Writes a newpage carriage control ('1') to the output unit LUNIT
C
      WRITE(LUNIT,2000)
2000  FORMAT ('1')
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NFIND(N,NTABLE,NBOUND)
      FUNCTION NFIND(N,NTABLE,NBOUND)
C
C *** NFIND by JCM 17 Apr 84 ***
C
CX
CC 11C
CH Searches for integer N in a table.
CA On entry N is the integer for which to search.
CA          NTABLE is an array of NBOUND integers.
CA          NBOUND is the number of entries in the table.
CD The function NFIND is set to zero if N is not in the table, or to
CD the number of the matching entry if one is found.
C
      DIMENSION NTABLE(NBOUND)
      DO 1 L=1,NBOUND
      IF (N .EQ. NTABLE(L)) GO TO 101
   1  CONTINUE
      L=0
 101  NFIND=L
      RETURN
      END
C
C
C
C
C LEVEL 3      FUNCTION NOPFIL(MODE)
      FUNCTION NOPFIL(MODE)

      USE WINTERACTER
C
C *** NOPFIL updated by PJB 24-Oct-1994 ***
C
CX
CC 13C
CH Opens a file on a FORTRAN unit for the first time in this job.
C
CA On entry MODE defines the file-type and indicates how to obtain the file name
CA  MODE may have up to 5 digits:
CA      the least significant, MODE1, indicates the file-type
CA      the tens digit,        MODE2, shows how to obtain the file-name
CA      the hundreds digit,    MODE3, deals with default extensions
CA      the thousands digit,   MODE4, deals with formatted or unformatted files
CA      the most significant,  MODE5, deals with sequential or direct access,
CA                                    and odd special cases.
CA
CA  MODE1 = 1 for a read file
CA        = 2 for a write file status new
CA        = 3 for a write file status undefined (UNKNOWN)
CA        = 4 for a write file to be modified (APPEND for sequential files)
CA        = 5 for a scratch file.
CA
CA  MODE2 = 0 Give standard messages for read or write files; machine specific
CA            information like disc, extension and ppn may be included in
CA            the user's response.
CA        = 1 Message in MESSAG otherwise as 0
CA        = 2 Find file-name in MESSAG. Report file opened.
CA        = 3 as 2 but don't give file-opened message.
CA        = 4 as 0 but don't give file-opened message.
CA
CA  MODE3 = 0 Use default disc and ppn, default extension .DAT
CA        = 1 Use defaults for extension, disc, and ppn found in characters
CA            1-4, 5-10, and 11-30 respectively of NAMFIL. If disc or ppn
CA            are absent default as system.
CA        = 2 Use file-name exactly as given.
CA
CA  MODE4 = 0 for formatted files with no FORTRAN carriage controls
CA        = 1 for unformatted files
CA        = 2 for formatted files with FORTRAN carriage controls (line printer)
CA
CA  MODE5 = 0 for sequential file access
CA        = 1 for direct access files
CA        = 2 for the special case for GENIE of sequential, RECL=128, the VMS-
CA            specific "RECORDTYPE='FIXED'".  This will expect MODE4=1 for
CA            unformatted, and will take account of MODEs 1,2 and 3.
C
CD Opens file, if possible, according to instructions in MODE.
CD NOPFIL is set to the logical unit assigned.
CD NOPFIL=0 indicates that no data have been given in response to the
CD interactive request for a file name. This may be useful as a way
CD  of reading several files in sequence, with RETURN given as response when no
CD more are wanted.
C
CD If on exit NOPFIL=-1, an error has occurred from which recovery was not
CD possible.
CD
CD 15 units are provided by the system; the numbers by which they are known
CD to the Operating System are in the array LUNTAB.
CD The array IOTAB marks the units available; IOTAB=0 for available
CD units, IOTAB=MODE for units in use.
C
C
      DIMENSION LM(2)
      LOGICAL SAYS,BRUCE
      CHARACTER *7 FILSTA(3)
      CHARACTER*10 FILACC(3)
      CHARACTER*12 FILFOR(2)
C%
C      CHARACTER*%FNAM% DEXT
      CHARACTER*100 DEXT
      CHARACTER*18 VFMT
      CHARACTER*40 SAVMES
      CHARACTER*26 MESS(2)
      CHARACTER*32 WFMT
      CHARACTER*80 FMT
      COMMON /FINAME/FILNAM(15)
      CHARACTER *10 FILNAM
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LOONEY/IOTAB(15),LUNTAB(15)
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /WHEN/DAT,TIM(2),MAIN
      CHARACTER *5 TIM
      CHARACTER *10 DAT
      CHARACTER *6 MAIN
C>> JCC Declaration
      INTEGER ISPAG
      DATA LM/13,14/
      DATA FILSTA/'OLD','NEW','UNKNOWN'/
      DATA FILACC/'SEQUENTIAL','DIRECT','APPEND'/
      DATA FILFOR/'FORMATTED','UNFORMATTED'/
      DATA MESS/'Give name of Input file  ',
     & 'Give name for Output file '/
C
C     filnam_root is the root filename (e.g. 'ct130k') communicated
C     in from FORTY via common block commun.
      common/commun/filnam_root
      character*10 filnam_root
C  UNSCRAMBLE MODE
      M=MODE
      MODE5=M/10000
      M=M-MODE5*10000
      MODE4=M/1000
      M=M-MODE4*1000
      MODE3=M/100
      M=M-MODE3*100
      MODE2=M/10
      MODE1=M-MODE2*10
C
C  SET DEFAULTS FORMATTED, SEQUENTIAL, AND NEW FOR WRITE, OLD FOR READ
      IS=MODE1
      IA=1
      IF=1
C
C
C  SET FORMAT AND ACCESS
      IF (MODE1 .EQ. 4) THEN
C APPEND
        IS=3
        IA=3
      ENDIF
C
C ADJUST FORMATTED/UNFORMATTED
      IF (MODE4.EQ.1) IF=2
C ADJUST SEQUENTIAL/DIRECT FOR ACCESS
      IF (MODE5.EQ.1) IA=2
C IO SET FOR INPUT OR OUTPUT FILE MESSAGE
      IO=1
      IF (MODE1.GT.1) IO=2
C
C  NOW FIND A UNIT
C%
C      DO 1 IU=1,%FILE%
      DO 1 IU=1,15
      IF (IOTAB(IU).EQ.0) GO TO 2
    1 CONTINUE
      GO TO 51
    2 LUN=LUNTAB(IU)
      IF (MODE1 .EQ. 5) THEN
C
C  OPEN SCRATCH FILE
        IF (IA .EQ. 1) OPEN (UNIT=LUN,ACCESS=FILACC(IA),FORM
     &                  =FILFOR(IF),STATUS='SCRATCH')
C FOR NOW, IF DIRECT ACCESS, RECL=80 - THIS WILL NEED ATTENTION
        IF (IA .EQ. 2) OPEN (UNIT=LUN,ACCESS=FILACC(IA),FORM
     &                  =FILFOR(IF),STATUS='SCRATCH',RECL=80)
        GO TO 101
      ENDIF
C
C  OTHERWISE GET THE FILE-NAME
C  DEAL WITH EXTENSIONS
      IF (MODE3.EQ.0) DEXT='.DAT'
      IF (MODE3.EQ.1) DEXT=NAMFIL
      IF (MODE3.EQ.2) DEXT=' '
      IF (MODE2 .NE. 1) THEN
C PRESERVE FILE NAME IF IT CAME IN MESSAG
        IF (MODE2 .EQ. 2 .OR. MODE2 .EQ. 3) NAMFIL=MESSAG
C PUT ENOUGH OF STANDARD MESSAGE INTO MESSAG TO USE ALSO WHEN REPORTING
        MESSAG=MESS(IO)(LM(IO)+1:)
      ENDIF
C
C%
C      LMES=LENG(MESSAG,%MESS%)+1
      LMES=LENG(MESSAG,100)+1
C AVOID SENDING MESSAGE IF NOT WANTED
      IF (MODE2 .EQ. 2 .OR. MODE2 .EQ. 3) GO TO 21
C  SEND MESSAGE:
C        WRITE (VFMT,2000) LM(IO),LMES
CVMS

 2000 FORMAT ('(1X,A',I2,',A',I2,',1X,$)')
C3084 2000 FORMAT ('(1X,A',I2,',A',I2,',1X)')
C

C JOIN HERE IF FIRST TRY FAILED, & IT MAY BE USEFUL TO TRY .CRY FOR .CCL:
C    6   WRITE (ITO,VFMT) MESS(IO)(1:LM(IO)),MESSAG

      ISPAG = 0

    6    CONTINUE
C   The above line added to supress unwanted output in the
C   noninteractive version
C>> JCC
C>> It seems that the program can get caught in an infinite spaghetti loop here
C>> so I've added in a pass count check. 
      ISPAG = ISPAG + 1
      IF (ISPAG .GT. 10) THEN
            NOPFIL = -1
            RETURN
      ENDIF
CUNIX
C           CALL FLUSH(ITO)
C SPECIAL BRUCE BLIP TO USE BOTH .CCL AND .CRY FILES WITHOUT KNOWING WHICH:
        BRUCE=.FALSE.
C
C      READ (ITI,2001) NAMFIL
C      Punt filnam_root into namfil instead of reading
C      it in from standard input
       write(namfil,'(a6)') filnam_root
C%
C 2001 FORMAT (A%FNAM%)
 2001 FORMAT (A100)
C
C  PROCESS FILE NAME
C%
C   21 IF (LENG(NAMFIL,%FNAM%).EQ.0) GO TO 40
   21 IF (LENG(NAMFIL,100).EQ.0) GO TO 40
  89  CALL FILPRO(DEXT,IU,LNAM)
C  CHECK FOR ERROR IN FORM OF FILE-NAME
      IF (LNAM.EQ.0) GO TO 50
C SAVE MESSAGE IN SAVMES
      SAVMES=MESSAG(1:40)
C
C  OPEN FILE
   30 CONTINUE
C
C IF GENIE FILE, VAX-SPECIFIC BECAUSE OF RECORDTYPE:
CVMS

      IF (MODE5 .EQ. 2) THEN
CVMS

      OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),
CVMS

     &  STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE,
CVMS

     &  RECL=128)
CVMS

      GO TO 22
CVMS

      ENDIF
C
CVMS

      IF (MODE1.EQ.1) THEN
C MUST SPECIFY READONLY FOR READ FILES ON VAX TO AVOID PROTECTION FAILURES
C WHEN READING FROM FOREIGN DIRECTORIES
CVMS
!     CALL ErrorMessage('NAMFIL = '//NAMFIL)
      OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),
CVMS

     & STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE)
CVMS

      ELSE
CVMS

      IF (MODE4 .EQ. 0) THEN
CVMS

      OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),
CVMS

     & STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE
CVMS

     & )
CVMS

      ELSE
      OPEN (UNIT=LUN,FILE=NAMFIL,ACCESS=FILACC(IA),FORM=FILFOR(IF),
     & STATUS=FILSTA(IS),ERR=52,IOSTAT=IOE)
CVMS

      ENDIF
CVMS

      ENDIF
  22  IF (MODE2 .EQ. 3 .OR. MODE2 .EQ. 4) GO TO 101
C
C  REPORT DONE
      IF (LMES.GT.40) LMES=40
      DO 31 J=1,LMES
      IF (SAVMES(J:J).NE.'(') GO TO 31
      IL=J-1
      GO TO 32
   31 CONTINUE
      IL=LMES
   32 WRITE (WFMT,2002) IL,LNAM
 2002 FORMAT ('(/1X,A',I2,',1X,A',I2,','' opened'')')
      WRITE (LPT,WFMT) SAVMES(1:IL),NAMFIL(1:LNAM)
      GO TO 101
C
C  NO RESPONSE - 'RETURN' TYPED
C DETECT SPECIALLY THE RESPONSE "RETURN" ON RVAX REQUEST FOR O/P FILE:
  40  IF (LUN .EQ. LPT .AND. MODE .EQ. 2142) THEN
        NAMFIL=MAIN//'.LIS'
        GO TO 89
      ENDIF
      NOPFIL=0
      GO TO 100
C
   51 CALL ERRMES(1,0,'*** ERROR No more logical units available')
C
C 'OPEN' ERROR - CHECK FIRST FOR BRUCE BLIP:
  52  IF (BRUCE .OR. MODE .NE. 111) GO TO 152
      BRUCE=.TRUE.
      IF (DEXT(2:4) .EQ. 'CRY') THEN
        DEXT(2:4)='CCL'
      ELSE IF (DEXT(2:4) .EQ. 'CCL') THEN
        DEXT(2:4)='CRY'
      ENDIF
C%
C      L1=LENG(NAMFIL,%FNAM%)
      L1=LENG(NAMFIL,100)
      NAMFIL(L1-2:L1)=DEXT(2:4)
      I=INDEX(FILNAM(IU),'.')
      FILNAM(IU)(I+1:I+3)=DEXT(2:4)
      GO TO 30
C
C IDENTIFY ERROR IN MACHINE DEPENDENT TABLE
  152 IOS=IPOPE(IOE)
      GO TO (55,56,53,54,60) , IOS+1
C
C ERROR TYPE 1 - 'OLD' FILE FOUND NOT TO EXIST:
   56 WRITE (FMT,601) LNAM
 601  FORMAT ('('' File '',A',I2,','' does not exist'')')
  62  WRITE (LPT,FMT) NAMFIL(1:LNAM)
      WRITE (ITO,FMT) NAMFIL(1:LNAM)
CUNIX
C           CALL FLUSH(ITO)
   50 IF (MODE2.LT.2 .OR. MODE2 .EQ. 4) GO TO 6
C NO WAY TO OFFER USER ANOTHER TRY:
      NOPFIL=-1
      GO TO 100
C
C ERROR TYPE 2 - 'NEW' FILE EXISTS ALREADY:
   53 CALL ASK('Existing file '//NAMFIL(1:LNAM)//' will be overwritten'
     &//' OK? (Y/N) ')
      IF (.NOT. SAYS('Y')) GO TO 50
C ALLOW TO TRY SAME FILE AGAIN WITH STATUS OLD
      IS=1
      GO TO 30
C
C ERROR TYPE 3 - BAD FILE NAME (COVERS VARIOUS SYNTACTICAL ERRORS):
   54 WRITE (FMT,603) LNAM
 603  FORMAT ('('' *** ERROR Bad file name '',A',I2,','' IOSTAT='',I4)')
      GO TO 61
C
C ERROR TYPE 4 - FILE ALREADY OPEN
  60  WRITE (FMT,604) LNAM
 604  FORMAT ('('' *** ERROR File '',A',I2,','' already open'')')
      GO TO 62
C
C ERROR TYPE 0 - OTHERS
   55 WRITE (FMT,600) LNAM
 600  FORMAT ('('' *** ERROR opening file '',A',I2,','' IOSTAT='',I4)')
  61  WRITE (ITO,FMT) NAMFIL,IOE
CUNIX
C         CALL FLUSH(ITO)
      WRITE (LPT,FMT) NAMFIL,IOE
      GO TO 50
C
C SUCCESSFUL EXIT
  101 NOPFIL=LUN
      IOTAB(IU)=MODE
  100 MESSAG=' '
      RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION NORDER(J)
      FUNCTION NORDER(J)
C
C *** NORDER by PJB/JCM 28 Jun 83 ***
C
CX
CC 1C
CH Returns the order of the Jth symmetry operator.
CA On entry J is the number of a symmetry operator.
CD The the absolute value of the function is set to the order of the
CD Jth symmetry operator, NORDER is negative if the rotation is an
CD improper one.
CP The symmetry operators must have been read by SYMOP, so that MULTAB is
CP filled in in /SYMTAB/ and SYM in /SYMDA/ holds the rotation matrices.
C
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      N=1
      I=J
    2 IF (I.EQ.1) GO TO 1
C KEEP LOOKING UP RESULT OF REPEATEDLY MULTIPLYING OPERATOR BY ITSELF
C IN THE MULTIPLICATION TABLE;  EVENTUALLY THIS WILL LEAD TO ELEMENT 1,
C THE IDENTITY OPERATOR, AND THE NUMBER OF REQUIRED MULTIPLICATIONS
C IS THE ORDER OF THE OPERATOR
      I=MULTAB(J,I)
      N=N+1
      GO TO 2
C
C N NOW THE ORDER; NEGATE IF INVERSION OPERATOR ALSO:
   1  IF (DETER3(SYM(1,1,J)) .LT. 0.) N=-N
      NORDER=N
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE NPACK(NPK,L,N,MODE,LPACK)
      SUBROUTINE NPACK(NPK,L,N,MODE,LPACK)
C
C *** NPACK updated by JCM 11 Jan 88 ***
C
CX
CC 16C
CH Deals with the packing and unpacking of up to 10 integers in/out of one
CH integer.
CA On entry MODE gives the required operation:
CA
CA   MODE=0: set up for subsequent entries.  The array L contains,
CA           for each item to be packed/unpacked, a number MAX(I).  If
CA           MAX(I) is positive, items in position I in the packing are
CA           expected to be in the range 0 to MAX(I).
CA
CA           if MAX(I) is negative, items in the I'th position in the packing
CA           are expected to be in the range MAX(I) to -MAX(I).
CA
CA           the array L is of dimension N; NPK is irrelevant.
CA           LPACK is a (10,3) array to be used as working space.
CA
CA   MODE=1: Pack an array of items given in L into the integer NPK, according
CA           to the information set up by a mode 0 entry.
CA
CA   MODE=2: Unpack into an array L the items previously packed into NPK.
C
      DIMENSION LPACK(10,3)
      DIMENSION L(N)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS
C
C
      IF (N .LE. 0) CALL ERRIN2(N,0,'misuse of NPACK - N=',' ')
C
      GO TO (21,22,23) , MODE+1
C
C SETTING UP:
  21  AM=1.
      DO 2 I=1,N
      LPACK(I,1)=L(I)
      IF (LPACK(I,1) .LT. 0) GO TO 10
C
C POSITIVE MAX VALUE:
  12  LPACK(I,2)=0
      LPACK(I,3)=LPACK(I,1)+1
      GO TO 13
C
C NEGATIVE 'MAX' VALUE:
  10  LPACK(I,2)=-LPACK(I,1)
      LPACK(I,3)=-2*LPACK(I,1)+1
  13  AM=AM*FLOAT((IABS(LPACK(I,1))+LPACK(I,2)+1))
   2  CONTINUE
C
C TEST LARGEST POSSIBLE PACKED NUMBER WILL FIT IN MACHINE INTEGER, LESS SIGN
      IF (AM .LT. 2.**NBITS) GO TO 100
      WRITE (LPT,3002) AM,NBITS
      WRITE (ITO,3002) AM,NBITS
3002  FORMAT (/' ERROR ** LARGEST POSSIBLE PACKED NUMBER IS',
     & G14.7,' AND WILL NOT FIT INTO',I5,' BITS')
      GO TO 100
C
C MODE=1:  PACKING:
  22  NPK1=0
      DO 3 I=1,N
      LP=L(I)
      IF (LPACK(I,1) .LT. 0) LP=IABS(L(I))
      IF (0 .LE. LP .AND. LP .LE. IABS(LPACK(I,1))) GO TO 4
      WRITE (LPT,3003) L(I),LPACK(I,1)
      WRITE (ITO,3003) L(I),LPACK(I,1)
3003  FORMAT (/' ERROR ** ITEM',I8,' CANNOT BE PACKED USING',
     & ' MAX=',I8)
      GO TO 100
C
   4  NPK1=NPK1*LPACK(I,3)+L(I)+LPACK(I,2)
   3  CONTINUE
      NPK=NPK1
      GO TO 100
C
C MODE 2:  UNPACKING:
  23  NPK1=NPK
      DO 5 I=1,N
      J=N-I+1
      K=NPK1/LPACK(J,3)
      L(J)=NPK1-LPACK(J,3)*K-LPACK(J,2)
      NPK1=K
   5  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NSIGFG(DX)
      FUNCTION NSIGFG(DX)
C
C *** NSIGFG new by PJB 9 Mar 1994 ***
C
CH Returns the number of figures to print after the decimal point based on the
CH ESD dx
CA On entry DX is n ESD
CD The value of the ESD DX is used to determine the number of figures to print
CD after the decimal point. NSIGFG is returned in the range 1-5. NSIGFG is used
CD for printing parameters, shifs and ESd's after a least squares cycle.
C
      DEL=ABS(DX)
      IFIG=1
    1 ISIG=IFIX(DEL)
      IF (ISIG.GT.0 .OR. IFIG .EQ.5) GO TO 2
      IFIG=IFIG+1
      DEL=10*DEL
      GO TO 1
  2   NSIGFG=IFIG
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NSYMBL(I)
      FUNCTION NSYMBL(I)
C
C *** NSYMBL by JCM 11 Oct 83 ***
C
CX
CC 13C
CH Finds whether the character I is one of the symbols recognised by the system.
CA I is an A1 character
CD The function is set to zero if I is not one of the 21 characters held
CD in the table ISMBOL in COMMON /CHARS/, otherwise it is set to the number
CD of the matching symbol.
C
      CHARACTER *1 I
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
C
      DO 1 J=1,21
      IF (I .EQ. ISMBOL(J)) GO TO 2
   1  CONTINUE
      J=0
   2  NSYMBL=J
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION NTICK(NTIME)
      FUNCTION NTICK(NTIME)
C
C *** NTICK by JCM 7 Jan 88 ***
C
CX
CC 16C
CH Advances its argument by 1 and sets the function to that value also.
CA NTIME on entry has some value, which on exit has been increased by 1
CD Used to keep track of the order of events in a job.
C
      NTIME=NTIME+1
      NTICK=NTIME
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE NUMA1(X,IFIELD,IFDIG,ITOT,IA)
      SUBROUTINE NUMA1(X,IFIELD,IFDIG,ITOT,IA)
C
C *** NUMA1 updated by JCM 12 Nov 89 ***
C
CX
CC 13C
CH Prepares a number for writing, probably on a plotter.
CA X is a floating point number to be decoded into characters
CA IA is a string variable of length at least IFIELD to receive the characters
CA IDIG is the number of digits to be given after the decimal point
CA      if IDIG is zero the decimal point is omitted.
CA The final number is right justified in a field of width ITOT
CA placed centrally in IFIELD.
CD If the number is too big to fit in IFIELD it is replaced by stars.
C
      CHARACTER *(*) IA
      DIMENSION ITEMP(5)
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF=IFIELD
      IF (ITOT .GE. IF) GO TO 6
      WRITE (LPT,3001) ITOT,IF,X
      WRITE (ITO,3001) ITOT,IF,X
3001  FORMAT (/' ERROR ** IN USE OF NUMA1 - TOTAL FIELD OF',I5,
     & ' AND NUMBER FIELD OF',I5,' REQUESTED - PRINTING',F12.4)
      DO 1 I=1,ITOT
   1  IA(I:I)='*'
      GO TO 100
C
   6  IPT=IF+(ITOT-IF)/2
C IPT MOVES FROM RIGHT TO LEFT IN OUTPUT ARRAY IA
      IA=' '
      Y=ABS(X)
      IF (IFDIG) 2,2,3
C
C HERE IF THERE IS A FRACTIONAL PART
   3  CALL FRACT(Y,A,N)
      CALL INTDIG(NINT(Y*10.**IFDIG),ITEMP,NDIG)
      L=NDIG
C COPY FRACTION BACKWARDS
      DO 4 K=1,IFDIG
      IF (ITEMP(L) .EQ. 0) ITEMP(L)=10
      IF (L .GT. 0) IA(IPT:IPT)=IDIGIT(ITEMP(L))
      IF (L .LE. 0) IA(IPT:IPT)=IDIGIT(10)
      IPT=IPT-1
      L=L-1
   4  CONTINUE
      IA(IPT:IPT)='.'
      IPT=IPT-1
      Y=A
C
C HERE TO DO INTEGER PART
   2  CALL INTDIG(NINT(Y),ITEMP,NDIG)
      L=NDIG
      DO 7 K=1,NDIG
      IF (ITEMP(L) .EQ. 0) ITEMP(L)=10
      IA(IPT:IPT)=IDIGIT(ITEMP(L))
      IPT=IPT-1
      L=L-1
   7  CONTINUE
C
C SIGN
      IF (IFDIG .EQ. 0 .AND. NINT(Y) .EQ. 0) GO TO 100
      IF (X .LT. 0.) IA(IPT:IPT)='-'
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE NUMDEN(X,N1,N2,KI,KO)
      SUBROUTINE NUMDEN(X,N1,N2,KI,KO)
C
C *** NUMDEN by JCM ***
C
CX
CC 13C
CH Converts a real number to the numerator and denominator of a fraction.
CA On entry X is a real number with absolute value normally less than 1
CA KI=0 If any range of fraction is allowed
CA  =1 If denominators of 2,3,4 and 6 only allowed
CA
CA On exit
CA KO=0 If X was 0 - in this case N1=N2=0
CA  =+1 If X was a +ve fraction < 1 . N1= numerator, N2=denominator
CA  =-1 If ABS(X) was as above but x was -ve.  N1, N2 set for ABS(X)
CA  =+2 If X WAS >1.  N1, N2 set for fractional part of X
CA  =-2 If X WAS < -1.  N1, N2 set for ABS(X)
CA  =99 If X would not convert to a fraction with single digit numerator
CA      and denominator, to 4 decimals
CA =-99 If X was as above and -ve
CN Used for printing fractions in OPSYM
C
      XX=X
      IEND=KI+9-4*KI
      N1=0
      N2=0
      KO=0
      IF (XX .EQ. 0) RETURN
C KEEP SIGN OF NUMBER IN NEG - THEN WORK WITH MODULUS:
      NEG=IFIX(SIGN(1.0,XX))
      XX=ABS(XX)
      NBIG=1
      IF (XX .LT. 1.) GO TO 1
C REDUCE TO FRACTION LESS THAN 1:
      CALL FRACT(XX,Y,J)
      NBIG=2
   1  DO 2 IDEN=2,IEND
C AVOID 5 IF ASKED TO CONSIDER ONLY 2,3,4,6:
      IF ((IEND .EQ. 6) .AND. IDEN .EQ. 5) GO TO 2
      ID1=IDEN-1
      DO 3 INUM=1,ID1
      IF (ABS((FLOAT(INUM)/FLOAT(IDEN))-XX) .LT. 0.0001) GO TO 4
      ITEMP=INUM/IDEN
      ATEMP=FLOAT(ITEMP)
      BTEMP=ATEMP-XX
      CTEMP=ABS(BTEMP)
   3  CONTINUE
   2  CONTINUE
C
C HERE NO FRACTION WILL FIT
      NBIG=99
      GO TO 5
   4  N1=INUM
      N2=IDEN
   5  KO=NBIG*NEG
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE NWINDS
      SUBROUTINE NWINDS
C
C *** NWINDS updated by PJB 29-Sept-93  ***
C
CX
CC 7B
CH Outputs to unit NEWIN a new input dataset at the end of a refinement of
CH cell parameters and propagation vector from d spacing values.
CP The Crystal Data File must have been read by PREFIN
CD Deals with changed C, I or Q PROP cards.
CO Writes a new Crystal Data File to unit NEWIN in /NEWOLD/
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      ID=0
   1  ID=ID+1
      IF (ID .GT. NTOTAL(1)) GO TO 100
      READ (IO10,REC=ID,FMT=1000) ICARD
1000  FORMAT (A80)
      L=LETTER(ICARD(1:1))
      IF (L .EQ.3) GO TO 3
      IF (L.EQ.17) GO TO 4
      IF (L .NE. 9) GO TO 2
C
C OUTPUT NEW I CARD:
      CALL OTPUTI
      GO TO 1
C
C OUTPUT NEW C CARD WITH NEW VALUES:
   3  CALL CELNEW
      GO TO 1
C
C Q PROP CARD NEEDS TO BE REWRITTEN
   4  CALL PROPAG(4,NEWIN)
      GO TO 1
C
C COPY UNCHANGED CARD:
   2  WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
2000  FORMAT (80A1)
      GO TO 1
C
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE NWINFW
      SUBROUTINE NWINFW
C
C *** NWINFW updated by JCM 10 May 88 ***
C
CX
CC 7B
CH Writes out a new Crystal Data File for main program FWLSQ.
CP NWINFW should only be called in the context of FWLSQ, after one
CP cycle of refinement in which the refined parameters have been updated.
CP An old Crystal Data File should be held on the scratch unit number IO10.
C
CD Writes out new file, with new values on the L VALS card.
C
CO Outputs new file on unit NEWIN.
C
      CHARACTER *4 LTEMP
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FWVALS/NVALS,COEFFS(9)
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
C
      ID=0
   1  ID=ID+1
      IF (ID .GT. NTOTAL(1)) GO TO 100
      READ(IO10,REC=ID,FMT=1000) ICARD
1000  FORMAT (A80)
      L=LETTER(ICARD(1:1))
      IF (L .EQ.12) GO TO 3
      IF (L .NE. 9) GO TO 2
C
C OUTPUT NEW I CARD:
      CALL OTPUTI
      GO TO 1
C
C OUTPUT NEW "L" CARD
   3  CALL RDWORD(LTEMP,LEN,3,IPT,80,0,IER)
      IF (LTEMP .NE. 'VALS') GO TO 2
C NEW "L VALS" CARD:
      IF (NVALS .LT. 0) GO TO 1
      LVCARD=1
      IF (NVALS .GE. 7) LVCARD=2
      IV=0
      DO 12 I=1,LVCARD
      JEND=6
      IF (IV+6 .GT. NVALS) JEND=NVALS-IV
      WRITE (NEWIN,2000) (COEFFS(IV+J),J=1,6)
2000  FORMAT ('L VALS',6F12.5)
      IV=IV+6
  12  CONTINUE
      NVALS=-NVALS
      GO TO 1
C
C COPY UNCHANGED CARD:
   2  WRITE (NEWIN,2001) (ICARD(I:I),I=1,LENGT(ICARD))
2001  FORMAT (80A1)
      GO TO 1
C
 100  RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE NWINMP(MAGROU)
      SUBROUTINE NWINMP(MAGROU)
C
C *** NWINMP updated by JCM 11 Feb 91 ***
C
CX
CC 7B
CH Writes to unit NEWIN a replacement Crystal Data File after multipole LSQ.
CD For J MPOL cards after refinement, writes out all multipoles, not just
CD the ones that went in.
C
C
      EXTERNAL MAGROU
      CHARACTER *4 WORD,CHANGE(2)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA CHANGE/'TFAC','SCAL'/
C
C OPEN NEW CD FILE:
      CALL NEWCD
C SCAN CARDS ON FILE IO10, PRODUCING NEW ONES IF THEY MAY HAVE CHANGED:
      ID=0
   1  ID=ID+1
      IF (ID .GT. NTOTAL(1)) GO TO 101
      READ(IO10,FMT=1000,REC=ID) ICARD
1000  FORMAT(A80)
      LOLD=L
      L=LETTER(ICARD(1:1))
      IF (LOLD .EQ. 10 .AND. L .NE. 10) CALL JMPOL
C CHECK FOR L CARD
      IF (L .EQ. 12) GO TO 3
C TREAT A, F AND T ALIKE:
      IF (L .EQ. 1 .OR. L .EQ. 6 .OR. L .EQ. 20) GO TO 4
C CHECK FOR E CARD
      IF (L .EQ. 5) GO TO 5
C CHECK FOR J CARD
      IF (L .EQ. 10) GO TO 6
C CHECK FOR Q CARDS:
      IF (L .EQ. 17) GO TO 7
      IF (L .NE. 9) GO TO 2
C
C I CARD:
      CALL OTPUTI
      GO TO 1
C
C ANY FAMILY 2 PARAMETERS:
   4  CALL F2NEW(L)
      GO TO 1
C
C J CARD
   6  CALL RDWORD(WORD,LEN,3,IPT,80,0,IER)
      CALL RDWORD(WORD,LEN,IPT,IPT,80,0,IER)
      IF (WORD .NE. 'MPOL') GO TO 2
      GO TO 1
C
C Q CARD:
   7  CALL MAGROU(4)
      GO TO 1
C
C L CARD:
   3  CALL RDWORD(WORD,LEN,3,IPT,80,0,IER)
      M=NCFIND(WORD,CHANGE,2)
      IF (M .LE. 0) GO TO 2
      GO TO (11,12) , M
C
C L TFAC:
  11  CALL LLTFAC(4)
      GO TO 1
C
C L SCAL:
  12  CALL LLSCAL(4)
      GO TO 1
C
C E CARD:
   5  CALL EXTINC(6,0.)
      GO TO 1
C
C OTHERWISE COPY CARD AS INPUT:
   2  WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
2000  FORMAT (80A1)
      GO TO 1
C
 101  IF (L .EQ. 10) CALL JMPOL
 100  RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE NWINSF(MAGNEW)
      SUBROUTINE NWINSF(MAGNEW)
C
C *** NWINSF updated by PJB 31-May-94 ***
C
CX
CC 7B
CH Outputs a replacement Crystal Data File after single crystal refinement.
CA On entry MAGNEW is the name of a routine which writes a new Q card,
CA if magnetic, or does nothing if not.  This is to avoid loading any of
CA the magnetic code in non-magnetic examples.
CP An old Crystal Data File should be held on the scratch unit number IO10.
CP A cycle of LSQ refinement involving structure parameters should have
CP been done.
CD In general, writes the same number of "cards" as were on the original
CD Crystal Data File.  If there was originally no L SCAL card, makes one.
CD The new cards have the latest values of the refined parameters.
CO Outputs on unit NEWIN the updated Crystal Data File.
C
      EXTERNAL MAGNEW
      CHARACTER *4 WORD, CHANGE(3)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /DEPMAT/LDEP
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA CHANGE/'TFAC','SCAL','ATOM'/
C
      IF (SIMUL) GO TO 100
      CALL NEWCD
C SCAN CARDS ON SCRATCH, PRODUCING NEW ONES IF THEY MAY HAVE CHANGED:
      ID=0
   1  ID=ID+1
      IF (ID .GT. NTOTAL(1)) GO TO 101
      READ (IO10,REC=ID,FMT=1000) ICARD
1000  FORMAT(A80)
      L=LETTER(ICARD(1:1))
      IF (L .EQ. 12) GO TO 3
C TREAT A, F AND T ALIKE:
      IF (L .EQ. 1 .OR. L .EQ. 6 .OR. L .EQ. 20) GO TO 4
      IF (L .EQ. 5) GO TO 5
      IF (L .EQ. 17) GO TO 6
      IF (L .NE. 9) GO TO 2
C
C I CARD:
      CALL OTPUTI
      GO TO 1
C
C ANY FAMILY 2 PARAMETERS:
   4  CALL F2NEW(L)
      GO TO 1
C
C L CARD - THE OUTPUT OF THESE IS PROBLEM DEPENDENT:
   3  CALL RDWORD(WORD,LEN,3,IPT,80,0,IER)
      L=NCFIND(WORD,CHANGE,3)
      IF (L .LE. 0) GO TO 2
C
C CARD TO UPDATE:
      GO TO (11,12,13) , L
C
C L TFAC:
  11  CALL LLTFAC(4)
      GO TO 1
C
C L SCAL:
  12  CALL LLSCAL(4)
      GO TO 1
C
C L ATOM:
  13  CALL GEOMCO(3)
      GO TO 1
C
C E CARD:
   5  CALL EXTINC(6,0.)
      GO TO 1
C
C "Q" CARD:
   6  CALL MAGNEW(4)
      GO TO 1
C
C OTHERWISE COPY CARD AS INPUT:
   2  WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
2000  FORMAT (80A1)
      GO TO 1
C
 101  IF (IPRNT(7) .EQ. 0) GO TO 100
C DEPOSITED MATERIAL SOURCE FILE:H,K,L,FOBS SIGMA FOBS, FCALC:
      MESSAG='Deposited Material'
      NAMFIL='.DEP'
      CALL OPNFIL(LDEP,112)
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE NWINT2
      SUBROUTINE NWINT2
C
C *** NWINT2 updated by JCM 10 May 88 ***
C
CX
CC 7B
CH Writes out a new Crystal Data File for main program T2LSQ.
CP Should only be called in the context of T2LSQ, after a cycle
CP of refinement in which its parameters have been adjusted.
CD Copies out the file with new values for cell parameters and an
CD L ZERO card.
C
CO Writes the new file to unit NEWIN.
C
      CHARACTER *4 WD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /ZEROPT/ZERO,KZERO
C
C
      ID=0
   1  ID=ID+1
      IF (ID .GT. NTOTAL(1)) GO TO 100
      READ (IO10,REC=ID,FMT=1000) ICARD
1000  FORMAT (A80)
      L=LETTER(ICARD(1:1))
      IF (L .EQ.3) GO TO 3
      IF (L .EQ. 12) GO TO 11
      IF (L .NE. 9) GO TO 2
C
C OUTPUT NEW I CARD:
      CALL OTPUTI
      GO TO 1
C
C OUTPUT NEW C CARD WITH NEW VALUES:
   3  CALL CELNEW
      GO TO 1
C
C L CARD - DISCOVER IF "ZERO"
  11  CALL RDWORD(WD,LEN,3,IPT,80,0,IER)
      IF (WD .NE. 'ZERO') GO TO 2
      WRITE (NEWIN,2005) ZERO
2005  FORMAT ('L ZERO',F10.4)
      GO TO 1
C
C COPY UNCHANGED CARD:
   2  WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
2000  FORMAT (80A1)
      GO TO 1
C
 100  RETURN
      END
C
C
C
C
C LEVEL 5      LOGICAL FUNCTION ONCARD(C,WORD,A)
      LOGICAL FUNCTION ONCARD(C,WORD,A)
C
C *** ONCARD corrected by JCM 6 Mar 89 ***
C
CX
CC 13C
CH Finds a card which starts with the given letter and word, and reads
CH one number from it.
C
CA On entry C is the first letter of card sought,
CA          WORD is the A4 word to be found anywhere on any card starting
CA               with the given letter C
CA On exit  A is a real number, set (if the word was found) to the number
CA               following WORD.
CD ONCARD is set .TRUE. if the word is found, otherwise .FALSE.
C
      CHARACTER *1 C
      CHARACTER *4 WORD,WD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IINFO/IIN,ACOEFF(20)
      COMMON /IINFOW/IIREAD(20)
      CHARACTER *4 IIREAD
C
      ONCARD=.TRUE.
      IF (C .NE. 'I') GO TO 1
C I IS SPECIAL:
      IF (INREAD(9) .GT. 0) CALL INPUTI
      DO 2 I=1,IIN
      IF (WORD .EQ. IIREAD(I)) GO TO 3
   2  CONTINUE
      GO TO 101
C
   3  A=ACOEFF(I)
      GO TO 100
C
C OTHER THAN I:
   1  L=LETTER(C)
      ID = IABS(INREAD(L))
      NCARD=ICDNO(L)
      IF (NCARD .LE. 0) GO TO 101
      DO 4 I=1,NCARD
      CALL CARDIN(ID)
      ID=ID+NYZ
      IPT=3
C ALLOW ANYTHING TO BE A WORD - THIS MAY HIT AGAINST > 10 CHARS
   5  CALL RDWORD(WD,LEN,IPT,IPT,80,-42,IER)
      IF (WD .EQ.WORD) GO TO 6
      IF (IER .NE. 100 .AND. IPT .LE. 80) GO TO 5
   4  CONTINUE
      GO TO 101
C
C FOUND WORD - READ NUMBER:
   6  CALL RDREAL(A,IPT,IPT,80,IER)
      GO TO 100
C
 101  ONCARD=.FALSE.
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE OPNFIL(L,M)
      SUBROUTINE OPNFIL(L,M)
C
C *** OPNFIL updated by JCM 1 Aug 88 ***
C
CX
CC 13C
CH Opens file L according to requirements given in M;  L may be preset.
C
CA If on input L=-9999, behaves as L=NOPFIL(M)
CA If L >= 0, behaves similarly, but uses unit number L
CA Sets L=unit number used
CA For meanings of M, see NOPFIL
C
CO Opens unit (either L or chosen from table in IOTAB in /LOONEY)
CO Fills in tables in /LOONEY and /FINAME
C
      LOGICAL HASNAM,ISOPEN
      CHARACTER *40 FILTEM
      COMMON /FINAME/FILNAM(15)
      CHARACTER *10 FILNAM
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LOONEY/IOTAB(15),LUNTAB(15)
C
      IF (L .NE. -9999) GO TO 1
      L=NOPFIL(M)
      GO TO 100
C
C REQUEST TO OPEN SPECIFIC UNIT NUMBER:
C
C L MUST BE IN TABLE SO THAT NOPFIL CAN FIND IT:
   1  LL=0
      K=0
C%
C      DO 2 I=1,%FILE%
      DO 2 I=1,15
C SET LL TO POINT TO FOUND L:
      IF (LUNTAB(I) .EQ. L) LL=I
C SET K TO POINT TO FIRST VACANT SLOT:
      IF (IOTAB(I) .EQ. 0 .AND. K .EQ. 0) K=I
   2  CONTINUE
      IF (LL .EQ. 0) GO TO 3
C L WAS ALREADY THERE - MAKE IT THE NEXT FREE:
      ITM=LUNTAB(K)
      LUNTAB(K)=L
      IOTAB(K)=IOTAB(LL)
      LUNTAB(LL)=ITM
      IOTAB(LL)=0
      FILTEM=FILNAM(LL)
      FILNAM(LL)=FILNAM(K)
      FILNAM(K)=FILTEM
      GO TO 4
C
C L NOT IN TABLE - CHECK SUITABILITY:
   3  IF (L .LT. 0 .OR. L .EQ. ITI .OR. L .EQ. ITO) CALL ERRIN2(
     &  L,0,'cannot open unit','in OPNFIL')
      LUNTAB(K)=L
   4  CONTINUE
CVMS
      INQUIRE (L,NAMED=HASNAM,NAME=FILTEM,OPENED=ISOPEN)
CVMS
      IOTAB(K)=0
CVMS
      IF (.NOT. ISOPEN) GO TO 7
CVMS
      IOTAB(K)=M
CVMS
      IF (HASNAM) FILNAM(K)=FILTEM
      GO TO 100
C NOTE TO PRUNE FILTEM IF NECESSARY
   7  L1=NOPFIL(M)
      IF (L1 .EQ. L) GO TO 100
      WRITE (LPT,3001) L,L1
      WRITE (ITO,3001) L,L1
3001  FORMAT (/' *** PROGRAM ERROR ** OPNFIL CALL OF NOPFIL HAS',
     & ' RETURNED ',2I5)
C>> JCC Handle the STOP through an extra function
C Was
C     STOP
C Now
      CALL BMBOUT
      RETURN
C
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE OPSYM(ISYM)
      SUBROUTINE OPSYM(ISYM)
C
C *** OPSYM by JCM 11 Apr 83 ***
C
CX
CC 1B
CH Prints out the symmetry operators in either real or reciprocal space.
CA If ISYM=1  Prints out the NLAT non-primitive lattice translations and
CA            the NOPC coordinates of general equivalent positions held in
CA            ALAT and SYM in COMMON /SYMDA/
CA If ISYM=2  The printing is required in reciprocal space, and equivalent
CA            reflection indices and their relative phases are printed.
CD The sequence numbers of the operators are printed.  These are useful
CD for referring to specific operators.
C
CO The relevant operator list is written to unit LPT.
C
      CHARACTER *1 ISIG
      CHARACTER *15 IPH
      CHARACTER *1 LET(3,2),IICHAR(3,9),INUM(3),IMID(3),IDEN(3)
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/TSYM(3,3,%SY*2%),TTRANS(3,%SY*2%),
      COMMON /SCRAT/TSYM(3,3,48),TTRANS(3,48),
C%
C     & MLTAB(%SY*2%,%SY*2%),TRANS1(3),TEMSYM(3,3)
     & MLTAB(48,48),TRANS1(3),TEMSYM(3,3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C SET UP X Y Z AND H K L IN ARRAY LET:
      DO 12 I=1,3
      LET(I,1)=LETLOW(I+23)
  12  LET(I,2)=LETLOW(I+9)
      LET(1,2)=LETLOW(8)
C
C SET + OR +-
      ISIG=ISPCE
      IF (CENTRC .OR. ((ISYM .EQ. 2) .AND. FRIEDL)) ISIG='-'
C
C HEADING FOR RECIPROCAL SPACE:
      IF (ISYM .EQ. 2) THEN
        WRITE (LPT,2000) ISIG
2000    FORMAT (/' Equivalent reflections are:  +',A1,20X,'with  ',
     &   'relative phases 2pi times:')
      ELSE
C
C HEADING FOR REAL SPACE:
        CALL MESS(LPT,1,'General equivalent positions are:')
        DO 13 I=1,NLAT
        DO 14 J=1,3
        INUM(J)=ISPCE
        IMID(J)=IDIGIT(10)
        IDEN(J)=ISPCE
C IN CASE IT IS ZERO
C
C NOW PREPARE PRINTING OF FRACTION IN LATTICE VECTOR
C SET 'CONSIDER ONLY DENOMINATORS OF 2,3,4,6'
        CALL NUMDEN(ALAT(J,I),NNUM,NDEN,1,IRR)
        IF (IRR .EQ. 0) GO TO 14
        IF (IRR .NE. 1) GO TO 15
C
        INUM(J)=IDIGIT(NNUM)
        IDEN(J)=IDIGIT(NDEN)
        IMID(J)='/'
  14    CONTINUE
        WRITE (LPT,2002) (INUM(J),IMID(J),IDEN(J),J=1,3),ISIG
2002    FORMAT (2X,2(4X,3A1,13X),4X,3A1,4X,'  +',A1)
        GO TO 13
C
C IN CASE ELEMENT OF LATTICE VECTOR NOT A FRACTION:
  15    WRITE (LPT,2003) (ALAT(J,I),J=1,3),ISIG
2003    FORMAT (2X,2(F10.4,10X),F10.4,4X,'  +',A1)
  13    CONTINUE
        WRITE (LPT,2004)
2004    FORMAT (1X)
C
      ENDIF
      DO 1 NO = 1,NOPC
C IP COUNTS ACROSS ARRAY OF CHARACTERS FOR PHASE, IF RECIPROCAL
      IP = 0
      IF (ISYM .EQ. 2) THEN
C
C IF RECIPROCAL SPACE, USE INVERSE MATRIX TRANSPOSED, AND THE NEGATED
C TRANSLATION VECTOR CORRESPONDING TO THAT MATRIX:
        DO 19 I=1,3
  19    TRANS1(I)=-TRANS(I,INVERS(NO))
        CALL GMEQ(SYM(1,1,INVERS(NO)),TEMSYM,3,3)
        CALL TRANSQ(TEMSYM,3)
      ELSE
C
C IF REAL SPACE , USE OPERATOR AS STORED:
        CALL GMEQ(TRANS(1,NO),TRANS1,1,3)
        CALL GMEQ(SYM(1,1,NO),TEMSYM,3,3)
      ENDIF
      DO 2 J=1,3
      L = 1
      CALL FRACT(TRANS1(J),ATEMP,ITEMP)
      IF (TRANS1(J) .GE. 0.00001) THEN
        DO 3 M=2,6
        X = FLOAT(M)*TRANS1(J) + 5.E-5
        IF (AMOD(X,1.) .GT. .0001) GO TO 3
        IQ = INT(X)
        IICHAR(J,1) = IDIGIT(IQ)
        IICHAR(J,2) = '/'
        IICHAR(J,3) = IDIGIT(M)
        L = 4
        GO TO 5
    3   CONTINUE
      ENDIF
C
    5 DO 8 M=1,3
      IF (TEMSYM(J,M) .EQ. 0) GO TO 8
      IF (TEMSYM(J,M) .LT. 0) THEN
        IICHAR(J,L) = '-'
      ELSE
        IICHAR(J,L) = '+'
         IF ((L .EQ. 1) .OR. ((ISYM .EQ. 2) .AND. (L .EQ. 4)))
     &    IICHAR(J,L)=ISPCE
      ENDIF
      IICHAR(J,L+1) = LET(M,ISYM)
      L = L+2
    8 CONTINUE
C
      IF (L .NE. 10) THEN
        DO 4 K=L,9
    4   IICHAR(J,K) = ISPCE
      ENDIF
      IF (ISYM .NE. 2) GO TO 2
      IF (MOD(L,2) .NE. 0) GO TO 2
      DO 30 IJ = 1,3
      IP = IP+1
      IPH(IP:IP) = IICHAR(J,IJ)
      DO 30 K = IJ,6,3
   30 IICHAR(J,K) = IICHAR(J,K+3)
      IP = IP+2
      IPH(IP-1:IP-1) = LET(J,2)
      IPH(IP:IP) = '+'
    2 CONTINUE
C
      IF (ISYM .EQ. 2) THEN
        IF (IP .LE. 0) THEN
          IPH(1:1) = IDIGIT(10)
          IP = 2
        ENDIF
        IPH(IP:15) = ' '
C
        WRITE (LPT,2006)NO,((IICHAR(J,K),K=1,6),J=1,3),IPH(1:14)
2006    FORMAT (1X,I2,3(5X,6A1,6X),10X,A14)
      ELSE
        WRITE (LPT,2005) NO,((IICHAR(J,K),K=1,9),J=1,3)
2005    FORMAT(1X,I2,3(5X,9A1,6X))
      ENDIF
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE ORTFUN(I,KMAX,VAL,MODE)
      SUBROUTINE ORTFUN(I,KMAX,VAL,MODE)
C
C *** ORTFUN updated by PJB/JBF 4 Sep 89 ***
C
CX
CC 18A
CH Finds the best set of orthonormal functions compatible with symmetry
CH based on the users input, and hence defines the multipoles to refine.
CA On entry  KMAX = 2l+1, VAL the initial amplitude of function I
CA  MODE = 1 First function - set up
CA       > 1 Try function I
CA       = 0 No more functions - tidy up
C
      CHARACTER*4 NAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON/SCRAT2/OROT(3,3,2),ISUB(24),MATP,ORMAT(169),
     & FUN(169),ORT(169),ANOR(13),USED(13),INDX(13),JNDX(13),DIJ(196)
      LOGICAL USED,FLAG
      COMPLEX DIJ
C
      IF (MODE.EQ.0) GO TO 30
      IF (KMAX.EQ.1) THEN
        NEW=1
        JJ=1
        INDX(1)=1
        GO TO 99
      ENDIF
      II=KMAX*(I-1)
      IF (MODE.EQ.1) THEN
C  INITIALISE TABLES
        JJ=0
        DO 9 J=1,KMAX
        INDX(J)=0
        JNDX(J)=0
        USED(J)=(ABS(FUN(KMAX*(J-1)+J)).LT. 0.0001)
    9   CONTINUE
      ENDIF
        IF (ABS(FUN(KMAX*(I-1)+I)).LT. 0.0001)  THEN
        CALL NAMPOL(NAME,KMAX,I)
        CALL MESS(LPT,0,'Amplitude of '//NAME//' is zero from symmetry')
      ENDIF
      IF (USED(I)) THEN
      NEW=0
      GO TO 99
      ENDIF
      USED(I)=.TRUE.
      IF (MODE.EQ.1) THEN
        CALL GMEQ(FUN(II+1),ORT,KMAX,1)
        JJ=1
        INDX(1)=I
        JNDX(I)=1
        NEW=1
        GO TO 99
      ENDIF
C
C  ATTEMPT TO INSERT A GIVEN VECTOR
      AMIN=0.
      DO 10 J=1,JJ
      AMIN=AMIN+ORMAT(I+KMAX*(INDX(J)-1))**2
   10 CONTINUE
      IMIN=I
      GO TO 1
C
C  COMPLETE THE ORTHONORMAL SET
   30 IF (KMAX.EQ.1) THEN
        NCMAT=NCMAT+1
        CONMAT(NCMAT,1)=1.
        CONMAT(NCMAT,2)=1.
        NCLUMP=NCLUMP+1
        KCLUMP(NCLUMP)=1
        GO TO 100
      ENDIF
      JJIN=JJ
   31 IF (JJ.EQ.KMAX) GO TO 20
      FLAG=JJ.EQ.0
      AMIN=FLOAT(KMAX)
      DO 2 I=1,KMAX
      IF (USED(I)) GO TO 2
      FLAG=.TRUE.
      VAL=0.
      DO 3 J=1,JJ
      VAL=VAL+ORMAT(I+KMAX*(INDX(J)-1))**2
    3 CONTINUE
      IF (VAL.GT.AMIN) GO TO 2
      AMIN=VAL
      IMIN=I
    2 CONTINUE
      IF (.NOT. FLAG) GO TO 20
C
    1 ANORM=(1.-AMIN)
C      IF (ANORM.LT.-.002) STOP 'Error in Normalisation'
C>> JCC Handle the STOP through an extra function
C Was
C     IF (ANORM.LT.-.002) STOP 'Error in Normalisation'
C Now
      IF (ANORM.LT.-.002) THEN
            WRITE(LPT,*) 'Error in Normalisation'
            WRITE(ITO,*) 'Error in Normalisation'
            CALL BMBOUT
            RETURN
      ENDIF

      IF (ANORM.LE..0002) THEN
        ANORM=0.
        GO TO 20
      ENDIF
      INDX(JJ+1)=IMIN
      JNDX(IMIN)=(JJ+1)
      USED(IMIN)=.TRUE.
      ANORM=1./SQRT(ANORM)
C
      JJ1=KMAX*(JJ)
      IJJ1=KMAX*(INDX(JJ+1)-1)
      II=0
      DO 4 I=1,KMAX
      ORT(I+JJ1)=FUN(I+IJJ1)
      DO 5 J=1,JJ
      IJ=KMAX*(J-1)
      ORT(I+JJ1)=ORT(I+JJ1)-ORT(I+IJ)*ORMAT(INDX(J)+IJJ1)
    5 CONTINUE
      ORT(I+JJ1)=ORT(I+JJ1)*ANORM
    4 CONTINUE
      DO 6 I=1,KMAX
      CALL GMPRD(ORT(1+JJ1),FUN(1+II),ORMAT(I+IJJ1),1,KMAX,1)
      II=II+KMAX
    6 CONTINUE
      IF (IOUT.GT.160) THEN
        L=0
        WRITE (LPT,4001) (ORT(JJ1+I),I=1,KMAX)
4001    FORMAT (/' New function: '/13F9.4//' New product matrix:')
        DO 600 I=1,KMAX
        WRITE (LPT,4002) JNDX(I),(ORMAT(L+J),J=1,KMAX)
4002    FORMAT (I4,13F9.4)
        L=L+KMAX
  600   CONTINUE
      ENDIF
C
      JJ=JJ+1
      IF (MODE.EQ.0) GO TO 31
      NEW=1
      GO TO 99
C
  20  IF (MODE.NE.0) THEN
        NEW=0
        GO TO 99
      ENDIF
      NEW=JJ-JJIN
      IF (IOUT.GT.120) THEN
        CALL MESS(LPT,1,'Orthonormal representation:')
        L=0
        DO 12 J=1,JJ
        WRITE (LPT,4004) INDX(J),(ORT(L+I),I=1,KMAX)
4004    FORMAT (1X,I5,12F8.4)
        L=L+KMAX
   12   CONTINUE
      ENDIF
C  CHECK THAT CONMAT WILL NOT OVERFLOW
      IF (NCMAT+JJ*JJ .GT.600) THEN
        CALL ERRMES(1,1,
     &  'Multipole matrix will overflow - increase dimension of CONMAT')
        GO TO 100
      ENDIF
      JK=JJ
      DO 21 I=1,KMAX
      IF (JNDX(I).NE.0) GO TO 21
      JK=JK+1
      INDX(JK)=I
   21 CONTINUE
C
C  DERIVE MATRICES RELATING PARAMETERS TO ACTUAL MULTIPOLES
      DO 40 I=1,JJ
      L=I
      II=KMAX*(INDX(I)-1)
      DO 41 J=1,JJ
      CONMAT(NCMAT+L,1)=FUN(INDX(J)+II)*ANOR(INDX(I))
      L=L+JJ
   41 CONTINUE
   40 CONTINUE
      CALL GMINV(CONMAT(NCMAT+1,1),CONMAT(NCMAT+1,2),JJ)
      IF (IOUT.GT.50) THEN
        WRITE (LPT,4010) (INDX(I),I=1,JJ)
 4010   FORMAT (' Inverse of Amount matrix:'/6X,12(I5,3X))
        L=0
        DO 42 I=1,JJ
        WRITE (LPT,4004) INDX(I),(CONMAT(NCMAT+L+J,1),J=1,JJ)
        L=L+JJ
   42   CONTINUE
        WRITE (LPT,4011) (INDX(I),I=1,JJ)
 4011   FORMAT (' Amount matrix:'/6X,12(I5,3X))
        L=0
        DO 44 I=1,JJ
        WRITE (LPT,4004) INDX(I),(CONMAT(NCMAT+L+J,2),J=1,JJ)
        L=L+JJ
   44   CONTINUE
      ENDIF
      NCMAT=NCMAT+JJ*JJ
      VAL=0.
C
   99 II=JJ-NEW
      DO 98 J=1,NEW
C%
C      CALL ERRCHK(2,NMPOL,%MPOL%,0,'multipoles')
      CALL ERRCHK(2,NMPOL,200,0,'multipoles')
      POLAMP(NMPOL,1)=VAL
      CALL NAMPOL(MPNAM(NMPOL),KMAX,INDX(II+J))
   98 CONTINUE
      IF (MODE.NE.0) GO TO 100
      LVAL=KMAX/2
      ISTART=NMPOL-JJ+1
C
C RECORD NEW CHUNK:
C%
C      CALL ERRCHK(2,NCLUMP,%MBLK%,0,
      CALL ERRCHK(2,NCLUMP,100,0,
     &  'blocks of l values for multipoles')
      KCLUMP(NCLUMP)=JJ
      IF (JJ.NE.1) THEN
        WRITE (LPT,2001) JJ,LVAL,(MPNAM(J),J=ISTART,NMPOL)
      ELSE
        WRITE (LPT,2002) LVAL,(MPNAM(J),J=ISTART,NMPOL)
      ENDIF
 2001 FORMAT (' There are',I2,' independent functions for l =',I2/
     & ' They are obtained from:', 13(2X,A4))
 2002 FORMAT (' There is one independent function for l =',I2/
     & ' It is obtained from:', 13(2X,A4))
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE ORTHG(IOP)
      SUBROUTINE ORTHG(IOP)
C
C *** ORTHG updated by JCM 11 Aug 88 ***
C
CX
CC 1A
CH Calculates matrices for the transformation of vectors in real or reciprocal
CH space, between crystallographic and orthogonal axes.
CA On entry IOP=1 means do not print result; =2 means print
CD The orthogonal set are defined as follows:
CD    X is parallel to a*
CD    Z is parallel to c
CD    Y makes up a right handed set.
CD
CD Thus H(orth) = h(cryst) times matrix ORTH.
CD H is is real space for matrix ORTH(,,1), and reciprocal for (,,2)
C
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      DO 5 M=1,2
      DO 3 I = 1,3
      DO 3 J = 1,3
    3 ORTH(I,J,M) = 0.
      N=3-M
      J=5-2*M
      K=2*M-1
      ORTH(J,J,M) = CELL(J,1,M)
      ORTH(2,J,M) = CELL(K,2,M)*CELL(2,1,M)
      ORTH(2,2,M) = CELL(K,3,M)*CELL(2,1,M)
      ORTH(K,J,M) = CELL(2,2,M)*CELL(K,1,M)
      ORTH(K,2,M) = -CELL(2,3,M)*CELL(J,2,N)*CELL(K,1,M)
      ORTH(K,K,M) = CELL(2,3,M)*CELL(J,3,N)*CELL(K,1,M)
   5  CONTINUE
      IF (IOP .EQ. 2) WRITE (LPT,2000) ORTH
2000  FORMAT (/' Matrices for transformation of vectors to',
     & ' orthogonal axes'/' Real space:'/3(3F10.4/),
     & /' Reciprocal space:'/3(3F10.4/))
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ORTHO(H,OH,IR)
      SUBROUTINE ORTHO(H,OH,IR)
C
C *** ORTHO corrected by PJB 25 Jun 86 ***
C
CX
CC 1B
CH Carries out conversions between crystallographic and orthogonal axes.
CA H(3) is the input vector
CA OH(3) is the transformed vector
CA IR indicates which conversion is required:
CD If IR=1  H is a real space vector on crystallographic axes and OH a vector
CD          on the standard orthogonal axes
CD    IR=2  H is a reciprocal vector on crystallographic axes and OH is on
CD          the orthogonal axes
CD    IR=-1 H is on orthogonal axes and OH on real crystallographic axes
CD    IR=-2 H is on orthogonal axes and OH on reciprocal crystallographic axes
CP RECIP should have read the cell  dimensions.
CN The standard orthogonal axes are defined with x parallel to (100)
CN z parallel to [001] and y making up a righ-handed orthogonal set.
CN The matrices used in the conversions are those printed out by RECIP
C
      DIMENSION H(3),OH(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
C
      IF (IR .GT. 0) CALL GMPRD(H,ORTH(1,1,IR),OH,1,3,3)
      IF (IR .LT. 0) CALL GMPRD(ORTH(1,1,3+IR),H,OH,3,3,1)
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE OTPUTI
      SUBROUTINE OTPUTI
C
C *** OTPUTI updated by JCM 14 Jul 86 ***
C
CX
CC 6B
CH Outputs a new I card after a LSQ refinement, updating the cycle number.
CO Writes the new card to unit NEWIN.
C
      CHARACTER *80 ITEMP
      CHARACTER *4 WORD
      DIMENSION IDIG(5)
      COMMON /IINFOW/IIREAD(20)
      CHARACTER *4 IIREAD
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
C%
C      DO 1 I=1,%ICRD%
      DO 1 I=1,20
      IF (IIREAD(I) .EQ. 'CYC1') GO TO 2
   1  CONTINUE
      GO TO 8
C
C CYC1 READ:
C REPLACE BY CURRENTLY REQUIRED CYC1
   2  IPT1=2
C SCAN CARD, AS IT MAY NOT BE THIS ONE THAT CYC1 IS ON:
    5  CALL RDWORD(WORD,NTEMP,IPT1,IPT,80,0,IER)
      IF (IER .EQ. 100) GO TO 8
C IGNORE NUMBER:
      CALL RDREAL(A,IPT,IPT1,80,IER)
      IF (WORD .NE. 'CYC1') GO TO 5
C
C FOUND CYC1:
      ITEMP(1:IPT-1)=ICARD(1:IPT-1)
      CALL INTDIG(LASTCY+1,IDIG,NDIG)
      IF (NDIG .LT. IPT1-IPT) GO TO 7
C NUMBER HAS MORE DIGITS - CHECK CARD END - IF CARD HAPPENS TO BE FULL WE
C COULD USE ANOTHER CARD, OR SHUFFLE ALONG LOOKING FOR 2 SPACES, BUT WE SHALL
C WAIT UNTIL THIS HAPPENS:
      IF (ICARD(80:80) .EQ. ' ') GO TO 7
      CALL ERRMES(-1,0,'new I card cannot be written')
      GO TO 100
C
   7  CALL INTCHR(IDIG,NDIG,ITEMP(IPT:IPT),80-IPT,0)
      ITEMP(IPT+NDIG+1:80)=ICARD(IPT1:IPT1+IPT+NDIG+1)
      ICARD=ITEMP
   8  WRITE (NEWIN,2000) (ICARD(I:I),I=1,LENGT(ICARD))
2000  FORMAT (80A1)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PARITY(N,M,EVEN)
      SUBROUTINE PARITY(N,M,EVEN)
C
C *** PARITY by JCM 26 Sep 85
C
CX
CC 11C
CH Finds out whether N is odd or even.
CA On entry N is the integer to be tested
CA On exit  EVEN is true if N is even, false if N is odd
CA          If N is even M=N/2; if odd M=(N-1)/2
C
      LOGICAL EVEN
C
      M1=N/2
      M2=(N-1)/2
      EVEN=(M1 .NE. M2)
      IF (N .LE. 0) EVEN = .NOT. EVEN
      M=M2
      IF (EVEN) M=M1
      RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE PARNAM(IPNAM1,IPNAM2,N,M)
      SUBROUTINE PARNAM(IPNAM1,IPNAM2,N,M)
C
C *** PARNAM updated by JCM 8 May 90 ***
C
CX
CC 6C
CH Obtains the printing name of a LSQ parameter.
CA On entry N specifies what the integer M is:
CA      N=1 means M is the number of a basic variable
CA      N=2 means M is the number of a variable
CA      N=3 means M is a packed parameter specification
CA On exit IPNAM1 contains the A4 genus name,
CA         IPNAM2 contains the A4 species name
CP LSETUP must have put the vocabulary into /WORDS/ etc
CP VARMAK must have set up varible structure and pointers
CD On exit KPHASE in /PHASE/ and KSOURC in /SOURCE/ have been given the
CD current values of the phase and source.
CD From family, genus and species, decides source of genus and species names
CD A large, +ve value for the species type indicates only 1 name, not 2
CD Picks up names via PRIWRD
C
      CHARACTER *4 IPNAM1,IPNAM2
      LOGICAL ONENAM
      DIMENSION LPAK(5)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
      MM=M
      IPNAM1=' '
      IPNAM2=' '
      GO TO (1,2,3) ,N
C
C N=1 - M SPECIFIES BASIC VARIABLE - WHICH VARIABLE WAS IT?
   1  MM=LBSVR(MM)
C
C N=2 - M SPECIFIES VARIABLE - WHICH PARAMETER WAS IT?
   2  MM=LVRPR(MM)
C
C N=3 - M IS A PACKED PARAMETER - UNPACK IT:
   3  CALL KUNPAK(MM,IFAM,IGEN,ISPC,KPHASE,KSOURC)
C THIS MENDS NON-MULTI, BUR MAY NEED ADJUSTING FOR MULTI:
      IF (KPHASE .EQ. 0) KPHASE=1
      IF (KSOURC .EQ. 0) KSOURC=1
      ONENAM=(IGEN .EQ. 1 .AND. (IFAM .EQ. 1 .OR. IFAM .EQ. 6))
C
C BRANCH ON FAMILY:
      GO TO (11,12,13,14,15,16) , IFAM
      CALL ERRIN2(IFAM,0,'PARNAM asked for name in family',' ')
C
C FAMILY 3 - SET ISP TO BE ITS SPECIES TYPE:
  13  ISP=LF3SP(IGEN,KPHASE,KSOURC)
      GO TO 30
C
C FAMILY 4 - "VERY LONG VECTORS" - SET SPECIES TYPE:
  14  ISP=-NSPCPS(4,KPHASE)
      GO TO 30
C
C FAMILY 1 - SET ISP TO BE ITS SPECIES TYPE:
  11  ISP=LF1SP(IGEN)
      GO TO 30
C
C FAMILY 6 - SET ISP TO BE SPECIES TYPE:
  16  ISP=LF6SP(IGEN,KSOURC)
C
C
C COPY GENUS NAME:
  30  IF (ONENAM) THEN
        IPNAM1=' '
        GO TO 23
      ELSE
        CALL PRIWRD(IFAM,IGEN,0,IPNAM1,1)
      ENDIF
      IF (IPNAM1 .EQ. 'XXXX') GO TO 22
C
C PICK UP SPECIES NAME BY CONSULTING, FIRST, TO SEE WHAT SORT OF SPECIES.
C POSSIBILITIES FOR ISP ARE:
C    -VE MEANS SPECIES ARE POSITIVE INTEGERS, 1, 2 ETC, UP TO IABS(ISP)
C     0 MEANS THERE ARE IN FACT NO SPECIES
C    +VE MEANS THAT EACH SPECIES HAS A NAME (LIKE TFAC, ASYM, PROR  . . ,
C   OR X, Y, Z, B11 . . . OR A*, B*, . . .) TOBE FOUND IN THE GENERAL WORD TABLE
      IF (ISP) 21,22,23
C
C IF 0, COMPLAIN.
  22  WRITE (LPT,3001)M,N,IFAM,IGEN,ISPC
      WRITE (ITO,3001)M,N,IFAM,IGEN,ISPC
3001   FORMAT (/' ERROR ** PARNAM CANNOT FIND NAME - M,N,IFAM,',
     & 'IGEN,ISPC =',5I6)

C
C IF +VE, NAME ALREADY PACKED IN MM FOR LOOK-UP IN PRIWRD:
  23  CALL PRIWRD(IFAM,IGEN,ISPC,IPNAM2,0)
C MAY NOT ALL BE PRESENT - TRY JUST SPECIES NAME IF NOT FOUND:
      IF (IPNAM2 .NE. 'XXXX') GO TO 100
      CALL PRIWRD(IFAM,0,ISPC,IPNAM2,0)
      IF (IPNAM2 .EQ. 'XXXX') GO TO 22
      GO TO 100
C
C IF -VE WANT TO TURN ISPC INTO DIGITS:
  21  CALL INTDIG(ISPC,LPAK,NDIG)
      CALL INTCHR(LPAK,NDIG,IPNAM2,4,0)
      GO TO 100
C
C FAMILY 2 IS SPECIAL BECAUSE ITS GENUS NAMES ARE ATOM NAMES:
  12  IF (MULFAS) THEN
        IPNAM1=ATNA(IGEN,KPHASE)
      ELSE
        IPNAM1=ATNAME(IGEN)
      ENDIF
C    AS IN FAMILY 1, FOR THE SPECIES WE HAVE POINTERS INTO /WORDS/.
      CALL PRIWRD(IFAM,0,ISPC,IPNAM2,0)
      GO TO 100
C
C FAMILY 5 - EXPECT 1 LONG GENUS AT FIRST:
  15  CALL MF5ADD(ISPC,IG,IS,-1)
C GENUS NAME IS ATOM NAME : IG AT PRESENT IS MULTIPOLE ATOM NAME:
      IF (MULFAS) THEN
        IPNAM1=ATNA(MPATAB(IG),KPHASE)
      ELSE
        IPNAM1=ATNAME(MPATAB(IG))
      ENDIF
C SPECIES NAME COMES FROM TABLE:
      IPNAM2=MPNAM(ISPC)
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE PARRD(IPT1,IPT2,K,IFAM,IGEN,ISPC)
      SUBROUTINE PARRD(IPT1,IPT2,K,IFAM,IGEN,ISPC)
C
C *** PARRD updated by JCM 3 Aug 92 ***
C
CX
CC 6C
CH Reads a LSQ parameter specification from a given card at given point.
CA On entry IPT1 points to the starting character on the card in /SCRACH/
CA On exit  IPT2 points to the next unread character on the card in /SCRACH/
CA          K is the packed parameter unless K is -ve:
CA K=-99 'ONLY' read
CA K=-100 'ALL' read as first word, in which case IFAM is expected to
CA                 be set, and possibly one but not both of ISPC and IGEN
CA K=-100 and IFAM -ve means that the word after 'ALL' was composite
CA        Composite words have come from MAIN program table with -ve small
CA        entries.  For SFLSQ and the like,
CA      IFAM=-1 'ALL XYZ'
CA      IFAM=-2 'ALL BIJ'
CA      IFAM=-3 'ALL XYZT'
CA      IFAM=-4 'ALL CELL'
CA      IFAM=-5 'ALL XYZB'
CA      IFAM=-6 'ALL XYZS'
C
CA K=-101  'XYZ' as 2nd word;  1st word was genus in IGEN
CA K=-102  'BIJ' as 2nd word;  1st word was genus in IGEN
CA K=-103  'XYZT' as 2nd word;  1st word was genus in IGEN
CA K=-104  'CELL' as 2nd word;  1st word was genus in IGEN
CA K=-105  'XYZB' as 2nd word;  1st word was genus in IGEN
CA K=-106  'XYZS' as 2nd word;  1st word was genus in IGEN
C
CP The tables used must be set up by LSETUP
C
CD PARRD expects only a limited vocabulary, identified via TBLFND, being
CD    a known genus followed by a species name
CD    'ONLY'
CD    'ALL' followed by anything making sense
CD    a genus name followed by a composite word like 'XYZ', 'BIJ', 'XYZT' 'XYZB'
CD    a species name alone, in the special case of family 1 or 6, genus 1
CD    (and for PR, if SCAL is read, ignores a 1 if it follows) -
CD    wherever it makes sense.
C
      CHARACTER *4  LWD1,LWD2
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
C IN CASE MULTI, SET SOURCE AND PHASE AS DEFAULT:
      KPHASE=JPHASE
      KSOURC=JSOURC
C
C IDENTIFY FIRST NAME - READ AND OFFER TO TBLFND:
      K=0
      IFAM=0
      IGEN=0
      ISPC=0
C THIS CALL MAY FLIP KSOURC OR KPHASE:
      CALL RDWORD(LWD1,L1,IPT1,IPT,80,1,IER)
C IF EMPTY WORD, END OF LINE:
      IF (IER .EQ. 100) GO TO 100
C
C
C FIND WORD IF POSSIBLE:
      CALL TBLFND(LWD1,K,IFAM1,IGEN1,ISPC1,KP,KS)
C RECORD ANY PHASE OR SOURCE INFORMATION GLEANED BY TBLFND:
      IF (KP .NE. 0) KPHASE=KP
      IF (KS .NE. 0) KSOURC=KS
C IF K IS ZERO, TBLFND COULD NOT RECOGNISE AT ALL:
      IF (K .EQ. 0) GO TO 99
C
C IF TBLFND FOUND WORD, IT HAS A PARALLEL TABLE OF INTEGERS SAYING WHAT THE
C WORDS ARE.  NEGATIVE K INDICATES A NON-PARAMETER WORD (LIKE ALL, ONLY
C - THESE TWO ARE THE ONLY ONES ALLOWED AS FIRST WORD)
C
C IF K +VE, TBLFND HAS FOUND EITHER A GENUS OR A SPECIES NAME.  IF SPECIES,
C SHOULD BELONG TO FAMILY 1 GENUS 1, AND TBLFND SHOULD HAVE DETECTED THIS
C AND SET GENUS AND FAMILY.
C
C SO ACCEPTABLE FIRST WORDS ARE:
C     A SPECIES NAME (IMPLICITLY FOR FAMILY1, GENUS 1 - READ NO MORE)
C     A GENUS NAME (SETTING IFAM & IGEN, EXPECTS TO READ SPECIES, OR BIJ OR
C                   XYZ ETC. NEXT)
C     'ALL' (EXPECTS ANOTHER WORD)
C     'ONLY' ( READ NO MORE)
C
C 'ONLY'
      IF (K .EQ. -99) GO TO 101
C
C SPECIES NAME - SPECIFICATION COMPLETE IF FAM=1 OR 6, GEN=1:
      IF ((IFAM1 .EQ. 1 .OR. IFAM1 .EQ. 6) .AND. IGEN1 .EQ. 1
     &   .AND. ISPC1 .NE. 0) THEN
        IFAM=IFAM1
        IGEN=IGEN1
        ISPC=ISPC1
C IF SCAL IGNORE ANY SUBSEQUENT 1:
        IF (IFAM .EQ. 6) THEN
          CALL RDINTG(I,IPT,IPT2,80,IER)
          IF (I .EQ. 1) IPT=IPT2
        ENDIF
        GO TO 102
      ENDIF
C
C CHECK ENOUGH INFO TO PROCEED WITH SECOND WORD:
      IF (K .LT. -102) GO TO 99
      IF ((IFAM1.EQ.0 .OR. IGEN1.EQ.0) .AND. (K .GT. 0)) GO TO 99
C
C NOW WE LOOK AT SECOND WORD AND CHECK COMPATIBLE WITH FIRST (-1 ON INPUT
C ALLOWS AN INTEGER TO BE READ AS A WORD):
      CALL RDWORD(LWD2,L2,IPT,IPT,80,-1,IER)
      IF (IER .EQ. 100) GO TO 99
      CALL TBLFND(LWD2,IANS2,IFAM2,IGEN2,ISPC2,KP,KS)
      IF (KP .NE. 0) KPHASE=KP
      IF (KS .NE. 0) KSOURC=KS
C WORDS ALLOWED IN SECOND PLACE ARE:
C      A SPECIES NAME FITTING THE FIRST GENUS NAME
C      A COMPOSITE WORD FOLLOWING 'ALL' OR A GENUS NAME - THE COMPOSITE WORDS
C                  ARE 'BIJ', 'XYZ' OR 'XYZT' ETC. FOR SFLSQ
C      SOMETHING ELSE FOLLOWING 'ALL' - THIS MAY BE A FAMILY NAME,
C                  A GENUS NAME OR A SPECIES NAME
C
C ALL COMPOSITE WORDS ARE ALLOWED EXCEPT 'ALL' AND 'ONLY':
      IF (IANS2 .EQ. -100 .OR. IANS2 .EQ. -99 ) GO TO 99
C
C IF TBLFND COULD NOT FIND IT, IT MAY BELONG TO A USER'S TABLE IN FAMILY 5:
      IF (IANS2 .EQ. 0) THEN
C BUT ONLY IF MULTIPOLE:
        IF (.NOT. MPL) GO TO 99
        IF (IFAM1 .NE. 2) GO TO 99
        IFAM1=5
        IG=MPNMTB(IGEN1)
        IGEN1=1
        ISPC2=NCFIND(LWD2,MPNAM(MPTAB(IG)),MPTAB(IG+1)-MPTAB(IG))
        IF (ISPC2 .EQ. 0) GO TO 99
C COUNT FROM BEGINNING OF TABLE:
        ISPC2=ISPC2+MPTAB(IG)-1
        GO TO 5
      ENDIF
C
C IF FIRST WORD IS 'ALL':
      IF (K .EQ. -100) THEN
        IF (IANS2 .LT. 0) GO TO 4
C
C 'ALL' FOLLOWED BY GENUS OR SPECIES:
        IFAM=IFAM2
        IGEN=IGEN2
        ISPC=ISPC2
        GO TO 101
C
C BOTH WORDS ARE COMPOSITE (EG 'ALL BIJ', 'ALL XYZ')
   4    IFAM=IANS2
        GO TO 101
      ENDIF
C
C FIRST IS NOT 'ALL', SO SHOULD HAVE BEEN GENUS:
      IF (IANS2 .GT. 0) GO TO 5
C GENUS FOLLOWED BY 'XYZ' OR 'BIJ' OR 'XYZT'
      K=IANS2-100
   1  IFAM=IFAM1
      IGEN=IGEN1
      ISPC=ISPC1
      GO TO 101
C
C CANNOT MAKE SENSE OF PARAMETER SPEC:
  99  CALL ERRIN2(IPT,2,'cannot read parameter at point',' ')
      IPT2=81
      GO TO 100
C
C GENUS FOLLOWED BY SPECIES:
   5  IFAM=IFAM1
      IGEN=IGEN1
      ISPC=ISPC2
 102  K=KPAK(IFAM,IGEN,ISPC,KPHASE,KSOURC)
 101  IPT2=IPT
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PARRUN(IFAM,IGEN,ISPC)
      SUBROUTINE PARRUN(IFAM,IGEN,ISPC)
C
C *** PARRUN updated by JCM 26 Oct 89 ***
C
CX
CC 6C
CH Controls the cycling over all parameters in LSQ (not for Profile Refinement).
C
CA On entry, if IFAM=0 the cycle is to be initialised
CA       and otherwise IFAM,IGEN,ISPC contain the "previous"  values
CA On exit  IFAM contains the family of the "next" parameter,
CA               or -1 if the end has been reached.
CA          IGEN contains the genus of the "next" parameter,
CA          ISPC contains the species of the "next" parameter.
C
CP LSETUP must have set up the structure of the problem in the arrays:
CP     NGENS(I)=number of genera in family I
CP     NSPC(I) =number of species in each genus of family I
CP     LF1SP(I)=number of species in family 1, genus I.
CN There is a similar routine in the Profile Refinement section, RUNPAR,
CN which deals also with the special family 4 parameters, and multi-phase
CN and multi-source data.
C
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
C
C INITIALISE ENTRY:
      IF (IFAM .EQ. 0) GO TO 5
C
C ENTRY TO ADVANCE:
      ISPC=ISPC+1
C CHECK NOT TOO MANY SPECIES FOR THIS FAMILY+GENUS:
   3  IF (ISPC .GT. NSPC(IFAM)) GO TO 2
C ALSO, FAMILY 1 HAS INDIVIDUAL GENERA OF DIFFERING LENGTHS:
      IF (IFAM .EQ. 1 .AND. ISPC .GT. IABS(LF1SP(IGEN))) GO TO 2
C
      GO TO 100
C
C NEXT GENUS:
   2  IGEN=IGEN+1
   1  ISPC=1
      IF (IGEN .LE. NGENS(IFAM)) GO TO 3
C NEXT FAMILY:
   5  IFAM=IFAM+1
   4  IGEN=1
C IN CASE NGENS(IFAM) = 0
      IF (IFAM .LE. NFAM) GO TO 1
      IFAM=-1
 100  RETURN
      END
C
C
C
C
C LEVEL 10      SUBROUTINE PARSDS
      SUBROUTINE PARSDS
C
C *** PARSDS updated by JCM 28 Jan 88 ***
C
CX
CC 7A
CH Collects together all parameter fixing and varying information for
CH LSQ refinement of cell parameters using d spacings.
C
CP The LSQ system must have been set up by LSETUP.  A "C" card with
CP cell parameters must have been read using RECIP.
C
CD Absorbs constraints due to symmetry for cell parameters, then reads
CD and interprets L FIX, LVARY and L RELA cards.
C
CI L FIX, LVARY and L RELA cards.
C
C ABSORB EXISTING CONSTRAINTS ON CELL PARAMETERS DUE TO SYMMETRY:
      CALL CELREL(1,1,1)
C
C READ ALL 'L RELA' CARDS:
      CALL RDRELA
C READ ALL 'L FIX' AND 'L VARY' CARDS:
      CALL RDFV
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PARSFW
      SUBROUTINE PARSFW
C
C *** PARSFW updated by JCM 12 Jan 88 ***
C
CX
CC 7A
CH An older routine to set up variables from parameters for FWLSQ.
CD All parameters are in fact both variables and basic variables, as there
CD      are no constraints on this problem
C
      CHARACTER *4 IPR1(8),IPR2(8)
      COMMON /CONSTR/JCONST,JROWPT(301),JCMAT(200),AMOUNT(200),
     & NEXTJ
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /FWVALS/NVALS,COEFFS(9)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
C
CTHIS AVOIDS USING VARMAK FOR NO REASON AT ALL OTHER THAN HISTORY.
C IT MAY NOT STILL WORK.
C
      JCONST=0
      LVARV=NVALS
      LVARB=NVALS
      IFAM=1
      IGEN=2
      DO 1 I=1,NVALS
      LBSVR(I)=I
      LVRBS(I)=I
      LVRPR(I)=KPAK(1,2,I,0,0)
   1  CONTINUE
C
      IF (LVARB .LE. 0) THEN
        CALL MESS(LPT,1,'No variables')
        GO TO 100
      ENDIF
      WRITE (LPT,2001) LVARB
2001  FORMAT (/'0',I5,' basic variables :'/)
      IC=0
      DO  4 IB=1,LVARB
      IC=IC+1
      CALL PARNAM(IPR1(IC),IPR2(IC),1,IB)
      IF (IC .LT. 8) GO TO  4
      WRITE (LPT,2002) (IPR1(J),IPR2(J),J=1,IC)
2002  FORMAT (' ',8(A4,1X,A4,2X))
      IC=0
   4  CONTINUE
      IF (IC .NE. 0)  WRITE (LPT,2002) (IPR1(J),IPR2(J),J=1,IC)
 100  RETURN
      END
C
C
C
C
C LEVEL 10      SUBROUTINE PARSSF(MAGPAR)
      SUBROUTINE PARSSF(MAGPAR)
C
C *** PARSSF updated by PJB 31-May-1994 ***
C
CX
CC 7B
CH Collects all parameter fix and vary information for single crystal LSQ.
C
CD Calls other routines to:
CD Collect all symmetry implied constraints
CD Read & interpret all L FIX, L RELA and L VARY cards.
C
CI L FIX, L VARY and L RELA cards.
      EXTERNAL MAGPAR
      DIMENSION ISPVEC(10)
      COMMON /EXTN/IEXTYP,DOMR,KDOMR,AMOSC,KMOSC,EXTCOR,CEXT(4),
     & DEXDFQ,DEXDRQ,DEXDGQ,LOREN,GAUSS
      LOGICAL LOREN,GAUSS
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /OVER/ITFAC,OTFAC(10),KOTFAC(10),NTFAC,JTFAC,KOM15
      EQUIVALENCE (TFAC,OTFAC(1)),(KTFAC,KOTFAC(1))
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
C
C IN FAM1 THERE MAY BE TFAC MADE EARLIER BECAUSE NO 'L TFAC' CARD, WHICH NOW
C WANTS FIXING:
      IFAM=1
      IGEN=1
      ISPC=1
      IF (NTFAC .EQ. 0) CALL LLTFAC(6)
C
C IF NO E CARD, FIX DOMR AND MOSC:
       IF (IEXTYP .EQ. 0) THEN
        DO 1 I=8,12
   1    CALL ADDFX5(1,1,I,1,1,4)
      ENDIF
C
C SET UP ALL FAMILY 2 RELATIONS DUE TO SYMMETRY:
      ISPVEC(1)=1
      ISPVEC(2)=4
      ISPVEC(3)=10
      CALL F2RELA(2,ISPVEC)
C AND IF MAGNETIC, DO RELATIONS BETWEEN MAGNETIC PARS ALSO:
      CALL MAGPAR(5)
C
C IF SLACK CONSTRAINTS, DO CELL RELATIONS:
      IF (NSLAK(1) .GT. 0) CALL CELREL(1,1,2)
C
C READ ALL 'L RELA' CARDS:
      CALL RDRELA
C READ ALL 'L FUDG' CARDS:
      CALL FUDGIN
C READ ALL 'L FIX AND 'L VARY' CARDS:
      CALL RDFV
      RETURN
      END
C
C
C
C
C LEVEL 7      COMPLEX FUNCTION PFCALC(H)
      COMPLEX FUNCTION PFCALC(H)
C
CX
CC 18B
CH Calculates the COMPLEX nuclear structure factor for the reflection H, using
CH a multipole expansion of the form factor.
C
      COMPLEX TERM,CRS,SUM1,HR,PSUM,PFACS(50)
      LOGICAL LATABS
      DIMENSION H(3),RH(3)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FCAL/FC,FCMOD,COSAL,SINAL,FCDERS(300),DERIVT(300)
      COMPLEX FC,DERIVT
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C CLEAR ANSWERS IN CASE ABSENT:
C
C  FC COLLECTS THE CONVENTIONAL STRUCTURE FACTOR, COMPLEX:
      FC=CMPLX(0.,0.)
C CLEAR MODULUS AND ANGLES:
      FCMOD=0.
      COSAL=0.
      SINAL=0.
C OUT IF ABSENT:
      IF (LATABS(H)) GO TO 100
C
C STHL = SIN THETA/LAMBDA
      STHL=VCTMOD(0.5,H,2)
C
C SET FIRST SCATTERING FACTOR:
      IFF=0
C
C CYCLE OVER INDEPENDENT ATOMS:
      DO 1 IR=1,NATOM
C
C  SET MP TO THE MULTIPOLE ATOM
      MP=MPNMTB(IR)
C
      SUM1=CMPLX(0.,0.)
C  CALCULATE RADIAL FORM-FACTORS
      CALL PFORMF(H,MP,PSUM,PFACS,0)
C
C INNER LOOP OVER SYMMETRY EQUIVALENTS:
   2  DO 3 IS=1,NOPC
      CALL ROTSYM(H,RH,IS,-1)
C CALCULATE NON-SPHERICAL FORM FACTORS
      CALL PFORMF(RH,MP,PSUM,PFACS,1)
      F1=TWOPI*(SCALPR(X(1,IR),RH)+SCALPR(TRANS(1,IS),H))
      ERS=ANITF(RH,IR)
C ANISOTROPIC T F (=1. IF NOT THERE) NEEDED SEPARATELY FOR LSQ:
      CRS=CEXP(CMPLX(0.,F1))*ERS
      TERM=CRS*PSUM
      ARS=REAL(TERM)
      BRS=AIMAG(TERM)
      SUM1=SUM1+TERM
C
   3  CONTINUE
C END OF INNERMOST CYCLE OVER SYMMETRY
C
C IF CENTROSYMMETRIC, COMPENSATE FOR USING ONLY HALF NUMBER OF OPERATORS:
      IF (CENTRC) SUM1=SUM1+CONJG(SUM1)
C
      FAC=AMULT(IR)*EXP(-TF(IR)*STHL*STHL)
      HR=FAC*SITE(IR)
C HR IS PRODUCT OF ATOM DEPENDENT BUT SYMMETRY INDEPENDENT FACTORS
      FC=FC + HR*SUM1
C
   1  CONTINUE
C END OF CYCLE OVER ATOMIC POSITIONS
C
C  TIDY FCALC AND COLLECT TRUE D(MODFC)/D(VARIABLE)
C
      A = REAL(FC)
      B = AIMAG(FC)
      FCMOD = SQRT(A*A+B*B)
      PFCALC=FC
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE PFORMF(H,MAT,PSUM,PFACS,KODE)
      SUBROUTINE PFORMF(H,MAT,PSUM,PFACS,KODE)
C
C *** PFORMF updated by PJB/JBF 4 Sep 89 ***
C
CX
CC 18B
CH Calculates radial form factors for multipole refinement.
CA On entry KODE gives the required action:
CA     = 0. Calculate radial form factors for atom MAT and put in POLFOR
CA     = 1. Return sum over multipoles in PSUM
C
      CHARACTER*1 ASIG
      DIMENSION H(3),OH(3),RH(3)
      COMPLEX Y(28),AI,PFACS(50),PSUM,FORMFA
      COMMON /CHOOSE/MFORTB(50)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /POLFOR/MPLFOR(20,6),NUMGEN,PFORFA(7),LMAX
      COMMON /QROT/ROT(3,3,10)
C
      IF (KODE.NE.0) GO TO 6
C  HERE TO CALCULATE RADIAL FORM FACTORS
      IFORM=MFORTB(MPATAB(MAT))
      AK = VCTMOD(0.5,H,2)
      AKK = FOURPI*AK
      CALL GMZER(PFORFA,7,1)
      LMAX=0
      DO 5 I = MPTAB(MAT),MPTAB(MAT+1)-1
      READ (MPNAM(I),10) L
   10 FORMAT (1X,I1)
      IF (L.GT.LMAX) LMAX=L
      IF (PFORFA(L+1).NE.0) GO TO 5
      IF (L .EQ. 0) THEN
        JFORM=IFORM
        IF (MPLFOR(IFORM,1) .LT. 0) JFORM=-JFORM
      ELSE
        JFORM=MPLFOR(IFORM,L)
      ENDIF
      IF (JFORM .GT. 0) THEN
        PFORFA(L+1)=FORMFA(AK,JFORM)
      ELSE
        PFORFA(L+1)=FORMFC(AKK,L,0,IFORM)
      ENDIF
    5 CONTINUE
      GO TO 100
C
C  HERE TO CALCULATE SUM OVER RADIAL FORM FACTORS * YLM
    6 CONTINUE
C  SQUARE ROOT OF -1
      AI=CMPLX(0.,1.)
      TWORTP=SQRT(FOURPI)
      ROOT2=SQRT(2.)
C
      CALL GMPRD(ROT(1,1,MAT),H,RH,3,3,1)
      CALL GMEQ(RH,OH,1,3)
      CALL UNIVEC(OH,D)
      T=ACOS(OH(3))
      P=0.
      IF (1.-ABS(OH(3)) .GT. 10E-5) P=ATAN2(OH(2),OH(1))
C  CALCULATE ALL NECESSARY SPHERICAL HARMONICS
      N=LMAX+1
      NUM=N*(N+1)/2
      CALL SPHARM(Y,T,P,N,NUM)
C
C  NOW ACCUMULATE THE NON-SPHERICAL FORM FACTOR
      II=1
      PSUM=0.
      DO 1 I=MPTAB(MAT),MPTAB(MAT+1)-1
      READ(MPNAM(I),11) L,M,ASIG
   11 FORMAT (1X,I1,I1,A1)
      NUM=M+1+(L*(L+1)/2)
      IF (ASIG.EQ.'-') THEN
        HARM=AIMAG(Y(NUM))
      ELSE
        HARM=REAL(Y(NUM))
      ENDIF
      IF (M.NE.0) HARM=HARM*ROOT2
      PFACS(II)=PFORFA(L+1)*HARM*TWORTP*(AI**L)
      PSUM=PSUM+PFACS(II)*POLAMP(I,2)
      II=II+1
    1 CONTINUE
C
  100 RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE PFSET
      SUBROUTINE PFSET
C
C *** PFSET corrected by PJB C17 17 Sept 93 ***
C
CX
CC 18A
CH Directs the reading of J and W cards for multipole calculations.
C
      CHARACTER*4 WORD,WWORDS(2)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CHOOSE/MFORTB(50)
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /MPODAC/MPNAM(200)
      CHARACTER *4 MPNAM
      COMMON /POLFOR/MPLFOR(20,6),NUMGEN,PFORFA(7),LMAX
C%
C      COMMON /SCRAT1/AMPS(%MPOL%),MPTMP(%MPOL%),LABEL(%MPOL%),LTMP(3),
      COMMON /SCRAT1/AMPS(200),MPTMP(200),LABEL(200),LTMP(3),
     & LPK(10,3),NMPL
      DATA WWORDS/'RADF','ROTN'/
C
C SAVE CURRENT ERROR COUNT
      IERSAV=IERR
C
      IF (IOUT.GT.200) CALL OPSYM(1)
C
C  READ IN J DATA
      NMPAT=0
      NMPL=0
      NCLUMP=0
      NJ=ICDNO(10)
      IF (NJ .EQ. 0) GO TO 32
C
      LTMP(1)=50
      LTMP(2)=7
      LTMP(3)=13
      CALL NPACK(NPK,LTMP,3,0,LPK)
C
C  READ DATA FROM  J MPOL CARDS
      IDT=IABS(INREAD(10))
      INREAD(10)=-IDT
      DO 21 J=1,NJ
      CALL INPUTJ(IDT,NTYP,IAT,IPT,IE)
      IF (NTYP.NE.1) GO TO 28
      IF (MPNMTB(IAT).EQ.0) THEN
C%
C      CALL ERRCHK(2,NMPAT,%MPAT%,0,'atoms with multipoles')
      CALL ERRCHK(2,NMPAT,20,0,'atoms with multipoles')
      MPATAB(NMPAT)=IAT
      MPNMTB(IAT)=NMPAT
      ENDIF
      LTMP(1)=MPNMTB(IAT)
  11  CALL RDWORD(WORD,ILEN,IPT,IPT1,80,0,IE)
C IF NOTHING MORE ON CARD:
      IF (IE .EQ. 100) GO TO 28
C
      IF (IE.NE.0) GO TO 29
      LTMP(2)=NCHINT(WORD(2:2))
      IF (LTMP(2) .LT. 0 .OR. LTMP(2) .GT. 6) GO TO 29
      LT=NCHINT(WORD(3:3))
      IF (LT .LT. 0 .OR. LT .GT. LTMP(2)) GO TO 29
      IF (WORD(4:4).EQ.'-') LT=-LT
      LTMP(3)=LTMP(2)+1-LT
C%
C      CALL ERRCHK(2,NMPL,%MPOL%,0,'multipoles')
      CALL ERRCHK(2,NMPL,200,0,'multipoles')
      CALL RDREAL(AMPS(NMPL),IPT1,IPT,80,IE)
      IF (IE.NE.0) GO TO 29
      CALL NPACK(MPTMP(NMPL),LTMP,3,1,LPK)
      GO TO 11
C
  29  CALL ERRMES(1,1,'reading J MPOL card')
C  TO GET NEXT J CARD
   28 IDT=IDT+NYZ
   21 CONTINUE
C
C END OF J CARDS TIDY UP
      CALL SORTN(MPTMP,LABEL,NMPL)
C
  32  IF (ICDNO(10) .GT. 0) THEN
C FIND OUT WHICH FORM FACTORS ARE WHICH:
      IDT=IABS(INREAD(10))
      CALL MPFORM(IDT,NJ)
C
      ENDIF
      NCARDW=ICDNO(23)
      IF (NCARDW .EQ. 0) GO TO 100
C
C  READ IN W DATA
      CALL RADFUN(MMPAT,-1)
      CALL READRT(NMPAT,-1)
      IDT=IABS(INREAD(23))
      DO 3 N=1,NCARDW
      CALL INPUTW(IAT,WORD,IDT,IPT)
      GO TO (3,4,5), 1+NCFIND(WORD,WWORDS,2)
    4 CALL RADFUN(IAT,IPT)
      GO TO 3
    5 MAT=MPNMTB(IAT)
      IF (MAT.EQ.0) GO TO 3
      CALL READRT(MAT,IPT)
    3 CONTINUE
      GO TO 100
C
      ENTRY PFEND(NEWAT)
      NAT=NEWAT
      NEXT=0
C
      MAT=0
   9  NEXT=NEXT+1
      IF (NEXT.GT.NMPL) THEN
        JAT=0
      ELSE
      NTAB=LABEL(NEXT)
      NPK=MPTMP(NTAB)
      PAMP=AMPS(NTAB)
      CALL NPACK(NPK,LTMP,3,2,LPK)
      JAT=LTMP(1)
      LV=LTMP(2)
      MVAL=LTMP(3)
      ENDIF
      IF ((MAT.NE.0).AND. (MAT.NE.JAT.OR. LVAL.NE.LV))
     & CALL ORTFUN(IFUN,KNUM,DUM,0)
      IF (JAT.EQ.MAT .AND. JAT .NE. 0) GO TO 40
C  NEW ATOM
      MPTAB(MAT+1)=NMPOL+1
      IF (MAT.NE.0)
     & WRITE (LPT,2001) (MPNAM(J),POLAMP(J,1),J=MPTAB(MAT)
     & ,MPTAB(MAT+1)-1)
 2001 FORMAT (/' Multipoles: ',5(A4,F8.4,2X)/12(13X,5(A4,F8.4,2X)/))
      IF (JAT.NE.0) THEN
        MAT=JAT
        IAT=MPATAB(MAT)
      ELSE
        IF (NAT .EQ. 0) GO TO 1
        MAT=NMPAT-NAT+1
        NAT=NAT-1
        IAT=MPATAB(MAT)
        CALL READRT(MAT,-2)
        PAMP=1.0
        LV=0
        MVAL=0
      ENDIF
      LVAL=-1
C FIND APPROPRIATE FORM FACTOR:
      IFORM=MFORTB(IAT)
      IF (MPLFOR(IFORM,1) .EQ. -999) THEN
        WRITE (LPT,2002) FONAME(IFORM)
2002    FORMAT (//' *********'/' *  ',A4,' *'/' *********')
        MPLFOR(IFORM,1)=-IFORM
        CALL RADFUN(IFORM,0)
      ENDIF
      WRITE (LPT,2002) ATNAME(IAT)
      IERCNT=IERR
      CALL READRT(MAT,0)
      IF (IERR .GT. IERCNT) CALL ERRMES(0,0,'in PFSET')
   40 IF (LV.EQ.LVAL) GO TO 41
C  NEW VALUE OF L
      LVAL=LV
      IF (LV .NE. 0 .AND. MPLFOR(IFORM,LV) .EQ. 0) THEN
        WRITE (LPT,3000) LV,FONAME(IFORM),ATNAME(JAT)
        WRITE (ITO,3000) LV,FONAME(IFORM),ATNAME(JAT)
3000    FORMAT(/' ERROR ** No <j',I1,'> form factor name for ',A4,
     &  ' needed by atom ',A4/)
        IERR=IERR+1
      ENDIF
      KNUM=2*LVAL+1
      IF (KNUM.GT.1) CALL MPCON(MAT,KNUM)
      MO=1
C TEST OF PAMP=0 & TO LABEL 9 OMITTED BY JBF Apr 89
  41  CALL ORTFUN(MVAL,KNUM,PAMP,MO)
      MO=MO+1
      GO TO 9
    1 IF (IERR.NE.0) WRITE (ITO,10) IERR
   10 FORMAT (/I5,' Errors in input of wave-functions',
     & ' and/or multipoles')
      IERR=IERR+IERSAV
C SET LSQ PARAMETERS:
      CALL CONVMP(1)
  100 RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE PLN3AD(I,J,K)
      SUBROUTINE PLN3AD(I,J,K)
C
C *** PLN3AD by JCM 3 Oct 84 ***
C
CX
CC 1A
CH A specialist routine used during the formation of the reciprocal unit
CH cell, to offer up 3 planes as boundaries, in cubic space groups.
CA On entry I,J,K specify 3 planes, by pointing to symmetry elements.
CA                The array AXI(,I,,) holds the axes of these elements
CA                in reciprocal space, and these axes are normal to the
CA                planes in question
C
CD The 3 axes are first oriented so that the angles between the planes
CD are acute.
C
      DIMENSION A(3),B(3),C(3)
C%
C      COMMON /SCRAT/AXI(3,%SYMO%,2),MIRROR(%SYMO%),D(3,3),PL1(3),PL2(3),PL3(3),
      COMMON /SCRAT/AXI(3,24,2),MIRROR(24),D(3,3),PL1(3),PL2(3),PL3(3),
     & HT(3),ASY(3,4),NSTAT(4),NOPL,NICE,VOL,MOP1,MOP2
C
      CALL GMEQ(AXI(1,I,2),A,1,3)
      CALL GMEQ(AXI(1,J,2),B,1,3)
      CALL GMEQ(AXI(1,K,2),C,1,3)
C MAKE AN ACUTE ANGLE BETWEN A AND B:
      IF (SCALPR(A,B) .LT. 0.) CALL GMREV(B,B,1,3)
C MAKE AN ACUTE ANGLE BETWEN A AND C:
      IF (SCALPR(A,C) .LT. 0.) CALL GMREV(C,C,1,3)
      CALL VECPRD(B,C,PL1)
      CALL VECPRD(C,A,PL2)
      CALL VECPRD(A,B,PL3)
      CALL FIXUNI(PL1,3)
      CALL FIXUNI(PL2,3)
      CALL FIXUNI(PL3,3)
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE POLUNI
      SUBROUTINE POLUNI
C
C *** POLUNI by PJB 8 Aug 83 ***
C
CX
CC 1A
CH A specialist routine to "polish" the edges of a found asymmetric unit
CH by specifying exactly how its faces and edges should be treated.
C
CP POLUNI is called from the end of SYMUNI, and would not be useful
CP outside this context.
C
CD Sets the array MARK in /GUNIT/ to indicate the exact treat ment of
CD faces and edges of the reciprocal cell asymmetric unit in order to
CD deduce the multiplicites of reflections occurring on them.
C
      LOGICAL BINDIG
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
      COMMON /GUNIT/MARK(3,2),BSYM(3,3),IBOX,KOM11
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/AXI(3,%SYMO%,2),MIRROR(%SYMO%),D(3,3),PL1(3),PL2(3),PL3(3),
      COMMON /SCRAT/AXI(3,24,2),MIRROR(24),D(3,3),PL1(3),PL2(3),PL3(3),
     & HT(3),ASY(3,4),NSTAT(4),NOPL,NICE,VOL,MOP1,MOP2
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C  CLEAR MARK TO 1
      DO 10 I=1,3
      DO 10 J=1,2
   10 MARK(I,J)=1
C
C  DEAL WITH P1 AND P-1
      IF ((NOP.EQ.1) .AND. .NOT. FRIEDL) GO TO 100
      IF (NOPC.GT.1) GO TO 14
C  FIND ANY LINE IN THE PLANE
      CALL INVENT(ASYM(1,1),ASYM(1,1),BSYM(1,1))
      CALL VECPRD(ASYM(1,1),BSYM(1,1),BSYM(1,2))
      MARK(1,1)=4
      GO TO 100
C
C  WORK OVER SYMMETRY ELEMENTS
   14 DO 1 N=2,NOPC
      IF (IABS(NORD(N)).GT.100) GO TO 1
      IORD=IABS(NORD(N))
      IF (MIRROR(N).EQ.0) GO TO 2
C
C  PROCEDURE IF MIRROR PLANE,EITHER END OF AXIS WILL DO
      DO 15 IR=1,2
      CALL EQVEC(ASYM,AXI(1,N,1),NASYM,M,0)
      IF (M.GT.NASYM) GO TO 15
C
C  MARK PLANE AS MIRROR
      MARK(M,1)=2
      GO TO 16
   15 CALL GMREV(AXI(1,N,1),AXI(1,N,1),3,1)
C
C  JUMP IF NOT A DIAD AXIS
   16 IF(NORD(N).EQ.-2 .AND. .NOT. FRIEDL ) GO TO 1
C
C  PROCEDURE FOR SYMMETRY AXES
    2 CALL INBOX(AXI(1,N,2),IN)
      IF (IN) 1,3,4
C
C  SYMMETRY AXIS INSIDE UNIT - ERROR\
   3  WRITE (LPT,3000)(AXI(I,N,1),I=1,3),N
      WRITE (ITO,3000)(AXI(I,N,1),I=1,3),N
3000  FORMAT (/' ERROR ** IN POLUNI - SYMMETRY AXIS ',3F5.1,
     & ' INSIDE UNIT - OPERATOR NUMBER',I4)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C  AXIS IS ON UNIT
    4 IF (IN.GT.10) GO TO 5
      IF (IORD.GT.2) GO TO 3
C
C  HERE FOR DIAD AXIS ON A PLANE - NOT AT CORNER
      CALL VECPRD(ASYM(1,IN),AXI(1,N,2),BSYM(1,IN))
      MARK(IN,1)=3
C  DECIDE WHICH EDGE TO INCLUDE
      IF (NASYM .GT. 2) GO TO 8
      MARK(3,2)=-IABS(MARK(3,2))
      GO TO 1
   8  M1=MOD(IN,3)+1
      M2=MOD(M1,3)+1
      M=M1
      L=M2
      IF (SCALPR(BSYM(1,IN),EDGE(1,M)).GT.0.) GO TO 13
      M=M2
      L=M1
C  CHECK MARK NOT ZERO ALREADY
   13 IF (MARK(L,2).NE.0) MARK(M,2)=0
      GO TO 1
C
C  AXIS ON EDGE
    5 IN=IN-10
      IF (NASYM.GT.2) GO TO 11
      IF (IORD.NE.3 .OR..NOT.FRIEDL) GO TO 11
C  SPECIAL BLISTER FOR BAR 3 AXIS
      MARK(IN,2)=-3*IABS(MARK(IN,2))
      RANG=TWOPI/6.
      GO TO 9
   11 MARK(IN,2)=ISIGN(MARK(IN,2)*IORD,NORD(N))
      IF (MARK(IN,2).LT.0 .AND. .NOT. FRIEDL) MARK(IN,2)=MARK(IN,2)/2
      IF (IORD.EQ.2) GO TO 1
      RANG=TWOPI/IORD
      IF (NASYM.EQ.2) GO TO 9
      IF (NORD(N) .LT. 0 .AND. .NOT. FRIEDL) RANG=RANG*2.
   9  IF (ABS(RANG-ANG(IN)).GT..0001) GO TO 1
      M=MOD(IN,3)+1
C  MARK POINTS ON PLANE AND AXIS NOT TO BE USED
      IF (NASYM.LT.3) GO TO 12
C  NO EDGES
C  IS AN EDGE ALREADY MARKED ZERO?
      M1=MOD(M,3)+1
      IF (MARK(M1,2).EQ.0) GO TO 12
C  IF THAT NOT ZERO MARK THE OTHER ONE
      MARK(M,2)=0
      M=M1
   12 MARK(M,1)=0
C
    1 CONTINUE
C
C  PUT IN EXTRA DIVISION IF A MIRROR PLANE GOES THROUGH AN EDGE
      IF (NASYM.LT.2) GO TO 100
C  NO EDGES:
      J=2
      K=3
      DO 7 I=1,3
      IF (MARK(I,1).EQ.2 .OR.MARK(J,1).EQ.2) MARK(K,2)=MARK(K,2)*2
      IF (NASYM.EQ.2) GO TO 100
      J=K
      K=I
    7 CONTINUE
  100 IF (BINDIG(IOUT,16)) THEN
        WRITE (LPT,4000) ((MARK(I,J),I=1,3),J=1,2),BSYM,EDGE
        WRITE (ITO,4000) ((MARK(I,J),I=1,3),J=1,2),BSYM,EDGE
 4000   FORMAT (/' MARK :',3I5,4X,3I5/' BSYM, EDGE :'/3(/3(1X,3F8.2/)))
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PRBLOK
      SUBROUTINE PRBLOK
C
C *** PRBLOK Modified by PJB 9-Mar-1994 ***
C
CX
CC 6C
CH Prints a block of shifts in parameters all relating to the same atom
CH in LSQ applications involving structure parameters.
CP IBUFF in /ATBLOK/ is the number of shifts to be printed. The calling
CP routine APSHxx must control IBUFF and store shift information in the
CP arrays in /ATBLOK and /ATBLOC.
C
CD If IBUFF=0 exits.  Otherwise prints shifts across the page for one specific
CD atom as stored.  Sets IBUFF=0
C
CO Prints blocks of new parameter, esd, shift, old parameter, shift/esd
CO labelled appropriately.
C
      CHARACTER *80 FBUF
      CHARACTER *6 LABEL(4)
C
      COMMON /ATBLOC/NAME,IPNAME(12)
      CHARACTER *4 NAME,IPNAME
      COMMON /ATBLOK/IBUFF,PNEW(12),PESD(12),PSHIFT(12),POLD(12),
     & PSESD(12)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      DATA LABEL/'   NEW','   ESD',' SHIFT','   OLD'/
C
      IF (IBUFF .EQ. 0) GO TO 100
C
      WRITE (LPT,2001) NAME,(IPNAME(II),II=1,IBUFF)
2001  FORMAT (/2X,A4,12(3X,A4,2X))
      FBUF(1:4) ='(A6,'
      J=5
      DO 1 II=1,IBUFF
      WRITE (FBUF(J:),2002) NSIGFG(PESD(II))
 2002 FORMAT ('F9.'I1,',')
      J=J+5
    1 CONTINUE
      FBUF(J-1:)=')'
      WRITE (LPT,FBUF) LABEL(1),(PNEW(II),II=1,IBUFF)
      WRITE (LPT,FBUF) LABEL(2),(PESD(II),II=1,IBUFF)
      WRITE (LPT,FBUF) LABEL(3),(PSHIFT(II),II=1,IBUFF)
      WRITE (LPT,FBUF) LABEL(4),(POLD(II),II=1,IBUFF)
      WRITE (LPT,2006) (PSESD(II),II=1,IBUFF)
2006  FORMAT (' SH/SD',12F9.5)
      IBUFF=0
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PREFIN(PROGRM)
C>> JCC Was
C    SUBROUTINE PREFIN(PROGRM)
C>> Now
      INTEGER FUNCTION PREFIN(PROGRM)
C>> Need this to be able to return an error status.
C
C *** PREFIN updated by JCM 28 Apr 92 ***
C
CX
CC 13A
CH Makes the Crystal Data File readable in a random order by writing it
CH to a scratch file.
CA On entry PROGRM is the A6 name of the calling program, to head the output
C
CD Creates a direct access file number IO10, writes to it the given
CD Crystal Data File as A80 records, and keeps an index so that subsequently
CD any "card" may be read or re-read as required.
CD
CD Sets ICDNO(1-26) to be the number of "cards" read for each letter, except Y
CD      and Z, which are passed through anywhere (printing out the Y).
CD Sets INREAD(1-26) to be the start record for each letter chunk.
CD Sets NTOTL to be the total number of records read.
CD Sets IERR=0 so that errors later may reset it.
CD
CD Also initialises system by a call to INITIL.  Jobs wishing to intervene
CD to change parameters must say, e.g.:
CD      CALL INITIL('NAME')
CD      change whatever wanted
CD      CALL PREFIN('NAME')
C
CD PREFIN reads "cards" labelled with a letter from A-Z in column 1,  from a
CD dataset called the "crystal data".  This is read from logical unit
CD ICRYDA, which may be assigned to a specific unit number in
CD the main program, or left unassigned so that the call to OPNFIL
CD will assign a unit number and ask interactively for a file name.
CD
CD    The "cards" give information about the current problem.
CD    Letters used so far are:
CD    A   atomic positions (read by ATOPOS)
CD    B   bond length and angle instructions (read by  main program BONDS)
CD    C   cell dimensions (read by RECIP)
CD    D   diffraction or diffractometer data (read by SETDC)
CD    E   extinction correction data (read by EXTINC)
CD    F   form factors (read by SETFOR)
CD    G   Gauss integration and other data for abs corr (read by SETGAU)
CD    I   instruction "cards" (read by main programs)
CD    J   multipole information (read by INPUTJ)
CD    L   least squares refinement data (read by, e.g., INPLSF)
CD    M   data for calculation of fourier maps (read by SETFOU)
CD    N   title (a single "card, read by INPUTN)
CD    P   polarisation data (read by SETPOL)
CD    Q   data for magnetic structures (read by DOMAG)
CD    S   symmetry "cards" (read by SYMOP)
CD    T   anisotropic temperature factors  (read bY SETANI)
CD    U   indices supplied to force use of a particular unit asymmetric
CD        unit (read by SYMUNI)
CD    V   choice of representation of the space group
CD    W   wave function data (read by INPUTW)
CD    X   entirely under user's control
CD    Y   comment repeated on ouput LPT
CD    Z   comment not repeated on output
CD
CD    All "cards" with the same initial letter must come together but the
CD    groups may be in any order.
CD
CD    Except in a few instances such as  "F" "cards", where if a table of
CD    values is given, SETFOR expects  "cards" to be in the correct sequence,
CD    the "cards" within a group may be in any order.
CD
CD    The restriction about having all "cards" of one letter together does not
CD    apply to "cards" starting Y or Z.
CD
CD    A completely blank line is ignored.
CD
CD    An end of file, or any "card" with a non letter symbol in column 1
CD    terminates a set of crystal data. Any further data in the file may
CD    be read subsequently by the user's program.
CD
CD If an "M GET" card is given, the user wishes to take his basic data, and any
CD saved Fourier maps, from a dataset supplied, not from this crystal data.  The
CD SUBROUTINE MAJUST is called to adjust his file IO10 in this case.
C
CI Reads in the Crystal Data File
CO Lists on the lineprinter output a summary of what it found.
C
      CHARACTER *(*) PROGRM
      LOGICAL ENDIP
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

C>> JCC Initialise return value ( successful = 1, anything else is failure)
      PREFIN = 1

C
C INITIALISE WHOLE SYSTEM - DATE, TIME, CONSTANTS, I/O UNIT NUMBERS ETC:
C UNLESS THAT HAS ALREADY BEEN DONE ONCE IN THE JOB
      CALL INITIL(PROGRM)
C
      GO TO 9
C
C ENTRY TO READ A NEW CRYSTAL DATA SET FROM THE SAME FILE:
C      ENTRY NEXPRE(ENDIP)
C      CALL CLOFIL(IO10)
C      GO TO 10
C
C ENTRY TO READ A NEW CRYSTAL DATA SET FROM A DIFFERENT FILE:
C      ENTRY NEWPRE
C      CALL CLOFIL(ICRYDA)
C      CALL CLOFIL(IO10)
C OPEN CRYSTAL DATA BY ASKING FOR NAME WHERE POSSIBLE:
   9  ENDIP=.FALSE.
      MESSAG='Crystal data file'
CILL      NAMFIL='.CRY'
CRAL
      NAMFIL='.CCL'
      CALL OPNFIL(ICRYDA,111)
C>> JCC
C>>  Check the value of ICRYDA. An error may have occurred on opening the file
      IF (ICRYDA .EQ. -1) THEN
            PREFIN = 0
            RETURN
      ENDIF
C OPEN SCRATCH FILE TO HOLD COPY OF CRYSTAL DATA:
   10 IO10=NOPFIL(10005)
C SET NO ERRORS DETECTED ON INPUT - SYSTEM PLOUGHS ON AS LONG AS IT CAN:
      IERR=0
C START COUNT IN OUTPUT:
      ID=0
      CALL CDFIN(1,ID,ENDIP)
      NTOTL=NTOTAL(1)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PRILIS(AVAL,IPT1,IPT2)
      SUBROUTINE PRILIS(AVAL,IPT1,IPT2)
C
C *** PRILIS updated by JCM 22 Aug 86 ***
C
CX
CC 13C
CH Prints a list of real numbers held in an array, 5 per line.
CA On entry AVAL holds the required real numbers,
CA          IPT1 points to the first to be printed,
CA          IPT2 points to the last to be printed.
C
CO Writes out AVAL(IPT1 to IPT2), in format G12.5, 5 per line.
C
      DIMENSION AVAL(IPT2),ATEMP(20)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IN=0
      DO 1 I=IPT1,IPT2
   3  IN=IN+1
      IF (IN .GT. 20) THEN
        WRITE (LPT,2000) ATEMP
2000    FORMAT (4(1X,5G12.5/))
        IN=1
      ENDIF
      ATEMP(IN)=AVAL(I)
   1  CONTINUE
C
C WRITE ANY SINGLE VALUES GATHERED INTO ATEMP:
      IF (IN .GT. 0) WRITE (LPT,2000) (ATEMP(I),I=1,IN)
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE PRIPLN(A,IR)
      SUBROUTINE PRIPLN(A,IR)
C
C *** PRIPLN updated by JCM 12 Nov 89 ***
C
CX
CC 13C
CH Given the normal to a plane face in A, prints the equation of the plane.
CA On entry A is a 1x3 array containing 3 elements of the normal.
CA          IR=1 indicates that the normal is in terms of h,k,l, and
CA          IR=2 in terms of x,y,z (for later use).
CO Writes on unit LPT the equation of the plane.
CN At present every plane is assumed to go through the origin.
C
      CHARACTER *12 NEQN
      DIMENSION A(3)
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IPT=0
      NEQN=' '
      DO 2 I=1,3
      IF (A(I)) 3,2,4
C
C NEGATIVE TERM:
   3  IPT=IPT+1
      NEQN(IPT:IPT)='-'
      GO TO 5
C
C POSITIVE TERM - OMIT EXPLICIT PLUS IF AT START:
   4  IF (IPT .EQ. 0) GO TO 5
      IPT=IPT+1
      NEQN(IPT:IPT)='+'
   5  K=IABS(NINT(A(I)))
      IF (K .NE. 1) THEN
C COEFFICIENT OTHER THAN 1:
        IPT=IPT+1
        NEQN(IPT:IPT)=IDIGIT(K)
      ENDIF
      IPT=IPT+1
C PICK LETTER:
      IF (IR .NE. 2) THEN
        L=I+9
        IF (I .EQ. 1) L=8
      ELSE
C WANT X,Y,Z:
        L=I+22
      ENDIF
      NEQN(IPT:IPT)=LETLOW(L)
   2  CONTINUE
C
C TIDY:
      IF (IPT .EQ. 0) GO TO 100
      IF ((IPT .EQ. 3) .AND. (NEQN(2:2) .EQ. '-')) THEN
        NEQN(4:4)=NEQN(3:3)
        NEQN(3:3)='='
        NEQN(2:2)='>'
      ELSE
        NEQN(IPT+1:IPT+1)='>'
        NEQN(IPT+2:IPT+2)='='
        NEQN(IPT+3:IPT+3)=IDIGIT(10)
      ENDIF
      WRITE (LPT,2000) NEQN
2000  FORMAT (13X,A12)
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PRIVAR
      SUBROUTINE PRIVAR
C
C *** PRIVAR updated for MK4 by JCM Aug 89 ***
C
CX
CC 6C
CH Prints a list of basic variables, and constraint relations, for LSQ.
CP VARMAK must have been obeyed to create the lists
CO For most basic variables, lists their names.  For family 4, ("very long
CO      vectors") only prints ranges.
CO Then lists the strict constraints.
CO Any phases and sources are indicated by *P and *S
C
      CHARACTER *1 IJOIN(8)
      CHARACTER *4 IPR1(8),IPR2(8),NAM1,NAM2,NOLD
      LOGICAL F4,F5
      DIMENSION K1(8),K2(8)
      COMMON /CONSTR/JCONST,JROWPT(301),JCMAT(200),AMOUNT(200),
     & NEXTJ
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
      IF (LVARB .LE. 0) THEN
        CALL MESS(LPT,1,'No variables')
        GO TO 100
      ENDIF
C
      WRITE (LPT,2001) LVARB
2001  FORMAT (/'    ',I5,' basic variable(s) :'/)
C NUMBER OF ITEMS PER LINE OF OUTPUT:
      LINE=7
      F4=.FALSE.
      IGENF4=0
      ISP1F4=0
      ISPNF4=0
      IC=0
      JPH=0
      JSO=0
C
      DO  4 IB=1,LVARB
      K=LVRPR(LBSVR(IB))
C
C SAVE PREVIOUS PHASE AND SOURCE:
      KPHASE=JPH
      KSOURC=JSO
      CALL KUNPAK(K,IFAM,IGEN,ISPC,JPH,JSO)
      IF (MULFAS .AND. KPHASE .NE. JPH) THEN
        WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
        IC=0
        WRITE (LPT,2006) JPH
2006    FORMAT(/' Phase',I3)
      ENDIF
C
      IF (IFAM .NE. 4) GO TO 5
C
C FAMILY 4 - LONG VECTORS, INCONVENIENT TO PRINT:
C GET PRINTING NAME:
      NOLD=NAM2
      CALL PARNAM(NAM1,NAM2,3,K)
      IF (IGEN .EQ. IGENF4) GO TO 8
C NEW GENUS (INCLUDING START)
      IF (F4 .AND. ISP1F4 .NE. ISPNF4) THEN
C PART-ENTRY ALREADY IN BUFFERS (BUT IF ONLY 1 IN RANGE, IS ALREADY THERE):
C PUT END VALUE OF RANGE INTO BUFF2:
        IPR2(IC)=NOLD
        IJOIN(IC)='-'
      ENDIF
C SET "HAVE HAD SOME FAMILY 4"
      F4=.TRUE.
C PUT GENUS NAME FOR FAMILY 4 INTO BUFFER BY ITSELF:
      IGENF4=IGEN
      IF (IC .GE. LINE-1) THEN
        WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
        IC=0
      ENDIF
      IC=IC+1
      IPR1(IC)=' '
      IPR2(IC)=NAM1
      IJOIN(IC)=' '
C PUT LOWER END OF RANGE INTO BUFF:
  14  IF (IC .GE. LINE-1) THEN
        WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
        IC=0
      ENDIF
      IC=IC+1
      IPR1(IC)=NAM2
      IPR2(IC)=' '
      IJOIN(IC)=' '
      ISP1F4=ISPC
      ISPNF4=ISPC
      GO TO 4
C
C SAME GENUS - IS IT NEXT SPECIES?
   8  IF (ISPNF4+1 .EQ. ISPC) THEN
        ISPNF4=ISPC
        GO TO 4
      ENDIF
C
C PUT RANGE END INTO BUFFER:
      IF (ISP1F4 .NE. ISPNF4) THEN
        IPR2(IC)=NOLD
        IJOIN(IC)='-'
        IF (IC .GE. LINE) THEN
          WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
          IC=0
        ENDIF
      ENDIF
      GO TO 14
C
C NOT FAMILY 4:
   5  IF (ISP1F4 .NE. ISPNF4) THEN
        ISPNF4=ISP1F4
        IPR2(IC)=NAM2
        IJOIN(IC)='-'
      ENDIF
      IF (IC .GE. LINE) THEN
        WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
        IC=0
      ENDIF
      IC=IC+1
      CALL PARNAM(IPR1(IC),IPR2(IC),1,IB)
      IJOIN(IC)=' '
      IF (IC .GE. LINE) THEN
        WRITE (LPT,2002) (IPR1(J),IJOIN(J),IPR2(J),J=1,IC)
2002    FORMAT (' ',10(A4,A1,A4,2X))
        IC=0
      ENDIF
   4  CONTINUE
C
C MAY BE REMNANTS OF FAMILY 4 NOT IN BUFFER:
      IF (F4 .AND. (ISP1F4 .NE. ISPNF4)) THEN
        IPR2(IC)=NAM2
        IJOIN(IC)='-'
      ENDIF
      IF (IC .NE. 0)  WRITE (LPT,2002) (IPR1(J),IJOIN(J),
     & IPR2(J),J=1,IC)
C
C CONSTRAINT LIST:
      IF (JCONST .LE. 0) GO TO 100
      WRITE (LPT,2003) JCONST
2003  FORMAT (/' ',I5,' constraint(s) - relations between shifts',
     & ' in variables are :'/)
      DO 3 J=JCONST,1,-1
      JROW=JROWPT(J)
      JNEXT=JROWPT(J+1)-1
      JR=JNEXT-JROW+1
      CALL PARNAM(NAM1,NAM2,2,LRDVR(J))
C KPHASE IS SET BY PARNAM:
      KP=KPHASE
      KS=KSOURC
      IF (JR .GT. 8) THEN
        WRITE (LPT,3000) NAM1,NAM2
        WRITE (ITO,3000) NAM1,NAM2
3000    FORMAT (/' ERROR ** redundant variable',2(A4),
     &   'related to > 8 basics - cannot yet print')
        JNEXT=JROW+7
        JR=8
      ENDIF
C
      F4=.TRUE.
      F5=.TRUE.
      DO 7 M=1,JR
      CALL PARNAM(IPR1(M),IPR2(M),1,JCMAT(M+JROW-1))
      K1(M)=KPHASE
      K2(M)=KSOURC
      IF (KPHASE .NE. KP) F4=.FALSE.
      IF (KSOURC .NE. KS) F5=.FALSE.
   7  CONTINUE
      IF (JR .NE. 1 .OR. ABS(AMOUNT(JROW)-1.) .GT. 0.00001) THEN
      IF (.NOT. MULFAS) WRITE (LPT,2004) NAM1,NAM2,AMOUNT(JROW),
     &  IPR1(1),IPR2(1),('+',AMOUNT(I+JROW-1),IPR1(I),
     &  IPR2(I),I=2,JR)
2004    FORMAT (1X,A4,1X,A4,' = ',F10.3,' times ',A4,1X,A4,
     &  (1X,A1,1X,F10.3,' times ',A4,1X,A4))
      IF (MULFAS) WRITE (LPT,2008) NAM1,NAM2,KP,AMOUNT(JROW),
     &  IPR1(1),IPR2(1),K1(1),('+',AMOUNT(I+JROW-1),IPR1(I),
     &  IPR2(I),K1(I),I=2,JR)
2008    FORMAT (1X,A4,1X,A4,' *P',I1,' = ',F10.3,' times ',A4,1X,A4,
     &  ' *P',I1,(1X,A1,1X,F10.3,' times ',A4,1X,A4,' *P',I1))
      ELSE
        IF (MULFAS) WRITE (LPT,2007) NAM1,NAM2,KP,IPR1(1),
     &  IPR2(1),K1(1)
2007    FORMAT (1X,A4,1X,A4,' *P',I1,' = ',A4,1X,A4,' *P',I1)
        IF (.NOT. MULFAS) WRITE(LPT,2005) NAM1,NAM2,IPR1(1),IPR2(1)
2005    FORMAT (1X,A4,1X,A4,' = ',A4,1X,A4)
      ENDIF
   3  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE PRIWRD(IFAM,IGEN,ISPC,NAME,MODE)
      SUBROUTINE PRIWRD(IFAM,IGEN,ISPC,NAME,MODE)
C
C *** PRIWRD updated for MK4 by JCM 8 May 90 ***
C
CX
CC 6C
CH Finds the name of the packed (possibly part) LSQ parameter from
CH the built-in table of parameter names.
CA On entry IFAM, IGEN, ISPC are family, genus and species of the parameter.
CA          MODE=0 requests left justify, =1 right justify.
CA On exit  NAME is the A4 name from the table in /WORDS/, or 'XXXX'
CP The table must have been set up by LSETUP
C
      CHARACTER *4 NAME
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      COMMON /WDSPC/IWDNUM,IWDSPC(60)
      COMMON /WORDS/LSQWD(60)
      CHARACTER *4 LSQWD
C
      NAME='XXXX'
C
C THE PHASE IS IRRELEVANT - EVERY PHASE HAS THE SAME VOCABULARY HERE
C IF MULTISOURCE, AND FAMILY 3 THE SOURCE IS RELEVANT:
      IF (MULONE) THEN
        K=0
        IF (IFAM .EQ. 3) K=KSOURC
      ENDIF
C
      IPACK=KPAK(IFAM,IGEN,ISPC,0,K)
      IPT=NFIND(IPACK,IWDSPC,IWDNUM)
      IF (IPT .GT. 0) THEN
C
        IF (MODE .EQ. 0) THEN
          NAME=LSQWD(IPT)
        ELSE
C
C RIGHT JUSTIFY:
          J=4
          NAME=' '
          DO 3 I=4,1,-1
          IF (LSQWD(IPT)(I:I) .NE. ' ') THEN
            NAME(J:J)=LSQWD(IPT)(I:I)
            J=J-1
          ENDIF
   3      CONTINUE
        ENDIF
      ENDIF
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PRMBLK
      SUBROUTINE PRMBLK
C
C *** PRMBLK updated by JCM 20 Mar 90 ***
C
CX
CC 7B
CH Applies shifts to multipole parameters, in both program and user units.
CP APSHMP must have stored a vector of multipole shifts in POLAMP(,4)
CP        and ESDs in POLAMP(,6)
CP CONMAT holds the conversion matrices
CD Applies matrix transformation to set of shifts to convert them into
CD user's notation.
CO Prints the converted shifts in the usual format on unit LPT
C
      CHARACTER *4 LNAM1,LNAM2
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
C
      CALL CONVMP(2)
C
C APPLY ALL USER SHIFTS TO USER VALUES:
      IFAM=5
      IGEN=1
      DO 51 ISPC=1,NMPOL
      IF (KPOLMP(ISPC) .EQ. 0) GO TO 51
      SHIFT=POLAMP(ISPC,3)
      ESD=POLAMP(ISPC,5)
      CALL ADJUST(POLAMP(ISPC,1))
      CALL FETSHF(2,SHIFT,ESD)
      CALL PARNAM(LNAM1,LNAM2,3,KPAK(5,1,ISPC,0,0))
      WRITE (LPT,2000) LNAM1,LNAM2,XNEW,ESD,SHIFT,XOLD,SHESD
2000  FORMAT (' ',1X,A4,1X,A4,5G14.5)
   51 CONTINUE
C
C CONVERT NEW USER VALUES TO NEW PROGRAM VALUES:
      CALL CONVMP(1)
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE PRMTIV
      SUBROUTINE PRMTIV
C
C *** PRMTIV by JCM 11 Oct 84 ***
C
CX
CC 3A
CH A specialist routine for use in generating h,k,l indices where the
CH natural stepping vectors do not define a primitive cell.
CP Called from SETGEN
CD Sets up coefficients NPRIM(2,2), MCOUNT(2), LFAC(2) in /HKLGEN/
CD to make primitive stepping vectors from the existing steps.
C
      LOGICAL BINDIG
      DIMENSION VEC(3,2)
      COMMON /HKLGEN/STEP(3,3),PT(3,3),VECEND(3,3),PRPT(3,3),
     & NPRIM(2,2),NP,LFAC(2),MCOUNT(2),KOM5
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
C
C  CHECK WHETHER PRIMITIVE AND SET UP INTERVENING STEPS IF NOT:
C
      DO 6 I=1,2
      DO 7 J=1,2
   7  NPRIM(I,J)=0
      MCOUNT(I)=1
   6  LFAC(I)=1
      IF (NP.EQ.1) GO TO 100
C
C  CHECK WHETHER BASE PLANE IS PRIMITIVE
      CALL VECPRD(STEP(1,1),STEP(1,2),VEC(1,1))
      CALL FCTOR(VEC(1,1),M)
      IF (M.EQ.1) GO TO 1
      CALL GMEQ(STEP(1,2),VEC(1,1),1,3)
      DO 10 N1=2,M
      CALL GMADD(VEC(1,1),STEP(1,1),VEC(1,1),1,3)
      CALL FCTOR(VEC(1,1),J)
      IF (J.EQ.M) GO TO 11
   10 CONTINUE
      WRITE (LPT,3000)M,J,(VEC(I,1),I=1,3),STEP
      WRITE (ITO,3000)M,J,(VEC(I,1),I=1,3),STEP
3000  FORMAT (/' ERROR ** IN PRMTIV FINDING BASE PLANE VECTOR - ',
     & ' M,J=',2I4,' VEC1=',3F5.1,' STEP ARRAY IS'/1X,9F5.1)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C
  11  NPRIM(1,1)=N1-1
      NPRIM(2,1)=1
      LFAC(1)=M
C
C  IS THIS ENOUGH?
      IF (M.EQ.NP) GO TO 101
C
C  INTERVENING LAYERS ON THIRD AXIS:
    1 L=NP/M
      CALL GMEQ(STEP(1,3),VEC(1,2),1,3)
      DO 3 N2=1,L
      CALL GMEQ(VEC(1,2),VEC(1,1),1,3)
      DO 4 N1=1,L
      CALL FCTOR(VEC(1,1),J)
      IF (J.EQ.L) GO TO 5
    4 CALL GMADD(VEC(1,1),STEP(1,1),VEC(1,1),1,3)
    3 CALL GMADD(VEC(1,2),STEP(1,2),VEC(1,2),1,3)
C
C  ERROR IF WE GET HERE
      WRITE (LPT,3001)L,M,J,((VEC(I,II),I=1,3),II=1,2),STEP
      WRITE (ITO,3001)L,M,J,((VEC(I,II),I=1,3),II=1,2),STEP
3001  FORMAT (/' ERROR ** in PRMTIV making second vector',
     & ' - L,M,J=',3I4/' Vectors so far are:',6F5.1/' Step array',
     & ' is',9F5.1)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C
   5  NPRIM(1,2)=N1-1
      NPRIM(2,2)=N2-1
      LFAC(2)=L
  101 IF (BINDIG(IOUT,16)) THEN
        WRITE (LPT,4000) M,L,NP,NPRIM,STEP
        WRITE (ITO,4000) M,L,NP,NPRIM,STEP
 4000   FORMAT (' M,L,NP,NPRIM,STEP:'/1X,3I5,4X,I4,4I3/9(1X,3F8.2/)
     &  /3(1X,3F8.2/))
      ENDIF
  100 RETURN
      END
C
C
C
C
C LEVEL 1      LOGICAL FUNCTION PRNCYC(N)
      LOGICAL FUNCTION PRNCYC(N)
C
C *** PRNCYC by JCM 17 Nov 84 ***
C
CX
CC 6C
CH Decides whether printing (of various different quantities in LSQ)
CH is needed during the current LSQ cycle.
CA On entry N is an integer specifying which member of the array IPRNT
CA            in /REFINE/ is to be consulted.  Values of N already
CA            assigned in some standard LSQ applications are:
CA N=1: PRIN - routine IICD1 (called by most LSQ applications) scans any
CA             I cards for a "PRIN" item;  it it finds one, it reads the
CA             integer which follows to IPRNT(1), and uses it as an
CA             indicator of the frequency with which general "obs and
CA             calc" lists are printed during the refinement.  These are:
CA     0 = no printing
CA     1 = print during first cycle
CA     2 = print during last cycle
CA     3 = print during first and last cycles
CA     4 = print every cycle
CA N=2: PRFC - similar to N=1, but used in the Profile Refinement system
CA             to control the printing of reflection information.
CA N=3: PRFO - as N=2, for a file to be reinput to the Fourier routines.
CA N=4: PRPR - as N=2, for a file containing the profile, to be reinput,
CA             e.g., to GENIE.
CA N=5: PRCV - as N=2, but only obeyed on last cycle.  The integer
CA             following "PRCV" is an indication of how many covariances
CA             from the inverse LSQ matrix to print on the .HKL file.
CA             So PRNCYC would be irrelevant here.
CA N=6: PREE - as N=2, for a file containing the eigenvalues and
CA             eigenvectors of that part of the inverse LSQ matrix
CA             relevant to intensities.  PRNCYC is again irrelevant,
CA             the integer here indicating how much material is sent to
CA             the line-printer file.
CA N=7: PRDM - read by IICD1, called by most LSQ applications.  Requests
CA             printing during the last cycle of h,k,l Fo Fc to a a
CA             .DEP file for "deposited Material".  This file may be
CA             subsequently interpreted by the main program DEPOS.
CA N=8: PRSK - read by GEOMCO, called by structure LSQ applications.
CA             Requests the printing of an "obs and calc" list for any
CA             slack constraints, including both geometrical constraints
CA             and those in Profile Refinement of Pawley-type.
CA
CA The user would be free to use higher values of N, e.g. from 20 downwards,
CA for his own print control.  An example of such use occurs in the main
CA program GRLSQ, in which N=2 is used in a different context from the above.
C
CP IPRNT(N) in /REFINE/ must contain an integer with a value in the
CP range 0-4, as described above.  ICYC in /REFINE/ must be the current
CP cycle number.
C
CD The function PRNCYC is set TRUE if printing is required, taking
CD of both IPRNT(N) and ICYC.
C
      LOGICAL FIRST,LAST
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      IP=IPRNT(N)
C
C IP=0 MEANS NO PRINTING:
      PRNCYC=.FALSE.
      IF (IP .GT. 0) THEN
C
        FIRST=(ICYC .EQ. NCYC1)
C DONE IS SET IF REFINEMENT HAS CONVERGED ACCORDING TO I CONV CARD:
        LAST=(ICYC .EQ. LASTCY) .OR. DONE
        PRNCYC=(IP .EQ. 4) .OR.
     &  ((IP .EQ. 3) .AND. (LAST .OR. FIRST)) .OR.
     &  ((IP .EQ. 2) .AND. LAST) .OR.
     &  ((IP .EQ. 1) .AND. (FIRST))
      ENDIF
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE PUNPAK(KK,IFAM,IGEN,ISPC)
      SUBROUTINE PUNPAK(KK,IFAM,IGEN,ISPC)
C
C *** PUNPAK by JCM 8 Nov 90 ***
C
CX
CC 6C
CH Unpacks a parameter specification from single integer.
CA On entry KK holds packed parameter specification
CA On exit IFAM holds family number
CA         IGEN holds genus number
CA         ISPC holds species number
CP KK  must have been made via a call of KPAK set up by LSETUP
CD Unpacks KK according to bases previously set
CN There is an inverse routine KPAK
C
      DIMENSION LPAK(5)
      COMMON /LSQPAK/KKPACK(10,3)
C
      CALL NPACK(KK,LPAK,3,2,KKPACK)
      IFAM=LPAK(1)
      IGEN=LPAK(2)
      ISPC=LPAK(3)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PUTPAR(A,I,NUM,PAR,BAD)
      SUBROUTINE PUTPAR(A,I,NUM,PAR,BAD)
C
C *** PUTPAR by PJB Aug 91 ***
C
CX
CC 13C
CH Distributes parameters read by RDNUMS amongst individually named
CH variables.
CA On entry A is a vector of length at least NUM
CA          NUM is the number of parameters read by RDNUMS
CA          I is the position of the required parameter in A
CA          BAD is the value to put into PAR if the parameter wasn't read
CA              ie if I > NUM.
CA On exit the value has been set into PAR.
C
      DIMENSION A(1)
      IF (I.LE.NUM) THEN
        PAR=A(I)
      ELSE
        PAR=BAD
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE QARRIN(N,K,F,DF,IEND)
      SUBROUTINE QARRIN(N,K,F,DF,IEND)
C
C *** QARRIN DUMMY by JCM 17 Oct 85 ***
C
CX
CC 2B
CH In the library, simply a dummy routine.  If the user wishes some special
CH new input format for "ARRNGE" type main programs, he provides a new
CH version of QUARRIN.
CA A replacement routine should set:
CA N=serial number of next reflection
CA K= h,k,l for next reflection
CA F=observation
CA DF=standard deviation
CA IEND is a logical, set TRUE if there are no more reflections.
C
CD In ARRNGE and similar main programs, if in /ARRDAT/ IFOR(1)=0, it is
CD expected that the user has included his own version of QUARRIN.
C
      LOGICAL IEND
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE QLSQIN(K,IEND)
      SUBROUTINE QLSQIN(K,IEND)
C
C *** QLSQIN DUMMY by JCM 17 Nov 84 ***
C
CX
CC 6B
CH In the library, simply a dummy routine.  If the user wishes some special
CH new input format for Least Squares routines, he provides a new version
CH QLSQIN.
C
CD Called if data input type 0 is specified.  The user's routine must set:
CD   K(1:3) = h,k,l
CD   LOGICAL IEND = TRUE if there are no more data,
CD  and in /OBSCAL/, OBS=the observation, DOBS=its esd.
C
      LOGICAL IEND
      DIMENSION K(3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
C
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE RADFUN(JAT,IEND)
      SUBROUTINE RADFUN(JAT,IEND)
C
C *** RADFUN updated by PJB/JBF 3 Sep 89 ***
C
CX
CC 4A
CH Reads coefficients for the expansion of an atomic wave function.
C
CA On entry JAT indicates which atom (but is irrelevant if IEND=0)
CA          IEND points to the first character position on the card to read,
CA               unless IEND is -ve, when it is a request for initialisation.
CA               or IEND=0, which requests checking and printing.
C
CD The card has already been read as far as "W atom-name RADF".
CD Entry IEND +ve reads the coefficients in atomic units of IMAX Slater
CD type functions, for the expansion of an atomic wave function as a sun
CD of tems of the form:    F(1,I)*R**N(I)*exp(-F(2,I)*R)
CD In mode 1 the F(1,I) are not normalised (Clementi) and in mode 2 they are
CD (Watson).
CD The numbers given are MODE, N(I,IAT), F(1,I,IAT) AND F(2,I,IAT)
CD
CD Entry IEND=0 converts the coefficients to Angstrom units, and checks for
CD normalisation, for atom JAT.
C
CO Entry IEND=0 writes its findings to unit LPT.
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /RADINT/N(20,5),FF(2,20,5),NTERMS(5),
     & IRADF(5),NRADF
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C%
C      COMMON /SCRAT/MODE(%MPAT%),IROT(%MPAT%),TEMP(3,3),MFUN(10,%MPAT%),LENG(3)
      COMMON /SCRAT/MODE(20),IROT(20),TEMP(3,3),MFUN(10,20),LENG(3)
      DATA S/1.8896/
C
      IF (IEND) 30,20,1
C
C  INITIALISE
  30  NRADF=0
C%
C      CALL JGMZER(NTERMS,1,%RADF%)
      CALL JGMZER(NTERMS,1,5)
      GO TO 100
C
C  READ REST OF CARD STARTING W <ATOM> RADF:
   1  IF (NRADF .EQ. 0 .OR. IRADF(NRADF) .NE. JAT) THEN
C%
C        CALL ERRCHK(2,NRADF,%RADF%,0,
        CALL ERRCHK(2,NRADF,5,0,
     &   'radial functions')
        IRADF(NRADF)=JAT
      ENDIF
      IPT1=IEND
      DO 4 I=1,2
      IPT=IPT1
      MODE(NRADF)=NN
      CALL RDINTG(NN,IPT,IPT1,80,IER)
      IF (IER.NE.0) GO TO 99
    4 CONTINUE
C
      IT=NTERMS(NRADF)
      IT=IT+1
      NTERMS(NRADF)=IT
      N(IT,NRADF)=NN
C
      DO 5 I=1,2
      IPT=IPT1
      CALL RDREAL(FF(I,IT,NRADF),IPT,IPT1,80,IER)
      IF (IER.NE.0) GO TO 99
    5 CONTINUE
      GO TO 100
C
C ERRORS IN READING:
  99  CALL ERRCH2(ICARD(IPT:IPT1-1),2,'cannot interpret','on W card')
      GO TO 100
C
C  ENTRY TO CHECK ONE ATOM AND OUTPUT PARAMETERS
  20  IAT=NFIND(JAT,IRADF,NRADF)
      IF (IAT .GT. 0) IMAX=NTERMS(IAT)
      IF (IAT .EQ. 0 .OR. IMAX .EQ. 0) THEN
      CALL ERRMES(1,1,'No radial wave-function given')
      GO TO 100
      ENDIF
      CALL MESS(LPT,1,'Radial wave function :  Exponent      '//
     & 'Amplitude     Screening')
      CALL MESS(LPT,0,'                           of r'//
     & '                      Constant')
      DO 21 I=1,IMAX
      XI=FF(2,I,IAT)
      A=FF(1,I,IAT)
      FF(2,I,IAT) = S*XI
      XX = FLOAT(N(I,IAT)) + 1.5
      IF (MODE(IAT) .EQ. 1) GO TO 22
      FF(1,I,IAT) = A*(S**XX)
      GO TO 23
   22 XX = 2.0*XX
      Y = (2.*FF(2,I,IAT))**XX
      Y = 1./Y
      Y = Y*FACT(IFIX(XX)-1)
      Y = SQRT(1./Y)
      FF(1,I,IAT) = A*Y
   23 WRITE (LPT,2001) N(I,IAT),A,XI
2001  FORMAT (25X,I5,7X,F10.5,4X,F10.5,3X,E12.5)
   21 CONTINUE
C
C     CHECK NORMALISATION
      SUM = 0.
      DO 24 I = 1,IMAX
      DO 25 J = 1,IMAX
      IX = (N(I,IAT)+N(J,IAT)+3)
      Y = FF(1,I,IAT)*FF(1,J,IAT)*FACT(IX-1)
      Y = Y/((FF(2,I,IAT)+FF(2,J,IAT))**IX)
   25 SUM = SUM + Y
   24 CONTINUE
      Y = ABS(SUM-1.0)
      IF (Y .GT. 0.0005) CALL ERRRE2(SUM,1,
     & 'Function is not normalised','needed as normalisation integral')
  100 RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION RANGE(X,A,B)
      FUNCTION ARANGE(X,A,B)
C
C *** RANGE by JCM 23 Sep 87 ***
C
CX
CC 11C
CH Puts a number into a given range.
CA On entry X is the current value of a real variable.
CA          A and B are the limits between which RANGE is to be set.
CD X must be periodic in (A-B).  A is included in the range, but B excluded.
CN A is not necessarily > or < B.
C
      P=ABS(A-B)
      IF (A .GT. B) THEN
        ALOW=B
        AUP=A
      ELSE
        ALOW=A
        AUP=B
      ENDIF
      R=X
   1  IF (R .LE. AUP) GO TO 2
      R=R-P
      GO TO 1
C
   2  IF (R .GE. ALOW) GO TO 101
      R=R+P
      GO TO 2
C
 101  IF (R .EQ. B) R=A
      ARANGE=R
      RETURN
      END
C
C
C
C
      SUBROUTINE RANMOV(NA,RANDEL)
C
C *** RANMOV by JCM 28 Nov 91 ***
C
CX
CC 4B
CH Makes a change in atomic coordinates, according to the constraints
CA On entry NA=which atom
CA          RANDEL is an array of required shifts, holding 0,1,2 or 3 numbers
CA                 as appropriate.
CP Routine XYZREL must have set up the constraints in /POSREL/.
CD If the atomic position is unconstrained, applies RANDEL(1) to x, RANDEL(2)
CD to y and RANDEL(3) to z.  If there are constraints in /POSREL/, uses these
CD to shift some or all of x,y and z according to these constraints.
CD
CD Takes its shifts in sequence from the array RANDEL.
C
      DIMENSION RANDEL(3)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /POSREL/NXYZ(3,150),XYZ(3,150)
C
      J=0
      NFIR=0
      DO 1 I=1,3
      IF (NXYZ(I,NA) .LE. 0) GO TO 1
      IF (NXYZ(I,NA) .EQ. 9999 .OR. NFIR .EQ. 0) THEN
        J=J+1
        X(I,NA)=X(I,NA)+RANDEL(J)
C FIRST OF A RELATED SET IS MARKED SPECIALLY:
        IF (NXYZ(I,NA) .NE. 9999) THEN
          NFIR=I
          NRAN=J
        ENDIF
      ELSE
        X(I,NA)=X(I,NA)+(XYZ(NFIR,NA)/XYZ(I,NA))*RANDEL(NRAN)
      ENDIF
   1  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE RDANGL(IPT,N1,N2,N3,NCOM,IE)
      SUBROUTINE RDANGL(IPT,N1,N2,N3,NCOM,IE)
C
C *** RDANGL by JCM 15 Oct 90 ***
C
CX
CC 8A
CH Reads a specification of a bond angle, by reading the names of 2 intersecting
CH bonds;  makes the third bond involved.
CA On entry IPT is the position at which to start reading, and it is updated.
CA On exit N1, N2 and N3 specify the 3 bonds making up the triangle containing
CA         the angle.  The angle is opposite N1, enclosed by N2 and N3,
CA         with N2 < N3.
CA         NCOM points to the common atom.
CA         IE is an error indicator - IE=0 if OK, not 0 if error.
C
CD Reads and identifies 2 bond names;  finds their common atom; makes the
CD third bond of the triangle and adds it to the bond list.
C
CO Reports an error if bonds are not in list, or if bonds have no atom in
CO common.
C
      CHARACTER *4 BNAME
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      IE=0
C READ FIRST BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N2=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N2 .LE. 0) GO TO 11
C READ SECOND BOND NAME:
      CALL RDWORD(BNAME,LEN,IPT,IPT,80,0,IER)
      N3=NCFIND(BNAME,BONNAM,NUMBON)
      IF (N3 .GT. 0) GO TO 12
C
  11  CALL ERRCH2(BNAME,2,' ','is not a bond name')
      IE=1
      GO TO 100
C
C FIND THIRD SIDE:
  12  CALL BONTRI(N2,N3,N1,NCOM,IE)
 100  RETURN
      END
C
C
C
C
C LEVEL 4        SUBROUTINE RDATOM(IPT,IA,XACT,ISYMM,ILATT,CS)
       SUBROUTINE RDATOM(IPT,IA,XACT,ISYMM,ILATT,CS)
C
C *** RDATOM updated by JCM 7 Sep 90 ***
C
CX
CC 8A
CH Reads the specification of an atom for slack constraint purposes
CA On entry IPT points to the first character to read
CA On exit IA holds which base atom the new one is related to
CA         XACT holds the actual x,y,z coordinates
CA         ISYMM, ILATT and CS hold the transformation which takes the
CA                   base atom into the actual coordinates
CP On entry ICARD in /SCRACH holds the card read
CD Reads an atom name, which must be the same as one on an A card.
CD This may be followed either by 3 coordinates, x,y,z or by a
CD symmetry operator number, a lattice translation number, and 3
CD cell shifts.
CD
CD In either case, the actual destination atom, related to that on the
CD A card, is identified.
C
      DIMENSION XACT(3),CS(3)
      CHARACTER *4 NAME
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C
C READ NAME OF BASE ATOM:
      CALL RDWORD(NAME,LEN,IPT,IPT,80,0,IER)
      IA=IATOM(NAME)
      IF (IA .LE. 0) THEN
        CALL ERRATM(NAME,1,'on L ATOM card')
        GO TO 100
      ENDIF
C
C READ SYMMETRY NUMBER (POSSIBLY -), LATTICE NUMBER AND 3 CELL TRANSLATIONS:
      IPKEEP=IPT
      ILATT=1
      DO 1 I=1,3
   1  CS(I)=0.
      CALL RDINTG(ISYMM,IPT,IPT,80,IER)
C MAY HAVE READ INTEGER, NOTHING OR DECIMAL NUMBER FOR X COORD:
      IF (IER .NE. -1) GO TO 10
C INSTEAD OF AN INTEGER WE HAVE TRIED TO READ A REAL.  X Y AND Z GIVEN.
      IPT=IPKEEP
      DO 19 I=1,3
  19  CALL RDREAL(XACT(I),IPT,IPT,80,IER)
      CALL XROOT(IA,XACT,ISYMM,ILATT,CS)
      IF (ISYMM .NE. 0) GO TO 2
      WRITE (LPT,3009) ATNAME(IA),XACT
      WRITE (ITO,3009) ATNAME(IA),XACT
3009  FORMAT (/' ERROR ** ',A4,' WILL NOT TRANSFORM INTO ',3F10.4)
      GO TO 99
C
C MAY BE SIMPLE LABEL FOR ATOM AS ON A CARD, OR PART SPEC:
C IF INTEGER READ IT IS SPECIFICATION FOR SYMMETRY OPERATOR:
  10  IF (IER .EQ. 100 .OR. ISYMM .EQ. 0) THEN
        ISYMM=1
        GO TO 2
      ENDIF
C
C CHECK SYMMETRY OPERATOR NUMBER:
      IF (IABS(ISYMM) .GT. NOPC) THEN
        WRITE (LPT,3003) ISYMM,NOPC
        WRITE (ITO,3003) ISYMM,NOPC
3003    FORMAT (/' ERROR ** symmetry operator number',I4,
     &   ' requested but only',I4,' present')
      GO TO 99
      ENDIF
C
      IF (ISYMM .LT. 0 .AND. .NOT. CENTRC) THEN
      CALL ERRMES(1,-1,'-ve symmetry operator in non-centric group')
      GO TO 99
      ENDIF
C
C READ AND CHECK LATTICE NUMBER (ALLOWED TO BE MISSING):
      CALL RDINTG(ILATT,IPT,IPT,80,IER)
      IF (IER .EQ. 100 .OR. ILATT .EQ. 0) GO TO 2
C
      IF (ILATT .GT. NLAT .OR. ILATT .LE. 0) THEN
        WRITE (LPT,3005) ILATT,NLAT
        WRITE (ITO,3005) ILATT,NLAT
3005    FORMAT (/' ERROR ** lattice translation number',I4,
     & ' requested but only',I4,' present')
        GO TO 99
      ENDIF
C
C READ AND CHECK CELL TRANSLATIONS:
      DO 3 K=1,3
      CALL RDREAL(CS(K),IPT,IPT,80,IER)
      IF (IER .NE. 0) GO TO 2
   3  CONTINUE
C
C MAKE ACTUAL COORDS FOR BOTH POSITIONS:
   2  CALL XTRANS(IA,XACT,ISYMM,ILATT,CS)
      GO TO 100
C
  99  IERR=IERR+1
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE RDBOND(IPT,NEND,IE)
      SUBROUTINE RDBOND(IPT,NEND,IE)
C
C *** RDBOND by JCM 15 Oct 1990 ***
C
CX
CC 8A
CH Reads a specification of a bond, by reading the names of the atoms at each
CH end.
CA On entry IPT is a starting position on the card.
CA On exit  NEND is an integer array of 2 elements which holds the numbers
CA             of the ends so identified in the list of L ATOM cards.
CA          IE is an error indicator - on exit IE=0 if OK, not 0 if error.
C
CD Given an L BOND card in /SCRACH/, reads the next 2 atom names.  An atom
CD may belong to the list already given on L ATOM cards, or it may be the
CD name of an atom in the asymmetric unit, on an A card.  In the latter case
CD it is added to the L ATOM list.
C
CO Reports an error if the atom names occur in neither list.
C
      CHARACTER *4 NAME
      DIMENSION NEND(2),CZ(3)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SLKGEC/ATTNAM(500),BONNAM(500),ANGNAM(100),
     & TORNAM(100)
      CHARACTER *4 ATTNAM,BONNAM,ANGNAM,TORNAM
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      IE=0
C READ 2 ATOM NAMES:
      DO 1 J=1,2
      CALL RDWORD(NAME,LEN,IPT,IPT,80,0,IER)
      NEND(J)=0
      IF (NTARNM .GT. 0) NEND(J)=NCFIND(NAME,ATTNAM,NTARNM)
      IF (NEND(J) .GT. 0) GO TO 1
      N=IATOM(NAME)
      IF (N .EQ. 0) THEN
        CALL ERRATM(NAME,2,'L BOND card')
        IE=1
        GO TO 100
      ENDIF
C NAME OFF A CARD USED - ADD IT TO TARGET TABLE:
      CALL GMZER(CZ,1,3)
      CALL ADDATM(ATNAME(N),N,X(1,N),1,1,CZ,NEND(J))
C
   1  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE RDDATA(NUNIT,K,H,F,IN,IOU)
      SUBROUTINE RDDATA(NUNIT,K,H,F,IN,IOU)
C
C *** RDDATA by JCM 5 Apr 89 ***
C
CX
CC 13C
CH Reads in free format h,k,l (possibly floating) and a list of values,
CH allowing for a possible title.
CA On entry NUNIT is the unit from which to read
CA          IN indicates other incoming information:
CA          IN +ve means allow one line of text (recognised by containing
CA             any letter other than E)
CA          IN -ve means expect only numbers - this entry would be used for
CA             reading lines of a files other than the first.
CA          IN absolute value gives maximum number of numbers to read to F
CA On exit H(1:3) contains the first 3 numbers read, real
CA         K(1:3) contains the same numbers, fixed to integers.
CA         F() contains all the subsequent numbers on the line, reals.
CA             (maximum abs(IN) of them)
CA         IOU, absolute value, holds (as its units digit) the number of
CA               numbers read to the array F.  No array bound check is done
CA               on F at present.
CA         IOU is -9999 if end of file reached, so nothing new read
CA         IOU, sign, is +ve if no errors detected
CA                         -ve if some reading error occurred, or title
CA                            out of context.
CA         IOU, absolute value, has 100 added if title read.  In this case
CA                         the routine reads in the next line also.
C
CD Ignores empty lines.  If IN > 0 and finds text line, adopts it as a title
C
      DIMENSION H(3),K(3),F(1)
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IOU=-9999
      READTI=0
   4  READ (NUNIT,1000,END=100) ICARD
1000  FORMAT (A80)
      IPT=1
      IOU=1
      L=LENGT(ICARD)
      IF (L .EQ. 0) GO TO 4
      DO 3 I=L,1,-1
      M=LETTER(ICARD(I:I))
      IF (M .EQ. 5) GO TO 3
      IF (M .GT. 0) THEN
C ASSUME TITLE GIVEN:
      IF (IN .LT. 0 .OR. READTI .EQ. 1) CALL ERRMES(1,0,
     &  'letters on data file')
      CALL INPUTN(-1)
      READTI=1
      GO TO 4
      ENDIF
   3  CONTINUE
C
      DO 2 I=1,3
   2  CALL RDREAL(H(I),IPT,IPT,80,IER)
      CALL INDFIX(H,K)
C READ AS MANY NUMBERS AS REMAIN:
      I=0
   1  I=I+1
      IF (I .GT. IABS(IN)) GO TO 5
C READ FIRST IN CASE EMPTY:
      CALL RDREAL(A,IPT,IPT,80,IER)
      IF (IER .EQ. 0) THEN
        F(I)=A
        GO TO 1
      ENDIF
      IF (IER .NE. 100) IOU=-1
C
   5  IOU=IOU*(I-1+100*READTI)
 100  RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE RDFV
      SUBROUTINE RDFV
C
C *** RDFV updated by JCM 4 Jul 88 ***
C
CX
CC 6A
CH Reads all the user's L FIX and L VARY cards in sequence.
C
CD The cards read here start either L FIX or L VARY.  Next come as many
CD parameter specifications as the user wishes.  A parameter specification
CD in this context is one of:
C
CD   ONLY
CD   ALL <F>                       where <F> is a family name
CD   ALL <G>                       where <G> is genus name
CD   ALL <S>                       where <S> is species name.
CD   ALL <W>                       where <W> is a word for a
CD                                  number of species as set up by the
CD                                  main program, e.g. "XYZ".
CD   <S>
CD   <G> <S>
CD    <G> <W>
CD
CD Examples:
CD L FIX ONLY SCAL 1  TFAC
CD L VARY ALL SITE
CD L VARY NA1 X  NA2 XYZ  O1 ITF
CD L FIX ALL BIJ
CD L VARY ONLY C1 B11   C1 B22   C1 B33   C2 XYZT  ALL C3
CD L VARY ALL FAM1
CD
CD In identifying a parameter, RDFV then stores the instruction to fix
CD or vary it accordingly.  Such instructions are not actually certain
CD to be used until the routine VARMAK is called, which surveys them all
CD and may discard some in favour of others.
C
CN It should be noted that even in this "general" routine used by all
CN LSQ programs, family 1 genus 1 is special, in having no genus name
CN (so that it includes items with single names, e.g. "TFAC").  Also
CN family 2 is special, being the crystal structure parameters.
CN When an atom name is found, it is taken to be a genus name in family 2.
C
CI Reads in L FIX and L VARY cards.
C
      CHARACTER *4 NFV(2)
      LOGICAL FX
      COMMON /CELFIX/IPTCEL(6),AMCELL(6),NCELF,NCELG,NCELS,KOM3
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LINKAG/NUMFV,NUMPAK,KKFV(200),KTYPFV(200),KSTFV(200),
     & KTIME(200),KUNPFV(5,30),NTIME,NUMCON,KKCON(500),AMCON(500),
     & KPTCON(201),KSTCON(200),KTPCON(200)
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      DATA NFV/'FIX','VARY'/
C
C COUNT CARDS - MAY BE NONE:
      IN=0
C LOOK FOR EITHER L FIX OR L VARY:
   8  CALL CDSCAN('L',NFV,2,IN,L,NW)
      IF (L .LE. 0) GO TO 100
      FX=(NW .EQ. 1)
      IN=L
      IPT=7
C
C READ NEXT PARAMETER SPECIFICATION ON CARD:
   7  CALL PARRD(IPT,IPT,KK,IFAM,IGEN,ISPC)
C KK ZERO MEANS NO MORE ON THIS CARD:
      IF (KK .EQ. 0) GO TO 8
C
C KK POSITIVE IS A PARAMETER SPEC.  ALL OF IFAM,IGEN AND ISPC SHOULD BE
C NON-ZERO.  ANY MORE COMPLICATED SPECS (LIKE 'ALL X' OR 'ALL FAM1') HAVE COME
C OUT WITH KK NEGATIVE.
C
      IF (KK .LT. 0) GO TO 1
C WE HAVE WHOLE SPEC - FIX OR VARY THE PARAMETER AS REQUIRED:
      CALL FVKPAK(KK,4,FX)
C IF VARY, NOW DO AND ALL WHO SAIL IN HER:
      IF (FX) GO TO 7
      DO 4 I=1,NUMCON
      IF (KPTCON(I+1)-KPTCON(I) .NE. 2) GO TO 4
      KK1=KKCON(KPTCON(I))
      KK2=KKCON(KPTCON(I)+1)
      IF (KK2 .EQ. KK) GO TO 15
      KK2=KKCON(KPTCON(I))
      KK1=KKCON(KPTCON(I)+1)
      IF (KK2 .NE. KK) GO TO 4
C VARIED PAR IS SIMPLY RELATED TO ANOTHER: VARY THAT TOO
  15  CALL FVKPAK(KK1,4,FX)
   4  CONTINUE
      GO TO 7
C
C KK LARGE & -VE MEANS WORDS  LIKE ONLY, ALL, XYZ BIJ - BRANCH:
   1  I100=-KK-98
C 1=ONLY  2=ALL  3=XYZ IN SFLSQ ETC, 4=BIJ DITTO, 5=XYZT DITTO
      GO TO (11,12,13,14,13,13,13) , I100
C UNFORSEEN VALUE:
      CALL ERRMES(1,0,'unforseen word in RDFV')
C
C 'ONLY'
  11  IF (IONLY(JPHASE) .NE. 0) GO TO 7
      IONLY(JPHASE)=1
      IF (.NOT. FX) IONLY(JPHASE)=2
      GO TO 7
C
C 'ALL'
  12  IF (IFAM .EQ. 0) THEN
        WRITE (LPT,3000) KK,IFAM,IGEN,ISPC
        WRITE (ITO,3000) KK,IFAM,IGEN,ISPC
3000    FORMAT (/' *** PROGRAM ERROR in RDFV ** ''ALL'' read, but',
     &   ' KK,IFAM,IGEN,ISPC=',4I5)
C>>JCC HAndle through extra function
C Was      STOP
C Now
       CALL BMBOUT
       RETURN
C
      ENDIF
C
C -VE IFAM MEANS "ALL <W>" READ, LIKE ALL BIJ:
      IF (IFAM .GT. 0) GO TO 29
      I1=IABS(IFAM)
      GO TO (21,22,21,24,25,21) , I1
      CALL ERRMES(1,0,'unforseen word after ALL in RDFV')
C
C ALL XYZB:
  25  L1=1
      GO TO 19
C
C ALL XYZ OR ALL XYZT OR ALL XYZS:
  21  DO 30 J=1,3
  30  CALL FIXVAR(FX,2,0,J,KPHASE,KSOURC,4)
      IF (I1 .EQ. 3) CALL FIXVAR(FX,2,0,12,KPHASE,KSOURC,4)
      IF (I1 .EQ. 6) CALL FIXVAR(FX,2,0,11,KPHASE,KSOURC,4)
      GO TO 7
C
C ALL BIJ:
  22  L1=4
  19  DO 32 J=L1,9
  32  CALL FIXVAR(FX,2,0,J,KPHASE,KSOURC,4)
      GO TO 7
C
C ALL CELL:
  24  DO 34 J=1,6
  34  CALL FIXVAR(FX,NCELF,NCELG,NCELS+J-1,KPHASE,KSOURC,4)
      GO TO 7
C
  29  IF (ISPC .NE. 0) GO TO 3
      IF (IGEN .NE. 0) GO TO 10
C FIX ALL OF FAMILY:
      CALL FIXVAR(FX,IFAM,0,0,KPHASE,KSOURC,4)
      GO TO 7
C
C OPERATE ON ALL SPECIES OF GIVEN GENUS:
  10  CALL FIXVAR(FX,IFAM,IGEN,0,KPHASE,KSOURC,4)
      GO TO 7
C
C KNOWN FAMILY, UNKNOWN GENUS:
   3  CALL FIXVAR(FX,IFAM,0,ISPC,KPHASE,KSOURC,4)
      GO TO 7
C
C <G> XYZ OR <G> XYZT :
  13  L1=3
      IF (I100 .EQ. 6) L1=9
      DO 5 I=1,L1
   5  CALL FIXVAR(FX,IFAM,IGEN,I,KPHASE,KSOURC,4)
      IF (I100 .EQ. 5) CALL FIXVAR(FX,IFAM,IGEN,12,KPHASE,KSOURC,4)
      IF (I100 .EQ. 7) CALL FIXVAR(FX,IFAM,IGEN,11,KPHASE,KSOURC,4)
      GO TO 7
C
C <G> BIJ:
  14  DO 6 I=4,9
   6  CALL FIXVAR(FX,IFAM,IGEN,I,KPHASE,KSOURC,4)
      GO TO 7
C
C
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE RDINTG(N,IPT1,IPT2,IPTEND,IER)
      SUBROUTINE RDINTG(N,IPT1,IPT2,IPTEND,IER)
C
C *** RDINTG by JCM 10 Oct 83 ***
C
CX
CC 13C
CH Reads an integer in free format from a character string.
CA On entry IPT1 points to the first character in the string to consider
CA          IPTEND points to the last to be considered
CA On exit  N is the integer which was read
CA          IPT2 points to the first character after the terminating
CA               character, unless there was an erroneous character,
CA               in which case IPT2 points to it.
CD          IER = 0 if no errors were found
CD              = 100 if N=0 was derived from all spaces (so note IER
CD                   non-zero is not always indicative of a fatal error)
CD              = number in range 1-52 if a letter was found (the value of
CD               IER indicates which letter)
CD              = small -ve number (being an address in table ISMBOL in
CD               COMMON /CHARS/) if a symbol was found out of context.
CD              =-100 if the character found did not occur in any table
CP Before entry the character string from which the integer is to be read
CP is read into /SCRACH/
CD The string is expected to contain only digits, a possible sign,
CD and a space or a comma to terminate.(The number is also terminated
CD after the character pointed to by IPTEND.)
C
      CHARACTER *1 IC
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IPT=IPT1
      IER=0
      N=0
      ISIG=0
C ISIG SAYS WHETHER ANYTHING SIGNIFICANT YET READ, THEN HOLDS SIGN
   1  IF (IPT .GT. IPTEND) GO TO 101
      IC=ICARD(IPT:IPT)
C IGNORE INITIAL SPACES;  TERMINATE ON FINAL SPACE:
      IF ((IC .EQ. ' ') .AND. (ISIG .EQ. 0)) GO TO 3
      IF ((IC .EQ. ' ') .AND. (ISIG .NE. 0)) GO TO 102
C TEST FOR DIGIT FIRST, AS MOST LIKELY CHARACTER TO OCCUR:
      N1=NDIGIT(IC)
      IF (N1 .LT. 0) GO TO 2
C
C DIGIT:
      IF (ISIG .EQ. 0) ISIG=1
      N=N*10+N1
C THIS OVERFLOWS IF TOO BIG A NUMBER IS GIVEN
      GO TO 3
C
C NEITHER DIGIT NOR SPACE:
   2  N2=NSYMBL(IC)
C JUMP IF SYMBOL (NOT LETTER):
      IF (N2 .GT. 0) GO TO 6
C IF HERE, ERROR, PROBABLY LETTER:
      L=LETTER(IC)
      IF (L .GT. 0) IER=L
      IF (L .EQ. 0) IER=-100
      GO TO 101
C
C SYMBOL: + AND - EXPECTED AT START;  COMMA COULD TERMINATE
   6  IF (N2 .EQ. 2) GO TO 102
      IF ((N2 .EQ.14) .AND. (ISIG .EQ.0)) GO TO 8
      IF ((N2 .EQ. 15) .AND. (ISIG .EQ.0)) GO TO 3
      IER=-N2
      GO TO 101
C
C MINUS:
   8  ISIG=-1
C
C NEXT CHARACTER:
   3  IPT=IPT+1
      GO TO 1
C
 102  IPT=IPT+1
 101  N=N*ISIG
      IPT2=IPT
      IF ((ISIG .EQ. 0) .AND. (IER .EQ. 0)) IER=100
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE RDNUMS(A,IPT1,NBOUND,NUM,IER)
      SUBROUTINE RDNUMS(A,IPT1,NBOUND,NUM,IER)
C
C *** RDNUMS corrected by JCM 23 Apr 92 ***
C
CX
CC 13C
CH Reads all the numbers on a line in free format.
CA On entry IPT1 points to the first character in the string to be considered
CA On exit NUM is the number of numbers read
CA         A(1:NBOUND) is an array into which NUM numbers have been read.
CA         IER = 1 if any non-reals read, except "STEP", (and stops reading)
CA         IER = 2 if more than NBOUND numbers were read
CA         IER = 3 if a zero STEP length is requested
CA         IER = 4 if a negative number of STEPS is requested
C
CP Before entry the character string (maximum 80 characters) from which
CP the numbers are to be read must be in the character array in /SCRACH/.
C
CD The character string may contain just a simple string of numbers or
CD may have anywhere the word STEP followed by 3 numbers.  This is treated
CD like a FORTRAN "DO" loop:   the first number is an initial value
CD                             the second number is a final value
CD                             the third number is a step length
CD All the values implied by the "STEP" function are put into array A
CD The "STEP" length may be negative so long as the final value is less than
CD the initial one.
C
CD Stops when it has only blank card left, so it reads any non-blank
CD numbers no matter where they are on the card.  It cannot be used for
CD the (little used) facility which allows fixed format in which blanks
CD mean zeros.
C
CN Beware rounding error in the use of "STEP".
C
C STOPS WHEN IT HAS ONLY BLANK CARD LEFT, SO IT READS ANY NON-BLANK NUMBERS NO
C MATTER WHERE THEY ARE ON THE CARD, & IS NO USE FOR "C" CARDS.
C
C
      CHARACTER *4 IWORD
      LOGICAL TESTOV
      DIMENSION A(NBOUND)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IPT=IPT1
      IER=0
      NUM=0
   1  CALL RDREAL(X,IPT,IPT,80,IE)
      IF ((IE .EQ. 100) .OR. IPT .GT . 80) GO TO 100
      IF (IE .EQ. 0) GO TO 2
C
C IF HERE, IT IS EITHER "STEP" OR AN ERROR:
      IF (IE .NE. 19) GO TO 4
      IP=IPT
      CALL RDWORD(IWORD,IWDLEN,IP,IPT,IP+3,0,IE)
      IF (IWORD .NE. 'STEP') GO TO 4
      CALL RDREAL(X,IPT,IPT,80,IE)
      IF (IE .NE. 0) GO TO 4
      CALL RDREAL(Y,IPT,IPT,80,IE)
      IF (IE .NE. 0) GO TO 4
      CALL RDREAL(Z,IPT,IPT,80,IE)
      IF (IE .NE. 0) GO TO 4
C
      IF (Z .GT. 0.) GO TO 7
      Z=-Z
      X1=X
      X=Y
      Y=X1
C
   7  IF (.NOT. TESTOV(Y,Z)) GO TO 6
      WRITE (LPT,3002) X,Y,Z
      WRITE (ITO,3002) X,Y,Z
3002  FORMAT (' ERROR ** zero step length from',3F10.4)
      IER=3
      GO TO 100
C
   6  NPT=NINT((Y-X+10.E-5)/Z)+1
      IF (NPT .GE. 1) GO TO 8
      WRITE (LPT,3003) X,Y,Z
      WRITE (ITO,3003) X,Y,Z
3003  FORMAT (' ERROR ** step',3F10.4,' gives -ve number of points')
      IER=4
      GO TO 100
C
   8  N1=NUM+NPT
      IF (N1 .GT. NBOUND) GO TO 5
      DO 9 I=1,NPT
   9  A(NUM+I)=X+FLOAT(I-1)*Z
      NUM=N1
      GO TO 1
C
C CANNOT MAKE SENSE OF CARD:
   4  IER=1
      CALL ERRIN2(IPT,2,'cannot interpret card at point',
     &  'expecting real number or "STEP"')
      GO TO 100
C
   2  NUM=NUM+1
      IF (NUM .LE. NBOUND) GO TO 3
   5  CALL ERRIN2(NBOUND,2,'more than','numbers read by RDNUMS')
      IER=2
      GO TO 100
C
   3  A(NUM)=X
      GO TO 1
C
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE RDREAL(X,IPT1,IPT2,IPTEND,IER)
      SUBROUTINE RDREAL(X,IPT1,IPT2,IPTEND,IER)
C
C *** RDREAL updated by JCM 11 Sep 91 ***
C
CX
CC 13C
CH Reads a real number in free format from a character string.
CA On entry IPT1 points to the first character in the string to consider
CA          IPTEND points to the last to be considered
CA On exit  X is the real number read,
CA          IPT2 points to the first character after the terminating character
CA               unless there was an erroneous character, in which case
CA               IPT2 points to it.
CA          IER =   0 if no errors were found
CA              = 100 if X=0 was derived from all spaces (so note IER
CA                    non-zero is not always indicative of a fatal error)
CA              = number in range 1-52 if a letter was found (the value of
CA                IER indicates which letter)
CA              = small -ve number (being an address in table ISMBOL in
CA                COMMON /CHARS/) if a symbol was found out of context.
CA              =-100 if the character found did not occur in any table
CA              =-101 if number of form N/M but M=0
CA              =-102 if more than 8 digits after decimal point
CP Before entry the character string (maximum 80 characters) from which
CP the number is to be read must be in the character array ICARD in /SCRACH/.
CD The string is expected to contain only digits, a possible sign,
CD a possible decimal point and a space or a comma to terminate.(The
CD number is also terminated after the character pointed to by IPTEND.)
CD A comma by itself will be read as 0. (So, e.g., on C cards, tetragonal
CD may come as 4.3,,4.5)
CD The routine will also read numbers in the form M/N (e.g. "2/3"),
CD and integers without the decimal point.
C
      CHARACTER *1 IC
      DOUBLE PRECISION TENTBL,XX,YY
      DIMENSION TENTBL(8)
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA TENTBL/0.1D0,0.01D0,0.001D0,0.0001D0,0.00001D0,
     & 0.000001D0,0.0000001D0,0.00000001D0/
C
      IPT=IPT1
      IER=0
      XX=0.
      ISIG=0
      SIG=1.
C ISIG SAYS WHETHER ANYTHING SIGNIFICANT YET READ, SIG HOLDS SIGN
      IPSH=0
C IPSH SAYS WHETHER POINT OR SLASH READ
   1  IF (IPT .GT. IPTEND) GO TO 5
      IC=ICARD(IPT:IPT)
C IGNORE INITIAL SPACES;  TERMINATE ON FINAL SPACE:
      IF ((IC .EQ. ' ') .AND. (ISIG .EQ. 0)) GO TO 3
      IF ((IC .EQ. ' ') .AND. (ISIG .NE. 0)) GO TO 15
C TEST FOR DIGIT FIRST, AS MOST LIKELY CHARACTER TO OCCUR:
      N=NDIGIT(IC)
      IF (N .LT. 0) GO TO 2
C
C DIGIT:
      XX=10.*XX+FLOAT(N)
      GO TO 17
C
C NEITHER DIGIT NOR SPACE:
   2  N=NSYMBL(IC)
C JUMP IF SYMBOL (NOT LETTER):
      IF (N .GT. 0) GO TO 6
C IF HERE, ERROR, PROBABLY LETTER:
      L=LETTER(IC)
      IF (L .GT. 0) IER=L
      IF (L .EQ. 0) IER=-100
      GO TO 5
C
C SYMBOL: + AND - EXPECTED AT START;  COMMA COULD TERMINATE
C COULD ALSO BE DECIMAL POINT OR SLASH:
   6  IF (N .EQ. 2) GO TO 15
      IF ((N .EQ.14) .AND. (ISIG .EQ.0)) GO TO 8
      IF ((N .EQ. 15) .AND. (ISIG .EQ.0)) GO TO 17
      IF ((N .EQ. 1) .AND. (IPSH .EQ. 0)) GO TO 7
      IF ((N .EQ. 10) .AND. (IPSH .EQ. 0)) GO TO 9
      IER=-N
      GO TO 5
C
C DECIMAL POINT:
   7  IPSH=1
C NOW READ FRACTION:
      GO TO 10
C
C SLASH:
   9  IPSH=-1
C NOW READ DENOMINATOR:
      GO TO 10
C
C MINUS:
   8  SIG=-1.
C
C SET "SOMETHING OTHER THAN SPACE READ"
  17  ISIG=1
C
C NEXT CHARACTER:
   3  IPT=IPT+1
      GO TO 1
C
C READ EITHER FRACTIONAL PART OR DENOMINATOR:
  10  YY=0.
      ISIG=1
      IT=0
  11  IPT=IPT+1
      IF (IPT .GT. IPTEND) GO TO 5
      IC=ICARD(IPT:IPT)
C FIRST LOOK FOR DIGITS:
      N=NDIGIT(IC)
      IF (N .LT. 0) GO TO 12
C DIGIT:
      IT=IT+1
      IF (IT .LE. 8) GO TO 13
      IER=-102
      GO TO 5
  13  YY=FLOAT(N)*TENTBL(IT) + YY
      GO TO 11
C SPACE TERMINATES:
  12  IF (IC .EQ. ' ') GO TO 15
C OTHERWISE, COMMA TERMINATES AND EVERYTHING ELSE ERROR:
      NS=NSYMBL(IC)
      IF (NS .EQ. 2) GO TO 15
      L=LETTER(IC)
      IF (NS .GT. 0) IER=-NS
      IF (L .GT. 0) IER=L
      IF (IER .EQ. 0) IER=-100
      GO TO 5
C
C END - XX HAS LEFT HAND PART,  POSSIBLY YY HAS RIGHT HAND PART:
  15  IPT=IPT+1
   5  IF (IPSH .EQ. 1) XX=XX+YY
      IF (IPSH .NE. -1) GO TO 101
C DEAL WITH N/M:
      IF (YY .GT. 0.) GO TO 16
      IF (IER .EQ. 0) IER=-101
      GO TO 101
  16  XX=XX/(YY*10.**IT)
 101  X = SNGL(XX)*SIG
      IPT2=IPT
      IF ((ISIG.EQ.0) .AND. (IER .EQ. 0) .AND. (IC .NE. ',')) IER=100
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE RDRELA
      SUBROUTINE RDRELA
C
C *** RDRELA updated JCM 13 Jan 88 ***
C
CX
CC 6A
CH Reads and interprets all user-supplied L RELA cards for constraints.
C
CD Deals with L RELA cards of type:
CD   L RELA 1  : followed by <a1> <p1>  <a2> <p2> where a1, a2 are
CD               constants and p1, p2 are parameter specifications.
CD               This is to be interpreted as:
CD               a1 x shift in p1 = a2 x shift in p2
CD
CD   L RELA 2  : followed by a string of <a1> <p1>  <a2> <p2>  ,a3> <p3> . .
CD               for as many as are needed.  This is to be interpreted as:
CD    a1 x shift in p1 + a2 x shift in p2 + a3 x shift in p3 + etc = 0.
CD
CD For each card the relation is read and stored.  It is not actually
CD absorbed until routine VARMAK is obeyed, when it may be modified in
CD the light of other FIX, VARY or RELA instructions.
C
CN Note that types 1 and 2 are not identical for a relation involving just
CN two parameters.
C
      DIMENSION AM(10),KK1(10)
C
C SET NO TYPE 2 CONSTRAINTS:
C
C READ ALL 'L RELA' CARDS:
      IN=0
   1  CALL FINDCD('L','RELA',4,IN,L)
      IF (L .LE. 0) GO TO 100
C INTERPRET NEXT CARD:
      IN=L
      CALL RDINTG(IRTYP,7,IPT,80,IER)
      IF (IRTYP .LE. 0 .OR. IRTYP .GE. 3) THEN
      CALL ERRIN2(IRTYP,2,'Relation type','not implemented')
      GO TO 1
      ENDIF
C
   3  GO TO (11,12) , IRTYP
C
C TYPE 1 CONSTRAINT - A1 TIMES P1 = A2 TIMES P2
  11  CALL RDREAL(AM(1),IPT,IPT,80,IER)
      CALL PARRD(IPT,IPT,KK1(1),IFAM1,IGEN1,ISPC1)
C PARAMETER SPEC INTO KK1(1) - MUST BE A GENUS NAME+SPECIES NAME SO KK1(1) +VE:
      IF (KK1(1) .LE. 0) GO TO 99
C
      CALL RDREAL(AM(2),IPT,IPT,80,IER)
      CALL PARRD(IPT,IPT,KK1(2),IFAM2,IGEN2,ISPC2)
      IF (KK1(2) .LE. 0) GO TO 99
      AM(2)=-AM(2)
      CALL ADDCON(2,KK1,AM,1)
      GO TO 1
C
C TYPE 2 - A LINEAR COMBINATION OF PARAMETERS, WITH CONSTANT COEFFICIENTS,
C          MUST BE CONSTANT.  ALL PARAMETERS ON ONE CARD - CONSTANTS FIRST
C READ AS MANY PAIRS OF CONSTANT, PARAMETER SPEC AS GIVEN:
  12  IPARS=0
   5  IPARS=IPARS+1
      CALL RDREAL(AM(IPARS),IPT,IPT,80,IER)
      IF (IER .EQ. 100) GO TO 21
      CALL PARRD(IPT,IPT,KK1(IPARS),IFAM,IGEN,ISPC)
      IF (KK1(IPARS) .LE. 0) GO TO 99
      GO TO 5
C
C NOW ABSORB TYPE 2 CONSTRAINT:
  21  CALL ADDCON(IPARS-1,KK1,AM,4)
      GO TO 1
C
C ERRORS:
  99  CALL ERRIN2(IPT,2,'cannot interpret L RELA card at point',' ')
      GO TO 1
C
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE RDWORD(WORD,IWDLEN,IPT1,IPT2,IPTEND,IANY,IER)
      SUBROUTINE RDWORD(WORD,IWDLEN,IPT1,IPT2,IPTEND,IANY,IER)
C
C *** RDWORD updated by JCM 28 Apr 90 ***
C
CX
CC 13C
CH Reads the next word from a character string.
C
CA WORD    is *(*), and set on exit to contain the next readable word
CA IWDLEN  integer, is set to the total number of characters read
CA On entry IPT1 points to the first character position to consider in ICARD
CA          IPTEND points to the last character position to consider.
CA          IANY indicates whether or not the "word" may start with a
CA               a non-letter.  If IANY is negative, any character may
CA               start the word, but if IANY = 0 it must start with a letter.
CA               If IANY is positive, a special entry used by multi-source
CA               multiphase Profile Refinement is invoked.  If *Sn or *Pn
CA               (n an integer) is read where a word is expected, n is
CA               transferred to KSOURC or KPHASE as appropriate, and the
CA               NEXT word is read as normal.
CA
CA On exit WORD contains the next readable word in ICARD terminated by
CA              a space.
CA         IWDLEN is set to the total number of characters read
CA         IPT2 points to the character position in ICARD after
CA              the one which terminated the word.
CA          IER is an error indicator:
CA IER =   0  no errors found
CA IER =   100 word contained all spaces (ie nothing left in field to consider)
CA IER =   number in range 1-10: initial character of word is a digit (IANY>=0)
CA IER =   small negative number: initial symbol out of context (the
CA         number is an address in the table ISMBOL in /CHARS/)
CA IER =  -100: initial symbol out of context, not found in any table
C
CP Expects ICARD in /SCRACH/ to contain enough characters.
CD Ignores spaces till a non-space;  then absorbs word until it finds
CD      either a space, or
CD      the end of the permitted field as given in IPTEND, or
CD      WORD is full (in which case it reads characters and counts them
CD      in IWDLEN, but does not store them
C
      CHARACTER *(*) WORD
      CHARACTER *1 IC
      LOGICAL SAID
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
      MAX=LEN(WORD)
      IPT=IPT1
   4  WORD=' '
      IER=0
      IWDLEN=0
   1  IF (IPT .GT. IPTEND) GO TO 101
      IC=ICARD(IPT:IPT)
C IGNORE INITIAL SPACES;  TERMINATE ON FINAL SPACE:
      IF ((IC .EQ. ' ') .AND. (IWDLEN .EQ. 0)) GO TO 3
      IF ((IC .EQ. ' ') .AND. (IWDLEN .NE. 0)) GO TO 102
C TEST FOR LETTER FIRST, AS MOST LIKELY CHARACTER TO OCCUR:
      N1=LETTER(IC)
      IF (N1 .LE. 0) GO TO 2
C
C LETTER, OR SUBSEQUENT NON-SPACE:
   5  IWDLEN=IWDLEN+1
      IF (IWDLEN .LE. MAX) WORD(IWDLEN:IWDLEN)=IC
      GO TO 3
C
C NEITHER LETTER NOR SPACE:
C IF INITIAL LETTER READ, OR IF IER IS -VE ON ENTRY,  ANY SYMBOL IS  ACCEPTABLE:
   2  IF (IWDLEN .GT. 0 .OR. IANY .LT. 0) GO TO 5
      N1=NSYMBL(IC)
C JUMP IF SYMBOL (NOT LETTER):
      IF (N1 .GT. 0) GO TO 6
      N1=NDIGIT(IC)
      IF (N1 .GT. 0) IER=N1
C BY NOW IT IS AN UNRECOGNISABLE SYMBOL:
      IF (N1 .EQ. 0) IER=-100
      GO TO 101
C
C SYMBOL AT START OF WORD:
C
   6  IF (IANY .GT. 0) THEN
        IF (N1 .EQ. 17) THEN
          IF (SAID(ICARD(IPT+1:IPT+1),'P')) THEN
            CALL RDINTG(KPHASE,IPT+2,IPT,80,IER)
            GO TO 4
          ELSE IF (SAID(ICARD(IPT+1:IPT+1),'S')) THEN
            CALL RDINTG(KSOURC,IPT+2,IPT,80,IER)
            GO TO 4
          ENDIF
        ENDIF
      ENDIF
      IER=-N1
      GO TO 101
C
C NEXT CHARACTER:
   3  IPT=IPT+1
      GO TO 1
C
 102  IPT=IPT+1
 101  IF ((IWDLEN .EQ. 0) .AND. (IER .EQ. 0)) IER=100
      IPT2=IPT
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE RDWRDS(WD,IPT1,NBOUND,NUM,IER)
      SUBROUTINE RDWRDS(WD,IPT1,NBOUND,NUM,IER)
C
C *** RDWRDS by JCM 1 Oct 86 ***
C
CX
CC 13C
CH Reads all the words on a line from column 3 onwards.
C
CA On entry IPT1 points to the first character in ICARD to read
CA          NBOUND is the dimension of the array WD
CA On exit WD is an A4 array which holds all the words read.
CA         NUM is the number of words read (it stops at a non-word)
CA IER = 1 on exit if a non-word read (starting with a non-letter)
CA IER = 2 on exit if the array is overfilled
C
CP The card must have already been read into ICARD in /SCRACH/, usually
CP by CARDIN
C
      CHARACTER *4 WD(NBOUND)
C
      IPT=IPT1
      IER=0
      NUM=0
   1  CALL RDWORD(WD(NUM+1),LEN,IPT,IPT,80,-1,IE)
      IF (IE .EQ. 100 .OR. IPT .GE. 80) GO TO 100
      IF (IE .EQ. 0) GO TO 2
      IER=1
      GO TO 100
C
   2  NUM=NUM+1
      IF (NUM .LE. NBOUND) GO TO 1
      CALL ERRIN2(NBOUND,2,'array full - ','items, in RDWRDS')
      IER=2
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE REAORB(CORB,ORB,KMAX)
      SUBROUTINE REAORB(CORB,ORB,KMAX)
C
C *** REAORB by PJB Oct 88 ***
C
CX
CC 18A
CH To change the orbital basis from Ylm and Yl-m to (Ylm +- Yl-m).
C
      COMPLEX CORB(KMAX,KMAX)
      DIMENSION ORB(KMAX,KMAX)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMPLEX FAC1,FAC2,D(169),D2(169),D3(169)
C
      K=KMAX/2
      RTHF=SQRT(0.5)
      FAC2=CMPLX(RTHF,0.)
      FAC1=FAC2*(-1)**K
      CALL CGMZER(D,KMAX,KMAX)
C
      I=K+1
      DO 1 M=-K,K
      IM=IABS(M)
      IF (M.EQ.0) THEN
        FAC1=CMPLX(0.,RTHF)
        FAC2=FAC1
        D(I)=CMPLX(1.,0.)
      ELSE
      D(I-IM)=FAC1
      D(I+IM)=FAC2
      FAC1=-FAC1
      ENDIF
      I=I+KMAX
    1 CONTINUE
      IF (IOUT.GT.220) THEN
        L=0
        CALL MESS(LPT,1,'Transformation Matrix:')
        DO 25 I=1,KMAX
        WRITE (LPT,2001) (D(J),J=L+1,L+KMAX)
2001    FORMAT (12(1X,7(2F8.4,2X)/))
        L=L+KMAX
   25   CONTINUE
      ENDIF
      CALL CGMPRD(CORB,D,D2,KMAX,KMAX,KMAX)
      CALL TRANSC(D2,KMAX)
      CALL CGMPRD(D2,D,D3,KMAX,KMAX,KMAX)
      L=0
      II=1
      DO 5 I=1,KMAX
      DO 5 J=1,KMAX
      ORB(I,J)=REAL(D3(II))
      IF (ABS(AIMAG(D3(II))).GT..0001) THEN
        WRITE (LPT,3000) I,J,AIMAG(D3(II))
        WRITE (ITO,3000) I,J,AIMAG(D3(II))
3000    FORMAT (/' ERROR ** in REAORB - imaginary part of element',
     &  2I3,' is non-zero; value is ',F8.4)
      ENDIF
      II=II+1
    5 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE RECELL(N,M)
      SUBROUTINE RECELL(N,M)
C
C *** RECELL by JCM 13 Jul 83 ***
C
CX
CC 1B
CH Makes real or reciprocal space cell parameters from the others.
CA On entry:
CA   N=1 means make real space parameters; N=2 reciprocal
CA   M=1 means start from the 6 quadratic products in CPARS, A,B,C,D,E,F
CA       if real or A*,B*,C*,D*,E*,F* if reciprocal.
CA       ( A=a squared, B=b squared, D=b c cos alpha, etc.,
CA       and A*=a* squared, B*=b* squared, D*=b* c* cos alpha*, etc.,
CA       where a,b,c, etc are the cell parameters in real space,
CA       a*,b*,c* etc are in reciprocal.
CA
CA   M=2 means start from cell parameters.
C
CD The remaining annotation assumes, for the sake of clarity,
CD that N=1 and M=1.
CD
CD Accepts in /CELPAR/ the 6 quadratic products A* B* C* D* E* F*;
CD Makes first the usual cell parameters a* b* c* alpa* beta* gamma*,
CD then cos alpha*, cos beta*, cos gamma*.
CD (join here if on entry M=2)
CD Makes sin alpha*, sin beta*, sin gamma*:
CD Then makes the 9 corresponding quantities in real space, a,b,c,
CD cos alpha, cos beta, cos gamma, sin alpha, sin beta, sin gamma.
CD
CD Makes the volumes of both real & reciprocal cells in V(1) and V(2).
CD Now for entry M=1, we already have one set of quadratic products;
CD make the other set, and for entry M=2, make both sets.
CD
CD Finally forms the orthogonal matrices used in transformation of axes,
CD by a call of ORTHG.
CD
CD Called at end of RECIP (N=2,M=2), and at the end of a LSQ cycle which
CD refines cell parameters (N=1,M=1)
C
CO Writes its findings on unit LPT.
C
      DOUBLE PRECISION C
      DIMENSION ANGLE(3,2)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      NOUT=N
      MM=M
      IF ((NOUT .NE. 1) .AND. (NOUT .NE. 2)) CALL ERRMES(
     & -1,0,'RECELL entry neither 1 nor 2')
      IN=3-NOUT
      IF (MM .EQ. 2) GO TO 5
C
C IF WE HAVE A,B, OR A* B* ETC. WE MUST FIRST MAKE FROM THEM THE
C CELL SIDES AND ANGLES TO CORRESPOND WITH ENTRY M=2
      DO 6 I=1,3
      IF (CPARS(I,IN) .GT. 0.) GO TO 7
      CALL ERRIN2(I,-1,'Cell side','has negative square - set to 0')
      CELL(I,1,IN)=0.
      GO TO 6
   7  CELL(I,1,IN)=SQRT(CPARS(I,IN))
   6  CONTINUE
      J=2
      K=3
C
C FORM COSINES:
      DO 18 I=1,3
      CELL(I,2,IN)=CPARS(I+3,IN)/(CELL(J,1,IN)*CELL(K,1,IN))
      IF (ABS(CELL(I,2,IN)) .LE. 1.) GO TO 8
      CALL ERRIN2(I,-1,'Cell angle',
     & 'has cosine of modulus > 1 - set to 1')
      CELL(I,2,IN)=SIGN(1.0,CELL(I,2,IN))
   8  J=K
      K=I
  18  CONTINUE
C
C FORM SINES - JOIN HERE IF M=2 - THEN START ON OTHER SPACE:
   5  DO 2 I=1,3
      CALL SINCOS(CELL(I,2,IN),CELL(I,3,IN),'RECELL')
      ANGLE(I,IN)=DEGREE(ATAN2(CELL(I,3,IN),CELL(I,2,IN)))
   2  CONTINUE
      J=2
      K=3
      DO 3 I=1,3
      C=DBLE((CELL(J,2,IN))*CELL(K,2,IN)-CELL(I,2,IN))/(CELL(
     & J,3,IN)*CELL(K,3,IN))
      CELL(I,2,NOUT)=SNGL(C)
      CALL SINCOS(CELL(I,2,NOUT),CELL(I,3,NOUT),'RECELL')
      ANGLE(I,NOUT)=DEGREE(ATAN2(CELL(I,3,NOUT),CELL(I,2,NOUT)))
      J=K
      K=I
   3  CONTINUE
C UNIT CELL VOLUME:
      V(IN)=CELL(1,1,IN)*CELL(2,1,IN)*CELL(3,1,IN)*CELL(1,3,NOUT)*
     & CELL(2,3,IN)*CELL(3,3,IN)
      V(NOUT)=1/V(IN)
C
C CELL SIDES IN OTHER SPACE:
      DO 4 I=1,3
      CELL(I,1,NOUT)=CELL(J,1,IN)*CELL(K,1,IN)*CELL(I,3,IN)/V(IN)
      J=K
      K=I
   4  CONTINUE
C
      IF (MM .EQ. 1) GO TO 10
C IF M=1 SET UP QUADRATIC PRODUCTS IN OTHER SPACE -
C IF M=2 SET UP QUADRATIC PRODUCTS IN BOTH SPACES:
      DO 9 I=1,3
      CPARS(I,IN)=CELL(I,1,IN)*CELL(I,1,IN)
      CPARS(I+3,IN)=CELL(J,1,IN)*CELL(K,1,IN)*CELL(I,2,IN)
      J=K
      K=I
   9  CONTINUE
  10  DO 11 I=1,3
      CPARS(I,NOUT)=CELL(I,1,NOUT)*CELL(I,1,NOUT)
      CPARS(I+3,NOUT)=CELL(J,1,NOUT)*CELL(K,1,NOUT)*CELL(I,2,NOUT)
      J=K
      K=I
  11  CONTINUE
      WRITE (LPT,2001) (CELL(I,1,1),I=1,3),(ANGLE(I,1),I=1,3),
     & V(1),(CELL(I,1,2),I=1,3),(ANGLE(I,2),I=1,3),V(2)
2001  FORMAT (/' Real cell      ',3F10.4,3F8.2/' Volume = ',F10.4//
     & ' Reciprocal cell',3F10.4,3F8.2/' Volume = ',E12.4/)
      IF ((NOUT .EQ. 2) .OR. (MM .EQ. 1)) WRITE (LPT,2002)
     & (CPARS(I,1),I=1,6)
2002  FORMAT (/' Real cell quadratic products:'/
     & ' A (=a  sqrd)     B            C   D (=b c cos alpha)  E ',
     & '         F '/1X,6F12.5)
      IF ((NOUT .EQ. 2) .OR. (MM .EQ. 2)) WRITE (LPT,2000) (
     & CPARS(I,2),I=1,6)
2000  FORMAT (/' Reciprocal cell quadratic products:'/
     & ' A*(=a* sqrd)     B*          C*  D*(=b*c*cos alpha*)  E*',
     & '         F*'/1X,6F12.5)
      CALL ORTHG(M)
** SOMEWHERE HERE MAKE ESDS TO PRINT LATER
      RETURN
      END
C
C
C
C
C LEVEL 10      SUBROUTINE RECIP
      SUBROUTINE RECIP
C
C *** RECIP by JCM 13 Jul 83 ***
C
CX
CC 1A
CH Reads the lattice parameters and forms the reciprocal cell.
CD The real cell parameters are read from the "C" card in the order
CD a, b, c (in Angstroms) alpha, beta, gamma (in degrees).
CD The reciprocal cell parameters,cell volume and othogonal transformations
CD are calculated.
CD Cell edges and angles which are fixed by symmetry need not be given
CD on the "C" card.  Redundant parameters at the right hand end of a card
CD may be omitted.  Those not at the end may be omitted, but a comma
CD should be present to show that something was there.
CD     E.g for a cubic cell:       C   3.456
CD         for a tetragonal cell:  C  1.234  ,  2.345
CD         for a monoclinic cell:  C  1.234 2.345 3.456 , 88.43
CD Redundant values may of course be present in the ordinary way,
CD e.g. C 3.456 3.456 3.456  90  90  90
C
CD On exit, CELL() in COMMON /CELPAR/ contains a,b,c,cos(alpha,beta,gamma),
CD sin(same),a*,b*,c*,cos(alpha*,beta*,gamma*), sin(same)
CD RECELL has been called to set up symmetry relations and  orthogonal
CD matrices and fill in CPARS
C
CO Write to unit LPT any constraints which the symmetry places on the
CO real space cell parameters.
C
      CHARACTER *42 HEADNG
      CHARACTER *5 LABEL(6)
      DIMENSION CIN(6)
      LOGICAL HEAD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CELFIX/IPTCEL(6),AMCELL(6),NCELF,NCELG,NCELS,KOM3
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      DATA HEADNG/'Symmetry constraints on lattice parameters'/
      DATA LABEL/'  a','  b','  c','alpha',' beta','gamma'/
C
      HEAD=.FALSE.
      IF (INREAD(19) .GT. 0) CALL SYMOP
C
C READ C CARDS:
  23  NCELL=ICDNO(3)
      IF (NCELL .EQ. 0) CALL ERRMES(3,0,
     & 'starting "C" with cell parameters')
C
      ID=IABS(INREAD(3))
      DO 4 IC=1,NCELL
      CALL INPUTC(ID,CIN)
      ID=ID+NYZ
      IF (SDREAD) THEN
        DO 6 I=1,6
   6    CELLSD(I,I)=CIN(I)
      ELSE
        I=0
        DO 5 J=1,2
        DO 5 K=1,3
        I=I+1
   5    CELL(K,J,1)=CIN(I)
      ENDIF
   4  CONTINUE
      INREAD(3)=-IABS(INREAD(3))
C
C DEAL WITH a:
      IP=1
C COMPLAIN IF EITHER a FIXED OR a NOT GIVEN ON CARD
      IF (IPTCEL(1) .NE. 0) GO TO 30
  10  CALL ERRCH2(LABEL(IP),0,'in symmetry in RECIP -',
     & 'would be fixed')
C
  30  IF (CELL(1,1,1) .NE. 0.) GO TO 31
C
C ERROR IF ANY ESSENTIAL ITEM MISSING FROM C CARD:
  21  CALL ERRCH2(LABEL(IP),0,'in symmetry in RECIP - ','found zero')
C
C NOW b and c:
  31  DO 9 IP=2,3
      IF (IPTCEL(IP) .NE. 9999) GO TO 8
C JUMP IF RELATION FOUND INVOLVING EITHER b OR c
  11  IF (CELL(IP,1,1) .EQ. 0.) GO TO 21
C COMPLAIN ON b OR c NOT GIVEN ON CARD WHEN NEEDED
      GO TO 9
C CELL SIDES MAY NOT BE FIXED:
   8  IF (IPTCEL(IP) .EQ. 0) GO TO 10
C
C UNCHAIN TO FIND PREVIOUS PARAMETER USED IN RELATION
      IP1=IP
  12  IP1=IPTCEL(IP1)
      IF (IP1-IP) 13,11,12
C
C CELL SIDE RELATION MUST BE SIMPLE EQUALITY:
  13  A=SQRT((AMCELL(IP1)/AMCELL(IP)))
      IF (ABS(A-1.) .GT. 0.0001) CALL ERRMES(-1,0,
     & 'cell side relation found other than equality')
C
      IF (.NOT. HEAD) CALL MESS(LPT,1,HEADNG)
      HEAD=.TRUE.
      WRITE (LPT,2002) LABEL(IP),LABEL(IP1)
2002  FORMAT (45X,A5,' =',A5)
      CELL(IP,1,1)=CELL(IP1,1,1)
   9  CONTINUE
C
C NOW CROSS TERMS - MAY BE FREE, FIXED (90 OR 120), OR RELATED TO ANOTHER TERM.
C 120 IS DETECTED BY RELATION OF A CROSS TERM TO A CELL SIDE TERM.
      J=2
      K=3
      DO 14 IP=4,6
      IF (IPTCEL(IP) .EQ. 9999 .AND. CELL(IP-3,2,1) .EQ. 0.) GO TO 21
C TURN ANGLES INTO COSINES;  THIS WILL TURN 0 INTO 1 FOR THOSE OMITTED
      CELL(IP-3,2,1)=COS(RADIAN(CELL(IP-3,2,1)))
C JUMP IF ANGLE FREE:
      IF (IPTCEL(IP).EQ.9999) GO TO 22
C JUMP IF ANGLE RELATED :
      IF (IPTCEL(IP) .NE. 0) GO TO 16
C CROSS TERM FIXED IMPLIES ANGLE IS 90:
      CELL(IP-3,2,1)=0.
      IF (.NOT. HEAD) CALL MESS(LPT,1,HEADNG)
      HEAD=.TRUE.
      WRITE (LPT,2003) LABEL(IP)
2003  FORMAT (45X,A5,' = 90')
      GO TO 22
C CROSS TERM IS INVOLVED IN A RELATION - FIND PREVIOUS CHAIN MEMBER:
  16  IP1=IP
  17  IP1=IPTCEL(IP1)
      IF (IP1-IP) 19,22,17
C RELATED TO PREVIOUS CELL PARAMETER - JUMP IF THAT ALSO A CROSS TERM:
  19  IF (IP1 .GE. 4) GO TO 18
C IF RELATED TO A*, B* OR C*, IMPLIES ANGLE IS 120.  THIS MAY BE ALTERED
C TO USE REAL SPACE CELL PARAMETERS - AT PRESENT THE MINUS SIGN IS
C NECESSARY BECAUSE THE RELATIONS HAVE BEEN OBTAINED IN RECIPROCAL SPACE
C NOT REAL, AND THE IMPLIED ANGLE IS 60 NOT 120.
  20  CELL(IP-3,2,1)=-(AMCELL(IP1)/(AMCELL(IP))*CELL(IP1,1,1)*
     & CELL(IP1,1,1)/(CELL(J,1,1)*CELL(K,1,1)))
      IF (.NOT. HEAD) CALL MESS(LPT,1,HEADNG)
      HEAD = .TRUE.
      WRITE (LPT,2004) LABEL(IP)
2004  FORMAT (45X,A5,'= 120')
      GO TO 22
C
C CROSS TERM RELATED TO A PREVIOUS ONE -
C PREVIOUS MUST HAVE BEEN GIVEN ON C CARD:
  18  IF (ABS(CELL(IP1-3,2,1)-1.).LT. 0.0001) GO TO 21
      CELL(IP-3,2,1)=(AMCELL(IP1)/AMCELL(IP))*CELL(IP-3,1,1)*CELL(
     & IP1-3,2,1)/CELL(IP1-3,1,1)
      IF (.NOT. HEAD) CALL MESS(LPT,1,HEADNG)
      HEAD = .TRUE.
      WRITE (LPT,2005) LABEL(IP),LABEL(IP1)
2005  FORMAT (45X,A5,'=',A5)
C
  22  J=K
      K=IP-3
  14  CONTINUE
C
C SET UP REMAINING QUANTITIES CONNECTED WITH CELL PARAMETERS:
      CALL RECELL(2,2)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE REINDX(DPROP)
      SUBROUTINE REINDX(DPROP)
C
C *** REINDX updated by PJB 1 Feb 1994 ***
C
CH To reindex a set of reflections after a least squares cycle in which
CH the propagation vector changes
C
CA On entry DPROP(3) is the change in propagation vector
C
      DIMENSION DPROP(3)
C>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
C
      DO 1 KNOW=1,MAXK
      IF (ISMAG(KNOW).EQ.0) GO TO 1
      IF (ISMAG(KNOW) .GT.0) THEN
        CALL GMADD(REFH(1,KNOW),DPROP,REFH(1,KNOW),3,1)
      ELSE
        CALL GMSUB(REFH(1,KNOW),DPROP,REFH(1,KNOW),3,1)
      ENDIF
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE REJECT(N,NEXT,LREJ,USE)
      SUBROUTINE REJECT(N,NEXT,LREJ,USE)
C
C *** REJECT corrected by PJB 22-June-95 ***
C
CX
CC 11C
CH Decides for ARRNGE-type main programs whether the record number of
CH a reflection occurs in a list of those to be rejected.
CA On entry N is normally the number of the reflection to be tested.
CA            but if negative indicates the setting up entry which
CA            opens the rejection list and reads the first entry.
CA          NEXT should initially be set to zero;  it should then be
CA               preserved between entries.
CA          LREJ is the number of the unit from which to read rejection
CA               numbers.
C
CA On exit LOGICAL USE is TRUE if reflection number N is to be used.
C
CD Reads one integer at a time from unit LREJ.  Takes a -ve integer
CD to imply "all the numbers between the previous number and this one".
C
CN N must be monotonic increasing.
C
      LOGICAL USE
      COMMON /ARRDAT/IFOR(6),INC,LINO,NREF,INLBUF,NBUF,IBUF,INFBUF,
     & INDLEN,NUMVAL,IPOS,ISTORE,ICHNGE,NEW,LPRINT,ICD,IS,NREF1,NREFL
      LOGICAL INC,NEW,LPRINT
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      EQUIVALENCE (ICARD(1:1),MESSAG)
C
      IF (N.GE.0) GO TO 1
C
      MESSAG='File containing rejection list?'
      NAMFIL='.REJ'
      LREJ=NOPFIL(111)
   31 READ (LREJ,1000,END=30) ICARD
1000  FORMAT (A80)
      CALL RDINTG(NEXT,1,IPT,80,IER)
      IF (IER.NE.0) GO TO 31
      IF (NEXT .GE. 0) GO TO 100
      WRITE (LPT,2000) NEXT
2000  FORMAT (/' No rejections - ',I6,' read')
  30  NEXT=99999999
      GO TO 100
C
   1  IF (N .LT. NEXT) GO TO 101
C
      USE=.FALSE.
      IF (N .LE. -NEXT) GO TO 100
C
C NEED NEXT NUMBER FROM LIST
      LAST=NEXT
    3 READ (LREJ,1000,END=2) ICARD
      CALL RDINTG(NEXT,1,IPT,80,IER)
      IF (IER.NE.0) GO TO 31
C
C CHECK CORRECT SEQUENCE
      IF ((LAST .LT. 0 .AND. NEXT+LAST .GT. 0)
     &  .OR. IABS(NEXT) .GT. LAST) GO TO 100
      WRITE (LPT,3000) LAST,NEXT
3000  FORMAT (/' ERROR ** in sequence of rejection list',2I10)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C
C AT END, SET NO MORE REJECTIONS
   2  NEXT=99999999
      GO TO 100
 101  USE = .TRUE.
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE RELATE
      SUBROUTINE RELATE
C
C *** RELATE updated by JCM 11 Aug 88 ***
C
CX
CC 6C
CH In LSQ programs, converts a vector of derivatives wrt variables into the
CH vector of derivatives wrt basic variables.
CP /DERVAR/ must hold NVARV derivatives of some calculated function wrt
CP all variables, in DERIVV.
CP The constraint information must be set up in /CONSTR/ by a call of
CP VARMAK.
C
CD Applies the (strict) constraints to the vector DERIVV to convert it into
CD a vector DERIVB in /DERBAS/, of LVARB derivatives of the same calculated
CD function wrt basic variables.
C
      COMMON /CONSTR/JCONST,JROWPT(301),JCMAT(200),AMOUNT(200),
     & NEXTJ
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      IF (SIMUL) GO TO 100
      IF (LVARV .LE. 0) GO TO 100
      DO 1 I=1,LVARB
      DERIVB(I)=DERIVV(LBSVR(I))
   1  CONTINUE
C
C ADD DERIVATIVES FOR RELATED VARIABLES:
      DO 2 J=1,JCONST
      JROW=JROWPT(J)
      JNEXT=JROWPT(J+1)-1
      DO 3 K=JROW,JNEXT
      I=JCMAT(K)
      DERIVB(I)=DERIVB(I)+AMOUNT(K)*DERIVV(LRDVR(J))
   3  CONTINUE
   2  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE RELPAR(N1,A1,N2,A2,NFIX,FIX)
      SUBROUTINE RELPAR(N1,A1,N2,A2,NFIX,FIX)
C
C *** RELPAR by JCM 13 Jul 83 ***
C
CX
CC 6A
CH In the setting up of LSQ  applications, relates two parameters by
CH a simple linear relationship.
CA On entry N1 = the serial number of parameter 1 in the array NFIX
CA          N2 = the serial number of parameter 2 in the array NFIX
CA          A1 = a constant multiplier for parameter 1
CA          A2 = a constant multiplier for parameter 2
CA          the arrays NFIX and AFIX hold a (temporary) structure for
CA          chained parameters including the two given here.  They could
CA          refer, e.g., to 3 atom position coordinates, or to 6 cell
CA          quadratic products.
C
CD RELPAR absorbs into NFIX and FIX the relation:
CD     A1 x shift in parameter N1 = A2 x shift in parameter N2
CD dealing, if necessary, with any other similar relationships already
CD present, by setting up chains.
C
      DIMENSION NFIX(1),FIX(1)
C
      I1=N1
      I2=N2
      IOLD1=NFIX(I1)
      IOLD2=NFIX(I2)
C IOLD1 AND IOLD2 ARE WHAT IS ALREADY THERE.  THESE WILL BE ONE OF
C     0=FIXED
C      9999=FREE - NO REFERENCE HAS BEEN MADE TO THIS PARAMETER SO FAR
C     N, A FORWARD REFERENCE IN A CHAIN
      IF ((IOLD1 .NE. 9999) .OR. (IOLD2 .NE. 9999)) GO TO 1
C IF HERE, BOTH WERE FREE; SET UP 2-ELEMENT CHAIN AND EXIT
      NFIX(I1)=I2
      NFIX(I2)=I1
      FIX(I1)=A1
      FIX(I2)=A2
      GO TO 100
C
C IF HERE, ONE/BOTH PARS WERE FIXED/RELATED:
   1  IF (IOLD1 .NE. 0) GO TO 2
C
C P1 WAS ALREADY FIXED;  FIX (POSSIBLE CHAIN CONTAINING) P2
      CALL FIXPAR(I2,NFIX)
      GO TO 100
C
   2  IF (IOLD2 .NE. 0) GO TO 3
C SIMILARLY IF P2 FIXED, FIX (POSSIBLE CHAIN CONTAINING) P1
      CALL FIXPAR(I1,NFIX)
      GO TO 100
C
   3  IF (IOLD1 .NE. 9999) GO TO 4
C IF HERE, P2 IS CHAINED BUT P1 FREE; ADD P1 TO CHAIN
      NFIX(I1)=NFIX(I2)
      NFIX(I2)=I1
      FIX(I1)=A1*FIX(I2)/A2
      GO TO 100
C
   4  IF (IOLD2 .NE. 9999) GO TO 5
C SIMILARLY, P1 CHAINED BUT P2 FREE
      NFIX(I2)=NFIX(I1)
      NFIX(I1)=I2
      FIX(I2)=A2*FIX(I1)/A1
      GO TO 100
C
C IF HERE, BOTH P1 AND P2 ALREADY BELONGED TO CHAINS.  DISCOVER
C WHETHER SAME OR DIFFERENT CHAINS.
   5  L=IOLD2
   6  IF (L .EQ. I1) GO TO 8
C TO 8 IF SAME CHAIN
      IF (L .EQ. I2) GO TO 7
C TO 7 IF DIFFERENT CHAINS
      L=NFIX(L)
C UNCHAIN
      GO TO 6
C
C HERE ON 2 SEPARATE CHAINS TO BE MERGED.  FIRST SCALE CHAIN 1 BY
C CHAIN 2
   7  A=A1*FIX(I2)/(A2*FIX(I1))
      CALL SCLCHN(I1,A,NFIX,FIX)
C CROSS LINKS
      IT=NFIX(I1)
      NFIX(I1)=NFIX(I2)
      NFIX(I2)=IT
      GO TO 100
C
C HERE ON SAME CHAIN - SHOULD HAVE SAME SCALE RATIOS
   8  IF (ABS(A1*FIX(I2)-A2*FIX(I1)).GE.0.0001) CALL FIXPAR(I1,NFIX)
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE RELSM3(R,NFIX,FIX)
      SUBROUTINE RELSM3(R,NFIX,FIX)
C
C *** RELSM3 updated by JCM 13 Feb 90 ***
C
CX
CC 6A
CH Forms a complete set of relations imposed by symmetry on the given
CH 3 parameters of a LSQ application.
CA On entry R holds a generalised symmetry rotation operator.
CA On exit arrays NFIX and FIX hold the relationships found.
C
CD Adds any found relationships to the general collection which will
CD eventually be used by routine VARMAK.
C
      DIMENSION R(3,3),NFIX(3),FIX(3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      DO 1 K=1,3
C COUNTS 3 EQUATIONS
      IZ=0
C COUNTS NON-ZERO COEFFICIENTS FOUND IN EACH EQUATION
      DO 2 I=1,3
C COUNTS COEFFICIENTS WITHIN AN EQUATION
      A=R(K,I)
      IF (I .EQ. K) A=A-1.
      IF (ABS(A) .LT. 1.E-5) GO TO 2
      IZ=IZ+1
      IF (IZ .NE. 1) GO TO 3
      N1=I
      A1=A
C N1 AND A1 RECORD POSITION AND VALUE OF 1ST NON-ZERO
      GO TO 2
   3  IF (IZ .EQ. 2) GO TO 4
      WRITE (LPT,3000) A1,A2,A
      WRITE (ITO,3000) A1,A2,A
3000  FORMAT (/' ERROR ** in RELSM3 finding symmetry relations ',
     & 'between parameters',/' row of matrix ',
     &'minus unit matrix is',3F8.2)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
   4  N2=I
      A2=-A
C RECORD 2ND NON-ZERO IN N2,A2
   2  CONTINUE
      IF (IZ .EQ. 1) CALL FIXPAR(N1,NFIX)
      IF (IZ .EQ. 2) CALL RELPAR(N1,A1,N2,A2,NFIX,FIX)
   1  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE RELSM6(R,NFIX,FIX)
      SUBROUTINE RELSM6(R,NFIX,FIX)
C
C *** RELSM6 updated by JCM 13 Feb 90 ***
C
CX
CC 6A
CH Forms a complete set of relations imposed by symmetry on the given
CH 6 parameters of a LSQ application.
CA On entry R holds a generalised symmetry rotation operator.
CA On exit arrays NFIX and FIX hold the relationships found.
C
CD Adds any found relationships to the general collection which will
CD eventually be used by routine VARMAK.
C
      DIMENSION R(3,3),NFIX(6),FIX(6)
      DIMENSION LKUP(3,3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      DATA LKUP/1,6,5,6,2,4,5,4,3/
C
      DO 1 K=1,3
      DO 1 L=K,3
C COUNTS 6 EQUATIONS
      IZ=0
C COUNTS NON-ZERO COEFFICIENTS FOUND IN EACH EQUATION
      DO 2 I=1,3
      DO 2 J=I,3
C COUNTS COEFFICIENTS WITHIN AN EQUATION
      A=R(K,I)*R(L,J)
      IF (I .NE. J) A=A+R(L,I)*R(K,J)
      IF ((I .EQ. K) .AND. (J .EQ. L)) A=A-1.
      A=FLOAT(NINT(A))
      IF (A .EQ. 0.) GO TO 2
      IZ=IZ+1
      IF (IZ .NE. 1) GO TO 3
      N1=LKUP(I,J)
      A1=A
C N1 AND A1 RECORD POSITION AND VALUE OF 1ST NON-ZERO
      GO TO 2
   3  IF (IZ .EQ. 2) GO TO 4
      WRITE (LPT,3000) A1,A2,A
      WRITE (ITO,3000) A1,A2,A
3000  FORMAT (/' ERROR ** in RELSM6 finding symmetry relations ',
     & 'between parameters',/' row of matrix ',
     & 'minus unit matrix is',3F8.2)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
   4  N2=LKUP(I,J)
      A2=-A
C RECORD 2ND NON-ZERO IN N2,A2
   2  CONTINUE
C
      IF (IZ .EQ. 1) CALL FIXPAR(N1,NFIX)
      IF (IZ .EQ. 2) CALL RELPAR(N1,A1,N2,A2,NFIX,FIX)
   1  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE RESHUF(A,IPT,N)
      SUBROUTINE RESHUF(A,IPT,N)
C
C *** RESHUF by JCM 22 Aug 86 ***
C
CX
CC 16C
CH Reorders a real array, given a parallel pointer array out of SORTX.
C
CA On entry A is a real array, of dimension at least N, of numbers
CA            whose pointers have been sorted using SORTX.
CA          IPT is an integer array, of dimension at least N, of pointers
CA            within A, probably just produced by sorting with SORTX.
CA          N is the number of entries in each of A and IPT.
CA On exit  A contains the same numbers reordered according to the
CA            pointers in IPT.
C
      DIMENSION A(N),IPT(N)
      COMMON /SCRAT/TEMP(3000)
C
      DO 1 I=1,N
   1  TEMP(I)=A(IPT(I))
C
      DO 2 I=1,N
   2  A(I)=TEMP(I)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE RFACS(IN)
      SUBROUTINE RFACS(IN)
C
C *** RFACS updated by JCM 14 Mar 89 ***
C
CX
CC 6B
CH A multiple entry routine to deal with all aspects of R Factor
CH calculations and statistics for single crystal observations, and
Ch for slack constraints.
CA On entry, IN indicates the calculation required:
CA   IN=1  Set up all quantities, clearing to zero
CA   IN=2  Add in contributions for conventional Obs R Factors
CA   IN=3  Print out for conventional Obs R Factors
CA   IN=4  Add in contributions for slack constraint type ISLKTP in /SLAKDA/
CA   IN=5  Print out for slack constraint, type ISLKTP
CA   IN=6  Print out for both obs and slack constraints
C
CD If IWGHT=1 we expect that the weights are unity: we try to avoid
CD unnecessary R factors in this case.
CD The R Factors are:
CD       R1=simple R factor, 100*sum[ABS(diffs)]/sum[obs]
CD       R2=squared R factor, 100*sum[sqrd diffs]/sum[sqrd obs]
CD       R3=simple weighted R factor,
CD                      1OO*sum[ABS(weighted(diffs))]/sum[weighted(obs)]
CD       R4=Squared weighted R factor
C
CO For entries 3,5 and 6 prints R factors on unit LPT.
C
      LOGICAL TESTOV
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /RSTATS/RNUM,RDEN,RSNUM,RSDEN,RWNUM,RWDEN,RWSDEN,CHI2
      COMMON /SLAKDA/NSLAK(4),SLKSWD(4),SLAKWT(4),
     & CHISQD(4),ISLKTP,NSKTOT,KOM24
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
      GO TO (1,2,3,4,5,6) , IN
      CALL ERRIN2(IN,0,'in RFACS call - type','not written')
C
C INITIAL ENTRY
   1  RNUM=0.0
      RDEN=0.0
      RSNUM=0.0
      RSDEN=0.0
      RWNUM=0.0
      RWDEN=0.0
      SUMWD=0.0
      RWSDEN=0.0
C TYPES OF SLACK CONSTRAINT:
C%
C      CALL GMZER(SLKSWD,1,%SKTP%)
      CALL GMZER(SLKSWD,1,4)
      GO TO 100
C
C ADDING IN ENTRY
   2  D=DIFF*DIFF
      G=OBS*OBS
      RNUM=RNUM+ABS(DIFF)
      RDEN=RDEN+ABS(OBS)
      RSNUM=RSNUM+D
      RSDEN=RSDEN+G
      RWNUM=RWNUM+ABS(WDIFF)
      RWDEN=RWDEN+SQRTWT*ABS(OBS)
      SUMWD=SUMWD+WT*D
      RWSDEN=RWSDEN+WT*G
      GO TO 100
C
C PRINT
   3  IF (TESTOV(RNUM,RDEN)) THEN
        CALL MESS(LPT,1,'R Factors not available as denominators zero')
        GO TO 100
      ENDIF
C
      R1=100.0*RNUM/RDEN
      R2=100.0*RSNUM/RSDEN
      WRITE (LPT,2001) R1,R2
2001  FORMAT (/' R1=Sum diffs/Sum obs =',G12.4/
     & ' R2=Sum squares diffs/Sum squares obs = ',G12.4)
      CHISQ=SUMWD/FLOAT(NOBS-LVARB)
      WRITE (LPT,2003) CHISQ,SUMWD
      WRITE (ITO,2003) CHISQ,SUMWD
2003  FORMAT (' Chi squared =',G12.3,' Sum weighted diffs sqrd =',
     & G12.3)
      WRITE (LPT,2033) NOBS,LVARB
2033  FORMAT (' For',I6,' observations and',I4,' basic variables')
      IF (IWGHT .EQ. 1) GO TO 100
      R3=100.0*RWNUM/RWDEN
      R4=100.0*SUMWD/RWSDEN
      WRITE (LPT,2002) R3,R4
2002  FORMAT (' R3=Sum weighted diffs/Sum weighted obs = ',G12.4/
     &' R4=Sum squares weighted diffs/Sum squares weighted obs =',G12.4)
      GO TO 100
C
C SPECIAL ENTRY FROM SLACK CONSTRAINT CALCULATION - ADD IN:
   4  SLKSWD(ISLKTP)=SLKSWD(ISLKTP)+WT*DIFF*DIFF
      GO TO 100
C
C PRINTING FOR SLACK CONSTRAINTS:
   5  WRITE (LPT,2010) ISLKTP
      WRITE (ITO,2010) ISLKTP
2010  FORMAT (/' For slack constraints type',I4)
** NEED DATA STATEMENT WITH DIFFERENT TYPES
      CSQTOT=SLKSWD(ISLKTP)/FLOAT(NSLAK(ISLKTP))
      WRITE (LPT,2011) CSQTOT,NSLAK(ISLKTP)
      WRITE (ITO,2011) CSQTOT,NSLAK(ISLKTP)
2011  FORMAT (/' Chi squared=',G12.3,' for',I4,' constraints')
      GO TO 100
C
C PRINTING FOR SLACK CONSTRAINTS AND CONVENTIONAL OBS TOGETHER:
   6  IF (NSKTOT .EQ. 0 .OR. SLONLY) GO TO 100
      CNUM=SUMWD
      CDEN=FLOAT(NOBS-LVARB)
C%
C      DO 36 ISK=1,%SKTP%
      DO 36 ISK=1,4
      IF (NSLAK(ISK) .EQ. 0) GO TO 36
C WE ONLY WANT THIS FIGURE IF WE HAVE BOTH CONVENTIONAL OBS AND SLACK
C CONSTRAINTS:
      CNUM=CNUM+SLAKWT(ISK)*SLKSWD(ISK)
      CDEN=CDEN+SLAKWT(ISK)*FLOAT(NSLAK(ISK))
  36  CONTINUE
      CSQTOT=CNUM/CDEN
      WRITE (LPT,2005) CSQTOT
      WRITE (ITO,2005) CSQTOT
2005  FORMAT (' Chi squared for conventional obs',
     &   ' and all slack constraints = ',G12.3)
 100  RETURN
      END
C
C
C
C
! JvdS The following routine was never called and used an uninitialised variable
! causing a compiler warning. Removing all exclamation marks restores the original routine.
!C LEVEL 2      FUNCTION RGAUSS(SIGMA)
!      FUNCTION RGAUSS(SIGMA)
!C
!C *** RGAUSS by PJB 27-Sept-93 ***
!C
!CX
!CC 9C
!CH Returns a value random number with a gaussian distribution.
!CA On entry SIGMA gives the width if the dsitribution such that
!CA          p(x)=exp-(x/SIGMA)**2
!CA          if SIGMA=0 sets the seed for random number generation
!CN  Uses the subprogram NB01A from the Harwell library.
!C
!      COMMON /LENINT/NBITS
!      COMMON /SEEDIT/LSEED
!C
!      IF (SIGMA.EQ.0) THEN
!c        SEED=SECNDS(0.)
!        ISEED=2**(NBITS-1) -INT(SEED)
!        ISEED=2*ISEED-1
!        GO TO 100
!      ENDIF
!c      AR=RAN(ISEED)
!      ar=.756
!      R=ABS(2*AR-1.)
!      K=0
!      NUM=0
!      A=0.
!      XLIM=100.
!      B=XLIM
!      ERR=.0001
!      MAXIT=100
!    5 CALL NB01A(K,A,B,ERR,X,Y,MAXIT)
!      GO TO (1,2,3,4) K
!C
!    1 IF (X.GT.XLIM) THEN
!        Y=1.-R
!      ELSE
!        Y=1.-ERFNC(X)-R
!      ENDIF
!      GO TO 5
!C
!    2 RGAUSS=X
!      IF (AR.LT.0.5) RGAUSS=-X
!      RGAUSS=RGAUSS*SIGMA
!      GO TO 100
!C
!    3 CALL ERRMES(1,0,'Too many iterations in RGAUSS')
!      GO TO 100
!    4 CALL ERRMES(1,0,'from RGAUSS')
!      GO TO 100
!C
!C
!  100 RETURN
!      END
C
C
C
C
C LEVEL 2      SUBROUTINE ROTOSM(H,RH,IOP,ISS)
      SUBROUTINE ROTOSM(H,RH,IOP,ISS)
C
C *** ROTOSM corrected by PJB 31-May-1994 ***
C
CX
CC 1B
CH Calculates the effect of the rotation matrix of a symmetry operator,
CH on a vector given on orthogonal axes.
CA On entry  H(1:3)is a real vector in standard orthogonal coordinates
CA           IOP  is the number of a symmetry operator, negative for one
CA                related by a centre of symmetry to the stored one
CA           ISS  defines the mode of operation:
CA ISS = 0  Set-up by calculating the whole set of symmetry operations in
CA          orthogonal co-ordinates
CA ISS > 0  Calculate the effect of the symmetry operator IOP.
CA ISS < 0  Calculate the effect of the inverse of the symmetry operator IOP.
CA On exit   RH(1:3) is the vector obtained by operating with the
CA                rotation matrix of the element IOP on H.
CP SYMOP and RECIP must have set up the symmetry and cell parameters.
CP ROTOSM must be called with ISS=0 before any other use is made of it
CD The call with ISS=0 sets the orthogonal symmetry elements into
CD COMMON /ORTSYM/
C
CN ENTRY CROTO(CH,CRH,IOP,ISS) does the rotation for the complex
CN vectors CH and CRH.
CN
CN Note that in orthogonal coordinates there is no distinction between
CN real and reciprocal space
C
      DIMENSION H(3),RH(3),TEMP(3,3)
      COMPLEX CH(3),CRH(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /ORTSYM/SYMORT(3,3,24),NFLAG
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      I=IABS(IOP)
      IF (ISS.NE.0 .AND. NFLAG .EQ. 42) GO TO 1
C
C PUT SYMMETRY ROTATIONS ONTO ORTHOGONAL AXES
C USE SYMORT(,,1) TEMPORARILY FOR TRANSPOSE:
      CALL GMEQ(ORTH(1,1,1),SYMORT(1,1,1),3,3)
      CALL TRANSQ(SYMORT(1,1,1),3)
      DO 27 NO=2,NOPC
      CALL GMPRD(SYM(1,1,NO),ORTH(1,1,2),TEMP,3,3,3)
      CALL GMPRD(SYMORT(1,1,1),TEMP,SYMORT(1,1,NO),3,3,3)
   27 CONTINUE
      CALL GMUNI(SYMORT(1,1,1),3)
      NFLAG=42
      IF (ISS .EQ. 0) GO TO 100
C
   1  IF (ISS.LT.0) I=INVERS(I)
      CALL GMPRD(SYMORT(1,1,I),H,RH,3,3,1)
      IF (IOP.LT.0) CALL GMREV(RH,RH,3,1)
      GO TO 100
C
C  ENTRY FOR COMPLEX VECTORS
      ENTRY CROTO(CH,CRH,IOQ,IST)
      I=IABS(IOQ)
      IF (IST.LT.0) I=INVERS(I)
      CALL RCMPRD(SYMORT(1,1,I),CH,CRH,3,3,1)
      IF (IOQ.LT.0) CALL GMREV(CRH,CRH,2,3)
      GO TO 100
C
  100 RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE ROTSYM(H,RH,IOP,ISS)
      SUBROUTINE ROTSYM(H,RH,IOP,ISS)
C
C *** ROTSYM by JCM 11 Apr 83 ***
C
CX
CC 1B
CH Rotates the vector H into RH by the given symmetry operator.
CA On entry  H(1:3) is a 1x3 vector (in either space)
CA          IOP is the serial number of a symmetry rotation matrix
CA          ISS is positive for pre-multiplication by rotation matrix,
CA              negative for post-multiplication
CA              the absolute value of ISS is 1 for real and 2 for
CA              reciprocal space
CA On exit  RH(1:3) is the 1x3 vector resulting from multiplying H by
CA              the rotation matrix as specified by ISS
CP SYMOP to set the symmetry matrices
C
      DIMENSION H(3),RH(3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      I=IOP
      IS=ISS
C
C IF RECIPROCAL SPACE REQUESTED, MATRIX USED MUST BE INVERSE TRANSPOSED OF
C THAT REFERRING TO REAL SPACE.  THE INVERSE IS OBTAINED BY INDIRECT ADDRESSING
C VIA ARRAY INVERS, AND THE TRANSPOSE BY REVERSING THE SEQUENCE OF THE
C MULTIPLICATION:
      IF (IABS(IS).EQ.2) I=INVERS(I)
      IF (IS .LT. 0) IS=IS+5
      GO TO (1,2,1,2) , IS
C
C PRE-MULTIPLICATION
   1  CALL GMPRD(SYM(1,1,I),H,RH,3,3,1)
      GO TO 100
C
C POST-MULTPLICATION
   2  CALL GMPRD(H,SYM(1,1,I),RH,1,3,3)
 100  RETURN
      END
C
C
C
C
C LEVEL 2      LOGICAL FUNCTION SAID(INCHAR,WANT)
      LOGICAL FUNCTION SAID(INCHAR,WANT)
C
C *** SAID by JCM 27 Sep 87 ***
C
CX
CC 11C
CH Decides whether the character variables INCHAR and WANT are the same,
CH ignoring any distinction between upper and lower case.
CA On entry INCHAR is an A1 character to test
CA          WANT is the other A1 char to test
CD Sets SAID .TRUE. if INCHAR and WANT are equal, ignoring the case
CD if they are letters.
C
      CHARACTER *1 INCHAR,WANT
C
      SAID = .TRUE.
      IF (INCHAR .EQ. WANT) GO TO 100
      L=LETTER(INCHAR)
      M=LETTER(WANT)
      IF (L .EQ. M .AND. L .NE. 0) GO TO 100
      SAID = .FALSE.
 100  RETURN
      END
C
C
C
C
C LEVEL 2      LOGICAL FUNCTION SAYS(WANT)
      LOGICAL FUNCTION SAYS(WANT)
C
C *** SAYS by PJB 1-Oct-93 ***
C
CX
CC 11C
CH Decides whether the string WANT matches a string of the same length,just
CH read into /SCRACH/ ignoring any distinction between upper and lower case.
CA On entry WANT is the string to test
CD Sets SAYS .TRUE. if WANT matches the string at the start of ICARD,
CD ignoring the case of letters.
C
      CHARACTER *(*) WANT
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      SAYS =.FALSE.
      L=LEN(WANT)
      DO 1 I=1,L
      N1=LETTER(WANT(I:I))
      IF (N1.EQ.0) THEN
C NOT A LETTER NEED AN EXACT MATCH
        IF (ICARD(I:I).EQ.WANT(I:I)) GO TO 1
        GO TO 100
      ELSE
C FOR LETTERS EITHER UPPER OR LOWER CASE WILL DO
        N2=LETTER(ICARD(I:I))
        IF (N1.EQ.N2) GO TO 1
        GO TO 100
      ENDIF
    1 CONTINUE
C ALL CHARACTERS MATCH
      SAYS=.TRUE.
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SCLCHN(NP,A,NFIX,FIX)
      SUBROUTINE SCLCHN(NP,A,NFIX,FIX)
C
C *** SCLCHN by JCM 13 Jul 83 ***
C
CX
CC 6A
CH A specialist routine used during the setting up of constraints on LSQ
CH variables.  Scales a chain of connected variables by the given constant.
CA On entry NP points to the first element of the chain in the arrays
CA             NFIX, FIX.
CA          A is the real constant by which the chain is to be scaled,
CA          NFIX is an integer array holding pointers within itself to
CA             describe the existing chain,
CA          FIX is a real array of multipliers of the corresponding
CA             elements of NFIX.
CD Scales all the elements of FIX which correspond to NFIX(NP) and those
CD elements which are chained to it.
C
      DIMENSION NFIX(1),FIX(1)
C
      J1=NP
   1  FIX(J1)=A*FIX(J1)
      J1=IABS(NFIX(J1))
      IF (J1 .NE. NP) GO TO 1
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION SCLPRD(H1,H2,IR)
      FUNCTION SCLPRD(H1,H2,IR)
C
C *** SCLPRD by JCM 26 Apr 84 ***
C
CX
CC 1B
CH Forms the scalar product of two vectors referred to crystal axes.
CA On entry H1 and H2 are two 1x3 vectors referred to the crystal axes.
CA             They are in real space if IR=1, reciprocal if IR=2.
CD The function is set to be the scalar product of H1 and H2
CP RECIP must have been obeyed to set up the cell parameters.
C
      DOUBLE PRECISION SCL
      DIMENSION H1(3),H2(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
C
      SCL=0.
      J=2
      K=3
      DO 1 I=1,3
      SCL=SCL+H1(I)*H2(I)*CPARS(I,IR) + (H1(J)*H2(K)+H1(K)*H2(J)
     & )*CPARS(I+3,IR)
      J=K
      K=I
   1  CONTINUE
      SCLPRD=SNGL(SCL)
      RETURN
      END
C
C

C
C
C LEVEL 12      SUBROUTINE SETABS
      SUBROUTINE SETABS
C
C *** SETABS by JCM 4 Oct 85 ***
C
CX
CC 2A
CH Sets up data for the calculation of absorption (or related) integrals.
CD Calls SETDC and SETG to set up the COMMON blocks /DGEOM/ and /GAUSS/
CD Checks that all necessary quantities have been given and are acceptable.
CD Checks that at least 3 plane faces have been given on G FACE cards.
CD Checks that D L/R card has been given.
CD If no G MODE card has been given, assumes mode 1, simple absorption
CD correction.
CD If no G PNTS card has been given assumes 5 x 5 x 5
CD  If no G MU card has been given assumes that mu will be set by a
CD main program
CP RECIP must have read in the cell parameters.
CI Causes D and "G" cards to be read from the copy of the Crystal Data
CI File on unit IO10.
CO Writes its findings to unit LPT.
C
      COMMON /ABSDAT/AMU,MODEA
      COMMON /CPLANE/AA(15),BB(15),CC(15),DD(15),NP
      COMMON /DREAD/IDREAD(8)
      COMMON /GAUSS/XX(1000),YY(1000),ZZ(1000),WW(1000),NL,NM,NN,NQ
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /GREAD/IGREAD(4)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      CALL MESS(LPT,2,
     & 'Absorption Corrections using Gaussian integration')
      IF (INREAD(4) .GT. 0) CALL SETDC
      IF (INREAD(7) .GT. 0 ) CALL INPUTG
C
C NOW CHECK SPECIFIC ITEMS NEEDED FOR ABSORPTION TYPE INTEGRALS:
      IF (NP .LE. 2) THEN
        CALL ERRMES(2,1,'equations of plane faces')
        GO TO 100
      ENDIF
C
      IF (IGREAD(2) .LE. 0) THEN
        NL=5
        NM=5
        NN=5
        CALL MESS(LPT,1,'No G PNTS card read - assuming 5 x 5 x 5')
      ENDIF
C
      IF (IGREAD(3) .LE. 0) CALL MESS(LPT,1,'No G MU card read - '//
     & 'assuming AMU put in place by MAIN program')
C
      IF (IGREAD(4) .LE. 0) THEN
        MODEA=1
        CALL MESS(LPT,1,'No G MODE card read - assuming 1')
      ENDIF
C
      IF (IDREAD(3) .LE. 0) THEN
        CALL ERRMES(2,1,'D L/R card for abs cor')
        GO TO 100
      ENDIF
C
      CALL SETGAU
 100  RETURN
      END
C
C
C
C
C LEVEL 11      SUBROUTINE SETANI
      SUBROUTINE SETANI
C
C *** SETANI by JCM 23 Sep 83 ***
C
CX
CC 4A
CH Reads "T" cards to set up for calculation of anisotropic temperature factors.
CD Reads cards starting "T"  giving the atom name, type of atf required
CD and 6 coefficients in order a11, a22, a33, a23, a13, a12.
CD SETANI converts the 6 coefficients to type 5 for internal use, stores
CD them in ATF in COMMON /ANISO/ and sets up vector IATYP which says which
CD type they started as.
C
CP RECIP and ATOPOS must have read the cell parameters and atomic positions.
CI Causes "T" cards to be read from the copy of the Crystal Data
CI File on unit IO10.
CO Writes its findings to unit LPT.
CN Types of atf at present allowed are:
CN  0: Convert given itf to an atf, then proceed as if type 2.
CN  2: exp-{1/4(a11 ha* ha* + . . +a12 ha* kb* + . . )}
CN  3: exp-{2*pi*pi(a11 ha* ha* + . .+2a12 ha* kb*  + . .)}
CN  4: exp-(a11 h h + . . +a12 h k + . .)
CN  5: exp-(a11 h h + . . +2a12 h k + . . )
C
CN Numbers 3 and 4 are out of Stout and Jensen pp449-450,
CN number 2 is used in Hewat profile refinement
CN and 5 is the form used internally in this program.
C
      CHARACTER *4 LABB
      DIMENSION ACOEFF(6)
      COMMON /ANISO/ATF(6,50),KATF(6,50),IAPT(150),
     & IATYP(50),KOM1
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
C
C IT IS NECESSARY THAT ATOPOS HAS SET UP THE ATOM LABELS:
      IF (INREAD(1) .GE. 0) CALL ATOPOS
C
C SET NO ATF ON ANY ATOM:
      CALL JGMZER(IAPT,1,NATOM)
C
C SET "T CARDS READ"
      INREAD(20)=-IABS(INREAD(20))
C
C READ NUMBER OF ANISOTROPIC TEMPERATURE FACTORS (NO. OF "T" CARDS):
      NCARD=ICDNO(20)
      IF (NCARD .EQ. 0) THEN
        CALL MESS(LPT,1,'No anisotropic temperature factors given')
        GO TO 100
      ENDIF
C
      IER=IERR
C%
C      CALL ERRCHK(1,NCARD,%ATFS%,1,'anisotropic temperature factors')
      CALL ERRCHK(1,NCARD,50,1,'anisotropic temperature factors')
      IF (IERR .GT. IER)GO TO 100
C
      CALL MESS(LPT,1,'Anisotropic temperature factors:')
      CALL MESS(LPT,0,'atom  type    11        22        33        '//
     & '23        13        12')
C
C READ T CARDS ONE BY ONE:
      ID=IABS(INREAD(20))
      DO 5 IP=1,NCARD
      CALL INPUTT(ID,LABB,LBBLEN,IATYP(IP),ATF(1,IP),IER)
      IF (IER .NE. 0) IERR=IERR+1
      ID=ID+NYZ
      J=IATOM(LABB)
      IF (J .LE. 0) THEN
        CALL ERRATM(LABB,2,'on T card')
        GO TO 100
      ENDIF
C
C NAME FOR ATF MATCHED WITH NAME IN ATOM LIST:
      IAPT(J)=IP
      IT=IATYP(IP)
      IF (IT .NE. 0) GO TO 11
C
C CONVERT ITF ALREADY READ TO BE AN ATF, AND SET ITF TO ZERO:
      DO 13 I=1,3
      ATF(I,IP)=TF(J)
      ATF(I+3,IP)=TF(J)*CELL(I,2,2)
  13  CONTINUE
      TF(J)=0.
      GO TO 12
C
C CHECK IF TYPE 1, AND FORCE THESE TO BE 2:
  11  IF (IT .NE. 1) GO TO 3
      CALL MESS(LPT,1,' *** WARNING ** ATF type 1 redefined to '//
     & 'exclude cosines - type 2 assumed')
  12  IATYP(IP)=2
      IT=2
C
C JOIN HERE ON ALL OTHER TYPES:
   3  WRITE (LPT,2002) LABB ,IATYP(IP),(ATF(I,IP), I=1,6)
2002  FORMAT(' ',A4,I5,6F10.4)
      GO TO (22,22,23,24,5) , IT
  22  FAC=0.25
      GO TO 30
C
  23  FAC=TWOPI*PI
  30  ACOEFF(1)=FAC*CPARS(1,2)
      ACOEFF(2)=FAC*CPARS(2,2)
      ACOEFF(3)=FAC*CPARS(3,2)
      ACOEFF(4) = FAC*CELL(3,1,2)*CELL(2,1,2)
      ACOEFF(5) = FAC*CELL(1,1,2)*CELL(3,1,2)
      ACOEFF(6) = FAC*CELL(2,1,2)*CELL(1,1,2)
      GO TO 25
C
  24  DO 9 I=1,3
      ACOEFF(3+I)=0.5
   9  ACOEFF(I)=1.
      GO TO 25
C
C CONVERT ATF() FOR INTERNAL USE;  CONATF WILL CONVERT BACK
  25  DO 6 I=1,6
   6  ATF(I,IP)=ATF(I,IP)*ACOEFF(I)
   5  CONTINUE
C
 100  RETURN
      END
C
C
C
C
C LEVEL 11      SUBROUTINE SETDC
      SUBROUTINE SETDC
C
C *** SETDC by JCM 8 Oct 85 ***
C
CX
CC 2A
CH Reads D cards to set up crystal orientation and diffraction geometry.
CP RECIP must have read in the cell parameters.
CD Sets the normalised orientation matrix into UM, the wavelength into
CD WLGTH, and the type of geometry into IGEOM all in COMMON /DGEOM/.
CD Also sets other useful quantities in /DGEOM/ for subsequent calculation
CD of the direction cosines of the incident and diffracted rays.
CD If IGEOM indicates 4-circle bisecting geometry, checks the determinant
CD of the UB matrix.  If it is a little different from 1., adjusts the
CD matrix elements until it is < 0.0001 away from 1.
C
      DIMENSION H(9)
      COMMON /DGEOM/IGEOM,UM(9),NLR,ANGLIN(3),ALAMBD(5,5),
     & NLAMB,ILAMB
      EQUIVALENCE (WLGTH,ALAMBD(1,1))
      COMMON /DREAD/IDREAD(8)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
       IF (INREAD(3) .GT. 0) CALL RECIP
       IF (INREAD(4) .GT. 0) CALL INPUTD
C
C CHECK D CARDS AND PREPARE FOR USE - AT THIS STAGE USE COULD BE ONE OF SEVERAL
C APPLICATIONS LIKE LP CORRECTIONS, ABSORTION INTEGRALS ETC:
C
      CALL ORTHO(UM(1),H(7),1)
      CALL UNIVEC(H(7),D)
      IF (IDREAD(1) .LE. 0) THEN
      CALL ERRMES(2,1,'D GEOM card for LP')
      GO TO 100
      ENDIF
C
      GO TO (1,2,1,40,1,6,6,6,40,40,6) , IGEOM
C
   40 CALL ERRMES(1,0,
     & 'SETDC only for normal beam, equi-inclination, 4 circle and D3')
C
C NORMAL BEAM EQUATORIAL:
   2  IF (IDREAD(7) .LE. 0) GO TO 16
      IF (IDREAD(8) .LE. 0) THEN
      CALL ERRMES(2,1,'D CHIA card for type 2 geometry')
      GO TO 100
      ENDIF
C
      IF (ABS(SCALPR(UM(1),UM(4))) .LT. 1.E-5) GO TO 15
      WRITE (LPT,3006) (UM(I),I=1,3),(UM(I),I=4,6)
      WRITE (ITO,3006) (UM(I),I=1,3),(UM(I),I=4,6)
3006  FORMAT (/' ERROR ** in SETDC - scalar product of',3F5.0,
     & ' and',3F5.0,' non-zero')
      IERR=IERR+1
      GO TO 100
C
  15  CALL ORTHO(UM(4),H(1),2)
      CALL UNIVEC (H(1),D)
      CALL VECPRD(H(7),H(1),H(4))
      UM(4) = COS(ANGLIN(1))
      UM(5) = SIN(ANGLIN(1))
      UM(6) = 0.
      CALL GMPRD(H(1),UM(4),UM(1),3,3,1)
      GO TO 100
C
C     NORMAL BEAM AND EQUI-INCLINATION GEOMETRIES:
   1  IF (IDREAD(7) .GT. 0) GO TO 18
  16  CALL ERRIN2(IGEOM,1,'Type','geometry needs D ROTA card')
      GO TO 100
C
  18  DO 3 I = 1,3
   3  UM(I) = H(6+I)
      GO TO 100
C
C 4 CIRCLE BISECTING & D3 GEOMETRIES:
   6  CALL GMEQ(UM,H,9,1)
      CALL TRANSQ(H,3)
      DO 4 I=1,7,3
   4  CALL ORTHO(H(I),UM(I),1)
      CALL TRANSQ(UM,3)
C
      CALL TRINV3(UM,D)
      WRITE (LPT,2000) D
2000  FORMAT (/' Determinant of UB matrix is ',F12.6)
      IF (ABS(D-1.) .GE. 10.E-2) CALL ERRMES(1,0,
     & 'cell dimensions and UB matrix not compatible')
   11 IF (ABS(D-1.) .LT. 1.E-4) GO TO 12
      CALL GMEQ(UM,H,1,9)
      CALL TRINV3(UM,D)
      DO 14 I = 1,9
   14 UM(I) = 0.5*(H(I)+UM(I))
      GO TO 11
C
C UB MATRIX NOW OK:
   12 WRITE (LPT,2002) UM
2002  FORMAT (/' Matrix between diffractometer axes and ',
     & 'orthogonal crystallographic axes is:'/3(12X,3F10.5/))
 100  RETURN
C
      END
C
C
C
C
C LEVEL 12      SUBROUTINE SETFC
      SUBROUTINE SETFC
C
C *** SETFC updated by JCM 17 Feb 88 ***
C
CX
CC 1A
CH Calls all the setting up routines needed for nuclear structure
CH factor calculations.
CD Calls INPUTN, SYMOP, OPSYM(1), RECIP, ATOPOS, SETFOR and SETANI to set data
CD in the COMMON blocks /SYMDA/, /SYMTAB/, /NSYM/, /CELLDA/, /ATNAM/, /POSNS/,
CD /FORNAM/, /FORDA/, possibly /ANSCAT/ and /ANISO/.
C
CI Causes all the crystallographic and structure cards to be read from
CI the copy of the Crystal Data File on unit IO10.
CO If any of the constituent routines sets the error flag IERR in /CARDRC,
CO prints an error message and stops.
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      CALL INPUTN(LPT)
      CALL SYMOP
      CALL OPSYM(1)
      CALL RECIP
      CALL ATOPOS
      CALL SETFOR
      CALL SETANI
      IF (IERR .NE. 0) CALL ERRMES(1,0,'s in SETFC')
      RETURN
      END
C
C
C
C
C LEVEL 11      SUBROUTINE SETFOR
      SUBROUTINE SETFOR
C
C *** SETFOR updated by JCM 24 Nov 91 ***
C
CX
CC 4A
CH Sets up data for scattering or form factor calculations.
CD Reads "F" cards which all start  "F <name> ITYP"
CD Where <name> is a character string of maximum length 4 introduced
CD and terminated by spaces. <name> is the label of the scattering
CD factor and will often be a chemical symbol. The factor applies to
CD all atoms whose name starts with the scattering factor name. In the
CD atom name the character following <name> must be a non-letter.
CD For example - a set of A cards:
CD A Pb1
CD A FreD
CD A Pb2
CD A P4
CD A O14A
CD A O14B
CD would need 4 scattering factors, labelled:
CD F Pb      (applies to Pb1 and Pb2)
CD F P       (applies to P4)
CD F O       (applies to O14A and O14B)
CD F FreD    (applies to FreD)
CD
CD ITYP is an integer indicating the way in which the form-factor will be
CD           given
CD ITYP = 0  means "this is a multiplicative factor".  It is useful
CD           when the user wants to scale his structure factors up or down,
CD           without altering his other "F" cards.
CD For ITYP positive, at present 4 values are defined:
CD ITYP = 1  Neutron nuclear scattering.  In this case the scattering
CD           length is given on the same card after ITYP
CD ITYP = 2  Form factors given by series expansion of exponential terms,
CD           of the form:    sum[a(n)*exp{-a(n+1)*s*s}] + c
CD                           with  n=1,2,3 or 4  and  s=sin(theta)/lambda
CD           the coefficients a(n) and c are given on one, or possibly two
CD           "F" cards.  The cards each start "F <name> ITYP" and the 5, 7
CD           or 9 coefficients follow ITYP in flexible FORMAT. If it is
CD           necessary to use two cards they should be consecutive.
CD ITYP = 3  Form factors given tabulated against s=sin(theta)/lambda.
CD           the tabulated values, pairs of s and f in ascending s, follow.
CD           If multiple cards are needed each should start "F <name> 3" and
CD           they must be consecutive.
CD ITYP = 4  As 2, but the result is multiplied by sin(theta)/lambda squared
CD           This is the form of expansion used for the <jl> form factors
CD           with l not zero.
CD ITYP = 5  Obtain form factor from radial functions supplied on
CD           W RADF cards.
CD ITYP negative indicates an anomalous scattering factor.
CD       At present only type -1 is defined, which is the straightforward
CD       complex anomalous scattering factor.  Two numbers follow the -1
CD       on the "F" card; they are f' and f" the real and imaginary parts
CD       of the complex multiplicative factor to be applied to this atom
CD       (in addition to any other scattering factor).
C
CI Reads "F" cards from the copy of the Crystal Data File on unit IO10.
CO Writes its findings to unit LPT.
CN If "slack constraints only" is set, allows "no F cards".
C
      CHARACTER *4 LABF,LSAV
      LOGICAL ANOM
      COMMON /ANSCAT/NAMODE(20),FDASH(20),KOM2
      COMPLEX FDASH
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /FORGRP/NATFOR(20,150),NAFPNT(20)
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SLKGEO/NSTYP,BOBS(500),EOBS(500),IATM(500,2),
     & ISYM(500),ILAT(500),CELLTR(3,500),XSLAK(3,500),
     & COSIN(3,3),IABASE(500),NST1,SLONLY,TOSTAR(6,6),BCALC(500),
     & DERCEL(6,500),DERPOS(3,500,2),ITYPSK(500),INVBON(10,
     & 500),NINVB(500),INANG(100,3),INTOR(100,6),
     & DERBON(10),NVB(10),NUMBON,NTARNM,NUMANG,NUMTOR,KOM25
      LOGICAL SLONLY
C
C ATOPOS MUST BE OBEYED TO SET UP REQUIRED SCATTERING FACTOR NAMES IN THE TABLE
C FONAME
      IF (INREAD(1) .GT. 0) CALL ATOPOS
C
C SET "F" CARDS READ:
      INREAD(6)=-IABS(INREAD(6))
C
C CLEAR SCALE FACTORS, POINTERS FOR BOTH SORTS OF SCATTERING FACTORS
C AND COUNTS OF NUMBERS OF TERMS (IN CASE TYPES 2 OR 3):
C%
C      DO 1 I=1,%FORM%
      DO 1 I=1,20
      CMULT(I)=1.0
      NT(I)=0
      MODE(I)=0
   1  NAMODE(I) = 0
C
C CLEAR FACTORS-ATOMS TABLES:
C%
C      DO 50 I=1,%FORM%
      DO 50 I=1,20
      NAFPNT(I)=0
C%
C      DO 50 J=1,%ATOM%
      DO 50 J=1,150
  50  NATFOR(I,J)=0
C
C SET "NO ANOMALOUS SCATTERING AT ALL" AND KEEP NUMBER OF FACTORS TO FIND:
      ANOM=.FALSE.
      NFKEEP=NUMFNM
C
C ID WORKS THROUGH "F" CARDS SEQUENTIALLY:
      NFREAD=0
      ID=IABS(INREAD(6))
C
C NCARD = NUMBER OF "F" CARDS ON CRYSTAL DATA FILE:
      NCARD=ICDNO(6)
      IF (NCARD .EQ. 0) THEN
        IF (.NOT. SLONLY) CALL ERRMES(1,1,'No cards labelled F read')
        GO TO 100
      ENDIF
C
   2  CALL INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      NFREAD=NFREAD+1
      ID=ID+NYZ
   5  IF (IER .NE. 0) IERR=IERR+1
      IPKEEP=IPT
C INPUTF READS THE LABEL, THE TYPE AND THE WHOLE CARD TO ICARD AS A1, AND
C LEAVES IPT POINTING TO WHERE TO CONTINUE READING.
C
C FIND LABEL IN TABLE MADE UP BY ATOPOS - IF THE NEW LABEL IS NOT THERE, IT
C DOES NOT APPLY TO ANY GIVEN POSITIONS.  IF A NEW LABEL IS ADDED, AND OVER-
C FILLS THE TABLE (INDICATING TOO MANY FACTORS FOR THE SPACE ALLOWED IN
C COMMON /FORMDA/), LMATCH WILL COMPLAIN:
      LKEEP=NUMFNM
C%
C      LFAC=LMATCH(LABF,FONAME,NUMFNM,%FORM%)
      LFAC=LMATCH(LABF,FONAME,NUMFNM,20)
      IF (LKEEP .NE. NUMFNM) THEN
        CALL MESS(LPT,1,'WARNING ** no atom card asks for factor '//
     & 'labelled '//LABF)
      ENDIF
C
C NTYP IS -VE FOR ANOMALOUS, 0 FOR A SCALE AND +VE FOR A SCATTERING FACTOR:
      IF (NTYP) 11,12,13
C
C ANOMALOUS SCATTERING:
  11  ANOM = .TRUE.
      IF (NAMODE(LFAC) .NE. 0) THEN
        CALL ERRCH2(LABF,2,
     &   'reading 2nd anomalous scattering factor for',' ')
        GO TO 6
      ENDIF
C
      NAMODE(LFAC)=IABS(NTYP)
      IF (NAMODE(LFAC) .EQ. 1) GO TO 3
      WRITE (LPT,3003) NTYP,LABF
      WRITE (ITO,3003) NTYP,LABF
3003  FORMAT (/' ERROR ** type',I4,' anomalous scattering ',
     & 'factor not recognised for factor labelled ',A4)
      IERR=IERR+1
      GO TO 6
C
C RECORD MODE IN NAMODE, AND FOR NOW EXPECT ONLY 1 - THEN READ FDASH AND FDDASH
   3  I=NAMODE(LFAC)
      CALL RDREAL(XX,IPT,IPT,80,IER)
      IF (IER .NE. 0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
        GO TO 6
      ENDIF
C
      IPKEEP=IPT
      CALL RDREAL(Y,IPT,IPT,80,IER)
      IF (IER .NE. 0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
      ELSE
        FDASH(LFAC)=CMPLX(XX,Y)
      ENDIF
      GO TO 6
C
C SIMPLE SCALE FACTOR:
  12  CALL RDREAL(XX,IPT,IPT,80,IER)
      IF (IER .NE. 0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
        GO TO 6
      ENDIF
C
      CMULT(LFAC)=CMULT(LFAC)*XX
      GO TO 6
C
C SCATTERING FACTOR:
  13  IF (NTYP .LE. 5) GO TO 7
      WRITE(LPT,3002) NTYP,LABF
      WRITE(ITO,3002) NTYP,LABF
3002  FORMAT (/' ERROR ** type',I4,' scattering factor not',
     & ' recognised for factor labelled ',A4)
      IERR=IERR+1
      GO TO 6
C
   7  IF (MODE(LFAC) .EQ. 0) GO TO 20
      CALL ERRCH2(LABF,2,'reading second scattering factor for',
     & ' ')
      GO TO 6
C
  20  MODE(LFAC)=NTYP
      GO TO (21,22,23,22,6) , NTYP
C
C TYPE 1 - NEUTRON NUCLEAR SCATTERING - READ IN 1 MORE NUMBER:
  21  CALL RDREAL(XX,IPT,IPT,80,IER)
      IF (IER .NE. 0) THEN
        CALL ERRIN2(IPKEEP,2,'in number on "F" card at point',' ')
        GO TO 6
      ENDIF
C
      CMULT(LFAC)=CMULT(LFAC)*XX
      GO TO 6
C
C HERE ON TYPE 2 - EXPONENTIAL APPROXIMATION:
  22  IF (NT(LFAC) .LT. 9) GO TO 9
  15  CALL ERRCH2(LABF,2,
     & '>9 coefficients for exponential approx for factor',' ')
      GO TO 6
C
C READ HOWEVER MANY NUMBERS THERE ARE ON THE CARD:
   9  IPKEEP=IPT
      CALL RDREAL(XX,IPT,IPT,80,IER)
C IF IER IS RETURNED AS 100 WE HAVE READ ALL SPACES:
      IF (IER .EQ. 100) GO TO 10
      IF (IER .NE. 0) THEN
        CALL ERRIN2(IPKEEP,2,
     &   'cannot read number from "F" card at point',' ')
        GO TO 6
      ENDIF
C
  14  NT(LFAC)=NT(LFAC)+1
      IF (NT(LFAC) .GT. 9) GO TO 15
      F(NT(LFAC),LFAC)=XX
      IF (IPT .LT. 80) GO TO 9
C
C END OF NUMBERS ON CARD - CHECK THERE ARE MORE CARDS:
  10  IF (NFREAD .GE. NCARD) GO TO 16
      LSAV=LABF
      CALL INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      NFREAD=NFREAD+1
      ID=ID+NYZ
      IF (NTYP .NE. 2) GO TO 5
      IF (LSAV .NE. LABF) GO TO 5
C
C WE HAVE ANOTHER CARD FOR THE SAME FACTOR:
      GO TO 9
C
C HERE ON TYPE 3 - READ POSSIBLY SEVERAL MORE F CARDS
C%
C  23  IF (NT(LFAC) .LT. %FTAB%) GO TO 8
  23  IF (NT(LFAC) .LT. 40) GO TO 8
  35  CALL ERRCH2(LABF,1,'more than %FTAB% entries in table for factor',
     & ' ')
      GO TO 6
C
C READ HOWEVER MANY NUMBERS THERE ARE ON THE CARD:
   8  IPKEEP=IPT
      CALL RDREAL(XX,IPT,IPT,80,IER)
C IF IER IS RETURNED AS 100 WE HAVE READ ALL SPACES:
      IF (IER .EQ. 100) GO TO 30
      IF (IER .NE. 0) THEN
        CALL ERRIN2(IPKEEP,2,
     &   'cannot read number from "F" card at point',' ')
        GO TO 6
      ENDIF
C
  34  IF (NT(LFAC) .EQ. 0) LL=1
      LL=-LL
      IF (LL .GT. 0) GO TO 33
      NT(LFAC)=NT(LFAC)+1
C%
C      IF (NT(LFAC) .GT. %FTAB%) GO TO 35
      IF (NT(LFAC) .GT. 40) GO TO 35
      S(NT(LFAC),LFAC)=XX
      F(NT(LFAC),LFAC)=9999.
      IF (NT(LFAC) .EQ. 1) GO TO 36
      IF (S(NT(LFAC),LFAC) .GT. S(NT(LFAC)-1,LFAC)) GO TO 36
      WRITE (LPT,3011) NT(LFAC),S(NT(LFAC),LFAC),S(NT(LFAC)-1,
     & LFAC),LABF
      WRITE (ITO,3011) NT(LFAC),S(NT(LFAC),LFAC),S(NT(LFAC)-1,
     & LFAC),LABF
3011  FORMAT (/' ERROR ** TERM',I5,' OF TABLE IS',F10.4,
     & ' AND IS LESS THAN THE PREVIOUS TERM',F10.4,' FOR FACTOR ',A4)
      GO TO 6
  33  F(NT(LFAC),LFAC)=XX
  36  IF (IPT .LT. 80) GO TO 8
C
C END OF NUMBERS ON CARD - CHECK THERE ARE MORE:
  30  IF (NFREAD .GE. NCARD) GO TO 16
      LSAV=LABF
      CALL INPUTF(ID,LABF,LBFLEN,NTYP,IPT,IER)
      NFREAD=NFREAD+1
      ID=ID+NYZ
      IF (NTYP .NE. 3) GO TO 5
      IF (LSAV .NE. LABF) GO TO 5
C
C WE HAVE ANOTHER CARD FOR THE SAME FACTOR:
      GO TO 8
C
C HERE WHEN CARD HAS BEEN DEALT WITH - ARE THERE MORE?
   6  IF (NFREAD .LT. NCARD) GO TO 2
C
C NOW WRITE OUT ALL INFORMATION ASSIMILATED:
  16  CALL MESS(LPT,1,'Scattering factors read for given atomic '//
     & 'positions:')
      CALL MESS(LPT,0,'Name  Type          Description')
C
C SCAN ALL FACTORS AS DECODED BY ATOPOS:
      DO 41 LFAC=1,NFKEEP
      IF (MODE(LFAC) .LE. 0) THEN
        CALL ERRCH2(FONAME(LFAC),1,'factor','needed but not read')
        GO TO 41
      ENDIF
C
      M=MODE(LFAC)
      GO TO (43,44,45,46,47) , M
C
  43  WRITE (LPT,2001) FONAME(LFAC),M,CMULT(LFAC)
2001  FORMAT (/1X,A4,I5,'  Neutron nuclear scattering factor value',
     & F10.4)
      GO TO 40
C
  44  N1=NT(LFAC)
      WRITE (LPT,2002) FONAME(LFAC),M,N1,CMULT(LFAC),(F(I,LFAC),
     & I=1,N1)
2002  FORMAT (/1X,A4,I5,' Series expansion form factor with',I4,
     & ' terms multiplied by',F10.4/11X,'Coefficients are:'/11X,9F10.4)
      GO TO 40
C
  45  N1=NT(LFAC)
      WRITE (LPT,2003)FONAME(LFAC),M,N1,CMULT(LFAC),(S(I,LFAC),
     & F(I,LFAC),I=1,N1)
2003  FORMAT (/1X,A4,I5,'  Form factors given in table with',I5,
     & ' entries multiplied by',F10.4/11X,'Table is:'/
     & (1X,5(F10.2,F10.4)))
      IF (F(NT(LFAC),LFAC) .NE. 9999.) GO TO 40
      CALL MESS(LPT,1,'WARNING ** an odd number of numbers was read'//
     & ' as the table of S and F')
      CALL MESS(ITO,1,'WARNING ** an odd number of numbers was read'//
     & ' as the table of S and F')
      GO TO 40
C
  46  N1=NT(LFAC)
      WRITE (LPT,2002) FONAME(LFAC),M,N1,CMULT(LFAC),(F(I,LFAC),
     & I=1,N1)
      CALL MESS(LPT,0,
     & '          Factor is finally multiplied by s squared')
      GO TO 40
C
  47  WRITE (LPT,2030) FONAME(LFAC),M
2030  FORMAT (/1X,A4,I5,'  Form factor to be calculated from radial',
     & ' wave functions given on W RADF cards')
      GO TO 40
C
C WRITE ANOMALOUS SCATTERING FACTOR IF PRESENT:
  40  IF (NAMODE(LFAC).NE.0) WRITE (LPT,2004) FONAME(LFAC),FDASH(LFAC)
2004  FORMAT (' ',A4,' also has anomalous scattering factor - coeff',
     & 'icients:',2F10.4)
C
C FORM LIST OF ATOM LABELS TO WHICH THIS FACTOR APPLIES:
      IPT=0
      ICARD=' '
      DO 48 I=1,NATOM
      IF (NFORMF(I) .NE. LFAC) GO TO 48
C ALSO, FILL IN TABLES FOR GROUPS OF ATOMS:
      NAFPNT(LFAC)=NAFPNT(LFAC)+1
      NATFOR(LFAC,NAFPNT(LFAC))=I
C
C PUT ATOM NAME INTO BUFFER IF IT WILL FIT:
      IF (IPT .GT. 54) THEN
        WRITE (LPT,2005) (ICARD(IJ:IJ),IJ=1,LENGT(ICARD))
2005    FORMAT (12X,'applies to ',80A1)
        ICARD=' '
        IPT=0
      ENDIF
C
      LEN=LENGT(ATNAME(I))
      ICARD(IPT+1:IPT+LEN)=ATNAME(I)(1:LEN)
      IPT=IPT+LEN+1
  48  CONTINUE
      IF (IPT.NE.0) WRITE (LPT,2005) (ICARD(IJ:IJ),IJ=1,LENGT(ICARD))
C
  41  CONTINUE
 100  RETURN
C
      END
C
C
C
C
C LEVEL 5      SUBROUTINE SETGAU
      SUBROUTINE SETGAU
C
C *** SETGAU BY JCM ***
C
CX
CC 2A
CH Sets up the COMMON /GAUSS/ for Gaussian integration (for use in
CH absorption correction type integrals).
CP NL,NM,NN in /GAUSS/ should be the required number of Gauss points
CP        to scan the given crystal in the 3 directions x,y,z.
CP INPUTG should have read all the "G" cards.
C
CD Sets up a 3D mesh of Gauss points and weights covering the given
CD crystal whose faces are described in /CPLANE/.  In /GAUSS/
CD the arrays XX, YY, ZZ hold the x,y and z coordinates of the points
CD referred to the standard crystallographic axes, and the array WW
CD holds the weights.
CO Writes the crystal volume so found to unit LPT.
C
CN SETGAU may be called several times in one job, to enable comparison
CN between integrations using different numbers of points.
C
      LOGICAL TESTOV
      COMMON /CPLANE/AA(15),BB(15),CC(15),DD(15),NP
      COMMON /GAUSS/XX(1000),YY(1000),ZZ(1000),WW(1000),NL,NM,NN,NQ
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRAT/GPT(150),GWT(150)
C
C
      IF (INREAD(7) .GT. 0) CALL INPUTG
      NQ = NL*NM*NN
C%
C      CALL ERRCHK(1,NQ,%GPTS%,0,'Gauss points, product of all 3')
      CALL ERRCHK(1,NQ,1000,0,'Gauss points, product of all 3')
      U = 0.
      V = 0.
      DO 11 IR = 1,NP
      DO 11 IS = 1,NP
      D = BB(IR)*CC(IS) - CC(IR)*BB(IS)
      DO 11 IT = 1,NP
      E = BB(IS)*CC(IT) - CC(IS)*BB(IT)
      F = BB(IT)*CC(IR) - CC(IT)*BB(IR)
      A = DD(IR)*E + DD(IS)*F + DD(IT)*D
      B = AA(IR)*E + AA(IS)*F + AA(IT)*D
      IF (TESTOV(A,B)) GO TO 11
      X = A/B
      A = (DD(IS) - AA(IS)*X)*CC(IT) - (DD(IT) - AA(IT)*X)*CC(IS)
      IF (TESTOV(A,E)) GO TO 11
      Y = A/E
      A = DD(IT) - AA(IT)*X -BB(IT)*Y
      IF (TESTOV(A,CC(IT))) GO TO 11
      Z = A/CC(IT)
      DO 12 I = 1,NP
      IF (AA(I)*X + BB(I)*Y + CC(I)*Z - DD(I) .GT. 0.0001) GO TO 11
  12  CONTINUE
      IF (U .LT. X) U=X
      IF (X .LT. V) V=X
  11  CONTINUE
      U = U-V
      IO = 1
C
C SET UP ENTIRE RANGES OF GAUSS POINTS AND WEIGHTS IN /SCRAT/:
      ITEM1=1
      ITEM2=NL
      CALL GAUSPT(NL,GPT(ITEM1),GWT(ITEM1))
      DO 16 IR=ITEM1,ITEM2
      X = V + U*GPT(IR)
      E = 6000.
      D = -6000.
      DO 17 IS = 1,NP
      DO 17 IT = 1,NP
      A = (DD(IS) - AA(IS)*X)*CC(IT) - (DD(IT) - AA(IT)*X)*CC(IS)
      B = BB(IS)*CC(IT) - CC(IS)*BB(IT)
      IF (TESTOV(A,B)) GO TO 17
      Y = A/B
      A = DD(IT) - AA(IT)*X - BB(IT)*Y
      IF (TESTOV(A,CC(IT))) GO TO 17
      Z = A/CC(IT)
      DO 18 IQ = 1,NP
      IF (AA(IQ)*X + BB(IQ)*Y + CC(IQ)*Z - DD(IQ) .GT. 0.0001) GO TO 17
  18  CONTINUE
      IF (D .LT. Y) D=Y
      IF (Y .LT. E) E=Y
  17  CONTINUE
      D = D-E
C
C SET UP GAUSS POINTS AND WEIGHTS FOR SECOND INTEGRATION DIRECTION (UNLESS SAME
C AS FIRST):
      ITEM3=ITEM1
      ITEM4=ITEM2
      IF (NM .EQ. NL) GO TO 1
      ITEM3=ITEM2+1
      ITEM4=ITEM3+NM-1
      CALL GAUSPT(NM,GPT(ITEM3),GWT(ITEM3))
   1  DO 16 IS = ITEM3,ITEM4
      Y = E + D*GPT(IS)
      G = 6000.
      F = -6000.
      DO 23 IT = 1,NP
      A = DD(IT) - AA(IT)*X - BB(IT)*Y
      IF (TESTOV(A,CC(IT))) GO TO 23
      Z=A/CC(IT)
      DO 24 IQ = 1,NP
      IF (AA(IQ)*X + BB(IQ)*Y + CC(IQ)*Z - DD(IQ) .GE. 0.0001) GO TO 23
  24  CONTINUE
      IF (F .LT. Z) F=Z
      IF (Z .LT. G) G=Z
  23  CONTINUE
      F = F-G
      ITEM5=ITEM1
      ITEM6=ITEM2
      IF (NN .EQ. NL) GO TO 2
      ITEM5=ITEM3
      ITEM6=ITEM4
      IF (NN .EQ. NM) GO TO 2
      ITEM5=ITEM4+1
      ITEM6=ITEM5+NN-1
      CALL GAUSPT(NN,GPT(ITEM5),GWT(ITEM5))
   2  DO 16 IT = ITEM5,ITEM6
      Z = G + F*GPT(IT)
      XX(IO) = X
      YY(IO) = Y
      ZZ(IO) = Z
      WW(IO) = D*F*GWT(IR)*GWT(IS)*GWT(IT)
      IO = IO + 1
  16  CONTINUE
      V = 0.
      DO 22 IQ = 1,NQ
      V = V + WW(IQ)
  22  CONTINUE
      IF (TESTOV(1.,V)) CALL ERRMES(1,0,
     & '- is origin strictly inside crystal?')
C
      VSTAR = 1/V
      DO 29 IQ = 1,NQ
      WW(IQ) = VSTAR*WW(IQ)
  29  CONTINUE
      V=V*U
      WRITE (LPT,2000) V
2000  FORMAT (/' Crystal volume ',F12.5)
      RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE SETGEN(S)
      SUBROUTINE SETGEN(S)
C
C *** SETGEN corrected by JBF 15 Aug 94 ***
C
CX
CC 3A
CH Sets up the generation of a complete set of reflection indices.
CA On entry S is the maximum value of Sin(theta)/lambda required.
CP SYMUNI to define the asymmetric unit
CD Uses the defined asymmetric unit and given S to set up COMMON /HKLGEN/
CD ready for repeated entries to GETGEN.  GETGEN will generate all hkl's in
CD the given asymmetric unit with sin(theta)/lambda < S.
C
CD S is put into COMMON /BRAGG/ here.
C
      DIMENSION TEMP(3)
      LOGICAL ONCARD,BINDIG
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
      COMMON /HKLGEN/STEP(3,3),PT(3,3),VECEND(3,3),PRPT(3,3),
     & NPRIM(2,2),NP,LFAC(2),MCOUNT(2),KOM5
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      STHLMX=S
      IF (S .NE. 0.) GO TO 5
C IF S ZERO, TRY TO FIND "SMAX" ON AN I CARD:
      IF (.NOT. ONCARD('I','SMAX',STHLMX)) THEN
C NEED SMAX:
      CALL ERRMES(2,1,'max sin theta/lambda to generate hkl values')
      GO TO 100
      ENDIF
C
C     SET UP DEFAULT VALUES
   5  CALL GMZER(PT,3,3)
      CALL GMZER(STEP,3,3)
      CALL GMZER(PRPT,3,3)
C
C     SWITCH ACCORDING TO THE NUMBER OF SYMMETRY PLANES:
      N1=NASYM+1
      GO TO (1,2,3,4) , N1
C
C NO PLANES:
   1  CALL GMUNI(STEP,3)
      GO TO 10
C
C ONE PLANE:
   2  CALL GMEQ(ASYM(1,1),STEP(1,3),1,3)
      CALL INVENT(STEP(1,3),STEP(1,3),STEP(1,1))
C THAT MIDDLE VALUE OF STEP IS JUST SOME VECTOR NOT IN THE PLANE
      CALL  INVENT(STEP(1,3),STEP(1,1),STEP(1,2))
      GO TO 10
C
C TWO PLANES:
   3  CALL GMEQ(EDGE(1,3),STEP(1,3),1,3)
      CALL INVENT(ASYM(1,1),STEP(1,3),STEP(1,1))
      CALL INVENT(ASYM(1,2),STEP(1,3),STEP(1,2))
      GO TO 10
C
C THREE PLANES:
   4  CALL GMEQ(EDGE,STEP,3,3)
C
C JOIN - DETERMINE VOLUME OF CELL
   10 DO 12 I=1,3
   12 CALL FCTOR(STEP(1,I),N)
      NP=NINT(DETER3(STEP))
      IF (NP.GT.0) GO TO 11
      NP=-NP
      CALL GMEQ(STEP(1,1),TEMP,1,3)
      CALL GMEQ(STEP(1,2),STEP(1,1),1,3)
      CALL GMEQ(TEMP,STEP(1,2),1,3)
C
C SET UP LIMITS AND STARTING VALUES:
  11  J=2
      K=3
      DO 16 I=1,3
      CALL VECPRD(STEP(1,J),STEP(1,K),VECEND(1,I))
      ALNGTH=2.*STHLMX*VCTMOD(1.,VECEND(1,I),1)
      LENGTH=IFIX(ALNGTH/N)+1
      DO 15 L=1,3
      VECEND(L,I)=VECEND(L,I)/ALNGTH
C DEPENDING ON THE NUMBER OF BOUNDING PLANES OF THE ASYMMETRIC
C UNIT, THE PREVIOUS POINT VECTOR STARTS AT 0 OR FAR END OF VECTOR:
      IF (NASYM .EQ. 3) GO TO 15
      IF ((NASYM .EQ. 2) .AND. (I .NE. 3)) GO TO 15
      IF ((NASYM .EQ. 1) .AND. (I .EQ. 3)) GO TO 15
      PRPT(L,3)=PRPT(L,3)-FLOAT(LENGTH)*STEP(L,I)
  15  CONTINUE
      J=K
  16  K=I
      CALL GMSUB(PRPT(1,3),STEP(1,1),PRPT(1,1),3,1)
      CALL GMEQ(PRPT(1,3),PRPT(1,2),1,3)
C START POINT IN PT ALSO:
      CALL GMEQ(PRPT,PT,3,3)
      IF (BINDIG(IOUT,16)) THEN
        WRITE (LPT,4000) STEP,PT,VECEND
        WRITE (ITO,4000) STEP,PT,VECEND
 4000   FORMAT (/' STEP,PT,VECEND :'/3(3(3F8.2/)/))
      ENDIF
      CALL PRMTIV
 100  RETURN
      END
C
C

C

C
C
C LEVEL 1      SUBROUTINE SHFESD(J)
      SUBROUTINE SHFESD(J)
C
C *** SHFESD by JCM 7 May 86 ***
C
CX
CC 6C
CH During the application of LSQ shifts, calculates the shift and ESD for
CH a "redundant" variable.
C
CA On entry J points to the constraint information in /CONSTR/
CA            for this redundant variable (which is related to a set
CA            of basic variables by the given constraint J)
C
CD On exit  SHIFT is set to the linear combination of relevant shifts
CD                in basic variables,
CD          ESD is set to the square root of the sum of squares of their esds.
C
      COMMON /CONSTR/JCONST,JROWPT(301),JCMAT(200),AMOUNT(200),
     & NEXTJ
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /MATDAT/MATPNT(401),BLSQ(400)
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      JROW=JROWPT(J)
      JNEXT=JROWPT(J+1)-1
      SHIFT=0.
      ESD=0.
      DO 1 M=JROW,JNEXT
      SHIFT=SHIFT + AMOUNT(M)*BLSQ(JCMAT(M))
   1  ESD=ESD + (AMOUNT(M)*DERIVB(JCMAT(M)))**2
      ESD=SQRT(ESD)
      RETURN
      END
C
C

C
C
C LEVEL 1      SUBROUTINE SORTN(IVAL,IP,N)
      SUBROUTINE SORTN(IVAL,IP,N)
C
C *** SORTN updated by JHM/JCM 22 Aug 86 ***
C
CX
CC 16C
CH Sorts pointers to an integer array using Heapsort.
C
CA On entry IVAL is an array of N integers.
CA On exit IP is an array of N pointers to IVAL in ascending order
C
CN Copyright John Matthewman 18 July 1983
CN  HEAPSORT
CN  (See Knuth 'Art of Computer Programming' Vol 3, Section 5.2.3)
C
      DIMENSION IVAL(N),IP(N)
C
C EXTRA PART (WHICH MAY BE REMOVED AGAIN) - SET UP POINTERS:
      DO 16 I=1,N
  16  IP(I)=I
      IF (N .LT. 2) GO TO 100
C  INITIALISE
      L=N/2+1
      IR=N
    1 L=L-1
      K=IP(L)
    3 J=L
      GO TO 4
C
C  SIFTING LOOP
    5 IF (IVAL(IP(J)).LT.IVAL(IP(J+1))) J=J+1
    7 IP(I)=IP(J)
    4 I=J
      J=J+J
      IF (J-IR) 5,7,8
C
C  FLOYDS MODIFICATION
   10 IP(J)=IP(I)
    8 J=I
      I=I/2
      IF (I) 6,6,9
    9 IF (J.GT.L .AND. IVAL(K).GT.IVAL(IP(I))) GO TO 10
   6  IP(J)=K
C
C  END OF A SIFT
    2 IF (L.GT.1) GO TO 1
      K=IP(IR)
      IP(IR)=IP(1)
      IR=IR-1
      IF (IR.GT.1) GO TO 3
      IP(1)=K
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SORTX(VAL,IP,N)
      SUBROUTINE SORTX(VAL,IP,N)
C
C *** SORTX updated by JHM/JCM 22 Aug 86 ***
C
CX
CC 16C
CH Sorts pointers to a real array using Heapsort.
C
CA On entry VAL is an array of N real numbers.
CA On exit IP is an array of N pointers to VAL in ascending order
C
CN Copyright John Matthewman 18 July 1983
CN  HEAPSORT
CN  (See Knuth 'Art of Computer Programming' Vol 3, Section 5.2.3)
C
C
      DIMENSION VAL(N),IP(N)
C
C EXTRA PART (WHICH MAY BE REMOVED AGAIN) - SET UP POINTERS:
      DO 16 I=1,N
  16  IP(I)=I
      IF (N .LT. 2) GO TO 100
C  INITIALISE
      L=N/2+1
      IR=N
    1 L=L-1
      K=IP(L)
    3 J=L
      GO TO 4
C
C  SIFTING LOOP
    5 IF (VAL(IP(J)).LT.VAL(IP(J+1))) J=J+1
    7 IP(I)=IP(J)
    4 I=J
      J=J+J
      IF (J-IR) 5,7,8
C
C  FLOYDS MODIFICATION
   10 IP(J)=IP(I)
    8 J=I
      I=I/2
      IF (I) 6,6,9
    9 IF (J.GT.L .AND. VAL(K).GT.VAL(IP(I))) GO TO 10
   6  IP(J)=K
C
C  END OF A SIFT
    2 IF (L.GT.1) GO TO 1
      K=IP(IR)
      IP(IR)=IP(1)
      IR=IR-1
      IF (IR.GT.1) GO TO 3
      IP(1)=K
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SPACE(NT,NSCR,NGENS)
      SUBROUTINE SPACE(NT,NSCR,NGENS)
C
C *** SPACE corrected by PJB  02-Nov-1994 ***
C
Cx
CC 1A
CH Interprets symbols from an S card which are the space group name.
CA On entry NT is a string of characters representing a space group short name.
CA There must be at least one space between elements, e.g. P21 21 21.
CA On exit NSCR is the unit on which S cards have been temporarily written.
CA         NGENS is the number of generators.
CD Generates S cards for subequent interpretation by SYMOP.
CN This is not the best or simplest way of doing this, but it came from a
CN working program.
CN *** SPACE adapted from an anonymous program by JCM 22 Aug 92 ***
C
      CHARACTER *1 LATLET(7),NDIGS(7),SPALET(6)
      CHARACTER *4 FIRST,MID,LAST
      CHARACTER *16 NT
      CHARACTER *12 IN
      CHARACTER *12 SYSTEM(6)
      CHARACTER *4 NTEX(21),AMTSA(3),AMGES(3,2)
      CHARACTER *4 TX(6,3),TEXB(9)
      LOGICAL TRI,MONO,ORTHO,TETRA,HEXA,CUBIC,CENTRE
      LOGICAL AFACE,BFACE,CFACE,BODY,RHOMB,FACE,PRIM,SLASH,MINUS
      LOGICAL EMPTY(3),LETT1(3),OUTSID
      LOGICAL FIRDIG(6),FIRA,FIRB,FIRC,FIRM,FIRN,FIRD,FIR21
      LOGICAL MIDA,MIDB,MIDC,MIDD,MIDM,MIDN,MID2,MID21
      LOGICAL LASTA,LASTB,LASTC,LASTD,LASTM,LASTN,LAST2
      DIMENSION MGES(3,3,21),NET(3),NES(3),MGET(3,13),
     & MSS(48,3,3),MTS(48,3),MSSS(3,3),MTSS(3),MTSA(3,3),NSV(3,18),
     & NSW(3)
      EQUIVALENCE (FIRST,IN(1:4))
      EQUIVALENCE (MID,IN(5:8))
      EQUIVALENCE (LAST,IN(9:12))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      DATA NDIGS/'0','1','2','3','4','5','6'/
      DATA LATLET/'A','B','C','I','R','F','P'/
      DATA SPALET/'A','B','C','M','N','D'/
      DATA SYSTEM/'TRICLINIC','MONOCLINIC','ORTHORHOMBIC','TETRAGONAL',
     &  'HEXAGONAL','CUBIC'/
      DATA MGES/1,3*0,1,3*0,1,-1,3*0,-1,3*0,1,0,1,0,2*-1,3*0,1,0,1,
     & 0,-1,4*0,1,0,1,3*0,2*1,2*0,2*1,0,-1,4*0,1,-1,3*0,-1,3*0,-1,1,
     & 3*0,1,3*0,-1,0,-1,0,2*1,3*0,-1,0,-1,0,1,4*0,2*-1,3*0,1,3*0,3*-1,
     & 0,1,4*0,-1,1,3*0,-1,3*0,1,0,1,0,1,4*0,-1,0,-1,0,-1,4*0,2*1,2*0,
     & 2*-1,3*0,2*-1,2*0,2*1,3*0,1,-1,3*0,1,3*0,1,0,1,0,1,4*0,2*1,3*0,
     & -1,3*0,-1,0,-1,0,-1,4*0,-1/
C MGES HOLDS:
C   1:  1  0  0     2: -1  0  0    3:  0  1  1    4:  0  1  0    5:  0  1  0
C       0  1  0         0 -1  0       -1 -1  0       -1  0  0        0  0  1
C       0  0  1         0  0  1        0  0  1        0  0  1        1  0  0
C
C   6:  1  1  0     7: -1  0  0    8:  1  0  0    9:  0 -1  0   10:  0 -1  0
C      -1  0  0         0 -1  0        0  1  0        1  1  0        1  0  0
C       0  0  1         0  0 -1        0  0 -1        0  0 -1        0  0 -1
C
C  11: -1  0  0    12: -1 -1  0   13:  1  0  0   14:  0  1  0   15:  0 -1  0
C       0  1  0         1  0  0        0 -1  0        1  0  0       -1  0  0
C       0  0 -1         0  0 -1        0  0  1        0  0 -1        0  0  1
C
C  16:  1  0  0    17: -1  0  0   18: -1  0  0   19:  0  1  0   20:  1  0  0
C      -1 -1  0         1  1  0        0  1  0        1  0  0        0 -1  0
C       0  0 -1         0  0  1        0  0  1        0  0  1        0  0 -1
C
C  21:  0 -1  0
C      -1  0  0
C       0  0 -1
C
      DATA NTEX/3*'    ','+1/8','+1/6','    ','+1/4',
     & '    ','+1/3','+3/8',2*'    ','+1/2',3*'    ','+2/3',
     & '    ','+3/4','    ','+5/6'/
      DATA TEXB/'  -Z','  -Y',' X-Y','  -X','    ','   X',' Y-X','   Y',
     & '   Z'/
      DATA MGET/0,0,0,    12,0,0,  0,12,0,  0,0,12,  0,12,12,  12,0,12,
     &        12,12,0,  12,12,12,   0,6,6,   6,0,6,    6,6,0,    6,6,6,
     &         18,6,6/
C MGET HOLDS:
C           1:     0    0    0                8:    1/2  1/2  1/2
C           2:    1/2   0    0                9:     0   1/4  1/4
C           3:     0   1/2   0               10:    1/4   0   1/4
C           4:     0    0   1/2              11:    1/4  1/4   0
C           5:     0   1/2  1/2              12:    1/4  1/4  1/4
C           6:    1/2   0   1/2              13:    3/4  1/4  1/4
C           7:    1/2  1/2   0
C
      DATA NSV/6,0,0,    0,6,0,   0,0,6,   0,0,4,   0,0,8,    9,9,0,
     &         6,6,6,    6,6,0,  6,18,0,  6,18,3,  6,18,6,   6,18,9,
     &        12,6,9,   0,12,0,  0,18,0,   0,0,0,  0,12,0,  12,12,12/
C NSV HOLDS:
C           1:    1/4   0    0               10:    1/4  3/4  1/8
C           2:     0   1/4   0               11:    1/4  3/4  1/4
C           3:     0    0   1/4              12:    1/4  3/4  3/8
C           4:     0    0   1/6              13:    1/2  1/4  3/8
C           5:     0    0   1/3              14:     0   1/2   0
C           6:    3/8  3/8   0               15:     0   3/4   0
C           7:    1/4  1/4  1/4              16:     0    0    0
C           8:    1/4  1/4   0               17:     0   1/2   0
C           9:    1/4  3/4   0               18:    1/2  1/2  1/2
C
C     READ THE SYMBOL, DETERMINE BRAVAIS LATTICE AND CRYSTAL FAMILY.
   2  JS=1
      JR=0
C DEFAULT MONOCLINIC:
      MSYS=2
      IN='000000000000'
      MINUS=.FALSE.
      SLASH=.FALSE.
C PUT THE SYMBOL INTO UPPER CASE:
      CALL UPPER(NT)
      DO 9 IS=1,16
      IF (NT(IS:IS) .NE. ' ') GO TO 10
   9  CONTINUE
      CALL ERRMES (1,0,'No space group symbol')
  10  NBR=NCFIND(NT(IS:IS),LATLET,7)
      IF (NBR .EQ. 0) CALL ERRCH2(NT(IS:IS),0,'lattice letter',
     & 'not recognised')
      AFACE=(NBR .EQ. 1)
      BFACE=(NBR .EQ. 2)
      CFACE=(NBR .EQ. 3)
      BODY =(NBR .EQ. 4)
      RHOMB=(NBR .EQ. 5)
      FACE =(NBR .EQ. 6)
      PRIM =(NBR .EQ. 7)
C
      OUTSID=.TRUE.
      DO 1 I=IS+1,16
      IF (OUTSID .AND. NT(I:I) .EQ. ' ') GO TO 1
      IF (JS .LE. 12) IN(JS:JS)=NT(I:I)
      IF (IN(JS:JS) .EQ. '-') THEN
        IF (JS .NE. 1) THEN
          CALL ERRMES(1,0,'Minus other than at start of first symbol')
        ELSE
          MINUS=.TRUE.
          JS=JS-1
        ENDIF
      ENDIF
      IF (IN(JS:JS) .EQ. '/') THEN
        IF (JS .NE. 2 .AND. JS .NE. 3) THEN
          CALL ERRMES(1,0,'Slash other than within first symbol')
        ELSE
          SLASH=.TRUE.
        ENDIF
      ENDIF
      IF (NT(I:I) .EQ. ' ') THEN
        OUTSID=.TRUE.
        IF (JS .LE. 12) IN(JS:JS)='0'
        JR=JR+1
        JS=4*JR
      ELSE
        OUTSID=.FALSE.
      ENDIF
      JS=JS+1
      IF (NT(I:I) .EQ. '3' .OR. NT(I:I) .EQ. '6') MSYS=5
   1  CONTINUE
C
      DO 3 J=1,3
      I=4*J-3
      EMPTY(J)=IN(I:I) .EQ. '0'
      LETT1(J)=(NCFIND(IN(I:I),SPALET,6) .GT. 0)
*      DIGIT1(J) =(NCFIND(IN(I:I),NDIGS(2),6) .GT. 0)
   3  CONTINUE
      IF (EMPTY(1)) CALL ERRMES(1,0,'No space group symbols')
      DO 7 I=1,6
   7  FIRDIG(I)=(FIRST(1:1) .EQ. NDIGS(I+1))
      FIRA=(FIRST(1:1) .EQ. 'A')
      FIRB=(FIRST(1:1) .EQ. 'B')
      FIRC=(FIRST(1:1) .EQ. 'C')
      FIRM=(FIRST(1:1) .EQ. 'M')
      FIRN=(FIRST(1:1) .EQ. 'N')
      FIRD=(FIRST(1:1) .EQ. 'D')
      FIR21=(FIRST(1:2) .EQ. '21')
      MIDA=(MID(1:1) .EQ. 'A')
      MIDB=(MID(1:1) .EQ. 'B')
      MIDC=(MID(1:1) .EQ. 'C')
      MIDD=(MID(1:1) .EQ. 'D')
      MIDM=(MID(1:1) .EQ. 'M')
      MIDN=(MID(1:1) .EQ. 'N')
      MID2=(MID(1:1) .EQ. '2')
      MID21=(MID(1:2) .EQ. '21')
      LAST2=(LAST(1:1) .EQ. '2')
      LASTA=(LAST(1:1) .EQ. 'A')
      LASTB=(LAST(1:1) .EQ. 'B')
      LASTC=(LAST(1:1) .EQ. 'C')
      LASTD=(LAST(1:1) .EQ. 'D')
      LASTM=(LAST(1:1) .EQ. 'M')
      LASTN=(LAST(1:1) .EQ. 'N')
C
      IF (FIRDIG(4)) MSYS=4
      IF (MID(1:1).EQ.'3') MSYS=6
      IF (MSYS.EQ.2.AND.FIRST(1:1).GE.'2'.AND.MID(1:1).GE.'2') MSYS=3
      IF (FIRDIG(1) .AND. EMPTY(2)) MSYS=1
      TRI=  (MSYS .EQ. 1)
      MONO= (MSYS .EQ. 2)
      ORTHO=(MSYS .EQ. 3)
      TETRA=(MSYS .EQ. 4)
      HEXA= (MSYS .EQ. 5)
      CUBIC=(MSYS .EQ. 6)
C
C       FOR MONOCLINIC SETTING WITH ONLY 1 FIELD, FORCE B AXIS UNIQUE:
      IF (MONO .AND. EMPTY(2)) THEN
        MID=FIRST
        FIRST='    '
        EMPTY(2)=.FALSE.
      ENDIF
C
C       SELECTION OF GENERATORS:
C
      NG=0
      KL=0
      DO 6 I=1,3
      NES(I)=1
      NET(I)=0
      DO 6 J=1,3
   6  MTSA(I,J)=0
C
C FORM FIRST 2 (DECMAL) DIGITS OF FIRST SYMBOL IN NDIG1 AND NSCRW1.  IF THEY
C WERE NOT DIGITS, THIS IS MEANINGLESS.  IF THERE IS NO SCREW NSCRW1 WILL BE
C EITHER -1 (IF / PRESENT) OR 0 (IF FIRST SYMBOL IS IN FACT A SINGLE DIGIT).
      NDIG1=ICHAR(FIRST(1:1))-ICHAR('0')
      NSCRW1=ICHAR(FIRST(2:2))-ICHAR('0')
      IF (TRI .OR. TETRA .OR. HEXA .OR. CUBIC) THEN
C
C        POINT GROUPS 1,-1,3,-3,4,-4,6,-6,4/M,6/M.
C
        NET(1)=NDIG1
        KL=24*NSCRW1/NDIG1
        IF (MINUS) NET(1)=NDIG1+6
        IF (NDIG1.EQ.3) NG=1
        IF (NDIG1.GE.3 .AND. NDIG1.LE.6 .AND. NSCRW1.GT.0) MTSA(1,3)=KL
        IF (EMPTY(2)) THEN
          NG=1
          IF (SLASH) THEN
            NET(2)=8
            IF (FIRST(4:4).EQ.'A' .OR. FIRST(3:3).EQ.'A') NES(2)=2
            IF (FIRST(4:4).EQ.'N' .OR. FIRST(3:3).EQ.'N') NES(2)=7
            NG=2
          ENDIF
          GO TO 400
        ENDIF
      ENDIF
C
C       MONOCLINIC-ORTHORHOMBIC. ((OR MORE THAN ONE AXIS))
C
      DO 105 I=1,3
      DO 105 J=1,4
      L=4*(I-1)+J
      M=L+4
      IF (M.GT.12) M=M-12
C
C      MONOCLINIC - ORTHORHOMBIC ACTUALLY STARTS HERE
C
      IF (MONO .OR. ORTHO) THEN
C
C   DETECT 2-FOLD AXIS NOT FOLLOWED BY A LETTER:
C
*        IF (IN(L:L).EQ. '2' .AND. IN(M:M) .LT. 'A')THEN
        IF (IN(L:L).EQ.'2' .AND. (NCFIND(IN(M:M),SPALET,6).EQ.0)) THEN
          NG=NG+1
          NET(NG)=29-9*I
          IF (IN(L+1:L+1) .EQ. '1') NES(NG)=I+1
          IF (NG.EQ.2 .AND. LAST(2:2) .EQ. '1') MTSA(2,3)=12
          IF (NG.EQ.2) GO TO 400
        ENDIF
C
C   WE HAVE A LETTER:
C
*        IF (IN(L:L) .GT. '6') THEN
        LETL=NCFIND(IN(L:L),SPALET,6)
        IF (LETL .GT. 0) THEN
          NG=NG+1
          NET(NG)=23-5*I
** TURNS A, B OR C INTO 2, 3 OR 4:
          IF (LETL .GE. 1 .AND. LETL .LE. 3) NES(NG)=LETL+1
          IF (IN(L:L) .EQ. 'N') NES(NG)=I+4
          IF (IN(L:L) .EQ. 'D') NES(NG)=I+8
        ENDIF
C
C       3. TETRAGONAL-HEXAGONAL-CUBIC
C
      ELSE IF (IN(M:M) .GT. '1') THEN
C
        IF (I.NE.3 .OR. SLASH .OR.
     &   (CUBIC .AND.(LETT1(1) .OR. EMPTY(3)))) THEN
          NG=NG+1
          IF (NG.EQ.4)NG=3
C  FIRST CYCLE  :
          IF (I.EQ.1) THEN
            IF (CUBIC) THEN
               NET(1)=5
            ELSE IF (MID2) THEN
              NET(NG)=36-4*MSYS
              IF (MID21) NES(1)=2
            ELSE IF (LETT1(2)) THEN
              NET(NG)=22-MSYS
            ENDIF
C
C   SECOND CYCLE :
C
          ELSE IF (I.EQ.2) THEN
            IF (LAST2) NET(NG)=21
            IF (LETT1(3)) NET(NG)=19
            IF (CUBIC) THEN
              IF (LAST2) NET(2)=14
              IF (MINUS) NET(2)=15
            ENDIF
          ELSEIF (I.EQ.3) THEN
            IF (NCFIND(IN(M:M),SPALET,6) .GT. 0) NET(NG)=8
            IF (IN(M:M) .EQ. '2') NET(NG)=2
          ENDIF
          IF (FIR21) NES(2)=6
*          IF (MID2 .AND. LAST2 .AND. I.EQ.1 .AND. FIRST(2:2).GT.'0')
          IF (MID2 .AND. LAST2 .AND. I.EQ.1 .AND. NSCRW1.GT.0)
     &     MTSA(2,3)=24-MTSA(1,3)
          IF (.NOT. FIRDIG(3)) MTSA(1,3)=0
*          IF (NET(2) .EQ. 14 .AND. FIRST(2:2) .GT. '0') THEN
          IF (NET(2) .EQ. 14 .AND. NSCRW1 .GT. 0) THEN
            MTSA(2,1)=24-KL
            MTSA(2,2)=KL
            MTSA(2,3)=KL
          ENDIF
          N=NCFIND(IN(M:M),LATLET,3)
          IF (N .GT. 0) NES(NG)=N+1
          IF (IN(M:M) .EQ. 'N') THEN
            NES(NG)=4+I
            IF ( I.EQ.2) NES(NG)=8
          ELSE IF (IN(M:M) .EQ. 'D')THEN
            NES(NG)=14-I
          ENDIF
          IF (LASTD .AND. CUBIC) THEN
            IF (FIRA) THEN
C GROUP 230 - I A 3 D:
              NES(2)=12
            ELSE
C GROUP 220 - I -4 3 D:
              NES(2)=13
            ENDIF
          ENDIF
        ENDIF
      ENDIF
 105  CONTINUE
C
C       COMPLETE SYMMETRY OPERATIONS
C
 400  NGENS=NG
      DO 415 K=1,NG
      DO 415 I=1,3
      MTS(K,I)=MGET(I,NES(K))+MTSA(K,I)
      DO 415 J=1,3
 415  MSS(K,I,J)=MGES(I,J,NET(K))
C
      NSS=-1
      K=0
 610  K=K+1
      IF (K.GT.NG) GO TO 617
      L=1
      IF (MSS(K,1,1)+MSS(K,2,2)+MSS(K,3,3).EQ.-3)NSS=K
      IF (MSS(K,1,1)+MSS(K,2,2)+MSS(K,3,3).EQ.3)NU=K
  616 IF (L.GT.NGENS) GO TO 610
      DO 620 I=1,3
      ITN=MTS(K,I)
      DO 620 J=1,3
      N=0
      ITN=ITN+MSS(K,I,J)*MTS(L,J)
      DO 625 M=1,3
  625 N=N+MSS(K,I,M)*MSS(L,M,J)
      ITN=MOD(ITN+24,24)
      MSSS(I,J)=N
 620  MTSS(I)=ITN
      DO 640 KK=1,NG
      N=0
      DO 645 I=1,3
      DO 645 J=1,3
  645 IF (MSSS(I,J).NE.MSS(KK,I,J)) N=1
      IF (N.EQ.0) GO TO 618
  640 CONTINUE
      NG=NG+1
      DO 646 I=1,3
      MTS(NG,I)=MTSS(I)
      DO 646 J=1,3
  646 MSS(NG,I,J)=MSSS(I,J)
  618 L=L+1
      IF (K.EQ.NSS) THEN
        DO 720 I=1,3
        J=I+1
        IK=I+2
        IF (J.GT.3)J=J-3
        IF (IK.GT.3)IK=IK-3
C
C  REMOVE LATTICE TRANSLATIONS -  FIRST I THEN F,A,B,C:
C
        IF (BODY .AND. I.EQ.1) THEN
          IF (MTS(NSS,1) .GE. 12 .AND. MTS(NSS,2) .GE. 12
     &     .AND. MTS(NSS,3) .GE. 12) THEN
            DO 700 IJ=1,3
 700        MTS(NSS,IJ)=MTS(NSS,IJ)-12
          ENDIF
        ELSE IF (FACE .OR. NBR.EQ.I) THEN
          IF (MTS(NSS,J).GE.12.AND.MTS(NSS,IK).GE.12)THEN
            DO 718 II=1,3
 718        MTS(NSS,II)=MTS(NSS,II)-MGET(II,I+4)
          ENDIF
        ENDIF
 720    CONTINUE
      ENDIF
      GO TO 616
C
C       DETERMINE CENTROSYMMETRY AND SHIFT VECTOR TO A CENTRE OF SYMM..
C
 617  CENTRE = (NSS.GT.0)
      DO 619 I=1,3
C RE-USE OF ARRAY MTSA, WHICH WAS THE SCREW AXIS TRANSLATION:
 619  MTSA(1,I)=0
      IF (CENTRE) THEN
        DO 615 K=1,3
          MTSA(1,K)=MOD((MTS(NSS,K)/2)+24,24)
 615    CONTINUE
      ENDIF
C
C   DETERMINE THE REF. NUMBER SHIFT VECTOR TO AN ORIGIN OF I.T..
      IIS=1
      DO 5 I=1,3
   5  NSW(I)=0
C NOW SET MS TO PICK SPECIAL INDIVIDUAL VECTORS TO GET THE ORIGIN RIGHT:
      MS=16
      IF (IIS.EQ.1) THEN
        IF (ORTHO) THEN
C
C          ORTHORHOMBIC         ********
C
          IF (FIRST(2:2) .EQ. '1' .AND. MID(2:2) .EQ. '1') THEN
C GROUP P21 21 2 (7 MORE ORTHO GROUPS & P42 N M ALSO HAVE MS=8):
            IF (LAST(2:2) .EQ. '0') MS=8
C GROUPS P (& I)21 21 21 (P N C 2 & A B M 2 ALSO HAVE MS=2):
            IF (LAST(2:2) .EQ. '1') MS=2
          ENDIF
C
C GROUPS P (& A & I)B A 2, PN A 21, PN N 2 (THE NEXT 5 ORTHO WITH MS=8):
C    (BUT NB THIS DOES TOO MANY WITH MIDA):
          IF ((MIDA .OR. (FIRN .AND. MIDN)) .AND. LAST2) THEN
            MS=8
C GROUPS PC A 21 AND P (& A & I) M A 2 (THE ONLY 4 GROUPS WITH MS=1):
            IF (FIRM .OR. FIRC) MS=1
          ENDIF
C
C GROUPS CM M A & CM C A (THE LAST 2 OF THE 8 ORTHOS WITH MS=8):
          IF (CFACE .AND. FIRM .AND. LASTA) MS=8
C
C GROUPS A B M 2 & P N C 2 (P&I 21 21 21 HAVE BEEN SET ALREADY):
          IF ((FIRN .OR. FIRB).AND.(MIDC .OR. MIDM)
     &     .AND. LAST2) MS=2
C
C GROUPS I B C A AND I M M A (IA 3, F41 3 2 & I A 3 D ALSO HAVE MS=7):
          IF (BODY .AND. LASTA) MS=7
C
C GROUP F D D 2 (THE ONLY GROUP WITH MS=6):
          IF (MIDD .AND. LAST2) MS=6
        ELSE IF (TETRA)THEN
C
C                 TETRAGONAL         *********
C
          IF (.NOT. SLASH)THEN
C GROUP 80 : I41 (AND OTHERS WHICH GET RESET LATER)
            IF (BODY .AND. NSCRW1.EQ.1) MS=9
            IF ((MID2.AND.LAST2) .AND. (NSCRW1.EQ.1.OR.NSCRW1.EQ.3))THEN
C GROUPS 91,92 : P41 2 2, P43 2 2 (AND OTHERS WHICH GET RESET LATER):
              MS=3
C GROUP 98 : I41 2 2 (96, P43 21 2 NEEDS MS=12 BUT IT IS NOT SET HERE):
              IF (BODY) MS=12
            ENDIF
C THE FOLLOWING LOOP LOOKS FOR 1,2 AND 3 - AND, NB, 0, MEANING "NO DIGIT":
C THESE SET:
C MS=9 FOR GROUPS 90,113 : P 4 21 2, P-4 21 M (AND OTHERS RESET LATER)
C MS=10 JUST FOR GROUP 90 : P41 21 2 (THE ONLY GROUP WITH MS=10)
C MS=11 JUST FOR GROUP 94 : P42 21 2 (114, P-4 21 C NEEDS MS=11 BUT NOT HERE)
C MS=12 JUST FOR GROUP 96 : P43 21 2 (98, I41 2 2 NEEDS MS=12 BUT ALREADY HAS)
            IF (MID21) THEN
              DO 815 I=1,4
              IF (NSCRW1 .EQ. I-1) MS=8+I
 815        CONTINUE
            ENDIF
C
            IF (LASTC) THEN
C GROUP 112 : P-4 2 C (AND 114 : P-4 21 C WHICH IS IMMEDIATELY RESET)
              IF (MID2) MS=3
C GROUP 114 : P -4 21 C
              IF (MID21) MS=11
            ENDIF
C
C GROUP I-4 2 D (THE ONLY GROUP WITH MS=13) (& OTHERS WHICH ARE LATER RESET):
            IF (LASTD) MS=13
C GROUP 116 : P-4 C 2 (AND 120 : I -4 C 2 WHICH IS LATER RESET)
            IF (MIDC .AND. MINUS) MS=3
            IF (MIDN) THEN
C GROUP P 42 N M (THE OTHER 8 FOR MS=8 ARE ORTHRHOMBIC):
              IF (NSCRW1 .EQ. 2) MS=8
C GROUPS P 4 N C & P -4 N 2 (THERE ARE 3 MORE WITH MS=9):
              IF (NSCRW1 .EQ. 0) MS=9
            ENDIF
C GROUP I 41 M D (THE ONLY GROUP WITH MS=15):
            IF (MIDM .AND. LASTD) MS=15
          ENDIF
C GROUPS P 4 B M, P42 B C, I4 C M, I41 C D, P -4 B 2, I -4 C 2, P4/N B M,
C        P4/M B M, P42/N B C, P42/M B C & I 4/M C M (& I41/A C D WHICH IS RESET
C        LATER):
          IF (MIDB .OR. BODY.AND.MIDC) MS=14
C GROUPS P42/N & P42/N N M:
          IF (FIRST(4:4).EQ.'N' .AND. (EMPTY(2) .OR. MIDN)) MS=14
C GROUPS P4/M N C & P4/N N C:
          IF (MIDN.AND.(FIRST(3:3).EQ.'M'.OR.FIRST(3:3).EQ.'N')) MS=14
          IF (FIRST(4:4).EQ.'A') THEN
C GROUP 141 : I 41/A M D (THE ONLY GROUP WITH MS=17)
            IF (MIDM) MS=17
C GROUP 142 : I 41/A C D (THE ONLY GROUP WITH MS=18)
            IF (MIDC) MS=18
          ENDIF
        ELSE
C
C ALL TYPES EXCEPT ORTHO AND TETRA:
C GROUPS P31 1 2, P31 2 1, P32 1 2, P32 1 2 (THE ONLY GROUPS GIVING MS=4 OR 5):
          IF (FIRDIG(3) .AND. .NOT. EMPTY(2)) THEN
            IF (NSCRW1 .EQ. 1) MS=4
            IF (NSCRW1 .EQ. 2) MS=5
          ENDIF
C CUBIC GROUPS F41 3 2 & IA 3 & IA 3 D (2 ORTHO GROUPS ALSO HAVE MS=7):
          IF (FACE .AND. NSCRW1.EQ.1 .OR. BODY .AND. FIRA) MS=7
        ENDIF
      ENDIF
C
*      IF (ONCARD('S','ORIG',A) THEN
*      CALL RDNUMS( for 3 values off card)
*      IIS=2
*      ENDIF
*** THIS WAS:
C       SHIFT TO ANOTHER ORIGIN
C
**        WRITE(ITO,2026)
**2026    FORMAT (' Type the shift vector:')
**        READ (ITI,1014) NV
**1014    FORMAT (A12)
**        JN=2
**        JM=0
**        DO 840 I=1,12
**        IF (JN.LT.14) IN(JN:JN)=NV(I:I)
**        IF (NV(I:I) .EQ. ' ') THEN
**          JM=JM+1
**          JN=4*JM+1
**        ENDIF
** 840    JN=JN+1
**        JP=0
**        DO 845 I=1,9,4
**        JP=JP+1
**        IO=I+1
**        LO=I+3
**        IF (IN(LO:LO).NE.'0') NSW(JP)=
**     1   (ICHAR(IN(IO:IO))-ICHAR('0'))*24/(ICHAR(IN(LO:LO))-ICHAR('0'))
** 845    CONTINUE
**      ENDIF
C
C
C       APPLY THE SHIFT OF ORIGIN TO ALL OPERATIONS.
C
** IIS IS ALWAYS 1 OR 2 NOW
      IF (CENTRE.OR.IIS.GT.0) THEN
        DO 823 K=1,3
          IF (IIS.NE.1)NSV(K,MS)=0
C MTSA HAS THE HALF ORIGIN SHIFT IN IT BY NOW:
          NSW(K)=NSV(K,MS)+MTSA(1,K)+NSW(K)
          NSW(K)=MOD(NSW(K)+24,24)
 823    CONTINUE
        DO 825 I=1,NG
        DO 825 J=1,3
          L=MTS(I,J)
          DO 822 K=1,3
 822      L=L-(MSS(NU,J,K)-MSS(I,J,K))*NSW(K)
 825      MTS(I,J)=MOD(L+144,24)
      ENDIF
C
C       NORMALIZATION OF CENTRING TYPE.
C
 830  DO 755 I=1,NG
      DO 755 J=1,3
C
      K=MOD(J,3)+1
      KI=MOD(K,3)+1
      IF (BODY .AND. J.EQ.1)THEN
        IF(MTS(I,1).GE.12.AND.MTS(I,2).GE.12.AND.MTS(I,3).GE.12)THEN
          DO 765 IJ=1,3
 765      MTS(I,IJ)=MTS(I,IJ)-12
        ENDIF
      ELSEIF (FACE .OR. NBR.EQ.J)THEN
        IF (MTS(I,K).GE.12 .AND. MTS(I,KI).GE.12)THEN
          DO 775 II=1,3
 775      MTS(I,II)=MTS(I,II)-MGET(II,J+4)
        ENDIF
      ENDIF
 755  CONTINUE
C
C OUTPUT:
C
C DIAGNOSTIC CCSL OUTPUT :
      IF (IOUT .EQ. 16) THEN
        WRITE (LPT,4000) SYSTEM(MSYS)
4000    FORMAT (/' Crystal family',29X,': ',A12)
        IF (CENTRE) THEN
          WRITE (LPT,4001) NT
4001      FORMAT (' Space group (centrosymmetric)',14X,':   ',A16)
        ELSE
          WRITE (LPT,4002) NT
4002      FORMAT (' Space group (noncentrosymmetric)',11X,': ',A16)
        ENDIF
        IF (IIS.EQ.1) THEN
          DO 422 I=1,3
          AMTSA(I)=NTEX(NSW(I)+1)
          IF (NSW(I).NE.0) AMTSA(I)(1:1)=' '
          IF (NSW(I).EQ.0) AMTSA(I)='   0'
 422      CONTINUE
          WRITE (LPT,4003) (AMTSA(I),I=1,3)
4003      FORMAT (' Shift vector (leads to the orig. of symb.):',3A4)
        ENDIF
C
        WRITE (LPT,4004) NT,MS,NGENS,NES,NET
4004    FORMAT (1X,A16,'MS=',I4,' NGENS=',I2,' NES=',3I4,' NET=',3I4)
        IR=-1
        WRITE (LPT,4005)
4005    FORMAT (/' Symmetry-operations'/)
 420    IR=IR+2
        IE=IR+1
        DO 425 I=IR,IE
        IU=I
        IO=MOD(IU,2)
        IF (IO.EQ.0)IO=2
        DO 425 J=1,3
        MO=MSS(I,J,1)+MSS(I,J,2)*3+MSS(I,J,3)*4+5
        TX(J,IO)=TEXB(MO)
 425    AMGES(J,IO)=NTEX(MTS(I,J)+1)
        IF (IE.LE.NG) WRITE (LPT,4006) IR,(TX(J,1),
     &  AMGES(J,1),J=1,3),IE,(TX(J,2),AMGES(J,2),J=1,3)
4006    FORMAT(2X,I2,'.',1X,2(A4,A4,' ,'),A4,A4,4X,I2,'.',1X,
     &  2(A4,A4,' ,'),A4,A4)
        IF (IE.GT.NG) WRITE (LPT,4006) IR,(TX(J,1),AMGES(J,1),J=1,3)
        IF (IE.LT.NG) GO TO 420
      ENDIF
C
C OUTPUT S CARD GENERATORS TO SCRATCH FILE:
      DO 8 I=1,NGENS
      WRITE (NSCR,5000) (TEXB(MSS(I,J,1)+MSS(I,J,2)*3+MSS(I,J,3)*4+5),
     & NTEX(MTS(I,J)+1),J=1,3)
5000  FORMAT ('S ',2(2A4,','),2A4)
   8  CONTINUE
C
C ADJUST NGENS IF FURTHER S CARDS ARE TO BE WRITTEN
C CENTROSYMMETRIC OPERATOR:
      IF (CENTRE) THEN
        NGENS=NGENS+1
        WRITE (NSCR,5001)
5001  FORMAT ('S -X, -Y, -Z')
      ENDIF
C
C NON-PRIMITIVE LATTICES:
      IF (NBR.LT.7) NGENS=NGENS+1
      IF (NBR.EQ.6) NGENS=NGENS+2
      IF (AFACE .OR. FACE) WRITE (NSCR,5002)
5002  FORMAT ('S X, 1/2+Y, 1/2+Z')
      IF (BFACE .OR. FACE) WRITE (NSCR,5003)
5003  FORMAT ('S 1/2+X, Y, 1/2+Z')
      IF (CFACE .OR. FACE) WRITE (NSCR,5004)
5004  FORMAT ('S 1/2+X, 1/2+Y, Z')
      IF (BODY) WRITE (NSCR,5005)
5005  FORMAT ('S 1/2+X, 1/2+Y, 1/2+Z')
      IF (RHOMB) WRITE (NSCR,5006)
5006  FORMAT ('S 2/3+X, 2/3+Y, 1/3+Z'/'S 1/3+X, 2/3+Y, 2/3+Z')
      REWIND (NSCR)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SPGNAM(N,NAME)
      SUBROUTINE SPGNAM(N,NAME)
C
C *** SPGNAM by JCM 30 Aug 92 ***
C
CX
CC 1A
CH Decodes space group symbol or integer from S GRUP card
CA On entry N is the space group number,
CA            or, if 0, means that the group symbol is given.
CA On exit NAME is 16 characters of short space group symbol.
CP On entry the card is in /SCRACH/
C
      CHARACTER *16 NAME
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      CHARACTER *16 SGSYMB(230),TRIMON(15),ORTHO(59),TETRA(68)
      CHARACTER *16 TRIHEX(52),CUBIC(36)
      EQUIVALENCE (SGSYMB(1),TRIMON(1)),(SGSYMB(16),ORTHO(1))
      EQUIVALENCE (SGSYMB(75),TETRA(1)),(SGSYMB(143),TRIHEX(1))
      EQUIVALENCE (SGSYMB(195),CUBIC(1))
      DATA TRIMON/'P1','P-1','P2','P21','C2','PM','PC','CM','CC','P2/M',
     & 'P21/M','C2/M','P2/C','P21/C','C2/C'/
      DATA ORTHO/'P2 2 2','P2 2 21','P21 21 2','P21 21 21','C2 2 21',
     & 'C2 2 2','F2 2 2','I2 2 2','I21 21 21','PM M 2',
     & 'PM C 21','PC C 2','PM A 2','PC A 21','PN C 2',
     & 'PM N 21','PB A 2','PN A 21','PN N 2','CM M 2',
     & 'CM C 21','CC C 2','AM M 2','AB M 2','AM A 2',
     & 'AB A 2','FM M 2','FD D 2','IM M 2','IB A 2',
     & 'IM A 2','PM M M','PN N N','PC C M','PB A N',
     & 'PM M A','PN N A','PM N A','PC C A','PB A M',
     & 'PC C N','PB C M','PN N M','PM M N','PB C N',
     & 'PB C A','PN M A','CM C M','CM C A','CM M M',
     & 'CC C M','CM M A','CC C A','FM M M','FD D D',
     & 'IM M M','IB A M','IB C A','IM M A'/
      DATA TETRA/'P4','P41','P42','P43','I4',
     & 'I41','P-4','I-4','P4/M','P42/M',
     & 'P4/N','P42/N','I4/M','I41/A','P4 2 2',
     & 'P4 21 2','P41 2 2','P41 21 2','P42 2 2','P42 21 2',
     & 'P43 2 2','P43 21 2','I4 2 2','I41 2 2','P4 M M',
     & 'P4 B M','P42 C M','P42 N M','P4 C C','P4 N C',
     & 'P42 M C','P42 B C','I4 M M','I4 C M','I41 M D',
     & 'I41 C D','P-4 2 M','P-4 2 C','P-4 21 M','P-4 21 C',
     & 'P-4 M 2','P-4 C 2','P-4 B 2','P-4 N 2','I-4 M 2',
     & 'I-4 C 2','I-4 2 M','I-4 2 D','P4/M M M','P4/M C C',
     & 'P4/N B M','P4/N N C','P4/M B M','P4/M N C','P4/N M M',
     & 'P4/N C C','P42/M M C','P42/M C M','P42/N B C','P42/N N M',
     & 'P42/M B C','P42/M N M','P42/N M C','P42/N C M','I4/M M M',
     & 'I4/M C M','I41/A M D','I41/A C D'/
      DATA TRIHEX/'P3','P31','P32','R3','P-3',
     & 'R-3','P3 1 2','P3 2 1','P31 1 2','P31 2 1',
     & 'P32 1 2','P32 2 1','R3 2','P3 M 1','P3 1 M',
     & 'P3 C 1','P3 1 C','R3 M','R3 C','P-3 1 M',
     & 'P-3 1 C','P-3 M 1','P-3 C 1','R-3 M','R-3 C',
     & 'P6','P61','P65','P62','P64',
     & 'P63','P-6','P6/M','P63/M','P6 2 2',
     & 'P61 2 2','P65 2 2','P62 2 2','P64 2 2','P63 2 2',
     & 'P6 M M','P6 C C','P63 C M','P63 M C','P-6 M 2',
     & 'P-6 C 2','P-6 2 M','P-6 2 C','P6/M M M','P6/M C C',
     & 'P63/M C M','P63/M M C'/
      DATA CUBIC/'P2 3','F2 3','I2 3','P21 3','I21 3',
     & 'PM 3','PN 3','FM 3','FD 3','IM 3',
     & 'PA 3','IA 3','P4 3 2','P42 3 2','F4 3 2',
     & 'F41 3 2','I4 3 2','P43 3 2','P41 3 2','I41 3 2',
     & 'P-4 3 M','F-4 3 M','I-4 3 M','P-4 3 N','F-4 3 C',
     & 'I-4 3 D','PM 3 M','PN 3 N','PM 3 N','PN 3 M',
     & 'FM 3 M','FM 3 C','FD 3 M','FD 3 C','IM 3 M','IA 3 D'/
C
      IF (N .GT. 0) THEN
        NAME=SGSYMB(N)
      ELSE
        I=7
   1    IF (ICARD(I:I) .EQ. ' ') THEN
          I=I+1
          IF (I .LE. 80) GO TO 1
          NAME=' '
        ELSE
          NAME=ICARD(I:I+15)
        ENDIF
      ENDIF
      RETURN
      END
C
C



C
C
C LEVEL 1      SUBROUTINE SUBSYM(ITAB)
      SUBROUTINE SUBSYM(ITAB)
C
C *** SUBSYM corrected by PJB 24-Oct-94 ***
C
CX
CC 1B
CH Replaces all the symmetry parameters by those of a subgroup.
C
CA On entry ITAB is a table of dimension NOPC in which ITAB(I)=+-1
CA               if element I (or the -ve element) is in the subgroup
CA          IABS(ITAB(I)) is not 1 otherwise
CA          ITAB(1)=-1 if the sub-group is non-centrosymmetric.
CP SYMOP
CD Replaces the full symmetry by the configurational symmetry of a
CD magnetic structure.
CD Stores all necessary quantities in the COMMON /OLDSYM/.
CN The routine SYMBAK may be used to restore the stored symmetry.
C
      DIMENSION ITAB(24),IGTAB(3),KTAB(24),MTAB(24),NTAB(24)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /OLDSYM/OSYM(3,3,24),OTRANS(3,24),JNVERS(24),
     & JNORD(24),MOLTAB(24,24),IOGEN(3),NOPONT(24),NOPO,
     & NCENTO,NOPCO
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      CALL MESS(LPT,1,
     & 'Full symmetry replaced by configurational symmetry')
C STORE NUMBERS
      NOPO=NOP
      NCENTO=NCENT
      NOPCO=NOPC
C COPY ALL SYMMETRY ELEMENTS:
      CALL GMEQ(SYM,OSYM,9,NOPC)
      CALL GMEQ(TRANS,OTRANS,3,NOPC)
      CALL JGMEQ(INVERS,JNVERS,1,NOPC)
      CALL JGMEQ(NORD,JNORD,1,NOPC)
      CALL JGMEQ(MULTAB,MOLTAB,24,NOPC)
      CALL JGMEQ(IGEN,IOGEN,1,3)
C
      CALL JGMEQ(ITAB,NTAB,1,NOPC)
C
C  IS THERE A CENTRE OF SYMMETRY
      NCENT=1
      IF (ITAB(1).EQ.1) NCENT=2
      CENTRC=(NCENT .EQ. 2)
C COUNT ELEMENTS OF THE EXTRACTED SUBGROUP IN IS:
      IS=1
C NOPONT(NEW ELEMENT)=OLD ELEMENT ADDRESS:
      NOPONT(1)=1
C KTAB(OLD ELEMENT)=NEW ELEMENT ADDRESS:
      CALL JGMZER(KTAB,NOPC,1)
      KTAB(1)=1
C COUNT DOWN ORDERS OF SYMMETRY ELEMENTS:
      DO 8 M=6,2,-1
      IF (M .EQ. 5) GO TO 8
      CALL JGMZER(MTAB,NOPC,1)
C COUNT ELEMENTS OF THE ORIGINAL GROUP:
      DO 1 I=2,NOPC
C PICK OUT ONLY ELEMENTS OF THE SUBGROUP:
      IF (IABS(NTAB(I)).NE.1) GO TO 1
C THEN PICK ONLY THOSE OF ORDER M:
      IF (MOD(IABS(JNORD(I)),100) .NE. M) GO TO 1
      II=I
C COUNT ALL POWERS OF THIS ELEMENT UP TO M:
      DO 9 K=1,M-1
      IF (NTAB(II) .EQ. 0) GO TO 9
      IS=IS+1
      MORD=MOD(JNORD(II),100)
      IF(K.NE.1) THEN
        NORD(IS)=ISIGN(IABS(MORD)+100,MORD)
      ELSE
        NORD(IS)=MORD
      ENDIF
      IF (NTAB(II) .EQ. -1) THEN
        NOPONT(IS)=-INVERS(II)
        KTAB(INVERS(II))=IS
        NORD(IS)=-NORD(IS)
      ELSE
        NOPONT(IS)=II
        KTAB(II)=IS
      ENDIF
      NTAB(II)=0
      MTAB(II)=NORD(IS)
   9  II=MOLTAB(II,I)
   1  CONTINUE
   8  CONTINUE
C
C RECONSTITUTE SYMMETRY, NOW ONLY FOR SUBGROUP:
      NOPC=IS
      DO 2 I=2,NOPC
      IC=IABS(NOPONT(I))
      CALL GMEQ(OSYM(1,1,IC),SYM(1,1,I),3,3)
      CALL GMEQ(OTRANS(1,IC),TRANS(1,I),3,1)
C  WAS THERE AN INVERSION?
      IF (NOPONT(I).LT.0) THEN
        CALL GMREV(SYM(1,1,I),SYM(1,1,I),3,3)
        INVERS(I)=KTAB(IC)
      ENDIF
      INVERS(I)=KTAB(JNVERS(IC))
C
      DO 2 J=2,NOPC
      ISS=IABS(NOPONT(J))
      M=MOLTAB(IC,ISS)
      MULTAB(I,J)=KTAB(M)
    2 CONTINUE
C
      NOP=NOPC*NCENT
      CALL JGMEQ(NORD,KTAB,NOPC,1)
      IGTAB(1)=ITAB(1)*NOPC
      CALL GENELM(MTAB,IGTAB)
      CALL JGMEQ(IGTAB(2),IGEN,2,1)
      NGEN=1
      IF (IGEN(2).NE.0) NGEN=2
      IF (IOUT .GT. 100) THEN
        WRITE(LPT,4000) ((MULTAB(I,J),I=1,NOPC),J=1,NOPC)
4000    FORMAT (' Multiplication table for subgroup:'/8(1X,8I5/))
        WRITE (LPT,4001) (NORD(I),I=1,NOPC)
4001    FORMAT ('   NORD: ',8I4)
        WRITE (LPT,4002) (INVERS(I),I=1,NOPC)
4002    FORMAT (' INVERS: ',8I4)
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE SYMBAK
      SUBROUTINE SYMBAK
C
C *** SYMBAK by PJB 1 May 92 ***
C
CX
CC 1B
CH Restores the original symmetry operators after a call to SUBSYM.
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /OLDSYM/OSYM(3,3,24),OTRANS(3,24),JNVERS(24),
     & JNORD(24),MOLTAB(24,24),IOGEN(3),NOPONT(24),NOPO,
     & NCENTO,NOPCO
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      CALL MESS(LPT,1,'Restoring full symmetry')
C RESTORE NUMBERS
      NOP=NOPO
      NOPC=NOPCO
      NCENT=NCENTO
      CENTRC=(NCENT .EQ. 2)
C COPY ALL SYMMETRY ELEMENTS:
      CALL GMEQ(OSYM,SYM,9,NOPC)
      CALL GMEQ(OTRANS,TRANS,3,NOPC)
      CALL JGMEQ(JNVERS,INVERS,1,NOPC)
      CALL JGMEQ(JNORD,NORD,1,NOPC)
      CALL JGMEQ(MOLTAB,MULTAB,24,NOPC)
      CALL JGMEQ(IOGEN,IGEN,1,3)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE SYMCEN(NC)
      SUBROUTINE SYMCEN(NC)
C
C *** SYMCEN by JCM 11 Jul 83 ***
C
CX
CC 1A
CH A specialist routine used during the input of space group symmetry
CH to discover whether there is a centre of symmetry.
C
CP Called at the start of routine SYMGEN.
C
CD Takes the temporary copy of the space group as generated in the
CD scratch COMMON /SCRAT/ and scans it for a centre of symmetry at the
CD origin.
CD
CD Sets NCENT=1 for non-centrosymmetric, 2 for centrosymmetric.
CD      CENTRC=.TRUE. if centrosymmetric, .FALSE. otherwise
CD      NC=0 if non-centrosymmetric, or the number of the (-x,-y,-z)
CD           operator.
CD      NOPC=NOP/NCENT.
C
CO Writes its findings on unit LPT.
C
      CHARACTER *5 WORD(2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/TSYM(3,3,%SY*2%),TTRANS(3,%SY*2%),
      COMMON /SCRAT/TSYM(3,3,48),TTRANS(3,48),
C%
C     & MLTAB(%SY*2%,%SY*2%),ITAB(%SYMO%),JTAB(%SY*2%),
     & MLTAB(48,48),ITAB(24),JTAB(48),
C%
C     & NNORD(%SY*2%),D(3,3)
     & NNORD(48),D(3,3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      DATA WORD/'Non-c','    C'/
C
      NCENT=1
      NC=0
      CALL GMZER(ORIGIN,1,3)
      DO 1 NO=1,NOP
      DO 2 I=1,3
      DO 2 J=1,3
      IF (ABS(TSYM(I,J,1)+TSYM(I,J,NO)).GT..0001) GO TO 1
    2 CONTINUE
      IF (ABS(TTRANS(1,NO))+ABS(TTRANS(2,NO))+ABS(TTRANS(3,NO))
     &  .EQ. 0.) GO TO 4
C
C CENTRE OF SYMMETRY FOUND NOT AT ORIGIN:
      CALL GMEQ(TTRANS(1,NO),ORIGIN,1,3)
      CALL GMSCA(ORIGIN,ORIGIN,0.5,1,3)
C FOR NOW, JUST KEEP TRANSLATION VECTOR ASSOCIATED WITH -X,-Y,-Z
      WRITE (LPT,2001) ORIGIN
2001  FORMAT (/' Centre at:',3F10.4,' Fcs complex')
      GO TO 3
C
C  CENTRE OF SYMMETRY FOUND AT ORIGIN:
   4  NCENT=2
      NC=NO
      GO TO 3
   1  CONTINUE
   3  WRITE (LPT,2000) WORD(NCENT),NOP
2000  FORMAT (/' ',A5,'entrosymmetric space group with',I3,
     & ' operator(s)')
      NOPC=NOP/NCENT
      CENTRC=NCENT.EQ.2
C%
C      CALL ERRCHK(1,NOPC,%SYMO%,0,'symmetry operators')
      CALL ERRCHK(1,NOPC,24,0,'symmetry operators')
 100  RETURN
      END
C
C
C

C
C
C LEVEL 6      SUBROUTINE SYMFRI
      SUBROUTINE SYMFRI
C
C *** SYMFRI updated  by JCM 14 Jul 86 ***
C
CX
CC 1A
CH Reads and interprets an item "FRIE" on an I card.
C
CP Called at the end of the specialist routine SYMTID, which is used to
CP tidy the symmetry operators towards the end of their input.
C
CD Finds the item "FRIE" if it is present, and reads a following integer.
CD Sets the LOGICAL FRIEDL to be TRUE if Friedel's law is to be assumed,
CD that h,k,l is equivalent to -h,-k-l.
Cd
CI The item "I FRIE n" has n=0 for "do NOT assume Friedel's law".  The
CI default  is to assume the law.
CO Reports its findings on unit LPT.
CN used for non-centrosymmetric structures only.
C
      LOGICAL ONCARD
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C
      FRIEDL=.TRUE.
      IF (CENTRC) GO TO 100
C
C IF NON-CENTROSYMMETRIC, LOOK FOR I FRIE CARD:
      IF (ONCARD('I','FRIE',A)) THEN
C
C AND IF I FRIE CARD GIVEN, TAKE INSTRUCTION FROM IT:
        FRIEDL=(A .NE. 0.)
        IF (FRIEDL) THEN
          CALL MESS(LPT,1,'Friedel''s law assumed - hkl equivalent '//
     &    'to -h-k-l')
          GO TO 100
        ENDIF
      ENDIF
C
      FRIEDL=.FALSE.
      CALL MESS(LPT,1,'Friedel''s law NOT assumed - hkl distinct '//
     & 'from -h-k-l')
 100  RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE SYMGEN
      SUBROUTINE SYMGEN
C
C *** SYMGEN by PJB/JCM 8 Jul 83 ***
C
CX
CC 1A
CH Produces the generators of a space group which has been read by SYMOP.
C
CP The entire group must be in the COMMON /SCRAT/, with TSYM holding
CP the rotation matrices and TRANS the corresponding translation vectors.
CP /SCRAT/ also holds the temporary multiplication table for operators,
CP MULTAB.
C
CD SYMGEN has 4 main sections.  The first has been separated off as routine
CD SYMCEN, and the last as SYMTID.
CD
CD SYMCEN identifies a centre of symmetry at the origin if there is one.
CD
CD Part 2 fills in the vector NNORD as a temporary list of the orders
CD of each of the operators.
CD Various elements of NNORD are set to zero:
CD       firstly, all those centrosymetrically related to others,
CD       next, for those of orders 3,4 or 6, zeros are inserted for
CD       the element cubed, to the power 4, etc.
CD Part 3 sets up the pointer vector ITAB, 1xNOPC sized, and JTAB
CD 1xNOP sized.  It makes a list of the generators of the group in IGEN,
CD counting them in NGEN.  A new numbering of operators in made in JTAB,
CD and used to index ITAB.  NUMOP counts up as elements of the group are
CD generated, stopping at NOPC.
C
CD SYMTID tidies the information out of the temporary arrays in /SCRAT/
CD into /SYMDA/ and /SYMTAB/, and sets up a table of pointers to
CD invers elements, INVERS.
C
      LOGICAL FIRST,BINDIG
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/TSYM(3,3,%SY*2%),TTRANS(3,%SY*2%),
      COMMON /SCRAT/TSYM(3,3,48),TTRANS(3,48),
C%
C     & MLTAB(%SY*2%,%SY*2%),ITAB(%SYMO%),JTAB(%SY*2%),
     & MLTAB(48,48),ITAB(24),JTAB(48),
C%
C     & NNORD(%SY*2%),D(3,3)
     & NNORD(48),D(3,3)
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C
C FIRST LOOK FOR CENTRE OF SYMMETRY:
      CALL SYMCEN(NC)
C
C CLEAR TABLE OF ORDERS OF ELEMENTS:
      DO 14 N=1,NOP
  14  NNORD(N)=1
C
C DISCOVER ORDERS & FILL IT TABLE, REJECTING CENTROSYMMETRICALLY
C RELATED OPERATORS, AND REJECTING HIGHER POWERS OF THOSE OF ORDER
C 3, 4 OR 6:
      NGEN=1
      IGEN(1)=1
      IF (NOP .EQ. 1) GO TO 18
C
      DO 1 NO=2,NOP
      IF (NNORD(NO).EQ.0.OR. NNORD(NO).GT.10) GO TO 1
C N WILL GIVE THE NUMBER OF THE LATEST GENERATED OPERATOR:
      N=1
C M WILL GIVE THE ORDER:
      M=1
   16 N=MLTAB(NO,N)
C  REJECT OPERATORS WHICH GENERATE A CENTRE:
      IF (N.EQ.NC) GO TO 1
C
C IF UNIT OPERATOR (NUMBER 1) HAS BEEN GENERATED, WE NOW HAVE THE
C ORDER OF OPERATOR NUMBER "NO":
      IF (N.EQ.1) GO TO 15
      M=M+1
      GO TO 16
C
C FILL IT ORDERS VECTOR;  CANCEL C-RELATED IF THERE IS ONE:
   15 NNORD(NO)=M
      IF (CENTRC) NNORD(MLTAB(NC,NO))=0
      IF (M.LE.2) GO TO 1
C HOLD ORDER FOR HIGHER POWERS OF THOSE OF ORDER GREATER THAN 2
C WITH 100 ADDED
      N=NO
      DO 17 J=3,M
      N=MLTAB(NO,N)
      IF (CENTRC) NNORD(MLTAB(NC,N))=0
      JJ=J-1
      IF (2*JJ .GT.M) JJ=M-JJ
      NNORD(N)=M
      IF (MOD(M,JJ).EQ.0) NNORD(N)=M/JJ
   17 NNORD(N)=NNORD(N)+100
    1 CONTINUE
C
C PART 3:
C NOW FILL IN VECTORS ITAB AND JTAB TO GIVE THE CROSS-REFERENCES BETWEEN
C OLD AND NEW NUMBERINGS OF THE OPERATORS:
      NGEN=0
      IGEN(1)=0
  18  NUMOP=1
      ITAB(1)=1
      JTAB(1)=1
C HERE MAY BE TRICLINIC: IF NOP=1 WE HAVE P1, AND WANT 1 GENERATOR BEING
C THE IDENTITY OPERATOR.  IF NOP=2 WE HAVE P-1, AND (BECAUSE OF THE
C WAY WE ARE STORING OPERATORS) WANT 0 GENERATORS.
      IF (NOPC .EQ. 1) GO TO 101
C
C HERE AT LEAST MONOCLINIC; CLEAR REMAINING ITEMS IN VECTORS:
  13  IF (NOP .GT. 1) CALL JGMZER(JTAB(2),1,NOP-1)
      IF (NOPC .GT. 1) CALL JGMZER(ITAB(2),1,NOPC-1)
C
C NOW COUNT DOWN THROUGH ORDERS OF POSSIBLE SYMMETRY ELEMENTS:
      DO 2 NN=1,5
      IORD=7-NN
C  OPERATORS OF ORDER 5 NOT ALLOWED
      IF (IORD.EQ.5) GO TO 2
      FIRST =.TRUE.
      DO 3 NNN=1,NOP
      IF (NNORD(NNN).NE.IORD) GO TO 3
C  WE HAVE FOUND AN OPERATOR OF ORDER IORD
C  HAS THIS OPERATOR BEEN USED ALREADY?
      IF (JTAB(NNN).NE.0) GO TO 3
C AND IS IT THE FIRST?
      IF (.NOT. FIRST) GO TO 4
C  THE FIRST OPERATOR OF THIS ORDER
      FIRST=.FALSE.
      M=NNN
      GO TO 9
C
C  HERE IF NOT FIRST:
    4 DO 30 M=1,NOP
      IF (MLTAB(ITAB(IGEN(NGEN)),M).EQ.NNN) GO TO 31
   30 CONTINUE
      WRITE (LPT,3000)NNN,NGEN
      WRITE (ITO,3000)NNN,NGEN
3000  FORMAT (' ERROR ** in SYMGEN - operator',I4,' not found',
     & ' in table MLTAB;  no. of generators so far -',I4)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C
   31 IF (NNORD(M).EQ.0 .OR. NNORD(M).GT.10) GO TO 3
      IF (JTAB(M).NE.0) GO TO 3
C  MAKE IT A GENERATOR
    9 NGEN=NGEN+1
      NUMOP=NUMOP+1
      IGEN(NGEN)=NUMOP
      ITAB(NUMOP)=M
      JTAB(M)=NUMOP
      IF (NUMOP.GE.NOPC) GO TO 101
      NUM=NUMOP-1
C  REMAKE MULTIPLICATION TABLE SAVING NEW ORDER
    7 NUM=NUM+1
      J=ITAB(NUM)
      DO 6 N=1,NUM
      I=ITAB(N)
      MUL=MLTAB(I,J)
C IPERM ENSURES OPERATORS ARE COMBINED BOTH WAYS ROUND:
      DO 8 IPERM=1,2
      IF (JTAB(MUL).NE.0) GO TO 8
      NUMOP=NUMOP+1
      ITAB(NUMOP)=MUL
      JTAB(MUL)=NUMOP
      IF (NUMOP.GE.NOPC) GO TO 101
    8 MUL=MLTAB(J,I)
    6 CONTINUE
      IF (NUMOP.GT.NUM) GO TO 7
C
    3 CONTINUE
C
    2 CONTINUE
C
C SOME DIAGNOSTIC OUTPUT IN CASE OF TROUBLE:
      IF (BINDIG(IOUT,16)) THEN
        WRITE (LPT,4000) (NNORD(I),I=1,NOP)
        WRITE (ITO,4000) (NNORD(I),I=1,NOP)
4000    FORMAT (/' NNORD:',(16I5))
        WRITE (LPT,4001) (JTAB(I),I=1,NOP)
        WRITE (ITO,4001) (JTAB(I),I=1,NOP)
4001    FORMAT (/' JTAB:',(16I5))
        WRITE (LPT,4002) (ITAB(I),I=1,NOPC)
        WRITE (ITO,4002) (ITAB(I),I=1,NOPC)
4002    FORMAT (/'ITAB:',(16I5))
      ENDIF
C
C PART 4 - TIDY UP INFORMATION INTO PERMANENT SPACE:
 101  CALL SYMTID(NC)
      RETURN
      END
C
C
C
C
C LEVEL 9      SUBROUTINE SYMOP
      SUBROUTINE SYMOP
C
C *** SYMOP updated by JCM 30 Aug 92 ***
C
CX
CC 1A
CH Reads all the symmetry operators or a space group specification from
CH "S" cards, and generates the space group.
C
CD Reads all "S" cards and stores (ultimately):
CD      the space group rotation matrices in SYM in /SYMDA
CD      the translation vectors associated with them in TRANS in /SYMDA
CD      the lattice translation in ALAT in /SYMDA
CD      NOP=total number of operators, in /NSYM
CD      NOPC= the number of operators actually stored (half NOP if
CD            centrosymmetric with origin at centre), in /NSYM
CD      NLAT= the number of lattice vectors, in /NSYM
CD      NCENT=1 if non-centrosymmetric, 2 if centrosymmetric, in /NSYM
CD      NGEN= the number of generators of the space group, in /NSYM
C
CD Only the generating elements of the space group need be given.
CD The position X,Y,Z is assumed, and non-primitive lattice translations
CD are taken care of.
CD
CD Operators related by a centre are eliminated.
C
CI Reads all the "S" cards from the copy of the Crystal Data File held
CI on unit IO10.
C
CN The complete group of operators formed may be printed out using OPSYM.
C
      LOGICAL ONCARD,SGIVEN
      CHARACTER *16 SNAME
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/TSYM(3,3,%SY*2%),TTRANS(3,%SY*2%),
      COMMON /SCRAT/TSYM(3,3,48),TTRANS(3,48),
C%
C     & MLTAB(%SY*2%,%SY*2%),R(3,3),T(3)
     & MLTAB(48,48),R(3,3),T(3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C INPUT NUMBER OF S CARDS:
      NNSYM=ICDNO(19)
C
C SET "S CARDS READ":
      INREAD(19)=-IABS(INREAD(19))
C
C INITIALISE AS THOUGH P1:
      NLAT = 1
      NOP = 1
      CALL GMZER(ALAT(1,1),1,3)
      CALL GMZER(TTRANS(1,1),1,3)
      CALL GMUNI(TSYM(1,1,1),3)
C ALAT HOLDS (UP TO 4) LATTICE TRANSLATION VECTORS
C TTRANS HOLDS (IN THIS FIRST STAGE) THE TRANSLATION VECTORS OF EACH OPERATOR
C TSYM HOLDS (IN THIS FIRST STAGE) THE ROTATION MATRICES OF EACH OPERATOR
C
      IF (NNSYM .EQ. 0) CALL MESS(LPT,1,
     & 'No cards starting S have been read - assuming space group P1')
      ID=IABS(INREAD(19))
C
C FIRST, IS THERE AN S GRUP CARD
      SGIVEN=(ONCARD('S','GRUP',A))
      IF (SGIVEN) THEN
        CALL SPGNAM(NINT(A),SNAME)
        NSPC=NOPFIL(5)
        CALL SPACE(SNAME,NSPC,NGENS)
        NNSYM=NGENS
        ID=-NSPC
      ENDIF
C
C READ AND INTERPRET S CARDS ONE BY ONE:
      DO 4 NCARD = 1,NNSYM
      CALL INPUTS(ID,R,T)
      IF (.NOT. SGIVEN) ID=ID+NYZ
      NO=NOP
C DISCOVER WHETHER R,T IS A NEW OPERATOR:
      CALL EQOP(R,T,NO,NLAT)
      IF (NO .LE. NOP) GO TO 4
C
C IF HERE, NEW OPERATOR:
   5  NOP=NOP+1
      DO 6 N=2,NOP
      N1 =NOP
      N2 = N
C USE IPERM TO TRY MULTIPLICATION BOTH WAYS ROUND:
      DO 8 IPERM=1,2
      CALL GMPRD (TSYM(1,1,N1),TTRANS(1,N2),T,3,3,1)
      CALL GMPRD (TSYM(1,1,N1),TSYM(1,1,N2),R,3,3,3)
      CALL GMADD(T,TTRANS(1,N1),T,1,3)
      CALL FRAC3(T)
      NP=NO
      CALL EQOP(R,T,NP,NLAT)
C FILL IN MULTIPLICATION TABLE - THIS IS WHY WE NEED TO KNOW WHICH OPERATOR
C IT WAS IF THERE IS A MATCH IN EQOP:
      MLTAB(N1,N2)=NP
      IF (NP.GT.NO) NO=NP
      IF (N1 .EQ. N2) GO TO 6
      N1 = N
      N2 = NOP
   8  CONTINUE
   6  CONTINUE
C THE NEXT TEST REPEATS THE PROCESS ON ANY NEWLY CREATED
C OPERATORS - WE CANNOT ALTER NOP AT THE END OF A DO LOOP:
      IF (NO-NOP) 4,4,5
    4 CONTINUE
C
C HERE WHEN ALL CARDS READ:
   3  DO 7 N=1,NOP
      MLTAB(1,N)=N
   7  MLTAB(N,1)=N
      IF (NLAT .LT. 2) GO TO  102
C
C     FORM COMPLETE TRANSLATION GROUP
      DO 12 N=2,NOP
      M1=NLAT
      CALL GMPRD(TSYM(1,1,N),ALAT(1,2),T,3,3,1)
      CALL FRAC3(T)
      CALL EQPOS(ALAT,T,M1,M2,4)
      IF (M2 .GT. NLAT) NLAT=M2
  12  CONTINUE
      J = 2
      I = 2
   1  M1=NLAT
      CALL GMADD(ALAT(1,I),ALAT(1,J),T,1,3)
      CALL FRAC3(T)
      CALL EQPOS(ALAT,T,M1,M2,4)
      IF (M2 .GT. NLAT) NLAT=M2
      J=J+1
      IF (J .LE. NLAT) GO TO 1
      I=I+1
      J=I
      IF (I .LE. NLAT) GO TO 1
C
C  COMPLETE GROUP FORMED - NOW FIND GENERATORS
 102  IF (IERR .NE. 0) CALL ERRMES(1,0,'on S cards')
C
      CALL SYMGEN
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE SYMREF(HIN,HOUT,NREFS,PHASE)
      SUBROUTINE SYMREF(HIN,HOUT,NREFS,PHASE)
C
C *** SYMREF corrected by PJB (davinda) 22-Apr-1994 ***
C
CX
CC 1B
CH Generates a set of equivalent reflections and related phases.
C
CP The space group symmetry must be set up by a call of SYMOP
C
CA On entry HIN holds an array of 3 reals, h,k,l
CA On exit  HOUT is a 2-D real array of sets of related h,k,l values.
CA          NREFS is set to the number of generated sets (including h,k,l)
CA          PHASE holds an array of scalar products, hx+ky+lz, with h,k,l
CA                corresponding to the array HIN, and x,y,z being the
CA                translation vector corresponding to the symmetry operator
CA                which produced it (in fact, the negated vector for the
CA                inverse operator).
C
C%
C      DIMENSION HIN(3),HOUT(3,%SY*2%),EH(3),PHASE(%SY*2%),TR(3)
      DIMENSION HIN(3),HOUT(3,48),EH(3),PHASE(48),TR(3)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      NREFS=0
      DO 2 J=1,NCENT
      DO 1 I=1,NOPC
      CALL ROTSYM(HIN,EH,I,2)
      IF (J .EQ. 2) CALL GMREV(EH,EH,3,1)
      CALL EQVEC(HOUT,EH,NREFS,M,NOP)
      IF (M .GT. NREFS) THEN
        NREFS=M
        CALL GMREV(TRANS(1,INVERS(I)),TR,3,1)
* I suppose this is right?
        IF (J .EQ. 2) CALL GMREV(TR,TR,3,1)
        PHASE(M)=SCALPR(EH,TR)
      ENDIF
   1  CONTINUE
   2  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE SYMTID(NC)
      SUBROUTINE SYMTID(NC)
C
C *** SYMTID by JCM 11 Jul 83 ***
C
CX
CC 1A
CH Tidies all the arrays connected with the space group symmetry.
CP A specialist routine called at the end of routine SYMGEN.
C
CD Copies out of the scratch COMMON /SCRAT/:
CD     rotation matrices TSYM to SYM in /SYMDA/
CD     translation vectors TTRANS to TRANS in /SYMDA/
CD     orders of elements NNORD into NORD in /SYMTAB/
CD     multiplication table from MLTAB to MULTAB in /SYMTAB/
CD
CD An inverting element is indicated by a -ve entry in NORD.
CD
CD Also sets up the constraints imposed on the cell parameters In
CD both spaces) by the symmetry.  This is done here in case the user
CD is about to use routines SCLPRD and VCTMOD, which refer to the
CD cell parameter quadratic products.  If RECIP has not yet been obeyed,
CD the actual cell parameters are not available, so sensible guesses
CD (consistent with the symmetry) are inserted.
C
CO Writes to unit LPT a brief report of the generators.
C
      LOGICAL BINDIG
      COMMON /CELFIX/IPTCEL(6),AMCELL(6),NCELF,NCELG,NCELS,KOM3
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/TSYM(3,3,%SY*2%),TTRANS(3,%SY*2%),
      COMMON /SCRAT/TSYM(3,3,48),TTRANS(3,48),
C%
C     & MLTAB(%SY*2%,%SY*2%),ITAB(%SYMO%),JTAB(%SY*2%),
     & MLTAB(48,48),ITAB(24),JTAB(48),
C%
C     & NNORD(%SY*2%),D(3,3)
     & NNORD(48),D(3,3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      CALL MESS(LPT,1,'The group is generated by the')
      IF (NGEN .GT. 0) WRITE (LPT,2001) NGEN,(IGEN(I),I=1,NGEN)
 2001 FORMAT ('+',31X,I3,' element(s) numbered:',3I3)
      IF (CENTRC .AND.  NOP .GT. 2) CALL MESS(LPT,0,'and the')
      IF (CENTRC) CALL MESS (LPT,0,'centre of symmetry.')
C
C  STORE DEFINITIVE SYMMETRY OPERATORS  IN NEW SEQUENCE;  ALSO ORDERS OF
C  ELEMENTS :
      DO 3 N1=1,NOPC
      CALL GMEQ(TSYM(1,1,ITAB(N1)),SYM(1,1,N1),3,3)
      CALL GMEQ(TTRANS(1,ITAB(N1)),TRANS(1,N1),1,3)
      NORD(N1)=NNORD(ITAB(N1))
      IF ((NORD(N1).EQ.0) .AND. (NC .GT. 0)) NORD(N1)=NNORD(MLTAB
     & (ITAB(N1),NC))
C  MAKE THE ORDERS OF INVERTING ELEMENTS NEGATIVE:
      IF (DETER3(SYM(1,1,N1)) .LT.0.) NORD(N1)=-NORD(N1)
C
C STORE MULTIPLICATION TABLE AND INVERSES:
    1 DO 2 N2=1,NOPC
      MULTAB(N2,N1)=JTAB(MLTAB(ITAB(N2),ITAB(N1)))
      IF (MULTAB(N2,N1) .EQ. 1) INVERS(N1)=N2
   2  CONTINUE
   3  CONTINUE
C
C DIAGNOSTIC OUTPUT IN CASE OF TROUBLE:
      IF (BINDIG(IOUT,16)) THEN
        DO 88 N1=1,NOPC
        WRITE (LPT,4000) (MULTAB(N2,N1),N2=1,NOPC)
        WRITE (ITO,4000) (MULTAB(N2,N1),N2=1,NOPC)
4000    FORMAT (/' MULTAB:',(16I5))
  88    CONTINUE
        WRITE (LPT,4001) (INVERS(I),I=1,NOPC)
        WRITE (ITO,4001) (INVERS(I),I=1,NOPC)
4001    FORMAT (/' INVERS:',(16I5))
        WRITE (LPT,4002) (NORD(I),I=1,NOPC)
        WRITE (ITO,4002) (NORD(I),I=1,NOPC)
4002    FORMAT (/' NORD:',(16I5))
      ENDIF
C
C
C SET UP CONSTRAINTS ON CELL PARAMETERS:
C CLEAR CHAIN INFORMATION FOR ALL CELL PARAMETERS
      IF (INREAD(3) .LT. 0) GO TO 101
      DO 6 I=1,6
   6  IPTCEL(I)=9999
C
C CYCLE OVER ALL SYMMETRY LOOKING FOR RELATIONS
      IF (NOPC .EQ. 1) GO TO 7
      DO 8 NO=1,NGEN
      CALL RELSM6(SYM(1,1,IGEN(NO)),IPTCEL,AMCELL)
   8  CONTINUE
C
C PUT SUITABLE NUMBERS IN CPARS TO KEEP SCLPRD AND VCTMOD HAPPY EVEN IF RECIP
C NOT OBEYED BEFORE SYMUNI:
   7  DO 4 I=1,2
      DO 5 J=1,3
      CPARS(J,I)=1.
      CPARS(J+3,I)=0.
      IF (IPTCEL(J+3) .NE. 0 .AND. IPTCEL(J+3) .LT.4)CPARS(J+3,I)=
     & 0.5*FLOAT(2*I-3)
   5  CONTINUE
   4  CONTINUE
C
C READ I FRIE CARD IF RELEVANT, SETTING LOGICAL FRIEDL FOR "FRIEDEL TO HOLD":
 101  CALL SYMFRI
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE SYMUNI
      SUBROUTINE SYMUNI
C
C *** SYMUNI updated by PJB 14 Jun 88 ***
C
CX
CC 1A
CH Selects a reciprocal space asymmetric unit fitting the symmetry.
CP SYMOP should have been obeyed, leaving NORD holding the orders of the
CP       symmetry operators, INVERS pointing at their inverse elements,
CP       FRIEDL set and the operators in SYM and TRANS.
CD Sets up in /FUNIT:
CD         NASYM planes bounding an asymmetric unit in ASYM
CD         EDGE to hold edges
CD         ANG to hold angles
CD         NMUL to hold fraction of reciprocal space used
CD By a call of UNITID makes the found unit hold the required typical h,k,l
CD whose default is 13,11,10 (i.e. all positive, h > k > l)
CD
CD By a call of POLUNI fills in /GUNIT with indicators used in finding the
CD multiplicity of a reflection.
CD
CD Builds up tentative unit in COMMON /SCRAT in ASY, communicating with other
CD routines like FIXUNI via /SCRAT.
CD
CD Works from a list of mirror planes in /SCRAT;  a unit may not contain A
CD mirror.  Notes also if point group is cubic, and takes those separately.
CD
CD Uses a list of axes of symmetry elements in AXI in /SCRAT.
CD If not cubic, and mirror planes are not enough to form a unit of the correct
CD size, uses such axes to form the remaining planes.  If there is only 1 axis,
CD takes an arbitrary plane containing it, and symmetry related planes.
CD
CD If there are more than one, the first is "principal" and must either be
CD perpendicular to a unit plane, or contained in it (all planes).  Planes
CD are taken through this axis and those of other elements in turn.
CD
CD Point group are dealt with as follows:
CD
CD Triclinic: 1 and -1 are singled out; -1 is given an arbitrary plane.
CD Cubic: 3 planes are needed, each containing two axes.  If no 4 axis,we have:
CD    either  23 for which we need three 3 axes (distinguished by NMUL=12)
CD    or      m3 for which we need one 3 and two 2's
CD
CD    If a 4 is present, then 4 3 3 will do for 432 and -43m,
CD           but m3m needs 4 3 2 (and is detected by having NMUL=48).
CD
CD Mirrors: use of mirrors as planes will finish:
CD       monoclinic: m
CD       orthorhombic: mm2 and mmm
CD       tetragonal: 4mm and 4/mmm
CD       trigonal 3m
CD       hexagonal: 6mm, -6m2 and 6/mmm
CD
CD Single axis: a number of planes through a single axis will finish:
CD       monoclinic: 2 and 2/m
CD       orthorhombic: no more
CD       tetragonal: 4, -4 and 4/m (which has also 1 mirror)
CD       trigonal: 3 and -3
CD       hexagonal:  6, -6, 6/m (the last two having 1 mirror also)
CD
CD Principal axis: planes through principal axis and a "2" axis finish:
CD       orthorhombic: 222
CD       tetragonal: 422 and -42m (which has 1 mirror also)
CD       trigonal: 32
CD       hexagonal: 622
CD
CD This leaves -3m, which belongs to the "principal axis" category above, but
CD because we have removed the centrosymmetrically related operators has no
CD explicit "2" axes left.  There are two ways of dealing with this - a plane
CD perpendicular to the principal axis, or a halving of the angle between
CD existing planes.  For the moment the former is taken.
CD
CO If the procedure outlined above does not produce a sensible outcome, writes
CO error mesages and stops.
C
CN This highlights a general point.  For most point groups it is evident how
CN many planes must be used to slice space into suitable units, but for a
CN subset of them (422, -42m, -3, 32, -3m, 622 : for laymen, those which:
CN          are non-cubic
CN          have at least 6 spots
CN          have both black and white spots
CN          but not superimposed
CN the space may be carved into two different sets of units, one with 3 faces
CN and one with 2.  The above methods will generally give preference to 2,
CN except in the case of -3m as described.
CN
CN The user may not get exactly the unit he wants for this reason.  He is
CN allowed to specify typical indices of a reflection to be inside the finished
CN unit, but this will not influence the carving in the cases mentioned.  I
CN have in mind to allow him also to offer a plane, so that, e.g., in
CN 422 if he offers the equator plane, he will get the broader wedge of space
CN cut off by that plane, instead of the narrower wedge "infinite" in both
CN directions.
C
      DIMENSION XAX(3),ZAX(3)
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/AXI(3,%SYMO%,2),MIRROR(%SYMO%),D(3,3),PL1(3),PL2(3),PL3(3),
      COMMON /SCRAT/AXI(3,24,2),MIRROR(24),D(3,3),PL1(3),PL2(3),PL3(3),
     & HT(3),ASY(3,4),NSTAT(4),NOPL,NICE,VOL,MOP1,MOP2
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
      DATA XAX,ZAX/1.0,0.,0.,0.,0.,1./
C
C READ IN POSSIBLE U CARD GIVING 3 TYPICAL INDICES REQUIRED BY USER TO
C BE WITHIN THE FINISHED ASYMMETRIC UNIT.  IF NO U CARD USE 13,11,10:
      CALL INPUTU(HT)
C
C CLEAR SPACE FOR FIXUNI TO BUILD UP NOPL PLANES IN ASY WITH STATUS NSTAT:
      NOPL=0
      CALL GMZER(ASY(1,1),3,4)
      CALL JGMZER(NSTAT,1,4)
C
C NMUL=FRACTION OF UNIT INVOLVED AND MUST TAKE ACCOUNT OF FRIEDEL:
      NMUL=NOP
      IF (FRIEDL) NMUL=NOPC*2
C
C JUMP IF POINT GROUP 1, NO PLANES AT ALL REQUIRED:
      IF (NMUL.EQ.1) GO TO 102
C JUMP IF NOT POINT GROUP -1:
      IF (NOPC .GT. 1) GO TO 2
C SPECIAL FOR P-1 OR P1 WITH FRIEDEL - PUT IN 0 0 1 AND ACCEPT:
      CALL FIXUNI(ZAX,1)
      GO TO 102
C
C HERE FOR MONOCLINIC ONWARDS:
C SET UP AXES OF SYMMETRY ELEMENTS, AND MARK MIRROR PLANES IF THEY EXIST:
C NOTE LASTAX=NO. OF LAST ELEMENT OF LIST (IRRELEVANT ELEMENTS ARE ALREADY 0)
C NP2 = FIRST "2", NP3 = FIRST "3" (THERE ARE NO -3) AND NP4 = FIRST "4"
C AND COUNT "3" AXES IN N3, BEAUSE CUBIC HAS 4 OF THESE AND NOTHING ELSE HAS >1
C ALSO, NPP2 IS SET TO POINT TO THE FIRST +2 (NOT -2) FOR THE SPECIAL CASE -3M.
C
   2  LASTAX=0
      N3=0
      NP2=0
      NPP2=0
      NP3=0
      NP4=0
      DO 3 N=2,NOPC
      MIRROR(N)=0
      IF (IABS(NORD(N)).GT.100) THEN
        CALL GMZER(AXI(1,N,1),3,1)
        CALL GMZER(AXI(1,N,2),3,1)
        GO TO 3
      ENDIF
C  GET THE AXIS IN REAL SPACE
      CALL AXIS(SYM(1,1,N),AXI(1,N,1))
C  GET THE AXIS IN RECIPROCAL COORDINATES
      CALL GMEQ(SYM(1,1,INVERS(N)),D,3,3)
      CALL TRANSQ(D,3)
      CALL AXIS(D,AXI(1,N,2))
C
C PICK OUT USEFUL AXES:
      LASTAX=N
      IF (NORD(N) .EQ. 2) NPP2=N
      I=IABS(NORD(N))
      IF (I .EQ. 3) N3=N3+1
      IF (I .EQ. 2 .AND. NP2 .EQ. 0) NP2=N
      IF (I .EQ. 3 .AND. NP3 .EQ. 0) NP3=N
      IF (I .EQ. 4 .AND. NP4 .EQ. 0) NP4=N
C
C MARK MIRRORS:
      IF (NORD(N).EQ.-6 .OR. NORD(N).EQ.-2) GO TO 4
      IF (.NOT.FRIEDL) GO TO 3
      IF (NORD(N).EQ.3) GO TO 3
   4  MIRROR(N)=1
   3  CONTINUE
C
C IF PRINCIPAL AXIS IS A "2" MOVE NP2 PAST IT:
      IF (NP2 .EQ. 2) NP2=3
C
C JUMP IF NOT CUBIC:
      IF (N3 .NE. 4) GO TO 5
C JUMP IF NO "4" AXIS:
      IF (NP4 .EQ. 0) GO TO 6
C
C PICK PLANE THROUGH 4 AND ANY 3:
      CALL VECPRD(AXI(1,NP4,2),AXI(1,NP3,2),PL1)
C AND INSIST ON KEEPING IT:
      CALL FIXUNI(PL1,2)
C NOW DISTINGUISH BETWEEN M3M AND THE REST;  4 3 3 FOR M3M IS NOT SMALL ENOUGH:
      NSTART=NP3+1
      NS=3
      IF (NMUL .LT. 48) GO TO 19
      NSTART=NP2
      NS=2
C SCAN REMAINING OPERATORS LOOKING ONLY AT SUITABLE AXES:
  19  DO 7 J=NSTART,LASTAX
      IF (IABS(NORD(J)) .NE. NS) GO TO 7
C MAKE 2 MORE PLANES, EACH THROUGH NEW AXIS AND ONE OF ORIGINAL:
      CALL VECPRD(AXI(1,J,2),AXI(1,NP4,2),PL1)
      CALL VECPRD(AXI(1,NP3,2),AXI(1,J,2),PL2)
      CALL FIXUNI(PL1,1)
      CALL FIXUNI(PL2,1)
      IF (NICE) 8,102,8
C IF NICE IS EVER 0 WE ARE HOME - OTHERWISE TAKE NEXT 3RD AXIS:
   8  CALL FIXUNI(PL1,-2)
      CALL FIXUNI(PL2,-3)
   7  CONTINUE
C
C  SHOULD NOT GET HERE:
      GO TO 11
C
C CUBIC WITH NO 4 AXIS - IS IT 23 OR 3M?:
   6  IF (NMUL .NE. 12) GO TO 20
C 23 - UNIT MUST HAVE THREE 3 AXES AS EDGES, ANY ANY 3 SHOULD DO:
      I=NP3
      J=I
  21  J=J+1
      IF (J .GT. LASTAX) GO TO 11
      IF (NORD(J) .NE. 3) GO TO 21
      K=J
  22  K=K+1
      IF (K .GT. LASTAX) GO TO 11
      IF (NORD(K) .NE. 3) GO TO 22
      CALL PLN3AD(I,J,K)
      IF (NICE) 11,102,11
C
C M3 - WANT ONE 3 AXIS AND TWO 2S:
  20  DO 9 J=NP2,LASTAX
C PICK OUT PAIRS OF 2 AXES TO PUT WITH 3:
      IF (NORD(J) .NE. 2) GO TO 9
      IF (J .EQ. LASTAX) GO TO 11
      J1=J+1
      DO 39 K=J1,LASTAX
      IF (NORD(K) .NE. 2) GO TO 39
      CALL PLN3AD(NP3,J,K)
      IF (NICE) 10,102,10
C
C IF DO NOT MAKE GOOD CELL, TAKE ALL 3 OUT AGAIN:
  10  CALL FIXUNI(PL1,-1)
      CALL FIXUNI(PL2,-2)
      CALL FIXUNI(PL3,-3)
  39  CONTINUE
   9  CONTINUE
C
C SHOULD HAVE MADE GOOD UNIT BY NOW:
  11  WRITE (LPT,3000)NP2,NP3,NP4
      WRITE (ITO,3000)NP2,NP3,NP4
3000  FORMAT (/' *** PROGRAM ERROR in SYMUNI - all cubic axes scanned',
     & ' - NP2, NP3, NP4=',3I3)
C>>JCC HAndle through extra function
C Was      STOP
C Now
      CALL BMBOUT
      RETURN
C
C
C NEITHER TRICLINIC NOR CUBIC - TRY ALL MIRRORS:
   5  DO 12 N=2,LASTAX
      IF (MIRROR(N).EQ.0) GO TO 12
      CALL FIXUNI(AXI(1,N,1),1)
      IF (NICE) 12,102,12
   12 CONTINUE
C
C MIRRORS INADEQUATE - PICK OUT THOSE WITH ONLY 1 SYMMETRY ELEMENT (OTHER THAN
C THE UNIT, A CENTRE, OR THOSE GENERATED FROM THE ONE):
      IF (LASTAX .NE. 2) GO TO 13
C THIS ASSUMES THAT SUCH AN AXIS IS ELEMENT 2 - SO LONG AS WE HAVE COME IN VIA
C SYMOP, SYMGEN IT WILL BE:
C
C TAKE PLANE THROUGH AXIS AND X AXIS (OR Z AXIS):
  18  CALL VECPRD(AXI(1,2,2),XAX,PL1)
      IF (VCTMOD(1.0,PL1,1).LT.0.0001) CALL VECPRD(AXI(1,2,2),ZAX,PL1)
C MAKE THIS FIRST PLANE MANDATORY:
      CALL FIXUNI(PL1,2)
      IF (NICE) 15,102,15
C NEED ANOTHER PLANE - SWING IT:
  15  DO 14 I=2,NOPC
      CALL ROTSYM(PL1,PL2,I,1)
      CALL FIXUNI(PL2,1)
      IF (NICE) 14,102,14
  14  CONTINUE
C
C SHOULD BE ENOUGH:
      CALL ERRMES(-1,0,'in SYMUNI - single axis not enough')
C
C HAVE PRINCIPAL AXES PLUS OTHERS - ARE THERE ANY 2'S?
  13  IF (NPP2 .EQ. 0) GO TO 16
C MAKE PLANES THROUGH PRINCIPAL AND A 2 AND OFFER IN TURN:
      DO 17 J=NP2,LASTAX
      IF (NORD(J) .NE. 2) GO TO 17
      CALL VECPRD(AXI(1,2,2),AXI(1,J,2),PL1)
      CALL FIXUNI(PL1,1)
      IF (NICE) 17,102,17
  17  CONTINUE
C
C THIS CONTINUAL OFFERING DOES NOT REMOVE THE PLANES IT DOES NOT LIKE, AND
C MAY CAUSE TROUBLE.
C
C SHOULD HAVE BEEN ENOUGH - IF NOT ONLY CASE  SHOULD BE CENTRE NOT AT ORIGIN:
      IF (.NOT. CENTRC) GO TO 18
      CALL ERRMES(-1,0,
     & 'in SYMUNI - principal axis plus 2 axes not enough')
C
C WE SHOULD BE AT -3M HERE, WITH 2 MIRRORS, BUT UNIT TOO BIG.  HALVE IT:
  16  CALL FIXUNI(AXI(1,2,1),1)
      IF (NICE .NE. 0) CALL ERRMES(-1,0,'reached end of SYMUNI')
C
C SUCCESSFUL UNIT - TIDY IT AND TRANSFORM TO HOLD TYPICAL REFLECTION:
 102  CALL UNITID
C
C FINAL UNIT - MARK EDGES FOR MULTIPLICITY:
      CALL POLUNI
C
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE TBLFND(NAME,IANS,IFAM,IGEN,ISPC,KP,KS)
      SUBROUTINE TBLFND(NAME,IANS,IFAM,IGEN,ISPC,KP,KS)
C
C *** TBLFND updated by JCM 8 May 90 ***
C
CX
CC 6C
CH Looks for an A4 NAME in every table it can find, trying to identify it as
CH part of a LSQ parameter name.
CA On entry NAME is the target name to find
CA On exit IANS gives the answer:
CA  IANS=0 means cannot find NAME anywhere
CA  IANS=large -ve means that NAME is not part of a parameter specification,
CA        but some other word recognised on a FIX/VARY card, like:
CA        'ONLY' (-99) or 'ALL' (-100)
CA  IANS=small -ve means that NAME is a word recognised from the user's
CA        table, and IANS has been picked up from the user's parallel table
CA        table of small -ve integers or parameter specs
CA  IANS=+ve means that NAME was part of a parameter specification;  it may
CA        be a family, genus or species name, and from it may have been
CA        deduced other things (like family name from genus name)
CA
CA        In this case, as many of IFAM, IGEN and ISPC as can be set on exit
CA        are set.
C
CP Expects LSQ to have been set up by LSETUP;  if structure parameters are to
CP be LSQ parameters, expects tables of atom names in /ATNAM and form factor
CP names in /FONAM
CP
CP If Multiphase, expects KPHASE to hold phase required for this one word.
C
      CHARACTER *4 NAME,ITB(2)
      DIMENSION ITBSPC(2)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /FORMD2/NBKF(20,9),NMFNM(9)
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      LOGICAL MULFAS,MULSOU,MULONE
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POSNS2/NATO(9)
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      COMMON /WDSPC/IWDNUM,IWDSPC(60)
      COMMON /WORDS/LSQWD(60)
      CHARACTER *4 LSQWD
      DATA ITB/'ALL','ONLY'/
      DATA ITBSPC/-100,-99/
C
C CLEAR ANSWERS:
      IFAM=0
      IGEN=0
      ISPC=0
      KP=0
      KS=0
C
C FIRST TRY TO FIND IN MAIN WORD TABLE WHICH WAS SET UP IN THE MAIN PROGRAM TO
C SPECIFY THE PROBLEM:
      L=NCFIND(NAME,LSQWD,IWDNUM)
      IF (L .GT. 0) THEN
C FOUND IN WORD TABLE - PARALLEL ARRAY IWDSPC SHOULD BE PACKED INFO
C AS TO WHAT THIS WORD ACTUALLY IS:
        IANS=IWDSPC(L)
        IF (IANS .GT. 0) CALL KUNPAK(IANS,IFAM,IGEN,ISPC,KP,KS)
        GO TO 100
      ENDIF
C
C NEXT TRY TBLFND'S OWN TABLE OF USEFUL WORDS APPLICABLE TO MOST LSQ:
      L=NCFIND(NAME,ITB,2)
      IF (L .GT. 0) THEN
        IANS=ITBSPC(L)
        GO TO 100
      ENDIF
C
C TRY DIGITS (SPECIES NAMES INVOLVED IN FAMILY 1, OR 3 ETC. ):
      N=0
      DO 4 J=1,4
C A DIGIT WILL BE LEFT-JUSTIFIED:
      IF (NAME(J:J) .NE. ' ') THEN
        DO 2 I=1,10
        IF (NAME(J:J) .EQ. IDIGIT(I)) GO TO 3
   2    CONTINUE
      ENDIF
      IF (N .EQ. 0) GO TO 5
      GO TO 9
   3  IF (I .EQ. 10) I=0
      N=10*N+I
   4  CONTINUE
   9  ISPC=N
C GO TO PACK INTO IANS
      GO TO 102
C
C OTHER WORDS REFER TO FAMILY 2 OR MORE - CHECK THERE MAY BE SOME:
   5  IF (NFAM .LT. 2) GO TO 101
      IF (MULFAS) THEN
        L=NCFIND(NAME,ATNA(1,KPHASE),NATO(KPHASE))
      ELSE
        L=IATOM(NAME)
      ENDIF
      IF (L .GT. 0) THEN
        IGEN=L
        IFAM=2
        GO TO 102
      ENDIF
C
C MAY BE SCATTERING FACTOR NAME:
      IF (MULFAS) THEN
        ISC=NCFIND(NAME,FONA(1,KPHASE),NMFNM(KPHASE))
      ELSE
       ISC=ISCAT(NAME)
      ENDIF
      IF (ISC .GT. 0) IGEN=NBKF(ISC,KPHASE)
      IF (IGEN .EQ. 0) GO TO 101
      ISPC=10
      IFAM=2
C IFAM, IGEN, ISPC OK - PACK INTO IANS:
 102  IANS=KPAK(IFAM,IGEN,ISPC,KPHASE,0)
      KP=KPHASE
      GO TO 100
C
C WORD CANNOT BE FOUND:
 101  IANS=0
 100  RETURN
      END
C
C
C
C
C LEVEL 1      LOGICAL FUNCTION TESTOV(A,B)
      LOGICAL FUNCTION TESTOV(A,B)
C
C *** TESTOV by JCM 22 Nov 83 ***
C
CX
CC 11C
CH Tests a floating division for potential overflow.
CA On entry A is the real numerator
CA          B is the real denominator
CA on exit  TESTOV is set .TRUE. if A/B would overflow, .FALSE. if not.
C
      TESTOV = ((A+B) .EQ. A)
      RETURN
      END
C
C
C

C
C
C LEVEL 4      SUBROUTINE TRYUNI(NCHK)
      SUBROUTINE TRYUNI(NCHK)
C
C *** TRYUNI by JCM 25 Sep 84 ***
C
CX
CC 1A
CH A specialist routine used in the setting up of the asymmetric unit, to
CH try out a given set of planes as faces of the unit.
CA On entry NCHK non-zero indicates that a check on the given representative
CA indices HT (see below) is to be performed whatever the given unit
CA is like.
C
CP The scratch COMMON /SCRAT/ is set up as follows:
CP A tentative asymmetric unit is held as NOPL planes in the array ASY,
CP the dimension of 4 allowing for "dead" planes in case we need to
CP revive them.
C
CP The status of each plane is in NSTAT:
CP       0=not there
CP       1=possible
CP       25=mandatory
C
CP HT is a 1x3 array holding the indices of a typical reflection which
CP must occur within (that is, not on the faces or the edges of) the
CP finished asymmetric unit.
C
C
CD TRYUNI does not necessarily produce the correct unit, but it checks
CD that any unit it sends out has 1 and only 1 relative of HT in it.
C
CD A unit which is too small, or which fails the above test, is
CD flagged on exit by NICE=-1.
CD An acceptable unit is falgged by NICE=1.
C
CD NOPL is expected to be 1, 2 or 3.  Copies NOPL planes out of the
CD temporary array ASY to the (eventually) permanent array ASYM in
CD /FUNIT/.
C
CD If there is more than 1 plane, the edge(s) and angle(s) involved are
CD set up, together with AMUL = the proportion of reciprocal space
CD occupied by the given unit.
CD
CD We then have VOL=AMUL*NMUL, and aim at VOL=1.
CD
CD If there are 3 planes forming a hinge, sets VOL=0., and NICE=1, and
CD leaves the configuration for the calling routine (FIXUNI) to deal with.
CD
CD If VOL=1., checks that the unit contains 1 and only 1 relative of
CD the representative indices in HT.
CD
CD If NCHK is non-zero does this check in any case, counting the number
CD of relatives both inside and outside.  This caould be useful if the
CD user is ever allowe to specify his own unit.
C
      DIMENSION NNSTAT(3)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
C%
C      COMMON /SCRAT/AXI(3,%SYMO%,2),MIRROR(%SYMO%),D(3,3),PL1(3),PL2(3),PL3(3),
      COMMON /SCRAT/AXI(3,24,2),MIRROR(24),D(3,3),PL1(3),PL2(3),PL3(3),
     & HT(3),ASY(3,4),NSTAT(4),NOPL,NICE,VOL,MOP1,MOP2
C
      NASYM=NOPL
      IF (NASYM .LE. 0 .OR. NASYM .GE. 4)
     &  CALL ERRIN2(NASYM,0,'in PROGRAM using TRYUNI with NOPL=',' ')
C
   1  I=1
      DO 2 J=1,NASYM
   3  IF (NSTAT(I) .NE. 0) GO TO 4
      I=I+1
      IF (I .LT. 5) GO TO 3
      CALL ERRMES(-1,0,'TRYUNI not given enough planes')
C
   4  CALL GMEQ(ASY(1,I),ASYM(1,J),1,3)
      NNSTAT(I)=NSTAT(J)
      I=I+1
   2  CONTINUE
C
C SET UP PI/3 FOR DETECTION OF ANGLE, NICE INITIALISED TO 'TOO BIG', AND
C CLEAR INDICATOR THAT WE HAVE PI/3 OR 2PI/3 ANGLE:
      PIBY3=PI/3.
      NICE=1
      N3AX=0
C BRANCH ON 1, 2 OR 3 PLANES:
   25  GO TO (7,8,9) , NASYM
C
C SINGLE PLANE - HALF SPACE:
   7  AMUL=0.5
      GO TO 10
C
C TWO PLANES - MAY FIRST NEED TO REVERSE ONE FOR SMALLER ANGLE:
   8  ANG(3)=ANGRAD(ASYM(1,1),ASYM(1,2),1)
      IF (ABS(ANG(3)-PIBY3) .LT. 0.0001) N3AX=3
      IF (ABS(ANG(3)-2.*PIBY3) .LT. 0.0001) N3AX=3
C DO NOT TRY TO REVERSE IF STATUS 3:
      IF (NNSTAT(1) .EQ. 3 .AND. NNSTAT(2) .EQ. 3) GO TO 11
      IF ((NICE .NE. -1) .AND. (ANG(3) .LE. PIBY2)) GO TO 11
      IF ((NICE .EQ. -1) .AND. (ANG(3) .GE. PIBY2)) GO TO 11
C ANGLE MUST BE ACUTE BETWEEN PLANES EXCEPT FOR CERTAIN CASES WHERE
C 2PI/3 NOT PI/3 IS WANTED
      NREV=2
      IF (NNSTAT(2) .EQ. 3) NREV=1
      CALL GMREV(ASYM(1,NREV),ASYM(1,NREV),1,3)
      ANG(3)=PI-ANG(3)
  11  AMUL=ANG(3)/TWOPI
      CALL VECPRD(ASYM(1,1),ASYM(1,2),EDGE(1,3))
C TAKE OUT ANY COMMON FACTOR:
      CALL FCTOR(EDGE(1,3),N)
      GO TO 10
C
C THREE PLANES - MAKE ANGLES SMALLER AS ABOVE, MAKE EDGES AND ANGLES:
   9  SUM=-PI
      NFLIP=0
      J2=2
      J3=3
      DO 12 J=1,3
      ANG(J)=ANGRAD(ASYM(1,J2),ASYM(1,J3),1)
      IF (ABS(ANG(J)-PIBY3) .LT. 0.0001) N3AX=J
      IF (ABS(ANG(J)-2.*PIBY3) .LT. 0.0001) N3AX=J
C DO NOT TEST 3RD ANGLE AS WE CANNOT COPE IF IT IS TOO BIG:
      IF (NFLIP .GT. 1) GO TO 13
      IF (NNSTAT(J2) .EQ. 3 .AND. NNSTAT(J3) .EQ. 3) GO TO 13
      IF ((NICE .NE. -1) .AND. (ANG(J) .LE. PIBY2)) GO TO 13
      IF ((NICE .EQ. -1) .AND. (ANG(J) .GE. PIBY2)) GO TO 13
      NREV=J3
      IF (NNSTAT(J3) .EQ.3 ) NREV=J2
      CALL GMREV(ASYM(1,NREV),ASYM(1,NREV),1,3)
C CHOOSE PLANE TO REVERSE IF NECESSARY
      ANG(J)=PI-ANG(J)
      NFLIP=NFLIP+1
  13  SUM=SUM+ANG(J)
C
C MAKE EDGE, POINTING SAME WAY AS OPPOSITE PLANE:
      CALL VECPRD(ASYM(1,J2),ASYM(1,J3),EDGE(1,J))
      IF (SCALPR(EDGE(1,J),ASYM(1,J)) .LT.0.) CALL GMREV(EDGE(1,J),
     & EDGE(1,J),1,3)
      CALL FCTOR(EDGE(1,J),N)
      J2=J3
      J3=J
  12  CONTINUE
      AMUL=SUM/FOURPI
C
C DETECT HINGE - 3 PLANES, BUT WITH A COMMON EDGE:
      CALL EQVEC(EDGE(1,1),EDGE(1,3),2,M,0)
      IF (M .GE.3) GO TO 10
C MARK HINGE TO BE DEALT WITH OUTSIDE:
      VOL=0.
      GO TO 100
C
C JOIN - WE HAVE A UNIT WITH 1, 2 OR 3 PLANES (NOT HINGE), AND WHERE RELEVANT
C EDGES AND ANGLES HAVE BEEN STORED;  MUL=FRACTION OCCUPIED:
  10  VOL=AMUL*FLOAT(NMUL)
C IF VOL IS 1 WE HAVE A UNIT OF THE RIGHT SIZE:
      IF (ABS(VOL-1.) .LT. 0.00001) NICE=0
C IF VOL IS TOO SMALL WE MAY HAVE TO REMOVE A PLANE, OR WE MAY HAVE TO STOP
C REDUCING THE ANGLE BETWEEN TWO PLANES TO PI/3 AND LEAVE IT AT 2PI/3:
      IF (VOL .LT. 0.9998) NICE=-1
      IF (NICE .EQ. -1 .AND. N3AX .NE. 0) GO TO 25
C THIS MAY LOOP, BUT THE SECOND TIME WE LOOK AT SUCH A UNIT IT SHOULD BE OK.
      IF (NCHK .EQ. 0 .AND. NICE .NE. 0) GO TO 100
C
C IF VOLUME OK, OR IF ASKED TO CHECK ANYWAY, COUNT RELATIVES OF TYPICAL REFLN:
      MOP1=0
      IC=1
      IF (FRIEDL) IC=2
C IF FRIEDEL, SCAN -HT AS WELL AS HT:
      CALL GMREV(HT,PL2,3,1)
      DO 18 NC=1,IC
      CALL GMREV(PL2,PL2,3,1)
C SCAN ALL SYMMETRY OPERATORS STORED:
      DO 18 N1=1,NOPC
      CALL ROTSYM(PL2,PL1,N1,2)
      CALL INBOX(PL1,IN)
C TEST RELATIVE FOR BEING IN GIVEN BOX - "ON" AS AN ANSWER IS AN ERROR:
      IF (IN .GT. 0) THEN
        WRITE (LPT,3002) HT
        WRITE (ITO,3002) HT
3002    FORMAT (/' *** ERROR - typical reflection',3F5.0,' not general')
C>>JCC HAndle through extra function
C Was      STOP
C Now
       CALL BMBOUT
       RETURN
C
      ENDIF
C
      IF (IN .GE. 0) THEN
C MOP1=NUMBER OF RELATIVES INSIDE BOX:
C MOP2 WILL HOLD "WHICH OPERATOR PUTS HT INTO BOX", -VELY IF BY A CENTRE ALSO:
        MOP1=MOP1+1
        MOP2=N1*(3-2*NC)
      ENDIF
  18  CONTINUE
C
C MOP1=0 MEANS WRONG BOX:
      IF (MOP1 .EQ. 0) NICE=-1
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE UNITID
      SUBROUTINE UNITID
C
C *** UNITID by JCM 26 Sep 84 ***
C
CX
CC 1A
CH A specialist routine called when an asymmetric unit has been found,
CH to tidy the unit, its faces and edges.
C
CP Must be called from SYMUNI, after calls of TRYUNI have found a
CP satisfactory unit, with NASYM faces in the array ASYM, edges in the array
CP EDGE and angles in the array ANG, all in /FUNIT/.
C
CP TRYUNI has also set MOP2 to point to the symmetry operator which
CP transforms the typical reflection HT into their relative in the
CP asymmetric unit.
C
CD Transforms the asymmetric unit as found back so that it contains
CD HT, writes out the equations of the final planes, and sets up the
CD edges again.
C
CO Write the equations of the planes bounding the asymmetric unit
CO to unit LPT.
C
      COMMON /FUNIT/NASYM,ASYM(3,3),EDGE(3,3),ANG(3),NMUL,KOM10
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C%
C      COMMON /SCRAT/AXI(3,%SYMO%,2),MIRROR(%SYMO%),D(3,3),PL1(3),PL2(3),PL3(3),
      COMMON /SCRAT/AXI(3,24,2),MIRROR(24),D(3,3),PL1(3),PL2(3),PL3(3),
     & HT(3),ASY(3,4),NSTAT(4),NOPL,NICE,VOL,MOP1,MOP2
C
      MO=IABS(MOP2)
      IF (NASYM .LE. 0) THEN
C SPECIAL FOR P1, NO FRIEDEL:
        CALL MESS(LPT,1,'No symmetrical equivalents')
        GO TO 100
      ENDIF
C
      WRITE (LPT,2001) NASYM
2001  FORMAT (/' Asymmetric unit has',I3,' plane(s):')
      DO 2 I=1,NASYM
      CALL ROTSYM(ASYM(1,I),PL1,MO,-2)
      IF (MOP2 .LT. 0) CALL GMREV(PL1,PL1,1,3)
      CALL GMEQ(PL1,ASYM(1,I),1,3)
      CALL PRIPLN(ASYM(1,I),1)
C
   2  CONTINUE
C
C REMAKE EDGES:
      IF (NASYM .LT.2) GO TO 100
      J2=2
      J3=3
      DO 3 J=1,3
      IF (NASYM .NE. 3 .AND. J .NE.3) GO TO 4
      CALL VECPRD(ASYM(1,J2),ASYM(1,J3),EDGE(1,J))
      IF (SCALPR(EDGE(1,J),ASYM(1,J)) .LT.0.) CALL GMREV(EDGE(1,J),
     & EDGE(1,J),1,3)
      CALL FCTOR(EDGE(1,J),N)
   4  J2=J3
      J3=J
   3  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE UPLOW(C)
      SUBROUTINE  UPLOW(C)
C
C *** UPLOW by JCM 3 Aug 92 ***
C
CX
CC 13C
CH Makes first letter of C upper case, and any subsequent letters lower case.
C
      CHARACTER *(*) C
      LOGICAL FIRST
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
C
      FIRST=.TRUE.
      L=LENGT(C)
      DO 1 I=1,L
      M=LETTER(C(I:I))
      IF (M .GT. 0) THEN
        IF (FIRST) C(I:I)=LETUP(M)
        IF (.NOT. FIRST) C(I:I)=LETLOW(M)
        FIRST=.FALSE.
      ENDIF
   1  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE UPONE(CH,ISYS)
      SUBROUTINE UPONE(CH,ISYS)
C
CC 13C
C *** UPONE new by PJB 28-Mar-1994 ***
C
CH Puts character strings into all uppercase or all lower case depending on ISYS
CA CH contains the string for conversion
CA If ISYS=3 (Unix) converts to lowwer case
CA All other values of ISYS convert CH to upper case
C
C TO LOWER CASE FOR UNIX UPPER FOR VMS
      CHARACTER *(*) CH
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
C
      N=LENGT(CH)
      DO 1 I=1,N
      DO 2 J=1,26
C UNIX NEEDS LOWER CASE
      IF (ISYS.EQ.3) THEN
        IF (CH(I:I) .NE. LETUP(J)) GO TO 2
        CH(I:I)=LETLOW(J)
      ELSE
        IF (CH(I:I) .NE. LETLOW(J)) GO TO 2
        CH(I:I)=LETUP(J)
      ENDIF
      GO TO 1
   2  CONTINUE
   1  CONTINUE
      RETURN
      END

C LEVEL 1      SUBROUTINE UPPER(C)
      SUBROUTINE UPPER(C)
C
C *** UPPER by JCM 3 Aug 92 ***
C
CX
CC 13C
CH Replaces any lower case letters in C by upper case
C
      CHARACTER *(*) C
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
C
      L=LENGT(C)
      DO 1 I=1,L
      M=LETTER(C(I:I))
      IF (M .GT. 0) C(I:I)=LETUP(M)
   1  CONTINUE
      RETURN
      END
C
C
C
C
      BLOCK DATA VARFMT
      COMMON/VARFOR/RLINE1,RLINE2
      CHARACTER*24 RLINE2
      CHARACTER*16 RLINE1
C
      DATA RLINE1,RLINE2/'(5X,3F5.0,I5,I5)','(15X,2F10.5,10X,2F10.5)'/
      END
C
C
C
C
C LEVEL 8       SUBROUTINE VARMAK(DEFALT,GETPAR,VARSXX)
      SUBROUTINE VARMAK(DEFALT,GETPAR,VARSXX)
C
C *** VARMAK updated by JCM 14 Nov 90 ***
C
CX
CC 6A
CH Makes variables for a LSQ cycle from given FIX/VARY and CONSTRAINT lists.
CA DEFALT is a dummy name for a subroutine to be obeyed if no other
CA        information is available about a parameter, and which gives
CA        out the values .TRUE./.FALSE. for fixed/varied by default.
CA GETPAR is a dummy name for a subroutine to be obeyed to get the next
CA        parameter.  It should be replaced by PARRUN in the single-
CA        crystal applications, RUNPAR for Profile Refinement.
CA VARSXX is the dummy name for a subroutine to be obeyed when all the
CA        variables have been designated, to record "which variable" is this
CA        parameter" against all possible parameters for this application.
CA        This method of recording the information is new to MK4.
C
CP Parameter naming must have been set up by LSETUP
CP All fixing, varying and constraining information must have been set up
CP into the COMMON /LINKAG/ using routines ADDFIX, ADDCON, SUBCON
CP ADDVAR
C
CD Scans all available information about all parameters.  Sets up tables
CD of: LVRPR "which parameter is this variable" (holds a packed KK value)
CD     LBSVR "which variable is this basic variable" (all +ve)
CD     LVRBS "which basic is this variable" (+=basic, -=redundant)
CD     LRDVR "which variable is this redundant variable"
CD
CD Stores all the constraints relevant to this cycle in /CONSTR/
CD
CD Sets up various useful quantities:
CD     LVARB=number of basic variables
CD     LVARV=number of variables
CD     NVARF()=numbers of variables in each phase in each family
CD     NBARF()=numbers of basics in each phase in each family
CD     LVFST1()=1 BEFORE starting place in a derivative vector for a
CD              particular family of a particular phase, i.e. which is the last
CD              variable before the first variable of that family.
CD     LBFST()=1 BEFORE starting place in a basic variable vector for a
CD              particular family of a particular phase, i.e. which is the last
CD              variable before the first basic variable of that family.
CD Calls VARSXX (application dependent) to store what used to be held in the
CD big array LPRVR, i.e. "which variable is this parameter?"
C
CO Calls PRIVAR to print out its findings.
C
      LOGICAL DEFALT
      EXTERNAL DEFALT,GETPAR,VARSXX
      LOGICAL FOUND,KSAME,FX
C%
C      DIMENSION KKCOL(%CPAR%),KBVCOL(%CPAR%),A(%CPAR%,%ALLC%),KPRVR(%PVAR%)
      DIMENSION KKCOL(500),KBVCOL(500),A(500,200),KPRVR(2000)
C%
C      DIMENSION KREDUN(%ALLC%)
      DIMENSION KREDUN(200)
      COMMON /CONSTR/JCONST,JROWPT(301),JCMAT(200),AMOUNT(200),
     & NEXTJ
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LINKAG/NUMFV,NUMPAK,KKFV(200),KTYPFV(200),KSTFV(200),
     & KTIME(200),KUNPFV(5,30),NTIME,NUMCON,KKCON(500),AMCON(500),
     & KPTCON(201),KSTCON(200),KTPCON(200)
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
C START COUNTS OF VARIABLES AND BASIC VARIABLES:
      LVARV=0
      LVARB=0
C NOT IF ONLY SIMULATION:
      IF (SIMUL) GO TO 100
C
C COUNT FIXED PARAMETERS:
      LFIX=0
C
      DO 88 I=1,NPHASE
      DO 88 J=1,NFAM
      DO 88 K=1,NSOURC
C CLEAR COUNT OF VARIABLES & BASICS/FAMILY/PHASE/SOURCE
      NBARF(J,I,K)=0
      NVARF(J,I,K)=0
C CLEAR POINTERS TO STARTS-1 OF FAMILY VARIABLES  & BASICS PER PHASE/SOURCE:
      LVFST1(J,I,K)=-1
  88  LBFST1(J,I,K)=-1
C
C FIRST SCAN FIX & VARY LISTS AND FIX WHERE POSSIBLE:
C START A SCAN OF PARAMETERS:
      IFAM=0
C
C NEXT PARAMETER:
   2  CALL GETPAR(IFAM,IGEN,ISPC)
      IF (IFAM .EQ. -1) GO TO 3
      FOUND=.FALSE.
      KTIM=0
      KSTAT=0
C GET AND KK FOR THIS PARAMETER:
      KK=KPAK(IFAM,IGEN,ISPC,JPHASE,JSOURC)
C SCAN FIXES & VARIES, BOTH SPECIFIC & BLANKET, FOR MATCHES:
      DO 1 I=1,NUMFV
      IF (.NOT. KSAME(KK,KKFV(I))) GO TO 1
      IF (KTIME(I) .GT. KTIM .AND. IABS(KSTFV(I)) .GE. KSTAT)THEN
        KTIM=KTIME(I)
        KSTAT=IABS(KSTFV(I))
        FX=(KSTFV(I) .GT. 0)
      ENDIF
      FOUND=.TRUE.
   1  CONTINUE
C
C NO MENTION SO FAR IF FOUND STILL FALSE:
      IF (.NOT. FOUND) THEN
C FIRST SEE IF 'ONLY' OCCURRED ON 'FIX' CARD:
        IF (IONLY(JPHASE) .EQ. 1) THEN
          FX  =.FALSE.
C OR ON 'VARY' CARD:
        ELSE IF (IONLY(JPHASE) .EQ. 2) THEN
          FX  =.TRUE.
        ELSE
C OTHERWISE DEFAULT:
          FX  =.NOT. (DEFALT(IFAM,IGEN,ISPC))
        ENDIF
      ENDIF
C
C RECORD KK FOR "CERTAINLY FIXED" (OF WHICH THE OPPOSITE IS "PROBABLY VARIED")
      IF (FX) THEN
C%
C      CALL ERRCHK(2,LFIX,%PVAR%,0,'LSQ parameters in VARMAK')
      CALL ERRCHK(2,LFIX,2000,0,'LSQ parameters in VARMAK')
        KPRVR(LFIX)=KK
      ENDIF
C NEXT PARAMETER
      GO TO 2
C
C SECOND LIST ALL PARAMETERS OCCURRING IN CONSTRAINTS:
   3  NPAR=0
      DO 41 I=1,NUMCON
C NOT IF CONSTRAINT DELETED:
      IF (KSTCON(I) .EQ. 0) GO TO 41
C
C SCAN EVERY PARAMETER SPEC IN THIS CONSTRAINT:
      DO 42 K=KPTCON(I),KPTCON(I+1)-1
C NOT IF FIXED:
      IF (LFIX .EQ. 0) GO TO 46
      IF (NFIND(KKCON(K),KPRVR,LFIX) .GT. 0) GO TO 42
  46  IF (NPAR .EQ. 0) GO TO 43
C IS IT THERE ALREADY?
      IF (NFIND(KKCON(K),KKCOL,NPAR) .GT. 0) GO TO 42
C ADD TO LIST:
C%
C  43  CALL ERRCHK(2,NPAR,%CPAR%,0,'parameters in strict constraints')
  43  CALL ERRCHK(2,NPAR,500,0,'parameters in strict constraints')
C
C IN DESCENDING SEQUENCE (SO THAT REDUNDANTS WILL BE LATER PARAMETERS)
      DO 44 J=1,NPAR-1
      IF (KKCOL(J) .GT. KKCON(K)) GO TO 44
      DO 45 L=NPAR,J+1,-1
  45  KKCOL(L)=KKCOL(L-1)
      KKCOL(J)=KKCON(K)
      GO TO 42
  44  CONTINUE
      KKCOL(NPAR)=KKCON(K)
C      DO 44 L=NPAR-1,1,-1
C      IF (KKCOL(L) .GT. KKCON(K)) GO TO 45
C  44  KKCOL(L)=KKCOL(L-1)
C      L=1
C  45  KKCOL(L)=KKCON(K)
  42  CONTINUE
  41  CONTINUE
C
C
C FILL IN MATRIX OF COEFFICIENTS IN CONSTRAINTS:
C IN MATRIX A, FIRST SUBSCRIPT = PARAMETER, SECOND=CONSTRAINT
C%
C      CALL GMZER(A,%CPAR%,%ALLC%)
      CALL GMZER(A,500,200)
      NCON=0
      DO 51 I=1,NUMCON
      IF (KSTCON(I) .EQ. 0) GO TO 51
C%
C      CALL ERRCHK(2,NCON,%ALLC%,0,'strict constraints')
      CALL ERRCHK(2,NCON,200,0,'strict constraints')
      DO 52 K=KPTCON(I),KPTCON(I+1)-1
C NOT IF ALREADY FIXED:
      IF (LFIX .EQ. 0) GO TO 53
      IF (NFIND(KKCON(K),KPRVR,LFIX) .GT. 0) GO TO 52
  53  A(NFIND(KKCON(K),KKCOL,NPAR),NCON)=AMCON(K)
  52  CONTINUE
  51  CONTINUE
C
C THIS MATRIX MAY CONTAIN REDUNDANT OR INCONSISTENT CONSTRAINTS, OR
C SIMPLY CONSTRAINTS NOT IN THE BEST FORM FOR DESIGNATING REDUNDANT
C VARIABLES.  PERFORM GAUSSIAN ELIMINATION ON IT:
C
      SMALL=0.000001
      DO 61 NP=1,NCON
      DO 62 J=NP,NPAR
      DO 62 I=NP,NCON
      IF (ABS(A(J,I)) .GT. SMALL) GO TO 63
  62  CONTINUE
C
C NO MORE NON-ZERO COEFFICIENTS LEFT - OUT
      GO TO 64
C
C PIVOT FOUND:
C SWOP COLUMNS J AND NP (EVEN IF THEY ARE THE SAME), AND SCALE:
  63  KTEMP=KKCOL(J)
      KKCOL(J)=KKCOL(NP)
      KKCOL(NP)=KTEMP
      PIVOT=A(J,I)
      DO 65 K=1,NCON
      TEMP=A(J,K)
      A(J,K)=A(NP,K)
      A(NP,K)=TEMP
  65  CONTINUE
C
C NOW SWOP ROWS I AND NP:
      DO 66 K=1,NPAR
      TEMP=A(K,I)
      A(K,I)=A(K,NP)
      A(K,NP)=TEMP/PIVOT
  66  CONTINUE
C
C NOW SCAN ALL CONSTRAINTS, OMITTING THE SECTION BETWEEN NP AND I
C WHICH WE ALREADY KNOW TO HAVE ZEROS, AND ELIMINATE:
      DO 71 L=1,NCON
      IF (L .GE. NP .AND. L .LE. I) GO TO 71
      IF (ABS(A(NP,L)) .LT. SMALL) GO TO 71
      DO 72 M=NP+1,NPAR
  72  A(M,L)=A(M,L)-A(NP,L)*A(M,NP)
  71  CONTINUE
C
  61  CONTINUE
C
  64  NCON=NP-1
      IF (NPAR .LT. NCON) THEN
        WRITE (LPT,3010) NPAR,NCON
        WRITE (ITO,3010) NPAR,NCON
3010    FORMAT (' ERROR ** ',I4,' parameters in ',I4,' constraints')
C>>JCC HAndle through extra function
C Was      STOP
C Now
        CALL BMBOUT
        RETURN
C
      ENDIF
C
C THIRD MARK THE FIRST NCON PARAMETERS FROM THE ELIMINATED
C MATRIX AS "NOT BASIC" (MAY BE FIXED OR REDUNDANT)
      DO 68 I=1,NCON
      M=-I
      DO 69 J=NCON+1,NPAR
C A NON-ZERO IN THIS PANEL MEANS A CONSTRAINT, NOT A FIXING:
      IF (ABS(A(J,I)) .GT. SMALL) GO TO 60
  69  CONTINUE
C
C HERE WE HAVE DISCOVERED THAT THE PARAMETER IS ACTUALLY FIXED:
C%
C      CALL ERRCHK(2,LFIX,%PVAR%,0,'LSQ parameters in VARMAK')
      CALL ERRCHK(2,LFIX,2000,0,'LSQ parameters in VARMAK')
      KPRVR(LFIX)=KKCOL(I)
      KREDUN(I)=0
      GO TO 68
C
C HERE WE HAVE DISCOVERED THAT THE PARAMETER IS REDUNDANT:
  60  KREDUN(I)=KKCOL(I)
  68  CONTINUE
C
C FOURTH SCAN PARAMETERS AGAIN, DESIGNATING BASICS AND REDUNDANTS:
C
      IFAM=0
C
C NEXT PARAMETER:
  21  CALL GETPAR(IFAM,IGEN,ISPC)
C GETPAR RETURNS IFAM -1 IF ALL FINISHED:
      IF (IFAM .EQ. -1) GO TO 22
      KK=KPAK(IFAM,IGEN,ISPC,JPHASE,JSOURC)
C
C IF PARAMETER IS FIXED, LEAVE IT SO:
      IF (LFIX .EQ. 0) GO TO 23
      IF (NFIND(KK,KPRVR,LFIX) .GT. 0) GO TO 21
C IF NOT DESIGNATED "FIX" IT MUST BE A VARIABLE:
C%
C  23  CALL ERRCHK(2,LVARV,%VVAR%,0,'variables in LSQ')
  23  CALL ERRCHK(2,LVARV,500,0,'variables in LSQ')
C
C COUNT VARIABLES/FAMILY/PHASE:
      NVARF(IFAM,JPHASE,JSOURC)=NVARF(IFAM,JPHASE,JSOURC)+1
C RECORD STARTS OF FAMILIES IN ANY VARIABLES VECTOR, -1:
      IF (LVFST1(IFAM,JPHASE,JSOURC) .EQ. -1) THEN
        LVFST1(IFAM,JPHASE,JSOURC)= LVARV-1
      ENDIF
C
*** and check that F2VA has not been exceeded
C
C RECORD "WHICH PARAMETER IS THIS VARIABLE?"
      LVRPR(LVARV)=KK
C IF REDUNDANT, DEFER FURTHER DETAILS TILL WE HAVE LABELLED ALL BASICS:
      IF (NCON .NE. 0) THEN
        N=NFIND(KK,KREDUN,NCON)
        IF (N .GT. 0) THEN
          KREDUN(N)=LVARV
          GO TO 21
        ENDIF
      ENDIF
C
C MAKE IT BASIC:
C%
C      CALL ERRCHK(2,LVARB,%BVAR%,0,'basic variables in LSQ')
      CALL ERRCHK(2,LVARB,400,0,'basic variables in LSQ')
C RECORD CROSS POINTERS FOR VARIABLES AND BASIC VARIABLES:
      LVRBS(LVARV)=LVARB
      LBSVR(LVARB)=LVARV
C RECORD STARTS OF FAMILIES IN ANY BASIC VARIABLES VECTOR, -1:
      IF (LBFST1(IFAM,JPHASE,JSOURC) .EQ. -1) THEN
        LBFST1(IFAM,JPHASE,JSOURC)= LVARB-1
      ENDIF
C
C ALL THE PARAMETERS INVOLVED IN THE RIGHT HAND SIDES OF CONSTRAINTS ARE
C BASIC - SEE IF THIS ONE IS THERE, AND RECORD WHICH VARIABLE IT IS IF SO:
      IF (NPAR .GT. 0) THEN
        N=NFIND(KK,KKCOL,NPAR)
        IF (N .GT. 0) KBVCOL(N)=LVARB
      ENDIF
C
C RECORD BASIC VARIABLES/FAMILY/PHASE:
      NBARF(IFAM,JPHASE,1)=NBARF(IFAM,JPHASE,1)+1
      GO TO 21
C
C A BIT OF OVERKILL UNTIL I DECIDE ON A PERMANENT STRUCTURE:
  22  IV=LVARV
      IB=LVARB
      DO 301 JJU=NPHASE,1,-1
      DO 301 IJU=NFAM,1,-1
      DO 301 KJU=NSOURC,1,-1
      IF (LBFST1(IJU,JJU,KJU) .EQ. -1) THEN
        LBFST1(IJU,JJU,KJU)=IB
      ELSE
        IB=LBFST1(IJU,JJU,KJU)
      ENDIF
      IF (LVFST1(IJU,JJU,KJU) .EQ. -1) THEN
        LVFST1(IJU,JJU,KJU)=IV
      ELSE
        IV=LVFST1(IJU,JJU,KJU)
      ENDIF
 301  CONTINUE
C
C FINALLY ABSORB ALL CONSTRAINTS FOR USE THIS CYCLE:
      JCONST=0
      NEXTJ=1
      JROWPT(1)=1
      DO 30 I=1,NCON
      LV=KREDUN(I)
      IF (LV .EQ. 0) GO TO 30
C%
C      CALL ERRCHK(2,JCONST,%CSTR%,0,'strict constraints')
      CALL ERRCHK(2,JCONST,300,0,'strict constraints')
C RECORD CROSS POINTERS FOR VARIABLES AND REDUNDANT VARIABLES:
      LVRBS(LV)=-JCONST
      LRDVR(JCONST)=LV
      DO 31 J=NCON+1,NPAR
      IF (ABS(A(J,I)) .LT. SMALL) GO TO 31
C
C PUT CONSTRAINT INTO TABLE TO USE THIS CYCLE:
      JCMAT(NEXTJ)=KBVCOL(J)
      AMOUNT(NEXTJ)=-A(J,I)
      NEXTJ=NEXTJ+1
      JROWPT(JCONST+1)=NEXTJ
  31  CONTINUE
  30  CONTINUE
C
C FETTLE STARTS OF FAMILIES FOR THOSE WITH NO MEMBERS:
***???
C
C PRINT OUT WHAT WE HAVE DONE:
      CALL PRIVAR
C
C CALL APPLICATION-DEPENDENT ROUTINE TO FILL IN "WHICH VARIABLE IS THIS
C PARAMETER?" FOR ALL PARAMETERS:
      CALL VARSXX
C
 100  RETURN
      END
C
C

C


C
C
C LEVEL 1      FUNCTION VCTMOD(SCALE,H,IR)
      FUNCTION VCTMOD(SCALE,H,IR)
C
C *** VCTMOD by JCM 26 Apr 84 ***
C
CX
CC 1B
CH Calculates the modulus of the vector H, in either space.
CA On entry H is a 1x3 array holding the given vector
CA          SCALE is the number by which to multiply the answer
CA          IR indicates the required space.  The modulus is calculated
CA             in real space if IR=1, reciprocal if IR=2.
C
CN Multiplies by the input quantity SCALE - useful if SCALE is equal
CN to say, lambda/2
C
      DOUBLE PRECISION VEC
      DIMENSION H(3)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
C
      VEC = 0.
      J = 2
      K = 3
      DO 1 I=1,3
      VEC=VEC+H(I)*H(I)*CPARS(I,IR) + H(J)*H(K)*2.*CPARS(I+3,IR)
      J = K
   1  K = I
      VEC = SCALE*DSQRT(VEC)
      VCTMOD = SNGL(VEC)
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE VOCAB(WORD,MEAN,NW)
      SUBROUTINE VOCAB(WORD,MEAN,NW)
C
C *** VOCAB by JCM 4 Aug 90 ***
C
CX
CC 6A
CH Adds a given set of vocabulary and meanings to the Least Squares
CH total vocabulary.
CA On entry WORD is an array of NW A4 words
CA          MEAN is a corresponding (3,NW) array of integers, being the family,
CA               genus and species assigned to the word
CP In /WDSPC/ IWDNUM should hold the existing number of entries in LSQWD.
CP If used in a PR context:
CP In /PHASE/ KPHASE should hold the relevant phase (or zero if for all phases)
CP In /SOURCE/ KSOURC should hold the relevant source (or zero for all sources)
CD Copies the word array to LSQWD, starting at entry IWDNUM+1
CD If the first of a triplet in MEAN is negative, copies it to IWDSPC
CD Otherwise packs the triplet according to preset packing to IWDSPC
CD Updates IWDNUM, checking it.
C
      CHARACTER *4 WORD(NW)
      DIMENSION MEAN(3,NW)
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      COMMON /WDSPC/IWDNUM,IWDSPC(60)
      COMMON /WORDS/LSQWD(60)
      CHARACTER *4 LSQWD
C
C CHECK ARRAY SIZES:
C%
C      CALL ERRCHK(1,IWDNUM+NW,%WORD%,0,
      CALL ERRCHK(1,IWDNUM+NW,60,0,
     & 'words specifying LSQ problem')
      DO 1 I=1,NW
      LSQWD(I+IWDNUM)=WORD(I)
      IF (MEAN(1,I) .LT. 0) THEN
        IWDSPC(I+IWDNUM)=MEAN(1,I)
      ELSE
        IWDSPC(I+IWDNUM)=KPAK(MEAN(1,I),MEAN(2,I),MEAN(3,I),
     &  KPHASE,KSOURC)
      ENDIF
   1  CONTINUE
      IWDNUM=IWDNUM+NW
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE VOIGT(X,SIGMA,GAMMA,YVAL,DERX,DERS,DERG)
      SUBROUTINE VOIGT(X,SIGMA,GAMMA,YVAL,DERX,DERS,DERG)
C
C *** VOIGT by WIFD Jun 84 ***
C
CC 9C
CH Calculates normalised Voigt function
C
CD...      VOIGT is the normalised VOIGT function; a convolution of a
CD      normalised Gaussian (half-width= sigma) and Lorentzian
CD      (Cauchy) function (full-width at half-height= width). The
CD      function is calculated by noting that the VOIGT function
CD      is, to within a scale factor, equal to the real part of the
CD      complex error function.
C            W.I.F.David            6-JUN-84
C            Neutron Division
C            RAL                  ext. 5179
C
      DOUBLE PRECISION WR,WI,XX,YY
C
      OVRTPI=0.564189584
      OVRT2=0.707106781
      BTEM=OVRT2/SIGMA
      ATEM=OVRTPI*BTEM
      XTEM=X*BTEM
      YTEM=0.5*GAMMA*BTEM
      XX= DBLE(XTEM)
      YY= DBLE(YTEM)
      CALL WERF(WR,WI,XX,YY)
      SWR=SNGL(WR)
      SWI=SNGL(WI)
      CTEM=ATEM*BTEM
      YVAL=ATEM*SWR
      DWRDX=-2.*(XTEM*SWR-YTEM*SWI)
      DWRDY= 2.*(YTEM*SWR+XTEM*SWI-OVRTPI)
      DERX=CTEM*DWRDX
      DERS=-ATEM*(SWR+DWRDX*XTEM+DWRDY*YTEM)/SIGMA
      DERG=0.5*CTEM*DWRDY
C
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE WERF(RS1,RS2,XX,YY)
      SUBROUTINE WERF(RS1,RS2,XX,YY)
C
C *** WERF by WIFD 25 May 84 ***
C
CC 9C
CH Weighted error function ???
C
      IMPLICIT DOUBLE PRECISION            (A-H,O-Z)
      DOUBLE PRECISION                  LAMBDA
      LOGICAL                  B
C
      X=DABS(XX)
      Y=DABS(YY)
      IF (Y .LT. 4.29 .AND. X .LT. 5.33) GO TO 1
      H= 0.
      NC= 0
      NU= 8
      LAMBDA= 0.
      B= .TRUE.
      GO TO 2
 1      S=(1.0-Y/4.29)*DSQRT(1.0-X**2/28.41)
      H=1.6*S
      H2=2.0*H
      NC=6+IDINT(23.0*S)
      NU=9+IDINT(21.0*S)
      LAMBDA=H2**NC
      B=LAMBDA .EQ. 0.
 2      R1=0.
      R2=0.
      S1=0.
      S2=0.
      N=NU+1
 3      N=N-1
      FN=N+1
      T1=Y+H+FN*R1
      T2=X-FN*R2
      C=0.5/(T1**2+T2**2)
      R1=C*T1
      R2=C*T2
      IF (H .LE. 0.0 .OR. N .GT. NC) GO TO 4
      T1= LAMBDA+S1
      S1=R1*T1-R2*S2
      S2=R2*T1+R1*S2
      LAMBDA=LAMBDA/H2
 4      IF (N .GT. 0) GO TO 3
      IF (B) GO TO 6
      RS1=S1
      RS2=S2
      GO TO 7
 6      RS1=R1
      RS2=R2
 7      RS1= 1.12837916709551*RS1
      IF (Y .EQ. 0.0) RS1= DEXP(-X**2)
      RS2= 1.12837916709551*RS2
      IF (XX .LT. 0) RS2= -RS2
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE WGHTLS(N,ARG)
      SUBROUTINE WGHTLS(N,ARG)
C
C *** WGHTLS updated by JCM 23 Mar 92 ***
C
CX
CC 6C
CH Performs various operations to do with weights for LSQ, either for PR
CH or simpler applications.
CH applications.
CA On entry N=1 means find an L WGHT card and read from it the type of
CA              weighting to be used, with default = 1.  ARG is irrelevant.
CA          N=2 means from ARG, IWGHT etc make a weight to go with this OBS
CA          N=3 means given WT, get WDIFF and SQRTWT.  ARG is included for
CA              diagnostic purposes only, and is sometimes used as the
CA              argument, or sometimes the observation.
C
      LOGICAL ONCARD
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /OBSCAL/OBS,DOBS,GCALC,YCALC,DIFF,ICODE,SUMWD,NOBS,
     & IWGH(5),WTC(4),WT,SQRTWT,WDIFF,YBACK,YPEAK,YMAX,CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
C
      GO TO (10,20,30) , N
C
C FIND AND INTERPRET AN L WGHT CARD, OR THE LACK OF IT:
  10  IF (.NOT. ONCARD('L','WGHT',W)) THEN
        CALL MESS(LPT,1,'No L WGHT card -')
        GO TO 101
      ELSE
        IWGH(KSOURC)=NINT(W)
        IF (IWGH(KSOURC) .GT. 3 .OR. IWGH(KSOURC) .LE. 0) THEN
          CALL ERRIN2(IWGH(KSOURC),1,'weighting scheme',
     &  ' not allowed -')
          GO TO 101
        ENDIF
      ENDIF
C
      GO TO (41,42,43) , IWGH(KSOURC)
C
C UNIT WEIGHTS:
  41  CALL MESS(LPT,1,'Unit weights')
      GO TO 100
C
C WEIGHT TO BE USED AS READ:
  42  CALL MESS(LPT,1,
     & 'Weights to be used as read from reflection data')
      GO TO 100
C
C SIGMA READ, WEIGHT IS 1/SIGMA SQUARED:
  43  CALL MESS(LPT,1,'Sigma read from reflection data - weight '//
     & 'is 1/sigma squared')
      GO TO 100
C
C INITIAL VALUE FOR WT - USED TO BE SUBROUTINE WTINPR:
  20  GO TO (1,2,3) , IWGH(JSOURC)
C
C UNIT WEIGHTS:
   1  WT=1.
      GO TO 100
C
C DOBS READ AND TO BE USED AS WEIGHT:
   2  WT=DOBS
      GO TO 100
C
C SIGMA READ (INTO DOBS) - USE 1/SIGMA SQUARED:
   3  IF (DOBS.NE.0.) THEN
        WT=1./(DOBS*DOBS)
      ELSE
   4    WRITE(LPT,3000) ARG,OBS
3000    FORMAT(/' WARNING ** zero weight found for point ',
     &   2(F10.3,2X),' -- weight set to unity ')
        WT=1.0
      ENDIF
      GO TO 100
C
  30  SQRTWT=SQRT(WT)
      WDIFF=SQRTWT*DIFF
      GO TO 100
C
 101  IWGH(KSOURC) = 1
      CALL MESS(LPT,0,'assuming unit weights')
 100  RETURN
      END
C
C

C

C
C
C LEVEL 3      SUBROUTINE XROOT(IA,XX,IS,IL,C)
      SUBROUTINE XROOT(IA,XX,IS,IL,C)
C
C *** XROOT updated by JCM 18 Oct 87 ***
C
CX
CC 8B
CH Finds the symmetry operations which take the given source atom into
CH the given coordinates.
C
CA On entry IA is the number of the atom in the given list on A cards
CA          XX is a 1x3 real array holding the given x,y,z to be identified.
CA On exit IS the number of the necessary symmetry operator, set -ve if
CA            the (-x, -y, -z) operator is also required;  if no such operator
CA            is found, IS=0
CA         IL is the number of the necessary lattice translation.
CA         C is a 1x3 real array holding any necessary cell translations,
CA           which will be whole numbers.
C
CP The symmetry should have been read by SYMOP, and the atomic positions
CP by ATOPOS.
C
CN There is an inverse subroutine XTRANS
CN Does not now involve the putting of a transformed atom back into
CN a central cell, because this had awkward repercussions in slack
CN constraints.
C
      DIMENSION XX(3),X1(3),X2(3),X3(3),C(3)
      LOGICAL GMSAME
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C CYCLE OVER CENTRE
      DO 1 IC=1,NCENT
C CYCLE OVER OPERATORS WITHOUT CENTRE:
      DO 2 IS=1,NOPC
      CALL ROTSYM(X(1,IA),X1,IS,1)
      CALL GMADD(X1,TRANS(1,IS),X1,1,3)
      IF (IC .EQ. 2) CALL GMREV(X1,X1,1,3)
C CYCLE OVER LATTICE TRANSLATIONS:
      DO 3 IL=1,NLAT
      CALL GMADD(X1,ALAT(1,IL),X2,1,3)
C DO NOT NOW PUT INTO CENTRAL CELL:
C      CALL FRAC3(X2)
C
C SCAN ALL 27 CELLS:
      DO 4 NCELZ=1,5
      C(3)=FLOAT(NCELZ-3)
      DO 5 NCELY=1,5
      C(2)=FLOAT(NCELY-3)
      DO 6 NCELX=1,5
      C(1)=FLOAT(NCELX-3)
      CALL GMADD(X2,C,X3,1,3)
C
C X3 SHOULD EVENTUALLY MATCH OUR TARGET:
      IF (GMSAME(XX,X3,3,0.0001)) GO TO 101
   6  CONTINUE
   5  CONTINUE
   4  CONTINUE
   3  CONTINUE
   2  CONTINUE
   1  CONTINUE
C IF HERE, WE HAVE NO MATCH:
      IS=0
 101  IF (IC .EQ. 2) IS=-IS
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE XTRANS(IAT,XX,IS,IL,C)
      SUBROUTINE XTRANS(IAT,XX,IS,IL,C)
C
C *** XTRANS corrected by JCM 18 Oct 87 ***
C
CX
CC 8B
CH Transforms a given atomic position by given symmetry operator and
CH lattice and cell translations.
C
CA On entry IAT = which atom
CA          IS = which symmetry operator, -ve if -x,-y,-z
CA          IL = which lattice translation
CA          C is a 1x3 real array holding cell translations in the x, y and
CA            z directions, which will usually be integers.
CA On exit  XX is a 1x3 real array holding the transformed coordinates
CP The symmetry should have been set up by SYMOP, and the atomic positions
CP by ATOPOS.
C
CN There is an inverse subroutine XROOT
CN Altered not to put atom into central cell after symmetry transformations
CN because this has awkward repercussions in slack constraints
C
      DIMENSION XX(3),C(3)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C ROTATE BY SYMMETRY OPERATOR:
      CALL ROTSYM(X(1,IAT),XX(1),IABS(IS),1)
C TRANSLATION VECTOR ASSOCIATED WITH THIS SYMMETRY OPERATOR:
      CALL GMADD(XX(1),TRANS(1,IABS(IS)),XX(1),1,3)
C CENTRE OF SYMMETRY IF APPLICABLE:
      IF (IS .LT. 0) CALL GMREV(XX,XX,1,3)
C LATTICE TRANSLATION:
      CALL GMADD(XX(1),ALAT(1,IL),XX(1),1,3)
C DO NOT NOW PUT INTO CENTRAL CELL:
C      CALL FRAC3(XX)
C CELL TRANSLATIONS:
      CALL GMADD(XX(1),C(1),XX(1),1,3)
      RETURN
      END
C
C


C>> JCC Added ion BMBOUT 

      SUBROUTINE BMBOUT
C BMBOUT BoMBs OUT of the code. Essentially this replaces STOP in the code but 
C implements a pseudo-error throwing mechanism. We cant have STOP being called while
C the GUI is running, since this will annoy users a lot

      INTEGER IBMBER
      COMMON / CCSLER / IBMBER 
      DATA IBMBER / 0 /
C
C If you want to stop on an error, then comment out the next line
C     STOP
C
      IBMBER = 1 ! The alternative: track with a flag
      RETURN
      END
