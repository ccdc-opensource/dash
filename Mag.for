C
C
C

C
C
C LEVEL 1      SUBROUTINE CENTRO(CVEC,SVEC,RMAT,PSIFAC,PSIFCC,PSI,PCEN)
      SUBROUTINE CENTRO(CVEC,SVEC,RMAT,PSIFAC,PSIFCC,PSI,PCEN)
C
C *** CENTRO updated by PJB 27 May 92 ***
C
CX
CC 17B
CH Executes the action of the centre of symmetry on a term in the
CH magnetic structure factor for helimagnetic structures.
CA On entry: CVEC(3)   is a complex vector, the term for one operator,
CA           RMAT(3,3) is the required rotation matrix,
CA           PSIFAC    is the phase factor associated with the operator,
CA           PSIFCC    is the phase factor for the centrosymmetic operator,
CA           PSI       is TRUE if PSIFAC is required,
CA           PCEN      is TRUE if the magnetic structure has a centre of
CA                     symmetry so that PSIFCC is not required.
CA On exit:  SVEC(3)   is the result of applying the phase factors and the
CA                     centrosymmetric rotation.
CA For entry CENDIF the two terms VEC1 and VEC2 whose sum is SVEC
CA are also returned to allow calculation of the differentials wrt the psi's.
CN SVEC can be the same as CVEC if replacement is required
CN Called in magnetic stucture factors and magnetic least squares when
CN treating the centre of symmetry.
C
      LOGICAL PSI,PCEN,DIF
      COMPLEX CVEC(3),SVEC(3),TVEC(3),TVEC1(3),TVEC2(3)
      COMPLEX PSIFAC,PSIFCC,VEC1(3),VEC2(3)
      DIMENSION RMAT(3,3)
C
      DIF=.FALSE.
      GO TO 1
C
      ENTRY CENDIF(CVEC,SVEC,VEC1,VEC2,RMAT,PSIFAC,PSIFCC,PSI,PCEN)
      DIF=.TRUE.
C COMPLEX CONJUGATE FOR CENTROSYMETRIC STRUCTURE FACTOR
   1  CALL CMCONJ(CVEC,TVEC1,3,1)
      IF (PSI) THEN
C PHASE FACTOR ASSOCIATED WITH DIRECT OPERATOR
        CALL CGMSCA(CVEC,TVEC,PSIFAC,3,1)
      ELSE
C VECTORS UNCHANGED IF NONE
        CALL CGMEQ(CVEC,TVEC,3,1)
      ENDIF
C SPIN ROTATION ASSOCIATED WITH THE CENTRE OF SYMMETRY
      CALL RCMPRD(RMAT,TVEC1,TVEC2,3,3,1)
      IF (.NOT.PCEN) THEN
C PHASE FACTOR ASSOCIATED WITH CENTRIC  OPERATOR
        CALL CGMSCA(TVEC2,TVEC2,PSIFCC,3,1)
      ENDIF
      CALL CGMADD(TVEC2,TVEC,SVEC,3,1)
      IF (DIF) THEN
C TWO TERMS IN THE SUM RETURNED FOR DIFFERENTIALS
        CALL CGMEQ(TVEC,VEC1,3,1)
        CALL CGMEQ(TVEC2,VEC2,3,1)
      ENDIF
      RETURN
      END
C
C


C

C
C
C LEVEL 6      SUBROUTINE FMCALC(H,FMCMOD,FMCSQR)
      SUBROUTINE FMCALC(H,FMCMOD,FMCSQR)
C
C *** FMCALC corrected by JBF/PJB 13-Jan-1995 ***
C
CX
CC 17B
CH Calculates magnetic interaction vectors and magnetic structure factors.
CA On entry H is the 1x3 vector containing h,k,l
CA On exit   FMCMOD = domain average of the lengths of the m.i. vector
CA           FMCSQR = square of the above
CD On exit Q(1:3,1:NDOM) in COMMON QCAL contains the magnetic interaction
CD vectors for each of the NDOM domains.
CP STHL in /BRAGG should hold sin theta/lambda
CP NKSTAR in /SATELL should have been set up by routine KSTAR
CP
CP The setting up routines:
CP     RECIP  (for the cell parameters)
CP     SYMOP  (for the space group symmetry)
CP     SETFOR (for the scattering factors, both nuclear and magnetic)
CP     SETANI (for the anisotropic temperature factors)
CP     DOMAG(1) and (2) (for the magnetic structure) and
CP     SPHELI (to set up the spin directions on spherical polars)
CP should all have been obeyed to set up the structure.
CD Sets SSQRD in /BRAGG to be STHL squared
CD Gives zero as answers for magnetic absences
CN There is also the routine LMCALC which does a similar calculation but also
CN calculates derivatives, for use in LSQ.
C
      COMPLEX SUM1(3),TERM,TVEC(3),FORM,HR,FORMFA,P(3),FMC(3)
      COMPLEX PSIFAC,PSIFCC
      LOGICAL SKIP,MAGABS,PSI,PSICEN,CENPSI,DOCENT
      DIMENSION RH(3),H(3),RS(3,3),HD(3,3),SDOM(3,3)
      COMMON /BRAGG/STHMXX(5),STHL,SINTH,COSTH,SSQRD,TWSNTH(5),
     & DSTAR2,TWOTHD(5),DIFANG(6)
      EQUIVALENCE(STHLMX,STHMXX(1))
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /QCAL/Q(3,12)
      COMPLEX Q
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C CLEAR ALL ANSWERS IN CASE ABSENT:
C
C CLEAR MODULUS AND SQUARE:
      FMCMOD=0.
      FMCSQR=0.
      SSQRD=STHL*STHL
C
C OUT IF ABSENT:
      IF (MAGABS(H,IK)) GO TO 100
      TAU=-FLOAT(IK)
C
C  CYCLE OVER DOMAINS
      ND=0
      ICHIR=1
      IHELIX=1
      DOCENT=CENTRC
      IF (MODUL)THEN
        PSICEN=(MSTAB(1).LT.0)
        CENPSI=.NOT. PSICEN
        IF (HELI) THEN
          IHELIX=2
C  POSSIBILITY OF CHIRALITY DOMAINS
          IF (IABS(IPROP).EQ.1) ICHIR=2
        ENDIF
      ELSE
C DON'T WANT PHASE SHIFTS
        PSI=.FALSE.
        CENPSI=.FALSE.
        PSICEN=.TRUE.
      ENDIF
      SKIP=.FALSE.
      DO 15 ICHI=1,ICHIR
      DO 15 IDOMOP=1,NOPC
      IF (FERO .AND. IDOMOP .NE.1) GO TO 18
C DOMAINS EXIST FOR ALL ELEMENTS NOT IN THE MAGNETIC GROUP
      IF (IDOMOP.NE.1 .AND. IABS(MSTAB(IDOMOP)).NE.IDOMOP) GO TO 15
C FMC COLLECTS THE MAGNETIC STRUCTURE FACTOR, A COMPLEX VECTOR,
C ZEROED BEFORE CALCULATING THE VALUE FOR EACH DOMAIN
      CALL CGMZER(FMC,1,3)
C FIRST SCATTERING FACTOR:
      IFF=0
C
C CYCLE OVER MAGNETIC ATOMS:
      DO 1 IM=1,NMAG
      IR=JMAGAT(IM)
C
C IS THERE A PHASE FACTOR ASSOCIATED WITH THE CENTRE OF SYMMETRY FOR THIS ATOM
      IF (CENPSI)  THEN
C IS THE ATOM ON THE CENTRE
        PSICEN=(ISGEN(1,IR).LT.0)
        IF (.NOT. PSICEN) NIPC=NPHI(IM)/2
        DOCENT=(CENTRC .AND. (.NOT. PSICEN))
      ENDIF
C
C JUMP IF FORM/SCATTERING FACTOR THE SAME AS BEFORE
      IF (NMFORM(IM) .EQ. IFF) GO TO 2
C IF NOT, GET IT
      IFF=NMFORM(IM)
      FORM=FORMFA(STHL,IFF)
C
C  CALCULATE FOR EACH COMPONENT OF THE HELIX IN TURN
   2  DO 30 ICOMP=1,IHELIX
      CALL CGMZER(SUM1,3,1)
C INNER LOOP OVER SYMMETRY EQUIVALENTS:
C PREPARE TO COUNT OPERATORS USED
      NFAC=0
      DO 3 IS=1,NOPC
C  ONLY USE OPERATORS WHICH LEAVE THE PROPAGATION DIRECTION INVARIANT
      IF (IS.NE.1 .AND.IABS(KSTAB(IS)).NE.1) GO TO 3
C  THE OPERATOR TO USE IS THE PRODUCT OF IS WITH THAT CREATING THE DOMAIN
      IROP=MULTAB(IDOMOP,IS)
C SKIP IF IROP DOESNT GENERATE A DISTINCT SUB-LATTICE
      IF (MODUL .AND. LPHI(IPTAB(IROP,IM),IM).NE.IROP) GO TO 3
      CALL ROTSYM(H,RH,IROP,-1)
      IF (IDOMOP .NE.1 .AND. MSTAB(IDOMOP).LT.0) CALL GMREV(RH,RH,3,1)
      F1=TWOPI*(SCALPR(X(1,IR),RH)+SCALPR(TRANS(1,IROP),H))
C  ANISOTROPIC T F (=1. IF NOT THERE) NEEDED SEPARATELY FOR LSQ:
      ERS=ANITF(RH,IR)
      ARS=COS(F1)*ERS
      BRS=SIN(F1)*ERS
      TERM=CMPLX(ARS,BRS)
C  FIND OUT WHAT THE SYMMETRY DOES TO THE SPIN DIRECTION
      CALL ROTMAG(SPIND(1,1,ICOMP,IM),SDOM,IS)
C  GET THE SPIN DIRECTION FOR THIS DOMAIN
      CALL ROTOSM(SDOM,RS,IDOMOP,1)
      CALL C1MSCA(RS,TVEC,TERM,3,1)
      IF (MODUL) THEN
C INCLUDE A PHASE SHIFT FOR THIS SUB-LATTICE IF NECESSARY
        IP=IPTAB(IROP,IM)
        TEST=TAU*(RADIAN(PHIH(IP,IM)))
C Changes made May 1994
C        TEST=TAU*(TPTAB(IROP,IM)+RADIAN(PHIH(IP,IM)))
C        IF (IROP.NE.1) TEST=TEST*FLOAT(KSTAB(IROP))
        PSI=(ABS(TEST).GT..0001)
        IF (PSI) PSIFAC=CEXP(CMPLX(0.,TEST))
      ENDIF
C IF CENTROSYMMETRIC, COMPENSATE FOR USING ONLY HALF NUMBER OF OPERATORS:
C  OTRSYM(25) SHOULD CONTAIN THE MATRIX RELATING THE MAGNETIC COMPONENTS
C  OF ATOMS RELATED BY THE CENTRE OF SYMMETRY, WHETHER THIS IS IN THE
C  MAGNETIC GROUP OR NOT.
      IF (DOCENT) THEN
        IF (.NOT. PSICEN) THEN
C Changed May 1994
          TEST=TAU*(RADIAN(PHIH(NIPC+IP,IM)))
C          TEST=TAU*(RADIAN(PHIH(NIPC+IP,IM))-TPTAB(IP,IM))
          PSIFCC=CEXP(CMPLX(0.,TEST))
        ENDIF
        CALL CENTRO(TVEC,TVEC,OTRSYM(1,1,25),PSIFAC,PSIFCC,PSI,PSICEN)
      ELSE
C IF CENTRIC THEN ATOM WAS ON THE CENTRE
        IF (CENTRC)CALL CMRSCA(TVEC,TVEC,2.0,3,1)
        IF (PSI) CALL CGMSCA(TVEC,TVEC,PSIFAC,3,1)
      ENDIF
      CALL CGMADD(SUM1,TVEC,SUM1,1,3)
C
C INCREMENT COUNT OF OPERATORS USED
      NFAC=NFAC+1
C END OF INNERMOST CYCLE OVER SYMMETRY
    3 CONTINUE
C
      FAC=AMULT(IR)*EXP(-TF(IR)*SSQRD)
C COMPENSATE FOR NOT USING ALL SYMMETRY ELEMENTS
      FACTOR=FLOAT(NOPC)/FLOAT(NFAC)
      FAC=FAC*FACTOR
C SCALE MOMENT TO CMS-12
      SM=SMOD(ICOMP,IM)*VALMUB
C HR IS PRODUCT OF ATOM DEPENDENT BUT SYMMETRY INDEPENDENT FACTORS
      HR=FAC*FORM*SITE(IR)
      TERM=HR*SM
C SHIFT THE PHASE OF THE PERPENDICULAR COMPONENT OF THE SPIRAL
      IF (ICOMP.EQ.2) THEN
        TERM=TERM*CMPLX(0.,TAU)
C  THE PHASE SHIFT IS NEGATIVE FOR REVERSE CHIRALITY
        IF (ICHI.EQ.2) TERM=-TERM
      ENDIF
      CALL CGMSCA(SUM1,TVEC,TERM,3,1)
      CALL CGMADD(FMC,TVEC,FMC,1,3)
   30 CONTINUE
C
C
   1  CONTINUE
C END OF CYCLE OVER ATOMIC POSITIONS
C
C  COMPENSATE FOR MULTIPLICITY OF THE STAR
        CALL CMRSCA(FMC,FMC,FKSTAR,3,1)
C
C NOW THE CYCLE FOR THE DOMAIN AVERAGE
   18 IF (IDOMOP .EQ.1 .OR. FERO) CALL MAGDOM(H,HD,IDOMOP,SKIP)
      CALL RCMPRD(HD,FMC,P,3,3,1)
      FMCSQR=FMCSQR+RSCALP(P,P)
      IF (.NOT.SKIP) ND=ND+1
      IF (FERO) THEN
C       PUT Q BACK INTO UNROTATED FRAME
        CALL CROTO(P,Q(1,ND),IS,1)
      ELSE
        CALL CGMEQ(P,Q(1,ND),3,1)
      ENDIF
   15 CONTINUE
C
C NOW THE DOMAIN AVERAGE
        FMCSQR=FMCSQR/FLOAT(ND)
        FMCMOD=SQRT(FMCSQR)
C
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE GENMAG(H,NOMORE,MUL,SMAX,NFLAG)
      SUBROUTINE GENMAG(H,NOMORE,MUL,SMAX,NFLAG)
C
C *** GENMAG updated by JCM 6 May 92 ***
C
CX
CC 17C
CH Generates the next set of magnetic h,k,l, scanning the asymmetric unit.
CA On entry NFLAG=-9999 if this is the very first entry
CA    if IPROP is non-zero this entry makes a magnetic asymmetric unit
CA    and sets things up for subsequent calls.
CA    otherwise NFLAG is left severely alone and just presented again
CA    on each subsequent entry.
CA On exit H is a real 1x3 array holding the next h,k,l (unless none)
CA         NOMORE is a LOGICAL saying whether or not no more generated
CA         MUL is the multiplicity of H
CP PROPER must have been called to fill in /SATELL/
C
      LOGICAL NOMORE
      DIMENSION H(3),HT(3)
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
      COMMON /TEMGEN/HF(3)
C
      IF (NFLAG .EQ. -9999) THEN
C INITIAL ENTRY:
C
        IF (NKSTAR.GT.1) THEN
          CALL SUBSYM(KSTAB)
          CALL SYMUNI
        ENDIF
C  INCREASE SINTHETA LIMIT TO ALLOW FOR PROPAGATION VECTOR
        SDIF=VCTMOD(0.5,PROP,2)
        SLIM=SMAX+SDIF
        CALL SETGEN(SLIM)
C FIRST PRESENT 0,0,0 AS FUNDAMENTAL:
        CALL GMZER(HF,1,3)
        NOMORE=.FALSE.
C
C ENTRIES OTHER THAN INITIAL - EXPECT NFLAG SET:
      ELSE IF (NFLAG.EQ.0) THEN
        CALL GETGEN(HF,NOMORE)
        IF (NOMORE) GO TO 100
      ELSE
        CALL GMSUB(HF,PROP,H,3,1)
        NFLAG=0
        GO TO 3
      ENDIF
C  ADD STAR VECTOR TO FUNDAMENTAL
      CALL GMADD(HF,PROP,H,3,1)
C  TAKE CARE OF SPECIAL CASES WHEN PROP IS HALF A RLV
      IF (IABS(IPROP).EQ.2) THEN
        CALL GMADD(H,PROP,HT,3,1)
        M=MULBOX(HT)
        IF (M.NE.0) NFLAG=0
      ENDIF
      NFLAG=1
   3  CALL ASUNIT(H,HT,N,MUL)
      CALL GMEQ(HT,H,3,1)
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE INPUTQ(ID,NTYP,IAT,IPT,IER)
      SUBROUTINE INPUTQ(ID,NTYP,IAT,IPT,IER)
C
C *** INPUTQ updated by PJB Aug 91 ***
C
CX
CC 17A
CH Reads individual "Q" cards
CA On entry ID is the position in the CDF of the "Q" card, or 0 (= card present)
CA On exit NTYP = a number indicating what kind of "Q" card was read:
CA      NTYP=1 for STYP (magnetic structure type)
CA      NTYP=2 for PROP the propagation vector
CA      NTYP=3 for MSYM a magnetic symmetry operator
CA      NTYP=4 for NSYM a non-symmetric rotation
CA      NTYP=5 for FORM (a form factor label should be read first).
CA      NTYP=6 for MU   (Magnetic moment: an atom label should be read first).
CA      NTYP=7 for SDIR (Spin directions: an atom label should be read first).
CA      NTYP=8 for PSI  (Relative phases of helices: an atom label should be
CA                       read first).
CA On exit IAT = number corresponding to the atom or form factor label,
CA               if one was read.
CA On exit IPT = the next column on the card to be interpreted.
CA On exit IER = the error indicator, = 0 for no error.
C
      CHARACTER*4 WORD,WORD1,QWORD(8)
      COMMON /FONAM/FONA(20,9),FONAME(20)
      CHARACTER *4 FONAME,FONA
      COMMON /FORMDA/NFORMF(150),MODE(20),NT(20),F(40,20),
     & S(40,20),CMULT(20),KCMULT(150),NBAKF(20),
     & NUMFNM,KOM7
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA NWRD/8/
      DATA QWORD/'STYP','PROP','MSYM','NSYM','FORM','MU','SDIR','PSI'/
C
C SET NO ERROR
      IER=0
C
C IF ID=0, EXPECT CARD ALREADY PRESENT:
      IF (ID.NE.0) CALL CARDIN(ID)
      CALL RDWORD(WORD,LWORD,3,IPT,80,0,IE)
      IF (IE.EQ.0) THEN
        IAT=IATOM(WORD)
        IF (IAT.EQ.0) THEN
          NTYP=NCFIND(WORD,QWORD,NWRD)
          IF (NTYP.NE.0) GO TO 100
        ENDIF
        IPT1=IPT
        CALL RDWORD(WORD1,LWORD,IPT1,IPT,80,0,IE)
        NTYP=NCFIND(WORD1,QWORD,NWRD)
        IF (NTYP.EQ.0) THEN
        CALL ERRCH2(WORD,-2,'Word on "Q" card',
     &   'is neither an atom name nor an allowed Q word')
          IER=IER+1
        ELSE
C%
C          IF (NTYP.EQ.5)IAT=LMATCH(WORD,FONAME,NUMFNM,%FORM%)
          IF (NTYP.EQ.5)IAT=LMATCH(WORD,FONAME,NUMFNM,20)
          GO TO 100
        ENDIF
      ELSE
        CALL ERRCH2(ICARD(3:6),-2,'cannot recogise word',
     &   'on "Q" card')
        IER=IER+1
      ENDIF
C
  100 RETURN
      END
C
C
C


C
C
C LEVEL 3      SUBROUTINE LOGMAG
      SUBROUTINE LOGMAG
C
C *** LOGMAG by JCM 27 Aug 91 ***
C
CX
CC 17A
CH Sets mnemonic logicals from the type of magnetic structure.
CA On entry MTYP gives the type of structure, usually read from a Q STYP
CA               card.
CD Complains and stops if MTYP is not between 1 and 5.
CD Sets only one of FERO, FERA, HELI, AMOD and ANTI to be TRUE and the rest
CD to be FALSE.  Sets MODUL to be TRUE if either HELI or AMOD.
C
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
C
      IF (MTYP .LT. 1 .OR. MTYP .GT. 5) CALL ERRMES(1,0,
     &  'Type of magnetic structure not recognised')
      FERO=MTYP.EQ.1
      FERA=MTYP.EQ.2
      HELI=MTYP.EQ.3
      AMOD=MTYP.EQ.4
      ANTI=MTYP.EQ.5
      MODUL=HELI .OR. AMOD
      RETURN
      END
C
C
C
C
C LEVEL 4      LOGICAL FUNCTION MAGABS(H,IK)
      LOGICAL FUNCTION MAGABS(H,IK)
C
C *** MAGABS modified by PJB Jan 91 ***
CX
CC 17B
CH Tests for systematic absence of magnetic reflections.
CA On entry H is a 1x3 array holding h,k,l
CA On exit MAGABS = .TRUE. if absent, .FALSE. if present
CA         IK is +1 if the indices correspond to a r.l.v + the
CA            propagation vector ie h=g+k
CA         IK is -1 if h=g-k
CA         IK is 0 if this is irrelevant.
CP The space group symmetry should have been set up by SYMOP
CP The propagation vector PROP should have been set up by DOMAG
C
      DIMENSION H(3),TEST(3)
      LOGICAL M,LATABS
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
C
      IK=0
      IF (IPROP.EQ.0) THEN
        M=LATABS(H)
      ELSE
        CALL GMADD(H,PROP,TEST,3,1)
        M=LATABS(TEST)
        IF (M) THEN
          CALL GMSUB(H,PROP,TEST,3,1)
          M=LATABS(TEST)
          IF (.NOT. M) IK=1
        ELSE
          IK=-1
        ENDIF
      ENDIF
      MAGABS=M
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE MAGCNC
      SUBROUTINE MAGCNC
C
C *** MAGCNC uodated by PJB 17-Jan-95 ***
C
CX
CC 17A
CH To find magnetic constraints in a non-least squares calculation.
CP The magnetic symmetry must have been set up by calls to SYMOP and
CP DOMAG
C
C%
C      DIMENSION IPSFIX(%PSIS%)
      DIMENSION IPSFIX(4)
      LOGICAL LMFIX(3),FIRST
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
C%
C
      FIRST=.TRUE.
      DO 1 MAT=1,NMAG
C IF FERROMAGNETIC NO CONSTRAINTS
      IF (.NOT. (FERO .OR. FERA)) CALL MAGCON(MAT,LMFIX,FIRST)
C SET UP SPHERICAL POLARS:
      CALL SPHPOL(ANGM(1,MAT),ANGM(2,MAT),SPIND(1,1,1,MAT),1)
      IF (MODUL) THEN
        CALL PSICON(MAT,IPSFIX)
        IF (HELI) THEN
          CALL SPHPOL(ANGM(3,MAT),ANGM(4,MAT),SPIND(1,1,2,MAT),1)
          CALL SPHELI(MAT,0)
        ENDIF
      ENDIF
    1 CONTINUE
      RETURN
      END
C
C

C
C
C LEVEL 4      SUBROUTINE MAGCON(IATO,LMFIX,FIRST)
      SUBROUTINE MAGCON(IATO,LMFIX,FIRST)
C
C *** MAGCON corrected by PJB 4 30-Nov-1994 ***
C
CX
CC 17A
CH Finds and reports the symmetry constraints on magnetic parameters.
CA IATO is the number of the magnetic atom in question
CA LMFIX(I) is set to TRUE if parameter I for this atom is fixed by symmetry
CA FIRST is TRUE if no constraints have yet been found
C
      EXTERNAL DUMMY
      LOGICAL FIRST,LMFIX(3),NONE
C%
C      DIMENSION RMAT(3,3),NFIX(3),FIX(3),IATAB(%SYMO%),RX(3),SCON(3)
      DIMENSION RMAT(3,3),NFIX(3),FIX(3),IATAB(24),RX(3),SCON(3)
       DIMENSION VEC(3),TVEC(3,2)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22

C
      IAT=JMAGAT(IATO)
C  CHECK THAT STRUCTURE AND SYMMETRY ARE COMPATIBLE WITH THE
C  PROPAGATION VECTOR
      CALL MAKGRP(ISGEN(1,IAT),IATAB,0,DUMMY)
      DO 28 N=2,NOPC
      IF (IABS(KSTAB(N)).NE.1 .AND. IATAB(N).EQ.0) THEN
        WRITE (ITO,2800) N
        WRITE (LPT,2800) N
 2800   FORMAT (/' ERROR ** Structure symmetry is reduced by ',
     &   'that of the propagation vector (operator #',I2,').'/
     &  ' **** Change S cards ****')
C>> JCC added
        CALL BMBOUT
	  RETURN
      ENDIF
   28 CONTINUE
C
      CANG1=ANGM(1,IATO)
      CANG2=ANGM(2,IATO)
      DO 29 J=1,3
      LMFIX(J)=.FALSE.
  29  NFIX(J)=9999
      N=ISGEN(1,IAT)
C  JUMP IF NOT SPECIAL
      NONE = N.EQ.1
      IF (NONE) GO TO 17
C
      IF (N.LT.0 .AND. MSTAB(1).LT.0) THEN
C CENTRE OF SYMMETRY PRESENT: IF NOT AT ORIGIN WHAT
C LATTICE VECTOR IS INVOLVED
        CALL GMREV(X(1,IAT),TVEC,1,3)
        CALL GMADD(X(1,IAT),TVEC,VEC,3,1)
   51   PH=SCALPR(PROP,VEC)
C  CHECK THE THE PHASE SHIFT IS n*PI
        IF (ABS(FLOAT(NINT(2.*PH))-2.*PH) .GT..001) THEN
          CALL ERRCH2(ATNAME(IATO),1,
     & 'The centre of symmetry is not consistent with a moment on',
     & 'for this propagation vector')
C  CHECK ALSO THAT IT IS NON-REVERSING
        ELSE
          IF (NINT(COS(TWOPI*PH))-MTSYM(25) .NE. 0) THEN
C  CAN HAVE NO MOMENT:
            NFIXED=3
            DO 2 I=1,3
   2        LMFIX(I)=.TRUE.
            SMOD(1,IATO)=0.
            CANG1=0.
            CANG2=0.
            GO TO 17
          ENDIF
        ENDIF
      ENDIF
C
C  LOOK TO SEE WHICH ELEMENTS OF THE ATOM SUB-GROUP ARE
C  IN THE MAGNETIC SYMMETRY GROUP
      DO 3 K=2,NOPC
      IF (IATAB(K).EQ.0) GO TO 3
      CALL GMEQ(OTRSYM(1,1,K),RMAT,3,3)
C  REVERSE THE MATRIX IF OPERATION IS COMBINED WITH INVERSION, AND
C  THE INVERSION IS TIME-REVERSING
      IF (IATAB(K) .LT.0 .AND. MTSYM(25).LT.0)
     &  CALL GMREV(RMAT,RMAT,3,3)
C IN CASE OF SHIFT BY A LATTICE VECTOR:
      IF (IPROP .NE. 0) THEN
        CALL ROTSYM(X(1,IAT),RX,K,1)
        CALL GMADD(RX,TRANS(1,K),RX,3,1)
        CALL GMSUB(RX,X(1,IAT),RX,3,1)
        PH=SCALPR(PROP,RX)
C  CHECK THE THE PHASE SHIFT IS n*PI
        IF (ABS(FLOAT(NINT(2.*PH))-2.*PH) .GT..001) THEN
          CALL ERRIN2(K,-1,'Operator',
     & 'is not consistent with a moment on'//ATNAME(IATO)//
     & 'for this propagation vector')
C  IF AN ODD MULTIPLE OF PI REVERSE THE MATRIX
          IF (ABS(FLOAT(NINT(PH))-PH) .GT..3)
     &    CALL GMREV(RMAT,RMAT,3,3)
        ENDIF
      ENDIF
      CALL RELSM3(RMAT,NFIX,FIX)
    3 CONTINUE
C
C  NOW CONVERT XYZ CONSTRAINTS TO SPHERICAL POLAR ONES
C  COUNT FIXED PARS
      NFIXED=0
      NREL=0
      DO 5 I=1,3
      IF (NFIX(I).EQ.0) THEN
        NFIXED=NFIXED+1
      ELSE
C CHECK THAT THIS 9999 IS STILL OK:
        IF (NFIX(I).LT.9999) NREL=NREL+1
      ENDIF
    5 CONTINUE
C
      GO TO (10,11,12,13) NFIXED+1
C
C ALL FIXED
   13 DO 6 I=1,3
   6  LMFIX(I)=.TRUE.
      GO TO 17
C
C  TWO FIXED - FIND WHICH
   12 IF (NFIX(1).EQ.0 .AND. NFIX(2).EQ.0) THEN
        CANG1=0.
      ELSE
        CANG1=90.
      ENDIF
      LMFIX(1)=.TRUE.
    7 IF (NFIX(1).EQ.0) CANG2=90.
      IF (NFIX(2).EQ.0) CANG2=0.
      LMFIX(2)=.TRUE.
      GO TO 10
C
C  ONE FIXED
   11 IF (NFIX(3).NE.0) GO TO 7
      CANG1=90.
      LMFIX(1)=.TRUE.
C
C  NOW RELATIONSHIPS
   10 GO TO (8,8,15,16) NREL+1
C NONE
    8 NONE = NFIXED.EQ.0
      GO TO 17
C
C  ALL THREE RELATED - FIX BOTH ANGLES
   16 CANG2=ATAN2(FIX(1),FIX(2))
      CANG1=DEGREE(ATAN2(FIX(1)*COS(CANG2),FIX(3)))
      LMFIX(1)=.TRUE.
      GO TO 14
C
C  IF TWO PARS RELATED THEY SHOULD BE 1 AND 2 IF THE THIRD IS FREE
   15 IF (NFIX(3).NE.9999 .AND. NFIXED.NE.1) THEN
        WRITE (LPT,3000) NFIX
        WRITE (ITO,3000) NFIX
 3000   FORMAT (' *** PROGRAM ERROR IN MAGCON ***'/' NFIX =',3I5/
     &   ' I thought this couldn''t happen! PJB')
        CALL BMBOUT
	  RETURN
      ENDIF
C
      GO TO (20,17,21,20) NFIX(1)+1
C
   21 CANG2=ATAN2(FIX(1),FIX(2))
  14  LMFIX(2)=.TRUE.
      CANG2=DEGREE(CANG2)
      GO TO 17
   20 CANG1=DEGREE(ATAN2(FIX(3),FIX(NFIX(3))))
      LMFIX(1)=.TRUE.
C
   17 IF (FIRST) CALL MESS(LPT,1,
     & 'Symmetry constraints on magnetic moments:')
      FIRST=.FALSE.
      WRITE (ICARD,2001) ATNAME(IAT)
2001  FORMAT (1X,A4,' None')
      IF (.NOT. NONE) THEN
C
C  CHOOSE BEST DIRECTION
        CALL SPHPOL(CANG1,CANG2,SOLD,1)
        CALL SPHPOL(ANGM(1,IATO),ANGM(2,IATO),SCON,1)
        IF (SCALPR(SCON,SOLD) .LT. 0.) THEN
          ANGM(1,IATO)=ARANGE(180.-CANG1,180.,-180.)
          ANGM(2,IATO)=ARANGE(180.+CANG2,180.,-180.)
        ELSE
          ANGM(1,IATO)=CANG1
          ANGM(2,IATO)=CANG2
        ENDIF
        J=6
        IF (LMFIX(3)) THEN
          WRITE (ICARD(J:),2003) 'MU',SMOD(1,IATO)
          J=J+16
        ENDIF
        IF (LMFIX(1)) THEN
          WRITE (ICARD(J:),2002) 'THET',ANGM(1,IATO)
          J=J+16
        ENDIF
        IF (LMFIX(2)) THEN
          WRITE (ICARD(J:),2002) 'PHI',ANGM(2,IATO)
          J=J+16
        ENDIF
 2002   FORMAT (1X,A4,' =',F9.2)
 2003   FORMAT (1X,A4,' =',F9.4)
      ENDIF
      CALL MESS(LPT,0,ICARD)
C  THE CONSTRAINTS ARE THE SAME FOR BOTH COMPONENTS OF A HELIX
      IF (HELI) THEN
        IF (LMFIX(1)) ANGM(3,IATO)=ANGM(1,IATO)
        IF (LMFIX(2)) ANGM(4,IATO)=ANGM(2,IATO)
        IF (LMFIX(3)) SMOD(2,IATO)=SMOD(1,IATO)
      ENDIF
 100  RETURN
      END
C
C

C
C
C LEVEL 3      SUBROUTINE MAGDOM(H,HK,IOP,SKIP)
      SUBROUTINE MAGDOM(H,HK,IOP,SKIP)
C
C *** MAGDOM by PJB Apr 87 ***
C
CX
CC 17B
CH In Least Squares refinement with magnetic scattering, forms the matrix
CH needed for derivatives of Q with respect to a spin direction.
CA On entry H is the scattering vector
CA          IOP is the symmetry operator to be used on H
CA On exit the 3x3 matrix HK contains the required matrix.
CA         LOGICAL SKIP is set to indicate
CP MTYP must indicate the type of magnetic structure.
CP NORD, MSTAB and KSTAB entries for IOP must be set.
CD  HK is formed, such that Q=KxSxK=S[HK]
C
      DIMENSION H(3),RH(3),OH(3),HK(3,3)
      LOGICAL SKIP
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
C  FOR FERROMAGNETIC USE THE WHOLE GROUP
      IF (FERO) GO TO 2
C
      IF (IOP.NE.1 .AND. (MSTAB(IOP).NE.1.OR.KSTAB(IOP).NE.1))THEN
        SKIP = .TRUE.
        GO TO 100
      ENDIF
    2 SKIP=.FALSE.
C
      CALL ROTSYM(H,RH,IOP,2)
C  REVERSE IF IMPROPER ROTATION
      IF (NORD(IOP).LT.0) CALL GMREV(RH,RH,3,1)
      CALL ORTHO(RH,OH,2)
      CALL UNIVEC(OH,D)
      J=2
      K=3
      DO 1 I=1,3
      HK(I,I)=OH(J)*OH(J)+OH(K)*OH(K)
      HK(I,J)=-OH(I)*OH(J)
      HK(I,K)=-OH(I)*OH(K)
      J=K
      K=I
    1 CONTINUE
C
  100 RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE MAGSYM(MODE)
      SUBROUTINE MAGSYM(MODE)
C
C *** MAGSYM updated by PJB 31-May-1994 ***
C
CX
CC 17B
CH Routine with 4 named entry points, MAGSYM, MELIN, NELIN and ROTMAG, to deal
CH generally with magnetic symmetry.  MAGSYM sets magnetic symmetry, MELIN puts
CH in an operator for a generator, NELIN puts in non-symmetric rotation and
CH ROTMAG rotates with a magnetic operator.
CA On entry to MAGSYM MODE = 0 to initialise
CA                           1 to generate the remaining operators and the
CA                             orthogonal spin rotations in OTRSYM
CA On entry to MELIN  IOP = which operator
CA                    VAL = + or - 1, its value
CA On entry to ROTMAG S is the 3x3 array to be rotated
CA                    IOP = which operator
CA On exit from ROTMAG RS is the 3x3 rotated array
CA On entry to NELIN  IOP = which operator
C                     SROT = the rotation matrix for the spin
C
      EXTERNAL MTPROD
      DIMENSION S(3,3),RS(3,3),TEMP(3,3),SROT(3,3),MGEN(3)
C%
C      DIMENSION NSTAB(%SY+1%),TMPSYM(%SYMO%),MJTAB(%SYMO%)
      DIMENSION NSTAB(25),MJTAB(24)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
C
      IF (MODE.EQ.1) GO TO 20
C
C  INITIALISE
      CALL JGMZER(MTSYM,1,NOPC)
      CALL JGMZER(MSTAB,1,NOPC)
      GO TO 100
C
C  ENTRY TO PUT IN ONE MAGNETIC SYMMETRY OPERATOR
      ENTRY MELIN(IOP,VAL)
      IO=IABS(IOP)
      IF (ABS(VAL).LT..0001) THEN
        CALL ERRIN2(IOP,1,'Zero value for magnetic symmetry operator',
     &  ' ')
        GO TO 100
      ENDIF
C  RECORD WHETHER THE OPERATOR INVOLVED INVERSION
      IF (IOP.LT.0) THEN
        MSTAB(IO)=ISIGN(2,IFIX(VAL))
      ELSE
        MSTAB(IO)=ISIGN(1,IFIX(VAL))
      ENDIF
      GO TO 100
C
C  ENTRY TO ADD A NON-SYMMETRIC ROTATION
      ENTRY NELIN(IOP,SROT)
      IO=IOP
      IF (IOP.EQ.-1) IO=25
      CALL GMEQ(SROT,OTRSYM(1,1,IO),3,3)
C  MARK NON-SYMMETRIC OPERATORS
      IF (IOP.EQ.-1) IOP=1
      MSTAB(IOP)=100
      GO TO 100
C
C FORM THE REST OF THE MAGNETIC OPERATORS FROM THE MULTIPLICATION TABLE
   20 DO 21 IO=1,NOPC
      IF (MSTAB(IO).EQ.100) THEN
        NSTAB(IO)=1
        MSTAB(IO)=0
      ELSE
        NSTAB(IO)=0
      ENDIF
   21 CONTINUE
      IF (FERO .OR. FERA) THEN
        CALL GMUNI(OTRSYM(1,1,25),3)
        GO TO 100
      ENDIF
      IF (CENTRC) THEN
        IF (MSTAB(1).EQ.0) THEN
          CALL MESS(LPT,1,'No centre of symmetry in the magnetic group')
          IF (NSTAB(1).EQ.0) THEN
            CALL ERRMES(1,2,
     &   'No spin rotation given for centrosymmetrically related atoms')
          ELSE
            WRITE (LPT,2031) ((OTRSYM(I,J,25),I=1,3),J=1,3)
 2031       FORMAT (/' Spin rotation for centro-symmetrically related',
     &      ' atoms is: ',3F6.2,2(/58X,3F6.2))
          ENDIF
        ELSE
          CALL GMUNI(OTRSYM(1,1,25),3)
          MSTAB(1)=ISIGN(1,MSTAB(1))
          IF (MSTAB(1).LT.0) CALL
     &    GMREV(OTRSYM(1,1,25),OTRSYM(1,1,25),3,3)
          MTSYM(25)=MSTAB(1)
        ENDIF
      ENDIF
C CONSTRUCT THE MAGNETIC SUB-GROUP
      MGEN(1)=-1
      IF (MSTAB(1).EQ.0) MGEN(1)=1
      NGEN=1
      DO 40 I=2,NOPC
      IF (MSTAB(I).EQ.0) GO TO 40
      NGEN=NGEN+1
      MGEN(NGEN)=(3-2*(IABS(MSTAB(I))))*I
      MTSYM(I)=ISIGN(1,MSTAB(I))
      IF (NGEN.EQ.3) GO TO 41
   40 CONTINUE
      MGEN(3)=1
      IF (NGEN.EQ.1) MGEN(2)=1
   41 CALL MAKGRP(MGEN,MJTAB,1,MTPROD)
      NMSYM=(MGEN(1))
C NUMBER OF ORIENTATION DOMAINS
      NDOM=NOPC/NMSYM
      MJTAB(1)=MSTAB(1)
C
C
   28 WRITE (LPT,2001) (I,MTSYM(I),I=1,NOPC)
 2001 FORMAT (/' Magnetic symmetry operators: ',12(I3,' =',I3)/31X
     & ,12(I3,' =',I3))
      IF (CENTRC) WRITE (LPT,2002) MSTAB(1)
 2002 FORMAT (/31X,' Centre of symmetry =',I3)
C
C  PUT SPIN ROTATIONS ONTO ORTHOGONAL AXES
C USE OTRSYM(,,1) TEMPORARILY FOR TRANSPOSE:
      CALL GMEQ(ORTH(1,1,1),OTRSYM(1,1,1),3,3)
      CALL TRANSQ(OTRSYM(1,1,1),3)
      DO 27 NO=2,NOPC
      IF (MTSYM(NO).EQ.0) GO TO 27
      CALL GMPRD(SYM(1,1,NO),ORTH(1,1,2),TEMP,3,3,3)
      IF (MTSYM(NO)*NORD(NO).LT.0.) CALL GMREV(TEMP,TEMP,3,3)
      CALL GMPRD(OTRSYM(1,1,1),TEMP,OTRSYM(1,1,NO),3,3,3)
   27 CONTINUE
      CALL GMUNI(OTRSYM(1,1,1),3)
C
      CALL FACTGP(MJTAB,MSTAB,NFAC)
      WRITE (LPT,2003) (I,MSTAB(I),I=1,NOPC)
 2003 FORMAT (/' Magnetic symmetry table : ',12(I3,' =',I3)/28X
     & ,12(I3,' =',I3))
C
C  NOW DEAL WITH NON-SYMMETRIC ROTATIONS
      DO 43 NO=2,NOPC
      IOPP=IABS(MSTAB(NO))
      IF (IOPP .EQ. 1) GO TO 43
      IF (IOPP.EQ.NO) THEN
        IF (NSTAB(NO).NE.1) THEN
          CALL ERRIN2(NO,1,
     &    'Missing non-symmetric spin rotation for element number',' ')
        ELSE
          WRITE(LPT,2004) NO,((OTRSYM(I,J,NO),I=1,3),J=1,3)
 2004     FORMAT (/' Non-symmetric spin rotation for element number',
     &    I3,' is:  ',3F6.2,2(/56X,3F6.2))
        ENDIF
      ELSE
        IX=MULTAB(NO,INVERS(IOPP))
        IF (MSTAB(IX) .EQ. 0) CALL ERRMES(-1,0,
     & 'Logical error in MAGSYM - bad tables')
C
        CALL GMPRD(OTRSYM(1,1,IX),OTRSYM(1,1,IOPP),
     &  OTRSYM(1,1,NO),3,3,3)
      ENDIF
      IF (MSTAB(NO).LT.0)
     & CALL GMREV(OTRSYM(1,1,NO),OTRSYM(1,1,NO),3,3)
   43 CONTINUE
      GO TO 100
C
C  ENTRY TO OPERATE WITH MAGNETIC SYMMETRY
      ENTRY ROTMAG(S,RS,IOP)
C  DONT ROTATE IF FERROMAGNETIC
      IF (FERO .OR. FERA) THEN
        CALL GMEQ(S,RS,3,3)
      ELSE
        DO 30 I=1,3
        CALL GMPRD(OTRSYM(1,1,IOP),S(1,I),RS(1,I),3,3,1)
   30   CONTINUE
      ENDIF
      GO TO 100
C
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE MAGVAR(IG,IS,NV)
      SUBROUTINE MAGVAR(IG,IS,NV)
C
C *** MAGVAR updated by JCM 3 Jun 92 ***
C
CX
CC 17B
CH Records the initial fixing, or subsequent varying of magnetic parameters.
CA On entry IG is the genus of the parameter (which atom)
CA          IS is the species (13-16 PHIH, 17-20 ANGM, 21,22 SMOD)
CA          NV is 0 if the while lot of parameters are to be initially fixed
CA                the number of the variable, if it is to be varied.
C
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
C
      IF (NV .EQ. 0) THEN
C ENTRY 0 IS TO FIX ALL PARAMETERS:
C
C COUNT ALL MAGNETIC ATOMS:
        DO 4 I=1,NMAG
        DO 3 J=1,2
   3    KSMOD(J,I)=0
        DO 1 J=1,4
   1    KANGM(J,I)=0
C%
C        DO 2 J=1,%PSIS%
        DO 2 J=1,4
   2    KPHIH(J,I)=0
   4    CONTINUE
      ELSE
C RECORD ONE VARIABLE:
        IM=MAGAT(IG)
        IF (IS .GE. 13 .AND. IS .LE. 16) KPHIH(IS-12,IM)=NV
        IF (IS .GE. 17 .AND. IS .LE. 20) KANGM(IS-16,IM)=NV
        IF (IS .GE. 21 .AND. IS .LE. 22) KSMOD(IS-20,IM)=NV
      ENDIF
C
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE MF5ADD(ISPC,IG,IS,N)
      SUBROUTINE MF5ADD(ISPC,IG,IS,N)
C
C *** MF5ADD by JCM 13 Jun 88 ***
C
CX
CC 18B
CH A specialist routine to deal with the refinement of multipoles.
CH Converts each type of LSQ family 5 (multipoles) addressing to the other.
CA On entry N=1  requests "give answer in in ISPC genus 1 from IG,IS"
CA          N=-1 requests "give answers as IG, IS from species ISPC"
CA So for N=1 IG, IS are set on entry to genus and species
CA    for N=-1, ISPC is set on entry to contain "species" in one long genus.
CP Table MPNAM must have been set up to contain species names, and
CP       MPTAB to point in it for each genus of family 5.
C
      COMMON /MPODA/NMPAT,NMPOL,MPATAB(20),MPNMTB(150),
     & NCLUMP,KCLUMP(100),MPTAB(21),POLAMP(200,6),KPOLMP(200),
     & NCMAT,CONMAT(600,2)
C
      IF (N .LT. 0) GO TO 1
C IG, IS GIVEN ; SET ISPC:
      ISPC=MPTAB(IG)+IS
      GO TO 100
C
C ISPC GIVEN; SET IS,IG:
   1  DO 2 I=1,NMPAT
      IF (MPTAB(I+1) .GT. ISPC) GO TO 3
   2  CONTINUE
C
C>> JCC      STOP 'ERROR ** IN MF5ADD'
      CALL BMBOUT
	RETURN
   3  IG=I
      IS=ISPC-MPTAB(I)+1
 100  RETURN
      END
C
C

C
C
C LEVEL 1      SUBROUTINE PROPAG(MODE,INOUT)
      SUBROUTINE PROPAG(MODE,INOUT)
C
C 17B
C *** PROPAG new by PJB 28-Sept-93 ***
C
CH Multiple entry subroutine for propagation vector refinement
CA MODE indicates what function is required
CA MODE = 1 Read a Q PROP card if present and if so call PROPER
CA          on exit INOUT is 1 id found 0 if not
CA      = 2 Set the symmetry constraints on the magnetic propagation vector
CA          INOUT is set on input to the offset of the propagation vector in
CA          the family 1 genus 1 parameters
CA      = 3 Apply a shift to the INOUTth component
CA      = 4 Write new Q PROP card on unit INOUT
CA      = 0 all components fixed
CA      = negative integer set the variable number of the -MODEth component to
CA       be INOUT
C
      DIMENSION ITAB(24),JGEN(3),NFIX3(3),RMAT(3,3),FIX3(3),NCOUNT(3)
C
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMTAB/MULTAB(24,24),INVERS(24),
     & NORD(24),IGEN(3),KOM22
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
      IF (MODE.LE.0) GO TO 60
      GO TO (10,20,30,40), MODE
C
C READ Q PROP CARD
   10 CALL FINDCD('Q','PROP',4,0,LCD)
      IF (LCD.GT.0) THEN
        CALL RDNUMS(FIX3,7,3,NUM,IER)
        IF (IER.NE.0 .OR. NUM.NE.3)
     &  CALL ERRMES(1,1,'reading propagation vector')
        CALL PROPER(FIX3)
        INOUT=1
      ELSE
        INOUT=-1
      ENDIF
      GO TO 100
C
C SET SYMMETRY CONSTRAINTS
   20 IPOFF=INOUT
C CLEAR OUT ALL FIX/RELA INFO
      DO 22 K=1,3
      NFIX3(K)=9999
  22  CONTINUE
C
      IF (IPROP.LE.0 .OR. IPROP.EQ.2) GO TO 23
      CALL JGMZER(ITAB,1,NOPC)
      ITAB(1)=1
      JGEN(1)=1
      DO 21 I=2,NOPC
      IF (KSTAB(I).NE.1) GO TO 21
      ITAB(I)=IABS(NORD(I))
      JGEN(1)=JGEN(1)+1
   21 CONTINUE
C
C Find generators of subgroup
      CALL GENELM(ITAB,JGEN)
C
C JUMP IF NOT SPECIAL:
      IF (JGEN(1) .EQ. 1) GO TO 26
C JUMP IF NOT ON A CENTRE OF SYMMETRY:
      IF (JGEN(1) .GT. 0) GO TO 24
C FIX ALL COMPONENTS:
  23   DO 25 I=1,3
  25  CALL FIXPAR(I,NFIX3)
      GO TO 26
C
C TAKE FIRST (OF POSSIBLE 2) SYMMETRY ELEMENTS MAKING THIS POSITION SPECIAL:
  24  DO 27 I=2,3
      K=IABS(JGEN(I))
      CALL GMEQ(SYM(1,1,K),RMAT,3,3)
      IF (JGEN(I) .LT. 0) CALL GMREV(RMAT,RMAT,3,3)
      CALL RELSM3(RMAT,NFIX3,FIX3)
C
C IS THERE A SECOND GENERATOR OF THE SUB-GROUP WHICH MAKES THIS ATOM SPECIAL?
      IF (JGEN(3) .EQ. 0) GO TO 26
  27  CONTINUE
C ALL COMPONENT RELATIONS COLLECTED IN TEMPORARY SPACE - USE:
  26  DO 28 I=1,3
  28  NCOUNT(I)=KPAK(1,1,IPOFF+I,JPHASE,1)
      CALL FIXREL(3,NFIX3,FIX3,NCOUNT,5)
      GO TO 100
C
C APPLY SHIFT
   30 IF (INOUT.GT.3) GO TO 100
      CALL ADJUST(PROP(INOUT))
      GO TO 100
C
C WRITE NEW Q PROP CARD
   40 IF (ICARD(3:6).NE.'PROP') THEN
        L=LENGT(ICARD)
        WRITE (INOUT,4001) (ICARD(I:I),I=1,L)
 4001 FORMAT (80A1)
      ELSE
        WRITE (INOUT,4000) PROP
 4000 FORMAT ('Q PROP',3F10.4)
      ENDIF
      GO TO 100
C
C TO SET ALL COMPONENTS FIXED, OR VARY ONE:
   60 N=IABS(MODE)
      IF (N .EQ. 0) THEN
        DO 1 I=1,3
   1    KPROP(I)=0
      ELSE
        IF (N.GT.3) GO TO 100
        KPROP(N)=INOUT
      ENDIF
C
 100  RETURN
      END
C
C

C
C
C
C LEVEL 5      SUBROUTINE PROPER(AKVEC)
      SUBROUTINE PROPER(AKVEC)
C
C *** PROPER updated by PJB 14-Dec-1994 ***
C
CX
CC 17A
CH Determines whether the satellites generated by the propagation vector
CH PROP have integer indices, and generates its "star".
C
CA On entry AKVEC is the 1X3 propagation vector copied to PROP
CA There is an ENTRY KSTAR(AKVEC,BKSTAR) to return the vectors AKSTAR
CA of the star in BKSTAR as well as filling in the common /SATELL/
CP SYMOP should have been obeyed to read the space group symmetry
CD On exit  IPROP in /SATELL/ is zero if PROP is 0 0 0
CD                negative for other integer indices
CD                and positive for non-integer values.
CD        IABS(IPROP) is set to 2 if twice PROP is a reciprocal
CD                lattice vector.
CD On exit the symmetry table of the star is in KSTAB.
CD NKSTAR holds the number of vectors in the star,
CD INCOM is .TRUE. if the propagation vector is fixed on a symmetry point
CD and FKSTAR is a scale for structure factors assuming a mono-k domain.
CD The ENTRY KSTAR not only writes the common /SATELL/ but also returns the
CD vectors AKSTAR of the star.
CO Writes its findings on unit LPT
C
      DIMENSION AKVEC(3),AKSTAR(3,24),RPROP(3),BKSTAR(1)
      LOGICAL KSTARS
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
C
      KSTARS=.FALSE.
      GO TO 2
C
      ENTRY KSTAR(AKVEC,BKSTAR)
      KSTARS=.TRUE.
C
    2 CALL GMEQ(AKVEC,PROP,3,1)
      IPROP=0
      IF (ABS(PROP(1))+ABS(PROP(2))+ABS(PROP(3)) .LT. .00001) GO TO 3
      IPROP=1
      DO 1 I=1,3
      IF (PROP(I)-AINT(PROP(I)) .GT. .00001) GO TO 3
    1 CONTINUE
      IPROP=-1
C
C THIS PART USED TO BE KSTAR:
   3  CALL GMEQ(PROP,AKSTAR(1,1),3,1)
      MN=1
      KSTAB(1)=1
      DO 9 I=2,NOPC
      IF (IPROP.EQ.0) THEN
        KSTAB(I)=1
      ELSE
        ISIG=1
        CALL ROTSYM(PROP,RPROP,I,2)
        CALL EQRLV(AKSTAR,RPROP,MN,M,NOPC)
        IF (M.GT.MN) THEN
          CALL GMREV(RPROP,RPROP,3,1)
          CALL EQRLV(AKSTAR,RPROP,MN,M,0)
          IF (M.GT.MN) THEN
            MN=M
          ELSE
            ISIG=-1
          ENDIF
        ENDIF
        KSTAB(I)=M*ISIG
      ENDIF
    9 CONTINUE
C
      NKC=MN
C  CHECK CENTRE OF SYMMETRY
      KCENT=1
      CALL GMREV(PROP,RPROP,3,1)
      CALL EQRLV(AKSTAR,RPROP,MN,M,0)
C DOES CENTRE OF SYMMETRY PRODUCE A NEW VECTOR
      IF (M.GT.MN) THEN
        KSTAB(1)=-MN
        KCENT=2
      ELSE
        KSTAB(1)=MN
      ENDIF
C
      WRITE (LPT,2000) (KSTAB(I),I=1,NOPC)
2000  FORMAT(' Group of K :',24I4)
      WRITE (LPT,2001) ((AKSTAR(I,J),I=1,3),J=1,NKC)
2001  FORMAT (' Vectors in the star: ',3F10.4/(22X,3F10.4))
C CASE WHERE CENTRE OF SYMMETRY PRODUCES NEW VECTORS:
      NKSTAR=KCENT*NKC
      FKSTAR=1.
      IPROP=(3-KCENT)*IPROP
      FKSTAR=FKSTAR/FLOAT(KCENT)
      INCOM=(KCENT.EQ.2)
  100 IF (KSTARS) CALL GMEQ(AKSTAR,BKSTAR,3,NKC)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PSICON(MGAT,IPSFIX)
      SUBROUTINE PSICON(MGAT,IPSFIX)
C
C *** PSICON corrected by PJB/JBF 13-Jan-1995 ***
C
CX
CC 17A
CH Determines the constraints on the phase factors in helimagnets.
CA MGAT identifies the magnetic atom in question.
CA On exit the vector IPSFIX indicates the constraints
CA         IPSFIX(i)=0 if the ith phase factor is fixed (or not required)
CA         IPSFIX(i)=j if the ith phase factor is coupled to the jth.
CD The table LPHI in common MAGDAT holds the number of the symmetry element
CD which generates the sub-lattice, and PHIH the corresponding phase factor.
CD The table IPTAB held in common MAGDAT has an entry for each symmetry
CD operator and each magnetic atom. IPTAB(J,MGAT) holds the number labelling the
CD sublattice which is generated by the operation of J on MGAT. The fundamental
CD sublattice has the label 1.
CD
CD For elements which are the product of one in the magnetic group: I and one
CD not in it: J the sub-lattice number is that belonging to J.
CD A parallel table TPTAB holds the phase shifts introduced by transforming
CD the coordinates of the sub-lattices produced by the symmetry operations
CD back into the original unit cell.
CD
CD Information for the centre of symmetry is in IPTAB(NOPC+1,MGAT). If this
CD is negative the centre of symmetry generates new sublattices, and the entries
CD for such sub-lattices in LPHI are negative.
C
C%
C      DIMENSION XEQ(3,%SYMO%),IPSFIX(%PSIS%),TPHI(%PSIS%),LTPHI(%PSIS%)
      DIMENSION XEQ(3,24),IPSFIX(4),TPHI(4),LTPHI(4)
      DIMENSION XR(3),XT(3),CELLT(3)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
      COMMON /SYMMAG/MTSYM(25),MSTAB(24),NMSYM,NFAC,OTRSYM(3,3,25),
     & MTYP,NDOM,FERO,FERA,HELI,AMOD,ANTI,MODUL,KOM20
      LOGICAL FERO,FERA,HELI,AMOD,ANTI,MODUL
C%
C      DATA NPSI /%PSIS%/
      DATA NPSI /4/
C
      IAT=JMAGAT(MGAT)
C
C  COPY DATA READ TO TEMPORARY STORAGE
      CALL JGMEQ(LPHI(1,MGAT),LTPHI,1,NPSI)
      CALL GMEQ(PHIH(1,MGAT),TPHI,1,NPSI)
C  AND CLEAR LPHI
      CALL JGMZER(LPHI(1,MGAT),1,NPSI)
C
      CALL GMEQ(X(1,IAT),XEQ(1,1),3,1)
      CALL GMEQ(X(1,IAT),XT,3,1)
      DO 1 I=1,NOPC+1
      IPTAB(I,MGAT)=1
    1 CONTINUE
      CALL JGMZER(IPSFIX,NPSI,1)
      ICENT=1
      IF (CENTRC .AND. MSTAB(1).GT.0) ICENT=2
C
C LOOP OVER ELEMENTS NOT IN THE MAGNETIC GROUP
      N=1
      ISIG=1
      IPTAB(1,MGAT)=1
      TPTAB(1,MGAT)=0
      DO 2 NO=2,NOPC+1
C LOGIC MODIFIED PJB/JBF 12 JAN 95
      IF (NO.EQ.NOPC+1) THEN
C JUMP OUT IF NON-CENTRIC OR IF CENTRE IS IN THE MAGNETIC GROUP
        IF (MSTAB(1).LT.0) GO TO 2
        CALL GMREV(XT,XR,3,1)
      ELSE
        IF (ABS(MSTAB(NO)).NE.NO) GO TO 2
        CALL ROTSYM(XT,XR,NO,1)
        IF (MSTAB(NO).LT.0) CALL GMREV(XR,XR,3,1)
        CALL GMADD(XR,TRANS(1,NO),XR,3,1)
      ENDIF
        CALL EQPPOS(XEQ,XR,N,M,24)
      IF (M.GT.N) THEN
        CALL ERRCHK(1,M,NPSI,1,'non-equivalent sublattices for'
     &  //ATNAME(IAT))
      ENDIF
    4 CALL GMSUB(XEQ(1,M),XR,CELLT,3,1)
C Caution this is in radians!
      T=TWOPI*SCALPR(CELLT,PROP)
C Be careful about the sign of T
      IF (NO .EQ.NOPC+1) THEN
        TPTAB(NOPC+1,MGAT)=T
        IF (M.GT.N) THEN
          N=2*N
          CALL ERRCHK(1,N,NPSI,1,'non-equivalent sublattices for'
     &  //ATNAME(IAT))
        ENDIF
C SET IPTAB(NOPC+1) TO GIVE THE OFFSET OF THE SUBLATTICE NUMBERS
C OF THE CENTROSYMMETRIC OPERATORS
        IPTAB(NOPC+1,MGAT)=M-IPTAB(1,MGAT)
      ELSE
        N=M
        DO 5 NNO=2,NOPC
        IF (MSTAB(NNO).EQ.NO) THEN
          IPTAB(NNO,MGAT)=M
          TPTAB(NNO,MGAT)=T
        ENDIF
    5   CONTINUE
      ENDIF
    2 CONTINUE
      NPHI(MGAT)=N
C
C NOW AMALGAMATE INFORMATION READ WITH THESE RELATIONSHIPS
C
      DO 10 I=1,NPSI
C FIND THE OPERATOR NUMBER
      NO=LTPHI(I)
C WAS THE PHASE FOR THIS OPERATOR GIVEN
      IF (NO .NE.0) THEN
        DO 12 J=1,I-1
        IF (NO.EQ.LTPHI(J)) THEN
          CALL ERRIN2(NO,-1,
     &    'Phase given for sublattice generated by operator',
     &    ' is redundant')
          GO TO 10
        ENDIF
   12   CONTINUE
        IPSI=IPTAB(IABS(NO),MGAT)
        IF (NO.LT.0) IPSI=IPSI+IPTAB(NOPC+1,MGAT)
        LPHI(IPSI,MGAT)=NO
        PHIH(IPSI,MGAT)=TPHI(I)
        IPSFIX(IPSI)=IPSI
      ENDIF
   10 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE READRT(IAT,IEND)
      SUBROUTINE READRT(IAT,IEND)
C
C *** READRT corrected by PJB C17 17 Sept 93 ***
C
CX
CC 17A
CH Reads whatever follows on a "W atom-name ROTN" card.
C
CA On entry IAT indicates which is the atom (or the total number,  if IEND
CA              is -ve.
CA          IEND points in ICARD to the next character to read
CA               unless IEND is -ve, when it requests initialisation
CA          IEND=-1 initialises the first IAT rotations
CA          IEND=-2 initialises the IATth rotation to be a unit matrix
CA          IEND=0 prints what has been read.
C
CO If IAT=0 writes its finding on unit LPT.
C
      CHARACTER*4 WORD,JAX(3)*1,RCAX(3)*2
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /QROT/ROT(3,3,10)
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C%
C      COMMON/SCRAT/MODE(%MPAT%),IROT(%MPAT%),TEMP(3,3),MFUN(10,%MPAT%),LENG(3)
      COMMON/SCRAT/MODE(20),IROT(20),TEMP(3,3),MFUN(10,20),LENG(3)
      DATA JAX,RCAX/'X','Y','Z','a*','b*','c*'/
C
      IF (IEND) 30,20,2
C
C   INITIALISE COUNT OVER AXES
   30 IF (IEND .EQ. -1) THEN
        CALL ERRCHK(1,IAT,10,0,' rotations in READRT')
        CALL JGMZER(IROT,1,IAT)
      ELSE
        CALL GMUNI(ROT(1,1,IAT),3)
        IROT(IAT)=7
      ENDIF
      GO TO 100
C
   2  IPT=IEND
      CALL RDWORD(WORD,LWORD,IPT,IPT1,80,0,IER)
      IF (IER.NE.0) GO TO 50
C
C  WHICH AXIS IS IT?
      IF (LWORD.GT.1) GO TO 51
      I=NCFIND(WORD(1:1),JAX,3)
      DO 6 J=1,3
      IPT=IPT1
      CALL RDREAL(ROT(J,I,IAT),IPT,IPT1,80,IER)
      IF (IER.NE.0) GO TO 50
    6 CONTINUE
      IROT(IAT)=IROT(IAT)+2**(I-1)
      GO TO 100
C
   20 DO 21 J=1,3
      IF (MOD(IROT(IAT),2).NE.1) GO TO 52
      CALL GMEQ(ROT(1,J,IAT),TEMP(1,J),1,3)
   21 IROT(IAT)=IROT(IAT)/2
      CALL GMPRD(ORTH(1,1,2),TEMP,ROT(1,1,IAT),3,3,3)
C  *** PJB CHANGED TO TRANSPOSE 4 Aug 88
      CALL TRANSQ(ROT(1,1,IAT),3)
      WRITE (LPT,2000) (JAX(J),(TEMP(K,J),K=1,3),J=1,3)
2000  FORMAT (' Direction cosines of quantum axes relative to ',
     & 'orthogonal crystallographic axes are:'/3(3X,A1,1X,3F10.5/))
      WRITE (LPT,2001) (JAX(J),J=1,3),(RCAX(J),(ROT(K,J,IAT),K=1,3)
     & ,J=1,3)
2001  FORMAT (' Components of reciprocal lattice vectors on ',
     & 'quantum axes are:'/5X,3(8X,A1,1X)/3(2X,A2,1X,3F10.5/))
      GO TO 100
C
C  REPORT ERRORS
   50 CALL ERRMES(1,1,'Reading rotation matrix')
      GO TO 100
C
   51 CALL ERRMES(1,1,'Axial direction'//WORD//' for '//
     1ATNAME(IAT)//' atom not recognised')
      GO TO 100
C
   52 CALL ERRMES(1,1,'Direction of quantum axis '//JAX(J)//
     1' not given')
      GO TO 100
C
  100 RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE SATFND(H,HS,NR)
      SUBROUTINE SATFND(H,HS,NR)
C
C *** SATFND updated by PJB 23 Nov 90 ***
C
CX
CC 17B
CH Finds the indices HS of the satellite equivalent to H, offset by the
CH propagation vector PROP from a reciprocal lattice point.
CA On entry H is a 1x3 vector in reciprocal space.
CA On exit NR is the number of the symmetry operator which takes H
CA            into HS
CA         NR is set to zero if an allowed satellite was not found.
CA         HS is a 1x3 vector holding the satellite equivalent vector.
C
CP SYMOP and DOMAG must have set up the symmetry.
C
      DIMENSION H(3),HS(3)
      LOGICAL MAGABS
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
C
      J=1
      DO 1 I=1,NOPC
C
C KSTAB(1) MAY NOT ACTUALLY CONTAIN 1:
      IF (I .NE. 1 .AND. KSTAB(I).NE.J) GO TO 1
      CALL ROTSYM(H,HS,I,-2)
      IF (.NOT.MAGABS(HS,IK)) GO TO 101
      J=J+1
      IF (J.GT.NKSTAR) GO TO 2
    1 CONTINUE
C
    2 I=0
C IF NOTHING FOUND PUT H INTO HS
      CALL GMEQ(H,HS,1,3)
  101 NR=I
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE SATGEN(HF,HS,MUL,NUM)
      SUBROUTINE SATGEN(HF,HS,MUL,NUM)
C
C *** SATGEN updated by PJB 22-Sept-93 ***
C
CX
CC 17B
CH Generates a set of magnetic satellite reflections.
CA On entry  HF contains the indices of the fundamental reflection
CA On exit   HS is an array of satellites of the fundamental which are
CA              inside the asymmetric unit
CA           MUL is an array containing the multiplicity of each satellite
CA           NUM is the number of distinct satellites found
CP SYMOP, SYMUNI and DOMAG must have set up the symmetry.
C
      DIMENSION HF(3),HS(3,1),MUL(1),H(3),HT(3),AK(3)
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SATELL/PROP(3),KPROP(3),KSTAB(24),NKSTAR,IPROP,FKSTAR,
     & NKC,KCENT,INCOM,KOM21
      LOGICAL INCOM
C
      NUM=0
      DO 1 J=1,NOPC
      IF (J.EQ.1) THEN
        CALL GMEQ(PROP,AK,3,1)
      ELSE
        NOP=IABS(KSTAB(J))
        IF (NOP.EQ.1) GO TO 1
        CALL ROTSYM(PROP,AK,2,1)
        IF (KSTAB(J).LT.0) CALL GMREV(AK,AK,3,1)
      ENDIF 
      DO 2 KC=1,2
C  ADD STAR VECTOR TO FUNDAMENTAL
      CALL GMADD(HF,AK,H,3,1)
C  TAKE CARE OF SPECIAL CASES WHEN PROP IS HALF A RLV
      IF (KC.EQ.1 .AND. KCENT.EQ.1) THEN
        CALL GMADD(H,AK,HT,3,1)
        M=MULBOX(HT)
        IF (M.NE.0) GO TO 2
      ENDIF
      M=MULBOX(H)
      IF (M.EQ.0) GO TO 2
      NUM=NUM+1
      MUL(NUM)=M
      CALL GMEQ(H,HS(1,NUM),3,1)
   2  CALL GMREV(AK,AK,3,1)
   1  CONTINUE
C
  100 RETURN
      END
C
C
C
C
C LEVEL 12      SUBROUTINE SETFCM(MAGSET)
      SUBROUTINE SETFCM(MAGSET)
C
C *** SETFCM by PJB 31-May-1994 ***
C
CX
CC 17A
CH Calls the routines needed to set up magnetic structure factor
CH calculations (or non-magnetic also).
CA On entry MAGSET is the name of a subroutine to do the specific magnetic
CA                setting up.  MAGSET is therefore DOMAG if actually a
CA magnetic structure, or DUMMY if non-magnetic.
CD Like SETFC it calls:
CD   INPUTN, SYMOP, OPSYM(1), RECIP, ATOPOS, SETFOR and SETANI to set
CD   data in the COMMON blocks /SYMDA/, /SYMTAB/, /NSYM/, /CELLDA/, /ATNAM/,
CD   /POSNS, /FORNAM/, /FORMDA/, possibly /ANSCAT/ and /ANISO/
CD If MAGSET=DOMAG, calls DOMAG(1) to read the magnetic data from "Q"  cards
CD and write the COMMON blocks /MAGDAT/, /SATTEL/ and  /SYMMAG/.
CI Causes all the crystallographic and structure cards to be read from
CI the copy of the Crystal Data File on unit IO10.
CO If any of the constituent routines sets the error flag IERR in /CARDRC,
CO prints an error message and stops.
C
      EXTERNAL MAGSET
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      CALL INPUTN(LPT)
      CALL SYMOP
      CALL OPSYM(1)
      CALL RECIP
      CALL ATOPOS
C TO BE RESET IF MULTIPOLE ROUTINES ARE USED:
      MPL=.FALSE.
C TO BE RESET IF MAGSET ACTUALLY=DOMAG:
      MAG=.FALSE.
      CALL MAGSET(1)
      CALL SETFOR
      CALL SETANI
      IF (IERR .NE. 0) CALL ERRMES(1,0,'in SETFCM')
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE SPHELI(IM,MODE)
      SUBROUTINE SPHELI(IM,MODE)
C
C *** SPHELI updated by PJB 30-May-95 ***
C
CX
CC 17B
CH Imposes perpendicularity on the two components of a helix.
CA On entry IM labels the magnetic atom in question
CA MODE = 0  For the setting up entry
CA MODE = 1  First least squares entry
CA MODE = 2  On entries after least squares cycles
C
      DIMENSION C(4),KK(4),PERP(3)
      LOGICAL TESTOV
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /MAGDAT/NMAG,MAGAT(150),JMAGAT(10),NMFORM(10),
     & ANGM(4,10),KANGM(4,10),SMOD(2,10),
     & KSMOD(2,10),PHIH(4,10),KPHIH(4,10),
     & LPHI(4,10),NPHI(10),TPTAB(25,10),
     & IPTAB(25,10),SPIND(3,3,2,10),KOM19
C%
C      DATA NPSI/%PSIS%/
      DATA NPSI/4/
C
      IF (MODE.LT.2) THEN
C CHECK PERPENDICULARITY
        TEST=SCALPR(SPIND(1,1,1,IM),SPIND(1,1,2,IM))
        IF (ABS(TEST).GT..0005) THEN
          CALL ERRRE2(TEST,-1,
     & 'Axes of '//ATNAME(JMAGAT(IM))//' helix not perpendicular:'//
     &  ' scalar product =',' ')
C FORCE SECOND AXIS TO BE PERPENDICULAR TO FIRST
          CALL VECPRD(SPIND(1,1,1,IM),SPIND(1,1,2,IM),PERP)
          CALL UNIVEC(PERP,D)
          CALL VECPRD(PERP,SPIND(1,1,1,IM),SPIND(1,1,2,IM))
          ANGM(3,IM)=DEGREE(ACOS(SPIND(3,1,2,IM)))
          ANGM(4,IM)=DEGREE(ATAN2(SPIND(2,1,2,IM),SPIND(1,1,2,IM)))
        ENDIF
      ENDIF
      IF (MODE.GT.0) THEN
C CALCULATE THE COEFFICIENTS C(N) OF THE PERPENDICULARITY CONSTRAINT
        N=0
        M=0
        DO 1 II=1,2
        JJ=II+1
        IF (JJ.GT.2) JJ=1
        DO 1 J=2,3
        N=N+1
        CC=0
C  GET THE PARAMETER SPECS
        DO 2 I=1,3
        CC=CC+SPIND(I,1,JJ,IM)*SPIND(I,J,II,IM)
    2   CONTINUE
        IF (TESTOV(1.,CC)) GO TO 1
        M=M+1
        C(M)=CC
        KK(M)=KPAK(2,IM,12+NPSI+N,1,1)
    1   CONTINUE
C
        IF (M.LE.1) GO TO 100
C SCALED SHIFTS MUST ADD TO ZERO
        C(1)=-C(1)
C  ADD NEW OR REPLACE THE EXISTING CONSTRAINT
        IF (MODE.EQ.1) THEN
          CALL ADDCON(M,KK,C,4)
        ELSE
          CALL RELCON(M,KK,C,4)
        ENDIF
      ENDIF
  100 RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE SPHPOL(ANG1,ANG2,SD,MODE)
      SUBROUTINE SPHPOL(ANG1,ANG2,SD,MODE)
C
C *** SPHPOL by PJB 24 Jan 89 ***
C
CX
CC 17A
CH Sets up spherical polar spin directions for magnetic structures.
CA On entry ANG1 holds the first spherical polar angle (with Z) in degrees
CA          ANG2 holds the second spherical polar angle (with X) in degrees
CA          MODE is 1 if the 3 directions alone are wanted,
CA                  3 if the derivative directions are also wanted.
C
CA On exit  SD always holds in its first 3 elements the components of the
CA             spin direction on the X, Y, Z (standard orthogonal) axes.
CA             If MODE=3 it also holds elements for the LSQ derivatives in
CA             columns 2 and 3.
C
      DIMENSION SD(3,MODE)
C
      SA1=SIN(RADIAN(ANG1))
      SA2=SIN(RADIAN(ANG2))
      CA1=COS(RADIAN(ANG1))
      CA2=COS(RADIAN(ANG2))
      SD(1,1)=SA1*CA2
      SD(2,1)=SA1*SA2
      SD(3,1)=CA1
C  AND THE DIFFERENTIALS
      IF (MODE .EQ.3)THEN
        SD(1,2)=RADIAN(CA1*CA2)
        SD(2,2)=RADIAN(CA1*SA2)
        SD(3,2)=RADIAN(-SA1)
        SD(1,3)=RADIAN(-SA1*SA2)
        SD(2,3)=RADIAN(SA1*CA2)
        SD(3,3)=0.
      ENDIF
C
      RETURN
      END
C
C
C
C
C LEVEL 13      SUBROUTINE VARSMG
      SUBROUTINE VARSMG
C
C *** VARSMG by JCM 16 Nov 90 ***
C
CX
CC 7A
CH Records variable pointers for all variables in magnetic structure-factor LSQ.
CP VARMAK has set up the variables
C
CD First clears all possible parameters for this application to be fixed.
CD Then scans variables as made by VARMAK.  Identifies each variable as a
CD type of parameter, and calls individual routines to record the information.
C
      COMMON /DERVAR/DERIVV(500),LVARV
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
C
      IF (SIMUL) GO TO 100
C SET ALL VARIABLES FIXED:
      CALL LTFAC9
      CALL CELVAR(0,0)
      CALL EXTIN9
      CALL LSCAL9
      CALL F2VAR9
      CALL MAGVAR(0,0,0)
C
C SCAN ALL VARIABLES:
      DO 1 I=1,LVARV
      KPACK=LVRPR(I)
      CALL PUNPAK(KPACK,IFAM,IGEN,ISPC)
C
C BRANCH ON FAMILY:
      GO TO (11,12), IFAM
C
  11  GO TO (21,22) , IGEN
C
C FAMILY 1, GENUS 1 - MISCELLANEOUS SPECIES (TFAC,A*,B* . . F*,DOMR,MOSC):
  21  GO TO (31,32,32,32,32,32,32,33,33) , ISPC
C
C TFAC:
  31  CALL LTFAC8(I)
      GO TO 1
C
C DOMR OR MOSC (EXTINCTION CORRECTION PARAMETERS)
  33  CALL EXTIN8(ISPC-7,I)
      GO TO 1
C
C CELL PARAMETERS:
  32  CALL CELVAR(ISPC-1,I)
      GO TO 1
C
C FAMILY 1, GENUS 2 - IN SF THIS IS 'SCAL':
  22  CALL LSCAL8(ISPC,I)
      GO TO 1
C
C FAMILY 2 - THESE ARE ALL TO DO WITH THE STRUCTURE FACTOR:
  12  IF (ISPC .LE. 12) CALL F2VAR8(IGEN,ISPC,I)
      IF (ISPC .GT. 12) CALL MAGVAR(IGEN,ISPC,I)
   1  CONTINUE
 100  RETURN
      END
