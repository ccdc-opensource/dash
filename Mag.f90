!
!*****************************************************************************
!
      SUBROUTINE CENTRO(CVEC,SVEC,RMAT,PSIFAC,PSIFCC,PSI,PCEN)
!
! *** CENTRO updated by PJB 27 May 92 ***
!
!X
!C 17B
!H Executes the action of the centre of symmetry on a term in the
!H magnetic structure factor for helimagnetic structures.
!A On entry: CVEC(3)   is a complex vector, the term for one operator,
!A           RMAT(3,3) is the required rotation matrix,
!A           PSIFAC    is the phase factor associated with the operator,
!A           PSIFCC    is the phase factor for the centrosymmetic operator,
!A           PSI       is TRUE if PSIFAC is required,
!A           PCEN      is TRUE if the magnetic structure has a centre of
!A                     symmetry so that PSIFCC is not required.
!A On exit:  SVEC(3)   is the result of applying the phase factors and the
!A                     centrosymmetric rotation.
!A For entry CENDIF the two terms VEC1 and VEC2 whose sum is SVEC
!A are also returned to allow calculation of the differentials wrt the psi's.
!N SVEC can be the same as CVEC if replacement is required
!N Called in magnetic stucture factors and magnetic least squares when
!N treating the centre of symmetry.
!
      LOGICAL PSI, PCEN, DIF
      COMPLEX CVEC(3), SVEC(3), TVEC(3), TVEC1(3), TVEC2(3)
      COMPLEX PSIFAC, PSIFCC, VEC1(3), VEC2(3)
      DIMENSION RMAT(3,3)

      DIF = .FALSE.
      GOTO 1
      ENTRY CENDIF(CVEC,SVEC,VEC1,VEC2,RMAT,PSIFAC,PSIFCC,PSI,PCEN)
      DIF = .TRUE.
! COMPLEX CONJUGATE FOR CENTROSYMETRIC STRUCTURE FACTOR
    1 CALL CMCONJ(CVEC,TVEC1,3,1)
      IF (PSI) THEN
! PHASE FACTOR ASSOCIATED WITH DIRECT OPERATOR
        CALL CGMSCA(CVEC,TVEC,PSIFAC,3,1)
      ELSE
! VECTORS UNCHANGED IF NONE
        CALL CGMEQ(CVEC,TVEC,3,1)
      ENDIF
! SPIN ROTATION ASSOCIATED WITH THE CENTRE OF SYMMETRY
      CALL RCMPRD(RMAT,TVEC1,TVEC2,3,3,1)
      IF (.NOT.PCEN) THEN
! PHASE FACTOR ASSOCIATED WITH CENTRIC  OPERATOR
        CALL CGMSCA(TVEC2,TVEC2,PSIFCC,3,1)
      ENDIF
      CALL CGMADD(TVEC2,TVEC,SVEC,3,1)
      IF (DIF) THEN
! TWO TERMS IN THE SUM RETURNED FOR DIFFERENTIALS
        CALL CGMEQ(TVEC,VEC1,3,1)
        CALL CGMEQ(TVEC2,VEC2,3,1)
      ENDIF

      END SUBROUTINE CENTRO
!
!*****************************************************************************
!
      SUBROUTINE FMCALC(H,FMCMOD,FMCSQR)
!
! *** FMCALC corrected by JBF/PJB 13-Jan-1995 ***
!
!X
!C 17B
!H Calculates magnetic interaction vectors and magnetic structure factors.
!A On entry H is the 1x3 vector containing h,k,l
!A On exit   FMCMOD = domain average of the lengths of the m.i. vector
!A           FMCSQR = square of the above
!D On exit Q(1:3,1:NDOM) in COMMON QCAL contains the magnetic interaction
!D vectors for each of the NDOM domains.
!P STHL in /BRAGG should hold sin theta/lambda
!P NKSTAR in /SATELL should have been set up by routine KSTAR
!P
!P The setting up routines:
!P     RECIP  (for the cell parameters)
!P     SYMOP  (for the space group symmetry)
!P     SETFOR (for the scattering factors, both nuclear and magnetic)
!P     SETANI (for the anisotropic temperature factors)
!P     DOMAG(1) and (2) (for the magnetic structure) and
!P     SPHELI (to set up the spin directions on spherical polars)
!P should all have been obeyed to set up the structure.
!D Sets SSQRD in /BRAGG to be STHL squared
!D Gives zero as answers for magnetic absences
!N There is also the routine LMCALC which does a similar calculation but also
!N calculates derivatives, for use in LSQ.
!
      COMPLEX SUM1(3), TERM, TVEC(3), FORM, HR, FORMFA, P(3), FMC(3)
      COMPLEX PSIFAC, PSIFCC
      LOGICAL SKIP, MAGABS, PSI, PSICEN, CENPSI, DOCENT
      DIMENSION RH(3), H(3), RS(3,3), HD(3,3), SDOM(3,3)
      REAL            STHMXX,    STHL, SINTH, COSTH, SSQRD, TWSNTH,    DSTAR2, TWOTHD
      COMMON /BRAGG / STHMXX(5), STHL, SINTH, COSTH, SSQRD, TWSNTH(5), DSTAR2, TWOTHD(5)
      EQUIVALENCE (STHLMX,STHMXX(1))
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
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
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /QCAL  / Q(3,12)
      COMPLEX Q
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
                      FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
                      KOM26
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
                      MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
                      KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
                      KOM22

! CLEAR ALL ANSWERS IN CASE ABSENT:
! CLEAR MODULUS AND SQUARE:
      FMCMOD = 0.
      FMCSQR = 0.
      SSQRD = STHL*STHL
! OUT IF ABSENT:
      IF (MAGABS(H,IK)) GOTO 100
      TAU = -FLOAT(IK)
!  CYCLE OVER DOMAINS
      ND = 0
      ICHIR = 1
      IHELIX = 1
      DOCENT = CENTRC
      IF (MODUL) THEN
        PSICEN = (MSTAB(1).LT.0)
        CENPSI = .NOT.PSICEN
        IF (HELI) THEN
          IHELIX = 2
!  POSSIBILITY OF CHIRALITY DOMAINS
          IF (IABS(IPROP).EQ.1) ICHIR = 2
        ENDIF
      ELSE
! DON'T WANT PHASE SHIFTS
        PSI = .FALSE.
        CENPSI = .FALSE.
        PSICEN = .TRUE.
      ENDIF
      SKIP = .FALSE.
      DO ICHI = 1, ICHIR
        DO IDOMOP = 1, NOPC
          IF (FERO .AND. IDOMOP.NE.1) GOTO 18
! DOMAINS EXIST FOR ALL ELEMENTS NOT IN THE MAGNETIC GROUP
          IF (IDOMOP.NE.1 .AND. IABS(MSTAB(IDOMOP)).NE.IDOMOP) GOTO 15
! FMC COLLECTS THE MAGNETIC STRUCTURE FACTOR, A COMPLEX VECTOR,
! ZEROED BEFORE CALCULATING THE VALUE FOR EACH DOMAIN
          CALL CGMZER(FMC,1,3)
! FIRST SCATTERING FACTOR:
          IFF = 0
! CYCLE OVER MAGNETIC ATOMS:
          DO IM = 1, NMAG
            IR = JMAGAT(IM)
! IS THERE A PHASE FACTOR ASSOCIATED WITH THE CENTRE OF SYMMETRY FOR THIS ATOM
            IF (CENPSI) THEN
! IS THE ATOM ON THE CENTRE
              PSICEN = (ISGEN(1,IR).LT.0)
              IF (.NOT.PSICEN) NIPC = NPHI(IM)/2
              DOCENT = (CENTRC .AND. (.NOT.PSICEN))
            ENDIF
! JUMP IF FORM/SCATTERING FACTOR THE SAME AS BEFORE
            IF (NMFORM(IM).EQ.IFF) GOTO 2
! IF NOT, GET IT
            IFF = NMFORM(IM)
            FORM = FORMFA(STHL,IFF)
!  CALCULATE FOR EACH COMPONENT OF THE HELIX IN TURN
    2       DO ICOMP = 1, IHELIX
              CALL CGMZER(SUM1,3,1)
! INNER LOOP OVER SYMMETRY EQUIVALENTS:
! PREPARE TO COUNT OPERATORS USED
              NFAC = 0
              DO IS = 1, NOPC
!  ONLY USE OPERATORS WHICH LEAVE THE PROPAGATION DIRECTION INVARIANT
                IF (IS.NE.1 .AND. IABS(KSTAB(IS)).NE.1) GOTO 3
!  THE OPERATOR TO USE IS THE PRODUCT OF IS WITH THAT CREATING THE DOMAIN
                IROP = MULTAB(IDOMOP,IS)
! SKIP IF IROP DOESNT GENERATE A DISTINCT SUB-LATTICE
                IF (MODUL .AND. LPHI(IPTAB(IROP,IM),IM).NE.IROP) GOTO 3
                CALL ROTSYM(H,RH,IROP,-1)
                IF (IDOMOP.NE.1 .AND. MSTAB(IDOMOP).LT.0) CALL GMREV(RH,RH,3,1)
                F1 = TWOPI*(SCALPR(X(1,IR),RH)+SCALPR(TRANS(1,IROP),H))
!  ANISOTROPIC T F (=1. IF NOT THERE) NEEDED SEPARATELY FOR LSQ:
                ERS = ANITF(RH,IR)
                ARS = COS(F1)*ERS
                BRS = SIN(F1)*ERS
                TERM = CMPLX(ARS,BRS)
!  FIND OUT WHAT THE SYMMETRY DOES TO THE SPIN DIRECTION
                CALL ROTMAG(SPIND(1,1,ICOMP,IM),SDOM,IS)
!  GET THE SPIN DIRECTION FOR THIS DOMAIN
                CALL ROTOSM(SDOM,RS,IDOMOP,1)
                CALL C1MSCA(RS,TVEC,TERM,3,1)
                IF (MODUL) THEN
! INCLUDE A PHASE SHIFT FOR THIS SUB-LATTICE IF NECESSARY
                  IP = IPTAB(IROP,IM)
                  TEST = TAU*(RADIAN(PHIH(IP,IM)))
! Changes made May 1994
!        TEST=TAU*(TPTAB(IROP,IM)+RADIAN(PHIH(IP,IM)))
!        IF (IROP.NE.1) TEST=TEST*FLOAT(KSTAB(IROP))
                  PSI = (ABS(TEST).GT..0001)
                  IF (PSI) PSIFAC = CEXP(CMPLX(0.,TEST))
                ENDIF
! IF CENTROSYMMETRIC, COMPENSATE FOR USING ONLY HALF NUMBER OF OPERATORS:
!  OTRSYM(25) SHOULD CONTAIN THE MATRIX RELATING THE MAGNETIC COMPONENTS
!  OF ATOMS RELATED BY THE CENTRE OF SYMMETRY, WHETHER THIS IS IN THE
!  MAGNETIC GROUP OR NOT.
                IF (DOCENT) THEN
                  IF (.NOT.PSICEN) THEN
! Changed May 1994
                    TEST = TAU*(RADIAN(PHIH(NIPC+IP,IM)))
!          TEST=TAU*(RADIAN(PHIH(NIPC+IP,IM))-TPTAB(IP,IM))
                    PSIFCC = CEXP(CMPLX(0.,TEST))
                  ENDIF
                  CALL CENTRO(TVEC,TVEC,OTRSYM(1,1,25),PSIFAC,PSIFCC,PSI,PSICEN)
                ELSE
! IF CENTRIC THEN ATOM WAS ON THE CENTRE
                  IF (CENTRC) CALL CMRSCA(TVEC,TVEC,2.0,3,1)
                  IF (PSI) CALL CGMSCA(TVEC,TVEC,PSIFAC,3,1)
                ENDIF
                CALL CGMADD(SUM1,TVEC,SUM1,1,3)
! INCREMENT COUNT OF OPERATORS USED
                NFAC = NFAC + 1
! END OF INNERMOST CYCLE OVER SYMMETRY
    3         ENDDO
              FAC = AMULT(IR)*EXP(-TF(IR)*SSQRD)
! COMPENSATE FOR NOT USING ALL SYMMETRY ELEMENTS
              FACTOR = FLOAT(NOPC)/FLOAT(NFAC)
              FAC = FAC*FACTOR
! SCALE MOMENT TO CMS-12
              SM = SMOD(ICOMP,IM)*VALMUB
! HR IS PRODUCT OF ATOM DEPENDENT BUT SYMMETRY INDEPENDENT FACTORS
              HR = FAC*FORM*SITE(IR)
              TERM = HR*SM
! SHIFT THE PHASE OF THE PERPENDICULAR COMPONENT OF THE SPIRAL
              IF (ICOMP.EQ.2) THEN
                TERM = TERM*CMPLX(0.,TAU)
!  THE PHASE SHIFT IS NEGATIVE FOR REVERSE CHIRALITY
                IF (ICHI.EQ.2) TERM = -TERM
              ENDIF
              CALL CGMSCA(SUM1,TVEC,TERM,3,1)
              CALL CGMADD(FMC,TVEC,FMC,1,3)
            ENDDO
          ENDDO
! END OF CYCLE OVER ATOMIC POSITIONS
!  COMPENSATE FOR MULTIPLICITY OF THE STAR
          CALL CMRSCA(FMC,FMC,FKSTAR,3,1)
! NOW THE CYCLE FOR THE DOMAIN AVERAGE
   18     IF (IDOMOP.EQ.1 .OR. FERO) CALL MAGDOM(H,HD,IDOMOP,SKIP)
          CALL RCMPRD(HD,FMC,P,3,3,1)
          FMCSQR = FMCSQR + RSCALP(P,P)
          IF (.NOT.SKIP) ND = ND + 1
          IF (FERO) THEN
!       PUT Q BACK INTO UNROTATED FRAME
            CALL CROTO(P,Q(1,ND),IS,1)
          ELSE
            CALL CGMEQ(P,Q(1,ND),3,1)
          ENDIF
   15   ENDDO
      ENDDO
! NOW THE DOMAIN AVERAGE
      FMCSQR = FMCSQR/FLOAT(ND)
      FMCMOD = SQRT(FMCSQR)
  100 RETURN

      END SUBROUTINE FMCALC
!
!*****************************************************************************
!
      SUBROUTINE GENMAG(H,NOMORE,MUL,SMAX,NFLAG)
!
! *** GENMAG updated by JCM 6 May 92 ***
!
!X
!C 17C
!H Generates the next set of magnetic h,k,l, scanning the asymmetric unit.
!A On entry NFLAG=-9999 if this is the very first entry
!A    if IPROP is non-zero this entry makes a magnetic asymmetric unit
!A    and sets things up for subsequent calls.
!A    otherwise NFLAG is left severely alone and just presented again
!A    on each subsequent entry.
!A On exit H is a real 1x3 array holding the next h,k,l (unless none)
!A         NOMORE is a LOGICAL saying whether or not no more generated
!A         MUL is the multiplicity of H
!P PROPER must have been called to fill in /SATELL/
!
      LOGICAL NOMORE
      DIMENSION H(3), HT(3)
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /TEMGEN/ HF(3)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (NFLAG.EQ.-9999) THEN
! INITIAL ENTRY:
        IF (NKSTAR.GT.1) THEN
          CALL SUBSYM(KSTAB)
          CALL SYMUNI
          IF (IBMBER .NE. 0) RETURN
        ENDIF
!  INCREASE SINTHETA LIMIT TO ALLOW FOR PROPAGATION VECTOR
        SDIF = VCTMOD(0.5,PROP,2)
        SLIM = SMAX + SDIF
        CALL SETGEN(SLIM)
! FIRST PRESENT 0,0,0 AS FUNDAMENTAL:
        CALL GMZER(HF,1,3)
        NOMORE = .FALSE.
! ENTRIES OTHER THAN INITIAL - EXPECT NFLAG SET:
      ELSEIF (NFLAG.EQ.0) THEN
        CALL GETGEN(HF,NOMORE)
        IF (NOMORE) RETURN
      ELSE
        CALL GMSUB(HF,PROP,H,3,1)
        NFLAG = 0
        GOTO 3
      ENDIF
!  ADD STAR VECTOR TO FUNDAMENTAL
      CALL GMADD(HF,PROP,H,3,1)
!  TAKE CARE OF SPECIAL CASES WHEN PROP IS HALF A RLV
      IF (IABS(IPROP).EQ.2) THEN
        CALL GMADD(H,PROP,HT,3,1)
        M = MULBOX(HT)
        IF (M.NE.0) NFLAG = 0
      ENDIF
      NFLAG = 1
    3 CALL ASUNIT(H,HT,N,MUL)
      CALL GMEQ(HT,H,3,1)

      END SUBROUTINE GENMAG
!
!*****************************************************************************
!
      LOGICAL FUNCTION MAGABS(H,IK)
!
! *** MAGABS modified by PJB Jan 91 ***
!X
!C 17B
!H Tests for systematic absence of magnetic reflections.
!A On entry H is a 1x3 array holding h,k,l
!A On exit MAGABS = .TRUE. if absent, .FALSE. if present
!A         IK is +1 if the indices correspond to a r.l.v + the
!A            propagation vector ie h=g+k
!A         IK is -1 if h=g-k
!A         IK is 0 if this is irrelevant.
!P The space group symmetry should have been set up by SYMOP
!P The propagation vector PROP should have been set up by DOMAG
!
      DIMENSION H(3), TEST(3)
      LOGICAL M, LATABS
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
!
      IK = 0
      IF (IPROP.EQ.0) THEN
        M = LATABS(H)
      ELSE
        CALL GMADD(H,PROP,TEST,3,1)
        M = LATABS(TEST)
        IF (M) THEN
          CALL GMSUB(H,PROP,TEST,3,1)
          M = LATABS(TEST)
          IF (.NOT.M) IK = 1
        ELSE
          IK = -1
        ENDIF
      ENDIF
      MAGABS = M
      RETURN
      END FUNCTION MAGABS
!
!*****************************************************************************
!
      SUBROUTINE MAGCNC
!
! *** MAGCNC uodated by PJB 17-Jan-95 ***
!
!X
!C 17A
!H To find magnetic constraints in a non-least squares calculation.
!P The magnetic symmetry must have been set up by calls to SYMOP and
!P DOMAG
!
      DIMENSION IPSFIX(4)
      LOGICAL LMFIX(3), FIRST
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
     &                KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      FIRST = .TRUE.
      DO MAT = 1, NMAG
! IF FERROMAGNETIC NO CONSTRAINTS
        IF (.NOT.(FERO.OR.FERA)) CALL MAGCON(MAT,LMFIX,FIRST)
! SET UP SPHERICAL POLARS:
        CALL SPHPOL(ANGM(1,MAT),ANGM(2,MAT),SPIND(1,1,1,MAT),1)
        IF (MODUL) THEN
          CALL PSICON(MAT,IPSFIX)
          IF (HELI) THEN
            CALL SPHPOL(ANGM(3,MAT),ANGM(4,MAT),SPIND(1,1,2,MAT),1)
            CALL SPHELI(MAT,0)
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE MAGCNC
!
!*****************************************************************************
!
      SUBROUTINE MAGCON(IATO,LMFIX,FIRST)
!
! *** MAGCON corrected by PJB 4 30-Nov-1994 ***
!
!X
!C 17A
!H Finds and reports the symmetry constraints on magnetic parameters.
!A IATO is the number of the magnetic atom in question
!A LMFIX(I) is set to TRUE if parameter I for this atom is fixed by symmetry
!A FIRST is TRUE if no constraints have yet been found
!
      EXTERNAL DUMMY
      LOGICAL FIRST, LMFIX(3), NONE
      DIMENSION RMAT(3,3), NFIX(3), FIX(3), IATAB(24), RX(3), SCON(3)
      DIMENSION VEC(3), TVEC(3,2)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
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
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
     &                KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
!
      IAT = JMAGAT(IATO)
!  CHECK THAT STRUCTURE AND SYMMETRY ARE COMPATIBLE WITH THE
!  PROPAGATION VECTOR
      CALL MAKGRP(ISGEN(1,IAT),IATAB,0,DUMMY)
      DO N = 2, NOPC
        IF (IABS(KSTAB(N)).NE.1 .AND. IATAB(N).EQ.0) THEN
          WRITE (LPT,2800) N
          CALL BMBOUT
          RETURN
        ENDIF
      ENDDO
      CANG1 = ANGM(1,IATO)
      CANG2 = ANGM(2,IATO)
      DO J = 1, 3
        LMFIX(J) = .FALSE.
        NFIX(J) = 9999
      ENDDO
      N = ISGEN(1,IAT)
!  JUMP IF NOT SPECIAL
      NONE = N.EQ.1
      IF (NONE) GOTO 17
      IF (N.LT.0 .AND. MSTAB(1).LT.0) THEN
! CENTRE OF SYMMETRY PRESENT: IF NOT AT ORIGIN WHAT
! LATTICE VECTOR IS INVOLVED
        CALL GMREV(X(1,IAT),TVEC,1,3)
        CALL GMADD(X(1,IAT),TVEC,VEC,3,1)
        PH = SCALPR(PROP,VEC)
!  CHECK THE THE PHASE SHIFT IS n*PI
        IF (ABS(FLOAT(NINT(2.*PH))-2.*PH).GT..001) THEN
          CALL ERRCH2(ATNAME(IATO),1,                                   &
     &       'The centre of symmetry is not consistent with a moment on'&
     &       ,'for this propagation vector')
!  CHECK ALSO THAT IT IS NON-REVERSING
        ELSE
          IF (NINT(COS(TWOPI*PH)).NE.MTSYM(25)) THEN
!  CAN HAVE NO MOMENT:
            NFIXED = 3
            DO I = 1, 3
              LMFIX(I) = .TRUE.
            ENDDO
            SMOD(1,IATO) = 0.
            CANG1 = 0.
            CANG2 = 0.
            GOTO 17
          ENDIF
        ENDIF
      ENDIF
!  LOOK TO SEE WHICH ELEMENTS OF THE ATOM SUB-GROUP ARE
!  IN THE MAGNETIC SYMMETRY GROUP
      DO K = 2, NOPC
        IF (IATAB(K).EQ.0) GOTO 3
        CALL GMEQ(OTRSYM(1,1,K),RMAT,3,3)
!  REVERSE THE MATRIX IF OPERATION IS COMBINED WITH INVERSION, AND
!  THE INVERSION IS TIME-REVERSING
        IF (IATAB(K).LT.0 .AND. MTSYM(25).LT.0)                         &
     &      CALL GMREV(RMAT,RMAT,3,3)
! IN CASE OF SHIFT BY A LATTICE VECTOR:
        IF (IPROP.NE.0) THEN
          CALL ROTSYM(X(1,IAT),RX,K,1)
          CALL GMADD(RX,TRANS(1,K),RX,3,1)
          CALL GMSUB(RX,X(1,IAT),RX,3,1)
          PH = SCALPR(PROP,RX)
!  CHECK THE THE PHASE SHIFT IS n*PI
          IF (ABS(FLOAT(NINT(2.*PH))-2.*PH).GT..001) THEN
            CALL ERRIN2(K,-1,'Operator','is not consistent with a moment on'//          &
     &                  ATNAME(IATO)//'for this propagation vector')
!  IF AN ODD MULTIPLE OF PI REVERSE THE MATRIX
            IF (ABS(FLOAT(NINT(PH))-PH).GT..3) CALL GMREV(RMAT,RMAT,3,3)
          ENDIF
        ENDIF
        CALL RELSM3(RMAT,NFIX,FIX)
    3 ENDDO
!  NOW CONVERT XYZ CONSTRAINTS TO SPHERICAL POLAR ONES
!  COUNT FIXED PARS
      NFIXED = 0
      NREL = 0
      DO I = 1, 3
        IF (NFIX(I).EQ.0) THEN
          NFIXED = NFIXED + 1
        ELSE
! CHECK THAT THIS 9999 IS STILL OK:
          IF (NFIX(I).LT.9999) NREL = NREL + 1
        ENDIF
      ENDDO
      GOTO (10,11,12,13), NFIXED + 1
! ALL FIXED
   13 DO I = 1, 3
        LMFIX(I) = .TRUE.
      ENDDO
      GOTO 17
!  TWO FIXED - FIND WHICH
   12 IF (NFIX(1).EQ.0 .AND. NFIX(2).EQ.0) THEN
        CANG1 = 0.
      ELSE
        CANG1 = 90.
      ENDIF
      LMFIX(1) = .TRUE.
    7 IF (NFIX(1).EQ.0) CANG2 = 90.
      IF (NFIX(2).EQ.0) CANG2 = 0.
      LMFIX(2) = .TRUE.
      GOTO 10
!  ONE FIXED
   11 IF (NFIX(3).NE.0) GOTO 7
      CANG1 = 90.
      LMFIX(1) = .TRUE.
!  NOW RELATIONSHIPS
   10 GOTO (8,8,15,16), NREL + 1
! NONE
    8 NONE = NFIXED.EQ.0
      GOTO 17
!  ALL THREE RELATED - FIX BOTH ANGLES
   16 CANG2 = ATAN2(FIX(1),FIX(2))
      CANG1 = DEGREE(ATAN2(FIX(1)*COS(CANG2),FIX(3)))
      LMFIX(1) = .TRUE.
      GOTO 14
!  IF TWO PARS RELATED THEY SHOULD BE 1 AND 2 IF THE THIRD IS FREE
   15 IF (NFIX(3).NE.9999 .AND. NFIXED.NE.1) THEN
        WRITE (LPT,3000) NFIX
        CALL BMBOUT
        RETURN
      ENDIF
      GOTO (20,17,21,20), NFIX(1) + 1
   21 CANG2 = ATAN2(FIX(1),FIX(2))
   14 LMFIX(2) = .TRUE.
      CANG2 = DEGREE(CANG2)
      GOTO 17
   20 CANG1 = DEGREE(ATAN2(FIX(3),FIX(NFIX(3))))
      LMFIX(1) = .TRUE.
!
   17 IF (FIRST) CALL MESS(LPT,1,                                       &
     &                     'Symmetry constraints on magnetic moments:')
      FIRST = .FALSE.
      WRITE (ICARD,2001) ATNAME(IAT)
 2001 FORMAT (1X,A4,' None')
      IF (.NOT.NONE) THEN
!
!  CHOOSE BEST DIRECTION
        CALL SPHPOL(CANG1,CANG2,SOLD,1)
        CALL SPHPOL(ANGM(1,IATO),ANGM(2,IATO),SCON,1)
        IF (SCALPR(SCON,SOLD).LT.0.) THEN
          ANGM(1,IATO) = ARANGE(180.-CANG1,180.,-180.)
          ANGM(2,IATO) = ARANGE(180.+CANG2,180.,-180.)
        ELSE
          ANGM(1,IATO) = CANG1
          ANGM(2,IATO) = CANG2
        ENDIF
        J = 6
        IF (LMFIX(3)) THEN
          WRITE (ICARD(J:),2003) 'MU', SMOD(1,IATO)
 2003     FORMAT (1X,A4,' =',F9.4)
          J = J + 16
        ENDIF
        IF (LMFIX(1)) THEN
          WRITE (ICARD(J:),2002) 'THET', ANGM(1,IATO)
          J = J + 16
        ENDIF
        IF (LMFIX(2)) THEN
          WRITE (ICARD(J:),2002) 'PHI', ANGM(2,IATO)
          J = J + 16
        ENDIF
      ENDIF
      CALL MESS(LPT,0,ICARD)
!  THE CONSTRAINTS ARE THE SAME FOR BOTH COMPONENTS OF A HELIX
      IF (HELI) THEN
        IF (LMFIX(1)) ANGM(3,IATO) = ANGM(1,IATO)
        IF (LMFIX(2)) ANGM(4,IATO) = ANGM(2,IATO)
        IF (LMFIX(3)) SMOD(2,IATO) = SMOD(1,IATO)
      ENDIF
      RETURN
 2800 FORMAT (/' ERROR ** Structure symmetry is reduced by ',           &
     &        'that of the propagation vector (operator #',I2,          &
     &        ').'/' **** Change S cards ****')
 3000 FORMAT (' *** PROGRAM ERROR IN MAGCON ***'/' NFIX =',             &
     &        3I5/' I thought this couldn''t happen! PJB')
 2002 FORMAT (1X,A4,' =',F9.2)
      END SUBROUTINE MAGCON
!
!*****************************************************************************
!
      SUBROUTINE MAGDOM(H,HK,IOP,SKIP)
!
! *** MAGDOM by PJB Apr 87 ***
!
!X
!C 17B
!H In Least Squares refinement with magnetic scattering, forms the matrix
!H needed for derivatives of Q with respect to a spin direction.
!A On entry H is the scattering vector
!A          IOP is the symmetry operator to be used on H
!A On exit the 3x3 matrix HK contains the required matrix.
!A         LOGICAL SKIP is set to indicate
!P MTYP must indicate the type of magnetic structure.
!P NORD, MSTAB and KSTAB entries for IOP must be set.
!D  HK is formed, such that Q=KxSxK=S[HK]
!
      DIMENSION H(3), RH(3), OH(3), HK(3,3)
      LOGICAL SKIP
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
     &                KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
!
!  FOR FERROMAGNETIC USE THE WHOLE GROUP
      IF (FERO) GOTO 2
!
      IF (IOP.NE.1 .AND. (MSTAB(IOP).NE.1.OR.KSTAB(IOP).NE.1)) THEN
        SKIP = .TRUE.
        GOTO 100
      ENDIF
    2 SKIP = .FALSE.
!
      CALL ROTSYM(H,RH,IOP,2)
!  REVERSE IF IMPROPER ROTATION
      IF (NORD(IOP).LT.0) CALL GMREV(RH,RH,3,1)
      CALL ORTHO(RH,OH,2)
      CALL UNIVEC(OH,D)
      J = 2
      K = 3
      DO I = 1, 3
        HK(I,I) = OH(J)*OH(J) + OH(K)*OH(K)
        HK(I,J) = -OH(I)*OH(J)
        HK(I,K) = -OH(I)*OH(K)
        J = K
        K = I
      ENDDO
!
  100 RETURN
      END SUBROUTINE MAGDOM
!
!*****************************************************************************
!
      SUBROUTINE MAGSYM(MODE)
!
! *** MAGSYM updated by PJB 31-May-1994 ***
!
!X
!C 17B
!H Routine with 4 named entry points, MAGSYM, MELIN, NELIN and ROTMAG, to deal
!H generally with magnetic symmetry.  MAGSYM sets magnetic symmetry, MELIN puts
!H in an operator for a generator, NELIN puts in non-symmetric rotation and
!H ROTMAG rotates with a magnetic operator.
!A On entry to MAGSYM MODE = 0 to initialise
!A                           1 to generate the remaining operators and the
!A                             orthogonal spin rotations in OTRSYM
!A On entry to MELIN  IOP = which operator
!A                    VAL = + or - 1, its value
!A On entry to ROTMAG S is the 3x3 array to be rotated
!A                    IOP = which operator
!A On exit from ROTMAG RS is the 3x3 rotated array
!A On entry to NELIN  IOP = which operator
!                     SROT = the rotation matrix for the spin
!
      EXTERNAL MTPROD
      DIMENSION S(3,3), RS(3,3), TEMP(3,3), SROT(3,3), MGEN(3)
      DIMENSION NSTAB(25), MJTAB(24)
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2),       &
     &                KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL, KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3), KOM22

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (MODE.EQ.1) GOTO 20
!  INITIALISE
      CALL JGMZER(MTSYM,1,NOPC)
      CALL JGMZER(MSTAB,1,NOPC)
      GOTO 100
!  ENTRY TO PUT IN ONE MAGNETIC SYMMETRY OPERATOR
      ENTRY MELIN(IOP,VAL)
      IO = IABS(IOP)
      IF (ABS(VAL).LT..0001) THEN
        CALL ERRIN2(IOP,1,'Zero value for magnetic symmetry operator',' ')
        GOTO 100
      ENDIF
!  RECORD WHETHER THE OPERATOR INVOLVED INVERSION
      IF (IOP.LT.0) THEN
        MSTAB(IO) = ISIGN(2,IFIX(VAL))
      ELSE
        MSTAB(IO) = ISIGN(1,IFIX(VAL))
      ENDIF
      GOTO 100
!  ENTRY TO ADD A NON-SYMMETRIC ROTATION
      ENTRY NELIN(IOP,SROT)
      IO = IOP
      IF (IOP.EQ.-1) IO = 25
      CALL GMEQ(SROT,OTRSYM(1,1,IO),3,3)
!  MARK NON-SYMMETRIC OPERATORS
      IF (IOP.EQ.-1) IOP = 1
      MSTAB(IOP) = 100
      GOTO 100
! FORM THE REST OF THE MAGNETIC OPERATORS FROM THE MULTIPLICATION TABLE
   20 DO IO = 1, NOPC
        IF (MSTAB(IO).EQ.100) THEN
          NSTAB(IO) = 1
          MSTAB(IO) = 0
        ELSE
          NSTAB(IO) = 0
        ENDIF
      ENDDO
      IF (FERO .OR. FERA) THEN
        CALL GMUNI(OTRSYM(1,1,25),3)
        GOTO 100
      ENDIF
      IF (CENTRC) THEN
        IF (MSTAB(1).EQ.0) THEN
          CALL MESS(LPT,1,'No centre of symmetry in the magnetic group')
          IF (NSTAB(1).EQ.0) THEN
            CALL ERRMES(1,2,'No spin rotation given for centrosymmetrically related atoms')
          ELSE
            WRITE (LPT,2031) ((OTRSYM(I,J,25),I=1,3),J=1,3)
 2031       FORMAT (/' Spin rotation for centro-symmetrically related',' atoms is: ',3F6.2,2(/58X,3F6.2))
          ENDIF
        ELSE
          CALL GMUNI(OTRSYM(1,1,25),3)
          MSTAB(1) = ISIGN(1,MSTAB(1))
          IF (MSTAB(1).LT.0) CALL GMREV(OTRSYM(1,1,25),OTRSYM(1,1,25),3,3)
          MTSYM(25) = MSTAB(1)
        ENDIF
      ENDIF
! CONSTRUCT THE MAGNETIC SUB-GROUP
      MGEN(1) = -1
      IF (MSTAB(1).EQ.0) MGEN(1) = 1
      NGEN = 1
      DO I = 2, NOPC
        IF (MSTAB(I).EQ.0) GOTO 40
        NGEN = NGEN + 1
        MGEN(NGEN) = (3-2*(IABS(MSTAB(I))))*I
        MTSYM(I) = ISIGN(1,MSTAB(I))
        IF (NGEN.EQ.3) GOTO 41
   40 ENDDO
      MGEN(3) = 1
      IF (NGEN.EQ.1) MGEN(2) = 1
   41 CALL MAKGRP(MGEN,MJTAB,1,MTPROD)
      NMSYM = (MGEN(1))
! NUMBER OF ORIENTATION DOMAINS
      NDOM = NOPC/NMSYM
      MJTAB(1) = MSTAB(1)
      WRITE (LPT,2001) (I,MTSYM(I),I=1,NOPC)
 2001 FORMAT (/' Magnetic symmetry operators: ',12(I3,' =',I3)/31X,12(I3,' =',I3))
      IF (CENTRC) WRITE (LPT,2002) MSTAB(1)
 2002 FORMAT (/31X,' Centre of symmetry =',I3)
!
!  PUT SPIN ROTATIONS ONTO ORTHOGONAL AXES
! USE OTRSYM(,,1) TEMPORARILY FOR TRANSPOSE:
      CALL GMEQ(ORTH(1,1,1),OTRSYM(1,1,1),3,3)
      CALL TRANSQ(OTRSYM(1,1,1),3)
      DO NO = 2, NOPC
        IF (MTSYM(NO).EQ.0) GOTO 27
        CALL MultiplyMatrices(SYM(1,1,NO),ORTH(1,1,2),TEMP,3,3,3)
        IF (MTSYM(NO)*NORD(NO).LT.0.) CALL GMREV(TEMP,TEMP,3,3)
        CALL MultiplyMatrices(OTRSYM(1,1,1),TEMP,OTRSYM(1,1,NO),3,3,3)
   27 ENDDO
      CALL GMUNI(OTRSYM(1,1,1),3)
      CALL FACTGP(MJTAB,MSTAB,NFAC)
      WRITE (LPT,2003) (I,MSTAB(I),I=1,NOPC)
 2003 FORMAT (/' Magnetic symmetry table : ',12(I3,' =',I3)/28X,12(I3,' =',I3))
!
!  NOW DEAL WITH NON-SYMMETRIC ROTATIONS
      DO NO = 2, NOPC
        IOPP = IABS(MSTAB(NO))
        IF (IOPP.EQ.1) GOTO 43
        IF (IOPP.EQ.NO) THEN
          IF (NSTAB(NO).NE.1) THEN
            CALL ERRIN2(NO,1,'Missing non-symmetric spin rotation for element number',' ')
          ELSE
            WRITE (LPT,2004) NO, ((OTRSYM(I,J,NO),I=1,3),J=1,3)
 2004       FORMAT (/' Non-symmetric spin rotation for element number', &
     &              I3,' is:  ',3F6.2,2(/56X,3F6.2))
          ENDIF
        ELSE
          IX = MULTAB(NO,INVERS(IOPP))
          IF (MSTAB(IX).EQ.0) THEN
            CALL ERRMES(-1,0,'Logical error in MAGSYM - bad tables')
            IF (IBMBER .NE. 0) RETURN
          ENDIF
          CALL MultiplyMatrices(OTRSYM(1,1,IX),OTRSYM(1,1,IOPP),OTRSYM(1,1,NO),3,3,3)
        ENDIF
        IF (MSTAB(NO).LT.0) CALL GMREV(OTRSYM(1,1,NO),OTRSYM(1,1,NO),3,3)
   43 ENDDO
      GOTO 100
!  ENTRY TO OPERATE WITH MAGNETIC SYMMETRY
      ENTRY ROTMAG(S,RS,IOP)
!  DONT ROTATE IF FERROMAGNETIC
      IF (FERO .OR. FERA) THEN
        CALL GMEQ(S,RS,3,3)
      ELSE
        DO I = 1, 3
          CALL MultiplyMatrices(OTRSYM(1,1,IOP),S(1,I),RS(1,I),3,3,1)
        ENDDO
      ENDIF
  100 RETURN
      END SUBROUTINE MAGSYM
!
!*****************************************************************************
!
      SUBROUTINE MF5ADD(ISPC,IG,IS,N)
!
! *** MF5ADD by JCM 13 Jun 88 ***
!
!X
!C 18B
!H A specialist routine to deal with the refinement of multipoles.
!H Converts each type of LSQ family 5 (multipoles) addressing to the other.
!A On entry N=1  requests "give answer in in ISPC genus 1 from IG,IS"
!A          N=-1 requests "give answers as IG, IS from species ISPC"
!A So for N=1 IG, IS are set on entry to genus and species
!A    for N=-1, ISPC is set on entry to contain "species" in one long genus.
!P Table MPNAM must have been set up to contain species names, and
!P       MPTAB to point in it for each genus of family 5.
!
      COMMON /MPODA / NMPAT, NMPOL, MPATAB(20), MPNMTB(150), NCLUMP,    &
     &                KCLUMP(100), MPTAB(21), POLAMP(200,6), KPOLMP(200)&
     &                , NCMAT, CONMAT(600,2)
!
      IF (N.LT.0) GOTO 1
! IG, IS GIVEN ; SET ISPC:
      ISPC = MPTAB(IG) + IS
      GOTO 100
!
! ISPC GIVEN; SET IS,IG:
    1 DO I = 1, NMPAT
        IF (MPTAB(I+1).GT.ISPC) GOTO 3
      ENDDO
!
!>> JCC      STOP 'ERROR ** IN MF5ADD'
      CALL BMBOUT
      RETURN
    3 IG = I
      IS = ISPC - MPTAB(I) + 1
  100 RETURN
      END SUBROUTINE MF5ADD
!
!*****************************************************************************
!
      SUBROUTINE PROPAG(MODE,INOUT)
!
! 17B
! *** PROPAG new by PJB 28-Sept-93 ***
!
!H Multiple entry subroutine for propagation vector refinement
!A MODE indicates what function is required
!A MODE = 1 Read a Q PROP card if present and if so call PROPER
!A          on exit INOUT is 1 id found 0 if not
!A      = 2 Set the symmetry constraints on the magnetic propagation vector
!A          INOUT is set on input to the offset of the propagation vector in
!A          the family 1 genus 1 parameters
!A      = 3 Apply a shift to the INOUTth component
!A      = 4 Write new Q PROP card on unit INOUT
!A      = 0 all components fixed
!A      = negative integer set the variable number of the -MODEth component to
!A       be INOUT
!
      DIMENSION ITAB(24), JGEN(3), NFIX3(3), RMAT(3,3), FIX3(3),        &
     &          NCOUNT(3)
!
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /SYMTAB/ MULTAB(24,24), INVERS(24), NORD(24), IGEN(3),     &
     &                KOM22
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
!
      IF (MODE.LE.0) GOTO 60
      GOTO (10,20,30,40), MODE
!
! READ Q PROP CARD
   10 CALL FINDCD('Q','PROP',4,0,LCD)
      IF (LCD.GT.0) THEN
        CALL RDNUMS(FIX3,7,3,NUM,IER)
        IF (IER.NE.0 .OR. NUM.NE.3) CALL ERRMES(1,1,'reading propagation vector')
        CALL PROPER(FIX3)
        INOUT = 1
      ELSE
        INOUT = -1
      ENDIF
      GOTO 100
! SET SYMMETRY CONSTRAINTS
   20 IPOFF = INOUT
! CLEAR OUT ALL FIX/RELA INFO
      DO K = 1, 3
        NFIX3(K) = 9999
      ENDDO
      IF (IPROP.LE.0 .OR. IPROP.EQ.2) GOTO 23
      CALL JGMZER(ITAB,1,NOPC)
      ITAB(1) = 1
      JGEN(1) = 1
      DO I = 2, NOPC
        IF (KSTAB(I).NE.1) GOTO 21
        ITAB(I) = IABS(NORD(I))
        JGEN(1) = JGEN(1) + 1
   21 ENDDO
! Find generators of subgroup
      CALL GENELM(ITAB,JGEN)
! JUMP IF NOT SPECIAL:
      IF (JGEN(1).EQ.1) GOTO 26
! JUMP IF NOT ON A CENTRE OF SYMMETRY:
      IF (JGEN(1).GT.0) GOTO 24
! FIX ALL COMPONENTS:
   23 DO I = 1, 3
        CALL FIXPAR(I,NFIX3)
      ENDDO
      GOTO 26
! TAKE FIRST (OF POSSIBLE 2) SYMMETRY ELEMENTS MAKING THIS POSITION SPECIAL:
   24 DO I = 2, 3
        K = IABS(JGEN(I))
        CALL GMEQ(SYM(1,1,K),RMAT,3,3)
        IF (JGEN(I).LT.0) CALL GMREV(RMAT,RMAT,3,3)
        CALL RELSM3(RMAT,NFIX3,FIX3)
! IS THERE A SECOND GENERATOR OF THE SUB-GROUP WHICH MAKES THIS ATOM SPECIAL?
        IF (JGEN(3).EQ.0) GOTO 26
      ENDDO
! ALL COMPONENT RELATIONS COLLECTED IN TEMPORARY SPACE - USE:
   26 DO I = 1, 3
        NCOUNT(I) = KPAK(1,1,IPOFF+I,JPHASE,1)
      ENDDO
      CALL FIXREL(3,NFIX3,FIX3,NCOUNT,5)
      GOTO 100
! APPLY SHIFT
   30 IF (INOUT.GT.3) GOTO 100
      CALL ADJUST(PROP(INOUT))
      GOTO 100
! WRITE NEW Q PROP CARD
   40 IF (ICARD(3:6).NE.'PROP') THEN
        L = LENGT(ICARD)
        WRITE (INOUT,4001) (ICARD(I:I),I=1,L)
 4001   FORMAT (80A1)
      ELSE
        WRITE (INOUT,4000) PROP
 4000   FORMAT ('Q PROP',3F10.4)
      ENDIF
      GOTO 100
! TO SET ALL COMPONENTS FIXED, OR VARY ONE:
   60 N = IABS(MODE)
      IF (N.EQ.0) THEN
        DO I = 1, 3
          KPROP(I) = 0
        ENDDO
      ELSE
        IF (N.GT.3) GOTO 100
        KPROP(N) = INOUT
      ENDIF
  100 RETURN
      END SUBROUTINE PROPAG
!
!*****************************************************************************
!
      SUBROUTINE PROPER(AKVEC)
!
! *** PROPER updated by PJB 14-Dec-1994 ***
!
!X
!C 17A
!H Determines whether the satellites generated by the propagation vector
!H PROP have integer indices, and generates its "star".
!
!A On entry AKVEC is the 1X3 propagation vector copied to PROP
!A There is an ENTRY KSTAR(AKVEC,BKSTAR) to return the vectors AKSTAR
!A of the star in BKSTAR as well as filling in the common /SATELL/
!P SYMOP should have been obeyed to read the space group symmetry
!D On exit  IPROP in /SATELL/ is zero if PROP is 0 0 0
!D                negative for other integer indices
!D                and positive for non-integer values.
!D        IABS(IPROP) is set to 2 if twice PROP is a reciprocal
!D                lattice vector.
!D On exit the symmetry table of the star is in KSTAB.
!D NKSTAR holds the number of vectors in the star,
!D INCOM is .TRUE. if the propagation vector is fixed on a symmetry point
!D and FKSTAR is a scale for structure factors assuming a mono-k domain.
!D The ENTRY KSTAR not only writes the common /SATELL/ but also returns the
!D vectors AKSTAR of the star.
!O Writes its findings on unit LPT
!
      DIMENSION AKVEC(3), AKSTAR(3,24), RPROP(3), BKSTAR(1)
      LOGICAL KSTARS
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
     &                KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      KSTARS = .FALSE.
      GOTO 2
      ENTRY KSTAR(AKVEC,BKSTAR)
      KSTARS = .TRUE.
    2 CALL GMEQ(AKVEC,PROP,3,1)
      IPROP = 0
      IF (ABS(PROP(1))+ABS(PROP(2))+ABS(PROP(3)).LT..00001) GOTO 3
      IPROP = 1
      DO I = 1, 3
        IF (PROP(I)-AINT(PROP(I)).GT..00001) GOTO 3
      ENDDO
      IPROP = -1
!
! THIS PART USED TO BE KSTAR:
    3 CALL GMEQ(PROP,AKSTAR(1,1),3,1)
      MN = 1
      KSTAB(1) = 1
      DO I = 2, NOPC
        IF (IPROP.EQ.0) THEN
          KSTAB(I) = 1
        ELSE
          ISIG = 1
          CALL ROTSYM(PROP,RPROP,I,2)
          CALL EQRLV(AKSTAR,RPROP,MN,M,NOPC)
          IF (M.GT.MN) THEN
            CALL GMREV(RPROP,RPROP,3,1)
            CALL EQRLV(AKSTAR,RPROP,MN,M,0)
            IF (M.GT.MN) THEN
              MN = M
            ELSE
              ISIG = -1
            ENDIF
          ENDIF
          KSTAB(I) = M*ISIG
        ENDIF
      ENDDO
      NKC = MN
!  CHECK CENTRE OF SYMMETRY
      KCENT = 1
      CALL GMREV(PROP,RPROP,3,1)
      CALL EQRLV(AKSTAR,RPROP,MN,M,0)
! DOES CENTRE OF SYMMETRY PRODUCE A NEW VECTOR
      IF (M.GT.MN) THEN
        KSTAB(1) = -MN
        KCENT = 2
      ELSE
        KSTAB(1) = MN
      ENDIF
      WRITE (LPT,2000) (KSTAB(I),I=1,NOPC)
 2000 FORMAT (' Group of K :',24I4)
      WRITE (LPT,2001) ((AKSTAR(I,J),I=1,3),J=1,NKC)
 2001 FORMAT (' Vectors in the star: ',3F10.4/(22X,3F10.4))
! CASE WHERE CENTRE OF SYMMETRY PRODUCES NEW VECTORS:
      NKSTAR = KCENT*NKC
      FKSTAR = 1.
      IPROP = (3-KCENT)*IPROP
      FKSTAR = FKSTAR/FLOAT(KCENT)
      INCOM = (KCENT.EQ.2)
      IF (KSTARS) CALL GMEQ(AKSTAR,BKSTAR,3,NKC)

      END SUBROUTINE PROPER
!
!*****************************************************************************
!
      SUBROUTINE PSICON(MGAT,IPSFIX)
!
! *** PSICON corrected by PJB/JBF 13-Jan-1995 ***
!
!X
!C 17A
!H Determines the constraints on the phase factors in helimagnets.
!A MGAT identifies the magnetic atom in question.
!A On exit the vector IPSFIX indicates the constraints
!A         IPSFIX(i)=0 if the ith phase factor is fixed (or not required)
!A         IPSFIX(i)=j if the ith phase factor is coupled to the jth.
!D The table LPHI in common MAGDAT holds the number of the symmetry element
!D which generates the sub-lattice, and PHIH the corresponding phase factor.
!D The table IPTAB held in common MAGDAT has an entry for each symmetry
!D operator and each magnetic atom. IPTAB(J,MGAT) holds the number labelling the
!D sublattice which is generated by the operation of J on MGAT. The fundamental
!D sublattice has the label 1.
!D
!D For elements which are the product of one in the magnetic group: I and one
!D not in it: J the sub-lattice number is that belonging to J.
!D A parallel table TPTAB holds the phase shifts introduced by transforming
!D the coordinates of the sub-lattices produced by the symmetry operations
!D back into the original unit cell.
!D
!D Information for the centre of symmetry is in IPTAB(NOPC+1,MGAT). If this
!D is negative the centre of symmetry generates new sublattices, and the entries
!D for such sub-lattices in LPHI are negative.
!
      DIMENSION XEQ(3,24), IPSFIX(4), TPHI(4), LTPHI(4)
      DIMENSION XR(3), XT(3), CELLT(3)
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
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
      COMMON /SATELL/ PROP(3), KPROP(3), KSTAB(24), NKSTAR, IPROP,      &
     &                FKSTAR, NKC, KCENT, INCOM, KOM21
      LOGICAL INCOM
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /SYMMAG/ MTSYM(25), MSTAB(24), NMSYM, NFAC, OTRSYM(3,3,25),&
     &                MTYP, NDOM, FERO, FERA, HELI, AMOD, ANTI, MODUL,  &
     &                KOM20
      LOGICAL FERO, FERA, HELI, AMOD, ANTI, MODUL
      DATA NPSI/4/
!
      IAT = JMAGAT(MGAT)
!
!  COPY DATA READ TO TEMPORARY STORAGE
      CALL JGMEQ(LPHI(1,MGAT),LTPHI,1,NPSI)
      CALL GMEQ(PHIH(1,MGAT),TPHI,1,NPSI)
!  AND CLEAR LPHI
      CALL JGMZER(LPHI(1,MGAT),1,NPSI)
!
      CALL GMEQ(X(1,IAT),XEQ(1,1),3,1)
      CALL GMEQ(X(1,IAT),XT,3,1)
      DO I = 1, NOPC + 1
        IPTAB(I,MGAT) = 1
      ENDDO
      CALL JGMZER(IPSFIX,NPSI,1)
      ICENT = 1
      IF (CENTRC .AND. MSTAB(1).GT.0) ICENT = 2
! LOOP OVER ELEMENTS NOT IN THE MAGNETIC GROUP
      N = 1
      ISIG = 1
      IPTAB(1,MGAT) = 1
      TPTAB(1,MGAT) = 0
      DO NO = 2, NOPC + 1
! LOGIC MODIFIED PJB/JBF 12 JAN 95
        IF (NO.EQ.NOPC+1) THEN
! JUMP OUT IF NON-CENTRIC OR IF CENTRE IS IN THE MAGNETIC GROUP
          IF (MSTAB(1).LT.0) GOTO 2
          CALL GMREV(XT,XR,3,1)
        ELSE
          IF (ABS(MSTAB(NO)).NE.NO) GOTO 2
          CALL ROTSYM(XT,XR,NO,1)
          IF (MSTAB(NO).LT.0) CALL GMREV(XR,XR,3,1)
          CALL GMADD(XR,TRANS(1,NO),XR,3,1)
        ENDIF
        CALL EQPPOS(XEQ,XR,N,M,24)
        IF (M.GT.N) THEN
          CALL ERRCHK(1,M,NPSI,1,'non-equivalent sublattices for'//ATNAME(IAT))
        ENDIF
        CALL GMSUB(XEQ(1,M),XR,CELLT,3,1)
! Caution this is in radians!
        T = TWOPI*SCALPR(CELLT,PROP)
! Be careful about the sign of T
        IF (NO.EQ.NOPC+1) THEN
          TPTAB(NOPC+1,MGAT) = T
          IF (M.GT.N) THEN
            N = 2*N
            CALL ERRCHK(1,N,NPSI,1,'non-equivalent sublattices for'//ATNAME(IAT))
          ENDIF
! SET IPTAB(NOPC+1) TO GIVE THE OFFSET OF THE SUBLATTICE NUMBERS
! OF THE CENTROSYMMETRIC OPERATORS
          IPTAB(NOPC+1,MGAT) = M - IPTAB(1,MGAT)
        ELSE
          N = M
          DO NNO = 2, NOPC
            IF (MSTAB(NNO).EQ.NO) THEN
              IPTAB(NNO,MGAT) = M
              TPTAB(NNO,MGAT) = T
            ENDIF
          ENDDO
        ENDIF
    2 ENDDO
      NPHI(MGAT) = N
! NOW AMALGAMATE INFORMATION READ WITH THESE RELATIONSHIPS
      DO I = 1, NPSI
! FIND THE OPERATOR NUMBER
        NO = LTPHI(I)
! WAS THE PHASE FOR THIS OPERATOR GIVEN
        IF (NO.NE.0) THEN
          DO J = 1, I - 1
            IF (NO.EQ.LTPHI(J)) THEN
              CALL ERRIN2(NO,-1,'Phase given for sublattice generated by operator',' is redundant')
              GOTO 10
            ENDIF
          ENDDO
          IPSI = IPTAB(IABS(NO),MGAT)
          IF (NO.LT.0) IPSI = IPSI + IPTAB(NOPC+1,MGAT)
          LPHI(IPSI,MGAT) = NO
          PHIH(IPSI,MGAT) = TPHI(I)
          IPSFIX(IPSI) = IPSI
        ENDIF
   10 ENDDO

      END SUBROUTINE PSICON
!
!*****************************************************************************
!
      SUBROUTINE SETFCM(MAGSET)
!
! *** SETFCM by PJB 31-May-1994 ***
!
!X
!C 17A
!H Calls the routines needed to set up magnetic structure factor
!H calculations (or non-magnetic also).
!A On entry MAGSET is the name of a subroutine to do the specific magnetic
!A                setting up.  MAGSET is therefore DOMAG if actually a
!A magnetic structure, or DUMMY if non-magnetic.
!D Like SETFC it calls:
!D   INPUTN, SYMOP, OPSYM(1), RECIP, ATOPOS, SETFOR and SETANI to set
!D   data in the COMMON blocks /SYMDA/, /SYMTAB/, /NSYM/, /CELLDA/, /ATNAM/,
!D   /POSNS, /FORNAM/, /FORMDA/, possibly /ANSCAT/ and /ANISO/
!D If MAGSET=DOMAG, calls DOMAG(1) to read the magnetic data from "Q"  cards
!D and write the COMMON blocks /MAGDAT/, /SATTEL/ and  /SYMMAG/.
!I Causes all the crystallographic and structure cards to be read from
!I the copy of the Crystal Data File on unit IO10.
!O If any of the constituent routines sets the error flag IERR in /CARDRC,
!O prints an error message and stops.
!
      EXTERNAL MAGSET
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      DIMENSION INREAD(26), ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      CALL INPUTN(LPT)
      CALL SYMOP
      IF (IBMBER .NE. 0) RETURN
      CALL OPSYM(1)
      CALL RECIP
      IF (IBMBER .NE. 0) RETURN
      CALL ATOPOS
! TO BE RESET IF MULTIPOLE ROUTINES ARE USED:
      MPL = .FALSE.
! TO BE RESET IF MAGSET ACTUALLY=DOMAG:
      MAG = .FALSE.
      CALL MAGSET(1)
      CALL SETFOR
      CALL SETANI
      IF (IERR.NE.0) CALL ERRMES(1,0,'in SETFCM')

      END SUBROUTINE SETFCM
!
!*****************************************************************************
!
      SUBROUTINE SPHELI(IM,MODE)
!
! *** SPHELI updated by PJB 30-May-95 ***
!
!X
!C 17B
!H Imposes perpendicularity on the two components of a helix.
!A On entry IM labels the magnetic atom in question
!A MODE = 0  For the setting up entry
!A MODE = 1  First least squares entry
!A MODE = 2  On entries after least squares cycles
!
      DIMENSION C(4), KK(4), PERP(3)
      LOGICAL TESTOV
      COMMON /ATNAM / ATNAME(150), ATNA(150,9)
      CHARACTER*4 ATNA, ATNAME
      COMMON /MAGDAT/ NMAG, MAGAT(150), JMAGAT(10), NMFORM(10),         &
     &                ANGM(4,10), KANGM(4,10), SMOD(2,10), KSMOD(2,10), &
     &                PHIH(4,10), KPHIH(4,10), LPHI(4,10), NPHI(10),    &
     &                TPTAB(25,10), IPTAB(25,10), SPIND(3,3,2,10), KOM19
      DATA NPSI/4/
!
      IF (MODE.LT.2) THEN
! CHECK PERPENDICULARITY
        TEST = SCALPR(SPIND(1,1,1,IM),SPIND(1,1,2,IM))
        IF (ABS(TEST).GT..0005) THEN
          CALL ERRRE2(TEST,-1,'Axes of '//ATNAME(JMAGAT(IM))            &
     &                //' helix not perpendicular:'//                   &
     &                ' scalar product =',' ')
! FORCE SECOND AXIS TO BE PERPENDICULAR TO FIRST
          CALL VECPRD(SPIND(1,1,1,IM),SPIND(1,1,2,IM),PERP)
          CALL UNIVEC(PERP,D)
          CALL VECPRD(PERP,SPIND(1,1,1,IM),SPIND(1,1,2,IM))
          ANGM(3,IM) = DEGREE(ACOS(SPIND(3,1,2,IM)))
          ANGM(4,IM) = DEGREE(ATAN2(SPIND(2,1,2,IM),SPIND(1,1,2,IM)))
        ENDIF
      ENDIF
      IF (MODE.GT.0) THEN
! CALCULATE THE COEFFICIENTS C(N) OF THE PERPENDICULARITY CONSTRAINT
        N = 0
        M = 0
        DO II = 1, 2
          JJ = II + 1
          IF (JJ.GT.2) JJ = 1
          DO J = 2, 3
            N = N + 1
            CC = 0
!  GET THE PARAMETER SPECS
            DO I = 1, 3
              CC = CC + SPIND(I,1,JJ,IM)*SPIND(I,J,II,IM)
            ENDDO
            IF (TESTOV(1.,CC)) GOTO 1
            M = M + 1
            C(M) = CC
            KK(M) = KPAK(2,IM,12+NPSI+N,1,1)
    1     ENDDO
        ENDDO
        IF (M.LE.1) GOTO 100
! SCALED SHIFTS MUST ADD TO ZERO
        C(1) = -C(1)
!  ADD NEW OR REPLACE THE EXISTING CONSTRAINT
        IF (MODE.EQ.1) THEN
          CALL ADDCON(M,KK,C,4)
        ELSE
          CALL RELCON(M,KK,C,4)
        ENDIF
      ENDIF
  100 RETURN
      END SUBROUTINE SPHELI
!
!*****************************************************************************
!
      SUBROUTINE SPHPOL(ANG1,ANG2,SD,MODE)
!
! *** SPHPOL by PJB 24 Jan 89 ***
!
!X
!C 17A
!H Sets up spherical polar spin directions for magnetic structures.
!A On entry ANG1 holds the first spherical polar angle (with Z) in degrees
!A          ANG2 holds the second spherical polar angle (with X) in degrees
!A          MODE is 1 if the 3 directions alone are wanted,
!A                  3 if the derivative directions are also wanted.
!
!A On exit  SD always holds in its first 3 elements the components of the
!A             spin direction on the X, Y, Z (standard orthogonal) axes.
!A             If MODE=3 it also holds elements for the LSQ derivatives in
!A             columns 2 and 3.
!
      DIMENSION SD(3,MODE)

      SA1 = SIN(RADIAN(ANG1))
      SA2 = SIN(RADIAN(ANG2))
      CA1 = COS(RADIAN(ANG1))
      CA2 = COS(RADIAN(ANG2))
      SD(1,1) = SA1*CA2
      SD(2,1) = SA1*SA2
      SD(3,1) = CA1
!  AND THE DIFFERENTIALS
      IF (MODE.EQ.3) THEN
        SD(1,2) = RADIAN(CA1*CA2)
        SD(2,2) = RADIAN(CA1*SA2)
        SD(3,2) = RADIAN(-SA1)
        SD(1,3) = RADIAN(-SA1*SA2)
        SD(2,3) = RADIAN(SA1*CA2)
        SD(3,3) = 0.
      ENDIF

      END SUBROUTINE SPHPOL
