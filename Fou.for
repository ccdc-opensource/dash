C
C
C
C
C LEVEL 6      SUBROUTINE ARROW(X,Y,L)
      SUBROUTINE ARROW(X,Y,L)
C
C *** ARROW by PJB Sep 87 ***
C
CX
CC 14C
CH Draws an arrow centred at X,Y in the current space.
CP General CCSL plotting must have been set up.
CA On entry L=0 requests outline arrow
CA          L=1 requests solid arrow
C
      DIMENSION SPTS(2,7),PTS(2,7)
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
      DATA SPTS/-8.,-1.,-8.,1.,4.,1.,4.,4.,8.,0.,4.,-4.,4.,-1./
      DATA NPTS/7/

C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C  ADD DISPLACEMENT
      DO 2 I=1,NPTS
      PTS(1,I)=SPTS(1,I)+X
    2 PTS(2,I)=SPTS(2,I)+Y
C
C  DRAW OUTLINE
      CALL KANGA1(PTS(1,NPTS),PTS(2,NPTS),3)
      DO 1 N=1,NPTS
      CALL KANGA1(PTS(1,N),PTS(2,N),2)
    1 CONTINUE
C
C  RETURN IF L=0
      IF (L.EQ.0) GO TO 100
C
C  GET EFFECTIVE GRID FOR SHADING
      CALL PLCONV(0.,0.,1,A1,B1,NSPCE)
	IF (IBMBER .NE. 0) RETURN
      CALL PLCONV(.02,0.,1,A,B,NSPCE)
	IF (IBMBER .NE. 0) RETURN
      A=A-A1
      B=B-B1
      STEP=SQRT(A**2+B**2)
C
C  NOW SHADE THE ARROW
C  FIRST THE TAIL
      CALL KANGA1(PTS(1,1),PTS(2,1),3)
      DO 3 XX=PTS(1,1)+STEP,PTS(1,7),STEP
      CALL KANGA1(XX,PTS(2,2),2)
    3 CALL KANGA1(XX,PTS(2,1),2)
C  THEN THE HEAD
      SY1=STEP*((PTS(2,5)-PTS(2,4))/(PTS(1,5)-PTS(1,4)))
      SY2=STEP*((PTS(2,5)-PTS(2,6))/(PTS(1,5)-PTS(1,6)))
      CALL KANGA1(PTS(1,6),PTS(2,6),3)
      Y1=PTS(2,4)
      Y2=PTS(2,6)
C
C NOTE FROM JCM - THIS OUGHT TO BE TIDIED:
      DO 4 XX=PTS(1,7)+STEP,PTS(1,5),STEP
      Y1=Y1+SY1
      Y2=Y2+SY2
      CALL KANGA1(XX,Y1,2)
      CALL KANGA1(XX,Y2,2)
    4 CONTINUE
  100 RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE ATMPLO(IFND,NFND,JP)
      SUBROUTINE ATMPLO(IFND,NFND,JP)
C
C *** ATMPLO by PJB Aug 86 ***
C
CX
CC 5B
CH Plots atom positions on a map.
CA On exit NFND=number of positions plotted
CA         IFND, an integer array, contains pointers to the atoms plotted
CA         JP, an integer array, points to the symbols used for each atom
C
      DIMENSION JP(4),IFND(4),Z(2)
      LOGICAL FOUND1
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON/SCRAT/AA(3,3),TRXX(3,48),TLAT(3,4),BOUNDS(2,6),XX(3,3),
     & TMPV1(3),TMPV2(3),TMPV3(3),TTXX(3,50),NT
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C SET MAP SPACE:
      CALL SPCSET(4)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      JP(1)=1
      JP(2)=2
      JP(3)=4
      JP(4)=9
C
      NT=NDIM
      NFND=0
C
C  GET INVERSE OF U
      CALL GMEQ(U,AA,3,3)
      CALL TRINV3(AA,D)
C  TRANSORM LATTICE VECTORS IF NECESSARY
      DO 6 L=1,NLAT
      IF (MODET.EQ.1) THEN
        CALL GMPRD(ALAT(1,L),AA,TLAT(1,L),1,3,3)
      ELSE
        CALL GMEQ(ALAT(1,L),TLAT(1,L),1,3)
      ENDIF
    6 CONTINUE
C  FIX BOUNDARIES
      CALL GMEQ(OUTLIM,BOUNDS,2,1)
      CALL GMEQ(OUTLIM(1,2),BOUNDS(1,2),2,1)
C  LIMIT IN Z
      AL=VCTMOD(1.,U(1,3),1)
      ALZ=.1/AL
      BOUNDS(1,3)=OUTLIM(1,3) -ALZ
      BOUNDS(2,3)=OUTLIM(1,3) +ALZ
C  COPY VECTOR
      CALL GMEQ(BOUNDS(1,1),BOUNDS(1,4),2,3)
      IF (MODET.EQ.0) GO TO 5
      II=1
      DO 20 I=1,2
      TMPV1(3)=BOUNDS(I,3)
      DO 20 J=1,2
      TMPV1(2)=BOUNDS(J,2)
      DO 20 L=1,2
      TMPV1(1)=BOUNDS(L,1)
      CALL GMPRD(U,TMPV1,TRXX(1,II),3,3,1)
   20 II=II+1
C  FIND MAXIMUM AND MINIMUM ON EACH CRYSTALLOGRAPHIC AXIS
      DO 21 I=1,3
      BOUNDS(1,I)=AMIN1(TRXX(I,1),TRXX(I,2),TRXX(I,3),TRXX(I,4),
     & TRXX(I,5),TRXX(I,6),TRXX(I,7),TRXX(I,8))
      BOUNDS(2,I)=AMAX1(TRXX(I,1),TRXX(I,2),TRXX(I,3),TRXX(I,4),
     & TRXX(I,5),TRXX(I,6),TRXX(I,7),TRXX(I,8))
   21 CONTINUE
C GENERATE ATOMIC POSITIONS
    5 DO 1 IAT=1,NATOM
C FOR SKEW BUT RATIONAL PLANE, POSITIONS MUST BE TRANSFORMED TO MATCH THE
C SYMMETRY OPERATORS
      IF (MODET.EQ.1) THEN
        CALL GMPRD(X(1,IAT),AA,TRXX(1,1),1,3,3)
      ELSE
        CALL GMEQ(X(1,IAT),TRXX(1,1),1,3)
      ENDIF
      CALL ATOGEN(MOLD)
      FOUND1=.FALSE.
      DO 3 M=1,MOLD
      IF (MODET.NE.1) THEN
        CALL GMEQ(TRXX(1,M),TMPV1,1,3)
      ELSE
        CALL GMPRD(TRXX(1,M),U,TMPV1,1,3,3)
      ENDIF
      CALL TBOUND(IFOUND)
      IF (IFOUND .EQ.0) GO TO 3
      FOUND1=.TRUE.
      DO 31 IFN=1,IFOUND
      DO 30 I=1,2
      Z(I)=(TTXX(I,IFN)-OUTLIM(1,I))/OUTLIM(3,I)
   30 CONTINUE
      CALL KANGA3(Z(1),Z(2),1.5*CHUNIT*CHSCAL(2,3),JP(NFND+1))
   31 CONTINUE
    3 CONTINUE
      IF (FOUND1) CALL ERRCHK(2,NFND,4,0,'atoms in ATMPLO')
      IFND(NFND)=IAT
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE ATOGEN(MOLD)
      SUBROUTINE ATOGEN(MOLD)
C
C *** ATOGEN by PJB Aug 86 ***
C
CX
CC 5B
CH Generates a set of equivalent positions.
CA MOLD on exit is the number generated
CP In /SCRAT/ on entry (usually from ATMPLO) TRXX(1:3,1) holds the original
CP atomic position.
CD Generates all the related (different) positions in TRXX within 1 unit cell.
C
CN A PJB special for use with ATMPLO.
C
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON/SCRAT/AA(3,3),TRXX(3,48),TLAT(3,4),BOUNDS(2,6),XX(3,3),
     & TMPV1(3),TMPV2(3),TMPV3(3),TTXX(3,50),NT
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
      MOLD=0
      CALL GMEQ(TRXX(1,1),XX,1,3)
      DO 1 L=1,NLAT
      DO 1 N=1,NOPC
      CALL GMADD(TRANS(1,N),TLAT(1,L),TMPV1,1,3)
      CALL ROTSYM(XX,XX(1,2),N,1)
C  CYCLE OVER CENTRE OF SYMMETRY
      DO 2 IR=1,NCENT
      CALL GMADD(TMPV1,XX(1,2),TMPV2,1,3)
      IF (MOLD.EQ.0) THEN
        MOLD=1
      ELSE
        CALL EQPOS(TRXX,TMPV2,MOLD,M,48)
        IF (M.GT.MOLD) MOLD=M
      ENDIF
C
    2 CALL GMREV(XX(1,2),XX(1,2),1,3)
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CHOOSF(F,SF,ALPHA1,ALPHA,MODEF)
      SUBROUTINE CHOOSF(F,SF,ALPHA1,ALPHA,MODEF)
C
C *** CHOOSF by PJB ***
C
CX
CC 5C
CH Chooses modulus and phase for a particular Fourier.
C
CA On entry ALPHA1 is an original phase
CA          F is a 1x2 array of read coefficients
CA          MODEF is the type of Fourier:
CA            MODEF=1   FCAL
CA            MODEF=2   FOBS (Centrosymmetric)
CA            MODEF=3   mod(FOBS)*phase(FCAL)
CA            MODEF=4   FOBS-FCAL
CA            MODEF=5   (mod(FOBS)-mod(FCAL))*phase(FCAL)
CA            MODEF=6   FOBS*FOBS (Patterson)
CA On exit SF holds the modulus and ALPHA the required phase.
C
      DIMENSION F(2)
C
      ALPHA=ALPHA1
      GO TO (21,22,23,24,25,26),MODEF
C
  21  SF = ABS(F(1))
      GO TO 10
C
   22  SF = F(2)
      ALPHA = 0.
      GO TO 10
C
  23  SF = ABS(F(2))
      GO TO 10
C
  24  SF = F(2) -F(1)
      ALPHA = 0.
      GO TO 10
C
  25  SF = ABS(F(2)) - ABS(F(1))
      GO TO 10
C
  26  SF = F(2)*F(2)
      ALPHA = 0.
C
C SF AND ALPHA NOW SET UP:
C
   10 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE DOSIDE(A,B,ISIDE,NR,NC,X,Y)
      SUBROUTINE DOSIDE(A,B,ISIDE,NR,NC,X,Y)
C
C *** DOSIDE 24 Nov 83 by JCM ***
C
CX
CC 5C
CH A specialist routine for contour plotting, to help to decide where a
CH contour crosses the side of a square.
C
      IF (ISIDE .NE. 1) THEN
        DO 3 I=2,ISIDE
        C=B
        B=A
   3    A=1.0-C
      ENDIF
      Y = FLOAT(NC-1) + A
      X = B + FLOAT(NR-1)
      RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE DPLOT(XXIN,YYIN,LINE)
      SUBROUTINE DPLOT(XXIN,YYIN,LINE)
C
C *** DPLOT by PJB Apr 85 ***
C
CX
CC 14C
CH Does plotting in current coordinate system.
CA On entry:
CA     XXIN,YYIN give the target position.
CA     LINE controls the kind of line being drawn:
CA          If LINE = 0 pen is "up"
CA          If LINE = 1 a continuous line is drawn
CA          If LINE = 2 a dashed line is drawn
CA          If LINE>10 but <21 the symbol LINE-10 is drawn at intervals of
CA                     DASH in /ADASH/
CP COMMON /ADASH/ is used to remember what is happening for dashed lines
CP        X1,Y1=where pen went to.
CD The pen is moved from its current position to XXIN,YYIN.
C
      COMMON /ADASH/DASH,X1,Y1,REM,IPEN,IDSH,DTRAN(2,2)
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP

C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C
      IF (LINE .LE. 1) GO TO 12
      ILINE=1
      IF (LINE.GT.10) ILINE=2
C  CHECK THAT THE SPACE HAS NOT CHANGED
      IF (IDSH.EQ.NSPCE) GO TO 10
C  MAKE A DUMMY CALL TO PLCONV
      CALL PLCONV(0.,0.,0,X,Y,3)
	IF (IBMBER .NE. 0) RETURN

C  AND SET DTRAN FROM THE NEW PTRAN
      CALL GMEQ(PTRAN(1,1,2),DTRAN,2,2)
      IDSH=NSPCE
      GO TO 10
C
C NOT DASHING - RESET DASH COUNT - JOIN HERE IF DASHING AT CONTOUR START:
   12 II=3-LINE
  11  REM = 0.
      IPEN = 2
      CALL KANGA1 (XXIN,YYIN,II)
C FOR NOW - AS IPN IS NOT SET THIS CANNOT BE RIGHT, BUT OFFHAND I DON'T KNOW
C WHAT IT SHOULD BE:
C      IF (IPN .GT. 0) GO TO 100
      GO TO 101
C AVOID STORAGE OF X1,Y1 IF NOT DASHING, AND EXIT
C
C HERE FOR DASHED LINE:
C IF "DASHING" BUT ACTUALLY PEN UP, DO NOT COMPUTE DASHES, BUT DO ARRANGE TO
C STORE CURRENT POSITION IN X1,Y1:
C LENGTH OF LINE TO BE PLOTTED
   10 U = XXIN-X1
      V = YYIN-Y1
      ALEN = SQRT((U*DTRAN(1,1)+V*DTRAN(1,2))**2+(U*DTRAN(2,1)+V*DTRAN(2
     & ,2))**2)
C
C  DEAL WITH ANY PART LEFT FROM LAST ENTRY:
      BLEN = ALEN-REM
      IF (BLEN .LE.0.) GO TO 1
C  IN CASE THAT VECTOR IS LESS THAN REMAINING DASH LENGTH
C
C  REMAINDER OF PREVIOUS DASH:
      FRAC = REM/ALEN
      X = X1 + FRAC*U
      Y = Y1 + FRAC*V
      GO TO (4,5),ILINE
    4 CALL KANGA1(X,Y,IPEN)
      IPEN=5-IPEN
      GO TO 6
   5  CONTINUE
      CALL KANGA3(X,Y,DASH/5,LINE-10)
C
C  CALCULATE NUMBER OF DASHES:
    6 NUM = IFIX(BLEN/DASH)
      REM = FLOAT(NUM+1)*DASH-BLEN
      IF (NUM .EQ.0) GO TO 2
      FRAC = DASH/ALEN
C
C  DO DASHES:
      DO 3 I=1,NUM
      X = X+FRAC*U
      Y = Y+FRAC*V
      GO TO (7,8),ILINE
    7 CALL KANGA1(X,Y,IPEN)
C  CHANGE MODE:
      IPEN = 5-IPEN
      GO TO 3
   8  CONTINUE
      CALL KANGA3(X,Y,DASH/5,LINE-10)
    3 CONTINUE
      GO TO 2
C
    1 REM = -BLEN
C  FINISH OFF
    2 IF (ILINE.EQ.2) GO TO 101
      CALL KANGA1(XXIN,YYIN,IPEN)
 101  X1=XXIN
      Y1=YYIN
 100  RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE ERRMAP
      SUBROUTINE ERRMAP
C
C *** ERRMAP updated by JCM 14 Apr 89 ***
C
CX
CC 5B
CH Calculates the standard deviation of the density in a Fourier map.
CP The results are meaningless if DELTA, the resolution length, is zero.
CP Should be set up in the same way as FOUR1Z
C
CD Uses the same kind of data as FOUR1Z, the normal Fourier routine, but the
CD calculation is much slower.  One should therefore use a coarser sampling
CD grid. Experience shows that the results vary very little throughout the
CD unit cell.
CD
CD Assumes errors in non-equivalent reflections are independent and those in
CD related reflections the same.
CI Reads data from unit LUNI in FORMAT given by MODED in /MAPDA/
CN Old.
C
      DIMENSION FIN(4)
C%
C      DIMENSION H(3,%SY*2%),EH(3),HI(3),K(3),SINCOX(100),SINCOY(100)
      DIMENSION H(3,48),EH(3),HI(3),K(3),SINCOX(100),SINCOY(100)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SCRAT/SUM(10201)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
      NOBSIN=-1
      NUSED=0
      SCALEF=SCALF1*SCALF2
C
C     GET OUT THE SINES AND COSINES REQUIRED IN THE RECURSIONS.
      CALL TRIG(SINCOX,TWOPI*OUTLIM(3,1),(NH-1)/2+1)
      CALL TRIG(SINCOY,TWOPI*OUTLIM(3,2),NK)
C     CLEAR STORE FOR SUMS
      DO 1 I=1,NXY
      SUM(I) = 0.
    1 DENS(I) = 0.
C
C     READ REFLECTIONS FROM STREAM LUNI
C     MODED=1 H,K,L, FCAL,FOBS,DELF
C     MODED=2 H,K,L, ARG(FCAL),PHASE ANGLE,FOBS,DELF
C     MODED=3 H,K,L, A,B,FOBS,DELF
C     MODED=4 H,K,L, FOBS,DELF
   2  NOBSIN=NOBSIN+1
      CALL RDDATA(LUNI,K,HI,FIN,4,IOU)
      IF (IOU .EQ. -9999) GO TO 7
C     SWITCH ACCORDING TO FORMAT OF REFLECTION CARDS (MODED)
      GO TO (3,4,4,5), MODED
   3  F=FIN(2)
      SF=FIN(3)
      GO TO 6
C
   4  F=FIN(3)
      SF=FIN(4)
      GO TO 6
C
   5  F=FIN(1)
      SF=FIN(2)
C     ABSOLUTE PHASE DOESN'T MATTER AS TERMS ARE TO BE SQUARED
C     TEST FOR 0 0 0 REFLECTION
    6 IF ((K(1).EQ.0) .AND. (K(2).EQ.0) .AND. (K(3) .EQ.0)) GO TO 8
      IF (VCTMOD(0.5,HI,2) .GT. SMAX) GO TO 2
C     MULTIPLY BY 2 BECAUSE WE ASSUME -H,-K,-L, IS NOT GIVEN AND WILL
C     NOT BE GENERATED IF NCENT=2
      NUSED=NUSED+1
      SF = 2.*SF*RESOL(HI,DELTA)*SCALEF
      DO 26 N = 1,NOPC
      CALL ROTSYM(HI,EH,N,2)
      IF (N .NE. 1) GO TO 44
      DO 45 I = 1,3
   45 H(I,1) = EH(I)
      MI = 1
      GO TO 46
  44  CALL EQVEC(H,EH,MI,M,NOP)
      IF (M .LE. MI) GO TO 26
      MI = M
   46 IF (NDIM .NE. 2) GO TO 27
C     SYMMETRY MATRICES ALREADY ROTATED TO AXES OF MAP
      IF (EH(3) .NE. 0) GO TO 26
   27 TRA = 0.
      DO 23 I = 1,3
      K(I) = NINT(EH(I))
   23 TRA = TRA + EH(I)*(TRANS(I,N) - OUTLIM(1,I))
C     PHASE AT ORIGIN OF MAP
      TRA = TWOPI*TRA
C
C     NOW CALCULATE THE CONTRIBUTION FROM THIS REFLECTION AT ALL POINTS
      J = 2*IABS(K(1))+1
      SDX = SINCOX(J+1)
      CDX = SINCOX(J)
      IF (K(1) .GT. 0) SDX = -SDX
      J = 2*IABS(K(2))+1
      SDY = SINCOY(J+1)
      CDY = SINCOY(J)
      IF (K(2) .GT. 0) SDY = -SDY
      COX = COS(TRA)*SF
      SOX = SIN(TRA)*SF
      DO 30 IX = 1,NX
      I = IX
      SUM(I) = SUM(I) + COX
      COY = COX*CDY - SOX*SDY
      SOY = SOX*CDY + COX*SDY
      DO 31 IY = 2,NY
      I = I+NX
      SUM(I) = SUM(I) + COY
      SNEW = SOY*CDY + COY*SDY
      COY = COY*CDY - SOY*SDY
   31 SOY = SNEW
      SNEW = SOX*CDX + COX*SDX
      COX = COX*CDX - SOX*SDX
   30 SOX = SNEW
C
C     ACCUMULATE SUM OVER ALL EQUIVALENTS
   26 CONTINUE
C     NOW SQUARE SUM, ADD TO RESULT AND CLEAR FOR NEXT
      DO 32 I = 1,NXY
      DENS(I) = DENS(I) + SUM(I)**2
   32 SUM(I) = 0.
      GO TO 2
C     ADD CONTRIBUTION FROM 0,0,0 REFLECTION
    8 SF = (SF*SCALEF)**2
      DO 9 I = 1,NXY
    9 DENS(I) = DENS(I) + SF
      GO TO 2
C     HAVE RUN OUT OF REFLECTIONS ON STREAM LUNI
C     TAKE SQRT TO GET STANDARD DEVIATION
    7 DO 50 I = 1,NXY
   50 DENS(I) = SQRT(DENS(I))
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE FORIER(IIN,IOP,START)
      SUBROUTINE FORIER(IIN,IOP,START)
C
C *** FORIER updated by PJB 29 Apr 88 ***
C
CX
CC 5B
CH Controls Fourier calculations.
C
CA On entry START, a logical, indicates whether this is the first call of FORIER
CA On exit  IIN indicates how the next map is to be obtained:
CA          IIN = 1 means get back previously saved map
CA                2 means read back pre-calculated map in binary form
CA                3 means calculate map using FOUR1Z
CA                4 means calculate map using FOURGP (general plane)
CA                5 means calculate map using ERRMAP (error map)
CA          IOP indicates how the next map is to be sent out:
CA          IOP contains 1 bit = print
CA                       2 bit = plot
CA                       4 bit = save
C
CP Must be set up by a call of SETFOU, reading the relevant M cards
C
CD If 2D (NDIM=2), only one possible map, a projection, is involved.  If 3D
CD (NDIM=3), several layers may be involved.  Their values of Z are stored
CD in arrays:
CD    ZRDVAL for reading down pre-calculated maps
CD    ZGTVAL for getting back previously saved maps
CD    ZSVVAL for saving maps just calculated
CD    ZPRVAL for printing
CD    ZPLVAL for plotting
CD NDIM=4 is a request for a bounded section
C
      LOGICAL START
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /MAPGT/ZGTVAL(20),ZCGT,IGT,IZGT,IDUMPG
      COMMON /MAPPR/ZPRVAL(20),ZCPR,IPR,IZPR
      COMMON /MAPRD/ZRDVAL(20),ZCRD,IRD,IZRD,IDUMPR
      COMMON /MAPSV/ZSVVAL(20),ZCSV,ISV,IZSV,NDUMPS,NSAV
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C
C DOES NOT OPEN FILE FROM WHICH TO READ REFLECTIONS - ASSUMES USER WANTS
C TO DO IT HIMSELF IN MAIN
C SET ZERO FOR A START
      IOP=0
      IIN=0
C  THEN SWITCH IF NOT STARTING
      IF (START) THEN
      START=.FALSE.
C
C SET FLAG TO SAY NO MAPS SAVED YET (TO BE CONSULTED IN SAVMAP):
      NSAV=0
C
C IF SAVING , OPEN FILE (IF GETTING, FILE IS OPENED IN MAJUST):
      MESSAG='Saved maps'
      NAMFIL='.SAV'
      IF (IZSV .GT. 0) CALL OPNFIL(NDUMPS,1112)
C
C IF READING BINARY PRE-CALCULATED MAP, OPEN FILE:
      MESSAG='Pre-calculated maps'
      IF (IZRD .GT. 0) CALL OPNFIL(IDUMPR,1011)
C
      IF (NDIM .EQ. 2) GO TO 1
C
C 3D - FIND ALL VALUES OF Z IN ALL 5 LISTS:
      IPR=1
      IF (IZPR .EQ. 0) THEN
      ZCPR=99999.
      ELSE
      ZCPR=ZPRVAL(1)
      ENDIF
      IPL=1
      IF (IZPL .EQ. 0) THEN
      ZCPL=99999.
      ELSE
      ZCPL=ZPLVAL(1)
      ENDIF
      ISV=1
      IF (IZSV .EQ. 0) THEN
      ZCSV=99999.
      ELSE
      ZCSV=ZSVVAL(1)
      ENDIF
      IGT=1
      IF (IZGT .EQ. 0) THEN
      ZCGT=99999.
      ELSE
      ZCGT=ZGTVAL(1)
      ENDIF
      IRD=1
      IF (IZRD .EQ. 0) THEN
      ZCRD=99999.
      ELSE
      ZCRD=ZRDVAL(1)
      ENDIF
C
      ENDIF
C  START HERE AFTER FIRST ENTRY
C
      IF (NDIM.EQ.2) GO TO 1
C
C NEXT VALUE OF Z:
      Z=AMIN1(ZCPL,ZCSV,ZCPR,ZCGT,ZCRD)
      IF (Z.GT.99998.) GO TO 100
      OUTLIM(1,3)=Z
C
C DO WE NEED TO GET SAVED FILE FOR THIS VALUE OF Z?
      IF (ABS(ZCGT-Z) .GT. 10.E-5) GO TO 37
      IIN=1
      IGT=IGT+1
      ZCGT=ZGTVAL(IGT)
      IF (IGT.GT.IZGT) ZCGT=99999.
      GO TO 10
C
C DO WE NEED TO READ BINARY FILE FOR THIS VALUE OF Z?
  37  IF (ABS(ZCRD-Z) .GT. 10.E-5) GO TO 97
      IIN=2
      IRD=IRD+1
      ZCRD=ZRDVAL(IRD)
      IF (IRD.GT.IZRD) ZCRD=99999.
      GO TO 10
C
C WE WANT TO CALCULATE A NEW MAP:
  97  IF (MODET .EQ. 2) THEN
      IIN=4
      ELSE
      IF (MODEF .LT. 7) IIN=3
      IF (MODEF .EQ. 7) IIN=5
      ENDIF
C
C DO WE WANT TO PRINT AT THIS VALUE OF Z?
  10  IF (ABS(ZCPR-Z) .GT. 10.E-5) GO TO 17
      IOP=IOP+1
      IPR=IPR+1
      ZCPR=ZPRVAL(IPR)
      IF (IPR.GT.IZPR) ZCPR=99999.
C
C DO WE WANT TO PLOT AT THIS VALUE OF Z?
  17  IF (ABS(ZCPL-Z) .GT. 10.E-5) GO TO 7
      IOP=IOP+2
      IPL=IPL+1
      ZCPL=ZPLVAL(IPL)
      IF (IPL.GT.IZPL) ZCPL=99999.
C
C DO WE WANT TO SAVE THIS VALUE OF Z?
   7  IF (ABS(ZCSV-Z) .GT. 10.E-5) GO TO 8
      IOP=IOP+4
      ISV=ISV+1
      ZCSV=ZSVVAL(ISV)
      IF (ISV.GT.IZSV) ZCSV=99999.
   8  IF (NDIM.NE.4 .OR.IOP.EQ.0) GO TO 100
C
C  SPECIAL FOR BOUNDED SECTIONS
      SECEND=Z
      IF (ZCPL.LT.99999.) SECEND=ZCPL
      IF (ZCPR.LT.99999.) SECEND=AMAX1(SECEND,ZCPR)
      IF (ZCSV.LT. 99999.) SECEND=AMAX1(SECEND,ZCSV)
      IF (ZCPR .LT.99999.) THEN
        IF (ABS(SECEND-ZCPR).GT.10.E-4) THEN
          IPR=IPR-1
          ZCPR=ZPRVAL(IPR)
        ELSE
          IPR=IPR+1
          ZCPR=ZPRVAL(IPR)
          IF (IPR.GT.IZPR) ZCPR=99999.
        ENDIF
      ENDIF
      IF (ZCPL.LT. 99999.) THEN
        IF (ABS(SECEND-ZCPL).GT.10.E-4) THEN
          IPL=IPL-1
          ZCPL=ZPLVAL(IPL)
        ELSE
          IPL=IPL+1
          ZCPL=ZPLVAL(IPL)
          IF (IPL.GT.IZPL) ZCPL=99999.
        ENDIF
      ENDIF
      IF (ZCSV.LT.99999.) THEN
        IF (ABS(SECEND-ZCSV).GT.10.E-4) THEN
          ISV=ISV-1
          ZCSV=ZSVVAL(ISV)
        ELSE
          ISV=ISV+1
          ZCSV=ZSVVAL(ISV)
          IF (ISV.GT.IZSV) ZCSV=99999.
        ENDIF
      ENDIF
      GO TO 100
C
C 2D IS EASIER BECAUSE THERE IS NO Z COUNT:
    1 IF (IZGT .EQ. 0) GO TO 2
      IIN=1
      GO TO 3
   2  IF (IZRD .EQ. 0) GO TO 90
      IIN=2
      GO TO 3
  90  OUTLIM(1,3)=0.
      IIN=3
      IF (MODEF .EQ. 7) IIN=5
   3  IF (IZPR .NE. 0) IOP=IOP+1
      IF (IZPL .NE. 0) IOP=IOP+2
      IF (IZSV .NE. 0) IOP=IOP+4
      GO TO 100
C
  100 RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE FOUINP(K,F,ALPHA,MODED,MODEF,ENDIP)
      SUBROUTINE FOUINP(K,F,ALPHA,MODED,MODEF,ENDIP)
C
C *** FOUINP updated by JCM 14 Apr 89 ***
C
CX
CC 5C
CH Reads one data item for a given type of Fourier, in a given format.
CA On entry MODED indicates the data format type, from M DTYP card
CA     MODED=0: user-supplied routine QFOUIN should set K, F, ALPHA, ENDIP
CA     MODED=1: read H,K,L FCAL,FOBS,(D)
CA     MODED=2: read H,K,L, mod(FCAL), phase angle, FOBS
CA     MODED=3: read H,K,L, A, B, FOBS
CA     MODED=4: read H,K,L, FOBS (or FCAL)
CA On entry MODEF indicates the Fourier type required, from M FTYP card
CA     MODEF=1:   FCAL
CA     MODEF=2:   FOBS (Centrosymmetric)
CA     MODEF=3:   mod(FOBS)*phase(FCAL)
CA     MODEF=4:   FOBS-FCAL
CA     MODEF=5:   (mod(FOBS)-mod(FCAL))*phase(FCAL)
CA     MODEF=6:   FOBS*FOBS (Patterson)
CA On exit  K is a 1x3 integer vector holding h,k,l
CA          F is a 1x2 vector holding whichever of FOBS, etc were requested
CA          ALPHA, if relevant, holds the phase
CA          ENDIP is a logical set TRUE if the end of the input has occurred.
CO Checks that MODED and MODEF are compatible and complains and stops if not.
C
      LOGICAL ENDIP
      DIMENSION K(3),F(3),H(3)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      ENDIP=.FALSE.
      IF (MODED .GT. 0) GO TO 9
      CALL QFOUIN(K,F,ALPHA,ENDIP)
      GO TO 100
C
   9  CALL RDDATA(LUNI,K,H,F,3,IOU)
      IF (IOU .EQ. -9999) GO TO 101
      GO TO (11,12,13,14) , MODED
  11  ALPHA = PIBY2 - SIGN(PIBY2,F(1))
      GO TO 100
C
  12  ALPHA=F(2)
      F(2)=F(3)
      GO TO (100,99,100,99,100,100),MODEF
C
  13  FA=F(1)
      FB=F(2)
      F(2)=F(3)
      F(1) = SQRT(FA*FA+FB*FB)
      IF (F(1) .EQ. 0.) GO TO 9
      ALPHA = ATAN2(FB,FA)
      GO TO (100,99,100,99,100,100),MODEF
C
  14  F(2)=F(1)
      GO TO (99,100,99,99,99,100),MODEF
C
C ERROR:
  99  CALL ERRMES(1,0,'MODEF & MODED values incompatible')
C
C  MARK END OF DATA
  101 ENDIP=.TRUE.
  100 RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE FOUR1D
      SUBROUTINE FOUR1D
C
C *** FOUR1D by PJB Dec 85 ***
C
CX
CC 5B
CH Calculates a Fourier along a general line.
CP SETFOU should have been obeyed to read M, N and I  cards and set up
CP the calculation.
CD Uses FOUINP to allow all different data input and Fourier types.
CD Puts calculated 1-D Fourier in array DENS in /MAPDA
C
CN Expects that the h,k,l values cover a suitable asymmetric unit, from which it
CN uses the given symmetry to generate an entire reciprocal space full.  If the
CN data stray outside one asymmetric unit, some h,k,l values will occur more
CN than once.
C
      COMPLEX RHO(1500),CE(100,3),FAC(2),Y,YM
      LOGICAL USED,ENDIP
C%
C      DIMENSION H(3,%SY*2%),HI(3),K(3),F(2)
      DIMENSION H(3,48),HI(3),K(3),F(2)
      DIMENSION KLIM(3),SF(2),ALPH(2)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      EQUIVALENCE (DENS,RHO),(CE,DENS(3001)),(KLIM(1),NH)
C
C NOBSIN COUNTS DATA READ, NUSED COUNTS THOSE USED
      NOBSIN=-1
      NUSED=0
      SCALEF=SCALF1*SCALF2
C
      NY=4*NX
C     CLEAR STORE FOR SUM OVER L
      DO 1 M = 1,NY
    1 DENS(M) = 0.
C
C  GET RQUIRED TRIG FUNCTIONS
      DO 2 I=1,3
      KK=KLIM(I)+1
      AMP=-TWOPI*OUTLIM(3,I)
      CALL TRIG(CE(1,I),AMP,KK)
    2 CONTINUE
C
C  NOW READ REFLECTIONS
    7 USED=.FALSE.
      CALL FOUINP(K,F,ALPHA,MODED,MODEF,ENDIP)
      IF (ENDIP) GO TO 20
C
      NOBSIN=NOBSIN+1
      CALL INDFLO(HI,K)
C DO NOT USE IF SIN THETA/LAMBDA TOO LARGE:
      VL =VCTMOD(0.5,HI,2)
      IF (VL .GT. SMAX) GO TO 7
C
C APPLY RESOLUTION FUNCTION AND USER'S SCALE:
      AMP = RESOL(HI,DELTA)*SCALEF
      DO 3 MODE=1,2
      CALL CHOOSF(F,SF(MODE),ALPHA,ALPH(MODE),MODE)
C SF AND ALPHA NOW SET UP:
C
C DOUBLE ALL BUT 000 REFLN. AS WE ARE ONLY USING HALF RECIPROCAL SPACE:
      IF  ((K(1).NE.0).OR.(K(2).NE.0).OR.(K(3).NE.0))
     & SF(MODE)=2.*SF(MODE)
C
      SF(MODE)=SF(MODE)*AMP
    3 CONTINUE
C
C DEAL WITH SYMMETRY EQUIVALENTS:
      MI=0
C TO MAKE EQVEC ACCEPT FIRST SET OF INDICES
      DO 8 N = 1,NOPC
      CALL SYMEQU(HI,H,K,N,MI,UU,BETA)
      IF (UU.EQ.0.) GO TO 8
      USED=.TRUE.
      FAC(1)=SF(1)*CEXP(CMPLX(0.,UU*ALPH(1)+BETA))
      FAC(2)=SF(2)*CEXP(CMPLX(0.,UU*ALPH(2)+BETA))
      DO 9 I=1,NX
      RHO(I)=RHO(I)+FAC(1)
      RHO(NX+I)=RHO(NX+I)+FAC(2)
      YM=CMPLX(1.,0.)
      DO 10 J=1,3
      Y=CE(IABS(K(J))+1,J)
      IF (K(J).LT.0) Y=CONJG(Y)
   10 YM=YM*Y
      FAC(1)=FAC(1)*YM
      FAC(2)=FAC(2)*YM
    9 CONTINUE
C
    8 CONTINUE
      IF (USED) NUSED=NUSED+1
      GO TO 7
C
C  HERE ON END OF DATA
   20 NY=2*NX
      II=2*NY
      DO 11 I=1,NY,2
      DENS(II+I)=DENS(NY+I)-DENS(I)
   11 CONTINUE
C
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE FOUR1Z
      SUBROUTINE FOUR1Z
C
C *** FOUR1Z corrected by PJB 17-Jun-1994 ***
C
CX
CC 5B
CH Calculates 1 layer of Fourier sum : a section if 3D, a projection if 2D,
CH or a bounded section if "4D".
C
CP Must be set up by call to SETFOU to read M, N, I cards
C
CD Uses FOUINP to allow all different data input and Fourier types.
CD Puts calculated  Fourier map in array DENS in /MAPDA
C
CN Expects that the h,k,l values cover a suitable asymmetric unit, from which it
CN uses the given symmetry to generate an entire reciprocal space full.  If the
CN data stray outside one asymmetric unit, some h,k,l values will occur more
CN than once.
CN
CN Ignores FRIEDL - if non-centrosymmetric, and Friedel's law not to be assumed,
CN the user must do something to combine F(H,K,L) and F(-H,-K,-L) outside
CN FOUR1Z.
C
      LOGICAL USED,ENDIP
C%
C      DIMENSION H(3,%SY*2%),HI(3),K(3),F(3),SINCO(200)
      DIMENSION H(3,48),HI(3),K(3),F(3),SINCO(200)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SCRAT/SUM(10201)
C
C NOBSIN COUNTS DATA READ, NUSED COUNTS THOSE USED
      NOBSIN=0
      NUSED=0
      SCALEF=SCALF1*SCALF2
C
C SET ORIGIN OF SECTION:
      DO 2 I=1,3
   2  SECZER(I)=OUTLIM(1,I)
C
C     CLEAR STORE FOR SUM OVER L
      DO 1 M = 1,NHK
    1 DENS(M) = 0.
C
C JJ=H MAX + 1
      JJ = ((NH-1)/2)+1
C  NOW READ REFLECTIONS
    7 USED=.FALSE.
      CALL FOUINP(K,F,ALPHA,MODED,MODEF,ENDIP)
      IF (ENDIP) GO TO 20
C
      NOBSIN=NOBSIN+1
      CALL INDFLO(HI,K)
C DO NOT USE IF SIN THETA/LAMBDA TOO LARGE:
      IF (VCTMOD(0.5,HI,2) .GT. SMAX) GO TO 7
C
C APPLY RESOLUTION FUNCTION AND USER'S SCALE:
      AMP = RESOL(HI,DELTA)*SCALEF
      CALL CHOOSF(F,SF,ALPHA,ALPH,MODEF)
C SF AND ALPHA NOW SET UP:
C
C DOUBLE ALL BUT 000 REFLN. AS WE ARE ONLY USING HALF RECIPROCAL SPACE:
      IF  ((K(1).NE.0).OR.(K(2).NE.0).OR.(K(3).NE.0))
     & SF=2.*SF
C
      SF=SF*AMP
    3 CONTINUE
C
C DEAL WITH SYMMETRY EQUIVALENTS:
C TO MAKE EQVEC ACCEPT FIRST SET OF INDICES
      MI=0
C TRANSFORM HI (GIVEN H) TO OCCUPY FIRST PLACE IN TABLE FOR EQVEC:
      CALL GMEQ(HI,H(1,1),1,3)
      CALL GMPRD(H(1,1),U,HI,1,3,3)
      DO 8 N = 1,NOPC
      CALL SYMEQU(HI,H,K,N,MI,UU,BETA)
      IF (UU.EQ.0.) GO TO 8
      USED=.TRUE.
C49 - SECTION updated by PJB
      ALPHA1=ALPH*SIGN(1.,UU)+BETA
      SFUU=SF*ABS(UU)
      KF=1
      KH = 2*(NK*(K(1)+JJ-1)+K(2)+KF)
      DENS(KH-1) = DENS(KH-1)+SFUU*COS(ALPHA1)
      DENS(KH) = DENS(KH) + SFUU*SIN(ALPHA1)
C49 - END OF UPDATED SECTION
C
    8 CONTINUE
      IF (USED) NUSED=NUSED+1
      GO TO 7
C
C     SUM OVER L COMPLETED - ALL REFLECTIONS HAVE BEEN READ
C CLEAR STORE FOR SUM OVER H:
  20  DO 30 N = 1,NKX
   30 SUM(N) = 0.
C
C SET UP COS AND SIN TABLES:
      CALL TRIG (SINCO,TWOPI*OUTLIM(3,1),JJ)
      KH = 1
      DO 34 IH=1,NH
      KXI = 1
      J = 2*IABS(IH-JJ)+1
      SD = -SINCO(J+1)
      CD = SINCO(J)
      IF (IH .LT. JJ) SD = -SD
      DO 34 IK=1,NK
      KX = KXI
      IF ((DENS(KH) .EQ. 0.) .AND. (DENS(KH+1) .EQ.0.)) GO TO 39
      C = DENS(KH)
      S = DENS(KH+1)
      DO 38 N=1,NX
      SUM(KX) = SUM(KX) + C
      SUM(KX+1) = SUM(KX+1) + S
      CNEW = C*CD - S*SD
      S = C*SD + S*CD
      C = CNEW
   38 KX = KX+2
   39 KH = KH+2
   34 KXI = KXI + 2*NX
C
C     SUM OVER H COMPLETE
C FINAL SUM OVER K:
      KF=NK
      CALL TRIG(SINCO,TWOPI*OUTLIM(3,2),KF)
C CLEAR DENS FOR ANSWERS:
      DO 40 N = 1,NXY
   40 DENS(N) = 0.
      KX = 1
      DO 41 IK=1,NK
      KK = 2*IK
      CD = SINCO(KK-1)
      SD = -SINCO(KK)
      DO 42 N = 1,NX
      M = N
      IF ((SUM(KX) .EQ. 0.) .AND. (SUM(KX+1) .EQ. 0.)) GO TO 42
      C = SUM(KX)
      S = SUM(KX+1)
      DO 43 IY = 1,NY
      DENS(M) = DENS(M) + C
      CNEW = C*CD - S*SD
      S = C*SD + S*CD
      C = CNEW
   43 M = M+NX
   42 KX = KX+2
   41 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE FOURGP
      SUBROUTINE FOURGP
C
C *** FOURGP by PJB Dec 85 ***
C
CX
CC 5B
CH Calculates a Fourier on a general plane.
C
CP Must be set up by call to SETFOU to read M, N, I cards
C
CD Uses FOUINP to allow all different data input and Fourier types.
CD Puts calculated  Fourier map in array DENS in /MAPDA
C
CN Expects that the h,k,l values cover a suitable asymmetric unit, from which it
CN uses the given symmetry to generate an entire reciprocal space full.  If the
CN data stray outside one asymmetric unit, some h,k,l values will occur more
CN than once.
CN
CN Ignores FRIEDL - if non-centrosymmetric, and Friedel's law not to be assumed,
CN the user must do something to combine F(H,K,L) and F(-H,-K,-L) outside
CN FOURGP
C
      COMPLEX RHO,CE,Y,YM(2),VAL
      LOGICAL USED,ENDIP
C%
C      DIMENSION H(3,%SY*2%),HI(3),K(3),F(2)
      DIMENSION H(3,48),HI(3),K(3),F(2)
      DIMENSION KLIM(3)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SCRAT/RHO(100),CE(100,6)
      EQUIVALENCE (KLIM(1),NH)
C
C NOBSIN COUNTS DATA READ, NUSED COUNTS THOSE USED
      NOBSIN=0
      NUSED=0
      SCALEF=SCALF1*SCALF2
C
C     CLEAR STORE FOR SUM
      DO 1 M = 1,NXY
    1 DENS(M) = 0.
C
C
C  GET REQUIRED TRIG FUNCTIONS, AND TRANSFORMED STARTING POS
      L=1
      DO 2 J=1,3
      SECZER(J)=0.
      DO 2 I=1,3
      IF (J.EQ.3) GO TO 4
      KK=KLIM(I)+1
      AMP=-TWOPI*OUTLIM(3,J)*U(I,J)
      CALL TRIG(CE(1,L),AMP,KK)
      L=L+1
    4 SECZER(J)=SECZER(J)+OUTLIM(1,I)*U(J,I)
    2 CONTINUE
C
C  NOW READ REFLECTIONS
    7 USED=.FALSE.
      CALL FOUINP(K,F,ALPHA,MODED,MODEF,ENDIP)
      IF (ENDIP) GO TO 100
C
      NOBSIN=NOBSIN+1
      CALL INDFLO(HI,K)
C DO NOT USE IF SIN THETA/LAMBDA TOO LARGE:
      IF (VCTMOD(0.5,HI,2) .GT. SMAX) GO TO 7
C
C APPLY RESOLUTION FUNCTION AND USER'S SCALE:
      AMP = RESOL(HI,DELTA)*SCALEF
      CALL CHOOSF(F,SF,ALPHA,ALPH,MODEF)
C SF AND ALPHA NOW SET UP:
C
C DOUBLE ALL BUT 000 REFLN. AS WE ARE ONLY USING HALF RECIPROCAL SPACE:
      IF  ((K(1).NE.0).OR.(K(2).NE.0).OR.(K(3).NE.0))
     & SF=2.*SF
C
      SF=SF*AMP
    3 CONTINUE
C
C DEAL WITH SYMMETRY EQUIVALENTS:
      MI=0
C TO MAKE EQVEC ACCEPT FIRST SET OF INDICES
      DO 8 N = 1,NOPC
      CALL SYMEQU(HI,H,K,N,MI,UU,BETA)
      IF (UU.EQ.0.) GO TO 8
      USED=.TRUE.
      VAL=SF*CEXP(CMPLX(0.,UU*ALPH+BETA))
      L=1
      DO 9 I=1,2
      YM(I)=CMPLX(1.,0.)
      DO 9 J=1,3
      Y=CE(IABS(K(J))+1,L)
      L=L+1
      IF (K(J).LT.0) Y=CONJG(Y)
   9  YM(I)=YM(I)*Y
C
      L=1
      RHO(1)=VAL
      DO 10 J=1,NY
      VAL=RHO(J)
      RHO(J+1)=RHO(J)*YM(2)
      DO 10 I=1,NX
      DENS(L)=DENS(L)+REAL(VAL)
      L=L+1
      VAL=VAL*YM(1)
   10 CONTINUE
C
    8 CONTINUE
      IF (USED) NUSED=NUSED+1
      GO TO 7
C
C  HERE ON END OF DATA
  100 RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE FRAME(X1,Y1,X2,Y2)
      SUBROUTINE FRAME(X1,Y1,X2,Y2)
C
C *** FRAME by JCM 24 Nov 83 ***
C
CX
CC 14C
CH Draws a rectangle in the plotting context.
CA On entry (X1,Y1) and (X2,Y2) are the coordinates of opposite corners of
CA the required rectangle.  These are in "current coordinates"
CP The plotting must have been set up by, e.g. STPLOT, and a suitable space
CP selected by SPCSET.
C
      CALL KANGA1(X1,Y1,3)
      CALL KANGA1(X1,Y2,2)
      CALL KANGA1(X2,Y2,2)
      CALL KANGA1(X2,Y1,2)
      CALL KANGA1(X1,Y1,2)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE GETMAP
      SUBROUTINE GETMAP
C
C *** GETMAP updated C19 by JCM 22 Aug 86 ***
C
CX
CC 5B
CH Retrieves 1 Fourier map previously filed using SAVMAP.
CP Assumes unit IDUMPG is positioned so that reading from it will
CP produce the "next" map dumped there.
CP
CP The required Z value must be set in OUTLIM(1,3)
C
CD Continues reading down maps until it finds one for the given Z.
CD
CD If no map is dumped for this value of Z, will eventually read the
CD trailer record of file IDUMPG and complain.
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /MAPGT/ZGTVAL(20),ZCGT,IGT,IZGT,IDUMPG
C>> JCC Use error flag instead of STOP

	INTEGER IBMBER
	COMMON / CCSLER / IBMBER 
C
   1  READ(IDUMPG) ZI,NOBSIN,NUSED,NXX,NYY
      IF (NXX .EQ. NX .AND. NYY .EQ. NY) GO TO 3
      WRITE (LPT,3001) NXX,NYY,NX,NY
      WRITE (ITO,3001) NXX,NYY,NX,NY
3001  FORMAT (/' ERROR ** MAP TO BE READ OF SIZE',I3,' BY',I3,
     & ' BUT CURRENT SET SIZE IS',I3,' BY',I3)
      IBMBER = 1
	RETURN
C
C CHECK NOT READING TRAILER:
   3  IF (ZI .GE. 99998.) CALL ERRRE2(OUTLIM(1,3),0,
     & 'no dumped map for z=',' ')
C
C READ RECTANGULAR ARRAY OF PREVIOUSLY CALCULATED MAP:
      I=0
      DO 4 IY=1,NY
      READ (IDUMPG) (DENS(I+J),J=1,NX)
   4  I=I+NX
C
C IF WRONG VALUE OF Z, BACK FOR NEXT:
      IF (ABS(OUTLIM(1,3)-ZI) .GT. 10.E-5) GO TO 1
      WRITE (LPT,2000) OUTLIM(1,3),NX,NY
2000  FORMAT(/' For Z=',F10.4,' map of size',I4,' by',I4,
     & ' read')
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE GETSCL(VMIN,VMAX,L)
      SUBROUTINE GETSCL(VMIN,VMAX,L)
C
C *** GETSCL updated by PJB Sep 87 ***
C
CX
CC 14A
CH Chooses a sensible scale for a graph.
CA On entry VMIN is minimum value to be plotted
CA          VMAX is maximum value to be plotted
CA          L=1 for x axis
CA            2 for y axis
CP X(L,2) must be set up as below:
CD The vector X(I,J) in /PLTS defines how the graph will be drawn.
CD           I=1  for X-axis, I=2 for Y-axis.
CD           J=1  length of axis in user units
CD           J=2  length of axis in cms.
CD           J=3  division of axis in user units
CD           J=4  minimum value in user units
CD           J=5  position of plotted axis in user units
CD Sets X(L,J) for J=1,3,4,5.
CO If VMAX and VMIN are not sensible, complains and bombs an error flag.
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PLTS/X(2,5),S(2),CH,XS,ISIG(2),YS,NDIVS(2,2)

C>> JCC Use bomb out flag instead of stop
C
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
      AMAX=VMAX
      AMIN=VMIN
      XX=AMAX-AMIN
      I=0
C
    1 IF (XX.GT.10.) GO TO 2
    4 IF (XX.GT.1.) GO TO 3
      I=I-1
      IF (I.LT.-8) THEN
        WRITE (ITO,3000) AMAX,AMIN
3000  FORMAT (' Range from ',E12.4,' to ',E12.4,' too small for GETSCL')
C>> Was STOP
       CALL BMBOUT
	 RETURN
      ENDIF
      XX=XX*10.
      GO TO 4
C
    2 I=I+1
      IF (I.GT.8) THEN
        WRITE(ITO,3001) AMAX,AMIN
3001  FORMAT (' Range from ',E12.4,' to ',E12.4,' too big for GETSCL')
C>>        STOP
        CALL BMBOUT
	  RETURN
      ENDIF
      XX=XX/10.
      GO TO 1
C
    3 AMUL=10.**I
      STEP=2.
      IF (XX.LE.5.) STEP=1.
      IF (XX.LE.2.5) STEP=.5
      IF (XX.LE.1.4) STEP=.2
      BIGSTP=AMUL*STEP
      STEPX=BIGSTP/5.
      BIT=STEPX*0.49
C  SET POSITION OF AXIS TO BE PLOTTED
C BIGSTP USED INSTEAD OF ZERO FOR COSMETIC REASONS:
      IF (AMIN.LE.BIGSTP  .AND. AMAX.GE.-BIGSTP) THEN
        X(L,5)=0.
        IF (AMIN .GT. 0.) AMIN=0.
        IF (AMAX .LT. 0.) AMAX=0.
      ELSE
C  ORIGIN OUTSIDE GRAPH
        IF (AMIN .LE. 0.) THEN
          X(L,5)=FLOAT(IFIX(AMIN/BIGSTP))*BIGSTP
        ELSE
          X(L,5)=FLOAT(IFIX((AMIN-BIT)/BIGSTP)+1)*BIGSTP
        ENDIF
      ENDIF
      NDIVS(L,1)=NINT((ABS(AMIN-X(L,5))+BIT)/STEPX)
      NDIVS(L,2)=NINT((ABS(AMAX-X(L,5))+BIT)/STEPX)
      X(L,1)=FLOAT(NDIVS(L,1)+NDIVS(L,2))*STEPX
      X(L,3)=BIGSTP
      X(L,4)=X(L,5)-FLOAT(NDIVS(L,1))*STEPX
      ISIG(L)=I
      IF (STEP.LT.1) ISIG(L)=I-1
      RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE INPUTM
      SUBROUTINE INPUTM
C
C *** INPUTM updated by PJB 29 Apr 88 ***
C
CX
CC 5A
CH Reads and interprets all "M" cards.
CD Takes the information from "M" cards into the map or contouring COMMON,
CD usually as a prelude to a Fourier calculation
CD "M" cards have a significant word in columns 3,4,5,6.  Possible words are:
CD    M NDIM     Number of dimensions for Fourier, 2 (projection), 3 (3D)
CD               or 4 (bounded section).
CD    M FTYP     Fourier type (see below).
CD    M MESH     Output mesh - 6 numbers, being:
CD               X(START) X(END) X(STEP), and the same in Y
CD    M AXES     Matrix of 9 integers turning the Fourier to a different
CD               orientation, or 9 reals asking for a plane section.
CD    M PRIN     By itself means "print calculated map".  Followed by a
CD               list of Z values, means "print only at these selected
CD               Z values".  If absent, the default is not to print anything,
CD               so if there are no "M PRIN", "M PLOT" or "M SAVE" cards the
CD               run will not tell the user much.
CD    M PLOT     Present if plotting required - also gives values of Z at which
CD               plotting is required if NDIM=3, on several cards if necessary.
CD    M CM/A     If plotting, number of centimetres of plotter paper per
CD               Angstrom.
CD    M CONT     If plotting, list of required contour values (may be several
CD               cards, all starting M CONT)
CD    M SAVE     Save the calculated map(s) on a named file in such a way that
CD               a subsequent run with an "M GET" card will retrieve it/them
CD               with a view to drawing another contour map.  Details in the
CD               specification of SUBROUTINE FORIER.
CD    M GET      Do not calculate a map at all - read an already calculated map
CD               from unit IDUMPG, and interpret only those cards which make
CD               sense - e.g. the user may alter contours, scale of map in
CD               cms/Angstrom, etc, but he may not alter cell dimensions, space
CD               group, theta maximum etc.
CD    M DTYP     data input type
CD    M DELT     delta for resolution function
CD    M SCAL     scale to multiply Fourier coefficients
CD    M SMAX     sin theta/lambda maximun for this particular run
CD    M READ     do not calculate map - take the crystal data cards on trust,
CD               and read from a binary file some pre-calculated map.
C
CI Reads all "M" cards
CO Writes its findings on unit LPT.
C
      CHARACTER *4 MWD,MTABLE(15)
      DIMENSION IU(3,3)
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /MAPGT/ZGTVAL(20),ZCGT,IGT,IZGT,IDUMPG
      COMMON /MAPPR/ZPRVAL(20),ZCPR,IPR,IZPR
      COMMON /MAPRD/ZRDVAL(20),ZCRD,IRD,IZRD,IDUMPR
      COMMON /MAPSV/ZSVVAL(20),ZCSV,ISV,IZSV,NDUMPS,NSAV
      COMMON /MREAD/IMREAD(15)
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
      DATA MTABLE/'NDIM','FTYP','MESH','AXES','PRIN','PLOT',
     & 'CM/A','CONT','SAVE','READ','DTYP','DELT','SCAL','GET',
     & 'SMAX'/
C
C SET "NO M CARDS READ":
C%
C      CALL JGMZER(IMREAD,1,%MCRD%)
      CALL JGMZER(IMREAD,1,15)
C
C INITIALISE COUNTS OF ITEMS WHICH MAY COME ON MORE THAN 1 CARD:
      IZPR=0
      IZPL=0
      IZSV=0
      IZGT=0
      IZRD=0
      NCONT=0
C
C READ ALL "M" CARDS:
      INREAD(13)=-IABS(INREAD(13))
      ID=IABS(INREAD(13))
      NCARD=ICDNO(13)
      IF (NCARD .LE. 0) THEN
        CALL MESS(LPT,1,'No "M" cards given')
        GO TO 100
      ENDIF
C
      DO 3 ICD=1,NCARD
      CALL CARDIN(ID)
      ID=ID+NYZ
      CALL RDWORD(MWD,LEN,3,IPT,80,0,IER)
C%
C  62  L=NCFIND(MWD,MTABLE,%MCRD%)
  62  L=NCFIND(MWD,MTABLE,15)
      IF (L .LE. 0) THEN
        CALL ERRCH2(MWD,2,'cannot recognise word','on "M" card')
        GO TO 3
      ENDIF
C
C SET "HAVE READ PARTICULAR WORD" (COUNTING NUMBER OF CARDS), THEN BRANCH:
   4  IMREAD(L)=IMREAD(L)+1
      GO TO (31,32,33,34,35,36,37,38,39,40,41,42,43,44,45) , L
C
C M NDIM:
C READ 1 INTEGER BEING NUMBER OF DIMENSIONS FOR FOURIER, 2 OR 3:
C NDIM=4 MEANS CALCULATE BOUNDED SECTIONS
  31  CALL RDINTG(NDIM,IPT,IPT,80,IER)
      IF (NDIM .EQ. 2) THEN
        CALL MESS(LPT,1,'Fourier projection required')
      ELSE IF (NDIM .EQ. 3) THEN
        CALL MESS(LPT,1,'3D Fourier required')
      ELSE IF (NDIM .EQ. 4) THEN
        CALL MESS(LPT,1,'Bounded sections of Fourier required')
      ELSE
        CALL ERRIN2(NDIM,2,'Number of dimensions on M NDIM card =',
     &  ' - only 2, 3 or 4 allowed')
      ENDIF
      GO TO 63
C
C M FTYP:
C READ 1 INTEGER GIVING TYPE OF FOURIER CALCULATION REQUIRED:
  32  CALL RDINTG(MODEF,IPT,IPT,80,IER)
      IF (MODEF .GE.1 .OR. MODEF .LT.8) GO TO 7
      CALL ERRIN2(MODEF,2,'Fourier type on M FTYP card =',
     & ' - only 1-7 allowed')
      GO TO 63
   7  WRITE (LPT,2003) MODEF
2003  FORMAT (/' Fourier type ',I3,' -')
      GO TO (21,22,23,24,25,26,27) , MODEF
C
  21  CALL MESS(LPT,0,'coeffs are F(cal)')
      GO TO 63
C
  22  CALL MESS(LPT,0,'coeffs are F(obs)')
      GO TO 63
C
  23  CALL MESS(LPT,0,'coeffs are mod(F(obs)*phase F(cal)')
      GO TO 63
C
  24  CALL MESS(LPT,0,'coeffs are F(obs)-F(cal)')
      GO TO 63
C
  25  CALL MESS(LPT,0,'coeffs are (mod(F(obs)-mod(F(cal))'//
     & ' * phase(F(cal))')
      GO TO 63
C
  26  CALL MESS(LPT,0,'coeffs are F(obs) sqrd '//
     & '(for Patterson function)')
      GO TO 63
C
  27  CALL MESS(LPT,0,'standard deviation of electron density')
      GO TO 63
C
C M MESH:
C READ 6 NUMBERS GIVING OUTPUT MESH IN X AND Y:
  33  DO 8 I=1,2
      DO 8 J=1,3
      CALL RDREAL(OUTLIM(J,I),IPT,IPT,80,IER)
      IF (IER .NE. 0 .AND. IER .NE. 100) IERR=IERR+1
   8  CONTINUE
   9  WRITE (LPT,2004) ((OUTLIM(I,J),I=1,3),J=1,2)
2004  FORMAT (/' Mesh for output of map is:  Initial     Final   Step'/
     & 2(26X,3F10.5/))
      GO TO 3
C
C M AXES:
C READ 9 INTEGERS  (OR 9 REALS) GIVING MATRIX TO TURN FOURIER MAP:
  34  IPKEEP=IPT
      DO 10 I=1,3
      DO 10 J=1,3
      CALL RDINTG(IU(J,I),IPT,IPT,80,IER)
C DETECT DECIMAL POINT:
      IF (IER .EQ. -1) GO TO 60
      IF (IER .NE. 0 .AND. IER .NE. 100) IERR=IERR+1
      U(J,I)=FLOAT(IU(J,I))
  10  CONTINUE
C
      WRITE (LPT,2005) IU
2005  FORMAT (/' Orientation matrix:'/3(4X,3I5/))
      MODET=1
      GO TO 3
C
C  HERE FOR GENERAL ORIENTATION
   60 IPT=IPKEEP
      DO 61 I=1,3
      DO 61 J=1,3
      CALL RDREAL(U(J,I),IPT,IPT,80,IER)
      IF (IER.NE.0 .AND. IER .NE.100) IERR=IERR+1
   61 CONTINUE
      WRITE (LPT,2020) U
 2020 FORMAT (/' General Fourier section with x parallel to',3F8.4/
     & 31X,'y         to',3F8.4/31X,'z         to',3F8.4)
      MODET=2
      GO TO 3
C
C M PRIN:
C READ PRINTING INSTRUCTIONS - A NUMBER OF Z VALUES (OR NOTHING) - MAY BE MORE
C THAN 1 CARD:
  35  IZKEEP=IZPR+1
C%
C      CALL RDNUMS(ZPRVAL(IZKEEP),IPT,%PRIN%,NUM,IER)
      CALL RDNUMS(ZPRVAL(IZKEEP),IPT,20,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      IZPR=IZPR+NUM
      IF (NUM .NE. 0) GO TO 3
      CALL MESS(LPT,1,'Print map')
      IZPR=1
      GO TO 3
C
C M PLOT:
C  READ NUMBER OF VALUES OF Z AT WHICH TO PLOT - MAY BE MORE THAN
C  ONE CARD:
  36  IZKEEP=IZPL+1
C%
C      CALL RDNUMS(ZPLVAL(IZKEEP),IPT,%PLOT%,NUM,IER)
      CALL RDNUMS(ZPLVAL(IZKEEP),IPT,20,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      IZPL=IZPL+NUM
      IF (NUM .NE. 0) GO TO 3
      CALL MESS(LPT,1,'Plot map')
      IZPL=1
      GO TO 3
C
C M CM/A - READ 1 REAL BEING THE SCALE OF PLOTTED MAPS IN CMS/ANGSTROM.
  37  CALL RDREAL(SCALMP,IPT,IPT,80,IER)
      IF (IER .NE. 0) IERR=IERR+1
      WRITE (LPT,2010) SCALMP
2010  FORMAT (/' Plot in',F10.4,' cms/Angstrom')
      GO TO 63
C
C M CONT - READ SOME CONTOURS TO PLOT - MAY BE MORE THAN 1 CARD:
  38  IZKEEP=NCONT+1
C%
C      CALL RDNUMS(CONT(IZKEEP),IPT,%CONT%,NUM,IER)
      CALL RDNUMS(CONT(IZKEEP),IPT,50,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      NCONT=NCONT+NUM
      IF (NUM .EQ. 0) GO TO 16
      CALL MESS(LPT,1,'Contour values')
      CALL PRILIS(CONT,IZKEEP,NCONT)
      GO TO 3
  16  CALL MESS(LPT,1,'No contours on card')
      GO TO 3
C
C M SAVE - READ Z VALUES IF GIVEN, AND KEEP INSTRUCTION TO DUMP
  39  IZKEEP=IZSV+1
C%
C      CALL RDNUMS(ZSVVAL(IZKEEP),IPT,%SAVE%,NUM,IER)
      CALL RDNUMS(ZSVVAL(IZKEEP),IPT,20,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      IZSV=IZSV+NUM
      IF (NUM .NE. 0) GO TO 3
      CALL MESS(LPT,1,'Save calculated map')
      IZSV=1
      GO TO 3
C
C M GET:
C READ NUMBER OF Z VALUES FOR WHICH MAPS REQUIRED TO BE GOT FROM SAVED FILE:
  44  IZKEEP=IZGT+1
C%
C      CALL RDNUMS(ZGTVAL(IZKEEP),IPT,%GETM%,NUM,IER)
      CALL RDNUMS(ZGTVAL(IZKEEP),IPT,20,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      IZGT=IZGT+NUM
      IF (NUM .GT. 0) THEN
        CALL MESS(LPT,1,
     &  'Undump previously calculated map for z values ')
        CALL PRILIS(ZGTVAL,IZKEEP,IZGT)
        GO TO 3
      ENDIF
      CALL MESS(LPT,1,'Undump previously calculated map')
      IZGT=1
      GO TO 3
C
C M READ:
C READ NUMBER OF Z VALUES FOR WHICH MAPS REQUIRED TO BE READ FROM BINARY:
  40  IZKEEP=IZRD+1
C%
C      CALL RDNUMS(ZRDVAL(IZKEEP),IPT,%READ%,NUM,IER)
      CALL RDNUMS(ZRDVAL(IZKEEP),IPT,20,NUM,IER)
      IF (IER .NE. 0) IERR=IERR+1
      IZRD=IZRD+NUM
      IF (NUM .GT. 0) THEN
        CALL MESS(LPT,1,'Read previously calculated map for z values ')
        CALL PRILIS(ZRDVAL,IZKEEP,IZRD)
        GO TO 3
      ENDIF
      CALL MESS(LPT,1,'Read previously calculated map')
      IZRD=1
      GO TO 3
C
C M DTYP:
C READ 1 INTEGER INTO MODED
  41  CALL RDINTG(MODED,IPT,IPT,80,IER)
      IF (IER .NE. 0 .AND. IER .NE. 100) IERR=IERR+1
      WRITE (LPT,2030) MODED
2030  FORMAT (/' Data input format type',I3)
      IF ((MODED .LT. 0) .OR. (MODED .GE. 5)) THEN
        CALL ERRMES(1,1,'Type is unacceptable')
        GO TO 63
      ENDIF
C
C MODED CHECKED OK - PRINT:
      GO TO (50,51,52,53,54) , MODED+1
C MODE 0 MEANS USER WILL SUPPLY INPUT ROUTINE TO READ OWN FORMAT:
  50  CALL MESS(LPT,0,'User to supply SUBROUTINE QFOUIN(K,F,ALPHA)'//
     & ' to read own format of data')
      GO TO 63
C
C MODES 1-4 AS IN MK2:
  51  CALL MESS (LPT,0,'h,k,l, F(cal), F(obs), possible Diff')
      GO TO 13
C
  52  CALL MESS(LPT,0,'h,k,l, mod F(cal), phase, F(obs)')
      GO TO 13
C
  53  CALL MESS(LPT,0,'h,k,l, A(cal), B(cal), F(obs)')
      GO TO 13
C
  54  CALL MESS(LPT,0,'h,k,l, F')
      GO TO 13
C
  13  CALL MESS(LPT,0,'In format 3I5,several F')
      GO TO 63
C
C M DELT:
C READ 1 REAL TO DELTA
  42  CALL RDREAL(DELTA,IPT,IPT,80,IER)
      D2=DELTA*2.
      WRITE (LPT,2011) D2
2011  FORMAT (/' Data to be averaged over a cube of edge ',F10.4)
      GO TO 63
C
C M SCAL:
C READ 1 REAL AS SCALE FACTOR TO APPLY TO INCOMING FOURIER COEFFICIENTS:
  43  CALL RDREAL(SCALF1,IPT,IPT,80,IER)
      IF (IER .NE. 0) IERR=IERR+1
      WRITE (LPT,2019) SCALF1
2019  FORMAT (/' Multiply Fourier coefficients by ',F10.4)
      GO TO 63
C
C M SMAX:
C READ SIN THETA/ LAMBDA MAXIMUM
  45  CALL RDREAL(SMAX,IPT,IPT,80,IER)
      IF (IER .NE. 0) IERR=IERR+1
      WRITE (LPT,2018) SMAX
2018  FORMAT (/' Maximum sin theta/lambda for this map =',F10.4)
      GO TO 63
C
C HERE AFTER ONE <WORD> <NUMBER> PAIR:
  63  CALL RDWORD(MWD,LEN,IPT,IPT,80,0,IER)
      IF (IER .NE. 100) GO TO 62
   3  CONTINUE
      IF (IZSV.NE.0) THEN
        IF (NDIM.EQ.4) THEN
          CALL MESS(LPT,1,'Save sections bounded by z values ')
          WRITE (LPT,2115) (ZSVVAL(I),I=1,IZSV)
2115      FORMAT ('+',33X,F8.4,' to',F8.4,/(34X,F8.4,' to',F8.4))
        ELSE
          CALL MESS(LPT,1,'Save calculated map at z values ')
          CALL PRILIS(ZSVVAL,IZKEEP,IZSV)
        ENDIF
      ENDIF
      IF (IZPL.NE.0) THEN
        IF (NDIM.EQ.4) THEN
          CALL MESS(LPT,1,'Plot sections bounded by z values ')
          WRITE (LPT,2115) (ZPLVAL(I),I=1,IZPL)
        ELSE
          CALL MESS(LPT,1,'Plot at z values ')
          CALL PRILIS(ZPLVAL,IZKEEP,IZPL)
        ENDIF
      ENDIF
      IF (IZPR.NE.0) THEN
        IF (NDIM.EQ.4) THEN
          CALL MESS(LPT,1,'Print sections bounded by z values ')
          WRITE (LPT,2115) (ZPRVAL(I),I=1,IZPR)
        ELSE
          CALL MESS(LPT,1,'Print at z values ')
          CALL PRILIS(ZPRVAL,IZKEEP,IZPR)
        ENDIF
      ENDIF
C
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE KANGA1(X,Y,MODE)
      SUBROUTINE KANGA1(X,Y,MODE)
C
C *** KANGA1 by JCM 24 Nov 83 ***
C
CX
CC 14C
CH Moves plotter pen (or equivalent) to X,Y in current coordinates.
CA On entry X,Y give the required destination of the pen.
CA          MODE indicates whether the pen is to be up or down while moving
CA          MODE=1 leaves pen in state it was last time
CA          MODE=2 lowers pen
CA          MODE=3 raises pen
CP Plotting must have been set up by, e.g., STPLOT.  In particular the
CP transformation PTRAN must be held in /PLTRAN to take current coordinates
CP into the basic hardware coordinates.
C
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
C
      M=MODE
C LEAVE M AS ONLY 2 OR 3, NOT 1, BECAUSE SPECIFIC PLOTTER SOFTWARE LIBRARIES
C MAY WELL NOT CATER FOR M=1:
      IF (M .EQ. 1) M=MPEN
      MPEN=M
C
C CONVERT CURRENT COORDINATES X,Y INTO PLOTTER'S COORDINATES X1,Y1:
      X1=X*PTRAN(1,1,1) + Y*PTRAN(1,2,1) + PTRAN(1,3,1)
      Y1=X*PTRAN(2,1,1) + Y*PTRAN(2,2,1) + PTRAN(2,3,1)
      CALL PIGLET(X1,Y1,M)
      RETURN
      END
C
C
C
C
C LEVEL 6      SUBROUTINE KANGA2(X0,Y0,XF,ICHARS,NCHAR)
      SUBROUTINE KANGA2(X0,Y0,XF,ICHARS,NCHAR)
C
C *** KANGA2 updated by JCM 12 Nov 89 **
C
CX
CC 14C
CH Writes on a plot a string of characters, or simulates this in order
CH to measure the length of the string.
CA On entry ICHARS is a character string
CA          NCHAR is the number of characters in ICHARS;  if NCHAR is given
CA                negatively, KANGA2 goes through the motions of writing but
CA                does not actually plot anything.  This facility is for
CA                measuring strings.
CA          X0, Y0 give where, in the current (character) space, to start
CA                 writing (i.e. the bottom left hand side of the first letter)
CA On exit XF has been updated to be the X position for the "next" character,
CA                 it is adjusted (by the subtraction of X0) so that it is the
CA                 length of the string in character units.
C
CP Plotting must have already been set up by, e.g., STPLOT, and we must
CP already be in "character" space, though this may be of one of various
CP types of character previously set up.
C
CD Instructions to plot the characters are held in the array LINES.
CD A character is described on a 30 by 30 grid.  The X direction is numbered
CD 0 to 30 (including both ends), but the Y direction is -8 to 22.  The line
CD on which the character looks to be written is thus the X axis.  Lower case
CD letters with descenders use the 0 to -8 range.
CD
CD The array LINES holds for each character in turn:
CD     First element = width of character as an integer (in character coords)
CD     Subsequent elements are instructions, packed one per element.  If MPACK
CD     is an instruction, then its sign says whether pen should be up or down
CD     (+ve=up, -ve=down) and the modulus of MPACK is X*32 + Y+8 (where this
CD     is an instruction to move to (X,Y).)
C
CN The character # (hash) is a special character, signalling that the
CN one character which follows it is in a special alphabet (so far, Greek,
CN and so far only theta and lambda allowed, requested by #T and #L)
CN
CN Unidentifiable characters are converted to spaces.
C
      CHARACTER *80 ICHARS
      CHARACTER *1 IALCHR
      LOGICAL GREEK
      DIMENSION IALCHR(84),ITBLCH(96),LINES(1030),LINE1(130),LINE2(130)
      DIMENSION LINE3(130),LINE4(130),LINE5(130),LINE6(130),LINE7(130)
      DIMENSION LINE8(82)
      COMMON /CHARS/LETUP(26),LETLOW(26),ISPCE,IDIGIT(10),ISMBOL(21)
      CHARACTER *1 LETUP,LETLOW,ISPCE,IDIGIT,ISMBOL
      EQUIVALENCE (IALCHR(1),LETUP(1)),(LINE1(1),LINES(1))
      EQUIVALENCE (LINE2(1),LINES(131)),(LINE3(1),LINES(261))
      EQUIVALENCE (LINE4(1),LINES(391)),(LINE5(1),LINES(521))
      EQUIVALENCE (LINE6(1),LINES(651)),(LINE7(1),LINES(781))
      EQUIVALENCE (LINE8(1),LINES(911))
      DATA ITBLCH/1,8,30,49,64,73,80,102,109,112,123,130,135,
     & 144,151,173,186,210,225,246,251,262,267,276,281,287,
     & 294,311,328,343,360,378,386,408,418,426,437,444,447,
     & 464,474,492,509,526,534,552,560,570,575,584,589,598,
     & 605,606,611,626,642,648,666,690,695,725,749,767,773,
     & 782,793,807,815,835,838,843,868,871,882,893,896,899,
     & 904,909,916,925,960,964,968,968,968,968,968,968,968,968,
     & 988,988,988,993/
      DATA LINE1/18,317,-40,317,-552,143,-463,21,157,-136,157,-445,-540,
     & -571,-601,-599,-565,-532,-435,147,-435,-530,-561,-591,-588,-554,
     & -521,-424,-136,21,600,-570,-508,-445,-317,-252,-186,-152,-117,
     & -112,-141,-171,-233,-296,-424,-489,-555,-589,21,157,-136,157,
     & -381,-476,-538,-568,-597,-592,-557,-523,-457,-360,-136,19,157,
     & -136,157,-573,147,-403,136,-552,18,157,-136,157,-573,147,
     & -403,21,600,-570,-508,-445,-317,-252,-186,-152,-117,-112,-141,
     & -171,-233,-296,-424,-489,-555,-589,-592,432,-592,22,157,-136,
     & 605,-584,147,-595,8,157,-136,16,413,-397,-362,-329,-264,
     & -200,-137,-106,-77,-79,21,157,-136,605,-143,308,-584,17/
      DATA LINE2/157,-136,136,-520,24,157,-136,157,-392,669,-392,669,
     & -648,22,157,-136,157,-584,605,-584,22,317,-252,-186,-152,-117,
     & -112,-141,-171,-233,-296,-424,-489,-555,-589,-624,-629,-600,-570,
     & -508,-445,-317,21,157,-136,157,-445,-540,-571,-601,-598,-564,
     & -531,-434,-146,22,317,-252,-186,-152,-117,-112,-141,-171,-233,
     & -296,-424,-489,-555,-589,-624,-629,-600,-570,-508,-445,-317,396,
     & -582,21,157,-136,157,-445,-540,-571,-601,-599,-565,-532,-435,
     & -147,371,-584,20,570,-508,-413,-285,-188,-122,-120,-150,-181,
     & -244,-434,-497,-528,-558,-555,-489,-392,-264,-169,-107,16,285,
     & -264,61,-509,22,157,-142,-171,-233,-328,-392,-489,-555,-590/
      DATA LINE3/-605,18,61,-296,573,-296,24,93,-232,413,-232,413,
     & -552,733,-552,20,125,-552,573,-104,18,61,-307,-296,573,-307,
     & 20,573,-104,125,-573,104,-552,19,502,-488,499,-437,-374,
     & -278,-213,-147,-112,-110,-139,-201,-264,-360,-425,-491,19,157,
     & -136,147,-213,-278,-374,-437,-499,-528,-526,-491,-425,-360,-264,
     & -201,-139,18,499,-437,-374,-278,-213,-147,-112,-110,-139,-201,
     & -264,-360,-425,-491,19,509,-488,499,-437,-374,-278,-213,-147,
     & -112,-110,-139,-201,-264,-360,-425,-491,18,112,-496,-498,-468,
     & -437,-374,-278,-213,-147,-112,-110,-139,-201,-264,-360,-425,-491,
     & 12,349,-285,-220,-185,-168,86,-310,19,502,-486,-451,-418/
      DATA LINE4/-353,-257,-194,499,-437,-374,-278,-213,-147,-112,-110,
     & -139,-201,-264,-360,-425,-491,19,157,-136,146,-245,-310,-406,
     & -469,-498,-488,8,125,-156,-189,-158,-125,150,-136,10,189,-220,
     & -253,-222,-189,214,-197,-162,-97,-33,17,157,-136,470,-140,272,
     & -488,8,157,-136,30,150,-136,146,-245,-310,-406,-469,-498,
     & -488,498,-597,-662,-758,-821,-850,-840,19,150,-136,146,-245,
     & -310,-406,-469,-498,-488,19,278,-213,-147,-112,-110,-139,-201,
     & -264,-360,-425,-491,-526,-528,-499,-437,-374,-278,19,150,-129,
     & 147,-213,-278,-374,-437,-499,-528,-526,-491,-425,-360,-264,-201,
     & -139,19,502,-481,499,-437,-374,-278,-213,-147,-112,-110,-139/
      DATA LINE5/-201,-264,-360,-425,-491,13,150,-136,144,-179,-245,
     & -310,-406,17,467,-437,-342,-246,-149,-115,-145,-208,-367,-430,
     & -460,-459,-425,-328,-232,-137,-107,12,189,-172,-201,-264,-328,86,
     & -310,19,150,-140,-169,-232,-328,-393,-492,502,-488,16,86,-264,
     & 470,-264,22,118,-232,374,-232,374,-488,630,-488,17,118,
     & -456,470,-104,16,86,-264,470,-264,-196,-130,-65,-33,17,
     & 470,-104,118,-470,104,-456,16,20,217,-282,-381,-360,20,
     & 152,-153,-187,-220,-285,-413,-476,-507,-537,-535,-501,-434,-104,
     & -552,20,189,-541,-341,-437,-500,-531,-560,-558,-523,-457,-360,
     & -264,-169,-138,-108,20,445,-111,-591,445,-424,20,509,-189/
      DATA LINE6/-148,-181,-278,-374,-469,-531,-560,-558,-523,-457,
     & -360,-264,-169,-138,-108,20,538,-508,-413,-349,-252,-185,-148,
     & -143,-171,-233,-328,-360,-457,-523,-558,-559,-530,-468,-373,
     & -341,-244,-178,-143,20,573,-232,125,-573,20,285,-188,-154,-152,
     & -182,-245,-372,-467,-529,-559,-556,-522,-489,-392,-264,-169,
     & -138,-108,-111,-145,-211,-308,-437,-502,-536,-538,-508,-413,-285,
     & 20,534,-499,-433,-336,-304,-209,-147,-118,-119,-154,-220,-317,
     & -349,-444,-506,-534,-529,-492,-425,-328,-264,-169,-139,20,317,
     & -220,-153,-116,-113,-140,-201,-296,-360,-457,-524,-561,-564,-537,
     & -476,-381,-317,10,170,-137,-168,-201,-170,10,201,-168,-137,-170,
     & -201,-199,-165/
      DATA LINE7/-132,10,182,-149,-180,-213,-182,170,-137,-168,-201,
     &-170,10,182,-149,-180,-213,-182,201,-168,-137,-170,-201,-199,-165,
     & -132,10,189,-175,170,-137,-168,-201,-170,18,120,-121,-155,-188,
     & -253,-381,-444,-475,-505,-503,-469,-436,-306,-303,298,-265,-296,
     & -329,-298,8,157,-150,16,157,-150,413,-406,20,286,-262,
     & 414,-390,570,-508,-413,-285,-188,-122,-120,-150,-181,-244,-434,
     & -497,-528,-558,-555,-489,-392,-264,-169,-107,22,670,-68,14,
     & 318,-285,-252,-184,-147,-143,-170,-230,-261,-292,14,190,-221,
     & -252,-312,-339,-335,-298,-230,-197,-164,8,158,-131,26,145,
     & -721,26,442,-424,145,-721,26,148,-724,142,-718,16,279/
      DATA LINE8/-267,116,-430,436,-110,21,382,-131,574,-323,148,-596,
     &110,-558,26,756,-757,-726,-694,-661,-627,-558,-491,-425,-360,-232,
     & -169,-138,-108,-110,-144,-177,-405,-438,-472,-474,-444,-381,-316,
     & -282,-280,-309,-370,-523,-585,-648,-712,-745,-746,26,734,-19,
     & -30,26,30,-723,-8,20,317,-220,-153,-116,-113,-140,-201,-296,
     & -360,-457,-524,-561,-564,-537,-476,-381,-317,115,-563,22,
     & 286,-648,404,-136/
C
C SET NOT IN FUNNY CHARACTER SET:
      GREEK=.FALSE.
C
C XF FOLLOWS CHARACTERS ALONG LINE:
      XF=X0
C
      IF (NCHAR .EQ. 0) GO TO 100
      MODNC=IABS(NCHAR)
      DO 1 I=1,MODNC
C
C IDENTIFY CHARACTER  - IF UNKNOWN TO TABLES, SET SPACE (NUMBER 53)
      DO 2 J=1,84
      IF (IALCHR(J) .EQ. ICHARS(I:I)) GO TO 3
   2  CONTINUE
      J=53
C
C IF HASH, SET THAT NEXT CHARACTER WILL BE IN FUNNY SET:
   3  IF (J .NE. 81) GO TO 6
      GREEK=.TRUE.
      GO TO 1
C
C ARE WE ALREADY IN FUNNY CHARACTER SET?
   6  IF (.NOT. GREEK) GO TO 7
      GREEK=.FALSE.
      IF (J .EQ. 46) J=92
      IF (J .EQ. 38) J=95
C J=WHICH CHARACTER - SET K=START OF ITS INSTRUCTIONS IN ARRAY LINES
   7  K=ITBLCH(J)
      L=ITBLCH(J+1)
C IF ONLY MEASURING, NO NEED TO UNPACK INSTRUCTIONS:
      IF (NCHAR .LT. 0) GO TO 4
C
C WITH M SCAN EACH PACKED INSTRUCTION FOR THIS CHARACTER
      M=K+1
   5  IF (M .GE. L) GO TO 4
      MPACK=LINES(M)
      IPEN=2
      IF (MPACK .GT. 0) IPEN=3
      MP=IABS(MPACK)
      M1=MP/32
      M2=MP-M1*32-8
      X=FLOAT(M1)+XF
      Y=FLOAT(M2)+Y0
      CALL KANGA1(X,Y,IPEN)
      M=M+1
      GO TO 5
C
C CHARACTER WRITTEN - ADJUST XF
   4  XWIDE=FLOAT(LINES(K))
      XF=XF+XWIDE
   1  CONTINUE
      XF=XF-X0
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE KANGA3(X,Y,SIZE,MSYM)
      SUBROUTINE KANGA3(X,Y,SIZE,MSYM)
C
C *** KANGA3 updated by PJB 23 May 90 ***
C
CX
CC 14C
CH Plots a special symbol.
CA On entry X,Y give the position in current coordinates of the CENTRE of the
CA              symbol.  The current space will usually be graph or map space.
CA          MSYM is negative if the symbol should be "filled in";
CA               its absolute value indicates which symbol is required:
CA          MSYM=1   square
CA          MSYM=2   triangle, apex up
CA          MSYM=3   triangle, apex down
CA          MSYM=4   hexagon (which if drawn small will make a circle)
CA          MSYM=5   cross like X
CA          MSYM=6   cross like +
CA          MSYM=7   cross like X with top and bottom (egg-timer)
CA          MSYM=8   cross like X with sides (butterfly)
CA          MSYM=9   diamond
CA                   The symbols are "the right way up" for CCSL space.
CA On entry SIZE is set so that the symbol is drawn within a notional square
CA               of side SIZE cms. (Cms are CCSL units).
CP Plotting must be set up by, e.g., STPLOT, and a suitable space set by SPCSET
C
CN This is distinct from KANGA2, which can also draw symbols if they can
CN be represented as A1 characters, but expects them to be part of a
CN sentence, and draws them on a 30 x 30 character space grid with origin
CN at a point part way up the left hand side.  KANGA3 centres the symbol.
C
      LOGICAL SOLID,FOUND
      DIMENSION SMPTS(2,12),MLINE(43),MPOINT(10),S(8),C(8)
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
      DATA SMPTS/-2.,2.,2.,2.,2.,-2.,-2.,-2.,-2.,0.,0.,2.,2.,0.,0.,
     & -2.,-2.,-1.,-2.,1.,2.,1.,2.,-1./
      DATA MPOINT/1,6,10,14,21,25,29,34,39,44/
      DATA MLINE/1,-2,-3,-4,-1,9,-6,-12,-9,8,-10,-11,-8,6,-11,
     & -12,-8,-9,-10,-6,1,-3,2,-4,6,-8,5,-7,1,-2,-4,-3,-1,1,-4,-2,
     & -3,-1,6,-7,-8,-5,-6/

C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C
      S4=SIZE/4.
C
      MSYMB=IABS(MSYM)
      SOLID=(MSYM.LT.0 .AND. (MSYMB.GT.-5 .OR. MSYMB.EQ.-9))
      IF (SOLID) THEN
        YSTEP=1./(100.*S4)
        XMIN=0.
        XMAX=0.
        YMIN=0.
        YMAX=0.
      ENDIF
C
      GO TO (2,3,3,3,2,4,2,2,4) ,MSYMB
C
C TIMES 1 OVER ROOT 2:
   2  S4=S4*0.7071
      GO TO 4
C
C TIMES ROOT 3 OVER 2:
   3  S4=S4*0.8660
C CONVERT POINT AT SYMBOL CENTRE TO BE IN PLOTTER SPACE:
   4  CALL PLCONV(X,Y,0,X0,Y0,1)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C MAKE UNIT VECTORS IN BOTH DIRECTIONS IN PLOTTER SPACE:
      CALL PLCONV(0.,0.,2,XX0,YY0,1)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
      CALL PLCONV(S4,0.,2,XX1,YY1,1)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
      CALL PLCONV(0.,S4,2,XX2,YY2,1)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      XX1=XX1-XX0
      YY1=YY1-YY0
      XX2=XX2-XX0
      YY2=YY2-YY0
      K=MPOINT(MSYMB)
      L=MPOINT(MSYMB+1)-1
      DO 1 I=K,L
      M=MLINE(I)
      MPEN=2
      IF (M .GT. 0) MPEN=3
      M=IABS(M)
      XIN=SMPTS(1,M)*XX1+SMPTS(2,M)*XX2+X0
      YIN=SMPTS(1,M)*YY1+SMPTS(2,M)*YY2+Y0
      CALL PIGLET(XIN,YIN,MPEN)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      IF (SOLID .AND. I.LT.L) THEN
        J=I+1
        IF (SMPTS(1,M).GT.XMAX) XMAX=SMPTS(1,M)
        IF (SMPTS(1,M).LT.XMIN) XMIN=SMPTS(1,M)
        IF (SMPTS(2,M).GT.YMAX) YMAX=SMPTS(2,M)
        IF (SMPTS(2,M).LT.YMIN) YMIN=SMPTS(2,M)
C GET SLOPES AND INTERCEPTS OF LINES JOINING POINTS
        MM=IABS(MLINE(J))
        DEN=(SMPTS(2,MM)-SMPTS(2,M))
        IF (ABS(DEN).LT.10E-6) THEN
          S(I-K+1)=1.1*10E6
        ELSE
          S(I-K+1)=(SMPTS(1,MM)-SMPTS(1,M))/DEN
          C(I-K+1)=(SMPTS(2,MM)*SMPTS(1,M)-SMPTS(2,M)*SMPTS(1,MM))
     &     /DEN
        ENDIF
      ENDIF
    1 CONTINUE
      IF (SOLID) THEN
        NY=IFIX((YMAX-YMIN)/YSTEP)
        MPEN=3
        YPT=YMIN
C FIND THE SMALLEST POSITIVE X ON AN EDGE
        DO 5 IY=1,NY
        YPT=YPT+YSTEP
        FOUND=.FALSE.
        XP=XMAX
        DO 6 I=1,L-K
        IF (S(I).GT.10.E6) GO TO 6
        XPT=YPT*S(I)+C(I)
        IF (XPT.GT. 0. .AND. XPT.LT.XP) XP=XPT
   6    CONTINUE
C USE THE SYMMETRY ABOUT X=0
          XIN=-XP*XX1+YPT*XX2+X0
          YIN=-XP*YY1+YPT*YY2+Y0
          CALL PIGLET(XIN,YIN,MPEN)
          MPEN=2
          XIN=XP*XX1+YPT*XX2+X0
          YIN=XP*YY1+YPT*YY2+Y0
          CALL PIGLET(XIN,YIN,MPEN)
	    IF (IBMBER .NE. 0) RETURN
   5    CONTINUE
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE LOCBIT(I,J,NR,NC,ISIDE,M,N)
      SUBROUTINE LOCBIT(I,J,NR,NC,ISIDE,M,N)
C
C *** LOCBIT by JCM 24 Nov 83 ***
C
CX
CC 15C
CH A specialist routine for contour plotting, which finds the "next" bit in
CH the bit-map, removes it, and indicates where on the picture it was.
CA On entry I=which bit within a word in the bit-map,
CA          J=which word in the bit-map
CA          M,N give the size of the bit-map
CA On exit ISIDE=1,2,3 or 4 to indicate side of picture,
CA         NR,NC are set to indicate the top left of the relevant square.
CD Finds whether the indicated bit is 0 or 1, and in any case removes it.
CP The bit-map must be set up in IBIT in /BITMAP/
CP NBITS must be set up as at most the number of bits in an integer.
C
      COMMON /BITMAP/IBIT(108,4),NWORDS
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS

C
      IF (J .GT. NWORDS) THEN
        WRITE (LPT,3000) J,NWORDS
        WRITE (ITO,3000) J,NWORDS
3000    FORMAT (' ERROR ** IN LOCBIT - INTEGER',I4,' CALLED FOR, ',
     &  ' BUT ONLY',I4,' AVAILABLE')
C>> Was STOP
        CALL BMBOUT
	  RETURN
      ENDIF
C
      ISIDE = I
      IF (I .GT. 4) ISIDE = 1
      MM=1
      IB=1
   5  IF (LOGAND(IBIT(I,J),MM) .NE. 0) GO TO 6
      MM=MM*2
C NB IF THIS GIVES INTEGER OVERFLOW, REDUCE THE VALUE OF NBITS IN COMMON
C CONTUR SO THAT THE SIGN BIT OF AN INTEGER IS NOT USED
      IB=IB+1
      GO TO 5
   6  IBIT(I,J)=IBIT(I,J)-MM
      GO TO(1,2,3,4),ISIDE
    1 NR = I
      IF (I .GT. 4) NR = I-3
      NC = (J-1)*NBITS +IB
      GO TO 100
    2 NC = N-1
      NR = (J-1)*NBITS + IB
      GO TO 100
    3 NR = M-1
      NC = N - (J-1)*NBITS - IB
      GO TO 100
    4 NC = 1
      NR = M - (J-1)*NBITS - IB
 100  RETURN
      END
C
C
C
C
C LEVEL 5      SUBROUTINE MAJUST
      SUBROUTINE MAJUST
C
C *** MAJUST updated by JCM May 88 ***
C
CC 5A
CH A specialist routine used in the input of the Crystal Data File needing
CH previously stored Fourier maps.
CD MAJUST is called from PREFIN, on discovering that the user has given an
CD "M GET" card.  It adjusts file IO10 so that it contains mainly cards from the
CD previously dumped run (to be found on named file), updated to include
CD any new "M" cards presented for this run.  The rule for "M" cards belonging
CD to the "old" and "new" sets is:
CD   take only new cards for PRIN, PLOT, SAVE and GET;
CD   take only old cards for NDIM, FTYP, MESH, DTYP, DELT, SCAL, SMAX and AXES.
CD   For CM/A and CONT:
CD     if the card type occurs in "old" but not "new", accept it
CD     if card type occurs in "new" but not "old", accept it
CD     if card type occurs in both, accept the "new" only.
CD Also takes a new "N" card, unless there is only an old one.
C
CN All the cards capable of being telescoped happen to occur in the "take only
CN old" category, so should not pose a problem by hiding something other than
CN in columns 3-6, but - care will be needed with any newly defined cards.
C
C
      LOGICAL NONEWN
      CHARACTER *4 MWORD,MTBL1(8),MTBL2(4),MNEW(20)
      CHARACTER *80 MCARD(20),NEWNCD
      CHARACTER *10 FILNOM
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPGT/ZGTVAL(20),ZCGT,IGT,IZGT,IDUMPG
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      DATA MTBL1/'NDIM','FTYP','MESH','AXES','DELT','SCAL',
     & 'SMAX','DTYP'/
      DATA MTBL2/'PRIN','PLOT','SAVE','GET'/
C
C
C IF THERE IS A NEW 'N' CARD, SAVE IT:
      NONEWN=.TRUE.
      IF (ICDNO(14) .EQ. 0) GO TO 16
      CALL CARDIN(IABS(INREAD(14)))
      NEWNCD=ICARD
      NONEWN=.FALSE.
C
C SAVE NEW M CARDS FOR CONT, CM/A,  PRIN, PLOT, READ OR SAVE:
   16 NEWMC=ICDNO(13)
      MSTART=INREAD(13)
      J=0
      ID=IABS(MSTART)
      DO 1 I=1,NEWMC
      CALL CARDIN(ID)
      ID=ID+NYZ
      CALL RDWORD(MWORD,ITEMP1,3,ITEMP2,80,0,IER)
C IGNORE IF ONE OF NDIM, AXES, MESH, FTYP, DTYP, DELT, SCAL, SMAX:
      DO 10 K=1,8
      IF (MWORD .EQ. MTBL1(K)) GO TO 1
  10  CONTINUE
      J=J+1
      MCARD(J)=ICARD
      MNEW(J)=MWORD
   1  CONTINUE
C RESET NUMBER OF NEW M CARDS:
      NEWMC=J
C
C COPY CARDS FROM UNIT IDUMPG, UNFORMATTED, ADDING NEW M CARDS:
      MESSAG='File containing saved Fourier '
      NAMFIL='.SAV'
      CALL OPNFIL(IDUMPG,1111)
      INEW=1
C RESTORE OLD VALUES FOR ARRAYS OF COUNTS AND START POINTERS FOR CARDS:
      READ (IDUMPG) INREAD,ICDNO,NCDS
C ENSURE ALL POINTERS INITIALISED POSITIVE:
      DO 2 I=1,26
   2  INREAD(I)=IABS(INREAD(I))
C
C COPY CARDS ACROSS FROM IDUMPG TO IO10 NOTING WHEN M BLOCK REACHED:
      I=0
      LET=-1
   3  I=I+1
      IF (I .GT. NCDS) GO TO 101
      READ(IDUMPG) ICARD
  14  LETNEW=LETTER(ICARD(1:1))
      IF (LETNEW .EQ. LET) GO TO 5
C
C NEW BATCH - ADJUST STARTING POINTER:
      INREAD(LETNEW)=INEW
      LET=LETNEW
C NOTE WHEN ABOUT TO DEAL WITH M CARDS, AND JUMP:
      IF (I .EQ. INREAD(13)) GO TO 4
C
C WRITE OUT CARD OTHER THAN ONE STARTING "M":
   5  INEW=INEW+1
C IF ABOUT TO WRITE AN 'N' CARD, SEE IF THERE IS A NEW ONE:
      IF (ICARD(1:1) .NE. 'N') GO TO 15
      IF (NONEWN) GO TO 15
      ICARD=NEWNCD
  15  WRITE (IO10,2000) ICARD
2000  FORMAT (A80)
C COUNT AS THOUGH A "DO" LOOP OVER I (BUT ALLOWING FOR MOVING I FURTHER DOWN)
      GO TO 3
C
C HERE ON FIRST OLD "M" CARD - SCAN ALL OLD CARDS:
   4  MEND=ICDNO(13)
      DO 6 K=1,MEND
      CALL RDWORD(MWORD,ITEMP1,3,ITEMP2,80,0,IER)
C REFUSE TO COPY PRIN, PLOT, SAVE, GET:
      DO 12 II=1,4
      IF (MWORD .EQ. MTBL2(II)) GO TO 9
  12  CONTINUE
C
C INSIST ON COPYING NDIM, AXES, FTYP, MESH:
      DO 13 II=1,4
      IF (MWORD .EQ. MTBL1(II)) GO TO 8
  13  CONTINUE
C
C THIS LEAVES CONT AND CM/A;  ONLY COPY IF THEY ARE OLD BUT NOT NEW:
      DO 7 J=1,NEWMC
      IF (MWORD .EQ. MNEW(J)) GO TO 9
   7  CONTINUE
C
C OLD CARD STILL WANTED:
   8  INEW=INEW+1
      WRITE (IO10,2000) ICARD
C
C OLD CARD OCCURS AGAIN IN NEW SET - DISCARD OLD:
C COUNT INPUT OLD CARDS
   9  I=I+1
      IF (I .GT. NCDS) GO TO 101
      READ(IDUMPG) ICARD
   6  CONTINUE
C
C END OF SCANNING OLD M CARDS - NOW ADD ALL NEW ONES:
      DO 11 K=1,NEWMC
      INEW=INEW+1
  11  WRITE (IO10,2000) MCARD(K)
      ICDNO(13)=INEW-INREAD(13)
      GO TO 14
C
 101  IF (ICDNO(14) .NE. 0 .OR. NONEWN) GO TO 100
      INREAD(14) = INEW
      INEW=INEW+1
      WRITE (IO10,2000) NEWNCD
      ICDNO(14)=1
 100  WRITE (LPT,2001) FILNOM(IDUMPG)
2001  FORMAT (/' "M GET" card given;  crystal data read from ',
     & A10,' and given relevant new M cards')
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE MAKEBM(I,A,C,M,N)
      SUBROUTINE MAKEBM(I,A,C,M,N)
C
C *** MAKEBM by JCM 24 Nov 82 ***
C
CX
CC 15C
CH A specialist contour plotting routine which makes a bit map to show where
CH the contours are.
C
      LOGICAL ABOVE,ABVE,TEST
      DIMENSION A(M,N)
      IC=1
      IR=0
      NB=N-1
      NR=I-3
      NC=1
      IF (NR .GT. 1) GO TO 6
      GO TO (1,2,3,4), I
   1  NR=1
      GO TO 6
   2  NR=1
      NC=N
      IR=1
      GO TO 5
   3  NR=M
      NC=N
      IC=-1
      GO TO 6
   4  NR=M
      NC=1
      IR=-1
   5  IC=0
      NB=M-1
   6  ABOVE = A(NR,NC) .GE. C
      DO 10 IB=1,NB
      NR = NR + IR
      NC = NC + IC
      ABVE = A(NR,NC) .GE. C
      IF (ABOVE .AND. ABVE .OR. .NOT.(ABOVE .OR. ABVE)) GO TO 10
      ABOVE = ABVE
      CALL BITSET (I,IB,TEST,.TRUE.)
  10  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE MAPCON(CFOUND,IC,IBMAX,IFMAX)
      SUBROUTINE MAPCON(CFOUND,IC,IBMAX,IFMAX)
C
C *** MAPCON by JCM 22 Aug 86 ***
C
CX
CC 5B
CH After a Fourier map has been plotted, sends to the plotter the list of
CH contours which were plotted, with a frame.
C
CA On entry CFOUND is a real array containing the contour values to write,
CA          IC is the number of elements in CFOUND,
CA          IBMAX and IFMAX are handed over to indicate general size of numbers.
C
CP STPLOT shuld set this up, and MPPLOT should call it, having made the list
C
      DIMENSION CFOUND(IC),PTEMP(2,3)
      CHARACTER *8 NUMBUF
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C WRITE CONTOUR LIST IF ANY FOUND:
C IF VALUES SO WIDELY DIFFERENT THAT COULD NOT ALL BE WRITTEN, EXIT:
      IF (IBMAX .GT. 8) GO TO 100
C BLACK PEN AGAIN:
      CALL PIGLET(0.,0.,-1)
C>> JCC Added
	IF (IBMBER .NE.0) RETURN
C NOW CHARACTER TYPE 3 - CONTOUR LIST PANEL - WE KNOW THIS TO BE 10 CHARS WIDE:
C WE NOW KNOW SIZE OF LIST - IC NUMBERS PLUS A TITLE:
      A=FLOAT(IC+1)*2.*CHUNIT
      CHSCAL(2,3)=(HGTMAP+5.*YWHITE+HGTTXT)/A
C DO NOT ALLOW TYPE 3 TO BECOME LARGER THAN TYPE 1:
      IF (CHSCAL(2,3) .GT. CHSCAL(2,1)) CHSCAL(2,3)=CHSCAL(2,1)
      CHSCAL(1,3)=CHSCAL(2,3)/ASPECT
C A DIGIT IS 20 PLOTTER UNITS WIDE:
      WIDCON=220.*CHSCAL(1,3)
      HGTCON=A*CHSCAL(2,3)
C
C PUT IN PLACE THE TRANSFORMATION FOR CHARACTER TYPE 3:
      CALL GMZER(PTEMP,2,2)
      PTEMP(1,1)=CHSCAL(1,3)
      PTEMP(2,2)=CHSCAL(2,3)
      PTEMP(1,3)=WIDPIC-XMARG-WIDCON
      PTEMP(2,3)=(PMTRIX(2,3,3)+YMARG-HGTCON)/2.
      CALL PLTRIN(PTEMP,7,3)
C>> JCC Added
	IF (IBMBER .NE.0) RETURN
C
C SET CHARACTER TYPE 3 SPACE:
      CALL SPCSET(7)
C>> JCC Added
	IF (IBMBER .NE.0) RETURN
      CALL FRAME(0.,0.,220.,A)
      B=A-2.*CHUNIT
      CALL KANGA1(0.,B,3)
      CALL KANGA1(220.,B,2)
C WRITE 'CONTOURS'
      CALL KANGA2(CHUNIT,B+CHUNIT/2.,TEMP,'CONTOURS',8)
C
C SELECT FIELD WIDTH:
      IBMAX=IBMAX+IFMAX
      IF (IFMAX .EQ. 0) IBMAX=IBMAX-1
      IW=IBMAX/2 + 4
      IF (IW .GT. 8) IFMAX=IFMAX-IW+8
C
C LIST VALUES PLOTTED:
      B=B+CHUNIT/2.
      DO 5 I=1,IC
      B=B-2.*CHUNIT
      CALL NUMA1(CFOUND(I),IW,IFMAX,8,NUMBUF)
   5  CALL KANGA2(CHUNIT,B,TEMP,NUMBUF,8)
 100  RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE MAPDRW(CFOUND,IC,IBMAX,IFMAX)
      SUBROUTINE MAPDRW(CFOUND,IC,IBMAX,IFMAX)
C
C *** MAPDRW by JCM 22 Aug 86 ***
C
CX
CC 5B
CH Draws an unframed contour map, in predetermined place.
C
CA On exit CFOUND is a real array which holds all the found contour values.
CA         IC is the number of found contours.
CA         IBMAX and IFMAX are sensible sizes for later contour list printing.
C
CP STPLOT must have set up the map contouring
C
      LOGICAL MORE,FOUND
C%
C      DIMENSION CFOUND(%CONT%)
      DIMENSION CFOUND(50)
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
C>> JCC Implement error handling
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C SET MAP SPACE:
      CALL SPCSET(4)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
C INITIALISE IC=COUNT OF FOUND CONTOURS, IBMAX AND IFMAX WHICH CONTROL THE
C DETAILS OF PRINTING THE CONTOUR LIST, AND ICON WHICH COUNTS CONTOURS SOUGHT:
      IC=0
      IBMAX=0
      IFMAX=0
      ICON=0
      MORE=.TRUE.
C
C NEXT CONTOUR:
   1  CALL NEXCON(C,ICON,MORE)
      IF (.NOT. MORE) GO TO 100
      CALL PLOTCT(C,DENS,FOUND,NX,NY)
C>> JCC Added
	IF (IBMBER .NE.0) RETURN
      IF (.NOT. FOUND) GO TO 3
C
C CONTOUR FOUND - KEEP:
      IC=IC+1
      CFOUND(IC)=C
C ADJUST IFMAX AND IBMAX SO THAT THE RESULTING LIST LOOKS NICE:
      CALL FETTLE(C,IF1,IF2)
      IB=IF1-IF2
      IF (IF2 .EQ. 0) IB=IB+1
      IF (IB .GT. IBMAX) IBMAX=IB
      IF (IF2 .GT. IFMAX) IFMAX=IF2
      WRITE (LPT,2000) C
2000  FORMAT (' Contour at',F12.4,' drawn')
      IF (NCONT .EQ. 0) WRITE(ITO,2000) C
C%
C      IF (IC .GE. %CONT%) GO TO 100
      IF (IC .GE. 50) GO TO 100
C
C JOIN IF REQUESTED CONTOUR NOT FOUND:
   3  MORE=FOUND
C BACK FOR NEXT CONTOUR VALUE:
      GO TO 1
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE MAPFRA
      SUBROUTINE MAPFRA
C
C *** MAPFRA by JCM 22 Aug 86 ***
C
CX
CC 5B
CH Draws a black frame round a potential contoured map, adding the labels
CH X Y and a 1A scale.
C
CP Needs to be set up within a whole picture by STPLOT
C
CO Causes (possibly skew) frame to be drawn via PIGLET, with labels X and
CO Y (and therefore implicit origin), and a one-angstrom scale underneath.
C
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP

C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C MOVE INTO MAP SPACE:

      CALL SPCSET(4)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
C BLACK PEN:
      CALL PIGLET(0.,0.,-1)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
C DRAW FRAME ROUND MAP:
      FX=FLOAT(NX-1)
      FY=FLOAT(NY-1)
      CALL FRAME(0.,0.,FX,FY)
C
C POSITIONS FOR 'X' AND 'Y' LABELS:
      CALL PLCONV(FX/2.,0.,4,X1,X2,5)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
      CALL PLCONV(0.,FY/2.,4,Y1,Y2,5)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
C POSITION FOR 1 ANGSTROM LINE, IN BOTH CHARACTER 1 AND PICTURE COORDS:
      CALL PLCONV(FX,FY/2.,4,C1,C2,5)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
      CALL PLCONV(FX,FY/2.,4,P1,P2,3)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
C
C BRIEF SOJURN IN PICTURE SPACE TO DRAW 1 ANGSTROM LINE:
      CALL SPCSET(3)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
      CALL KANGA1(P1-SCALMP/2.,P2-YWHITE*1.5,3)
      CALL KANGA1(P1-SCALMP/2.,P2-YWHITE*2.,2)
      CALL KANGA1(P1+SCALMP/2.,P2-YWHITE*2.,2)
      CALL KANGA1(P1+SCALMP/2.,P2-YWHITE*1.5,2)
C MOVE INTO CHARACTER TYPE 1 SPACE, WITH X1,X2 AND Y1,Y2 SET AS POSITIONS TO
C WRITE 'X' AND 'Y' AND C1,C2 MIDDLE POINT FOR '1A':
      CALL SPCSET(5)
C>> JCC Added 
	IF (IBMBER .NE. 0) RETURN
C
      CALL KANGA2(C1-CHUNIT,C2-1.8*CHUNIT,TEMP,'1A',2)
      CALL KANGA2(X1-1.5*CHUNIT,X2,TEMP,'X',1)
      CALL KANGA2(Y1-CHUNIT/2.,Y2+CHUNIT/2.,TEMP,'Y',1)
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE MAPKEY(IFND,NFND,JP)
      SUBROUTINE MAPKEY(IFND,NFND,JP)
C
C *** MAPKEY updated by JCM Sep 91 ***
C
CX
CC 5B
CH Plots a key to the atoms found by ATMPLO.
C
      DIMENSION IFND(4),JP(4),PTEMP(2,3)
      COMMON /ATNAM/ATNAME(150),ATNA(150,9)
      CHARACTER *4 ATNA,ATNAME
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP

C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C
C FIT THE KEY TO NAMES OF ATOMS IN THE SPACE NORMALLY USED FOR CONTOUR VALUES
      IF (NFND.LE.0) GO TO 100
C  SET BLACK PEN
      CALL PIGLET(0.,0.,-1)
	IF (IBMBER .NE. 0) RETURN
C
C  LIST TO CONTAIN NFND ENTRIES PLUS HEADING
C A=HEIGHT OF LIST IN CHARACTER UNITS:
      A=FLOAT(NFND+1)*2.*CHUNIT
C CONVERT MIDPOINT OF RHS OF MAP FRAME INTO PICTURE COORDS:
      FX=FLOAT(NX-1)/2.
      FY=FLOAT(NY-1)
      CALL PLCONV(FX,FY,4,X,Y,3)
	IF (IBMBER .NE. 0) RETURN

C NOW CHARACTER TYPE 3 - ATOM LIST PANEL - WE SET THIS TO BE 10 CHARS WIDE:
       CHSCAL(2,3)=(HGTMAP+5.*YWHITE+HGTTXT)/A
C DO NOT ALLOW TYPE 3 TO BECOME LARGER THAN TYPE 1:
      IF (CHSCAL(2,3) .GT. CHSCAL(2,1)) CHSCAL(2,3)=CHSCAL(2,1)
      CHSCAL(1,3)=CHSCAL(2,3)/ASPECT
C A DIGIT IS 20 PLOTTER UNITS WIDE:
      WIDCON=240.*CHSCAL(1,3)
      HGTCON=A*CHSCAL(2,3)
C
C PUT IN PLACE THE TRANSFORMATION FOR CHARACTER TYPE 3:
      CALL GMZER(PTEMP,2,2)
      PTEMP(1,1)=CHSCAL(1,3)
      PTEMP(2,2)=CHSCAL(2,3)
      PTEMP(1,3)=WIDPIC-XMARG-WIDCON
      PTEMP(2,3)=Y-HGTCON*0.5
      CALL PLTRIN(PTEMP,7,3)
C>> JCC Added
	IF (IBMBER .NE.0) RETURN
C
C  CHARACTER 3 SPACE COMPLETED - SET IT
      CALL SPCSET(7)
	IF (IBMBER .NE. 0) RETURN

      CALL FRAME(0.,0.,240.,A)
      B=A-2.*CHUNIT
      CALL KANGA1(0.,B,3)
      CALL KANGA1(240.,B,2)
C  WRITE 'KEY'
      CALL KANGA2(88.,B+CHUNIT/2.,TEMP,'KEY',3)
C  NOW LIST OF ATOMS FOUND
      B=B+CHUNIT/2.
      DO 5 I=1,NFND
      B=B-2.*CHUNIT
C  PLOT APPROPRIATE CHARACTER
      CALL KANGA3(55.,B+.5*CHUNIT,1.5*CHUNIT*CHSCAL(2,3),JP(I))
C  THEN NAME OF CORRESPONDING ATOM
      CALL KANGA2(5.*CHUNIT,B,TEMP,ATNAME(IFND(I)),4)
    5 CONTINUE
C
  100 RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE MAPTIT
      SUBROUTINE MAPTIT
C
C *** MAPTIT by JCM 22 Aug 86 ***
C
CX
CC 5B
CH Writes a title over a plotted map, with a frame.
C
CP The picture must be initialised by a call of STPLOT, setting up in particular
CP space 6 to be "character type 2" space, measured for suitability for title.
CP The title must have been read by INPUTN.
C
      COMMON /NTITL/NTITLE,KOM14
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /TITLE/ITITLE
      CHARACTER *80 ITITLE
C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C
C MOVE TO CHARACTER TYPE 2 SPACE (TITLE CHARACTERS):
      CALL SPCSET(6)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

C WRITE TITLE, ONE CHARACTER IN AND HALF CHARACTER UP:
      CALL KANGA2(CHUNIT,CHUNIT/2.,TEMP,ITITLE,NTITLE)
C AND PUT FRAME ROUND IT:
      CALL FRAME(0.,0.,TEMP+2.*CHUNIT,2.*CHUNIT)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE NEXCON(C,ICON,MORE)
      SUBROUTINE NEXCON(C,ICON,MORE)
C
C *** NEXCON updated by JCM 22 Aug 86 ***
C
CX
CC 5A
CH Sets up the "next" contour value to plot for Fouriers.
CA On entry LOGICAL MORE is .TRUE. if there is no reason why more contours
CA                           should not be requested,
CA                       or .FALSE. if the interactive user
CA                           has requested a contour which was not found.
CA If non-interactive, on entry ICON = which contour in the list in CONT
CA we have just tried to draw.
CA On exit C = the next contour to draw, if possible.
CA         ICON has been incremented.
CA         MORE = .TRUE. if C has been found, or .FALSE. if no more contours
CA                       are to be drawn.
C
CD If NCONT (in COMMON /CONTUR/) is zero, expects to read contour values
CD interactively.  If NCONT is non-zero, expects ICON to be a count within
CD a previously read list of contours, NCONT in total in the array CONT.
C
      LOGICAL MORE,SAYS
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
C IF NCONT IS ZERO, INTERACTIVE WORKING:
      IF (NCONT .EQ. 0) GO TO 1
C
C NEXT C FROM LIST, UNLESS FINISHED:
C MAY BE SIMPLE LIST ITEM (ICONT=0) OR STEP (ICONT=-1):
      MORE = .FALSE.
      ICON=ICON+1
      IF (ICON .GT. NCONT) GO TO 100
      C=CONT(ICON)
      GO TO 101
C
C INTERACTIVE WORKING - IF MORE CAME IN FALSE, LAST ONE REQUESTED NOT FOUND:
   1  IF (MORE) GO TO 2
      WRITE (ITO,1001)
 1001 FORMAT (' Contour at',F12.2,' not found ')
      CALL ASK('type C to continue or X to exit')
      IF (.NOT. SAYS('X')) GO TO 2
      MORE = .FALSE.
      GO TO 100
C
   2  CALL ASK('Type contour value required')
      CALL RDREAL(C,1,IPT,80,IER)
 101  MORE = .TRUE.
 100  RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PICMOV
      SUBROUTINE PICMOV
C
C *** PICMOV by JCM 22 Aug 86 ***
C
CX
CC 14B
CH If plotting to Tektronix, do nothing;  if to plotter, move to next picture.
CP FROMCM in /PLODAT/ is 0 if plotting is actually to a VDU, for which
CP the scale "from cm" is irrelevant.
CP NYPIC in /PLOMAP/ is the number of pictures expected, and IYPIC is the
CP number already plotted.
CD If a large plotter is in use for several pictures plotted in sequence,
CD the matrix PMTRIX(,,2) holds the part of the transformation which
CD moves from one picture to another.  This routine deals with the moving
CD from one picture to another, advancing IYPIC and adjusting PMTRIX(,,2)
C
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
C
C USE FROMCM ALSO TO TELL WHETHER ACTUAL PLOTTER  (CALCOMP, BENSON, ZETA,
C ETC -FROMCM > 0) OR VDU (FROMCM =< 0)
      IF (FROMCM .LE. 0.) GO TO 100
C
C DEAL WITH PUTTING PICTUES NEATLY ON A LARGE PLOTTER:
      IF (IYPIC .EQ. 0) GO TO 11
      IF (IYPIC .LT. NYPIC) GO TO 12
      PMTRIX(1,3,2)=PMTRIX(1,3,2)+WIDPIC+BORDER
      PMTRIX(2,3,2)=-(HGTPIC+BORDER)
      IYPIC=0
  12  PMTRIX(2,3,2)=PMTRIX(2,3,2)+HGTPIC+BORDER
  11  IYPIC=IYPIC+1
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE PIGLET(X,Y,N)
       SUBROUTINE PIGLET(X,Y,N)
C
C *** PIGLET updated by JCM 27 Jun 86 ***
C
CX
CC 14C
CH A complete set of device-specific plotting commands.
CA On entry X and Y are plotter coordinates
CA          N indicates the function required:
CA
CA N=0  Called at very start of a plotting job, to set up physical things like
CA       plotter width in cms, plotter units (as FROMCM which converts from cms
CA       to plotter units).  Also sets up some transformation matrices and
CA       initialises quantities for transformations.
CD
CA N=2   Move the pen to the point X,Y (in the plotter's own coordinates)
CA or          N=2 moves with pen down (i.e. draws)
CA N=3         N=3 moves with pen up.
CA             There was originally an N=1 meaning move with the pen in the
CA             state it was last time.  Not every library implements this,
CA             so although CCSL uses it, it is now done by the routines which
CA             call PIGLET.
CD
CA N=-1 or -2 or -3 Changes the colour of the ink in the pen
CA             N=-1 Asks for black
CA             N=-2 Asks for red
CA             N=-3 Asks for green
CA             (These may be altered, or others added, as the user wishes)
CD
CA The remaining values of N carry out whatever special actions the local
CA plotter software needs to make when starting or finishing various stages.
CD
CD The complete plotter output produced by the whole job is called here a
CD "plot".  The plot may be made up of various "pictures" such as layers of
CD a Fourier map; or it may be just one graph. Within a "picture" there are
CD various boxes of explanatory text and the main graphic object such as a
CD "map" for fouriers or a "graph" from PLOTO
CD
CD Most plotter software libraries require that a certain routine be called to
CD start a plot, and some also require another routine to be called to finish
CD a plot (say, to move the pen clear of the plot ready for the next job).
CD There is no obvious need for any special action on an actual plotter at the
CD start and finish of one "picture", but if a Tektronix or other vdu is used,
CD there will be only one "picture" on the screen at once (remember several
CD "pictures"=1 "plot"), so special actions will be needed to start (say, clear
CD screen) and finish (say, ask user if he wants a hard copy) a picture.
CD
CD To accomplish these actions large values of n are assigned as follows:
CD N=999 Start "plot" (which will be X cms wide and Y cms high)
CD N=-999 Finish "plot" (which was X cms wide and Y cms high)
CD N=888 Start "picture" (which will be X cms wide and Y cms high)
CD N=-888 Finish "picture" (which was X cms wide and Y cms high)
C
CD Sets PMTRIX number 1: the CCSL to hardware transformation
C
CO When requested, sends output to graphical device.
CN Contains all plotting commands which are specific to whatever machine (and
CN which output device) is being used.  These are believed to be a bare
CN minimum.  Whenever a new plotter (specifically a new plotter software
CN library) is implemented on the system a specific version of this routine
CN must be made.
CN
CN There are other versions of PIGLET in CCSL for specific output devices.
C
      DIMENSION PTEMP(2,3)
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
C>> JCC Implement error handling
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
      IF (N .NE. 0) GO TO 3
C INITIAL ENTRY - SET UP PLOTTER WITHOUT YET KNOWING WHAT WE WISH TO PLOT:
C
C PLOTTER CONVENTIONS - FIRST SET UP DEFAULTS COMMON TO ALL:
      CALL PINITL
	IF (IBMBER .NE. 0) RETURN
C CONVERSION MATRIX WHICH TAKES (X,Y) IN CCSL UNITS INTO (X',Y') IN YOUR
C ACTUAL PLOTTER UNITS.  SEE THE ANNOTATION FOR SUBROUTINE PLCONV FOR
C DEFINITIONS.  THE DEFAULT HERE IS THE UNIT TRANSFORMATION.
      CALL GMZER(PTEMP,2,3)
      CALL GMUNI(PTEMP,2)
C
C FROMCM POSITIVE USED TO INDICATE PLOTTER NOT VDU:
CRAL
C RUTHERFORD VAX PLOTTER WORKS IN SAME ORIENTATION AS CCSL EXPECTS,
CRAL
C  BUT IN INCHES.
CRAL
      FROMCM=1./2.54
CRAL
      PAPERW=28.
C
C3084C THE 3084 PLOTTER HAS ITS ORIGIN THE OTHER SIDE, WITH X AND Y REVERSED,
C3084C  AND IT WORKS IN MILLIMETRES.
C3084      FROMCM=10.
C3084      PAPERW=30.
C3084      PTEMP(1,1)=0.
C3084      PTEMP(2,1)=1.
C3084      PTEMP(1,2)=-1.
C3084      PTEMP(2,2)=0.
C3084      PTEMP(1,3)=PAPERW
C3084      PTEMP(2,3)=0.
C
C SET UP CCSL TO PLOTTER TRANSFORMATION:
      CALL GMSCA(PTEMP,PTEMP,ABS(FROMCM),2,3)
      CALL PLTRIN(PTEMP,2,1)
	IF (IBMBER .NE. 0) RETURN
C
      GO TO 100
C
   3  IF (IABS(N) .GT. 500) GO TO 1
      IF (N .LT. 0) GO TO 2
C
C BASIC PEN MOVING: 2=DOWN, 3=UP
CVMS
      IF ((N .EQ.2) .OR. (N .EQ. 3)) GO TO 4
C3084      IF (N .EQ. 2) GO TO 4
C3084      IF (N .EQ. 3) GO TO 5
      CALL ERRMES(-1,0,'call of PIGLET with small N not 2 or 3')
C
C3084C INSTALLATION DEPENDENT LIBRARY CALLS FOR CURVE PLOTTING ON 3084
C3084C 'DRAW TO'
C3084   4  CALL GLDT2S(X,Y)
C3084      GO TO 100
C3084C 'MOVE TO'
C3084   5  CALL GLMT2S(X,Y)
C
CVMS
C   4  CALL PLOT(X,Y,N)
4     CONTINUE
      GO TO 100
C
C PEN COLOUR CHANGE:
C3084   2  CALL GLDV2S(-N)
CVMS
C   2  CALL NEWPEN(-N)
2     CONTINUE
      GO TO 100
C
C START/FINISH ROUTINES:
C INPUT X AND Y WERE IN CCSL UNITS - PUT INTO PLOTTER:
   1  CALL PLCONV(X,Y,2,XX,YY,1)
C>> JCC added
      IF (IBMBER .NE. 0) RETURN
      IF (IABS(N) .NE. 999) GO TO 100
C ONLY "START/STOP PLOT" IS RELEVANT HERE AT PRESENT:
C3084      IF (N .EQ. -999) GO TO 9
C3084C INITIALISE PACKAGE:
C3084      CALL GLPC2S
C3084C TIE UP PLOTTING WITH JCL ITEM LABELLED 'PLOT';  ASK FOR PAPER WHICH IS
C3084C 300 MMS WIDE (MAXIMUM 30 CMS AS SET IN PAPERW IN ROUTINE FRIG AND ALREADY
C3084C CHECKED) AND MAX 6 METRES LONG (NO CHECK AT PRESENT)
C3084C
C3084C THE LAST PARAMETER IS THE RATIO OF THE LENGTH TO THE WIDTH
C3084      CALL GLPS2S('PLOT    ',300.0,20.001)
C3084C THESE NEXT 4 PROVIDED BY CS - THIS SAYS IT DEFINES DEVICE WINDOW IN
C3084C NORMALISED DEVICE SPACE WITH PREFERRED WIDTH:
C3084      CALL GLWV4S(0.0, 0.05, 0.0, 1.0, 300.0)
C3084C THIS SAYS IT DEFINES USER VIEWPORT IN NORMALISED DEVICE SPACE:
C3084      CALL GLWV3S(0.0, 0.05, 0.0, 0.05)
C3084C AND THIS SAYS IT DEFINES USER WINDOW IN WORLD COORDINATE SPACE:
C3084      CALL GLWV2S(0.0, 300.0, 0.0, 300.0)
C3084C SELECT TRANSPARENT MODE FOR SOME REASON:
C3084      CALL GLMD3S(3)
C3084      GO TO 100
C3084C
C3084C FINISH PLOT:
C3084C CLOSE OUTPUT STREAM:
C3084   9  CALL GLPS7S
C3084C CLOSE PACKAGE:
C3084      CALL GLPC4S
C
CILL      IF (N .EQ. 999) CALL PLOTS(0,XX,YY)
CILL      IF (N .EQ. -999) CALL PLOT(0.,0.,999)
C
CRAL
C      IF (N .EQ. 999) CALL PLOTS(53,0,8)
CRAL
C      IF (N .EQ. -999) CALL PLOT(XX,-0.5,999)
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE PINITL
      SUBROUTINE PINITL
C
C *** PINITL by JCM 27 Jun 86 ***
C
CX
CC 14A
CH Initialises the system in order to make graphical output.
C
CD Sets up various quantities, some of them probably machine specific, to
CD enable SUBROUTINE PIGLET to be called.  PIGLET may well alter some
CD of them.  Suitable for either the plotter version or the Tektronix
C
CD  Sets    PAPERW = width of paper in cms (or no. of pixels)
CD          PAPERH = "height" of paper in cms - this is the maximum
CD                   amount allowed in direction perpendicular to the
CD                   axis of the plotter (or no. of pixels)
CD          FROMCM = the conversion factor from centimetres to hardware
CD                   units - set to 1 for now, and adjusted in PIGLET
CD                   where necessary (<0 for VDU output)
CD          CHUNIT = number of character units of a character grid which
CD                   make the height of a character (and also the maximum
CD                   width of a character, which may not use it all)
CD          ASPECT = ratio of the height of a plotted character to its width
CD          BORDER = width of border between pictures, in cms
CD          DASH = length of the dash of a dashed line, in cms
CD  Initialises the general structure of transformations in /PLODAT;
CD  does not initialise the basic 'CCSL to hardware' transformation,
CD which must be done in individual PIGLETs.
      DIMENSION PTEMP(2,3)
      COMMON /ADASH/DASH,X1,Y1,REM,IPEN,IDSH,DTRAN(2,2)
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
C>> JCC Implement error handling
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C RATIO OF HEIGHT OF CHARACTER TO ITS WIDTH:
      ASPECT=1.0
C   PAPERW AND PAPERH = MAXIMUM VALUES FOR WIDTH (CCSL Y DIRECTION) AND
C   HEIGHT (CCSL X DIRECTION, WHICH WOULD BE "INFINITE" IF A DRUM PLOTTER
C   IS BEING USED) EXPRESSED IN CCSL UNITS WHICH ARE ** CENTIMETRES **
C INITIALISATION IS FOR GRENOBLE BENSON:
      PAPERW = 72.
      PAPERH = 500.
C FROMCM = CONVERSION FACTOR FROM CMS TO HARDWARE UNITS:
      FROMCM=1.
C BORDER = SPACE BETWEEN ONE PICTURE AND THE NEXT IN A PLOT (SEE PIGLET
C FOR EXPLANATION)
      BORDER = 0.5
C CHUNIT IS THE NUMBER OF UNITS OF CHARACTER GRID WHICH MAKE THE HEIGHT OF A
C CHARACTER, AND THE MAXIMUM WIDTH (CHARACTERS MAY TAKE FEWER UNITS ACROSS).
      CHUNIT=30.
C LENGTH OF A DASH IN NEGATIVE CONTOURS IN CMS:
      DASH=0.2
C
C SET UP PICTURE TO CCSL TRANSFORMATION (DEFAULT 1 PICTURE):
      CALL GMZER(PTEMP,2,3)
      CALL GMUNI(PTEMP,2)
      CALL PLTRIN(PTEMP,3,2)
	IF (IBMBER .NE. 0) RETURN
C INITIALISE TRANSFORMATIONS:
      NSPCE=0
      NCON1=0
      NCON2=0
      MAXSP=2
      IDSH=0
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE PLCONV(X1,Y1,NN1,X2,Y2,NN2)
      SUBROUTINE PLCONV(X1,Y1,NN1,X2,Y2,NN2)
C
C *** PLCONV updated by PJB 4 Apr 85 ***
C
CX
CC 14B
CH Performs the transformation of coordinates between different plotter spaces.
CA On entry X1, Y1 are coordinates in coordinate system number NN1
CA On exit  X2, Y2 are the same coordinates transformed into the
CA                 coordinate system number NN2
C
CP PIGLET with N=0, PLTRIN to set up the required transformation matrices.
C
CD Coordinate systems:
CD 0=Current - The current space is held in NSPCE in COMMON /PLTRAN/
CD 1=Plotter (actual coords on a particular plotter)
CD 2=CCSL (the coords in which the programs are written)
CD Coordinate sytems 3 to 7 are for user applications, for example in plotting
CD Fourier maps they are used as follows:
CD 3=Picture (one "picture" which contains one section of the map)
CD 4=Map (The crystallographically related axes of the Fourier calculation)
CD 5=Character type 1 (a mesh of 30 by 30 on which characters are defined)
CD 6=Character type 2 (a mesh of 30 by 30 on which characters are defined)
CD 7=Character type 3 (a mesh of 30 by 30 on which characters are defined)
CD
CD Conversion involves both a translation (or origin shift) and a rotation.
CD These are combined by holding the conversion matrix as 3 by 2 with
CD the translation vector as the 3rd column.
C
      DIMENSION A(2,3),B(2,3),C(2,3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
C
      N1=NN1
      N2=NN2
      IF (N1 .EQ. 0) N1=NSPCE
      IF (N2 .EQ. 0) N2=NSPCE
C  DO WE NEED A NEW MATRIX?
      IF (N1.EQ.NCON1 .AND. N2.EQ.NCON2) GO TO 101
C MAKE IT:
      N=N1-1
      M=N2-1
      CALL GMZER(A,2,3)
      A(1,1)=1.
      A(2,2)=1.
      CALL GMEQ(A,B,2,3)
    1 IF (N-M) 4,3,2
C
C  SOURCE SPACE HIGHER
    2 IF (N .LE. 0) GO TO 6
      CALL PMTMUL(PMTRIX(1,1,N),A,C)
      CALL GMEQ(C,A,2,3)
      N=NTRAN(N)-1
      GO TO 1
C
C  DESTINATION HIGHER
    4 IF (M .LE. 0) GO TO 6
C CHECK WHICH WAY ROUND THIS SHOULD BE:
      CALL PMTMUL(PMTRIX(1,1,M),B,C)
      CALL GMEQ(C,B,2,3)
      M=NTRAN(M)-1
      GO TO 1
C
C  SOURCE AND DESTINATION NOW THE SAME, FORM MATRIX
    3 CALL PMTINV(B,C)
      CALL PMTMUL(C,A,PTRAN(1,1,2))
C  AND SET NEW VALUES IN NCON
      NCON1=N1
      NCON2=N2
C  GO AND DO TRANSFORMATION
      GO TO 101
C
    6 WRITE (LPT,3000) N1,N2
      WRITE (ITO,3000) N1,N2
3000  FORMAT (' ERROR ** PLCONV REQUIRED TO CONVERT FROM COORDS',
     & I3,' TO COORDS',I3)
C>> JCC Was STOP
      CALL BMBOUT
	RETURN
C
 101  X2=PTRAN(1,1,2)*X1+PTRAN(1,2,2)*Y1+PTRAN(1,3,2)
      Y2=PTRAN(2,1,2)*X1+PTRAN(2,2,2)*Y1+PTRAN(2,3,2)
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PLOTCT(C,A,FOUND,M,N)
      SUBROUTINE PLOTCT(C,A,FOUND,M,N)
C
C *** PLOTCT by JCM 24 Nov 83 ***
C
CX
CC 14B
CH Plots a single contour throughout a given array.
CA On entry C is the contour value required
CA          A is a M by N array of values in which the contour C is to be
CA            interpolated
CA On exit  FOUND is true if a contour was found and plotted, false otherwise.
CP PIGLET with N=0 and N=999, to set up the plotter.
CP The desired mesh on the plotter should be set up with PLTRIN and SPCSET
CO Output is to a plotter whose characteristics are defined in the version of
CO PIGLET linked.
C
      LOGICAL FOUND,MID,TEST
      DIMENSION A(M,N),T(6)
      COMMON /BITMAP/IBIT(108,4),NWORDS
C>> JCC Implemented error handling
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
      FOUND = .FALSE.
      IPEN=3
      ICC=1
      IF (C) 6,7,8
   6  ICC=2
      IPEN=2
      GO TO 8
   7  IPEN=1
      ICC=14
C
C SELECT COLOUR OF PEN:
   8  CALL PIGLET(0.,0.,-IPEN)
      M2=M+2
      DO 1 I=1,M2
      DO 5 J = 1,4
    5 IBIT(I,J) = 0
      CALL MAKEBM(I,A,C,M,N)
   1  CONTINUE
      DO 2 I=1,M2
      DO 2 J=1,4
  22  IF (IBIT(I,J) .EQ. 0) GO TO 2
      FOUND = .TRUE.
      CALL LOCBIT(I,J,NR,NC,ISIDE,M,N)
	IF (IBMBER .NE. 0) RETURN ! Bomb out on error
C        SETS NR,NC TO TOP LEFT OF SQUARE - ISIDE = 1,2,3 OR 4
      CALL GETSQ(A,T,C,NR,NC,M,N)
      XPNT = 0.
      POINT = T(ISIDE)/(T(ISIDE) - T(ISIDE+1))
      LINE=0
      CALL DOSIDE (POINT,XPNT,ISIDE,NR,NC,X,Y)
      CALL DPLOT(X,Y,LINE)
      LINE = ICC
   4  MID = T(6) .GE. 0.
      IC = 0
      IF (MID .AND. T(ISIDE) .GE. 0. .OR. .NOT. MID .AND. T(ISIDE)
     & .LT. 0.) IC = 1
      ID = ISIDE + IC
   3  POINT = 0.5 * T(ID)/(T(ID) - T(6))
      XPNT = POINT
      IF (IC .NE. 0)  XPNT = 1.-POINT
      CALL DOSIDE (XPNT,POINT,ISIDE,NR,NC,X,Y)
      CALL DPLOT(X,Y,LINE)
      LINE = ICC
      ISIDE = ISIDE - 1 + IC + IC
      IF (ISIDE .EQ. 5)  ISIDE = 1
      IF (ISIDE .EQ. 0)  ISIDE = 4
      ID = ISIDE + IC
      IF (MID .AND. T(ID) .LT. 0. .OR. .NOT. MID .AND. T(ID)
     & .GE. 0.) GO TO 3
      XPNT = 0.
      POINT = T(ISIDE)/(T(ISIDE) - T(ISIDE+1))
      CALL DOSIDE (POINT,XPNT,ISIDE,NR,NC,X,Y)
      CALL DPLOT(X,Y,LINE)
      GO TO (11,12,13,14), ISIDE
  11  ISIDE = 3
      IF (NR .GT. 1) GO TO 21
      CALL BITSET(1,NC,TEST,.FALSE.)
  17  IF (TEST)  GO TO 22
      CALL ERRMES(-1,0,'in PLOTCT')
C
  21  NR = NR-1
      CALL BITSET(NR+4,NC,TEST,.FALSE.)
      IF (.NOT. TEST) GO TO 22
      GO TO 16
  12  NC = NC+1
      ISIDE = 4
      IF (NC .LT. N) GO TO 16
      CALL BITSET (2,NR,TEST,.FALSE.)
      GO TO 17
  13  NR = NR+1
      ISIDE = 1
      IF (NR .LT. M) GO TO 15
      CALL BITSET (3,N-NC,TEST,.FALSE.)
      GO TO 17
  14  NC = NC-1
      ISIDE = 2
      IF (NC .GT. 0) GO TO 16
      CALL BITSET(4,M-NR,TEST,.FALSE.)
      GO TO 17
  15  CALL BITSET(NR+3,NC,TEST,.FALSE.)
      IF (.NOT. TEST) GO TO 22
  16  CALL GETSQ(A,T,C,NR,NC,M,N)
      GO TO 4
   2  CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 8      SUBROUTINE PLOTIT(X,NP,SIZE)
      SUBROUTINE PLOTIT(X,NP,SIZE)
C
C *** PLOTIT by PJB Sep 87 ***
C
CX
CC 14B
CH Plots the graph of given vector y against x, with esds.
CA On entry X(3,NP) holds values to be plotted
CA    X(1,1:NP)= x values
CA    X(2,1:NP) = y values
CA    X(3,1:NP) = standard deviations of y values
CA On entry NP = number of points in graph
CA          SIZE a vector of dimension 2 holds the lengths of the x and
CA                 y axes in cms.
CD This subroutine sets up the plotter as well as drawing the graph.*
CO Output to a plotter whose characteristics are defined in the version of
CO PIGLET linked.
C
      DIMENSION X(3,NP),SIZE(2)
      DIMENSION XMAX(2),XMIN(2)
C>> JCC Trap for error in GETSCL to prevent STOP
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C  FIND MAXIMUM AND MINIMUM VALUES
      DO 3 J=1,2
      XMAX(J)=X(J,1)
      XMIN(J)=X(J,1)
      DO 2 I=2,NP
      IF (XMAX(J).LT.X(J,I)) XMAX(J)=X(J,I)
      IF (XMIN(J).GT.X(J,I)) XMIN(J)=X(J,I)
    2 CONTINUE
C  SET SCALE
    3 CALL GETSCL(XMIN(J),XMAX(J),J)
	IF (IBMBER .GT. 0) RETURN
C
C  DRAW AXES ETC
      CALL PLOTO(SIZE(1),SIZE(2),.8,0)
	IF (IBMBER .GT. 0) RETURN

C
      DO 1 I=1,NP
      CALL PLOTO(X(1,I),X(2,I),X(3,I),2)
	IF (IBMBER .GT. 0) RETURN
    1 CONTINUE
C
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PLOTO(XX,YY,ER,MODE)
      SUBROUTINE PLOTO(XX,YY,ER,MODE)
C
C *** PLOTO updated by PJB/JCM 10 Jun 88 ***
C
CX
CC 14B
CH A multi-purpose graph-drawing routine.
CA On entry XX and YY are x and y coordinates whose meaning is defined by MODE:
CA MODE=0 Set up to draw a graph in which the lengths of the axes are x, y.
CA     >0 Line drawing: move the "pen" from its current position to x, y.
CA        If MODE = -1 the "pen" is down
CA                = -2 a dashed line is drawn. The dash interval is ER.
CA                = -3 the "pen" is up
CA        If -MODE > 10 and < 21 the symbol ABS(MODE+10) is drawn at
CA                   intervals of ER.
CA MODE>0 Plot the point x,y with an error bar of length ER. The value of
CA           MODE defines the symbol to be plotted.
CA           MODE = 1 square
CA           MODE = 2 triangle, apex up
CA           MODE = 3 triangle, apex down
CA           MODE = 4 hexagon, which if small will look like a circle
CA           MODE = 5 cross like x
CA           MODE = 6 cross like +
CA           MODE = 7 cross like x with top and bottom (egg-timer)
CA           MODE = 8 cross like x with sides (butterfly)
CA           MODE = 9 diamond
CP The vector X(I,J) held in COMMON /PLTS/ defines how the graph will be drawn
CP     I=1 for x-axis, I=2 for y-axis.
CP         J=1  length of axis in user units
CP         J=2  length of axis in cms.
CP         J=3  division of axis in user units
CP         J=4  minimum value in user units
CP         J=5  position of axis to be plotted
CP GETSCL should be called for both x and y axes, to define the ranges of
CP values to plot.
C
      DIMENSION YC(2),Y(2),XO(2),IPNT(2),ICH(2),AM(2,3)
      COMMON /ADASH/DASH,X1,Y1,REM,IPEN,IDSH,DTRAN(2,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
      COMMON /PLTS/X(2,5),S(2),CH,XS,ISIG(2),YS,NDIVS(2,2)
      COMMON /PLOTCH/ITEXT,NTEX(2)
      CHARACTER *80 ITEXT,NTEX*15
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      EQUIVALENCE (Y(1),A),(Y(2),B)
C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
      IF (MODE) 1,2,3
C
C  MODE=0 SET UP AND DRAW AXES
C
C  GET HOLD OF PLOTTER
    2 CALL PIGLET(0.,0.,0)
C  SET DASH LENGTH
      DASH=0.5
C  IF ABLE, ACCEPT TITLE (IF NOT, IT MUST BE SET UP BEFORE ENTRY):
      CALL ASK('Give title (up to 80 characters)')
      ITEXT=ICARD
C
C  SET LENGTHS OF AXES IN CMS
      X(1,2)=XX
      X(2,2)=YY
C  DETERMINE NUMBER OF FIGURES BEFORE AND AFTER DECIMAL POINT FOR
C  LABELLING AXES
      DO 21 I=1,2
      IPNT(I)=1-ISIG(I)
      ICH(I)=2+IABS(ISIG(I))
      IF (ISIG(I).EQ.0) ICH(I)=ICH(I)+1
      IF (ISIG(I).GT.1) ICH(I)=ICH(I)-1
C  SET SCALE OF GRAPH
      S(I)=X(I,2)/X(I,1)
   21 CONTINUE
C  LIMITING CHARACTER SIZE
      CLIM=1.5
      IF (ER.NE.0) CLIM=ER
C  NORMALLY CHARACTERS LABELLING AXES SHOULD TAKE HALF THE SPACE BETWEEN
C  LABELLED DIVISIONS IN THE X DIRECTION
      CH2=S(1)*X(1,3)/(2.*FLOAT(ICH(1)))
      IF (CH2.GT.CLIM) CH2=CLIM
C  ALEN IS THE LENGTH IN USER UNITS OF THE FIGURES ON THE Y-AXIS
      ALEN=(ICH(2)+3)*CH2/S(1)
C  SET XO TO BE MINIMUM VALUE IN USER UNITS INCLUDUNG SPACE FOR NUMBERS
      DO 31 I=1,2
      XO(I)=AMIN1(X(I,5)-ALEN,X(I,4))
   31 ALEN=4.*CH2/S(2)
C
C  SPACE FOR TITLE AND CHARACTER SIZE FOR TITLE
      ITITLE=LENGT(ITEXT)
      CALL KANGA2(0.,0.,TLEN,ITEXT,-ITITLE)
      TLEN=TLEN+2.*CHUNIT
      CH1=X(1,2)*CHUNIT/TLEN
      CLIM=1.5*CH2
      IF (CH1.GT.CLIM)CH1=CLIM
C  CENTRE TITLE
      ALEN=CH1*TLEN/CHUNIT
C      ALEN=((X(1,1)-XO(1))*S(1)-ALEN)/2.
       ALEN=BORDER+(X(1,4)-XO(1))*S(1)+(X(1,2)-ALEN)/2
C  ALEN IS DISTANCE IN CMS OF START OF TITLE FROM LH EDGE
C  SET SIZES OF OTHER MARKS
C  XS FOR DASHES ON AXES
      XS=0.4*CH2
C  YS FOR TOP AND BOTTOM OF ERROR BARS
      YS=0.6*CH2/S(1)
      CH=0.5*CH2
C
C  CHARACTER 1 SPACE - FOR TITLE
      AM(1,1)=CH1/CHUNIT
      AM(2,2)=AM(1,1)*ASPECT
      AM(1,2)=0.
      AM(2,1)=0.
      AM(1,3)=BORDER+ALEN
      AM(2,3)=BORDER
      CALL PLTRIN(AM,5,3)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
C  CHARACTER 2 ROTATED BY 90 DEGREES - FOR LABEL ON Y AXIS
      A1=CH2/CHUNIT
      A2=A1*ASPECT
      AM(1,2)=-A1
      AM(2,1)=A2
      AM(1,1)=0.
      AM(2,2)=0.
      AM(1,3)=BORDER-XO(1)*S(1)
C  CHANGE HERE WHEN NO TITLE
C      AM(2,3)=BORDER-XO(2)*S(2)+5*CH1
      AM(2,3)=BORDER-XO(2)*S(2)+2.*CH1
      CALL PLTRIN(AM,6,3)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C  CHARACTER 2 SPACE - FOR NUMBERS ON AXES AND X AXIS LABEL
      AM(1,1)=A1
      AM(2,2)=A2
      AM(1,2)=0.
      AM(2,1)=0.
      CALL PLTRIN(AM,7,3)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
C  GRAPH SPACE
      AM(1,1)=S(1)
      AM(2,2)=S(2)
      CALL PLTRIN(AM,4,3)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C  SIZE OF PLOT
      PW=X(1,2)+(X(1,4)-XO(1))*S(1)+(ICH(1)+3)*CH2+3.*BORDER
      PH=X(2,2)+(X(2,4)-XO(2))*S(2)+2.*BORDER+5.*CH1
C
      CALL PIGLET(PW,PH,999)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
      CALL PIGLET(0.,0.,-1)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
C  WRITE TITLE
      CALL SPCSET(5)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      IF (ITITLE.NE.0) THEN
      CALL KANGA2(CHUNIT,0.5*CHUNIT,TLEN,ITEXT,ITITLE)
      CALL FRAME(0.,0.,TLEN+2.*CHUNIT,2*CHUNIT)
      ENDIF
C
C  LABEL AXES
      CALL SPCSET(6)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      X1=X(1,5)
      X2=X(2,1)+X(2,4)-X(2,3)
      CALL PLCONV(X1,X2,4,YC(1),YC(2),6)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
      YC(2)=YC(2)+(ICH(2)+1)*CHUNIT
      N=LENG(NTEX(2),15)
      IF (N .GT. 0) THEN
        CALL KANGA2(YC(1),YC(2),ALEN,NTEX(2),-N)
        YC(1)=YC(1)-ALEN
        CALL KANGA2(YC(1),YC(2),ALEN,NTEX(2),N)
      ENDIF
      CALL SPCSET(7)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      X1=X(1,1)+X(1,4)-X(1,3)
      X2=X(2,5)
      CALL PLCONV(X1,X2,4,YC(1),YC(2),7)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
      N=LENG(NTEX(1),15)
      IF (N .GT. 0) THEN
        CALL KANGA2(YC(1),YC(2),ALEN,NTEX(1),-N)
        YC(1)=YC(1)-ALEN
        YC(2)=YC(2)-3*CHUNIT
        CALL KANGA2(YC(1),YC(2),ALEN,NTEX(1),N)
      ENDIF
C NOW LABEL THE DIVISIONS
      L=2
      DO 22 J=1,2
      VAL0=X(J,4)
      II=NDIVS(J,1)/5+NDIVS(J,2)/5 + 1
      NLFT=IFIX((ABS(X(J,4)/X(J,3)))+.001)
      VAL=SIGN(FLOAT(NLFT),X(J,4))*X(J,3)
C CASE WHERE ALL VALUES ARE POSITIVE
      IF (VAL0-VAL .GT. .01*X(J,3)) VAL=VAL+X(J,3)
      Y(J)=VAL+X(J,3)
      Y(L)=X(L,5)
      CALL PLCONV(Y(1),Y(2),4,YC(1),YC(2),7)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
      STEP=YC(J)
      Y(J)=VAL
      CALL PLCONV(Y(1),Y(2),4,YC(1),YC(2),7)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
C
      YC(L)=YC(L)-1.5*CHUNIT
      STEP=STEP-YC(J)
C
      DO 4 I=1,II
      IF (ABS(X(J,5)-VAL).LT.10E-4) GO TO 6
      IF (ABS(VAL).LT. 10E-5) GO TO 6
      CALL FETTLE(VAL,IW,IP)
      CALL NUMA1(VAL,IW,IP,IW,ITEXT)
      IW2=IW/2
      CALL KANGA2(YC(1)-IW2*CHUNIT,YC(2),ALEN,ITEXT,IW)
    6 YC(J)=YC(J)+STEP
      VAL=VAL+X(J,3)
    4 CONTINUE
      L=1
   22 CONTINUE
C
C  DRAW AXES
      L=2
      CALL SPCSET(4)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      DO 23 J=1,2
      YO=X(L,5)
      Y2=XS/S(L)
      Y1=YO+Y2
      Y2=Y1+Y2
      II=NDIVS(J,1)+NDIVS(J,2) + 1
      STEP=X(J,3)/5.
      Y(L)=YO
      Y(J)=X(J,4)
      CALL KANGA1(A,B,3)
      Y(J)=Y(J)+AMOD(X(J,4),STEP)
C
      DO 5 I=1,II
      CALL KANGA1(A,B,2)
      Y(L)=Y1
      BIT=X(J,3)/50.
      BITTLE=BIT/10.
      TEST=ABS(AMOD(ABS(Y(J))+BITTLE,X(J,3)))
      IF (TEST.LT.BIT) Y(L)=Y2
      CALL KANGA1(A,B,1)
      Y(L)=YO
      CALL KANGA1(A,B,3)
      Y(J)=Y(J)+STEP
    5 CONTINUE
      L=1
   23 CONTINUE
C
C
C USE THIS IF WANT A FRAME:
C      CALL SPCSET(2)
C      CALL FRAME(0.,0.,PW,PH)
      GO TO 100
C
C  ENTRY TO PLOT A POINT WITH AN ERROR BAR
    3 CALL SPCSET(4)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN
      A=XX
      B=YY
      X1=A-0.5*YS
      X2=X1+YS
C MAKE THE SYMBOLS BIGGER
      SIZ=1.5*YS*S(1)
      IF (ER.EQ.0) GO TO 20
      CALL KANGA3(A,B,SIZ,MODE)
      IF (2.*ER*S(2) .LT. SIZ) GO TO 100
      B=B+ER
      CALL KANGA1(X1,B,3)
      CALL KANGA1(X2,B,2)
      CALL KANGA1(A,B,3)
      B=B-2*ER
      CALL KANGA1(A,B,2)
      CALL KANGA1(X1,B,3)
      CALL KANGA1(X2,B,2)
      GO TO 100
C
C  ENTRY TO PLOT A VECTOR
    1 CALL SPCSET(4)
C>> JCC Added
	IF (IBMBER .NE. 0) RETURN

      IF (MODE.GE.-3) GO TO 40
C  SET UP FOR DASHED LINES
      DASH=ER
   40 J=IABS(MODE)
      IF (J.EQ.3) J=0
      CALL DPLOT(XX,YY,J)
      GO TO 100
C
C  CALCULATED POINT
   20 CALL KANGA3(A,B,SIZ,MODE)
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE PLTRIN(PMAT,N,M)
      SUBROUTINE PLTRIN(PMAT,N,M)
C
C *** PLTRIN by JCM 4 Apr 85 ***
C
CX
CC 14B
CH Defines a new coordinate transformation for plotting.
CA On entry PMAT is a 2 X 3 real matrix which defines the coordinate system
CA N with respect to M.
CA          PMAT(1:2,1:2) gives the scaling and rotation
CA          PMAT(1:2,3) gives the origin of space N with respect to that of
CA                      space M, in M's cordinates.
CA M must be greater than N
C
CD PMAT and N will be written as items number N-1 in PMTRIX and NTRAN in
CD COMMON /PLTRAN/. MAXSP in /PLTRAN/ holds the largest N yet given.
C
      DIMENSION PMAT(2,3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
      IF (M .LT. N .AND. M .GT. 0) GO TO 1
      WRITE (LPT,3000) N,M
      WRITE (ITO,3000) N,M
3000  FORMAT (/' ERROR ** TRYING TO TRANSFORM FROM SPACE',I4,
     & ' TO SPACE',I4,' IN PLTRIN')
      IBMBER =  1
	RETURN
C
   1  CALL GMEQ(PMAT,PMTRIX(1,1,N-1),2,3)
      NTRAN(N-1)=M
      IF (N .GT. MAXSP) MAXSP=N
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE PLTTXT
      SUBROUTINE PLTTXT
C
C *** PLTTXT updated by JCM 29 Apr 92 ***
C
CX
CC 5B
CH Plots a block of explanatory text under a plotted Fourier map.
C
CD Plots the block of text which lists all the necessary details for
CD identification of the map.  Does this in what is for the Fourier plotting
CD "character 1 space", space number 5.
C
CO Outputs to graphical device as set up by PIGLET
C
      CHARACTER *10 MTYP(5)
      CHARACTER *1 IXCHR(2)
      CHARACTER *52 LINE1
      CHARACTER *58 LINE2
      CHARACTER *63 LINE3
      CHARACTER *50 LINE4
      CHARACTER *45 LINE5
      CHARACTER *22 LINE6
      CHARACTER *75 LINE7
      DIMENSION ITYPE(7)
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      DATA IXCHR/'X','Y'/
      DATA MTYP/'  FCALC','   FOBS','FOBS-FCALC',' FOBS**2',
     & 'SIGMA FOBS'/
      DATA LINE1/'    FOURIER PROJECTION DOWN'/
      DATA LINE2/'  FOURIER SECTION AT           ON'/
      DATA LINE3/'   parallel to                         from         to
     & '/
      DATA LINE4/'Coefficients are            multiplied by'/
      DATA LINE5/'    and averaged over a cube of edge'/
      DATA LINE6/'No resolution function'/
      DATA LINE7/'Bounded section from          to          on'/
      DATA ITYPE/1,2,2,3,3,4,5/

C>> JCC Added error trapping
	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0
C
C WE SHOULD BE NOW WORKING IN CHAR 1 SPACE:
      CALL SPCSET(5)
	IF (IBMBER .NE. 0) RETURN
C
C BLACK PEN:
      CALL PIGLET(0.,0.,-1)
	IF (IBMBER .NE. 0) RETURN
C
C
C COLLECT LENGTH OF LONGEST LINE FOR FRAME SIZE:
      A=0.
C
C LINES OF TEXT:
      IF (NDIM.EQ.3) GO TO 3
      IF (NDIM.EQ.4) GO TO 12
      DO 4 I=1,3
      CALL FETTLE(U(I,3),IW,IF)
   4  CALL NUMA1(U(I,3),IW,IF,8,LINE1(21+8*I:28+8*I))
      CALL KANGA2(CHUNIT,8.5*CHUNIT,TEMP,LINE1,52)
      IF (TEMP .GT. A) A=TEMP
      GO TO 5
    3 DO 6 I=1,3
      CALL FETTLE(U(I,3),IW,IF)
    6 CALL NUMA1(U(I,3),IW,IF,8,LINE2(27+8*I:34+8*I))
      CALL FETTLE(OUTLIM(1,3),IW,IF)
      CALL NUMA1(OUTLIM(1,3),IW,IF,9,LINE2(22:30))
      CALL KANGA2(CHUNIT,8.5*CHUNIT,TEMP,LINE2,58)
      IF (TEMP .GT. A) A=TEMP
      GO TO 5
C
  12  DO 13 I=1,3
      CALL FETTLE(U(I,3),IW,IF)
      IP=38+8*I
   13 CALL NUMA1(U(I,3),IW,IF,8,LINE7(IP:IP+7))
      CALL FETTLE(OUTLIM(1,3),IW,IF)
      CALL NUMA1(OUTLIM(1,3),IW,IF,9,LINE7(22:30))
      CALL FETTLE(SECEND,IW,IF)
      CALL NUMA1(SECEND,IW,IF,9,LINE7(33:41))
      CALL KANGA2(CHUNIT,8.5*CHUNIT,TEMP,LINE7,64)
      IF (TEMP .GT. A) A=TEMP
C
C LINES DESCRIBING AXES:
   5  Y=6.3*CHUNIT
      DO 1 J=1,2
      DO 8 I=1,3
      CALL FETTLE(U(I,J),IW,IF)
    8 CALL NUMA1(U(I,J),IW,IF,8,LINE3(7+8*I:14+8*I))
      DO 9 I=1,2
      CALL FETTLE(OUTLIM(I,J),IW,IF)
    9 CALL NUMA1(OUTLIM(I,J),IW,IF,7,LINE3(33+11*I:39+11*I))
      LINE3(2:2)=IXCHR(J)
      CALL KANGA2(CHUNIT,Y,TEMP,LINE3,62)
      IF (TEMP .GT. A) A=TEMP
    1 Y=Y-CHUNIT*2.
C
C COEFFICIENT TYPE AND SCALE FACTOR:
      J=ITYPE(MODEF)
      CALL FETTLE(SCALF1,IW,IF)
      CALL NUMA1(SCALF1,IW,IF,9,LINE4(42:50))
      LINE4(18:27)=MTYP(J)
      CALL KANGA2(CHUNIT,2.3*CHUNIT,TEMP,LINE4,50)
      IF (TEMP .GT. A) A=TEMP
C
C DECIDE ON MESSAGE ABOUT RESOLUTI0N FUNCTION
      IF (DELTA .NE. 0.) GO TO 11
      CALL KANGA2(8.*CHUNIT,CHUNIT/2.,TEMP,LINE6,22)
      IF (TEMP .GT. A) A=TEMP
      GO TO 101
  11  TWODEL=2.*DELTA
      CALL FETTLE(TWODEL,IW,IF)
      CALL NUMA1(TWODEL,IW,IF,8,LINE5(38:45))
      CALL KANGA2(CHUNIT,0.3*CHUNIT,TEMP,LINE5,45)
      IF (TEMP .GT. A) A=TEMP
C
C DRAW BOXES TO BE ROUND EXPLANATORY TEXT:
 101  CALL FRAME(0.,0.,A+2.*CHUNIT,10.*CHUNIT)
      CALL KANGA1(0.,4.*CHUNIT,3)
      CALL KANGA1(A+2.*CHUNIT,4.*CHUNIT,2)
      CALL KANGA1(A+2.*CHUNIT,8.*CHUNIT,3)
      CALL KANGA1(0.,8.*CHUNIT,2)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PMTINV(A,B)
      SUBROUTINE PMTINV(A,B)
C
C *** PMTINV by JCM 18 May 84 ***
C
CX
CC 14B
CH Specialist routine to invert a 2x3 matrix, such as those which transform
CH plotting coordinates from one space to another.
C
CA On entry A holds a 2x3 matrix as A(2,3)
CA On exit  B(2,3) holds the matrix representing the inverse transformation.
C
      DIMENSION A(2,3),B(2,3)
      D=1./(A(1,1)*A(2,2)-A(1,2)*A(2,1))
      B(1,1)=A(2,2)*D
      B(1,2)=-A(1,2)*D
      B(1,3)=(A(1,2)*A(2,3) - A(1,3)*A(2,2))*D
      B(2,1)=-A(2,1)*D
      B(2,2)=A(1,1)*D
      B(2,3)=(A(1,3)*A(2,1)-A(1,1)*A(2,3))*D
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE PMTMUL(A,B,C)
      SUBROUTINE PMTMUL(A,B,C)
C
C *** PMTMUL by JCM 1 May 84 ***
C
CX
CC 14B
CH Specialist routine to multiply together two 2x3 matrices, such as those
CH which transform plotting coordinates from one space to another.
C
CA On entry A and B are 2x3 real arrays
CA On exit  C represents the result of performing B then A.
C
      DIMENSION A(2,3),B(2,3),C(2,3)
C
      C(1,1)=A(1,1)*B(1,1)+A(1,2)*B(2,1)
      C(2,1)=A(2,1)*B(1,1)+A(2,2)*B(2,1)
      C(1,2)=A(1,1)*B(1,2)+A(1,2)*B(2,2)
      C(2,2)=A(2,1)*B(1,2)+A(2,2)*B(2,2)
      C(1,3)=A(1,1)*B(1,3)+A(1,2)*B(2,3)+A(1,3)
      C(2,3)=A(2,1)*B(1,3)+A(2,2)*B(2,3)+A(2,3)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE PRNTMP(L21)
      SUBROUTINE PRNTMP(L21)
C
C *** PRNTMP updated by PJB 29 Apr 88 ***
C
CX
CC 5B
CH Prints out a Fourier projection or one layer of a 3D Fourier.
C
CA On entry L21 is the number of I5 integers required on a line.
CP The values of points on the map must be in array dens in COMMON /MAPDA/,
CP They are usually put there by one of FOUR1Z, FOURGP, ERRMAP, or GETMAP
CP which of these is used is usually decided by FORIER in respons to requests
CP on the M "cards"
CO The array of values is rounded to I5 integers and printed in rows of L21
CO columns on unit LPT
C
C
      LOGICAL EXACT
      DIMENSION JDENS(100),IU(3,3)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
C
      WRITE (LPT,2000) NOBSIN,NUSED
2000  FORMAT ('1Layer of Fourier calculated reading',I10,' reflections',
     & ' and using',I10)
      IF (MODET .EQ. 1) THEN
      DO 4 I=1,3
   4  CALL INDFIX(U(1,I),IU(1,I))
      IF (NDIM .EQ. 2) WRITE (LPT,2001) (IU(I,3),I=1,3),((IU(I,J),
     & I=1,3),J=1,2)
2001  FORMAT (//' Fourier projection down',3I4,' axis'/' x-axis along',
     & 3I4,'  y-axis along',3I4)
      IF (NDIM .EQ. 3) WRITE (LPT,2002) (IU(I,3),I=1,3),OUTLIM(1,3),
     & ((IU(I,J),I=1,3),J=1,2)
2002  FORMAT (//' Fourier section through',3I4,' axis at height',
     & F8.4/' x-axis parallel to',3I4,'  y-axis parallel to',3I4)
      IF (NDIM .EQ. 4) WRITE (LPT,2007) (IU(I,3),I=1,3),OUTLIM(1,3),
     & SECEND,((IU(I,J),I=1,3),J=1,2)
2007  FORMAT (//' Bounded section on ',3I4,' axis from ',F7.4,' to'
     & ,F8.4/' x-axis parallel to',3I4,'  y-axis parallel to',3I4)
      ELSE
      WRITE (LPT,2009) (U(I,3),I=1,3),OUTLIM(1,3),((U(I,J),I=1,3),
     & J=1,2)
2009  FORMAT (//' Fourier section through',3F8.4,' axis at height',
     & F8.4/' x-axis parallel to',3F8.4,'  y-axis parallel to',3F8.4)
      ENDIF
      WRITE (LPT,2003) ((OUTLIM(I,J),I=1,3),J=1,2)
2003  FORMAT (/' Rows of constant x and columns of constant y'/' x',
     & ' varies down the page from',F6.3,' to',F6.3,' in steps of',F6.3/
     & ' y varies along a row from',F6.3,' to',F6.3,' in steps of',F6.3)
      NL = NY/L21
      NR = MOD(NY,L21)
      EXACT = (NR .EQ. 0)
      IF (.NOT. EXACT) NL=NL+1
      DO 1 IX = 1,NX
      CALL MESS(LPT,1,' ')
      M = IX
      DO 2 N = 1,NL
      NEND=L21
      IF (N .EQ. NL .AND. .NOT. EXACT) NEND=NR
      DO 3 I=1,NEND
      JDENS(I) = NINT(DENS(M))
    3 M = M+NX
    2 WRITE (LPT,2004) (JDENS(I),I=1,NEND)
2004  FORMAT (1X,100(I6))
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE QFOUIN(K,F,ALPHA,ENDD)
      SUBROUTINE QFOUIN(K,F,ALPHA,ENDD)
C
C *** QFOUIN DUMMY by JCM 20 Mar 84 ***
C
CX
CC 5B
CH In the library, simply a dummy routine.  If the user wishes some special
CH new input format for Fourier routines, he provides a new version of
CH QFOUIN.
C
CD Called when type 0 Fourier coefficient input is specified.  The user-
CD supplied routine must set:
CD K(1:3) = h,k,l
CD F(1:2) and PHASE according to the value of DTYP on an "M" card,
CD LOGICAL ENDD to be TRUE if there are no more items of data.
C
      LOGICAL ENDD
      DIMENSION K(3),F(2)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE READMP
      SUBROUTINE READMP
C
C *** READMP updated by JCM 22 Aug 86 ***
C
CX
CC 5B
CH Reads into the array DENS a map previously written to file, unformatted.
C
CP Assumes simply that NX by NY numbers have been written to unit IDUMPR
C
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /MAPRD/ZRDVAL(20),ZCRD,IRD,IZRD,IDUMPR
C
      I=0
      DO 1 IY=1,NY
      READ (IDUMPR) (DENS(I+J),J=1,NX)
   1  I=I+NX
      RETURN
      END
C
C
C
C
C LEVEL 3      FUNCTION RESOL(H,D)
      FUNCTION RESOL(H,D)
C
C *** RESOL by JCM 17 Apr 84 ***
C
CX
CC 5B
CH Calculates a resolution function for use with Fourier inversion.
CA On entry H(1:3) contains the indices h,k,l of a reflection.
CA          D is the resolution length in Angstroms.
CD The function value returned in RESOL multiplies the amplitude of the
CD term corresponding to H in the Fourier sum. The result corresponds
CD to averaging the density over a cube of edge 2D.
C
CP RECIP to set up the metric
C
      DIMENSION H(3),OH(3)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      RESOL=1.
      IF (ABS(D) .LT. 0.00001) GO TO 100
      CALL ORTHO(H,OH,2)
      DO 1 I = 1,3
      A = TWOPI*OH(I)*D
      IF (ABS(A) .LT. .001) GO TO 1
      RESOL = RESOL*SIN(A)/A
    1 CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SAVMAP
      SUBROUTINE SAVMAP
C
C *** SAVMAP updated by JCM 10 May 88 ***
C
CX
CC 5B
CH Writes 1 layer of map to given file, plus information for later retrieval.
C
CP One of FOUR1Z, FOURGP etc must have been obeyed first to produce map.
C
CO Writes on first entry a header block, plus one map;  on subsequent entries,
CO one map, and on the final entry one map and a trailer block.
C
CO The material written is the value of Z, NOBS and NUSED, and the array DENS.
C
CN The map can be later got back by GETMAP.
C
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /MAPSV/ZSVVAL(20),ZCSV,ISV,IZSV,NDUMPS,NSAV
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

C
C
C NSAV=HOW MANY MAPS ALREADY SENT - IF 0, SEND HEADER:
      IF (NSAV .GT. 0) GO TO 2
C
C * THERE IS A STRONG SUSPICION THAT THE USE OF NTOTAL WILL SIMPLIFY THIS
C COUNT AND CHECK NUMBER OF CARDS IN FILE IO10:
      NCDS=0
      DO 5 I=1,26
   5  NCDS=NCDS+ICDNO(I)
      WRITE (NDUMPS) INREAD,ICDNO,NCDS
      ICD=0
      ID=0
   9  ID=ID+1
      IF (ID .GT. NTOTAL(1)) GO TO 6
      READ (IO10,REC=ID,FMT=1000) ICARD
1000  FORMAT (A80)
      WRITE (NDUMPS) ICARD
      IF (ICARD(1:1) .NE. 'Y' .AND. ICARD(1:1) .NE. 'Z')ICD=ICD+1
      GO TO 9
C
   6  IF (ICD .EQ. NCDS) GO TO 2
      WRITE (LPT,3000) IO10,NCDS
      WRITE (ITO,3000) IO10,NCDS
3000  FORMAT (/' ERROR ** FILE',I3,' AND VECTOR ICDNO ARE',
     & ' OUT OF STEP - ICDNO EXPECTS',I4,' CARDS')
C>> JCC Was      STOP
	CALL BMBOUT
	RETURN
C
C WRITE INFO FOR ONE MAP:
   2  WRITE (NDUMPS) OUTLIM(1,3),NOBSIN,NUSED,NX,NY
      I=0
      DO  4 IY=1,NY
      WRITE (NDUMPS) (DENS(I+J),J=1,NX)
   4  I=I+NX
      NSAV=NSAV+1
C
 101  IF (NSAV .EQ. 1) WRITE (LPT,2003) NCDS
2003  FORMAT (/' Header record with',I4,' cards written')
      IF (NDIM .EQ. 2) WRITE (LPT,2001) NSAV
2001  FORMAT (/' Map no.',I3,' saved')
      IF (NDIM .EQ. 3) WRITE (LPT,2002) NSAV,OUTLIM(1,3)
2002  FORMAT (/' Map no.',I3,' saved for Z=',F10.4)
      IF (ZCSV .LE. 99998.) GO TO 100
C
C WRITE TRAILER:
      WRITE (NDUMPS) 99999.,0,0,0,0
      CALL MESS(LPT,1,'Trailer record written')
 100  RETURN
      END
C
C
C
C
C LEVEL 11      SUBROUTINE SETFOU
      SUBROUTINE SETFOU
C
C *** SETFOU updated by JCM 22 Aug 86  ***
C
CX
CC 5A
CH Sets up data for Fourier map calculations.
CD Reads in:  one "N" card with the title
CD            several "M" cards with map information (including that
CD            for contour plotting if required by user)
CD Sets up:   COMMON ready for calls of FORIER, which will then organise
CD            the actual calculation of (possibly several) maps, and their
CD            plotting, printing and saving as requested.
CD
CD Sets defaults of: 3 dimensions if no M NDIM card is given
CD            no resolution if no M DELT card is given
CD            a scale for Fourier coefficients of 1. if no M SCAL card is given
CD            a map scale of 2.5 cm/Angstrom if no M CM/A card is given.
CD Checks that M cards have been read for DTYP, SMAX, FTYP, MESH.
CI Causes "N" and "M" cards to be read from the copy of the Crystal
CI Data File on unit IO10.
CO Writes its findings to unit LPT.
C
      LOGICAL TESTOV
      DIMENSION KLIM(3),UU(3,3),NPOINT(100)
      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),
     & CELESD(6,6,2),CELLSD(6,6),KOM4
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /FRIED/FRIEDL,KOM8
      LOGICAL FRIEDL
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      DIMENSION INREAD(26),ICDNO(26)
      EQUIVALENCE (INREAD(1),INREA(1,1))
      EQUIVALENCE (ICDNO(1),ICDN(1,1))
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /MAPGT/ZGTVAL(20),ZCGT,IGT,IZGT,IDUMPG
      COMMON /MAPPR/ZPRVAL(20),ZCPR,IPR,IZPR
      COMMON /MAPRD/ZRDVAL(20),ZCRD,IRD,IZRD,IDUMPR
      COMMON /MAPSV/ZSVVAL(20),ZCSV,ISV,IZSV,NDUMPS,NSAV
      COMMON /MREAD/IMREAD(15)
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP

C
C RECIP MUST BE OBEYED FIRST (AND THEREFORE SYMOP ALSO)
      IF (INREAD(3) .GT. 0) CALL RECIP
C
C COMMENT THAT FRIED CANNOT BE USED (IF FALSE) HERE:
      IF (.NOT. FRIEDL) CALL MESS(LPT,1,
     & 'Friedel''s law being assumed for Fourier')
C
C READ, STORE AND PRINT TITLE FROM AN "N" CARD:
      CALL INPUTN(LPT)
C
C NOW READ ALL "M" CARDS:
      CALL INPUTM
C
C NOW CHECK ALL INFO GIVEN, AND WRITE OUT FOR THOSE CARDS NOT GIVEN
      IF (IMREAD(11) .EQ. 0) CALL ERRMES(2,1,
     & 'M DTYP card giving data input type')
C
      IF (IMREAD(12) .LE. 0) THEN
        CALL MESS(LPT,1,'No resolution function')
        DELTA=0.
      ENDIF
C
      IF (IMREAD(15) .LE. 0) CALL ERRMES(2,1,
     & 'sin theta/lambda maximum')
C
      IF (IMREAD(13) .EQ. 0) SCALF1=1.0
C
      IF (IMREAD(1) .LE. 0) THEN
        NDIM=3
        CALL MESS(LPT,1,'No M NDIM card - assuming 3D')
      ENDIF
C
      IF (IMREAD(2) .LE. 0) CALL ERRMES(2,1,
     & 'M FTYP card giving type of Fourier required')
C
      IF (IMREAD(3) .LE. 0) CALL ERRMES(2,1,
     & 'M MESH card giving mesh for output')
C
      IF ((IMREAD(6) .NE. 0) .AND. (IMREAD(7) .LE. 0)) THEN
        SCALMP=2.5
       CALL MESS(LPT,1,
     & 'Default scale of 2.5 cm/Angstrom for plotted map')
      ENDIF
C
C SET UP SCALE ACCORDING TO NUMBER OF DIMENSIONS:
      IF (IMREAD(4) .EQ. 0) THEN
        CALL GMUNI(U,3)
        MODET=0
      ENDIF
      IF (MODET.EQ.1) THEN
        M=1
      ELSE
        M=0
      ENDIF
      CALL USYM(M)
C
C WE ARE NOW DOWN TO DECIDING WHERE THE MAP IS TO COME FROM, AND WHAT IS
C TO BE DONE WITH IT ONCE WE HAVE IT.
C
C SAVE, GET, READ, PRIN AND PLOT CARDS HAVE ALL BEEN INTERPRETED IN INPUTM.
C HERE WE NEED ONLY MAKE SURE WE HAVE SOMETHING SENSIBLE TO DO.
C
C FIRST SORT ALL LISTS INTO ASCENDING ORDER:
      IF (NCONT .GT. 0) THEN
        CALL SORTX(CONT,NPOINT,NCONT)
        CALL RESHUF(CONT,NPOINT,NCONT)
      ENDIF
      IF (IZPR .GT. 0) THEN
        CALL SORTX(ZPRVAL,NPOINT,IZPR)
        CALL RESHUF(ZPRVAL,NPOINT,IZPR)
      ENDIF
      IF (IZPL .GT. 0) THEN
        CALL SORTX(ZPLVAL,NPOINT,IZPL)
        CALL RESHUF(ZPLVAL,NPOINT,IZPL)
      ENDIF
      IF (IZSV .GT. 0) THEN
        CALL SORTX(ZSVVAL,NPOINT,IZSV)
        CALL RESHUF(ZSVVAL,NPOINT,IZSV)
      ENDIF
      IF (IZRD .GT. 0) THEN
        CALL SORTX(ZRDVAL,NPOINT,IZRD)
        CALL RESHUF(ZRDVAL,NPOINT,IZRD)
      ENDIF
      IF (IZGT .GT. 0) THEN
        CALL SORTX(ZGTVAL,NPOINT,IZGT)
        CALL RESHUF(ZGTVAL,NPOINT,IZGT)
      ENDIF
C
      IF ((IMREAD(5) .LE. 0).AND.(IMREAD(6) .LE. 0).AND.(IMREAD
     & (9) .LE. 0)) CALL ERRMES(1,1,
     & 'no M PRIN, M PLOT or M SAVE cards')
C
      SCALF2=V(2)
      IF (NDIM .EQ. 2) SCALF2=SCALF2*VCTMOD(1.,U(1,3),1)
C
C CALCULATE AND CHECK MAXIMUM VALUES OF INDICES, LIMITS ETC:
      CALL GMUNI(UU,3)
      IF (MODET .EQ.1) CALL GMEQ(U,UU,3,3)
      DO 1 I=1,2
      KLIM(I)=IFIX(VCTMOD(2.*SMAX,UU(1,I),1))
      IF (TESTOV(1.,OUTLIM(3,I))) THEN
        CALL ERRIN2(I,1,'zero step length for Fourier in direction',' ')
        GO TO 1
      ENDIF
C
      IF (OUTLIM(3,I) .LE. 0.) THEN
        WRITE (LPT,3001) OUTLIM(3,I),I
        WRITE (ITO,3001) OUTLIM(3,I),I
3001    FORMAT (' ERROR ** Negative step for Fourier',F10.4,
     & ' in direction',I2)
        IERR=IERR+1
        GO TO 1
      ENDIF
C
      NN=NINT((OUTLIM(2,I)-OUTLIM(1,I)+10.E-5)/OUTLIM(3,I)) +1
      IF (I .EQ. 1) NX=NN
      IF (I .EQ. 2) NY=NN
      IF (NN .GT. 0) GO TO 1
      WRITE (LPT,3002) NN,I
      WRITE (ITO,3002) NN,I
3002  FORMAT (' ERROR ** Negative number',I3,' of points ',
     & 'required for Fourier in direction',I2)
      IERR=IERR+1
   1  CONTINUE
C
C CHECK STORAGE LIMITS:
      NH=2*KLIM(1)+1
      NK=KLIM(2)+1
      NHK=2*NH*NK
      IF (NHK .GT. 10201) THEN
        WRITE (LPT,3010) KLIM(1),KLIM(2),NHK
        WRITE (ITO,3010) KLIM(1),KLIM(2),NHK
3010    FORMAT (/' ERROR ** Too much data for Fourier store'/
     &  ' h max=',I4,' k max=',I4, 'needing space',I7,' - only 10201',
     &  ' available')
C>> JCC WAS STOP
        CALL BMBOUT
	  RETURN
      ENDIF
C
      NXY=NX*NY
      IF (NXY .GT. 10201) THEN
        WRITE (LPT,3011) NX,NY,NXY
        WRITE (ITO,3011) NX,NY,NXY
3011    FORMAT (/' ERROR ** Too much data for Fourier store'/
     & ' No. of points in x direction =',I5/
     & ' no. of points in y direction =',I5/
     & ' requiring store',i7,' - only 10201 available')
C>> JCC WAS STOP
        CALL BMBOUT
	  RETURN
      ENDIF
C
      IF (MODET.EQ.2) THEN
C  REDEFINE NH,NK,NHK TO BE MAX VALUES OF INDICES FOR GENERAL PLANE FOURIER
        NHK=IFIX(VCTMOD(2.*SMAX,UU(1,3),1))
        NH=KLIM(1)
      ELSE
        NKX=2*NK*NX
        IF (NKX .GT. 10201) THEN
          WRITE (LPT,3012) KLIM(1),NX,NKX
          WRITE (ITO,3012) KLIM(1),NX,NKX
3012      FORMAT (/' Error ** too much data for Fourier store'/
     &    ' k max =',I4,' no. of points in x direction =',I5/
     &    ' requiring store',i7,' - only 10201 available')
          CALL BMBOUT
	    RETURN
        ENDIF
C
        CALL ERRCHK(1,KLIM(1)+1,100,0,'h values in Fourier')
        CALL ERRCHK(1,NK,100,0,'k values in Fourier')
      ENDIF
C
C NOW DISCOVER WHETHER PLOTTING
      IF (IMREAD(6) .NE. 0) CALL STPLOT
      IF (IERR .NE. 0) CALL ERRMES(1,0,'during SETFOU')
C
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE SPCSET(N)
      SUBROUTINE SPCSET(N)
C
C *** SPCSET updated by JCM 14 Nov 89 ***
C
CX
CC 14B
CH Defines the "space" in which coordinates will be given for plotting.
CA On entry N is the number of the space in which subsequent "plot"
CA            coordinates will be given.
C
CD The current space is held in NSPCE in COMMON /PLTRAN/.
CD  Coordinate systems useful in most plotting applications:
CD        1=Plotter (actual coordinates on a particular graphical output device)
CD        2=CCSL (the coordinates in which the programs are written)
CD Coordinate sytems 3 to 7 are for user applications; for example in plotting
CD Fourier maps they are used as follows:
CD        3=Picture (one "picture" which contains one section of the map)
CD        4=Map (The crystallographically related axes of the Fourier
CD               calculation).
CD        5=Character type 1 (in contouring, the text under the map)
CD        6=Character type 2 ( the title over the map)
CD        7=Character type 3 (the contour list panel)
C
CD A new conversion matrix is set up into PTRAN(,,1).
CD It should take a point expressed in current coordinates (space N) into
CD plotter coordinates (space 1).
C
CP PLTRIN  must have been used to set up matrices defining the
CP transformation from space N to "plotter" space.
C
CP For plotting FOURIERS:
CP Matrix 1 takes CCSL into plotter
CP Matrix 2 takes picture into CCSL
CP Matrix 3 takes map into picture
CP Matrix 4 takes char 1 into picture
CP Matrix 5 takes char 2 into picture (if required)
CP Matrix 6 takes char 3 into picture (if required)
CP
CP These must have been set up initially.  They are mostly unchanging,
CP but to move from one picture to the next we alter column 3 of matrix 2.
C
      DIMENSION PTEMP(2,3)
      COMMON /PLTRAN/PMTRIX(2,3,10),PTRAN(2,3,2),NSPCE,NCON1,NCON2,MPEN,
     & NTRAN(10),MAXSP
C
C CHECK PLAUSIBLE N:
      IF (N .LE. 0 .OR. N .GT. MAXSP) CALL ERRIN2(N,0,
     & 'cannot set space','in SPCSET')
C
   1  IF (N .GT. 1) GO TO 2
      CALL GMZER(PTRAN(1,1,1),2,3)
      CALL GMUNI(PTRAN(1,1,1),2)
      GO TO 101
C
C ANY SPACE OTHER THAN 1:
   2  M=N-1
      CALL GMEQ(PMTRIX(1,1,M),PTRAN(1,1,1),2,3)
C
C HAVE WE REACHED PLOTTER SPACE YET?
   3  M=NTRAN(M)-1
      IF (M .LT. 0) CALL ERRIN2(N,0,'Space',
     & 'in plotting not available - check setting up')
      IF (M .EQ. 0) GO TO 101
      CALL PMTMUL(PMTRIX(1,1,M),PTRAN(1,1,1),PTEMP)
      CALL GMEQ(PTEMP,PTRAN(1,1,1),2,3)
      GO TO 3
C
 101  NSPCE=N
      RETURN
      END
C
C
C
C
C LEVEL 7      SUBROUTINE STPLOT
      SUBROUTINE STPLOT
C
C *** STPLOT updated by JCM 26 Sep 87 ***
C
CX
CC 14A
CH Sets up the plotting of maps;  fits elements of a picture together.
C
CP Plotting of a Fourier map must be set up by SETFOU.  In particular:
CP NX=number of x values wanted for plotted points
CP NY=number of y values wanted for plotted points
CP PMAP(2,2) in /CONTUR/ holds the matrix which converts map coords
CP       to cms, set by USYM.
CP There is a title in ITITLE, of length NTITLE, read by INPUTN
C
CD Calls the special plotting routine PIGLET to do whatever is necessary
CD locally to start using graphical output.
CD
CD Decides the scales of various sizes of characters which will be
CD written on a picture.  Sets up all conversion matrices needed in
CD subsequent map plotting, by calculating the layout of the various
CD elements of a picture (map, title, expanatory text, etc) within its
CD frame.
CD
CD Finally calls PIGLET to do whatever is required locally to start the
CD whole plot, now we know how big it will be.
C
      DIMENSION PTEMP(2,3)
      COMMON /BITMAP/IBIT(108,4),NWORDS
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /LENINT/NBITS
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NTITL/NTITLE,KOM14
      COMMON /PLODAT/PAPERW,PAPERH,ASPECT,BORDER,CHUNIT,FROMCM
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
      COMMON /TITLE/ITITLE
      CHARACTER *80 ITITLE
C>> JCC Implement error tracking rather than stopping

	INTEGER IBMBER
      COMMON / CCSLER / IBMBER
	IBMBER = 0

C
C INITIALISE ANY SPECIAL HARDWARE QUANTITIES FOR PLOTTER:
      CALL PIGLET(0.,0.,0)
	IF (IBMBER .NE. 0) RETURN
C
C SET UP NWORDS=NUMBER OF CONSECUTIVE INTEGERS OF LENGTH NBITS NEEDED TO
C TAKE 108 BITS (OR WHATEVER IS SET UP):
C%
C      NWORDS=IFIX(%CBIT%./FLOAT(NBITS))+1
      NWORDS=IFIX(108./FLOAT(NBITS))+1
C
C WE MUST CHECK THAT THERE IS ROOM FOR NX ROWS + 2 EDGES IN THE LONG
C DIMENSION OF IBIT, AND THAT BOTH THE NUMBER OF ROWS AND THE NUMBER OF COLUMNS
C WILL FIT IN THE "BITS ALLOWED PER ROW", WHICH IS THE SHORTER DIMENSION
C OF IBIT TIMES THE WORD LENGTH IN NBITS.
C
C NBITS IS ACTUALLY THE WORD LENGTH MINUS ONE, TO AVOID THE SIGN BIT.
C
C%
C      M1DIM=%CBIT%
      M1DIM=108
      M2=NX+2
      MSIZE=NBITS*NWORDS
      M1DIM2=M1DIM-2
      IF (M1DIM2 .LT. NX) THEN
        WRITE (LPT,3000) M1DIM2,NX
        WRITE (ITO,3000) M1DIM2,NX
3000    FORMAT (' ERROR ** Too many x values to plot -',I5,
     &  ' allowed by parameter CM1-2,',I5,' given')
C>> JCC Was STOP
        CALL BMBOUT
	  RETURN
      ENDIF
C
      IF (NX .GT. MSIZE) THEN
        WRITE (LPT,3001) NX,NBITS,NWORDS
        WRITE (ITO,3001) NX,NBITS,NWORDS
3001    FORMAT (' ERROR ** Too many x values to plot -',I5,
     &  ' requested - NBITS,NWORDS in STPLOT=',2I5)
C>> JCC Was STOP
        CALL BMBOUT
	  RETURN
      ENDIF
C
      IF (NY .GT. MSIZE) THEN
        CALL ERRIN2(NY,-1,'too many y values to plot -','requested')
        WRITE (LPT,3003) MSIZE,NWORDS,NBITS
        WRITE (ITO,3003) MSIZE,NWORDS,NBITS
3003    FORMAT (' but only space for',I5,' arising from',I5,' words',
     &  ' using',I5,' bits in each')
C>> JCC Was STOP
        CALL BMBOUT
	  RETURN	  
      ENDIF
C
      FX=FLOAT(NX-1)
      FY=FLOAT(NY-1)
C
C PMAP HAS BEEN SET UP IN USYM FOR MAP TO CMS CONVERSION:
      HGTMAP=-PMAP(2,1)*FX
C WIDTH OF PROJECTING PIECE OF MAP:
      WIDPRO=ABS(PMAP(1,1)*FX)
      WIDMAP=WIDPRO+PMAP(1,2)*FY
C
C DECIDE CHARACTER SCALES - FIRST TYPE 1, THE TEXT BLOCK:
C FIRST TRY TO MAKE THE TEXT BLOCK THE SAME WIDTH AS THE MAP:
      CHSCAL(1,1)=WIDMAP/1050.
C 1050 IS A GUESS, BEING  AN ARBITRARY AMOUNT TO BE THE WIDTH OF A LINE
C OF TEXT, THE WHOLE THING BEING IN CHARACTER COORDINATES.  THE BEST SIZE FOR
C THE FRAME IS CALCULATED IN PLTTXT.
      CHSCAL(2,1)=ASPECT*CHSCAL(1,1)
C IF CHARS TYPE 1 ARE MORE THAN 1 CM OR LESS THAN 0.2 CMS, ADJUST SO THAT THEY
C HIT THE LIMIT:
      CLIM1=1./CHUNIT
      CLIM2=0.2/CHUNIT
      IF (CHSCAL(2,1) .GT. CLIM1) CHSCAL(2,1)=CLIM1
      IF (CHSCAL(2,1) .LT. CLIM2) CHSCAL(2,1)=CLIM2
      CHSCAL(1,1)=CHSCAL(2,1)/ASPECT
C ALL WIDTHS OF ITEMS IN PICTURE ARE WANTED IN CMS:
      WIDTXT=CHSCAL(1,1)*1050.
      HGTTXT=10.*CHSCAL(2,1)*CHUNIT
C THE 10 COVERS 5 LINES OF TEXT AND 5 BLANKS IN BETWEEN
C
C CHAR TYPE 1 IS DEFINITIVE FOR ALL THE WHITE SPACE AND INTERNAL BORDERS:
      XWHITE=CHSCAL(1,1)*CHUNIT
      YWHITE=CHSCAL(2,1)*CHUNIT
      XMARG=4.*XWHITE
      YMARG=4.*YWHITE
C
C NOW CHARACTER TYPE 2, THE TITLE.  WE MAKE THIS MATCH THE TEXT BLOCK BY
C TRYING TO MAKE THEIR WIDTHS THE SAME, BUT IF THIS GIVES CHAR TYPE 2 OF
C MORE THAN 1.5 CMS HIGH, WE ALLOW IT TO BE SMALLER:
C
C FIRST MEASURE LENGTH OF TITLE IN CHARACTER UNITS:
      CALL KANGA2(0.,0.,TLEN,ITITLE,-NTITLE)
      TLEN=TLEN+2.*CHUNIT
      CHSCAL(1,2)=WIDTXT/TLEN
      CLIM1=1.5/CHUNIT
      IF (CHSCAL(1,2) .GT. CLIM1) CHSCAL(1,2)=CLIM1
      CHSCAL(2,2)=CHSCAL(1,2)*ASPECT
      WIDTTL=TLEN*CHSCAL(1,2)
      HGTTTL=2.*CHSCAL(2,2)*CHUNIT
C
C DO NOTHING ABOUT CHARACTER TYPE 3 - CONTOUR LIST PANEL YET:
C A DIGIT IS 20 PLOTTER UNITS WIDE - PRETEND USING TYPE 1 FOR NOW:
      WIDCON=220.*CHSCAL(1,1)
      WIDMAX=WIDMAP
      IF (WIDTXT .GT. WIDMAP) WIDMAX=WIDTXT
      WIDPIC=3.*XMARG+WIDMAX+WIDCON
      HGTPIC=4.*YMARG+HGTMAP+HGTTXT+HGTTTL+2.*YWHITE
C
C NOW SET UP THE VARIOUS MATRICES WHICH MOVE FROM ONE SPACE TO ANOTHER:
C
C A TRANSFORMATION TAKES THE FORM:
C        A B C    OLD X = NEW X
C        D E F    OLD Y   NEW Y
C                  1
C SO A,B,D,E ROTATES OLD X,OLD Y AND C,F GIVES A TRANSLATION.  THUS C AND
C F ARE IN THE SAME UNITS AS NEW X, NEW Y.
C
C PMTRIX 1 MOVES FROM CCSL TO PLOTTER - IT HAS BEEN SET UP IN PINTIL
C
C PMTRIX 2 MOVES FROM PICTURE TO CCSL - WE HAVE NO REASON TO ALTER THE SCALE OR
C ORIENTATION OF A PICTURE IN CCSL SPACE.  (IF, LATER, WE WANT TO DO THIS, HERE
C IS THE PLACE TO DO IT.)
C
C THIS MATRIX IS THE ONLY ONE WHICH CHANGES ONCE SET UP - ITS THIRD COLUMN IS
C THE ONE WHICH MOVES FROM PICTURE TO PICTURE, POSSIBLY PUTTING SEVERAL
C SIDE BY SIDE ON WIDE PAPER.
C
C FIRST CALCULATE HOW SEVERAL PICTURES CAN FIT ON THE PLOTTER:
      IF (HGTPIC .GE. PAPERW) THEN
        WRITE (LPT,3004) HGTPIC,PAPERW
        WRITE (ITO,3004) HGTPIC,PAPERW
3004    FORMAT (' ERROR ** picture of height',F10.2,'cms requested',
     &  ' but plotter paper given as only',F10.2,'cms')
C>> Was STOP
        CALL BMBOUT
	  RETURN        
      ENDIF
C
      NYPIC=IFIX(PAPERW/(HGTPIC+BORDER))
      N1=(IZPL-1)/NYPIC + 1
      N2=NYPIC
      IF (N1 .EQ. 1) N2=IZPL
C
C TOTAL SIZE OF PLOT IN CMS:
      WIDTOT=(WIDPIC+BORDER)*FLOAT(N1)
      HGTTOT=(HGTPIC+BORDER)*FLOAT(N2)
C
C INITIALISE COUNT ACROSS PAGE:
      IYPIC=0
C
C USE PLTRIN TO PUT MATRICES 3 4 5 AND 6 IN PLACE:
C
C PMTRIX 3 TAKES MAP COORDINATES INTO PICTURE COORDINATES.  THE ROTATION
C PART HAS BEEN SET UP BY USYM AND IS IN PMAP IN /CONTUR/
C
      CALL GMEQ(PMAP,PTEMP,2,2)
C MAP ORIGIN IN PICTURE COORDS:
      XO=XMARG
      DIFF=(WIDTXT-WIDMAP)/2.
      IF (DIFF .GT. 0.) XO=XO+DIFF
      IF (WIDPRO .GT. 0.) XO=XO+WIDPRO
      PTEMP(1,3)=XO
C
      PTEMP(2,3)=2.*YMARG+HGTTXT+3.*YWHITE+HGTMAP
      CALL PLTRIN(PTEMP,4,3)
	IF (IBMBER .NE. 0) RETURN
C
C NOW THE CHARACTERS INTO PICTURE MATRICES.  ALL THE ROTATIONS ARE AT
C PRESENT SIMPLE SCALES, NOT ACTUAL ROTATIONS.
C
      CALL GMZER(PTEMP,2,2)
      PTEMP(1,1)=CHSCAL(1,1)
      PTEMP(2,2)=CHSCAL(2,1)
C
C PMTRIX 4 TAKES CHARACTERS OF TYPE 1 INTO PICTURE COORDS.  THE ORIGIN HERE
C IS THE BOTTOM LEFT HAND CORNER OF THE TEXT BLOCK:
      XO=XMARG
      IF (DIFF .LT. 0.) XO=XO-DIFF
      PTEMP(1,3)=XO
      PTEMP(2,3)=YMARG
      CALL PLTRIN(PTEMP,5,3)
	IF (IBMBER .NE. 0) RETURN
C
C PMTRIX 5 IS FOR CHARACTER TYPE 2, THE TITLE CHARACTERS.  THE ORIGIN IS THE
C BOTTOM LEFT HAND CORNER OF THE TITLE BLOCK:
C
      CALL GMZER(PTEMP,2,2)
      PTEMP(1,1)=CHSCAL(1,2)
      PTEMP(2,2)=CHSCAL(2,2)
      DIFF1=(WIDMAP-WIDTTL)/2.
      IF (DIFF .GT. 0.) DIFF1=DIFF1+DIFF
      IF (DIFF1 .LT. 0.) DIFF1=0.
      PTEMP(1,3)=DIFF1+XMARG
      PTEMP(2,3)=HGTPIC-YMARG-HGTTTL
      CALL PLTRIN(PTEMP,6,3)
	IF (IBMBER .NE. 0) RETURN
C
C PMTRIX 6 IS FOR CHARACTER TYPE 3, THE CONTOUR LIST.  THE ORIGIN IS NOT
C YET DETERMINED AS WE DO NOT KNOW HOW MANY CONTOURS WILL BE FOUND
C
C
C WE ARE NOW IN A POSITION TO CALL WHATEVER SPECIAL ROUTINE IS NEEDED TO START
C A PLOT, REQUESTING A PIECE OF PAPER HGTTOT WIDE (ACROSS PLOTTER) AND WIDTOT
C LONG (ALONG LENGTH OF PLOTTER, OFTEN APPROACHING INFINITY IF REALLY A PLOTTER)
C
C WHEN WE START TO APPLY THIS TO VDUS, OUR REQUIRED SCREEN (IN CMS) IS
C WIDTOT WIDE AND HGTTOT HIGH.  BUT WE WOULD IN PRACTICE ASK FOR WIDPIC TIMES
C HGTPIC BECAUSE OF DRAWING ONE AT A TIME.  LEAVE THIS FOR NOW.
C
      IF (FROMCM .GT. 0)   CALL PIGLET(WIDTOT,HGTTOT,999)	
      IF (FROMCM .LE. 0)   CALL PIGLET(WIDPIC,HGTPIC,999)
	IF (IBMBER .NE. 0) RETURN
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE SYMEQU(HI,H,K,N,MI,UU,BETA)
      SUBROUTINE SYMEQU(HI,H,K,N,MI,UU,BETA)
C
C *** SYMEQU updated by PJB 29 Apr 88 ***
C
CX
CC 5B
CH Generates new indices and a phase, in Fourier calculations.
C
CA On entry HI is a 1x3 vector containg h,k,l
CA          N is the number of a symmetry operator
CA          H is an array of indices already found for this HI
CA          MI is the number of entries in H.
CA On exit  EH holds the new h,k,l
CA          UU=0. if these indices have occurred before in the array H
CA             or, if NDIM=2 or 3, =-1 if the (-h,-k,-l) operator was
CA                 necessary to put EH into the correct half of reciprocal
CA                  space with l >=0,
CA                                  +1 if (-h,-k,-l) was not used.
CA            or, if NDIM=2, the transformed l is non-zero;
CA            or, if NDIM=4 for a bounded section, UU= the coefficient
CA                needed for this EH in the sum.
CA          BETA is the phase for this h,k,l.
C
CP Fourier calculations should be set up by SETFOU.
C
      DIMENSION HI(3),EH(3),K(3),H(1)
      COMPLEX BOUND
      LOGICAL TESTOV
C
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
      UU=1.
      CALL ROTSYM(HI,EH,N,2)
      CALL INDFIX(EH,K)
      IF (K(2)) 4,3,2
   3  IF (K(1)) 4,5,2
   5  IF (K(3)) 4,2,2
   4  DO 6 I=1,3
      K(I)=-K(I)
      EH(I)=-EH(I)
   6  CONTINUE
C TO ENSURE FRIEDEL EXACT HALF
      UU=-UU
   2  CALL EQVEC(H,EH,MI,M,NOP)
C DO NOT USE IF EXACTLY THIS HKL HAS ALREADY OCCURRED FROM THIS CYCLE:
      IF (M .LE. MI) GO TO 101
      MI = M
C ONLY TEST L IF 2D:
      IF ((NDIM .EQ. 2) .AND. (K(3) .NE. 0)) GO TO 101
      TRA = 0.
      DO 31 I = 1,3
      Q = -SECZER(I)
      IF (NDIM.NE.3 .AND. I.EQ.3) Q=0
      IF (MODEF .NE. 6) Q=Q+TRANS(I,N)
      TRA = TRA+EH(I)*Q
  31  CONTINUE
      BETA = TWOPI*TRA
      IF (NDIM.NE.4) GO TO 100
      IF (TESTOV(1.,EH(3))) GO TO 100
      ARG=TWOPI*EH(3)
      BOUND=CEXP(CMPLX(0.,-ARG*SECEND))
     & -CEXP(CMPLX(0.,-ARG*SECZER(3)))
      BOUND=BOUND*CMPLX(0.,1./ARG)
      ABOUND=CABS(BOUND)
      UU=UU*ABOUND/(SECEND-SECZER(3))
      IF (ABOUND.GT.10E-5)
     &  BETA=BETA+ATAN2(AIMAG(BOUND),REAL(BOUND))
      GO TO 100
C
 101  UU=0.
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE TBOUND(IFOUND)
      SUBROUTINE TBOUND(IFOUND)
C
C *** TBOUND by PJB Aug 86 ***
C
CX
CC 5B
CH A specialist routine used duing the plotting of atomic positions
CH in main program ATMPLO.
C
      DIMENSION Y(3)
C%
C      COMMON/SCRAT/AA(3,3),TRXX(3,%SY*2%),TLAT(3,4),BOUNDS(2,6),XX(3,3),
      COMMON/SCRAT/AA(3,3),TRXX(3,48),TLAT(3,4),BOUNDS(2,6),XX(3,3),
     & TMPV1(3),TMPV2(3),TMPV3(3),TTXX(3,50),NT
      EQUIVALENCE(Y,TMPV1)
C
C  FIRST GET JUST INSIDE THE LOWER BOUND
      IFOUND=0
      CALL GMEQ(Y,XX(1,NT),1,3)
      DO 3 I=1,NT
    1 DIFF=XX(I,NT)-BOUNDS(1,I)
      IF (DIFF.GT. -.001) GO TO 2
      XX(I,NT)=XX(I,NT)+1.
      GO TO 1
C
C  HERE IF INSIDE LOWER BOUND, SEE IF LOWEST POSSIBLE
    2 IF (DIFF.LT.1.) GO TO 3
      XX(I,NT)=XX(I,NT)-1.
      DIFF=DIFF-1.
      GO TO 2
    3 CONTINUE
C
C  NOW TEST UPPER BOUND
      J=NT
   12 IF (BOUNDS(2,J)-XX(J,J).LT.-.001) GO TO 10
      IF (J.EQ.1) THEN
C  TEST IF STRICLY INSIDE BOX
      CALL GMPRD(XX(1,1),AA,TMPV2,1,3,3)
      DO 21 I=1,3
      IF (TMPV2(I).LT.BOUNDS(1,3+I) .OR. TMPV2(I).GT.BOUNDS(2,3+I))
     &  GO TO 10
   21 CONTINUE
      IFOUND=IFOUND+1
      CALL GMEQ(TMPV2,TTXX(1,IFOUND),1,3)
      XX(1,1)=XX(1,1)+1.
      ELSE
      CALL GMEQ(XX(1,J),XX(1,J-1),1,3)
      J=J-1
      ENDIF
      GO TO 12
C
C  HERE IF TESTED POINT OUTSIDE
   10 IF(J.EQ.NT) GO TO 100
      J=J+1
      XX(J,J)=XX(J,J)+1.
      GO TO 12
C
C  HERE WHEN NO MORE POSSIBILITIES
 100  RETURN
C
      END
C
C
C
C
C LEVEL 3      SUBROUTINE USYM(N)
      SUBROUTINE USYM(N)
C
C *** USYM by JCM 19 Mar 83 ***
C
CX
CC 5A
CH Transforms all the symmetry operators by pre- and post-multiplying
CH them by U, the orientation matrix for a Fourier map.
C
CA On entry N=0 if no matrix U has in fact been read.
C
CP Fourier calculations should be set up by SETFOU;  in particular the
CP number of dimensions, NDIM, is needed.
C
CD Each symmetry matrix R except the first is replaced by:
CD         (U)**-1 * R * U
CD and each translation operator T by (U)**-1 * T
C
CD The routine also fills in the matrix PMAP(2,2), using the matrix
CD U expressed on the standard orthogonal axes.  This matrix is
CD then used in a "map to picture" conversion during plotting.
C
CN If the original symmetry matrices are ever needed we can recover the
CN transformations from the first element.
C
      COMMON /CONTUR/ZPLVAL(20),ZCPL,IPL,IZPL,CONT(50),
     & NCONT,PMAP(2,2)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /MAPDA/U(3,3),OUTLIM(3,3),NX,NY,NXY,NH,NK,NHK,NKX,NDIM,
     & DENS(10201),MODEF,NOBSIN,NUSED,SCALF1,SCALF2,DELTA,MODED,SMAX,
     & MODET,SECZER(3),SECEND
      COMMON /NSYM/NOP,NCENT,NOPC,NLAT,NGEN,CENTRC,KOM13
      LOGICAL CENTRC
      COMMON /PLOMAP/WIDTOT,HGTTOT,WIDPIC,HGTPIC,WIDMAP,HGTMAP,WIDTTL,
     & HGTTTL,WIDTXT,HGTTXT,WIDCON,HGTCON,XMARG,YMARG,XWHITE,YWHITE,
     & NYPIC,IYPIC,CHSCAL(2,3),SCALMP
C%
C      COMMON /SCRAT/B(3,3,%SYMO%),A(3,3),C(3,3),TT(3)
      COMMON /SCRAT/B(3,3,24),A(3,3),C(3,3),TT(3)
      COMMON /SYMDA/SYM(3,3,24),TRANS(3,24),ALAT(3,4),
     & ORIGIN(3),KOM26
C
C
      CALL GMEQ(U,A,3,3)
C
C IF UNIT MATRIX, SKIP INVERSION:
      IF (N .EQ. 0) GO TO 10
      CALL TRINV3(A,D)
      WRITE (LPT,2000) D
2000  FORMAT (/' Unit cell transformed for Fourier is ',F6.2,' times',
     & ' original cell')
      IF (ABS(D) .GT. 0.0001) GO TO 2
      CALL ERRMES(1,1,'3 axes of transformed map are coplanar')
      GO TO 100
C
C INTO A PUT INVERSE OF U (NOT TRANSPOSED)
   2  CALL TRANSQ(A,3)
      DO 1 N=2,NOPC
      CALL GMPRD(SYM(1,1,N),U,B(1,1,N),3,3,3)
      CALL GMPRD(A,TRANS(1,N),TT,3,3,1)
      CALL GMEQ(TT,TRANS(1,N),1,3)
   1  CONTINUE
C
C COPY MATRICES BACK:
      DO 3 N=2,NOPC
      CALL GMPRD(A,B(1,1,N),SYM(1,1,N),3,3,3)
   3  CONTINUE
C
  10  DO 4 I=1,3
   4  CALL ORTHO(U(1,I),A(1,I),1)
      GO TO (100,5,6), NDIM
C 2-D PROJECTION:
    5 CALL UNIVEC(A(1,3),TT(3))
      DO 7 I=1,2
      CALL VECPRD(A(1,I),A(1,3),C(1,I))
      CALL UNIVEC(C(1,I),TT(I))
   7  CONTINUE
      COSPHI=SCALPR(C(1,1),C(1,2))
      GO TO 8
C
C 3-D:
   6  DO 9 I=1,2
   9  CALL UNIVEC(A(1,I),TT(I))
      COSPHI=SCALPR(A(1,1),A(1,2))
C
   8  CALL SINCOS(COSPHI,SINPHI,'USYM')
      TEMP=TT(1)*OUTLIM(3,1)*SCALMP
      PMAP(1,1)=TEMP*COSPHI
      PMAP(2,1)=-TEMP*SINPHI
      PMAP(1,2)=TT(2)*OUTLIM(3,2)*SCALMP
      PMAP(2,2)=0.0
 100  RETURN
      END
C
C
C
C
      BLOCK DATA FFTADD
      COMMON /FFTDA/KJUMP,UR(15),UI(15)
      DATA KJUMP/1/
      END
