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
      BLOCK DATA FFTADD
      COMMON /FFTDA/KJUMP,UR(15),UI(15)
      DATA KJUMP/1/
      END
