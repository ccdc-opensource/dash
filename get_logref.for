      SUBROUTINE GET_LOGREF(FILE,lenfil,ier)
C
	include 'params.inc'
      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)

      COMMON /FCSPC2/ ARGK(MFCSP2),DSTAR(MFCSP2)

      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17

      COMMON /SAREFLNS/AIOBS(MSAREF),AICALC(MSAREF)

      COMMON /CHISTO/ KKOR,WTIJ(MCHIHS),S2S(MCHIHS),S4S(MCHIHS),
     &IKKOR(MCHIHS),JKKOR(MCHIHS)
C
      LOGICAL IHMINLT0,IKMINLT0,ILMINLT0
      COMMON /CSQLOG/ IHMINLT0,IKMINLT0,ILMINLT0
      COMMON /CSQINT/ IHMIN,IHMAX,IKMIN,IKMAX,ILMIN,ILMAX,IIMIN,IIMAX
c
      COMMON /CHISTOP/ NOBS,NFIT,IFIT(MCHSTP),CHIOBS,
     &WT(MCHSTP),XOBS(MCHSTP),YOBS(MCHSTP),YCAL(MCHSTP),ESD(MCHSTP)

      COMMON /FPINF/PIK(0:50,MFPINF),KMINST(MFPINF),KMAXST(MFPINF)
C
C	These declarations are needed for the get_logref.inc
C	file to work correctly
C	The following integers represent h,k,l,h+k,h+l,k+l and h+k+l
	INTEGER H_,K_,L_,HPK,HPL,KPL,HPKPL
C	The following integers represent the previous integers, divided by 2 
C	and then multiplied by 2
	INTEGER H_m,K_m,L_m,HPKm,HPLm,KPLm,HPKPLm
c
      include 'statlog.inc'
c
      character*80  file
C
      IHMIN=9999
      IKMIN=9999
      ILMIN=9999
      IIMIN=9999
      IHMAX=-9999
      IKMAX=-9999
      ILMAX=-9999
      IIMAX=-9999

	ier = 0

      OPEN(31,FILE=FILE(:Lenfil),STATUS='OLD',err=998)
C
C**
C      MAXXKK=100000
      MAXXKK = MFCSPE
      MAXK=0
      DO IR=1,MAXXKK
        READ(31,*,err=998,end=200) 
     &     (IREFH(I,IR),I=1,3),ARGK(IR),DSTAR(IR)
        MAXK=MAXK+1
        IHMIN=MIN(IREFH(1,IR),IHMIN)
        IKMIN=MIN(IREFH(2,IR),IKMIN)
        ILMIN=MIN(IREFH(3,IR),ILMIN)
        IHMAX=MAX(IREFH(1,IR),IHMAX)
        IKMAX=MAX(IREFH(2,IR),IKMAX)
        ILMAX=MAX(IREFH(3,IR),ILMAX)
C       Now calculate 'i' index for hexagonals
        ITEM =-(IREFH(1,IR)+IREFH(2,IR))
        IIMIN=MIN(ITEM,IIMIN)
        IIMAX=MAX(ITEM,IIMAX)
      END DO
C
 200  continue
C
C
      IHMINLT0=IHMIN.LT.0
      IKMINLT0=IKMIN.LT.0
      ILMINLT0=ILMIN.LT.0

      if (abs(ihmin).gt.abs(ihmax)) then
        ihmax=abs(ihmin)
      endif

      if (abs(ikmin).gt.abs(ikmax)) then
        ikmax=abs(ikmin)
      endif

      if (abs(ilmin).gt.abs(ilmax)) then
        ilmax=abs(ilmin)
      endif

C
      include 'GET_LOGREF.inc'
C
c	 write(56,*) ' In get_logref ',maxk,ihmin,ikmin,ilmin,
c     & ihmax,ikmax,ilmax,
c     & ihminlt0,ikminlt0,ilminlt0
C
      CLOSE(31)
C
      goto 999
 998  ier=1
	 close(31)
 999  RETURN
      END
