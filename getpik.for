      SUBROUTINE GETPIK(FILE,lenfil,ier)
C

	INCLUDE 'params.inc'
      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)

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
      COMMON /FPINF1/ KREFT(MFPINF),KNIPT(50,MFPINF),PIKVAL(50,MFPINF)
      COMMON /FPINF2/ NTERMS
c
      common /sappcmn/ xpmin,xpmax,ypmin,ypmax
c
      common /sapgcmn/ xpgmin,xpgmax,ypgmin,ypgmax
c
      character*80  file,inf_file,zm_file
      common /inffil/ lfinf,lfzm,inf_file,zm_file
	ier = 0
C
      OPEN(21,FILE=FILE(:Lenfil),STATUS='OLD',err=998, Iostat = Istat)
C
      CHIOBS=0.
      NFIT=0
      xpmin= 1.e20
      xpmax=-1.e20
      ypmin= 1.e20
      ypmax=-1.e20
      NTERMS=0
      NOBS=0
      MMOBS=MCHSTP
		ittem=0
      DO I=1,MMOBS
        READ(21,*,end=200) XOBS(I),YOBS(I),ESD(I),KTEM
        KREFT(I)=KTEM
        NOBS=NOBS+1
c..        ESD(I)=1./SQRT(WT(I))
        wt(i)=1./esd(i)**2
        YCAL(I)=0.
c..        KTEM=KMAXST(I)-KMINST(I)
        IF (KTEM.GT.0) THEN
           NTERMS=NTERMS+KTEM
           READ(21,*,err=998) (KNIPT(K,I),PIKVAL(K,I),K=1,KTEM)
           NFIT=NFIT+1
           IFIT(NFIT)=I
           CHIOBS=CHIOBS+WT(I)*YOBS(I)**2
        END IF
        xpmin=min(xpmin,xobs(i))
        xpmax=max(xpmax,xobs(i))
        ypmin=min(ypmin,yobs(i))
        ypmax=max(ypmax,yobs(i))
      END DO
 200  xpgmin=xpmin
      xpgmax=xpmax
      ypgmin=ypmin
      ypgmax=ypmax
C      write(56,*) ' In GetPik ',nterms,xpmin,xpmax,ypmin,ypmax
C**
      CLOSE(21)
      GOTO 999
 998  ier=1
      CLOSE(21,IOSTAT=ISTAT)
 999  RETURN
      END
