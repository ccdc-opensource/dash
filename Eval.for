CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE MAKEFRAC(CHROM,NGENES)!,NATS,ICOM)
C     -------------------------------------------
C
c      PARAMETER (MAXATM=100)
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /frgcom/ nfrag,lfrag(maxfrg)
      common /zmcomi/ ntatm,natoms(maxfrg),
     &ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),
     &iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),
     &alph(maxatm,maxfrg),bet(maxatm,maxfrg),
     &f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),
     &al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),
     &occ(maxatm,maxfrg)           
      common /zmcomg/ icomflg(maxfrg)
c

	include 'IZMcheck.inc'
      parameter (mvar=100)
      logical log_shad
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
c      DOUBLE PRECISION BLEN,ALPH,BET,F2CMAT
      REAL*8           CHROM(*)
      REAL*8           CKK1,CKK2,CKK3
      REAL*8           TRAN(3),ROTA(3,3),POS(3,MAXATM),
     &CART(MAXATM,3)
      REAL*8           QUATER(4),QQSUM,QDEN,QUATT(MVAR)
      REAL*8           XC,YC,ZC,RADEG,ZERO,ONE,XNORM,V1,V2,V3
C
C      COMMON  /ZMCOMI/ NATOMS,IOPTT(MAXATM),IZ1(MAXATM),IZ2(MAXATM),IZ3(MAXATM)
C      COMMON  /ZMCOMR/ BLEN(MAXATM),ALPH(MAXATM),BET(MAXATM),F2CMAT(3,3)
C      COMMON  /ZMCOMC/ ASYM(MAXATM)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      DATA             ZERO,ONE /0.0D0,1.0D0/
      DATA             RADEG /1.7453292519943296D-2/
C
      KK=0
      KATOM=0
C.. Loop over all the fragments
	IFRG = 0
      DO JJJ=1,NFRAG

	  do while ( ifrg .LE. CheckSize )
		ifrg = ifrg + 1
	    if ( IZMCheck(ifrg) .EQ. 1 ) EXIT ! the loop since we have a fragment we are using
	  end do

       NATS=NATOMS(IFRG)
       KK1=KK+1
       KK2=KK+2
       KK3=KK+3
       CHROM(KK1)=CHROM(KK1)-INT(CHROM(KK1))
       CHROM(KK2)=CHROM(KK2)-INT(CHROM(KK2))
       CHROM(KK3)=CHROM(KK3)-INT(CHROM(KK3))
       IF (LOG_SHAD(KK1)) THEN
         CKK1=CHROM(KK1)+CHROM(KSHAD(KK1))
       ELSE
         CKK1=CHROM(KK1)
       END IF
       IF (LOG_SHAD(KK2)) THEN
         CKK2=CHROM(KK2)+CHROM(KSHAD(KK2))
       ELSE
         CKK2=CHROM(KK2)
       END IF
       IF (LOG_SHAD(KK3)) THEN
         CKK3=CHROM(KK3)+CHROM(KSHAD(KK3))
       ELSE
         CKK3=CHROM(KK3)
       END IF
       TRAN(1)=CKK1*F2CMAT(1,1)
       TRAN(2)=CKK1*F2CMAT(1,2)+CKK2*F2CMAT(2,2)
       TRAN(3)=CKK1*F2CMAT(1,3)+CKK2*F2CMAT(2,3)+CKK3*F2CMAT(3,3)
       KK=KK+3 
C.. If more than one atom then proceed
       IF (NATS.GT.1) THEN
        QQSUM=0.
        DO JQ=1,4
          JQS=JQ+KK
	    IF (LOG_SHAD(JQS)) THEN
	      QUATT(JQS)=CHROM(JQS)+CHROM(KSHAD(JQS))
	    ELSE
	      QUATT(JQS)=CHROM(JQS)
	    END IF
          QQSUM=QQSUM+QUATT(JQS)**2
        END DO
        QDEN=1./SQRT(QQSUM)
        DO JQ=1,4
          JQS=JQ+KK
          QUATT(JQS)=QDEN*QUATT(JQS)
          QUATER(JQ)=QUATT(JQS)
        END DO
        CALL ROTMAK(QUATER,ROTA)
        KK=KK+4
       ENDIF
       DO I=1,NATS
          IF (IOPTB(I,IFRG).EQ.1) THEN
            KK=KK+1
            IF (LOG_SHAD(KK)) THEN
              BLEN(I,IFRG)=CHROM(KSHAD(KK))+CHROM(KK)
            ELSE
              BLEN(I,IFRG)=CHROM(KK)
            END IF
          ENDIF
          IF (IOPTA(I,IFRG).EQ.1) THEN
            KK=KK+1
            IF (LOG_SHAD(KK)) THEN
              ALPH(I,IFRG)=CHROM(KSHAD(KK))+CHROM(KK)
            ELSE
              ALPH(I,IFRG)=CHROM(KK)
            END IF
          ENDIF
          IF (IOPTT(I,IFRG).EQ.1) THEN
            KK=KK+1
            IF (LOG_SHAD(KK)) THEN
              BET(I,IFRG)=CHROM(KSHAD(KK))+CHROM(KK)
            ELSE
              BET(I,IFRG)=CHROM(KK)
            END IF
          ENDIF
       END DO
       CALL MAKEXYZ(NATS,BLEN(1,IFRG),ALPH(1,IFRG),BET(1,IFRG),
     &        IZ1(1,IFRG),IZ2(1,IFRG),IZ3(1,IFRG),
     &        CART(1,1),CART(1,2),CART(1,3))
       ICFRG=ICOMFLG(IFRG)
       IF (ICFRG.EQ.0) THEN
        XC=ZERO
        YC=ZERO
        ZC=ZERO
        DO I=1,NATS
          XC=XC+CART(I,1)
          YC=YC+CART(I,2)
          ZC=ZC+CART(I,3)
        END DO
        XNORM=ONE/DFLOAT(NATS)
        XC=XC*XNORM
        YC=YC*XNORM
        ZC=ZC*XNORM
       ELSE
        XC=CART(ICFRG,1)
        YC=CART(ICFRG,2)
        ZC=CART(ICFRG,3)
       ENDIF
       DO I=1,NATS
          CART(I,1)=CART(I,1)-XC
          CART(I,2)=CART(I,2)-YC
          CART(I,3)=CART(I,3)-ZC
       END DO
       DO I=1,NATS
        POS(1,I)=CART(I,1)
        POS(2,I)=CART(I,2)
        POS(3,I)=CART(I,3)
       END DO
       CALL DO_ATOM_POS(TRAN,ROTA,POS,NATS)
       V1=ONE/F2CMAT(1,1)
       V2=ONE/F2CMAT(2,2)
       V3=ONE/F2CMAT(3,3)
       DO I=1,NATS
        POS(1,I)=POS(1,I)*V1
        POS(2,I)=(POS(2,I)-POS(1,I)*F2CMAT(1,2))*V2
        POS(3,I)=(POS(3,I)-POS(1,I)*F2CMAT(1,3)-POS(2,I)*F2CMAT(2,3))*V3
        KI=KATOM+I
        X(1,KI)=SNGL(POS(1,I))
        X(2,KI)=SNGL(POS(2,I))
        X(3,KI)=SNGL(POS(3,I))
       END DO
       KATOM=KATOM+NATS
      END DO
      END
C
C----------<<<<<<<<<<<<<<<==========+++++++++==========>>>>>>>>>>>>>>>----------
C
      SUBROUTINE MAKEFRAC_PRT(CHROM,NGENES,IWRUN)!,NATS,ICOM)
C     -------------------------------------------
C
c      PARAMETER (MAXATM=100)
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
	integer IWRUN
      common /frgcom/ nfrag,lfrag(maxfrg)
      common /zmcomi/ ntatm,natoms(maxfrg),
     &ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),
     &iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),
     &alph(maxatm,maxfrg),bet(maxatm,maxfrg),
     &f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),
     &al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),
     &occ(maxatm,maxfrg)            
      common /zmcomg/ icomflg(maxfrg)
c
      parameter (mvar=100)
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
c      DOUBLE PRECISION BLEN,ALPH,BET,F2CMAT
      REAL*8           CHROM(*)
      REAL*8           TRAN(3),ROTA(3,3),
     &POS(3,MAXATM),CART(MAXATM,3)
      REAL*8           QUATER(4),QQSUM,QDEN,QUATT(MVAR)
      REAL*8           XC,YC,ZC,ZERO,ONE,XNORM,V1,V2,V3
C
C      COMMON  /ZMCOMI/ NATOMS,IOPTT(MAXATM),IZ1(MAXATM),IZ2(MAXATM),IZ3(MAXATM)
C      COMMON  /ZMCOMR/ BLEN(MAXATM),ALPH(MAXATM),BET(MAXATM),F2CMAT(3,3)
C      COMMON  /ZMCOMC/ ASYM(MAXATM)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      DATA             ZERO,ONE /0.0D0,1.0D0/
      DATA             RADEG /1.7453292519943296D-2/
C
      KK=0
      KATOM=0
C.. Loop over all the fragments
      DO IFRG=1,NFRAG
       NATS=NATOMS(IFRG)

       IF (IWRUN .GT. 0) THEN
        IF (NATS.EQ.1) THEN

	   WRITE(IWRUN,6009) IFRG

 6009    FORMAT('  Fragment number ',I2,' is a single atom')

	  ELSE

	   WRITE(IWRUN,6010) IFRG,NATS
 6010    FORMAT('  Fragment number ',I2,' contains ',I3,' atoms')

        END IF
	 END IF

       KK1=KK+1
       KK2=KK+2
       KK3=KK+3
c       TRAN(1)=CHROM(KK1)*F2CMAT(1,1)
c       TRAN(2)=CHROM(KK1)*F2CMAT(1,2)+CHROM(KK2)*F2CMAT(2,2)
c       TRAN(3)=CHROM(KK1)*F2CMAT(1,3)+CHROM(KK2)*F2CMAT(2,3)+
c     *        CHROM(KK3)*F2CMAT(3,3)
       IF (LOG_SHAD(KK1)) THEN
         CKK1=CHROM(KK1)+CHROM(KSHAD(KK1))
       ELSE
         CKK1=CHROM(KK1)
       END IF
       IF (LOG_SHAD(KK2)) THEN
         CKK2=CHROM(KK2)+CHROM(KSHAD(KK2))
       ELSE
         CKK2=CHROM(KK2)
       END IF
       IF (LOG_SHAD(KK3)) THEN
         CKK3=CHROM(KK3)+CHROM(KSHAD(KK3))
       ELSE
         CKK3=CHROM(KK3)
       END IF
       TRAN(1)=CKK1*F2CMAT(1,1)
       TRAN(2)=CKK1*F2CMAT(1,2)+CKK2*F2CMAT(2,2)
       TRAN(3)=CKK1*F2CMAT(1,3)+CKK2*F2CMAT(2,3)+CKK3*F2CMAT(3,3)

	 IF (IWRUN .GT. 0) THEN
         WRITE(IWRUN,6020) CKK1,CKK2,CKK3
 6020    FORMAT('   and is positioned at ',3f10.5)
       END IF

       KK=KK+3 
C.. If more than one atom then proceed
       IF (NATS.GT.1) THEN
        QQSUM=0.
        DO JQ=1,4
          JQS=JQ+KK
	    IF (LOG_SHAD(JQS)) THEN
	      QUATT(JQS)=CHROM(JQS)+CHROM(KSHAD(JQS))
	    ELSE
		  QUATT(JQS)=CHROM(JQS)
	    END IF
          QQSUM=QQSUM+QUATT(JQS)**2
        END DO
        QDEN=1./SQRT(QQSUM)
        DO JQ=1,4
          JQS=JQ+KK
          QUATT(JQS)=QDEN*QUATT(JQS)
          QUATER(JQ)=QUATT(JQS)
        END DO
        CALL ROTMAK(QUATER,ROTA)
        KK=KK+4
       ENDIF

	 IF (NATS.GT.1) THEN
	  IF (IWRUN .GT. 0) THEN
          WRITE(IWRUN,6030) (SNGL(QUATER(I)),I=1,4)
 6030     FORMAT('   with orientation 4-vector ',4f10.5)
        END IF
      END IF

       DO I=1,NATS
          IF (IOPTB(I,IFRG).EQ.1) THEN
            KK=KK+1
            IF (LOG_SHAD(KK)) THEN
              BLEN(I,IFRG)=CHROM(KSHAD(KK))+CHROM(KK)
            ELSE
              BLEN(I,IFRG)=CHROM(KK)
            END IF
          ENDIF
          IF (IOPTA(I,IFRG).EQ.1) THEN
            KK=KK+1
            IF (LOG_SHAD(KK)) THEN
              ALPH(I,IFRG)=CHROM(KSHAD(KK))+CHROM(KK)
            ELSE
              ALPH(I,IFRG)=CHROM(KK)
            END IF
          ENDIF
          IF (IOPTT(I,IFRG).EQ.1) THEN
            KK=KK+1
            IF (LOG_SHAD(KK)) THEN
              BET(I,IFRG)=CHROM(KSHAD(KK))+CHROM(KK)
            ELSE
              BET(I,IFRG)=CHROM(KK)
            END IF
		  IF (IWRUN .GT. 0) THEN
               WRITE(IWRUN,6040) I,SNGL(BET(I,IFRG))
 6040          FORMAT('   torsion number ',i2,' is ',f10.3)
            END IF
          ENDIF
       END DO
       CALL MAKEXYZ(NATS,BLEN(1,IFRG),ALPH(1,IFRG),BET(1,IFRG),
     &        IZ1(1,IFRG),IZ2(1,IFRG),IZ3(1,IFRG),
     &        CART(1,1),CART(1,2),CART(1,3))
c
       ICFRG=ICOMFLG(IFRG)
       IF (ICFRG.EQ.0) THEN
        XC=ZERO
        YC=ZERO
        ZC=ZERO
        DO I=1,NATS
          XC=XC+CART(I,1)
          YC=YC+CART(I,2)
          ZC=ZC+CART(I,3)
        END DO
        XNORM=ONE/DFLOAT(NATS)
        XC=XC*XNORM
        YC=YC*XNORM
        ZC=ZC*XNORM
       ELSE
        XC=CART(ICFRG,1)
        YC=CART(ICFRG,2)
        ZC=CART(ICFRG,3)
       ENDIF
       DO I=1,NATS
          CART(I,1)=CART(I,1)-XC
          CART(I,2)=CART(I,2)-YC
          CART(I,3)=CART(I,3)-ZC
       END DO
       DO I=1,NATS
        POS(1,I)=CART(I,1)
        POS(2,I)=CART(I,2)
        POS(3,I)=CART(I,3)
       END DO
       CALL DO_ATOM_POS(TRAN,ROTA,POS,NATS)
       V1=ONE/F2CMAT(1,1)
       V2=ONE/F2CMAT(2,2)
       V3=ONE/F2CMAT(3,3)
       DO I=1,NATS
        POS(1,I)=POS(1,I)*V1
        POS(2,I)=(POS(2,I)-POS(1,I)*F2CMAT(1,2))*V2
        POS(3,I)=(POS(3,I)-POS(1,I)*F2CMAT(1,3)-POS(2,I)*F2CMAT(2,3))*V3
        KI=KATOM+I
        X(1,KI)=SNGL(POS(1,I))
        X(2,KI)=SNGL(POS(2,I))
        X(3,KI)=SNGL(POS(3,I))
       END DO
       KATOM=KATOM+NATS
      END DO
      END
C
C----------<<<<<<<<<<<<<<<==========+++++++++==========>>>>>>>>>>>>>>>----------
C
	SUBROUTINE ROTMAK(DC4,ROTA)
C
	REAL*8 DC4(4),EL(4,4),ROTA(3,3)
C
        DO I=1,4
          DO J=I,4
            EL(I,J)=2.*DC4(I)*DC4(J)
          END DO
        END DO
C
        ROTA(1,1)=1.0-(EL(2,2)+EL(3,3))
        ROTA(2,2)=1.0-(EL(1,1)+EL(3,3))
        ROTA(3,3)=1.0-(EL(1,1)+EL(2,2))
        ROTA(1,2)=EL(1,2)-EL(3,4)
        ROTA(1,3)=EL(1,3)+EL(2,4)
        ROTA(2,3)=EL(2,3)-EL(1,4)
        ROTA(2,1)=EL(1,2)+EL(3,4)
        ROTA(3,1)=EL(1,3)-EL(2,4)
        ROTA(3,2)=EL(2,3)+EL(1,4)
C
	RETURN
	END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
