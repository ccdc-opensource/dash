      SUBROUTINE VALCHI(CHIVAL)
C
	include 'params.inc'
      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)

      COMMON /CHISTO/ KKOR,WTIJ(MCHIHS),S2S(MCHIHS),S4S(MCHIHS),
     &IKKOR(MCHIHS),JKKOR(MCHIHS)
      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)
      COMMON /SAREFLNS/AIOBS(MSAREF),AICALC(MSAREF)

      DOUBLE PRECISION PRJADD
      DOUBLE PRECISION PRJMAT0,PRJMAT1,PRJMAT2
      COMMON /PRJMAT/PRJMAT0,PRJMAT1(MPRJMT),PRJMAT2(MPRJMT)

      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150),
     &TF(150),KTF(150),SITE(150),KSITE(150),
     &ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C
      INCLUDE 'GLBVAR.INC'
      include 'statlog.inc'
C
      LOGICAL USEREF
      COMMON /CHIFS1/ KKORSH,IPRJAV(MCHFS1),SPRJAV(MCHFS1)
      COMMON /CHIFS2/ USEREF(MCHFS2)
      COMMON /ITRINF/ iteration
c
	common /temtemtem/ kpp
c
      CALL PRECFC
      SUM1=0.
      SUM2=0.
C
      include 'AllFFCalc.inc'
C
        DO IK=1,KKOR
          II=IKKOR(IK)
          JJ=JKKOR(IK)
          SUM1=SUM1+AICALC(II)*WTIJ(IK)*AIOBS(JJ)
     &       +AICALC(JJ)*WTIJ(IK)*AIOBS(II)
          SUM2=SUM2+AICALC(II)*WTIJ(IK)*AICALC(JJ)
        END DO
c
	lpp=lpp+1
	if (lpp.eq.1) then
!	write(56,*) ' In ValChi ',NumberSGTable
!	write(56,*) ' In ValChi ',kpp,kkor,sum1,sum2
	end if
C
        RESCL=0.5*SUM1/SUM2
        CHIVAL=0.
        PRJMAT0=PRJMAT0+1
        DO IK=1,KKOR
          II=IKKOR(IK)
          JJ=JKKOR(IK)
          DELI=AIOBS(II)-RESCL*AICALC(II)
          DELJ=AIOBS(JJ)-RESCL*AICALC(JJ)
          CHIADD=DELI*WTIJ(IK)*DELJ
          CHIVAL=CHIVAL+CHIADD
          PRJADD=DBLE(CHIADD)
          PRJMAT1(IK)=PRJMAT1(IK)+PRJADD
          PRJMAT2(IK)=PRJMAT2(IK)+PRJADD*PRJADD
        END DO
        CHIVAL = CHIVAL/FLOAT(MAXK-2)
C
      RETURN
      END
C
C
C
      SUBROUTINE PRECFC
C
C... Pre-calculates sin and cosine terms for the structure factor calculation
C
C
	include 'params.inc'
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /FCSTOR/ MAXK,FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)
      COMMON /CSQSTO/ COSQS(-20:20,3,150),SINQS(-20:20,3,150)
      LOGICAL IHMINLT0,IKMINLT0,ILMINLT0
      COMMON /CSQLOG/ IHMINLT0,IKMINLT0,ILMINLT0
      COMMON /CSQINT/ IHMIN,IHMAX,IKMIN,IKMAX,ILMIN,ILMAX,IIMIN,IIMAX
      DATA TWOPI /6.2831853071796/
C
C     cosqs(a,1,c) holds hx terms
C     cosqs(a,2,c) holds ky terms
C     cosqs(a,3,c) holds lz terms
C
      DO N=1,NATOM
        C1N=COS(TWOPI*X(1,N))
        C2N=COS(TWOPI*X(2,N))
        C3N=COS(TWOPI*X(3,N))
        S1N=SIN(TWOPI*X(1,N))
        S2N=SIN(TWOPI*X(2,N))
        S3N=SIN(TWOPI*X(3,N))
        COSQS(0,1,N)=1.
        COSQS(0,2,N)=1.
        COSQS(0,3,N)=1.

        SINQS(0,1,N)=0.
        SINQS(0,2,N)=0.
        SINQS(0,3,N)=0.

C.. IH
        DO IH=1,IHMAX
          IH1=IH-1
          COSQS(IH,1,N)=COSQS(IH1,1,N)*C1N-SINQS(IH1,1,N)*S1N !hx
          SINQS(IH,1,N)=SINQS(IH1,1,N)*C1N+COSQS(IH1,1,N)*S1N !hx
        END DO
        IF (IHMINLT0) THEN
          DO IH=IHMIN,-1
            COSQS(IH,1,N)=COSQS(-IH,1,N)
            SINQS(IH,1,N)=-SINQS(-IH,1,N)
          END DO
        END IF
C>> JCC	   write(56,*) 'ihmax = ', ihmax, ' and ihmin = ',ihmin
C.. IK
        DO IK=1,IKMAX
          IK1=IK-1
          COSQS(IK,2,N)=COSQS(IK1,2,N)*C2N-SINQS(IK1,2,N)*S2N !ky
          SINQS(IK,2,N)=SINQS(IK1,2,N)*C2N+COSQS(IK1,2,N)*S2N !ky
        END DO
        IF (IKMINLT0) THEN
          DO IK=IKMIN,-1
            COSQS(IK,2,N)=COSQS(-IK,2,N)
            SINQS(IK,2,N)=-SINQS(-IK,2,N)
          END DO
        END IF
C.. IL
        DO IL=1,ILMAX
          IL1=IL-1
          COSQS(IL,3,N)=COSQS(IL1,3,N)*C3N-SINQS(IL1,3,N)*S3N
          SINQS(IL,3,N)=SINQS(IL1,3,N)*C3N+COSQS(IL1,3,N)*S3N
        END DO
        IF (ILMINLT0) THEN
          DO IL=ILMIN,-1
            COSQS(IL,3,N)=COSQS(-IL,3,N)
            SINQS(IL,3,N)=-SINQS(-IL,3,N)
          END DO
        END IF
C
      END DO
C
C   	 write(89,*)ihmax,ikmax,ilmax,iimax
C 	 write(89,*)ihmin,ikmin,ilmin,iimin
      RETURN
      END
