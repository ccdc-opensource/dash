!*==GETPIK.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
!
!*****************************************************************************
!
      SUBROUTINE GETPIK(FILE,lenfil,ier)
!
      CHARACTER*(*), INTENT(IN) :: FILE
!
      INCLUDE 'PARAMS.INC'
!
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)
      LOGICAL LOGREF
      COMMON /FCSPEC/ NLGREF, IREFH(3,MFCSPE), LOGREF(8,MFCSPE)
!
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /SAREFLNS/ AIOBS(MSAREF), AICALC(MSAREF)
      COMMON /CHISTO/ KKOR, WTIJ(MCHIHS), S2S(MCHIHS), S4S(MCHIHS),     &
     &                IKKOR(MCHIHS), JKKOR(MCHIHS)
!
      LOGICAL IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQLOG/ IHMINLT0, IKMINLT0, ILMINLT0
      COMMON /CSQINT/ IHMIN, IHMAX, IKMIN, IKMAX, ILMIN, ILMAX, IIMIN,  &
     &                IIMAX
!
      COMMON /CHISTOP/ NOBS, NFIT, IFIT(MCHSTP), CHIOBS, WT(MCHSTP),    &
     &                 XOBS(MCHSTP), YOBS(MCHSTP), YCAL(MCHSTP),        &
     &                 ESD(MCHSTP)
!
      COMMON /FPINF / PIK(0:50,MFPINF), KMINST(MFPINF), KMAXST(MFPINF)
      COMMON /FPINF1/ KREFT(MFPINF), KNIPT(50,MFPINF), PIKVAL(50,MFPINF)
      COMMON /FPINF2/ NTERMS
!
      COMMON /sappcmn/ xpmin, xpmax, ypmin, ypmax
!
      COMMON /sapgcmn/ xpgmin, xpgmax, ypgmin, ypgmax
!
      ier = 0
      OPEN (21,FILE=FILE(1:Lenfil),STATUS='OLD',ERR=998,IOSTAT=Istat)
      CHIOBS = 0.
      NFIT = 0
      xpmin = 1.E20
      xpmax = -1.E20
      ypmin = 1.E20
      ypmax = -1.E20
      NTERMS = 0
      NOBS = 0
      MMOBS = MCHSTP
      ittem = 0
      DO I = 1, MMOBS
        READ (21,*,END=200) XOBS(I), YOBS(I), ESD(I), KTEM
        KREFT(I) = KTEM
        NOBS = NOBS + 1
!..        ESD(I)=1./SQRT(WT(I))
        WT(I) = 1.0/ESD(I)**2
        YCAL(I) = 0.0
!..        KTEM=KMAXST(I)-KMINST(I)
        IF (KTEM.GT.0) THEN
          NTERMS = NTERMS + KTEM
          READ (21,*,ERR=998) (KNIPT(K,I),PIKVAL(K,I),K=1,KTEM)
          NFIT = NFIT + 1
          IFIT(NFIT) = I
          CHIOBS = CHIOBS + WT(I)*YOBS(I)**2
        ENDIF
        xpmin = MIN(xpmin,xobs(i))
        xpmax = MAX(xpmax,xobs(i))
        ypmin = MIN(ypmin,yobs(i))
        ypmax = MAX(ypmax,yobs(i))
      ENDDO
  200 xpgmin = xpmin
      xpgmax = xpmax
      ypgmin = ypmin
      ypgmax = ypmax
      CLOSE (21)
      GOTO 999
  998 ier = 1
      CLOSE (21,IOSTAT=ISTAT)
  999 RETURN
!
      END SUBROUTINE GETPIK
