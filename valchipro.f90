!
!*****************************************************************************
!
      SUBROUTINE VALCHIPRO(chivalpro)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      COMMON /FCSPEC/ NLGREF,IREFH(3,MFCSPE),LOGREF(8,MFCSPE)
      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)
      COMMON /SAREFLNS/AIOBS(MSAREF),AICALC(MSAREF)
      COMMON /CHISTOP/ NOBS,NFIT,IFIT(MCHSTP),CHIOBS,&
        WT(MCHSTP),XOBS(MCHSTP),YOBS(MCHSTP),YCAL(MCHSTP),ESD(MCHSTP)
      COMMON /chibest/ ycalbest(MCHSTP)
      COMMON /FPINF1/ KREFT(MFPINF),KNIPT(50,MFPINF),PIKVAL(50,MFPINF)

! JvdS VALCHIPRO, which calculates the profile chi-squared, is always
! called after VALCHI. VALCHI already fills AICALC in COMMON /SAREFLNS/ (the COMMON block needs
! to be specified here: they are kept in COMMON /REFLNS/ in Reflns.inc as well)). 
  !    INCLUDE 'AllFFCalc.inc'
      SUM1 = 0.0
      SUM2 = 0.0
      DO II = 1, NFIT
        I = IFIT(II)
        YCALC = 0.0
        DO K = 1, KREFT(I)
          KK = KNIPT(K,I)
          YCALC = YCALC+AICALC(KK)*PIKVAL(K,I)
        ENDDO
!!        DO K=KMINST(I),KMAXST(I)
!!          KD=K-KMINST(I)
!!          YCALC=YCALC+AICALC(K)*PIK(KD,I)
!!        END DO
!        WY=WT(I)*YCALC
!        SUM1=SUM1+WY*YCALC
!        SUM2=SUM2+WY*YOBS(I)
        YCAL(I)=YCALC
        SUM1=SUM1+YCALC
        SUM2=SUM2+YOBS(I)
      ENDDO
      RESCL = SUM2 / SUM1
      CVP = 0.0
      DO II = 1, NFIT
        I = IFIT(II)
        CVP = CVP+WT(I)*(YOBS(I)-RESCL*YCAL(I))**2
        ycalbest(I) = rescl*ycal(I)
      END DO
!      CHIVALPRO=CHIOBS!-SUM2*SUM2/SUM1
      CHIVALPRO = CVP/FLOAT(NFIT-2)

      END SUBROUTINE VALCHIPRO
!
!*****************************************************************************
!
