! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      SUBROUTINE VALCHIPRO(chivalpro)
!
! Must be called after VALCHI, because it needs the BICALC to have been set up.
!
      USE REFVAR
      
      IMPLICIT NONE

      REAL, INTENT (  OUT) :: chivalpro

      INCLUDE 'params.inc'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL            CummChiSqd
      COMMON /CMN007/ CummChiSqd(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(MaxKTem,MOBS), PIKVAL(MaxKTem,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      REAL    SUM1, SUM2, YCALC, RESCL, CVP, LastValue
      INTEGER II, I, K, KK
      REAL    tScale

! VALCHIPRO, which calculates the profile chi-squared, is always
! called after VALCHI. VALCHI already fills BICALC.
      SUM1 = 0.0
      SUM2 = 0.0
      CummChiSqd = -1.0
      DO II = 1, NFITA
        I = IFITA(II)
        YCALC = 0.0
        DO K = 1, KREFT(I)
          KK = KNIPT(K,I)
          YCALC = YCALC + BICALC(KK) * PIKVAL(K,I)
        ENDDO
        YCBIN(I) = YCALC
        SUM1 = SUM1 + YCALC
        SUM2 = SUM2 + YOBIN(I)
      ENDDO
      RESCL = SUM2 / SUM1
      CVP = 0.0
      DO II = 1, NFITA
        I = IFITA(II)
        YCBIN(I) = RESCL * YCBIN(I)
        CVP = CVP + WTSA(I) * (YOBIN(I) - YCBIN(I))**2
        CummChiSqd(I) = CVP
      ENDDO
      CHIVALPRO = CVP/FLOAT(NFITA-2)
      tScale = ypmax / CVP
      LastValue = 0.0
      DO i = 1, NBIN
        IF (CummChiSqd(i) .LT. 0.0) THEN
          CummChiSqd(i) = LastValue
        ELSE
! Rescale cumulative profile chi-squared
          CummChiSqd(i) = CummChiSqd(i) * tScale
          LastValue = CummChiSqd(i)
        ENDIF
      ENDDO

      END SUBROUTINE VALCHIPRO
!
!*****************************************************************************
!
