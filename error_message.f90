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
      SUBROUTINE CHKMAXREF
!
! Checks if the maximum number of reflections has been exceeded
!
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'Reflns.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ,       KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ARGK, PKCNSP
      INTEGER                              KPCNSP
      REAL                                                DTDPCN,    DTDWL
      INTEGER         NPKCSP
      REAL                         ARGMIN,    ARGMAX,    ARGSTP,    PCON
      COMMON /PRPKCN/ ARGK, PKCNSP(6,9,5), KPCNSP(6,9,5), DTDPCN(6), DTDWL, &
                      NPKCSP(9,5), ARGMIN(5), ARGMAX(5), ARGSTP(5), PCON

      LOGICAL routine_called
      SAVE    routine_called
      DATA    routine_called / .FALSE. /

      CHARACTER*20 tStr
      CHARACTER*20, EXTERNAL :: Integer2String
      INTEGER len

      NumOfRef = maxk
      IF (NumOfRef .GT. MaxNumRef) THEN
        NumOfRef = MaxNumRef
        IF (.NOT. routine_called) THEN
          tStr = Integer2String(MaxNumRef)
          CALL StrClean(tStr, len)
          CALL InfoMessage('DASH has a maximum limit of '//tStr(1:len)//' reflections.'//CHAR(13)//&
                           'Only the '//tStr(1:len)//' lowest angle reflections will be indexed and used.')
          routine_called = .TRUE.
        ENDIF
        know = NumOfRef
! Calculate peak centre of KNOW in ARGK, and its derivatives
        CALL PCCN01(2)
! argk now contains the peak position of the last reflection
        NPTS = 1
        DO WHILE ((XBIN(NPTS) .LT. argk) .AND. (NPTS .LT. MOBS))
          CALL INC(NPTS) ! NPTS is the number of points used for Pawley refinement.
        ENDDO
        argmax(1) = argk
        maxk = NumOfRef
        NBIN = NPTS
        DataSetChange = DataSetChange + 1
        CALL GetProfileLimits
        CALL Get_IPMaxMin 
      ENDIF

      END SUBROUTINE CHKMAXREF
!
!*****************************************************************************
!
