!
!*****************************************************************************
!
      SUBROUTINE FindPeaks

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL                PeakFindPos
      INTEGER                                           nPeaksFound
      COMMON / PEAKFIND / PeakFindPos(1:MaxPeaksFound), nPeaksFound

      INTEGER I, J, Window, workNBIN
      REAL workYOBIN(1:MOBS), tempYOBIN(1:MOBS)
      REAL Iavg

      workNBIN = NBIN
!C Copy pattern, divided by ESDs
      DO I = 1, workNBIN
        workYOBIN(I) = YOBIN(I) / EBIN(I)
      ENDDO
!C Smooth the pattern
      Window = 3
      DO I = 1+Window, workNBIN-Window
        Iavg = 0.0
        DO J = -Window, Window
          Iavg = Iavg + workYOBIN(I+J)
        ENDDO
        tempYOBIN(I) = Iavg / (2.0*Window+1.0)
      ENDDO
      workNBIN = workNBIN-2*Window
      DO I = 1, workNBIN
        workYOBIN(I) = tempYOBIN(I+Window)
      ENDDO
!C First derivative
      workNBIN = workNBIN - 1
      DO I = 1, workNBIN
        workYOBIN(I) = workYOBIN(I+1) - workYOBIN(I)
        ! Maybe take the average of the slope on either side of this data point?
      ENDDO
!O!C Second derivative
!O      workNBIN = workNBIN - 1
!O      DO I = 1, workNBIN
!O        workYOBIN(I) = workYOBIN(I+1) - workYOBIN(I)
!O        ! Maybe take the average of the slope on either side of this data point?
!O      ENDDO
!C Smooth the pattern
      Window = 3
      DO I = 1+Window, workNBIN-Window
        Iavg = 0.0
        DO J = -Window, Window
          Iavg = Iavg + workYOBIN(I+J)
        ENDDO
        tempYOBIN(I) = Iavg / (2.0*Window+1.0)
      ENDDO
      workNBIN = workNBIN-2*Window
      DO I = 1, workNBIN
        workYOBIN(I) = tempYOBIN(I+Window)
      ENDDO
!C Now find all points lower than -0.5 that on both sides have a point that is higher
      nPeaksFound = 0
      DO I = 2, workNBIN-1
        IF (workYOBIN(I) .LT. -0.5) THEN
          IF ((workYOBIN(I-1) .GT. workYOBIN(I)) .AND.  &
              (workYOBIN(I+1) .GT. workYOBIN(I))) THEN
            IF (nPeaksFound .EQ. 50) THEN
              CALL DebugErrorMessage("More than 50 peaks found")
              GOTO 10
            ELSE
              nPeaksFound = nPeaksFound + 1
              PeakFindPos(nPeaksFound) = XBIN(I+Window)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
   10 CALL Profile_Plot

      END SUBROUTINE FindPeaks
!
!*****************************************************************************
!
