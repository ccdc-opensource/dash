! Subroutines concerned with saving the project file
!
!*****************************************************************************
!
      SUBROUTINE ProjectSave
!
! This subroutine saves the project file.
!
      USE VARIABLES
      USE ZMVAR
      USE PRJVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      INTEGER ifrg
      CHARACTER*MaxPathLength :: tFileName
      INTEGER I, J, tInteger
      REAL    tReal
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      hPrjFile = 10
      tFileName = 'Example.dash'
      OPEN(UNIT=hPrjFile,FILE=tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      iPrjRecNr = 1
! Store Wizard Window

! Store radiation source
      CALL FileWriteInteger(hPrjFile,iPrjRecNr,JRadOption)
! Store Wavelength
      CALL FileWriteReal(hPrjFile,iPrjRecNr,ALambda)
! We store the original pattern + all information to get the processed pattern:
! - Truncation limits
! - Background subtraction parameters
! - LBIN
      CALL FileWriteInteger(hPrjFile,iPrjRecNr,BackupNOBS)
      IF (BackupNOBS .GT. 0) THEN
! This is where we must decide if we want to store just the pattern, or also the .pik file
!            WRITE (IPK,*) ARGI, OBS - YBACK, DOBS, NTEM
!        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM
        DO I = 1, BackupNOBS
          CALL FileWriteReal(hPrjFile,iPrjRecNr,BackupXOBS(I))
          CALL FileWriteReal(hPrjFile,iPrjRecNr,BackupYOBS(I))
          CALL FileWriteReal(hPrjFile,iPrjRecNr,BackupEOBS(I))
        ENDDO
! Store start / end
        CALL WDialogSelect(IDD_PW_Page5)
        IF (WDialogGetCheckBoxLogical(IDF_TruncateStartYN)) THEN
          CALL WDialogGetReal(IDF_Min2Theta,tReal)
        ELSE
! If the user doesn't want to truncate the data, just restore the old values
          tReal = 0.0
        ENDIF
        CALL FileWriteReal(hPrjFile,iPrjRecNr,tReal)
        IF (WDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
          CALL WDialogGetReal(IDF_Max2Theta,tReal)
        ELSE
! If the user doesn't want to truncate the data, just restore the old values
          tReal = 90.0
        ENDIF
        CALL FileWriteReal(hPrjFile,iPrjRecNr,tReal)
! Store the parameters for the background algorithm
        CALL WDialogSelect(IDD_PW_Page6)
        CALL WDialogGetInteger(IDF_NumOfIterations,tInteger)
        CALL FileWriteInteger(hPrjFile,iPrjRecNr,tInteger)
        CALL WDialogGetInteger(IDF_WindowWidth,tInteger)
        CALL FileWriteInteger(hPrjFile,iPrjRecNr,tInteger)
        CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_UseMCYN))
! Store LBIN
        CALL FileWriteInteger(hPrjFile,iPrjRecNr,LBIN)
      ENDIF

! Store Crystal System
      CALL FileWriteInteger(hPrjFile,iPrjRecNr,LatBrav)
! Store unit cell
      DO I = 1, 6
        CALL FileWriteReal(hPrjFile,iPrjRecNr,CellPar(I))
      ENDDO
! Store zero-point
      CALL FileWriteReal(hPrjFile,iPrjRecNr,ZeroPoint)
! Store space group
      CALL FileWriteInteger(hPrjFile,iPrjRecNr,NumberSGTable)
! Store Pawley refinement related stuff
! Store the peak fit ranges
      CALL  PrjReadWritePeakFitRanges
! We _must_ read the Peak Fit Ranges after the data needed to generate the tickmarks (unit cell,
! zero point, wavelength, space group, powder pattern) because it needs the tick marks
! to assign a relection to each peak position.

! Store the Z-matrices
      CALL FileWriteInteger(hPrjFile,iPrjRecNr,nfrag)
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,zmNumberOfCopies(iFrg))
          CALL FileWriteString(hPrjFile,iPrjRecNr,frag_file(iFrg))
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,icomflg(iFrg))
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,natoms(iFrg))
          DO J = 1, natoms(iFrg)
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,ioptb(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,iopta(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,ioptt(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,iz1(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,iz2(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,iz3(J,iFrg))
            CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(blen(J,iFrg)))
            CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(alph(J,iFrg)))
            CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(bet(J,iFrg)))
            CALL FileWriteString(hPrjFile,iPrjRecNr,asym(J,iFrg))
            CALL FileWriteString(hPrjFile,iPrjRecNr,OriginalLabel(J,iFrg))
            CALL FileWriteReal(hPrjFile,iPrjRecNr,tiso(J,iFrg))
            CALL FileWriteReal(hPrjFile,iPrjRecNr,occ(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,izmoid(J,iFrg))
            CALL FileWriteInteger(hPrjFile,iPrjRecNr,izmbid(J,iFrg))
          ENDDO
        ENDIF
      ENDDO
! Save solutions
! Save number of solutions
      CALL FileWriteInteger(hPrjFile,iPrjRecNr,SA_Run_Number)
      IF (SA_Run_Number .NE. 0) THEN




      ENDIF



      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing project file.')
      CALL PopActiveWindowID

      END SUBROUTINE ProjectSave
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWritePeakFitRanges
!
! Read or writes information on peak fit ranges to / from binary project file.
!
      USE PRJVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkFnVarVal,                   PkFnVarEsd,                   &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      INTEGER iPFR, iPeak, iPkDes, iPoint, RW

! Read or Write?
      RW = iPrjReadOrWrite
! If no Peak Fit Ranges, write 0 and exit   
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumPeakFitRange)
      IF (NumPeakFitRange .NE. 0) THEN
        DO iPFR = 1, NumPeakFitRange
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPF_Range(1,iPFR))
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPF_Range(2,iPFR))
          CALL FileRWLogical(hPrjFile,iPrjRecNr,RW,RangeFitYN(iPFR))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,IPF_Lo(iPFR))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,IPF_Hi(iPFR))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumInPFR(iPFR))
          DO iPeak = 1, NumInPFR(iPFR)
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPF_Pos(iPeak,iPFR))
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,YPF_Pos(iPeak,iPFR))
          ENDDO
          IF (RangeFitYN(iPFR)) THEN
            DO iPkDes = 1, MPkDes
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkFnVal(iPkDes,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkFnEsd(iPkDes,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkFnCal(iPkDes,iPFR))
            ENDDO
            DO iPeak = 1, NumInPFR(iPFR)
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkAreaVal(iPeak,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkAreaEsd(iPeak,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkPosVal(iPeak,iPFR))
              CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkPosEsd(iPeak,iPFR))
            ENDDO
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,PkPosAv(iPFR))
          ENDIF
! Calculate IPF_Range (only necessary on read)
          IPF_Range(iPFR) = 1 + IPF_Hi(iPFR) - IPF_Lo(iPFR)
        ENDDO
! Calculate IPF_RPt
        IPF_RPt(1) = 0
        DO iPFR = 1, NumPeakFitRange
          IPF_RPt(iPFR+1) = IPF_RPt(iPFR) + IPF_Range(iPFR)
        ENDDO
        DO iPoint = 1, IPF_RPt(NumPeakFitRange+1)
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,XPeakFit(iPoint))
          CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,YPeakFit(iPoint))
        ENDDO
      ENDIF
      IF (RW .EQ. cRead) THEN
! Update 'View'|'Peak Positions'...
        CALL Upload_Positions ! Calculates COMMON /ALLPEAKS/
!... and 'View'|'Peak Widths' tabs
        CALL Upload_Widths
      ENDIF

      END SUBROUTINE PrjReadWritePeakFitRanges
!
!*****************************************************************************
!
