! Subroutines concerned with saving the project file
!
!*****************************************************************************
!
      SUBROUTINE PrjReadWrite(ReadOrWrite)
!
! This subroutine saves the project file.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PRJVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: ReadOrWrite

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

      REAL            BestValuesDoF
      COMMON /SOLCOM/ BestValuesDoF(1:mvar,1:MaxRun)

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      CHARACTER*MaxPathLength :: tFileName
      INTEGER I, tInteger, RW
      LOGICAL tLogical
      REAL    tReal
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      iPrjReadOrWrite = ReadOrWrite
      RW = iPrjReadOrWrite
      hPrjFile = 10
      tFileName = 'Example.dash'
      OPEN(UNIT=hPrjFile,FILE=tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      iPrjRecNr = 1
! Read / Write Wizard Window
      IF (RW .EQ. cWrite) THEN
        CALL FileWriteInteger(hPrjFile,iPrjRecNr,CurrentWizardWindow)
      ELSE
        CALL FileReadInteger(hPrjFile,iPrjRecNr,tInteger)
        CALL WizardWindowShow(tInteger)
      ENDIF
! Read / Write radiation source
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,JRadOption)
! Read / Write Wavelength
      CALL FileRWReal(hPrjFile,iPrjRecNr,RW,ALambda)
      IF (RW .EQ. cRead) THEN
        CALL Upload_Source
        CALL Upload_Wavelength
      ENDIF
! We Read / Write the original pattern + all information to get the processed pattern:
! - Truncation limits
! - Background subtraction parameters
! - LBIN
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,BackupNOBS)
      IF (BackupNOBS .EQ. 0) THEN
        NoData = .TRUE.
      ELSE
        NoData = .FALSE.
        DO I = 1, BackupNOBS
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BackupXOBS(I))
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BackupYOBS(I))
          CALL FileRWReal(hPrjFile,iPrjRecNr,RW,BackupEOBS(I))
        ENDDO
! Read / Write LBIN
        CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,LBIN)
! Read / Write start / end
        CALL WDialogSelect(IDD_PW_Page5)
        IF (RW .EQ. cWrite) THEN
          CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_TruncateStartYN))
          CALL WDialogGetReal(IDF_Min2Theta,tReal)
          CALL FileWriteReal(hPrjFile,iPrjRecNr,tReal)
          CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_TruncateEndYN))
          CALL WDialogGetReal(IDF_Max2Theta,tReal)
          CALL FileWriteReal(hPrjFile,iPrjRecNr,tReal)
        ELSE
          CALL FileReadLogical(hPrjFile,iPrjRecNr,tLogical)
          CALL WDialogPutCheckBoxLogical(IDF_TruncateStartYN,tLogical)
          CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
          CALL WDialogPutReal(IDF_Min2Theta,tReal)
          CALL FileReadLogical(hPrjFile,iPrjRecNr,tLogical)
          CALL WDialogPutCheckBoxLogical(IDF_TruncateEndYN,tLogical)
          CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
          CALL WDialogPutReal(IDF_Max2Theta,tReal)
        ENDIF
! Read / Write the parameters for the background algorithm
        CALL WDialogSelect(IDD_PW_Page6)
        IF (RW .EQ. cWrite) THEN
          CALL WDialogGetInteger(IDF_NumOfIterations,tInteger)
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL WDialogGetInteger(IDF_WindowWidth,tInteger)
          CALL FileWriteInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL FileWriteLogical(hPrjFile,iPrjRecNr,WDialogGetCheckBoxLogical(IDF_UseMCYN))
        ELSE
          CALL FileReadInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL WDialogPutInteger(IDF_NumOfIterations,tInteger)
          CALL FileReadInteger(hPrjFile,iPrjRecNr,tInteger)
          CALL WDialogPutInteger(IDF_WindowWidth,tInteger)
          CALL FileReadLogical(hPrjFile,iPrjRecNr,tLogical)
          CALL WDialogPutCheckBoxLogical(IDF_UseMCYN,tLogical)
        ENDIF
        IF (RW .EQ. cRead) THEN
          CALL WizardApplyDiffractionFileInput  ! @@ Error here with LBIN (is reset to 1 ?)
          CALL WizardApplyProfileRange
          CALL WizardApplyBackground
          IPTYPE = 1
        ENDIF
      ENDIF
! Read / Write Crystal System
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,LatBrav)
! Read / Write unit cell
      DO I = 1, 6
        CALL FileRWReal(hPrjFile,iPrjRecNr,RW,CellPar(I))
      ENDDO
! Read / Write zero-point
      CALL FileRWReal(hPrjFile,iPrjRecNr,RW,ZeroPoint)
! Read / Write space group
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,NumberSGTable)
! Calculate tick marks
      IF (RW .EQ. cRead) CALL Generate_TicMarks 
! Read / Write Pawley refinement related stuff
! Read / Write the peak fit ranges
      CALL PrjReadWritePeakFitRanges
! We _must_ read the Peak Fit Ranges after the data needed to generate the tickmarks (unit cell,
! zero point, wavelength, space group, powder pattern) because it needs the tick marks
! to assign a reflection to each peak position.
! Read / Write the .pik file
!            WRITE (IPK,*) ARGI, OBS - YBACK, DOBS, NTEM
!        READ (21,*,END=200,ERR=998) XBIN(I), YOBIN(I), EBIN(I), KTEM



      IF (RW .EQ. cRead) CALL Profile_Plot
! Read / Write the Z-matrices
      CALL PrjReadWriteZmatrices
! Read / Write solutions
! Read / Write number of solutions
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,SA_Run_Number)
      IF (SA_Run_Number .NE. 0) THEN




      ENDIF



      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing project file.')
      CALL PopActiveWindowID

      END SUBROUTINE PrjReadWrite
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
      SUBROUTINE PrjReadWriteZmatrices
!
! Read or writes information on peak fit ranges to / from binary project file.
!
      USE PRJVAR
      USE ZMVAR

      IMPLICIT NONE

      INTEGER iFrg, RW, iAtomNr
      REAL    tReal

! Read or Write?
      RW = iPrjReadOrWrite
      CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,nfrag)
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,zmNumberOfCopies(iFrg))
          CALL FileRWString (hPrjFile,iPrjRecNr,RW,frag_file(iFrg))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,icomflg(iFrg))
          CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,natoms(iFrg))
          DO iAtomNr = 1, natoms(iFrg)
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,ioptb(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iopta(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,ioptt(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iz1(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iz2(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,iz3(iAtomNr,iFrg))
            IF (RW .EQ. cWrite) THEN
              CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(blen(iAtomNr,iFrg)))
              CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(alph(iAtomNr,iFrg)))
              CALL FileWriteReal(hPrjFile,iPrjRecNr,SNGL(bet(iAtomNr,iFrg)))
            ELSE
              CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
              blen(iAtomNr,iFrg) = DBLE(tReal)
              CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
              alph(iAtomNr,iFrg) = DBLE(tReal)
              CALL FileReadReal(hPrjFile,iPrjRecNr,tReal)
              bet(iAtomNr,iFrg) = DBLE(tReal)
            ENDIF
            CALL FileRWString (hPrjFile,iPrjRecNr,RW,asym(iAtomNr,iFrg))
            CALL FileRWString (hPrjFile,iPrjRecNr,RW,OriginalLabel(iAtomNr,iFrg))
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,tiso(iAtomNr,iFrg))
            CALL FileRWReal   (hPrjFile,iPrjRecNr,RW,occ(iAtomNr,iFrg))
            CALL FileRWInteger(hPrjFile,iPrjRecNr,RW,izmoid(iAtomNr,iFrg))
            izmbid(izmoid(iAtomNr,iFrg),iFrg) = iAtomNr ! the back mapping
          ENDDO
        ENDIF
      ENDDO

      END SUBROUTINE PrjReadWriteZmatrices
!
!*****************************************************************************
!
