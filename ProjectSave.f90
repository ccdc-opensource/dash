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

      INTEGER RecNr, tFileHandle, ifrg
      CHARACTER*MaxPathLength :: tFileName
      INTEGER I, J, tInteger
      REAL    tReal
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      tFileHandle = 10
      tFileName = 'Example.dash'
      OPEN(UNIT=tFileHandle,FILE=tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      RecNr = 1
! Store radiation source
      CALL FileWriteInteger(tFileHandle,RecNr,JRadOption)
! Store Wavelength
      CALL FileWriteReal(tFileHandle,RecNr,ALambda)
! We store the original pattern + all information to get the processed pattern:
! - Truncation limits
! - Background subtraction parameters
! - LBIN
      CALL FileWriteInteger(tFileHandle,RecNr,BackupNOBS)
      IF (BackupNOBS .GT. 0) THEN
! This is where we must decide if we want to store just the pattern, or also the .pik file
        DO I = 1, BackupNOBS
          CALL FileWriteReal(tFileHandle,RecNr,BackupXOBS(I))
          CALL FileWriteReal(tFileHandle,RecNr,BackupYOBS(I))
          CALL FileWriteReal(tFileHandle,RecNr,BackupEOBS(I))
        ENDDO
! Store start / end
        CALL WDialogSelect(IDD_PW_Page5)
        IF (WDialogGetCheckBoxLogical(IDF_TruncateStartYN)) THEN
          CALL WDialogGetReal(IDF_Min2Theta,tReal)
        ELSE
! If the user doesn't want to truncate the data, just restore the old values
          tReal = 0.0
        ENDIF
        CALL FileWriteReal(tFileHandle,RecNr,tReal)
        IF (WDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
          CALL WDialogGetReal(IDF_Max2Theta,tReal)
        ELSE
! If the user doesn't want to truncate the data, just restore the old values
          tReal = 90.0
        ENDIF
        CALL FileWriteReal(tFileHandle,RecNr,tReal)
! Store the parameters for the background algorithm
        CALL WDialogSelect(IDD_PW_Page6)
        CALL WDialogGetInteger(IDF_NumOfIterations,tInteger)
        CALL FileWriteInteger(tFileHandle,RecNr,tInteger)
        CALL WDialogGetInteger(IDF_WindowWidth,tInteger)
        CALL FileWriteInteger(tFileHandle,RecNr,tInteger)
        CALL FileWriteLogical(tFileHandle,RecNr,WDialogGetCheckBoxLogical(IDF_UseMCYN))
! Store LBIN
        CALL FileWriteInteger(tFileHandle,RecNr,LBIN)
      ENDIF
! Store Crystal System
      CALL FileWriteInteger(tFileHandle,RecNr,LatBrav)
! Store unit cell
      DO I = 1, 6
        CALL FileWriteReal(tFileHandle,RecNr,CellPar(I))
      ENDDO
! Store zero-point
      CALL FileWriteReal(tFileHandle,RecNr,ZeroPoint)
! Store space group
      CALL FileWriteInteger(tFileHandle,RecNr,NumberSGTable)
! Store Pawley refinement related stuff


! Store the Z-matrices
      CALL FileWriteInteger(tFileHandle,RecNr,nfrag)
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          CALL FileWriteInteger(tFileHandle,RecNr,zmNumberOfCopies(iFrg))
          CALL FileWriteString(tFileHandle,RecNr,frag_file(iFrg))
          CALL FileWriteInteger(tFileHandle,RecNr,icomflg(iFrg))
          CALL FileWriteInteger(tFileHandle,RecNr,natoms(iFrg))
          DO J = 1, natoms(iFrg)
            CALL FileWriteInteger(tFileHandle,RecNr,ioptb(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,iopta(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,ioptt(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,iz1(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,iz2(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,iz3(J,iFrg))
            CALL FileWriteReal(tFileHandle,RecNr,SNGL(blen(J,iFrg)))
            CALL FileWriteReal(tFileHandle,RecNr,SNGL(alph(J,iFrg)))
            CALL FileWriteReal(tFileHandle,RecNr,SNGL(bet(J,iFrg)))
            CALL FileWriteString(tFileHandle,RecNr,asym(J,iFrg))
            CALL FileWriteString(tFileHandle,RecNr,OriginalLabel(J,iFrg))
            CALL FileWriteReal(tFileHandle,RecNr,tiso(J,iFrg))
            CALL FileWriteReal(tFileHandle,RecNr,occ(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,izmoid(J,iFrg))
            CALL FileWriteInteger(tFileHandle,RecNr,izmbid(J,iFrg))
          ENDDO
        ENDIF
      ENDDO
! Save solutions
! Save number of solutions
      CALL FileWriteInteger(tFileHandle,RecNr,SA_Run_Number)
      IF (SA_Run_Number .NE. 0) THEN




      ENDIF



      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error while accessing project file.')
      CALL PopActiveWindowID

      END SUBROUTINE ProjectSave
!
!*****************************************************************************
!


