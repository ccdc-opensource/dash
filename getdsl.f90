!//////////////////////////////////////////////////////////////////////////
! Routines for reading and writing a 'DSL' file. This file contains
! The additional data that is part of the Winteracter front end: Namely
! A list of the peak positions, the shape/asymmetry parameters, 
! radiation type/wavelength etc.
!
!//////////////////////////////////////////////////////////////////////////
      SUBROUTINE GETDSL(FileName,LenFn,Ierr)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      CHARACTER*(*), INTENT (IN   ) :: FileName
      INTEGER,       INTENT (IN   ) :: LenFn
      INTEGER,       INTENT (  OUT) :: Ierr

      CHARACTER*128 line
      CHARACTER*3   KeyChar
      INTEGER       Idum, nl
      REAL          Temp
      INTEGER       Itemp

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
      INTEGER I

      Ierr = 0
!     Open the file
      OPEN (UNIT = 77, FILE=FileName(1:LenFn), STATUS='OLD', ERR=999)
! Loop over all records
      DO WHILE ( .TRUE. )
 10     READ(77,'(a)',END=100,ERR=999) line
        nl = LEN_TRIM(line)
        CALL ILowerCase(line(:nl))
        CALL INextString(line,keychar)
        SELECT CASE(KeyChar(1:LEN_TRIM(keychar)))
          CASE ('rad')
            I = InfoError(1) ! reset the errors
! Read the wavelength
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL INextInteger(line,itemp)
            IF (InfoError(1) .EQ. 0) THEN
              JRadOption = itemp
            ELSE
! default = X-ray lab data
              JRadOption = 1
            END IF
            CALL Upload_Source
! Now we know all there is to know about the wavelength and source: update it
            CALL Set_Wavelength(Temp)
          CASE ('sig') ! Sigma
! Sigma 1
            CALL WDialogSelect(IDD_Sigma_info)
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL WDialogPutReal(IDF_Sigma1,Temp,'(f10.4)')
            PkFnVarVal(1,1) = Temp
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999  
            PkFnVarEsd(1,1) = Temp
! Sigma 2
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            CALL WDialogPutReal(IDF_Sigma2,Temp,'(f10.4)')
            PkFnVarVal(2,1) = Temp
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999  
            PkFnVarEsd(2,1) = Temp
          CASE ('gam') ! Gamma
! Gamma 1
            CALL WDialogSelect(IDD_Gamma_info)
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            CALL WDialogPutReal(IDF_Gamma1,Temp,'(f10.4)')
            PkFnVarVal(1,2) = Temp
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999  
            PkFnVarEsd(1,2) = Temp
! Gamma 2
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            CALL WDialogPutReal(IDF_Gamma2,Temp,'(f10.4)')
            PkFnVarVal(2,2) = Temp
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999  
            PkFnVarEsd(2,2) = Temp
          CASE ('asy') ! HMSL/HPSL Shape parameters
! HPSL
            CALL WDialogSelect(IDD_HPSL_info)
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            CALL WDialogPutReal(IDF_HPSL1,Temp,'(f10.4)')
            PkFnVarVal(1,3) = MAX(0.0001,Temp)
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999  
            PkFnVarEsd(1,3) = Temp
! HMSL
            CALL WDialogSelect(IDD_HMSL_info)
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            CALL WDialogPutReal(IDF_HMSL1,Temp,'(f10.4)')
            PkFnVarVal(1,4) = MAX(0.0001,Temp)
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999        
            PkFnVarEsd(1,4)=Temp
          CASE ('zer')
! Zero point
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
            ZeroPoint = Temp
            CALL Upload_ZeroPoint
          CASE ('sli')
! Pawley SLIM parameter
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            SlimValue = Temp 
            CALL WDialogSelect(IDD_Pawley_Status)
            CALL WDialogPutReal(IDF_Slim_Parameter,Temp,'(f7.3)')
          CASE ('sca')
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999                          
            ScalFac = Temp        
        END SELECT
      END DO       
 100  CONTINUE
      BACKREF = .FALSE.
      CLOSE(77)
      RETURN
!C Error if we get here
  999 Ierr = 1
      CLOSE(77,IOSTAT=IDUM)

      END SUBROUTINE GETDSL
!
!*****************************************************************************
!
      SUBROUTINE WRTDSL(FileName,LenFn)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'GLBVAR.INC'

      CHARACTER*(*) FileName
      INTEGER       LenFn, Idum

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      OPEN (UNIT = 77,FILE=FileName(1:LenFn),STATUS='UNKNOWN',ERR=999)
      WRITE(77,*)'! Radiation wavelength and data type'
      WRITE(77,'(A3,1X,F10.5,I2)') 'rad', ALambda, JRadOption
      WRITE(77,*)'! Sigma shape parameters: format sigma1 esd sigma2 esd'
      WRITE(77,100) 'sig',PkFnVarVal(1,1),PkFnVarEsd(1,1),PkFnVarVal(2,1),PkFnVarEsd(2,1)
      WRITE(77,*)'! Gamma shape parameters: format gamma1 esd gamma2 esd'
      WRITE(77,100) 'gam',PkFnVarVal(1,2),PkFnVarEsd(1,2),PkFnVarVal(2,2),PkFnVarEsd(2,2)
      WRITE(77,*)'! Asymmetry parameters: format HPSL esd HMSL esd'
      WRITE(77,100) 'asy',PkFnVarVal(1,3),PkFnVarEsd(1,3),PkFnVarVal(1,4),PkFnVarEsd(1,4)
      WRITE(77,*)'! Calculated zero point'
      WRITE(77,110) 'zer',ZeroPoint
      WRITE(77,*)'! Pawley-fit SLIM parameter setting'
      WRITE(77,110) 'sli',SLIMVALUE
      WRITE(77,*)'! Pawley-fit Scale factor setting'
      WRITE(77,110) 'sca',SCALFAC
  100 FORMAT(A3,1X,4(F10.4,1X))
  110 FORMAT(A3,1X,F10.4)
      CLOSE(77)
      RETURN
!C Error if we get here
  999 CLOSE(77,IOSTAT=IDUM)

      END SUBROUTINE WRTDSL
!
!*****************************************************************************
!
      SUBROUTINE SDIFileBrowse
!
! This routine lets the user browse a directory for an SDI file.
! If a valid file has been selected, it will be opened automatically.
! Effectively, this routine is just a wrap around a file_open routine
! such that it lets the user visually select a file first.
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER(LEN=60) FILTER
      INTEGER           IFLAGS, IFTYPE 
      CHARACTER(LEN=MaxPathLength) tFileName

      IFLAGS = LoadDialog + DirChange + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|'//&
               'DASH Pawley files (*.sdi)|*.sdi|'
      tFileName = ' '
! IFTYPE specifies which of the file types in the list is the default
      IFTYPE = 2
      CALL WSelectFile(FILTER,IFLAGS,tFileName,'Open DASH Pawley file',IFTYPE)
! Did the user press cancel?
      IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
! Note that up to this point, none of the global variables had changed. Baling out was no problem.
! Try to open the file. This can be removed, of course, and relocated to places in the code where
! the current subroutine is called.
! Actually, that is how it works in practice under windows (try 'Start' -> 'Run...' -> 'Browse...'
! it will not actually open the file, just select it).
      CALL SDIFileOpen(tFileName)

      END SUBROUTINE SDIFileBrowse
!
!*****************************************************************************
!
      SUBROUTINE SDIFileOpen(TheFileName)
!
! This routine tries to open an SDI file.
!
! INPUT   : TheFileName = the file name
!
      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INCLUDE 'GLBVAR.INC'

      LOGICAL FExists
      INTEGER KLEN

      KLEN = LEN_TRIM(TheFileName)
      IF (KLEN .EQ. 0) RETURN
      INQUIRE(FILE=TheFileName(1:KLEN),EXIST=FExists)
      IF (.NOT. FExists) THEN
        CALL ErrorMessage("The file "//TheFileName(1:KLEN)//" does not exist!")
        RETURN
      ENDIF
! This is the point of no return: the selected file will be new file, valid data or not
! Change global variable FNAME
      FNAME = TheFileName
      CALL SDIFileLoad(FNAME(1:KLEN)) 
      IF (NoData) THEN
        CALL ErrorMessage("Could not read the project file "//FNAME(1:KLEN)//&
                          CHAR(13)//"successfully.")
        RETURN
      ENDIF
      STATBARSTR(1) = FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
!  update the file name of the project in the SA pop up
      CALL SetSAFileName(TheFileName(1:LEN_TRIM(TheFileName)))
      
      END SUBROUTINE SDIFileOpen
!
!*****************************************************************************
!
      SUBROUTINE SDIFileLoad(SDIFile)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) ::  SDIFile

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      CHARACTER(LEN = MaxPathLength) :: line

      INTEGER nl
      CHARACTER*12 KeyChar

      INTEGER i, KLEN
      INTEGER ihcver,iticer,ipiker,iloger,idsler, isst, ised
      INTEGER GetCrystalSystem ! Function
      INTEGER GETTIC ! Function
      INTEGER tFileHandle

! JCC Set to success in all cases
      ihcver = 0
      iloger = 0
      iticer = 1
      ipiker = 0
      idsler = 0
      IF (LEN_TRIM(SDIFile) .GT. 80) THEN
        CALL DebugErrorMessage('LEN_TRIM(SDIFile) too long in SDIFileLoad')
      ENDIF
! Now open all the appropriate PIK, TIC and HCV files
      tFileHandle = 10
      OPEN(tFileHandle,FILE=SDIFile(1:LEN_TRIM(SDIFile)),STATUS='old',ERR=999)
      CALL sa_SetOutputFiles(SDIFile)
      TicExists = .FALSE.
      HcvExists = .FALSE.
      PikExists = .FALSE.
      RawExists = .FALSE.
      DslExists = .FALSE.
 10   line = ' '
      READ(tFileHandle,'(A)',END=100) line
      nl = LEN_TRIM(line)
      CALL ILowerCase(line(:nl))
      CALL INextString(line,keychar)
      SELECT CASE (KeyChar(1:3))
        CASE ('tic')
          CALL ILocateString(line,isst,ised)
          DashTicFile(1:80) = line(isst:isst+79)
          TicExists = .TRUE.
        CASE ('hcv')
          CALL ILocateString(line,isst,ised)
          DashHcvFile(1:80) = line(isst:isst+79)
          HcvExists = .TRUE.
        CASE ('pik')
          CALL ILocateString(line,isst,ised)
          DashPikFile(1:80) = line(isst:isst+79)
          PikExists = .TRUE.
        CASE ('raw')
          CALL ILocateString(line,isst,ised)
          DashRawFile(1:80) = line(isst:isst+79)
          RawExists = .TRUE.      
        CASE ('dsl')
          CALL ILocateString(line,isst,ised)
          DashDslFile(1:80) = line(isst:isst+79)
          DslExists = .TRUE.
        CASE ('cel')
          DO I = 1, 6
            CALL INextReal(line,CellPar(i))
          ENDDO
          CALL Upload_Cell_Constants()
        CASE ('spa')
          CALL INextInteger(line,NumberSGTable)
! Set the crystal system
          LatBrav = GetCrystalSystem(NumberSGTable)
          CALL Upload_CrystalSystem
          CALL FillSymmetry()
        CASE ('paw')
          CALL INextReal(line,PAWLEYCHISQ)
      END SELECT
      GOTO 10 
 100  CONTINUE
      CLOSE(tFileHandle)
      IF (DslExists) THEN
        CALL GETDSL(DashDslFile,LEN_TRIM(DashDslFile),idsler)
        DslExists = (idsler .EQ. 0)
      ENDIF
      klen = LEN_TRIM(DashTicFile)
      IF (TicExists) THEN
        CALL GET_LOGREF(DashTicFile,klen,iloger)
! JvdS I think that GET_LOGREF already loaded the DashTicFile
        iticer = GETTIC(klen,DashTicFile)
        IF (iticer .EQ. 0) TicExists = .FALSE.
      ENDIF
      IF (HcvExists) THEN
        CALL GETHCV(DashHcvFile,LEN_TRIM(DashHcvFile),ihcver)
        HcvExists = (ihcver .EQ. 0)
      ENDIF
      IF (PikExists) THEN
        CALL GETPIK(DashPikFile,LEN_TRIM(DashPikFile),ipiker)
! Now:
! None of the arrays in PROFOBS has been filled: the arrays in CHISTOP were filled instead.
! YCAL has been reset to 0.0
        PikExists = (ipiker .EQ. 0)
      ENDIF
      CALL Init_PeakFitRanges
! JCC Last thing - reload the profile. Previously this was done in Load_TIC_File but 
! I moved it, since i wanted to check that all the data read in ok before calling it
      IF (PikExists) THEN
! JCC before, this just didnt plot anything, even though in theory we should be able
! to observe the full profile. Firstly have to synchronize the common blocks though
        CALL Synchronize_Data()
        NoData = .FALSE.
      ENDIF
      IPTYPE = 1
      CALL Profile_Plot
! enable the buttons,
      IF (.NOT. NoData) THEN
        IF (idsler .EQ. 0) THEN
          CALL SetModeMenuState(1,1,1)
        ELSE
          CALL SetModeMenuState(1,-1,1)
        ENDIF
      ENDIF

 999  END SUBROUTINE SDIFileLoad
!
!*****************************************************************************
!
      INTEGER FUNCTION GETTIC(FLEN,TheFileName)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (IN   ) :: FLEN

      INCLUDE 'PARAMS.INC'

      INTEGER NTIC
      INTEGER IH
      REAL    ARGK
      REAL    DSTAR
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      INTEGER I, II

! JCC - set return status
      GETTIC = 1
! JCC - add in an error trap for bad file opening
      OPEN(11,FILE=TheFileName(1:FLEN),STATUS='OLD',ERR=999)
      I=1
 10   READ(11,*,ERR=100,END=100) (IH(II,I),II=1,3),ARGK(I),DSTAR(I)
      I=I+1
      IF (I .GT. MTIC) GOTO 100
      GOTO 10
 100  NTIC=I-1
      CLOSE(11)
      RETURN
 999  GETTIC = 0

      END FUNCTION GETTIC
!
!*****************************************************************************
!
