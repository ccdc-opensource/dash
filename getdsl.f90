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

       INCLUDE 'Lattice.inc'
       INCLUDE 'GLBVAR.INC' ! Contains ALambda

       CHARACTER*(*) FileName
       CHARACTER*128 line
       CHARACTER*3   KeyChar
       INTEGER       LenFn, Idum, nl
       REAL          Temp
       INTEGER       Itemp

      INCLUDE 'PARAMS.INC'

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      Ierr = 0
!     Open the file
      OPEN (UNIT = 77, &
           FILE=FileName(1:LenFn), &
           STATUS='OLD', &
           ERR=999)
! Loop over all records
      DO WHILE ( .TRUE. )
 10     READ(77,'(a)',END=100,ERR=999) line
        nl = LEN_TRIM(line)
        CALL ILowerCase(line(:nl))
        CALL INextString(line,keychar)
        SELECT CASE(KeyChar(1:LEN_TRIM(keychar)))
          CASE ('rad')
            I = InfoError(1) ! reset the errors
            CALL INextReal(line,Temp)
            IF (InfoError(1) .NE. 0) GOTO 999
! Postpone updating the wavelength until we know the type of radiation used.
! If laboratory data, we can try to assign a material to the anode based on the wavelength
 !O           DO I = 1, NRad_Types
 !O             IF (ABS(Alambda - RadWaveLengths(I)) .LT. 0.0001) CALL SetWavelengthToSelection(I + 1)
! Set the wizard/main to read as this selection
 !O           END DO
            CALL INextInteger(line,itemp)
            IF (InfoError(1) .EQ. 0) THEN
              CALL SetSourceDataState(itemp)
            ELSE
! default = X-ray lab data
              CALL SetSourceDataState(1)
            END IF
! Now we know all there is to know about the wavelength and source: update it
            CALL UpdateWavelength(Temp)

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
            CALL WDialogSelect(IDD_Peak_Positions)                            
            CALL WDialogPutReal(IDF_zeropt_refine,Temp,'(f10.4)')
            ZeroPoint = Temp
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
      SUBROUTINE WRTDSL(FileName,LenFn,Ierr)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'GLBVAR.INC' ! Contains ALambda

      CHARACTER*(*) FileName
      INTEGER       LenFn, Idum, nl

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      OPEN (UNIT = 77, &
           FILE=FileName(1:LenFn), &
           STATUS='UNKNOWN', &
           ERR=999)
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
  999 Ierr = 1
      CLOSE(77,IOSTAT=IDUM)

      END SUBROUTINE WRTDSL
!
!*****************************************************************************
!
