!//////////////////////////////////////////////////////////////////////////
! Routines for reading and writing a 'DSL' file. This file contains
! The additional data that is part of the Winteracter front end: Namely
! A list of the peak positions, the shape/asymmetry parameters, 
! radiation type/wavelength etc.
!
!//////////////////////////////////////////////////////////////////////////
	 SUBROUTINE GETDSL(FileName,LenFn,Ierr)
	 USE Winteracter
	 USE druid_header
	 INCLUDE 'Lattice.inc'
	 CHARACTER*(*) FileName
	 CHARACTER*128 line
	 CHARACTER*3 KeyChar
	 INTEGER LenFn,Idum,nl
	 REAL Temp
	 INTEGER Itemp


     INCLUDE 'PARAMS.INC'

     COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
     PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
     PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
     PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

	 Ierr = 0
!
!     Open the file
	 OPEN (UNIT = 77, &
           FILE=FileName(1:LenFn), &
           STATUS='OLD', &
           ERR=999)
! Loop over all records
     DO WHILE ( .TRUE. )
 10		READ(77,'(a)',END=100,ERR=999) line
		nl=len_trim(line)
		call ILowerCase(line(:nl))
		call INextString(line,keychar)
		Select Case(KeyChar(1:len_trim(keychar)))
			Case ('rad')
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999
				CALL UpdateWavelength(Temp)
				do i = 1, NRad_Types
					if (Abs(Alambda - RadWaveLengths(i)) .LT. 0.0001) then
						! Set the wizard/main  to read as this selection
						Call SetWavelengthToSelection(i + 1)
					endif
				end do

				call INextInteger(line,itemp)
				IF ( InfoError(1) .EQ. 0) THEN
					CALL SetSourceDataState(itemp)
				ELSE
					CALL SetSourceDataState(2)
				END IF

			Case ('sig') ! Sigma
! Sigma 1
				CALL WDialogSelect(IDD_Sigma_info)
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999
				call WDialogPutReal(IDF_Sigma1,Temp,'(f10.4)')
				PkFnVarVal(1,1) = Temp

				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999	
				PkFnVarEsd(1,1) = Temp
! Sigma 2
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				call WDialogPutReal(IDF_Sigma2,Temp,'(f10.4)')
				PkFnVarVal(2,1) = Temp

				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999	
				PkFnVarEsd(2,1) = Temp

			Case ('gam') ! Gamma
! Gamma 1
				CALL WDialogSelect(IDD_Gamma_info)
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				call WDialogPutReal(IDF_Gamma1,Temp,'(f10.4)')
				PkFnVarVal(1,2) = Temp

				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999	
				PkFnVarEsd(1,2) = Temp
! Gamma 2
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				call WDialogPutReal(IDF_Gamma2,Temp,'(f10.4)')
				PkFnVarVal(2,2) = Temp

				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999	
				PkFnVarEsd(2,2) = Temp

			Case ('asy') ! HMSL/HPSL Shape parameters

! HPSL
				CALL WDialogSelect(IDD_HPSL_info)
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				call WDialogPutReal(IDF_HPSL1,Temp,'(f10.4)')
				PkFnVarVal(1,3)=max(0.0001,Temp)

				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999	
				PkFnVarEsd(1,3)=Temp

! HMSL
				CALL WDialogSelect(IDD_HMSL_info)
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				call WDialogPutReal(IDF_HMSL1,Temp,'(f10.4)')
				PkFnVarVal(1,4)=max(0.0001,Temp)

				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999		
				PkFnVarEsd(1,4)=Temp
 
			Case ('zer')
! Zero point
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999
				CALL WDialogSelect(IDD_Peak_Positions)					
				call WDialogPutReal(IDF_zeropt_refine,Temp,'(f10.4)')
				ZeroPoint = Temp

			Case ('sli')
! Pawley SLIM parameter
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				SlimValue = Temp 
				CALL WDialogSelect(IDD_Pawley_Status)
				call WDialogPutReal(IDF_Slim_Parameter,Temp,'(f7.3)')
			Case ('sca')
				I = InfoError(1) ! reset the errors
				call INextReal(line,Temp)
				IF ( InfoError(1) .NE. 0) GOTO 999					
				ScalFac = Temp 	    
	    End Select

				
     END DO		
 100 CONTINUE
     BACKREF = .FALSE.

	 CLOSE(77)
	 RETURN
!C Error if we get here
  999 Ierr = 1
	CLOSE(77,IOSTAT=IDUM)
	END


	 SUBROUTINE WRTDSL(FileName,LenFn,Ierr)
	 USE Winteracter
	 USE druid_header

     INCLUDE 'PARAMS.INC'
	 INCLUDE 'Lattice.inc'

	 CHARACTER*(*) FileName
	 CHARACTER*128 line
	 CHARACTER*20 KeyChar
	 INTEGER Istart,Idum,nl
	 REAL Temp

     COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
     PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
     PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
     PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)
	 COMMON /RadOption/JRadOption

	 OPEN (UNIT = 77, &
           FILE=FileName(1:LenFn), &
           STATUS='UNKNOWN', &
           ERR=999)
	 WRITE(77,*)'! Radiation wavelength and data type'
	 WRITE(77,'(a3,1x,f10.5,I2)') 'rad', Alambda, JRadOption
	 WRITE(77,*)'! Sigma shape parameters: format sigma1 esd sigma2 esd'
	 WRITE(77,100) &
	   'sig',PkFnVarVal(1,1),PkFnVarEsd(1,1),PkFnVarVal(2,1),PkFnVarEsd(2,1)
	 WRITE(77,*)'! Gamma shape parameters: format gamma1 esd gamma2 esd'
	 WRITE(77,100) &
	   'gam',PkFnVarVal(1,2),PkFnVarEsd(1,2),PkFnVarVal(2,2),PkFnVarEsd(2,2)
	 WRITE(77,*)'! Asymmetry parameters: format HPSL esd HMSL esd'
	 WRITE(77,100) 'asy',PkFnVarVal(1,3),PkFnVarEsd(1,3),PkFnVarVal(1,4),PkFnVarEsd(1,4)
	 WRITE(77,*)'! Calculated zero point'
	 WRITE(77,110) 'zer',ZeroPoint
	 WRITE(77,*)'! Pawley-fit SLIM parameter setting'
	 WRITE(77,110) 'sli',SLIMVALUE
	 WRITE(77,*)'! Pawley-fit Scale factor setting'
	 WRITE(77,110) 'sca',SCALFAC
100  FORMAT(a3,1x,4(f10.4,1x))
110  FORMAT(a3,1x,f10.4)
	 CLOSE(77)
	 RETURN
!C Error if we get here
  999 Ierr = 1
	 CLOSE(77,IOSTAT=IDUM)
	 END