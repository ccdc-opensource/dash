    Subroutine Upload_Crystal_Symmetry()
!
    Use Winteracter
    Use druid_header
!
!
!C>> JCC The declarations are now in a include file since they are used in > 1 place
	include 'Lattice.inc'
!
!
!C>> JCC No need for this anymore 50	Listbrav=Latbrav+1

!	write(76,*) ' LatBrav (in Upload) is ',LatBrav

!C>>  JCC Was    Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
!Now
      Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
      Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
      NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
      Do ISG=1,NumBrSG
         JSG=LPosSG(IOption)+ISG-1
         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
      End Do
      ISPosSG=1+IPosSG-LposSG(IOption)
      Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
!
    call Upload_Lattice_Only()
!

!
    EndSubroutine Upload_Crystal_Symmetry
!
    SUBROUTINE Upload_Lattice_Only()
!
    Use Winteracter
    Use druid_header
!
      REAL :: CELLPAR,ZEROPOINT,ALAMBDA
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
      include 'statlog.inc'
! Calculate the unit cell volume - if sensible upload lattice constants
	d2r=atan(1.)/45.
     a=cellpar(1)
	b=cellpar(2)
	c=cellpar(3)
     calp=cos(d2r*cellpar(4))
     cbet=cos(d2r*cellpar(5))
     cgam=cos(d2r*cellpar(6))
	arg=1.+2.*calp*cbet*cgam-(calp**2+cbet**2+cgam**2)
    VCELL=a*b*c*sqrt(max(0.,arg))
	cellok=VCELL.gt.10.
    If (cellok) Call Upload_Cell_Constants
   END SUBROUTINE Upload_Lattice_Only
!
!
!
     subroutine Upload_Data_Properties()
!
      USE WINTERACTER
      USE druid_header
!
      IMPLICIT NONE
!
      LOGICAL NoData
      INTEGER IPTYPE,JPTYPE
      COMMON /PLTYPE/ IPTYPE
      INTEGER IDCurrent_Cursor_Mode
      COMMON /CURSOR_MODE/ IDCurrent_Cursor_Mode
!
      REAL :: CELLPAR,ZEROPOINT,ALAMBDA
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
!
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
      include 'statlog.inc'
!
!	write(76,*) ' In Upload Data Properties  ',IRadOption,INWOption
	   CALL SetSourceDataState(IRadOption)
!          SELECT CASE (IRadOption)
!            CASE(1)
! Lab X-ray
!              CALL WDialogFieldState(IDF_CW_group,Enabled)
!              CALL WDialogFieldState(IDF_radiation_label,Enabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Enabled)
!              CALL WDialogFieldState(IDF_wavelength1,Enabled)
!              CALL WDialogGetRadioButton(IDF_single_wavelength,INWOption)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Enabled)
!>>              If (INWOption.eq.1) then
!>>                CALL WDialogFieldState(IDF_wavelength2,Disabled)
!>>              Else
!>>                CALL WDialogFieldState(IDF_wavelength2,Enabled)
!>>              End If
!              CALL WDialogFieldState(IDF_TOF_group,Disabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
!              CALL WDialogFieldState(IDF_flight_path,Disabled)
!              CALL WDialogFieldState(IDF_2theta_label,Disabled)
!              CALL WDialogFieldState(IDF_2theta0,Disabled)
!              CALL WDialogPutRadioButton(IDF_LabX_Source)
!              If (Alambda.gt.0.1 .and. Alambda.lt.20.0) then
!                CALL WDialogPutReal(IDF_wavelength1,Alambda,'f10.5')
!              End If
!            CASE(0,2,3)
! Synchrotron X-ray & CW neutron (and nothing!)
!              CALL WDialogFieldState(IDF_CW_group,Enabled)
!              CALL WDialogFieldState(IDF_radiation_label,Disabled)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Enabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_wavelength1,Enabled)
!>>              CALL WDialogFieldState(IDF_wavelength2,Disabled)
!              CALL WDialogFieldState(IDF_TOF_group,Disabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
!              CALL WDialogFieldState(IDF_flight_path,Disabled)
!              CALL WDialogFieldState(IDF_2theta_label,Disabled)
!              CALL WDialogFieldState(IDF_2theta0,Disabled)
!              If (IradOption.eq.2) then
!                CALL WDialogPutRadioButton(IDF_SynX_Source)
!              Else
!                CALL WDialogPutRadioButton(IDF_CWN_Source)
!              End If
!              If (Alambda.gt.0.1 .and. Alambda.lt.20.0) then
!                CALL WDialogPutReal(IDF_wavelength1,Alambda,'(f10.5)')
!              End If
!            CASE(4)
! TOF neutron
!              CALL WDialogFieldState(IDF_CW_group,Disabled)
!              CALL WDialogFieldState(IDF_radiation_label,Disabled)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_wavelength1,Disabled)
!>> JCC              CALL WDialogFieldState(IDF_wavelength2,Disabled)
!              CALL WDialogFieldState(IDF_TOF_group,Enabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Enabled)
!              CALL WDialogFieldState(IDF_flight_path,Enabled)
!              CALL WDialogFieldState(IDF_2theta_label,Enabled)
!              CALL WDialogFieldState(IDF_2theta0,Enabled)
!              CALL WDialogPutRadioButton(IDF_TOF_source)
!          END SELECT
!
   end subroutine Upload_Data_Properties
!
!
!
!*****************************************************************************
!
      subroutine UPLOAD_RANGE()
!
      USE WINTERACTER
      USE druid_header
      character*8 chrfmt
!
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
!
      CALL WDialogSelect(IDD_Data_Properties)
      atem1=max(abs(xpmin),abs(xpmax))
!C>> JCC Dont continue if range is silly
	  if (atem1.LE.0.000000001) RETURN
      atem2=0.0001*abs(xpmax-xpmin)

      item1=0.5+alog10(atem1)
      item2=max(0.,0.5-alog10(atem2))
      item=2+item1+item2
      chrfmt(1:2)='(F'
      if (item.ge.10) then
        call IntegerToString(item,chrfmt(3:4),'(I2)')
        inext=5
      else
        call IntegerToString(item,chrfmt(3:3),'(I1)')
        inext=4
      end if
      chrfmt(inext:inext)='.'
      inext=inext+1
      call IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext=inext+1
      chrfmt(inext:inext)=')'
      CALL WDialogPutReal(IDF_xmin,xpmin,chrfmt(1:inext))
      CALL WDialogPutReal(IDF_xmax,xpmax,chrfmt(1:inext))
!
      atem1=max(abs(ypmin),abs(ypmax))
      atem2=0.0001*abs(ypmax-ypmin)
      item1=0.5+alog10(atem1)
      item2=max(0.,0.5-alog10(atem2))
      item=2+item1+item2
      chrfmt(1:2)='(F'
      if (item.ge.10) then
        call IntegerToString(item,chrfmt(3:4),'(I2)')
        inext=5
      else
        call IntegerToString(item,chrfmt(3:3),'(I1)')
        inext=4
      end if
      chrfmt(inext:inext)='.'
      inext=inext+1
      call IntegerToString(item2,chrfmt(inext:inext),'(I1)')
      inext=inext+1
      chrfmt(inext:inext)=')'
      CALL WDialogPutReal(IDF_ymin,ypmin,chrfmt(1:inext))
      CALL WDialogPutReal(IDF_ymax,ypmax,chrfmt(1:inext))
!
      endsubroutine UPLOAD_RANGE


	  integer function PlotErrorBars
	  use winteracter
	  use druid_header

	  integer ICurSel
	  ICurSel = WinfoDialog(CurrentDialog)
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      CALL WDialogGetCheckBox(IDF_ErrorBar_Check,PlotErrorBars) 
	  IF (ICurSel .NE. -1) CALL WDialogSelect(ICurSel)
	  end function PlotErrorBars



	  integer function PlotBackground
	  use winteracter
	  use druid_header

	  integer ICurSel
	  ICurSel = WinfoDialog(CurrentDialog)
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      CALL WDialogGetCheckBox(IDF_background_check,PlotBackground)

	  IF (ICurSel .NE. -1) CALL WDialogSelect(ICurSel)
	  end function PlotBackground