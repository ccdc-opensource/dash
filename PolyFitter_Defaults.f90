!
      SUBROUTINE PP_DEFAULTS
!
      USE WINTERACTER
      USE druid_header
!
      TYPE(WIN_STYLE)   :: MAIN_WINDOW
!

      INCLUDE 'PARAMS.INC'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
       itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PLTINI/ XPG1,XPG2,YPG1,YPG2

      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      REAL CHAR_SIZE,MARKER_SIZE
      LOGICAL ERROR_BAR
      COMMON /PROFDEF/ERROR_BAR,CHAR_SIZE,MARKER_SIZE
      INTEGER IDCurrent_Cursor_Mode
      COMMON /CURSOR_MODE/ IDCurrent_Cursor_Mode 
      INCLUDE 'statlog.inc'

	  INCLUDE 'lattice.inc'
!      REAL :: CELLPAR,ZEROPOINT,ALAMBDA
!      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA 


      INCLUDE 'Poly_Colours.inc'
      INCLUDE 'DialogPosCmnf90.inc'
!
      IDCurrent_Cursor_Mode=ID_Default_Mode
!      call WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
!      call WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOff)
!
	  call SetModeMenuState(-1,-1,-1)
      CELLOK=.FALSE.
      WVLNOK=.FALSE.
      SPGPOK=.FALSE.
      DataSetChange=0
      NumInternalDSC=-1
      ZEROPOINT=0.0
!>>JCC Added
	  SLIMVALUE=1.0
	  BACKREF =.TRUE.
!
      IRadOption=2
      INWOption=1
! Default on P21/c
      NumberSGTable=64
!
      IXPos_IDD_Pawley_Status=0.1*XBSWidth
      IYPos_IDD_Pawley_Status=0.06*XBSHeight
      IXPos_IDD_SA_Input=0.1*XBSWidth
      IYPos_IDD_SA_Input=0.06*XBSHeight
      IXPos_IDD_Wizard=0.1*XBSWidth
      IYPos_IDD_Wizard=0.06*XBSHeight
!
      FromPawleyFit=.false. 
!
      NUMOBSTIC=0
      NTIC=0
      LBIN=1
!
      MARKER_SIZE=0.35
      CHAR_SIZE=1.0
      ERROR_BAR=.FALSE.
!
      XPG1=0.12
      XPG2=0.95
      YPG1=0.12
      YPG2=0.93
!
      KolNumPGWindow=220
      KolNumMain=221
      KolNumObs=222
      KolNumCal=223
      KolNumDif=224
      KolNumMTic=225
      KolNumCTic=226
      KolNumPanelVLite=227
      KolNumPanelLite=228
      KolNumPanelDark=229
      KolNumPanelVDark=230
      KolNumPanelOuter=231
      KolNumRectSelect=232
      KolNumLargeCrossHair=233
      KolNumPeakFit=234
      KolNumPeakPos=235
	  KolNumBack=236
!
      KolDefPGWindow=Win_RGB(253,253,248)
      KolDefMain=Win_RGB(20,20,150)
      KolDefObs=Win_RGB(161,0,0)
      KolDefCal=Win_RGB(10,70,10)
      KolDefDif=Win_RGB(200,100,200)
      KolDefMTic=Win_RGB(191,0,0)
      KolDefCTic=Win_RGB(0,131,131)
      KolDefPanelVLite=Win_RGB(245,245,245)
      KolDefPanelLite=Win_RGB(235,235,235)
      KolDefPanelDark=Win_RGB(210,210,210)
      KolDefPanelVDark=Win_RGB(170,170,170)
      KolDefPanelOuter=Win_RGB(190,190,190)
!      KolDefPanelVLite=Win_RGB(250,250,250)
!      KolDefPanelLite=Win_RGB(245,245,245)
!      KolDefPanelDark=Win_RGB(230,230,230)
!      KolDefPanelVDark=Win_RGB(220,220,220)
!      KolDefPanelOuter=Win_RGB(225,225,225)
      KolDefRectSelect=Win_RGB(150,150,5)
      KolDefLargeCrossHair=Win_RGB(150,150,5)
      KolDefPeakFit=Win_RGB(20,20,240)
      KolDefPeakPos=Win_RGB(50,50,200)
	  KolDefBack=Win_RGB(164,211,105)
!
      KolPGWindow=KolDefPGWindow
      KolMain=KolDefMain
      KolObs=KolDefObs
      KolCal=KolDefCal
      KolDif=KolDefDif
      KolMTic=KolDefMTic
      KolCTic=KolDefCTic
      KolPanelVLite=KolDefPanelVLite
      KolPanelLite=KolDefPanelLite
      KolPanelDark=KolDefPanelDark
      KolPanelVDark=KolDefPanelVDark
      KolPanelOuter=KolDefPanelOuter
      KolRectSelect=KolDefRectSelect
      KolLargeCrossHair=KolDefLargeCrossHair
      KolPeakFit=KolDefPeakFit
      KolPeakPos=KolDefPeakPos
	  KolBack=KolDefBack
!
      CALL IGrPaletteRGB(KolNumPGWindow,KolPGWindow%IRed,&
                                        KolPGWindow%IGreen,&
                                        KolPGWindow%IBlue)
      CALL IGrPaletteRGB(KolNumMain,KolMain%IRed,&
                                    KolMain%IGreen,&
                                    KolMain%IBlue)
      CALL IGrPaletteRGB(KolNumObs,KolObs%IRed,&
                                   KolObs%IGreen,&
                                   KolObs%IBlue)
      CALL IGrPaletteRGB(KolNumCal,KolCal%IRed,&
                                   KolCal%IGreen,&
                                   KolCal%IBlue)
      CALL IGrPaletteRGB(KolNumDif,KolDif%IRed,&
                                   KolDif%IGreen,&
                                   KolDif%IBlue)
      CALL IGrPaletteRGB(KolNumMTic,KolMTic%IRed,&
                                    KolMTic%IGreen,&
                                    KolMTic%IBlue)
      CALL IGrPaletteRGB(KolNumCTic,KolCTic%IRed,&
                                    KolCTic%IGreen,&
                                    KolCTic%IBlue)
      CALL IGrPaletteRGB(KolNumPanelVLite,KolPanelVLite%IRed,&
                                          KolPanelVLite%IGreen,&
                                          KolPanelVLite%IBlue)
      CALL IGrPaletteRGB(KolNumPanelLite,KolPanelLite%IRed,&
                                         KolPanelLite%IGreen,&
                                         KolPanelLite%IBlue)
      CALL IGrPaletteRGB(KolNumPanelDark,KolPanelDark%IRed,&
                                         KolPanelDark%IGreen,&
                                         KolPanelDark%IBlue)
      CALL IGrPaletteRGB(KolNumPanelVDark,KolPanelVDark%IRed,&
                                          KolPanelVDark%IGreen,&
                                          KolPanelVDark%IBlue)
      CALL IGrPaletteRGB(KolNumPanelOuter,KolPanelOuter%IRed,&
                                          KolPanelOuter%IGreen,&
                                          KolPanelOuter%IBlue)
      CALL IGrPaletteRGB(KolNumRectSelect,KolRectSelect%IRed,&
                                          KolRectSelect%IGreen,&
                                          KolRectSelect%IBlue)
      CALL IGrPaletteRGB(KolNumLargeCrossHair,KolLargeCrossHair%IRed,&
                                              KolLargeCrossHair%IGreen,&
                                              KolLargeCrossHair%IBlue)
      CALL IGrPaletteRGB(KolNumPeakFit,KolPeakFit%IRed,&
                                       KolPeakFit%IGreen,&
                                       KolPeakFit%IBlue)
      CALL IGrPaletteRGB(KolNumPeakPos,KolPeakPos%IRed,&
                                       KolPeakPos%IGreen,&
                                       KolPeakPos%IBlue)
	  CALL IGrPaletteRGB(KolNumBack,   KolBack%IRed,&
                                       KolBack%IGreen,&
                                       KolBack%IBlue)
!
!
      END SUBROUTINE PP_DEFAULTS