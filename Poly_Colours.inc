!
! Common blocks associated with all the plotting colours
!

      TYPE (Win_RGB) ::   KolMain, KolObs, KolCal, KolDif, KolMTic, KolCTic,&
                          KolPanelVLite, KolPanelLite, KolPanelDark,&
      KolPanelVDark,KolPanelOuter,KolPGWindow,&
      KolRectSelect,KolLargeCrossHair,&
      KolPeakFit,KolPeakPos,KolBack
      COMMON /COLOUR_NOW/ KolMain, KolObs, KolCal, KolDif, KolMTic, KolCTic,&
                          KolPanelVLite, KolPanelLite, KolPanelDark,&
      KolPanelVDark,KolPanelOuter,KolPGWindow,&
      KolRectSelect,KolLargeCrossHair,&
      KolPeakFit,KolPeakPos,KolBack

      INTEGER KolNumMain,KolNumObs,KolNumCal,KolNumDif,KolNumMTic,KolNumCTic,&
      KolNumPanelVLite,KolNumPanelLite,KolNumPanelDark,&
      KolNumPanelVDark,KolNumPanelOuter,KolNumPGWindow,&
      KolNumRectSelect,KolNumLargeCrossHair,&
      KolNumPeakFit,KolNumPeakPos,KolNumBack
      COMMON /COLOUR_NUM/ KolNumMain,KolNumObs,KolNumCal,KolNumDif,KolNumMTic,KolNumCTic,&
      KolNumPanelVLite,KolNumPanelLite,KolNumPanelDark,&
      KolNumPanelVDark,KolNumPanelOuter,KolNumPGWindow,&
      KolNumRectSelect,KolNumLargeCrossHair,&
      KolNumPeakFit,KolNumPeakPos,KolNumBack
!