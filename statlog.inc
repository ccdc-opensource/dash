! CELLOK is now a function called FnUnitCellOK()
! WVLNOK is now a function called FnWaveLengthOK()
! SPGPOK was never used anyway
      INTEGER DataSetChange
      COMMON /STATLOG2/ DataSetChange
      LOGICAL SkipPawleyRef
      INTEGER NumPawleyRef,NumInternalDSC
      COMMON /qpcommon/ SkipPawleyRef,NumPawleyRef,NumInternalDSC
! IRadOption has been replaced by JRadOption, a global variable	that was used in the non-Wizard part of DASH
! JRadOption is in GLBVAR.INC
      INTEGER NumberSGTable
      COMMON /NSGTCMN/ NumberSGTable
      INTEGER XBSWidth,XBSHeight,XBSColours
      COMMON /SCREENPARS/ XBSWidth,XBSHeight,XBSColours
      INTEGER WRWidth,WRHeight,WRXPos,WRYPos
      COMMON /WROOTPARS/ WRWidth,WRHeight,WRXPos,WRYPos
      LOGICAL FromPawleyFit
      COMMON /SAOrigins/ FromPawleyFit


      

