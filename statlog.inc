! CELLOK is now a function called FnUnitCellOK()
! WVLNOK is now a function called FnWaveLengthOK()
! SPGPOK was never used anyway

      INTEGER DataSetChange
      COMMON /STATLOG2/ DataSetChange

      LOGICAL SkipPawleyRef
      INTEGER NumPawleyRef,NumInternalDSC
      COMMON /qpcommon/ SkipPawleyRef,NumPawleyRef,NumInternalDSC

! IRadOption has been replaced by JRadOption, a global variable	that was used in the non-Wizard part of DASH
! JRadOption is in 'GLBVAR.INC'.

! NumberSGTable has moved to 'GLBVAR.INC'.

      INTEGER XBSWidth,XBSHeight
      COMMON /SCREENPARS/ XBSWidth,XBSHeight

      LOGICAL FromPawleyFit
      COMMON /SAOrigins/ FromPawleyFit


      

