      INTEGER IXPos_IDD_Pawley_Status
      INTEGER IYPos_IDD_Pawley_Status
      INTEGER IXPos_IDD_SA_Input
      INTEGER IYPos_IDD_SA_Input
      INTEGER IXPos_IDD_Wizard
      INTEGER IYPos_IDD_Wizard
! The continuation lines are written such that they are both f77 and f90 compatible.
! It is not beautiful, but it is as recommended in the Compaq Fortran manual (p. 2-16).
      COMMON /DialoguePosCmn/ IXPos_IDD_Pawley_Status,                  &
     &                        IYPos_IDD_Pawley_Status,                  &
     &                        IXPos_IDD_SA_Input,                       &
     &                        IYPos_IDD_SA_Input,                       &
     &                        IXPos_IDD_Wizard,                         &
     &                        IYPos_IDD_Wizard
! These variables store the positions of several dialogue windows.
! This is necessary to preserve these coordinates when a window is
! displayed, moved, closed and then displayed again.
