!
!*****************************************************************************
!
      MODULE RRVAR

      IMPLICIT NONE

      INTEGER     RR_MVAR
      PARAMETER ( RR_MVAR = 200 )

      INTEGER RR_maxatm
      PARAMETER ( RR_maxatm = 150 )

      INTEGER RR_maxfrg
      PARAMETER ( RR_maxfrg = 4 )

      INTEGER RR_maxcopies
      PARAMETER ( RR_maxcopies = 8 )

! RR_maxfrg = Maximum number of fragments = individual Z-matrices
! At the moment the maximum number of fragments is limited by the interface

      INTEGER RR_iopttran(1:3,1:RR_maxfrg,1:RR_maxcopies)
      INTEGER RR_ioptrot(1:4,1:RR_maxfrg,1:RR_maxcopies)
      INTEGER RR_ioptb(1:RR_maxatm,1:RR_maxfrg,1:RR_maxcopies)
      INTEGER RR_iopta(1:RR_maxatm,1:RR_maxfrg,1:RR_maxcopies)
      INTEGER RR_ioptt(1:RR_maxatm,1:RR_maxfrg,1:RR_maxcopies)
      INTEGER RR_ioptITF
      INTEGER RR_ioptPO

! ioptb  = optimise bond length 1=YES, 0=NO.
! iopta  = optimise valence angle 1=YES, 0=NO.
! ioptt  = optimise torsion angle 1=YES, 0=NO.

      REAL RR_tran(1:3,1:RR_maxfrg,1:RR_maxcopies)
      REAL RR_rot(1:4,1:RR_maxfrg,1:RR_maxcopies)
      REAL RR_blen(1:RR_maxatm,1:RR_maxfrg,1:RR_maxcopies)
      REAL RR_alph(1:RR_maxatm,1:RR_maxfrg,1:RR_maxcopies)
      REAL RR_bet(1:RR_maxatm,1:RR_maxfrg,1:RR_maxcopies)
      REAL RR_ITF ! Global Isotropic Temperature Factor
      REAL RR_PO ! Extent of Preferred Orientation

! tran   = translation
! blen   = bond length     (wrt iz1)
! alph   = valence angle   (wrt iz1 & iz2)
! bet    = torsion angle   (wrt iz1, iz2 & iz3)

      REAL RR_Params(1:RR_MVAR)
      REAL RR_InitSteps(1:RR_MVAR) ! Initial step sizes for Simplex
      INTEGER RR_var2PO
      INTEGER RR_var2ITF

      INTEGER RR_nvar, RR_npar

! RR_nvar = RR_npar for Rietveld refinement

      REAL RR_XATO_Orig(1:3,1:RR_maxatm)

! The original atomic co-ordinates before Rietveld refinement, for comparison.

      END MODULE RRVAR
!
!*****************************************************************************
!
