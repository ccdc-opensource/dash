!
!*****************************************************************************
!
      MODULE RRVAR

      IMPLICIT NONE

      INTEGER     RR_MVAR
      PARAMETER ( RR_MVAR = 300 )

      INTEGER     RR_maxatm
      PARAMETER ( RR_maxatm = 300 )

      INTEGER     RR_maxfrg
      PARAMETER ( RR_maxfrg = 32 )

! RR_maxfrg = Maximum number of fragments = individual Z-matrices
! At the moment the maximum number of fragments is limited by the interface

      INTEGER RR_iopttran(1:3, 1:RR_maxfrg)
      INTEGER RR_ioptrot(1:4, 1:RR_maxfrg)
      INTEGER RR_ioptb(1:RR_maxatm, 1:RR_maxfrg)
      INTEGER RR_iopta(1:RR_maxatm, 1:RR_maxfrg)
      INTEGER RR_ioptt(1:RR_maxatm, 1:RR_maxfrg)
      INTEGER RR_ioptITF
      INTEGER RR_ioptPO

! ioptb  = optimise bond length 1=YES, 0=NO.
! iopta  = optimise valence angle 1=YES, 0=NO.
! ioptt  = optimise torsion angle 1=YES, 0=NO.

      LOGICAL RR_Show_bond(1:RR_maxatm,1:RR_maxfrg)
      LOGICAL RR_Show_angle(1:RR_maxatm,1:RR_maxfrg)
      LOGICAL RR_Show_torsion(1:RR_maxatm,1:RR_maxfrg)

! Optimising torsions/angles/bonds with Hydrogens in them does not make much sense,
! so these parameters can be hidden from the Rietveld refinement dialogue.

      REAL RR_tran(1:3, 1:RR_maxfrg)
      REAL RR_rot(1:4, 1:RR_maxfrg)
      REAL RR_blen(1:RR_maxatm, 1:RR_maxfrg)
      REAL RR_alph(1:RR_maxatm, 1:RR_maxfrg)
      REAL RR_bet(1:RR_maxatm, 1:RR_maxfrg)
      REAL RR_ITF ! Global Isotropic Temperature Factor
      REAL RR_PO ! Extent of Preferred Orientation

! tran   = translation
! blen   = bond length     (wrt iz1)
! alph   = valence angle   (wrt iz1 & iz2)
! bet    = torsion angle   (wrt iz1, iz2 & iz3)

      REAL RR_Params(1:RR_MVAR)
      REAL RR_InitSteps(1:RR_MVAR) ! Initial step sizes for Simplex
      INTEGER RR_var2ITF

      INTEGER RR_npar

! RR_nvar = RR_npar for Rietveld refinement

      REAL RR_XATO_Orig(1:3,1:RR_maxatm)

! The original atomic co-ordinates before Rietveld refinement, for comparison.

      END MODULE RRVAR
!
!*****************************************************************************
!
