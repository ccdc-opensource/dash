!
!*****************************************************************************
!
! This module contains all Z-matrix related data
!
      MODULE ZMVAR

      IMPLICIT NONE

      REAL f2cmat(1:3, 1:3), c2fmat(1:3, 1:3)

! f2cmat = 3x3 matrix for conversion from fractional to Cartesian  coordinates 
! c2fmat = 3x3 matrix for conversion from Cartesian  to fractional coordinates 

      INTEGER     maxatm
      PARAMETER ( maxatm = 150 )

      INTEGER     maxfrg
      PARAMETER ( maxfrg = 32 )

      INTEGER     maxfrginterface
      PARAMETER ( maxfrginterface = 6 )

! maxfrg = Maximum number of fragments = individual Z-matrices.
! maxfrginterface = Maximum number of fragments in the interface
! maxfrg and maxfrginterface can be different when a crystal structure has been reaad in from file
! for Rietveld refinement.
! At the moment the maximum number of fragments is limited by the interface
! In variables declared as (0:maxfrg), position 0 is reserved for a temporary copy
! used when editing a Z-matrix.

      INTEGER     MaxDOF
      PARAMETER ( MaxDOF = 50 )
! MaxDOF = Maximum number of degrees of freedom per fragment (= per individual Z-matrix)

      INTEGER     MVAR_2
      PARAMETER ( MVAR_2 = 100 )

      INTEGER     maxbnd_2
      PARAMETER ( maxbnd_2 = 1500 )
! Maximum number of bonds. Must be equal to MAXBND in SAMVAR

      INTEGER Par2iFrg(1:MVAR_2)
! Per SA parameter, to which Z-matrix does it belong. 0 = non-structural, e.g. preferred orientation

      INTEGER zm2Par(1:MaxDOF, 1:maxfrg)
! Mapping of parameters per degree of freedom per Z-matrix

      INTEGER          nfrag
! nfrag = number of fragments

      CHARACTER*255    frag_file(0:maxfrg)
! frag_file = name of the .zmatrix file containing fragment number iFrag

      LOGICAL          gotzmfile(0:maxfrg)

      INTEGER          icomflg(0:maxfrg)
      REAL             AtomicWeighting(1:maxatm, 0:maxfrg)
      LOGICAL          UseQuaternions(0:maxfrg)
! icomflg         = Centre of mass flag.
!                   0 = use centre of mass of molecule as centre of rotation
!           otherwise = use atom number icomflg as centre of rotation (necessary if atom on special position)
! AtomicWeigthing = Weight of that atom used for calculating centre of mass.
!  if all weights = 1.0 : geometric centre of mass
! UseQuaternions    .TRUE.  : all rotations allowed, described by 4 quaternions
!                   .FALSE. : only rotations about a single axis allowed (e.g. when on special position)

! We want to have some variables that specify the orientation of the Z-matrix when 
! rotation is restricted to a single axis      
      REAL     zmInitialQs(0:3,0:maxfrg)

      INTEGER  zmSingleRotAxDef(0:maxfrg)
! 1 = to atom
! 2 = fractional co-ordinates
! 3 = normal to plane

      INTEGER  zmSingleRotAxAtm(1:2, 0:maxfrg)
      REAL     zmSingleRotAxFrac(1:3, 0:maxfrg)
      INTEGER  zmSingleRotAxAtms(1:3, 0:maxfrg)
      REAL     zmSingleRotationQs(0:3, 0:maxfrg)
! zmSingleRotAxAtm  : Line through two atoms
! zmSingleRotAxFrac : Fractional co-ordinates
! zmSingleRotAxAtms : Three atoms defining a plane the normal of which is the direction of rotation
!                     These numbers are entered by the users according to their numbering scheme,
!                     but stored in the DASH numbering
! zmSingleRotationQs   = Factors in the quaternion-expression of the rotation about a single axis
!                        which are due to the orientation of the single axis
    
      INTEGER         izmpar(0:maxfrg)
      CHARACTER*36    czmpar(1:MaxDOF, 0:maxfrg)
      INTEGER         kzmpar(1:MaxDOF, 0:maxfrg)
      INTEGER         kzmpar2(1:MVAR_2)
      REAL            xzmpar(1:MaxDOF, 0:maxfrg)

! izmpar = number of degrees of freedom ('parameters') per Z-matrix
! czmpar = Character string associated with this parameter value
! kzmpar = type of parameter
!          1 = translation (3 D.O.F.)
!          2 = rotation with quaternions (4 D.O.F., 0 D.O.F. if single atom)
!          3 = torsion        (1 D.O.F. per flexible torsion)
!          4 = valence angle
!          5 = bond length
!          6 = rotation about a single axis (2 D.O.F.)
! kzmpar2 = as kzmpar, but per parameter rather than per Z-matrix per parameter.
!           This way, e.g. preferred orientation (which doesn't belong to a Z-matrix)
!           can be dealt with.
! xzmpar = initial value of parameter

      INTEGER         natoms(0:maxfrg)
      INTEGER         ioptb(1:maxatm, 0:maxfrg), iopta(1:maxatm, 0:maxfrg), ioptt(1:maxatm, 0:maxfrg)
      INTEGER         iz1(1:maxatm, 0:maxfrg), iz2(1:maxatm, 0:maxfrg), iz3(1:maxatm, 0:maxfrg)

! natoms = number of atoms in this fragment (=Z-matrix)
! ioptb  = optimise bond length 1=YES, 0=NO.
! iopta  = optimise valence angle 1=YES, 0=NO.
! ioptt  = optimise torsion angle 1=YES, 0=NO.
! iz1, iz2, iz3 = atoms with respect to which the current atom is defined in the Z-matrix

      REAL blen(1:maxatm, 0:maxfrg), alph(1:maxatm, 0:maxfrg), bet(1:maxatm, 0:maxfrg)

! blen   = bond length     (wrt iz1)
! alph   = valence angle   (wrt iz1 & iz2)
! bet    = torsion angle   (wrt iz1, iz2 & iz3)

      CHARACTER*3     asym(1:maxatm, 0:maxfrg)
      INTEGER         zmElementCSD(1:maxatm, 0:maxfrg)
      CHARACTER*5     OriginalLabel(1:maxatm, 0:maxfrg)

! asym = Atom SYMbol--e.g. 'H  ' for hydrogen, 'Ag ' for silver--of the current atom.
! zmElement = CSD element number. MaxElm = dummy.
! OriginalLabel = the label of the atom as read from the .res/.mol2/etc. file
! (read from column 14 in the Z-matrix file)
! Note that we allow five characters, .pdb allows 4, .res and .cssr can't cope with the
! atom label being a real 'name', it must be the element + a number

      REAL tiso(1:maxatm, 0:maxfrg), occ(1:maxatm, 0:maxfrg)

! tiso = Isotropic temperature factor of the current atom
! occ  = Occupancy of the current atom

      INTEGER izmoid(0:maxatm, 0:maxfrg), izmbid(0:maxatm, 0:maxfrg)

! The original atom ids to list in the labels and the back mapping
! Atom number 0 means 'not specified' and always maps onto itself

      INTEGER NumberOfBonds(0:maxfrg)
      INTEGER BondType(1:maxbnd_2, 0:maxfrg)
      INTEGER Bonds(1:2, 1:maxbnd_2, 0:maxfrg)

! Bondtypes and bonds. Precalculated and stored for speed. 90,000 bytes
!   BondType:
!     1 = single
!     2 = double
!     3 = triple
!     4 = quadruple
!     5 = aromatic
!     6 = polymeric single
!     7 = delocalised
!     9 = pi-bond

      INTEGER CurrentlyEditedFrag
      LOGICAL zmAtomDeleted

! CurrentlyEditedFrag Holds the number of the Z-matrix which is being edited 
!                     if the Z-matrix edit dialogue is active.
! zmAtomDeleted       We don't want to remake a Z-matrix if not necessary (it might be
!                     a hand-made Z-matrix). This variable is set if an atom was deleted
!                     in the Edit Z-matrix window.
  
      END MODULE ZMVAR
!
!*****************************************************************************
!
