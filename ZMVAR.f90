!
!*****************************************************************************
!
! This module contains all Z-matrix related data
!
      MODULE ZMVAR

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER maxatm
      PARAMETER ( maxatm = 150 )

      INTEGER maxcopies
      PARAMETER ( maxcopies = 8 )

      INTEGER maxfrg
      PARAMETER ( maxfrg = 4 )

! maxfrg = Maximum number of fragments = individual Z-matrices
! At the moment the maximum number of fragments is limited by the interface
! In variables declared as (0:maxfrg), position 0 is reserved for a temporary copy
! used when editing a Z-matrix.

      INTEGER MaxDOF
      PARAMETER ( MaxDOF = 30 )

! MaxDOF = Maximum number of degrees of freedom per fragment (= per individual Z-matrix)

      INTEGER maxbnd_2
      PARAMETER ( maxbnd_2 = 1500 )

! Maximum number of bonds. Must be equal to MAXBND in SAMVAR

      INTEGER Par2iFrg(1:100)

! Per SA parameter, to which Z-matrix does it belong. 0 = non-structural, e.g. preferred orientation

      INTEGER Par2iFrgCopy(1:100)

! Per SA parameter, to which copy of the Z-matrix does it belong.

      INTEGER zm2Par(1:MaxDOF,1:maxcopies,1:maxfrg)

! Mapping of parameters per dergee of freedom per copy per Z-matrix

! The following variables are there to allow the dialogue fields in the
! window dealing with Z-matrices to be handled by DO...ENDDO loops.
! The field identifiers assigned by Winteracter are not necessarily consecutive, 
! but these mappings are.

      INTEGER        IDFZMNumber,           IDFZMFile,                &
                     IDBZMDelete,           IDBZMBrowse,              &
                     IDBZMView,             IDBZMEdit,                &
                     IDFZMpars
      COMMON /IDFZM/ IDFZMNumber(1:maxfrg), IDFZMFile(1:maxfrg),      &
                     IDBZMDelete(1:maxfrg), IDBZMBrowse(1:maxfrg),    &
                     IDBZMView(1:maxfrg),   IDBZMEdit(1:maxfrg),      &
                     IDFZMpars(1:maxfrg)
      DATA IDFzmNumber / IDF_zmNumOf1,  IDF_zmNumOf2,  IDF_zmNumOf3,  IDF_zmNumOf4  /
      DATA IDFZMFile   / IDF_zmFile1,   IDF_zmFile2,   IDF_zmFile3,   IDF_zmFile4   /
      DATA IDBZMDelete / IDB_zmDelete1, IDB_zmDelete2, IDB_zmDelete3, IDB_zmDelete4 /
      DATA IDBZMBrowse / IDB_zmBrowse1, IDB_zmBrowse2, IDB_zmBrowse3, IDB_zmBrowse4 /
      DATA IDBZMView   / IDB_zmView1,   IDB_zmView2,   IDB_zmView3,   IDB_zmView4   /
      DATA IDBzmEdit   / IDB_zmEdit1,   IDB_zmEdit2,   IDB_zmEdit3,   IDB_zmEdit4   /
      DATA IDFZMpars   / IDF_ZM_pars1,  IDF_ZM_pars2,  IDF_ZM_pars3,  IDF_ZM_pars4  /

      INTEGER         nfrag

! nfrag = number of fragments

      INTEGER         TotNumZMatrices

! Total number of Z-matrices, including number of copies

      CHARACTER*255   frag_file(0:maxfrg)

! frag_file = name of the .zmatrix file containing fragment number ifrag

      INTEGER         zmNumberOfCopies(0:maxfrg)

! zmNumberOfCopies  =  number of copies of this Z-matrix used during the SA.
!                      This way, it is easy e.g. to solve salts or solve structures in P1
!                      Each copy is identical (including occupancies / single axis) except for:
!                      1. its translation x, y, z
!                      2. its quaternions
!                      3. its torsion angles

      LOGICAL         gotzmfile(0:maxfrg)

! zmFileChanged Set to .TRUE.  when Z-matrix opened/deleted.
!               Set to .FALSE. after SA parameter boundaries dialogue has been initialised

      INTEGER          icomflg(0:maxfrg)
      REAL             AtomicWeighting(1:maxatm,0:maxfrg)
      LOGICAL          UseQuaternions(0:maxfrg)
      REAL             zmSingleRotationAxis(1:3,0:maxfrg)
      DOUBLE PRECISION zmSingleRotationQs(0:3,0:maxfrg)

! icomflg         = Centre of mass flag.
!                   0 = use centre of mass of molecule as centre of rotation
!           otherwise = use atom number icomflg as centre of rotation (necessary if atom on special position)
! AtomicWeigthing = Weight of that atom used for calculating centre of mass.
!  if all weights = 1.0 : geometric centre of mass
! UseQuaternions    .TRUE.  : all rotations allowed, described by 4 quaternions
!                   .FALSE. : only rotations about a single axis allowed (e.g. when on special position)
! zmSingleRotationAxis = If UseQuaternions = .FALSE., this is the axis that is used
! zmSingleRotationQs   = Factors in the quaternion-expression of the rotation about a single axis
!                        which are due to the orientation of the single axis
    
      INTEGER         izmpar(0:maxfrg)
      CHARACTER*36    czmpar(1:MaxDOF,0:maxfrg)
      INTEGER         kzmpar(1:MaxDOF,0:maxfrg)
      INTEGER         kzmpar2(1:100)
      REAL            xzmpar(1:MaxDOF,0:maxfrg)

! izmpar = number of degrees of freedom ('parameters') per Z-matrix
! czmpar = Character string associated with this parameter value
! kzmpar = type of parameter
!          1 = translation (3 D.O.F.)
!          2 = rotation with quaternions (4 D.O.F., 0 D.O.F. if single atom)
!          3 = torsion        (1 D.O.F. per flexible torsion)
!          4 = valence angle
!          5 = bond length
!          6 = rotation about a single axis (also 4 D.O.F., could be made 1 or 2)
! kzmpar2 = as kzmpar, but per parameter rather than per Z-matrix per parameter.
!           This way, e.g. preferred orientation (which doesn't belong to a Z-matrix)
!           can be dealt with.
! xzmpar = initial value of parameter

      INTEGER         ntatm
      INTEGER         natoms(0:maxfrg)
      INTEGER         ioptb(1:maxatm,0:maxfrg), iopta(1:maxatm,0:maxfrg), ioptt(1:maxatm,0:maxfrg)
      INTEGER         iz1(1:maxatm,0:maxfrg), iz2(1:maxatm,0:maxfrg), iz3(1:maxatm,0:maxfrg)

! ntatm  = total number of atoms. Must be equal to NATOM in /POSNS/
! natoms = number of atoms in this fragment (=Z-matrix)
! ioptb  = optimise bond length 1=YES, 0=NO. Not implemented.
! iopta  = optimise valence angle 1=YES, 0=NO. Not implemented.
! ioptt  = optimise torsion angle 1=YES, 0=NO.
! iz1, iz2, iz3 = atoms with respect to which the current atom is defined in the Z-matrix

      DOUBLE PRECISION blen(1:maxatm,0:maxfrg), alph(1:maxatm,0:maxfrg), bet(1:maxatm,0:maxfrg)

! blen   = bond length     (wrt iz1)
! alph   = valence angle   (wrt iz1 & iz2)
! bet    = torsion angle   (wrt iz1, iz2 & iz3)

      DOUBLE PRECISION f2cmat(1:3,1:3), c2fmat(1:3,1:3)

! f2cmat = 3x3 matrix for conversion from fractional to Cartesian  coordinates 
! c2fmat = 3x3 matrix for conversion from Cartesian  to fractional coordinates 

      CHARACTER*3     asym(1:maxatm,0:maxfrg)
      CHARACTER*5     OriginalLabel(1:maxatm,0:maxfrg)

! asym = Atom SYMbol--e.g. 'H  ' for hydrogen, 'Ag ' for silver--of the current atom.
! OriginalLabel = the label of the atom as read from the .res/.mol2/etc. file
! (read from column 14 in the Z-matrix file)
! Note that we allow five characters, .pdb allows 4, .res and .cssr can't cope with the
! atom label being a real 'name', it must be the element + a number

      REAL tiso(1:maxatm,0:maxfrg), occ(1:maxatm,0:maxfrg)

! tiso = Isotropic temperature factor of the current atom
! occ  = Occupancy of the current atom

      INTEGER izmoid(1:maxatm,0:maxfrg), izmbid(1:maxatm,0:maxfrg)

! The original atom ids to list in the labels and the back mapping

      INTEGER NumberOfBonds(0:maxfrg)
      INTEGER BondType(1:maxbnd_2,0:maxfrg)
      INTEGER Bonds(1:2,1:maxbnd_2,0:maxfrg)

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
