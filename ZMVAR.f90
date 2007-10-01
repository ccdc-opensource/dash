!
!*****************************************************************************
!
! This module contains all z-matrix related data
!
      MODULE ZMVAR

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

! The following variables are there to allow the dialogue fields in the
! window dealing with z-matrices to be handled by DO...ENDDO loops.
! The field identifiers assigned by Winteracter are not necessarily consecutive, 
! but these mappings are.

! For use in common blocks ZMCOMI, ZMCOMR, ZMCOMC, ZMCOMO, FRGCOM, FRGCHA, ZMCOMG, ZMLGOT, ZMNPAR

      INTEGER maxatm
      PARAMETER ( maxatm = 100 )

      INTEGER maxfrg
      PARAMETER ( maxfrg = 5 )

! maxfrg = Maximum number of fragments = individual z-matrices

      INTEGER MaxDOF
      PARAMETER ( MaxDOF = 30 )

! MaxDOF = Maximum number of degrees of freedom per fragment (= per individual z-matrix)

      INTEGER        IDFZMFile,                                       &
                     IDBZMDelete,           IDBZMBrowse,              &
                     IDBZMView,             IDBZMEdit,                &
                     IDFZMpars
      COMMON /IDFZM/ IDFZMFile(1:maxfrg),                             &
                     IDBZMDelete(1:maxfrg), IDBZMBrowse(1:maxfrg),    &
                     IDBZMView(1:maxfrg),   IDBZMEdit(1:maxfrg),      &
                     IDFZMpars(1:maxfrg)
      DATA IDFZMFile   / IDF_ZMatrix_file1,   IDF_ZMatrix_file2,   IDF_ZMatrix_file3,   IDF_ZMatrix_file4,   IDF_ZMatrix_file5   /
      DATA IDBZMDelete / IDB_ZmatrixDelete1,  IDB_ZmatrixDelete2,  IDB_ZmatrixDelete3,  IDB_ZmatrixDelete4,  IDB_ZmatrixDelete5  /
      DATA IDBZMBrowse / IDB_ZMatrix_Browse1, IDB_ZMatrix_Browse2, IDB_ZMatrix_Browse3, IDB_ZMatrix_Browse4, IDB_ZMatrix_Browse5 /
      DATA IDBZMView   / IDB_ZMatrixView1,    IDB_ZMatrixView2,    IDB_ZMatrixView3,    IDB_ZMatrixView4,    IDB_ZMatrixView5    /
      DATA IDBZMEdit   / IDB_ZMatrixEdit1,    IDB_ZMatrixEdit2,    IDB_ZMatrixEdit3,    IDB_ZMatrixEdit4,    IDB_ZMatrixEdit5    /
      DATA IDFZMpars   / IDF_ZM_pars1,        IDF_ZM_pars2,        IDF_ZM_pars3,        IDF_ZM_pars4,        IDF_ZM_pars5        /

      INTEGER         nfrag
      COMMON /frgcom/ nfrag

! nfrag = number of fragments

      CHARACTER*80    frag_file
      COMMON /frgcha/ frag_file(maxfrg)

! maxfrg    = (=20) appears to be the maximum number of z-matrices. The Winteracter window
! only allows up to CheckSize = 5, though.
! frag_file = name of the .zmatrix file containing fragment number ifrag

      LOGICAL         gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)

      INTEGER         icomflg
      REAL                             AtomicWeighting
      COMMON /zmcomg/ icomflg(maxfrg), AtomicWeighting(maxatm,maxfrg)

! icomflg         = Centre of mass flag.
!                   0 = use centre of mass of molecule as centre of rotation
!           otherwise = use atom number icomflg as centre of rotation (necessary if atom on special position)
! AtomicWeigthing = Weight of that atom usied for calculating centre of mass.
!  if all weights = 1.0 : geometric centre of mass
    
      INTEGER         izmpar
      CHARACTER*36                    czmpar
      INTEGER                                                kzmpar
      REAL                                                                          xzmpar
      COMMON /zmnpar/ izmpar(maxfrg), czmpar(MaxDOF,maxfrg), kzmpar(MaxDOF,maxfrg), xzmpar(MaxDOF,maxfrg)

! izmpar = number of degrees of freedom ('parameters')
! czmpar = Character string associated with this parameter value
! kzmpar = type of parameter
!          1 = translation
!          2 = rotation
!          3 = torsion
!          4 = valence angle
!          5 = bond length
! xzmpar = initial value of parameter

      INTEGER         ntatm, natoms
      INTEGER         ioptb,                iopta,                ioptt
      INTEGER         iz1,                  iz2,                  iz3
      COMMON /zmcomi/ ntatm, natoms(maxfrg),                                             &
     &                ioptb(maxatm,maxfrg), iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),  &
     &                iz1(maxatm,maxfrg),   iz2(maxatm,maxfrg),   iz3(maxatm,maxfrg)

! ntatm  =
! natoms = number of atoms in this fragment (=z-matrix)
! ioptb  = optimise bond length 1=YES, 0=NO. Not implemented.
! iopta  = optimise valence angle 1=YES, 0=NO. Not implemented.
! ioptt  = optimise torsion angle 1=YES, 0=NO.
! iz1, iz2, iz3 = atoms with respect to which the current atom is defined in the z-matrix

      DOUBLE PRECISION blen,                alph,                bet,                f2cmat
      COMMON /zmcomr/  blen(maxatm,maxfrg), alph(maxatm,maxfrg), bet(maxatm,maxfrg), f2cmat(3,3)

! blen   = bond length     (wrt iz1)
! alph   = valence angle   (wrt iz1 & iz2)
! bet    = torsion angle   (wrt iz1, iz2 & iz3)
! f2cmat = 3x3 matrix for conversion from fractional to Cartesian coordinates 

      CHARACTER*3     asym
      CHARACTER*5                          OriginalLabel
      COMMON /zmcomc/ asym(maxatm,maxfrg), OriginalLabel(maxatm,maxfrg)

! asym = Atom SYMbol--e.g. 'H  ' for hydrogen, 'Ag ' for silver--of the current atom.
! OriginalLabel = the label of the atom as read from the .res/.mol2/etc. file
! (read from column 14 in the z-matrix file)

      REAL            tiso,                occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)

! tiso = Isotropic temperature factor of the current atom
! occ  = Occupancy of the current atom

! The original atom ids to list in the labels and the back mapping
      INTEGER         izmoid,                izmbid
      COMMON /zmjcmp/ izmoid(maxatm,maxfrg), izmbid(maxatm,maxfrg)

      END MODULE ZMVAR
!
!*****************************************************************************
!
