!*==SAMABO.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
      SUBROUTINE SAMABO
!-- Function: Assign bond types to a crystal connectivity with coordinates.
!-- Version:  29.9.94  27.4.95           Sam Motherwell     19.8.94
!-- Notes:
!-- 1. This makes a best guess at the bond types using standard ranges
!--    for various atom pairs between  C N O S P.  If a bond type can
!--    not be assigned with confidence the type is set in BTYPE(i)=0
!--    The CSD bond types are:  1 = single  2= double  3=triple  4=quadruple
!--                             5 = aromatic      6 = polymeric single
!--                             7 = delocalised   9 = pi-bond
!-- 2. The hybridisation state  1=linear sp1, 2= planar sp2, 3=tetrahed. sp3
!--    is assigned for each atom.  Normal valence states  (C = 4,  N = 3 etc)
!--    are used to guess at double single bonds.
!-- 3. Aromatic rings are detected by looking at torsion angles - the
!--    bond lengths are not as important here as the planarity of a
!--    5- or 6-membered ring.
!-- 4. Pi-bonds are detected by the Metal -C-C  triangle.
!-- 5. Experiments in progress as to best order of working.
!--    On the principle of doing the easiest first:
!--    1. detect pi-bonds
!--    2. detect aromatic (flat) rings
!--    3. assign double,triple bonds for O,S,P
!--    4. assign double bonds to satisfy valency on C, N
!--    5. final check over for valency problems & set highlight flag perhaps.
!--
!--
!-- 6. Input is the cryst. bond list BOND()
!--    This is processed into standard 3D connectivity arrays NHYC,NCAC, etc
!--    These define the crystal connectivity.
!--   AELEM   element code number  e.g. C=1 N=56 etc
!--   NHYC    number of terminal h
!--   NCAC    number of connections exclude term. hyd.
!--   ATRESN  number of cryst. residue
!--   BOND    array of bonds Iat, Jat
!--   BTYPE   bondtype
!--   NATCRY  number of atoms in cryst. arrays
!--   NBOCRY  number of bonds in BOND
!--   AXYZO   orthogonal coords in angstroms
!--
!-- 7. Output.   The bond types derived are set in BTYPE()
!
      USE SAMVAR
      IMPLICIT NONE
!
! Local variables
!
      REAL :: Angmax, Aval, D1, Dmin, Tor1, Torave, Tormax, V
      REAL, DIMENSION(30) :: Dij, Torang
      REAL, INTRINSIC :: FLOAT
      INTEGER :: Hypres, I, I1, I9, Iat, Iel, Iok, Ipass, Ipib, J, J1, J9, Jat, K, Kat, Kmin, Lat,  &
     &           M, N1, Nbt, Ncc, Ncon, Ncona, Nheter, Nhy, Nmetal, Nnitro, Nnot, Nnot1, Noxy,  &
     &           Nphosp, Npib, Nring, Nsp3, Ntor, Nval, Nz
      INTEGER, DIMENSION(30) :: Icob, Icon, Llig, Lmig
      INTEGER, DIMENSION(3) :: Ltype
      INTEGER, INTRINSIC :: NINT
      INTEGER, DIMENSION(20) :: Ringat
!
!-- HYBR      estimate of hybridisation 1 = sp1 2=sp2 3=sp3  >100 = metal
!
!--   HYPRES   H total in cryst.
!--   METTOT   metal total in cryst
!--   ICON list of atoms connected to Iat
!--   ICOB list of bondtypes for ICON
!----------------------------------------------------------------------
!--
!-- set up 3D connectivity arrays NHYC, NCAC etc in common
!-- using BOND() as input
      CALL SAMCC3
!
!-- Set all bond to type = 0.
!-- mark any bonds involving suppressed atoms with bt=99. Residue number < 0
!-- assign single bond to all H atoms  -element code = 2
!--
      DO I = 1, nbocry
        Iat = bond(I,1)
        Jat = bond(I,2)
        IF (aelem(Iat).EQ.2 .OR. aelem(Jat).EQ.2) THEN
          btype(I) = 1
        ELSE
          btype(I) = 0
        ENDIF
      ENDDO
!-- scan to see if H present in cryst conn & identify metal atoms
      D1 = 0.0
      Hypres = 0
      DO I = 1, natcry
        IF (aelem(I).EQ.2) Hypres = Hypres + 1
      ENDDO
!-- assign bonds to metals
! Looks for pi-bonds and sets metal-metal bonds to 1 (i.e., single bond)
      CALL SAMABM(Npib)
!-- set flag for SAMCON routine to ignore pi-bonds
      Ipib = -1
!-- get hybridisation state for each atom
      DO I = 1, natcry
        IF (hybr(I).EQ.0) THEN ! It is a non-metal
!-- get number of connections NCONA - ignoring pi-bonds
!-- These connections include any H-atoms.
          CALL SAMCON(I,Ncona,Icon,Icob,Ipib)
          Nhy = nhyc(I)
!-- reset the number of connections to non-H atoms,  NCAC
! This should never be necessary, we haven't touched ncac
          IF (ncac(I) .NE. (Ncona - Nhy)) CALL DebugErrorMessage('ncac(I) .NE. (Ncona - Nhy)')
          ncac(I) = Ncona - Nhy
          Ncon = Ncona
!-- connection gt 4   just set HYBR to nc
          IF(ncac(I).GT.4)THEN
            hybr(I) = ncac(I)
            GOTO 100
          ENDIF
!-- if 3 connections test planarity of 4-atoms          A
!-- use torsion angle     A-B-C-X                       |
!-- planar groups set hybr=2                            X
!--                                                    . .
!--                                                   B   C
!--
!-- Angle-max criterion is 12.5
!-- If a,b, or c are Hydrogen then relax criterion angle-max to 20
          IF (Ncon.EQ.3) THEN
            Iat = Icon(1)
            Jat = Icon(2)
            Kat = Icon(3)
            Lat = I
            CALL SAMTOX(Iat,Jat,Kat,Lat,Aval)
            Angmax = 12.5
            IF (nhyc(I).GT.0) Angmax = 20.0
            IF (ABS(Aval).LT.Angmax) THEN
              hybr(I) = 2
            ELSE
              hybr(I) = 3
            ENDIF
            GOTO 100
          ENDIF
!-- use terminal-H counts to identify hybridisation.
!-- No action if no H in cryst. conn.  HYPRES = 0
!-- This gives a working guess for hybridisation state.
!--
!-- Carbon
          Iel = aelem(I)
          Ncon = ncac(I)
          IF (hybr(I).NE.0) CALL DebugErrorMessage('hybr(I) .NE. 0')
          IF (Iel.EQ.1 .AND. hybr(I).EQ.0) THEN
            SELECT CASE (Ncon)
              CASE (1)
                IF (Nhy.EQ.1) hybr(I) = 1
                IF (Nhy.EQ.2) hybr(I) = 2
                IF (Nhy.GE.3) hybr(I) = 3
              CASE (2)
                IF (Nhy.EQ.1) hybr(I) = 2
                IF (Nhy.EQ.2) hybr(I) = 3
              CASE (3)
                IF (Nhy.EQ.1) hybr(I) = 3
              CASE (4)
                hybr(I) = 3
            END SELECT
          ENDIF
!-- Nitrogen
          IF (Iel.EQ.56) THEN
            SELECT CASE (Ncon)
              CASE (0)
                ! do nothing
              CASE (1)
                IF (Hypres.GT.0 .AND. Nhy.EQ.0) hybr(I) = 1
                IF (Nhy.EQ.1) hybr(I) = 2
                IF (Nhy.GE.2) hybr(I) = 3
              CASE (2)
                IF (Hypres.GT.0 .AND. Nhy.EQ.0) hybr(I) = 2
                IF (Nhy.GE.2) hybr(I) = 3
              CASE DEFAULT
                hybr(I) = 3
            END SELECT
          ENDIF
!-- Oxygen / Sulfur
          IF (Iel.EQ.64 .OR. Iel.EQ.81) THEN
            IF (Hypres.GT.0 .AND. Nhy.EQ.0) hybr(I) = 2
            IF (Nhy.GE.1) hybr(I) = 3
          ENDIF
        ENDIF
  100 ENDDO
!--
!-- First do the easy bits of the puzzle!
!--
!-- assign bonds which are unambiguous & commonly occurring
!--  H - A      single
!--  C = O      carboxyl
!--  C = O      carbonyl triple
!--  C - OH     single
!--  C - C      single terminal methyl
!--  C - O - C  singles
!--  C = S      double
!--  C - S      single
!--  C = N      triple terminal
!--  A - N - B  N with 3 bonds single
!--      |
!--      C
!--  Halogen-C  single
!--  O = N
!--  O = S
!--
      DO Iat = 1, natcry
        CALL SAMCON(Iat,N1,Llig,Lmig,Ipib)
!-- Hydrogen & Deuterium
        IF (aelem(Iat).EQ.2 .OR. aelem(Iat).EQ.27) THEN
          DO J = 1, N1
            Jat = Llig(J)
            Nbt = 1
            CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
          ENDDO
        ENDIF
!-- Oxygen
        IF(aelem(Iat).EQ.64)THEN
!--  O terminal.
!--   C = O         terminal O,  C ncac=3,  dij < 1.30
!--   CO  carbonyl  terminal O,  C ncac=2,  dij < 1.30
!--   N = O         terminal O,  dij < 1.30
!--   S = O         terminal O,  dij < 1.60
!--   P = O                      dij < 1.60
!--   if distances longer than limits set single bond
          IF (N1.EQ.1) THEN
            Jat = Llig(1)
            CALL PLUDIJ(Iat,Jat,D1)
            Nbt = 1
            IF(aelem(Jat).EQ.1 .AND. D1.LT.1.30)THEN
              IF (ncac(Jat).EQ.3) Nbt = 2
              IF (ncac(Jat).EQ.2 .AND. nhyc(Jat).EQ.1) Nbt = 2
              IF (ncac(Jat).EQ.2 .AND. nhyc(Jat).EQ.0) THEN
!-- for carbonyl M-C.TRIPLE.O  bond we must have a metal attached to the C
!-- as   kat-jat-iat   M-C-O
!-- look at connections to JAT for metal KAT flagged with hybr > 100
!-- If no metal then must set as C=O as in aldehyde
                Nbt = 3
                M = 0
                CALL SAMCON(Jat,Ncon,Icon,Icob,Ipib)
                DO K = 1, Ncon
                  Kat = Icon(K)
                  IF (hybr(Kat).GT.100) M = M + 1
                ENDDO
                IF (M.EQ.0) Nbt = 2
              ENDIF
            ENDIF
            IF (aelem(Jat).EQ.56 .AND. D1.LT.1.30) Nbt = 2   ! Nitrogen
            IF (aelem(Jat).EQ.81 .AND. D1.LT.1.60) Nbt = 2   ! Sulfur
            IF (aelem(Jat).EQ.66 .AND. D1.LT.1.60) Nbt = 2
            IF (Nbt.GT.0) CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
          ENDIF
!--     A - O - B       Single bonds
          IF (N1.GE.2) THEN
            DO J = 1, N1
              Jat = Llig(J)
              Nbt = 1
              CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
            ENDDO
          ENDIF
        ENDIF
!-- Sulphur
!--  S = C
        IF (aelem(Iat).EQ.81) THEN
!--  S terminal                  S=C,   S=P
          IF (N1.EQ.1) THEN
            Jat = Llig(1)
            Nbt = 2
            IF(aelem(Jat).EQ.1 .OR. aelem(Jat).EQ.66) CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
          ENDIF
!--    a - S - b
          IF (N1.GE.2) THEN
            DO J = 1, N1
              Jat = Llig(J)
              Nbt = 1
              CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
            ENDDO
          ENDIF
        ENDIF
!-- Nitrogen -       the tricky one!
        IF (aelem(Iat).EQ.56) THEN
!--
!--  N  3 connections
!--  set  single bonds. Ignore if N - metal bonds present.
!--  If Nitro group  then code  O = N = O
!--                                 |
!--
!--  Nitroso group code only     N - O   ,leave other bonds unset.
!
          IF(N1.GE.3)THEN
            Nmetal = 0
            Noxy = 0
            DO J = 1, N1
              Jat = Llig(J)
              IF(hybr(Jat).GE.100)Nmetal = Nmetal + 1
              IF(aelem(Jat).EQ.64)Noxy = Noxy + 1
            ENDDO
            IF(Nmetal.EQ.0)THEN
              DO J = 1, N1
                Jat = Llig(J)
                IF(hybr(Jat).LT.100)THEN
                  Nbt = 1
                  IF(Noxy.NE.1 .OR. aelem(Jat).EQ.64)THEN
                    IF(Noxy.GT.1 .AND. aelem(Jat).EQ.64 .AND. ncac(Jat).EQ.1)Nbt = 2
                    CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
!--
!-- N  terminal     then  check for C triple N    < 1.25
!--                                 C  =  N       < 1.32
!--                                 N TRIPLE N    < 1.20
          IF(ncac(Iat).EQ.1)THEN
            DO J = 1, N1
              Jat = Llig(J)
              CALL PLUDIJ(Iat,Jat,D1)
              IF(aelem(Jat).EQ.1)THEN
                Nbt = 1
                IF(hybr(Iat).EQ.2 .AND. D1.LT.1.32)Nbt = 2
                IF(D1.LT.1.25)Nbt = 3
                CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
              ENDIF
              IF(aelem(Jat).EQ.56)THEN
                IF(D1.LT.1.20)THEN
                  Nbt = 3
                  CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                  hybr(Iat) = 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!--
!-- Phosphorus
!-- Set single bonds.  Except if to Oxygen terminal dij < 1.50
        IF(aelem(Iat).EQ.66)THEN
          DO J = 1, N1
            Jat = Llig(J)
            CALL PLUDIJ(Iat,Jat,D1)
            Nbt = 1
            IF(aelem(Jat).EQ.64 .AND. ncac(Jat).EQ.1 .AND. D1.LT.1.50)Nbt = 2
            CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
          ENDDO
        ENDIF
!
!-- Halogen terminal
        IF(aelem(Iat).EQ.32 .OR. aelem(Iat).EQ.21 .OR. aelem(Iat).EQ.16 .OR. aelem(Iat).EQ.43 .OR.  &
     &     aelem(Iat).EQ.9)THEN
          IF(N1.EQ.1)THEN
            Jat = Llig(N1)
            Nbt = 1
            CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
          ENDIF
        ENDIF
!
!-- Boron, Si, As, Se, Te
        IF(aelem(Iat).EQ.11 .OR. aelem(Iat).EQ.85 .OR. aelem(Iat).EQ.8 .OR. aelem(Iat).EQ.84 .OR.   &
     &     aelem(Iat).EQ.92)THEN
          DO J = 1, N1
            Jat = Llig(J)
            Nbt = 1
            CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
          ENDDO
        ENDIF
!
!-- Carbon
        IF (aelem(Iat).EQ.1) THEN
          Nval = ncac(Iat) + nhyc(Iat)
          Ncc = ncac(Iat)
          Nhy = nhyc(Iat)
          Nbt = 0
          Jat = Llig(1)
!
!-- Only apply to terminal C - C         (others have been done e.g. C - O)
          IF( Ncc.EQ.1 .AND. aelem(Jat).NE.1) Ncc = -1
!-- terminal C - C  check bond length.
          IF (Ncc.EQ.1) THEN
            Jat = Llig(1)
            CALL PLUDIJ(Iat,Jat,D1)
          ENDIF
!-- terminal C  and no hydrogen, Use the bond length
          IF(Ncc.EQ.1 .AND. Nhy.EQ.0)THEN
            Nbt = 1
            IF(D1.LT.1.30)Nbt = 3
            IF(D1.GE.1.30 .AND. D1.LT.1.44)Nbt = 2
          ENDIF
!-- terminal C and 1 hydrogen - probably triple bond
          IF(Ncc.EQ.1 .AND. Nhy.EQ.1 .AND. D1.LT.1.30)Nbt = 3
!-- terminal C and 2 hydrogens - probably double bond
          IF (Ncc.EQ.1 .AND. Nhy.EQ.2 .AND. hybr(Iat).EQ.2) Nbt = 2
          IF (Nbt.GT.0) CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
!-- carbon with 4 connections - set all to single bonds
          IF (Nval.EQ.4) THEN
            DO J = 1, N1
              Jat = Llig(J)
              Nbt = 1
              CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
!
!U!-- DEBUG LIST
!U      IF(idebug.GT.0)THEN
!U        DO I = 1, natcry
!U          WRITE(Lu,99002)I, aelem(I), nhyc(I), ncac(I), atresn(I), hybr(I)
!U99002     FORMAT(I5,4I3,' hybr=',I3)
!U        ENDDO
!U        DO I = 1, nbocry
!U          WRITE(Lu,99003)I, (bond(I,K),K=1,2), btype(I)
!U99003     FORMAT(' bond ',I3,6X,3I3)
!U        ENDDO
!U      ENDIF
!-- If carboxyl groups detected, then set single bonds from carbon
      DO I = 1, nbocry
        Iat = bond(I,1)
        Jat = bond(I,2)
        Nbt = btype(I)
        IF (Nbt.EQ.2) THEN
          Kat = 0
          IF (aelem(Iat).EQ.64) Kat = Jat        ! 64 = oxygen
          IF (aelem(Jat).EQ.64) Kat = Iat
          IF (Kat.NE.0) THEN
            CALL SAMCON(Kat,Ncon,Icon,Icob,Ipib)
            IF(aelem(Kat).EQ.1 .AND. Ncon.EQ.3)THEN
!-- selected Carbon with 3 connections   x
!--                                       .
!--                                        C = O
!--                                       .
!--                                      y
              DO K = 1, Ncon
                IF(Icob(K).EQ.0)THEN
                  Lat = Icon(K)
                  Nbt = 1
                  CALL SAMSBT(Kat,Lat,Nbt,bond(1,1),bond(1,2),btype)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!--
!-- Look for flat rings.  Assign as aromatic or delocalised.
!-- Phenyls & cyclopenatdienyls  should be easy
!-- If just one N in ring then could be flagged aromatic.
!-- If other hetero-atoms or N > 1  then leave as single just now.
!--
!-- allow rings to contain unassigned and aromatic bonds (e.g. fused aromatics).
!-- First item in LTYPE array is no. of types
      Ltype(1) = 2
      Ltype(2) = 0
      Ltype(3) = 5
      DO Iat = 1, natcry
!-- search for rings which start on atoms with 2 conections.
!-- and with at least 2 bonds not yet assigned at type. NZ count zero btype.
        Nz = 0
        IF(ncac(Iat).GE.2)THEN
          CALL SAMCON(Iat,Ncon,Icon,Icob,Ipib)
          DO K = 1, Ncon
            IF(Icob(K).EQ.0)Nz = Nz + 1
          ENDDO
        ENDIF
!-- look for ring starting on atom Iat - at least 2 connections with bond type 0
!-- SAMRIQ looks for a ring  restricted to bond type 0, max size 6
        Tormax = 999.
        Torave = 999.
        IF(ncac(Iat).GE.2 .AND. Nz.GE.2)THEN
          CALL SAMRIQ(Iat,Ringat,Nring,Ltype,6)
          IF(Nring.GE.4 .AND. Nring.LE.8)THEN
            CALL SAMRIT(Ringat,Nring,Torang,Tormax)
            Aval = 0.0
            DO K = 1, Nring
              Aval = Aval + ABS(Torang(K))
            ENDDO
            Torave = Aval/FLOAT(Nring)
!-- assess ring as candidate for aromatic.
!-- count hetero atoms, metals, Nitrogen, sp3 hybrid
            Nheter = 0
            Nmetal = 0
            Nnitro = 0
            Nphosp = 0
            Nsp3 = 0
            DO K = 1, Nring
              Kat = Ringat(K)
              IF(aelem(Kat).NE.1 .AND. hybr(Kat).LT.10)Nheter = Nheter + 1
              IF(aelem(Kat).EQ.56)Nnitro = Nnitro + 1
              IF(aelem(Kat).EQ.66)Nphosp = Nphosp + 1
              IF(hybr(Kat).GT.100)Nmetal = Nmetal + 1
              IF(hybr(Kat).EQ.3 .OR. hybr(Kat).EQ.4)Nsp3 = Nsp3 + 1
            ENDDO
          ENDIF
        ENDIF
!-- check if ring is flat   - TORAVE < 10 degrees
!-- if  hetero atoms 0 or 1  then assign aromatic bond type 5.
!-- if metal involved then skip!  do not assign delocalise bond type 7.
        Iok = 0
        IF(Torave.LE.10.0 .AND. Tormax.LT.20.0)THEN
          IF(Nheter.EQ.0)Iok = 1
          IF(Nnitro.GT.0 .AND. Nheter.EQ.Nnitro)Iok = 1
          IF(Nphosp.GT.0 .AND. Nheter.EQ.Nphosp)Iok = 1
          IF(Nsp3.GT.0)Iok = 0
          IF(Nmetal.EQ.1)Iok = 0
        ENDIF
        IF(Iok.EQ.1)THEN
          Nbt = 5
          IF(Nmetal.GT.0)Nbt = 7
          DO K = 1, Nring
            I1 = Ringat(K)
            J1 = Ringat(K+1)
            IF(K.EQ.Nring)J1 = Ringat(1)
            CALL SAMSBT(I1,J1,Nbt,bond(1,1),bond(1,2),btype)
          ENDDO
        ENDIF
      ENDDO
!
!--
!-- Check pi-bond triangles             Tr
!--                                    .  .
!--                                   .    .
!--                                  .      .
!--                                 C ----- C
!--
!-- If the C---C bond has not been assigned as aromatic then set it as
!-- double bond.  Note that we do not test if both carbons pi-bond to same Tr.
!--
      IF(Npib.GT.0)THEN
        DO I = 1, nbocry
          IF(btype(I).EQ.0)THEN
            Iat = bond(I,1)
            Jat = bond(I,2)
            IF(aelem(Iat).EQ.1 .AND. aelem(Jat).EQ.1)THEN
              I9 = 0
              J9 = 0
              DO J = 1, nbocry
                IF(btype(J).EQ.9)THEN
                  IF(bond(J,1).EQ.Iat .OR. bond(J,2).EQ.Iat)I9 = 1
                  IF(bond(J,1).EQ.Jat .OR. bond(J,2).EQ.Jat)J9 = 1
                ENDIF
              ENDDO
              IF(I9.GT.0 .AND. J9.GT.0)THEN
                Nbt = 2
                btype(I) = Nbt
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!--
!-- check all atoms for unassigned bonds  code = 0
!-- make reasonable guess at bond.
!-- Allow several passes through the list, as assignment can sometimes
!-- not be complete at pass one through the atoms list
!--
      Nnot = 0
      DO Ipass = 1, 3
!-- count number of not-assigned bonds NNOT. Compare with previous pass, NNOT1.
        Nnot1 = Nnot
        Nnot = 0
        DO K = 1, nbocry
          IF(btype(K).EQ.0)Nnot = Nnot + 1
        ENDDO
        IF(Nnot.EQ.0 .OR. Nnot.EQ.Nnot1)GOTO 200
!-- loop on all atoms IAT  - get connection list for atom IAT in ICON, ICOB
        DO I = 1, natcry
          Iat = I
          CALL SAMCON(Iat,Ncon,Icon,Icob,Ipib)
!-- loop on bonds Iat - Jat
          DO M = 1, Ncon
            IF(Icob(M).EQ.0)THEN
              Nnot = Nnot + 1
              Jat = Icon(M)
              I1 = Iat
              J1 = Jat
!-- unassigned bond to metal is set as single-bond
              IF(hybr(I1).GT.100 .OR. hybr(J1).GT.100)THEN
                Nbt = 1
                CALL SAMSBT(I1,J1,Nbt,bond(1,1),bond(1,2),btype)
                GOTO 110
              ENDIF
              V = 0.
              Nz = 0
              Nmetal = 0
              DO K = 1, Ncon
                IF(Icob(K).EQ.0)Nz = Nz + 1
                IF(Icob(K).GE.1 .AND. Icob(K).LE.4)V = V + FLOAT(Icob(K))
                IF(Icob(K).EQ.5 .OR. Icob(K).EQ.7)V = V + 1.51
                IF(Icob(K).EQ.6)V = V + 1.0
                Kat = Icon(K)
                IF(hybr(Kat).GT.100)Nmetal = Nmetal + 1
                CALL PLUDIJ(Iat,Kat,Dij(K))
              ENDDO
!--                              .
!-- carbon valence check      - C         set single/double bonds
!--                              .
              IF(aelem(Iat).EQ.1)THEN
                Nbt = 0
!-- if non-planar and 4 connections - then set single bonds
!-- if non-planar and 3 connections - set any zero bonds as single
                IF(hybr(Iat).EQ.3)THEN
                  DO K = 1, Ncon
                    IF(Icob(K).EQ.0)THEN
                      Jat = Icon(K)
                      Nbt = 1
                      CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                    ENDIF
                  ENDDO
                  GOTO 110
                ENDIF
!-- if planar , v 2 or 3 or 4 ,  nz 1 or 2
!-- then we must have 1 double, 2 single
!-- set the zero-bonds with correct bondtype
!-- Do not allow b=2 if dij > 1.46
                IF(hybr(Iat).EQ.2 .AND. NINT(V).GE.2)THEN
                  Nbt = 0
                  IF(NINT(V).GE.3 .AND. Nz.EQ.1)Nbt = 1
                  IF(NINT(V).EQ.2 .AND. Nz.EQ.2)Nbt = 1
                  IF(NINT(V).EQ.2 .AND. Nz.EQ.1)Nbt = 2
                  DO K = 1, Ncon
                    IF(Icob(K).EQ.0 .AND. Nbt.GT.0)THEN
                      Jat = Icon(K)
                      IF(Nbt.EQ.2)THEN
                        CALL PLUDIJ(Iat,Jat,D1)
                        IF(D1.GT.1.46)Nbt = 1
                      ENDIF
                      CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                    ENDIF
                  ENDDO
                  GOTO 110
                ENDIF
!-- if planar, 1 single bond, 2 zero bonds.
!-- Assign double bond to shorter distance, single to longer
                IF(hybr(Iat).EQ.2 .AND. NINT(V).EQ.1)THEN
                  Nbt = 0
                  Dmin = 999.
                  Kmin = 0
                  DO K = 1, Ncon
                    IF(Icob(K).EQ.0 .AND. Dij(K).LT.Dmin)THEN
                      Dmin = Dij(K)
                      Kmin = K
                    ENDIF
                  ENDDO
                  DO K = 1, Ncon
                    Nbt = 0
                    IF(Icob(K).EQ.0 .AND. K.NE.Kmin)Nbt = 1
                    IF(Icob(K).EQ.0 .AND. K.EQ.Kmin)Nbt = 2
                    Jat = Icon(K)
                    IF(Nbt.GT.0)CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                  ENDDO
                  GOTO 110
                ENDIF
!--
!-- if hybr not known and 2 connections     C -- C -- C
!--                                         j    i    k
!--
!--     Possible triple bond - get angle. Set triple if > 160
                IF(hybr(Iat).EQ.0 .AND. Ncon.EQ.2)THEN
                  Jat = Icon(1)
                  Kat = Icon(2)
                  CALL SAMANF(Jat,Iat,Kat,Aval)
!--
!--  linear  x -- C -- y
!--
                  IF(Aval.GT.160.0)THEN
!--  C = C = C            current V=2
!
                    IF(NINT(V).EQ.2)THEN
                      Nbt = 2
                      IF(Icob(1).EQ.0)CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                      IF(Icob(2).EQ.0)CALL SAMSBT(Iat,Kat,Nbt,bond(1,1),bond(1,2),btype)
                      GOTO 110
                    ENDIF
!--  C triple C           current V=1
                    Nbt = 3
                    IF(Dij(1).LT.Dij(2))THEN
                      Icob(1) = Nbt
                      Icob(2) = 1
                    ELSE
                      Icob(1) = 1
                      Icob(2) = Nbt
                    ENDIF
                    DO K = 1, Ncon
                      Jat = Icon(K)
                      Nbt = Icob(K)
                      CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                    ENDDO
                    GOTO 110
                  ENDIF
!--      j-i-k  angle < 160   test for double bond  i-j by looking
!--      at torsion angles   involving bond  i - j
                  IF(Icob(1).EQ.0)Jat = Icon(1)
                  IF(Icob(2).EQ.0)Jat = Icon(2)
                  CALL SAMTOB(Iat,Jat,Ntor,Torang)
!--   all torsion angles in range 0 - 20   or  160-180   then b=2
!--   Do not allow double bond assign if dij > 1.46
!--                                   or hybr atom j =3
                  Nbt = 2
                  CALL PLUDIJ(Iat,Jat,D1)
                  IF(D1.GT.1.46)Nbt = 1
                  IF(hybr(Jat).EQ.3)Nbt = 1
                  DO K = 1, Ntor
                    Tor1 = ABS(Torang(K))
                    IF(Tor1.GT.20.0 .AND. Tor1.LT.160.0)Nbt = 1
                  ENDDO
                  CALL SAMSBT(Iat,Jat,Nbt,bond(1,1),bond(1,2),btype)
                  GOTO 110
                ENDIF
                IF(Nbt.EQ.0)THEN
                  IF(V.LT.4.0 .AND. V.GE.3.0)Nbt = 1
                  IF(ABS(V-2.0).LT.0.001)Nbt = 2
                ENDIF
!-- set the bond type if assigned
                IF(Nbt.GT.0)THEN
                  CALL SAMSBT(I1,J1,Nbt,bond(1,1),bond(1,2),btype)
                  GOTO 110
                ENDIF
              ENDIF
!-- nitrogen valence check = 3
!-- case of 2 connections.           x - N = y
              IF(aelem(Iat).EQ.56 .AND. Nmetal.EQ.0 .AND. Ncon.EQ.2)THEN
                Nbt = 0
                Jat = Icon(1)
                Kat = Icon(2)
!-- work out if linear (sp) or bent (sp2)
                CALL SAMANF(Jat,Iat,Kat,Aval)
!-- do not attempt to assign if linear - not x-N=y
                IF(Aval.LT.150.0)THEN
!-- hydrogens present elsewhere - therefore  x-N=y
                  IF (Hypres.GT.0) THEN
                    IF(ABS(V-2.0).LT.0.001)Nbt = 1
                    IF(ABS(V-1.0).LT.0.001)Nbt = 2
                    CALL SAMSBT(I1,J1,Nbt,bond(1,1),bond(1,2),btype)
                    GOTO 110
                  ENDIF
!-- hydrogens not present - therefore cannot tell if x-N-y  or  x-N=y
!-- if elements x y are the same and a significant difference in bond length
!-- assign double bond to shorter.
                  IF(Hypres.EQ.0)THEN
                    IF(aelem(Jat).EQ.aelem(Kat) .AND. ABS(Dij(1)-Dij(2)).GT.0.05)THEN
                      IF(Icob(1).EQ.0)THEN
                        Nbt = 1
                        IF(Dij(1).LT.Dij(2))Nbt = 2
                        CALL SAMSBT(I1,Jat,Nbt,bond(1,1),bond(1,2),btype)
                      ENDIF
                      IF(Icob(2).EQ.0)THEN
                        Nbt = 1
                        IF(Dij(2).LT.Dij(1))Nbt = 2
                        CALL SAMSBT(I1,Kat,Nbt,bond(1,1),bond(1,2),btype)
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
!-- end loop on bonds Iat-Jat
  110     ENDDO
!-- end loop on atoms IAT
        ENDDO
!-- end loop on pass of assignment
      ENDDO
!--   Stage 2.    Tidying up.
!-- set any unassigned bond types to single  b=1
  200 CONTINUE
      DO I = 1, nbocry
        IF (btype(I).EQ.0) btype(I) = 1
      ENDDO
!-- scan for functional groups and set CSD standard patterns
!-- This also assigns charges in simple cases like  Br-   Na+   ClO4-
      CALL SAMBFG
!-- Final check over for valence error on C N O S
!-- Tidy up by assigning double/single or delocalised
      DO I = 1, natcry
        CALL SAMCON(I,Ncon,Icon,Icob,Ipib)
        V = 0.
        Nz = 0
        Nmetal = 0
        DO K = 1, Ncon
          IF(Icob(K).EQ.0)Nz = Nz + 1
          IF(Icob(K).GE.1 .AND. Icob(K).LE.4)V = V + FLOAT(Icob(K))
          IF(Icob(K).EQ.5 .OR. Icob(K).EQ.7)V = V + 1.50
          IF(Icob(K).EQ.6)V = V + 1.0
          Kat = Icon(K)
          IF (hybr(Kat).GT.100) Nmetal = Nmetal + 1
        ENDDO
!-- valence check on elements  --  if problem set M=1
        M = 0
!-- carbon
        IF(aelem(I).EQ.1)THEN
          IF(hybr(I).EQ.2 .AND. NINT(V).NE.4)M = 1
          IF(NINT(V).GT.4)M = 1
        ENDIF
!-- nitrogen
        IF(aelem(I).EQ.56)THEN
          IF(hybr(I).EQ.2 .AND. NINT(V).NE.3)M = 1
          IF (NINT(V).GE.5) M = 1
        ENDIF
!-- oxygen & sulphur
        IF(aelem(I).EQ.64 .AND. NINT(V)-Nmetal.GT.2)M = 1
        IF(aelem(I).EQ.81 .AND. NINT(V)-Nmetal.GT.2)M = 1
      ENDDO
!
      END SUBROUTINE SAMABO
!*==SAMABM.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
!
      SUBROUTINE SAMABM(Npib)

      USE SAMVAR

      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Npib
      INTENT (INOUT) Npib
!
! Local variables
!
      INTEGER :: I, Iat, Ipibon, Jat, Mettot
      LOGICAL ISMET ! Function

!-- HYBR      estimate of hybridiation 1 = sp1 2=sp2 3=sp3  >100 = metal
      
!--   METTOT   metal total in cryst
!--   ICON list of atoms connected to Iat
!--   ICOB list of bondtypes for ICON
!-- Non-metals
!
      Mettot = 0
      DO I = 1, natcry
! Check if atom is a metal
        IF (ISMET(aelem(I))) THEN
! Count metals --  set hybridisation number = ncac + 100
          Mettot = Mettot + 1
          hybr(I) = ncac(I) + 100
        ELSE
! Otherwise set hybr = 0
          hybr(I) = 0
        ENDIF
      ENDDO
!--
!-- Look for pi-bonds.   Only if metal present of course.
!--                      Metals are flagged with HYBR > 100
!--
      Npib = 0
      IF (Mettot.GT.0) THEN
        DO I = 1, nbocry
          Iat = bond(I,1)
          Jat = bond(I,2)
          CALL SAMPIQ(Iat,Jat,aelem,Ipibon)
          IF (Ipibon.GT.0) THEN
            btype(I) = 9
            Npib = Npib + 1
          ENDIF
        ENDDO
!-- Metal - metal bonds all set to single btype=1
        DO I = 1, nbocry
          Iat = bond(I,1)
          Jat = bond(I,2)
          IF (hybr(Iat).GT.100 .AND. hybr(Jat).GT.100) btype(I) = 1
        ENDDO
      ENDIF

      END SUBROUTINE SAMABM
!*==SAMCO9.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
!
!
!U      SUBROUTINE SAMCO9(Iat,Ibon,Ibont,Nbonds,I9)
!U!-- Function: Check if atom Iat has a pi-bond in list IBON
!U!-- Version:  26.1.95
!U!-- Arguments:
!U!-- IAT    input test atom number
!U!-- IBON   input bond list for search
!U!-- IBONT  input bond type
!U!-- NBONDS input count of bonds in IBON
!U!-- I9     returned value = first atom found pi-bonded to Iat.  =0 no atom
!U      USE SAMVAR
!U      IMPLICIT NONE
!U!
!U! Dummy arguments
!U!
!U      INTEGER :: I9, Iat, Nbonds
!U      INTEGER, DIMENSION(MAXBND,2) :: Ibon
!U      INTEGER, DIMENSION(MAXBND) :: Ibont
!U      INTENT (IN) Iat, Ibon, Ibont, Nbonds
!U      INTENT (OUT) I9
!U!
!U! Local variables
!U!
!U      INTEGER :: I
!U!
!U!-- scan the bond list looking for pi-bond type b=9
!U      I9 = 0
!U      DO I = 1, Nbonds
!U        IF(Ibont(I).EQ.9)THEN
!U          IF(Ibon(I,1).EQ.Iat)THEN
!U            I9 = Ibon(I,2)
!U            GOTO 99999
!U          ENDIF
!U          IF(Ibon(I,2).EQ.Iat)THEN
!U            I9 = Ibon(I,1)
!U            GOTO 99999
!U          ENDIF
!U        ENDIF
!U      ENDDO
!U99999 END SUBROUTINE SAMCO9
!
!*****************************************************************************
!
!*==SAMCON.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
      SUBROUTINE SAMCON(Iat,Ncon,Icon,Icob,Ipib)
!-- Function: Get list of connected atoms for given atom Iat.
!-- Version:  27.9.94
!-- Notes:
!-- 1. This uses just the list of bonds (in any order) in IBON(*,3).
!--    Output atoms number in ICON(), count in NCON
!-- 2. Skip pi-bonds           (bond type 9 ) if IPIB <=0
!--
!-- Arguments:
!-- IAT     given atom number
!-- NCON    output number of connected atoms for Iat
!-- ICON    output list of atoms connected to IAT
!-- ICOB    output bond types for each connection in ICON
!-- IPIB    whether to include pi bonds

      USE SAMVAR

      IMPLICIT NONE

!
! Dummy arguments
!
      INTEGER :: Iat, Ipib, Ncon
      INTEGER, DIMENSION(30) :: Icon, Icob
      INTENT (IN) Iat, Ipib
      INTENT (OUT) Icob, Icon
      INTENT (INOUT) Ncon
!
! Local variables
!
      INTEGER :: I

      Ncon = 0
      DO I = 1, nbocry
        IF (Ipib.GT.0 .OR. btype(I).NE.9) THEN
          IF     (bond(I,1).EQ.Iat) THEN
            Ncon = Ncon + 1
            Icon(Ncon) = bond(I,2)
            Icob(Ncon) = btype(I)
          ELSEIF (bond(I,2).EQ.Iat) THEN
            Ncon = Ncon + 1
            Icon(Ncon) = bond(I,1)
            Icob(Ncon) = btype(I)
          ENDIF
          IF (Ncon.GE.30) THEN
            CALL DebugErrorMessage('Number of bonds to one atom > 30')
            RETURN
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE SAMCON
!
!*****************************************************************************
!
!*==SAMRIT.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
      SUBROUTINE SAMRIT(Ringat,Nring,Torang,Tormax)
!-- Function: Get torsion angles for atoms in ring given
!-- Version:  6.10.94
!-- Arguments:
!-- RINGAT   defines atom numbers for ring in sequence  1,2,3,4,5,...
!-- NRING    number of atoms in ring
!-- TORANG   output torsion angles for 1-2-3-4  2-3-4-5  etc
!-- TORMAX   output max. abs. torsion angle in ring
!--
!--             1 --- 2
!--            .       .
!--           6         3     Example of atoms for 6-membered ring
!--            .       .
!--             5 --- 4
!--
      USE SAMVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Nring
      REAL :: Tormax
      INTEGER, DIMENSION(30) :: Ringat
      REAL, DIMENSION(30) :: Torang
      INTENT (IN) Ringat
      INTENT (OUT) Torang
      INTENT (INOUT) Nring, Tormax
!
! Local variables
!
      REAL :: Aval
      INTEGER :: I, Iat, J, Jat, K, Kat, L, Lat
!
      Tormax = 0.0
      DO I = 1, Nring
        Torang(I) = 0.0
      ENDDO
      IF(Nring.LT.4)RETURN
      IF(Nring.GT.30)Nring = 30
      DO I = 1, Nring
        J = I + 1
        K = I + 2
        L = I + 3
        IF(J.GT.Nring)J = MOD(J,Nring)
        IF(K.GT.Nring)K = MOD(K,Nring)
        IF(L.GT.Nring)L = MOD(L,Nring)
        Iat = Ringat(I)
        Jat = Ringat(J)
        Kat = Ringat(K)
        Lat = Ringat(L)
        CALL SAMTOX(Iat,Jat,Kat,Lat,Aval)
        Torang(I) = Aval
        Aval = ABS(Aval)
        IF(Aval.GT.Tormax)Tormax = Aval
      ENDDO
      END SUBROUTINE SAMRIT
!*==SAMANF.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
!
      SUBROUTINE SAMANF(Iat,Jat,Kat,Aval)
!--Function:  Get  angle for i-j-k in atom list coords XO.
!--Version:  21.10.94     Sam Motherwell
!--Arguments:
!-- IAT,JAT,KAT  define atom number for torsion anngle i-j-k
!-- AVAL   returned angle in degrees
      USE SAMVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      REAL :: Aval
      INTEGER :: Iat, Jat, Kat
      INTENT (IN) Iat, Jat, Kat
!
! Local variables
!
      INTEGER :: K
      REAL, DIMENSION(3) :: X1, X2, X3

      Aval = 0.0
      IF(Iat.LE.0 .OR. Jat.LE.0 .OR. Kat.LE.0)RETURN
      DO K = 1, 3
        X1(K) = Axyzo(Iat,K)
        X2(K) = Axyzo(Jat,K)
        X3(K) = Axyzo(Kat,K)
      ENDDO
      CALL SAMANG(X1,X2,X3,Aval)
      END SUBROUTINE SAMANF
!*==SAMTOB.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
!
      SUBROUTINE SAMTOB(Iat,Jat,Ntor,Torang)
!--Function:  Get torsion angles about bond Iat-Jat
!--Version:  24.10.94     Sam Motherwell
!--Arguments:
!-- IAT,JAT input define atom number for bond in IBOC
!-- NTOR    output number of tor angles found
!-- TORANG  output list of tor angles
      USE SAMVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Iat, Jat, Ntor
      REAL, DIMENSION(30) :: Torang
      INTENT (OUT) Torang
      INTENT (INOUT) Ntor
!
! Local variables
!
      REAL :: Aval
      INTEGER, DIMENSION(30) :: Icobk, Icobl, Iconk, Iconl
      INTEGER :: Ipib, Kat, Lat, M, N, Nconk, Nconl
!
      Ntor = 0
      Ipib = -1
      IF(Iat.LE.0 .OR. Jat.LE.0)RETURN
!--  Iat - get list of connections  to atom Kat
!--  Jat - get list of connections  to atom Lat
      CALL SAMCON(Iat,Nconk,Iconk,Icobk,Ipib)
      CALL SAMCON(Jat,Nconl,Iconl,Icobl,Ipib)
!-- systematically generate torsion angles k-i-j-l
      DO N = 1, Nconk
        Kat = Iconk(N)
        IF(Kat.NE.Jat)THEN
          DO M = 1, Nconl
            Lat = Iconl(M)
            IF(Lat.NE.Iat)THEN
              IF(Kat.GT.0 .AND. Lat.GT.0)THEN
         !U       DO K = 1, 3
         !U         X1(K) = Axyzo(Kat,K)
         !U         X2(K) = Axyzo(Iat,K)
         !U         X3(K) = Axyzo(Jat,K)
         !U         X4(K) = Axyzo(Lat,K)
         !U       ENDDO
                CALL SAMTOX(Kat,Iat,Jat,Lat,Aval)
                Ntor = Ntor + 1
                Torang(Ntor) = Aval
                IF(Ntor.GE.30)GOTO 99999
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
99999 END SUBROUTINE SAMTOB
!*==SAMTOX.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
!
!
      SUBROUTINE SAMTOX(Iat,Jat,Kat,Lat,Aval)
!--Function:  Get torsion angle for i-j-k-l in atom list coords XO.
!--Version:  19.8.94     Sam Motherwell
!--Arguments:
!-- IAT,JAT,KAT,LAT  define atom number for torsion angle i-j-k-l
!-- AVAL   returned angle in degrees
      USE SAMVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      REAL :: Aval
      INTEGER :: Iat, Jat, Kat, Lat
      INTENT (IN) Iat, Jat, Kat, Lat
!
! Local variables
!
      INTEGER :: K
      REAL, DIMENSION(3) :: X1, X2, X3, X4
!
      IF(Iat.LE.0 .OR. Jat.LE.0 .OR. Kat.LE.0 .OR. Lat.LE.0)RETURN
!
      DO K = 1, 3
        X1(K) = Axyzo(Iat,K)
        X2(K) = Axyzo(Jat,K)
        X3(K) = Axyzo(Kat,K)
        X4(K) = Axyzo(Lat,K)
      ENDDO
      CALL SAMTOR(X1,X2,X3,X4,Aval)
      END SUBROUTINE SAMTOX
!*==SAMSBT.f90  processed by SPAG 6.11Dc at 13:53 on  5 Oct 2001
!
      SUBROUTINE SAMSBT(I1,J1,Nbt,Iboci,Ibocj,Ibot)
!-- Function: Set bond type code in list for given bond.
!-- Version:  4.10.94        Sam Motherwell
!-- Arguments:
!--  I1 J1   atom numbers for bond
!--  NBT     bond type code
!--  IBOC    bond list
!--  IBOT    bond type
      USE SAMVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: I1, J1, Nbt
      INTEGER, DIMENSION(MAXBND) :: Iboci, Ibocj, Ibot
      INTENT (IN) I1, Iboci, Ibocj, J1, Nbt
      INTENT (OUT) Ibot
!
! Local variables
!
      INTEGER :: I
!
      DO I = 1, nbocry
        IF((Iboci(I).EQ.I1 .AND. Ibocj(I).EQ.J1) .OR. (Iboci(I).EQ.J1 .AND. Ibocj(I).EQ.I1))THEN
          Ibot(I) = Nbt
          GOTO 99999
        ENDIF
      ENDDO
99999 END SUBROUTINE SAMSBT
!
!*****************************************************************************
!
      SUBROUTINE SAMANG(X1,X2,X3,ANGLE)
!-- Function:   Get angle in degrees x1-x2-x3
!-- Version:    10.11.94   Sam Motherwell   8.12.93
!-- Arguments:
!-- X1      coords for point X1
!-- X2      coords for point X2
!-- X3      coords for point X3
!-- ANGLE   output angle in degrees, 0.0 <= ANGLE <= 180.0
!--         on error, return negative value
!--
!-- local
!-- V, U         unit vector
!--
      USE SAMVAR
      IMPLICIT NONE
      REAL X1(3),X2(3),X3(3),ANGLE
      REAL U(3),V(3),DU,DV,COSA
      REAL RTOL
      PARAMETER (RTOL=0.000001)

      CALL SAMVEC(X2,X1,U,DU)
      CALL SAMVEC(X2,X3,V,DV)
      IF(DU.LT.RTOL .OR. DV.LT.RTOL) THEN
        ANGLE=-360.0
      ELSE
        COSA=U(1)*V(1)+U(2)*V(2)+U(3)*V(3)
        ANGLE=ACOS(COSA)
      ENDIF
      RETURN
      END
!
!*****************************************************************************
!
      SUBROUTINE SAMBFG
!-- Function:  Bond assignment for groups. Assign standard patterns
!--            to  certain functional groups as in CSD
!-- Version:   29.9.95           Sam Motherwell   18.9.95
!-- Notes:
!-- 1. Auxiliary to SAMABO.  This is called in the stage 2 (Tidy Up)
!--    All connectivity has been set up in  
!--    AELEM    element type 
!--    NHYC     number of terminal H 
!--    NCAC     number of connections other than terminal H 
!--    ATRESN   residue number 
!--    BOND     list of bonds Iat, Jat   
!      BTYPE    nbt          
!--    NATCRY   number of atoms
!--    NBOCRY   number of bonds 
!--
!-- 2. scan is made for those functional groups known to give 
!--    trouble with the automatic bond assignment in SAMABO. For
!--    example  carboxylate.  Also groups like perchlorate  ClO4- are 
!--    explicity recognised and assigned a pattern of 3 double, one single bond
!--
!-- 3. Charges.  In some simple cases one can deduce an ionic change.
!--    For example,   Na +    Cl-   or  ClO4 -      NR4 + 
!--    The charge is assigned to an array  ATCHG as output COMMON PLUTAC
!--   
      USE SAMVAR
      IMPLICIT NONE

!-- HYBR      estimate of hybridisation 1 = sp1 2=sp2 3=sp3  >100 = metal
!--
!-- ATVAL     atom valency - temporary use for checking
      INTEGER ATVAL

!-- local 
      INTEGER  I,J,K,M,CHGMIN,CHGPLU,IAT,JAT,KAT,KOXY,KMETAL
      INTEGER  JMIN,JMAX,NBT,ICASE,NZ

      INTEGER  ICON(30),JCON(30),ICOB(30),JCOB(30),NCON,MCON
      INTEGER  ATLIST(30),NLIST,KLIST,JLINK,IPIB
      REAL V,DMIN,DMAX,DIST(30),D1
!-------------------------------------------------------

!-- ignore pi-bonds
      IPIB=-1
!--
!-- look for single atom residues and assign charge if needed
!-- Skip suppressed atoms  which have iarc = -1 
!--
      CHGMIN = 0
      CHGPLU = 0
      DO I = 1, NATCRY
        ATCHG(I) = 0.0
        IF (NCAC(I).EQ.0) THEN 
          SELECT CASE(AELEM(I))
            CASE (32,21,16,43)       ! F Cl Br I          (-1)
              ATCHG(I) = -1.0
              CHGMIN = CHGMIN - 1
            CASE (49,57,46,76,25)    ! Li Na K  Rb Cs     (+1)
              ATCHG(I) = 1.0
              CHGPLU = CHGPLU + 1
            CASE (13,53,17,88,12,75) ! Be Mg Ca Sr Ba Ra  (+2)
              ATCHG(I) = 2.0
              CHGPLU = CHGPLU + 2
          END SELECT
        ENDIF
      ENDDO
!--
!--
!-- Loop through all atoms  --  look for specified groups 
!--
!--
      DO 500 I = 1, NATCRY
!--
!--  quaternary N      e.g    -NH3 +    NH4 +    c-NH2-c   HN- (C)3   N-(C)4
!--                    This does not apply if connected N - metal    
!--
      IF(AELEM(I).EQ.56 .AND. HYBR(I).EQ.3) THEN 
        IAT=I
        CALL SAMCON(IAT,NCON,ICON,ICOB,IPIB)
        KMETAL=0
        DO 110 J=1,NCON
          JAT=ICON(J)
          IF(HYBR(JAT).GT.100) KMETAL=KMETAL+1
110     CONTINUE
        IF(KMETAL.EQ.0 .AND. NCON.EQ.4)THEN 
          ATCHG(I) = +1.0
          CHGPLU = CHGPLU + 1
          ENDIF
        ENDIF
!--
!-- Planar nitrogen with 3 connections and valence 4       C = N (R)2
!--
!--                                                        C = NH2
!--
      
      IF((AELEM(I).EQ.56 .AND. NCAC(I).EQ.3 .AND. HYBR(I).EQ.2).OR.(AELEM(I).EQ.56 .AND. NCAC(I).EQ.1 .AND. NHYC(I).EQ.2) ) THEN 

        IAT=I
        CALL SAMCON(IAT,NCON,ICON, ICOB,IPIB)
        KMETAL=0
        V=0.0
        DO 115 J=1,NCON
        JAT=ICON(J)
        IF(HYBR(JAT).GT.100) KMETAL=KMETAL+1
        IF(ICOB(J).EQ.7) THEN
          V=V+1.51
        ELSEIF(ICOB(J).EQ.5) THEN
          V=V+1.34
        ELSE
          V=V+FLOAT(ICOB(J))
        ENDIF
115     CONTINUE   
        ICASE=0
        IF(KMETAL.EQ.0 .AND. NINT(V).EQ.4  .AND. NCAC(JAT).EQ.3) ICASE=1
        IF(KMETAL.EQ.0 .AND. NINT(V).EQ.4  .AND. NHYC(JAT).EQ.2) ICASE=2
        IF(ICASE.GT.0) THEN 
          ATCHG(I) = +1.0
          CHGPLU = CHGPLU + 1
          ENDIF
        ENDIF
!--
!-- Thiocyanate. 
!--
      IF(AELEM(I).EQ.1 .AND. NCAC(I).EQ.2) THEN
        IAT=I
        CALL SAMCON(IAT,NCON,ICON, ICOB,IPIB)
        JAT=ICON(1)
        KAT=ICON(2)
        KOXY=0
!-- see if C bound to N and S alone
        IF(AELEM(JAT).EQ.56 .AND. AELEM(KAT).EQ.81) THEN
          KOXY=KAT
        ELSEIF(AELEM(KAT).EQ.56 .AND. AELEM(JAT).EQ.81) THEN
          KOXY=JAT
!-- swap KAT, JAT
          JAT=KAT
          KAT=KOXY          
        ENDIF
!-- check N and S monocoordinate
        IF(KOXY.GT.0 .AND. NCAC(JAT).EQ.1 .AND. NHYC(JAT).EQ.0 .AND. NCAC(KAT).EQ.1 .AND. NHYC(KAT).EQ.0) THEN
!-- set charge and standard bond patterns
          ATCHG(KAT)=-1
          CHGMIN=CHGMIN-1
!-- S-C bond single
          NBT=1
          CALL SAMSBT(IAT,KAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
!-- C-N bond triple
          NBT=3
          CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE) 
        ENDIF
      ENDIF         
          
!--
!-- Carboxylate.        detect    a - C - O        A - C - S
!-- Thiocarboxylate                   |                |
!--                                   O                S
!--
!--                  this can include carbonate   CO3-- 
!-- If C connected atoms not 3  , or not planar, then skip 
!-- Check the valence of the carbon by counting bond type 1 & 2 
      IF(AELEM(I).EQ.1 .AND. NCAC(I).EQ.3 .AND. HYBR(I).EQ.2)THEN 
        IAT=I
        CALL SAMCON(IAT,NCON,ICON, ICOB,IPIB)
        V=0.0
        KOXY=0
        DO 155 J=1,NCON
        JAT=ICON(J)
        IF(AELEM(JAT).EQ.64 .OR. AELEM(JAT).EQ.81) KOXY=KOXY+1
        V=V+FLOAT(ICOB(J))
155     CONTINUE
        IF( KOXY.GE.2) THEN 
!--   Carboxylate detected. 
!--   Now look for a metal connected to either 
!--   oxygen.  A metal is detected by the HYBR > 100.   
          KMETAL=0
          DO 160 J=1,NCON
          JAT=ICON(J)
          CALL SAMCON(JAT,MCON,JCON, JCOB,IPIB)
          DO 165 K=1,MCON
          KAT=JCON(K)
          IF(HYBR(KAT).GT.100) KMETAL=KMETAL+1
165       CONTINUE
160       CONTINUE
          ENDIF
      ICASE=0
      IF(KOXY.GE.2 .AND. NINT(V).NE.4 .AND. KMETAL.LE.1) ICASE=1
      IF(KOXY.GE.2 .AND. NINT(V).NE.4 .AND. KMETAL.GE.2) ICASE=2     

!--
!-- carboxylate  case 1.    Metal-O  = 0   or  =1     
!-- set carboxylate  as double/single bonds     O = C - O (- charge)
!-- with C=O assigned to the shortest bond. 

      IF(ICASE.EQ.1) THEN 
         DMIN=999.
         DMAX=-999.
         JMIN=1
         JMAX=1
         DO 170 J=1,NCON
         JAT=ICON(J)
         CALL PLUDIJ(IAT,JAT,DIST(J))
         IF(AELEM(JAT).EQ.64 .OR. AELEM(JAT).EQ.81) THEN 
           IF(DIST(J).LT.DMIN) THEN 
              JMIN=JAT
              DMIN=DIST(J)
              ENDIF
           IF(DIST(J).GT.DMAX) THEN 
              JMAX=JAT
              DMAX=DIST(J)
              ENDIF
           ENDIF
170      CONTINUE
!-- set a double bond for shorter C-O  
         NBT=2
         CALL SAMSBT(IAT,JMIN,NBT,BOND(1,1),BOND(1,2),BTYPE)
!-- set single bond for longer C-O    and charge -1 on O
         NBT=1
         CALL SAMSBT(IAT,JMAX,NBT,BOND(1,1),BOND(1,2),BTYPE)
         IF (NCAC(JMAX).LE.1 .AND. NHYC(JMAX).LE.0) THEN
            ATCHG(JMAX)=-1
            CHGMIN=CHGMIN-1
         ENDIF
         ENDIF
        
!--
!-- carboxylate case 2.    Metal connection at  both oxygens
!--                        Set delocalised bond b=7
!--
      IF(ICASE.EQ.2) THEN 
        DO 175 J=1,NCON
        JAT=ICON(J)
        IF(AELEM(JAT).EQ.64 .OR. AELEM(JAT).EQ.81) THEN 
          NBT=7
          CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
          ENDIF
175     CONTINUE
        ENDIF
!-- end of carboxylate section
       ENDIF
!--
!--
!-- CLO4 -    perchlorate.  Set 3 bonds double, one single with charge -1 on O
!--                         If there is an Oxygen with more than one 
!--                         connection, give it the single bond,
!--                         and only apply a negative charge if exactly 1 
!--                         connection.
!--
!--                         This whole code is actually still too selective,
!--                         as the assignment of charge is done only
!--                         if there is a valency error at the root atom.
!--                         In cases where bonds are assigned correctly,
!--                         the charge can never be set.
!--
      IF(AELEM(I).EQ.21 .AND. NCAC(I).EQ.4) THEN 
        IAT=I
        KOXY=0
        CALL SAMCON(IAT,NCON,ICON,  ICOB,IPIB)
        JLINK=0
        DO 180 J=1,NCON
        JAT=ICON(J)
        IF(AELEM(JAT).EQ.64) THEN
           KOXY=KOXY+1
           IF(NCAC(JAT).GT.1)JLINK=JAT
           IF(JLINK.EQ.0 .AND. J.EQ.NCON) JLINK=JAT
        ENDIF
180     CONTINUE
        IF(KOXY.EQ.4) THEN 
          DO 185 J=1,NCON
          JAT=ICON(J)
          NBT=2
          IF(JAT.EQ.JLINK) NBT=1
          CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
          IF(JAT.EQ.JLINK .AND. NCAC(JAT).EQ.1 .AND.   NHYC(JAT).EQ.0) THEN 
             ATCHG(JAT)=-1
             CHGMIN=CHGMIN-1
             ENDIF
185       CONTINUE
          ENDIF
         ENDIF

!--
!-- BF4 -         set charge -1 on B 
!--
      IF(AELEM(I).EQ.11 .AND. NCAC(I).EQ.4) THEN 
         IAT=I
         CALL SAMCON(IAT,NCON,ICON,   ICOB,IPIB)
         K=0
         DO 190 J=1,NCON
         JAT=ICON(J)
         IF(AELEM(JAT).EQ.32) K=K+1
190      CONTINUE
         IF(K.EQ.4) THEN 
            ATCHG(IAT)=-1
            CHGMIN=CHGMIN-1
            ENDIF
         ENDIF
!--
!-- NO3 - 
!--        If there is an Oxygen with more than one 
!--        connection, give it the single bond,
!--        and only apply a negative charge if exactly 1 
!--        connection.
!--
!--        This whole code is actually still too selective,
!--        as the assignment of charge is done only
!--        if there is a valency error at the root atom.
!--        In cases where bonds are assigned correctly,
!--        the charge can never be set.
!--
      IF(AELEM(I).EQ.56 .AND. HYBR(I).EQ.2 .AND.  NCAC(I).EQ.3) THEN 
         IAT=I
         CALL SAMCON(IAT,NCON,ICON,  ICOB,IPIB)
         KOXY=0
         JLINK=0
	 JMAX=0
	 DMAX=-99.9
         D1=-99.9
         DO 195 J=1,NCON
         JAT=ICON(J)
         IF(AELEM(JAT).EQ.64) THEN
            KOXY=KOXY+1
	    CALL PLUDIJ(IAT,JAT,DIST(J))
! find the longest bond to an oxygen with two connections
	    IF(DIST(J).GT.DMAX) THEN
	      DMAX=DIST(J)
	      JMAX=JAT
	    ENDIF
            IF(NCAC(JAT).GT.1) THEN
	      IF(DIST(J).GT.D1) THEN
	        JLINK=JAT
		D1=DIST(J)
	      ENDIF
	    ENDIF
! find the longest bond if all oxygens have only one connection
	    IF(JLINK.EQ.0 .AND. J.EQ.NCON) JLINK=JMAX
         ENDIF
195      CONTINUE
         IF(KOXY.EQ.3) THEN 
           DO 196 J=1,NCON
           JAT=ICON(J)
           NBT=2
           IF(JAT.EQ.JLINK) NBT=1
           CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
           IF(JAT.EQ.JLINK .AND. NCAC(JAT).EQ.1 .AND.   NHYC(JAT).EQ.0) THEN 
             ATCHG(JAT)=-1
             CHGMIN=CHGMIN-1
             ENDIF
196        CONTINUE
           ENDIF
         ENDIF
!--
!-- SO3   AND SO4 --      Count terminal oxygens. 
!--                       Assign S=O to first two,   S-O (minus) to rest
!--
!--                       Note that this is not selective enough, as the
!--                       order of bonds to S is arbitrary.
!--
      IF(AELEM(I).EQ.81 .AND. NCAC(I).EQ.4) THEN 
         IAT=I
         CALL SAMCON(IAT,NCON,ICON,ICOB,IPIB)
         DO 200 J=1,NCON
         JAT=ICON(J)
         IF(AELEM(JAT).EQ.64  .AND. NCAC(JAT).EQ.1) KOXY=KOXY+1
200      CONTINUE
         IF(KOXY.GE.3) THEN 
          DO 205 J=1,NCON
          JAT=ICON(J)
          IF(AELEM(JAT).NE.64) GOTO 205
          NBT=2
          IF(J.GE.3) NBT=1
          CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
!         (but we do not know if the first two were oxygens!)
          IF(J.GE.3 .AND. NHYC(JAT).EQ.0) THEN 
              ATCHG(JAT)=-1
              CHGMIN=CHGMIN-1
              ENDIF
205       CONTINUE
          ENDIF
         ENDIF
!--
!-- PF6 (-)      
!--
        IF(AELEM(I).EQ.66 .AND. NCAC(I).EQ.6) THEN
           ATCHG(I)=-1
           CHGMIN=CHGMIN-1
           ENDIF      
!--
!-- diazo group          C - N (+) triple N         diazonium salt (case 1)
!--                      C = N (+) = N (-)          diazo          (case 2)
!--
        IF(AELEM(I).EQ.56 .AND. NCAC(I).EQ.1  .AND. HYBR(I).EQ.1) THEN 
         IAT=I
         CALL SAMCON(IAT,NCON,ICON,   ICOB,IPIB)
         K=0
         DO 206 J=1,NCON
         JAT=ICON(J)
         IF(AELEM(JAT).EQ.56 .AND. ICOB(J).EQ.3) K=1
206      CONTINUE
!-- we take a look at the bond to the Carbon (kat).  If this is 
!-- a single bond then leave it - and set case 1
!-- if a double  then  set as case 2
         ICASE=0
         IF(K.EQ.1) THEN 
           ICASE=1
           CALL SAMCON(JAT,MCON,JCON,  JCOB,IPIB)
           DO 207 J=1,MCON
           KAT=JCON(J)
           IF(JCOB(J).EQ.2)  ICASE=2 
207        CONTINUE
           ENDIF

         IF(ICASE.EQ.1) THEN 
            ATCHG(JAT)=1
            ATCHG(IAT)=0
            CHGPLU=CHGPLU+1
            ENDIF
         IF(ICASE.EQ.2) THEN 
            ATCHG(JAT)=1
            ATCHG(IAT)=-1
            NBT=2
            CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
            ENDIF
         ENDIF

!-- end loop on all atoms -- fixing secified groups 
500   CONTINUE
!-- 
!-- check over for valence error on C N O S
!--
!-- Fix up any delocalised C bonding problems 
!--
      DO 550 I=1,NATCRY        
        CALL SAMCON(I,NCON,ICON,ICOB,IPIB)
        V=0.
        NZ=0
        KMETAL=0
        DO 805 K=1,NCON
          IF (ICOB(K).EQ.0) NZ=NZ+1
          IF (ICOB(K).GE.1 .AND. ICOB(K).LE.4) THEN
            V=V+FLOAT(ICOB(K))
          ELSEIF (ICOB(K).EQ.5 .OR. ICOB(K).EQ.7) THEN 
            V=V+1.50
          ELSEIF (ICOB(K).EQ.6) THEN
            V=V+1.0
          ENDIF
          KAT=ICON(K)
          IF (HYBR(KAT).GT.100) KMETAL=KMETAL+1
 805    CONTINUE
!-- save integer value for valence.  Ensure that 4.500   = 4 integer 
        ATVAL=NINT(V - 0.001)
!-- valence check on elements  --  if problem set M=1
        M=0
!-- carbon
        IF (AELEM(I).EQ.1) THEN
          IF (HYBR(I).EQ.2 .AND. NINT(V).NE.4) M=1
          IF (NINT(V).GT.4) M=1
        ENDIF
!-- nitrogen
        IF (AELEM(I).EQ.56) THEN
          IF (HYBR(I).EQ.2 .AND. NINT(V).NE.3) M=1
          IF (NINT(V).GE.5) M=1
        ENDIF
!-- oxygen & sulphur
        IF (AELEM(I).EQ.64 .AND. NINT(V)-KMETAL.GT.2) M=1
        IF (AELEM(I).EQ.81 .AND. NINT(V)-KMETAL.GT.2) M=1
!--
!-- detect Carbon valency problems.   These indicate delocalisation
!--
!-- One of the most common examples is acetylacetonate ligands. The 
!-- principle is to fix the problem at the atom flagged by setting 
!-- delocalised bonds type b=7  for bond dij < 1.40   
!-- Then work outward to beta-atoms, and set any double bonds to delocalised
!-- and any to hetero atoms  O or N 
            
         
!-- carbon only 
        IF (AELEM(I).EQ.1) THEN
          ICASE=0
          IAT=I
!-- check for valence 3  on   planar C
          IF (HYBR(I).EQ.2 .AND. ATVAL.NE.4) ICASE=1
!-- check for valence 5 
          IF (ATVAL.EQ.5) ICASE=2
!-- check for    C = C = C    and no hybridisation state known 
!-- count the double bonds b=2
          IF (HYBR(I).EQ.0 .AND. NHYC(I).EQ.0  .AND. NCAC(I).EQ.2) THEN 
            K=0
            KMETAL=0
            CALL SAMCON(IAT,NCON,ICON,  ICOB,IPIB)
            DO 210 J=1,NCON
            JAT=ICON(J)
            IF(HYBR(JAT).GT.100) KMETAL=KMETAL+1
            IF(ICOB(J).EQ.2) K=K+1
210         CONTINUE
            IF(K.GT.1)  ICASE=3
            IF(KMETAL.GT.0) ICASE=0
            ENDIF
!-- if C valence > 5   then probably in a metal cluster. Leave alone
          IF(ATVAL.GT.5) ICASE=0

!-- first set delocalised b=7 for short bonds on this atom Iat
!-- Thus   C-C=C      or  C=C=C     becomes    C..C..C
!-- Make a note of the alpha-atoms in the list ATLIST
!-- Exclude H atoms    el=2 from any delocalised net. 
!-- Stop delocalisation if C - N    el=56  
!-- Stop delocalisation if C - Metal bond found 
!-- Stop delocalise if aromatic bond found  b=5 
          IF(ICASE.GT.0) THEN 
            CALL SAMCON(IAT,NCON,ICON,   ICOB,IPIB)
            DO 218 J=1,NCON
            JAT=ICON(J)
            IF(AELEM(JAT).EQ.56 )  ICASE=-1
            IF(HYBR(JAT).GT.100)  ICASE=-1
            IF(ICOB(J).EQ.5)  ICASE=-1 
218         CONTINUE

            NLIST=0
            KLIST=0
            IF(ICASE.GT.0) THEN 
              DO 220 J=1,NCON
              JAT=ICON(J)
              CALL PLUDIJ(IAT,JAT,D1)
              IF(D1.LT. 1.450  .AND. AELEM(JAT).NE.2)  THEN 
                NBT=7
                CALL SAMSBT(IAT,JAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
                NLIST=NLIST+1
                ATLIST(NLIST)=JAT
                ENDIF
220           CONTINUE
              ENDIF
!--
!-- now do the same for the alpha atoms, extending the b=7 network
!-- the only candidates are bonds with d<1.450 
!-- This by good luck stops expansion to metal centres, or to non-conjugated
!-- bonds.  The distance 1.450  may need some experimental adjustment. 
!--
            IF(NLIST.GT.0) THEN 
               KLIST=0
               DO 230 J=1,NLIST
               JAT=ATLIST(J)
               CALL SAMCON(JAT,MCON,JCON,JCOB,IPIB)
               DO 240 K=1,MCON
               IF(JCOB(K).EQ.7) GOTO 240
               KAT=JCON(K)
               CALL PLUDIJ(JAT,KAT,D1)
               
               IF(D1.LT. 1.450  .AND. AELEM(KAT).NE.2) THEN 
                  NBT=7
                  CALL SAMSBT(JAT,KAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
                  KLIST=KLIST+1
                  ATLIST(NLIST+KLIST)=KAT
                  ENDIF
240            CONTINUE
230            CONTINUE
               ENDIF
!-- if beta atoms then repeat extension process ( and thats as far as we go)
   
            IF(KLIST.GT.0) THEN 
               DO 250 J=1,KLIST
               JAT=ATLIST(J+NLIST)
               CALL SAMCON(JAT,MCON,  JCON,JCOB,IPIB)
               DO 260 K=1,MCON
               IF(JCOB(K).EQ.7) GOTO 260
               KAT=JCON(K)
               CALL PLUDIJ(JAT,KAT,D1)
               IF(D1.LT. 1.450 .AND. AELEM(KAT).NE.2) THEN 
                  NBT=7
                  CALL SAMSBT(JAT,KAT,NBT,BOND(1,1),BOND(1,2),BTYPE)
                  ENDIF
260            CONTINUE
250            CONTINUE
               ENDIF
            ENDIF
!-- end of section for Carbon delocalised
           ENDIF
550   CONTINUE
!--
!-- Final assignmemt of balancing charge to metal atom(s)
!--
!-- In cases with no metal  like  NR4(+)   Cl(-)   all OK 
!-- Cases like    ClO4(-)   and Cu      assign  (+1) to the Cu 
!--
!-- Count number of neutral metals, and split charge evenly among them.
!-- Do not allow balancing charge -ve on metals, must be +1 , +2, etc
!-- 
      K=CHGPLU+CHGMIN
      J=K
      IF (K.LT.0 ) THEN 
        KMETAL=0
        J=IABS(K)
        DO 555 I=1,NATCRY
        IF(HYBR(I).GT.100 .AND. ATCHG(I).EQ.0) THEN
	  CALL SAMCON(I,NCON,ICON, ICOB,IPIB)
          DO 556 K=1,NCON
	    IF(ICOB(K).NE.6) GOTO 557
556       CONTINUE
!-- if here, no non-polymeric bonds, so omit this metal.
          HYBR(I)=-IABS(HYBR(I))
          GOTO 555
557       CONTINUE
          KMETAL=KMETAL+1
        ENDIF
555     CONTINUE
        IF(KMETAL.GT.0) THEN 
          M=J/KMETAL  
          IF(M.EQ.0) M=1
          DO 560 I=1,NATCRY
          IF(HYBR(I).GT.100) THEN 
            ATCHG(I)=M
            J=J-M
            IF (J.LE.0) GOTO 561
          ENDIF
560       CONTINUE
561       CONTINUE
          ENDIF
        ENDIF
      IF(J.NE.0) THEN 
!        IF(IDEBUG.GT.0)WRITE(LU,*)'WARNING - unbalanced charge sum =', J
      ENDIF

      END
!
!*****************************************************************************
!
      SUBROUTINE SAMTOR(XI,XJ,XK,XL,OMEGA)
!-- Function: Calculate torsion angle for a set of 4 atom coords.
!-- Version:  10.11.94  1.2.94        Sam Motherwell (based on TORANG)
!-- Notes:
!-- 1. The angle returned OMEGA is in degrees.
!--          I         L
!--           \       /
!--            J --- K
!--    When viewed in direction j > k, the angle omega is the rotation
!--    required to bring the projected line i-j to overlie k-l.
!--    Clockwise rotation gives +ve omega.
!--
!-- 2. If i-j-k or j-k-l  are colinear groups an indeterminate situation
!--    occurs.  A value of 0.0 is returned for Omega.
!--
!-- Arguments:
!-- XI,XJ,XK,XL   input orthog. coordinates for atoms i,j,k,l
!-- OMEGA         output torsion angle, in degrees
      USE SAMVAR
      IMPLICIT NONE
      REAL XI(3),XJ(3),XK(3),XL(3),OMEGA
      REAL VIJ(3),VJK(3),VKL(3),R(3),S(3),T(3),COSW,TP
      INTEGER N

      DO 100 N=1,3
        VIJ(N)=XJ(N)-XI(N)
        VJK(N)=XK(N)-XJ(N)
        VKL(N)=XL(N)-XK(N)
 100  CONTINUE
      CALL VPROD(VIJ,VJK,R)
      CALL VPROD(VJK,VKL,S)
      CALL VPROD(R,S,T)
      COSW=R(1)*S(1)+R(2)*S(2)+R(3)*S(3)
      TP=VJK(1)*T(1)+VJK(2)*T(2)+VJK(3)*T(3)
      IF (COSW.GT.1.00000) COSW=1.0
      IF (COSW.LT.-1.000000) COSW=-1.0
      OMEGA=ACOS(COSW)
      IF (TP.LT.0.0) OMEGA=-OMEGA
      RETURN
      END
!
!*****************************************************************************
!
      SUBROUTINE SAMCC3
!-- Function:  set up 3D connectivity arrays in PLUTQY common
!--            using MV arrays as input  
!-- Version:   26.9.95                 Sam Motherwell    26.9.95
!-- Notes:
!-- 1. This is useful in several places in Pluto / Prequest.  It works
!--    on the 3D crystallographic connectivity data.   
!--    Input is the list of atoms   1:Tatom
!--    and  list of bonds BOND(*,2) and bond-types BTYPE().    1:Tbond
!--    Output:
!--          NHYC    number of terminal hydrogens
!--          NCAC    number of connections (excluding terminal H)
!--
      USE SAMVAR

      IMPLICIT NONE

      INTEGER I, IAT, JAT
!-- 
!-- set up arrays for 3D crystal connectivity
!--
!-- NATCRY number of atoms, NBOCRY number of bonds
      NATCRY = tatom
      NBOCRY = tbond
!-- Initialise number of connections NCAC = 0, number of terminal hyds NHYC = 0
      NCAC = 0
      NHYC = 0
!-- process the bonds, setting number of connections NCAC exclude terminal H
!-- Omit any bonds involving suppressed atoms with incl < 0
      DO I = 1, nbocry
        IAT = BOND(I,1)
        JAT = BOND(I,2)
        IF (AELEM(JAT).NE.2) NCAC(IAT) = NCAC(IAT)+1
        IF (AELEM(IAT).NE.2) NCAC(JAT) = NCAC(JAT)+1
!-- if BOTH atoms are H include their mutual connections
!-- otherwise H's are incorrectly assumed to be terminal
        IF((AELEM(IAT).EQ.2) .AND. (AELEM(JAT).EQ.2)) THEN
          CALL DebugErrorMessage('Hydrogen gas discovered in crystal structure / z-matrix')
          NCAC(JAT)=NCAC(JAT)+1
          NCAC(IAT)=NCAC(IAT)+1
        ENDIF 
      ENDDO
!-- set number of terminal H, element code hydrogen = 2
!-- Detect bridge H here, and add this bond to the ncac for the atoms bridged.
      DO I = 1, nbocry
        IAT = BOND(I,1)
        JAT = BOND(I,2)
        IF (AELEM(IAT).EQ.2 .AND. NCAC(IAT).EQ.1) NHYC(JAT) = NHYC(JAT)+1
        IF (AELEM(JAT).EQ.2 .AND. NCAC(JAT).EQ.1) NHYC(IAT) = NHYC(IAT)+1
        IF (AELEM(IAT).EQ.2 .AND. NCAC(IAT).GE.2) NCAC(JAT) = NCAC(JAT)+1
        IF (AELEM(JAT).EQ.2 .AND. NCAC(JAT).GE.2) NCAC(IAT) = NCAC(IAT)+1
      ENDDO

      END SUBROUTINE SAMCC3
!
!*****************************************************************************
!
      SUBROUTINE VPROD(A,B,C)
! OJ100 from GSTAT
! Calculate C, the vector product of A & B normalised to unit length
!
      IMPLICIT NONE
!     Passed arguments:
      REAL A(3),B(3),C(3)
!     Local variables:
      INTEGER I
      REAL D,DSQ

      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      DO  I=1,3
       IF(ABS(C(I)).LT.1.E-15)C(I)=0.0
      ENDDO
      DSQ=C(1)*C(1)+C(2)*C(2)+C(3)*C(3)
      IF(DSQ.LT.0.00001)RETURN
      D=SQRT(DSQ)
      C = C / D

      END
!
!*****************************************************************************
!
      SUBROUTINE SAMVEC(X1,X2,V,D12)
!-- Function:   Get unit vector between points X1 and X2
!-- Version:    9.11.94          Sam Motherwell  8.12.93
!-- Arguments:
!-- X1      coords for point X1
!-- X2      coords for point X2
!-- V       output unit vector (in sense   X1  -->  X2)
!-- D12     output distance X1 - X2
      IMPLICIT NONE
!--
      REAL X1(3),X2(3),V(3)
      REAL D12, DTOL
      PARAMETER (DTOL=0.000001)

      V(1)=X2(1)-X1(1)
      V(2)=X2(2)-X1(2)
      V(3)=X2(3)-X1(3)
      D12=SQRT(V(1)*V(1)+V(2)*V(2)+V(3)*V(3))
      IF (D12.LT.DTOL) THEN
        V(1)=1.
        V(2)=0.
        V(3)=0.
      ELSE
        V = V / D12
      ENDIF

      END
!
!*****************************************************************************
!
      LOGICAL FUNCTION ISMET(IELEM)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: IELEM
!
!-- Non-metals
      INTEGER NONMET(23), J
!--                H   He  B   C   N   O   F   Ne  Si  P   S   Cl  Ar  As  Se  Br  Kr  Te  I   Xe  At  Rn  D
      DATA NONMET/  2, 39, 11,  1, 56, 64, 32, 60, 85, 66, 81, 21,  7,  8, 84, 16, 47, 92, 43,101,  9, 79, 27/

      IF (IELEM .EQ. 0) THEN
        CALL DebugErrorMessage('Element = 0 in ISMET()')
        ISMET = .FALSE.
        RETURN
      ENDIF
      ISMET = .TRUE.
      DO J = 1, 23
        IF (IELEM.EQ.NONMET(J)) THEN 
          ISMET = .FALSE.
          RETURN
        ENDIF
      ENDDO

      END FUNCTION ISMET
!
!*****************************************************************************
!
      SUBROUTINE SAMRIQ(IAT1,RINGAT,NRING,IBTYPE,MAXRNG)
!--
!-- Function: Find smallest ring starting from IAT, with bond type check.
!-- Version:  24.2.95  8.12.94   13.10.94       Sam Motherwell
!-- Notes:
!-- 1. Atom numbers of the ring are entered in RINGAT, count NRING.
!-- 2. MAXRNG  The routine will find rings up to maximum MAXRNG (input)
!-- 3. IBTYPE > 0 on input then restrict search to rings with
!--    given bondtype  e.g.  1,   5,    100  means bond type 0
!--
!-- Arguments:
!--  IAT1      start atom number for tree
!--  RINGAT() output atom number for ring if found
!--  NRING    output number of atoms found in RINGAT
!--  IBTYPE() control option. If (1) > 0 then restrict ring to this bond types
!--           in this array.
!--  MAXRNG   control option. restrict search to this max. ring size.

      USE SAMVAR

      IMPLICIT NONE

      INTEGER IAT1, RINGAT(30),NRING,IBTYPE(*),MAXRNG
!-- local
!-- IPT   point to connection in use for ringat(n)
!-- IFROM atom number from which we came to current ringat
      INTEGER IPT(30),IFROM(30),LLIG(30),LMIG(30)
      INTEGER L,N,NCX,IBACK,ITRY,IAT,JAT,NBTEST,CAT,IPIB
!--------------------------------------------------------------------
!--
!-- search through the table for ring starting from IAT - a ring is detected
!-- when a growth point atom = the start atom Iat.
!-- Max size of the ring is limited by the value MAXRNG
!-- RINGAT is set to the trial atoms for the ring
!-- IPT    points to the connection in use for current RINGAT
!-- IFROM  points to atom from which we reached current RINGAT
!-- N      is current number of atoms in RINGAT
 
      NBTEST=IBTYPE(1)
      RINGAT(1)=IAT1
      IFROM(1)=0
      IPT(1)=0
      N=1
      CAT=0
!-- ignore pi-bonds
      IPIB=-1
!--
      DO 500 ITRY=1,999999
        IAT=RINGAT(N)
        IF(IAT.NE.CAT) THEN
          CALL SAMCON(IAT,NCX,LLIG,LMIG,IPIB)
!-- record atom for which we have connections in LLIG array
          CAT=IAT
        ENDIF
        IPT(N)=IPT(N)+1
        IF (IPT(N).GT.NCX) THEN
          IBACK=1
        ELSE
          JAT=LLIG(IPT(N))
          IF (JAT.EQ.IFROM(N)) GOTO 500
!-- reject if bond type not as required
          IF (NBTEST.GT.0) THEN
            DO 410 L=2,NBTEST+1
              IF(IABS(LMIG(IPT(N))).EQ.IBTYPE(L)) GOTO 420
  410       CONTINUE
!-- bond type does not match
            GOTO 500
  420       CONTINUE
          ENDIF
!-- reject if already in the ring list - this is a secondary ring closure.
          DO 450 L=2,N
            IF (JAT.EQ.RINGAT(L)) GOTO 500
 450      CONTINUE
!-- growth point JAT is rejected if a terminal atom
          CALL SAMCON(JAT,NCX,LLIG,LMIG,IPIB)
!-- record atom for which we have connections stored
          CAT=JAT
          IF (NCX.LE.1) GOTO 500
!-- accept this as possible ring atom
          N=N+1
          RINGAT(N)=JAT
          IFROM(N)=IAT
          IPT(N)=0
          IBACK=0
!-- test from ring closure if growth atom Jat = start atom Iat1
          IF (JAT.EQ.IAT1) GOTO 501
        ENDIF
!-- if max ring size  is exceeded then backtrack
        IF (N.GT.MAXRNG) IBACK=1
!-- backtrack on trial atom N , so we can try next connect to Iat
!-- if N = 0 then stop process, no ring found
        IF (IBACK.EQ.1) THEN
          IAT=IFROM(N)
          IF (IAT.LE.0) GOTO 501
          N=N-1
        ENDIF
!--
!-- loop on trials
!--
 500  CONTINUE
 501  CONTINUE
!--
!--
      NRING=N-1

      END
!
!*****************************************************************************
!
      SUBROUTINE PLUDIJ(IAT,JAT,DVAL)

	USE SAMVAR
!-- Function: Calculate the distance IAT - JAT  give orthor coords XO
!-- Version:  23.5.94                  SAM MOTHERWELL 20.10.93
!-- Notes:
!-- 1. Iat , Jat are atom numbers inthe list of atoms with coords XO
!--   The distance is returned as DVAL

      IMPLICIT NONE

      INTEGER IAT,JAT
      REAL DVAL
      REAL DXO,DYO,DZO

      DXO=AXYZO(IAT,1)-AXYZO(JAT,1)
      DYO=AXYZO(IAT,2)-AXYZO(JAT,2)
      DZO=AXYZO(IAT,3)-AXYZO(JAT,3)
      DVAL=SQRT(DXO*DXO+DYO*DYO+DZO*DZO)

      END SUBROUTINE PLUDIJ
!
!*****************************************************************************
!
      SUBROUTINE SAMPIQ(IAT,JAT,IEL,IPIBON)
!--
!-- Function: Pi-bond query - is bond Iat-Jat a pi-bond.
!-- Version:  24.2.95   8.12.94    Sam Motherwell
!-- Notes:
!--    jat  C -- C  kat
!--          \  /
!--           Tr
!--         iat
!--    The test is simply to find the above triangle
!--
!-- Arguments:
!-- IAT, JAT   define the input query bond
!-- IEL        table of element codes
!-- IBON       bond list input for structure
!-- IBONT      bond type list
!-- NBON       number of bonds in list
!-- IPIBON     returned answer =0 not pi-bond,  =1 pibond
      USE SAMVAR

      IMPLICIT NONE

      INTEGER IAT,JAT,IEL(MAXATM),IPIBON
!-- local
      INTEGER NCX,NCK,K,L,KAT,I1,I2,IAT1,JAT1,IPIB
      INTEGER ILIG(30),LMIG(30),KLIG(30)
!-- Functions referenced 
      LOGICAL ISMET

!-- we assume that the connection table is correctly set up.
!-- each atom has a maximum of 29 connections.  (*,30) is the number of conn.
!--
!-- check that atom IAT is a transiton metal.
      IPIBON=0
      I1=0
      I2=0
      IF (.NOT.ISMET(IEL(IAT)).AND.IEL(IAT).NE.85) I1=1
      IF (.NOT.ISMET(IEL(JAT)).AND.IEL(JAT).NE.85) I2=1      
 
!-- swap so Tr metal is Iat
      IF (I1.EQ.0) THEN
        IAT1=IAT
        JAT1=JAT
      ELSE
        IAT1=JAT
        JAT1=IAT
      ENDIF
      IF (IEL(JAT1).NE.1) RETURN

!-- NC is number of connections to Metal atom Iat. Search these for
!-- a Carbon   (iel=1)  which bonds to the carbon atom Jat.
!-- including bonds already assigned as pi-bonds
      IPIB=1
      CALL SAMCON(IAT1,NCX,ILIG,LMIG,IPIB)
      DO  K=1,NCX
        KAT=ILIG(K)
        IF (KAT.EQ.JAT1) RETURN
        IF (IEL(KAT).NE.1) RETURN
        CALL SAMCON(KAT,NCK,KLIG,LMIG,IPIB)
        DO L=1,NCK
          IF(KLIG(L).EQ.JAT1) IPIBON=1
        ENDDO
        IF (IPIBON.GT.0) RETURN
      ENDDO

      END SUBROUTINE SAMPIQ
!
!*****************************************************************************
!
