!
!*****************************************************************************
!
      SUBROUTINE create_fob

      USE ATMVAR
      USE ZMVAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL            FOB
      COMMON /FCSTOR/ FOB(MaxAtm_3,MFCSTO)

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER iFrg, i, item, iRef, iFrgCopy
      INTEGER tNumHydrogens, tNumNonHydrogens, tAtomNumber
      REAL ssq, atem, btem
      REAL, EXTERNAL :: ascfac
      INTEGER, EXTERNAL :: ElmSymbol2CSD
      INTEGER tElemNumber

! JvdS Order all atoms such that the Hydrogen atoms are always at
! the end of the atom list. That way, if we don't want to use hydrogens
! we can simply subtract the number of hydrogens from the number of atoms and
! everything works

! Preliminary loop to determine number of atoms, number of hydrogen atoms,
! and number of non-hydrogen atoms
      TotNumOfAtoms = 0
      NumOfHydrogens = 0
      NumOfNonHydrogens = 0
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 1, natoms(iFrg)
              TotNumOfAtoms = TotNumOfAtoms + 1
              IF (asym(i,iFrg).EQ.'H  ') THEN
                NumOfHydrogens = NumOfHydrogens + 1
              ELSE
                NumOfNonHydrogens = NumOfNonHydrogens + 1
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      natom = TotNumOfAtoms
! The 'real' loop. Information for hydrogens is stored after all the non-hydrogen atoms      
      item = 0
      tNumHydrogens = 0
      tNumNonHydrogens = 0
      tAtomNumber = 0         ! To make life easier, we just use a mapping in MAKEFRAC
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 1, natoms(iFrg)
              tAtomNumber = tAtomNumber + 1
              IF (asym(i,iFrg).EQ.'H  ') THEN
                tNumHydrogens = tNumHydrogens + 1
                item = NumOfNonHydrogens + tNumHydrogens ! Start counting after non-hydrogens
              ELSE
                tNumNonHydrogens = tNumNonHydrogens + 1
                item = tNumNonHydrogens
              ENDIF
              OrderedAtm(tAtomNumber) = item ! To make life easier, we just use a mapping in MAKEFRAC
              tElemNumber = ElmSymbol2CSD(asym(i,iFrg)(1:2))
              DO iRef = 1, NumOfRef
                ssq = 0.25*DSTAR(iRef)**2
                atem = occ(i,iFrg) * AScFac(ssq,tElemNumber)
                btem = tiso(i,iFrg)*ssq
                FOB(item,iRef) = atem*EXP(-btem)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO

      END SUBROUTINE CREATE_FOB
!
!*****************************************************************************
!
      REAL FUNCTION AScFac(ss,TheElemNumber)
!
! INPUT   : ss            = angle dependence of the atomic scattering factor
!           TheElemNumber = the CSD element number (1 = Carbon, 2 = Hydrogen, etc.)
!
! RETURNS : the Atomic SCattering FACtor
!
      USE ATMVAR

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: ss
      INTEGER, INTENT (IN   ) :: TheElemNumber

      AScFac = A1(TheElemNumber) * EXP(-B1(TheElemNumber)*ss) +        &
               A2(TheElemNumber) * EXP(-B2(TheElemNumber)*ss) +        &
               A3(TheElemNumber) * EXP(-B3(TheElemNumber)*ss) +        &
               A4(TheElemNumber) * EXP(-B4(TheElemNumber)*ss) + CV(TheElemNumber)

      END FUNCTION ASCFAC
!
!*****************************************************************************
!
      SUBROUTINE Create_AtomicWeightings
! This routine sets the weights for the atoms used when calculating the centre of mass
! of a Z-matrix.
! The weights are set such that the 'crystallographic' centre of mass,
! i.e. the centre of scattering power, is used for rotations.
! The scattering power of an atom is the square of the number of its electrons,
! which is taken to be Z, its atomic number, i.e. we are neglecting ions.
! Ideally, we would want hydrogens to have weight 0.0 when not taken into account.

      USE ZMVAR

      IMPLICIT NONE

      INTEGER iFrg

      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          IF (icomflg(iFrg) .EQ. 0)  THEN
            CALL zmCreate_AtomicWeightings(iFrg)
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE Create_AtomicWeightings
!
!*****************************************************************************
!
      SUBROUTINE zmCreate_AtomicWeightings(iFrg)
! This routine sets the weights for the atoms used when calculating the centre of mass
! of a Z-matrix.
! The weights are set such that the 'crystallographic' centre of mass,
! i.e. the centre of scattering power, is used for rotations.
! The scattering power of an atom is the square of the number of its electrons,
! which is taken to be Z, its atomic number, i.e. we are neglecting ions.
! Ideally, we would want hydrogens to have weight 0.0 when not taken into account.

      USE ZMVAR
      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER I, J
      REAL    TotalAtomicWeighting

      DO I = 1, natoms(iFrg)
        DO J = 1, MaxElm
          IF (asym(I,iFrg)(1:2) .EQ. ElementStr(J)(1:2)) THEN
            AtomicWeighting(I,iFrg) = FLOAT(atnr(J))**2
            EXIT
          ENDIF
        ENDDO
      ENDDO
      TotalAtomicWeighting = 0.0
      DO I = 1, natoms(iFrg)
        TotalAtomicWeighting = TotalAtomicWeighting + AtomicWeighting(I,iFrg) 
      ENDDO
      DO I = 1, natoms(iFrg)
        AtomicWeighting(I,iFrg) = AtomicWeighting(I,iFrg) / TotalAtomicWeighting 
      ENDDO

      END SUBROUTINE zmCreate_AtomicWeightings
!
!*****************************************************************************
!
