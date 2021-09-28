! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      SUBROUTINE create_fob(AbsorbHydrogens)

      USE ATMVAR
      USE ZMVAR
      USE REFVAR

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: AbsorbHydrogens

      INCLUDE 'params.inc'

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
      COMMON /POSNS / NATOM, X(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      REAL         atem,                      btem
      COMMON /FOB/ atem(1:MaxAtm_3,1:MaxRef), btem(1:MaxAtm_3,1:MaxRef)

      INTEGER, EXTERNAL :: NumOfBondedHydrogens, ElmNumber2CSD
      REAL, EXTERNAL :: ascfac
      INTEGER iFrg, iAtom, item, iRef, Element, AtomicNumber
      INTEGER tNumHydrogens, tNumNonHydrogens, tAtomNumber
      REAL ssq

! Order all atoms such that the Hydrogen atoms are always at
! the end of the atom list. That way, if we don't want to use hydrogens
! we can simply subtract the number of hydrogens from the number of atoms and
! everything works

! Preliminary loop to determine number of atoms, number of hydrogen atoms,
! and number of non-hydrogen atoms
      TotNumOfAtoms = 0
      NumOfHydrogens = 0
      NumOfNonHydrogens = 0
      DO iFrg = 1, nFrag
        DO iAtom = 1, natoms(iFrg)
          TotNumOfAtoms = TotNumOfAtoms + 1
          IF (zmElementCSD(iAtom,iFrg) .EQ. 2) THEN
            NumOfHydrogens = NumOfHydrogens + 1
          ELSE
            NumOfNonHydrogens = NumOfNonHydrogens + 1
          ENDIF
        ENDDO
      ENDDO
      natom = TotNumOfAtoms
! The 'real' loop. Information for hydrogens is stored after all the non-hydrogen atoms      
      item = 0
      tNumHydrogens = 0
      tNumNonHydrogens = 0
      tAtomNumber = 0         ! To make life easier, we just use a mapping in MAKEFRAC
      DO iFrg = 1, nFrag
        DO iAtom = 1, natoms(iFrg)
          tAtomNumber = tAtomNumber + 1
          IF (zmElementCSD(iAtom,iFrg) .EQ. 2) THEN
            tNumHydrogens = tNumHydrogens + 1
            item = NumOfNonHydrogens + tNumHydrogens ! Start counting after non-hydrogens
          ELSE
            tNumNonHydrogens = tNumNonHydrogens + 1
            item = tNumNonHydrogens
          ENDIF
          OrderedAtm(tAtomNumber) = item ! To make life easier, we just use a mapping in MAKEFRAC
          AtomicNumber = atnr(zmElementCSD(iAtom,iFrg))
          Element = ElmNumber2CSD(AtomicNumber)
          DO iRef = 1, NumOfRef
            ssq = 0.25*DSTAR(iRef)**2
            IF ( AbsorbHydrogens ) THEN
              atem(item,iRef) = occ(iAtom,iFrg) * AScFac(ssq,Element) * (1.0 + (FLOAT(NumOfBondedHydrogens(iAtom, iFrg))/FLOAT(AtomicNumber)))
            ELSE
              atem(item,iRef) = occ(iAtom,iFrg) * AScFac(ssq,Element)
            ENDIF
            btem(item,iRef) = tiso(iAtom,iFrg)*ssq
            FOB(item,iRef) = atem(item,iRef)*EXP(-btem(item,iRef))
          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE CREATE_FOB
!
!*****************************************************************************
!
      SUBROUTINE CreateFobITF

      USE ATMVAR
      USE ZMVAR
      USE REFVAR
      USE RRVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      REAL            FOB
      COMMON /FCSTOR/ FOB(MaxAtm_3,MFCSTO)

      REAL         atem,                      btem
      COMMON /FOB/ atem(1:MaxAtm_3,1:MaxRef), btem(1:MaxAtm_3,1:MaxRef)

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER item, tNumHydrogens, tNumNonHydrogens, iFrg, iAtom, iRef

      item = 0
      tNumHydrogens = 0
      tNumNonHydrogens = 0
      DO iFrg = 1, nFrag
        DO iAtom = 1, natoms(iFrg)
          IF (zmElementCSD(iAtom,iFrg) .EQ. 2) THEN
            tNumHydrogens = tNumHydrogens + 1
            item = NumOfNonHydrogens + tNumHydrogens ! Start counting after non-hydrogens
          ELSE
            tNumNonHydrogens = tNumNonHydrogens + 1
            item = tNumNonHydrogens
          ENDIF
          DO iRef = 1, NumOfRef
            FOB(item,iRef) = atem(item,iRef)*EXP(-RR_ITF*btem(item,iRef))
          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE CreateFobITF
!
!*****************************************************************************
!
      REAL FUNCTION AScFac(ss, TheElemNumber)
      REAL, EXTERNAL :: xrayascfac
      REAL, EXTERNAL :: edascfac
      LOGICAL ELECDI
      COMMON /USEELE / ELECDI
      
      IF ( .NOT. ELECDI ) THEN
          AscFac = XrayAScFac(ss,TheElemNumber)
      ELSE
          AscFac = EdAScFac(ss,TheElemNumber)
      ENDIF
      
      END FUNCTION ASCFAC      

      REAL FUNCTION XrayAScFac(ss, TheElemNumber)

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

      XrayAScFac = A1(TheElemNumber) * EXP(-B1(TheElemNumber)*ss) +        &
               A2(TheElemNumber) * EXP(-B2(TheElemNumber)*ss) +        &
               A3(TheElemNumber) * EXP(-B3(TheElemNumber)*ss) +        &
               A4(TheElemNumber) * EXP(-B4(TheElemNumber)*ss) + CV(TheElemNumber)

      END FUNCTION XrayAScFac  

      REAL FUNCTION EdAScFac(ss, TheElemNumber)

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

      EdAScFac = EDA1(TheElemNumber) * EXP(-EDB1(TheElemNumber)*ss) +        &
                 EDA2(TheElemNumber) * EXP(-EDB2(TheElemNumber)*ss) +        &
                 EDA3(TheElemNumber) * EXP(-EDB3(TheElemNumber)*ss) +        &
                 EDA4(TheElemNumber) * EXP(-EDB4(TheElemNumber)*ss) + EDCV(TheElemNumber)

      END FUNCTION EdAScFac      
!
!*****************************************************************************
!
      SUBROUTINE Create_AtomicWeightings(HydrogenTreatment)
! This routine sets the weights for the atoms used when calculating the centre of mass
! of a Z-matrix.
! The weights are set such that the 'crystallographic' centre of mass,
! i.e. the centre of scattering power, is used for rotations.
! The scattering power of an atom is the square of the number of its electrons,
! which is taken to be Z, its atomic number, i.e. we are neglecting ions.
! Hydrogens have a negligible effect and are included even if not included during the SA.

      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: HydrogenTreatment ! 1 = ignore, 2 = absorb, 3 = explicit

      INTEGER iFrg

      DO iFrg = 1, nFrag
        IF (icomflg(iFrg) .EQ. 0)  THEN
          CALL zmCreate_AtomicWeightings(iFrg, HydrogenTreatment)
        ENDIF
      ENDDO

      END SUBROUTINE Create_AtomicWeightings
!
!*****************************************************************************
!
      SUBROUTINE zmCreate_AtomicWeightings(iFrg, HydrogenTreatment)
! This routine sets the weights for the atoms used when calculating the centre of mass
! of a Z-matrix.
! The weights are set such that the 'crystallographic' centre of mass,
! i.e. the centre of scattering power, is used for rotations.
! The scattering power of an atom is the square of the number of its electrons,
! which is taken to be Z, its atomic number, i.e. we are neglecting ions.
! Hydrogens have a negligible effect and are included even if not included during the SA.

      USE ZMVAR
      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg
      INTEGER, INTENT (IN   ) :: HydrogenTreatment ! 1 = ignore, 2 = absorb, 3 = explicit

      INTEGER, EXTERNAL :: NumOfBondedHydrogens
      INTEGER iAtom, AtomicNumber
      REAL    TotalAtomicWeighting

      TotalAtomicWeighting = 0.0
      DO iAtom = 1, natoms(iFrg)
        AtomicNumber = atnr(zmElementCSD(iAtom,iFrg))
        IF (HydrogenTreatment .EQ. 2) AtomicNumber = AtomicNumber + NumOfBondedHydrogens(iAtom, iFrg)
        IF ((HydrogenTreatment .NE. 3) .AND. (AtomicNumber .EQ. 1)) AtomicNumber = 0
        AtomicWeighting(iAtom,iFrg) = FLOAT(AtomicNumber)**2
!F        AtomicWeighting(iAtom,iFrg) = FLOAT(AtomicNumber)
        TotalAtomicWeighting = TotalAtomicWeighting + AtomicWeighting(iAtom,iFrg) 
      ENDDO
      DO iAtom = 1, natoms(iFrg)
        AtomicWeighting(iAtom,iFrg) = AtomicWeighting(iAtom,iFrg) / TotalAtomicWeighting 
      ENDDO

      END SUBROUTINE zmCreate_AtomicWeightings
!
!*****************************************************************************
!
      INTEGER FUNCTION NumOfBondedHydrogens(iAtom, iFrg)

      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iAtom, iFrg

      INTEGER iBond, tNumOfBondedHydrogens

      tNumOfBondedHydrogens = 0
      DO iBond = 1, NumberOfBonds(iFrg)
        IF (((Bonds(1,iBond,iFrg) .EQ. iAtom) .AND. (zmElementCSD(Bonds(2,iBond,iFrg),iFrg) .EQ. 2)) .OR. &
            ((Bonds(2,iBond,iFrg) .EQ. iAtom) .AND. (zmElementCSD(Bonds(1,iBond,iFrg),iFrg) .EQ. 2)))     &
          tNumOfBondedHydrogens = tNumOfBondedHydrogens + 1
      ENDDO
      NumOfBondedHydrogens = tNumOfBondedHydrogens

      END FUNCTION NumOfBondedHydrogens
!
!*****************************************************************************
!
