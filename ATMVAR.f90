!
!*****************************************************************************
!
! This module contains all atom related data
!
      MODULE ATMVAR

      IMPLICIT NONE

      INTEGER     MaxAtm_3
      PARAMETER ( MaxAtm_3 = 150 )

      INTEGER     MaxElm
      PARAMETER ( MaxElm = 109 )

! Elements (plus other CSD 'element' definitions What's 'Zz'??)
! Me = methyl, Du = Dummy
      CHARACTER*3 ElementStr(1:MaxElm)

      DATA        ElementStr                                                            &
               /'C  ', 'H  ', 'Ac ', 'Ag ', 'Al ', 'Am ', 'Ar ', 'As ', 'At ', 'Au ',   &
                'B  ', 'Ba ', 'Be ', 'Bi ', 'Bk ', 'Br ', 'Ca ', 'Cd ', 'Ce ', 'Cf ',   &
                'Cl ', 'Cm ', 'Co ', 'Cr ', 'Cs ', 'Cu ', 'D  ', 'Dy ', 'Er ', 'Es ',   &
                'Eu ', 'F  ', 'Fe ', 'Fm ', 'Fr ', 'Ga ', 'Gd ', 'Ge ', 'He ', 'Hf ',   &
                'Hg ', 'Ho ', 'I  ', 'In ', 'Ir ', 'K  ', 'Kr ', 'La ', 'Li ', 'Lu ',   &
                'Lw ', 'Md ', 'Mg ', 'Mn ', 'Mo ', 'N  ', 'Na ', 'Nb ', 'Nd ', 'Ne ',   &
                'Ni ', 'No ', 'Np ', 'O  ', 'Os ', 'P  ', 'Pa ', 'Pb ', 'Pd ', 'Pm ',   &
                'Po ', 'Pr ', 'Pt ', 'Pu ', 'Ra ', 'Rb ', 'Re ', 'Rh ', 'Rn ', 'Ru ',   &
                'S  ', 'Sb ', 'Sc ', 'Se ', 'Si ', 'Sm ', 'Sn ', 'Sr ', 'Ta ', 'Tb ',   &
                'Tc ', 'Te ', 'Th ', 'Ti ', 'Tl ', 'Tm ', 'U  ', 'V  ', 'W  ', 'X  ',   &
                'Xe ', 'Y  ', 'Yb ', 'Z  ', 'Zn ', 'Zr ', 'Zz ', 'Me ', 'Du '/
  
      END MODULE ATMVAR
!
!*****************************************************************************
!
