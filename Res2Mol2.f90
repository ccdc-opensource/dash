! @@ The validity of tLattice is never checked !!!
!
!*****************************************************************************
!
      INTEGER FUNCTION Res2Mol2(TheFileName)
!
! This subroutine tries to read a SHELX .res file and tries to convert it to a Sybyl .mol2 file.
! The .mol2 file stuff was taken from 'cad2mol2.f' by Jos Lommerse.
! All variables are read in directly to the global variables in SAMVAR, ready for
! Sam's routines to calculate bond types.
!
! A .res file is free format.
!
! RETURNS : 0 for failure
!           1 for success
!
!TITLBA09_2                                  
!CELL   0.0000   4.0207   4.5938  21.0298  90.7123  69.6944  80.3612
!LATT  1
!O1    0    0.19295   -0.33996    0.06295    1.00000    0.02476
!O2    0   -0.17961   -0.00616    0.13265    1.00000    0.02000
!C3    0   -0.10381   -0.30317    0.43567    1.00000    0.08073
!H4    0   -0.29456   -0.41679    0.45015    1.00000    0.10000
!H5    0    0.14042   -0.43747    0.42625    1.00000    0.10000
!AG19  0   -0.28653    0.22594    0.03911    1.00000    0.04943
!H20   0   -0.13171   -0.14728    0.47266    1.00000    0.10000
!END 
!
      USE VARIABLES
      USE SAMVAR
      USE ZMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      CHARACTER*2  AtmElement
      INTEGER      i
      REAL         Coordinates(1:3,1:MAXATM_2)
      INTEGER      InputFile
      REAL         a, b, c, alpha, beta, gamma
      REAL         tLattice(1:3,1:3)
      INTEGER      NewLength
      LOGICAL, EXTERNAL :: ChrIsLetter
      REAL         tX, tY, tZ
      CHARACTER*255 tString, tSubString
      REAL         DummyReal
      INTEGER      DummyInteger
      INTEGER, EXTERNAL :: WriteMol2
      INTEGER, EXTERNAL :: ElmSymbol2CSD

! Initialise to 'failure'
      Res2Mol2 = 0
      natcry = 0
      InputFile = 10
      OPEN(UNIT=InputFile,file=TheFileName,form='formatted',status='old',err=998)
! Initialise cell parameters to invalid values
      a = 0.0
      b = 0.0
      c = 0.0
      alpha = 0.0
      beta  = 0.0
      gamma = 0.0
   10 READ(InputFile,'(A)',ERR=990,END=100) tString
      IF (LEN_TRIM(tString) .LT. 3) GOTO 10
      CALL StrUpperCase(tString)
      SELECT CASE (tString(1:4))
        CASE ('CELL')
          READ(tString(5:),*,ERR=990,END=990) DummyReal, a, b, c, alpha, beta, gamma
          CALL LatticeCellParameters2Lattice(a, b, c, alpha, beta, gamma, tLattice)
        CASE ('TITL', 'ZERR', 'LATT', 'SYMM', 'SFAC', 'DISP', 'UNIT', 'L.S.', 'REM ')
          ! do nothing
        CASE ('HKLF', 'END ') ! We have finished, process the data we have read
          GOTO 100
        CASE DEFAULT ! If it's none of the above, it is probably an atom
! Check if first character is a letter. If not, skip this line and read the next.
          IF (.NOT. ChrIsLetter(tString(1:1))) GOTO 10
! If the first four are characters, it was another keyword
          IF (ChrIsLetter(tString(2:2)) .AND.            &
              ChrIsLetter(tString(3:3)) .AND.            &
              ChrIsLetter(tString(4:4))) GOTO 10
! From now on, we assume it was an atom. Atomic element symbols can only have 2 characters in DASH
          CALL INC(natcry)
          AtmElement(1:2) = tString(1:2)
          IF (.NOT. ChrIsLetter(tString(2:2))) AtmElement(2:2) = ' '
          aelem(natcry) = ElmSymbol2CSD(AtmElement)
          CALL StrClean(tString,NewLength)
          CALL GetSubString(tString,' ',tSubString)
          atomlabel(natcry) = tSubString(1:5)
          READ(tString,*,ERR=990) DummyInteger, Coordinates(1,natcry), Coordinates(2,natcry), Coordinates(3,natcry)
      END SELECT
! Read next line
      GOTO 10
  100 CONTINUE
      IF (natcry .EQ. 0) THEN
        CALL ErrorMessage('No Atoms found.')
        RETURN
      ENDIF
! The variable Coordinates holds the atomic fractional co-ordinates, the mol2 file
! needs orthogonal co-ordinates => convert
      DO I = 1, natcry
        tX = Coordinates(1,I) * tLattice(1,1) + Coordinates(2,I) * tLattice(1,2) + Coordinates(3,I) * tLattice(1,3)
        tY = Coordinates(1,I) * tLattice(2,1) + Coordinates(2,I) * tLattice(2,2) + Coordinates(3,I) * tLattice(2,3)
        tZ = Coordinates(1,I) * tLattice(3,1) + Coordinates(2,I) * tLattice(3,2) + Coordinates(3,I) * tLattice(3,3)
        axyzo(I,1) = tX
        axyzo(I,2) = tY
        axyzo(I,3) = tZ
      ENDDO
      CLOSE(InputFile)
      CALL SAMABO
      DO i = 1, natcry
        izmbid(i,0) = i
        izmoid(i,0) = i
      ENDDO
      Res2Mol2 = WriteMol2(TheFileName(1:LEN_TRIM(TheFileName)-3)//'mol2',.FALSE.,0)
      RETURN
  998 CALL ErrorMessage('Error opening input file.')
      RETURN
  990 CALL ErrorMessage('Error while reading input file.')
      RETURN

      END FUNCTION Res2Mol2
!
!*****************************************************************************
!
      INTEGER FUNCTION CSSR2Mol2(TheFileName)
!
! This subroutine tries to read a .cssr file and tries to convert it to a Sybyl .mol2 file.
! The .mol2 file stuff was taken from 'cad2mol2.f' by Jos Lommerse.
! All variables are read in directly to the global variables in SAMVAR, ready for
! Sam's routines to calculate bond types.
!
! RETURNS : 0 for failure
!           1 for success
!
! A .cssr file is fixed format.
!
!                                         3.988   3.988   9.769
!                       90.000  90.000  60.000    SPGR =  1 P 1         OPT = 1
!   6   0 Created by Cerius2
!     0 halite_3    : halite_3                                
!   1 Na1     0.00000   0.00000   0.00000    0   0   0   0   0   0   0   0   0.000
!   2 Na2     0.66667   0.66667   0.33333    0   0   0   0   0   0   0   0   0.000
!   3 Cl3     0.00000   0.00000   0.50000    0   0   0   0   0   0   0   0   0.000
!   4 Cl4     0.66667   0.66667   0.83333    0   0   0   0   0   0   0   0   0.000
!   5 Na5     0.33333   0.33333   0.66667    0   0   0   0   0   0   0   0   0.000
!   6 Cl6     0.33333   0.33333   0.16667    0   0   0   0   0   0   0   0   0.000
!
      USE VARIABLES
      USE SAMVAR
      USE ZMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      CHARACTER*2  AtmElement(MAXATM_2)
      INTEGER      i
      REAL         Coordinates(3,MAXATM_2)
      INTEGER      InputFile
      REAL         a, b, c, alpha, beta, gamma
      INTEGER      INORM
      REAL         tLattice(1:3,1:3)
      CHARACTER*50 tString
      INTEGER*4    SERIAL
      CHARACTER*4  NAME(MAXATM_2)
      LOGICAL, EXTERNAL :: ChrIsLetter
      REAL         tX, tY, tZ
      INTEGER, EXTERNAL :: WriteMol2, ElmSymbol2CSD

! Initialise to 'failure'
      CSSR2Mol2 = 0
      natcry = 0
      InputFile = 10
      OPEN(UNIT=InputFile,file=TheFileName,form='formatted',status='old',err=998)
! Initialise cell parameters to invalid values
! If no valid values are read, then the atomic co-ordinates must be orthogonal
      a = 0.0
      b = 0.0
      c = 0.0
      alpha = 0.0
      beta  = 0.0
      gamma = 0.0
! Record # 1 : unit cell lengths
      READ(InputFile,'(38X,3F8.3)',ERR=10,END=999) a, b, c ! On error, just continue
! Record # 2 : unit cell angles
   10 READ(InputFile,'(21X,3F8.3)',ERR=20,END=999) alpha, beta, gamma ! On error, just continue
! Record # 3 : number of atoms and co-ordinate system.
! INORM = 0   =>   fractional co-ordinates
! INORM = 1   =>   orthogonal co-ordinates
   20 READ(InputFile,'(2I4)',ERR=990,END=999) natcry, INORM
      IF (natcry .EQ. 0) THEN
        CALL ErrorMessage('No Atoms found.')
        RETURN
      ENDIF
      IF (INORM .EQ. 1) THEN
        a = 1.0
        b = 1.0
        c = 1.0
        alpha = 90.0
        beta  = 90.0
        gamma = 90.0
      ENDIF
      CALL LatticeCellParameters2Lattice(a, b, c, alpha, beta, gamma, tLattice)
! Record # 4 : second title, just skip.
      READ(InputFile,'(A50)',ERR=30,END=999) tString ! On error, just continue
   30 CONTINUE
      DO I = 1, natcry
        READ(InputFile,'(I4,1X,A4,2X,F9.5,1X,F9.5,1X,F9.5)',ERR=990,END=999) SERIAL, NAME(I), &
                                                            Coordinates(1,I), Coordinates(2,I), Coordinates(3,I)
      ENDDO
! The variable X holds the atomic fractional co-ordinates, the mol2 file
! needs orthogonal co-ordinates => convert
      DO I = 1, natcry
        tX = Coordinates(1,I) * tLattice(1,1) + Coordinates(2,I) * tLattice(1,2) + Coordinates(3,I) * tLattice(1,3)
        tY = Coordinates(1,I) * tLattice(2,1) + Coordinates(2,I) * tLattice(2,2) + Coordinates(3,I) * tLattice(2,3)
        tZ = Coordinates(1,I) * tLattice(3,1) + Coordinates(2,I) * tLattice(3,2) + Coordinates(3,I) * tLattice(3,3)
        axyzo(I,1) = tX
        axyzo(I,2) = tY
        axyzo(I,3) = tZ
      ENDDO
      CLOSE(InputFile)
! The variable NAME now holds 'Cl6 ' etc. for every atom. 
! We must extract the element from that
      DO I = 1, natcry
        atomlabel(I)(1:4) = NAME(I)(1:4)
        atomlabel(I)(5:5) = ' '
        AtmElement(I) = NAME(I)(1:2)
        IF (.NOT. ChrIsLetter(AtmElement(I)(2:2))) AtmElement(I)(2:2) = ' '
        CALL StrUpperCase(AtmElement(I))
      ENDDO
! Given the element, assign the CSD element (fill aelem(1:MAXATM))
      DO I = 1, natcry
        aelem(I) = ElmSymbol2CSD(AtmElement(I))
      ENDDO
      CALL SAMABO
      DO i = 1, natcry
        izmbid(i,0) = i
        izmoid(i,0) = i
      ENDDO
      CSSR2Mol2 = WriteMol2(TheFileName(1:LEN_TRIM(TheFileName)-4)//'mol2',.FALSE.,0)
      RETURN
  990 CALL ErrorMessage('Error while reading input file.')
      RETURN
  998 CALL ErrorMessage('Error opening input file.')
      RETURN
  999 CALL ErrorMessage('Unexpected end of input file.')
      RETURN

      END FUNCTION CSSR2Mol2
!
!*****************************************************************************
!
      INTEGER FUNCTION WriteMol2(TheFileName,IncludeUnitCell,iFrg)
!
! Takes number of atoms    from natcry    in SAMVAR
! Takes atomic coordinates from axyzo     in SAMVAR  (orthogonal)
! Takes element types      from aelem     in SAMVAR  (CSD style)
! Takes atom labels        from atomlabel in SAMVAR
! Takes bonds              from bond      in SAMVAR
! Takes bond types         from btype     in SAMVAR
! Takes unit cell from global variables in DASH
! Sets space group to P1
! and writes out a .mol2 file
! Takes the order of the atoms from izmbid(:,iFrg)
!
! mol2 files contain an unresolved ambiguity: atom co-ordinates are given wrt. the 
! the orthogonal axes, but the unit cell is given as a, b, c, alpha, beta, gamma.
! It is not specified how the unit cell is to be constructed wrt. the orthogonal axes
! from the unit cell parameters.
! Mercury turns out to chose: a along x. Everywhere else in DASH, we have used c along z
!
! RETURNS 0 for failure
!         1 for success

      USE SAMVAR
      USE ATMVAR
      USE ZMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (IN   ) :: IncludeUnitCell
      INTEGER,       INTENT (IN   ) :: iFrg

      INCLUDE 'Lattice.inc'

      CHARACTER*4 sybatom(1:MAXATM_2)
      CHARACTER*2 BondStr(0:9)
      CHARACTER*2 HybridisationStr
      CHARACTER*1, EXTERNAL :: ChrLowerCase, ChrUpperCase
      INTEGER      ii, I, J, Ilen, OutputFile
      LOGICAL, EXTERNAL :: FnUnitCellOK
      LOGICAL      tIncludeUnitCell
      REAL         tLattice(1:3,1:3), tRecLattice(1:3,1:3), tLattice_2(1:3,1:3)
      REAL         tX, tY, tZ
      REAL         NEWaxyzo(1:MAXATM_2,1:3)

!    The CSD bond types are:  1 = single  2= double  3=triple  4=quadruple
!                             5 = aromatic      6 = polymeric single
!                             7 = delocalised   9 = pi-bond
!
!   Sam's                            mol2
!     1       (single)                1
!     2       (double)                2
!     3       (triple)                3
!     4       (quadruple)             un
!     5       (aromatic)              ar
!     6       (polymeric)             un
!     7       (delocalised double)    un
!     9       (pi)                    un

      tIncludeUnitCell = IncludeUnitCell
      IF (.NOT. FnUnitCellOK()) tIncludeUnitCell = .FALSE.
      BondStr(0) = 'un'   ! unspecified
      BondStr(1) = ' 1'
      BondStr(2) = ' 2'
      BondStr(3) = ' 3'
      BondStr(4) = 'un'
      BondStr(5) = 'ar'
      BondStr(6) = 'un'
      BondStr(7) = 'un'
      BondStr(8) = 'un'
      BondStr(9) = 'un'
! Initialise to failure
      WriteMol2 = 0
      DO I = 1, natcry
        sybatom(I) = '    '
        SELECT CASE (aelem(I))
          CASE (1, 56, 64) ! C, N, O
            SELECT CASE (hybr(I))
              CASE (1)
                HybridisationStr = '1 '
              CASE (2)
                HybridisationStr = '2 '
              CASE (3)
                HybridisationStr = '3 '
              CASE (4)
                HybridisationStr = 'ar'
              CASE DEFAULT
                HybridisationStr = '0 '
            END SELECT
            sybatom(I) = ElementStr(aelem(I))(1:1)//'.'//HybridisationStr
          CASE DEFAULT
            sybatom(I)(1:1) = ChrUpperCase(ElementStr(aelem(I))(1:1))
            sybatom(I)(2:2) = ChrLowerCase(ElementStr(aelem(I))(2:2))
        END SELECT
      ENDDO
      Ilen = LEN_TRIM(TheFileName)
      OutputFile = 3
      OPEN(UNIT=OutputFile,file=TheFileName(1:Ilen),form='formatted',ERR=999)
      WRITE(OutputFile,"('@<TRIPOS>MOLECULE')",ERR=999)
      WRITE(OutputFile,'(A)',ERR=999) 'Temporary file created by DASH'
      WRITE(OutputFile,"(2(I5,1X),'    1     0     0')",ERR=999) natcry, nbocry
      WRITE(OutputFile,"('SMALL')",ERR=999)
      WRITE(OutputFile,"('NO_CHARGES')",ERR=999)
      WRITE(OutputFile,"('@<TRIPOS>ATOM')",ERR=999)
      IF (tIncludeUnitCell) THEN
        CALL LatticeCellParameters2Lattice(CellPar(1), CellPar(2), CellPar(3), &
                                           CellPar(4), CellPar(5), CellPar(6), tLattice)
! tLattice now holds the matrix for fractional to c-along-z orthogonal
        CALL InverseMatrix(tLattice,tRecLattice,3)
! tRecLattice now holds the matrix for c-along-z orthogonal to fractional
        CALL LatticeCellParameters2Lattice_2(CellPar(1), CellPar(2), CellPar(3), &
                                             CellPar(4), CellPar(5), CellPar(6), tLattice_2)
! tLattice_2 now holds the matrix for fractional to a-along-x orthogonal
        CALL GMPRD(tLattice_2,tRecLattice,tLattice,3,3,3)
! tLattice now holds the matrix for c-along-z orthogonal to a-along-x orthogonal
! axyzo now holds the orthogonal co-ordinates if c is along z  
        DO I = 1, natcry
          tX = axyzo(I,1) * tLattice(1,1) + axyzo(I,2) * tLattice(1,2) + axyzo(I,3) * tLattice(1,3)
          tY = axyzo(I,1) * tLattice(2,1) + axyzo(I,2) * tLattice(2,2) + axyzo(I,3) * tLattice(2,3)
          tZ = axyzo(I,1) * tLattice(3,1) + axyzo(I,2) * tLattice(3,2) + axyzo(I,3) * tLattice(3,3)
          NEWaxyzo(I,1) = tX
          NEWaxyzo(I,2) = tY
          NEWaxyzo(I,3) = tZ
        ENDDO
! NEWaxyzo now holds the orthogonal co-ordinates if a is along x
      ELSE
        DO I = 1, natcry
          NEWaxyzo(I,1) = axyzo(I,1)
          NEWaxyzo(I,2) = axyzo(I,2)
          NEWaxyzo(I,3) = axyzo(I,3)
        ENDDO
      ENDIF
      DO I = 1, natcry
        WRITE(OutputFile,270,ERR=999) I,atomlabel(izmbid(I,iFrg)),(NEWaxyzo(izmbid(I,iFrg),j),j=1,3),sybatom(izmbid(I,iFrg))
  270   FORMAT(I3,1X,A5,1X,3(F10.4,1X),A4,' 1 <1> 0.0')
      ENDDO
      WRITE(OutputFile,"('@<TRIPOS>BOND')",ERR=999)
      DO i = 1, nbocry
        WRITE(OutputFile,'(3(I3,1X),A2)',ERR=999) i,izmoid(bond(i,1),iFrg),izmoid(bond(i,2),iFrg),BondStr(btype(I))
      ENDDO
! Write out unit cell. First six numbers: a, b, c, alpha, beta, gamma
! Next two integers: space group followed by axis setting
! For this purpose, we set the space group to P1
!C@<TRIPOS>CRYSIN
!C   11.3720   10.2720    7.3590  108.7500   71.0700   96.1600     2     1
      IF (tIncludeUnitCell) THEN
        WRITE(OutputFile,"('@<TRIPOS>CRYSIN')",ERR=999)
        WRITE(OutputFile,'(6(F8.4,1X),"   1    1")',ERR=999) (CellPar(ii),ii=1,6)
      ENDIF
      CLOSE(OutputFile)
      WriteMol2 = 1
      RETURN
  999 CALL ErrorMessage('Error writing mol2 file.')
      CLOSE(OutputFile)

      END FUNCTION WriteMol2
!
!*****************************************************************************
!
      INTEGER FUNCTION ElmSymbol2CSD(TheElementSymbol)
! This function takes an element symbol (e.g. 'Ag') and converts it to the corresponding CSD element number

      USE ATMVAR

      CHARACTER*2, INTENT (IN   ) :: TheElementSymbol

      INTEGER I
      CHARACTER*2 tElem
      CHARACTER*1, EXTERNAL :: ChrLowerCase, ChrUpperCase

      tElem(1:1) = ChrUpperCase(TheElementSymbol(1:1))
      tElem(2:2) = ChrLowerCase(TheElementSymbol(2:2))
      DO I = 1, MaxElm
        IF (tElem .EQ. ElementStr(I)) THEN
          ElmSymbol2CSD = I
          RETURN
        ENDIF
      ENDDO
      CALL WarningMessage('Unknown element '//TheElementSymbol(1:2)//'.'//CHAR(13)//&
                          'Element has been set to Dummy.')
      ElmSymbol2CSD = MaxElm

      END FUNCTION ElmSymbol2CSD
!
!*****************************************************************************
!
