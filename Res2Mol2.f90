!
!*****************************************************************************
!
      INTEGER FUNCTION CSSR2Mol2(TheFileName)
!
! This subroutine tries to read a .cssr file and tries to convert it to a Sybyl .mol2 file.
! The .mol2 file stuff was taken from 'cad2mol2.f' by Jos Lommerse.
! Bonds are calculated from the distances between 2 atoms.
! The .cssr format is fixed.
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

      IMPLICIT NONE

      CHARACTER*MaxPathLength, INTENT (IN   ) :: TheFileName

      INTEGER maxatom, maxbond, maxelm
      PARAMETER (maxatom=100, maxbond=100, maxelm=108)

      CHARACTER*2  AtmElement(maxatom)
      CHARACTER*4  sybatom(maxatom)
      CHARACTER*80 mol2file, title
      INTEGER      i,j,natom,nbond, bat(maxbond,2)
      REAL         x(maxatom,3), bndr(maxatom)
      INTEGER      InputFile
      INTEGER      OutputFile
      REAL         a, b, c, alpha, beta, gamma
      INTEGER      INORM
      REAL         tLattice(1:3,1:3)
      CHARACTER*50 tString
      INTEGER*4    SERIAL
      CHARACTER*4  NAME(maxatom)
      INTEGER      I1, I2
      LOGICAL      ChrIsLetter ! Function
      REAL         tX, tY, tZ

! Initialise to 'failure'
      CSSR2Mol2 = 0
      natom = 0
      InputFile = 10
      OPEN(UNIT=InputFile,file=TheFileName,form='formatted',status='old',err=998)
      mol2file = 'Temp.mol2'
      OutputFile = 3
      OPEN(UNIT=OutputFile,file=mol2file,form='formatted',err=997)
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
   20 READ(InputFile,'(2I4)',ERR=990,END=999) natom, INORM
      IF (natom .EQ. 0) THEN
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
      DO I = 1, natom
        READ(InputFile,'(I4,1X,A4,2X,F9.5,1X,F9.5,1X,F9.5)',ERR=990,END=999) SERIAL, NAME(I), &
                                                            X(I,1), X(I,2), X(I,3)
      ENDDO
! The variable X holds the atomic fractional co-ordinates, the mol2 file
! needs orthogonal co-ordinates => convert
      DO I = 1, natom
        tX = X(I,1) * tLattice(1,1) + X(I,2) * tLattice(1,2) + X(I,3) * tLattice(1,3)
        tY = X(I,1) * tLattice(2,1) + X(I,2) * tLattice(2,2) + X(I,3) * tLattice(2,3)
        tZ = X(I,1) * tLattice(3,1) + X(I,2) * tLattice(3,2) + X(I,3) * tLattice(3,3)
        X(I,1) = tX
        X(I,2) = tY
        X(I,3) = tZ
      ENDDO
! The variable NAME now holds 'Cl6 ' etc. for every atom. 
! We must extract the element from that
      DO I = 1, natom    
        AtmElement(I) = '  '
        I2 = 1
        DO I1 = 1, 4
          IF (ChrIsLetter(NAME(I)(I1:I1))) THEN
            IF (I2 .EQ. 3) THEN
              CALL DebugErrorMessage('Element more than two chars in CSSR2Mol2.')
              I2 = 2
            ENDIF
            AtmElement(I)(I2:I2) = NAME(I)(I1:I1)
            I2 = I2 + 1
          ENDIF
        ENDDO
      ENDDO
      CLOSE(InputFile)
! Given the element, assign the bond radius
      CALL ass_type(natom,AtmElement,bndr)
! Make the bonds using a simple distance criterion
      CALL make_bond(natom,x,bndr,nbond,bat)
      CALL sybylatom(natom,AtmElement,sybatom,nbond,bat)
      WRITE(OutputFile,"('@<TRIPOS>MOLECULE')")
      WRITE(OutputFile,'(A)') 'Temporary file'
      WRITE(OutputFile,"(2(I5,X),'    1     0     0')") natom, nbond
      WRITE(OutputFile,"('SMALL')")
      WRITE(OutputFile,"('NO_CHARGES')")
      title = 'Temporary file created by DASH'
      WRITE(OutputFile,"(A80)") title
      WRITE(OutputFile,"('@<TRIPOS>ATOM')")
      DO i = 1, natom
        WRITE(OutputFile,270) i,AtmElement(i),(x(i,j),j=1,3),sybatom(i)
      ENDDO
  270 FORMAT(I3,1X,A2,1X,3(F10.4,1X),A4,' 1 <1> 0.0')
      WRITE(OutputFile,"('@<TRIPOS>BOND')")
      DO i = 1, nbond
        WRITE(OutputFile,'(4(I3,X))') i,bat(i,1),bat(i,2),1
      ENDDO
      CLOSE(OutputFile)
      CSSR2Mol2 = 1
      RETURN
  990 CALL ErrorMessage('Error while reading input file.')
      RETURN
  997 CALL ErrorMessage('Error opening mol2file')
      RETURN 
  998 CALL ErrorMessage('Error opening input file.')
      RETURN
  999 CALL ErrorMessage('Unexpected end of input file.')
      RETURN

      END FUNCTION CSSR2Mol2
!
!*****************************************************************************
!
      SUBROUTINE sybylatom(natom,AtmElement,sybatom,nbond,bat)

      PARAMETER(maxatom=100,maxbond=100)

!O      CHARACTER*1  number, getal
      CHARACTER*1  getal
      CHARACTER*2  AtmElement(maxatom)
      CHARACTER*4  sybatom(maxatom)
      INTEGER      i, j, neighbour, bat(maxbond,2)

      DO i = 1, natom
        SELECT CASE (AtmElement(i)(1:2))
          CASE ('O ')
            neighbour = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
              neighbour = neighbour + 1
              ENDIF
            ENDDO
            WRITE(getal,'(I1)') neighbour+1
!            READ(number,'(A1)') getal
            sybatom(i)=AtmElement(i)(1:1)//'.'//getal
          CASE ('N ')
            neighbour = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
                neighbour = neighbour + 1
              ENDIF
            ENDDO
            WRITE(getal,'(I1)') neighbour
!            READ(number,'(A1)') getal
            sybatom(i)=AtmElement(i)(1:1)//'.'//getal
          CASE ('C ')
            neighbour = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
                neighbour = neighbour + 1
              ENDIF
            ENDDO
            WRITE(getal,'(I1)') neighbour-1
!            READ(number,'(A1)') getal
            sybatom(i)=AtmElement(i)(1:1)//'.'//getal
          CASE DEFAULT
            sybatom(i)=AtmElement(i)
        END SELECT
      ENDDO
      RETURN

      END SUBROUTINE sybylatom
!
!*****************************************************************************
!
      SUBROUTINE ass_type(natom,AtmElement,bndr)

      PARAMETER(maxatom=100,maxelm=108)

      INTEGER     natom
      CHARACTER*2 AtmElement(maxatom)
      REAL        bndr(maxatom)
  
      REAL         rsd(maxelm)
      CHARACTER*2  el(maxelm)

! Elements (plus other CSD 'element' definitions What's 'Zz'??)
      DATA el  /'C ','H ','Ac','Ag','Al','Am','Ar','As','At','Au','B ', &
           'Ba','Be','Bi','Bk','Br','Ca','Cd','Ce','Cf','Cl','Cm','Co', &
           'Cr','Cs','Cu','D ','Dy','Er','Es','Eu','F ','Fe','Fm','Fr', &
           'Ga','Gd','Ge','He','Hf','Hg','Ho','I ','In','Ir','K ','Kr', &
           'La','Li','Lu','Lw','Md','Mg','Mn','Mo','N ','Na','Nb','Nd', &
           'Ne','Ni','No','Np','O ','Os','P ','Pa','Pb','Pd','Pm','Po', &
           'Pr','Pt','Pu','Ra','Rb','Re','Rh','Rn','Ru','S ','Sb','Sc', &
           'Se','Si','Sm','Sn','Sr','Ta','Tb','Tc','Te','Th','Ti','Tl', &
           'Tm','U ','V ','W ','X ','Xe','Y ','Yb','Z ','Zn','Zr','Zz', &
           'Me'/

!U! Elements (plus other CSD 'element' definitions What's 'Zz'??)
!U      DATA atnr/   6,   1,  89,  47,  13,  95,  18,  33,  85,  79,   5, &
!U             56,   4,  83,  97,  35,  20,  48,  58,  98,  17,  96,  27, &
!U             24,  55,  29,   0,  66,  68,  99,  63,   9,  26, 100,  87, &
!U             31,  64,  32,   2,  72,  80,  67,  53,  49,  77,  19,  36, &
!U             57,   3,  71,   0, 101,  12,  25,  42,   7,  11,  41,  60, &
!U             10,  28, 102,  93,   8,  76,  15,  91,  82,  46,  61,  84, &
!U             59,  78,  94,  88,  37,  75,  45,  86,  44,  16,  51,  21, &
!U             34,  14,  62,  50,  38,  73,  65,  43,  52,  90,  22,  81, &
!U             69,  92,  23,  74,   0,  54,  39,  70,   0,  30,  40,   0, &
!U              0/

! Bonding radii
      DATA rsd /0.68,0.23,1.88,1.59,1.35,1.51,1.61,1.21,0.00,1.50,0.83, &
           1.34,0.35,1.54,0.00,1.21,0.99,1.69,1.83,0.00,0.99,0.00,1.33, &
           1.35,1.67,1.52,0.23,1.75,1.73,0.00,1.99,0.64,1.34,0.00,0.00, &
           1.22,1.79,1.17,0.00,1.57,1.70,1.74,1.40,1.63,1.32,1.33,0.00, &
           1.87,0.68,1.72,0.00,0.00,1.10,1.35,1.47,0.68,0.97,1.48,1.81, &
           0.00,1.50,0.00,1.55,0.68,1.37,1.05,1.61,1.54,1.50,1.80,1.68, &
           1.82,1.50,1.53,1.90,1.47,1.35,1.45,0.00,1.40,1.02,1.46,1.44, &
           1.22,1.20,1.80,1.46,1.12,1.43,1.76,1.35,1.47,1.79,1.47,1.55, &
           1.72,1.58,1.33,1.37,0.00,1.62,1.78,1.94,0.00,1.45,1.56,0.00, &
           0.68/

      INTEGER     I, J
      LOGICAL     FOUND

! We know AtmElement, now get the bond radius
      DO I = 1, natom
        FOUND = .FALSE.
        DO J = 1, maxelm
          IF (AtmElement(I)(1:2) .EQ. el(J)(1:2)) THEN
             bndr(I) = rsd(J)
             FOUND = .TRUE.
          ENDIF
        ENDDO
        IF (.NOT. FOUND) THEN
          CALL DebugErrorMessage('Atomnumber not found')
          RETURN
        ENDIF
      ENDDO
      RETURN

      END SUBROUTINE ass_type
!
!*****************************************************************************
!
      SUBROUTINE make_bond(natom,x,bndr,nbond,bat)

      IMPLICIT NONE

      INTEGER maxatom, maxbond
      PARAMETER (maxatom=100,maxbond=100)

      INTEGER natom
      REAL    x(1:maxatom,1:3), bndr(1:maxatom)
      INTEGER nbond, bat(1:maxbond,1:2) 

      INTEGER I, J, K
      REAL    dist, tol

      nbond = 0
      IF (natom .LE. 1) RETURN
      tol = 0.5
      DO I = 1, natom-1
        DO J = I+1, natom
          dist = 0.0
          DO K = 1, 3
            dist = dist + (x(I,K)-x(J,K))**2
          ENDDO
          dist = SQRT(dist)
          IF (dist .LT. (bndr(I)+bndr(J)+tol)) THEN
            nbond = nbond + 1
            bat(nbond,1) = I
            bat(nbond,2) = J
          ENDIF
        ENDDO
      ENDDO
      RETURN

      END SUBROUTINE make_bond
!
!*****************************************************************************
!
