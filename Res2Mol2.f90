! @ The validity of tLattice is never checked !!!
!
!*****************************************************************************
!
      INTEGER FUNCTION Res2Mol2(TheFileName)
!
! This subroutine tries to read a SHELX .res file and tries to convert it to a Sybyl .mol2 file.
! The .mol2 file stuff was taken from 'cad2mol2.f' by Jos Lommerse.
! Bonds are calculated from the distances between 2 atoms.
!
! Free format.
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

      IMPLICIT NONE

      CHARACTER*MaxPathLength, INTENT (IN   ) :: TheFileName

      INTEGER maxatom, maxbond, maxelm
      PARAMETER (maxatom=100, maxbond=100, maxelm=108)

      CHARACTER*2  AtmElement(maxatom)
      CHARACTER*4  sybatom(maxatom)
      CHARACTER*80 mol2file, title
      INTEGER      i,j,natom,nbond, bat(maxbond,2)
      REAL         Coordinates(3,maxatom), bndr(maxatom)
      INTEGER      InputFile
      INTEGER      OutputFile
      REAL         a, b, c, alpha, beta, gamma
      REAL         tLattice(1:3,1:3)
      INTEGER      Ilen
      LOGICAL      ChrIsLetter ! Function
      REAL         tX, tY, tZ
      CHARACTER*255 tString
      REAL         DummyReal
      CHARACTER*4  DummyChar4
      INTEGER      DummyInteger
      CHARACTER*1  ChrLowerCase ! Function

! Initialise to 'failure'
      Res2Mol2 = 0
      natom = 0
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
          READ(tString,*,ERR=990,END=990) DummyChar4, DummyReal, a, b, c, alpha, beta, gamma
          CALL LatticeCellParameters2Lattice(a, b, c, alpha, beta, gamma, tLattice)
!        CASE ('TITL', 'ZERR', 'LATT', 'SYMM', 'SFAC', 'DISP', 'UNIT')
        CASE ('HKLF', 'END ') ! We have finished, process the data we have read
          GOTO 100
        CASE DEFAULT ! If it's none of the above, it is probably an atom
! Check if first character is a letter. If not, skip this line and read the next.
          IF (.NOT. ChrIsLetter(tString(1:1))) GOTO 10
! If the third character is still a letter, it couldn't have been an element.
! Skip this line and read the next. (Note that 'C X', where C an element and X any letter, 
! should not be possible.)
          IF (ChrIsLetter(tString(3:3))) GOTO 10
! Now we are up to the point where we can assume that the first one or two characters,
! but not the third, form an element.
          natom = natom + 1
          AtmElement(natom)(1:2) = tString(1:2)
          IF (.NOT. ChrIsLetter(tString(2:2))) AtmElement(natom)(2:2) = ' '
          READ(tString,*,ERR=990) DummyChar4, DummyInteger, Coordinates(1,natom), Coordinates(2,natom), Coordinates(3,natom)
      END SELECT
! Read next line
      GOTO 10
  100 CONTINUE
      IF (natom .EQ. 0) THEN
        CALL ErrorMessage('No Atoms found.')
        RETURN
      ENDIF
! The variable Coordinates holds the atomic fractional co-ordinates, the mol2 file
! needs orthogonal co-ordinates => convert
      DO I = 1, natom
        tX = Coordinates(1,I) * tLattice(1,1) + Coordinates(2,I) * tLattice(1,2) + Coordinates(3,I) * tLattice(1,3)
        tY = Coordinates(1,I) * tLattice(2,1) + Coordinates(2,I) * tLattice(2,2) + Coordinates(3,I) * tLattice(2,3)
        tZ = Coordinates(1,I) * tLattice(3,1) + Coordinates(2,I) * tLattice(3,2) + Coordinates(3,I) * tLattice(3,3)
        Coordinates(1,I) = tX
        Coordinates(2,I) = tY
        Coordinates(3,I) = tZ
      ENDDO
      CLOSE(InputFile)
! Given the element, assign the bond radius
      CALL ass_type(natom,AtmElement,bndr)
! Make the bonds using a simple distance criterion
      CALL make_bond(natom,Coordinates,bndr,nbond,bat)
      CALL sybylatom(natom,AtmElement,sybatom,nbond,bat)
      Ilen = LEN_TRIM(TheFileName)
! Replace 'res' by 'mol2'
      mol2file = TheFileName(1:Ilen-3)//'mol2'
      OutputFile = 3
      OPEN(UNIT=OutputFile,file=mol2file,form='formatted',err=997)
      WRITE(OutputFile,"('@<TRIPOS>MOLECULE')")
      WRITE(OutputFile,'(A)') 'Temporary file'
      WRITE(OutputFile,"(2(I5,1X),'    1     0     0')") natom, nbond
      WRITE(OutputFile,"('SMALL')")
      WRITE(OutputFile,"('NO_CHARGES')")
      title = 'Temporary file created by DASH'
      WRITE(OutputFile,"(A80)") title
      WRITE(OutputFile,"('@<TRIPOS>ATOM')")
      DO i = 1, natom
        AtmElement(i)(2:2) = ChrLowerCase(AtmElement(i)(2:2))
        WRITE(OutputFile,270) i,AtmElement(i),(Coordinates(j,i),j=1,3),sybatom(i)
      ENDDO
  270 FORMAT(I3,1X,A2,1X,3(F10.4,1X),A4,' 1 <1> 0.0')
      WRITE(OutputFile,"('@<TRIPOS>BOND')")
      DO i = 1, nbond
        WRITE(OutputFile,'(4(I3,1X))') i,bat(i,1),bat(i,2),1
      ENDDO
      CLOSE(OutputFile)
      Res2Mol2 = 1
      RETURN
  990 CALL ErrorMessage('Error while reading input file.')
      RETURN
  997 CALL ErrorMessage('Error opening mol2file.')
      RETURN 
  998 CALL ErrorMessage('Error opening input file.')
      RETURN

      END FUNCTION Res2Mol2
!
!*****************************************************************************
!
      INTEGER FUNCTION CSSR2Mol2(TheFileName)
!
! This subroutine tries to read a .cssr file and tries to convert it to a Sybyl .mol2 file.
! The .mol2 file stuff was taken from 'cad2mol2.f' by Jos Lommerse.
! Bonds are calculated from the distances between 2 atoms.
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

      IMPLICIT NONE

      CHARACTER*MaxPathLength, INTENT (IN   ) :: TheFileName

      INTEGER maxatom, maxbond, maxelm
      PARAMETER (maxatom=100, maxbond=100, maxelm=108)

      CHARACTER*2  AtmElement(maxatom)
      CHARACTER*4  sybatom(maxatom)
      CHARACTER*80 mol2file, title
      INTEGER      i,j,natom,nbond, bat(maxbond,2)
      REAL         Coordinates(3,maxatom), bndr(maxatom)
      INTEGER      InputFile
      INTEGER      OutputFile
      REAL         a, b, c, alpha, beta, gamma
      INTEGER      INORM
      REAL         tLattice(1:3,1:3)
      CHARACTER*50 tString
      INTEGER*4    SERIAL
      CHARACTER*4  NAME(maxatom)
      INTEGER      I1, I2, Ilen
      LOGICAL      ChrIsLetter ! Function
      REAL         tX, tY, tZ
      CHARACTER*1  ChrLowerCase

! Initialise to 'failure'
      CSSR2Mol2 = 0
      natom = 0
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
                                                            Coordinates(1,I), Coordinates(2,I), Coordinates(3,I)
      ENDDO
! The variable X holds the atomic fractional co-ordinates, the mol2 file
! needs orthogonal co-ordinates => convert
      DO I = 1, natom
        tX = Coordinates(1,I) * tLattice(1,1) + Coordinates(2,I) * tLattice(1,2) + Coordinates(3,I) * tLattice(1,3)
        tY = Coordinates(1,I) * tLattice(2,1) + Coordinates(2,I) * tLattice(2,2) + Coordinates(3,I) * tLattice(2,3)
        tZ = Coordinates(1,I) * tLattice(3,1) + Coordinates(2,I) * tLattice(3,2) + Coordinates(3,I) * tLattice(3,3)
        Coordinates(1,I) = tX
        Coordinates(2,I) = tY
        Coordinates(3,I) = tZ
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
          CALL StrUpperCase(AtmElement(I))
        ENDDO
      ENDDO
      CLOSE(InputFile)
! Given the element, assign the bond radius
      CALL ass_type(natom,AtmElement,bndr)
! Make the bonds using a simple distance criterion
      CALL make_bond(natom,Coordinates,bndr,nbond,bat)
      CALL sybylatom(natom,AtmElement,sybatom,nbond,bat)
      Ilen = LEN_TRIM(TheFileName)
! Replace 'cssr' by 'mol2'
      mol2file = TheFileName(1:Ilen-4)//'mol2'
      OutputFile = 3
      OPEN(UNIT=OutputFile,file=mol2file,form='formatted',err=997)
      WRITE(OutputFile,"('@<TRIPOS>MOLECULE')")
      WRITE(OutputFile,'(A)') 'Temporary file'
      WRITE(OutputFile,"(2(I5,1X),'    1     0     0')") natom, nbond
      WRITE(OutputFile,"('SMALL')")
      WRITE(OutputFile,"('NO_CHARGES')")
      title = 'Temporary file created by DASH'
      WRITE(OutputFile,"(A80)") title
      WRITE(OutputFile,"('@<TRIPOS>ATOM')")
      DO i = 1, natom
        AtmElement(i)(2:2) = ChrLowerCase(AtmElement(i)(2:2))
        WRITE(OutputFile,270) i,AtmElement(i),(Coordinates(j,i),j=1,3),sybatom(i)
      ENDDO
  270 FORMAT(I3,1X,A2,1X,3(F10.4,1X),A4,' 1 <1> 0.0')
      WRITE(OutputFile,"('@<TRIPOS>BOND')")
      DO i = 1, nbond
        WRITE(OutputFile,'(4(I3,1X))') i,bat(i,1),bat(i,2),1
      ENDDO
      CLOSE(OutputFile)
      CSSR2Mol2 = 1
      RETURN
  990 CALL ErrorMessage('Error while reading input file.')
      RETURN
  997 CALL ErrorMessage('Error opening mol2file.')
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

      IMPLICIT NONE

      INTEGER maxatom, maxbond, maxelm
      PARAMETER (maxatom=100, maxbond=100, maxelm=108)

      INTEGER      natom
      CHARACTER*2  AtmElement(maxatom)
      CHARACTER*4  sybatom(maxatom)
      INTEGER      nbond
      INTEGER      bat(maxbond,2)

      CHARACTER*1  getal
      INTEGER      i, j, NumOfNeighbours
      CHARACTER*1  ChrLowerCase, ChrUpperCase ! Function

      DO i = 1, natom
        sybatom(i) = '    '
        SELECT CASE (AtmElement(i)(1:2))
          CASE ('C ', 'N ', 'O ')
            NumOfNeighbours = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
              NumOfNeighbours = NumOfNeighbours + 1
              ENDIF
            ENDDO
            IF (AtmElement(i)(1:1) .EQ. 'C') NumOfNeighbours = NumOfNeighbours - 1
            IF (AtmElement(i)(1:1) .EQ. 'O') NumOfNeighbours = NumOfNeighbours + 1
            WRITE(getal,'(I1)') NumOfNeighbours
            sybatom(i)=AtmElement(i)(1:1)//'.'//getal
          CASE DEFAULT
            sybatom(i)(1:1)=ChrUpperCase(AtmElement(i)(1:1))
            sybatom(i)(2:2)=ChrLowerCase(AtmElement(i)(2:2))
        END SELECT
      ENDDO
      RETURN

      END SUBROUTINE sybylatom
!
!*****************************************************************************
!
      SUBROUTINE ass_type(natom,AtmElement,bndr)

      INTEGER maxatom, maxbond, maxelm
      PARAMETER (maxatom=100, maxbond=100, maxelm=108)

      INTEGER     natom
      CHARACTER*2 AtmElement(maxatom)
      REAL        bndr(maxatom)
  
      REAL         rsd(maxelm)
      CHARACTER*2  el(maxelm)

! Elements (plus other CSD 'element' definitions What's 'ZZ'??)
      DATA el  /'C ','H ','AC','AG','AL','AM','AR','AS','AT','AU','B ', &
           'BA','BE','BI','BK','BR','CA','CD','CE','CF','CL','CM','CO', &
           'CR','CS','CU','D ','DY','ER','ES','EU','F ','FE','FM','FR', &
           'GA','GD','GE','HE','HF','HG','HO','I ','IN','IR','K ','KR', &
           'LA','LI','LU','LW','MD','MG','MN','MO','N ','NA','NB','ND', &
           'NE','NI','NO','NP','O ','OS','P ','PA','PB','PD','PM','PO', &
           'PR','PT','PU','RA','RB','RE','RH','RN','RU','S ','SB','SC', &
           'SE','SI','SM','SN','SR','TA','TB','TC','TE','TH','TI','TL', &
           'TM','U ','V ','W ','X ','XE','Y ','YB','Z ','ZN','ZR','ZZ', &
           'ME'/

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
          CALL DebugErrorMessage('Element '//AtmElement(I)(1:2)//' not found')
          bndr(I) = 1.0
        ENDIF
      ENDDO
      RETURN

      END SUBROUTINE ass_type
!
!*****************************************************************************
!
      SUBROUTINE make_bond(natom,Coordinates,bndr,nbond,bat)

      IMPLICIT NONE

      INTEGER maxatom, maxbond, maxelm
      PARAMETER (maxatom=100, maxbond=100, maxelm=108)

      INTEGER natom
      REAL    Coordinates(1:3,1:maxatom), bndr(1:maxatom)
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
            dist = dist + (Coordinates(K,I)-Coordinates(K,J))**2
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
