! @ The validity of tLattice is never checked !!!
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
      USE SAMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      CHARACTER*2  AtmElement(1:MAXATM_2)
      INTEGER      i
      REAL         Coordinates(1:3,1:MAXATM_2)
      INTEGER      InputFile
      REAL         a, b, c, alpha, beta, gamma
      REAL         tLattice(1:3,1:3)
      INTEGER      NewLength
      LOGICAL      ChrIsLetter ! Function
      REAL         tX, tY, tZ
      CHARACTER*255 tString, tSubString
      REAL         DummyReal
      INTEGER      DummyInteger
      INTEGER      WriteMol2 ! Function

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
        CASE ('TITL', 'ZERR', 'LATT', 'SYMM', 'SFAC', 'DISP', 'UNIT', 'L.S.')
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
          natcry = natcry + 1
          AtmElement(natcry)(1:2) = tString(1:2)
          IF (.NOT. ChrIsLetter(tString(2:2))) AtmElement(natcry)(2:2) = ' '
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
! Given the element, assign the CSD element (fill aelem(1:MAXATM)
      CALL AssignCSDElement(AtmElement)
      Res2Mol2 = WriteMol2(TheFileName(1:LEN_TRIM(TheFileName)-3)//'mol2')
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
!D      USE VARIABLES
!D
!D      IMPLICIT NONE
!D
      CHARACTER*(*), INTENT (IN   ) :: TheFileName
!D
!D      INTEGER maxatom, maxbond, maxelm
!D      PARAMETER (maxatom=100, maxbond=100, maxelm=108)
!D
!D      CHARACTER*2  AtmElement(maxatom)
!D      CHARACTER*4  sybatom(maxatom)
!D      CHARACTER*80 mol2file, title
!D      INTEGER      i,j,natom,nbond, bat(maxbond,2)
!D      REAL         Coordinates(3,maxatom), bndr(maxatom)
!D      INTEGER      InputFile
!D      INTEGER      OutputFile
!D      REAL         a, b, c, alpha, beta, gamma
!D      INTEGER      INORM
!D      REAL         tLattice(1:3,1:3)
!D      CHARACTER*50 tString
!D      INTEGER*4    SERIAL
!D      CHARACTER*4  NAME(maxatom)
!D      INTEGER      Ilen
!D      LOGICAL      ChrIsLetter ! Function
!D      REAL         tX, tY, tZ
!D      CHARACTER*1  ChrLowerCase
!D      INTEGER      CSD_El_nr(maxatom)
!D
!D! Initialise to 'failure'
!D      CSSR2Mol2 = 0
!D      natom = 0
!D      InputFile = 10
!D      OPEN(UNIT=InputFile,file=TheFileName,form='formatted',status='old',err=998)
!D! Initialise cell parameters to invalid values
!D! If no valid values are read, then the atomic co-ordinates must be orthogonal
!D      a = 0.0
!D      b = 0.0
!D      c = 0.0
!D      alpha = 0.0
!D      beta  = 0.0
!D      gamma = 0.0
!D! Record # 1 : unit cell lengths
!D      READ(InputFile,'(38X,3F8.3)',ERR=10,END=999) a, b, c ! On error, just continue
!D! Record # 2 : unit cell angles
!D   10 READ(InputFile,'(21X,3F8.3)',ERR=20,END=999) alpha, beta, gamma ! On error, just continue
!D! Record # 3 : number of atoms and co-ordinate system.
!D! INORM = 0   =>   fractional co-ordinates
!D! INORM = 1   =>   orthogonal co-ordinates
!D   20 READ(InputFile,'(2I4)',ERR=990,END=999) natom, INORM
!D      IF (natom .EQ. 0) THEN
!D        CALL ErrorMessage('No Atoms found.')
!D        RETURN
!D      ENDIF
!D      IF (INORM .EQ. 1) THEN
!D        a = 1.0
!D        b = 1.0
!D        c = 1.0
!D        alpha = 90.0
!D        beta  = 90.0
!D        gamma = 90.0
!D      ENDIF
!D      CALL LatticeCellParameters2Lattice(a, b, c, alpha, beta, gamma, tLattice)
!D! Record # 4 : second title, just skip.
!D      READ(InputFile,'(A50)',ERR=30,END=999) tString ! On error, just continue
!D   30 CONTINUE
!D      DO I = 1, natom
!D        READ(InputFile,'(I4,1X,A4,2X,F9.5,1X,F9.5,1X,F9.5)',ERR=990,END=999) SERIAL, NAME(I), &
!D                                                            Coordinates(1,I), Coordinates(2,I), Coordinates(3,I)
!D      ENDDO
!D! The variable X holds the atomic fractional co-ordinates, the mol2 file
!D! needs orthogonal co-ordinates => convert
!D      DO I = 1, natom
!D        tX = Coordinates(1,I) * tLattice(1,1) + Coordinates(2,I) * tLattice(1,2) + Coordinates(3,I) * tLattice(1,3)
!D        tY = Coordinates(1,I) * tLattice(2,1) + Coordinates(2,I) * tLattice(2,2) + Coordinates(3,I) * tLattice(2,3)
!D        tZ = Coordinates(1,I) * tLattice(3,1) + Coordinates(2,I) * tLattice(3,2) + Coordinates(3,I) * tLattice(3,3)
!D        Coordinates(1,I) = tX
!D        Coordinates(2,I) = tY
!D        Coordinates(3,I) = tZ
!D      ENDDO
!D! The variable NAME now holds 'Cl6 ' etc. for every atom. 
!D! We must extract the element from that
!D      DO I = 1, natom    
!D        AtmElement(I) = NAME(I)(1:2)
!D        IF (.NOT. ChrIsLetter(AtmElement(I)(2:2))) AtmElement(natom)(2:2) = ' '
!D        CALL StrUpperCase(AtmElement(I))
!D      ENDDO
!D      CLOSE(InputFile)
!D! Given the element, assign the bond radius
!D      CALL ass_type(natom,AtmElement,bndr,CSD_El_nr)
!D! Make the bonds using a simple distance criterion
!D      CALL make_bond(natom,Coordinates,bndr,nbond,bat)
!D      CALL sybylatom(natom,AtmElement,sybatom,nbond,bat)
!D      Ilen = LEN_TRIM(TheFileName)
!D! Replace 'cssr' by 'mol2'
!D      mol2file = TheFileName(1:Ilen-4)//'mol2'
!D      OutputFile = 3
!D      OPEN(UNIT=OutputFile,file=mol2file,form='formatted',err=997)
!D      WRITE(OutputFile,"('@<TRIPOS>MOLECULE')")
!D      WRITE(OutputFile,'(A)') 'Temporary file'
!D      WRITE(OutputFile,"(2(I5,1X),'    1     0     0')") natom, nbond
!D      WRITE(OutputFile,"('SMALL')")
!D      WRITE(OutputFile,"('NO_CHARGES')")
!D      title = 'Temporary file created by DASH'
!D      WRITE(OutputFile,"(A80)") title
!D      WRITE(OutputFile,"('@<TRIPOS>ATOM')")
!D      DO i = 1, natom
!D        AtmElement(i)(2:2) = ChrLowerCase(AtmElement(i)(2:2))
!D        WRITE(OutputFile,270) i,NAME(I),(Coordinates(j,i),j=1,3),sybatom(i)
!D  270   FORMAT(I3,1X,A4,1X,3(F10.4,1X),A4,' 1 <1> 0.0')
!D      ENDDO
!D      WRITE(OutputFile,"('@<TRIPOS>BOND')")
!D      DO i = 1, nbond
!D        WRITE(OutputFile,'(4(I3,1X))') i,bat(i,1),bat(i,2),1
!D      ENDDO
!D      CLOSE(OutputFile)
      CSSR2Mol2 = 1
!D      RETURN
!D  990 CALL ErrorMessage('Error while reading input file.')
!D      RETURN
!D  997 CALL ErrorMessage('Error opening mol2file.')
!D      RETURN 
!D  998 CALL ErrorMessage('Error opening input file.')
!D      RETURN
!D  999 CALL ErrorMessage('Unexpected end of input file.')
!D      RETURN

      END FUNCTION CSSR2Mol2
!
!*****************************************************************************
!
      SUBROUTINE AssignCSDElement(AtmElement)

      USE SAMVAR

      INTEGER maxelm
      PARAMETER (maxelm=108)

      CHARACTER*2 AtmElement(MAXATM_2)
  
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

      INTEGER I, J
      LOGICAL FOUND
      CHARACTER*2 tElem
      CHARACTER*1 ChrUpperCase ! Function

! We know AtmElement, now get the CSD element number
      DO I = 1, natcry
        FOUND = .FALSE.
        tElem(1:1) = ChrUpperCase(AtmElement(I)(1:1))
        tElem(2:2) = ChrUpperCase(AtmElement(I)(2:2))
        DO J = 1, maxelm
          IF (tElem(1:2) .EQ. el(J)(1:2)) THEN
             aelem(I) = J
             FOUND = .TRUE.
             EXIT
          ENDIF
        ENDDO
        IF (.NOT. FOUND) CALL DebugErrorMessage('Element '//AtmElement(I)(1:2)//' not found')
      ENDDO

      END SUBROUTINE AssignCSDElement
!
!*****************************************************************************
!
