!
!*****************************************************************************
!
      SUBROUTINE Res2Mol2(TheFileName)
!
! This subroutine tries to read a SHELX .res file and tries to convert it to a Sybyl .mol2 file.
! The .mol2 file stuff was taken from 'cad2mol2.f' by Jos Lommerse.
! Bonds are calculated from the distances between 2 atoms.
!
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*MaxPathLength, INTENT (IN   ) :: TheFileName

      INTEGER maxatom, maxbond, maxdimer, maxelm, maxatom_sup, maxbond_sup
      PARAMETER (maxatom=100,maxbond=100,maxdimer=100,maxelm=108, maxatom_sup=1000,maxbond_sup=1000)

      CHARACTER*2  attype(maxatom)
      CHARACTER*6  atom(maxatom)
      CHARACTER*4  sybatom(maxatom)
      CHARACTER*80 mol2file,title,nix
      CHARACTER*120 word(40),line
      INTEGER      i,j,natom,nbond, bat(maxbond,2), nwords
      REAL         x(maxatom,3),atomno(maxatom),  vdwr(maxatom),bndr(maxatom)
      LOGICAL      IMPTPUN,IMPTOUT

      natom = 0
      nbond = 0
      OPEN(unit=2,file=TheFileName,form='formatted',status='old',err=998)
      mol2file = 'Temp.mol2'
      OPEN(unit=3,file=mol2file,form='formatted',err=997)

      if (IMPTPUN) then
         read(2,'(A80)') nix
         read(2,'(A80)') title
      elseif (IMPTOUT) then
         read(2,'(A120)') line
         CALL line_to_words(line,word,nwords)
         do while ((word(2).NE.'X') .AND. (word(3).NE.'Y') .AND. (word(4).NE.'Z'))
            read(2,'(A120)') line
            CALL line_to_words(line,word,nwords)
         enddo
         read(2,'(A120)') line
      endif
      natom = 0
      read(2,'(A120)') line
      CALL line_to_words(line,word,nwords)
      do while ((word(1)(1:3).NE.'Es:') .AND. (word(1)(1:3).NE.'---'))
         natom = natom + 1
         read(word(1),'(A)') atom(natom)
         read(word(2),*) atomno(natom)
         read(word(3),*) x(natom,1)
         read(word(4),*) x(natom,2)
         read(word(5),*) x(natom,3)
         read(2,'(A120)') line
         CALL line_to_words(line,word,nwords)
      enddo
      close(2)

      IF (natom .EQ. 0) THEN
        CALL ErrorMessage('No Atoms found.')
        RETURN
      ENDIF
      CALL ass_type(natom,atomno,attype,vdwr,bndr)
      CALL make_bond(natom,x,bndr,nbond,bat)
      CALL sybylatom(natom,attype,sybatom,nbond,bat)
      write(3,"('@<TRIPOS>MOLECULE')")
      write(3,'(A)') 'Temporary file'
      write(3,220) natom,nbond
      write(3,"('SMALL')")
      write(3,"('NO_CHARGES')")
      write(3,"(A80)") title
      write(3,"('@<TRIPOS>ATOM')")
      do i=1,natom
         write(3,270) i,atom(i),(x(i,j),j=1,3),sybatom(i),1,1
      enddo
      write(3,"('@<TRIPOS>BOND')")
      do i=1,nbond
        write(3,'(4(I3,X))') i,bat(i,1),bat(i,2),1
      enddo
      close(3)
  220 FORMAT(2(I5,X),'    1     0     0')
  270 FORMAT(I3,X,A2,X,3(F10.4,X),A4,X,I1,X,'<',I1,'> 0.0')
  997 stop 'Error opening mol2file'
  998 stop 'Error opening readfile' 

      END SUBROUTINE Res2Mol2
!
!*****************************************************************************
!
      SUBROUTINE sybylatom(natom,attype,sybatom,nbond,bat)

      PARAMETER(maxatom=100,maxbond=100)

      CHARACTER*1  number, getal
      CHARACTER*2  attype(maxatom)
      CHARACTER*4  sybatom(maxatom)
      INTEGER      i, j, neighbour, bat(maxbond,2)

      DO i = 1, natom
        SELECT CASE (attype(i)(1:2))
          CASE ('O ')
            neighbour = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
              neighbour = neighbour + 1
              ENDIF
            ENDDO
            WRITE(number,'(I1)') neighbour+1
            READ(number,'(A1)') getal
            sybatom(i)=attype(i)(1:1)//'.'//getal
          CASE ('N ')
            neighbour = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
                neighbour = neighbour + 1
              ENDIF
            ENDDO
            WRITE(number,'(I1)') neighbour
            READ(number,'(A1)') getal
            sybatom(i)=attype(i)(1:1)//'.'//getal
          CASE ('C ')
            neighbour = 0
            DO j = 1, nbond
              IF ((bat(j,1).EQ.i) .OR. (bat(j,2).EQ.i)) THEN
                neighbour = neighbour + 1
              ENDIF
            ENDDO
            WRITE(number,'(I1)') neighbour-1
            READ(number,'(A1)') getal
            sybatom(i)=attype(i)(1:1)//'.'//getal
          CASE DEFAULT
            sybatom(i)=attype(i)
        END SELECT
      ENDDO
      RETURN

      END SUBROUTINE sybylatom
!
!*****************************************************************************
!
      SUBROUTINE line_to_words(line,word,nwords)
!!-- This subroutine maps the input string into the array words and
!!-- returns the number of words
!!-- As delimiters spaces only are allowed

      CHARACTER*120 line,word(40)
      INTEGER, INTENT (  OUT) :: nwords
      CHARACTER letter

      DO I = 1, 40 
        word(I) = ' '
      ENDDO
      nwords = 0
      ip = 1
      DO I = 1, 120
        letter = line(I:I)
        IF (letter .NE. ' ') THEN
          IF (ip .EQ. 1) nwords = nwords + 1
          word(nwords)(ip:ip) = letter
          ip = ip + 1
        ELSE
          ip = 1
        ENDIF
      ENDDO
      RETURN

      END SUBROUTINE line_to_words
!
!*****************************************************************************
!
      SUBROUTINE ass_type(natom,atomno,attype,vdwr,bndr)

      PARAMETER(maxatom=100,maxelm=108)
  
      REAL         rvdw(maxelm),rsd(maxelm)
      INTEGER      atnr(maxelm)
      CHARACTER*2  el(maxelm)

! Van der Waals radii
! Methylgroup (Me) added (element 108)
! For H (element no. 2) rvdw=1.10 Angstrom (in stead of 1.20)
      DATA rvdw/1.70,1.10,2.00,1.72,2.00,2.00,1.88,1.85,2.00,1.66,2.00, &
           2.00,2.00,2.00,2.00,1.85,2.00,1.58,2.00,2.00,1.75,2.00,2.00, &
           2.00,2.00,1.40,1.20,2.00,2.00,2.00,2.00,1.47,2.00,2.00,2.00, &
           1.87,2.00,2.00,1.40,2.00,1.55,2.00,1.98,1.93,2.00,2.75,2.02, &
           2.00,1.82,2.00,2.00,2.00,1.73,2.00,2.00,1.55,2.27,2.00,2.00, &
           1.54,1.63,2.00,2.00,1.52,2.00,1.80,2.00,2.02,1.63,2.00,2.00, &
           2.00,1.72,2.00,2.00,2.00,2.00,2.00,2.00,2.00,1.80,2.00,2.00, &
           1.90,2.10,2.00,2.17,2.00,2.00,2.00,2.00,2.06,2.00,2.00,1.96, &
           2.00,1.86,2.00,2.00,2.00,2.16,2.00,2.00,2.00,1.39,2.00,2.00, &
           2.00/

! Elements (plus other CSD 'element' definitions What's 'Zz'??)
      DATA el/'C ','H ','Ac','Ag','Al','Am','Ar','As','At','Au','B ',   &
           'Ba','Be','Bi','Bk','Br','Ca','Cd','Ce','Cf','Cl','Cm','Co', &
           'Cr','Cs','Cu','D ','Dy','Er','Es','Eu','F ','Fe','Fm','Fr', &
           'Ga','Gd','Ge','He','Hf','Hg','Ho','I ','In','Ir','K ','Kr', &
           'La','Li','Lu','Lw','Md','Mg','Mn','Mo','N ','Na','Nb','Nd', &
           'Ne','Ni','No','Np','O ','Os','P ','Pa','Pb','Pd','Pm','Po', &
           'Pr','Pt','Pu','Ra','Rb','Re','Rh','Rn','Ru','S ','Sb','Sc', &
           'Se','Si','Sm','Sn','Sr','Ta','Tb','Tc','Te','Th','Ti','Tl', &
           'Tm','U ','V ','W ','X ','Xe','Y ','Yb','Z ','Zn','Zr','Zz', &
           'Me'/
! Elements (plus other CSD 'element' definitions What's 'Zz'??)
      DATA atnr/6,1,89,47,13,95,18,33,85,79,5,                          &
           56,4,83,97,35,20,48,58,98,17,96,27,                          &
           24,55,29,0,66,68,99,63,9,26,100,87,                          &
           31,64,32,2,72,80,67,53,49,77,19,36,                          &
           57,3,71,0,101,12,25,42,7,11,41,60,                           &
           10,28,102,93,8,76,15,91,82,46,61,84,                         &
           59,78,94,88,37,75,45,86,44,16,51,21,                         &
           34,14,62,50,38,73,65,43,52,90,22,81,                         &
           69,92,23,74,0,54,39,70,0,30,40,0,                            &
           0/

! Bonding radii
      DATA rsd/0.68,0.23,1.88,1.59,1.35,1.51,1.61,1.21,0.00,1.50,0.83,  &
           1.34,0.35,1.54,0.00,1.21,0.99,1.69,1.83,0.00,0.99,0.00,1.33, &
           1.35,1.67,1.52,0.23,1.75,1.73,0.00,1.99,0.64,1.34,0.00,0.00, &
           1.22,1.79,1.17,0.00,1.57,1.70,1.74,1.40,1.63,1.32,1.33,0.00, &
           1.87,0.68,1.72,0.00,0.00,1.10,1.35,1.47,0.68,0.97,1.48,1.81, &
           0.00,1.50,0.00,1.55,0.68,1.37,1.05,1.61,1.54,1.50,1.80,1.68, &
           1.82,1.50,1.53,1.90,1.47,1.35,1.45,0.00,1.40,1.02,1.46,1.44, &
           1.22,1.20,1.80,1.46,1.12,1.43,1.76,1.35,1.47,1.79,1.47,1.55, &
           1.72,1.58,1.33,1.37,0.00,1.62,1.78,1.94,0.00,1.45,1.56,0.00, &
           0.68/

      CHARACTER*2 attype(maxatom)
      INTEGER     atomnr,natom,i,j
      REAL        atomno(maxatom),vdwr(maxatom),bndr(maxatom)
      LOGICAL     FOUND

      DO I = 1, natom
        atomnr = INT(atomno(I))
        FOUND = .FALSE.
        DO J = 1, maxelm
          IF (atnr(J) .EQ. atomnr) THEN
             attype(I) = el(J)
             vdwr(I) = rvdw(J)
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

      PARAMETER (maxatom=100,maxbond=100)

      INTEGER natom
      REAL    x(1:maxatom,1:3), bndr(1:maxatom)
      INTEGER nbond, bat(1:maxbond,1:2) 

      REAL    dist, tol

      tol = 0.5
      nbond = 0
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
