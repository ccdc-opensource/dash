!
!*****************************************************************************
!
      INTEGER FUNCTION Read_One_Zm(iFrg)

      USE ZMVAR
      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg
!
!Zmatrix generated by Mercury
!1.0 1.0 1.0 90.0 90.0 90.0
!  59   0
!  C      0.0000000  0    0.0000000  0    0.0000000  0    0    0    0  3.0  1.0   23 C3
!  C      1.4851024  0    0.0000000  0    0.0000000  0    1    0    0  3.0  1.0   11 C3 C3
!  O      1.4315310  0  103.9587653  0    0.0000000  0    1    2    0  3.0  1.0   24 O1 C3 C3
!  C      1.5029528  0  114.1777953  0  120.1869348  0    1    2    3  3.0  1.0   35 C3 C3 C3 O1
!  O      1.4285269  0  106.2731908  0 -177.0187547  0    2    1    3  3.0  1.0   12 O5 C3 C3 O1
! <SNIP>
!  C      1.5152617  0  113.2370014  0 -179.8250018  0   54   51   48  3.0  1.0   58 C6 C7 C8 C9
!  C      1.4929873  0  113.1748255  0 -179.7006193  1   55   52   49  3.0  1.0   38 C7 C8 C9 C1
!  C      1.5029990  0  113.0372990  0  179.9092170  0   57   54   51  3.0  1.0   59 C5 C6 C7 C8
!
      INTEGER, EXTERNAL :: GetNumOfColumns
      CHARACTER*1, EXTERNAL :: ChrLowerCase, ChrUpperCase
      INTEGER, EXTERNAL :: ElmSymbol2CSD
      CHARACTER*255 line
      CHARACTER*255 tSubString
      INTEGER ErrorStatus
      REAL ta, tb, tc, talpha, tbeta, tgamma
      INTEGER I, J, NumCol
      INTEGER nlin, natof
      INTEGER AsymLen, IDlen
      CHARACTER*3 tIDstr, tStr3

! JCC Initialise return value to successful (zero)
! If the return value is non-zero, then an error occurred. The return status corresponds
! to the appropriate FOR$IOS flag
      Read_One_Zm = 0
! JCC Added in error trap for missing file/ file opening problems
      OPEN (UNIT=19, FILE=frag_file(iFrg), STATUS='old', ERR=999, IOSTAT=ErrorStatus)
! JCC Added in a read error trap, in case user selects incorrect file
! First line is a title
      READ (19, '(A)', ERR=999, IOSTAT=ErrorStatus) line
! Second line contains unit cell parameters
!1.0 1.0 1.0 90.0 90.0 90.0
! These are never used, so just read into dummy variables.
      READ (19, *, ERR=999, IOSTAT=ErrorStatus) ta, tb, tc, talpha, tbeta, tgamma
! Third line contains number of atoms and an integer IAT, defining the centre for the rotations.
! IAT = 0        : use centre of mass
! IAT = non-zero : use atom nr. IAT   (necessary if atom on special position).
!  59   0
      READ (19, *, ERR=999, IOSTAT=ErrorStatus) natof, icomflg(iFrg)
      IF (natof .GT. maxatm) THEN
        CALL WarningMessage('Z-matrix contains too many atoms--truncated.')
        natof = maxatm
      ENDIF
      natoms(iFrg) = natof
      DO i = 1, natoms(iFrg)
! Remaining lines contain the Z-matrix
!  C      1.5152617  0  113.2370014  0 -179.8250018  0   54   51   48  3.0  1.0   58 C6 C7 C8 C9
        READ (19, 1900, ERR=999, IOSTAT=ErrorStatus) nlin, line
! JCC Added in traps on internal read
        READ (line(3:5), '(A3)', ERR=999, IOSTAT=ErrorStatus) tStr3
        CALL StrClean(tStr3,AsymLen)
        ElSym(i, iFrg)(1:1) = ChrUpperCase(tStr3(1:1))
        ElSym(i, iFrg)(2:2) = ChrLowerCase(tStr3(2:2))
        zmElementCSD(i, iFrg) = ElmSymbol2CSD(ElSym(i, iFrg))
! First item--the element--has been read. Rest more tricky.
! First, convert tabs to spaces and remove redundant spaces
        CALL StrClean(line, nlin)
! First column--the element--has been read: remove it
        CALL GetSubString(Line, ' ', tSubString)
! Then count the number of columns
        NumCol = GetNumOfColumns(Line)
        IF (NumCol .LT. 11) THEN
          ErrorStatus = 99
          GOTO 999
        ENDIF
        READ (line(1:nlin), *, ERR=999, IOSTAT=ErrorStatus) blen(i, iFrg),&
              ioptb(i, iFrg), alph(i, iFrg), iopta(i, iFrg), bet(i, iFrg),&
              ioptt(i, iFrg), iz1(i, iFrg), iz2(i, iFrg), iz3(i, iFrg),   &
              tiso(i, iFrg), occ(i, iFrg)
! Adjust torsion angle to be between -180.0 and +180.0
        DO WHILE (bet(i, iFrg) .LT. -180.0)
          bet(i, iFrg) = bet(i, iFrg) + 360.0
        ENDDO
        DO WHILE (bet(i, iFrg) .GT.  180.0)
          bet(i, iFrg) = bet(i, iFrg) - 360.0
        ENDDO
! Remove what we have just read from the string
        DO J = 1, 11
          CALL GetSubString(Line, ' ', tSubString)
        ENDDO
        nlin = LEN_TRIM(Line)
! How many columns do we have left?
        NumCol = GetNumOfColumns(Line)
        IF (NumCol .EQ. 0) THEN
          izmoid(i, iFrg) = i
        ELSE
          READ (line(1:nlin), *, IOSTAT=ErrorStatus) izmoid(i, iFrg)
        ENDIF
        IF (NumCol .GE. 2) THEN
          CALL GetSubString(Line, ' ', tSubString) ! That should be the original atom number
          CALL GetSubString(Line, ' ', tSubString) ! And that should be what we really want:
                                                 ! the original atom label
        ELSE
! Emulate original atom labels by adding original atom number to element, e.g. 'C4'
          WRITE(tIDstr,'(I3)') izmoid(i, iFrg)
          CALL StrClean(tIDstr, IDlen)
          tSubString = ElementStr(zmElementCSD(i, iFrg))
          tSubString = tSubString(1:LEN_TRIM(tSubString))//tIDstr
        ENDIF
        OriginalLabel(i, iFrg) = tSubString(1:5)
        izmbid(izmoid(i, iFrg), iFrg) = i   ! the backward mapping from atoms in the Z-matrix
      ENDDO
      CLOSE (19)
! Initialise all the stuff that isn't present in a .zmatrix file
      UseQuaternions(iFrg) = .TRUE.
      zmInitialQs(0, iFrg) = 1.0
      zmInitialQs(1, iFrg) = 0.0
      zmInitialQs(2, iFrg) = 0.0
      zmInitialQs(3, iFrg) = 0.0
      zmSingleRAIniOrDef(iFrg) = 1 ! 1 = Align with axis (only possible when axis itself is defined from atoms)
      zmSingleRAIniOrFrac(1, iFrg) = 0.0 ! Fractional co-ords of axis to align with
      zmSingleRAIniOrFrac(2, iFrg) = 0.0 ! Fractional co-ords of axis to align with
      zmSingleRAIniOrFrac(3, iFrg) = 1.0 ! Fractional co-ords of axis to align with
      zmSingleRAIniOrEuler(1, iFrg) = 0.0 ! The Euler angles
      zmSingleRAIniOrEuler(2, iFrg) = 0.0 ! The Euler angles
      zmSingleRAIniOrEuler(3, iFrg) = 0.0 ! The Euler angles
      zmSingleRAIniOrQuater(0, iFrg) = 1.0 ! The quaternions
      zmSingleRAIniOrQuater(1, iFrg) = 0.0 ! The quaternions
      zmSingleRAIniOrQuater(2, iFrg) = 0.0 ! The quaternions
      zmSingleRAIniOrQuater(3, iFrg) = 0.0 ! The quaternions
      zmSingleRotAxDef(iFrg) = 3  ! 3 = normal to plane
      zmSingleRotAxAtm(1, iFrg) = izmbid(1, iFrg)
      zmSingleRotAxAtm(2, iFrg) = izmbid(2, iFrg)
      zmSingleRotAxFrac(1, iFrg) = 0.0
      zmSingleRotAxFrac(2, iFrg) = 0.0
      zmSingleRotAxFrac(3, iFrg) = 1.0
      zmSingleRotAxPlnAtm(1, iFrg) = izmbid(1, iFrg)
      zmSingleRotAxPlnAtm(2, iFrg) = izmbid(2, iFrg)
      zmSingleRotAxPlnAtm(3, iFrg) = izmbid(3, iFrg)
      zmSingleRotationQs(0, iFrg) = 1.0
      zmSingleRotationQs(1, iFrg) = 0.0
      zmSingleRotationQs(2, iFrg) = 0.0
      zmSingleRotationQs(3, iFrg) = 0.0
      CALL zmDoAdmin(iFrg)
! Now precalculate the bonds
      CALL zmGenerateBonds(iFrg)
      RETURN
! Return status for failed reading and failed opening
  999 Read_One_Zm = ErrorStatus
      CLOSE (19)
      RETURN
 1900 FORMAT (Q,A)

      END FUNCTION Read_One_Zm
!
!*****************************************************************************
!
      SUBROUTINE zmDoAdmin(iFrg)

      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER i, ii, izm

! izmpar = number of degrees of freedom ('parameters')
! kzmpar = type of parameter (1 = translation, 2 = rotation)
! xzmpar = initial value of parameter
! czmpar = Character string associated with this parameter value
      czmpar(1, iFrg) = ' x(frag )'
      czmpar(2, iFrg) = ' y(frag )'
      czmpar(3, iFrg) = ' z(frag )'
      DO ii = 1, 3
        kzmpar(ii, iFrg) = 1 ! Translation
        xzmpar(ii, iFrg) = 0.5
      ENDDO
      IF (natoms(iFrg) .EQ. 1) THEN
! Single atom: no rotations
        izmpar(iFrg) = 3
      ELSE IF (UseQuaternions(iFrg)) THEN
! Molecule with quaternions
        izmpar(iFrg) = 7
        czmpar(4, iFrg) = 'Q0(frag )'
        czmpar(5, iFrg) = 'Q1(frag )'
        czmpar(6, iFrg) = 'Q2(frag )'
        czmpar(7, iFrg) = 'Q3(frag )'
        DO ii = 4, 7
          kzmpar(ii, iFrg) = 2 ! Quaternion
          xzmpar(ii, iFrg) = 0.5
        ENDDO
      ELSE
! Molecule with rotation restricted to a single axis
        izmpar(iFrg) = 5
        czmpar(4, iFrg) = 'Q0(frag )'
        czmpar(5, iFrg) = 'Q1(frag )'
        DO ii = 4, 5
          kzmpar(ii, iFrg) = 6 ! Single axis
          xzmpar(ii, iFrg) = SQRT(0.5)
        ENDDO
      ENDIF
      DO ii = 1, izmpar(iFrg)
        WRITE (czmpar(ii, iFrg)(8:8),'(I1)') iFrg
      ENDDO
      DO i = 1, natoms(iFrg)
! IOPTB = 1 OPTIMISE BOND
        IF (ioptb(i, iFrg).EQ.1) THEN
          izmpar(iFrg) = izmpar(iFrg) + 1
          izm = izmpar(iFrg)
          kzmpar(izm, iFrg) = 5
          czmpar(izm, iFrg) = '('//OriginalLabel(i, iFrg)(1:LEN_TRIM(OriginalLabel(i, iFrg)))//':'// &
                                  OriginalLabel(iz1(i, iFrg), iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i, iFrg), iFrg)))// &
                             ') bond'
          xzmpar(izm, iFrg) = blen(i, iFrg)
        ENDIF
! IOPTA = 1 OPTIMISE ANGLE
        IF (iopta(i, iFrg).EQ.1) THEN
          izmpar(iFrg) = izmpar(iFrg) + 1
          izm = izmpar(iFrg)
          kzmpar(izm, iFrg) = 4
          czmpar(izm, iFrg) = '('//OriginalLabel(i, iFrg)(1:LEN_TRIM(OriginalLabel(i, iFrg)))//':'// &
                                  OriginalLabel(iz1(i, iFrg), iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i, iFrg), iFrg)))//':'// &
                                  OriginalLabel(iz2(i, iFrg), iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i, iFrg), iFrg)))// &
                             ') angle'
          xzmpar(izm, iFrg) = alph(i, iFrg)
        ENDIF
! IOPTT = 1 OPTIMISE TORSION
        IF (ioptt(i, iFrg).EQ.1) THEN
! Boundary check on number of degrees of freedom
          IF (izmpar(iFrg) .EQ. MaxDOF) THEN
            ioptt(i, iFrg) = 0
          ELSE
            izmpar(iFrg) = izmpar(iFrg) + 1
            izm = izmpar(iFrg)
            kzmpar(izm, iFrg) = 3
            czmpar(izm, iFrg) = '('//OriginalLabel(i, iFrg)(1:LEN_TRIM(OriginalLabel(i, iFrg)))//':'// &
                                    OriginalLabel(iz1(i, iFrg), iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i, iFrg), iFrg)))//':'// &
                                    OriginalLabel(iz2(i, iFrg), iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i, iFrg), iFrg)))//':'// &
                                    OriginalLabel(iz3(i, iFrg), iFrg)(1:LEN_TRIM(OriginalLabel(iz3(i, iFrg), iFrg)))// &
                               ') torsion'
            xzmpar(izm, iFrg) = bet(i, iFrg)
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE zmDoAdmin
!
!*****************************************************************************
!
      SUBROUTINE zmGenerateBonds(iFrg)

      USE SAMVAR
      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER I, BondNr

      natcry = NATOMS(iFrg)
      CALL makexyz(natcry,BLEN(1, iFrg),ALPH(1, iFrg),BET(1, iFrg),IZ1(1, iFrg),IZ2(1, iFrg),IZ3(1, iFrg),axyzo)
      DO I = 1, natcry
        aelem(I) = zmElementCSD(I, iFrg)
      ENDDO
! Calculate bonds and assign bond types.
      CALL SAMABO
! OUTPUT : nbocry             = number of bonds
!          bond(1:MAXBND,1:2) = the atoms connected by the bond
!          btype(1:MAXBND)    = the bond type
!                               1 = single
!                               2 = double
!                               3 = triple
!                               4 = quadruple
!                               5 = aromatic
!                               6 = polymeric single
!                               7 = delocalised
!                               9 = pi-bond
      NumberOfBonds(iFrg) = nbocry
      DO BondNr = 1, nbocry
        BondType(BondNr, iFrg) = btype(BondNr)
        Bonds(1,BondNr, iFrg)  = bond(BondNr,1)
        Bonds(2,BondNr, iFrg)  = bond(BondNr,2)
      ENDDO

      END SUBROUTINE zmGenerateBonds
!
!*****************************************************************************
!
