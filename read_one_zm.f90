!*==READ_ONE_ZM.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
! JCC Re-implemented as a function
!
      INTEGER FUNCTION read_one_zm(ifrg)

      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: ifrg
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
      CHARACTER*255 line
      CHARACTER*255 tSubString
      INTEGER ErrorStatus
      REAL ta, tb, tc, talpha, tbeta, tgamma
      INTEGER I, J, NumCol
      INTEGER GetNumOfColumns ! Function
      CHARACTER*1 ChrLowerCase, ChrUpperCase ! Functions
      INTEGER item, II, izm, IKK, nlin, natof
      INTEGER AsymLen, IDlen
      CHARACTER*3 tIDstr
      REAL*8 CART(1:3,1:MAXATM)
      CHARACTER*2  AtmElement(1:MAXATM_2)
      INTEGER BondNr

! JCC Initialise return value to successful (zero)
! If the return value is non-zero, then an error occurred. The return status corresponds
! to the appropriate FOR$IOS flag
      Read_One_Zm = 0
! JCC Added in error trap for missing file/ file opening problems
      OPEN (UNIT=19,FILE=frag_file(ifrg),STATUS='old',ERR=999,IOSTAT=ErrorStatus)
! JCC Added in a read error trap, in case user selects incorrect file
! First line is a title
      READ (19,'(A)',ERR=999,IOSTAT=ErrorStatus) line
! Second line contains unit cell parameters
!1.0 1.0 1.0 90.0 90.0 90.0
! These are never used, so just read into dummy variables.
      READ (19,*,ERR=999,IOSTAT=ErrorStatus) ta, tb, tc, talpha, tbeta, tgamma
! Third line contains number of atoms and an integer IAT, defining the centre for the rotations.
! IAT = 0        : use centre of mass
! IAT = non-zero : use atom nr. IAT   (necessary if atom on special position).
!  59   0
      READ (19,*,ERR=999,IOSTAT=ErrorStatus) natof, item
      natoms(ifrg) = natof
      icomflg(ifrg) = item
      izmpar(ifrg) = 7 ! always reserve 4 parameters for rotations, whether quaternion or single axis
      czmpar(1,ifrg) = ' x(frag )'
      czmpar(2,ifrg) = ' y(frag )'
      czmpar(3,ifrg) = ' z(frag )'
! izmpar = number of degrees of freedom ('parameters')
! kzmpar = type of parameter (1 = translation)
! xzmpar = initial value of parameter
! czmpar = Character string associated with this parameter value
      DO ii = 1, 3
        kzmpar(ii,ifrg) = 1
        xzmpar(ii,ifrg) = 0.5
      ENDDO
      czmpar(4,ifrg) = 'Q1(frag )'
      czmpar(5,ifrg) = 'Q2(frag )'
      czmpar(6,ifrg) = 'Q3(frag )'
      czmpar(7,ifrg) = 'Q4(frag )'
      DO ii = 4, 7
        kzmpar(ii,ifrg) = 2 ! Quaternion
        xzmpar(ii,ifrg) = 0.5
      ENDDO
      DO i = 1, 7
        WRITE (czmpar(i,ifrg)(8:8),880) ifrg
      ENDDO
      ikk = 7 ! always reserve 4 parameters for rotations, whether quaternion or single axis
      DO i = 1, natof
! Remaining lines contain the Z-matrix
!  C      1.5152617  0  113.2370014  0 -179.8250018  0   54   51   48  3.0  1.0   58 C6 C7 C8 C9
        READ (19,1900,ERR=999,IOSTAT=ErrorStatus) nlin, line
! JCC Added in traps on internal read
        READ (line(3:5),'(A3)',ERR=999,IOSTAT=ErrorStatus) asym(i,ifrg)
        CALL StrClean(Asym,AsymLen)
        Asym(i,ifrg)(1:1) = ChrUpperCase(Asym(i,ifrg)(1:1))
        Asym(i,ifrg)(2:2) = ChrLowerCase(Asym(i,ifrg)(2:2))
        Asym(i,ifrg)(3:3) = ChrLowerCase(Asym(i,ifrg)(3:3))
! First item--the element--has been read. Rest more tricky.
! First, convert tabs to spaces and remove redundant spaces
        CALL StrClean(line,nlin)
! First column--the element--has been read: remove it
        CALL GetSubString(Line,' ',tSubString)
! Then count the number of columns
        NumCol = GetNumOfColumns(Line)
        IF (NumCol .LT. 11) THEN
          ErrorStatus = 99
          GOTO 999
        ENDIF
        READ (line(1:nlin),*,ERR=999,IOSTAT=ErrorStatus) blen(i,ifrg),&
     &        ioptb(i,ifrg), alph(i,ifrg), iopta(i,ifrg), bet(i,ifrg),&
     &        ioptt(i,ifrg), iz1(i,ifrg), iz2(i,ifrg), iz3(i,ifrg),   &
     &        tiso(i,ifrg), occ(i,ifrg)
! Remove what we have just read from the string
        DO J = 1, 11
          CALL GetSubString(Line,' ',tSubString)
        ENDDO
        nlin = LEN_TRIM(Line)
! How many columns do we have left?
        NumCol = GetNumOfColumns(Line)
        IF (NumCol .EQ. 0) THEN
          izmoid(i,ifrg) = i
        ELSE
          READ (line(1:nlin),*,IOSTAT=ErrorStatus) izmoid(i,ifrg)
        ENDIF
        IF (NumCol .GE. 2) THEN
          CALL GetSubString(Line,' ',tSubString) ! That should be the original atom number
          CALL GetSubString(Line,' ',tSubString) ! And that should be what we really want:
                                                 ! the original atom label
        ELSE
! Emulate original atom labels by adding original atom number to element, e.g. 'C4'
          WRITE(tIDstr,'(I3)') izmoid(i,ifrg)
          CALL StrClean(tIDstr,IDlen)
          tSubString = Asym(i,ifrg)(1:AsymLen)//tIDstr
        ENDIF
        OriginalLabel(i,ifrg) = tSubString(1:5)
        izmbid(izmoid(i,ifrg),ifrg) = i   ! the backward mapping from atoms in the zmatrix
      ENDDO
! Broken into 2 loops now so that we can get the labels to relate to the original molecule IDs
      DO i = 1, natof
! IOPTB = 1 OPTIMISE BOND
        IF (ioptb(i,ifrg).EQ.1) THEN
          izmpar(ifrg) = izmpar(ifrg) + 1
          izm = izmpar(ifrg)
          kzmpar(izm,ifrg) = 5
          czmpar(izm,ifrg) = '('//OriginalLabel(i,ifrg)(1:LEN_TRIM(OriginalLabel(i,ifrg)))//':'// &
                                  OriginalLabel(iz1(i,ifrg),ifrg)(1:LEN_TRIM(OriginalLabel(iz1(i,ifrg),ifrg)))// &
                             ') bond'
          xzmpar(izm,ifrg) = blen(i,ifrg)
        ENDIF
! IOPTA = 1 OPTIMISE ANGLE
        IF (iopta(i,ifrg).EQ.1) THEN
          izmpar(ifrg) = izmpar(ifrg) + 1
          izm = izmpar(ifrg)
          kzmpar(izm,ifrg) = 4
          czmpar(izm,ifrg) = '('//OriginalLabel(i,ifrg)(1:LEN_TRIM(OriginalLabel(i,ifrg)))//':'// &
                                  OriginalLabel(iz1(i,ifrg),ifrg)(1:LEN_TRIM(OriginalLabel(iz1(i,ifrg),ifrg)))//':'// &
                                  OriginalLabel(iz2(i,ifrg),ifrg)(1:LEN_TRIM(OriginalLabel(iz2(i,ifrg),ifrg)))// &
                             ') angle'
          xzmpar(izm,ifrg) = alph(i,ifrg)
        ENDIF
! IOPTT = 1 OPTIMISE TORSION
        IF (ioptt(i,ifrg).EQ.1) THEN
          izmpar(ifrg) = izmpar(ifrg) + 1
          izm = izmpar(ifrg)
          kzmpar(izm,ifrg) = 3
          czmpar(izm,ifrg) = '('//OriginalLabel(i,ifrg)(1:LEN_TRIM(OriginalLabel(i,ifrg)))//':'// &
                                  OriginalLabel(iz1(i,ifrg),ifrg)(1:LEN_TRIM(OriginalLabel(iz1(i,ifrg),ifrg)))//':'// &
                                  OriginalLabel(iz2(i,ifrg),ifrg)(1:LEN_TRIM(OriginalLabel(iz2(i,ifrg),ifrg)))//':'// &
                                  OriginalLabel(iz3(i,ifrg),ifrg)(1:LEN_TRIM(OriginalLabel(iz3(i,ifrg),ifrg)))// &
                             ') torsion'
          xzmpar(izm,ifrg) = bet(i,ifrg)
        ENDIF
      ENDDO
      CLOSE (19)
      gotzmfile(ifrg) = .TRUE.
      IF (natof.EQ.1) THEN
        izmpar(ifrg) = 3
        czmpar(1,ifrg) = ' x(frag )'
        czmpar(2,ifrg) = ' y(frag )'
        czmpar(3,ifrg) = ' z(frag )'
        DO ii = 1, 3
          WRITE (czmpar(ii,ifrg)(8:8),880) ifrg
          kzmpar(ii,ifrg) = 1
          xzmpar(ii,ifrg) = 0.5
        ENDDO
      ENDIF
! Now precalculate the bonds
      natcry = NATOMS(ifrg)
      CALL MAKEXYZ_2(natcry,BLEN(1,ifrg),ALPH(1,ifrg),BET(1,ifrg),      &
                     IZ1(1,ifrg),IZ2(1,ifrg),IZ3(1,ifrg),CART)
! Conversion of asym to aelem : very dirty, but works
      DO I = 1, natcry
        axyzo(I,1) = SNGL(CART(1,I))
        axyzo(I,2) = SNGL(CART(2,I))
        axyzo(I,3) = SNGL(CART(3,I))
        AtmElement(I)(1:2) = asym(I,ifrg)(1:2)
      ENDDO
      CALL AssignCSDElement(AtmElement)
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
      NumberOfBonds(ifrg) = nbocry
      DO BondNr = 1, nbocry
        BondType(BondNr,ifrg) = btype(BondNr)
        Bonds(1,BondNr,ifrg)  = bond(BondNr,1)
        Bonds(2,BondNr,ifrg)  = bond(BondNr,2)
      ENDDO
      RETURN
! JCC Added in return status for failed reading and failed opening
  999 Read_One_Zm = ErrorStatus
      RETURN
 1900 FORMAT (Q,A)
  880 FORMAT (I1)
  580 FORMAT (I4)

      END FUNCTION READ_ONE_ZM
!
!*****************************************************************************
!
