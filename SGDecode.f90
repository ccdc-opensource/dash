!
!*****************************************************************************
!
      SUBROUTINE DecodeSGSymbol(SGsymb)
!
! This program decodes the explicit space group symbols in Vol.B
      CHARACTER*24 SGsymb

      REAL rotmat(3,3,10), tran(3,10), alat(3,10)
      INTEGER idol(5)
      INTEGER matsym(3,3,12)
      DATA matsym/1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, -1, 0, 0, 0,   &
     &     -1, -1, 0, 0, 0, 1, 0, 0, 0, -1, -1, 0, 0, 0, -1, 0, 0, 0, 1,&
     &     0, 1, 0, 1, 0, 0, 0, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, -1, 1, &
     &     -1, 0, 0, -1, 0, 0, 0, -1, 1, 0, 0, 1, -1, 0, 0, 0, -1, 0, 0,&
     &     1, 1, 0, 0, 0, 1, 0, 0, -1, 0, 1, -1, 0, 0, 0, 1, 0, -1, 0,  &
     &     1, 0, 0, 0, 0, 1, 1, -1, 0, 1, 0, 0, 0, 0, 1/
      INTEGER latvec(3,7)
!	data latvec/ 6,0,0, 0,6,0, 0,0,6, 6,6,6, 6,6,0, 6,0,6, 8,4,4/
!	the above latvec is all wrong, but the one below only works
!	for A,B,C and I centering
      DATA latvec/0,6,6, 6,0,6, 6,6,0, 6,6,6, 8,4,4, 4,8,8, 0,0,0/
      PARAMETER (msymmin=10)
      CHARACTER*20 symline
      COMMON /symgencmn/ nsymmin, symmin(4,4,msymmin), symline(msymmin)
      CHARACTER*50 stout

      nele = 1
      ns = 24
      DO i = 1, ns
        IF (SGsymb(i:i).EQ.'$') THEN
          idol(nele) = i
          nele = nele + 1
        ENDIF
      ENDDO
      idol(nele) = Ns + 1
!... Let's decode the first part to do with lattice translations etc.
!... It should always be 3 letters long
!... (1) Lattice type
      isym = 1
      SELECT CASE (SGsymb(1:1))
      CASE ('P')
        isym = 0
      CASE ('A')
        ilatar = 1
      CASE ('B')
        ilatar = 2
      CASE ('C')
        ilatar = 3
      CASE ('I')
        ilatar = 4
      CASE ('F')
        isym = 3
      CASE ('R')
        isym = 2
      END SELECT
      IF (Isym.EQ.1) THEN
        DO i = 1, 3
          alat(i,isym) = float(latvec(i,ilatar))/12.
        ENDDO
      ELSEIF (Isym.EQ.3) THEN
        DO j = 1, 3
          DO i = 1, 3
            alat(i,j) = float(latvec(i,j))/12.
          ENDDO
        ENDDO
      ELSEIF (Isym.EQ.2) THEN
        DO j = 1, 2
          DO i = 1, 3
            alat(i,j) = float(latvec(i,j+4))/12.
          ENDDO
        ENDDO
      ENDIF
      nlat = isym
      IF (nlat.GT.0) THEN
        DO ilat = 1, nlat
          DO i = 1, 3
            DO j = 1, 3
              rotmat(i,j,ilat) = matsym(i,j,1)
            ENDDO
            tran(i,ilat) = alat(i,ilat)
          ENDDO
        ENDDO
      ENDIF
      DO jj = 2, nele
        id1 = idol(jj-1) + 1
        id2 = idol(jj) - 1
! 1+id2-id1=6 always
        isym = isym + 1
        SELECT CASE (SGsymb(id1+1:id1+2))
        CASE ('1A')
          irotmat = 1
        CASE ('2A')
          irotmat = 2
        CASE ('2B')
          irotmat = 3
        CASE ('2C')
          irotmat = 4
        CASE ('2D')
          irotmat = 5
        CASE ('2E')
          irotmat = 6
        CASE ('2F')
          irotmat = 7
        CASE ('2G')
          irotmat = 8
        CASE ('3Q')
          irotmat = 9
        CASE ('3C')
          irotmat = 10
        CASE ('4C')
          irotmat = 11
        CASE ('6C')
          irotmat = 12
        END SELECT
        DO j = 1, 3
          DO i = 1, 3
            rotmat(i,j,isym) = float(matsym(j,i,irotmat))
          ENDDO
        ENDDO
        IF (SGsymb(id1:id1) .EQ. 'I') THEN
! Improper rotation - negate the matrix
          DO j = 1, 3
            DO i = 1, 3
              rotmat(i,j,isym) = -rotmat(i,j,isym)
            ENDDO
          ENDDO
        ENDIF
        DO I = 1, 3
          jd = id1 + 2 + i
          READ (SGsymb(jd:jd),1400) Num12th
 1400     FORMAT (i1)
          Tran(i,isym) = float(Num12th)/12.
          IF (Num12th.EQ.5) Tran(i,isym) = 2.*Tran(i,isym)
        ENDDO
      ENDDO
!.. Now make the Jones faithful representation
      nsymmin = isym
      DO js = 1, nsymmin
        DO i = 1, 3
          DO j = 1, 3
            symmin(i,j,js) = rotmat(i,j,js)
          ENDDO
          symmin(i,4,js) = tran(i,js)
        ENDDO
        DO j = 1, 3
          symmin(4,j,js) = 0.
        ENDDO
        symmin(4,4,js) = 1.
        CALL M2S_SYMCON(symmin(1,1,js),stout,lstout)
 4000   FORMAT (i5,5x,a)
        symline(js) = stout(:20)
      ENDDO
      CALL PDB_SymmRecords

      END SUBROUTINE DECODESGSYMBOL
!
!*****************************************************************************
!
      SUBROUTINE M2S_SYMCON(symtem,stout,lstout)
! Makes the Jones faithful representation from the 4 by 4 matrix
!
      CHARACTER*50 stem, stout, stoutt
      CHARACTER*3 strtran(12)
      REAL symtem(4,4)
      INTEGER lentran(12)
      DATA lentran/0, 0, 3, 3, 3, 0, 3, 0, 3, 3, 3, 0/
      DATA strtran/'   ', '   ', '1/6', '1/4', '1/3', '   ', '1/2',     &
     &     '   ', '2/3', '3/4', '5/6', '   '/
      INTEGER kk

      lstout = 0
      DO i = 1, 3
        ipt = 0
        item = 1 + NINT(12.*symtem(i,4))
        jpt = ipt + lentran(item)
        stem(ipt+1:jpt) = strtran(item)
        ipt = jpt
        kk = NINT(symtem(i,1))
        IF (kk.EQ.-1) THEN
          stem(ipt+1:ipt+2) = '-x'
          ipt = ipt + 2
        ELSEIF (kk.EQ.1) THEN
          IF (ipt.EQ.0) THEN
            stem(ipt+1:ipt+1) = 'x'
            ipt = ipt + 1
          ELSE
            stem(ipt+1:ipt+2) = '+x'
            ipt = ipt + 2
          ENDIF
        ENDIF
        kk = NINT(symtem(i,2))
        IF (kk.EQ.-1) THEN
          stem(ipt+1:ipt+2) = '-y'
          ipt = ipt + 2
        ELSEIF (kk.EQ.1) THEN
          IF (ipt.EQ.0) THEN
            stem(ipt+1:ipt+1) = 'y'
            ipt = ipt + 1
          ELSE
            stem(ipt+1:ipt+2) = '+y'
            ipt = ipt + 2
          ENDIF
        ENDIF
        kk = NINT(symtem(i,3))
        IF (kk.EQ.-1) THEN
          stem(ipt+1:ipt+2) = '-z'
          ipt = ipt + 2
        ELSEIF (kk.EQ.1) THEN
          IF (ipt.EQ.0) THEN
            stem(ipt+1:ipt+1) = 'z'
            ipt = ipt + 1
          ELSE
            stem(ipt+1:ipt+2) = '+z'
            ipt = ipt + 2
          ENDIF
        ENDIF
        stoutt(lstout+1:lstout+ipt) = stem(1:ipt)
        lstout = lstout + ipt
        lstout = lstout + 1
        stoutt(lstout:lstout) = ','
      ENDDO
      lstout = lstout - 1
      stout = ' '
      stout(21-lstout:20) = stoutt(1:lstout)

      END SUBROUTINE M2S_SYMCON
!
!*****************************************************************************
!
