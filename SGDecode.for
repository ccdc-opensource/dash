	subroutine DecodeSGSymbol(SGsymb)
!
!... This program decodes the explicit space group symbols in Vol.B
	character*24	SGsymb
!
	real rotmat(3,3,10),tran(3,10),alat(3,10)
	integer idol(5)
	integer matsym(3,3,12)
	data matsym /1,0,0, 0,1,0, 0,0,1,   1,0,0, 0,-1,0, 0,0,-1,
     &            -1,0,0, 0,1,0, 0,0,-1, -1,0,0, 0,-1,0, 0,0,1,
     &             0,1,0, 1,0,0, 0,0,-1,  0,-1,0, -1,0,0, 0,0,-1,
     &             1,-1,0,0,-1,0,0,0,-1,  1,0,0, 1,-1,0, 0,0,-1,
     &             0,0,1, 1,0,0, 0,1,0,  0,-1,0, 1,-1,0, 0,0,1,
     &             0,-1,0,1,0,0, 0,0,1,  1,-1,0, 1,0,0, 0,0,1/
	integer latvec(3,7)
C	data latvec/ 6,0,0, 0,6,0, 0,0,6, 6,6,6, 6,6,0, 6,0,6, 8,4,4/
C	the above latvec is all wrong, but the one below only works
C	for A,B,C and I centering
	data latvec/0,6,6, 6,0,6, 6,6,0 ,6,6,6, 8,4,4, 4,8,8, 0,0,0 /
      parameter (msymmin=10)
      character*20 symline
	common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
	character*50 stout
!
	nele=1
	ns=24
	do i=1,ns
	  if (SGsymb(i:i).eq.'$') then
	    idol(nele)=i
	    nele=nele+1
	  end if	
	end do
	idol(nele)=Ns+1
!
!... (2) Crystal system


C	SELECT CASE (SGsymb(2:2))
C	  CASE('A')
C          write(76,*) ' Triclinic Bravais lattice'
C	  CASE('M')
C          write(76,*) ' Monoclinic Bravais lattice'
C	  CASE('O')
C          write(76,*) ' Orthorhombic Bravais lattice'
C	  CASE('T')
C          write(76,*) ' Tetragonal Bravais lattice'
C	  CASE('R')
C          write(76,*) ' Rhombohedral Bravais lattice'
C	  CASE('H')
C          write(76,*) ' Hexagonal/Trigonal Bravais lattice'
C	  CASE('C')
C          write(76,*) ' Cubic Bravais lattice'
C	END SELECT
!... (3) Centre of symmetry at origin or not
C	SELECT CASE (SGsymb(3:3))
C	  CASE('C')
C          write(76,*) ' Centrosymmetric'
C	  CASE('N')
C          write(76,*) ' Non-centrosymmetric'
C	END SELECT
!
!... Let's decode the first part to do with lattice translations etc.
!... It should always be 3 letters long
!... (1) Lattice type
	isym=1
	SELECT CASE (SGsymb(1:1))
	  CASE('P')
	    isym=0
	  CASE('A')
          ilatar=1
	  CASE('B')
          ilatar=2
	  CASE('C')
          ilatar=3
	  CASE('I')
          ilatar=4
	  CASE('F')
	    isym=3
	  CASE('R')
          isym=2
	END SELECT
	If (Isym.eq.1) then
        do i=1,3
	    alat(i,isym)=float(latvec(i,ilatar))/12.
	  end do
!	  write(76,*) ' '
!	  write(76,*) ' Lattice operation ',Isym
!        do i=1,3
!	    write(76,1500) (matsym(j,i,1),j=1,3),alat(i,isym)
!	  end do
	Else If (Isym.eq.3) then
	  do j=1,3
	  do i=1,3
	    alat(i,j)=float(latvec(i,j))/12.
	  end do
	  end do
	Else If (Isym.eq.2) then
	  do j=1,2
	  do i=1,3
	    alat(i,j)=float(latvec(i,j+4))/12.
	  end do
	  end do
!	  write(76,*) ' '
!	  write(76,*) ' Lattice operation ',Isym1
!        do i=1,3
!	    write(76,1500) (matsym(j,i,1),j=1,3),alat(i,isym1)
!	  end do
!	  ilatar=ilatar+1
!	  do i=1,3
!	    alat(i,isym)=float(latvec(i,ilatar))/12.
!	  end do
!	  write(76,*) ' '
!	  write(76,*) ' Lattice operation ',Isym
!        do i=1,3
!	    write(76,1500) (matsym(j,i,1),j=1,3),alat(i,isym)
!	  end do
	End if
!
	nlat=isym
	if (nlat.gt.0) then
	  do ilat=1,nlat
	    do i=1,3
	      do j=1,3
	        rotmat(i,j,ilat)=matsym(i,j,1)
	      end do
	      tran(i,ilat)=alat(i,ilat)
	    end do
	  end do
	end if
!
	do jj=2,nele
	 id1=idol(jj-1)+1
	 id2=idol(jj)-1
! 1+id2-id1=6 always
!
	 isym=isym+1 
       SELECT CASE (SGsymb(id1+1:id1+2))
	   CASE('1A')
          irotmat=1
	   CASE('2A')
          irotmat=2
	   CASE('2B')
          irotmat=3
	   CASE('2C')
          irotmat=4
	   CASE('2D')
          irotmat=5
	   CASE('2E')
          irotmat=6
	   CASE('2F')
          irotmat=7
	   CASE('2G')
          irotmat=8
	   CASE('3Q')
          irotmat=9
	   CASE('3C')
          irotmat=10
	   CASE('4C')
          irotmat=11
	   CASE('6C')
          irotmat=12
	 END SELECT
	 DO j=1,3
	   DO i=1,3
	     rotmat(i,j,isym)=float(matsym(j,i,irotmat))
	   END DO
	 END DO
       SELECT CASE (SGsymb(id1:id1))
	   CASE('P')
! Proper rotation - leave the rotation matrix unchanged
	   CASE('I')
! Improper rotation - negate the matrix
	     DO j=1,3
	       DO i=1,3
	         rotmat(i,j,isym)=-rotmat(i,j,isym)
	       END DO
	     END DO
	 END SELECT
	 DO I=1,3
	   jd=id1+2+i
	   READ(SGsymb(jd:jd),1400) Num12th
 1400	   format(i1)
	   Tran(i,isym)=float(Num12th)/12.
	   if (Num12th.eq.5) Tran(i,isym)=2.*Tran(i,isym)
	 END DO
!	 write(76,*) ' '
!	 write(76,*) ' Operator number   ',Isym
!       do i=1,3
!	   write(76,1500) (nint(rotmat(i,j,isym)),j=1,3),tran(i,isym)
! 1500	   format(5x,3i4,f9.5)
!	 end do
	END DO
!
!.. Now make the Jones faithful representation
      nsymmin=isym
!      write(76,*) ' Number of symmetry generators ',nsymmin
      do js=1,nsymmin
	  do i=1,3
	    do j=1,3
	      symmin(i,j,js)=rotmat(i,j,js)
	    end do
	    symmin(i,4,js)=tran(i,js)
	  end do
	  do j=1,3
	    symmin(4,j,js)=0.
	  end do
	  symmin(4,4,js)=1.
!	do i=1,4
!	 write(76,3900) (nint(symmin(i,j,js)),j=1,3),symmin(i,4,js)
! 3900	 format(10x,3i5,f10.5)
!	end do
	  call M2S_SYMCON(symmin(1,1,js),stout,lstout)
!	  write (76,4000) js,stout(:20)
 4000	  format(i5,5x,a)
       symline(js)=stout(:20)
	end do
!
 999	end
!
!
!
!
!
!
	subroutine M2S_SYMCON(symtem,stout,lstout)
! Makes the Jones faithful representation from the 4 by 4 matrix
!
	character*50 stem,stout,stoutt
	character*3 strtran(12)
	real symtem(4,4)
	integer lentran(12)
	data lentran /0,0,3,3,3,0,3,0,3,3,3,0/
	data strtran /'   ','   ','1/6','1/4','1/3','   ',
     &'1/2','   ','2/3','3/4','5/6','   '/
	integer kk
!
	lstout=0
	do i=1,3
!
	  ipt=0
!
	  item=1+nint(12.*symtem(i,4))
	  jpt=ipt+lentran(item)
	  stem(ipt+1:jpt)=strtran(item)
	  ipt=jpt
        kk = nint(symtem(i,1))
	  if ( kk .eq. -1) then
	    stem(ipt+1:ipt+2)='-x'
	    ipt=ipt+2
	  else if ( kk .eq. 1) then
	    if (ipt.eq.0) then
	      stem(ipt+1:ipt+1)='x'
	      ipt=ipt+1
	    else
	      stem(ipt+1:ipt+2)='+x'
	      ipt=ipt+2
	    end if
	  end if
!

	  kk = nint(symtem(i,2))
	  if ( kk .eq. -1) then
	    stem(ipt+1:ipt+2)='-y'
	    ipt=ipt+2
	  else if ( kk .eq. 1) then
	    if (ipt.eq.0) then
	      stem(ipt+1:ipt+1)='y'
	      ipt=ipt+1
	    else
	      stem(ipt+1:ipt+2)='+y'
	      ipt=ipt+2
	    end if
	  end if
!
	  kk = nint(symtem(i,3))
	  if ( kk .eq.-1) then
	    stem(ipt+1:ipt+2)='-z'
	    ipt=ipt+2
	  else if (  kk .eq. 1.) then
	    if (ipt.eq.0) then
	      stem(ipt+1:ipt+1)='z'
	      ipt=ipt+1
	    else
	      stem(ipt+1:ipt+2)='+z'
	      ipt=ipt+2
	    end if
	  end if
	  stoutt(lstout+1:lstout+ipt)=stem(1:ipt)
	  lstout=lstout+ipt
	  lstout=lstout+1
	  stoutt(lstout:lstout)=','
!
	end do
	lstout=lstout-1
	stout=' '
	stout(21-lstout:20)=stoutt(1:lstout)
!
	end