C>> JCC Re-implemented as a function
C>> 
      integer function read_one_zm(ifrg)
c      real*8 x(maxatm),y(maxatm),z(maxatm)
      character*120 line
c
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),
     &ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),
     &iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),
     &bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),
     &al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),
     &occ(maxatm,maxfrg)
c
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      common /zmcomg/ icomflg(maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),
     &czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)

C>> JCC the original atom ids to list in the labels and the back mapping
	common /zmjcmp/ izmoid(maxatm,maxfrg), izmbid(maxatm,maxfrg)
      character*50 cstring,cshort
C>> JCC Declaration
	Integer ErrorStatus
c
c       double precision dcel(6)
c
c      ntatm=0
c      do ifrg=1,nfrag

C>> JCC Initialise return value to successful (zero)
C>> 
C>> If the return value is non-zero, then an error occurred. The return status corresponds 
C>> to the appropriate FOR$IOS flag
	Read_One_Zm = 0
!       write(76,960) ifrg,frag_file(ifrg)(:lfrag(ifrg)) 
! 960   format('  Fragment file number ',i2,' is ',a)
c..
C>> JCC 
C>> Added in error trap for missing file/ file opening problems
       open(unit=19,
     &      file=frag_file(ifrg)(:lfrag(ifrg)),
     &	  status='old', 
     &      err = 999,
     &      iostat = ErrorStatus)
C>> JCC
C>> Added in a read error trap, in case user selects incorrect file
       read(19,1900, err=999, iostat = ErrorStatus) nlin,line
 1900  format(q,a)
       read(19,*,err=999, iostat = ErrorStatus) a(ifrg),b(ifrg),c(ifrg),
     &    al(ifrg),be(ifrg),ga(ifrg)
       read(19,*,err=999, iostat = Read_One_Zm) natof,item
       natoms(ifrg)=natof
       icomflg(ifrg)=item
!       ntatm=ntatm+natof
       izmpar(ifrg)=7
       czmpar(1,ifrg)=' x(frag )'
       czmpar(2,ifrg)=' y(frag )'
       czmpar(3,ifrg)=' z(frag )'
       do ii=1,3
         kzmpar(ii,ifrg)=1
         xzmpar(ii,ifrg)=0.5
       end do
       czmpar(4,ifrg)='Q1(frag )'
       czmpar(5,ifrg)='Q2(frag )'
       czmpar(6,ifrg)='Q3(frag )'
       czmpar(7,ifrg)='Q4(frag )'
       do ii=4,7
         kzmpar(ii,ifrg)=2
         xzmpar(ii,ifrg)=0.0
       end do
       do i=1,7
         write(czmpar(i,ifrg)(8:8),880) ifrg
 880     format(i1)
       end do
       ikk=7
       do i=1,natof
        read(19, 1900, err = 999, iostat = ErrorStatus) nlin,line

C>> JCC Added in traps on internal read
        read(line(3:5),'(a3)', err = 999, iostat = ErrorStatus) 
     &  asym(i,ifrg)

C>> JCC Try to read with the original ID if possible, else just map one-to-one
        read(line(7:nlin),*, iostat = ErrorStatus) 
     &   blen(i,ifrg),ioptb(i,ifrg),
     &   alph(i,ifrg),iopta(i,ifrg),bet(i,ifrg),
     &   ioptt(i,ifrg),iz1(i,ifrg),iz2(i,ifrg),
     &   iz3(i,ifrg),tiso(i,ifrg),occ(i,ifrg),izmoid(i,ifrg)
	  IF (ErrorStatus .NE. 0) THEN
         read(line(7:nlin),*, err = 999, iostat = ErrorStatus) 
     &    blen(i,ifrg),ioptb(i,ifrg),
     &    alph(i,ifrg),iopta(i,ifrg),bet(i,ifrg),
     &    ioptt(i,ifrg),iz1(i,ifrg),iz2(i,ifrg),
     &    iz3(i,ifrg),tiso(i,ifrg),occ(i,ifrg)
         	izmoid(i,ifrg) = i
	  END IF
	  izmbid(izmoid(i,ifrg),ifrg) = i ! the backward mapping from atoms in the zmatrix
	end do
C>> Broken into 2 loops now so that we can get the labels to relate to the original molecule
C>> IDs

	do i = 1,natof
 580    format(i4)
        if (ioptb(i,ifrg).eq.1) then
          izmpar(ifrg)=izmpar(ifrg)+1
          izm=izmpar(ifrg)
          kzmpar(izm,ifrg)=5
          cstring=' '
          cstring(1:1)='('
          cstring(2:4)=asym(i,ifrg)
C>> JCC Changed from
C          write(cstring(5:8),580) ifrg	
          write(cstring(5:8),580) izmoid(i,ifrg)
          cstring(9:9)=':'
          cstring(10:12)=asym(iz1(i,ifrg),ifrg)
C>> Was
C          write(cstring(14:17),580) iz1(i,ifrg)
		write(cstring(14:17),580) izmoid(iz1(i,ifrg),ifrg)
          cstring(20:25)=') bond'
          cshort=' '
          kk=0
          do jj=1,25
C>> JCC Keep space before bond
            if (cstring(jj:jj).ne.' ' .OR. jj .EQ. 21) then
              kk=kk+1
              cshort(kk:kk)=cstring(jj:jj)
            end if
          end do
          czmpar(izm,ifrg)=cshort
          xzmpar(izm,ifrg)=blen(i,ifrg)
        end if
        if (iopta(i,ifrg).eq.1) then
          izmpar(ifrg)=izmpar(ifrg)+1
          izm=izmpar(ifrg)
          kzmpar(izm,ifrg)=4
          cstring=' '
          cstring(1:1)='('
          cstring(2:4)=asym(i,ifrg)
C>> JCC Changed from
C          write(cstring(5:8),580) ifrg	
C>> Was
C          write(cstring(5:8),580) i
          write(cstring(5:8),580) izmoid(i,ifrg)
          cstring(9:9)=':'
          cstring(10:12)=asym(iz1(i,ifrg),ifrg)
C>> Was
C          write(cstring(14:17),580) iz1(i,ifrg)
          write(cstring(14:17),580) izmoid(iz1(i,ifrg),ifrg)
          cstring(19:19)=':'
          cstring(20:22)=asym(iz2(i,ifrg),ifrg)
C>> Was    write(cstring(24:27),580) iz2(i,ifrg)
          write(cstring(24:27),580) izmoid(iz2(i,ifrg),ifrg)
          cstring(30:36)=') angle'
          cshort=' '
          kk=0
          do jj=1,36
C>> JCC Keep space before angle
            if (cstring(jj:jj).ne.' ' .OR. jj .EQ. 31) then
              kk=kk+1
              cshort(kk:kk)=cstring(jj:jj)
            end if
          end do
          czmpar(izm,ifrg)=cshort
          xzmpar(izm,ifrg)=alph(i,ifrg)
        end if
        if (ioptt(i,ifrg).eq.1) then
          izmpar(ifrg)=izmpar(ifrg)+1
          izm=izmpar(ifrg)
          kzmpar(izm,ifrg)=3
          cstring=' '
          cstring(1:1)='('
          cstring(2:4)=asym(i,ifrg)
C>> JCC Changed from
C          write(cstring(5:8),580) ifrg	
C>> Was    write(cstring(5:8),580) i
          write(cstring(5:8),580) izmoid(i,ifrg)
          cstring(9:9)=':'
          cstring(10:12)=asym(iz1(i,ifrg),ifrg)
C>> Was    write(cstring(14:17),580) iz1(i,ifrg)
          write(cstring(14:17),580) izmoid(iz1(i,ifrg),ifrg)
          cstring(19:19)=':'
          cstring(20:22)=asym(iz2(i,ifrg),ifrg)
C>> Was    write(cstring(24:27),580) iz2(i,ifrg)
          write(cstring(24:27),580) izmoid(iz2(i,ifrg),ifrg)
          cstring(29:29)=':'
          cstring(30:32)=asym(iz3(i,ifrg),ifrg)
C>> Was    write(cstring(34:37),580) iz3(i,ifrg)
          write(cstring(34:37),580) izmoid(iz3(i,ifrg),ifrg)
          cstring(40:48)=') torsion'
          cshort=' '
          kk=0
          do jj=1,48
C>> JCC Keep space before torsion
            if (cstring(jj:jj).ne.' '. OR. jj .EQ. 41) then
              kk=kk+1
              cshort(kk:kk)=cstring(jj:jj)
            end if
          end do
          czmpar(izm,ifrg)=cshort
          xzmpar(izm,ifrg)=bet(i,ifrg)
        end if
!	  write(76,7600) sngl(blen(i,ifrg)),ioptb(i,ifrg),
!     &       sngl(alph(i,ifrg)),
!     &       iopta(i,ifrg),sngl(bet(i,ifrg)),ioptt(i,ifrg),
!     &  iz1(i,ifrg),iz2(i,ifrg),iz3(i,ifrg),
!     &  sngl(tiso(i,ifrg)),sngl(occ(i,ifrg))
! 7600   format(f8.4,i3,f8.3,i3,f8.3,i3,3i3,2f8.3)
       end do


 998   close(19)
       gotzmfile(ifrg)=.true.
       if (natof.eq.1) then
         izmpar(ifrg)=3
         czmpar(1,ifrg)=' x(frag )'
         czmpar(2,ifrg)=' y(frag )'
         czmpar(3,ifrg)=' z(frag )'
         do ii=1,3
           write(czmpar(ii,ifrg)(8:8),880) ifrg
           kzmpar(ii,ifrg)=1
           xzmpar(ii,ifrg)=0.5
         end do
       end if
c
c.. We should have the option of using (cellpar(i),i=1,6) -- done!
c	write(56,*) ' In read_one_zm ',(cellpar(ii),ii=1,6)
c      if (ifrg.eq.1) call frac2cart(f2cmat,a(1),b(1),c(1),al(1),be(1),ga(1))
c      if (ifrg.eq.1) then
c        do i=1,6
c          dcel(i)=dble(cellpar(i))
c        end do
c        call frac2cart(f2cmat,dcel(1),dcel(2),dcel(3),
c     &       dcel(4),dcel(5),dcel(6))
c      end if
c
      return
C>> JCC Added in return status for failed reading and failed opening
 999  Read_One_Zm = ErrorStatus
	return

      end


	subroutine Write_Solution_Information

	


	end subroutine Write_Solution_Information
