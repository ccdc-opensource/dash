      subroutine readzm()
c      real*8 x(maxatm),y(maxatm),z(maxatm)
      character*80 line
      logical file_present
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
c
      ntatm=0
      do ifrg=1,nfrag
!       write(76,960) ifrg,frag_file(ifrg)(:lfrag(ifrg)) 
! 960   format('  Fragment file number ',i2,' is ',a)
c..
       open(unit=19,file=frag_file(ifrg)(:lfrag(ifrg)),status='old')
c       read(19,'(a80)') line
       read(19,1900) nlin,line
 1900  format(q,a)
       read(19,*,err=999) a(ifrg),b(ifrg),c(ifrg),
     &    al(ifrg),be(ifrg),ga(ifrg)
       read(19,*,err=999) natof,item
       natoms(ifrg)=natof
       icomflg(ifrg)=item
       ntatm=ntatm+natof
       do i=1,natof
c        read(19,'(a80)') line
        read(19,1900) nlin,line
        read(line(3:5),'(a3)') asym(i,ifrg)
        read(line(7:nlin),*) blen(i,ifrg),ioptb(i,ifrg),alph(i,ifrg),
     &                       iopta(i,ifrg),bet(i,ifrg),ioptt(i,ifrg),
     &  iz1(i,ifrg),iz2(i,ifrg),iz3(i,ifrg),tiso(i,ifrg),occ(i,ifrg)
!	  write(76,*) blen(i,ifrg),ioptb(i,ifrg),alph(i,ifrg),
!     &                       iopta(i,ifrg),bet(i,ifrg),ioptt(i,ifrg),
!     &  iz1(i,ifrg),iz2(i,ifrg),iz3(i,ifrg),tiso(i,ifrg),occ(i,ifrg)
       end do
 998   close(19)
      end do
c
c.. We should have the option of using (cellpars(i),i=1,6)
      call frac2cart(f2cmat,a(1),b(1),c(1),al(1),be(1),ga(1))
c
 999  return
      end