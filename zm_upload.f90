      subroutine zm_upload()
!
!
  USE WINTERACTER
  USE DRUID_HEADER
!
      character*80 line
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
     ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
     bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),&
      al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),&
      occ(maxatm,maxfrg)              
!
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      common /zmcomg/ icomflg(maxfrg)
!
            CALL WDialogSelect(IDD_zmatrix_files)
            CALL WDialogPutInteger(IDF_nfrag,NFRAG)
            CALL WGridRows(IDF_zmatrixfile_grid,nfrag)
            DO i=1,nFRAG
              CALL WGridPutCellString(IDF_zmatrixfile_grid,1,i,frag_file(i)(:lfrag(i)))
            END DO
!
!
      end