!
!*****************************************************************************
!
        SUBROUTINE SA_structure_output(t,fopt,cpb,parvals,ntotmov)
C
C       Called when a new minimum is found
C
      DOUBLE PRECISION t, fopt
      REAL cpb
      DOUBLE PRECISION parvals(*) ! The current torsion parameters (cant be called X here)
      INTEGER ntotmov

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'
      INCLUDE 'IZMCheck.inc'

      parameter (maxatm=100)
      parameter (maxfrg=20)
      character*3 asym
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomi/ ntatm,natoms(maxfrg),
     &  ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),
     &  iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)

      CHARACTER*36 czmpar
      COMMON /zmnpar/ izmtot,izmpar(maxfrg),
     &   czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)

      INTEGER ipcount
      INTEGER CheckedFragNo

      COMMON /frgcom/ nfrag,lfrag(maxfrg)
      DOUBLE PRECISION a,b,c,al,be,ga,chrg
      COMMON /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),
     &  al(maxfrg),be(maxfrg),ga(maxfrg),chrg(maxatm,maxfrg)
      DOUBLE PRECISION blen,alph,bet,f2cmat
      COMMON /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),
     &  bet(maxatm,maxfrg),f2cmat(3,3)
      DOUBLE PRECISION inv(3,3)
      COMMON /posopt/ XATOPT(3,150)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      CHARACTER*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &pro_file   
      COMMON /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,
     &log_file,pro_file
      INTEGER logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen
      COMMON /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,
     &log_flen,pro_flen

      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops,cpdbops

C>> JCC the original atom ids to list in the labels and the back mapping
      COMMON /zmjcmp/ izmoid(maxatm,maxfrg), izmbid(maxatm,maxfrg)
C>> JCC Use standard PDB orthogonalisation
      DOUBLE PRECISION f2cpdb, c2fpdb
      REAL qvals(4), qnrm
      COMMON /pdbcat/ f2cpdb(3,3), c2fpdb(3,3)
c
      ntem=NumberSGTable

C     ep added.  Following subroutine saves calculated and observed
C     diffraction patterns in .pro file
      CALL Sa_soln_store
c
c       Output a CSSR file to fort.64
C       Output a PDB  file to fort.65
C       Output a CCL  to fort.66
C
C       Write the file headers first
C
C       The CSSR file first
      OPEN(UNIT=64,file=cssr_file(1:cssr_flen),status='unknown')
      OPEN(UNIT=65,file=pdb_file(1:pdb_flen),status='unknown')
      OPEN(UNIT=66,file=ccl_file(1:ccl_flen),status='unknown')
c        write(64,1000) a(nfrag),b(nfrag),c(nfrag)
c        write(64,1010) al(nfrag),be(nfrag),ga(nfrag),
c     &                 SGNumStr(Ntem)(1:3)
      WRITE(64,1000) (CellPar(ii),ii=1,3)
      WRITE(64,1010) (CellPar(ii),ii=4,6),
     &                 SGNumStr(Ntem)(1:3)
      WRITE(64,1020) natom
      WRITE(64,1030) SNGL(t),-SNGL(fopt),cpb,ntotmov      
C       Now the PDB...
C>> JCC included again
      CALL sagminv(f2cpdb,inv,3)

C>> Add in a Header record

      WRITE(65,1036)
      WRITE(65,1040) SNGL(t),-SNGL(fopt),cpb,ntotmov
      WRITE(65,1050) (cellpar(ii),ii=1,6),SGHMaStr(NTem)

C>> JCC Add in V2 pdb records to store space group and symmetry

      WRITE(65,1380)
      WRITE(65,1381)
      WRITE(65,1382) SGHMaStr(NTem)
      WRITE(65,1380)
      WRITE(65,1383)
      WRITE(65,1384)
      DO i = 1, npdbops
        WRITE(65,1385) (i*1000 + 555), cpdbops(i)
      END DO
      WRITE(65,1380)
      WRITE(65,1386)
      WRITE(65,1387)
      WRITE(65,1380)
      WRITE(65,1388)

C>> JCC included again
      WRITE(65,1060) inv(1,1),inv(1,2),inv(1,3)
      WRITE(65,1070) inv(2,1),inv(2,2),inv(2,3)
      WRITE(65,1080) inv(3,1),inv(3,2),inv(3,3)
C       And the CCL
      WRITE(66,1090) SNGL(t),-SNGL(fopt),cpb,ntotmov
c        write(66,1100) a(nfrag),b(nfrag),c(nfrag),
c     &                 al(nfrag),be(nfrag),ga(nfrag)
      WRITE(66,1100) (cellpar(ii),ii=1,6)

C>> Was 
C       ii = 0
      iiact  = 0
      itotal = 0
      ipcount = 0
C>> To revert this code, set ii to iiact and iorig to i
      CheckedFragNo = 0
      DO j = 1, nfrag
        itotal = iiact
        DO WHILE ( CheckedFragNo .LE. CheckSize )
          CheckedFragNo = CheckedFragNo + 1
          IF ( IZMCheck(CheckedFragNo) .EQ. 1 ) EXIT ! the loop
        END DO

C>> Write out the translation/rotation information for each residue
        WRITE(65,1039) j
        WRITE(65,1037) 
     &   (SNGL(parvals(ij)),ij = ipcount + 1, ipcount + 3)
        IF ( natoms(CheckedFragNo) .GT. 1) THEN

C>> Normalise the Q-rotations before writing them out ...
          qvals(1) = SNGL( parvals(ipcount + 4) )
          qvals(2) = SNGL( parvals(ipcount + 5) )
          qvals(3) = SNGL( parvals(ipcount + 6) )
          qvals(4) = SNGL( parvals(ipcount + 7) )
          qnrm =  SQRT(qvals(1)*qvals(1) + 
     &                 qvals(2)*qvals(2) + 
     &                 qvals(3)*qvals(3) +
     &                 qvals(4)*qvals(4))
          DO ij = 1,4
            qvals(ij) = qvals(ij)/qnrm
          END DO
          WRITE(65,1038)    (qvals(ij),ij = 1,4)
          ipcount = ipcount + izmpar(CheckedFragNo)
        ENDIF
        DO i = 1, natoms(CheckedFragNo) 
C>> Was   ii = ii + 1
          iiact = iiact + 1
          ii = itotal + izmbid(i,CheckedFragNo)
          iorig = izmbid(i,CheckedFragNo)
C         
C         The CSSR atom lines
C>> Was          write(64,1110) ii,asym(i,j),(xatopt(k,ii),k=1,3)
          WRITE(64,1110) 
     &    iiact,asym( iorig , CheckedFragNo),(xatopt(k,ii),k=1,3)
C       The PDB atom lines

C>> JCC Changed to use the PDB's orthogonalisation  definition
C>> JCC Shouldnt make any difference the next change - I've made sure that the conversion
C>> Uses single precision, but I think this is implicit anyway 
C>>          xc=  xatopt(1,ii)*SNGL(f2cmat(1,1))
C>>
C>>          yc= (xatopt(2,ii)*SNGL(f2cmat(2,2)))
C>>     &      + (xatopt(1,ii)*SNGL(f2cmat(1,2)))
C>>
C>>          zc= (xatopt(3,ii)*SNGL(f2cmat(3,3)))
C>>     &      + (xatopt(1,ii)*SNGL(f2cmat(1,3)))
C>>     &      + (xatopt(2,ii)*SNGL(f2cmat(2,3)))

C     Now rotate cartesians about y
c     rnew=(-1.0*(be(nfrag)-90.))*.0174533
C>>   rnew=(-1.0*(cellpar(5)-90.))*.0174533
C>>   xc=xc*cos(rnew) + zc*sin(rnew)
C>>   zc=zc*cos(rnew) - xc*sin(rnew)
      
          xc=   xatopt(1,ii)*SNGL(f2cpdb(1,1))
     &        + xatopt(2,ii)*SNGL(f2cpdb(1,2))
     &        + xatopt(3,ii)*SNGL(f2cpdb(1,3))
          yc=   xatopt(2,ii)*SNGL(f2cpdb(2,2))
     &        + xatopt(3,ii)*SNGL(f2cpdb(2,3))
          zc=   xatopt(3,ii)*SNGL(f2cpdb(3,3))
     
C>> Was
C          if (asym(i,j)(2:2).eq.' ') then
C            write(65,1120) ii,asym(i,j),xc,yc,zc
C          else
C            write(65,1130) ii,asym(i,j),xc,yc,zc
C          endif
C>> Now
          IF (asym(iorig,CheckedFragNo)(2:2) .EQ. ' ') THEN
            WRITE(65,1120) iiact,asym(iorig,CheckedFragNo),xc,yc,zc
          ELSE
            WRITE(65,1130) iiact,asym(iorig,CheckedFragNo),xc,yc,zc
          ENDIF
C       The CCL atom lines
          WRITE(66,1033) asym(iorig,CheckedFragNo),(xatopt(k,ii),k=1,3)
        END DO
      END DO
      WRITE(65,1400)
      CLOSE(64)
      CLOSE(65)
      CLOSE(66)
      CALL UpdateViewer()
C
1000  format(' REFERENCE STRUCTURE = 00000   A,B,C =',3F8.3)
1010  format('   ALPHA,BETA,GAMMA =',3F8.3,'   SPGR = ',A3)
1020  format(' ',I3,'   0  DASH solution')
1030  format(' T=',F6.2,', chi**2=',F7.2, ' and profile chi**2=',  
     &       F7.2,' after ',I8,' moves')
1036  format('HEADER PDB Solution File generated by DASH')
1037  format('REMARK Translations: ',3F10.6)
1038  format('REMARK Q-Rotations : ',4F10.6)
1039  format('REMARK Start of molecule number ',I6)
1040  format('REMARK T=',F6.2,', chi**2=',F7.2,' and profile chi**2=', 
     &       F7.2,' after ',I8,' moves')
c1050  format('CRYST1',3F9.3,3F7.2,' P 21/c')
1050  format('CRYST1',3F9.3,3F7.2,X,A12)
1060  format('SCALE1    ',3F10.5,'      0.00000')
1070  format('SCALE2    ',3F10.5,'      0.00000')
1080  format('SCALE3    ',3F10.5,'      0.00000')
1090  format('Z ','T=',F6.2,', chi**2=',F7.2,' and profile chi**2=', 
     &       F7.2,' after ',i8,' moves')
1100  format('C ',6F10.5)
1110  format(' ',I3,' ',A3,' ',3F10.5)
1120  format('HETATM',I5,'  ',A3,' NONE    1    ',3F8.3,
     &       '  1.00  0.00')
1130  format('HETATM',I5,' ',A3,'  NONE    1    ',3F8.3,
     &       '  1.00  0.00')
1033  format('A ',A3,' ',3F10.5,'  3.0  1.0')
1380  format('REMARK 290 ')
1381  format('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')
1382  format('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
1383  format('REMARK 290      SYMOP   SYMMETRY')
1384  format('REMARK 290     NNNMMM   OPERATOR')
1385  format('REMARK 290',5X,I6,3X,A)
1386  format('REMARK 290     WHERE NNN -> OPERATOR NUMBER')
1387  format('REMARK 290           MMM -> TRANSLATION VECTOR')
1388  format('REMARK 290 REMARK:')
1400  format('END')
      return

      end SUBROUTINE SA_structure_output
!
!*****************************************************************************
!
      SUBROUTINE SAGMINV(A,B,N)

      DIMENSION II(100),IL(100),IG(100)
      REAL*8 A(N,N),B(N,N)
C
      CALL SAGMEQ(A,B,N,N)
      D=1.0
      IS=N-1
      DO 10 K=1,N
      IL(K)=0
   10 IG(K)=K
C
      DO 150 K=1,N
      R=0.
      DO 40 I=1,N
      IF (IL(I) .NE. 0) GO TO 40
      W=B(I,K)
      X=ABS(W)
      IF (R .GT. X) GO TO 40
      R=X
      P=W
      KF=I
   40 CONTINUE
      II(K)=KF
      IL(KF)=KF
      D=D*P
C      IF (D .EQ. 0.) write(*,*) 'Zero determinant'
C
      DO 80 I=1,N
      IF (I .EQ. KF) THEN
      B(I,K)=1./P
      ELSE
      B(I,K)=-B(I,K)/P
      ENDIF
   80 CONTINUE
C
      DO 140 J=1,N
      IF (J .EQ. K) GO TO 140
      W=B(KF,J)
      IF (W .EQ. 0.) GO TO 140
      DO 130 I=1,N
      IF (I .EQ. KF) THEN
      B(I,J)=W/P
      ELSE
      B(I,J)=B(I,J)+W*B(I,K)
      ENDIF
  130 CONTINUE
  140 CONTINUE
C
  150 CONTINUE
C.....
C
      DO 190 K=1,IS
      KF=II(K)
      KL=IL(KF)
      KG=IG(K)
      IF(KF .EQ. KG) GO TO 190
      DO 170 I=1,N
      R=B(I,KF)
      B(I,KF)=B(I,KG)
  170 B(I,KG)=R
      DO 180 J=1,N
      R=B(K,J)
      B(K,J)=B(KL,J)
  180 B(KL,J)=R
      IL(KF)=K
      IL(KG)=KL
      IG(KL)=IG(K)
      IG(K)=KF
      D=-D
  190 CONTINUE
      RETURN

      END SUBROUTINE SAGMINV
!
!*****************************************************************************
!
      SUBROUTINE SAGMEQ(A,B,NI,NJ)

CH Sets matrix B = matrix A.
CA On entry A is a real matrix of dimension NIxNJ
CA On exit  B is a real matrix equal to A
CN NI and NJ must be at least 1
C
      REAL*8 A(NI,NJ),B(NI,NJ)
      DO 1 I=1,NI
      DO 1 J=1,NJ
    1 B(I,J)=A(I,J)
      RETURN

      END SUBROUTINE SAGMEQ
!
!*****************************************************************************
!
      SUBROUTINE PDB_SymmRecords()

      PARAMETER (msymmin=10)
      CHARACTER*20 symline
      COMMON /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
      CHARACTER*50 stout

      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops,cpdbops

      REAL rpdb(4,4,mpdbops), rtmp(4,4)
      LOGICAL cmp
      LOGICAL PDB_CmpMat

C Expand the symmetry generators into a list of symm ops by cross-multiplication
      DO i = 1, 4
        DO j = 1, 4
          rpdb(i,j,1) = 0.0
        END DO
        rpdb(i,i,1) = 1.0
      END DO
      DO k = 1, nsymmin
        DO j = 1, 4
          DO i = 1, 4
            rpdb(i,j,k+1) = symmin(i,j,k)
          END DO
        END DO
        CALL PDB_PosTrans( rpdb(1,1,k+1) )
      END DO
      npdbops = nsymmin + 1
      ilast = 0
      iprev = 1
      DO WHILE (ilast .lt. npdbops  .and. npdbops .le. mpdbops)
        ilast = iprev 
        iprev = npdbops + 1
        DO i = 1, npdbops
          DO j = ilast, npdbops
            CALL PDB_MatMul(rpdb(1,1,i),rpdb(1,1,j),rtmp)
            CALL PDB_PosTrans(rtmp) 
            DO k = 1, npdbops
              cmp = PDB_CmpMat( rpdb(1,1,k), rtmp)
              IF ( cmp ) GOTO 11
            END DO
            npdbops = npdbops + 1
            DO k = 1, 4
              DO m = 1, 4
                rpdb(k,m,npdbops) = rtmp(k,m)
              END DO
            END DO
 11         CONTINUE
          END DO
        END DO
      END DO
      DO k = 1, npdbops
        CALL M2S_SYMCON(rpdb(1,1,k),stout,lstout)
        m = 1
        DO WHILE (stout(m:m) .EQ. ' ' .AND. m .LE. 20)
          m = m + 1
        END DO
        cpdbops(k) = stout(m:20)
      END DO

      END SUBROUTINE PDB_SymmRecords
!
!*****************************************************************************
!
      SUBROUTINE PDB_MatMul(a,b,c)

      REAL a(4,4), b(4,4), c(4,4)

      DO i = 1, 4
        DO j = 1, 4
          c(j,i) = a(j,1)*b(1,i) + 
     &             a(j,2)*b(2,i) + 
     &             a(j,3)*b(3,i) + 
     &             a(j,4)*b(4,i)
        END DO
      END DO
      RETURN

      END SUBROUTINE PDB_MatMul
!
!*****************************************************************************
!
      LOGICAL FUNCTION PDB_CmpMat(a,b)

      REAL a(4,4), b(4,4)

      PDB_CmpMat = .FALSE.
      DO i = 1, 4
        DO j = 1, 4
          IF (ABS( a(i,j) - b(i,j) ) .GT. 0.001 ) RETURN
        END DO
      END DO 
      PDB_CmpMat = .TRUE.

      END FUNCTION PDB_CmpMat
!
!*****************************************************************************
!
      SUBROUTINE PDB_PosTrans(r) 

      REAL r(4,4)

      DO i = 1, 3
! Tidy up any rounding errors on the translations
        r(i,4) = FLOAT( NINT( r(i,4)*10000.0 ) ) / 10000.0
        IF ( r(i,4) .LT. -0.01) THEN
          DO WHILE (r(i,4) .lt. -0.01)
            r(i,4) = r(i,4) + 1.0
          END DO
        ELSE
          DO WHILE (r(i,4) .gt. 0.999)
            r(i,4) = r(i,4) - 1.0
          END DO
        END IF
      END DO
      r(4,4) = 1.0
      RETURN 

      END SUBROUTINE PDB_PosTrans
!
!*****************************************************************************
!
      SUBROUTINE AddSingleSolution(ProfileChi,IntensityChi)

      REAL ProfileChi, IntensityChi

      CHARACTER*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &pro_file   
      COMMON /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,
     &log_file,pro_file
      INTEGER logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen
      COMMON /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,
     &log_flen,pro_flen

      LOGICAL RESTART
      INTEGER SA_Run_Number
      COMMON /MULRUN/ RESTART, SA_Run_Number, 
     &                MaxRuns, MinMoves, MaxMoves, ChiMult

      SA_Run_Number = 1
      CALL Log_SARun_Entry(pdb_file,ProfileChi,IntensityChi)

      END SUBROUTINE AddSingleSolution
!
!*****************************************************************************
!
      SUBROUTINE AddMultiSolution(ProfileChi,IntensityChi)

      USE WINTERACTER

      REAL ProfileChi, IntensityChi

      LOGICAL RESTART
      INTEGER SA_Run_Number
      COMMON /MULRUN/ RESTART, SA_Run_Number, 
     &                MaxRuns,  MinMoves, MaxMoves, ChiMult
      CHARACTER*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &pro_file   
      COMMON /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,
     &log_file,pro_file
      INTEGER logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen
      COMMON /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,
     &log_flen,pro_flen
c
      CHARACTER*85 new_fname

      SA_Run_Number = SA_Run_Number + 1
      CALL AppendNumToFileName(SA_Run_Number,cssr_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile( cssr_file(1:LEN_TRIM(cssr_file)),new_fname)
      CALL AppendNumToFileName(SA_Run_Number,ccl_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile( ccl_file(1:LEN_TRIM(ccl_file)),new_fname)
C ep appended
      CALL AppendNumToFileName(SA_Run_Number,pro_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile( pro_file(1:LEN_TRIM(pro_file)),new_fname)
      CALL AppendNumToFileName(SA_Run_Number,pdb_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile( pdb_file(1:LEN_TRIM(pdb_file)),new_fname)
      CALL Log_SARun_Entry(new_fname,ProfileChi,IntensityChi)

      END SUBROUTINE AddMultiSolution
!
!*****************************************************************************
!
      SUBROUTINE AppendNumToFileName(Num,infilename,outfilename)

      CHARACTER*(*) infilename, outfilename
      INTEGER iinlen,icount
      CHARACTER*3 NumStr

      iinlen = LEN_TRIM(infilename)
      ipos = 0
      iout = 1
      WRITE(NumStr,'(I3.3)') Num
      DO I = 1, LEN(outfilename)
        outfilename(I:I) = ' '
      END DO
      icount = iinlen
      DO WHILE (icount .GT. 0)
! Find the last dot in the filename
        IF (infilename(icount:icount) .EQ. '.') THEN
          ipos = icount
          EXIT
        END IF
        icount = icount - 1
      END DO
      icount = 1
      DO WHILE (icount .LT. ipos)
        outfilename(icount:icount) = infilename(icount:icount)
        icount = icount + 1
      END DO
      iout = icount
      outfilename(iout:iout+3) = '_'//NumStr
      iout = iout + 4
      DO WHILE (icount .LE. iinlen)
        outfilename(iout:iout) = infilename(icount:icount)
        icount = icount + 1
        iout = iout + 1
      END DO

      END SUBROUTINE AppendNumToFileName
!
!*****************************************************************************
!
      SUBROUTINE UpdateViewer()

      USE VARIABLES

      IF (AutoUpdate .AND. ViewAct) CALL ViewBest

      END SUBROUTINE UpdateViewer
!
!*****************************************************************************
!
