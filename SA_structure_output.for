        Subroutine SA_structure_output(t,fopt,cpb,parvals,ntotmov)
C
C       Called when a new minimum is found
C

	double precision t,fopt
	real cpb
	double precision parvals(*) ! The current torsion parameters (cant be called X here)
        character*10 fname
	integer ntotmov
        parameter (maxatm=100)
        parameter (maxfrg=20)
        character*3 asym
        common /zmcomc/ asym(maxatm,maxfrg)
        common /zmcomi/ ntatm,natoms(maxfrg),
     &  ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),
     &  iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)

	  character*36 czmpar
        common /zmnpar/ izmtot,izmpar(maxfrg),
     &   czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)


	  integer ipcount
	  include 'IZMCheck.inc'
	  integer CheckedFragNo


        common /frgcom/ nfrag,lfrag(maxfrg)
        double precision a,b,c,al,be,ga,chrg
        common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),
     &  al(maxfrg),be(maxfrg),ga(maxfrg),chrg(maxatm,maxfrg)
        double precision blen,alph,bet,f2cmat
        common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),
     &  bet(maxatm,maxfrg),f2cmat(3,3)
	double precision inv(3,3)
        common /posopt/ XATOPT(3,150)
      COMMON /POSNS/NATOM,X(3,150),KX(3,150),AMULT(150),
     & TF(150),KTF(150),SITE(150),KSITE(150),
     & ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
C ep appended
        character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &                     pro_file   
        common /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,
     &                     log_file, pro_file
        common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,
     &                     log_flen, pro_flen
c
C>> JCC Cell/Lattice declarations now in an include file


	parameter (mpdbops=192)
	character*20 cpdbops(mpdbops)
	common /pdbops/ npdbops,cpdbops


C>> JCC the original atom ids to list in the labels and the back mapping
	common /zmjcmp/ izmoid(maxatm,maxfrg), izmbid(maxatm,maxfrg)
C>> JCC Use standard PDB orthogonalisation
	double precision f2cpdb, c2fpdb
	real qvals(4), qnrm
	common /pdbcat/ f2cpdb(3,3), c2fpdb(3,3)
	include 'Lattice.inc'
      include 'statlog.inc'
c
      ntem=NumberSGTable

C	ep added.  Following subroutine saves calculated and observed
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
	open(unit=64,file=cssr_file(1:cssr_flen),status='unknown')
	open(unit=65,file=pdb_file(1:pdb_flen),status='unknown')
	open(unit=66,file=ccl_file(1:ccl_flen),status='unknown')
c        write(64,1000) a(nfrag),b(nfrag),c(nfrag)
c        write(64,1010) al(nfrag),be(nfrag),ga(nfrag),
c     &                 SGNumStr(Ntem)(1:3)
        write(64,1000) (cellpar(ii),ii=1,3)
        write(64,1010) (cellpar(ii),ii=4,6),
     &                 SGNumStr(Ntem)(1:3)
        write(64,1020) natom
        write(64,1030) sngl(t),-sngl(fopt),cpb,ntotmov	
C       Now the PDB...
C>> JCC included again
        call sagminv(f2cpdb,inv,3)

C>> Add in a Header record

        write(65,1036)
        write(65,1040) sngl(t),-sngl(fopt),cpb,ntotmov
        write(65,1050) (cellpar(ii),ii=1,6),SGHMaStr(NTem)

C>> JCC Add in V2 pdb records to store space group and symmetry

	  write(65,1380)
	  write(65,1381)
	write(65,1382) SGHMaStr(NTem)
	write(65,1380)
	write(65,1383)
	write(65,1384)
	do i = 1,npdbops
		write(65,1385) (i*1000 + 555), cpdbops(i)
	end do
	write(65,1380)
	write(65,1386)
	write(65,1387)
	write(65,1380)
	write(65,1388)

C>> JCC included again
        write(65,1060) inv(1,1),inv(1,2),inv(1,3)
        write(65,1070) inv(2,1),inv(2,2),inv(2,3)
        write(65,1080) inv(3,1),inv(3,2),inv(3,3)
C       And the CCL
        write(66,1090) sngl(t),-sngl(fopt),cpb,ntotmov
c        write(66,1100) a(nfrag),b(nfrag),c(nfrag),
c     &                 al(nfrag),be(nfrag),ga(nfrag)
        write(66,1100) (cellpar(ii),ii=1,6)
C

C>> Was 
C	  ii = 0
        iiact  = 0
	  itotal = 0
	  ipcount = 0
C>> To revert this code, set ii to iiact and iorig to i
        CheckedFragNo = 0
        do j=1,nfrag
	   itotal = iiact

         do while ( CheckedFragNo .LE. CheckSize )
		CheckedFragNo = CheckedFragNo + 1
		IF ( IZMCheck(CheckedFragNo) .EQ. 1 ) EXIT ! the loop
	   end do

C>> Write out the translation/rotation information for each residue
	   write(65,1039) j
         write(65,1037) 
     &   (sngl(parvals(ij)),ij = ipcount + 1, ipcount + 3)

	   if ( natoms(CheckedFragNo) .GT. 1) then

C>> Normalise the Q-rotations before writing them out ...
		qvals(1) = sngl( parvals(ipcount + 4) )
		qvals(2) = sngl( parvals(ipcount + 5) )
		qvals(3) = sngl( parvals(ipcount + 6) )
		qvals(4) = sngl( parvals(ipcount + 7) )

		qnrm =  sqrt(qvals(1)*qvals(1) + 
     &        	qvals(2)*qvals(2) + 
     &            qvals(3)*qvals(3) +
     &            qvals(4)*qvals(4))

          do ij = 1,4
			qvals(ij) = qvals(ij)/qnrm
	    end do
		write(65,1038)	(qvals(ij),ij = 1,4)
	    ipcount = ipcount + izmpar(CheckedFragNo)

	   endif

         do i=1,natoms(CheckedFragNo) 
C>> Was   ii = ii + 1
          iiact=iiact+1
		ii = itotal + izmbid(i,CheckedFragNo)
	    iorig = izmbid(i,CheckedFragNo)
C         
C         The CSSR atom lines
C>> Was          write(64,1110) ii,asym(i,j),(xatopt(k,ii),k=1,3)
		write(64,1110) 
     &    iiact,asym( iorig , CheckedFragNo),(xatopt(k,ii),k=1,3)
C	  The PDB atom lines

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

C	Now rotate cartesians about y
c	rnew=(-1.0*(be(nfrag)-90.))*.0174533
C>>	rnew=(-1.0*(cellpar(5)-90.))*.0174533
C>>	xc=xc*cos(rnew) + zc*sin(rnew)
C>>	zc=zc*cos(rnew) - xc*sin(rnew)

	
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
          if (asym(iorig,CheckedFragNo)(2:2).eq.' ') then
            write(65,1120) iiact,asym(iorig,CheckedFragNo),xc,yc,zc
          else
            write(65,1130) iiact,asym(iorig,CheckedFragNo),xc,yc,zc
          endif
C	  The CCL atom lines
          write(66,1033) asym(iorig,CheckedFragNo),(xatopt(k,ii),k=1,3)
	 end do
        end do

	write(65,1400)
C
	close(64)
	close(65)
	close(66)

	Call UpdateViewer()
C
1000    format(' REFERENCE STRUCTURE = 00000   A,B,C =',3f8.3)
1010    format('   ALPHA,BETA,GAMMA =',3f8.3,'   SPGR = ',a3)
1020    format(' ',i3,'   0  DASH solution')
1030    format(' T=',F6.2,', chi**2=',F7.2, ' and profile chi**2=',  
     &         F7.2,' after ',i8,' moves')


1036    format('HEADER PDB Solution File generated by DASH')
1037    format('REMARK Translations: ',3f10.6)
1038    format('REMARK Q-Rotations : ',4f10.6)
1039    format('REMARK Start of molecule number ',i6)
1040    format('REMARK T=',F6.2,', chi**2=',F7.2,' and profile chi**2=', 
     &         F7.2,' after ',i8,' moves')
c1050    format('CRYST1',3f9.3,3f7.2,' P 21/c')
1050    format('CRYST1',3f9.3,3f7.2,x,a12)
1060    format('SCALE1    ',3F10.5,'      0.00000')
1070    format('SCALE2    ',3F10.5,'      0.00000')
1080    format('SCALE3    ',3F10.5,'      0.00000')
1090    format('Z ','T=',F6.2,', chi**2=',F7.2,' and profile chi**2=', 
     &         F7.2,' after ',i8,' moves')
1100    format('C ',6F10.5)
1110    format(' ',i3,' ',a3,' ',3f10.5)
1120    format('HETATM',I5,'  ',A3,' NONE    1    ',3f8.3,
     &         '  1.00  0.00')
1130    format('HETATM',I5,' ',A3,'  NONE    1    ',3f8.3,
     &         '  1.00  0.00')
1033    format('A ',A3,' ',3F10.5,'  3.0  1.0')

1380    format('REMARK 290 ')
1381    format('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')
1382    format('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
1383    format('REMARK 290      SYMOP   SYMMETRY')
1384    format('REMARK 290     NNNMMM   OPERATOR')
1385    format('REMARK 290',5x,i6,3x,A)
1386    format('REMARK 290     WHERE NNN -> OPERATOR NUMBER')
1387    format('REMARK 290           MMM -> TRANSLATION VECTOR')
1388    format('REMARK 290 REMARK:')
1400    format('END')
        return
        end
c

c

      SUBROUTINE SAGMINV(A,B,N)
      DIMENSION II(100),IL(100),IG(100)
	 REAL*8 A(N,N),B(N,N)
C
      CALL SAGMEQ(A,B,N,N)
      D=1.
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
      END

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
      END


	subroutine PDB_SymmRecords()

      parameter (msymmin=10)
      character*20 symline
	common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
	character*50 stem,stout,stoutt

	parameter (mpdbops=192)
	character*20 cpdbops(mpdbops)
	common /pdbops/ npdbops,cpdbops

	real rpdb(4,4,mpdbops), rtmp(4,4)
	logical cmp
	logical PDB_CmpMat

C Expand the symmetry generators into a list of symm ops by cross-multiplication
	do i = 1,4
		do j = 1,4
			rpdb(i,j,1) = 0.0
		end do
		rpdb(i,i,1) = 1.0
	end do

	do k = 1, nsymmin
		do j = 1,4
			do i = 1,4
				rpdb(i,j,k+1) = symmin(i,j,k)
			end do
		end do
		call PDB_PosTrans( rpdb(1,1,k+1) )
	end do

	npdbops = nsymmin + 1
	ilast = 0
	iprev = 1
	do while (ilast .lt. npdbops  .and. npdbops .le. mpdbops)
		ilast = iprev 
		iprev = npdbops + 1
		do i = 1, npdbops
			do j = ilast, npdbops
				call PDB_MatMul(rpdb(1,1,i),rpdb(1,1,j),rtmp)

				call PDB_PosTrans(rtmp) 

				do k = 1, npdbops
					cmp = PDB_CmpMat( rpdb(1,1,k), rtmp )
					if ( cmp ) goto 11
				end do

				npdbops = npdbops + 1
				do k = 1,4
					do m = 1,4
						rpdb(k,m,npdbops) = rtmp(k,m)
					end do
				end do
 11               continue
			end do
		end do
	end do

	do k = 1,npdbops
		call M2S_SYMCON(rpdb(1,1,k),stout,lstout)
		m = 1
		do while (stout(m:m) .eq. ' ' .and. m .le. 20)
			m = m + 1
		end do
		cpdbops(k) = stout(m:20)
	end do

	end subroutine PDB_SymmRecords

	subroutine PDB_MatMul(a,b,c)
	real a(4,4),b(4,4),c(4,4)

	do i = 1,4
	  do j = 1,4
		c(j,i) = a(j,1)*b(1,i) + 
     &			 a(j,2)*b(2,i) + 
     &			 a(j,3)*b(3,i) + 
     &			 a(j,4)*b(4,i)
	  end do
	end do

	return
	end

	logical function PDB_CmpMat(a,b)
	real a(4,4),b(4,4), val1,val2
	PDB_CmpMat = .false.

	do i = 1,4
		do j = 1,4
			if (  abs( a(i,j) - b(i,j) ) .gt. 0.001 ) return
		end do
	end do 

	PDB_CmpMat = .true.
	end function PDB_CmpMat

	subroutine PDB_PosTrans(r) 
	real r(4,4)

	do i = 1,3
! Tidy up any rounding errors on the translations
	  r(i,4) = float( nint( r(i,4)*10000.0 ) ) / 10000.0

	  if ( r(i,4) .lt. -0.01) then
		do while (r(i,4) .lt. -0.01)
			r(i,4) = r(i,4) + 1.0
		end do
	  else
		do while (r(i,4) .gt. 0.999)
			r(i,4) = r(i,4) - 1.0
		end do
	  end if

	end do
	r(4,4) = 1.0
	return 
	end subroutine PDB_PosTrans



	subroutine sa_dash_solution_report(T,CHIMIN,CHIAV,CHIESD,
     & xopt,dxvav,xvsig,flav,lb,ub,vm,n,iteration)
!
!
      USE WINTERACTER
      USE DRUID_HEADER
      COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP
!
	real*8 xopt(*),dxvav(*),xvsig(*),flav(*),lb(*),ub(*),vm(*)
!
!
      parameter (maxiter=10000)
      common /pltstore/ xiter(maxiter),tstore(maxiter),
     &        foptstore(maxiter),fpavstore(maxiter)

	parameter (maxfrg=20)
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),
     &      czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)

      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)

	INTEGER ipnum

      OPEN(UNIT=101,FILE='test.sum',STATUS='UNKNOWN',ERR=99)

	WRITE(101,'(a)')'# Dash summary file for current best solution'

      WRITE(101,'(a, f8.2)')
     & '# Best profile chi-squared:   ', CHIPROBEST(iteration)
	WRITE(101,'(a, f8.2)')
     & '# Best intensity chi-squared: ', chimin

      WRITE(101,'(a, f8.2)')
     & '# Current temperature:        ', T
!
	WRITE(101,'(a)')
     & '# Summary of new parameter values follows'

	



	write(101,'(a)')'# Translations'
	write(101,'(3a,12a,12a,12a)')
     &  '#  ','Value','Lower bound','Upper bound'
	write(101,'(a,3f12.5)')'X: ',
     &                       sngl(xopt(1)), sngl(lb(1))  ,sngl(ub(1))
	write(101,'(a,3f12.5)')'Y: ',
     &                       sngl(xopt(2)), sngl(lb(2))  ,sngl(ub(2))
	write(101,'(a,3f12.5)')'Z: ',
     &                       sngl(xopt(3)), sngl(lb(3))  ,sngl(ub(3))

	write(101,'(a)')'# Q-rotations'
	write(101,'(3a,12a,12a,12a)')
     &  '#  ','Value','Lower bound','Upper bound'
	do i = 4,7
	write(101,'(a,i1,1x,3f12.5)')'Q',i,
     &                       sngl(xopt(i)), sngl(lb(i))  ,sngl(ub(i))
	end do

	write(101,'(a)')
	

	


	close(101)
 99   return
	end

      Subroutine AddSingleSolution(ProfileChi,IntensityChi)

	REAL ProfileChi,IntensityChi
C ep appended
      character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &						pro_file
      common /outfilnam/logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &						pro_file
      common /outfillen/logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,
     &						pro_flen
	LOGICAL RESTART
	INTEGER SA_Run_Number
	COMMON /MULRUN/ RESTART, SA_Run_Number, 
     &                MaxRuns, MinMoves, MaxMoves, ChiMult

	SA_Run_Number = 1
	CALL Log_SARun_Entry(pdb_file,ProfileChi,IntensityChi)

	end Subroutine AddSingleSolution

	Subroutine AddMultiSolution(ProfileChi,IntensityChi)
	USE WINTERACTER

	REAL ProfileChi,IntensityChi

	LOGICAL RESTART
	INTEGER SA_Run_Number
	COMMON /MULRUN/ RESTART, SA_Run_Number, 
     &                MaxRuns,  MinMoves, MaxMoves, ChiMult
C ep appended
      character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &    pro_file
      common /outfilnam/logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &    pro_file
      common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,pro_flen
c
	character*85 new_fname

	SA_Run_Number = SA_Run_Number + 1
	CALL AppendNumToFileName(SA_Run_Number,cssr_file,new_fname)
	CALL IOsDeleteFile(new_fname)
	CALL IOsRenameFile( cssr_file(1:len_trim(cssr_file)),new_fname)


	CALL AppendNumToFileName(SA_Run_Number,ccl_file,new_fname)
	CALL IOsDeleteFile(new_fname)
	CALL IOsRenameFile( ccl_file(1:len_trim(ccl_file)),new_fname)

C ep appended
	CALL AppendNumToFileName(SA_Run_Number,pro_file,new_fname)
	CALL IOsDeleteFile(new_fname)
	CALL IOsRenameFile( pro_file(1:len_trim(pro_file)),new_fname)

	CALL AppendNumToFileName(SA_Run_Number,pdb_file,new_fname)
	CALL IOsDeleteFile(new_fname)
	CALL IOsRenameFile( pdb_file(1:len_trim(pdb_file)),new_fname)

	CALL Log_SARun_Entry(new_fname,ProfileChi,IntensityChi)

	end Subroutine AddMultiSolution



	Subroutine AppendNumToFileName(Num,infilename,outfilename)
	character*(*) infilename, outfilename
	integer iinlen,icount
      character*3 NumStr,l
	iinlen = len_trim(infilename)
	ipos = 0
	iout = 1
	WRITE(NumStr,'(I3.3)') Num
	Do I = 1,len(outfilename)
		outfilename(I:I) = ' '
	End Do

	icount = iinlen
	Do while (icount .GT. 0)
! Find the last dot in the filename
		if (infilename(icount:icount) .EQ. '.') then
			ipos = icount
			exit
		end if
		icount = icount - 1
	end do

	icount = 1
	Do while (icount .LT. ipos)
		outfilename(icount:icount) = infilename(icount:icount)
		icount = icount + 1
	End do

	iout = icount
	outfilename(iout:iout+3) = '_'//NumStr
	iout = iout + 4
	Do while (icount .LE. iinlen)
		outfilename(iout:iout) = infilename(icount:icount)
		icount = icount + 1
		iout = iout + 1
	End do

	End Subroutine AppendNumToFileName


	Subroutine UpdateViewer()
      USE WINTERACTER
      USE DRUID_HEADER
!>> JCC 
      USE VARIABLES

	IF (AutoUpdate .AND. ViewAct) THEN
		CALL ViewBest
	END IF
	end Subroutine UpdateViewer
