      SUBROUTINE SimulatedAnnealing(imyexit)
!
      EXTERNAL FCN

      INCLUDE 'PARAMS.INC'

      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,C,FSTAR,XP,FOPT
      common /sacmn/ XOPT(NMAX),C(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT
!
      INTEGER  NACP(NMAX), NS, NT, NFCNEV, IER, ISEED1, ISEED2
      INTEGER MAXEVL, IPRINT, NACC, NOBDS
      LOGICAL  MAXLOG,MAKET0
!
      double precision cen,sig
      logical gaussb
      double precision T,T0,rt,eps,target_value
      common /frgcom/ nfrag,lfrag(maxfrg)
      parameter (mvar=100)
      common /gaubou/ cen(mvar),sig(mvar)
      common /gaulog/ gaussb(mvar)
      character*80  torfile
      logical ltorfil
      common /torfcm/ torfile(mvar)
      common /torlog/ ltorfil(mvar)
      common /jitter/ rjittr
      double precision x,lb,ub,vm,xpreset
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
!
      common /saparl/ T0,rt,eps,target_value
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
!
!
      DOUBLE PRECISION RFIX
      DOUBLE PRECISION RULB(100)
      DOUBLE PRECISION RANARR(30000),RANAR1(30000)
      DOUBLE PRECISION RANAPM(30000),RANGAR(30000)
      DOUBLE PRECISION XR(100),XRP(100)
      DOUBLE PRECISION DXVAV(100),XVSIG(100),FLAV(100)
      double precision X0SUM(100),XSUM(100),XXSUM(100)
      DOUBLE PRECISION XDSS(100),A0SUM(100)
      DOUBLE PRECISION WSUM(100),XWSUM(100),XXWSUM(100)
C.. We have set RANGEN to limited dimensions in MVAR
C.. Really should reduce MVAR all round - to do!
      DOUBLE PRECISION RANGEN(30000,20)

C  Type all internal variables.
      DOUBLE PRECISION FPSUM0, FPSUM1, FPSUM2, FPAV, FPSD
      DOUBLE PRECISION  F, FP, P, PP, RATIO ,DX
      DOUBLE PRECISION RANIN, XK
      INTEGER  NUP, NDOWN, NREJ, NNEW, LNOBDS, H, I, J, M
      INTEGER MRAN,MRAN1,IARR,IAR1

C  Type all functions.
      DOUBLE PRECISION  EXPREP
c
      double precision DP360,DP1,DP0
      double precision RANTORSTO
      common/rantst/ rantorsto(1001,20)
c
      COMMON /SIMSTO/ NP,IP(100),PSTORE(100)
      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150), 
     &TF(150),KTF(150),SITE(150),KSITE(150), 
     &ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      common /posopt/ XATOPT(3,150)

      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)
c
      parameter (maxiter=10000)

      common /pltstore/ xiter(maxiter),tstore(maxiter),
     &foptstore(maxiter),fpavstore(maxiter)
      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)
c
c
      COMMON /CHISTOP/ NOBS,NFIT,IFIT(MCHSTP),CHIOBS,
     &WT(MCHSTP),XOBS(MCHSTP),YOBS(MCHSTP),YCAL(MCHSTP),ESD(MCHSTP)
c
      common /sappcmn/ xpmin,xpmax,ypmin,ypmax
      common /chibest/ ycalbest(MCHSTP)

      CHARACTER*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &pro_file   
      COMMON /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,
     &log_file,pro_file
      INTEGER logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen
      COMMON /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,
     &log_flen,pro_flen
C
      DOUBLE PRECISION PRJMAT0,PRJMAT1,PRJMAT2

      COMMON /PRJMAT/PRJMAT0,PRJMAT1(MPRJMT),PRJMAT2(MPRJMT)
      COMMON /CHISTO/ KKOR,WTIJ(MCHIHS),S2S(MCHIHS),S4S(MCHIHS),
     &IKKOR(MCHIHS),JKKOR(MCHIHS)
C
      LOGICAL USEREF
      COMMON /CHIFS1/ KKORSH,IPRJAV(MCHFS1),SPRJAV(MCHFS1)
      COMMON /CHIFS2/ USEREF(MCHFS2)
      COMMON /ITRINF/ iteration

	LOGICAL RESTART
	INTEGER SA_Run_Number
	DATA SA_Run_Number / 0 /
	COMMON /MULRUN/ RESTART, SA_Run_Number, 
     &                MaxRuns, MinMoves, MaxMoves,ChiMult

	LOGICAL CheckTerm
C ep Required to keep a record of the child windows opened for Chi-sqd
C    vs. Number of moves plots
	INTEGER ChiSqdChildWindows
      COMMON /ChiSqdWindowsUsed/ ChiSqdChildWindows(MaxNumChildWin)

C JCC Initialise PDB output records 

	CALL PDB_SymmRecords()
	CALL Init_MultiRun()
 1    CONTINUE ! The start point for repeats.
c
C  Set initial values.
      num_sa_profile_plot = 0
      num_new_min = 0
      num_old_min = -1
      n = nvar
      npar = nvar
      iteration = 0
      MAKET0 = (T0 .LE. 0.0)
      IF (MAKET0) THEN
        T=100000.0
      ELSE
        T=T0
      END IF
c
!  Initialize the random number generator RANMAR.
! Increment the seeds for each SA run
      CALL RMARIN(ISEED1 + SA_Run_Number, ISEED2 + SA_Run_Number)
!
      CALL MAKXIN(N) 
      DO IV=1,N
        C(IV)=2.0
      END DO
      DO i = 1, nobs
        ycalbest(i) = 0.0
      END DO
      ITERATION=0
      NACC = 0
      NOBDS = 0
      NFCNEV = 0
      NTOTMOV=0
      IER = 99
      do i=1,maxiter
        xiter(i)=i
      end do
      NP=0
      RFIX=1E-3
      DO 10, I = 1, N
         XOPT(I) = X(I)
         NACP(I) = 0
         RULB(I) = UB(I) - LB(I)
         IF (RULB(I).GT.RFIX) THEN
           NP=NP+1
           IP(NP)=I
         END IF
10    CONTINUE
      DO 20, I = 1, NEPS
         FSTAR(I) = 1.0D+20
20    CONTINUE 
C.. Here we are doing something special with torsion angles
      DP360=360.
      DP1=1.
      DP0=0.
      DO II=1,NP
        I=IP(II)
        IF (LTORFIL(I)) THEN
          UB(I)=1.
          LB(I)=0.
          RULB(I)=1.
          VM(I)=1.
          X(I)=MOD(DP360+X(I),DP360)
          XOPT(I)=X(I)
        END IF
      END DO
C  Evaluate the function with input X and return value as F.
      CALL FCN(N,X,F)
C.. Evaluate the profile chi-squared as well
      call valchipro(cpb)
       call SA_PROFILE_PLOT(num_sa_profile_plot)
c
C  If the function is to be minimized, switch the sign of the function.
C  Note that all intermediate and final output switches the sign back
C  to eliminate any possible confusion for the user.

! JvdS According to the compiler, this part was never initialised.
! As it usually works, I assume MAXLOG is usually 0 probably meaning .FALSE.
      MAXLOG = .FALSE.
      IF(.NOT. MAXLOG) F = -F
      NFCNEV = NFCNEV + 1
      FOPT = F
      FSTAR(1) = F
C.. Set up a random number generator store
C.. Use a quick and dirty one from NR
      IM=29282
      IA=1255
      IC=6173
      JRAN=1
      CALL RANX2I
      CALL RANXGI
      DO I=1,30000
        JRAN=MOD(JRAN*IA+IC,IM)
        RANARR(I)=FLOAT(JRAN)/FLOAT(IM)
        RANIN=2.*RANARR(I)-1.
        RANAPM(I)=RANIN
        CALL RANX2E(RANIN,RANAR1(I))
        CALL RANX2G(RANIN,RANGAR(I))
      END DO
      DO J=1,N
        IF (LTORFIL(J)) THEN
          OPEN(20,FILE=TORFILE(J),STATUS='OLD')
          CALL RANSETGEN
          DO I=1,30000
            JRAN=MOD(JRAN*IA+IC,IM)
            RANIN=FLOAT(JRAN)/FLOAT(IM)
! JvdS @ Next line is very odd: this is the only line of code in DASH
! where the variable RANGEN is used.
            CALL RANX2GEN(RANIN,RANGEN(I,J))
          END DO
C.. Now store the transformation from linear to torsion file
          DO KK=1,1001
            XK=0.001*FLOAT(KK-1)
            CALL RANX2GEN(XK,RANTORSTO(KK,J))
          END DO
          DO KK=2,1001
            IF (RANTORSTO(KK,J).GE.X(J)) THEN
              XR(J)=0.001*(FLOAT(KK)+
     &      (X(J)-RANTORSTO(KK,J))/(RANTORSTO(KK,J)-RANTORSTO(KK-1,J)))
             GOTO 202
            END IF
          END DO
 202      CLOSE(20)
        END IF
      END DO
      NS_STORE=NS
      NT_STORE=NT
      IM=7875
      IA=211
      IC=1663
      MRAN=ISEED1
      MRAN1=ISEED2
      MRANG=ISEED2
      MRANGEN=ISEED1
100   NUP = 0
      NREJ = 0
      NNEW = 0
      NDOWN = 0
      LNOBDS = 0
	DO II=1,N
	  X0SUM(II)=0.
	  A0SUM(II)=0.
	  XSUM(II)=0.
	  XXSUM(II)=0.
	  XDSS(II)=0.
	  WSUM(II)=0.
	  XWSUM(II)=0.
	  XXWSUM(II)=0.
	END DO
      FPSUM0=0.
      FPSUM1=0.
      FPSUM2=0.
      PRJMAT0=0.
      DO II=1,KKOR
        PRJMAT1(II)=0.
        PRJMAT2(II)=0.
      END DO
c
C      IF (ITERATION.EQ.0) THEN
C        NT=2
C        NS=5
C      ELSE
C        NT=NT_STORE
C        NS=NS_STORE
C      END IF
C
      nmpert=nt*ns*np
	 call sa_move_status(0,nmpert,0)
      DO 400, M = 1, NT
C.. MRAN RANGE IS 0 -> IM=7875
         MRAN=MOD(MRAN*IA+IC,IM)
         IARR=MRAN+1
         MRAN1=MOD(MRAN1*IA+IC,IM)
         IAR1=MRAN1+1
         MRANG=MOD(MRANG*IA+IC,IM)
         IARG=MRANG+1
         MRANGEN=MOD(MRANGEN*IA+IC,IM)
         IARGEN=MRANGEN+1
         DO 300, J = 1, NS
            DO 200, IH = 1, NP
               H=IP(IH)
C  Generate XP, the trial value of X. Note use of VM to choose XP.
               DO 110, I = 1, N
                     XP(I) = X(I)
110            CONTINUE
               IF (LTORFIL(H)) THEN
                 DX = DP1 + RANAPM(IARR) * VM(H)
                 IARR=IARR+1
                 XRP(H)=MOD(XR(H)+DX,DP1)
                 XP(H)=RANTORGET(XRP(H),H)
               ELSE
                 DX = RANAR1(IAR1) * VM(H)
                 IAR1=IAR1+1
                 XP(H) = X(H) + DX
C  If XP is out of bounds, select a point in bounds for the trial.
                 IF((XP(H) .LT. LB(H)) .OR. (XP(H) .GT. UB(H))) THEN
                  IF (GAUSSB(H)) THEN
                    XP(H) = CEN(H)+SIG(H)*RANGAR(IARG)
                    IARG=IARG+1
                  ELSE
                    XP(H) = LB(H) + RULB(H)*RANARR(IARR)
                    IARR= IARR+1
                  END IF
                  LNOBDS = LNOBDS + 1
                  NOBDS = NOBDS + 1
                 END IF
               END IF
C  Evaluate the function with the trial point XP and return as FP.
               CALL FCN(N,XP,FP)
               FP = -FP
               FPSUM0=FPSUM0+1
               FPSUM1=FPSUM1+FP
               FPSUM2=FPSUM2+FP*FP
               A0SUM(H)=A0SUM(H)+1
	         XDSS(H)=XDSS(H)+(FP-F)**2
C  Accept the new point if the function value increases.
               IF ( FP .GE. F ) THEN
                  X(H) = XP(H)
                  IF (LTORFIL(H)) XR(H)=XRP(H)
                  F = FP
                  NACC = NACC + 1
                  NACP(H) = NACP(H) + 1
	            X0SUM(H)=X0SUM(H)+1.
	            XSUM(H)=XSUM(H)+X(H)
	            XXSUM(H)=XXSUM(H)+X(H)**2
	            EWAIT=EXP(-FP/T)
	            XWSUM(H)=XWSUM(H)+EWAIT*X(H)
	            XXWSUM(H)=XXWSUM(H)+EWAIT*X(H)**2
	            WSUM(H)=WSUM(H)+EWAIT
                  NUP = NUP + 1
C  If greater than any other point, record as new optimum.
                  IF (FP .GT. FOPT) THEN
                     DO 130, I = 1, N
                        XOPT(I) = XP(I)
130                  CONTINUE
                     DO II=1,NATOM
                       DO III=1,3
                         XATOPT(III,II)=XATO(III,II)
                       END DO
                     END DO
                     num_new_min=num_new_min+1
C.. output the structure coordinates in different formats
c                     CALL SA_STRUCTURE_OUTPUT(t,fopt,ntotmov)
c.. plot the profile
c                    call SA_PROFILE_PLOT(num_sa_profile_plot)
                     call valchipro(cpb)
C>> JCC Error! iteration starts as zero, and has not necessarily been incremented yet
C>> Was
C                     CHIPROBEST(iteration)=cpb
C Changed to
				   CHIPROBEST(iteration + 1)=cpb
                     FOPT = FP
                     NNEW = NNEW + 1
C>> JCC Same point as above ...
C         CALL SA_OUTPUT(0,SNGL(T),-sngl(fopt),-SNGL(FPAV),SNGL(FPSD),
C     &   xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,LNOBDS,NNEW,
C     &   nmpert,ntotmov,iteration)
C>> Changed to
         CALL SA_OUTPUT(0,SNGL(T),-sngl(fopt),-SNGL(FPAV),SNGL(FPSD),
     &   xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,LNOBDS,NNEW,
     &   nmpert,ntotmov,iteration + 1)
                  END IF

C  If the point is lower, use the Metropolis criteria to decide on
C  acceptance or rejection.
               ELSE
                  P = EXPREP((FP - F)/T)
                  PP = RANARR(IARR)
                  IARR=IARR+1
                  IF (PP .LT. P) THEN
                     X(H) = XP(H)
                     IF (LTORFIL(H)) XR(H)=XRP(H)
                     F = FP
                     NACC = NACC + 1
                     NACP(H) = NACP(H) + 1
	               X0SUM(H)=X0SUM(H)+1.
	               XSUM(H)=XSUM(H)+X(H)
	               XXSUM(H)=XXSUM(H)+X(H)**2
	               EWAIT=EXP(-FP/T)
	               XWSUM(H)=XWSUM(H)+EWAIT*X(H)
	               XXWSUM(H)=XXWSUM(H)+EWAIT*X(H)**2
	               WSUM(H)=WSUM(H)+EWAIT
                     NDOWN = NDOWN + 1
                  ELSE
                     NREJ = NREJ + 1
                  END IF
               END IF
200         CONTINUE
            movenow=m*np*ns
            call sa_move_status(1,nmpert,movenow)
C>> JCC Same point as above: what if iteration is zero?
C>> Was
C            call sa_refresh(imyexit,iteration,num_new_min,cpb)
C Now
	      call sa_refresh(imyexit,iteration+1,num_new_min,cpb)
            if (imyexit.gt.0.and.imyexit.le.3) then
	        IF ( RESTART ) THEN
		        Call AddMultiSolution(cpb,-sngl(fopt))
			ELSE
				Call AddSingleSolution(cpb,-sngl(fopt))
	        ENDIF
			goto 998 ! terminate and close the log file.
	      end if
300      CONTINUE
C  Adjust VM so that approximately half of all evaluations are accepted.
         DO 310, II = 1, NP
            I=IP(II)
            RATIO = DFLOAT(NACP(I)) /DFLOAT(NS)
            IF (RATIO .GT. .6) THEN
               VM(I) = VM(I)*(1. + C(I)*(RATIO - .6)/.4)
            ELSE IF (RATIO .LT. .4) THEN
               VM(I) = VM(I)/(1. + C(I)*((.4 - RATIO)/.4))
            END IF
            IF (VM(I) .GT. RULB(I)) THEN
               VM(I) = RULB(I)
            END IF
            IF (VM(I) .LT. 0.01*RULB(I)) THEN
               VM(I) = 0.01*RULB(I)
            END IF
310      CONTINUE
         DO 320, I = 1, N
            NACP(I) = 0
320      CONTINUE
400   CONTINUE
C.. Calculate the average energy and deviation
      FPAV=FPSUM1/FPSUM0
      FPSD=SQRT(MAX(DP0,FPSUM2/FPSUM0-FPAV**2))
C..      IF (ITERATION.EQ.0) THEN
C       DO KKK=1,KKOR
C        PRJAV=PRJMAT1(KKK)/PRJMAT0
C        PRJSD=SQRT(MAX(0.00,PRJMAT2(KKK)/PRJMAT0-PRJAV**2))
C        PRJAV=PRJAV/FLOAT(MAXK-2)
C        PRJSD=PRJSD/FLOAT(MAXK-2)
C        WRITE(83,8300) KKK,IKKOR(KKK),JKKOR(KKK),SNGL(PRJAV),SNGL(PRJSD)
C 8300   FORMAT(3I6,2F15.3)
C        SPRJAV(KKK)=-SNGL(ABS(PRJAV))
C       END DO
C       DO IR=1,MAXK
C         USEREF(IR)=.FALSE.
C       END DO
C       CALL SORTX(SPRJAV,IPRJAV,KKOR)
C       KKORSH=3*NVAR
C       DO KKK=1,KKORSH
C         KK=IPRJAV(KKK)
C         USEREF(IKKOR(KK))=.TRUE.
C         USEREF(JKKOR(KK))=.TRUE.
C..       END DO
C	  irtem=0
C       DO IR=1,MAXK
C	    if (useref(ir)) then
C           irtem=irtem+1
C           write(89,*) irtem,ir
C         end if
C       END DO
C..      END IF
C
   	DO I=1,N
        IF (X0SUM(I).GT.0.) THEN
          DXVAV(I)=XSUM(I)/X0SUM(I)
          XVSIG(I)=SQRT(MAX(DP0,
     &       XXSUM(I)/X0SUM(I)-DXVAV(I)*DXVAV(I)))
   	    FLAV(I)=SQRT(MAX(DP0,XDSS(I)/A0SUM(I)))
        END IF
    	END DO
      ntotmov=ntotmov+nmpert
      iteration=iteration+1
      tstore(iteration)=sngl(t)
      foptstore(iteration)=-sngl(fopt)
      fpavstore(iteration)=-sngl(fpav)
c.. corrint specific
      chiprobest(iteration)=cpb
C ep added - following call to Chi_sq_plot routine. PLots Chi-sqd vs.
C    number of moves in a child window
      CALL chi_sq_plot(ntotmov, iteration, cpb) 
      IF (num_new_min .NE. num_old_min) THEN
C.. output the structure coordinates in different formats
        CALL SA_STRUCTURE_OUTPUT(t,fopt,cpb,x,ntotmov)
c.. plot the profile
        CALL SA_PROFILE_PLOT(num_sa_profile_plot)
      END IF
      num_old_min = num_new_min
      CALL SA_PLOT(iteration)
      CALL SA_OUTPUT(1,SNGL(T),-SNGL(fopt),-SNGL(FPAV),SNGL(FPSD),
     &   xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,LNOBDS,NNEW,
     &   nmpert,ntotmov,iteration)
c
C  Check termination criteria.
c
C  Terminate SA if appropriate.
      IF (.FALSE.) THEN
        DO 420, I = 1, N
          X(I) = XOPT(I)
  420   CONTINUE
        IER = 0
        IF (.NOT. MAXLOG) FOPT = -FOPT
      END IF
C.. If we have asked for an initial temperature to be calculated then
C.. do so
      IF (ITERATION.EQ.1) THEN
        IF (MAKET0) THEN
C.. JCC Start temperature increased by 50% as asked for
          T0=FPSD*1.5
          T=T0
          ITERATION=0
        ELSE
          T = T/(1.+(RT*T)/(3.*FPSD))
          F = FOPT
          DO I = 1, N
            X(I) = XOPT(I)
          END DO         
        END IF
        MAKET0=.FALSE.
        GOTO 100
      END IF
C
C  If termination criteria is not met, prepare for another loop.
C.. We will use the energy fluctuation to reduce the temperature
      T = T/(1.+(RT*T)/(3.*FPSD))

      DO 430, I = NEPS, 2, -1
         FSTAR(I) = FSTAR(I-1)
430   CONTINUE
      F = FOPT
      DO 440, I = 1, N
         X(I) = XOPT(I)
440   CONTINUE

      IF (RESTART) THEN
		IF ( CheckTerm(NTOTMOV, CHIPROBEST(iteration) ) ) THEN
			CALL AddMultiSolution(cpb,sngl(-fopt))
C  ep added.  Before the next SA run starts close the Chi-sqd vs. no moves window opened
C in the subroutine Chi_sq_plot. 
                  CALL Close_Chisq_Plot                
			IF ( SA_Run_Number .LT. MaxRuns ) GOTO 1
	    ELSE
	        GO TO 100
		END IF
	ELSE IF (iteration .LT. MaxIter .AND. T .GT. 0.0) THEN
		GO TO 100
	ELSE
		CALL AddSingleSolution(cpb,-sngl(fopt))
	END IF
	imyexit = 3
 998  CONTINUE
      END

      FUNCTION  EXPREP(RDUM)
C  This function replaces exp to avoid under- and overflows and is
C  designed for IBM 370 type machines. It may be necessary to modify
C  it for other machines. Note that the maximum and minimum values of
C  EXPREP are such that they has no effect on the algorithm.

      DOUBLE PRECISION  RDUM, EXPREP

      IF (RDUM .GT. 174.) THEN
         EXPREP = 3.69D+75
      ELSE IF (RDUM .LT. -180.) THEN
         EXPREP = 0.0
      ELSE
         EXPREP = EXP(RDUM)
      END IF

      RETURN
      END FUNCTION  EXPREP

      SUBROUTINE RMARIN(IJ,KL)
C  This subroutine and the next function generate random numbers. See
C  the comments for SA for more information. The only changes from the
C  orginal code is that (1) the test to make sure that RMARIN runs first
C  was taken out since SA assures that this is done (this test didn't
C  compile under IBM's VS Fortran) and (2) typing ivec as integer was
C  taken out since ivec isn't used. With these exceptions, all following
C  lines are original.

C This is the initialization routine for the random number generator
C     RANMAR()
C NOTE: The seed variables can have values between:    0 <= IJ <= 31328
C                                                      0 <= KL <= 30081
      real U(97), C, CD, CM
      integer I97, J97
      common /raset1/ U, C, CD, CM, I97, J97
      if( IJ .lt. 0  .or.  IJ .gt. 31328  .or.
     *    KL .lt. 0  .or.  KL .gt. 30081 ) then
c          print '(A)', ' The first random number seed must have a value
c     *between 0 and 31328'
c          print '(A)',' The second seed must have a value between 0 and
c     *30081'
            stop
      endif
      i = mod(IJ/177, 177) + 2
      j = mod(IJ    , 177) + 2
      k = mod(KL/169, 178) + 1
      l = mod(KL,     169)
      do 2 ii = 1, 97
         s = 0.0
         t = 0.5
         do 3 jj = 1, 24
            m = mod(mod(i*j, 179)*k, 179)
            i = j
            j = k
            k = m
            l = mod(53*l+1, 169)
            if (mod(l*m, 64) .ge. 32) then
               s = s + t
            endif
            t = 0.5 * t
3        continue
         U(ii) = s
2     continue
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
      return
      end SUBROUTINE RMARIN

      function ranmar()
      real U(97), C, CD, CM
      integer I97, J97
      common /raset1/ U, C, CD, CM, I97, J97
         uni = U(I97) - U(J97)
         if( uni .lt. 0.0 ) uni = uni + 1.0
         U(I97) = uni
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.0 ) C = C + CM
         uni = uni - C
         if( uni .lt. 0.0 ) uni = uni + 1.0
         RANMAR = uni
      return
      END function ranmar
C
C
      subroutine RANX2E(DRIN,DROUT)
c
      double precision drin,drout
      common /x2eran/ xs,yran(200)
c
      rin=sngl(drin)
      rin=abs(rin)
      jn=1
        do j=199,1,-1
          if (rin.gt.yran(j)) then
            jn=j
            goto 10
          end if
        end do
c.. rin is between yran(jn) and yran(jn+1) now iterate
 10     xl=xs*float(jn-1)
        xsn=0.5*xs
        xm=xl+xsn
        xh=xm+xsn
      yl=yran(jn)
      yh=yran(jn+1)
      ym=1.-(0.5*xm*xm+xm+1.)*exp(-xm)
 15   xlo=xl
      xmo=xm
      xho=xh
      ylo=yl
      ymo=ym
      yho=yh
      if (abs(yh-yl).lt.1e-3) goto 20
      if (rin.ge.ym) then
        xl=xmo
        xh=xho
        yl=ymo
        yh=yho
      else
        xl=xlo
        xh=xmo
        yl=ylo
        yh=ymo
      end if
      xm=0.5*(xh+xl)
      ym=1.-(0.5*xm*xm+xm+1.)*exp(-xm)
      goto 15
 20   rout=xm
      if (drin.lt.0) rout=-rout
      drout=dble(rout)
c
      return
      end subroutine RANX2E
c
c
c
      subroutine ranx2i
c
      common /x2eran/ xs,yran(200)
c
      xs=0.1
      do i=1,200
        xi=xs*float(i-1)
        yran(i)=1.-(0.5*xi*xi+xi+1.)*exp(-xi)
      end do
      return
      end subroutine ranx2i
C
C
C
      subroutine RANX2G(DRIN,DROUT)
c
      double precision drin,drout
      common /x2gran/ xs,ygran(200)
c
      rin=sngl(drin)
      rin=abs(rin)
      jn=1
        do j=199,1,-1
          if (rin.gt.ygran(j)) then
            jn=j
            goto 10
          end if
        end do
c.. rin is between yran(jn) and yran(jn+1) now iterate
 10     xl=xs*float(jn-1)
        xsn=0.5*xs
        xm=xl+xsn
        xh=xm+xsn
      yl=ygran(jn)
      yh=ygran(jn+1)
      ym=erf(xm)
 15   xlo=xl
      xmo=xm
      xho=xh
      ylo=yl
      ymo=ym
      yho=yh
      if (abs(yh-yl).lt.1e-5) goto 20
      if (rin.ge.ym) then
        xl=xmo
        xh=xho
        yl=ymo
        yh=yho
      else
        xl=xlo
        xh=xmo
        yl=ylo
        yh=ymo
      end if
      xm=0.5*(xh+xl)
      ym=erf(xm)
      goto 15
 20   rout=xm
      if (drin.lt.0) rout=-rout
      drout=dble(rout)
c
      return
      end subroutine RANX2G
c
c
c
      subroutine ranxgi
c
      common /x2gran/ xs,ygran(200)
c
      xs=0.05
      do i=1,200
c
        xi=xs*float(i-1)
        ygran(i)=erf(xi)
c
      end do
c
      return
      end subroutine ranxgi
c
c
c
      function erf(x)
      a=abs(x)
      t=1./(1+0.5*a)
      erf=t*exp(-a*a-1.26551223+t*(1.00002368+t*(.37409196+
     &  t*(.09678418+t*(-0.18628806+t*(.27886807+t*(-1.13520398+
     &  t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))))
      erf=1.-erf
      if(x.lt.0) erf=-erf
c
      end function erf
C
C
C
      SUBROUTINE RANSETGEN
c
      real x(1000),yd(1000),xv(1000)
      common /rangenfun/npt,xs,y(1000)
c
      IM=29282
      IA=1255
      IC=6173
      JRAN=1
C
      do i=1,1000
        read(20,*,err=100) x(i),yd(i)
        npt=i
      end do
c
 100  continue
c
      sumv=0.
      xs=(x(2)-x(1))
      xv(1)=0.
      do i=1,npt
        xv(i+1)=x(i)+xs
        sumv=sumv+yd(i)
      end do
      y(1)=0.
      sumt=0.
      do i=1,npt
        sumt=sumt+yd(i)
        y(i+1)=sumt/sumv
      end do
c
      end SUBROUTINE RANSETGEN
c
c
c
      subroutine RANX2GEN(drin,drout)
c
      double precision drin,drout
      common /rangenfun/npt,xs,y(1000)
c
      npt1=npt+1
c
c.. At this point we have the integral stored from 0 to 360 degrees
c.. There are npt1 integral values.
c
      p5=0.5
      rin=sngl(drin)
c      do ir=1,40000
c        jran=mod(jran*ia+ic,im)

c        rin=float(jran)/float(im)
c
        do j=npt,1,-1
          if (rin.gt.y(j)) then
            jn=j
            goto 10
          end if
        end do
      xm=0.
      goto 20  
c.. rin is between y(jn) amd y(jn+1) now iterate
 10     xl=xs*float(jn-1)
        xsn=0.5*xs
        xm=xl+xsn
        xh=xm+xsn
      yl=y(jn)
      yh=y(jn+1)
      ym=yintpl(jn,p5)
      p=p5
      dp=0.5
 15   xlo=xl
      xmo=xm
      xho=xh
      ylo=yl
      ymo=ym
      yho=yh
      dp=0.5*dp
      if (abs(yh-yl).lt.1e-5) goto 20
      if (rin.ge.ym) then
        xl=xmo
        xh=xho
        yl=ymo
        yh=yho
        p=p+dp
      else
        xl=xlo
        xh=xmo
        yl=ylo
        yh=ymo
        p=p-dp
      end if
      xm=0.5*(xh+xl)
      ym=yintpl(jn,p)
      goto 15
 20   drout=dble(xm)
c
      end subroutine RANX2GEN
c
c
c
      function yintpl(jn,p)
c
      common /rangenfun/npt,xs,y(1000)
      real yt(-2:2)
      real vt(-2:2)
c
      do ii=-2,2
        jt=1+mod(jn+ii+npt-1,npt)
	  yt(ii)=y(jt)
      end do
c
      pm2=p-2
      pm1=p-1
      pp1=p+1
      pp2=p+2
c
      t2=pm1*pp1*p/24.
      t1=-pm2*pp2*p/6.
	yt0=yt(0)
      vt(-2)=t2*pm2
      vt(-1)=t1*pm1
      vt(0)=pm2*pm1*pp1*pp2/4.
      vt(1)=t1*pp1
      vt(2)=t2*pp2
	if (yt(-2).gt.yt0) yt(-2)=yt(-2)-1.
	if (yt(-1).gt.yt0) yt(-1)=yt(-1)-1.
	if (yt(1) .lt.yt0) yt(1)=yt(1)+1.
	if (yt(2) .lt.yt0) yt(2)=yt(2)+1.
      yintpl=0.
      do i=-2,2
        yintpl=yintpl+vt(i)*yt(i)
      end do
c
      end function yintpl
c
c
c
      function rantorget(x,i)
c
      double precision x
      double precision RANTORSTO
      common/rantst/ rantorsto(1001,20)
c
      ival=1.+1000.*x
      if (ival.eq.1001) then
        rantorget=rantorsto(1001,i)
      else
        del=1.+1000.*x-ival
        rantorget=(1.-del)*rantorsto(ival,i)
     &             +del*rantorsto(ival+1,i)
      end if
c
      end function rantorget
c
c
c
      SUBROUTINE MAKXIN(N)
c
      parameter (mvar=100)
      double precision x,lb,ub,vm,xpreset
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
C
      IF (LOG_PRESET) THEN
        DO IV=1,N
          X(IV)=XPRESET(IV)
        END DO
      ELSE
        DO IV=1,N
          X(IV)=LB(IV)+(UB(IV)-LB(IV))*RANMAR()
        END DO
      END IF
      RETURN

      END SUBROUTINE MAKXIN




	LOGICAL FUNCTION CheckTerm(Nmoves, BestProChiSq)

	LOGICAL RESTART
	INTEGER SA_Run_Number
	COMMON /MULRUN/ RESTART, SA_Run_Number, 
     &                MaxRuns, MinMoves, MaxMoves, ChiMult
	COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP

	IF (     Nmoves .GT. MinMoves 
     &   .AND. (  Nmoves .GT. MaxMoves .OR. 
     &            BestProChiSq .LT. ChiMult*PawleyChiSq) )THEN
		CheckTerm = .TRUE.
	ELSE
		CheckTerm = .FALSE.
	END IF
	RETURN
	END FUNCTION CheckTerm