!
!*****************************************************************************
!
      SUBROUTINE SimulatedAnnealing(imyexit)

      EXTERNAL FCN

      INCLUDE 'PARAMS.INC'

      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,C,FSTAR,XP,FOPT
      COMMON /sacmn/ XOPT(NMAX),C(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT

      INTEGER NACP(NMAX), NS, NT, NFCNEV, ISEED1, ISEED2
      INTEGER MAXEVL, IPRINT, NACC
      LOGICAL MAXLOG,MAKET0

      COMMON /frgcom/ nfrag,lfrag(maxfrg)
      PARAMETER (mvar=100)
      DOUBLE PRECISION x,lb,ub,vm,xpreset
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      COMMON /presetr/ xpreset(mvar)
      LOGICAL log_preset
      COMMON /presetl/ log_preset

      DOUBLE PRECISION T,T0,rt,eps,target_value
      COMMON /saparl/ T0,rt,eps,target_value
      COMMON /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      COMMON /shadl/ log_shad(mvar)
      COMMON /shadi/ kshad(mvar)

      DOUBLE PRECISION RFIX
      DOUBLE PRECISION RULB(100)
      DOUBLE PRECISION RANARR(30000),RANAR1(30000)
      DOUBLE PRECISION DXVAV(100),XVSIG(100),FLAV(100)
      double precision X0SUM(100),XSUM(100),XXSUM(100)
      DOUBLE PRECISION XDSS(100),A0SUM(100)
      DOUBLE PRECISION WSUM(100),XWSUM(100),XXWSUM(100)

C  Type all internal variables.
      DOUBLE PRECISION FPSUM0, FPSUM1, FPSUM2, FPAV, FPSD
      DOUBLE PRECISION  F, FP, P, PP, RATIO ,DX
      DOUBLE PRECISION RANIN
      INTEGER  NUP, NDOWN, NREJ, H, I, J, M
      INTEGER MRAN,MRAN1,IARR,IAR1

C  Type all functions.
      DOUBLE PRECISION EXPREP

      DOUBLE PRECISION DP0

      COMMON /SIMSTO/ NP,IP(100),PSTORE(100)
      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150), 
     &TF(150),KTF(150),SITE(150),KSITE(150), 
     &ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
      COMMON /posopt/ XATOPT(3,150)

      COMMON /FCSTOR/MAXK,FOB(150,MFCSTO)

      COMMON /PLTSTO2/ CHIPROBEST(MAXITER)

      COMMON /CHISTOP/ NOBS,NFIT,IFIT(MCHSTP),CHIOBS,
     &WT(MCHSTP),XOBS(MCHSTP),YOBS(MCHSTP),YCAL(MCHSTP),ESD(MCHSTP)

      COMMON /sappcmn/ xpmin,xpmax,ypmin,ypmax
      COMMON /chibest/ ycalbest(MCHSTP)

      CHARACTER*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,
     &pro_file   
      COMMON /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,
     &log_file,pro_file
      INTEGER logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen
      COMMON /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,
     &log_flen,pro_flen

      DOUBLE PRECISION PRJMAT0,PRJMAT1,PRJMAT2

      COMMON /PRJMAT/PRJMAT0,PRJMAT1(MPRJMT),PRJMAT2(MPRJMT)
      COMMON /CHISTO/ KKOR,WTIJ(MCHIHS),S2S(MCHIHS),S4S(MCHIHS),
     &IKKOR(MCHIHS),JKKOR(MCHIHS)

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
 1    CONTINUE ! The start point for multiple runs.
c
C  Set initial values.
      num_sa_profile_plot = 0
      num_new_min = 0
      num_old_min = -1
      n = nvar
      npar = nvar
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
! Initialise all degrees of freedom either to a preset value or to 
! their current value with some noise added
      CALL MAKXIN(N) 
      DO IV=1,N
        C(IV)=2.0
      END DO
      DO i = 1, nobs
        ycalbest(i) = 0.0
      END DO
      ITERATION = 1
      NACC = 0
      NFCNEV = 0
      NTOTMOV=0
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
      DP0 = 0.0
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
      IF (.NOT. MAXLOG) F = -F
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
        CALL RANX2E(RANIN,RANAR1(I))
      END DO
      IM=7875
      IA=211
      IC=1663
      MRAN=ISEED1
      MRAN1=ISEED2
      MRANG=ISEED2
      MRANGEN=ISEED1
100   NUP = 0
      NREJ = 0
      NDOWN = 0
      DO II=1,N
        X0SUM(II)  = 0.0
        A0SUM(II)  = 0.0
        XSUM(II)   = 0.0
        XXSUM(II)  = 0.0
        XDSS(II)   = 0.0
        WSUM(II)   = 0.0
        XWSUM(II)  = 0.0
        XXWSUM(II) = 0.0
      END DO
      FPSUM0  = 0.0
      FPSUM1  = 0.0
      FPSUM2  = 0.0
      PRJMAT0 = 0.0
      DO II = 1, KKOR
        PRJMAT1(II) = 0.0
        PRJMAT2(II) = 0.0
      END DO
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
         DO 300, J = 1, NS
            DO 200, IH = 1, NP
               H=IP(IH)
C  Generate XP, the trial value of X. Note use of VM to choose XP.
               DO 110, I = 1, N
                     XP(I) = X(I)
110            CONTINUE
                 DX = RANAR1(IAR1) * VM(H)
                 IAR1=IAR1+1
                 XP(H) = X(H) + DX
C  If XP is out of bounds, select a point in bounds for the trial.
                 IF((XP(H) .LT. LB(H)) .OR. (XP(H) .GT. UB(H))) THEN
                   XP(H) = LB(H) + RULB(H)*RANARR(IARR)
                   IARR= IARR+1
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
                     CHIPROBEST(iteration)=cpb
                     FOPT = FP
         CALL SA_OUTPUT(0,SNGL(T),-SNGL(fopt),-SNGL(FPAV),SNGL(FPSD),
     &   xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,
     &   nmpert,ntotmov,iteration)
                  END IF

C  If the point is lower, use the Metropolis criteria to decide on
C  acceptance or rejection.
               ELSE
                  P = EXPREP((FP - F)/T)
                  PP = RANARR(IARR)
                  IARR=IARR+1
                  IF (PP .LT. P) THEN
                     X(H) = XP(H)
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
            CALL sa_move_status(1,nmpert,movenow)
            CALL sa_refresh(imyexit,iteration,num_new_min,cpb)
            IF (imyexit.GT.0.AND.imyexit.LE.3) THEN
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
      DO I=1,N
        IF (X0SUM(I).GT.0.) THEN
          DXVAV(I)=XSUM(I)/X0SUM(I)
          XVSIG(I)=SQRT(MAX(DP0,
     &       XXSUM(I)/X0SUM(I)-DXVAV(I)*DXVAV(I)))
          FLAV(I)=SQRT(MAX(DP0,XDSS(I)/A0SUM(I)))
        END IF
      END DO
      ntotmov=ntotmov+nmpert
C ep added - following call to Chi_sq_plot routine. Plots Chi-sqd vs.
C    number of moves in a child window
      CALL chi_sq_plot(ntotmov, iteration, cpb) 
      iteration=iteration+1
c.. corrint specific
      chiprobest(iteration)=cpb
      IF (num_new_min .NE. num_old_min) THEN
C.. output the structure coordinates in different formats
        CALL SA_STRUCTURE_OUTPUT(t,fopt,cpb,x,ntotmov)
c.. plot the profile
        CALL SA_PROFILE_PLOT(num_sa_profile_plot)
      END IF
      num_old_min = num_new_min
      CALL SA_OUTPUT(1,SNGL(T),-SNGL(fopt),-SNGL(FPAV),SNGL(FPSD),
     &   xopt,dxvav,xvsig,flav,lb,ub,vm,n,NUP,NDOWN,NREJ,
     &   nmpert,ntotmov,iteration)
c
C.. If we have asked for an initial temperature to be calculated then
C.. do so
      IF (ITERATION.EQ.2) THEN
        IF (MAKET0) THEN
C.. JCC Start temperature increased by 50% as asked for
          T0=FPSD*1.5
          T=T0
          ITERATION=1
        ELSE
          T = T/(1.+(RT*T)/(3.*FPSD))
          F = FOPT
          DO I = 1, N
            X(I) = XOPT(I)
          END DO         
        END IF
        MAKET0 = .FALSE.
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

C  Check termination criteria.
c
C  Terminate SA if appropriate.
      IF (RESTART) THEN
        IF ( CheckTerm(NTOTMOV, CHIPROBEST(iteration) ) ) THEN
          CALL AddMultiSolution(cpb,SNGL(-fopt))
C  ep added.  Before the next SA run starts close the Chi-sqd vs. no moves window opened
C in the subroutine Chi_sq_plot. 
          CALL Close_Chisq_Plot                
          IF ( SA_Run_Number .LT. MaxRuns ) GOTO 1
        ELSE
          GOTO 100
        END IF
      ELSE IF ((iteration .LT. MaxIter) .AND. (T .GT. 0.0)) THEN
        GOTO 100
      ELSE
        CALL AddSingleSolution(cpb,-sngl(fopt))
      END IF
      imyexit = 3
 998  CONTINUE

      END SUBROUTINE SimulatedAnnealing
!
!*****************************************************************************
!
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
!
!*****************************************************************************
!
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
      REAL U(97), C, CD, CM
      INTEGER I97, J97
      COMMON /raset1/ U, C, CD, CM, I97, J97

      IF ( (IJ .LT. 0) .OR. (IJ .GT. 31328) .OR.
     &     (KL .LT. 0) .OR. (KL .GT. 30081) ) THEN
        CALL ErrorMessage('Seeds for random numbergenerator must be '//
     & 'between 0 and 30081 and have been reset to 1375 and 29767.')
        IJ = 1375
        KL = 29767
      ENDIF
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
!
!*****************************************************************************
!
      FUNCTION ranmar()

      REAL U(97), C, CD, CM
      INTEGER I97, J97
      COMMON /raset1/ U, C, CD, CM, I97, J97

      uni = U(I97) - U(J97)
      IF (uni .LT. 0.0) uni = uni + 1.0
      U(I97) = uni
      I97 = I97 - 1
      IF (I97 .EQ. 0) I97 = 97
      J97 = J97 - 1
      IF (J97 .EQ. 0) J97 = 97
      C = C - CD
      IF ( C .LT. 0.0 ) C = C + CM
      uni = uni - C
      IF (uni .LT. 0.0) uni = uni + 1.0
      RANMAR = uni
      RETURN

      END FUNCTION ranmar
!
!*****************************************************************************
!
      SUBROUTINE RANX2E(DRIN,DROUT)

      DOUBLE PRECISION drin,drout
      COMMON /x2eran/ xs,yran(200)

      rin=SNGL(drin)
      rin=ABS(rin)
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
!
!*****************************************************************************
!
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
!
!*****************************************************************************
!
      subroutine ranxgi

      common /x2gran/ xs,ygran(200)

      xs=0.05
      do i=1,200
        xi=xs*float(i-1)
        ygran(i)=erf(xi)
      end do
      return

      end subroutine ranxgi
!
!*****************************************************************************
!
      function erf(x)

      a=abs(x)
      t=1./(1+0.5*a)
      erf=t*exp(-a*a-1.26551223+t*(1.00002368+t*(.37409196+
     &  t*(.09678418+t*(-0.18628806+t*(.27886807+t*(-1.13520398+
     &  t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))))
      erf=1.-erf
      if(x.lt.0) erf=-erf

      end function erf
!
!*****************************************************************************
!
      SUBROUTINE MAKXIN(N)

      parameter (mvar=100)
      double precision x,lb,ub,vm,xpreset
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset

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
!
!*****************************************************************************
!
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
!
!*****************************************************************************
!
