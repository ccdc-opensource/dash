!*==FORTIC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
! LEVEL 14      SUBROUTINE FORTIC(PNAME,ALSQ,MATSZ,PCXX,PFXX,MAGROU,CALROU,FILNMR)
      SUBROUTINE FORTIC(PNAME,ALSQ,MATSZ,PCXX,PFXX,MAGROU,CALROU,FILNMR)
!
! *** FORTIC from FORTY - WIFD***
!
!X
!C 19B
!H Main body of a PR LSQ program, with dummy arguments
!A PNAME holds the name, probably of the calling program, to print
!A PCXX is the routine for the peak centre
!A PFXX is the routine for the peak function
!A MAGROU is the routine to deal with magnetic parameters.  this will be set to
!A        DOMAG if expecting magnetic parameters, or DUMMY if not.
!A CALROU is the routine to give the calculated function.  At present expected
!A        settings for this are CALPRM for magnetic, CALPR for non-magnetic.
!
!
      INCLUDE 'params.inc'
!
      LOGICAL DFLTPR
      EXTERNAL DFLTPR, PCXX, PFXX, MAGROU, CALROU, RUNPAR, VARSPR
      CHARACTER*6 PNAME
      DIMENSION ALSQ(MATSZ)
      COMMON /DERVAR/ DERIVV(500), LVARV
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
     &                KBCKGD(100,5), NBK, LBKD(20), ZBAKIN
      LOGICAL ZBAKIN
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH
      COMMON /OBSCAL/ OBS, DOBS, GCALC, YCALC, DIFF, ICODE, SUMWD, NOBS,&
     &                IWGH(5), WTC(4), WT, SQRTWT, WDIFF, YBACK, YPEAK, &
     &                YMAX, CSQTOT
      EQUIVALENCE (IWGHT,IWGH(1))
      COMMON /PAWLPR/ AKLO, AKHI, SLACK, STRKT, STRTOL, SLKTOL, ITST,   &
     &                ISPSLK(2,1000), IGSLAK(1000), AMSLAK(2,1000),     &
     &                WTSLAK(1000), WEELEV, KOM16
      LOGICAL STRKT
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),        &
     &                SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED,&
     &                PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC,&
     &        TIC
!>> JCC Moved to an include file
      INCLUDE 'REFLNS.INC'
      COMMON /SLAKDA/ NSLAK(4), SLKSWD(4), SLAKWT(4), CHISQD(4), ISLKTP,&
     &                NSKTOT, KOM24
      COMMON /SLKGEO/ NSTYP, BOBS(500), EOBS(500), IATM(500,2),         &
     &                ISYM(500), ILAT(500), CELLTR(3,500), XSLAK(3,500),&
     &                COSIN(3,3), IABASE(500), NST1, SLONLY, TOSTAR(6,6)&
     &                , BCALC(500), DERCEL(6,500), DERPOS(3,500,2),     &
     &                ITYPSK(500), INVBON(10,500), NINVB(500),          &
     &                INANG(100,3), INTOR(100,6), DERBON(10), NVB(10),  &
     &                NUMBON, NTARNM, NUMANG, NUMTOR, KOM25
      LOGICAL SLONLY
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
     &                NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),      &
     &                NPCSOU(9,5)
!
      LOGICAL LOGIPK
      COMMON /IPKCMN/ LOGIPK, IPK, PIK(MIPK)
!
      COMMON /CMN299/ KIPT(MPTS), KNIPT(MAXPIK), ZNORM(MAXPIK),         &
     &                DZNDKQ(MAXPIK), DZNDVQ(9,MAXPIK), IOCCR(MPTS),    &
     &                JOCCR(MPTS)
!     &KOBZ(MPTS)
      COMMON /CMNNOW/ NOBSNOW
!
!.. Note only 3 phases specifically hardwired here
      COMMON /REFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
!
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS),    &
     &                ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)
!
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      CHARACTER*10 filnmr
      COMMON /commun/ filnam_root
      CHARACTER*10 filnam_root
      filnam_root = filnmr
!
!      write(76,*) ' about to enter PREFIN '
      CALL PREFIN(PNAME)
!
! SET UP PRECISE PROBLEM, AND READ MOST L CARDS:
      CALL REFSET
! THIS ROUTINE IS ONLY FOR ONE PHASE:
      CALL LOGPHA(1)
!      write(76,*) ' about to enter SETPR '
      CALL SETPR(PCXX,PFXX,MAGROU)
!
! COLLECT CONSTRAINTS IMPOSED BY SYMMETRY, AND THOSE REQUESTED, AND
! SET UP PARAMETERS AS VARIABLES (NOT YET AS BASIC VARIABLES)
!      write(76,*) ' about to enter PARSPR '
      CALL PARSPR(MAGROU)
!
! MAKE LIST OF REFLECTION INDICES:
!     write(76,*) ' about to enter INRFPR '
      CALL INRFPR(PCXX,PFXX)
!.. Check if we have too many reflections
      CALL CHKMAXREF(PCXX)
!
!
! OUTPUT H,K,L IF REQUIRED:
!      write(76,*) ' about to enter HKLOUT '
      CALL HKLOUT(PCXX,ALSQ,MATSZ)
!      write(76,*) ' just left HKLOUT '
      RETURN
      END SUBROUTINE FORTIC
!*==FORSYM.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
      SUBROUTINE FORSYM(pname,filnmr)
      CHARACTER*6 PNAME
      CHARACTER*10 filnmr
      COMMON /commun/ filnam_root
      CHARACTER*10 filnam_root
      filnam_root = filnmr
!
      CALL PREFIN(PNAME)
      CALL SYMOP
      CALL OPSYM(1)
      RETURN
      END SUBROUTINE FORSYM
!*==MAKRHM.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE MAKRHM
!.. Makes a number of matrices to speed up the default calculation
!
      DIMENSION H(3)
      INCLUDE 'SGinc\FFCALCTOP.INC'
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3),   &
     &                KOM26
      COMMON /symsto/ sctrh(24,10000), rhsto(3,24,10000)
!
!           CALL ROTSYM(H,RH,I,-1)
!           F1=SCALPR(X(1,N),RH)+SCALPR(TRANS(1,I),H)
      DO ir = 1, maxk
        DO ii = 1, 3
          h(ii) = irefh(ii,ir)
        ENDDO
        DO i = 1, nopc
          CALL rotsym(H,RHSTO(1,i,ir),I,-1)
          sctrh(i,ir) = SCALPR(TRANS(1,I),H)
        ENDDO
      ENDDO
!
      END SUBROUTINE MAKRHM
