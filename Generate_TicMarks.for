      subroutine Generate_TicMarks
C.. This is the routine that generates tic marks
C.. Multiple checks before attempting to calculate tic marks
C.. We need 
C..    (i)   lattice constants
C..    (ii)  space group
C..    (iii) wavelength
C..    (iv)  diffraction file for range limits 
C..            (strictly not necessary - we could put in a 2 theta max of 60 degrees
C..             and redo the tic marks when we load in the data.)
C..   Check the lattice constants
C..   Check the wavelength
C..   Check the space group
      INTEGER           :: I
      INCLUDE 'statlog.inc' 
C
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX
c
C>> JCC Now Cell/lattice declarations in an include file
      INCLUDE 'Lattice.inc'
      INCLUDE 'GLBVAR.INC' ! Contains ALambda

      parameter (msymmin=10)
      character*20 symline
      common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
      character*80 ticfile
C>> JCC Added declaration
      LOGICAL Check_TicMark_Data
      LOGICAL FnWavelengthOK ! Function
      REAL    WavelengthOf ! Function
      INTEGER TicRead
C
!      write(76,*) ' Space group number         : ',SGNumStr(IPosSg)
!      write(76,*) ' Space Group (IT tables)    : ',SGHMaStr(IPosSg)
!      write(76,*) ' Space Group Hall symbol    : ',SGHalStr(IPosSg)
!      write(76,*) ' Space Group explicit symbol: ',SGShmStr(IPosSg)
C
C.. We should only proceed with this if we have good cell constants 
C.. If no wavelength then assume Cu Ka1 wvln=1.54056
C..
   
C>> JCC
C>> Need more checks here.
C>> I think that everything should be set to continue
C>> so I added in these checks. Everything should be bonafide before
C>> we try to add any tick marks in the GUI.
C>> Now call fuller checking function


      IF (.NOT. Check_TicMark_Data()) RETURN
C 
      open(42,file='polyf.ccl',status='unknown')
      write(42,4210) 
 4210 format('N Polyfitter file')
      write(42,4220) (CellPar(i),i=1,6)
 4220 format ('C ',3f10.5,3f10.3)
      write(42,4230) 
 4230 format('F C 2 2.31 20.8439 1.02 10.2075 ',
     &'1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0')
      if (NumberSGTable.ge.1) then
        call DecodeSGSymbol(SGShmStr(NumberSGTable))
        if (nsymmin.gt.0) then
          do isym=1,nsymmin
            write(42,4235) symline(isym)
 4235       format('S ',a)
          end do
        end if
      end if
      write(42,4240) 
 4240 format('I NCYC 6 PRCV 14 MCOR 0 FRIE 1'/  
     &'L REFI RIET'/ 
     &'L SORC SYNX'/
     &'L WGHT 3')
      write(42,4245) xpmin,xpmax
 4245 format('L RTYP    2 ',2f10.3,'   0.001')
      IF (.NOT. FnWavelengthOK()) ALambda = WavelengthOf('Cu')
      WRITE(42,4250) ALambda
 4250 FORMAT('L WVLN ',F10.5)
      IF ((ZeroPoint .LT. -1.0) .OR. (ZeroPoint .GT. 1.0)) ZeroPoint=0.0
      WRITE(42,4260) zeropoint
 4260 FORMAT('L ZERO ',F10.5)
      WRITE(42,4270) 
 4270 FORMAT('L SCAL   0.10000'/
     &'L SLIM 2.0'/
     &'L PKCN TYPE 1'/
     &'L PKFN TYPE 3'/
     &'L PKFN LIMS 0.005'/
     &'L PKFN SIGM    0.0051    0.0001'/
     &'L PKFN GAMM    0.0009    0.2703'/
     &'L PKFN HPSL    0.0250'/
     &'L PKFN HMSL    0.0008'/
     &'L BACK 2 0.0 0.0 0.0 0.0 0.0'/
     &'L VARY ONLY ALL INTS'/
     &'L VARY ALL BACK ')
      CLOSE(42)
c
      CALL Generate_TicMarks_CCSLcode
      ticfile='polyf.tic'
      lenfil=9
C>> JCC Was
C      call Load_Tic_File(9,ticfile)
C>> Now
      TicRead =  Load_Tic_File(9,ticfile)
      IF (TicRead .EQ. 1) CALL Profile_Plot(IPTYPE)

      END
C
C LEVEL 50      subroutine Generate_TicMarks_CCSLcode
      subroutine Generate_TicMarks_CCSLcode
C DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
C A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES
C
      EXTERNAL PCCN01,PFCN03,DUMMY,CALPR
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      DIMENSION ALSQ(100000)
	LOGICAL SDREAD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
     & ICDN(26,9),IERR,IO10,SDREAD

!U    COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),
!U   & ICDN(26,9),IERR,IO10,SDREAD
!U    LOGICAL SDREAD
!U    DIMENSION INREAD(26),ICDNO(26)
!U    EQUIVALENCE (INREAD(1),INREA(1,1))
!U    EQUIVALENCE (ICDNO(1),ICDN(1,1))




      common/iounit/lpt,iti,ito,iplo,luni,iout
      integer matsz
      character*6 xxx
      character*10 fname
      fname='polyf'
      xxx='CN11LS'
      MATSZ=100000
      NINIT=1
      CALL FORTIC(xxx,ALSQ,MATSZ,PCCN01,PFCN03,DUMMY,CALPR,fname)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)
      END
!
!*****************************************************************************
!
      LOGICAL FUNCTION Check_TicMark_Data

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,
     &YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, 
     &XGGMIN,XGGMAX,YGGMIN,YGGMAX

      LOGICAL FnUnitCellOK ! Function
      LOGICAL FnWaveLengthOK ! Function
c
      Check_TicMark_Data = (XPMAX - XPMIN) . GT. 0.1 ! Check that we have some data
      Check_TicMark_Data = Check_TicMark_Data .AND. 
     & FnUnitCellOK() .AND. FnWaveLengthOK()
      RETURN

      END FUNCTION Check_TicMark_Data
!
!*****************************************************************************
!
