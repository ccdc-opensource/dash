!
!*****************************************************************************
!
      SUBROUTINE Generate_TicMarks

      USE VARIABLES
      USE REFVAR
!
! This is the routine that generates tic marks
! Multiple checks before attempting to calculate tic marks
! We need
!    (i)   lattice constants
!    (ii)  space group
!    (iii) wavelength
!    (iv)  diffraction file for range limits
!            (strictly not necessary - we could put in a 2 theta max of 60 degrees
!             and redo the tic marks when we load in the data.)
!   Check the lattice constants
!   Check the wavelength
!   Check the space group
!
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      LOGICAL, EXTERNAL :: Check_TicMark_Data
      INTEGER I, isym
      INTEGER, EXTERNAL :: GETTIC

! Need more checks here.
! I think that everything should be set to continue
! so I added in these checks. Everything should be bonafide before
! we try to add any tick marks in the GUI.
!
      IF (.NOT. Check_TicMark_Data()) THEN
        NumOfRef = 0
        CALL Profile_Plot
        RETURN
      ENDIF
      IF (PastPawley) RETURN
   10 IBMBER = 0
      OPEN (42,FILE='polyf.ccl',STATUS='unknown',ERR=999)
      WRITE (42,4210,ERR=999)
 4210 FORMAT ('N Polyfitter file')
      WRITE (42,4220,ERR=999) (CellPar(I),I=1,6)
 4220 FORMAT ('C ',3F10.5,3F10.3)
      WRITE (42,4230,ERR=999)
 4230 FORMAT ('F C 2 2.31 20.8439 1.02 10.2075 1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0')
      IF (NumberSGTable.GE.1) THEN
        CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
        IF (nsymmin.GT.0) THEN
          DO isym = 1, nsymmin
            WRITE (42,4235,ERR=999) symline(isym)
 4235       FORMAT ('S ',A)
          ENDDO
        ENDIF
      ENDIF
      WRITE (42,4240,ERR=999)
 4240 FORMAT ('I NCYC 6 PRCV 14 MCOR 0 FRIE 1'/'L REFI RIET'/'L SORC SYNX'/'L WGHT 3')
      WRITE (42,4245,ERR=999) XPMIN-ZeroPoint, XPMAX-ZeroPoint
 4245 FORMAT ('L RTYP    2 ',2F10.3,'   0.001')
      WRITE (42,4250,ERR=999) ALambda
 4250 FORMAT ('L WVLN ',F10.5)
      WRITE (42,4260,ERR=999) ZeroPoint
 4260 FORMAT ('L ZERO ',F10.5)
      WRITE (42,4270,ERR=999)
 4270 FORMAT ('L SCAL   0.10000'/'L SLIM 2.0'/'L PKCN TYPE 1'/          &
              'L PKFN TYPE 3'/'L PKFN LIMS 0.005'/                      &
              'L PKFN SIGM    0.0051    0.0001'/                        &
              'L PKFN GAMM    0.0009    0.2703'/'L PKFN HPSL    0.0250'/&
              'L PKFN HMSL    0.0008'/'L BACK 2 0.0 0.0 0.0 0.0 0.0'/   &
              'L VARY ONLY ALL INTS'/'L VARY ALL BACK ')
      CLOSE (42)
      CALL Generate_TicMarks_CCSLcode
      IF (IBMBER .NE. 0) THEN
        NumberSGTable = 1 ! P1
        CALL ErrorMessage('Error while calculating tick marks: space group reset to P1.')
! Set the crystal system
        LatBrav = 1
        CALL Upload_CrystalSystem
        GOTO 10
      ENDIF
      IF (GETTIC('polyf.tic') .EQ. 0) CALL Profile_Plot
      RETURN
  999 CALL ErrorMessage('Error writing temporary file for tick marks: disk full or no write permission?')
      CLOSE (42)

      END SUBROUTINE GENERATE_TICMARKS
!
!*****************************************************************************
!
      SUBROUTINE Generate_TicMarks_CCSLcode
! DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
! A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES
!
      INCLUDE 'Lattice.inc'
      
      EXTERNAL PCCN01, PFCN03, DUMMY
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      LOGICAL MULFAS, MULSOU, MULONE
      DIMENSION ALSQ(100000)

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER matsz
      CHARACTER*6 xxx
      CHARACTER*10 fname

      fname = 'polyf'
      xxx = 'CN11LS'
      MATSZ = 100000
      NINIT = 1
      CALL FORTIC(xxx,ALSQ,MATSZ,PCCN01,PFCN03,DUMMY,fname)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)

      END SUBROUTINE GENERATE_TICMARKS_CCSLCODE
!
!*****************************************************************************
!
      LOGICAL FUNCTION Check_TicMark_Data

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: FnPatternOK, FnUnitCellOK, FnWaveLengthOK

      Check_TicMark_Data = (FnPatternOK() .AND. FnUnitCellOK() .AND. FnWaveLengthOK())

      END FUNCTION CHECK_TICMARK_DATA
!
!*****************************************************************************
!
