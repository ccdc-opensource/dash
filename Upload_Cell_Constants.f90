!
!*****************************************************************************
!
      SUBROUTINE RefineLattice

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      REAL ChiGetLattice
      EXTERNAL ChiGetLattice

      INTEGER     MMPAR
      PARAMETER ( MMPAR = MVAR * MVAR )
      REAL XDD(MVAR),DXDD(MVAR),COVDD(MMPAR)

      INTEGER         NVAL
      REAL                  XVAL,       YVAL,       ZVAL,       EVAL
      COMMON /FUNVAL/ NVAL, XVAL(MVAL), YVAL(MVAL), ZVAL(MVAL), EVAL(MVAL)

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      INTEGER IASS(6)
      LOGICAL NOCREF
      REAL    GReal(3,3), GRec(3,3)
      INTEGER KELPT(6,10)
      DATA KELPT /2,3,4,5,6,7, 2,3,4,5,10,10, 2,3,4,10,5,10, 2,3,4,10,10,5, &
      2,3,4,10,10,10, 2,2,3,10,10,10, 2,2,3,9,10,10, 2,2,2,3,3,3, 2,2,3,9,10,10, 2,2,2,10,10,10/ 
      LOGICAL FnWaveLengthOK, FnUnitCellOK ! Function
      INTEGER I, II, iOrd, NDD
      REAL    DDMAX

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! JCC Check the wavelength: if the user has not set it, then
! we should not be here!
      IF (.NOT. FnWaveLengthOK()) RETURN
! JvdS Check the unit cell parameters: if the user has not set them, then
! we should not be here!
      IF (.NOT. FnUnitCellOK()) RETURN
! The unit cell parameters being refined automatically makes it impossible to 
! play around with indexing. Therefore: when in one of the indexing windows, ignore
! this routine.
      IF ((CurrentWizardWindow .EQ. IDD_PW_Page7) .OR.       &
          (CurrentWizardWindow .EQ. IDD_PW_Page8) .OR.       &
          (CurrentWizardWindow .EQ. IDD_PW_Page9)) RETURN
      NVal = 0
      DO I = 1, NTPeak
        iOrd = IOrdTem(I) ! iOrd is now a pointer into AllPkPosVal, sorted
! Use peak only when probability > 0.95
        IF (PkProb(iOrd) .GT. 0.95) THEN
          CALL INC(NVal)
          DO II = 1, 3
! Yet another place where h, k and l are stored
            IHLR(II,NVal) = IHPk(II,I)
          ENDDO
          YVal(NVal) = AllPkPosVal(iOrd)
          EVal(NVal) = AllPkPosEsd(iOrd)
        ENDIF
      ENDDO
! JCC Updated to the correct values ...!
! Number of degrees of freedom, including the zero point.
      SELECT CASE (LatBrav)
        CASE (1)        ! Triclinic
          NDD = 7
        CASE (2,3,4)    ! Monoclinic (a/b/c-axis)
          NDD = 5
        CASE (5)        ! Orthorhombic
          NDD = 4
        CASE (6,7,8,9)  ! Tetragonal/Trigonal/Rhombohedral/Hexagonal
          NDD = 3
        CASE (10)       ! Cubic
          NDD = 2
      END SELECT
! NVal is the number of peaks indexed with a probability over 95% (the Number of VALid peaks)
      IF (NVal .EQ. 0) THEN
        IF (NTPeak .GT. NDD) CALL ErrorMessage('Problems with cell refinement.'//CHAR(13)// &
                                               'Are the unit cell parameters correct?')
        RETURN
      ENDIF
      DO I = 1, 3
        GREAL(I,I) = CELLPAR(I)**2
      ENDDO
      GREAL(1,2) = CELLPAR(1)*CELLPAR(2)*COSD(CELLPAR(6))
      GREAL(1,3) = CELLPAR(1)*CELLPAR(3)*COSD(CELLPAR(5))     
      GREAL(2,3) = CELLPAR(2)*CELLPAR(3)*COSD(CELLPAR(4))
      GREAL(2,1) = GREAL(1,2)    
      GREAL(3,1) = GREAL(1,3)         
      GREAL(3,2) = GREAL(2,3)
! Real = direct space
! GREC = Reciprocal space vectors
      CALL InverseMatrix(GREAL,GREC,3)
      XDD(1) = ZeroPoint
      DXDD(1) = 0.01*ABS(ZeroPoint)+0.001
      IASS = 0
      DO I = 1, NVal
        DO II = 1, 3
          IASS(II) = IASS(II) + IHLR(II,I)**2
        ENDDO
        IASS(4) = IASS(4) + (IHLR(1,I) * IHLR(2,I))**2
        IASS(5) = IASS(5) + (IHLR(1,I) * IHLR(3,I))**2
        IASS(6) = IASS(6) + (IHLR(2,I) * IHLR(3,I))**2
      ENDDO
      XDD(2) = GREC(1,1)
      SELECT CASE (LatBrav)
        CASE ( 1) ! Triclinic
          XDD(3) = GREC(2,2) 
          XDD(4) = GREC(3,3)
          XDD(5) = GREC(1,2)
          XDD(6) = GREC(1,3)
          XDD(7) = GREC(2,3)
          NOCREF = .FALSE.
          DO I = 1, 6
            NOCREF = NOCREF .OR. (IASS(I) .EQ. 0)
          ENDDO
        CASE ( 2) ! Monoclinic a
          XDD(3) = GREC(2,2) 
          XDD(4) = GREC(3,3)
          XDD(5) = GREC(2,3)
          NOCREF = (IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(6).EQ.0)
        CASE ( 3) ! Monoclinic b
          XDD(3) = GREC(2,2) 
          XDD(4) = GREC(3,3)
          XDD(5) = GREC(1,3)
          NOCREF = (IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(5).EQ.0)
        CASE ( 4) ! Monoclinic c
          XDD(3) = GREC(2,2) 
          XDD(4) = GREC(3,3)
          XDD(5) = GREC(1,2)
          NOCREF = (IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(4).EQ.0)
        CASE ( 5) ! Orthorhombic
          XDD(3) = GREC(2,2) 
          XDD(4) = GREC(3,3)
          NOCREF = (IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0)
        CASE ( 6) ! Tetragonal
          XDD(3) = GREC(3,3)
          NOCREF = ((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
        CASE ( 7) ! Trigonal
          XDD(3) = GREC(3,3)
          NOCREF = ((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
        CASE ( 8) ! Rhombohedral
          XDD(2) = GREC(1,1)
          XDD(3) = GREC(1,2)
          NOCREF = ((IASS(1).EQ.0).AND.(IASS(2).EQ.0).AND.(IASS(3).EQ.0)) & 
               .OR. ((IASS(4).EQ.0).AND.(IASS(5).EQ.0).AND.(IASS(6).EQ.0))
        CASE ( 9) ! Hexagonal
          XDD(3) = GREC(3,3)
          NOCREF = ((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
        CASE (10) ! Cubic
      END SELECT
      IF (NOCREF) RETURN
      IF (NVal .LE. NDD) RETURN
      DDMAX = 0.0
      DO I = 2, NDD
        DDMAX = MAX(DDMAX,1.0E-4*ABS(XDD(I)))
      ENDDO
      DO I = 2, NDD
        DXDD(I) = DDMAX
      ENDDO
! Perform simplex
      IBMBER = 0
      CALL WCursorShape(CurHourGlass)
      CALL SIMOPT(XDD,DXDD,COVDD,NDD,ChiGetLattice)
      CALL WCursorShape(CurCrossHair)
      IF (IBMBER .NE. 0) THEN
        IBMBER = 0
        CALL DebugErrorMessage('Simplex optimisation of cell parameters failed.')
        RETURN
      ENDIF
      XDD(9) = 0.5 * XDD(2)
      XDD(10) = 0.0
      DO I = 1, 3
        GREC(I,I) = XDD(KELPT(I, LatBrav))
      ENDDO
      GREC(1,2) = XDD(KELPT(4, LatBrav))
      GREC(1,3) = XDD(KELPT(5, LatBrav))     
      GREC(2,3) = XDD(KELPT(6, LatBrav))
      GREC(2,1) = GREC(1,2)    
      GREC(3,1) = GREC(1,3)         
      GREC(3,2) = GREC(2,3)
      CALL InverseMatrix(GREC,GREAL,3)
      DO I = 1, 3
        CellPar(I) = SQRT(MAX(0.0,Greal(I,I)))
      ENDDO
      CellPar(4) = ACOSD(GReal(2,3)/(CellPar(2)*CellPar(3)))  
      CellPar(5) = ACOSD(GReal(1,3)/(CellPar(1)*CellPar(3)))            
      CellPar(6) = ACOSD(GReal(1,2)/(CellPar(1)*CellPar(2)))
      ZeroPoint  = XDD(1)
      CALL Upload_Cell_Constants
      CALL Upload_ZeroPoint
      CALL Generate_TicMarks
      IF (NVal .LE. NDD+2) RETURN
! Now attempt a quick Pawley refinement
      CALL ShowPawleyFitWindow

      END SUBROUTINE RefineLattice
!
!*****************************************************************************
!
      REAL FUNCTION ChiGetLattice(N,P)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL P(MVAR)

      INCLUDE 'GLBVAR.INC' ! Contains ALambda
      INCLUDE 'lattice.inc'

      INTEGER         NVAL
      REAL                  XVAL,       YVAL,       ZVAL,       EVAL
      COMMON /FUNVAL/ NVAL, XVAL(MVAL), YVAL(MVAL), ZVAL(MVAL), EVAL(MVAL)

      REAL zp, p1, p2, p3, p4, p5, p6, vh, vk, vl, dd, tthc, ctem
      INTEGER I, N

      ChiGetLattice = 0.0
! Zero point
      zp = p(1)
! Assume cubic
!
! p1, p2 and p3 are the dot products aa, bb and cc
! setting them to the same value means: a = b = c
      p1 = p(2)
      p2 = p(2)
      p3 = p(2)
! p4, p5 and p6 are the dot products ab, ac and ab
! setting them to zero means: angle is 90.0
      p4 = 0.0
      p5 = 0.0
      p6 = 0.0
! Adjust values if not cubic
      SELECT CASE (LatBrav)
        CASE ( 1) ! Triclinic
          p2 = p(3)
          p3 = p(4)
          p4 = p(5)
          p5 = p(6)
          p6 = p(7)
        CASE ( 2) ! Monoclinic a
          p2 = p(3)
          p3 = p(4)
          p6 = p(5)
        CASE ( 3) ! Monoclinic b
          p2 = p(3)
          p3 = p(4)
          p5 = p(5)
        CASE ( 4) ! Monoclinic c
          p2 = p(3)
          p3 = p(4)
          p4 = p(5)
        CASE ( 5) ! Orthorhombic
          p2 = p(3)
          p3 = p(4)
        CASE ( 6) ! Tetragonal
          p3 = p(3)
        CASE ( 7, 9) ! Trigonal / Hexagonal
          p3 = p(3)
          p4 =0.5*p(2)
        CASE ( 8) ! Rhombohedral
          p4 = p(3)
          p5 = p(3)
          p6 = p(3)
        CASE (10) ! Cubic
      END SELECT
      DO I = 1, NVAL
        vh = FLOAT(IHLR(1,I))
        vk = FLOAT(IHLR(2,I))
        vl = FLOAT(IHLR(3,I))
! d-value
        dd = vh*vh*p1 + vk*vk*p2 + vl*vl*p3 + 2.0 * (vh*vk*p4 + vh*vl*p5 + vk*vl*p6)
! 2 theta value
        tthc = 2.0 * ASIND(0.5 * ALambda * SQRT(dd))
! Correct for zero-point error
        ZVAL(I) = tthc + zp
        CTem = (ZVAL(I) - YVAL(I)) / EVAL(I)
        ChiGetLattice = ChiGetLattice + CTem * CTem
      ENDDO

      END FUNCTION ChiGetLattice
!
!*****************************************************************************
!
