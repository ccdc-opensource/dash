!
!*****************************************************************************
!
      SUBROUTINE Upload_Crystal_Data()

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'lattice.inc'
      INCLUDE 'GLBVAR.INC' ! Contains ALambda

! Update all the cell constants ...
      CALL Upload_Cell_Constants
      CALL Upload_Zero_Point
      CALL UpdateWavelength(ALambda)
! Let's do a symmetry check
      CALL Check_Crystal_Symmetry()
      CALL RefineLattice()           

      END SUBROUTINE Upload_Crystal_Data
!
!*****************************************************************************
!
      SUBROUTINE Upload_Zero_Point()

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'Lattice.inc'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogPutReal(IDF_zeropt_refine,ZeroPoint,'(F10.4)')
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Zero_Point
!
!*****************************************************************************
!
      SUBROUTINE Upload_Cell_Constants()

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'Lattice.inc'

      CALL PushActiveWindowID
! Update all the cell constants ...
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutReal(IDF_a_latt,CellPar(1),'(F10.5)')
      CALL WDialogPutReal(IDF_b_latt,CellPar(2),'(F10.5)')      
      CALL WDialogPutReal(IDF_c_latt,CellPar(3),'(F10.5)')      
      CALL WDialogPutReal(IDF_alp_latt,CellPar(4),'(F10.3)')      
      CALL WDialogPutReal(IDF_bet_latt,CellPar(5),'(F10.3)')      
      CALL WDialogPutReal(IDF_gam_latt,CellPar(6),'(F10.3)')
!C>> And in the wizard too
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutReal(IDF_a_latt,CellPar(1),'(F10.5)')
      CALL WDialogPutReal(IDF_b_latt,CellPar(2),'(F10.5)')      
      CALL WDialogPutReal(IDF_c_latt,CellPar(3),'(F10.5)')      
      CALL WDialogPutReal(IDF_alp_latt,CellPar(4),'(F10.3)')      
      CALL WDialogPutReal(IDF_bet_latt,CellPar(5),'(F10.3)')      
      CALL WDialogPutReal(IDF_gam_latt,CellPar(6),'(F10.3)')
!C>> And in the peak positions box
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogPutReal(IDF_a_refine,CellPar(1),'(F10.5)')
      CALL WDialogPutReal(IDF_b_refine,CellPar(2),'(F10.5)')      
      CALL WDialogPutReal(IDF_c_refine,CellPar(3),'(F10.5)')      
      CALL WDialogPutReal(IDF_alp_refine,CellPar(4),'(F10.3)')      
      CALL WDialogPutReal(IDF_bet_refine,CellPar(5),'(F10.3)')      
      CALL WDialogPutReal(IDF_gam_refine,CellPar(6),'(F10.3)')
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Cell_Constants
!
!*****************************************************************************
!
      SUBROUTINE Download_Cell_Constants(IDownFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN) :: IDownFrom

      INCLUDE 'Lattice.inc'

      REAL a,b,c,alpha,beta,gamma   

      IF ((IDownFrom .NE. IDD_Crystal_Symmetry) .AND. (IDownFrom .NE. IDD_PW_Page1)) RETURN
      CALL PushActiveWindowID
! Get all the cell constants from the selected area
      CALL WDialogSelect(IDownFrom)
      CALL WDialogGetReal(IDF_a_latt,a)
      CALL WDialogGetReal(IDF_b_latt,b)      
      CALL WDialogGetReal(IDF_c_latt,c)      
      CALL WDialogGetReal(IDF_alp_latt,alpha)      
      CALL WDialogGetReal(IDF_bet_latt,beta)      
      CALL WDialogGetReal(IDF_gam_latt,gamma)
      IF (a     .GT. 0.0) CellPar(1) = a
      IF (b     .GT. 0.0) CellPar(2) = b
      IF (c     .GT. 0.0) CellPar(3) = c
      IF (alpha .GT. 0.0) CellPar(4) = alpha
      IF (beta  .GT. 0.0) CellPar(5) = beta
      IF (gamma .GT. 0.0) CellPar(6) = gamma
      CALL PopActiveWindowID

      END SUBROUTINE Download_Cell_Constants
!
!*****************************************************************************
!
      SUBROUTINE Check_Crystal_Symmetry()

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'Lattice.inc'
      INTEGER OldLatBrav

      CALL PushActiveWindowID
! The routine is called Check_Crystal_Symmetry, but it seems to fill the cell parameters
! The following piece of code is a bit odd. It downloads the cell parameters from a dialogue window
! a. There is a special routine to do so, why isn't it used?
! b. There are two dialogue windows: which one to download from?
! c. Why aren't the global variables CellPar() themselves used?
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogGetReal(IDF_a_latt,CellPar(1))
      CALL WDialogGetReal(IDF_b_latt,CellPar(2))      
      CALL WDialogGetReal(IDF_c_latt,CellPar(3))      
      CALL WDialogGetReal(IDF_alp_latt,CellPar(4))      
      CALL WDialogGetReal(IDF_bet_latt,CellPar(5))      
      CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
!C>> JCC Changed to call Check_Lattice_Type
      OldLatBrav = LatBrav
      CALL Check_Lattice_Type
      IF (OldLatBrav .NE. LatBrav) CALL Set_Crystal_Symmetry(LatBrav)
      CALL PopActiveWindowID

      END SUBROUTINE Check_Crystal_Symmetry
!
!*****************************************************************************
!
      SUBROUTINE Set_Crystal_Symmetry(TheLatticeSystem)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheLatticeSystem

      INCLUDE 'Lattice.inc'

      IF ((TheLatticeSystem .GE. 1) .AND. (TheLatticeSystem .LE. 11)) THEN
        CALL PushActiveWindowID
        LatBrav = TheLatticeSystem
        CALL WDialogSelect(IDD_Crystal_Symmetry)
        CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
        CALL WDialogSelect(IDD_PW_Page1)
        CALL WDialogPutOption(IDF_PW_Crystal_System_Menu,LatBrav)
        CALL PopActiveWindowID
      END IF

      END SUBROUTINE Set_Crystal_Symmetry
!
!*****************************************************************************
!
      SUBROUTINE RefineLattice()

      USE WINTERACTER
      USE DRUID_HEADER

      REAL ChiGetLattice
      EXTERNAL ChiGetLattice

      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL XDD(MPAR),DXDD(MPAR),COVDD(MMPAR)

      INCLUDE 'lattice.inc'
!O      PARAMETER (MVAL=50)
!O      COMMON /LATREFCMN/ LatBrav,IHLR(3,MVAL)
!O      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      INCLUDE 'GLBVAR.INC' ! Contains ALambda

      PARAMETER (MTPeak=100)
      COMMON /ALLPEAKS/ NTPeak,AllPkPosVal(MTPeak),AllPkPosEsd(MTPeak),&
      PkArgK(MTPeak),PkTicDif(MTPeak),PkProb(MTPeak), &
      IOrdTem(MTPeak),IHPk(3,MTPeak),IArgK(MTPeak)

      COMMON /AASVAL/ AAS(6),AASLO(6),AASHI(6)
      INTEGER IASS(6)
      LOGICAL NOCREF

      REAL GReal(3,3),GRec(3,3)
      INTEGER KELPT(6,10)
      DATA KELPT /2,3,4,5,6,7, 2,3,4,5,10,10, 2,3,4,10,5,10, 2,3,4,10,10,5, &
      2,3,4,10,10,10, 2,2,3,10,10,10, 2,2,3,9,10,10, &
      2,2,2,3,3,3, 2,2,3,9,10,10, 2,2,2,10,10,10/ 
!>> JCC Check the wavelength: if the user has not set it, then
!>> we should not be here!

      INCLUDE 'statlog.inc'

      LOGICAL FnWaveLengthOK ! Function

      IF (.NOT. FnWaveLengthOK()) RETURN
      NVal = 0
      DO I = 1, NTPeak
        IOrd = IOrdTem(I)
        IF (PkProb(Iord) .GT. 0.95) THEN
          NVal = NVal + 1
          DO II = 1, 3
            IHLR(II,NVal) = IHPk(II,I)
          END DO
          YVal(NVal) = AllPkPosVal(IOrd)
          EVal(NVal) = AllPkPosEsd(IOrd)
        END IF
      END DO
!C>> JCC Updated to the correct values ...!
      SELECT CASE (LatBrav)
        CASE (2)        ! Triclinic
          NDD = 7
        CASE (3,4,5)    ! Monoclinic (a/b/c-axis)
          NDD = 5
        CASE (6)        ! Orthorhombic
          NDD = 4
        CASE (7,8,9,10) ! Tetragonal/Trigonal/Rhombohedral/Hexagonal
          NDD = 3
        CASE (11)       ! Cubic
          NDD = 2
      END SELECT
      IF (NDD .EQ. 0) RETURN
      IF (NVal .EQ. 0) THEN
        IF (NTPeak .GT. NDD) CALL ErrorMessage('Problems with cell refinement!'//CHAR(13)// &
                                               'Have you entered the cell constants?')
        RETURN
      END IF
      DO I = 1, 3
        GREAL(I,I) = CELLPAR(I)**2
      END DO
      GREAL(1,2)=CELLPAR(1)*CELLPAR(2)*COSD(CELLPAR(6))
      GREAL(1,3)=CELLPAR(1)*CELLPAR(3)*COSD(CELLPAR(5))     
      GREAL(2,3)=CELLPAR(2)*CELLPAR(3)*COSD(CELLPAR(4))
      GREAL(2,1)=GREAL(1,2)    
      GREAL(3,1)=GREAL(1,3)         
      GREAL(3,2)=GREAL(2,3)
! Real = direct space
! GREC = Reciprocal space vectors
      CALL InverseMatrix(GREAL,GREC,3)
      XDD(1) = ZeroPoint
      DXDD(1) = 0.01*ABS(zeropoint)+0.001
      DO I = 1, 3
        AAS(I) = GREC(I,I)
      END DO
      AAS(4) = GREC(1,2)
      AAS(5) = GREC(1,3)
      AAS(6) = GREC(2,3)
      DO I = 1, 6
        AASLO(I) = 0.9*AAS(I)
        AASHI(I) = 1.1*AAS(I)
      END DO
      DO I = 1, 6
        IASS(I) = 0
      END DO
      DO I = 1, NVal
        DO II = 1, 3
          IASS(II) = IASS(II) + IHLR(II,I)**2
        END DO
        IASS(4) = IASS(4) + (IHLR(1,I) * IHLR(2,I))**2
        IASS(5) = IASS(5) + (IHLR(1,I) * IHLR(3,I))**2
        IASS(6) = IASS(6) + (IHLR(2,I) * IHLR(3,I))**2
      END DO
      XDD(2)=GREC(1,1)
!C>> JCC LatBrav is now 1 for unknown so we have to account for this here
      IF ((LatBrav .EQ. 1) .OR. (LatBrav .EQ. 2)) THEN
! Triclinic
        XDD(3) = GREC(2,2) 
        XDD(4) = GREC(3,3)
        XDD(5) = GREC(1,2)
        XDD(6) = GREC(1,3)
        XDD(7) = GREC(2,3)
        NOCREF = .FALSE.
        DO I = 1, 6
          NOCREF = NOCREF .OR. (IASS(I) .EQ. 0)
        END DO
!C>> JCC And here
      ELSE IF (LatBrav .EQ. 3) THEN
! Monoclinic (a-axis)
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(2,3)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(6).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.4) Then
! Monoclinic (b-axis)
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(1,3)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(5).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.5) Then
! Monoclinic (c-axis)
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(1,2)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(4).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.6) Then
! Orthorhombic
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.7) Then
! Tetragonal
        XDD(3)=GREC(3,3)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
!C>> JCC And here
      Else If (LatBrav.eq.8) Then
! Trigonal
        XDD(3)=GREC(3,3)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
!C>> JCC And here
      Else If (LatBrav.eq.9) Then
! Rhombohedral
        XDD(2)=GREC(1,1)
        XDD(3)=GREC(1,2)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0).AND.(IASS(3).EQ.0)) & 
               .OR. ((IASS(4).EQ.0).AND.(IASS(5).EQ.0).AND.(IASS(6).EQ.0))
!C>> JCC And here
      Else If (LatBrav.eq.10) Then
! Hexagonal
        XDD(3)=GREC(3,3)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
!C>> JCC And here
      Else If (LatBrav.eq.11) Then
! Cubic
      End If
      IF (NoCRef) RETURN
      IF (NVal .LE. NDD) RETURN
!
!>> JCC Does this fix the rhombohedral problem?
!      MAXDD=0.
      DDMAX=0.
      DO I=2,NDD
        DDMAX = MAX(DDMAX,1.e-4*ABS(XDD(I)))
      END DO
      DO I = 2, NDD
        DXDD(I) = DDMAX
      END DO
!.. Perform simplex
      CALL SIMOPT(XDD,DXDD,COVDD,NDD,ChiGetLattice)
      XDD(9) = 0.5 * XDD(2)
      XDD(10) = 0.0
      DO I = 1 ,3
!C>> JCC Account for LatBrav now being 1-11, but 2 as triclinic
        GREC(I,I)=XDD(KELPT(I, MAX(LatBrav - 1,1) ))
      END DO
      GREC(1,2)=XDD(KELPT(4, MAX(LatBrav - 1,1) ))
      GREC(1,3)=XDD(KELPT(5, MAX(LatBrav - 1,1) ))     
      GREC(2,3)=XDD(KELPT(6, MAX(LatBrav - 1,1) ))
      GREC(2,1)=GREC(1,2)    
      GREC(3,1)=GREC(1,3)         
      GREC(3,2)=GREC(2,3)
      CALL InverseMatrix(GREC,GREAL,3)
      DO I=1,3
        CellPar(I)=SQRT(MAX(0.,Greal(I,I)))
      END DO
      Cellpar(4)=ACOSD(GReal(2,3)/(CellPar(2)*CellPar(3)))  
      Cellpar(5)=ACOSD(GReal(1,3)/(CellPar(1)*CellPar(3)))            
      Cellpar(6)=ACOSD(GReal(1,2)/(CellPar(1)*CellPar(2)))
      ZeroPoint=XDD(1)
 999  CONTINUE
      CALL Upload_Cell_Constants()
      CALL Upload_Zero_Point()
!  First ensure that we have the plotting mode correct
      CALL IGrPlotMode(' ') 
      CALL Generate_TicMarks()
      IF (NVal .LE. NDD+2) RETURN
!.. Now attempt a quick Pawley refinement
      CALL Quick_Pawley()                     

      END SUBROUTINE RefineLattice
!
!*****************************************************************************
!
      REAL FUNCTION ChiGetLattice(N,P)

      PARAMETER (MPAR=50)
      REAL P(MPAR)

      INCLUDE 'GLBVAR.INC' ! Contains ALambda
      INCLUDE 'lattice.inc'
!O      PARAMETER (MVAL=50)
!O      COMMON /LATREFCMN/ LatBrav,IHLR(3,MVAL)
!O      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      COMMON /AASVAL/ AAS(6),AASLO(6),AASHI(6)

      ChiGetLattice = 0.0
      zp=p(1)
      p4=0.
      p5=0.
      p6=0.
!C>> JCC Changed to correct settings of LatBrav
      If (LatBrav.eq.2 .OR. LatBrav.eq.1) Then
! Triclinic or Unknown
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p4=p(5)
        p5=p(6)
        p6=p(7)
      Else If (LatBrav.eq.3) Then
! Monoclinic - a axis unique
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p6=p(5)
      Else If (LatBrav.eq.4) Then
! Monoclinic - b axis unique
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p5=p(5)
      Else If (LatBrav.eq.5) Then
! Monoclinic - c axis unique
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p4=p(5)
      Else If (LatBrav.eq.6) Then
! Orthorhombic
        p1=p(2)
        p2=p(3)
        p3=p(4)
      Else If (LatBrav.eq.7) Then
! Tetragonal
        p1=p(2)
        p2=p(2)
        p3=p(3)
      Else If (LatBrav.eq.8) Then
! Trigonal
        p1=p(2)
        p2=p(2)
        p3=p(3)
!>> JCC Wrong parameter extracted        p6=0.5*p(2)
          p4=0.5*p(2)
      Else If (LatBrav.eq.9) Then
! Rhombohedral
        p1=p(2)
        p2=p(2)
        p3=p(2)
        p4=p(3)
        p5=p(3)
        p6=p(3)
      Else If (LatBrav.eq.10) Then
! Hexagonal
        p1=p(2)
        p2=p(2)
        p3=p(3)
            p4=0.5*p(2)
!>> JCC Wrong parameter extracted        p6=0.5*p(2)
      Else If (LatBrav.eq.11) Then
! Cubic
        p1=p(2)
        p2=p(2)
        p3=p(2)
      End If
!
!      p1=min(aashi(1),p1)
!      p1=max(aaslo(1),p1)
!
!      p2=min(aashi(2),p2)
!      p2=max(aaslo(2),p2)
!
!      p3=min(aashi(3),p3)
!      p3=max(aaslo(3),p3)
!
!      p4=min(aashi(4),p4)
!      p4=max(aaslo(4),p4)
!
!      p5=min(aashi(5),p5)
!      p5=max(aaslo(5),p5)
!
!      p6=min(aashi(6),p6)
!      p6=max(aaslo(6),p6)
!      
      DO I = 1, NVAL
        vh = IHLR(1,I)
        vk = IHLR(2,I)
        vl = IHLR(3,I)
        dd = vh*vh*p1 + vk*vk*p2 + vl*vl*p3 + 2.0 * (vh*vk*p4 + vh*vl*p5 + vk*vl*p6)
        tthc = 2.0 * ASIND(0.5 * ALambda * SQRT(dd))
        ZI = tthc + zp
        CTem = (ZI - YVal(I)) / EVal(I)
        ChiGetLattice = ChiGetLattice + CTem * CTem
      END DO

      RETURN
      END FUNCTION ChiGetLattice
!
!*****************************************************************************
!
