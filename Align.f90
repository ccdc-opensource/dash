!********************************************************************************
! Attempts to write out solutions from DASH such that they are superimposable.
! Doesn't treat Sohnke groups any differently from any other space group and
! therefore sometimes get "unaligned" solutions.  Not everything behaves well 
! all the time, for example Pca21 has caused problems.
!********************************************************************************
	SUBROUTINE ALIGN()

      USE DRUID_HEADER
      USE VARIABLES
!      IMPLICIT NONE
      INCLUDE 'GLBVAR.INC' !NumberSGTable
      INCLUDE 'lattice.inc'!Cellpar and space group strings

! Required for XATOPT
      COMMON /posopt/ XATOPT(3,150)
! Required for NATOM
      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
!Required for npdbops
      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops
!
      INTEGER MaxNumAtom !in an include somewhere??
      PARAMETER (MaxNumAtom = 150)

! Local Variables
      CHARACTER(len = 80) :: line
      INTEGER      nlin
      INTEGER      Inversion
      INTEGER      NumOfShifts
      CHARACTER*80 ShiftString
      CHARACTER*5  Shift 
      DIMENSION    Shift(5)
      INTEGER      EquivAxes
      REAL         sumx, sumy, sumz
      REAL, DIMENSION (3)    :: CentreOfMass 
      REAL, DIMENSION (50,3) :: CoMMAtrix
      INTEGER      Transflag
      REAL, DIMENSION(3)     :: ShiftMatrix
      INTEGER, DIMENSION(3)  :: Infiniteaxes
      INTEGER       NumOfTimesToApplyShift
      REAL          snum
      REAL, DIMENSION (50,400,3) ::CoMSGMatrix 
      INTEGER       icount
      INTEGER       INumOfShifts, IinfiniteAxes
      REAL, DIMENSION(3)     :: TempShiftMatrix
      REAL          BestDistance
      REAL          TDistance
      REAL          Centrex, Centrey, Centrez
      REAL          Tempx, Tempy
      INTEGER       ISymOpBest
      INTEGER       IShiftBest
      INTEGER i,j,k
      REAL, DIMENSION(3,MaxNumAtom) ::FinalMol
      REAL, DIMENSION(3,MaxNumAtom) ::ConnArray

!  COMMON BLOCK Created

!  SOSign, SOnumber and SOAxis are matrices which contain the symmetry operators 
!  generated for the space group
       REAL SONumber
       DIMENSION SONumber(50,3)
       CHARACTER*1 SOSign
       DIMENSION SOSign(50,3)
       INTEGER SOAxis
       DIMENSION SOAxis(50,3)
       COMMON/symops/SOSign, SONumber, SOAxis
       
       EXTERNAL ErrorMessage
   

      OPEN(220,file=INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'SGSymbandShift.txt',status='old', err = 10)
      DO j = 1,(NumberSGTable-1)
        READ (220, 50) nlin, line
50      FORMAT(q,a)
      ENDDO
      READ(220,55) Inversion, NumOfShifts, ShiftString
55    FORMAT(28x, I1, 4x, I1, x, A40)
        DO j = 1,NumofShifts
          Shift(j) = ShiftString(((j*5)-4):(j*5))
        END DO 
      CLOSE(220)   
       
!
! Clear Sumx, sumy and sumz
      Sumx = 0.0
      Sumy = 0.0
      Sumz = 0.0

      DO J = 1,3
        TempShiftMAtrix(j) = 0.0
      END DO
! Create matrix containing the symmetry operations - one matrix for the sign and one for
! the fraction - describes situations like 0.5 - x.
! Also creates matrix which stores which axis the symop applies to.  In cases like I-4 
! x and y coordinates are swapped by the symmetry operations
      
      CALL PDB_SymmRecords()

! Determine in there are two equivalent axes i.e. is space group tetragonal,
! trigonal or hexagonal?  Cubic groups and hence three equivalent axes are not handled yet

      IF ((NumberSGTable.ge.349).and.(NumberSGTable.le.488))THEN
       EquivAxes = 2
      END IF

! Calculate centre of mass for input molecule
      DO i = 1,natom
        sumx = sumx + xatopt(1,i)
        sumy = sumy + xatopt(2,i)
        sumz = sumz + xatopt(3,i)
      END DO
     
      CentreOfMass(1) = sumx / FLOAT(natom)
      CentreOfMass(2) = sumy / FLOAT(natom)
      CentreOfMass(3) = sumz / FLOAT(natom)

! If Space Group like P1 where only inversion allowed then set Centre of Mass to (1/2,1/2,1/2)
      IF((Inversion.eq.1).and.(NumOfShifts.eq.0)) THEN
        DO j = 1,3
          CentreOfMass(j) = 0.50
        END DO
      END IF
!
! Start to create matrix which contains possible positions for the centre of mass.  This loop
! will look at translations allowed for the space group and apply them to the original centre
! of mass.  Shifthandler identifies whether the shift is a translation, in which case it returns
! the value of the translation.  If the axes allows infinite translations the centre of mass
! is set to zero and no shift value is returned.  The infinite axis is flagged in the matrix
! InfiniteAxes
!
! First entry into CoMMatrix is the original centre of mass
     DO j = 1,3
      CoMMatrix(1,j) = CentreofMass(j)
     END DO

!        icount = 0
         icount = 1
          DO k = 1,NumOfShifts
            Transflag = 0
            CALL ShiftHandler(Shift(k), ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
              IF (Transflag.ne.1) THEN    
                sNum = MINVAL(ShiftMatrix, MASK=ShiftMAtrix.gt.0)
                NumOfTimesToApplyShift = ((NINT(1/sNum)) - 1)
                DO j = 1,NumOfTimesToApplyShift
                  iCount = iCount+1
                  CoMMAtrix(icount,1) = CentreOfMass(1) + j*ShiftMatrix(1)
                  CoMMAtrix(icount,2) = CentreOfMass(2) + j*ShiftMatrix(2)                                      
                  CoMMAtrix(icount,3) = CentreOfMass(3) + j*ShiftMatrix(3)             
                END DO
              END IF
          END DO

     DO j = 1,3
       IF(InfiniteAxes(j) .EQ. 1) THEN
        CoMMatrix(1,j) = 0.0
       END IF
     END DO


!  Handle Sohnke groups here?
!------------------------------------------------------------------------------------------------
! Clumsily calculate positions for the centre of mass which would be generated by combinations of 
! allowed translations and add them to CoMMatrix
  
! How many shifts are translations on infinite axes
         iinfiniteaxes = 0
         DO j = 1,3
           IF (infiniteaxes(j).eq.1) THEN
             iinfiniteaxes = iinfiniteaxes + 1
           END IF
         END DO

!Calculate number of genuine shifts         
         inumofshifts = NumofShifts - iinfiniteaxes
!If only two shifts, apply them both and calculate centre of mass to put into CoMMatrix         
         IF(inumofshifts.eq.2) THEN
         DO k = 1,inumofshifts                        
           CALL ShiftHandler(Shift(iinfiniteaxes+k), ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
             DO j = 1,3
               TempShiftMatrix(j) = Tempshiftmatrix(j)+shiftMatrix(j)
             END DO
         END DO
           icount = icount + 1
           DO j = 1,3
             CoMMAtrix(icount,j) = CentreOfMass(j) + TempShiftMatrix(j)
           END DO
         END IF

! If there are 3 allowable shifts calculate all possible combinations
         IF(inumofshifts.eq.3) THEN
! Combination x + y
           DO j = 1,3
             Tempshiftmatrix(j) = 0.0
           END DO
           DO k = 1,2
             CALL ShiftHandler(Shift(iinfiniteaxes+k), ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
               DO j = 1,3
               Tempshiftmatrix(j) = tempshiftmatrix(j) + ShiftMatrix(j)
               END DO
           END DO
           icount = icount +1
           DO j = 1,3
             CoMMAtrix(icount,j) = CentreOfMass(j) + TempShiftMatrix(j)
           END DO
! Combination x + z
           DO j = 1,3
             Tempshiftmatrix(j) = 0.0
           END DO
           DO k = 1,3,2
             CALL ShiftHandler(Shift(iinfiniteaxes+k), ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
               DO j = 1,3
               Tempshiftmatrix(j) = tempshiftmatrix(j) + ShiftMatrix(j)
               END DO
           END DO
           icount = icount +1
           DO j = 1,3
             CoMMAtrix(icount,j) = CentreOfMass(j) + TempShiftMatrix(j)
           END DO
! Combination y + z
           DO j = 1,3
             Tempshiftmatrix(j) = 0.0
           END DO
           DO k = 2,3
             CALL ShiftHandler(Shift(iinfiniteaxes+k), ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
             DO j = 1,3
               Tempshiftmatrix(j) = tempshiftmatrix(j) + ShiftMatrix(j)
             END DO
           END DO
           icount = icount +1
           DO j = 1,3
             CoMMAtrix(icount,j) = CentreOfMass(j) + TempShiftMatrix(j)
           END DO
! combination x+y+z
           DO j = 1,3
             Tempshiftmatrix(j) = 0.0
           END DO
           DO k = 1,3
             CALL ShiftHandler(Shift(iinfiniteaxes+k), ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
             DO j = 1,3
               Tempshiftmatrix(j) = tempshiftmatrix(j) + ShiftMatrix(j)
             END DO
           END DO
           icount = icount +1
           DO j = 1,3
             CoMMAtrix(icount,j) = CentreOfMass(j) + TempShiftMatrix(j)
           END DO
         END IF
!--------------------------------------------------------------------------------------------
! Have to perform space group operations on the CoMMatrix to make sure all origins generated:
! CoMSGMatrix: 3 dimensional matrix (i,k,j).  i describes number of CoM's generated
! from shifts, k is number of symmetry ops and j is x,y,or z
          DO i = 1,icount
             DO k = 1,npdbops
               DO j = 1,3
                IF (SOsign(k,j).eq.'+') THEN
                IF(infiniteaxes(j).ne.1) THEN
                   CoMSGMatrix(i,k,j) = SONumber(k,j) + CoMMatrix(i,SOAxis(k,j))
                 END IF
                IF(infiniteaxes(j).eq.1) THEN
                   CoMSGMatrix(i,k,j) = CoMMatrix(i, SOAxis(k,j))
                 END IF   
                END IF 
                IF (SoSign(k,j).eq.'-') THEN
                  CoMSGMatrix(i,k,j) = SONumber(k,j) - CoMMatrix(i,SOAxis(k,j))
                END IF
                IF (SoSign(k,j).eq.'*') THEN
                  CoMSGMatrix(i,k,j) = SONumber(k,j) * CoMMatrix(i,SOAxis(k,j))
                END IF   
               END DO
             END DO
         END DO

          
! If inversion allowed by space group, apply it to all centres of mass in ComSGmatrix and add
! to CoMSGMatrix         
          IF (Inversion.eq.1) THEN
            DO i = 1,icount 
             DO k = 1,npdbops
              DO j=1,3
               CoMSGMatrix((icount+i),k,j) = (-1)*CoMSGMatrix(i,k,j)
              END DO
             END DO
            END DO
           Icount = icount*2
          END IF
!
! Put all centres of mass into one cell          
          CALL CoMCell(CoMSGMatrix, icount, npdbops) 
! Calculate distance of centre of mass from (0,0,0) and store the best solution                 
          tdistance = 0.0
          bestdistance = 100
          DO i = 1, icount
            DO k = 1,npdbops
              tdistance=SQRT((CoMSGMatrix(i,k,1)*CoMSGMatrix(i,k,1))+(CoMSGMatrix(i,k,2)*CoMSGMatrix(i,k,2))+(CoMSGMatrix(i,k,3)*CoMSGMatrix(i,k,3)))
              IF (tdistance.LT.bestdistance) THEN
                centrex = CoMSGMatrix(i,k,1)
                centrey = CoMSGMatrix(i,k,2)
                centrez = CoMSGMatrix(i,k,3)
                bestdistance = tdistance
                isymopbest = k !store symmetry operation which gave best answer
                ishiftbest = i !store shift which gave best answer
              END IF
             END DO
          END DO
! Apply best symmetry operation to original molecule. Take care of inversion if required
! too. Return with matrix (ConnArray) which describes molecule with respect to its 
! centre of mass

     CALL MakeMol(XATOPT, isymopbest, ishiftbest, NATOM, Connarray, icount, inversion)
! To make final molecule add ConnArray to best centre of mass
     DO i = 1,NATOM
       FinalMol(1,i) = ConnArray(1,i) + centrex
       FinalMol(2,i) = ConnArray(2,i) + centrey
       FinalMol(3,i) = ConnArray(3,i) + centrez
     END DO
! If have equivalent axes then apply abitrary criterion to see if x and y should be swapped
     IF (equivAxes.eq.2) THEN
       IF (centrex.gt.centrey) THEN
         DO i = 1,NATOM
           tempx = FinalMol(1,i)
           tempy = FinalMol(2,i)
           FinalMol(1,i) = tempy
           FinalMol(2,i) = tempx
         END DO
       END IF
     END IF

! The following is a nasty, rough and ready fix.  If there is an infinite translation axes
! present the centre of mass coordinate for that axes is set to 0.0.  In some cases an inversion
! can be applied to a molecule and then the axes which are not infinite translation axes reset
! with symmetry operations.  However the infinite translation axes still carries the inversion
! since 0.0 goes to 0.0.  So this quick and dirty fix looks at infinite translation axes and
! reflects the molecule upon an arbitrary criterion.
     IF(inversion.eq.1) THEN
       IF (InfiniteAxes(3).eq.1) THEN
        AtomOnez = FinalMol(3,1)
        Atomtwoz = FinalMol(3,Natom)
          IF (Atomtwoz.LT.Atomonez) THEN
           DO i = 1,natom
             FinalMol(3,i) = centrez - connarray(3,i)
           END DO
         END IF
       END IF
         
     IF (InfiniteAxes(2).eq.1) THEN
       AtomOnez = FinalMol(2,1)
       Atomtwoz = FinalMol(2,Natom)
         IF (Atomtwoz.LT.Atomonez) THEN
          DO i = 1,natom
            FinalMol(2,i) = centrey - connarray(2,i)
          END DO
       END IF
      END IF

       IF (InfiniteAxes(1).eq.1) THEN
        AtomOnez = FinalMol(1,1)
        Atomtwoz = FinalMol(1,Natom)
          IF (Atomtwoz.LT.Atomonez) THEN
           DO i = 1,natom
             FinalMol(1,i) = centrex - connarray(1,i)
           END DO
         END IF
       END IF
      END IF

! FinalMol Contains the solution in fractional coordinates.  Write to Xatopt. 
     DO J = 1, NATOM
        DO K = 1,3
         XATOPT(K,J) = FinalMol(K,J)
        END DO
     END DO

     RETURN
10   CALL ErrorMessage('Could not open file'//CHAR(13)// 'SGSymbandShift.txt' &
                        //CHAR(13)// 'It should be in the installation directory')
           
     END SUBROUTINE Align


!********************************************************************************************
!
!The following three subroutines supply the details of the allowed shifts to Realign
!
!********************************************************************************************
    SUBROUTINE ShiftHandler(Shift, ShiftMatrix, CentreofMass, Transflag, InfiniteAxes)
    USE DRUID_HEADER


    CHARACTER*5 Shift
    REAL, DIMENSION(3) :: ShiftMatrix
    REAL, DIMENSION(3) :: CentreofMass
    INTEGER, DIMENSION(3) :: InfiniteAxes
    INTEGER Transflag
    Shift = TRIM(Shift)

    Transflag = 0
    IF ((Shift.eq.'r').or.(Shift.eq.'s').or.(Shift.eq.'t')) THEN
     CALL ZeroInfiniteTransAxes(Shift, CentreOfMass, Transflag, InfiniteAxes)
    ELSE
     CALL GetShift(Shift, Shiftmatrix)
    END IF
    RETURN
    
    END SUBROUTINE ShiftHandler


!******************************************************************************
    SUBROUTINE GetShift(Shift, ShiftMatrix)
! Returns the allowed shifts for the Space Group
    USE DRUID_HEADER
    
    CHARACTER*5 Shift
    REAL, DIMENSION(3) :: ShiftMatrix

    SELECT CASE (Shift)
      
      CASE('hx')
        ShiftMatrix(1) = 0.5
        ShiftMatrix(2) = 0.0
        ShiftMatrix(3) = 0.0
      CASE('hy')
        ShiftMatrix(1) = 0.0
        ShiftMatrix(2) = 0.5
        ShiftMatrix(3) = 0.0
      CASE('hz')
        ShiftMatrix(1) = 0.0
        ShiftMatrix(2) = 0.0
        ShiftMatrix(3) = 0.5
      CASE('qqq')
        ShiftMatrix(1) = 0.25
        ShiftMatrix(2) = 0.25
        ShiftMatrix(3) = 0.25
      CASE ('hxhy')
        ShiftMatrix(1) = 0.5
        ShiftMatrix(2) = 0.5
        ShiftMatrix(3) = 0.0
      CASE ('hxqz')
        ShiftMatrix(1) = 0.5
        ShiftMatrix(2) = 0.0
        ShiftMatrix(3) = 0.25
      CASE ('txty')
        ShiftMatrix(1) = 0.66
        ShiftMatrix(2) = 0.33
        ShiftMatrix(3) = 0.0
      CASE ('hhh')
        ShiftMatrix(1) = 0.5
        ShiftMatrix(2) = 0.5
        ShiftMatrix(3) = 0.5

    END SELECT
    RETURN

    END SUBROUTINE GetShift
!********************************************************************************************
    SUBROUTINE ZeroInfiniteTransAxes(Shift, CentreOfMass, Transflag, InfiniteAxes)
! An axis allows infinite translations so sets the centre of mass coordinate to zero and
! records which axis infinite translations apllicable to. 
    USE DRUID_HEADER

    REAL, DIMENSION(3) :: CentreOfMass
    CHARACTER*5 Shift
    INTEGER, DIMENSION(3) :: Infiniteaxes
    INTEGER Transflag

    Transflag = 0

    SELECT CASE (Shift)
      CASE ('t')
          CentreOfMass(3) = 0.000
          InfiniteAxes(3) = 1
      CASE ('s')
          CentreOfMass(2) = 0.000
          InfiniteAxes(2) = 1        
      CASE ('r')
          CentreOfMass(1) = 0.000
          InfiniteAxes(1) = 1
    END SELECT
    
    Transflag = 1
    RETURN
    END SUBROUTINE ZeroInfiniteTransAxes
!*********************************************************************************************
!
!  CoMCell clumsily renormalises fractional coordinates to be within a single cell
!
!*********************************************************************************************
    SUBROUTINE CoMCell(CoMSGMatrix, icount, npdbops)
    USE DRUID_HEADER

    REAL, DIMENSION (50,400,3) ::CoMSGMatrix
    INTEGER icount
    INTEGER, INTENT(IN) :: npdbops
      

    DO i = 1,icount
     DO k = 1, npdbops
      DO j = 1, 3
      IF ((CoMSGMAtrix(i,k,j).lt.0.0).and.(CoMSGMAtrix(i,k,j).ge.(-1.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) + 1.0
      END IF
      IF ((CoMSGMAtrix(i,k,j).lt.(-1.0)).and.(CoMSGMAtrix(i,k,j).ge.(-2.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) + 2.0
      END IF
      IF ((CoMSGMAtrix(i,k,j).lt.(-2.0)).and.(CoMSGMAtrix(i,k,j).ge.(-3.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) + 3.0
      END IF
      IF ((CoMSGMAtrix(i,k,j).lt.(-3.0)).and.(CoMSGMAtrix(i,k,j).ge.(-4.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) + 4.0
      END IF
      IF ((CoMSGMAtrix(i,k,j).ge.(1.0)).and.(CoMSGMAtrix(i,k,j).lt.(2.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) - 1.0
      END IF
      IF ((CoMSGMAtrix(i,k,j).ge.(2.0)).and.(CoMSGMAtrix(i,k,j).lt.(3.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) - 2.0
      END IF
      IF ((CoMSGMAtrix(i,k,j).ge.(3.0)).and.(CoMSGMAtrix(i,k,j).lt.(4.00))) THEN
           CoMSGMAtrix(i,k,j) = CoMSGMAtrix(i,k,j) - 3.0
      END IF
      END DO
     END DO
    END DO

    END SUBROUTINE CoMCell
!********************************************************************************************
!
! MakeMol applies "best" symmetry operation to original molecule and returns to Align the
! coordinates of the new molecule with respect to its centre of mass
!
!********************************************************************************************
      SUBROUTINE MakeMol(XATOPT, isymopbest, ishiftbest, NATOM, Connarray, icount, inversion)
      USE DRUID_HEADER

      INTEGER     MaxNumAtom
      PARAMETER  (MaxNumAtom = 150)
      INTEGER     NATOM

      REAL        XATOPT, FinalMol, ConnArray
      DIMENSION   XATOPT(3,MaxNumAtom),FinalMol(3,MaxNumAtom),ConnArray(3,MaxNumAtom)
      REAL        CoMFM
      DIMENSION   CoMFM(3)
      REAL        SONumber
      DIMENSION   SONumber(50,3)
      CHARACTER*1 SOSign
      DIMENSION   SOSign(50,3)
      INTEGER     SOAxis
      DIMENSION   SOAXIS(50,3)
      COMMON /symops/ SOSign, SONumber, SOAxis
    

      DO k = 1,NATOM
        DO j = 1,3
          IF (SOsign((isymopbest),j).eq.'+') THEN
            FinalMol(j,k) = SONumber((isymopbest),j) + XATOPT((SOAxis((isymopbest),j)),k)
          END IF 
          IF (SoSign((isymopbest),j).eq.'-') THEN
            FinalMol(j,k) = SONumber((isymopbest),j) - XATOPT((SOAxis((isymopbest),j)),k)
          END IF
          IF (SoSign((isymopbest),j).eq.'*') THEN
            FinalMol(j,k) = SONumber((isymopbest),j) * XATOPT((SOAxis((isymopbest),j)),k)
          END IF   
        END DO
       END DO

      IF (Inversion.eq.1) THEN
!      IF ((ishiftbest.gt.(icount/2)).and.(isymopbest.ne.1)) THEN
       IF (ishiftbest.gt.(icount/2)) THEN
         Do k = 1, NATOM
         DO j = 1,3
          FinalMol(j,k) = FinalMol(j,k) * (-1)
          END DO
          END DO
          END IF 
          END IF


        Sumx = 0.0
        sumy = 0.0
        sumz = 0.0

      DO i = 1,NATOM
        sumx = sumx + FinalMol(1,i)
        sumy = sumy + FinalMol(2,i)
        sumz = sumz + FinalMol(3,i)
      END DO
! If a shift has been applied by the symmetry operators to an axis of infinite translation
! it should be taken care of by the calculation of the molecule's centre of mass     
      CoMFM(1) = sumx / FLOAT(natom)
      CoMFM(2) = sumy / FLOAT(natom)
      CoMFM(3) = sumz / FLOAT(natom)
! Passing back to Align a matrix which describes the molecule with respect to a centre of mass
      DO i = 1,NATOM
          Connarray(1,i) = FinalMol(1,i) - ComFM(1)
          Connarray(2,i) = FinalMol(2,i) - ComFM(2)
          Connarray(3,i) = FinalMol(3,i) - ComFM(3)
      END DO

     END SUBROUTINE MakeMol 
!******************************************************************************************
