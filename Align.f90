!********************************************************************************
! Attempts to write out solutions from DASH such that they are superimposable.
! Doesn't treat Sohnke groups any differently from any other space group and
! therefore sometimes get "unaligned" solutions.  Not everything behaves well 
! all the time, for example Pca21 has caused problems.
! NOV2001 Added nasty fix which will help with cases like Pca21
! JAN2002 Now use array rpdb for symmetry operations (instead of SOAxis, SOnumber etc.
!         All symmetry operations decoded correctly (for example x-y)
!         Added fix to align space groups with two infinite translation axes, Cc
! MAR2002 Trigonal and Hexagonal Groups handled better.  Further Generators (Section
!         Int.Tables A) have been added and special cases where x and y axes are swapped
!         without inverting z are taken care of.
!         Three equivalent axes (i.e. cubic groups) are not taken account of explicitly 
!         but cubic groups (Pa3 at least) shown to align
! TODO R and H settings in some trigonal groups?
!
!*****************************************************************************
!

	SUBROUTINE ALIGN()

      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR            ! Number of zmatrices, nfrag
      USE SOLVAR

!      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'lattice.inc' ! Cellpar, space group strings and NumberSGTable

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult
!
      PARAMETER (mpdbops=192)
      COMMON /fullsymmops/ rpdb(4,4,mpdbops)!Symmetry operations
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
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops
!Required to check if x,y, or z have been fixed or bounds changed from defaults

!Can't call first common block member x
      DOUBLE PRECISION xx,lb,ub,vm
      COMMON /values/ xx(mvar),lb(mvar),ub(mvar),vm(mvar)

!
      INTEGER MaxNumAtom !in an include somewhere??
      PARAMETER (MaxNumAtom = 150)

! Local Variables
      CHARACTER(len = 255) :: line
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
      INTEGER, DIMENSION(3)  :: FixedAxes
      INTEGER       NumOfTimesToApplyShift
      REAL          snum
      REAL, DIMENSION (20,200,3) ::CoMSGMatrix     
      INTEGER       icount
      INTEGER       INumOfShifts, IinfiniteAxes
      REAL, DIMENSION(3)     :: TempShiftMatrix
      REAL          TDistance
      REAL, DIMENSION(3)     :: Centre
      REAL          Tempx, Tempy
      INTEGER       ISymOpBest
      INTEGER       IShiftBest
      INTEGER i,j,k
      REAL, DIMENSION(3,MaxNumAtom) ::FinalMol
      REAL, DIMENSION(3,MaxNumAtom) ::ConnArray
! arrays which store results for multiple origins
      REAL, DIMENSION(3) :: Obestdistance
      INTEGER, DIMENSION(3) :: Oisymopbest
      INTEGER, DIMENSION(3) :: Oishiftbest
      REAL, DIMENSION(3,3) :: Ocentre
      REAL, DIMENSION (3,4)   ::Origin
      INTEGER bestorigin(1)
      INTEGER NumberofOrigins
      INTEGER ActualOrigin
      INTEGER Odd

      
      EXTERNAL ErrorMessage

!        
!--------- Check to see if align algorithm should be applied-----------
! For now, if the space group is trigonal, hexagonal or cubic, does not attempt alignment.  
     
!!      IF (NumberSGTable.GE.430) THEN
!!        RETURN
!!      END IF       
! If number of Z-matrices greater than 1, do not align
      IF (nfrag.GT.1) THEN
        RETURN
      END IF       
! If number of Z-matrices greater than 1, do not align
      IF (TotNumZMatrices.GT.1) RETURN

! Check if any x,y,z coords have been fixed or upper and lower bounds changed from defaults.
! If they have then alignment not carried out.  If user fixed an axis which is an infinite
! axis, alignment will be applied
        DO j = 1,3
          IF ((ub(j).EQ.DBLE(0.5 + 1.0D-5)) .AND. (lb(j).EQ.DBLE(0.5 - 1.0D-5))) THEN
            FixedAxes(j) = 1 !user fixed an axis but may be infinite axis
          ELSEIF ((lb(j).NE.0.0000).OR.(ub(j).NE.1.000)) THEN
            RETURN
          END IF
        END DO
!--------- End of Checks ----------

      OPEN(220,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'SpaceGroupSymbols.dat',STATUS='OLD', ERR = 10)
      DO j = 1,(NumberSGTable-1)
        READ (220, 50) nlin, line
50      FORMAT(q,a)
      ENDDO
      READ(220,55) Inversion, NumOfShifts, ShiftString
!55    FORMAT(28x, I1, 4x, I1, x, A40)
55    FORMAT(75x, I1, 4x, I1, x, A20)
        DO j = 1,NumofShifts
          Shift(j) = ShiftString(((j*5)-4):(j*5))
        ENDDO 
      CLOSE(220)

! Clear Sumx, sumy and sumz
      Sumx = 0.0
      Sumy = 0.0
      Sumz = 0.0
! Clear TempShiftMatrix
      DO J = 1,3
        TempShiftMAtrix(j) = 0.0
      END DO

! Determine if there are two equivalent axes i.e. is space group tetragonal,
! trigonal or hexagonal?  Cubic groups and hence three equivalent axes are 
! not handled yet

      IF ((NumberSGTable.ge.349).and.(NumberSGTable.le.488)) THEN
       EquivAxes = 2
      END IF

! Calculate centre of mass for input molecule
      DO i = 1,natom
        sumx = sumx + XAtmCoords(1,i,Curr_SA_Run)
        sumy = sumy + XAtmCoords(2,i,Curr_SA_Run)
        sumz = sumz + XAtmCoords(3,i,Curr_SA_Run)
      END DO
     
      CentreOfMass(1) = sumx / FLOAT(NATOM)
      CentreOfMass(2) = sumy / FLOAT(NATOM)
      CentreOfMass(3) = sumz / FLOAT(NATOM)

! If Space Group like P1 where only inversion allowed then set Centre of Mass to (1/2,1/2,1/2)
      IF((Inversion.eq.1).and.(NumOfShifts.eq.0)) THEN
        DO j = 1,3
          CentreOfMass(j) = 0.50
        END DO
      END IF
! Looks to see if there are further generators (Table 15, IT VolA) needed to explore 
!  all the correct space. 
      CALL SpecialOriginRequirements(NumberSGTable, CentreofMass, Origin, NumberofOrigins)
!
! Start to create matrix which contains possible positions for the centre of mass.  This loop
! will look at translations allowed for the space group and apply them to the centre(s)
! of mass.  Shifthandler identifies whether the shift is a translation, in which case it returns
! the value of the translation.  If the axes allows infinite translations the centre of mass
! is set to 0.5 and no shift value is returned.  The infinite axis is flagged in the matrix
! InfiniteAxes
!

      DO IZ = 1, NumberOfOrigins
        
        DO j = 1,3
          CentreofMass(j) = Origin(IZ, j)
        END DO
     
!       First entry into CoMMatrix is the original centre of mass
        DO j = 1,3
          CoMMatrix(1,j) = CentreofMass(j)
        END DO

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

!       Check to see if user fixed infinite translation axes 
        DO j = 1,3
          IF(InfiniteAxes(j) .EQ. 1) THEN
            CoMMatrix(1,j) = 0.5
          ELSEIF((InfiniteAxes(j) .EQ. 0).AND.(FixedAxes(j) .EQ. 1)) THEN 
            RETURN !user fixed an axis which is not an infinite axis so do not align
          END IF
        END DO



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

! Have to perform space group operations on each entry of CoMMatrix to make sure all origins 
! generated:
! CoMSGMatrix: 3 dimensional matrix (i,k,j).  i describes number of CoM's generated
! from shifts, k is number of symmetry ops and j is x,y,or z
!
! rpdb description: rpdb(a,b,c) 
! a = 1-4.  When a = 1 this describes the x component of the 
! symmetry operation, a=2 is the y component of the symop etc.  Not sure what the
! 4th column is for. (4,4,_) usually = 1 though.
! b = 1-4.  Contains the symmetry operation.  (1,0,0,1/2) means x + 1/2, (1, -1, 0, 0) 
! means x - y 
! c 1-192.  This is the number of symmetry operations for the space group. First entry
! is always x,y,z
     IF (icount.gt.20) THEN
       CALL ErrorMessage(' Array bounds exceeded during Alignment ')
       RETURN
     END IF
       DO i = 1, icount
         DO k = 1, npdbops
           DO j = 1,3
             IF (infiniteaxes(j).NE.1) THEN
               CoMSGMatrix(i,k,j) = rpdb(j,4,k) + (rpdb(j,1,k)*CoMMatrix(i,1) + rpdb(j,2,k)*CoMMatrix(i,2) + rpdb(j,3,k)*CoMMatrix(i,3))
             ELSE 
               CoMSGMatrix(i,k,j) = CoMMatrix(i,j)
             END IF
           END DO
         END DO
       END DO       
! If inversion allowed by space group shifts, apply it to all centres of mass in ComSGmatrix and add
! to CoMSGMatrix         
       IF (Inversion.eq.1) THEN
         IF (icount.gt. 10) THEN
           CALL ErrorMessage(' Array bounds exceeded during Alignment ')
           RETURN
          END IF           
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
       obestdistance(IZ) = 100
          DO i = 1, icount
            DO k = 1,npdbops
              tdistance=SQRT((CoMSGMatrix(i,k,1)*CoMSGMatrix(i,k,1))+(CoMSGMatrix(i,k,2)*CoMSGMatrix(i,k,2))+(CoMSGMatrix(i,k,3)*CoMSGMatrix(i,k,3)))
              IF (tdistance.LT.obestdistance(IZ)) THEN
                ocentre(IZ,1) = CoMSGMatrix(i,k,1)
                ocentre(IZ,2) = CoMSGMatrix(i,k,2)
                ocentre(IZ,3) = CoMSGMatrix(i,k,3)
                obestdistance(IZ) = tdistance
                oisymopbest(IZ) = k !store symmetry operation which gave best answer
                oishiftbest(IZ) = i !store shift which gave best answer
              END IF
            END DO
          END DO


     END DO  ! End of loop through origins


            

     bestorigin = MINLOC(obestdistance, MASK=obestdistance.gt.0) !Row of Origin containing correct origin
     ActualOrigin = INT(Origin(BestOrigin(1),4)) ! origin number referred to by CASE statements
         
     DO j = 1,3
       Centre(j) = Ocentre(bestorigin(1), j)
     END DO
     
     isymopbest = oisymopbest(bestorigin(1))
     ishiftbest = oishiftbest(bestorigin(1))


! Apply best symmetry operation to original molecule. Take care of inversion if required
! too. Return with matrix (ConnArray) which describes molecule with respect to its 
! centre of mass

     CALL MakeMol(XAtmCoords(1,1,Curr_SA_Run), isymopbest, ishiftbest, NATOM, Connarray, icount, inversion, ActualOrigin)

! To make final molecule add ConnArray to best centre of mass
     DO i = 1,NATOM
       DO j = 1,3
         FinalMol(j,i) = ConnArray(j,i) + centre(j)
       END DO
     END DO

     CALL OddlybehavedAxisSwaps(NumberSGTable, Odd)

! If have equivalent axes then apply abitrary criterion to see if x and y should be swapped
     IF (equivAxes.eq.2) THEN
       IF (centre(1).gt.centre(2)) THEN
         DO i = 1,NATOM
           tempx = FinalMol(1,i)
           tempy = FinalMol(2,i)
           FinalMol(1,i) = tempy
           FinalMol(2,i) = tempx
             IF ((Inversion .NE. 1).AND. (Odd .NE. 1)) THEN ! If no centre of inversion then
              FinalMol(3,i) = (FinalMol(3,i)*(-1) + 1)      ! should invert z, unless odd case
             END IF
         END DO
       END IF
     END IF

! The following is a nasty, rough and ready fix.  If there is an infinite translation axes
! present the centre of mass coordinate for that axes is set to 0.5.  In some cases an inversion
! can be applied to a molecule and then the axes which are not infinite translation axes reset
! with symmetry operations.  However the infinite translation axes still carries the inversion
! and (0.5)^2 = (-0.5)^2.  So this quick and dirty fix looks at infinite translation axis and
! reflects the molecule upon an arbitrary criterion.  Only applied to cases where there is one
! infinite axes.
     IF((inversion.eq.1).and.(iinfiniteaxes.eq.1)) THEN
       DO j = 1,3
         IF (InfiniteAxes(j).eq.1) THEN
            AtomOnez = FinalMol(j,1)
            Atomtwoz = FinalMol(j,Natom)
              IF (Atomtwoz.LT.Atomonez) THEN
                 DO i = 1,natom
                    FinalMol(j,i) = centre(j) - connarray(j,i)
                 END DO
              END IF
          END IF
       END DO
     END IF
 


! Case where there are two infinite translation axes and inversion centre.
! For example Cc - applying the same fix as above.  Sometimes inverted option
! is ignored because infinite axes = 0.5

        IF((Iinfiniteaxes.eq.2).and.(Inversion.eq.1)) THEN
          IF (infiniteAxes(1).eq.1) THEN
            AtomOnez = FinalMol(1,1)
            Atomtwoz = FinalMol(1,Natom)  
          ELSE
            AtomOnez = FinalMol(2,1)
            Atomtwoz = FinalMol(2,Natom) 
          ENDIF
          IF (Atomtwoz.LT.Atomonez) THEN
            DO j = 1,3
              IF(InfiniteAxes(j).eq.1) THEN
                DO i = 1,natom
                   FinalMol(j,i) = centre(j) - connarray(j,i)
                ENDDO  
              ENDIF
            ENDDO
          ENDIF
        ENDIF              

! FinalMol contains the solution in fractional coordinates.  Write to XAtmCoords. 
      DO J = 1, NATOM
        DO K = 1,3
          XAtmCoords(K,J,Curr_SA_Run) = FinalMol(K,J)
        ENDDO
      ENDDO
      RETURN
   10 CALL ErrorMessage('Sorry, could not find the file'//CHAR(13)// 'SpaceGroupSymbols.dat' &
                        //CHAR(13)// 'in the installation directory')
           
      END SUBROUTINE Align


!********************************************************************************************
!
!The following three subroutines supply the details of the allowed shifts to align
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
! Returns an allowed shift for the Space Group
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
        ShiftMatrix(1) = 0.667
        ShiftMatrix(2) = 0.333
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
! An axis allows infinite translations so sets the centre of mass coordinate to 0.5 and
! records which axis infinite translations applicable to. 
    USE DRUID_HEADER

    REAL, DIMENSION(3) :: CentreOfMass
    CHARACTER*5 Shift
    INTEGER, DIMENSION(3) :: Infiniteaxes
    INTEGER Transflag

    Transflag = 0

    SELECT CASE (Shift)
      CASE ('t')
          CentreOfMass(3) = 0.500
          InfiniteAxes(3) = 1
      CASE ('s')
          CentreOfMass(2) = 0.500
          InfiniteAxes(2) = 1        
      CASE ('r')
          CentreOfMass(1) = 0.500
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

    REAL, DIMENSION (20,200,3) ::CoMSGMatrix
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
      SUBROUTINE MakeMol(XATOPT, isymopbest, ishiftbest, NATOM, Connarray, icount, inversion, Actualorigin)
      USE DRUID_HEADER

      INTEGER     MaxNumAtom
      PARAMETER  (MaxNumAtom = 150)
      INTEGER     NATOM
      INTEGER     ActualOrigin
      REAL        Tempxx
      REAL        Tempyy

      REAL        XATOPT, FinalMol, ConnArray
      DIMENSION   XATOPT(3,MaxNumAtom),FinalMol(3,MaxNumAtom),ConnArray(3,MaxNumAtom)
      REAL        CoMFM
      DIMENSION   CoMFM(3)
!
      PARAMETER (mpdbops=192)
      COMMON /fullsymmops/ rpdb(4,4,mpdbops)

! Generate correct origin by manipulating XATOPT (XATOPT = XAtmCoords)
      SELECT CASE (ActualOrigin)
        CASE(1) ! -x, -y, z
          DO k = 1, NATOM
             XATOPT(1,k) = (-1) * XATOPT(1,k)
             XATOPT(2,k) = (-1) * XATOPT(2,k)
          END DO
        CASE(2)  ! y, x, -z
          DO k = 1, NATOM
            Tempxx = XATOPT(1,k)
            Tempyy = XATOPT(2,k)
            XATOPT(1,k) = Tempyy
            XATOPT(2,k) = Tempxx
            XATOPT(3,k) = (-1) * XATOPT(3,k)
          END DO
        CASE(3) !-y, -x, z
          DO k = 1, NATOM
            Tempxx = XATOPT(1,k)
            Tempyy = XATOPT(2,k)
            XATOPT(1,k) =  (-1) * Tempyy
            XATOPT(2,k) =  (-1) * Tempxx
          END DO
        CASE(4) !y+1/4, x+1/4, z+1/4
          DO k = 1, NATOM
            Tempxx = XATOPT(1,k)
            Tempyy = XATOPT(2,k)
            XATOPT(1,k) = Tempyy - 0.25
            XATOPT(2,k) = Tempxx - 0.25
            XATOPT(3,k) = XATOPT(3,k) - 0.25
          END DO          
      END SELECT   


      DO k = 1,NATOM
        FinalMol(1,k) = rpdb(1,4,isymopbest) + (rpdb(1,1,isymopbest)*XATOPT(1,k) + rpdb(1,2,isymopbest)*XATOPT(2,k) + rpdb(1,3,isymopbest)*XATOPT(3,k))
        FinalMol(2,k) = rpdb(2,4,isymopbest) + (rpdb(2,1,isymopbest)*XATOPT(1,k) + rpdb(2,2,isymopbest)*XATOPT(2,k) + rpdb(2,3,isymopbest)*XATOPT(3,k))
        FinalMol(3,k) = rpdb(3,4,isymopbest) + (rpdb(3,1,isymopbest)*XATOPT(1,k) + rpdb(3,2,isymopbest)*XATOPT(2,k) + rpdb(3,3,isymopbest)*XATOPT(3,k))
      END DO


! If inversion allowed and inversion used, invert coords
      IF (Inversion.eq.1) THEN
       IF (ishiftbest.gt.(icount/2)) THEN
         DO k = 1, NATOM
           DO j = 1,3
             FinalMol(j,k) = FinalMol(j,k) * (-1)
           ENDDO
         ENDDO
       ENDIF 
      ENDIF

      Sumx = 0.0
      sumy = 0.0
      sumz = 0.0
! calculate centre of mass of final molecule
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


     SUBROUTINE SpecialOriginRequirements(NumberSGTable, CentreofMass, Origin, NumberofOrigins)
! Not really origins, rather further generators
     REAL, DIMENSION (3,4) :: Origin
     REAL, DIMENSION (3) :: CentreofMass
     INTEGER NumberSGTable
     INTEGER NumberofOrigins
     INTEGER OriginToApply
     INTEGER OriginIndex

     NumberofOrigins = 0

     DO j = 1,3
       Origin(1,j) = CentreOfMass(j)
     END DO

     NumberofOrigins = 1 


       SELECT CASE (NumberSGTable)
         CASE (431:432)
           OriginIndex = NumberOfOrigins + 1
           OriginToApply = 1
           CALL ApplyOrigins(OriginIndex, OriginToApply, CentreofMass, Origin)
           NumberofOrigins = NumberofOrigins + 1

           OriginIndex = NumberofOrigins +1
           OriginToApply = 2
           CALL ApplyOrigins(OriginIndex, OriginToApply, CentreofMass, Origin)
           NumberofOrigins = NumberofOrigins + 1

         CASE (430, 434, 436:441, 443:446, 449:452)
           OriginIndex = NumberOfOrigins + 1
           OriginToApply = 1
           CALL ApplyOrigins(OriginIndex, OriginToApply, CentreofMass, Origin)
           NumberofOrigins = NumberofOrigins + 1
         CASE (433, 435)
           OriginIndex = NumberOfOrigins + 1
           OriginToApply = 3
           CALL ApplyOrigins(OriginIndex, OriginToApply, CentreofMass, Origin)
           NumberofOrigins = NumberofOrigins + 1
         CASE (492, 493, 502)
           OriginIndex = NumberOfOrigins + 1
           OriginToApply = 4
           CALL ApplyOrigins(OriginIndex, OriginToApply, CentreofMass, Origin)
           NumberOfOrigins = NumberofOrigins + 1           
        END SELECT


    END SUBROUTINE SpecialOriginRequirements

!********************************************************************************************    
    
    SUBROUTINE ApplyOrigins(OriginIndex, OriginToApply, CentreofMass, Origin)
    REAL, DIMENSION (3,4) :: Origin
    REAL, DIMENSION (3) :: CentreofMass  
    INTEGER OriginIndex
    INTEGER OriginToApply
       
       SELECT CASE (OriginToApply)
         CASE(1) ! -x, -y, z
           Origin(OriginIndex,1) = (-1) * CentreofMass(1)
           Origin(OriginIndex,2) = (-1) * CentreofMass(2)
           Origin(OriginIndex,3) = CentreofMass(3)
           Origin(OriginIndex,4) = 1
         CASE(2) !  y, x, -z
           Origin(OriginIndex,1) = CentreofMass(2)
           Origin(OriginIndex,2) = CentreofMass(1)
           Origin(OriginIndex,3) = (-1)*CentreofMass(3)
           Origin(OriginIndex,4) = 2
         CASE(3) ! -y, -x, z
           Origin(OriginIndex,1) = (-1) * CentreofMass(2)
           Origin(OriginIndex,2) = (-1) * CentreofMass(1)
           Origin(OriginIndex,3) = CentreofMass(3)
           Origin(OriginIndex,4) = 3
         CASE(4) ! y+0.25, x+0.25, z+0.25
           Origin(OriginIndex,1) = CentreofMass(2) - 0.25
           Origin(OriginIndex,2) = CentreofMass(1) - 0.25
           Origin(OriginIndex,3) = CentreofMass(3) - 0.25
           Origin(OriginIndex,4) = 4
       END SELECT
    END SUBROUTINE   
    
! *******************************************************************************************
    SUBROUTINE OddlybehavedAxisSwaps(NumberSGTable,Odd)
      INTEGER Odd
      INTEGER NumberSGTable           
      
      Odd = 0
      SELECT CASE (NumberSGTable)
        CASE(357:363, 469, 470, 494:500)
          Odd = 1
      END SELECT
    END SUBROUTINE 
! *********************************************************************************************
