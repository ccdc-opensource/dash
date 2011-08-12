!
!*****************************************************************************
!
      SUBROUTINE InitSADistRestraint

      IMPLICIT NONE      

      INCLUDE 'SA_restrain.inc'

      DRestrNumb = 0

      END SUBROUTINE InitSADistRestraint
!
!*****************************************************************************
!
      LOGICAL FUNCTION ParseSADistRestraint( line, iFrg )

      USE ZMVAR

      IMPLICIT NONE
      
      CHARACTER*(*), INTENT (IN   ) :: line
      INTEGER,       INTENT (IN   ) :: iFrg

      INCLUDE 'SA_restrain.inc'

      INTEGER I, atms(2), frags(2), sflag
      REAL width, rweight, dist
      
      ParseSADistRestraint = .FALSE.
      IF (DRestrNumb .GE. MaxSADRestr) GOTO 200
      ! frag1, atom1, frag2, atom2, dist, width, weight
      READ(line, *, END=200, ERR=200) (frags(I), atms(I), I=1, 2), &
                                      dist, width, rweight
      ! Check bounds
      IF (rweight .LT. 0.0 .OR. dist .LT. 0.0 .OR. width .LT. 0.0) GOTO 200
      DO I = 1, 2
        IF ( frags(I) .LE. 0 .OR. frags(I) .GT.  iFrg .OR. &
             atms(I) .LE. 0 .OR. atms(I) .GT. nAtoms(frags(I)) ) GOTO 200
      END DO
      IF ( frags(1) .EQ. frags(2) ) &
        CALL CheckZMBondAtoms(atms, frags(1))
      IF (sflag .NE. 0) sflag = 1
      ! Update global varibles
      DRestrNumb = DRestrNumb + 1
      CALL Frag2AtomID( DRestrAtomIDs(1, DRestrNumb), frags(1),  atms(1))
      CALL Frag2AtomID( DRestrAtomIDs(2, DRestrNumb), frags(2),  atms(2))
      DRestrWeights(DRestrNumb) = rweight
      DRestrLens(DRestrNumb) = dist
      DRestrWidths(DRestrNumb) = width
      ParseSADistRestraint = .TRUE.
  200 CONTINUE
      RETURN
   
      END FUNCTION ParseSADistRestraint
!
!*****************************************************************************
!
      SUBROUTINE CheckZMBondAtoms( AtmSeqs, iFrgID )

      USE ZMVAR
 
      IMPLICIT NONE
 
      INTEGER,       INTENT (IN   ) :: AtmSeqs(*), iFrgID
      
      INTEGER I
      
      DO I = 1, natoms(iFrgID)
        IF ( ( AtmSeqs(1) .EQ. I .AND. AtmSeqs(2) .EQ. iz1(I, iFrgID) ) .OR. &
             ( AtmSeqs(2) .EQ. I .AND. AtmSeqs(1) .EQ. iz1(I, iFrgID) ) ) THEN
          CALL WarningMessage('Bond between atoms ' &
                              //OriginalLabel(AtmSeqs(1), iFrgID)//' ' &
                              //OriginalLabel(AtmSeqs(2), iFrgID)// &
                              ' aready exists in the zmatrix.')
          EXIT
        ENDIF
      ENDDO
      RETURN
   
      END SUBROUTINE CheckZMBondAtoms
!
!*****************************************************************************
!
! Map atomSeq of the fragID to the global atomID
      SUBROUTINE Frag2AtomID( atomID, fragID, atomSeq )

      USE ZMVAR

      IMPLICIT NONE
      
      INTEGER, INTENT (IN   ) :: fragID, atomSeq
      INTEGER, INTENT (  OUT) :: atomID

      INTEGER I

      atomID = atomSeq
      I = fragID - 1
      DO WHILE (I .GT. 0)
        atomID = atomID + NATOMS(I)
        I = I - 1
      END DO
   
      END SUBROUTINE Frag2AtomID
!
!*****************************************************************************
!
! Map the global atomID to atomSeq and fragID
      SUBROUTINE AtomID2Frag( atomID, fragID, atomSeq )

      USE ZMVAR

      IMPLICIT NONE
      
      INTEGER, INTENT (IN   ) :: atomID
      INTEGER, INTENT (  OUT) :: fragID, atomSeq
      
      atomSeq = atomID
      fragID = 1
      DO WHILE (fragID .LT. nFrag .AND. atomSeq .GT. NATOMS(fragID) )
        atomSeq = atomSeq - NATOMS(fragID)
        fragID = fragID + 1
      END DO
   
      END SUBROUTINE AtomID2Frag
!
!*****************************************************************************
!
      SUBROUTINE AddPenalty()

      IMPLICIT NONE
      
      INCLUDE 'SA_restrain.inc'
      
      REAL delta, D
      INTEGER I,ID1,ID2,IFRG1,IFRG2,IAT1,IAT2
      REAL, EXTERNAL :: CalculateMinContribution
      
      
      SAPenalty = 0.0
      DO I = 1, DRestrNumb
        ID1 = DRestrAtomIDs(1,I)
        ID2 = DRestrAtomIDs(2,I)
        CALL AtomID2Frag(ID1,IFRG1,IAT1)
        CALL AtomID2Frag(ID2,IFRG2,IAT2)        
        D =  CalculateMinContribution(ID1,ID2,IFRG1 .NE. IFRG2,  DRestrLens(I) )
        delta = MAX(0.0, D - DRestrWidths(I)) ! Only penalise if D > width
        SAPenalty = SAPenalty + DRestrWeights(I) * delta * delta
      ENDDO
      
      
      RETURN
   
      END SUBROUTINE AddPenalty
!
!*****************************************************************************
!
      SUBROUTINE SetSpringWeight( f )

      IMPLICIT NONE
      
      REAL,    INTENT (IN   ) :: f
   
      INCLUDE 'SA_restrain.inc'
   
!      SpringWeight = EXP( 2.0 * f ) - 1.0
      SpringWeight = f
      RETURN
   
      END SUBROUTINE SetSpringWeight
!
!*****************************************************************************
!
! Calculate the distance between atomID1 of fragID1 and atomID2 of fragID2
! Note: no consideration about distance involving symmetry related atoms.
      REAL FUNCTION CalculateMinContribution( atomID1, atomID2, expandBySymm, IdealLength )

      USE ZMVAR
      USE ATMVAR
      IMPLICIT NONE
      
      INTEGER NOP,NCENT,NOPC,NLAT,NGEN,KOM13
      COMMON /NSYM  / NOP, NCENT, NOPC, NLAT, NGEN, CENTRC, KOM13
      LOGICAL CENTRC   
         
      REAL SYM,TRANS,ALAT,ORIGIN
      INTEGER KOM26
      COMMON /SYMDA / SYM(3,3,24), TRANS(3,24), ALAT(3,4), ORIGIN(3), KOM26

      INTEGER,    INTENT (IN   ) :: atomID1, atomID2,expandBySymm
      REAL D
      REAL, INTENT(IN) :: IdealLength
   
      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER         NATOM
      REAL                   XATO
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, XATO(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
     &                KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
     &                SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      INTEGER I
      REAL v1(3), v2(3), tmp
      
      REAL X1,X2,X3,C,D2
      DIMENSION X1(3), X2(3), X3(3), C(3)
      
      INTEGER IC,IS,IL,NCELX,NCELY,NCELZ
      
      ! frac -> cart

      CALL PremultiplyVectorByMatrix(f2cmat, XATO(1,OrderedAtm(atomID2)), v2)


      
      IF ( expandBySymm ) THEN
         D = 1e9
         DO IC = 1, NCENT
    ! CYCLE OVER OPERATORS WITHOUT CENTRE:
            DO IS = 1, NOPC
               CALL ROTSYM( XATO(1,OrderedAtm(atomID1)),X1(1),IS, 1)
               CALL GMADD(X1(1),TRANS(1,IS),X1(1),1,3)
               IF (IC.EQ.2) CALL GMREV(X1,X1,1,3)            
    ! CYCLE OVER LATTICE TRANSLATIONS:
               DO IL = 1, NLAT
                  CALL GMADD(X1(1),ALAT(1,IL),X2(1),1,3)
                  DO NCELX = 1,5
                     C(1)= FLOAT(NCELX-3)
                     DO NCELY = 1,5
                        C(2) = FLOAT(NCELY-3)
                        DO NCELZ = 1,5      
                           C(3) = FLOAT(NCELZ-3)
                           CALL GMADD(X2(1),C(1),X3(1),1,3)
                           CALL PremultiplyVectorByMatrix(f2cmat, X3, v1)      

                           DO I = 1, 3
                              tmp = v1(I) - v2(I)
                              D2 = D2 + tmp * tmp
                           END DO
                           D2 = ABS(SQRT(D2) - IdealLength)
                           IF ( D2 .LT. D ) THEN
                              D = D2
                           ENDIF        
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ELSE
        CALL PremultiplyVectorByMatrix(f2cmat, XATO(1,OrderedAtm(atomID1)), v1)
        DO I = 1, 3
            tmp = v1(I) - v2(I)
            D = D + tmp * tmp
         END DO
        D = ABS( SQRT(D) - IdealLength )      
      ENDIF
      
      CalculateMinContribution = D
      RETURN
   
      END FUNCTION CalculateMinContribution
!
!*****************************************************************************
!
