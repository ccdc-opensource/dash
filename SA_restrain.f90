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
      ! frag1, atom1, frag2, atom2, dist, width, weight, spring_flag (0=no,1=yes)
      READ(line, *, END=200, ERR=200) (frags(I), atms(I), I=1, 2), &
                                      dist, width, rweight, sflag
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
      DRestrSpringOpts(DRestrNumb) = sflag
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
      INTEGER I
      
      DO I = 1, DRestrNumb
        CALL CalculateDistance(DRestrAtomIDs(1,I), DRestrAtomIDs(2,I), D)
        delta = ABS(D - DRestrLens(I))
        IF (delta .GT. DRestrWidths(I)) THEN
          IF (DRestrSpringOpts(I) .NE. 0) THEN
            SASpringPenalty = SASpringPenalty + DRestrWeights(I) * delta
          ELSE
            SANonSpringPenalty = SANonSpringPenalty + DRestrWeights(I) * delta
          ENDIF
        ENDIF
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
      SUBROUTINE CalculateDistance( atomID1, atomID2, D )

      USE ZMVAR
      USE ATMVAR
      
      IMPLICIT NONE
      
      INTEGER,    INTENT (IN   ) :: atomID1, atomID2
      REAL,       INTENT (  OUT) :: D
   
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
      
      ! frac -> cart
      CALL PremultiplyVectorByMatrix(f2cmat, XATO(1,OrderedAtm(atomID1)), v1)
      CALL PremultiplyVectorByMatrix(f2cmat, XATO(1,OrderedAtm(atomID2)), v2)
      D = 0.0
      DO I = 1, 3
        tmp = v1(I) - v2(I)
        D = D + tmp * tmp
      END DO
      D = SQRT(D)
      RETURN
   
      END SUBROUTINE CalculateDistance
!
!*****************************************************************************
!
