!
!*****************************************************************************
!
      LOGICAL FUNCTION ParseDistribution(Line, Item)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      CHARACTER*(*), INTENT (IN   ) :: Line
      INTEGER,       INTENT (IN   ) :: Item

      INTEGER NumMogulBins(MVAR), MogulBins(MaxMogulBin, MVAR)
      REAL MogulDistributions(-180:180, MVAR)
      COMMON /MDB/ NumMogulBins, MogulBins, MogulDistributions

      LOGICAL, External :: InterpolateBins
      INTEGER N

      ParseDistribution = .FALSE.
      NumMogulBins(Item) = 0
      READ(Line, *, END=200, ERR=200) N
      IF (N .LE. 0 .OR. N .GT. MaxMogulBin) GOTO 200
      READ(Line, *, END=200, ERR=200) N, MogulBins(1:N, Item)
      NumMogulBins(Item) = N
      ParseDistribution = InterpolateBins(MogulDistributions(-180, Item), MogulBins(1, Item), N)
 200  RETURN
 
      END FUNCTION ParseDistribution
!
!*****************************************************************************
!
      LOGICAL FUNCTION ProcessDistribution(nBin, iBins, Item)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER, INTENT (IN   ) :: nBin, iBins(*), Item

      INTEGER NumMogulBins(MVAR), MogulBins(MaxMogulBin, MVAR)
      REAL MogulDistributions(-180:180, MVAR)
      COMMON /MDB/ NumMogulBins, MogulBins, MogulDistributions

      LOGICAL, External :: InterpolateBins

      ProcessDistribution = .FALSE.
      NumMogulBins(Item) = 0
      IF (nBin .LE. 0 .OR. nBin .GT. MaxMogulBin) GOTO 200
      NumMogulBins(Item) = nBin
      MogulBins(1:nBin, Item) = iBins(1:nBin)
      ProcessDistribution = InterpolateBins(MogulDistributions(-180,Item), iBins, nBin)
 200  RETURN
 
      END FUNCTION ProcessDistribution
!
!*****************************************************************************
!
      LOGICAL FUNCTION InterpolateBins(Prof, Bins, nBin)
! Make one degree per step profile by linear interpolation  between bins-points

      IMPLICIT NONE

      REAL,     INTENT (  OUT) :: Prof(-180:180)
      INTEGER,  INTENT (IN   ) :: Bins(*), nBin

      REAL, PARAMETER :: cYShift = 0.005
      INTEGER I, J, nBinWidth, iOffSet
      REAL    Y, YMax

      InterpolateBins = .FALSE.

! The implementation expects nBinWidth is an integer
      IF (MOD(180, nBin) .NE. 0) RETURN
      nBinWidth = 180 / nBin
      iOffSet = nBinWidth / 2 + mod(nBinWidth, 2) ! shift to bin-centre
      YMax = MAXVAL(Bins(1:nBin))
! Fill both ends
      DO I = 0, iOffSet
        Prof(I)       = FLOAT(Bins(1)) / YMax + cYShift
        Prof(180 - I) = FLOAT(Bins(nBin)) / YMax + cYShift
      enddo
      DO I = 1, nBin-1
        Y = FLOAT(Bins(I + 1) - Bins(I)) / FLOAT(nBinWidth)
        DO J = 0, nBinWidth - 1
          Prof((I - 1) * nBinWidth + J + iOffSet) = (FLOAT(Bins(i)) + J * Y) / YMax + cYShift
        ENDDO
      ENDDO
! Copy to other side
      DO I = 1, 180
        Prof(-I) = Prof(I)
      ENDDO

      InterpolateBins = .TRUE.
      RETURN
 
      END FUNCTION InterpolateBins
!
!*****************************************************************************
!
      REAL FUNCTION GetMDBRandomTorsion(RandValue, X, StepSize, Item)
! RandValue MUST be within [0,1]

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL,     INTENT (IN   ) :: RandValue, X, StepSize
      INTEGER,  INTENT (IN   ) :: Item

      INTEGER NumMogulBins(MVAR), MogulBins(MaxMogulBin, MVAR)
      REAL MogulDistributions(-180:180, MVAR)
      COMMON /MDB/ NumMogulBins, MogulBins, MogulDistributions

      INTEGER idx, I, J, iX, iXX
      REAL    Y, ovlp(-180:180) !, integrate(-180:180)

      REAL,    EXTERNAL  :: MaxwellValue
      INTEGER, PARAMETER :: ciArrSize = 200
      REAL,    PARAMETER :: cXToIdx = ciArrSize / 10.0
      REAL,    SAVE :: MaxwellArr(0:ciArrSize)
      LOGICAL, SAVE :: MaxwellArrInitial
      DATA    MaxwellArrInitial/.FALSE./

! Optimise: save the maxwell values between [0, 10] into an array
      IF (.NOT. MaxwellArrInitial) THEN
        DO I = 0, ciArrSize
          MaxwellArr(I) = MaxwellValue(FLOAT(I) / cXToIdx)
        END DO
        MaxwellArrInitial = .TRUE.
      ENDIF

      ! Assume: Torsion always in (-180,180], ie. -180 excluded
      iX = INT(X + SIGN(0.5, X))
      IF (iX .EQ. -180) iX = 180
! Overlap two Maxwell distributions at each side of origin X
      ovlp(iX) = MogulDistributions(iX, Item) * MaxwellArr(0)
      DO I = 1, 180
!        XX = FLOAT(I) / StepSize
!        Y = scale * XX*XX/2.0*EXP(-XX)
        iXX = INT(FLOAT(I)*cXToIdx / StepSize + 0.5)
        IF (iXX .GT. ciArrSize) iXX = ciArrSize
        Y = MaxwellArr(iXX)
        idx = I + iX
        IF (idx .GT. 180) idx = idx - 360
        ovlp(idx) = MogulDistributions(idx, Item) * Y
        idx = iX - I
        IF (idx .LE. -180) idx = idx + 360
        ovlp(idx) = MogulDistributions(idx, Item) * Y
      ENDDO
! Integrate the overlapped function
      ovlp(-180) = 0.0
      DO I = -179, 180
        ovlp(i) = ovlp(i-1) + ovlp(i)
      ENDDO
! Normalise
      ovlp = ovlp / ovlp(180)
! Use bisection to find the inverse distribution
      idx = -179
      DO j = 179, -179, -1
        IF (RandValue .GT. ovlp(j)) THEN
          idx = j + 1
          EXIT
        ENDIF
      ENDDO
      GetMDBRandomTorsion = FLOAT(idx) - SIGN(0.5, FLOAT(idx))
      RETURN
 
      END FUNCTION GetMDBRandomTorsion
!
!*****************************************************************************
!
      REAL FUNCTION MaxwellValue(X)

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: X

      MaxwellValue = 0.5 * X*X*EXP(-X) ! Maxwell-like distribution

      END FUNCTION MaxwellValue
!
!*****************************************************************************
!
