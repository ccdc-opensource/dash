! JCC Include declarations here for REFLNS
! rather than hard-coded ... you know it makes sense

! JCC GGCALC dimension increased to 500

      REAL              AIOBS,         AICALC
      COMMON /SAREFLNS/ AIOBS(MFCSTO), AICALC(MFCSTO)

      REAL            rHKL,           AMUL
      REAL            ESDOBS,         SOMEGA,       GGCALC
      INTEGER         MAXKK, KMIN, KMAX, KMOD, KNOW
      INTEGER                                           ISMAG
      REAL            DKDDS
      INTEGER                KOM23
      COMMON /REFLNS/ rHKL(3,MFCSTO), AMUL(MFCSTO),                                   &
                      ESDOBS(MFCSTO), SOMEGA(MFCSTO), GGCALC(500),                    &
                      MAXKK(9), KMIN, KMAX, KMOD, KNOW, ISMAG(MFCSTO), &
                      DKDDS, KOM23

      INTEGER         MAXK

      EQUIVALENCE (MAXK,MAXKK(1))
