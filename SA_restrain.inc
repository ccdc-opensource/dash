! Include declarations here for SA_Restraints
! rather than hard-coded ... you know it makes sense

      INTEGER, PARAMETER :: MaxSADRestr = 5
!     Max number of bond restraints for SA

      REAL                SpringWeight, SASpringPenalty, SANonSpringPenalty
      COMMON /SA_Penalty/ SpringWeight, SASpringPenalty, SANonSpringPenalty

      INTEGER            DRestrNumb, DRestrAtomIDs(2,MaxSADRestr), DRestrSpringOpts(MaxSADRestr) 
      REAL			         DRestrLens(MaxSADRestr), DRestrWidths(MaxSADRestr), DRestrWeights(MaxSADRestr)
      COMMON /SA_DRestr/ DRestrNumb, DRestrAtomIDs, DRestrLens, DRestrWidths, DRestrWeights, DRestrSpringOpts
