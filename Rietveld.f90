!
!*****************************************************************************
!
      SUBROUTINE ShowWindowRietveld
!
! The window containing the Rietveld Refinement needs a lot of initialisation,
! so here is a special routine to open that window.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE 

      CALL WDialogSelect(IDD_Rietveld)
      CALL WDialogShow(-1,-1,0,ModeLess)

      END SUBROUTINE ShowWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE DealWithWindowRietveld

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE      

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Refine)



            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide()
            CASE (IDBSAVE)



            CASE (IDB_View)



          END SELECT
        CASE (FieldChanged)



      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWindowRietveld
!
!*****************************************************************************
!
