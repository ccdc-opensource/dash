!
!*****************************************************************************
!
! Issues:
!
! We're probably going to include high angle data as well for the Rietveld Refinement,
! which gives problems with the following items:
!
! - Background, both Monte Carlo and Polynomial
! - Peak shape
! - Cell parameters / zero point
! - Maximum number of reflections ("350")?
!
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

      INTEGER iCol, iRow

      iCol = 1
      iRow = 1
      CALL WDialogSelect(IDD_RRsubZmatrices)
      CALL WGridPutCellString(IDF_RR_ZmatrixGrid,1,iRow,'x(frag1)')
      CALL WGridPutCellReal(IDF_RR_ZmatrixGrid,2,iRow,0.5)
      CALL WDialogSelect(IDD_RRsubBonds)
      CALL WGridPutCellString(IDF_RR_BondGrid,1,iRow,'C1:C2')
      CALL WGridPutCellReal(IDF_RR_BondGrid,2,iRow,1.54)
      CALL WDialogSelect(IDD_RRsubAngles)
      CALL WGridPutCellString(IDF_RR_AngleGrid,1,iRow,'C1:C2:C3')
      CALL WGridPutCellReal(IDF_RR_AngleGrid,2,iRow,112.0)
      CALL WDialogSelect(IDD_RRsubTorsions)
      CALL WGridPutCellString(IDF_RR_TorsionGrid,1,iRow,'C1:C2:C3:C4')
      CALL WGridPutCellReal(IDF_RR_TorsionGrid,2,iRow,180.0)
      CALL WDialogSelect(IDD_RRsubAtoms)
      CALL WGridPutCellString(IDF_RR_AtomGrid,1,iRow,'C1')
      CALL WGridPutCellReal(IDF_RR_AtomGrid,2,iRow,3.0)
      CALL WGridPutCellReal(IDF_RR_AtomGrid,4,iRow,1.0)
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

            CASE (IDB_Undo)

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
