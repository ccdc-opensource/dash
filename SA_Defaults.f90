!
!*****************************************************************************
!
      SUBROUTINE SA_SetOutputFilenamesToDefaults()

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file
      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      LOGICAL outfilset
      COMMON /outfileset/ outfilset
      DATA outfilset / .FALSE. /

      IF (outfilset) RETURN
      cssr_file = 'SA_best.cssr'
      pdb_file  = 'SA_best.pdb'
      ccl_file  = 'SA_best.ccl'
      log_file  = 'SA_best.log'
      pro_file  = 'SA_best.pro'
      cssr_flen = LEN_TRIM(cssr_file)
      pdb_flen  = LEN_TRIM(pdb_file)
      ccl_flen  = LEN_TRIM(ccl_file)
      log_flen  = LEN_TRIM(ccl_file)
      pro_flen  = LEN_TRIM(pro_file)

      END SUBROUTINE SA_SetOutputFilenamesToDefaults
!
!*****************************************************************************
!
! JCC New subroutine to set the output file names
      SUBROUTINE sa_SetOutputFiles(FileHead)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: FileHead 

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file
      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      LOGICAL outfilset
      COMMON /outfileset/ outfilset

      INTEGER POS

! Find the last occurrence of '.'
      POS = LEN_TRIM(FileHead)
      DO WHILE ((POS .GT. 0) .AND. (FileHead(POS:POS) .NE. '.'))
        POS = POS - 1
      END DO
! If no '.' present, pretend there is one after the filename
      IF (POS .EQ. 0) POS = LEN_TRIM(FileHead) + 1
! Now point to the position just before the '.'
      POS = POS - 1
! We will append '.cssr', which is five characters, and after that the total length shouldn't exceed 80
      IF (POS .GT. 75) THEN
        CALL DebugErrorMessage('File name too long in sa_SetOutputFiles')
        POS = 75
      ENDIF
      cssr_file = FileHead(1:POS)//'.cssr'
      pdb_file  = FileHead(1:POS)//'.pdb'
      ccl_file  = FileHead(1:POS)//'.ccl'
      log_file  = FileHead(1:POS)//'.log'
      pro_file  = FileHead(1:POS)//'.pro'    
      cssr_flen = LEN_TRIM(cssr_file)
      pdb_flen  = LEN_TRIM(pdb_file)
      ccl_flen  = LEN_TRIM(ccl_file)
      log_flen  = LEN_TRIM(log_file)
      pro_flen  = LEN_TRIM(pro_file)
      outfilset = .TRUE.

      END SUBROUTINE sa_SetOutputFiles
!
!*****************************************************************************
!
