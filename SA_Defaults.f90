!
!*****************************************************************************
!
      SUBROUTINE sa_SetOutputFiles(TheFileHead)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileHead 

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

! Find the last occurrence of '.'
      OFBN_Len = LEN_TRIM(TheFileHead)
      DO WHILE ((OFBN_Len .GT. 0) .AND. (TheFileHead(OFBN_Len:OFBN_Len) .NE. '.'))
        OFBN_Len = OFBN_Len - 1
      ENDDO
! If no '.' present, pretend there is one after the filename
      IF (OFBN_Len .EQ. 0) OFBN_Len = LEN_TRIM(TheFileHead) + 1
! Now point to the position just before the '.'
      OFBN_Len = OFBN_Len - 1
! We will append '_001.cssr', which is nine characters, and after that the total length shouldn't exceed 255
      IF (OFBN_Len .GT. (MaxPathLength-9)) THEN
        CALL DebugErrorMessage('File name too long in sa_SetOutputFiles()')
        OFBN_Len = MaxPathLength-9
      ENDIF
      OutputFilesBaseName = TheFileHead(1:OFBN_Len)

      END SUBROUTINE sa_SetOutputFiles
!
!*****************************************************************************
!
