!
!*****************************************************************************
!
      SUBROUTINE GetPathToMercuryFromRegistry

      USE DFWIN
      USE VARIABLES

      IMPLICIT NONE

      INTEGER*4, PARAMETER :: MAX_VALUE_NAME = 128
      INTEGER*4, PARAMETER :: MAX_DATA_LEN   = 1024

      INTEGER*4                  hKeyRoot
      CHARACTER*(512)            RegPath
      INTEGER*4                  hKey
      INTEGER*4                  retCode
      CHARACTER*(MAX_PATH)       ClassName
      INTEGER*4                  dwcClassLen
      INTEGER*4                  dwcSubKeys
      INTEGER*4                  dwcMaxSubKey
      INTEGER*4                  dwcMaxClass
      INTEGER*4                  dwcValues
      INTEGER*4                  dwcMaxValueName
      INTEGER*4                  dwcMaxValueData
      INTEGER*4                  dwcSecDesc
      TYPE (T_FILETIME)          ftLastWriteTime
      CHARACTER*(MAX_VALUE_NAME) ValueName
      INTEGER*4                  cbValueName
      INTEGER*4                  cbData
      INTEGER*4                  dwLBIndex
      INTEGER*4                  dwType
      CHARACTER*MAX_DATA_LEN     bData
      INTEGER                    i, j

! First attempt: HKEY_CLASSES_ROOT\mercuryFile\shell\open\command
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_CLASSES_ROOT
      RegPath = 'mercuryFile\shell\open\command'//CHAR(0)
! Use RegOpenKeyEx() with the new Registry path to get an open handle
! to the child key you want to enumerate.
      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 20
! ADD A QUERY AND ALLOCATE A BUFFER FOR BDATA.
      retCode = RegQueryInfoKey(hKey,                 &  ! Key handle returned from RegOpenKeyEx.
                                ClassName,            &  ! Buffer for class name.
                                LOC(dwcClassLen),     &  ! Length of class string.
                                NULL,                 &  ! Reserved.
                                LOC(dwcSubKeys),      &  ! Number of sub keys.
                                LOC(dwcMaxSubKey),    &  ! Longest sub key size.
                                LOC(dwcMaxClass),     &  ! Longest class string.
                                LOC(dwcValues),       &  ! Number of values for this key.
                                LOC(dwcMaxValueName), &  ! Longest Value name.
                                LOC(dwcMaxValueData), &  ! Longest Value data.
                                LOC(dwcSecDesc),      &  ! Security descriptor.
                                ftLastWriteTime)         ! Last write time.
      IF (retCode .NE. ERROR_SUCCESS) GOTO 20
      cbData = dwcMaxValueData
! ENUMERATE THE KEY.
      dwLBIndex = 0 ! I'm guessing here
      retCode = RegEnumValue (hKey,             & ! Key handle returned from RegOpenKeyEx.
                              dwLBIndex,        & ! Value index, taken from listbox.
                              ValueName,        & ! Name of value.
                              LOC(cbValueName), & ! Size of value name.
                              NULL,             & ! Reserved, dword = NULL.
                              LOC(dwType),      & ! Type of data.
                              LOC(bData),       & ! Data buffer.
                              LOC(cbData))        ! Size of data buffer.
      IF (retCode .NE. ERROR_SUCCESS) GOTO 20
      IF (dwType .NE. REG_SZ) GOTO 20
      i = RegCloseKey(hKey) ! Close the key handle.
! Remove first double quote
      VIEWEXE(1:cbData-1) = bData(2:cbData)
! Find second
      i = 1
      DO WHILE ((VIEWEXE(i:i) .NE. '"') .AND. (i .LT. cbData))
        i = i + 1
      ENDDO
      IF (VIEWEXE(i:i) .EQ. '"') THEN
        DO j = i, MaxPathLength
          VIEWEXE(j:j) = ' '
        ENDDO
      ENDIF
      RETURN
   20 CONTINUE
! Second attempt: HKCU\Software\CCDC\Mercury\1.0\InstallDir
      i = RegCloseKey(hKey) ! Close the key handle.

      END SUBROUTINE GetPathToMercuryFromRegistry
!
!*****************************************************************************
!
