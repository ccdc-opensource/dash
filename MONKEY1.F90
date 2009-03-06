#ifdef _WIN32
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
      i = RegCloseKey(hKey) ! Close the key handle.
! Second attempt: HKCR\Applications\mercury.exe\shell\open\command
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_CLASSES_ROOT
      RegPath = 'Applications\mercury.exe\shell\open\command'//CHAR(0)
! Use RegOpenKeyEx() with the new Registry path to get an open handle
! to the child key you want to enumerate.
      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
      IF (dwType .NE. REG_SZ) GOTO 30
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
   30 CONTINUE
! Failure...

      END SUBROUTINE GetPathToMercuryFromRegistry

!
!*****************************************************************************
!
      SUBROUTINE GetPathToMogulFromRegistry

      USE DFWIN
      USE VARIABLES

      IMPLICIT NONE

      INTEGER*4, PARAMETER :: MAX_VALUE_NAME = 128
      INTEGER*4, PARAMETER :: MAX_DATA_LEN   = 1024

      INTEGER*4                  hKeyRoot
      CHARACTER*(512)            RegPath
      CHARACTER*(6)              LatestVersion
      CHARACTER*(128)            TempValueName
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

      LOGICAL, EXTERNAL :: InfoMessage

! First attempt: HKEY_LOCAL_MACHINE\SOFTWARE\CCDC\
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_LOCAL_MACHINE 
! Need to get latest version number of Mogul
      RegPath = 'SOFTWARE\CCDC\'//CHAR(0)
! Will be more than one "latest version" in SOFTWARE CCDC (CSD for instance)
! so need to get number of values of keys to search through (dwcValues)

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
      i = RegCloseKey(hKey) ! Close the key handle.


! Look through values of key
      DO J = 1, dwcValues
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
      dwLBIndex = J - 1
      cbValueName = MAX_VALUE_NAME
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
      
      IF (ValueName(1:5) .EQ. 'Mogul') THEN
        LatestVersion = TRIM(bData)
        RegPath = 'SOFTWARE\CCDC\Mogul\'//LatestVersion(1:cbData)//'\'//CHAR(0) ! this seems to work//'\'//CHAR(0)
        EXIT
      ENDIF
      i = RegCloseKey(hKey) ! Close the key handle.
      ENDDO

!Looking for executable but will more than one directory listed (install, data etc)
!so need to find number of values for key
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
      i = RegCloseKey(hKey) ! Close the key handle.


      DO J = 1, dwcValues
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
      dwLBIndex = J - 1
      cbValueName = MAX_VALUE_NAME
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
      TempValueName = ValueName
      IF (TempValueName(1:3) .EQ. 'Exe') THEN
        MOGULEXE(1:cbData) = bData(1:cbData)
        RETURN
      ENDIF
      i = RegCloseKey(hKey) ! Close the key handle.
      ENDDO

20 CONTINUE

! Second attempt: HKEY_CURRENT_USER\SOFTWARE\CCDC\
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_CURRENT_USER !HKEY_CLASSES_ROOT

! Need to get latest version number of Mogul
      RegPath = 'SOFTWARE\CCDC\'//CHAR(0)
! Will be more than one "latest version" in SOFTWARE CCDC (CSD for instance)
! so need to get number of values of keys to search through (dwcValues)

      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      i = RegCloseKey(hKey) ! Close the key handle.


! Look through values of key
      DO J = 1, dwcValues
! Use RegOpenKeyEx() with the new Registry path to get an open handle
! to the child key you want to enumerate.
      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
      cbData = dwcMaxValueData
! ENUMERATE THE KEY.
      dwLBIndex = J - 1
      cbValueName = MAX_VALUE_NAME
      retCode = RegEnumValue (hKey,             & ! Key handle returned from RegOpenKeyEx.
                              dwLBIndex,        & ! Value index, taken from listbox.
                              ValueName,        & ! Name of value.
                              LOC(cbValueName), & ! Size of value name.
                              NULL,             & ! Reserved, dword = NULL.
                              LOC(dwType),      & ! Type of data.
                              LOC(bData),       & ! Data buffer.
                              LOC(cbData))        ! Size of data buffer.
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
      IF (dwType .NE. REG_SZ) GOTO 30

      IF (ValueName(1:5) .EQ. 'Mogul') THEN
        LatestVersion = TRIM(bData)
        RegPath = 'SOFTWARE\CCDC\Mogul\'//LatestVersion(1:cbData)//'\'//CHAR(0) ! this seems to work//'\'//CHAR(0)
        EXIT
      ENDIF
      i = RegCloseKey(hKey) ! Close the key handle.
      ENDDO

!Looking for executable but will more than one directory listed (install, data etc)
!so need to find number of values for key

      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      i = RegCloseKey(hKey) ! Close the key handle.


      DO J = 1, dwcValues ! number of strings that have to be checked

      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
      cbData = dwcMaxValueData

! ENUMERATE THE KEY.
      dwLBIndex = J - 1 
      cbValueName = MAX_VALUE_NAME
      retCode = RegEnumValue (hKey,             & ! Key handle returned from RegOpenKeyEx.
                              dwLBIndex,        & ! Value index, taken from listbox.
                              ValueName,        & ! Name of value.
                              LOC(cbValueName), & ! Size of value name.
                              NULL,             & ! Reserved, dword = NULL.
                              LOC(dwType),      & ! Type of data.
                              LOC(bData),       & ! Data buffer.
                              LOC(cbData))        ! Size of data buffer.
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
      IF (dwType .NE. REG_SZ) GOTO 30
      TempValueName = ValueName
      IF (TempValueName(1:3) .EQ. 'Exe') THEN
        MOGULEXE(1:cbData) = bData(1:cbData)
        RETURN
      ENDIF
      i = RegCloseKey(hKey) ! Close the key handle.
      ENDDO
   30 CONTINUE  ! failure
      i = RegCloseKey(hKey) ! Close the key handle.

      END SUBROUTINE GetPathToMogulFromRegistry
!
!*****************************************************************************
!
      SUBROUTINE GetPathToExpguiFromRegistry

      USE DFWIN
      USE VARIABLES
      USE TAVAR

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
      INTEGER                    i

! First attempt: HKEY_CLASSES_ROOT\EXPfile\Shell\EXPGUI\command
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_CLASSES_ROOT
      RegPath = 'EXPFile\shell\EXPGUI\command'//CHAR(0)
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
      CALL CopyFirstString(EXPGUIEXE, bData, cbData)
      RETURN
   20 CONTINUE
      i = RegCloseKey(hKey) ! Close the key handle.
! Second attempt: HKCR\Applications\tcl84+.exe\shell\EXPGUI\command
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_CLASSES_ROOT
      RegPath = 'Applications\tcl84+.exe\shell\EXPGUI\command'//CHAR(0)
! Use RegOpenKeyEx() with the new Registry path to get an open handle
! to the child key you want to enumerate.
      retCode = RegOpenKeyEx(hKeyRoot,    &
                             RegPath,     &
                             0,           &
                             KEY_EXECUTE, &
                             LOC(hKey))
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
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
      IF (retCode .NE. ERROR_SUCCESS) GOTO 30
      IF (dwType .NE. REG_SZ) GOTO 30
      i = RegCloseKey(hKey) ! Close the key handle.
      CALL CopyFirstString(EXPGUIEXE, bData, cbData)
      RETURN
   30 CONTINUE
! Failure...
      RETURN

      END SUBROUTINE GetPathToExpguiFromRegistry
!
!*****************************************************************************
!
      SUBROUTINE GetPathToTopasFromRegistry

      USE DFWIN
      USE VARIABLES
      USE TAVAR

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
      INTEGER                    i

! First attempt: HKEY_CLASSES_ROOT\TopasEditor.Document.1\shell\open\command
      cbValueName = MAX_VALUE_NAME
      dwcClassLen = MAX_PATH
      hKeyRoot = HKEY_CLASSES_ROOT
      RegPath = 'TopasEditor.Document.1\shell\open\command'//CHAR(0)
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
      CALL CopyFirstString(TOPASEXE, bData, cbData)
      RETURN
   20 CONTINUE
! Failure...
      RETURN

      END SUBROUTINE GetPathToTopasFromRegistry
!
!*****************************************************************************
!
      SUBROUTINE CopyFirstString(dst, src, src_len)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (OUT) :: dst
      CHARACTER*(*), INTENT ( IN) :: src
      INTEGER, INTENT (IN) :: src_len

      CHARACTER*1 tDelimiter
      INTEGER p

      IF ( src(1:1) .EQ. '"' ) THEN
        tDelimiter = '"'
        dst = src(2:src_len) ! Skip first double quote
      ELSE
        tDelimiter = ' '
        dst = src(:src_len)
      ENDIF
      p = SCAN(dst, tDelimiter)
      IF ( p .GT. 0 ) THEN
! Found match tDelimiter
        dst(p:) = ' '
      ELSE IF ( tDelimiter .NE. ' ' ) THEN
        dst = ' '
      ENDIF
      RETURN

      END SUBROUTINE CopyFirstString
!
!*****************************************************************************
!
#else

      SUBROUTINE GetPathToMercuryFromRegistry()
      IMPLICIT NONE
      END SUBROUTINE GetPathToMercuryFromRegistry
      
      SUBROUTINE GetPathToMogulFromRegistry()
      IMPLICIT NONE
      END SUBROUTINE GetPathToMogulFromRegistry
      
      SUBROUTINE GetPathToTopasFromRegistry()
      IMPLICIT NONE
      END SUBROUTINE GetPathToTopasFromRegistry
      
      SUBROUTINE GetPathToExpguiFromRegistry()
      IMPLICIT NONE
      END SUBROUTINE GetPathToExpguiFromRegistry

#endif
