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

! Initialise to 'no viewer found'
      VIEWEXE = ''
      VIEWARG = ''
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
      IF (retCode .NE. 0) GOTO 20
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
      IF (retCode .NE. 0) GOTO 20
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
!U!
!U!*****************************************************************************
!U!
!U!  FUNCTION: DisplayKeyData()
!U!
!U!  PURPOSE:  To display the keys values and value types to the Value edit
!U!            field. This function is called when the right hand listbox
!U!            is double clicked. The functionality is much like that found
!U!            in the function PrintTree, please see it for more details.
!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE DisplayKeyData (hDlg, sRegPath, hKeyRoot)
!U
!U      USE dfwin
!U      USE monkeyin
!U
!U      INTEGER*4                   hDlg
!U      CHARACTER*(*)               sRegPath
!U      INTEGER*4                   hKeyRoot
!U      INTEGER*4                   hKey
!U      INTEGER*4                   dwLBIndex
!U      CHARACTER*(LINE_LEN)        Buf
!U      CHARACTER*(MAX_VALUE_NAME)  ValueName
!U      INTEGER*4                   cbValueName
!U      INTEGER*4                   dwType
!U      INTEGER*4                   retCode
!U      CHARACTER*(MAX_PATH)    ClassName
!U      INTEGER*4               dwcClassLen
!U      INTEGER*4               dwcSubKeys
!U      INTEGER*4               dwcMaxSubKey
!U      INTEGER*4               dwcMaxClass
!U      INTEGER*4               dwcValues
!U      INTEGER*4               dwcMaxValueName
!U      INTEGER*4               dwcMaxValueData
!U      INTEGER*4               dwcSecDesc
!U      TYPE (T_FILETIME)       ftLastWriteTime
!U      CHARACTER*1024          bData
!U      CHARACTER*1024          outBuf
!U      CHARACTER*1024          BinaryStrBuf
!U      INTEGER*4               ptr ![allocatable] (:)
!U      INTEGER*4               cbData
!U      INTEGER*4               i
!U      INTEGER*4               cStrLen
!U      CHARACTER*4             ByteBuf
!U      CHARACTER*100           lpszHeader
!U
!U      RegPath = sRegPath
!U! OPEN THE KEY.
!U      cbValueName = MAX_VALUE_NAME
!U      dwcClassLen = MAX_PATH
!U      dwLBIndex = SendMessage (GetDlgItem (hDlg, IDL_LISTBOX2), LB_GETCURSEL, 0, 0)
!U      retCode = RegOpenKeyEx (hKeyRoot,    & ! Key handle at root level.
!U                              RegPath,     & ! Path name of child key.
!U                              0,           & ! Reserved.
!U                              KEY_EXECUTE, & ! Requesting read access.
!U                              LOC(hKey))     ! Address of key to be returned.
!U      IF (retCode .NE. 0) RETURN
!U! ADD A QUERY AND ALLOCATE A BUFFER FOR BDATA.
!U      retCode = RegQueryInfoKey (hKey,                 &  ! Key handle.
!U                                 ClassName,            &  ! Buffer for class name.
!U                                 LOC(dwcClassLen),     &  ! Length of class string.
!U                                 NULL,                 &  ! Reserved.
!U                                 LOC(dwcSubKeys),      &  ! Number of sub keys.
!U                                 LOC(dwcMaxSubKey),    &  ! Longest sub key size.
!U                                 LOC(dwcMaxClass),     &  ! Longest class string.
!U                                 LOC(dwcValues),       &  ! Number of values for this key.
!U                                 LOC(dwcMaxValueName), &  ! Longest Value name.
!U                                 LOC(dwcMaxValueData), &  ! Longest Value data.
!U                                 LOC(dwcSecDesc),      &  ! Security descriptor.
!U                                 ftLastWriteTime)         ! Last write time.
!U      IF (retCode .NE. 0) THEN
!U        WRITE (Buf, 2000)  retCode
!U2000    FORMAT ('Error: RegQIK = ', I5)
!U        i = MessageBox (hDlg, Buf, ""C, MB_OK)
!U      ENDIF
!U      cbData = dwcMaxValueData
!U! ENUMERATE THE KEY.
!U      retCode = RegEnumValue (hKey,             & ! Key handle returned from RegOpenKeyEx.
!U                              dwLBIndex,        & ! Value index, taken from listbox.
!U                              ValueName,        & ! Name of value.
!U                              LOC(cbValueName), & ! Size of value name.
!U                              NULL,             & ! Reserved, dword = NULL.
!U                              LOC(dwType),      & ! Type of data.
!U                              LOC(bData),       & ! Data buffer.
!U                              LOC(cbData))        ! Size of data buffer.
!U      IF (retCode .NE. ERROR_SUCCESS) THEN
!U        IF (dwType < REG_FULL_RESOURCE_DESCRIPTOR) THEN
!U          WRITE (Buf, 3000) retCode, cbData
!U3000      FORMAT ('Error: RegEnumValue = ', I5, 'cbData = ',I5)
!U          i = MessageBox (hDlg, Buf, ""C, MB_OK)
!U        ENDIF
!U      ENDIF
!U      SELECT CASE (dwType)
!U   !    REG_NONE                     ( 0 )   ! No value type
!U   !    REG_SZ                       ( 1 )   ! Unicode nul terminated string
!U   !    REG_EXPAND_SZ                ( 2 )   ! Unicode nul terminated string
!U   !    (with environment variable references)
!U   !    REG_BINARY                   ( 3 )   ! Free form binary
!U   !    REG_DWORD                    ( 4 )   ! 32-bit number
!U   !    REG_DWORD_LITTLE_ENDIAN      ( 4 )   ! 32-bit number (same as REG_DWORD)
!U   !    REG_DWORD_BIG_ENDIAN         ( 5 )   ! 32-bit number
!U   !    REG_LINK                     ( 6 )   ! Symbolic Link (unicode)
!U   !    REG_MULTI_SZ                 ( 7 )   ! Multiple Unicode strings
!U   !    REG_RESOURCE_LIST            ( 8 )   ! Resource list in the resource map
!U   !    REG_FULL_RESOURCE_DESCRIPTOR ( 9 )   ! Resource list in the hardware description
!U        CASE (REG_NONE)
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, "REG_NONE: No defined value type."C)
!U        CASE (REG_SZ)
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, "REG_SZ: A null-terminated Unicode string."C)
!U          outBuf = '\0'C
!U          i = lstrcat (outBuf, '\"'C)
!U          i = lstrcat (outBuf, bData)
!U          i = lstrcat (outBuf,'\"'C)
!U          i = SetDlgItemText (hDlg, IDE_VALUE2, outBuf)
!U        CASE (REG_EXPAND_SZ)
!U          i = SetDlgItemText (hDlg, IDE_VALUE1,"REG_EXPAND_SZ: A String referencing environment variables i.e. PATH."C)
!U          i = lstrcat (outBuf, '\"')
!U          i = lstrcat (outBuf, bData)
!U          i = lstrcat (outBuf,'\"'C)
!U          i = SetDlgItemText (hDlg, IDE_VALUE2, outBuf)
!U        CASE (REG_BINARY)
!U          lpszHeader = "REG_BINARY: Freeform binary data."C
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, lpszHeader)
!U          i = SetCursor (LoadCursor (NULL, IDC_WAIT))
!U          BinaryStrBuf(1:1) = '\0'C
!U          ByteBuf(1:1) = '\0'C
!U          DO i = 1 ,cbData
!U            WRITE(ByteBuf, 5000) bData(i:i)
!U5000        FORMAT  (A)
!U            i2 = lstrcat (BinaryStrBuf, ByteBuf)
!U          ENDDO
!U          i = SetDlgItemText (hDlg, IDE_VALUE2, BinaryStrBuf)
!U          i = SetDlgItemText (hDlg, IDL_LISTBOX2, BinaryStrBuf)
!U          i = SetCursor (LoadCursor (NULL, IDC_ARROW))
!U        CASE (REG_DWORD)
!U          lpszHeader = "REG_DWORD: A 32 bit number."C
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, lpszHeader)
!U          i1 = ICHAR(bData(1:1))
!U          i = SetDlgItemInt (hDlg, IDE_VALUE2, i1, .FALSE.)
!U        CASE (REG_DWORD_BIG_ENDIAN)
!U          lpszHeader = "REG_DWORD_BIG_ENDIAN: A 32 bit number in big endian format."C
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, lpszHeader)
!U          i1 = ICHAR(bData(1:1))
!U          i = SetDlgItemInt (hDlg, IDE_VALUE2, i1, .TRUE.)
!U        CASE (REG_LINK)
!U          lpszHeader = "REG_LINK: A Unicode symbolic link."C
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, lpszHeader)
!U          i = SetDlgItemText (hDlg, IDE_VALUE2, bData)
!U        CASE (REG_MULTI_SZ)
!U          lpszHeader = "REG_MULTI_SZ: An array of null-terminated strings."C
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, lpszHeader)
!U          i = SetCursor (LoadCursor (NULL, IDC_WAIT))
!U! Count the NULLs in the buffer to find out how many strings there are.
!U          cStrLen = 4
!U          DO i=1, cbData
!U            IF (ICHAR(bData(i:i)) == 0) THEN
!U               cStrLen =cStrLen+4  ! Add room for two quotes and two spaces per string.
!U            ENDIF
!U          ENDDO
!U          ptr = 1        !bData  Set ptr to beginning of buffer.
!U          outBuf(1:1) = '\0'C                   ! Initialize output string.
!U          i2   = lstrcat (outBuf,"{"C)          ! Do first bracket.
!U          DO WHILE (ICHAR(bData(ptr:ptr)) .NE. 0)         ! Loop til you hit 2 NULLs in a row.
!U            i2 = lstrcat (outBuf,'\"'C)        ! Put quotes around each string.
!U            i2 = lstrcat (outBuf, '\"  'C)
!U            ptr = lstrlen(bdata(ptr:ptr))+1
!U          ENDDO
!U          i2 = lstrcat (outBuf, "}"C)            ! Add final bracket.
!U          i2 = SetDlgItemText (hDlg, IDE_VALUE2, outBuf)
!U          i2 = SetCursor (LoadCursor (NULL, IDC_ARROW))
!U        CASE (REG_RESOURCE_LIST)            ! CM_RESOURCE_LIST is kind of complex,
!U                                          ! it's defined in ntconfig.h.  Print
!U                                          ! it as a free formed binary data now,
!U                                          ! and structure it later with a
!U                                          ! different release.
!U          i2 = SetDlgItemText (hDlg, IDE_VALUE1, "REG_RESOURCE_LIST: A device-driver resource list."C)
!U          BinaryStrBuf(1:1) = '\0'C
!U          ByteBuf(1:1) = '\0'C
!U          DO i = 1, cbData
!U            WRITE(ByteBuf, 9000) bData(i:i)
!U9000        FORMAT  (A)
!U            i2 = lstrcat (BinaryStrBuf, ByteBuf)
!U          ENDDO
!U          i = SetDlgItemText (hDlg, IDE_VALUE2, BinaryStrBuf)
!U          i = SetDlgItemText (hDlg, IDL_LISTBOX2, BinaryStrBuf)
!U        CASE (REG_FULL_RESOURCE_DESCRIPTOR)
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, "REG_FULL_RESOURCE_DESCRIPTOR: A resource list in the hardware description."C)
!U        CASE DEFAULT
!U          WRITE (Buf, 6000) dwType
!U6000      FORMAT ('Undefined in this verion of the Registry Monkey. ', I5)
!U          i = SetDlgItemText (hDlg, IDE_VALUE1, Buf)
!U      END SELECT
!U
!U      END SUBROUTINE DisplayKeyData
!U!
!U!*****************************************************************************
!U!
!U!  FUNCTION: QueryKey()
!U!
!U!  PURPOSE:  To display the key's children (subkeys) and the names of
!U!            the Values associated with it.  This function uses RegEnumKey,
!U!            RegEnumValue, and RegQueryInfoKey.
!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE QueryKey(hDlg, hKey)
!U
!U      USE dfwin
!U      USE monkeyin
!U
!U      INTEGER*4                       hDlg, hKey
!U      CHARACTER*(MAX_PATH)            KeyName
!U      CHARACTER*(MAX_PATH)            ClassName                ! Buffer for class name.
!U      INTEGER*4                       dwcClassLen              ! Length of class string.
!U      INTEGER*4                       dwcSubKeys               ! Number of sub keys.
!U      INTEGER*4                       dwcMaxSubKey             ! Longest sub key size.
!U      INTEGER*4                       dwcMaxClass              ! Longest class string.
!U      INTEGER*4                       dwcValues                ! Number of values for this key.
!U      INTEGER*4                       dwcMaxValueName          ! Longest Value name.
!U      INTEGER*4                       dwcMaxValueData          ! Longest Value data.
!U      INTEGER*4                       dwcSecDesc               ! Security descriptor.
!U      TYPE (T_FILETIME)               ftLastWriteTime          ! Last write time.
!U      INTEGER*4                       i
!U      INTEGER*4                       retCode
!U      INTEGER*4                       j
!U      INTEGER*4                       retValue
!U      CHARACTER*(MAX_VALUE_NAME)      ValueName
!U      INTEGER*4                       dwcValueName
!U      CHARACTER*(LINE_LEN)            Buf
!U
!U      CALL ZeroMemory(LOC(ClassName),MAX_PATH)
!U      dwcClassLen  = MAX_PATH
!U      dwcValueName = MAX_VALUE_NAME
!U! Get Class name, Value count.
!U      i = RegQueryInfoKey (hKey,                 & ! Key handle.
!U                           ClassName,            & ! Buffer for class name.
!U                           LOC(dwcClassLen),     & ! Length of class string.
!U                           NULL,                 & ! Reserved.
!U                           LOC(dwcSubKeys),      & ! Number of sub keys.
!U                           LOC(dwcMaxSubKey),    & ! Longest sub key size.
!U                           LOC(dwcMaxClass),     & ! Longest class string.
!U                           LOC(dwcValues),       & ! Number of values for this key.
!U                           LOC(dwcMaxValueName), & ! Longest Value name.
!U                           LOC(dwcMaxValueData), & ! Longest Value data.
!U                           LOC(dwcSecDesc),      & ! Security descriptor.
!U                           ftLastWriteTime)        ! Last write time.
!U      i = SetDlgItemText(hDlg, IDE_CLASS, ClassName)
!U      i = SetDlgItemInt (hDlg, IDE_CVALUES, dwcValues, .FALSE.)
!U      i = SendMessage(GetDlgItem (hDlg, IDL_LISTBOX),LB_ADDSTRING, 0, LOC(".."C))
!U! Loop until RegEnumKey fails, get the name of each child and enter it into the box.
!U! Enumerate the Child Keys.
!U      i = SetCursor (LoadCursor (NULL, IDC_WAIT))
!U      retCode = ERROR_SUCCESS
!U      i = -1
!U      DO WHILE (retCode == ERROR_SUCCESS  )
!U        i = i + 1
!U        retCode = RegEnumKey (hKey, i, KeyName, MAX_PATH)
!U        IF (retCode == ERROR_SUCCESS ) i = SendMessage (GetDlgItem(hDlg, IDL_LISTBOX),LB_ADDSTRING, 0, LOC(KeyName))
!U      ENDDO
!U      i = SetCursor (LoadCursor (NULL, IDC_ARROW))
!U! Enumerate the Key Values
!U      i = SetCursor (LoadCursor (NULL, IDC_WAIT))
!U      IF (dwcValues .NE. 0) THEN
!U        retValue = ERROR_SUCCESS
!U        DO j = 0, dwcValues - 1
!U          dwcValueName = MAX_VALUE_NAME
!U          ValueName(1:1) = '\0'C
!U          retValue = RegEnumValue (hKey, j, ValueName,  &
!U                                   LOC(dwcValueName),   &
!U                                   NULL,                &
!U                                   NULL,                &  ! dwType
!U                                   NULL,                &  ! bData
!U                                   NULL)                   ! bcData
!U          IF ((retValue .NE. ERROR_SUCCESS ) .AND. (retValue .NE. ERROR_INSUFFICIENT_BUFFER)) THEN
!U            WRITE (Buf, 300) j, retValue, dwcValueName
!U300         FORMAT ('0 based index = ',I5,'retValue = ', I5, 'ValueLen =', I5)
!U            i = MessageBox (hDlg, Buf,"Debug"C, MB_OK)
!U          ENDIF
!U          Buf(1:1) = '\0'C
!U          IF (lstrlen(ValueName) == 0) i = lstrcpy (ValueName, "<NO NAME>"C)
!U          WRITE (Buf, 410) j, ValueName
!U410       FORMAT (I5,') ', A30 )
!U          i = SendMessage (GetDlgItem (hDlg, IDL_LISTBOX2),LB_ADDSTRING, 0, LOC(Buf))
!U        ENDDO ! end for()
!U      ENDIF
!U      i = SetCursor (LoadCursor (NULL, IDC_ARROW))
!U
!U      END SUBROUTINE QueryKey
!U!
!U!*****************************************************************************
!U!
!U!  FUNCTION: EnumerateLevel()
!U!
!U!  PURPOSE: To get a valid key handle (either to determine if the one sent
!U!           to the function was one of the pre-defined, or to open a key
!U!           specified by the path), and to pass that key handle along
!U!           to QueryKey().
!U!
!U!           To enumerate the children of a key, you must have
!U!           an open handle to it. The four top keys of the
!U!           Registry are predefined and open for use:
!U!           HKEY_LOCAL_MACHINE, HKEY_USERS, HKEY_CURRENT_USER,
!U!           and HKEY_CLASSES_ROOT. These 4 can be used for
!U!           RegEnumKey as is but to RegEnumKey on any of the
!U!           children of these you must first have an open key
!U!           handle to the child.
!U!
!U!           If hKeyRoot .NE. 0, assume you are lower than the
!U!           first level of the Registry and the user is trying
!U!           to enumerate one of the children. First calculate
!U!           the name of the child, and then use RegOpenKey to
!U!           get an open handle.
!U!
!U!           If hKeyRoot .EQ. 0, assume you are at the top level
!U!           of the Registry, and set the hKey to be enumerated
!U!           to be one of the 4 predefined values.
!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE EnumerateLevel (hDlg, NameLBSelect, sRegPath, hKeyRoot)
!U!DEC$ ATTRIBUTES REFERENCE :: sRegPath
!U      USE dfwin
!U      USE monkeyin
!U
!U      INTEGER*4       hDlg
!U      CHARACTER*(*)   NameLBSelect
!U      CHARACTER*(*)   sRegPath
!U      INTEGER*4       hKeyRoot
!U      INTEGER*4       hKey
!U      INTEGER*4       retCode
!U      CHARACTER*(LINE_LEN) Buf
!U
!U      RegPath = sRegPath
!U      IF (hKeyRoot .NE. 0) THEN
!U! If RegPath is not NULL, then you have to add a backslash to the
!U! path name before appending the next level child name.
!U        IF (RegPath(1:1) .NE. CHAR(0)) i = lstrcat (RegPath, "\\"C)
!U        i = lstrcat (RegPath,NameLBSelect) ! Add the next level child name.
!U! Use RegOpenKeyEx() with the new Registry path to get an open handle
!U! to the child key you want to enumerate.
!U        retCode = RegOpenKeyEx (hKeyRoot,                      &
!U                                RegPath,                       &
!U                                0,                             &
!U                                IOR(KEY_ENUMERATE_SUB_KEYS,IOR(KEY_EXECUTE,KEY_QUERY_VALUE)),             &
!U                                LOC(hKey))
!U        IF (retCode .NE. ERROR_SUCCESS) THEN
!U          IF (retCode == ERROR_ACCESS_DENIED) THEN
!U            WRITE(Buf, 100)
!U100         FORMAT ('Error: unable to open key. Probably due to security reasons.')
!U          ELSE
!U            WRITE (Buf, 200) retCode
!U200         FORMAT ('Error: Unable to open key, RegOpenKey = ',I5)
!U          ENDIF
!U          i = MessageBox (hDlg, Buf,""C, MB_OK)
!U          i = PostMessage (hDlg, WM_COMMAND, IDB_BACK, 0)
!U          RETURN
!U        ENDIF
!U      ELSE  ! Set the *hKeyRoot handle based on the text taken from the ListBox.
!U        IF (lstrcmp(NameLBSelect, "HKEY_CLASSES_ROOT"C)  .EQ. 0) hKeyRoot = HKEY_CLASSES_ROOT
!U        IF (lstrcmp(NameLBSelect, "HKEY_USERS"C)         .EQ. 0) hKeyRoot = HKEY_USERS
!U        IF (lstrcmp(NameLBSelect, "HKEY_LOCAL_MACHINE"C) .EQ. 0) hKeyRoot = HKEY_LOCAL_MACHINE
!U        IF (lstrcmp(NameLBSelect, "HKEY_CURRENT_USER"C)  .EQ. 0) hKeyRoot = HKEY_CURRENT_USER
!U        hKey = hKeyRoot     ! hKey is used in RegEnumKey().
!U      ENDIF
!U      CALL QueryKey (hDlg, hKey)
!U      i = RegCloseKey(hKey)   ! Close the key handle.
!U      i = SetDlgItemText(hDlg, IDE_TEXTOUT, RegPath)
!U
!U      END SUBROUTINE EnumerateLevel
!U!
!U!*****************************************************************************
!U!
