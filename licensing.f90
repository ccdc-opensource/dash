!
!*****************************************************************************
!
      SUBROUTINE CheckLicence

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*2 Exp
      TYPE (License_Info) Info

      CALL WDialogLoad(IDD_LicenceAgreement)
      CALL WDialogLoad(IDD_License_Dialog)
      CALL ReadLicenceValid(Info)
      DO WHILE (Info%Valid .LE. 0) 
        SELECT CASE (Info%Valid)
          CASE (-1)
            CALL ErrorMessage("Could not find or open the licence file"//CHAR(13)//&
              InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"License.dat.")
          CASE (-2) ! Checksum not OK
            CALL ErrorMessage("Your DASH licence key is not valid.")
          CASE (-3)
            CALL ErrorMessage("Your DASH licence has expired.")
          CASE (-4)
            CALL ErrorMessage("Your DASH licence is invalid for this machine.")
        END SELECT
        CALL LicencePopup
        CALL ReadLicenceValid(Info)
      ENDDO
      IF (Info%DaysLeft .LE. 7) THEN
        WRITE(Exp,'(I2)') Info%DaysLeft
        CALL InfoMessage("Your DASH licence will expire in "//Exp//" days.")
      ENDIF
      CALL WDialogSelect(IDD_LicenceAgreement)
      CALL WDialogUnload
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogUnload

      END SUBROUTINE CheckLicence
!
!*****************************************************************************
!
      SUBROUTINE LicencePopup

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL     INLOOP
      INTEGER     ICode
      CHARACTER*MaxPathLength ClString
      TYPE (License_Info) Info
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      INLOOP = .TRUE.
      Info%Valid = 0
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogShow(-1,-1,0,SemiModeless)
      CALL WMessageEnable(FieldChanged,Enabled)
      IF (WDialogGetCheckBoxLogical(IDF_License_Site)) THEN
        CALL WDialogFieldState(IDF_License_SiteCode,Enabled)
        CALL WDialogFieldState(IDF_License_SiteCodeLabel,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_License_SiteCode,Disabled)
        CALL WDialogFieldState(IDF_License_SiteCodeLabel,Disabled)
      ENDIF
      DO WHILE (INLOOP)
        CALL GetEvent
        SELECT CASE (EventType)
          CASE (PushButton)
            SELECT CASE(EventInfo%VALUE1)
              CASE (IDCANCEL, ID_Licensing_Exit)
                CALL DoExit
              CASE (IDOK)
                CALL WDialogGetString(IDF_License_String, CLString)
                CALL DecodeLicence(CLString,Info)
                IF (Info%Valid .LT. 0 ) THEN
                  SELECT CASE (Info%Valid)
                    CASE (-2) ! Checksum not OK
                      CALL ErrorMessage("Licence key not valid.")
                    CASE (-3)
                      CALL ErrorMessage("Your DASH licence has expired.")
                    CASE (-4)
                      CALL ErrorMessage("Your DASH licence is invalid for this machine.")
                  END SELECT
                ELSE
                  IF (WDialogGetCheckBoxLogical(IDF_License_Site)) THEN
                    IF (Info%LicenceType .NE. SiteKey) THEN
                      CALL ErrorMessage("Sorry, the licence key is not a site licence.")
                      Info%Valid = -6
                    ELSE
                      CALL WDialogGetInteger(IDF_License_SiteCode, ICode) 
                      IF (Info%SerialNumber .NE. ICode) THEN
                        CALL ErrorMessage("Sorry, the licence key is not valid for this site.") 
                        Info%Valid = -6
                      ENDIF
                    ENDIF
                  ELSE
                    IF (Info%LicenceType .EQ. SiteKey) THEN ! Key indicates site licence, but user didn't fill out
                      CALL ErrorMessage("The licence key is a site licence:"//CHAR(13)//&
                                    "please select the Site Licence check-box and enter your site code as well.")
                      Info%Valid = -6
                    ENDIF
                  ENDIF
                ENDIF
                IF (Info%Valid .GT. 0) THEN
                  CALL WriteLicenceFile(CLString)
                  INLOOP = .FALSE.
                ENDIF
              CASE (ID_Licence_Request)
                CALL WriteLicenceRequestForm
                CALL WExit
            END SELECT
          CASE (CloseRequest)
            CALL WExit
          CASE (FieldChanged)
            IF (WDialogGetCheckBoxLogical(IDF_License_Site)) THEN
              CALL WDialogFieldState(IDF_License_SiteCode,Enabled)
              CALL WDialogFieldState(IDF_License_SiteCodeLabel,Enabled)
            ELSE
              CALL WDialogFieldState(IDF_License_SiteCode,Disabled)
              CALL WDialogFieldState(IDF_License_SiteCodeLabel,Disabled)
            ENDIF
        END SELECT
      ENDDO
   99 CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogHide

      END SUBROUTINE LicencePopup
!
!*****************************************************************************
!
      SUBROUTINE decipher(v,w)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: v(2)
      INTEGER, INTENT (  OUT) :: w(2)

      INTEGER y,  z
      INTEGER a, b, c, d
      INTEGER n 
      INTEGER sum
      INTEGER :: delta = 16#9E3779B9

      sum = 16#C6EF3720
      n = 32
      y = v(1)
      z = v(2)
      a = 2453
      b = 1768
      c = 4567
      d = 1453
      DO WHILE (n .GT. 0)
        n = n - 1
        z = z - (ISHFT(y,4)) - IEOR(c,y) - IEOR(sum,ISHFT(y,-5)) - d
        y = y - (ISHFT(z,4)) - IEOR(a,z) - IEOR(sum,ISHFT(z,-5)) - b
        sum = sum - delta
      ENDDO
      w(1) = y
      w(2) = z

      END SUBROUTINE decipher
!
!*****************************************************************************
!
      SUBROUTINE DecodeLicence(LString,Info)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*) LString
      TYPE (License_Info) Info
      INTEGER v(2), w(2), cs
      INTEGER*2 tCheckSum
      EQUIVALENCE (tCheckSum,cs)
      INTEGER*2 checksum
      INTEGER, EXTERNAL :: Get_DashSerialNumber, DateToday

      Info%Valid = 1
! JvdS Next lines very dirty: v is INTEGER*4, but their XOR is INTEGER*2. Not possible.
      READ(LString,'(2Z8,Z4)',ERR = 99) v(1), v(2), checksum
      cs = IEOR(v(1),v(2))
      cs = IEOR(cs,16#1504)
! Check the checksum
      IF (tCheckSum .NE. checksum) THEN
! If the checksum is invalid, then that's the end of our checks.
        Info%Valid = -2
        RETURN
      ENDIF
      CALL decipher(v,w)
      Info%SerialNumber = w(1)
      Info%LicenceType  = w(2)/100000000
      Info%ExpiryDate   = w(2) - Info%LicenceType*100000000
      IF (Info%LicenceType .EQ. SiteKey) Info%SerialNumber = Info%SerialNumber - 145789123 ! demangle into a site number
      Info%DaysLeft = MAX(0, Info%ExpiryDate - DateToday())
      IF (Info%DaysLeft .EQ. 0) THEN
! If the licence key has expired, then that's the end of our checks.
        Info%Valid = -3
        RETURN
      ENDIF
      IF (Info%LicenceType .EQ. NodeKey) THEN
! For node-locked licences check the serial id. Site-Wide licences just encode a serial id for our reference
! so if we catch any non-authorized users using the key, we know where it came from. Perhaps we may want to make
! the user key in this site code on installation for checking purposes.
        IF (Info%SerialNumber .NE. Get_DashSerialNumber("C:\\"C)) Info%Valid = -4
      ENDIF
      RETURN
   99 Info%Valid = -2

      END SUBROUTINE DecodeLicence
!
!*****************************************************************************
!
      SUBROUTINE ReadLicenceValid(Info)

      USE VARIABLES  

      IMPLICIT NONE

      TYPE(License_Info) Info

      CHARACTER*80 line, CLString
      INTEGER      dummy, hFile

      Info%Valid = -1
      hFile = 10
      OPEN(UNIT=hFile,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'License.dat',STATUS='OLD',ERR=999)
   10 READ(hFile,'(A)',ERR=999,END=999) line
      IF (line(1:1) .EQ. '#') GOTO 10
      CALL INextString(line,CLString)
      CALL DecodeLicence(CLString,Info)
      IF (Info%Valid .EQ. 1) THEN
        IF (Info%LicenceType .EQ. DemoKey) CALL ShowLicenceAgreement(Info)
      ENDIF
  999 CLOSE(hFile,iostat=dummy)

      END SUBROUTINE ReadLicenceValid
!
!*****************************************************************************
!
      SUBROUTINE WriteLicenceFile(LString)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*)  LString
      CHARACTER*11   Ctypestr
      TYPE (License_Info) Info
      CHARACTER(17) DateStr
      INTEGER       hFile, tLen

      CALL DecodeLicence(LString,Info)
      IF (Info%Valid .LE. 0) GOTO 99
      SELECT CASE ( Info%LicenceType ) 
        CASE (DemoKey)
          Ctypestr = 'Demo'
        CASE (NodeKey)
          Ctypestr = 'Node Locked'
        CASE (SiteKey)
          Ctypestr = 'Site'
        CASE DEFAULT
          GOTO 99
      END SELECT
      hFile = 10
      OPEN(UNIT=hFile,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'License.dat',STATUS='UNKNOWN',ERR=99)
      WRITE(hFile,'(A)',ERR=99)     "# Licence File for "//ProgramVersion
      WRITE(hFile,'(A)',ERR=99)     "#"
      WRITE(hFile,'(A,A,A)',ERR=99) '# This is a ',Ctypestr(1:LEN_TRIM(Ctypestr)),' licence '
      IF      (Info%LicenceType .EQ. NodeKey) THEN
        WRITE(hFile,'(A,Z8)',ERR=99) '# Your DASH Serial ID for this machine is ',Info%SerialNumber
      ELSE IF (Info%LicenceType .EQ. SiteKey) THEN
        WRITE(hFile,'(A,Z8)',ERR=99) '# Your DASH Site ID is ',Info%SerialNumber
      ENDIF
      IF (Info%ExpiryDate .EQ. 99990000) THEN
        WRITE(hFile,'(A)',ERR=99)'# The licence is non-expiring'
      ELSE
        CALL Date2String(Info%ExpiryDate,DateStr,tLen)
        WRITE(hFile,'(A)',ERR=99) '# The licence expires on '//DateStr(1:tLen)
      ENDIF
      WRITE(hFile,'(A)',ERR=99)"# Licence key follows :"
      WRITE(hFile,'(A)',ERR=99) LString(1:LEN_TRIM(LString))
   99 CONTINUE
      CLOSE(hFile)

      END SUBROUTINE WriteLicenceFile
!
!*****************************************************************************
!
      SUBROUTINE WriteLicenceRequestForm

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*40 fstr
      INTEGER      Iflags, Idummy, Sn
      INTEGER      Iun, IHan
      PARAMETER (Iun = 117)
      INTEGER      Get_DashSerialNumber

      IFlags = SaveDialog + DirChange + AppendExt
      fstr = 'Text files|*.txt|All files|*.*|'
      fname = ' '
      Idummy = 1
      CALL WSelectFile(fstr,IFlags,Fname,"Please enter a filename",Idummy)
      IF (Fname(1:1) .EQ. ' ') RETURN
      OPEN(unit = Iun, file = Fname(1:LEN_TRIM(Fname)),status = 'unknown',err=99)
      Sn = Get_DashSerialNumber("C:\\"C)
      WRITE(Iun,'(A)',ERR=100) 'This file is provided to submit requests for '//ProgramVersion//' licences.'
      WRITE(Iun,'(A)',ERR=100) 'A DASH evaluation licence will allow you to run DASH on any PC.'
      WRITE(Iun,'(A)',ERR=100) 'A site licence will allow you to install DASH on any PC on your own site.'
      WRITE(Iun,'(A)',ERR=100) 'Most licences, however, are node-locked. For this, we use a unique identifier.'
      WRITE(Iun,'(A,Z8)',ERR=100)'For this PC, this is ',Sn
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100) 'Please complete as applicable:'
      WRITE(Iun,*)
      WRITE(Iun,'(A)',ERR=100) 'I would like to evaluate/purchase DASH'
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100) 'I work in industry/an academic institution'
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100) 'Please enter your address here:'
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100) 'Name: '
      WRITE(Iun,'(A)',ERR=100) 'Address: '
      WRITE(Iun,'(A)',ERR=100) '         '
      WRITE(Iun,'(A)',ERR=100) '         '
      WRITE(Iun,'(A)',ERR=100) '         '
      WRITE(Iun,'(A)',ERR=100) 'You should send the completed contents of this file to support@ccdc.cam.ac.uk'
      WRITE(Iun,*,ERR=100)
      CLOSE(iun,iostat=idummy)
      CALL WMessageBox(YesNo,InformationIcon,CommonYes,&
        "A file "//Fname(1:LEN_TRIM(Fname))//" has been created."//CHAR(13)//&
        "You should edit this file and then send it to"//CHAR(13)//CHAR(13)//&
        "support@ccdc.cam.ac.uk"//CHAR(13)//CHAR(13)//&
        "Would you like to edit this file now?","Edit licence request file")
      IF (WinfoDialog(4) .EQ. 1) THEN
        CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,'Edit licence request file'),IHan)
        CALL WEditFile(Fname(1:LEN_TRIM(Fname)), Modal, 0, 0, SystemFixed)
      ENDIF
      RETURN
   99 CONTINUE
      CALL ErrorMessage("Sorry, could not open the file "//CHAR(13)//fname(1:LEN_TRIM(Fname)))
      CLOSE(iun,iostat=idummy)
      RETURN            
  100 CONTINUE
      CALL ErrorMessage("Sorry, could not write to the file "//CHAR(13)//fname(1:LEN_TRIM(Fname)))
      CLOSE(iun,iostat=idummy)      

      END SUBROUTINE WriteLicenceRequestForm
!
!*****************************************************************************
!
      INTEGER FUNCTION Get_DashSerialNumber( lpszDriveName )

      USE DFWIN

      IMPLICIT NONE

      CHARACTER*(*)   lpszDriveName
      CHARACTER*100   lpszSystemName
      INTEGER(4)      lpszSerialNumber
      INTEGER(4)      nSystemNameSize 
      INTEGER         Mangler
      PARAMETER (Mangler = 149355525)

      LOGICAL(4)      bRC
      INTEGER*4       ret
      CHARACTER*50    Volume

      nSystemNameSize   = 100
      lpszSerialNumber    = 1
      ret = lstrcpy(lpszSystemName, "                               "C)
      bRC = GetVolumeInformation(                            &
                           lpszdrivename,                    &
                           Volume,                           &
                           50,                               &
                           LOC(lpszSerialNumber),            &
                           NULL,                             &
                           NULL,                             &
                           lpszSystemName,                   &
                           nSystemNameSize)
      Get_DashSerialNumber = IEOR(lpszSerialNumber,Mangler)

      END FUNCTION Get_DashSerialNumber
!
!*****************************************************************************
!
      SUBROUTINE ShowLicenceAgreement(Info)
!
! RETURNS : 1 = I do NOT agree
!           2 = I agree
!           3 = I want to enter a new key
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      TYPE(License_Info) Info

      CHARACTER*5000 kString
      CHARACTER*4 NextLine
      CHARACTER*18 tDateStr
      INTEGER  iOption, tLen

! Initialise to failure
      Info%Valid = -5
      NextLine = CHAR(13)//CHAR(10)//CHAR(13)//CHAR(10)
! Convert expiry date to a string
      CALL Date2String(Info%ExpiryDate,tDateStr,tLen)
      kString = 'In order to run this evaluation version of DASH, you must first read and agree to the '// &
                'terms of the following licence agreement:'//NextLine// &
                'DASH (the "Program") is a copyright work belonging to CCDC Software Limited.  In '// &
                'consideration of the access to the Program granted you, you agree to run and use the '// &
                'Program solely in accordance with the following terms.'//NextLine// &
                'You are permitted to run and to use the Program and the documentation'// &
                ' until '//tDateStr(1:tLen)//' for the purpose of evaluating whether or not the software '// &
                'meets your requirements. Within 14 days of this date you agree to delete '// &
                'all copies of the Program from your computers and storage systems unless an extension '// &
                'of the evaluation period is granted by CCDC Software Limited.'//NextLine// &
                'You may not supply, assign, transfer or sublicense (in whole or part) the Program to any'// &
                ' third party as part of a commercial transaction or for any consideration, in money,'// &
                ' money''s worth or otherwise, or free of charge.  The Program shall only be accessible'// &
                ' to your employees.'//NextLine// &
                'You may not bundle this Program together with any other software product or products'// &
                ' without the prior written consent of CCDC Software Limited.'//NextLine// &
                'You may copy the Program only to the extent strictly necessary for evaluation and'// &
                ' back-up purposes. Subject thereto or as otherwise expressly permitted by applicable'// &
                ' law, you may not copy, reproduce, translate, adapt, decompile, modify, reverse'// &
                ' engineer or disassemble the Program. You shall ensure at all times that all'// &
                ' copies of the Programs made by you contain the copyright notice issued by CCDC'// &
                ' and contained in the Program.  You shall not amend or obscure this notice or'// &
                ' any logos or trademarks of CCDC contained in the Program.'//NextLine// &
                'THE PROGRAM IS SUPPLIED TO YOU WITHOUT CHARGE, AND ACCORDINGLY YOU AGREE THAT THE'// &
                ' PROGRAM IS PROVIDED ON AN *AS IS* BASIS, AND NO REPRESENTATION IS MADE OR WARRANTY'// &
                ' GIVEN, WHETHER WITH REGARD TO THE FUNCTIONALITY OR FITNESS FOR PURPOSE OF THE'// &
                ' PROGRAM OR OTHERWISE, AND ALL SUCH REPRESENTATIONS AND WARRANTIES, WHETHER '// &
                'EXPRESSED OR IMPLIED (BY LAW OR OTHERWISE) ARE HEREBY EXCLUDED TO THE FULLEST'// &
                ' EXTENT PERMITTED BY LAW. WITHOUT PREJUDICE TO THE FOREGOING IN NO EVENT SHALL'// &
                ' CCDC SOFTWARE LIMITED BE LIABLE TO YOU, IN CONTRACT, IN TORT OR OTHERWISE,'// &
                ' FOR ANY INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY OR CONSEQUENTIAL LOSS OR DAMAGE,'// &
                ' INCLUDING, WITHOUT LIMITATION, ADMINISTRATION COSTS, LOSS OF BUSINESS AND GOODWILL,'// &
                ' LOSS UNDER CURRENT AND FUTURE CONTRACTS, LOSS OF PROFIT OR OPPORTUNITY OR FINANCIAL'// &
                ' LOSS OF ANY KIND ARISING IN ANY WAY OUT OF OR IN CONNECTION WITH YOUR USE OF THE PROGRAM.'//NextLine// &
                'You agree to minimise any adverse effect of downloading, installing and using the'// &
                ' Program, including by keeping back-up data and implementing adequate disaster '// &
                'recovery procedures.  Accordingly, CCDC Software Limited shall be in no manner '// &
                'liable for any effect which the Program may have on your data, software, hardware'// &
                ' or other systems or products.'//NextLine// &
                'No amendment, variation or discharge of these terms and conditions is valid '// &
                'unless accepted in writing by both parties.'//NextLine// &
                'The failure of either party to exercise or enforce any rights under these terms'// &
                ' and conditions shall not amount to a waiver of those rights.'//NextLine// &
                'The illegality or invalidity of any part of these terms and conditions shall not'// &
                ' affect the legality or validity of the remainder of them.'//NextLine// &
                'These terms and conditions are not intended to confer rights on any third party,'// &
                ' whether pursuant to the Contracts (Rights of Third Parties) Act 1999 or otherwise,'// &
                ' and no third party shall have any right to enforce any provision of these terms'// &
                ' and conditions.'//NextLine// &
                'The foregoing terms and conditions and any dispute in connection with them shall be'// &
                ' governed by and construed in accordance with English law and shall be subject to the'// &
                ' exclusive jurisdiction of the English courts.'//NextLine// &
                'If you agree to the foregoing terms and conditions then please select the "I'// &
                ' have read the full text above and I Agree" option below. If you do not agree'// &
                ' to the foregoing terms and conditions you should select the "I do NOT Agree"'// &
                ' option below. After making your selection, please click OK to proceed.'
      CALL WDialogSelect(IDD_LicenceAgreement)
      CALL WDialogPutString(IDF_Agreement,kString)
      CALL WDialogShow(-1,-1,0,SemiModeless)
      DO WHILE (.TRUE.)
        CALL GetEvent
        SELECT CASE (EventType)
          CASE (PushButton) ! one of the buttons was pushed
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDCANCEL)
                CALL DoExit
              CASE (IDB_Here)
                Info%Valid = -5
              CASE (IDOK)
                CALL WDialogGetRadioButton(IDF_IDoNotAgree,iOption)
                SELECT CASE (iOption)
                  CASE (1)
                    CALL DoExit
                  CASE (2)
                    Info%Valid =  1
                  CASE (3)
                    Info%Valid = -5
                END SELECT
            END SELECT
            CALL WDialogHide
            RETURN
        END SELECT
      ENDDO

      END SUBROUTINE ShowLicenceAgreement
!
!*****************************************************************************
!
