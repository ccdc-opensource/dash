!
!*****************************************************************************
!
      SUBROUTINE check_licence

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INTEGER valid_license
      INTEGER, EXTERNAL :: Read_License_Valid
      CHARACTER*2 Exp
      CHARACTER*200 MessageStr
      LOGICAL, EXTERNAL :: Confirm

      valid_license = 0
      DO WHILE (valid_license .LE. 0) 
        valid_license = Read_License_Valid()
        IF      (valid_license .EQ. -7) THEN
          MessageStr = "Demo licence not valid."
        ELSE IF (valid_license .LE. -2) THEN
          MessageStr = "DASH problem: could not find or open the licence file"//CHAR(13)//&
            InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//"License.dat."
        ELSE IF (valid_license .EQ. -1) THEN
          MessageStr = "DASH problem: Your DASH licence is invalid for this machine."
        ELSE IF (valid_license .EQ.  0) THEN
          MessageStr = "DASH problem: Your DASH licence has expired."
        ENDIF
        IF (valid_license .LE. 0) THEN
          MessageStr = MessageStr(1:LEN_TRIM(MessageStr))//CHAR(13)//&
                       "Would you like to enter a new licence?"
          IF (Confirm(MessageStr)) THEN
            CALL LicensePopup
          ELSE
            CALL DoExit
          ENDIF
        ENDIF
      ENDDO
      IF (valid_license .LE. 7) THEN
        WRITE(Exp,'(I2)') valid_license
        CALL InfoMessage("Your DASH licence will expire in "//Exp//" days.")
      ENDIF
! Now we can remove the licence dialogue from memory:
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogUnload

      END SUBROUTINE check_licence
!
!*****************************************************************************
!
      SUBROUTINE LicensePopup

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      LOGICAL    :: INLOOP
      INTEGER     Valid, ICode
      CHARACTER*MaxPathLength ClString
      TYPE (License_Info) Info
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: DateToday

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
                CALL Decode_License(CLString,Info)
                IF (Info%Valid .LT. 0 ) THEN
                  CALL ErrorMessage("Sorry, the licence key is invalid--please check your input.")
                ELSE IF (WDialogGetCheckBoxLogical(IDF_License_Site) .AND. (Info%LicenseType .NE. SiteKey)) THEN
                  CALL ErrorMessage("Sorry, the licence key is not a site licence.")
                ELSE IF ((.NOT. WDialogGetCheckBoxLogical(IDF_License_Site)) .AND. (Info%LicenseType .EQ. SiteKey)) THEN
                  CALL ErrorMessage("The licence key is a site licence:"//CHAR(13)//&
                                    "please select the Site Licence check-box and enter your site code as well.")
                ELSE
                  Valid = License_Valid(Info)
                  IF (WDialogGetCheckBoxLogical(IDF_License_Site)) THEN
                    CALL WDialogGetInteger(IDF_License_SiteCode, ICode) 
                    IF (Info%SerialNumber .NE. ICode) Valid = -99
                  ENDIF
                  IF (Valid .GT. 0) THEN
                    CALL Write_License_File(CLString)
                    INLOOP = .FALSE.
                  ELSE IF (Valid .EQ. 0) THEN
                    CALL ErrorMessage("Sorry, the licence key has expired.")
                  ELSE IF (Valid .EQ. -1) THEN
                    CALL ErrorMessage("Sorry, the licence key is not valid for this machine.")
                  ELSE IF (Valid .EQ. -99) THEN
                    CALL ErrorMessage("Sorry, the licence key is not valid for this site.") 
                  ENDIF
                ENDIF
              CASE (ID_Licence_Request)
                CALL Write_License_Request_Form
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

      END SUBROUTINE LicensePopup
!
!*****************************************************************************
!
      SUBROUTINE decipher(v,w)

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
      SUBROUTINE Decode_License(LString,Info)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*) LString
      TYPE (License_Info) Info
      INTEGER v(2), w(2), cs
      INTEGER*2 tCheckSum
      EQUIVALENCE (tCheckSum,cs)
      INTEGER*2 checksum

      Info%Valid = 1
! JvdS Next lines very dirty: v is INTEGER*4, but their XOR is INTEGER*2. Not possible.
      READ(LString,'(2Z8,Z4)',ERR = 99) v(1), v(2), checksum
      cs = IEOR(v(1),v(2))
! Check the checksum
      IF (tCheckSum .NE. checksum) GOTO 99
      CALL decipher(v,w)
      Info%SerialNumber = w(1)
      Info%LicenseType  = w(2)/100000000
      Info%DateCode     = w(2) - Info%LicenseType*100000000
      Info%Year         =  Info%DateCode/10000
      Info%Month        = (Info%DateCode - Info%Year*10000)/100
      Info%Day          = (Info%DateCode - Info%Year*10000 - Info%Month*100)
      IF (Info%LicenseType .EQ. SiteKey) Info%SerialNumber = Info%SerialNumber - 145789123 ! demangle into a site number
      RETURN 
 99   CONTINUE
      Info%Valid = -1

      END SUBROUTINE Decode_License
!
!*****************************************************************************
!
      INTEGER FUNCTION Read_License_Valid()

      USE VARIABLES  

      CHARACTER*80 line, CLString
      INTEGER      dummy
      TYPE(License_Info) Info
      INTEGER, EXTERNAL :: ShowLicenceAgreement
      INTEGER tRead_License_Valid, ttRead_License_Valid

      Read_License_Valid = -2
      OPEN(UNIT=117,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'License.dat',STATUS='OLD',ERR=99)
      DO WHILE (Read_License_Valid .LE. 0)
        READ(117,'(A)',ERR=99,END=99) line
        IF (line(1:1) .NE. '#') THEN
          CALL INextString(line,clstring)
          CALL Decode_License(CLString,Info)
          IF (Info%Valid) THEN
            tRead_License_Valid = License_Valid(Info)
            IF (tRead_License_Valid .GT. 0) THEN
              IF (Info%LicenseType .EQ. DemoKey) THEN
                ttRead_License_Valid = ShowLicenceAgreement(Info)
                IF (ttRead_License_Valid .EQ. -7) tRead_License_Valid = -7
              ENDIF
            ENDIF
            Read_License_Valid = tRead_License_Valid
          ENDIF
        ENDIF
      ENDDO
! Have a decodable key ...
 99   CONTINUE
      CLOSE(117,iostat=dummy)

      END FUNCTION Read_License_Valid
!
!*****************************************************************************
!
      INTEGER FUNCTION License_Valid(Info)

      USE VARIABLES

      TYPE(License_Info)   Info

      INTEGER, EXTERNAL :: DateToday
      INTEGER              snum
      INTEGER              Get_DashSerialNumber

! Check the date
      License_Valid = MAX(0, Info%DateCode - DateToday())
! For node-locked licenses check the serial id. Site-Wide licenses just encode a serial id for our reference
! so if we catch any non-authorized users using the key, we know where it came from. Perhaps we may want to make
! the user key in this site code on installation for checking purposes.
      IF (Info%LicenseType .EQ. NodeKey) THEN
        snum = Get_DashSerialNumber("C:\\"C)
        IF (snum .NE. Info%SerialNumber) License_Valid = -1
      ENDIF       

      END FUNCTION License_Valid 
!
!*****************************************************************************
!
      SUBROUTINE Write_License_File(LString)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INTEGER        IUN
      PARAMETER (IUN = 117)
      CHARACTER*(*)  LString
      CHARACTER*8    Months(12)
      CHARACTER*11   Ctypestr
      DATA Months / 'January', 'February', 'March', 'April', &
                    'May', 'June', 'July', 'August', &
                    'September', 'October', 'November', 'December' /
      TYPE (License_Info) Info

      CALL Decode_License(LString,Info)
      IF (Info%Valid .LE. 0) GOTO 99
      SELECT CASE ( Info%LicenseType ) 
        CASE (DemoKey)
          Ctypestr = 'Demo'
        CASE (NodeKey)
          Ctypestr = 'Node Locked'
        CASE (SiteKey)
          Ctypestr = 'Site'
        CASE DEFAULT
          GOTO 99
      END SELECT
      OPEN(UNIT=IUN,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'License.dat',STATUS='UNKNOWN',ERR=99)
      WRITE(iun,'(A)',ERR=99)     "# Licence File for DASH"
      WRITE(iun,'(A)',ERR=99)     "#"
      WRITE(iun,'(A,A,A)',ERR=99) '# This is a ',Ctypestr(1:LEN_TRIM(Ctypestr)),' licence '
      IF      (Info%LicenseType .EQ. NodeKey) THEN
        WRITE(iun,'(A,z8)',ERR=99) '# Your DASH Serial ID for this machine is ',Info%SerialNumber
      ELSE IF (Info%LicenseType .EQ. SiteKey) THEN
        WRITE(iun,'(A,z8)',ERR=99) '# Your DASH Site ID is ',Info%SerialNumber
      ENDIF
      IF (Info%Year .EQ. 9999) THEN
        WRITE(iun,'(A)',ERR=99)'# The licence is non-expiring'
      ELSE
        WRITE(iun,2,ERR=99) Info%Day, Months(Info%Month), Info%Year
    2   FORMAT('# The licence expires on ',I3,1X,A,1X,I4)
      ENDIF
      WRITE(iun,'(A)',ERR=99)"# Licence key follows :"
      WRITE(iun,'(A)',ERR=99) LString(1:LEN_TRIM(LString))
   99 CONTINUE
      CLOSE(iun)

      END SUBROUTINE Write_License_File
!
!*****************************************************************************
!
      SUBROUTINE Write_License_Request_Form

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

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

      END SUBROUTINE Write_License_Request_Form
!
!*****************************************************************************
!
      INTEGER FUNCTION Get_DashSerialNumber( lpszDriveName )

      USE DFWIN

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
      INTEGER FUNCTION ShowLicenceAgreement(Info)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      TYPE(License_Info) Info

      CHARACTER*5000 kString
      CHARACTER*4 NextLine
      CHARACTER*18 tDateStr
      INTEGER tDate, iOption

! Initialise to failure
      ShowLicenceAgreement = -7
      NextLine = CHAR(13)//CHAR(10)//CHAR(13)//CHAR(10)
! Convert expiry date to a string
      tDate = Info%Year*10000 + Info%Month*100 + Info%Day
      CALL Date2String(tDate,tDateStr)
      kString = 'In order to run this evaluation version of DASH, you must first read and agree to the '// &
                'terms of the following licence agreement:'//NextLine// &
                'DASH (the "Program") is a copyright work belonging to CCDC Software Limited.  In '// &
                'consideration of the access to the Program granted you, you agree to run and use the '// &
                'Program solely in accordance with the following terms.'//NextLine// &
                'You are permitted to run and to use the Program and the documentation for a period of 60'// &
                ' days until '//tDateStr(1:LEN_TRIM(tDateStr))//' for the purpose of evaluating whether or not the software '// &
                'meets your requirements. Within 14 days of the end of such period you agree to delete '// &
                'all copies of the Program from your computers and storage systems.'//NextLine// &
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
                ShowLicenceAgreement = -7
                CALL WDialogHide
                RETURN
              CASE (IDOK)
                CALL WDialogGetRadioButton(IDF_IDoNotAgree,iOption)
                IF (iOption .EQ. 2) THEN
                  ShowLicenceAgreement = 0
                ELSE
                  ShowLicenceAgreement = -7
                ENDIF
                CALL WDialogHide
                RETURN
            END SELECT
        END SELECT
      ENDDO

      END FUNCTION ShowLicenceAgreement
!
!*****************************************************************************
!
