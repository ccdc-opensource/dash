!
!*****************************************************************************
!
       SUBROUTINE check_license
       USE WINTERACTER
       USE DRUID_HEADER
       USE VARIABLES

       INTEGER valid_license
       INTEGER Read_License_Valid
       CHARACTER*2 Exp

!      valid_license = 2
!      CALL DATE_AND_TIME (DATEINFO (1), DATEINFO (2), DATEINFO (3), TIMEDATA)
! Check the year
!      IF (TIMEDATA(1) .GT. EXPIRY_YEAR) THEN
!           valid_license = 0
!      ELSE IF (TIMEDATA(1) .EQ. EXPIRY_YEAR) THEN 
! Take it further - the year is the same so check the month
!           IF (TIMEDATA(2) .GT. EXPIRY_MONTH) THEN
!                 valid_license = 0
!           ELSE IF (TIMEDATA(2) .EQ. EXPIRY_MONTH) THEN
! Take it further - the month is the same so check the day
!                 IF (TIMEDATA(3) .GT. EXPIRY_DAY) THEN
!                       valid_license = 0
!                 ELSE 
!                       ndays = EXPIRY_DAY - TIMEDATA(3)
!                       IF (ndays .LE. 7) THEN
!                             valid_licence = 1
!                       END IF
!                 END IF
!           END IF
!      END IF
      valid_license = 0
      DO WHILE (valid_license .LE. 0) 
        valid_license = Read_License_Valid()
        IF (valid_license .LE. -2) THEN
          CALL WMessageBox(OkCancel,StopIcon,CommonOk, &
            "DASH problem: could not find or open the license file"//CHAR(13)//&
            INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//"License.dat"//CHAR(13)//CHAR(13)//&
            "Would you like to enter a new license?",&
            "Missing license file")
        ELSE IF (valid_license .EQ. -1) THEN
          CALL WMessageBox(OkCancel,StopIcon,CommonOk, &
            "DASH problem: Your DASH license is invalid for this machine"//CHAR(13)//&
            "Would you like to enter a new license?",&      
            "Invalid or expired license")
        ELSE IF (valid_license .EQ. 0) THEN
          CALL WMessageBox(OkCancel,StopIcon,CommonOk, &
            "DASH problem: Your DASH license has expired"//CHAR(13)//&
            "Would you like to enter a new license?",&      
            "Invalid or expired license")
        END IF
        IF (valid_license .LE. 0) THEN
          IF (WinfoDialog(4) .EQ. 1) THEN
            CALL LicensePopup()
          ELSE
            CALL WExit
          END IF
        END IF
      END DO
      IF (valid_license .LE. 7) THEN
        WRITE(Exp,'(I2)') valid_license
        CALL WMessageBox(OKOnly,InformationIcon,CommonOk, &
          "Information: Your DASH license will expire in "//Exp//" days", &
          "Soon-to-expire licence")
      ENDIF
! JvdS Now we can remove the licence dialogue from memory:
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogUnload

      END SUBROUTINE check_license
!
!*****************************************************************************
!
      SUBROUTINE LicensePopup

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      LOGICAL           :: INLOOP = .TRUE.
      INTEGER       ISite, Valid, ICode
      CHARACTER*255 ClString
      TYPE (License_Info) Info

      Info%Valid = 0
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogShow(-1,-1,0,SemiModeless)
      CALL WDialogFieldState(ID_Enter_License,Enabled)
      CALL WMessageEnable(FieldChanged,Enabled)
      CALL WDialogGetCheckBox(IDF_License_Site,ISite)
      IF (Isite .EQ. 1) THEN
        CALL WDialogFieldState(IDF_License_SiteCode,Enabled)
        CALL WDialogFieldState(IDF_License_SiteCodeLabel,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_License_SiteCode,Disabled)
        CALL WDialogFieldState(IDF_License_SiteCodeLabel,Disabled)
      END IF
      DO WHILE(INLOOP)
        CALL GetEvent
        SELECT CASE (EventType)
          CASE (PushButton)
            SELECT CASE(EventInfo%VALUE1)
              CASE (ID_Licensing_Exit)
                CALL DoExit()
              CASE (IDCANCEL)
                CALL WExit
              CASE (ID_Enter_License)
                CALL WDialogGetString(IDF_License_String, CLString)
                CALL Decode_License(CLString,Info)
                IF (Info%Valid .LT. 0 ) THEN
                  CALL ErrorMessage("Sorry, the license key is invalid - please check your input.")
                ELSE IF ((ISite .EQ. 1) .AND. (Info%LicenseType .NE. SiteKey)) THEN
                  CALL ErrorMessage("Sorry, the license key is not a site license.")
                ELSE IF ((ISite .EQ. 0) .AND. (Info%LicenseType .EQ. SiteKey)) THEN
                  CALL ErrorMessage("The license key is a site license: Please select"//&
                                    " the Site License check-box and enter your site code as well.")                                             
                ELSE
                  Valid = License_Valid(Info)
                  IF (ISite .EQ. 1) THEN
                    CALL WDialogGetInteger(IDF_License_SiteCode, ICode) 
                    IF (Info%SerialNumber .NE. ICode) THEN
                      Valid = -99
                    END IF
                  END IF
                  IF (Valid .GT. 0) THEN
                    CALL Write_License_File(CLString)
                    INLOOP = .FALSE.
                  ELSE IF (Valid .EQ. 0) THEN
                    CALL ErrorMessage("Sorry, the license key has expired.")
                  ELSE IF (Valid .EQ. -1) THEN
                    CALL ErrorMessage("Sorry, the license key is not valid for this machine.")
                  ELSE IF (Valid .EQ. -99) THEN
                    CALL ErrorMessage("Sorry, the license key is not valid for this site.") 
                  END IF
                END IF
              CASE (ID_Licence_Request)
                CALL Write_License_Request_Form()
                CALL WExit
            END SELECT
          CASE (CloseRequest)
            CALL WExit
          CASE (FieldChanged)
            CALL WDialogGetCheckBox(IDF_License_Site,ISite)
            IF (Isite .EQ. 1) THEN
              CALL WDialogFieldState(IDF_License_SiteCode,Enabled)
              CALL WDialogFieldState(IDF_License_SiteCodeLabel,Enabled)
            ELSE
              CALL WDialogFieldState(IDF_License_SiteCode,Disabled)
              CALL WDialogFieldState(IDF_License_SiteCodeLabel,Disabled)
            END IF
        END SELECT
      END DO
! JvdS Why disable FieldChanged?
      CALL WMessageEnable(FieldChanged,Disabled)
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogHide()

      END SUBROUTINE LicensePopup
!
!*****************************************************************************
!
      SUBROUTINE Decode_License(LString,Info)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*) LString
      TYPE (License_Info) Info
      INTEGER v(2), w(2), k(4), cs
      INTEGER*2 tCheckSum
      EQUIVALENCE (tCheckSum,cs)
      INTEGER*2 checksum

      k(1) = 2453
      k(2) = 1768
      k(3) = 4567
      k(4) = 1453
      Info%Valid = 1
! JvdS Next lines very dirty: v is INTEGER*4, but their XOR is INTEGER*2. Not possible.
      READ(Lstring,'(2z8,z4)',err = 99) v(1), v(2), checksum
      cs = IEOR(v(1),v(2))
      IF (tCheckSum .NE. checksum) GOTO 99
! Check the checksum
      CALL decipher(v,w,k)
      Info%SerialNumber =  w(1)
      Info%LicenseType = w(2)/100000000
      Info%DateCode = w(2) - Info%LicenseType*100000000
      Info%Year     = Info%DateCode/10000
      Info%Month    = (Info%DateCode - Info%Year*10000)/100
      Info%Day      = (Info%DateCode - Info%Year*10000 - Info%Month*100)
      IF (Info%LicenseType .EQ. SiteKey) THEN
        Info%SerialNumber = Info%SerialNumber - 145789123 ! demangle into a site number
      END IF 
      RETURN 
 99   CONTINUE
      Info%Valid = -1
      RETURN

      END SUBROUTINE Decode_License
!
!*****************************************************************************
!
      INTEGER Function Read_License_Valid()

      USE VARIABLES  

      CHARACTER*80 line, CLString
      INTEGER      dummy

      TYPE(License_Info) Info

      Read_License_Valid = -2
      OPEN(UNIT=117,&
        file=INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'License.dat',&
        STATUS='OLD',ERR=99)
      DO WHILE (Read_License_Valid .LE. 0)
        READ(117,'(A)',err=99,END=99) line
        IF (line(1:1) .NE. '#') THEN
          CALL INextString(line,clstring)
          CALL Decode_License(CLString,Info)
          IF (Info%Valid) THEN
            Read_License_Valid = License_Valid(Info)
          END IF
        END IF
      END DO
! Have a decodable key ...
 99   CONTINUE
      CLOSE(117,iostat=dummy)
      RETURN

      END FUNCTION Read_License_Valid
!
!*****************************************************************************
!
      INTEGER FUNCTION License_Valid(Info)

      USE VARIABLES

      INTEGER      today, snum
      INTEGER      Get_DashSerialNumber
      CHARACTER*8  dt
      TYPE(License_Info) Info

! Check the date
      License_Valid = 0
      CALL DATE_AND_TIME(dt)
      READ(dt,*,ERR=99) today
      License_Valid = MAX (0, Info%DateCode - today)

! For node-locked licenses check the serial id. Site-Wide licenses just encode a serial id for our reference
! so if we catch any non-authorized users using the key, we know where it came from. Perhaps we may want to make
! the user key in this site code on installation for checking purposes.

      IF (Info%LicenseType .EQ. NodeKey) THEN
        snum = Get_DashSerialNumber("C:\\"C)
        IF (snum .NE. Info%SerialNumber) THEN
          License_Valid = -1
        END IF
      END IF       
 99   RETURN

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
      OPEN(FILE=INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'License.dat', &
           UNIT=IUN,&
           STATUS='UNKNOWN',&
           ERR=99)
      WRITE(iun,'(A)',ERR=99)         "# License File for DASH"
      WRITE(iun,'(A)',ERR=99)         "#"
      WRITE(iun,'(A,A,A)',ERR=99) '# This is a ',Ctypestr(1:LEN_TRIM(Ctypestr)),' license '

! JvdS @ The following lines look weird: the second statement cannot be reached.
! The second IF should probably read:  ELSE IF (Info%LicenseType .EQ. SiteKey) THEN
      IF      (Info%LicenseType .EQ. NodeKey) THEN
        WRITE(iun,'(A,z8)',ERR=99) '# Your DASH Serial ID for this machine is ',Info%SerialNumber
      ELSE IF (Info%LicenseType .EQ. NodeKey) THEN
        WRITE(iun,'(A,z8)',ERR=99) '# Your DASH Site ID is ',Info%SerialNumber
      END IF

      IF (Info%Year .EQ. 9999) THEN
        WRITE(iun,'(A)',ERR=99)'# The license is non-expiring'
      ELSE
        WRITE(iun,2,ERR=99)Info%Day, Months(Info%Month),Info%Year
  2     FORMAT('# The license expires on ',i3,1x,A,1x,i4)
      END IF
      WRITE(iun,'(A)',ERR=99)"# License key follows :"
      WRITE(iun,'(A)',ERR=99) LString
 99   CONTINUE
      CLOSE(iun)
      RETURN

      END SUBROUTINE Write_License_File
!
!*****************************************************************************
!
      SUBROUTINE Write_License_Request_Form()

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
! JvdS Added
      Idummy = 1
      CALL WSelectFile(fstr,IFlags,Fname,"Please enter a filename",Idummy)
      IF (Fname(1:1) .EQ. ' ') RETURN
      OPEN(unit = Iun, file = Fname(1:LEN_TRIM(Fname)),status = 'unknown',err=99)
      Sn = Get_DashSerialNumber("C:\\"C)
      WRITE(Iun,'(A)',ERR=100)'This file is provided to submit requests for DASH licenses'
      WRITE(Iun,'(A)',ERR=100)'A DASH evaluation  will allow you to run DASH on any PC'
      WRITE(Iun,'(A)',ERR=100)'A site license will allow you to install DASH on any PC on your own site'
      WRITE(Iun,'(A)',ERR=100)'Most licenses, however, are node-locked. For this, we use a unique identifier'
      WRITE(Iun,'(A,z8)',ERR=100)'For this PC, this is ',Sn
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100)'Please complete as applicable:'
      WRITE(Iun,*)
      WRITE(Iun,'(A)',ERR=100)'I would like to evaluate/purchase DASH'
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100)'I work in industry/an academic institution'
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100)'Please enter your address here: '
      WRITE(Iun,*,ERR=100)
      WRITE(Iun,'(A)',ERR=100)'Name: '
      WRITE(Iun,'(A)',ERR=100)'Address: '
      WRITE(Iun,'(A)',ERR=100)'         '
      WRITE(Iun,'(A)',ERR=100)'         '
      WRITE(Iun,'(A)',ERR=100)'         '
      WRITE(Iun,'(A)',ERR=100)'You should send the completed contents of this file to software@ccdc.cam.ac.uk'
      WRITE(Iun,*,ERR=100)
      CLOSE(iun,iostat=idummy)
      CALL WMessageBox(YesNo,InformationIcon,CommonYes,&
        "A file "//Fname(1:LEN_TRIM(Fname))//" has been created."//CHAR(13)//&
        "You should edit this file and then send it to"//CHAR(13)//CHAR(13)//&
        "software@ccdc.cam.ac.uk"//CHAR(13)//CHAR(13)//&
        "Would you like to edit this file now?","Edit license request file")
      IF (WinfoDialog(4) .EQ. 1) THEN
        CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,&
          'Edit license request file'),IHan)
        CALL WEditFile(Fname(1:LEN_TRIM(Fname)), Modal, 0, 0, SystemFixed)
      END IF
      RETURN
 99   CONTINUE
      CALL ErrorMessage("Sorry, could not open the file "//CHAR(13)//fname(1:LEN_TRIM(Fname)))
      CLOSE(iun,iostat=idummy)
      RETURN            
 100  CONTINUE
      CALL ErrorMessage("Sorry, could not write to the file "//CHAR(13)//fname(1:LEN_TRIM(Fname)))
      CLOSE(iun,iostat=idummy)      

      END SUBROUTINE Write_License_Request_Form
!
!*****************************************************************************
!
subroutine decipher(v,w,k)


integer, intent(in)     ::  v(2)
integer, intent(out)    ::  w(2)
integer, intent(in)     ::  k(4)

integer y,  z
integer a, b, c, d
integer :: n 
integer :: sum
integer :: delta = 16#9E3779B9

sum = 16#C6EF3720
n = 32

y = v(1)
z = v(2)

a = k(1)
b = k(2)
c = k(3)
d = k(4)

do while ( n .gt. 0)
      n = n - 1
      z = z - (ishft(y,4)) - ieor(c,y) - ieor(sum,ishft(y,-5)) - d
      y = y - (ishft(z,4)) - ieor(a,z) - ieor(sum,ishft(z,-5)) - b
      sum = sum - delta
end do
w(1) = y
w(2) = z
return
end subroutine decipher
!
!*****************************************************************************
!
     integer function Get_DashSerialNumber( lpszDriveName )
     use dfwin
     character*(*)   lpszDriveName
     character*100   lpszSystemName
     integer(4)      lpszSerialNumber
       integer(4)      nSystemNameSize 
       integer         Mangler
       parameter (Mangler = 149355525)

     logical(4)      bRC
     integer*4       ret
     character*50    Volume
       nSystemNameSize     = 100
!     lpszSystemName      = lpszSystemName
     lpszSerialNumber    = 1


     ret = lstrcpy(lpszSystemName, "                               "C)
     bRC = GetVolumeInformation(                                       &
                           lpszdrivename,                              &
                           Volume,                                     &
                           50,                                         &
                           loc(lpszSerialNumber),                      &
                           NULL,                                       &
                           NULL,                                       &
                           lpszSystemName,                             &
                           nSystemNameSize)

      Get_DashSerialNumber  = ieor(lpszSerialNumber,Mangler)
    return
      end
!
!*****************************************************************************
!
