!  ENCODE.f90 
!
!  FUNCTIONS:
!	ENCODE      - Entry point of console application.
!
!	Encodes license keys for DASH
!

!****************************************************************************
!
!  PROGRAM: ENCODE
!
!  PURPOSE:	Encodes license keys for DASH
!
!****************************************************************************

	program ENCODE
	use dfwin

	implicit none

    integer ExpiryDay
	integer ExpiryMonth
	integer ExpiryYear
	real VersionNumber
	integer KeyType 
	integer SerialNumber
	integer*2 CheckSum


	integer args(4)
	integer*2 stat
	integer v(2), w(2), i, ch1, ch2, nch, Dcode
	character*40 lstring
	character*8  CSerial
	character*4  TypeKey

	integer NodeKey
	integer DemoKey
	integer SiteKey
	parameter (NodeKey = 1)
	parameter (DemoKey = 2)
	parameter (SiteKey = 3)

	integer k(4)

	k(1) = 2453
	k(2) = 1768
	k(3) = 4567
	k(4) = 1453

	call getarg(1,CSerial,stat)
	if (stat .eq. -1) goto 99
	read(CSerial,*,err = 99) ExpiryDay

	call getarg(2,CSerial,stat)
	if (stat .eq. -1) goto 99
	read(CSerial,*,err = 99) ExpiryMonth
	IF (ExpiryYear .GT. 99) goto 99

	call getarg(3,CSerial,stat)
	if (stat .eq. -1) goto 99 
	read(CSerial,*,err = 99) ExpiryYear

	call getarg(4,CSerial,stat)
	if (stat .eq. -1) goto 99
	read(CSerial,'(z8)',err = 99) SerialNumber

    call getarg(5,CSerial,stat)
	if (stat .eq. -1) goto 99
	read(CSerial,*,err = 99) VersionNumber

	call getarg(6,CSerial,stat)

    if (stat .eq. -1) THEN
		KeyType = NodeKey  ! Just a node-locked license
	else if ( CSerial(1:1) .EQ. 'D' .OR. CSerial(1:1) .EQ. 'd' ) then
		KeyType = DemoKey ! a demo license
	else if ( CSerial(1:1) .EQ. 'S' .OR. CSerial(1:1) .EQ. 's' ) then
		KeyType = SiteKey  ! a site license

		call getarg(4,CSerial,stat)
		read(CSerial,'(i)',err = 99) SerialNumber
		SerialNumber = SerialNumber + 145789123 ! Mangle the site number
	end if

	v(1) = SerialNumber
	v(2) = KeyType*100000000 + ExpiryYear*10000 + ExpiryMonth*100 + ExpiryDay

	call encipher(v,w,k)
    checksum = ieor(w(1),w(2))

	write(lstring,'(2z8.8,z4.4)') w(1), w(2), checksum

	write (*,'(A)')'The Licence key string is '
	write (*,'(A)')  lstring
	write (*,'(A)') 'Checking ...'

    call WriteLicenceFile(LString,6)
 
   goto 10
99 continue
   write (*,*) 'Input invalid; Usage: encode <day> <month> <year> <SerialNumber|SiteNumber> <Version Number> [DEMO|SITE]'
10 continue
   end program ENCODE


    subroutine WriteLicenceFile(LString,unit)
    integer unit
    character*(*)LString
    integer v(2),w(2), k(4)
    character*8 Months(12)
    data Months / 'January','February',   'March',   'April', &
	                  'May',    'June',    'July',  'August', &
			    'September', 'October','November','December'/
    integer ExpiryYear
	integer ExpiryMonth
	integer ExpiryDay
	integer LicenceType

	k(1) = 2453
	k(2) = 1768
	k(3) = 4567
 	k(4) = 1453

    read(Lstring,'(2z8)') v(1), v(2)
    call decipher(v,w,k)
  

	LicenceType  =  w(2)/100000000
    ExpiryYear   = (w(2) - LicenceType*100000000)/10000
    ExpiryMonth  = (w(2) - LicenceType*100000000 - ExpiryYear*10000)/100
    ExpiryDay    = (w(2) - LicenceType*100000000 - ExpiryYear*10000 - ExpiryMonth*100)

    if (LicenceType .NE. 3) then
		write (unit,'(A,z8)') '# The machines C drive serial number is ',w(1)
	else
		write (unit,'(A,i)' ) '# The Site code is ',w(1) - 145789123
	end if

	if (ExpiryYear .EQ.9999) THEN
		write(unit,'(A)')'# The licence is non-expiring'
    else
 		write(unit,2)ExpiryDay, Months(ExpiryMonth),ExpiryYear
  2     format('# The licence expires on ',i3,1x,A,1x,i4)
    end if
    write(unit,*) LicenceType


    end subroutine WriteLicenceFile



    subroutine encipher(v,w,k)

    integer, intent(in)     ::  v(2)
    integer, intent(out)    ::  w(2)
    integer, intent(in)     ::  k(4)

    integer y,  z
    integer a, b, c, d
    integer :: n = 32
    integer :: sum = 0
    integer :: delta = 16#9E3779B9

    integer sz, sy

    y = v(1)
    z = v(2)

    a = k(1)
    b = k(2)
    c = k(3)
    d = k(4)

    do while ( n .gt. 0)
	   n = n - 1
	   sum = sum + delta
	   y = y + (ishft(z,4)) + ieor(a,z) + ieor(sum,ishft(z,-5)) + b
	   z = z + (ishft(y,4)) + ieor(c,y) + ieor(sum,ishft(y,-5)) + d
    end do
    w(1) = y
    w(2) = z
    return
    end subroutine encipher


    subroutine decipher(v,w,k)


    integer, intent(in)     ::  v(2)
    integer, intent(out)    ::  w(2)
    integer, intent(in)     ::  k(4)

    integer y,  z
    integer a, b, c, d
    integer :: n = 32
    integer :: sum   = 16#C6EF3720
    integer :: delta = 16#9E3779B9

    integer sz, sy

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


