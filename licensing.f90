


subroutine decipher(v,w,k)


integer, intent(in)     ::  v(2)
integer, intent(out)    ::  w(2)
integer, intent(in)     ::  k(4)

integer y,  z
integer a, b, c, d
integer :: n 
integer :: sum
integer :: delta = 16#9E3779B9

integer sz, sy

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
