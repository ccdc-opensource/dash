!  POLARISATION.f90 
!
!  FUNCTIONS:
!	POLARISATION      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: POLARISATION
!
!  PURPOSE:  Lab data corrrection program
!		     Applies a simple polarisation correction 
!            to an xye file
!            Adds in the esd also with the correction applied
!
!****************************************************************************

	program POLARISATION
	use dflib
	implicit none
	character(len=255) :: infile
	character(len=255) :: iofile
	character(len=255) :: mangle
	real, parameter :: rad = 0.017453293
	integer(2) iflen, iolen, mlen
	integer Np
	real rtt, rint,resd, rcorr, costt, r
	character(len=255) :: line

	write(6,*)'CCDC Polarisation correction program - DASH alpha July 2000'
	write(6,*)
	write(6,*)'This program  will  read an xy or  xye  file and write  out a  modified file'
	write(6,*)'that has been corrected by multiplying each point intensity by the following'
	write(6,*)'factor:'
	write(6,*)
	write(6,*)'2.0/(1+cos(2theta)*cos(2theta)) '
	write(6,*)
	write(6,*)'The input xy  or  xye  file is  assumed to contain records of  the following'
	write(6,*)'type:'
	write(6,*)
	write(6,*)'<twotheta> <intensity> [<esd>]'
	write(6,*)
	write(6,*)'If no esd is given, then the value will be set initially to sqrt(intensity)'
	write(6,*)

!* Simple polarisation correction

	call getarg(1, infile, iflen)
	call getarg(2, iofile, iolen)
	if (iflen .EQ. -1) then
		write (6, 1001)
		read  (5,*) infile
		iflen = len_trim(infile)
	end if 
1001 format(' Enter input xye file name (the full path) >> ',$)

	if (iolen .EQ. -1) then
		write (6, 1002)
		read  (5,*) iofile
		iolen = len_trim(iofile)
	end if
1002 format(' Enter output xye file name (the full path) >> ',$)

	open(unit = 10, file = infile(1:iflen), status = 'old', err= 110)
	open(unit = 11, file = iofile(1:iolen), status = 'unknown', err= 120)
	Np = 0
	do 
		read (10,  '(A)',  end = 10)   line
		read (line,* , err = 1) rtt, rint, resd
		goto 2
 1		read (line,* , err = 10) rtt, rint
		resd = sqrt(rint)
 2      continue
		costt = cos(rad*rtt)
		rcorr = 2.0/(1.0 + costt*costt)
		rint = rint*rcorr
		resd = resd*rcorr
		write(11,'(f10.4,f10.4,f8.3)') rtt,rint,resd
		Np = Np + 1
	end do
10  continue

! Success

    write (6,*)'Read the file ',infile(1:iflen)
	write (6,*)'Wrote ',Np,' points  to the file ',iofile(1:iolen)

    close(10)
	close(11)
	goto 999

100 continue
! Error handles
	write(6,*) 'Usage: <input file> <output file>'
	goto 999
110 continue
	write (6,*) 'Could not open the input file ',  infile(1:iflen)
	goto 999 
120 continue
	write (6,*) 'Could not open the output file ', iofile(1:iolen)
	goto 999

999 continue
	write (6,*) 'Hit <CR> to exit'
	read (5,*)


	end program POLARISATION

