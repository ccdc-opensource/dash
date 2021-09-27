! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!

#ifdef __G95__
!
!   Various hacking functions/subroutines for G95 on Linux
!
      MODULE FOR_G95

      REAL, PARAMETER :: M_PI = 3.1415926

! G95 runtime errors
!       -2 End of record
!       -1 End of file
!       0  Successful return
!          Operating system errno codes (1 - 199)
!      200 Conficting statement options
!      201 Bad statement option
!      202 Missing statement option
!      203 File already opened in another unit
!      204 Unattached unit
!      205 FORMAT error
!      206 Incorrect ACTION specified
!      207 Read past ENDFILE record
!      208 Bad value during read
!      209 Numeric overflow on read
!      210 Out of memory
!      211 Array already allocated
!      212 Deallocated a bad pointer
!      213 Bad record number in direct-access file
!      214 Corrupt record in unformatted sequential-access file
!      215 Reading more data than the record size (RECL)
!      216 Writing more data than the record size (RECL)
!      217 Unknown error code

      ABSTRACT INTERFACE
        SUBROUTINE CallbackFunc
        END SUBROUTINE
      END INTERFACE

      TYPE CallbackPointer
        SEQUENCE
        PROCEDURE (CallbackFunc), POINTER :: p
      END TYPE

      CONTAINS

      REAL FUNCTION COSD(Degrees)
      IMPLICIT NONE
      REAL, INTENT (IN   ) :: Degrees
      COSD = COS(Degrees * M_PI / 180.0)
      END FUNCTION COSD

      REAL FUNCTION TAND(Degrees)
      IMPLICIT NONE
      REAL, INTENT (IN   ) :: Degrees
      TAND = TAN(Degrees * M_PI / 180.0)
      END FUNCTION TAND

      REAL FUNCTION ACOSD(X)
      IMPLICIT NONE
      REAL, INTENT (IN   ) :: X
      ACOSD = ACOS(X) * 180.0 / M_PI
      END FUNCTION ACOSD

      REAL FUNCTION ASIND(X)
      IMPLICIT NONE
      REAL, INTENT (IN   ) :: X
      ASIND = ASIN(X) * 180.0 / M_PI
      END FUNCTION ASIND

      INTEGER FUNCTION NARGS()
      IMPLICIT NONE
      NARGS = IARGC() + 1
      END FUNCTION NARGS

      END MODULE FOR_G95
#endif
