      subroutine makexyz(n,blen,alph,bet,iz1,iz2,iz3,x,y,z)
      implicit none
C     Arguments
      integer n
      integer iz1(*),iz2(*),iz3(*)
      double precision x(*),y(*),z(*),blen(*),alph(*),bet(*)
C     Constants
      real*8 radian,pi,sqrtpi,twosix,logten
      parameter (radian=57.29577951308232088d0)
      parameter (pi=3.141592653589793238d0)
      parameter (sqrtpi=1.772453850905516027d0)
      parameter (twosix=1.122462048309372981d0)
      parameter (logten=2.302585092994045684d0)
C     Local
c  replace radian with rad=(1/radian)
      integer i,k,i1,i2,i3
      real*8 bond,angle1,angle2,sign, rad

c      do i=1,n
c      write (6,*) blen(i),alph(i),bet(i)
c      end do
c      do i=1,n
c      write (6,*) iz1(i),iz2(i),iz3(i)
c      end do

c cosmetic change to demo cvs

      rad = 1.0/radian
c     First atom is placed at the origin
      if (n .ge. 1) then
         x(1) = 0.0d0
         y(1) = 0.0d0
         z(1) = 0.0d0
      endif
C     Second atom is placed along the z-axis
      if (n .ge. 2) then
         x(2) = 0.0d0
         y(2) = 0.0d0
         z(2) = blen(2)
      endif
C     Third atom is placed in the x,z-plane
      if (n .ge. 3) then
         x(3) = blen(3) * sin(alph(3)*rad)
         y(3) = 0.0d0
         if (iz1(3) .eq. 1) then
            z(3) = blen(3) * cos(alph(3)*rad)
         else
            z(3) = z(2) - blen(3)*cos(alph(3)*rad)
         end if
      endif
C     As long as atoms remain linear with the first
C     two atoms, keep placing them along the z-axis
      i = 3
      if (n .gt. 3) then
         dowhile (nint(x(i)*10000) .eq. 0)
            i = i + 1
            i1 = iz1(i)
            i2 = iz2(i)
            if (z(i1) .gt. z(i2)) then
               sign = 1.0d0
            else
               sign = -1.0d0
            end if
            x(i) = blen(i) * sin(alph(i)*rad)
            y(i) = 0.0d0
            z(i) = z(i1) - sign*blen(i)*cos(alph(i)*rad)
         end do
      end if
C     Loop over each atom in turn, finding its coordinates
      k = i + 1
      if (k .le. n) then
       do i = k, n
         i1 = iz1(i)
         i2 = iz2(i)
         i3 = iz3(i)
         bond = blen(i)
         angle1 = alph(i)
         angle2 = bet(i)
         call xyzatm (x,y,z,i,i1,bond,i2,angle1,i3,angle2)
       end do
      end if
      return
      end

C     "xyzatm" computes the Cartesian coordinates of a single
C     atom from its defining internal coordinate values

      subroutine xyzatm(x,y,z,i,i1,bond,i2,angle1,i3,angle2)
      implicit none
C     Arguments
      real*8 x(*),y(*),z(*)
      real*8 bond,angle1,angle2
      integer i,i1,i2,i3
C     Constants
      real*8 radian,pi,sqrtpi,twosix,logten
      parameter (radian=57.29577951308232088d0)
      parameter (pi=3.141592653589793238d0)
      parameter (sqrtpi=1.772453850905516027d0)
      parameter (twosix=1.122462048309372981d0)
      parameter (logten=2.302585092994045684d0)
      real*8 small
      parameter (small=1.d-8)
C     Local
      real*8 ang_1,ang_2
      real*8 sin_1,cos_1,sin_2,cos_2
      real*8 cosine,one_over_sine,sine2,norm,eps,sinarg
      real*8 u1(3),u2(3),u3(3),u4(3),a,b,c, rad
c
c     convert the angle values from degrees to radians;
c     then find their sine and cosine values
c
      eps = 0.00000001d0
      rad = 1.0/radian
      ang_1 = angle1 * rad
      ang_2 = angle2 * rad
      sin_1 = sin(ang_1)
      cos_1 = cos(ang_1)
      sin_2 = sin(ang_2)
      cos_2 = cos(ang_2)
      u1(1) = x(i2) - x(i3)
      u1(2) = y(i2) - y(i3)
      u1(3) = z(i2) - z(i3)
c      write(6,*) i2,i3
c      write(6,*) (u1(1)*u1(1) + u1(2)*u1(2) + u1(3)*u1(3))
      norm = 1.0/sqrt(u1(1)*u1(1) + u1(2)*u1(2) + u1(3)*u1(3))
      u1(1) = u1(1) * norm
      u1(2) = u1(2) * norm
      u1(3) = u1(3) * norm
      u2(1) = x(i1) - x(i2)
      u2(2) = y(i1) - y(i2)
      u2(3) = z(i1) - z(i2)
c      write(6,*) i1,i2
c      write(6,*) (u2(1)*u2(1) + u2(2)*u2(2) + u2(3)*u2(3))
      norm = 1.0/sqrt(u2(1)*u2(1) + u2(2)*u2(2) + u2(3)*u2(3))
      u2(1) = u2(1) * norm
      u2(2) = u2(2) * norm
      u2(3) = u2(3) * norm
      u3(1) = u1(2)*u2(3) - u1(3)*u2(2)
      u3(2) = u1(3)*u2(1) - u1(1)*u2(3)
      u3(3) = u1(1)*u2(2) - u1(2)*u2(1)
      cosine = u1(1)*u2(1) + u1(2)*u2(2) + u1(3)*u2(3)
      if (abs(cosine) .lt. 1.0d0) then
         one_over_sine = 1.0/sqrt(1.0d0 - cosine**2)
      else
!         write (*,10)  i
!   10    format (' XYZATM - Undefined Dihed Angle at Atom',i6)
         sinarg=dmax1(small,cosine**2 - 1.0d0 )
c	 write (*,*) ' cosine is ',cosine,sinarg
         one_over_sine = 1.0/sqrt(sinarg)
c	 write (*,*) ' cosine is ',cosine,sinarg
c         sine = 1.0/sqrt(cosine**2 - 1.0d0)
      end if
      u3(1) = u3(1) * one_over_sine
      u3(2) = u3(2) * one_over_sine
      u3(3) = u3(3) * one_over_sine
      u4(1) = u3(2)*u2(3) - u3(3)*u2(2)
      u4(2) = u3(3)*u2(1) - u3(1)*u2(3)
      u4(3) = u3(1)*u2(2) - u3(2)*u2(1)
      x(i) = x(i1) + bond * (-u2(1)*cos_1 + u4(1)*sin_1*cos_2
     &                             + u3(1)*sin_1*sin_2)
      y(i) = y(i1) + bond * (-u2(2)*cos_1 + u4(2)*sin_1*cos_2
     &                              + u3(2)*sin_1*sin_2)
      z(i) = z(i1) + bond * (-u2(3)*cos_1 + u4(3)*sin_1*cos_2
     &                              + u3(3)*sin_1*sin_2)
      return
      end
