!
!*****************************************************************************
!
! Variables for DICVOL
      MODULE DICVAR

      IMPLICIT NONE

      INTEGER DICVOL_Error

      INTEGER     cDICVOL_ErrorOnWrite
      PARAMETER ( cDICVOL_ErrorOnWrite     = 1 )
      INTEGER     cDICVOL_TooManySolutions
      PARAMETER ( cDICVOL_TooManySolutions = 2 )
      INTEGER     cDIVCOLExpErrTooLarge
      PARAMETER ( cDIVCOLExpErrTooLarge    = 3 )
      INTEGER     cDICVOL_ErrorInterrupted
      PARAMETER ( cDICVOL_ErrorInterrupted = 4 )

      INTEGER     cCubic
      PARAMETER ( cCubic        =   1 )
      INTEGER     cHexagonal
      PARAMETER ( cHexagonal    =   2 )
      INTEGER     cTetragonal
      PARAMETER ( cTetragonal   =   4 )
      INTEGER     cTrigonal
      PARAMETER ( cTrigonal     =   8 )
      INTEGER     cRhombohedral
      PARAMETER ( cRhombohedral =  16 )
      INTEGER     cOrthorhombic
      PARAMETER ( cOrthorhombic =  32 )
      INTEGER     cMonoclinic
      PARAMETER ( cMonoclinic   =  64 )
      INTEGER     cTriclinic
      PARAMETER ( cTriclinic    = 128 )
      
      INTEGER DICVOL_NumOfSolutions(1:6) ! Number of solutions per crystal system
! 1 = Cubic
! 2 = Tetragonal
! 3 = Hexagonal
! 4 = Orthorhombic
! 5 = Monoclinic
! 6 = Triclinic

      REAL DV_ScaleFactor
! To scale all the data, so that large unit cells can be treated as though they were small.
! implementation: 
! 1. the wavelength is multiplied by this factor before being fed to DICVOL.
! 2. All peak position errors are multiplied by this factor
! 3. all a, b, and c output by DICVOL are divided by it. 
! 4. The volume is divided by DV_ScaleFactor**3

      REAL        coeff
      PARAMETER ( coeff = 1.E+08 )
      REAL        Coef4
      PARAMETER ( Coef4 = 1.E-04 )
      REAL        pirad
      PARAMETER ( pirad = 0.3141592653589793115997963469E+01 / 180.0 )
      REAL        pideg
      PARAMETER ( pideg = 180.0 / 0.3141592653589793115997963469E+01 )
      REAL dth, pas, pas16, pas2, pas32, pas4, pas64, pas8, rap, v, vinf, vsup, wave2
      REAL, DIMENSION(50) :: d, epsil, epsq, q, th
      INTEGER :: kdens, kz, mh, mh2, mk, mk2, ml, ml2, n
      INTEGER, DIMENSION(50) :: kepsq, kq, kqt
      REAL :: amoi1, amoi2, amoi3, aplu1, aplu2, aplu3, bmoi1, bmoi2, bmoi3, bplu1, bplu2, bplu3,      &
     &        cmoi1, cmoi2, cmoi3, cplu1, cplu2, cplu3, emoi1, emoi2, emoi3, eplu1fplu1, eplu2fplu2,    &
     &        eplu3fplu3, fmoi1, fmoi2, fmoi3
      INTEGER :: kamoi1, kamoi2, kamoi3, kaplu1, kaplu2, kaplu3, kbmoi1, kbmoi2, kbmoi3, kbplu1, kbplu2,&
     &           kbplu3, kcmoi1, kcmoi2, kcmoi3, kcplu1, kcplu2, kcplu3, kemoi1, kemoi2, kemoi3, keplu1,&
     &           keplu2, keplu3, kfmoi1, kfmoi2, kfmoi3, kfplu1, kfplu2, kfplu3
      REAL :: amoi5, amoi6, amoi7, aplu5, aplu6, aplu7, bmoi5, bmoi6, bmoi7, bplu5, bplu6, bplu7,      &
     &        cmoi5, cmoi6, cmoi7, cplu5, cplu6, cplu7, emoi5, emoi6, emoi7, eplu5fplu5, eplu6fplu6,    &
     &        eplu7fplu7, fmoi5, fmoi6, fmoi7
      INTEGER :: kamoi5, kamoi6, kamoi7, kaplu5, kaplu6, kaplu7, kbmoi5, kbmoi6, kbmoi7, kbplu5, kbplu6,&
     &           kbplu7, kcmoi5, kcmoi6, kcmoi7, kcplu5, kcplu6, kcplu7, kemoi5, kemoi6, kemoi7, keplu5,&
     &           keplu6, keplu7, kfmoi5, kfmoi6, kfmoi7, kfplu5, kfplu6, kfplu7
      REAL, DIMENSION(100) :: aas, bbs, ccs, dds, ees, ffs
      REAL, DIMENSION(50) :: arx, brx, crx, drx, erx, frx
      REAL :: cxa, cxb, cxc, cxd, cxe, cxf, cya, pasa, pasa16, pasa2, pasa32, pasa4, pasa64, pasa8,  &
     &        pasb, pasb16, pasb2, pasb32, pasb4, pasb64, pasb8, pasc, pasc16, pasc2, pasc32, pasc4,   &
     &        pasc64, pasc8, pasd, pasd16, pasd2, pasd32, pasd4, pasd64, pasd8, pase, pase16, pase2,   &
     &        pase32, pase4, pase64, pase8, pasf, pasf16, pasf2, pasf32, pasf4, pasf64, pasf8
      INTEGER, DIMENSION(8) :: ndich
      REAL :: amin, amn, bmax, bmin, bmn, dmax, dmin
      INTEGER :: kamax, kamin, kbmax, kbmin, kdmax, kdmin
      REAL :: amax, dd1, dd2
      REAL :: fwolff
      INTEGER, DIMENSION(50,50,8) :: ih, ik
      INTEGER, DIMENSION(50,100,8) :: il
      INTEGER, DIMENSION(50,8) :: irj
      INTEGER :: nposs, nt
      REAL :: aa, ar, bb, beta, br, cc, cr, dr, er, fr
      REAL :: dmoi1, dmoi2, dmoi3, dmoi4, dmoi5, dmoi6, dmoi7, dplu1, dplu2, dplu3, dplu4, dplu5,      &
     &        dplu6, dplu7
      INTEGER :: kdmoi1, kdmoi2, kdmoi3, kdmoi4, kdmoi5, kdmoi6, kdmoi7, kdplu1, kdplu2, kdplu3, kdplu4,&
     &           kdplu5, kdplu6, kdplu7
      REAL :: cmax
      INTEGER :: iidd, jrd
      REAL, DIMENSION(50) :: rcmax, rmax, rmin
      REAL :: epst, fom
      INTEGER :: jcount, nini
      REAL :: amaxm, bmaxm, cmaxm, petiamax
      INTEGER :: mc
      REAL :: bebe, beinf6, pab64
      INTEGER, DIMENSION(50,50) :: ihb, ikb, ilb
      INTEGER, DIMENSION(50) :: irjb
      INTEGER :: mt
      INTEGER :: n5, n6
      REAL, DIMENSION(6) :: par, rec
      REAL :: amoi4, aplu4, bmoi4, bplu4, cmin, cmoi4, cplu4, emax, emin, emoi4, eplu4, errb, errd,   &
     &        erre, fmax, fmin, fmoi4, fplu4, s1, s3, s5, s7, s8, zco
      INTEGER :: iiaa, iibb, iicc, iiee, iiff, jjaa, jjbb, jjcc, jjdd, jjee, jjff, kamoi4,        &
     &           kaplu4, kbmoi4, kbplu4, kcmax, kcmin, kcmoi4, kcplu4, kemoi4, keplu4,    &
     &           kfmoi4, kfplu4, kt, ktp, md12, md34, md56, md7, md89
      INTEGER :: iw
      INTEGER :: ly
      REAL :: vmax2, vmin2, vmoii, vr2
      REAL Bemin, Bemax

      INTEGER MaxDICVOLSolutions
      PARAMETER ( MaxDICVOLSolutions = 30 )
! The maximum number of solutions generated by DICVOL.
! This also determines the size of the grid in the Winteracter dialogue where the user can
! choose from the solutions.

      TYPE T_UnitCell
        INTEGER :: CrystalSystem
        REAL    :: a, b, c, alpha, beta, gamma
        REAL    :: Volume
        REAL    :: F, M
      END TYPE
! CrystalSystem = 
!            1 = Triclinic
!            2 = Monoclinic-a
!            3 = Monoclinic-b
!            4 = Monoclinic-c
!            5 = Orthorhombic
!            6 = Tetragonal
!            7 = Trigonal
!            8 = Rhombohedral
!            9 = Hexagonal
!           10 = Cubic
!
! F = Figure of merit
! M = Figure of merit

      INTEGER           :: NumOfDICVOLSolutions
      TYPE (T_UnitCell) :: DICVOLSolutions(MaxDICVOLSolutions)

      END MODULE DICVAR
!
!*****************************************************************************
!
