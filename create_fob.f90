!*==CREATE_FOB.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE create_fob()
!
      INCLUDE 'PARAMS.INC'
      INCLUDE 'IZMCheck.inc'
!
      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen, alph, bet, f2cmat
      CHARACTER*3 asym
      INTEGER ioptb, iopta, ioptt, iz1, iz2, iz3
!
      COMMON /zmcomi/ ntatm, natoms(maxfrg), ioptb(maxatm,maxfrg),      &
     &                iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),       &
     &                iz1(maxatm,maxfrg), iz2(maxatm,maxfrg),           &
     &                iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg), alph(maxatm,maxfrg),         &
     &                bet(maxatm,maxfrg), f2cmat(3,3)
      COMMON /zmcomc/ asym(maxatm,maxfrg)
!
      COMMON /frgcom/ nfrag, lfrag(maxfrg)
!
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      COMMON /FCSPC2/ ARGK(MFCSP2), DSTAR(MFCSP2)
      LOGICAL HYDNOT
      COMMON /HIDDAT/ HYDNOT(150), nsatom, isatom(150)
!
      INTEGER jj
!
      item = 0
      NSATOM = 0
      ifrg = 0
      DO jj = 1, nfrag
        DO WHILE (ifrg.LE.CheckSize)
          ifrg = ifrg + 1
          IF (IZMCheck(ifrg).EQ.1) EXIT     ! the loop since we have a fragment we are using
        ENDDO
        DO i = 1, natoms(ifrg)
          item = item + 1
          hydnot(item) = (asym(i,ifrg).NE.'H  ')
          IF (hydnot(item)) THEN
            nsatom = nsatom + 1
            isatom(nsatom) = item
          ENDIF
          DO iref = 1, maxk
            ssq = 0.25*dstar(iref)**2
            atem = occ(i,ifrg)*ascfac(asym(i,ifrg),ssq)
            btem = tiso(i,ifrg)*ssq
            fob(item,iref) = atem*EXP(-btem)
          ENDDO
        ENDDO
      ENDDO
      natom = item
!
      END SUBROUTINE CREATE_FOB
!*==ASCFAC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      FUNCTION ascfac(asym,ss)
! Atomic SCattering FACtors
!
      PARAMETER (melem=99)
      CHARACTER*3 asym, symba(melem)
      REAL a1(melem), b1(melem), a2(melem), b2(melem)
      REAL a3(melem), b3(melem), a4(melem), b4(melem)
      REAL cv(melem)
!
      DATA SYMBA/'Du', 'H  ', 'He ', 'Li ', 'Be ', 'B  ', 'C  ', 'N  ', &
     &     'O  ', 'F  ', 'Ne ', 'Na ', 'Mg ', 'Al ', 'Si ', 'P  ',      &
     &     'S  ', 'Cl ', 'Ar ', 'K  ', 'Ca ', 'Sc ', 'Ti ', 'V  ',      &
     &     'Cr ', 'Mn ', 'Fe ', 'Co ', 'Ni ', 'Cu ', 'Zn ', 'Ga ',      &
     &     'Ge ', 'As ', 'Se ', 'Br ', 'Kr ', 'Rb ', 'Sr ', 'Y  ',      &
     &     'Zr ', 'Nb ', 'Mo ', 'Tc ', 'Ru ', 'Rh ', 'Pd ', 'Ag ',      &
     &     'Cd ', 'In ', 'Sn ', 'Sb ', 'Te ', 'I  ', 'Xe ', 'Cs ',      &
     &     'Ba ', 'La ', 'Ce ', 'Pr ', 'Nd ', 'Pm ', 'Sm ', 'Eu ',      &
     &     'Gd ', 'Tb ', 'Dy ', 'Ho ', 'Er ', 'Tm ', 'Yb ', 'Lu ',      &
     &     'Hf ', 'Ta ', 'W  ', 'Re ', 'Os ', 'Ir ', 'Pt ', 'Au ',      &
     &     'Hg ', 'Tl ', 'Pb ', 'Bi ', 'Po ', 'At ', 'Rn ', 'Fr ',      &
     &     'Ra ', 'Ac ', 'Th ', 'Pa ', 'U  ', 'Np ', 'Pu ', 'Am ',      &
     &     'Cm ', 'Bk ', 'Cf '/
!
      DATA A1/0.0000, 0.48992, 0.87340, 1.12820, 1.59190, 2.05450,      &
     &     2.31000, 12.21260, 3.04850, 3.53920, 3.95530, 4.76260,       &
     &     5.42040, 6.42020, 6.29150, 6.43450, 6.90530, 11.46040,       &
     &     7.48450, 8.21860, 8.62660, 9.18900, 9.75950, 10.29710,       &
     &     10.64060, 11.28190, 11.76950, 12.28410, 12.83760, 13.33800,  &
     &     14.07430, 15.23540, 16.08160, 16.67230, 17.00060, 17.17890,  &
     &     17.35550, 17.17840, 17.56630, 17.77600, 17.87650, 17.61420,  &
     &     3.70250, 19.13010, 19.26740, 19.29570, 19.33190, 19.28080,   &
     &     19.22140, 19.16240, 19.18890, 19.64180, 19.96440, 20.14720,  &
     &     20.29330, 20.38920, 20.33610, 20.57800, 21.16710, 22.04400,  &
     &     22.68450, 23.34050, 24.00420, 24.62740, 25.07090, 25.89760,  &
     &     26.50700, 26.90490, 27.65630, 28.18190, 28.66410, 28.94760,  &
     &     9.14400, 29.20240, 29.08180, 28.76210, 28.18940, 27.30490,   &
     &     27.00590, 16.88190, 20.68090, 27.54460, 31.06170, 33.36890,  &
     &     34.67260, 35.31630, 35.56310, 35.92990, 35.76300, 35.65970,  &
     &     35.56450, 35.88470, 36.02280, 36.18740, 36.52540, 36.67060,  &
     &     36.64880, 36.78810, 36.91850/
!
      DATA B1/1.0000, 20.65930, 9.10370, 3.95460, 43.64270, 23.21850,   &
     &     20.84390, 0.00570, 13.27710, 10.28250, 8.40420, 3.28500,     &
     &     2.82750, 3.03870, 2.43860, 1.90670, 1.46790, 0.01040,        &
     &     0.90720, 12.79490, 10.44210, 9.02130, 7.85080, 6.86570,      &
     &     6.10380, 5.34090, 4.76110, 4.27910, 3.87850, 3.58280,        &
     &     3.26550, 3.06690, 2.85090, 2.63450, 2.40980, 2.17230,        &
     &     1.93840, 1.78880, 1.55640, 1.40290, 1.27618, 1.18865,        &
     &     0.27720, 0.86413, 0.80852, 0.75154, 0.69866, 0.64460,        &
     &     0.59460, 0.54760, 5.83030, 5.30340, 4.81742, 4.34700,        &
     &     3.92820, 3.56900, 3.21600, 2.94817, 2.81219, 2.77393,        &
     &     2.66248, 2.56270, 2.47274, 2.38790, 2.25341, 2.24256,        &
     &     2.18020, 2.07051, 2.07356, 2.02859, 1.98890, 1.90182,        &
     &     1.83262, 1.77333, 1.72029, 1.67191, 1.62903, 1.59279,        &
     &     1.51293, 0.46110, 0.54500, 0.65515, 0.69020, 0.70400,        &
     &     0.70100, 0.68587, 0.66310, 0.64645, 0.61634, 0.58909,        &
     &     0.56336, 0.54775, 0.52930, 0.51193, 0.49938, 0.48363,        &
     &     0.46515, 0.45102, 0.43753/
!
      DATA A2/0.0000, 0.26200, 0.63090, 0.75080, 1.12780, 1.33260,      &
     &     1.02000, 3.13220, 2.28680, 2.64120, 3.11250, 3.17360,        &
     &     2.17350, 1.90020, 3.03530, 4.17910, 5.20340, 7.19640,        &
     &     6.77230, 7.43980, 7.38730, 7.36790, 7.35580, 7.35110,        &
     &     7.35370, 7.35730, 7.35730, 7.34090, 7.29200, 7.16760,        &
     &     7.03180, 6.70060, 6.37470, 6.07010, 5.81960, 5.23580,        &
     &     6.72860, 9.64350, 9.81840, 10.29460, 10.94800, 12.01440,     &
     &     17.23560, 11.09480, 12.91820, 14.35010, 15.50170, 16.68850,  &
     &     17.64440, 18.55960, 19.10050, 19.04550, 19.01380, 18.99490,  &
     &     19.02980, 19.10620, 19.29700, 19.59900, 19.76950, 19.66970,  &
     &     19.68470, 19.60950, 19.42580, 19.08860, 19.07980, 18.21850,  &
     &     17.63830, 17.29400, 16.42850, 15.88510, 15.43450, 15.22080,  &
     &     15.17260, 15.22930, 15.43000, 15.71890, 16.15500, 16.72960,  &
     &     17.76390, 18.59130, 19.04170, 19.15840, 13.06370, 12.95100,  &
     &     15.47330, 19.02110, 21.28160, 23.05470, 22.90640, 23.10320,  &
     &     23.42190, 23.29480, 23.41280, 23.59640, 23.80830, 24.09920,  &
     &     24.40960, 24.77360, 25.19950/
!
      DATA B2/1.0000, 7.74039, 3.35680, 1.05240, 1.86230, 1.02100,      &
     &     10.20750, 9.89330, 5.70110, 4.29440, 3.42620, 8.84220,       &
     &     79.26110, 0.74260, 32.33370, 27.15700, 22.21510, 1.16620,    &
     &     14.84070, 0.77480, 0.65990, 0.57290, 0.50000, 0.43850,       &
     &     0.39200, 0.34320, 0.30720, 0.27840, 0.25650, 0.24700,        &
     &     0.23330, 0.24120, 0.25160, 0.26470, 0.27260, 16.57960,       &
     &     16.56230, 17.31510, 14.09880, 12.80060, 11.91600, 11.76600,  &
     &     1.09580, 8.14487, 8.43467, 8.21758, 7.98929, 7.47260,        &
     &     6.90890, 6.37760, 0.50310, 0.46070, 0.42088, 0.38140,        &
     &     0.34400, 0.31070, 0.27560, 0.24448, 0.22684, 0.22209,        &
     &     0.21063, 0.20209, 0.19645, 0.19420, 0.18195, 0.19614,        &
     &     0.20217, 0.19794, 0.22354, 0.23885, 0.25712, 9.98519,        &
     &     9.59990, 9.37046, 9.22590, 9.09227, 8.97948, 8.86553,        &
     &     8.81174, 8.62160, 8.44840, 8.70751, 2.35760, 2.92380,        &
     &     3.55078, 3.97458, 4.06910, 4.17619, 3.87135, 3.65155,        &
     &     3.46204, 3.41519, 3.32530, 3.25396, 3.26371, 3.20647,        &
     &     3.08997, 3.04619, 3.00775/
!
      DATA A3/0.0000, 0.19677, 0.31120, 0.61750, 0.53910, 1.09790,      &
     &     1.58860, 2.01250, 1.54630, 1.51700, 1.45460, 1.26740,        &
     &     1.22690, 1.59360, 1.98910, 1.78000, 1.43790, 6.25560,        &
     &     0.65390, 1.05190, 1.58990, 1.64090, 1.69910, 2.07030,        &
     &     3.32400, 3.01930, 3.52220, 4.00340, 4.44380, 5.61580,        &
     &     5.16520, 4.35910, 3.70680, 3.43130, 3.97310, 5.63770,        &
     &     5.54930, 5.13990, 5.42200, 5.72629, 5.41732, 4.04183,        &
     &     12.88760, 4.64901, 4.86337, 4.73425, 5.29537, 4.80450,       &
     &     4.46100, 4.29480, 4.45850, 5.03710, 6.14487, 7.51380,        &
     &     8.97670, 10.66200, 10.88800, 11.37270, 11.85130, 12.38560,   &
     &     12.77400, 13.12350, 13.43960, 13.76030, 13.85180, 14.31670,  &
     &     14.55960, 14.55830, 14.97790, 15.15420, 15.30870, 15.10000,  &
     &     14.75860, 14.51350, 14.43270, 14.55640, 14.93050, 15.61150,  &
     &     15.71310, 25.55820, 21.65750, 15.53800, 18.44200, 16.58770,  &
     &     13.11380, 9.49887, 8.00370, 12.14390, 12.47390, 12.59770,    &
     &     12.74730, 14.18910, 14.94910, 15.64020, 16.77070, 17.34150,  &
     &     17.39900, 17.89190, 18.33170/
!
      DATA B3/1.0000, 49.55190, 22.92760, 85.39050, 103.4830, 60.34980, &
     &     0.56870, 28.99750, 0.32390, 0.26150, 0.23060, 0.31360,       &
     &     0.38080, 31.54720, 0.67850, 0.52600, 0.25360, 18.51940,      &
     &     43.89830, 213.18700, 85.74840, 136.10800, 35.63380, 26.89380,&
     &     20.26260, 17.86740, 15.35350, 13.53590, 12.17630, 11.39660,  &
     &     10.31630, 10.78050, 11.44680, 12.94790, 15.23720, 0.26090,   &
     &     0.22610, 0.27480, 0.16640, 0.12560, 0.11762, 0.20479,        &
     &     11.00400, 21.57070, 24.79970, 25.87490, 25.20520, 24.66050,  &
     &     24.70080, 25.84990, 26.89090, 27.90740, 28.52840, 27.76600,  &
     &     26.46590, 24.38790, 20.20730, 18.77260, 17.60830, 16.76690,  &
     &     15.88500, 15.10090, 14.39960, 13.75460, 12.93310, 12.66480,  &
     &     12.18990, 11.44070, 11.36040, 10.99750, 10.66470, 0.26103,   &
     &     0.27512, 0.29598, 0.32170, 0.35050, 0.38266, 0.41792,        &
     &     0.42459, 1.48260, 1.57290, 1.96347, 8.61800, 8.79370,        &
     &     9.55642, 11.38240, 14.04220, 23.10520, 19.98870, 18.59900,   &
     &     17.83090, 16.92350, 16.09270, 15.36220, 14.94550, 14.31360,  &
     &     13.43460, 12.89460, 12.40440/
!
      DATA A4/0.0000, 0.04988, 0.17800, 0.46530, 0.70290, 0.10680,      &
     &     0.86500, 1.16630, 0.86700, 1.02430, 1.12510, 1.11280,        &
     &     2.30730, 1.96460, 1.54100, 1.49080, 1.58630, 1.64550,        &
     &     1.64420, 0.86590, 1.02110, 1.46800, 1.90210, 2.05710,        &
     &     1.49220, 2.24410, 2.30450, 2.34880, 2.38000, 1.67350,        &
     &     2.41000, 2.96230, 3.68300, 4.27790, 4.35430, 3.98510,        &
     &     3.53750, 1.52920, 2.66940, 3.26588, 3.65721, 3.53346,        &
     &     3.74290, 2.71263, 1.56756, 1.28918, 0.60584, 1.04630,        &
     &     1.60290, 2.03960, 2.46630, 2.68270, 2.52390, 2.27350,        &
     &     1.99000, 1.49530, 2.69590, 3.28719, 3.33049, 2.82428,        &
     &     2.85137, 2.87516, 2.89604, 2.92270, 3.54545, 2.95354,        &
     &     2.96577, 3.63837, 2.98233, 2.98706, 2.98963, 3.71601,        &
     &     4.30013, 4.76492, 5.11982, 5.44174, 5.67589, 5.83377,        &
     &     5.78370, 5.86000, 5.96760, 5.52593, 5.96960, 6.46920,        &
     &     7.02588, 7.42518, 7.44330, 2.11253, 3.21097, 4.08655,        &
     &     4.80703, 4.17287, 4.18800, 4.18550, 3.47947, 3.49331,        &
     &     4.21665, 4.23284, 4.24391/
!
      DATA B4/1.0000, 2.20159, 0.98210, 168.26100, 0.54200, 0.14030,    &
     &     51.65120, 0.58260, 32.90890, 26.14760, 21.71840, 129.42400,  &
     &     7.19370, 85.08860, 81.69370, 68.16450, 56.17200, 47.77840,   &
     &     33.39290, 41.68410, 178.43700, 51.35310, 116.10500,          &
     &     102.47800, 98.73990, 83.75430, 76.88050, 71.16920, 66.34210, &
     &     64.81260, 58.70970, 61.41350, 54.76250, 47.79720, 43.81630,  &
     &     41.43280, 39.39720, 164.93401, 132.37601, 104.35400,         &
     &     87.66270, 69.79570, 61.65840, 86.84720, 94.29280, 98.60620,  &
     &     76.89860, 99.81560, 87.48250, 92.80290, 83.95710, 75.28250,  &
     &     70.84030, 66.87760, 64.26580, 213.90401, 167.20200,          &
     &     133.12399, 127.11300, 143.64400, 137.90300, 132.72099,       &
     &     128.00700, 123.17400, 101.39800, 115.36200, 111.87400,       &
     &     92.65660, 105.70300, 102.96100, 100.41700, 84.32980,         &
     &     72.02900, 63.36440, 57.05600, 52.08610, 48.16470, 45.00110,  &
     &     38.61030, 36.39560, 38.32460, 45.81490, 47.25790, 48.00930,  &
     &     47.00450, 45.47150, 44.24730, 150.64500, 142.32500,          &
     &     117.02000, 99.17220, 105.25100, 100.61300, 97.49080,         &
     &     105.98000, 102.27300, 88.48340, 86.00300, 83.78810/
!
      DATA CV/0.0000, 0.00131, 0.00640, 0.03770, 0.03850, -0.19320,     &
     &     0.21560, -11.52900, 0.25080, 0.27760, 0.35150, 0.67600,      &
     &     0.85840, 1.11510, 1.14070, 1.11490, 0.86690, -9.55740,       &
     &     1.44450, 1.42280, 1.37510, 1.33290, 1.28070, 1.21990,        &
     &     1.18320, 1.08960, 1.03690, 1.01180, 1.03410, 1.19100,        &
     &     1.30410, 1.71890, 2.13130, 2.53100, 2.84090, 2.95570,        &
     &     2.82500, 3.48730, 2.50640, 1.91213, 2.06929, 3.75591,        &
     &     4.38750, 5.40428, 5.37874, 5.32800, 5.26593, 5.17900,        &
     &     5.06940, 4.93910, 4.78210, 4.59090, 4.35200, 4.07120,        &
     &     3.71180, 3.33520, 2.77310, 2.14678, 1.86264, 2.05830,        &
     &     1.98486, 2.02876, 2.20963, 2.57450, 2.41960, 3.58024,        &
     &     4.29728, 4.56796, 5.92046, 6.75621, 7.56672, 7.97628,        &
     &     8.58154, 9.24354, 9.88750, 10.47200, 11.00050, 11.47220,     &
     &     11.68830, 12.06580, 12.60890, 13.17460, 13.41180, 13.57820,  &
     &     13.67700, 13.71080, 13.69050, 13.72470, 13.62110, 13.52660,  &
     &     13.43140, 13.42870, 13.39660, 13.35730, 13.38120, 13.35920,  &
     &     13.28870, 13.27540, 13.26740/
!
      DO I = 1, melem
        IF (asym.EQ.symba(I)) THEN
          ascfac = a1(I)*EXP(-b1(I)*ss) + a2(I)*EXP(-b2(I)*ss) + a3(I)  &
     &             *EXP(-b3(I)*ss) + a4(I)*EXP(-b4(I)*ss) + cv(I)
          GOTO 999
        ENDIF
      ENDDO
! default is a dummy.
      ascfac = a1(1)*EXP(-b1(1)*ss) + a2(1)*EXP(-b2(1)*ss) + a3(1)      &
     &         *EXP(-b3(1)*ss) + a4(1)*EXP(-b4(1)*ss) + cv(1)
  999 RETURN
!
      END FUNCTION ASCFAC
!
!*****************************************************************************
!
