!     AllFFCalc.inc
      SELECT CASE(NumberSGTable)
        CASE(1)				! P1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_001(IR)
          END DO
        CASE(2)				! P-1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_002(IR)
          END DO
        CASE(39)			! P 1 21 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_039(IR)
          END DO
       CASE(40)				! C 1 2 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_040(IR)
          END DO
       CASE(44)				! P 1 c 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_044(IR)
          END DO
        CASE(50)			! C 1 c 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_050(IR)
          END DO
		CASE(52)			! I 1 a 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_052(IR)
          END DO
        CASE(57)			! P 1 21/m 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_057(IR)
          END DO
        CASE(58)			! C 1 2/m 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_058(IR)
          END DO
        CASE(61)			! P 1 2/c 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_061(IR)	
          END DO
        CASE(64)			! P 1 21/c 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_064(IR)	
          END DO
        CASE(65)			! P 1 21/n 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_065(IR)
          END DO
        CASE(66)			! P 1 21/a 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_066(IR)
          END DO
        CASE(67)			! C 1 2/c 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_067(IR)
          END DO
		CASE(69)			! I 1 2/a 1
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_069(IR)
          END DO
        CASE(112)			! P 21 21 2
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_112(IR)
          END DO
        CASE(115)			! P 21 21 21
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_115(IR)
          END DO
        CASE(116)			! C 2 2 21
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_116(IR)
          END DO
        CASE(143)			! P c a 21
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_143(IR)
          END DO
        CASE(164)			! P n a 21
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_164(IR)
          END DO
       CASE(176)			! C m c 21
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_176(IR)
          END DO
       CASE(212)			! F d d 2
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_212(IR)
          END DO
       CASE(266)			! P c c n
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_266(IR)
          END DO
       CASE(269)			! P b c m
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_269(IR)
          END DO
        CASE(284)			! P b c n
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_284(IR)
          END DO
        CASE(290)			! P b c a
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_290(IR)
          END DO
        CASE(292)			! P n m a
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_292(IR)
          END DO
        CASE(298)			! C m c m
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_298(IR)
          END DO
        CASE(304)			! C m c a
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_304(IR)
          END DO
        CASE(356)			! I -4
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_356(IR)
          END DO
        CASE(365)			! I 41/a (origin choice 2)
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_365(IR)
          END DO
        CASE(369)			! P 41 21 2
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_369(IR)
          END DO

!
! Obscure from here on in !
!
        CASE(430)			! P3
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_430(IR)
          END DO
        CASE(431)			! P31
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_431(IR)
          END DO
        CASE(432)			! P32
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_432(IR)
          END DO
!
        CASE(433)			! R3 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_433(IR)
          END DO
        CASE(434)			! P-3
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_434(IR)
          END DO
        CASE(435)			! R-3 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_435(IR)
          END DO
        CASE(449)			! P-31m hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_449(IR)
          END DO
        CASE(451)			! P-3m1 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_451(IR)
          END DO
        CASE(462)			! P6 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_462(IR)
          END DO
        CASE(468)			! P-6 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_468(IR)
          END DO
        CASE(469)			! P6/m hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_469(IR)
          END DO
        CASE(471)			! P622 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_471(IR)
          END DO
        CASE(481)			! P-6m2 hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_481(IR)
          END DO
        CASE(483)			! P-62m hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_483(IR)
          END DO
		CASE(485)			! P6/mmm hexagonal axes
          DO IR=1,MAXK
            AICALC(IR)=FFCALC_485(IR)
          END DO
!
        CASE DEFAULT
		  DO IR=1,MAXK
            AICALC(IR)=FFCALC_DEFAULT(IR)
		  END DO
      END SELECT
!