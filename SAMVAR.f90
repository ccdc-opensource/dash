!
!*****************************************************************************
!
      MODULE SAMVAR

      IMPLICIT NONE

! PARAMETER definitions

      INTEGER, PARAMETER :: MAXATM_2 = 150
      INTEGER, PARAMETER :: MAXBND   = 1500

      REAL atchg(1:MAXATM_2)
! atchg = formal charge

      CHARACTER*5 atomlabel(1:MAXATM_2)
! atomlabel = Original atom labels

      REAL, DIMENSION(MAXATM_2,3) :: axyzo
! axyzo = atomic xyz co-ordinates, orthogonal

      INTEGER, DIMENSION(MAXATM_2) :: aelem
! aelem = element type as used by CSD

!U! Elements (plus other CSD 'element' definitions What's 'ZZ'??)
!U      DATA el  /'C ','H ','AC','AG','AL','AM','AR','AS','AT','AU','B ', &
!U           'BA','BE','BI','BK','BR','CA','CD','CE','CF','CL','CM','CO', &
!U           'CR','CS','CU','D ','DY','ER','ES','EU','F ','FE','FM','FR', &
!U           'GA','GD','GE','HE','HF','HG','HO','I ','IN','IR','K ','KR', &
!U           'LA','LI','LU','LW','MD','MG','MN','MO','N ','NA','NB','ND', &
!U           'NE','NI','NO','NP','O ','OS','P ','PA','PB','PD','PM','PO', &
!U           'PR','PT','PU','RA','RB','RE','RH','RN','RU','S ','SB','SC', &
!U           'SE','SI','SM','SN','SR','TA','TB','TC','TE','TH','TI','TL', &
!U           'TM','U ','V ','W ','X ','XE','Y ','YB','Z ','ZN','ZR','ZZ', &
!U           'ME','DU'/

!U! Elements (plus other CSD 'element' definitions What's 'Zz'??)
!U      DATA atnr/   6,   1,  89,  47,  13,  95,  18,  33,  85,  79,   5, &
!U             56,   4,  83,  97,  35,  20,  48,  58,  98,  17,  96,  27, &
!U             24,  55,  29,   0,  66,  68,  99,  63,   9,  26, 100,  87, &
!U             31,  64,  32,   2,  72,  80,  67,  53,  49,  77,  19,  36, &
!U             57,   3,  71,   0, 101,  12,  25,  42,   7,  11,  41,  60, &
!U             10,  28, 102,  93,   8,  76,  15,  91,  82,  46,  61,  84, &
!U             59,  78,  94,  88,  37,  75,  45,  86,  44,  16,  51,  21, &
!U             34,  14,  62,  50,  38,  73,  65,  43,  52,  90,  22,  81, &
!U             69,  92,  23,  74,   0,  54,  39,  70,   0,  30,  40,   0, &
!U              0,   0/

      INTEGER, DIMENSION(MAXBND)   :: btype
      INTEGER, DIMENSION(MAXBND,2) :: bond
! btype  = bond type
! bond   = list of bonds Iat, Jat   

      INTEGER :: natcry, nbocry
! natcry = number of atoms in list
! nbocry = number of bonds in list

      INTEGER, DIMENSION(MAXATM_2) :: ncac, nhyc
! ncac = number of connections excluding hydrogens
! nhyc = number of connections to hydrogens

      INTEGER, DIMENSION(MAXATM_2) :: hybr
! hybr      estimate of hybridisation 1 = sp1 2=sp2 3=sp3 4 = aromatic  >100 = metal

      END MODULE SAMVAR
!
!*****************************************************************************
!
