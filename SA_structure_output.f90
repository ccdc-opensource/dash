!
!*****************************************************************************
!
      SUBROUTINE SA_structure_output(T,fopt,parvals,ntotmov)

      USE VARIABLES
      USE ATMVAR
      USE ZMVAR

      IMPLICIT NONE

      DOUBLE PRECISION t, fopt
      DOUBLE PRECISION parvals(*) ! The current torsion parameters (can't be called X here)
      INTEGER ntotmov

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      REAL            XATOPT
      COMMON /posopt/ XATOPT(3,MaxAtm_3)

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      REAL                pdbAtmCoords
      COMMON /PDBOVERLAP/ pdbAtmCoords(1:3,1:maxatm,1:maxcopies,1:maxfrg,1:MaxRun)

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      DOUBLE PRECISION f2cpdb
      COMMON /pdbcat/ f2cpdb(3,3)

      INTEGER              iMyExit, num_new_min
      COMMON / CMN000001 / iMyExit, num_new_min

      REAL qvals(4), qnrm
! Use standard PDB orthogonalisation
      LOGICAL tSavePDB, tSaveCSSR, tSaveCCL, tSaveCIF, tSaveRES
      INTEGER ipcount, iScat, tElement, k1
      LOGICAL, EXTERNAL :: SavePDB, SaveCSSR, SaveCCL, SaveCIF, SaveRES
      INTEGER hFileCSSR, hFilePDB, hFileCCL, hFileCIF, hFileRES
      INTEGER I, J, II, K, iiact, iTotal, iFrg, iFrgCopy, IJ, iOrig
      REAL    xc, yc, zc
      INTEGER TotNumBonds, NumOfAtomsSoFar, iBond1, iBond2, iTem, tLen, iRadSelection
      INTEGER tLen1, tLen2
      CHARACTER(MaxPathLength) tFileName
      CHARACTER(8) TemperatureStr
      CHARACTER*80 tString, tString1, tString2
      CHARACTER*2  LATT
      CHARACTER*1, EXTERNAL :: ChrLowerCase
      REAL, EXTERNAL :: UnitCellVolume
      INTEGER, EXTERNAL :: ElmSymbol2CSD
      INTEGER NumOfAtmPerElm(1:MaxElm)
      DOUBLE PRECISION inv(3,3)
      CHARACTER*20, EXTERNAL :: Integer2String

      IF (T .GT. 999.9) THEN
        TemperatureStr = 'T=******'
      ELSE
        WRITE (TemperatureStr,"('T=',F6.2)") SNGL(T)
      ENDIF
! When iMyExit = 5, we are going to view the structure. Just write out .pdb
      IF (iMyExit .EQ. 5) THEN
        tSavePDB  = .TRUE.
        tSaveCSSR = .FALSE.
        tSaveCCL  = .FALSE.
        tSaveCIF  = .FALSE.
        tSaveRES  = .FALSE.
      ELSE
! Just in case the user decides to change this in the options menu just while we are in this routine:
! make local copies of the variables that determine which files to save.
        tSavePDB  = SavePDB()
        tSaveCSSR = SaveCSSR()
        tSaveCCL  = SaveCCL()
        tSaveCIF  = SaveCIF()
        tSaveRES  = SaveRES()
      ENDIF
!
!       Output a CSSR file to fort.64
!       Output a PDB  file to fort.65
!       Output a CCL  file to fort.66
!       Output a CIF  file to fort.67
!       Output a RES  file to fort.68
!
!       Write the file headers first
!
! CSSR ...
      IF (tSaveCSSR) THEN
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.cssr'
        hFileCSSR = 64
        OPEN (UNIT=hFileCSSR,FILE=tFileName(1:OFBN_Len+9),STATUS='unknown')
        WRITE (hFileCSSR,1000,ERR=999) (CellPar(ii),ii=1,3)
 1000   FORMAT (' REFERENCE STRUCTURE = 00000   A,B,C =',3F8.3)
        WRITE (hFileCSSR,1010) (CellPar(ii),ii=4,6), SGNumStr(NumberSGTable)(1:3)
 1010   FORMAT ('   ALPHA,BETA,GAMMA =',3F8.3,'    SPGR = ',A3)
        WRITE (hFileCSSR,"(' ',I3,'   0  DASH solution')") natom
        WRITE (hFileCSSR,1030) TemperatureStr, -SNGL(fopt), CHIPROBEST, ntotmov
 1030   FORMAT (' ',A8,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
      ENDIF
! PDB ...
      IF (tSavePDB) THEN
        IF (iMyExit .EQ. 5) THEN
          tFileName = 'SA_best.pdb'
        ELSE
          tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.pdb'
        ENDIF
        hFilePDB = 65
        OPEN (UNIT=hFilePDB,FILE=tFileName,STATUS='unknown')
        CALL sagminv(f2cpdb,inv,3)
! Add in a Header record
        WRITE (hFilePDB,"('HEADER    PDB Solution File generated by DASH')")
        WRITE (hFilePDB,1040) TemperatureStr, -SNGL(fopt), CHIPROBEST, ntotmov
 1040   FORMAT ('REMARK ',A8,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        WRITE (hFilePDB,1050) (CellPar(ii),ii=1,6), SGHMaStr(NumberSGTable)
 1050   FORMAT ('CRYST1',3F9.3,3F7.2,X,A12)
! JCC Add in V2 pdb records to store space group and symmetry
        WRITE (hFilePDB,1380)
        WRITE (hFilePDB,"('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')")
        WRITE (hFilePDB,1382) SGHMaStr(NumberSGTable)
 1382   FORMAT ('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
        WRITE (hFilePDB,1380)
        WRITE (hFilePDB,"('REMARK 290      SYMOP   SYMMETRY')")
        WRITE (hFilePDB,"('REMARK 290     NNNMMM   OPERATOR')")
        DO i = 1, npdbops
          WRITE (hFilePDB,1385) (i*1000+555), cpdbops(i)
 1385     FORMAT ('REMARK 290',5X,I6,3X,A)
        ENDDO
        WRITE (hFilePDB,1380)
        WRITE (hFilePDB,"('REMARK 290     WHERE NNN -> OPERATOR NUMBER')")
        WRITE (hFilePDB,"('REMARK 290           MMM -> TRANSLATION VECTOR')")
        WRITE (hFilePDB,1380)
        WRITE (hFilePDB,"('REMARK 290 REMARK:')")
        WRITE (hFilePDB,1060) inv(1,1), inv(1,2), inv(1,3)
 1060   FORMAT ('SCALE1    ',3F10.5,'      0.00000')
        WRITE (hFilePDB,1070) inv(2,1), inv(2,2), inv(2,3)
 1070   FORMAT ('SCALE2    ',3F10.5,'      0.00000')
        WRITE (hFilePDB,1080) inv(3,1), inv(3,2), inv(3,3)
 1080   FORMAT ('SCALE3    ',3F10.5,'      0.00000')
      ENDIF
! CCL ...
      IF (tSaveCCL) THEN
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.ccl'
        hFileCCL = 66
        OPEN (UNIT=hFileCCL,FILE=tFileName(1:OFBN_Len+8),STATUS='unknown')
        WRITE (hFileCCL,1090) TemperatureStr, -SNGL(fopt), CHIPROBEST, ntotmov
 1090   FORMAT ('Z ',A8,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        WRITE (hFileCCL,1100) (CellPar(ii),ii=1,6)
 1100   FORMAT ('C ',6F10.5)
      ENDIF
! CIF ...
      IF (tSaveCIF) THEN
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.cif'
        hFileCIF = 67
        OPEN (UNIT=hFileCIF,FILE=tFileName(1:OFBN_Len+8),STATUS='unknown')
        WRITE (hFileCIF,'("data_global")')
        tString = CrystalSystemString(LatBrav)
        tString(1:1) = ChrLowerCase(tString(1:1))
        tLen = LEN_TRIM(tString)
! Remove '-a' etc. from monoclinic
        IF (LatBrav .EQ. 2 .OR. LatBrav .EQ. 3 .OR. LatBrav .EQ. 4) tLen = tLen -2
        WRITE (hFileCIF,'("_symmetry_cell_setting ",A)') tString(1:tLen)
! Following line: issues with ":" in certain space group names
        tString = SGHMaStr(NumberSGTable)
        tLen = LEN_TRIM(tString)
        DO WHILE ((tLen .GT. 1) .AND. (tString(tLen:tLen) .NE. ':'))
          tLen = tLen - 1
        ENDDO
        IF (tString(tLen:tLen) .EQ. ':') THEN
          tLen = tLen - 1
        ELSE
          tLen = LEN_TRIM(tString)
        ENDIF
        tString = "'"//tString(1:tLen)//"'"
        tLen = tLen + 2
        WRITE (hFileCIF,"('_symmetry_space_group_name_H-M ',A)") tString(1:tLen)
        WRITE (hFileCIF,'("loop_")')
        WRITE (hFileCIF,'("  _symmetry_equiv_pos_as_xyz")')
        DO i = 1, npdbops
          tString = cpdbops(i)
          tLen = LEN_TRIM(tString)
          tString = "  '"//tString(1:tLen)//"'"
          tLen = tLen + 4
          WRITE (hFileCIF,"(A)") tString(1:tLen)
        ENDDO
        WRITE (hFileCIF,'("_cell_length_a    ",F8.4)') CellPar(1)
        WRITE (hFileCIF,'("_cell_length_b    ",F8.4)') CellPar(2)
        WRITE (hFileCIF,'("_cell_length_c    ",F8.4)') CellPar(3)
        WRITE (hFileCIF,'("_cell_angle_alpha ",F8.4)') CellPar(4)
        WRITE (hFileCIF,'("_cell_angle_beta  ",F8.4)') CellPar(5)
        WRITE (hFileCIF,'("_cell_angle_gamma ",F8.4)') CellPar(6)
        WRITE (hFileCIF,'("_cell_volume",F7.1)') UnitCellVolume(CellPar(1),CellPar(2),CellPar(3),CellPar(4),CellPar(5),CellPar(6))
        WRITE (hFileCIF,'("_cell_formula_units_Z  ?")')
        SELECT CASE (JRadOption)
          CASE (1) ! X-ray lab data
            CALL PushActiveWindowID
            CALL WDialogSelect(IDD_Data_Properties)
            CALL WDialogGetMenu(IDF_Wavelength_Menu,iRadSelection)
            CALL PopActiveWindowID
            SELECT CASE (iRadSelection)
! Winteracter menu:
!     1 = <...>
!     2 = Cu      <==  DEFAULT
!     3 = Mo
!     4 = Co
!     5 = Cr
!     6 = Fe
              CASE (1)
                tString = 'Xx'
              CASE (2)
                tString = 'Cu'
              CASE (3)
                tString = 'Mo'
              CASE (4)
                tString = 'Co'
              CASE (5)
                tString = 'Cr'
              CASE (6)
                tString = 'Fe'
            END SELECT
            tString = "'"//tString(1:2)//" K\a'"
          CASE (2) ! X-ray synchrotron data
            tString = 'synchrotron'
          CASE (3) ! Constant Wavelength Neutrons
          CASE (4) ! Time-of-Flight Neutrons
        END SELECT
        tLen = LEN_TRIM(tString)
        WRITE (hFileCIF,'("_diffrn_radiation_type ",A)') tString(1:tLen)
        WRITE (hFileCIF,'("_diffrn_radiation_wavelength",F10.5)') ALambda
        WRITE (hFileCIF,'("_refine_ls_goodness_of_fit_all ",F7.3)') SQRT(MAX(0.0,-SNGL(fopt)))

!C data_FILENAME
!C _symmetry_cell_setting            monoclinic
!C _symmetry_space_group_name_H-M    'P 1 21/n 1'
!C loop_
!C _symmetry_equiv_pos_as_xyz
!C   '   +x,   +y,   +z'
!C   '1/2-x,1/2+y,1/2-z'
!C   '   -x,   -y,   -z'
!C   'x+1/2,1/2-y,1/2+z'
!C _cell_length_a                    20.9674(3)
!C _cell_length_b                    14.5059(2)
!C _cell_length_c                    10.9862(1)
!C _cell_angle_alpha                 90.0
!C _cell_angle_beta                  117.825(2)
!C _cell_angle_gamma                 90.0
!C _cell_volume                      2955.0
!C _cell_formula_units_Z             4
!C _diffrn_radiation_wavelength      0.80008
!C # The following item is the same as CHI, the square root of 'CHI
!C squared'
!C _refine_ls_goodness_of_fit_all    3.26
!C # 9. ATOMIC COORDINATES AND DISPLACEMENT PARAMETERS
!C loop_
!C        _atom_site_label
!C        _atom_site_fract_x
!C        _atom_site_fract_y
!C        _atom_site_fract_z
!C        _atom_site_occupancy
!C        _atom_site_adp_type
!C        _atom_site_B_iso_or_equiv
!C      C1     -0.10853   0.45223   0.14604  1.0 Biso 3.0
!C      C2     -0.05898   0.41596   0.27356  1.0 Biso 3.0

        WRITE (hFileCIF,'("loop_")')
        WRITE (hFileCIF,'("  _atom_site_label")')
        WRITE (hFileCIF,'("  _atom_site_fract_x")')
        WRITE (hFileCIF,'("  _atom_site_fract_y")')
        WRITE (hFileCIF,'("  _atom_site_fract_z")')
        WRITE (hFileCIF,'("  _atom_site_occupancy")')
        WRITE (hFileCIF,'("  _atom_site_adp_type")')
        WRITE (hFileCIF,'("  _atom_site_B_iso_or_equiv")')
      ENDIF
! RES ...
      IF (tSaveRES) THEN
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.res'
        hFileRES = 68
        OPEN (UNIT=hFileRES,FILE=tFileName(1:OFBN_Len+8),STATUS='unknown')
        WRITE (hFileRES,"('TITL Solution File generated by DASH')")
        WRITE (hFileRES,1031) TemperatureStr, -SNGL(fopt), CHIPROBEST, ntotmov
 1031   FORMAT ('REM  ',A8,', chi**2=',F7.2,', profile chi**2=',F7.2,' after ',I8,' moves')
        WRITE (hFileRES,1032,ERR=999) ALambda, (CellPar(ii),ii=1,6)
 1032   FORMAT ('CELL ',F7.5,1X,6(F8.4,1X))
        WRITE (hFileRES,'("ZERR ",I2," 0.000 0.000 0.000 0.000 0.000 0.000")',ERR=999) npdbops
        IF (SGShmStr(NumberSGTable)(3:3) .EQ. 'C') THEN
          LATT(1:1) = ' '
        ELSE
          LATT(1:1) = '-'
        ENDIF
        SELECT CASE (SGShmStr(NumberSGTable)(1:1))
          CASE ('P')
            LATT(2:2) = '1'
          CASE ('I')
            LATT(2:2) = '2'
          CASE ('R') ! See Int. Tables page 5
            LATT(2:2) = '3'
          CASE ('F')
            LATT(2:2) = '4'
          CASE ('A')
            LATT(2:2) = '5'
          CASE ('B')
            LATT(2:2) = '6'
          CASE ('C')
            LATT(2:2) = '7'
        END SELECT
        WRITE (hFileRES,'(A)',ERR=999) 'LATT '//LATT
!C LATT N [1] 
!C Lattice type: 1=P, 2=I, 3=rhombohedral obverse on hexagonal axes, 4=F, 5=A, 6=B, 7=C. N must be made
!C negative if the structure is non-centrosymmetric. 
        IF (npdbops .GE. 2) THEN
          DO i = 2, npdbops ! " +X, +Y, +Z" must be left out
            tString = cpdbops(i)
            tLen = LEN_TRIM(tString)
            WRITE (hFileRES,"('SYMM ',A)",ERR=999) tString(1:tLen)
          ENDDO
        ENDIF
        NumOfAtmPerElm = 0
        DO iFrg = 1, maxfrg
          IF (gotzmfile(iFrg)) THEN
            DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
              DO i = 1, natoms(iFrg)
                CALL INC(NumOfAtmPerElm(ElmSymbol2CSD(asym(i,iFrg)(1:2))))
              ENDDO
            ENDDO
          ENDIF
        ENDDO
        tString1 = 'SFAC'
        tString2 = 'UNIT'
        tLen1 = 4
        tLen2 = 4
        DO i = 1, MaxElm
          IF (NumOfAtmPerElm(i) .GE. 1) THEN
            tString1 = tString1(1:tLen1)//'  '//ElementStr(i)
            tLen1 = tLen1 + 4
            WRITE (tString2(tLen2+1:tLen2+4),'(I4)') NumOfAtmPerElm(i)*npdbops
            tLen2 = tLen2 + 4
          ENDIF
        ENDDO
        WRITE (hFileRES,'(A)',ERR=999) tString1(1:tLen1)
        WRITE (hFileRES,'(A)',ERR=999) tString2(1:tLen2)
      ENDIF
      iiact = 0
      itotal = 0
      ipcount = 0
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            itotal = iiact
! Write out the translation/rotation information for each residue
            IF (tSavePDB) THEN
              WRITE (hFilePDB,1039) iFrg
 1039         FORMAT ('REMARK Start of molecule number ',I6)
              WRITE (hFilePDB,1037) (SNGL(parvals(ij)),ij=ipcount+1,ipcount+3)
 1037         FORMAT ('REMARK Translations: ',3F10.6)
            ENDIF
            IF (natoms(iFrg) .GT. 1) THEN
! Normalise the Q-rotations before writing them out ...
              qvals(1) = SNGL(parvals(ipcount+4))
              qvals(2) = SNGL(parvals(ipcount+5))
              qvals(3) = SNGL(parvals(ipcount+6))
              qvals(4) = SNGL(parvals(ipcount+7))
              qnrm = SQRT(qvals(1)**2 + qvals(2)**2 + qvals(3)**2 + qvals(4)**2)
              qvals = qvals / qnrm
              IF (tSavePDB) THEN
                WRITE (hFilePDB,1038) (qvals(ij),ij=1,4)
 1038           FORMAT ('REMARK Q-Rotations : ',4F10.6)
              ENDIF
              ipcount = ipcount + izmpar(iFrg)
            ENDIF
            DO i = 1, natoms(iFrg)
              iiact = iiact + 1
              ii = itotal + izmbid(i,iFrg)
              iorig = izmbid(i,iFrg)
! The CSSR atom lines
              IF (tSaveCSSR) THEN
                WRITE (hFileCSSR,1110) iiact, OriginalLabel(iOrig,iFrg)(1:4),(xatopt(k,ii),k=1,3), 0, 0, 0, 0, 0, 0, 0, 0, 0.0
 1110           FORMAT (I4,1X,A4,2X,3(F9.5,1X),8I4,1X,F7.3)
              ENDIF
! The PDB atom lines
              xc = xatopt(1,ii)*SNGL(f2cpdb(1,1)) + xatopt(2,ii)*SNGL(f2cpdb(1,2)) + xatopt(3,ii)*SNGL(f2cpdb(1,3))
              yc =                                  xatopt(2,ii)*SNGL(f2cpdb(2,2)) + xatopt(3,ii)*SNGL(f2cpdb(2,3))
              zc =                                                                   xatopt(3,ii)*SNGL(f2cpdb(3,3))
! Note that elements are right-justified
! WebLab viewer even wants the elements in the atom names to be right justified.
              IF (tSavePDB) THEN
                IF (asym(iOrig,iFrg)(2:2).EQ.' ') THEN
                  WRITE (hFilePDB,1120) iiact, OriginalLabel(iOrig,iFrg)(1:3), xc, yc, zc, &
                                  occ(iOrig,iFrg), tiso(iOrig,iFrg), asym(iOrig,iFrg)(1:1)
 1120             FORMAT ('HETATM',I5,'  ',A3,' NON     1    ',3F8.3,2F6.2,'           ',A1,'  ')
                ELSE
                  WRITE (hFilePDB,1130) iiact, OriginalLabel(iOrig,iFrg)(1:4), xc, yc, zc, &
                                  occ(iOrig,iFrg), tiso(iOrig,iFrg), asym(iOrig,iFrg)(1:2)
 1130             FORMAT ('HETATM',I5,' ',A4,' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
                ENDIF
                pdbAtmCoords(1,iOrig,iFrgCopy,iFrg,Curr_SA_Run) = xc
                pdbAtmCoords(2,iOrig,iFrgCopy,iFrg,Curr_SA_Run) = yc
                pdbAtmCoords(3,iOrig,iFrgCopy,iFrg,Curr_SA_Run) = zc
              ENDIF
! The CCL atom lines
              IF (tSaveCCL) THEN
                WRITE (hFileCCL,1033) asym(iOrig,iFrg), (xatopt(k,ii),k=1,3), tiso(iOrig,iFrg), occ(iOrig,iFrg) 
 1033           FORMAT ('A ',A3,' ',F10.5,1X,F10.5,1X,F10.5,1X,F4.2,1X,F4.2)
              ENDIF
! The CIF atom lines
!C # 9. ATOMIC COORDINATES AND DISPLACEMENT PARAMETERS
!C loop_
!C        _atom_site_label
!C        _atom_site_fract_x
!C        _atom_site_fract_y
!C        _atom_site_fract_z
!C        _atom_site_occupancy
!C        _atom_site_adp_type
!C        _atom_site_B_iso_or_equiv
!C      C1     -0.10853   0.45223   0.14604  1.0 Biso 3.0
!C      C2     -0.05898   0.41596   0.27356  1.0 Biso 3.0
              IF (tSaveCIF) THEN
                WRITE (hFileCIF,1034) OriginalLabel(iOrig,iFrg), (xatopt(k,ii),k=1,3), occ(iOrig,iFrg), tiso(iOrig,iFrg) 
 1034           FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Biso ',F4.2)
              ENDIF
              IF (tSaveRES) THEN
! Determine this atom's entry number in the scattering factor list
                tElement = ElmSymbol2CSD(asym(iOrig,iFrg)(1:2))
                iScat = 0
                DO k1 = 1, tElement
                  IF (NumOfAtmPerElm(k1) .NE. 0) iScat = iScat + 1
                ENDDO
                WRITE (hFileRES,1035) OriginalLabel(iOrig,iFrg), iScat, (xatopt(k,ii),k=1,3), &
                                      occ(iOrig,iFrg), tiso(iOrig,iFrg)/(8.0*(PI**2)) 
 1035           FORMAT (A5,1X,I2,1X,3(F10.5,1X),F5.3,1X,F5.3)
              ENDIF
            ENDDO ! loop over atoms
          ENDDO ! Loop over copies
        ENDIF
      ENDDO ! loop over Z-matrices
      IF (tSaveCSSR) CLOSE (hFileCSSR)
      IF (tSavePDB) THEN
! Per Z-matrix, write out the connectivity.
        TotNumBonds = 0
        NumOfAtomsSoFar = 0
        DO ifrg = 1, maxfrg
          IF (gotzmfile(iFrg)) THEN
            DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
              IF (NumberOfBonds(iFrg) .GT. 0) THEN
                DO J = 1, NumberOfBonds(iFrg)
! Due to the backmapping, it is possible that the original number of the first atom is greater than the
! original number of the second atom. Mercury can't always read pdb files where this is the case.
                  iBond1 = izmoid(Bonds(1,J,iFrg),iFrg)+NumOfAtomsSoFar
                  iBond2 = izmoid(Bonds(2,J,iFrg),iFrg)+NumOfAtomsSoFar
                  IF (iBond1 .GT. iBond2) THEN
                    iTem   = iBond1
                    iBond1 = iBond2
                    iBond2 = iTem
                  ENDIF
                  WRITE(hFilePDB,'(A6,I5,I5)') 'CONECT', iBond1, iBond2
                ENDDO
              ENDIF
              NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
              TotNumBonds = TotNumBonds + NumberOfBonds(iFrg)
            ENDDO
          ENDIF
        ENDDO ! loop over Z-matrices
        WRITE (hFilePDB,"('END')")
        CLOSE (hFilePDB)
      ENDIF
      IF (tSaveCCL) CLOSE (hFileCCL)
      IF (tSaveCIF) CLOSE (hFileCIF)
      IF (tSaveRES) THEN
        WRITE (hFileRES,"('END ')")
        CLOSE (hFileRES)
      ENDIF
      RETURN
 1380 FORMAT ('REMARK 290 ')
  999 CALL ErrorMessage('Error while writing SA output files.')

      END SUBROUTINE SA_STRUCTURE_OUTPUT
!
!*****************************************************************************
!
      SUBROUTINE SA_STRUCTURE_OUTPUT_OVERLAP

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      REAL                pdbAtmCoords
      COMMON /PDBOVERLAP/ pdbAtmCoords(1:3,1:maxatm,1:maxcopies,1:maxfrg,1:MaxRun)

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )
      INTEGER         npdbops
      CHARACTER*20             cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult
! SA_Run_Number now holds the number of the last completed multirun

! Use standard PDB orthogonalisation
      DOUBLE PRECISION f2cpdb
      COMMON /pdbcat/ f2cpdb(3,3)

      DOUBLE PRECISION inv(3,3)
      INTEGER RunNr, TickedRunNr, NumOfOverlaidStructures
      INTEGER pdbBond(1:maxbnd_2*maxcopies*maxfrg,1:2)
      INTEGER TotNumBonds, NumOfAtomsSoFar
      CHARACTER*4 LabelStr
      CHARACTER*2 ColourStr
      CHARACTER*2 RunStr
      LOGICAL, EXTERNAL :: ChrIsLetter
      INTEGER AtomLabelOption, AtomColourOption, RangeOption
      INTEGER GridRowNr
      CHARACTER(MaxPathLength) tString
      INTEGER II, I, iFrg, iFrgCopy, J, iiact, ISTATUS, BondNr, ilen
      REAL xc, yc, zc
      LOGICAL UseThisSolution
      INTEGER Limit1, Limit2
      INTEGER tInteger

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
! Write the file headers first
      OPEN (UNIT=65,FILE='Overlap_Temp.pdb',STATUS='unknown',ERR=999)
! JCC included again
      CALL sagminv(f2cpdb,inv,3)
! Add in a Header record
      WRITE (65,1036,ERR=999)
 1036 FORMAT ('HEADER    PDB Solution File generated by DASH')
      WRITE (65,1050,ERR=999) (CellPar(ii),ii=1,6), SGHMaStr(NumberSGTable)
 1050 FORMAT ('CRYST1',3F9.3,3F7.2,X,A12)
! JCC Add in V2 pdb records to store space group and symmetry
      WRITE (65,1380,ERR=999)
      WRITE (65,1381,ERR=999)
 1381 FORMAT ('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')
      WRITE (65,1382,ERR=999) SGHMaStr(NumberSGTable)
 1382 FORMAT ('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
      WRITE (65,1380,ERR=999)
      WRITE (65,1383,ERR=999)
 1383 FORMAT ('REMARK 290      SYMOP   SYMMETRY')
      WRITE (65,1384,ERR=999)
 1384 FORMAT ('REMARK 290     NNNMMM   OPERATOR')
      DO i = 1, npdbops
        WRITE (65,1385,ERR=999) (i*1000+555), cpdbops(i)
 1385   FORMAT ('REMARK 290',5X,I6,3X,A)
      ENDDO
      WRITE (65,1380,ERR=999)
      WRITE (65,1386,ERR=999)
 1386 FORMAT ('REMARK 290     WHERE NNN -> OPERATOR NUMBER')
      WRITE (65,1387,ERR=999)
 1387 FORMAT ('REMARK 290           MMM -> TRANSLATION VECTOR')
      WRITE (65,1380,ERR=999)
      WRITE (65,1388,ERR=999)
 1388 FORMAT ('REMARK 290 REMARK:')
! JCC included again
      WRITE (65,1060,ERR=999) inv(1,1), inv(1,2), inv(1,3)
 1060 FORMAT ('SCALE1    ',3F10.5,'      0.00000')
      WRITE (65,1070,ERR=999) inv(2,1), inv(2,2), inv(2,3)
 1070 FORMAT ('SCALE2    ',3F10.5,'      0.00000')
      WRITE (65,1080,ERR=999) inv(3,1), inv(3,2), inv(3,3)
 1080 FORMAT ('SCALE3    ',3F10.5,'      0.00000')
! Per Z-matrix, determine the connectivity. This has to be done only once.
      TotNumBonds = 0
      NumOfAtomsSoFar = 0
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            IF (NumberOfBonds(iFrg) .GT. 0) THEN
              DO J = 1, NumberOfBonds(iFrg)
                pdbBond(J+TotNumBonds,1) = Bonds(1,J,iFrg) + NumOfAtomsSoFar
                pdbBond(J+TotNumBonds,2) = Bonds(2,J,iFrg) + NumOfAtomsSoFar
              ENDDO
            ENDIF
            NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
            TotNumBonds = TotNumBonds + NumberOfBonds(iFrg)
          ENDDO
        ENDIF
      ENDDO ! loop over Z-matrices
! Get atom label option from dialogue. Two options: 
! 1. "Element symbol + solution number"
! 2. "Orignal atom labels"
      CALL WDialogGetRadioButton(IDF_UseSolutionNr,AtomLabelOption)
! Get atom colour option from dialogue. Two options: 
! 1. "By solution number"
! 2. "By element"
      CALL WDialogGetRadioButton(IDF_ColourBySolution,AtomColourOption)
! Get atom colour option from dialogue. Two options: 
! 1. "Show Solutions p through q"
! 2. "Show Selected"
      CALL WDialogGetRadioButton(IDF_ShowRange,RangeOption)
      IF (RangeOption .EQ. 1) THEN ! "Show Solutions p through q"
        CALL WDialogGetInteger(IDF_Limit1,Limit1)
        CALL WDialogGetInteger(IDF_Limit2,Limit2)
        IF (Limit1 .GT. Limit2) THEN
          tInteger = Limit2
          Limit2 = Limit1
          Limit1 = tInteger
        ENDIF
      ENDIF
! JvdS Oct 2001
! Note that for the following code--which can colour an atom assigning a dummy element while retaining
! the original atom label even if this contains the contradictory element symbol--relies
! on the viewer reading .pdb files as specified on the .pdb file webpage. Sounds normal,
! but it turns out that WebLabViewer doesn't do this. So, for this piece of code to work, we
! really need Mercury.
! That's not too much of a restriction, because WebLabViewer cannot cope with the overlaying atoms
! anyway (it will recalculate bonds even when CONECT records are provided), so WebLabViewer
! will always show a mess, irrespective of the exact contents of the .pdb file.
! In short: works only with Mercury.
      iiact = 0
      TickedRunNr = 0
      NumOfOverlaidStructures = 0
      DO GridRowNr = 1, NumOf_SA_Runs
        IF (RangeOption .EQ. 1) THEN ! "Show Solutions p through q"
          UseThisSolution = ((GridRowNr .GE. Limit1) .AND. (GridRowNr .LE. Limit2))
        ELSE ! "Show Selected"
          CALL WGridGetCellCheckBox(IDF_SA_summary,3,GridRowNr,istatus)
          UseThisSolution = (istatus .EQ. 1)
        ENDIF
        IF (UseThisSolution) THEN
          NumOfOverlaidStructures = NumOfOverlaidStructures + 1
! The solutions have been ordered wrt chi**2. We must parse the original run nr from the
! number of the .pdb file.
          CALL WGridGetCellString(IDF_SA_Summary,1,GridRowNr,tString)
          ilen = LEN_TRIM(tString)
          RunStr = tString(ilen-5:ilen-4)
          IF (RunStr(1:1) .EQ. '0') THEN
            RunStr(1:1) = RunStr(2:2)
            RunStr(2:2) = ' '
          ENDIF
          READ(RunStr,'(I2)',ERR=999) RunNr
          TickedRunNr = TickedRunNr + 1 ! Number of ticked runs, counter used for choosing the colour
          IF (TickedRunNr .EQ. 11) TickedRunNr = 1 ! Re-use colours.
! Note that elements are right-justified
          IF (AtomColourOption .EQ. 1) THEN ! Colour by solution
            SELECT CASE (TickedRunNr)
              CASE ( 1)
                ColourStr = 'Co'  ! Cobalt        Blue
              CASE ( 2)
                ColourStr = ' O'  ! Oxygen        Red
              CASE ( 3)
                ColourStr = ' S'  ! Sulphur       Yellow
              CASE ( 4)
                ColourStr = 'Cl'  ! Chlorine      Green
              CASE ( 5)
                ColourStr = ' N'  ! Nitrogen      Light blue
              CASE ( 6)
                ColourStr = 'Br'  ! Bromine       Brown
              CASE ( 7)
                ColourStr = ' I'  ! Iodine        Pink
              CASE ( 8)
                ColourStr = ' C'  ! Carbon        Grey
              CASE ( 9)
                ColourStr = ' H'  ! Hydrogen      White
              CASE (10)
                ColourStr = ' P'  ! Phosphorus
            END SELECT
          ENDIF
          WRITE(RunStr,'(I2)',ERR=999) GridRowNr
          CALL StrClean(RunStr,ilen) ! Left justify
          LabelStr = LabelStr(1:LEN_TRIM(LabelStr))//RunStr
          DO iFrg = 1, maxfrg
            IF (gotzmfile(iFrg)) THEN
              DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
                DO i = 1, natoms(iFrg)
                  iiact = iiact + 1
                  xc = pdbAtmCoords(1,i,iFrgCopy,iFrg,RunNr)
                  yc = pdbAtmCoords(2,i,iFrgCopy,iFrg,RunNr)
                  zc = pdbAtmCoords(3,i,iFrgCopy,iFrg,RunNr)
! Note that elements are right-justified
                  IF (AtomColourOption .EQ. 2) THEN ! Colour by Element
                    IF (asym(i,iFrg)(2:2) .EQ. ' ') THEN
                      ColourStr(1:2) = ' '//asym(i,iFrg)(1:1)
                    ELSE
                      ColourStr(1:2) = asym(i,iFrg)(1:2)
                    ENDIF
                  ENDIF
                  IF (AtomLabelOption .EQ. 1) THEN ! Element symbol + solution number
                    LabelStr = asym(i,iFrg)(1:LEN_TRIM(asym(i,iFrg)))//RunStr
                  ELSE  ! Orignal atom labels
                    LabelStr(1:4) = OriginalLabel(i,ifrg)(1:4)
                  ENDIF
                  WRITE (65,1120,ERR=999) iiact, LabelStr(1:4), xc, yc, zc, occ(i,iFrg), tiso(i,iFrg), ColourStr(1:2)
 1120             FORMAT ('HETATM',I5,' ',A4' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
                ENDDO ! loop over atoms
              ENDDO
            ENDIF
          ENDDO ! loop over Z-matrices
        ENDIF ! Was this solution ticked to be displayed?
      ENDDO ! loop over runs
      DO RunNr = 1, NumOfOverlaidStructures
        DO BondNr = 1, TotNumBonds
          WRITE(65,'(A6,I5,I5)',ERR=999) 'CONECT', (pdbBond(BondNr,1)+NATOM*(RunNr-1)), (pdbBond(BondNr,2)+NATOM*(RunNr-1))
        ENDDO
      ENDDO ! loop over runs
      WRITE (65,"('END')",ERR=999)
      CLOSE (65)
      CALL ViewStructure('Overlap_Temp.pdb')
      CALL PopActiveWindowID
      RETURN
 1380 FORMAT ('REMARK 290 ')
  999 CALL ErrorMessage('Error writing temporary file.')
      CLOSE (65)
      CALL PopActiveWindowID

      END SUBROUTINE SA_STRUCTURE_OUTPUT_OVERLAP
!
!*****************************************************************************
!
      SUBROUTINE SAGMINV(A,B,N)

      DIMENSION II(100), IL(100), IG(100)
      REAL*8 A(N,N), B(N,N)

      CALL SAGMEQ(A,B,N,N)
      D = 1.0
      IS = N - 1
      DO K = 1, N
        IL(K) = 0
        IG(K) = K
      ENDDO
      DO K = 1, N
        R = 0.
        DO I = 1, N
          IF (IL(I).NE.0) GOTO 40
          W = B(I,K)
          X = ABS(W)
          IF (R.GT.X) GOTO 40
          R = X
          P = W
          KF = I
   40   ENDDO
        II(K) = KF
        IL(KF) = KF
        D = D*P
!      IF (D .EQ. 0.) write(*,*) 'Zero determinant'
        DO I = 1, N
          IF (I.EQ.KF) THEN
            B(I,K) = 1./P
          ELSE
            B(I,K) = -B(I,K)/P
          ENDIF
        ENDDO
        DO J = 1, N
          IF (J.EQ.K) GOTO 140
          W = B(KF,J)
          IF (W.EQ.0.) GOTO 140
          DO I = 1, N
            IF (I.EQ.KF) THEN
              B(I,J) = W/P
            ELSE
              B(I,J) = B(I,J) + W*B(I,K)
            ENDIF
          ENDDO
  140   ENDDO
      ENDDO
      DO K = 1, IS
        KF = II(K)
        KL = IL(KF)
        KG = IG(K)
        IF (KF.EQ.KG) GOTO 190
        DO I = 1, N
          R = B(I,KF)
          B(I,KF) = B(I,KG)
          B(I,KG) = R
        ENDDO
        DO J = 1, N
          R = B(K,J)
          B(K,J) = B(KL,J)
          B(KL,J) = R
        ENDDO
        IL(KF) = K
        IL(KG) = KL
        IG(KL) = IG(K)
        IG(K) = KF
        D = -D
  190 ENDDO

      END SUBROUTINE SAGMINV
!
!*****************************************************************************
!
      SUBROUTINE SAGMEQ(A,B,NI,NJ)
!
!H Sets matrix B = matrix A.
!A On entry A is a real matrix of dimension NIxNJ
!A On exit  B is a real matrix equal to A
!N NI and NJ must be at least 1
!
      REAL*8 A(NI,NJ), B(NI,NJ)

      DO I = 1, NI
        DO J = 1, NJ
          B(I,J) = A(I,J)
        ENDDO
      ENDDO

      END SUBROUTINE SAGMEQ
!
!*****************************************************************************
!
      SUBROUTINE PDB_SymmRecords()

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)

      CHARACTER*50 stout

      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

      REAL rtmp(4,4)
!   ep added common block.   Used by Align
      COMMON /fullsymmops/ rpdb(4,4,mpdbops)
      LOGICAL cmp
      LOGICAL PDB_CmpMat

! Expand the symmetry generators into a list of symm ops by cross-multiplication
      DO i = 1, 4
        DO j = 1, 4
          rpdb(i,j,1) = 0.0
        ENDDO
        rpdb(i,i,1) = 1.0
      ENDDO
      DO k = 1, nsymmin
        DO j = 1, 4
          DO i = 1, 4
            rpdb(i,j,k+1) = symmin(i,j,k)
          ENDDO
        ENDDO
        CALL PDB_PosTrans(rpdb(1,1,k+1))
      ENDDO
      npdbops = nsymmin + 1
      ilast = 0
      iprev = 1
      DO WHILE (ilast.LT.npdbops .AND. npdbops.LE.mpdbops)
        ilast = iprev
        iprev = npdbops + 1
        DO i = 1, npdbops
          DO j = ilast, npdbops
            CALL PDB_MatMul(rpdb(1,1,i),rpdb(1,1,j),rtmp)
            CALL PDB_PosTrans(rtmp)
            DO k = 1, npdbops
              cmp = PDB_CmpMat(rpdb(1,1,k),rtmp)
              IF (cmp) GOTO 11
            ENDDO
            npdbops = npdbops + 1
            DO k = 1, 4
              DO m = 1, 4
                rpdb(k,m,npdbops) = rtmp(k,m)
              ENDDO
            ENDDO
   11       CONTINUE
          ENDDO
        ENDDO
      ENDDO
      DO k = 1, npdbops
        CALL M2S_SYMCON(rpdb(1,1,k),stout,lstout)
        m = 1
        DO WHILE (stout(m:m).EQ.' ' .AND. m.LE.20)
          m = m + 1
        ENDDO
        cpdbops(k) = stout(m:20)
      ENDDO

      END SUBROUTINE PDB_SYMMRECORDS
!
!*****************************************************************************
!
      SUBROUTINE PDB_MatMul(a,b,c)

      REAL a(4,4), b(4,4), c(4,4)

      DO i = 1, 4
        DO j = 1, 4
          c(j,i) = a(j,1)*b(1,i) + a(j,2)*b(2,i) + a(j,3)*b(3,i) + a(j,4)*b(4,i)
        ENDDO
      ENDDO

      END SUBROUTINE PDB_MATMUL
!
!*****************************************************************************
!
      LOGICAL FUNCTION PDB_CmpMat(a,b)

      REAL a(4,4), b(4,4)

      PDB_CmpMat = .FALSE.
      DO i = 1, 4
        DO j = 1, 4
          IF (ABS(a(i,j)-b(i,j)).GT.0.001) RETURN
        ENDDO
      ENDDO
      PDB_CmpMat = .TRUE.

      END FUNCTION PDB_CMPMAT
!
!*****************************************************************************
!
      SUBROUTINE PDB_PosTrans(r)

      REAL r(4,4)

      DO i = 1, 3
! Tidy up any rounding errors on the translations
        r(i,4) = FLOAT(NINT(r(i,4)*10000.0))/10000.0
        IF (r(i,4).LT.-0.01) THEN
          DO WHILE (r(i,4).LT.-0.01)
            r(i,4) = r(i,4) + 1.0
          ENDDO
        ELSE
          DO WHILE (r(i,4).GT.0.999)
            r(i,4) = r(i,4) - 1.0
          ENDDO
        ENDIF
      ENDDO
      r(4,4) = 1.0

      END SUBROUTINE PDB_POSTRANS
!
!*****************************************************************************
!
