!*==SA_STRUCTURE_OUTPUT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE SA_structure_output(T,fopt,cpb,parvals,ntotmov,RunNr)

      USE VARIABLES
!
!       Called when a new minimum is found
!
      DOUBLE PRECISION t, fopt
      REAL cpb
      DOUBLE PRECISION parvals(*) ! The current torsion parameters (can't be called X here)
      INTEGER ntotmov, RunNr

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      CHARACTER*3     asym
      CHARACTER*5                          OriginalLabel
      COMMON /zmcomc/ asym(maxatm,maxfrg), OriginalLabel(maxatm,maxfrg)

      INTEGER         ntatm, natoms
      INTEGER         ioptb,                iopta,                ioptt
      INTEGER         iz1,                  iz2,                  iz3
      COMMON /zmcomi/ ntatm, natoms(maxfrg),                                             &
     &                ioptb(maxatm,maxfrg), iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),  &
     &                iz1(maxatm,maxfrg),   iz2(maxatm,maxfrg),   iz3(maxatm,maxfrg)

      INTEGER         izmpar
      CHARACTER*36                    czmpar
      INTEGER                                                kzmpar
      REAL                                                                          xzmpar
      COMMON /zmnpar/ izmpar(maxfrg), czmpar(MaxDOF,maxfrg), kzmpar(MaxDOF,maxfrg), xzmpar(MaxDOF,maxfrg)

      INTEGER         nfrag
      COMMON /frgcom/ nfrag

      DOUBLE PRECISION blen,                alph,                bet,                f2cmat
      COMMON /zmcomr/  blen(maxatm,maxfrg), alph(maxatm,maxfrg), bet(maxatm,maxfrg), f2cmat(3,3)

      REAL            tiso,                occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)

      DOUBLE PRECISION inv(3,3)

      COMMON /posopt/ XATOPT(3,150)

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
      COMMON /PDBOVERLAP/ pdbAtmCoords(1:3,1:maxatm,1:maxfrg,1:30)

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file
      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      LOGICAL         gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)

      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

! The original atom ids to list in the labels and the back mapping
      COMMON /zmjcmp/ izmoid(maxatm,maxfrg), izmbid(maxatm,maxfrg)

      REAL qvals(4), qnrm
! Use standard PDB orthogonalisation
      DOUBLE PRECISION f2cpdb
      COMMON /pdbcat/ f2cpdb(3,3)
      LOGICAL tSavePDB, tSaveCSSR, tSaveCCL
      INTEGER ipcount
      LOGICAL, EXTERNAL :: SaveCSSR, SaveCCL

! Just in case the user decides to change this in the options menu just while we are in this routine:
! make local copies of the variables that determine which files to save.
      tSavePDB = SavePDB
      tSaveCSSR = SaveCSSR()
      tSaveCCL = SaveCCL()
!
!       Output a CSSR file to fort.64
!       Output a PDB  file to fort.65
!       Output a CCL  file to fort.66
!
!       Write the file headers first
!
! The CSSR file first
      IF (tSaveCSSR) THEN
        OPEN (UNIT=64,FILE=cssr_file(1:cssr_flen),STATUS='unknown')
        WRITE (64,1000) (CellPar(ii),ii=1,3)
 1000   FORMAT (' REFERENCE STRUCTURE = 00000   A,B,C =',3F8.3)
        WRITE (64,1010) (CellPar(ii),ii=4,6), SGNumStr(NumberSGTable)(1:3)
 1010   FORMAT ('   ALPHA,BETA,GAMMA =',3F8.3,'    SPGR = ',A3)
        WRITE (64,"(' ',I3,'   0  DASH solution')") natom
        IF (T .GT. 999.9) THEN
          WRITE (64,1030) -SNGL(fopt), cpb, ntotmov
 1030     FORMAT (' T=******, chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        ELSE
          WRITE (64,1031) SNGL(T), -SNGL(fopt), cpb, ntotmov
 1031     FORMAT (' T=',F6.2,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        ENDIF
      ENDIF
! Now the PDB...
      IF (tSavePDB) THEN
        OPEN (UNIT=65,FILE=pdb_file(1:pdb_flen),STATUS='unknown')
! JCC included again
        CALL sagminv(f2cpdb,inv,3)
! Add in a Header record
        WRITE (65,1036)
 1036   FORMAT ('HEADER PDB Solution File generated by DASH')
        IF (T .GT. 999.9) THEN
          WRITE (65,1040) -SNGL(fopt), cpb, ntotmov
 1040     FORMAT ('REMARK T=******, chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        ELSE
          WRITE (65,1041) SNGL(t), -SNGL(fopt), cpb, ntotmov
 1041     FORMAT ('REMARK T=',F6.2,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        ENDIF
        WRITE (65,1050) (CellPar(ii),ii=1,6), SGHMaStr(NumberSGTable)
 1050   FORMAT ('CRYST1',3F9.3,3F7.2,X,A12)
! JCC Add in V2 pdb records to store space group and symmetry
        WRITE (65,1380)
        WRITE (65,1381)
 1381   FORMAT ('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')
        WRITE (65,1382) SGHMaStr(NumberSGTable)
 1382   FORMAT ('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
        WRITE (65,1380)
        WRITE (65,1383)
 1383   FORMAT ('REMARK 290      SYMOP   SYMMETRY')
        WRITE (65,1384)
 1384   FORMAT ('REMARK 290     NNNMMM   OPERATOR')
        DO i = 1, npdbops
          WRITE (65,1385) (i*1000+555), cpdbops(i)
 1385     FORMAT ('REMARK 290',5X,I6,3X,A)
        ENDDO
        WRITE (65,1380)
        WRITE (65,1386)
 1386   FORMAT ('REMARK 290     WHERE NNN -> OPERATOR NUMBER')
        WRITE (65,1387)
 1387   FORMAT ('REMARK 290           MMM -> TRANSLATION VECTOR')
        WRITE (65,1380)
        WRITE (65,1388)
 1388   FORMAT ('REMARK 290 REMARK:')
! JCC included again
        WRITE (65,1060) inv(1,1), inv(1,2), inv(1,3)
 1060   FORMAT ('SCALE1    ',3F10.5,'      0.00000')
        WRITE (65,1070) inv(2,1), inv(2,2), inv(2,3)
 1070   FORMAT ('SCALE2    ',3F10.5,'      0.00000')
        WRITE (65,1080) inv(3,1), inv(3,2), inv(3,3)
 1080   FORMAT ('SCALE3    ',3F10.5,'      0.00000')
      ENDIF
!       And the CCL
      IF (tSaveCCL) THEN
        OPEN (UNIT=66,FILE=ccl_file(1:ccl_flen),STATUS='unknown')
        IF (T .GT. 999.9) THEN
          WRITE (66,1090) -SNGL(fopt), cpb, ntotmov
 1090     FORMAT ('Z ','T=******, chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        ELSE
          WRITE (66,1091) SNGL(t), -SNGL(fopt), cpb, ntotmov
 1091     FORMAT ('Z ','T=',F6.2,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
        ENDIF
        WRITE (66,1100) (CellPar(ii),ii=1,6)
 1100   FORMAT ('C ',6F10.5)
      ENDIF
      iiact = 0
      itotal = 0
      ipcount = 0
      DO ifrg = 1, maxfrg
        IF (gotzmfile(ifrg)) THEN
          itotal = iiact
! Write out the translation/rotation information for each residue
          IF (tSavePDB) THEN
            WRITE (65,1039) ifrg
 1039       FORMAT ('REMARK Start of molecule number ',I6)
            WRITE (65,1037) (SNGL(parvals(ij)),ij=ipcount+1,ipcount+3)
 1037       FORMAT ('REMARK Translations: ',3F10.6)
          ENDIF
          IF (natoms(ifrg).GT.1) THEN
! Normalise the Q-rotations before writing them out ...
            qvals(1) = SNGL(parvals(ipcount+4))
            qvals(2) = SNGL(parvals(ipcount+5))
            qvals(3) = SNGL(parvals(ipcount+6))
            qvals(4) = SNGL(parvals(ipcount+7))
            qnrm = SQRT(qvals(1)**2 + qvals(2)**2 + qvals(3)**2 + qvals(4)**2)
            qvals = qvals / qnrm
            IF (tSavePDB) THEN
              WRITE (65,1038) (qvals(ij),ij=1,4)
 1038         FORMAT ('REMARK Q-Rotations : ',4F10.6)
            ENDIF
            ipcount = ipcount + izmpar(ifrg)
          ENDIF
          DO i = 1, natoms(ifrg)
! Was     ii = ii + 1
            iiact = iiact + 1
            ii = itotal + izmbid(i,ifrg)
            iorig = izmbid(i,ifrg)
! The CSSR atom lines
            IF (tSaveCSSR) THEN
              WRITE (64,1110) iiact, OriginalLabel(iorig,ifrg)(1:4),(xatopt(k,ii),k=1,3), 0, 0, 0, 0, 0, 0, 0, 0, 0.0
 1110         FORMAT (I4,1X,A4,2X,3(F9.5,1X),8I4,1X,F7.3)
            ENDIF
!       The PDB atom lines
!
! JCC Changed to use the PDB's orthogonalisation  definition
! JCC Shouldnt make any difference the next change - I've made sure that the conversion
! Uses single precision, but I think this is implicit anyway
!          xc=  xatopt(1,ii)*SNGL(f2cmat(1,1))
!
!          yc= (xatopt(2,ii)*SNGL(f2cmat(2,2)))
!     &      + (xatopt(1,ii)*SNGL(f2cmat(1,2)))
!
!          zc= (xatopt(3,ii)*SNGL(f2cmat(3,3)))
!     &      + (xatopt(1,ii)*SNGL(f2cmat(1,3)))
!     &      + (xatopt(2,ii)*SNGL(f2cmat(2,3)))
!
!     Now rotate cartesians about y
!     rnew=(-1.0*(be(nfrag)-90.))*.0174533
!   rnew=(-1.0*(cellpar(5)-90.))*.0174533
!   xc=xc*cos(rnew) + zc*sin(rnew)
!   zc=zc*cos(rnew) - xc*sin(rnew)
!
            xc = xatopt(1,ii)*SNGL(f2cpdb(1,1)) + xatopt(2,ii)*SNGL(f2cpdb(1,2)) + xatopt(3,ii)*SNGL(f2cpdb(1,3))
            yc = xatopt(2,ii)*SNGL(f2cpdb(2,2)) + xatopt(3,ii)*SNGL(f2cpdb(2,3))
            zc = xatopt(3,ii)*SNGL(f2cpdb(3,3))
! Note that elements are right-justified
! WebLab viewer even wants the elements in the atom names to be right justified.
            IF (tSavePDB) THEN
              IF (asym(iorig,ifrg)(2:2).EQ.' ') THEN
                WRITE (65,1120) iiact, OriginalLabel(iorig,ifrg)(1:3), xc, yc, zc, &
                                occ(iorig,ifrg), tiso(iorig,ifrg), asym(iorig,ifrg)(1:1)
 1120           FORMAT ('HETATM',I5,'  ',A3,' NON     1    ',3F8.3,2F6.2,'           ',A1,'  ')
              ELSE
                WRITE (65,1130) iiact, OriginalLabel(iorig,ifrg)(1:4), xc, yc, zc, &
                                occ(iorig,ifrg), tiso(iorig,ifrg), asym(iorig,ifrg)(1:2)
 1130           FORMAT ('HETATM',I5,' ',A4,' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
              ENDIF
              pdbAtmCoords(1,iorig,ifrg,RunNr) = xc
              pdbAtmCoords(2,iorig,ifrg,RunNr) = yc
              pdbAtmCoords(3,iorig,ifrg,RunNr) = zc
            ENDIF
!         The CCL atom lines
            IF (tSaveCCL) THEN
              WRITE (66,1033) asym(iorig,ifrg), (xatopt(k,ii),k=1,3), tiso(iorig,ifrg), occ(iorig,ifrg) 
 1033         FORMAT ('A ',A3,' ',F10.5,1X,F10.5,1X,F10.5,1X,F4.2,1X,F4.2)
            ENDIF
          ENDDO ! loop over atoms
        ENDIF
      ENDDO ! loop over z-matrices
      IF (tSaveCSSR) CLOSE (64)
      IF (tSavePDB) THEN
        WRITE (65,"('END')")
        CLOSE (65)
      ENDIF
      IF (tSaveCCL) CLOSE (66)
      CALL UpdateViewer()
      RETURN
 1380 FORMAT ('REMARK 290 ')

      END SUBROUTINE SA_STRUCTURE_OUTPUT
!
!*****************************************************************************
!
      SUBROUTINE SA_STRUCTURE_OUTPUT_OVERLAP

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SAMVAR
!
!       Called at the end of the SA
!
      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      CHARACTER*3     asym
      CHARACTER*5                          OriginalLabel
      COMMON /zmcomc/ asym(maxatm,maxfrg), OriginalLabel(maxatm,maxfrg)

      INTEGER         ntatm, natoms
      INTEGER         ioptb,                iopta,                ioptt
      INTEGER         iz1,                  iz2,                  iz3
      COMMON /zmcomi/ ntatm, natoms(maxfrg),                                             &
     &                ioptb(maxatm,maxfrg), iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),  &
     &                iz1(maxatm,maxfrg),   iz2(maxatm,maxfrg),   iz3(maxatm,maxfrg)

      INTEGER         izmpar
      CHARACTER*36                    czmpar
      INTEGER                                                kzmpar
      REAL                                                                          xzmpar
      COMMON /zmnpar/ izmpar(maxfrg), czmpar(MaxDOF,maxfrg), kzmpar(MaxDOF,maxfrg), xzmpar(MaxDOF,maxfrg)

      INTEGER         nfrag
      COMMON /frgcom/ nfrag

      DOUBLE PRECISION blen,                alph,                bet,                f2cmat
      COMMON /zmcomr/  blen(maxatm,maxfrg), alph(maxatm,maxfrg), bet(maxatm,maxfrg), f2cmat(3,3)

      REAL            tiso,                occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)

      DOUBLE PRECISION inv(3,3)

      COMMON /posopt/ XATOPT(3,150)

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
      COMMON /PDBOVERLAP/ pdbAtmCoords(1:3,1:maxatm,1:maxfrg,1:30)

      LOGICAL         gotzmfile
      COMMON /zmlgot/ gotzmfile(maxfrg)

      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

! The original atom ids to list in the labels and the back mapping
      COMMON /zmjcmp/ izmoid(maxatm,maxfrg), izmbid(maxatm,maxfrg)

      LOGICAL RESTART
      INTEGER SA_Run_Number, I
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult
! SA_Run_Number now holds the number of the last completed multirun

! Use standard PDB orthogonalisation
      DOUBLE PRECISION f2cpdb
      COMMON /pdbcat/ f2cpdb(3,3)
      INTEGER RunNr, TickedRunNr
      REAL*8 CART(MAXATM,3)
      CHARACTER*2  AtmElement(1:MAXATM_2)
      INTEGER  pdbBond(1:MAXBND*maxfrg,1:2)
      INTEGER TotNumBonds, NumOfAtomsSoFar
      CHARACTER*4 LabelStr
      CHARACTER*2 ColourStr
      CHARACTER*2 RunStr
      LOGICAL, EXTERNAL :: ChrIsLetter
      INTEGER AtomLabelOption, AtomColourOption

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
! Write the file headers first
      OPEN (UNIT=65,FILE='Test_Overlap.pdb',STATUS='unknown',ERR=999)
! JCC included again
      CALL sagminv(f2cpdb,inv,3)
! Add in a Header record
      WRITE (65,1036)
 1036 FORMAT ('HEADER PDB Solution File generated by DASH')
      WRITE (65,1050) (CellPar(ii),ii=1,6), SGHMaStr(NumberSGTable)
 1050 FORMAT ('CRYST1',3F9.3,3F7.2,X,A12)
! JCC Add in V2 pdb records to store space group and symmetry
      WRITE (65,1380)
      WRITE (65,1381)
 1381 FORMAT ('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')
      WRITE (65,1382) SGHMaStr(NumberSGTable)
 1382 FORMAT ('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
      WRITE (65,1380)
      WRITE (65,1383)
 1383 FORMAT ('REMARK 290      SYMOP   SYMMETRY')
      WRITE (65,1384)
 1384 FORMAT ('REMARK 290     NNNMMM   OPERATOR')
      DO i = 1, npdbops
        WRITE (65,1385) (i*1000+555), cpdbops(i)
 1385   FORMAT ('REMARK 290',5X,I6,3X,A)
      ENDDO
      WRITE (65,1380)
      WRITE (65,1386)
 1386 FORMAT ('REMARK 290     WHERE NNN -> OPERATOR NUMBER')
      WRITE (65,1387)
 1387 FORMAT ('REMARK 290           MMM -> TRANSLATION VECTOR')
      WRITE (65,1380)
      WRITE (65,1388)
 1388 FORMAT ('REMARK 290 REMARK:')
! JCC included again
      WRITE (65,1060) inv(1,1), inv(1,2), inv(1,3)
 1060 FORMAT ('SCALE1    ',3F10.5,'      0.00000')
      WRITE (65,1070) inv(2,1), inv(2,2), inv(2,3)
 1070 FORMAT ('SCALE2    ',3F10.5,'      0.00000')
      WRITE (65,1080) inv(3,1), inv(3,2), inv(3,3)
 1080 FORMAT ('SCALE3    ',3F10.5,'      0.00000')
! Per z-matrix, determine the connectivity. This has to be done only once.
      TotNumBonds = 0
      NumOfAtomsSoFar = 0
      DO ifrg = 1, maxfrg
        IF (gotzmfile(ifrg)) THEN
          natcry = NATOMS(ifrg)
          CALL MAKEXYZ(natcry,BLEN(1,ifrg),ALPH(1,ifrg),BET(1,ifrg),      &
         &             IZ1(1,ifrg),IZ2(1,ifrg),IZ3(1,ifrg),CART(1,1),     &
         &             CART(1,2),CART(1,3))
! Conversion of asym to aelem : very dirty, but works
          DO I = 1, natcry
            axyzo(I,1) = SNGL(CART(I,1))
            axyzo(I,2) = SNGL(CART(I,2))
            axyzo(I,3) = SNGL(CART(I,3))
            AtmElement(I)(1:2) = asym(I,ifrg)(1:2)
          ENDDO
          CALL AssignCSDElement(AtmElement)
! Calculate bonds
          CALL MakeBonds
! OUTPUT : nbocry             = number of bonds
!          bond(1:MAXBND,1:2) = the atoms connected by the bond
          DO J = 1, nbocry
            pdbBond(J+TotNumBonds,1) = bond(J,1) + NumOfAtomsSoFar
            pdbBond(J+TotNumBonds,2) = bond(J,2) + NumOfAtomsSoFar
          ENDDO
          NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(ifrg)
          TotNumBonds = TotNumBonds + nbocry
        ENDIF
      ENDDO ! loop over z-matrices
! Get atom label option from dialogue. Two options: 
! 1. "Element symbol + solution number" (default)
! 2. "Orignal atom labels"
      CALL WDialogGetRadioButton(IDF_UseSolutionNr,AtomLabelOption)
! Get atom colour option from dialogue. Two options: 
! 1. "By solution number" (default)
! 2. "By element"
      CALL WDialogGetRadioButton(IDF_ColourBySolution,AtomColourOption)
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
      DO RunNr = 1, SA_Run_Number
        CALL WGridGetCellCheckBox(IDF_SA_summary,3,RunNr,istatus)
        IF (istatus .EQ. 1) THEN
          TickedRunNr = TickedRunNr + 1
! Note that elements are right-justified
          IF (AtomColourOption .EQ. 1) THEN ! Colour by solution
            SELECT CASE(TickedRunNr)
              CASE ( 1)
                ColourStr = 'Co' ! Cobalt        Blue
              CASE ( 2)
                ColourStr = ' O '  ! Oxygen        Red
              CASE ( 3)
                ColourStr = ' S '  ! Sulphur       Yellow
              CASE ( 4)
                ColourStr = 'Cl' ! Chlorine      Green
              CASE ( 5)
                ColourStr = ' N '  ! Nitrogen      Light blue
              CASE ( 6)
                ColourStr = 'Br' ! Bromine       Brown
              CASE ( 7)
                ColourStr = ' I '  ! Iodine        Pink
              CASE ( 8)
                ColourStr = ' C '  ! Carbon        Grey
              CASE ( 9)
                ColourStr = ' H '  ! Hydrogen      White
              CASE (10)
                ColourStr = ' P '  ! Phosphorus
            END SELECT
          ENDIF
          WRITE(RunStr,'(I2)') RunNr
          CALL StrClean(RunStr,ilen) ! Left justify
          LabelStr = LabelStr(1:LEN_TRIM(LabelStr))//RunStr
          DO ifrg = 1, maxfrg
            IF (gotzmfile(ifrg)) THEN
              DO i = 1, natoms(ifrg)
                iiact = iiact + 1
                xc = pdbAtmCoords(1,i,ifrg,RunNr)        ! We could replace (ii) by (orig,ifrg) and get rid of ii and itotal
                yc = pdbAtmCoords(2,i,ifrg,RunNr)
                zc = pdbAtmCoords(3,i,ifrg,RunNr)
! Note that elements are right-justified
                IF (AtomColourOption .EQ. 2) THEN ! Colour by Element
                  IF (asym(i,ifrg)(2:2) .EQ. ' ') THEN
                    ColourStr(1:2) = ' '//asym(i,ifrg)(1:1)
                  ELSE
                    ColourStr(1:2) = asym(i,ifrg)(1:2)
                  ENDIF
                ENDIF
                IF (AtomLabelOption .EQ. 1) THEN ! Element symbol + solution number
                  LabelStr = asym(i,ifrg)(1:LEN_TRIM(asym(i,ifrg)))//RunStr
                ELSE  ! Orignal atom labels
                  LabelStr(1:4) = OriginalLabel(i,ifrg)(1:4)
                ENDIF
                WRITE (65,1120) iiact, LabelStr(1:4), xc, yc, zc, occ(i,ifrg), tiso(i,ifrg), ColourStr(1:2)
 1120           FORMAT ('HETATM',I5,' ',A4' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
              ENDDO ! loop over atoms
            ENDIF
          ENDDO ! loop over z-matrices
        ENDIF ! Was this solution ticked to be displayed?
      ENDDO ! loop over runs
      TickedRunNr = 0
      DO RunNr = 1, SA_Run_Number
        CALL WGridGetCellCheckBox(IDF_SA_summary,3,RunNr,istatus)
        IF (istatus .EQ. 1) THEN
          TickedRunNr = TickedRunNr + 1
          DO BondNr = 1, TotNumBonds
            WRITE(65,'(A6,I5,I5)') 'CONECT', (pdbBond(BondNr,1)+NATOM*(TickedRunNr-1)), (pdbBond(BondNr,2)+NATOM*(TickedRunNr-1))
          ENDDO
        ENDIF
      ENDDO ! loop over runs
      WRITE (65,"('END')")
      CLOSE (65)
      CALL ViewStructure('Test_Overlap.pdb')
      CALL PopActiveWindowID
      RETURN
 1380 FORMAT ('REMARK 290 ')
  999 CALL ErrorMessage('Could not open temporary file.')
      CALL PopActiveWindowID

      END SUBROUTINE SA_STRUCTURE_OUTPUT_OVERLAP
!*==SAGMINV.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
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
!*==SAGMEQ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
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
!*==PDB_SYMMRECORDS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE PDB_SymmRecords()

      PARAMETER (msymmin=10)
      CHARACTER*20 symline
      COMMON /symgencmn/ nsymmin, symmin(4,4,msymmin), symline(msymmin)
      CHARACTER*50 stout

      PARAMETER (mpdbops=192)
      CHARACTER*20 cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

      REAL rpdb(4,4,mpdbops), rtmp(4,4)
      LOGICAL cmp
      LOGICAL PDB_CmpMat
!
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
!*==PDB_MATMUL.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
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
!*==PDB_CMPMAT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
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
!
      END FUNCTION PDB_CMPMAT
!*==PDB_POSTRANS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
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
      RETURN

      END SUBROUTINE PDB_POSTRANS
!*==ADDSINGLESOLUTION.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE AddSingleSolution(ProfileChi,IntensityChi)

      REAL ProfileChi, IntensityChi

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file
      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      LOGICAL RESTART
      INTEGER SA_Run_Number
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult

      SA_Run_Number = 1
      CALL Log_SARun_Entry(pdb_file,ProfileChi,IntensityChi)

      END SUBROUTINE ADDSINGLESOLUTION
!*==ADDMULTISOLUTION.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE AddMultiSolution(ProfileChi,IntensityChi)

      REAL ProfileChi, IntensityChi

      LOGICAL RESTART
      INTEGER SA_Run_Number
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult
      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file
      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      CHARACTER*85 new_fname

      SA_Run_Number = SA_Run_Number + 1
      CALL AppendNumToFileName(SA_Run_Number,cssr_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile(cssr_file(1:LEN_TRIM(cssr_file)),new_fname)
      CALL AppendNumToFileName(SA_Run_Number,ccl_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile(ccl_file(1:LEN_TRIM(ccl_file)),new_fname)
! ep appended
      CALL AppendNumToFileName(SA_Run_Number,pro_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile(pro_file(1:LEN_TRIM(pro_file)),new_fname)
      CALL AppendNumToFileName(SA_Run_Number,pdb_file,new_fname)
      CALL IOsDeleteFile(new_fname)
      CALL IOsRenameFile(pdb_file(1:LEN_TRIM(pdb_file)),new_fname)
      CALL Log_SARun_Entry(new_fname,ProfileChi,IntensityChi)

      END SUBROUTINE ADDMULTISOLUTION
!*==APPENDNUMTOFILENAME.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE AppendNumToFileName(Num,infilename,outfilename)

      CHARACTER*(*) infilename, outfilename
      INTEGER iinlen, icount
      CHARACTER*3 NumStr

      iinlen = LEN_TRIM(infilename)
      ipos = 0
      iout = 1
      WRITE (NumStr,'(I3.3)') Num
      DO I = 1, LEN(outfilename)
        outfilename(I:I) = ' '
      ENDDO
      icount = iinlen
      DO WHILE (icount.GT.0)
! Find the last dot in the filename
        IF (infilename(icount:icount).EQ.'.') THEN
          ipos = icount
          GOTO 100
        ENDIF
        icount = icount - 1
      ENDDO
  100 icount = 1
      DO WHILE (icount.LT.ipos)
        outfilename(icount:icount) = infilename(icount:icount)
        icount = icount + 1
      ENDDO
      iout = icount
      outfilename(iout:iout+3) = '_'//NumStr
      iout = iout + 4
      DO WHILE (icount.LE.iinlen)
        outfilename(iout:iout) = infilename(icount:icount)
        icount = icount + 1
        iout = iout + 1
      ENDDO

      END SUBROUTINE APPENDNUMTOFILENAME
!*==UPDATEVIEWER.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!*****************************************************************************
!
      SUBROUTINE UpdateViewer()

      USE VARIABLES

      IF (AutoUpdate .AND. ViewAct) CALL ViewBest

      END SUBROUTINE UPDATEVIEWER
!
!*****************************************************************************
!
