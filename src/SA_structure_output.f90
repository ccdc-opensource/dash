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
!*****************************************************************************
!
      SUBROUTINE SA_structure_output(T, parvals, ntotmov)

      USE dash_gui_resources
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE

      REAL T
      REAL parvals(*) ! The current torsion parameters
      INTEGER ntotmov

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      INTEGER, EXTERNAL :: WritePDBCommon, WriteCIFCommon
      REAL, EXTERNAL :: UnitCellVolume
      CHARACTER*20, EXTERNAL :: GetSeed1SuffixString
      CHARACTER*1, EXTERNAL :: ChrLowerCase
      LOGICAL, EXTERNAL :: SavePDB, SaveCSSR, SaveCCL, SaveCIF, SaveRES
      REAL qvals(4), qnrm
      LOGICAL tSavePDB, tSaveCSSR, tSaveCCL, tSaveCIF, tSaveRES
      INTEGER ipcount, iScat, tElement, k1
      INTEGER hFileCSSR, hFilePDB, hFileCCL, hFileCIF, hFileRES
      INTEGER I, J, II, K, iiact, iTotal, iFrg, IJ, iOrig
      REAL    x_pdb(1:3), tPO
      INTEGER NumOfAtomsSoFar, iBond1, iBond2, iTem, tLen
      INTEGER tLen1, tLen2
      CHARACTER(MaxPathLength) tFileName
      CHARACTER(8) TemperatureStr
      CHARACTER*80 tString, tString1, tString2
      CHARACTER*2  LATT
      INTEGER NumOfAtmPerElm(1:MaxElm)
      CHARACTER*80 DASHRemarkStr
      REAL TOUISO

      IF (in_batch) THEN
        ! log SA paramters in easily parsing format (delims: ,=)
        WRITE (DASHRemarkStr,105,ERR=999) T, FOPT, CHIPROBEST, ntotmov
        CALL InfoMessage(TRIM(DASHRemarkStr))
  105   FORMAT ('T=',F8.2,', chi**2=',F7.2,', profile chi**2=',F7.2,', total moves=',I8)
        tFileName = OutputFilesBaseName(1:OFBN_Len)//TRIM(GetSeed1SuffixString())//SA_RunNumberStr
      ELSE
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr
      ENDIF
      IF (T .GT. 999.9) THEN
        TemperatureStr = 'T=******'
      ELSE
        WRITE (TemperatureStr,"('T=',F6.2)") T
      ENDIF
      WRITE (DASHRemarkStr,100,ERR=999) TemperatureStr, FOPT, CHIPROBEST, ntotmov
  100 FORMAT (A8,', chi**2=',F7.2,' and profile chi**2=',F7.2,' after ',I8,' moves')
! Just in case the user decides to change this in the options menu just while we are in this routine:
! make local copies of the variables that determine which files to save.
      tSavePDB  = SavePDB()
      tSaveCSSR = SaveCSSR()
      tSaveCCL  = SaveCCL()
      tSaveCIF  = SaveCIF()
      tSaveRES  = SaveRES()
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
      IF ( tSaveCSSR ) THEN
        hFileCSSR = 64
        OPEN (UNIT=hFileCSSR,FILE=TRIM(tFileName)//'.cssr',STATUS='unknown',ERR=999)
        WRITE (hFileCSSR,1000,ERR=999) (CellPar(ii),ii=1,3)
 1000   FORMAT (' REFERENCE STRUCTURE = 00000   A,B,C =',3F8.3)
        WRITE (hFileCSSR,1010,ERR=999) (CellPar(ii),ii=4,6), SGNumStr(NumberSGTable)(1:3)
 1010   FORMAT ('   ALPHA,BETA,GAMMA =',3F8.3,'    SPGR = ',A3)
        IF (PrefParExists) THEN
          WRITE (hFileCSSR,"(' ',I3,'   0 PO',3(1X,I3),1X,F9.5)",ERR=999) natom,(PO_Direction(ii),ii=1,3),BestValuesDoF(iPrfPar,Curr_SA_Run)
        ELSE
          WRITE (hFileCSSR,"(' ',I3,'   0 DASH solution')",ERR=999) natom
        ENDIF
        WRITE (hFileCSSR,'(A)',ERR=999) '     1 '//DASHRemarkStr(11:51) 
      ENDIF
! PDB ...
      IF ( tSavePDB ) THEN
        hFilePDB = 65
        OPEN (UNIT=hFilePDB,FILE=TRIM(tFileName)//'.pdb',STATUS='unknown',ERR=999)
! Add in a Header record
        WRITE (hFilePDB,"('HEADER    PDB Solution File generated by DASH')",ERR=999)
        WRITE (hFilePDB,'(A)',ERR=999) 'REMARK '//DASHRemarkStr 
        IF ( PrefParExists ) THEN
          WRITE (hFilePDB,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'REMARK PO',(PO_Direction(ii),ii=1,3),BestValuesDoF(iPrfPar,Curr_SA_Run)
        ENDIF
        IF ( WritePDBCommon(hFilePDB) .NE. 0 ) GOTO 999
      ENDIF
! CCL ...
      IF (tSaveCCL) THEN
        hFileCCL = 66
        OPEN (UNIT=hFileCCL,FILE=TRIM(tFileName)//'.ccl',STATUS='unknown',ERR=999)
        WRITE (hFileCCL,'(A)',ERR=999) 'Z '//DASHRemarkStr 
        IF (PrefParExists) THEN
          WRITE (hFileCCL,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'Z PO',(PO_Direction(ii),ii=1,3),BestValuesDoF(iPrfPar,Curr_SA_Run)
        ENDIF
        WRITE (hFileCCL,1100,ERR=999) (CellPar(ii),ii=1,6)
 1100   FORMAT ('C ',6F10.5)
      ENDIF
! CIF ...
      IF ( tSaveCIF ) THEN
        hFileCIF = 67
        OPEN (UNIT=hFileCIF,FILE=TRIM(tFileName)//'.cif',STATUS='unknown',ERR=999)
        WRITE (hFileCIF,'("data_global")',ERR=999)
        WRITE (hFileCIF,'(A)',ERR=999) '# '//DASHRemarkStr
        tPO = 0.0
        IF (PrefParExists) tPO = BestValuesDoF(iPrfPar,Curr_SA_Run)
        IF (WriteCIFCommon(hFileCIF, FOPT, tPO, .FALSE.) .NE. 0) GOTO 999
       ENDIF
! RES ...
      IF ( tSaveRES ) THEN
        hFileRES = 68
        OPEN (UNIT=hFileRES,FILE=TRIM(tFileName)//'.res',STATUS='unknown',ERR=999)
        WRITE (hFileRES,"('TITL Solution File generated by DASH')",ERR=999)
        WRITE (hFileRES,'(A)',ERR=999) 'REM  '//DASHRemarkStr
        IF (PrefParExists) THEN
          WRITE (hFileRES,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'REM  PO',(PO_Direction(ii),ii=1,3),BestValuesDoF(iPrfPar,Curr_SA_Run)
        ENDIF
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
        DO iFrg = 1, nFrag
          DO i = 1, natoms(iFrg)
            CALL INC(NumOfAtmPerElm(zmElementCSD(i,iFrg)))
          ENDDO
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
      DO iFrg = 1, nFrag
        itotal = iiact
! Write out the translation/rotation information for each residue
        IF ( tSavePDB ) THEN
          WRITE (hFilePDB,1039,ERR=999) iFrg
 1039     FORMAT ('REMARK Start of molecule number ',I6)
          WRITE (hFilePDB,1037,ERR=999) (parvals(ij),ij=ipcount+1,ipcount+3)
 1037     FORMAT ('REMARK Translations: ',3F10.6)
        ENDIF
        IF (natoms(iFrg) .GT. 1) THEN
! Normalise the Q-rotations before writing them out ...
          qvals(1) = parvals(ipcount+4)
          qvals(2) = parvals(ipcount+5)
          qvals(3) = parvals(ipcount+6)
          qvals(4) = parvals(ipcount+7)
          qnrm = SQRT(qvals(1)**2 + qvals(2)**2 + qvals(3)**2 + qvals(4)**2)
          qvals = qvals / qnrm
          IF ( tSavePDB ) THEN
            WRITE (hFilePDB,1038,ERR=999) (qvals(ij),ij=1,4)
 1038       FORMAT ('REMARK Q-Rotations : ',4F10.6)
          ENDIF
          ipcount = ipcount + izmpar(iFrg)
        ENDIF
        DO i = 1, natoms(iFrg)
          iiact = iiact + 1
          iOrig = izmbid(i,iFrg)
          ii = OrderedAtm(itotal + iOrig)
! The CSSR atom lines
          IF ( tSaveCSSR ) THEN
            WRITE (hFileCSSR,1110,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:4),(XAtmCoords(k,ii,Curr_SA_Run),k=1,3), 0, 0, 0, 0, 0, 0, 0, 0, 0.0
 1110       FORMAT (I4,1X,A4,2X,3(F9.5,1X),8I4,1X,F7.3)
          ENDIF
          IF ( tSavePDB ) THEN
! The PDB atom lines
          CALL PremultiplyVectorByMatrix(f2cpdb, XAtmCoords(1,ii,Curr_SA_Run), x_pdb)
! Note that elements are right-justified
! WebLab viewer even wants the elements in the atom names to be right justified.
            IF (ElSym(iOrig,iFrg)(2:2).EQ.' ') THEN
              WRITE (hFilePDB,1120,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:3), x_pdb(1), x_pdb(2), x_pdb(3), &
                              occ(iOrig,iFrg), tiso(iOrig,iFrg), ElSym(iOrig,iFrg)(1:1)
 1120         FORMAT ('HETATM',I5,'  ',A3,' NON     1    ',3F8.3,2F6.2,'           ',A1,'  ')
            ELSE
              WRITE (hFilePDB,1130,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:4), x_pdb(1), x_pdb(2), x_pdb(3), &
                              occ(iOrig,iFrg), tiso(iOrig,iFrg), ElSym(iOrig,iFrg)(1:2)
 1130         FORMAT ('HETATM',I5,' ',A4,' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
            ENDIF
          ENDIF
! The CCL atom lines
          IF (tSaveCCL) THEN
            WRITE (hFileCCL,1033,ERR=999) ElSym(iOrig,iFrg), (XAtmCoords(k,ii,Curr_SA_Run),k=1,3), tiso(iOrig,iFrg), occ(iOrig,iFrg) 
 1033       FORMAT ('A ',A2,'  ',F10.5,1X,F10.5,1X,F10.5,1X,F4.2,1X,F4.2)
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
!C        _atom_site_U_iso_or_equiv
!C      C1     -0.10853   0.45223   0.14604  1.0 Uiso 3.0
!C      C2     -0.05898   0.41596   0.27356  1.0 Uiso 3.0
          IF (tSaveCIF) THEN
!C          This is 1/( 8 * pi^2)          
            TOUISO = 0.01266514796 
            WRITE (hFileCIF,1034,ERR=999) OriginalLabel(iOrig,iFrg), (XAtmCoords(k,ii,Curr_SA_Run),k=1,3), occ(iOrig,iFrg), TOUISO * tiso(iOrig,iFrg) 
 1034       FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Uiso ',F6.4)
          ENDIF
          IF (tSaveRES) THEN
! Determine this atom's entry number in the scattering factor list
            tElement = zmElementCSD(iOrig,iFrg)
            iScat = 0
            DO k1 = 1, tElement
              IF (NumOfAtmPerElm(k1) .NE. 0) iScat = iScat + 1
            ENDDO
            WRITE (hFileRES,1035,ERR=999) OriginalLabel(iOrig,iFrg), iScat, (XAtmCoords(k,ii,Curr_SA_Run),k=1,3), &
                                  occ(iOrig,iFrg), tiso(iOrig,iFrg)/(8.0*(PI**2)) 
 1035       FORMAT (A5,1X,I2,1X,3(F10.5,1X),F5.3,1X,F5.3)
          ENDIF
        ENDDO ! loop over atoms
      ENDDO ! loop over Z-matrices
      IF (tSaveCSSR) CLOSE (hFileCSSR)
      IF (tSavePDB) THEN
! Per Z-matrix, write out the connectivity.
        NumOfAtomsSoFar = 0
        DO iFrg = 1, nFrag
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
              WRITE(hFilePDB,'(A6,I5,I5)',ERR=999) 'CONECT', iBond1, iBond2
            ENDDO
          ENDIF
          NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
        ENDDO ! loop over Z-matrices
        WRITE (hFilePDB,"('END')",ERR=999)
        CLOSE (hFilePDB)
      ENDIF
      IF (tSaveCCL) CLOSE (hFileCCL)
      IF (tSaveCIF) CLOSE (hFileCIF)
      IF (tSaveRES) THEN
        WRITE (hFileRES,"('END ')",ERR=999)
        CLOSE (hFileRES)
      ENDIF
      RETURN
 1380 FORMAT ('REMARK 290 ')
  999 CALL ErrorMessage('Error writing SA output files.')
      CLOSE(hFilePDB)
      CLOSE(hFileCSSR)
      CLOSE(hFileCCL)
      CLOSE(hFileCIF)
      CLOSE(hFileRES)

      END SUBROUTINE SA_STRUCTURE_OUTPUT
!
!*****************************************************************************
!
      SUBROUTINE SA_STRUCTURE_OUTPUT_NON_OVERLAP(TheRunNr, file_name)

! This subroutine implements the option of write CIF format for viewer 
! by wrapping existing subs 

      USE dash_gui_resources

      IMPLICIT NONE

      INTEGER,        INTENT (IN   ) :: TheRunNr
      CHARACTER*(*), INTENT (  OUT) :: file_name

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      CHARACTER(3) RunStr

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      IF ( DASHWDialogGetCheckBoxLogical(IDC_cif_for_viewer) ) THEN
        WRITE(RunStr, '(I3.3)') TheRunNr
        file_name = "DASH_tmp"//RunStr//".cif"
        CALL SA_structure_output_2(0, TheRunNr, file_name, 4)
      ELSE
        CALL SA_STRUCTURE_OUTPUT_PDB(TheRunNr, file_name)
      ENDIF
      CALL PopActiveWindowID

      RETURN

      END SUBROUTINE SA_STRUCTURE_OUTPUT_NON_OVERLAP
!
!*****************************************************************************
!
      SUBROUTINE SA_STRUCTURE_OUTPUT_PDB(TheRunNr, file_name)
!
! This subroutine writes out a single solution to a temporary file for viewing.
! It is a combination of SA_STRUCTURE_OUTPUT() and SA_STRUCTURE_OUTPUT_OVERLAP()
!
! It relies on XAtmCoords being up to date
! Uses TheRunNr to determine which solution to output

      USE WINTERACTER
      USE ZMVAR
      USE ATMVAR
      USE SOLVAR

      IMPLICIT NONE

      INTEGER,        INTENT (IN   ) :: TheRunNr
      CHARACTER*(15), INTENT (  OUT) :: file_name

      INCLUDE 'params.inc'

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      INTEGER, EXTERNAL :: WritePDBCommon
      INTEGER TotNumBonds, NumOfAtomsSoFar
      INTEGER I, iFrg, J, iAtom
      REAL    x_pdb(1:3)
      INTEGER hFilePDB
      CHARACTER(3) RunStr

      hFilePDB = 65
      WRITE(RunStr, '(I3.3)') TheRunNr
! Write the file headers first
      file_name = "DASH_tmp"//RunStr//".pdb"
      OPEN (UNIT=hFilePDB, FILE=file_name, STATUS='unknown', ERR=999)
! Add in a Header record
      WRITE (hFilePDB, "('HEADER    CSD ENTRY RUNNUM',A3)", ERR=999) RunStr
      IF (WritePDBCommon(hFilePDB) .NE. 0) GOTO 999
      iAtom = 0
      DO iFrg = 1, nFrag
        DO i = 1, natoms(iFrg)
          iAtom = iAtom + 1
          CALL PremultiplyVectorByMatrix(f2cpdb, XAtmCoords(1,OrderedAtm(iAtom),TheRunNr), x_pdb)
! Note that elements are right-justified
          IF (ElSym(i,iFrg)(2:2).EQ.' ') THEN
            WRITE (hFilePDB,1120,ERR=999) iAtom, OriginalLabel(i,iFrg)(1:3), x_pdb(1), x_pdb(2), x_pdb(3), &
                                          occ(i,iFrg), tiso(i,iFrg), ElSym(i,iFrg)(1:1)
 1120       FORMAT ('HETATM',I5,'  ',A3,' NON     1    ',3F8.3,2F6.2,'           ',A1,'  ')
          ELSE
            WRITE (hFilePDB,1130,ERR=999) iAtom, OriginalLabel(i,iFrg)(1:4), x_pdb(1), x_pdb(2), x_pdb(3), &
                                          occ(i,iFrg), tiso(i,iFrg), ElSym(i,iFrg)(1:2)
 1130       FORMAT ('HETATM',I5,' ',A4,' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
          ENDIF
        ENDDO ! loop over atoms
      ENDDO ! loop over Z-matrices
! Per Z-matrix, write out the connectivity.
      TotNumBonds = 0
      NumOfAtomsSoFar = 0
      DO iFrg = 1, nFrag
        IF (NumberOfBonds(iFrg) .GT. 0) THEN
          DO J = 1, NumberOfBonds(iFrg)
            WRITE(hFilePDB,'(A6,I5,I5)') 'CONECT', Bonds(1,J,iFrg)+NumOfAtomsSoFar, Bonds(2,J,iFrg)+NumOfAtomsSoFar
          ENDDO
        ENDIF
        NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
        TotNumBonds = TotNumBonds + NumberOfBonds(iFrg)
      ENDDO ! loop over Z-matrices
      WRITE (hFilePDB,"('END')",ERR=999)
      CLOSE (hFilePDB)
      RETURN
 1380 FORMAT ('REMARK 290 ')
  999 CALL ErrorMessage('Error writing temporary file.')
      CLOSE (hFilePDB)

      END SUBROUTINE SA_STRUCTURE_OUTPUT_PDB
!
!*****************************************************************************
!
      SUBROUTINE SA_STRUCTURE_OUTPUT_OVERLAP(DialogueID)

      USE WINTERACTER
      USE dash_gui_resources
      USE ZMVAR
      USE ATMVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: DialogueID

      INCLUDE 'params.inc'

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult
! NumOf_SA_Runs holds the number of completed SA runs

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      LOGICAL, EXTERNAL :: Get_AutoAlign, DASHWDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: WritePDBCommon, WriteCIFCommon
      INTEGER iSol, TickedRunNr, NumOfOverlaidStructures
      INTEGER pdbBond(1:maxbnd_2*maxfrg,1:2)
      INTEGER TotNumBonds, NumOfAtomsSoFar
      CHARACTER*5 LabelStr
      CHARACTER*2 ColourStr
      CHARACTER*3 SolStr
      INTEGER AtomLabelOption, AtomColourOption
      INTEGER I, iFrg, J, iiact, ISTATUS, BondNr, ilen
      REAL    x_pdb(1:3), tPO
      INTEGER iAtom
      INTEGER tNumOf_SA_Runs
      LOGICAL tUseCif
      INTEGER, PARAMETER :: chFile = 65
      REAL, PARAMETER :: TOUISO = 0.01266514796 
      CHARACTER*(*), PARAMETER :: PDBFileName = 'Overlap_Temp.pdb', CIFFileName =  'Overlap_Temp.cif'

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      tUseCif = DASHWDialogGetCheckBoxLogical(IDC_cif_for_viewer)
      CALL SelectDASHDialog(DialogueID)
      tNumOf_SA_Runs = NumOf_SA_Runs
      ! If DialogueID .EQ. IDD_Summary, then we could add 1 to the NumOfSARuns, because
      ! there is currently a run in progress which also stores its intermediate optima
      ! in XAtmCoords.
      IF (DialogueID .EQ. IDD_Summary) tNumOf_SA_Runs = tNumOf_SA_Runs + 1
      IF ( tUseCif ) THEN
        OPEN (UNIT=chFile,FILE=CIFFileName,STATUS='unknown',ERR=999)
        WRITE (chFile,1036,ERR=999) '# CIF '
      ELSE
! Write the file headers first
        OPEN (UNIT=chFile,FILE=PDBFileName,STATUS='unknown',ERR=999)
! Add in a Header record
        WRITE (chFile,1036,ERR=999) 'HEADER    PDB '
        IF (WritePDBCommon(chFile) .NE. 0) GOTO 999
      ENDIF
 1036 FORMAT (A, 'Solution File generated by DASH')
! Get atom label option from dialogue. Two options: 
! 1. "Element + solution #"
! 2. "Original atom labels"
      CALL DASHWDialogGetRadioButton(IDF_UseSolutionNr, AtomLabelOption)
! Get atom colour option from dialogue. Two options: 
! 1. "By solution number"
! 2. "By element"
      CALL DASHWDialogGetRadioButton(IDF_ColourBySolution, AtomColourOption)
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
      DO iSol = 1, tNumOf_SA_Runs
        IF (iSol .EQ. (NumOf_SA_Runs + 1)) THEN
          IF (Get_AutoAlign()) CALL Align
        ENDIF
        WRITE(SolStr,'(I3)',ERR=999) iSol
        CALL StrClean(SolStr,ilen) ! Left justify
        CALL DASHWGridGetCellCheckBox(IDF_SA_summary, 3, iSol, istatus)
        IF (istatus .EQ. 1) THEN
          NumOfOverlaidStructures = NumOfOverlaidStructures + 1
          IF ( tUseCif ) THEN
            WRITE (chFile,'(/"data_solution_",A)',ERR=999) SolStr
            tPO = 0.0
            IF (PrefParExists) tPO = BestValuesDoF(iPrfPar,iSol2Run(iSol))
            IF (WriteCIFCommon(chFile, IntensityChiSqd(iSol2Run(iSol)), tPO, .FALSE.) .NE. 0) GOTO 999
! Note that elements are right-justified
          ELSE IF (AtomColourOption .EQ. 1) THEN ! Colour by solution
            TickedRunNr = TickedRunNr + 1 ! Number of ticked runs, counter used for choosing the colour
            IF (TickedRunNr .EQ. 11) TickedRunNr = 1 ! Re-use colours.
            SELECT CASE (TickedRunNr)
              CASE ( 1)
                ColourStr = ' S'  ! Sulphur       Yellow
              CASE ( 2)
                ColourStr = ' O'  ! Oxygen        Red
              CASE ( 3)
                ColourStr = 'Co'  ! Cobalt        Dark Blue
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
          iAtom = 0
          DO iFrg = 1, nFrag
            DO i = 1, natoms(iFrg)
              iiact = iiact + 1
              iAtom = iAtom + 1
              IF (AtomLabelOption .EQ. 1) THEN ! Element symbol + solution number
                LabelStr = TRIM(ElSym(i,iFrg))//SolStr
              ELSE  ! Orignal atom labels
                LabelStr = OriginalLabel(i,iFrg)
              ENDIF
              IF ( tUseCif ) THEN
                WRITE (chFile,1034,ERR=999) LabelStr, XAtmCoords(1:3,OrderedAtm(iAtom),iSol2Run(iSol)), occ(i,iFrg), TOUISO * tiso(i,iFrg) 
 1034           FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Uiso ',F6.4)
              ELSE
! Note that elements are right-justified
                IF (AtomColourOption .EQ. 2) THEN ! Colour by Element
                  IF (ElSym(i,iFrg)(2:2) .EQ. ' ') THEN
                    ColourStr(1:2) = ' '//ElSym(i,iFrg)(1:1)
                  ELSE
                    ColourStr = ElSym(i,iFrg)
                  ENDIF
                ENDIF
                CALL PremultiplyVectorByMatrix(f2cpdb, XAtmCoords(1,OrderedAtm(iAtom),iSol2Run(iSol)), x_pdb)
                WRITE (chFile,1120,ERR=999) iiact, LabelStr(1:4), x_pdb(1:3), occ(i,iFrg), tiso(i,iFrg), ColourStr(1:2)
 1120           FORMAT ('HETATM',I5,' ',A4' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
              ENDIF
            ENDDO ! loop over atoms
          ENDDO ! loop over Z-matrices
        ENDIF ! Was this solution ticked to be displayed?
      ENDDO ! loop over runs
      IF ( tUseCif ) GOTO 200
! Per Z-matrix, determine the connectivity. This has to be done only once.
      TotNumBonds = 0
      NumOfAtomsSoFar = 0
      DO iFrg = 1, nFrag
        IF (NumberOfBonds(iFrg) .GT. 0) THEN
          DO J = 1, NumberOfBonds(iFrg)
            pdbBond(J+TotNumBonds,1) = Bonds(1,J,iFrg) + NumOfAtomsSoFar
            pdbBond(J+TotNumBonds,2) = Bonds(2,J,iFrg) + NumOfAtomsSoFar
          ENDDO
        ENDIF
        NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
        TotNumBonds = TotNumBonds + NumberOfBonds(iFrg)
      ENDDO ! loop over Z-matrices
      DO iSol = 1, NumOfOverlaidStructures
        DO BondNr = 1, TotNumBonds
          WRITE(chFile,'(A6,I5,I5)',ERR=999) 'CONECT', (pdbBond(BondNr,1)+NATOM*(iSol-1)), (pdbBond(BondNr,2)+NATOM*(iSol-1))
        ENDDO
      ENDDO ! loop over runs
      WRITE (chFile,"('END')",ERR=999)
  200 CLOSE (chFile)
      IF ( tUseCif ) THEN
        CALL ViewStructure(CIFFileName, .FALSE.)
      ELSE
        CALL ViewStructure(PDBFileName, .FALSE.)
      ENDIF
      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing temporary file.')
      CLOSE (chFile)
      CALL PopActiveWindowID

      END SUBROUTINE SA_STRUCTURE_OUTPUT_OVERLAP
!
!*****************************************************************************
!
! Merged from SA_structure_output, SA_structure_output_2 and RR_SaveAs
! Note: if FromRR, GoodnessOfFit line is bypassed
!
! RETURNS : 0 for success
!
     INTEGER FUNCTION WriteCIFCommon(hFileCIF, GoodnessOfFit, POMagnitude, FromRR)

      USE dash_gui_resources
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: hFileCIF
      REAL,          INTENT (IN   ) :: GoodnessOfFit, POMagnitude
      LOGICAL,       INTENT (IN   ) :: FromRR

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      REAL, EXTERNAL         :: UnitCellVolume, WavelengthOf
      CHARACTER*1, EXTERNAL  :: ChrLowerCase
      INTEGER I, tLen !, iRadSelection
      CHARACTER*80              tString

! Initialise to failure
      WriteCIFCommon = 1

      tString = CrystalSystemString(LatBrav)
      tString(1:1) = ChrLowerCase(tString(1:1))
      tLen = LEN_TRIM(tString)
! Remove '-a' etc. from monoclinic
      IF (LatBrav .EQ. 2 .OR. LatBrav .EQ. 3 .OR. LatBrav .EQ. 4) tLen = tLen -2
      WRITE (hFileCIF,'("_symmetry_cell_setting ",A)',ERR=999) tString(1:tLen)
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
      WRITE (hFileCIF,"('_symmetry_space_group_name_H-M ',A)",ERR=999) tString(1:tLen)
      WRITE (hFileCIF,'("loop_")',ERR=999)
      WRITE (hFileCIF,'("  _symmetry_equiv_pos_as_xyz")',ERR=999)
      DO i = 1, npdbops
        tString = cpdbops(i)
        tLen = LEN_TRIM(tString)
        tString = "  '"//tString(1:tLen)//"'"
        tLen = tLen + 4
        WRITE (hFileCIF,"(A)",ERR=999) tString(1:tLen)
      ENDDO
      WRITE (hFileCIF,'("_cell_length_a    ",F8.4)',ERR=999) CellPar(1)
      WRITE (hFileCIF,'("_cell_length_b    ",F8.4)',ERR=999) CellPar(2)
      WRITE (hFileCIF,'("_cell_length_c    ",F8.4)',ERR=999) CellPar(3)
      WRITE (hFileCIF,'("_cell_angle_alpha ",F8.4)',ERR=999) CellPar(4)
      WRITE (hFileCIF,'("_cell_angle_beta  ",F8.4)',ERR=999) CellPar(5)
      WRITE (hFileCIF,'("_cell_angle_gamma ",F8.4)',ERR=999) CellPar(6)
      WRITE (hFileCIF,'("_cell_volume",F7.1)',ERR=999) UnitCellVolume(CellPar(1),CellPar(2),CellPar(3),CellPar(4),CellPar(5),CellPar(6))
      WRITE (hFileCIF,'("_cell_formula_units_Z  ?")',ERR=999)
      SELECT CASE (JRadOption)
        CASE (1) ! X-ray lab data
! Access GUI can be expensive, especially in batch mode, should avoid.
!          CALL PushActiveWindowID
!          CALL SelectDASHDialog(IDD_Data_Properties)
!          CALL DASHWDialogGetMenu(IDF_Wavelength_Menu,iRadSelection)
!          CALL PopActiveWindowID
!          SELECT CASE (iRadSelection)
!! Winteracter menu:
!!     1 = <...>
!!     2 = Cu      <==  DEFAULT
!!     3 = Mo
!!     4 = Co
!!     5 = Cr
!!     6 = Fe
!            CASE (1)
!              tString = 'Xx'
!            CASE (2)
!              tString = 'Cu'
!            CASE (3)
!              tString = 'Mo'
!            CASE (4)
!              tString = 'Co'
!            CASE (5)
!              tString = 'Cr'
!            CASE (6)
!              tString = 'Fe'
!          END SELECT
          tString = 'Xx'
          IF (ABS(ALambda - WavelengthOf('Cu')) .LT. 0.0003) tString = 'Cu'
          IF (ABS(ALambda - WavelengthOf('Mo')) .LT. 0.0003) tString = 'Mo'
          IF (ABS(ALambda - WavelengthOf('Co')) .LT. 0.0003) tString = 'Co'
          IF (ABS(ALambda - WavelengthOf('Cr')) .LT. 0.0003) tString = 'Cr'
          IF (ABS(ALambda - WavelengthOf('Fe')) .LT. 0.0003) tString = 'Fe'
          tString = "'"//tString(1:2)//" K\a'"
        CASE (2) ! X-ray synchrotron data
          tString = 'synchrotron'
        CASE (3) ! Constant Wavelength Neutrons
        CASE (4) ! Time-of-Flight Neutrons
      END SELECT
      tLen = LEN_TRIM(tString)
      WRITE (hFileCIF,'("_diffrn_radiation_type ",A)',ERR=999) tString(1:tLen)
      WRITE (hFileCIF,'("_diffrn_radiation_wavelength",F10.5)',ERR=999) ALambda
      IF ( .NOT. FromRR ) &
        WRITE (hFileCIF,'("_refine_ls_goodness_of_fit_all ",F7.3)',ERR=999) SQRT(MAX(0.0, GoodnessOfFit))
      IF (PrefParExists) THEN
        WRITE (hFileCIF,'("_pd_proc_ls_pref_orient_corr")',ERR=999)               
        WRITE (hFileCIF,'(";")',ERR=999)
        WRITE (hFileCIF,'("  March-Dollase function")',ERR=999)
        WRITE (hFileCIF,'("  Orientation =",3(1X,I3))',ERR=999) PO_Direction(1:3)
        WRITE (hFileCIF,'("  Magnitude   = ",F6.3)',ERR=999) POMagnitude
        WRITE (hFileCIF,'(";")',ERR=999)
      ENDIF
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
!C # The following item is the same as CHI, the square root of 'CHI squared'
!C _refine_ls_goodness_of_fit_all    3.26
!C # 9. ATOMIC COORDINATES AND DISPLACEMENT PARAMETERS
!C loop_
!C        _atom_site_label
!C        _atom_site_fract_x
!C        _atom_site_fract_y
!C        _atom_site_fract_z
!C        _atom_site_occupancy
!C        _atom_site_adp_type
!C        _atom_site_U_iso_or_equiv
!C      C1     -0.10853   0.45223   0.14604  1.0 Uiso 0.038
!C      C2     -0.05898   0.41596   0.27356  1.0 Uiso 0.038

      WRITE (hFileCIF,'("loop_")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_label")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_fract_x")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_fract_y")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_fract_z")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_occupancy")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_adp_type")',ERR=999)
      WRITE (hFileCIF,'("  _atom_site_U_iso_or_equiv")',ERR=999)

      WriteCIFCommon = 0
  999 RETURN

      END FUNCTION WriteCIFCommon
!
!*****************************************************************************
!
      INTEGER FUNCTION WritePDBCommon(hFilePDB)
!
! We've got three different .pdb files now:
! 1. solution output file, just like .cssr or .ccl, if so requested by user
! 2. a single current structure to monitor progress during SA
! 3. overlapping solutions
! They all have the unit cell and symmetry part in common: this subroutine writes that part.
!
! RETURNS : 0 for success
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: hFilePDB

      INCLUDE 'Lattice.inc'

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )
      INTEGER         npdbops
      CHARACTER*20             cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      INTEGER ii, tLen
      REAL    inv(3,3)

! Initialise to failure
      WritePDBCommon = 1
      CALL InverseMatrix(f2cpdb, inv, 3)
! Add in a Header record
! In the space group table file, rhombohedral and hexagonal axes for trigonal space groups
! are flagged with ":R" and ":H" respectively. According to the pdb specification:
! "For a rhombohedral space group in the hexagonal setting, the lattice type symbol used is H."
! This also implies that if it is rhombohedral, the ":R" should simply be deleted
! (Mercury doesn't expect it and it should not be present according to the specification).
      tLen = LEN_TRIM(SGHMaStr(NumberSGTable))
      IF (SGHMaStr(NumberSGTable)(tLen-1:tLen) .EQ. ":H") THEN
        SGHMaStr(NumberSGTable)(1:1) = "H"
      ENDIF
      IF (SGHMaStr(NumberSGTable)(tLen-1:tLen-1) .EQ. ":") tLen = tLen - 2
      WRITE (hFilePDB,1050,ERR=999) (CellPar(ii),ii=1,6), SGHMaStr(NumberSGTable)(1:tLen)
 1050 FORMAT ('CRYST1',3F9.3,3F7.2,1X,A10)
! Add in V2 pdb records to store space group and symmetry
      WRITE (hFilePDB,1380,ERR=999)
      WRITE (hFilePDB,1381,ERR=999)
 1381 FORMAT ('REMARK 290 CRYSTALLOGRAPHIC SYMMETRY')
      WRITE (hFilePDB,1382,ERR=999) SGHMaStr(NumberSGTable)
 1382 FORMAT ('REMARK 290 SYMMETRY OPERATORS FOR SPACE GROUP: ',A)
      WRITE (hFilePDB,1380,ERR=999)
      WRITE (hFilePDB,1383,ERR=999)
 1383 FORMAT ('REMARK 290      SYMOP   SYMMETRY')
      WRITE (hFilePDB,1384,ERR=999)
 1384 FORMAT ('REMARK 290     NNNMMM   OPERATOR')
      DO ii = 1, npdbops
        WRITE (hFilePDB,1385,ERR=999) (ii*1000+555), cpdbops(ii)
 1385   FORMAT ('REMARK 290',5X,I6,3X,A)
      ENDDO
      WRITE (hFilePDB,1380,ERR=999)
      WRITE (hFilePDB,1386,ERR=999)
 1386 FORMAT ('REMARK 290     WHERE NNN -> OPERATOR NUMBER')
      WRITE (hFilePDB,1387,ERR=999)
 1387 FORMAT ('REMARK 290           MMM -> TRANSLATION VECTOR')
      WRITE (hFilePDB,1380,ERR=999)
      WRITE (hFilePDB,1388,ERR=999)
 1388 FORMAT ('REMARK 290 REMARK:')
! JCC included again
      WRITE (hFilePDB,1060,ERR=999) inv(1,1), inv(1,2), inv(1,3)
 1060 FORMAT ('SCALE1    ',3F10.5,'      0.00000')
      WRITE (hFilePDB,1070,ERR=999) inv(2,1), inv(2,2), inv(2,3)
 1070 FORMAT ('SCALE2    ',3F10.5,'      0.00000')
      WRITE (hFilePDB,1080,ERR=999) inv(3,1), inv(3,2), inv(3,3)
 1080 FORMAT ('SCALE3    ',3F10.5,'      0.00000')
 1380 FORMAT ('REMARK 290 ')
      WritePDBCommon = 0
  999 RETURN

      END FUNCTION WritePDBCommon
!
!*****************************************************************************
!
      SUBROUTINE PDB_SymmRecords

      IMPLICIT NONE

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops(mpdbops)
      COMMON /pdbops/ npdbops, cpdbops

!   ep added common block.   Used by Align
      REAL                 rpdb
      COMMON /fullsymmops/ rpdb(4,4,mpdbops)

      REAL rtmp(4,4)
      LOGICAL cmp
      LOGICAL PDB_CmpMat
      INTEGER I, J, K, M, iLast, iPrev
      CHARACTER*50 stout

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
            CALL PDB_MatMul(rpdb(1,1,i), rpdb(1,1,j), rtmp)
            CALL PDB_PosTrans(rtmp)
            DO k = 1, npdbops
              cmp = PDB_CmpMat(rpdb(1,1,k), rtmp)
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
        CALL M2S_SYMCON(rpdb(1,1,k),stout)
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

      REAL, INTENT (INOUT) :: r(4,4)

      DO i = 1, 3
! Tidy up any rounding errors on the translations
        r(i,4) = FLOAT(NINT(r(i,4)*10000.0))/10000.0
        IF (r(i,4).LT.-0.01) THEN
          DO WHILE (r(i,4) .LT. -0.01)
            r(i,4) = r(i,4) + 1.0
          ENDDO
        ELSE
          DO WHILE (r(i,4) .GT. 0.999)
            r(i,4) = r(i,4) - 1.0
          ENDDO
        ENDIF
      ENDDO
      r(4,4) = 1.0

      END SUBROUTINE PDB_POSTRANS
!
!*****************************************************************************
!
