      
!***********************************************************************      
      
      SUBROUTINE CheckMogulUse

      USE DRUID_HEADER
      USE VARIABLES
      USE WINTERACTER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: Confirm

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogGetString(IDF_MogulExe, MOGULEXE)
      CALL PopActiveWindowID
      IF (LEN_TRIM(MOGULEXE) .NE. 0) THEN
        UseMogul = .TRUE. 
      ELSE
        IF (UseMogul) UseMogul = (Confirm('Do you intend to use Mogul?'))
      ENDIF
      
      END SUBROUTINE CheckMogulUse
      
!***********************************************************************      
      
      SUBROUTINE WriteMogulMol2(iFRow)

! Writes Mol2 file for MOGUL.  
! Calls GetAtomLineNumbers   
  
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFRow

      INTEGER, EXTERNAL :: WriteMol2
      INTEGER I,K
      CHARACTER(MaxPathLength) MogulMol2
      INTEGER tLength, BondNr
      INTEGER iFrg, DoF


!     Given the number of the parameter want to know which zmatrix, fragment it belongs to.
      iFrg = 0
      DO i = 1, maxDOF
        DO k = 1, nfrag
          IF (IFRow .EQ. zm2par(i,k)) THEN
            DoF = i
            iFrg = k
            EXIT
          ENDIF
        ENDDO
        IF (iFrg .NE. 0) EXIT
      ENDDO


      natcry = NATOMS(iFrg)
      CALL makexyz(natcry,BLEN(1, iFrg),ALPH(1, iFrg),BET(1, iFrg),IZ1(1, iFrg),IZ2(1, iFrg),IZ3(1, iFrg),axyzo)
      DO I = 1, natcry
        aelem(I) = zmElementCSD(I, iFrg)
        atomlabel(I) = OriginalLabel(I, iFrg)
      ENDDO
      nbocry = NumberOfBonds(iFrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr, iFrg)
        bond(BondNr,1) = Bonds(1,BondNr, iFrg)
        bond(BondNr,2) = Bonds(2,BondNr, iFrg)
      ENDDO
      tLength = LEN_TRIM(frag_file(iFrg))
      MogulMol2 = frag_file(iFrg)(1:tLength-8)//'_mogul.mol2'

! Write mol2 file
      IF (WriteMol2(MogulMol2,.FALSE., iFrg) .EQ. 1) THEN
        CALL GetAtomLineNumbers(MogulMol2, IFrg, DoF, iFRow)
      ELSE
        CALL DebugErrorMessage('Error writing temporary file.')
      ENDIF


      END SUBROUTINE WriteMogulMol2

!*****************************************************************

      SUBROUTINE GetAtomLineNumbers(MogulMol2, iFrg, DoF, iFRow)

! For parameter, gets corresponding line numbers of atoms from Mol2
! file.  MOGUL does not use Atom Labels but AtomIDs.
! Calls WriteMogulScript.
      
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg, DoF
      INTEGER, INTENT (IN   ) :: iFRow

      CHARACTER(MaxPathLength), INTENT(IN   ) :: MogulMol2

      CHARACTER*36 TempAtomLabel
      
      CHARACTER*5, Atom(4)
      INTEGER Marker(5), AtomID(4)
      INTEGER I,J
      INTEGER tLength

      TempAtomLabel = czmpar(DoF, iFrg)
      tLength = LEN_TRIM(TempAtomLabel)

      DO I = 1,4
       Atom(I) = '     '
      END DO

      I = 0
      J = 1
      DO WHILE (I .LE. tLength) !length of label
        I = I + 1
        IF(TempAtomLabel(I:I) .EQ. "(" ) THEN
          Marker(J) = I
          J = J+1
        ENDIF
        IF(TempAtomLabel(I:I) .EQ. ":" ) THEN
          Marker(J) = I
          J = J + 1
        ENDIF       
        IF(TempAtomLabel(I:I) .EQ. ")" ) THEN
          Marker(J) = I
          EXIT
        ENDIF
      ENDDO


      Atom(1) = TempAtomLabel(Marker(1)+1 : Marker(2)-1) 
      Atom(2) = TempAtomLabel(Marker(2)+1 : Marker(3)-1)
      Atom(3) = TempAtomLabel(Marker(3)+1 : Marker(4)-1)
      Atom(4) = TempAtomLabel(Marker(4)+1 : Marker(5)-1)

      DO J = 1,4 ! Mogul does not use atom labels but number of atom in Mol2 file 
        DO I = 1, MaxDoF
          IF(Atom(J) .EQ. AtomLabel(izmbid(I,IFrg))) THEN
            AtomID(J) = I
            EXIT 
          ENDIF
        ENDDO
      ENDDO

      CALL WriteMogulScript(MogulMol2, AtomID, iFRow)  

      END SUBROUTINE GetAtomLineNumbers
     
     
!*****************************************************************   
     
      SUBROUTINE WriteMogulScript(MogulMol2, AtomID, IFRow)

! Writes the Mogul Script file which contains instructions for Mogul such
! as molecule file name, output filename, Torsion Angle Fragment.
! Calls Mogul
      
      USE WINTERACTER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, DIMENSION(4), INTENT (IN   )   :: AtomID
      CHARACTER(MaxPathLength), INTENT(IN   ) :: MogulMol2
      INTEGER, INTENT (IN   ) :: iFRow


      INTEGER I
      CHARACTER(MaxPathLength) CurrentDirectory, Script_file, MogulOutputFile
      INTEGER tLength, olength

      CALL IOsDirName(CurrentDirectory)

      tLength = LEN_TRIM(MogulMol2)
      Script_file = MogulMol2(1:tLength-10)//'script.qf'
      MogulOutputFile = MogulMol2(1:tLength-10)//'mogul.out'

      olength = LEN_TRIM(MogulOutputfile)

      OPEN(240,FILE=Script_file,STATUS='UNKNOWN', ERR = 999)
      WRITE(240,10) MogulMol2(1:tlength)
10    FORMAT(('MOGUL MOLECULE '), A)
      WRITE(240,20) MogulOutputFile(1:olength)
20    FORMAT(('MOGUL OUTPUT_FILE '), A)
      WRITE(240,25) 
25    FORMAT('MOGUL EDIT BOND_TYPES GUESS ALL_3D')
      WRITE(240, 27)
27    FORMAT('CONFIG SEARCH ALL GENERALISATION ON')

      SELECT CASE (kzmpar2(IFrow))
        CASE(3) !Torsion
          WRITE(240,30) (AtomID(I), I = 1,4)
30        FORMAT(('TORSION '), 4(I3,1X))
        CASE(4) ! Angle
          WRITE(240,31) (AtomID(I), I= 1,3)
31        FORMAT(('ANGLE '), 3(I3, 1X))
        CASE(5) ! Bond
          WRITE(240,32) (AtomID(I), I= 1,2)
32        FORMAT(('BOND '), 2(I3, 1X))
      END SELECT

      WRITE(240,40)
40    FORMAT(('MOGUL GUI OPEN'))
      
      CLOSE (240)

      CALL Mogul(Script_file, MogulOutputFile, iFRow)
      RETURN       

999   CALL ErrorMessage('Error generating Mogul Script file')
      RETURN

      END SUBROUTINE WriteMogulScript

!********************************************************************************

      SUBROUTINE Mogul(Script_file, MogulOutputFile, iFRow)

! Calls command to execute Mogul.  Path to Mogul in Configuration Window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR


      IMPLICIT NONE

      CHARACTER(MaxPathLength), INTENT(IN   ) :: Script_file, MogulOutputFile
      INTEGER, INTENT (IN   ) :: iFRow
            
      INTEGER I,M
      LOGICAL exists

      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogGetString(IDF_MogulExe,MOGULEXE)
      CALL PopActiveWindowID
      I = LEN_TRIM(MOGULEXE)
      INQUIRE(FILE = MOGULEXE(1:I),EXIST=exists)
      IF (.NOT. exists) GOTO 999
      M = InfoError(1) ! Clear errors
      CALL IOSCommand(MOGULEXE(1:I)//' -ins '//'"'//Script_file(1:LEN_TRIM(Script_file))//'"', ProcBlocked)
      IF (InfoError(1) .NE. 0) GOTO 999
      IF(kzmpar2(IFRow) .EQ. 3) THEN ! Modal Torsion so try to process
        CALL ProcessMogulOutput(MogulOutputFile, iFRow)
      ENDIF
      RETURN
999   CALL ErrorMessage("DASH could not launch Mogul."//CHAR(13)//&
                        "The Mogul executable is currently configured to launch the program: "//CHAR(13)//&
                        MOGULEXE(1:I)//CHAR(13)//&
                        "This can be changed in the Configuration Window"//CHAR(13)//&
                        "under Options in the menu bar.")


      END SUBROUTINE Mogul



!********************************************************************************
      
      SUBROUTINE ProcessMogulOutput(MogulOutputFile, iFRow)

! Uses simple criteria to parse the output from Mogul.  Will recommend the 
! type of modal torsion angle range and return "standard" torsion angle ranges.
! Works ok but is very simple (just pattern recognition) so is not 100% accurate.
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES


      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER, INTENT (IN   ) :: iFRow

      CHARACTER(MaxPathLength), INTENT(IN   ) ::  MogulOutputFile
      INTEGER nlin, I
      CHARACTER*255 line
      CHARACTER*12 Distribution
      CHARACTER*2 Colon
      CHARACTER*40 MogulText
      LOGICAL exists
      LOGICAL Assigned
      LOGICAL blank
      INTEGER, DIMENSION(180) :: TC
      INTEGER NumberofBins, HalfBins, UpperBins, MaxAngle, MinAngle, Bin
      INTEGER Lmarker1, Hmarker1, LMarker2, HMarker2 !count of molecules in bin
      INTEGER LIndex1(1), HIndex1(1), LIndex2(1), HIndex2(1)
      INTEGER TotalSum, TempSum

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      MogulText = ' '
      
      INQUIRE(FILE = MogulOutputFile,EXIST=exists)
      IF (.NOT. exists) GOTO 999
      NumberofBins = 0 
      OPEN(240,FILE=MogulOutputFile,STATUS='UNKNOWN', ERR = 999)
      I = 0
      DO WHILE (I .EQ. 0)
        READ(240, 10) nlin, line
10      FORMAT (q,a)
        IF ((line(1:6) .EQ. "NOHITS") .OR. (line(1:6) .EQ. "ERROR")) GOTO 888
        IF (line(1:5).eq."STATS") THEN
          READ(240,*) Distribution, MinAngle, MaxAngle, Bin, NumberofBins, Colon, TC(1:NumberofBins)
          I = 1 
        ENDIF
      ENDDO
      CLOSE(240)
! Will not delete Mogul Output in this release as it may be useful for support problems
! If DASH_Mogul comes about, may want to delete output file.
!      CALL IosDeleteFile(MogulOutputFile) 
      
      TotalSum = 0
      DO I = 1,NumberOfBins
        TotalSum = TotalSum + TC(I) ! number of hits in histogram
      ENDDO

      IF (TotalSum .LT. 30) THEN
        MogulText = 'No recommendation - not enough data'
        ModalFlag(IFROW) = 1
        Assigned = .TRUE.
        CALL WDialogSelect(IDD_ModalDialog)
        CALL WDialogPutString(IDF_MogulText, MogulText)
        RETURN
      ENDIF
      
      HalfBins = NumberofBins/2
      UpperBins = HalfBins + (Halfbins/3)*2
   

      CALL MinimumValue(TC, 1,HalfBins,LMarker1, LIndex1, Halfbins)
      CALL MaximumValue(TC, 1,HalfBins,HMarker1, HIndex1, Halfbins)

      Assigned = .FALSE.
      Blank = .FALSE.

      IF (REAL(HMarker1)/REAL(TotalSum) .LT. 0.05) THEN
        blank = .TRUE.
      ENDIF
      

      IF (HIndex1(1) .LE. LIndex1(1)) THEN ! Peak, Trough - 4 possible scenarios
        
        CALL MaximumValue(TC, Halfbins+1, Numberofbins, HMarker2, HIndex2, Halfbins)
        IF (HIndex2(1) .GT. Upperbins) THEN ! Peak above 150 degs
          IF (.NOT. Blank) THEN
            MogulText = 'Planar, Bimodal'
            ModalFlag(IFRow) = 2
            LB(IFRow) = -160.00
            UB(IFRow) =  160.00
            Assigned = .TRUE.
          ELSE
            MogulText = 'Bimodal around 180 degrees'
            ModalFlag(IFrow) = 2
            LB(IFRow) = 160.00
            UB(IFRow) = 180.00
            Assigned = .TRUE.
          ENDIF
        ENDIF
        IF ((HIndex2(1) .GT. HalfBins+1) .AND. (HIndex2(1) .LT. UpperBins) .AND. (Blank .EQ. .FALSE.)) THEN !second peak of trimodal
          MogulText = 'Trimodal -30 to 30 degrees'
          ModalFlag(IFRow) = 3
          LB(IFRow) = -30.00
          UB(IFRow) =  30.00
          Assigned = .TRUE.
        ENDIF
        IF ((HMarker2 .EQ. 0) .OR. (REAL(HMarker2)/REAL(TotalSum) .LT. 0.05)) THEN ! nothing here
          MogulText = 'Bimodal around 0 degrees'
          ModalFlag(IFRow) = 2
          LB(IFRow) = -20.00
          UB(IFRow) =   0.00
          Assigned = .TRUE.
        ENDIF
     
      ENDIF

      IF (LIndex1(1) .LT. HIndex1(1)) THEN !Trough, Peak - 3 scenarios
       
       CALL MaximumValue(TC, HalfBins+1, NumberofBins, HMarker2, Hindex2,Halfbins)
       IF (HIndex2(1) .GT. UpperBins) THEN !Peak above 150 degs
         CALL MinimumValue(TC, HalfBins+1, NumberofBins, LMarker2, Lindex2, Halfbins)
         IF ((LMarker2 .EQ. 0) .OR. (REAL(LMarker2)/REAL(TotalSum) .LT. 0.05)) THEN ! minimum in between peaks         
           IF (.NOT. Blank) THEN !Peak in first half
             IF (Hindex2(1) - Hindex1(1) .GT. (100/Bin)) THEN !Separation of peaks > 100 degs
               MogulText = 'Trimodal +150 to -150'
               ModalFlag(IFRow) = 3
               LB(IFrow) = -150.00
               UB(IFRow) =  150.00
               Assigned = .TRUE.
             ENDIF
           ELSE
             MogulText = 'Bimodal around 180 degrees'
             ModalFlag(IFRow) = 2
             LB(IFrow) = 160.00
             UB(IFRow) = 180.00
             Assigned = .TRUE.
           ENDIF
         ELSE
           MogulText = 'No recommendation'
           ModalFlag(IFRow) = 1
           Assigned = .TRUE.
         ENDIF
       ENDIF
       IF (HIndex2(1) .LT. UpperBins) THEN ! Bimodal- single bump
         MogulText = 'Bimodal'
         ModalFlag(IFRow) = 2
         LB(IFRow) = 45.00
         UB(IFRow) = 135.00
         Assigned = .TRUE.
       ENDIF

      ENDIF

     
      TempSum = Lmarker1 + HMarker1 + Hmarker2
      TempSum = NINT(REAL(Tempsum)/3)
      IF (REAL(LMarker1) / REAL(TempSum) .GT. .50) THEN
       MogulText = 'No recommendation'
       ModalFlag(IFRow) = 1
       Assigned = .TRUE.
      ENDIF

      IF (.NOT. Assigned) THEN
        MogulText = 'No recommendation'
        ModalFlag(IFRow) = 1
      ENDIF

      CALL WDialogSelect(IDD_ModalDialog)
      CALL WDialogPutString(IDF_MogulText, MogulText)


      RETURN

999   CALL ErrorMessage("Mogul could not read output file.")
888   ModalFlag(IFRow) = 1 ! Will not default to modal ranges in dialog

      END SUBROUTINE ProcessMogulOutput

!*********************************************************************************

      SUBROUTINE MinimumValue(TC, Lower, Upper, Lmarker, Lindex, Halfbins)

      USE DRUID_HEADER
      USE VARIABLES


      IMPLICIT NONE

      INTEGER, DIMENSION(180), INTENT(IN   ) ::  TC
      INTEGER,                 INTENT(IN   ) ::  Halfbins
      INTEGER Lower, Upper
      INTEGER LMarker
      INTEGER LIndex(1)

      Lmarker = MINVAL(TC(Lower: Upper))
      LIndex = MINLOC(TC(Lower: Upper))

      IF (Lower .GT. Halfbins) Lindex(1) = Lindex(1) + (Lower - 1)

      
      END SUBROUTINE MinimumValue

!*********************************************************************************

      SUBROUTINE MaximumValue(TC, Lower, Upper, Hmarker, HIndex, Halfbins)

      USE DRUID_HEADER
      USE VARIABLES


      IMPLICIT NONE

      INTEGER, DIMENSION(180), INTENT(IN   ) ::  TC
      INTEGER,                 INTENT(IN   ) ::  Halfbins
      INTEGER Lower, Upper
      INTEGER Hmarker
      INTEGER HIndex(1)

      Hmarker = MAXVAL(TC(Lower: Upper))
      HIndex = MAXLOC(TC(Lower: Upper))

      IF (Lower .GT. Halfbins) Hindex(1) = Hindex(1) + (Lower - 1)

      
      END SUBROUTINE MaximumValue