      SUBROUTINE Upload_Positions()

      USE WINTERACTER
      USE DRUID_HEADER 

      INCLUDE 'PARAMS.INC'

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
        IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
        CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
        XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
        IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)

      COMMON /PROFTIC/ NTic,IH(3,MTic),ArgK(MTic),DStar(MTic)

      COMMON /ALLPEAKS/ NTPeak,AllPkPosVal(MTPeak),AllPkPosEsd(MTPeak),&
        PkArgK(MTPeak),PkTicDif(MTPeak),PkProb(MTPeak), &
        IOrdTem(MTPeak),IHPk(3,MTPeak),IArgK(MTPeak)

      INTEGER ICurSel

      NTPeak = 0
      Do J = 1, NumPeakFitRange
        Do I = 1, NumInPFR(J)
          NTPeak = NTPeak + 1
          AllPkPosVal(NTPeak) = PkPosVal(I,J)
          AllPkPosEsd(NTPeak) = PkPosEsd(I,J)
        END DO
      END DO
      DO I = 1, NumObsTic
        NTPeak = NTPeak + 1
        AllPkPosVal(NTPeak) = XObsTic(I)
        AllPkPosEsd(NTPeak) = 0.005
      END DO
      CALL SORT_REAL(AllPkPosVal,IOrdTem,NTPeak)
      IF (NTic .NE. 0) THEN
!.. Let's find the closest peaks and their distribution around the observed peak positions
        IR1 = 1
        DO I = 1, NTPeak
          IOrd = IOrdTem(I)
          xtem = ArgK(IR1)-AllPkPosVal(IOrd)
          atem = ABS(xtem)
          item = IR1
          DO IR = IR1, NTic
            xnew = ArgK(IR) - AllPkPosVal(IOrd)
            anew = ABS(xnew)
            IF (anew .LE. atem) THEN
              item = IR
              atem = anew
              xtem = xnew
            END IF
            IF (xnew .GT. 0.0) THEN
              IR1 = MAX(1,IR-1)
              GOTO 20
            END IF
          END DO
 20       PkTicDif(I) = xtem
          DO II = 1, 3
            IHPk(II,I) = IH(II,item)
          END DO
          PkArgK(I) = ArgK(Item)
          IArgK(I) = Item
        END DO
        IF (NTPeak .EQ. 1) THEN
          SigmDif = 0.01
        ELSE
          PfTDMin = PkTicDif(1)
          PfTDMax = PkTicDif(1)
          DO II = 1, NTPeak
            PfTDMin = MIN(PfTDMin,PkTicDif(II))
            PfTDMax = MAX(PfTDMax,PkTicDif(II))
          END DO
          SigmDif = 0.2886751345948*Abs(PfTDMax-PfTDMin)
        END IF
        DO I = 1, NTPeak
          IOrd = IOrdTem(I)
          IA = IArgK(I)
          IRef1 = MAX(1,IA-5)
          IRef2 = MIN(NTic,IA+5)
          ProbTot = 0.0
          ProbTop = 0.0
          DifMin = ABS(AllPkPosVal(IOrd)-ArgK(IA))
          DifMinSq = DifMin**2
          ArgBot = 0.5/(SigmDif**2+AllPkPosEsd(IOrd)**2)
          DO IR = IRef1, IRef2
            ArgTop=(AllPkPosVal(IOrd)-ArgK(IR))**2
            ProbAdd=EXP(-ArgTop*ArgBot)
            IF (ABS(ArgTop-DifMinSq).LT.1.e-10) THEN
              ProbTop=ProbTop+ProbAdd
            END IF
            ProbTot=ProbTot+ProbAdd
          END DO
          PkProb(IOrd)=ProbTop/ProbTot
        END DO
      END IF
! Write out all the peak positions in an ordered list ...
      ICurSel = WinfoDialog(CurrentDialog)
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogClearField(IDD_Peak_Positions_Grid)
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WGridRows(IDF_Peak_Positions_Grid,NTPeak)
      IF (NTPeak .GT. 0) THEN
        CALL WDialogFieldState(ID_Index_Output,Enabled)
        DO I = 1, NTPeak
          IOrd=IOrdTem(i)
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,1,I,AllPkPosVal(IOrd),'(F12.4)')
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,2,I,AllPkPosEsd(IOrd),'(F12.4)')
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,3,I,PkArgK(I),'(F12.4)')
          DifTem=AllPkPosVal(IOrd)-PkArgK(I)
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,4,I,DifTem,'(F12.4)')
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,5,I,IHPk(1,I))
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,6,I,IHPk(2,I))
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,7,I,IHPk(3,I))
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,8,I,PkProb(IOrd),'(F8.3)')
        END DO
      ELSE
        CALL WDialogFieldState(ID_Index_Output,Disabled)
        DO I = 1, 8
          CALL WGridClearCell(IDF_Peak_Positions_Grid,I,1)
        END DO
      END IF
! Now do a refinement ...
      CALL RefineLattice()
      IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)

      END SUBROUTINE Upload_Positions