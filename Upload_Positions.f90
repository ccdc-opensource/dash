      subroutine Upload_Positions()
!
      use Winteracter
      use druid_header 
!

      INCLUDE 'PARAMS.INC'

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
      IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
      CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
!

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
      PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
      PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
      PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
      itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
!

      COMMON /PROFTIC/ NTic,IH(3,MTic),ArgK(MTic),DStar(MTic)
!
      Parameter (MTPeak=100)
      COMMON /ALLPEAKS/ NTPeak,AllPkPosVal(MTPeak),AllPkPosEsd(MTPeak),&
      PkArgK(MTPeak),PkTicDif(MTPeak),PkProb(MTPeak), &
      IOrdTem(MTPeak),IHPk(3,MTPeak),IArgK(MTPeak)
      Character(Len=48) ChPtGp

!>JCC 
      Integer ICurSel

      NTPeak=0
      Do J=1,NumPeakFitRange
        Do I=1,NumInPFR(J)
          NTPeak=NTPeak+1
          AllPkPosVal(NTPeak)=PkPosVal(I,J)
          AllPkPosEsd(NTPeak)=PkPosEsd(I,J)
        End Do
      End Do
!
      Do I=1,NumObsTic
        NTPeak=NTPeak+1
        AllPkPosVal(NTPeak)=XObsTic(I)
        AllPkPosEsd(NTPeak)=0.005
      End Do
!
      CALL SORT_REAL(AllPkPosVal,IOrdTem,NTPeak)
!
      If (NTic.ne.0) Then
!.. Let's find the closest peaks and their distribution around the observed peak positions
        IR1=1
        Do I=1,NTPeak
          IOrd=IOrdTem(I)
          xtem=ArgK(IR1)-AllPkPosVal(IOrd)
          atem=abs(xtem)
          item=IR1
          Do IR=IR1,NTic
            xnew=ArgK(IR)-AllPkPosVal(IOrd)
            anew=abs(xnew)
            If (anew.le.atem) then
              item=IR
              atem=anew
              xtem=xnew
            End If
            If (xnew.gt.0.) Then
              IR1=max(1,IR-1)
              goto 20
            End If
          End Do
 20       PkTicDif(I)=xtem
          Do II=1,3
            IHPk(II,I)=IH(II,item)
          End Do
          PkArgK(i)=ArgK(Item)
          IArgK(i)=Item
        End Do
        If (NTPeak.eq.1) Then
          SigmDif=0.01
!	     write(76,*) ' SigmDif= ',SigmDif,NTPeak
        Else
          PfTDMin=PkTicDif(1)
          PfTDMax=PkTicDif(1)
          Do II=1,NTPeak
            PfTDMin=Min(PfTDMin,PkTicDif(II))
            PfTDMax=Max(PfTDMax,PkTicDif(II))
          End Do
          SigmDif= 0.2886751345948*Abs(PfTDMax-PfTDMin)
!	     write(76,*) ' SigmDif= ',SigmDif,NTPeak
        End If
        Do I=1,NTPeak
          IOrd=IOrdTem(I)
          IA=IArgK(I)
          IRef1=Max(1,IA-5)
          IRef2=Min(NTic,IA+5)
          ProbTot=0.
          ProbTop=0.
          DifMin=Abs(AllPkPosVal(IOrd)-ArgK(IA))
          DifMinSq=DifMin**2
          ArgBot=0.5/(SigmDif**2+AllPkPosEsd(IOrd)**2)
          Do IR=IRef1,IRef2
            ArgTop=(AllPkPosVal(IOrd)-ArgK(IR))**2
            ProbAdd=Exp(-ArgTop*ArgBot)
            If (Abs(ArgTop-DifMinSq).lt.1.e-10) Then
              ProbTop=ProbTop+ProbAdd
            End If
            ProbTot=ProbTot+ProbAdd
          End Do
          PkProb(IOrd)=ProbTop/ProbTot
!	     write(76,*) ' Prob= ',I,Iord,PkProb(IOrd),ProbTop,ProbTot
        End Do
      End If
!
! Write out all the peak positions in an ordered list ...
!C>>      call WDialogLoad(IDD_Peak_Positions)
      ICurSel = WinfoDialog(CurrentDialog)
	  CALL WDialogSelect(IDD_Peak_Positions)
	  CALL WDialogClearField(IDD_Peak_Positions_Grid)
	  CALL WDialogSelect(IDD_Peak_Positions)
	  CALL WGridRows(IDF_Peak_Positions_Grid,NTPeak)

	  IF (NTPeak .GT. 0) THEN
	    CALL WDialogFieldState(ID_Index_Output,Enabled)
        DO I=1,NTPeak
          IOrd=IOrdTem(i)
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,1,i,AllPkPosVal(IOrd),'(F12.4)')
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,2,i,AllPkPosEsd(IOrd),'(F12.4)')
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,3,i,PkArgK(I),'(F12.4)')
          DifTem=AllPkPosVal(IOrd)-PkArgK(I)
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,4,i,DifTem,'(F12.4)')
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,5,i,IHPk(1,I))
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,6,i,IHPk(2,I))
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,7,i,IHPk(3,I))
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,8,i,PkProb(IOrd),'(F8.3)')
!	    write(76,*) ' PutCell ',I,(IHPk(jjj,I),jjj=1,3),AllPkPosVal(IOrd),AllPkPosEsd(IOrd)
        END DO
	  ELSE
	    CALL WDialogFieldState(ID_Index_Output,Disabled)
	    DO I = 1,8
          CALL WGridClearCell(IDF_Peak_Positions_Grid,i,1)
		END DO
	  END IF


!  
! Now do a refinement ...
!C>> JCC       call WDialogLoad(IDD_Crystal_Symmetry)
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      Call WDialogGetMenu(IDF_Crystal_System_Menu,IPtGp,ChPtGp)
!      Write(76,*) 'Crystal system ',IPtGp
      Call RefineLattice()
	  IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)
!
!       
!
      endsubroutine Upload_Positions