      subroutine Upload_Crystal_Data()
!
      use Winteracter
      use druid_header

! Update all the cell constants ...
	  Call Upload_Cell_Constants

	  Call Upload_Zero_Point

      Call UpdateWavelength(Alambda)
!
! Let's do a symmetry check

      Call Check_Crystal_Symmetry()
!
      Call RefineLattice()           
!
      endsubroutine Upload_Crystal_Data
!

	  subroutine Upload_Zero_Point()
      use Winteracter
      use druid_header
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA 
	  Integer ICurSel
	  ICurSel = WInfoDialog(CurrentDialog)
! Update all the cell constants ...
      Call WDialogSelect(IDD_Peak_Positions)
      Call WDialogPutReal(IDF_zeropt_refine,ZeroPoint,'(F10.4)')
	  if (ICurSel.NE.0) CALL WDialogSelect(ICurSel)
	  endsubroutine Upload_Zero_Point
!
!
	  subroutine Upload_Cell_Constants()

      use Winteracter
      use druid_header
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA 
	  Integer ICurSel
!

	  ICurSel = WInfoDialog(CurrentDialog)
! Update all the cell constants ...
!C>> JCC      Call WDialogLoad(IDD_Crystal_Symmetry)
      Call WDialogSelect(IDD_Crystal_Symmetry)
      Call WDialogPutReal(IDF_a_latt,CellPar(1),'(F10.5)')
      Call WDialogPutReal(IDF_b_latt,CellPar(2),'(F10.5)')      
      Call WDialogPutReal(IDF_c_latt,CellPar(3),'(F10.5)')      
      Call WDialogPutReal(IDF_alp_latt,CellPar(4),'(F10.3)')      
      Call WDialogPutReal(IDF_bet_latt,CellPar(5),'(F10.3)')      
      Call WDialogPutReal(IDF_gam_latt,CellPar(6),'(F10.3)')

!C>> And in the wizard too

	  Call WDialogSelect(IDD_PW_Page1)
      Call WDialogPutReal(IDF_PW_a_latt,CellPar(1),'(F10.5)')
      Call WDialogPutReal(IDF_PW_b_latt,CellPar(2),'(F10.5)')      
      Call WDialogPutReal(IDF_PW_c_latt,CellPar(3),'(F10.5)')      
      Call WDialogPutReal(IDF_PW_alp_latt,CellPar(4),'(F10.3)')      
      Call WDialogPutReal(IDF_PW_bet_latt,CellPar(5),'(F10.3)')      
      Call WDialogPutReal(IDF_PW_gam_latt,CellPar(6),'(F10.3)')
!
!C>> And in the peak positions box
      Call WDialogSelect(IDD_Peak_Positions)
      Call WDialogPutReal(IDF_a_refine,CellPar(1),'(F10.5)')
      Call WDialogPutReal(IDF_b_refine,CellPar(2),'(F10.5)')      
      Call WDialogPutReal(IDF_c_refine,CellPar(3),'(F10.5)')      
      Call WDialogPutReal(IDF_alp_refine,CellPar(4),'(F10.3)')      
      Call WDialogPutReal(IDF_bet_refine,CellPar(5),'(F10.3)')      
      Call WDialogPutReal(IDF_gam_refine,CellPar(6),'(F10.3)')

	  if (ICurSel.NE.0) CALL WDialogSelect(ICurSel)
	  endsubroutine Upload_Cell_Constants


	  subroutine Download_Cell_Constants(IDownFrom)

      use Winteracter
      use druid_header
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA 
	  Integer ICurSel,IDownFrom

	  REAL a,b,c,alpha,beta,gamma	  
!

	  ICurSel = WInfoDialog(CurrentDialog)
! Get all the cell constants from the selected area
!C>> JCC      Call WDialogLoad(IDD_Crystal_Symmetry)

	  IF (IDownFrom .EQ. IDD_Crystal_Symmetry) THEN
		Call WDialogSelect(IDD_Crystal_Symmetry)
		Call WDialogGetReal(IDF_a_latt,a)
		Call WDialogGetReal(IDF_b_latt,b)      
		Call WDialogGetReal(IDF_c_latt,c)      
		Call WDialogGetReal(IDF_alp_latt,alpha)      
		Call WDialogGetReal(IDF_bet_latt,beta)      
		Call WDialogGetReal(IDF_gam_latt,gamma)

	  ELSE IF (IDownFrom .EQ. IDD_PW_Page1) THEN
		Call WDialogSelect(IDD_PW_Page1)
		Call WDialogGetReal(IDF_PW_a_latt,a)
		Call WDialogGetReal(IDF_PW_b_latt,b)      
		Call WDialogGetReal(IDF_PW_c_latt,c)      
		Call WDialogGetReal(IDF_PW_alp_latt,alpha)      
		Call WDialogGetReal(IDF_PW_bet_latt,beta)      
		Call WDialogGetReal(IDF_PW_gam_latt,gamma)
	  
	  END IF

	  IF (a     .GT. 0.0) cellpar(1) = a
	  IF (b     .GT. 0.0) cellpar(2) = b
	  IF (c     .GT. 0.0) cellpar(3) = c
	  IF (alpha .GT. 0.0) cellpar(4) = alpha
	  IF (beta  .GT. 0.0) cellpar(5) = beta
	  IF (gamma .GT. 0.0) cellpar(6) = gamma


	  if (ICurSel.NE.0) CALL WDialogSelect(ICurSel)
	  endsubroutine Download_Cell_Constants




      subroutine Check_Crystal_Symmetry()
!
!
      use Winteracter
      use druid_header
	  INCLUDE 'Lattice.inc'
	  INTEGER OldLatBrav,ICurSel
!

	  ICurSel = WInfoDialog(CurrentDialog)
!C>> JCC       Call WDialogLoad(IDD_Crystal_Symmetry)
      Call WDialogSelect(IDD_Crystal_Symmetry)
      Call WDialogGetReal(IDF_a_latt,CellPar(1))
      Call WDialogGetReal(IDF_b_latt,CellPar(2))      
      Call WDialogGetReal(IDF_c_latt,CellPar(3))      
      Call WDialogGetReal(IDF_alp_latt,CellPar(4))      
      Call WDialogGetReal(IDF_bet_latt,CellPar(5))      
      Call WDialogGetReal(IDF_gam_latt,CellPar(6))
!

!C>> JCC Changed to call Check_Lattice_Type
!
	  OldLatBrav = LatBrav
	  Call Check_Lattice_Type
	  IF (OldLatBrav .NE. LatBrav) THEN
		CALL Set_Crystal_Symmetry(LatBrav)
	  END IF
!
	  IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)
      endsubroutine Check_Crystal_Symmetry
!
!
	  subroutine Set_Crystal_Symmetry(LatNum)
	  use Winteracter
      use druid_header
	  INCLUDE 'Lattice.inc'

      INTEGER ICurSel

!C>> JCC       Call WDialogLoad(IDD_Crystal_Symmetry)
	  IF (LatNum .GT. 0 .AND. LatNum .LT. 12) THEN
	  	ICurSel = WInfoDialog(CurrentDialog)
		LatBrav = LatNum
		Call WDialogSelect(IDD_Crystal_Symmetry)
!C>> JCC ListBrav changed to LatBrav here.
		Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
		Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
		Call WDialogSelect(IDD_PW_Page1)
		Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
		IF (ICurSel .NE. 0)  CALL WDialogSelect(ICurSel)
	  END IF
	  end subroutine Set_Crystal_Symmetry

!
      Subroutine RefineLattice()
!
!
      use Winteracter
      use druid_header
!
      REAL ChiGetLattice
      EXTERNAL ChiGetLattice
!
      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL XDD(MPAR),DXDD(MPAR),COVDD(MMPAR)
!
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      COMMON /LATREFCMN/ LatBrav,IHLR(3,MVAL)
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
!
      Parameter (MTPeak=100)
      COMMON /ALLPEAKS/ NTPeak,AllPkPosVal(MTPeak),AllPkPosEsd(MTPeak),&
      PkArgK(MTPeak),PkTicDif(MTPeak),PkProb(MTPeak), &
      IOrdTem(MTPeak),IHPk(3,MTPeak),IArgK(MTPeak)
!
      COMMON /AASVAL/ AAS(6),AASLO(6),AASHI(6)
      INTEGER IASS(6)
      LOGICAL NOCREF
!
      REAL GReal(3,3),GRec(3,3)
      INTEGER KELPT(6,10)
      DATA KELPT /2,3,4,5,6,7, 2,3,4,5,10,10, 2,3,4,10,5,10, 2,3,4,10,10,5, &
      2,3,4,10,10,10, 2,2,3,10,10,10, 2,2,3,9,10,10, &
      2,2,2,3,3,3, 2,2,3,9,10,10, 2,2,2,10,10,10/ 
!>> JCC Check the wavelength: if the user has not set it, then
!>> we should not be here!

	  INCLUDE 'statlog.inc'
	  IF (.NOT.wvlnok) return

      NVal=0
      Do I=1,NTPeak
        IOrd=IOrdTem(I)
        If(PkProb(Iord).gt.0.95) Then
          NVal=NVal+1
          Do II=1,3
            IHLR(II,NVal)= IHPk(II,I)
          End Do
          YVal(NVal)=AllPkPosVal(IOrd)
          EVal(NVal)=AllPkPosEsd(IOrd)
!	     write(76,*) ' Refine ',i,(IHLR(II,I),ii=1,3),yval(IOrd),eval(IOrd) 
        End If
      End Do
!
!C>> JCC Updated to the correct values ...!
      If (LatBrav.eq.2) Then
! Triclinic
        NDD=7
      Else If (LatBrav.eq.3) Then
! Monoclinic (a-axis)
        NDD=5
      Else If (LatBrav.eq.4) Then
! Monoclinic (b-axis)
        NDD=5
      Else If (LatBrav.eq.5) Then
! Monoclinic (c-axis)
        NDD=5
      Else If (LatBrav.eq.6) Then
! Orthorhombic
        NDD=4
      Else If (LatBrav.eq.7) Then
! Tetragonal
        NDD=3 
      Else If (LatBrav.eq.8) Then
! Trigonal
        NDD=3
      Else If (LatBrav.eq.9) Then
! Rhombohedral
        NDD=3
      Else If (LatBrav.eq.10) Then
! Hexagonal
        NDD=3
      Else If (LatBrav.eq.11) Then
! Cubic
        NDD=2
      End If
      If (NDD.eq.0) Return
      If (NVal.eq.0) Then
        If (NTPeak.gt.NDD) Then
	     Call WMessageBox(OKOnly,ExclamationIcon,CommonOK,&
             'Problems with cell refinement!'//CHAR(13)// &
                       'Have you entered the cell constants?','Cell refinement failure')
        End If
        Return
      End If
!
      DO I=1,3
        GREAL(I,I)=CELLPAR(I)**2
      END DO
      GREAL(1,2)=CELLPAR(1)*CELLPAR(2)*COSD(CELLPAR(6))
      GREAL(1,3)=CELLPAR(1)*CELLPAR(3)*COSD(CELLPAR(5))     
      GREAL(2,3)=CELLPAR(2)*CELLPAR(3)*COSD(CELLPAR(4))
      GREAL(2,1)=GREAL(1,2)    
      GREAL(3,1)=GREAL(1,3)         
      GREAL(3,2)=GREAL(2,3)
      Call InverseMatrix(GREAL,GREC,3)
      XDD(1)=ZeroPoint
      DXDD(1)=0.01*abs(zeropoint)+0.001
!
      DO I=1,3
        AAS(I)=GREC(I,I)
      END DO
      AAS(4)=GREC(1,2)
      AAS(5)=GREC(1,3)
      AAS(6)=GREC(2,3)
      DO I=1,6
        AASLO(I)=0.9*AAS(I)
        AASHI(I)=1.1*AAS(I)
      END DO
!
!
      Do I=1,6
        IASS(I)=0
      End Do
      Do I=1,NVal
        Do II=1,3
          IASS(II)=IASS(II)+IHLR(II,I)**2
        End Do
        IASS(4)=IASS(4)+(IHLR(1,I)*IHLR(2,I))**2
        IASS(5)=IASS(5)+(IHLR(1,I)*IHLR(3,I))**2
        IASS(6)=IASS(6)+(IHLR(2,I)*IHLR(3,I))**2
      End Do
!
      XDD(2)=GREC(1,1)
!C>> JCC LatBrav is now 1 for unknown so we have to account for this here
      If (LatBrav.eq.1 .OR. LatBrav .eq. 2) Then
! Triclinic
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(1,2)
        XDD(6)=GREC(1,3)
        XDD(7)=GREC(2,3)
        NOCREF=.FALSE.
        DO I=1,6
          NOCREF=NOCREF.OR.(IASS(I).EQ.0)
        END DO
!C>> JCC And here
      Else If (LatBrav.eq.3) Then
! Monoclinic (a-axis)
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(2,3)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(6).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.4) Then
! Monoclinic (b-axis)
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(1,3)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(5).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.5) Then
! Monoclinic (c-axis)
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        XDD(5)=GREC(1,2)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0).OR.(IASS(4).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.6) Then
! Orthorhombic
        XDD(3)=GREC(2,2) 
        XDD(4)=GREC(3,3)
        NOCREF=(IASS(1).EQ.0).OR.(IASS(2).EQ.0).OR.(IASS(3).EQ.0)
!C>> JCC And here
      Else If (LatBrav.eq.7) Then
! Tetragonal
        XDD(3)=GREC(3,3)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
!C>> JCC And here
      Else If (LatBrav.eq.8) Then
! Trigonal
        XDD(3)=GREC(3,3)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
!C>> JCC And here
      Else If (LatBrav.eq.9) Then
! Rhombohedral
        XDD(2)=GREC(1,1)
        XDD(3)=GREC(1,2)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0).AND.(IASS(3).EQ.0)) & 
               .OR. ((IASS(4).EQ.0).AND.(IASS(5).EQ.0).AND.(IASS(6).EQ.0))
!C>> JCC And here
      Else If (LatBrav.eq.10) Then
! Hexagonal
        XDD(3)=GREC(3,3)
        NOCREF=((IASS(1).EQ.0).AND.(IASS(2).EQ.0)).OR.(IASS(3).EQ.0) 
!C>> JCC And here
      Else If (LatBrav.eq.11) Then
! Cubic
      End If
!
!	write(76,*) ' In refine lattice : ',NoCRef
      IF (NoCRef) Return
!	write(76,*) ' In refine lattice : ',NTPeak,NDD,LatBrav
      If (NVal.le.NDD) return
!
!>> JCC Does this fix the rhombnohedral problem?
!      MAXDD=0.
	  DDMAX=0.
      Do I=2,NDD
        DDMAX=MAX(DDMAX,1.e-4*ABS(XDD(I)))
      End Do
      Do I=2,NDD
        DXDD(I)=DDMAX
      End Do
!
!      write(76,*) ' Greal'
!      write(76,*) greal(1,1),greal(2,2),greal(3,3),greal(1,2),greal(1,3),greal(2,3) 
!      write(76,*) ' Grec'
!      write(76,*) grec(1,1),grec(2,2),grec(3,3),grec(1,2),grec(1,3),grec(2,3)
!	 write(76,*) ' XDD in ',(Xdd(II),ii=1,NDD)                         
!
!.. Perform simplex
      CALL SIMOPT(XDD,DXDD,COVDD,NDD,ChiGetLattice)
!
!      write(76,*) ' XDD out ',(Xdd(II),ii=1,NDD)
      XDD(9)=0.5*XDD(2)
      XDD(10)=0.
      DO I=1,3
!C>> JCC Account for LatBrav now being 1-11, but 2 as triclinic
        GREC(I,I)=XDD(KELPT(I, max(LatBrav - 1,1) ))
      END DO
      GREC(1,2)=XDD(KELPT(4, max(LatBrav - 1,1) ))
      GREC(1,3)=XDD(KELPT(5, max(LatBrav - 1,1) ))     
      GREC(2,3)=XDD(KELPT(6, max(LatBrav - 1,1) ))
      GREC(2,1)=GREC(1,2)    
      GREC(3,1)=GREC(1,3)         
      GREC(3,2)=GREC(2,3)
!      write(76,*) ' Grec out'
!      write(76,*) grec(1,1),grec(2,2),grec(3,3),grec(1,2),grec(1,3),grec(2,3)
      Call InverseMatrix(GREC,GREAL,3)
!      write(76,*) ' Greal out'
!      write(76,*) greal(1,1),greal(2,2),greal(3,3),greal(1,2),greal(1,3),greal(2,3)
!
      Do I=1,3
        CellPar(I)=Sqrt(max(0.,Greal(I,I)))
      End Do
!
      Cellpar(4)=acosd(GReal(2,3)/(CellPar(2)*CellPar(3)))  
      Cellpar(5)=acosd(GReal(1,3)/(CellPar(1)*CellPar(3)))            
      Cellpar(6)=acosd(GReal(1,2)/(CellPar(1)*CellPar(2)))
      ZeroPoint=XDD(1)
 999  continue
!     Write(76,*) 'Cell ',(CellPar(i),i=1,6),ZeroPoint
!
!
!C>> JCC      Call WDialogLoad(IDD_Crystal_Symmetry)
      Call WDialogSelect(IDD_Crystal_Symmetry)
      Call WDialogPutReal(IDF_a_latt,CellPar(1),'(F10.5)')
      Call WDialogPutReal(IDF_b_latt,CellPar(2),'(F10.5)')      
      Call WDialogPutReal(IDF_c_latt,CellPar(3),'(F10.5)')      
      Call WDialogPutReal(IDF_alp_latt,CellPar(4),'(F10.3)')      
      Call WDialogPutReal(IDF_bet_latt,CellPar(5),'(F10.3)')      
      Call WDialogPutReal(IDF_gam_latt,CellPar(6),'(F10.3)')
!
!C>> JCC      Call WDialogLoad(IDD_Peak_Positions)
      Call WDialogSelect(IDD_Peak_Positions)
      Call WDialogPutReal(IDF_a_refine,CellPar(1),'(F10.5)')
      Call WDialogPutReal(IDF_b_refine,CellPar(2),'(F10.5)')      
      Call WDialogPutReal(IDF_c_refine,CellPar(3),'(F10.5)')      
      Call WDialogPutReal(IDF_alp_refine,CellPar(4),'(F10.3)')      
      Call WDialogPutReal(IDF_bet_refine,CellPar(5),'(F10.3)')      
      Call WDialogPutReal(IDF_gam_refine,CellPar(6),'(F10.3)')
      Call WDialogPutReal(IDF_zeropt_refine,ZeroPoint,'(F10.4)')                      
!
!  First ensure that we have the plotting mode correct
      call IGrPlotMode(' ') 
      call Generate_TicMarks()
!
      If (NVal.le.NDD+2) return
!
!.. Now attempt a quick Pawley refinement

      call Quick_Pawley()                     
!
      EndSubroutine RefineLattice
!
!
!
      FUNCTION ChiGetLattice(N,P)
      PARAMETER (MPAR=50)
      REAL ChiGetLattice,P(MPAR)
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)

      COMMON /LATREFCMN/ LatBrav,IHLR(3,MVAL)
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
!
      COMMON /AASVAL/ AAS(6),AASLO(6),AASHI(6)
!
      ChiGetLattice=0.
!
      zp=p(1)
      p4=0.
      p5=0.
      p6=0.
!C>> JCC Changed to correct settings of LatBrav
      If (LatBrav.eq.2 .OR. LatBrav.eq.1) Then
! Triclinic or Unknown
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p4=p(5)
        p5=p(6)
        p6=p(7)
      Else If (LatBrav.eq.3) Then
! Monoclinic - a axis unique
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p6=p(5)
      Else If (LatBrav.eq.4) Then
! Monoclinic - b axis unique
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p5=p(5)
      Else If (LatBrav.eq.5) Then
! Monoclinic - c axis unique
        p1=p(2)
        p2=p(3)
        p3=p(4)
        p4=p(5)
      Else If (LatBrav.eq.6) Then
! Orthorhombic
        p1=p(2)
        p2=p(3)
        p3=p(4)
      Else If (LatBrav.eq.7) Then
! Tetragonal
        p1=p(2)
        p2=p(2)
        p3=p(3)
      Else If (LatBrav.eq.8) Then
! Trigonal
        p1=p(2)
        p2=p(2)
        p3=p(3)
!>> JCC Wrong parameter extracted        p6=0.5*p(2)
	    p4=0.5*p(2)
      Else If (LatBrav.eq.9) Then
! Rhombohedral
        p1=p(2)
        p2=p(2)
        p3=p(2)
        p4=p(3)
        p5=p(3)
        p6=p(3)
      Else If (LatBrav.eq.10) Then
! Hexagonal
        p1=p(2)
        p2=p(2)
        p3=p(3)
		p4=0.5*p(2)
!>> JCC Wrong parameter extracted        p6=0.5*p(2)
      Else If (LatBrav.eq.11) Then
! Cubic
        p1=p(2)
        p2=p(2)
        p3=p(2)
      End If
!
!      p1=min(aashi(1),p1)
!      p1=max(aaslo(1),p1)
!
!      p2=min(aashi(2),p2)
!      p2=max(aaslo(2),p2)
!
!      p3=min(aashi(3),p3)
!      p3=max(aaslo(3),p3)
!
!      p4=min(aashi(4),p4)
!      p4=max(aaslo(4),p4)
!
!      p5=min(aashi(5),p5)
!      p5=max(aaslo(5),p5)
!
!      p6=min(aashi(6),p6)
!      p6=max(aaslo(6),p6)
!      
      DO I=1,NVAL
        vh=IHLR(1,I)
        vk=IHLR(2,I)
        vl=IHLR(3,I)
        dd=vh*vh*p1+vk*vk*p2+vl*vl*p3+ 2.*(vh*vk*p4+vh*vl*p5+vk*vl*p6)
        tthc=2.*asind(0.5*alambda*sqrt(dd))
        ZI=tthc+zp
        CTem=(ZI-YVal(I))/EVal(I)
        ChiGetLattice=ChiGetLattice+CTem*CTem
      END DO
!
      RETURN
      END
