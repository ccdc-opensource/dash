      SUBROUTINE PutSimplexChisq(CCHI)
!
      USE WINTERACTER
      USE DRUID_HEADER
!
      character*7 chistr
!
      common /chitemcmn/ ncchi,ncchimin,cchimin
!
!
      cchimin=min(cchimin,cchi)
!
      if (cchi.eq.cchimin) then
        chistr(1:7)='0000.00'
        write(chistr(1:7),'(f7.2)') cchimin
!
        CALL WDialogSelect(IDD_SA_Action1)
        CALL WDialogPutString(IDF_Simplex_Now,chistr)
        call valchipro(cpb)
      end if
!
!
	 END