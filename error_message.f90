!
	SUBROUTINE CHKMAXREF(PCXX)
! Checks if the maximum number of reflections have been exceeded
!
    USE WINTERACTER
	external PCXX
	INCLUDE 'REFLNS.INC'
      PARAMETER (MPPTS=15000,MKPTS=150000)
      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),&
     ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /PRPKCN/ARGK,PKCNSP(6,9,5),&
      KPCNSP(6,9,5),DTDPCN(6),DTDWL,&
      NPKCSP(9,5),ARGMIN(5),ARGMAX(5),&
      ARGSTP(5),PCON
	  integer iorda(10)
	  real ardi(10)
	  common /mxrfcm/ aadd

	  logical routine_called
      save routine_called
	  data routine_called / .false. /
!
	aadd=0.
	if (maxk.gt.360) then
!.. We've too many reflections ... must reduce
       if (.not. routine_called) then
         CALL WMessageBox(OKOnly,InformationIcon,CommonOK,  &
         'DASH has a maximium limit of 350 reflections.'//&
		 'Only the 350 lowest angle reflections will be indexed and used','File truncation')
		 routine_called =.true.
	   endif
	  know=350
	  call pcxx(2)
	  arrt=argk
	  do ii=1,1
	    know=350+ii
		call PCXX(2)
		ardi(ii)=argk-arrt
	    arrt=argk
	  end do
	  call sortx(ardi,iorda,10)
	  item=iorda(10)
	  maxk=349+item
	  aadd=ardi(10)
	end if
!
	  know=maxk
! Calculate peak centre in argk, and its derivatives
	  call pcxx(2)
	  armx=argk+aadd
	  ii=1
  	  do while ((zargi(ii) .LT. armx) .AND. (ii .LE. MPPTS))
	    ii=ii+1
	  end do
	  npts=min(npts,ii)
	  if (aadd.ne.0.0) argmax(1)=armx
!
	END