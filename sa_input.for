      subroutine sa_input(sa_file,nsa,IER)
      character*132 line
      character*80  sa_file
      logical   log_inf_file,log_nvar,log_bounds,log_reduce,
     &log_eps,log_ns,log_nt,log_neps,log_maxevl,log_iprint,
     &log_iseed1,log_iseed2,log_T0,log_target_value,
     &log_frag_file
      double precision cen,sig
      logical gaussb
      character*80  inf_file,zm_file
      double precision x,lb,ub,vm,xpreset
      double precision T0,rt,eps,target_value
      common /inffil/ lfinf,lfzm,inf_file,zm_file
      parameter (maxfrg=20)
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      parameter (mvar=100)
      common /gaubou/ cen(mvar),sig(mvar)
      common /gaulog/ gaussb(mvar)
      character*80  torfile
      logical ltorfil
      common /torfcm/ torfile(mvar)
      common /torlog/ ltorfil(mvar)
      common /jitter/ rjittr
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
      logical log_hydrogens
      common /hydrogen/ log_hydrogens
c
      common /saparl/ T0,rt,eps,target_value
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
c
      open(10,file=sa_file(:nsa),status='old')!,err=2)
c
      IER=0
      inf_file=' '
      do ifrg=1,maxfrg
         frag_file(ifrg)=' '
      end do
      nfrag=0
      do i=1,mvar
        log_shad(i)=.false.
      end do
      log_inf_file=.false.
      log_frag_file=.false.
      log_nvar=.false.
      log_bounds=.false.
      log_preset=.false.
      log_T0=.false.
      log_target_value=.false.
      log_reduce=.false.
      log_eps=.false.
      log_ns=.false.
      log_nt=.false.
      log_neps=.false.
      log_maxevl=.false.
      log_iprint=.false.
      log_iseed1=.false.
      log_iseed2=.false.
      log_hydrogens=.false.
c
 10   line=' '
      read(10,1010,end=100,ERR=998) line
 1010 format(a)
      i=132
      do i=132,1,-1
        if (line(i:i).ne.' ') then
          ie=i
          goto 12
        end if
      end do
c.. blank line 
      goto 10
 12   ix=index(line,'!')-1
      if (ix.eq.-1) ix=ie
      if (index(line(1:ix),'NVAR').ne.0) then
        ii=index(line(1:ix),'NVAR')
        read(line(ii+4:ix),*,err=10) nvar
        log_nvar=.true.
      else if (index(line(1:ix),'SHADOWS').ne.0) then
        ii=index(line(1:ix),'SHADOWS')
        read(line(1:ii-1),*,err=10) ishad
        read(line(ii+7:ix),*,err=10) ktem
        kshad(ishad)=ktem
        log_shad(ishad)=.true.
      else if (index(line(1:ix),'BOUNDS').ne.0) then
        ii=index(line(1:ix),'BOUNDS')
        if (log_nvar) then
          do iv=1,nvar
c            read(10,*,err=999) lb(iv),ub(iv),vm(iv)
c          end do
c          log_bounds=.true.
            line=' '
            read(10,1010,end=100) line
            ig=index(line(1:132),'G')
            itorf=index(line(1:132),'Torsion file')
            gaussb(iv)=(ig.ne.0)
            ltorfil(iv)=(itorf.ne.0)
            if (ltorfil(iv)) gaussb(iv)=.false.
            if (ltorfil(iv)) then
              read(line(1:itorf-1),*,err=999) lb(iv),ub(iv),vm(iv)
              torfile(iv)=' '
              read(line(itorf+12:132),5020,err=999) torfile(iv)
 5020         format(a)
ccc	write(6,*) iv,torfile(iv)(1:60)
            else if (gaussb(iv)) then
              read(line(1:ig-1),*,err=999) lb(iv),ub(iv),vm(iv)
              read(line(ig+1:132),*,err=999) cen(iv),sig(iv)
            else
              read(line(1:132),*,err=999) lb(iv),ub(iv),vm(iv)
            end if
ccc            write(6,*) lb(iv),ub(iv),vm(iv),cen(iv),sig(iv)
          end do
          log_bounds=.true.
        else
c.. we need to know the number of variables before we can read the bounds
          goto 999
        end if
      else if (index(line(1:ix),'PRESET').ne.0) then
        ii=index(line(1:ix),'PRESET')
        if (log_nvar) then
          do iv=1,nvar
            read(10,*,err=999) xpreset(iv)
          end do
          log_preset=.true.
        else
c.. we need to know the number of variables before we can read the preset values
          goto 999
        end if
      else if (index(line(1:ix),'INF').ne.0) then
        ii=index(line(1:ix),'INF')
        do jj=ii+3,ix
          if (line(jj:jj).ne.' ') then
            kk=1
            inf_file(kk:kk)=line(jj:jj)
            do k=jj+1,ix
              if (line(k:k).ne.' ') then
                kk=kk+1
                inf_file(kk:kk)=line(k:k)
                lfinf=kk
              else
                goto 14
              end if
            end do
            goto 14
          end if
        end do
 14     log_inf_file=.true.
        inf_file(lfinf+1:lfinf+4)='.inf'
        lfinf=lfinf+4
c        zm_file(:lfinf)=inf_file(:lfinf)
c        zm_file(lfinf+1:lfinf+8)='.zmatrix'
c        lfzm=lfinf+8
      else if (index(line(1:ix),'FRAG').ne.0) then
        nfrag=nfrag+1
        ii=index(line(1:ix),'FRAG')
        do jj=ii+4,ix
          if (line(jj:jj).ne.' ') then
            kk=1
            frag_file(nfrag)(kk:kk)=line(jj:jj)
            do k=jj+1,ix
              if (line(k:k).ne.' ') then
                kk=kk+1
                frag_file(nfrag)(kk:kk)=line(k:k)
                lfrag(nfrag)=kk
              else
                goto 141
              end if
            end do
            goto 141
          end if
        end do
 141    log_frag_file=.true.
        ltem=lfrag(nfrag)+1
        frag_file(nfrag)(ltem:ltem+7)='.zmatrix'
        lfrag(nfrag)=lfrag(nfrag)+8
      else if (index(line(1:ix),'T0').ne.0) then
        ii=index(line(1:ix),'T0')
        read(line(ii+2:ix),*,err=10) T0
        log_T0=.true.
      else if (index(line(1:ix),'TARGET').ne.0) then
        ii=index(line(1:ix),'TARGET')
        read(line(ii+6:ix),*,err=10) target_value
        log_target_value=.true.
      else if (index(line(1:ix),'REDUCE').ne.0) then
        ii=index(line(1:ix),'REDUCE')
        read(line(ii+6:ix),*,err=10) rt
        log_reduce=.true.
c.. hydrogens
      else if (index(line(1:ix),'HYDROGENS').ne.0) then
        log_hydrogens=.true.
      else if (index(line(1:ix),'EPSI').ne.0) then
        ii=index(line(1:ix),'EPSI')
        read(line(ii+4:ix),*,err=10) eps
        log_eps=.true.
      else if (index(line(1:ix),'#NS').ne.0) then
        ii=index(line(1:ix),'#NS')
        read(line(ii+3:ix),*,err=10) ns
        log_ns=.true.
      else if (index(line(1:ix),'#NT').ne.0) then
        ii=index(line(1:ix),'#NT')
        read(line(ii+3:ix),*,err=10) nt
        log_nt=.true.
      else if (index(line(1:ix),'NEPS').ne.0) then
        ii=index(line(1:ix),'NEPS')
        read(line(ii+4:ix),*,err=10) neps
        log_neps=.true.
      else if (index(line(1:ix),'MAXEVL').ne.0) then
        ii=index(line(1:ix),'MAXEVL')
        read(line(ii+6:ix),*,err=10) maxevl
        log_maxevl=.true.
      else if (index(line(1:ix),'IPRINT').ne.0) then
        ii=index(line(1:ix),'IPRINT')
        read(line(ii+6:ix),*,err=10) iprint
        log_iprint=.true.
      else if (index(line(1:ix),'ISEED1').ne.0) then
        ii=index(line(1:ix),'ISEED1')
        read(line(ii+6:ix),*,err=10) iseed1
        log_iseed1=.true.
      else if (index(line(1:ix),'ISEED2').ne.0) then
        ii=index(line(1:ix),'ISEED2')
        read(line(ii+6:ix),*,err=10) iseed2
        log_iseed2=.true.
      end if
      goto 10
c
c.. check if parameters have been specified - if not use defaults.
 100  if (.not.log_inf_file) goto 999
      if (.not.log_frag_file) goto 999
      if (.not.log_nvar) goto 999
      if (.not.log_bounds) goto 999
      if (.not.log_T0) T0=200.0
      if (.not.log_target_value) target_value=20.0
      if (.not.log_reduce) rt=0.1
      if (.not.log_eps) eps=0.1
      if (.not.log_ns) ns=20
      if (.not.log_nt) nt=min(100,5*nvar)
      if (.not.log_neps) neps=4
      if (.not.log_maxevl) maxevl=1000000
      if (.not.log_iprint) iprint=0
      if (.not.log_iseed1) iseed1=401
      if (.not.log_iseed2) iseed2=101
c
      GOTO 999
 998  IER=1
      CALL ERROR_MESSAGE(2)
 999  close(10)
      end