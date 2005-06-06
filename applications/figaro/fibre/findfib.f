      subroutine findfib(image,nl,ni,ichst,ixst,centre,nfound,widths)
*+
* Name:
*    FINDFIB

* Invocation:
*    CALL FINDFIB(IMAGE,NL,NI,ICHST,IXST,CENTRE,NFOUND,WIDTHS)

* Purpose:
*  To find fibre output in a 2-d sepctrum

* Description:
*  To find fibre output in a 2-d sepctrum
*
* Arguments:
*    IMAGE(NL,NI) = REAL ARRAY (Given)
*        The 2-d spectrum
*    NL,NI = INTEGER (Given)
*        Dimensions of IMAGE
*    ICHST = INTEGER (Given)
*        Lower X limit of display
*    IXST = INTEGER (Given)
*        Lower Y limit of display
*    NFOUND = INTEGER (Returned)
*        Number of fibres found
*    CENTRE(200) = REAL ARRAY (Returned)
*        Centres of fibres
*    WIDTHS(200) = REAL ARRAY (Returned)
*        Widths of fibres
*
*   T.N.Wilkins Manchester 13/7/88
*   TNW 29/11/88 Changed to use GETWORK
*-
      implicit none
      include 'SAE_PAR'
      include 'opt_cmn'
      include 'gr_inc'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer nl,ni,ixst,ichst,nfib,i,work1,work2,idel
      integer status,dirn,gen_pmax,nfound,slot,slot2
      real image(nl,ni),xl,yl,xu,yu,x(2),y(2),dummy,value,sep
      real yst,yen,sg_parms(6),sg_error(6),centre(200),flux(200)
      real xmark,widths(200),x1,x2,y1,y2
      integer gen_bsearch,iup,idown,pgcurse
      logical go,loop,censt,par_quest,termsav
      character*1 ch

      prfits = .true.
      steprat = 1.0

* Get user to mark end fibres

      call par_wruser('Mark 2 adjacent fibres',status)
      call sync
      if(pgcurse(xl,yl,ch).ne.1) return
      xu = xl
      yu = yl
      if(pgcurse(xu,yu,ch).ne.1) return
      sep = abs(yu-yl)

* Get regions in X of spectrum to consider

      call par_wruser('mark to either side (in x) of the part of the
     : spectrum to use',status)
      call sync
      if(pgcurse(x(1),y(1),ch).ne.1) return
      x(2) = x(1)
      y(2) = y(1)
      if(pgcurse(x(2),y(2),ch).ne.1) return
      if(x(2).lt.x(1)) then
        dummy = x(1)
        x(1) = x(2)
        x(2) = dummy
      end if
      call pgqwin(x1,x2,y1,y2)
      x(1) = max(x1,x(1))
      x(1) = min(x2,x(1))
      x(2) = max(x1,x(2))
      x(2) = min(x2,x(2))

* Show region selected

      call gr_spen(2)
      call gr_vline(x(1))
      call gr_vline(x(2))

      call dsa_get_work_array(ni,'float',work1,slot,status)
      call dsa_get_work_array(ni,'float',work2,slot2,status)
      if(status.ne.SAI__OK) return
      call gen_nfillf(ni,%VAL(CNF_PVAL(work2)))
      call fig_ytract(image,nl,ni,nint(x(1))+ichst,nint(x(2))+ichst,
     :                %VAL(CNF_PVAL(work1)))
      call par_rdval('NFIB',2.0,200.0,30.0,' ',value)
      nfib = nint(value)
      i = 0
      loop = .true.
      yl = real(gen_pmax(%VAL(CNF_PVAL(work1)),ni))
      xmark = (x(1)+x(2))*0.5
      iup = 0
      idown = 0

* Tell profile_fit that no terminal-type device is open, but save value
* of terminal so closed properly

      termsav = terminal
      terminal = .false.

* Set direction to go up spectrum

      dirn = 1
      do while(loop)
        censt = .true.
        go = .true.
        do while(go)
          i = i + 1
          if(i.eq.1) then
            yst = yl - sep*0.5
            yen = yl + sep*0.5
          else if(censt) then
            yst = centre(1)- sep*0.5
            yen = yst - sep
          else
            yst = centre(i-1) + sep*0.5*real(dirn)
            yen = yst + sep*real(dirn)
          end if
          if(yst.gt.yen) then
            dummy = yen
            yen = yst
            yst = dummy
          end if
          crash = .false.

*   Fit Gaussian to possible fibre

          call profile_fit(sg_parms,sg_error,ni,%VAL(CNF_PVAL(work2)),
     :                     %VAL(CNF_PVAL(work1)),nint(yst),
     :                     nint(yen-yst)+1,status)
          call opt_release(status)
          if(status.ne.SAI__OK) return
          sg_parms(2) = abs(sg_parms(2))
          if(((sg_parms(4).ge.yst).and.(sg_parms(4).le.yen)).and.
     :       (sg_parms(3).gt.0.0)) then
            centre(i) = sg_parms(4)
          else
            centre(i) = (yen+yst)*0.5
          end if
          widths(i) = sg_parms(2)
          flux(i) = sg_parms(2)*sg_parms(3)
          go = (i.lt.nfib+20).and.(flux(i).gt.0.0).and.
     :       (sg_parms(2).lt.sep).and.(centre(i).lt.real(ni))
     :       .and.(centre(i).gt.1.0).and.(sg_parms(2).gt.0.2).and.
     :       (.not.crash)
          if(go.and.(.not.censt).and.(i.gt.1)) then
            sep = abs(centre(i)-centre(i-1))
          end if
          censt = .false.
          if(dirn.eq.1) then
            iup = iup + 1
          else
            idown = idown + 1
          end if
          if(.not.go) then
            if(dirn.eq.1) then
              go = iup.le.nfib/10
            else
              go = (i.le.nfib*2/3).or.(idown.le.nfib/10)
            end if
          end if
          if(go) then

*   Mark position of centre

            call pgpoint(1,xmark,centre(i)+real(ixst-1),3)
          end if
        end do

* Set direction to go down spectrum

        loop = dirn.eq.1
        dirn = -1
        i = i-1
      end do
      terminal = termsav
      call dsa_free_workspace(slot,status)
      call dsa_free_workspace(slot2,status)
      nfound = i

* Sort by centre

      call arsort(centre,widths,nfound)

      call gr_spen(3)
      call par_wruser('Points used are in red, deleted points in green'
     :   ,status)
      do while(par_quest('Delete any points',.false.))
        call sync
        if(pgcurse(xl,yl,ch).ne.1) return
        yl = yl + real(ixst-1)
        if(yl.gt.centre(nfound)) then
          idel = nfound
        else if(yl.lt.centre(1)) then
          idel = 1
        else
          idel = gen_bsearch(centre,nfound,yl)
        end if
        nfound = nfound - 1
        call pgpoint(1,xmark,centre(idel)+real(ixst-1),3)
        do i = idel,nfound
          centre(i) = centre(i+1)
        end do
      end do
      end
