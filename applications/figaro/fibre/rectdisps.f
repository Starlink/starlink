      subroutine rectdisps(data,nw,xa,ya,work,plane,xr,yr,limit,
     :          disp,chkstat,fitstat)
*+
* Name:
*    RECTDISPS

* Invocation:
*    CALL RECTDISPS(DATA,NW,XA,YA,WORK,PLANE,XR,YR,LIMIT,
*               DISP,CHKSTAT,FITSTAT)

* Purpose:
*  To display a plane from a rectangular fibre cube (sorted data).
*
* Description:
*  To display a plane from a rectangular fibre cube (sorted data).
*
* Arguments:
*     NW = INTEGER (Given)
*        Wavelength dimension of array
*     NX = INTEGER (Given)
*        X dimension of array
*     SPDIM2 = INTEGER (Given)
*        Y dimension of array
*     DATA(NW,NX,SPDIM2) = REAL ARRAY (Given)
*        Intensity array
*     XA(NX) = REAL ARRAY (Given)
*        X array
*     YA(SPDIM2) = REAL ARRAY (Given)
*        Y array
*     XR(2) = REAL ARRAY (Given)
*        X limits for display (if limit=true)
*     YR(2) = REAL ARRAY (Given)
*        Y limits for display (if limit=true)
*     LIMIT = LOGICAL (Given)
*        If to limit display
*     FITSTAT(3,NX<SPDIM2) = INTEGER ARRAY (Given)
*        Fit status array
*     DISP = LOGICAL (Given and returned)
*        If image etx. displayed
*     WORK(SPDIM1,SPDIM2) = REAL ARRAY (Workspace)
*
*   Subroutine/functions referenced:
*     COLRAIN           : Set colour tale to rainbow
*     GET_GREY          : Open greyscale device
*     RX2CHN = INTEGER (Workspace)
*        Convert array value to array index
*     SETGRY            : Set colour table to greyscale
*
*     GEN_RANGEF        : Get range of array

* History:
*   T.N.Wilkins Manchester
*-
      implicit none
      include 'PRM_PAR'
      include 'SAE_PAR'
      include 'arc_dims'
      include 'status_inc'
      integer nw
      real work(spdim1,spdim2)
      integer plane,i,j,k
      real data(nw,spdim1,spdim2),xa(spdim1),ya(spdim2),datmin,datmax
      real xdisp,ydisp,xmin,xmax,ymin,ymax
      real xr(2),yr(2)
      logical par_quest,limit,disp,chkstat,plotthis
      logical shownag
      integer nlevs,status,start,ndata,pstat,nwork
      integer minlev,fitstat(ncntrl,spdim1,spdim2)
      integer xst,xen,yst,yen,rx2chn,cstat
      real tr(6)
      real value

      tr(1) = 0.0
      tr(2) = 1.0
      tr(3) = 0.0
      tr(4) = 0.0
      tr(5) = 0.0
      tr(6) = 1.0

      if(chkstat) then
        shownag = par_quest('Show fits with Nag errors?',.false.)
      end if

* Scale data and copy into 2-d array, putting missing values to 0

      status = SAI__OK
      call get_grey('device',status)
      if(status.ne.SAI__OK) return
      disp = .true.

* Scale data

      datmax = VAL__MINR
      datmin = VAL__MAXR
      if(limit) then
        xst = rx2chn(xa,spdim1,xr(1))
        xen = rx2chn(xa,spdim1,xr(2))
        yst = rx2chn(ya,spdim2,yr(1))
        yen = rx2chn(ya,spdim2,yr(2))
      else
        xst = 1
        xen = spdim1
        yst = 1
        yen = spdim2
      end if
      ndata = 0
      do j = yst,yen
        do i = xst, xen
          value = data(plane,i,j)
          if(value.ne.VAL__BADR) then
            if(chkstat) then
              call decode_status(ncntrl,fitstat(1,i,j),deccntr)
              cstat = deccntr(FIT_STAT)
              plotthis = (cstat.eq.1).or.((cstat.eq.2).and.shownag)
            else
              plotthis = .true.
            end if
            if(plotthis) then
              datmax = max(datmax,value)
              datmin = min(datmin,value)
              ndata = ndata + 1
              work(i,j) = value
            end if
          else
            work(i,j) = -1.0e37
          end if
        end do
      end do
      if(ndata.eq.0) then
        call par_wruser('No data to plot',pstat)
        return
      end if

*  Set missing values to minimum

      do j = yst,yen
        do i = xst, xen
          if(work(i,j).lt.-9.0e36) work(i,j) = datmin
        end do
      end do

* Get range for scaling, first setting the default to the limit of the
* range

      call par_sdval('low',datmin,status)
      call par_rdval('low',datmin,datmax,datmin,' ',value)
      call par_sdval('high',datmax,status)
      call par_rdval('high',datmin,datmax,datmax,' ',datmax)
      call par_cnpar('low')
      call par_cnpar('high')
      datmin = value

* Inquire how maspdim2 grey levels available

      call pgqcol(minlev,nlevs)

      xdisp = (xa(spdim1)-xa(1))/real(spdim1-1)
      ydisp = (ya(spdim2)-ya(1))/real(spdim2-1)

      ymax = ya(yen)
      ymin = ya(yst)
      xmin = xa(xst)
      xmax = xa(xen)

* Allow for half pixels at ends

      xmax = xmax + xdisp*0.5
      xmin = xmin - xdisp*0.5
      ymax = ymax + ydisp*0.5
      ymin = ymin - ydisp*0.5

* Set up X range

      call pgvport(0.05,0.92,0.07,0.98)
      call pgwnad(real(xst)-0.5,real(xen)+0.5,real(yst)-0.5,
     :    real(yen)+0.5)

      call pggray(work,spdim1,spdim2,xst,xen,yst,yen,datmax,datmin,tr)

      call pgbox('BCN',0.0,0,'BCN',0.0,0)
      call pgvport(0.95,1.0,0.07,0.98)

      tr(1) = -0.5
      tr(2) = 1.0
      tr(3) = 0.0
      tr(5) = 0.0
      nwork = min(spdim1*spdim2,1024)
      tr(6) = (datmax-datmin)/real((nwork/2)-1)
      tr(4) = datmin - tr(6)*1.5
      ymin = tr(4) + tr(6)
      ymax = datmax + tr(6)*0.5
      call pgwindow(0.0,1.0,ymin,ymax)
      call gr_spen(1)
      call pgbox('BC',0.0,0,'BCN',0.0,0)
      i = 0
      j = 1
      do k = 1, nwork
        i = i + 1
        if(i.gt.spdim1) then
          i = 1
          j = j + 1
        endif
        work(i,j) = tr(4) + real((k+1)/2)*tr(6)
      enddo
      call pggray(work,2,nwork/2,1,2,1,nwork/2,datmax,datmin,tr)
      start = 16
      if(par_quest('Use colour look-up table?',.false.)) then

*    Set colour table to rainbow with pen 0 black

        call colrain(start,nlevs)
        call pgscr(0,0.0,0.0,0.0)
      else

*    Set colour table to grey, but with pen 0 set to dark blue

        call setgry(start,nlevs)
        call pgscr(0,0.0,0.0,0.2)
      end if
      call gr_spen(1)
      end
