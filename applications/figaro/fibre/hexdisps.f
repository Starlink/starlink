      subroutine hexdisps(data,nw,miss,plane,xadj,xa,ya,size,xr,
     :                  yr,limit,disp,chkstat,fitstat)
*+
* Name:
*    HEXDISPS

* Invocation:
*    CALL HEXDISPS(DATA,NW,MISS,PLANE,XADJ,XA,YA,SIZE,XR,
*                       YR,LIMIT,DISP,CHKSTAT,FITSTAT)

* Purpose:
*  To display an image with hexagonal pixels, from a sorted cube
*
* Description:
*  To display an image with hexagonal pixels, from a sorted cube
*
* Arguments:
*   DATA(NW,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        The data
*   NW,SPDIM1,SPDIM2 = INTEGER (Given)
*        Dimensions of above
*   PLANE = INTEGER (Given)
*        Plane to display
*   XADJ(SPDIM2) = REAL ARRAY (Given)
*        X position adjustment
*   XA(SPDIM1) = REAL ARRAY (Given)
*        X (spatial) array
*   YA(SPDIM2) = REAL ARRAY (Given)
*        Y (spatial) array
*   XR(2) = REAL ARRAY (Given)
*        X range for display (if limit true)
*   YR(2) = REAL ARRAY (Given)
*        Y range for display (if limit true)
*   LIMIT = LOGICAL (Given)
*        If to limit range for display
*   CHKSTAT = LOGICAL (Given)
*        If to check status word before displaying
*   FITSTAT(3,NX<SPDIM2) = INTEGER ARRAY (Given)
*        Fit status array
*   DISP = LOGICAL (Given and returned)
*        If image etc. displayed
*   SIZE = REAL (Returned)
*        Size of hexagons
*   MISS(SPDIM1,SPDIM2) = INTEGER ARRAY (Workspace)
*        Missing values work array
*
* Functions/subroutines referenced:
*     COLRAIN           : Set colour tale to rainbow
*     GET_GREY          : Open greyscale device
*     HEXMARK           : Draw hexagon
*     SETGRY            : Set colour table to greyscale
*     TNW_AGREST        : Load default state of NCAR
*
*     GEN_RANGEF        : Get range of array
*     GQCF              : Inquire colour facilities of device
*     GQWKCA            : Inquire device category
*     GSCR              : Set colour representation
*     GSFAR             : Set fill area representaion
*     SGS_FLUSH         : Flush SGS/GKS

* History:
*   T.N.Wilkins Manchester
*   Altered to use gen_rangef TNW 6/7/88
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'arc_dims'
      include 'status_inc'
      integer nw,plane,i,status,j,nlevs,len1
      integer ind,start,cstat,minlev,pstat,ndata
      integer fitstat(ncntrl,spdim1,spdim2)
      integer*2 miss(spdim1,spdim2)
      real data(nw,spdim1,spdim2),xadj(spdim2),xa(spdim1),ya(spdim2)
     :     ,x,y,datmin,datmax
      real xmin,xmax,ymin,ymax,xdisp,size,ydisp
      real scale,offset,xr(2),yr(2),value,xx(4),yy(4)
      logical par_quest,limit,disp,chkstat,plotthis
      character*3 string
      logical shownag

      if(chkstat) then
        shownag = par_quest('Show fits with Nag errors?',.false.)
      end if
      status = SAI__OK
      call get_grey('device',status)
      if(status.ne.SAI__OK) return
      disp = .true.

* Scale data, also flag data points to be displayed by setting missing
* values array elements to 1. Otherwise these are 0

      call zero_short(miss,spdim1*spdim2)
      datmax = VAL__MINR
      datmin = VAL__MAXR
      ndata = 0
      do j = 1,spdim2
        y = ya(j)
        do i = 1, spdim1
          x = xa(i)+xadj(j)
          if((.not.limit).or.((x.ge.xr(1)).and.(x.le.xr(2))
     :           .and.(y.ge.yr(1)).and.(y.le.yr(2)))) then
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
                miss(i,j) = 1
              end if
            end if
          end if
        end do
      end do
      if(ndata.eq.0) then
        call par_wruser('No data to plot',pstat)
        return
      end if

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
      call pgqinf('hardcopy',string,len1)
      if(string(:2).eq.'NO') then
        nlevs = nlevs-4
        start = 4
      else
        nlevs = nlevs
        start = 0
      end if
      offset = datmin
      scale = real(nlevs-1)/(datmax-datmin)

* Get range of X

      xdisp = (xa(spdim1)-xa(1))/real(spdim1-1)

* If to limit range of display then set limits

      if(limit) then
        xmin = xr(1)
        xmax = xr(2)
        ymax = yr(1)
        ymax = yr(2)
      else
        call gen_rangef(xadj,1,spdim2,xmax,xmin)
        xmin = xmin + xa(1)
        xmax = xmax + xa(spdim1)
        ymin = ya(1)
        ymax = ya(spdim2)
      end if

* Allow for half pixels at ends

      xmax = xmax + xdisp*0.5
      xmin = xmin - xdisp*0.5
      ymax = ymax + xdisp*0.57735
      ymin = ymin - xdisp*0.57735

* Set up X range

      size = xdisp*0.57735
      call pgvport(0.05,0.92,0.07,0.98)
      call pgwnad(xmin,xmax,ymin,ymax)
      call pgbox('BCN',0.0,0,'BCN',0.0,0)

      if(par_quest('Use colour look-up table?',.false.)) then

*    Set colour table to rainbow with pen 0 black

        call pgscr(0,0.0,0.0,0.0)
        call colrain(start,nlevs)
      else

*    Set colour table to grey, but with pen 0 set to dark blue

        call setgry(start,nlevs+start-1)
        call pgscr(0,0.0,0.0,0.2)
      end if
      call pgsfs(1)
      do j=1,spdim2
        y = ya(j)
        do i=1,spdim1
          if(miss(i,j).eq.1) then
            x = xa(i)+xadj(j)
            value = data(plane,i,j)
            ind = start+nint((value-offset)*scale)

*       This might work!

            call pgsci(ind)
            call hexmark(x,y,size)
          end if
        end do
      end do
      call pgvport(0.95,1.0,0.07,0.98)
      ydisp = (datmax-datmin)/511.0
      ymin = datmin - ydisp*0.5
      ymax = datmax + ydisp*0.5
      call pgwindow(0.0,1.0,ymin,ymax)
      call gr_spen(1)
      xx(1) = 0.0
      xx(2) = 1.0
      xx(3) = 1.0
      xx(4) = 0.0
      do i = 1, 512
        value = datmin + ydisp*real(i-1)
        ind = start+nint((value-offset)*scale)

*       This might work!

        call pgsci(ind)
        yy(1) = value - ydisp*0.5
        yy(2) = yy(1)
        yy(3) = yy(1) + ydisp
        yy(4) = yy(3)
        call pgpoly(4,xx,yy)
      enddo
      call gr_spen(1)
      call pgbox('BC',0.0,0,'BCN',0.0,0)
      end
