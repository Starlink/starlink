      subroutine clone_plot(in1,x1,y1,in2,x2,y2,chan1,chan2)
*+
* Name:
*    CLONE_PLOT

* Invocation:
*    CALL CLONE_PLOT(IN1,X1,Y1,IN2,X2,Y2,CHAN1,CHAN2)

* Purpose:
*   To get the positions of 3 lines in each of 2 spectra, so as to
*   enable the remaining lines to be located.

* Description:
*   To get the positions of 3 lines in each of 2 spectra, so as to
*   enable the remaining lines to be located. The positions of the lines
*   and their identifications are to be copied from the cloning data to
*   the current data, after allowing for any shifts etc.
*
* Arguments:
*     IN1 = INTEGER (Given)
*        X dimension of current data
*     X1(IN1) = REAL ARRAY (Given)
*        X data (current data)
*     Y1(IN1) = REAL ARRAY (Given)
*        Y data (current data)
*     IN2 = INTEGER (Given)
*        X dimension of cloning data
*     X2(IN2) = REAL ARRAY (Given)
*        X data (cloning data)
*     Y2(IN2) = REAL ARRAY (Given)
*        Y data (cloning data)
*     CHAN1(3) = DOUBLE PRECISION ARRAY (Returned)
*        Positions of lines on current data
*     CHAN2(3) = DOUBLE PRECISION ARRAY (Returned)
*        Positions of lines on cloning data
*
* History:
*  Altered to use GEN_MOVE TNW 5/12/88, changed to COPR2R 6/12/88
*  PGPLOT version 3/90 TNW/IOA
*  Output trams made double precision, TNW 8/7/91
*
      implicit none

* Import

      integer in1,in2
      real x1(in1),y1(in1)
      real x2(in2),y2(in2)

* Export

      double precision chan1(3),chan2(3)
*-
* ----------------------------------------------------------------------

* Local

      integer status
      integer line,len1,pgcurse,i
      character*30 chars
      logical go,plot,whole
      real y1max,y1min,y2max,y2min,x,y
      real xmint,xmaxt,xranget,xminl,xmaxl,xrangel
      real curxmint,curxmaxt,curxminl,curxmaxl

      call pgqinf('cursor',chars,len1)
      if(chars(:3).ne.'YES') then
        call par_wruser('No cursor available',status)
        return
      end if
      call par_wruser('B - blowup, W - whole',status)
      go = .true.
      plot = .true.
      whole = .true.
      xmint = x1(1)
      xmaxt = x1(in1)
      xranget = xmaxt - xmint
      xminl = x2(1)
      xmaxl = x2(in2)
      xrangel = xmaxl - xminl
      curxmint = xmint
      curxmaxt = xmaxt
      curxminl = xminl
      curxmaxl = xmaxl

      line = 0
      do while(go)

        if(plot) then

* plot, firstly the current data at the top

          call gr_spen(1)
          call pgvport(0.05,0.98,0.55,0.99)
          call gr_range(y1,1,in1,y1min,y1max,status)
          call pgwindow(curxmint,curxmaxt,y1min,y1max)
          call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
          call pgbin(in1,x1,y1,.true.)

* then the cloning data at the bottom

          call pgvport(0.05,0.98,0.05,0.49)
          call gr_range(y2,1,in2,y2min,y2max,status)
          call pgwindow(curxminl,curxmaxl,y2min,y2max)
          call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
          call pgbin(in2,x2,y2,.true.)

* Locate 3 lines (or other features) common to both plots

          call pgvport(0.05,0.98,0.05,0.99)
          call gr_spen(2)
          plot = .false.
          do i = 1, line
            call gr_vline(real(chan1(i)))
            call gr_vline(real(chan2(i)))
          enddo
        end if
        if(line.lt.3) then

* Locate line on upper plot (current data)

          write (chars,'(''Locate line'',i2,'' on upper plot'')')
     :            line+1
          call par_wruser(chars,status)
          call pgwindow(curxmint,curxmaxt,-1.0,1.0)
          call pgvport(0.05,0.98,0.05,0.99)
          call sync
          if(pgcurse(x,y,chars).ne.1) goto 500
          call chr_ucase(chars)
c         if(chars(1:1).eq.'E') then
c           go = .false.
c         else
          if(chars(1:1).eq.'B') then
            plot = .true.
            curxmint = max(xmint,(x-xranget*0.1))
            curxmaxt = min(xmaxt,(x+xranget*0.1))
            x = ((x-xmint)/xranget)*xrangel + xminl
            curxminl = max(xminl,(x-xrangel*0.1))
            curxmaxl = min(xmaxl,(x+xrangel*0.1))
            whole = .false.
            call pgpage
          else if(chars(1:1).eq.'W') then
            whole = .true.
            plot = .true.
            curxmint = xmint
            curxmaxt = xmaxt
            curxminl = xminl
            curxmaxl = xmaxl
            call pgpage
          end if
          if(.not.plot) then
            line = line + 1
            chan1(line) = dble(x)
            call pgvport(0.05,0.98,0.55,0.99)
            call gr_vline(x)

* Locate line on bottom plot (cloning data)

            write (chars,'(''Locate line'',i2,'' on lower plot'')') line
            call par_wruser(chars,status)
            call pgwindow(curxminl,curxmaxl,-1.0,1.0)
            call pgvport(0.05,0.98,0.05,0.99)
            call sync
            if(pgcurse(x,y,chars).ne.1) goto 500
            call pgvport(0.05,0.98,0.05,0.49)
            call gr_vline(x)
            chan2(line) = dble(x)
          end if
        end if

        if(line.eq.3) then
          go = .false.
          if(.not.whole) then
            plot = .true.
            go = .true.
            whole = .true.
            call pgpage
            curxmint = xmint
            curxmaxt = xmaxt
            curxminl = xminl
            curxmaxl = xmaxl
          end if
        end if
      end do
      call gr_spen(1)

* Set current transformation to current data-for clone_match

      call pgvport(0.05,0.98,0.55,0.99)
      call pgwindow(x1(1),x1(in1),y1min,y1max)
  500 continue
      end
