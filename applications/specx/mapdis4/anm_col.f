*  History:
*     19 Mar 1992 (jfl):
*        Modified to use SXGQCOL 19th March 1992 REVAD::JFL
*     17 Nov 1992 (hme):
*        Use uppercase for include file specs.
*     24 Jun 1998 (bly):
*        Modified ANMSCR to cater for 1-256 cols in GKS/PGPLOT
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------
      subroutine wrap(r,g,b)

* wrap up a colour table entry WRAP times

      implicit none

      include 'ANM_COL.INC'
      real*4   r,g,b

      r=amod(nwrap*r,1.0)
      g=amod(nwrap*g,1.0)
      b=amod(nwrap*b,1.0)

*     print *, '-- wrap -- (r,g,b) = ', r, g, b

      end
*----6-----------------------------------------------------------------*-------
      subroutine setcol5 (c5start, c5rotate, c5exp)

      implicit  none
      real*4    c5start, c5rotate, c5exp

      real*4 start,rotation,exponent
      common/colfive/start,rotation,exponent

      start    = c5start
      rotation = c5rotate
      exponent = c5exp

      end
*----6-----------------------------------------------------------------*-------
      subroutine toggle_log (xmin, xmax)

      implicit none

      real*4   xmin, xmax

      include 'ANM_LOGCOL.INC'

      vmin   =  xmin
      vmax   =  xmax
      logcol = .not. logcol

*     print *, '"logcol" flag = ', logcol

      end
*----6-----------------------------------------------------------------*-------
      subroutine colmaxmin

      implicit none
      include 'ANM_COL.INC'

      integer jdef

      call gen_geti4a('Min and max colours to address:',
     &    colmin,2,'2i5',colmin,jdef)

      end
*----6-----------------------------------------------------------------*-------
      subroutine anmscr(i,r,g,b)

      implicit none
      include 'ANM_COL.INC'

      integer i
      real*4    r,g,b

*     print *, '-- anmscr -- i,r,g,b = ', i, r, g, b
*     if (i.le.255 .and. i.ge.0) then
      if (i.le.256 .and. i.ge.0) then
        col_red(i)=r
        col_green(i)=g
        col_blue(i)=b
      else
        write(*,*) 'Illegal parameter passed to ANMSCR',i
      endif

      end
*----6-----------------------------------------------------------------*-------
      subroutine set_colours

      implicit none
      include 'ANM_COL.INC'
      include 'FLAGCOMM'

      integer i

      call sxgqcol (colmin, colmax)
*     print *, '-- set_colours --- min and max colours...',
*    &           colmin, colmax

      call coltab(col_num)

      do i=colmin,colmax
       call sxgscr(i,col_red(i),col_green(i),col_blue(i))
      end do

      end
*----6-----------------------------------------------------------------*-------
      subroutine colour_plot(i, ifail)

      implicit none

      integer i
      integer ifail

      include 'ANM_COL.INC'
      include 'ANM_LOGCOL.INC'

      logcol  = .false.

      if (i.le.5 .and.i.ge.1) then
        col_num = i
        ifail   = 0

      else
        write(*,*) 'No such colour table defined'
        ifail = 21

      end if


      end
*----6-----------------------------------------------------------------*-------
      subroutine coltab(i)

      include 'ANM_COL.INC'
      include 'FLAGCOMM'

      call sxgqcol (colmin, colmax)
*     print *, '-- coltab --  setting colours ', colmin, ' to ', colmax

      if (i.eq.1) call colour1(colmin,colmax)
      if (i.eq.2) call colour2(colmin,colmax)
      if (i.eq.3) call colour3(colmin,colmax)
      if (i.eq.4) call colour4(colmin,colmax)
      if (i.eq.5) call colour5(colmin,colmax)

      end
*----6-----------------------------------------------------------------*-------
      subroutine colour1(colmin,colmax)

      implicit none

      real*4    r,g,b,x
      integer colmin,colmax,i

      include 'ANM_LOGCOL.INC'

      write(*,*) 'Setting colour table 1: linear grey scale'
      if (logcol) then
        write (*,*) 'Using log scaling...'
        write (*,*) 'min and max levels in = ', vmin, vmax
      end if

      do i=colmin,colmax

          x = real(i-colmin)/real(colmax-colmin)

          if (logcol .and. vmax.gt.vref) then
            x = vmin + x*(vmax-vmin)
            x = (alog(max(x,vref))-alog(max(vmin,vref)))
     &           / (alog(max(vmax,vref))-alog(max(vmin,vref)))
          end if

          if (x.gt.0.999) x = 0.999

          R = x
          G = x
          B = x

*         call wrap(r,g,b)
          call anmscr(i,r,g,b)
      end do

      end
*----6-----------------------------------------------------------------*-------
      subroutine colour2(colmin,colmax)

      implicit none

      real*4    r,g,b

      integer colmin,colmax,nstep,jdef,istep,isize,icol,istart,iend
      data nstep/9/

      write(*,*) 'Setting colour table 2: colour contours '

      call gen_geti4('Number of steps:',nstep,'i3',nstep,jdef)

      isize=(colmax-colmin)/nstep

      do istep = 1,nstep

       istart = colmin + ((istep-1)*isize)
       iend =  istart+isize

       istart = min(istart,colmax)
       iend   = min(iend,colmax)

       call get_colour(istep,r,g,b)

          do icol= istart,iend
             call anmscr(icol,r,g,b)
          end do

      end do

      end
*----6-----------------------------------------------------------------*-------
      subroutine colour3(colmin,colmax)

      implicit none

      integer colmin,colmax,jdef,nstep,i
      data nstep/10/

      real*4    r,g,b,x,z
      data z/1.0/

      write(*,*) 'Setting colour table 3: grey scale power law '

      call gen_getr4(' Power for variable x:',z,'1pe10.2',z,jdef)

      do i = colmin,colmax

      x = real(i-colmin)/real(colmax-colmin)
      if (x.gt.0.999) x = 0.999
      x = x**z

       r=x
       g=x
       b=x

       call wrap(r,g,b)
       call anmscr(i,r,g,b)

      end do

      end
*----6-----------------------------------------------------------------*-------
      subroutine colour4(colmin,colmax)

      implicit none

      include 'ANM_LOGCOL.INC'

      integer colmin,colmax,i
      real*4    r,g,b,x

      write(*,*) 'Setting colour table 4: blue to yellow'

      do i=colmin,colmax
        x = real(i-colmin)/real(colmax-colmin)

        if (logcol .and. vmax.gt.vref) then
          x = vmin + x*(vmax-vmin)
          x = (alog(max(x,vref))-alog(max(vmin,vref)))
     &         / (alog(max(vmax,vref))-alog(max(vmin,vref)))
        end if

        if (x.gt.0.999) x = 0.999
        r=x
        g=x*x
        b=0.35 * (1.-x)**2.
        call wrap(r,g,b)
        call anmscr(i,r,g,b)
      end do
      end

*-----------------------------------------------------------------------
* Modified to take into account colmin, colmax 17th March (REVAD::JFL)

      subroutine colour5 (colmin, colmax)

      integer colmin, colmax

      include 'ANM_LOGCOL.INC'

      real*4 start,rotation,exponent
      common/colfive/start,rotation,exponent

      call spiral(start,  rotation, exponent,
     &            logcol, vmin,     vmax,
     &            vref,   colmin,   colmax)

      return
      end

C-----------------------------------------------------------------------

      subroutine spiral (start,  rotation, exponent,
     &                   logcol, vmin,     vmax,
     &                   vref,   colmin,   colmax)

      implicit  none

      real      start,  rotation, exponent
      logical   logcol
      real      vmin,   vmax
      real      vref
      integer   colmin, colmax

      integer   i, iz
      real      r, g, b
      real      f, t, x, z
      real      rexponent
      real      crotations
      real      rtable(3)

      if (exponent.ne.0.0) then
        rexponent  = 1.0/exponent
      else
        rexponent  = 1.0
      end if

      crotations = 3.0*rotation

      do i = colmin, colmax

        t = real(i+1-colmin)/real(colmax+1-colmin)

        if (logcol .and. vmax.gt.vref) then
          t = vmin + t*(vmax-vmin)
          t = (alog(max(t,vref))-alog(max(vmin,vref)))
     &          / (alog(max(vmax,vref))-alog(max(vmin,vref)))
        end if

        x  = t**rexponent
        z  = start + crotations*x

C Get Z in range 1 <= Z < 4

        do while (z.le.1.)
         z = z + 3.
        end do

        z  = mod ((z-1.),3.) + 1.
        iz = z
        f  = z - float(iz)

        rtable(iz)            = x + 3.*(1.-x)*(1.-f)
        rtable(mod(iz,  3)+1) = x + 3.*(1.-x)*f
        rtable(mod(iz+1,3)+1) = x

C Now put in intensity variation

        x = t**exponent

        r = max (min (rtable(1)*x,1.0 ),0.0)
        g = max (min (rtable(2)*x,1.0 ),0.0)
        b = max (min (rtable(3)*x,1.0 ),0.0)

        call anmscr (i,r,g,b)

      end do

      return
      end

*----6-----------------------------------------------------------------*-------
       subroutine get_colour(n,r,g,b)

* Colours defined by RGB e.g. 961 = (1,6/9,1/9)

       parameter (ncol=10)
       integer col(0:ncol)

       data col/
     &         999,102,016,145,183,582,953,947,669,789,999
     & /

       character char*3

* Wrap the colours if too many are asked for:
       istep = mod(n,ncol)

       write(char,'(i3.3)') col(istep)

       read(char(1:1),'(i1)',err=1) i1
       read(char(2:2),'(i1)',err=1) i2
       read(char(3:3),'(i1)',err=1) i3

       r = real(i1)/9.
       g = real(i2)/9.
       b = real(i3)/9.

*      write(*,*) 'Colour ',istep,'==',char,r,g,b

       return

1      r = 1
       g = 1
       b = 1

       end
*----6-----------------------------------------------------------------*-------
