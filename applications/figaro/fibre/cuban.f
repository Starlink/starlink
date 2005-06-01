      subroutine cuban(x,y,hex,disp,size,data,xadj,total,datmin,datmax,
     :            fitsta,status)
*+
* Name:
*    CUBAN

* Invocation:
*    CALL CUBAN(X,Y,HEX,DISP,SIZE,DATA,XADJ,TOTAL,DATMIN,DATMAX,
*                 FITSTA,STATUS)

* Purpose:
*   Inspect fits for 3-d data.

* Description:
*   To plot an array of profiles, and move around inspecting fits.
*     A small greyscale plot is also provided in the top right-hand
*   corner to give an idea of the current location within the data set.
*   Control of motion and fitting is by the use of a cursor.

* Arguments:
*      X(SPDIM1) = REAL ARRAY (Given)
*        X spatial dimension axis array
*      Y(SPDIM2) = REAL ARRAY (Given)
*        Y spatial dimension axis array
*      XADJ(SPDIM2) = REAL ARRAY (Given)
*        X displacement array (if present)
*      HEX = LOGICAL (Given)
*        If hexagonal array
*      DATA(WAVDIM,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Data array
*      TOTAL(SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Total intensity array
*      DATMIN = REAL (Given)
*        Data minimum for scaling profile plots
*      DATMAX = REAL (Given)
*        Data maximum for scaling profile plots
*      FITSTA(NCNTR,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given and returned)
*        Fit status
*      STATUS = INTEGER (Given and returned)
*        status, 0=ok
*      DISP = LOGICAL (Returned)
*        If data displayed

* Common:
*   arc_dims:
*      NCNTRL = INTEGER (Workspace)
*        Number of elements in control per position
*      NYP = INTEGER (Workspace)
*        Number of line slots
*      SPDIM1 = INTEGER (Workspace)
*        1st spatial dimension of data
*      SPDIM2 = INTEGER (Workspace)
*        2nd spatial dimension of data
*      WAVDIM = INTEGER (Workspace)
*        Wavelength dimension of data
*      D_CPTR = INTEGER (Workspace)
*        "Pointer" to control array
*      D_TLPTR = INTEGER (Workspace)
*        "Pointer" to left tram array
*      D_TRPTR = INTEGER (Workspace)
*        "Pointer" to right tram array
*      IDSPTR = INTEGER (Workspace)
*        "Pointer" line names array
*      IDSEND = INTEGER (Workspace)
*        "Pointer" end of line names array
*      D_VSPTR = INTEGER (Workspace)
*        "Pointer" to WAVDIM of REAL workspace

* Subroutines/functions referenced:
*      COPY2WORK, ENCODE_CONTRL, HEXPRARR, RECTPRARR,
*      ONE_LINE, RCHN2RX, RX2CHN, SET_FIT, SET_MASK, ZERO_REAL
*      GEN_RANGEF, PAR_QNUM, PAR_QUEST, PAR_WRUSER
*      PGBOX, PGCURSE, PGGRAY, PGPOINT, PGQVP, PGQWIN, PGQVPORT,
*      PGVPORT, PGWINDOW, PGWNAD
*      CHR_PUTC, CHR_PUTR

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 1-OCT-1990 Original version
*   TNW: 19-NOV-1990 Scaling for profile plots can be set by user
*   TNW: 4-JAN-91 Allow hardcopy plot.
*   TNW: 28-MAY-1991 New fit encoding/decoding
*   TNW: 2-MAR-1992 Alter number of profiles displayed
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      real x(spdim1)
      real y(spdim2)
      real xadj(spdim2)
      real fitsta(ncntrl,nyp,spdim1,spdim2)
      real data(wavdim,spdim1,spdim2)
      real total(spdim1,spdim2)
      logical hex
      logical disp
      real datmax,datmin

*

      real work(20,20)
      logical limit,loop,redraw,par_quest,add,inrange,floop,qstat
      logical par_qnum,dummy1,dummy2
      character ch,chr_upper
      character*40 chars
      integer pstat,pgcurse,len1,ix,iy,rx2chn
      integer line,nfit,nfailed,maskedout,nold,nnew
      include 'status_inc'
      integer fit_status(max_control)
      real size,xr(2),yr(2),cur_cenx,cur_ceny,xincr,yincr,xp,yp,value
      real xmin,xmax,ymin,ymax,xscale,wmin,wmax,tmp_cenx,tmp_ceny
      real curmax,curmin,xcmin,xcmax,ycmin,ycmax
      real tr(6),save(8),gr(4),xp1,yp1,rchn2rx
      integer xcmp,ycmp,i,j
      real npltx,nplty
      character dynamic_chars
      include 'DYNAMIC_MEMORY'
      equivalence(dynamic_mem,dynamic_chars)

* Copy default model into current

      do i = 1, MAX_DECODE_CONTROL
        deccntr(i) = default_model(i)
      end do

      line = 1
      npltx = 5
      nplty = 5

      title = 'Profile'

* Set range of data for plotting profiles

      curmin = datmin
      curmax = datmax

* What is the increment along each axis

      xincr = (x(spdim1) - x(1))/(spdim1-1)
      yincr = (y(spdim2) - y(1))/(spdim2-1)

* Get range of X

      if(hex) then
        call gen_rangef(xadj,1,spdim2,xmax,xmin)
        xmin = xmin + x(1)
        xmax = xmax + x(spdim1)
      else
        xmin = x(1)
        xmax = x(spdim1)
      end if
      xmin = xmin - xincr*0.5
      xmax = xmax + xincr*0.5
      ymax = y(spdim2) + yincr*0.5
      ymin = y(1) - yincr*0.5

* Get max and min values for centre

      xcmin = xmin + xincr*5.5
      xcmax = xmax - xincr*5.5
      if(xcmin.gt.xcmax) then
        xcmin = (xcmin+xcmax)*0.5
        xcmax = xcmin
      end if
      ycmin = ymin + yincr*5.5
      ycmax = ymax - yincr*5.5
      if(ycmin.gt.ycmax) then
        ycmin = (ycmin+ycmax)*0.5
        ycmax = ycmin
      end if

* Prepare for small greyscale plot, we copy the total array into a work
* array with dimensions 20 by 20.

      call zero_real(work,400)
      ycmp = spdim2/20 + 1
      if(hex) then
        xcmp = (spdim1/20)*nint( (xmax-xmin)/(x(spdim1)-x(1)) ) + 1
        xscale = real(spdim1)/((xmax-xmin)*real(xcmp))
        do j = 1, spdim2
          do i = 1, spdim1
            value = total(i,j)
            if(value.gt.0.0) then
              xp = x(i) + xadj(j)
              ix = int((xp - xmin)*xscale) + 1
              work(ix,iy) = work(ix,iy) + value
            end if
          end do
        end do
      else
        xcmp = spdim1/20 + 1
        do j = 1, spdim2
          iy = j/ycmp + 1
          do i = 1, spdim1
            value = total(i,j)
            if(value.gt.0.0) then
              ix = i/xcmp + 1
              work(ix,iy) = work(ix,iy) + value
            end if
          end do
        end do
      end if

* What is the range of this work array?

      call gen_rangef(work,1,400,wmax,wmin)

* Fill transform array for PGGRAY

      tr(2) = xincr*xcmp
      tr(1) = (x(1) + x(xcmp-1))*0.5 - tr(2)
      tr(3) = 0.0
      tr(6) = yincr*ycmp
      tr(4) = (y(1) + y(ycmp-1))*0.5 - tr(6)
      tr(5) = 0.0

*  Get starting position

      limit = .true.
      qstat = par_qnum('X starting position (centre)',xcmin,xcmax,
     :      x(spdim1/2),.true.,' ',cur_cenx)
      qstat = par_qnum('Y starting position (centre)',ycmin,ycmax,
     :      y(spdim2/2),.true.,' ',cur_ceny)
      loop = .true.
      redraw = .true.
      add = .false.
      call par_wruser('? for help',pstat)

      disp = .true.

      do while(loop)

*     Check centres in range

        cur_cenx = max(min(xcmax,cur_cenx),xcmin)
        cur_ceny = max(min(ycmax,cur_ceny),ycmin)

*     Work out limits

        xr(1) = cur_cenx - npltx*xincr
        xr(2) = cur_cenx + npltx*xincr
        yr(1) = cur_ceny - nplty*yincr
        yr(2) = cur_ceny + nplty*yincr

*     Display profiles with fits

        if(redraw) then
          len1 = 0
          call chr_putc('Centre of plot : ',chars,len1)
          call chr_putr(cur_cenx,chars,len1)
          call chr_putc(',',chars,len1)
          call chr_putr(cur_ceny,chars,len1)
          call par_wruser(chars(:len1),pstat)
          if(hex) then
            call hexprarr(data,total,xadj,x,y,size,xr,yr,limit,disp,
     :           .false.,curmin,curmax,line)
          else
            call rectprarr(data,x,y,total,xr,yr,limit,disp,.false.,
     :           curmin,curmax,line)
          end if

*     Get viewport and window

          call pgqvp(0,save(1),save(2),save(3),save(4))
          call pgqwin(save(5),save(6),save(7),save(8))

*     Draw mini-picture (greyscale) in corner

          call pgvport(0.86,0.99,0.50,0.99)
          call pgwnad(xmin,xmax,y(1),y(spdim2))
          call pgqvp(0,gr(1),gr(2),gr(3),gr(4))
          call pggray(work,20,20,1,20,1,20,wmax,wmin,tr)
          call pgbox('BC',0.0,0,'BC',0.0,0)

*     Mark current location

          call pgpoint(1,cur_cenx,cur_ceny,2)

*     Set coordinate transformation back to main plot

          call pgvport(save(1),save(2),save(3),save(4))
          call pgwindow(save(5),save(6),save(7),save(8))
        end if
        redraw = .true.

        call sync
        if(.not.disp) then

*     We've just made a hardcopy plot, so loop around again

          disp = .true.

*     Read cursor position (and key)

        else if(pgcurse(xp,yp,ch).ne.1) then
          call par_wruser('Error with cursor',pstat)
          loop = .false.
          status = SAI__ERROR
        else
          ch = chr_upper(ch)

*       If rectangular array convert position to axis array values.
*       Check if in range on main plot

          if(hex) then
            xp1 = xp
            yp1 = yp
            ix = rx2chn(x,spdim1,xp-xadj(iy))
            iy = rx2chn(y,spdim2,yp)
          else
            xp1 = rchn2rx(x,spdim1,xp)
            yp1 = rchn2rx(y,spdim2,yp)
            ix = nint(xp)
            iy = nint(yp)
          end if
          inrange = (xp1.ge.xmin).and.(xp1.le.xmax).and.
     :                  (yp1.ge.ymin).and.(yp1.le.ymax)
          if(ch.eq.'E') then
            loop = .false.

*      Move around:

          else if(ch.eq.'U') then

*       Up

            cur_ceny = cur_ceny + 10.0*yincr
          else if(ch.eq.'D') then

*       Down

            cur_ceny = cur_ceny - 10.0*yincr
          else if(ch.eq.'L') then

*       Left

            cur_cenx = cur_cenx - 10.0*xincr
          else if(ch.eq.'R') then

*       Right

            cur_cenx = cur_cenx + 10.0*xincr
          else if(ch.eq.'C') then
            if(inrange) then
              cur_cenx = xp1
              cur_ceny = yp1
            else
              call par_wruser('Out of range',pstat)
            end if

          else if(ch.eq.'P') then

*        Indicate position

            if(inrange) then
              len1 = 0
              call chr_putc('Position : ',chars,len1)
              call chr_putr(xp1,chars,len1)
              call chr_putc(',',chars,len1)
              call chr_putr(yp1,chars,len1)
              call par_wruser(chars(:len1),pstat)
            else
              call par_wruser('Out of range',pstat)
            end if
            redraw = .false.

          else if(ch.eq.'X') then

*        Delete fit

            if(inrange) then
              call zero_int(fitsta(1,line,ix,iy),ncntrl)
            else
              call par_wruser('Out of range',pstat)
            end if
            redraw = .false.

          else if(ch.eq.'A') then

            redraw = .false.

            if(inrange) then

*     Add for fit

              call copy2work(dynamic_mem(d_vsptr),wavdim,data(1,ix,iy),
     :                add)
            else
              call par_wruser('Out of range',pstat)
            end if
          else if(ch.eq.'F') then

*       Fit

            if(inrange) then
              floop = .true.
              do while(floop)

*            Perform fit

                call copy2work(dynamic_mem(d_vsptr),wavdim,data(1,ix,iy)
     :                  ,add)

*            get the model type

                call set_fit_menu(dummy1,dummy2,deccntr,wavdim,gpscal
     :               ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss
     :               ,line_count,errpre,inherit,status)

*            encode into control

                call encode_contrl(deccntr,ncntrl,fit_status)
                call set_control(%VAL(CNF_PVAL(d_cptr)),line,ix,iy,
     :                  fit_status)
                nfit = nfit + 1

*             Perform fitting

                call one_line(1,%VAL(CNF_PVAL(d_tlptr)),
     :                  %VAL(CNF_PVAL(d_trptr)),ix,ix,nfit,
*     :     dynamic_chars(idsptr:idsend),%VAL(CNF_PVAL(d_cptr))
     :            idstring,%VAL(CNF_PVAL(d_cptr))
     :                  ,line,nnew,nold,nfailed,maskedout,
     :            par_quest('Plot data',.true.),.true.,floop,iy,iy,
     :            status)
                floop = .not.par_quest('Return to profile array?'
     :                                    ,.not.floop)
                if(status.ne.SAI__OK) floop = .false.
                add = .false.
              end do
            else
              call par_wruser('Out of range',pstat)
            end if

          else if(ch.eq.'J') then

*     Convert to NDC

            tmp_cenx = ( (xp-save(5))/(save(6)-save(5)) )
     :                        * (save(2)-save(1)) + save(1)
            tmp_ceny = ( (yp-save(7))/(save(8)-save(7)) )
     :                        * (save(4)-save(3)) + save(3)

*     Convert to X,Y on greyscale plot

            tmp_cenx = ( (tmp_cenx-gr(1))/(gr(2)-gr(1)) )
     :                        * (xmax - xmin) + xmin
            tmp_ceny = ( (tmp_ceny-gr(3))/(gr(4)-gr(3)) )
     :                        * (ymax - ymin) + ymin
            if((tmp_cenx.gt.xmin).and.(tmp_cenx.lt.xmax).and.
     :                  (tmp_ceny.gt.ymin).and.(tmp_ceny.lt.ymax)) then
              cur_cenx = tmp_cenx
              cur_ceny = tmp_ceny
            else
              call par_wruser('Out of range',pstat)
            end if

          else if(ch.eq.'S') then

*     Scale
            qstat = par_qnum('Minimum? ',datmin,datmax,curmin,.true.,
     :           ' ',value)
            curmin = value
            qstat = par_qnum('Maximum? ',datmin,datmax,curmax,.true.,
     :           ' ',value)
            curmax = value

          else if(ch.eq.'H') then

*     Hardcopy

            disp = .false.

          else if(ch.eq.'N') then

            qstat = par_qnum('Number in X?',1.0,real(spdim1),npltx*2.0,
     :           .true.,' ',value)
            npltx = value*0.5
            qstat = par_qnum('Number in Y?',1.0,real(spdim1),nplty*2.0,
     :           .true.,' ',value)
            nplty = value*0.5

*     Help

          else if(ch.eq.'?') then
            redraw = .false.
            call par_wruser('The following keys are defined:',pstat)
            call par_wruser(' A - Add profiles to fit (end with F)',
     :                        pstat)
            call par_wruser(' C - Centre here',pstat)
            call par_wruser(' D - Down',pstat)
            call par_wruser(' E - Exit',pstat)
            call par_wruser(' F - Make fit to point',pstat)
            call par_wruser(' H - Make hardcopy of current plot',pstat)
            call par_wruser(
     :            ' J - Jump to new area (using greyscale plot)',pstat)
            call par_wruser(' L - Left',pstat)
            call par_wruser(' N - Alter number of profiles plotted',
     :           pstat)
            call par_wruser(' P - Indicate position',pstat)
            call par_wruser(' R - Right',pstat)
            call par_wruser(' S - Set scaling for profile plots',pstat)
            call par_wruser(' U - Up',pstat)
            call par_wruser(' X - Erase fit',pstat)
            call par_wruser(' ? - Help',pstat)
            call par_wruser(' + - Go to next line in list',pstat)
            call par_wruser(' - - Go to previous line in list',pstat)
          else if(ch.eq.'+') then
            line = line + 1
            if(line.gt.line_count) line = 1
            len1 = 0
            call chr_putc('Line ',chars,len1)
            call chr_puti(line,chars,len1)
            call par_wruser(chars(:len1),pstat)
          else if(ch.eq.'-') then
            line = line - 1
            if(line.eq.0) line = line_count
            len1 = 0
            call chr_putc('Line ',chars,len1)
            call chr_puti(line,chars,len1)
            call par_wruser(chars(:len1),pstat)
          else
            call par_wruser('Key '//ch//' undefined',pstat)
            redraw = .false.
          end if
        end if
      end do
      end
