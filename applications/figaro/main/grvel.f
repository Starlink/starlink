      subroutine grvel(z,spdim1,nchans,left,right,lincnt,vcorr,vtype,
     :     datafile,wave,sdata,linnam,batch,ifcont,ifsoft,status)
*+
* Name:
*    GRVEL

* Invocation:
*    CALL GRVEL(Z,SPDIM1,NCHANS,LEFT,RIGHT,LINCNT,VCORR,VTYPE,
*          DATAFILE,WAVE,SDATA,LINNAM,BATCH,IFCONT,IFSOFT,STATUS)

*
* Purpose:
*   To plot a grey-scale image or contour plot of velocities.
*
* Description:
*   To plot a grey-scale image or contour plot of velocities.
*
* Author:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92 then
*        Durham
* History:
*   TNW: 4/88 Original version
*   Revised to use get_grey, TNW 30/6-1/7/88
*   TNW 12,14/12/88 Made able to work in batch
*   TNW 16/12/88, Changed to use FIG_XVALUE
*   TNW 3/90, PGPLOT version
*   TNW 26/4/90, Made to also perform contour plotting.
*   TNW 21/2/91, Check for colour based on >50 colour levels rather
*                than >100.
*   TNW 25/3/91 Use PRM_PAR
*   TNW 5/7/91 All parameters cancelled after use (if interactive)
*   TNW: 15/2/94 Use QMENU
*   TNW: 18/3/94 Use QCHECK

* Arguments:
*     Z(NCHANS,SPDIM1) = REAL ARRAY (Given)
*        The data
*     NCHANS,SPDIM1 = INTEGER (Given)
*        Dimensions of above
*     LEFT(LINCNT) = REAL ARRAY (Given)
*        Left boundaries of lines
*     RIGHT(LINCNT) = REAL ARRAY (Given)
*        Right boundaries of lines
*     LINCNT = INTEGER (Given)
*        Number of lines
*     VCORR = REAL (Given)
*        Correction for velocity
*     VTYPE = INTEGER (Given)
*        Type of correction
*     DATAFILE = CHARACTER*(*) (Given)
*        Name of file
*     WAVE(LINCNT) = REAL ARRAY (Given)
*        Rest wavelengths of lines
*     SDATA(NCHANS) = REAL ARRAY (Given)
*        X array
*     LINNAM(LINCNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*     BATCH = LOGICAL (Given)
*        If running in batch mode
*     IFCONT = LOGICAL (Given)
*        If to create contour plot rather than greyscale
*     IFSOFT = LOGICAL (Given)
*        If plot is to be in softcopy (contour plot only)
*
* Subroutines/functions referenced:
*     GET_GREY             : Select/open greyscale graphics device
*     GRYPLT               : Create greyscale image
*     GR_SELCT             : Select/open graphics device
*     RX2RCHN = REAL (Given)
*        Convert X value to channel number
*
*     DSA_AXIS_RANGE       : Get range of axis to use
*     DSA_FREE_WORKSPACE   : Free workspace
*     DSA_GET_WORK_ARRAY   : Get workspace
*     FIG_XVALUE = REAL (Given)
*        Convert real bin number to real "array value"
*     PAR_GIVEN = LOGICAL (Given)
*        Find out if parameter given in command line
*     PAR_QNUM             : Obtain numeric response from user
*     PAR_QUEST = LOGICAL (Given)
*        Obtain YES/NO response from user
*     PAR_RDKEY            : Obtain YES/NO parameter from user
*     PAR_RDVAL            : Obtain numeric (parameter) response from
*                            user
*     PGCONS               : Draw contour map
*     PGBOX, PGENV, PGLABEL, PGPAGE, PGQCOL, PGQVP, PGVPORT, PGWINDOW
*
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'gr_inc'

      integer nchans,spdim1,vtype,lincnt,line,status
      integer ist,ien,xrange
      logical batch,ifcont,ifsoft
      integer iwork,slot
      integer nlevs,iyst,yrange,iyen
      real z(nchans,spdim1),left(lincnt),right(lincnt),vcorr
      real wave(lincnt),velb(2),ylim(2),value,fig_xvalue,minval
      integer i,j,len1,len2
      real x1,x2,y1,y2,xm,ym,aspz
      real white,black,sdata(nchans),asp,dummy1,dummy2
      logical loop,first,floop
      real flo,hi,tr(6)
      integer MLEVEL
      parameter (MLEVEL = 20)
      real level(MLEVEL),dcont
      real xstart,rxrange,xend,rx2rchn
      logical getwht,getblk,par_given,allowcolour
      character*(*) datafile*(*),xlabel*25
      character*3 vctype(2)
      character*10 linnam(lincnt)
      character bss*2,bs*1
      character*80 label,chars
      integer NDICT,curmen,key
      parameter (NDICT = 10)
      character*39 dict(NDICT)
      character*15 title,dumc*1

* velocity of light

      real c
      parameter(c=2.997925e5)
      integer ci1,ci2,slev,dict1,cndict
      integer MAXOPT, OPT_COL, OPT_LOG, OPT_RAT, VAL_LINE, MAXVAL,
     :     VAL_START, VAL_END
      parameter (OPT_COL = 1, OPT_LOG = 2, OPT_RAT = 3, VAL_LINE = 1,
     :     VAL_START = 4, VAL_END = 5)
      parameter (MAXOPT = 3, MAXVAL = 5)
      logical options(MAXOPT)
      real values(MAXVAL)

      save black,white,first,options,values
      data black,white/0.0,100.0/
      data first/.true./
      data vctype/'HEL','LSR'/
      data bss/'\\'/
      data options/.false.,.false.,.true./
      data values/1.0,0.0,100.0,1.0,0.0/
      data dict/
     :     'L COLOUR : Use colour look-up table',
     :     'L LOG    : Use logarithmic scaling',
     :     'L RATIO  : Make plot same ratio as data',
     :     'F LINE   : Line to plot',
     :     'F BLACK  : Value for black',
     :     'F WHITE  : Value for white',
     :     'F START  : Start cross-section',
     :     'F END    : End cross-section',
     :     'Q PLOT   : Perform next plot',
     :     'Q QUIT   : Stop plotting'/

      iyst = 1
      iyen = spdim1
      bs = bss(1:1)
      if(ifcont) then
         title = 'Contour plots'
      else
         title = 'Greyscale plots'
      end if

      if(status.ne.SAI__OK) return

* Open plotting device, and if greyscale find out how many levels it has

      curmen = 0
      allowcolour = .false.
      if(ifcont) then
         call gr_selct(ifsoft,status)
         if(status.ne.SAI__OK) then
            return
         end if
      else
         call get_grey('device',status)
         if(status.ne.SAI__OK) then
            return
         end if

*  Check colour facilities

         call pgqcol(ci1,ci2)
         nlevs = ci2

*   No way to find out that I can see if does actually have colour

         if(nlevs.gt.50) then
            allowcolour = .true.
         end if
      end if

* If in batch mode then this routine will plot all lines starting at
* one. Auto-scaling will be used unless the scaling parameters are
* explicitly given in the command line (the values will be obtained
* later). If only one of these parameters is given, then the other
* will use the value from auto-scaling.

      if(batch) then
         values(VAL_LINE) = 1.0
         getwht = par_given('white')
         getblk = par_given('black')
         call par_rdkey('log',.false.,options(OPT_LOG))
      else
         getwht = .true.
         getblk = .true.
      end if

* Don't allow user to try to take the log of zero, or of a negative
* number.

      if(options(OPT_LOG)) then
         minval = VAL__SMLR
         if(first) then
            values(3) = 1
         end if
      else
         minval = VAL__MINR
      end if
      first = .false.
      values(3) = max(minval,values(3))
      values(2) = max(minval,values(2))

      floop = .true.

      if(allowcolour) then
         dict1 = 1
         cndict = NDICT
      else
         dict1 = 2
         cndict = NDICT - 1
      end if

      loop = .true.
      do while(loop)

*  Loop until settings are ok

         if(.not.batch) then
            values(VAL_START) = real(iyst)
            values(VAL_END) = real(iyen)
            call qcheck(title,dict(dict1),cndict,values,dumc,
     :                  options(dict1),key,status)
            line = nint(values(VAL_LINE))
            iyst = nint(values(VAL_START))
            iyen = nint(values(VAL_END))
            iyst = max(1,iyst)
            iyst = min(iyst,spdim1)
            iyen = max(iyst,iyen)
            iyen = min(iyen,spdim1)
         end if
         if(status.ne.SAI__OK) then
            return
         else if(key.eq.cndict) then

*  If greyscale device, then close it

            if(.not.ifcont) call clgrap
            return
         end if

*  Erase plot after first loop

         if(floop) then
            floop = .false.
         else if(options(OPT_RAT)) then
            call pgpage
         end if

* Get pixel boundaries of line

         xstart = rx2rchn(sdata,nchans,left(line))
         xend = rx2rchn(sdata,nchans,right(line))

*   Scaling is slightly different for contour or greyscale plots

         if(ifcont) then
            ist = max(1,int(xstart))
            ien = min(nchans,(int(xend)+1))
            rxrange = xend - xstart

* Convert boundaries to velocity

            velb(1) = c*(left(line)-wave(line))/wave(line) - vcorr
            velb(2) = c*(right(line)-wave(line))/wave(line) - vcorr

         else

* Get pixel boundaries of line

            ist = nint(xstart)
            ien = nint(xend)
            xrange = ien - ist + 1
            rxrange = real(xrange)

* Convert boundaries to velocity

            velb(1) = c*(fig_xvalue((real(ist)-0.5),sdata,nchans)-
     :           wave(line))/wave(line) - vcorr
            velb(2) = c*(fig_xvalue((real(ien)+0.5),sdata,nchans)-
     :           wave(line))/wave(line) - vcorr
         end if

* Set up Y limits

         if(batch) then
            call dsa_axis_range('data',2,' ',.false.,dummy1,dummy2,iyst,
     :                          iyen,status)
         end if
         if(status.ne.SAI__OK) return
         if(ifcont) then
            ylim(1) = real(iyst)
            yrange = iyen - iyst
            ylim(2) = real(iyen)
         else
            ylim(1) = real(iyst) - 0.5
            yrange = iyen - iyst + 1
            ylim(2) = real(iyen) + 0.5
         end if

         if(options(OPT_RAT)) then
            call pgvport(0.0,1.0,0.0,1.0)
            call pgqvp(2,x1,x2,y1,y2)
            xm = x2
            ym = y2
            aspz = xm/ym
            asp = rxrange/(real(yrange)*aspz)
            if(asp.lt.1.0) then

* Set Y dimension to max

               y1 = 0.15
               y2 = 0.9
               x1 = 0.55-asp*0.4
               x2 = 0.55+asp*0.4
            else

* Set X dimension to max

               x1 = 0.15
               x2 = 0.95
               y1 = 0.55-0.4/asp
               y2 = 0.55+0.4/asp
            end if
            call pgvport(x1,x2,y1,y2)
            call pgwindow(velb(1),velb(2),ylim(1),ylim(2))
         else
            call pgenv(velb(1),velb(2),ylim(1),ylim(2),0,-2)
         end if

* If in batch mode then auto-scale data, unless both scaling parameters
* are given in command line.

         if(batch.and.(.not.(getwht.and.getblk))) then
            values(2) = z(ist,iyst)
            values(3) = values(2)
            do j = iyst,iyen
               do i = ist, ien
                  values(2) = max(z(i,j),values(2))
                  values(3) = min(z(i,j),values(3))
               end do
            end do

*   Force "WHITE" to be a sensible amount above zero for log plots
*   - might be better to use get_median here

            if(options(OPT_LOG)) values(3)=max(values(3),values(2)*5.0e
     :           -3)
         end if
         if(batch) then
            if(getwht) then
               call par_rdval('white',minval,VAL__MAXR,values(3),' '
     :              ,value)
            end if
            if(getblk) then
               call par_rdval('black',minval,VAL__MAXR,values(2),' '
     :              ,value)
            end if
         end if

         if(ifcont) then

*    We don't need to take the log of the data, only use logs for the
*    contour levels

            if(options(OPT_LOG)) then
               flo=log10(values(3))
               hi=log10(values(2))
               dcont = (hi-flo)/real(MLEVEL-1)
               do i=1,MLEVEL
                  level(i) = 10**(flo + dcont*real(i-1))
               end do
            else
               flo=values(3)
               hi=values(2)
               dcont = (hi-flo)/real(MLEVEL-1)
               do i=1,MLEVEL
                  level(i) = flo + dcont*real(i-1)
               end do
            end if

*     dispersion (km/s)

            tr(2) = (velb(2)-velb(1))/(rxrange)
            tr(1) = c*(sdata(ist)-wave(line))/wave(line) - vcorr -
     :           real(ist)*tr(2)
            tr(3) = 0.0
            tr(4) = 0.0
            tr(5) = 0.0
            tr(6) = 1.0

* Draw contours

            call pgcons(z,nchans,spdim1,ist,ien,iyst,iyen,level,MLEVEL,
     :           tr)
         else

*   Perform greyscale plotting

            if(nlevs.gt.50) then
               slev = 16
            else
               slev = 2
            end if

*      Get VM

            call dsa_get_work_array(xrange*yrange,'float',iwork,slot,
     :                              status)
            if(status.ne.SAI__OK) return
            call gryplt(z(1,iyst),%VAL(CNF_PVAL(iwork)),nchans,yrange,
     :                  xrange,ist,nlevs,values(3),values(2),
     :                  options(OPT_COL),options(OPT_LOG),slev)
            call dsa_free_workspace(slot,status)
         end if
         if(status.ne.SAI__OK) return

* Plot axis

         call pgbox('BCNITS',0.0,0.0,'BCNITS',0.0,0.0)

* Label axis

         if((vtype.eq.1).or.(vtype.eq.2)) then
            xlabel = 'V'//bs//'d'//vctype(vtype)//bs//'u/kms'//bs//
     :           'u-1'//bs//'d'
         else
            xlabel = 'Velocity/km s'//bs//'u-1'//bs//'d'
         end if

*     If name of line includes a greek letter (e.g. HALPHA) then plot as
*     such

         len2 = 0
         call chr_fill(' ',chars)
         call chr_appnd(datafile,chars,len2)
         len2 = len2 + 1
         call chr_appnd(linnam(line),chars,len2)
         label = ' '
         call greek_letters(chars(:len2),label,len1,.false.)
         call pglabel(xlabel,'Cross-section',label)
         if(batch) then
            line = line+1
            loop = line.le.lincnt
         end if
      end do
      end
