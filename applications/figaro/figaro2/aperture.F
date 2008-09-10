#if HAVE_CONFIG_H
#  include "config.h"
#endif
      subroutine aperture
C+-----------------------------------------------------------------------------
C
C   ---------------
C   A P E R T U R E
C   ---------------
C
C   Description
C   -----------
C   Do simple-minded aperture photometry on a series of frames
C
C
C   Scope of program
C   ----------------
C   - Handles 2-D images only.
C   - Data array converted to FLOAT.
C   - Magic values supported.
C   - Error arrays not supported
C   - Quality arrays not supported.
C   - Batch execution not supported.
C
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters
C   ----------
C   OUTPUT    Name of output ASCII file for results. The output info 
C             goes in the following order: (1) Either 'O' or 'S' for 
C             "object" or  "sky"; (2) a sequence number; (3) the x,y 
C             coordinates of the cursor, (4) the radius of the aperture;
C             (5) the total flux in pixels which are with a radial
C             distance less than or equal to the aperture radius.  No 
C             correction is made for partial pixels; (6) the total 
C             number of pixels included.
C             (character)(prompted for)
C
C   IMAGE     Name of individual images in case you're not using a 
C             list file. (character)(prompted for)
C
C   RADIUS    Radius for integration. (real)(prompted for)
C
C   LOW       Data value for lowest level in 2-d plot 
C             (real)(prompted for)
C
C   HIGH      Data value for highest level in 2-d plot
C             (real)(prompted for)
C
C
C   Keywords 
C   --------
C   OK
C
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant
C
C
C   Method
C   ------
C   - The image name is read and that image is plotted on the current
C     plot device.
C   - The user is presented with a menu which allows him/her/it to
C     specify object and sky regions, change the colour levels, change 
C     the radius of the aperture, show cuts or quit.
C   - Integrations are a simple sum of the values of the pixels within
C     the aperture radius.
C   
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_FREE_WORKSPACE
C     DSA_GET_ACTUAL_NAME
C     DSA_GET_FLAG_VALUE
C     DSA_GET_WORK_ARRAY
C     DSA_MAP_DATA
C     DSA_INPUT
C     DSA_OPEN
C     DSA_OPEN_TEXT_FILE
C     DSA_SEEK_FLAGGED_VALUES
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library ICH:
C     ICH_FOLD
C     ICH_LEN
C
C   Library NDP:
C     NDP_IMAGE_INDEX
C     NDP_IMAGE_LUT
C     NDP_IMAGE_PLOT
C     NDP_IMAGE_VIEWPORT
C
C   Library PAR:
C     PAR_CNPAR
C     PAR_RDCHAR
C     PAR_RDUSER
C     PAR_RDVAL
C
C   PGPLOT:
C     PGASK
C     PGBEGIN
C     PGCURSE
C     PGEND
C     PGENV
C     PGLABEL
C     PGLINE
C     PGPAGE
C     PGWINDOW
C
C   Library VAR:
C     VAR_GETCHR
C
C   Internal subroutines called
C   ---------------------------
C     ADDUP
C     CRE_CIRCLE
C     DISPLAY
C     MOVE_CIRCLE
C     PLOT_CUT
C     SUB_APERTURE
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters /
C   DO WHILE / APPEND access
C
C
C   Possible future upgrades
C   ------------------------
C
C   Author/s
C   --------
C   Jim Lewis           RGO  (CAVAD::JRL  or JRL@UK.AC.CAM.AST-STAR)
C   Horst Meyerdierks   UoE, Starlink  (hme)
C
C
C   History
C   -------
C     05 Sep 1991  Original program
C     04 Feb 1992  Modified to use new NDP 2d graphics routines. (JRL)
C     27 Aug 1992  Modified to allow list directed parameters.  A bit
C                  of general tidying up was also done. (JRL)
C     16 Jun 1993  Use user variable SOFT, not SOFTDEV. Allow "?"
C                  instead of "H" to get help. In "P" option do not 
C                  cancel TABLE parameter. Thus it should not be asked 
C                  for, a new value would have no effect anyway. Check 
C                  PAR_ABORT flag. (hme)
C     24 Aug 1994  Port to Unix. Change INCLUDE. Disable colour table.
C                  Problem is that the PGBEGIN calls will reset the
C                  colour table from what might be set previously. Must
C                  use the technique of IMAGE, i.e. open /append and
C                  draw some nonsense and advance one page. Use 
C                  DSA_WRUSER with DSA_FLUSH where appropriate.
C                  Reserve 16 pens instead of four. Thus compatible with
C                  IMAGE. Then also use IDEV instead of SOFT.
C                  (hme)
C     15 Sep 1994  Where the two 1-D cuts are plotted, two calls
C                  PGPOINT/PGPAGE are necessary. In the same area,
C                  disuse PAR_RDUSER and introduce the logical 
C                  parameter OK. (hme)
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Avoid _NAMED_ routines.
C                  Bad pixel handling.
C     18 Jul 1996  MJCL / Starlink, UCL.  Set CHARACTER variables for
C                  storage of file names to 132 chars.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C
C   Other Comments
C   --------------
C
C+-----------------------------------------------------------------------------
      implicit none

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

C
C     Functions
C
      logical par_abort
      integer ich_len
C
C     Local variables
C
      logical   badpix           ! Flag for existence of magic values
      integer   dims(2)          ! Dimensions of input data
      character input*132        ! Name of input file
      integer   iptr             ! Dynamic-memory pointer
      integer   islot            ! Dynamic-memory slot
      character message*80       ! Message character variable
      integer   ndim             ! Number of dimensions
      integer   nelm             ! Number of elements
      character oname*132        ! Full name of output file
      character output*132       ! Name of output file
      integer   ounit            ! Output file logical unit
      integer   status           ! Status variable
      integer   wptr             ! Dynamic-memeory pointer
      integer   wslot            ! Dynamic-memory slot
      integer   wxslot           ! Dynamic-memory slot
      integer   wyslot           ! Dynamic-memory slot
      integer   wxptr            ! Dynamic-memory pointer
      integer   wyptr            ! Dynamic-memory pointer
C
C     Startup DSA
C
      status = 0
      call dsa_open(status)
      if (status .ne. 0) go to 500
C
C     Open the image file
C
      call dsa_input('image','image',status)
      call dsa_get_actual_name('image',input,status)
      if (status .ne. 0) go to 500
C     
C     Get the output file name
C
      call par_rdchar('output',' ',output)
      if (par_abort()) go to 500
      call dsa_open_text_file(output,'.out','unknown',.true.,ounit,
     :                        oname,status)
      close(ounit)
      open(unit=ounit,file=oname,status='old',
#if HAVE_FC_OPEN_POSITIONAPPEND
     :     access='sequential',
     :     position='append',
#elif HAVE_FC_OPEN_ACCESSAPPEND
     :     access='append',
#else
      Do not have ACCESS=APPEND or POSITION=APPEND !
#endif
     :     iostat=status)
      if (status .ne. 0) then
         call dsa_wruser('Trouble opening output file')
         call dsa_wrflush
         go to 500
      end if
C
C     Get dimensionality and map input
C     
      call dsa_data_size('image',2,ndim,dims,nelm,status)
      call dsa_use_flagged_values('image',status)
      call dsa_map_data('image','read','float',iptr,islot,status)
      call dsa_seek_flagged_values('image',badpix,status)
      if (status .ne. 0) go to 500
C
C     Get some dummy arrays for the axis arrays and for display
C
      call dsa_get_work_array(dims(1),'float',wxptr,wxslot,status)
      call dsa_get_work_array(dims(2),'float',wyptr,wyslot,status)
      call dsa_get_work_array(dims(1)*dims(2),'int',wptr,wslot,status)
      if (status .ne. 0) go to 500
C
C     Write a few things to the output file
C
      write(ounit,101,iostat=status) input(:ich_len(input))
  101 format('Results for file: ',a)
      if (status .ne. 0) then
         call dsa_wruser('Trouble writing to output file')
         call dsa_wrflush
         go to 500
      end if
C
C     Write a helpful message to the user...
C
      write(message,102) input(:ich_len(input))
  102 format('Begining analysis of file: ',a)
      call dsa_wruser(message(:ich_len(message)))
      call dsa_wrflush
C     
C     Now do any work required
C
      call sub_aperture(dims(1),dims(2),%VAL(CNF_PVAL(iptr)),
     :                  badpix,%VAL(CNF_PVAL(wxptr)),
     :                  %VAL(CNF_PVAL(wyptr)),
     :                  %VAL(CNF_PVAL(wptr)),ounit,status)
      if (status .ne. 0) go to 500
C
C     Tidy and exit
C
  500 call dsa_close(status)
      end


      subroutine sub_aperture(nx,ny,input,badpix,xaxis,yaxis,work,unit,
     :                        status)
C+----------------------------------------------------------------------
C     SUB_APERTURE --- Present the menu for aperture photometry
C
C     Parameters (">" Input, "<" Output, "W" Workspace, "    !" Changed)
C
C     (>) NX:          (integer) X dimension of the data
C     (>) NY:          (integer) Y dimension of the data
C     (>) INPUT:       (real array INPUT(NX,NY)) Input frame
C     (>) BADPIX:      (logical) Flag for magic values in data
C     (W) XAXIS:       (real array XAXIS(NX)) Workspace for X axis array
C     (W) YAXIS:       (real array YAXIS(NY)) Workspace for Y axis array
C     (W) WORK:        (integer array WORK(NX,NY)) Display workspace
C     (>) UNIT:        (integer) Logical unit of output file
C     (!) STATUS:      (integer) Status variable
C
C
C     AUTHOR:      Jim Lewis -- RGO
C     DATE:        05-SEP-1991
C     UPDATE:      04-FEB-1991 -- Modified to use new NDP 2d plotting
C                                 routines (JRL)
C
C+----------------------------------------------------------------------
      implicit none

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      integer nx,ny,unit,status
      integer work(nx,ny)
      real input(nx,ny),xaxis(nx),yaxis(ny)
      logical badpix
C
C     Functions
C
      logical par_abort
      integer ich_fold
      integer ich_len
C
C     Local variables
C
      integer dumint,i,wslot,wslot2,wslot3,wslot4,wslot5,wslot6
      integer wpt1,wpt2,wpt3,wpt4,wpt5,wpt6
      integer iobj,isky,ixmin,ixmax,iymin,iymax,nbig
      real low,high,x,y,sum,sumsky,radius,oldlow,oldhigh,apparea
      character opt*1,optup*1,table*20,softdev*16
      logical continue,picture,init
C
C     Number of pixels used to draw a circle
C
      integer npix
      parameter (npix=50)
C
C     Numeric ranges
C
C     include 'ndp_source:ndp_numeric_ranges.inc'
      INTEGER*2 MAX_BYTE,MIN_BYTE
      PARAMETER (MAX_BYTE=255,MIN_BYTE=-255)
C
      INTEGER*2 MAX_SHORT,MIN_SHORT
      PARAMETER (MAX_SHORT=32767,MIN_SHORT=-32767)
C
      INTEGER   MAX_INT,MIN_INT
      PARAMETER (MAX_INT=65535,MIN_INT=-65535)
C
      REAL      MIN_FLOAT,MAX_FLOAT
      PARAMETER (MAX_FLOAT=1.7E38,MIN_FLOAT=-1.7E38)
C
C     Data statements 
C
      data init /.true./
C
C     Get current PGPLOT screen device on initial entry
C
      call var_getchr('idev',0,0,softdev,status)
      if (status .ne. 0) then
         call dsa_wruser
     :   ('No screen device selected -- use IDEV command')
         call dsa_wrflush
         go to 500
      end if
      call pgbegin(0,softdev(:ich_len(softdev))//'/append',1,1)
      call pgask(.false.)
      call pgpoint( 1, 0.5, 0.5, -1 )
      call pgpage
      if (status .ne. 0) go to 500
C
C     Fill in the axis arrays
C
      do i = 1,nx
         xaxis(i) = float(i)
      end do
      do i = 1,ny
         yaxis(i) = float(i)
      end do
C
C     Initialise a few things
C
      iobj = 0
      isky = 0
      sumsky = 0.0  
      picture = .false.
C
C     Preliminary set of radius
C
      call par_rdval('radius',0.0,max_float,1.0,'pixels',
     :               radius)
      if (par_abort()) go to 500
C
C     Get workspace for drawing circles
C
      call dsa_get_work_array(npix,'float',wpt1,wslot,status)
      call dsa_get_work_array(npix,'float',wpt2,wslot2,status)
      call dsa_get_work_array(npix,'float',wpt3,wslot3,status)
      call dsa_get_work_array(npix,'float',wpt4,wslot4,status)
      if (status .ne. 0) go to 500
      call cre_circle(npix,radius,%VAL(CNF_PVAL(wpt1)),
     :                %VAL(CNF_PVAL(wpt2)))
C
C     Do preliminary display
C
      if (.not. init) call par_cnpar('low')
      call par_rdval('low',min_float,max_float,oldlow,' ',low)
      if (.not. init) call par_cnpar('high')
      call par_rdval('high',min_float,max_float,oldhigh,' ',high)
      if (par_abort()) go to 500
      table = 'grey'
      call ndp_image_index(nx*ny,low,high,input,badpix,work,status)
      if (status .ne. 0) go to 500
      call display(work,nx,ny,low,high,xaxis,yaxis,table,status)
      if (status .ne. 0) go to 500
      picture = .true.
      oldlow = low
      oldhigh = high
C
C     Helpful advice
C
      call dsa_wruser('Issue commands at the keyboard (H for help)')
      call dsa_wrflush
C
C     Loop until you decide you want to quit
C
      continue = .true.
      do while (continue)
C
C        Get the option
C
         call pgcurse(x,y,opt)
         optup = opt
         dumint = ich_fold(optup)
C
C        P:  Do a 2-d plot of the image
C
         if (optup .eq. 'P') then
            call par_cnpar('low')
            call par_rdval('low',min_float,max_float,oldlow,' ',low)
            call par_cnpar('high')
            call par_rdval('high',min_float,max_float,oldhigh,' ',high)
            if (par_abort()) go to 500
            table = 'grey'
            if ((low .ne. oldlow) .or. (high .ne. oldhigh)) then
               call ndp_image_index(nx*ny,low,high,input,badpix,
     :                              work,status)
               if (status .ne. 0) go to 500
            end if             
            call pgpage
            call display(work,nx,ny,low,high,xaxis,yaxis,table,status)
            if (status .ne. 0) go to 500
            picture = .true.
            oldlow = low
            oldhigh = high
C
C        R:  Change the radius of the circle
C
         else if (optup .eq. 'R') then
            call par_cnpar('radius')
            call par_rdval('radius',0.0,max_float,1.0,'pixels',
     :                     radius)
            if (par_abort()) go to 500
            call cre_circle(npix,radius,%VAL(CNF_PVAL(wpt1)),
     :                      %VAL(CNF_PVAL(wpt2)))
C
C        Q:  Quit
C
         else if (optup .eq. 'Q') then
            continue = .false.
C
C        O: Indicate an object
C
         else if (optup .eq. 'O') then
            if (.not. picture) then
               call dsa_wruser('This might be a bit difficult without')
               call dsa_wruser(' a plot on the screen.  Try option')
               call dsa_wruser(' P first')
               call dsa_wrflush
            else
               call move_circle(npix,%VAL(CNF_PVAL(wpt1)),
     :                          %VAL(CNF_PVAL(wpt2)),x,y,
     :                          %VAL(CNF_PVAL(wpt3)),
     :                          %VAL(CNF_PVAL(wpt4)))
               call pgline(npix,%VAL(CNF_PVAL(wpt3)),
     :                     %VAL(CNF_PVAL(wpt4)))
               ixmin = max(nint(x-radius),1)
               ixmax = min(nint(x+radius),nx)
               iymin = max(nint(y-radius),1)
               iymax = min(nint(y+radius),ny)
               call addup(nx,ny,ixmin,ixmax,iymin,iymax,radius,x,y,
     :                    input,sum,apparea)
               iobj = iobj + 1
               write(unit,100) 'O',iobj,x,y,radius,sum,apparea
  100          format(3x,a,i3,3f8.2,2(1pg12.4))
            end if
C
C        S: Indicate a sky patch
C
         else if (optup .eq. 'S') then
            if (.not. picture) then
               call dsa_wruser('This might be a bit difficult without')
               call dsa_wruser(' a plot on the screen.  Try option')
               call dsa_wruser(' P first')
               call dsa_wrflush
            else
               call move_circle(npix,%VAL(CNF_PVAL(wpt1)),
     :                          %VAL(CNF_PVAL(wpt2)),x,y,
     :                          %VAL(CNF_PVAL(wpt3)),
     :                          %VAL(CNF_PVAL(wpt4)))
               call pgline(npix,%VAL(CNF_PVAL(wpt3)),
     :                     %VAL(CNF_PVAL(wpt4)))
               ixmin = max(nint(x-radius),1)
               ixmax = min(nint(x+radius),nx)
               iymin = max(nint(y-radius),1)
               iymax = min(nint(y+radius),ny)
               call addup(nx,ny,ixmin,ixmax,iymin,iymax,radius,x,y,
     :                    input,sum,apparea)
               isky = isky + 1
               write(unit,100) 'S',isky,x,y,radius,sum,apparea
               sumsky = sumsky + sum
            end if
C
C        C:  Show cuts in X and Y through the cursor point
C
         else if (optup .eq. 'C') then
            nbig = max(nx,ny)
            call dsa_get_work_array(nbig,'float',wpt5,wslot5,status)
            call dsa_get_work_array(nbig,'float',wpt6,wslot6,status)
            if (status .ne. 0) go to 500
            call plot_cut(nx,ny,nbig,input,x,y,%VAL(CNF_PVAL(wpt5)),
     :                    %VAL(CNF_PVAL(wpt6)),status)
            call dsa_free_workspace(wslot6,status)
            call dsa_free_workspace(wslot5,status)
            if (status .ne. 0) go to 500
            call pgbegin(0,softdev(:ich_len(softdev))//'/append',1,1)
            call pgask(.false.)
            call pgpoint( 1, 0.5, 0.5, -1 )
            call pgpage
            call display(work,nx,ny,low,high,xaxis,yaxis,table,status)
            if (status .ne. 0) go to 500
C
C        H:  Help
C
         else if (optup .eq. 'H' .or. optup .eq. '?') then
            call dsa_wrflush
            call dsa_wruser('You have a choice of the following:')
            call dsa_wrflush
            call dsa_wruser('  C:  Show x,y slices through a point')
            call dsa_wrflush
            call dsa_wruser('  H:  Help')
            call dsa_wrflush
            call dsa_wruser('  O:  Indicate an object')
            call dsa_wrflush
            call dsa_wruser('  P:  Renew the 2-d plot')
            call dsa_wrflush
            call dsa_wruser('  Q:  Quit analysis of current frame')
            call dsa_wrflush
            call dsa_wruser('  R:  Change radius for integration')
            call dsa_wrflush
            call dsa_wruser('  S:  Indicate a sky patch')
            call dsa_wrflush
            call dsa_wrflush
         end if
      end do      
      call pgend
      
      call dsa_free_workspace(wslot4,status)
      call dsa_free_workspace(wslot3,status)
      call dsa_free_workspace(wslot2,status)
      call dsa_free_workspace(wslot,status)
C
C     Average the sky values
C
      if (isky .ne. 0) then
         sumsky = sumsky/float(isky)
         write(unit,101) sumsky
  101    format(3x,'Mean sky for this field is:',1pg12.4)
         write(unit,102) ' '
  102    format(a1)
      end if
C
C     Reset init flag
C
      init = .false.
  500 end


      subroutine display(array,nx,ny,low,high,xaxis,yaxis,table,status)
C+-----------------------------------------------------------------------------
C     DISPLAY  ---  Do quickie 2-d image display
C
C     Parameters (">" Input, "<" Output, "W" Workspace, "!" Changed)
C
C     (>) ARRAY:     (integer array ARRAY(NX,NY)) Input array to be plotted.
C     (>) NX:        (integer) X dimension of data.
C     (>) NY:        (integer) Y dimension of data.
C     (>) LOW:       (real) Minimum value to plot
C     (>) HIGH:      (real) Maximum value to plot
C     (>) XAXIS:     (real array XAXIS(NX)) X axis data values
C     (>) YAXIS:     (real array YAXIS(NY)) Y axis data values
C     (>) TABLE:     (character) LUT name
C     (!) STATUS:    (integer) Status variable
C
C
C     AUTHOR:      Jim Lewis -- RGO
C     DATE:        05-SEP-1991 -- Cannibalised from many sources
C     UPDATE:
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer nx,ny,status
      integer array(nx,ny)
      real low,high,xaxis(nx),yaxis(ny)
      character table*(*)
C
C     Local variables
C
      integer spx2d(2),epx2d(2)
      real mag,sqvp,sta2d(2),end2d(2),ximv(2),yimv(2)
      character control*2
C
C     Set up boundries of map
C
      spx2d(1) = 1
      spx2d(2) = 1
      epx2d(1) = nx
      epx2d(2) = ny
C
C     Magnification and control variable
C
      mag = 1.0
      control = 'C '
C
C     Starting and ending points in axis values
C
      sta2d(1) = xaxis(1)
      sta2d(2) = yaxis(1)
      end2d(1) = xaxis(nx)
      end2d(2) = yaxis(ny)
C
C     Compute viewport for image
C
      call ndp_image_viewport(spx2d,epx2d,mag,control,ximv,yimv,sqvp)
C
C     Plot it
C
      control = 'AR'
      call ndp_image_plot(array,nx,ny,spx2d,epx2d,sta2d,
     :                    end2d,high,low,' ',control,ximv,yimv)
      call pgwindow(sta2d(1),end2d(1),sta2d(2),end2d(2))
  500 end


      subroutine cre_circle(npix,radius,x,y)
C+-----------------------------------------------------------------------------
C     CRE_CIRCLE -- Create arrays of a circle for plotting
C
C     Parameters (">" Input, "<" Output, "W" Workspace, "!" Changed)
C
C     (>) NPIX:      (integer) Size of the arrays
C     (>) RADIUS:    (real) Radius of the circle
C     (>) X:         (real array X(NPIX)) X values for circle
C     (>) Y:         (real array Y(NPIX)) Y values for circle
C
C
C     AUTHOR:      Jim Lewis -- RGO
C     DATE:        05-SEP-1991 
C     UPDATE:
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer npix
      real x(npix),y(npix),radius
C
C     Local variables
C
      integer i
      real pi,pinpix,theta
C
C     Set up a few things
C
      pi = 4.0*atan(1.0)
      pinpix = 2.0*pi/float(npix-1)
C
C     Now set up the circle
C
      do i = 1,npix
         theta = float(i-1)*pinpix
         x(i) = radius*cos(theta)
         y(i) = radius*sin(theta)
      end do
      end


      subroutine move_circle(npix,xin,yin,xoff,yoff,xout,yout)
C+-----------------------------------------------------------------------------
C     MOVE_CIRCLE -- Move the circle to a desired location
C
C     Parameters (">" Input, "<" Output, "W" Workspace, "!" Changed)
C
C     (>) NPIX:      (integer) Size of the arrays
C     (>) XIN:       (real array XIN(NPIX)) Input X values for circle
C     (>) YIN:       (real array YIN(NPIX)) Input Y values for circle
C     (>) XOFF:      (real) X offset
C     (>) YOFF:      (real) Y offset
C     (>) XOUT:      (real array XOUT(NPIX)) Output X values for circle
C     (>) YOUT:      (real array YOUT(NPIX)) Output Y values for circle
C
C
C     AUTHOR:      Jim Lewis -- RGO
C     DATE:        05-SEP-1991 
C     UPDATE:
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer npix
      real xin(npix),yin(npix),xoff,yoff,xout(npix),yout(npix)
C
C     Local variables
C
      integer i
C
C     Shift the circle
C
      do i = 1,npix
         xout(i) = xin(i) + xoff
         yout(i) = yin(i) + yoff
      end do
      end


      subroutine addup(nx,ny,ix1,ix2,iy1,iy2,radius,x,y,input,sum,
     :                 apparea)
C+----------------------------------------------------------------------
C     ADDUP  --- Add up the amount of signal in an aperture
C
C     Parameters (">" Input, "<" Output, "W" Workspace, "!" Changed)
C
C     (>) NX:          (integer) X dimension of the data
C     (>) NY:          (integer) Y dimension of the data
C     (>) IX1:         (integer) X lower limit for scan
C     (>) IX2:         (integer) X upper limit for scan
C     (>) IY1:         (integer) Y lower limit for scan
C     (>) IY2:         (integer) Y upper limit for scan
C     (>) RADIUS:      (real) Radius of the circle
C     (>) X:           (real) X centre of the circle
C     (>) Y:           (real) Y centre of the circle
C     (>) INPUT:       (real array INPUT(NX,NY)) Input frame
C     (<) SUM:         (real) Total amount of signal in the aperture
C     (<) APPAREA:     (real) Total area of aperture
C
C
C     AUTHOR:      Jim Lewis -- RGO
C     DATE:        05-SEP-1991
C     UPDATE:
C
C+----------------------------------------------------------------------
      implicit none
C
C     Parameters
C  
      integer nx,ny,ix1,ix2,iy1,iy2
      real radius,x,y,input(nx,ny),sum,apparea
C
C     Local variables
C 
      integer i,j,status
      real dy2,dx2,rad,magic_float
C
C     Magic value
C
      call dsa_get_flag_value('float',magic_float,status)
C
C     Add up the contributions from each pixel
C
      sum = 0.0
      apparea = 0.0
      do j = iy1,iy2
         dy2 = (float(j) - y)**2
         do i = ix1,ix2
            if (input(i,j) .ne. magic_float) then
               dx2 = (float(i) - x)**2
               rad = sqrt(dx2 + dy2)
               if (rad .le. radius) then
                  sum = sum + input(i,j)
                  apparea = apparea + 1.0
               end if
            end if
         end do
      end do
      end


      subroutine plot_cut(nx,ny,nw,input,x,y,work1,work2,status)
C+----------------------------------------------------------------------
C     PLOT_CUT -- Plot horizontal and vertical cuts through a point
C
C     Parameters (">" Input, "<" Output, "W" Workspace, "!" Changed)
C
C     (>) NX:          (integer) X dimension of the data
C     (>) NY:          (integer) Y dimension of the data
C     (>) NW:          (integer) Workspace dimensions
C     (>) INPUT:       (real array INPUT(NX,NY)) Input frame
C     (>) X:           (real) X centre of the circle
C     (>) Y:           (real) Y centre of the circle
C     (W) WORK1:       (real array WORK1(NW)) Workspace
C     (W) WORK2:       (real array WORK2(NW)) Workspace
C     (!) STATUS:      (integer) Status variable
C
C
C     AUTHOR:      Jim Lewis -- RGO
C     DATE:        05-SEP-1991
C     UPDATE:
C
C+----------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer nx,ny,nw
      real input(nx,ny),x,y,work1(nw),work2(nw)
C
C     Functions
C
      integer ich_len
C
C     Local variables
C
      integer status,iy,i,ix
      real amin,amax
      character softdev*20
      logical init, ignore
C
      data init /.true./
C
C     Get current PGPLOT screen device on initial entry
C
      if (init) then
         call var_getchr('idev',0,0,softdev,status)
         if (status .ne. 0) then
            call dsa_wruser
     :      ('No screen device selected -- use IDEV command')
            call dsa_wrflush
            go to 500
         end if
      end if
      call pgbegin(0,softdev(:ich_len(softdev))//'/append',1,2)
      call pgask(.false.)
      call pgpoint( 1, 0.5, 0.5, -1 )
      call pgpage
      call pgpoint( 1, 0.5, 0.5, -1 )
      call pgpage
C
C     Get cut in X
C
      iy = nint(y)
      amin = input(1,iy)
      amax = amin
      do i = 1,nx
         work1(i) = float(i)
         work2(i) = input(i,iy)
         amin = min(amin,work2(i))
         amax = max(amax,work2(i))
      end do
      amax = 0.3*abs(amax) + amax
C
C     Plot this up
C
      call pgenv(work1(1),work1(nx),amin,amax,0,0)
      call pgline(nx,work1,work2)
      call pglabel('X','Counts',' ')
C
C     Get cut in Y
C
      ix = nint(x)
      amin = input(ix,1)      
      amax = amin
      do i = 1,ny
         work1(i) = float(i)
         work2(i) = input(ix,i)
         amin = min(amin,work2(i))
         amax = max(amax,work2(i))
      end do
      amax = 0.3*abs(amax) + amax
C
C     Plot this up
C
      call pgenv(work1(1),work1(ny),amin,amax,0,0)
      call pgline(ny,work1,work2)
      call pglabel('Y','Counts',' ')
      call pgend
C
C     Give the user some time to look at this before it is erased
C
C     call dsa_wruser('Hit <return> to restore 2-d plot')
C     call dsa_wrflush
C     call par_rduser(dummy,status)
      call par_cnpar('ok')
      call par_rdkey('ok',.true.,ignore)
  500 end

      subroutine ndp_image_index(nelm,low,high,input,badpix,output,
     :                           status)
C+-----------------------------------------------------------------------------
C
C   -----------------------------
C   N D P _ I M A G E _ I N D E X
C   -----------------------------
C
C   Description
C   -----------
C   Translate a real array into an array of integer colour indices.
C
C
C   Parameters
C   ----------
C   NELM     (> integer) Number of elements in array
C   LOW      (> real). Lowest data value to be displayed.
C   HIGH     (> real). Highest data value to be displayed.
C   INPUT    (> real array) Input array
C   BADPIX   (> logical) Flag for magic values.
C   OUTPUT   (< integer array) Output array
C   STATUS   (! integer) Status variable
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library NDP:
C     NDP_DEVICE_INDEX
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   21-JAN-1992   - Original program
C
C
C+-----------------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer nelm,status
      integer output(nelm)
      real low,high,input(nelm)
      logical badpix
C
C     Local variables
C
      integer ci_start,ci_end,ncol,lev,i
      real rat,magic_float,dat
C
C     Inherited status
C
      if (status .ne. 0) go to 500
C
C     Magic value
C
      call dsa_get_flag_value('float',magic_float,status)
C
C     Number of colours
C
      call ndp_device_index(ci_start,ci_end,status)
      if (status .ne. 0) go to 500
      ncol = ci_end - ci_start + 1
C
C     A useful constant
C
      rat = float(ncol)/(high - low)
C
C     Rescale now
C
      if (.not. badpix) then
         do i = 1,nelm
            lev = nint((input(i) - low)*rat) + ci_start
            lev = max(ci_start,lev)
            lev = min(ci_end,lev)
            output(i) = lev
         end do
      else
         do i = 1,nelm
            dat = input(i)
            if (dat .eq. magic_float) then
               lev = ci_start
            else
               lev = nint((dat - low)*rat) + ci_start
               lev = max(ci_start,lev)
               lev = min(ci_end,lev)
            end if
            output(i) = lev
         end do
      end if
C
C     Exit
C
  500 continue
      end

      subroutine ndp_image_plot
     &  (array,nx,ny,stapix,endpix,start,end,high,low,label,control,
     &   ximv,yimv)
C+----------------------------------------------------------------------
C
C   ---------------------------
C   N D P _ I M A G E _ P L O T
C   ---------------------------
C
C   Description
C   -----------
C   Displays an image on the current PGPLOT device. High and low data
C   values may be selected. Calibrated axes and a title may be plotted.
C   A ramp of the LUT may be displayed to the right of the image.
C
C
C   Parameters
C   ----------
C   ARRAY    (> real array). Image data array.
C   NX       (> integer) Array X dimension.
C   NY       (> integer) Array Y dimension.
C   STAPIX   (> real array). Start pixel numbers being displayed.
C   ENDPIX   (> real array). End pixel numbers being displayed.
C   START    (> real array). Start axis values being displayed.
C   END      (> real array). End axis values being displayed.
C   HIGH     (> real). Highest data value to be displayed.
C   LOW      (> real). Lowest data value to be displayed.
C   LABEL    (> character). Image label.
C   CONTROL  (> character). Option control characters.
C   XIMV     (> real array). X coordinates of viewport in inches.
C   YIMV     (> real array). Y coordinates of viewport in inches.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library NDP:
C     NDP_IMAGE_RAMP
C
C   PGPLOT:
C     PGBOX
C     PGLABEL
C     PGPIXL
C     PGVSIZE
C     PGWINDOW
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-JUN-1900   - Changed calling parameters so that ARRAY isn't 
C                   dimensions with elements of another array.  (JRL)
C   21-JAN-1992   - Input array is now an integer array of colour 
C                   indices. The original data should have been
C                   processed with NDP_IMAGE_INDEX.  Now makes use of 
C                   PGPIXL which give more control over colour 
C                   plotting. (JRL)
C
C
C+----------------------------------------------------------------------
c
      implicit none
c
c   Parameters.
c
      character*(*) label,control
      integer       stapix(2),endpix(2),nx,ny
      integer       array(nx,ny)
      real          start(2),end(2),high,low,ximv(2),yimv(2)
c
c   Local variables.
c
      logical       axes
      logical       ramp
c
c   Interpret control instructions.
c
      axes=index(control,'A').ne.0
      ramp=index(control,'R').ne.0
c
c   Define image viewport.
c
      call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
c
c   Set world coordinates to axis units.
c
      call pgwindow(start(1),end(1),start(2),end(2))
c
c   Plot image.
c
      call pgpixl(array,nx,ny,stapix(1),endpix(1),stapix(2),
     :            endpix(2),start(1),end(1),start(2),end(2))
c      call pgpixl(array,nx,ny,stapix(1),endpix(1),stapix(2),
c     :            endpix(2),start(1),end(1),end(2),start(2))
c
c   Plot axes if required.
c
      if(axes)then
        call pgbox('BCINST',0.0,0,'BCINST',0.0,0)
      else
        call pgbox('BC',0.0,0,'BC',0.0,0)
      end if
c
c   Plot label.
c
      call pglabel(' ',' ',label)
c
c   Plot colour or grey scale ramp if required. Afterwards restore the 
c   image viewport.
c
      if(ramp)then
        call ndp_image_ramp(ximv,yimv,low,high)
        call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
      end if
c
      end

      subroutine ndp_image_viewport
     &  (stapix,endpix,mag,control,ximv,yimv,square)
C+
C
C   -----------------------------------
C   N D P _ I M A G E _ V I E W P O R T
C   -----------------------------------
C
C   Description
C   -----------
C   Sets the PGPLOT viewport for image display. The image size may be 
C   selected as a fraction of the whole display durface. The location on
C   the surface may be any combination of Top/Centre/Bottom paired with 
C   Left/Centre/Right, e.g. 'TL' is top left. A central position is 
C   always returned unless otherwise specified, e.g. 'L' gives left of 
C   screen, vertically centred.
C
C
C   Parameters
C   ----------
C   STAPIX  (> integer array). Start pixel numbers to be displayed.
C   ENDPIX  (> integer array). End pixel numbers to be displayed.
C   MAG     (> real). Magnification. 1.0 = fit to full display surface.
C   CONTROL (> character). Option control characters.
C   XIMV    (< real array). X coordinates of image viewport in inches.
C           (coords of image area only, does not include axes and titles).
C   YIMV    (< real array). Y coordinates of image viewport in inches.
C           (coords of image area only, does not include axes and titles).
C   SQUARE  (< real). Size of largest possible square viewport in inches
C           (size of image area only, does not include axes and titles).
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library ICH:
C     ICH_LEN
C
C   Starlink PGPLOT:
C     PGQVP
C     PGVSTAND
C
C
C   INCLUDE statements
C   ------------------
C   None.
C                                                
C
C   Extensions to FORTRAN77
C   -----------------------
C   IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C
C
C+----------------------------------------------------------------------
c
      implicit none
c
      character*(*) control
      integer       stapix(2),endpix(2)
      real          mag,ximv(2),yimv(2),square
c                   
      logical       bottom
      logical       left
      integer       ncol
      integer       nrow
      real          ratio
      logical       right
      logical       rlcent
      logical       tbcent
      logical       top
      real          xmax
      real          xmax1
      real          xmax2
      real          xmin
      real          xmin1
      real          xmin2
      real          ymax
      real          ymax1
      real          ymax2
      real          ymin
      real          ymin1
      real          ymin2
c
c   Default is right-left and top-bottom centering.
c
      rlcent=.true.      
      tbcent=.true.      
c
c   Interpret control instructions.
c
      right=index(control,'R').ne.0
      left=index(control,'L').ne.0
      top=index(control,'T').ne.0
      bottom=index(control,'B').ne.0
c
c   Work out whether right-left or top-bottom centering is required.
c
      if(right.or.left)then
        rlcent=.false.
      end if
      if(top.or.bottom)then
        tbcent=.false.
      end if
c
c   Compute device aspect ratio.
c
      call pgvstand
      call pgqvp(1,xmin,xmax,ymin,ymax)
      ratio=(ymax-ymin)/(xmax-xmin)            
c       
c   Compute location of largest possible square.
c
      xmin1=xmin
      xmax1=xmax
      ymin1=ymin
      ymax1=ymax
c
      if(ratio.lt.1.0)then
        if(rlcent)then
          xmin1=xmin+0.5*(xmax-ymax)
        else if(right)then
          xmin1=xmin+(xmax-ymax)
        else if(left)then
          xmin1=xmin
        end if
        xmax1=xmin1+(ymax-ymin)
        ymin1=ymin
        ymax1=ymax
        square=ymax
      else if(ratio.gt.1.0)then
        if(tbcent)then
          ymin1=ymin+0.5*(ymax-xmax)
        else if(top)then
          ymin1=ymin+(ymax-xmax)
        else if(bottom)then
          ymin1=ymin
        end if
        xmin1=xmin
        xmax1=xmax
        ymax1=ymin1+(xmax-xmin)
        square=xmax
      end if
c
c   Compute image aspect ratio.
c
      ncol=endpix(1)-stapix(1)+1
      nrow=endpix(2)-stapix(2)+1
      ratio=real(nrow)/real(ncol)
c
c   Adjust for image aspect ratio.
c
      xmin2=xmin1
      xmax2=xmax1
      ymin2=ymin1
      ymax2=ymax1
c
      if(ratio.gt.1.0)then
        if(right)then
          xmin2=xmin1+(ymax1-ymin1)*(1.0-1.0/ratio)
          xmax2=xmax1
        else if(left)then
          xmin2=xmin1
          xmax2=xmax1-(ymax1-ymin1)*(1.0-1.0/ratio)
        else if(rlcent)then
          xmin2=xmin1+0.5*(ymax1-ymin1)*(1.0-1.0/ratio)
          xmax2=xmax1-0.5*(ymax1-ymin1)*(1.0-1.0/ratio)
        end if
        ymin2=ymin1
        ymax2=ymax1
      else if(ratio.lt.1.0)then
        if(top)then
          ymin2=ymin1+(xmax1-xmin1)*(1.0-ratio)
          ymax2=ymax1
        else if(bottom)then
          ymin2=ymin1
          ymax2=ymax1-(xmax1-xmin1)*(1.0-ratio)
        else if(tbcent)then
          ymin2=ymin1+0.5*(xmax1-xmin1)*(1.0-ratio)
          ymax2=ymax1-0.5*(xmax1-xmin1)*(1.0-ratio)
        end if
        xmin2=xmin1
        xmax2=xmax1
      end if
c
c   Adjust for required magnification.
c
      ximv(1)=xmin2
      ximv(2)=xmax2
      yimv(1)=ymin2
      yimv(2)=ymax2
c      
      if(right)then
        ximv(1)=xmin2+(xmax2-xmin2)*(1.0-mag)
        ximv(2)=xmax2
      else if(left)then
        ximv(1)=xmin2
        ximv(2)=xmax2-(xmax2-xmin2)*(1.0-mag)
      else if(rlcent)then
        ximv(1)=xmin2+0.5*(xmax2-xmin2)*(1.0-mag)
        ximv(2)=xmax2-0.5*(xmax2-xmin2)*(1.0-mag)
      end if
c
      if(top)then
        yimv(1)=ymin2+(ymax2-ymin2)*(1.0-mag)
        yimv(2)=ymax2
      else if(bottom)then
        yimv(1)=ymin2
        yimv(2)=ymax2-(ymax2-ymin2)*(1.0-mag)
      else if(tbcent)then
        yimv(1)=ymin2+0.5*(ymax2-ymin2)*(1.0-mag)
        yimv(2)=ymax2-0.5*(ymax2-ymin2)*(1.0-mag)
      end if
c
      end

      subroutine ndp_device_index(ci_start,ci_end,status)
C+----------------------------------------------------------------------
C
C   -------------------------------
C   N D P _ D E V I C E _ I N D E X
C   -------------------------------
C
C   Description
C   -----------
C   Gets range of colour indices allowed for a PGPLOT device.
C
C
C   Parameters
C   ----------
C   CI_START (< integer) First colour index
C   CI_END   (< integer) Last colour index
C   STATUS   (! integer) Status variable
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_WRUSER
C
C   PGPLOT:
C     PGQCOL
C     PGQINF
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   21-JAN-1992   - Original program
C   03-FEB-1992   - Now checks that a device is open first (JRL)
C   24-AUG-1994   - Reserve 16 pens instead of four. (hme)
C
C
C+----------------------------------------------------------------------
      implicit none
C
C     Parameters
C
      integer ci_start,ci_end,status
C
C     Local variables
C
      integer mincol,maxcol,ncol,length
      character pgstate*80
C
C     Inherited status
C
      if (status .ne. 0) go to 500
C
C     Check that a device is open
C
      call pgqinf('state',pgstate,length)
      if (pgstate .ne. 'OPEN') then
         call dsa_wrflush
         call dsa_wruser('No PGPLOT device open -- programming error')
         call dsa_wrflush
         call dsa_wrflush
         status = 1
         go to 500
      end if
C
C     Number of colours
C
      call pgqcol(mincol,maxcol)
C
C     If you can write in the background colour then subtract an extra
C     colour index.
C
      if (mincol .eq. 0) then
         ncol = maxcol - mincol - 1
      else
         ncol = maxcol - mincol
      end if
      if (ncol .lt. 2) then
         call dsa_wruser('Fewer than two colour levels are available')
         call dsa_wrflush
         status = 1
         go to 500
      end if
C
C     Now set up the colour index range
C
      ci_start = 16
      ci_end = maxcol
C
C     Exit
C
  500 continue
      end

      subroutine ndp_image_ramp(ximv,yimv,low,high)

C+  NDP_IMAGE_RAMP - plot an intensity  ramp
C
C   Description
C   -----------
C   Plots a ramp or bar of the current lookup table to the right of the
C   current image viewport.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Starlink PGPLOT:
C     PGBOX
C     PGDRAW
C     PGMOVE
C     PGPIXL
C     PGQCOL
C     PGVSIZE
C     PGWINDOW
C
C   Library NDP:
C     NDP_DEVICE_INDEX
C     NDP_IMAGE_INDEX
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  
C   Guy Rixon    RGO  
C   Jim Lewis    RGO
C
C
C   History
C   -------
C   01-FEB-1989   - Original program. (NMJF)
C   15-JAN-1990   - Now gets colour range using PGQCOL. No change to
C                   functionality or calling sequence. (GTR)
C   21-JAN-1992   - Changed to use PGPIXL instead of PGGRAY. (JRL)
C
C
C+----------------------------------------------------------------------
c
      implicit none

c    Given parameters:
c
      real          
     :   ximv(2), yimv(2),  ! limits of imge viewport
     :   low, high          ! extreme data-values to be plotted
c                   
c    local constants:
c
      integer
     :   xdim, ydim         ! size of ramp array
      parameter ( xdim=2, ydim=256 )
c
c    Local variables:
c
      integer       
     :   cispan,            ! number of colour values to be set
     :   i, j,              ! loop indices
     :   iarray(xdim,ydim), ! colour index array
     :   ci_start,ci_end,   ! colour index range
     :   status             ! status variable
      real          
     :   rarray(xdim,ydim), ! array of ramp values
     :   xmax, xmin,        ! limits of the ramp viewport
     :   val                ! dummy variable


c
c    Set the ramp viewport. The ramp will appear at the right of the 
c    image.
c
      xmin=ximv(2)+0.025*(ximv(2)-ximv(1))
      xmax=xmin+0.05*(ximv(2)-ximv(1))
      call pgvsize(xmin,xmax,yimv(1),yimv(2))
c
c    Set world coordinates in Y to the image data range.
c
      call pgwindow(1.0,2.0,low,high)
c
c    Fill the ramp array with the image data range interpolated over
c    the available colour indices.
c
      call ndp_device_index(ci_start,ci_end,status)
      if (status .ne. 0) go to 500
      cispan = ci_end - ci_start + 1
      do j=1,cispan
         val = low + (real(j-1)/cispan)*(high-low)
         do i=1,2
            rarray(i,j) = val
         end do
      end do
      call ndp_image_index(xdim*ydim,low,high,rarray,.false.,iarray,
     :                     status)
      if (status .ne. 0) go to 500
c
c    Plot the ramp array.
c
      call pgpixl(iarray,xdim,ydim,1,xdim,1,cispan,1.0,float(xdim),
     :            low,high)
c
c    Plot box with annotation on right side only.
c
      call pgbox('C',0.0,0,'CIMSTV',0.0,0)
      call pgmove(1.0,high)
      call pgdraw(1.0,low)
      call pgdraw(2.0,low)
c
  500 continue
      end

