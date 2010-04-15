C+
      SUBROUTINE MSPLOT
C
C     M S P L O T
C
C     Produces a plot of a spectrum, splitting it up into a number of
C     sections, each plotted separately in a series down the plotting
C     surface.  The plots are directed to the device defined by the
C     user variables 'SOFT' and 'HARD', and by the value of the command
C     keyword 'HARDCOPY', so will appear immediately if these specify a
C     video device (VT125, Args, etc.).  If a hardcopy device is
C     specified, the file for that device will be produced, but MSPLOT
C     does not attempt to spool it off for printing.
C
C     Command parameters -
C
C     SPECTRUM    The data to be plotted.  If this contains X-axis
C                 information, this will be used.  If not, the X-axis
C                 will just have to be the numbers from 1 to n.
C     XSTART      The x-value at which plotting is to start.
C     XEND        The x-value at which plotting is to end.
C                 (XSTART and XEND are not required if the
C                 WHOLE keyword is specified.)
C     LABEL       A label for the plot.
C     COLOUR      The colour for the plot, assuming the display device
C                 supports it.  The axes are always white.
C     THICKNESS   The width of the lines used for the plot.  This is
C                 only used for 'hard' & 'build' plots, and should
C                 really be 1 for anything other than a high-resolution
C                 device like a Versatec or a laser printer.
C
C     Command keywords -
C
C     SAMESCALE   The program is to use a the same scale for all the
C                 plots (ie the scale is to be global).  Otherwise the
C                 plots will be autoscaled individually.  The global
C                 scale values are determined from the data range of
C                 the whole of the spectrum to be ploted.
C     SHOWZERO    If true, the autoscale values are constrained to
C                 include zero.
C     WHOLE       The program is to display all of the spectrum.
C     HARDCOPY    The plot is to produce a hard copy.
C     NEXT        Used to pause before a new page.
C
C     User variables -    (">" input, "<" output)
C
C     (>) SOFT    Specifies the device and type to be used for soft
C                 plots.  See the SOFT command for more details.
C     (>) HARD    Specifies the device and type to be used for hard
C                 plots.  See the HARD command for more details.
C
C     (Other user variables may be set by the command processor, in
C     connection with the parameter values.)
C
C                                         KS / AAO 15th Dec 1988
C
C     Modified:
C
C     03 Sep 1992  INCLUDE changed. The parameter SHOWZERO is neither
C                  in the ICL Figaro interface, nor documented. It is
C                  removed here and a value of .FALSE. used instead.
C                  PGASK is banned from ADAM, commented out.
C                  HME / UoE, Starlink.
C     25 Jan 1993  Put PGASK back in.  HME / UoE, Starlink.
C     27 Jul 1993  Disuse PAR_Q*. Added parameter NEXT.
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap mapping x data
C                  behind x axis range.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_CLEAN, ICH_FOLD, ICH_KEY
      REAL GEN_ELEMF
C
C     Floating point limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Note that this code contains much of the code needed to implement
C     an error bar version, except for the routine to actually do the
C     plotting.
C
C     Local variables
C
      INTEGER   CKEY             ! Colour code used by PGPLOT
      CHARACTER COLOUR*10        ! COLOUR specification
      CHARACTER COMMAND*16       ! Figaro command being executed
      CHARACTER DEVICE*32        ! PGPLOT device specification
      CHARACTER DLAB*64          ! Plot data axis label
      CHARACTER DLABEL*32        ! Label for data given in structure
      INTEGER   DPTR             ! Dynamic memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   DSTATUS          ! Status code for PGPLOT device names
      REAL      DUMMY            ! Dummy argument
      CHARACTER DUNITS*32        ! Data units given in structure
      INTEGER   EPTR             ! Dynamic memory pointer to error array
      LOGICAL   ERRUSE           ! True if errors to be used
      INTEGER   ESLOT            ! Map slot used for error data
      LOGICAL   FAULT            ! True if non DSA error occurs
      LOGICAL   HARD             ! True if HARD specified
      INTEGER   IGNORE           ! Used for disregarded status codes
      INTEGER   INVOKE           ! Used to invoke functions
      INTEGER   IXEN             ! Last element to be plotted
      INTEGER   IXST             ! First integer to be plotted
      DOUBLE PRECISION MAGNITUDE ! Magnitude flag value for data
      LOGICAL   MSPLO            ! True if command is MSPLOT
      INTEGER   NDIM             ! Dimensionality of input spectrum
      INTEGER   NELM             ! Number of elements in data - ignored
      INTEGER   NEXT             ! ICH_KEY argument - ignored
      INTEGER   NSPECT           ! Value of NSPECT parameter
      INTEGER   NX               ! Number of elements in data
      CHARACTER PLAB*64          ! Label for plot
      LOGICAL   REVPLOT          ! True if scales have to be reversed
      LOGICAL   SAMESCALE        ! True if SAMESCALE specified
      LOGICAL   SHOWZERO         ! True if SHOWZERO specified
      CHARACTER SPECT*80         ! Actual name of spectrum
      INTEGER   STATUS           ! Status return from DSA_ routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      INTEGER   THICK            ! Line thickness for plot
      REAL      VALUE            ! Temporary real
      REAL      VMAX             ! Maximum value in data array
      REAL      VMIN             ! Minimum value in data array
      LOGICAL   WHOLE            ! True if WHOLE specified
      CHARACTER XLAB*64          ! X-axis label for plot
      CHARACTER XLABEL*32        ! Structure x-axis label
      INTEGER   XPTR             ! Dynamic memory pointer to X-axis data
      INTEGER   XSLOT            ! Map slot used for X-axis data
      REAL      XRANGE           ! Range in X for a single plot
      CHARACTER XUNITS*32        ! Structure X-axis units
      REAL      XVEN             ! Last X-axis value to be plotted
      REAL      XVST             ! First X-axis value to be plotted
C
C     Initialisation of DSA_ routines
C
      FAULT=.FALSE.
      STATUS=0
      CALL DSA_OPEN (STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Find command being serviced
C
      CALL PAR_COMMAND(COMMAND)
      MSPLO=COMMAND.EQ.'MSPLOT'
C
C     Initial assumptions
C
      ERRUSE=.FALSE.
C
C     Open input spectrum.
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,NX,NELM,STATUS)
C
C     Try for X-axis information
C
      CALL DSA_GET_AXIS_INFO ('SPECT',1,2,STRINGS,0,DUMMY,STATUS)
      XUNITS=STRINGS(1)
      XLABEL=STRINGS(2)
C
C     Get XSTART and XEND, unless WHOLE was
C     specified, in which case use all of the spectrum.
C
      CALL PAR_RDKEY('WHOLE',.FALSE.,WHOLE)
      CALL DSA_AXIS_RANGE ('SPECT',1,'Unconstrained',WHOLE,XVST,XVEN,
     :                                              IXST,IXEN,STATUS)
C
C     Map the X-axis data array (will be 1..N if no such array).
C
      CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get the number of plots per page
C
      CALL PAR_RDVAL('NSPECT',1.,20.,5.,'Plots',VALUE)
      NSPECT=VALUE
      IF (PAR_ABORT()) GO TO 500    ! Abort requested
C
C     And get the x-axis increment for each.
C
      CALL PAR_RDVAL ('XRANGE',ABS(XVEN-XVST)*.001,ABS(XVEN-XVST)*2.,
     :                ABS(XVST-XVEN)/NSPECT,' ',XRANGE)
C
C     Map the spectrum data
C
      CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
C
C     For ESPLOT, try to map the error data
C
      IF (.NOT.MSPLO) THEN
         CALL DSA_SEEK_ERRORS ('SPECT',ERRUSE,STATUS)
         IF (ERRUSE) THEN
            CALL DSA_MAP_ERRORS ('SPECT','READ','FLOAT',EPTR,
     :                           ESLOT,STATUS)
         END IF
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C     How is the autoscaling to be done?
C
      CALL PAR_RDKEY('SAMESCALE',.TRUE.,SAMESCALE)
      SHOWZERO = .FALSE.
C
C     Get Z data information (units and label)
C
      CALL DSA_GET_DATA_INFO ('SPECT',2,STRINGS,1,MAGNITUDE,STATUS)
      DUNITS=STRINGS(1)
      DLABEL=STRINGS(2)
C
C     Get the label for the plot -
C
      CALL PAR_RDCHAR('LABEL',' ',PLAB)
C
C     Get the colour for the plot (note that BLACK and BLUE are reversed
C     in the colour list, so BL will be taken as BLUE - the codes are
C     then reversed to give the correct PGPLOT colour code).
C
      CALL PAR_RDCHAR('COLOUR','White',COLOUR)
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
      INVOKE=ICH_FOLD(COLOUR)
      INVOKE=ICH_CLEAN(COLOUR)
      CKEY=ICH_KEY(COLOUR,1,',; ',
     :      'BLUE:WHITE:RED:GREEN:BLACK:CYAN:MAGENTA:YELLOW:',
     :      'Abbr.',NEXT)-1
      IF (CKEY.EQ.0) THEN
         CKEY=4
      ELSE IF (CKEY.EQ.4) THEN
         CKEY=0
      END IF
      IF (CKEY.LT.0) THEN
         CALL PAR_WRUSER('Cannot colour the plot '//COLOUR,IGNORE)
         CKEY=1
      END IF
C
C     Check for 'HARD'
C
      CALL PAR_RDKEY('HARDCOPY',.FALSE.,HARD)
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
      IF (HARD) THEN
C
C        Look for the value of the user variable 'HARD'
C
         CALL VAR_GETCHR('HARD',0,0,DEVICE,DSTATUS)
         IF (DSTATUS.NE.0) THEN
            CALL PAR_WRUSER('No hardcopy device specified.',IGNORE)
            CALL PAR_WRUSER(
     :          'Use "HARD" command eg "HARD PRINTRONIX" to rectify.',
     :                                                         IGNORE)
            FAULT=.TRUE.
            GO TO 500       ! Error exit
         END IF
      ELSE
C
C        Look for the value of the user variable 'SOFT'
C
         CALL VAR_GETCHR('SOFT',0,0,DEVICE,DSTATUS)
         IF (DSTATUS.NE.0) THEN
            CALL PAR_WRUSER('No plotting device specified.',IGNORE)
            CALL PAR_WRUSER(
     :         'Use "SOFT" command eg "SOFT ARGS1" to rectify.',IGNORE)
            FAULT=.TRUE.
            GO TO 500       ! Error exit
         END IF
      END IF
C
C     For 'HARD' get line thickness
C
      IF (HARD) THEN
         CALL PAR_RDVAL('THICKNESS',1.,21.,1.,'Dots',VALUE)
         THICK=VALUE
      ELSE
         THICK=1
      END IF
C
C     Make sure there is a range of data to plot.
C
      IF (IXST.EQ.IXEN) THEN
         IXST=MAX(IXST-1,1)
         IXEN=MIN(IXEN+1,NX)
         XVST=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IXST)
         XVEN=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IXEN)
      END IF
C
C     Get the axis labels from the labels and units
C
      CALL FIG_MAKE_AXIS_LABEL(XLABEL,XUNITS,XLAB)
      CALL FIG_MAKE_AXIS_LABEL(DLABEL,DUNITS,DLAB)
C
C     See if the data should be plotted in reverse
C
      REVPLOT=(MAGNITUDE.NE.0.0)
C
C     Finally, perform the plot - there should be an error version
C     as well for the ELSE clause, but there isn't yet.
C
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
      IF (MSPLO) THEN
         CALL FIG_MXZPLOT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),
     :                    NX,IXST,IXEN,SAMESCALE,SHOWZERO,XLAB,DLAB,
     :                    PLAB,DEVICE,THICK,CKEY,REVPLOT,NSPECT,XRANGE,
     :                    HARD,XVST,XVEN,IGNORE)
         IF (PAR_ABORT()) GO TO 500   ! User requested abort
      END IF
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_MXZPLOT(XVALS,ZVALS,NX,IXST,IXEN,SAMESCALE,
     :               SHOWZERO,XLAB,ZLAB,PLAB,DEVICE,THICK,CKEY,REVPLOT,
     :                        NSPECT,XRANGE,HARD,XVST,XVEN,STATUS)
C
C     F I G _ M X Z P L O T
C
C     Plots an array (ZVALS) against another array (XVALS).  It is
C     assumed that these map element for element, and that the XVALS
C     values represent the coordinates at the center of each 'bin'.
C     The plot is split up into a number of sections, plotted in
C     a stack that goes down the plotting surface.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) XVALS    (Real array XVALS(NX) The abscissae
C                  for each point to be plotted.
C     (>) ZVALS    (Real array ZVALS(NX) The data to be plotted.
C     (>) NX       (Integer) Number of elements in XVALS and ZVALS.
C     (>) IXST     (Integer) The first array element to be plotted.
C     (>) IXEN     (Integer) The last array element to be plotted.
C     (>) SAMESCALE(Logical) True if the same scale is to be used for
C                  each of the individual plots.
C     (>) SHOWZERO (Logical) True if the plots are to be forced to show
C                  the zero level.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) ZLAB     (Character) The Z-label for the plot.
C     (>) PLAB     (Character) The label for the plot as a whole.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot - see PGPLOT documentation for details.
C                  If BUILD is true, DEVICE is the filename to be
C                  used for the 'build' file.
C     (>) THICK    (Integer) The line thickness for the plot.
C     (>) CKEY     (Integer) The GRPCKG code (0..7) for the colour
C                  of the plot.
C     (>) REVPLOT  (Logical) True if the vertical scale is to be reversed.
C     (>) NSPECT   (Integer) The number of plots to fit on one page.
C     (>) XRANGE   (Real) The range in X values to be plotted for each
C                  individual section.
C     (>) HARD     (Logical) True if the plot is to a hardcopy device.
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error opening the plot.
C
C     Subroutines / functions used -
C
C     PGBEGIN    (PGPLOT package) Open a plot file.
C     PGBIN      (  "      "    ) Plot a histogram of data.
C     PGBOX      (  "      "    ) Draw the box for a plot.
C     PGADVANCE  (  "      "    ) Clear screen or start new plot.
C     PGWINDOW   (  "      "    ) Set the world-coordinate window.
C     PGVSTAND   (  "      "    ) Set the standard viewport.
C     PGEND      (  "      "    ) Terminate a plot.
C     PGLABEL    (  "      "    ) Label a plot.
C     PGLINE     (  "      "    ) Plot data by joining points.
C     PGSCI      (  "      "    ) Set plot colour
C     PGSLW      (  "      "    ) Set line width
C     PAR_WRUSER (PAR_     "    ) Send a message to user.
C     ICH_LEN    (ICH_     "    ) Get length of string to trailing blanks
C
C     Note: the avoidance of PGENV predates the /APPEND feature of
C     PGPLOT, and was intended to give explicit control over whether
C     or not the screen was erased.
C                                       KS / AAO 15th Dec 1988
C     Modified:
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL SAMESCALE, SHOWZERO, HARD, REVPLOT
      INTEGER IXST,IXEN,NX,STATUS,THICK,NSPECT,CKEY
      REAL XVALS(NX),ZVALS(NX),XRANGE,XVST,XVEN
      CHARACTER*(*) DEVICE,XLAB,ZLAB,PLAB
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER GEN_BSEARCH,ICH_LEN,PGBEGIN
C
C     Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C     Local variables
C
      LOGICAL MORE,PQUEST
      INTEGER ISPECT,IPXST,IPXEN
      REAL HIGH,LOW,XPVST,XPVEN,TEMP
      CHARACTER TYPE*16
C
C     If same scale is to be used for all, then work out the values now.
C
      IF (SAMESCALE) THEN
         CALL GEN_RANGEF(ZVALS,IXST,IXEN,HIGH,LOW)
         IF (SHOWZERO) THEN
            IF (HIGH.LT.0.0) THEN
               HIGH=0.0
            ELSE
               LOW=0.0
            END IF
         END IF
         IF (REVPLOT) THEN
            TEMP=HIGH
            HIGH=LOW
            LOW=TEMP
         END IF
      END IF
C
C     Ordinary plot.  Open plot. If OK, plot data.
C
      STATUS=PGBEGIN(0,DEVICE,1,NSPECT)
      IF (STATUS.EQ.1) THEN
C
         ISPECT=0
         CALL PGASK(.FALSE.)
         MORE=.TRUE.
         XPVST=XVST
         XPVEN=XPVST+XRANGE
         DO WHILE(MORE)
C
C           Setup plot environment, and plot data
C
            ISPECT=ISPECT+1
            IF ((.NOT.HARD).AND.(ISPECT.GT.NSPECT)) THEN
               PQUEST=.FALSE.
               DO WHILE(.NOT.PQUEST)
                  CALL PAR_CNPAR('NEXT')
                  CALL PAR_RDKEY('NEXT',.TRUE.,PQUEST)
                  IF (PAR_ABORT()) THEN
                     CALL PGEND
                     RETURN
                  END IF
               END DO
               ISPECT=1
            END IF
            IPXST=GEN_BSEARCH(XVALS,NX,XPVST)
            IPXEN=GEN_BSEARCH(XVALS,NX,XPVEN)
            IF (IPXST.EQ.0) IPXST=1
            IF (IPXEN.EQ.0) IPXEN=NX
            IF (.NOT.SAMESCALE) THEN
               CALL GEN_RANGEF(ZVALS,IPXST,IPXEN,HIGH,LOW)
               IF (SHOWZERO) THEN
                  IF (HIGH.LT.0.0) THEN
                     HIGH=0.0
                  ELSE
                     LOW=0.0
                  END IF
               END IF
               IF (REVPLOT) THEN
                  TEMP=HIGH
                  HIGH=LOW
                  LOW=TEMP
               END IF
            END IF
            CALL PGADVANCE
            CALL PGSCI(WHITE)
            CALL PGSLW(THICK)
            CALL PGVSTAND
            CALL PGWINDOW(XPVST,XPVEN,LOW,HIGH)
            CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
            IF (ISPECT.EQ.1) THEN
               CALL PGLABEL(XLAB,ZLAB,PLAB)
            ELSE
               CALL PGLABEL(XLAB,ZLAB,' ')
            END IF
            CALL PGSCI(CKEY)
            CALL PGBIN(IPXEN-IPXST+1,XVALS(IPXST),ZVALS(IPXST),
     :                                                    .TRUE.)
            CALL PGSCI(WHITE)
            CALL PGSLW(1)
            XPVST=XPVEN
            XPVEN=XPVST+XRANGE
            MORE=XPVST.LT.XVEN
         END DO
C
C        Close down plot
C
         CALL PGEND
         STATUS=0
      ELSE
         STATUS=1
      END IF
C
      END
