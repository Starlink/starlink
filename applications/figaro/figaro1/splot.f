C+
      SUBROUTINE SPLOT
C
C     S P L O T    /    E S P L O T
C
C     Produces a plot of a spectrum.  The plot is directed
C     to the device defined by the user variables 'SOFT' and
C     'HARD', and by the value of the command keyword 'HARDCOPY',
C     so will appear immediately if these specify a video
C     device (VT125, Args, etc.).  If a hardcopy device
C     is specified, the file for that device will be produced,
C     but SPLOT does not attempt to spool it off for printing.
C
C     ESPLOT is similar to SPLOT, but plots error bars based on the
C     errors in the data.
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
C     HIGH        The maximum value to be used for the plot.
C     LOW         The minimum value to be used for the plot.
C     BIAS        A value used to displace the plot - BIAS is
C                 effectively a value added to the data before
C                 it is plotted. (It is implemented as a value
C                 subtracted from both HIGH and LOW.)
C                 (HIGH,LOW and BIAS are not required if the
C                 AUTOSCALE keyword is specified.)
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
C     AUTOSCALE   The program is to work out the values for HIGH
C                 and LOW, using the maximum and minimum values
C                 in the data over the specified range.
C     WHOLE       The program is to display all of the spectrum.
C     HARDCOPY    The plot is to produce a hard copy.
C     AXES        Axes will be plotted.
C     ERASE       The screen will be erased before the plot.
C     LINES       The plot is not done as a histogram, but as
C                 a 'join the dots' line plot.  (Only applies
C                 to SPLOT.)
C
C     User variables -    (">" input, "<" output)
C
C     (>) SOFT    Specifies the device and type to be used for soft
C                 plots.  See the SOFT command for more details.
C     (>) HARD    Specifies the device and type to be used for hard
C                 plots.  See the HARD command for more details.
C     (<) TVXST   is set to the starting x-value for the plot.
C     (<) TVXEN   Is set to the final x-value for the plot.
C     (<) TVHIGH  Is set to the same value as HIGH.
C     (<) TVLOW   Is set to the same value as LOW.
C     (<) TVFILE  Is set to the value of SPECTRUM.
C     (<) TVCOLOR Is set to the GRPCKG code for the plot colour.
C                 (The TV.. variables are intended for use by
C                 cursor routines, and reflect the settings for the
C                 last plot made, even if XSTART etc are changed.)
C
C     (Other user variables may be set by the command processor, in
C     connection with the parameter values.)
C
C                                         KS / CIT  30th April 1984
C
C     Modified:
C
C     14 Jun 1985  KS / AAO.  ESPLOT added.
C     12 Aug 1985  KS / AAO.  Modified to expect the errors array
C                  to contain percentage rather than absolute values.
C     22 Jul 1986  KS / AAO.  Reverts to absolute error values.
C     04 Jun 1987  KS / AAO.  Main routine re-written to use the
C                  new DSA_ package.
C     09 Jun 1987  KS / AAO.  High limit in call to PAR_RDVAL for 'LOW'
C                  corrected to make work with very low valued data.
C     15 Jul 1987  KS / AAO.  Revise DSA_ routine calls - specs for some
C                    have changed.
C     21 Jul 1987  KS / AAO.  Modify dynamic memory handling.  Now uses
C                  DYN_ routines.
C     09 Sep 1988  KS / AAO.  Add PAR_ABORT calls to support user
C                  requested aborts.
C     13 Dec 1990  JAB / JAC.  Handle data quality.
C     25 Jan 1991  JMS/AAO & JAB/JAC. AAO recent modifications (bug
C                  fixes: PAR_ABORT calls added, BUILD code corrected)
C                  merged with data quality support added by JAB at
C                  UKIRT.  PAR_ABORTS and STATUS checks added.
C     31 Jan 1991  JMS / AAO. Exits program if number of good data
C                  points is fewer than or equal to one.
C     10 Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Eliminate
C                  DSK_ package (and the build option).
C                  Call PAR_WRUSER rather than DSA_WRUSER.
C     07 Apr 1993  HME / UoE, Starlink.  Change WHOLE to reset to
C                  .TRUE. Thus "splot reset" will plot the whole
C                  range, even though XSTART/XEND are global.
C     11 Jan 1995  HME / UoE, Starlink. Passive AGI compliance,
C                  use FIG_PGBEG/END.
C     23 Jan 1995  HME / UoE, Starlink. Increase TVFILE to *132.
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap mapping x data
C                  behind getting x axis range.
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
C     Real variable plot value limits - keeps plot range within
C     the VAX real number limits.
C
      REAL PLMAX,PLMIN
      PARAMETER (PLMAX=1.0E36,PLMIN=-1.0E36)
C
C     Floating point limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.0E38,FMIN=-1.0E38)
C
C     Local variables
C
      LOGICAL   AUTOSC           ! True if AUTOSCALE specified
      LOGICAL   AXES             ! True if axes to be plotted
      REAL      BIAS             ! Value specified for BIAS
      LOGICAL   BUILD            ! True if BUILD specified
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
      LOGICAL   ERASE            ! True if ERASE specified
      LOGICAL   ERRUSE           ! True if errors to be used
      INTEGER   ESLOT            ! Map slot used for error data
      LOGICAL   HARD             ! True if HARD specified
      REAL      HIGH             ! Maximum Y-value for plot
      INTEGER   IGNORE           ! Used for disregarded status codes
      INTEGER   INVOKE           ! Used to invoke functions
      INTEGER   IXEN             ! Last element to be plotted
      INTEGER   IXST             ! First integer to be plotted
      LOGICAL   LINES            ! True if LINES specified
      REAL      LOW              ! Maximum Y-value for plot
      DOUBLE PRECISION MAGNITUDE ! Mganitude flag value for data
      INTEGER   NDIM             ! Dimensionality of input spectrum
      INTEGER   NELM             ! Number of elements in data - ignored
      INTEGER   NEXT             ! ICH_KEY argument - ignored
      INTEGER   NGOOD            ! Number of good points in spectrum
      INTEGER   NX               ! Number of elements in data
      CHARACTER PLAB*64          ! Label for plot
      INTEGER   QPTR             ! Dynamic memory pointer to quality
      INTEGER   QSLOT            ! Map slot used for quality
      CHARACTER SPECT*132        ! Actual name of spectrum
      LOGICAL   SPLO             ! True if command is SPLOT
      INTEGER   STATUS           ! Status return from DSA_ routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      INTEGER   THICK            ! Line thickness for plot
      INTEGER   TDPTR            ! Temporary Data pointer
      INTEGER   TEPTR            ! Temporary Error pointer
      INTEGER   TXPTR            ! Temporary X pointer
      INTEGER   TSLOT            ! Temporary slot
      REAL      VALUE            ! Temporary real
      REAL      VMAX             ! Maximum value in data array
      REAL      VMIN             ! Minimum value in data array
      LOGICAL   WHOLE            ! True if WHOLE specified
      CHARACTER XLAB*64          ! X-axis label for plot
      CHARACTER XLABEL*32        ! Structure x-axis label
      INTEGER   XPTR             ! Dynamic memory pointer to X-axis data
      INTEGER   XSLOT            ! Map slot used for X-axis data
      CHARACTER XUNITS*32        ! Structure X-axis units
      REAL      XVEN             ! Last X-axis value to be plotted
      REAL      XVST             ! First X-axis value to be plotted
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Find command being serviced
C
      CALL PAR_COMMAND(COMMAND)
      SPLO=COMMAND.EQ.'SPLOT'
C
C     Initial assumptions
C
      ERRUSE=.FALSE.
C
C     Open input spectrum.
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_USE_QUALITY('SPECT',STATUS)
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,NX,NELM,STATUS)
C
C     Try for X-axis information
C
      CALL DSA_GET_AXIS_INFO ('SPECT',1,2,STRINGS,0,DUMMY,STATUS)
      IF (STATUS.NE.0) GO TO 500
      XUNITS=STRINGS(1)
      XLABEL=STRINGS(2)
C
C     Get XSTART and XEND, unless WHOLE was
C     specified, in which case use all of the spectrum.
C
      CALL PAR_RDKEY('WHOLE',.TRUE.,WHOLE)
      IF (PAR_ABORT()) GOTO 500
      CALL DSA_AXIS_RANGE ('SPECT',1,'Unconstrained',WHOLE,XVST,XVEN,
     :                                              IXST,IXEN,STATUS)
C
C     Map the X-axis data array (will be 1..N if no such array).
C
      CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the spectrum data
C
      CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
C
C     Map the data quality
C
      CALL DSA_MAP_QUALITY ('SPECT','READ','BYTE',QPTR,QSLOT,STATUS)
C
C     For ESPLOT, try to map the error data
C
      IF (.NOT.SPLO) THEN
         CALL DSA_SEEK_ERRORS ('SPECT',ERRUSE,STATUS)
         IF (ERRUSE) THEN
            CALL DSA_MAP_ERRORS ('SPECT','READ','FLOAT',EPTR,
     :                           ESLOT,STATUS)
         END IF
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Are there any bad points? If so remove them.
C
      CALL FIG_SPLOT_NGOOD(NX,%VAL(CNF_PVAL(QPTR)),NGOOD)
C
C     If number of good points is just "1", then exit program since
C     SPLOT cannot plot just one data point in isolation.
C
      IF (NGOOD.EQ.1) THEN
         CALL PAR_WRUSER('There is only one good data point. '//
     :      'SPLOT cannot plot this in isolation.',STATUS)
         GOTO 500        !Exit
      ELSE IF (NGOOD.EQ.0) THEN
         CALL PAR_WRUSER('There are no good data points to plot.',
     :      STATUS)
         GOTO 500        !Exit
      END IF
C
      IF (NGOOD .NE. NX) THEN
          CALL DSA_GET_WORK_ARRAY(NGOOD,'FLOAT',TDPTR,TSLOT,STATUS)
          CALL DSA_GET_WORK_ARRAY(NGOOD,'FLOAT',TEPTR,TSLOT,STATUS)
          CALL DSA_GET_WORK_ARRAY(NGOOD,'FLOAT',TXPTR,TSLOT,STATUS)
          IF (STATUS .NE. 0) GO TO 500
          CALL FIG_SPLOT_REMOVE(NX,%VAL(CNF_PVAL(DPTR)),
     :                          %VAL(CNF_PVAL(EPTR)),
     :                          %VAL(CNF_PVAL(XPTR)),
     :                          %VAL(CNF_PVAL(QPTR)),ERRUSE,
     :                          NGOOD,%VAL(CNF_PVAL(TDPTR)),
     :                          %VAL(CNF_PVAL(TEPTR)),
     :                          %VAL(CNF_PVAL(TXPTR)),IXST,IXEN)

          NX=NGOOD
          DPTR=TDPTR
          EPTR=TEPTR
          XPTR=TXPTR
      END IF
C
C     Was AUTOSCALE specified?
C
      CALL PAR_RDKEY('AUTOSCALE',.FALSE.,AUTOSC)
      IF (PAR_ABORT()) GOTO 500
C
C     Specified or not, find out the scale range because we can
C     use that for the reset values.
C
      IF ((.NOT.SPLO).AND.ERRUSE) THEN
         CALL FIG_ERANGE(%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(EPTR)),
     :                   IXST,IXEN,VMAX,VMIN)
      ELSE
         CALL GEN_RANGEF(%VAL(CNF_PVAL(DPTR)),IXST,IXEN,VMAX,VMIN)
      END IF
C
C     Get Z data information (units and label)
C
      CALL DSA_GET_DATA_INFO ('SPECT',2,STRINGS,1,MAGNITUDE,STATUS)
      DUNITS=STRINGS(1)
      DLABEL=STRINGS(2)
C
C     If AUTOSCALE not specified, get values of HIGH and LOW
C
      IF (AUTOSC) THEN
         HIGH=VMAX+(VMAX-VMIN)*.10
         LOW=VMIN
         CALL PAR_SDVAL('HIGH',HIGH,IGNORE)
         CALL PAR_SDVAL('LOW',LOW,IGNORE)
         CALL PAR_SDVAL('BIAS',0.,IGNORE)
      ELSE
         CALL PAR_RDVAL('HIGH',FMIN,FMAX,VMAX,DUNITS,HIGH)
         CALL PAR_RDVAL('LOW',FMIN,HIGH,VMIN,DUNITS,LOW)
         CALL PAR_RDVAL('BIAS',FMIN,FMAX,0.,DUNITS,BIAS)
         HIGH=HIGH-BIAS
         LOW=LOW-BIAS
      END IF
      IF (HIGH.EQ.LOW) THEN
         IF (LOW.EQ.0.0) THEN
            HIGH=0.01
         ELSE
            HIGH=ABS(LOW)*1.1
         END IF
      END IF
C
C     Check for AXES and ERASE and LINES
C
      IF (STATUS.NE.0) GO TO 500
      CALL PAR_RDKEY('AXES',.TRUE.,AXES)
      CALL PAR_RDKEY('ERASE',.TRUE.,ERASE)
      IF (SPLO) CALL PAR_RDKEY('LINES',.FALSE.,LINES)
C
C     Get the label for the plot -
C
      CALL PAR_RDCHAR('LABEL',' ',PLAB)
C
C     Get the colour for the plot (note that BLACK and BLUE are reversed
C     in the colour list, so BL will be taken as BLUE - the codes are
C     then reversed to give the correct PGPLOT colour code)
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
C     Check for 'BUILD' or 'HARD'
C
      HARD=.FALSE.
      BUILD=.FALSE.
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
     :          'Use "HARD" command eg "HARD VER" to rectify.',IGNORE)
            CALL PAR_WRUSER('Will generate a "BUILD" plot instead',
     :                                                         IGNORE)
            HARD=.FALSE.
            BUILD=.TRUE.
         END IF
      ELSE
C
C        Look for the value of the user variable 'SOFT'
C
         CALL VAR_GETCHR('SOFT',0,0,DEVICE,DSTATUS)
         IF (DSTATUS.NE.0) THEN
            CALL PAR_WRUSER('No plotting device specified.',IGNORE)
            CALL PAR_WRUSER(
     :         'Use "SOFT" command eg "SOFT /VT" to rectify.',IGNORE)
            CALL PAR_WRUSER('Will generate a "BUILD" plot instead',
     :                                                        IGNORE)
            BUILD=.TRUE.
         END IF
      END IF
C
C     For 'BUILD' or 'HARD' get line thickness
C
      IF (BUILD.OR.HARD) THEN
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
      IF (MAGNITUDE.NE.0.0) THEN
         VALUE=HIGH
         HIGH=LOW
         LOW=VALUE
      END IF
C
C     Finally, perform the plot
C
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
      IF (SPLO) THEN
         CALL FIG_XZPLOT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),NX,
     :                   IXST,IXEN,HIGH,LOW,XLAB,DLAB,PLAB,AXES,ERASE,
     :                   LINES,DEVICE,BUILD,THICK,CKEY,XVST,XVEN,IGNORE)
      ELSE
         CALL FIG_XZEPLOT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),
     :                    %VAL(CNF_PVAL(EPTR)),NX,IXST,IXEN,HIGH,LOW,
     :                    XLAB,DLAB,PLAB,AXES,ERASE,ERRUSE,DEVICE,BUILD,
     :                    THICK,CKEY,XVST,XVEN,IGNORE)
      END IF
C
C     Set the user variables describing the plot.
C
      CALL DSA_GET_ACTUAL_NAME ('SPECT',SPECT,STATUS)
      CALL VAR_SETNUM('TVXST',0,0,XVST,IGNORE)
      CALL VAR_SETNUM('TVXEN',0,0,XVEN,IGNORE)
      CALL VAR_SETNUM('TVHIGH',0,0,HIGH,IGNORE)
      CALL VAR_SETNUM('TVLOW',0,0,LOW,IGNORE)
      CALL VAR_SETCHR('TVFILE',0,0,SPECT,IGNORE)
      CALL VAR_SETNUM('TVCOLOR',0,0,FLOAT(CKEY),IGNORE)
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_XZPLOT(XVALS,ZVALS,NX,IXST,IXEN,HIGH,LOW,
     :               XLAB,ZLAB,PLAB,AXES,ERASE,LINES,DEVICE,BUILD,
     :                              THICK,CKEY,XVST,XVEN,STATUS)
C
C     F I G _ X Z P L O T
C
C     Plots an array (ZVALS) against another array (XVALS).  It is
C     assumed that these map element for element, and that the XVALS
C     values represent the coordinates at the center of each 'bin'.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) XVALS    (Real array XVALS(NX) The abscissae
C                  for each point to be plotted.
C     (>) ZVALS    (Real array ZVALS(NX) The data to be plotted.
C     (>) NX       (Integer) Number of elements in XVALS and ZVALS.
C     (>) IXST     (Integer) The first array element to be plotted.
C     (>) IXEN     (Integer) The last array element to be plotted.
C     (>) HIGH     (Real) The maximum value for the plot.
C     (>) LOW      (Real) The minimum value for the plot.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) ZLAB     (Character) The Z-label for the plot.
C     (>) PLAB     (Character) The label for the plot as a whole.
C     (>) AXES     (Logical) True if axes are to be plotted.
C     (>) ERASE    (Logical) True if device is to be erased first.
C     (>) LINES    (Logical) True if plot is to be as a joined points
C                  plot rather than as a histogram.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot - see PGPLOT documentation for details.
C                  If BUILD is true, DEVICE is the filename to be
C                  used for the 'build' file.
C     (>) BUILD    (Logical) Indicates that a 'build' file is to
C                  be used. Should always be .FALSE.
C     (>) THICK    (Integer) The line thickness for the plot.
C     (>) CKEY     (Integer) The GRPCKG code (0..7) for the colour
C                  of the plot.
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error opening the plot.
C
C     Subroutines / functions used -
C
C     FIG_PGBEG  (PGPLOT package) Open a plot file.
C     PGBIN      (  "      "    ) Plot a histogram of data.
C     PGBOX      (  "      "    ) Draw the box for a plot.
C     PGADVANCE  (  "      "    ) Clear screen or start new plot.
C     PGWINDOW   (  "      "    ) Set the world-coordinate window.
C     PGVSTAND   (  "      "    ) Set the standard viewport.
C     FIG_PGEND  (  "      "    ) Terminate a plot.
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
C                                       KS / CIT  26th July 1984
C     Modified:
C
C     8th March 1988   KS / AAO.  Changed for the GKS version of PGPLOT.
C                      Use of /APPEND re-introduced, and references to
C                      GRPCKG routines changed to PGPLOT equivalents.
C     31st Aug. 1992.  HME / UoE, Starlink.  Eliminate DSK_ package (and
C                      the build option).
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL AXES,BUILD,ERASE,LINES
      INTEGER IXST,IXEN,NX,STATUS,THICK,CKEY
      REAL XVALS(NX),ZVALS(NX),HIGH,LOW,XVST,XVEN
      CHARACTER*(*) DEVICE,XLAB,ZLAB,PLAB
C
C     Functions
C
      INTEGER ICH_LEN,FIG_PGBEG
C
C     Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C     Local variables
C
      CHARACTER PGDEVL*64
C
C        Ordinary plot.  Open plot. If OK, plot data.
C
         IF (ERASE) THEN
            STATUS=FIG_PGBEG(0,DEVICE,1,1)
         ELSE
            PGDEVL=DEVICE(:ICH_LEN(DEVICE))//'/APPEND'
            STATUS=FIG_PGBEG(0,PGDEVL,1,1)
         END IF
         IF (STATUS.EQ.1) THEN
C
C           Setup plot environment, and plot data
C
            IF (ERASE) CALL PGADVANCE
            CALL PGSCI(WHITE)
            CALL PGSLW(THICK)
            CALL PGVSTAND
            CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
            IF (AXES) THEN
               CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
               CALL PGLABEL(XLAB,ZLAB,PLAB)
            ELSE
               CALL PGLABEL(' ',' ',PLAB)
            END IF
            CALL PGSCI(CKEY)
            IF (LINES) THEN
               CALL PGLINE(IXEN-IXST+1,XVALS(IXST),ZVALS(IXST))
            ELSE
               CALL PGBIN(IXEN-IXST+1,XVALS(IXST),ZVALS(IXST),
     :                                                   .TRUE.)
            END IF
            CALL PGSCI(WHITE)
            CALL PGSLW(1)
C
C           Close down plot
C
            CALL FIG_PGEND
            STATUS=0
         ELSE
            STATUS=1
         END IF
C
      END
C+
      SUBROUTINE FIG_XZEPLOT(XVALS,ZVALS,ERRORS,NX,IXST,IXEN,HIGH,LOW,
     :               XLAB,ZLAB,PLAB,AXES,ERASE,ERRUSE,DEVICE,BUILD,
     :                              THICK,CKEY,XVST,XVEN,STATUS)
C
C     F I G _ X Z E P L O T
C
C     Plots an array (ZVALS) against another array (XVALS), with errors
C     as specified by an error array (ERRORS). It is assumed that these
C     map element for element, and that the XVALS values represent the
C     coordinates at the center of each 'bin', and are in ascending order.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) XVALS    (Real array XVALS(NX) The abscissae
C                  for each point to be plotted.
C     (>) ZVALS    (Real array ZVALS(NX) The data to be plotted.
C     (>) ERRORS   (Real array ERRROS(NX)) The Y errors on the data.
C     (>) NX       (Integer) Number of elements in XVALS and ZVALS.
C     (>) IXST     (Integer) The first array element to be plotted.
C     (>) IXEN     (Integer) The last array element to be plotted.
C     (>) HIGH     (Real) The maximum value for the plot.
C     (>) LOW      (Real) The minimum value for the plot.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) ZLAB     (Character) The Z-label for the plot.
C     (>) PLAB     (Character) The label for the plot as a whole.
C     (>) AXES     (Logical) True if axes are to be plotted.
C     (>) ERASE    (Logical) True if device is to be erased first.
C     (>) ERRUSE   (Logical) True if ERRORS values are to be used.  If
C                  false, errors should be taken as zero.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot - see PGPLOT documentation for details.
C                  If BUILD is true, DEVICE is the filename to be
C                  used for the 'build' file.
C     (>) BUILD    (Logical) Indicates that a 'build' file is to
C                  be used. Should always be .FALSE.
C     (>) THICK    (Integer) The line thickness for the plot.
C     (>) CKEY     (Integer) The GRPCKG code (0..7) for the colour
C                  of the plot.
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error opening the plot.
C
C     Subroutines / functions used -
C
C     FIG_PGBEG  (PGPLOT package) Open a plot file.
C     PGBOX      (  "      "    ) Draw the box for a plot.
C     PGERRX     (  "      "    ) Plot error bars in X
C     PGERRY     (  "      "    ) Plot error bars in Y.
C     PGADVANCE  (  "      "    ) Clear screen or start new plot.
C     PGWINDOW   (  "      "    ) Set the world-coordinate window.
C     PGVSTAND   (  "      "    ) Set the standard viewport.
C     FIG_PGEND  (  "      "    ) Terminate a plot.
C     PGLABEL    (  "      "    ) Label a plot.
C     PGSCI      (  "      "    ) Set plot colour
C     PGSLW      (  "      "    ) Set line width
C     PAR_WRUSER (PAR_     "    ) Send a message to user.
C     ICH_LEN    (ICH_     "    ) Get length of string to trailing blanks
C
C     Note: the avoidance of PGENV predates the /APPEND feature of
C     PGPLOT, and was intended to give explicit control over whether
C     or not the screen was erased.
C                                       KS / CIT  14th June 1985
C     Modified:
C
C     12th Aug  1985.  KS / AAO.  Now expects ERRORS to be % values.
C     22nd July 1986.  KS / AAO.  Reverts to absolute error values.
C     8th March 1988   KS / AAO.  Changed for the GKS version of PGPLOT.
C                      Use of /APPEND re-introduced, and references to
C                      GRPCKG routines changed to PGPLOT equivalents.
C     31st Aug. 1992.  HME / UoE, Starlink.  Eliminate DSK_ package (and
C                      the build option).
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL AXES,BUILD,ERASE,ERRUSE
      INTEGER IXST,IXEN,NX,STATUS,THICK,CKEY
      REAL XVALS(NX),ZVALS(NX),ERRORS(NX),HIGH,LOW,XVST,XVEN
      CHARACTER*(*) DEVICE,XLAB,ZLAB,PLAB
C
C     Functions
C
      INTEGER ICH_LEN,FIG_PGBEG
C
C     Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C     Local variables
C
      INTEGER I
      REAL XVAL1, XVAL2, YVAL1, YVAL2
      CHARACTER PGDEVL*64
C
C        Ordinary plot.  Open plot. If OK, plot data.
C
         IF (ERASE) THEN
            STATUS=FIG_PGBEG(0,DEVICE,1,1)
         ELSE
            PGDEVL=DEVICE(:ICH_LEN(DEVICE))//'/APPEND'
            STATUS=FIG_PGBEG(0,PGDEVL,1,1)
         END IF
         IF (STATUS.EQ.1) THEN
C
C           Setup plot environment, and plot data
C
            IF (ERASE) CALL PGADVANCE
            CALL PGSCI(WHITE)
            CALL PGSLW(THICK)
            CALL PGVSTAND
            CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
            IF (AXES) THEN
               CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
               CALL PGLABEL(XLAB,ZLAB,PLAB)
            ELSE
               CALL PGLABEL(' ',' ',PLAB)
            END IF
            CALL PGSCI(CKEY)
            DO I=IXST,IXEN
               IF (I.GT.1) THEN
                  XVAL1=(XVALS(I)+XVALS(I-1))*0.5
               ELSE
                  XVAL1=XVALS(I)
               END IF
               IF (I.LT.NX) THEN
                  XVAL2=(XVALS(I)+XVALS(I+1))*0.5
               ELSE
                  XVAL2=XVALS(NX)
               END IF
               CALL PGERRX(1,XVAL1,XVAL2,ZVALS(I),0.0)
               IF (ERRUSE) THEN
                  YVAL1=ZVALS(I)-ERRORS(I)
                  YVAL2=ZVALS(I)+ERRORS(I)
                  CALL PGERRY(1,XVALS(I),YVAL1,YVAL2,0.0)
               END IF
            END DO
            CALL PGSCI(WHITE)
            CALL PGSLW(1)
C
C           Close down plot
C
            CALL FIG_PGEND
            STATUS=0
         ELSE
            STATUS=1
         END IF
C
      END


      SUBROUTINE FIG_SPLOT_NGOOD(NX,QUALITY,NGOOD)
C
C     Count the number of good points in a spectrum
C
      INTEGER NX,NGOOD
      BYTE QUALITY(NX)

      INTEGER IX

      NGOOD=0
      DO IX=1,NX
          IF (QUALITY(IX) .EQ. 0) NGOOD=NGOOD+1
      END DO
      END


      SUBROUTINE FIG_SPLOT_REMOVE(NX,D1,E1,X1,QUALITY,ERRUSE,NGOOD,
     :     D2,E2,X2,IXST,IXEN)
C
C     Make copy of spectrum with bad points removed
C
      IMPLICIT NONE
      INTEGER NX,NGOOD,IXST,IXEN
      REAL D1(NX),E1(NX),X1(NX),D2(NGOOD),E2(NGOOD),X2(NGOOD)
      BYTE QUALITY(NX)
      LOGICAL ERRUSE

      INTEGER I2,I1

      I2=1
      DO I1=1,NX
          IF (I1.EQ.IXST) IXST=I2
          IF (I1.EQ.IXEN) IXEN=I2
          IF (QUALITY(I1) .EQ. 0) THEN
              D2(I2)=D1(I1)
              X2(I2)=X1(I1)
              IF (ERRUSE) E2(I2)=E1(I1)
              I2=MIN(I2+1,NGOOD)
          END IF
      END DO
      END
