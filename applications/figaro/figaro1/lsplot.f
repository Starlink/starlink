C+
      SUBROUTINE LSPLOT
C
C     L S P L O T    /    E L S P L O T
C
C     These are versions of SPLOT and ESPLOT that allow the size of
C     the plot to be specified.  LSPLOT produces a plot of a single
C     spectrum, while ESPLOT produces an error bar plot of a spectrum
C     which has error information.
C
C     Command parameters -
C
C     XSIZE       The size of the plot in X, in metres.
C     YSIZE       The size of the plot in Y, in metres.
C     SPECTRUM    The data to be plotted.  If there
C                 is an x-axis data component this will be used to
C                 give the x-axis.  If not, the x-axis will just
C                 have to be the numbers from 1 to n.
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
C
C     Command keywords -
C
C     AUTOSCALE   The program is to work out the values for HIGH
C                 and LOW, using the maximum and minimum values
C                 in the data over the specified range.
C     WHOLE       The program is to display all of the spectrum.
C     LINES       The plot is not done as a histogram, but as
C                 a 'join the dots' line plot.  (LSPLOT only)
C
C     User variables used:
C
C     HARD        (Character) The device used for HARD plots.
C
C     Note:
C
C     The original version of LSPLOT used GKS 6.2 and the DIAGRAM
C     package.  This has now been discontinued, and some of the
C     functionality of DIAGRAM (the ability to specify the size of
C     the plot in physical units) has appeared in PGPLOT.  This new
C     version uses PGPLOT.  It can produce a plot of the specified
C     size, BUT only if that size is SMALLER than the default size
C     for the device.  In practice, this means that it can only work
C     in the way it was intended with `unusual' devices that have
C     particularly large default plot sizes (which often need to be set
C     up specially for the purpose).
C
C                                         KS / AAO 30th Jan 1984
C     Modified:
C
C     17 Jun 1985  KS / AAO.  ELSPLOT added.
C     26 Jul 1985  KS / AAO.  Minor modification to message output
C                  when NOSPOOL specified, to allow for new (VMS 4)
C                  version of Starlink rasteriser queue.
C     12 Aug 1985  KS / AAO.  Now expects errors to be percentage
C                  values rather than absolute values.
C     22 Jul 1986  KS / AAO.  Reverts to absolute error values.
C     20 Jul 1987  DJA / AAO. Revised DSA_ routines - specifications
C                  for some have changed. Now uses DYN routines for
C                  dynamic memory handling.
C     15 Sep 1988  KS / AAO.  Major re-write to use PGPLOT.  Parameters
C                  have changed: SPOOL and PLOTDEV are no longer
C                  accepted.  Support for user-specified aborts added.
C     25 Sep 1992  HME / UoE, Starlink. INCLUDE changed. TABs removed.
C                  No longer handle status from PGBEGIN as logical.
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap mapping x data
C                  behind x axis range.
C     26 Jul 1996  MJCL / Starlink, UCL.  One extra PAR_ABORT check.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.  Tidy variable declarations.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER PGBEGIN
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
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      LOGICAL   AUTOSC           ! TRUE if AUTOSCALE is specified
      REAL      BIAS             ! Value of bias parameter
      CHARACTER COMMAND*96       ! The Figaro command passed
      CHARACTER DEVICE*32        ! PGPLOT device specification
      CHARACTER DLAB*64          ! Plot data axis label
      CHARACTER DLABEL*32        ! Structure data axis label
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   DSTATUS          ! Plotting status
      REAL      DUMMY            ! REAL dummy arguement
      CHARACTER DUNITS*32        ! Structure data axis units
      INTEGER   EPTR             ! Dynamic-memory pointer to error array
      LOGICAL   EREXIST          ! TRUE if the error array exists
      LOGICAL   ERRPLT           ! TRUE if the command is ELSPLOT
      INTEGER   ESLOT            ! Map slot number used for error
      LOGICAL   FAULT            ! TRUE if plotting terminates early
      REAL      HIGH             ! Maximum Y-value for a plot
      INTEGER   IGNORE           ! Used to ignore status codes
      INTEGER   IXEN             ! Last element to be plotted in x-axis
      INTEGER   IXST             ! First element to be plotted in x-axis
      LOGICAL   LINES            ! Value of keyword LINES
      REAL      LOW              ! Minimum Y-value for a plot
      DOUBLE PRECISION MAGNITUDE ! Flag TRUE if data in magnitudes
      INTEGER   NDIM             ! Dimensionality of input data
                                 ! structure
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   NX               ! X-dimension of data
      CHARACTER PLAB*64          ! Label for plot
      LOGICAL   PLOPEN           ! Indicates plotting device open
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      REAL      VALUE            ! Temporary REAL
      REAL      VMAX             ! Maximum value in data array
      REAL      VMIN             ! Minimum value in data array
      LOGICAL   WHOLE            ! Value specified for WHOLE
      REAL      X1               ! Position of left axis
      REAL      X2               ! Position of right axis
      CHARACTER XLAB*64          ! X-axis label for plot
      CHARACTER XLABEL*32        ! Structure x-axis label
      REAL      XM               ! X size in meters
      REAL      XSIZE            ! Physical length of plot, in metres
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      CHARACTER XUNITS*32        ! Structure x-axis units
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      REAL      XVEN             ! Last X-axis value to be plotted
      REAL      XVST             ! First X-axis value to be plotted
      REAL      Y1               ! Position of bottom axis
      REAL      Y2               ! Position of top axis
      REAL      YM               ! Y size in meters
      REAL      YSIZE            ! Physical width of plot, in metres
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get command name
C
      CALL PAR_COMMAND(COMMAND)
      ERRPLT=COMMAND.EQ.'ELSPLOT'
C
C     Get name of file for SPECTRUM, and open it.
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of plot device, and open it.
C
      CALL VAR_GETCHR('HARD',0,0,DEVICE,DSTATUS)
      IF (DSTATUS.NE.0) THEN
         CALL PAR_WRUSER('No hardcopy device specified.',IGNORE)
         CALL PAR_WRUSER(
     :          'Use "HARD" command eg "HARD VER" to rectify.',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      DSTATUS=PGBEGIN(0,DEVICE,1,1)
      IF (DSTATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open hardcopy device',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      PLOPEN=.TRUE.
C
C     Get size of plot (XSIZE,YSIZE) Note - PGQVP works in mm,
C     our parameters are in meters, and PGVSIZE works in inches!
C
      CALL PGVSTAND
      CALL PGQVP (2,X1,X2,Y1,Y2)
      YM=(Y2-Y1)/1000.0
      XM=(X2-X1)/1000.0
      CALL PAR_RDVAL('XSIZE',0.01,XM,XM,'Metres',XSIZE)
      IF (XSIZE.EQ.0.) XSIZE=XM
      CALL PAR_RDVAL('YSIZE',0.01,YM,YM,'Metres',YSIZE)
      IF (YSIZE.EQ.0.) YSIZE=YM
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
      X2=X1+XSIZE*1000.0
      Y2=Y1+YSIZE*1000.0
      CALL PGVSIZE (X1/25.4,X2/25.4,Y1/25.4,Y2/25.4)
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,NX,NDELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Try for X-axis information
C
      CALL DSA_GET_AXIS_INFO ('SPECT',1,2,STRINGS,0,DUMMY,STATUS)
      XUNITS=STRINGS(1)
      XLABEL=STRINGS(2)
      IF (STATUS.NE.0) GO TO 500
C
C     Get XSTART and XEND, unless WHOLE was
C     specified, in which case use all of the spectrum.
C
      CALL PAR_RDKEY('WHOLE',.FALSE.,WHOLE)
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
      CALL DSA_AXIS_RANGE ('SPECT',1,'Unconstrained',WHOLE,XVST,XVEN,
     :                     IXST,IXEN,STATUS)
C
C     Try to map the X-axis data array
C
      CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the main data array
C
      CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     For an error plot, map the error data.
C
      IF (ERRPLT) THEN
         CALL DSA_SEEK_ERRORS ('SPECT',EREXIST,STATUS)
         CALL DSA_MAP_ERRORS ('SPECT','READ','FLOAT',EPTR,ESLOT,STATUS)
      END IF
C
C     Was AUTOSCALE specified?
C
      CALL PAR_RDKEY('AUTOSCALE',.FALSE.,AUTOSC)
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
C
C     Specified or not, find out the scale range because we can
C     use that for the reset values.
C
      IF (ERRPLT) THEN
         CALL FIG_ERANGE(%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(EPTR)),IXST,
     :                   IXEN,VMAX,VMIN)
      ELSE
         CALL GEN_RANGEF(%VAL(CNF_PVAL(DPTR)),IXST,IXEN,VMAX,VMIN)
      END IF
C
C     Get data information (units and label)
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
         CALL PAR_SDVAL('HIGH',HIGH,STATUS)
         CALL PAR_SDVAL('LOW',LOW,STATUS)
         CALL PAR_SDVAL('BIAS',0.,STATUS)
      ELSE
         CALL PAR_RDVAL('HIGH',FMIN,FMAX,VMAX,DUNITS,HIGH)
         CALL PAR_RDVAL('LOW',FMIN,HIGH-1.E-20,VMIN,DUNITS,LOW)
         CALL PAR_RDVAL('BIAS',FMIN,FMAX,0.,DUNITS,BIAS)
         HIGH=HIGH-BIAS
         LOW=LOW-BIAS
      END IF
C
C     Check for LINES
C
      IF (.NOT.ERRPLT) CALL PAR_RDKEY('LINES',.FALSE.,LINES)
C
C     Get the label for the plot -
C
      CALL PAR_RDCHAR('LABEL',' ',PLAB)
      IF (PAR_ABORT()) GO TO 500   ! User requested abort
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
C     See if the data axis should be plotted in reverse
C
      IF (MAGNITUDE.NE.0.0) THEN
         VALUE=HIGH
         HIGH=LOW
         LOW=VALUE
      END IF
C
C     Finally, perform the plot
C
      IF (ERRPLT) THEN
         CALL FIG_LEXZPLT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),
     :                    %VAL(CNF_PVAL(EPTR)),NX,IXST,IXEN,HIGH,
     :                    LOW,XLAB,DLAB,PLAB,EREXIST,XVST,XVEN,STATUS)
      ELSE
         CALL FIG_LXZPLOT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),
     :                    NX,IXST,IXEN,HIGH,LOW,XLAB,DLAB,PLAB,
     :                    LINES,XVST,XVEN,STATUS)
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything.
C
      IF (PLOPEN) CALL PGEND
      CALL DSA_CLOSE (STATUS)
C
      END
C+
      SUBROUTINE FIG_LXZPLOT(XVALS,ZVALS,NX,IXST,IXEN,HIGH,LOW,
     :                    XLAB,ZLAB,PLAB,LINES,XVST,XVEN,STATUS)
C
C     F I G _ L X Z P L O T
C
C     Plots an array (ZVALS) against another array (XVALS).  It is
C     assumed that these map element for element, and that the XVALS
C     values represent the coordinates at the center of each 'bin'.
C     This routine uses PGPLOT to plot in an viewport that is assumed
C     to be already open and defined.  It does not call PGEND.
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
C     (>) LINES    (Logical) True if plot is to be as a joined points
C                  plot rather than as a histogram.
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error making the plot.
C
C     Subroutines / functions used -
C
C     PGBOX      Draw the box for a plot.
C     PGBIN      Plot a histogram of data.
C     PGLINE     Plot data by joining points.
C     PGWINDOW   Set the world-coordinate window.
C     PGLABEL    Label a plot.
C     ICH_LEN    Get length of string to trailing blanks
C
C                                       KS / AAO 11th Jan 1985
C     Modified:
C
C     15th Sept 1988  Now uses PGPLOT instead of DIAGRAM.  KS/AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LINES
      INTEGER IXST,IXEN,NX,STATUS
      REAL XVALS(NX),ZVALS(NX),HIGH,LOW,XVST,XVEN
      CHARACTER*(*) XLAB,ZLAB,PLAB
C
C     Make the plot
C
      CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
      CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
      CALL PGLABEL(XLAB,ZLAB,PLAB)
      IF (LINES) THEN
         CALL PGLINE(IXEN-IXST+1,XVALS(IXST),ZVALS(IXST))
      ELSE
         CALL PGBIN(IXEN-IXST+1,XVALS(IXST),ZVALS(IXST),.TRUE.)
      END IF
C
      END
C+
      SUBROUTINE FIG_LEXZPLT(XVALS,ZVALS,ERRORS,NX,IXST,IXEN,HIGH,LOW,
     :                          XLAB,ZLAB,PLAB,ERRUSE,XVST,XVEN,STATUS)
C
C     F I G _ L E X Z P L T
C
C     Plots an array (ZVALS) against another array (XVALS), as an error
C     bar plot, with Y errors specified by a third array (ERRORS),
C     and X error bars simply covering the space between the points. It
C     is assumed that these map element for element, and that the XVALS
C     values represent the coordinates at the center of each 'bin'.
C     This routine uses PGPLOT to plot in an viewport that is assumed
C     to be already open and defined.  It does not call PGEND.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) XVALS    (Real array XVALS(NX) The abscissae
C                  for each point to be plotted.
C     (>) ZVALS    (Real array ZVALS(NX) The data to be plotted.
C     (>) ERRORS   (Real array ERRORS(NX)) The errors in Y for the
C                  points.
C     (>) NX       (Integer) Number of elements in XVALS and ZVALS.
C     (>) IXST     (Integer) The first array element to be plotted.
C     (>) IXEN     (Integer) The last array element to be plotted.
C     (>) HIGH     (Real) The maximum value for the plot.
C     (>) LOW      (Real) The minimum value for the plot.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) ZLAB     (Character) The Z-label for the plot.
C     (>) PLAB     (Character) The label for the plot as a whole.
C     (>) ERRUSE   (Logical) True if the values in the ERRORS array
C                  are to be used.  If false, all the Y error values
C                  will be taken as zero.
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error making the plot.
C
C     Subroutines / functions used -
C
C     PGBOX      Draw the box for a plot.
C     PGERRX     Plot error bars in X
C     PGERRY     Plot error bars in Y.
C     PGADVANCE  Clear screen or start new plot.
C     PGWINDOW   Set the world-coordinate window.
C     PGVSTAND   Set the standard viewport.
C     PGEND      Terminate a plot.
C     PGLABEL    Label a plot.
C     ICH_LEN    Get length of string to trailing blanks
C
C                                       KS / AAO 17th June 1985
C     Modified:
C
C     12th Aug 1985.   KS / AAO.  Now expects errors to be % errors.
C     22nd July 1986.  KS / AAO.  Reverts to absolute errors.
C     15th Sept 1988.  KS / AAO.  Rewritten to use PGPLOT instead of DIAGRAM
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ERRUSE
      INTEGER IXST,IXEN,NX,STATUS
      REAL XVALS(NX),ZVALS(NX),ERRORS(NX),HIGH,LOW,XVST,XVEN
      CHARACTER*(*) XLAB,ZLAB,PLAB
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER I, LENGTH
      REAL XVAL1, XVAL2, YVAL1, YVAL2
C
C     Make the plot
C
      CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
      CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
      CALL PGLABEL(XLAB,ZLAB,PLAB)
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
C
      END
