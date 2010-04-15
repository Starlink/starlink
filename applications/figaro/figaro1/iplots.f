C+
      SUBROUTINE IPLOTS
C
C     I P L O T S
C
C     Produces a plot of successive cross-sections of an image ,
C     several to a page. The plot is directed to the device
C     defined by the user variable 'HARD', and by the value of
C     the command keyword 'HARDCOPY'.
C
C     Command parameters -
C
C     IMAGE       The data to be plotted.  If there is X-axis
C                 information then this will be used.  If not, the
C                  X-axis will just have to be the numbers from 1 to n.
C     XSTART      The x-value at which plotting is to start. XSTART and
C                 XEND are only prompted for if WHOLE is not specified.
C     XEND        The x-value at which plotting is to end.
C     YSTART      The first cross-section to be plotted.
C     YEND        The last cross-section to be plotted.
C     LABEL       A label for the plot.
C     COLOUR      The colour for the plot (only meaningful for the
C                 Grinnell - later may be extended to map onto
C                 different line types).  The axes are always white.
C     NSPECT      The number of cross-sections to be plotted per sheet.
C
C     Command keywords -
C
C     WHOLE       The program is to display all of each cross-section.
C     AUTOSCALE   If NO (the default) , then each spectrum will be
C                 scaled to the extrema of all the sections under
C                 consideration. If YES then all spectra will be
C                 autoscaled individually.
C     HARDCOPY    If specified then output is sent to the device
C                 determined by HARD. If no device is specified by HARD,
C                 or HARDCOPY is not specified, then IPLOTS will attempt
C                 to plot on the current SOFT device.
C     NEXT        Used to pause between plots.
C
C     User variables -    (">" input, "<" output)
C
C     (>) SOFT    Specifies the device and type to be used for soft
C                 plots.  See the SOFT command for more details.
C     (>) HARD    Specifies the device and type to be used for hard
C                 plots.  See the HARD command for more details.
C
C
C                                         D.J.A  AAO  9th July 1987
C     Modified:
C
C     15 Jul 1987  DJA / AAO. Revised DSA_ routine calls - specs
C                  for some have changed.
C     22 Jul 1987  DJA / AAO. Modified dynamic memory handling -
C                  now uses DYN_ routines
C      8 Jan 1988  KS / AAO.  Was colouring axes as well as data
C                  for all plots after the first if COLOUR was
C                  specified as non-white.
C                  Call to DSA_AXIS_BOUNDS dummied out to remove
C                  error from link step.
C      4 Mar 1988  KS / AAO.  Corrected calculation of IDEN if
C                  AUTOSCALE specified.  Calculation of number
C                  of elements to be plotted also corrected -
C                  hard to see how that ever got wrong!
C                  Also changed to produce a histogram plot,
C                  rather than a join-the-dots plot.
C     17 Mar 1988  KS / AAO.  Modified for GKS version of PGPLOT
C      5 Oct 1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                  changed. PGASK is banned from ADAM, commented out.
C     25 Jan 1993  HME / UoE, Starlink.  Put PGASK back in.
C     28 Jul 1993  HME / UoE, Starlink.  Disuse GKD_* and PAR_Q*. Use
C                  PAR_ABORT. Added parameter NEXT.
C     25 Jul 1995  HME / UoE, Starlink.  Use PGSCH instead of PGSETC.
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap axis map behind
C                  axis range for X.
C     14 May 1999  TDCA / RAL, Starlink. Type of MAGNITUDE changed from
C                  REAL to DOUBLE PRECISION.
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_CLEAN,ICH_FOLD,ICH_KEY
      CHARACTER ICH_CI*3
C
C     Real variable plot value limits - keeps plot range within
C     the VAX real number limits.
C
C     REAL PLMAX,PLMIN
C     PARAMETER (PLMAX=1.0E36,PLMIN=-1.0E36)
C
C     Floating point limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      LOGICAL   AUTOSC           ! True if AUTOSCALE is specified
      INTEGER   CKEY             ! Colour code used by PGPLOT
      CHARACTER COLOUR*10        ! COLOUR specification
      CHARACTER CONTROL2*32      ! Controller for x-axis,1D array
      CHARACTER CONTROL3*32      ! Controller for x-axis,2d array
      INTEGER   CURXSECT         ! Current cross-section being plotted
      CHARACTER DEVICE*32        ! PGPLOT device specification
      INTEGER   DDIMS(10)        ! Sizes of the dimensions of the data
      CHARACTER DLAB*64          ! Plot data axis label
      CHARACTER DLABEL*32        ! Structure data axis label
      INTEGER   DSLOT            ! Map slot number used for data
      LOGICAL   DTWODIMS         ! True if data is two dimensional
      REAL      DUMMY            ! REAL dummy arguement
      CHARACTER DUNITS*32        ! Structure data axis units
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      LOGICAL   HARD             ! Output device is the hard one?
      REAL      HIGH             ! Maximum Y-value for a plot
      INTEGER   IDYMAX           ! The last cross-section to be plotted
      INTEGER   IDYMIN           ! The first cross-section to be plotted
      INTEGER   IGNORE           ! Used to ignore status codes
      INTEGER   INVOKE           ! Used to invoke functions
      INTEGER   IDEN             ! Last element to be plotted in data
                                 ! array
      INTEGER   IDST             ! First element to be plotted in data
                                 ! array
      INTEGER   IXEN             ! Last element to be plotted in x-axis
      INTEGER   IXST             ! First element to be plotted in x-axis
      LOGICAL   KEEPGOING        ! FALSE if plotting to be stopped early
      CHARACTER LABEL*64         ! The group label for all the plots
      INTEGER   LASTY            ! Number of last cross-section on a
                                 ! page
      REAL      LOW              ! Minimum Y-value for a plot
      DOUBLE PRECISION MAGNITUDE ! Magnitude flag value for data
      INTEGER   NCROSS           ! Number of cross-sections plotted per
                                 ! page
      INTEGER   NDD              ! Dimensionality of input data
                                 ! structure
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   ND1              ! Total number of elements per
                                 ! cross-section
      INTEGER   NEXT             ! ICH_KEY arguement - ignored
      INTEGER   NPELMS           ! Number of array elements to be
                                 ! plotted
      INTEGER   NXELM            ! Total number of elements in x-axis
                                 ! array
      INTEGER   NX1              ! The number of elements per x-axis
      INTEGER   NX2              ! The number of x-axes
      INTEGER   NXD              ! Dimensionality of x-axis info
      CHARACTER PLAB*6           ! Sub-label for corners of each plot
      LOGICAL   SOFT             ! Output device is the soft one?
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      INTEGER   THICK            ! Line thickness of plot
      INTEGER   TOPPAGE          ! Cross-section at the top of the page
      REAL      VALUE1           ! Temporary REAL
      REAL      VALUE2           ! Temporary REAL
      REAL      VMAX             ! Maximum value in data array
      REAL      VMIN             ! Minimum value in data array
      LOGICAL   WHOLE            ! Value specified for WHOLE
      INTEGER   XDIMS(10)        ! Sizes of the dimensions of x-axis
                                 ! data
      LOGICAL   XINDCALC         ! TRUE if both x-axis & data are 2D
      CHARACTER XLAB*64          ! X-axis label for plot
      CHARACTER XLABEL*32        ! Structure x-axis label
      REAL      XMAX             ! Maximum value in x-axis data per plot
      REAL      XMIN             ! Minimum value in x-axis data per plot
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      LOGICAL   XTWODIMS         ! X-axis data is two dimensional?
      CHARACTER XUNITS*32        ! Structure x-axis units
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      REAL      XVEN             ! Max x-axis value in all data to be
                                 ! plotted
      REAL      XVST             ! Min x-axis value in all data to be
                                 ! plotted
C
C      Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C      Initial values
C
      THICK=1
      KEEPGOING=.TRUE.
      CONTROL2='Unconstrained , not complex '
      CONTROL3='Unconstrained , Complex'
C
C      Open input image/spectrum.
C
      CALL DSA_INPUT ('SPECTRA','IMAGE',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C      Get dimensions of data
C
      CALL DSA_DATA_SIZE ('SPECTRA',2,NDD,DDIMS,NDELM,STATUS)
      ND1=DDIMS(1)
      DTWODIMS=(NDD.EQ.2)
C
C      Get YSTART and YEND
C
      WHOLE=(.NOT.DTWODIMS)
      CALL DSA_AXIS_RANGE ('SPECTRA',2,' ',WHOLE,VALUE1,VALUE2,
     :                                  IDYMIN,IDYMAX,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C      Get dimensions of X-axis info. Usually 1D but could be 2D...one
C      set of axis values for each cross-section in the image. In the latter
C      case the different axes must be stepped through in a similar fashion
C      to the cross-section data itself. We assume the x-axis maps one-to-one
C      with the image data.
C
      CALL DSA_AXIS_SIZE ('SPECTRA',1,2,NXD,XDIMS,NXELM,STATUS)
      NX1=XDIMS(1)
      XTWODIMS=(NXD.EQ.2)
      XINDCALC=(XTWODIMS.AND.DTWODIMS)
      IF (XTWODIMS) THEN
         NX2=XDIMS(2)
      ELSE
         NX2=1
      END IF
C
C      Try for X-axis information
C
      CALL DSA_GET_AXIS_INFO ('SPECTRA',1,2,STRINGS,0,DUMMY,STATUS)
      XUNITS=STRINGS(1)
      XLABEL=STRINGS(2)
C
C      Get WHOLE specification. Get range of data to display.
C
      CALL PAR_RDKEY('WHOLE',.TRUE.,WHOLE)
      IF (PAR_ABORT()) GOTO 500
      IF (XINDCALC) THEN
C
C         IX.. are passed as dummy arguements here
C
         CALL DSA_AXIS_RANGE ('SPECTRA',1,CONTROL3,WHOLE,XVST,XVEN,
     :                                                 IXST,IXEN,STATUS)
      ELSE
         CALL DSA_AXIS_RANGE ('SPECTRA',1,CONTROL2,WHOLE,XVST,XVEN,
     :                                                 IXST,IXEN,STATUS)
         XMIN=XVST
         XMAX=XVEN
      END IF
C
C      Try to map the X-axis data array
C
      CALL DSA_MAP_AXIS_DATA ('SPECTRA',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C      Map in the image data
C
      CALL DSA_MAP_DATA ('SPECTRA','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C      Get data information (units and label)
C
      CALL DSA_GET_DATA_INFO ('SPECTRA',2,STRINGS,1,MAGNITUDE,STATUS)
      DUNITS=STRINGS(1)
      DLABEL=STRINGS(2)
C
C      Was AUTOSCALE specified?
C
      CALL PAR_RDKEY('AUTOSCALE',.FALSE.,AUTOSC)
      IF (PAR_ABORT()) GOTO 500
C
C      Get label for the plot.
C
      CALL PAR_RDCHAR('LABEL',' ',LABEL)
      IF (PAR_ABORT()) GOTO 500
C
C      If more than 1 dimension, ie. more than 1 cross-section, get number
C      of sections to be plotted per page.
C
      IF (DTWODIMS) THEN
         CALL PAR_RDVAL('NSPECT',1.,6.,3.,' ',VALUE1)
         IF (PAR_ABORT()) GOTO 500
         NCROSS=NINT(VALUE1)
      ELSE
         NCROSS=1
      END IF
C
C      Check for 'HARD'
C
      HARD=.FALSE.
      CALL PAR_RDKEY('HARDCOPY',.FALSE.,HARD)
      IF (PAR_ABORT()) GOTO 500
      IF (HARD) THEN
           CALL VAR_GETCHR('HARD',0,0,DEVICE,STATUS)
           IF (STATUS.NE.0) THEN
              CALL PAR_WRUSER('No hardcopy device specified.',STATUS)
              CALL PAR_WRUSER(
     :            'Use "HARD" command eg "HARD VER" to rectify.',STATUS)
              GO TO 500
           END IF
      ELSE
C
C      Look for the value of the user variable 'SOFT'
C
         CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
         IF (STATUS.NE.0) THEN
              CALL PAR_WRUSER('No plotting device specified.',STATUS)
              CALL PAR_WRUSER(
     :           'Use "SOFT" command eg "SOFT /VT" to rectify.',STATUS)
              GO TO 500
         END IF
         SOFT=.TRUE.
      END IF
C
C      Get the colour for the plot (note that BLACK and BLUE are reversed
C      in the colour list, so BL will be taken as BLUE - the codes are
C      then reversed to give the correct PGPLOT colour code)
C
      CALL PAR_RDCHAR('COLOUR','White',COLOUR)
      IF (PAR_ABORT()) GOTO 500
      INVOKE=ICH_FOLD(COLOUR)
      INVOKE=ICH_CLEAN(COLOUR)
      CKEY=ICH_KEY(COLOUR,1,',; ',
     :        'BLUE:WHITE:RED:GREEN:BLACK:CYAN:MAGENTA:YELLOW:',
     :        'Abbr.',NEXT)-1
      IF (CKEY.EQ.0) THEN
           CKEY=4
      ELSE IF (CKEY.EQ.4) THEN
           CKEY=0
      END IF
      IF (CKEY.LT.0) THEN
           CALL PAR_WRUSER('Cannot colour the plot '//COLOUR,STATUS)
           CKEY=1
      END IF
C
C      Get the axis labels from the labels and units
C
      CALL FIG_MAKE_AXIS_LABEL(XLABEL,XUNITS,XLAB)
      CALL FIG_MAKE_AXIS_LABEL(DLABEL,DUNITS,DLAB)
C
C      Set up plotting environment.
C
      CALL FIG_GRASTART(DEVICE,THICK,NCROSS,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error opening device for plotting',IGNORE)
         GOTO 500
      END IF
C
C      If AUTOSCALE not specified then find the extrema of the data
C      values in all the cross-sections under consideration.
C
      IF (.NOT.AUTOSC) THEN
         VALUE1=0.
         VALUE2=0.
         DO CURXSECT=IDYMIN,IDYMAX
            IDST=(CURXSECT-1)*ND1+IXST
            IDEN=IDST+(IXEN-IXST)
            CALL GEN_RANGEF(%VAL(CNF_PVAL(DPTR)),IDST,IDEN,VMAX,VMIN)
            VALUE1=MIN(VMIN,VALUE1)
            VALUE2=MAX(VMAX,VALUE2)
         END DO
         VMAX=VALUE2
         VMIN=VALUE1
      END IF
C
C      Plot cross-sections of image
C
      TOPPAGE=IDYMIN
      DO WHILE ((TOPPAGE.LE.IDYMAX).AND.(KEEPGOING))
         LASTY=MIN((TOPPAGE+NCROSS-1),IDYMAX)
C
C         Put label at top of each page
C
         CALL PGSCH(2.0)
         CALL PGMTEXT('Top',1.,0.5,0.5,LABEL)
         CALL PGSCH(1.0)
C
         DO CURXSECT=TOPPAGE,LASTY
C
C            If we must locate the x-axis data then do so, otherwise
C            just use defaults for 1D array.
C
C             This section dummied until AXIS_BOUNDS written.
C
            IF (XTWODIMS) THEN
                 CALL PAR_WRUSER('Note: 2D X-data not handled properly',
     :                                                           STATUS)
C               CALL DSA_AXIS_BOUNDS ('SPECTRA',1,CURXSECT,XMIN,XMAX,
C     :                                                  IXST,IXEN,STATUS)
            END IF
C
C            Locate cross-section in the data array
C
            NPELMS=IXEN-IXST+1
            IDST=(CURXSECT-1)*ND1+IXST
            IDEN=IDST+(IXEN-IXST)
C
C            If AUTOSCALE was specified then scale this section otherwise
C            use the values derived for all the data.
C
            IF (AUTOSC) THEN
               CALL GEN_RANGEF(%VAL(CNF_PVAL(DPTR)),IDST,IDEN,VMAX,VMIN)
            END IF
            HIGH=VMAX+(VMAX-VMIN)*.10
            LOW=VMIN
            IF (HIGH.EQ.LOW) THEN
               IF (LOW.EQ.0.0) THEN
                  HIGH=0.01
               ELSE
                  HIGH=ABS(LOW)*1.1
               END IF
            END IF
C
C            See if the data should be plotted in reverse
C
            IF (MAGNITUDE.NE.0.0) THEN
               VALUE1=HIGH
               HIGH=LOW
               LOW=VALUE1
            END IF
C
C            Create a sublabel
C
            PLAB='No '//ICH_CI(CURXSECT)
C
C            Plot the cross-section
C
            CALL FIG_SEG_PLOT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),
     :                        NXELM,NDELM,IXST,IDST,NPELMS,HIGH,
     :                        LOW,XLAB,DLAB,PLAB,CKEY,XMIN,XMAX)
C
C            Clear viewport unless we're at the last plot on the page , as
C            this would prematurely clear the screen.
C
            IF (CURXSECT.NE.LASTY) CALL PGADVANCE
         END DO
         TOPPAGE=LASTY+1
         IF (TOPPAGE.LT.IDYMAX) THEN
            IF (.NOT.HARD) THEN
               CALL PAR_CNPAR('NEXT')
               CALL PAR_RDKEY('NEXT',.TRUE.,KEEPGOING)
               IF (PAR_ABORT()) GOTO 500
            ELSE
               KEEPGOING=.TRUE.
            END IF
            CALL PGADVANCE
         END IF
      END DO
C
C      Close plotting down.
C
      CALL FIG_GRAEND
C
C      Set the user variables describing the plot.
C
      CALL VAR_SETNUM('TVXST',0,0,XVST,STATUS)
      CALL VAR_SETNUM('TVXEN',0,0,XVEN,STATUS)
      CALL VAR_SETNUM('TVHIGH',0,0,HIGH,STATUS)
      CALL VAR_SETNUM('TVLOW',0,0,LOW,STATUS)
      CALL VAR_SETCHR('TVFILE',0,0,'SPECTRA',STATUS)
      CALL VAR_SETNUM('TVCOLOR',0,0,FLOAT(CKEY),STATUS)
C
C      Close down everything else
C
  500 CALL DSA_CLOSE(STATUS)
C
        END

C+
      SUBROUTINE FIG_GRASTART(DEVICE,THICK,NCROSS,STATUS)
C
C      F I G _ G R A S T A R T
C
C      Sets up the plotting routines.
C
C      Parameters -  (">" input, "<" output)
C
C      (>) DEVICE   (Character) The device/type to be used for the
C                    plot - see PGPLOT documentation for details.
C      (>) THICK    (Integer) The line thickness for the plot.
C      (>) NCROSS   (Integer) The number of spectra per page.
C      (<) STATUS   (Integer) Returns plot status.
C                    0 => OK, non zero => some error opening the plot.
C
C      Subroutines / functions used:-
C
C      PGBEGIN    (PGPLOT package) Open a plot file.
C      PGVSTAND   (  "      "    ) Set the standard viewport.
C      PGADVANCE  (  "      "    ) Clear screen or start new plot.
C      PGASK      (  "      "    ) Set status of interactive prompting.
C       PGSCI      (  "      "    ) Set plotting colour
C      PGSLW      (  "      "    ) Set line width
C
C                                  D.J.A  AAO  9th July 1987
C       Modified:
C
C       17th March 1988   KS / AAO.  Modified for use with GKS version
C                         of PGPLOT.
C+
      IMPLICIT NONE
C
C      Parameters
C
      INTEGER NCROSS , STATUS , THICK
      CHARACTER*(*) DEVICE
C
C      Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C      Function
C
      INTEGER PGBEGIN
C
      STATUS=PGBEGIN(0,DEVICE,1,NCROSS)
      IF (STATUS.EQ.1) THEN
C
C         Turn off runtime messages...handled by GKD_QUEST in main prog.
C
         CALL PGASK(.FALSE.)
C
C         Define plotting environment.
C
         CALL PGVSTAND
         CALL PGADVANCE
         CALL PGSCI(WHITE)
         CALL PGSLW(THICK)
         STATUS=0
      ELSE
         STATUS=1
      END IF

      END
C+
      SUBROUTINE FIG_GRAEND
C
C      F I G _ G R A E N D
C
C      Closes down the plotting system.
C
C      Subroutines / functions used:-
C
C      PGEND      (PGPLOT package)   Terminate a plot.
C      PGSCI      (  "      "    )   Set plot colour
C      PGSLW      (  "      "    )   Set line width
C
C                                  D.J.A  AAO  9th July 1987
C       Modified:
C
C       17th March 1988   KS / AAO.  Modified for use with GKS version
C                         of PGPLOT.
C+
C      Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)

      CALL PGSCI(WHITE)
      CALL PGSLW(1)
      CALL PGEND

      END

C+
      SUBROUTINE FIG_SEG_PLOT(XVALS,DVALS,NXELM,NDELM,IXST,IDST,NPELMS,
     :                           HIGH,LOW,XLAB,DLAB,PLAB,CKEY,XVST,XVEN)
C
C      F I G _ S E G _ P L O T
C
C      Plots segment of the array (DVALS) against that of another array (XVALS)
C      It is assumed that these map element for element, and that the XVALS
C      values represent the coordinates at the center of each 'bin'.
C
C      Parameters -   (">" input, "<" output)
C
C      (>) XVALS    (Real array 1..NXELM) The abscissae
C                    for each point to be plotted.
C      (>) DVALS    (Real array 1..NDELM) The data to be plotted.
C      (>) NXELM    (Integer)   Number of elements in XVALS.
C      (>) NDELM    (Integer)   Number of elements in DVALS.
C      (>) IXST     (Integer)   The first x-array element to be plotted.
C      (>) IDST     (Integer)   The first data-array element to be plotted.
C      (>) NPELMS   (Integer)   Number of elements from x & data arrays to
C                             be plotted.
C      (>) HIGH     (Real)      The maximum value for the plot.
C      (>) LOW      (Real)      The minimum value for the plot.
C      (>) XLAB     (Character) The X-label for the plot.
C      (>) DLAB     (Character) The y-axis label for the plot.
C      (>) PLAB     (Character) The sub-label for each spectrum.
C      (>) CKEY     (Integer)   The GRPCKG code (0..7) for the colour
C                                of the plot.
C      (>) XVST     (Real)      The actual x-start value for the plot.
C      (>) XVEN     (Real)      The actual x-end value for the plot.
C
C                                  D.J.A  AAO  9th July 1987
C       Modified:
C
C       8th Jan 1988    Now ensures that axes are plotted in white.  KS / AAO.
C       4th March 1988  Now uses a histogram plot.  KS / AAO.
C       17th March 1988 KS / AAO.  Modified for use with GKS version
C                       of PGPLOT.
C+
      IMPLICIT NONE
C
C      Parameters
C
      INTEGER IXST , IDST , NXELM , NDELM , CKEY , NPELMS
      REAL XVALS(NXELM) , DVALS(NDELM) , HIGH , LOW , XVST , XVEN
      CHARACTER*(*) XLAB , DLAB , PLAB
C
C      Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C      Plot data.
C
      CALL PGSCI(WHITE)
      CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
      CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
C
C      The top label is only printed for the plot on the top of the
C      page and so is done in the IPLOTS subroutine.
C
      CALL PGLABEL(XLAB,DLAB,' ')
C
C      Put the sub-label in the top left and right corners of the plot.
C
      CALL PGSCH(1.8)
      CALL PGMTEXT('Top',-2.0,0.025,0.0,PLAB)
      CALL PGMTEXT('Top',-2.0,0.975,1.0,PLAB)
      CALL PGSCH(1.0)

      CALL PGSCI(CKEY)

      CALL PGBIN(NPELMS,XVALS(IXST),DVALS(IDST),.TRUE.)

      END
