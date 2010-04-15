C+
      SUBROUTINE ISPLOT
C
C     I S P L O T
C
C     Produces a hidden line histogram plot of an image.  This routine
C     is really intended for the case where the image is a 2D set of
C     spectra.  The plot may be slanted to give a '3D' effect if
C     required, and may be in reverse order.  The plot is directed
C     to the device defined by the user variables 'SOFT' and
C     'HARD', and by the value of the command keyword 'HARDCOPY',
C     so will appear immediately if these specify a video
C     device (VT125, Grinnell, etc.).  If a hardcopy device
C     is specified, the file for that device will be produced,
C     but SPLOT does not attempt to spool it off for printing.
C
C     Command parameters -
C
C     IMAGE       The data to be plotted.  This should have a
C               two dimensional data structure. If there
C                 is an x-axis component this will be used to
C                 give the x-axis.  If not, the x-axis will just
C                 have to be the numbers from 1 to n.
C     YSTART      The y-value at which plotting is to start.
C     YEND        The y-value at which plotting is to end.
C     XSTART      The x-value at which plotting is to start.
C     XEND        The x-value at which plotting is to end.
C                 (XSTART and XEND are not required if the
C                 WHOLE keyword is specified.)
C     HIGH        The maximum value to be used for the plot.
C     LOW         The minimum value to be used for the plot.
C                 (HIGH and LOW are not required if the
C                 AUTOSCALE keyword is specified.)
C     OFFSET      A bias to be added to each successive cross-section
C                 to offset it from the previous one.  Normally, this
C                 will be allowed to default, unless it is explicitly
C                 specified in the command line.
C     LABEL       A label for the plot.
C     COLOUR      The colour for the plot (only meaningful for the
C                 Grinnell - later may be extended to map onto
C                 different line types).  The axes are always white.
C
C     Command keywords -
C
C     AUTOSCALE   The program is to work out the values for HIGH
C                 and LOW, using the maximum and minimum values
C                 in the data over the specified range.
C     WHOLE       The program is to plot the whole of each cross-section
C                 of the image - note, not all the image, ie not every
C                 cross-section.
C     HARDCOPY    The plot is to produce a hard copy.
C     AXES        Axes will be plotted.
C     ERASE       The screen will be erased before the plot.
C     REVERSE     The cross-sections will be plotted in reverse order.
C
C     User variables -    (">" input, "<" output)
C
C     (>) SOFT    Specifies the device and type to be used for soft
C                 plots.  See the SOFT command for more details.
C     (>) HARD    Specifies the device and type to be used for hard
C                 plots.  See the HARD command for more details.
C     (<) TVDIM   is set to 1 if a single spectrum is plotted, to
C                 2 otherwise.
C     (<) TVYST   is set to the starting y-value for the plot.
C     (<) TVYEN   is set to the final y-value for the plot.
C     (<) TVXST   is set to the starting x-value for the plot.
C     (<) TVXEN   Is set to the final x-value for the plot.
C     (<) TVHIGH  Is set to the same value as HIGH.
C     (<) TVLOW   Is set to the same value as LOW.
C     (<) TVFILE  Is set to the value of IMAGE.
C     (<) TVCOLOR Is set to the GRPCKG code for the plot colour.
C                 (The TV.. variables are intended for use by
C                 cursor routines, and reflect the settings for the
C                 last plot made, even if XSTART etc are changed.)
C
C     (Other user variables may be set by the command processor, in
C     connection with the parameter values.)
C
C                                         KS / CIT  5th March 1984
C     Modified:
C
C     06 May 1985  KS / AAO.  Autoscale algorithm modified slightly.
C     17 Jul 1987  DJA / AAO.  New DSA_ routines - specs changed
C     22 Jul 1987  DJA / AAO.  Modified dynamic memory handling -
C                  now uses DYN_ routines
C     17 Mar 1988  KS / AAO. Modified for GKS version of PGPLOT.
C     05 Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                  removed.
C     17 Sep 1993  HME / UoE, Starlink.  Correct call to PGHI2D.
C     16 Feb 1996  HME / UoE, Starlink.  Convert to FDA:
C                  No concurrent mapping. Had to swap mapping axis
C                  behind axis range, for Y and X.
C     20 Mar 1996  HME / UoE, Starlink.  Swap label and units in call
C                  to FIG_MAKE_AXIS_LABEL.
C     24 Jul 1996  MJCL / Starlink, UCL.  Corrected type of REVERS.
C                  Catenations updated for Linux.
C     26 Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_FOLD,ICH_CLEAN,ICH_KEY
      REAL    GEN_ELEMF
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Floating point limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Real variable plot value limits - keeps plot range within
C     the VAX real number limits.
C
      REAL PLMAX,PLMIN
      PARAMETER (PLMAX=1.0E36,PLMIN=-1.0E36)
C
C     Local variables
C
      LOGICAL   AUTOSC           ! TRUE if AUTOSCALE is specified
      LOGICAL   AXES             ! TRUE if axes are to be drawn
      INTEGER   CKEY             ! Colour code used by PGPLOT
      CHARACTER COLOUR*10        ! COLOUR specification
      CHARACTER DEVICE*32        ! PGPLOT device specification
      INTEGER   DDIMS(10)        ! Sizes of the dimensions of the data
      REAL      DHIGH            !
      CHARACTER DLAB*64          ! Plot data axis label
      CHARACTER DLABEL*32        ! Structure data axis label
      REAL      DLOW             !
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      REAL      DUMMY            ! REAL dummy arguement
      CHARACTER DUNITS*32        ! Structure data axis units
      LOGICAL   ERASE            ! TRUE if the screen is to be wiped
      LOGICAL   HARD             ! True if the output device is hard
      REAL      HIGH             ! Maximum Y-value for a plot
      REAL      HIGHD            !
      INTEGER   INVOKE           ! Used to invoke functions
      INTEGER   INCY             ! +1 for normal plots, -1 for reverse
      INTEGER   IPTR             !
      LOGICAL   ISNEW            ! Is address new to CNF?
      INTEGER   IXEN             ! Last element to be plotted in x-axis
      INTEGER   IXST             ! First element to be plotted in x-axis
      INTEGER   IY               !
      INTEGER   IY1              !
      INTEGER   IY2              !
      INTEGER   IYEN             ! Last element to be plotted in y-axis
      INTEGER   IYST             ! First element to be plotted in y-axis
      CHARACTER LABEL*64         ! The group label for all the plots
                                 ! page
      INTEGER   LLAB             ! Length of label string
      REAL      LOW              ! Minimum Y-value for a plot
      REAL      MAGNITUDE        !
      INTEGER   MSPEC            !
      INTEGER   NDD              ! Dimensionality of input data
                                 ! structure
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   ND1              ! Total number of elements per
                                 ! x-section
      INTEGER   ND2              ! Total number of cross-sections
      INTEGER   NEXT             ! ICH_KEY arguement - ignored
      INTEGER   NSPECT           ! Number of x-sections to be plotted
      REAL      OFFSET           !
      LOGICAL   PISNEW           ! Previous CNF pointer to data new?
      REAL      RESET            !
      LOGICAL   REVERS           ! TRUE if REVERSE is specified
      INTEGER   SLANT            ! Slant in pixels between x-sections
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      INTEGER   TPTR             ! Temp dynamic-memory pointer
      REAL      TVDIM            !
      REAL      VALUE            ! Temporary REAL
      REAL      VMAX             ! Maximum value in data array
      REAL      VMIN             ! Minimum value in data array
      LOGICAL   WHOLE            ! Value specified for WHOLE
      INTEGER   WPTR             ! Pointer to plotting workspace
      INTEGER   WSLOT            ! Map slot number for plot workspace
      CHARACTER XLAB*64          ! X-axis label for plot
      CHARACTER XLABEL*32        ! Structure x-axis label
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      CHARACTER XUNITS*32        ! Structure x-axis units
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      REAL      XVEN             ! Last X-axis value to be plotted
      REAL      XVST             ! First X-axis value to be plotted
      INTEGER   YPTR             ! Dynamic-memory pointer to y-axis data
      INTEGER   YSLOT            ! Map slot number used for y-axis info
      REAL      YVEN             ! Last Y-axis value to be plotted
      REAL      YVST             ! First Y-axis value to be plotted
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of file for IMAGE and open it.
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDD,DDIMS,NDELM,STATUS)
      ND1=DDIMS(1)
      IF (NDD.EQ.1) THEN
         ND2=1
      ELSE
         ND2=DDIMS(2)
      END IF
C
C     Get limits on data range  (YSTART, YEND)
C
      CALL DSA_AXIS_RANGE('IMAGE',2,' ',.FALSE.,YVST,YVEN,
     :                    IYST,IYEN,STATUS)
      CALL DSA_MAP_AXIS_DATA('IMAGE',2,'READ','FLOAT',YPTR,
     :                       YSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Try for x-axis information
C
      CALL DSA_GET_AXIS_INFO ('IMAGE',1,2,STRINGS,0,DUMMY,STATUS)
      XUNITS=STRINGS(1)
      XLABEL=STRINGS(2)
C
C     Get XSTART and XEND, unless WHOLE was
C     specified, in which case use all of the image.
C
      CALL PAR_RDKEY('WHOLE',.TRUE.,WHOLE)
      IF ( PAR_ABORT() ) GO TO 500
      CALL DSA_AXIS_RANGE ('IMAGE',1,'Unconstrained',WHOLE,XVST,
     :                     XVEN,IXST,IXEN,STATUS)
C
C     Try to map the x-axis data array
C
      CALL DSA_MAP_AXIS_DATA ('IMAGE',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the image data
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     See if REVERSE specified
C
      CALL PAR_RDKEY('REVERSE',.FALSE.,REVERS)
      IF (REVERS) THEN
         IY1=IYEN
         IY2=IYST
         INCY=-1
      ELSE
         IY1=IYST
         IY2=IYEN
         INCY=1
      END IF
C
C     Was AUTOSCALE specified?
C
      CALL PAR_RDKEY('AUTOSCALE',.FALSE.,AUTOSC)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Specified or not, find out the scale range because we can
C     use that for the reset values.  The calculation of the reset
C     values for HIGH and LOW is as follows:
C       LOW is simply the lowest value found in whichever cross-section
C       is to be plotted first.
C
C       HIGH is then calculated on the basis of the following relation:
C       if m is the cross-section number containing the data that will
C       reach the highest in the plot, and Y is the maximum value in
C       that cross-section, then OFFSET=(HIGH-LOW)/(IYEN-IYST+1) and
C       HIGH = OFFSET*(M-1)+Y-LOW, which can be solved for HIGH for
C       each cross-section in turn, and the highest resulting value is
C       used.
C
C     Clear up previous CNF pointer if new.
C
      CALL DYN_INCAD(DPTR,'FLOAT',(IY1-1)*ND1,IPTR,ISNEW,STATUS)
      CALL GEN_RANGEF(%VAL(CNF_PVAL(IPTR)),IXST,IXEN,VMAX,DLOW)
      NSPECT=IYEN-IYST+1
      MSPEC=0
      PISNEW = .FALSE.
      DO IY=IY1,IY2,INCY
         IF (IY.NE.IY1) THEN
            CALL GEN_RANGEF(%VAL(CNF_PVAL(IPTR)),IXST,IXEN,VMAX,VMIN)
         END IF

         CALL DYN_INCAD(IPTR,'FLOAT',INCY*ND1,TPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(IPTR)
         IPTR = TPTR
         PISNEW = ISNEW

         MSPEC=MSPEC+1
         HIGHD=(VMAX*(NSPECT)-DLOW*(MSPEC+NSPECT-1))/(NSPECT-MSPEC+1)
         IF (IY.NE.IY1) THEN
            DHIGH=MAX(DHIGH,HIGHD)
         ELSE
            DHIGH=HIGHD
         END IF
      END DO
      IF (ISNEW) CALL CNF_UNREGP(IPTR)
C
C     Get image data information (units and label)
C
      CALL DSA_GET_DATA_INFO ('IMAGE',2,STRINGS,1,MAGNITUDE,STATUS)
      DUNITS=STRINGS(1)
      DLABEL=STRINGS(2)
C
C     If AUTOSCALE not specified, get values of HIGH and LOW
C
      HIGH=DHIGH+(DHIGH-DLOW)*.10
      LOW=DLOW
      OFFSET=(HIGH-LOW)/FLOAT(NSPECT+1)
      IF (AUTOSC) THEN
         CALL PAR_SDVAL('HIGH',HIGH,STATUS)
         CALL PAR_SDVAL('LOW',LOW,STATUS)
         CALL PAR_SDVAL('OFFSET',OFFSET,STATUS)
      ELSE
         RESET=HIGH
         CALL PAR_RDVAL('HIGH',FMIN,FMAX,RESET,DUNITS,HIGH)
         RESET=LOW
         CALL PAR_RDVAL('LOW',FMIN,HIGH-1.E-20,RESET,DUNITS,LOW)
         RESET=OFFSET
         CALL PAR_RDVAL('OFFSET',FMIN,HIGH,RESET,DUNITS,OFFSET)
      END IF
C
C     Get value of SLANT
C
      CALL PAR_RDVAL('SLANT',-FLOAT(ND1),FLOAT(ND1),0.,'Pixels',VALUE)
      SLANT=VALUE
C
C     Check for AXES and ERASE
C
      CALL PAR_RDKEY('AXES',.TRUE.,AXES)
      CALL PAR_RDKEY('ERASE',.TRUE.,ERASE)
C
C     Get the label for the plot -
C
      CALL PAR_RDCHAR('LABEL',' ',LABEL)
C
C     Get the colour for the plot (note that BLACK and BLUE are reversed
C     in the colour list, so BL will be taken as BLUE - the codes are
C     then reversed to give the correct PGPLOT colour code)
C
      CALL PAR_RDCHAR('COLOUR','White',COLOUR)
      IF ( PAR_ABORT() ) GO TO 500
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
         CALL PAR_WRUSER('Cannot colour the plot '//COLOUR,STATUS)
         CKEY=1
      END IF
C
C     Check for 'HARD'
C
      HARD=.FALSE.
      CALL PAR_RDKEY('HARDCOPY',.FALSE.,HARD)
      IF ( PAR_ABORT() ) GO TO 500
      IF (HARD) THEN
         CALL VAR_GETCHR('HARD',0,0,DEVICE,STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('No hardcopy device specified.',STATUS)
            CALL PAR_WRUSER(
     :          'Use "HARD" command eg "HARD VER" to rectify.',STATUS)
            GO TO 500
         END IF
      ELSE
C
C        Look for the value of the user variable 'SOFT'
C
         CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('No plotting device specified.',STATUS)
            CALL PAR_WRUSER(
     :         'Use "SOFT" command eg "SOFT /VT" to rectify.',STATUS)
            GO TO 500
         END IF
      END IF
C
C     Make sure there is a range of data to plot.
C
      IF (IXST.EQ.IXEN) THEN
         IXST=MAX(IXST-1,1)
         IXEN=MIN(IXEN+1,ND1)
         XVST=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IXST)
         XVEN=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IXEN)
      END IF
C
C     Get the axis labels from the labels and units
C
      CALL FIG_MAKE_AXIS_LABEL(XLABEL,XUNITS,XLAB)
      CALL FIG_MAKE_AXIS_LABEL(DLABEL,DUNITS,DLAB)
C
C     Get workspace for PLHI2D
C
      CALL DSA_GET_WORK_ARRAY (ND1,'FLOAT',WPTR,WSLOT,STATUS)
C
C     Finally, perform the plot
C
      CALL FIG_PLHI2D(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),ND1,ND2,
     :                IY1,IY2,IXST,IXEN,HIGH,LOW,SLANT,OFFSET,XLAB,
     :                DLAB,LABEL,AXES,ERASE,DEVICE,CKEY,XVST,XVEN,
     :                %VAL(CNF_PVAL(WPTR)),STATUS)
C
C     Set the user variables describing the plot.
C
      CALL VAR_SETNUM('TVYST',0,0,YVST,STATUS)
      CALL VAR_SETNUM('TVYEN',0,0,YVEN,STATUS)
      IF (IYEN.GT.IYST) THEN
         TVDIM=2.
      ELSE
         TVDIM=1.
      END IF
      CALL VAR_SETNUM('TVDIM',0,0,TVDIM,STATUS)
      CALL VAR_SETNUM('TVXST',0,0,XVST,STATUS)
      CALL VAR_SETNUM('TVXEN',0,0,XVEN,STATUS)
      CALL VAR_SETNUM('TVHIGH',0,0,HIGH,STATUS)
      CALL VAR_SETNUM('TVLOW',0,0,LOW,STATUS)
      CALL VAR_SETCHR('TVFILE',0,0,'IMAGE',STATUS)
      CALL VAR_SETNUM('TVCOLOR',0,0,FLOAT(CKEY),STATUS)
C
C     Close down everything
C
500   CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_PLHI2D(XVALS,DVALS,NX,NY,IY1,IY2,IXST,IXEN,
     :                HIGH,LOW,SLANT,OFFSET,XLAB,DLAB,LABEL,AXES,ERASE,
     :                              DEVICE,CKEY,XVST,XVEN,WORK,STATUS)
C
C     F I G _ P L H I 2 D
C
C     Produces a hidden line histogram plot of a data array.
C
C     Parameters -   (">" input, "<" output, "W" work, "!" modified)
C
C     (>) XVALS    (Real array (1..NX) The abscissae
C                  for each point to be plotted.
C     (>) DVALS    (Real array (1..NX,1..NY) The data to be plotted.
C     (>) NX       (Integer) Number of elements in XVALS.
C     (>) NY       (Integer) Number of cross-sections in ZVALS.
C     (>) IY1      (Integer) The first cross-section to be plotted.
C     (>) IY2      (Integer) The last cross-section to be plotted.
C     (>) IXST     (Integer) The first array element to be plotted.
C     (>) IXEN     (Integer) The last array element to be plotted.
C     (>) HIGH     (Real) The maximum value for the plot.
C     (>) LOW      (Real) The minimum value for the plot.
C     (>) SLANT    (Integer) Number of pixels to offset successive
C                  cross-sections by in X when plotting.
C     (>) OFFSET   (Real) Amount to bias successive cross-sections
C                  in Y when plotting.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) DLAB     (Character) The data array label for the plot.
C     (>) LABEL    (Character) The label for the plot as a whole.
C     (>) AXES     (Logical) True if axes are to be plotted.
C     (>) ERASE    (Logical) True if device is to be erased first.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot - see PGPLOT documentation for details.
C                  If BUILD is true, DEVICE is the filename to be
C                  used for the 'build' file.
C     (>) CKEY     (Integer) The GRPCKG code (0..7) for the colour
C                  of the plot.
C     (!) XVST     (Real) The actual x-start value for the plot.
C     (!) XVEN     (Real) The actual x-end value for the plot.
C                  Note: XVST and XVEN are modified if SLANT is
C                  set, in order to accomodate the extra space needed.
C     (W) WORK     (Real array WORK(IXEN-IXST+1)) Workspace.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error opening the plot.
C
C     Subroutines / functions used -
C
C     PGBEGIN    (PGPLOT package) Open a plot file.
C     PGBOX      (  "      "    ) Draw the box for a plot.
C     PGADVANCE  (  "      "    ) Clear screen or start new plot.
C     PGWINDOW   (  "      "    ) Set the world-coordinate window.
C     PGVSTAND   (  "      "    ) Set the standard viewport.
C     PGEND      (  "      "    ) Terminate a plot.
C     PGLABEL    (  "      "    ) Label a plot.
C     PGHI2D     (  "      "    ) Plot a hidden line histogram.
C     PGSCI      (  "      "    ) Set plot colour
C     PAR_WRUSER (PAR_     "    ) Send a message to user.
C     ICH_LEN    (ICH_     "    ) Get length of string to trailing blanks
C
C                                       KS / CIT  26th July 1984
C     Modified:
C
C     17th Mar 1988  KS / AAO.  Modified for use with GKS version of PGPLOT.
C     17th Sep 1993  HME / UoE, Starlink.  Correct call to PGHI2D.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL AXES,ERASE
      INTEGER IY1,IY2,IXST,IXEN,NX,NY,SLANT,STATUS,CKEY
      REAL XVALS(NX),DVALS(NX,NY),HIGH,LOW,XVST,XVEN,OFFSET
      REAL WORK(IXEN-IXST+1)
      CHARACTER*(*) DEVICE,XLAB,DLAB,LABEL
      CHARACTER PGDEVL*64
C
C     Functions
C
      INTEGER ICH_LEN,PGBEGIN
C
C     Local variables
C
      INTEGER NSPECT
C
C     Colour for axes
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C     Ordinary plot.  Open plot.
C
      IF (ERASE) THEN
         STATUS=PGBEGIN(0,DEVICE,1,1)
      ELSE
         PGDEVL=DEVICE(:ICH_LEN(DEVICE))//'/APPEND'
         STATUS=PGBEGIN(0,PGDEVL,1,1)
      END IF
      IF (STATUS.EQ.1) THEN
C
C        Setup plot environment, and plot data.  Avoidance of PGENV
C        predates '/append' qualifier - provided control over screen
C        erasure.
C
         IF (ERASE) CALL PGADVANCE
         CALL PGSCI(WHITE)
         CALL PGVSTAND
         NSPECT=ABS(IY1-IY2)+1
         IF (NSPECT.GT.1) THEN
            IF (SLANT.GT.0) THEN
               XVEN=XVEN+
     :           (XVEN-XVST)*(NSPECT+1)*FLOAT(SLANT)/FLOAT(IXEN-IXST+1)
            ELSE IF (SLANT.LT.0) THEN
               XVST=XVST+
     :           (XVEN-XVST)*(NSPECT+1)*FLOAT(SLANT)/FLOAT(IXEN-IXST+1)
            END IF
         END IF
         CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
         IF (AXES) THEN
            CALL PGBOX('BCNST',0.,0,'BCNST',0.,0)
            CALL PGLABEL(XLAB,DLAB,LABEL)
         ELSE
            CALL PGLABEL(' ',' ',LABEL)
         END IF
         CALL PGSCI(CKEY)
C        CALL PGHI2D(DVALS,NX,NY,IXST,IXEN,IY1,IY2,XVALS,SLANT,
C    :                                       OFFSET,.TRUE.,WORK)
         CALL PGHI2D(DVALS,NX,NY,IXST,IXEN,IY1,IY2,XVALS(IXST),SLANT,
     :                                       OFFSET,.TRUE.,WORK)
         CALL PGSCI(WHITE)
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
