C+
      SUBROUTINE ICONT
C
C     I C O N T    /    I G R E Y
C
C     Handles an 'ICONT' command, producing a contour plot of an
C     image on either the current hard or soft graphics device,
C     or an 'IGREY' command, which produces a grey-scale plot.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the image to be contoured.
C     YSTART   (Numeric) The first Y value to be displayed.
C     YEND     (Numeric) The last Y value to be displayed.
C     XSTART   (Numeric) The first X value to be displayed.
C     XEND     (Numeric) The last X value to be displayed.
C              Note that this initial version only accepts these
C              values as pixel numbers.
C     LOW      (Numeric) The minimum contour level (ICONT) or
C              the black level (IGREY).
C     HIGH     (Numeric) The maximum contour level (ICONT) or
C              the white level (IGREY).
C     CONTOURS (Numeric) The number of contours displayed - these
C              will be divided evenly between HIGH and LOW, unless
C              explicitly specified as BYVALUE. (ICONT only).
C     LABEL    (Character) A label for the plot.
C     THICKNESS(Numeric) Thickness to use for lines (only used if
C              the HARDCOPY parameter is specified, and ICONT only)
C     LEVELS   (Numeric array) The contour levels to use (ICONT only,
C              and only if the BYVALUE keyword is specified).
C
C     Command keywords -
C
C     HARDCOPY Output the plot to the current hard graphics device.
C     ADJUST   Adjust scales so as to fill screen.
C     BYVALUE  For ICONT, indicates that explicit contour values are
C              specified in the LEVELS parameter.
C
C     User variables used -   (">" input, "<" output)
C
C     (>) SOFT     Current device/type (PGPLOT convention) for soft
C                  graphics output.
C     (>) HARD     Current device/type (PGPLOT convention) for hardcopy
C                  graphics output.
C     (<) TVXST    is set to the starting x-value for the plot.
C     (<) TVXEN    Is set to the final x-value for the plot.
C     (<) TVYST    is set to the starting y-value for the plot
C     (<) TVYEN    is set to the final y-value for the plot.
C     (<) TVHIGH   Is set to the same value as HIGH.
C     (<) TVLOW    Is set to the same value as LOW.
C     (<) PGENVARG Arguments used for PGENV.
C     (<) IMFILE   File containing the displayed image.
C
C                                               KS / CIT 21st March 1984.
C     Modified:
C
C     31 Oct 1985 KS / AAO. Minimum value for XEND corrected.
C     27 Nov 1985 KS / AAO. IGREY added.  LABEL added.
C     20 Jul 1987 DJA / AAO. Revised DSA_ routines - specs changed
C     29 Jul 1987 DJA / AAO. Now uses DYN_ routines for dynamic
C                 memory handling
C     04 Jan 1989 KS / AAO. Now takes notice of contents of X and
C                 Y axis structures, labeling the axes properly
C                 instead of just using pixel numbers.  THICKNESS,
C                 LEVELS and BYVALUE added.
C     29 Mar 1991 KS / AAO. Bug fix from JLC/CIT added - fixes
C                 problem when a subset of the image is selected.
C     23 Sep 1992 HME / UoE, Starlink.  TABs removed, INCLUDE changed.
C     16 Nov 1992 HME / UoE, Starlink.  Due to typo would set TVXEN
C                 twice and TVYEN not at all. Store the PGENV arguments
C                 in the variable array PGENVARG.
C     07 Apr 1993 HME / UoE, Starlink.  For IGREY make PGENV draw no
C                 box. Draw the box with PGBOX after PGGRAY.
C     10 Jan 1995 HME / UoE, Starlink.  Check that there is more than
C                 one pixel to plot in each direction.
C     11 Jan 1995 HME / UoE, Starlink.  Trial of AGI compliance, i.e.
C                 use FIG_PGBEG/FIG_PGEND instead of PGBEGIN/PGEND.
C     18 Jul 1996 MJCL / Starlink, UCL.  Set variables for storage of
C                 file names to 132 chars.
C     26 Jul 1996 MJCL / Starlink, UCL.  Added PAR_ABORT checks.
C     2005 June 8 MJC / Starlink  Use CNF_PVAL for pointers to
C                 mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL FIG_SCRCHK, PAR_ABORT
C
C     Maximum number of contours
C
      INTEGER MAX_CONT
      PARAMETER (MAX_CONT=50)
C
C     Local variables
C
      LOGICAL   ADJUST           ! See above
      INTEGER   ASLOT            ! Map slot used for axis data
      LOGICAL   AXES             ! TRUE if axes are to be drawn
      INTEGER   AXIS             ! Loop index through axes
      INTEGER   AXPTR            ! Dynamic memory element for axis data
      LOGICAL   BYVALUE          ! Value of BYVALUE keyword
      CHARACTER COMMAND*8        ! Actual FIGARO command passed
      CHARACTER DEVICE*32        ! PGPLOT device specification
      INTEGER   DDIMS(10)        ! Sizes of the dimensions of the data
      REAL      DELTA            ! Increment between contour levels
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      DOUBLE PRECISION DUMMY     ! Dummy argument for GET_AXIS_INFO
      LOGICAL   ERASE            ! TRUE if the screen is to be wiped
      LOGICAL   EXIST            ! True if axis data array exists
      LOGICAL   HARDCOPY         ! True if hardcopy is required
      REAL      HIGH             ! The highest brightness level
      INTEGER   I                ! General loop index
      LOGICAL   IGNORE           ! Used to ignore status codes
      INTEGER   IXEN             ! Last element to be plotted in x-axis
      INTEGER   IXST             ! First element to be plotted in x-axis
      INTEGER   IYEN             ! Last element to be plotted in y-axis
      INTEGER   IYST             ! First element to be plotted in y-axis
      LOGICAL   KNOWN            ! True if data range already known
      CHARACTER LABEL*64         ! The group label for all the plots
      REAL      LEVELS(MAX_CONT) ! Values of each of the contour levels
      REAL      LOW              ! The lowest brightness level
      INTEGER   NCONT            ! The number of contours
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   NDIM             ! Number of dimensions in data
                                 ! structure
      INTEGER   NX               ! The size of the data's 1st dimension
      INTEGER   NY               ! The size of the data's 2nd dimension
      LOGICAL   SAME_UNITS       ! True if units for both axes are same
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRINGS(2)*64    ! Axis labels and units
      INTEGER   THICK            ! Thickness for hardcopy plots
      REAL      XEND             ! Last X axis value to plot
      CHARACTER XLAB*64          ! Label for X axis
      REAL      XSTART           ! First X axis value to plot
      CHARACTER XUNITS*16        ! Units for X axis
      REAL      YEND             ! Last Y axis value to plot
      CHARACTER YLAB*64          ! Label for Y axis
      REAL      YSTART           ! First Y axis value to plot
      REAL      VALUE            ! Temporary REAL number
      REAL      VMAX             ! Maximum data value
      REAL      VMIN             ! Minimum data value
      REAL      PGARRAY(6)       ! The PGENV arguments used
      CHARACTER INAME*132        ! Actual name of the image
C
C     Limiting values for HIGH and LOW.  (somewhat arbitrary, each a
C     factor ten from the VAX limits)
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E37,FMIN=-1.7E37)
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get the command name
C
      CALL PAR_COMMAND(COMMAND)
C
C     Get the object name and open the file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     We now have an object name, start to find out about it.
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DDIMS,NDELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('This is not an image',IGNORE)
         IF (NDIM.GT.2) THEN
            CALL PAR_WRUSER('But it has more than 2 dimensions, so',
     :                                                       IGNORE)
            CALL PAR_WRUSER('let''s have a go at it',IGNORE)
         ELSE
            GO TO 500
         END IF
      END IF
C
C     Map the data
C
      NX=DDIMS(1)
      NY=DDIMS(2)
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Check on the axis values.  If the image has non-linear axis
C     values, we ought to warn the user, because the algorithm assumes
C     that they are linear.
C
      DO AXIS=1,2
         CALL DSA_SEEK_AXIS ('IMAGE',AXIS,EXIST,STATUS)
         IF (EXIST) THEN
            CALL DSA_MAP_AXIS_DATA ('IMAGE',AXIS,'READ','FLOAT',AXPTR,
     :                              ASLOT,STATUS)
            IF (.NOT.FIG_SCRCHK(DDIMS(AXIS),%VAL(CNF_PVAL(AXPTR)))) THEN
               CALL PAR_WRUSER('Warning - axis data is not linear. The '
     :                 //'axis scales will only be approximate.',IGNORE)

            END IF
            CALL DSA_UNMAP(ASLOT,STATUS)
         END IF
      END DO
C
C     Get axis range parameters (XSTART,XEND,YSTART,YEND) and axis
C     information.
C
      CALL DSA_AXIS_RANGE('IMAGE',2,' ',.FALSE.,YSTART,YEND,IYST,
     :                                               IYEN,STATUS)
      CALL DSA_AXIS_RANGE('IMAGE',1,' ',.FALSE.,XSTART,XEND,IXST,
     :                                               IXEN,STATUS)
      CALL DSA_GET_AXIS_INFO ('IMAGE',1,2,STRINGS,0,DUMMY,STATUS)
      CALL FIG_MAKE_AXIS_LABEL (STRINGS(2),STRINGS(1),XLAB)
      XUNITS=STRINGS(1)
      CALL DSA_GET_AXIS_INFO ('IMAGE',2,2,STRINGS,0,DUMMY,STATUS)
      CALL FIG_MAKE_AXIS_LABEL (STRINGS(2),STRINGS(1),YLAB)
      SAME_UNITS=XUNITS.EQ.STRINGS(1)
      IF (STATUS.NE.0) GO TO 500
C
C     Check that the image subset is not degenerate
C
      IF (IYEN.LE.IYST.OR.IXEN.LE.IXST) THEN
         CALL PAR_WRUSER('Error - image subset is not 2-D.', STATUS)
         GO TO 500
      END IF
C
C     Get data value limits (HIGH, LOW). Work out contour levels
C     for ICONT.  If we know the data range, make use of that for
C     the default levels, but don't waste time determining them
C     if they aren't already known.  If specific contour values are
C     to be specified, use them instead of working out levels from
C     HIGH and LOW.
C
      CALL DSA_SEEK_RANGE ('IMAGE',KNOWN,STATUS)
      IF (KNOWN) THEN
         CALL DSA_GET_RANGE('IMAGE',VMIN,VMAX,STATUS)
      ELSE
         VMIN=0.0
         VMAX=1000.0
      END IF
      IF (COMMAND.EQ.'ICONT') THEN
         CALL PAR_RDKEY('BYVALUE',.FALSE.,BYVALUE)
         IF (.NOT.BYVALUE) THEN
            CALL PAR_RDVAL('LOW',FMIN,FMAX,VMIN,' ',LOW)
            CALL PAR_RDVAL('HIGH',FMIN,FMAX,VMAX,' ',HIGH)
            CALL PAR_RDVAL('CONTOURS',1.,FLOAT(MAX_CONT),11.,' ',VALUE)
            NCONT=VALUE
         END IF
C
C        If values are to be specified explicitly, then we ask for them
C        using PAR_RDARY.  The only question is what to use as the reset
C        values.  Again, if the data range is known, we can make a good
C        set of guesses.  If it isn't, we make a bad set of guesses.
C        (Note that the parameter system can only allow a maximum of
C        30 values for an array parameter - at present).
C
         IF (BYVALUE) THEN
            CALL PAR_RDVAL('CONTOURS',1.,FLOAT(MIN(30,MAX_CONT)),
     :                                                   11.,' ',VALUE)
            NCONT=VALUE
            IF (KNOWN.AND.(NCONT.GT.1)) THEN
               DELTA=(VMAX-VMIN)/(FLOAT(NCONT)-1.)
               VALUE=VMIN
            ELSE
               VALUE=0.0
               DELTA=100.0
            END IF
            DO I=1,NCONT
               LEVELS(I)=VALUE
               VALUE=VALUE+DELTA
            END DO
            CALL PAR_RDARY('LEVELS',FMIN,FMAX,'None',' ',NCONT,
     :                                     MIN(30,MAX_CONT),LEVELS)
         ELSE
C
C           If contours are not specified explicitly, we just calculate
C           a set of evenly spaced values between HIGH and LOW.
C
            IF (NCONT.GT.1) THEN
               DELTA=(HIGH-LOW)/(FLOAT(NCONT)-1.)
            ELSE
               DELTA=0.
            END IF
            VALUE=LOW
            DO I=1,NCONT
               LEVELS(I)=VALUE
               VALUE=VALUE+DELTA
            END DO
         END IF
      ELSE
C
C        For IGREY, all we want is HIGH and LOW
C
         CALL PAR_RDVAL('LOW',FMIN,FMAX,VMIN,' ',LOW)
         CALL PAR_RDVAL('HIGH',FMIN,FMAX,VMAX,' ',HIGH)
      END IF
C
C     Plot axes?  Erase screen?  Note, this code is dormant at the
C     moment, waiting for the time when a PGPLOT routine that will
C     set an adjusted viewport is available.
C
C     CALL PAR_RDKEY('AXES',.TRUE.,AXES)
C     CALL PAR_RDKEY('ERASE',.TRUE.,ERASE)
      AXES=.TRUE.
      ERASE=.TRUE.
C
C     Adjust scales to fill screen?  This is a little tricky, since the
C     adjusting is done in terms of the axis values.  In general, it only
C     makes sense not to adjust if the axes are in the same units, so
C     we make that test and adjust the default accordingly.  (If the
C     axis units differ, there isn't any sensible relationship between them
C     so the most sensible thing is to adjust the plot to fill the screen -
C     that's the argument, anyway.)
C
      CALL PAR_RDKEY('ADJUST',.NOT.SAME_UNITS,ADJUST)
C
C     HARD or SOFT plot?
C
      CALL PAR_RDKEY('HARDCOPY',.FALSE.,HARDCOPY)
      IF (PAR_ABORT()) GO TO 500      ! User requested abort
      IF (HARDCOPY) THEN
         CALL VAR_GETCHR('HARD',0,0,DEVICE,STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('No hardcopy device specified.',IGNORE)
            CALL PAR_WRUSER(
     :          'Use "HARD" command eg "HARD VER" to rectify.',IGNORE)
            GO TO 500
         END IF
      ELSE
         CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('No plotting device specified.',IGNORE)
            CALL PAR_WRUSER(
     :        'Use "SOFT" command eg "SOFT device" to rectify.',IGNORE)
            GO TO 500
         END IF
      END IF
C
C     Get the label for the plot -
C
      CALL PAR_RDCHAR('LABEL',' ',LABEL)
C
C     For a hardcopy contour plot, we want to know the thickness
C
      IF ((COMMAND.EQ.'ICONT').AND.HARDCOPY) THEN
         CALL PAR_RDVAL('THICKNESS',1.,21.,1.,'Dots',VALUE)
         THICK=VALUE
      ELSE
         THICK=1
      END IF
C
C     Generate the plot, depending on the command being processed.
C
      IF (PAR_ABORT()) GO TO 500      ! User requested abort
      IF (COMMAND.EQ.'ICONT') THEN
         CALL FIG_CPLOT(%VAL(CNF_PVAL(DPTR)),NX,NY,IXST,IXEN,XSTART,
     :                  XEND,IYST,IYEN,YSTART,YEND,DEVICE,XLAB,YLAB,
     :                  LABEL,ERASE,AXES,ADJUST,LEVELS,NCONT,THICK,
     :                  STATUS)
      ELSE
         CALL FIG_GPLOT(%VAL(CNF_PVAL(DPTR)),NX,NY,IXST,IXEN,XSTART,
     :                  XEND,IYST,IYEN,YSTART,YEND,DEVICE,XLAB,YLAB,
     :                  LABEL,ERASE,AXES,ADJUST,HIGH,LOW,STATUS)
      END IF
C
C     Set the user variables describing the plot.
C
      CALL VAR_SETNUM('TVXST',0,0,FLOAT(IXST),STATUS)
      CALL VAR_SETNUM('TVXEN',0,0,FLOAT(IXEN),STATUS)
      CALL VAR_SETNUM('TVYST',0,0,FLOAT(IYST),STATUS)
      CALL VAR_SETNUM('TVYEN',0,0,FLOAT(IYEN),STATUS)
      CALL VAR_SETNUM('TVHIGH',0,0,HIGH,STATUS)
      CALL VAR_SETNUM('TVLOW',0,0,LOW,STATUS)
C
C     Set a user variable array that tells how PGENV was called.
C
      PGARRAY(1)=XSTART
      PGARRAY(2)=XEND
      PGARRAY(3)=YSTART
      PGARRAY(4)=YEND
      IF (ADJUST) THEN
         PGARRAY(5)=0
      ELSE
         PGARRAY(5)=1
      END IF
      PGARRAY(6)=0
      CALL VAR_SETARY('PGENVARG',6,PGARRAY,STATUS)
C
C     Store the image file name for other applications to retrieve.
C
      CALL DSA_GET_ACTUAL_NAME('IMAGE',INAME,STATUS)
      CALL VAR_SETCHR('IMFILE',0,0,INAME,STATUS)
C
C     Close down
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END
C+
      SUBROUTINE FIG_CPLOT(DATA,NX,NY,IXST,IXEN,XSTART,XEND,IYST,IYEN,
     :                YSTART,YEND,DEVICE,XLAB,YLAB,LABEL,ERASE,AXES,
     :                                ADJUST,LEVELS,NCONT,THICK,STATUS)
C
C     F I G _ C P L O T
C
C     Produces a contour plot of a 2D array.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) DATA     (Real array DATA(NX,NY)) The data to be contoured.
C     (>) NX       (Integer) The number of x-pixels in DATA.
C     (>) NY       (Integer) The number of y-pixels in DATA.
C     (>) IXST     (Integer) The first x-pixel to be used.
C     (>) IXEN     (Integer) The last x-pixel to be used.
C     (>) XSTART   (Real) X axis value for first X pixel.
C     (>) XEND     (Real) X axis value for last X pixel.
C     (>) IYST     (Integer) The first y-pixel to be used.
C     (>) IYEN     (Integer) The last y-pixel to be used.
C     (>) YSTART   (Real) Y axis value for first Y pixel.
C     (>) YEND     (Real) Y axis value for last Y pixel.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot, in the form required by FIG_PGBEG.
C     (>) XLAB     (Character) Label for X axis.
C     (>) YLAB     (Character) Label for Y axis.
C     (>) LABEL    (Character) A label for the plot.
C     (>) ERASE    (Logical) Erase the screen before plotting, if true.
C     (>) AXES     (Logical) Plot and label axes, if true.
C     (>) LEVELS   (Real array LEVELS(NCONT)) The contour levels to
C                  be plotted.
C     (>) ADJUST   (Logical) Adust scales so as to fil display.
C     (>) NCONT    (Integer) The number of levels to be plotted.
C     (>) THICK    (Integer) The line thickness for the plot.
C     (<) STATUS   (Integer) Return status.  0 => OK, non-zero
C                  indicates an error code from FIG_PGBEG.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     FIG_PGBEG      (PGPLOT) Initialise PGPLOT routines
C     PGCONT       (  "   ) Plot contour map of data
C     FIG_PGEND        (  "   ) Close PGPLOT routines
C     PGENV        (  "   ) Set plotting environment
C     PGSLW        (  "   ) Set line width
C
C                                      KS / CIT 21st March 1984
C     Modified:
C
C     27th Nov 1985.  KS / AAO.  LABEL, ERASE and AXES added.
C                     At the moment, AXES and ERASE are ignored.
C     4th Jan 1989.   KS / AAO.  XSTART,XEND,YSTART,YEND,XLAB,YLAB
C                     parameters added.  Program now takes note of
C                     the axis data values, instead of just using pixel
C                     numbers.  THICK also added.
C     29th Mar 1991.  KS/AAO. Fix from JLC/CIT re calculation of TR(1)
C                     and TR(4) incorporated.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ADJUST,AXES,ERASE
      INTEGER NX,NY,IXST,IXEN,IYST,IYEN,NCONT,THICK,STATUS
      REAL DATA(NX,NY),LEVELS(NCONT),XSTART,YSTART,XEND,YEND
      CHARACTER*(*) DEVICE,LABEL,XLAB,YLAB
C
C     Functions
C
      INTEGER FIG_PGBEG
C
C     Local variables
C
      INTEGER IADJ
      REAL TR(6)
C
C     Set up the transformation matrix
C
      TR(3)=0.0
      TR(5)=0.0
      TR(2)=(XEND-XSTART)/FLOAT(IXEN-IXST)
      TR(6)=(YEND-YSTART)/FLOAT(IYEN-IYST)
      TR(1)=XSTART-TR(2)*FLOAT(IXST)
      TR(4)=YSTART-TR(6)*FLOAT(IYST)
C
C     Perform the plot
C
      STATUS=FIG_PGBEG(0,DEVICE,1,1)
      IF (STATUS.EQ.1) THEN
         STATUS=0
         IF (ADJUST) THEN
            IADJ=0
         ELSE
            IADJ=1
         END IF
         CALL PGSLW(THICK)
         CALL PGENV(XSTART,XEND,YSTART,YEND,IADJ,0)
         CALL PGLABEL(XLAB,YLAB,LABEL)
         CALL PGCONT(DATA,NX,NY,IXST,IXEN,IYST,IYEN,LEVELS,NCONT,TR)
         CALL FIG_PGEND
      END IF
C
      END
C+
      SUBROUTINE FIG_GPLOT(DATA,NX,NY,IXST,IXEN,XSTART,XEND,IYST,IYEN,
     :                YSTART,YEND,DEVICE,XLAB,YLAB,LABEL,ERASE,AXES,
     :                                         ADJUST,HIGH,LOW,STATUS)
C
C     F I G _ C P L O T
C
C     Produces a grey scale plot of a 2D array.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) DATA     (Real array DATA(NX,NY)) The data to be contoured.
C     (>) NX       (Integer) The number of x-pixels in DATA.
C     (>) NY       (Integer) The number of y-pixels in DATA.
C     (>) IXST     (Integer) The first x-pixel to be used.
C     (>) IXEN     (Integer) The last x-pixel to be used.
C     (>) XSTART   (Real) X axis value for first X pixel.
C     (>) XEND     (Real) X axis value for last X pixel.
C     (>) IYST     (Integer) The first y-pixel to be used.
C     (>) IYEN     (Integer) The last y-pixel to be used.
C     (>) YSTART   (Real) Y axis value for first Y pixel.
C     (>) YEND     (Real) Y axis value for last Y pixel.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot, in the form required by FIG_PGBEG.
C     (>) XLAB     (Character) Label for X axis.
C     (>) YLAB     (Character) Label for Y axis.
C     (>) LABEL    (Character) A label for the plot.
C     (>) ERASE    (Logical) Erase the screen before plotting, if true.
C     (>) AXES     (Logical) Plot and label axes, if true.
C     (>) ADJUST   (Logical) Adust scales so as to fil display.
C     (>) HIGH     (Real) Max data value to be used (the white level)
C     (>) LOW      (Real) Min data value to be used (the black level)
C     (<) STATUS   (Integer) Return status.  0 => OK, non-zero
C                  indicates an error code from FIG_PGBEG.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     FIG_PGBEG      (PGPLOT) Initialise PGPLOT routines
C     PGGRAY       (  "   ) Plot grey scale map of data
C     FIG_PGEND        (  "   ) Close PGPLOT routines
C     PGENV        (  "   ) Set plotting environment
C
C                                      KS / AAO 27th Nov 1985
C
C     Note: At present, AXES and ERASE are ignored.
C
C     Modified:
C
C     4th Jan 1989.   KS / AAO.  XSTART,XEND,YSTART,YEND,XLAB,YLAB
C                     parameters added.  Program now takes note of
C                     the axis data values, instead of just using pixel
C                     numbers.
C     29th Mar 1991.  KS/AAO. Fix from JLC/CIT re calculation of TR(1)
C                     and TR(4) incorporated.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ADJUST,AXES,ERASE
      INTEGER NX,NY,IXST,IXEN,IYST,IYEN,STATUS
      REAL DATA(NX,NY),HIGH,LOW,XSTART,YSTART,XEND,YEND
      CHARACTER*(*) DEVICE,LABEL,XLAB,YLAB
C
C     Functions
C
      INTEGER FIG_PGBEG
C
C     Local variables
C
      INTEGER IADJ
      REAL TR(6)
C
C     Set up the transformation matrix
C
      TR(3)=0.0
      TR(5)=0.0
      TR(2)=(XEND-XSTART)/FLOAT(IXEN-IXST)
      TR(6)=(YEND-YSTART)/FLOAT(IYEN-IYST)
      TR(1)=XSTART-TR(2)*FLOAT(IXST)
      TR(4)=YSTART-TR(6)*FLOAT(IYST)
C
C     Perform the plot
C
      STATUS=FIG_PGBEG(0,DEVICE,1,1)
      IF (STATUS.EQ.1) THEN
         STATUS=0
         IF (ADJUST) THEN
            IADJ=0
         ELSE
            IADJ=1
         END IF
         CALL PGENV(XSTART,XEND,YSTART,YEND,IADJ,-2)
         CALL PGGRAY(DATA,NX,NY,IXST,IXEN,IYST,IYEN,LOW,HIGH,TR)
         CALL PGBOX('BCNST',0.,0,'BCNST',0.,0)
         CALL PGLABEL(XLAB,YLAB,LABEL)
         CALL FIG_PGEND
      END IF
C
      END
