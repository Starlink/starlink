      SUBROUTINE PISAPLOT( STATUS )
*+
*  Name:
*     PISAPLOT

*  Purpose:
*     Plots an ellipse map of the objects found by PISAFIND.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAPLOT( STATUS )

*  Description:
*     This application shows the positions, shapes and sizes of the
*     objects located by PISAFIND via a plot of ellipses, one per
*     object. Optional annotation of the ellipses provide
*     cross-referencing with the object list. The plot may be overlaid
*     on an existing image for a direct comparison. In this case the
*     plot appears within the last DATA picture in the graphics
*     database, otherwise it is situated within the current picture.
*
*     Control of the plot allows labelled axes whose extent may be
*     defined and the selection of the colour of the ellipses

*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the plot abscissa, may include PGPLOT escape
*        sequences.  This parameter is only used when the axes option
*        is selected. [X]
*     ANNOTA = _LOGICAL (Read)
*        If true the ellipses are annotated with the object
*        identification number to the top right. [TRUE]
*     ANNOSCALE = REAL (Read)
*        The scale height of the annotations. This value is a multiple
*        of the normal text height.[1.0]
*     AXES = _LOGICAL (Read)
*        True if annotated axes are to be drawn around the displayed
*        image. This parameter is ignored in the overlay mode, since
*        there is no guarantee that the axes would lie entirely
*        within the current picture. [TRUE]
*     CLEAR = _LOGICAL (Read)
*        True if the device is to be cleared first when in overlay mode.
*        Useful if displaying different classes of objects successively.
*        This flag acts as a switch retaining its last value. [FALSE]
*     DEFAXES = _LOGICAL (Read)
*        If set true then prompting for the axis bounds will occur,
*        otherwise the program generated defaults will be used. [FALSE]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device on which to plot the map
*        of images found. If the overlay mode is required it is
*        recommended that the image be displayed in KAPPA on an
*        image display's base plane, then run this application using
*        the device's overlay plane. [Current graphics device]
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks for
*        the x and y axes.  A negative value for an axis makes the
*        graphics package decide an appropriate value.  This parameter
*        is only used when the axes option is selected. [-1.,-1.]
*     MINTIC( 2 ) = _INTEGER (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values.   This parameter is
*        only used when the axes option is selected. [-1.,-1.]
*     ORDLAB  =  LITERAL (Read)
*        Label for the plot ordinate, may include PGPLOT escape
*        sequences.  This parameter is only used when the axes option
*        is selected. [Y]
*     OUTTIC = _LOGICAL (Read)
*        True if the axis tick marks are to appear on the outside of
*        the axes instead of inside.   This parameter is only used
*        when the axes option is selected. [TRUE]
*     OVERLAY = _LOGICAL (Read)
*        True if the plot is to be overlaid on the last DATA picture
*        in the graphics database.  This is used to compare a displayed
*        2-d image with the objects detected. This flag acts as a switch
*        retaining its last value. [FALSE]
*     PALNUM = _INTEGER (Read)
*        PISAPLOT allows the user to specify which pen number to use
*        when plotting. Thus different classifications of objects can
*        be identified on the same plot using different colours. The
*        colours associated with these pens are the default PGPLOT
*        pens (see the PGPLOT manual for a complete description).
*        These are:
*               0 = background colour
*               1 = foreground colour
*               2 = red
*               3 = green
*               4 = blue
*               5 = cyan
*               6 = magenta
*               7 = yellow
*               8 = orange
*        and so on up to pen 16 ( up to the number available on the
*        current graphics device ). After PISAPLOT has been run these
*        colours can be superceded by using the KAPPA palette
*        facilities PALDEF and PALENTRY, but note that any subsequent
*        runs of PISAPLOT will reinstate the PGPLOT default colours so
*        using the KAPPA facilities should be delayed until all object
*        classifications have been displayed. The KAPPA palette pen
*        numbers correspond to PALNUM values (hence the parameter name).
*        [3]
*     PLTITL = CHAR (Read)
*        The title of the plot, may include PGPLOT escape sequences.
*        Up to about 40 characters can be accommodated.  This parameter
*        is only used when axes option is selected. [Images Detected]
*     RESULTS = FILENAME (Read)
*        The ASCII file produced by the PISAFIND application containing
*        the parameterised data for the various objects it detected.
*        [PISAFIND.DAT]
*     THICK = _REAL (Read)
*        The thickness of the lines in the plot, where 1.0 is the
*        normal thickness. It should be between 0.5 and 5. This
*        feature is only available on some devices. This parameter is
*        only used when axes option is selected. [1.0]
*     XPIXS = _REAL (Read)
*        Initial and final pixel coordinates of plot in x-direction.
*        [Dynamic]
*     YPIXS = _REAL (Read)
*        Initial and final pixel coordinates of plot in y-direction.
*        [Dynamic]

*  Examples:
*     PISAPLOT OVERLAY
*        Using the overlay parameter allows ellipses to be plotted over
*        a previously displayed image.
*     PISAPLOT OVERLAY CLEAR
*        Including the clear parameters clears a plot before overlaying.
*        This is useful when displaying the overlaid ellipses in an
*        overlay.
*     PISAPLOT RESULTS=STARS.DAT PALNUM=3 OVERLAY
*        Using this combination of parameters would use a green pen to
*        plot the ellipse overlaying them on the displayed picture.
*        Using sequences of this example with difference results files
*        and palnums would result in a display which had different
*        colours for each results file displayed over the current
*        picture.

*  Notes:
*     -  The following pictures are stored in the graphics database: a
*     FRAME encompassing annotated axes and the plot itself, provided
*     the overlay option is not selected; a DATA picture for the plot
*     itself.  The latter uses the standard Starlink co-ordinate
*     system.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 July 31 (MJC):
*        Original version.
*     1990 October 22 (PDRAPER):
*        Changed to read in pixel area not index (bug).
*        Used MCA (ignored previously) parameter to allow room for
*        annotations. Changed 0.75 to 2.0; character aspect ratio
*        plus a bit. Found the maximum index number in file, external
*        manipulations (SCAR) of pisafind file could cause reordering.
*     1990 October 30 (PDRAPER):
*        Changed to offer coordinates of axis to user.
*     22-NOV-1990 (PDRAPER):
*        Added AGI_END and AGI_BEGIN.
*     21-JAN-1991 (PDRAPER):
*        Added 1X's in format to correct fortran reads.
*     14-MAR-1991 (PDRAPER):
*        Changed reads to buffered FIO calls. Added clear parameter
*        and changed Overlay to use current value.
*     3-SEP-1991 (PDRAPER):
*        Removed white pen calls - X windows allows other colours for
*        background.
*     3-FEB-1992 (PDRAPER):
*        Added PALNUM parameter.
*     24-MAR-1992 (PDRAPER):
*        Changed graphics to PGPLOT.
*     18-MAR-1993 (PDRAPER):
*        Stopped ellipticity of 1 breaking routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'FIO_ERR'          ! FIO error defintions
      INCLUDE 'FIO_PAR'          ! FIO parameterisations
      INCLUDE 'PRM_PAR'          ! Magic-value definitions
      INCLUDE 'PAR_ERR'          ! Parameter-system error definitions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER( NDIM = 2 )

      INTEGER NELPS              ! Number of polyline segments for each
                                 ! ellipse.
      PARAMETER( NELPS = 100 )

      REAL PI                    ! Pi
      PARAMETER( PI = 3.141592 )

      REAL DGTRD                 ! Degrees to radians
      PARAMETER( DGTRD = PI / 180. )

      REAL THT                   ! Text height as a fraction of the
                                 ! picture height
      PARAMETER( THT = 0.002 )
      REAL ALMOST                ! Almost 1
      PARAMETER ( ALMOST = 0.99999 )


*  Local Variables:
      LOGICAL                    ! True if :
     :  ANNOTA,                  ! The ellipses are annotated with id
     :  ANOTHR,                  ! Another line of the data file is
                                 ! to be read
     :  AXES,                    ! Annotated axes are to be drawn
     :  CATPOS,                  ! The data file is open
     :  DEVCAN,                  ! Image-display parameter is to be
                                 ! cancelled
     :  OUTTIC,                  ! Axis tick marks are to be placed
                                 ! outside the box instead of inside
     :  OVRLAY,                  ! The plot is an overlay on an image
     :  NEWAX,                   ! The user wants to change the plot
     :                           ! axes scales.
     :  CLEAR,                   ! whether or not the user requires the
     :                           ! overlay plane cleared before
     :                           ! plotting.
     :  LOBS                     ! Temporary flag

      REAL
     :  EA,                      ! Semi-major axis of an ellipse
     :  EB,                      ! Semi-minor axis of an ellipse
     :  ELL,                     ! Ellipticity of an image
     :  HT,                      ! Text height
     :  LBND( NDIM ),            ! Lower bounds of the image
     :  UBND( NDIM ),            ! Upper bounds of the image
     :  MAJTIC( 2 ),             ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively
     :  MAWID                    ! Approx maximum width of the annotations

      REAL
     :  OA,                      ! Orientation of the annotation with
                                 ! respect to the ellipse in polar
                                 ! co-ordinates
     :  PA,                      ! Position angle of an image
     :  RAD,                     ! Mean radius of an image
     :  RPA,                     ! Position angle of an image in radians
     :  WDIM,                    ! Width of the plotting region in pixels
     :  XA, YA                   ! Position of the annotation

      REAL
     :  XE( NELPS ),             ! X locus of an ellipse
     :  YE( NELPS ),             ! Y locus of an ellipse
     :  XP, YP,                  ! Centroid of an object
     :  RVAL( 2 ),               ! buffer to hold plot limits
     :  INTENS,                  ! intensity of object (not used)
     :  PEAK,                    ! intensity of objects (not used)
     :  SXX,
     :  SYY,
     :  SXY,                     ! intensity weighted moments (not used)
     :  TXTSCL                   ! Scale factor for text height.

      REAL
     :  WX1,                     ! World coordinates of base picture
     :  WX2,
     :  WY1,
     :  WY2

      CHARACTER * ( 72 )
     :  ABSLAB,                  ! Label for the abscissa of the plot
     :  BUFFER * ( 132 ),        ! Buffer for reading the data file
     :  ORDLAB,                  ! Label for the ordinate of the plot
     :  PLTITL,                  ! Title of the plot
     :  PNAME * ( DAT__SZNAM ),  ! Name of the input picture
     :  NUM * ( 20 ),            ! Index number of object
     :  FORSTR * ( 12 ),         ! String specifying axes control
     :  FNAME * ( FIO__SZFNM )   ! Results file name

      INTEGER
     :  FD,                      ! File description of parameterised data
     :  THICK,                   ! The line thickness (standard is 1.0)
     :  IDNO,                    ! Identification number of an image
     :  MCA,                     ! Maximum number of characters in
                                 ! annotation
     :  N,                       ! Number of lines read from the data
                                 ! file
     :  NPIX,                    ! The number of pixels in an image
     :  PICID,                   ! Graphics' database identifier on input
     :  PICIDF,                  ! Graphics' database identifier for
                                 ! the frame including axes
     :  PICIDI,                  ! DATA image picture identifier
     :  PICIDT,                  ! Work picture identifier
     :  TSTAT                    ! Temporary status

      INTEGER
     :  MAXIND,                  ! the maximum index value of entries
     :  NVAL,                    ! the number of values returned by get1r
     :  IPEN,                    ! Pen number selected
     :  MINTIC( 2 )              ! Numbers of minor tick marks along x and
                                 ! y axes respectively

      INTEGER
     :   CI1,                    ! Minimum colour index
     :   CI2                     ! Maximum colour index
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Open the catalogue of image parameters read by images.
      CALL PSA1_ASFIO( 'RESULTS', 'READ', 'LIST', 0, FD, CATPOS,
     :                 STATUS )
      CALL FIO_FNAME( FD, FNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*    See whether the plot is to be an overlay or not.
      OVRLAY = .TRUE.
      CALL PAR_GET0L( 'OVERLAY', OVRLAY, STATUS )
      IF ( OVRLAY ) THEN

*    If in overlay mode see if the device is to be cleared first. This
*    is useful if displaying different classes of objects over the same
*    displayed image.
         CLEAR = .FALSE.
         CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )
      END IF

*    Inquire the style of the plot.
*    ==============================

*    Whether or not annotated ellipses are required.
      ANNOTA = .TRUE.
      CALL PAR_GET0L( 'ANNOTA', ANNOTA, STATUS )

*    Whether or not annotated axes are required.
      AXES = .FALSE.
      IF ( .NOT. OVRLAY ) CALL PAR_GET0L( 'AXES', AXES, STATUS )
      IF ( AXES ) THEN

*       Start a new error context.
         CALL ERR_MARK

*       Obtain the title for the plot.
         CALL PAR_DEF0C( 'PLTITL', 'Images Detected', STATUS )
         CALL PAR_GET0C( 'PLTITL', PLTITL, STATUS )
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
            PLTITL = ' '
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 980
         END IF

*       Obtain axis labels.  A null value causes the default to be
*       chosen, namely, 'X' for the abscissa...
         CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
            ABSLAB = 'X'
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 980
         END IF

*       ...and 'Y' for the ordinate.
         CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
            ORDLAB = 'Y'
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 980
         END IF

*       Get the number of minor ticks.
         CALL PAR_GET1I( 'MINTIC', 2, MINTIC, NVAL, STATUS )
         IF ( MINTIC( 1 ) .LT. 0 ) MINTIC( 1 ) = 0
         IF ( MINTIC( 2 ) .LT. 0 ) MINTIC( 2 ) = 0

*       Get the parameter controlling the number of major ticks per
*       axis.
         CALL PAR_GET1R( 'MAJTIC', 2, MAJTIC, NVAL, STATUS )
         IF ( MAJTIC( 1 ) .LT. 0.0 ) MAJTIC( 1 ) = 0.0
         IF ( MAJTIC( 2 ) .LT. 0.0 ) MAJTIC( 2 ) = 0.0

*       Are the tick marks on the outside of the axes?  Default is
*       outside so that they do not hide any of the image.
        OUTTIC = .TRUE.
        CALL PAR_GET0L( 'OUTTIC', OUTTIC, STATUS )
        IF ( OUTTIC ) THEN
           FORSTR = 'TBCINS'
        ELSE
           FORSTR = 'TBCNS'
        END IF

*       Release the error context.

         CALL ERR_RLSE
         IF ( STATUS .EQ. PAR__ABORT ) GOTO 980
      END IF

*    Open the device and obtain the frame zone in which to plot.
*    ===========================================================
*
*    Associate a graphics device in the database. Clear the overlay
*    plane only is requested to.
      IF ( OVRLAY ) THEN
         IF( CLEAR ) THEN
            CALL AGI_ASSOC( 'DEVICE', 'WRITE', PICID, STATUS )
         ELSE
            CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )
         END IF
      ELSE
         CALL AGI_ASSOC( 'DEVICE', 'WRITE', PICID, STATUS )
      END IF
      CALL AGI_BEGIN

*    Activate PGPLOT.
      CALL AGP_ACTIV( STATUS )

*  Clear the current picture - PGQCOL bug requires this.
      IF ( OVRLAY .AND. CLEAR .OR. .NOT. OVRLAY ) THEN

*  Use false activation of PGPLOT window to clear whole of area.
         CALL AGP_NVIEW( .FALSE., STATUS )
         CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )
         CALL PGVCLR( WX1, WX2, WY1, WY2, STATUS )
      END IF
      CALL AGP_NVIEW( .TRUE., STATUS )

*    Now decide the thickness of the pens used to draw the plot.
      CALL PAR_GET0I( 'THICK', THICK, STATUS )
      THICK = MIN( 21, MAX( THICK, 1 ) )
      CALL PGSLW( THICK )

*    Inquire the number of colours available on this device.
      CALL PGQCOL( CI1, CI2 )

*  If colour available ask user which pen to use.
      IF ( CI2 .GT. 2  ) THEN

*    Ask user which pen number to use.
         CALL PAR_GET0I( 'PALNUM', IPEN, STATUS )
         IPEN = MIN( CI2, MAX( IPEN, 0 ) )

*  Set this up as the current pen
         CALL PGSCI( IPEN )

*  Attempt to ensure that solid lines are drawn.
         CALL PGSLS( 1 )

*  Cause GKS to update the plot immediately in case GKS thinks that
*  plotting performed previously (by AGI) is now invalid (because the
*  pens were different) and that it should update the whole image
*  (which it always seems to do at the end erasing the whole plot!).
         CALL PGUPDT
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Course of action depends on whether or not the ellipse plot is
*    to be drawn over a display of the image.  If it is then look
*    for that picture in the database and map the ellipse plot directly
*    on to it; there can be no axes. The alternative has no such
*    constraints, so the plot, with or without axes, is fitted into the
*    current picture giving square pixels.
      IF ( OVRLAY ) THEN

*       Find the last DATA picture.
*       ===========================
*       Initialise the temporary and image picture identifications.
         PICIDT = -1
         PICIDI = -1

*       It could be the input picture.
         CALL AGI_INAME( PNAME, STATUS )
         CALL CHR_UCASE( PNAME )
         IF ( PNAME( 1:4 ) .NE. 'DATA' ) THEN

*          Try to get the last related DATA picture from the database.
            CALL AGI_IPOBS( PICID, LOBS, STATUS ) ! Call IPOBS to reset
                                                  ! number of pictures
                                                  ! to force update
                                                  ! (bug)
            CALL AGI_RCL( 'DATA', PICIDT, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN

*             There is no DATA picture in the current frame so the
*             application will have to exit
               CALL ERR_REP( 'PISAPLOT_NDATAP',
     :           'PISAPLOT: The current database picture is not '/
     :           /'of name DATA, or does not contain a DATA '/
     :           /'picture within itself', STATUS )
               CALL AGP_DEACT( STATUS )
               CALL AGI_CANCL( 'DEVICE', STATUS )

*             Current picture is still the input picture, and so
*             can exit immediately
               DEVCAN = .TRUE.
               GOTO 960
            ELSE
               PICIDI = PICIDT
            END IF
         ELSE

*          Use the current picture.
            PICIDI = PICID
         END IF

*       Get a zone id for the data picture (on the graphics device)
*       that will be overlaid.
         CALL AGI_SELP( PICIDI, STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )

*       Find the lower and upper pixel bounds.
         CALL AGI_IWOCO( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ),
     :                   STATUS )

*    No overlay so the current picture is used.
      ELSE

*       Read the catalogue to estimate the lower and upper bounds.
*       ==========================================================
*
*       For plots that are not to be overlaid there is no pixel
*       co-ordinate information, so this has to be estimated from
*       the catalogue.

*       Initialise the bounds.
         LBND( 1 ) = VAL__MAXR
         LBND( 2 ) = VAL__MAXR
         UBND( 1 ) = VAL__MINR
         UBND( 2 ) = VAL__MINR

*       Set the largest index value variable
         MAXIND = 0

*       Start new error context.
         CALL ERR_MARK

         N = 0
         ANOTHR = .TRUE.
         DO WHILE ( ANOTHR .AND.  STATUS .EQ. SAI__OK )

*       Extract the data from the PISAFIND parameter file
            CALL RDPIFD( FD, BUFFER, IDNO, XP, YP, INTENS, NPIX,
     :                   PEAK, ELL, PA, SXX, SYY, SXY, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*             Another line read succesfully.
               N = N + 1

*             Find the mean radius of the image.
               RAD = SQRT( REAL( NPIX ) / PI )

*             Compute bounds, allowing 2* the mean radius for the
*             image ellipse to fit in the data region.  This may
*             clip some (ellipticity >0.8 and the major axis oriented
*             near a cardinal), but gives sensible results without
*             computing it exactly.  Speed is usually more important
*             than accuracy in graphics.
               LBND( 1 ) = MIN( LBND( 1 ), XP - 2.0 * RAD )
               LBND( 2 ) = MIN( LBND( 2 ), YP - 2.0 * RAD )
               UBND( 1 ) = MAX( UBND( 1 ), XP + 2.0 * RAD )
               UBND( 2 ) = MAX( UBND( 2 ), YP + 2.0 * RAD )

*             record maximum index value
               MAXIND = ABS( MAX( MAXIND, IDNO ) )
            ELSE

*             Report error context unless the end-of-file has been
*             reached.
               IF ( STATUS .NE. FIO__EOF ) THEN
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL ERR_REP( 'PISAPLOT_RDDATA',
     :              'PISAPLOT: Error reading data record '/
     :              /'from  file ^FNAME', STATUS )
               ELSE

*             No more positions to be read therefore rewind the file so
*             that data can be read again for plotting.
                  CALL ERR_ANNUL( STATUS )
                  CALL FIO_RWIND( FD, STATUS )
               END IF
               ANOTHR = .FALSE.

*           End of no-error-reading-record-from-file check.

            END IF
         END DO

*       Release the error context.
         CALL ERR_RLSE

*       Abort if there was an error.
         IF ( STATUS .NE. SAI__OK ) GOTO 960

*       If there are going to be annotated ellipses some room is needed
*       to the top-right when computing the size of the data area.
*       MAXIND is the maximum index number in file. The 2.0 allows for
*       the aspect ratio of the characters.
         IF ( ANNOTA ) THEN
            MAXIND = MAX( MAXIND, 1 )
            MCA = INT( LOG10( REAL( MAXIND ) ) ) + 2
            MAWID = ( THT + 0.003 ) * ( UBND( 2 ) - UBND( 1 ) + 1. )

*        Use MCA in the x-axis extension
            UBND( 1 ) = UBND( 1 ) + REAL( MCA ) * MAWID

*        Just enough+  for character height in y-axis extension
            MAWID = 2.0 * MAWID
            UBND( 2 ) = UBND( 2 ) + MAWID
         END IF

*        See if user wants to modify the axes coordinates
         NEWAX = .FALSE.
         CALL PAR_GET0L( 'DEFAXES', NEWAX, STATUS )
         IF ( NEWAX ) THEN

*        Offer these values to the user for modification. May well
*        require to compare the same frame with different classes of
*        objects.
345         CONTINUE
            RVAL( 1 ) = LBND( 1 )
            RVAL( 2 ) = UBND( 1 )
            CALL PAR_DEF1R( 'XPIXS', 2, RVAL, STATUS )
            CALL PAR_GET1R( 'XPIXS', 2, RVAL, NVAL, STATUS )
            LBND( 1 ) = MIN( RVAL( 1 ), RVAL( 2 ))
            UBND( 1 ) = MAX( RVAL( 2 ), RVAL( 1 ))
            RVAL( 1 ) = LBND( 2 )
            RVAL( 2 ) = UBND( 2 )
            CALL PAR_DEF1R( 'YPIXS', 2, RVAL, STATUS )
            CALL PAR_GET1R( 'YPIXS', 2, RVAL, NVAL, STATUS )
            LBND( 2 ) = MIN( RVAL( 1 ), RVAL( 2 ))
            UBND( 2 ) = MAX( RVAL( 2 ), RVAL( 1 ))

*        Check for zero extent axes
            IF( STATUS .EQ. SAI__OK ) THEN
               IF ( LBND( 1 ) .EQ. UBND( 1 ) .OR.
     :              LBND( 2 ) .EQ. UBND( 2 ) ) THEN
                  CALL MSG_OUT( 'ZERO_AXES',
     :                          'Cannot have zero extent axes', STATUS )
                  CALL PAR_CANCL( 'XPIXS', STATUS )
                  CALL PAR_CANCL( 'YPIXS', STATUS )
                  GO TO 345
               END IF
            END IF
         END IF
*
*       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*        Create a zone of the specified size.
         CALL PGWINDOW( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ) )

*        Record the frame picture in the database.
         CALL AGP_SVIEW( 'FRAME', 'PISAPLOT', PICIDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PISAPLOT_DBSF',
     :        'PISAPLOT: Error while storing the frame in the '/
     :        /'graphics database.', STATUS )
           GOTO 960
         END IF

*        Reset input picture as current in case of an accident.
         CALL AGI_SELP( PICID, STATUS )

*        Define aspect ratio of the plot to give square pixels and
*        define a new zone of the shape.
         CALL PGWNAD( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ) )

*        Need to define a frame border for annotated axes.
         IF ( AXES ) THEN

*           If MAJTIC set then changed to intervals.
            IF ( MAJTIC( 1 ) .GT. 0 ) THEN
               MAJTIC( 1 ) = ( UBND( 1 ) - LBND( 1 ) ) /
     :                       ( MAJTIC( 1 ) + 1.0 )
            END IF
            IF ( MAJTIC( 2 ) .GT. 0 ) THEN
               MAJTIC( 2 ) = ( UBND( 2 ) - LBND( 2 ) )/
     :                       ( MAJTIC( 2 ) + 1.0 )
            END IF

*       Draw axes.
            CALL PGBOX( FORSTR, MAJTIC( 1 ), MINTIC( 1 ),
     :                  FORSTR, MAJTIC( 2 ), MINTIC( 2 ) )
            CALL PGLABEL( ABSLAB, ORDLAB, PLTITL )
         END IF

*    End of the check whether or not the plot is an overlay or not.
      END IF

*    Draw the ellipses.
*    ==================

*    Set the character height for the ANNotations as a fraction of the
*    pixel height of the data region
      IF ( ANNOTA ) THEN
         HT = THT * ( UBND( 2 ) - LBND( 2 ) + 1.0 )
         CALL PAR_GET0R( 'ANNOSCALE', TXTSCL, STATUS )
         TXTSCL = MIN( 1.0E5, MAX( TXTSCL, 0.0001 ) )
         CALL PGSCH( HT * TXTSCL )
         WDIM = UBND( 1 ) - LBND( 1 ) + 1.0
      END IF

*    Start new error context.
      CALL ERR_MARK

      N = 0
      ANOTHR = .TRUE.
      DO WHILE ( ANOTHR .AND. STATUS .EQ. SAI__OK )

*       Read a record of the catalogue of parameterised data.
*       Extract the identification number, the number of pixels
*       within the image (i.e. size), the centroid position, the
*       ellipticity and position angle from the file.
         CALL RDPIFD( FD, BUFFER, IDNO, XP, YP, INTENS, NPIX,
     :                PEAK, ELL, PA, SXX, SYY, SXY, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*          Another line read succesfully.
            N = N + 1

*          Now finally this is the part that plots the ellipses.
*          =====================================================

*          Find the semi-major and semi-minor axes (this is based
*          on the geometric formula for the area of an ellipse).
            EA = SQRT( REAL( NPIX ) /
     :            MAX( ( PI * ( 1.0 - ELL ) ) , ALMOST ) )
            EA = MIN( EA, 0.5 * REAL( NPIX ) )
            EB = EA * ( 1.0 - ELL )

*          Convert the position angle from degrees to radians.
            RPA = PA * DGTRD

*          Compute the locus of the ellipse.
            CALL ELLPSE( EA, EB, RPA, XP, YP, NELPS, XE, YE, STATUS )

*          Plot the ellipse.
            CALL PGLINE( NELPS, XE, YE )

*          Annotate the ellipse with the identification number if
*          requested. Compute an offset for the annotation depending
*          on the shape of the ellipse.  Get the orientation of
*          the annotation with respect to the ellipse in polar
*          co-ordinates, and get the position of the ellipse locus
*          at the direction.  Add a fraction of the picture width so
*          the annotation does not abut the ellipse. (Actually, the
*          OA should be RPA - 0.25 * PI, but the below gives better
*          results!)
            IF ( ANNOTA ) THEN
               OA = RPA + 0.25 * PI
               XA = XP + ABS( EA * COS( RPA ) * COS( OA ) -
     :              EB * SIN( RPA ) * SIN( OA ) ) + 0.003 * WDIM
               YA = YP + ABS( EA * SIN( RPA ) * COS( OA ) +
     :              EB * COS( RPA ) * SIN( OA ) ) + 0.003 * WDIM
               CALL CHR_ITOC( IDNO, NUM, N )

*  Clip so that XA and YA lie within frame.
               IF ( XA .GE. LBND( 1 ) .AND. XA .LE. UBND( 1 ) .AND.
     :              YA .GE. LBND( 2 ) .AND. YA .LE. UBND( 2 ) ) THEN
                  CALL PGTEXT( XA, YA, NUM( :N ) )
               END IF
            END IF

*             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ELSE

*          Report error reading from file, unless it is the end of file.
            IF ( STATUS .NE. FIO__EOF ) THEN
               CALL MSG_SETC( 'FNAME', FNAME )
               CALL ERR_REP( 'PISAPLOT_RDDATA',
     :           'PISAPLOT: Error reading data record '/
     :           /'from file ^FNAME', STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF
            ANOTHR = .FALSE.

*          No more positions to be read therefore close the file. so
*          The flag prevents the closedown operation from attempting
*          to close the file again.
            CALL FIO_CLOSE( FD, STATUS )
            CATPOS = .FALSE.

*        End of no-error-reading-record-from-file check.
         END IF
      END DO

*  Flush buffers.
      CALL PGUPDT

*    Release the error context.
      CALL ERR_RLSE

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*    Record the data picture in the database.
      CALL AGP_SVIEW( 'DATA', 'PISPLOT', PICIDI, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAPLOT_DBSI',
     :     'PISAPLOT: Error while storing the data picture in the '/
     :     /'graphics database.', STATUS )
      END IF


*    Tidy the graphics database operations.
*    ======================================
 960  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         TSTAT = STATUS
         STATUS = SAI__OK

*       Reset the current picture.
         CALL AGI_SELP( PICID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) STATUS = TSTAT
      ELSE
         CALL AGI_SELP( PICID, STATUS )
      END IF

*    Close down PGPLOT and AGI.
      CALL AGP_DEACT( STATUS )
      CALL AGI_END( -1 , STATUS )

      IF ( DEVCAN ) THEN
         CALL AGI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL AGI_ANNUL( PICID, STATUS )
      END IF

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*    Close the parameterised-data file if it is currently open.
 980  CONTINUE
      IF ( CATPOS ) CALL FIO_CLOSE( FD, STATUS )

 999  CONTINUE

      END
* $Id$
