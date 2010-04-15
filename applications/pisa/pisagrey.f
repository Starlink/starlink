      SUBROUTINE PISAGREY( STATUS )
*+
*  Name:
*     PISAGREY

*  Purpose:
*     Plots an NDF as a greyscale.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAGREY( STATUS )

*  Description:
*     This routine displays a greyscale representation of the data
*     component of an NDF.

*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the plot abscissa, may include PGPLOT escape
*        sequences.  This parameter is only used when the axes option
*        is selected. [X]
*     AXES = _LOGICAL (Read)
*        True if annotated axes are to be drawn around the displayed
*        image. This parameter is ignored in the overlay mode, since
*        there is no guarantee that the axes would lie entirely
*        within the current picture. [TRUE]
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
*     PLTITL = CHAR (Read)
*        The title of the plot, may include PGPLOT escape sequences.
*        Up to about 40 characters can be accommodated.  This parameter
*        is only used when axes option is selected. [PISAGREY]
*     XPIXS = _REAL (Read)
*        Initial and final pixel coordinates of plot in x-direction.
*     YPIXS = _REAL (Read)
*        Initial and final pixel coordinates of plot in y-direction.

*  Notes:
*     -  UNIX specific. Only supplied on UNIX machines


*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-APR-1992 (PDRAPER):
*        Original version.
*     07-SEP-2004 (PDRAPER):
*        Changed to use CNF_PVAL.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error definitions
      INCLUDE 'PRM_PAR'
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL                    ! True if :
     :  AXES,                    ! Annotated axes are to be drawn
     :  OUTTIC                   ! Axis tick marks are to be placed
                                 ! outside the box instead of inside

      REAL
     :  LBND( 2 ),               ! Lower bounds of the image
     :  UBND( 2 ),               ! Upper bounds of the image
     :  MAJTIC( 2 ),             ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively
     :  RVAL( 2 )                ! buffer to hold plot limits

      CHARACTER*72
     :  ABSLAB,                  ! Label for the abscissa of the plot
     :  ORDLAB,                  ! Label for the ordinate of the plot
     :  PLTITL,                  ! Title of the plot
     :  FORSTR*12                ! String specifying axes control


      INTEGER
     :  PICID,                   ! Graphics' database identifier on input
     :  PICIDI,                  ! DATA image picture identifier
     :  TSTAT                    ! Temporary status

      INTEGER
     :  NVAL,                    ! the number of values returned by get1r
     :  MINTIC( 2 )              ! Numbers of minor tick marks along x and
                                 ! y axes respectively
      INTEGER ILBND( 2 )
      INTEGER IUBND( 2 )
      INTEGER IPIN
      INTEGER IDIN
      INTEGER EL
      INTEGER NX
      INTEGER NY
      INTEGER XORIG
      INTEGER YORIG
      INTEGER IPWORK
      REAL TR( 6 )
      INTEGER NDIM
      REAL DRANGE( 2 )
      REAL WX1, WX2, WY1, WY2
      REAL RMIN, RMAX

*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NDF.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', IDIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Map in the data.
      CALL NDF_MAP( IDIN, 'Data', '_REAL', 'READ', IPIN, EL, STATUS )

*  Get array range.
      CALL NDF_BOUND( IDIN, 2, ILBND, IUBND, NDIM, STATUS )
      NX = IUBND( 1 ) - ILBND( 1 ) + 1
      NY = IUBND( 2 ) - ILBND( 2 ) + 1
      XORIG = ILBND( 1 )
      YORIG = ILBND( 2 )
      IF ( STATUS .NE. SAI__OK ) GO TO 960

*  Whether or not annotated axes are required.
      AXES = .FALSE.
      CALL PAR_GET0L( 'AXES', AXES, STATUS )
      IF ( AXES ) THEN

*  Start a new error context.
         CALL ERR_MARK

*  Obtain the title for the plot.
         CALL PAR_DEF0C( 'PLTITL', 'PISAGREY', STATUS )
         CALL PAR_GET0C( 'PLTITL', PLTITL, STATUS )
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
            PLTITL = ' '
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 960
         END IF

*  Obtain axis labels.  A null value causes the default to be
*  chosen, namely, 'X' for the abscissa...
         CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
            ABSLAB = 'X'
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 960
         END IF

*  ...and 'Y' for the ordinate.
         CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )
            ORDLAB = 'Y'
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 960
         END IF

*  Get the number of minor ticks.
         CALL PAR_GET1I( 'MINTIC', 2, MINTIC, NVAL, STATUS )
         IF ( MINTIC( 1 ) .LT. 0 ) MINTIC( 1 ) = 0
         IF ( MINTIC( 2 ) .LT. 0 ) MINTIC( 2 ) = 0

*  Get the parameter controlling the number of major ticks per axis.
         CALL PAR_GET1R( 'MAJTIC', 2, MAJTIC, NVAL, STATUS )
         IF ( MAJTIC( 1 ) .LT. 0.0 ) MAJTIC( 1 ) = 0.0
         IF ( MAJTIC( 2 ) .LT. 0.0 ) MAJTIC( 2 ) = 0.0

*  Are the tick marks on the outside of the axes?  Default is outside
*  so that they do not hide any of the image.
        OUTTIC = .TRUE.
        CALL PAR_GET0L( 'OUTTIC', OUTTIC, STATUS )
        IF ( OUTTIC ) THEN
           FORSTR = 'TBCINS'
        ELSE
           FORSTR = 'TBCNS'
        END IF

*  Release the error context.
         CALL ERR_RLSE
         IF ( STATUS .EQ. PAR__ABORT ) GOTO 960
      END IF

*  Get device.
      CALL AGI_ASSOC( 'DEVICE', 'WRITE', PICID, STATUS )
      CALL AGI_BEGIN

*  Activate PGPLOT and get the current zone.
      CALL AGI_SELP( PICID, STATUS )
      CALL AGP_ACTIV( STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Clear zone of any previous information.
      CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )
      CALL PGVCLR( WX1, WX2, WY1, WY2, STATUS )

*  Activate new zone with border (if require)
      IF ( AXES ) CALL AGP_NVIEW( .TRUE., STATUS )

*  Inform user of possible range of image.
      CALL MSG_SETI( 'X1', ILBND( 1 ) )
      CALL MSG_SETI( 'X2', IUBND( 1 ) )
      CALL MSG_SETI( 'Y1', ILBND( 2 ) )
      CALL MSG_SETI( 'Y2', IUBND( 2 ) )
      CALL MSG_OUT( ' ', '  Input array bounds (^X1:^X2,^Y1:^Y2)',
     : STATUS )

*  Get range of coordinates user wants to plot.
 345  CONTINUE
      LBND( 1 ) = REAL( ILBND( 1 ) )
      LBND( 2 ) = REAL( ILBND( 2 ) )
      UBND( 1 ) = REAL( IUBND( 1 ) )
      UBND( 2 ) = REAL( IUBND( 2 ) )
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

*  Check for zero extent axes
      IF( STATUS .EQ. SAI__OK ) THEN
         IF ( LBND( 1 ) .EQ. UBND( 1 ) .OR.
     :        LBND( 2 ) .EQ. UBND( 2 ) ) THEN
            CALL MSG_OUT( 'ZERO_AXES', 'Cannot have zero extent axes',
     :                    STATUS )
            CALL PAR_CANCL( 'XPIXS', STATUS )
            CALL PAR_CANCL( 'YPIXS', STATUS )
            GO TO 345
         END IF
      END IF

*  Set range of image to display.
      ILBND( 1 ) = MAX( ILBND( 1 ) , INT( LBND( 1 ) ) )
      ILBND( 2 ) = MAX( ILBND( 2 ) , INT( LBND( 2 ) ) )
      IUBND( 1 ) = MIN( IUBND( 1 ) , INT( UBND( 1 ) ) )
      IUBND( 2 ) = MIN( IUBND( 2 ) , INT( UBND( 2 ) ) )

*  Get data range
      CALL PSA1_MNMAX( %VAL( CNF_PVAL( IPIN ) ), EL , RMIN, RMAX,
     :                 STATUS )
      CALL MSG_SETR( 'MIN', RMIN )
      CALL MSG_SETR( 'MAX', RMAX )
      CALL MSG_OUT( ' ',
     : '  Data range ^MIN : ^MAX ', STATUS )
      DRANGE( 1 ) = RMIN
      DRANGE( 2 ) = RMAX
      CALL PAR_DEF1R( 'DRANGE', 2, DRANGE, STATUS )
      CALL PAR_GET1R( 'DRANGE', 2, DRANGE, NVAL, STATUS )

*  Create a zone of the specified size.
      IF ( STATUS .NE. SAI__OK ) GO TO 960
      CALL PGWINDOW( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ) )

*  Define aspect ratio of the plot to give square pixels.
      CALL PGWNAD( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ) )

*  Need to define a frame border for annotated axes.
      IF ( AXES ) THEN

*  If MAJTIC set then changed to intervals.
         IF ( MAJTIC( 1 ) .GT. 0 ) THEN
              MAJTIC( 1 ) = ( UBND( 1 ) - LBND( 1 ) ) /
     :                      ( MAJTIC( 1 ) + 1 )
         END IF
         IF ( MAJTIC( 2 ) .GT. 0 ) THEN
               MAJTIC( 2 ) = ( UBND( 2 ) - LBND( 2 ) )/
     :                       ( MAJTIC( 2 ) + 1 )
         END IF

*  Draw axes.
         CALL PGBOX( FORSTR, MAJTIC( 1 ), MINTIC( 1 ),
     :                  FORSTR, MAJTIC( 2 ), MINTIC( 2 ) )
         CALL PGLABEL( ABSLAB, ORDLAB, PLTITL )
      END IF

*  Trap invalid pixels etc.
      CALL PSX_MALLOC( EL * VAL__NBR, IPWORK, STATUS )
      CALL PSA1_RMINV( %VAL( CNF_PVAL( IPIN ) ), EL,
     :                 %VAL( CNF_PVAL( IPWORK ) ), RMIN, STATUS )

*  Draw greyscale.
      TR( 3 ) = 0.0
      TR( 5 ) = 0.0
      TR( 2 ) = 1.0
      TR( 6 ) = 1.0
      TR( 1 ) = XORIG - 1.5
      TR( 4 ) = YORIG - 1.5
      ILBND( 1 ) = ILBND( 1 ) - XORIG + 1
      IUBND( 1 ) = IUBND( 1 ) - XORIG + 1
      ILBND( 2 ) = ILBND( 2 ) - YORIG + 1
      IUBND( 2 ) = IUBND( 2 ) - YORIG + 1
      IF ( STATUS .NE. SAI__OK ) GO TO 960
      CALL PGGRAY( %VAL( CNF_PVAL( IPWORK ) ), NX, NY,
     :             ILBND( 1 ), IUBND( 1 ),
     :             ILBND( 2 ), IUBND( 2 ), DRANGE( 1 ), DRANGE( 2 ),
     :             TR )

*  Flush buffers.
      CALL PGUPDT

*  Record the data picture in the database.
      CALL AGP_SVIEW( 'DATA', 'PISAGREY', PICIDI, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAGREY_DBSI',
     :     'PISAPLOT: Error while storing the data picture in the '/
     :     /'graphics database.', STATUS )
      END IF


*  Tidy the graphics database operations.
 960  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         TSTAT = STATUS
         STATUS = SAI__OK

*  Reset the current picture.
         CALL AGI_SELP( PICID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) STATUS = TSTAT
      ELSE
         CALL AGI_SELP( PICID, STATUS )
      END IF

*  Close any NDFs.
      CALL NDF_END( STATUS )

*  Close down PGPLOT and AGI.
      CALL AGP_DEACT( STATUS )
      CALL AGI_END( -1 , STATUS )
      CALL AGI_ANNUL( PICID, STATUS )

 999  CONTINUE

      END
* $Id$
