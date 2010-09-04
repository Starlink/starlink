      SUBROUTINE SPD_CZAD( REASON, PICID, OVER, NDF, TEXT, LABGVN,
     :                     BOTTOM, LEFT, TOP, RIGHT, FILL, WLDGVN,
     :                     WORLD, LABSPC, ROMAN, CHIGHT, COLOUR, THICK,
     :                     AXES, TICK, NUML, MAJOR, MINOR, DASH,
     :                     START, STEP, NMAJOR, STATUS )
*+
*  Name:
*     SPD_CZAD

*  Purpose:
*     Parameter-free SPECCONT call back.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZAD( REASON, PICID, OVER, NDF,
*        TEXT, LABGVN, BOTTOM, LEFT, TOP, RIGHT,
*        FILL, WLDGVN, WORLD, LABSPC, ROMAN, CHIGHT, COLOUR, THICK,
*        AXES, TICK, NUML, MAJOR, MINOR, DASH,
*        START, STEP, NMAJOR, STATUS )

*  Description:
*     This is the call back for the SPECCONT application common to the
*     Motif- and ADAM-based interfaces. All parameter handling takes
*     place outwith this routine, all data access and work takes place
*     under the control of this routine. Since NDF_BEGIN/END must be
*     outside NDF_ASSOC, they too are handled outwith this routine.

*  Arguments:
*     REASON = INTEGER (Given)
*        The call back reason:
*         0 Clean up,
*         1 start up,
*         2 do the work.
*     PICID = INTEGER (Given)
*        The AGI picture identifier. Depending on OVER this is a DATA
*        picture to be used as view port or a picture inside with a new
*        view port and DATA picture is to be created.
*     OVER = INTEGER (Given)
*        Non-zero if the plot is to be an overlay on a previous plot. In
*        this case the AGI data base is checked for a recent DATA
*        picture to define the view port.
*     NDF = INTEGER (Given)
*        The image NDF identifier.
*     TEXT( 4 ) = INTEGER (Given)
*        For each axis (bottom, left, top, right), telling whether a
*        text label is to be drawn. Used only if OVER is zero (false).
*     LABGVN( 4 ) = INTEGER (Given)
*        Each is non-zero if a plot label string is supplied by the
*        calling routine for the bottom, left, top, right of the plot.
*        This is used only if OVER is zero (false).
*     BOTTOM = CHARACTER * ( * ) (Given)
*        The given plot label string for the bottom edge. Used only if
*        LABGVN(1) is non-zero (true) and OVER is zero.
*     LEFT = CHARACTER * ( * ) (Given)
*        The given plot label string for the left edge. Used only if
*        LABGVN(2) is non-zero (true) and OVER is zero.
*     TOP = CHARACTER * ( * ) (Given)
*        The given plot label string for the top edge. Used only if
*        LABGVN(3) is non-zero (true) and OVER is zero.
*     RIGHT = CHARACTER * ( * ) (Given)
*        The given plot label string for the right edge. Used only if
*        LABGVN(4) is non-zero (true) and OVER is zero.
*     FILL = INTEGER (Given)
*        Non-zero if the PGPLOT window is to be adjusted so as to give
*        equal plot scales horizontally and vertically. Used only if
*        OVER is zero (false).
*     WLDGVN = INTEGER (Given)
*        Non-zero if the PGPLOT window is specified by the calling
*        routine. Used only if OVER is zero (false).
*     WORLD( 4 ) = REAL (Given)
*        If WLDGVN is non-zero (true) then this is the PGPLOT window to
*        be used. Used only if OVER is zero (false).
*     LABSPC( 4 ) = REAL (Given)
*        The fraction of the view surface to be reserved for labels. The
*        elements apply to the bottom, left, top, right in that order.
*        Each element must be between 0. and 0.5, or this routine will
*        abort with an error message.
*        Used only if OVER is zero (false).
*     ROMAN = INTEGER (Given)
*        Non-zero if the double-stroke roman font is to be used.
*     CHIGHT = REAL (Given)
*        Character height in PGPLOT units. This height applies to the
*        box labels. Contour labels are plotted at 2.8 mm size.
*     COLOUR = INTEGER (Given)
*        PGPLOT pen number to be used.
*     THICK = INTEGER (Given)
*        PGPLOT line thickness parameter.
*     AXES( 4 ) = INTEGER (Given)
*        0 or 1, telling whether the axis is to be drawn. 1 for yes.
*        Used only if OVER is zero (false).
*     TICK( 4 ) = INTEGER (Given)
*        -1, 0, or +1, telling whether ticks are to be drawn at all and
*        whether they are inside or outside the box. +1 for outside.
*        Used only if OVER is zero (false).
*     NUML( 4 ) = INTEGER (Given)
*        0 or 1, telling whether numeric labels are to be drawn. 1 for
*        yes. Used only if OVER is zero (false).
*     MAJOR( 2 ) = REAL (Given)
*        Distance between major tick marks for horizontal and vertical
*        directions. Used only if OVER is zero (false).
*     MINOR( 2 ) = INTEGER (Given)
*        Number of minor tick intervals per major tick interval for
*        horizontal and vertical directions. Used only if OVER is zero
*        (false).
*     DASH = INTEGER (Given)
*        PGPLOT line style (dash pattern) parameter.
*     START = REAL (Given)
*        The value of the first contour. See documentation on
*        NOD_CONTRS.
*     STEP = REAL (Given)
*        The step between successive contours. See documentation on
*        NOD_CONTRS.
*     NMAJOR = INTEGER (Given)
*        The number of contours.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine changes the PGPLOT attributes. It is expected that
*     PGEND will be called after the call to this routine, and between
*     invokations of this routine.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20 Jun 1994 (hme):
*        Original version.
*     24 Jun 1994 (hme):
*        Use NOD_CONTRS.
*     20 Apr 1995 (hme):
*        Pass to NOD_CONTRS a badval of 1E-19. Map must be processed and
*        a work space is needed for that.
*     21 Apr 1995 (hme):
*        Offer several contour styles and choice of NMINOR. Adapt to
*        changed version of NOD_CONTRS.
*     20 Nov 1995 (hme):
*        Remove the STYLE and NMINOR arguments. This routine uses plain
*        PGPLOT.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER REASON
      INTEGER PICID
      INTEGER OVER
      INTEGER NDF
      INTEGER TEXT(   4 )
      INTEGER LABGVN( 4 )
      CHARACTER * ( * ) BOTTOM, LEFT, TOP, RIGHT
      INTEGER FILL
      INTEGER WLDGVN
      REAL    WORLD(  4 )
      REAL    LABSPC( 4 )
      INTEGER ROMAN
      REAL    CHIGHT
      INTEGER COLOUR, THICK
      INTEGER AXES( 4 ), TICK( 4 ), NUML( 4 )
      REAL    MAJOR( 2 )
      INTEGER MINOR( 2 )
      INTEGER DASH
      REAL    START
      REAL    STEP
      INTEGER NMAJOR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL EPS                   ! Tolerance for coordinate linearity
      PARAMETER ( EPS = 1E-5 )
      REAL BADVAL                ! Bad value for NOD contouring routines
      PARAMETER ( BADVAL = +1E19 )

*  Local Volatile Variables:
      LOGICAL XTHERE, STHERE     ! Existence of HDS components
      LOGICAL LINEAR             ! True for linear arrays
      INTEGER I                  ! Temporary integer
      INTEGER WNDF( 3 )          ! Workspace NDF identifier
      INTEGER WPTR( 3 )          ! Workspace array pointers
      INTEGER NDIM               ! NDF dimensionality
      INTEGER NELM               ! NDF size
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER ACTDIM             ! Actual dimensionality
      INTEGER AXIS( NDF__MXDIM ) ! Which two axes are non-degenerate
      INTEGER DATA               ! Data array pointer
      INTEGER ADAT( 2 )          ! Axis centre pointers
      INTEGER IPATH              ! Pointer to contour value array
      REAL DELTA                 ! Floating point range
      REAL RTEMP(  4 )           ! Temporary floating point numbers
      REAL DATRNG( 4 )           ! Extreme pixel centre coordinates
      REAL LWORLD( 4 )           ! PGPLOT window according to data
      REAL TR( 6 )               ! PGCONB transform matrix
      REAL XWIDTH, YWIDTH        ! View surface in millimetre
      REAL ASPECT                ! Width divided by height
      CHARACTER * ( 6 ) BOXOPT   ! Option string for PGBOX
      CHARACTER * ( 64 ) LABEL   ! Axis or data label string
      CHARACTER * ( 64 ) UNITS   ! Axis or data unit string
      CHARACTER * ( 64 ) LPLAB( 4 ) ! Actual plot labels
      CHARACTER * ( DAT__SZLOC ) XLOC ! Extension locator

*  Internal References:
      INTEGER CHR_LEN            ! Used length of a string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Clean up.
*  =========

      IF ( REASON .EQ. 0 ) THEN
         CONTINUE


*  Start up.
*  =========

      ELSE IF ( REASON .EQ. 1 ) THEN
         CONTINUE


*  Work.
*  =====

      ELSE IF ( REASON .EQ. 2 ) THEN


*     Check and access data.
*     ======================

*     Check that input is actually 2-D.
         CALL NDF_DIM( NDF, NDF__MXDIM, DIM, NDIM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         ACTDIM = 0
         DO 1 I = 1, NDIM
            IF ( DIM(I) .GT. 1 ) THEN
               ACTDIM = ACTDIM + 1
               AXIS(ACTDIM) = I
            END IF
 1       CONTINUE
         IF ( ACTDIM .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZAD_E01',
     :         'SPECCONT: Error: Input NDF is not two-dimensional.',
     :         STATUS )
            GO TO 500
         END IF

*     Check also that none of the displayed axes has SPECVALS assocated
*     with it. (That is, if SPECVALS exist, make sure SPECAXIS is not
*     one of the displayed.)
         CALL SPD_EAAA( NDF, 'READ', XTHERE, XLOC, STATUS )
         IF ( XTHERE ) THEN
            CALL DAT_THERE( XLOC, XCMP6, STHERE, STATUS )
            IF ( STHERE ) THEN
               CALL SPD_EABA( NDF, XTHERE, I, STATUS )
               IF ( DIM(I) .GT. 1 ) THEN
                  CALL DAT_ANNUL( XLOC, STATUS )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZAD_E02', 'SPECCONT: Error: ' //
     :               'One of the axes to be displayed is the ' //
     :               'spectroscopic axis. The existence of ' //
     :               'spectroscopic values in the input Specdre ' //
     :               'Extension prevents such a display.', STATUS )
                  GO TO 500
               END IF
            END IF
            CALL DAT_ANNUL( XLOC, STATUS )
         END IF

*     Map data.
         CALL NDF_MAP( NDF, 'DATA', '_REAL', 'READ',
     :      DATA, NELM, STATUS )


*     Access axis data.
*     =================

*     Map the two axis centre arrays.
         CALL NDF_AMAP( NDF, 'CENTRE', AXIS(1), '_REAL', 'READ',
     :      ADAT(1), I, STATUS )
         CALL NDF_AMAP( NDF, 'CENTRE', AXIS(2), '_REAL', 'READ',
     :      ADAT(2), I, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Check that the coordinates are linear.
*     We get the first and last values as a side effect.
         CALL SPD_UAAHR( DIM(AXIS(1)), %VAL( CNF_PVAL( ADAT(1) ) ),
     :                   EPS, DATRNG(1), DATRNG(2), LINEAR, STATUS )
         IF ( .NOT. LINEAR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZAD_E03', 'SPECCONT: Error: First ' //
     :         'axis is not linear.', STATUS )
            GO TO 500
         END IF
         CALL SPD_UAAHR( DIM(AXIS(2)), %VAL( CNF_PVAL( ADAT(2) ) ),
     :                   EPS, DATRNG(3), DATRNG(4), LINEAR, STATUS )
         IF ( .NOT. LINEAR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZAD_E04', 'SPECCONT: Error: Second ' //
     :         'axis is not linear.', STATUS )
            GO TO 500
         END IF

*     At the moment we have the data co-ordinate range in DATRNG, which
*     we use to remember how to convert array index to co-ordinate.
*     We work out the transform matrix for the plotting routine here.
         TR(2) = ( DATRNG(2) - DATRNG(1) ) / FLOAT( DIM(AXIS(1)) - 1 )
         TR(6) = ( DATRNG(4) - DATRNG(3) ) / FLOAT( DIM(AXIS(2)) - 1 )
         TR(1) = DATRNG(1) - TR(2)
         TR(4) = DATRNG(3) - TR(6)
         TR(3) = 0.
         TR(5) = 0.

*     Work out LWORLD. These are currently the centres of the edge pixels.
*     In accordance with the common greyscale display, we want to
*     include all of the edge pixels in the viewport, not just their
*     inside half. Thus we extend the range now by 1/2 a pixel in each
*     direction.
*     There is also the possibility that WORLD has been given by the
*     calling routine.
         IF ( WLDGVN .NE. 0 ) THEN
            DO 2 I = 1, 4
               LWORLD(I) = WORLD(I)
 2          CONTINUE
         ELSE
            DELTA = ( DATRNG(2) - DATRNG(1) ) / FLOAT( DIM(AXIS(1))-1 )
            LWORLD(1) = DATRNG(1) - DELTA / 2.
            LWORLD(2) = DATRNG(2) + DELTA / 2.
            DELTA = ( DATRNG(4) - DATRNG(3) ) / FLOAT( DIM(AXIS(2))-1 )
            LWORLD(3) = DATRNG(3) - DELTA / 2.
            LWORLD(4) = DATRNG(4) + DELTA / 2.
         END IF


*     Work out plot labels.
*     =====================

*     Plot labels are needed only if this is not an overlay. The labels
*     may have been given as parameters. Otherwise they have to be
*     constructed from what is in the data:
*     bottom: LABEL1 [UNITS1]
*     left: LABEL2 [UNITS2]
*     top: TITLE
*     right: LABEL [UNITS]

         IF ( OVER .EQ. 0 ) THEN
            IF ( LABGVN(1) .EQ. 0 ) THEN
               LABEL = ' '
               UNITS = ' '
               CALL NDF_ACGET( NDF, 'LABEL', AXIS(1), LABEL, STATUS )
               CALL NDF_ACGET( NDF, 'UNITS', AXIS(1), UNITS, STATUS )
               LPLAB(1) = LABEL
               IF ( UNITS .NE. ' ' )
     :            LPLAB(1) = LABEL(:CHR_LEN(LABEL)) //
     :               ' [' // UNITS(:CHR_LEN(UNITS)) // ']'
            ELSE
               LPLAB(1) = BOTTOM
            END IF
            IF ( LABGVN(2) .EQ. 0 ) THEN
               LABEL = ' '
               UNITS = ' '
               CALL NDF_ACGET( NDF, 'LABEL', AXIS(2), LABEL, STATUS )
               CALL NDF_ACGET( NDF, 'UNITS', AXIS(2), UNITS, STATUS )
               LPLAB(2) = LABEL
               IF ( UNITS .NE. ' ' )
     :            LPLAB(2) = LABEL(:CHR_LEN(LABEL)) //
     :               ' [' // UNITS(:CHR_LEN(UNITS)) // ']'
            ELSE
               LPLAB(2) = LEFT
            END IF
            IF ( LABGVN(3) .EQ. 0 ) THEN
               LPLAB(3) = ' '
               CALL NDF_CGET( NDF, 'TITLE', LPLAB(3), STATUS )
            ELSE
               LPLAB(3) = TOP
            END IF
            IF ( LABGVN(4) .EQ. 0 ) THEN
               LABEL = ' '
               UNITS = ' '
               CALL NDF_CGET( NDF, 'LABEL', LABEL, STATUS )
               CALL NDF_CGET( NDF, 'UNITS', UNITS, STATUS )
               LPLAB(4) = LABEL
               IF ( UNITS .NE. ' ' )
     :            LPLAB(4) = LABEL(:CHR_LEN(LABEL)) //
     :               ' [' // UNITS(:CHR_LEN(UNITS)) // ']'
            ELSE
               LPLAB(4) = RIGHT
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500


*     Work out viewport and window.
*     =============================

*     If overlay.
         IF ( OVER .NE. 0 ) THEN

*        The calling routine will have given us as the current picture
*        what was the last DATA picture inside what it found to be the
*        current picture. That DATA picture will have become the SGS
*        zone that acts as the PGPLOT view surface. Thus the viewport is
*        the full view surface.
            CALL PGSVP( 0., 1., 0., 1. )

*        Now, the picture will first of all have world co-ordinates.
*        These are free, but must be linear and increasing. KAPPA
*        DISPLAY, for example uses NDF pixel indices to set a picture's
*        world co-ordinates. Then, the picture may have a
*        transformation. The forward function transforms our world
*        co-ordinates (axis centres, which may run backward and in other
*        applications might not be monotonous or linear) into the
*        picture's world co-ordinates. The inverse function transforms
*        the picture's world co-ordinates into ours. (AGI calls our world
*        co-ordinates data co-ordinates.)
*        There are two things to do here. Firstly the transform must be
*        linear in each direction and independent of the co-ordinate in
*        the other direction. Secondly we need to transform the
*        picture's world co-ordinate range into our world co-ordinates and
*        use those numbers for the PGPLOT window.

*        Get workspace to transform axis centres. The transformation is
*        from 2-D to 2-D. So we need two arrays of the same length, one
*        for x and one for corresponding y. Also we need an array of
*        constant values 1 to pair with the axis centre arrays.
            CALL NDF_TEMP( I, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, MAX(DIM(AXIS(1)),DIM(AXIS(2))),
     :                    I, WNDF(3), STATUS )
            CALL NDF_AMAP( WNDF(3), 'CENTRE', 1, '_REAL', 'WRITE',
     :                     WPTR(1), I, STATUS )
            CALL NDF_MAP(  WNDF(3), 'DATA,VARIANCE', '_REAL', 'WRITE',
     :                     WPTR(2), I, STATUS )
            CALL SPD_UAAFR( 1, I, %VAL( CNF_PVAL( WPTR(1) ) ), 1.,
     :                      STATUS )

*        Transform pixels parallel to x axis from data to world. Result
*        x should be linear, result y should be constant.
*        For data x we can use the mapped axis centres. But we need a
*        corresponding array of y values identical to 1 (not 0).
            CALL AGI_TDTOW( PICID, DIM(AXIS(1)),
     :                      %VAL( CNF_PVAL( ADAT(1) ) ),
     :                      %VAL( CNF_PVAL( WPTR(1) ) ),
     :                      %VAL( CNF_PVAL( WPTR(2) ) ),
     :                      %VAL( CNF_PVAL( WPTR(3) ) ), STATUS )

*        If transform was successful.
            IF ( STATUS .EQ. SAI__OK ) THEN

*           Check that world x is linear w.r.t. data x.
               CALL SPD_UAAHR( DIM(AXIS(1)),
     :                         %VAL( CNF_PVAL( WPTR(2) ) ),
     :                         EPS, RTEMP(1), RTEMP(2), LINEAR, STATUS )
               IF ( .NOT. LINEAR .OR. RTEMP(1) .EQ. RTEMP(2) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZAD_E03',
     :               'SPECCONT: Error: First ' //
     :               'axis is not linear.', STATUS )
               END IF

*           Check that world y is independent of data x.
               CALL SPD_UAAAR( .FALSE., DIM(AXIS(1)),
     :                         %VAL( CNF_PVAL( WPTR(3) ) ),
     :                         RTEMP(1), RTEMP(2), STATUS )
               IF ( (RTEMP(2)-RTEMP(1)) .GT.
     :               EPS*(ABS(RTEMP(2))+ABS(RTEMP(1))) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZAD_E05',
     :               'SPECCONT: Error: Axis transforms are not ' //
     :               'independent.', STATUS )
               END IF

            END IF

*        Transform pixels parallel to y axis from data to world. Result
*        y should be linear, result x should be constant.
*        For data y we can use the mapped axis centres. But we need a
*        corresponding array of x values identical to 1 (not 0).
            CALL AGI_TDTOW( PICID, DIM(AXIS(2)),
     :                      %VAL( CNF_PVAL( WPTR(1) ) ),
     :                      %VAL( CNF_PVAL( ADAT(2) ) ),
     :                      %VAL( CNF_PVAL( WPTR(2) ) ),
     :                      %VAL( CNF_PVAL( WPTR(3) ) ), STATUS )

*        If transform was successful.
            IF ( STATUS .EQ. SAI__OK ) THEN

*           Check that world y is linear w.r.t. data y.
               CALL SPD_UAAHR( DIM(AXIS(2)),
     :                         %VAL( CNF_PVAL( WPTR(3) ) ),
     :                         EPS, RTEMP(1), RTEMP(2), LINEAR, STATUS )
               IF ( .NOT. LINEAR .OR. RTEMP(1) .EQ. RTEMP(2) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZAD_E04',
     :               'SPECCONT: Error: Second ' //
     :               'axis is not linear.', STATUS )
               END IF

*           Check that world y is independent of data x.
               CALL SPD_UAAAR( .FALSE., DIM(AXIS(2)),
     :                         %VAL( CNF_PVAL( WPTR(2) ) ),
     :                         RTEMP(1), RTEMP(2), STATUS )
               IF ( (RTEMP(2)-RTEMP(1)) .GT.
     :               EPS*(ABS(RTEMP(2))+ABS(RTEMP(1))) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZAD_E05',
     :               'SPECCONT: Error: Axis transforms are not ' //
     :               'independent.', STATUS )
               END IF

            END IF

*        Release the axis work spaces.
            CALL NDF_ANNUL( WNDF(3), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Derive window from transform. For this we must get the
*        picture's world co-ordinates and transform two opposite corners
*        to data co-ordinates.
            CALL AGI_IWOCO( RTEMP(1), RTEMP(2), RTEMP(3), RTEMP(4),
     :                      STATUS )
            CALL AGI_TWTOD( PICID, 2, RTEMP(1), RTEMP(3),
     :                      LWORLD(1), LWORLD(3), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
            CALL PGSWIN( LWORLD(1), LWORLD(2), LWORLD(3), LWORLD(4) )

*     Else (not overlay).
         ELSE

*        Check given LABSPC.
            DO 3 I = 1, 4
               IF ( LABSPC(I) .LT. 0. .OR. LABSPC(I) .GT. 0.5 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZAD_E06',
     :               'SPECCONT: Error: Invalid label space ' //
     :               'requested.', STATUS )
                  GO TO 500
               END IF
 3          CONTINUE

*        Check window.
            IF ( LWORLD(1) .EQ. LWORLD(2) .OR.
     :           LWORLD(3) .EQ. LWORLD(4)      ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CZAD_E07',
     :            'SPECCONT: Error: Plot window with zero extent.',
     :            STATUS )
               GO TO 500
            END IF

*        Derive viewport from current picture and LABSPC.
            CALL PGSVP( LABSPC(2), 1.-LABSPC(4),
     :                  LABSPC(1), 1.-LABSPC(3) )

*        Set window from given WORLD or as derived from data.
            IF ( FILL .NE. 0 ) THEN
               CALL PGSWIN( LWORLD(1), LWORLD(2), LWORLD(3), LWORLD(4) )
            ELSE
               CALL PGWNAD( LWORLD(1), LWORLD(2), LWORLD(3), LWORLD(4) )
            END IF

         END IF


*     Plot box.
*     =========

*     Set attributes for whole plot: font, size, colour, thickness.
         IF ( ROMAN .NE. 0 ) CALL PGSCF( 2 )
         CALL PGSCH( CHIGHT )
         CALL PGSCI( COLOUR )
         CALL PGSLW( THICK )

*     If not overlay, Plot the box.
         IF ( OVER .EQ. 0 ) THEN

*        Plot the box and numeric labels according to box layout
*        requests, separately for each side of the box.
*        =======================================================

            BOXOPT = '      '
            IF ( AXES(1) .EQ. 1 ) BOXOPT(1:1) = 'B'
            IF ( TICK(1) .NE. 0 ) BOXOPT(2:3) = 'TS'
            IF ( TICK(1) .EQ. 1 ) BOXOPT(4:4) = 'I'
            IF ( NUML(1) .EQ. 1 ) BOXOPT(5:5) = 'N'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( BOXOPT, MAJOR(1), MINOR(1),
     :                     ' ', MAJOR(2), MINOR(2) )
            BOXOPT = '      '
            IF ( AXES(2) .EQ. 1 ) BOXOPT(1:1) = 'B'
            IF ( TICK(2) .NE. 0 ) BOXOPT(2:3) = 'TS'
            IF ( TICK(2) .EQ. 1 ) BOXOPT(4:4) = 'I'
            IF ( NUML(2) .NE. 0 ) BOXOPT(5:5) = 'N'
            IF ( NUML(2) .EQ. 1 ) BOXOPT(6:6) = 'V'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( ' ', MAJOR(1), MINOR(1),
     :                     BOXOPT, MAJOR(2), MINOR(2) )
            BOXOPT = '      '
            IF ( AXES(3) .EQ. 1 ) BOXOPT(1:1) = 'C'
            IF ( TICK(3) .NE. 0 ) BOXOPT(2:3) = 'TS'
            IF ( TICK(3) .EQ. 1 ) BOXOPT(4:4) = 'I'
            IF ( NUML(3) .EQ. 1 ) BOXOPT(5:5) = 'M'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( BOXOPT, MAJOR(1), MINOR(1),
     :                     ' ', MAJOR(2), MINOR(2) )
            BOXOPT = '      '
            IF ( AXES(4) .EQ. 1 ) BOXOPT(1:1) = 'C'
            IF ( TICK(4) .NE. 0 ) BOXOPT(2:3) = 'TS'
            IF ( TICK(4) .EQ. 1 ) BOXOPT(4:4) = 'I'
            IF ( NUML(4) .NE. 0 ) BOXOPT(5:5) = 'M'
            IF ( NUML(4) .EQ. 1 ) BOXOPT(6:6) = 'V'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( ' ', MAJOR(1), MINOR(1),
     :                     BOXOPT, MAJOR(2), MINOR(2) )

*        Plot the text labels.
*        The position must be specified in units of character height
*        and counts from the viewport edge. We would like the label to
*        be one character height inwards from the view surface.
*        At the moment we know only via LABSPC the fraction of the view
*        surface between the viewport and the view surface.
*        On the other hand the character height is 1/40 of the view
*        surface height.
*        ==============================================================

*        Aspect ratio of view surface (width/height).
*        First get the view port in millimetres.
*        Then transform it to view surface size in millimetres.
*        Then divide width by height.
            CALL PGQVP( 2, RTEMP(1), RTEMP(2), RTEMP(3), RTEMP(4) )
            XWIDTH = ( RTEMP(2) - RTEMP(1) )
            XWIDTH = XWIDTH / ( 1. - LABSPC(2) - LABSPC(4) )
            YWIDTH = ( RTEMP(4) - RTEMP(3) )
            YWIDTH = YWIDTH / ( 1. - LABSPC(1) - LABSPC(3) )
            ASPECT = XWIDTH / YWIDTH

*        Bottom label.
            IF ( TEXT(1) .EQ. 1 ) THEN
               CALL PGMTXT( 'B', LABSPC(1) * 40 / CHIGHT - 1.,
     :                      0.5, .5, LPLAB(1) )
            END IF

*        Left label.
            IF ( TEXT(2) .EQ. 1 ) THEN
               CALL PGMTXT( 'L', LABSPC(2) * 40 * ASPECT / CHIGHT - 1.,
     :                      0.5, .5, LPLAB(2) )
            END IF

*        Top label.
            IF ( TEXT(3) .EQ. 1 ) THEN
               CALL PGMTXT( 'T', LABSPC(3) * 40 / CHIGHT - 1.,
     :                      0.5, .5, LPLAB(3) )
            END IF

*        Right label.
            IF ( TEXT(4) .EQ. 1 ) THEN
               CALL PGMTXT( 'R', LABSPC(2) * 40 * ASPECT / CHIGHT - 1.,
     :                      0.5, .5, LPLAB(4) )
            END IF

         END IF


*     Plot contours.
*     ==============

*     Set dash attribute.
         CALL PGSLS( DASH )

*     Get a work space with contour levels.
         CALL NDF_TEMP( I, STATUS )
         CALL NDF_NEW( '_REAL', 1, 1, NMAJOR, I, WNDF(2), STATUS )
         CALL NDF_MAP( WNDF(2), 'DATA', '_REAL', 'WRITE',
     :                 IPATH, I, STATUS )
         CALL SPD_UAAJR( START, START+(NMAJOR-1)*STEP, NMAJOR,
     :                   %VAL( CNF_PVAL( IPATH ) ), STATUS )

*     Plot the contours.
         CALL PGCONB( %VAL( CNF_PVAL( DATA ) ), DIM(AXIS(1)),
     :                DIM(AXIS(2)), 1, DIM(AXIS(1)), 1, DIM(AXIS(2)),
     :                %VAL( CNF_PVAL( IPATH ) ), NMAJOR, TR, VAL__BADR )

*     Release the data and path work spaces.
         CALL NDF_ANNUL( WNDF(2), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500


*     Save viewport as DATA picture.
*     ==============================

         IF ( OVER .EQ. 0 )
     :      CALL SPD_UGAD( 'DATA', 'SPECDRE_SPECCONT', I, STATUS )

      END IF


*  Return.
*  =======

 500  CONTINUE

      END
