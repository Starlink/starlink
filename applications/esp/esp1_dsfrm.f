      SUBROUTINE ESP1_DSFRM( FSET, TEXT, STATUS )
*+
*  Name:
*     ESP1_DSFRM

*  Purpose:
*     Display a textual description of the Current Frame in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ESP1_DSFRM( FSET, TEXT, STATUS )

*  Description:
*     This routine displays a textual description of the Current Frame
*     in the supplied AST FrameSet.

*  Arguments:
*     FSET = INTEGER (Given)
*        An AST pointer to the FrameSet.
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to display before the Frame description.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1998 (DSB):
*        Original version.
*     4-NOV-1999 (MBT):
*        Renamed from KPG1_ version and appropriated for ESP.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER FSET
      CHARACTER TEXT*(*)

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables :
      CHARACTER ATTRIB*20        ! AST Frame attribute name
      CHARACTER FRMDMN*80        ! Frame domain
      CHARACTER FRMTTL*80        ! Frame title
      CHARACTER PRJ*50           ! Sky projection
      CHARACTER SYS*30           ! Sky coordinate system
      CHARACTER UNIT*15          ! Units string
      DOUBLE PRECISION CFIRST( 1, NDF__MXDIM ) ! Frame coords of first pixel
      DOUBLE PRECISION EP        ! Epoch of observattion
      DOUBLE PRECISION EQ        ! Epoch of reference equinox
      DOUBLE PRECISION GFIRST( 1, NDF__MXDIM ) ! GRID coords of first pixel
      INTEGER CFRM               ! Frame to be described
      INTEGER FRMNAX             ! Frame dimensionality
      INTEGER IAT                ! Current length of a string
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER IBASE              ! Index of Base Frame in FrameSet
      INTEGER IGRID              ! Index of GRID Frame in FrameSet
      INTEGER NDIM               ! Number of dimensions
      LOGICAL GOTFS              ! Was a FrameSet supplied?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a pointer to the Frame to be described.
      GOTFS = AST_ISAFRAMESET( FSET, STATUS )

      IF( GOTFS ) THEN
         CFRM = AST_GETFRAME( FSET, AST__CURRENT, STATUS )

      ELSE IF( AST_ISAFRAME( FSET, STATUS ) ) THEN
         CFRM = AST_CLONE( FSET, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'CLASS', AST_GETC( FSET, 'CLASS', STATUS ) )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_DSFRM_ERR', 'KPG1_DSFRM: Inappropriate '//
     :                 'AST Object (class ^CLASS) supplied '//
     :                 '(programming error).', STATUS )
      END IF

*  Display any header text.
      IF( TEXT .NE. ' ' ) THEN
         CALL MSG_SETC( 'TEXT', TEXT )
         CALL MSG_OUT( 'KPG1_DSFRM_1', '^TEXT', STATUS )
      END IF
      CALL MSG_BLANK( STATUS )

*  Get the Frame title, domain and dimensionality.
      FRMTTL = AST_GETC( CFRM, 'TITLE', STATUS )
      FRMDMN = AST_GETC( CFRM, 'DOMAIN', STATUS )
      FRMNAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Display the title (upto 45 characters), and domain. The number of
*  dimensions is implied by the list of axes displayed later.
      CALL MSG_SETC( 'TTL', FRMTTL( : 45 ) )
      IF( CHR_LEN( FRMTTL ) .GT. 45 ) CALL MSG_SETC( 'TTL', '...' )

      CALL MSG_OUT( 'WCS_TITLE',
     :   '      Frame title         : "^TTL"', STATUS )

      CALL MSG_SETC( 'DOMAIN', FRMDMN )
      CALL MSG_OUT( 'WCS_DOMAIN',
     :   '      Domain              : ^DOMAIN', STATUS )

*  If the Frame is a SkyFrame, display the epoch, equinox, system and
*  projection.
      IF( AST_ISASKYFRAME( CFRM, STATUS ) ) THEN

*  First get the epoch, equinox, system and projection.
         EP = AST_GETD( CFRM, 'EPOCH', STATUS )
         EQ = AST_GETD( CFRM, 'EQUINOX', STATUS )
         SYS = AST_GETC( CFRM, 'SYSTEM', STATUS )
         PRJ = AST_GETC( CFRM, 'PROJECTION', STATUS )

*  Construct a message token holding suitable description for each type of
*  system...
*  RA/DEC...
         IF( SYS .EQ. 'FK4' .OR. SYS .EQ. 'FK5' ) THEN
            CALL MSG_SETC( 'SYS', 'Equatorial (' )
            CALL MSG_SETC( 'SYS', SYS )
            CALL MSG_SETC( 'SYS', ' -' )
            IF( EQ .LT. 1984.0 ) THEN
               CALL MSG_SETC( 'SYS', ' B' )
            ELSE
               CALL MSG_SETC( 'SYS', ' J' )
            END IF
            CALL MSG_SETD( 'SYS', EQ )
            CALL MSG_SETC( 'SYS', ')' )

         ELSE IF( SYS .EQ. 'FK4-NO-E' ) THEN
            CALL MSG_SETC( 'SYS', 'Equatorial without E-terms (FK4 -' )
            IF( EQ .LT. 1984.0 ) THEN
               CALL MSG_SETC( 'SYS', ' B' )
            ELSE
               CALL MSG_SETC( 'SYS', ' J' )
            END IF
            CALL MSG_SETD( 'SYS', EQ )
            CALL MSG_SETC( 'SYS', ')' )

*  Geocentric apparent...
         ELSE IF( SYS .EQ. 'GAPPT' ) THEN
            CALL MSG_SETC( 'SYS', 'Equatorial (geocentric apparent)' )

*  Ecliptic...
         ELSE IF( SYS .EQ. 'ECLIPTIC' ) THEN
            CALL MSG_SETC( 'SYS', 'Ecliptic (' )
            IF( EQ .LT. 1984.0 ) THEN
               CALL MSG_SETC( 'SYS', ' B' )
            ELSE
               CALL MSG_SETC( 'SYS', ' J' )
            END IF
            CALL MSG_SETD( 'SYS', EQ )
            CALL MSG_SETC( 'SYS', ')' )

*  Galactic...
         ELSE IF( SYS .EQ. 'GALACTIC' ) THEN
            CALL MSG_SETC( 'SYS', 'Galactic' )

*  Supergalactic...
         ELSE IF( SYS .EQ. 'SUPERGALACTIC' ) THEN
            CALL MSG_SETC( 'SYS', 'Supergalactic' )

*  Anything else..
         ELSE
            CALL MSG_SETC( 'SYS', SYS )
         END IF

*  Display the system.
         CALL MSG_OUT( 'WCS_SYS',
     :   '      System              : ^SYS', STATUS )

*  Display the epoch.
         IF( EP .LT. 1984.0 ) THEN
            CALL MSG_SETC( 'EPOCH', 'B' )
         ELSE
            CALL MSG_SETC( 'EPOCH', 'J' )
         END IF
         CALL MSG_SETD( 'EPOCH', EP )
         CALL MSG_OUT( 'WCS_EPOCH',
     :   '      Epoch of observation: ^EPOCH', STATUS )

*  Display the projection.
         IF( PRJ .NE. ' ' ) THEN
            CALL MSG_SETC( 'PROJ', PRJ )
            CALL MSG_OUT( 'WCS_PROJ',
     :   '      Projection          : ^PROJ', STATUS )
         END IF

      END IF

*  Only proceed if we have a FrameSet.
      IF( GOTFS ) THEN

*  See if there is a GRID Frame in the FrameSet.
         CALL ESP1_ASFFR( FSET, 'GRID', IGRID, STATUS )
         IF( IGRID .NE. AST__NOFRAME ) THEN

*  Ensure the GRID Frame is the Base Frame.
            IBASE = AST_GETI( FSET, 'BASE', STATUS )
            CALL AST_SETI( FSET, 'BASE', IGRID, STATUS )

*  Gets its dimensionality. Pass on if it is more than NDF__MXDIM.
            NDIM = AST_GETI( FSET, 'NIN', STATUS )
            IF( NDIM .LE. NDF__MXDIM ) THEN

*  Store the GRID coordinates of the centre of the first pixel. This is
*  defined to be (1.0,1.0,...). This position will be mapped into the
*  other Frame, to find the coordinates of the first pixel.
               DO IAXIS = 1, NDIM
                  GFIRST( 1, IAXIS ) = 1.0
               END DO

*  Map the GRID coordinates at the centre of the first pixel to obtain the
*  corresponding coordinates in the Frame.
               CALL AST_TRANN( FSET, 1, NDIM, 1, GFIRST, .TRUE., FRMNAX,
     :                         1, CFIRST, STATUS )

*  Display the resulting coordinates.
               CALL MSG_SETC( 'FIRST', AST_FORMAT( FSET, 1,
     :                                             CFIRST( 1, 1 ),
     :                                             STATUS ) )

               DO IAXIS = 2, FRMNAX
                  CALL MSG_SETC( 'FIRST', ',' )
                  CALL MSG_SETC( 'FIRST', ' ' )
                  CALL MSG_SETC( 'FIRST', AST_FORMAT( FSET, IAXIS,
     :                            CFIRST( 1, IAXIS ), STATUS ) )
               END DO

               CALL MSG_OUT( 'WCS_FIRSTP',
     :      '      First pixel centre  : ^FIRST', STATUS )
            END IF

*  Re-instate the original Base Frame.
            CALL AST_SETI( FSET, 'BASE', IBASE, STATUS )

         END IF

      END IF

*  Now display the axis number, label and units for each axis of the Frame.
      CALL MSG_BLANK( STATUS )
      DO IAXIS = 1, FRMNAX

*  Display the axis number.
         CALL MSG_SETI( 'IAXIS', IAXIS )
         CALL MSG_OUT( 'AXIS_NUMBER',
     :   '         Axis ^IAXIS:', STATUS )

*  Construct the name of the attribute holding the label for this axis.
         ATTRIB = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the label and display it.
         CALL MSG_SETC( 'LABEL', AST_GETC( CFRM, ATTRIB( : IAT ),
     :                                     STATUS ) )
         CALL MSG_OUT( 'AXIS_LABEL',
     :   '            Label: ^LABEL', STATUS )

*  Construct the name of the attribute holding the units for this axis.
         ATTRIB = 'UNIT('
         IAT = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the units string and display it (if not blank).
         UNIT = AST_GETC( CFRM, ATTRIB( : IAT ), STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL MSG_SETC( 'UNIT', UNIT )
            CALL MSG_OUT( 'AXIS_UNITS',
     :      '            Units: ^UNIT', STATUS )
         END IF

*  Add a spacing line after the information for each axis.
         CALL MSG_BLANK( STATUS )

      END DO

*  End the AST context.
      CALL AST_END( STATUS )

      END
