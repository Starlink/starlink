      SUBROUTINE SALIA1( PARAM, INDF2, INDF1, INDFR, SCS, P, PROJ,
     :                   EPOCH, METHOD, XY1, XY2, STATUS )
*+
*  Name:
*     SALIA1

*  Purpose:
*     Add history to the output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SALIA1( PARAM, INDF2, INDF1, INDFR, SCS, P, PROJ, EPOCH,
*                  METHOD, XY1, XY2, STATUS )

*  Description:
*     The HISTORY added to the output NDF consists of a record of the
*     input and output NDFs, and the values of key parameters.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter used to control HISTORY.
*     INDF2 = INTEGER (Given)
*        The output NDF identifier.
*     INDF1 = INTEGER (Given)
*        The input NDF identifier.
*     INDFR = INTEGER (Given)
*        The reference NDF identifier. If NDF__NOID then no reference
*        NDF is recorded.
*     SCS = CHARACTER * ( * ) (Given)
*        The value of the COORDS parameter.
*     P( 8 ) = DOUBLE PRECISION (Given)
*        The reference projection parameters.
*     PROJ = CHARACTER * ( * ) (Given)
*        The value of the PROJTYPE parameter.
*     EPOCH = DOUBLE PRECISION (Given)
*        The value of the EPOCH parameter.
*     METHOD = CHARACTER * ( * ) (Given)
*        The value of the METHOD parameter.
*     XY1( 2 ) = INTEGER (Given)
*        The value of the XY1 parameter.
*     XY2( 2 ) = INTEGER (Given)
*        The value of the XY2 parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER INDF1
      INTEGER INDF2
      INTEGER INDFR
      CHARACTER SCS*(*)
      DOUBLE PRECISION P( 8 )
      CHARACTER PROJ*(*)
      DOUBLE PRECISION EPOCH
      CHARACTER METHOD*(*)
      INTEGER XY1( 2 )
      INTEGER XY2( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATEXT*(IRA__SZFSC)! Formatted longitude value.
      CHARACTER BTEXT*(IRA__SZFSC)! Formatted latitude value.
      INTEGER LENGTH             ! Length of text string.
      INTEGER NLINES             ! No. of lines of text to store.
      CHARACTER TEXT( 14 )*80    ! Lines of history text.
      LOGICAL VALID              ! True if a reference NDF was given.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if a valid reference NDF was supplied.
      CALL NDF_VALID( INDFR, VALID, STATUS )

*  If so, record the input, output and reference NDFs.
      IF( VALID ) THEN
         CALL NDF_MSG( 'IN', INDF1 )
         CALL NDF_MSG( 'OUT', INDF2 )
         CALL NDF_MSG( 'REF', INDFR )
         CALL MSG_LOAD( ' ',
     :                  ' ^OUT created by aligning ^IN with ^REF',
     :                  TEXT( 1 ), LENGTH, STATUS )

*  Otherwise, record input and output NDFs.
      ELSE
         CALL NDF_MSG( 'IN', INDF1 )
         CALL NDF_MSG( 'OUT', INDF2 )
         CALL MSG_LOAD( ' ', ' ^OUT created from ^IN',
     :                  TEXT( 1 ), LENGTH, STATUS )
      END IF

*  Now store parameter values...
      TEXT( 2 ) = ' '
      TEXT( 3 ) = 'SKYALIGN parameter values:'

*  COORDS...
      CALL MSG_SETC( 'C', SCS )
      CALL MSG_LOAD( ' ', '   COORDS     = ^C', TEXT( 4 ), LENGTH,
     :               STATUS )

*  METHOD...
      CALL MSG_SETC( 'C', METHOD )
      CALL MSG_LOAD( ' ', '   METHOD     = ^C', TEXT( 5 ), LENGTH,
     :               STATUS )

*  XY1...
      IF( XY1( 1 ) .NE. VAL__BADI ) THEN
         CALL MSG_SETI( 'X', XY1( 1 ) )
         CALL MSG_SETI( 'Y', XY1( 2 ) )
         CALL MSG_LOAD( ' ', '   XY1        = (^X,^Y)', TEXT( 6 ),
     :                  LENGTH, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', '   XY1        = (null)', TEXT( 6 ),
     :                  LENGTH, STATUS )
      END IF

*  XY2...
      IF( XY2( 1 ) .NE. VAL__BADI ) THEN
         CALL MSG_SETI( 'X', XY2( 1 ) )
         CALL MSG_SETI( 'Y', XY2( 2 ) )
         CALL MSG_LOAD( ' ', '   XY2        = (^X,^Y)', TEXT( 7 ),
     :                  LENGTH, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', '   XY2        = (null)', TEXT( 7 ),
     :                  LENGTH, STATUS )
      END IF


*  If there was a reference NDF, record the no. of lines of text to be
*  saved.
      IF( VALID ) THEN
         NLINES = 7

*  If no reference NDF was given, format the sky coordinates of the
*  projection centre.
      ELSE
         CALL IRA_DTOC( P( 1 ), P( 2 ), SCS, 2, ATEXT, BTEXT, STATUS )

*  CENTRE_LAT...
         CALL MSG_SETC( 'C', BTEXT )
         CALL MSG_LOAD( ' ', '   CENTRE_LAT = ^C', TEXT( 8 ), LENGTH,
     :                  STATUS )

*  CENTRE_LON...
         CALL MSG_SETC( 'C', ATEXT )
         CALL MSG_LOAD( ' ', '   CENTRE_LON = ^C', TEXT( 9 ), LENGTH,
     :                  STATUS )

*  CENTRE_XY...
         CALL MSG_SETR( 'X', REAL( P( 3 ) ) )
         CALL MSG_SETR( 'Y', REAL( P( 4 ) ) )
         CALL MSG_LOAD( ' ', '   CENTRE_XY  = (^X,^Y)', TEXT( 10 ),
     :                  LENGTH, STATUS )

*  EPOCH...
         CALL MSG_SETD( 'D', EPOCH)
         CALL MSG_LOAD( ' ', '   EPOCH      = ^D', TEXT( 11 ), LENGTH,
     :                  STATUS )

*  PIXSIZE...
         CALL MSG_SETR( 'X', REAL( P( 5 )*IRA__R2AM ) )
         CALL MSG_SETR( 'Y', REAL( P( 6 )*IRA__R2AM ) )
         CALL MSG_LOAD( ' ', '   PIXSIZE    = (^X,^Y)', TEXT( 12 ),
     :                  LENGTH, STATUS )

*  PROJTYPE...
         CALL MSG_SETC( 'C', PROJ)
         CALL MSG_LOAD( ' ', '   PROJTYPE   = ^C', TEXT( 13 ), LENGTH,
     :                  STATUS )

*  ORIENT...
         CALL MSG_SETR( 'R', REAL( P( 7 )*IRA__R2AM ) )
         CALL MSG_LOAD( ' ', '   ORIENT     = ^R', TEXT( 14 ),
     :                  LENGTH, STATUS )

*  Record the number of lines.
         NLINES = 14

      END IF

*  Now put the text into a history record.
      CALL IRM_HIST( 'HISTORY', INDF2, 'IRAS90:SKYALIGN', NLINES, TEXT,
     :               STATUS )

      END
