      SUBROUTINE KPG1_WRLS2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS,
     :                       TITLE, ID0, IDENTS, STATUS )
*+
*  Name:
*     KPG1_WRLS2

*  Purpose:
*     Puts a set of positions into a text file as a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRLS2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS, TITLE, 
*                      ID0, IDENTS, STATUS )

*  Description:
*     This routine writes the supplied positions to a CAT catalogue
*     (see SUN/181). A dump of the supplied FrameSet (if any) is included
*     in the text file as a set of "text" lines. A column is created
*     with name "PIDENT" to contain the integer identifiers. A column is 
*     also created for each axis of the Base Frame, with a name equal to 
*     the Symbol attribute of the Axis (AXIS_<n> is used if the Symbol is 
*     blank). The catalogue can be read using KPG1_RDLST (and also XCATVIEW 
*     etc). 

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of the positions array. This must 
*        be larger than or equal to NPOS.
*     NPOS = INTEGER (Given)
*        The number of positions to store in the file.
*     NAX = INTEGER (Given)
*        The number of axes for each position.
*     POS( ARRDIM, NAX ) = DOUBLE PRECISION (Given)
*        The positions to store in the file. POS( I, J ) should give the
*        axis J value for position I. The positions should be in the Base
*        Frame of the FrameSet supplied using argument IWCS.
*     IWCS = INTEGER (Given)
*        A pointer to an AST FrameSet to store with the positions. 
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to store at the top of the text file. 
*     ID0 = INTEGER (Given)
*        The integer identifier value to associate with the first
*        supplied position. Identifiers for subsequent positions increase
*        by 1 for each position. If this is supplied less than or equal
*        to zero, then its value is ignored and the identifiers supplied
*        in array IDENTS are used instead.
*     IDENTS( NPOS ) = INTEGER (Given)
*        The individual integer identifiers to associate with each
*        position. Only accessed if ID0 is less than or equal to zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER ARRDIM
      INTEGER NPOS
      INTEGER NAX
      DOUBLE PRECISION POS( ARRDIM, NAX )
      INTEGER IWCS
      CHARACTER TITLE*(*)
      INTEGER ID0
      INTEGER IDENTS( NPOS )
 
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER ATTR*10          ! AST attribute name
      CHARACTER BUFFER*80        ! Text buffer
      CHARACTER LAB*50           ! Axis label
      CHARACTER SYM*20           ! Axis symbol
      CHARACTER UNT*20           ! Axis units
      INTEGER CI                 ! CAT identifier for catalogue
      INTEGER COLID( 0:NDF__MXDIM )! CAT identifiers for columns
      INTEGER FRM                ! Pointer to Frame
      INTEGER I                  ! Position index
      INTEGER IAT                ! No. of characters in string
      INTEGER J                  ! Axis index
      INTEGER LWCS               ! Pointer to the FrameSet to be stored
      INTEGER TI                 ! CAT identifier for TITLE parameter
      LOGICAL COPIED             ! Has a copy of the Frameet been taken?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST Context.
      CALL AST_BEGIN( STATUS )

*  Take a clone of the FrameSet pointer, and indicate that we have a
*  clone and not a copy.
      LWCS = AST_CLONE( IWCS, STATUS )
      COPIED = .FALSE.

*  Get a pointer to the Base Frame.
      FRM = AST_GETFRAME( LWCS, AST__BASE, STATUS )

*  Create the output catalogue.
      CALL LPG_CATCREAT( PARAM, CI, STATUS )

*  Store the supplied title as the catalogue's TITLE parameter. 
      CALL CAT_PPTAC( CI, 'TITLE', MAX( 1, CHR_LEN( TITLE ) ), 
     :                CAT__SCALR, 1, ' ', ' ', .TRUE., 'Title', TITLE, 
     :                TI, STATUS) 

*  Create a column to hold integer identifiers for each position.
      CALL CAT_CNEWS( CI, 'PIDENT', CAT__TYPEI, 0, ' ', ' ', 
     :                'Position identifier', COLID( 0 ), STATUS )

*  Loop round creating columns for each axis.
      DO I = 1, NAX

*  Get the Symbol, Unit and Label attributes for this axis.
         ATTR = 'Symbol('
         IAT = 7
         CALL CHR_PUTI( I, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )
         SYM = AST_GETC( FRM, ATTR( : IAT ), STATUS )

         ATTR( : 6 ) = ' Label'
         LAB = AST_GETC( FRM, ATTR( : IAT ), STATUS )

         ATTR( : 6 ) = '  Unit'
         UNT = AST_GETC( FRM, ATTR( : IAT ), STATUS )

*  The axis symbol is used as the column name. If the symbol is blank,
*  use "AXIS_<i>" instead. Take a copy of the FrameSet first to avoid
*  changing the original.
         IF( SYM .EQ. ' ' ) THEN
            SYM = 'AXIS_'
            IAT = 5
            CALL CHR_PUTI( I, SYM, IAT )

            IF( .NOT. COPIED ) THEN
               CALL AST_ANNUL( LWCS, STATUS )
               LWCS = AST_COPY( IWCS, STATUS )
               COPIED = .TRUE.
            END IF

            ATTR( : 6 ) = 'Symbol'
            CALL AST_SETC( FRM, ATTR, SYM( : IAT ), STATUS )

         END IF

*  Create the column.
         CALL CAT_CNEWS( CI, SYM, CAT__TYPED, 0, UNT, ' ', LAB, 
     :                   COLID( I ), STATUS )

      END DO

*  Loop round each supplied position.
      DO I = 1, NPOS

*  Store column values in the current row buffer...

*  The integer identifier.
         IF( ID0 .GT. 0 ) THEN
            CALL CAT_PUT0I( COLID( 0 ), ID0 + I - 1, .FALSE., STATUS )
         ELSE
            CALL CAT_PUT0I( COLID( 0 ), IDENTS( I ), .FALSE., STATUS )
         END IF

*  Loop round each axis...
         DO J = 1, NAX
            CALL CAT_PUT0D( COLID( J ), POS( I, J ), 
     :                      ( POS( I, J ) .EQ. AST__BAD ), STATUS )
         END DO

*  Append the current row buffer to the catalogue.
         CALL CAT_RAPND( CI, STATUS )

      END DO

*  Now dump the FrameSet if one was supplied.
      IF( LWCS .NE. AST__NULL ) THEN

*  Add a header to the textual information.
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      Coordinate system '//
     :                   'information follows, stored ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      as an AST FrameSet '//
     :                   '(see Starlink User Note 210).', STATUS )

         IF( NAX .EQ. 1 ) THEN
            BUFFER = '      The axis value stored in column 2 '
            IAT = 40 
         ELSE
            BUFFER = '      The axis values stored in columns 2 to '
            IAT = 45
            CALL CHR_PUTI( NAX + 1, BUFFER, IAT )
            IAT = IAT + 1
         END IF
         CALL CHR_APPND( 'of the', BUFFER, IAT )

         CALL CAT_PUTXT( CI, 'COMMENT', BUFFER( : IAT ), STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      table refer to the '//
     :                   'Base Frame within this FrameSet.', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )

*  Write out the WCS information.
         CALL KPG1_WCATW( LWCS, CI, STATUS )

      END IF

*  Release the catalogue.
      CALL CAT_TRLSE( CI, STATUS )

*  Emnd the AST Context.
      CALL AST_END( STATUS )

      END
