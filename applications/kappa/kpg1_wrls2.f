      SUBROUTINE KPG1_WRLS2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS,
     :                       TITLE, ID0, IDENTS, STATUS )
*+
*  Name:
*     KPG1_WRLS2

*  Purpose:
*     Puts a set of PIXEL positions into a text file as a positions list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRLS2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS, TITLE, 
*                      ID0, IDENTS, STATUS )

*  Description:
*     This routine writes the supplied positions to a text file.
*     Each line contains an integer identifier followed by unformatted
*     axis values, separated by white space. A dump of the supplied 
*     FrameSet (if any) is then stored in the text file as a set of comment 
*     lines. The file can be read using KPG1_RDLST. 

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of the positions array. This must 
*        be larger than or equal to NPOS.
*     NPOS = INTEGER (Given)
*        The number of positions to store in the file.
*     NAX = INTEGER (Given)
*        The number of axes in the PIXEL Frame.
*     POS( ARRDIM, NAX ) = DOUBLE PRECISION (Given)
*        The positions to store in the file. POS( I, J ) should give the
*        axis J value for position I. 
*     IWCS = INTEGER (Given)
*        A pointer to an AST FrameSet to store with the positions. 
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to store at the top of the text file. Ignored if blank.
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
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTGRP = INTEGER (Write)
*           GRP identifier for the group.
*        ASTGSP = CHARACTER * 1 (Write)
*           The character to be prepended to the text. Ignored if blank.

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
      EXTERNAL KPG1_ASGFW        ! Sink function for AST Channel
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER COL0               ! Column no. for start of first axis value
      PARAMETER ( COL0 = 18 )

      INTEGER ID                 ! Column no. for start of identifier
      PARAMETER ( ID = 7 )

      INTEGER NSP                ! No. of spaces following each axis value
      PARAMETER ( NSP = 1 )

      INTEGER AW                 ! Field width occupied by an axis value
      PARAMETER ( AW = VAL__SZD + NSP )

*  Local Variables:
      CHARACTER ATTRIB*10        ! AST attribute name
      CHARACTER BUFFER*(GRP__SZNAM)! Text buffer
      CHARACTER FTTL*80          ! Frame title
      CHARACTER SYM*50           ! Axis symbol
      INTEGER CHAN               ! AST pointer to Channel
      INTEGER FRM                ! Pointer to Frame
      INTEGER I                  ! Position index
      INTEGER IAT                ! No. of characters in string
      INTEGER IGRP               ! GRP identifier for group holding file contents
      INTEGER J                  ! Axis index
      INTEGER JAT                ! No. of characters in string
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a pointer to the Base Frame, and store the Frame title (if set).
      FRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      FTTL = AST_GETC( FRM, 'TITLE', STATUS )
      CALL KPG1_PGESC( FTTL, STATUS )

*  Create a GRP to act as a temporary staging post for the file.
      CALL GRP_NEW( ' ', IGRP, STATUS )

*  Store a header for the text file.
      IF( TITLE .NE. ' ' ) THEN
         BUFFER = '#  Title: '
         IAT = 10
         CALL CHR_APPND( TITLE, BUFFER, IAT )
         CALL GRP_PUT( IGRP, 1, BUFFER( : IAT ), 0, STATUS ) 
         CALL GRP_PUT( IGRP, 1, ' ', 0, STATUS ) 
      END IF

      BUFFER = '#  Position'
      IAT = ( NAX*AW + 2*COL0 - 1 )/2 - CHR_LEN( FTTL )/2 + 1
      CALL CHR_APPND( FTTL, BUFFER, IAT )
      CALL CHR_APPND( ':', BUFFER, IAT )
      CALL GRP_PUT( IGRP, 1, BUFFER( : IAT ), 0, STATUS ) 
      
      BUFFER = '#  identifier'

*  Add symbols for each axis.
      DO J = 1, NAX

*  Get the symbol for this axis.
         ATTRIB = 'SYMBOL('
         JAT = 7
         CALL CHR_PUTI( J, ATTRIB, JAT )
         CALL CHR_APPND( ')', ATTRIB, JAT )
         SYM = AST_GETC( FRM, ATTRIB( : JAT ), STATUS )
         CALL KPG1_PGESC( SYM, STATUS )

*  Store the length of the axis symbol.
         JAT = CHR_LEN( SYM )

*  Determine where to put the symbol.
         IAT = ( J - 1 )*AW + COL0 + MAX( 0, ( AW - JAT )/ 2 )

*  Store it in the buffer.
         CALL CHR_APPND( SYM( : JAT ), BUFFER, IAT )

      END DO

      CALL GRP_PUT( IGRP, 1, BUFFER( : IAT ), 0, STATUS ) 


      BUFFER = '#  ----------'

      IAT = COL0
      DO J = 1, NAX

         DO I = IAT, IAT + VAL__SZD - 1
            BUFFER( I : I ) = '-'
         END DO

         IAT = IAT + AW

      END DO

      CALL GRP_PUT( IGRP, 1, BUFFER( : IAT ), 0, STATUS ) 

*  Add a blank line.
      CALL GRP_PUT( IGRP, 1, ' ', 0, STATUS ) 

*  Loop round each supplied position.
      DO I = 1, NPOS

*  Get the integer identifier for this line.

*  Format this line.
         BUFFER = ' '
         IAT = ID - 1

         IF( ID0 .GT. 0 ) THEN
            CALL CHR_PUTI( ID0 + I - 1, BUFFER, IAT )         
         ELSE
            CALL CHR_PUTI( IDENTS( I ), BUFFER, IAT )         
         END IF

         DO J = 1, NAX
            IF( POS( I, J ) .NE. AST__BAD ) THEN
               IAT = ( J - 1 )*AW + COL0 
               CALL CHR_PUTD( POS( I, J ), BUFFER, IAT )
            ELSE
               IAT = ( J - 1 )*AW + COL0 + AW/2 - 4
               CALL CHR_APPND( '<bad>', BUFFER, IAT )
            END IF
         END DO

*  Store this line in the group.
         CALL GRP_PUT( IGRP, 1, BUFFER( : IAT ), 0, STATUS ) 

      END DO

*  Now dump the FrameSet if one was supplied.
      IF( IWCS .NE. AST__NULL ) THEN

         CALL GRP_PUT( IGRP, 1, ' ', 0, STATUS ) 
         BUFFER = '#'
         DO I = 2, 80
            BUFFER( I : I ) = '-'
         END DO
         CALL GRP_PUT( IGRP, 1, BUFFER( : 70 ), 0, STATUS ) 

         BUFFER = '# '
         IAT = 2
         CALL CHR_APPND( 'The above positions refer to the Base '//
     :                   'Frame (Frame', BUFFER, IAT )
         IAT = IAT + 1
         CALL CHR_PUTI( AST_GETI( IWCS, 'BASE', STATUS ), BUFFER, IAT )
         CALL CHR_APPND( ') of the following', BUFFER, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( AST_GETC( IWCS, 'CLASS', STATUS ), BUFFER, 
     :                   IAT )
         CALL GRP_PUT( IGRP, 1, BUFFER( : 70 ), 0, STATUS ) 
         CALL GRP_PUT( IGRP, 1, ' ', 0, STATUS ) 

*  Create an AST Channel which can be used to write the FrameSet description
*  to the group in the form of comments. Do not store any unnecessary
*  information in the output file. Prepend each line of the output with 
*  "#" to make it a comment.
         CHAN = AST_CHANNEL( AST_NULL, KPG1_ASGFW, 'FULL=-1', STATUS )
         ASTGRP = IGRP
         ASTGSP = '#'

*  Write the FrameSet to the group. Report an error if nothing is
*  written.
         IF( AST_WRITE( CHAN, IWCS, STATUS ) .EQ. 0 .AND. 
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_WRLS2_ERR', 'KPG1_WRLS2: Failed to '//
     :                    'write a FrameSet to a GRP group (possible '//
     :                    'programming error).', STATUS )
         END IF

*  Annul the channel.
         CALL AST_ANNUL( CHAN, STATUS )

      END IF

*  Write the group contents out to a file.
      CALL GRP_LIST( PARAM, 0, 0, ' ', IGRP, STATUS ) 

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

      END
