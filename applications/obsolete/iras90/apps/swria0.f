      SUBROUTINE SWRIA0( PATRB, PDIR, PHIGT, PRATIO, PJSTF, PSPACE,
     :                   PFONT, PPEN, DIR, HEIGHT, RATIO, JSTFCT,
     :                   SPACE, FONT, PEN, STATUS )
*+
*  Name:
*     SWRIA0

*  Purpose:
*     Get text attributes from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRI0( PATRB, PDIR, PHIGT, PRATIO, PJSTF, PSPACE, PFONT,
*                 PPEN, DIR, HEIGHT, RATIO, JSTFCT, SPACE, FONT, PEN,
*                 STATUS )

*  Description:
*     This subroutine set the text attributes according to the user's
*     input from the keyboard. The subroutine will keep prompting the
*     user for the next attribute to set after setting an attribute
*     until a null response is obtained.

*  Arguments:
*     PATRB = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the name of the text
*        attribute to be set.
*     PDIR = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of text
*        direction. 
*     PHIGT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of text
*        height.
*     PRATIO = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of text
*        aspect ratio.
*     PJSTF = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of text
*        justification.
*     PSPACE = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of the
*        spaces between characters.
*     PFONT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of text
*        font.
*     PPEN = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the new value of the pen
*        used to write the texts.
*     DIR( 2 ) = REAL (Given and Returned)
*        The up direction vector of the text. 
*     HEIGHT = REAL (Given and Returned )
*        The height of the text.
*     RATIO = REAL (Given and Returned)
*        The aspect ratio of the text.
*     JSTFCT = CHARACTER*( * ) (Given and Returned)
*        The justification of the text.
*     SPACE = REAL (Given and Returned)
*        The space of the characters in texts.
*     FONT = INTEGER (Given and Returned)
*        The font of the text
*     PEN = INTEGER (Given and Returned)
*        The number of the pen used to write the text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     27-AUG-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'SKY_PAR'          ! SKY_ constants
                              
*  Arguments Given:
      CHARACTER*( * ) PATRB, PDIR, PHIGT, PRATIO, PJSTF, PSPACE,
     :                PFONT, PPEN
      

*  Arguments Given and Returned:
      REAL DIR( 2 )
      REAL HEIGHT, RATIO
      CHARACTER*( * ) JSTFCT
      REAL SPACE
      INTEGER FONT, PEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local variables:
      CHARACTER*( 15 ) ATRB      ! Attribute to set
      CHARACTER*( 3 ) CVAL       ! Character string
      REAL DEFDIR( 2 )           ! Default text direction 
      LOGICAL EXIT               ! Exit flag
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Enter a do loop to set the attributes until exit is required.
      EXIT = .FALSE.
      DO WHILE ( .NOT.EXIT .AND. STATUS .EQ. SAI__OK )
      
*  See which attribute to set.
         CALL PAR_CHOIC( PATRB, 'DEFAULT', 'DEFAULT,SHOW,DIRECTION,'/
     :                 /'HEIGHT,ASPECT_RATIO,JUSTIFICATION,SPACE,FONT,'/
     :                 /'PEN', .FALSE., ATRB, STATUS )

*  If get a null response, set exit flag.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            EXIT = .TRUE.

*  If an attribute is obtained, see what it is.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  If default setting is selected, assign default values to the
*  attributeS.
            IF ( ATRB( : 7 ) .EQ. 'DEFAULT' ) THEN
               DIR( 1 ) = SKY__DFDR1
               DIR( 2 ) = SKY__DFDR2
               HEIGHT = SKY__DFHT
               RATIO = SKY__DFRTO
               JSTFCT = SKY__DFJST
               SPACE = SKY__DFSPC
               FONT = SKY__DFFNT
               PEN = SKY__DFPEN
               EXIT = .TRUE.

*  If the user wants current attribute setting, ...
            ELSE IF ( ATRB( : 4 ) .EQ. 'SHOW' ) THEN
               CALL MSG_BLANKIF( MSG__NORM, STATUS )

               CALL MSG_SETR( 'DX', DIR( 1 ) )
               CALL MSG_SETR( 'DY', DIR( 2 ) )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG1',
     :'  DIRECTION: text up direction - ( ^DX, ^DY )', STATUS )

               CALL MSG_SETR( 'H', HEIGHT )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG2',
     :'  HEIGHT: text height as a fraction of X size of the image - ^H',
     :                         STATUS )

               CALL MSG_SETR( 'R', RATIO )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG3',
     :'  ASPECT_RATIO: character aspect ratio (width/height) - ^R',
     :                        STATUS )

               CALL MSG_SETC( 'J', JSTFCT( : 2 ) )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG4',
     :'  JUSTIFICATION: text justification - ^J', STATUS )

               CALL MSG_SETR( 'S', SPACE )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG5',
     :'  SPACE: space between characters - ^S', STATUS )

               CALL MSG_SETI( 'F', FONT )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG6',
     :'  FONT - GKS font number - ^F', STATUS )

               CALL MSG_SETI( 'P', PEN )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIA0_MSG7',
     :'  PEN - pen number - ^P', STATUS )

               CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If the up direction of the text to be set, ask for new value.
            ELSE IF ( ATRB( : 9 ) .EQ. 'DIRECTION' ) THEN
               DEFDIR( 1 ) = SKY__DFDR1
               DEFDIR( 2 ) = SKY__DFDR2
               CALL PAR_DEF1R( PDIR, 2, DEFDIR, STATUS )
               CALL PAR_EXACR( PDIR, 2, DIR, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PDIR, STATUS )
      
*  If height of the text is to be set, get a value from the user
            ELSE IF ( ATRB( : 6 ) .EQ. 'HEIGHT' ) THEN
               CALL PAR_DEF0R( PHIGT, SKY__DFHT, STATUS )
               CALL PAR_GET0R( PHIGT, HEIGHT, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PHIGT, STATUS )

*  If aspect ratio is to be set, get a value from the user.            
            ELSE IF ( ATRB( : 12 ) .EQ. 'ASPECT_RATIO' ) THEN
               CALL PAR_DEF0R( PRATIO, SKY__DFRTO, STATUS )
               CALL PAR_GET0R( PRATIO, RATIO, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PRATIO, STATUS )

*  If the justification is to be set, get a value from the user.
            ELSE IF ( ATRB( : 13 ) .EQ. 'JUSTIFICATION' ) THEN
               CALL PAR_CHOIC( PJSTF, 'BL', 
     :                         'BL,BC,BR,CL,CC,CR,TL,TC,TR', .FALSE.,
     :                         JSTFCT, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PJSTF, STATUS )

*  If the text space is to be set, set the space.
            ELSE IF ( ATRB( : 5 ) .EQ. 'SPACE' ) THEN
               CALL PAR_DEF0R( PSPACE, SKY__DFSPC, STATUS )
               CALL PAR_GET0R( PSPACE, SPACE, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PSPACE, STATUS )

*  If the text font is to be set, set a font number.
            ELSE IF ( ATRB( : 4 ) .EQ. 'FONT' ) THEN
               CALL PAR_CHOIC( PFONT, '1', 
     :                      '1,101,102,103,104,105,106,107,108,109,110',
     :                         .FALSE., CVAL, STATUS )
               CALL CHR_CTOI( CVAL, FONT, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PFONT, STATUS )

*  If the pen number is to be set, set a pen.
            ELSE IF ( ATRB( : 3 ) .EQ. 'PEN' ) THEN
               CALL PAR_DEF0I( PPEN, SKY__DFPEN, STATUS )
               CALL PAR_GET0I( PPEN, PEN, STATUS )

*  Cancel the parameter for use next time.
               CALL PAR_CANCL( PPEN, STATUS )
            END IF

*  If a null is obtained when setting one particular attribute, reset
*  the status to prevent exit.
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
       
         END IF

*  Cancel the parameter PATRB to get another attribute to set.
         CALL PAR_CANCL( PATRB, STATUS )
      END DO
      
      END
