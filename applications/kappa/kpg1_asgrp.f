      SUBROUTINE KPG1_ASGRP( FRM, IGRP, NP, NAX, OUT, STATUS )
*+
*  Name:
*     KPG1_ASGRP

*  Purpose:
*     Read spatial positions from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGRP( FRM, IGRP, NP, NAX, OUT, STATUS )

*  Description:
*     This routine reads formatted positions from a GRP group. The
*     positions are assumed to be in the supplied Frame. Each element 
*     in the group should contain 1 position per line. Each position is 
*     given by a set of strings delimited by comma, space or tab (the 
*     first gives the value for axis 1, the second for axis 2, etc). The 
*     number of strings per element in the group should equal the number 
*     of axes in the Base Frame of the supplied FrameSet.
*
*     An error is reported if any unreadable elements are found.

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to an AST Frame.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group to read.
*     NP = INTEGER (Given)
*        The number of positions which can be stored in the returned array.
*     NAX = INTEGER (Given)
*        The number of axes in the supplied Frame.
*     OUT( NP, NAX ) = DOUBLE PRECISION (Returned)
*        The array to hold the returned co-ordinates. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER FRM
      INTEGER IGRP
      INTEGER NP
      INTEGER NAX

*  Arguments Returned:
      DOUBLE PRECISION OUT( NP, NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER MESS*30          ! Error message phrase
      CHARACTER TEXT*(GRP__SZNAM)! Text of current element
      CHARACTER WORDS( NDF__MXDIM )*30! Words extracted from current element
      INTEGER I                  ! Element index
      INTEGER J                  ! Axis index
      INTEGER LSTAT              ! CHR status
      INTEGER NC                 ! No. of characters used by AST_UNFORMAT
      INTEGER NCW                ! No. of characters in current word
      INTEGER NWRD               ! No. of words in current element
      INTEGER SIZE               ! No. of elements in group
      INTEGER WSTART( NDF__MXDIM )! Index of start of each word
      INTEGER WSTOP( NDF__MXDIM )! Index of end of each word
      LOGICAL OK                 ! Can element be read?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Loop round each element in the group.
      DO I = 1, MIN( SIZE, NP )
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS ) 

*  Assume the element cannot be read.
         OK = .FALSE.

*  Replace commas and tabs by spaces.
         LSTAT = SAI__OK
         CALL CHR_TRCHR( ',	', '  ', TEXT, LSTAT ) 

*  Split the element up in to words delimited by spaces.
         CALL CHR_DCWRD( TEXT, NDF__MXDIM, NWRD, WSTART, WSTOP, WORDS, 
     :                   LSTAT ) 

*  If this element has too many or too few fields, store a message.
         IF( NWRD .LT. NAX ) THEN
            MESS = 'too few fields'

         ELSE IF( NWRD .GT. NAX ) THEN
            MESS = 'too many fields'

*  Otherwise...
         ELSE 

*  Assume the element can be read OK.
            OK = .TRUE.
 
*  Loop round each word in the element.
            DO J = 1, NAX

*  Store number of characters in this word.
               NCW = WSTOP( J ) - WSTART( J ) + 1

*  Attempt to read an axis value from the current word.
               NC = AST_UNFORMAT( FRM, J, WORDS( J )( : NCW ), 
     :                            OUT( I, J ), STATUS ) 

*  If the word could not be decoded, or if not all of the word was used,
*  we cannot use this group element.
               IF( NC .NE. NCW ) THEN
                  OK = .FALSE.
                  MESS = ' '
                  GO TO 10
               END IF

            END DO

 10         CONTINUE

         END IF

*  If this element could not be used, report an error.
         IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NAX )
            CALL MSG_SETC( 'TEXT', TEXT )

            IF( MESS .EQ. ' ' ) THEN
               CALL ERR_REP( 'KPG1_ASGRP_1', 'The following string '//
     :                       'could not  be interpreted as an ^N '//
     :                       'dimensional position: ``^TEXT''.', 
     :                       STATUS )
            ELSE
               CALL MSG_SETC( 'M', MESS )
               CALL ERR_REP( 'KPG1_ASGRP_2', 'The following string '//
     :                       'could not  be interpreted as an ^N '//
     :                       'dimensional position (^M): ``^TEXT''.', 
     :                       STATUS )

            END IF

         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Fill any un-used room in the array with bad values.
      DO I = MIN( SIZE, NP ) + 1, NP
         DO J = 1, NAX
            OUT( I, J ) = AST__BAD
         END DO
      END DO

 999  CONTINUE

      END
