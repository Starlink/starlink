      SUBROUTINE POL1_GTCTW( CI, IWCS, STATUS )
*+
*  Name:
*     POL1_GTCTW

*  Purpose:
*     Attempt to read an AST FrameSet from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_GTCTW( CI, IWCS, STATUS )

*  Description:
*     This routine attempts to read an AST FrameSet from the textual
*     information stored with the supplied catalogue (class COMMENT).
*     If not succesful, a null pointer is returned without error.

*  Arguments:
*     CI = INTEGER (Given)
*        A CAT identifier (see SUN/181) for the supplied catalogue.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the returned FrameSet. AST__NULL is returned if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-AUG-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT constants

*  Arguments Given:
      INTEGER CI

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR

*  Local Variables:
      LOGICAL DONE               ! Have we read enough AST Objects?
*.

*  Initialise.
      IWCS = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Reset the pointer for the next item of textual information to be read
*  from the catalogue.
      CALL CAT_RSTXT( CI, STATUS )

*  Read Objects from the catalogue until a FrameSet is obtained, or no
*  more Objects are left.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )
         CALL KPG1_RCATW( CI, IWCS, STATUS )

         IF( IWCS .NE. AST__NULL ) THEN
            IF( AST_ISAFRAMESET( IWCS, STATUS ) ) THEN
               DONE = .TRUE.
            ELSE
               CALL AST_ANNUL( IWCS, STATUS )
            END IF

         ELSE
            DONE = .TRUE.
         END IF

      END DO

*  If an error has occurred, annul the returned FrameSet pointer. Otherwise
*  export the pointer (if it is not null) from the current AST context.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IWCS, STATUS )

      ELSE IF( IWCS .NE. AST__NULL ) THEN
         CALL AST_EXPORT( IWCS, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
