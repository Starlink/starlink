      SUBROUTINE NDF1_GTENV( NAME, DEF, VAL, LVAL, STATUS )
*+
* Name:
*    NDF1_GTENV

*  Purpose:
*     Translate an environment variable.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GTENV( NAME, DEF, VAL, LVAL, STATUS )

*  Description:
*     The routine translates an environment variable, returning a
*     logical value to indicate if a translation exists. If it does, the
*     translated value and its length are also returned.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the environment variable to be translated.
*     DEF = LOGICAL (Returned)
*        Returns .TRUE. if a translation exists, otherwise .FALSE..
*     VAL = CHARACTER * ( * ) (Returned)
*        The translation value, if defined.
*     LVAL = INTEGER (Returned)
*        The number of significant characters in the translation value
*        (i.e. ignoring trailing blanks). A value of zero is returned if
*        no translation is defined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The interpretation of the term "environment variable" is dependent
*     on the host operating system. For example, logical names fill this
*     role on VMS.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-NOV-1994 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSX_ERR'          ! PSX_ error codes
      
*  Arguments Given:
      CHARACTER * ( * ) NAME
      
*  Arguments Returned:
      LOGICAL DEF
      CHARACTER * ( * ) VAL
      INTEGER LVAL
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Initialise.
      DEF = .FALSE.
      LVAL = 0

*  Mark the error stack.
      CALL ERR_MARK

*  Attempt to translate the environment variable.
      CALL PSX_GETENV( NAME, VAL, STATUS )

*  If translation succeeded, note the environment variable is defined
*  and find the length of its translation.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DEF = .TRUE.
         LVAL = CHR_LEN( VAL )

*  If there was no translation, simpy annul the error.
      ELSE IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Release ther error stack.
      CALL ERR_RLSE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_GTENV', STATUS )

      END
