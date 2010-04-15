      SUBROUTINE GETDIR( DIRNAM, ENV, PATH, PLEN, STATUS )
*+
*  Name:
*     GETDIR

*  Purpose:
*     Get the path of a directory specified by an environment
*     variable or logical name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETDIR( DIRNAM, ENV, PATH, PLEN, STATUS )

*  Description:
*     An attempt is made to translate the supplied environment variable or
*     logical name. If this fails, the associated error message is
*     annulled and STATUS is returned indicating that no error has occurred
*     (PATH is returned blank in this case). If the variable is translated
*     successfully, then a slash is added to the end of it if required, to
*     ensure that a file name can be appended directly to the end of the
*     returned path.

*  Arguments:
*     DIRNAM = CHARACTER * ( * ) (Given)
*        The environment variable or logical name to be translated.
*     ENV = CHARACTER * ( * ) (Given)
*        A comma separated list of "name=value" pairs to over-ride the
*        values supplied in environment variables.
*     PATH = CHARACTER * ( * ) (Returned)
*        The directory path.
*     PLEN = INTEGER (Returned)
*        The index of the last non-blank character in PATH.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If PATH is returned blank, then PLEN is returned set to 1, NOT
*     zero. Therefore, PLEN should not be used to check for blank strings.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER DIRNAM*(*)
      CHARACTER ENV*(*)

*  Arguments Returned:
      CHARACTER PATH*(*)
      INTEGER PLEN

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        MACH*1,            ! Hardware name
     :        NODE*1,            ! Node name
     :        REL*1,             ! OS release ID
     :        SYS*10,            ! Operating system
     :        VERS*1             ! OS version ID

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value of the supplied logical name (on VMS) or environment
*  variable (on UNIX).
      CALL GTENV( DIRNAM, ENV, PATH, STATUS )

*  If it was not defined, annul the error message. Ensure that the directory
*  path returned is blank.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         PATH = ' '
         PLEN = 0

*  Otherwise, find the index of the last non-blank chartacter in PATH.
      ELSE
         PLEN = CHR_LEN( PATH )
         IF( PLEN .GT. 0 ) THEN

*  Find out what operating system we are on.
            CALL PSX_UNAME( SYS, NODE , REL, VERS, MACH, STATUS )

*  If we are on VMS, return the PATH without modification. If we are not
*  on VMS, add a slash to the end of the PATH, so long as there is room
*  for one, and so long as the PATH does not already end with a slash.
            IF( INDEX( SYS, 'VMS' ) .EQ. 0 ) THEN

               IF( PATH( PLEN : PLEN ) .NE. '/' ) THEN
                  PLEN = PLEN + 1

                  IF( PLEN .GT. LEN( PATH ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'DIR', DIRNAM )
                     CALL ERR_REP( 'GETDIR_ERR1', 'Translation of '//
     :                             '"^DIR" is too long to store '//
     :                             '(programming error).', STATUS )
                  ELSE
                     PATH( PLEN : PLEN ) = '/'
                  END IF

               END IF

            END IF

         END IF

      END IF

      IF( PATH .EQ. ' ' ) PLEN = 1

      END
