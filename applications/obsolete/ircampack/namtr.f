      SUBROUTINE NAMTR( KMD, INSTR, OUTSTR, J )
*+
*  Name:
*     NAMTR

*  Purpose:
*     Translate help library names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NAMTR( KMD, INSTR, OUTSTR, J )

*  Description:
*     This routine provides the service of the NAMETR subroutine
*     required when calling HLP_HELP (see SUN/124). It checks to see
*     if the supplied string starts with a logical name or environment
*     variable. If it does, the reference is replaced by the value
*     in the returned string. Otherwise, the string is left unaltered.

*  Arguments:
*     KMD = INTEGER (Given)
*        The function flag. Ignored by this routine.
*     INSTR = CHARACTER * ( * ) (Given)
*        The supplied help library name.
*     OUTSTR = CHARACTER * ( * ) (Returned)
*        The help library name with logical name or environment
*        variable references replaced by their value.
*     J = INTEGER (Returned)
*        Status. Always returned equal to 0.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1993 (DSB):
*        Original version, copied from IRAS90 routine IRM1_NAMTR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER KMD
      CHARACTER INSTR*(*)

*  Arguments Returned:
      CHARACTER OUTSTR*(*)
      INTEGER J

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Variables:
      CHARACTER
     :        TRANS*255,         ! Value of log. name or env. variable.
     :        VARNAM*80          ! Log. name or env. variable.

      INTEGER
     :        BEGIN,             ! Position of first character of
                                 ! variable name.
     :        END,               ! Position of last character in INSTR.
     :        SEP,               ! Position of separator.
     :        START,             ! Position of first character in INSTR.
     :        STATUS,            ! Inherited status.
     :        TLEN               ! Used length of variable value.

*.

*  Find the start and end of the supplied string.
      CALL CHR_FANDL( INSTR, START, END )

*  Initialise the variable name to blank.
      VARNAM = ' '

*  Find the position of the first colon...
      SEP = INDEX( INSTR, ':' )

*  If found, use the first character as the start of the logical name.
      IF( SEP .GT. 0 ) THEN
         BEGIN = START
         VARNAM = INSTR( BEGIN + 1 : SEP - 1 )

*  If not found, see if the first character is a dollar...
      ELSE
         IF( INSTR( START : START ) .EQ. '$' ) THEN

*  If it is, use the second character as the start of the environment
*  variable name and find the first occurence of a / character.
            BEGIN = START + 1
            SEP = INDEX( INSTR, '/' )

*  If a / was found, store the environment variable name.
            IF( SEP .GT. 0 ) VARNAM = INSTR( BEGIN : SEP - 1 )

         END IF

      END IF

*  If a variable name was found, attempt to translate it.
      IF( VARNAM .NE. ' ' ) THEN
         STATUS = SAI__OK
         CALL PSX_GETENV( VARNAM( : SEP - BEGIN ), TRANS, STATUS )

*  If the variable could not be translated, annul the error and return
*  the supplied string unaltered.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            OUTSTR = INSTR

*  If a translation was obtained, construct the output string.
         ELSE
            TLEN = CHR_LEN( TRANS )
            OUTSTR = TRANS( : TLEN )//INSTR( SEP + 1 : )
         END IF

*  If no variables was found in the supplied string, return the supplied
*  string unaltered.
      ELSE
         OUTSTR = INSTR
      END IF

*  Set returned status.
      J = 0

      END
