      LOGICAL FUNCTION PAREAL( PAR, VAL )
*+
*  Name:
*     PAREAL

*  Purpose:
*     Tests if character string is a real value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PAREAL( PAR, VAL )

*  Description:
*     This routine checks if the value stored in PAR can be converted
*     into a single precision value or not. If it can the return is set
*     true and the converted value is returned in VAL.

*  Arguments:
*     PAR = CHARACTER * ( * ) (Given)
*        The string which might be a real value.
*     VAL = REAL (Returned)
*        The real value if conversion is achieved.

*  Returned Value:
*     PAREAL = LOGICAL
*        True if the value has been converted to a real value, false
*        otherwise.

*  Side Effects:
*     -  This function modifies the argument VAL.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-JUN-1994 (PDRAPER):
*        Added prologue and made platform independent, original author
*        unknown (olaf?).
*     7-NOV-1994 (PDRAPER):
*        Now uses standard Starlink routines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE parameters

*  Arguments Given:
      CHARACTER * ( * ) PAR

*  Arguments Returned:
      REAL VAL

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( 10 ) FMT     ! Format statement
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Position of first non-blank character
      INTEGER LP                 ! Used length of input string
      INTEGER STATUS             ! Local status value
*.

*  Set up the initial values.
      PAREAL = .FALSE.

*  Use the standard character to real conversion routine.
      STATUS = SAI__OK
      CALL CHR_FANDL( PAR, J, LP )
      CALL CHR_CTOR( PAR( J : LP ), VAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         PAREAL = .FALSE.
      ELSE
         PAREAL = .TRUE.
      END IF
      END
* $Id$
