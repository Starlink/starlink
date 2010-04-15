      LOGICAL FUNCTION PARDBL( PAR, VAL )
*+
*  Name:
*     PARDBL

*  Purpose:
*     Tests if character string is a double precision value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PARDBL( PAR, VAL )

*  Description:
*     This routine checks if the value stored in PAR can be converted
*     into a double precision value or not. If it can the return is set
*     true and the converted value is returned in VAL.

*  Arguments:
*     PAR = CHARACTER * ( * ) (Given)
*        The string which might be a double precision value.
*     VAL = DOUBLE PRECISION (Returned)
*        The double precision value if conversion is achieved.

*  Returned Value:
*     PARDBL = LOGICAL
*        True if the value has been converted to a double precision
*        value, false otherwise.

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
*        Now uses standard Starlink functions.
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
      DOUBLE PRECISION VAL

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
      PARDBL = .FALSE.

*  Use the standard character to double precision conversion routine.
      STATUS = SAI__OK
      CALL CHR_FANDL( PAR, J, LP )
      CALL CHR_CTOD( PAR( J : LP ), VAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         PARDBL = .FALSE.
      ELSE
         PARDBL = .TRUE.
      END IF

      END
* $Id$
