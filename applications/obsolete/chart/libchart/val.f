      DOUBLE PRECISION FUNCTION VAL( STRING, J, K, STATUS )
*+
*  Name:
*     VAL

*  Purpose:
*     Convert a string to its numerical value, ignoring any sign

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = VAL( STRING, J, K, STATUS )

*  Description:
*     Evaluate a numeric substring of a character string as an
*     unsigned string, i.e. ignore any negative sign.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string containing the number
*     J = INTEGER (Given)
*        The first character in STRING to be used
*     K = INTEGER (Given)
*        The last character in STRING to be used
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     VAL = DOUBLE PRECISION
*        The numerical value of STRING

*  Algorithm:
*     -  Call a CHR routine to do the conversion.
*     -  Get the absolute value and return it.

*  Previous Implementation:
*     The INTERIM version of this routine returned a status value of 1
*     if the conversion failed.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     ANO: Someone (Somewhere)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (ANO):
*        Original version.
*     1-FEB-1993 (PMA):
*        Converted to use CHR_CTOD.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING
      INTEGER J
      INTEGER K

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION LVALUE    ! Local value

*.

      VAL = 0
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the sub-string to a number.
      CALL CHR_CTOD( STRING( J:K ), LVALUE, STATUS )

*  Take the modulus of the number.
      IF ( STATUS .EQ. SAI__OK ) THEN
         VAL = ABS( LVALUE )
      ELSE
         VAL = 0.0D0
      END IF

      END

