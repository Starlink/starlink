      INTEGER FUNCTION CHR_INDEX( STRING, SUBSTR )
*+
*  Name:
*     CHR_INDEX

*  Purpose:
*     Return the index of a substring in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_INDEX( STRING, SUBSTR )

*  Description:
*     Find the position of a substring within a given string. If no 
*     substring is found, the value zero is returned.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     SUBSTR = CHARACTER * ( * ) (Given)
*        The substring to be used in the search.

*  Returned Value:
*     CHR_INDEX = INTEGER
*        The position of SUBSTR within STRING.

*  Algorithm:
*     Use the Fortran 77 INDEX function.

*  Notes:
*     This routine is OBSOLETE.  It exists for historical reasons.
*     Its function is identical to the Fortran intrinsic function INDEX. 
*     It is recommended that the INDEX intrinsic function be called 
*     directly.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Improved documentation.
*     2-SEP-1988 (AJC):
*        Added to description.
*     30-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STRING * ( * )
      CHARACTER SUBSTR * ( * )

*.

*  Use the Fortran INDEX intrinsic function to set the returned value.
      CHR_INDEX = INDEX( STRING, SUBSTR )

      END
