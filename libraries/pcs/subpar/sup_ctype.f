      CHARACTER*15 FUNCTION SUBPAR_CTYPE( TYPE )
*+
*  Name:
*     SUBPAR_CTYPE

*  Purpose:
*     To return the parameter type as a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_CTYPE( TYPE )

*  Description:
*     The function value is set to the character form of a primitive
*     type, or to 'NON-PRIMITIVE'

*  Arguments:
*     TYPE = INTEGER (Given)
*        The TYPE code to be interpreted

*  Returned Value:
*     SUBPAR_CTYPE = CHARACTER * ( 15 )
*        The character form of the type.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-DEC-1992 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants

*  Arguments Given:
      INTEGER TYPE
*.

      IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
         SUBPAR_CTYPE = '_CHAR'
      ELSEIF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN
         SUBPAR_CTYPE = '_DOUBLE'
      ELSEIF ( TYPE .EQ. SUBPAR__INTEGER ) THEN
         SUBPAR_CTYPE = '_INTEGER'
      ELSEIF ( TYPE .EQ. SUBPAR__LOGICAL ) THEN
         SUBPAR_CTYPE = '_LOGICAL'
      ELSEIF ( TYPE .EQ. SUBPAR__REAL) THEN
         SUBPAR_CTYPE = '_REAL'
      ELSE
         SUBPAR_CTYPE = 'NON-PRIMITIVE'
      ENDIF

      END
